;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/bible/database
  (:use #:common-lisp)
  (:import-from #:graph-db)
  (:local-nicknames (#:store #:project-isidore/store))
  (:export #:*database*
           #:*store-path*
           #:open-database
           #:close-database

           #:bible-metadata
           #:make-bible-metadata
           #:book-numbers-of #:chapter-urls-of #:book-urls-of

           #:bible
           #:make-bible
           #:unique-id-of #:heading-of #:text-of #:cross-references-of #:footnotes-of
           #:bible-obj-with-id

           #:+bible-book-numbers-alist+
           #:+bible-chapter-url-alist+
           #:+bible-book-url-alist+
           #:+bible-heading-to-uid+
           #:+bible-uid-to-heading+)
  (:documentation
   "The bible's graph-db store: the 1883 Haydock Douay-Rheims text, one BIBLE
vertex per verse, read-only at runtime.

The store lives at *STORE-PATH*, the same path in development and
production; a machine without it (the guix.scm build sandbox, a fresh
checkout) simply has no bible, and OPEN-DATABASE returns NIL there."))

(in-package #:project-isidore/bible/database)

(defvar *store-path* #p"/var/local/project-isidore/bible/"
  "The directory of the bible's store.")

(defvar *database* nil
  "The open bible store, or NIL before OPEN-DATABASE or without a store.")

;;; Schema.  OPEN-GRAPH and MAKE-GRAPH attach the types and views declared
;;; for the store named :BIBLE.

(graph-db:def-vertex bible-metadata ()
  ((book-numbers :accessor book-numbers-of)
   (chapter-urls :accessor chapter-urls-of)
   (book-urls    :accessor book-urls-of))
  :bible)

(graph-db:def-vertex bible ()
  ((unique-id        :accessor unique-id-of        :type fixnum)
   (heading          :accessor heading-of          :type cons)
   (text             :accessor text-of             :type string)
   (cross-references :accessor cross-references-of :type (or string null))
   (footnotes        :accessor footnotes-of        :type (or string null)))
  :bible)

(graph-db:def-view bible-by-uid :lessp (bible :bible)
  (:map
   (lambda (v)
     (let ((uid (graph-db:node-slot-value v :unique-id)))
       (when uid
         (graph-db:yield uid nil))))))

;;; Lookup tables, filled from the store by OPEN-DATABASE.

(defparameter +bible-book-numbers-alist+ nil
  "Alist of book name to book number, aliases included.")

(defparameter +bible-chapter-url-alist+ nil
  "Alist of chapter URL to \"Book N\" link text, in bible order.")

(defparameter +bible-book-url-alist+ nil
  "Alist of book URL to book name link text, in bible order.")

(defparameter +bible-heading-to-uid+ (make-hash-table :test 'equalp)
  "Maps (book chapter verse) lists to bible unique IDs.")

(defparameter +bible-uid-to-heading+ (make-hash-table :test 'eql)
  "Maps bible unique IDs to heading alists.")

(defvar *bible-obj-cache* (make-hash-table :test 'eql :size 40000)
  "UID -> BIBLE vertex, memoized by BIBLE-OBJ-WITH-ID.  Safe because the store
is read-only at runtime; CLOSE-DATABASE empties it.")

(defun load-tables ()
  "Fill the lookup tables from *DATABASE* in one pass over its BIBLE vertices."
  (let ((heading-to-uid (make-hash-table :test 'equalp :size 40000))
        (uid-to-heading (make-hash-table :test 'eql :size 40000))
        (metadata (first (graph-db:map-vertices #'identity *database*
                                                :vertex-type 'bible-metadata
                                                :collect-p t))))
    (graph-db:map-vertices
     (lambda (v)
       (let ((heading (heading-of v))
             (uid (unique-id-of v)))
         (when (and heading uid)
           (setf (gethash (list (cdar heading) (cdadr heading) (cdaddr heading))
                          heading-to-uid)
                 uid
                 (gethash uid uid-to-heading) heading))))
     *database*
     :vertex-type 'bible)
    (setf +bible-book-numbers-alist+ (and metadata (book-numbers-of metadata))
          +bible-chapter-url-alist+ (and metadata (chapter-urls-of metadata))
          +bible-book-url-alist+ (and metadata (book-urls-of metadata))
          +bible-heading-to-uid+ heading-to-uid
          +bible-uid-to-heading+ uid-to-heading)))

(defun open-database ()
  "Open the bible store and fill the lookup tables from it.  Returns the
store, or NIL when there is none at *STORE-PATH*."
  (close-database)
  (when (store:store-exists-p *store-path*)
    (setf *database* (store:open-store :bible *store-path*))
    (load-tables)
    *database*))

(defun close-database ()
  "Close the bible store if it is open."
  (clrhash *bible-obj-cache*)
  (setf *database* (store:close-store *database*)))

(defun bible-obj-with-id (bible-uid)
  "Return the BIBLE vertex whose UNIQUE-ID is BIBLE-UID, or NIL."
  (or (gethash bible-uid *bible-obj-cache*)
      (let* ((rows (graph-db:invoke-graph-view 'bible 'bible-by-uid
                                               :key bible-uid
                                               :graph *database*))
             (id (cdr (assoc :id (first rows)))))
        (when id
          (setf (gethash bible-uid *bible-obj-cache*)
                (graph-db:lookup-vertex id :graph *database*))))))
