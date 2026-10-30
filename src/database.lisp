;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/database
  (:use #:common-lisp)
  (:import-from #:graph-db)
  (:local-nicknames (#:store #:project-isidore/store)
                    (#:bible-db #:project-isidore/bible/database)
                    (#:bot-db #:project-isidore/slack-bot/database))
  ;; The old stores' text records (the type registry, the schema manifest)
  ;; name these with one colon, as the old package exported them, and are
  ;; read back with the Lisp reader: they must stay external.
  (:export #:bible-metadata #:make-bible-metadata
           #:book-numbers-of #:chapter-urls-of #:book-urls-of
           #:bible #:make-bible
           #:unique-id-of #:heading-of #:text-of #:cross-references-of #:footnotes-of
           #:slack-subscription #:make-slack-subscription
           #:slack-identity #:make-slack-identity
           #:user-id-of #:initial-of)
  (:export #:*bible-store-path*
           #:migrate-stores)
  (:documentation
   "The stores from before the bible and the Slack bot had a database each,
kept only to copy them into theirs.

graph-db registers every vertex type by its package-qualified symbol, so the
types below have to stay in this package, under these names, for the old
stores to open.  MIGRATE-STORES copies their contents into the stores of
PROJECT-ISIDORE/BIBLE/DATABASE and PROJECT-ISIDORE/SLACK-BOT/DATABASE, under
a new system directory, and leaves the old stores as they were, so an older
build can still run against them.

Once production has migrated, delete this file, its call in
PROJECT-ISIDORE/MAIN and the test in tests/migration.lisp."))

(in-package #:project-isidore/database)

(defvar *bible-store-path* #p"/var/local/project-isidore/graph-db/bible/"
  "The directory of the old bible store.")

(defun crud-store-path ()
  "The directory of the old store of the Slack bot's data, <data-root>/graph-db/."
  (uiop:subpathname (store:data-root) "graph-db/"))

(defun system-directory ()
  "The old stores' graph-db system directory, <data-root>/graph-db-system/."
  (uiop:subpathname (store:data-root) "graph-db-system/"))

;;; The old schema, exactly as the old stores recorded it.

(graph-db:def-vertex bible-metadata ()
  ((book-numbers :accessor book-numbers-of)
   (chapter-urls :accessor chapter-urls-of)
   (book-urls    :accessor book-urls-of))
  :isidore-bible)

(graph-db:def-vertex bible ()
  ((unique-id        :accessor unique-id-of        :type fixnum)
   (heading          :accessor heading-of          :type cons)
   (text             :accessor text-of             :type string)
   (cross-references :accessor cross-references-of :type (or string null))
   (footnotes        :accessor footnotes-of        :type (or string null)))
  :isidore-bible)

(graph-db:def-vertex slack-subscription ()
  ((user-id :accessor user-id-of :type string)
   (initial :accessor initial-of :type string))
  :isidore-crud)

(graph-db:def-vertex slack-identity ()
  ((user-id :accessor user-id-of :type string)
   (initial :accessor initial-of :type string))
  :isidore-crud)

(graph-db:def-view bible-by-uid :lessp (bible :isidore-bible)
  (:map
   (lambda (v)
     (let ((uid (graph-db:node-slot-value v :unique-id)))
       (when uid
         (graph-db:yield uid nil))))))

(graph-db:def-view subscription-by-user-ci :lessp
  (slack-subscription :isidore-crud)
  (:map
   (lambda (s)
     (let ((uid (graph-db:node-slot-value s :user-id)))
       (when uid
         (graph-db:yield (string-downcase uid) nil))))))

(graph-db:def-view identity-by-user-ci :lessp
  (slack-identity :isidore-crud)
  (:map
   (lambda (s)
     (let ((uid (graph-db:node-slot-value s :user-id)))
       (when uid
         (graph-db:yield (string-downcase uid) nil))))))

;;; Reading the old stores

(defun call-with-old-store (name location function)
  "Call FUNCTION with the old store NAME at LOCATION open, then close it."
  (setf graph-db:*system-directory* (namestring (system-directory)))
  (let ((dirty-file (merge-pathnames ".dirty" location)))
    (when (probe-file dirty-file)
      (delete-file dirty-file)))
  (let ((graph (graph-db:open-graph name location :buffer-pool-p nil)))
    (unwind-protect (funcall function graph)
      (store:close-store graph))))

(defun read-bible ()
  "The old bible store's verses, as plists of BIBLE-DB:MAKE-BIBLE arguments,
and its metadata, as a plist of BIBLE-DB:MAKE-BIBLE-METADATA arguments."
  (call-with-old-store
   :isidore-bible *bible-store-path*
   (lambda (graph)
     (values
      (graph-db:map-vertices
       (lambda (v)
         (list :unique-id (unique-id-of v)
               :heading (copy-tree (heading-of v))
               :text (text-of v)
               :cross-references (cross-references-of v)
               :footnotes (footnotes-of v)))
       graph :vertex-type 'bible :collect-p t)
      (let ((metadata (first (graph-db:map-vertices #'identity graph
                                                    :vertex-type 'bible-metadata
                                                    :collect-p t))))
        (when metadata
          (list :book-numbers (copy-tree (book-numbers-of metadata))
                :chapter-urls (copy-tree (chapter-urls-of metadata))
                :book-urls (copy-tree (book-urls-of metadata)))))))))

(defun read-bot ()
  "The old store's subscriptions and identities, as two alists of
(user-id . initial)."
  (flet ((pairs (graph type)
           (graph-db:map-vertices (lambda (v) (cons (user-id-of v) (initial-of v)))
                                  graph :vertex-type type :collect-p t)))
    (call-with-old-store
     :isidore-crud (crud-store-path)
     (lambda (graph)
       (values (pairs graph 'slack-subscription)
               (pairs graph 'slack-identity))))))

;;; Writing the new ones

(defun count-vertices (graph type)
  (let ((n 0))
    (graph-db:map-vertices (lambda (v) (declare (ignore v)) (incf n)) graph :vertex-type type)
    n))

(defun write-bible (verses metadata)
  "Create the new bible store and copy VERSES and METADATA into it."
  (let ((graph (store:open-store :bible bible-db:*store-path*)))
    (unwind-protect
         (progn
           (when metadata
             (store:with-transaction (graph :context "migrate bible metadata")
               (apply #'bible-db:make-bible-metadata metadata)))
           ;; A thousand verses to a transaction.
           (let ((left verses))
             (loop while left
                   do (store:with-transaction (graph :context "migrate bible verses")
                        (loop repeat 1000
                              while left
                              do (apply #'bible-db:make-bible (pop left))))))
           (let ((copied (count-vertices graph 'bible-db:bible)))
             (unless (= copied (length verses))
               (error "The new bible store has ~D verses, not ~D." copied (length verses)))))
      (store:close-store graph))))

(defun write-bot (subscriptions identities)
  "Create the bot's new store and copy SUBSCRIPTIONS and IDENTITIES into it."
  (let ((bot-db:*database* (store:open-store :slack-bot (bot-db:store-path))))
    (unwind-protect
         (progn
           (loop for (user-id . initial) in subscriptions
                 do (bot-db:add-slack-subscription user-id initial))
           (loop for (user-id . initial) in identities
                 do (bot-db:add-slack-identity user-id initial))
           (unless (and (= (length subscriptions) (length (bot-db:all-slack-subscriptions)))
                        (= (length identities) (length (bot-db:all-slack-identities))))
             (error "The bot's new store does not hold every subscription and identity.")))
      (bot-db:close-database))))

(defun migrate-stores ()
  "Copy the old stores into the new ones, the first time there are new ones
to make: when the new system directory does not exist yet and the old one
does.  Either everything is copied or nothing is left behind: on an error the
new system directory and the new stores are deleted again, so that the next
start retries.  The old stores are never written to."
  (let ((bible-p (store:store-exists-p *bible-store-path*))
        (bot-p (store:store-exists-p (crud-store-path))))
    (when (and (or bible-p bot-p)
               (uiop:directory-exists-p (system-directory))
               (not (uiop:directory-exists-p (store:system-directory))))
      (when (and bible-p (uiop:directory-exists-p bible-db:*store-path*))
        (error "Refusing to migrate the bible over the existing ~A." bible-db:*store-path*))
      (multiple-value-bind (verses metadata) (when bible-p (read-bible))
        (multiple-value-bind (subscriptions identities) (when bot-p (read-bot))
          (let ((done nil))
            (unwind-protect
                 (progn
                   (when bible-p
                     (write-bible verses metadata))
                   (when bot-p
                     (write-bot subscriptions identities))
                   (setf done t))
              ;; By now WRITE-BIBLE and WRITE-BOT have closed their stores.
              (unless done
                (dolist (directory (list (store:system-directory)
                                         bible-db:*store-path*
                                         (bot-db:store-path)))
                  (when (uiop:directory-exists-p directory)
                    (uiop:delete-directory-tree directory :validate t)))
                ;; graph-db caches its registries by directory; forget the
                ;; deleted ones, so that a retry in this image starts afresh.
                (setf graph-db::*type-registry* nil
                      graph-db::*store-registry* nil))))
          (format *error-output* "~&migration: copied ~D bible verses, ~D subscriptions ~
                                  and ~D identities into the new stores~%"
                  (length verses) (length subscriptions) (length identities)))))))
