;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/database
  (:mix #:common-lisp #:series #:alexandria #:serapeum)
  ;; Drives package-inferred-system to depend on graph-db. Symbols are
  ;; otherwise referenced via the GRAPH-DB: prefix throughout this file.
  (:import-from #:graph-db #:*graph*)
  (:export
   #:*database*
   #:*bible-database*
   #:data-root
   #:database-path
   #:system-directory
   #:index-path

   #:bible-metadata
   #:book-numbers-of #:chapter-urls-of #:book-urls-of

   #:bible
   #:unique-id-of #:heading-of #:text-of #:cross-references-of #:footnotes-of

   #:ensure-bible-metadata
   #:*bible-metadata*

   #:bible-obj-with-id
   #:clear-bible-obj-cache

   #:+bible-book-numbers-alist+
   #:+bible-chapter-url-alist+
   #:+bible-book-url-alist+
   #:+bible-heading-to-uid+
   #:+bible-uid-to-heading+

   #:reopen-database
   #:reopen-bible-database
   #:bible-store-present-p
   #:load-bible-tables

   #:slack-subscription
   #:user-id-of #:initial-of
   #:find-slack-subscription
   #:all-slack-subscriptions
   #:add-slack-subscription
   #:remove-slack-subscription

   #:slack-identity
   #:find-slack-identity
   #:all-slack-identities
   #:add-slack-identity
   #:remove-slack-identity

   #:with-db-transaction
   #:open-or-make-graph
   #:bible-graph-path
   #:make-bible
   #:make-bible-metadata
   #:make-slack-subscription
   #:make-slack-identity)
  (:documentation
   "Project Isidore database setup and teardown.
Manages two graph-db (VivaceGraph 4) stores:
  *BIBLE-DATABASE* — read-only at runtime, lives at /var/local/project-isidore/graph-db/bible/.
  *DATABASE*       — general-purpose runtime CRUD store at <data-root>/graph-db/.
*DATABASE* and graph-db's system directory (SYSTEM-DIRECTORY) resolve
relative to a single data root (DATA-ROOT): $PROJECT_ISIDORE_DATA, falling
back to $XDG_DATA_HOME/project-isidore/.
Defines the persistent vertex schema."))

(in-package #:project-isidore/database)

(defun data-root ()
  "Return the directory pathname for project persistent CRUD data.
Honors PROJECT_ISIDORE_DATA; falls back to $XDG_DATA_HOME/project-isidore/.
Used by DATABASE-PATH for the runtime CRUD graph-db. BIBLE-GRAPH-PATH
and INDEX-PATH point at /var/local/project-isidore/ unconditionally
because that read-only bible content is shipped at the same path in
development and production."
  (let ((env (uiop:getenvp "PROJECT_ISIDORE_DATA")))
    (if env
        (uiop:ensure-directory-pathname env)
        (uiop:xdg-data-home "project-isidore/"))))

(defun database-path ()
  "Return the directory pathname for the general-purpose CRUD graph-db.
Lives at <data-root>/graph-db/."
  (merge-pathnames "graph-db/" (data-root)))

(defun system-directory ()
  "Return the directory pathname OPEN-OR-MAKE-GRAPH points graph-db's
*SYSTEM-DIRECTORY* at: the state graph-db shares between every store of an
image, chiefly the registry that gives each vertex type one type-id across
both stores.  Lives at <data-root>/graph-db-system/, beside the CRUD store,
because the registry is written to whenever a store first meets a type.

The registry adopts the ids a store already uses when it first opens it, and
refuses to open a store whose ids it has given to other types.  Copying a
store from another machine therefore means copying this directory with it."
  (merge-pathnames "graph-db-system/" (data-root)))

(defun index-path ()
  "Return the directory pathname for the Montezuma search index.
Same path in development and production: the index is read-only bible
content shipped alongside *BIBLE-DATABASE*."
  #p"/var/local/project-isidore/montezuma/")

(defun bible-graph-path ()
  "Return the directory pathname for the bible graph-db store.
Same path in development and production."
  #p"/var/local/project-isidore/graph-db/bible/")

(defun bible-store-present-p ()
  "True when the bible graph-db directory exists on this machine.  The
package build (guix.scm) runs in a sandbox with no /var/local/, so loading
leaves *BIBLE-DATABASE* NIL there and REOPEN-BIBLE-DATABASE opens the
store, mounted into the container, when the application starts."
  (uiop:directory-exists-p (bible-graph-path)))

(defun open-or-make-graph (name path)
  "Open the graph at PATH if it has a schema, else create a fresh one.
Points graph-db's *SYSTEM-DIRECTORY* at SYSTEM-DIRECTORY first, which graph-db
requires before any store is opened.

A .dirty file left by an unclean shutdown is removed before OPEN-GRAPH, which
then recovers the store by replaying its transaction log: graph-db's own
procedure for its STORE-NOT-CLOSED-CLEANLY-ERROR.  Nothing closes the graphs
when the application stops, so every production start takes this path.

Opens without graph-db's buffer pool (:BUFFER-POOL-P NIL), a vertex/edge
pre-allocator for write-heavy loads.  It costs ~130 MB and ~2 s on first use,
and its background thread outlives CLOSE-GRAPH, so make.lisp would have to kill
it before SAVE-LISP-AND-DIE."
  (setf graph-db:*system-directory*
        (namestring (ensure-directories-exist (system-directory))))
  (let ((schema-file (merge-pathnames "schema.dat" path))
        (dirty-file  (merge-pathnames ".dirty"     path)))
    (when (and (probe-file schema-file) (probe-file dirty-file))
      (delete-file dirty-file))
    (ensure-directories-exist path)
    (if (probe-file schema-file)
        (graph-db:open-graph name path :buffer-pool-p nil)
        (let ((graph (graph-db:make-graph name path :buffer-pool-p nil)))
          ;; Unlike OPEN-GRAPH, MAKE-GRAPH attaches the declared vertex types
          ;; but not the declared views.
          (graph-db:install-views graph)
          graph))))

(defmacro with-db-transaction ((&key (context "db-op") (graph '*database*))
                               &body body)
  "Run BODY inside GRAPH-DB:WITH-TRANSACTION against GRAPH (defaults to
*DATABASE*), logging errors tagged with CONTEXT before re-signaling."
  (with-gensyms (c g)
    `(let* ((,g ,graph)
            (graph-db:*graph* ,g))
       (handler-case
           (graph-db:with-transaction ()
             ,@body)
         (error (,c)
           (format *error-output* "database: ~A failed: ~A~%" ,context ,c)
           (error ,c))))))

;;; Schema: vertex types and their views are declared before any graph is
;;; opened; OPEN-GRAPH and MAKE-GRAPH attach them to the store named in each.
;;; Re-evaluating them while the graphs are open (a reload) reconciles them
;;; with the open stores, and an unchanged DEF-VIEW does not rebuild its index.

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

;; Who a Slack account is, so "book me a van" can name them.  Deliberately
;; a separate vertex from SLACK-SUBSCRIPTION: that one is the opt-in to
;; reminder DMs, and /unsubscribe must not cost someone their identity.
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

;;; Now open the graphs.

(defparameter *bible-database*
  (or (graph-db:lookup-graph :isidore-bible)
      (when (bible-store-present-p)
        (open-or-make-graph :isidore-bible (bible-graph-path))))
  "Vendored bible graph-db store. Read-only at runtime.  NIL until
REOPEN-BIBLE-DATABASE when the store is absent at load time (see
BIBLE-STORE-PRESENT-P).
On reload, reuses the entry from GRAPH-DB::*GRAPHS* so we don't double-open
files (which can leave fd's leaked) and don't trip GC/schema-init paths
that assume the graph isn't already registered.")

(defparameter *database*
  (or (graph-db:lookup-graph :isidore-crud)
      (open-or-make-graph :isidore-crud (database-path)))
  "General-purpose CRUD graph-db store. Path resolved by DATABASE-PATH.
Reload-safe: reuses the existing GRAPH-DB::*GRAPHS* entry if present.")

;;; Bible accessors

(defparameter *bible-obj-cache*
  (make-hash-table :test 'eql :size 40000)
  "UID -> hydrated BIBLE vertex. Safe because the vendored bible graph is
read-only at runtime; mutators must clear via CLEAR-BIBLE-OBJ-CACHE.")

(defun clear-bible-obj-cache ()
  "Drop all memoized bible vertex objects. Call after any write to the
bible graph and from REOPEN-BIBLE-DATABASE (cached vertices reference the
old graph's pages)."
  (clrhash *bible-obj-cache*))

(defun bible-obj-with-id (bible-uid)
  "Return the BIBLE vertex whose UNIQUE-ID equals BIBLE-UID, or NIL.

Memoized via *BIBLE-OBJ-CACHE* — the vendored bible graph is read-only at
runtime, so caching the vertex object skips the view lookup and
LOOKUP-VERTEX on repeat hits. Mutators and REOPEN-BIBLE-DATABASE must call
CLEAR-BIBLE-OBJ-CACHE."
  (or (gethash bible-uid *bible-obj-cache*)
      (let* ((rows (graph-db:invoke-graph-view 'bible 'bible-by-uid
                                               :key bible-uid
                                               :graph *bible-database*))
             (id   (cdr (assoc :id (first rows)))))
        (when id
          (setf (gethash bible-uid *bible-obj-cache*)
                (graph-db:lookup-vertex id :graph *bible-database*))))))

(defun ensure-bible-metadata ()
  "Return the singleton BIBLE-METADATA vertex from the bible graph, or NIL."
  (first (graph-db:map-vertices #'identity *bible-database*
                                :vertex-type 'bible-metadata
                                :collect-p t)))

;;; The tables below are read from *BIBLE-DATABASE* by LOAD-BIBLE-TABLES, at
;;; load time when the store is present and again on every
;;; REOPEN-BIBLE-DATABASE, so a binary built without the store (guix.scm)
;;; fills them from the store it is started against.

(defparameter *bible-metadata* nil
  "The singleton BIBLE-METADATA vertex, or NIL. Set by LOAD-BIBLE-TABLES.")

(defparameter +bible-book-numbers-alist+ nil)

(defparameter +bible-chapter-url-alist+ nil)

(defparameter +bible-book-url-alist+ nil)

(defparameter +bible-heading-to-uid+
  (make-hash-table :test 'equalp :size 40000)
  "Maps (book chapter verse) lists to bible unique IDs for O(1) lookup.")

(defparameter +bible-uid-to-heading+
  (make-hash-table :test 'eql :size 40000)
  "Maps bible unique IDs to heading alists for O(1) reverse lookup.")

(defun load-bible-tables ()
  "Set *BIBLE-METADATA* and the +BIBLE-...+ lookup tables from
*BIBLE-DATABASE*, in one pass over its BIBLE vertices."
  (let ((heading-to-uid (make-hash-table :test 'equalp :size 40000))
        (uid-to-heading (make-hash-table :test 'eql :size 40000))
        (metadata (ensure-bible-metadata)))
    (graph-db:map-vertices
     (lambda (v)
       (let ((h   (heading-of v))
             (uid (unique-id-of v)))
         (when (and h uid)
           (setf (gethash (list (cdar h) (cdadr h) (cdaddr h)) heading-to-uid) uid
                 (gethash uid uid-to-heading) h))))
     *bible-database*
     :vertex-type 'bible)
    (setf *bible-metadata* metadata
          +bible-book-numbers-alist+ (when metadata (book-numbers-of metadata))
          +bible-chapter-url-alist+ (when metadata (chapter-urls-of metadata))
          +bible-book-url-alist+ (when metadata (book-urls-of metadata))
          +bible-heading-to-uid+ heading-to-uid
          +bible-uid-to-heading+ uid-to-heading)
    *bible-database*))

(when *bible-database*
  (load-bible-tables))

(defun reopen-bible-database ()
  "Reopen the vendored bible graph-db after sb-ext:save-lisp-and-die."
  (clear-bible-obj-cache)
  (setf *bible-database*
        (open-or-make-graph :isidore-bible (bible-graph-path)))
  (load-bible-tables))

(defun reopen-database ()
  "Reopen the general-purpose CRUD graph-db after sb-ext:save-lisp-and-die.
Re-resolves DATABASE-PATH so PROJECT_ISIDORE_DATA takes effect even when
the binary was built against a different path."
  (setf *database*
        (open-or-make-graph :isidore-crud (database-path))))

(defun close-databases ()
  "Close both stores before the image is saved.  graph-db keeps an image-wide
table of open stores by store id, and the ids are minted per system directory:
a store the build left open would hold the id that REOPEN-BIBLE-DATABASE's
store is given at startup, and that open would fail."
  (dolist (graph (list *bible-database* *database*))
    (when graph
      (graph-db:close-graph graph :snapshot-p nil))))

(uiop:register-image-dump-hook 'close-databases)

;;; CRUD: slack subscriptions

(defun find-slack-subscription (user-id)
  "Return the persistent SLACK-SUBSCRIPTION vertex for USER-ID, or NIL.
Lookup is case-insensitive on USER-ID."
  (let* ((rows (graph-db:invoke-graph-view 'slack-subscription
                                           'subscription-by-user-ci
                                           :key (string-downcase user-id)
                                           :graph *database*))
         (id (cdr (assoc :id (first rows)))))
    (when id
      (graph-db:lookup-vertex id :graph *database*))))

(defun all-slack-subscriptions ()
  "Return an alist of (user-id . initial) for every persisted subscription."
  (let ((acc '()))
    (graph-db:map-vertices
     (lambda (v)
       (push (cons (user-id-of v) (initial-of v)) acc))
     *database*
     :vertex-type 'slack-subscription)
    (nreverse acc)))

(defun add-slack-subscription (user-id initial)
  "Upsert a slack subscription. Returns the persistent vertex."
  (with-db-transaction (:context "add-slack-subscription")
    (let ((existing (find-slack-subscription user-id)))
      (if existing
          (let ((c (graph-db:copy existing)))
            (setf (initial-of c) initial)
            (graph-db:save c)
            c)
          (make-slack-subscription :user-id user-id :initial initial)))))

(defun remove-slack-subscription (user-id)
  "Delete any slack subscription for USER-ID. Returns T if one was removed."
  (with-db-transaction (:context "remove-slack-subscription")
    (let ((obj (find-slack-subscription user-id)))
      (when obj
        (graph-db:mark-deleted obj)
        t))))

;;; Slack identity: the initials a Slack account goes by, so a message can say
;;; "me" instead of spelling them out.  Kept apart from SLACK-SUBSCRIPTION on
;;; purpose — that is the opt-in to reminder DMs, and /unsubscribe must not
;;; cost someone the ability to write "book me a van".

(defun find-slack-identity (user-id)
  "Return the persistent SLACK-IDENTITY vertex for USER-ID, or NIL.
Lookup is case-insensitive on USER-ID."
  (let* ((rows (graph-db:invoke-graph-view 'slack-identity
                                           'identity-by-user-ci
                                           :key (string-downcase user-id)
                                           :graph *database*))
         (id (cdr (assoc :id (first rows)))))
    (when id
      (graph-db:lookup-vertex id :graph *database*))))

(defun all-slack-identities ()
  "Return an alist of (user-id . initial) for every registered identity."
  (let ((acc '()))
    (graph-db:map-vertices
     (lambda (v)
       (push (cons (user-id-of v) (initial-of v)) acc))
     *database*
     :vertex-type 'slack-identity)
    (nreverse acc)))

(defun add-slack-identity (user-id initial)
  "Upsert the initials USER-ID goes by. Returns the persistent vertex."
  (with-db-transaction (:context "add-slack-identity")
    (let ((existing (find-slack-identity user-id)))
      (if existing
          (let ((c (graph-db:copy existing)))
            (setf (initial-of c) initial)
            (graph-db:save c)
            c)
          (make-slack-identity :user-id user-id :initial initial)))))

(defun remove-slack-identity (user-id)
  "Delete any registered identity for USER-ID. Returns T if one was removed."
  (with-db-transaction (:context "remove-slack-identity")
    (let ((obj (find-slack-identity user-id)))
      (when obj
        (graph-db:mark-deleted obj)
        t))))
