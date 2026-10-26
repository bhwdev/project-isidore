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
   #:ensure-bible-views
   #:ensure-crud-views
   #:make-bible
   #:make-bible-metadata
   #:make-slack-subscription
   #:make-slack-identity)
  (:documentation
   "Project Isidore database setup and teardown.
Manages two graph-db (Vivace Graph V3) stores:
  *BIBLE-DATABASE* — read-only at runtime, lives at /var/local/project-isidore/graph-db/bible/.
  *DATABASE*       — general-purpose runtime CRUD store at <data-root>/graph-db/.
Both *DATABASE* and the Montezuma search index resolve relative to a single
data root (DATA-ROOT): $PROJECT_ISIDORE_DATA, falling back to
$XDG_DATA_HOME/project-isidore/.
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
Tolerates a stale .dirty file from an unclean shutdown by removing it
before OPEN-GRAPH (which would otherwise refuse). A production-grade
recovery would replay the txn log; for our workload the snapshot is
sufficient.

Disables graph-db's buffer pool (:BUFFER-POOL-P NIL) on both make-graph
and open-graph paths. The pool is an optional vertex/edge recycler — when
absent, GET-VERTEX-BUFFER falls back to MAKE-INSTANCE. The upstream calls
ENSURE-BUFFER-POOL with zero arguments inside OPEN-GRAPH, which fails at
SBCL's arity check regardless of pool state, and the background thread
has been a source of worker crashes during long imports."
  (ensure-directories-exist path)
  (let ((schema-file (merge-pathnames "schema.dat" path))
        (dirty-file  (merge-pathnames ".dirty"     path)))
    (when (and (probe-file schema-file) (probe-file dirty-file))
      (delete-file dirty-file))
    (if (probe-file schema-file)
        ;; :GC-HEAP-P NIL avoids the load-time vertex iteration in OPEN-GRAPH.
        ;; That iteration calls CHANGE-CLASS to per-vertex subclasses (e.g.
        ;; BIBLE) which only exist after DEF-VERTEX runs — schema is registered
        ;; before opening, but the CLOS classes themselves are also created by
        ;; DEF-VERTEX, so heap GC must run after the class definitions.
        (graph-db:open-graph name path :buffer-pool-p nil :gc-heap-p nil)
        (graph-db:make-graph name path :buffer-pool-p nil))))

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

;;; Schema: vertex classes registered in *SCHEMA-NODE-METADATA* before any
;;; graph is opened, so OPEN-GRAPH's UPDATE-SCHEMA can attach them.
;;;
;;; DEF-VERTEX's expansion calls INSTANTIATE-NODE-TYPE whenever the graph is
;;; already registered in GRAPH-DB::*GRAPHS* (i.e., on every reload after
;;; the first load). INSTANTIATE-NODE-TYPE then calls LOOKUP-NODE-TYPE-BY-NAME
;;; with no :GRAPH arg, so it reads GRAPH-DB:*GRAPH* — NIL at top-level on
;;; reload — and SCHEMA(NIL) blows up. Binding *GRAPH* around DEF-VERTEX
;;; lets the reload-time instantiate path see the right schema; on first
;;; load LOOKUP-GRAPH returns NIL, DEF-VERTEX skips INSTANTIATE-NODE-TYPE,
;;; and the NIL binding is harmless.

(let ((graph-db:*graph* (graph-db:lookup-graph :isidore-bible)))
  (declare (ignorable graph-db:*graph*))
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
    :isidore-bible))

(let ((graph-db:*graph* (graph-db:lookup-graph :isidore-crud)))
  (declare (ignorable graph-db:*graph*))
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
    :isidore-crud))

;;; graph-db cannot add a vertex type to a graph that already exists on disk,
;;; and this is the repair.  SAVE-SCHEMA nulls the schema's CLASS-LOCKS table
;;; before CL-STORE:STORE and restores it only in memory (graph-db schema.lisp
;;; SAVE-SCHEMA), so schema.dat always records NIL; OPEN-GRAPH rebuilds
;;; SCHEMA-LOCK afterwards but not CLASS-LOCKS (graph-db graph.lisp
;;; OPEN-GRAPH).  A restored schema therefore carries CLASS-LOCKS = NIL, and
;;; the first DEF-VERTEX of a type that is not already in schema.dat reaches
;;; INSTANTIATE-NODE-TYPE's new-type branch, which does
;;; (setf (gethash name (schema-class-locks ...)) (make-rw-lock)) and dies with
;;; "The value NIL is not of type HASH-TABLE".  Existing types take the other
;;; branch, which is why every load before SLACK-IDENTITY was added worked.
;;;
;;; UPDATE-SCHEMA is called from inside OPEN-GRAPH, so the table has to be back
;;; in place before that call rather than after OPEN-OR-MAKE-GRAPH returns.
;;; WITH-WRITE-LOCKED-CLASS and WITH-READ-LOCKED-CLASS are the only readers of
;;; this table and graph-db never calls either, so an empty table is as good as
;;; the original; the per-class locks are seeded anyway to match INIT-SCHEMA.

(defmethod graph-db::update-schema :before ((graph graph-db::graph))
  "Give a schema restored from disk back the CLASS-LOCKS hash-table that
SAVE-SCHEMA dropped, so a newly defined vertex type can be added to a graph
that already exists.  A no-op for a freshly made graph, whose INIT-SCHEMA
already built the table."
  (let ((schema (graph-db:schema graph)))
    (when (and schema (not (hash-table-p (graph-db:schema-class-locks schema))))
      (setf (graph-db:schema-class-locks schema)
            (make-hash-table :test 'eql :synchronized t))
      (dolist (parent '(graph-db::edge graph-db::vertex))
        (setf (gethash parent (graph-db:schema-class-locks schema))
              (graph-db:make-rw-lock))))))

;;; Now open the graphs — UPDATE-SCHEMA attaches the registered metadata.

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

;;; Views are installed via dedicated functions so they can be skipped on a
;;; reopen (RESTORE-VIEWS already rebuilt them from disk) and re-run by the
;;; test fixture against a fresh CRUD graph.

(defun view-installed-p (graph class-name view-name)
  "Like GRAPH-DB:LOOKUP-VIEW but returns NIL instead of erroring when
the view group for CLASS-NAME has not yet been created on GRAPH."
  (let ((group (graph-db:lookup-view-group class-name graph)))
    (when group
      (graph-db:lookup-view graph class-name view-name))))

(defun ensure-bible-views (&optional (graph *bible-database*))
  "Install the BIBLE-BY-UID view on GRAPH if it does not already exist.
DEF-VIEW writes views.dat and calls REGENERATE-VIEW (a linear scan of all
matching vertices) — expensive on the 37 199-row bible store, so guard
against re-running when OPEN-GRAPH has already restored the view."
  (unless (view-installed-p graph 'bible 'bible-by-uid)
    (let ((graph-db:*graph* graph))
      (graph-db:def-view bible-by-uid :lessp (bible :isidore-bible)
        (:map
         (lambda (v)
           (let ((uid (graph-db:node-slot-value v :unique-id)))
             (when uid
               (graph-db:yield uid nil)))))))))

(defun ensure-crud-views (&optional (graph *database*))
  "Install the SUBSCRIPTION-BY-USER-CI and IDENTITY-BY-USER-CI views on GRAPH
if they do not already exist. Idempotent across reopens."
  (unless (view-installed-p graph 'slack-subscription
                            'subscription-by-user-ci)
    (let ((graph-db:*graph* graph))
      (graph-db:def-view subscription-by-user-ci :lessp
        (slack-subscription :isidore-crud)
        (:map
         (lambda (s)
           (let ((uid (graph-db:node-slot-value s :user-id)))
             (when uid
               (graph-db:yield (string-downcase uid) nil))))))))
  (unless (view-installed-p graph 'slack-identity
                            'identity-by-user-ci)
    (let ((graph-db:*graph* graph))
      (graph-db:def-view identity-by-user-ci :lessp
        (slack-identity :isidore-crud)
        (:map
         (lambda (s)
           (let ((uid (graph-db:node-slot-value s :user-id)))
             (when uid
               (graph-db:yield (string-downcase uid) nil)))))))))

(when *bible-database*
  (ensure-bible-views))
(ensure-crud-views)

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
Eagerly hydrates the vertex's data alist so callers can use accessors
without binding GRAPH-DB:*GRAPH* themselves; once the bytes slot is
populated, subsequent NODE-SLOT-VALUE calls never consult *GRAPH*.

Memoized via *BIBLE-OBJ-CACHE* — the vendored bible graph is read-only at
runtime, so caching the inflated vertex object skips the view lookup,
LOOKUP-VERTEX, and MAYBE-INIT-NODE-DATA on repeat hits (~11μs uncached
vs ~5ns cached, measured). Mutators (migration.lisp) and REOPEN-BIBLE-DATABASE
must call CLEAR-BIBLE-OBJ-CACHE."
  (or (gethash bible-uid *bible-obj-cache*)
      (let* ((graph-db:*graph* *bible-database*)
             (rows (graph-db:invoke-graph-view 'bible 'bible-by-uid
                                               :key bible-uid))
             (id   (cdr (assoc :id (first rows)))))
        (when id
          (let ((v (graph-db:lookup-vertex id)))
            (graph-db::maybe-init-node-data v :graph *bible-database*)
            (setf (gethash bible-uid *bible-obj-cache*) v))))))

(defun ensure-bible-metadata ()
  "Return the singleton BIBLE-METADATA vertex from the bible graph, or NIL.
Eagerly hydrates the vertex's data alist (see BIBLE-OBJ-WITH-ID)."
  (let ((graph-db:*graph* *bible-database*)
        (result nil))
    (graph-db:map-vertices
     (lambda (v) (setf result v))
     *bible-database*
     :vertex-type 'bible-metadata)
    (when result
      (graph-db::maybe-init-node-data result :graph *bible-database*))
    result))

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
  (let ((graph-db:*graph* *bible-database*)
        (heading-to-uid (make-hash-table :test 'equalp :size 40000))
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
  (ensure-bible-views *bible-database*)
  (load-bible-tables))

(defun reopen-database ()
  "Reopen the general-purpose CRUD graph-db after sb-ext:save-lisp-and-die.
Re-resolves DATABASE-PATH so PROJECT_ISIDORE_DATA takes effect even when
the binary was built against a different path."
  (setf *database*
        (open-or-make-graph :isidore-crud (database-path)))
  (ensure-crud-views *database*)
  *database*)

;;; CRUD: slack subscriptions

(defun find-slack-subscription (user-id)
  "Return the persistent SLACK-SUBSCRIPTION vertex for USER-ID, or NIL.
Lookup is case-insensitive on USER-ID. Eagerly hydrates the vertex's
data alist (see BIBLE-OBJ-WITH-ID).  Binds GRAPH-DB:*GRAPH* to *DATABASE*
itself: MAP-VIEW looks each matching row up through *GRAPH* rather than
the :GRAPH argument, so without the binding a call made outside
WITH-DB-TRANSACTION fails with VERTEX-TABLE (NIL) once a row exists."
  (let* ((graph-db:*graph* *database*)
         (rows (graph-db:invoke-graph-view 'slack-subscription
                                           'subscription-by-user-ci
                                           :key (string-downcase user-id)
                                           :graph *database*))
         (id (cdr (assoc :id (first rows)))))
    (when id
      (let ((v (graph-db:lookup-vertex id :graph *database*)))
        (graph-db::maybe-init-node-data v :graph *database*)
        v))))

(defun all-slack-subscriptions ()
  "Return an alist of (user-id . initial) for every persisted subscription."
  (let ((acc '())
        (graph-db:*graph* *database*))
    (graph-db:map-vertices
     (lambda (v)
       (graph-db::maybe-init-node-data v :graph *database*)
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
Lookup is case-insensitive on USER-ID.  FIND-SLACK-SUBSCRIPTION documents
why GRAPH-DB:*GRAPH* is bound and why the vertex is hydrated eagerly; the
same reasoning applies here."
  (let* ((graph-db:*graph* *database*)
         (rows (graph-db:invoke-graph-view 'slack-identity
                                           'identity-by-user-ci
                                           :key (string-downcase user-id)
                                           :graph *database*))
         (id (cdr (assoc :id (first rows)))))
    (when id
      (let ((v (graph-db:lookup-vertex id :graph *database*)))
        (graph-db::maybe-init-node-data v :graph *database*)
        v))))

(defun all-slack-identities ()
  "Return an alist of (user-id . initial) for every registered identity."
  (let ((acc '())
        (graph-db:*graph* *database*))
    (graph-db:map-vertices
     (lambda (v)
       (graph-db::maybe-init-node-data v :graph *database*)
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
