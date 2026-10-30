;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/store
  (:use #:common-lisp)
  (:import-from #:alexandria #:with-gensyms)
  (:import-from #:graph-db)
  (:export #:data-root
           #:system-directory
           #:store-exists-p
           #:open-store
           #:close-store
           #:with-transaction)
  (:documentation
   "The graph-db plumbing that every Project Isidore database shares.

Each part of the application owns its own store and schema:
PROJECT-ISIDORE/BIBLE/DATABASE the bible's, PROJECT-ISIDORE/SLACK-BOT/DATABASE
the bot's.  What they cannot avoid sharing is graph-db's system directory,
which holds the registry giving every vertex type one id across all the
stores a process opens; graph-db keeps exactly one per image."))

(in-package #:project-isidore/store)

(defun data-root ()
  "Return the directory that holds Project Isidore's writable data:
$PROJECT_ISIDORE_DATA, or else $XDG_DATA_HOME/project-isidore/."
  (let ((env (uiop:getenvp "PROJECT_ISIDORE_DATA")))
    (if env
        (uiop:ensure-directory-pathname env)
        (uiop:xdg-data-home "project-isidore/"))))

(defun system-directory ()
  "Return graph-db's system directory, <data-root>/system/.

The type registry in it adopts the ids a store already uses when it first
opens that store, and refuses a store whose ids it has given to other types.
Copying a store from another machine therefore means copying this directory
with it."
  (uiop:subpathname (data-root) "system/"))

(defun store-exists-p (location)
  "True when the directory LOCATION holds a graph-db store."
  (and (probe-file (merge-pathnames "schema.dat" location)) t))

(defun open-store (name location &key (initial-size (* 64 1024 1024)))
  "Open the graph-db store NAME at the directory LOCATION, creating it there
when LOCATION holds none, and return it.

A .dirty file left by an unclean shutdown is removed first, so that
OPEN-GRAPH recovers the store by replaying its transaction log: graph-db's
own procedure for its STORE-NOT-CLOSED-CLEANLY-ERROR.

A new store starts with INITIAL-SIZE bytes for each of its heap and index
regions, rather than graph-db's 1000 MB, and grows on demand.  Stores are
opened without graph-db's buffer pool, a vertex pre-allocator for write-heavy
loads that costs ~130 MB and starts a thread that outlives CLOSE-GRAPH."
  (setf graph-db:*system-directory*
        (namestring (ensure-directories-exist (system-directory))))
  (let ((dirty-file (merge-pathnames ".dirty" location)))
    (cond ((store-exists-p location)
           (when (probe-file dirty-file)
             (delete-file dirty-file))
           (graph-db:open-graph name location :buffer-pool-p nil))
          (t
           (ensure-directories-exist location)
           (let ((graph (graph-db:make-graph name location
                                             :buffer-pool-p nil
                                             :heap-size initial-size
                                             :index-size initial-size)))
             ;; Unlike OPEN-GRAPH, MAKE-GRAPH attaches the declared vertex
             ;; types but not the declared views.
             (graph-db:install-views graph)
             graph)))))

(defun close-store (graph)
  "Close GRAPH, a store from OPEN-STORE, unless it is NIL.  Returns NIL."
  (when graph
    (graph-db:close-graph graph :snapshot-p nil))
  nil)

(defmacro with-transaction ((graph &key (context "transaction")) &body body)
  "Run BODY in a graph-db transaction on GRAPH, logging an error that escapes
it, tagged with CONTEXT, before passing it on."
  (with-gensyms (condition)
    `(let ((graph-db:*graph* ,graph))
       (handler-bind ((error (lambda (,condition)
                               (format *error-output* "database: ~A failed: ~A~%"
                                       ,context ,condition))))
         (graph-db:with-transaction ()
           ,@body)))))
