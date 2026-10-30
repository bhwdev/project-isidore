;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/main
  (:use #:common-lisp)
  (:import-from #:bordeaux-threads)
  (:import-from #:cffi)
  (:import-from #:cl+ssl)
  (:import-from #:clog #:initialize #:is-running-p #:shutdown)
  (:import-from #:dexador)
  (:import-from #:ironclad)
  (:import-from #:slynk)
  (:import-from #:usocket)
  (:local-nicknames (#:conn #:clog-connection)
                    (#:legacy #:project-isidore/database)
                    (#:pages #:project-isidore/site/pages)
                    (#:bible-db #:project-isidore/bible/database)
                    (#:bible #:project-isidore/bible/text)
                    (#:reader #:project-isidore/bible/reader)
                    (#:bot-db #:project-isidore/slack-bot/database)
                    (#:bot #:project-isidore/slack-bot/bot))
  (:export #:initialize-application
           #:terminate-application
           #:application-toplevel)
  (:documentation
   "Project Isidore's one process: the personal site, the bible reader and the
Slack bot, served by one CLOG (Hunchentoot) web server.

INITIALIZE-APPLICATION opens each part's database, starts the bot's
reminder scheduler and the web server, and has each part register its pages;
the bot's endpoints are Lack middleware in front of CLOG.  Loading the system
does none of this, so loading has no side effects and the build can save the
image straight after loading it.  APPLICATION-TOPLEVEL is the executable's
entry point."))

(in-package #:project-isidore/main)

(defvar *server* nil
  "Non-NIL when the CLOG web server is running.")

;; Override CLOG-CONNECTION::RANDOM-HEX-STRING.  Upstream's POSIX path uses
;; cl-isaac's RAND32, which unconditionally DECFs an (UNSIGNED-BYTE 32)
;; struct slot before checking for zero.  The slot writer signals a
;; TYPE-ERROR on the underflow, wedging the PRNG -- every subsequent call
;; retraps, so CLOG can no longer mint connection IDs and fresh browser
;; sessions never receive the bootstrap frame.  CLOG's own Windows branch
;; already uses ironclad; we extend that to all platforms.
(defun conn::random-hex-string ()
  "Generate a cryptographic 128-bit random hex string for connection IDs."
  (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))

(defun wait-for-port-ready (port &key (host "127.0.0.1") (timeout 5.0) (interval 0.05))
  "Block until the web server on HOST:PORT has fully started, or TIMEOUT
seconds elapse.  Returns T on success, NIL on timeout.

CLOG's INITIALIZE returns as soon as clack:clackup hands back a handler
struct, but the underlying hunchentoot acceptor finishes START in a separate
thread, in two steps: START-LISTENING binds the socket, so the port accepts
connections, and only then does EXECUTE-ACCEPTOR store the listener thread in
the taskmaster's ACCEPTOR-PROCESS slot.  If TERMINATE-APPLICATION runs between
those steps, clack's BT2:DESTROY-THREAD cleanup calls HUNCHENTOOT:STOP, whose
SHUTDOWN reads the still-unbound slot and signals UNBOUND-SLOT in that thread,
and an unhandled error in any thread of a non-interactive SBCL, such as the
test suite's, terminates the whole process.  So this waits for both signs:
the port accepting connections and a \"hunchentoot-listener-HOST:PORT\"
thread existing, plus one more INTERVAL for the slot assignment that
immediately follows the thread's creation."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second)))
        (listener-name (format nil "hunchentoot-listener-~A:~A" host port)))
    (flet ((port-ready-p ()
             (handler-case
                 (let ((sock (usocket:socket-connect host port)))
                   (usocket:socket-close sock)
                   t)
               (error () nil)))
           (listener-thread-p ()
             (find listener-name (bordeaux-threads:all-threads)
                   :key #'bordeaux-threads:thread-name :test #'equal))
           (expired-p ()
             (> (get-internal-real-time) deadline)))
      (loop until (and (port-ready-p) (listener-thread-p))
            do (when (expired-p)
                 (return-from wait-for-port-ready nil))
               (sleep interval))
      (sleep interval)
      t)))

(defun initialize-application (&key (port 8080))
  "Open the databases, start the Slack bot's reminder scheduler and serve the
site, the bible reader and the bot's endpoints on PORT, on localhost."
  (terminate-application)
  ;; One-time: copies the pre-split stores, then never runs again.  Remove
  ;; with src/database.lisp.
  (legacy:migrate-stores)
  (bible-db:open-database)
  (bible:open-search-index)
  (bot-db:open-database)
  (bot:load-config)
  (bot:start-reminder-scheduler)
  (initialize 'pages:on-index-handler
              :host "127.0.0.1"
              :port port
              :extended-routing t
              :lack-middleware-list (list (bot:make-slack-middleware))
              :static-root (namestring
                            (asdf:system-relative-pathname :project-isidore "./")))
  (pages:register-routes)
  (reader:register-routes)
  ;; CLOG's own JS files (boot.js, jquery)
  (conn:add-plugin-path "^/js/"
                        (merge-pathnames "static-files/"
                                         (asdf:system-source-directory :clog)))
  (conn:add-plugin-path "^/assets/"
                        (namestring (asdf:system-relative-pathname :project-isidore "./")))
  (conn:add-plugin-path "^/favicon\\.ico"
                        (namestring (asdf:system-relative-pathname :project-isidore "assets/")))
  (setf *server* t)
  (format t "~%
========================================
Project Isidore
========================================
Copyright (c) 2021  Ben H. W. <foss@bhw.name>
Project Isidore is free software, provided as is, and comes with ABSOLUTELY NO
WARRANTY. This program is licensed under the GNU AFFERO GENERAL PUBLIC LICENSE
Version 3, 19 November 2007 You are welcome to redistribute the program or any
parts hereof under certain conditions. Please visit
https://www.gnu.org/licenses/agpl-3.0.html for License details.
Homepage: https://www.bhw.name/assets/blog/project-isidore-doc.html
Source code repository: https://github.com/bhwdev/project-isidore
Navigate to http://localhost:~A to continue... ~%" port)
  (wait-for-port-ready port)
  t)

(defun terminate-application ()
  "Stop what INITIALIZE-APPLICATION started and close the databases.
Idempotent."
  (bot:stop-reminder-scheduler)
  (when (or *server* (is-running-p))
    (ignore-errors (shutdown))
    (setf *server* nil))
  (bible:close-search-index)
  (bible-db:close-database)
  (bot-db:close-database)
  t)

(defun reset-network-state ()
  "Drop the foreign state a saved image inherits from the process that built
it: cl+ssl's foreign pointers (its Lisp BIO method, the SSL_CTX, the C string
it hands to OpenSSL's error queue, which it allocates when it loads) and
dexador's pool of keep-alive connections.  In the restarted image those
addresses and sockets belong to nobody: OpenSSL faults on them (\"CORRUPTION
WARNING ... Memory fault\" with the PC in libcrypto's BIO_new_ex) and a
request to a pooled host fails.  Registered below as a UIOP image-restore
hook, which runs when the saved image starts."
  ;; forget the SSL_CTX and method so the first TLS use re-initialises cl+ssl,
  ;; which also makes a fresh Lisp BIO method
  (cl+ssl:reload)
  (when (boundp 'cl+ssl::*file-name*)
    (setf cl+ssl::*file-name* (cffi:foreign-string-alloc "cl+ssl/src/bio.lisp")))
  ;; a new, empty pool: closing the inherited connections would call OpenSSL
  ;; on the same stale pointers
  (setf dexador.connection-cache:*connection-pool*
        (dexador.connection-cache:make-connection-pool)))

(uiop:register-image-restore-hook 'reset-network-state nil)

(defun application-toplevel ()
  "The executable's entry point: serve on $PORT (default 8080) until SIGINT.
On the default port a Slynk server on localhost:4005 allows a remote REPL."
  (let ((port (let ((env (uiop:getenvp "PORT")))
                (if env (parse-integer env) 8080))))
    (when (= 8080 port)
      (slynk:create-server :port 4005 :dont-close t)
      (setf slynk:*use-dedicated-output-stream* nil))
    (initialize-application :port port)
    (format t "~% Close this window or press Control+C to exit the program...~%")
    (handler-case (loop (sleep 1))
      (sb-sys:interactive-interrupt ()
        (format *error-output* "~%Aborting.~&~%")
        (slynk:stop-server 4005)
        (terminate-application)
        (format t "~%Server successfully stopped.~%")
        (uiop:quit)))))
