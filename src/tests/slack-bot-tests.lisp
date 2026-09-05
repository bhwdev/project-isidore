;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/slack-bot-tests
  (:use #:common-lisp #:parachute)
  (:local-nicknames (#:sb #:project-isidore/slack-bot)
                    (#:db #:project-isidore/database)
                    (#:vb #:project-isidore/van-booking)
                    (#:bn #:project-isidore/booking-nlp))
  (:import-from #:project-isidore/tests/test-utils #:with-mocked-functions)
  (:import-from #:alexandria #:plist-hash-table)
  (:import-from #:bordeaux-threads)
  (:import-from #:com.inuoe.jzon)
  (:import-from #:flexi-streams)
  (:import-from #:ironclad)
  (:documentation
   "Parachute tests for project-isidore/slack-bot.

Covers pure helpers (form decoding, initial parsing, dispatch table,
docstring-driven help output, reminder-key determinism, date-rollover
reset), request signature verification, the Lack middleware and both
endpoints, the slash-command handlers, message events, the reminder
scheduler tick, and graph-db-backed subscription CRUD.  Everything that
would hit the Slack API or Google Sheets is mocked with
WITH-MOCKED-FUNCTIONS."))

(in-package #:project-isidore/tests/slack-bot-tests)

(define-test slack-bot-suite
  :description "Slack bot unit tests.")

;;; --- Pure helpers ---

(define-test decode-form-body
  :parent slack-bot-suite
  (let ((ht (sb::decode-form-body
             "command=%2Fsubscribe&text=BW&user_id=U999&channel_id=C1")))
    (is string= "/subscribe" (gethash "command" ht))
    (is string= "BW"         (gethash "text" ht))
    (is string= "U999"       (gethash "user_id" ht))
    (is string= "C1"         (gethash "channel_id" ht)))
  ;; URL-encoded spaces and plus signs
  (let ((ht (sb::decode-form-body "text=book+a+van+for+swim")))
    (is string= "book a van for swim" (gethash "text" ht)))
  ;; Empty body yields empty table
  (is = 0 (hash-table-count (sb::decode-form-body ""))))

(define-test parse-initial
  :parent slack-bot-suite
  (is string= "BW" (sb::parse-initial "BW"))
  (is string= "BW" (sb::parse-initial "   bw no wait BW please"))
  (is string= "CGH" (sb::parse-initial "CGH"))
  (false (sb::parse-initial ""))
  (false (sb::parse-initial "lowercase only"))
  (false (sb::parse-initial nil)))

(define-test slash-commands-table
  :parent slack-bot-suite
  (let ((names (mapcar #'car sb::*slash-commands*)))
    (is equal '("/book" "/view" "/subscribe" "/unsubscribe" "/help") names))
  ;; Every handler has a docstring (developer reference; users get +help-text+).
  (dolist (entry sb::*slash-commands*)
    (true (documentation (cdr entry) 'function)
          "~a has a docstring" (car entry))))

(define-test cmd-help-includes-all-commands
  :parent slack-bot-suite
  ;; /help posts the one user-facing help page, and that page names every
  ;; registered command, so adding a command without documenting it fails here.
  (let ((out (sb::cmd-help (make-hash-table :test #'equal))))
    (is string= sb::+help-text+ out)
    (dolist (entry sb::*slash-commands*)
      (true (search (car entry) out) "help output mentions ~a" (car entry)))
    ;; no developer notation leaks into what users read
    (dolist (jargon '("<" ">" "[" "]" "docstring" "argument"))
      (false (search jargon out) "help output contains ~s" jargon))))

(define-test reply-channel-for
  :parent slack-bot-suite
  (flet ((params (cmd)
           (let ((h (make-hash-table :test #'equal)))
             (setf (gethash "command" h) cmd)
             (setf (gethash "user_id" h) "U1")
             (setf (gethash "channel_id" h) "C1")
             h)))
    (is string= "U1" (sb::reply-channel-for (params "/subscribe")))
    (is string= "U1" (sb::reply-channel-for (params "/unsubscribe")))
    (is string= "C1" (sb::reply-channel-for (params "/book")))
    (is string= "C1" (sb::reply-channel-for (params "/view")))
    (is string= "C1" (sb::reply-channel-for (params "/help")))))

;;; --- Scheduler bookkeeping ---

(define-test today-date-key-matches-decoded-date
  :parent slack-bot-suite
  (multiple-value-bind (s m h day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    (is = (+ (* year 10000) (* month 100) day)
        (sb::today-date-key))))

(define-test sent-reminders-resets-across-days
  :parent slack-bot-suite
  (let ((sb::*sent-reminders* (make-hash-table :test #'equal))
        (sb::*sent-reminders-date* (- (sb::today-date-key) 1)))
    (setf (gethash '(stale) sb::*sent-reminders*) t)
    (sb::maybe-reset-sent-reminders)
    (is = 0 (hash-table-count sb::*sent-reminders*))
    (is = (sb::today-date-key) sb::*sent-reminders-date*))
  ;; Second call on same day is a no-op.
  (let ((sb::*sent-reminders* (make-hash-table :test #'equal))
        (sb::*sent-reminders-date* (sb::today-date-key)))
    (setf (gethash '(keep) sb::*sent-reminders*) t)
    (sb::maybe-reset-sent-reminders)
    (is = 1 (hash-table-count sb::*sent-reminders*))))

;;; --- Subscription CRUD (graph-db round-trip) ---
;;;
;;; Each CRUD test runs against a fresh, temporary graph-db store so nothing
;;; leaks into ~/.local/share/project-isidore/ (or wherever
;;; PROJECT_ISIDORE_DATA points). WITH-TEMP-DATABASE handles the open,
;;; the *database* rebind, the slack-subscription schema
;;; registration, and the teardown.

(defparameter +test-user-id+ "__TEST_USER_slack_bot_tests__"
  "Sentinel Slack user_id used inside the temp DB.")

(defmacro with-temp-database (&body body)
  "Open a fresh graph-db CRUD store in a unique temp dir, swap it in for
DB:*DATABASE*, install the SUBSCRIPTION-BY-USER-CI view, run BODY, then
close and delete. The graph is opened under name :ISIDORE-CRUD so the
SLACK-SUBSCRIPTION schema (registered globally at load time) is auto-
instantiated by UPDATE-SCHEMA. The previous binding in graph-db's
*GRAPHS* registry is saved and restored on unwind so subsequent tests
keep seeing the production graph."
  (let ((dir (gensym "DIR"))
        (saved-graph (gensym "SAVED"))
        (saved-db (gensym "SAVED-DB"))
        (g (gensym "G")))
    `(let* ((,dir (uiop:ensure-directory-pathname
                   (format nil "/tmp/pi-crud-test-~A-~A/"
                           (get-universal-time) (random 1000000))))
            (,saved-graph (graph-db:lookup-graph :isidore-crud))
            (,saved-db db:*database*)
            (,g nil))
       (ensure-directories-exist ,dir)
       (unwind-protect
            (let ((db:*database* nil))
              (declare (special db:*database*))
              (setf ,g (db:open-or-make-graph :isidore-crud ,dir))
              (setf db:*database* ,g)
              (db:ensure-crud-views ,g)
              (locally ,@body))
         (when ,g
           (graph-db:close-graph ,g :snapshot-p nil))
         (when ,saved-graph
           (setf (gethash :isidore-crud graph-db::*graphs*) ,saved-graph))
         (setf db:*database* ,saved-db)
         (uiop:delete-directory-tree ,dir :validate (constantly t))))))

(define-test subscription-add-upsert-remove
  :parent slack-bot-suite
  (with-temp-database
      (db:add-slack-subscription +test-user-id+ "BW")
    ;; Direct lookup outside WITH-DB-TRANSACTION, i.e. with GRAPH-DB:*GRAPH*
    ;; unbound: used to fail with VERTEX-TABLE (NIL) once a row existed.
    (let ((found (db:find-slack-subscription +test-user-id+)))
      (true found)
      (is string= "BW" (db:initial-of found)))
    (is equal '(("BW"))
        (mapcar (lambda (s) (list (cdr s)))
                (remove-if-not
                 (lambda (s) (string= (car s) +test-user-id+))
                 (db:all-slack-subscriptions))))
    (db:add-slack-subscription +test-user-id+ "CG")
    (is equal '(("CG"))
        (mapcar (lambda (s) (list (cdr s)))
                (remove-if-not
                 (lambda (s) (string= (car s) +test-user-id+))
                 (db:all-slack-subscriptions))))
    (true (db:remove-slack-subscription +test-user-id+))
    (false (find +test-user-id+ (db:all-slack-subscriptions)
                 :key #'car :test #'string=))
    (false (db:remove-slack-subscription +test-user-id+))))

;;; --- Database split: bible vs CRUD are isolated ---

(define-test database-path-honors-env-var
  :parent slack-bot-suite
  ;; Custom path
  (let ((saved (uiop:getenvp "PROJECT_ISIDORE_DATA")))
    (unwind-protect
         (progn
           (setf (uiop:getenv "PROJECT_ISIDORE_DATA") "/tmp/pi-env-var-test/")
           (is equal (merge-pathnames
                      "graph-db/"
                      (uiop:ensure-directory-pathname "/tmp/pi-env-var-test/"))
               (db:database-path))
           ;; Empty / unset: XDG default + graph-db subdir
           (setf (uiop:getenv "PROJECT_ISIDORE_DATA") "")
           (is equal (merge-pathnames "graph-db/"
                                      (uiop:xdg-data-home "project-isidore/"))
               (db:database-path)))
      (if saved
          (setf (uiop:getenv "PROJECT_ISIDORE_DATA") saved)
          (setf (uiop:getenv "PROJECT_ISIDORE_DATA") "")))))

(defun count-bible-objects ()
  "Count BIBLE vertices in DB:*BIBLE-DATABASE*. Binds GRAPH-DB:*GRAPH*
because MAP-VERTICES' :VERTEX-TYPE branch routes through
LOOKUP-NODE-TYPE-BY-NAME, which defaults :GRAPH to *GRAPH* (unbound at
top-level)."
  (let ((c 0)
        (graph-db:*graph* db:*bible-database*))
    (graph-db:map-vertices
     (lambda (v) (declare (ignore v)) (incf c))
     db:*bible-database*
     :vertex-type 'project-isidore/database::bible)
    c))

(define-test bible-and-general-databases-are-distinct
  :parent slack-bot-suite
  (false (eq db:*database* db:*bible-database*))
  ;; CRUD writes go to *database*, never *bible-database*. Snapshot bible
  ;; object count before and after a subscription round-trip.
  (let ((bible-count-before (count-bible-objects)))
    (with-temp-database
        (db:add-slack-subscription +test-user-id+ "BW")
      (db:remove-slack-subscription +test-user-id+))
    (let ((bible-count-after (count-bible-objects)))
      (is = bible-count-before bible-count-after)
      (true (> bible-count-before 0)))))

(define-test database-creates-missing-directory
  :parent slack-bot-suite
  (let* ((tmp-root (uiop:ensure-directory-pathname
                    (format nil "/tmp/pi-mkdir-test-~A/" (get-universal-time))))
         (target (uiop:ensure-directory-pathname
                  (merge-pathnames "deeply/nested/subdir/" tmp-root)))
         (graph-db-dir (merge-pathnames "graph-db/" target))
         (saved (uiop:getenvp "PROJECT_ISIDORE_DATA")))
    (unwind-protect
         (progn
           (false (uiop:directory-exists-p graph-db-dir))
           ;; Close the production graph before pointing the env var at a
           ;; temp dir; otherwise REOPEN-DATABASE swaps it out of GRAPH-DB::
           ;; *GRAPHS* without closing it, leaking fd's, mmaps, and the
           ;; replication/transaction-manager threads. Several such leaks
           ;; per test invocation accumulate fast enough to crash SBCL.
           (when db:*database*
             (let ((graph-db:*graph* db:*database*))
               (graph-db:close-graph db:*database* :snapshot-p nil)))
           (setf (uiop:getenv "PROJECT_ISIDORE_DATA") (namestring target))
           (db:reopen-database)
           (true (uiop:directory-exists-p graph-db-dir))
           ;; graph-db's MAKE-GRAPH eagerly creates schema.dat and heap.dat
           ;; under the location; a transaction is not required.
           (let ((files (mapcar #'file-namestring
                                (uiop:directory-files graph-db-dir))))
             (true (member "heap.dat" files :test #'string=))
             (true (member "schema.dat" files :test #'string=))))
      (when db:*database*
        (let ((graph-db:*graph* db:*database*))
          (graph-db:close-graph db:*database* :snapshot-p nil)))
      (if saved
          (setf (uiop:getenv "PROJECT_ISIDORE_DATA") saved)
          (setf (uiop:getenv "PROJECT_ISIDORE_DATA") ""))
      (db:reopen-database)
      (when (uiop:directory-exists-p tmp-root)
        (uiop:delete-directory-tree tmp-root :validate (constantly t))))))

;;; --- Subscribe / unsubscribe handlers (stubs for DB side-effects) ---

(defun make-params (&rest kv)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kv by #'cddr
          do (setf (gethash k h) v))
    h))

(define-test cmd-subscribe-requires-user-id
  :parent slack-bot-suite
  (let ((reply (sb::cmd-subscribe (make-params "text" "BW"))))
    (true (search "Slack user" reply))))

(define-test cmd-subscribe-requires-initial
  :parent slack-bot-suite
  (let ((reply (sb::cmd-subscribe (make-params "user_id" "Uabc" "text" ""))))
    (true (search "Usage" reply))))

(define-test cmd-subscribe-happy-path
  :parent slack-bot-suite
  (with-temp-database
      (let ((reply (sb::cmd-subscribe
                    (make-params "user_id" +test-user-id+
                                 "text" "please use BW thanks"))))
        (true (search "BW" reply))
        (true (search +test-user-id+
                      (format nil "~{~A~%~}"
                              (mapcar #'car (db:all-slack-subscriptions))))))))

(define-test cmd-unsubscribe-when-not-subscribed
  :parent slack-bot-suite
  (with-temp-database
      (let ((reply (sb::cmd-unsubscribe (make-params "user_id" +test-user-id+))))
        (true (search "not subscribed" reply)))))

(define-test cmd-unsubscribe-after-subscribe
  :parent slack-bot-suite
  (with-temp-database
      (sb::cmd-subscribe (make-params "user_id" +test-user-id+ "text" "BW"))
    (let ((reply (sb::cmd-unsubscribe (make-params "user_id" +test-user-id+))))
      (true (search "Unsubscribed" reply)))))

(defun unix-now ()
  "Current time as Unix epoch seconds, as Slack sends it."
  (- (get-universal-time) 2208988800))

(defun sign (secret timestamp body)
  "Compute Slack's v0 request signature for BODY at TIMESTAMP with SECRET."
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array secret) :sha256)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array
                                (format nil "v0:~a:~a" timestamp body)))
    (format nil "v0=~(~{~2,'0X~}~)" (coerce (ironclad:hmac-digest hmac) 'list))))

(defun body-env (string &rest plist)
  "A minimal Lack ENV plist whose raw body is STRING (UTF-8)."
  (list* :raw-body
         (flexi-streams:make-in-memory-input-stream
          (flexi-streams:string-to-octets string :external-format :utf-8))
         plist))

(defun signed-env (path body &key (secret "testsecret") signature extra-headers)
  "A Lack ENV for a Slack POST to PATH with BODY, signed with SECRET (or a
literal SIGNATURE).  EXTRA-HEADERS is a plist of additional header values."
  (let ((ts (princ-to-string (unix-now)))
        (headers (make-hash-table :test #'equal)))
    (setf (gethash "x-slack-request-timestamp" headers) ts
          (gethash "x-slack-signature" headers) (or signature (sign secret ts body)))
    (loop for (k v) on extra-headers by #'cddr
          do (setf (gethash k headers) v))
    (body-env body :request-method :post :path-info path :headers headers)))

(defun message-event (&rest plist)
  "Build a Slack event hash-table from PLIST (\"text\" \"...\" \"channel\" \"C1\" ...)."
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash k h) v))
    h))

(define-test signature-verification
  :parent slack-bot-suite
  (let ((sb::*slack-signing-secret* "testsecret")
        (ts (princ-to-string (unix-now)))
        (body "command=%2Fbook&text=hi"))
    (true (sb::verify-slack-signature ts body (sign "testsecret" ts body)))
    ;; tampered body or wrong secret
    (false (sb::verify-slack-signature ts (concatenate 'string body "x") (sign "testsecret" ts body)))
    (false (sb::verify-slack-signature ts body (sign "othersecret" ts body)))
    ;; stale timestamp (replay protection)
    (let ((old (princ-to-string (- (unix-now) 1000))))
      (false (sb::verify-slack-signature old body (sign "testsecret" old body))))
    ;; missing or malformed pieces
    (false (sb::verify-slack-signature nil body "v0=00"))
    (false (sb::verify-slack-signature ts body nil))
    (false (sb::verify-slack-signature "not-a-number" body "v0=00")))
  ;; no secret configured -> nothing verifies
  (let ((sb::*slack-signing-secret* nil)
        (ts (princ-to-string (unix-now))))
    (false (sb::verify-slack-signature ts "b" (sign "testsecret" ts "b")))))

(define-test request-helpers
  :parent slack-bot-suite
  ;; strip-mention
  (is string= "book a van" (sb::strip-mention "<@U123ABC> book a van"))
  (is string= "book a van" (sb::strip-mention "book a van"))
  (is string= "hi <@U123ABC>" (sb::strip-mention "hi <@U123ABC>"))
  (is string= "" (sb::strip-mention "<@U123ABC>"))
  ;; json-response
  (let ((resp (sb::json-response 200 (plist-hash-table '("challenge" "abc") :test #'equal))))
    (is = 200 (first resp))
    (is equal '(:content-type "application/json") (second resp))
    (is string= "abc" (gethash "challenge" (com.inuoe.jzon:parse (first (third resp))))))
  ;; read-body-string decodes UTF-8 and tolerates a missing body
  (is string= "héllo=wörld" (sb::read-body-string (body-env "héllo=wörld")))
  (is string= "" (sb::read-body-string (body-env "")))
  (false (sb::read-body-string '(:path-info "/x")))
  ;; help text describes the free-form contract
  (dolist (needle '("destination" "day" "passengers" "times" "vehicle" "re-send"
                    "/view" "/subscribe" "/unsubscribe" "/help"))
    (true (search needle sb::+help-text+) "help mentions ~a" needle)))

(define-test middleware-routing
  :parent slack-bot-suite
  (let* ((app-calls '())
         (app (lambda (env) (push (getf env :path-info) app-calls) '(200 () ("app"))))
         (handler (funcall (sb:make-slack-middleware) app)))
    ;; GET passes through, even on the slack paths
    (is equal '(200 () ("app"))
        (funcall handler '(:request-method :get :path-info "/slack/events")))
    ;; POST elsewhere passes through
    (is equal '(200 () ("app"))
        (funcall handler (body-env "x" :request-method :post :path-info "/other")))
    (is equal '("/other" "/slack/events") app-calls)
    ;; POST to the slack paths is handled here: no body -> 400
    (is = 400 (first (funcall handler '(:request-method :post :path-info "/slack/events"))))
    (is = 400 (first (funcall handler '(:request-method :post :path-info "/slack/commands"))))
    (is = 2 (length app-calls))))

(define-test events-endpoint
  :parent slack-bot-suite
  (let ((sb::*slack-signing-secret* "testsecret"))
    ;; the url_verification handshake echoes the challenge as JSON
    (let ((resp (sb::handle-slack-request
                 (signed-env "/slack/events"
                             "{\"type\":\"url_verification\",\"challenge\":\"xyz\"}"))))
      (is = 200 (first resp))
      (is equal '(:content-type "application/json") (second resp))
      (is string= "xyz" (gethash "challenge" (com.inuoe.jzon:parse (first (third resp))))))
    ;; bad or missing signature -> 403; no body -> 400
    (is = 403 (first (sb::handle-slack-request
                      (signed-env "/slack/events" "{\"type\":\"url_verification\"}"
                                  :signature "v0=bad"))))
    (is = 403 (first (sb::handle-slack-request
                      (body-env "{}" :request-method :post :path-info "/slack/events"))))
    (is = 400 (first (sb::handle-slack-request '(:request-method :post :path-info "/slack/events"))))
    ;; retries are acknowledged but not processed; first deliveries dispatch
    (let ((events '()))
      (with-mocked-functions ((sb::handle-message-event (lambda (event) (push event events) nil)))
        (let ((body "{\"type\":\"event_callback\",\"event\":{\"type\":\"message\",\"text\":\"hi\"}}"))
          (is = 200 (first (sb::handle-slack-request
                            (signed-env "/slack/events" body
                                        :extra-headers '("x-slack-retry-num" "1")))))
          (is = 0 (length events))
          (is = 200 (first (sb::handle-slack-request (signed-env "/slack/events" body))))
          (is = 1 (length events))
          (is string= "hi" (gethash "text" (first events))))
        ;; an event_callback without an event is harmless
        (is = 200 (first (sb::handle-slack-request
                          (signed-env "/slack/events" "{\"type\":\"event_callback\"}"))))
        (is = 1 (length events))))
    ;; unknown payload types are acknowledged
    (is = 200 (first (sb::handle-slack-request
                      (signed-env "/slack/events" "{\"type\":\"something_else\"}"))))))

(define-test commands-endpoint
  :parent slack-bot-suite
  (let ((sb::*slack-signing-secret* "testsecret"))
    (is = 403 (first (sb::handle-slash-command
                      (signed-env "/slack/commands" "command=%2Fhelp" :signature "v0=bad"))))
    (is = 400 (first (sb::handle-slash-command '(:request-method :post :path-info "/slack/commands"))))
    (let ((resp (sb::handle-slash-command
                 (signed-env "/slack/commands" "command=%2Fbogus&user_id=U1&channel_id=C1"))))
      (is = 200 (first resp))
      (true (search "Unknown command: /bogus" (first (third resp)))))
    ;; a known command runs in a worker thread and posts its reply to the channel
    (let ((posted '()))
      (with-mocked-functions ((sb::post-slack-message
                               (lambda (channel text) (push (cons channel text) posted))))
        (let ((resp (sb::handle-slash-command
                     (signed-env "/slack/commands" "command=%2Fhelp&user_id=U1&channel_id=C1"))))
          (is = 200 (first resp))
          (loop repeat 100 until posted do (sleep 0.05))
          (is = 1 (length posted))
          (is string= "C1" (car (first posted)))
          (true (search "/book" (cdr (first posted)))))))
    ;; subscribe replies go to the user as a DM
    (let ((posted '()))
      (with-mocked-functions ((sb::post-slack-message
                               (lambda (channel text) (push (cons channel text) posted)))
                              (db:add-slack-subscription (lambda (user-id initial)
                                                           (declare (ignore user-id initial)) t)))
        (sb::handle-slash-command
         (signed-env "/slack/commands" "command=%2Fsubscribe&text=BW&user_id=U7&channel_id=C1"))
        (loop repeat 100 until posted do (sleep 0.05))
        (is = 1 (length posted))
        (is string= "U7" (car (first posted)))
        (true (search "Subscribed" (cdr (first posted))))))))

(define-test cmd-book-replies
  :parent slack-bot-suite
  (true (search "Usage" (sb::cmd-book (make-params "text" ""))))
  (true (search "Usage" (sb::cmd-book (make-params "text" "   "))))
  (true (search "Usage" (sb::cmd-book (make-params))))
  ;; an incomplete booking returns the re-send prompt, not an error banner
  (with-mocked-functions ((vb:book-natural
                           (lambda (text &key week) (declare (ignore week))
                             (error 'bn:booking-incomplete :missing '(:day)
                                                           :understood '(:destination "pool")
                                                           :text text))))
    (let ((reply (sb::cmd-book (make-params "text" "for the pool for MM"))))
      (true (search "Please re-send" reply))
      (true (search "destination: pool" reply))
      (true (search (bn:missing-piece-hint :day) reply))
      (false (search "Booking failed" reply))))
  ;; the booking's printed output is the reply
  (with-mocked-functions ((vb:book-natural
                           (lambda (text &key week) (declare (ignore text week))
                             (format t "Created DGH booking~%"))))
    (true (search "Created DGH booking" (sb::cmd-book (make-params "text" "x")))))
  ;; silent success
  (with-mocked-functions ((vb:book-natural (lambda (text &key week) (declare (ignore text week)) nil)))
    (true (search "no output" (sb::cmd-book (make-params "text" "x")))))
  ;; other errors are reported with their condition type
  (with-mocked-functions ((vb:book-natural (lambda (text &key week) (declare (ignore text week))
                                             (error "boom"))))
    (let ((reply (sb::cmd-book (make-params "text" "x"))))
      (true (search "Booking failed [SIMPLE-ERROR]" reply))
      (true (search "boom" reply))
      (true (search "/help" reply)))))

(define-test cmd-view-replies
  :parent slack-bot-suite
  (with-mocked-functions ((vb:view-schedule
                           (lambda (&optional range) (format t "TABLE for ~a" range) "ignored")))
    (true (search "TABLE for today" (sb::cmd-view (make-params "text" ""))))
    (true (search "TABLE for today" (sb::cmd-view (make-params))))
    (true (search "TABLE for Apr 20 to Apr 26"
                  (sb::cmd-view (make-params "text" " Apr 20 to Apr 26 ")))))
  ;; a multi-line table is posted in code fences (a list of message parts)
  ;; so its columns line up in Slack
  (with-mocked-functions ((vb:view-schedule
                           (lambda (&optional range) (declare (ignore range))
                             (format t "Date  Vehicle~%----  -------~%Mon   DGH~%")
                             "ignored")))
    (let ((reply (sb::cmd-view (make-params "text" "today"))))
      (true (listp reply))
      (is = 1 (length reply))
      (true (uiop:string-prefix-p (format nil "```~%Date  Vehicle~%") (first reply)))
      (true (search "Mon   DGH" (first reply)))
      (true (uiop:string-suffix-p (first reply) "```"))))
  (with-mocked-functions ((vb:view-schedule
                           (lambda (&optional range) (declare (ignore range)) (error "no sheet"))))
    (let ((reply (sb::cmd-view (make-params "text" "x"))))
      (true (search "View failed" reply))
      (true (search "no sheet" reply)))))

(define-test code-block-chunks
  :parent slack-bot-suite
  ;; short text: one fenced part
  (let ((parts (sb::code-block-chunks (format nil "a~%b~%"))))
    (is = 1 (length parts))
    (is string= (format nil "```~%a~%b~%```") (first parts)))
  ;; long text is split on line boundaries under the limit, each part
  ;; repeating the header lines, and no row is lost or duplicated
  (let* ((header (format nil "Date  Vehicle~%----  -------"))
         (rows (loop for i from 1 to 40
                     collect (format nil "row ~2d ~a" i (make-string 60 :initial-element #\x))))
         (text (format nil "~a~%~{~a~%~}" header rows))
         (parts (sb::code-block-chunks text :limit 1000 :header-lines 2)))
    (true (> (length parts) 1))
    (dolist (p parts)
      (true (<= (length p) (+ 1000 8)))
      (true (uiop:string-prefix-p (format nil "```~%Date  Vehicle~%----  -------~%") p))
      (true (uiop:string-suffix-p p "```")))
    (let ((all (apply #'concatenate 'string parts)))
      (dolist (r rows)
        (is = 1 (loop for start = (search r all) then (search r all :start2 (1+ start))
                      while start count t)))))
  ;; a split prefers the blank line between groups (the days of a schedule),
  ;; so no part opens in the middle of a day or with a blank line
  (let* ((header (format nil "  Vehicle  Destination~%  -------  -----------"))
         (groups (loop for d from 1 to 8
                       collect (format nil "Day ~d~%~{~a~%~}" d
                                       (loop for r from 1 to 5
                                             collect (format nil "  ROW ~d-~d ~a" d r
                                                             (make-string 30 :initial-element #\z))))))
         (text (format nil "~a~%~{~a~^~%~}" header groups))
         (parts (sb::code-block-chunks text :limit 700 :header-lines 2)))
    (true (> (length parts) 1))
    (dolist (p parts)
      (let ((lines (uiop:split-string p :separator '(#\Newline))))
        (is string= "```" (first lines))
        (is string= "  Vehicle  Destination" (second lines))
        (true (uiop:string-prefix-p "Day " (fourth lines)))))
    (let ((all (apply #'concatenate 'string parts)))
      (loop for d from 1 to 8
            for needle = (format nil "Day ~d~%" d)
            do (is = 1 (loop for start = (search needle all)
                               then (search needle all :start2 (1+ start))
                             while start count t))))))

(define-test message-events
  :parent slack-bot-suite
  (let ((posted '()))
    (with-mocked-functions ((sb::post-slack-message
                             (lambda (channel text) (push (cons channel text) posted)))
                            (vb:book-natural
                             (lambda (text &key week) (declare (ignore week))
                               (format t "booked ~a" text))))
      ;; ignored events post nothing and return NIL
      (false (sb::handle-message-event (message-event "text" "x" "channel" "C1" "bot_id" "B1")))
      (false (sb::handle-message-event (message-event "text" "x" "channel" "C1" "subtype" "edited")))
      (false (sb::handle-message-event (message-event "text" "   " "channel" "C1")))
      (false (sb::handle-message-event (message-event "channel" "C1")))
      (false (sb::handle-message-event (message-event "text" "x")))
      (is = 0 (length posted))
      ;; help keywords post the help text synchronously
      (false (sb::handle-message-event (message-event "text" "help" "channel" "C1")))
      (false (sb::handle-message-event (message-event "text" "?" "channel" "C1")))
      (is = 2 (length posted))
      (true (search "Van Steward Bot" (cdr (first posted))))
      ;; a booking runs in a worker thread; the mention prefix is stripped
      (let ((thread (sb::handle-message-event
                     (message-event "text" "<@U1> for the pool" "channel" "C2"))))
        (true (bt:threadp thread))
        (bt:join-thread thread)
        (is string= "C2" (car (first posted)))
        (is string= "booked for the pool" (cdr (first posted))))))
  ;; an incomplete booking posts the re-send prompt, not an error banner
  (let ((posted '()))
    (with-mocked-functions ((sb::post-slack-message
                             (lambda (channel text) (declare (ignore channel)) (push text posted)))
                            (vb:book-natural
                             (lambda (text &key week) (declare (ignore week))
                               (error 'bn:booking-incomplete :missing '(:passengers)
                                                             :understood '(:destination "pool")
                                                             :text text))))
      (bt:join-thread (sb::handle-message-event (message-event "text" "for the pool" "channel" "C1")))
      (is = 1 (length posted))
      (true (search "Please re-send" (first posted)))
      (true (search "destination: pool" (first posted)))
      (false (search "Booking failed" (first posted)))))
  ;; silent success and other errors
  (let ((posted '()))
    (with-mocked-functions ((sb::post-slack-message
                             (lambda (channel text) (declare (ignore channel)) (push text posted)))
                            (vb:book-natural (lambda (text &key week) (declare (ignore text week)) nil)))
      (bt:join-thread (sb::handle-message-event (message-event "text" "for the pool" "channel" "C1")))
      (true (search "no output" (first posted))))
    (with-mocked-functions ((sb::post-slack-message
                             (lambda (channel text) (declare (ignore channel)) (push text posted)))
                            (vb:book-natural (lambda (text &key week) (declare (ignore text week))
                                               (error "boom"))))
      (bt:join-thread (sb::handle-message-event (message-event "text" "for the pool" "channel" "C1")))
      (true (search "Booking failed [SIMPLE-ERROR]" (first posted)))
      (true (search "boom" (first posted))))))

(define-test parse-slash-text
  :parent slack-bot-suite
  (multiple-value-bind (cmd args) (sb::parse-slash-text "/view")
    (is string= "/view" cmd)
    (is string= "" args))
  (multiple-value-bind (cmd args) (sb::parse-slash-text "/view  this week and next week ")
    (is string= "/view" cmd)
    (is string= "this week and next week" args))
  ;; an argument line may itself span lines
  (multiple-value-bind (cmd args) (sb::parse-slash-text (format nil "/book a van~%for MM"))
    (is string= "/book" cmd)
    (is string= (format nil "a van~%for MM") args))
  ;; plain requests, and a slash that is not a command word, stay bookings
  (false (sb::parse-slash-text "book a van for MM"))
  (false (sb::parse-slash-text "9/4 trip to town"))
  (false (sb::parse-slash-text "/ view")))

(define-test message-event-slash-commands
  :parent slack-bot-suite
  ;; A client that does not expand slash commands sends "/view" as message
  ;; text; it must show the schedule, not book a trip to "view".
  (let ((posted '())
        (booked '()))
    (with-mocked-functions ((sb::post-slack-message
                             (lambda (channel text) (push (cons channel text) posted)))
                            (vb:book-natural
                             (lambda (text &key week) (declare (ignore week))
                               (push text booked)
                               (format t "booked ~a" text)))
                            (vb:view-schedule
                             (lambda (&optional range)
                               (format t "TABLE for ~a" range) "ignored")))
      (bt:join-thread (sb::handle-message-event
                       (message-event "text" "/view" "channel" "C1" "user" "U1")))
      (is = 0 (length booked))
      (is string= "C1" (car (first posted)))
      (true (search "TABLE for today" (cdr (first posted))))
      ;; the rest of the line is the date range, mention prefix and all
      (bt:join-thread (sb::handle-message-event
                       (message-event "text" "<@U9> /view tomorrow" "channel" "C1" "user" "U1")))
      (true (search "TABLE for tomorrow" (cdr (first posted))))
      (is = 0 (length booked))
      ;; /book still books, with the command word stripped
      (bt:join-thread (sb::handle-message-event
                       (message-event "text" "/book for the pool" "channel" "C1" "user" "U1")))
      (is equal '("for the pool") booked)
      ;; a mistyped command is named rather than booked
      (false (sb::handle-message-event
              (message-event "text" "/vew tomorrow" "channel" "C1" "user" "U1")))
      (is equal '("for the pool") booked)
      (true (search "Unknown command: /vew" (cdr (first posted)))))))

(define-test message-event-subscribe-uses-event-user
  :parent slack-bot-suite
  ;; /subscribe as message text identifies the user from the event, since
  ;; there is no slash-command payload to carry user_id.
  (let ((posted '())
        (subscribed '()))
    (with-mocked-functions ((sb::post-slack-message
                             (lambda (channel text) (push (cons channel text) posted)))
                            (db:add-slack-subscription
                             (lambda (user-id initial)
                               (push (cons user-id initial) subscribed) t)))
      (bt:join-thread (sb::handle-message-event
                       (message-event "text" "/subscribe BW" "channel" "D1" "user" "U7")))
      (is equal '(("U7" . "BW")) subscribed)
      ;; the reply goes back to the conversation the message came from
      (is string= "D1" (car (first posted)))
      (true (search "Subscribed" (cdr (first posted)))))))

(define-test scheduler-tick-window
  :parent slack-bot-suite
  (let* ((trip (vb:make-trip :day "Monday" :vehicle :dgh :destination "SHMS"
                             :departure "8:00 am" :block-start-row 3
                             :dep-passengers '("BW")))
         (dep-offset 100)
         (posted '())
         (sb::*sent-reminders* (make-hash-table :test #'equal))
         (sb::*sent-reminders-date* (sb::today-date-key)))
    (with-mocked-functions ((db:all-slack-subscriptions
                             (lambda () '(("U1" . "BW"))))
                            (vb:trips-for-driver-today
                             (lambda (initial) (declare (ignore initial)) (list trip)))
                            (vb:trip-departure-universal-time
                             (lambda (tr) (declare (ignore tr)) (+ (get-universal-time) dep-offset)))
                            (vb:remind-driver
                             (lambda (initial &optional day)
                               (declare (ignore day))
                               (format nil "reminder for ~a" initial)))
                            (sb::post-slack-message
                             (lambda (channel text) (push (cons channel text) posted))))
      ;; inside the 5-minute window -> one DM to the subscriber
      (sb::scheduler-tick)
      (is = 1 (length posted))
      (is string= "U1" (car (first posted)))
      (is string= "reminder for BW" (cdr (first posted)))
      ;; the same trip on the next tick is deduplicated
      (sb::scheduler-tick)
      (is = 1 (length posted))
      ;; outside the window (too far ahead, or already departed) -> nothing
      (clrhash sb::*sent-reminders*)
      (setf dep-offset 400)
      (sb::scheduler-tick)
      (is = 1 (length posted))
      (setf dep-offset -10)
      (sb::scheduler-tick)
      (is = 1 (length posted))
      ;; the window's far edge is inclusive
      (setf dep-offset 355)
      (sb::scheduler-tick)
      (is = 2 (length posted))
      ;; a trip without a parseable departure is skipped
      (clrhash sb::*sent-reminders*)
      (with-mocked-functions ((vb:trip-departure-universal-time (lambda (tr) (declare (ignore tr)) nil)))
        (sb::scheduler-tick)
        (is = 2 (length posted))))
    ;; errors for one subscription are logged, not raised
    (with-mocked-functions ((db:all-slack-subscriptions (lambda () '(("U1" . "BW"))))
                            (vb:trips-for-driver-today (lambda (initial) (declare (ignore initial))
                                                         (error "sheet unavailable"))))
      (let ((*error-output* (make-string-output-stream)))
        (finish (sb::scheduler-tick))
        (true (search "sheet unavailable" (get-output-stream-string *error-output*)))))))

(define-test reminder-key-shape
  :parent slack-bot-suite
  (let ((a (vb:make-trip :vehicle :dgh :block-start-row 3))
        (b (vb:make-trip :vehicle :lgh :block-start-row 3))
        (c (vb:make-trip :vehicle :dgh :block-start-row 18)))
    (is equal (sb::reminder-key "U1" a) (sb::reminder-key "U1" a))
    (false (equal (sb::reminder-key "U1" a) (sb::reminder-key "U2" a)))
    (false (equal (sb::reminder-key "U1" a) (sb::reminder-key "U1" b)))
    (false (equal (sb::reminder-key "U1" a) (sb::reminder-key "U1" c)))
    (is = (sb::today-date-key) (fourth (sb::reminder-key "U1" a)))))
