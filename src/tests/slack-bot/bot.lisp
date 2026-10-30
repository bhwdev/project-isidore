;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/slack-bot/bot
  (:use #:common-lisp)
  (:import-from #:rove #:deftest #:ok #:ng #:fail)
  (:local-nicknames (#:sb #:project-isidore/slack-bot/bot)
                    (#:db #:project-isidore/slack-bot/database)
                    (#:vb #:project-isidore/slack-bot/van-booking)
                    (#:bn #:project-isidore/slack-bot/booking-nlp))
  (:import-from #:project-isidore/tests/test-utils
                #:with-mocked-functions #:with-temp-data-root)
  (:import-from #:alexandria #:plist-hash-table)
  (:import-from #:bordeaux-threads)
  (:import-from #:com.inuoe.jzon)
  (:import-from #:flexi-streams)
  (:import-from #:ironclad)
  (:documentation
   "Tests for project-isidore/slack-bot/bot.

Covers pure helpers (form decoding, initial parsing, dispatch table,
docstring-driven help output, reminder-key determinism, date-rollover
reset), request signature verification, the Lack middleware and both
endpoints, the slash-command handlers, message events, the reminder
scheduler tick, and graph-db-backed subscription CRUD.  Everything that
would hit the Slack API or Google Sheets is mocked with
WITH-MOCKED-FUNCTIONS."))

(in-package #:project-isidore/tests/slack-bot/bot)

;;; --- Pure helpers ---

(deftest decode-form-body
  (let ((ht (sb::decode-form-body
             "command=%2Fsubscribe&text=BW&user_id=U999&channel_id=C1")))
    (ok (string= "/subscribe" (gethash "command" ht)))
    (ok (string= "BW"         (gethash "text" ht)))
    (ok (string= "U999"       (gethash "user_id" ht)))
    (ok (string= "C1"         (gethash "channel_id" ht))))
  ;; URL-encoded spaces and plus signs
  (let ((ht (sb::decode-form-body "text=book+a+van+for+swim")))
    (ok (string= "book a van for swim" (gethash "text" ht))))
  ;; Empty body yields empty table
  (ok (= 0 (hash-table-count (sb::decode-form-body "")))))

(deftest parse-initial
  (ok (string= "BW" (sb::parse-initial "BW")))
  (ok (string= "BW" (sb::parse-initial "   bw no wait BW please")))
  (ok (string= "CGH" (sb::parse-initial "CGH")))
  (ng (sb::parse-initial ""))
  (ng (sb::parse-initial "lowercase only"))
  (ng (sb::parse-initial nil)))

(deftest slash-commands-table
  (let ((names (mapcar #'car sb::*slash-commands*)))
    (ok (equal '("/book" "/view" "/iam" "/subscribe" "/unsubscribe" "/help") names)))
  ;; Every handler has a docstring (developer reference; users get +help-text+).
  (dolist (entry sb::*slash-commands*)
    (ok (documentation (cdr entry) 'function) (format nil "~a has a docstring" (car entry)))))

(deftest cmd-help-includes-all-commands
  ;; /help posts the one user-facing help page, and that page names every
  ;; registered command, so adding a command without documenting it fails here.
  (let ((out (sb::cmd-help (make-hash-table :test #'equal))))
    (ok (string= sb::+help-text+ out))
    (dolist (entry sb::*slash-commands*)
      (ok (search (car entry) out) (format nil "help output mentions ~a" (car entry))))
    ;; no developer notation leaks into what users read
    (dolist (jargon '("<" ">" "[" "]" "docstring" "argument"))
      (ng (search jargon out) (format nil "help output contains ~s" jargon)))))

(deftest reply-channel-for
  (flet ((params (cmd)
           (let ((h (make-hash-table :test #'equal)))
             (setf (gethash "command" h) cmd)
             (setf (gethash "user_id" h) "U1")
             (setf (gethash "channel_id" h) "C1")
             h)))
    (ok (string= "U1" (sb::reply-channel-for (params "/subscribe"))))
    (ok (string= "U1" (sb::reply-channel-for (params "/unsubscribe"))))
    (ok (string= "C1" (sb::reply-channel-for (params "/book"))))
    (ok (string= "C1" (sb::reply-channel-for (params "/view"))))
    (ok (string= "C1" (sb::reply-channel-for (params "/help"))))))

;;; --- Scheduler bookkeeping ---

(deftest today-date-key-matches-decoded-date
  (multiple-value-bind (s m h day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    (ok (= (+ (* year 10000) (* month 100) day)
           (sb::today-date-key)))))

(deftest sent-reminders-resets-across-days
  (let ((sb::*sent-reminders* (make-hash-table :test #'equal))
        (sb::*sent-reminders-date* (- (sb::today-date-key) 1)))
    (setf (gethash '(stale) sb::*sent-reminders*) t)
    (sb::maybe-reset-sent-reminders)
    (ok (= 0 (hash-table-count sb::*sent-reminders*)))
    (ok (= (sb::today-date-key) sb::*sent-reminders-date*)))
  ;; Second call on same day is a no-op.
  (let ((sb::*sent-reminders* (make-hash-table :test #'equal))
        (sb::*sent-reminders-date* (sb::today-date-key)))
    (setf (gethash '(keep) sb::*sent-reminders*) t)
    (sb::maybe-reset-sent-reminders)
    (ok (= 1 (hash-table-count sb::*sent-reminders*)))))

;;; --- Subscription CRUD (graph-db round-trip) ---
;;;
;;; Each CRUD test runs against a fresh store under a temporary data root, so
;;; nothing leaks into ~/.local/share/project-isidore/ (or wherever
;;; PROJECT_ISIDORE_DATA points).

(defparameter +test-user-id+ "__TEST_USER_slack_bot_tests__"
  "Sentinel Slack user_id used inside the temp DB.")

(defmacro with-temp-database (&body body)
  "Run BODY with the bot's store open, fresh, under a temporary data root."
  `(with-temp-data-root ()
     (db:open-database)
     (unwind-protect (progn ,@body)
       (db:close-database))))

(deftest subscription-add-upsert-remove
  (with-temp-database
    (db:add-slack-subscription +test-user-id+ "BW")
    ;; Direct lookup outside a transaction, i.e. with GRAPH-DB:*GRAPH*
    ;; unbound: used to fail with VERTEX-TABLE (NIL) once a row existed.
    (let ((found (db:find-slack-subscription +test-user-id+)))
      (ok found)
      (ok (string= "BW" (db:initial-of found))))
    (ok (equal '(("BW"))
               (mapcar (lambda (s) (list (cdr s)))
                       (remove-if-not
                        (lambda (s) (string= (car s) +test-user-id+))
                        (db:all-slack-subscriptions)))))
    (db:add-slack-subscription +test-user-id+ "CG")
    (ok (equal '(("CG"))
               (mapcar (lambda (s) (list (cdr s)))
                       (remove-if-not
                        (lambda (s) (string= (car s) +test-user-id+))
                        (db:all-slack-subscriptions)))))
    (ok (db:remove-slack-subscription +test-user-id+))
    (ng (find +test-user-id+ (db:all-slack-subscriptions)
              :key #'car :test #'string=))
    (ng (db:remove-slack-subscription +test-user-id+))))

;;; --- Where the store lives ---

(deftest store-path-honors-env-var
  (let ((saved (uiop:getenvp "PROJECT_ISIDORE_DATA")))
    (unwind-protect
         (progn
           (setf (uiop:getenv "PROJECT_ISIDORE_DATA") "/tmp/pi-env-var-test/")
           (ok (equal #p"/tmp/pi-env-var-test/slack-bot/" (db:store-path)))
           ;; Empty / unset: the XDG default
           (setf (uiop:getenv "PROJECT_ISIDORE_DATA") "")
           (ok (equal (uiop:xdg-data-home "project-isidore/slack-bot/")
                      (db:store-path))))
      (setf (uiop:getenv "PROJECT_ISIDORE_DATA") (or saved "")))))

(deftest open-database-creates-missing-directory
  (with-temp-data-root (root)
    (let ((store (uiop:subpathname root "slack-bot/")))
      (ng (uiop:directory-exists-p store))
      (db:open-database)
      (unwind-protect
           ;; MAKE-GRAPH creates schema.dat and heap.dat eagerly; no
           ;; transaction is needed.
           (let ((files (mapcar #'file-namestring (uiop:directory-files store))))
             (ok (member "heap.dat" files :test #'string=))
             (ok (member "schema.dat" files :test #'string=)))
        (db:close-database)))))

;;; --- Subscribe / unsubscribe handlers (stubs for DB side-effects) ---

(defun make-params (&rest kv)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kv by #'cddr
          do (setf (gethash k h) v))
    h))

(deftest cmd-subscribe-requires-user-id
  (let ((reply (sb::cmd-subscribe (make-params "text" "BW"))))
    (ok (search "Slack user" reply))))

(deftest cmd-subscribe-requires-initial
  (let ((reply (sb::cmd-subscribe (make-params "user_id" "Uabc" "text" ""))))
    (ok (search "Usage" reply))))

(deftest cmd-subscribe-happy-path
  (with-temp-database
      (let ((reply (sb::cmd-subscribe
                    (make-params "user_id" +test-user-id+
                                 "text" "please use BW thanks"))))
        (ok (search "BW" reply))
        (ok (search +test-user-id+
                    (format nil "~{~A~%~}"
                            (mapcar #'car (db:all-slack-subscriptions))))))))

(deftest cmd-unsubscribe-when-not-subscribed
  (with-temp-database
      (let ((reply (sb::cmd-unsubscribe (make-params "user_id" +test-user-id+))))
        (ok (search "not subscribed" reply)))))

(deftest cmd-unsubscribe-after-subscribe
  (with-temp-database
      (sb::cmd-subscribe (make-params "user_id" +test-user-id+ "text" "BW"))
    (let ((reply (sb::cmd-unsubscribe (make-params "user_id" +test-user-id+))))
      (ok (search "Unsubscribed" reply)))))

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

(deftest signature-verification
  (let ((sb::*slack-signing-secret* "testsecret")
        (ts (princ-to-string (unix-now)))
        (body "command=%2Fbook&text=hi"))
    (ok (sb::verify-slack-signature ts body (sign "testsecret" ts body)))
    ;; tampered body or wrong secret
    (ng (sb::verify-slack-signature ts (concatenate 'string body "x") (sign "testsecret" ts body)))
    (ng (sb::verify-slack-signature ts body (sign "othersecret" ts body)))
    ;; stale timestamp (replay protection)
    (let ((old (princ-to-string (- (unix-now) 1000))))
      (ng (sb::verify-slack-signature old body (sign "testsecret" old body))))
    ;; missing or malformed pieces
    (ng (sb::verify-slack-signature nil body "v0=00"))
    (ng (sb::verify-slack-signature ts body nil))
    (ng (sb::verify-slack-signature "not-a-number" body "v0=00")))
  ;; no secret configured -> nothing verifies
  (let ((sb::*slack-signing-secret* nil)
        (ts (princ-to-string (unix-now))))
    (ng (sb::verify-slack-signature ts "b" (sign "testsecret" ts "b")))))

(deftest request-helpers
  ;; strip-mention
  (ok (string= "book a van" (sb::strip-mention "<@U123ABC> book a van")))
  (ok (string= "book a van" (sb::strip-mention "book a van")))
  (ok (string= "hi <@U123ABC>" (sb::strip-mention "hi <@U123ABC>")))
  (ok (string= "" (sb::strip-mention "<@U123ABC>")))
  ;; json-response
  (let ((resp (sb::json-response 200 (plist-hash-table '("challenge" "abc") :test #'equal))))
    (ok (= 200 (first resp)))
    (ok (equal '(:content-type "application/json") (second resp)))
    (ok (string= "abc" (gethash "challenge" (com.inuoe.jzon:parse (first (third resp)))))))
  ;; read-body-string decodes UTF-8 and tolerates a missing body
  (ok (string= "héllo=wörld" (sb::read-body-string (body-env "héllo=wörld"))))
  (ok (string= "" (sb::read-body-string (body-env ""))))
  (ng (sb::read-body-string '(:path-info "/x")))
  ;; help text describes the free-form contract
  (dolist (needle '("destination" "day" "passengers" "times" "vehicle" "re-send"
                    "/view" "/subscribe" "/unsubscribe" "/help"))
    (ok (search needle sb::+help-text+) (format nil "help mentions ~a" needle))))

(deftest middleware-routing
  (let* ((app-calls '())
         (app (lambda (env) (push (getf env :path-info) app-calls) '(200 () ("app"))))
         (handler (funcall (sb:make-slack-middleware) app)))
    ;; GET passes through, even on the slack paths
    (ok (equal '(200 () ("app"))
               (funcall handler '(:request-method :get :path-info "/slack/events"))))
    ;; POST elsewhere passes through
    (ok (equal '(200 () ("app"))
               (funcall handler (body-env "x" :request-method :post :path-info "/other"))))
    (ok (equal '("/other" "/slack/events") app-calls))
    ;; POST to the slack paths is handled here: no body -> 400
    (ok (= 400 (first (funcall handler '(:request-method :post :path-info "/slack/events")))))
    (ok (= 400 (first (funcall handler '(:request-method :post :path-info "/slack/commands")))))
    (ok (= 2 (length app-calls)))))

(deftest events-endpoint
  (let ((sb::*slack-signing-secret* "testsecret"))
    ;; the url_verification handshake echoes the challenge as JSON
    (let ((resp (sb::handle-slack-request
                 (signed-env "/slack/events"
                             "{\"type\":\"url_verification\",\"challenge\":\"xyz\"}"))))
      (ok (= 200 (first resp)))
      (ok (equal '(:content-type "application/json") (second resp)))
      (ok (string= "xyz" (gethash "challenge" (com.inuoe.jzon:parse (first (third resp)))))))
    ;; bad or missing signature -> 403; no body -> 400
    (ok (= 403 (first (sb::handle-slack-request
                       (signed-env "/slack/events" "{\"type\":\"url_verification\"}"
                                   :signature "v0=bad")))))
    (ok (= 403 (first (sb::handle-slack-request
                       (body-env "{}" :request-method :post :path-info "/slack/events")))))
    (ok (= 400 (first (sb::handle-slack-request '(:request-method :post :path-info "/slack/events")))))
    ;; retries are acknowledged but not processed; first deliveries dispatch
    (let ((events '()))
      (with-mocked-functions ((sb::handle-message-event (lambda (event) (push event events) nil)))
        (let ((body "{\"type\":\"event_callback\",\"event\":{\"type\":\"message\",\"text\":\"hi\"}}"))
          (ok (= 200 (first (sb::handle-slack-request
                             (signed-env "/slack/events" body
                                         :extra-headers '("x-slack-retry-num" "1"))))))
          (ok (= 0 (length events)))
          (ok (= 200 (first (sb::handle-slack-request (signed-env "/slack/events" body)))))
          (ok (= 1 (length events)))
          (ok (string= "hi" (gethash "text" (first events)))))
        ;; an event_callback without an event is harmless
        (ok (= 200 (first (sb::handle-slack-request
                           (signed-env "/slack/events" "{\"type\":\"event_callback\"}")))))
        (ok (= 1 (length events)))))
    ;; unknown payload types are acknowledged
    (ok (= 200 (first (sb::handle-slack-request
                       (signed-env "/slack/events" "{\"type\":\"something_else\"}")))))))

(deftest commands-endpoint
  (let ((sb::*slack-signing-secret* "testsecret"))
    (ok (= 403 (first (sb::handle-slash-command
                       (signed-env "/slack/commands" "command=%2Fhelp" :signature "v0=bad")))))
    (ok (= 400 (first (sb::handle-slash-command '(:request-method :post :path-info "/slack/commands")))))
    (let ((resp (sb::handle-slash-command
                 (signed-env "/slack/commands" "command=%2Fbogus&user_id=U1&channel_id=C1"))))
      (ok (= 200 (first resp)))
      (ok (search "Unknown command: /bogus" (first (third resp)))))
    ;; a known command runs in a worker thread and posts its reply to the channel
    (let ((posted '()))
      (with-mocked-functions ((sb::post-slack-message
                               (lambda (channel text) (push (cons channel text) posted))))
        (let ((resp (sb::handle-slash-command
                     (signed-env "/slack/commands" "command=%2Fhelp&user_id=U1&channel_id=C1"))))
          (ok (= 200 (first resp)))
          (loop repeat 100 until posted do (sleep 0.05))
          (ok (= 1 (length posted)))
          (ok (string= "C1" (car (first posted))))
          (ok (search "/book" (cdr (first posted)))))))
    ;; subscribe replies go to the user as a DM
    (let ((posted '()))
      (with-mocked-functions ((sb::post-slack-message
                               (lambda (channel text) (push (cons channel text) posted)))
                              (db:add-slack-subscription (lambda (user-id initial)
                                                           (declare (ignore user-id initial)) t))
                              (db:add-slack-identity (lambda (user-id initial)
                                                       (declare (ignore user-id initial)) t)))
        (sb::handle-slash-command
         (signed-env "/slack/commands" "command=%2Fsubscribe&text=BW&user_id=U7&channel_id=C1"))
        (loop repeat 100 until posted do (sleep 0.05))
        (ok (= 1 (length posted)))
        (ok (string= "U7" (car (first posted))))
        (ok (search "Subscribed" (cdr (first posted))))))))

(deftest cmd-book-replies
  (ok (search "Usage" (sb::cmd-book (make-params "text" ""))))
  (ok (search "Usage" (sb::cmd-book (make-params "text" "   "))))
  (ok (search "Usage" (sb::cmd-book (make-params))))
  ;; an incomplete booking returns the re-send prompt, not an error banner
  (with-mocked-functions ((vb:book-natural
                           (lambda (text &key week registered-initials)
                             (declare (ignore week registered-initials))
                             (error 'bn:booking-incomplete :missing '(:day)
                                                           :understood '(:destination "pool")
                                                           :text text))))
    (let ((reply (sb::cmd-book (make-params "text" "for the pool for MM"))))
      (ok (search "Please re-send" reply))
      (ok (search "destination: pool" reply))
      (ok (search (bn:missing-piece-hint :day) reply))
      (ng (search "Booking failed" reply))))
  ;; the booking's printed output is the reply
  (with-mocked-functions ((vb:book-natural
                           (lambda (text &key week registered-initials)
                             (declare (ignore text week registered-initials))
                             (format t "Created DGH booking~%"))))
    (ok (search "Created DGH booking" (sb::cmd-book (make-params "text" "x")))))
  ;; silent success
  (with-mocked-functions ((vb:book-natural
                           (lambda (text &key week registered-initials)
                             (declare (ignore text week registered-initials))
                             nil)))
    (ok (search "no output" (sb::cmd-book (make-params "text" "x")))))
  ;; other errors are reported with their condition type
  (with-mocked-functions ((vb:book-natural
                           (lambda (text &key week registered-initials)
                             (declare (ignore text week registered-initials))
                             (error "boom"))))
    (let ((reply (sb::cmd-book (make-params "text" "x"))))
      (ok (search "Booking failed [SIMPLE-ERROR]" reply))
      (ok (search "boom" reply))
      (ok (search "/help" reply))))
  ;; the initials registered with /iam or /subscribe reach BOOK-NATURAL, once
  ;; each, so a request may write them in lowercase
  (let ((seen :none))
    (with-mocked-functions ((db:all-slack-identities
                             (lambda () '(("U1" . "DMM") ("U2" . "BW"))))
                            (db:all-slack-subscriptions
                             (lambda () '(("U2" . "BW") ("U3" . "CG"))))
                            (vb:book-natural
                             (lambda (text &key week registered-initials)
                               (declare (ignore text week))
                               (setf seen registered-initials)
                               (format t "booked"))))
      (sb::cmd-book (make-params "text" "dmm and cg to the pool"))
      (ok (= 3 (length seen)))
      (ng (set-exclusive-or '("DMM" "BW" "CG") seen :test #'string=)))))

(deftest cmd-view-replies
  (with-mocked-functions ((vb:view-schedule
                           (lambda (&optional range) (format t "TABLE for ~a" range) "ignored")))
    (ok (search "TABLE for today" (sb::cmd-view (make-params "text" ""))))
    (ok (search "TABLE for today" (sb::cmd-view (make-params))))
    (ok (search "TABLE for Apr 20 to Apr 26"
                (sb::cmd-view (make-params "text" " Apr 20 to Apr 26 ")))))
  ;; a multi-line table is posted in code fences (a list of message parts)
  ;; so its columns line up in Slack
  (with-mocked-functions ((vb:view-schedule
                           (lambda (&optional range) (declare (ignore range))
                             (format t "Date  Vehicle~%----  -------~%Mon   DGH~%")
                             "ignored")))
    (let ((reply (sb::cmd-view (make-params "text" "today"))))
      (ok (listp reply))
      (ok (= 1 (length reply)))
      (ok (uiop:string-prefix-p (format nil "```~%Date  Vehicle~%") (first reply)))
      (ok (search "Mon   DGH" (first reply)))
      (ok (uiop:string-suffix-p (first reply) "```"))))
  (with-mocked-functions ((vb:view-schedule
                           (lambda (&optional range) (declare (ignore range)) (error "no sheet"))))
    (let ((reply (sb::cmd-view (make-params "text" "tomorrow"))))
      (ok (search "View failed" reply))
      (ok (search "no sheet" reply))))
  ;; a range it cannot read -- a question, say -- gets examples, and the
  ;; schedule is not read
  (with-mocked-functions ((vb:view-schedule
                           (lambda (&optional range)
                             (fail (format nil "VIEW-SCHEDULE ran for ~s" range)))))
    (dolist (text '("xyzzy" "when is my ride home tuesday"
                    "what's on the van this week?"))
      (let ((reply (sb::cmd-view (make-params "text" text))))
        (ok (search (format nil "I can't read \"~a\"" text) reply))
        (ok (search "/view tomorrow" reply))))))

(deftest code-block-chunks
  ;; short text: one fenced part
  (let ((parts (sb::code-block-chunks (format nil "a~%b~%"))))
    (ok (= 1 (length parts)))
    (ok (string= (format nil "```~%a~%b~%```") (first parts))))
  ;; long text is split on line boundaries under the limit, each part
  ;; repeating the header lines, and no row is lost or duplicated
  (let* ((header (format nil "Date  Vehicle~%----  -------"))
         (rows (loop for i from 1 to 40
                     collect (format nil "row ~2d ~a" i (make-string 60 :initial-element #\x))))
         (text (format nil "~a~%~{~a~%~}" header rows))
         (parts (sb::code-block-chunks text :limit 1000 :header-lines 2)))
    (ok (> (length parts) 1))
    (dolist (p parts)
      (ok (<= (length p) (+ 1000 8)))
      (ok (uiop:string-prefix-p (format nil "```~%Date  Vehicle~%----  -------~%") p))
      (ok (uiop:string-suffix-p p "```")))
    (let ((all (apply #'concatenate 'string parts)))
      (dolist (r rows)
        (ok (= 1 (loop for start = (search r all) then (search r all :start2 (1+ start))
                       while start count t))))))
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
    (ok (> (length parts) 1))
    (dolist (p parts)
      (let ((lines (uiop:split-string p :separator '(#\Newline))))
        (ok (string= "```" (first lines)))
        (ok (string= "  Vehicle  Destination" (second lines)))
        (ok (uiop:string-prefix-p "Day " (fourth lines)))))
    (let ((all (apply #'concatenate 'string parts)))
      (loop for d from 1 to 8
            for needle = (format nil "Day ~d~%" d)
            do (ok (= 1 (loop for start = (search needle all)
                              then (search needle all :start2 (1+ start))
                              while start count t)))))))

(deftest message-events
  (let ((posted '())
        (booked '()))
    (with-mocked-functions ((sb::post-slack-message
                             (lambda (channel text) (push (cons channel text) posted)))
                            (vb:book-natural
                             (lambda (text &key week registered-initials)
                               (declare (ignore week registered-initials))
                               (push text booked)
                               (format t "booked ~a" text))))
      ;; ignored events post nothing and return NIL
      (ng (sb::handle-message-event (message-event "text" "x" "channel" "C1" "bot_id" "B1")))
      (ng (sb::handle-message-event (message-event "text" "x" "channel" "C1" "subtype" "edited")))
      (ng (sb::handle-message-event (message-event "text" "   " "channel" "C1")))
      (ng (sb::handle-message-event (message-event "channel" "C1")))
      (ng (sb::handle-message-event (message-event "text" "x")))
      (ok (= 0 (length posted)))
      ;; Anything that does not start with a command -- a booking in plain
      ;; words, a question, small talk, "help" -- gets the pointer to the
      ;; commands, synchronously, and nothing is read or booked.
      (dolist (text '("<@U1> book BW to the pool tomorrow 7-9pm"
                      "bw and mm to the pool thurs 7-9pm"
                      "when is my ride home this coming tuesday"
                      "what's on the van this week?"
                      "thanks!" "help" "?"))
        (setf posted '())
        (ng (sb::handle-message-event (message-event "text" text "channel" "C2")))
        (ok (= 1 (length posted)) text)
        (ok (string= "C2" (car (first posted))))
        (ok (string= sb::+commands-only-reply+ (cdr (first posted)))))
      (ok (equal '() booked))
      ;; the pointer names every command
      (dolist (entry sb::*slash-commands*)
        (ok (search (car entry) sb::+commands-only-reply+)
            (format nil "the pointer mentions ~a" (car entry))))
      ;; /help as message text posts the help page
      (setf posted '())
      (bt:join-thread (sb::handle-message-event (message-event "text" "/help" "channel" "C1")))
      (ok (string= sb::+help-text+ (cdr (first posted)))))))

(deftest parse-slash-text
  (multiple-value-bind (cmd args) (sb::parse-slash-text "/view")
    (ok (string= "/view" cmd))
    (ok (string= "" args)))
  (multiple-value-bind (cmd args) (sb::parse-slash-text "/view  this week and next week ")
    (ok (string= "/view" cmd))
    (ok (string= "this week and next week" args)))
  ;; an argument line may itself span lines
  (multiple-value-bind (cmd args) (sb::parse-slash-text (format nil "/book a van~%for MM"))
    (ok (string= "/book" cmd))
    (ok (string= (format nil "a van~%for MM") args)))
  ;; plain requests, and a slash that is not a command word, are not commands
  (ng (sb::parse-slash-text "book a van for MM"))
  (ng (sb::parse-slash-text "9/4 trip to town"))
  (ng (sb::parse-slash-text "/ view")))

(deftest message-event-slash-commands
  ;; A client that does not expand slash commands sends "/view" as message
  ;; text; it must show the schedule, not book a trip to "view".
  (let ((posted '())
        (booked '()))
    (with-mocked-functions ((sb::post-slack-message
                             (lambda (channel text) (push (cons channel text) posted)))
                            (vb:book-natural
                             (lambda (text &key week registered-initials)
                               (declare (ignore week registered-initials))
                               (push text booked)
                               (format t "booked ~a" text)))
                            (vb:view-schedule
                             (lambda (&optional range)
                               (format t "TABLE for ~a" range) "ignored")))
      (bt:join-thread (sb::handle-message-event
                       (message-event "text" "/view" "channel" "C1" "user" "U1")))
      (ok (= 0 (length booked)))
      (ok (string= "C1" (car (first posted))))
      (ok (search "TABLE for today" (cdr (first posted))))
      ;; the rest of the line is the date range, mention prefix and all
      (bt:join-thread (sb::handle-message-event
                       (message-event "text" "<@U9> /view tomorrow" "channel" "C1" "user" "U1")))
      (ok (search "TABLE for tomorrow" (cdr (first posted))))
      (ok (= 0 (length booked)))
      ;; /book still books, with the command word stripped
      (bt:join-thread (sb::handle-message-event
                       (message-event "text" "/book for the pool" "channel" "C1" "user" "U1")))
      (ok (equal '("for the pool") booked))
      ;; a mistyped command is named rather than booked
      (ng (sb::handle-message-event
           (message-event "text" "/vew tomorrow" "channel" "C1" "user" "U1")))
      (ok (equal '("for the pool") booked))
      (ok (search "Unknown command: /vew" (cdr (first posted)))))))

(deftest message-event-subscribe-uses-event-user
  ;; /subscribe as message text identifies the user from the event, since
  ;; there is no slash-command payload to carry user_id.
  (let ((posted '())
        (subscribed '()))
    (with-mocked-functions ((sb::post-slack-message
                             (lambda (channel text) (push (cons channel text) posted)))
                            (db:add-slack-subscription
                             (lambda (user-id initial)
                               (push (cons user-id initial) subscribed) t))
                            ;; /subscribe registers the identity too; unmocked,
                            ;; that wrote U7 -> BW into the real database on
                            ;; every build that ran this suite.
                            (db:add-slack-identity
                             (lambda (user-id initial)
                               (declare (ignore user-id initial)) t)))
      (bt:join-thread (sb::handle-message-event
                       (message-event "text" "/subscribe BW" "channel" "D1" "user" "U7")))
      (ok (equal '(("U7" . "BW")) subscribed))
      ;; the reply goes back to the conversation the message came from
      (ok (string= "D1" (car (first posted))))
      (ok (search "Subscribed" (cdr (first posted))))
      ;; ... and says how far ahead the reminders come
      (ok (search "30 minutes before each departure or return that BW drives"
                  (cdr (first posted)))))))

(deftest scheduler-tick-window
  ;; A leg is due from *REMINDER-LEAD-MINUTES* plus the minute between sweeps
  ;; until it leaves: with the lead at 30 minutes, 0 to 1860 seconds ahead.
  (let* ((trip (vb:make-trip :day "Monday" :vehicle :dgh :destination "SHMS"
                             :departure "8:00 am" :block-start-row 3
                             :dep-passengers '("BW")))
         (dep-offset 100)
         (posted '())
         (sb::*reminder-lead-minutes* 30)
         (sb::*sent-reminders* (make-hash-table :test #'equal))
         (sb::*sent-reminders-date* (sb::today-date-key)))
    (with-mocked-functions ((db:all-slack-subscriptions
                             (lambda () '(("U1" . "BW"))))
                            (vb:trips-for-driver-today
                             (lambda (initial) (declare (ignore initial)) (list trip)))
                            (vb:trips-for-return-driver-today
                             (lambda (initial) (declare (ignore initial)) nil))
                            (vb:trip-departure-universal-time
                             (lambda (tr) (declare (ignore tr)) (+ (get-universal-time) dep-offset)))
                            (vb:remind-driver
                             (lambda (initial &optional day)
                               (declare (ignore day))
                               (format nil "reminder for ~a" initial)))
                            (sb::post-slack-message
                             (lambda (channel text) (push (cons channel text) posted))))
      ;; inside the window -> one DM to the subscriber
      (sb::scheduler-tick)
      (ok (= 1 (length posted)))
      (ok (string= "U1" (car (first posted))))
      (ok (string= "reminder for BW" (cdr (first posted))))
      ;; the same trip on the next tick is deduplicated
      (sb::scheduler-tick)
      (ok (= 1 (length posted)))
      ;; outside the window (too far ahead, or already departed) -> nothing
      (clrhash sb::*sent-reminders*)
      (setf dep-offset 1900)
      (sb::scheduler-tick)
      (ok (= 1 (length posted)))
      (setf dep-offset -10)
      (sb::scheduler-tick)
      (ok (= 1 (length posted)))
      ;; the window's far edge is inclusive
      (setf dep-offset 1855)
      (sb::scheduler-tick)
      (ok (= 2 (length posted)))
      ;; the lead is half an hour, not the five minutes it used to be: a
      ;; departure 20 minutes off is already due
      (clrhash sb::*sent-reminders*)
      (setf dep-offset 1200)
      (sb::scheduler-tick)
      (ok (= 3 (length posted)))
      ;; ... and it is whatever *REMINDER-LEAD-MINUTES* says
      (clrhash sb::*sent-reminders*)
      (let ((sb::*reminder-lead-minutes* 10))
        (sb::scheduler-tick)
        (ok (= 3 (length posted)))
        (setf dep-offset 650)
        (sb::scheduler-tick)
        (ok (= 4 (length posted))))
      ;; a trip without a parseable departure is skipped
      (clrhash sb::*sent-reminders*)
      (with-mocked-functions ((vb:trip-departure-universal-time (lambda (tr) (declare (ignore tr)) nil)))
        (sb::scheduler-tick)
        (ok (= 4 (length posted)))))
    ;; errors for one subscription are logged, not raised
    (with-mocked-functions ((db:all-slack-subscriptions (lambda () '(("U1" . "BW"))))
                            (vb:trips-for-driver-today (lambda (initial) (declare (ignore initial))
                                                         (error "sheet unavailable"))))
      (let ((*error-output* (make-string-output-stream)))
        (ok (progn (sb::scheduler-tick) t))
        (ok (search "sheet unavailable" (get-output-stream-string *error-output*)))))))

(deftest scheduler-tick-return-leg
  ;; The driver of the return is reminded before the return time, apart from
  ;; the departure: the two legs of one trip are deduplicated separately.
  (let* ((trip (vb:make-trip :day "Thursday" :vehicle :dgh :destination "SHMS"
                             :departure "8:35 am" :return-time "12 pm"
                             :block-start-row 138
                             :dep-passengers '("BW" "DM")
                             :ret-passengers '("BW" "DM" "CG")))
         (dep-offset 100)
         (ret-offset 200)
         (posted '())
         (sb::*sent-reminders* (make-hash-table :test #'equal))
         (sb::*sent-reminders-date* (sb::today-date-key)))
    (with-mocked-functions ((db:all-slack-subscriptions
                             (lambda () '(("U1" . "BW"))))
                            (vb:trips-for-driver-today
                             (lambda (initial) (declare (ignore initial)) (list trip)))
                            (vb:trips-for-return-driver-today
                             (lambda (initial) (declare (ignore initial)) (list trip)))
                            (vb:trip-departure-universal-time
                             (lambda (tr) (declare (ignore tr)) (+ (get-universal-time) dep-offset)))
                            (vb:trip-return-universal-time
                             (lambda (tr) (declare (ignore tr)) (+ (get-universal-time) ret-offset)))
                            (vb:remind-driver
                             (lambda (initial &optional day)
                               (declare (ignore day))
                               (format nil "departure for ~a" initial)))
                            (sb::post-slack-message
                             (lambda (channel text) (push (cons channel text) posted))))
      (let ((*error-output* (make-broadcast-stream)))
        ;; both legs in the window -> two DMs, the return one worded by
        ;; REMIND-RETURN-DRIVER and naming the vehicle to drive
        (sb::scheduler-tick)
        (ok (= 2 (length posted)))
        (ok (find "departure for BW" posted :key #'cdr :test #'string=))
        (ok (find "BW is the driver of the DGH for the return from SHMS at 12 pm and DM, CG are passengers."
                  posted :key #'cdr :test #'string=))
        ;; neither leg repeats on the next tick
        (sb::scheduler-tick)
        (ok (= 2 (length posted)))
        ;; a return still far off waits, while a new day's departure goes out
        (clrhash sb::*sent-reminders*)
        (setf ret-offset 3600)
        (sb::scheduler-tick)
        (ok (= 3 (length posted)))
        (ok (string= "departure for BW" (cdr (first posted))))))))

(deftest reminder-key-shape
  (let ((a (vb:make-trip :vehicle :dgh :block-start-row 3))
        (b (vb:make-trip :vehicle :lgh :block-start-row 3))
        (c (vb:make-trip :vehicle :dgh :block-start-row 18)))
    (ok (equal (sb::reminder-key "U1" a) (sb::reminder-key "U1" a)))
    (ng (equal (sb::reminder-key "U1" a) (sb::reminder-key "U2" a)))
    (ng (equal (sb::reminder-key "U1" a) (sb::reminder-key "U1" b)))
    (ng (equal (sb::reminder-key "U1" a) (sb::reminder-key "U1" c)))
    (ok (= (sb::today-date-key) (fourth (sb::reminder-key "U1" a))))))

;;; --- Sender identity: who "me" is ---------------------------------------
;;;
;;; A booking names its passengers by initials, and the bot has more than one
;;; user, so "book me a van" has to be resolved to the sender before either
;;; parser sees it.  Initials come from a /iam registration when there is one
;;; and from the Slack profile otherwise; a guess is announced in the reply
;;; and never persisted.

(deftest derive-initials-from-a-name
  ;; The cases this was built for: three-letter initials that include a title.
  (ok (string= "DMM" (sb::derive-initials "Deacon Matt Marin")))
  (ok (string= "FBM" (sb::derive-initials "Fr Bob Marin")))
  (ok (string= "CG"  (sb::derive-initials "Cassian Gray")))
  ;; Punctuation between the parts is ignored, so "Ben H. W." still yields
  ;; three letters -- wrong for someone who signs BW, which is exactly why a
  ;; derived guess is announced rather than stored.
  (ok (string= "BHW" (sb::derive-initials "Ben H. W.")))
  ;; A display name that is already initials is taken as it stands.
  (ok (string= "DMM" (sb::derive-initials "dmm")))
  (ok (string= "DMM" (sb::derive-initials "DMM")))
  (ok (string= "CG"  (sb::derive-initials "  CG  ")))
  ;; Pronouns and status markers are not part of a name.
  (ok (string= "MM"  (sb::derive-initials "Matt Marin (he/him)")))
  ;; Anything that would not produce 2-3 letters is refused rather than
  ;; guessed at; the caller then asks for /iam.
  (ng (sb::derive-initials "Madonna"))
  (ng (sb::derive-initials "Juan Carlos de la Cruz Smith"))
  (ng (sb::derive-initials ""))
  (ng (sb::derive-initials nil))
  ;; Accented letters are letters.  Splitting on [A-Za-z] instead cut
  ;; "Garcia"-with-an-accent in two and turned a two-word name into a
  ;; well-formed but wrong three-letter guess, which is the one failure
  ;; nothing downstream could have caught.  This workspace really does hold a
  ;; "Marin" spelled with an accent.
  (ok (string= "JG"  (sb::derive-initials "Jose Garcia")))
  (ok (string= "JG"  (sb::derive-initials (format nil "Jos~c Garc~ca" #\LATIN_SMALL_LETTER_E_WITH_ACUTE
                                                  #\LATIN_SMALL_LETTER_I_WITH_ACUTE))))
  (ok (string= "MMC" (sb::derive-initials (format nil "Matt Mar~cn, CC"
                                                  #\LATIN_SMALL_LETTER_I_WITH_ACUTE))))
  ;; ...but an initial the sheet could not hold is no guess at all: both
  ;; readers of an initial require plain A-Z, so a name yielding one outside
  ;; that shape asks for /iam rather than producing a passenger nothing will
  ;; recognise.
  (ng (sb::derive-initials (format nil "Fr. ~ctienne Dubois"
                                   #\LATIN_CAPITAL_LETTER_E_WITH_ACUTE))))

(deftest parse-initial-argument-is-strict
  ;; A lone word is accepted in any case, so a phone that eats capitals still
  ;; registers someone.
  (ok (string= "DMM" (sb::parse-initial-argument "dmm")))
  (ok (string= "DMM" (sb::parse-initial-argument "  DMM ")))
  (ok (string= "CG"  (sb::parse-initial-argument "cg")))
  ;; A lone word of the wrong length is refused outright rather than trimmed
  ;; to its first three letters, which would have registered DMM for DMMM.
  (ng (sb::parse-initial-argument "DMMM"))
  (ng (sb::parse-initial-argument "toolongname"))
  (ng (sb::parse-initial-argument "x"))
  (ng (sb::parse-initial-argument ""))
  (ng (sb::parse-initial-argument nil))
  ;; A sentence still falls back to scanning for an uppercase run.
  (ok (string= "BW" (sb::parse-initial-argument "my initials are BW"))))

(deftest sender-initials-prefers-registration
  (with-temp-database
      (with-mocked-functions ((sb::slack-user-names
                               (lambda (uid)
                                 (declare (ignore uid))
                                 (values nil "Deacon Matt Marin"))))
        ;; With nothing registered, the Slack profile is the source, and the
        ;; name it came from is returned so the reply can show its working.
        (multiple-value-bind (initial source name) (sb::sender-initials +test-user-id+)
          (ok (string= "DMM" initial))
          (ok (eq :derived source))
          (ok (string= "Deacon Matt Marin" name)))
        ;; A registration wins over the guess, and reports no name.
        (db:add-slack-identity +test-user-id+ "FBM")
        (multiple-value-bind (initial source name) (sb::sender-initials +test-user-id+)
          (ok (string= "FBM" initial))
          (ok (eq :registered source))
          (ng name))))
  ;; No user at all (a payload without user_id) resolves to nobody.
  (ng (sb::sender-initials nil)))

(deftest sender-initials-when-nothing-works
  (with-temp-database
      (with-mocked-functions ((sb::slack-user-names
                               (lambda (uid) (declare (ignore uid)) (values nil "Madonna"))))
        (ng (sb::sender-initials +test-user-id+)))
    ;; A users.info that fails outright (no scope, network down) is not an
    ;; error, just an unknown sender.
    (with-mocked-functions ((sb::slack-user-names
                             (lambda (uid) (declare (ignore uid)) (values nil nil))))
      (ng (sb::sender-initials +test-user-id+)))))

(deftest resolve-sender-rewrites-and-announces
  (with-temp-database
      (with-mocked-functions ((sb::slack-user-names
                               (lambda (uid)
                                 (declare (ignore uid))
                                 (values nil "Deacon Matt Marin"))))
        ;; Registered: rewritten silently, no notice.
        (db:add-slack-identity +test-user-id+ "DMM")
        (multiple-value-bind (text notice)
            (sb::resolve-sender "book me a van to the pool" +test-user-id+)
          (ok (string= "book DMM a van to the pool" text))
          (ng notice))
        ;; Derived: rewritten, but the reply must say where the name came from.
        (db:remove-slack-identity +test-user-id+)
        (multiple-value-bind (text notice)
            (sb::resolve-sender "book me a van to the pool" +test-user-id+)
          (ok (string= "book DMM a van to the pool" text))
          (ok notice)
          (ok (search "DMM" notice))
          (ok (search "Deacon Matt Marin" notice))
          (ok (search "/iam" notice)))
        ;; A message that names nobody is returned untouched, and must not
        ;; cost a lookup: SLACK-USER-NAMES is mocked to fail the test if hit.
        (with-mocked-functions ((sb::slack-user-names
                                 (lambda (uid)
                                   (declare (ignore uid))
                                   (fail "users.info called for a message with no \"me\"")
                                   (values nil nil))))
          (multiple-value-bind (text notice)
              (sb::resolve-sender "book a van for CG and DMM" +test-user-id+)
            (ok (string= "book a van for CG and DMM" text))
            (ng notice))))))

(deftest resolve-sender-gives-up-on-an-unknown-sender
  (with-temp-database
      (with-mocked-functions ((sb::slack-user-names
                               (lambda (uid) (declare (ignore uid)) (values nil "Madonna"))))
        ;; NIL, not the original text: the caller must refuse rather than let
        ;; a message with an unresolved "me" reach the sheet.
        (ng (sb::resolve-sender "book me a van to the pool" +test-user-id+)))))

(deftest cmd-iam-registers-and-reports
  (with-temp-database
      (with-mocked-functions ((sb::slack-user-names
                               (lambda (uid)
                                 (declare (ignore uid))
                                 (values nil "Deacon Matt Marin"))))
        ;; Bare /iam before registering offers the guess to confirm.
        (let ((reply (sb::cmd-iam (make-params "user_id" +test-user-id+ "text" ""))))
          (ok (search "not registered" reply))
          (ok (search "DMM" reply)))
        ;; Registering, in lower case.
        (let ((reply (sb::cmd-iam (make-params "user_id" +test-user-id+ "text" "fbm"))))
          (ok (search "FBM" reply)))
        (ok (string= "FBM" (db:initial-of (db:find-slack-identity +test-user-id+))))
        ;; Bare /iam afterwards reports the registration.
        (let ((reply (sb::cmd-iam (make-params "user_id" +test-user-id+ "text" ""))))
          (ok (search "registered as" reply))
          (ok (search "FBM" reply)))
        ;; Re-registering replaces.
        (sb::cmd-iam (make-params "user_id" +test-user-id+ "text" "DMM"))
        (ok (string= "DMM" (db:initial-of (db:find-slack-identity +test-user-id+))))
        ;; Junk is refused with usage rather than truncated.
        (let ((reply (sb::cmd-iam (make-params "user_id" +test-user-id+ "text" "DMMM"))))
          (ok (search "Usage" reply)))
        (ok (string= "DMM" (db:initial-of (db:find-slack-identity +test-user-id+))))))
  (ok (search "Slack user" (sb::cmd-iam (make-params "text" "DMM")))))

(deftest subscribing-registers-an-identity-that-outlives-it
  (with-temp-database
      ;; /subscribe says who you are as well as asking for reminders, so
      ;; nobody has to register twice.
      (sb::cmd-subscribe (make-params "user_id" +test-user-id+ "text" "CG"))
    (ok (string= "CG" (db:initial-of (db:find-slack-identity +test-user-id+))))
    ;; ...and /unsubscribe gives up the reminders only.  Losing the identity
    ;; here would silently break "book me a van" for anyone who stops their
    ;; DMs, which is why the two are separate vertices.
    (sb::cmd-unsubscribe (make-params "user_id" +test-user-id+))
    (ng (db:find-slack-subscription +test-user-id+))
    (ok (string= "CG" (db:initial-of (db:find-slack-identity +test-user-id+))))))

(deftest run-request-resolves-me-end-to-end
  (with-temp-database
      (with-mocked-functions ((sb::slack-user-names
                               (lambda (uid)
                                 (declare (ignore uid))
                                 (values nil "Deacon Matt Marin")))
                              (sb::book-natural-reply (lambda (text) text)))
        ;; Registered: the booking runs on the rewritten sentence, and the
        ;; reply is the handler's alone.
        (db:add-slack-identity +test-user-id+ "DMM")
        (ok (string= "book DMM a van to the pool"
                     (sb::run-request "book me a van to the pool" :user-id +test-user-id+)))
        ;; Derived: same booking, with the notice posted above it.
        (db:remove-slack-identity +test-user-id+)
        (let ((reply (sb::run-request "book me a van to the pool"
                                      :user-id +test-user-id+)))
          (ok (listp reply))
          (ok (search "DMM" (first reply)))
          (ok (string= "book DMM a van to the pool" (second reply))))))
  ;; Unknown sender: the reply asks for /iam and the booking never runs.
  (with-temp-database
      (with-mocked-functions ((sb::slack-user-names
                               (lambda (uid) (declare (ignore uid)) (values nil "Madonna")))
                              (sb::book-natural-reply
                               (lambda (text)
                                 (declare (ignore text))
                                 (fail "a booking ran for an unidentified sender"))))
        (let ((reply (sb::run-request "book me a van to the pool"
                                      :user-id +test-user-id+)))
          (ok (search "/iam" reply))
          (ok (search "initials" reply))))))
