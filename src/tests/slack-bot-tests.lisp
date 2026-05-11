;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/slack-bot-tests
  (:use #:common-lisp #:parachute)
  (:local-nicknames (#:sb #:project-isidore/slack-bot)
                    (#:db #:project-isidore/database))
  (:documentation
   "Parachute tests for project-isidore/slack-bot.

Covers pure helpers (form decoding, initial parsing, dispatch table,
docstring-driven help output, reminder-key determinism, date-rollover
reset) and rucksack-backed subscription CRUD. Handlers that would hit
the Slack API or Google Sheets (cmd-book, cmd-view, full scheduler
tick) are stubbed via FLET shadowing."))

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
  ;; Every handler has a docstring (source of truth for /help).
  (dolist (entry sb::*slash-commands*)
    (true (documentation (cdr entry) 'function)
          "~a has a docstring" (car entry))))

(define-test cmd-help-includes-all-commands
  :parent slack-bot-suite
  (let ((out (sb::cmd-help (make-hash-table :test #'equal))))
    (dolist (name '("/book" "/view" "/subscribe" "/unsubscribe" "/help"))
      (true (search name out) "help output mentions ~a" name))))

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

;;; --- Subscription CRUD (rucksack round-trip) ---
;;;
;;; Each CRUD test runs against a fresh, temporary rucksack so nothing
;;; leaks into ~/.local/share/project-isidore/ (or wherever
;;; PROJECT_ISIDORE_DB points). WITH-TEMP-DATABASE handles the open,
;;; the *database*/*rucksack* rebind, the slack-subscription schema
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
  (let ((saved (uiop:getenvp "PROJECT_ISIDORE_DB")))
    (unwind-protect
         (progn
           (setf (uiop:getenv "PROJECT_ISIDORE_DB") "/tmp/pi-env-var-test/")
           (is equal (uiop:ensure-directory-pathname "/tmp/pi-env-var-test/")
               (db:database-path))
           ;; Empty / unset: XDG default
           (setf (uiop:getenv "PROJECT_ISIDORE_DB") "")
           (is equal (uiop:xdg-data-home "project-isidore/")
               (db:database-path)))
      (if saved
          (setf (uiop:getenv "PROJECT_ISIDORE_DB") saved)
          (setf (uiop:getenv "PROJECT_ISIDORE_DB") "")))))

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
         (saved (uiop:getenvp "PROJECT_ISIDORE_DB")))
    (unwind-protect
         (progn
           (false (uiop:directory-exists-p target))
           ;; Close the production graph before pointing the env var at a
           ;; temp dir; otherwise REOPEN-DATABASE swaps it out of GRAPH-DB::
           ;; *GRAPHS* without closing it, leaking fd's, mmaps, and the
           ;; replication/transaction-manager threads. Several such leaks
           ;; per test invocation accumulate fast enough to crash SBCL.
           (when db:*database*
             (let ((graph-db:*graph* db:*database*))
               (graph-db:close-graph db:*database* :snapshot-p nil)))
           (setf (uiop:getenv "PROJECT_ISIDORE_DB") (namestring target))
           (db:reopen-database)
           (true (uiop:directory-exists-p target))
           ;; graph-db's MAKE-GRAPH eagerly creates schema.dat and heap.dat
           ;; under the location; a transaction is not required.
           (let ((files (mapcar #'file-namestring
                                (uiop:directory-files target))))
             (true (member "heap.dat" files :test #'string=))
             (true (member "schema.dat" files :test #'string=))))
      (when db:*database*
        (let ((graph-db:*graph* db:*database*))
          (graph-db:close-graph db:*database* :snapshot-p nil)))
      (if saved
          (setf (uiop:getenv "PROJECT_ISIDORE_DB") saved)
          (setf (uiop:getenv "PROJECT_ISIDORE_DB") ""))
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
