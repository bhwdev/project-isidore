;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/slack-bot
  (:use #:common-lisp)
  (:import-from #:alexandria
                #:plist-hash-table)
  (:import-from #:bordeaux-threads)
  (:import-from #:cl-ppcre)
  (:import-from #:com.inuoe.jzon)
  (:import-from #:dexador)
  (:import-from #:flexi-streams)
  (:import-from #:ironclad)
  (:import-from #:quri)
  (:import-from #:project-isidore/booking-nlp
                #:booking-incomplete)
  (:import-from #:project-isidore/van-booking
                #:book-natural
                #:view-schedule
                #:remind-driver
                #:trips-for-driver-today
                #:trip-departure-universal-time
                #:trip-vehicle
                #:trip-block-start-row)
  (:import-from #:project-isidore/database
                #:all-slack-subscriptions
                #:add-slack-subscription
                #:remove-slack-subscription)
  (:export #:make-slack-middleware
           #:load-config
           #:start-reminder-scheduler
           #:stop-reminder-scheduler)
  (:documentation
   "Slack bot integration for van booking.

Provides a Lack middleware that serves two endpoints:

  POST /slack/events   — Events API (DMs / @mentions) dispatched to
                         book-natural, original behaviour.  A message whose
                         text begins with a slash command runs that command
                         instead, for clients that deliver \"/view\" as plain
                         text rather than expanding it.
  POST /slack/commands — Slash commands /book, /view, /subscribe,
                         /unsubscribe, /help.

Booking requests are free-form English (see project-isidore/booking-nlp).
When a required piece is missing, the BOOKING-INCOMPLETE report is posted
back: it lists what was understood and asks the user to re-send the request
with the missing piece.

A persistent /subscribe registry (graph-db-backed via project-isidore/database)
plus an in-process scheduler thread DMs subscribed drivers 5 minutes before
each of their trips.

Setup: set SLACK_BOT_TOKEN and SLACK_SIGNING_SECRET environment variables.
See the project plan for Slack app configuration details."))

(in-package #:project-isidore/slack-bot)

;;; Configuration

(defvar *slack-bot-token* nil
  "Slack Bot User OAuth Token (xoxb-...). Read from SLACK_BOT_TOKEN env var at load time.")

(defvar *slack-signing-secret* nil
  "Slack app signing secret for request verification. Read from SLACK_SIGNING_SECRET env var.")

(defun load-config ()
  "Load Slack credentials from environment variables.
Must be called at runtime (from the application entry point), not at
build time — a tree-shaken executable has no load-time at startup, so
values read inside EVAL-WHEN would be baked in from the build host."
  (setf *slack-bot-token* (uiop:getenv "SLACK_BOT_TOKEN"))
  (setf *slack-signing-secret* (uiop:getenv "SLACK_SIGNING_SECRET")))

(defvar *slash-commands* ()
  "Dispatch alist: slash command name -> handler symbol.
Populated by DEFSLASH-COMMAND. Each handler takes the decoded params
hash-table and returns a reply string, or a list of strings that are posted
as separate messages. The docstring documents the handler for developers;
the user-facing help is +HELP-TEXT+, which must mention every command here.")

(defmacro defslash-command (slash-name (params-var) docstring &body body)
  "Define a Slack slash-command handler and register it in *SLASH-COMMANDS*.
SLASH-NAME is the literal command including the leading slash, e.g. \"/book\".
The handler is named CMD-<NAME> in the current package."
  (let* ((bare (string-upcase (string-left-trim "/" slash-name)))
         (fn-sym (intern (concatenate 'string "CMD-" bare))))
    `(progn
       (defun ,fn-sym (,params-var)
         ,docstring
         ,@body)
       (eval-when (:load-toplevel :execute)
         (let ((cell (assoc ,slash-name *slash-commands* :test #'string=)))
           (if cell
               (setf (cdr cell) ',fn-sym)
               (setf *slash-commands*
                     (append *slash-commands*
                             (list (cons ,slash-name ',fn-sym)))))))
       ',fn-sym)))

(defmacro with-slack-error-reply ((channel &key (context "Operation") thread-name)
                                  &body body)
  "Run BODY in a new thread, catching errors and posting them back to CHANNEL.
On thread-spawn failure, log to *ERROR-OUTPUT* and post the error too.
If CHANNEL evaluates to NIL, error reports are suppressed (only logged)."
  (alexandria:with-gensyms (chan ctx c)
    `(let ((,chan ,channel)
           (,ctx ,context))
       (flet ((report-error (,c)
                (when ,chan
                  (post-slack-message
                   ,chan
                   (format nil "~A failed [~A]: ~A~2%Type `/help` for usage."
                           ,ctx (type-of ,c) ,c)))))
         (handler-case
             (bt:make-thread
              (lambda ()
                (handler-case (progn ,@body)
                  (error (,c) (report-error ,c))))
              :name (or ,thread-name (format nil "slack-~A" ,ctx)))
           (error (,c)
             (format *error-output*
                     "slack-bot: failed to spawn ~A thread for ~A: ~A~%"
                     ,ctx ,chan ,c)
             (report-error ,c)))))))

(defparameter +help-text+
  "*Van Steward Bot* — I book the house vans on the weekly schedule sheet and show you what is booked.

*To book a trip*, tell me about it in your own words: send me a direct message, @mention me in a channel, or type `/book` followed by the request. Say:
  • *destination* — `for swimming`, `to the dentist`, `the SHMS trip`
  • *day* — `tomorrow`, `next Tuesday`, `April 22`
  • *passengers* — initials in CAPITALS, driver first: `for CG and BW`, `with MM, DM & CG`
  • *times* out and back — `at 7 pm to 9 pm`, `leaving at 8:30 and back around 11`
  • *vehicle* (optional) — `take the DGH`, `in the LGH`, `use the Chevy`, `the personal car`. If you don't say, I take the first van that is free (DGH, then LGH, then the Chevy).
The order and the wording don't matter. For example:
  • `book a van for swimming next Tuesday for CG and BW at 7 pm to 9 pm`
  • `Can BW and MM take the DGH to the dentist tomorrow, leaving at 8:30 and back around 11?`

*To join a trip already on the sheet*, name the trip and who is coming; no times needed:
  • `add GG to the SHMS trip on Thursday`

If something is missing, I reply with what I understood and ask you to re-send the whole request with the missing piece added.

*To see the schedule*, type `/view` for today, or add a day or a range:
  `/view tomorrow`   `/view the rest of the week`   `/view the weekend`   `/view Apr 20 to Apr 26`

*Reminders*: type `/subscribe` and your initials, e.g. `/subscribe BW`, and I'll message you 5 minutes before each trip you are driving. `/unsubscribe` turns them off.

*This message*: `/help`, or send me `help` or `?`."
  "The help page: what the bot is and how to use every command, for people
who are not programmers.  Posted for /help and for `help`/`?` messages.
Slack mrkdwn: *bold*, `code`.  Keep it well under Slack's 4000-character
message limit.")

;;; Signature Verification

(defun verify-slack-signature (timestamp body-string signature)
  "Verify a Slack request signature.
Returns T if valid, NIL otherwise.
TIMESTAMP is the X-Slack-Request-Timestamp header value (string).
BODY-STRING is the raw request body.
SIGNATURE is the X-Slack-Signature header value (v0=hex...)."
  (when (or (null *slack-signing-secret*) (null signature) (null timestamp))
    (return-from verify-slack-signature nil))
  ;; Reject timestamps older than 5 minutes (replay protection)
  (let ((ts (parse-integer timestamp :junk-allowed t)))
    (when (or (null ts) (> (abs (- (get-universal-time)
                                   ;; Convert Unix epoch to universal time
                                   (+ ts 2208988800)))
                           300))
      (return-from verify-slack-signature nil)))
  (let* ((basestring (format nil "v0:~A:~A" timestamp body-string))
         (key-bytes (ironclad:ascii-string-to-byte-array *slack-signing-secret*))
         (hmac (ironclad:make-hmac key-bytes :sha256)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array basestring))
    (let* ((digest (ironclad:hmac-digest hmac))
           (computed (format nil "v0=~(~{~2,'0X~}~)" (coerce digest 'list))))
      (ironclad:constant-time-equal
       (ironclad:ascii-string-to-byte-array computed)
       (ironclad:ascii-string-to-byte-array signature)))))

;;; Slack API

(defun post-slack-message (channel text)
  "Post TEXT to Slack CHANNEL using the chat.postMessage API.
chat.postMessage returns HTTP 200 even when the call is rejected (e.g.
not_authed, channel_not_found) — the real status is in the JSON body's
\"ok\" field. Log rejections to *error-output* so bad credentials don't
fail silently."
  (handler-case
      (let ((resp (dex:post "https://slack.com/api/chat.postMessage"
                            :content (com.inuoe.jzon:stringify
                                      (plist-hash-table
                                       (list "channel" channel "text" text)
                                       :test #'equal))
                            :headers `(("Content-Type" . "application/json; charset=utf-8")
                                       ("Authorization" . ,(format nil "Bearer ~A" *slack-bot-token*))))))
        (handler-case
            (let* ((parsed (com.inuoe.jzon:parse resp))
                   (ok (gethash "ok" parsed)))
              (unless (eq ok t)
                (format *error-output*
                        "slack-bot: chat.postMessage to ~A rejected: ~A~%"
                        channel (gethash "error" parsed))))
          (error (c)
            (format *error-output*
                    "slack-bot: could not parse chat.postMessage response: ~A~%" c))))
    (error (c)
      (format *error-output* "slack-bot: failed to post message to ~A: ~A~%" channel c))))

;;; Event Handling

(defun strip-mention (text)
  "Strip leading Slack bot mention (<@U...>) from TEXT."
  (string-trim '(#\Space) (cl-ppcre:regex-replace "^<@[A-Z0-9]+>\\s*" text "")))

(defun run-booking (text)
  "Run BOOK-NATURAL on TEXT and return the reply to post.  The captured output
is the reply; a BOOKING-INCOMPLETE condition becomes its own report, which
asks the user to re-send the request with the missing pieces.  Other errors
propagate to the caller."
  (handler-case
      (let ((output (with-output-to-string (*standard-output*)
                      (book-natural text))))
        (if (plusp (length (string-trim '(#\Space #\Newline) output)))
            output
            "Booking processed (no output)."))
    (booking-incomplete (c)
      (princ-to-string c))))

(defun post-command-reply (reply-to reply)
  "Post a command handler's REPLY to REPLY-TO.
A handler returns one string, or a list of strings that are posted as
separate messages (a long table in code fences)."
  (when (and reply reply-to (plusp (length reply)))
    (dolist (part (if (listp reply) reply (list reply)))
      (when (plusp (length part))
        (post-slack-message reply-to part)))))

(defun parse-slash-text (text)
  "Split TEXT into a leading slash command word and its argument text.
Returns two values, the command (\"/view\") and the rest of the line, or NIL
when TEXT does not begin with one.  A slash command typed in Slack itself
arrives at /slack/commands, but the same line reaches the Events API as
plain message text whenever the client does not expand it — a bridged
room, or a workspace where the command is not installed — and \"/view\"
must not then be read as a booking for a destination named \"view\"."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "(?s)^(/[A-Za-z][A-Za-z0-9_-]*)\\s*(.*)$" text)
    (when match
      (values (aref groups 0)
              (string-trim '(#\Space #\Tab #\Newline) (aref groups 1))))))

(defun message-command-params (event command args)
  "Build the params hash-table a command handler expects from a message EVENT
that carried COMMAND as plain text, with ARGS as the argument line."
  (plist-hash-table (list "command" command
                          "text" args
                          "user_id" (gethash "user" event)
                          "channel_id" (gethash "channel" event))
                    :test #'equal))

(defun handle-message-event (event)
  "Process a Slack message event and post the result back.
Ignores bot messages to prevent loops. Recognizes `help`/`/help`/`?` as a
request for usage docs. Text that begins with a slash command (`/view`,
`/book ...`) runs that command's handler rather than the booking parser —
Slack expands its own slash commands into a /slack/commands POST, but a
client that does not (a bridged room, an uninstalled command) delivers the
line here as ordinary text. Anything else is a booking request: a
BOOKING-INCOMPLETE condition is posted as-is (it asks the user to re-send
the request with the missing pieces); any other failure is reported with
the condition class as an error code and a pointer to `/help`. Returns the
worker thread, or NIL when the event was answered synchronously or ignored."
  (let ((text (gethash "text" event))
        (channel (gethash "channel" event))
        (bot-id (gethash "bot_id" event))
        (subtype (gethash "subtype" event)))
    (when (or bot-id subtype (null text) (null channel))
      (return-from handle-message-event nil))
    (let ((clean-text (strip-mention text)))
      (when (zerop (length (string-trim '(#\Space) clean-text)))
        (return-from handle-message-event nil))
      (when (member clean-text '("help" "/help" "?")
                    :test #'string-equal)
        (post-slack-message channel +help-text+)
        (return-from handle-message-event nil))
      (multiple-value-bind (command args) (parse-slash-text clean-text)
        (if command
            (let ((handler (cdr (assoc command *slash-commands* :test #'string-equal))))
              (unless handler
                (post-slack-message
                 channel
                 (format nil "Unknown command: ~A. Type `/help` to see what I can do." command))
                (return-from handle-message-event nil))
              (with-slack-error-reply (channel
                                       :context (format nil "Command ~A" command)
                                       :thread-name (format nil "slack-cmd-~A" command))
                (post-command-reply channel
                                    (funcall handler
                                             (message-command-params event command args)))))
            (with-slack-error-reply (channel :context "Booking"
                                             :thread-name "slack-booking")
              (post-slack-message channel (run-booking clean-text))))))))

;;; Lack Middleware

(defun read-body-string (env)
  "Read the raw request body from a Lack/Clack ENV plist into a UTF-8 string."
  (let ((stream (getf env :raw-body)))
    (when stream
      (let ((octets (alexandria:read-stream-content-into-byte-vector stream)))
        (flexi-streams:octets-to-string octets :external-format :utf-8)))))

(defun json-response (status hash-table)
  "Return a Lack response triplet with JSON content."
  (list status
        '(:content-type "application/json")
        (list (com.inuoe.jzon:stringify hash-table))))

(defun make-slack-middleware ()
  "Return a Lack middleware that intercepts POST /slack/events and
POST /slack/commands. All other requests pass through to the wrapped app."
  (lambda (app)
    (lambda (env)
      (cond
        ((not (eq :post (getf env :request-method)))
         (funcall app env))
        ((string= "/slack/events" (getf env :path-info))
         (handle-slack-request env))
        ((string= "/slack/commands" (getf env :path-info))
         (handle-slash-command env))
        (t (funcall app env))))))

(defun handle-slack-request (env)
  "Handle an incoming Slack Events API request.
Verifies the signature, dispatches on event type, returns an appropriate response."
  (let ((body-string (read-body-string env)))
    (unless body-string
      (return-from handle-slack-request '(400 () ("bad request"))))
    ;; Verify signature
    (let* ((headers (getf env :headers))
           (timestamp (and headers (gethash "x-slack-request-timestamp" headers)))
           (signature (and headers (gethash "x-slack-signature" headers))))
      (unless (verify-slack-signature timestamp body-string signature)
        (return-from handle-slack-request '(403 () ("invalid signature"))))
      ;; Skip retries (deduplication)
      (when (gethash "x-slack-retry-num" headers)
        (return-from handle-slack-request '(200 () ("")))))
    ;; Parse and dispatch
    (let* ((payload (com.inuoe.jzon:parse body-string))
           (type (gethash "type" payload)))
      (cond
        ;; Slack URL verification handshake
        ((string= type "url_verification")
         (json-response 200 (plist-hash-table
                             (list "challenge" (gethash "challenge" payload))
                             :test #'equal)))
        ;; Event callback — respond immediately, process async
        ((string= type "event_callback")
         (let ((event (gethash "event" payload)))
           (when event
             (handle-message-event event)))
         '(200 () ("")))
        ;; Unknown type
        (t '(200 () ("")))))))

;;; Slash Commands

(defun decode-form-body (body-string)
  "Decode an application/x-www-form-urlencoded BODY-STRING into a hash-table."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (pair (quri:url-decode-params body-string))
      (setf (gethash (car pair) ht) (cdr pair)))
    ht))

(defun parse-initial (text)
  "Extract the first 2-3 uppercase letter initial from TEXT, or NIL."
  (let ((m (cl-ppcre:scan-to-strings "[A-Z]{2,3}" (or text ""))))
    (and m m)))

(defun code-block-chunks (text &key (limit 3500) (header-lines 0))
  "Wrap TEXT in Slack code fences so a fixed-width table keeps its columns in
the proportional-font chat view.  Slack splits or truncates messages past
4000 characters, which would cut a code block in two, so TEXT longer than
LIMIT characters is split into several fenced strings, each repeating the
first HEADER-LINES lines (column titles and rule) so every part reads on its
own.  A split lands on the last blank line of the part when there is one (the
gap between two days of a schedule), otherwise between lines.  Returns a list
of strings."
  (let* ((lines (cl-ppcre:split "\\n" (string-right-trim '(#\Newline #\Space) text)))
         (header (subseq lines 0 (min header-lines (length lines))))
         (body (nthcdr (length header) lines))
         (chunks '())
         (current '())                  ; body lines of the open part, newest first
         (size 0))
    (flet ((fence (body-lines)
             (format nil "```~%~{~a~%~}```" (append header body-lines)))
           (line-size (line) (1+ (length line))))
      (setf size (reduce #'+ header :key #'line-size :initial-value 0))
      (dolist (line body)
        (when (and current (> (+ size (line-size line)) limit))
          (let* ((ordered (reverse current))
                 (blank (position "" ordered :test #'string= :from-end t))
                 (split-at-blank (and blank (plusp blank)))
                 (kept (if split-at-blank (subseq ordered 0 blank) ordered))
                 (carried (if split-at-blank (subseq ordered (1+ blank)) '())))
            (push (fence kept) chunks)
            (setf current (reverse carried)
                  size (reduce #'+ (append header carried)
                               :key #'line-size :initial-value 0))))
        ;; a part never opens with the blank line that closed the previous one
        (unless (and (null current) (string= line ""))
          (push line current)
          (incf size (line-size line))))
      (push (fence (reverse current)) chunks))
    (nreverse chunks)))

(defslash-command "/book" (params)
  "/book <plain-English request> — book a trip or join one already on the sheet.
Examples:
  /book a van for swimming next Tuesday for CG and BW at 7 pm to 9 pm
  /book Can BW and MM take the DGH to the dentist tomorrow, leaving at 8:30 and back around 11?
  /book add GG to the SHMS trip on Thursday
Needed, in any order: destination, day, passengers (uppercase initials).
Times (departure and return) are needed only when no matching trip exists.
Optional: mention a vehicle (DGH, LGH, Chevy, personal car).
If a piece is missing, the reply says what was understood and asks you to
re-send the request including it."
  (let ((text (gethash "text" params)))
    (when (or (null text) (zerop (length (string-trim '(#\Space) text))))
      (return-from cmd-book
        "Usage: type `/book` followed by the trip in your own words, e.g. `/book take the DGH to the dentist tomorrow for BW, leaving at 8:30 and back by 11`. Type `/help` for more."))
    (handler-case (run-booking text)
      (error (c)
        (format nil "Booking failed [~A]: ~A~%Type `/help` to see how to phrase a request."
                (type-of c) c)))))

(defslash-command "/view" (params)
  "/view [<date range>] — show booked trips as a table. With no argument, shows today.
Examples:
  /view
  /view tomorrow
  /view the whole week
  /view the rest of the week
  /view this week and next week
  /view the weekend
  /view Apr 20 to Apr 26"
  (let* ((text (gethash "text" params))
         (range (if (or (null text) (zerop (length (string-trim '(#\Space) text))))
                    "today"
                    (string-trim '(#\Space) text))))
    (handler-case
        (let ((out (with-output-to-string (*standard-output*)
                     (view-schedule range))))
          ;; A table is several lines: post it in code fences so the columns
          ;; line up in Slack's proportional font, in more than one message if
          ;; it is long.  One-line answers ("No trips scheduled ...") stay plain.
          (if (find #\Newline (string-trim '(#\Newline #\Space) out))
              (code-block-chunks out :header-lines 2)
              out))
      (error (c)
        (format nil "View failed [~A]: ~A" (type-of c) c)))))

(defslash-command "/subscribe" (params)
  "/subscribe <INITIAL> — receive a direct message 5 minutes before each trip
today where you are the driver (first passenger listed). Example:
  /subscribe BW
Persists across restarts. Use /unsubscribe to stop."
  (let* ((text (gethash "text" params))
         (user-id (gethash "user_id" params))
         (initial (parse-initial text)))
    (cond
      ((null user-id) "Could not identify Slack user.")
      ((null initial) "Usage: type `/subscribe` followed by your initials in CAPITALS, e.g. `/subscribe BW`.")
      (t
       (add-slack-subscription user-id (string-upcase initial))
       (format nil "Subscribed. You will be DM'd 5 minutes before each trip where ~A is the driver."
               (string-upcase initial))))))

(defslash-command "/unsubscribe" (params)
  "/unsubscribe — stop receiving trip reminder DMs."
  (let ((user-id (gethash "user_id" params)))
    (cond
      ((null user-id) "Could not identify Slack user.")
      ((remove-slack-subscription user-id)
       "Unsubscribed. No further reminders will be sent.")
      (t "You were not subscribed."))))

(defslash-command "/help" (params)
  "/help — post the help page (+HELP-TEXT+): what the bot does and how to use
every command, written for non-technical users.  Handler docstrings are
developer documentation and are not shown to users."
  (declare (ignore params))
  +help-text+)

(defun reply-channel-for (params)
  "Where should the async reply be posted?
Subscribe/unsubscribe go to the user as a DM; others go to the channel."
  (let ((cmd (gethash "command" params)))
    (if (member cmd '("/subscribe" "/unsubscribe") :test #'string=)
        (gethash "user_id" params)
        (gethash "channel_id" params))))

(defun handle-slash-command (env)
  "Verify and dispatch a Slack slash-command POST.
Responds 200 immediately with an empty body; the handler's output is
posted back asynchronously via chat.postMessage."
  (let ((body-string (read-body-string env)))
    (unless body-string
      (return-from handle-slash-command '(400 () ("bad request"))))
    (let* ((headers (getf env :headers))
           (timestamp (and headers (gethash "x-slack-request-timestamp" headers)))
           (signature (and headers (gethash "x-slack-signature" headers))))
      (unless (verify-slack-signature timestamp body-string signature)
        (return-from handle-slash-command '(403 () ("invalid signature")))))
    (let* ((params (decode-form-body body-string))
           (cmd (gethash "command" params))
           (handler (cdr (assoc cmd *slash-commands* :test #'string=)))
           (reply-to (reply-channel-for params)))
      (cond
        ((null handler)
         (list 200 '(:content-type "text/plain")
               (list (format nil "Unknown command: ~A. Type /help to see what I can do." cmd))))
        (t
         (with-slack-error-reply (reply-to
                                  :context (format nil "Command ~A" cmd)
                                  :thread-name (format nil "slack-cmd-~A" cmd))
           (post-command-reply reply-to (funcall handler params)))
         '(200 () ("")))))))

;;; Reminder Scheduler

(defvar *scheduler-thread* nil
  "The running reminder-scheduler thread, or NIL.")

(defvar *scheduler-lock* (bt:make-lock "slack-scheduler-lock"))

(defvar *sent-reminders* (make-hash-table :test #'equal)
  "Dedup set. Key: (list user-id sheet-row vehicle date-key). Value: T.")

(defvar *sent-reminders-date* nil
  "YYYYMMDD of the day *sent-reminders* was last reset.")

(defun today-date-key ()
  (multiple-value-bind (s m h day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    (+ (* year 10000) (* month 100) day)))

(defun maybe-reset-sent-reminders ()
  (let ((today (today-date-key)))
    (unless (eql today *sent-reminders-date*)
      (clrhash *sent-reminders*)
      (setf *sent-reminders-date* today))))

(defun reminder-key (user-id trip)
  (list user-id
        (trip-block-start-row trip)
        (trip-vehicle trip)
        (today-date-key)))

(defun scheduler-tick ()
  "Single sweep: for each subscription, DM 5m-before reminders for due trips.
Window is [now, now+360s] — with a 60s sleep that covers 5 minutes ±30s."
  (maybe-reset-sent-reminders)
  (let ((now (get-universal-time)))
    (dolist (sub (all-slack-subscriptions))
      (let ((user-id (car sub))
            (initial (cdr sub)))
        (handler-case
            (dolist (trip (trips-for-driver-today initial))
              (let ((dep-ut (trip-departure-universal-time trip)))
                (when (and dep-ut
                           (<= 0 (- dep-ut now) 360)
                           (not (gethash (reminder-key user-id trip)
                                         *sent-reminders*)))
                  (post-slack-message user-id (remind-driver initial))
                  (setf (gethash (reminder-key user-id trip) *sent-reminders*) t))))
          (error (c)
            (format *error-output*
                    "slack scheduler: error for ~A/~A: ~A~%"
                    user-id initial c)))))))

(defun reminder-scheduler-loop ()
  (loop
    (handler-case (scheduler-tick)
      (error (c)
        (format *error-output* "slack scheduler tick error: ~A~%" c)))
    (sleep 60)))

(defun start-reminder-scheduler ()
  "Start the reminder scheduler thread if not already running. Idempotent."
  (bt:with-lock-held (*scheduler-lock*)
    (when (and *scheduler-thread*
               (bt:thread-alive-p *scheduler-thread*))
      (return-from start-reminder-scheduler *scheduler-thread*))
    (setf *scheduler-thread*
          (bt:make-thread #'reminder-scheduler-loop
                          :name "slack-reminder-scheduler"))))

(defun stop-reminder-scheduler ()
  "Stop the reminder scheduler thread if running. Idempotent."
  (bordeaux-threads:with-lock-held (*scheduler-lock*)
    (when (and *scheduler-thread*
               (bordeaux-threads:thread-alive-p *scheduler-thread*))
      (bordeaux-threads:destroy-thread *scheduler-thread*))
    (setf *scheduler-thread* nil)))
