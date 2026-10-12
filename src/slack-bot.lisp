;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/slack-bot
  (:use #:common-lisp)
  (:import-from #:alexandria
                #:plist-hash-table
                #:ensure-list)
  (:import-from #:bordeaux-threads)
  (:import-from #:cl-ppcre)
  (:import-from #:com.inuoe.jzon)
  (:import-from #:dexador)
  (:import-from #:flexi-streams)
  (:import-from #:ironclad)
  (:import-from #:quri)
  (:import-from #:project-isidore/booking-nlp
                #:booking-incomplete
                #:booking-incomplete-missing
                #:schedule-question-range
                #:self-reference-p
                #:resolve-self-reference)
  (:import-from #:project-isidore/booking-llm
                #:load-llm-config
                #:warm-up-llm
                #:llm-enabled-p
                #:llm-normalize
                #:render-intent)
  (:import-from #:project-isidore/van-booking
                #:book-natural
                #:view-schedule
                #:date-range-phrase-p
                #:fetch-all-trips
                #:known-destinations
                #:remind-driver
                #:trips-for-driver-today
                #:trip-departure-universal-time
                #:trips-for-return-driver-today
                #:trip-return-universal-time
                #:remind-return-driver
                #:trip-vehicle
                #:trip-block-start-row)
  (:import-from #:project-isidore/database
                #:all-slack-subscriptions
                #:add-slack-subscription
                #:remove-slack-subscription
                #:find-slack-identity
                #:add-slack-identity
                #:remove-slack-identity
                #:initial-of)
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
with the missing piece.  A question about a trip's timing that names no
passengers (\"when is my ride home this coming Tuesday?\") is not a booking
at all: it is answered as `/view` of that day, by the rule parser, before
anything else is tried.

When the rule parser cannot find the destination, day or passengers, and a
local model server is configured (VAN_LLM_URL, see
project-isidore/booking-llm), the message is first shown to the model, which
reads it into a `/book` sentence or a `/view` range.  The bot posts that
interpretation and then runs the ordinary handler on it, so the model never
does more than rephrase; without a server the report is posted as before.

A persistent /subscribe registry (graph-db-backed via project-isidore/database)
plus an in-process scheduler thread DMs subscribed drivers 5 minutes before
each leg they drive: the departure when they are its first passenger, the
return when they are the first listed for the return.

A second registry records which initials each Slack account goes by, so a
message can say \"me\", \"my\" or \"I\" instead of spelling them out: /iam sets
it, /subscribe sets it as a side effect, and /unsubscribe leaves it alone.
For someone who has registered neither, the initials are guessed from their
Slack profile name (users.info), and the reply says so every time, because a
guess is never persisted.

Setup: set SLACK_BOT_TOKEN and SLACK_SIGNING_SECRET environment variables,
and optionally VAN_LLM_URL.  The bot token needs the `users:read` scope for
the profile guess; without it users.info answers \"missing_scope\", which is
handled like any other failure -- everyone simply has to run /iam once.  See
the project plan for Slack app configuration details."))

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
  (setf *slack-signing-secret* (uiop:getenv "SLACK_SIGNING_SECRET"))
  (load-llm-config)
  (warm-up-llm))

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
  • *passengers* — initials in CAPITALS, driver first: `for CG and BW`, `with MM, DM & CG`. Say `me` for yourself once you have run `/iam` (see below).
  • *times* out and back — `at 7 pm to 9 pm`, `leaving at 8:30 and back around 11`
  • *vehicle* (optional) — `take the DGH`, `in the LGH`, `use the Chevy`, `the personal car`. If you don't say, I take the first van that is free (DGH, then LGH, then the Chevy).
The order and the wording don't matter. For example:
  • `book a van for swimming next Tuesday for CG and BW at 7 pm to 9 pm`
  • `Can BW and MM take the DGH to the dentist tomorrow, leaving at 8:30 and back around 11?`

I never book over a clash: if the van you asked for is already out at that time, or if somebody on your list is already on another trip that overlaps, I say which trip is in the way and book nothing.

*To join a trip already on the sheet*, name the trip and who is coming; no times needed:
  • `add GG to the SHMS trip on Thursday`

If something is missing, I reply with what I understood and ask you to re-send the whole request with the missing piece added. If I can't follow the wording at all, I first show how I read it as a `/book` or `/view` line, then act on that reading.

*To see the schedule*, type `/view` for today, or add a day or a range:
  `/view tomorrow`   `/view the rest of the week`   `/view the weekend`   `/view Apr 20 to Apr 26`   `/view Sept 19-22`
Or just ask about a trip's timing — `when is my ride home this coming Tuesday?`, `what time does the van leave tomorrow?` — and I show that day.

*Who you are*: type `/iam` and your initials, e.g. `/iam DMM`, and I'll read `me`, `my` and `I` as you from then on — `book me a van to the dentist tomorrow 9 to 11`, `can I get a van for the pool Saturday 8-10`, `me and CG to swimming Tuesday 7-9pm`. Type `/iam` on its own to see who I have you down as. I remember it across restarts and through `/unsubscribe`.

*Reminders*: type `/subscribe` and your initials, e.g. `/subscribe BW`, and I'll message you 30 minutes before each trip you are driving, out or back, with the vehicle you are taking (the first name listed drives). `/unsubscribe` turns them off.

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

(defun book-natural-reply (text)
  "Run BOOK-NATURAL on TEXT and return what it printed as the reply, or a
placeholder when it printed nothing.  Conditions propagate."
  (let ((output (with-output-to-string (*standard-output*)
                  (book-natural text))))
    (if (plusp (length (string-trim '(#\Space #\Newline) output)))
        output
        "Booking processed (no output).")))

(defun run-booking (text)
  "Run BOOK-NATURAL on TEXT and return the reply to post.  The captured output
is the reply; a BOOKING-INCOMPLETE condition becomes its own report, which
asks the user to re-send the request with the missing pieces.  Other errors
propagate to the caller.  This is the rule parser alone; RUN-REQUEST adds
the model fallback around it."
  (handler-case (book-natural-reply text)
    (booking-incomplete (c)
      (princ-to-string c))))

(defun first-pass-incomplete-p (condition)
  "True when the BOOKING-INCOMPLETE CONDITION is about the destination, day
or passengers -- what BOOK-NATURAL's first pass needs before it reads the
sheet -- rather than only about the times, which BOOK asks for when no
existing trip matches."
  (and (intersection '(:destination :day :passengers)
                     (booking-incomplete-missing condition))
       t))

(defun llm-known-destinations ()
  "The sheet's destinations, for the model's prompt and as the parser's
gazetteer, or NIL when the sheet cannot be read.  A function of its own so
tests can stub it."
  (ignore-errors (known-destinations (fetch-all-trips))))

(defun llm-interpretation (text &key (allow-view t))
  "Ask the model (PROJECT-ISIDORE/BOOKING-LLM) what TEXT means and turn its
answer into a command: (values \"/book\" sentence) or (values \"/view\"
range), or NIL when no model is configured, it did not answer, or its answer
did not survive validation.  ALLOW-VIEW NIL keeps an explicit `/book` from
turning into a view."
  (when (llm-enabled-p)
    (let* ((known (llm-known-destinations))
           (intent (llm-normalize text :known-destinations known)))
      (when intent
        (render-intent intent text :known-destinations known :allow-view allow-view)))))

(defun interpretation-line (command args)
  "The message posted before a request that was read as COMMAND ARGS -- by
SCHEDULE-QUESTION-INTERPRETATION or by the model -- is run, so the user sees
how it was understood and can correct it."
  (format nil "Interpreted as: `~a ~a` — if that is wrong, re-send with the details spelled out (`/help` shows how)."
          command args))

(defun run-interpretation (command args)
  "The reply for a request that was read as COMMAND ARGS rather than run
verbatim: the interpretation line, then what the ordinary handler says about
ARGS -- CMD-VIEW for `/view`, RUN-BOOKING for `/book`, so a rewritten booking
that is still incomplete gets the report and nothing further is tried."
  (cons (interpretation-line command args)
        (if (string= command "/view")
            (ensure-list (cmd-view (plist-hash-table (list "text" args) :test #'equal)))
            (list (run-booking args)))))

(defun schedule-question-interpretation (text)
  "(values \"/view\" range) when TEXT asks about a trip's timing rather than
for a trip -- \"when is my ride home this coming Tuesday?\", \"what time does
the van leave tomorrow?\" -- and names no passengers (see
BOOKING-NLP:SCHEDULE-QUESTION-RANGE); NIL otherwise, or when the schedule
cannot read the day it names, in which case the message is treated as a
booking like any other."
  (let ((range (schedule-question-range text)))
    (when (and range (date-range-phrase-p range))
      (values "/view" range))))

(defun slack-user-names (user-id)
  "Return the names Slack holds for USER-ID as (values display-name real-name),
or NIL NIL when the lookup fails.  Needs the `users:read` scope; without it
users.info answers ok=false with \"missing_scope\", which is treated like any
other failure -- the caller falls back to asking for `/iam`.  Every failure
is logged and swallowed, because not knowing a name must never take the bot
down mid-booking."
  (handler-case
      (let* ((resp (dex:get (format nil "https://slack.com/api/users.info?user=~A"
                                    (quri:url-encode user-id))
                            :headers `(("Authorization"
                                        . ,(format nil "Bearer ~A" *slack-bot-token*)))))
             (parsed (com.inuoe.jzon:parse resp)))
        (if (eq (gethash "ok" parsed) t)
            (let* ((user (gethash "user" parsed))
                   (profile (and user (gethash "profile" user))))
              (values (and profile (gethash "display_name" profile))
                      (or (and profile (gethash "real_name" profile))
                          (and user (gethash "real_name" user)))))
            (progn
              (format *error-output* "slack-bot: users.info for ~A rejected: ~A~%"
                      user-id (gethash "error" parsed))
              (values nil nil))))
    (error (c)
      (format *error-output* "slack-bot: users.info for ~A failed: ~A~%" user-id c)
      (values nil nil))))

(defun name-words (text)
  "The runs of letters in TEXT, in order.
Letters are ALPHA-CHAR-P's, not [A-Za-z]: this workspace holds a Marin
spelled \"Marín\", and an ASCII-only split cuts that into \"Mar\" and \"n\",
turning a two-word name into a well-formed three-letter guess.  A guess that
is wrong but plausible is the one outcome worth going out of the way to
avoid, because nothing downstream can tell it from a right one."
  (loop with word = '()
        with words = '()
        for ch across text
        do (if (alpha-char-p ch)
               (push ch word)
               (when word
                 (push (coerce (nreverse word) 'string) words)
                 (setf word '())))
        finally (when word
                  (push (coerce (nreverse word) 'string) words))
                (return (nreverse words))))

(defun derive-initials (name)
  "Guess the initials NAME goes by, or NIL when the guess would not be one.
A name that is already initials is taken as it stands (\"dmm\" -> \"DMM\"),
which covers the people who set their Slack display name to them; anything
else contributes the first letter of each of its words, so \"Dcn. Matt
Marin\" gives \"DMM\".  Titles are kept rather than stripped, because here
they are part of the initials people actually use on the sheet.  Only a 2-3
letter result is returned -- +INITIALS-PATTERN+'s shape -- so a one-word name
or a long one yields NIL and the caller asks for `/iam` instead of guessing.

The guess is right for most of this workspace and wrong for some of it: the
display name \"Fr. Bernard\" gives FB for someone who signs FBM.  So it is
never persisted on its own, and the reply names the profile it came from
every time, until the sender settles it with `/iam`."
  (when (stringp name)
    (let* ((trimmed (string-trim '(#\Space #\Tab) name))
           ;; "Matt Marin (he/him)" and "Bob [PTO]" carry parentheticals that
           ;; are not part of anyone's name.
           (bare (cl-ppcre:regex-replace-all "[(\\[{][^)\\]}]*[)\\]}]" trimmed " "))
           (words (name-words bare))
           (guess
             (cond
               ;; Already initials: one word of 2-3 letters and nothing else.
               ((and (= 1 (length words))
                     (<= 2 (length (first words)) 3))
                (string-upcase (first words)))
               ((<= 2 (length words) 3)
                (string-upcase (map 'string (lambda (w) (char w 0)) words)))
               (t nil))))
      ;; The sheet writes plain A-Z, and both readers of an initial check that
      ;; shape, so a guess outside it ("Fr. Étienne Dubois" -> "FÉD") would be
      ;; substituted into the message and then not recognised as a passenger.
      ;; Better to have no guess and ask for `/iam`.
      (when (and guess (cl-ppcre:scan "^[A-Z]{2,3}$" guess))
        guess))))

(defun sender-initials (user-id)
  "Work out whose initials USER-ID writes under.
Returns (values INITIAL SOURCE NAME): SOURCE is :REGISTERED for initials the
sender gave with `/iam` or `/subscribe`, :DERIVED for ones guessed from their
Slack profile (NAME then says which name the guess came from, so the reply
can show its working), and NIL with no initials when neither is available.
Registration always wins, so one `/iam` corrects a wrong guess for good."
  (if (null user-id)
      (values nil nil nil)
      (let ((registered (ignore-errors (find-slack-identity user-id))))
        (if registered
            (values (initial-of registered) :registered nil)
            (multiple-value-bind (display real) (slack-user-names user-id)
              (loop for name in (list display real)
                    for guess = (derive-initials name)
                    when guess
                      do (return (values guess :derived (string-trim '(#\Space) name)))
                    finally (return (values nil nil nil))))))))

(defparameter +unknown-sender-reply+
  "I don't know your initials yet, so I can't tell who \"me\" is.

Tell me once with `/iam` and your initials in CAPITALS — e.g. `/iam DMM` — then re-send your request. After that I'll read \"me\", \"my\" and \"I\" as you in every message."
  "What to say to someone who wrote \"me\" but cannot be identified.")

(defun derived-initials-notice (initial name)
  "The line prepended to a reply whose \"me\" was resolved from a Slack
profile name rather than from a registration.  It is repeated on every such
booking on purpose: a guess about who someone is never gets persisted, so
this stays visible until the sender confirms it with `/iam`, which is also
how a wrong guess is corrected."
  (format nil "I read \"me\" as *~A*~@[, from your Slack name \"~A\"~]. If that's not right, run `/iam` with your initials — e.g. `/iam DMM` — and I'll remember it."
          initial name))

(defun resolve-sender (text user-id)
  "Rewrite TEXT so its first-person references name the person who sent it.
Returns (values RESOLVED-TEXT NOTICE); NOTICE is a line the caller must post
above the reply, or NIL.  Returns NIL when TEXT refers to its sender and the
sender cannot be identified, meaning the caller should answer
+UNKNOWN-SENDER-REPLY+ and book nothing.

TEXT that refers to nobody comes back unchanged, with no notice and no
lookup at all, so a message that spells its passengers out costs neither a
database read nor a Slack API call."
  (if (not (self-reference-p text))
      (values text nil)
      (multiple-value-bind (initial source name) (sender-initials user-id)
        (cond
          ((null initial) nil)
          ((eq source :derived)
           (values (resolve-self-reference text initial)
                   (derived-initials-notice initial name)))
          (t (values (resolve-self-reference text initial) nil))))))

(defun run-request (text &key (allow-view t) user-id)
  "Run TEXT as a request from USER-ID.  When ALLOW-VIEW, a question about a
trip's timing that names no passengers (\"when is my ride home this coming
Tuesday?\", see SCHEDULE-QUESTION-INTERPRETATION) is answered as a `/view` of
that day, with no model involved.

Anything else is a booking.  First-person references in it are rewritten to
the sender's initials (RESOLVE-SENDER) so \"book me a van\" names somebody;
that happens after the timing-question check, which must keep seeing the
original \"my\" -- SCHEDULE-QUESTION-RANGE answers only for a question that
names no passengers, and a rewritten \"when is DMM ride home Tuesday?\" names
one.  A sender who writes \"me\" but cannot be identified is asked for `/iam`
and nothing is booked.  When the initials were guessed from a Slack profile
rather than registered, the reply carries a notice saying so.

The booking itself runs with the model as a fallback reader: the rule parser
goes first (BOOK-NATURAL); when it cannot find the destination, day or
passengers and a model is configured, LLM-INTERPRETATION rewrites the text as
a `/book` sentence or, when ALLOW-VIEW, a `/view` range.  Either way a request
that was read rather than run verbatim gets a list as its reply: the
interpretation line, then what the ordinary handler said about the reading
(RUN-INTERPRETATION).  Otherwise the reply is the BOOKING-INCOMPLETE report,
exactly as RUN-BOOKING gives it.  A rewritten booking goes through
RUN-BOOKING, so a sentence that is still incomplete gets the report and never
a second consultation.  Other errors propagate to the caller."
  (multiple-value-bind (command args)
      (and allow-view (schedule-question-interpretation text))
    (if command
        (run-interpretation command args)
        (multiple-value-bind (resolved notice) (resolve-sender text user-id)
          (if (null resolved)
              +unknown-sender-reply+
              (let ((reply
                      (handler-case (book-natural-reply resolved)
                        (booking-incomplete (c)
                          (or (when (first-pass-incomplete-p c)
                                (multiple-value-bind (command args)
                                    (llm-interpretation resolved :allow-view allow-view)
                                  (when command
                                    (run-interpretation command args))))
                              (princ-to-string c))))))
                (if notice
                    (cons notice (ensure-list reply))
                    reply)))))))

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
line here as ordinary text. Anything else goes to RUN-REQUEST: a question
about a trip's timing that names no passengers (\"when is my ride home
Tuesday?\") is shown as a `/view` of that day; the rest is a booking, whose
BOOKING-INCOMPLETE condition is posted as-is (it asks the user to re-send
the request with the missing pieces), unless a model is configured and can
read the message, in which case its reading is posted and then run; any
other failure is reported with the condition class as an error code and a
pointer to `/help`. Returns the
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
              (post-command-reply
               channel
               (run-request clean-text :user-id (gethash "user" event)))))))))

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

(defun parse-initial-argument (text)
  "Read TEXT as the initials argument of `/iam` or `/subscribe`, or NIL.
Stricter and more forgiving than PARSE-INITIAL at once.  A lone word is
accepted whatever its case, so `/iam dmm` works from a phone that
autocorrects capitals away, and rejected outright when it is not 2-3 letters,
so `/subscribe DMMM` asks again instead of quietly registering the DMM its
first three letters spell.  Anything longer than one word falls back to
PARSE-INITIAL's scan for an uppercase run, which is what lets a sentence
\(\"my initials are BW\") still work.  The result is upcased for the sheet."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) (or text ""))))
    (cond
      ((cl-ppcre:scan "^[A-Za-z]{2,3}$" trimmed) (string-upcase trimmed))
      ((cl-ppcre:scan "^[A-Za-z]+$" trimmed) nil)
      (t (let ((scanned (parse-initial trimmed)))
           (and scanned (string-upcase scanned)))))))

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
    (handler-case (run-request text :allow-view nil
                                    :user-id (gethash "user_id" params))
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
  /view Apr 20 to Apr 26
  /view Sept 19-22
  /view when is my ride home Tuesday?
A question about a trip's timing shows the day it asks about."
  (let* ((text (string-trim '(#\Space) (or (gethash "text" params) "")))
         (range (cond ((zerop (length text)) "today")
                      ((date-range-phrase-p text) text)
                      ;; "/view when is my ride home Tuesday?": the day the
                      ;; question asks about (SCHEDULE-QUESTION-RANGE)
                      ((schedule-question-range text))
                      (t text))))
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

(defslash-command "/iam" (params)
  "/iam <INITIALS> — tell the bot which initials you go by, so that \"me\",
\"my\" and \"I\" in a booking request name you. Example:
  /iam DMM
Persists across restarts, and is kept when you /unsubscribe. Sending it
again replaces the old initials. With no argument it reports what is
currently registered, so it doubles as `/whoami`."
  (let* ((text (gethash "text" params))
         (user-id (gethash "user_id" params))
         (initial (parse-initial-argument text)))
    (cond
      ((null user-id) "Could not identify Slack user.")
      ;; Bare `/iam` asks rather than sets: report the registration, or the
      ;; guess that would be used, so someone can check before booking.
      ((and (null initial)
            (or (null text) (zerop (length (string-trim '(#\Space #\Tab) text)))))
       (multiple-value-bind (current source name) (sender-initials user-id)
         (cond
           ((eq source :registered)
            (format nil "You are registered as *~A*. Send `/iam` and different initials to change that." current))
           ((eq source :derived)
            (format nil "You are not registered yet. I would read \"me\" as *~A*~@[, from your Slack name \"~A\"~]. Send `/iam ~A` to confirm it, or `/iam` with the right initials."
                    current name current))
           (t "You are not registered, and I can't guess your initials from your Slack profile. Send `/iam` followed by your initials in CAPITALS, e.g. `/iam DMM`."))))
      ((null initial)
       "Usage: type `/iam` followed by your initials in CAPITALS, e.g. `/iam DMM`. Two or three letters, exactly as they appear on the schedule sheet.")
      (t
       (add-slack-identity user-id initial)
       (format nil "Got it — you are *~A*. I'll read \"me\", \"my\" and \"I\" as ~:*~A in your booking requests."
               initial)))))

(defparameter *reminder-lead-minutes* 30
  "How many minutes before a trip leg leaves its driver is sent the reminder
DM (SCHEDULER-TICK).  /subscribe's reply quotes it; +HELP-TEXT+ states it in
words and has to be changed by hand.")

(defslash-command "/subscribe" (params)
  "/subscribe <INITIAL> — receive a direct message *REMINDER-LEAD-MINUTES*
before each trip leg today where you are the driver, naming the vehicle: the
departure when you are the first passenger listed for it, the return when you
are the first listed for that.
Example:
  /subscribe BW
Persists across restarts. Use /unsubscribe to stop."
  (let* ((text (gethash "text" params))
         (user-id (gethash "user_id" params))
         (initial (parse-initial-argument text)))
    (cond
      ((null user-id) "Could not identify Slack user.")
      ((null initial) "Usage: type `/subscribe` followed by your initials in CAPITALS, e.g. `/subscribe BW`.")
      (t
       (add-slack-subscription user-id (string-upcase initial))
       ;; Subscribing also says who you are, so nobody has to register twice
       ;; to get both reminders and "book me a van".  /unsubscribe drops only
       ;; the subscription; the identity outlives it.
       (add-slack-identity user-id (string-upcase initial))
       (format nil "Subscribed. You will be DM'd ~D minutes before each departure or return that ~A drives, with the vehicle to take. I'll also read \"me\" as ~:*~A in your messages."
               *reminder-lead-minutes* (string-upcase initial))))))

(defslash-command "/unsubscribe" (params)
  "/unsubscribe — stop receiving trip reminder DMs."
  (let ((user-id (gethash "user_id" params)))
    (cond
      ((null user-id) "Could not identify Slack user.")
      ((remove-slack-subscription user-id)
       "Unsubscribed. No further reminders will be sent. I still know your initials, so \"book me a van\" keeps working — `/iam` changes them.")
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
  "Dedup set. Key: (list user-id sheet-row vehicle date-key leg), LEG being
:DEPARTURE or :RETURN. Value: T.")

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

(defun reminder-key (user-id trip &optional (leg :departure))
  (list user-id
        (trip-block-start-row trip)
        (trip-vehicle trip)
        (today-date-key)
        leg))

(defun scheduler-tick ()
  "Single sweep: for each subscription, DM a reminder *REMINDER-LEAD-MINUTES*
before each leg of today's trips that the subscriber drives -- the departure
when they are the first dep-passenger, the return when they are the first
ret-passenger.  A leg is due from the lead time plus the 60s between sweeps
until it leaves, so the DM goes out between the lead time and a minute more
ahead of it, and a leg booked later than that is still reminded, on the next
sweep."
  (maybe-reset-sent-reminders)
  (let ((now (get-universal-time))
        (window (* 60 (1+ *reminder-lead-minutes*))))
    (dolist (sub (all-slack-subscriptions))
      (let ((user-id (car sub))
            (initial (cdr sub)))
        (flet ((remind (trip leg leaves text-fn)
                 (let ((key (reminder-key user-id trip leg)))
                   (when (and leaves
                              (<= 0 (- leaves now) window)
                              (not (gethash key *sent-reminders*)))
                     (post-slack-message user-id (funcall text-fn))
                     (setf (gethash key *sent-reminders*) t)
                     (format *error-output* "slack scheduler: reminded ~A/~A of the ~(~A~) at ~A~%"
                             user-id initial leg (trip-vehicle trip))))))
          (handler-case
              (progn
                (dolist (trip (trips-for-driver-today initial))
                  (remind trip :departure (trip-departure-universal-time trip)
                          (lambda () (remind-driver initial))))
                (dolist (trip (trips-for-return-driver-today initial))
                  (remind trip :return (trip-return-universal-time trip)
                          (lambda () (remind-return-driver trip)))))
            (error (c)
              (format *error-output*
                      "slack scheduler: error for ~A/~A: ~A~%"
                      user-id initial c))))))))

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
