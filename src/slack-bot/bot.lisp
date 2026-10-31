;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/slack-bot/bot
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
  (:import-from #:project-isidore/slack-bot/booking-nlp
                #:booking-incomplete
                #:booking-incomplete-missing
                #:booking-incomplete-preamble
                #:slot-label
                #:self-reference-p
                #:resolve-self-reference)
  (:import-from #:project-isidore/slack-bot/van-booking
                #:propose-natural
                #:revise-proposal
                #:commit-booking
                #:proposal-action
                #:proposal-day
                #:proposal-passengers
                #:proposal-day-view
                #:proposal-trip-view
                #:proposal-clash-checked-p
                #:view-schedule
                #:date-range-phrase-p
                #:remind-driver
                #:trips-for-driver-today
                #:trip-departure-universal-time
                #:trips-for-return-driver-today
                #:trip-return-universal-time
                #:remind-return-driver
                #:trip-vehicle
                #:trip-block-start-row)
  (:import-from #:project-isidore/slack-bot/database
                #:all-slack-subscriptions
                #:add-slack-subscription
                #:remove-slack-subscription
                #:find-slack-identity
                #:all-slack-identities
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

  POST /slack/commands — Slash commands /book, /view, /iam, /subscribe,
                         /unsubscribe, /help.
  POST /slack/events   — Events API (DMs / @mentions).  Only a message whose
                         text begins with one of those commands is acted on,
                         for clients that deliver \"/view\" as plain text
                         rather than expanding it; any other message is
                         answered with +COMMANDS-ONLY-REPLY+.

`/book` takes the request in free-form English (see
project-isidore/slack-bot/booking-nlp).  When a required piece is missing,
the BOOKING-INCOMPLETE report is posted back: it lists what was understood
and asks the user to re-send the request with the missing piece.  A
request that can be booked is not written straight away: the reply shows the
day as booked now and the trip as it would be written, and the booking is
held (*PENDING-BOOKINGS*) for *CONFIRMATION-MINUTES* until its asker
answers `y` (write it), `n` (drop it) or `edit` and a change (propose the
changed booking instead).  The answer may be a plain message -- the one kind
of message without a command that is acted on -- or `/book y`, for a
channel whose messages do not reach the bot.  `/view` takes a day or a range
of days.

A persistent /subscribe registry (graph-db-backed via
project-isidore/slack-bot/database)
plus an in-process scheduler thread DMs subscribed drivers
*REMINDER-LEAD-MINUTES* before each leg they drive: the departure when they are its first passenger, the
return when they are the first listed for the return.

A second registry records which initials each Slack account goes by, so a
message can say \"me\", \"my\" or \"I\" instead of spelling them out: /iam sets
it, /subscribe sets it as a side effect, and /unsubscribe leaves it alone.
For someone who has registered neither, the initials are guessed from their
Slack profile name (users.info), and the reply says so every time, because a
guess is never persisted.  The initials in both registries, with those on
the schedule sheet, are the ones a `/book` may write in lowercase.

Setup: set SLACK_BOT_TOKEN, SLACK_SIGNING_SECRET and VAN_SCHEDULE_SECRET (the
van schedule web app's, see van-booking.gs) environment variables.  The bot token needs the `users:read` scope for
the profile guess; without it users.info answers \"missing_scope\", which is
handled like any other failure -- everyone simply has to run /iam once.  See
the project plan for Slack app configuration details."))

(in-package #:project-isidore/slack-bot/bot)

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
  "*Van Steward Bot* — I book the house vans on the weekly schedule sheet and show you what is booked. Every request starts with a command: `/book`, `/view`, `/iam`, `/subscribe`, `/unsubscribe` or `/help`.

*To book a trip*, type `/book` followed by the trip in your own words. Say:
  • *destination* — `for swimming`, `to the dentist`, `the SHMS trip`
  • *day* — `tomorrow`, `next Tuesday`, `April 22`
  • *passengers* — initials, driver first: `for CG and BW`, `with MM, DM & CG`. Lowercase (`for cg and bw`) works for anyone already on the schedule; use capitals for someone new. Say `me` for yourself once you have run `/iam` (see below).
  • *times* out and back — `at 7 pm to 9 pm`, `leaving at 8:30 and back around 11`
  • *vehicle* (optional) — `take the DGH`, `in the LGH`, `use the Chevy`, `the personal car`. If you don't say, I take the first van that is free (DGH, then LGH, then the Chevy).
The order and the wording don't matter. For example:
  • `/book a van for swimming next Tuesday for CG and BW at 7 pm to 9 pm`
  • `/book Can BW and MM take the DGH to the dentist tomorrow, leaving at 8:30 and back around 11?`

*Before I write anything*, I show you the day as it is booked now and the trip as I would write it, and wait 15 minutes for your answer: `y` to book it, `n` to cancel, or `edit` and what to change — `edit 8 to 10 pm`, `edit take the LGH`, `edit Friday`, `edit for BW and GG` (the new list), `edit add GG`. If a plain reply doesn't reach me, put `/book` in front: `/book y`.

I never book over a clash: if the van you asked for is already out at that time, or if somebody on your list is already on another trip that overlaps, I say which trip is in the way and book nothing.

*To join a trip already on the sheet*, name the trip and who is coming; no times needed:
  • `/book add GG to the SHMS trip on Thursday`

If something is missing, I reply with what I understood and ask you to re-send the whole `/book` request with the missing piece added.

*To see the schedule*, type `/view` for today, or add a day or a range:
  `/view tomorrow`   `/view the rest of the week`   `/view the weekend`   `/view Apr 20 to Apr 26`   `/view Sept 19-22`

*Who you are*: type `/iam` and your initials, e.g. `/iam DMM`, and I'll read `me`, `my` and `I` as you from then on — `/book me a van to the dentist tomorrow 9 to 11`, `/book can I get a van for the pool Saturday 8-10`, `/book me and CG to swimming Tuesday 7-9pm`. Type `/iam` on its own to see who I have you down as. I remember it across restarts and through `/unsubscribe`.

*Reminders*: type `/subscribe` and your initials, e.g. `/subscribe BW`, and I'll message you 30 minutes before each trip you are driving, out or back, with the vehicle you are taking (the first name listed drives). `/unsubscribe` turns them off.

*This message*: `/help`."
  "The help page: what the bot is and how to use every command, for people
who are not programmers.  Posted for /help.  Slack mrkdwn: *bold*, `code`.
Keep it well under Slack's 4000-character message limit.")

(defparameter +commands-only-reply+
  "I only act on commands, which start with `/`: `/book` and the trip to book one, e.g. `/book BW to the pool tomorrow 7-9pm`; `/view` and a day to see the schedule; `/iam`, `/subscribe` and `/unsubscribe`; and `/help` for how to use them all."
  "The reply to a message that does not begin with a command.  Nothing in such
a message is read as a request: a booking needs `/book`.")

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

(defun registered-initials ()
  "The initials registered with `/iam` or `/subscribe`, which a booking may
write in lowercase like those on the sheet (BOOK-NATURAL), or NIL when the
store cannot be read."
  (ignore-errors
   (remove-duplicates (mapcar #'cdr (append (all-slack-identities)
                                            (all-slack-subscriptions)))
                      :test #'string=)))

;;; Booking confirmation
;;;
;;; `/book` writes nothing straight away.  It shows the day as booked now and
;;; the trip as it would be written, and holds that proposal for its asker's
;;; answer: `y` writes it (COMMIT-BOOKING re-reads the sheet and checks it
;;; again first), `n` drops it, and `edit` with a change proposes the changed
;;; booking in its place.

(defparameter *confirmation-minutes* 15
  "How long a proposed booking waits for its asker's answer.  +HELP-TEXT+
states it in words and has to be changed by hand.")

(defstruct pending-booking
  "A PROPOSAL shown to its asker and not yet answered, and the universal time
after which a yes no longer writes it."
  proposal expires)

(defvar *pending-bookings* (make-hash-table :test #'equal)
  "Slack user id -> the PENDING-BOOKING awaiting that user's answer.  One per
user: a new `/book` replaces it.  An expired one stays until it is answered
or replaced, so a late `y` hears that it expired rather than that nothing is
waiting.  Held in memory only, so a restart forgets them.")

(defvar *pending-lock* (bt:make-lock "slack-pending-bookings"))

(defun pending-expired-p (pending)
  (> (get-universal-time) (pending-booking-expires pending)))

(defun hold-booking (user-id proposal)
  "Hold PROPOSAL for USER-ID's answer, in place of whatever was held for them.
True when that was a booking still waiting, which the reply mentions."
  (let ((pending (make-pending-booking
                  :proposal proposal
                  :expires (+ (get-universal-time) (* 60 *confirmation-minutes*)))))
    (bt:with-lock-held (*pending-lock*)
      (let ((old (gethash user-id *pending-bookings*)))
        (setf (gethash user-id *pending-bookings*) pending)
        (and old (not (pending-expired-p old)))))))

(defun held-booking (user-id)
  "The PENDING-BOOKING held for USER-ID, expired or not, or NIL."
  (bt:with-lock-held (*pending-lock*)
    (gethash user-id *pending-bookings*)))

(defun take-booking (user-id)
  "Remove the PENDING-BOOKING held for USER-ID and return it, or NIL.  Taking
it under the lock is what stops two quick `y`s writing one booking twice."
  (bt:with-lock-held (*pending-lock*)
    (prog1 (gethash user-id *pending-bookings*)
      (remhash user-id *pending-bookings*))))

(defun answer-word (text)
  "Read TEXT as an answer to a booking's confirmation: :YES for `y` or `yes`,
:NO for `n`, `no` or `cancel`, :EDIT for `edit` followed by the change, which
is the second value; NIL for anything else.  Case and trailing punctuation do
not matter, but a yes or a no must stand alone: \"no van needed after all\"
is not an answer."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) (or text ""))))
    (cond ((cl-ppcre:scan "(?i)^(?:y|yes)[.!]*$" trimmed) :yes)
          ((cl-ppcre:scan "(?i)^(?:n|no|cancel)[.!]*$" trimmed) :no)
          (t (multiple-value-bind (match groups)
                 (cl-ppcre:scan-to-strings "(?is)^edit\\b[\\s:,-]*(.*)$" trimmed)
               (when match
                 (values :edit (aref groups 0))))))))

(defparameter +edit-examples+
  "`edit 8 to 10 pm`, `edit take the LGH`, `edit Friday`, `edit for BW and GG` or `edit add GG`"
  "Examples of an `edit` answer, quoted wherever the bot asks for one.")

(defparameter +answer-reminder+
  "*Reply `y` (yes) to book it, `n` (no) to cancel, or `edit` and the change.*"
  "The last line of every reply that leaves a booking waiting, so the way to
answer is the last thing read.")

(defun fenced-table (heading table &optional footer)
  "TABLE, a schedule from FORMAT-SCHEDULE-TABLE, in code fences
(CODE-BLOCK-CHUNKS) as a list of messages, with HEADING above the first and
FOOTER, when given, below the last."
  (let ((parts (code-block-chunks table :header-lines 2)))
    (setf (first parts) (format nil "~a~%~a" heading (first parts)))
    (when footer
      (setf (car (last parts)) (format nil "~a~%~a" (car (last parts)) footer)))
    parts))

(defun confirmation-reply (proposal &key replaced)
  "The messages that put PROPOSAL to its asker: the day as booked now, then
the trip as it would be written and how to answer.  REPLACED says PROPOSAL
took the place of a booking that was still waiting."
  (let ((day (proposal-day proposal))
        (day-view (proposal-day-view proposal)))
    (append
     (if day-view
         (fenced-table (format nil "Booked on *~a* so far:" day) day-view)
         (list (format nil "Nothing is booked on *~a* yet." day)))
     (fenced-table
      (if (eq (proposal-action proposal) :join)
          (format nil "I'd add ~{~a~^, ~} to this trip, which would then read:"
                  (proposal-passengers proposal))
          "I'd book this trip:")
      (proposal-trip-view proposal)
      (format nil "~:[Note: I can't read this trip's times as a window, so I haven't checked it against the other trips that day.~%~;~]~:[~;This replaces the booking you had waiting.~%~]Nothing is written until you answer, and I'll wait ~D minutes. To change something, reply `edit` and the change, e.g. ~a. If a plain reply doesn't reach me here, start it with `/book`, as in `/book y`.~2%~a"
              (proposal-clash-checked-p proposal) replaced
              *confirmation-minutes* +edit-examples+ +answer-reminder+)))))

(defun propose-reply (text user-id)
  "Propose TEXT as a booking (PROPOSE-NATURAL) and return the reply.  When
there is a booking to make, it is held for USER-ID's answer (HOLD-BOOKING)
and the reply is its confirmation (CONFIRMATION-REPLY); otherwise the reply
is what PROPOSE-NATURAL printed about why there is none, or a placeholder
when it printed nothing.  Conditions propagate."
  (let* ((proposal nil)
         (output (with-output-to-string (*standard-output*)
                   (setf proposal (propose-natural
                                   text :registered-initials (registered-initials))))))
    (cond (proposal
           (confirmation-reply proposal :replaced (hold-booking user-id proposal)))
          ((plusp (length (string-trim '(#\Space #\Newline) output)))
           output)
          (t "Nothing was booked (no output)."))))

(defun run-booking (text user-id)
  "Propose TEXT as a booking held for USER-ID (PROPOSE-REPLY) and return the
reply to post.  A BOOKING-INCOMPLETE condition becomes its own report, which
asks the user to re-send the request with the missing pieces.  Other errors
propagate to the caller."
  (handler-case (propose-reply text user-id)
    (booking-incomplete (c)
      (princ-to-string c))))

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

Tell me once with `/iam` and your initials — e.g. `/iam DMM` — then re-send your `/book` request. After that I'll read \"me\", \"my\" and \"I\" as you in every message."
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

(defun run-request (text &key user-id)
  "Run TEXT, the argument of a `/book`, as a booking from USER-ID: propose it
and hold it for their answer (RUN-BOOKING).
First-person references in it are rewritten to the sender's initials
\(RESOLVE-SENDER) so \"book me a van\" names somebody.  A sender who writes
\"me\" but cannot be identified is asked for `/iam` and nothing is proposed.
When the initials were guessed from a Slack profile rather than registered,
the reply is a list: a notice saying so, then RUN-BOOKING's reply; otherwise
it is RUN-BOOKING's reply alone.  Errors other than BOOKING-INCOMPLETE
propagate to the caller."
  (multiple-value-bind (resolved notice) (resolve-sender text user-id)
    (cond ((null resolved) +unknown-sender-reply+)
          (notice (cons notice (ensure-list (run-booking resolved user-id))))
          (t (run-booking resolved user-id)))))

(defparameter +nothing-waiting-reply+
  "There's no booking waiting for your answer, so nothing was written. If you were answering one, it has been answered already or I have restarted since: send the `/book` request again."
  "The reply to `y`, `n` or `edit` from someone with no booking held.")

(defparameter +still-waiting+
  (format nil "The booking above is still waiting.~%~a" +answer-reminder+)
  "Closes the reply to an `edit` that changed nothing.")

(defun expired-reply ()
  (format nil "That booking waited more than ~D minutes for an answer, so I dropped it and wrote nothing. Send the `/book` request again."
          *confirmation-minutes*))

(defun commit-reply (proposal)
  "Write PROPOSAL (COMMIT-BOOKING) and return what it printed: what was
written, or why nothing was."
  (let* ((written nil)
         (output (with-output-to-string (*standard-output*)
                   (setf written (commit-booking proposal)))))
    (cond ((null written)
           (format nil "~aSend the `/book` request again to try afresh." output))
          ((plusp (length (string-trim '(#\Space #\Newline) output))) output)
          (t "Booked."))))

(defun edit-booking (proposal change user-id)
  "Apply CHANGE, the words after `edit`, to PROPOSAL, the booking held for
USER-ID (REVISE-PROPOSAL), and return the reply: the confirmation of the
changed booking, which is held in PROPOSAL's place, or why it could not be
changed, with PROPOSAL left waiting.  \"me\" in CHANGE is the sender, as in
`/book` (RESOLVE-SENDER)."
  (if (zerop (length (string-trim '(#\Space #\Tab #\Newline) change)))
      (format nil "What should I change? Reply `edit` and the change, e.g. ~a." +edit-examples+)
      (multiple-value-bind (resolved notice) (resolve-sender change user-id)
        (if (null resolved)
            +unknown-sender-reply+
            (let* ((revised nil)
                   (changed nil)
                   (incomplete nil)
                   (output (with-output-to-string (*standard-output*)
                             (handler-case
                                 (multiple-value-setq (revised changed)
                                   (revise-proposal proposal resolved
                                                    :registered-initials (registered-initials)))
                               (booking-incomplete (c) (setf incomplete c))))))
              (cond
                (incomplete
                 (format nil "~@[~a ~]So nothing changed: I'd also need ~{the ~a~^ and ~}. Reply `edit` with all of it at once, e.g. `edit Friday 7 pm to 9 pm`.~%~a"
                         (booking-incomplete-preamble incomplete)
                         (mapcar #'slot-label (booking-incomplete-missing incomplete))
                         +still-waiting+))
                ((null changed)
                 (format nil "I couldn't find anything to change in \"~a\". Try e.g. ~a.~%~a"
                         change +edit-examples+ +still-waiting+))
                ((null revised)
                 (format nil "~aSo nothing changed. ~a" output +still-waiting+))
                (t
                 (hold-booking user-id revised)
                 (append (and notice (list notice))
                         (confirmation-reply revised)))))))))

(defun answer-booking (text user-id)
  "Act on TEXT, an answer (ANSWER-WORD) to the booking held for USER-ID, and
return the reply.  `y` writes the booking (COMMIT-REPLY), `n` drops it, and
`edit` changes it (EDIT-BOOKING).  A booking past *CONFIRMATION-MINUTES* is
dropped unwritten whatever the answer."
  (multiple-value-bind (answer change) (answer-word text)
    (if (eq answer :edit)
        (let ((pending (held-booking user-id)))
          (cond ((null pending) +nothing-waiting-reply+)
                ((pending-expired-p pending)
                 (take-booking user-id)
                 (expired-reply))
                (t (edit-booking (pending-booking-proposal pending) change user-id))))
        (let ((pending (take-booking user-id)))
          (cond ((null pending) +nothing-waiting-reply+)
                ((pending-expired-p pending) (expired-reply))
                ((eq answer :no) "Cancelled. Nothing was booked.")
                (t (commit-reply (pending-booking-proposal pending))))))))

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
Ignores bot messages to prevent loops.  Text that begins with a slash
command (`/view`, `/book ...`) runs that command's handler -- Slack expands
its own slash commands into a /slack/commands POST, but a client that does
not (a bridged room, an uninstalled command) delivers the line here as
ordinary text.  The one exception is the answer to a booking's confirmation
-- `y`, `n` or `edit` and a change (ANSWER-WORD) -- which goes to
ANSWER-BOOKING.  Any other text is answered with +COMMANDS-ONLY-REPLY+ and
not read at all: every request starts with a command.  Returns the worker
thread a command or an answer runs in, or NIL when the event was answered
synchronously or ignored."
  (let ((text (gethash "text" event))
        (channel (gethash "channel" event))
        (bot-id (gethash "bot_id" event))
        (subtype (gethash "subtype" event)))
    (when (or bot-id subtype (null text) (null channel))
      (return-from handle-message-event nil))
    (let ((clean-text (strip-mention text)))
      (when (zerop (length (string-trim '(#\Space) clean-text)))
        (return-from handle-message-event nil))
      (multiple-value-bind (command args) (parse-slash-text clean-text)
        (let ((handler (and command
                            (cdr (assoc command *slash-commands* :test #'string-equal)))))
          (cond
            ((and (null command) (answer-word clean-text))
             (with-slack-error-reply (channel :context "Answer"
                                              :thread-name "slack-answer")
               (post-command-reply channel
                                   (answer-booking clean-text (gethash "user" event)))))
            ((null command)
             (post-slack-message channel +commands-only-reply+)
             nil)
            ((null handler)
             (post-slack-message
              channel
              (format nil "Unknown command: ~A. Type `/help` to see what I can do." command))
             nil)
            (t
             (with-slack-error-reply (channel
                                      :context (format nil "Command ~A" command)
                                      :thread-name (format nil "slack-cmd-~A" command))
               (post-command-reply channel
                                   (funcall handler
                                            (message-command-params event command args)))))))))))

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
re-send the request including it.
Nothing is written at once: the reply shows the day as booked and the trip as
it would be written, and waits *CONFIRMATION-MINUTES* for the answer, which
can also come as the argument of `/book` itself:
  /book y
  /book n
  /book edit 8 to 10 pm"
  (let ((text (gethash "text" params))
        (user-id (gethash "user_id" params)))
    (when (or (null text) (zerop (length (string-trim '(#\Space) text))))
      (return-from cmd-book
        "Usage: type `/book` followed by the trip in your own words, e.g. `/book take the DGH to the dentist tomorrow for BW, leaving at 8:30 and back by 11`. Type `/help` for more."))
    (handler-case (if (answer-word text)
                      (answer-booking text user-id)
                      (run-request text :user-id user-id))
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
A range it cannot read gets examples of ones it can."
  (let* ((text (string-trim '(#\Space) (or (gethash "text" params) "")))
         (range (if (zerop (length text)) "today" text)))
    (unless (date-range-phrase-p range)
      (return-from cmd-view
        (format nil "I can't read \"~A\" as a day or a range of days. Try `/view` on its own for today, or `/view tomorrow`, `/view Friday`, `/view the weekend`, `/view next week` or `/view Apr 20 to Apr 26`."
                text)))
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
           (t "You are not registered, and I can't guess your initials from your Slack profile. Send `/iam` followed by your initials, e.g. `/iam DMM`."))))
      ((null initial)
       "Usage: type `/iam` followed by your initials, e.g. `/iam DMM`. Two or three letters, exactly as they appear on the schedule sheet.")
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
      ((null initial) "Usage: type `/subscribe` followed by your initials, e.g. `/subscribe BW`. Two or three letters, exactly as they appear on the schedule sheet.")
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
