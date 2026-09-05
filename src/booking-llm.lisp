;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/booking-llm
  (:use #:common-lisp)
  (:local-nicknames (#:nlp #:project-isidore/nlp)
                    (#:bn #:project-isidore/booking-nlp))
  (:import-from #:alexandria
                #:plist-hash-table)
  (:import-from #:bordeaux-threads)
  (:import-from #:cl-ppcre)
  (:import-from #:com.inuoe.jzon)
  (:import-from #:dexador)
  (:import-from #:chronicity)
  (:import-from #:project-isidore/van-booking
                #:date-range-phrase-p)
  (:export
   ;; configuration
   #:*llm-url* #:*llm-timeout* #:*llm-connect-timeout* #:*llm-max-tokens*
   #:load-llm-config #:llm-enabled-p #:warm-up-llm
   ;; request and reply
   #:intent-schema #:system-prompt #:llm-request-body #:parse-llm-response
   #:vehicle-in-text-p #:time-in-text-p #:day-in-text-p #:destination-in-text
   #:word-in-text-p #:constrain-intent #:llm-normalize
   ;; rendering and validation
   #:sanitize-field #:initial-in-text-p
   #:render-book-request #:render-view-range
   #:validate-book-intent #:render-intent)
  (:documentation
   "A small local language model as a fallback reader for van-booking messages.

The rule parser in PROJECT-ISIDORE/BOOKING-NLP handles every phrasing in its
corpus, deterministically and in-process.  When it cannot find the
destination, day or passengers in a message -- a paraphrase outside its cue
lists, or a question such as \"what's on the van this week?\" -- the Slack
bot asks a llama.cpp server on the loopback interface what the message
means.  The model does one thing only: it fills a fixed JSON object (intent,
destination, day, passengers, times, vehicle, range), and the server's
JSON-schema grammar makes any other output impossible.

Nothing the model says reaches the sheet directly.  RENDER-INTENT turns the
JSON into a canonical `/book` sentence or `/view` range, VALIDATE-BOOK-INTENT
re-parses that sentence with the rule parser and checks that every initial
occurs in the original message, and the bot echoes the interpretation before
running the ordinary handler on it.  With VAN_LLM_URL unset, or the server
unreachable or slow, every function here returns NIL and the bot behaves
exactly as it did without this file."))

(in-package #:project-isidore/booking-llm)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defvar *llm-url* nil
  "URL of the llama-server chat-completions endpoint
\(http://127.0.0.1:8081/v1/chat/completions), or NIL when the fallback is
disabled.  Set from VAN_LLM_URL by LOAD-LLM-CONFIG.")

(defparameter *llm-timeout* 45
  "Seconds to wait for the model's reply before giving up.")

(defparameter *llm-connect-timeout* 2
  "Seconds to wait for the connection, so a stopped server costs almost nothing.")

(defparameter *llm-max-tokens* 256
  "Longest reply accepted; the JSON object needs well under 100 tokens.")

(defun load-llm-config ()
  "Read VAN_LLM_URL into *LLM-URL*; blank or unset disables the fallback.
Must be called at runtime, not at build time, for the same reason as
PROJECT-ISIDORE/SLACK-BOT:LOAD-CONFIG."
  (let ((url (uiop:getenv "VAN_LLM_URL")))
    (setf *llm-url* (and url
                         (plusp (length (string-trim '(#\Space #\Tab) url)))
                         (string-trim '(#\Space #\Tab) url)))))

(defun llm-enabled-p ()
  "True when a model server URL is configured."
  (and *llm-url* t))

;;; ---------------------------------------------------------------------------
;;; The JSON object the model fills in
;;; ---------------------------------------------------------------------------

(defparameter +time-pattern+ "^(1[0-2]|[1-9]):[0-5][0-9] (am|pm)$"
  "Times must come back exactly as the booking ruler reads them: \"8:30 am\".")

(defparameter +initials-pattern+ "^[A-Z]{2,3}$"
  "Passenger initials as the sheet writes them.")

(defun json-object (&rest plist)
  "An EQUAL hash-table from PLIST, the shape jzon serialises as a JSON object.
Keys keep their order, which matters: llama.cpp emits properties in schema
order, so INTENT is decided before the fields that depend on it."
  (plist-hash-table plist :test #'equal))

(defun intent-schema ()
  "JSON Schema for the model's reply.  Everything is REQUIRED and nullable so
the grammar always produces the same nine keys; only the features llama.cpp's
schema-to-grammar converter supports are used (enum with null, anchored
pattern, maxLength, maxItems, additionalProperties false)."
  (json-object
   "type" "object"
   "properties"
   (json-object
    "intent" (json-object "type" "string" "enum" #("book" "view" "other"))
    "destination" (json-object "type" #("string" "null") "maxLength" 60)
    "day" (json-object "type" #("string" "null") "maxLength" 40)
    "passengers" (json-object "type" "array"
                              "items" (json-object "type" "string"
                                                   "pattern" +initials-pattern+)
                              "maxItems" 8)
    "dep_time" (json-object "type" #("string" "null") "pattern" +time-pattern+)
    "ret_time" (json-object "type" #("string" "null") "pattern" +time-pattern+)
    "vehicle" (json-object "enum" (vector "dgh" "lgh" "chevy" "personal" 'null))
    "range" (json-object "type" #("string" "null") "maxLength" 60)
    "unclear" (json-object "type" #("string" "null") "maxLength" 120))
   "required" #("intent" "destination" "day" "passengers" "dep_time" "ret_time"
                "vehicle" "range" "unclear")
   "additionalProperties" nil))

;;; ---------------------------------------------------------------------------
;;; The prompt
;;; ---------------------------------------------------------------------------

(defun intent-json (&key intent destination day passengers dep-time ret-time
                         vehicle range unclear)
  "The reply JSON for one example, serialised in schema key order."
  (com.inuoe.jzon:stringify
   (json-object "intent" intent
                "destination" (or destination 'null)
                "day" (or day 'null)
                "passengers" (coerce passengers 'vector)
                "dep_time" (or dep-time 'null)
                "ret_time" (or ret-time 'null)
                "vehicle" (or vehicle 'null)
                "range" (or range 'null)
                "unclear" (or unclear 'null))))

(defparameter *few-shot-examples*
  (list
   (cons "Can BW and MM take the DGH to the dentist tomorrow morning, leaving at 8:30 and back around 11?"
         (intent-json :intent "book" :destination "dentist" :day "tomorrow"
                      :passengers '("BW" "MM") :dep-time "8:30 am" :ret-time "11:00 am"
                      :vehicle "dgh"))
   (cons "Please add GG to the SHMS trip on Thursday"
         (intent-json :intent "book" :destination "SHMS" :day "Thursday"
                      :passengers '("GG")))
   (cons "hey can you book swimming tues 7-9pm for cg & bw"
         (intent-json :intent "book" :destination "swimming" :day "Tuesday"
                      :passengers '("CG" "BW") :dep-time "7:00 pm" :ret-time "9:00 pm"))
   (cons "We need the van Friday, going to Saint Marys church for the ordination with MM DM and CG, out at 9 and home by noon"
         (intent-json :intent "book" :destination "Saint Marys church" :day "Friday"
                      :passengers '("MM" "DM" "CG") :dep-time "9:00 am" :ret-time "12:00 pm"))
   (cons "could BW have a van Monday 6 to 9 for the airport?"
         (intent-json :intent "book" :destination "airport" :day "Monday"
                      :passengers '("BW") :dep-time "6:00 am" :ret-time "9:00 am"))
   (cons "what's on the van this week?"
         (intent-json :intent "view" :range "this week"))
   (cons "anything on next week?"
         (intent-json :intent "view" :range "next week"))
   (cons "who has the DGH tomorrow?"
         (intent-json :intent "view" :range "tomorrow"))
   (cons "what's happening on Saturday?"
         (intent-json :intent "view" :range "Saturday"))
   (cons "is anything booked Sept 19 to 22?"
         (intent-json :intent "view" :range "Sept 19 to Sept 22"))
   (cons "thanks!"
         (intent-json :intent "other" :unclear "Not a booking or a schedule question.")))
  "Message / JSON pairs shown to the model.  The bookings are corpus
sentences from booking-nlp-tests, so the model's reading matches the rule
parser's; the questions cover the view ranges the schedule understands.")

(defparameter *prompt-prefix*
  (format nil "~{~a~%~}"
          '("You turn one Slack message about the house vans into a JSON object with exactly these keys: intent, destination, day, passengers, dep_time, ret_time, vehicle, range, unclear. Every value must come from the message; never invent anything. Use null for anything the message does not say."
            ""
            "- intent: \"book\" when the message wants to book or reserve a van, or add people to a trip; \"view\" when it asks what is booked, who has a van, or to see the schedule; \"other\" for anything else (thanks, chit-chat, questions about you). A message that names passengers (initials) and a destination is a booking even when it is phrased as a question."
            "- destination: where the trip goes, in the message's own words, without a leading \"the\" or a trailing \"trip\" (\"dentist\", \"SHMS\", \"Saint Marys church\"). null unless intent is \"book\"."
            "- day: the day as the message gives it, with shorthand spelled out (\"tmrw\" -> \"tomorrow\", \"thurs\" -> \"Thursday\", \"Sept 12\" stays \"Sept 12\", \"4/22\" stays \"4/22\"); null if not given."
            "- passengers: the 2-3 letter initials in the message, uppercased, in the order written (driver first), e.g. [\"BW\",\"MM\"]. Only initials that appear in the message; [] if none."
            "- dep_time: the departure time as \"H:MM am\" or \"H:MM pm\" (\"8:30 am\"); null if not given. A bare hour is daytime: 7 to 11 are am, 12 to 6 are pm (8 -> \"8:00 am\", 2 -> \"2:00 pm\", 5 -> \"5:00 pm\"), \"half past 8\" -> \"8:30 am\", noon -> \"12:00 pm\"."
            "- ret_time: the return time in the same format (\"back by 5\" -> \"5:00 pm\"); null if not given."
            "- vehicle: \"dgh\", \"lgh\", \"chevy\" or \"personal\" only when the message itself says DGH, LGH, Chevy or personal car. \"the van\" or \"a van\" is not a vehicle choice: use null."
            "- range: only when intent is \"view\": the period asked about, in these words: \"today\", \"tomorrow\", \"this week\", \"next week\", \"the weekend\", \"the rest of the week\", a day such as \"Saturday\" or \"Sept 12\", or \"Sept 19 to Sept 22\". \"today\" when no period is given. Keep \"today\" and \"tomorrow\" as words; never replace them with the weekday's name. null unless intent is \"view\"."
            "- unclear: one short sentence about what could not be determined; else null."
            ""
            "Examples:"))
  "The fixed part of the system prompt.  It comes first so llama-server's
prompt cache can reuse it across requests; the date and the sheet's
destinations follow it.")

(defun system-prompt (&key known-destinations (today (chronicity:parse "today")))
  "The system prompt: field rules, examples, then today's date and the
destinations already on the sheet (KNOWN-DESTINATIONS, sanitised)."
  (with-output-to-string (s)
    (write-string *prompt-prefix* s)
    (loop for (message . json) in *few-shot-examples*
          do (format s "Message: ~a~%JSON: ~a~%" message json))
    (format s "~%Today is ~a ~d.~%"
            (bn:format-booking-day today) (chronicity:year-of today))
    (let ((known (remove nil (mapcar (lambda (d) (sanitize-field d :max 40))
                                     known-destinations))))
      (when known
        (format s "Destinations already on the sheet (reuse these spellings when the message means one of them): ~{~a~^, ~}.~%"
                known)))))

;;; ---------------------------------------------------------------------------
;;; Talking to llama-server
;;; ---------------------------------------------------------------------------

(defun truncate-input (text)
  "TEXT cut to what the rule parser would read anyway."
  (if (> (length text) nlp:*max-input-length*)
      (subseq text 0 nlp:*max-input-length*)
      text))

(defun llm-request-body (text &key known-destinations today)
  "The chat-completions request for TEXT as a JSON string.  Deterministic
sampling, thinking off (llama.cpp does not apply the grammar while a model
thinks), the schema as RESPONSE_FORMAT, and CACHE_PROMPT so the fixed prefix
of the system prompt is not re-read on every call."
  (com.inuoe.jzon:stringify
   (json-object
    "model" "local"
    "temperature" 0
    "seed" 42
    "max_tokens" *llm-max-tokens*
    "cache_prompt" t
    "chat_template_kwargs" (json-object "enable_thinking" nil)
    "response_format" (json-object
                       "type" "json_schema"
                       "json_schema" (json-object "name" "van_intent"
                                                  "schema" (intent-schema)))
    "messages" (vector (json-object
                        "role" "system"
                        "content" (if today
                                      (system-prompt :known-destinations known-destinations
                                                     :today today)
                                      (system-prompt :known-destinations known-destinations)))
                       (json-object "role" "user" "content" text)))))

(defparameter *intent-keywords*
  '(("book" . :book) ("view" . :view) ("other" . :other)))

(defparameter *vehicle-keywords*
  '(("dgh" . :dgh) ("lgh" . :lgh) ("chevy" . :chevy) ("personal" . :personal)))

(defun json-string (value)
  "VALUE trimmed when it is a non-blank string, else NIL (JSON null parses as
the symbol NULL, which is not a string)."
  (and (stringp value)
       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) value)))
         (and (plusp (length trimmed)) trimmed))))

(defun json-keyword (value table)
  "The keyword TABLE maps the string VALUE to, or NIL for anything else."
  (and (stringp value)
       (cdr (assoc value table :test #'string-equal))))

(defun intent-plist (object)
  "Read the model's reply OBJECT (a parsed JSON object) into a plist:
\(:intent :book :destination \"dentist\" :day \"tomorrow\" :passengers (\"BW\")
 :dep-time \"8:30 am\" :ret-time nil :vehicle :dgh :range nil :unclear nil).
Anything outside the schema's vocabulary becomes NIL."
  (flet ((field (key) (gethash key object)))
    (list :intent (json-keyword (field "intent") *intent-keywords*)
          :destination (json-string (field "destination"))
          :day (json-string (field "day"))
          :passengers (let ((value (field "passengers")))
                        (and (vectorp value)
                             (remove-duplicates
                              (loop for p across value
                                    for initial = (json-string p)
                                    when initial collect (string-upcase initial))
                              :test #'string= :from-end t)))
          :dep-time (json-string (field "dep_time"))
          :ret-time (json-string (field "ret_time"))
          :vehicle (json-keyword (field "vehicle") *vehicle-keywords*)
          :range (json-string (field "range"))
          :unclear (json-string (field "unclear")))))

(defun parse-llm-response (body)
  "Return (values PLIST RAW-CONTENT) for a chat-completions reply BODY (a
string or octets), or NIL when the reply has no choice, was cut off before
the closing brace (finish_reason other than \"stop\"), or is not an object."
  (let* ((root (com.inuoe.jzon:parse body))
         (choices (and (hash-table-p root) (gethash "choices" root)))
         (choice (and (vectorp choices) (plusp (length choices)) (aref choices 0)))
         (finish (and (hash-table-p choice) (gethash "finish_reason" choice)))
         (message (and (hash-table-p choice) (gethash "message" choice)))
         (content (and (hash-table-p message) (gethash "content" message))))
    (when (and (stringp content) (equal finish "stop"))
      (let ((object (ignore-errors (com.inuoe.jzon:parse content))))
        (when (hash-table-p object)
          (values (intent-plist object) content))))))

(defparameter *vehicle-words*
  '((:dgh "dgh") (:lgh "lgh") (:chevy "chevy" "chevrolet") (:personal "personal"))
  "Words the message must contain for the model's vehicle to stand.")

(defparameter *number-words*
  '(("one" . 1) ("two" . 2) ("three" . 3) ("four" . 4) ("five" . 5) ("six" . 6)
    ("seven" . 7) ("eight" . 8) ("nine" . 9) ("ten" . 10) ("eleven" . 11) ("twelve" . 12))
  "Hours written as words, as a message may give them (\"seven PM\").")

(defun word-in-text-p (word text)
  "True when WORD occurs in TEXT as a whole word, ignoring case and any
punctuation inside the word, so \"marys\" is found in \"Mary's\" and \"st\" in
\"St.\"."
  (let ((letters (remove-if-not #'alphanumericp word)))
    (and (plusp (length letters))
         (cl-ppcre:scan
          (format nil "(?i)(?<![a-z0-9])~{~a~^[^a-z0-9 ]*~}(?![a-z0-9])"
                  (map 'list #'string letters))
          text)
         t)))

(defun time-in-text-p (time text)
  "True when TEXT gives some ground for TIME (\"8:30 am\"): its hour written
as digits (\"8:30\", \"8am\", \"8-10\") or as a word (\"eight\"), or noon or
midnight for twelve.  Loose on purpose -- a day of the month counts -- it
exists to stop a time the message never mentions at all."
  (let* ((hour (and (stringp time) (parse-integer time :junk-allowed t)))
         (word (car (rassoc hour *number-words*))))
    (and hour
         (or (cl-ppcre:scan (format nil "(?<![0-9])~d(?![0-9])" hour) text)
             (and word (word-in-text-p word text))
             (and (= hour 12)
                  (or (word-in-text-p "noon" text) (word-in-text-p "midnight" text))))
         t)))

(defparameter *day-evidence-regex*
  (concatenate
   'string
   "(?i)(?<![a-z])(?:today|tonight|tomorrow|tmrw|tmr|tmw|2moro|tomorow"
   "|mon|monday|tue|tues|tuesday|wed|weds|wednesday|thu|thur|thurs|thursday"
   "|fri|friday|sat|saturday|sun|sunday"
   "|jan|january|feb|february|mar|march|apr|april|may|jun|june|jul|july"
   "|aug|august|sep|sept|september|oct|october|nov|november|dec|december"
   "|(?:this|next|last)\\s+week)(?![a-z])"
   "|\\d{1,2}/\\d{1,2}|\\d{4}-\\d{2}-\\d{2}|(?<![0-9])\\d{1,2}(?:st|nd|rd|th)(?![a-z])")
  "Anything a message can name a day with: a relative day (and its usual
shorthand), a weekday, a month, a numeric date or an ordinal.")

(defun day-in-text-p (text)
  "True when TEXT names a day in any of the ways *DAY-EVIDENCE-REGEX* lists,
so a day the model made up (there is nothing in \"ZZ and QQ to the pool\"
to read one from) is dropped."
  (and (cl-ppcre:scan *day-evidence-regex* text) t))

(defun destination-in-text (destination text)
  "DESTINATION reduced to the words TEXT contains, in order, or NIL when none
is left.  The model sometimes completes a name from its examples (\"Saint
Marys church\" for a message that says \"Saint Marys\"); only what the
message says may reach the sheet."
  (when (stringp destination)
    (let ((kept (remove-if-not (lambda (word) (word-in-text-p word text))
                               (cl-ppcre:split "\\s+" destination))))
      (and kept (format nil "~{~a~^ ~}" kept)))))

(defun vehicle-in-text-p (vehicle text)
  "True when TEXT names VEHICLE (a keyword from *VEHICLE-WORDS*) as a word."
  (some (lambda (word)
          (cl-ppcre:scan (format nil "(?i)(?<![a-z])~a(?![a-z])" word) text))
        (cdr (assoc vehicle *vehicle-words*))))

(defun constrain-intent (intent text)
  "INTENT with everything the model cannot have read in TEXT taken out, so
that only what the message says can reach the sheet:

  - a vehicle the message does not name is dropped (a small model reads
    \"the van\" as the DGH), and the ordinary DGH > LGH > Chevy priority applies;
  - a departure or return time whose hour the message never mentions is
    dropped (the model copied \"8:30 am\" from its examples for \"zz and qq
    wanna hit the test lab tmrw\"), so BOOK asks for the times instead of
    inventing a trip;
  - the day is dropped unless the message names one somehow
    (DAY-IN-TEXT-P), so the request comes back as incomplete;
  - the destination keeps only the words the message contains
    (DESTINATION-IN-TEXT).

A \"view\" that, after this, still names a destination, passengers or a time
is a booking phrased as a question (\"is the LGH free tomorrow 2-4 for BW to
see the dentist?\"), so its intent becomes :BOOK; VALIDATE-BOOK-INTENT then
holds it to the same rules as any other.  Passengers are left alone: one not
in TEXT makes VALIDATE-BOOK-INTENT reject the whole request."
  (when intent
    (let ((intent (copy-list intent)))
      (when (and (getf intent :vehicle)
                 (not (vehicle-in-text-p (getf intent :vehicle) text)))
        (setf (getf intent :vehicle) nil))
      (dolist (slot '(:dep-time :ret-time))
        (when (and (getf intent slot)
                   (not (time-in-text-p (getf intent slot) text)))
          (setf (getf intent slot) nil)))
      (when (and (getf intent :day) (not (day-in-text-p text)))
        (setf (getf intent :day) nil))
      (setf (getf intent :destination)
            (destination-in-text (getf intent :destination) text))
      (when (and (eq (getf intent :intent) :view)
                 (or (getf intent :destination) (getf intent :passengers)
                     (getf intent :dep-time) (getf intent :ret-time)))
        (setf (getf intent :intent) :book))
      intent)))

(defun llm-normalize (text &key known-destinations today)
  "Ask the model what TEXT means.  Returns the intent plist (see
INTENT-PLIST), or NIL when the fallback is disabled, the server is
unreachable or too slow, or the reply is unusable; nothing signals.  One
line per call goes to *ERROR-OUTPUT* (the journal in production) with the
elapsed time, the message and the raw reply, for review."
  (when (llm-enabled-p)
    (let ((text (truncate-input text))
          (start (get-internal-real-time)))
      (flet ((elapsed ()
               (/ (- (get-internal-real-time) start)
                  (float internal-time-units-per-second))))
        (handler-case
            (let ((body (dex:request *llm-url*
                                     :method :post
                                     :content (llm-request-body
                                               text
                                               :known-destinations known-destinations
                                               :today today)
                                     :headers '(("Content-Type" . "application/json"))
                                     :connect-timeout *llm-connect-timeout*
                                     :read-timeout *llm-timeout*)))
              (multiple-value-bind (plist raw) (parse-llm-response body)
                (format *error-output* "~&booking-llm: ~,1fs ~s -> ~a~%"
                        (elapsed) text (or raw "no usable reply"))
                (and plist (constrain-intent plist text))))
          (error (c)
            (format *error-output* "~&booking-llm: ~,1fs ~s -> ~a: ~a~%"
                    (elapsed) text (type-of c) c)
            nil))))))

(defun warm-up-llm (&key (attempts 6) (interval 10))
  "Prime llama-server's prompt cache with one request in a background thread,
so the first real message after a restart does not pay for reading the whole
system prompt (about 1,500 tokens, some 10 s on the production host).  The
thread retries every INTERVAL seconds up to ATTEMPTS times while the server
is still loading its model.  Returns the thread, or NIL when no server is
configured."
  (when (llm-enabled-p)
    (bordeaux-threads:make-thread
     (lambda ()
       (loop repeat attempts
             until (ignore-errors (llm-normalize "warm-up: what's on the van today?"))
             do (sleep interval)))
     :name "booking-llm-warm-up"
     ;; a new thread sees global values only; hand it the caller's settings
     :initial-bindings (list (cons '*llm-url* *llm-url*)
                             (cons '*llm-timeout* *llm-timeout*)
                             (cons '*llm-connect-timeout* *llm-connect-timeout*)
                             (cons '*error-output* *error-output*)))))

;;; ---------------------------------------------------------------------------
;;; From the model's JSON to a command the bot already understands
;;; ---------------------------------------------------------------------------

(defparameter *vehicle-phrases*
  '((:dgh . "DGH") (:lgh . "LGH") (:chevy . "Chevy") (:personal . "personal car"))
  "How each vehicle is written in the canonical sentence (\"in the DGH\").")

(defparameter *bare-days* '("today" "tomorrow" "tonight")
  "Days written without \"on\" in the canonical sentence.")

(defparameter *not-initials* '("AM" "PM" "OK" "FYI" "TBD" "ETA" "PS" "EOD" "DGH" "LGH")
  "Uppercase words the booking ruler never reads as initials (mirrors
BOOKING-NLP::MAKE-BOOKING-RULER).")

(defun sanitize-field (value &key (max 60))
  "VALUE as one line of plain text: control characters become spaces,
backticks (Slack formatting) are dropped, whitespace is collapsed and
trimmed, and the result is cut to MAX characters.  NIL when VALUE is not a
string or nothing is left."
  (when (stringp value)
    (let* ((spaced (map 'string
                        (lambda (ch)
                          (if (or (< (char-code ch) 32) (= (char-code ch) 127))
                              #\Space
                              ch))
                        value))
           (collapsed (string-trim " " (cl-ppcre:regex-replace-all
                                        "\\s+" (remove #\` spaced) " "))))
      (when (plusp (length collapsed))
        (if (> (length collapsed) max)
            (string-right-trim " " (subseq collapsed 0 max))
            collapsed)))))

(defun join-with-and (strings)
  "\"BW\", \"BW and MM\", \"MM, DM and CG\"."
  (case (length strings)
    (0 "")
    (1 (first strings))
    (2 (format nil "~a and ~a" (first strings) (second strings)))
    (t (format nil "~{~a~^, ~} and ~a" (butlast strings) (car (last strings))))))

(defun day-clause (day)
  "DAY as the canonical sentence writes it: \"tomorrow\" stays bare, anything
else gets \"on\" (\"on Thursday\", \"on Sept 12\", \"on 2026-09-12\")."
  (when day
    (if (some (lambda (bare) (uiop:string-prefix-p bare (string-downcase day)))
              *bare-days*)
        day
        (format nil "on ~a" day))))

(defun render-book-request (intent)
  "Write the book INTENT plist as the one sentence shape PARSE-BOOKING is
known to read:

  BW and MM to the dentist tomorrow leaving at 8:30 am and back at 11:00 am in the DGH

Pieces the model left null are left out, so an incomplete request renders to
an incomplete sentence and gets the ordinary BOOKING-INCOMPLETE report."
  (let* ((passengers (getf intent :passengers))
         (destination (sanitize-field (getf intent :destination)))
         (day (sanitize-field (getf intent :day) :max 40))
         (dep (sanitize-field (getf intent :dep-time) :max 12))
         (ret (sanitize-field (getf intent :ret-time) :max 12))
         (vehicle (cdr (assoc (getf intent :vehicle) *vehicle-phrases*)))
         (parts (remove nil
                        (list (and passengers (join-with-and passengers))
                              (and destination (format nil "to the ~a" destination))
                              (day-clause day)
                              (and dep (format nil "leaving at ~a" dep))
                              (and ret (format nil "~:[~;and ~]back at ~a" dep ret))
                              (and vehicle (format nil "in the ~a" vehicle))))))
    (format nil "~{~a~^ ~}" parts)))

(defun render-view-range (intent)
  "The view INTENT's range as a `/view` argument, or NIL."
  (sanitize-field (getf intent :range)))

(defun initial-in-text-p (initial text)
  "True when INITIAL is well-formed, is not one of the words the ruler
excludes, and occurs in TEXT as a whole word in any case.  The model may
uppercase \"bw\" for the sheet, but it may not add anyone."
  (and (stringp initial)
       (cl-ppcre:scan +initials-pattern+ initial)
       (not (member initial *not-initials* :test #'string=))
       (cl-ppcre:scan (format nil "(?i)(?<![a-z])~a(?![a-z])" initial) text)
       t))

(defun phrase-key (text)
  "Letters and digits of TEXT, lowercased: the gazetteer's notion of equality
\(\"Saint Mary's Church\" and \"saint marys church\" give the same key)."
  (remove-if-not #'alphanumericp (string-downcase text)))

(defun slot-agrees-p (wanted parsed)
  "True when the model gave nothing for the slot, or the parser read the same
value back."
  (or (null wanted)
      (and parsed (string-equal wanted parsed))))

(defun validate-book-intent (intent text &key known-destinations)
  "Render the book INTENT and check it against TEXT and the rule parser.
Returns the canonical sentence, or NIL when there are no passengers, an
initial does not occur in TEXT, the destination or day is blank, or
PARSE-BOOKING (with KNOWN-DESTINATIONS as its gazetteer) reads the sentence
differently from what the model meant: any of destination, day or passengers
missing, other passengers, another destination, a different time or
vehicle."
  (let ((passengers (getf intent :passengers))
        (destination (sanitize-field (getf intent :destination)))
        (day (sanitize-field (getf intent :day) :max 40)))
    (when (and passengers destination day
               (every (lambda (p) (initial-in-text-p p text)) passengers))
      (let* ((rendered (render-book-request intent))
             (req (bn:parse-booking rendered :known-destinations known-destinations))
             (parsed-destination (bn:booking-request-destination req)))
        (when (and (null (intersection '(:destination :day :passengers)
                                       (bn:booking-request-missing req)))
                   (equal passengers (bn:booking-request-passengers req))
                   parsed-destination
                   (search (phrase-key parsed-destination) (phrase-key destination))
                   (slot-agrees-p (getf intent :dep-time) (bn:booking-request-dep-time req))
                   (slot-agrees-p (getf intent :ret-time) (bn:booking-request-ret-time req))
                   (or (null (getf intent :vehicle))
                       (eq (getf intent :vehicle) (bn:booking-request-vehicle req))))
          rendered)))))

(defun render-intent (intent text &key known-destinations (allow-view t))
  "Turn the model's INTENT for TEXT into a slash command: (values \"/book\"
sentence) when VALIDATE-BOOK-INTENT accepts it, (values \"/view\" range) when
ALLOW-VIEW and the range is one VIEW-SCHEDULE can read, otherwise NIL."
  (case (getf intent :intent)
    (:book
     (let ((sentence (validate-book-intent intent text
                                           :known-destinations known-destinations)))
       (and sentence (values "/book" sentence))))
    (:view
     (let ((range (and allow-view (render-view-range intent))))
       (and range (date-range-phrase-p range) (values "/view" range))))
    (t nil)))
