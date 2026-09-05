;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/booking-llm-tests
  (:use #:common-lisp #:parachute)
  (:local-nicknames (#:llm #:project-isidore/booking-llm)
                    (#:bn #:project-isidore/booking-nlp)
                    (#:bnt #:project-isidore/tests/booking-nlp-tests))
  (:import-from #:alexandria #:plist-hash-table)
  (:import-from #:com.inuoe.jzon)
  (:documentation
   "Parachute tests for project-isidore/booking-llm.

Everything here runs without a model server: the schema and prompt are
inspected as data, replies are canned JSON, and the rendering of a reply into
the canonical booking sentence is checked by feeding that sentence back to
the rule parser and asserting the corpus expectations against it."))

(in-package #:project-isidore/tests/booking-llm-tests)

(define-test booking-llm-suite
  :description "Local-LLM fallback: schema, prompt, reply parsing, rendering.")

;;; --- Configuration ---

(define-test llm-config
  :parent booking-llm-suite
  (let ((saved (uiop:getenv "VAN_LLM_URL")))
    (unwind-protect
         (progn
           (setf (uiop:getenv "VAN_LLM_URL") "  ")
           (false (llm:load-llm-config))
           (false (llm:llm-enabled-p))
           (setf (uiop:getenv "VAN_LLM_URL") " http://127.0.0.1:8081/v1/chat/completions ")
           (is string= "http://127.0.0.1:8081/v1/chat/completions" (llm:load-llm-config))
           (true (llm:llm-enabled-p)))
      (setf (uiop:getenv "VAN_LLM_URL") (or saved ""))
      (llm:load-llm-config))))

;;; --- Schema and prompt ---

(define-test intent-schema-shape
  :parent booking-llm-suite
  (let* ((json (com.inuoe.jzon:stringify (llm:intent-schema)))
         (schema (com.inuoe.jzon:parse json))
         (props (gethash "properties" schema))
         (required (coerce (gethash "required" schema) 'list)))
    (is equal "object" (gethash "type" schema))
    ;; false, not merely absent
    (true (search "\"additionalProperties\":false" json))
    ;; every property is required, so the grammar always emits all nine keys,
    ;; intent first
    (is = 9 (hash-table-count props))
    (is = (hash-table-count props) (length required))
    (dolist (key required)
      (true (nth-value 1 (gethash key props)) "~a is a property" key))
    (is equal "intent" (first required))
    ;; llama.cpp's converter needs anchored patterns
    (dolist (key '("dep_time" "ret_time"))
      (let ((pattern (gethash "pattern" (gethash key props))))
        (true (and (char= #\^ (char pattern 0))
                   (char= #\$ (char pattern (1- (length pattern)))))
              "~a pattern anchored" key)))
    (is equal "^[A-Z]{2,3}$"
        (gethash "pattern" (gethash "items" (gethash "passengers" props))))
    (true (search "\"enum\":[\"dgh\",\"lgh\",\"chevy\",\"personal\",null]" json))
    (true (search "\"enum\":[\"book\",\"view\",\"other\"]" json))))

(define-test system-prompt
  :parent booking-llm-suite
  (let ((prompt (llm:system-prompt
                 :known-destinations (list "SHMS" (format nil "Pool`~%x") nil))))
    (true (search "Today is " prompt))
    (true (search (format nil "Today is ~a" (bn:format-booking-day (chronicity:parse "today")))
                  prompt))
    ;; sheet destinations are listed, sanitised
    (true (search "SHMS, Pool x" prompt))
    (false (find #\` prompt))
    ;; the examples are there, as message / JSON pairs
    (true (search "Message: what's on the van this week?" prompt))
    (true (search "\"range\":\"this week\"" prompt))
    (true (search "\"passengers\":[\"BW\",\"MM\"]" prompt)))
  ;; without sheet destinations the prompt ends with the date
  (let ((prompt (llm:system-prompt)))
    (false (search "Destinations already" prompt))
    (true (search "Today is " prompt))))

(define-test request-body
  :parent booking-llm-suite
  (let* ((body (com.inuoe.jzon:parse
                (llm:llm-request-body "bw to the pool tmrw"
                                      :known-destinations '("pool"))))
         (messages (gethash "messages" body)))
    (is equal "json_schema" (gethash "type" (gethash "response_format" body)))
    (is equal "object" (gethash "type" (gethash "schema"
                                                (gethash "json_schema"
                                                         (gethash "response_format" body)))))
    (is eq t (gethash "cache_prompt" body))
    (is = 0 (gethash "temperature" body))
    ;; thinking is off: llama.cpp skips the grammar while a model thinks
    (multiple-value-bind (value present)
        (gethash "enable_thinking" (gethash "chat_template_kwargs" body))
      (true present)
      (false value))
    (is = 2 (length messages))
    (is equal "system" (gethash "role" (aref messages 0)))
    (true (search "pool" (gethash "content" (aref messages 0))))
    (is equal "user" (gethash "role" (aref messages 1)))
    (is equal "bw to the pool tmrw" (gethash "content" (aref messages 1)))))

;;; --- Reply parsing ---

(defun completion (content &key (finish "stop"))
  "A chat-completions reply body whose first choice says CONTENT."
  (com.inuoe.jzon:stringify
   (plist-hash-table
    (list "choices"
          (vector (plist-hash-table
                   (list "finish_reason" finish
                         "message" (plist-hash-table
                                    (list "role" "assistant" "content" content)
                                    :test #'equal))
                   :test #'equal)))
    :test #'equal)))

(define-test parse-llm-response
  :parent booking-llm-suite
  (multiple-value-bind (plist raw)
      (llm:parse-llm-response
       (completion "{\"intent\":\"book\",\"destination\":\" dentist \",\"day\":\"tomorrow\",\"passengers\":[\"bw\",\"MM\",\"MM\"],\"dep_time\":\"8:30 am\",\"ret_time\":null,\"vehicle\":\"dgh\",\"range\":null,\"unclear\":null}"))
    (is eq :book (getf plist :intent))
    (is equal "dentist" (getf plist :destination))
    (is equal "tomorrow" (getf plist :day))
    ;; initials are uppercased and de-duplicated
    (is equal '("BW" "MM") (getf plist :passengers))
    (is equal "8:30 am" (getf plist :dep-time))
    (false (getf plist :ret-time))
    (is eq :dgh (getf plist :vehicle))
    (false (getf plist :range))
    (false (getf plist :unclear))
    (true (search "\"intent\":\"book\"" raw)))
  (let ((plist (llm:parse-llm-response
                (completion "{\"intent\":\"view\",\"destination\":null,\"day\":null,\"passengers\":[],\"dep_time\":null,\"ret_time\":null,\"vehicle\":null,\"range\":\"this week\",\"unclear\":null}"))))
    (is eq :view (getf plist :intent))
    (false (getf plist :passengers))
    (is equal "this week" (getf plist :range)))
  ;; a reply cut off by max_tokens is not trusted
  (false (llm:parse-llm-response (completion "{\"intent\":\"book\"" :finish "length")))
  ;; nor one that is not an object, or has no choice at all
  (false (llm:parse-llm-response (completion "[1,2]")))
  (false (llm:parse-llm-response (completion "not json")))
  (false (llm:parse-llm-response "{}"))
  ;; words outside the schema's vocabulary never become keywords
  (let ((plist (llm:parse-llm-response (completion "{\"intent\":\"delete\",\"vehicle\":\"tank\"}"))))
    (false (getf plist :intent))
    (false (getf plist :vehicle))))

;;; --- Rendering ---

(define-test render-book-template
  :parent booking-llm-suite
  (is string= "BW and MM to the dentist tomorrow leaving at 8:30 am and back at 11:00 am in the DGH"
      (llm:render-book-request '(:passengers ("BW" "MM") :destination "dentist" :day "tomorrow"
                                 :dep-time "8:30 am" :ret-time "11:00 am" :vehicle :dgh)))
  (is string= "MM, DM and CG to the Saint Marys church on Friday leaving at 9:00 am and back at 12:00 pm"
      (llm:render-book-request '(:passengers ("MM" "DM" "CG") :destination "Saint Marys church"
                                 :day "Friday" :dep-time "9:00 am" :ret-time "12:00 pm")))
  (is string= "GG to the SHMS on Thursday"
      (llm:render-book-request '(:passengers ("GG") :destination "SHMS" :day "Thursday")))
  (is string= "BW to the pool tomorrow back at 10:00 am"
      (llm:render-book-request '(:passengers ("BW") :destination "pool" :day "tomorrow"
                                 :ret-time "10:00 am")))
  (is string= "BW to the pool tomorrow morning leaving at 8:00 am"
      (llm:render-book-request '(:passengers ("BW") :destination "pool" :day "tomorrow morning"
                                 :dep-time "8:00 am")))
  (is string= "DM to the store on Saturday leaving at 10:00 am and back at 11:00 am in the personal car"
      (llm:render-book-request '(:passengers ("DM") :destination "store" :day "Saturday"
                                 :dep-time "10:00 am" :ret-time "11:00 am" :vehicle :personal)))
  (is string= "BW to the dentist on 2026-09-12 leaving at 8:30 am"
      (llm:render-book-request '(:passengers ("BW") :destination "dentist" :day "2026-09-12"
                                 :dep-time "8:30 am")))
  ;; pieces the model left out are simply absent
  (is string= "MM" (llm:render-book-request '(:passengers ("MM"))))
  (is string= "" (llm:render-book-request '()))
  ;; backticks, newlines and control characters never reach Slack or the sheet
  (is string= "BW to the dentist x tomorrow"
      (llm:render-book-request (list :passengers '("BW")
                                     :destination (format nil "dentist`~%x")
                                     :day "tomorrow")))
  (is string= "this week" (llm:render-view-range (list :range (format nil " this~%week `"))))
  (false (llm:render-view-range '(:range nil))))

(define-test sanitize-field
  :parent booking-llm-suite
  (is string= "a b c" (llm:sanitize-field (format nil " a~%~%b~C  `c` " (code-char 7))))
  (is string= "abcde" (llm:sanitize-field "abcdefgh" :max 5))
  (is string= "abc" (llm:sanitize-field "abc d" :max 4))
  (false (llm:sanitize-field "   "))
  (false (llm:sanitize-field nil))
  (false (llm:sanitize-field 42)))

(defparameter *round-trip-cases*
  `(((:passengers ("BW" "MM") :destination "dentist" :day "tomorrow"
      :dep-time "8:30 am" :ret-time "11:00 am" :vehicle :dgh)
     :destination "dentist" :day-like "tomorrow" :passengers ("BW" "MM")
     :dep-time "8:30 am" :ret-time "11:00 am" :vehicle :dgh :missing nil)
    ((:passengers ("GG") :destination "SHMS" :day "Thursday")
     :destination "SHMS" :day-contains "thursday" :passengers ("GG")
     :missing (:dep-time :ret-time))
    ((:passengers ("MM" "DM" "CG") :destination "Saint Marys church" :day "Friday"
      :dep-time "9:00 am" :ret-time "12:00 pm")
     :destination "Saint Marys church" :day-contains "friday" :passengers ("MM" "DM" "CG")
     :dep-time "9:00 am" :ret-time "12:00 pm" :vehicle nil :missing nil)
    ((:passengers ("CG" "BW") :destination "swimming" :day "Tuesday"
      :dep-time "7:00 pm" :ret-time "9:00 pm")
     :destination "swimming" :day-contains "tuesday" :passengers ("CG" "BW")
     :dep-time "7:00 pm" :ret-time "9:00 pm" :missing nil)
    ((:passengers ("CG") :destination "swimming" :day "next Tuesday"
      :dep-time "7:00 pm" :ret-time "9:00 pm")
     :destination "swimming" :day-contains "tuesday" :passengers ("CG")
     :dep-time "7:00 pm" :ret-time "9:00 pm" :missing nil)
    ((:passengers ("MM" "DM") :destination "pool" :day "April 22nd"
      :dep-time "9:00 am" :ret-time "12:00 pm")
     :destination "pool" :day-md "April 22" :passengers ("MM" "DM")
     :dep-time "9:00 am" :ret-time "12:00 pm" :missing nil)
    ((:passengers ("BW") :destination "doctor" :day "4/22"
      :dep-time "8:30 am" :ret-time "10:00 am")
     :destination "doctor" :day-md "April 22" :passengers ("BW")
     :dep-time "8:30 am" :ret-time "10:00 am" :missing nil)
    ((:passengers ("DM") :destination "store" :day "Saturday"
      :dep-time "10:00 am" :ret-time "11:00 am" :vehicle :personal)
     :destination "store" :day-contains "saturday" :passengers ("DM") :vehicle :personal
     :dep-time "10:00 am" :ret-time "11:00 am" :missing nil)
    ((:passengers ("ZZ") :destination "test lab" :day "Sat Sep 12"
      :dep-time "9:00 am" :ret-time "10:00 am")
     :destination "test lab" :day-md "September 12" :passengers ("ZZ")
     :dep-time "9:00 am" :ret-time "10:00 am" :missing nil)
    ((:passengers ("BW") :destination "dentist" :day ,(format nil "~d-09-12" (bnt::current-year))
      :dep-time "8:30 am")
     :destination "dentist" :day-md "September 12" :passengers ("BW")
     :dep-time "8:30 am" :missing (:ret-time))
    ((:passengers ("CG") :destination "airport" :day "tomorrow morning"
      :dep-time "6:00 am" :ret-time "12:00 am")
     :destination "airport" :day-like "tomorrow" :passengers ("CG")
     :dep-time "6:00 am" :ret-time "12:00 am" :missing nil)
    ((:passengers ("BW") :destination "dentist" :day "tomorrow" :vehicle :lgh
      :dep-time "2:00 pm" :ret-time "4:00 pm")
     :destination "dentist" :day-like "tomorrow" :passengers ("BW") :vehicle :lgh
     :dep-time "2:00 pm" :ret-time "4:00 pm" :missing nil)
    ((:passengers ("GG") :destination "haircut" :day "tomorrow" :vehicle :chevy
      :dep-time "12:30 pm" :ret-time "2:30 pm")
     :destination "haircut" :day-like "tomorrow" :passengers ("GG") :vehicle :chevy
     :dep-time "12:30 pm" :ret-time "2:30 pm" :missing nil))
  "(INTENT-PLIST . EXPECTED): what the rule parser must read back from the
rendered sentence, in the notation of BOOKING-NLP-TESTS::CHECK-REQUEST.")

(define-test render-round-trip
  :parent booking-llm-suite
  (dolist (case *round-trip-cases*)
    (destructuring-bind (intent &rest expected) case
      (let ((sentence (llm:render-book-request intent)))
        (bnt::check-request sentence (bn:parse-booking sentence) expected)))))

;;; --- Validation ---

(define-test initial-in-text
  :parent booking-llm-suite
  (true (llm:initial-in-text-p "BW" "can bw and MM go"))
  (true (llm:initial-in-text-p "CG" "for cg&bw"))
  (false (llm:initial-in-text-p "BW" "BWX and MM"))
  (false (llm:initial-in-text-p "BW" "nobody"))
  ;; the ruler's non-initial words, and anything not 2-3 letters
  (false (llm:initial-in-text-p "PM" "at 7 PM"))
  (false (llm:initial-in-text-p "DGH" "take the DGH"))
  (false (llm:initial-in-text-p "B" "B"))
  (false (llm:initial-in-text-p "bw" "bw"))
  (false (llm:initial-in-text-p nil "bw")))

(define-test validate-book-intent
  :parent booking-llm-suite
  (let ((intent '(:intent :book :passengers ("BW" "MM") :destination "dentist" :day "tomorrow"
                  :dep-time "8:30 am" :ret-time "11:00 am")))
    ;; lowercase initials in the message are fine: the rule parser cannot
    ;; read them, which is why the model was asked
    (is string= "BW and MM to the dentist tomorrow leaving at 8:30 am and back at 11:00 am"
        (llm:validate-book-intent intent "bw and mm need the dentist tmrw, 8:30 till 11"))
    ;; but every initial must be in the message
    (false (llm:validate-book-intent intent "BW needs the dentist tomorrow 8:30-11")))
  ;; the ruler's non-initial words are never passengers
  (false (llm:validate-book-intent '(:passengers ("PM") :destination "dentist" :day "tomorrow")
                                   "PM to the dentist tomorrow"))
  ;; destination, day and passengers are all required
  (false (llm:validate-book-intent '(:passengers ("BW") :day "tomorrow") "BW tomorrow"))
  (false (llm:validate-book-intent '(:passengers ("BW") :destination "dentist") "BW dentist"))
  (false (llm:validate-book-intent '(:destination "dentist" :day "tomorrow") "dentist tomorrow"))
  ;; a destination the parser cannot recover from the sentence (a vehicle
  ;; noun is never a destination) is rejected
  (false (llm:validate-book-intent '(:passengers ("BW") :destination "van" :day "tomorrow")
                                   "BW van tomorrow"))
  ;; the sheet's own spelling wins through the gazetteer, apostrophes and all
  (true (llm:validate-book-intent '(:passengers ("MM") :destination "saint marys church" :day "Friday")
                                  "mm to saint marys church friday"
                                  :known-destinations '("Saint Mary's Church")))
  (is string= "Saint Mary's Church"
      (bn:booking-request-destination
       (bn:parse-booking
        (llm:validate-book-intent '(:passengers ("MM") :destination "saint marys church" :day "Friday")
                                  "mm to saint marys church friday"
                                  :known-destinations '("Saint Mary's Church"))
        :known-destinations '("Saint Mary's Church")))))

(define-test constrain-intent
  :parent booking-llm-suite
  (true (llm:vehicle-in-text-p :dgh "take the DGH"))
  (true (llm:vehicle-in-text-p :chevy "the chevrolet please"))
  (true (llm:vehicle-in-text-p :personal "in the personal car"))
  (false (llm:vehicle-in-text-p :dgh "take the van"))
  (false (llm:vehicle-in-text-p :lgh "the DGH"))
  (false (llm:vehicle-in-text-p :dgh "DGHX"))
  ;; a vehicle the message does not name is dropped: "the van" is not the DGH
  (let ((intent '(:intent :book :passengers ("MM") :destination "pool" :day "tomorrow"
                  :vehicle :dgh)))
    (false (getf (llm:constrain-intent intent "book the van for the pool tomorrow for MM")
                 :vehicle))
    (is eq :dgh (getf (llm:constrain-intent intent "book the dgh for the pool tomorrow for MM")
                      :vehicle))
    ;; the rest of the reply is untouched, and so is the original
    (is equal '("MM") (getf (llm:constrain-intent intent "the van for MM") :passengers))
    (is eq :dgh (getf intent :vehicle)))
  ;; a "view" that names passengers, a destination or times is a booking
  ;; phrased as a question
  (is eq :book (getf (llm:constrain-intent
                      '(:intent :view :passengers ("BW") :destination "dentist" :range "tomorrow")
                      "is the LGH free tomorrow for BW to see the dentist?")
                     :intent))
  (is eq :book (getf (llm:constrain-intent '(:intent :view :dep-time "8:00 am") "x") :intent))
  (is eq :book (getf (llm:constrain-intent '(:intent :view :ret-time "8:00 am") "x") :intent))
  ;; a real question stays one, even with a day or vehicle in the reply
  (is eq :view (getf (llm:constrain-intent '(:intent :view :range "this week" :day "Saturday")
                                           "what's on this week?")
                     :intent))
  (is eq :view (getf (llm:constrain-intent '(:intent :view :range "tomorrow" :vehicle :dgh)
                                           "who has the DGH tomorrow?")
                     :intent))
  (is eq :other (getf (llm:constrain-intent '(:intent :other) "thanks") :intent))
  (false (llm:constrain-intent nil "x")))

(define-test render-intent
  :parent booking-llm-suite
  (multiple-value-bind (command args)
      (llm:render-intent '(:intent :view :range "this week") "what's on the van this week?")
    (is string= "/view" command)
    (is string= "this week" args))
  (multiple-value-bind (command args)
      (llm:render-intent '(:intent :view :range "Sept 19 to Sept 22") "schedule Sept 19-22?")
    (is string= "/view" command)
    (is string= "Sept 19 to Sept 22" args))
  ;; an explicit /book never turns into a view
  (false (llm:render-intent '(:intent :view :range "this week") "x" :allow-view nil))
  ;; a range the schedule cannot read is dropped, as is a missing one
  (false (llm:render-intent '(:intent :view :range "xyzzy") "x"))
  (false (llm:render-intent '(:intent :view :range nil) "x"))
  ;; other, and no intent at all
  (false (llm:render-intent '(:intent :other :unclear "Not a request.") "thanks"))
  (false (llm:render-intent '(:intent nil) "x"))
  (false (llm:render-intent nil "x"))
  (multiple-value-bind (command args)
      (llm:render-intent '(:intent :book :passengers ("BW") :destination "pool" :day "tomorrow")
                         "bw pool tmrw")
    (is string= "/book" command)
    (is string= "BW to the pool tomorrow" args))
  (false (llm:render-intent '(:intent :book :passengers ("BW") :destination "pool" :day "tomorrow")
                            "mm pool tmrw")))

;;; --- Talking to the server ---

(define-test llm-disabled
  :parent booking-llm-suite
  (let ((llm:*llm-url* nil))
    (false (llm:llm-enabled-p))
    (false (llm:llm-normalize "BW to the pool tomorrow"))))

(define-test llm-unreachable
  :parent booking-llm-suite
  ;; nothing listens on port 1: the call returns NIL quickly and nothing
  ;; signals
  (let ((llm:*llm-url* "http://127.0.0.1:1/v1/chat/completions")
        (llm:*llm-timeout* 2)
        (*error-output* (make-broadcast-stream)))
    (true (llm:llm-enabled-p))
    (false (llm:llm-normalize "BW to the pool tomorrow"))))

(define-test warm-up
  :parent booking-llm-suite
  ;; nothing to warm up without a server
  (let ((llm:*llm-url* nil))
    (false (llm:warm-up-llm)))
  ;; with one, a background thread tries, gives up after ATTEMPTS, and
  ;; nothing escapes it
  (let ((llm:*llm-url* "http://127.0.0.1:1/v1/chat/completions")
        (llm:*llm-timeout* 2)
        (*error-output* (make-broadcast-stream)))
    (let ((thread (llm:warm-up-llm :attempts 2 :interval 0)))
      (true (bordeaux-threads:threadp thread))
      (bordeaux-threads:join-thread thread)
      (false (bordeaux-threads:thread-alive-p thread)))))
