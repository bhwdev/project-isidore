;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/slack-bot/booking-llm
  (:use #:common-lisp)
  (:import-from #:rove #:deftest #:ok #:ng)
  (:local-nicknames (#:llm #:project-isidore/slack-bot/booking-llm)
                    (#:bn #:project-isidore/slack-bot/booking-nlp)
                    (#:bnt #:project-isidore/tests/slack-bot/booking-nlp))
  (:import-from #:alexandria #:plist-hash-table)
  (:import-from #:com.inuoe.jzon)
  (:documentation
   "Tests for project-isidore/slack-bot/booking-llm.

Everything here runs without a model server: the schema and prompt are
inspected as data, replies are canned JSON, and the rendering of a reply into
the canonical booking sentence is checked by feeding that sentence back to
the rule parser and asserting the corpus expectations against it."))

(in-package #:project-isidore/tests/slack-bot/booking-llm)

;;; --- Configuration ---

(deftest llm-config
  (let ((saved (uiop:getenv "VAN_LLM_URL")))
    (unwind-protect
         (progn
           (setf (uiop:getenv "VAN_LLM_URL") "  ")
           (ng (llm:load-llm-config))
           (ng (llm:llm-enabled-p))
           (setf (uiop:getenv "VAN_LLM_URL") " http://127.0.0.1:8081/v1/chat/completions ")
           (ok (string= "http://127.0.0.1:8081/v1/chat/completions" (llm:load-llm-config)))
           (ok (llm:llm-enabled-p)))
      (setf (uiop:getenv "VAN_LLM_URL") (or saved ""))
      (llm:load-llm-config))))

;;; --- Schema and prompt ---

(deftest intent-schema-shape
  (let* ((json (com.inuoe.jzon:stringify (llm:intent-schema)))
         (schema (com.inuoe.jzon:parse json))
         (props (gethash "properties" schema))
         (required (coerce (gethash "required" schema) 'list)))
    (ok (equal "object" (gethash "type" schema)))
    ;; false, not merely absent
    (ok (search "\"additionalProperties\":false" json))
    ;; every property is required, so the grammar always emits all nine keys,
    ;; intent first
    (ok (= 9 (hash-table-count props)))
    (ok (= (hash-table-count props) (length required)))
    (dolist (key required)
      (ok (nth-value 1 (gethash key props)) (format nil "~a is a property" key)))
    (ok (equal "intent" (first required)))
    ;; llama.cpp's converter needs anchored patterns
    (dolist (key '("dep_time" "ret_time"))
      (let ((pattern (gethash "pattern" (gethash key props))))
        (ok (and (char= #\^ (char pattern 0))
                 (char= #\$ (char pattern (1- (length pattern))))) (format nil "~a pattern anchored" key))))
    (ok (equal "^[A-Z]{2,3}$"
               (gethash "pattern" (gethash "items" (gethash "passengers" props)))))
    (ok (search "\"enum\":[\"dgh\",\"lgh\",\"chevy\",\"personal\",null]" json))
    (ok (search "\"enum\":[\"book\",\"view\",\"other\"]" json))))

(deftest system-prompt
  (let ((prompt (llm:system-prompt
                 :known-destinations (list "SHMS" (format nil "Pool`~%x") nil))))
    (ok (search "Today is " prompt))
    (ok (search (format nil "Today is ~a" (bn:format-booking-day (chronicity:parse "today")))
                prompt))
    ;; sheet destinations are listed, sanitised
    (ok (search "SHMS, Pool x" prompt))
    (ng (find #\` prompt))
    ;; the examples are there, as message / JSON pairs
    (ok (search "Message: what's on the van this week?" prompt))
    ;; a question about a trip's timing is shown as a view of that day
    (ok (search "Message: when is my ride home this coming Tuesday?" prompt))
    (ok (search "\"range\":\"Tuesday\"" prompt))
    (ok (search "when a van or ride leaves or gets home" prompt))
    (ok (search "\"range\":\"this week\"" prompt))
    (ok (search "\"passengers\":[\"BW\",\"MM\"]" prompt)))
  ;; without sheet destinations the prompt ends with the date
  (let ((prompt (llm:system-prompt)))
    (ng (search "Destinations already" prompt))
    (ok (search "Today is " prompt))))

(deftest request-body
  (let* ((body (com.inuoe.jzon:parse
                (llm:llm-request-body "bw to the pool tmrw"
                                      :known-destinations '("pool"))))
         (messages (gethash "messages" body)))
    (ok (equal "json_schema" (gethash "type" (gethash "response_format" body))))
    (ok (equal "object" (gethash "type" (gethash "schema"
                                                 (gethash "json_schema"
                                                          (gethash "response_format" body))))))
    (ok (eq t (gethash "cache_prompt" body)))
    (ok (= 0 (gethash "temperature" body)))
    ;; thinking is off: llama.cpp skips the grammar while a model thinks
    (multiple-value-bind (value present)
        (gethash "enable_thinking" (gethash "chat_template_kwargs" body))
      (ok present)
      (ng value))
    (ok (= 2 (length messages)))
    (ok (equal "system" (gethash "role" (aref messages 0))))
    (ok (search "pool" (gethash "content" (aref messages 0))))
    (ok (equal "user" (gethash "role" (aref messages 1))))
    (ok (equal "bw to the pool tmrw" (gethash "content" (aref messages 1))))))

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

(deftest parse-llm-response
  (multiple-value-bind (plist raw)
      (llm:parse-llm-response
       (completion "{\"intent\":\"book\",\"destination\":\" dentist \",\"day\":\"tomorrow\",\"passengers\":[\"bw\",\"MM\",\"MM\"],\"dep_time\":\"8:30 am\",\"ret_time\":null,\"vehicle\":\"dgh\",\"range\":null,\"unclear\":null}"))
    (ok (eq :book (getf plist :intent)))
    (ok (equal "dentist" (getf plist :destination)))
    (ok (equal "tomorrow" (getf plist :day)))
    ;; initials are uppercased and de-duplicated
    (ok (equal '("BW" "MM") (getf plist :passengers)))
    (ok (equal "8:30 am" (getf plist :dep-time)))
    (ng (getf plist :ret-time))
    (ok (eq :dgh (getf plist :vehicle)))
    (ng (getf plist :range))
    (ng (getf plist :unclear))
    (ok (search "\"intent\":\"book\"" raw)))
  (let ((plist (llm:parse-llm-response
                (completion "{\"intent\":\"view\",\"destination\":null,\"day\":null,\"passengers\":[],\"dep_time\":null,\"ret_time\":null,\"vehicle\":null,\"range\":\"this week\",\"unclear\":null}"))))
    (ok (eq :view (getf plist :intent)))
    (ng (getf plist :passengers))
    (ok (equal "this week" (getf plist :range))))
  ;; a reply cut off by max_tokens is not trusted
  (ng (llm:parse-llm-response (completion "{\"intent\":\"book\"" :finish "length")))
  ;; nor one that is not an object, or has no choice at all
  (ng (llm:parse-llm-response (completion "[1,2]")))
  (ng (llm:parse-llm-response (completion "not json")))
  (ng (llm:parse-llm-response "{}"))
  ;; words outside the schema's vocabulary never become keywords
  (let ((plist (llm:parse-llm-response (completion "{\"intent\":\"delete\",\"vehicle\":\"tank\"}"))))
    (ng (getf plist :intent))
    (ng (getf plist :vehicle))))

;;; --- Rendering ---

(deftest render-book-template
  (ok (string= "BW and MM to the dentist tomorrow leaving at 8:30 am and back at 11:00 am in the DGH"
               (llm:render-book-request '(:passengers ("BW" "MM") :destination "dentist" :day "tomorrow"
                                          :dep-time "8:30 am" :ret-time "11:00 am" :vehicle :dgh))))
  (ok (string= "MM, DM and CG to the Saint Marys church on Friday leaving at 9:00 am and back at 12:00 pm"
               (llm:render-book-request '(:passengers ("MM" "DM" "CG") :destination "Saint Marys church"
                                          :day "Friday" :dep-time "9:00 am" :ret-time "12:00 pm"))))
  (ok (string= "GG to the SHMS on Thursday"
               (llm:render-book-request '(:passengers ("GG") :destination "SHMS" :day "Thursday"))))
  (ok (string= "BW to the pool tomorrow back at 10:00 am"
               (llm:render-book-request '(:passengers ("BW") :destination "pool" :day "tomorrow"
                                          :ret-time "10:00 am"))))
  (ok (string= "BW to the pool tomorrow morning leaving at 8:00 am"
               (llm:render-book-request '(:passengers ("BW") :destination "pool" :day "tomorrow morning"
                                          :dep-time "8:00 am"))))
  (ok (string= "DM to the store on Saturday leaving at 10:00 am and back at 11:00 am in the personal car"
               (llm:render-book-request '(:passengers ("DM") :destination "store" :day "Saturday"
                                          :dep-time "10:00 am" :ret-time "11:00 am" :vehicle :personal))))
  (ok (string= "BW to the dentist on 2026-09-12 leaving at 8:30 am"
               (llm:render-book-request '(:passengers ("BW") :destination "dentist" :day "2026-09-12"
                                          :dep-time "8:30 am"))))
  ;; pieces the model left out are simply absent
  (ok (string= "MM" (llm:render-book-request '(:passengers ("MM")))))
  (ok (string= "" (llm:render-book-request '())))
  ;; backticks, newlines and control characters never reach Slack or the sheet
  (ok (string= "BW to the dentist x tomorrow"
               (llm:render-book-request (list :passengers '("BW")
                                              :destination (format nil "dentist`~%x")
                                              :day "tomorrow"))))
  (ok (string= "this week" (llm:render-view-range (list :range (format nil " this~%week `")))))
  (ng (llm:render-view-range '(:range nil))))

(deftest sanitize-field
  (ok (string= "a b c" (llm:sanitize-field (format nil " a~%~%b~C  `c` " (code-char 7)))))
  (ok (string= "abcde" (llm:sanitize-field "abcdefgh" :max 5)))
  (ok (string= "abc" (llm:sanitize-field "abc d" :max 4)))
  (ng (llm:sanitize-field "   "))
  (ng (llm:sanitize-field nil))
  (ng (llm:sanitize-field 42)))

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

(deftest render-round-trip
  (dolist (case *round-trip-cases*)
    (destructuring-bind (intent &rest expected) case
      (let ((sentence (llm:render-book-request intent)))
        (bnt::check-request sentence (bn:parse-booking sentence) expected)))))

;;; --- Validation ---

(deftest initial-in-text
  (ok (llm:initial-in-text-p "BW" "can bw and MM go"))
  (ok (llm:initial-in-text-p "CG" "for cg&bw"))
  (ng (llm:initial-in-text-p "BW" "BWX and MM"))
  (ng (llm:initial-in-text-p "BW" "nobody"))
  ;; the ruler's non-initial words, and anything not 2-3 letters
  (ng (llm:initial-in-text-p "PM" "at 7 PM"))
  (ng (llm:initial-in-text-p "DGH" "take the DGH"))
  (ng (llm:initial-in-text-p "B" "B"))
  (ng (llm:initial-in-text-p "bw" "bw"))
  (ng (llm:initial-in-text-p nil "bw")))

(deftest validate-book-intent
  (let ((intent '(:intent :book :passengers ("BW" "MM") :destination "dentist" :day "tomorrow"
                  :dep-time "8:30 am" :ret-time "11:00 am")))
    ;; lowercase initials in the message are fine: the rule parser cannot
    ;; read them, which is why the model was asked
    (ok (string= "BW and MM to the dentist tomorrow leaving at 8:30 am and back at 11:00 am"
                 (llm:validate-book-intent intent "bw and mm need the dentist tmrw, 8:30 till 11")))
    ;; but every initial must be in the message
    (ng (llm:validate-book-intent intent "BW needs the dentist tomorrow 8:30-11")))
  ;; the ruler's non-initial words are never passengers
  (ng (llm:validate-book-intent '(:passengers ("PM") :destination "dentist" :day "tomorrow")
                                "PM to the dentist tomorrow"))
  ;; destination, day and passengers are all required
  (ng (llm:validate-book-intent '(:passengers ("BW") :day "tomorrow") "BW tomorrow"))
  (ng (llm:validate-book-intent '(:passengers ("BW") :destination "dentist") "BW dentist"))
  (ng (llm:validate-book-intent '(:destination "dentist" :day "tomorrow") "dentist tomorrow"))
  ;; a destination the parser cannot recover from the sentence (a vehicle
  ;; noun is never a destination) is rejected
  (ng (llm:validate-book-intent '(:passengers ("BW") :destination "van" :day "tomorrow")
                                "BW van tomorrow"))
  ;; the sheet's own spelling wins through the gazetteer, apostrophes and all
  (ok (llm:validate-book-intent '(:passengers ("MM") :destination "saint marys church" :day "Friday")
                                "mm to saint marys church friday"
                                :known-destinations '("Saint Mary's Church")))
  (ok (string= "Saint Mary's Church"
               (bn:booking-request-destination
                (bn:parse-booking
                 (llm:validate-book-intent '(:passengers ("MM") :destination "saint marys church" :day "Friday")
                                           "mm to saint marys church friday"
                                           :known-destinations '("Saint Mary's Church"))
                 :known-destinations '("Saint Mary's Church"))))))

(deftest constrain-intent
  (ok (llm:vehicle-in-text-p :dgh "take the DGH"))
  (ok (llm:vehicle-in-text-p :chevy "the chevrolet please"))
  (ok (llm:vehicle-in-text-p :personal "in the personal car"))
  (ng (llm:vehicle-in-text-p :dgh "take the van"))
  (ng (llm:vehicle-in-text-p :lgh "the DGH"))
  (ng (llm:vehicle-in-text-p :dgh "DGHX"))
  ;; a vehicle the message does not name is dropped: "the van" is not the DGH
  (let ((intent '(:intent :book :passengers ("MM") :destination "pool" :day "tomorrow"
                  :vehicle :dgh)))
    (ng (getf (llm:constrain-intent intent "book the van for the pool tomorrow for MM")
              :vehicle))
    (ok (eq :dgh (getf (llm:constrain-intent intent "book the dgh for the pool tomorrow for MM")
                       :vehicle)))
    ;; the rest of the reply is untouched, and so is the original
    (ok (equal '("MM") (getf (llm:constrain-intent intent "the van for MM tomorrow, pool")
                             :passengers)))
    (ok (eq :dgh (getf intent :vehicle))))
  ;; times: the hour must be in the message, as digits, a word, or noon/midnight
  (ok (llm:time-in-text-p "8:30 am" "out half past 8, back 11ish"))
  (ok (llm:time-in-text-p "11:00 am" "out half past 8, back 11ish"))
  (ok (llm:time-in-text-p "7:00 pm" "at seven PM to nine PM"))
  (ok (llm:time-in-text-p "9:00 pm" "at seven PM to nine PM"))
  (ok (llm:time-in-text-p "12:00 pm" "9am-noon"))
  (ok (llm:time-in-text-p "12:00 am" "back by midnight"))
  (ok (llm:time-in-text-p "2:00 pm" "tomorrow 2-4 for BW"))
  (ng (llm:time-in-text-p "8:30 am" "zz and qq wanna hit the test lab tmrw"))
  (ng (llm:time-in-text-p "8:00 am" "leaving at 18:00"))
  (ng (llm:time-in-text-p nil "8"))
  (let ((invented (llm:constrain-intent
                   '(:intent :book :passengers ("ZZ" "QQ") :destination "test lab" :day "tomorrow"
                     :dep-time "8:30 am" :ret-time "11:00 am" :vehicle :dgh)
                   "zz and qq wanna hit the test lab tmrw")))
    (ng (getf invented :dep-time))
    (ng (getf invented :ret-time))
    (ng (getf invented :vehicle))
    (ok (equal "test lab" (getf invented :destination)))
    (ok (equal "tomorrow" (getf invented :day))))
  (let ((kept (llm:constrain-intent
               '(:intent :book :passengers ("BW") :destination "dentist" :day "tomorrow"
                 :dep-time "8:30 am" :ret-time "11:00 am")
               "me and BW need wheels to the dentist tmrw, out half past 8, back 11ish")))
    (ok (equal "8:30 am" (getf kept :dep-time)))
    (ok (equal "11:00 am" (getf kept :ret-time))))
  ;; the day must be named somehow: relative word, weekday, month, date, ordinal
  (dolist (text '("tmrw" "tomorrow morning" "next Tuesday" "thurs 7-9" "Sept 12" "on the 22nd"
                  "4/22" "2026-09-12" "next week" "sat"))
    (ok (llm:day-in-text-p text) (format nil "day evidence in ~s" text)))
  (dolist (text '("zz and qq to the pool" "book the van for MM" "at 8 to 10" "mondayish"))
    (ng (llm:day-in-text-p text) (format nil "no day evidence in ~s" text)))
  (ng (getf (llm:constrain-intent '(:intent :book :passengers ("ZZ") :destination "pool"
                                    :day "tomorrow")
                                  "zz to the pool")
            :day))
  ;; the destination keeps only the words the message has
  (ok (equal "Saint Marys" (llm:destination-in-text "Saint Marys church"
                                                    "take the van to Saint Marys on Friday")))
  (ok (equal "saint marys church" (llm:destination-in-text "saint marys church"
                                                           "MM to Saint Mary's Church friday")))
  (ok (equal "SHMS" (llm:destination-in-text "SHMS" "add GG to Thursday's SHMS trip")))
  (ok (equal "test lab" (llm:destination-in-text "test lab" "zz and qq wanna hit the test lab tmrw")))
  (ng (llm:destination-in-text "airport" "swimming for CG"))
  (ng (llm:destination-in-text nil "x"))
  (ng (getf (llm:constrain-intent '(:intent :book :destination "airport" :day "tomorrow")
                                  "cg tomorrow")
            :destination))
  ;; a "view" that names passengers, a destination or times is a booking
  ;; phrased as a question
  (ok (eq :book (getf (llm:constrain-intent
                       '(:intent :view :passengers ("BW") :destination "dentist" :range "tomorrow")
                       "is the LGH free tomorrow for BW to see the dentist?")
                      :intent)))
  (ok (eq :book (getf (llm:constrain-intent '(:intent :view :dep-time "8:00 am") "at 8") :intent)))
  (ok (eq :book (getf (llm:constrain-intent '(:intent :view :ret-time "8:00 am") "by 8") :intent)))
  ;; but not on the strength of an invented destination or time
  (ok (eq :view (getf (llm:constrain-intent '(:intent :view :destination "dentist" :range "tomorrow")
                                            "who has the van tomorrow?")
                      :intent)))
  ;; a real question stays one, even with a day or vehicle in the reply
  (ok (eq :view (getf (llm:constrain-intent '(:intent :view :range "this week" :day "Saturday")
                                            "what's on this week?")
                      :intent)))
  (ok (eq :view (getf (llm:constrain-intent '(:intent :view :range "tomorrow" :vehicle :dgh)
                                            "who has the DGH tomorrow?")
                      :intent)))
  (ok (eq :other (getf (llm:constrain-intent '(:intent :other) "thanks") :intent)))
  (ng (llm:constrain-intent nil "x"))
  ;; a view's range must be in the message too, in some form
  (ok (llm:range-in-text-p "this week" "what's on this week?"))
  (ok (llm:range-in-text-p "the weekend" "is anything booked for the weekend?"))
  (ok (llm:range-in-text-p "the rest of the week" "show me the schedule for the rest of the week"))
  (ok (llm:range-in-text-p "Sept 19 to Sept 22" "schedule for Sept 19 to 22?"))
  (ok (llm:range-in-text-p "tomorrow" "what's on tmrw?"))
  (ok (llm:range-in-text-p "Tuesday" "anything tues?"))
  (ok (llm:range-in-text-p "today" "when's the DGH back?"))
  (ng (llm:range-in-text-p "this week" "when is my van home this tuesday"))
  (ng (llm:range-in-text-p "tomorrow" "how late is the van out on thursday?"))
  (ng (llm:range-in-text-p "Saturday" "when's the DGH back?"))
  (ng (llm:range-in-text-p nil "x"))
  ;; a range the model did not read gives way to the day it did: "this week"
  ;; for "this tuesday", "tomorrow" for "thursday", nothing at all
  (ok (equal "Tuesday" (getf (llm:constrain-intent '(:intent :view :day "Tuesday" :range "this week")
                                                   "when is my van home this tuesday")
                             :range)))
  (ok (equal "Thursday" (getf (llm:constrain-intent '(:intent :view :day "Thursday" :range "tomorrow")
                                                    "how late is the van out on thursday?")
                              :range)))
  (ok (equal "Tuesday" (getf (llm:constrain-intent '(:intent :view :day "Tuesday" :range nil)
                                                   "when is my ride home this coming tuesday")
                             :range)))
  ;; a range that is in the message stays, whatever the day says
  (ok (equal "this week" (getf (llm:constrain-intent '(:intent :view :day "Saturday" :range "this week")
                                                     "what's on this week?")
                               :range)))
  (ok (equal "today" (getf (llm:constrain-intent '(:intent :view :day "Saturday" :range "today")
                                                 "show me todays")
                           :range)))
  ;; with no day to fall back on, or one the schedule cannot read by itself,
  ;; the range is left as it is
  (ok (equal "Saturday" (getf (llm:constrain-intent '(:intent :view :day nil :range "Saturday")
                                                    "when's the DGH back?")
                              :range)))
  (ok (equal "Sept 22" (getf (llm:constrain-intent '(:intent :view :day "22nd" :range "Sept 22")
                                                   "anything on the 22nd?")
                             :range))))

(deftest render-intent
  (multiple-value-bind (command args)
      (llm:render-intent '(:intent :view :range "this week") "what's on the van this week?")
    (ok (string= "/view" command))
    (ok (string= "this week" args)))
  (multiple-value-bind (command args)
      (llm:render-intent '(:intent :view :range "Sept 19 to Sept 22") "schedule Sept 19-22?")
    (ok (string= "/view" command))
    (ok (string= "Sept 19 to Sept 22" args)))
  ;; a day in the message's own words, as the schedule reads it
  (multiple-value-bind (command args)
      (llm:render-intent '(:intent :view :range "this coming Tuesday")
                         "when is my ride home this coming Tuesday?")
    (ok (string= "/view" command))
    (ok (string= "this coming Tuesday" args)))
  ;; an explicit /book never turns into a view
  (ng (llm:render-intent '(:intent :view :range "this week") "x" :allow-view nil))
  ;; a range the schedule cannot read is dropped, as is a missing one
  (ng (llm:render-intent '(:intent :view :range "xyzzy") "x"))
  (ng (llm:render-intent '(:intent :view :range nil) "x"))
  ;; other, and no intent at all
  (ng (llm:render-intent '(:intent :other :unclear "Not a request.") "thanks"))
  (ng (llm:render-intent '(:intent nil) "x"))
  (ng (llm:render-intent nil "x"))
  (multiple-value-bind (command args)
      (llm:render-intent '(:intent :book :passengers ("BW") :destination "pool" :day "tomorrow")
                         "bw pool tmrw")
    (ok (string= "/book" command))
    (ok (string= "BW to the pool tomorrow" args)))
  (ng (llm:render-intent '(:intent :book :passengers ("BW") :destination "pool" :day "tomorrow")
                         "mm pool tmrw")))

;;; --- Talking to the server ---

(deftest llm-disabled
  (let ((llm:*llm-url* nil))
    (ng (llm:llm-enabled-p))
    (ng (llm:llm-normalize "BW to the pool tomorrow"))))

(deftest llm-unreachable
  ;; nothing listens on port 1: the call returns NIL quickly and nothing
  ;; signals
  (let ((llm:*llm-url* "http://127.0.0.1:1/v1/chat/completions")
        (llm:*llm-timeout* 2)
        (*error-output* (make-broadcast-stream)))
    (ok (llm:llm-enabled-p))
    (ng (llm:llm-normalize "BW to the pool tomorrow"))))

(deftest warm-up
  ;; nothing to warm up without a server
  (let ((llm:*llm-url* nil))
    (ng (llm:warm-up-llm)))
  ;; with one, a background thread tries, gives up after ATTEMPTS, and
  ;; nothing escapes it
  (let ((llm:*llm-url* "http://127.0.0.1:1/v1/chat/completions")
        (llm:*llm-timeout* 2)
        (*error-output* (make-broadcast-stream)))
    (let ((thread (llm:warm-up-llm :attempts 2 :interval 0)))
      (ok (bordeaux-threads:threadp thread))
      (bordeaux-threads:join-thread thread)
      (ng (bordeaux-threads:thread-alive-p thread)))))
