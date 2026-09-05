;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/booking-nlp-tests
  (:use #:common-lisp #:parachute)
  (:local-nicknames (#:bn #:project-isidore/booking-nlp)
                    (#:nlp #:project-isidore/nlp))
  (:import-from #:chronicity)
  (:documentation
   "Parachute tests for project-isidore/booking-nlp.

*BOOKING-CORPUS* is the regression corpus: every phrasing the old regex
parser was tested with, plus free-form sentences the new pipeline must
handle.  Add a line there whenever a user's message is misread."))

(in-package #:project-isidore/tests/booking-nlp-tests)

;;; --- Helpers ---

(defun current-year ()
  (nth-value 5 (decode-universal-time (get-universal-time))))

(defun day-string (phrase)
  "Lowercased sheet-format day (\"saturday september 5\") for a relative
PHRASE such as \"tomorrow\", computed at check time."
  (string-downcase (bn:format-booking-day (chronicity:parse phrase :context :future))))

(defun month-day-string (month-day)
  "Lowercased sheet-format day for MONTH-DAY (\"April 11\") in the current year."
  (day-string (format nil "~a ~d" month-day (current-year))))

(defun request-day (req)
  (let ((day (bn:booking-request-day req)))
    (and day (string-downcase day))))

(defun check-request (text req expected)
  "Assert each KEY VALUE pair of the EXPECTED plist against REQ."
  (loop for (key value) on expected by #'cddr
        do (ecase key
             (:destination
              (is equal value (bn:booking-request-destination req) "~s: destination" text))
             (:day-like
              (is equal (day-string value) (request-day req) "~s: day" text))
             (:day-md
              (is equal (month-day-string value) (request-day req) "~s: day" text))
             (:day-contains
              (true (and (request-day req) (search value (request-day req)))
                    "~s: day ~s contains ~s" text (request-day req) value))
             (:passengers
              (is equal value (bn:booking-request-passengers req) "~s: passengers" text))
             (:dep-time
              (is equal value (bn:booking-request-dep-time req) "~s: departure" text))
             (:ret-time
              (is equal value (bn:booking-request-ret-time req) "~s: return" text))
             (:vehicle
              (is eq value (bn:booking-request-vehicle req) "~s: vehicle" text))
             (:missing
              (is equal value (bn:booking-request-missing req) "~s: missing" text))
             (:missing-includes
              (true (subsetp value (bn:booking-request-missing req))
                    "~s: missing ~s includes ~s" text (bn:booking-request-missing req) value)))))

;;; --- Regression corpus ---

(defparameter *legacy-corpus*
  '(;; canonical sentences
    ("I want to book a van for swimming next week Tuesday for CG and BW at seven PM to nine PM"
     :destination "swimming" :day-contains "tuesday" :passengers ("CG" "BW")
     :dep-time "7:00 pm" :ret-time "9:00 pm" :missing nil)
    ("book for pool tomorrow for MM at 8 am to 10 am"
     :destination "pool" :day-like "tomorrow" :passengers ("MM")
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for the pool next Friday for BW at 9 am to 11 am"
     :destination "pool" :day-contains "friday" :passengers ("BW")
     :dep-time "9:00 am" :ret-time "11:00 am" :missing nil)
    ("no structure here"
     :missing (:destination :day :passengers :dep-time :ret-time))
    ;; order-agnostic components; no day now means "day missing"
    ("book a van for a haircut from 12:30pm to 2:30pm for GG."
     :destination "haircut" :passengers ("GG") :dep-time "12:30 pm" :ret-time "2:30 pm"
     :vehicle nil :missing (:day))
    ("for CG and BW for swimming at 7 pm to 9 pm"
     :destination "swimming" :passengers ("CG" "BW") :dep-time "7:00 pm" :ret-time "9:00 pm"
     :missing (:day))
    ("from 12:30pm to 2:30pm for a haircut for GG"
     :destination "haircut" :passengers ("GG") :dep-time "12:30 pm" :ret-time "2:30 pm"
     :missing (:day))
    ("from 8 am to 10 am for MM for pool"
     :destination "pool" :passengers ("MM") :dep-time "8:00 am" :ret-time "10:00 am"
     :missing (:day))
    ("tomorrow for swimming for CG at 7 pm to 9 pm"
     :destination "swimming" :day-like "tomorrow" :passengers ("CG") :missing nil)
    ("for swimming for CG at 7 pm to 9 pm tomorrow"
     :destination "swimming" :day-like "tomorrow" :passengers ("CG") :missing nil)
    ("for swimming tomorrow for CG at 7 pm to 9 pm"
     :destination "swimming" :day-like "tomorrow" :passengers ("CG") :missing nil)
    ("for pool for MM at 8 am to 10 am"
     :destination "pool" :passengers ("MM") :vehicle nil :missing (:day))
    ("for pool for MM at 8 am to 10 am."
     :destination "pool" :passengers ("MM") :ret-time "10:00 am" :missing (:day))
    ("from 8 am to 10 am for MM, DM and CG for pool tomorrow"
     :destination "pool" :day-like "tomorrow" :passengers ("MM" "DM" "CG")
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ;; preambles
    ("I need to book the van for swimming tomorrow for CG at 8 am to 10 am"
     :destination "swimming" :day-like "tomorrow" :passengers ("CG")
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("can you book a van for swimming tomorrow for CG at 8 am to 10 am"
     :destination "swimming" :passengers ("CG") :missing nil)
    ("please reserve the van for swimming tomorrow for CG at 8 am to 10 am"
     :destination "swimming" :passengers ("CG") :missing nil)
    ("I'd like to schedule a van for swimming tomorrow for CG at 8 am to 10 am"
     :destination "swimming" :passengers ("CG") :missing nil)
    ("could you please book the van for swimming tomorrow for CG at 8 am to 10 am"
     :destination "swimming" :passengers ("CG") :missing nil)
    ("I want to get the van for swimming tomorrow for CG at 8 am to 10 am"
     :destination "swimming" :passengers ("CG") :missing nil)
    ("book for swimming tomorrow for CG at 8 am to 10 am"
     :destination "swimming" :passengers ("CG") :missing nil)
    ;; departure-time introducers
    ("for swimming tomorrow for CG from seven PM to nine PM"
     :destination "swimming" :dep-time "7:00 pm" :ret-time "9:00 pm" :missing nil)
    ("for pool tomorrow for MM starting at 8 am to 10 am"
     :destination "pool" :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM starting 8 am to 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM departing 8 am to 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM leaving at 8 am to 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM leaving 8 am to 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM beginning at 8 am to 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM going at 8 am to 10 am"
     :destination "pool" :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM picking up at 8 am to 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM pickup at 8 am to 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ;; return-time introducers
    ("for pool tomorrow for MM at 8 am until 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am till 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am through 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am returning at 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am returning by 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am ending at 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am back by 10 am"
     :destination "pool" :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am back at 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am dropping off at 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am dropoff at 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am arrive by 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am arrive at 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am be back by 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am get back by 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am return by 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am done by 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM at 8 am finished by 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM leaving 8 am be back by 10 am"
     :destination "pool" :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM leaving 8 am until 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("for pool tomorrow for MM starting at 8 am till 10 am"
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ;; passenger introducers
    ("for swimming tomorrow with CG at 8 am to 10 am"
     :destination "swimming" :passengers ("CG") :dep-time "8:00 am" :missing nil)
    ("for swimming tomorrow carrying CG at 8 am to 10 am"
     :destination "swimming" :passengers ("CG") :missing nil)
    ("for swimming tomorrow taking CG at 8 am to 10 am"
     :destination "swimming" :passengers ("CG") :missing nil)
    ("for swimming tomorrow bringing CG at 8 am to 10 am"
     :destination "swimming" :passengers ("CG") :missing nil)
    ("for swimming tomorrow passengers CG at 8 am to 10 am"
     :destination "swimming" :passengers ("CG") :missing nil)
    ("book for swimming tomorrow taking CG from 8 am to 10 am"
     :destination "swimming" :passengers ("CG") :dep-time "8:00 am" :ret-time "10:00 am"
     :missing nil)
    ;; destination introducers
    ("going to swimming tomorrow for CG at 8 am to 10 am"
     :destination "swimming" :passengers ("CG") :missing nil)
    ("headed to the pool tomorrow for CG at 8 am to 10 am"
     :destination "pool" :passengers ("CG") :missing nil)
    ("heading to the pool tomorrow for CG at 8 am to 10 am"
     :destination "pool" :passengers ("CG") :missing nil)
    ("driving to the store tomorrow for CG at 8 am to 10 am"
     :destination "store" :passengers ("CG") :missing nil)
    ;; bare time ranges
    ("for swimming tomorrow for CG 8am to 10am"
     :destination "swimming" :passengers ("CG") :dep-time "8:00 am" :ret-time "10:00 am"
     :missing nil)
    ("for pool for MM 7 pm to 9 pm"
     :destination "pool" :passengers ("MM") :dep-time "7:00 pm" :ret-time "9:00 pm"
     :missing (:day))
    ("book a van for swimming tomorrow for CG 8am to 3pm"
     :destination "swimming" :passengers ("CG") :dep-time "8:00 am" :ret-time "3:00 pm"
     :missing nil)
    ;; vehicle preference
    ("for swimming tomorrow for CG at 8 am to 10 am take the DGH"
     :destination "swimming" :passengers ("CG") :vehicle :dgh :missing nil)
    ("use the LGH for pool for MM at 8 am to 10 am"
     :destination "pool" :vehicle :lgh :missing (:day))
    ("for pool for MM at 8 am to 10 am prefer the Chevy"
     :destination "pool" :vehicle :chevy :missing (:day))
    ("for pool for MM at 8 am to 10 am in the DGH"
     :destination "pool" :vehicle :dgh :missing (:day))
    ("I'd like to reserve the DGH going to swimming tomorrow for CG leaving 8 am be back by 10 am"
     :destination "swimming" :passengers ("CG") :vehicle :dgh
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("book the van for pool tomorrow for MM 8am to 3pm"
     :destination "pool" :passengers ("MM") :vehicle nil
     :dep-time "8:00 am" :ret-time "3:00 pm" :missing nil)
    ;; meridiem spellings
    ("for pool for MM at 9:30 a.m. to 11:30 a.m."
     :dep-time "9:30 am" :ret-time "11:30 am" :missing (:day))
    ("for pool for MM at 9:30 p.m. to 11:30 p.m."
     :dep-time "9:30 pm" :ret-time "11:30 pm" :missing (:day))
    ("for pool for MM at 9:30 a.m to 11:30 a.m"
     :dep-time "9:30 am" :ret-time "11:30 am" :missing (:day))
    ("for pool for MM at 9:30 A.M. to 11:30 A.M."
     :dep-time "9:30 am" :ret-time "11:30 am" :missing (:day))
    ;; month-day phrases (anchored to the current year)
    ("for BW for April 11th at 9:30 am to 11:30 am for seminary ordination"
     :destination "seminary ordination" :day-md "April 11" :passengers ("BW")
     :dep-time "9:30 am" :ret-time "11:30 am" :missing nil)
    ("for pool for MM for Apr 11 at 8 am to 10 am"
     :destination "pool" :day-md "April 11" :missing nil)
    ("for pool for MM for March 30 at 8 am to 10 am"
     :destination "pool" :day-md "March 30" :missing nil)
    ("Book a van for BW for April 11th from 9:30 a.m. to 11:30 a.m going to seminary ordination."
     :destination "seminary ordination" :day-md "April 11" :passengers ("BW")
     :dep-time "9:30 am" :ret-time "11:30 am" :missing nil)
    ("for CG and BW at seven PM to nine PM going to swimming next week Tuesday"
     :destination "swimming" :day-contains "tuesday" :passengers ("CG" "BW")
     :dep-time "7:00 pm" :ret-time "9:00 pm" :missing nil))
  "Sentences carried over from the regex-era tests, with expectations in the
new parser's terms (no articles in destinations, no default day).")

(defparameter *free-form-corpus*
  '(("Can BW and MM take the DGH to the dentist tomorrow morning, leaving at 8:30 and back around 11?"
     :destination "dentist" :day-like "tomorrow" :passengers ("BW" "MM") :vehicle :dgh
     :dep-time "8:30 am" :ret-time "11:00 am" :missing nil)
    ("Please add GG to the SHMS trip on Thursday"
     :destination "SHMS" :day-contains "thursday" :passengers ("GG")
     :missing (:dep-time :ret-time))
    ("We need the van Friday, going to Saint Marys church for the ordination with MM DM and CG, out at 9 and home by noon"
     :destination "Saint Marys church" :day-contains "friday" :passengers ("MM" "DM" "CG")
     :vehicle nil :dep-time "9:00 am" :ret-time "12:00 pm" :missing nil)
    ("reserve the Chevy for a haircut from 12:30pm to 2:30pm for GG."
     :destination "haircut" :passengers ("GG") :vehicle :chevy
     :dep-time "12:30 pm" :ret-time "2:30 pm" :missing (:day))
    ("The van is needed by CG for the airport on Monday from 6 am to 9 am"
     :destination "airport" :day-contains "monday" :passengers ("CG")
     :dep-time "6:00 am" :ret-time "9:00 am" :missing nil)
    ("Could we get the LGH for a dentist visit tomorrow at 2pm until 4pm for BW?"
     :destination "dentist" :day-like "tomorrow" :passengers ("BW") :vehicle :lgh
     :dep-time "2:00 pm" :ret-time "4:00 pm" :missing nil)
    ("MM and DM to the pool on April 22nd, leaving at 9 a.m. and returning at noon"
     :destination "pool" :day-md "April 22" :passengers ("MM" "DM")
     :dep-time "9:00 am" :ret-time "12:00 pm" :missing nil)
    ("swimming next week Tuesday for CG 7pm-9pm"
     :destination "swimming" :day-contains "tuesday" :passengers ("CG")
     :dep-time "7:00 pm" :ret-time "9:00 pm" :missing nil)
    ("BW to the doctor 4/22 from 8:30 to 10"
     :destination "doctor" :day-md "April 22" :passengers ("BW")
     :dep-time "8:30 am" :ret-time "10:00 am" :missing nil)
    ("CG needs a ride to the airport tomorrow morning, departing 6am, back by midnight"
     :destination "airport" :day-like "tomorrow" :passengers ("CG")
     :dep-time "6:00 am" :ret-time "12:00 am" :missing nil)
    ("add BW to Thursday's SHMS trip"
     :destination "SHMS" :day-contains "thursday" :passengers ("BW")
     :missing (:dep-time :ret-time))
    ("Take the personal car to the store on Saturday for DM 10 am to 11 am"
     :destination "store" :day-contains "saturday" :passengers ("DM") :vehicle :personal
     :dep-time "10:00 am" :ret-time "11:00 am" :missing nil)
    ("hey can you book swimming Tues 7-9pm for CG & BW"
     :destination "swimming" :day-contains "tuesday" :passengers ("CG" "BW")
     :dep-time "7:00 pm" :ret-time "9:00 pm" :missing nil)
    ("for pool tomorrow for MM leaving at 2 back by 5"
     :destination "pool" :dep-time "2:00 pm" :ret-time "5:00 pm" :missing nil)
    ("for pool tomorrow for MM 8 am and 10 am"
     :destination "pool" :dep-time "8:00 am" :ret-time "10:00 am" :missing nil)
    ("book the van for the pool tomorrow for MM 8 am to 10 am"
     :destination "pool" :vehicle nil :missing nil)
    ("use Chevy for pool tomorrow for MM 8 am to 10 am"
     :destination "pool" :vehicle :chevy :missing nil)
    ;; one missing piece at a time
    ("book the van for MM"
     :passengers ("MM") :missing (:destination :day :dep-time :ret-time))
    ("tomorrow at 8 am to 10 am for MM"
     :day-like "tomorrow" :passengers ("MM") :dep-time "8:00 am" :ret-time "10:00 am"
     :missing (:destination))
    ("for pool tomorrow at 8 am to 10 am"
     :destination "pool" :missing (:passengers))
    ("for pool tomorrow for MM back by 10 am"
     :destination "pool" :ret-time "10:00 am" :missing (:dep-time))
    ("for pool tomorrow for MM leaving at 8 am"
     :destination "pool" :dep-time "8:00 am" :missing (:ret-time))
    ("back by 10 am"
     :ret-time "10:00 am" :missing (:destination :day :passengers :dep-time))
    ("hello there"
     :missing (:destination :day :passengers :dep-time :ret-time))
    ;; a weekday written next to the date must not win over the date
    ("ZZ and QQ need the DGH for a test run on Saturday September 12, out at 3 pm and home by 5 pm"
     :day-md "September 12" :passengers ("ZZ" "QQ") :vehicle :dgh
     :dep-time "3:00 pm" :ret-time "5:00 pm" :missing nil)
    ("book a van to the test lab on Sat Sep 12 for ZZ at 9 am to 10 am"
     :destination "test lab" :day-md "September 12" :passengers ("ZZ")
     :dep-time "9:00 am" :ret-time "10:00 am" :missing nil)
    ("MM to the dentist on Saturday, September 12th leaving at 8:30 and back around 11"
     :destination "dentist" :day-md "September 12" :passengers ("MM")
     :dep-time "8:30 am" :ret-time "11:00 am" :missing nil))
  "Free-form sentences the regex parser could not handle.")

(defparameter *booking-corpus* (append *legacy-corpus* *free-form-corpus*))

(define-test booking-corpus
  (dolist (entry *booking-corpus*)
    (destructuring-bind (text &rest expected) entry
      (check-request text (bn:parse-booking text) expected))))

;;; --- Focused unit tests ---

(define-test gazetteer
  (is string= "SHMS"
      (bn:booking-request-destination
       (bn:parse-booking "add GG to the SHMS trip on Thursday"
                         :known-destinations '("SHMS" "Shrine"))))
  ;; the sheet's spelling is used, not the user's
  (is string= "Saint Marys Church"
      (bn:booking-request-destination
       (bn:parse-booking "going to saint marys church for MM tomorrow 9am to noon"
                         :known-destinations '("Saint Marys Church"))))
  (is string= "Pool"
      (bn:booking-request-destination
       (bn:parse-booking "for GG going to the pool tomorrow" :known-destinations '("Pool"))))
  ;; a known destination that looks like initials is not taken as a passenger
  (let ((req (bn:parse-booking "BW to SJU tomorrow 9am to noon" :known-destinations '("SJU"))))
    (is string= "SJU" (bn:booking-request-destination req))
    (is equal '("BW") (bn:booking-request-passengers req)))
  ;; ... but without the gazetteer it is (documented limitation)
  (let ((req (bn:parse-booking "BW to SJU tomorrow 9am to noon")))
    (is equal '("BW" "SJU") (bn:booking-request-passengers req))
    (true (member :destination (bn:booking-request-missing req))))
  ;; unknown destinations are unaffected by the gazetteer
  (is string= "dentist"
      (bn:booking-request-destination
       (bn:parse-booking "to the dentist for MM tomorrow 9am to noon"
                         :known-destinations '("SHMS" "Pool")))))

(define-test day-phrase-normalisation
  (is string= "tuesday" (bn::normalize-day-phrase "Tues"))
  (is string= "Thursday" (bn::normalize-day-phrase "Thursday's"))
  (is string= (format nil "April 22 ~d" (current-year)) (bn::normalize-day-phrase "4/22"))
  (is string= "April 22 2026" (bn::normalize-day-phrase "4/22/26"))
  (is string= "April 22 2026" (bn::normalize-day-phrase "4/22/2026"))
  (is string= (format nil "April 11th ~d" (current-year)) (bn::normalize-day-phrase "April 11th"))
  (is string= (format nil "Apr 11 ~d" (current-year)) (bn::normalize-day-phrase "Apr. 11"))
  (is string= (format nil "11th of April ~d" (current-year)) (bn::normalize-day-phrase "11th of April"))
  (is string= "tomorrow" (bn::normalize-day-phrase "tomorrow morning"))
  (is string= "next week Tuesday" (bn::normalize-day-phrase "Tuesday next week"))
  (is string= "2026-09-08" (bn::normalize-day-phrase "2026-09-08"))
  ;; a weekday next to a month-day is dropped: chronicity would otherwise read
  ;; "Saturday September 12" as the coming Saturday
  (is string= (format nil "September 12 ~d" (current-year))
      (bn::normalize-day-phrase "Saturday September 12"))
  (is string= (format nil "September 12 ~d" (current-year))
      (bn::normalize-day-phrase "Saturday, September 12"))
  (is string= (format nil "Sep 12 ~d" (current-year))
      (bn::normalize-day-phrase "Sat Sep 12"))
  (is string= (format nil "12th of September ~d" (current-year))
      (bn::normalize-day-phrase "next Saturday the 12th of September"))
  (is string= "saturday" (bn::normalize-day-phrase "Sat"))
  ;; month-day phrases resolve to the current year even when that date has passed
  (let ((req (bn:parse-booking "for pool for MM on January 2 at 8 am to 9 am")))
    (is = (current-year) (chronicity:year-of (bn:booking-request-day-datetime req)))))

(define-test time-roles
  (flet ((times (text)
           (let ((r (bn:parse-booking text)))
             (list (bn:booking-request-dep-time r) (bn:booking-request-ret-time r)))))
    (is equal '("8:00 am" "10:00 am") (times "at 8 am to 10 am"))
    (is equal '(nil "10:00 am") (times "back by 10 am"))
    (is equal '("8:00 am" nil) (times "leaving at 8 am"))
    (is equal '("8:00 am" nil) (times "at 8 am"))
    (is equal '("8:30 am" "11:00 am") (times "leaving at 8:30 and back around 11"))
    (is equal '("7:00 pm" "9:00 pm") (times "7-9pm"))
    (is equal '("7:00 pm" "9:00 pm") (times "7 to 9 pm"))
    (is equal '("8:30 am" "10:00 am") (times "from 8:30 to 10"))
    (is equal '("2:00 pm" "5:00 pm") (times "leaving at 2 back by 5"))
    (is equal '("9:00 am" "12:00 pm") (times "out at 9 and home by noon"))
    (is equal '("6:00 am" "12:00 am") (times "departing 6am, back by midnight"))
    (is equal '("12:00 pm" "2:00 pm") (times "12 pm to 2 pm"))
    (is equal '("9:30 am" "11:30 am") (times "9:30 a.m. until 11:30 a.m."))
    ;; the return cue may come first
    (is equal '("8:00 am" "10:00 am") (times "back by 10 am, leaving at 8 am"))
    ;; a return after midnight-ish inference: return earlier than departure without meridiem
    (is equal '("10:00 am" "1:00 pm") (times "leaving at 10 am and back at 1"))
    (is equal '(nil nil) (times "no times at all"))))

(define-test destination-selection
  (flet ((dest (text &rest keys)
           (bn:booking-request-destination (apply #'bn:parse-booking text keys))))
    (is string= "haircut" (dest "for a haircut for GG at 1 pm to 2 pm"))
    (is string= "SHMS" (dest "add GG to the SHMS trip on Thursday"))
    (is string= "SHMS" (dest "add BW to Thursday's SHMS trip"))
    (is string= "dentist" (dest "take the DGH to the dentist tomorrow for BW at 8 am to 9 am"))
    (is string= "Saint Marys church"
        (dest "going to Saint Marys church for the ordination with MM tomorrow out at 9 and home by noon"))
    (is string= "swimming" (dest "swimming tomorrow for CG 7pm-9pm"))
    (is string= "House of Prayer" (dest "to the House of Prayer for MM tomorrow 9 am to 10 am"))
    (is string= "seminary ordination" (dest "for BW tomorrow 9 am to 10 am for seminary ordination"))
    ;; the vehicle noun is never the destination
    (is string= "pool" (dest "book the van for the pool tomorrow for MM 8 am to 10 am"))
    ;; nothing but request words
    (false (dest "no structure here"))
    (false (dest "book the van for MM"))
    (false (dest "tomorrow at 8 am to 10 am for MM"))
    (false (dest "hello there"))))

(define-test vehicles
  (flet ((veh (text) (bn:booking-request-vehicle (bn:parse-booking text))))
    (is eq :dgh (veh "take the DGH to the pool"))
    (is eq :lgh (veh "in the LGH"))
    (is eq :chevy (veh "use Chevy"))
    (is eq :chevy (veh "the chevrolet"))
    (is eq :personal (veh "take the personal car"))
    (is eq :personal (veh "my personal vehicle"))
    (is eq :dgh (veh "DGH to the pool for MM"))
    (false (veh "the van to the pool"))
    (false (veh "a personal matter"))
    (false (veh "for the pool for MM"))))

(define-test passengers
  (flet ((pax (text) (bn:booking-request-passengers (bn:parse-booking text))))
    (is equal '("CG" "BW") (pax "for CG and BW"))
    (is equal '("MM" "DM" "CG") (pax "with MM DM and CG"))
    (is equal '("MM" "DM" "CG") (pax "MM, DM & CG"))
    (is equal '("CG") (pax "CG and CG"))
    (is equal '("CGB") (pax "for CGB"))
    (is equal '() (pax "for SHMS at 9 AM to 10 PM"))
    (is equal '() (pax "the DGH and LGH"))
    (is equal '() (pax "OK FYI"))
    (is equal '() (pax "cg and bw"))))

(define-test request-structure
  (let ((req (bn:parse-booking "Can BW and MM take the DGH to the dentist tomorrow morning, leaving at 8:30 and back around 11?")))
    (true (bn:booking-request-p req))
    (true (nlp:doc-p (bn:booking-request-doc req)))
    (is string= "tomorrow morning" (bn:booking-request-day-phrase req))
    (true (bn:booking-request-day-datetime req))
    (is string= "8:30" (bn:booking-request-dep-phrase req))
    (is string= "11" (bn:booking-request-ret-phrase req))
    (is equal '() (bn:booking-request-missing req))
    (is equal '(:destination :day :passengers :dep-time :ret-time :vehicle)
        (loop for (k nil) on (bn:booking-request-understood req) by #'cddr collect k))
    (is string= "dentist" (getf (bn:booking-request-understood req) :destination)))
  (let ((req (bn:parse-booking "hello there")))
    (is equal '() (bn:booking-request-understood req))
    (is equal bn:*booking-slots* (bn:booking-request-missing req))))

(define-test booking-incomplete-report
  (let* ((c (make-condition 'bn:booking-incomplete
                            :missing '(:day :passengers)
                            :understood '(:destination "swimming" :dep-time "7:00 pm"
                                          :ret-time "9:00 pm" :vehicle :dgh)
                            :text "x"))
         (report (princ-to-string c)))
    (true (typep c 'error))
    (is equal '(:day :passengers) (bn:booking-incomplete-missing c))
    (is string= "x" (bn:booking-incomplete-text c))
    (false (bn:booking-incomplete-preamble c))
    (true (search "destination: swimming" report))
    (true (search "departure time: 7:00 pm" report))
    (true (search "return time: 9:00 pm" report))
    (true (search "vehicle: DGH" report))
    (true (search "the day and the passengers" report))
    (true (search (bn:missing-piece-hint :day) report))
    (true (search (bn:missing-piece-hint :passengers) report))
    (false (search (bn:missing-piece-hint :destination) report))
    (true (search "Please re-send" report)))
  ;; preamble first, three missing pieces, nothing understood
  (let ((report (princ-to-string
                 (make-condition 'bn:booking-incomplete
                                 :missing '(:destination :dep-time :ret-time)
                                 :preamble "Context line."))))
    (is = 0 (search "Context line." report))
    (true (search "the destination, the departure time and the return time" report))
    (false (search "What I understood" report)))
  ;; a single missing piece reads naturally
  (let ((report (princ-to-string
                 (make-condition 'bn:booking-incomplete :missing '(:ret-time)))))
    (true (search "I still need the return time." report)))
  (dolist (slot bn:*booking-slots*)
    (true (bn:missing-piece-hint slot) "hint for ~a" slot))
  (false (bn:missing-piece-hint :vehicle))
  (is string= "departure time" (bn:slot-label :dep-time))
  (is string= "vehicle" (bn:slot-label :vehicle))
  (is string= "bogus" (bn:slot-label :bogus)))

(define-test formatting-helpers
  (is string= "12:00 am" (bn:format-time-12h 0 0))
  (is string= "12:00 pm" (bn:format-time-12h 12 0))
  (is string= "7:00 pm"  (bn:format-time-12h 19 0))
  (is string= "11:30 am" (bn:format-time-12h 11 30))
  (is string= "1:05 pm"  (bn:format-time-12h 13 5))
  (is string= "Monday April 20" (bn:format-booking-day (chronicity:parse "April 20 2026")))
  (is string= "Thursday January 1" (bn:format-booking-day (chronicity:parse "January 1 2026")))
  (is = 12 (length bn:*month-names*))
  (is string= "September" (nth 8 bn:*month-names*)))
