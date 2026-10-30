;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/slack-bot/booking-nlp
  (:use #:common-lisp)
  (:import-from #:rove #:deftest #:ok #:ng)
  (:local-nicknames (#:bn #:project-isidore/slack-bot/booking-nlp)
                    (#:nlp #:project-isidore/slack-bot/nlp))
  (:import-from #:chronicity)
  (:documentation
   "Tests for project-isidore/slack-bot/booking-nlp.

*BOOKING-CORPUS* is the regression corpus: every phrasing the old regex
parser was tested with, plus free-form sentences the new pipeline must
handle.  Add a line there whenever a user's message is misread."))

(in-package #:project-isidore/tests/slack-bot/booking-nlp)

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
              (ok (equal value (bn:booking-request-destination req)) (format nil "~s: destination" text)))
             (:day-like
              (ok (equal (day-string value) (request-day req)) (format nil "~s: day" text)))
             (:day-md
              (ok (equal (month-day-string value) (request-day req)) (format nil "~s: day" text)))
             (:day-contains
              (ok (and (request-day req) (search value (request-day req))) (format nil "~s: day ~s contains ~s" text (request-day req) value)))
             (:passengers
              (ok (equal value (bn:booking-request-passengers req)) (format nil "~s: passengers" text)))
             (:dep-time
              (ok (equal value (bn:booking-request-dep-time req)) (format nil "~s: departure" text)))
             (:ret-time
              (ok (equal value (bn:booking-request-ret-time req)) (format nil "~s: return" text)))
             (:vehicle
              (ok (eq value (bn:booking-request-vehicle req)) (format nil "~s: vehicle" text)))
             (:missing
              (ok (equal value (bn:booking-request-missing req)) (format nil "~s: missing" text)))
             (:missing-includes
              (ok (subsetp value (bn:booking-request-missing req)) (format nil "~s: missing ~s includes ~s" text (bn:booking-request-missing req) value))))))

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
     :dep-time "7:00 pm" :ret-time "9:00 pm" :missing nil)
    ;; Three-letter initials.  DMM and FBM are real people on this sheet, and
    ;; DMM in particular contains both DM and MM, who are two others -- the
    ;; ruler must read the whole token and never a substring of it.
    ("book a van for swimming next Tuesday for DMM and CG at 7 pm to 9 pm"
     :destination "swimming" :day-contains "tuesday" :passengers ("DMM" "CG")
     :dep-time "7:00 pm" :ret-time "9:00 pm" :missing nil)
    ("Can DMM and FBM take the DGH to the dentist tomorrow, leaving at 8:30 and back around 11?"
     :destination "dentist" :day-like "tomorrow" :passengers ("DMM" "FBM")
     :vehicle :dgh :dep-time "8:30 am" :ret-time "11:00 am" :missing nil)
    ("add FBM to the SHMS trip on Thursday"
     :destination "SHMS" :day-contains "thursday" :passengers ("FBM")
     :missing (:dep-time :ret-time))
    ("could DMM have a van Monday 6 to 9 for the airport?"
     :destination "airport" :day-contains "monday" :passengers ("DMM")
     :dep-time "6:00 am" :ret-time "9:00 am" :missing nil)
    ;; DM and MM still parse as themselves next to DMM.
    ("book a van for DM, MM and DMM to the pool tomorrow 8 am to 10 am"
     :destination "pool" :day-like "tomorrow" :passengers ("DM" "MM" "DMM")
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil))
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
     :dep-time "8:30 am" :ret-time "11:00 am" :missing nil)
    ;; "this coming Tuesday" is one day phrase: "coming" is not a destination
    ("MM to the dentist this coming Tuesday leaving at 8:30 and back around 11"
     :destination "dentist" :day-contains "tuesday" :passengers ("MM")
     :dep-time "8:30 am" :ret-time "11:00 am" :missing nil)
    ;; a day-first month-day
    ("MM to the pool on the 22nd of April, 9 am to noon"
     :destination "pool" :day-md "April 22" :passengers ("MM")
     :dep-time "9:00 am" :ret-time "12:00 pm" :missing nil)
    ;; "o'clock" between the hour and its meridiem, which the start of the
    ;; range shares: this message was once booked for 8 to 9 in the morning
    ("Book trip to the Detroit Airport pickup with BW as the driver and JC and BW as the returning passengers for yesterday from 8 to 9 o'clock p.m. Oct 11 Sunday. DGH is the vehicle."
     :destination "Detroit Airport" :day-md "October 11" :passengers ("BW" "JC")
     :vehicle :dgh :dep-time "8:00 pm" :ret-time "9:00 pm" :missing nil))
  "Free-form sentences the regex parser could not handle.")

(defparameter *booking-corpus* (append *legacy-corpus* *free-form-corpus*))

(deftest booking-corpus
  (dolist (entry *booking-corpus*)
    (destructuring-bind (text &rest expected) entry
      (check-request text (bn:parse-booking text) expected))))

;;; --- Focused unit tests ---

(deftest gazetteer
  (ok (string= "SHMS"
               (bn:booking-request-destination
                (bn:parse-booking "add GG to the SHMS trip on Thursday"
                                  :known-destinations '("SHMS" "Shrine")))))
  ;; the sheet's spelling is used, not the user's
  (ok (string= "Saint Marys Church"
               (bn:booking-request-destination
                (bn:parse-booking "going to saint marys church for MM tomorrow 9am to noon"
                                  :known-destinations '("Saint Marys Church")))))
  (ok (string= "Pool"
               (bn:booking-request-destination
                (bn:parse-booking "for GG going to the pool tomorrow" :known-destinations '("Pool")))))
  ;; a known destination that looks like initials is not taken as a passenger
  (let ((req (bn:parse-booking "BW to SJU tomorrow 9am to noon" :known-destinations '("SJU"))))
    (ok (string= "SJU" (bn:booking-request-destination req)))
    (ok (equal '("BW") (bn:booking-request-passengers req))))
  ;; ... but without the gazetteer it is (documented limitation)
  (let ((req (bn:parse-booking "BW to SJU tomorrow 9am to noon")))
    (ok (equal '("BW" "SJU") (bn:booking-request-passengers req)))
    (ok (member :destination (bn:booking-request-missing req))))
  ;; unknown destinations are unaffected by the gazetteer
  (ok (string= "dentist"
               (bn:booking-request-destination
                (bn:parse-booking "to the dentist for MM tomorrow 9am to noon"
                                  :known-destinations '("SHMS" "Pool"))))))

(deftest day-phrase-normalisation
  (ok (string= "tuesday" (bn::normalize-day-phrase "Tues")))
  (ok (string= "Thursday" (bn::normalize-day-phrase "Thursday's")))
  (ok (string= (format nil "April 22 ~d" (current-year)) (bn::normalize-day-phrase "4/22")))
  (ok (string= "April 22 2026" (bn::normalize-day-phrase "4/22/26")))
  (ok (string= "April 22 2026" (bn::normalize-day-phrase "4/22/2026")))
  (ok (string= (format nil "April 11th ~d" (current-year)) (bn::normalize-day-phrase "April 11th")))
  (ok (string= (format nil "Apr 11 ~d" (current-year)) (bn::normalize-day-phrase "Apr. 11")))
  ;; a day-first month-day is turned round: chronicity reads "April 11th",
  ;; not "11th of April"
  (ok (string= (format nil "April 11th ~d" (current-year)) (bn::normalize-day-phrase "11th of April")))
  (ok (string= (format nil "April 22nd ~d" (current-year))
               (bn::normalize-day-phrase "the 22nd of April")))
  (ok (string= "tomorrow" (bn::normalize-day-phrase "tomorrow morning")))
  (ok (string= "next week Tuesday" (bn::normalize-day-phrase "Tuesday next week")))
  (ok (string= "2026-09-08" (bn::normalize-day-phrase "2026-09-08")))
  ;; a weekday next to a month-day is dropped: chronicity would otherwise read
  ;; "Saturday September 12" as the coming Saturday
  (ok (string= (format nil "September 12 ~d" (current-year))
               (bn::normalize-day-phrase "Saturday September 12")))
  (ok (string= (format nil "September 12 ~d" (current-year))
               (bn::normalize-day-phrase "Saturday, September 12")))
  (ok (string= (format nil "Sep 12 ~d" (current-year))
               (bn::normalize-day-phrase "Sat Sep 12")))
  (ok (string= (format nil "September 12th ~d" (current-year))
               (bn::normalize-day-phrase "next Saturday the 12th of September")))
  (ok (string= "saturday" (bn::normalize-day-phrase "Sat")))
  ;; "coming" and "upcoming" are filler, as is a "the" then left before the weekday
  (ok (string= "this tuesday" (bn::normalize-day-phrase "this coming Tues")))
  (ok (string= "Tuesday" (bn::normalize-day-phrase "the coming Tuesday")))
  (ok (string= "Tuesday" (bn::normalize-day-phrase "upcoming Tuesday")))
  (ok (string= "day after tomorrow" (bn::normalize-day-phrase "the day after tomorrow")))
  ;; month-day phrases resolve to the current year even when that date has passed
  (let ((req (bn:parse-booking "for pool for MM on January 2 at 8 am to 9 am")))
    (ok (= (current-year) (chronicity:year-of (bn:booking-request-day-datetime req)))))
  ;; ... and a day-first one resolves at all
  (let ((dt (bn:booking-request-day-datetime
             (bn:parse-booking "MM to the pool on the 22nd of April, 9 am to noon"))))
    (ok dt)
    (ok (= 4 (and dt (chronicity:month-of dt))))
    (ok (= 22 (and dt (chronicity:day-of dt))))))

(deftest schedule-questions
  ;; a question about a trip's timing that names no passengers is a view of
  ;; the day it names, in the message's own words, or of today
  (ok (string= "this coming tuesday"
               (bn:schedule-question-range "when is my ride home this coming tuesday")))
  (ok (string= "this tuesday" (bn:schedule-question-range "when is my van home this tuesday")))
  (ok (string= "monday" (bn:schedule-question-range "when is my van home on monday")))
  (ok (string= "tomorrow" (bn:schedule-question-range "what time does the van leave tomorrow?")))
  (ok (string= "thursday" (bn:schedule-question-range "How late is the van out on thursday?")))
  (ok (string= "Saturday September 12"
               (bn:schedule-question-range "what time is the SHMS trip back on Saturday September 12?")))
  (ok (string= "today" (bn:schedule-question-range "when's the DGH back?")))
  (ok (string= "today" (bn:schedule-question-range "What time's the van leaving")))
  ;; passengers make it a booking phrased as a question
  (ng (bn:schedule-question-range "when can BW and MM have the van for the dentist tomorrow?"))
  ;; no timing cue: not a question, or not one about timing
  (ng (bn:schedule-question-range "book a van for the pool tomorrow for MM"))
  (ng (bn:schedule-question-range "whenever works, book the van for the pool tomorrow"))
  (ng (bn:schedule-question-range "who has the DGH tomorrow?"))
  (ng (bn:schedule-question-range "what's on the van this week?"))
  ;; "this coming Tuesday" is one date span, so "coming" is not left over
  ;; as a destination, and it resolves to a Tuesday
  (let ((req (bn:parse-booking "when is my ride home this coming tuesday")))
    (ng (bn:booking-request-destination req))
    (ok (string= "this coming tuesday" (bn:booking-request-day-phrase req)))
    (ok (search "Tuesday" (bn:booking-request-day req)))))

(deftest time-roles
  (flet ((times (text)
           (let ((r (bn:parse-booking text)))
             (list (bn:booking-request-dep-time r) (bn:booking-request-ret-time r)))))
    (ok (equal '("8:00 am" "10:00 am") (times "at 8 am to 10 am")))
    (ok (equal '(nil "10:00 am") (times "back by 10 am")))
    (ok (equal '("8:00 am" nil) (times "leaving at 8 am")))
    (ok (equal '("8:00 am" nil) (times "at 8 am")))
    (ok (equal '("8:30 am" "11:00 am") (times "leaving at 8:30 and back around 11")))
    (ok (equal '("7:00 pm" "9:00 pm") (times "7-9pm")))
    (ok (equal '("7:00 pm" "9:00 pm") (times "7 to 9 pm")))
    (ok (equal '("8:30 am" "10:00 am") (times "from 8:30 to 10")))
    (ok (equal '("2:00 pm" "5:00 pm") (times "leaving at 2 back by 5")))
    (ok (equal '("9:00 am" "12:00 pm") (times "out at 9 and home by noon")))
    (ok (equal '("6:00 am" "12:00 am") (times "departing 6am, back by midnight")))
    (ok (equal '("12:00 pm" "2:00 pm") (times "12 pm to 2 pm")))
    (ok (equal '("9:30 am" "11:30 am") (times "9:30 a.m. until 11:30 a.m.")))
    ;; the return cue may come first
    (ok (equal '("8:00 am" "10:00 am") (times "back by 10 am, leaving at 8 am")))
    ;; a return after midnight-ish inference: return earlier than departure without meridiem
    (ok (equal '("10:00 am" "1:00 pm") (times "leaving at 10 am and back at 1")))
    ;; the start of a range shares the end's meridiem after a cue as well
    (ok (equal '("8:00 pm" "9:00 pm") (times "from 8 to 9 pm")))
    (ok (equal '("8:30 pm" "10:00 pm") (times "from 8:30 to 10 pm")))
    (ok (equal '("8:00 am" "9:00 pm") (times "from 8 am to 9 pm")))
    ;; ... unless that would have the trip end before it starts
    (ok (equal '("11:00 am" "1:00 pm") (times "from 11 to 1 pm")))
    (ok (equal '("11:00 am" "1:00 pm") (times "11-1pm")))
    (ok (equal '("9:00 am" "12:00 pm") (times "9 to 12 pm")))
    ;; "o'clock" may stand between the hour and its meridiem, or alone
    (ok (equal '("8:00 pm" "9:00 pm") (times "from 8 to 9 o'clock p.m.")))
    (ok (equal '("8:00 pm" "9:00 pm") (times "from 8 o'clock to 9 o'clock pm")))
    (ok (equal '("9:00 pm" nil) (times "at nine o'clock pm")))
    (ok (equal '("9:00 am" nil) (times "at 9 o'clock")))
    (ok (equal '("8:00 am" "10:00 am") (times "leaving at eight o'clock and back by ten o'clock")))
    (ok (equal '(nil nil) (times "no times at all")))))

(deftest destination-selection
  (flet ((dest (text &rest keys)
           (bn:booking-request-destination (apply #'bn:parse-booking text keys))))
    (ok (string= "haircut" (dest "for a haircut for GG at 1 pm to 2 pm")))
    (ok (string= "SHMS" (dest "add GG to the SHMS trip on Thursday")))
    (ok (string= "SHMS" (dest "add BW to Thursday's SHMS trip")))
    (ok (string= "dentist" (dest "take the DGH to the dentist tomorrow for BW at 8 am to 9 am")))
    (ok (string= "Saint Marys church"
                 (dest "going to Saint Marys church for the ordination with MM tomorrow out at 9 and home by noon")))
    (ok (string= "swimming" (dest "swimming tomorrow for CG 7pm-9pm")))
    (ok (string= "House of Prayer" (dest "to the House of Prayer for MM tomorrow 9 am to 10 am")))
    (ok (string= "seminary ordination" (dest "for BW tomorrow 9 am to 10 am for seminary ordination")))
    ;; the vehicle noun is never the destination
    (ok (string= "pool" (dest "book the van for the pool tomorrow for MM 8 am to 10 am")))
    ;; nothing but request words
    (ng (dest "no structure here"))
    (ng (dest "book the van for MM"))
    (ng (dest "tomorrow at 8 am to 10 am for MM"))
    (ng (dest "hello there"))))

(deftest vehicles
  (flet ((veh (text) (bn:booking-request-vehicle (bn:parse-booking text))))
    (ok (eq :dgh (veh "take the DGH to the pool")))
    (ok (eq :lgh (veh "in the LGH")))
    (ok (eq :chevy (veh "use Chevy")))
    (ok (eq :chevy (veh "the chevrolet")))
    (ok (eq :personal (veh "take the personal car")))
    (ok (eq :personal (veh "my personal vehicle")))
    (ok (eq :dgh (veh "DGH to the pool for MM")))
    (ng (veh "the van to the pool"))
    (ng (veh "a personal matter"))
    (ng (veh "for the pool for MM"))))

(deftest passengers
  (flet ((pax (text) (bn:booking-request-passengers (bn:parse-booking text))))
    (ok (equal '("CG" "BW") (pax "for CG and BW")))
    (ok (equal '("MM" "DM" "CG") (pax "with MM DM and CG")))
    (ok (equal '("MM" "DM" "CG") (pax "MM, DM & CG")))
    (ok (equal '("CG") (pax "CG and CG")))
    (ok (equal '("CGB") (pax "for CGB")))
    (ok (equal '() (pax "for SHMS at 9 AM to 10 PM")))
    (ok (equal '() (pax "the DGH and LGH")))
    (ok (equal '() (pax "OK FYI")))
    (ok (equal '() (pax "cg and bw")))))

(deftest request-structure
  (let ((req (bn:parse-booking "Can BW and MM take the DGH to the dentist tomorrow morning, leaving at 8:30 and back around 11?")))
    (ok (bn:booking-request-p req))
    (ok (nlp:doc-p (bn:booking-request-doc req)))
    (ok (string= "tomorrow morning" (bn:booking-request-day-phrase req)))
    (ok (bn:booking-request-day-datetime req))
    (ok (string= "8:30" (bn:booking-request-dep-phrase req)))
    (ok (string= "11" (bn:booking-request-ret-phrase req)))
    (ok (equal '() (bn:booking-request-missing req)))
    (ok (equal '(:destination :day :passengers :dep-time :ret-time :vehicle)
               (loop for (k nil) on (bn:booking-request-understood req) by #'cddr collect k)))
    (ok (string= "dentist" (getf (bn:booking-request-understood req) :destination))))
  (let ((req (bn:parse-booking "hello there")))
    (ok (equal '() (bn:booking-request-understood req)))
    (ok (equal bn:*booking-slots* (bn:booking-request-missing req)))))

(deftest booking-incomplete-report
  (let* ((c (make-condition 'bn:booking-incomplete
                            :missing '(:day :passengers)
                            :understood '(:destination "swimming" :dep-time "7:00 pm"
                                          :ret-time "9:00 pm" :vehicle :dgh)
                            :text "x"))
         (report (princ-to-string c)))
    (ok (typep c 'error))
    (ok (equal '(:day :passengers) (bn:booking-incomplete-missing c)))
    (ok (string= "x" (bn:booking-incomplete-text c)))
    (ng (bn:booking-incomplete-preamble c))
    (ok (search "destination: swimming" report))
    (ok (search "departure time: 7:00 pm" report))
    (ok (search "return time: 9:00 pm" report))
    (ok (search "vehicle: DGH" report))
    (ok (search "the day and the passengers" report))
    (ok (search (bn:missing-piece-hint :day) report))
    (ok (search (bn:missing-piece-hint :passengers) report))
    (ng (search (bn:missing-piece-hint :destination) report))
    (ok (search "Please re-send" report)))
  ;; preamble first, three missing pieces, nothing understood
  (let ((report (princ-to-string
                 (make-condition 'bn:booking-incomplete
                                 :missing '(:destination :dep-time :ret-time)
                                 :preamble "Context line."))))
    (ok (= 0 (search "Context line." report)))
    (ok (search "the destination, the departure time and the return time" report))
    (ng (search "What I understood" report)))
  ;; a single missing piece reads naturally
  (let ((report (princ-to-string
                 (make-condition 'bn:booking-incomplete :missing '(:ret-time)))))
    (ok (search "I still need the return time." report)))
  (dolist (slot bn:*booking-slots*)
    (ok (bn:missing-piece-hint slot) (format nil "hint for ~a" slot)))
  (ng (bn:missing-piece-hint :vehicle))
  (ok (string= "departure time" (bn:slot-label :dep-time)))
  (ok (string= "vehicle" (bn:slot-label :vehicle)))
  (ok (string= "bogus" (bn:slot-label :bogus))))

(deftest formatting-helpers
  (ok (string= "12:00 am" (bn:format-time-12h 0 0)))
  (ok (string= "12:00 pm" (bn:format-time-12h 12 0)))
  (ok (string= "7:00 pm"  (bn:format-time-12h 19 0)))
  (ok (string= "11:30 am" (bn:format-time-12h 11 30)))
  (ok (string= "1:05 pm"  (bn:format-time-12h 13 5)))
  (ok (string= "Monday April 20" (bn:format-booking-day (chronicity:parse "April 20 2026"))))
  (ok (string= "Thursday January 1" (bn:format-booking-day (chronicity:parse "January 1 2026"))))
  (ok (= 12 (length bn:*month-names*)))
  (ok (string= "September" (nth 8 bn:*month-names*))))

;;; --- First-person resolution -------------------------------------------
;;;
;;; SLACK-BOT rewrites a message's "me"/"my"/"I" to the sender's initials
;;; before either parser sees it, so that "book me a van" names somebody.
;;; These tests pin the rewrite itself; the identity lookup that supplies the
;;; initials is tested in SLACK-BOT-TESTS.

(deftest self-reference-detection
  ;; Every word and contraction *SELF-WORDS* / *SELF-CONTRACTIONS* cover.
  (dolist (text '("book me a van" "my ride to SHMS" "can I get a van"
                  "just myself thursday" "I'd like a van" "I'm going to SHMS"
                  "I'll take the DGH" "I've got a dentist appointment"
                  "BOOK ME A VAN" "Me and CG to the pool"))
    (ok (bn:self-reference-p text) (format nil "~s refers to its sender" text)))
  ;; A word that merely contains one must not count, nor must other people's
  ;; initials -- "MM" and "DM" both live inside the sender word "DMM".
  (dolist (text '("meet CG at home before the SHMS trip"
                  "book a van for CG and MM to the pool"
                  "add DMM to the SHMS trip"
                  "the Chevy is in the shop"
                  "" "   ")
           )
    (ng (bn:self-reference-p text) (format nil "~s refers to nobody" text)))
  (ng (bn:self-reference-p nil)))

(deftest resolve-self-reference-rewrites
  (macrolet ((rewrites (input expected)
               `(ok (string= ,expected (bn:resolve-self-reference ,input "DMM")))))
    ;; Bare words.
    (rewrites "book me a van to the dentist tomorrow 9 to 11"
              "book DMM a van to the dentist tomorrow 9 to 11")
    (rewrites "my ride to SHMS thursday" "DMM ride to SHMS thursday")
    (rewrites "just myself, thursday 9 to 11 to SHMS"
              "just DMM, thursday 9 to 11 to SHMS")
    (rewrites "can I get a van to the pool sat 8 to 10 am"
              "can DMM get a van to the pool sat 8 to 10 am")
    ;; Contractions expand to a clause rather than "DMM'd", whose tokens no
    ;; longer look like initials.
    (rewrites "I'd like a van for the pool sat 8-10"
              "DMM would like a van for the pool sat 8-10")
    (rewrites "I'm going to SHMS thursday" "DMM is going to SHMS thursday")
    (rewrites "I'll take the DGH tomorrow" "DMM will take the DGH tomorrow")
    (rewrites "I've got a dentist appointment" "DMM has got a dentist appointment")
    ;; The typographic apostrophe Slack's phone clients substitute.
    (rewrites "I’d like a van" "DMM would like a van")
    ;; Case is ignored on the way in; the initials are written as given.
    (rewrites "BOOK ME A VAN" "BOOK DMM A VAN")
    ;; Alongside other passengers, in either order.
    (rewrites "me and CG to the pool" "DMM and CG to the pool")
    (rewrites "CG and I need a van" "CG and DMM need a van")
    ;; Left alone: words that merely contain a self-word, and initials that
    ;; are substrings of the sender's own.
    (rewrites "meet CG at home before the SHMS trip"
              "meet CG at home before the SHMS trip")
    (rewrites "book a van for CG and MM to the pool"
              "book a van for CG and MM to the pool"))
  ;; Non-strings pass through rather than signalling.
  (ok (eq nil (bn:resolve-self-reference nil "DMM")))
  (ok (string= "book me a van" (bn:resolve-self-reference "book me a van" nil))))

;; The whole point of rewriting before parsing: PARSE-BOOKING must read the
;; result as a complete booking for the sender.  Three-letter initials are used
;; throughout because DMM and FBM are the cases this was built for.
(deftest resolved-self-reference-parses
  (dolist (case '(("book me a van to the dentist tomorrow 9 to 11" "DMM" ("DMM") "dentist")
                  ("I'd like a van for the pool tomorrow 8 to 10 am" "DMM" ("DMM") "pool")
                  ("can I get a van to the pool tomorrow 8 to 10 am" "FBM" ("FBM") "pool")
                  ("me and CG to the pool tomorrow 8 to 10 am" "FBM" ("FBM" "CG") "pool")
                  ("CG and I need a van to the pool tomorrow 8 to 10 am"
                   "DMM" ("CG" "DMM") "pool")))
    (destructuring-bind (text initial expected-pax expected-dest) case
      (let ((req (bn:parse-booking (bn:resolve-self-reference text initial))))
        (ok (equal expected-pax (bn:booking-request-passengers req)) (format nil "~s resolves to ~a" text expected-pax))
        (ok (string= expected-dest (bn:booking-request-destination req)))
        (ng (bn:booking-request-missing req) (format nil "~s is complete once resolved" text))))))

;; An unresolved ME or MY -- the sender could not be identified -- must not
;; become a passenger of that name, or the sheet gains a person who does not
;; exist.
(deftest me-is-never-a-passenger
  (let ((req (bn:parse-booking "BOOK ME A VAN TO THE POOL TOMORROW 8 TO 10 AM")))
    (ng (bn:booking-request-passengers req))
    (ok (member :passengers (bn:booking-request-missing req))))
  (let ((req (bn:parse-booking "MY RIDE TO SHMS TOMORROW 8 TO 10 AM")))
    (ng (bn:booking-request-passengers req))))
