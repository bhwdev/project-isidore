;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/booking-nlp
  (:use #:common-lisp)
  (:local-nicknames (#:nlp #:project-isidore/nlp))
  (:import-from #:cl-ppcre)
  (:import-from #:chronicity
                #:hour-of #:minute-of #:dow-of #:dow-name
                #:year-of #:month-of #:day-of)
  (:export
   ;; parsing
   #:parse-booking #:schedule-question-range
   #:booking-request #:booking-request-p #:make-booking-request
   #:booking-request-text #:booking-request-doc
   #:booking-request-destination
   #:booking-request-day-phrase #:booking-request-day-datetime #:booking-request-day
   #:booking-request-passengers
   #:booking-request-dep-phrase #:booking-request-ret-phrase
   #:booking-request-dep-time #:booking-request-ret-time
   #:booking-request-vehicle
   #:booking-request-missing #:booking-request-understood
   #:*booking-slots* #:*booking-ruler* #:*cue-matcher*
   ;; condition
   #:booking-incomplete #:booking-incomplete-missing #:booking-incomplete-understood
   #:booking-incomplete-text #:booking-incomplete-preamble
   #:missing-piece-hint #:slot-label
   ;; formatting helpers shared with van-booking
   #:format-time-12h #:format-booking-day #:normalize-day-phrase #:*month-names*)
  (:documentation
   "Natural-language understanding for van booking requests.

PARSE-BOOKING turns free-form English such as

  \"Can BW and MM take the DGH to the dentist tomorrow morning, leaving at
   8:30 and back around 11?\"

into a BOOKING-REQUEST.  It runs the PROJECT-ISIDORE/NLP pipeline, then a
spaCy-style entity ruler for VEHICLE, TIME, DATE and PASSENGER entities, a cue
matcher for departure/return/destination cue words, and a slot filler that
decides which time is the departure and which the return, picks the
destination from the remaining noun material, and resolves dates and times
with CHRONICITY.  Whatever cannot be found is listed in
BOOKING-REQUEST-MISSING so the caller can ask the user to re-send the request.

BOOKING-INCOMPLETE is the condition used to tell the user exactly which pieces
are missing; its report is ready to post to Slack."))

(in-package #:project-isidore/booking-nlp)

;;; ---------------------------------------------------------------------------
;;; Formatting helpers (shared with van-booking)
;;; ---------------------------------------------------------------------------

(defparameter *month-names*
  '("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defun format-time-12h (hour minute)
  "Format HOUR (0-23) and MINUTE as a 12-hour time string, e.g. \"7:00 pm\"."
  (let* ((period (if (>= hour 12) "pm" "am"))
         (h12 (cond ((= hour 0) 12)
                    ((> hour 12) (- hour 12))
                    (t hour))))
    (format nil "~D:~2,'0D ~A" h12 minute period)))

(defun format-booking-day (dt)
  "Format a chronicity datetime DT as \"DayOfWeek MonthName Day\", e.g. \"Tuesday April 7\"."
  (format nil "~A ~A ~D"
          (string-capitalize (symbol-name (dow-name (dow-of dt))))
          (nth (1- (month-of dt)) *month-names*)
          (day-of dt)))

(defun current-year ()
  (nth-value 5 (decode-universal-time (get-universal-time))))

;;; ---------------------------------------------------------------------------
;;; Lexicons
;;; ---------------------------------------------------------------------------

(defparameter *booking-slots* '(:destination :day :passengers :dep-time :ret-time)
  "The pieces a booking needs, in report order.")

(defparameter *weekday-names*
  '("monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday"))

(defparameter *weekday-abbrevs*
  '(("mon" . "monday") ("tue" . "tuesday") ("tues" . "tuesday")
    ("wed" . "wednesday") ("weds" . "wednesday")
    ("thu" . "thursday") ("thur" . "thursday") ("thurs" . "thursday")
    ("fri" . "friday") ("sat" . "saturday") ("sun" . "sunday")))

(defparameter *month-words*
  '("january" "jan" "february" "feb" "march" "mar" "april" "apr" "may"
    "june" "jun" "july" "jul" "august" "aug" "september" "sep" "sept"
    "october" "oct" "november" "nov" "december" "dec"))

(defparameter *meridiem-words* '("am" "pm" "a.m." "p.m." "a.m" "p.m"))

(defparameter *day-parts* '("morning" "afternoon" "evening" "night"))

(defparameter *number-words*
  '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
    "eleven" "twelve"))

(defparameter *range-connectors* '("to" "-" "until" "till" "through" "thru"))

(defparameter *strip-leading*
  '("the" "a" "an" "my" "our" "your" "his" "her" "their" "some" "this" "that"
    "these" "those")
  "Determiners removed from the front of a destination candidate.")

(defparameter *strip-trailing* '("trip" "run" "outing")
  "Words removed from the end of a destination candidate (\"the SHMS trip\").")

(defparameter *run-breakers*
  '(;; pronouns, greetings, fillers
    "i" "we" "you" "me" "us" "it" "they" "them" "someone" "anyone" "somebody"
    "hello" "hi" "hey" "please" "thanks" "thank" "also" "then" "there" "here"
    "just" "so" "ok" "okay" "yes" "no" "not"
    ;; auxiliaries, modals, request verbs
    "is" "are" "be" "been" "was" "were" "will" "would" "could" "can" "should"
    "may" "might" "must" "do" "does" "did" "need" "needs" "needed" "want"
    "wants" "wanted" "like" "get" "got" "book" "booked" "booking" "reserve"
    "reserved" "schedule" "scheduled" "request" "requesting" "use" "using"
    "take" "taking" "add" "put" "sign" "up" "go" "going" "goes" "headed"
    "heading" "driving" "drive" "have" "has" "had" "let" "make"
    ;; conjunctions, prepositions (except \"of\", which joins place names)
    "and" "or" "&" "but" "that" "this" "which" "who" "what" "when" "where"
    "on" "in" "at" "from" "to" "for" "with" "by" "into" "onto" "before"
    "after" "around" "about" "near" "via"
    ;; vehicle / trip nouns and time words
    "van" "vehicle" "car" "minivan" "bus" "ride" "passenger" "passengers"
    "morning" "afternoon" "evening" "night" "tonight" "today" "tomorrow"
    "am" "pm" "o'clock")
  "Words that end a destination candidate run.")

(defparameter *excluded-run-pos* '(:prp :prp$ :md :wdt :wp :wp$ :ex :uh :cc)
  "POS tags whose tokens end a destination candidate run.")

;;; ---------------------------------------------------------------------------
;;; Entity ruler and cue matcher
;;; ---------------------------------------------------------------------------

(defun weekday-regex ()
  (format nil "^(?:~{~a~^|~})(?:'s)?$"
          (append *weekday-names* (mapcar #'car *weekday-abbrevs*))))

(defun make-booking-ruler ()
  "Build the entity ruler for booking requests."
  (let ((r (nlp:make-entity-ruler))
        (weekday (weekday-regex)))
    ;; Vehicles (a preference for the booking).
    (nlp:add-entity-patterns r :vehicle 10
                             '(((:lower ("the" "our") :op "?") (:lower "dgh")))
                             :value :dgh)
    (nlp:add-entity-patterns r :vehicle 10
                             '(((:lower ("the" "our") :op "?") (:lower "lgh")))
                             :value :lgh)
    (nlp:add-entity-patterns r :vehicle 10
                             '(((:lower ("the" "our") :op "?") (:lower ("chevy" "chevrolet"))))
                             :value :chevy)
    ;; "personal" only counts with a vehicle cue before it or a vehicle noun
    ;; after it, so "a personal matter" is not a vehicle.
    (nlp:add-entity-patterns r :vehicle 10
                             '(((:lower ("the" "use" "in" "take" "prefer"))
                                (:lower "personal")
                                (:lower ("car" "vehicle" "van") :op "?"))
                               ((:lower ("a" "my" "our" "the") :op "?")
                                (:lower "personal")
                                (:lower ("car" "vehicle" "van"))))
                             :value :personal)
    ;; Generic vehicle nouns: never a destination, never a preference.
    (nlp:add-entity-patterns r :vehicle-noun 15
                             '(((:lower ("a" "the" "our" "my") :op "?")
                                (:lower ("van" "vehicle" "car" "minivan" "bus")))))
    ;; Times.
    (nlp:add-entity-patterns
     r :time 20
     `(((:regex "^\\d{1,2}:\\d{2}(?:[ap]m|[ap]\\.m\\.?)?$") (:lower ,*meridiem-words* :op "?"))
       ((:regex "^\\d{1,2}(?:[ap]m|[ap]\\.m\\.?)$"))
       ((:regex "^\\d{1,2}$") (:lower ,*meridiem-words*))
       ((:lower ,*number-words*) (:lower ,*meridiem-words*))
       ((:lower ("noon" "midnight")))
       ;; bare hour after a time cue: "at 7", "back around 11", "to 10"
       (:trim-leading 1
        (:lower ("at" "around" "about" "by" "until" "till" "to" "from" "after" "before"))
        (:regex "^(?:[1-9]|1[0-9]|2[0-4])$"))))
    ;; Dates.  A weekday written next to the date ("Saturday September 12",
    ;; "Sat, Sep 12th", "Saturday the 12th of September") must be one span:
    ;; on its own the weekday would win as an entity and resolve to the
    ;; coming Saturday instead of the date.
    (nlp:add-entity-patterns
     r :date 30
     `(((:lower ("today" "tomorrow" "tonight")) (:lower ,*day-parts* :op "?"))
       ((:lower ("next" "this" "last") :op "?") (:regex ,weekday) (:lower "," :op "?")
        (:lower ,*month-words*) (:lower "." :op "?") (:regex "^\\d{1,2}(?:st|nd|rd|th)?$"))
       ((:lower ("next" "this" "last") :op "?") (:regex ,weekday) (:lower "," :op "?")
        (:lower "the" :op "?") (:regex "^\\d{1,2}(?:st|nd|rd|th)?$")
        (:lower "of" :op "?") (:lower ,*month-words*))
       ((:lower ("next" "this" "last") :op "?") (:lower "week") (:regex ,weekday))
       ;; "this coming Tuesday", "the upcoming Tuesday": one span, so that
       ;; "coming" is neither left over as a destination nor lost
       ((:lower ("next" "this" "last") :op "?") (:lower ("coming" "upcoming") :op "?")
        (:regex ,weekday) (:lower ,*day-parts* :op "?"))
       ((:lower "the") (:lower ("coming" "upcoming")) (:regex ,weekday)
        (:lower ,*day-parts* :op "?"))
       ((:regex ,weekday) (:lower "next") (:lower "week"))
       ((:lower ,*month-words*) (:lower "." :op "?") (:regex "^\\d{1,2}(?:st|nd|rd|th)?$"))
       ((:regex "^\\d{1,2}(?:st|nd|rd|th)?$") (:lower "of" :op "?") (:lower ,*month-words*))
       ((:regex "^\\d{4}-\\d{2}-\\d{2}$"))
       ((:regex "^\\d{1,2}/\\d{1,2}(?:/\\d{2,4})?$"))))
    ;; Passengers: 2-3 uppercase letters.
    (nlp:add-entity-patterns
     r :passenger 40
     '(((:shape ("XX" "XXX") :is-upper t
         :not-in ("AM" "PM" "OK" "FYI" "TBD" "ETA" "PS" "EOD" "DGH" "LGH")))))
    r))

(defun word-specs (words &optional tail)
  "Turn a list of literal WORDS into token specs, followed by TAIL specs."
  (append (mapcar (lambda (w) (list :lower w)) words) tail))

(defun make-cue-matcher ()
  "Build the matcher for departure / return / destination / passenger cues."
  (let ((m (nlp:make-matcher))
        (at-by '((:lower ("at" "by") :op "?"))))
    (dolist (words '(("leave") ("leaving") ("leaves") ("depart") ("departing")
                     ("departure") ("start") ("starting") ("begin") ("beginning")
                     ("pickup") ("pick" "up") ("picking" "up") ("going" "at")
                     ("go" "at") ("out" "at") ("from") ("head" "out")
                     ("heading" "out")))
      (nlp:add-pattern m :dep-cue (word-specs words at-by)))
    (dolist (words '(("be" "back") ("get" "back") ("back") ("home") ("return")
                     ("returning") ("returns") ("arrive") ("arriving") ("until")
                     ("till") ("thru") ("through") ("ending") ("end") ("done")
                     ("finish") ("finished") ("drop" "off") ("dropoff")
                     ("dropping" "off") ("by")))
      (nlp:add-pattern m :ret-cue
                       (word-specs words '((:lower ("by" "at" "around" "about") :op "?")))))
    (dolist (word *range-connectors*)
      (nlp:add-pattern m :range-connector (list (list :lower word))))
    (dolist (words '(("going" "to") ("headed" "to") ("heading" "to") ("driving" "to")
                     ("drive" "to") ("trip" "to") ("ride" "to") ("visit") ("visiting")
                     ("to") ("for") ("at")))
      (nlp:add-pattern m :dest-cue (word-specs words)))
    (dolist (words '(("with") ("carrying") ("taking") ("bringing") ("passenger")
                     ("passengers") ("for")))
      (nlp:add-pattern m :pax-cue (word-specs words)))
    m))

(defparameter *booking-ruler* (make-booking-ruler)
  "Entity ruler for VEHICLE, VEHICLE-NOUN, TIME, DATE and PASSENGER spans.")

(defparameter *cue-matcher* (make-cue-matcher)
  "Matcher for DEP-CUE, RET-CUE, RANGE-CONNECTOR, DEST-CUE and PAX-CUE spans.")

;;; ---------------------------------------------------------------------------
;;; Booking request and condition
;;; ---------------------------------------------------------------------------

(defstruct booking-request
  "What PARSE-BOOKING understood.  DAY, DEP-TIME and RET-TIME are in the
sheet's formats (\"Tuesday April 7\", \"7:00 pm\"); the *-PHRASE slots keep
the user's words.  MISSING lists the absent *BOOKING-SLOTS*; UNDERSTOOD is a
plist of the filled ones (plus :VEHICLE) for echoing back to the user."
  text doc destination day-phrase day-datetime day passengers
  dep-phrase ret-phrase dep-time ret-time vehicle missing understood)

(defmethod print-object ((req booking-request) stream)
  (print-unreadable-object (req stream :type t)
    (format stream "~s" (booking-request-understood req))))

(defparameter *slot-labels*
  '((:destination . "destination") (:day . "day") (:passengers . "passengers")
    (:dep-time . "departure time") (:ret-time . "return time")
    (:vehicle . "vehicle")))

(defparameter *missing-piece-hints*
  '((:destination . "e.g. \"for swimming\" or \"to the dentist\"")
    (:day . "e.g. \"tomorrow\", \"next Tuesday\" or \"April 22\"")
    (:passengers . "uppercase initials, e.g. \"for CG and BW\"")
    (:dep-time . "e.g. \"leaving at 8 am\"")
    (:ret-time . "e.g. \"back by 10 am\"")))

(defun slot-label (slot)
  (or (cdr (assoc slot *slot-labels*)) (string-downcase (symbol-name slot))))

(defun missing-piece-hint (slot)
  "Return the example phrasing for the booking SLOT keyword."
  (cdr (assoc slot *missing-piece-hints*)))

(defun join-with-and (strings)
  (case (length strings)
    (0 "")
    (1 (first strings))
    (2 (format nil "~a and ~a" (first strings) (second strings)))
    (t (format nil "~{~a~^, ~} and ~a" (butlast strings) (car (last strings))))))

(defun format-understood-value (slot value)
  (case slot
    (:passengers (format nil "~{~a~^, ~}" value))
    (:vehicle (string-upcase (string value)))
    (t (princ-to-string value))))

(define-condition booking-incomplete (error)
  ((missing :initarg :missing :reader booking-incomplete-missing)
   (understood :initarg :understood :initform nil :reader booking-incomplete-understood)
   (text :initarg :text :initform nil :reader booking-incomplete-text)
   (preamble :initarg :preamble :initform nil :reader booking-incomplete-preamble))
  (:documentation
   "Signalled when a booking cannot proceed because MISSING pieces (keywords
from *BOOKING-SLOTS*) were not found.  UNDERSTOOD is a plist of what was
found; PREAMBLE is an optional first line of context.  The report is a
Slack-ready message asking the user to re-send the request.")
  (:report report-booking-incomplete))

(defun report-booking-incomplete (condition stream)
  (let* ((missing (booking-incomplete-missing condition))
         (understood (booking-incomplete-understood condition))
         (needed (join-with-and (mapcar (lambda (slot) (format nil "the ~a" (slot-label slot)))
                                        missing)))
         (known (loop for slot in (append *booking-slots* '(:vehicle))
                      for value = (getf understood slot)
                      when value collect (cons slot value))))
    (when (booking-incomplete-preamble condition)
      (format stream "~a~%" (booking-incomplete-preamble condition)))
    (format stream "I couldn't complete that booking — I still need ~a.~%" needed)
    (when known
      (format stream "~%What I understood:~%")
      (dolist (kv known)
        (format stream "  • ~a: ~a~%"
                (slot-label (car kv)) (format-understood-value (car kv) (cdr kv)))))
    (format stream "~%How to phrase the missing pieces:~%")
    (dolist (slot missing)
      (format stream "  • ~a — ~a~%" (slot-label slot) (missing-piece-hint slot)))
    (format stream "~%Please re-send your request including ~a." needed)))

;;; ---------------------------------------------------------------------------
;;; Slot filling
;;; ---------------------------------------------------------------------------

(defun ents-with-label (doc label)
  (remove-if-not (lambda (e) (eq (nlp:span-label e) label)) (nlp:doc-ents doc)))

(defun covered-p (index spans)
  (some (lambda (s) (nlp:span-covers-p s index)) spans))

(defun promote-range-starts (doc)
  "Add :TIME entities for bare numbers directly before a range connector that
precedes a TIME (\"7-9pm\", \"7 to 9 pm\").  The new span inherits the
following time's meridiem via :INHERIT-MERIDIEM-FROM in its data."
  (let ((ents (nlp:doc-ents doc))
        (new '()))
    (dolist (e ents)
      (when (eq (nlp:span-label e) :time)
        (let* ((s (nlp:span-start e))
               (conn (nlp:doc-token doc (1- s)))
               (num (nlp:doc-token doc (- s 2))))
          (when (and conn num
                     (member (nlp:token-lower conn) *range-connectors* :test #'string=)
                     (cl-ppcre:scan "^(?:[1-9]|1[0-2])$" (nlp:token-text num))
                     (not (covered-p (- s 2) ents))
                     (not (covered-p (- s 2) new)))
            (push (nlp:make-span :doc doc :start (- s 2) :end (1- s) :label :time
                                 :data (list :priority 20 :inherit-meridiem-from e))
                  new)))))
    (when new
      (setf (nlp:doc-ents doc)
            (sort (append new ents) #'< :key #'nlp:span-start)))
    (nlp:doc-ents doc)))

;;; Times

(defun time-phrase-meridiem (text)
  (cond ((cl-ppcre:scan "(?i)(?:p\\.?m\\.?|noon)$" text) :pm)
        ((cl-ppcre:scan "(?i)(?:a\\.?m\\.?|midnight)$" text) :am)
        (t nil)))

(defun resolve-time-span (span)
  "Return (values MINUTES-SINCE-MIDNIGHT HAS-MERIDIEM-P PHRASE) for a :TIME
span, or NIL when chronicity cannot read it."
  (let* ((text (nlp:span-text span))
         (inherit (getf (nlp:span-data span) :inherit-meridiem-from))
         (own (time-phrase-meridiem text))
         (meridiem (or own (and inherit (time-phrase-meridiem (nlp:span-text inherit)))))
         (phrase (if (and meridiem (not own))
                     (format nil "~a ~a" text (string-downcase (symbol-name meridiem)))
                     text))
         (dt (ignore-errors (chronicity:parse phrase :context :future))))
    (when dt
      (values (+ (* 60 (hour-of dt)) (minute-of dt)) (and meridiem t) phrase))))

(defun cue-in-window-p (cues label window-start window-end)
  (some (lambda (c) (and (eq (nlp:span-label c) label)
                         (>= (nlp:span-start c) window-start)
                         (<= (nlp:span-end c) window-end)))
        cues))

(defun assign-time-roles (doc cues)
  "Decide which :TIME entities are the departure and the return.
A time is the departure when a DEP-CUE precedes it (since the previous
entity), the return when a RET-CUE does or when a RANGE-CONNECTOR links it to
an earlier time.  Times left over are assigned in text order to whichever
role is still free, so \"8 am and 10 am\" reads as departure then return and
a lone \"back by 10 am\" leaves the departure missing.
Returns (values DEP-SPAN RET-SPAN)."
  (let ((prev-end 0)
        (seen-time nil)
        (dep nil) (ret nil) (free '()))
    (dolist (e (nlp:doc-ents doc))
      (when (eq (nlp:span-label e) :time)
        (let* ((start (nlp:span-start e))
               (role (cond ((cue-in-window-p cues :dep-cue prev-end start) :dep)
                           ((cue-in-window-p cues :ret-cue prev-end start) :ret)
                           ((and seen-time
                                 (cue-in-window-p cues :range-connector prev-end start))
                            :ret)
                           (t nil))))
          (case role
            (:dep (if dep (push e free) (setf dep e)))
            (:ret (if ret (push e free) (setf ret e)))
            (t (push e free)))
          (setf seen-time t)))
      (setf prev-end (nlp:span-end e)))
    (dolist (e (nreverse free))
      (cond ((null dep) (setf dep e))
            ((null ret) (setf ret e))))
    (values dep ret)))

(defun resolve-times (dep-span ret-span)
  "Resolve the departure/return spans to \"H:MM am\" strings, inferring a
missing meridiem when the return would otherwise precede the departure.
Returns (values DEP-TIME RET-TIME DEP-PHRASE RET-PHRASE)."
  (multiple-value-bind (dep-min dep-mer dep-phrase)
      (and dep-span (resolve-time-span dep-span))
    (multiple-value-bind (ret-min ret-mer ret-phrase)
        (and ret-span (resolve-time-span ret-span))
      (when (and dep-min (not dep-mer) (<= 1 (floor dep-min 60) 5))
        (incf dep-min 720))
      (when (and dep-min ret-min (not ret-mer) (<= ret-min dep-min))
        (setf ret-min (mod (+ ret-min 720) 1440)))
      (values (and dep-min (format-time-12h (floor dep-min 60) (mod dep-min 60)))
              (and ret-min (format-time-12h (floor ret-min 60) (mod ret-min 60)))
              dep-phrase ret-phrase))))

;;; Days

(defun month-number (word)
  (let ((w (string-downcase (string-right-trim "." word))))
    (loop for name in *month-names*
          for i from 1
          when (and (>= (length name) (length w)) (>= (length w) 3)
                    (string-equal w name :end2 (length w)))
            return i)))

(defun normalize-day-phrase (text)
  "Rewrite a :DATE span's text into a form chronicity reads reliably:
possessives, commas, periods, day parts and \"coming\"/\"upcoming\" dropped
(with a \"the\" then left standing before a bare weekday: \"the coming
Tuesday\" becomes \"Tuesday\"), weekday abbreviations expanded, slash dates
spelled out, a day-first month-day turned round (\"22nd of April\" becomes
\"April 22nd\", the only order chronicity reads), explicit month-day phrases
anchored to the current year (the sheet only ever covers the current year),
and a weekday dropped when it accompanies a month-day (\"Saturday September
12\" becomes \"September 12\": chronicity would otherwise take the weekday
alone and answer the coming Saturday).  VAN-BOOKING's PHRASE-DATE-RANGE puts `/view` ranges through this
too, so a day means the same thing in a booking and in a view."
  (let* ((s (cl-ppcre:regex-replace-all "(?i)'s\\b" text ""))
         (words (cl-ppcre:split "\\s+" (string-trim " " s)))
         (words (remove "" (mapcar (lambda (w) (string-trim ".," w)) words)
                        :test #'string=))
         (words (mapcar (lambda (w)
                          (let ((abbr (assoc (string-downcase w) *weekday-abbrevs*
                                             :test #'string=)))
                            (if abbr (cdr abbr) w)))
                        words))
         (words (remove-if (lambda (w) (member (string-downcase w)
                                               (append *day-parts* '("coming" "upcoming"))
                                               :test #'string=))
                           words))
         (words (if (and (rest words) (string-equal (first words) "the"))
                    (rest words)
                    words))
         (words (if (some #'month-number words)
                    (remove-if (lambda (w)
                                 (member (string-downcase w)
                                         (append *weekday-names* '("next" "this" "last" "the"))
                                         :test #'string=))
                               words)
                    words))
         (phrase (format nil "~{~a~^ ~}" words)))
    ;; "4/22" or "4/22/2026" -> "April 22 2026"
    (cl-ppcre:register-groups-bind (m d y)
        ("^(\\d{1,2})/(\\d{1,2})(?:/(\\d{2,4}))?$" phrase)
      (let ((month (parse-integer m)))
        (when (<= 1 month 12)
          (setf phrase (format nil "~a ~a ~a" (nth (1- month) *month-names*) d
                               (cond ((null y) (current-year))
                                     ((= (length y) 2) (+ 2000 (parse-integer y)))
                                     (t y)))))))
    ;; "22nd of April" -> "April 22nd": chronicity reads only the month-first order
    (cl-ppcre:register-groups-bind (d m)
        ("(?i)^(\\d{1,2}(?:st|nd|rd|th)?) of (\\S+)$" phrase)
      (when (month-number m)
        (setf phrase (format nil "~a ~a" m d))))
    ;; "Tuesday next week" -> "next week Tuesday"
    (cl-ppcre:register-groups-bind (wd) ("(?i)^(\\w+) next week$" phrase)
      (setf phrase (format nil "next week ~a" wd)))
    ;; explicit month-day without a year -> current year
    (when (and (some #'month-number words)
               (some (lambda (w) (cl-ppcre:scan "^\\d{1,2}(?:st|nd|rd|th)?$" w)) words)
               (not (cl-ppcre:scan "\\d{4}" phrase)))
      (setf phrase (format nil "~a ~d" phrase (current-year))))
    phrase))

(defun resolve-day-span (span)
  "Return (values DATETIME DAY-STRING PHRASE) for a :DATE span, or NIL."
  (let* ((text (nlp:span-text span))
         (phrase (normalize-day-phrase text))
         (dt (ignore-errors (chronicity:parse phrase :context :future))))
    (when dt
      (values dt (format-booking-day dt) text))))

;;; Destination

(defun run-token-ok-p (token index ents cues)
  (and (not (covered-p index ents))
       (not (covered-p index cues))
       (not (nlp:token-flag-p token :is-punct))
       (not (member (nlp:token-lower token) *run-breakers* :test #'string=))
       (not (member (nlp:token-pos token) *excluded-run-pos*))))

(defun candidate-runs (doc ents cues)
  "Return maximal (START . END) token ranges available as destination text."
  (let ((tokens (nlp:doc-tokens doc))
        (runs '())
        (start nil))
    (loop for i from 0 below (length tokens)
          do (if (run-token-ok-p (aref tokens i) i ents cues)
                 (unless start (setf start i))
                 (when start
                   (push (cons start i) runs)
                   (setf start nil))))
    (when start (push (cons start (length tokens)) runs))
    (nreverse runs)))

(defun strip-run (doc run)
  "Strip leading determiners and trailing trip-words from RUN.  Returns
(START . END) or NIL when nothing is left."
  (let ((s (car run)) (e (cdr run)))
    (loop while (and (< s e)
                     (member (nlp:token-lower (nlp:doc-token doc s)) *strip-leading*
                             :test #'string=))
          do (incf s))
    (loop while (and (< s e)
                     (member (nlp:token-lower (nlp:doc-token doc (1- e))) *strip-trailing*
                             :test #'string=))
          do (decf e))
    (when (< s e) (cons s e))))

(defun dest-cue-before (doc index cues)
  "Return the :DEST-CUE span ending right before INDEX, skipping determiners."
  (let ((i index))
    (loop while (and (> i 0)
                     (member (nlp:token-lower (nlp:doc-token doc (1- i))) *strip-leading*
                             :test #'string=))
          do (decf i))
    (find-if (lambda (c) (and (eq (nlp:span-label c) :dest-cue) (= (nlp:span-end c) i)))
             cues)))

(defun score-run (doc run stripped cues)
  (let* ((tokens (loop for i from (car stripped) below (cdr stripped)
                       collect (nlp:doc-token doc i)))
         (cue (dest-cue-before doc (car run) cues))
         (score 0))
    (when cue
      (incf score (if (string-equal (nlp:span-text cue) "at") 2 3)))
    (when (some (lambda (chunk) (and (< (nlp:span-start chunk) (cdr stripped))
                                     (< (car stripped) (nlp:span-end chunk))))
                (nlp:doc-chunks doc))
      (incf score 1))
    (when (some (lambda (tk) (member (nlp:token-pos tk) '(:vbg :nnp :nnps :nn :nns))) tokens)
      (incf score 1))
    (when (and (= (length tokens) 1)
               (member (nlp:token-pos (first tokens)) '(:vb :vbp :vbd :vbz)))
      (decf score 2))
    (values score (and cue t))))

(defun select-destination (doc ents cues other-slots-filled-p)
  "Pick the destination text from DOC.  A gazetteer :DESTINATION entity wins
outright; otherwise the best-scoring candidate run is used.  An uncued
candidate is only accepted when some other slot was filled."
  (let ((gazetteer (first (ents-with-label doc :destination))))
    (when gazetteer
      (return-from select-destination
        (or (getf (nlp:span-data gazetteer) :value) (nlp:span-text gazetteer)))))
  (let ((best nil) (best-score nil) (best-cued nil))
    (dolist (run (candidate-runs doc ents cues))
      (let ((stripped (strip-run doc run)))
        (when (and stripped
                   (notevery (lambda (i) (nlp:token-flag-p (nlp:doc-token doc i) :like-num))
                             (loop for i from (car stripped) below (cdr stripped) collect i)))
          (multiple-value-bind (score cued) (score-run doc run stripped cues)
            (when (or (null best-score) (> score best-score))
              (setf best stripped best-score score best-cued cued))))))
    (when (and best (or best-cued other-slots-filled-p))
      (format nil "~{~a~^ ~}"
              (loop for i from (car best) below (cdr best)
                    collect (nlp:token-text (nlp:doc-token doc i)))))))

;;; Entry point

(defun gazetteer-spans (doc known-destinations)
  (when known-destinations
    (mapcar (lambda (s)
              (setf (nlp:span-data s) (list* :priority 0 (nlp:span-data s)))
              s)
            (nlp:run-phrase-matcher
             (nlp:make-phrase-matcher known-destinations :label :destination)
             doc))))

(defun parse-booking (text &key known-destinations)
  "Parse free-form booking TEXT into a BOOKING-REQUEST.  KNOWN-DESTINATIONS
is an optional list of destination strings already on the sheet; when one
appears in TEXT it is used verbatim so the booking joins the existing trip."
  (let* ((doc (nlp:analyze text))
         (ents (progn
                 (nlp:apply-entity-ruler *booking-ruler* doc
                                         :extra-spans (gazetteer-spans doc known-destinations))
                 (promote-range-starts doc)))
         (cues (nlp:run-matcher *cue-matcher* doc))
         (passengers (remove-duplicates
                      (mapcar (lambda (e) (string-upcase (nlp:span-text e)))
                              (ents-with-label doc :passenger))
                      :test #'string= :from-end t))
         (vehicle (let ((v (first (ents-with-label doc :vehicle))))
                    (and v (getf (nlp:span-data v) :value))))
         (day-span (first (ents-with-label doc :date))))
    (multiple-value-bind (day-dt day day-phrase)
        (and day-span (resolve-day-span day-span))
      (multiple-value-bind (dep-span ret-span) (assign-time-roles doc cues)
        (multiple-value-bind (dep-time ret-time dep-phrase ret-phrase)
            (resolve-times dep-span ret-span)
          (let* ((destination (select-destination
                               doc ents cues
                               (or day passengers dep-time ret-time)))
                 (values (list :destination destination :day day
                               :passengers passengers :dep-time dep-time
                               :ret-time ret-time))
                 (missing (loop for slot in *booking-slots*
                                unless (getf values slot) collect slot))
                 (understood (append
                              (loop for slot in *booking-slots*
                                    for v = (getf values slot)
                                    when v append (list slot v))
                              (when vehicle (list :vehicle vehicle)))))
            (make-booking-request
             :text text :doc doc :destination destination
             :day-phrase day-phrase :day-datetime day-dt :day day
             :passengers passengers
             :dep-phrase dep-phrase :ret-phrase ret-phrase
             :dep-time dep-time :ret-time ret-time
             :vehicle vehicle :missing missing :understood understood)))))))

(defparameter *timing-question-regex*
  "(?i)(?<![a-z])(?:when(?:'s|s)?|what\\s+time(?:'s|s)?|how\\s+(?:late|early|long))(?![a-z])"
  "How a message asks about the time of a trip instead of asking for one:
\"when is my ride home\", \"what time does the van leave\", \"when's the DGH
back\", \"how late is the van out\".")

(defun schedule-question-range (text)
  "The `/view` range TEXT asks about, when TEXT is a question about a trip's
timing rather than a request for one (*TIMING-QUESTION-REGEX*); NIL otherwise.

  \"when is my ride home this coming Tuesday?\"  -> \"this coming Tuesday\"
  \"what time does the van leave tomorrow?\"    -> \"tomorrow\"
  \"when's the DGH back?\"                       -> \"today\"

The day comes back in the message's own words -- the text of the :DATE span
PARSE-BOOKING reads -- or as \"today\" when none is named, the default `/view`
has.  A question that names passengers (\"when can BW and MM have the van for
the dentist tomorrow?\") is a booking phrased as a question and gets NIL, as
does one whose day chronicity cannot read."
  (when (cl-ppcre:scan *timing-question-regex* text)
    (let ((req (parse-booking text)))
      (unless (booking-request-passengers req)
        (if (ents-with-label (booking-request-doc req) :date)
            (booking-request-day-phrase req)
            "today")))))
