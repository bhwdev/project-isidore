;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/slack-bot/booking-nlp
  (:use #:common-lisp)
  (:local-nicknames (#:nlp #:project-isidore/slack-bot/nlp))
  (:import-from #:cl-ppcre)
  (:import-from #:chronicity
                #:hour-of #:minute-of #:dow-of #:dow-name
                #:year-of #:month-of #:day-of)
  (:export
   ;; parsing
   #:parse-booking #:revise-booking #:possible-initials
   #:self-reference-p #:resolve-self-reference
   #:*self-words* #:*self-contractions*
   #:booking-request #:booking-request-p #:make-booking-request
   #:booking-request-text #:booking-request-doc
   #:booking-request-destination
   #:booking-request-day-phrase #:booking-request-day-datetime #:booking-request-day
   #:booking-request-passengers
   #:booking-request-dep-phrase #:booking-request-ret-phrase
   #:booking-request-dep-time #:booking-request-ret-time
   #:booking-request-vehicle
   #:booking-request-missing #:booking-request-understood
   #:*booking-slots* #:*booking-ruler* #:*cue-matcher* #:*not-initials*
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

into a BOOKING-REQUEST.  It runs the project-isidore/slack-bot/NLP pipeline, then a
spaCy-style entity ruler for VEHICLE, TIME, DATE and PASSENGER entities, a cue
matcher for departure/return/destination cue words, and a slot filler that
decides which time is the departure and which the return, picks the
destination from the remaining noun material, and resolves dates and times
with CHRONICITY.  Whatever cannot be found is listed in
BOOKING-REQUEST-MISSING so the caller can ask the user to re-send the request.
REVISE-BOOKING lays a correction (\"8 to 10 pm\", \"take the LGH\") over a
request already parsed, for the `edit` answer to a booking's confirmation.

BOOKING-INCOMPLETE is the condition used to tell the user exactly which pieces
are missing; its report is ready to post to Slack."))

(in-package #:project-isidore/slack-bot/booking-nlp)

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

(defparameter *oclock-words* '("o'clock" "o’clock" "oclock")
  "How \"o'clock\" is written after an hour: \"9 o'clock\", \"nine o’clock pm\".")

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

(defparameter *not-initials*
  (sort
   (remove-duplicates
    (append
     '("AM" "PM" "OK" "FYI" "TBD" "ETA" "PS" "EOD"
       ;; The vehicles, written in the same shape as a person.
       "DGH" "LGH"
       ;; ME and MY have a meaning of their own: SLACK-BOT resolves them to
       ;; the sender's initials before parsing (RESOLVE-SELF-REFERENCE).  One
       ;; that reaches the parser belongs to a sender who could not be
       ;; identified, and must not become a passenger of that name.
       "ME" "MY")
     ;; Every short word this file already knows is not a name.  The ruler
     ;; tells initials from ordinary words by their capitals, which says
     ;; nothing in a message typed entirely in capitals: without these,
     ;; "BOOK ME A VAN TO THE POOL" is read as a trip for TO and THE, and
     ;; "SEP 12" adds a passenger named SEP.  Taking them from the lists the
     ;; destination and date scanners already use keeps one set of words to
     ;; maintain rather than several that drift apart.
     (mapcar #'string-upcase
             (remove-if-not
              (lambda (w) (and (<= 2 (length w) 3) (every #'alpha-char-p w)))
              (append *run-breakers* *strip-leading* *strip-trailing*
                      *range-connectors* *day-parts* *month-words*
                      *weekday-names* *number-words* *meridiem-words*
                      (mapcar #'car *weekday-abbrevs*)))))
    :test #'string=)
   #'string<)
  "Uppercase words MAKE-BOOKING-RULER never reads as passenger initials, nor
KNOWN-INITIAL-SPANS in any case.  Anyone whose real initials collide with an
entry here has to be added to a trip by someone else naming them, the same
trade-off DGH and LGH have always carried.")

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
       ;; "9 o'clock", "nine o'clock p.m."
       ((:regex "^\\d{1,2}$") (:lower ,*oclock-words*) (:lower ,*meridiem-words* :op "?"))
       ((:lower ,*number-words*) (:lower ,*oclock-words*) (:lower ,*meridiem-words* :op "?"))
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
    ;; Passengers: 2-3 uppercase letters that are not a word the sentence
    ;; needs for something else (see *NOT-INITIALS*).
    (nlp:add-entity-patterns
     r :passenger 40
     `(((:shape ("XX" "XXX") :is-upper t
         :not-in ,*not-initials*))))
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
    (:passengers . "initials, e.g. \"for CG and BW\"; lowercase works for anyone already on the schedule, capitals for anyone new")
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
    (format stream "Nothing is booked yet — I still need ~a.~%" needed)
    (when known
      (format stream "~%What I understood:~%")
      (dolist (kv known)
        (format stream "  • ~a: ~a~%"
                (slot-label (car kv)) (format-understood-value (car kv) (cdr kv)))))
    (format stream "~%How to phrase the missing pieces:~%")
    (dolist (slot missing)
      (format stream "  • ~a — ~a~%" (slot-label slot) (missing-piece-hint slot)))
    (format stream "~%Please re-send the whole `/book` request with ~a added." needed)))

;;; ---------------------------------------------------------------------------
;;; Slot filling
;;; ---------------------------------------------------------------------------

(defun ents-with-label (doc label)
  (remove-if-not (lambda (e) (eq (nlp:span-label e) label)) (nlp:doc-ents doc)))

(defun covered-p (index spans)
  (some (lambda (s) (nlp:span-covers-p s index)) spans))

(defun promote-range-starts (doc)
  "Give the start of a time range the meridiem of its end: \"7-9pm\", \"7 to 9
pm\", \"from 8 to 9 pm\".  A bare number directly before a range connector
that precedes a TIME becomes a :TIME entity; a TIME already standing there
\(\"from 8\" is one) is kept when it says neither am nor pm itself.  Either
way the span's data gets :INHERIT-MERIDIEM-FROM, the time that follows."
  (let ((ents (nlp:doc-ents doc))
        (new '()))
    (dolist (e ents)
      (when (eq (nlp:span-label e) :time)
        (let* ((s (nlp:span-start e))
               (conn (nlp:doc-token doc (1- s)))
               (num (nlp:doc-token doc (- s 2)))
               (before (find-if (lambda (p)
                                  (and (eq (nlp:span-label p) :time)
                                       (= (nlp:span-end p) (1- s))))
                                ents)))
          (when (and conn num
                     (member (nlp:token-lower conn) *range-connectors* :test #'string=))
            (cond
              (before
               (unless (time-phrase-meridiem (nlp:span-text before))
                 (setf (getf (nlp:span-data before) :inherit-meridiem-from) e)))
              ((and (cl-ppcre:scan "^(?:[1-9]|1[0-2])$" (nlp:token-text num))
                    (not (covered-p (- s 2) ents))
                    (not (covered-p (- s 2) new)))
               (push (nlp:make-span :doc doc :start (- s 2) :end (1- s) :label :time
                                    :data (list :priority 20 :inherit-meridiem-from e))
                     new)))))))
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
  "Return (values MINUTES-SINCE-MIDNIGHT MERIDIEM-SOURCE PHRASE) for a :TIME
span, or NIL when chronicity cannot read it.  MERIDIEM-SOURCE is :OWN when
the span says am or pm itself, :INHERITED when it took that from the time it
ranges to (PROMOTE-RANGE-STARTS), and NIL when it has neither."
  (let* ((text (nlp:span-text span))
         (inherit (getf (nlp:span-data span) :inherit-meridiem-from))
         (own (time-phrase-meridiem text))
         (meridiem (or own (and inherit (time-phrase-meridiem (nlp:span-text inherit)))))
         (phrase (if (and meridiem (not own))
                     (format nil "~a ~a" text (string-downcase (symbol-name meridiem)))
                     text))
         ;; chronicity reads "9 pm" but not "9 o'clock pm"
         (dt (ignore-errors
              (chronicity:parse (cl-ppcre:regex-replace-all "(?i)\\s*o['’]?clock" phrase "")
                                :context :future))))
    (when dt
      (values (+ (* 60 (hour-of dt)) (minute-of dt))
              (cond (own :own) (meridiem :inherited))
              phrase))))

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
An am or pm the departure only took from the return (\"7 to 9 pm\") is given
up when it would put the departure at or after the return: \"11 to 1 pm\"
leaves at 11 am.
Returns (values DEP-TIME RET-TIME DEP-PHRASE RET-PHRASE)."
  (multiple-value-bind (dep-min dep-mer dep-phrase)
      (and dep-span (resolve-time-span dep-span))
    (multiple-value-bind (ret-min ret-mer ret-phrase)
        (and ret-span (resolve-time-span ret-span))
      (when (and dep-min ret-min (eq dep-mer :inherited) (>= dep-min ret-min))
        (setf dep-min (mod (+ dep-min 720) 1440)))
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
       (not (member (nlp:token-lower token) *run-breakers* :test #'string=))))

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

(defun score-run (doc run cues)
  "Score the candidate RUN by the destination cue right before it: 3 after one
such as \"to\" or \"visiting\", 2 after \"at\" (which also introduces
times), 0 when uncued.  Returns (values SCORE CUED-P)."
  (let ((cue (dest-cue-before doc (car run) cues)))
    (values (cond ((null cue) 0)
                  ((string-equal (nlp:span-text cue) "at") 2)
                  (t 3))
            (and cue t))))

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
          (multiple-value-bind (score cued) (score-run doc run cues)
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

(defun initial-shaped-p (token)
  "True when TOKEN could be someone's initials written without capitals: two
or three letters, not all uppercase (the ruler reads those itself), and not
one of the words *NOT-INITIALS* lists."
  (let ((text (nlp:token-text token)))
    (and (<= 2 (length text) 3)
         (nlp:token-flag-p token :is-alpha)
         (not (nlp:token-flag-p token :is-upper))
         (not (member (string-upcase text) *not-initials* :test #'string=)))))

(defun known-initial-spans (doc known-initials)
  "Passenger spans for the words of DOC that are KNOWN-INITIALS (uppercase
strings) written in lowercase or mixed case: \"bw and mm\", \"Dmm\".  A
lowercase word is read as initials only when somebody goes by them, so
\"gym\" or \"zoo\" stays a destination."
  (when known-initials
    (loop for token across (nlp:doc-tokens doc)
          for i from 0
          when (and (initial-shaped-p token)
                    (member (string-upcase (nlp:token-text token)) known-initials
                            :test #'string=))
            collect (nlp:make-span :doc doc :start i :end (1+ i) :label :passenger
                                   :data (list :priority 40)))))

(defun possible-initials (text)
  "The words of TEXT that would be read as passengers if somebody went by
them (INITIAL-SHAPED-P): \"bw\" and \"mm\" in \"bw and mm to the pool\".
BOOK-NATURAL reads the sheet's initials before reporting a request
incomplete only when there are some."
  (loop for token across (nlp:doc-tokens (nlp:make-doc text))
        when (initial-shaped-p token)
          collect (nlp:token-text token)))

(defun summarize-request (request)
  "Fill in REQUEST's MISSING and UNDERSTOOD from its other slots, and return
REQUEST."
  (let ((values (list :destination (booking-request-destination request)
                      :day (booking-request-day request)
                      :passengers (booking-request-passengers request)
                      :dep-time (booking-request-dep-time request)
                      :ret-time (booking-request-ret-time request)))
        (vehicle (booking-request-vehicle request)))
    (setf (booking-request-missing request)
          (loop for slot in *booking-slots*
                unless (getf values slot) collect slot)
          (booking-request-understood request)
          (append (loop for slot in *booking-slots*
                        for v = (getf values slot)
                        when v append (list slot v))
                  (when vehicle (list :vehicle vehicle))))
    request))

(defun parse-booking (text &key known-destinations known-initials fragment)
  "Parse free-form booking TEXT into a BOOKING-REQUEST.  KNOWN-DESTINATIONS
is an optional list of destination strings already on the sheet; when one
appears in TEXT it is used verbatim so the booking joins the existing trip.
KNOWN-INITIALS is an optional list of the initials people go by, in
uppercase; any of them is read as a passenger in lowercase too (see
KNOWN-INITIAL-SPANS).  Without it, initials have to be in capitals.
FRAGMENT says TEXT corrects a request rather than making one (REVISE-BOOKING):
its destination is then taken only when a cue such as \"to\" or \"for\"
introduces it, because the words left over in a correction (\"instead\",
\"change the time\") are not one."
  (let* ((doc (nlp:make-doc text))
         (ents (progn
                 (nlp:apply-entity-ruler *booking-ruler* doc
                                         :extra-spans (append
                                                       (gazetteer-spans doc known-destinations)
                                                       (known-initial-spans doc known-initials)))
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
          (summarize-request
           (make-booking-request
            :text text :doc doc
            :destination (select-destination
                          doc ents cues
                          (and (not fragment)
                               (or day passengers dep-time ret-time)))
            :day-phrase day-phrase :day-datetime day-dt :day day
            :passengers passengers
            :dep-phrase dep-phrase :ret-phrase ret-phrase
            :dep-time dep-time :ret-time ret-time
            :vehicle vehicle)))))))

(defun revised-times (request edit)
  "REQUEST's departure and return once EDIT's are laid over it, as (values
DEP-TIME RET-TIME DEP-PHRASE RET-PHRASE).  When EDIT gives one time and
REQUEST has the other, the two are read together, as \"DEP to RET\" in one
request would be (RESOLVE-TIMES), so a time without am or pm takes the
meaning that fits: after a 7 pm departure \"back at 10\" is 10 pm, and before
a 9 pm return \"leaving at 8\" is 8 pm.  A pair that cannot be read that way
keeps EDIT's own reading."
  (let ((dep-phrase (and (booking-request-dep-time edit) (booking-request-dep-phrase edit)))
        (ret-phrase (and (booking-request-ret-time edit) (booking-request-ret-phrase edit)))
        (old-dep (booking-request-dep-time request))
        (old-ret (booking-request-ret-time request)))
    (flet ((laid-over ()
             (values (or (booking-request-dep-time edit) old-dep)
                     (or (booking-request-ret-time edit) old-ret)
                     (or dep-phrase (booking-request-dep-phrase request))
                     (or ret-phrase (booking-request-ret-phrase request)))))
      (if (and (or dep-phrase ret-phrase) (not (and dep-phrase ret-phrase))
               old-dep old-ret)
          (let* ((dep (or dep-phrase old-dep))
                 (ret (or ret-phrase old-ret))
                 (pair (parse-booking (format nil "~a to ~a" dep ret))))
            (if (and (booking-request-dep-time pair) (booking-request-ret-time pair))
                (values (booking-request-dep-time pair) (booking-request-ret-time pair)
                        dep ret)
                (laid-over)))
          (laid-over)))))

(defun revise-booking (request text &key known-destinations known-initials)
  "REQUEST, a BOOKING-REQUEST, with the changes TEXT asks for: TEXT is what
followed `edit` in the answer to a booking's confirmation, such as \"8 to 10
pm\", \"take the LGH\", \"Friday\" or \"for BW and GG\".  Each piece TEXT
names replaces REQUEST's and the rest are kept.  Passengers it names replace
the list, unless TEXT begins with \"add\", when they join it.  Its
destination needs a cue (PARSE-BOOKING's FRAGMENT), and a lone time is read
against the one it pairs with (REVISED-TIMES).  KNOWN-DESTINATIONS and
KNOWN-INITIALS are as for PARSE-BOOKING.
Returns (values REVISED CHANGED): CHANGED lists the pieces TEXT named, from
*BOOKING-SLOTS* and :VEHICLE.  When it named none, REVISED is REQUEST and
CHANGED is NIL."
  (let* ((edit (parse-booking text :known-destinations known-destinations
                                   :known-initials known-initials
                                   :fragment t))
         (changed (loop for slot in (append *booking-slots* '(:vehicle))
                        when (getf (booking-request-understood edit) slot)
                          collect slot)))
    (if (null changed)
        (values request nil)
        (let ((revised (copy-booking-request request)))
          (when (booking-request-destination edit)
            (setf (booking-request-destination revised) (booking-request-destination edit)))
          (when (booking-request-day edit)
            (setf (booking-request-day revised) (booking-request-day edit)
                  (booking-request-day-phrase revised) (booking-request-day-phrase edit)
                  (booking-request-day-datetime revised) (booking-request-day-datetime edit)))
          (when (booking-request-passengers edit)
            (setf (booking-request-passengers revised)
                  (if (cl-ppcre:scan "(?i)^\\s*add\\b" text)
                      (remove-duplicates (append (booking-request-passengers request)
                                                 (booking-request-passengers edit))
                                         :test #'string= :from-end t)
                      (booking-request-passengers edit))))
          (when (booking-request-vehicle edit)
            (setf (booking-request-vehicle revised) (booking-request-vehicle edit)))
          (multiple-value-bind (dep-time ret-time dep-phrase ret-phrase)
              (revised-times request edit)
            (setf (booking-request-dep-time revised) dep-time
                  (booking-request-ret-time revised) ret-time
                  (booking-request-dep-phrase revised) dep-phrase
                  (booking-request-ret-phrase revised) ret-phrase))
          (values (summarize-request revised) changed)))))

(defparameter *self-contractions*
  '(("i'm"   . "~a is")
    ("i’m"   . "~a is")
    ("i'd"   . "~a would")
    ("i’d"   . "~a would")
    ("i'll"  . "~a will")
    ("i’ll"  . "~a will")
    ("i've"  . "~a has")
    ("i’ve"  . "~a has"))
  "Contractions of \"I\" and the clause each becomes once the sender's
initials are known, as a FORMAT control taking those initials.  Written out
rather than left to the bare-\"I\" rule so the rewrite stays a sentence:
\"I'd like a van\" becomes \"DMM would like a van\", not \"DMM'd like a van\",
whose tokenizer output no longer looks like initials.  Both the ASCII
apostrophe and the typographic one Slack's phone clients insert are listed.")

(defparameter *self-words* '("me" "my" "myself" "i")
  "First-person singular words that name the sender.  \"my\" is included for
\"my ride to SHMS\": the parser reads the rewrite \"DMM ride to SHMS\" as a
passenger and a destination, which is what the message meant.  Plurals
\(\"us\", \"we\") are deliberately absent -- they name a group whose other
members the message does not give, so resolving them to the sender alone
would silently under-book the trip.")

(defparameter *self-reference-scanner*
  (cl-ppcre:create-scanner
   (format nil "(?i)(?<![a-z'’])(?:~{~a~^|~})(?![a-z'’])"
           (append (mapcar (lambda (c) (cl-ppcre:quote-meta-chars (car c)))
                           *self-contractions*)
                   *self-words*)))
  "Matches any *SELF-CONTRACTIONS* key or *SELF-WORDS* word standing alone.
The contractions are listed first so \"i'm\" wins over the bare \"i\" that
also starts it.  The lookarounds keep \"me\" out of \"meet\" and \"home\",
\"my\" out of \"myself\" (the following \"s\" fails the lookahead, so the
longer word matches on its own), and bare \"i\" out of \"i'd\".")

(defun self-reference-p (text)
  "True when TEXT refers to its sender in the first person singular.
SLACK-BOT calls this before resolving an identity, so a message that spells
its passengers out costs no database read and no Slack API call."
  (and (stringp text)
       (cl-ppcre:scan *self-reference-scanner* text)
       t))

(defun resolve-self-reference (text initial)
  "Return TEXT with every first-person singular reference to the sender
replaced by INITIAL, so the ordinary parsers can read it knowing nothing
about who is speaking:

  \"book me a van to the dentist tomorrow 9 to 11\"
  -> \"book DMM a van to the dentist tomorrow 9 to 11\"
  \"I'd like a van for the pool sat 8-10\"
  -> \"DMM would like a van for the pool sat 8-10\"

The rewrite is textual, and happens before parsing rather than injecting a
passenger afterwards, so PARSE-BOOKING reads the sender's initials like any
others.  Grammar is not preserved -- \"my ride\" becomes \"DMM ride\" --
because PARSE-BOOKING reads initials positionally and does not inflect."
  (if (or (not (stringp text)) (not (stringp initial)))
      text
      (cl-ppcre:regex-replace-all
       *self-reference-scanner* text
       (lambda (match)
         (let ((expansion (cdr (assoc (string-downcase match) *self-contractions*
                                      :test #'string=))))
           (if expansion
               (format nil expansion initial)
               initial)))
       :simple-calls t)))
