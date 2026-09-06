;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/booking-llm-eval
  (:use #:common-lisp)
  (:local-nicknames (#:llm #:project-isidore/booking-llm)
                    (#:bn #:project-isidore/booking-nlp)
                    (#:vb #:project-isidore/van-booking)
                    (#:bnt #:project-isidore/tests/booking-nlp-tests))
  (:export #:run-eval #:*view-corpus* #:*paraphrase-corpus*)
  (:documentation
   "Measure a model behind llama-server against the booking corpus.

Not part of project-isidore/tests: it needs a live server.  Load it and call

  (project-isidore/tests/booking-llm-eval:run-eval
   :url \"http://127.0.0.1:8081/v1/chat/completions\")

Every sentence of BOOKING-NLP-TESTS::*BOOKING-CORPUS*, plus *VIEW-CORPUS*
and *PARAPHRASE-CORPUS*, is sent to the model.  A booking reply is rendered
with RENDER-BOOK-REQUEST and re-read by the rule parser, then held to the
corpus entry's expectations, exactly as production would use it; a view
reply must name the same days as the expected range.  The report lists every
mismatch, hallucinated initials (not in the message), and latency
percentiles, and RUN-EVAL returns a summary plist for scripts."))

(in-package #:project-isidore/tests/booking-llm-eval)

(defparameter *view-corpus*
  '(("what's on the van this week?" :intent :view :range "this week")
    ("who has the DGH tomorrow?" :intent :view :range "tomorrow")
    ("is anything booked for the weekend?" :intent :view :range "the weekend")
    ("show me the schedule for the rest of the week" :intent :view :range "the rest of the week")
    ("what trips are there Saturday?" :intent :view :range "Saturday")
    ("schedule for Sept 19 to 22?" :intent :view :range "Sept 19 to Sept 22")
    ("any vans out today?" :intent :view :range "today")
    ("can I see next week's bookings" :intent :view :range "next week")
    ;; questions about a trip's timing are a view of that day
    ("when is my ride home this coming tuesday" :intent :view :range "Tuesday")
    ("when is my van home this tuesday" :intent :view :range "Tuesday")
    ("what time does the van leave tomorrow?" :intent :view :range "tomorrow")
    ("how late is the van out on thursday?" :intent :view :range "Thursday")
    ("thanks!" :intent :other)
    ("good morning everyone" :intent :other)
    ("what can you do?" :intent :other))
  "Messages that are not bookings: the rule parser answers all of these with
a re-send report today.")

(defparameter *paraphrase-corpus*
  '(("bw and mm to the pool thurs 7-9pm"
     :destination "pool" :day-contains "thursday" :passengers ("BW" "MM")
     :dep-time "7:00 pm" :ret-time "9:00 pm" :missing nil)
    ("me and BW need wheels to the dentist tmrw, out half past 8, back 11ish"
     :destination "dentist" :day-like "tomorrow" :passengers ("BW")
     :dep-time "8:30 am" :ret-time "11:00 am" :missing nil)
    ("Could someone put CG down for the airport on Monday? leave at 6, back by 9"
     :destination "airport" :day-contains "monday" :passengers ("CG")
     :dep-time "6:00 am" :ret-time "9:00 am" :missing nil)
    ("DM wants the Chevy Saturday morning for groceries, 10 till 11"
     :destination "groceries" :day-contains "saturday" :passengers ("DM") :vehicle :chevy
     :dep-time "10:00 am" :ret-time "11:00 am" :missing nil)
    ("Sign GG up for Thursday's SHMS trip please"
     :destination "SHMS" :day-contains "thursday" :passengers ("GG")
     :missing (:dep-time :ret-time))
    ("MM + DM, pool, Apr 22, 9am-noon"
     :destination "pool" :day-md "April 22" :passengers ("MM" "DM")
     :dep-time "9:00 am" :ret-time "12:00 pm" :missing nil)
    ("Is the LGH free tomorrow 2-4 for BW to see the dentist?"
     :destination "dentist" :day-like "tomorrow" :passengers ("BW") :vehicle :lgh
     :dep-time "2:00 pm" :ret-time "4:00 pm" :missing nil)
    ("we'd like to take the van to Saint Marys on Friday: MM, DM, CG; leaving 9, home noon"
     :destination "Saint Marys" :day-contains "friday" :passengers ("MM" "DM" "CG")
     :dep-time "9:00 am" :ret-time "12:00 pm" :missing nil)
    ("could bw get the dgh tmrw for the doctor, 8 to 10?"
     :destination "doctor" :day-like "tomorrow" :passengers ("BW") :vehicle :dgh
     :dep-time "8:00 am" :ret-time "10:00 am" :missing nil))
  "Phrasings outside the rule parser's cue lists, in the corpus notation.")

;;; --- Comparing a reply with an entry ---

(defun slot-mismatches (req expected)
  "Like BOOKING-NLP-TESTS::CHECK-REQUEST, but returns a list of mismatch
descriptions instead of asserting."
  (let ((out '()))
    (flet ((check (ok label wanted got)
             (unless ok
               (push (format nil "~a: wanted ~s, got ~s" label wanted got) out))))
      (loop for (key value) on expected by #'cddr
            do (case key
                 (:destination
                  (check (equal value (bn:booking-request-destination req))
                         "destination" value (bn:booking-request-destination req)))
                 (:day-like
                  (let ((wanted (bnt::day-string value)) (got (bnt::request-day req)))
                    (check (equal wanted got) "day" wanted got)))
                 (:day-md
                  (let ((wanted (bnt::month-day-string value)) (got (bnt::request-day req)))
                    (check (equal wanted got) "day" wanted got)))
                 (:day-contains
                  (let ((got (bnt::request-day req)))
                    (check (and got (search value got)) "day" value got)))
                 (:passengers
                  (check (equal value (bn:booking-request-passengers req))
                         "passengers" value (bn:booking-request-passengers req)))
                 (:dep-time
                  (check (equal value (bn:booking-request-dep-time req))
                         "departure" value (bn:booking-request-dep-time req)))
                 (:ret-time
                  (check (equal value (bn:booking-request-ret-time req))
                         "return" value (bn:booking-request-ret-time req)))
                 (:vehicle
                  (check (eq value (bn:booking-request-vehicle req))
                         "vehicle" value (bn:booking-request-vehicle req)))
                 (:missing
                  (check (equal value (bn:booking-request-missing req))
                         "missing" value (bn:booking-request-missing req)))
                 (:missing-includes
                  (check (subsetp value (bn:booking-request-missing req))
                         "missing includes" value (bn:booking-request-missing req)))
                 (t nil))))
    (nreverse out)))

(defun without-keys (plist keys)
  (loop for (key value) on plist by #'cddr
        unless (member key keys) append (list key value)))

(defun range-keys (phrase)
  "(START-KEY . END-KEY) for a `/view` range PHRASE, or NIL when the schedule
cannot read it."
  (ignore-errors
   (multiple-value-bind (start-phrase end-phrase) (vb::split-range-phrase phrase)
     (multiple-value-bind (s1 e1) (vb::phrase-date-range start-phrase)
       (multiple-value-bind (s2 e2) (vb::phrase-date-range end-phrase)
         (when (and s1 s2)
           (cons (min (vb::current-year-date-key s1) (vb::current-year-date-key s2))
                 (max (vb::current-year-date-key e1) (vb::current-year-date-key e2)))))))))

(defun evaluate-entry (text intent expected)
  "Compare the model's INTENT for TEXT with the corpus EXPECTED plist.
Returns (values MISMATCHES HALLUCINATED-INITIALS)."
  (let* ((wanted-intent (getf expected :intent :book))
         (got-intent (getf intent :intent))
         (complete-p (and (eq wanted-intent :book)
                          (member :missing expected)
                          (null (getf expected :missing)))))
    (cond
      ((null intent)
       (values (list "no usable reply from the model") nil))
      ((eq wanted-intent :view)
       (values (append
                (unless (eq got-intent :view)
                  (list (format nil "intent: wanted VIEW, got ~a" got-intent)))
                (let ((wanted (getf expected :range)) (got (getf intent :range)))
                  (unless (or (null wanted)
                              (and got (equal (range-keys wanted) (range-keys got))))
                    (list (format nil "range: wanted ~s, got ~s" wanted got)))))
               nil))
      ((eq wanted-intent :other)
       (values (unless (eq got-intent :other)
                 (list (format nil "intent: wanted OTHER, got ~a" got-intent)))
               nil))
      ;; an incomplete booking may also be declined outright
      ((and (not complete-p) (eq got-intent :other))
       (values nil nil))
      ((not (eq got-intent :book))
       (values (list (format nil "intent: wanted BOOK, got ~a" got-intent)) nil))
      (t
       (let* ((hallucinated (remove-if (lambda (p) (llm:initial-in-text-p p text))
                                       (getf intent :passengers)))
              (req (bn:parse-booking (llm:render-book-request intent)))
              (mismatches (slot-mismatches
                           req
                           (if complete-p
                               expected
                               (without-keys expected '(:intent :missing :missing-includes))))))
         (values (append mismatches
                         (when (and complete-p
                                    (null (llm:validate-book-intent intent text)))
                           (list "validate-book-intent rejected the reply")))
                 hallucinated))))))

;;; --- The run ---

(defun percentile (sorted fraction)
  (if (zerop (length sorted))
      0.0
      (aref sorted (min (1- (length sorted)) (floor (* fraction (length sorted)))))))

(defun run-eval (&key (url llm:*llm-url*)
                      (corpora (list bnt::*booking-corpus* *view-corpus* *paraphrase-corpus*))
                      known-destinations
                      (stream *standard-output*))
  "Send every entry of CORPORA to the model at URL and report.  Returns a
plist (:total :passed :failed :hallucinated :p50 :p95 :max :failures)."
  (let ((llm:*llm-url* url)
        (results '()))
    (dolist (corpus corpora)
      (dolist (entry corpus)
        (destructuring-bind (text &rest expected) entry
          (let* ((start (get-internal-real-time))
                 (intent (llm:llm-normalize text :known-destinations known-destinations))
                 (secs (/ (- (get-internal-real-time) start)
                          (float internal-time-units-per-second))))
            (multiple-value-bind (mismatches hallucinated) (evaluate-entry text intent expected)
              (push (list :text text :secs secs :intent intent
                          :mismatches mismatches :hallucinated hallucinated)
                    results)
              (format stream "~:[FAIL~;ok  ~] ~5,1fs ~s~@[~%       ~{~a~^~%       ~}~]~@[~%       hallucinated: ~{~a~^ ~}~]~@[~%       reply: ~s~]~%"
                      (and (null mismatches) (null hallucinated)) secs text
                      mismatches hallucinated
                      (and (or mismatches hallucinated) intent)))))))
    (let* ((results (nreverse results))
           (secs (sort (map 'vector (lambda (r) (getf r :secs)) results) #'<))
           (failures (remove-if (lambda (r) (and (null (getf r :mismatches))
                                                 (null (getf r :hallucinated))))
                                results))
           (summary (list :total (length results)
                          :passed (- (length results) (length failures))
                          :failed (length failures)
                          :hallucinated (count-if (lambda (r) (getf r :hallucinated)) results)
                          :p50 (percentile secs 0.5)
                          :p95 (percentile secs 0.95)
                          :max (percentile secs 1.0)
                          :failures (mapcar (lambda (r) (getf r :text)) failures))))
      (format stream "~%~d entries: ~d passed, ~d failed, ~d with hallucinated initials~%latency p50 ~,1fs  p95 ~,1fs  max ~,1fs~%"
              (getf summary :total) (getf summary :passed) (getf summary :failed)
              (getf summary :hallucinated)
              (getf summary :p50) (getf summary :p95) (getf summary :max))
      summary)))
