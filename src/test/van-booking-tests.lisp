;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/test/van-booking-tests
  (:use #:common-lisp #:parachute #:project-isidore/van-booking)
  (:local-nicknames (#:vb #:project-isidore/van-booking))
  (:documentation
   ""))

(in-package #:project-isidore/test/van-booking-tests)

;;; --- Helper: build fake spreadsheet rows ---

(defun make-block (day vehicle-dests vehicle-times vehicle-passengers)
  "Build a single time-block's rows (header + dest + dep/ret + times + passengers).
   VEHICLE-DESTS: plist (:dgh \"SHMS\" :lgh \"Shrine\" ...)
   VEHICLE-TIMES: plist (:dgh (\"8:35 am\" \"10:30 am\") ...)
   VEHICLE-PASSENGERS: plist (:dgh ((\"MM\" \"DM\") (\"MM\")) ...) = (dep-list ret-list)"
  (let ((header (list day "" "Chevy" "" "" "DGH" "" "" "LGH" "" "" "Personal" ""))
        (dest-row (list "" "" "" "" "" "" "" "" "" "" "" "" ""))
        (dep-ret (list "" "" "Dep." "Ret." "" "Dep." "Ret." "" "Dep." "Ret." "" "Dep." "Ret."))
        (time-row (list "" "" "" "" "" "" "" "" "" "" "" "" "")))
    ;; Fill destinations
    (loop for (veh dest) on vehicle-dests by #'cddr
          for spec = (assoc veh vb::*vehicles*)
          when spec do (setf (nth (second spec) dest-row) dest))
    ;; Fill times
    (loop for (veh times) on vehicle-times by #'cddr
          for spec = (assoc veh vb::*vehicles*)
          when spec do (setf (nth (second spec) time-row) (first times))
                       (setf (nth (third spec) time-row) (second times)))
    ;; Build passenger rows (up to 10 slots)
    (let ((pass-rows '()))
      (dotimes (r 10)
        (let ((row (make-list 13 :initial-element "")))
          (loop for (veh pax) on vehicle-passengers by #'cddr
                for spec = (assoc veh vb::*vehicles*)
                when spec
                  do (destructuring-bind (dep-pax ret-pax) pax
                       (when (< r (length dep-pax))
                         (setf (nth (second spec) row) (nth r dep-pax)))
                       (when (< r (length ret-pax))
                         (setf (nth (third spec) row) (nth r ret-pax)))))
          (push row pass-rows)))
      (append (list header dest-row dep-ret time-row) (nreverse pass-rows)))))

(defun make-test-sheet (&rest blocks)
  "Build a fake spreadsheet from BLOCKS. Adds 2 header rows before the blocks."
  (let ((title-row (list "" "" "TEST SHEET" "" "" "" "" "" "" "" "" "" ""))
        (steward-row (list "" "" "Stewards: test" "" "" "" "" "" "" "" "" "" "")))
    (append (list title-row steward-row)
            (apply #'append blocks))))

;;; --- Unit tests: utility functions ---

(define-test utilities
    (define-test col-letter
        (is string= "A" (vb::col-letter 0))
      (is string= "C" (vb::col-letter 2))
      (is string= "F" (vb::col-letter 5))
      (is string= "M" (vb::col-letter 12)))

  (define-test non-blank-p
      (true (vb::non-blank-p "hello"))
    (true (vb::non-blank-p " x "))
    (false (vb::non-blank-p ""))
    (false (vb::non-blank-p "   "))
    (false (vb::non-blank-p nil)))

  (define-test safe-nth
      (is string= "b" (vb::safe-nth 1 '("a" "b" "c")))
    (is string= "" (vb::safe-nth 5 '("a" "b")))
    (is string= "" (vb::safe-nth 0 nil)))

  (define-test phantom-row-p
      (true (vb::phantom-row-p '("")))
    (false (vb::phantom-row-p '("" "")))
    (false (vb::phantom-row-p '("hello")))
    (false (vb::phantom-row-p '("" "x"))))

  (define-test header-cell-p
      (true (vb::header-cell-p "Chevy"))
    (true (vb::header-cell-p "chevy"))
    (true (vb::header-cell-p "Dep."))
    (true (vb::header-cell-p "MONDAY"))
    (true (vb::header-cell-p "Sunday"))
    (false (vb::header-cell-p "MM"))
    (false (vb::header-cell-p "BW"))
    (false (vb::header-cell-p "")))

  (define-test vehicle-cols
      (multiple-value-bind (dep ret) (vb::vehicle-cols :dgh)
        (is = 5 dep)
        (is = 6 ret))
    (multiple-value-bind (dep ret) (vb::vehicle-cols :chevy)
      (is = 2 dep)
      (is = 3 ret))
    (fail (vb::vehicle-cols :nonexistent) error))

  (define-test extract-day-from-header
      (is string= "Monday March 30"
          (vb::extract-day-from-header
           '("Monday March 30           Monday March 30" "" "Chevy")))
    (false (vb::extract-day-from-header '("" "" "Chevy")))
    (is string= "Tuesday April 7"
        (vb::extract-day-from-header '("Tuesday April 7" "" "Chevy")))))

;;; --- Unit tests: clean-rows ---

(define-test clean-rows
    (define-test removes-phantom-rows
        (multiple-value-bind (clean map)
            (vb::clean-rows '(("a" "b") ("") ("c" "d") ("") ("e" "f")))
          (is = 3 (length clean))
          (is equal '("a" "b") (first clean))
          (is equal '("c" "d") (second clean))
          (is equal '("e" "f") (third clean))
          ;; Phantom rows are skipped — only real rows get sheet-row numbers
          (is = 1 (aref map 0))
          (is = 2 (aref map 1))
          (is = 3 (aref map 2))))

  (define-test no-phantoms
      (multiple-value-bind (clean map)
          (vb::clean-rows '(("a") ("b") ("c")))
        (is = 3 (length clean))
        (is = 1 (aref map 0))
        (is = 2 (aref map 1))
        (is = 3 (aref map 2)))))

;;; --- Unit tests: collect-passengers ---

(define-test collect-passengers
    (define-test basic-collection
        (let ((rows '(("" "" "" "" "" "MM" "MM")
                      ("" "" "" "" "" "DM" "")
                      ("" "" "" "" "" "CG" "")
                      ("" "" "" "" "" "" ""))))
          (is equal '("MM" "DM" "CG") (vb::collect-passengers rows 0 5 10))
          (is equal '("MM") (vb::collect-passengers rows 0 6 10))))

  (define-test stops-at-header
      (let ((rows '(("" "" "" "" "" "MM" "")
                    ("" "" "" "" "" "BW" "")
                    ("" "" "" "" "" "Chevy" "")
                    ("" "" "" "" "" "XX" ""))))
        (is equal '("MM" "BW") (vb::collect-passengers rows 0 5 10))))

  (define-test stops-at-dep
      (let ((rows '(("" "" "" "" "" "MM" "")
                    ("" "" "" "" "" "Dep." ""))))
        (is equal '("MM") (vb::collect-passengers rows 0 5 10))))

  (define-test respects-max-rows
      (let ((rows '(("" "" "" "" "" "A" "")
                    ("" "" "" "" "" "B" "")
                    ("" "" "" "" "" "C" ""))))
        (is equal '("A" "B") (vb::collect-passengers rows 0 5 2))))

  (define-test empty-column
      (let ((rows '(("" "" "" "" "" "" ""))))
        (is equal '() (vb::collect-passengers rows 0 5 10)))))

;;; --- Unit tests: parse-schedule ---

(define-test parse-schedule
    (define-test single-block
        (let* ((block (make-block "Monday April 20           Monday April 20"
                                  '(:dgh "SHMS" :lgh "Shrine")
                                  '(:dgh ("8:35 am" "10:30 am") :lgh ("5:00 pm" "8:00 pm"))
                                  '(:dgh (("MM" "DM") ("MM")) :lgh (("MK") ("MK")))))
               (sheet (make-test-sheet block))
               (trips (vb::parse-schedule sheet :sheet-name "test-sheet")))
          ;; 4 vehicles * 1 block = 4 trips
          (is = 4 (length trips))
          ;; Check DGH trip
          (let ((dgh (find :dgh trips :key #'trip-vehicle)))
            (true dgh)
            (is string= "Monday April 20" (trip-day dgh))
            (is string= "SHMS" (trip-destination dgh))
            (is string= "8:35 am" (trip-departure dgh))
            (is string= "10:30 am" (trip-return-time dgh))
            (is equal '("MM" "DM") (trip-dep-passengers dgh))
            (is equal '("MM") (trip-ret-passengers dgh))
            (is = 3 (trip-block-start-row dgh))
            (is string= "test-sheet" (trip-sheet-name dgh)))
          ;; Check LGH trip
          (let ((lgh (find :lgh trips :key #'trip-vehicle)))
            (is string= "Shrine" (trip-destination lgh))
            (is equal '("MK") (trip-dep-passengers lgh)))))

  (define-test multiple-blocks
      (let* ((b1 (make-block "Monday April 20           Monday April 20"
                             '(:dgh "SHMS") '(:dgh ("8 am" "10 am")) '(:dgh (("MM") ("MM")))))
             (b2 (make-block ""  ; no day header = same day
                             '(:lgh "Shrine") '(:lgh ("5 pm" "8 pm")) '(:lgh (("MK") ("MK")))))
             (sheet (make-test-sheet b1 b2))
             (trips (vb::parse-schedule sheet)))
        ;; 4 vehicles * 2 blocks = 8 trips
        (is = 8 (length trips))
        ;; First block at row 3, second at row 18
        (let ((block1-trips (remove-if-not (lambda (tr) (= 3 (trip-block-start-row tr))) trips))
              (block2-trips (remove-if-not (lambda (tr) (= 18 (trip-block-start-row tr))) trips)))
          (is = 4 (length block1-trips))
          (is = 4 (length block2-trips)))))

  (define-test day-carries-forward
      (let* ((b1 (make-block "Tuesday April 7           Tuesday April 7"
                             '() '() '()))
             (b2 (make-block "" '() '() '()))
             (sheet (make-test-sheet b1 b2))
             (trips (vb::parse-schedule sheet)))
        ;; Both blocks should have "Tuesday April 7"
        (is = 8 (length trips))
        (dolist (tr trips)
          (is string= "Tuesday April 7" (trip-day tr)))))

  (define-test no-dest-row
      ;; When dest row is missing (gviz drops it), parser should handle gracefully
      ;; Simulate: header row followed directly by Dep./Ret.
      (let* ((rows (list (list "" "" "TEST SHEET" "" "" "" "" "" "" "" "" "" "")
                         (list "" "" "Stewards" "" "" "" "" "" "" "" "" "" "")
                         (list "Wednesday April 8" "" "Chevy" "" "" "DGH" "" "" "LGH" "" "" "Personal" "")
                         (list "" "" "Dep." "Ret." "" "Dep." "Ret." "" "Dep." "Ret." "" "Dep." "Ret.")
                         (list "" "" "" "" "" "8:35 am" "10:30 am" "" "" "" "" "" "")
                         (list "" "" "" "" "" "MM" "MM" "" "" "" "" "" "")))
             (trips (vb::parse-schedule rows)))
        (let ((dgh (find :dgh trips :key #'trip-vehicle)))
          (true dgh)
          (is string= "" (trip-destination dgh))
          (is string= "8:35 am" (trip-departure dgh))
          (is equal '("MM") (trip-dep-passengers dgh))))))

;;; --- Unit tests: find-trips ---

(define-test find-trips
    (let* ((trips (list (make-trip :day "Monday April 20" :vehicle :dgh
                                   :destination "SHMS" :departure "8 am")
                        (make-trip :day "Monday April 20" :vehicle :lgh
                                   :destination "Shrine" :departure "5 pm")
                        (make-trip :day "Tuesday April 21" :vehicle :dgh
                                   :destination "SHMS" :departure "10 am")
                        (make-trip :day "Wednesday April 22" :vehicle :chevy
                                   :destination "" :departure ""))))

      (define-test filter-by-day
          (is = 2 (length (vb::find-trips trips :day "Monday")))
        (is = 1 (length (vb::find-trips trips :day "Tuesday")))
        (is = 0 (length (vb::find-trips trips :day "Thursday"))))

      (define-test filter-by-destination
          (is = 2 (length (vb::find-trips trips :destination "SHMS")))
        (is = 1 (length (vb::find-trips trips :destination "Shrine"))))

      (define-test filter-both
          (is = 1 (length (vb::find-trips trips :day "Monday" :destination "SHMS")))
        (is = 0 (length (vb::find-trips trips :day "Tuesday" :destination "Shrine"))))

      (define-test case-insensitive
          (is = 2 (length (vb::find-trips trips :destination "shms")))
        (is = 1 (length (vb::find-trips trips :day "monday" :destination "shrine"))))

      (define-test substring-match
          (is = 2 (length (vb::find-trips trips :day "April 2")))
        (is = 1 (length (vb::find-trips trips :destination "Shr"))))

      (define-test no-filters
          (is = 4 (length (vb::find-trips trips))))

      (define-test empty-dest-excluded
          (is = 0 (length (vb::find-trips trips :destination ""))))))

;;; --- Unit tests: find-empty-slot ---

;;; --- Unit tests: filter-new-passengers ---

(define-test filter-new-passengers
    (define-test removes-existing
        (is equal '("CG") (vb::filter-new-passengers '("MM" "DM" "CG") '("MM" "DM"))))

  (define-test case-insensitive
      (is equal '() (vb::filter-new-passengers '("mm") '("MM"))))

  (define-test all-new
      (is equal '("A" "B") (vb::filter-new-passengers '("A" "B") '())))

  (define-test all-existing
      (is equal '() (vb::filter-new-passengers '("X" "Y") '("X" "Y"))))

  (define-test empty-input
      (is equal '() (vb::filter-new-passengers '() '("X")))))

(define-test find-empty-slot
    (let* ((trips (list (make-trip :day "Monday April 20" :vehicle :dgh
                                   :destination "SHMS" :departure "8 am")
                        (make-trip :day "Monday April 20" :vehicle :lgh
                                   :destination "" :departure "")
                        (make-trip :day "Monday April 20" :vehicle :chevy
                                   :destination "" :departure "")
                        (make-trip :day "Monday April 20" :vehicle :personal
                                   :destination "" :departure ""))))

      (define-test auto-priority
          ;; DGH is taken, should pick LGH (next in priority)
          (let ((slot (vb::find-empty-slot trips "Monday")))
            (true slot)
            (is eq :lgh (trip-vehicle slot))))

      (define-test preferred-vehicle
          (let ((slot (vb::find-empty-slot trips "Monday" :chevy)))
            (true slot)
            (is eq :chevy (trip-vehicle slot))))

      (define-test preferred-taken
          ;; DGH is taken
          (false (vb::find-empty-slot trips "Monday" :dgh)))

      (define-test personal-not-auto
          ;; Personal should not be auto-selected even though it's empty
          (let* ((all-taken (list (make-trip :day "Monday" :vehicle :dgh
                                             :destination "X" :departure "8")
                                  (make-trip :day "Monday" :vehicle :lgh
                                             :destination "Y" :departure "9")
                                  (make-trip :day "Monday" :vehicle :chevy
                                             :destination "Z" :departure "10")
                                  (make-trip :day "Monday" :vehicle :personal
                                             :destination "" :departure ""))))
            (false (vb::find-empty-slot all-taken "Monday"))))

      (define-test wrong-day
          (false (vb::find-empty-slot trips "Tuesday")))))

;;; --- Unit tests: print-trip ---

;;; --- Unit tests: add-passengers deduplication ---

(define-test add-passengers-dedup
    (define-test skips-dep-duplicates
        ;; MM and DM already in dep; CG is new → only CG written to dep
        (let ((original (symbol-function 'vb::write-range))
              (dep-written :not-called))
          (unwind-protect
               (progn
                 (setf (symbol-function 'vb::write-range)
                       (lambda (range values &key sheet-name)
                         (declare (ignore sheet-name))
                         (when (search "F" range)
                           (setf dep-written (mapcar #'car values)))))
                 (let ((trip (make-trip :day "Monday" :vehicle :dgh :destination "SHMS"
                                        :dep-passengers '("MM" "DM") :ret-passengers '()
                                        :block-start-row 3 :dep-col 5 :ret-col 6
                                        :sheet-name "test")))
                   (vb::add-passengers trip '("MM" "DM" "CG"))
                   (is equal '("CG") dep-written)))
            (setf (symbol-function 'vb::write-range) original))))

  (define-test skips-ret-duplicates
      ;; MM already in ret; CG is new → only CG written to ret
      (let ((original (symbol-function 'vb::write-range))
            (ret-written :not-called))
        (unwind-protect
             (progn
               (setf (symbol-function 'vb::write-range)
                     (lambda (range values &key sheet-name)
                       (declare (ignore sheet-name))
                       (when (search "G" range)
                         (setf ret-written (mapcar #'car values)))))
               (let ((trip (make-trip :day "Monday" :vehicle :dgh :destination "SHMS"
                                      :dep-passengers '() :ret-passengers '("MM")
                                      :block-start-row 3 :dep-col 5 :ret-col 6
                                      :sheet-name "test")))
                 (vb::add-passengers trip '("MM" "CG"))
                 (is equal '("CG") ret-written)))
          (setf (symbol-function 'vb::write-range) original))))

  (define-test skips-all-when-fully-present
      ;; MM in both dep and ret → zero writes
      (let ((original (symbol-function 'vb::write-range))
            (write-count 0))
        (unwind-protect
             (progn
               (setf (symbol-function 'vb::write-range)
                     (lambda (range values &key sheet-name)
                       (declare (ignore range values sheet-name))
                       (incf write-count)))
               (let ((trip (make-trip :day "Monday" :vehicle :dgh :destination "SHMS"
                                      :dep-passengers '("MM") :ret-passengers '("MM")
                                      :block-start-row 3 :dep-col 5 :ret-col 6
                                      :sheet-name "test")))
                 (vb::add-passengers trip '("MM"))
                 (is = 0 write-count)))
          (setf (symbol-function 'vb::write-range) original))))

  (define-test case-insensitive-dedup
      ;; "mm" matches "MM" already in dep → only CG written to dep
      (let ((original (symbol-function 'vb::write-range))
            (dep-written :not-called))
        (unwind-protect
             (progn
               (setf (symbol-function 'vb::write-range)
                     (lambda (range values &key sheet-name)
                       (declare (ignore sheet-name))
                       (when (search "F" range)
                         (setf dep-written (mapcar #'car values)))))
               (let ((trip (make-trip :day "Monday" :vehicle :dgh :destination "SHMS"
                                      :dep-passengers '("MM") :ret-passengers '()
                                      :block-start-row 3 :dep-col 5 :ret-col 6
                                      :sheet-name "test")))
                 (vb::add-passengers trip '("mm" "CG"))
                 (is equal '("CG") dep-written)))
          (setf (symbol-function 'vb::write-range) original))))

  (define-test returns-trip
      ;; Return value should be the trip regardless
      (let ((original (symbol-function 'vb::write-range)))
        (unwind-protect
             (progn
               (setf (symbol-function 'vb::write-range)
                     (lambda (range values &key sheet-name)
                       (declare (ignore range values sheet-name))))
               (let* ((trip (make-trip :day "Monday" :vehicle :dgh :destination "SHMS"
                                       :dep-passengers '() :ret-passengers '()
                                       :block-start-row 3 :dep-col 5 :ret-col 6
                                       :sheet-name "test"))
                      (result (vb::add-passengers trip '("MM"))))
                 (is eq trip result)))
          (setf (symbol-function 'vb::write-range) original)))))

(define-test print-trip
    (define-test basic-format
        (let* ((tr (make-trip :day "Monday April 20" :vehicle :dgh
                              :destination "SHMS" :departure "8:35 am"
                              :return-time "10:30 am"
                              :dep-passengers '("MM" "DM")
                              :sheet-name "Apr 20-26"))
               (output (with-output-to-string (s) (vb::print-trip tr s))))
          (true (search "Monday April 20" output))
          (true (search "DGH" output))
          (true (search "SHMS" output))
          (true (search "8:35 am" output))
          (true (search "MM, DM" output))
          (true (search "Apr 20-26" output))))

  (define-test no-sheet-name
      (let* ((tr (make-trip :day "Monday" :vehicle :lgh
                            :destination "X" :departure "9 am"
                            :return-time "11 am"
                            :dep-passengers '()
                            :sheet-name nil))
             (output (with-output-to-string (s) (vb::print-trip tr s))))
        (false (search "[" output)))))

;;; --- Run all tests ---

(define-test book-natural-tests

  (define-test format-time-12h
      (is string= "12:00 am" (vb::format-time-12h 0 0))
    (is string= "12:00 pm" (vb::format-time-12h 12 0))
    (is string= "7:00 pm"  (vb::format-time-12h 19 0))
    (is string= "11:30 am" (vb::format-time-12h 11 30))
    (is string= "1:05 pm"  (vb::format-time-12h 13 5)))

  (define-test parse-passenger-initials
      (is equal '("CG" "BW") (vb::parse-passenger-initials "CG and BW"))
    (is equal '("MM" "DM" "CG") (vb::parse-passenger-initials "MM, DM and CG"))
    (is equal '("MM") (vb::parse-passenger-initials "MM"))
    (is equal '() (vb::parse-passenger-initials "no initials here")))

  (define-test parse-booking-text
      ;; canonical sentence
      (multiple-value-bind (dest day pax dep ret)
          (vb::parse-booking-text
           "I want to book a van for swimming next week Tuesday for CG and BW at seven PM to nine PM")
        (is string= "swimming" dest)
        (true (search "tuesday" (string-downcase day)))
        (is string= "CG and BW" pax)
        (true (search "seven" (string-downcase dep)))
        (true (search "nine" (string-downcase ret))))
    ;; tomorrow
    (multiple-value-bind (dest day pax dep ret)
        (vb::parse-booking-text "book for pool tomorrow for MM at 8 am to 10 am")
      (is string= "pool" dest)
      (is string= "tomorrow" (string-downcase day))
      (is string= "MM" pax)
      (is string= "8 am" dep)
      (is string= "10 am" ret))
    ;; multi-word destination
    (multiple-value-bind (dest day pax dep ret)
        (vb::parse-booking-text "for the pool next Friday for BW at 9 am to 11 am")
      (declare (ignore pax dep ret))
      (is string= "the pool" dest)
      (true (search "friday" (string-downcase day))))
    ;; no match
    (false (vb::parse-booking-text "no structure here")))

  (define-test normalize-booking-text
      ;; "from X to Y" → "at X to Y"
      (true (search "at seven PM to"
                    (vb::normalize-booking-text
                     "for swimming tomorrow for CG from seven PM to nine PM")))
    ;; "starting at" → "at"
    (true (search "at 8 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM starting at 8 am to 10 am")))
    ;; "starting" (without "at") → "at"
    (true (search "at 8 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM starting 8 am to 10 am")))
    ;; "departing" → "at"
    (true (search "at 8 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM departing 8 am to 10 am")))
    ;; "leaving at" → "at"
    (true (search "at 8 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM leaving at 8 am to 10 am")))
    ;; "leaving" → "at"
    (true (search "at 8 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM leaving 8 am to 10 am")))
    ;; "beginning at" → "at"
    (true (search "at 8 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM beginning at 8 am to 10 am")))
    ;; "going at" → "at"
    (true (search "at 8 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM going at 8 am to 10 am")))
    ;; "picking up at" → "at"
    (true (search "at 8 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM picking up at 8 am to 10 am")))
    ;; "pickup at" → "at"
    (true (search "at 8 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM pickup at 8 am to 10 am")))
    ;; "until" → "to"
    (true (search "to 10 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM at 8 am until 10 am")))
    ;; "till" → "to"
    (true (search "to 10 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM at 8 am till 10 am")))
    ;; "through" → "to"
    (true (search "to 10 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM at 8 am through 10 am")))
    ;; "returning at" → "to"
    (true (search "to 10 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM at 8 am returning at 10 am")))
    ;; "returning by" → "to"
    (true (search "to 10 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM at 8 am returning by 10 am")))
    ;; "ending at" → "to"
    (true (search "to 10 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM at 8 am ending at 10 am")))
    ;; "back by" → "to"
    (true (search "to 10 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM at 8 am back by 10 am")))
    ;; "back at" → "to"
    (true (search "to 10 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM at 8 am back at 10 am")))
    ;; "dropping off at" → "to"
    (true (search "to 10 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM at 8 am dropping off at 10 am")))
    ;; "dropoff at" → "to"
    (true (search "to 10 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM at 8 am dropoff at 10 am")))
    ;; "with PAX" → "for PAX"
    (true (search "for CG"
                  (vb::normalize-booking-text
                   "for swimming tomorrow with CG at 8 am to 10 am")))
    ;; "carrying PAX" → "for PAX"
    (true (search "for CG"
                  (vb::normalize-booking-text
                   "for swimming tomorrow carrying CG at 8 am to 10 am")))
    ;; "taking PAX" → "for PAX"
    (true (search "for CG"
                  (vb::normalize-booking-text
                   "for swimming tomorrow taking CG at 8 am to 10 am")))
    ;; "bringing PAX" → "for PAX"
    (true (search "for CG"
                  (vb::normalize-booking-text
                   "for swimming tomorrow bringing CG at 8 am to 10 am")))
    ;; "passengers PAX" → "for PAX"
    (true (search "for CG"
                  (vb::normalize-booking-text
                   "for swimming tomorrow passengers CG at 8 am to 10 am")))
    ;; idempotent on canonical form
    (let ((canonical "for swimming tomorrow for CG at 8 am to 10 am"))
      (is string= canonical (vb::normalize-booking-text canonical))))

  (define-test parse-booking-text-flexible
      ;; "from ... to" instead of "at ... to"
      (multiple-value-bind (dest day pax dep ret)
          (vb::parse-booking-text
           "book for swimming tomorrow for CG from seven PM to nine PM")
        (is string= "swimming" dest)
        (is string= "tomorrow" (string-downcase day))
        (is string= "CG" pax)
        (true (search "seven" (string-downcase dep)))
        (true (search "nine" (string-downcase ret))))
    ;; "leaving ... until"
    (multiple-value-bind (dest day pax dep ret)
        (vb::parse-booking-text
         "for pool tomorrow for MM leaving 8 am until 10 am")
      (is string= "pool" dest)
      (is string= "8 am" dep)
      (is string= "10 am" ret))
    ;; "starting at ... till"
    (multiple-value-bind (dest day pax dep ret)
        (vb::parse-booking-text
         "for pool tomorrow for MM starting at 8 am till 10 am")
      (is string= "pool" dest)
      (is string= "8 am" dep)
      (is string= "10 am" ret))
    ;; "with" as passenger introducer
    (multiple-value-bind (dest day pax dep ret)
        (vb::parse-booking-text
         "for swimming tomorrow with CG at 8 am to 10 am")
      (is string= "swimming" dest)
      (is string= "CG" pax)
      (is string= "8 am" dep))
    ;; "carrying" as passenger introducer
    (multiple-value-bind (dest day pax dep ret)
        (vb::parse-booking-text
         "for swimming tomorrow carrying CG at 8 am to 10 am")
      (is string= "swimming" dest)
      (is string= "CG" pax))
    ;; mixed: "taking PAX ... from X to Y"
    (multiple-value-bind (dest day pax dep ret)
        (vb::parse-booking-text
         "book for swimming tomorrow taking CG from 8 am to 10 am")
      (is string= "swimming" dest)
      (is string= "CG" pax)
      (is string= "8 am" dep)
      (is string= "10 am" ret))
    ;; "back by" as return-time introducer
    (multiple-value-bind (dest day pax dep ret)
        (vb::parse-booking-text
         "for pool tomorrow for MM at 8 am back by 10 am")
      (is string= "pool" dest)
      (is string= "8 am" dep)
      (is string= "10 am" ret)))

  (define-test book-natural-calls-book
      ;; Verify book-natural parses NL text and calls book with correctly shaped args.
      ;; Mocks book to avoid network calls.
      (let ((original (symbol-function 'vb::book))
            (book-args nil))
        (unwind-protect
             (progn
               (setf (symbol-function 'vb::book)
                     (lambda (day dest pax &key dep-time ret-time vehicle week)
                       (declare (ignore vehicle week))
                       (setf book-args (list :day day :dest dest :pax pax
                                             :dep dep-time :ret ret-time))))
               (vb:book-natural
                "I want to book a van for swimming next week Tuesday for CG and BW at seven PM to nine PM")
               (is string= "swimming" (getf book-args :dest))
               (is equal '("CG" "BW") (getf book-args :pax))
               ;; day format: "DayName MonthName DayNum"
               (true (cl-ppcre:scan "^\\w+ \\w+ \\d+$" (getf book-args :day)))
               ;; time format: "H:MM am/pm"
               (true (cl-ppcre:scan "^\\d+:\\d{2} [ap]m$" (getf book-args :dep)))
               (true (cl-ppcre:scan "^\\d+:\\d{2} [ap]m$" (getf book-args :ret))))
          (setf (symbol-function 'vb::book) original)))))

(define-test all-initials-p
    (define-test single-initial
        (true (vb::all-initials-p "GG"))
      (true (vb::all-initials-p "MM"))
      (true (vb::all-initials-p "CGB")))

  (define-test multiple-with-and
      (true (vb::all-initials-p "CG and BW"))
    (true (vb::all-initials-p "MM and DM and CG")))

  (define-test multiple-with-commas
      (true (vb::all-initials-p "MM, DM, CG"))
    (true (vb::all-initials-p "MM, DM and CG")))

  (define-test rejects-non-initials
      (false (vb::all-initials-p "swimming"))
    (false (vb::all-initials-p "the pool"))
    (false (vb::all-initials-p "a haircut"))
    (false (vb::all-initials-p "SHMS"))
    (false (vb::all-initials-p ""))
    (false (vb::all-initials-p "ABC store"))))

(define-test parse-booking-text-order-agnostic
    ;; dest → times → pax (user's requested example)
    (define-test dest-times-pax
        (multiple-value-bind (dest day pax dep ret)
            (vb::parse-booking-text
             "book a van for a haircut from 12:30pm to 2:30pm for GG.")
          (is string= "a haircut" dest)
          (is string= "today" day)
          (is string= "GG" pax)
          (is string= "12:30pm" dep)
          (is string= "2:30pm" ret)))

  ;; pax → dest → times
  (define-test pax-dest-times
      (multiple-value-bind (dest day pax dep ret)
          (vb::parse-booking-text
           "for CG and BW for swimming at 7 pm to 9 pm")
        (is string= "swimming" dest)
        (is string= "CG and BW" pax)
        (is string= "7 pm" dep)
        (is string= "9 pm" ret)))

  ;; times → dest → pax
  (define-test times-dest-pax
      (multiple-value-bind (dest day pax dep ret)
          (vb::parse-booking-text
           "from 12:30pm to 2:30pm for a haircut for GG")
        (is string= "a haircut" dest)
        (is string= "GG" pax)
        (is string= "12:30pm" dep)
        (is string= "2:30pm" ret)))

  ;; times → pax → dest
  (define-test times-pax-dest
      (multiple-value-bind (dest day pax dep ret)
          (vb::parse-booking-text
           "from 8 am to 10 am for MM for pool")
        (is string= "pool" dest)
        (is string= "MM" pax)
        (is string= "8 am" dep)
        (is string= "10 am" ret)))

  ;; day can appear anywhere
  (define-test day-before-dest
      (multiple-value-bind (dest day pax dep ret)
          (vb::parse-booking-text
           "tomorrow for swimming for CG at 7 pm to 9 pm")
        (declare (ignore dep ret))
        (is string= "swimming" dest)
        (is string= "tomorrow" day)
        (is string= "CG" pax)))

  (define-test day-after-times
      (multiple-value-bind (dest day pax dep ret)
          (vb::parse-booking-text
           "for swimming for CG at 7 pm to 9 pm tomorrow")
        (declare (ignore dep ret))
        (is string= "swimming" dest)
        (is string= "tomorrow" day)
        (is string= "CG" pax)))

  (define-test day-between-dest-and-pax
      (multiple-value-bind (dest day pax dep ret)
          (vb::parse-booking-text
           "for swimming tomorrow for CG at 7 pm to 9 pm")
        (declare (ignore dep ret))
        (is string= "swimming" dest)
        (is string= "tomorrow" day)
        (is string= "CG" pax)))

  ;; day defaults to today when omitted
  (define-test day-defaults-to-today
      (multiple-value-bind (dest day pax dep ret)
          (vb::parse-booking-text
           "for pool for MM at 8 am to 10 am")
        (declare (ignore dep ret))
        (is string= "pool" dest)
        (is string= "today" day)
        (is string= "MM" pax)))

  ;; trailing punctuation stripped
  (define-test trailing-period
      (multiple-value-bind (dest day pax dep ret)
          (vb::parse-booking-text
           "for pool for MM at 8 am to 10 am.")
        (declare (ignore dep))
        (is string= "pool" dest)
        (is string= "MM" pax)
        (is string= "10 am" ret)))

  ;; multiple passengers with comma + and
  (define-test multiple-passengers-any-order
      (multiple-value-bind (dest day pax dep ret)
          (vb::parse-booking-text
           "from 8 am to 10 am for MM, DM and CG for pool tomorrow")
        (declare (ignore dep ret))
        (is string= "pool" dest)
        (is string= "tomorrow" day)
        (is string= "MM, DM and CG" pax))))

(define-test preamble-stripping
    ;; "I need to book the van" stripped
    (is string= "for swimming tomorrow for CG at 8 am to 10 am"
        (vb::normalize-booking-text
         "I need to book the van for swimming tomorrow for CG at 8 am to 10 am"))
  ;; "can you book a van" stripped
  (is string= "for swimming tomorrow for CG at 8 am to 10 am"
      (vb::normalize-booking-text
       "can you book a van for swimming tomorrow for CG at 8 am to 10 am"))
  ;; "please reserve the van" stripped
  (is string= "for swimming tomorrow for CG at 8 am to 10 am"
      (vb::normalize-booking-text
       "please reserve the van for swimming tomorrow for CG at 8 am to 10 am"))
  ;; "I'd like to schedule a van" stripped
  (is string= "for swimming tomorrow for CG at 8 am to 10 am"
      (vb::normalize-booking-text
       "I'd like to schedule a van for swimming tomorrow for CG at 8 am to 10 am"))
  ;; "could you please book" stripped
  (is string= "for swimming tomorrow for CG at 8 am to 10 am"
      (vb::normalize-booking-text
       "could you please book the van for swimming tomorrow for CG at 8 am to 10 am"))
  ;; "I want to get the van" stripped
  (is string= "for swimming tomorrow for CG at 8 am to 10 am"
      (vb::normalize-booking-text
       "I want to get the van for swimming tomorrow for CG at 8 am to 10 am"))
  ;; just "book" with no article/van
  (is string= "for swimming tomorrow for CG at 8 am to 10 am"
      (vb::normalize-booking-text
       "book for swimming tomorrow for CG at 8 am to 10 am"))
  ;; preamble-free text unchanged
  (is string= "for swimming tomorrow for CG at 8 am to 10 am"
      (vb::normalize-booking-text
       "for swimming tomorrow for CG at 8 am to 10 am")))

(define-test new-return-time-introducers
    ;; "arrive by" → "to"
    (true (search "to 10 am"
                  (vb::normalize-booking-text
                   "for pool tomorrow for MM at 8 am arrive by 10 am")))
  ;; "arrive at" → "to"
  (true (search "to 10 am"
                (vb::normalize-booking-text
                 "for pool tomorrow for MM at 8 am arrive at 10 am")))
  ;; "be back by" → "to"
  (true (search "to 10 am"
                (vb::normalize-booking-text
                 "for pool tomorrow for MM at 8 am be back by 10 am")))
  ;; "get back by" → "to"
  (true (search "to 10 am"
                (vb::normalize-booking-text
                 "for pool tomorrow for MM at 8 am get back by 10 am")))
  ;; "return by" → "to"
  (true (search "to 10 am"
                (vb::normalize-booking-text
                 "for pool tomorrow for MM at 8 am return by 10 am")))
  ;; "done by" → "to"
  (true (search "to 10 am"
                (vb::normalize-booking-text
                 "for pool tomorrow for MM at 8 am done by 10 am")))
  ;; "finished by" → "to"
  (true (search "to 10 am"
                (vb::normalize-booking-text
                 "for pool tomorrow for MM at 8 am finished by 10 am")))
  ;; full parse: "leaving 8 am be back by 10 am"
  (multiple-value-bind (dest day pax dep ret)
      (vb::parse-booking-text
       "for pool tomorrow for MM leaving 8 am be back by 10 am")
    (is string= "pool" dest)
    (is string= "8 am" dep)
    (is string= "10 am" ret)))

(define-test destination-introducers
    ;; "going to" → "for"
    (true (search "for swimming"
                  (vb::normalize-booking-text
                   "going to swimming tomorrow for CG at 8 am to 10 am")))
  ;; "headed to" → "for"
  (true (search "for the pool"
                (vb::normalize-booking-text
                 "headed to the pool tomorrow for CG at 8 am to 10 am")))
  ;; "heading to" → "for"
  (true (search "for the pool"
                (vb::normalize-booking-text
                 "heading to the pool tomorrow for CG at 8 am to 10 am")))
  ;; "driving to" → "for"
  (true (search "for the store"
                (vb::normalize-booking-text
                 "driving to the store tomorrow for CG at 8 am to 10 am")))
  ;; full parse: "going to swimming"
  (multiple-value-bind (dest day pax dep ret)
      (vb::parse-booking-text
       "going to swimming tomorrow for CG at 8 am to 10 am")
    (is string= "swimming" dest)
    (is string= "CG" pax)))

(define-test bare-time-fallback
    ;; bare "8am to 10am" without introducer
    (multiple-value-bind (dest day pax dep ret)
        (vb::parse-booking-text
         "for swimming tomorrow for CG 8am to 10am")
      (is string= "swimming" dest)
      (is string= "CG" pax)
      (is string= "8am" dep)
      (is string= "10am" ret))
  ;; bare "7 pm to 9 pm"
  (multiple-value-bind (dest day pax dep ret)
      (vb::parse-booking-text
       "for pool for MM 7 pm to 9 pm")
    (is string= "pool" dest)
    (is string= "MM" pax)
    (is string= "7 pm" dep)
    (is string= "9 pm" ret))
  ;; bare with preamble: "book a van for X 8am to 10am for Y"
  (multiple-value-bind (dest day pax dep ret)
      (vb::parse-booking-text
       "book a van for swimming tomorrow for CG 8am to 3pm")
    (is string= "swimming" dest)
    (is string= "CG" pax)
    (is string= "8am" dep)
    (is string= "3pm" ret)))

(define-test vehicle-preference
    ;; "take the DGH" → vehicle :DGH
    (multiple-value-bind (dest day pax dep ret vehicle)
        (vb::parse-booking-text
         "for swimming tomorrow for CG at 8 am to 10 am take the DGH")
      (is string= "swimming" dest)
      (is string= "CG" pax)
      (is eq :dgh vehicle))
  ;; "use the LGH"
  (multiple-value-bind (dest day pax dep ret vehicle)
      (vb::parse-booking-text
       "use the LGH for pool for MM at 8 am to 10 am")
    (is string= "pool" dest)
    (is eq :lgh vehicle))
  ;; "prefer the Chevy"
  (multiple-value-bind (dest day pax dep ret vehicle)
      (vb::parse-booking-text
       "for pool for MM at 8 am to 10 am prefer the Chevy")
    (is string= "pool" dest)
    (is eq :chevy vehicle))
  ;; "in the DGH"
  (multiple-value-bind (dest day pax dep ret vehicle)
      (vb::parse-booking-text
       "for pool for MM at 8 am to 10 am in the DGH")
    (is string= "pool" dest)
    (is eq :dgh vehicle))
  ;; no vehicle → NIL
  (multiple-value-bind (dest day pax dep ret vehicle)
      (vb::parse-booking-text
       "for pool for MM at 8 am to 10 am")
    (is string= "pool" dest)
    (false vehicle)))

(define-test book-natural-expanded-integration
    ;; Full sentence with preamble + destination introducer + vehicle + new return introducer
    (let ((original (symbol-function 'vb::book))
          (book-args nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'vb::book)
                   (lambda (day dest pax &key dep-time ret-time vehicle week)
                     (declare (ignore week))
                     (setf book-args (list :day day :dest dest :pax pax
                                           :dep dep-time :ret ret-time
                                           :vehicle vehicle))))
             ;; preamble + vehicle + destination introducer + return introducer
             (vb:book-natural
              "I'd like to reserve the DGH going to swimming tomorrow for CG leaving 8 am be back by 10 am")
             (is string= "swimming" (getf book-args :dest))
             (is eq :dgh (getf book-args :vehicle))
             (is equal '("CG") (getf book-args :pax))
             (true (cl-ppcre:scan "\\d+:\\d{2} [ap]m" (getf book-args :dep)))
             (true (cl-ppcre:scan "\\d+:\\d{2} [ap]m" (getf book-args :ret))))
        (setf (symbol-function 'vb::book) original)))
  ;; Bare times with preamble
  (let ((original (symbol-function 'vb::book))
        (book-args nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'vb::book)
                 (lambda (day dest pax &key dep-time ret-time vehicle week)
                   (declare (ignore week))
                   (setf book-args (list :day day :dest dest :pax pax
                                         :dep dep-time :ret ret-time
                                         :vehicle vehicle))))
           (vb:book-natural "book the van for pool tomorrow for MM 8am to 3pm")
           (is string= "pool" (getf book-args :dest))
           (is equal '("MM") (getf book-args :pax))
           (false (getf book-args :vehicle)))
      (setf (symbol-function 'vb::book) original))))

(define-test meridiem-normalization
    ;; "a.m." → "am"
    (true (search "9:30 am"
                  (vb::normalize-booking-text
                   "for pool for MM at 9:30 a.m. to 11:30 a.m.")))
  ;; "p.m." → "pm"
  (true (search "9:30 pm"
                (vb::normalize-booking-text
                 "for pool for MM at 9:30 p.m. to 11:30 p.m.")))
  ;; "a.m" without trailing period
  (true (search "9:30 am"
                (vb::normalize-booking-text
                 "for pool for MM at 9:30 a.m to 11:30 a.m")))
  ;; mixed case "A.M."
  (true (search "9:30 am"
                (vb::normalize-booking-text
                 "for pool for MM at 9:30 A.M. to 11:30 A.M."))))

(define-test month-day-phrase
    ;; "April 11th" extracted as day phrase
    (multiple-value-bind (dest day pax dep ret)
        (vb::parse-booking-text
         "for BW for April 11th at 9:30 am to 11:30 am for seminary ordination")
      (declare (ignore dep ret))
      (is string= "seminary ordination" dest)
      (true (search "april" (string-downcase day)))
      (true (search "11" day))
      (is string= "BW" pax))
  ;; Abbreviated month
  (multiple-value-bind (dest day pax dep ret)
      (vb::parse-booking-text
       "for pool for MM for Apr 11 at 8 am to 10 am")
    (declare (ignore dest dep ret pax))
    (true (search "apr" (string-downcase day)))
    (true (search "11" day)))
  ;; Bare day number without ordinal suffix
  (multiple-value-bind (dest day pax dep ret)
      (vb::parse-booking-text
       "for pool for MM for March 30 at 8 am to 10 am")
    (declare (ignore dest dep ret pax))
    (true (search "march" (string-downcase day)))
    (true (search "30" day))))

(define-test book-natural-month-day-and-meridiem
    ;; The exact input from the bug report
    (multiple-value-bind (dest day pax dep ret)
        (vb::parse-booking-text
         "Book a van for BW for April 11th from 9:30 a.m. to 11:30 a.m going to seminary ordination.")
      (is string= "seminary ordination" dest)
      (true (search "april" (string-downcase day)))
      (true (search "11" day))
      (is string= "BW" pax)
      (is string= "9:30 am" dep)
      (is string= "11:30 am" ret)))

(define-test month-abbrev-num
    (is = 1 (vb::month-abbrev-num "Jan"))
  (is = 3 (vb::month-abbrev-num "Mar"))
  (is = 4 (vb::month-abbrev-num "Apr"))
  (is = 5 (vb::month-abbrev-num "May"))
  (is = 12 (vb::month-abbrev-num "Dec"))
  (is = 1 (vb::month-abbrev-num "January"))
  (fail (vb::month-abbrev-num "Xyz") error))

(define-test date-key
    (is = 20260420 (vb::date-key (chronicity:parse "April 20 2026")))
  (is = 20260101 (vb::date-key (chronicity:parse "January 1 2026")))
  (is = 20251231 (vb::date-key (chronicity:parse "December 31 2025"))))

(define-test parse-week-range
    (define-test single-month
        (let ((year (chronicity:year-of (chronicity:parse "today"))))
          (multiple-value-bind (start end) (vb::parse-week-range "Apr 20-26")
            (is = (+ (* year 10000) 420) start)
            (is = (+ (* year 10000) 426) end))))
  (define-test cross-month
      (let ((year (chronicity:year-of (chronicity:parse "today"))))
        (multiple-value-bind (start end) (vb::parse-week-range "Mar 30-Apr 5 (Holy wk)")
          (is = (+ (* year 10000) 330) start)
          (is = (+ (* year 10000) 405) end))))
  (define-test trailing-whitespace
      (let ((year (chronicity:year-of (chronicity:parse "today"))))
        (multiple-value-bind (start end) (vb::parse-week-range "Apr 13-19 ")
          (is = (+ (* year 10000) 413) start)
          (is = (+ (* year 10000) 419) end))))
  (define-test no-match
      (multiple-value-bind (start end) (vb::parse-week-range "garbage")
        (false start)
        (false end))))

(define-test find-week-sheet
    (define-test mid-week-match
        (let* ((year (chronicity:year-of (chronicity:parse "today")))
               (target (chronicity:parse (format nil "April 22 ~D" year))))
          (is string= "Apr 20-26" (vb::find-week-sheet target))))
  (define-test boundary-match-start
      (let* ((year (chronicity:year-of (chronicity:parse "today")))
             (target (chronicity:parse (format nil "April 20 ~D" year))))
        (is string= "Apr 20-26" (vb::find-week-sheet target))))
  (define-test boundary-match-end
      (let* ((year (chronicity:year-of (chronicity:parse "today")))
             (target (chronicity:parse (format nil "April 26 ~D" year))))
        (is string= "Apr 20-26" (vb::find-week-sheet target))))
  (define-test cross-month-match
      (let* ((year (chronicity:year-of (chronicity:parse "today")))
             (target (chronicity:parse (format nil "April 1 ~D" year))))
        (is string= "Mar 30-Apr 5 (Holy wk)" (vb::find-week-sheet target))))
  (define-test no-match
      (let* ((year (chronicity:year-of (chronicity:parse "today")))
             (target (chronicity:parse (format nil "December 25 ~D" year))))
        (false (vb::find-week-sheet target)))))

(defun call-with-mock-read-range (rows thunk)
  "Run THUNK with vb::read-range stubbed to return ROWS regardless of args."
  (let ((original (symbol-function 'vb::read-range)))
    (unwind-protect
         (progn
           (setf (symbol-function 'vb::read-range)
                 (lambda (range &key sheet-name)
                   (declare (ignore range sheet-name))
                   rows))
           (funcall thunk))
      (setf (symbol-function 'vb::read-range) original))))

(define-test remind-driver-tests
    (define-test golden-path-multi-passenger
        (let* ((day-str (vb::format-booking-day (chronicity:parse "April 22")))
               (block (make-block (format nil "~A           ~A" day-str day-str)
                                  '(:dgh "X location")
                                  '(:dgh ("12:00 pm" "2:00 pm"))
                                  '(:dgh (("BW" "CG" "GG") ("BW" "CG" "GG")))))
               (sheet (make-test-sheet block)))
          (call-with-mock-read-range sheet
            (lambda ()
              (let ((result (remind-driver "BW" "April 22")))
                (true (search "BW is the driver" result))
                (true (search "X location" result))
                (true (search "12:00 pm" result))
                (true (search "CG, GG are passengers" result))))))
)
  (define-test single-other-passenger-grammar
      (let* ((day-str (vb::format-booking-day (chronicity:parse "April 22")))
             (block (make-block (format nil "~A           ~A" day-str day-str)
                                '(:dgh "Pool")
                                '(:dgh ("8:00 am" "10:00 am"))
                                '(:dgh (("BW" "CG") ("BW" "CG")))))
             (sheet (make-test-sheet block)))
        (call-with-mock-read-range sheet
          (lambda ()
            (let ((result (remind-driver "BW" "April 22")))
              (true (search "CG is a passenger" result))
              (false (search "are passengers" result)))))))

  (define-test no-other-passengers-clause
      (let* ((day-str (vb::format-booking-day (chronicity:parse "April 22")))
             (block (make-block (format nil "~A           ~A" day-str day-str)
                                '(:dgh "SHMS")
                                '(:dgh ("12:35 pm" "2:35 pm"))
                                '(:dgh (("BW") ("BW")))))
             (sheet (make-test-sheet block)))
        (call-with-mock-read-range sheet
          (lambda ()
            (let ((result (remind-driver "BW" "April 22")))
              (true (search "with no other passengers" result)))))))

  (define-test multiple-trips-same-day
      (let* ((day-str (vb::format-booking-day (chronicity:parse "April 22")))
             (b1 (make-block (format nil "~A           ~A" day-str day-str)
                             '(:dgh "SHMS")
                             '(:dgh ("12:35 pm" "2:35 pm"))
                             '(:dgh (("BW") ("BW")))))
             (b2 (make-block ""
                             '(:lgh "Shrine")
                             '(:lgh ("5:35 pm" "8:00 pm"))
                             '(:lgh (("BW" "CG") ("BW" "CG")))))
             (sheet (make-test-sheet b1 b2)))
        (call-with-mock-read-range sheet
          (lambda ()
            (let ((result (remind-driver "BW" "April 22")))
              (true (search "SHMS" result))
              (true (search "Shrine" result))
              (true (search "12:35 pm" result))
              (true (search "5:35 pm" result))
              (true (search "CG is a passenger" result)))))))

  (define-test driver-not-first-is-not-driver
      (let* ((day-str (vb::format-booking-day (chronicity:parse "April 22")))
             (block (make-block (format nil "~A           ~A" day-str day-str)
                                '(:dgh "SHMS")
                                '(:dgh ("8:00 am" "10:00 am"))
                                '(:dgh (("MM" "BW") ("MM" "BW")))))
             (sheet (make-test-sheet block)))
        (call-with-mock-read-range sheet
          (lambda ()
            (let ((result (remind-driver "BW" "April 22")))
              (true (search "BW is not driving" result))
              (true (search day-str result)))))))

  (define-test driver-match-case-insensitive
      (let* ((day-str (vb::format-booking-day (chronicity:parse "April 22")))
             (block (make-block (format nil "~A           ~A" day-str day-str)
                                '(:dgh "SHMS")
                                '(:dgh ("8:00 am" "10:00 am"))
                                '(:dgh (("BW") ("BW")))))
             (sheet (make-test-sheet block)))
        (call-with-mock-read-range sheet
          (lambda ()
            (let ((result (remind-driver "bw" "April 22")))
              (true (search "bw is the driver" result)))))))

  (define-test no-sheet-covers-day
      (let ((called nil))
        (let ((original (symbol-function 'vb::read-range)))
          (unwind-protect
               (progn
                 (setf (symbol-function 'vb::read-range)
                       (lambda (range &key sheet-name)
                         (declare (ignore range sheet-name))
                         (setf called t)
                         nil))
                 (let* ((year (chronicity:year-of (chronicity:parse "today")))
                        (result (remind-driver
                                 "BW" (format nil "December 25 ~D" year))))
                   (true (search "No schedule sheet covers" result))
                   (false called)))
            (setf (symbol-function 'vb::read-range) original))))))

(define-test view-schedule-tests
    (define-test split-range-phrase
        (multiple-value-bind (s e) (vb::split-range-phrase "today")
          (is string= "today" s)
          (is string= "today" e))
      (multiple-value-bind (s e) (vb::split-range-phrase "tomorrow through Friday")
        (is string= "tomorrow" s)
        (is string= "Friday" e))
      (multiple-value-bind (s e) (vb::split-range-phrase "Apr 20 to Apr 26")
        (is string= "Apr 20" s)
        (is string= "Apr 26" e))
      (multiple-value-bind (s e) (vb::split-range-phrase "Apr 20 - Apr 26")
        (is string= "Apr 20" s)
        (is string= "Apr 26" e))
      ;; case-insensitive
      (multiple-value-bind (s e) (vb::split-range-phrase "Monday TO Friday")
        (is string= "Monday" s)
        (is string= "Friday" e))
      ;; only the first separator splits (limit 2)
      (multiple-value-bind (s e) (vb::split-range-phrase "Apr 20 to Apr 26 to Apr 30")
        (is string= "Apr 20" s)
        (is string= "Apr 26 to Apr 30" e)))

  (define-test trip-date-key
      (let ((year (chronicity:year-of (chronicity:parse "today"))))
        ;; Standard trip-day format: "DayOfWeek MonthName Day"
        (is = (+ (* year 10000) 420)
            (vb::trip-date-key (make-trip :day "Monday April 20")))
        (is = (+ (* year 10000) 1225)
            (vb::trip-date-key (make-trip :day "Friday December 25"))))
      ;; Blank day → NIL
      (false (vb::trip-date-key (make-trip :day "")))
      (false (vb::trip-date-key (make-trip :day nil)))
      ;; Unparseable → NIL (graceful failure)
      (false (vb::trip-date-key (make-trip :day "garbage words here"))))

  (define-test find-week-sheets-in-range
      (let ((year (chronicity:year-of (chronicity:parse "today"))))
        ;; Fully inside one week
        (is equal '("Apr 20-26")
            (vb::find-week-sheets-in-range (+ (* year 10000) 421)
                                           (+ (* year 10000) 424)))
        ;; Exact boundary
        (is equal '("Apr 20-26")
            (vb::find-week-sheets-in-range (+ (* year 10000) 420)
                                           (+ (* year 10000) 426)))
        ;; Spans two weeks
        (let ((result (vb::find-week-sheets-in-range (+ (* year 10000) 419)
                                                     (+ (* year 10000) 422))))
          (true (member "Apr 13-19 " result :test #'string=))
          (true (member "Apr 20-26" result :test #'string=)))
        ;; Out of range → empty
        (is equal '()
            (vb::find-week-sheets-in-range (+ (* year 10000) 1225)
                                           (+ (* year 10000) 1231)))))

  (define-test format-short-date
      (is string= "Mon Apr 20" (vb::format-short-date "Monday April 20"))
      (is string= "Thu Jan 1"  (vb::format-short-date "Thursday January 1"))
      ;; Fallback on unparseable
      (is string= "garbage" (vb::format-short-date "garbage")))

  (define-test vehicle-priority
      (is = 0 (vb::vehicle-priority :dgh))
      (is = 1 (vb::vehicle-priority :lgh))
      (is = 2 (vb::vehicle-priority :chevy))
      (is = 3 (vb::vehicle-priority :personal))
      ;; unknown vehicle sinks to bottom
      (true (> (vb::vehicle-priority :bogus) 100)))

  (define-test departure-minutes
      (is = (+ (* 8 60) 35)   (vb::departure-minutes "8:35 am"))
      (is = (+ (* 12 60) 35)  (vb::departure-minutes "12:35 pm"))
      (is = (* 17 60)         (vb::departure-minutes "5:00 pm"))
      (is = 0                 (vb::departure-minutes ""))
      (is = 0                 (vb::departure-minutes nil))
      ;; garbage → 0 (sort-safe default)
      (is = 0                 (vb::departure-minutes "not a time"))
      ;; ordering property: am < pm regardless of lexical order
      (true (< (vb::departure-minutes "8:35 am")
               (vb::departure-minutes "12:35 pm")))
      (true (< (vb::departure-minutes "12:35 pm")
               (vb::departure-minutes "5:35 pm"))))

  (define-test truncate-cell
      (is string= "hello"     (vb::truncate-cell "hello" 10))
      (is string= "hello"     (vb::truncate-cell "hello" 5))
      (is string= "hel..."    (vb::truncate-cell "hello world" 6))
      (is string= ""          (vb::truncate-cell "" 5))
      (is string= ""          (vb::truncate-cell nil 5)))

  (define-test booked-trip-p
      (true (vb::booked-trip-p (make-trip :destination "SHMS" :departure "")))
      (true (vb::booked-trip-p (make-trip :destination "" :departure "8 am")))
      (true (vb::booked-trip-p (make-trip :destination "SHMS" :departure "8 am")))
      (false (vb::booked-trip-p (make-trip :destination "" :departure "")))
      (false (vb::booked-trip-p (make-trip :destination nil :departure nil))))

  (define-test format-schedule-table-shape
      (let* ((trips (list (make-trip :day "Monday April 20" :vehicle :dgh
                                     :destination "SHMS" :departure "8:35 am"
                                     :return-time "10:30 am"
                                     :dep-passengers '("MM" "DM")
                                     :ret-passengers '("MM" "DM" "BW"))))
             (out (vb::format-schedule-table trips)))
        (true (search "Date" out))
        (true (search "Vehicle" out))
        (true (search "Destination" out))
        (true (search "Dep Pax" out))
        (true (search "Ret Pax" out))
        (true (search "Mon Apr 20" out))
        (true (search "DGH" out))
        (true (search "SHMS" out))
        (true (search "8:35 am" out))
        (true (search "MM, DM" out))
        (true (search "MM, DM, BW" out))
        ;; separator row present
        (true (search "---" out))))

  (define-test format-schedule-table-autosizes-destination
      ;; Long destinations must render in full (no truncation) and every row
      ;; must be the same rendered width so columns stay aligned.
      (let* ((long-dest (make-string 60 :initial-element #\X))
             (trips (list (make-trip :day "Monday April 20" :vehicle :dgh
                                     :destination long-dest :departure "8 am"
                                     :return-time "10 am"
                                     :dep-passengers '("MM"))
                          (make-trip :day "Tuesday April 21" :vehicle :lgh
                                     :destination "SHMS" :departure "9 am"
                                     :return-time "11 am"
                                     :dep-passengers '("BW"))))
             (out (vb::format-schedule-table trips))
             (lines (remove "" (cl-ppcre:split "\\n" out) :test #'string=)))
        (true (search long-dest out))
        (false (search "..." out))
        (let ((w (length (first lines))))
          (dolist (line (rest lines))
            (is = w (length line))))))

  (define-test view-schedule-default-today
      ;; With an empty sheet, view-schedule returns the "no trips" message.
      ;; Uses a sheet whose range covers today so find-week-sheets-in-range returns it.
      (let* ((year (chronicity:year-of (chronicity:parse "today")))
             (today-key (vb::date-key (chronicity:parse "today")))
             (covered (loop for entry in vb::*weeks*
                            for name = (first entry)
                            thereis (multiple-value-bind (s e)
                                        (vb::parse-week-range name)
                                      (and s (<= s today-key e) name)))))
        (declare (ignore year))
        (when covered
          (let ((sheet (make-test-sheet)))
            (call-with-mock-read-range sheet
              (lambda ()
                (let ((out (view-schedule)))
                  (true (search "No trips scheduled" out)))))))))

  (define-test view-schedule-renders-trips
      (let* ((day-str (vb::format-booking-day (chronicity:parse "April 22")))
             (block (make-block (format nil "~A           ~A" day-str day-str)
                                '(:dgh "SHMS")
                                '(:dgh ("8:35 am" "10:30 am"))
                                '(:dgh (("MM" "DM") ("MM" "DM")))))
             (sheet (make-test-sheet block)))
        (call-with-mock-read-range sheet
          (lambda ()
            (let ((out (view-schedule "April 22")))
              (true (search "SHMS" out))
              (true (search "8:35 am" out))
              (true (search "DGH" out))
              (true (search "MM, DM" out))
              (true (search "Date" out)))))))

  (define-test view-schedule-no-sheet
      ;; Date out of any configured week → informative message, no fetch.
      (let* ((year (chronicity:year-of (chronicity:parse "today")))
             (called nil)
             (original (symbol-function 'vb::read-range)))
        (unwind-protect
             (progn
               (setf (symbol-function 'vb::read-range)
                     (lambda (range &key sheet-name)
                       (declare (ignore range sheet-name))
                       (setf called t)
                       nil))
               (let ((out (view-schedule (format nil "December 25 ~D" year))))
                 (true (search "No schedule sheets cover" out))
                 (false called)))
          (setf (symbol-function 'vb::read-range) original))))

  (define-test view-schedule-sort-order
      ;; Two trips same day, different departure times and vehicles.
      ;; Ensure 8:35 am (DGH) appears before 12:35 pm (LGH) even though
      ;; "12:35 pm" < "8:35 am" under naive string sort.
      (let* ((day-str (vb::format-booking-day (chronicity:parse "April 22")))
             (block (make-block (format nil "~A           ~A" day-str day-str)
                                '(:dgh "Morning" :lgh "Afternoon")
                                '(:dgh ("8:35 am" "10:30 am")
                                  :lgh ("12:35 pm" "2:30 pm"))
                                '(:dgh (("MM") ("MM"))
                                  :lgh (("DM") ("DM")))))
             (sheet (make-test-sheet block)))
        (call-with-mock-read-range sheet
          (lambda ()
            (let* ((out (view-schedule "April 22"))
                   (morning-pos (search "Morning" out))
                   (afternoon-pos (search "Afternoon" out)))
              (true morning-pos)
              (true afternoon-pos)
              (true (< morning-pos afternoon-pos))))))))

(define-test vb-tests
    (test 'utilities)
  (test 'clean-rows)
  (test 'collect-passengers)
  (test 'parse-schedule)
  (test 'find-trips)
  (test 'filter-new-passengers)
  (test 'find-empty-slot)
  (test 'add-passengers-dedup)
  (test 'print-trip)
  (test 'book-natural-tests)
  (test 'all-initials-p)
  (test 'parse-booking-text-order-agnostic)
  (test 'preamble-stripping)
  (test 'new-return-time-introducers)
  (test 'destination-introducers)
  (test 'bare-time-fallback)
  (test 'vehicle-preference)
  (test 'book-natural-expanded-integration)
  (test 'meridiem-normalization)
  (test 'month-day-phrase)
  (test 'book-natural-month-day-and-meridiem)
  (test 'month-abbrev-num)
  (test 'date-key)
  (test 'parse-week-range)
  (test 'find-week-sheet)
  (test 'remind-driver-tests)
  (test 'view-schedule-tests))
