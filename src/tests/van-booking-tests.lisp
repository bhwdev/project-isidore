;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/van-booking-tests
  (:use #:common-lisp #:parachute #:project-isidore/van-booking)
  (:local-nicknames (#:vb #:project-isidore/van-booking)
                    (#:bn #:project-isidore/booking-nlp))
  (:import-from #:project-isidore/tests/test-utils #:with-mocked-functions)
  (:import-from #:chronicity)
  (:import-from #:com.inuoe.jzon)
  (:import-from #:dexador)
  (:documentation
   "Parachute tests for project-isidore/van-booking: sheet parsing, trip
lookup, booking writes (with the Google Sheets calls mocked), the
BOOK-NATURAL front end, reminders and the schedule view.  Natural-language
parsing itself is covered in booking-nlp-tests."))

(in-package #:project-isidore/tests/van-booking-tests)

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

(defparameter *sample-weeks*
  '(("Mar 30-Apr 5 (Holy wk)" "367982190")
    ("Apr 6-12"               nil)
    ("Apr 13-19 "             nil)
    ("Apr 20-26"              nil)
    ("Apr 27-May 3"           nil))
  "A fixed week-sheet list bound to VB::*WEEKS* by tests that look sheets up
by date, so they neither depend on the live spreadsheet's tabs nor fetch them.")

(defparameter *htmlview-fixture*
  "<html><head><script nonce=\"x\">var items = [{name: \"Aug 31-Sep 6\", pageUrl: \"https:\\/\\/docs.google.com\\/spreadsheets\\/d\\/id\\/htmlview\\/sheet?headers\\x3dtrue&gid=1696388518\", gid: \"1696388518\",initialSheet: (\"1696388518\" == gid)},{name: \"Sep 7-13\", pageUrl: \"u\", gid: \"217444495\",initialSheet: false},{name: \"Mar 30-Apr 5 (Holy wk)\", pageUrl: \"u\", gid: \"367982190\"},{name: \"St\\x27s \\/ Dates\", pageUrl: \"u\", gid: \"1762101409\"},{name: \"Apr 13-19 \", pageUrl: \"u\", gid: \"5\"}];</script></head><body></body></html>"
  "A cut-down copy of the spreadsheet's public htmlview page: the tab list
Google embeds as JavaScript object literals, with the escapes it uses.")

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
          (is equal '("MM") (trip-dep-passengers dgh)))))

  (define-test weekday-label-row
      ;; The sheet keeps a weekday reminder in the Personal column (L) on the
      ;; last row of each block.  gviz drops empty rows, so for a block with
      ;; no destination and no times that reminder is the row found at the
      ;; time offset.  It must not be read as a Personal departure time
      ;; (which made /view list a PERSONAL trip departing at "Saturday" for
      ;; every unbooked block and marked the Personal slot as taken).
      (let* ((rows (list (list "" "" "TEST SHEET" "" "" "" "" "" "" "" "" "" "")
                         (list "" "" "Stewards" "" "" "" "" "" "" "" "" "" "")
                         (list "Saturday September 5" "" "Chevy" "" "" "DGH" "" "" "LGH" "" "" "Personal" "")
                         (list "" "" "Dep." "Ret." "" "Dep." "Ret." "" "Dep." "Ret." "" "Dep." "Ret.")
                         (list "" "" "" "" "" "" "" "" "" "" "" "Saturday" "")
                         (list "" "" "Chevy" "" "" "DGH" "" "" "LGH" "" "" "Personal" "")
                         (list "" "" "Dep." "Ret." "" "Dep." "Ret." "" "Dep." "Ret." "" "Dep." "Ret.")
                         (list "" "" "" "" "" "" "" "" "" "" "" "Saturday" "")))
             (trips (vb::parse-schedule rows)))
        (is = 8 (length trips))
        (dolist (tr trips)
          (is string= "Saturday September 5" (trip-day tr))
          (is string= "" (trip-destination tr))
          (is string= "" (trip-departure tr))
          (is string= "" (trip-return-time tr))
          (false (trip-dep-passengers tr))
          (false (vb::booked-trip-p tr)))
        ;; ... so the Personal slot is free for a new booking
        (true (vb::find-empty-slot trips "Saturday September 5" :personal))))

  (define-test label-row-p
      (true (vb::label-row-p '("" "" "" "" "" "" "" "" "" "" "" "Saturday" "")))
    (true (vb::label-row-p '("" "" "" "" "" "" "" "" "" "" "" "MONDAY" "")))
    (true (vb::label-row-p '("Saturday September 5" "" "Chevy" "" "" "DGH" "" "" "LGH" "" "" "Personal" "")))
    (true (vb::label-row-p '("" "" "Dep." "Ret." "" "Dep." "Ret." "" "Dep." "Ret." "" "Dep." "Ret.")))
    (true (vb::label-row-p '("" "")))
    (false (vb::label-row-p '("" "" "" "" "" "8:35 am" "10:30 am" "" "" "" "" "" "")))
    (false (vb::label-row-p '("" "" "SHMS" "" "" "" "" "" "" "" "" "" "")))
    (false (vb::label-row-p '("" "" "" "" "" "MM" "" "" "" "" "" "Saturday" "")))))

;;; --- Unit tests: find-trips ---

(defun sample-trips ()
  "Four trips over three days for the FIND-TRIPS tests."
  (list (make-trip :day "Monday April 20" :vehicle :dgh
                   :destination "SHMS" :departure "8 am")
        (make-trip :day "Monday April 20" :vehicle :lgh
                   :destination "Shrine" :departure "5 pm")
        (make-trip :day "Tuesday April 21" :vehicle :dgh
                   :destination "SHMS" :departure "10 am")
        (make-trip :day "Wednesday April 22" :vehicle :chevy
                   :destination "" :departure "")))

(define-test find-trips
  (define-test filter-by-day
    (let ((trips (sample-trips)))
      (is = 2 (length (vb::find-trips trips :day "Monday")))
      (is = 1 (length (vb::find-trips trips :day "Tuesday")))
      (is = 0 (length (vb::find-trips trips :day "Thursday")))))

  (define-test filter-by-destination
    (let ((trips (sample-trips)))
      (is = 2 (length (vb::find-trips trips :destination "SHMS")))
      (is = 1 (length (vb::find-trips trips :destination "Shrine")))))

  (define-test filter-both
    (let ((trips (sample-trips)))
      (is = 1 (length (vb::find-trips trips :day "Monday" :destination "SHMS")))
      (is = 0 (length (vb::find-trips trips :day "Tuesday" :destination "Shrine")))))

  (define-test find-trips-case-insensitive
    (let ((trips (sample-trips)))
      (is = 2 (length (vb::find-trips trips :destination "shms")))
      (is = 1 (length (vb::find-trips trips :day "monday" :destination "shrine")))))

  (define-test substring-match
    (let ((trips (sample-trips)))
      (is = 2 (length (vb::find-trips trips :day "April 20")))
      ;; a bare "April 2" is a prefix of every April 2x day
      (is = 4 (length (vb::find-trips trips :day "April 2")))
      (is = 1 (length (vb::find-trips trips :destination "Shr")))))

  (define-test no-filters
    (is = 4 (length (vb::find-trips (sample-trips)))))

  (define-test empty-dest-excluded
    (is = 0 (length (vb::find-trips (sample-trips) :destination "")))))

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

(defun slot-trips ()
  "One Monday with the DGH taken and the other vehicles free."
  (list (make-trip :day "Monday April 20" :vehicle :dgh
                   :destination "SHMS" :departure "8 am")
        (make-trip :day "Monday April 20" :vehicle :lgh
                   :destination "" :departure "")
        (make-trip :day "Monday April 20" :vehicle :chevy
                   :destination "" :departure "")
        (make-trip :day "Monday April 20" :vehicle :personal
                   :destination "" :departure "")))

(define-test find-empty-slot
  (define-test auto-priority
    ;; DGH is taken, should pick LGH (next in priority)
    (let ((slot (vb::find-empty-slot (slot-trips) "Monday")))
      (true slot)
      (is eq :lgh (trip-vehicle slot))))

  (define-test preferred-vehicle
    (let ((slot (vb::find-empty-slot (slot-trips) "Monday" :chevy)))
      (true slot)
      (is eq :chevy (trip-vehicle slot))))

  (define-test preferred-taken
    ;; DGH is taken
    (false (vb::find-empty-slot (slot-trips) "Monday" :dgh)))

  (define-test personal-not-auto
    ;; Personal should not be auto-selected even though it's empty
    (let ((all-taken (list (make-trip :day "Monday" :vehicle :dgh
                                      :destination "X" :departure "8")
                           (make-trip :day "Monday" :vehicle :lgh
                                      :destination "Y" :departure "9")
                           (make-trip :day "Monday" :vehicle :chevy
                                      :destination "Z" :departure "10")
                           (make-trip :day "Monday" :vehicle :personal
                                      :destination "" :departure ""))))
      (false (vb::find-empty-slot all-taken "Monday"))
      ;; ... unless asked for explicitly
      (is eq :personal (trip-vehicle (vb::find-empty-slot all-taken "Monday" :personal)))))

  (define-test wrong-day
    (false (vb::find-empty-slot (slot-trips) "Tuesday"))))

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
  (define-test parse-week-range-no-match
    (multiple-value-bind (start end) (vb::parse-week-range "garbage")
      (false start)
      (false end))
    ;; Right shape but the month word is not a month (a helper tab, say)
    (multiple-value-bind (start end) (vb::parse-week-range "Week 1-2")
      (false start)
      (false end))))

(define-test find-week-sheet
  ;; Each child pins VB::*WEEKS* to *SAMPLE-WEEKS* so the lookup is
  ;; deterministic and never fetches the live tab list.
  (define-test mid-week-match
    (let* ((vb::*weeks* *sample-weeks*)
           (year (chronicity:year-of (chronicity:parse "today")))
           (target (chronicity:parse (format nil "April 22 ~D" year))))
      (is string= "Apr 20-26" (vb::find-week-sheet target))))
  (define-test boundary-match-start
    (let* ((vb::*weeks* *sample-weeks*)
           (year (chronicity:year-of (chronicity:parse "today")))
           (target (chronicity:parse (format nil "April 20 ~D" year))))
      (is string= "Apr 20-26" (vb::find-week-sheet target))))
  (define-test boundary-match-end
    (let* ((vb::*weeks* *sample-weeks*)
           (year (chronicity:year-of (chronicity:parse "today")))
           (target (chronicity:parse (format nil "April 26 ~D" year))))
      (is string= "Apr 20-26" (vb::find-week-sheet target))))
  (define-test cross-month-match
    (let* ((vb::*weeks* *sample-weeks*)
           (year (chronicity:year-of (chronicity:parse "today")))
           (target (chronicity:parse (format nil "April 1 ~D" year))))
      (is string= "Mar 30-Apr 5 (Holy wk)" (vb::find-week-sheet target))))
  (define-test find-week-sheet-no-match
    (let* ((vb::*weeks* *sample-weeks*)
           (year (chronicity:year-of (chronicity:parse "today")))
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
  ;; Each child pins VB::*WEEKS* to *SAMPLE-WEEKS* (which cover April 22) so
  ;; the sheet lookup is deterministic and never fetches the live tab list.
  (define-test golden-path-multi-passenger
      (let* ((vb::*weeks* *sample-weeks*)
             (day-str (vb::format-booking-day (chronicity:parse "April 22")))
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
                                       (true (search "CG, GG are passengers" result)))))))

  (define-test single-other-passenger-grammar
      (let* ((vb::*weeks* *sample-weeks*)
             (day-str (vb::format-booking-day (chronicity:parse "April 22")))
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
      (let* ((vb::*weeks* *sample-weeks*)
             (day-str (vb::format-booking-day (chronicity:parse "April 22")))
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
      (let* ((vb::*weeks* *sample-weeks*)
             (day-str (vb::format-booking-day (chronicity:parse "April 22")))
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
      (let* ((vb::*weeks* *sample-weeks*)
             (day-str (vb::format-booking-day (chronicity:parse "April 22")))
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
      (let* ((vb::*weeks* *sample-weeks*)
             (day-str (vb::format-booking-day (chronicity:parse "April 22")))
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
      (let ((vb::*weeks* *sample-weeks*)
            (called nil))
        (with-mocked-functions ((vb::read-range (lambda (range &key sheet-name)
                                                  (declare (ignore range sheet-name))
                                                  (setf called t)
                                                  nil)))
          (let* ((year (chronicity:year-of (chronicity:parse "today")))
                 (result (remind-driver
                          "BW" (format nil "December 25 ~D" year))))
            (true (search "No schedule sheet covers" result))
            (false called))))))

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
        (is string= "Apr 26 to Apr 30" e))
      ;; "and" / "&" join two periods (the /help example "this week and next week")
      (multiple-value-bind (s e) (vb::split-range-phrase "this week and next week")
        (is string= "this week" s)
        (is string= "next week" e))
      (multiple-value-bind (s e) (vb::split-range-phrase "Saturday & Sunday")
        (is string= "Saturday" s)
        (is string= "Sunday" e))
      ;; a period on its own is not split
      (multiple-value-bind (s e) (vb::split-range-phrase "next week")
        (is string= "next week" s)
        (is string= "next week" e)))

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
      (let ((vb::*weeks* *sample-weeks*)
            (year (chronicity:year-of (chronicity:parse "today"))))
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
        ;; titles and rule are indented like the rows; the day is a flush-left
        ;; heading over them
        (true (uiop:string-prefix-p "  Vehicle  " out))
        (true (search (format nil "~%Mon Apr 20~%  DGH") out))
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

  (define-test format-schedule-table-caps-columns
      ;; Destination, Dep and Ret size themselves to their longest cell only up
      ;; to *COLUMN-WIDTH-CAPS*: longer cells are shortened with "..." and the
      ;; values stay under their titles.
      (let* ((dest-cap (cdr (assoc "Destination" vb::*column-width-caps* :test #'string=)))
             (ret-cap (cdr (assoc "Ret" vb::*column-width-caps* :test #'string=)))
             (long-dest (make-string 60 :initial-element #\X))
             (long-ret "after reception (around 2:30 pm)")
             (trips (list (make-trip :day "Monday April 20" :vehicle :dgh
                                     :destination long-dest :departure "8 am"
                                     :return-time long-ret
                                     :dep-passengers '("MM"))
                          (make-trip :day "Tuesday April 21" :vehicle :lgh
                                     :destination "SHMS" :departure "9 am"
                                     :return-time "11 am"
                                     :dep-passengers '("BW"))))
             (out (vb::format-schedule-table trips))
             (lines (cl-ppcre:split "\\n" out))
             (header (first lines)))
        (false (search long-dest out))
        (false (search long-ret out))
        (true (search (concatenate 'string
                                   (make-string (- dest-cap 3) :initial-element #\X) "...")
                      out))
        (true (search (concatenate 'string (subseq long-ret 0 (- ret-cap 3)) "...") out))
        ;; lines: titles, rule, "Mon Apr 20", row, "", "Tue Apr 21", row
        (let ((dep-col (search "Dep " header))
              (ret-col (search "Ret " header))
              (pax-col (search "Dep Pax" header))
              (row1 (nth 3 lines))
              (row2 (nth 6 lines)))
          (is = dep-col (search "8 am" row1))
          (is = ret-col (search "after rec" row1))
          (is = pax-col (search "MM" row1))
          (is = dep-col (search "9 am" row2))
          (is = ret-col (search "11 am" row2))
          (is = pax-col (search "BW" row2))))
    ;; a destination exactly at the cap renders in full
    (let* ((at-cap (make-string (cdr (assoc "Destination" vb::*column-width-caps*
                                            :test #'string=))
                                :initial-element #\Y))
           (out (vb::format-schedule-table
                 (list (make-trip :day "Monday April 20" :vehicle :dgh
                                  :destination at-cap :departure "8 am"
                                  :return-time "10 am" :dep-passengers '("MM"))))))
      (true (search at-cap out))
      (false (search "..." out)))
    ;; short destinations keep the narrower, content-sized column
    (let ((out (vb::format-schedule-table
                (list (make-trip :day "Monday April 20" :vehicle :dgh
                                 :destination "SHMS" :departure "8 am"
                                 :return-time "10 am" :dep-passengers '("MM"))))))
      (true (search "Destination  Dep" out))))

  (define-test view-schedule-default-today
      ;; With an empty sheet, view-schedule returns the "no trips" message.
      ;; Pins a week range covering every day of the year so the default
      ;; "today" always finds a sheet, whatever today's date is.
      (let ((vb::*weeks* '(("Jan 1-Dec 31" nil)))
            (sheet (make-test-sheet)))
        (call-with-mock-read-range sheet
                                   (lambda ()
                                     (let ((out (view-schedule)))
                                       (true (search "No trips scheduled" out)))))))

  (define-test view-schedule-renders-trips
      (let* ((day-str (vb::format-booking-day (chronicity:parse "April 22")))
             (block (make-block (format nil "~A           ~A" day-str day-str)
                                '(:dgh "SHMS")
                                '(:dgh ("8:35 am" "10:30 am"))
                                '(:dgh (("MM" "DM") ("MM" "DM")))))
             (sheet (make-test-sheet block)))
        (call-with-mock-read-range sheet
                                   (lambda ()
                                     (let ((out (let ((vb::*weeks* *sample-weeks*))
                                                  (view-schedule "April 22"))))
                                       (true (search "SHMS" out))
                                       (true (search "8:35 am" out))
                                       (true (search "DGH" out))
                                       (true (search "MM, DM" out))
                                       (true (search "Vehicle" out)))))))

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
               (let ((out (let ((vb::*weeks* *sample-weeks*))
                            (view-schedule (format nil "December 25 ~D" year)))))
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
                                     (let* ((out (let ((vb::*weeks* *sample-weeks*))
                                                   (view-schedule "April 22")))
                                            (morning-pos (search "Morning" out))
                                            (afternoon-pos (search "Afternoon" out)))
                                       (true morning-pos)
                                       (true afternoon-pos)
                                       (true (< morning-pos afternoon-pos))))))))

(define-test format-schedule-table-groups-by-day
  :parent view-schedule-tests
  (let* ((trips (list (make-trip :day "Monday April 20" :vehicle :dgh :destination "SHMS"
                                 :departure "8 am" :return-time "10 am" :dep-passengers '("MM"))
                      (make-trip :day "Monday April 20" :vehicle :lgh :destination "Bank"
                                 :departure "1 pm" :return-time "2 pm" :dep-passengers '("DM"))
                      (make-trip :day "Tuesday April 21" :vehicle :chevy :destination "Pool"
                                 :departure "9 am" :return-time "11 am" :dep-passengers '("CG"))))
         (out (vb::format-schedule-table trips))
         (lines (cl-ppcre:split "\\n" out)))
    ;; titles and rule first, indented like the rows
    (true (uiop:string-prefix-p "  Vehicle  Destination  " (first lines)))
    (true (uiop:string-prefix-p "  -------  -----------  " (second lines)))
    ;; one flush-left heading per day over its indented rows, a blank line
    ;; between days, and the date nowhere else
    (is equal '("Mon Apr 20" "  DGH" "  LGH" "" "Tue Apr 21" "  CHEVY")
        (mapcar (lambda (line)
                  (if (uiop:string-prefix-p "  " line)
                      (subseq line 0 (position #\Space line :start 2))
                      line))
                (nthcdr 2 lines)))
    (is = 1 (count "Apr 20" lines :test #'search))
    ;; no trailing spaces on any line
    (dolist (line lines)
      (is string= line (string-right-trim " " line)))))

(define-test phrase-date-range-tests
  :parent view-schedule-tests
  ;; Pinned to Wednesday April 22 2026 so the sample sheets "Apr 20-26" and
  ;; "Apr 27-May 3" are the ones in play, whatever today is.
  (let ((chronicity:*now* (chronicity:make-datetime 2026 4 22 12 0 0))
        (vb::*weeks* *sample-weeks*))
    ;; week phrases cover the sheet's Monday-to-Sunday week
    (multiple-value-bind (s e) (vb::phrase-date-range "this week")
      (is = 20260420 (vb::date-key s))
      (is = 20260426 (vb::date-key e)))
    (multiple-value-bind (s e) (vb::phrase-date-range "next week")
      (is = 20260427 (vb::date-key s))
      (is = 20260503 (vb::date-key e)))
    (multiple-value-bind (s e) (vb::phrase-date-range "last week")
      (is = 20260413 (vb::date-key s))
      (is = 20260419 (vb::date-key e)))
    ;; filler around "week" is ignored: these all mean this Monday to Sunday
    (dolist (p '("the week" "the whole week" "whole week" "week" "all week"
                 "all of this week" "this whole week" "the entire week"
                 "all week long" "The Whole Week"))
      (multiple-value-bind (s e) (vb::phrase-date-range p)
        (is equal 20260420 (and s (vb::date-key s)) "start of ~s" p)
        (is equal 20260426 (and e (vb::date-key e)) "end of ~s" p)))
    (dolist (p '("the whole of next week" "all of next week" "the coming week"
                 "the following week"))
      (multiple-value-bind (s e) (vb::phrase-date-range p)
        (is equal 20260427 (and s (vb::date-key s)) "start of ~s" p)
        (is equal 20260503 (and e (vb::date-key e)) "end of ~s" p)))
    (multiple-value-bind (s e) (vb::phrase-date-range "the previous week")
      (is = 20260413 (vb::date-key s))
      (is = 20260419 (vb::date-key e)))
    ;; "the rest of the week" starts today (a Wednesday) and still ends Sunday
    (multiple-value-bind (s e) (vb::phrase-date-range "the rest of the week")
      (is = 20260422 (vb::date-key s))
      (is = 20260426 (vb::date-key e)))
    ;; a weekend is the Saturday and Sunday of the same Monday-to-Sunday week
    (multiple-value-bind (s e) (vb::phrase-date-range "the weekend")
      (is = 20260425 (vb::date-key s))
      (is = 20260426 (vb::date-key e)))
    (multiple-value-bind (s e) (vb::phrase-date-range "next weekend")
      (is = 20260502 (vb::date-key s))
      (is = 20260503 (vb::date-key e)))
    ;; anything else is left to chronicity
    (false (vb::week-phrase-range "Wednesday"))
    (false (vb::week-phrase-range "weekly"))
    (false (vb::week-phrase-range "the week after next"))
    ;; a single day is a one-day window
    (multiple-value-bind (s e) (vb::phrase-date-range "today")
      (is = 20260422 (vb::date-key s))
      (is = 20260422 (vb::date-key e)))
    (multiple-value-bind (s e) (vb::phrase-date-range "tomorrow")
      (is = 20260423 (vb::date-key s))
      (is = 20260423 (vb::date-key e)))
    (multiple-value-bind (s e) (vb::phrase-date-range "Apr 20")
      (is = 20260420 (vb::date-key s))
      (is = 20260420 (vb::date-key e)))
    (multiple-value-bind (s e) (vb::phrase-date-range "Saturday")
      (is = 20260425 (vb::date-key s))
      (is = 20260425 (vb::date-key e)))
    (false (vb::phrase-date-range "gibberish nonsense"))
    ;; the /help example: both weeks' sheets are read and the window is labelled
    (let ((sheets-read '()))
      (with-mocked-functions ((vb::read-range (lambda (range &key sheet-name)
                                                (declare (ignore range))
                                                (push sheet-name sheets-read)
                                                (make-test-sheet))))
        (let ((out (view-schedule "this week and next week")))
          (true (search "No trips scheduled from Monday April 20 to Sunday May 3" out))
          (is equal '("Apr 20-26" "Apr 27-May 3") (reverse sheets-read)))))
    ;; the phrases that once failed in Slack ("/view the whole week",
    ;; "/view the week") read as this week end to end
    (call-with-mock-read-range (make-test-sheet)
                               (lambda ()
                                 (dolist (p '("the whole week" "the week" "all week"))
                                   (true (search "from Monday April 20 to Sunday April 26"
                                                 (view-schedule p))
                                         "view-schedule ~s" p))
                                 (true (search "from Wednesday April 22 to Sunday April 26"
                                               (view-schedule "the rest of the week")))
                                 (true (search "from Saturday April 25 to Sunday May 3"
                                               (view-schedule "the weekend and next week")))))
    ;; a reversed range still works, and an unreadable one names the whole phrase
    (call-with-mock-read-range (make-test-sheet)
                               (lambda ()
                                 (true (search "from Monday April 20 to Sunday April 26"
                                               (view-schedule "Apr 26 to Apr 20")))))
    (let ((c (handler-case (progn (view-schedule "this week and nonsense") nil)
               (error (c) c))))
      (true c)
      (true (search "this week and nonsense" (princ-to-string c))))))

(define-test parse-passenger-initials
  (is equal '("CG" "BW") (vb::parse-passenger-initials "CG and BW"))
  (is equal '("MM" "DM" "CG") (vb::parse-passenger-initials "MM, DM and CG"))
  (is equal '("MM") (vb::parse-passenger-initials "MM"))
  (is equal '() (vb::parse-passenger-initials "no initials here")))

(define-test trips-and-destinations
  (define-test known-destinations-are-deduplicated
    (is equal '("SHMS" "Shrine")
        (vb::known-destinations
         (list (make-trip :destination "SHMS")
               (make-trip :destination "")
               (make-trip :destination "Shrine")
               (make-trip :destination "shms")
               (make-trip :destination nil))))
    (is equal '() (vb::known-destinations '())))

  (define-test fetch-all-trips-reads-every-week
    (let ((reads '())
          (block (make-block "Monday April 20           Monday April 20"
                             '(:dgh "SHMS") '(:dgh ("8 am" "10 am")) '(:dgh (("MM") ("MM"))))))
      (with-mocked-functions ((vb::read-range (lambda (range &key sheet-name)
                                                (declare (ignore range))
                                                (push sheet-name reads)
                                                (make-test-sheet block))))
        (let ((vb::*weeks* '(("Apr 13-19 " nil) ("Apr 20-26" nil))))
          (let ((trips (vb::fetch-all-trips)))
            (is equal '("Apr 13-19 " "Apr 20-26") (reverse reads))
            ;; 4 vehicles x 1 block x 2 weeks
            (is = 8 (length trips))
            (is equal '("Apr 13-19 " "Apr 20-26")
                (remove-duplicates (mapcar #'trip-sheet-name trips) :test #'string= :from-end t))))
        (setf reads '())
        (let ((trips (vb::fetch-all-trips :week "Apr 6-12")))
          (is equal '("Apr 6-12") reads)
          (is = 4 (length trips)))))))

(defun sheet-with-shms ()
  "A one-block sheet for \"Thursday April 9\" with MM on the DGH SHMS trip."
  (make-test-sheet
   (make-block "Thursday April 9           Thursday April 9"
               '(:dgh "SHMS") '(:dgh ("8:35 am" "10:30 am")) '(:dgh (("MM") ("MM"))))))

(defun empty-sheet ()
  "A one-block sheet for \"Thursday April 9\" with every vehicle free."
  (make-test-sheet
   (make-block "Thursday April 9           Thursday April 9" '() '() '())))

(defun call-recording-writes (thunk)
  "Run THUNK with WRITE-RANGE mocked and *STANDARD-OUTPUT* captured.
Returns (values OUTPUT WRITES); WRITES is a list of (range values sheet-name)
in call order."
  (let ((writes '()))
    (let ((out (with-output-to-string (*standard-output*)
                 (with-mocked-functions ((vb::write-range
                                          (lambda (range values &key sheet-name)
                                            (push (list range values sheet-name) writes))))
                   (funcall thunk)))))
      (values out (nreverse writes)))))

(define-test book-tests
  ;; Fixtures (see SHEET-WITH-SHMS / EMPTY-SHEET): one block for
  ;; "Thursday April 9" starting at sheet row 3.  DGH occupies columns F (dep)
  ;; and G (ret); passengers start at row 7.
  (define-test joins-existing-trip-without-times
    (let ((trips (vb::parse-schedule (sheet-with-shms) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda () (vb:book "Thursday April 9" "SHMS" '("GG") :trips trips)))
        (true (search "Found 1 existing trip" out))
        ;; MM already occupies row 7, so GG goes to row 8 in both columns
        (is equal '(("F8:F8" (("GG")) "test-sheet")
                    ("G8:G8" (("GG")) "test-sheet"))
            writes))))

  (define-test trips-argument-skips-the-sheet-read
    (let ((trips (vb::parse-schedule (sheet-with-shms) :sheet-name "test-sheet"))
          (read-called nil))
      (with-mocked-functions ((vb::read-range (lambda (range &key sheet-name)
                                                (declare (ignore range sheet-name))
                                                (setf read-called t)
                                                nil)))
        (call-recording-writes
         (lambda () (vb:book "Thursday April 9" "SHMS" '("GG") :trips trips)))
        (false read-called))))

  (define-test week-argument-reads-one-sheet
    (let ((reads '()))
      (with-mocked-functions ((vb::read-range (lambda (range &key sheet-name)
                                                (declare (ignore range))
                                                (push sheet-name reads)
                                                (sheet-with-shms))))
        (multiple-value-bind (out writes)
            (call-recording-writes
             (lambda () (vb:book "Thursday April 9" "SHMS" '("GG") :week "Apr 6-12")))
          (declare (ignore out))
          (is equal '("Apr 6-12") reads)
          (is equal '("F8:F8" "G8:G8") (mapcar #'first writes))
          (is equal '("Apr 6-12" "Apr 6-12") (mapcar #'third writes))))))

  (define-test creates-new-booking-with-times
    (let ((trips (vb::parse-schedule (empty-sheet) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (vb:book "Thursday April 9" "Dentist" '("BW" "CG")
                      :dep-time "8:00 am" :ret-time "10:00 am" :trips trips)))
        (true (search "Created DGH booking" out))
        (is equal '(("F4" (("Dentist")) "test-sheet")
                    ("F6" (("8:00 am")) "test-sheet")
                    ("G6" (("10:00 am")) "test-sheet")
                    ("F7:F8" (("BW") ("CG")) "test-sheet")
                    ("G7:G8" (("BW") ("CG")) "test-sheet"))
            writes))))

  (define-test vehicle-override-uses-that-vehicles-columns
    (let ((trips (vb::parse-schedule (empty-sheet) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (vb:book "Thursday April 9" "Dentist" '("BW")
                      :dep-time "8:00 am" :ret-time "10:00 am"
                      :vehicle :chevy :trips trips)))
        (true (search "Created CHEVY booking" out))
        (is equal '("C4" "C6" "D6" "C7:C7" "D7:D7") (mapcar #'first writes)))))

  (define-test missing-both-times-signals-booking-incomplete
    (let* ((trips (vb::parse-schedule (empty-sheet) :sheet-name "test-sheet"))
           (c (handler-case
                  (progn (vb:book "Thursday April 9" "Dentist" '("BW") :trips trips) nil)
                (bn:booking-incomplete (c) c))))
      (true c)
      (is equal '(:dep-time :ret-time) (bn:booking-incomplete-missing c))
      (is string= "Dentist" (getf (bn:booking-incomplete-understood c) :destination))
      (is string= "Thursday April 9" (getf (bn:booking-incomplete-understood c) :day))
      (is equal '("BW") (getf (bn:booking-incomplete-understood c) :passengers))
      (true (search "No existing trip to Dentist on Thursday April 9"
                    (bn:booking-incomplete-preamble c)))
      (true (search "Please re-send" (princ-to-string c)))))

  (define-test missing-one-time-names-only-that-one
    (let* ((trips (vb::parse-schedule (empty-sheet) :sheet-name "test-sheet"))
           (c (handler-case
                  (progn (vb:book "Thursday April 9" "Dentist" '("BW")
                                  :dep-time "8:00 am" :vehicle :lgh :trips trips)
                         nil)
                (bn:booking-incomplete (c) c))))
      (is equal '(:ret-time) (bn:booking-incomplete-missing c))
      (is string= "8:00 am" (getf (bn:booking-incomplete-understood c) :dep-time))
      (is eq :lgh (getf (bn:booking-incomplete-understood c) :vehicle))))

  (define-test no-empty-slot-reports-and-returns-nil
    (let* ((sheet (make-test-sheet
                   (make-block "Thursday April 9           Thursday April 9"
                               '(:dgh "A" :lgh "B" :chevy "C")
                               '(:dgh ("8 am" "9 am") :lgh ("8 am" "9 am") :chevy ("8 am" "9 am"))
                               '(:dgh (("MM") ("MM")) :lgh (("DM") ("DM")) :chevy (("CG") ("CG"))))))
           (trips (vb::parse-schedule sheet :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (false (vb:book "Thursday April 9" "Dentist" '("BW")
                             :dep-time "8:00 am" :ret-time "10:00 am" :trips trips))))
        (true (search "No available vehicle slots" out))
        (is equal '() writes)))))

(define-test day-without-a-sheet-reports-it
  :parent book-tests
  ;; Only "Thursday April 9" is on the sheet, so a booking for another day
  ;; names the missing sheet instead of claiming every vehicle is taken (with
  ;; times) or asking for times (without them).
  (let ((trips (vb::parse-schedule (empty-sheet) :sheet-name "test-sheet")))
    (multiple-value-bind (out writes)
        (call-recording-writes
         (lambda ()
           (false (vb:book "Tuesday October 20" "Dentist" '("BW")
                           :dep-time "8:00 am" :ret-time "10:00 am" :trips trips))
           (false (vb:book "Tuesday October 20" "Dentist" '("BW") :trips trips))))
      (true (search "No schedule sheet covers Tuesday October 20" out))
      (false (search "No available vehicle slots" out))
      (is equal '() writes))))

(defun fixture-header ()
  "Sheet header text for April 9 of the current year, whatever weekday that is."
  (let* ((year (nth-value 5 (decode-universal-time (get-universal-time))))
         (day-str (bn:format-booking-day (chronicity:parse (format nil "April 9 ~d" year)))))
    (format nil "~a           ~a" day-str day-str)))

(defun run-book-natural (text sheet)
  "Run BOOK-NATURAL on TEXT with the sheet reads returning SHEET, writes
recorded and only one week configured.
Returns (values WRITES OUTPUT READS CONDITION): WRITES is a list of
(range values sheet-name), READS the number of sheet reads, CONDITION the
BOOKING-INCOMPLETE signalled, if any."
  (let ((writes '()) (reads 0) (condition nil) (out ""))
    (let ((vb::*weeks* '(("Apr 6-12" nil))))
      (with-mocked-functions ((vb::read-range (lambda (range &key sheet-name)
                                                (declare (ignore range sheet-name))
                                                (incf reads)
                                                sheet))
                              (vb::write-range (lambda (range values &key sheet-name)
                                                 (push (list range values sheet-name) writes))))
        (setf out (with-output-to-string (*standard-output*)
                    (handler-case (vb:book-natural text)
                      (bn:booking-incomplete (c) (setf condition c)))))))
    (values (nreverse writes) out reads condition)))

(define-test book-natural-tests
  ;; End-to-end through the parser and BOOK with the sheet mocked (see
  ;; RUN-BOOK-NATURAL).  The fixture day is "April 9" of the current year so
  ;; the parsed day string matches the sheet header whatever the weekday.
  (define-test creates-a-new-booking
    (multiple-value-bind (writes out reads condition)
        (run-book-natural "Can BW and CG take the van to the dentist on April 9 from 8 am to 10 am?"
                          (make-test-sheet (make-block (fixture-header) '() '() '())))
      (false condition)
      (is = 1 reads)
      (true (search "Created DGH booking" out))
      (is equal '(("F4" (("dentist")) "Apr 6-12")
                  ("F6" (("8:00 am")) "Apr 6-12")
                  ("G6" (("10:00 am")) "Apr 6-12")
                  ("F7:F8" (("BW") ("CG")) "Apr 6-12")
                  ("G7:G8" (("BW") ("CG")) "Apr 6-12"))
          writes)))

  (define-test honours-vehicle-preference
    (multiple-value-bind (writes out)
        (run-book-natural "take the LGH to the dentist on April 9 for BW, 8 am to 10 am"
                          (make-test-sheet (make-block (fixture-header) '() '() '())))
      (true (search "Created LGH booking" out))
      (is equal '("I4" "I6" "J6" "I7:I7" "J7:J7") (mapcar #'first writes))))

  (define-test joins-an-existing-trip-without-times
    (multiple-value-bind (writes out)
        (run-book-natural "Please add GG to the SHMS trip on April 9"
                          (make-test-sheet
                           (make-block (fixture-header) '(:dgh "SHMS")
                                       '(:dgh ("8:35 am" "10:30 am"))
                                       '(:dgh (("MM") ("MM"))))))
      (true (search "Found 1 existing trip" out))
      (is equal '(("F8:F8" (("GG")) "Apr 6-12")
                  ("G8:G8" (("GG")) "Apr 6-12"))
          writes)))

  (define-test uses-the-sheets-destination-spelling
    ;; "saint marys church" in the message resolves to the sheet's
    ;; "Saint Mary's Church" and joins that trip instead of creating one.
    (multiple-value-bind (writes out)
        (run-book-natural "MM to saint marys church on April 9"
                          (make-test-sheet
                           (make-block (fixture-header) '(:lgh "Saint Mary's Church")
                                       '(:lgh ("5:00 pm" "8:00 pm"))
                                       '(:lgh (("MK") ("MK"))))))
      (true (search "Found 1 existing trip" out))
      (is equal '("I8:I8" "J8:J8") (mapcar #'first writes))))

  (define-test skips-passengers-already-on-the-trip
    (multiple-value-bind (writes out)
        (run-book-natural "add MM and GG to the SHMS trip on April 9"
                          (make-test-sheet
                           (make-block (fixture-header) '(:dgh "SHMS")
                                       '(:dgh ("8:35 am" "10:30 am"))
                                       '(:dgh (("MM") ("MM"))))))
      (true (search "Skipped (already booked): MM" out))
      (is equal '(("F8:F8" (("GG")) "Apr 6-12")
                  ("G8:G8" (("GG")) "Apr 6-12"))
          writes)))

  (define-test missing-day-is-reported-before-any-sheet-read
    (multiple-value-bind (writes out reads condition)
        (run-book-natural "for the pool for MM 8 am to 10 am" (make-test-sheet))
      (declare (ignore out))
      (true condition)
      (is = 0 reads)
      (is equal '() writes)
      (is equal '(:day) (bn:booking-incomplete-missing condition))
      (is string= "pool" (getf (bn:booking-incomplete-understood condition) :destination))
      (is string= "for the pool for MM 8 am to 10 am" (bn:booking-incomplete-text condition))))

  (define-test missing-destination-and-passengers
    (multiple-value-bind (writes out reads condition)
        (run-book-natural "tomorrow at 8 am" (make-test-sheet))
      (declare (ignore writes out))
      (is = 0 reads)
      (is equal '(:destination :passengers) (bn:booking-incomplete-missing condition))))

  (define-test no-trip-and-no-times-asks-for-times
    (multiple-value-bind (writes out reads condition)
        (run-book-natural "add BW to the pool trip on April 9"
                          (make-test-sheet (make-block (fixture-header) '() '() '())))
      (declare (ignore out))
      (true condition)
      (is = 1 reads)
      (is equal '() writes)
      (is equal '(:dep-time :ret-time) (bn:booking-incomplete-missing condition))
      (true (search "No existing trip to pool" (bn:booking-incomplete-preamble condition)))
      (is string= "pool" (getf (bn:booking-incomplete-understood condition) :destination))))

  (define-test only-return-time-missing
    (multiple-value-bind (writes out reads condition)
        (run-book-natural "BW to the pool on April 9 leaving at 8 am"
                          (make-test-sheet (make-block (fixture-header) '() '() '())))
      (declare (ignore writes out reads))
      (is equal '(:ret-time) (bn:booking-incomplete-missing condition))
      (is string= "8:00 am" (getf (bn:booking-incomplete-understood condition) :dep-time))))

  (define-test book-natural-calls-book
    ;; The parsed pieces reach BOOK in the sheet's formats, with the
    ;; pre-fetched trips passed through.
    (let ((book-args nil))
      (with-mocked-functions ((vb::fetch-all-trips (lambda (&key week) (declare (ignore week))
                                                     (list (make-trip :destination "Pool"))))
                              (vb::book (lambda (day dest pax &key dep-time ret-time vehicle week trips)
                                          (setf book-args (list :day day :dest dest :pax pax
                                                                :dep dep-time :ret ret-time
                                                                :vehicle vehicle :week week
                                                                :trips trips)))))
        (vb:book-natural
         "I want to book the DGH for the pool next week Tuesday for CG and BW at seven PM to nine PM"
         :week "Apr 6-12")
        (is string= "Pool" (getf book-args :dest))
        (is equal '("CG" "BW") (getf book-args :pax))
        (true (cl-ppcre:scan "^\\w+ \\w+ \\d+$" (getf book-args :day)))
        (is string= "7:00 pm" (getf book-args :dep))
        (is string= "9:00 pm" (getf book-args :ret))
        (is eq :dgh (getf book-args :vehicle))
        (is string= "Apr 6-12" (getf book-args :week))
        (is = 1 (length (getf book-args :trips)))))))

(define-test sheet-io-tests
  (define-test write-range-payload
    (let ((requests '()) (gets '()))
      (with-mocked-functions ((dexador:request
                               (lambda (uri &rest args)
                                 (push (list* uri args) requests)
                                 (let ((headers (make-hash-table :test #'equal)))
                                   (setf (gethash "location" headers) "https://example.invalid/result")
                                   (values "" 302 headers))))
                              (dexador:get (lambda (uri &rest args)
                                             (declare (ignore args))
                                             (push uri gets)
                                             "ok")))
        (is string= "ok" (vb:write-range "F4" '(("SHMS")) :sheet-name "Apr 20-26"))
        (is = 1 (length requests))
        (is equal '("https://example.invalid/result") gets)
        (destructuring-bind (uri &key method content headers max-redirects) (first requests)
          (true (search "script.google.com" uri))
          (true (search (format nil "spreadsheetId=~a" vb::*spreadsheet-id*) uri))
          (is eq :post method)
          (is = 0 max-redirects)
          (is string= "application/json" (cdr (assoc "Content-Type" headers :test #'string=)))
          (let* ((payload (com.inuoe.jzon:parse content))
                 (arguments (gethash "arguments" payload))
                 (body (gethash "body" payload)))
            (is string= "Apr 20-26!F4" (gethash "range" arguments))
            (is string= "USER_ENTERED" (gethash "valueInputOption" arguments))
            (is equalp #(#("SHMS")) (gethash "values" body)))))
      ;; without a sheet name the range is unqualified
      (with-mocked-functions ((dexador:request
                               (lambda (uri &rest args)
                                 (declare (ignore uri))
                                 (push args requests)
                                 (let ((headers (make-hash-table :test #'equal)))
                                   (setf (gethash "location" headers) "https://example.invalid/r")
                                   (values "" 302 headers))))
                              (dexador:get (lambda (uri &rest args) (declare (ignore uri args)) "ok")))
        (vb:write-range "F7:F8" '(("BW") ("CG")))
        (let ((payload (com.inuoe.jzon:parse (getf (first requests) :content))))
          (is string= "F7:F8" (gethash "range" (gethash "arguments" payload)))
          (is equalp #(#("BW") #("CG")) (gethash "values" (gethash "body" payload)))))))

  (define-test write-range-without-redirect-errors
    (with-mocked-functions ((dexador:request
                             (lambda (uri &rest args)
                               (declare (ignore uri args))
                               (values "" 200 (make-hash-table :test #'equal)))))
      (fail (vb:write-range "F4" '(("SHMS")) :sheet-name "Apr 20-26") error)))

  (define-test read-range-urls
    (let ((urls '()))
      (with-mocked-functions ((dexador:get (lambda (uri &rest args)
                                             (declare (ignore args))
                                             (push uri urls)
                                             (format nil "a,b~%c,d~%"))))
        ;; gviz endpoint for a named sheet
        (let ((rows (vb:read-range nil :sheet-name "Apr 20-26")))
          (is equal '(("a" "b") ("c" "d")) rows)
          (true (search "gviz/tq" (first urls)))
          (true (search "sheet=Apr%2020-26" (first urls)))
          (true (search "out:csv" (first urls)))
          (true (search vb::*spreadsheet-id* (first urls))))
        ;; CSV export for a range on the default gid
        (vb:read-range "A1:B2")
        (true (search "export?format=csv" (first urls)))
        (true (search "range=A1:B2" (first urls)))
        (true (search (format nil "gid=~a" vb::*gid*) (first urls)))))))

(define-test sheet-discovery-tests
  ;; The week tabs are discovered from the spreadsheet's public htmlview
  ;; page rather than hardcoded, so /view keeps working as weeks are added.
  (define-test decode-js-string
    (is string= "Mar 30-Apr 5 (Holy wk)" (vb::decode-js-string "Mar 30-Apr 5 (Holy wk)"))
    (is string= "St's / A \"q\"" (vb::decode-js-string "St\\x27s \\/ \\u0041 \\\"q\\\""))
    (is string= "back\\slash" (vb::decode-js-string "back\\\\slash"))
    ;; a dangling backslash or a bad hex escape is passed through, not an error
    (is string= "trailing\\" (vb::decode-js-string "trailing\\"))
    (is string= "badxZZ" (vb::decode-js-string "bad\\xZZ")))

  (define-test sheet-tabs-parses-htmlview
    (let ((urls '()))
      (with-mocked-functions ((dexador:get (lambda (uri &rest args)
                                             (declare (ignore args))
                                             (push uri urls)
                                             *htmlview-fixture*)))
        ;; every tab, in tab order, names decoded and trailing space kept
        (is equal '(("Aug 31-Sep 6" "1696388518")
                    ("Sep 7-13" "217444495")
                    ("Mar 30-Apr 5 (Holy wk)" "367982190")
                    ("St's / Dates" "1762101409")
                    ("Apr 13-19 " "5"))
            (vb:sheet-tabs))
        (is = 1 (length urls))
        (true (search "/htmlview" (first urls)))
        (true (search vb::*spreadsheet-id* (first urls))))))

  (define-test sheet-tabs-errors-without-tabs
    (with-mocked-functions ((dexador:get (lambda (uri &rest args)
                                           (declare (ignore uri args))
                                           "<html><body>Sign in</body></html>")))
      (fail (vb:sheet-tabs) error)))

  (define-test week-sheets-filters-caches-and-refreshes
    (let ((fetches 0)
          (vb::*weeks* nil)
          (vb::*week-sheets-cache* nil)
          (vb::*week-sheets-cache-seconds* 300))
      (with-mocked-functions ((vb:sheet-tabs
                               (lambda ()
                                 (incf fetches)
                                 (if (= fetches 1)
                                     '(("Aug 31-Sep 6" "1") ("Fall 2026" "2") ("Sep 7-13" "3")
                                       ("Dates" "4") ("Blank Temp" "5") ("Week 1-2" "6"))
                                     '(("Sep 14-20" "7"))))))
        ;; only week-shaped tab names survive, in tab order
        (is equal '(("Aug 31-Sep 6" "1") ("Sep 7-13" "3")) (vb:week-sheets))
        (is = 1 fetches)
        ;; a second call within the TTL reuses the cache
        (is equal '(("Aug 31-Sep 6" "1") ("Sep 7-13" "3")) (vb:week-sheets))
        (is = 1 fetches)
        ;; :refresh forces a new fetch
        (is equal '(("Sep 14-20" "7")) (vb:week-sheets :refresh t))
        (is = 2 fetches)
        ;; an expired cache is fetched again
        (let ((vb::*week-sheets-cache-seconds* 0))
          (is equal '(("Sep 14-20" "7")) (vb:week-sheets))
          (is = 3 fetches))
        ;; a fixed *weeks* bypasses discovery entirely
        (let ((vb::*weeks* '(("Apr 20-26" nil))))
          (is equal '(("Apr 20-26" nil)) (vb:week-sheets))
          (is = 3 fetches)))))

  (define-test lookups-use-discovered-weeks
    ;; The regression behind "No schedule sheets cover Friday September 4":
    ;; a week present in the spreadsheet but absent from any hardcoded list.
    (let ((vb::*weeks* nil)
          (vb::*week-sheets-cache* nil)
          (year (chronicity:year-of (chronicity:parse "today"))))
      (with-mocked-functions ((vb:sheet-tabs
                               (lambda () '(("Aug 31-Sep 6" "1") ("Dates" "2") ("Sep 7-13" "3")))))
        (is string= "Aug 31-Sep 6"
            (vb::find-week-sheet (chronicity:parse (format nil "September 4 ~D" year))))
        (is equal '("Aug 31-Sep 6" "Sep 7-13")
            (vb::find-week-sheets-in-range (+ (* year 10000) 904) (+ (* year 10000) 908)))
        (false (vb::find-week-sheet (chronicity:parse (format nil "December 25 ~D" year))))
        ;; fetch-all-trips reads exactly the discovered week sheets
        (let ((reads '()))
          (with-mocked-functions ((vb::read-range (lambda (range &key sheet-name)
                                                    (declare (ignore range))
                                                    (push sheet-name reads)
                                                    (make-test-sheet))))
            (vb:fetch-all-trips)
            (is equal '("Aug 31-Sep 6" "Sep 7-13") (reverse reads))))))))

(define-test driver-helpers
  (define-test departure-universal-time
    (let ((ut (vb::trip-departure-universal-time (make-trip :departure "8:35 am"))))
      (true ut)
      (multiple-value-bind (s m h day month year) (decode-universal-time ut)
        (multiple-value-bind (ts tm th tday tmonth tyear) (decode-universal-time (get-universal-time))
          (declare (ignore ts tm th))
          (is = 0 s)
          (is = 35 m)
          (is = 8 h)
          (is = tday day)
          (is = tmonth month)
          (is = tyear year))))
    (multiple-value-bind (s m h) (decode-universal-time
                                  (vb::trip-departure-universal-time (make-trip :departure "12:35 pm")))
      (declare (ignore s))
      (is = 35 m)
      (is = 12 h))
    (false (vb::trip-departure-universal-time (make-trip :departure "")))
    (false (vb::trip-departure-universal-time (make-trip :departure nil)))
    (false (vb::trip-departure-universal-time (make-trip :departure "garbage"))))

  (define-test driver-trips-today
    (let* ((day-str (bn:format-booking-day (chronicity:parse "today")))
           (sheet (make-test-sheet
                   (make-block (format nil "~a           ~a" day-str day-str)
                               '(:dgh "SHMS" :lgh "Shrine")
                               '(:dgh ("8:00 am" "10:00 am") :lgh ("5:00 pm" "8:00 pm"))
                               '(:dgh (("BW" "CG") ("BW" "CG")) :lgh (("CG") ("CG")))))))
      (call-with-mock-read-range
       sheet
       (lambda ()
         ;; a week range covering every day of the year
         (let ((vb::*weeks* '(("Jan 1-Dec 31" nil))))
           (let ((trips (vb::trips-for-driver-today "BW")))
             (is = 1 (length trips))
             (is string= "SHMS" (trip-destination (first trips))))
           ;; CG drives the LGH trip but only rides on the DGH one
           (let ((trips (vb::trips-for-driver-today "cg")))
             (is = 1 (length trips))
             (is string= "Shrine" (trip-destination (first trips))))
           (is equal '() (vb::trips-for-driver-today "ZZ")))
         ;; no sheet covers today
         (let ((vb::*weeks* '(("Feb 30-Feb 31" nil))))
           (is equal '() (vb::trips-for-driver-today "BW")))))))

  (define-test current-year-key
    (let ((year (nth-value 5 (decode-universal-time (get-universal-time)))))
      (is = (+ (* year 10000) 422)
          (vb::current-year-date-key (chronicity:parse "April 22 2030")))
      (is = (+ (* year 10000) 101)
          (vb::current-year-date-key (chronicity:parse "January 1 1999"))))))

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
  (test 'parse-passenger-initials)
  (test 'trips-and-destinations)
  (test 'book-tests)
  (test 'book-natural-tests)
  (test 'sheet-io-tests)
  (test 'sheet-discovery-tests)
  (test 'driver-helpers)
  (test 'month-abbrev-num)
  (test 'date-key)
  (test 'parse-week-range)
  (test 'find-week-sheet)
  (test 'remind-driver-tests)
  (test 'view-schedule-tests))
