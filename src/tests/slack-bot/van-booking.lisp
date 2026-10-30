;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/slack-bot/van-booking
  (:use #:common-lisp)
  (:import-from #:rove #:deftest #:testing #:ok #:ng #:signals)
  (:local-nicknames (#:vb #:project-isidore/slack-bot/van-booking)
                    (#:bn #:project-isidore/slack-bot/booking-nlp))
  (:import-from #:project-isidore/slack-bot/van-booking
                #:make-trip
                #:trip-block-start-row #:trip-day #:trip-vehicle #:trip-destination
                #:trip-departure #:trip-return-time
                #:trip-dep-passengers #:trip-ret-passengers
                #:trip-dep-col #:trip-ret-col #:trip-sheet-name
                #:remind-driver #:view-schedule)
  (:import-from #:project-isidore/tests/test-utils #:with-mocked-functions)
  (:import-from #:chronicity)
  (:import-from #:com.inuoe.jzon)
  (:import-from #:dexador)
  (:import-from #:quri)
  (:documentation
   "Tests for project-isidore/slack-bot/van-booking: sheet parsing, trip
lookup, booking writes (with the Google Sheets calls mocked), the
BOOK-NATURAL front end, reminders and the schedule view.  Natural-language
parsing itself is covered in booking-nlp-tests."))

(in-package #:project-isidore/tests/slack-bot/van-booking)

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
  '("Mar 30-Apr 5 (Holy wk)" "Apr 6-12" "Apr 13-19 " "Apr 20-26" "Apr 27-May 3")
  "A fixed week-sheet list bound to VB::*WEEKS* by tests that look sheets up
by date, so they neither depend on the live spreadsheet's tabs nor fetch them.")

;;; --- Unit tests: utility functions ---

(deftest utilities
  (testing "col-letter"
    (ok (string= "A" (vb::col-letter 0)))
    (ok (string= "C" (vb::col-letter 2)))
    (ok (string= "F" (vb::col-letter 5)))
    (ok (string= "M" (vb::col-letter 12))))

  (testing "non-blank-p"
    (ok (vb::non-blank-p "hello"))
    (ok (vb::non-blank-p " x "))
    (ng (vb::non-blank-p ""))
    (ng (vb::non-blank-p "   "))
    (ng (vb::non-blank-p nil)))

  (testing "safe-nth"
    (ok (string= "b" (vb::safe-nth 1 '("a" "b" "c"))))
    (ok (string= "" (vb::safe-nth 5 '("a" "b"))))
    (ok (string= "" (vb::safe-nth 0 nil))))

  (testing "header-cell-p"
    (ok (vb::header-cell-p "Chevy"))
    (ok (vb::header-cell-p "chevy"))
    (ok (vb::header-cell-p "Dep."))
    (ok (vb::header-cell-p "MONDAY"))
    (ok (vb::header-cell-p "Sunday"))
    (ng (vb::header-cell-p "MM"))
    (ng (vb::header-cell-p "BW"))
    (ng (vb::header-cell-p "")))

  (testing "vehicle-cols"
    (multiple-value-bind (dep ret) (vb::vehicle-cols :dgh)
      (ok (= 5 dep))
      (ok (= 6 ret)))
    (multiple-value-bind (dep ret) (vb::vehicle-cols :chevy)
      (ok (= 2 dep))
      (ok (= 3 ret)))
    (ok (signals (vb::vehicle-cols :nonexistent) 'error)))

  (testing "extract-day-from-header"
    (ok (string= "Monday March 30"
                 (vb::extract-day-from-header
                  '("Monday March 30           Monday March 30" "" "Chevy"))))
    (ng (vb::extract-day-from-header '("" "" "Chevy")))
    (ok (string= "Tuesday April 7"
                 (vb::extract-day-from-header '("Tuesday April 7" "" "Chevy"))))))

;;; --- Unit tests: collect-passengers ---

(deftest collect-passengers
  (testing "basic-collection"
      (let ((rows '(("" "" "" "" "" "MM" "MM")
                    ("" "" "" "" "" "DM" "")
                    ("" "" "" "" "" "CG" "")
                    ("" "" "" "" "" "" ""))))
        (ok (equal '("MM" "DM" "CG") (vb::collect-passengers rows 0 5 10)))
        (ok (equal '("MM") (vb::collect-passengers rows 0 6 10)))))

  (testing "stops-at-header"
      (let ((rows '(("" "" "" "" "" "MM" "")
                    ("" "" "" "" "" "BW" "")
                    ("" "" "" "" "" "Chevy" "")
                    ("" "" "" "" "" "XX" ""))))
        (ok (equal '("MM" "BW") (vb::collect-passengers rows 0 5 10)))))

  (testing "stops-at-dep"
      (let ((rows '(("" "" "" "" "" "MM" "")
                    ("" "" "" "" "" "Dep." ""))))
        (ok (equal '("MM") (vb::collect-passengers rows 0 5 10)))))

  (testing "respects-max-rows"
      (let ((rows '(("" "" "" "" "" "A" "")
                    ("" "" "" "" "" "B" "")
                    ("" "" "" "" "" "C" ""))))
        (ok (equal '("A" "B") (vb::collect-passengers rows 0 5 2)))))

  (testing "empty-column"
      (let ((rows '(("" "" "" "" "" "" ""))))
        (ok (equal '() (vb::collect-passengers rows 0 5 10))))))

;;; --- Unit tests: parse-schedule ---

(deftest parse-schedule
  (testing "single-block"
      (let* ((block (make-block "Monday April 20           Monday April 20"
                                '(:dgh "SHMS" :lgh "Shrine")
                                '(:dgh ("8:35 am" "10:30 am") :lgh ("5:00 pm" "8:00 pm"))
                                '(:dgh (("MM" "DM") ("MM")) :lgh (("MK") ("MK")))))
             (sheet (make-test-sheet block))
             (trips (vb::parse-schedule sheet :sheet-name "test-sheet")))
        ;; 4 vehicles * 1 block = 4 trips
        (ok (= 4 (length trips)))
        ;; Check DGH trip
        (let ((dgh (find :dgh trips :key #'trip-vehicle)))
          (ok dgh)
          (ok (string= "Monday April 20" (trip-day dgh)))
          (ok (string= "SHMS" (trip-destination dgh)))
          (ok (string= "8:35 am" (trip-departure dgh)))
          (ok (string= "10:30 am" (trip-return-time dgh)))
          (ok (equal '("MM" "DM") (trip-dep-passengers dgh)))
          (ok (equal '("MM") (trip-ret-passengers dgh)))
          (ok (= 3 (trip-block-start-row dgh)))
          (ok (string= "test-sheet" (trip-sheet-name dgh))))
        ;; Check LGH trip
        (let ((lgh (find :lgh trips :key #'trip-vehicle)))
          (ok (string= "Shrine" (trip-destination lgh)))
          (ok (equal '("MK") (trip-dep-passengers lgh))))))

  (testing "multiple-blocks"
      (let* ((b1 (make-block "Monday April 20           Monday April 20"
                             '(:dgh "SHMS") '(:dgh ("8 am" "10 am")) '(:dgh (("MM") ("MM")))))
             (b2 (make-block ""  ; no day header = same day
                             '(:lgh "Shrine") '(:lgh ("5 pm" "8 pm")) '(:lgh (("MK") ("MK")))))
             (sheet (make-test-sheet b1 (block-spacer) b2))
             (trips (vb::parse-schedule sheet)))
        ;; 4 vehicles * 2 blocks = 8 trips
        (ok (= 8 (length trips)))
        ;; First block at row 3, second at row 18
        (let ((block1-trips (remove-if-not (lambda (tr) (= 3 (trip-block-start-row tr))) trips))
              (block2-trips (remove-if-not (lambda (tr) (= 18 (trip-block-start-row tr))) trips)))
          (ok (= 4 (length block1-trips)))
          (ok (= 4 (length block2-trips))))))

  (testing "block-rows-follow-the-sheet"
      ;; A block's start row is the row its header is on, not its ordinal
      ;; times fifteen.  Two rows inserted by hand under the first block (the
      ;; "Oct 5-11" tab had that under Sunday's first block) put the second
      ;; block on row 20, and that is where a booking for it must be written.
      (let* ((b1 (make-block "Sunday October 11           Sunday October 11"
                             '(:dgh "St. Paul") '(:dgh ("11:00 am" "2:30 pm")) '(:dgh (("GL") ("GL")))))
             (b2 (make-block "" '() '() '()))
             (sheet (make-test-sheet b1 (block-spacer) (block-spacer) (block-spacer) b2)))
        (ok (equal '(3 20)
                   (remove-duplicates (mapcar #'trip-block-start-row (vb::parse-schedule sheet)))))
        ))

  (testing "day-carries-forward"
      (let* ((b1 (make-block "Tuesday April 7           Tuesday April 7"
                             '() '() '()))
             (b2 (make-block "" '() '() '()))
             (sheet (make-test-sheet b1 b2))
             (trips (vb::parse-schedule sheet)))
        ;; Both blocks should have "Tuesday April 7"
        (ok (= 8 (length trips)))
        (dolist (tr trips)
          (ok (string= "Tuesday April 7" (trip-day tr))))))

  (testing "no-dest-row"
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
          (ok dgh)
          (ok (string= "" (trip-destination dgh)))
          (ok (string= "8:35 am" (trip-departure dgh)))
          (ok (equal '("MM") (trip-dep-passengers dgh))))))

  (testing "weekday-label-row"
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
        (ok (= 8 (length trips)))
        (dolist (tr trips)
          (ok (string= "Saturday September 5" (trip-day tr)))
          (ok (string= "" (trip-destination tr)))
          (ok (string= "" (trip-departure tr)))
          (ok (string= "" (trip-return-time tr)))
          (ng (trip-dep-passengers tr))
          (ng (vb::booked-trip-p tr)))
        ;; ... so the Personal slot is free for a new booking
        (ok (vb::find-empty-slot trips "Saturday September 5" :personal))))

  (testing "label-row-p"
    (ok (vb::label-row-p '("" "" "" "" "" "" "" "" "" "" "" "Saturday" "")))
    (ok (vb::label-row-p '("" "" "" "" "" "" "" "" "" "" "" "MONDAY" "")))
    (ok (vb::label-row-p '("Saturday September 5" "" "Chevy" "" "" "DGH" "" "" "LGH" "" "" "Personal" "")))
    (ok (vb::label-row-p '("" "" "Dep." "Ret." "" "Dep." "Ret." "" "Dep." "Ret." "" "Dep." "Ret.")))
    (ok (vb::label-row-p '("" "")))
    (ng (vb::label-row-p '("" "" "" "" "" "8:35 am" "10:30 am" "" "" "" "" "" "")))
    (ng (vb::label-row-p '("" "" "SHMS" "" "" "" "" "" "" "" "" "" "")))
    (ng (vb::label-row-p '("" "" "" "" "" "MM" "" "" "" "" "" "Saturday" "")))))

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

(deftest find-trips
  (testing "filter-by-day"
    (let ((trips (sample-trips)))
      (ok (= 2 (length (vb::find-trips trips :day "Monday"))))
      (ok (= 1 (length (vb::find-trips trips :day "Tuesday"))))
      (ok (= 0 (length (vb::find-trips trips :day "Thursday"))))))

  (testing "filter-by-destination"
    (let ((trips (sample-trips)))
      (ok (= 2 (length (vb::find-trips trips :destination "SHMS"))))
      (ok (= 1 (length (vb::find-trips trips :destination "Shrine"))))))

  (testing "filter-both"
    (let ((trips (sample-trips)))
      (ok (= 1 (length (vb::find-trips trips :day "Monday" :destination "SHMS"))))
      (ok (= 0 (length (vb::find-trips trips :day "Tuesday" :destination "Shrine"))))))

  (testing "find-trips-case-insensitive"
    (let ((trips (sample-trips)))
      (ok (= 2 (length (vb::find-trips trips :destination "shms"))))
      (ok (= 1 (length (vb::find-trips trips :day "monday" :destination "shrine"))))))

  (testing "substring-match"
    (let ((trips (sample-trips)))
      (ok (= 2 (length (vb::find-trips trips :day "April 20"))))
      ;; a bare "April 2" is a prefix of every April 2x day
      (ok (= 4 (length (vb::find-trips trips :day "April 2"))))
      (ok (= 1 (length (vb::find-trips trips :destination "Shr"))))))

  (testing "no-filters"
    (ok (= 4 (length (vb::find-trips (sample-trips))))))

  (testing "empty-dest-excluded"
    (ok (= 0 (length (vb::find-trips (sample-trips) :destination ""))))))

;;; --- Unit tests: find-empty-slot ---

;;; --- Unit tests: filter-new-passengers ---

(deftest filter-new-passengers
  (testing "removes-existing"
      (ok (equal '("CG") (vb::filter-new-passengers '("MM" "DM" "CG") '("MM" "DM")))))

  (testing "case-insensitive"
      (ok (equal '() (vb::filter-new-passengers '("mm") '("MM")))))

  (testing "all-new"
      (ok (equal '("A" "B") (vb::filter-new-passengers '("A" "B") '()))))

  (testing "all-existing"
      (ok (equal '() (vb::filter-new-passengers '("X" "Y") '("X" "Y")))))

  (testing "empty-input"
      (ok (equal '() (vb::filter-new-passengers '() '("X"))))))

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

(deftest find-empty-slot
  (testing "auto-priority"
    ;; DGH is taken, should pick LGH (next in priority)
    (let ((slot (vb::find-empty-slot (slot-trips) "Monday")))
      (ok slot)
      (ok (eq :lgh (trip-vehicle slot)))))

  (testing "preferred-vehicle"
    (let ((slot (vb::find-empty-slot (slot-trips) "Monday" :chevy)))
      (ok slot)
      (ok (eq :chevy (trip-vehicle slot)))))

  (testing "preferred-taken"
    ;; DGH is taken
    (ng (vb::find-empty-slot (slot-trips) "Monday" :dgh)))

  (testing "personal-not-auto"
    ;; Personal should not be auto-selected even though it's empty
    (let ((all-taken (list (make-trip :day "Monday" :vehicle :dgh
                                      :destination "X" :departure "8")
                           (make-trip :day "Monday" :vehicle :lgh
                                      :destination "Y" :departure "9")
                           (make-trip :day "Monday" :vehicle :chevy
                                      :destination "Z" :departure "10")
                           (make-trip :day "Monday" :vehicle :personal
                                      :destination "" :departure ""))))
      (ng (vb::find-empty-slot all-taken "Monday"))
      ;; ... unless asked for explicitly
      (ok (eq :personal (trip-vehicle (vb::find-empty-slot all-taken "Monday" :personal))))))

  (testing "wrong-day"
    (ng (vb::find-empty-slot (slot-trips) "Tuesday"))))

;;; --- Unit tests: print-trip ---

;;; --- Unit tests: add-passengers deduplication ---

(deftest add-passengers-dedup
    ;; The writer itself, called directly: ADD-PASSENGERS re-reads the sheet
    ;; and refuses conflicts before delegating here, and that is covered by
    ;; CONFLICT-TESTS.
  (testing "skips-dep-duplicates"
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
                 (vb::write-passengers trip '("MM" "DM" "CG"))
                 (ok (equal '("CG") dep-written))))
          (setf (symbol-function 'vb::write-range) original))))

  (testing "skips-ret-duplicates"
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
                 (vb::write-passengers trip '("MM" "CG"))
                 (ok (equal '("CG") ret-written))))
          (setf (symbol-function 'vb::write-range) original))))

  (testing "skips-all-when-fully-present"
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
                 (vb::write-passengers trip '("MM"))
                 (ok (= 0 write-count))))
          (setf (symbol-function 'vb::write-range) original))))

  (testing "case-insensitive-dedup"
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
                 (vb::write-passengers trip '("mm" "CG"))
                 (ok (equal '("CG") dep-written))))
          (setf (symbol-function 'vb::write-range) original))))

  (testing "returns-trip"
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
                      (result (vb::write-passengers trip '("MM"))))
                 (ok (eq trip result))))
          (setf (symbol-function 'vb::write-range) original)))))

(deftest print-trip
  (testing "basic-format"
      (let* ((tr (make-trip :day "Monday April 20" :vehicle :dgh
                            :destination "SHMS" :departure "8:35 am"
                            :return-time "10:30 am"
                            :dep-passengers '("MM" "DM")
                            :sheet-name "Apr 20-26"))
             (output (with-output-to-string (s) (vb::print-trip tr s))))
        (ok (search "Monday April 20" output))
        (ok (search "DGH" output))
        (ok (search "SHMS" output))
        (ok (search "8:35 am" output))
        (ok (search "MM, DM" output))
        (ok (search "Apr 20-26" output))))

  (testing "no-sheet-name"
      (let* ((tr (make-trip :day "Monday" :vehicle :lgh
                            :destination "X" :departure "9 am"
                            :return-time "11 am"
                            :dep-passengers '()
                            :sheet-name nil))
             (output (with-output-to-string (s) (vb::print-trip tr s))))
        (ng (search "[" output)))))

;;; --- Run all tests ---

(deftest month-abbrev-num
  (ok (= 1 (vb::month-abbrev-num "Jan")))
  (ok (= 3 (vb::month-abbrev-num "Mar")))
  (ok (= 4 (vb::month-abbrev-num "Apr")))
  (ok (= 5 (vb::month-abbrev-num "May")))
  (ok (= 12 (vb::month-abbrev-num "Dec")))
  (ok (= 1 (vb::month-abbrev-num "January")))
  (ok (signals (vb::month-abbrev-num "Xyz") 'error)))

(deftest date-key
  (ok (= 20260420 (vb::date-key (chronicity:parse "April 20 2026"))))
  (ok (= 20260101 (vb::date-key (chronicity:parse "January 1 2026"))))
  (ok (= 20251231 (vb::date-key (chronicity:parse "December 31 2025")))))

(deftest parse-week-range
  (testing "single-month"
    (let ((year (chronicity:year-of (chronicity:parse "today"))))
      (multiple-value-bind (start end) (vb::parse-week-range "Apr 20-26")
        (ok (= (+ (* year 10000) 420) start))
        (ok (= (+ (* year 10000) 426) end)))))
  (testing "cross-month"
    (let ((year (chronicity:year-of (chronicity:parse "today"))))
      (multiple-value-bind (start end) (vb::parse-week-range "Mar 30-Apr 5 (Holy wk)")
        (ok (= (+ (* year 10000) 330) start))
        (ok (= (+ (* year 10000) 405) end)))))
  (testing "trailing-whitespace"
    (let ((year (chronicity:year-of (chronicity:parse "today"))))
      (multiple-value-bind (start end) (vb::parse-week-range "Apr 13-19 ")
        (ok (= (+ (* year 10000) 413) start))
        (ok (= (+ (* year 10000) 419) end)))))
  (testing "parse-week-range-no-match"
    (multiple-value-bind (start end) (vb::parse-week-range "garbage")
      (ng start)
      (ng end))
    ;; Right shape but the month word is not a month (a helper tab, say)
    (multiple-value-bind (start end) (vb::parse-week-range "Week 1-2")
      (ng start)
      (ng end))))

(deftest find-week-sheet
  ;; Each child pins VB::*WEEKS* to *SAMPLE-WEEKS* so the lookup is
  ;; deterministic and never fetches the live tab list.
  (testing "mid-week-match"
    (let* ((vb::*weeks* *sample-weeks*)
           (year (chronicity:year-of (chronicity:parse "today")))
           (target (chronicity:parse (format nil "April 22 ~D" year))))
      (ok (string= "Apr 20-26" (vb::find-week-sheet target)))))
  (testing "boundary-match-start"
    (let* ((vb::*weeks* *sample-weeks*)
           (year (chronicity:year-of (chronicity:parse "today")))
           (target (chronicity:parse (format nil "April 20 ~D" year))))
      (ok (string= "Apr 20-26" (vb::find-week-sheet target)))))
  (testing "boundary-match-end"
    (let* ((vb::*weeks* *sample-weeks*)
           (year (chronicity:year-of (chronicity:parse "today")))
           (target (chronicity:parse (format nil "April 26 ~D" year))))
      (ok (string= "Apr 20-26" (vb::find-week-sheet target)))))
  (testing "cross-month-match"
    (let* ((vb::*weeks* *sample-weeks*)
           (year (chronicity:year-of (chronicity:parse "today")))
           (target (chronicity:parse (format nil "April 1 ~D" year))))
      (ok (string= "Mar 30-Apr 5 (Holy wk)" (vb::find-week-sheet target)))))
  (testing "find-week-sheet-no-match"
    (let* ((vb::*weeks* *sample-weeks*)
           (year (chronicity:year-of (chronicity:parse "today")))
           (target (chronicity:parse (format nil "December 25 ~D" year))))
      (ng (vb::find-week-sheet target)))))

(defun call-with-mock-read-sheet (rows thunk)
  "Run THUNK with vb::read-sheet stubbed to return ROWS whatever sheet is asked for."
  (let ((original (symbol-function 'vb::read-sheet)))
    (unwind-protect
         (progn
           (setf (symbol-function 'vb::read-sheet)
                 (lambda (sheet-name)
                   (declare (ignore sheet-name))
                   rows))
           (funcall thunk))
      (setf (symbol-function 'vb::read-sheet) original))))

(deftest remind-driver-tests
  ;; Each child pins VB::*WEEKS* to *SAMPLE-WEEKS* (which cover April 22) so
  ;; the sheet lookup is deterministic and never fetches the live tab list.
  (testing "golden-path-multi-passenger"
      (let* ((vb::*weeks* *sample-weeks*)
             (day-str (vb::format-booking-day (chronicity:parse "April 22")))
             (block (make-block (format nil "~A           ~A" day-str day-str)
                                '(:dgh "X location")
                                '(:dgh ("12:00 pm" "2:00 pm"))
                                '(:dgh (("BW" "CG" "GG") ("BW" "CG" "GG")))))
             (sheet (make-test-sheet block)))
        (call-with-mock-read-sheet sheet
                                   (lambda ()
                                     (let ((result (remind-driver "BW" "April 22")))
                                       (ok (search "BW is the driver" result))
                                       ;; the vehicle to be driven is named
                                       (ok (search "BW is the driver of the DGH for a trip to X location departing at 12:00 pm"
                                                   result))
                                       (ok (search "X location" result))
                                       (ok (search "12:00 pm" result))
                                       (ok (search "CG, GG are passengers" result)))))))

  (testing "single-other-passenger-grammar"
      (let* ((vb::*weeks* *sample-weeks*)
             (day-str (vb::format-booking-day (chronicity:parse "April 22")))
             (block (make-block (format nil "~A           ~A" day-str day-str)
                                '(:dgh "Pool")
                                '(:dgh ("8:00 am" "10:00 am"))
                                '(:dgh (("BW" "CG") ("BW" "CG")))))
             (sheet (make-test-sheet block)))
        (call-with-mock-read-sheet sheet
                                   (lambda ()
                                     (let ((result (remind-driver "BW" "April 22")))
                                       (ok (search "CG is a passenger" result))
                                       (ng (search "are passengers" result)))))))

  (testing "no-other-passengers-clause"
      (let* ((vb::*weeks* *sample-weeks*)
             (day-str (vb::format-booking-day (chronicity:parse "April 22")))
             (block (make-block (format nil "~A           ~A" day-str day-str)
                                '(:dgh "SHMS")
                                '(:dgh ("12:35 pm" "2:35 pm"))
                                '(:dgh (("BW") ("BW")))))
             (sheet (make-test-sheet block)))
        (call-with-mock-read-sheet sheet
                                   (lambda ()
                                     (let ((result (remind-driver "BW" "April 22")))
                                       (ok (search "with no other passengers" result)))))))

  (testing "multiple-trips-same-day"
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
        (call-with-mock-read-sheet sheet
                                   (lambda ()
                                     (let ((result (remind-driver "BW" "April 22")))
                                       (ok (search "SHMS" result))
                                       (ok (search "Shrine" result))
                                       ;; each trip with its own vehicle
                                       (ok (search "of the DGH for a trip to SHMS" result))
                                       (ok (search "of the LGH for a trip to Shrine" result))
                                       (ok (search "12:35 pm" result))
                                       (ok (search "5:35 pm" result))
                                       (ok (search "CG is a passenger" result)))))))

  (testing "driver-not-first-is-not-driver"
      (let* ((vb::*weeks* *sample-weeks*)
             (day-str (vb::format-booking-day (chronicity:parse "April 22")))
             (block (make-block (format nil "~A           ~A" day-str day-str)
                                '(:dgh "SHMS")
                                '(:dgh ("8:00 am" "10:00 am"))
                                '(:dgh (("MM" "BW") ("MM" "BW")))))
             (sheet (make-test-sheet block)))
        (call-with-mock-read-sheet sheet
                                   (lambda ()
                                     (let ((result (remind-driver "BW" "April 22")))
                                       (ok (search "BW is not driving" result))
                                       (ok (search day-str result)))))))

  (testing "driver-match-case-insensitive"
      (let* ((vb::*weeks* *sample-weeks*)
             (day-str (vb::format-booking-day (chronicity:parse "April 22")))
             (block (make-block (format nil "~A           ~A" day-str day-str)
                                '(:dgh "SHMS")
                                '(:dgh ("8:00 am" "10:00 am"))
                                '(:dgh (("BW") ("BW")))))
             (sheet (make-test-sheet block)))
        (call-with-mock-read-sheet sheet
                                   (lambda ()
                                     (let ((result (remind-driver "bw" "April 22")))
                                       (ok (search "bw is the driver" result)))))))

  (testing "no-sheet-covers-day"
      (let ((vb::*weeks* *sample-weeks*)
            (called nil))
        (with-mocked-functions ((vb::read-sheet (lambda (sheet-name)
                                                  (declare (ignore sheet-name))
                                                  (setf called t)
                                                  nil)))
          (let* ((year (chronicity:year-of (chronicity:parse "today")))
                 (result (remind-driver
                          "BW" (format nil "December 25 ~D" year))))
            (ok (search "No schedule sheet covers" result))
            (ng called))))))

(deftest view-schedule-tests
  (testing "split-range-phrase"
    (multiple-value-bind (s e) (vb::split-range-phrase "today")
      (ok (string= "today" s))
      (ok (string= "today" e)))
    (multiple-value-bind (s e) (vb::split-range-phrase "tomorrow through Friday")
      (ok (string= "tomorrow" s))
      (ok (string= "Friday" e)))
    (multiple-value-bind (s e) (vb::split-range-phrase "Apr 20 to Apr 26")
      (ok (string= "Apr 20" s))
      (ok (string= "Apr 26" e)))
    (multiple-value-bind (s e) (vb::split-range-phrase "Apr 20 - Apr 26")
      (ok (string= "Apr 20" s))
      (ok (string= "Apr 26" e)))
    ;; case-insensitive
    (multiple-value-bind (s e) (vb::split-range-phrase "Monday TO Friday")
      (ok (string= "Monday" s))
      (ok (string= "Friday" e)))
    ;; only the first separator splits (limit 2)
    (multiple-value-bind (s e) (vb::split-range-phrase "Apr 20 to Apr 26 to Apr 30")
      (ok (string= "Apr 20" s))
      (ok (string= "Apr 26 to Apr 30" e)))
    ;; "and" / "&" join two periods (the /help example "this week and next week")
    (multiple-value-bind (s e) (vb::split-range-phrase "this week and next week")
      (ok (string= "this week" s))
      (ok (string= "next week" e)))
    (multiple-value-bind (s e) (vb::split-range-phrase "Saturday & Sunday")
      (ok (string= "Saturday" s))
      (ok (string= "Sunday" e)))
    ;; a period on its own is not split
    (multiple-value-bind (s e) (vb::split-range-phrase "next week")
      (ok (string= "next week" s))
      (ok (string= "next week" e)))
    ;; the sheet's own compact shape: a bare hyphen splits, and the month
    ;; named once carries to the side that omits it ("/view sept 19-22")
    (multiple-value-bind (s e) (vb::split-range-phrase "sept 19-22")
      (ok (string= "sept 19" s))
      (ok (string= "sept 22" e)))
    (multiple-value-bind (s e) (vb::split-range-phrase "September 19-22")
      (ok (string= "September 19" s))
      (ok (string= "September 22" e)))
    (multiple-value-bind (s e) (vb::split-range-phrase "Mar 30-Apr 5")
      (ok (string= "Mar 30" s))
      (ok (string= "Apr 5" e)))
    (multiple-value-bind (s e) (vb::split-range-phrase "19-22 sept")
      (ok (string= "sept 19" s))
      (ok (string= "sept 22" e)))
    (multiple-value-bind (s e) (vb::split-range-phrase "22nd-25th September")
      (ok (string= "September 22nd" s))
      (ok (string= "September 25th" e)))
    ;; the month also carries across a spaced separator
    (multiple-value-bind (s e) (vb::split-range-phrase "Apr 20 - 26")
      (ok (string= "Apr 20" s))
      (ok (string= "Apr 26" e)))
    (multiple-value-bind (s e) (vb::split-range-phrase "Apr 20 to 26")
      (ok (string= "Apr 20" s))
      (ok (string= "Apr 26" e)))
    ;; an en dash typed by a phone keyboard reads as a hyphen
    (multiple-value-bind (s e)
        (vb::split-range-phrase (format nil "Sept 19~a22" (code-char 8211)))
      (ok (string= "Sept 19" s))
      (ok (string= "Sept 22" e)))
    ;; numbers with no month are times far more often than dates: left whole
    (multiple-value-bind (s e) (vb::split-range-phrase "8-10")
      (ok (string= "8-10" s))
      (ok (string= "8-10" e))))

  (testing "trip-date-key"
    (let ((year (chronicity:year-of (chronicity:parse "today"))))
      ;; Standard trip-day format: "DayOfWeek MonthName Day"
      (ok (= (+ (* year 10000) 420)
             (vb::trip-date-key (make-trip :day "Monday April 20"))))
      (ok (= (+ (* year 10000) 1225)
             (vb::trip-date-key (make-trip :day "Friday December 25")))))
    ;; Blank day → NIL
    (ng (vb::trip-date-key (make-trip :day "")))
    (ng (vb::trip-date-key (make-trip :day nil)))
    ;; Unparseable → NIL (graceful failure)
    (ng (vb::trip-date-key (make-trip :day "garbage words here"))))

  (testing "find-week-sheets-in-range"
      (let ((vb::*weeks* *sample-weeks*)
            (year (chronicity:year-of (chronicity:parse "today"))))
        ;; Fully inside one week
        (ok (equal '("Apr 20-26")
                   (vb::find-week-sheets-in-range (+ (* year 10000) 421)
                                                  (+ (* year 10000) 424))))
        ;; Exact boundary
        (ok (equal '("Apr 20-26")
                   (vb::find-week-sheets-in-range (+ (* year 10000) 420)
                                                  (+ (* year 10000) 426))))
        ;; Spans two weeks
        (let ((result (vb::find-week-sheets-in-range (+ (* year 10000) 419)
                                                     (+ (* year 10000) 422))))
          (ok (member "Apr 13-19 " result :test #'string=))
          (ok (member "Apr 20-26" result :test #'string=)))
        ;; Out of range → empty
        (ok (equal '()
                   (vb::find-week-sheets-in-range (+ (* year 10000) 1225)
                                                  (+ (* year 10000) 1231))))))

  (testing "format-short-date"
    (ok (string= "Mon Apr 20" (vb::format-short-date "Monday April 20")))
    (ok (string= "Thu Jan 1"  (vb::format-short-date "Thursday January 1")))
    ;; Fallback on unparseable
    (ok (string= "garbage" (vb::format-short-date "garbage"))))

  (testing "vehicle-priority"
    (ok (= 0 (vb::vehicle-priority :dgh)))
    (ok (= 1 (vb::vehicle-priority :lgh)))
    (ok (= 2 (vb::vehicle-priority :chevy)))
    (ok (= 3 (vb::vehicle-priority :personal)))
    ;; unknown vehicle sinks to bottom
    (ok (> (vb::vehicle-priority :bogus) 100)))

  (testing "departure-minutes"
    (ok (= (+ (* 8 60) 35)   (vb::departure-minutes "8:35 am")))
    (ok (= (+ (* 12 60) 35)  (vb::departure-minutes "12:35 pm")))
    (ok (= (* 17 60)         (vb::departure-minutes "5:00 pm")))
    (ok (= 0                 (vb::departure-minutes "")))
    (ok (= 0                 (vb::departure-minutes nil)))
    ;; garbage → 0 (sort-safe default)
    (ok (= 0                 (vb::departure-minutes "not a time")))
    ;; ordering property: am < pm regardless of lexical order
    (ok (< (vb::departure-minutes "8:35 am")
           (vb::departure-minutes "12:35 pm")))
    (ok (< (vb::departure-minutes "12:35 pm")
           (vb::departure-minutes "5:35 pm"))))

  (testing "truncate-cell"
    (ok (string= "hello"     (vb::truncate-cell "hello" 10)))
    (ok (string= "hello"     (vb::truncate-cell "hello" 5)))
    (ok (string= "hel..."    (vb::truncate-cell "hello world" 6)))
    (ok (string= ""          (vb::truncate-cell "" 5)))
    (ok (string= ""          (vb::truncate-cell nil 5))))

  (testing "booked-trip-p"
    (ok (vb::booked-trip-p (make-trip :destination "SHMS" :departure "")))
    (ok (vb::booked-trip-p (make-trip :destination "" :departure "8 am")))
    (ok (vb::booked-trip-p (make-trip :destination "SHMS" :departure "8 am")))
    (ng (vb::booked-trip-p (make-trip :destination "" :departure "")))
    (ng (vb::booked-trip-p (make-trip :destination nil :departure nil))))

  (testing "format-schedule-table-shape"
      (let* ((trips (list (make-trip :day "Monday April 20" :vehicle :dgh
                                     :destination "SHMS" :departure "8:35 am"
                                     :return-time "10:30 am"
                                     :dep-passengers '("MM" "DM")
                                     :ret-passengers '("MM" "DM" "BW"))))
             (out (vb::format-schedule-table trips)))
        ;; titles and rule are indented like the rows; the day is a flush-left
        ;; heading over them
        (ok (uiop:string-prefix-p "  Vehicle  " out))
        (ok (search (format nil "~%Mon Apr 20~%  DGH") out))
        (ok (search "Destination" out))
        (ok (search "Dep Pax" out))
        (ok (search "Ret Pax" out))
        (ok (search "Mon Apr 20" out))
        (ok (search "DGH" out))
        (ok (search "SHMS" out))
        (ok (search "8:35 am" out))
        (ok (search "MM, DM" out))
        (ok (search "MM, DM, BW" out))
        ;; separator row present
        (ok (search "---" out))))

  (testing "format-schedule-table-caps-columns"
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
      (ng (search long-dest out))
      (ng (search long-ret out))
      (ok (search (concatenate 'string
                               (make-string (- dest-cap 3) :initial-element #\X) "...")
                  out))
      (ok (search (concatenate 'string (subseq long-ret 0 (- ret-cap 3)) "...") out))
      ;; lines: titles, rule, "Mon Apr 20", row, "", "Tue Apr 21", row
      (let ((dep-col (search "Dep " header))
            (ret-col (search "Ret " header))
            (pax-col (search "Dep Pax" header))
            (row1 (nth 3 lines))
            (row2 (nth 6 lines)))
        (ok (= dep-col (search "8 am" row1)))
        (ok (= ret-col (search "after rec" row1)))
        (ok (= pax-col (search "MM" row1)))
        (ok (= dep-col (search "9 am" row2)))
        (ok (= ret-col (search "11 am" row2)))
        (ok (= pax-col (search "BW" row2)))))
    ;; a destination exactly at the cap renders in full
    (let* ((at-cap (make-string (cdr (assoc "Destination" vb::*column-width-caps*
                                            :test #'string=))
                                :initial-element #\Y))
           (out (vb::format-schedule-table
                 (list (make-trip :day "Monday April 20" :vehicle :dgh
                                  :destination at-cap :departure "8 am"
                                  :return-time "10 am" :dep-passengers '("MM"))))))
      (ok (search at-cap out))
      (ng (search "..." out)))
    ;; short destinations keep the narrower, content-sized column
    (let ((out (vb::format-schedule-table
                (list (make-trip :day "Monday April 20" :vehicle :dgh
                                 :destination "SHMS" :departure "8 am"
                                 :return-time "10 am" :dep-passengers '("MM"))))))
      (ok (search "Destination  Dep" out))))

  (testing "view-schedule-default-today"
      ;; With an empty sheet, view-schedule returns the "no trips" message.
      ;; Pins a week range covering every day of the year so the default
      ;; "today" always finds a sheet, whatever today's date is.
      (let ((vb::*weeks* '("Jan 1-Dec 31"))
            (sheet (make-test-sheet)))
        (call-with-mock-read-sheet sheet
                                   (lambda ()
                                     (let ((out (view-schedule)))
                                       (ok (search "No trips scheduled" out)))))))

  (testing "view-schedule-renders-trips"
      (let* ((day-str (vb::format-booking-day (chronicity:parse "April 22")))
             (block (make-block (format nil "~A           ~A" day-str day-str)
                                '(:dgh "SHMS")
                                '(:dgh ("8:35 am" "10:30 am"))
                                '(:dgh (("MM" "DM") ("MM" "DM")))))
             (sheet (make-test-sheet block)))
        (call-with-mock-read-sheet sheet
                                   (lambda ()
                                     (let ((out (let ((vb::*weeks* *sample-weeks*))
                                                  (view-schedule "April 22"))))
                                       (ok (search "SHMS" out))
                                       (ok (search "8:35 am" out))
                                       (ok (search "DGH" out))
                                       (ok (search "MM, DM" out))
                                       (ok (search "Vehicle" out)))))))

  (testing "view-schedule-no-sheet"
      ;; Date out of any configured week → informative message, no fetch.
      (let* ((year (chronicity:year-of (chronicity:parse "today")))
             (called nil)
             (original (symbol-function 'vb::read-sheet)))
        (unwind-protect
             (progn
               (setf (symbol-function 'vb::read-sheet)
                     (lambda (sheet-name)
                       (declare (ignore sheet-name))
                       (setf called t)
                       nil))
               (let ((out (let ((vb::*weeks* *sample-weeks*))
                            (view-schedule (format nil "December 25 ~D" year)))))
                 (ok (search "No schedule sheets cover" out))
                 (ng called)))
          (setf (symbol-function 'vb::read-sheet) original))))

  (testing "view-schedule-sort-order"
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
        (call-with-mock-read-sheet sheet
                                   (lambda ()
                                     (let* ((out (let ((vb::*weeks* *sample-weeks*))
                                                   (view-schedule "April 22")))
                                            (morning-pos (search "Morning" out))
                                            (afternoon-pos (search "Afternoon" out)))
                                       (ok morning-pos)
                                       (ok afternoon-pos)
                                       (ok (< morning-pos afternoon-pos))))))))

(deftest format-schedule-table-groups-by-day
  (let* ((trips (list (make-trip :day "Monday April 20" :vehicle :dgh :destination "SHMS"
                                 :departure "8 am" :return-time "10 am" :dep-passengers '("MM"))
                      (make-trip :day "Monday April 20" :vehicle :lgh :destination "Bank"
                                 :departure "1 pm" :return-time "2 pm" :dep-passengers '("DM"))
                      (make-trip :day "Tuesday April 21" :vehicle :chevy :destination "Pool"
                                 :departure "9 am" :return-time "11 am" :dep-passengers '("CG"))))
         (out (vb::format-schedule-table trips))
         (lines (cl-ppcre:split "\\n" out)))
    ;; titles and rule first, indented like the rows
    (ok (uiop:string-prefix-p "  Vehicle  Destination  " (first lines)))
    (ok (uiop:string-prefix-p "  -------  -----------  " (second lines)))
    ;; one flush-left heading per day over its indented rows, a blank line
    ;; between days, and the date nowhere else
    (ok (equal '("Mon Apr 20" "  DGH" "  LGH" "" "Tue Apr 21" "  CHEVY")
               (mapcar (lambda (line)
                         (if (uiop:string-prefix-p "  " line)
                             (subseq line 0 (position #\Space line :start 2))
                             line))
                       (nthcdr 2 lines))))
    (ok (= 1 (count "Apr 20" lines :test #'search)))
    ;; no trailing spaces on any line
    (dolist (line lines)
      (ok (string= line (string-right-trim " " line))))))

(deftest phrase-date-range-tests
  ;; Pinned to Wednesday April 22 2026 so the sample sheets "Apr 20-26" and
  ;; "Apr 27-May 3" are the ones in play, whatever today is.
  (let ((chronicity:*now* (chronicity:make-datetime 2026 4 22 12 0 0))
        (vb::*weeks* *sample-weeks*))
    ;; week phrases cover the sheet's Monday-to-Sunday week
    (multiple-value-bind (s e) (vb::phrase-date-range "this week")
      (ok (= 20260420 (vb::date-key s)))
      (ok (= 20260426 (vb::date-key e))))
    (multiple-value-bind (s e) (vb::phrase-date-range "next week")
      (ok (= 20260427 (vb::date-key s)))
      (ok (= 20260503 (vb::date-key e))))
    (multiple-value-bind (s e) (vb::phrase-date-range "last week")
      (ok (= 20260413 (vb::date-key s)))
      (ok (= 20260419 (vb::date-key e))))
    ;; filler around "week" is ignored: these all mean this Monday to Sunday
    (dolist (p '("the week" "the whole week" "whole week" "week" "all week"
                 "all of this week" "this whole week" "the entire week"
                 "all week long" "The Whole Week"))
      (multiple-value-bind (s e) (vb::phrase-date-range p)
        (ok (equal 20260420 (and s (vb::date-key s))) (format nil "start of ~s" p))
        (ok (equal 20260426 (and e (vb::date-key e))) (format nil "end of ~s" p))))
    (dolist (p '("the whole of next week" "all of next week" "the coming week"
                 "the following week"))
      (multiple-value-bind (s e) (vb::phrase-date-range p)
        (ok (equal 20260427 (and s (vb::date-key s))) (format nil "start of ~s" p))
        (ok (equal 20260503 (and e (vb::date-key e))) (format nil "end of ~s" p))))
    (multiple-value-bind (s e) (vb::phrase-date-range "the previous week")
      (ok (= 20260413 (vb::date-key s)))
      (ok (= 20260419 (vb::date-key e))))
    ;; "the rest of the week" starts today (a Wednesday) and still ends Sunday
    (multiple-value-bind (s e) (vb::phrase-date-range "the rest of the week")
      (ok (= 20260422 (vb::date-key s)))
      (ok (= 20260426 (vb::date-key e))))
    ;; a weekend is the Saturday and Sunday of the same Monday-to-Sunday week
    (multiple-value-bind (s e) (vb::phrase-date-range "the weekend")
      (ok (= 20260425 (vb::date-key s)))
      (ok (= 20260426 (vb::date-key e))))
    (multiple-value-bind (s e) (vb::phrase-date-range "next weekend")
      (ok (= 20260502 (vb::date-key s)))
      (ok (= 20260503 (vb::date-key e))))
    ;; anything else is left to chronicity
    (ng (vb::week-phrase-range "Wednesday"))
    (ng (vb::week-phrase-range "weekly"))
    (ng (vb::week-phrase-range "the week after next"))
    ;; a single day is a one-day window
    (multiple-value-bind (s e) (vb::phrase-date-range "today")
      (ok (= 20260422 (vb::date-key s)))
      (ok (= 20260422 (vb::date-key e))))
    (multiple-value-bind (s e) (vb::phrase-date-range "tomorrow")
      (ok (= 20260423 (vb::date-key s)))
      (ok (= 20260423 (vb::date-key e))))
    (multiple-value-bind (s e) (vb::phrase-date-range "Apr 20")
      (ok (= 20260420 (vb::date-key s)))
      (ok (= 20260420 (vb::date-key e))))
    (multiple-value-bind (s e) (vb::phrase-date-range "Saturday")
      (ok (= 20260425 (vb::date-key s)))
      (ok (= 20260425 (vb::date-key e))))
    ;; a day reads as it does in a booking (BOOKING-NLP:NORMALIZE-DAY-PHRASE):
    ;; "coming" and "upcoming" are filler, a weekday beside a month-day is
    ;; dropped rather than believed, a slash date means this year
    (dolist (p '("this coming Tuesday" "the coming Tuesday" "upcoming Tuesday"
                 "this upcoming Tues" "Tuesday April 28" "Tue, Apr 28th"))
      (multiple-value-bind (s e) (vb::phrase-date-range p)
        (ok (equal 20260428 (and s (vb::date-key s))) (format nil "start of ~s" p))
        (ok (equal 20260428 (and e (vb::date-key e))) (format nil "end of ~s" p)))
      (ok (vb:date-range-phrase-p p) (format nil "readable ~s" p)))
    (multiple-value-bind (s e) (vb::phrase-date-range "4/24")
      (ok (= 20260424 (vb::date-key s)))
      (ok (= 20260424 (vb::date-key e))))
    (multiple-value-bind (s e) (vb::phrase-date-range "the day after tomorrow")
      (ok (= 20260424 (vb::date-key s)))
      (ok (= 20260424 (vb::date-key e))))
    (ng (vb::phrase-date-range "gibberish nonsense"))
    (ng (vb:date-range-phrase-p "gibberish nonsense"))
    ;; the /help example: both weeks' sheets are read and the window is labelled
    (let ((sheets-read '()))
      (with-mocked-functions ((vb::read-sheet (lambda (sheet-name)
                                                (push sheet-name sheets-read)
                                                (make-test-sheet))))
        (let ((out (view-schedule "this week and next week")))
          (ok (search "No trips scheduled from Monday April 20 to Sunday May 3" out))
          (ok (equal '("Apr 20-26" "Apr 27-May 3") (reverse sheets-read))))))
    ;; the phrases that once failed in Slack ("/view the whole week",
    ;; "/view the week") read as this week end to end
    (call-with-mock-read-sheet (make-test-sheet)
                               (lambda ()
                                 (dolist (p '("the whole week" "the week" "all week"))
                                   (ok (search "from Monday April 20 to Sunday April 26"
                                               (view-schedule p)) (format nil "view-schedule ~s" p)))
                                 (ok (search "from Wednesday April 22 to Sunday April 26"
                                             (view-schedule "the rest of the week")))
                                 (ok (search "from Saturday April 25 to Sunday May 3"
                                             (view-schedule "the weekend and next week")))))
    ;; the compact shape the sheet's tabs use, which "/view sept 19-22" once
    ;; failed on: the month carries across the hyphen either way round
    (call-with-mock-read-sheet (make-test-sheet)
                               (lambda ()
                                 (dolist (p '("Apr 20-26" "Apr 20 - 26" "Apr 20-Apr 26"
                                              "20-26 Apr" "April 20-26"))
                                   (ok (search "from Monday April 20 to Sunday April 26"
                                               (view-schedule p)) (format nil "view-schedule ~s" p)))
                                 (ok (search "from Monday March 30 to Sunday April 5"
                                             (view-schedule "Mar 30-Apr 5")))))
    ;; a reversed range still works, and an unreadable one names the whole phrase
    (call-with-mock-read-sheet (make-test-sheet)
                               (lambda ()
                                 (ok (search "from Monday April 20 to Sunday April 26"
                                             (view-schedule "Apr 26 to Apr 20")))))
    (let ((c (handler-case (progn (view-schedule "this week and nonsense") nil)
               (error (c) c))))
      (ok c)
      (ok (search "this week and nonsense" (princ-to-string c))))))

(deftest parse-passenger-initials
  (ok (equal '("CG" "BW") (vb::parse-passenger-initials "CG and BW")))
  (ok (equal '("MM" "DM" "CG") (vb::parse-passenger-initials "MM, DM and CG")))
  (ok (equal '("MM") (vb::parse-passenger-initials "MM")))
  (ok (equal '() (vb::parse-passenger-initials "no initials here"))))

(deftest trips-and-destinations
  (testing "known-initials-from-both-legs"
    (ok (equal '("MM" "DM" "CG")
               (vb::known-initials
                (list (make-trip :dep-passengers '("MM" "DM") :ret-passengers '("MM" "cg"))
                      ;; a note in a passenger cell is not initials
                      (make-trip :dep-passengers '("DM" "back late"))))))
    (ok (equal '() (vb::known-initials '()))))

  (testing "known-destinations-are-deduplicated"
    (ok (equal '("SHMS" "Shrine")
               (vb::known-destinations
                (list (make-trip :destination "SHMS")
                      (make-trip :destination "")
                      (make-trip :destination "Shrine")
                      (make-trip :destination "shms")
                      (make-trip :destination nil)))))
    (ok (equal '() (vb::known-destinations '()))))

  (testing "fetch-all-trips-reads-every-week"
    (let ((reads '())
          (block (make-block "Monday April 20           Monday April 20"
                             '(:dgh "SHMS") '(:dgh ("8 am" "10 am")) '(:dgh (("MM") ("MM"))))))
      (with-mocked-functions ((vb::read-sheet (lambda (sheet-name)
                                                (push sheet-name reads)
                                                (make-test-sheet block))))
        (let ((vb::*weeks* '("Apr 13-19 " "Apr 20-26")))
          (let ((trips (vb::fetch-all-trips)))
            (ok (equal '("Apr 13-19 " "Apr 20-26") (reverse reads)))
            ;; 4 vehicles x 1 block x 2 weeks
            (ok (= 8 (length trips)))
            (ok (equal '("Apr 13-19 " "Apr 20-26")
                       (remove-duplicates (mapcar #'trip-sheet-name trips) :test #'string= :from-end t)))))
        (setf reads '())
        (let ((trips (vb::fetch-all-trips :week "Apr 6-12")))
          (ok (equal '("Apr 6-12") reads))
          (ok (= 4 (length trips))))))))

(defun sheet-with-shms ()
  "A one-block sheet for \"Thursday April 9\" with MM on the DGH SHMS trip."
  (make-test-sheet
   (make-block "Thursday April 9           Thursday April 9"
               '(:dgh "SHMS") '(:dgh ("8:35 am" "10:30 am")) '(:dgh (("MM") ("MM"))))))

(defun empty-sheet ()
  "A one-block sheet for \"Thursday April 9\" with every vehicle free."
  (make-test-sheet
   (make-block "Thursday April 9           Thursday April 9" '() '() '())))

(defun full-day-sheet ()
  "A one-block sheet for \"Thursday April 9\" with every auto-assignable
vehicle booked from 8 to 9."
  (make-test-sheet
   (make-block "Thursday April 9           Thursday April 9"
               '(:dgh "A" :lgh "B" :chevy "C")
               '(:dgh ("8 am" "9 am") :lgh ("8 am" "9 am") :chevy ("8 am" "9 am"))
               '(:dgh (("MM") ("MM")) :lgh (("DM") ("DM")) :chevy (("CG") ("CG"))))))

(defun call-recording-writes (thunk &key sheet)
  "Run THUNK with WRITE-RANGE and READ-RANGE mocked and *STANDARD-OUTPUT*
captured.  Returns (values OUTPUT WRITES READS); WRITES is a list of (range
values sheet-name) in call order and READS the sheet names READ-RANGE was
asked for.  SHEET is the row list every read answers with, and every test that
writes needs one: the sheet being written is re-read immediately beforehand
(VB::REFRESH-TRIP), so leaving it out would reach the network."
  (let ((writes '()) (reads '()))
    (let ((out (with-output-to-string (*standard-output*)
                 (with-mocked-functions ((vb::write-range
                                          (lambda (range values &key sheet-name)
                                            (push (list range values sheet-name) writes)))
                                         (vb::read-sheet
                                          (lambda (sheet-name)
                                            (push sheet-name reads)
                                            sheet)))
                   (funcall thunk)))))
      (values out (nreverse writes) (nreverse reads)))))

(deftest book-tests
  ;; Fixtures (see SHEET-WITH-SHMS / EMPTY-SHEET): one block for
  ;; "Thursday April 9" starting at sheet row 3.  DGH occupies columns F (dep)
  ;; and G (ret); passengers start at row 7.
  (testing "joins-existing-trip-without-times"
    (let ((trips (vb::parse-schedule (sheet-with-shms) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda () (vb:book "Thursday April 9" "SHMS" '("GG") :trips trips))
           :sheet (sheet-with-shms))
        (ok (search "Found 1 existing trip" out))
        ;; MM already occupies row 7, so GG goes to row 8 in both columns
        (ok (equal '(("F8:F8" (("GG")) "test-sheet")
                     ("G8:G8" (("GG")) "test-sheet"))
                   writes)))))

  (testing "trips-argument-skips-the-initial-read"
    ;; :TRIPS skips FETCH-ALL-TRIPS, which would read every week sheet.  The
    ;; one sheet being written is still read once, immediately before the
    ;; write, so a snapshot taken minutes ago cannot decide where the
    ;; passengers land (VB::REFRESH-TRIP).
    (let ((trips (vb::parse-schedule (sheet-with-shms) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes reads)
          (call-recording-writes
           (lambda () (vb:book "Thursday April 9" "SHMS" '("GG") :trips trips))
           :sheet (sheet-with-shms))
        (declare (ignore out))
        (ok (equal '("test-sheet") reads))
        (ok (equal '("F8:F8" "G8:G8") (mapcar #'first writes))))))

  (testing "week-argument-reads-one-sheet"
    (multiple-value-bind (out writes reads)
        (call-recording-writes
         (lambda () (vb:book "Thursday April 9" "SHMS" '("GG") :week "Apr 6-12"))
         :sheet (sheet-with-shms))
      (declare (ignore out))
      ;; the named week sheet, then that same sheet again before writing
      (ok (equal '("Apr 6-12" "Apr 6-12") reads))
      (ok (equal '("F8:F8" "G8:G8") (mapcar #'first writes)))
      (ok (equal '("Apr 6-12" "Apr 6-12") (mapcar #'third writes)))))

  (testing "creates-new-booking-with-times"
    (let ((trips (vb::parse-schedule (empty-sheet) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (vb:book "Thursday April 9" "Dentist" '("BW" "CG")
                      :dep-time "8:00 am" :ret-time "10:00 am" :trips trips))
           :sheet (empty-sheet))
        (ok (search "Created DGH booking" out))
        (ok (equal '(("F4" (("Dentist")) "test-sheet")
                     ("F6" (("8:00 am")) "test-sheet")
                     ("G6" (("10:00 am")) "test-sheet")
                     ("F7:F8" (("BW") ("CG")) "test-sheet")
                     ("G7:G8" (("BW") ("CG")) "test-sheet"))
                   writes)))))

  (testing "vehicle-override-uses-that-vehicles-columns"
    (let ((trips (vb::parse-schedule (empty-sheet) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (vb:book "Thursday April 9" "Dentist" '("BW")
                      :dep-time "8:00 am" :ret-time "10:00 am"
                      :vehicle :chevy :trips trips))
           :sheet (empty-sheet))
        (ok (search "Created CHEVY booking" out))
        (ok (equal '("C4" "C6" "D6" "C7:C7" "D7:D7") (mapcar #'first writes))))))

  (testing "missing-both-times-signals-booking-incomplete"
    (let* ((trips (vb::parse-schedule (empty-sheet) :sheet-name "test-sheet"))
           (c (handler-case
                  (progn (vb:book "Thursday April 9" "Dentist" '("BW") :trips trips) nil)
                (bn:booking-incomplete (c) c))))
      (ok c)
      (ok (equal '(:dep-time :ret-time) (bn:booking-incomplete-missing c)))
      (ok (string= "Dentist" (getf (bn:booking-incomplete-understood c) :destination)))
      (ok (string= "Thursday April 9" (getf (bn:booking-incomplete-understood c) :day)))
      (ok (equal '("BW") (getf (bn:booking-incomplete-understood c) :passengers)))
      (ok (search "No existing trip to Dentist on Thursday April 9"
                  (bn:booking-incomplete-preamble c)))
      (ok (search "Please re-send" (princ-to-string c)))))

  (testing "missing-one-time-names-only-that-one"
    (let* ((trips (vb::parse-schedule (empty-sheet) :sheet-name "test-sheet"))
           (c (handler-case
                  (progn (vb:book "Thursday April 9" "Dentist" '("BW")
                                  :dep-time "8:00 am" :vehicle :lgh :trips trips)
                         nil)
                (bn:booking-incomplete (c) c))))
      (ok (equal '(:ret-time) (bn:booking-incomplete-missing c)))
      (ok (string= "8:00 am" (getf (bn:booking-incomplete-understood c) :dep-time)))
      (ok (eq :lgh (getf (bn:booking-incomplete-understood c) :vehicle)))))

  (testing "no-empty-slot-reports-and-returns-nil"
    ;; Every van is booked for the day, and the request is for a window none
    ;; of them overlaps: the day is simply out of blocks, which is a
    ;; different answer from a van being on the road (see CONFLICT-TESTS).
    (let* ((sheet (full-day-sheet))
           (trips (vb::parse-schedule sheet :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (ng (vb:book "Thursday April 9" "Dentist" '("BW")
                          :dep-time "10:00 am" :ret-time "11:00 am" :trips trips))))
        (ok (search "No available vehicle slots" out))
        (ok (equal '() writes)))))

  (testing "overlapping-a-full-day-names-the-vans-that-are-out"
    (let* ((sheet (full-day-sheet))
           (trips (vb::parse-schedule sheet :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (ng (vb:book "Thursday April 9" "Dentist" '("BW")
                          :dep-time "8:00 am" :ret-time "10:00 am" :trips trips))))
        (ok (search "No vehicle is free" out))
        (ng (search "No available vehicle slots" out))
        (ok (equal '() writes))))))

(deftest day-without-a-sheet-reports-it
  ;; Only "Thursday April 9" is on the sheet, so a booking for another day
  ;; names the missing sheet instead of claiming every vehicle is taken (with
  ;; times) or asking for times (without them).
  (let ((trips (vb::parse-schedule (empty-sheet) :sheet-name "test-sheet")))
    (multiple-value-bind (out writes)
        (call-recording-writes
         (lambda ()
           (ng (vb:book "Tuesday October 20" "Dentist" '("BW")
                        :dep-time "8:00 am" :ret-time "10:00 am" :trips trips))
           (ng (vb:book "Tuesday October 20" "Dentist" '("BW") :trips trips))))
      (ok (search "No schedule sheet covers Tuesday October 20" out))
      (ng (search "No available vehicle slots" out))
      (ok (equal '() writes)))))

(defun block-spacer ()
  "One blank row between two blocks of a fixture sheet.  PARSE-SCHEDULE takes
a block's start row from where its header is, MAKE-BLOCK builds 14 rows and
the sheet's blocks are 15 apart, so one spacer after a block puts the next
one on the row the real sheet has it on.  Each spacer beyond that stands for
a row somebody inserted by hand."
  (list (make-list 13 :initial-element "")))

(defun two-block-day (&key (dests '(:dgh "SHMS"))
                        (times '(:dgh ("8:00 am" "11:00 am")))
                        (pax '(:dgh (("MM") ("MM")))))
  "Two blocks for \"Thursday April 9\": the first booked as DESTS/TIMES/PAX
say, the second blank throughout.  The second block's header leaves column A
empty, which is how the sheet carries a day across a second block
(EXTRACT-DAY-FROM-HEADER).  Block start rows come out 3 and 18."
  (make-test-sheet
   (make-block "Thursday April 9           Thursday April 9" dests times pax)
   (block-spacer)
   (make-block "" '() '() '())))

(defun shifted-two-block-day ()
  "TWO-BLOCK-DAY with two rows inserted under the first block, so that the
second block's header is on sheet row 20 instead of 18: the shape of the
\"Oct 5-11\" tab when a booking for Sunday's second block was written two
rows too high."
  (make-test-sheet
   (make-block "Thursday April 9           Thursday April 9"
               '(:dgh "SHMS") '(:dgh ("8:00 am" "11:00 am")) '(:dgh (("MM") ("MM"))))
   (block-spacer) (block-spacer) (block-spacer)
   (make-block "" '() '() '())))

(deftest conflict-tests
  ;; Nothing is written over a conflict: a van already out at that time, a
  ;; passenger already on an overlapping trip, or a slot booked by somebody
  ;; else since the schedule was read.  TWO-BLOCK-DAY supplies the day with a
  ;; second, blank block, which is the only way the sheet holds the same van
  ;; twice in one day.

  (testing "named-vehicle-out-at-that-time-is-refused"
    (let ((trips (vb::parse-schedule (two-block-day) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (ng (vb:book "Thursday April 9" "Dentist" '("BW")
                          :dep-time "9:00 am" :ret-time "10:00 am"
                          :vehicle :dgh :trips trips)))
           :sheet (two-block-day))
        (ok (search "No vehicle is free" out))
        (ok (search "SHMS" out))
        (ok (equal '() writes)))))

  (testing "auto-selection-passes-over-a-van-that-is-out"
    ;; The DGH's second-block slot is blank, but the DGH is on the road from
    ;; 8 to 11, so the LGH takes the trip.
    (let ((trips (vb::parse-schedule (two-block-day) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (vb:book "Thursday April 9" "Dentist" '("BW")
                      :dep-time "9:00 am" :ret-time "10:00 am" :trips trips))
           :sheet (two-block-day))
        (ok (search "Created LGH booking" out))
        (ok (equal '("I4" "I6" "J6" "I7:I7" "J7:J7") (mapcar #'first writes))))))

  (testing "the-same-van-again-once-it-is-back"
    ;; Windows are half-open: the DGH is due back at 11:00 and may go out
    ;; again at 11:00, into the day's second block (start row 18).
    (let ((trips (vb::parse-schedule (two-block-day) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (vb:book "Thursday April 9" "Dentist" '("BW")
                      :dep-time "11:00 am" :ret-time "1:00 pm"
                      :vehicle :dgh :trips trips))
           :sheet (two-block-day))
        (ok (search "Created DGH booking" out))
        (ok (equal '("F19" "F21" "G21" "F22:F22" "G22:G22") (mapcar #'first writes))))))

  (testing "a-block-moved-down-is-written-where-it-is"
    ;; The incident this guards against: two rows had been inserted under
    ;; Sunday's first block on the "Oct 5-11" tab, and a booking for the
    ;; second block, placed by counting blocks, went two rows too high --
    ;; the times into the destination row and the passengers over the
    ;; "Dep./Ret." labels.  The rows written follow the block's header, here
    ;; on row 20: destination 21, times 23, passengers from 24.
    (let ((trips (vb::parse-schedule (shifted-two-block-day) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (vb:book "Thursday April 9" "Detroit Airport" '("BW" "JC")
                      :dep-time "8:00 pm" :ret-time "9:00 pm"
                      :vehicle :dgh :trips trips))
           :sheet (shifted-two-block-day))
        (ok (search "Created DGH booking" out))
        (ok (equal '(("F21" (("Detroit Airport")) "test-sheet")
                     ("F23" (("8:00 pm")) "test-sheet")
                     ("G23" (("9:00 pm")) "test-sheet")
                     ("F24:F25" (("BW") ("JC")) "test-sheet")
                     ("G24:G25" (("BW") ("JC")) "test-sheet"))
                   writes)))))

  (testing "a-block-moved-since-the-snapshot-is-not-written"
    ;; The snapshot has the second block on row 18; two rows inserted since
    ;; have put it on row 20.  The read before the write no longer finds a
    ;; block on row 18, so nothing is written by the stale row number.
    (let ((stale (vb::parse-schedule (two-block-day) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (ng (vb:book "Thursday April 9" "Dentist" '("BW")
                          :dep-time "11:00 am" :ret-time "1:00 pm"
                          :vehicle :dgh :trips stale)))
           :sheet (shifted-two-block-day))
        (ok (search "is no longer on the test-sheet sheet" out))
        (ok (equal '() writes)))))

  (testing "passenger-already-out-is-refused"
    ;; MM is on the DGH SHMS trip 8-11, so a second trip for MM at 9-10 in
    ;; another van is refused -- and BW, who is free, is not booked either.
    (let ((trips (vb::parse-schedule (two-block-day) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (ng (vb:book "Thursday April 9" "Dentist" '("MM" "BW")
                          :dep-time "9:00 am" :ret-time "10:00 am"
                          :vehicle :lgh :trips trips)))
           :sheet (two-block-day))
        (ok (search "MM is already booked" out))
        (ok (search "nothing was booked" out))
        (ok (equal '() writes)))))

  (testing "passenger-free-once-their-trip-is-back"
    (let ((trips (vb::parse-schedule (two-block-day) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (vb:book "Thursday April 9" "Dentist" '("MM")
                      :dep-time "11:00 am" :ret-time "1:00 pm"
                      :vehicle :lgh :trips trips))
           :sheet (two-block-day))
        (ok (search "Created LGH booking" out))
        (ok (equal '("I4" "I6" "J6" "I7:I7" "J7:J7") (mapcar #'first writes))))))

  (testing "a-slot-taken-since-the-snapshot-is-not-overwritten"
    ;; The snapshot says every van is free; the sheet now has the DGH booked.
    ;; The read before the write catches it and nothing is written.
    (let ((stale (vb::parse-schedule (empty-sheet) :sheet-name "test-sheet")))
      (multiple-value-bind (out writes reads)
          (call-recording-writes
           (lambda ()
             (ng (vb:book "Thursday April 9" "Dentist" '("BW")
                          :dep-time "9:00 am" :ret-time "10:00 am"
                          :vehicle :dgh :trips stale)))
           :sheet (sheet-with-shms))
        (ok (search "while this request was being handled" out))
        (ok (equal '("test-sheet") reads))
        (ok (equal '() writes)))))

  (testing "a-passenger-added-since-the-snapshot-shifts-the-write"
    ;; The snapshot has MM alone on the SHMS trip, so a write decided from it
    ;; would put GG in row 8, over DM.  The re-read puts GG in row 9.
    (let ((stale (vb::parse-schedule (sheet-with-shms) :sheet-name "test-sheet"))
          (now (make-test-sheet
                (make-block "Thursday April 9           Thursday April 9"
                            '(:dgh "SHMS") '(:dgh ("8:35 am" "10:30 am"))
                            '(:dgh (("MM" "DM") ("MM" "DM")))))))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda () (vb:book "Thursday April 9" "SHMS" '("GG") :trips stale))
           :sheet now)
        (ok (search "Added GG" out))
        (ok (equal '("F9:F9" "G9:G9") (mapcar #'first writes))))))

  (testing "joining-refuses-someone-already-out-elsewhere"
    ;; GG is on the LGH pool run 9-10, which overlaps the SHMS trip
    ;; 8:35-10:30, so GG is not added to it.
    (let* ((sheet (make-test-sheet
                   (make-block "Thursday April 9           Thursday April 9"
                               '(:dgh "SHMS" :lgh "Pool")
                               '(:dgh ("8:35 am" "10:30 am") :lgh ("9:00 am" "10:00 am"))
                               '(:dgh (("MM") ("MM")) :lgh (("GG") ("GG"))))))
           (trips (vb::parse-schedule sheet :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda () (ng (vb:book "Thursday April 9" "SHMS" '("GG") :trips trips)))
           :sheet sheet)
        (ok (search "GG is already booked" out))
        (ok (equal '() writes)))))

  (testing "an-unreadable-time-blocks-rather-than-guesses"
    ;; A booked trip whose own times cannot be read cannot be shown to be out
    ;; of the way, so the van it holds is not handed out again that day.
    (let* ((sheet (two-block-day :times '(:dgh ("" ""))))
           (trips (vb::parse-schedule sheet :sheet-name "test-sheet")))
      (multiple-value-bind (out writes)
          (call-recording-writes
           (lambda ()
             (ng (vb:book "Thursday April 9" "Dentist" '("BW")
                          :dep-time "9:00 am" :ret-time "10:00 am"
                          :vehicle :dgh :trips trips)))
           :sheet sheet)
        (ok (search "No vehicle is free" out))
        (ok (equal '() writes))))))

(defun fixture-header ()
  "Sheet header text for April 9 of the current year, whatever weekday that is."
  (let* ((year (nth-value 5 (decode-universal-time (get-universal-time))))
         (day-str (bn:format-booking-day (chronicity:parse (format nil "April 9 ~d" year)))))
    (format nil "~a           ~a" day-str day-str)))

(defun run-book-natural (text sheet &key registered-initials)
  "Run BOOK-NATURAL on TEXT (and REGISTERED-INITIALS) with the sheet reads
returning SHEET, writes recorded and only one week configured.
Returns (values WRITES OUTPUT READS CONDITION): WRITES is a list of
(range values sheet-name), READS the number of sheet reads, CONDITION the
BOOKING-INCOMPLETE signalled, if any."
  (let ((writes '()) (reads 0) (condition nil) (out ""))
    (let ((vb::*weeks* '("Apr 6-12")))
      (with-mocked-functions ((vb::read-sheet (lambda (sheet-name)
                                                (declare (ignore sheet-name))
                                                (incf reads)
                                                sheet))
                              (vb::write-range (lambda (range values &key sheet-name)
                                                 (push (list range values sheet-name) writes))))
        (setf out (with-output-to-string (*standard-output*)
                    (handler-case (vb:book-natural
                                   text :registered-initials registered-initials)
                      (bn:booking-incomplete (c) (setf condition c)))))))
    (values (nreverse writes) out reads condition)))

(deftest book-natural-tests
  ;; End-to-end through the parser and BOOK with the sheet mocked (see
  ;; RUN-BOOK-NATURAL).  The fixture day is "April 9" of the current year so
  ;; the parsed day string matches the sheet header whatever the weekday.
  (testing "creates-a-new-booking"
    (multiple-value-bind (writes out reads condition)
        (run-book-natural "Can BW and CG take the van to the dentist on April 9 from 8 am to 10 am?"
                          (make-test-sheet (make-block (fixture-header) '() '() '())))
      (ng condition)
      ;; the week sheet, then that same sheet again to verify before writing
      (ok (= 2 reads))
      (ok (search "Created DGH booking" out))
      (ok (equal '(("F4" (("dentist")) "Apr 6-12")
                   ("F6" (("8:00 am")) "Apr 6-12")
                   ("G6" (("10:00 am")) "Apr 6-12")
                   ("F7:F8" (("BW") ("CG")) "Apr 6-12")
                   ("G7:G8" (("BW") ("CG")) "Apr 6-12"))
                 writes))))

  (testing "honours-vehicle-preference"
    (multiple-value-bind (writes out)
        (run-book-natural "take the LGH to the dentist on April 9 for BW, 8 am to 10 am"
                          (make-test-sheet (make-block (fixture-header) '() '() '())))
      (ok (search "Created LGH booking" out))
      (ok (equal '("I4" "I6" "J6" "I7:I7" "J7:J7") (mapcar #'first writes)))))

  (testing "joins-an-existing-trip-without-times"
    (multiple-value-bind (writes out)
        (run-book-natural "Please add GG to the SHMS trip on April 9"
                          (make-test-sheet
                           (make-block (fixture-header) '(:dgh "SHMS")
                                       '(:dgh ("8:35 am" "10:30 am"))
                                       '(:dgh (("MM") ("MM"))))))
      (ok (search "Found 1 existing trip" out))
      (ok (equal '(("F8:F8" (("GG")) "Apr 6-12")
                   ("G8:G8" (("GG")) "Apr 6-12"))
                 writes))))

  (testing "uses-the-sheets-destination-spelling"
    ;; "saint marys church" in the message resolves to the sheet's
    ;; "Saint Mary's Church" and joins that trip instead of creating one.
    (multiple-value-bind (writes out)
        (run-book-natural "MM to saint marys church on April 9"
                          (make-test-sheet
                           (make-block (fixture-header) '(:lgh "Saint Mary's Church")
                                       '(:lgh ("5:00 pm" "8:00 pm"))
                                       '(:lgh (("MK") ("MK"))))))
      (ok (search "Found 1 existing trip" out))
      (ok (equal '("I8:I8" "J8:J8") (mapcar #'first writes)))))

  (testing "skips-passengers-already-on-the-trip"
    (multiple-value-bind (writes out)
        (run-book-natural "add MM and GG to the SHMS trip on April 9"
                          (make-test-sheet
                           (make-block (fixture-header) '(:dgh "SHMS")
                                       '(:dgh ("8:35 am" "10:30 am"))
                                       '(:dgh (("MM") ("MM"))))))
      (ok (search "Skipped (already booked): MM" out))
      (ok (equal '(("F8:F8" (("GG")) "Apr 6-12")
                   ("G8:G8" (("GG")) "Apr 6-12"))
                 writes))))

  (testing "missing-day-is-reported-before-any-sheet-read"
    (multiple-value-bind (writes out reads condition)
        (run-book-natural "for the pool for MM 8 am to 10 am" (make-test-sheet))
      (declare (ignore out))
      (ok condition)
      (ok (= 0 reads))
      (ok (equal '() writes))
      (ok (equal '(:day) (bn:booking-incomplete-missing condition)))
      (ok (string= "pool" (getf (bn:booking-incomplete-understood condition) :destination)))
      (ok (string= "for the pool for MM 8 am to 10 am" (bn:booking-incomplete-text condition)))))

  (testing "missing-destination-and-passengers"
    (multiple-value-bind (writes out reads condition)
        (run-book-natural "tomorrow at 8 am" (make-test-sheet))
      (declare (ignore writes out))
      (ok (= 0 reads))
      (ok (equal '(:destination :passengers) (bn:booking-incomplete-missing condition)))))

  (testing "lowercase-initials-on-the-sheet"
    ;; BW and GG are on the sheet already (the SHMS trip), so "bw and gg" in
    ;; lowercase names them; the times do not overlap, so the LGH takes them.
    (multiple-value-bind (writes out reads condition)
        (run-book-natural "bw and gg to the pool on April 9 from 2 pm to 4 pm"
                          (make-test-sheet
                           (make-block (fixture-header) '(:dgh "SHMS")
                                       '(:dgh ("8:35 am" "10:30 am"))
                                       '(:dgh (("BW" "GG") ("BW" "GG"))))))
      (ng condition)
      (ok (search "Created LGH booking" out))
      (ok (= 2 reads))
      (ok (member '("I7:I8" (("BW") ("GG")) "Apr 6-12") writes :test #'equal))))

  (testing "lowercase-initials-registered-with-the-bot"
    ;; nobody but MM is on the sheet; BW is known from /iam
    (multiple-value-bind (writes out reads condition)
        (run-book-natural "add bw to the SHMS trip on April 9"
                          (make-test-sheet
                           (make-block (fixture-header) '(:dgh "SHMS")
                                       '(:dgh ("8:35 am" "10:30 am"))
                                       '(:dgh (("MM") ("MM")))))
                          :registered-initials '("BW"))
      (declare (ignore reads))
      (ng condition)
      (ok (search "Found 1 existing trip" out))
      (ok (equal '(("F8:F8" (("BW")) "Apr 6-12")
                   ("G8:G8" (("BW")) "Apr 6-12"))
                 writes))))

  (testing "lowercase-initials-in-the-report"
    ;; the day is missing, but "bw" is on the sheet: the sheet is read once
    ;; so the report does not ask for passengers it was given
    (multiple-value-bind (writes out reads condition)
        (run-book-natural "bw to the pool at 2 pm to 4 pm"
                          (make-test-sheet
                           (make-block (fixture-header) '(:dgh "SHMS")
                                       '(:dgh ("8:35 am" "10:30 am"))
                                       '(:dgh (("BW") ("BW"))))))
      (declare (ignore out))
      (ok (= 1 reads))
      (ok (equal '() writes))
      (ok (equal '(:day) (bn:booking-incomplete-missing condition)))
      (ok (equal '("BW") (getf (bn:booking-incomplete-understood condition) :passengers)))))

  (testing "unknown-lowercase-initials-are-missing-passengers"
    (multiple-value-bind (writes out reads condition)
        (run-book-natural "zz to the pool on April 9 at 2 pm to 4 pm"
                          (make-test-sheet
                           (make-block (fixture-header) '(:dgh "SHMS")
                                       '(:dgh ("8:35 am" "10:30 am"))
                                       '(:dgh (("BW") ("BW"))))))
      (declare (ignore out))
      (ok (= 1 reads))
      (ok (equal '() writes))
      (ok (equal '(:passengers) (bn:booking-incomplete-missing condition)))
      (ok (search "capitals for anyone new" (princ-to-string condition)))))

  (testing "no-trip-and-no-times-asks-for-times"
    (multiple-value-bind (writes out reads condition)
        (run-book-natural "add BW to the pool trip on April 9"
                          (make-test-sheet (make-block (fixture-header) '() '() '())))
      (declare (ignore out))
      (ok condition)
      (ok (= 1 reads))
      (ok (equal '() writes))
      (ok (equal '(:dep-time :ret-time) (bn:booking-incomplete-missing condition)))
      (ok (search "No existing trip to pool" (bn:booking-incomplete-preamble condition)))
      (ok (string= "pool" (getf (bn:booking-incomplete-understood condition) :destination)))))

  (testing "only-return-time-missing"
    (multiple-value-bind (writes out reads condition)
        (run-book-natural "BW to the pool on April 9 leaving at 8 am"
                          (make-test-sheet (make-block (fixture-header) '() '() '())))
      (declare (ignore writes out reads))
      (ok (equal '(:ret-time) (bn:booking-incomplete-missing condition)))
      (ok (string= "8:00 am" (getf (bn:booking-incomplete-understood condition) :dep-time)))))

  (testing "book-natural-calls-book"
    ;; The parsed pieces reach BOOK in the sheet's formats, with the
    ;; pre-fetched trips passed through.
    (let ((book-args nil))
      (with-mocked-functions ((vb::fetch-all-trips (lambda (&key week) (declare (ignore week))
                                                     (list (make-trip :destination "Pool"
                                                                      :dep-passengers '("CG")))))
                              (vb::book (lambda (day dest pax &key dep-time ret-time vehicle week trips)
                                          (setf book-args (list :day day :dest dest :pax pax
                                                                :dep dep-time :ret ret-time
                                                                :vehicle vehicle :week week
                                                                :trips trips)))))
        (vb:book-natural
         "I want to book the DGH for the pool next week Tuesday for cg and BW at seven PM to nine PM"
         :week "Apr 6-12")
        (ok (string= "Pool" (getf book-args :dest)))
        (ok (equal '("CG" "BW") (getf book-args :pax)))
        (ok (cl-ppcre:scan "^\\w+ \\w+ \\d+$" (getf book-args :day)))
        (ok (string= "7:00 pm" (getf book-args :dep)))
        (ok (string= "9:00 pm" (getf book-args :ret)))
        (ok (eq :dgh (getf book-args :vehicle)))
        (ok (string= "Apr 6-12" (getf book-args :week)))
        (ok (= 1 (length (getf book-args :trips))))))))

(defvar *urls* '()
  "The URLs the REPLY mock was asked for, most recent first.")

(defun reply (json)
  "A mock for DEXADOR:GET that answers JSON, a Lisp value, as the web app
would, and records the URL it was asked for in *URLS*."
  (lambda (uri &rest args)
    (declare (ignore args))
    (push uri *urls*)
    (com.inuoe.jzon:stringify json)))

(defun query-parameter (url name)
  (cdr (assoc name (quri:uri-query-params (quri:uri url)) :test #'string=)))

(defun call-with-schedule-secret (secret thunk)
  "Call THUNK with the environment variable VAN_SCHEDULE_SECRET set to SECRET,
or, when SECRET is NIL, empty, which APPS-SCRIPT-SECRET takes as unset."
  (let ((saved (uiop:getenv "VAN_SCHEDULE_SECRET")))
    (unwind-protect
         (progn
           (setf (uiop:getenv "VAN_SCHEDULE_SECRET") (or secret ""))
           (funcall thunk))
      (setf (uiop:getenv "VAN_SCHEDULE_SECRET") (or saved "")))))

(deftest sheet-io-tests
  (testing "write-range-payload"
    (let ((requests '()) (*urls* '()))
      (with-mocked-functions ((vb::apps-script-secret (constantly "test-secret"))
                              (dexador:request
                               (lambda (uri &rest args)
                                 (push (list* uri args) requests)
                                 (let ((headers (make-hash-table :test #'equal)))
                                   (setf (gethash "location" headers) "https://example.invalid/result")
                                   (values "" 302 headers))))
                              (dexador:get (reply (alexandria:plist-hash-table
                                                   '("updatedCells" 1) :test #'equal))))
        (ok (eql 1 (gethash "updatedCells"
                            (vb:write-range "F4" '(("SHMS")) :sheet-name "Apr 20-26"))))
        (ok (= 1 (length requests)))
        (ok (equal '("https://example.invalid/result") *urls*))
        (destructuring-bind (uri &key method content headers max-redirects) (first requests)
          (ok (search "script.google.com" uri))
          (ok (search (format nil "spreadsheetId=~a" vb::*spreadsheet-id*) uri))
          (ok (eq :post method))
          (ok (= 0 max-redirects))
          (ok (string= "application/json" (cdr (assoc "Content-Type" headers :test #'string=))))
          (let* ((payload (com.inuoe.jzon:parse content))
                 (arguments (gethash "arguments" payload))
                 (body (gethash "body" payload)))
            (ok (string= "test-secret" (gethash "secret" payload)))
            (ok (string= "Apr 20-26!F4" (gethash "range" arguments)))
            (ok (string= "USER_ENTERED" (gethash "valueInputOption" arguments)))
            (ok (equalp #(#("SHMS")) (gethash "values" body))))))
      ;; without a sheet name the range is unqualified
      (with-mocked-functions ((vb::apps-script-secret (constantly "test-secret"))
                              (dexador:request
                               (lambda (uri &rest args)
                                 (declare (ignore uri))
                                 (push args requests)
                                 (let ((headers (make-hash-table :test #'equal)))
                                   (setf (gethash "location" headers) "https://example.invalid/r")
                                   (values "" 302 headers))))
                              (dexador:get (reply (make-hash-table))))
        (vb:write-range "F7:F8" '(("BW") ("CG")))
        (let ((payload (com.inuoe.jzon:parse (getf (first requests) :content))))
          (ok (string= "F7:F8" (gethash "range" (gethash "arguments" payload))))
          (ok (equalp #(#("BW") #("CG")) (gethash "values" (gethash "body" payload))))))))

  (testing "write-range-without-redirect-errors"
    (with-mocked-functions ((vb::apps-script-secret (constantly "test-secret"))
                            (dexador:request
                             (lambda (uri &rest args)
                               (declare (ignore uri args))
                               (values "" 200 (make-hash-table :test #'equal)))))
      (ok (signals (vb:write-range "F4" '(("SHMS")) :sheet-name "Apr 20-26") 'error))))

  (testing "write-range-needs-the-secret"
    ;; The web app refuses a write without it, so none is sent.
    (let ((requests '()))
      (with-mocked-functions ((dexador:request
                               (lambda (uri &rest args)
                                 (push (list* uri args) requests)
                                 (values "" 200 (make-hash-table :test #'equal)))))
        (call-with-schedule-secret
         nil
         (lambda ()
           (ok (signals (vb::apps-script-secret) 'error))
           (ok (signals (vb:write-range "F4" '(("SHMS")) :sheet-name "Apr 20-26") 'error))
           (ok (null requests))))
        (call-with-schedule-secret
         "from-the-environment"
         (lambda ()
           (ok (string= "from-the-environment" (vb::apps-script-secret))))))))

  (testing "write-range-error-reply-errors"
    ;; A write the script could not make, e.g. to a protected range, is an
    ;; error rather than a booking that silently did not happen.
    (let ((*urls* '()))
      (with-mocked-functions ((vb::apps-script-secret (constantly "test-secret"))
                              (dexador:request
                               (lambda (uri &rest args)
                                 (declare (ignore uri args))
                                 (let ((headers (make-hash-table :test #'equal)))
                                   (setf (gethash "location" headers) "https://example.invalid/r")
                                   (values "" 302 headers))))
                              (dexador:get (reply (alexandria:plist-hash-table
                                                   '("error" "Exception: protected") :test #'equal))))
        (ok (signals (vb:write-range "F4" '(("SHMS")) :sheet-name "Apr 20-26") 'error)))))

  (testing "read-sheet-asks-the-web-app"
    (let ((*urls* '()))
      (with-mocked-functions ((dexador:get (reply #(#("a" "b") #("c" "d")))))
        (ok (equal '(("a" "b") ("c" "d")) (vb:read-sheet "Apr 20-26")))
        (ok (= 1 (length *urls*)))
        (ok (search vb::*apps-script-url* (first *urls*)))
        (ok (string= "values" (query-parameter (first *urls*) "action")))
        (ok (string= "Apr 20-26" (query-parameter (first *urls*) "sheet")))
        (ok (string= vb::*spreadsheet-id* (query-parameter (first *urls*) "spreadsheetId"))))))

  (testing "read-sheet-keeps-sheet-rows"
    ;; Row N of what READ-SHEET returns must be row N of the sheet, because
    ;; that is how PARSE-SCHEDULE knows where a block is.
    (let ((*urls* '())
          (rows #(#("title" "" "") #("" "" "")
                  #("Monday April 20" "" "Chevy") #("" "two lines" "") #("" "" "Dep.")
                  #("" "" "") #("" "" "") #("" "" "") #("" "" "")
                  #("" "" "Chevy") #("" "" "") #("" "" "Dep.") #("" "" "") #("" "" ""))))
      (with-mocked-functions ((dexador:get (reply rows)))
        (let ((rows (vb:read-sheet "Apr 20-26")))
          (ok (= 14 (length rows)))
          (ok (equal '("Monday April 20" "" "Chevy") (third rows)))
          (ok (equal '("" "" "Chevy") (tenth rows)))
          (ok (equal '(3 10)
                     (remove-duplicates
                      (mapcar #'trip-block-start-row (vb::parse-schedule rows)))))))))

  (testing "read-sheet-error-reply-errors"
    (let ((*urls* '()))
      (with-mocked-functions ((dexador:get (reply (alexandria:plist-hash-table
                                                   '("error" "Error: The spreadsheet has no tab named Nov 2-8")
                                                   :test #'equal))))
        (ok (signals (vb:read-sheet "Nov 2-8") 'error))))))

(deftest sheet-discovery-tests
  ;; The week tabs are discovered from the spreadsheet rather than
  ;; hardcoded, so /view keeps working as weeks are added.
  (testing "sheet-tabs-asks-the-web-app"
    (let ((*urls* '()))
      (with-mocked-functions ((dexador:get (reply #("Aug 31-Sep 6" "Sep 7-13" "St's / Dates" "Apr 13-19 "))))
        ;; every tab, in tab order, trailing space kept
        (ok (equal '("Aug 31-Sep 6" "Sep 7-13" "St's / Dates" "Apr 13-19 ")
                   (vb:sheet-tabs)))
        (ok (= 1 (length *urls*)))
        (ok (string= "tabs" (query-parameter (first *urls*) "action")))
        (ok (string= vb::*spreadsheet-id* (query-parameter (first *urls*) "spreadsheetId"))))))

  (testing "week-sheets-filters-caches-and-refreshes"
    (let ((fetches 0)
          (vb::*weeks* nil)
          (vb::*week-sheets-cache* nil)
          (vb::*week-sheets-cache-seconds* 300))
      (with-mocked-functions ((vb:sheet-tabs
                               (lambda ()
                                 (incf fetches)
                                 (if (= fetches 1)
                                     '("Aug 31-Sep 6" "Fall 2026" "Sep 7-13"
                                       "Dates" "Blank Temp" "Week 1-2")
                                     '("Sep 14-20")))))
        ;; only week-shaped tab names survive, in tab order
        (ok (equal '("Aug 31-Sep 6" "Sep 7-13") (vb:week-sheets)))
        (ok (= 1 fetches))
        ;; a second call within the TTL reuses the cache
        (ok (equal '("Aug 31-Sep 6" "Sep 7-13") (vb:week-sheets)))
        (ok (= 1 fetches))
        ;; :refresh forces a new fetch
        (ok (equal '("Sep 14-20") (vb:week-sheets :refresh t)))
        (ok (= 2 fetches))
        ;; an expired cache is fetched again
        (let ((vb::*week-sheets-cache-seconds* 0))
          (ok (equal '("Sep 14-20") (vb:week-sheets)))
          (ok (= 3 fetches)))
        ;; a fixed *weeks* bypasses discovery entirely
        (let ((vb::*weeks* '("Apr 20-26")))
          (ok (equal '("Apr 20-26") (vb:week-sheets)))
          (ok (= 3 fetches))))))

  (testing "lookups-use-discovered-weeks"
    ;; The regression behind "No schedule sheets cover Friday September 4":
    ;; a week present in the spreadsheet but absent from any hardcoded list.
    (let ((vb::*weeks* nil)
          (vb::*week-sheets-cache* nil)
          (year (chronicity:year-of (chronicity:parse "today"))))
      (with-mocked-functions ((vb:sheet-tabs
                               (lambda () '("Aug 31-Sep 6" "Dates" "Sep 7-13"))))
        (ok (string= "Aug 31-Sep 6"
                     (vb::find-week-sheet (chronicity:parse (format nil "September 4 ~D" year)))))
        (ok (equal '("Aug 31-Sep 6" "Sep 7-13")
                   (vb::find-week-sheets-in-range (+ (* year 10000) 904) (+ (* year 10000) 908))))
        (ng (vb::find-week-sheet (chronicity:parse (format nil "December 25 ~D" year))))
        ;; fetch-all-trips reads exactly the discovered week sheets
        (let ((reads '()))
          (with-mocked-functions ((vb:read-sheet (lambda (sheet-name)
                                                   (push sheet-name reads)
                                                   (make-test-sheet))))
            (vb:fetch-all-trips)
            (ok (equal '("Aug 31-Sep 6" "Sep 7-13") (reverse reads)))))))))

(deftest driver-helpers
  (testing "departure-universal-time"
    (let ((ut (vb::trip-departure-universal-time (make-trip :departure "8:35 am"))))
      (ok ut)
      (multiple-value-bind (s m h day month year) (decode-universal-time ut)
        (multiple-value-bind (ts tm th tday tmonth tyear) (decode-universal-time (get-universal-time))
          (declare (ignore ts tm th))
          (ok (= 0 s))
          (ok (= 35 m))
          (ok (= 8 h))
          (ok (= tday day))
          (ok (= tmonth month))
          (ok (= tyear year)))))
    (multiple-value-bind (s m h) (decode-universal-time
                                  (vb::trip-departure-universal-time (make-trip :departure "12:35 pm")))
      (declare (ignore s))
      (ok (= 35 m))
      (ok (= 12 h)))
    (ng (vb::trip-departure-universal-time (make-trip :departure "")))
    (ng (vb::trip-departure-universal-time (make-trip :departure nil)))
    (ng (vb::trip-departure-universal-time (make-trip :departure "garbage"))))

  (testing "driver-trips-today"
    (let* ((day-str (bn:format-booking-day (chronicity:parse "today")))
           (sheet (make-test-sheet
                   (make-block (format nil "~a           ~a" day-str day-str)
                               '(:dgh "SHMS" :lgh "Shrine")
                               '(:dgh ("8:00 am" "10:00 am") :lgh ("5:00 pm" "8:00 pm"))
                               '(:dgh (("BW" "CG") ("BW" "CG")) :lgh (("CG") ("CG")))))))
      (call-with-mock-read-sheet
       sheet
       (lambda ()
         ;; a week range covering every day of the year
         (let ((vb::*weeks* '("Jan 1-Dec 31")))
           (let ((trips (vb::trips-for-driver-today "BW")))
             (ok (= 1 (length trips)))
             (ok (string= "SHMS" (trip-destination (first trips)))))
           ;; CG drives the LGH trip but only rides on the DGH one
           (let ((trips (vb::trips-for-driver-today "cg")))
             (ok (= 1 (length trips)))
             (ok (string= "Shrine" (trip-destination (first trips)))))
           (ok (equal '() (vb::trips-for-driver-today "ZZ"))))
         ;; no sheet covers today
         (let ((vb::*weeks* '("Feb 30-Feb 31")))
           (ok (equal '() (vb::trips-for-driver-today "BW"))))))))

  (testing "return-driver-trips-today"
    ;; The return leg has its own driver, the first ret-passenger: DM drives
    ;; the DGH home although BW drove it out.
    (let* ((day-str (bn:format-booking-day (chronicity:parse "today")))
           (sheet (make-test-sheet
                   (make-block (format nil "~a           ~a" day-str day-str)
                               '(:dgh "SHMS")
                               '(:dgh ("8:35 am" "12 pm"))
                               '(:dgh (("BW" "DM") ("DM" "BW" "CG")))))))
      (call-with-mock-read-sheet
       sheet
       (lambda ()
         (let ((vb::*weeks* '("Jan 1-Dec 31")))
           (let ((trips (vb::trips-for-return-driver-today "dm")))
             (ok (= 1 (length trips)))
             (ok (string= "SHMS" (trip-destination (first trips)))))
           (ok (equal '() (vb::trips-for-return-driver-today "BW")))
           (ok (equal '() (vb::trips-for-driver-today "DM"))))))))

  (testing "return-universal-time"
    (multiple-value-bind (s m h) (decode-universal-time
                                  (vb::trip-return-universal-time (make-trip :return-time "12 pm")))
      (ok (= 0 s))
      (ok (= 0 m))
      (ok (= 12 h)))
    (ng (vb::trip-return-universal-time (make-trip :return-time "")))
    (ng (vb::trip-return-universal-time (make-trip :departure "8:35 am"))))

  (testing "remind-return-driver-wording"
    (ok (string= "BW is the driver for the return from SHMS at 12 pm with no other passengers."
                 (vb:remind-return-driver (make-trip :destination "SHMS" :return-time "12 pm"
                                                     :ret-passengers '("BW")))))
    (ok (string= "BW is the driver for the return from SHMS at 12 pm and DM is a passenger."
                 (vb:remind-return-driver (make-trip :destination "SHMS" :return-time "12 pm"
                                                     :ret-passengers '("BW" "DM")))))
    (ok (string= "BW is the driver for the return from SHMS at 12 pm and DM, CG are passengers."
                 (vb:remind-return-driver (make-trip :destination "SHMS" :return-time "12 pm"
                                                     :ret-passengers '("BW" "DM" "CG")))))
    ;; a trip that knows its vehicle names it
    (ok (string= "BW is the driver of the LGH for the return from SHMS at 12 pm and DM is a passenger."
                 (vb:remind-return-driver (make-trip :vehicle :lgh :destination "SHMS"
                                                     :return-time "12 pm"
                                                     :ret-passengers '("BW" "DM"))))))

  (testing "vehicle-phrase-wording"
    (ok (string= "the DGH" (vb::vehicle-phrase :dgh)))
    (ok (string= "the LGH" (vb::vehicle-phrase :lgh)))
    (ok (string= "the Chevy" (vb::vehicle-phrase :chevy)))
    (ok (string= "a personal vehicle" (vb::vehicle-phrase :personal)))
    (ng (vb::vehicle-phrase nil)))

  (testing "current-year-key"
    (let ((year (nth-value 5 (decode-universal-time (get-universal-time)))))
      (ok (= (+ (* year 10000) 422)
             (vb::current-year-date-key (chronicity:parse "April 22 2030"))))
      (ok (= (+ (* year 10000) 101)
             (vb::current-year-date-key (chronicity:parse "January 1 1999")))))))
