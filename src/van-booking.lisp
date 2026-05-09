;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/van-booking
  (:use #:common-lisp #:series)
  (:import-from #:alexandria)
  (:import-from #:cl-ppcre)
  (:import-from #:com.inuoe.jzon)
  (:import-from #:quri)
  (:import-from #:cl-csv)
  (:import-from #:dexador)
  (:import-from #:chronicity
                #:hour-of #:minute-of #:dow-of #:dow-name
                #:year-of #:month-of #:day-of)
  (:export
   ;; struct
   #:trip #:make-trip #:copy-trip #:trip-p
   #:trip-day #:trip-vehicle #:trip-destination #:trip-departure
   #:trip-return-time #:trip-dep-passengers #:trip-ret-passengers
   #:trip-block-start-row #:trip-dep-col #:trip-ret-col #:trip-sheet-name
   ;; public API
   #:parse-schedule #:find-trips #:find-empty-slot
   #:add-passengers #:create-booking #:book #:print-trip
   #:read-range #:write-range #:main
   #:book-natural
   #:remind-driver
   #:view-schedule
   #:trips-for-driver-today
   #:trip-departure-universal-time)
  (:documentation
   "
  - parse-schedule — reads the full sheet, filters CSV artifacts, extracts 72 trip
  structs with correct sheet row numbers
  - find-trips — filters by day and/or destination (substring, case-insensitive)
  - book — top-level entry point:
    - Existing trip found → adds passengers to both dep and ret columns
    - No match + times provided → creates new booking with vehicle priority DGH → LGH →
  Chevy
    - No match + no times → tells user to provide :dep-time and :ret-time
    - No empty slots → informs user
  - book-natural — natural language frontend to book
  - view-schedule — render a plaintext table of booked trips for a natural-language date range"))

(in-package #:project-isidore/van-booking)

(defparameter *spreadsheet-id* "1qxx5mDsrqAqGBrwLeSs0V6R_70FSNm1-ruFWxvHVgu8")

(defparameter *gid* "367982190")

(defparameter *weeks*
  '(("Mar 30-Apr 5 (Holy wk)" "367982190")
    ("Apr 6-12"               nil)
    ("Apr 13-19 "             nil)
    ("Apr 20-26"              nil)
    ("Apr 27-May 3"         nil))
  "List of (sheet-name gid-or-nil). GID is only needed for the default CSV read path.")

(defparameter *apps-script-url*
  "https://script.google.com/macros/s/AKfycbyR4Ffk5mjAUPUXOwQLIfjBiviyvm2rnNLpbBQvXAfTBNdAgEfR5KfE-45pPd2eTSz98g/exec")

(defparameter *vehicles*
  '((:dgh      5 6)
    (:lgh      8 9)
    (:chevy    2 3)
    (:personal 11 12))
  "Vehicle specs: (keyword dep-col ret-col). Order = booking priority (Personal excluded from auto).")

(defparameter *auto-vehicles* '(:dgh :lgh :chevy)
  "Vehicle priority order for automatic assignment.")

(defparameter *month-names*
  '("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defstruct trip
  day
  vehicle
  destination
  departure
  return-time
  dep-passengers
  ret-passengers
  block-start-row
  dep-col
  ret-col
  sheet-name)

(defun vehicle-cols (vehicle)
  "Return (dep-col ret-col) for VEHICLE keyword."
  (let ((spec (assoc vehicle *vehicles*)))
    (unless spec (error "Unknown vehicle: ~a" vehicle))
    (values (second spec) (third spec))))

(defun col-letter (idx)
  "Convert 0-indexed column number to spreadsheet letter (0=A, 2=C, etc.)."
  (string (code-char (+ (char-code #\A) idx))))

(defun non-blank-p (s)
  "Return T if S is a non-empty, non-blank string."
  (and (stringp s) (plusp (length (string-trim '(#\Space #\Tab) s)))))

(defun phantom-row-p (row)
  "Return T if ROW is a CSV artifact (single empty string element)."
  (and (= (length row) 1) (string= "" (first row))))

(defun clean-rows (rows)
  "Remove phantom CSV rows and return (values clean-rows sheet-row-map).
   SHEET-ROW-MAP is a vector where element i = real 1-indexed sheet row for clean-rows[i]."
  (let ((clean '())
        (mapping '())
        (sheet-row 0))
    (dolist (row rows)
      (if (phantom-row-p row)
          nil
          (progn
            (incf sheet-row)
            (push row clean)
            (push sheet-row mapping))))
    (values (nreverse clean)
            (coerce (nreverse mapping) 'vector))))

(defun safe-nth (n list)
  "Like NTH but returns \"\" if out of bounds."
  (or (nth n list) ""))

(defun extract-day-from-header (row)
  "Extract day string from the merged header row (col A has repeated day text).
   Returns e.g. \"Monday March 30\" or NIL."
  (let ((cell (string-trim '(#\Space #\Tab) (safe-nth 0 row))))
    (when (non-blank-p cell)
      (let ((pos (search "  " cell)))
        (if pos (subseq cell 0 pos) cell)))))

(defun header-cell-p (cell)
  "Return T if CELL looks like a block header rather than a passenger initial."
  (member cell '("Chevy" "DGH" "LGH" "Personal" "Dep." "Ret." "MONDAY" "Tuesday"
                 "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
          :test #'string-equal))

(defun collect-passengers (rows start-offset col max-rows)
  "Collect non-blank passenger initials from column COL starting at START-OFFSET in ROWS.
   Stops at empty cells, block headers, or after MAX-ROWS."
  (collect
      (until-if (lambda (cell) (or (not (non-blank-p cell)) (header-cell-p cell)))
                (map-fn 'string
                        (lambda (i) (string-trim '(#\Space #\Tab) (safe-nth col (nth i rows))))
                        (scan-range :from start-offset
                                    :below (min (+ start-offset max-rows) (length rows)))))))

(defun filter-new-passengers (passengers existing)
  "Return elements of PASSENGERS not already in EXISTING (string-equal comparison)."
  (collect (choose-if (lambda (p) (not (member p existing :test #'string-equal)))
                      (scan passengers))))

(defun parse-schedule (raw-rows &key sheet-name)
  "Parse spreadsheet RAW-ROWS into a list of TRIP structs.
   SHEET-NAME is stored on each trip for write-back.
   Block start rows are computed as 3 + block_index * 15 (fixed layout)."
  (multiple-value-bind (rows row-map) (clean-rows raw-rows)
    (declare (ignore row-map))
    (let ((trips '())
          (current-day nil)
          (block-index 0)
          (i 0))
      (loop while (< i (length rows))
            for row = (nth i rows)
            do (cond
                 ((and (>= (length row) 3)
                       (string-equal (string-trim " " (safe-nth 2 row)) "Chevy"))
                  (let ((day (extract-day-from-header row)))
                    (when day (setf current-day day)))
                  (let ((sheet-row (+ 3 (* block-index 15))))
                    ;; Verify next row is a destination row (not "Dep.")
                    ;; In gviz output, empty rows between header and Dep. may be dropped
                    (let* ((next-row (nth (1+ i) rows))
                           (has-dest-row (and next-row
                                              (not (string-equal "Dep."
                                                                 (string-trim " " (safe-nth 2 next-row))))))
                           (dest-row (if has-dest-row next-row nil))
                           (time-offset (if has-dest-row 3 2))
                           (pass-offset (if has-dest-row 4 3)))
                      (when (and current-day (< (+ i time-offset) (length rows)))
                        (let* ((time-row (nth (+ i time-offset) rows))
                               (time-row-empty
                                 (or (string-equal "Chevy" (string-trim " " (safe-nth 2 time-row)))
                                     (string-equal "Dep." (string-trim " " (safe-nth 2 time-row))))))
                          (iterate ((vspec (scan *vehicles*)))
                            (destructuring-bind (vname dep-col ret-col) vspec
                              (let ((dest (if dest-row
                                              (string-trim " " (safe-nth dep-col dest-row))
                                              ""))
                                    (dep-time (if time-row-empty ""
                                                  (string-trim " " (safe-nth dep-col time-row))))
                                    (ret-time (if time-row-empty ""
                                                  (string-trim " " (safe-nth ret-col time-row)))))
                                (push (make-trip
                                       :day current-day
                                       :vehicle vname
                                       :destination dest
                                       :departure dep-time
                                       :return-time ret-time
                                       :dep-passengers (collect-passengers rows (+ i pass-offset) dep-col 10)
                                       :ret-passengers (collect-passengers rows (+ i pass-offset) ret-col 10)
                                       :block-start-row sheet-row
                                       :dep-col dep-col
                                       :ret-col ret-col
                                       :sheet-name sheet-name)
                                      trips))))))))
                  (incf block-index)
                  (incf i))
                 (t (incf i))))
      (nreverse trips))))

(defun find-trips (trips &key day destination)
  "Return trips matching DAY (substring) and/or DESTINATION (case-insensitive substring).
   Both filters must match when provided. A blank DESTINATION filter matches no trips,
   so callers must use NIL (not \"\") to mean \"any destination\"."
  (collect
      (choose-if
       (lambda (tr)
         (and (or (null day)
                  (search (string-upcase day) (string-upcase (trip-day tr))))
              (or (null destination)
                  (and (non-blank-p destination)
                       (non-blank-p (trip-destination tr))
                       (search (string-upcase destination)
                               (string-upcase (trip-destination tr)))))))
       (scan trips))))

(defun print-trip (trip &optional (stream t))
  "Pretty-print a trip summary."
  (format stream "~a | ~a | ~a | Dep: ~a | Ret: ~a | Pax: ~{~a~^, ~}~@[ [~a]~]~%"
          (trip-day trip)
          (trip-vehicle trip)
          (trip-destination trip)
          (trip-departure trip)
          (trip-return-time trip)
          (trip-dep-passengers trip)
          (trip-sheet-name trip)))

(defun add-passengers (trip passengers)
  "Add PASSENGERS (list of initials) to TRIP in both dep and ret columns.
   Passengers already present in a column are skipped for that column.
   Writes to the spreadsheet using the trip's sheet-name."
  (let* ((new-dep (filter-new-passengers passengers (trip-dep-passengers trip)))
         (new-ret (filter-new-passengers passengers (trip-ret-passengers trip)))
         (passenger-start-offset 4)
         (dep-col-letter (col-letter (trip-dep-col trip)))
         (ret-col-letter (col-letter (trip-ret-col trip)))
         (sn (trip-sheet-name trip)))
    (when new-dep
      (let* ((dep-start-row (+ (trip-block-start-row trip) passenger-start-offset
                               (length (trip-dep-passengers trip))))
             (dep-range (format nil "~a~a:~a~a"
                                dep-col-letter dep-start-row
                                dep-col-letter (+ dep-start-row (length new-dep) -1))))
        (write-range dep-range (mapcar #'list new-dep) :sheet-name sn)))
    (when new-ret
      (let* ((ret-start-row (+ (trip-block-start-row trip) passenger-start-offset
                               (length (trip-ret-passengers trip))))
             (ret-range (format nil "~a~a:~a~a"
                                ret-col-letter ret-start-row
                                ret-col-letter (+ ret-start-row (length new-ret) -1))))
        (write-range ret-range (mapcar #'list new-ret) :sheet-name sn)))
    (let* ((added (remove-duplicates (append new-dep new-ret) :test #'string-equal))
           (skipped (set-difference passengers added :test #'string-equal)))
      (when added
        (format t "Added ~{~a~^, ~} to ~a ~a trip (~a)~%"
                added (trip-vehicle trip) (trip-destination trip) (trip-day trip)))
      (when skipped
        (format t "Skipped (already booked): ~{~a~^, ~}~%" skipped)))
    trip))

(defun find-empty-slot (trips day &optional preferred-vehicle)
  "Find an empty vehicle slot for DAY. If PREFERRED-VEHICLE given, try that first.
   Otherwise try *auto-vehicles* in priority order.
   Returns the empty trip struct or NIL."
  (let ((day-trips (find-trips trips :day day))
        (try-order (if preferred-vehicle
                       (list preferred-vehicle)
                       *auto-vehicles*)))
    (collect-first
     (choose-if #'identity
                (map-fn t (lambda (veh)
                            (find-if (lambda (tr)
                                       (and (eq (trip-vehicle tr) veh)
                                            (not (non-blank-p (trip-destination tr)))
                                            (not (non-blank-p (trip-departure tr)))))
                                     day-trips))
                        (scan try-order))))))

(defun create-booking (trips day destination dep-time ret-time passengers
                       &key vehicle)
  "Create a new booking in an empty slot. Auto-selects vehicle by priority if not given.
   Returns the trip or NIL if no slots available."
  (let ((slot (find-empty-slot trips day vehicle)))
    (unless slot
      (format t "No available vehicle slots for ~a~@[ (tried ~a only)~].~%" day vehicle)
      (return-from create-booking nil))
    (let ((sn (trip-sheet-name slot)))
      (let ((dest-row (1+ (trip-block-start-row slot)))
            (dep-col-letter (col-letter (trip-dep-col slot))))
        (write-range (format nil "~a~a" dep-col-letter dest-row)
                     (list (list destination)) :sheet-name sn))
      (let ((time-row (+ (trip-block-start-row slot) 3))
            (dep-col-letter (col-letter (trip-dep-col slot)))
            (ret-col-letter (col-letter (trip-ret-col slot))))
        (write-range (format nil "~a~a" dep-col-letter time-row)
                     (list (list dep-time)) :sheet-name sn)
        (write-range (format nil "~a~a" ret-col-letter time-row)
                     (list (list ret-time)) :sheet-name sn))
      (let* ((pass-start (+ (trip-block-start-row slot) 4))
             (dep-col-letter (col-letter (trip-dep-col slot)))
             (ret-col-letter (col-letter (trip-ret-col slot)))
             (dep-range (format nil "~a~a:~a~a"
                                dep-col-letter pass-start
                                dep-col-letter (+ pass-start (length passengers) -1)))
             (ret-range (format nil "~a~a:~a~a"
                                ret-col-letter pass-start
                                ret-col-letter (+ pass-start (length passengers) -1)))
             (values (mapcar #'list passengers)))
        (write-range dep-range values :sheet-name sn)
        (write-range ret-range values :sheet-name sn)))
    (format t "Created ~a booking on ~a → ~a (~a - ~a) for ~{~a~^, ~}~%"
            (trip-vehicle slot) day destination dep-time ret-time passengers)
    slot))

(defun book (day destination passengers &key dep-time ret-time vehicle week)
  "Book PASSENGERS for a trip on DAY to DESTINATION.
   If an existing trip matches, adds passengers to it.
   Otherwise creates a new booking (requires DEP-TIME and RET-TIME).
   VEHICLE can override automatic selection (DGH > LGH > Chevy).
   WEEK is the sheet name (e.g. \"Apr 20-26\"). If omitted, searches all weeks."
  (let* ((weeks-to-search (if week
                              (list (list week nil))
                              *weeks*))
         (all-trips (collect-append
                     (map-fn 'list
                             (lambda (w)
                               (let* ((sheet-name (first w))
                                      (rows (read-range nil :sheet-name sheet-name)))
                                 (parse-schedule rows :sheet-name sheet-name)))
                             (scan weeks-to-search)))))
    (let ((matches (find-trips all-trips :day day :destination destination)))
      (cond
        (matches
         (format t "Found ~d existing trip~:p to ~a on ~a:~%" (length matches) destination day)
         (iterate ((m (scan matches))) (print-trip m))
         (let ((target (first matches)))
           (format t "Adding passengers to ~a trip (~a)...~%"
                   (trip-vehicle target) (trip-sheet-name target))
           (add-passengers target passengers)))
        ((and dep-time ret-time)
         (create-booking all-trips day destination dep-time ret-time passengers
                         :vehicle vehicle))
        (t
         (format t "No existing trips to ~a on ~a.~%Provide :dep-time and :ret-time to create a new booking.~%"
                 destination day)
         nil)))))

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

(defun parse-passenger-initials (text)
  "Extract uppercase 2-3 character initials from TEXT."
  (cl-ppcre:all-matches-as-strings "[A-Z]{2,3}" text))

(defparameter *day-phrase-pattern*
  "(?:(?:next\\s+(?:week\\s+)?|this\\s+|last\\s+)?(?:monday|tuesday|wednesday|thursday|friday|saturday|sunday)|tomorrow|today|yesterday|(?:january|february|march|april|may|june|july|august|september|october|november|december|jan|feb|mar|apr|jun|jul|aug|sep|sept|oct|nov|dec)\\.?\\s+\\d{1,2}(?:st|nd|rd|th)?)"
  "CL-PPCRE pattern matching temporal day expressions understood by chronicity.")

(defparameter *booking-normalizations*
  '(;; --- Preamble stripping (must be first, anchored to start) ---
    ("(?i)^(?:I(?:'d| would) like to|I need to|I want to|can you(?: please)?|could you(?: please)?|please)\\s+" . "")
    ("(?i)^(?:book|reserve|schedule|get)\\s+(?:(?:a|the)\\s+)?(?:van\\s+)?" . "")
    ("(?i)^(dgh|lgh|chevy|personal)\\b" . "use \\1")

    ;; --- Period-style meridiem → plain am/pm (must precede time rules) ---
    ("(?i)\\ba\\.m\\.?" . "am")
    ("(?i)\\bp\\.m\\.?" . "pm")

    ;; --- Time-range: "from X to Y" → "at X to Y" ---
    ("(?i)\\bfrom\\s+(.+?)\\s+to\\b"    . "at \\1 to")

    ;; --- Departure-time introducers → "at" ---
    ("(?i)\\bstarting\\s+at\\b"          . "at")
    ("(?i)\\bstarting\\b"               . "at")
    ("(?i)\\bdeparting\\s+at\\b"         . "at")
    ("(?i)\\bdeparting\\b"              . "at")
    ("(?i)\\bleaving\\s+at\\b"           . "at")
    ("(?i)\\bleaving\\b"                . "at")
    ("(?i)\\bbeginning\\s+at\\b"         . "at")
    ("(?i)\\bbeginning\\b"              . "at")
    ("(?i)\\bgoing\\s+at\\b"             . "at")
    ("(?i)\\bpick(?:ing)?\\s*up\\s+at\\b" . "at")

    ;; --- Return-time introducers → "to" ---
    ("(?i)\\buntil\\b"                  . "to")
    ("(?i)\\btill\\b"                   . "to")
    ("(?i)\\bthrough\\b"               . "to")
    ("(?i)\\breturning\\s+at\\b"         . "to")
    ("(?i)\\breturning\\s+by\\b"         . "to")
    ("(?i)\\breturning\\b"              . "to")
    ("(?i)\\bending\\s+at\\b"            . "to")
    ("(?i)\\bending\\b"                 . "to")
    ("(?i)\\bbe\\s+back\\s+(?:by|at)\\b"  . "to")
    ("(?i)\\bget\\s+back\\s+(?:by|at)\\b" . "to")
    ("(?i)\\bback\\s+(?:by|at)\\b"       . "to")
    ("(?i)\\bdrop(?:ping)?\\s*off\\s+at\\b" . "to")
    ("(?i)\\barrive\\s+(?:by|at)\\b"      . "to")
    ("(?i)\\breturn\\s+(?:by|at)\\b"      . "to")
    ("(?i)\\bdone\\s+(?:by|at)\\b"        . "to")
    ("(?i)\\bfinish(?:ed)?\\s+(?:by|at)\\b" . "to")

    ;; --- Destination introducers → "for" ---
    ("(?i)\\b(?:going|headed|heading|driving)\\s+to\\b" . "for")

    ;; --- Vehicle preference → "use VEHICLE" ---
    ("(?i)\\b(?:take|prefer|in)\\s+(?:the\\s+)?(dgh|lgh|chevy|personal)\\b" . "use \\1")

    ;; --- Passenger introducers → "for" ---
    ("(?i)\\bwith\\s+([A-Z]{2,3})"      . "for \\1")
    ("(?i)\\bcarrying\\s+([A-Z]{2,3})"  . "for \\1")
    ("(?i)\\btaking\\s+([A-Z]{2,3})"    . "for \\1")
    ("(?i)\\bbringing\\s+([A-Z]{2,3})"  . "for \\1")
    ("(?i)\\bpassengers?\\s+([A-Z]{2,3})" . "for \\1"))
  "Alist of (regex . replacement) pairs applied to normalize booking text
before the structural parse.  Order matters: the from→at rule must precede
individual departure-time introducers.")

(defparameter *time-expr-pattern*
  "(?:\\d{1,2}:\\d{2}\\s*(?:am|pm)?|\\d{1,2}\\s*(?:am|pm)|(?:one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve)\\s+(?:am|pm))"
  "CL-PPCRE pattern matching a single time expression (H:MM with optional am/pm, or H am/pm).")

(defun normalize-booking-text (text)
  "Canonicalize synonyms in TEXT for parse-booking-text.
Collapses whitespace and applies *booking-normalizations* in order."
  (let ((initial (cl-ppcre:regex-replace-all "\\s+" (string-trim " " text) " ")))
    (collect-fn 'string
                (lambda () initial)
                (lambda (acc pair)
                  (cl-ppcre:regex-replace-all (car pair) acc (cdr pair)))
                (scan *booking-normalizations*))))

(defun all-initials-p (text)
  "Return T if TEXT consists entirely of uppercase 2-3 char initials with separators (and, &, commas)."
  (let* ((trimmed (string-trim " " text))
         (cleaned (cl-ppcre:regex-replace-all "(?i)\\b(?:and|&)\\b|," trimmed " "))
         (cleaned (cl-ppcre:regex-replace-all "\\s+" (string-trim " " cleaned) " ")))
    (and (plusp (length cleaned))
         (every (lambda (token) (cl-ppcre:scan "^[A-Z]{2,3}$" token))
                (cl-ppcre:split "\\s+" cleaned)))))

(defun parse-booking-text (text)
  "Parse natural language booking TEXT into components using order-agnostic extraction.
Returns (values destination day-phrase passengers-raw dep-time-raw ret-time-raw vehicle)
or NIL if required components (destination, passengers, times) cannot be found.
Day defaults to \"today\" if not specified.  VEHICLE is a keyword (:DGH, :LGH, :CHEVY,
:PERSONAL) or NIL.

Components may appear in any order:
  \"book a van for swimming next Tuesday for CG at 7 pm to 9 pm\"
  \"I'd like to reserve the DGH for a haircut from 12:30pm to 2:30pm for GG.\"
  \"for CG and BW at seven PM to nine PM going to swimming next week Tuesday\""
  (let ((norm (normalize-booking-text text)))
    ;; Strip trailing punctuation
    (setf norm (string-trim " " (cl-ppcre:regex-replace "[.!?]+\\s*$" norm "")))
    (let (dep-raw ret-raw day-phrase pax-raw dest vehicle)
      ;; 1. Extract time range: "at DEP-TIME to RET-TIME"
      (let ((time-re (concatenate 'string
                                  "(?i)\\bat\\s+(" *time-expr-pattern* ")\\s+to\\s+("
                                  *time-expr-pattern* ")")))
        (cl-ppcre:register-groups-bind (d r) (time-re norm)
          (setf dep-raw d ret-raw r))
        (when dep-raw
          (setf norm (string-trim " "
                                  (cl-ppcre:regex-replace-all "\\s+"
                                                              (cl-ppcre:regex-replace time-re norm " ") " ")))))
      ;; 1b. Fallback: bare "TIME to TIME" without "at" introducer
      (unless dep-raw
        (let ((bare-re (concatenate 'string
                                    "(?i)(" *time-expr-pattern* ")\\s+to\\s+("
                                    *time-expr-pattern* ")")))
          (cl-ppcre:register-groups-bind (d r) (bare-re norm)
            (setf dep-raw d ret-raw r))
          (when dep-raw
            (setf norm (string-trim " "
                                    (cl-ppcre:regex-replace-all "\\s+"
                                                                (cl-ppcre:regex-replace bare-re norm " ") " "))))))
      ;; 2. Extract vehicle preference: "use DGH" etc.
      (let ((veh-re "(?i)\\buse\\s+(?:the\\s+)?(dgh|lgh|chevy|personal)\\b"))
        (cl-ppcre:register-groups-bind (v) (veh-re norm)
          (when v
            (setf vehicle (find (string-upcase v)
                                '(:dgh :lgh :chevy :personal)
                                :key #'symbol-name :test #'string=))
            (setf norm (string-trim " "
                                    (cl-ppcre:regex-replace-all "\\s+"
                                                                (cl-ppcre:regex-replace veh-re norm " ") " "))))))
      ;; 3. Extract day phrase (optionally preceded by "on")
      (let ((day-re (concatenate 'string
                                 "(?i)\\b(?:on\\s+)?(" *day-phrase-pattern* ")\\b")))
        (cl-ppcre:register-groups-bind (d) (day-re norm)
          (setf day-phrase d))
        (when day-phrase
          (setf norm (string-trim " "
                                  (cl-ppcre:regex-replace-all "\\s+"
                                                              (cl-ppcre:regex-replace day-re norm " ") " ")))))
      ;; 4. Classify "for" segments as passengers or destination
      (let ((segments (cl-ppcre:split "(?i)\\bfor\\b" norm)))
        (dolist (seg (rest segments))
          (let ((trimmed (string-trim " " seg)))
            (when (plusp (length trimmed))
              (if (all-initials-p trimmed)
                  (unless pax-raw (setf pax-raw trimmed))
                  (unless dest (setf dest trimmed)))))))
      (when (and dest pax-raw dep-raw ret-raw)
        (values dest (or day-phrase "today") pax-raw dep-raw ret-raw vehicle)))))

(defun book-natural (text &key week)
  "
Examples:
  (book-natural \"book a van for swimming next Tuesday for CG and BW at 7 pm to 9 pm\")
  (book-natural \"I'd like to reserve the DGH for a haircut from 12:30pm to 2:30pm for GG.\")
  (book-natural \"going to swimming tomorrow for CG 8am to 10am\")

DAY-PHRASE is resolved by chronicity (supports: \"next Tuesday\", \"next week Friday\",
\"this Monday\", \"tomorrow\", etc.).  DEP-TIME and RET-TIME are likewise resolved
\(\"seven PM\", \"9:00 am\", etc.).  PASSENGERS are uppercase 2-3 letter initials."
  (multiple-value-bind (dest day-phrase passengers-raw dep-raw ret-raw vehicle)
      (parse-booking-text text)
    (unless dest
      (let ((missing '()))
        (let ((norm (normalize-booking-text text)))
          (setf norm (string-trim " " (cl-ppcre:regex-replace "[.!?]+\\s*$" norm "")))
          (let ((time-re (concatenate 'string
                                      "(?i)\\bat\\s+(" *time-expr-pattern* ")\\s+to\\s+("
                                      *time-expr-pattern* ")"))
                (bare-re (concatenate 'string
                                      "(?i)(" *time-expr-pattern* ")\\s+to\\s+("
                                      *time-expr-pattern* ")")))
            (unless (or (cl-ppcre:scan time-re norm)
                        (cl-ppcre:scan bare-re norm))
              (push "times (e.g. 'from 8am to 10am' or '8am to 10am')" missing))
            (setf norm (string-trim " " (cl-ppcre:regex-replace-all "\\s+"
                                                                    (cl-ppcre:regex-replace (if (cl-ppcre:scan time-re norm) time-re bare-re)
                                                                                            norm " ") " "))))
          (let ((segments (cl-ppcre:split "(?i)\\bfor\\b" norm))
                (has-pax nil) (has-dest nil))
            (dolist (seg (rest segments))
              (let ((trimmed (string-trim " " seg)))
                (when (plusp (length trimmed))
                  (if (all-initials-p trimmed)
                      (setf has-pax t)
                      (setf has-dest t)))))
            (unless has-pax (push "passengers (e.g. 'for BW' or 'for CG and BW')" missing))
            (unless has-dest (push "destination (e.g. 'for swimming' or 'for haircut')" missing))))
        (error "Cannot parse booking request: missing ~{~A~^, ~}.~%~
                Examples:~%  ~
                  'for swimming tomorrow for CG from 8am to 10am'~%  ~
                  'going to the pool next Tuesday for BW at 7 pm to 9 pm'~%  ~
                  'reserve the DGH for haircut for MM 8:35am to 10:25am'~%~
                Got: ~A" missing text)))
    (let* ((day-dt (chronicity:parse day-phrase :context :future))
           (dep-dt (chronicity:parse dep-raw    :context :future))
           (ret-dt (chronicity:parse ret-raw    :context :future))
           (day    (format-booking-day day-dt))
           (dep    (format-time-12h (hour-of dep-dt) (minute-of dep-dt)))
           (ret    (format-time-12h (hour-of ret-dt) (minute-of ret-dt)))
           (pax    (parse-passenger-initials passengers-raw)))
      (apply #'book day dest pax :dep-time dep :ret-time ret
             (append (when vehicle (list :vehicle vehicle))
                     (when week (list :week week)))))))

(defun month-abbrev-num (abbrev)
  "Return 1-based month number from ABBREV like \"Mar\", \"Apr\", or \"May\".
Matches case-insensitively as a prefix of *month-names*."
  (1+ (or (position abbrev *month-names*
                    :test (lambda (a b)
                            (and (>= (length b) (length a))
                                 (string-equal a b :end2 (length a)))))
          (error "Unknown month abbreviation: ~a" abbrev))))

(defun date-key (dt)
  "Return YYYYMMDD integer for chronicity datetime DT, suitable for ordering."
  (+ (* (year-of dt) 10000) (* (month-of dt) 100) (day-of dt)))

(defun current-year-date-key (dt)
  "Like DATE-KEY but always uses today's year. Use this when comparing
   against ranges from *WEEKS*, which PARSE-WEEK-RANGE anchors to the
   current year. This avoids :context :future rolling absolute month-day
   phrases (e.g. \"April 22\") into the next year and missing the sheet."
  (+ (* (year-of (chronicity:parse "today")) 10000)
     (* (month-of dt) 100)
     (day-of dt)))

(defun parse-week-range (sheet-name)
  "Parse SHEET-NAME like \"Apr 20-26\" or \"Mar 30-Apr 5 (Holy wk)\".
Returns (values start-key end-key) as YYYYMMDD integers using the current
year, or (values nil nil) if the name does not match the expected shape."
  (cl-ppcre:register-groups-bind (sm sd em ed)
      ("^([A-Za-z]+)\\s+(\\d+)\\s*-\\s*(?:([A-Za-z]+)\\s+)?(\\d+)" sheet-name)
    (let* ((year (year-of (chronicity:parse "today")))
           (start-month (month-abbrev-num sm))
           (end-month (if em (month-abbrev-num em) start-month))
           (start-day (parse-integer sd))
           (end-day (parse-integer ed)))
      (values (+ (* year 10000) (* start-month 100) start-day)
              (+ (* year 10000) (* end-month 100) end-day)))))

(defun find-week-sheet (target-dt)
  "Return the sheet-name from *weeks* whose date range covers TARGET-DT
\(inclusive), or NIL if none match."
  (let ((target-key (current-year-date-key target-dt)))
    (loop for entry in *weeks*
          for name = (first entry)
          do (multiple-value-bind (start end) (parse-week-range name)
               (when (and start (<= start target-key end))
                 (return name))))))

(defun remind-driver (driver &optional (day-phrase "today"))
  "Return a string summarizing the trips DRIVER (e.g. \"BW\") is driving on
DAY-PHRASE (natural-language string parsed by chronicity, defaults to today).
The driver is the first initial in dep-passengers. If multiple trips match,
all are listed in one string separated by spaces."
  (let* ((day-dt (chronicity:parse day-phrase :context :future))
         (day-str (format-booking-day day-dt))
         (sheet (find-week-sheet day-dt)))
    (unless sheet
      (return-from remind-driver
        (format nil "No schedule sheet covers ~a." day-str)))
    (let* ((rows (read-range nil :sheet-name sheet))
           (trips (parse-schedule rows :sheet-name sheet))
           (day-trips (find-trips trips :day day-str))
           (driver-trips
             (remove-if-not
              (lambda (tr)
                (let ((pax (trip-dep-passengers tr)))
                  (and pax (string-equal driver (first pax)))))
              day-trips)))
      (if (null driver-trips)
          (format nil "~a is not driving on ~a." driver day-str)
          (format nil "~{~a~^ ~}"
                  (mapcar
                   (lambda (tr)
                     (let ((others (rest (trip-dep-passengers tr))))
                       (cond
                         ((null others)
                          (format nil "~a is the driver for a trip to ~a departing at ~a with no other passengers."
                                  driver (trip-destination tr) (trip-departure tr)))
                         ((null (rest others))
                          (format nil "~a is the driver for a trip to ~a departing at ~a and ~a is a passenger."
                                  driver (trip-destination tr) (trip-departure tr) (first others)))
                         (t
                          (format nil "~a is the driver for a trip to ~a departing at ~a and ~{~a~^, ~} are passengers."
                                  driver (trip-destination tr) (trip-departure tr) others)))))
                   driver-trips))))))

(defun trips-for-driver-today (initial)
  "Return the trips departing today where INITIAL is the first dep-passenger.
Used by the slack-bot reminder scheduler. Returns NIL when no sheet covers
today or no matching trips exist."
  (let* ((day-dt (chronicity:parse "today"))
         (day-str (format-booking-day day-dt))
         (sheet (find-week-sheet day-dt)))
    (when sheet
      (let* ((rows (read-range nil :sheet-name sheet))
             (trips (parse-schedule rows :sheet-name sheet))
             (day-trips (find-trips trips :day day-str)))
        (remove-if-not
         (lambda (tr)
           (let ((pax (trip-dep-passengers tr)))
             (and pax (string-equal initial (first pax)))))
         day-trips)))))

(defun trip-departure-universal-time (trip)
  "Return the CL universal-time at which TRIP departs today, or NIL.
Combines today's date with TRIP-DEPARTURE's HH:MM (12h format)."
  (let ((mins (departure-minutes (trip-departure trip))))
    (when (and mins (plusp mins))
      (multiple-value-bind (s m h day month year)
          (decode-universal-time (get-universal-time))
        (declare (ignore s m h))
        (encode-universal-time 0
                               (mod mins 60)
                               (floor mins 60)
                               day month year)))))

(defun split-range-phrase (phrase)
  "Split PHRASE on ' to ', ' through ', or ' - ' into (values start end).
   Returns (values phrase phrase) when no separator is found (1-day range)."
  (let* ((trimmed (string-trim " " phrase))
         (pattern "(?i)\\s+(?:to|through|-)\\s+")
         (parts (cl-ppcre:split pattern trimmed :limit 2)))
    (if (= (length parts) 2)
        (values (string-trim " " (first parts))
                (string-trim " " (second parts)))
        (values trimmed trimmed))))

(defun trip-date-key (trip)
  "Return YYYYMMDD key for TRIP by parsing its day string, or NIL on failure.
   TRIP-DAY is formatted as \"DayOfWeek MonthName Day\" (e.g., \"Monday April 20\").
   Chronicity does not parse that combined form, so strip the weekday first.
   The year is forced to today's year so the key matches the fixed-year
   ranges in *WEEKS*."
  (let ((day (trip-day trip)))
    (when (non-blank-p day)
      (handler-case
       (let* ((trimmed (string-trim " " day))
              (space (position #\  trimmed))
              (tail
               (if space
                   (subseq trimmed (1+ space))
                   trimmed))
              (dt (chronicity:parse tail :context :future)))
         (when dt (current-year-date-key dt)))
       (error nil nil)))))

(defun find-week-sheets-in-range (start-key end-key)
  "Return the sheet names from *weeks* whose date range overlaps [START-KEY, END-KEY]."
  (loop for entry in *weeks*
        for name = (first entry)
        append (multiple-value-bind (ws we) (parse-week-range name)
                 (when (and ws we (<= ws end-key) (>= we start-key))
                   (list name)))))

(defun format-short-date (day-str)
  "Reformat a TRIP-DAY string like \"Monday April 20\" as \"Mon Apr 20\".
   Falls back to DAY-STR unchanged if parsing fails."
  (handler-case
      (let* ((trimmed (string-trim " " day-str))
             (space (position #\Space trimmed))
             (tail (if space (subseq trimmed (1+ space)) trimmed))
             (weekday (if space (subseq trimmed 0 space) ""))
             (dt (chronicity:parse tail :context :future)))
        (if dt
            (format nil "~a ~a ~D"
                    (subseq (string-capitalize weekday) 0
                            (min 3 (length weekday)))
                    (subseq (nth (1- (month-of dt)) *month-names*) 0 3)
                    (day-of dt))
            day-str))
    (error () day-str)))

(defun vehicle-priority (vehicle)
  "Return a sort key for VEHICLE based on position in *vehicles* (lower = higher priority)."
  (or (position vehicle *vehicles* :key #'first) 999))

(defun departure-minutes (time-str)
  "Convert a 12h TIME-STR like \"8:35 am\" to minutes-since-midnight. 0 on failure."
  (or (handler-case
          (let ((dt (chronicity:parse (or time-str "") :context :future)))
            (when dt (+ (* 60 (hour-of dt)) (minute-of dt))))
        (error () nil))
      0))

(defun truncate-cell (s width)
  "Truncate S to WIDTH chars, appending '...' when shortened."
  (let ((s (or s "")))
    (if (> (length s) width)
        (concatenate 'string (subseq s 0 (max 0 (- width 3))) "...")
        s)))

(defun booked-trip-p (trip)
  "Return T if TRIP has any booking data (destination or departure time)."
  (or (non-blank-p (trip-destination trip))
      (non-blank-p (trip-departure trip))))

(defun format-schedule-table (trips)
  "Render TRIPS as a plaintext fixed-width table. Returns the full string."
  (let* ((rows (mapcar
                (lambda (tr)
                  (list (format-short-date (trip-day tr))
                        (string-upcase (symbol-name (trip-vehicle tr)))
                        (or (trip-destination tr) "")
                        (or (trip-departure tr) "")
                        (or (trip-return-time tr) "")
                        (format nil "~{~a~^, ~}" (trip-dep-passengers tr))
                        (format nil "~{~a~^, ~}" (trip-ret-passengers tr))))
                trips))
         (headers '("Date" "Vehicle" "Destination" "Dep" "Ret" "Dep Pax" "Ret Pax"))
         (widths
           (loop for i from 0 below (length headers)
                 collect (max (length (nth i headers))
                              (reduce #'max rows
                                      :key (lambda (r) (length (nth i r)))
                                      :initial-value 0))))
         (out (make-string-output-stream)))
    (labels ((emit-row (cells)
               (loop for cell in cells
                     for w in widths
                     for i from 0
                     do (format out "~vA" w (truncate-cell cell w))
                        (when (< i (1- (length widths)))
                          (format out "  ")))
               (format out "~%")))
      (emit-row headers)
      (emit-row (mapcar (lambda (w) (make-string w :initial-element #\-)) widths))
      (dolist (r rows) (emit-row r)))
    (get-output-stream-string out)))

(defun view-schedule (&optional (range-phrase "today"))
  "Display booked trips over a natural-language date range as a plaintext table.
   RANGE-PHRASE may be a single date (\"today\", \"tomorrow\", \"Apr 20\") for a 1-day
   window, or a range using 'to', 'through', or '-' as the separator
   (\"today to next Friday\", \"Apr 20 through Apr 26\"). Defaults to \"today\".
   Returns the rendered string and also prints it."
  (multiple-value-bind (start-phrase end-phrase) (split-range-phrase range-phrase)
    (let ((start-dt (chronicity:parse start-phrase :context :future))
          (end-dt   (chronicity:parse end-phrase   :context :future)))
      (unless (and start-dt end-dt)
        (error "Could not parse date range: ~a" range-phrase))
      (let* ((sk (current-year-date-key start-dt))
             (ek (current-year-date-key end-dt))
             (start-key (min sk ek))
             (end-key   (max sk ek))
             (start-label (format-booking-day (if (<= sk ek) start-dt end-dt)))
             (end-label   (format-booking-day (if (<= sk ek) end-dt start-dt)))
             (sheets (find-week-sheets-in-range start-key end-key)))
        (if (null sheets)
            (let ((msg (format nil "No schedule sheets cover ~a~@[ through ~a~].~%"
                               start-label
                               (unless (= start-key end-key) end-label))))
              (format t "~a" msg)
              msg)
            (let* ((all-trips
                     (loop for sheet in sheets
                           append (parse-schedule
                                   (read-range nil :sheet-name sheet)
                                   :sheet-name sheet)))
                   (in-range
                     (remove-if-not
                      (lambda (tr)
                        (let ((k (trip-date-key tr)))
                          (and k (<= start-key k end-key)
                               (booked-trip-p tr))))
                      all-trips))
                   (sorted
                     (sort in-range
                           (lambda (a b)
                             (let ((ka (trip-date-key a))
                                   (kb (trip-date-key b)))
                               (cond ((< ka kb) t)
                                     ((> ka kb) nil)
                                     (t (let ((da (departure-minutes (trip-departure a)))
                                              (db (departure-minutes (trip-departure b))))
                                          (cond ((< da db) t)
                                                ((> da db) nil)
                                                (t (< (vehicle-priority (trip-vehicle a))
                                                      (vehicle-priority (trip-vehicle b))))))))))))
                   (out (if sorted
                            (format-schedule-table sorted)
                            (format nil "No trips scheduled from ~a to ~a.~%"
                                    start-label end-label))))
              (format t "~a" out)
              out))))))

(defun read-range (range &key sheet-name)
  "Read cells from the public Google Sheet via CSV export. Returns a list of rows.
   If SHEET-NAME is given, uses gviz endpoint (supports sheet names)."
  (if sheet-name
      (let* ((url (format nil "https://docs.google.com/spreadsheets/d/~a/gviz/tq?tq=select+*&sheet=~a&headers=0&tqx=out:csv"
                          *spreadsheet-id* (quri:url-encode sheet-name)))
             (body (dex:get url)))
        (cl-csv:read-csv body))
      (let* ((url (format nil
                          "https://docs.google.com/spreadsheets/d/~a/export?format=csv&range=~a&gid=~a"
                          *spreadsheet-id* range *gid*))
             (body (dex:get url)))
        (cl-csv:read-csv body))))

(defun write-range (range values &key sheet-name)
  "Write VALUES (list of rows) to RANGE via the Apps Script doPost endpoint.
   If SHEET-NAME is given, prepends it to the range for sheet-qualified writes."
  (let* ((qualified-range (if sheet-name
                              (format nil "~a!~a" sheet-name range)
                              range))
         (args (alexandria:plist-hash-table
                (list "range" qualified-range "valueInputOption" "USER_ENTERED") :test #'equal))
         (body-obj (alexandria:plist-hash-table
                    (list "values" (map 'vector (lambda (row) (coerce row 'vector)) values))
                    :test #'equal))
         (root (alexandria:plist-hash-table
                (list "arguments" args "body" body-obj) :test #'equal))
         (payload (com.inuoe.jzon:stringify root))
         (url (format nil "~a?spreadsheetId=~a" *apps-script-url* *spreadsheet-id*)))
    (multiple-value-bind (body status headers)
        (dex:request url
                     :method :post
                     :content payload
                     :headers '(("Content-Type" . "application/json"))
                     :max-redirects 0)
      (declare (ignore body))
      (let ((location (gethash "location" headers)))
        (if location
            (dex:get location)
            (error "Expected redirect from Apps Script, got status ~a" status))))))
