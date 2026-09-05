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
                #:hour-of #:minute-of #:year-of #:month-of #:day-of)
  (:import-from #:local-time)
  (:import-from #:project-isidore/booking-nlp
                #:parse-booking
                #:booking-request-destination #:booking-request-day
                #:booking-request-passengers #:booking-request-dep-time
                #:booking-request-ret-time #:booking-request-vehicle
                #:booking-request-missing #:booking-request-understood
                #:booking-incomplete
                #:format-time-12h #:format-booking-day #:*month-names*)
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
   #:fetch-all-trips #:known-destinations
   #:sheet-tabs #:week-sheets
   #:book-natural
   #:remind-driver
   #:view-schedule
   #:date-range-phrase-p
   #:trips-for-driver-today
   #:trip-departure-universal-time)
  (:documentation
   "
  - parse-schedule — reads the full sheet, filters CSV artifacts, extracts 72 trip
  structs with correct sheet row numbers
  - find-trips — filters by day and/or destination (substring, case-insensitive)
  - sheet-tabs / week-sheets — list the spreadsheet's tabs (scraped from its public
  HTML view) and the subset whose names are week ranges such as \"Aug 31-Sep 6\";
  the week list is cached briefly and can be pinned by binding *weeks*
  - fetch-all-trips — reads and parses every week sheet (or one week)
  - book — top-level entry point:
    - Existing trip found → adds passengers to both dep and ret columns
    - No match + times provided → creates new booking with vehicle priority DGH → LGH →
  Chevy
    - No match + no times → signals BOOKING-INCOMPLETE naming the absent time(s)
    - No week sheet covers the day → informs user (before asking for times)
    - No empty slots → informs user
  - book-natural — free-form English frontend to book (see project-isidore/booking-nlp).
    Signals BOOKING-INCOMPLETE, whose report asks the user to re-send the request
    with the missing pieces, when destination, day or passengers cannot be found.
  - view-schedule — render a plaintext table of booked trips for a natural-language date range"))

(in-package #:project-isidore/van-booking)

(defparameter *spreadsheet-id* "1qxx5mDsrqAqGBrwLeSs0V6R_70FSNm1-ruFWxvHVgu8")

(defparameter *gid* "367982190")

(defparameter *weeks* nil
  "Optional fixed list of (sheet-name gid-or-nil) week sheets.  NIL, the
default, means WEEK-SHEETS discovers the current week tabs from the
spreadsheet itself, so the code keeps working as new weeks are added.
Bind or set this to a fixed list (e.g. '((\"Apr 20-26\" nil))) to bypass
discovery; the tests do that.  GID is only needed for the default CSV read
path.")

(defparameter *week-sheets-cache-seconds* 300
  "How long, in seconds, a discovered week-sheet list is reused by WEEK-SHEETS
before the spreadsheet's tab list is fetched again.")

(defvar *week-sheets-cache* nil
  "(fetched-universal-time . week-sheets) from the last discovery, or NIL.")

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

(defun label-row-p (row)
  "Return T if ROW carries no booking data: every cell from column C on is
blank or a block label (see HEADER-CELL-P).  That covers the
\"Chevy/DGH/LGH/Personal\" and \"Dep./Ret.\" header rows and the weekday
reminder the sheet keeps in the Personal column (e.g. \"Saturday\" in L) on
the last row of each block.  Columns A and B are ignored because the merged
day text lives in A on header rows.

PARSE-SCHEDULE uses this to decide whether the row at the time offset really
holds times: the gviz CSV drops empty rows, so for a block with no times the
row found there is that weekday reminder, and reading it as data made every
unbooked block look like a Personal trip departing at \"Saturday\"."
  (every (lambda (cell)
           (let ((c (string-trim '(#\Space #\Tab) cell)))
             (or (not (non-blank-p c)) (header-cell-p c))))
         (nthcdr 2 row)))

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
                               ;; gviz drops empty rows, so with no times the
                               ;; row at this offset is the next block header,
                               ;; a "Dep./Ret." row, or the weekday reminder in
                               ;; column L.  LABEL-ROW-P covers all three.
                               (time-row-empty (label-row-p time-row)))
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

(defun fetch-all-trips (&key week)
  "Read and parse the schedule for every week sheet (see WEEK-SHEETS), or only
   WEEK (a sheet name such as \"Apr 20-26\") when given.  Returns a list of
   TRIP structs."
  (let ((weeks-to-search (if week
                             (list (list week nil))
                             (week-sheets))))
    (collect-append
     (map-fn 'list
             (lambda (w)
               (let* ((sheet-name (first w))
                      (rows (read-range nil :sheet-name sheet-name)))
                 (parse-schedule rows :sheet-name sheet-name)))
             (scan weeks-to-search)))))

(defun known-destinations (trips)
  "Return the distinct non-blank destinations found in TRIPS, in the sheet's
   own spelling.  Used as a gazetteer by BOOK-NATURAL."
  (remove-duplicates
   (remove-if-not #'non-blank-p (mapcar #'trip-destination trips))
   :test #'string-equal :from-end t))

(defun book (day destination passengers &key dep-time ret-time vehicle week trips)
  "Book PASSENGERS for a trip on DAY to DESTINATION.
   If an existing trip matches, adds passengers to it (no times needed).
   If no week sheet covers DAY (the sheet has no blocks for it), says so and
   returns NIL rather than asking for times or reporting full vehicles.
   Otherwise creates a new booking, which requires DEP-TIME and RET-TIME; when
   either is missing, signals BOOKING-INCOMPLETE so the caller can ask for them.
   VEHICLE can override automatic selection (DGH > LGH > Chevy).
   WEEK is the sheet name (e.g. \"Apr 20-26\"). If omitted, searches all weeks.
   TRIPS, when given, is a pre-fetched list from FETCH-ALL-TRIPS and skips the read."
  (let* ((all-trips (or trips (fetch-all-trips :week week)))
         (matches (find-trips all-trips :day day :destination destination)))
    (cond
      (matches
       (format t "Found ~d existing trip~:p to ~a on ~a:~%" (length matches) destination day)
       (iterate ((m (scan matches))) (print-trip m))
       (let ((target (first matches)))
         (format t "Adding passengers to ~a trip (~a)...~%"
                 (trip-vehicle target) (trip-sheet-name target))
         (add-passengers target passengers)))
      ((null (find-trips all-trips :day day))
       (format t "No schedule sheet covers ~a yet, so nothing can be booked for that day. Ask the van steward to add that week's tab.~%" day)
       nil)
      ((and dep-time ret-time)
       (create-booking all-trips day destination dep-time ret-time passengers
                       :vehicle vehicle))
      (t
       (error 'booking-incomplete
              :missing (remove nil (list (unless dep-time :dep-time)
                                         (unless ret-time :ret-time)))
              :understood (append (list :destination destination :day day
                                        :passengers passengers)
                                  (when dep-time (list :dep-time dep-time))
                                  (when ret-time (list :ret-time ret-time))
                                  (when vehicle (list :vehicle vehicle)))
              :preamble (format nil "No existing trip to ~a on ~a to join, so this would be a new booking."
                                destination day))))))

(defun parse-passenger-initials (text)
  "Extract uppercase 2-3 character initials from TEXT."
  (cl-ppcre:all-matches-as-strings "[A-Z]{2,3}" text))

(defun book-natural (text &key week)
  "Book from free-form English TEXT, e.g.

  (book-natural \"book a van for swimming next Tuesday for CG and BW at 7 pm to 9 pm\")
  (book-natural \"Can BW and MM take the DGH to the dentist tomorrow, leaving at 8:30 and back around 11?\")
  (book-natural \"add GG to the SHMS trip on Thursday\")

TEXT is parsed by PROJECT-ISIDORE/BOOKING-NLP:PARSE-BOOKING.  Destination, day
and passengers are required; if any is missing, BOOKING-INCOMPLETE is signalled
before the sheet is read, and its report tells the user what was understood and
how to phrase the missing pieces.  Times are optional when an existing trip
matches; otherwise BOOK signals BOOKING-INCOMPLETE for the absent time(s).
The sheet's own destination spellings are used as a gazetteer so \"the SHMS
trip\" joins the existing SHMS trip.  WEEK restricts the search to one sheet."
  (let* ((first-pass (parse-booking text))
         (missing (remove-if-not (lambda (slot) (member slot '(:destination :day :passengers)))
                                 (booking-request-missing first-pass))))
    (when missing
      (error 'booking-incomplete
             :missing missing
             :understood (booking-request-understood first-pass)
             :text text))
    (let* ((trips (fetch-all-trips :week week))
           (req (parse-booking text :known-destinations (known-destinations trips))))
      (book (booking-request-day req)
            (booking-request-destination req)
            (booking-request-passengers req)
            :dep-time (booking-request-dep-time req)
            :ret-time (booking-request-ret-time req)
            :vehicle (booking-request-vehicle req)
            :week week
            :trips trips))))

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
year, or (values nil nil) if the name does not match the expected shape
\(including names whose month word is not a month, e.g. \"Week 1-2\")."
  (cl-ppcre:register-groups-bind (sm sd em ed)
      ("^([A-Za-z]+)\\s+(\\d+)\\s*-\\s*(?:([A-Za-z]+)\\s+)?(\\d+)" sheet-name)
    (let* ((year (year-of (chronicity:parse "today")))
           (start-month (ignore-errors (month-abbrev-num sm)))
           (end-month (if em (ignore-errors (month-abbrev-num em)) start-month))
           (start-day (parse-integer sd))
           (end-day (parse-integer ed)))
      (when (and start-month end-month)
        (values (+ (* year 10000) (* start-month 100) start-day)
                (+ (* year 10000) (* end-month 100) end-day))))))

(defun discover-week-sheets ()
  "Return the (sheet-name gid) tabs of the spreadsheet whose names parse as a
week range (see PARSE-WEEK-RANGE), in tab order.  Helper tabs such as
\"Dates\" or \"Blank Temp\" are dropped so they are never read as schedules."
  (remove-if-not (lambda (tab) (parse-week-range (first tab)))
                 (sheet-tabs)))

(defun week-sheets (&key refresh)
  "Return the list of (sheet-name gid-or-nil) week sheets to search.
When *WEEKS* is non-NIL it is returned as is.  Otherwise the week tabs are
discovered from the spreadsheet (DISCOVER-WEEK-SHEETS) and cached for
*WEEK-SHEETS-CACHE-SECONDS*; REFRESH forces a new fetch."
  (or *weeks*
      (let ((now (get-universal-time))
            (cache *week-sheets-cache*))
        (if (and cache (not refresh)
                 (< (- now (car cache)) *week-sheets-cache-seconds*))
            (cdr cache)
            (let ((sheets (discover-week-sheets)))
              (setf *week-sheets-cache* (cons now sheets))
              sheets)))))

(defun find-week-sheet (target-dt)
  "Return the sheet-name from WEEK-SHEETS whose date range covers TARGET-DT
\(inclusive), or NIL if none match."
  (let ((target-key (current-year-date-key target-dt)))
    (loop for entry in (week-sheets)
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

(defparameter *month-regex*
  (format nil "(?:~{~a~^|~})[a-z]*\\.?"
          (mapcar (lambda (name) (string-downcase (subseq name 0 3))) *month-names*))
  "Regex fragment matching an English month name or abbreviation: \"Sep\",
\"Sept\", \"Sept.\" and \"September\" all match.  Use it case-insensitively.")

(defparameter *day-regex* "\\d{1,2}(?:st|nd|rd|th)?"
  "Regex fragment matching a day of the month: \"5\", \"19\", \"22nd\".")

(defun normalize-dashes (s)
  "Replace Unicode en and em dashes in S with plain hyphens, so a range whose
hyphen a phone keyboard autocorrected into a dash still reads as a range."
  (substitute #\- (code-char 8212) (substitute #\- (code-char 8211) s)))

(defun phrase-month-word (phrase)
  "Return the first month name or abbreviation written in PHRASE, or NIL."
  (cl-ppcre:scan-to-strings (format nil "(?i)\\b~a" *month-regex*) phrase))

(defun carry-month (start end)
  "Return END qualified by START's month when END names nothing but a day, so
the far end of \"Sept 19 - 22\" reads as \"Sept 22\" instead of a bare \"22\"
chronicity would resolve against the wrong month.  END is returned unchanged
otherwise."
  (let ((month (and (cl-ppcre:scan (format nil "^\\s*~a\\s*$" *day-regex*) end)
                    (phrase-month-word start))))
    (if month
        (format nil "~a ~a" month (string-trim " " end))
        end)))

(defun split-compact-range (phrase)
  "Split a date range hyphenated without spaces -- \"Sept 19-22\", \"19-22
Sept\", \"Mar 30-Apr 5\" -- into (values START END), the shape the sheet's own
tab names use.  A month named on one side is carried to the side that omits it,
so both halves come back as a full date.  Returns NIL unless PHRASE is exactly
such a range and names a month: a bare \"8-10\" reads as a pair of times far
more often than as a pair of dates, so it is left alone."
  (cl-ppcre:register-groups-bind (lead-month d1 mid-month d2 trail-month)
      ((format nil "(?i)^\\s*(~a)?\\s*(~a)\\s*-\\s*(~a)?\\s*(~a)\\s*(~a)?\\s*$"
               *month-regex* *day-regex* *month-regex* *day-regex* *month-regex*)
       phrase)
    (let ((start-month (or lead-month trail-month mid-month)))
      (when start-month
        (values (format nil "~a ~a" start-month d1)
                (format nil "~a ~a" (or mid-month start-month) d2))))))

(defun split-range-phrase (phrase)
  "Split PHRASE on ' to ', ' through ', ' thru ', ' until ', ' till ', ' and ',
   ' & ' or ' - ' into (values start end); failing that, on the bare hyphen of a
   compact date range (\"Sept 19-22\", \"Mar 30-Apr 5\") the way the sheet's own
   tab names are written.
   A month named on only one side carries over to the other, so \"Sept 19-22\"
   and \"Sept 19 - 22\" both end on September 22.
   Returns (values phrase phrase) when no separator is found (a single day or
   period such as \"today\" or \"next week\")."
  (let* ((trimmed (normalize-dashes (string-trim " " phrase)))
         (pattern "(?i)\\s+(?:to|through|thru|until|till|and|&|-)\\s+")
         (parts (cl-ppcre:split pattern trimmed :limit 2)))
    (if (= (length parts) 2)
        (let ((start (string-trim " " (first parts)))
              (end (string-trim " " (second parts))))
          (values start (carry-month start end)))
        (multiple-value-bind (start end) (split-compact-range trimmed)
          (if start
              (values start end)
              (values trimmed trimmed))))))

(defun week-phrase-range (phrase)
  "Return (values START END), chronicity datetimes, when PHRASE names a week
or weekend relative to today; NIL otherwise.  Weeks run Monday to Sunday like
the sheet's tabs, which chronicity's own week spans do not.
  \"this week\", \"the week\", \"the whole week\", \"all week\", \"week\"
                                        -> this Monday to this Sunday
  \"next week\", \"the coming week\", \"all of next week\"
                                        -> next Monday to next Sunday
  \"last week\", \"the previous week\"  -> last Monday to last Sunday
  \"the rest of the week\"              -> today to this Sunday
  \"the weekend\", \"this weekend\", \"next weekend\"
                                        -> that week's Saturday and Sunday
Filler (\"the\", \"all\", \"of\", \"whole\", \"entire\", \"full\", a trailing
\"long\") is ignored, so \"the whole of next week\" and \"all week long\" read
as expected."
  (let* ((words (cl-ppcre:split "\\s+" (string-downcase (string-trim " " phrase))))
         (rest-p (member "rest" words :test #'string=))
         (words (remove-if (lambda (w)
                             (member w '("the" "of" "all" "whole" "entire" "full"
                                         "long" "rest")
                                     :test #'string=))
                           words))
         (noun (car (last words)))
         (which (if (= (length words) 2) (first words) "this")))
    (when (and (<= 1 (length words) 2)
               (member noun '("week" "weekend") :test #'string=)
               (member which '("this" "next" "coming" "upcoming" "following"
                               "last" "previous" "past")
                       :test #'string=))
      (let* ((today (chronicity:parse "today"))
             ;; local-time counts Sunday as day 0
             (since-monday (mod (+ (local-time:timestamp-day-of-week today) 6) 7))
             (offset (cond ((member which '("next" "coming" "upcoming" "following")
                                    :test #'string=)
                            7)
                           ((member which '("last" "previous" "past") :test #'string=)
                            -7)
                           (t 0)))
             (monday (local-time:timestamp+ today (- offset since-monday) :day))
             (sunday (local-time:timestamp+ monday 6 :day))
             (start (if (string= noun "weekend")
                        (local-time:timestamp+ monday 5 :day)
                        monday))
             ;; "the rest of the week" starts today rather than Monday
             (start (if (and rest-p (zerop offset) (local-time:timestamp< start today))
                        today
                        start)))
        (values start sunday)))))

(defun phrase-date-range (phrase)
  "Resolve a date PHRASE to (values START END), chronicity datetimes on the
first and last day it covers.  \"today\", \"tomorrow\", \"Apr 20\" and
\"Saturday\" cover one day; \"this week\", \"the whole week\", \"next week\"
and the like cover the sheet's Monday-to-Sunday week, \"the rest of the week\"
today to Sunday and \"the weekend\" Saturday to Sunday (see WEEK-PHRASE-RANGE,
which takes precedence over chronicity).  A month-day without a year
(\"Apr 20\", \"20th of April\") means this year's date, since the sheet only
ever covers the current year, not the next occurrence chronicity's :FUTURE
context would pick once the day has passed.  Returns NIL when chronicity
cannot read PHRASE."
  (multiple-value-bind (monday sunday) (week-phrase-range phrase)
    (if monday
        (values monday sunday)
        (let* ((month *month-regex*)
               (day *day-regex*)
               (anchored
                 (if (and (not (cl-ppcre:scan "\\d{4}" phrase))
                          (or (cl-ppcre:scan (format nil "(?i)\\b~a\\s+~a\\b" month day) phrase)
                              (cl-ppcre:scan (format nil "(?i)\\b~a\\s+(?:of\\s+)?~a" day month) phrase)))
                     (format nil "~a ~d" phrase (year-of (chronicity:parse "today")))
                     phrase))
               (span (chronicity:parse anchored :context :future :guess nil)))
          (when span
            (let* ((start (chronicity:span-start span))
                   ;; a span ends at the following midnight, so step back
                   ;; into the last day it covers
                   (end (local-time:timestamp- (chronicity:span-end span) 1 :sec)))
              (values start (if (local-time:timestamp< end start) start end))))))))

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
  "Return the sheet names from WEEK-SHEETS whose date range overlaps [START-KEY, END-KEY]."
  (loop for entry in (week-sheets)
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

(defparameter *column-width-caps*
  '(("Destination" . 24) ("Dep" . 12) ("Ret" . 12))
  "Widest each named column of FORMAT-SCHEDULE-TABLE grows.  A column sizes
itself to its longest cell up to its cap; longer cells are shortened with
\"...\" (see TRUNCATE-CELL) so one long note on the sheet, such as \"SHMS &
Appt approved (Not returning from SHMS)\" or \"after reception (around 2:30
pm)\", does not widen every row past what Slack shows without wrapping inside
a code block.  Times such as \"12:00 pm\" fit the Dep and Ret caps.")

(defun format-schedule-table (trips)
  "Render TRIPS as a plaintext fixed-width table grouped by day. Returns the
   full string.  The column titles (Vehicle, Destination, Dep, Ret, Dep Pax,
   Ret Pax) and their rule come first; then each day is a flush-left heading
   such as \"Tue Sep 8\" over its indented rows, with a blank line between
   days, so the eye finds a day without reading a date on every row.  TRIPS
   must already be ordered by day.  Columns size themselves to their content
   up to *COLUMN-WIDTH-CAPS*, and lines carry no trailing spaces."
  (let* ((headers '("Vehicle" "Destination" "Dep" "Ret" "Dep Pax" "Ret Pax"))
         (rows (mapcar
                (lambda (tr)
                  (list (string-upcase (symbol-name (trip-vehicle tr)))
                        (or (trip-destination tr) "")
                        (or (trip-departure tr) "")
                        (or (trip-return-time tr) "")
                        (format nil "~{~a~^, ~}" (trip-dep-passengers tr))
                        (format nil "~{~a~^, ~}" (trip-ret-passengers tr))))
                trips))
         (widths
           (loop for header in headers
                 for i from 0
                 for content-width = (max (length header)
                                          (reduce #'max rows
                                                  :key (lambda (r) (length (nth i r)))
                                                  :initial-value 0))
                 for cap = (cdr (assoc header *column-width-caps* :test #'string=))
                 collect (if cap (min content-width cap) content-width)))
         (out (make-string-output-stream)))
    (labels ((emit-cells (cells)
               (let ((line (with-output-to-string (s)
                             (write-string "  " s)
                             (loop for cell in cells
                                   for w in widths
                                   for i from 0
                                   do (format s "~vA" w (truncate-cell cell w))
                                      (when (< i (1- (length widths)))
                                        (write-string "  " s))))))
                 (format out "~a~%" (string-right-trim " " line)))))
      (emit-cells headers)
      (emit-cells (mapcar (lambda (w) (make-string w :initial-element #\-)) widths))
      (let ((previous-day nil))
        (loop for tr in trips
              for cells in rows
              do (unless (equal (trip-day tr) previous-day)
                   (when previous-day (terpri out))
                   (format out "~a~%" (format-short-date (trip-day tr)))
                   (setf previous-day (trip-day tr)))
                 (emit-cells cells))))
    (get-output-stream-string out)))

(defun view-schedule (&optional (range-phrase "today"))
  "Display booked trips over a natural-language date range as a plaintext table.
   RANGE-PHRASE may be a single day or period (\"today\", \"tomorrow\", \"Apr 20\",
   \"this week\", \"the whole week\", \"the rest of the week\", \"next week\",
   \"the weekend\") for that window, or two of them joined by 'to',
   'through', 'and', '&' or '-' (\"today to next Friday\", \"this week and next
   week\", \"Apr 20 through Apr 26\") for the window spanning both.  A range may
   also be written the compact way the sheet's tabs are (\"Sept 19-22\",
   \"Mar 30-Apr 5\").  Defaults to \"today\".  Returns the rendered string and
   also prints it."
  (multiple-value-bind (start-phrase end-phrase) (split-range-phrase range-phrase)
    (multiple-value-bind (s1 e1) (phrase-date-range start-phrase)
      (multiple-value-bind (s2 e2) (phrase-date-range end-phrase)
        (unless (and s1 s2)
          (error "Could not parse date range: ~a" range-phrase))
        (let* ((start-dt (if (<= (current-year-date-key s1) (current-year-date-key s2)) s1 s2))
               (end-dt (if (>= (current-year-date-key e1) (current-year-date-key e2)) e1 e2))
               (start-key (current-year-date-key start-dt))
               (end-key (current-year-date-key end-dt))
               (start-label (format-booking-day start-dt))
               (end-label (format-booking-day end-dt))
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
                out)))))))

(defun date-range-phrase-p (phrase)
  "True when PHRASE is a date range VIEW-SCHEDULE can read: a single day or
period (\"today\", \"the weekend\", \"Sept 12\") or two of them joined the way
SPLIT-RANGE-PHRASE splits them (\"Sept 19 to Sept 22\").  A phrase chronicity
cannot read, or that signals while being read, gives NIL rather than an
error, so a caller can screen a range that came from somewhere less
trustworthy than the user (the booking-llm fallback) before VIEW-SCHEDULE
would error on it."
  (and (stringp phrase)
       (plusp (length (string-trim " " phrase)))
       (ignore-errors
        (multiple-value-bind (start-phrase end-phrase) (split-range-phrase phrase)
          (and (phrase-date-range start-phrase)
               (phrase-date-range end-phrase)
               t)))))

(defun decode-js-string (s)
  "Decode the escapes Google uses inside the JavaScript string literal S:
\\xNN and \\uNNNN hex escapes and single-character escapes such as \\/ or \\\"."
  (with-output-to-string (out)
    (let ((i 0) (n (length s)))
      (loop while (< i n)
            do (let ((c (char s i)))
                 (cond
                   ((and (char= c #\\) (< (1+ i) n))
                    (let ((e (char s (1+ i))))
                      (flet ((hex-char (digits)
                               (let ((end (+ i 2 digits)))
                                 (when (<= end n)
                                   (let ((code (parse-integer s :start (+ i 2) :end end
                                                                :radix 16 :junk-allowed t)))
                                     (when code
                                       (write-char (code-char code) out)
                                       (setf i end)
                                       t))))))
                        (cond
                          ((and (char-equal e #\x) (hex-char 2)))
                          ((and (char-equal e #\u) (hex-char 4)))
                          (t (write-char e out) (incf i 2))))))
                   (t (write-char c out) (incf i))))))))

(defun sheet-tabs ()
  "Return the spreadsheet's tabs as a list of (sheet-name gid) strings, in
tab order.  There is no unauthenticated API for this, so the public
\"htmlview\" page is fetched and the tab list embedded in its script
\(entries like {name: \"Aug 31-Sep 6\", pageUrl: ..., gid: \"1696388518\"})
is extracted.  Signals an error if no tabs can be found, since a silently
empty list would look like a spreadsheet with no weeks."
  (let* ((url (format nil "https://docs.google.com/spreadsheets/d/~a/htmlview"
                      *spreadsheet-id*))
         (html (dex:get url))
         (tabs '()))
    (cl-ppcre:do-register-groups (name gid)
        ("\\{\\s*name:\\s*\"((?:[^\"\\\\]|\\\\.)*)\"[^{}]*?gid:\\s*\"(\\d+)\"" html)
      (push (list (decode-js-string name) gid) tabs))
    (when (null tabs)
      (error "Could not find any sheet tabs in the HTML view of spreadsheet ~a; ~
              Google may have changed the page format."
             *spreadsheet-id*))
    (nreverse tabs)))

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
