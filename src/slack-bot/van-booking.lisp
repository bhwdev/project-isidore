;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/slack-bot/van-booking
  (:use #:common-lisp)
  (:import-from #:alexandria)
  (:import-from #:cl-ppcre)
  (:import-from #:com.inuoe.jzon)
  (:import-from #:dexador)
  (:import-from #:quri)
  (:import-from #:local-time)
  (:import-from #:bhw/slack-bot/booking-nlp
                #:parse-booking #:revise-booking #:possible-initials
                #:booking-request-destination #:booking-request-day
                #:booking-request-passengers #:booking-request-dep-time
                #:booking-request-ret-time #:booking-request-vehicle
                #:booking-request-missing #:booking-request-understood
                #:booking-incomplete
                #:today #:monday-index #:parse-day #:parse-clock #:month-number
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
   #:read-sheet #:write-range
   #:fetch-all-trips #:known-destinations
   #:sheet-tabs #:week-sheets
   #:book-natural
   ;; proposals: a booking decided, shown, then written
   #:proposal #:make-proposal #:proposal-p
   #:proposal-action #:proposal-day #:proposal-trip #:proposal-trips
   #:proposal-passengers
   #:proposal-request
   #:propose-booking #:propose-natural #:revise-proposal #:commit-booking
   #:proposal-day-view #:proposal-trip-view #:proposal-clash-checked-p
   #:remind-driver
   #:remind-departure
   #:view-schedule
   #:date-range-phrase-p
   #:trips-for-passenger-today
   #:trip-departure-universal-time
   #:remind-return-driver
   #:trips-for-return-driver-today
   #:trip-return-universal-time)
  (:documentation
   "
  - parse-schedule — reads the full sheet and extracts its trip structs with
  their sheet row numbers
  - find-trips — filters by day and/or destination (substring, case-insensitive)
  - sheet-tabs / week-sheets — list the spreadsheet's tabs and the subset whose
  names are week ranges such as \"Aug 31-Sep 6\"; the week list is cached
  briefly and can be pinned by binding *weeks*
  - read-sheet / write-range — the spreadsheet itself, through the Apps Script
  web app in van-booking.gs: the only way this code touches it
  - fetch-all-trips — reads and parses every week sheet (or one week)
  - book — top-level entry point:
    - Existing trip found → adds passengers to both dep and ret columns
    - No match + times provided → creates new booking with vehicle priority DGH → LGH →
  Chevy
    - No match + no times → signals BOOKING-INCOMPLETE naming the absent time(s)
    - No week sheet covers the day → informs user (before asking for times)
    - No empty slots → informs user
    - Conflicts → informs user and writes nothing (see below)
  - conflict checks, run by BOOK before every write and again on a fresh read
  of the sheet immediately before the first cell is written:
    - the van is already out that day over an overlapping window — passed over
  when the vehicle was chosen automatically, refused when it was named
    - a passenger is already on an overlapping trip that day, in any vehicle
    - the slot was booked by somebody else since the schedule was read
  Times are half-open, so a van due back at 11:00 can go out again at 11:00,
  and a booked trip whose own times cannot be read blocks unconditionally
  - book-natural — free-form English frontend to book (see bhw/slack-bot/booking-nlp).
    Signals BOOKING-INCOMPLETE, whose report asks the user to re-send the request
    with the missing pieces, when destination, day or passengers cannot be found.
  - propose-booking / propose-natural — book and book-natural up to the write:
    they return a PROPOSAL, shown with proposal-day-view (the day as booked
    now) and proposal-trip-view (the trip as it will read), which
    commit-booking writes after re-reading the sheet and checking again;
    revise-proposal applies an edit (\"8 to 10 pm\", \"take the LGH\")
  - view-schedule — list the booked trips of a natural-language date range, day by day"))

(in-package #:bhw/slack-bot/van-booking)

(defparameter *spreadsheet-id* "1qxx5mDsrqAqGBrwLeSs0V6R_70FSNm1-ruFWxvHVgu8")

(defparameter *weeks* nil
  "Optional fixed list of week sheet names.  NIL, the default, means
WEEK-SHEETS discovers the current week tabs from the spreadsheet itself, so
the code keeps working as new weeks are added.  Bind or set this to a fixed
list (e.g. '(\"Apr 20-26\")) to bypass discovery; the tests do that.")

(defparameter *week-sheets-cache-seconds* 300
  "How long, in seconds, a discovered week-sheet list is reused by WEEK-SHEETS
before the spreadsheet's tab list is fetched again.")

(defvar *week-sheets-cache* nil
  "(fetched-universal-time . week-sheets) from the last discovery, or NIL.")

(defparameter *apps-script-url*
  "https://script.google.com/macros/s/AKfycbyR4Ffk5mjAUPUXOwQLIfjBiviyvm2rnNLpbBQvXAfTBNdAgEfR5KfE-45pPd2eTSz98g/exec"
  "The web app deployment of van-booking.gs, which reads and writes the
spreadsheet on this code's behalf.")

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
  (and (stringp s) (plusp (length (trim-cell s)))))

(defun safe-nth (n list)
  "Like NTH but returns \"\" if out of bounds."
  (or (nth n list) ""))

(defun trim-cell (cell)
  "CELL without the spaces and tabs around its text."
  (string-trim '(#\Space #\Tab) cell))

(defun row-cell (row col)
  "The trimmed text of ROW's cell in column COL (0-indexed), \"\" when ROW
is that short."
  (trim-cell (safe-nth col row)))

(defun extract-day-from-header (row)
  "Extract day string from the merged header row (col A has repeated day text).
   Returns e.g. \"Monday March 30\" or NIL."
  (let ((cell (row-cell row 0)))
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
holds times: in a read that has lost the block's empty rows (the gviz CSV
dropped them), the row found there for a block with no times is that weekday
reminder, and reading it as data made every unbooked block look like a
Personal trip departing at \"Saturday\"."
  (every (lambda (cell)
           (let ((c (trim-cell cell)))
             (or (not (non-blank-p c)) (header-cell-p c))))
         (nthcdr 2 row)))

(defun collect-passengers (rows start-offset col max-rows)
  "Collect non-blank passenger initials from column COL starting at START-OFFSET in ROWS.
   Stops at empty cells, block headers, or after MAX-ROWS."
  (loop for i from start-offset below (min (+ start-offset max-rows) (length rows))
        for cell = (row-cell (nth i rows) col)
        until (or (not (non-blank-p cell)) (header-cell-p cell))
        collect cell))

(defun filter-new-passengers (passengers existing)
  "Return elements of PASSENGERS not already in EXISTING (string-equal comparison)."
  (remove-if (lambda (p) (member p existing :test #'string-equal)) passengers))

(defun trip-passengers (trip)
  "Everyone written on TRIP, out or back; someone on both legs is listed twice."
  (append (trip-dep-passengers trip) (trip-ret-passengers trip)))

(defun parse-schedule (rows &key sheet-name)
  "Parse spreadsheet ROWS into a list of TRIP structs.
   SHEET-NAME is stored on each trip for write-back.
   A block's start row is the sheet row its header (\"Chevy\" in column C) was
   found on, so ROWS must be the whole tab with its empty rows in place,
   which is what READ-SHEET returns.  Blocks are not assumed to sit a fixed
   number of rows apart: a row inserted by hand into one block moves every
   block below it, and a booking placed by counting blocks then landed that
   many rows too high, over the \"Dep./Ret.\" labels."
  (let ((trips '())
        (current-day nil))
    (loop for row in rows
          for i from 0
          when (and (>= (length row) 3)
                    (string-equal (row-cell row 2) "Chevy"))
            do (setf current-day (or (extract-day-from-header row) current-day))
               ;; The row after the header is the destination row, unless
               ;; that row is missing and "Dep./Ret." follows directly.
               (let* ((next-row (nth (1+ i) rows))
                      (dest-row (and next-row
                                     (not (string-equal "Dep." (row-cell next-row 2)))
                                     next-row))
                      (time-offset (if dest-row 3 2))
                      (pass-offset (1+ time-offset)))
                 (when (and current-day (< (+ i time-offset) (length rows)))
                   (let* ((time-row (nth (+ i time-offset) rows))
                          ;; With no times the row is blank, or, in a read
                          ;; that has lost the block's empty rows, it is the
                          ;; next block header, a "Dep./Ret." row, or the
                          ;; weekday reminder in column L.  LABEL-ROW-P
                          ;; covers all four.
                          (time-row-empty (label-row-p time-row)))
                     (flet ((cell (row col)
                              (if row (row-cell row col) "")))
                       (loop for (vehicle dep-col ret-col) in *vehicles*
                             do (push (make-trip
                                       :day current-day
                                       :vehicle vehicle
                                       :destination (cell dest-row dep-col)
                                       :departure (cell (unless time-row-empty time-row) dep-col)
                                       :return-time (cell (unless time-row-empty time-row) ret-col)
                                       :dep-passengers (collect-passengers rows (+ i pass-offset) dep-col 10)
                                       :ret-passengers (collect-passengers rows (+ i pass-offset) ret-col 10)
                                       :block-start-row (1+ i)
                                       :dep-col dep-col
                                       :ret-col ret-col
                                       :sheet-name sheet-name)
                                      trips)))))))
    (nreverse trips)))

(defun find-trips (trips &key day destination)
  "Return trips matching DAY and/or DESTINATION (case-insensitive substrings).
   Both filters must match when provided. A blank DESTINATION filter matches no trips,
   so callers must use NIL (not \"\") to mean \"any destination\".
   DAY may not stop partway through a number: \"Sunday November 1\" finds
   \"Sunday November 1\" but not \"Sunday November 15\", two weeks on, whose
   tab FETCH-ALL-TRIPS reads in the same search.  A plain substring did, and
   so a booking for the 1st could join, clash with or be written into the
   15th."
  (let ((day-scanner (and day (cl-ppcre:create-scanner
                               (format nil "~a(?!\\d)" (cl-ppcre:quote-meta-chars day))
                               :case-insensitive-mode t))))
    (remove-if-not
     (lambda (tr)
       (and (or (null day-scanner)
                (cl-ppcre:scan day-scanner (or (trip-day tr) "")))
            (or (null destination)
                (and (non-blank-p destination)
                     (non-blank-p (trip-destination tr))
                     (search (string-upcase destination)
                             (string-upcase (trip-destination tr)))))))
     trips)))

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

;;; --- Conflict detection ---
;;;
;;; A slot on the sheet is one vehicle on one day, so the only way a van is
;;; booked twice in a day is a second block for that day.  PARSE-SCHEDULE
;;; supports that (a block header whose column A is blank carries the previous
;;; day forward), and FIND-EMPTY-SLOT would otherwise hand out the blank slot
;;; in the second block while the van is on the road for the first.  These
;;; functions are what stops that, and what stops somebody being written onto
;;; two trips that run at the same time.

(defun request-window (dep-time ret-time)
  "Return (values START END), minutes since midnight, for a trip leaving at
DEP-TIME and back at RET-TIME, or NIL when either is blank, unreadable, or
the two do not run forwards -- a return before its departure cannot be read
as a window, and a trip carrying on past midnight is one such pair.  An
unreadable time must not be taken for midnight, as DEPARTURE-MINUTES takes it."
  (let ((start (parse-clock dep-time))
        (end (parse-clock ret-time)))
    (when (and start end (< start end))
      (values start end))))

(defun trip-window (trip)
  "Return (values START END), minutes since midnight, for TRIP's departure and
return times, or NIL when they cannot be read as a window (REQUEST-WINDOW)."
  (request-window (trip-departure trip) (trip-return-time trip)))

(defun windows-overlap-p (start-a end-a start-b end-b)
  "True when [START-A, END-A) and [START-B, END-B) share a minute.  The
intervals are half-open on purpose: a van due back at 11:00 may be taken out
again at 11:00."
  (and (< start-a end-b) (< start-b end-a)))

(defun trip-blocks-window-p (trip start end)
  "True when the booked TRIP stands in the way of a trip running START to END
on the same day.  A booked trip whose own times cannot be read blocks
unconditionally: nothing can show it is out of the way, and writing over a van
that may already be on the road is the worse mistake."
  (and (booked-trip-p trip)
       (multiple-value-bind (trip-start trip-end) (trip-window trip)
         (or (null trip-start)
             (windows-overlap-p start end trip-start trip-end)))))

(defun vehicle-conflicts (trips day vehicle start end &key exclude)
  "The booked TRIPS that already have VEHICLE out on DAY over a window
overlapping START to END.  EXCLUDE is the slot about to be written, which
never conflicts with itself."
  (remove-if-not (lambda (tr)
                   (and (eq (trip-vehicle tr) vehicle)
                        (not (eq tr exclude))
                        (trip-blocks-window-p tr start end)))
                 (find-trips trips :day day)))

(defun passenger-conflicts (trips day passengers start end &key exclude)
  "An alist of (PASSENGER . TRIP) naming each of PASSENGERS already written
onto a booked trip on DAY that overlaps START to END, whatever vehicle that
trip uses.  EXCLUDE is the trip being booked or joined, which is not a
conflict with itself.  Someone on two overlapping trips is reported once,
against the first."
  (let ((candidates (remove-if-not (lambda (tr)
                                     (and (not (eq tr exclude))
                                          (trip-blocks-window-p tr start end)))
                                   (find-trips trips :day day))))
    (loop for p in passengers
          for clash = (find-if (lambda (tr)
                                 (member p (trip-passengers tr) :test #'string-equal))
                               candidates)
          when clash collect (cons p clash))))

(defun format-vehicle-conflicts (vehicle day conflicts)
  "The report for CONFLICTS, the trips that already have VEHICLE out on DAY."
  (with-output-to-string (s)
    (format s "The ~a is already out on ~a over those times, so nothing was booked:~%"
            vehicle day)
    (dolist (tr conflicts) (print-trip tr s))))

(defun format-passenger-conflicts (day conflicts)
  "The report for CONFLICTS, an alist of (passenger . trip) from
PASSENGER-CONFLICTS.  Nobody is booked when one person clashes: half a request
written to a shared sheet is harder to make sense of than a refused one, and
the reply names who has to be left out."
  (with-output-to-string (s)
    (format s "~{~a~^, ~} ~:[are~;is~] already booked on ~a over those times, so nothing was booked:~%"
            (mapcar #'car conflicts) (null (rest conflicts)) day)
    (dolist (tr (remove-duplicates (mapcar #'cdr conflicts) :test #'eq :from-end t))
      (print-trip tr s))))

(defun booking-obstacle (trips day vehicle passengers start end &key exclude)
  "The reason a booking of VEHICLE on DAY for PASSENGERS running START to END
cannot go ahead against TRIPS, as a report ready to print, or NIL when nothing
stands in its way.  EXCLUDE is the slot being written.  A NIL START or END
means the request carries no readable window, so no overlap can be judged and
none is claimed."
  (when (and start end)
    (let ((veh (vehicle-conflicts trips day vehicle start end :exclude exclude)))
      (if veh
          (format-vehicle-conflicts vehicle day veh)
          (let ((pax (passenger-conflicts trips day passengers start end
                                          :exclude exclude)))
            (when pax (format-passenger-conflicts day pax)))))))

(defun refresh-trip (trip)
  "Re-read TRIP's own week sheet and return (values FRESH-TRIP FRESH-TRIPS):
that same slot as the sheet now has it -- matched by block start row and
departure column -- and every trip on the sheet.  Returns (values TRIP NIL)
when TRIP carries no sheet name, since there is then nothing to re-read, and
(values NIL FRESH-TRIPS) when the block has gone.

Every booking is decided against a snapshot taken before the request was
parsed (FETCH-ALL-TRIPS, or the caller's own :TRIPS), and two Slack messages
seconds apart both work from that one snapshot.  Everything that writes goes
through here first, so the second request sees what the first one booked."
  (let ((sheet (trip-sheet-name trip)))
    (if (null sheet)
        (values trip nil)
        (let ((fresh (sheet-trips sheet)))
          (values (find-if (lambda (tr)
                             (and (eql (trip-block-start-row tr)
                                       (trip-block-start-row trip))
                                  (eql (trip-dep-col tr) (trip-dep-col trip))))
                           fresh)
                  fresh)))))

(defparameter *destination-row-offset* 1
  "How many rows below its header row a block's destinations are written.")

(defparameter *time-row-offset* 3
  "How many rows below its header row a block's times are written.")

(defparameter *passenger-row-offset* 4
  "How many rows below its header row a block's first passengers are written.")

(defun write-cell (trip col row-offset value)
  "Write VALUE to the cell of TRIP's sheet in column COL (0-indexed),
ROW-OFFSET rows below TRIP's block header."
  (write-range (format nil "~a~a" (col-letter col) (+ (trip-block-start-row trip) row-offset))
               (list (list value))
               :sheet-name (trip-sheet-name trip)))

(defun write-column (trip col row-offset values)
  "Write VALUES, a list of cell values, down column COL (0-indexed) of TRIP's
sheet, from ROW-OFFSET rows below TRIP's block header.  Writes nothing when
VALUES is empty."
  (when values
    (let ((letter (col-letter col))
          (first-row (+ (trip-block-start-row trip) row-offset)))
      (write-range (format nil "~a~a:~a~a" letter first-row
                           letter (+ first-row (length values) -1))
                   (mapcar #'list values)
                   :sheet-name (trip-sheet-name trip)))))

(defun write-passengers (trip passengers)
  "Write PASSENGERS (list of initials) into TRIP's dep and ret columns.
   Passengers already present in a column are skipped for that column.
   Writes to the spreadsheet using the trip's sheet-name.
   TRIP must be current: the rows written are counted from the passengers it
   already carries, so a stale TRIP writes over whoever was added since.
   ADD-PASSENGERS is the entry point that guarantees that."
  (let ((new-dep (filter-new-passengers passengers (trip-dep-passengers trip)))
        (new-ret (filter-new-passengers passengers (trip-ret-passengers trip))))
    (write-column trip (trip-dep-col trip)
                  (+ *passenger-row-offset* (length (trip-dep-passengers trip)))
                  new-dep)
    (write-column trip (trip-ret-col trip)
                  (+ *passenger-row-offset* (length (trip-ret-passengers trip)))
                  new-ret)
    (let* ((added (remove-duplicates (append new-dep new-ret) :test #'string-equal))
           (skipped (set-difference passengers added :test #'string-equal)))
      (when added
        (format t "Added ~{~a~^, ~} to ~a ~a trip (~a)~%"
                added (trip-vehicle trip) (trip-destination trip) (trip-day trip)))
      (when skipped
        (format t "Skipped (already booked): ~{~a~^, ~}~%" skipped)))
    trip))

(defun add-passengers (trip passengers &key (verify t))
  "Add PASSENGERS (list of initials) to TRIP in both dep and ret columns, and
return the trip written to, or NIL when nothing was written.

Unless VERIFY is NIL, TRIP's sheet is re-read first (REFRESH-TRIP) and the
write goes to the slot as it now stands: TRIP may have been parsed minutes
ago, and appending after a stale passenger count writes over whoever was added
since.  Anyone already on another trip that day whose times overlap this one
is refused, and then nobody is added at all (see FORMAT-PASSENGER-CONFLICTS).
VERIFY NIL writes against TRIP as given, for a caller that has just read it."
  (multiple-value-bind (current fresh-trips)
      (if verify (refresh-trip trip) (values trip nil))
    (cond
      ((null current)
       (format t "The ~a block for ~a is no longer on the ~a sheet, so nobody was added.~%"
               (trip-vehicle trip) (trip-day trip) (trip-sheet-name trip))
       nil)
      (t
       (multiple-value-bind (start end) (trip-window current)
         (let ((clashes (when (and start end fresh-trips)
                          (passenger-conflicts fresh-trips (trip-day current)
                                               passengers start end
                                               :exclude current))))
           (cond
             (clashes (princ (format-passenger-conflicts (trip-day current) clashes))
                      nil)
             (t (write-passengers current passengers)))))))))

(defun find-empty-slot (trips day &optional preferred-vehicle start end)
  "Find an empty vehicle slot for DAY. If PREFERRED-VEHICLE given, try that first.
   Otherwise try *auto-vehicles* in priority order.
   When START and END (minutes since midnight) are both given, a vehicle
   already out on DAY over an overlapping window is passed over even though
   the slot in this block is blank: the sheet can carry more than one block
   for a day, and the van cannot be in two places at once.
   Returns the empty trip struct or NIL."
  (let ((day-trips (find-trips trips :day day))
        (try-order (if preferred-vehicle
                       (list preferred-vehicle)
                       *auto-vehicles*)))
    (some (lambda (veh)
            (unless (and start end
                         (vehicle-conflicts day-trips day veh start end))
              (find-if (lambda (tr)
                         (and (eq (trip-vehicle tr) veh)
                              (not (non-blank-p (trip-destination tr)))
                              (not (non-blank-p (trip-departure tr)))))
                       day-trips)))
          try-order)))

(defun report-no-slot (trips day vehicle start end)
  "Say why CREATE-BOOKING found nowhere to write on DAY.  A vehicle that is
merely out over the requested window is a different answer from a day whose
slots are all taken, and only the first tells the asker that another time
would work."
  (let ((clashes (when (and start end)
                   (loop for veh in (if vehicle (list vehicle) *auto-vehicles*)
                         append (vehicle-conflicts trips day veh start end)))))
    (if clashes
        (progn
          (format t "No vehicle is free on ~a between those times~@[ (tried ~a only)~]. Already out:~%"
                  day vehicle)
          (dolist (tr clashes) (print-trip tr)))
        (format t "No available vehicle slots for ~a~@[ (tried ~a only)~].~%" day vehicle))))

(defun choose-slot (trips day passengers dep-time ret-time &key vehicle)
  "The blank slot on DAY that a new booking of PASSENGERS leaving at DEP-TIME
and back at RET-TIME would be written to, judged against TRIPS, or NIL,
having printed why, when there is none.  VEHICLE is as for CREATE-BOOKING,
which describes the checks."
  (multiple-value-bind (start end) (request-window dep-time ret-time)
    (let ((slot (find-empty-slot trips day vehicle start end)))
      (if (null slot)
          (progn (report-no-slot trips day vehicle start end) nil)
          (let ((obstacle (booking-obstacle trips day (trip-vehicle slot) passengers
                                            start end :exclude slot)))
            (if obstacle
                (progn (princ obstacle) nil)
                slot))))))

(defun write-booking (slot day destination dep-time ret-time passengers &key (verify t))
  "Write a new booking into SLOT, the blank slot CHOOSE-SLOT picked on DAY,
and return the slot written, or NIL, having printed why, when nothing was.
Unless VERIFY is NIL, SLOT's sheet is re-read immediately before the first
write (REFRESH-TRIP) and CHOOSE-SLOT's checks run again against it, so a
booking decided on a stale snapshot cannot write over one made since."
  (when verify
    (multiple-value-bind (start end) (request-window dep-time ret-time)
      (multiple-value-bind (current fresh-trips) (refresh-trip slot)
        (cond
          ((null current)
           (format t "The ~a block for ~a is no longer on the ~a sheet, so nothing was booked.~%"
                   (trip-vehicle slot) day (trip-sheet-name slot))
           (return-from write-booking nil))
          ((booked-trip-p current)
           (format t "The ~a was booked for ~a while this request was being handled, so nothing was written:~%"
                   (trip-vehicle current) day)
           (print-trip current)
           (return-from write-booking nil))
          (fresh-trips
           (let ((obstacle (booking-obstacle fresh-trips day (trip-vehicle current)
                                             passengers start end :exclude current)))
             (when obstacle
               (princ obstacle)
               (return-from write-booking nil)))))
        (setf slot current))))
  (let ((dep-col (trip-dep-col slot))
        (ret-col (trip-ret-col slot)))
    (write-cell slot dep-col *destination-row-offset* destination)
    (write-cell slot dep-col *time-row-offset* dep-time)
    (write-cell slot ret-col *time-row-offset* ret-time)
    (write-column slot dep-col *passenger-row-offset* passengers)
    (write-column slot ret-col *passenger-row-offset* passengers))
  (format t "Created ~a booking on ~a → ~a (~a - ~a) for ~{~a~^, ~}~%"
          (trip-vehicle slot) day destination dep-time ret-time passengers)
  slot)

(defun create-booking (trips day destination dep-time ret-time passengers
                       &key vehicle (verify t))
  "Create a new booking in an empty slot. Auto-selects vehicle by priority if not given.
   Returns the trip or NIL when the booking could not be made.

   A vehicle already out on DAY over a window overlapping DEP-TIME to RET-TIME
   is passed over when the vehicle is chosen automatically and refused when it
   was named (FIND-EMPTY-SLOT, REPORT-NO-SLOT), and a passenger already on an
   overlapping trip that day is refused outright (PASSENGER-CONFLICTS).  Both
   checks are skipped when the times cannot be read as a window, since then
   there is no overlap to judge.  CHOOSE-SLOT makes them against TRIPS.

   Unless VERIFY is NIL, the chosen slot's sheet is re-read immediately before
   the first write and both checks run again against it (WRITE-BOOKING), so a
   booking decided on a stale snapshot cannot write over one made since."
  (let ((slot (choose-slot trips day passengers dep-time ret-time :vehicle vehicle)))
    (when slot
      (write-booking slot day destination dep-time ret-time passengers :verify verify))))

(defun sheet-trips (sheet-name)
  "The trips on the tab SHEET-NAME, read afresh (READ-SHEET, PARSE-SCHEDULE)."
  (parse-schedule (read-sheet sheet-name) :sheet-name sheet-name))

(defun fetch-all-trips (&key week)
  "Read and parse the schedule for every week sheet (see WEEK-SHEETS), or only
   WEEK (a sheet name such as \"Apr 20-26\") when given.  Returns a list of
   TRIP structs."
  (mapcan #'sheet-trips (if week (list week) (week-sheets))))

(defun known-destinations (trips)
  "Return the distinct non-blank destinations found in TRIPS, in the sheet's
   own spelling.  Used as a gazetteer by BOOK-NATURAL."
  (remove-duplicates
   (remove-if-not #'non-blank-p (mapcar #'trip-destination trips))
   :test #'string-equal :from-end t))

(defun known-initials (trips)
  "Return the distinct initials of everyone on TRIPS, out or back, upcased.
Used by BOOK-NATURAL so a request can name them in lowercase."
  (remove-duplicates
   (loop for trip in trips
         append (loop for cell in (trip-passengers trip)
                      when (cl-ppcre:scan "^[A-Za-z]{2,3}$" cell)
                        collect (string-upcase cell)))
   :test #'string= :from-end t))

;;; --- Proposals ---
;;;
;;; A booking is decided and written in two steps, so the bot can show the
;;; asker what it is about to write and wait for a yes: PROPOSE-BOOKING makes
;;; every decision and check BOOK makes and writes nothing, and
;;; COMMIT-BOOKING writes what it decided.  The answer can take minutes, so
;;; COMMIT-BOOKING re-reads the sheet and checks again before the first write.

(defstruct proposal
  "A booking PROPOSE-BOOKING found room for, not yet written.  ACTION is
:JOIN, adding PASSENGERS to the booked trip SLOT, or :CREATE, filling the
blank SLOT with DESTINATION, DEP-TIME, RET-TIME and PASSENGERS, on DAY as the
request named it.  TRIP is SLOT as it will read once written, to show the
asker; TRIPS the schedule the booking was decided against.  REQUEST is the
BOOKING-REQUEST it was read from, when it came from words (PROPOSE-NATURAL),
for REVISE-PROPOSAL to change."
  action day slot trip passengers destination dep-time ret-time trips request)

(defun propose-join (trips target passengers)
  "The PROPOSAL adding PASSENGERS to TARGET, a booked trip among TRIPS, or
NIL, having printed why, when nobody would be added: everyone named is on it
already, or somebody is on another trip that day whose times overlap it (the
check ADD-PASSENGERS makes again before writing)."
  (let ((new-dep (filter-new-passengers passengers (trip-dep-passengers target)))
        (new-ret (filter-new-passengers passengers (trip-ret-passengers target))))
    (if (not (or new-dep new-ret))
        (progn
          (format t "~{~a~^, ~} ~:[are~;is~] already on the ~a trip to ~a on ~a, so there is nobody to add.~%"
                  passengers (null (rest passengers))
                  (trip-vehicle target) (trip-destination target) (trip-day target))
          nil)
        (multiple-value-bind (start end) (trip-window target)
          (let ((clashes (when start
                           (passenger-conflicts trips (trip-day target) passengers
                                                start end :exclude target))))
            (if clashes
                (progn (princ (format-passenger-conflicts (trip-day target) clashes))
                       nil)
                (let ((joined (copy-trip target)))
                  (setf (trip-dep-passengers joined) (append (trip-dep-passengers target) new-dep)
                        (trip-ret-passengers joined) (append (trip-ret-passengers target) new-ret))
                  (make-proposal :action :join :day (trip-day target) :slot target
                                 :trip joined :passengers passengers
                                 :destination (trip-destination target) :trips trips))))))))

(defun propose-create (trips day destination dep-time ret-time passengers &key vehicle)
  "The PROPOSAL for a new booking on DAY (CREATE-BOOKING's arguments), or
NIL, having printed why, when no slot can take it (CHOOSE-SLOT)."
  (let ((slot (choose-slot trips day passengers dep-time ret-time :vehicle vehicle)))
    (when slot
      (let ((filled (copy-trip slot)))
        (setf (trip-destination filled) destination
              (trip-departure filled) dep-time
              (trip-return-time filled) ret-time
              (trip-dep-passengers filled) passengers
              (trip-ret-passengers filled) passengers)
        (make-proposal :action :create :day day :slot slot :trip filled
                       :passengers passengers :destination destination
                       :dep-time dep-time :ret-time ret-time :trips trips)))))

(defun propose-booking (day destination passengers &key dep-time ret-time vehicle week trips)
  "Decide the booking BOOK would make, with BOOK's arguments, and return it
as a PROPOSAL without writing anything; COMMIT-BOOKING writes it.  Returns
NIL, having printed why, when BOOK would write nothing: no sheet covers DAY,
no vehicle is free, a van or a passenger clashes, or everyone named is on
the trip being joined already.  Signals BOOKING-INCOMPLETE as BOOK does."
  (let* ((all-trips (or trips (fetch-all-trips :week week)))
         (matches (find-trips all-trips :day day :destination destination)))
    (cond
      (matches
       (propose-join all-trips (first matches) passengers))
      ((null (find-trips all-trips :day day))
       (format t "No schedule sheet covers ~a yet, so nothing can be booked for that day. Ask the van steward to add that week's tab.~%" day)
       nil)
      ((and dep-time ret-time)
       (propose-create all-trips day destination dep-time ret-time passengers
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

(defun commit-booking (proposal)
  "Write PROPOSAL to the sheet and return the trip written to, or NIL, having
printed why, when nothing was written.  What was written is printed too.
The proposal's sheet is re-read first and PROPOSE-BOOKING's checks are made
again against it (ADD-PASSENGERS, WRITE-BOOKING): an answer can take
minutes, and a booking made meanwhile must not be written over."
  (ecase (proposal-action proposal)
    (:join
     (add-passengers (proposal-slot proposal) (proposal-passengers proposal)))
    (:create
     (write-booking (proposal-slot proposal) (proposal-day proposal)
                    (proposal-destination proposal)
                    (proposal-dep-time proposal) (proposal-ret-time proposal)
                    (proposal-passengers proposal)))))

(defun book (day destination passengers &key dep-time ret-time vehicle week trips)
  "Book PASSENGERS for a trip on DAY to DESTINATION.
   If an existing trip matches, adds passengers to it (no times needed).
   If no week sheet covers DAY (the sheet has no blocks for it), says so and
   returns NIL rather than asking for times or reporting full vehicles.
   Otherwise creates a new booking, which requires DEP-TIME and RET-TIME; when
   either is missing, signals BOOKING-INCOMPLETE so the caller can ask for them.
   VEHICLE can override automatic selection (DGH > LGH > Chevy).
   WEEK is the sheet name (e.g. \"Apr 20-26\"). If omitted, searches all weeks.
   TRIPS, when given, is a pre-fetched list from FETCH-ALL-TRIPS and skips the
   initial read; the sheet being written is re-read before the write either
   way (REFRESH-TRIP).
   Nothing is written over a conflict: a van already out at that time, a
   passenger already on an overlapping trip that day, or a slot booked by
   somebody else since TRIPS was read.  The reason is printed and NIL
   returned, as for a day with no free slots.
   This is PROPOSE-BOOKING followed by COMMIT-BOOKING, for a caller with
   nobody to ask whether the booking is the one they meant."
  (let ((proposal (propose-booking day destination passengers
                                   :dep-time dep-time :ret-time ret-time
                                   :vehicle vehicle :week week :trips trips)))
    (when proposal
      (commit-booking proposal))))

(defun propose-request (req trips)
  "PROPOSE-BOOKING for REQ, a BOOKING-REQUEST, against TRIPS; the proposal
returned keeps REQ."
  (let ((proposal (propose-booking (booking-request-day req)
                                   (booking-request-destination req)
                                   (booking-request-passengers req)
                                   :dep-time (booking-request-dep-time req)
                                   :ret-time (booking-request-ret-time req)
                                   :vehicle (booking-request-vehicle req)
                                   :trips trips)))
    (when proposal
      (setf (proposal-request proposal) req))
    proposal))

(defun readable-initials (trips registered-initials)
  "The initials a request may write in lowercase: REGISTERED-INITIALS and
those of everyone on TRIPS (KNOWN-INITIALS)."
  (union registered-initials (known-initials trips) :test #'string=))

(defun propose-natural (text &key week registered-initials)
  "PROPOSE-BOOKING from free-form English TEXT: the PROPOSAL BOOK-NATURAL
would write, or NIL, having printed why, when it would write nothing.

TEXT is parsed by bhw/slack-bot/BOOKING-NLP:PARSE-BOOKING.  Destination, day
and passengers are required; if any is missing, BOOKING-INCOMPLETE is signalled
and its report tells the user what was understood and how to phrase the
missing pieces.  That happens before the sheet is read, unless TEXT has a
lowercase word that may be someone's initials (POSSIBLE-INITIALS): then the
sheet is read first, so the report does not ask for passengers it was given.
Times are optional when an existing trip matches; otherwise PROPOSE-BOOKING
signals BOOKING-INCOMPLETE for the absent time(s).
The sheet's own destination spellings are used as a gazetteer so \"the SHMS
trip\" joins the existing SHMS trip, and the initials on it, together with
REGISTERED-INITIALS (the bot's `/iam` and `/subscribe` registrations), let
TEXT name passengers in lowercase: \"bw and mm\".  WEEK restricts the search
to one sheet."
  (let ((req (parse-booking text :known-initials registered-initials))
        (trips nil))
    (flet ((incomplete ()
             (remove-if-not (lambda (slot) (member slot '(:destination :day :passengers)))
                            (booking-request-missing req)))
           (read-sheet-and-reparse ()
             (setf trips (fetch-all-trips :week week)
                   req (parse-booking text
                                      :known-destinations (known-destinations trips)
                                      :known-initials (readable-initials
                                                       trips registered-initials)))))
      (when (and (incomplete) (possible-initials text))
        (read-sheet-and-reparse))
      (when (incomplete)
        (error 'booking-incomplete
               :missing (incomplete)
               :understood (booking-request-understood req)
               :text text))
      (unless trips
        (read-sheet-and-reparse))
      (propose-request req trips))))

(defun revise-proposal (proposal text &key registered-initials)
  "PROPOSAL, which PROPOSE-NATURAL made, with the changes TEXT asks for (the
words after `edit`, read by REVISE-BOOKING), proposed afresh against a new
read of the sheet.  Returns (values REVISED CHANGED): CHANGED lists the
pieces TEXT named, and REVISED is the new PROPOSAL, or NIL, having printed
why, when the changed booking cannot be made.  When TEXT names no change,
both are NIL and nothing is printed.  Signals BOOKING-INCOMPLETE as
PROPOSE-BOOKING does, when the change leaves a new booking without times.
REGISTERED-INITIALS is as for PROPOSE-NATURAL."
  (let ((trips (fetch-all-trips)))
    (multiple-value-bind (req changed)
        (revise-booking (proposal-request proposal) text
                        :known-destinations (known-destinations trips)
                        :known-initials (readable-initials trips registered-initials))
      (values (and changed (propose-request req trips))
              changed))))

(defun book-natural (text &key week registered-initials)
  "Book from free-form English TEXT, e.g.

  (book-natural \"book a van for swimming next Tuesday for CG and BW at 7 pm to 9 pm\")
  (book-natural \"Can BW and MM take the DGH to the dentist tomorrow, leaving at 8:30 and back around 11?\")
  (book-natural \"add GG to the SHMS trip on Thursday\")

This is PROPOSE-NATURAL, which describes how TEXT is read, followed by
COMMIT-BOOKING."
  (let ((proposal (propose-natural text :week week
                                        :registered-initials registered-initials)))
    (when proposal
      (commit-booking proposal))))

(defun make-date-key (year month day)
  "The YYYYMMDD integer of YEAR, MONTH and DAY, which orders as the dates do."
  (+ (* year 10000) (* month 100) day))

(defun date-key (dt)
  "Return YYYYMMDD integer for the LOCAL-TIME timestamp DT, suitable for ordering."
  (make-date-key (local-time:timestamp-year dt)
                 (local-time:timestamp-month dt)
                 (local-time:timestamp-day dt)))

(defun current-year-date-key (dt)
  "Like DATE-KEY but always uses today's year. Use this when comparing
   against ranges from *WEEKS*, which PARSE-WEEK-RANGE anchors to the
   current year."
  (make-date-key (local-time:timestamp-year (today))
                 (local-time:timestamp-month dt)
                 (local-time:timestamp-day dt)))

(defun parse-week-range (sheet-name)
  "Parse SHEET-NAME like \"Apr 20-26\" or \"Mar 30-Apr 5 (Holy wk)\".
Returns (values start-key end-key) as YYYYMMDD integers using the current
year, or (values nil nil) if the name does not match the expected shape
\(including names whose month word is not a month, e.g. \"Week 1-2\")."
  (cl-ppcre:register-groups-bind (sm sd em ed)
      ("^([A-Za-z]+)\\s+(\\d+)\\s*-\\s*(?:([A-Za-z]+)\\s+)?(\\d+)" sheet-name)
    (let* ((year (local-time:timestamp-year (today)))
           (start-month (month-number sm))
           (end-month (if em (month-number em) start-month)))
      (when (and start-month end-month)
        (values (make-date-key year start-month (parse-integer sd))
                (make-date-key year end-month (parse-integer ed)))))))

(defun discover-week-sheets ()
  "Return the names of the spreadsheet's tabs that parse as a
week range (see PARSE-WEEK-RANGE), in tab order.  Helper tabs such as
\"Dates\" or \"Blank Temp\" are dropped so they are never read as schedules."
  (remove-if-not #'parse-week-range (sheet-tabs)))

(defun week-sheets (&key refresh)
  "Return the names of the week sheets to search.
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
    (first (find-week-sheets-in-range target-key target-key))))

(defun vehicle-phrase (vehicle)
  "VEHICLE, a key of *VEHICLES*, as a sentence names it: \"the DGH\", \"the
Chevy\", \"a personal vehicle\".  NIL for no vehicle."
  (case vehicle
    ((nil) nil)
    (:chevy "the Chevy")
    (:personal "a personal vehicle")
    (t (format nil "the ~a" vehicle))))

(defun name-other-passengers (lead others)
  "LEAD, a reminder's sentence naming a leg's driver, finished with OTHERS,
the leg's passengers besides its driver."
  (cond
    ((null others)
     (format nil "~a with no other passengers." lead))
    ((null (rest others))
     (format nil "~a and ~a is a passenger." lead (first others)))
    (t
     (format nil "~a and ~{~a~^, ~} are passengers." lead others))))

(defun drives-p (initial passengers)
  "True when INITIAL drives a leg whose PASSENGERS are as listed: the driver
is the first of them."
  (and passengers (string-equal initial (first passengers))))

(defun day-trips (date)
  "The trips in the block for DATE, a LOCAL-TIME timestamp, of the week sheet
that covers it, read afresh, or NIL when no sheet covers it.  The sheet's name
is the second value."
  (let ((sheet (find-week-sheet date)))
    (when sheet
      (values (find-trips (sheet-trips sheet) :day (format-booking-day date))
              sheet))))

(defun remind-departure (trip)
  "Return the reminder for TRIP's departure, the same for everyone on it: the
driver (the first of its dep-passengers), the vehicle (VEHICLE-PHRASE), the
destination, the time, and the other passengers."
  (destructuring-bind (&optional driver &rest others) (trip-dep-passengers trip)
    (name-other-passengers
     (format nil "~a is the driver~@[ of ~a~] for a trip to ~a departing at ~a"
             driver (vehicle-phrase (trip-vehicle trip))
             (trip-destination trip) (trip-departure trip))
     others)))

(defun remind-driver (driver &optional (day-phrase "today"))
  "Return a string summarizing the trips DRIVER (e.g. \"BW\") is driving on
DAY-PHRASE (a day as PARSE-DAY reads it, defaults to today).
The driver is the first initial in dep-passengers.  Each trip is worded by
REMIND-DEPARTURE.  If multiple trips match, all are listed in one string
separated by spaces."
  (let* ((day-dt (parse-day day-phrase))
         (day-str (format-booking-day day-dt)))
    (multiple-value-bind (trips sheet) (day-trips day-dt)
      (let ((driver-trips (remove-if-not
                           (lambda (tr) (drives-p driver (trip-dep-passengers tr)))
                           trips)))
        (cond ((null sheet)
               (format nil "No schedule sheet covers ~a." day-str))
              ((null driver-trips)
               (format nil "~a is not driving on ~a." driver day-str))
              (t
               (format nil "~{~a~^ ~}" (mapcar #'remind-departure driver-trips))))))))

(defun remind-return-driver (trip)
  "Return the reminder for the driver of TRIP's return leg, the first of its
ret-passengers, worded like REMIND-DEPARTURE's and naming the vehicle likewise."
  (destructuring-bind (&optional driver &rest others) (trip-ret-passengers trip)
    (name-other-passengers
     (format nil "~a is the driver~@[ of ~a~] for the return from ~a at ~a"
             driver (vehicle-phrase (trip-vehicle trip))
             (trip-destination trip) (trip-return-time trip))
     others)))

(defun todays-trips ()
  "The trips in today's block of the week sheet that covers today, or NIL
when no sheet covers today.  Reads the sheet afresh on every call."
  (values (day-trips (today))))

(defun trips-for-passenger-today (initial)
  "Return the trips departing today with INITIAL among the dep-passengers,
driving (the first of them) or riding.  Used by the slack-bot reminder
scheduler. Returns NIL when no sheet covers today or no matching trips exist."
  (remove-if-not
   (lambda (tr)
     (member initial (trip-dep-passengers tr) :test #'string-equal))
   (todays-trips)))

(defun trips-for-return-driver-today (initial)
  "Return today's trips whose return leg INITIAL drives: INITIAL is the first
ret-passenger.  The return can have a different driver from the departure,
so it is looked up on its own.  Used by the slack-bot reminder scheduler;
NIL when no sheet covers today or no matching trips exist."
  (remove-if-not (lambda (tr) (drives-p initial (trip-ret-passengers tr)))
                 (todays-trips)))

(defun today-universal-time (time-str)
  "Return the CL universal-time at which the 12h TIME-STR (\"8:35 am\") falls
today, or NIL when TIME-STR cannot be read."
  (let ((mins (departure-minutes time-str)))
    (when (and mins (plusp mins))
      (multiple-value-bind (s m h day month year)
          (decode-universal-time (get-universal-time))
        (declare (ignore s m h))
        (encode-universal-time 0
                               (mod mins 60)
                               (floor mins 60)
                               day month year)))))

(defun trip-departure-universal-time (trip)
  "Return the CL universal-time at which TRIP departs today, or NIL.
Combines today's date with TRIP-DEPARTURE's HH:MM (12h format)."
  (today-universal-time (trip-departure trip)))

(defun trip-return-universal-time (trip)
  "Return the CL universal-time at which TRIP's return leg leaves today, or
NIL.  Combines today's date with TRIP-RETURN-TIME's HH:MM (12h format)."
  (today-universal-time (trip-return-time trip)))

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
the far end of \"Sept 19 - 22\" reads as \"Sept 22\" instead of a bare \"22\",
which PARSE-DAY cannot place.  END is returned unchanged otherwise."
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

(defun period-phrase-range (phrase)
  "Return (values START END), dates, when PHRASE names a week, weekend or
month; NIL otherwise.  Weeks run Monday to Sunday like the sheet's tabs.
  \"this week\", \"the week\", \"the whole week\", \"all week\", \"week\"
                                        -> this Monday to this Sunday
  \"next week\", \"the coming week\", \"all of next week\"
                                        -> next Monday to next Sunday
  \"last week\", \"the previous week\"  -> last Monday to last Sunday
  \"the weekend\", \"this weekend\", \"next weekend\"
                                        -> that week's Saturday and Sunday
  \"this month\", \"next month\", \"last month\" and the like
                                        -> that month's first to last day
  \"May\", \"all of May\", \"May 2027\"   -> May 1 to May 31, in the current
                                           year unless the phrase gives one
  \"the rest of the week\", \"the rest of the month\", \"the rest of May\"
                                        -> today to the period's last day,
                                           when today falls in it
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
         (which (if (= (length words) 2) (first words) "this"))
         (shift (cond ((member which '("next" "coming" "upcoming" "following")
                               :test #'string=)
                       1)
                      ((member which '("last" "previous" "past") :test #'string=)
                       -1)
                      ((string= which "this") 0)))
         (named-month (and words (month-number (first words))))
         (named-year (and named-month (= (length words) 2)
                          (cl-ppcre:scan "^\\d{4}$" (second words))
                          (parse-integer (second words))))
         (today (today)))
    (flet ((whole-month (first-day)
             (values first-day
                     (local-time:timestamp+
                      first-day
                      (1- (local-time:days-in-month (local-time:timestamp-month first-day)
                                                    (local-time:timestamp-year first-day)))
                      :day))))
      (multiple-value-bind (start end)
          (cond
            ((and named-month (or (null (rest words)) named-year))
             (whole-month (local-time:encode-timestamp
                           0 0 0 0 1 named-month
                           (or named-year (local-time:timestamp-year today)))))
            ((not (and (<= 1 (length words) 2) shift)) nil)
            ((string= noun "month")
             (whole-month (local-time:timestamp+
                           (local-time:timestamp-minimize-part today :day) shift :month)))
            ((member noun '("week" "weekend") :test #'string=)
             (let ((monday (local-time:timestamp+
                            today
                            (- (* 7 shift) (monday-index today))
                            :day)))
               (values (if (string= noun "weekend")
                           (local-time:timestamp+ monday 5 :day)
                           monday)
                       (local-time:timestamp+ monday 6 :day)))))
        (when start
          (values (if (and rest-p
                           (local-time:timestamp< start today)
                           (local-time:timestamp<= today end))
                      today
                      start)
                  end))))))

(defun phrase-date-range (phrase)
  "Resolve a date PHRASE to (values START END), the first and last day it
covers.  A week, weekend or month (\"next week\", \"the weekend\", \"this
month\", \"May\") covers all its days (PERIOD-PHRASE-RANGE); anything else is
a single day as a booking reads it (PARSE-DAY): \"today\", \"Saturday\",
\"Apr 20\", \"4/22\", \"in 3 days\".  Returns NIL when PHRASE is neither."
  (multiple-value-bind (start end) (period-phrase-range phrase)
    (if start
        (values start end)
        (let ((day (parse-day phrase)))
          (when day
            (values day day))))))

(defun trip-date-key (trip)
  "Return YYYYMMDD key for TRIP's day (\"Monday April 20\"), in the current
year like the ranges in *WEEKS*, or NIL when it does not read as a day."
  (let ((day (parse-day (trip-day trip))))
    (when day
      (date-key day))))

(defun find-week-sheets-in-range (start-key end-key)
  "Return the sheet names from WEEK-SHEETS whose date range overlaps [START-KEY, END-KEY]."
  (loop for name in (week-sheets)
        append (multiple-value-bind (ws we) (parse-week-range name)
                 (when (and ws we (<= ws end-key) (>= we start-key))
                   (list name)))))

(defun format-short-date (day-str)
  "Reformat a TRIP-DAY string like \"Monday April 20\" as \"Mon Apr 20\".
   Falls back to DAY-STR unchanged if it does not read as a day."
  (let ((day (parse-day day-str)))
    (if day
        (format nil "~a ~a ~D"
                (subseq (aref local-time:+day-names+ (local-time:timestamp-day-of-week day)) 0 3)
                (subseq (nth (1- (local-time:timestamp-month day)) *month-names*) 0 3)
                (local-time:timestamp-day day))
        day-str)))

(defun vehicle-priority (vehicle)
  "Return a sort key for VEHICLE based on position in *vehicles* (lower = higher priority)."
  (or (position vehicle *vehicles* :key #'first) 999))

(defun departure-minutes (time-str)
  "Convert a 12h TIME-STR like \"8:35 am\" to minutes-since-midnight. 0 on failure.
   Sorting a day's rows wants a number for every row; PARSE-CLOCK is the one
   to use where an unreadable time must not pass for midnight."
  (or (parse-clock time-str) 0))

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

(defun format-schedule-list (trips)
  "Render TRIPS as Slack mrkdwn that reads on a phone, where the rows of
FORMAT-SCHEDULE-TABLE are wider than the screen and wrap.  Each day is a bold
heading such as \"*Tue Sep 8*\", with a blank line between days; under it each
trip is a bullet -- its times in bold, then the vehicle and the destination --
and, on the line below, its passengers, driver first:

  *Tue Sep 8*
  • *8:35 am – 10:30 am* · DGH · SHMS
        MM, DM

The passengers are listed once when the same people go and come back, and as
\"out: ... · back: ...\" when they differ.  Nothing is shortened: a long note
wraps.  TRIPS must already be ordered by day."
  (with-output-to-string (out)
    (let ((previous-day nil))
      (dolist (tr trips)
        (unless (equal (trip-day tr) previous-day)
          (when previous-day (terpri out))
          (format out "*~a*~%" (format-short-date (trip-day tr)))
          (setf previous-day (trip-day tr)))
        (let ((departure (trip-departure tr))
              (return-time (trip-return-time tr))
              (destination (trip-destination tr))
              (going (trip-dep-passengers tr))
              (coming (trip-ret-passengers tr)))
          (format out "• *~a* · ~a~@[ · ~a~]~%"
                  (cond ((and (non-blank-p departure) (non-blank-p return-time))
                         (format nil "~a – ~a" departure return-time))
                        ((non-blank-p departure) departure)
                        ((non-blank-p return-time) (format nil "back ~a" return-time))
                        (t "no time"))
                  (string-upcase (symbol-name (trip-vehicle tr)))
                  (and (non-blank-p destination) destination))
          (cond ((not (equal going coming))
                 (format out "      out: ~:[—~;~:*~{~a~^, ~}~] · back: ~:[—~;~:*~{~a~^, ~}~]~%"
                         going coming))
                (going
                 (format out "      ~{~a~^, ~}~%" going))))))))

(defun schedule-order (a b)
  "True when trip A comes before trip B in a schedule table: by day, then
departure time, then vehicle priority.  A day that cannot be read sorts
first."
  (let ((ka (or (trip-date-key a) 0))
        (kb (or (trip-date-key b) 0)))
    (cond ((< ka kb) t)
          ((> ka kb) nil)
          (t (let ((da (departure-minutes (trip-departure a)))
                   (db (departure-minutes (trip-departure b))))
               (cond ((< da db) t)
                     ((> da db) nil)
                     (t (< (vehicle-priority (trip-vehicle a))
                           (vehicle-priority (trip-vehicle b))))))))))

(defun view-schedule (&optional (range-phrase "today"))
  "Display booked trips over a natural-language date range, as the list of
   FORMAT-SCHEDULE-LIST (Slack mrkdwn, a few short lines a trip).
   RANGE-PHRASE may be a single day or period (\"today\", \"tomorrow\", \"Apr 20\",
   \"in 3 days\", \"this week\", \"the whole week\", \"the rest of the week\",
   \"next week\", \"the weekend\", \"this month\", \"May\") for that window, or two of them joined by 'to',
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
          (flet ((in-range-p (tr)
                   (let ((k (trip-date-key tr)))
                     (and k (<= start-key k end-key) (booked-trip-p tr)))))
            (let* ((trips (sort (remove-if-not #'in-range-p (mapcan #'sheet-trips sheets))
                                #'schedule-order))
                   (out (cond ((null sheets)
                               (format nil "No schedule sheets cover ~a~@[ through ~a~].~%"
                                       start-label
                                       (unless (= start-key end-key) end-label)))
                              ((null trips)
                               (format nil "No trips scheduled from ~a to ~a.~%"
                                       start-label end-label))
                              (t (format-schedule-list trips)))))
              (write-string out)
              out)))))))

(defun proposal-day-view (proposal)
  "The trips already booked on PROPOSAL's day, in the schedule it was decided
against, as a table (FORMAT-SCHEDULE-TABLE), or NIL when none is."
  (let ((booked (remove-if-not #'booked-trip-p
                               (find-trips (proposal-trips proposal)
                                           :day (proposal-day proposal)))))
    (when booked
      (format-schedule-table (sort booked #'schedule-order)))))

(defun proposal-trip-view (proposal)
  "PROPOSAL's trip as it will read once written, as a table
(FORMAT-SCHEDULE-TABLE)."
  (format-schedule-table (list (proposal-trip proposal))))

(defun proposal-clash-checked-p (proposal)
  "True when PROPOSAL's times could be read as a window, so that it was
checked for clashes with the vans and passengers already out that day.  The
checks are skipped for times that cannot be (BOOKING-OBSTACLE), which the
asker should hear before saying yes."
  (and (trip-window (proposal-trip proposal)) t))

(defun date-range-phrase-p (phrase)
  "True when PHRASE is a date range VIEW-SCHEDULE can read: a single day or
period (\"today\", \"the weekend\", \"Sept 12\") or two of them joined the way
SPLIT-RANGE-PHRASE splits them (\"Sept 19 to Sept 22\").  A phrase PARSE-DAY
cannot read, or that signals while being read, gives NIL rather than an
error, so `/view` can answer a phrase it cannot read with examples rather
than with VIEW-SCHEDULE's error."
  (and (stringp phrase)
       (plusp (length (string-trim " " phrase)))
       (ignore-errors
        (multiple-value-bind (start-phrase end-phrase) (split-range-phrase phrase)
          (and (phrase-date-range start-phrase)
               (phrase-date-range end-phrase)
               t)))))

;;; The spreadsheet, through the Apps Script web app in van-booking.gs

(defun apps-script-reply (json what)
  "JSON, the web app's parsed reply about WHAT, unless it reports an error."
  (when (and (hash-table-p json) (gethash "error" json))
    (error "The van schedule's Apps Script could not ~a: ~a" what (gethash "error" json)))
  json)

(defun apps-script-get (what action &rest parameters)
  "The parsed reply about WHAT (as for APPS-SCRIPT-REPLY) of the web app's
doGet to ACTION, with PARAMETERS, a list of alternating query parameter names
and values."
  (let ((query (list* (cons "action" action)
                      (cons "spreadsheetId" *spreadsheet-id*)
                      (loop for (name value) on parameters by #'cddr
                            collect (cons name value)))))
    (apps-script-reply
     (com.inuoe.jzon:parse
      (dex:get (format nil "~a?~a" *apps-script-url* (quri:url-encode-params query))))
     what)))

(defun sheet-tabs ()
  "Return the names of the spreadsheet's tabs, in tab order."
  (coerce (apps-script-get "list the spreadsheet's tabs" "tabs") 'list))

(defun read-sheet (sheet-name)
  "Return every row of the tab SHEET-NAME as a list of cell strings, as the
sheet displays them.  Empty rows are kept, so the Nth row returned is row N
of the sheet; PARSE-SCHEDULE relies on that to know which rows a booking is
written to."
  (map 'list (lambda (row) (coerce row 'list))
       (apps-script-get (format nil "read the tab ~a" sheet-name)
                        "values" "sheet" sheet-name)))

(defun apps-script-secret ()
  "The secret every write to the web app carries: the environment variable
VAN_SCHEDULE_SECRET, which must equal the web app's script property of that
name.  Reads carry none, since the spreadsheet is public to view."
  (or (uiop:getenvp "VAN_SCHEDULE_SECRET")
      (error "VAN_SCHEDULE_SECRET is not set, so the van schedule cannot be written.")))

(defun write-range (range values &key sheet-name)
  "Write VALUES, a list of rows, to RANGE (of the tab SHEET-NAME when given)
through the web app's doPost."
  (let* ((qualified-range (if sheet-name
                              (format nil "~a!~a" sheet-name range)
                              range))
         (args (alexandria:plist-hash-table
                (list "range" qualified-range "valueInputOption" "USER_ENTERED") :test #'equal))
         (body-obj (alexandria:plist-hash-table
                    (list "values" (map 'vector (lambda (row) (coerce row 'vector)) values))
                    :test #'equal))
         (root (alexandria:plist-hash-table
                (list "secret" (apps-script-secret) "arguments" args "body" body-obj)
                :test #'equal))
         (payload (com.inuoe.jzon:stringify root))
         (url (format nil "~a?spreadsheetId=~a" *apps-script-url* *spreadsheet-id*)))
    ;; A web app answers a POST with a redirect to its reply.
    (multiple-value-bind (body status headers)
        (dex:request url
                     :method :post
                     :content payload
                     :headers '(("Content-Type" . "application/json"))
                     :max-redirects 0)
      (declare (ignore body))
      (let ((location (gethash "location" headers)))
        (unless location
          (error "Expected redirect from Apps Script, got status ~a" status))
        (apps-script-reply (com.inuoe.jzon:parse (dex:get location))
                           (format nil "write ~a" qualified-range))))))
