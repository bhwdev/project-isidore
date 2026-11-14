;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/plp/import
  (:use #:common-lisp)
  (:import-from #:cl-ppcre)
  (:import-from #:bhw/plp/song #:chord-line-p)
  (:local-nicknames (#:song #:bhw/plp/song)
                    (#:db #:bhw/plp/database))
  (:export #:chord-line-p
           #:split-columns
           #:parse-song-page
           #:parse-index-words
           #:index-tags
           #:staging-directory
           #:import-book
           #:load-staging
           #:page-chords
           #:graft-chords
           #:add-chords-to-staging
           #:add-chords-to-store)
  (:documentation
   "The Companions of the Cross chord book, from its PDF into the store, in
two steps with a person between them.

IMPORT-BOOK runs poppler's pdftotext over the PDF (a development-time tool:
it is not among the package's inputs, and production never imports), reads
the indices at the front for each song's tags and other titles, reads each
song's pages for its words, and writes one file per song, in BHW/PLP/SONG's
text format, into STAGING-DIRECTORY with a REPORT.txt of what looks wrong.
The files are then read against the book and corrected.  LOAD-STAGING stores
them; it can be run again after every correction.

The words come from the text laid out as on the page (pdftotext -layout): a
line of nothing but chords is dropped, a page set in two columns is cut at
the gutter, labels and verse numbers open sections, and the (Ref.) at the end
of a verse puts the refrain after it in the song's order.  The indices are
three columns of entries under headings that apply down a column, which only
the words' positions (pdftotext -tsv) sort out.  The chords come from those
positions too (PAGE-CHORDS): the laid-out text sets every letter as wide as
the next, and a chord would stand letters away from its syllable.

Songs staged or stored before the import kept the chords get them without
losing what was corrected since: ADD-CHORDS-TO-STAGING reads the book again
and gives each staged line that still has the book's words the book's chords
(GRAFT-CHORDS), and ADD-CHORDS-TO-STORE does the same for the stored songs
from the staging files, which is all a machine without the PDF needs.

None of the book's text is in this repository: the staging files and the
store are under the data root."))

(in-package #:bhw/plp/import)

;;; Lines of a song page

(defun words (line)
  (cl-ppcre:split "\\s+" (string-trim " " line)))

(defparameter *repeat-mark*
  (cl-ppcre:create-scanner
   "\\s*\\(\\s*(?:Ref|Refrain|Chorus)\\b\\.?\\s*([^)]*)\\)\\s*$" :case-insensitive-mode t)
  "Matches the mark at the end of a line that sends the singers to the
refrain: (Ref.), (Refrain 2), (Ref. x2).")

(defun right-chunk (line from)
  "Where in LINE, at or after column FROM, text starts after a gap of five
spaces or the start of the line, or NIL."
  (let ((start (position #\Space line :test #'char/= :start (min from (length line)))))
    (when start
      (let ((gap (- start (or (let ((end (position #\Space line :test #'char/=
                                                               :end start :from-end t)))
                                (and end (1+ end)))
                              0))))
        (cond ((>= gap 5) start)
              ;; Text runs across FROM: look past it.
              (t (let ((next (search "     " line :start2 start)))
                   (and next (right-chunk line next)))))))))

(defun split-columns (lines &key (from 45) (votes 4))
  "LINES of a page as a list of lines read down its columns.  The page is two
columns when a verse number or a label stands at or after column FROM behind
a gap, and at least VOTES lines have words (not chords, not a repeat mark)
from three columns left of the leftmost such label on; it is cut there.
Words that only stand far to the right (an echo, a deep indent) do not make
a column.  Returns as a second value T when the page was cut, and :UNCUT
when it looked like two columns but words run across the cut, so that it was
left as it was."
  (let* ((chunks (loop for line in lines
                       for start = (right-chunk line from)
                       for chunk = (and start (subseq line start))
                       when (and chunk
                                 (not (chord-line-p chunk))
                                 (not (cl-ppcre:scan "^\\(" chunk)))
                         collect (cons start chunk)))
         (labels (loop for (start . chunk) in chunks
                       when (inline-label chunk) collect start))
         (cut (and labels (- (reduce #'min labels) 3))))
    (cond
      ((or (null cut)
           (< (count-if (lambda (chunk) (>= (car chunk) cut)) chunks) votes))
       (values lines nil))
      ((some (lambda (line)
               (and (> (length line) cut)
                    (not (chord-line-p line))
                    (let ((left (position #\Space line :test #'char/= :end cut :from-end t))
                          (right (position #\Space line :test #'char/= :start cut)))
                      (and left right (< (- right left 1) 3)))))
             lines)
       (values lines :uncut))
      (t
       (flet ((left (line)
                (string-right-trim " " (subseq line 0 (min cut (length line)))))
              (right (line)
                ;; Keeps its indentation, as the left column has.
                (if (> (length line) cut)
                    (string-right-trim " " (subseq line cut))
                    "")))
         (values (append (mapcar #'left lines) (list "") (mapcar #'right lines))
                 t))))))

(defun clean-lyric (line &optional columns)
  "LINE as it is sung: one space between words, and a word spread under its
notes closed up (\"fore ----- ver - more\", \"ev'n – ing\").  A dash is taken
for such a break when a small letter follows it; one before a capital stays.
As a second value COLUMNS, positions in LINE, as positions in the result."
  (dolist (edit '(("\\s*-{2,}\\s*" "")
                  ("(?<=[a-z'])\\s+[-–]\\s+(?=[a-z])" "")
                  ("\\s+" " ")))
    (multiple-value-setq (line columns)
      (song:replace-tracking (first edit) line (second edit) columns)))
  (let ((lead (or (position #\Space line :test #'char/=) 0))
        (line (string-trim " " line)))
    (values line
            (mapcar (lambda (column) (max 0 (min (- column lead) (length line))))
                    columns))))

(defun split-page (text)
  "The page TEXT as a list of the texts of its songs: a page may hold two
short songs, the second under a centred title numbered one more than the
first's."
  (let* ((lines (cl-ppcre:split "\\n" text))
         (number (some (lambda (line)
                         (and (string/= (string-trim " " line) "")
                              (or (cl-ppcre:register-groups-bind (n)
                                      ("^\\s*(\\d{1,3})\\.\\s+\\S" line)
                                    (parse-integer n))
                                  0)))
                       lines))
         (parts '()) (part '()))
    (dolist (line lines)
      (let ((next (and number (plusp number) part
                       (cl-ppcre:register-groups-bind (n)
                           ("^ {15,}(\\d{1,3})\\.\\s+[A-Z0-9]" line)
                         (parse-integer n)))))
        (when (eql next (1+ number))
          (push (nreverse part) parts)
          (setf part '() number next))
        (push line part)))
    (push (nreverse part) parts)
    (mapcar (lambda (lines) (format nil "~{~A~^~%~}" lines)) (nreverse parts))))

(defun normal-label (word)
  "WORD, a label of the book, as BHW/PLP/SONG knows it: its misprints and
abbreviations spelled out."
  (cond ((cl-ppcre:scan "(?i)^ref" word) "Refrain")
        ((cl-ppcre:scan "(?i)^chorus" word) "Refrain")
        ((cl-ppcre:scan "(?i)^instr" word) "Instrumental")
        ((cl-ppcre:scan "(?i)^pre" word) "Pre-Chorus")
        (t word)))

(defparameter *known-labels*
  '("Refrain" "Bridge" "Pre-Chorus" "Tag" "Build" "Outro" "Coda" "Ending" "End"
    "Intro" "Instrumental" "Interlude" "Verse")
  "The labels that open a section wherever they stand; any other word does
only before a colon and a gap.")

(defun inline-label (line)
  "When LINE opens with a label, the label as BHW/PLP/SONG reads it (\"2.\",
\"Refrain 2:\") and the rest of the line; else NIL."
  (or (cl-ppcre:register-groups-bind (number rest) ("^\\s*(\\d{1,2})\\.(?:\\s+(.*))?$" line)
        (list (format nil "~A." number) (or rest "")))
      (cl-ppcre:register-groups-bind (word number gap rest)
          ("^\\s*([A-Za-z][A-Za-z'.-]*(?: [A-Za-z'.-]+){0,2}?)\\.?(?: (\\d))?:(\\s*)(.*)$" line)
        (let ((label (normal-label word)))
          (when (or (member label *known-labels* :test #'string-equal)
                    (string= rest "")
                    (>= (length gap) 2))
            (list (format nil "~A~@[ ~A~]:" label number) rest))))))

(defparameter *footer-start*
  (cl-ppcre:create-scanner
   "^ {0,6}(?:©|\\(c\\)|Copyright|Public Domain|Traditional|Words|Text|Tune|Music|Used [Bb]y|\\S.*©)")
  "Matches the first line of a page's footer: the copyright and the credits,
at the left margin.")

(defun parse-song-page (text)
  "What the page TEXT (pdftotext -layout) holds, as a plist: :NUMBER and
:TITLE when it opens a song, :WRITERS, :COPYRIGHT, :CCLI, :COLUMNS-P when it
was cut into two columns, :RESIDUE (lines kept that look like chords) and
:LINES, its words in the text format: labels on lines of their own, a blank
line after each stanza, and \"(Ref.)\" marks left at the ends of lines for
BUILD-SONG.  The chords are not among them: PAGE-CHORDS reads those."
  (let* ((lines (mapcar (lambda (line)
                          (string-right-trim
                           '(#\Space #\Return)
                           ;; A chord's fingering in the margin: E/G# 4x2400.
                           (cl-ppcre:regex-replace
                            "(?:^|\\s{2,})[A-G]\\S*\\s+[xX0-9]{6}\\s*$" line "")))
                        (cl-ppcre:split "\\n" text)))
         (lines (member "" lines :test #'string/=))
         (result '()))
    (cl-ppcre:register-groups-bind (number title) ("^\\s*(\\d{1,3})\\.\\s+(\\S.*)$" (first lines))
      (setf (getf result :number) (parse-integer number)
            (getf result :title) (clean-lyric title))
      (pop lines)
      (loop while (and lines (string= (first lines) "")) do (pop lines))
      ;; The writers stand under the title, centred, on up to three lines; a
      ;; hymn's begin "Text" or "Words", as a footer does.
      (let ((writers '()))
        (loop for line = (first lines)
              while (and line
                         (< (length writers) 3)
                         (or (cl-ppcre:scan "^ {12,}\\S" line)
                             (cl-ppcre:scan "^\\s*(?:Text|Words|Music|Tune)\\b" line))
                         (not (chord-line-p line))
                         (not (cl-ppcre:scan "^\\s*(?:\\d{1,2}\\.|[A-Za-z-]+(?: \\d)?:)\\s" line))
                         (not (cl-ppcre:scan "(?i)^\\s*(capo|\\(recording)" line)))
              do (push (clean-lyric (pop lines)) writers))
        (when writers
          (setf (getf result :writers) (format nil "~{~A~^ ~}" (nreverse writers))))))
    (let ((footer (position-if (lambda (line) (cl-ppcre:scan *footer-start* line)) lines)))
      (when footer
        (let ((text (clean-lyric (format nil "~{~A~^ ~}" (nthcdr footer lines)))))
          (setf (getf result :copyright) text
                (getf result :ccli) (cl-ppcre:register-groups-bind (number)
                                        ("CCLI Song #\\s*(\\d+)" text)
                                      number)
                lines (subseq lines 0 footer)))))
    (setf lines (remove-if (lambda (line)
                             (cl-ppcre:scan "(?i)^\\s*capo\\b|\\(recording:" line))
                           lines))
    (multiple-value-bind (lines columns-p) (split-columns lines)
      (setf (getf result :columns-p) columns-p)
      (let ((out '()) (residue '()))
        (dolist (line lines)
          (let ((label (inline-label line)))
            (when label
              (push "" out)
              (push (first label) out)
              (setf line (second label)))
            (let* ((mark (cl-ppcre:scan-to-strings *repeat-mark* line))
                   (words (if mark (cl-ppcre:regex-replace *repeat-mark* line "") line)))
              (cond ((string= (string-trim " " line) "")
                     (unless label (push "" out)))
                    ((or (chord-line-p words) (string= (string-trim " " words) ""))
                     ;; A mark under a chord line belongs to the line before.
                     (when (and mark out (string/= (first out) "")
                                (not (inline-label (first out))))
                       (setf (first out)
                             (format nil "~A ~A" (first out) (string-trim " " mark)))))
                    (t
                     (let ((lyric (clean-lyric words)))
                       (when (>= (count-if (lambda (word) (cl-ppcre:scan song::*chord-token* word))
                                           (words lyric))
                                 (max 2 (ceiling (length (words lyric)) 2)))
                         (push lyric residue))
                       (push (if mark
                                 (format nil "~A ~A" lyric (string-trim " " mark))
                                 lyric)
                             out)))))))
        (setf (getf result :lines) (nreverse out)
              (getf result :residue) (nreverse residue))))
    result))

;;; From pages to a song

(defun label-text-p (line)
  (and (song::label-line line) t))

(defun explicit-labels (lines)
  "LINES with a verse label put before each stanza that stands under none:
the lowest verse numbers the page's own labels do not use.  A stanza is under
a label when the label is the line before it."
  (let* ((used (loop for line in lines
                     for found = (song::label-line line)
                     when (and found (eq (first found) :verse))
                       collect (second found)))
         (out '()) (previous ""))
    (dolist (line lines)
      (when (and (string/= line "")
                 (string= previous "")
                 (not (label-text-p line)))
        (let ((number (loop for n from 1 unless (member n used) return n)))
          (push number used)
          (push (format nil "~D." number) out)))
      (push line out)
      (setf previous line))
    (nreverse out)))

(defun distinct-keys (sections)
  "SECTIONS with a later section that repeats a key (the second song of a
medley has its own verse 1) given the lowest number its kind has not used."
  (let ((seen '()))
    (mapcar (lambda (section)
              (let* ((section (copy-list section))
                     (kind (song:section-kind section)))
                (loop while (member (cons kind (or (song:section-number section) 1)) seen
                                    :test #'equal)
                      do (setf (getf section :number)
                               (1+ (or (song:section-number section) 1))))
                (push (cons kind (or (song:section-number section) 1)) seen)
                section))
            sections)))

(defun strip-marks (sections)
  "SECTIONS without the repeat marks at the ends of their lines, and the
order they give: each section, and after one that ends in a mark the refrain
the mark names.  The order is NIL when no line has a mark."
  (let ((order '()) (marked nil))
    (values
     (mapcar (lambda (section)
               (let ((section (copy-list section)) (after '()))
                 (setf (getf section :slides)
                       (loop for slide in (song:section-slides section)
                             collect (loop for line in slide
                                           for mark = (cl-ppcre:scan-to-strings *repeat-mark* line)
                                           for words = (string-trim
                                                        " " (cl-ppcre:regex-replace *repeat-mark* line ""))
                                           when mark
                                             do (setf marked t)
                                                (push (cl-ppcre:register-groups-bind (n)
                                                          ("(?i)(?:ref|refrain|chorus)\\.?\\s*(\\d)" mark)
                                                        (format nil "R~:[~A~;~]" (string= n "1") n))
                                                      after)
                                           when (string/= words "")
                                             collect words)))
                 (push (song:section-key section) order)
                 (dolist (key (reverse after))
                   (push (or key "R") order))
                 section))
             sections)
     ;; A verse marked (Ref.) with the refrain written next names it once.
     (and marked
          (loop for (key next) on (nreverse order)
                unless (equal key next) collect key)))))

(defun build-song (pages &key aliases tags)
  "The song that PAGES, the PARSE-SONG-PAGE plists of its pages in order,
hold, with ALIASES among its other titles and TAGS; and as a second value
what a reader should check, a list of sentences."
  (let* ((first (first pages))
         (number (getf first :number))
         (title (getf first :title))
         (lines (explicit-labels
                 (loop for page in pages
                       append (getf page :lines)
                       collect "")))
         (parsed (song::parse-body lines))
         (notes '()))
    (multiple-value-bind (sections order) (strip-marks (distinct-keys parsed))
      (let* ((sections (song:chorded-sections
                        (remove-if (lambda (section)
                                     (every #'null (song:section-slides section)))
                                   sections)))
             (keys (mapcar #'song:section-key sections))
             (order (remove-if-not (lambda (key) (member key keys :test #'string=)) order))
             (song (song:make-song
                    :id (format nil "cc2021/~D" number)
                    :title title
                    :alt-titles (remove-duplicates
                                 (remove-if (lambda (alias) (string-equal alias title))
                                            aliases)
                                 :test #'string-equal)
                    :writers (getf first :writers)
                    :copyright (format nil "~{~A~^ ~}"
                                       (remove-duplicates
                                        (remove nil (mapcar (lambda (page) (getf page :copyright))
                                                            pages))
                                        :test #'string=))
                    :ccli (some (lambda (page) (getf page :ccli)) pages)
                    :number number
                    :sections sections
                    :order (and (some (lambda (key) (char= (char key 0) #\R)) order)
                                order)
                    :tags (remove-duplicates tags :test #'equal :from-end t))))
        (when (string= (song:song-copyright song) "")
          (setf (song:song-copyright song) nil))
        (unless sections (push "no words were found" notes))
        (unless (song:song-writers song) (push "no writers line" notes))
        (unless (song:song-copyright song) (push "no copyright footer" notes))
        (when (some (lambda (page) (eq (getf page :columns-p) t)) pages)
          (push "set in two columns: check the order of the verses" notes))
        (when (some (lambda (page) (eq (getf page :columns-p) :uncut)) pages)
          (push "words stand beside the verses and were left in their lines" notes))
        (when (/= (length parsed)
                  (length (remove-duplicates parsed :test #'string= :key #'song:section-key)))
          (push "a section key came twice (a medley?): later ones were renumbered" notes))
        (dolist (page pages)
          (dolist (line (getf page :residue))
            (push (format nil "looks like chords: ~S" line) notes)))
        (dolist (section sections)
          (when (eq (song:section-kind section) :other)
            (push (format nil "unknown label ~S" (song:section-label section)) notes))
          (dolist (slide (song:section-slides section))
            (dolist (line slide)
              (when (cl-ppcre:scan "\\w\\s+[-–]\\s+\\w" line)
                (push (format nil "a dash between words: ~S" line) notes)))))
        (values song (nreverse notes))))))

;;; Indices

(defun parse-index-words (tsv)
  "The words of pdftotext -tsv output TSV as a list of plists (:PAGE :LEFT
:TOP :HEIGHT :TEXT), in the file's order."
  (loop for line in (cl-ppcre:split "\\n" tsv)
        for fields = (cl-ppcre:split "\\t" line)
        when (and (= (length fields) 12) (string= (first fields) "5"))
          collect (flet ((number (n)
                           (let ((*read-default-float-format* 'double-float))
                             (float (let ((*read-eval* nil))
                                      (read-from-string (nth n fields)))
                                    1.0))))
                    (list :page (parse-integer (nth 1 fields))
                          :left (number 6) :top (number 7)
                          :width (number 8) :height (number 9)
                          :text (nth 11 fields)))))

(defun column (word)
  (cond ((< (getf word :left) 215) 0)
        ((< (getf word :left) 395) 1)
        (t 2)))

(defun index-lines (words)
  "WORDS of one page as its lines, read down each of the three columns: a
list of lists of words, each from left to right."
  (let ((lines '()))
    (dotimes (column 3)
      (let ((sorted (sort (remove column words :key #'column :test #'/=)
                          (lambda (a b)
                            (if (< (abs (- (getf a :top) (getf b :top))) 3.5)
                                (< (getf a :left) (getf b :left))
                                (< (getf a :top) (getf b :top)))))))
        (loop while sorted
              do (let* ((top (getf (first sorted) :top))
                        (line (loop while (and sorted
                                               (< (abs (- (getf (first sorted) :top) top)) 3.5))
                                    collect (pop sorted))))
                   (push (sort line #'< :key (lambda (word) (getf word :left))) lines)))))
    (nreverse lines)))

(defun join-words (words)
  "The text of WORDS, with no space where one starts where the last ended:
small capitals come as a capital and the rest."
  (with-output-to-string (out)
    (loop for (word next) on words
          do (write-string (getf word :text) out)
             (when (and next
                        (> (- (getf next :left) (+ (getf word :left) (getf word :width))) 1.5))
               (write-char #\Space out)))))

(defparameter *indices*
  '(("ALPHABETICAL" . :alias) ("TEMPO" . :tempo) ("KEY" . :key) ("MASS" . :mass)
    ("LITURGICAL" . :liturgy) ("LENTEN" . :lent))
  "The word in an index's title that tells which it is, and its category.")

(defun index-tags (words)
  "What the indices say of each song, from WORDS (PARSE-INDEX-WORDS) of
their pages: a list of (number category name note), category :ALIAS for the
alphabetical index's titles and else a tag's; and as a second value the
entries without a page number, as strings."
  (let ((entries '()) (unresolved '()) (category nil))
    (dolist (page (remove-duplicates (mapcar (lambda (word) (getf word :page)) words)))
      (let* ((page-words (remove page words :key (lambda (word) (getf word :page)) :test #'/=))
             (title (string-upcase
                     (format nil "~{~A~}"
                             (mapcar (lambda (word) (getf word :text))
                                     (remove-if-not (lambda (word)
                                                      (and (>= (getf word :height) 15)
                                                           (< (getf word :top) 100)))
                                                    page-words)))))
             (heading nil) (note nil) (heading-open nil) (pending '()) (pending-left nil))
        (loop for (word . found) in *indices*
              when (search word title) do (setf category found))
        (flet ((drop-pending ()
                 (when (and pending category)
                   (push (format nil "~{~A~^ ~}" (reverse pending)) unresolved))
                 (setf pending '())))
          (dolist (line (index-lines page-words))
            (let* ((line (remove-if (lambda (word) (cl-ppcre:scan "^[.\\s]+$" (getf word :text)))
                                    line))
                   (height (reduce #'max line :key (lambda (word) (getf word :height))
                                              :initial-value 0)))
              (cond ((or (null line) (>= height 14.5)))
                    ;; A heading: its capital is taller than an entry's words.
                    ((>= (getf (first line) :height) (if (eq category :key) 12 9.5))
                     (drop-pending)
                     (let* ((main (remove-if (lambda (word) (< (getf word :height) 8)) line))
                            (small (remove-if-not (lambda (word) (< (getf word :height) 8)) line))
                            (text (cl-ppcre:regex-replace "(?i)\\s*\\(cont\\)" (join-words main) "")))
                       (setf heading (if heading-open (format nil "~A ~A" heading text) text)
                             note (let ((text (string-trim "()" (join-words small))))
                                    (cond ((string= text "") (and heading-open note))
                                          (t text)))
                             heading-open t)))
                    ;; Small print under a heading: the rest of its dates.
                    ((and heading-open (< height 8))
                     (setf note (string-trim "()" (format nil "~@[~A ~]~A" note (join-words line)))))
                    (t
                     (setf heading-open nil)
                     (let* ((texts (remove
                                    ""
                                    ;; The leader may run into the word
                                    ;; before it and the number after it.
                                    (loop for word in line
                                          append (cl-ppcre:split "\\.{2,}|\\.$" (getf word :text)))
                                    :test #'string=))
                            (last (first (last texts)))
                            (number (and (rest texts)
                                         (every #'digit-char-p last)
                                         (parse-integer last)))
                            (left (getf (first line) :left)))
                       (when (and pending (not (> left (+ pending-left 8))))
                         (drop-pending))
                       (unless pending
                         (setf pending-left left))
                       (push (format nil "~{~A~^ ~}" (if number (butlast texts) texts)) pending)
                       (when number
                         (let ((title (format nil "~{~A~^ ~}" (reverse pending))))
                           (setf pending '())
                           (case category
                             ((nil))
                             (:alias (push (list number :alias title nil) entries))
                             (:lent (push (list number :lent "No Alleluia" nil) entries))
                             (:key (when heading
                                     (push (list number :key heading nil) entries)))
                             (t (when heading
                                  (push (list number category (string-capitalize heading)
                                              (and (eq category :liturgy) note))
                                        entries)))))))))))
          (drop-pending))))
    (values (nreverse entries) (nreverse unresolved))))

;;; Chords

(defun page-rows (words)
  "WORDS of one page (PARSE-INDEX-WORDS) as its rows from the top down, each
a list of the words that stand at one height, from left to right."
  (let ((rows '()))
    (dolist (word (sort (copy-list words) #'< :key (lambda (word) (getf word :top))))
      (if (and rows (< (abs (- (getf word :top) (getf (first (first rows)) :top))) 3))
          (push word (first rows))
          (push (list word) rows)))
    (mapcar (lambda (row) (sort row #'< :key (lambda (word) (getf word :left))))
            (nreverse rows))))

(defun row-segments (row &key (gap 40))
  "The words of ROW in runs: a run ends where the next word stands more than
GAP points on, as across the gutter of a page set in two columns."
  (let ((segments '()))
    (loop for word in row
          for previous = nil then last
          for last = word
          do (if (and previous
                      (<= (- (getf word :left) (+ (getf previous :left) (getf previous :width)))
                          gap))
                 (push word (first segments))
                 (push (list word) segments)))
    (nreverse (mapcar #'reverse segments))))

(defun segment-text (segment)
  (format nil "~{~A~^ ~}" (mapcar (lambda (word) (getf word :text)) segment)))

(defun segment-left (segment) (getf (first segment) :left))

(defun segment-right (segment)
  (let ((word (first (last segment))))
    (+ (getf word :left) (getf word :width))))

(defun segment-column (segment x)
  "Which character of SEGMENT's text stands at X points from the page's left
edge: the start of a word for the space before it, and the text's length
past its end."
  (let ((start 0))
    (dolist (word segment (length (segment-text segment)))
      (let ((left (getf word :left))
            (length (length (getf word :text))))
        (cond ((< x left) (return start))
              ((< x (+ left (getf word :width)))
               (return (+ start (min (1- length)
                                     (round (* length (- x left)) (getf word :width)))))))
        (incf start (1+ length))))))

(defun page-chords (words)
  "The lines of words of one page with the chords that stand over them, from
WORDS (PARSE-INDEX-WORDS): a list of (line . chords) from the top down, the
line as CLEAN-LYRIC leaves it, without its label and its repeat mark, and
the chords as (offset . chord).  A chord is that of the words under it: of
the run it stands over or, over none, of the nearest; one past the end of
its line, over a line that goes on under that one with no chords of its own,
is played on the line below.  A chord over no words at all is left out."
  (let* ((rows (mapcar (lambda (row)
                         (mapcar (lambda (segment)
                                   (list :words segment
                                         :chords-p (chord-line-p (segment-text segment))
                                         :top (getf (first segment) :top)
                                         :chords '()))
                                 (row-segments row)))
                       (page-rows words)))
         (rows (coerce rows 'vector)))
    (flet ((under (index)
             ;; The runs of words in the row next below row INDEX.
             (let ((next (and (< (1+ index) (length rows)) (aref rows (1+ index)))))
               (and next
                    (< (- (getf (first next) :top) (getf (first (aref rows index)) :top)) 22)
                    (remove-if (lambda (segment) (getf segment :chords-p)) next))))
           (distance (segment x)
             (let ((words (getf segment :words)))
               (cond ((< x (segment-left words)) (- (segment-left words) x))
                     ((> x (segment-right words)) (- x (segment-right words)))
                     (t 0)))))
      (dotimes (index (length rows))
        (let ((below (under index)))
          (when below
            (dolist (segment (aref rows index))
              (when (getf segment :chords-p)
                (dolist (chord (getf segment :words))
                  (let* ((x (getf chord :left))
                         (nearest (first (sort (copy-list below) #'<
                                               :key (lambda (segment) (distance segment x)))))
                         (wrapped (and (> x (+ 12 (segment-right (getf nearest :words))))
                                       (notany (lambda (segment) (getf segment :chords-p))
                                               (aref rows (1+ index)))
                                       (find-if (lambda (segment) (< (distance segment x) 12))
                                                (under (1+ index)))))
                         (target (or wrapped nearest)))
                    (push (cons x (getf chord :text)) (getf target :chords))))))))))
    (loop for row across rows
          append (loop for segment in row
                       for text = (segment-text (getf segment :words))
                       for label = (inline-label text)
                       for line = (if label (second label) text)
                       for shift = (- (length text) (length line))
                       for words = (cl-ppcre:regex-replace *repeat-mark* line "")
                       unless (or (getf segment :chords-p)
                                  (string= (string-trim " " words) ""))
                         collect (let ((chords (sort (copy-list (getf segment :chords)) #'<
                                                     :key #'car)))
                                   (multiple-value-bind (lyric columns)
                                       (clean-lyric
                                        words
                                        (mapcar (lambda (chord)
                                                  (- (segment-column (getf segment :words)
                                                                     (car chord))
                                                     shift))
                                                chords))
                                     (cons lyric
                                           (mapcar (lambda (column chord)
                                                     (cons (song:snap-column lyric column)
                                                           (cdr chord)))
                                                   columns chords))))))))

(defun letters-before (line offset)
  "How many of the first OFFSET characters of LINE BHW/PLP/SONG:SLUG keeps."
  (count-if (lambda (char) (or (char<= #\a (char-downcase char) #\z) (digit-char-p char)))
            line :end (min offset (length line))))

(defun graft-line (from chords onto)
  "CHORDS, those of the line FROM, as the chords of the line ONTO, which has
FROM's words but not always its spaces, capitals and punctuation: each
before the letter it stands before in FROM."
  (loop for (offset . chord) in chords
        for letters = (letters-before from offset)
        collect (cons (or (and (< offset (length from))
                               (loop for at from 0 below (length onto)
                                     when (= (letters-before onto (1+ at)) (1+ letters))
                                       return at))
                          (length onto))
                      chord)))

(defun graft-chords (lines song)
  "SONG with the chords of LINES, a list of (line . chords) as PAGE-CHORDS
makes: each line of SONG that has no chords and has the words of one of
LINES, whatever their capitals and punctuation, gets that line's chords.  Of
several of LINES with the same words, each is taken once, in order, and the
last of them from then on; one of them without chords, the sheet not giving
them again, has those of the one before.  Two lines of SONG that have the words of one of
LINES between them, the page's line having been broken, share its chords."
  (let ((table (make-hash-table :test #'equal))
        (played (make-hash-table :test #'equal))
        (grafted (song:copy-song song)))
    (dolist (line (reverse lines))
      (push line (gethash (song:slug (car line)) table)))
    (flet ((take (text)
             ;; The chords of the line of LINES that has the words of TEXT.
             (let* ((key (song:slug text))
                    (found (first (gethash key table))))
               (when (rest (gethash key table))
                 (pop (gethash key table)))
               (when (cdr found)
                 (setf (gethash key played) found))
               (let ((found (if (cdr found) found (gethash key played))))
                 (and found (graft-line (car found) (cdr found) text))))))
      (setf (song:song-sections grafted)
            (loop for section in (song:song-sections song)
                  for had = (song:section-chords section)
                  for chords
                    = (loop for slide in (song:section-slides section)
                            for slide-had = (pop had)
                            collect
                            (let ((result (make-list (length slide))))
                              (loop for (line next) on slide
                                    for at from 0
                                    for both = (and next (format nil "~A ~A" line next))
                                    do (cond ((nth at result))
                                             ((nth at slide-had)
                                              ;; Its line of LINES is taken all the same.
                                              (take line)
                                              (setf (nth at result) (nth at slide-had)))
                                             ((gethash (song:slug line) table)
                                              (setf (nth at result) (take line)))
                                             ((and both
                                                   (not (nth (1+ at) slide-had))
                                                   (gethash (song:slug both) table))
                                              (let ((chords (take both))
                                                    (end (length line)))
                                                (setf (nth at result)
                                                      (remove-if (lambda (chord) (> (car chord) end))
                                                                 chords)
                                                      (nth (1+ at) result)
                                                      (loop for (offset . chord) in chords
                                                            when (> offset end)
                                                              collect (cons (- offset end 1)
                                                                            chord)))))))
                              result))
                  collect (append (loop for (key value) on section by #'cddr
                                        unless (eq key :chords)
                                          append (list key value))
                                  (and (some (lambda (slide) (some #'identity slide)) chords)
                                       (list :chords chords))))))
    grafted))

(defun song-chords (song)
  "The lines of SONG with their chords, as GRAFT-CHORDS takes them."
  (loop for section in (song:song-sections song)
        append (loop with chords = (song:section-chords section)
                     for slide in (song:section-slides section)
                     for slide-chords = (pop chords)
                     append (loop for line in slide
                                  collect (cons line (pop slide-chords))))))

;;; The book

(defun staging-directory ()
  "<data-root>/plp/staging/, where the songs wait to be read."
  (uiop:subpathname (db:plp-directory) "staging/"))

(defun pdftotext (pdf &rest options)
  "The output of poppler's pdftotext on PDF with OPTIONS, as a string."
  (uiop:run-program (append (list "pdftotext") options (list (namestring pdf) "-"))
                    :output :string
                    :external-format :utf-8
                    :error-output nil))

(defun staging-file (song)
  (uiop:subpathname (staging-directory)
                    (format nil "~3,'0D-~A.txt" (song:song-number song)
                            (let ((slug (song:slug (song:song-title song))))
                              (subseq slug 0 (min 60 (length slug)))))))

(defun reviewed-file-p (number)
  "True when the staging file of song NUMBER says Reviewed: yes."
  (some (lambda (file)
          (and (uiop:string-prefix-p (format nil "~3,'0D-" number) (pathname-name file))
               (song:song-reviewed
                (song:parse-song (uiop:read-file-string file :external-format :utf-8)))))
        (uiop:directory-files (staging-directory) "*.txt")))

(defun book-songs (pdf)
  "The songs of the chord book PDF in its order, each as (song . notes),
notes being what a reader should check; and as a second and third value the
numbers the indices name that no song has, and the index entries without a
page number."
  (let* ((parsed (loop for text in (cl-ppcre:split "\\f" (pdftotext pdf "-layout"))
                       for page-number from 1
                       append (mapcar (lambda (part)
                                        (list* :page page-number (parse-song-page part)))
                                      (split-page text))))
         ;; The first index is headed "1. Alphabetical Index".
         (first-song (position-if (lambda (page)
                                    (and (eql (getf page :number) 1)
                                         (not (search "INDEX" (string-upcase (getf page :title))))))
                                  parsed))
         (words (parse-index-words (pdftotext pdf "-tsv")))
         (chords (make-hash-table))
         (groups '()) (group '()) (expected 1))
    (flet ((page (word) (getf word :page)))
      (dolist (page (remove-duplicates (mapcar #'page words)))
        (setf (gethash page chords)
              (page-chords (remove page words :key #'page :test #'/=)))))
    (unless first-song
      (error "No page of ~A opens song 1." pdf))
    (multiple-value-bind (entries unresolved)
        (index-tags (remove-if (lambda (word)
                                 (>= (getf word :page) (getf (nth first-song parsed) :page)))
                               words))
      ;; A page opens a song when its number is the next one or near it; a
      ;; verse numbered at the top of a second page is not a title.
      (loop for page in (nthcdr first-song parsed)
            for number = (getf page :number)
            do (cond ((and number (<= expected number (+ expected 3)))
                      (when group (push (nreverse group) groups))
                      (setf group (list page)
                            expected (1+ number)))
                     (t
                      (when number
                        ;; Not a title after all: give the line back.
                        (setf page (list* :lines
                                          (append (list "" (format nil "~D." number)
                                                        (getf page :title) )
                                                  (getf page :lines))
                                          page)))
                      (push page group))))
      (when group (push (nreverse group) groups))
      (let ((songs (loop for pages in (nreverse groups)
                         collect (let* ((number (getf (first pages) :number))
                                        (mine (remove number entries :key #'first :test #'/=)))
                                   (multiple-value-bind (song notes)
                                       (build-song
                                        pages
                                        :aliases (mapcar #'third
                                                         (remove :alias mine :key #'second
                                                                             :test-not #'eq))
                                        :tags (mapcar #'rest (remove :alias mine :key #'second)))
                                     (setf (song:song-page song) (getf (first pages) :page))
                                     (cons (graft-chords
                                            (loop for page in (remove-duplicates
                                                               (mapcar (lambda (page)
                                                                         (getf page :page))
                                                                       pages))
                                                  append (gethash page chords))
                                            song)
                                           (if (remove :alias mine :key #'second)
                                               notes
                                               (cons "in no index but the alphabetical"
                                                     notes))))))))
        (values songs
                (sort (set-difference (remove-duplicates (mapcar #'first entries))
                                      (mapcar (lambda (song) (song:song-number (car song)))
                                              songs))
                      #'<)
                unresolved)))))

(defun write-staging-file (song)
  (with-open-file (file (staging-file song) :direction :output
                                            :if-exists :supersede
                                            :external-format :utf-8)
    (write-string (song:print-song song) file)))

(defun import-book (pdf)
  "Stage every song of the chord book PDF and write REPORT.txt beside them.
A staging file that says Reviewed: yes is left as it is.  Returns the number
of songs staged and the report's pathname."
  (multiple-value-bind (songs missing unresolved) (book-songs pdf)
    (ensure-directories-exist (staging-directory))
    (let ((report (uiop:subpathname (staging-directory) "REPORT.txt"))
          (staged 0))
      (with-open-file (out report :direction :output :if-exists :supersede
                                  :external-format :utf-8)
        (format out "Staged from ~A~%~%" (file-namestring pdf))
        (loop for (song . notes) in songs
              for number = (song:song-number song)
              do (cond ((reviewed-file-p number)
                        (format out "~3D ~A: reviewed, left alone~%" number
                                (song:song-title song)))
                       (t
                        (write-staging-file song)
                        (incf staged)
                        (when notes
                          (format out "~3D ~A~%~{      ~A~%~}" number (song:song-title song)
                                  notes)))))
        (format out "~%Numbers in the indices with no song: ~{~D~^, ~}~%" missing)
        (format out "~%Index entries with no page number:~%~{  ~A~%~}" unresolved))
      (values staged report))))

(defun load-staging (&key (directory (staging-directory)))
  "Store every song staged in DIRECTORY, in place of the stored song of the
same id.  Returns the number stored and a list of (file . problems) for the
files that were not."
  (let ((stored 0) (refused '()))
    (loop for (file . song) in (staged-songs directory)
          for problems = (song:song-problems song)
          do (cond (problems (push (cons (file-namestring file) problems) refused))
                   (t (db:save-song song)
                      (incf stored))))
    (values stored (nreverse refused))))

;;; Chords for songs that were read without them

(defun staged-songs (directory)
  "The songs staged in DIRECTORY as a list of (file . song), by file name."
  (loop for file in (sort (uiop:directory-files directory "*.txt")
                          #'string< :key #'namestring)
        unless (string= (pathname-name file) "REPORT")
          collect (cons file (song:parse-song
                              (uiop:read-file-string file :external-format :utf-8)))))

(defun chorded-lines (song)
  "How many lines of SONG have chords."
  (loop for section in (song:song-sections song)
        sum (loop for slide in (song:section-chords section)
                  sum (count-if #'identity slide))))

(defun chorded-text (text song)
  "TEXT, a song in the text format, with the chords of SONG, the song that
TEXT writes with chords since found for it, written into its lines.  Nothing
else of TEXT changes: its marks for the musicians stay."
  (let ((chords (song-chords song))
        (headers-p t))
    (format nil "~{~A~%~}"
            (loop for line in (cl-ppcre:split "\\r?\\n" text)
                  for sung = (song:split-chords (song:sung-line (string-trim " " line)))
                  collect (cond ((string= line "") (setf headers-p nil) line)
                                ((or headers-p
                                     (string/= sung (or (car (first chords)) "")))
                                 line)
                                (t (song:join-chords
                                    (string-trim " " line)
                                    (graft-line sung (cdr (pop chords))
                                                (string-trim " " line)))))))))

(defun add-chords-to-staging (pdf &key (directory (staging-directory)))
  "Give the songs staged in DIRECTORY the chords the chord book PDF has over
their lines (GRAFT-CHORDS), and write again the files that gained some: what
was corrected in them stays.  Returns the number of files written."
  (let ((book (mapcar #'car (book-songs pdf)))
        (written 0))
    (loop for (file . song) in (staged-songs directory)
          for from = (find (song:song-id song) book :key #'song:song-id :test #'equal)
          for grafted = (and from (graft-chords (song-chords from) song))
          when (and grafted (> (chorded-lines grafted) (chorded-lines song)))
            do (let ((text (chorded-text (uiop:read-file-string file :external-format :utf-8)
                                         grafted)))
                 (with-open-file (out file :direction :output :if-exists :supersede
                                           :external-format :utf-8)
                   (write-string text out)))
               (incf written))
    written))

(defun add-chords-to-store (&key (directory (staging-directory)))
  "Give each stored song the chords that the song of its id staged in
DIRECTORY has over the same lines (GRAFT-CHORDS), and store those that
gained some: what was corrected in the store stays.  Returns the number of
songs stored."
  (let ((stored 0))
    (loop for (nil . from) in (staged-songs directory)
          for song = (and (song:song-id from) (db:find-song (song:song-id from)))
          for grafted = (and song (graft-chords (song-chords from) song))
          when (and grafted (> (chorded-lines grafted) (chorded-lines song)))
            do (db:save-song grafted)
               (incf stored))
    stored))
