;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/plp/song
  (:use #:common-lisp)
  (:import-from #:cl-ppcre)
  (:export #:song #:make-song #:copy-song
           #:song-id #:song-title #:song-alt-titles #:song-writers
           #:song-copyright #:song-ccli #:song-number #:song-page
           #:song-sections #:song-order #:song-tags #:song-reviewed
           #:section-kind #:section-number #:section-label #:section-slides
           #:section-key #:section-name
           #:section-chords
           #:parse-song #:parse-sections #:parse-words #:parse-tag
           #:print-song #:print-sections #:print-tag
           #:song-problems
           #:sung-line #:sung-lines
           #:chord-line-p #:split-chords #:join-chords #:chorded-sections
           #:replace-tracking #:snap-column
           #:transpose-chord #:song-key #:flat-key-p
           #:outline #:*outline-kinds* #:kind-label #:relabel #:outline-song
           #:song-first-line #:slug
           #:*max-lines* #:split-lines
           #:song-deck #:slide-key #:slide-label #:slide-lines #:slide-part
           #:slide-of #:slide-group #:slide-chords
           #:deck-jump #:same-slide-p #:deck-chart #:deck-next)
  (:documentation
   "A song as data, with no store and no page.

A SONG holds its credits and its SECTIONS in the order written.  A section is
a plist: :KIND (:VERSE :REFRAIN :BRIDGE :PRE-CHORUS :TAG :ENDING :OTHER),
:NUMBER, :LABEL (what the source called it), :SLIDES, a list of slides, each
a list of lines, and, when the song has them, :CHORDS: for each slide a list
with, for each of its lines, the chords played over it as (offset . chord),
the offset counting the line's characters.  A section's key is what the operator presses and what
an Order: header names: 1 2 3 for verses, R B P T O X for the other kinds,
with the number after it from the second on (R2).

PARSE-SONG and PRINT-SONG read and write the text format, the one the chord
book's import stages its songs in and the one a custom song is pasted in:

  Title: Abide With Me
  Writers: Henry Francis Lyte
  Order: 1 R 2 R

  1.
  Abide with me, fast falls the eventide

  Refrain:
  ...

Headers end at the first blank line.  A label stands alone on its line; a
blank line inside a labelled section breaks the slide; text under no label
is verses, one per block.  The marks a song sheet puts beside its words for
the musicians, (x2) or (V1, V2, Ref.), are not words: PARSE-SONG leaves
them out (SUNG-LINE), and a deck never shows them.

Chords are written in the words, each in brackets before the syllable it is
played on: [G]Amazing [D/F#]grace.  A line of nothing but chords over a line
of words, as a chord sheet has them, is read as that line's chords, each
where it stands (INLINE-CHORD-LINES).  The lines of a section never hold
them: the screen shows words only, and a musician's page puts the chords
back over them, in another key if asked (TRANSPOSE-CHORD).

OUTLINE reads words pasted from elsewhere, with or without labels, and
suggests their sections and the order they are sung in; OUTLINE-SONG makes
the song of it.

SONG-DECK lays the song out as the slides to show, in its order, and
DECK-JUMP finds where a key press leads; DECK-CHART is each of its slides
once, as a musician reads them."))

(in-package #:bhw/plp/song)

(defstruct song
  "A song.  TAGS is a list of (category name note), category a keyword and
note a string or NIL; ORDER a list of section keys or NIL for as written."
  id title alt-titles writers copyright ccli number page
  sections order tags reviewed)

(defun section-kind (section) (getf section :kind))
(defun section-number (section) (getf section :number))
(defun section-label (section) (getf section :label))
(defun section-slides (section) (getf section :slides))
(defun section-chords (section) (getf section :chords))

(defparameter *kinds*
  '((:refrain "R" "Refrain" "refrain" "chorus" "ref" "refr")
    (:bridge "B" "Bridge" "bridge")
    (:pre-chorus "P" "Pre-Chorus" "pre-chorus" "prechorus" "pre chorus")
    (:tag "T" "Tag" "tag" "build")
    (:ending "O" "Outro" "outro" "coda" "ending" "end")
    (:verse nil "Verse" "verse")
    (:wordless nil nil "intro" "instrumental" "instr" "instr." "interlude")
    (:other "X" nil))
  "Each kind of section: its key letter, its default label and the words a
label line may use for it.")

(defun kind-letter (kind) (second (assoc kind *kinds*)))

(defun label-kind (word)
  "The kind that the label word WORD names, or :OTHER."
  (or (first (find-if (lambda (entry)
                        (member word (cdddr entry) :test #'string-equal))
                      *kinds*))
      :other))

(defun section-key (section)
  "The key of SECTION: \"2\" for the second verse, \"R\", \"R2\", \"B\"."
  (let ((number (or (section-number section) 1)))
    (if (eq (section-kind section) :verse)
        (princ-to-string number)
        (format nil "~A~:[~;~D~]" (kind-letter (section-kind section))
                (> number 1) number))))

(defun section-name (section)
  "What to call SECTION on a preview: \"Verse 2\", \"Refrain\", \"Bridge 2\"."
  (let ((number (or (section-number section) 1)))
    (format nil "~A~:[~; ~D~]" (section-label section)
            (or (eq (section-kind section) :verse) (> number 1))
            number)))

;;; Reading

(defun trim (string)
  (string-trim '(#\Space #\Tab #\Return #\Newline) string))

(defun split-list (string separator)
  "The trimmed, non-empty parts of STRING between occurrences of SEPARATOR."
  (remove "" (mapcar #'trim (cl-ppcre:split separator string)) :test #'string=))

;;; Chords

(defparameter *chord-token*
  (let* ((chord "[A-G][#b]?(?:maj|min|sus|dim|aug|add|m|M|[0-9]|\\+|°)*(?:/[A-G][#b]?)?\\*?")
         (part (format nil "(?:(?:~A)+|/[A-G][#b]?|sus\\d?)" chord)))
    (cl-ppcre:create-scanner
     (format nil "^\\(?(?:~A(?:-~A)*-?|N\\.?C\\.?|[-–—|/%]+|x\\d|\\d+x)\\)?[,.]?$" part part)))
  "Matches a word of a chord line: a chord (G, Bm7, Asus4, A/C#), chords run
together (G-D, BE), a bass note alone (/F#), or one of the marks between
them.")

(defun chord-line-p (line)
  "True when every word of LINE is a chord: G, Bm7, Asus4, A/C#, a dash."
  (let ((words (cl-ppcre:split "\\s+" (string-trim " " line))))
    (and words
         (every (lambda (word) (cl-ppcre:scan *chord-token* word)) words)
         ;; A dash alone is not a chord line.
         (some (lambda (word) (cl-ppcre:scan "[A-GN]" word)) words))))

(defun split-chords (line)
  "LINE without the chords written in it, and as a second value those chords
as a list of (offset . chord): \"[G]Amazing [D]grace\" -> \"Amazing grace\",
((0 . \"G\") (8 . \"D\")).  Brackets around anything but a chord are words."
  (let ((out (make-string-output-stream)) (chords '()) (length 0) (last 0))
    (flet ((emit (text)
             (write-string text out)
             (incf length (length text))))
      (cl-ppcre:do-scans (start end starts ends "\\[([^\\]\\[\\s]+)\\]" line)
        (let ((chord (subseq line (aref starts 0) (aref ends 0))))
          (when (cl-ppcre:scan *chord-token* chord)
            (emit (subseq line last start))
            (push (cons length chord) chords)
            (setf last end))))
      (emit (subseq line last)))
    (let* ((text (get-output-stream-string out))
           (lead (or (position #\Space text :test #'char/=) (length text)))
           (text (trim text)))
      (values text
              (mapcar (lambda (chord)
                        (cons (max 0 (min (- (car chord) lead) (length text))) (cdr chord)))
                      (nreverse chords))))))

(defun join-chords (line chords)
  "LINE with CHORDS, a list of (offset . chord), written in it in brackets;
SPLIT-CHORDS reads it back."
  (if (null chords)
      line
      (with-output-to-string (out)
        (let ((last 0))
          (dolist (chord (stable-sort (copy-list chords) #'< :key #'car))
            (let ((at (max last (min (car chord) (length line)))))
              (write-string line out :start last :end at)
              (format out "[~A]" (cdr chord))
              (setf last at)))
          (write-string line out :start last)))))

(defun snap-column (lyric column)
  "COLUMN, where a chord stands over LYRIC, or the start of the word beside
it: a chord sheet is not set to the letter, and no chord is played on the
space before a word, on its second letter or on the last of three or more."
  (flet ((space-p (at) (char= (char lyric at) #\Space)))
    (cond ((not (< 0 column (length lyric))) column)
          ((space-p column) (1+ column))
          ((and (not (space-p (1- column)))
                (or (= column 1) (space-p (- column 2))))
           (1- column))
          ((and (>= column 2)
                (notany #'space-p (list (1- column) (- column 2)))
                (or (= (1+ column) (length lyric)) (space-p (1+ column))))
           (min (+ column 2) (length lyric)))
          (t column))))

(defun inline-chord-lines (lines)
  "LINES, as a chord sheet sets them, with each line of nothing but chords
written into the line of words under it (JOIN-CHORDS), each chord where it
stands over the words; a chord line over no words is dropped.  The lines
come back trimmed."
  (loop for (line next) on lines
        for chords-p = (chord-line-p line)
        for words-p = (and chords-p next
                           (string/= (trim next) "")
                           (not (chord-line-p next))
                           (not (label-line (trim next))))
        with skip = nil
        if skip
          do (setf skip nil)
        else if words-p
               collect (let ((indent (or (position #\Space next :test #'char/=) 0))
                             (words (trim next))
                             (chords '()))
                         (cl-ppcre:do-matches (start end "\\S+" line)
                           (push (cons (snap-column words (max 0 (- start indent)))
                                       (subseq line start end))
                                 chords))
                         (setf skip t)
                         (join-chords words (nreverse chords)))
        else unless chords-p
               collect (trim line)))

(defun chorded-sections (sections)
  "SECTIONS, whose lines may have chords written in them, with the chords
taken out of the lines into each section's :CHORDS.  A line left with no
words is dropped, and so are a slide and a section left with no line."
  (loop for section in sections
        for pairs = (remove nil
                            (loop for slide in (section-slides section)
                                  collect (loop for line in slide
                                                for (text chords)
                                                  = (multiple-value-list (split-chords line))
                                                when (string/= text "")
                                                  collect (cons text chords))))
        when pairs
          collect (append
                   (loop for (key value) on section by #'cddr
                         unless (member key '(:slides :chords))
                           append (list key value))
                   (list :slides (mapcar (lambda (slide) (mapcar #'car slide)) pairs))
                   (and (some (lambda (slide) (some #'cdr slide)) pairs)
                        (list :chords (mapcar (lambda (slide) (mapcar #'cdr slide))
                                              pairs))))))

(defun section-chorded-slides (section)
  "The slides of SECTION with its chords written in their lines."
  (let ((chords (section-chords section)))
    (if (null chords)
        (section-slides section)
        (loop for slide in (section-slides section)
              for slide-chords in chords
              collect (loop for line in slide
                            for line-chords = (pop slide-chords)
                            collect (join-chords line line-chords))))))

(defparameter *sharp-notes* #("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))
(defparameter *flat-notes* #("C" "Db" "D" "Eb" "E" "F" "Gb" "G" "Ab" "A" "Bb" "B"))

(defun note-index (note)
  "How many semitones the note NOTE, \"F#\" or \"Bb\", is above C."
  (mod (+ (position (char note 0) "C D EF G A B")
          (cond ((= (length note) 1) 0)
                ((char= (char note 1) #\#) 1)
                (t -1)))
       12))

(defun transpose-chord (chord semitones &key flats)
  "CHORD, \"G\", \"Bm7\" or \"D/F#\", SEMITONES higher: every note it names
moved, and spelled with flats when FLATS and with sharps when not.  CHORD
itself when SEMITONES is no change, or CHORD names no note (N.C.)."
  (if (or (zerop (mod semitones 12)) (cl-ppcre:scan "^\\(?N\\.?C" chord))
      chord
      (cl-ppcre:regex-replace-all
       "[A-G][#b]?" chord
       (lambda (note)
         (aref (if flats *flat-notes* *sharp-notes*)
               (mod (+ (note-index note) semitones) 12)))
       :simple-calls t)))

(defun song-key (song)
  "The key SONG is written in, as \"D\" or \"Bm\": what its key tag says, or
else its first chord; NIL for a song without either."
  (or (second (find :key (song-tags song) :key #'first))
      (loop for section in (song-sections song)
            thereis (loop for slide in (section-chords section)
                          thereis (loop for line in slide
                                        thereis (loop for (nil . chord) in line
                                                      thereis (cl-ppcre:scan-to-strings
                                                               "[A-G][#b]?m?(?!aj)" chord)))))))

(defun flat-key-p (key)
  "True when the key KEY, \"F\" or \"Gm\", is written with flats."
  (let ((note (and key (cl-ppcre:scan-to-strings "^[A-G][#b]?" key))))
    (and note
         (member (mod (+ (note-index note)
                         ;; A minor key is written as the major a third above.
                         (if (cl-ppcre:scan "^[A-G][#b]?m(?!aj)" key) 3 0))
                      12)
                 '(5 10 3 8 1))
         t)))

(defun parse-tag (text)
  "The tag (category name note) that TEXT, as \"mass=Communion\" or
\"liturgy=Christmas {Dec 25}\", writes, or NIL."
  (cl-ppcre:register-groups-bind (category name note)
      ("^\\s*([A-Za-z-]+)\\s*=\\s*(.*?)\\s*(?:\\{(.*)\\})?\\s*$" text)
    (when (plusp (length name))
      (list (intern (string-upcase category) :keyword) name note))))

(defun label-line (line)
  "When LINE is a section label, its kind, number (or NIL) and label; else
NIL.  \"2.\" is a verse; a known word may come bare, bracketed or with a
colon (\"Chorus\", \"[Bridge 2]\", \"Refrain:\"); any other word needs the
colon (\"Women:\")."
  (or (cl-ppcre:register-groups-bind (number) ("^(\\d{1,2})\\.$" line)
        (list :verse (parse-integer number) "Verse"))
      (cl-ppcre:register-groups-bind (word number colon)
          ("^\\[?([A-Za-z][A-Za-z'.-]*(?: [A-Za-z'.-]+)?)\\s*(\\d{1,2})?\\]?(:)?$" line)
        (let ((kind (label-kind word)))
          (when (or colon (not (eq kind :other)))
            (list kind
                  (and number (parse-integer number))
                  (cond ((eq kind :other) word)
                        ((member word '("build" "coda" "ending" "end")
                                 :test #'string-equal)
                         (string-capitalize word))
                        (t (third (assoc kind *kinds*))))))))))

(defparameter *marking*
  (let ((strong "x\\s?\\d|\\d\\s?x\\b|v\\d*\\b|verse|ref|chorus|pre-?\\s?ch|bridge|build|tag\\b|instr|int\\b|intro|coda|outro|repeat")
        (word "x\\s?\\d+|\\d+\\s?x|\\d+|v\\d*|verse|ref|refr|refrain|chorus|pre-?\\s?ch(?:orus)?|bridge|build|tag|instr(?:umental)?|int(?:ro)?|coda|outro|ending|end|repeat|return|song|last|lines?|two|to|or|and|then"))
    (cl-ppcre:create-scanner
     (format nil "\\s*\\((?=[^)]*\\b(?:~A))(?:\\s*(?:~A)\\b\\.?\\s*[,;&/+-]?)+\\s*(?:\\)|$)"
             strong word)
     :case-insensitive-mode t))
  "Matches a mark for the musicians in a line of words: a parenthesis that
holds nothing but repeats and names of sections, as (x2), (Ref. x2),
(V1, V2, Ref.), (Pre-Chorus, Refrain 1), (Repeat last two lines).  Sung
words in parentheses, (echo) or (You are), are not matched.")

(defun replace-tracking (regex line replacement columns)
  "LINE with every match of REGEX replaced by the string REPLACEMENT, and
COLUMNS, positions in LINE, as the positions of the same places in the
result: one inside a match goes to where the replacement starts."
  (let ((edits '()))
    (cl-ppcre:do-matches (start end regex line)
      (push (cons start end) edits))
    (values (cl-ppcre:regex-replace-all regex line replacement)
            (mapcar (lambda (column)
                      (let ((moved column))
                        (loop for (start . end) in edits
                              do (cond ((<= end column)
                                        (incf moved (- (length replacement) (- end start))))
                                       ((< start column)
                                        (decf moved (- column start)))))
                        moved))
                    columns))))

(defun sung-line (line)
  "LINE without the marks for the musicians: \"Today (x2)\" -> \"Today\".
The chords written in LINE stay, each where it was."
  (multiple-value-bind (text chords) (split-chords line)
    (multiple-value-bind (sung offsets)
        (replace-tracking *marking* text "" (mapcar #'car chords))
      ;; SPLIT-CHORDS trims what JOIN-CHORDS wrote, and keeps the offsets right.
      (multiple-value-bind (sung chords)
          (split-chords (join-chords sung (mapcar #'cons offsets (mapcar #'cdr chords))))
        (join-chords sung chords)))))

(defun sung-lines (lines)
  "LINES without their marks, and without the lines that were only marks."
  (remove "" (mapcar #'sung-line lines) :test #'string=))

(defun sung-sections (sections)
  "SECTIONS without the marks in their lines; a slide and a section left
with no line are dropped."
  (loop for section in sections
        for slides = (remove nil (mapcar #'sung-lines (section-slides section)))
        when slides
          collect (let ((section (copy-list section)))
                    (setf (getf section :slides) slides)
                    section)))

(defun blocks (lines)
  "LINES split at its blank lines into non-empty lists of lines."
  (let ((blocks '()) (block '()))
    (dolist (line lines)
      (cond ((string= line "")
             (when block
               (push (nreverse block) blocks)
               (setf block '())))
            (t (push line block))))
    (when block
      (push (nreverse block) blocks))
    (nreverse blocks)))

(defun number-sections (sections)
  "SECTIONS with a number given to each that has none: the lowest its kind
has not used yet."
  (let ((used '()))
    (dolist (section sections)
      (when (section-number section)
        (push (cons (section-kind section) (section-number section)) used)))
    (mapcar (lambda (section)
              (if (section-number section)
                  section
                  (let ((number (loop for n from 1
                                      unless (member (cons (section-kind section) n)
                                                     used :test #'equal)
                                        return n)))
                    (push (cons (section-kind section) number) used)
                    (list* :number number
                           (loop for (k v) on section by #'cddr
                                 unless (eq k :number) append (list k v))))))
            sections)))

(defun parse-body (lines)
  "The sections that LINES, a song's text without its headers, write."
  (let ((sections '()) (label nil) (pending '()))
    (flet ((flush ()
             (let ((blocks (blocks (nreverse pending))))
               (setf pending '())
               (cond ((null blocks))
                     ((null label)
                      ;; Under no label: verses, one per block.
                      (dolist (block blocks)
                        (push (list :kind :verse :number nil :label "Verse"
                                    :slides (list block))
                              sections)))
                     ((eq (first label) :wordless))
                     (t
                      (push (list :kind (first label) :number (second label)
                                  :label (third label) :slides blocks)
                            sections))))))
      (dolist (line lines)
        (let ((found (label-line line)))
          (cond (found (flush) (setf label found))
                (t (push line pending)))))
      (flush))
    (number-sections (nreverse sections))))

(defun right-trim (string)
  (string-right-trim '(#\Space #\Tab #\Return #\Newline) string))

(defun parse-sections (lines)
  "The sections that LINES, a song's text without its headers, write, and as
a second value the keys of the sections that were written with nothing to
sing in them."
  (let* ((written (parse-body (inline-chord-lines lines)))
         (sung (chorded-sections (sung-sections written))))
    (values sung
            (set-difference (mapcar #'section-key written) (mapcar #'section-key sung)
                            :test #'string=))))

(defun parse-words (text)
  "The sections that TEXT, a song's text without its headers, writes."
  (values (parse-sections (mapcar #'right-trim (cl-ppcre:split "\\r?\\n" text)))))

(defun parse-song (text)
  "The SONG that TEXT writes in the text format.  Lenient: what is missing
is NIL, and SONG-PROBLEMS says what is wrong with the result."
  (let* ((lines (mapcar #'right-trim (cl-ppcre:split "\\r?\\n" text)))
         (song (make-song)))
    (loop while (and lines (string= (first lines) "")) do (pop lines))
    (loop for line = (and lines (trim (first lines)))
          while (and line (string/= line ""))
          do (multiple-value-bind (match groups)
                 (cl-ppcre:scan-to-strings "^([A-Za-z]+):\\s*(.*)$" line)
               (unless match (return))
               (let ((key (string-downcase (aref groups 0)))
                     (value (aref groups 1)))
                 (cond ((string= key "title") (setf (song-title song) value))
                       ((string= key "id") (setf (song-id song) value))
                       ((string= key "alt")
                        (setf (song-alt-titles song)
                              (append (song-alt-titles song) (split-list value ";"))))
                       ((member key '("writers" "writer" "author" "authors" "artist")
                                :test #'string=)
                        (setf (song-writers song) value))
                       ((string= key "copyright") (setf (song-copyright song) value))
                       ((string= key "ccli") (setf (song-ccli song) value))
                       ((string= key "number")
                        (setf (song-number song) (parse-integer value :junk-allowed t)))
                       ((string= key "page")
                        (setf (song-page song) (parse-integer value :junk-allowed t)))
                       ((string= key "order")
                        (setf (song-order song)
                              (mapcar #'string-upcase (split-list value "[\\s,]+"))))
                       ((string= key "tags")
                        (setf (song-tags song)
                              (append (song-tags song)
                                      (remove nil (mapcar #'parse-tag
                                                          (split-list value ";"))))))
                       ((string= key "reviewed")
                        (setf (song-reviewed song)
                              (and (member value '("yes" "y" "true") :test #'string-equal)
                                   t)))
                       ;; Not a header: the body starts here.
                       (t (return))))
               (pop lines)))
    ;; A section that was only marks is gone, from the order too.
    (multiple-value-bind (sung gone) (parse-sections lines)
      (setf (song-sections song) sung
            (song-order song) (remove-if (lambda (key) (member key gone :test #'string=))
                                         (song-order song))))
    (dolist (field '(song-title song-writers song-copyright song-ccli song-id))
      (when (equal (funcall field song) "")
        (funcall (fdefinition `(setf ,field)) nil song)))
    song))

(defun song-problems (song)
  "What keeps SONG from being shown, as a list of sentences; NIL when it is
fine."
  (let ((keys (mapcar #'section-key (song-sections song))))
    (append
     (unless (song-title song) (list "It has no Title: header."))
     (unless (song-sections song) (list "It has no words."))
     (loop for (key . rest) on keys
           when (member key rest :test #'string=)
             collect (format nil "Two sections are both ~A." key))
     (loop for key in (song-order song)
           unless (member key keys :test #'string=)
             collect (format nil "Order: names ~A, which the song does not have." key)))))

;;; Suggesting a structure

(defparameter *outline-kinds* '(:verse :refrain :pre-chorus :bridge :tag :ending)
  "The kinds a part of an outline can be given, as a form offers them.")

(defun kind-label (kind)
  "What a section of KIND is called: \"Refrain\", \"Pre-Chorus\"."
  (third (assoc kind *kinds*)))

(defun relabel (part kind)
  "PART, a section of an outline, as a section of KIND."
  (list :kind kind :number nil :label (kind-label kind) :slides (section-slides part)))

(defun words-key (lines)
  "LINES as they compare: their words in small letters, nothing else."
  (trim (string-downcase
         (cl-ppcre:regex-replace-all
          "\\W+" (format nil "~{~A~^ ~}" (mapcar #'split-chords lines)) " "))))

(defun outline (text)
  "A suggestion of how TEXT, the words of a song as pasted from anywhere,
is built: its distinct parts as a list of sections, and as a second value
the order they are sung in, a list of indices into the first.

Each block of lines between blank lines is a part.  A label line (\"Verse
2\", \"[Chorus]\", \"Bridge:\") names the block under it; a label with
nothing under it, and a block whose words came before, sing that part
again.  Of the parts under no label, those sung more than once are taken
for the refrain, the bridge and the tag, as far as the text names none, and
the rest for verses.  Marks for the musicians are left out (SUNG-LINE), and
so is what stands under Intro or Instrumental."
  (let ((parts '()) (places '()) (label nil) (pending '()))
    (labels ((add (kind number name block)
               (setf parts (append parts (list (list :kind kind :number number :label name
                                                     :slides (list block)))))
               (push (1- (length parts)) places))
             (take (block label)
               (let* ((key (words-key block))
                      (same (position-if
                             (lambda (part)
                               (and (string= key (words-key (first (section-slides part))))
                                    (or (null label)
                                        (null (section-kind part))
                                        (eq (section-kind part) (first label)))))
                             parts)))
                 (cond ((null same)
                        (add (first label) (second label) (third label) block))
                       (t
                        ;; A label on words that came unlabelled names them.
                        (when (and label (null (section-kind (nth same parts))))
                          (setf (nth same parts)
                                (list :kind (first label) :number (second label)
                                      :label (third label) :slides (list block))))
                        (push same places)))))
             (flush ()
               (let* ((lines (nreverse pending))
                      (apart (equal (first lines) ""))
                      (blocks (blocks lines)))
                 (setf pending '())
                 (cond ((null label)
                        (dolist (block blocks) (take block nil)))
                       ((eq (first label) :wordless)
                        (dolist (block (if apart blocks (rest blocks))) (take block nil)))
                       ((or (null blocks)
                            (and apart (find (first label) parts :key #'section-kind)))
                        ;; A label alone: that part is sung again here.
                        ;; Which it is shows when every part has its kind.
                        (push label places)
                        (dolist (block blocks) (take block nil)))
                       (blocks
                        (take (first blocks) label)
                        (dolist (block (rest blocks)) (take block nil)))))))
      (dolist (line (inline-chord-lines
                     (mapcar #'right-trim (cl-ppcre:split "\\r?\\n" text))))
        (let* ((sung (sung-line line))
               (found (label-line sung)))
          (cond (found (flush) (setf label found))
                ((string= line "") (push "" pending))
                ;; A line that was only a mark, or only chords, is no line,
                ;; and no break.
                ((string/= (split-chords sung) "") (push sung pending)))))
      (flush))
    (let ((free (remove-if (lambda (kind) (find kind parts :key #'section-kind))
                           '(:refrain :bridge :tag))))
      (setf parts (loop for part in parts
                        for index from 0
                        collect (cond ((section-kind part) part)
                                      ((and free (> (count index places) 1))
                                       (relabel part (pop free)))
                                      (t (relabel part :verse)))))
      (values parts
              (loop for place in (nreverse places)
                    for index = (if (integerp place)
                                    place
                                    (position-if
                                     (lambda (part)
                                       (and (eq (section-kind part) (first place))
                                            (or (null (second place))
                                                (eql (second place) (section-number part)))))
                                     parts))
                    when index collect index)))))

(defun outline-song (parts places &key title writers copyright tags)
  "The song of PARTS sung in the order PLACES (see OUTLINE), with its
credits and TAGS: the parts numbered, their chords taken out of their lines,
and an order when it is not the parts' own."
  (let* ((sections (chorded-sections (number-sections parts)))
         (keys (mapcar #'section-key sections))
         (order (loop for place in places collect (nth place keys))))
    (flet ((text (string)
             (let ((string (trim (or string ""))))
               (and (string/= string "") string))))
      (make-song :title (text title) :writers (text writers) :copyright (text copyright)
                 :sections sections
                 :tags tags
                 :order (and (not (equal order keys)) order)))))

;;; Writing

(defun print-tag (tag)
  (destructuring-bind (category name &optional note) tag
    (format nil "~(~A~)=~A~@[ {~A}~]" category name note)))

(defun print-sections (sections)
  "SECTIONS as the text format writes them under a song's headers, chords
and all; PARSE-SECTIONS reads it back."
  (with-output-to-string (out)
    (loop for (section . more) on sections
          do (if (eq (section-kind section) :verse)
                 (format out "~D.~%" (section-number section))
                 (format out "~A:~%" (section-name section)))
             (format out "~{~{~A~%~}~^~%~}" (section-chorded-slides section))
             (when more (terpri out)))))

(defun print-song (song)
  "SONG in the text format; PARSE-SONG reads it back."
  (with-output-to-string (out)
    (flet ((header (name value)
             (when value (format out "~A: ~A~%" name value))))
      (header "Title" (song-title song))
      (header "Id" (song-id song))
      (when (song-alt-titles song)
        (format out "Alt: ~{~A~^; ~}~%" (song-alt-titles song)))
      (header "Writers" (song-writers song))
      (header "Copyright" (song-copyright song))
      (header "CCLI" (song-ccli song))
      (header "Number" (song-number song))
      (header "Page" (song-page song))
      (when (song-order song)
        (format out "Order: ~{~A~^ ~}~%" (song-order song)))
      (when (song-tags song)
        (format out "Tags: ~{~A~^; ~}~%" (mapcar #'print-tag (song-tags song))))
      (when (song-reviewed song)
        (header "Reviewed" "yes")))
    (when (song-sections song)
      (terpri out)
      (write-string (print-sections (song-sections song)) out))))

(defun song-first-line (song)
  "The first line of SONG's words, or NIL."
  (first (first (section-slides (first (song-sections song))))))

(defun slug (string)
  "STRING in lowercase with each run of other than letters and digits made
one hyphen: \"O Come, All Ye Faithful\" -> \"o-come-all-ye-faithful\"."
  (string-trim "-" (cl-ppcre:regex-replace-all "[^a-z0-9]+" (string-downcase string) "-")))

;;; Slides

(defparameter *max-lines* 6
  "The most lines on one slide: a longer block of a section is split.")

(defun split-lines (lines &optional (max-lines *max-lines*))
  "LINES as a list of slides of at most MAX-LINES lines, as even as can be."
  (let* ((count (length lines))
         (parts (max 1 (ceiling count max-lines)))
         (size (ceiling count parts)))
    (loop for rest = lines then (nthcdr size rest)
          while rest
          collect (subseq rest 0 (min size (length rest))))))

(defun slide-key (slide) (getf slide :key))
(defun slide-label (slide) (getf slide :label))
(defun slide-lines (slide) (getf slide :lines))
(defun slide-part (slide) (getf slide :part))
(defun slide-of (slide) (getf slide :of))

(defun slide-chords (slide)
  "The chords over each line of SLIDE, lists of (offset . chord), or NIL for
a slide of a song without chords."
  (getf slide :chords))

(defun slide-group (slide)
  "The key that jumps to SLIDE: its section's key without the number, or the
verse's number."
  (getf slide :group))

(defun section-deck (section index)
  "The slides of SECTION, the INDEXth of its song."
  (let* ((chords (section-chords section))
         (slides (loop for block in (section-slides section)
                       ;; A song stored before PARSE-SONG left the marks out;
                       ;; one with chords was stored since.
                       append (split-lines (if chords block (sung-lines block)))))
         (slide-chords (loop for block in chords append (split-lines block)))
         (of (length slides)))
    (loop for lines in slides
          for part from 1
          collect (list* :key (section-key section)
                        :group (if (eq (section-kind section) :verse)
                                   (section-key section)
                                   (kind-letter (section-kind section)))
                        :label (section-name section)
                        :section index
                        :rank (or (section-number section) 1)
                        :part part :of of :lines lines
                        (and chords (list :chords (pop slide-chords)))))))

(defun song-deck (song)
  "The slides of SONG in the order it is sung, as a list of plists (:KEY
:GROUP :LABEL :SECTION :PART :OF :LINES, and :CHORDS for a song that has
them): its Order:, or else as written,
and then any section the order left out."
  (let* ((sections (song-sections song))
         (keys (mapcar #'section-key sections))
         (order (remove-if-not (lambda (key) (member key keys :test #'string=))
                               (or (song-order song) keys)))
         (order (append order (set-difference keys order :test #'string=))))
    (loop for key in order
          for index = (position key keys :test #'string=)
          append (section-deck (nth index sections) index))))

(defun deck-jump (deck index group)
  "The index in DECK that pressing GROUP (\"2\", \"R\", \"B\") leads to from
INDEX, or NIL when the song has no such section.

The first press shows the group's first slide, a press while one of its
slides is showing the next of them (the second half of a long refrain,
Refrain 2), round again from the last.  Of the places where that slide stands
in the deck, the next from INDEX on is taken, so that what follows it is
what follows it in the song."
  (let* ((group (if (string-equal group "C") "R" (string-upcase group)))
         (members (remove-duplicates
                   (remove-if-not (lambda (slide) (string= (slide-group slide) group))
                                  deck)
                   :test #'equal
                   :key (lambda (slide) (list (getf slide :section) (slide-part slide)))
                   :from-end t))
         (members (stable-sort (copy-list members) #'<
                               :key (lambda (slide) (getf slide :rank))))
         (current (and index (nth index deck))))
    (when members
      (let* ((here (position-if (lambda (slide)
                                  (and current
                                       (eql (getf slide :section) (getf current :section))
                                       (eql (slide-part slide) (slide-part current))))
                                members))
             (target (nth (if here (mod (1+ here) (length members)) 0) members))
             (same (lambda (slide)
                     (and (eql (getf slide :section) (getf target :section))
                          (eql (slide-part slide) (slide-part target))))))
        (or (position-if same deck :start (or index 0))
            (position-if same deck))))))

(defun same-slide-p (slide other)
  "True when SLIDE and OTHER are the same slide of the song, wherever in its
deck they stand."
  (and (eql (getf slide :section) (getf other :section))
       (eql (slide-part slide) (slide-part other))))

(defun deck-chart (deck)
  "The slides of DECK once each, in the order they first come: the song as a
musician reads it, the refrain written out once."
  (remove-duplicates deck :test #'same-slide-p :from-end t))

(defun deck-next (deck index slide)
  "The index in DECK where SLIDE next stands after INDEX, round again from
the start; NIL when DECK has no such slide."
  (or (position slide deck :test #'same-slide-p
                           :start (min (1+ (or index -1)) (length deck)))
      (position slide deck :test #'same-slide-p)))
