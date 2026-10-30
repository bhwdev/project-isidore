;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/slack-bot/nlp
  (:use #:common-lisp)
  (:import-from #:alexandria)
  (:import-from #:cl-ppcre)
  (:export
   ;; documents, tokens, spans
   #:doc #:doc-p #:doc-text #:doc-tokens #:doc-ents
   #:doc-token #:doc-length
   #:token #:token-p #:token-index #:token-text #:token-lower
   #:token-shape #:token-start #:token-end #:token-flags #:token-flag-p
   #:span #:make-span #:span-p #:span-doc #:span-start #:span-end #:span-label
   #:span-data #:span-text #:span-tokens #:span-length #:spans-overlap-p
   #:span-covers-p
   ;; pipeline
   #:tokenize #:make-doc #:word-shape #:*max-input-length*
   ;; matcher
   #:matcher #:make-matcher #:add-pattern #:run-matcher #:match-tokens
   ;; entity ruler
   #:entity-ruler #:make-entity-ruler #:add-entity-patterns #:apply-entity-ruler
   ;; phrase matcher
   #:phrase-matcher #:make-phrase-matcher #:run-phrase-matcher)
  (:documentation
   "A small spaCy-style NLP layer.

The pipeline is TOKENIZE -> rule-based matching:

  - DOC / TOKEN / SPAN structs mirror spaCy's Doc, Token and Span.  A span's
    END is exclusive.  Tokens carry the text, a lowercased copy, a
    spaCy-style SHAPE (\"XX\", \"dd:ddxx\", \"Xxxxx\") and boolean FLAGS.
  - MATCHER runs token-attribute patterns (lists of plists such as
    (:lower \"back\") (:lower (\"by\" \"at\") :op \"?\")) with the operators
    \"?\", \"*\" and \"+\", returning leftmost-longest spans.
  - ENTITY-RULER applies labelled pattern sets with priorities and assigns
    non-overlapping entity spans to (DOC-ENTS doc), like spaCy's EntityRuler
    combined with filter_spans.
  - PHRASE-MATCHER matches a gazetteer of literal phrases (spaCy's
    PhraseMatcher), used for destinations already present on the sheet."))

(in-package #:project-isidore/slack-bot/nlp)

;;; ---------------------------------------------------------------------------
;;; Tokens, documents, spans
;;; ---------------------------------------------------------------------------

(defparameter *max-input-length* 1000
  "Inputs longer than this many characters are truncated before tokenizing.")

(defparameter *number-words*
  '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
    "eleven" "twelve")
  "Word forms that count as numbers for the :LIKE-NUM token flag.")

(defstruct (token (:constructor %make-token))
  "One token of a DOC.  START/END are character offsets into DOC-TEXT.  FLAGS is a plist of booleans:
:IS-ALPHA :IS-UPPER :IS-TITLE :IS-PUNCT :LIKE-NUM."
  index text lower shape start end flags)

(defstruct (doc (:constructor %make-doc))
  "A tokenized text.  TOKENS is a simple-vector.  ENTS are the entity spans
assigned by an ENTITY-RULER, sorted by start."
  text tokens ents)

(defstruct span
  "A contiguous run of tokens [START, END) of DOC with a LABEL keyword and a
DATA plist (:value, :priority, :trim-leading, ...)."
  doc start end label data)

(defmethod print-object ((doc doc) stream)
  (print-unreadable-object (doc stream :type t)
    (format stream "~s ~d token~:p" (doc-text doc) (length (doc-tokens doc)))))

(defmethod print-object ((span span) stream)
  (print-unreadable-object (span stream :type t)
    (format stream "~a [~d,~d) ~s" (span-label span) (span-start span)
            (span-end span) (span-text span))))

(defun word-shape (text)
  "Return the spaCy-style shape of TEXT: uppercase letters become X, lowercase
x, digits d, everything else is kept; runs longer than four are collapsed."
  (with-output-to-string (out)
    (let ((prev nil) (run 0))
      (loop for ch across text
            for cls = (cond ((digit-char-p ch) #\d)
                            ((upper-case-p ch) #\X)
                            ((lower-case-p ch) #\x)
                            (t ch))
            do (if (eql cls prev) (incf run) (setf run 1 prev cls))
               (when (<= run 4) (write-char cls out))))))

(defun compute-token-flags (text)
  (let ((lower (string-downcase text))
        (alpha (some #'alpha-char-p text)))
    (list :is-alpha (and (plusp (length text)) (every #'alpha-char-p text))
          :is-upper (and alpha
                         (every (lambda (c) (or (not (alpha-char-p c)) (upper-case-p c)))
                                text))
          :is-title (and alpha
                         (upper-case-p (char text 0))
                         (every (lambda (c) (or (not (alpha-char-p c)) (lower-case-p c)))
                                (subseq text 1)))
          :is-punct (and (plusp (length text)) (notany #'alphanumericp text))
          :like-num (or (and (plusp (length text)) (every #'digit-char-p text))
                        (and (member lower *number-words* :test #'string=) t)))))

(defun make-token (index text start end)
  (%make-token :index index :text text :lower (string-downcase text)
               :shape (word-shape text) :start start :end end
               :flags (compute-token-flags text)))

(defun token-flag-p (token flag)
  "Return the boolean FLAG (:is-upper, :like-num, ...) of TOKEN."
  (getf (token-flags token) flag))

(defun doc-length (doc)
  (length (doc-tokens doc)))

(defun doc-token (doc index)
  "Return the token at INDEX, or NIL when out of range."
  (let ((tokens (doc-tokens doc)))
    (when (and (<= 0 index) (< index (length tokens)))
      (aref tokens index))))

(defun span-length (span)
  (- (span-end span) (span-start span)))

(defun span-tokens (span)
  "Return the tokens covered by SPAN as a list."
  (let ((tokens (doc-tokens (span-doc span))))
    (loop for i from (span-start span) below (span-end span)
          collect (aref tokens i))))

(defun span-text (span)
  "Return the original text covered by SPAN (spacing and case preserved)."
  (let ((doc (span-doc span)))
    (if (>= (span-start span) (span-end span))
        ""
        (let ((tokens (doc-tokens doc)))
          (subseq (doc-text doc)
                  (token-start (aref tokens (span-start span)))
                  (token-end (aref tokens (1- (span-end span)))))))))

(defun spans-overlap-p (a b)
  (and (< (span-start a) (span-end b))
       (< (span-start b) (span-end a))))

(defun span-covers-p (span index)
  (and (<= (span-start span) index) (< index (span-end span))))

;;; ---------------------------------------------------------------------------
;;; Tokenizer
;;; ---------------------------------------------------------------------------

(defparameter *token-scanner*
  (cl-ppcre:create-scanner
   (concatenate 'string
                "\\d{4}-\\d{2}-\\d{2}"                         ; ISO date
                "|\\d{1,2}:\\d{2}(?:[ap]m|[ap]\\.m\\.?)?"     ; 12:30, 12:30pm, 8:30a.m.
                "|\\d{1,2}(?:[ap]m|[ap]\\.m\\.?)"             ; 7pm, 7p.m.
                "|\\d{1,2}(?:st|nd|rd|th)"                     ; 11th
                "|\\d{1,2}/\\d{1,2}(?:/\\d{2,4})?"             ; 4/22
                "|\\d+"                                        ; numbers
                "|[ap]\\.m\\.?"                                ; a.m., p.m
                "|[a-z]+(?:'[a-z]+)*"                          ; words, I'd, Mary's
                "|[,.?!;:()\\[\\]\"-]"                          ; punctuation
                "|\\S")                                        ; anything else
   :case-insensitive-mode t)
  "Ordered token alternatives; at a given position the first alternative that
matches wins, so specific shapes (times, ordinals) beat plain numbers/words.")

(defun normalize-text (text)
  "Map typographic quotes/dashes to ASCII, whitespace to spaces, drop
non-graphic characters and truncate to *MAX-INPUT-LENGTH*."
  (let ((s (if (> (length text) *max-input-length*)
               (subseq text 0 *max-input-length*)
               text)))
    (with-output-to-string (out)
      (loop for ch across s
            for code = (char-code ch)
            do (cond ((member code '(#x2018 #x2019 #x02BC)) (write-char #\' out))
                     ((member code '(#x201C #x201D)) (write-char #\" out))
                     ((member code '(#x2013 #x2014 #x2212)) (write-char #\- out))
                     ((or (= code #xA0) (member ch '(#\Tab #\Newline #\Return #\Page)))
                      (write-char #\Space out))
                     ((graphic-char-p ch) (write-char ch out))
                     (t nil))))))

(defun tokenize (text)
  "Tokenize TEXT.  Returns (values TOKENS NORMALIZED-TEXT) where TOKENS is a
simple-vector of TOKEN structs whose offsets refer to NORMALIZED-TEXT."
  (let ((norm (normalize-text text))
        (tokens '())
        (i 0))
    (cl-ppcre:do-matches (s e *token-scanner* norm)
      (push (make-token i (subseq norm s e) s e) tokens)
      (incf i))
    (values (coerce (nreverse tokens) 'simple-vector) norm)))

(defun make-doc (text)
  "Tokenize TEXT into a fresh DOC."
  (multiple-value-bind (tokens norm) (tokenize text)
    (%make-doc :text norm :tokens tokens :ents nil)))

;;; ---------------------------------------------------------------------------
;;; Matcher (token patterns)
;;; ---------------------------------------------------------------------------

(defstruct (matcher (:constructor make-matcher ()))
  "Holds pattern entries of the form (LABEL SPECS DATA)."
  (patterns '()))

(defun compile-spec (spec)
  "Return a copy of SPEC with :REGEX values compiled to scanners."
  (loop for (key val) on spec by #'cddr
        append (list key (if (and (eq key :regex) (stringp val))
                             (cl-ppcre:create-scanner val :case-insensitive-mode t)
                             val))))

(defun add-pattern (matcher label pattern &rest data)
  "Register PATTERN (a list of token specs) under LABEL.  DATA is a plist
attached to every resulting span.  Returns MATCHER."
  (setf (matcher-patterns matcher)
        (append (matcher-patterns matcher)
                (list (list label (mapcar #'compile-spec pattern) data))))
  matcher)

(defun spec-value-matches (expected actual &key (test #'string-equal))
  (if (listp expected)
      (and (member actual expected :test test) t)
      (and (funcall test actual expected) t)))

(defun match-tokens (spec token)
  "Return T when TOKEN satisfies every attribute constraint in SPEC.
Supported keys: :lower/:in, :not-in, :text, :shape, :regex,
:is-alpha, :is-upper, :is-title, :is-punct, :like-num.  :op is ignored here."
  (loop for (key val) on spec by #'cddr
        always (case key
                 (:op t)
                 ((:lower :in) (spec-value-matches val (token-lower token)))
                 (:not-in (not (spec-value-matches val (token-lower token))))
                 (:text (spec-value-matches val (token-text token) :test #'string=))
                 (:shape (spec-value-matches val (token-shape token) :test #'string=))
                 (:regex (and (cl-ppcre:scan val (token-text token)) t))
                 ((:is-alpha :is-upper :is-title :is-punct :like-num)
                  (eq (and (token-flag-p token key) t) (and val t)))
                 (t (error "Unknown token attribute ~s in pattern spec ~s" key spec)))))

(defun match-from (specs tokens i)
  "Return the largest end index (exclusive) at which SPECS match TOKENS
starting at I, or NIL when they do not match."
  (if (null specs)
      i
      (let* ((spec (first specs))
             (op (getf spec :op))
             (n (length tokens))
             (best nil))
        (flet ((try-rest (j)
                 (let ((e (match-from (rest specs) tokens j)))
                   (when (and e (or (null best) (> e best)))
                     (setf best e)))))
          (cond ((null op)
                 (when (and (< i n) (match-tokens spec (aref tokens i)))
                   (try-rest (1+ i))))
                ((string= op "?")
                 (try-rest i)
                 (when (and (< i n) (match-tokens spec (aref tokens i)))
                   (try-rest (1+ i))))
                ((string= op "*")
                 (try-rest i)
                 (loop for j from i below n
                       while (match-tokens spec (aref tokens j))
                       do (try-rest (1+ j))))
                ((string= op "+")
                 (loop for j from i below n
                       while (match-tokens spec (aref tokens j))
                       do (try-rest (1+ j))))
                (t (error "Unknown pattern operator ~s" op))))
        best)))

(defun run-matcher (matcher doc)
  "Run every pattern of MATCHER over DOC.  Returns spans sorted by start;
per pattern, matches are leftmost-longest and do not overlap each other."
  (let ((tokens (doc-tokens doc))
        (results '()))
    (dolist (entry (matcher-patterns matcher))
      (destructuring-bind (label specs data) entry
        (let ((i 0)
              (n (length tokens)))
          (loop while (< i n)
                do (let ((e (match-from specs tokens i)))
                     (if (and e (> e i))
                         (progn
                           (push (make-span :doc doc :start i :end e
                                            :label label :data data)
                                 results)
                           (setf i e))
                         (incf i)))))))
    (stable-sort (nreverse results) #'< :key #'span-start)))

;;; ---------------------------------------------------------------------------
;;; Entity ruler
;;; ---------------------------------------------------------------------------

(defstruct (entity-ruler (:constructor make-entity-ruler ()))
  "Labelled, prioritised pattern sets that produce non-overlapping entities."
  (matcher (make-matcher)))

(defun add-entity-patterns (ruler label priority patterns &key value)
  "Register PATTERNS under LABEL with PRIORITY (lower wins).  Each pattern is a
list of token specs, optionally headed by (:trim-leading N ...) to drop the
first N matched tokens (cue words) from the resulting entity.  VALUE is
attached as :VALUE in the span data.  Returns RULER."
  (dolist (pattern patterns)
    (let ((trim 0)
          (specs pattern))
      (when (eq (first pattern) :trim-leading)
        (setf trim (second pattern)
              specs (cddr pattern)))
      (add-pattern (entity-ruler-matcher ruler) label specs
                   :priority priority :value value :trim-leading trim)))
  ruler)

(defun trim-span (span)
  (let ((trim (or (getf (span-data span) :trim-leading) 0)))
    (if (plusp trim)
        (make-span :doc (span-doc span) :start (+ (span-start span) trim)
                   :end (span-end span) :label (span-label span)
                   :data (span-data span))
        span)))

(defun span-priority (span)
  (or (getf (span-data span) :priority) 100))

(defun entity-candidate< (a b)
  "Order: lower priority number first, then longer span, then earlier start."
  (let ((pa (span-priority a)) (pb (span-priority b)))
    (cond ((< pa pb) t)
          ((> pa pb) nil)
          ((> (span-length a) (span-length b)) t)
          ((< (span-length a) (span-length b)) nil)
          (t (< (span-start a) (span-start b))))))

(defun apply-entity-ruler (ruler doc &key extra-spans)
  "Assign entities to DOC: run the ruler's patterns, add EXTRA-SPANS (e.g. from
a PHRASE-MATCHER), then keep candidates greedily by priority and length so no
two entities overlap.  Sets and returns (DOC-ENTS DOC), sorted by start."
  (let* ((candidates (mapcar #'trim-span
                             (append (run-matcher (entity-ruler-matcher ruler) doc)
                                     extra-spans)))
         (sorted (stable-sort (copy-list candidates) #'entity-candidate<))
         (chosen '()))
    (dolist (c sorted)
      (when (and (> (span-end c) (span-start c))
                 (notany (lambda (x) (spans-overlap-p x c)) chosen))
        (push c chosen)))
    (setf (doc-ents doc) (sort chosen #'< :key #'span-start))))

;;; ---------------------------------------------------------------------------
;;; Phrase matcher (gazetteer)
;;; ---------------------------------------------------------------------------

(defstruct (phrase-matcher (:constructor %make-phrase-matcher))
  "ENTRIES is a list of (KEY-TOKENS . ORIGINAL-STRING)."
  entries label)

(defun phrase-key (text)
  "Comparison key for gazetteer matching: TEXT lowercased with everything but
letters and digits removed (\"Mary's\" and \"marys\" both give \"marys\").
Returns NIL for punctuation-only text."
  (let ((key (remove-if-not #'alphanumericp (string-downcase text))))
    (and (plusp (length key)) key)))

(defun phrase-key-tokens (string)
  "Comparison keys of the word tokens of STRING; punctuation tokens dropped."
  (loop for tok across (tokenize string)
        for key = (phrase-key (token-text tok))
        when key collect key))

(defun make-phrase-matcher (strings &key (label :phrase))
  "Build a matcher for the literal phrases STRINGS (case- and
punctuation-insensitive).  Matches carry :VALUE = the original string."
  (%make-phrase-matcher
   :label label
   :entries (loop for s in strings
                  for keys = (phrase-key-tokens s)
                  when keys collect (cons keys s))))

(defun phrase-match-at (keys tokens i)
  "Return the end index when KEYS match TOKENS from I (comparing PHRASE-KEYs,
so apostrophes and punctuation do not matter; punctuation tokens between
words are skipped), else NIL."
  (let ((j i)
        (n (length tokens)))
    (dolist (k keys j)
      (loop while (and (< j n) (null (phrase-key (token-text (aref tokens j)))))
            do (incf j))
      (if (and (< j n) (string= k (phrase-key (token-text (aref tokens j)))))
          (incf j)
          (return nil)))))

(defun run-phrase-matcher (pm doc)
  "Return spans of DOC matching the matcher's phrases; the longest phrase wins
at each position."
  (let* ((tokens (doc-tokens doc))
         (n (length tokens))
         (results '())
         (i 0))
    (loop while (< i n)
          do (let ((best nil))
               (dolist (entry (phrase-matcher-entries pm))
                 (let ((e (phrase-match-at (car entry) tokens i)))
                   (when (and e (or (null best) (> e (car best))))
                     (setf best (cons e (cdr entry))))))
               (if best
                   (progn
                     (push (make-span :doc doc :start i :end (car best)
                                      :label (phrase-matcher-label pm)
                                      :data (list :value (cdr best)))
                           results)
                     (setf i (car best)))
                   (incf i))))
    (nreverse results)))
