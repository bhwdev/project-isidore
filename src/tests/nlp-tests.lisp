;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/nlp-tests
  (:use #:common-lisp #:parachute)
  (:local-nicknames (#:nlp #:project-isidore/nlp))
  (:import-from #:bordeaux-threads)
  (:documentation
   "Parachute tests for project-isidore/nlp: tokenizer, langutils tagging
bridge (parity, chunks, thread safety), Matcher, EntityRuler and
PhraseMatcher."))

(in-package #:project-isidore/tests/nlp-tests)

(defun token-texts (doc)
  (map 'list #'nlp:token-text (nlp:doc-tokens doc)))

(defun token-named (doc text)
  (find text (nlp:doc-tokens doc) :key #'nlp:token-text :test #'string=))

(defun spans-with (spans label)
  (mapcar #'nlp:span-text
          (remove-if-not (lambda (s) (eq (nlp:span-label s) label)) spans)))

;;; --- Tokenizer ---

(define-test tokenizer
  (define-test meridiem-and-clock-forms
    (is equal '("for" "pool" "for" "MM" "at" "9:30" "a.m." "to" "11:30" "a.m.")
        (token-texts (nlp:make-doc "for pool for MM at 9:30 a.m. to 11:30 a.m.")))
    (is equal '("from" "12:30pm" "to" "2:30pm" "for" "GG" ".")
        (token-texts (nlp:make-doc "from 12:30pm to 2:30pm for GG.")))
    ;; the sentence-final period is not swallowed by the hour+meridiem token
    (is equal '("at" "7pm" ".") (token-texts (nlp:make-doc "at 7pm.")))
    (is equal '("9:30a.m." "and" "7p.m") (token-texts (nlp:make-doc "9:30a.m. and 7p.m"))))

  (define-test ordinals-dates-and-punctuation
    (is equal '("April" "11th" ",") (token-texts (nlp:make-doc "April 11th,")))
    (is equal '("I'd" "like") (token-texts (nlp:make-doc "I'd like")))
    (is equal '("(" "BW" ")") (token-texts (nlp:make-doc "(BW)")))
    (is equal '("back" "around" "11" "?") (token-texts (nlp:make-doc "back around 11?")))
    (is equal '("7" "-" "9pm") (token-texts (nlp:make-doc "7-9pm")))
    (is equal '("4/22" "2026-09-08" "Thursday's")
        (token-texts (nlp:make-doc "4/22 2026-09-08 Thursday's")))
    (is equal '("CG" "&" "BW") (token-texts (nlp:make-doc "CG & BW"))))

  (define-test offsets-round-trip
    (let ((doc (nlp:make-doc "Can BW take the DGH to the dentist tomorrow, leaving at 8:30?")))
      (loop for tok across (nlp:doc-tokens doc)
            for i from 0
            do (is = i (nlp:token-index tok))
               (is string= (nlp:token-text tok)
                   (subseq (nlp:doc-text doc) (nlp:token-start tok) (nlp:token-end tok))))))

  (define-test typographic-normalisation
    (let ((text (format nil "I~cd like ~c ~cx~c" (code-char #x2019) (code-char #x2014)
                        (code-char #x201C) (code-char #x201D))))
      (is equal '("I'd" "like" "-" "\"" "x" "\"") (token-texts (nlp:make-doc text))))
    ;; tabs/newlines become spaces; control characters vanish
    (is equal '("a" "b" "c")
        (token-texts (nlp:make-doc (format nil "a~cb~cc~c" #\Tab #\Newline (code-char 7))))))

  (define-test truncation
    (let ((nlp:*max-input-length* 10))
      (is = 2 (length (nlp:doc-tokens (nlp:make-doc "abcde fghij klmno"))))))

  (define-test empty-and-blank
    (is = 0 (nlp:doc-length (nlp:make-doc "")))
    (is = 0 (nlp:doc-length (nlp:make-doc "   "))))

  (define-test shapes
    (is string= "XX" (nlp:word-shape "CG"))
    (is string= "dd:ddxx" (nlp:word-shape "12:30pm"))
    (is string= "Xxxxx" (nlp:word-shape "Tuesday"))
    (is string= "x.x." (nlp:word-shape "a.m."))
    (is string= "dddd" (nlp:word-shape "20260908"))
    (is string= "" (nlp:word-shape "")))

  (define-test flags
    (let* ((doc (nlp:make-doc "seven 7 CG Tuesday , swimming"))
           (toks (nlp:doc-tokens doc)))
      (true (nlp:token-flag-p (aref toks 0) :like-num))
      (true (nlp:token-flag-p (aref toks 1) :like-num))
      (false (nlp:token-flag-p (aref toks 2) :like-num))
      (true (nlp:token-flag-p (aref toks 2) :is-upper))
      (true (nlp:token-flag-p (aref toks 3) :is-title))
      (false (nlp:token-flag-p (aref toks 3) :is-upper))
      (true (nlp:token-flag-p (aref toks 4) :is-punct))
      (false (nlp:token-flag-p (aref toks 5) :is-upper))
      (true (nlp:token-flag-p (aref toks 5) :is-alpha))
      (is string= "seven" (nlp:token-lower (aref toks 0)))
      (is string= "cg" (nlp:token-lower (aref toks 2))))))

;;; --- Tagging bridge ---

(define-test tagging
  (define-test parity-and-tags
    (let ((doc (nlp:analyze "I would like to book a van for swimming next Tuesday for CG at 7 pm")))
      (true (nlp:doc-tags-p doc))
      (true (every (lambda (tk) (keywordp (nlp:token-pos tk))) (nlp:doc-tokens doc)))
      (false (some (lambda (tk) (eq :unk (nlp:token-pos tk))) (nlp:doc-tokens doc)))
      (is eq :nnp (nlp:token-pos (token-named doc "Tuesday")))
      (is eq :nnp (nlp:token-pos (token-named doc "CG")))
      (is eq :vbg (nlp:token-pos (token-named doc "swimming")))
      (is eq :cd (nlp:token-pos (token-named doc "7")))))

  (define-test empty-input
    (let ((doc (nlp:analyze "")))
      (is = 0 (nlp:doc-length doc))
      (true (nlp:doc-tags-p doc))
      (is equal '() (nlp:doc-chunks doc))))

  (define-test untagged-doc
    (let ((doc (nlp:analyze "for pool" :tag nil)))
      (false (nlp:doc-tags-p doc))
      (true (every (lambda (tk) (eq :unk (nlp:token-pos tk))) (nlp:doc-tokens doc)))))

  (define-test noun-chunks
    (let* ((doc (nlp:analyze "Please add GG to the SHMS trip on Thursday"))
           (chunks (nlp:doc-chunks doc)))
      (true (find "the SHMS trip" chunks :key #'nlp:span-text :test #'string=))
      (true (every (lambda (c) (eq :nx (nlp:span-label c))) chunks))
      (dolist (c chunks)
        (true (< (nlp:span-start c) (nlp:span-end c)))
        (true (<= (nlp:span-end c) (nlp:doc-length doc))))
      ;; sorted by start
      (is equal (mapcar #'nlp:span-start chunks)
          (sort (mapcar #'nlp:span-start chunks) #'<))))

  (define-test span-helpers
    (let* ((doc (nlp:analyze "Please add GG to the SHMS trip on Thursday"))
           (a (nlp:make-span :doc doc :start 4 :end 7 :label :x))
           (b (nlp:make-span :doc doc :start 6 :end 8 :label :y))
           (c (nlp:make-span :doc doc :start 7 :end 9 :label :z)))
      (is string= "the SHMS trip" (nlp:span-text a))
      (is = 3 (nlp:span-length a))
      (is equal '("the" "SHMS" "trip") (mapcar #'nlp:token-text (nlp:span-tokens a)))
      (true (nlp:spans-overlap-p a b))
      (false (nlp:spans-overlap-p a c))
      (true (nlp:span-covers-p a 5))
      (false (nlp:span-covers-p a 7))
      (is string= "" (nlp:span-text (nlp:make-span :doc doc :start 2 :end 2)))))

  (define-test thread-safety
    ;; Eight threads tag different sentences concurrently; every result must
    ;; equal the sequential result, which fails without *NLP-LOCK*.
    (let* ((sentences '("I would like to book a van for swimming next Tuesday for CG"
                        "Can BW and MM take the DGH to the dentist tomorrow morning"
                        "Please add GG to the SHMS trip on Thursday"
                        "reserve the Chevy for a haircut from 12:30pm to 2:30pm for GG"
                        "We need the van Friday going to Saint Marys church for the ordination"
                        "hey can you book swimming Tues 7-9pm for CG and BW"
                        "The van is needed by CG for the airport on Monday from 6 am to 9 am"
                        "CG needs a ride to the airport tomorrow morning departing 6am"))
           (tags-of (lambda (s) (map 'list #'nlp:token-pos (nlp:doc-tokens (nlp:analyze s)))))
           (expected (mapcar tags-of sentences))
           (results (make-array (length sentences) :initial-element :unset))
           (threads (loop for s in sentences
                          for i from 0
                          collect (let ((s s) (i i))
                                    (bt:make-thread
                                     (lambda ()
                                       (setf (aref results i)
                                             (handler-case
                                                 (let (last)
                                                   (dotimes (k 20 last)
                                                     (setf last (funcall tags-of s))))
                                               (error (e)
                                                 (list :error (princ-to-string e))))))
                                     :name "nlp-thread-safety-test")))))
      (mapc #'bt:join-thread threads)
      (loop for exp in expected
            for i from 0
            do (is equal exp (aref results i) "thread ~d" i)))))

;;; --- Matcher ---

(define-test matcher
  (define-test optional-op
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "be back by 10 or back at 11 or back")))
      (nlp:add-pattern m :ret '((:lower "back") (:lower ("by" "at") :op "?")))
      (is equal '("back by" "back at" "back")
          (mapcar #'nlp:span-text (nlp:run-matcher m doc)))))

  (define-test plus-op-greedy
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "with MM DM CG and BW")))
      (nlp:add-pattern m :pax '((:lower "with") (:shape "XX" :op "+")))
      (is equal '("with MM DM CG") (mapcar #'nlp:span-text (nlp:run-matcher m doc)))))

  (define-test star-op
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "the big red van and the van")))
      (nlp:add-pattern m :veh '((:lower "the") (:lower ("big" "red") :op "*") (:lower "van")))
      (is equal '("the big red van" "the van")
          (mapcar #'nlp:span-text (nlp:run-matcher m doc)))))

  (define-test zero-width-patterns-produce-nothing
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "a b c")))
      (nlp:add-pattern m :none '((:lower "zzz" :op "*")))
      (nlp:add-pattern m :none2 '((:lower "zzz" :op "?")))
      (is equal '() (nlp:run-matcher m doc))))

  (define-test leftmost-longest-non-overlapping
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "a b a b b a")))
      (nlp:add-pattern m :ab '((:lower "a") (:lower "b" :op "*")))
      (is equal '("a b" "a b b" "a") (mapcar #'nlp:span-text (nlp:run-matcher m doc)))))

  (define-test attribute-kinds
    (let ((m (nlp:make-matcher))
          (doc (nlp:analyze "CG leaves at 7 pm on Tuesday")))
      (nlp:add-pattern m :time '((:regex "^\\d{1,2}$") (:lower ("am" "pm"))))
      (nlp:add-pattern m :pax '((:shape "XX" :is-upper t)))
      (nlp:add-pattern m :day '((:pos :nnp :not-in ("cg"))))
      (nlp:add-pattern m :num '((:like-num t)))
      (nlp:add-pattern m :title '((:is-title t)))
      (nlp:add-pattern m :exact '((:text "CG")))
      (let ((spans (nlp:run-matcher m doc)))
        (is equal '("7 pm") (spans-with spans :time))
        (is equal '("CG") (spans-with spans :pax))
        (is equal '("Tuesday") (spans-with spans :day))
        (is equal '("7") (spans-with spans :num))
        (is equal '("Tuesday") (spans-with spans :title))
        (is equal '("CG") (spans-with spans :exact))
        ;; results are sorted by start
        (is equal (mapcar #'nlp:span-start spans)
            (sort (mapcar #'nlp:span-start spans) #'<)))))

  (define-test case-insensitive-lower-and-regex
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "DGH Tues 7PM")))
      (nlp:add-pattern m :veh '((:lower "dgh")))
      (nlp:add-pattern m :day '((:in ("TUES" "wed"))))
      (nlp:add-pattern m :time '((:regex "^\\d{1,2}pm$")))
      (is = 3 (length (nlp:run-matcher m doc)))))

  (define-test label-and-data
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "take the DGH")))
      (nlp:add-pattern m :vehicle '((:lower "dgh")) :value :dgh :priority 10)
      (let ((s (first (nlp:run-matcher m doc))))
        (is eq :vehicle (nlp:span-label s))
        (is eq :dgh (getf (nlp:span-data s) :value))
        (is = 10 (getf (nlp:span-data s) :priority)))))

  (define-test unknown-attribute-errors
    (let ((tok (aref (nlp:doc-tokens (nlp:make-doc "x")) 0)))
      (fail (nlp:match-tokens '(:bogus 1) tok) error)
      (true (nlp:match-tokens '() tok))
      (true (nlp:match-tokens '(:lower "x" :op "?") tok)))))

;;; --- Entity ruler ---

(defun with-priority (spans priority)
  (mapcar (lambda (s)
            (setf (nlp:span-data s) (list* :priority priority (nlp:span-data s)))
            s)
          spans))

(define-test entity-ruler
  (define-test longer-wins-within-priority
    (let ((r (nlp:make-entity-ruler))
          (doc (nlp:make-doc "at 7 pm")))
      (nlp:add-entity-patterns r :time 20
                               '(((:regex "^\\d{1,2}$") (:lower ("am" "pm")))
                                 (:trim-leading 1 (:lower "at") (:regex "^\\d{1,2}$"))))
      (let ((ents (nlp:apply-entity-ruler r doc)))
        (is = 1 (length ents))
        (is string= "7 pm" (nlp:span-text (first ents)))
        (is eq ents (nlp:doc-ents doc)))))

  (define-test lower-priority-number-wins
    (let ((r (nlp:make-entity-ruler))
          (doc (nlp:make-doc "the DGH van")))
      (nlp:add-entity-patterns r :vehicle 10 '(((:lower "dgh"))) :value :dgh)
      (nlp:add-entity-patterns r :vehicle-noun 15 '(((:lower "the") (:lower "dgh") (:lower "van"))))
      (let ((ents (nlp:apply-entity-ruler r doc)))
        (is = 1 (length ents))
        (is eq :vehicle (nlp:span-label (first ents)))
        (is eq :dgh (getf (nlp:span-data (first ents)) :value)))))

  (define-test non-overlapping-and-sorted
    (let ((r (nlp:make-entity-ruler))
          (doc (nlp:make-doc "CG and BW to SHMS")))
      (nlp:add-entity-patterns r :pax 40 '(((:shape "XX"))))
      (nlp:add-entity-patterns r :dest 0 '(((:lower "shms"))))
      (let ((ents (nlp:apply-entity-ruler r doc)))
        (is equal '("CG" "BW" "SHMS") (mapcar #'nlp:span-text ents))
        (is equal '(:pax :pax :dest) (mapcar #'nlp:span-label ents)))))

  (define-test trim-leading
    (let ((r (nlp:make-entity-ruler))
          (doc (nlp:make-doc "back around 11")))
      (nlp:add-entity-patterns r :time 20 '((:trim-leading 1 (:lower "around") (:regex "^\\d+$"))))
      (let ((ents (nlp:apply-entity-ruler r doc)))
        (is = 1 (length ents))
        (is string= "11" (nlp:span-text (first ents)))
        (is = 2 (nlp:span-start (first ents))))))

  (define-test extra-spans-from-gazetteer
    (let ((r (nlp:make-entity-ruler))
          (doc (nlp:make-doc "to Saint Marys church")))
      (nlp:add-entity-patterns r :pax 40 '(((:is-title t))))
      (let* ((pm (nlp:make-phrase-matcher '("Saint Marys Church") :label :dest))
             (extra (with-priority (nlp:run-phrase-matcher pm doc) 0))
             (ents (nlp:apply-entity-ruler r doc :extra-spans extra)))
        (is = 1 (length ents))
        (is eq :dest (nlp:span-label (first ents)))
        (is string= "Saint Marys Church" (getf (nlp:span-data (first ents)) :value)))))

  (define-test no-patterns-no-entities
    (let ((doc (nlp:make-doc "anything at all")))
      (is equal '() (nlp:apply-entity-ruler (nlp:make-entity-ruler) doc))
      (is equal '() (nlp:doc-ents doc)))))

;;; --- Phrase matcher ---

(define-test phrase-matcher
  (define-test longest-and-value
    (let* ((pm (nlp:make-phrase-matcher '("SHMS" "Saint Mary church" "Saint Mary")))
           (doc (nlp:make-doc "to Saint Mary church and SHMS"))
           (spans (nlp:run-phrase-matcher pm doc)))
      (is equal '("Saint Mary church" "SHMS") (mapcar #'nlp:span-text spans))
      (is equal '("Saint Mary church" "SHMS")
          (mapcar (lambda (s) (getf (nlp:span-data s) :value)) spans))
      (is equal '(:phrase :phrase) (mapcar #'nlp:span-label spans))))

  (define-test punctuation-insensitive
    (let* ((pm (nlp:make-phrase-matcher '("St. Mary's") :label :dest))
           (doc (nlp:make-doc "going to St Mary's tomorrow"))
           (spans (nlp:run-phrase-matcher pm doc)))
      (is = 1 (length spans))
      (is string= "St Mary's" (nlp:span-text (first spans)))
      (is eq :dest (nlp:span-label (first spans))))
    ;; apostrophes are ignored in both directions
    (let ((pm (nlp:make-phrase-matcher '("Saint Mary's Church"))))
      (is = 1 (length (nlp:run-phrase-matcher pm (nlp:make-doc "to saint marys church"))))
      (is = 1 (length (nlp:run-phrase-matcher pm (nlp:make-doc "to Saint Mary's Church"))))
      (is = 0 (length (nlp:run-phrase-matcher pm (nlp:make-doc "to Saint Mary church"))))))

  (define-test case-insensitive
    (let ((pm (nlp:make-phrase-matcher '("Pool"))))
      (is = 1 (length (nlp:run-phrase-matcher pm (nlp:make-doc "for the pool"))))
      (is string= "Pool"
          (getf (nlp:span-data (first (nlp:run-phrase-matcher pm (nlp:make-doc "POOL")))) :value))))

  (define-test no-match-and-blank-entries
    (is equal '() (nlp:run-phrase-matcher (nlp:make-phrase-matcher '("Shrine"))
                                          (nlp:make-doc "for the pool")))
    ;; blank / punctuation-only gazetteer entries are ignored, not matched everywhere
    (is equal '() (nlp:run-phrase-matcher (nlp:make-phrase-matcher '("" "..."))
                                          (nlp:make-doc "for the pool")))))
