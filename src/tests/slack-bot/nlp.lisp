;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/slack-bot/nlp
  (:use #:common-lisp)
  (:import-from #:rove #:deftest #:testing #:ok #:ng #:signals)
  (:local-nicknames (#:nlp #:project-isidore/slack-bot/nlp))
  (:documentation
   "Tests for project-isidore/slack-bot/nlp: tokenizer, spans, Matcher,
EntityRuler and PhraseMatcher."))

(in-package #:project-isidore/tests/slack-bot/nlp)

(defun token-texts (doc)
  (map 'list #'nlp:token-text (nlp:doc-tokens doc)))

(defun token-named (doc text)
  (find text (nlp:doc-tokens doc) :key #'nlp:token-text :test #'string=))

(defun spans-with (spans label)
  (mapcar #'nlp:span-text
          (remove-if-not (lambda (s) (eq (nlp:span-label s) label)) spans)))

;;; --- Tokenizer ---

(deftest tokenizer
  (testing "meridiem-and-clock-forms"
    (ok (equal '("for" "pool" "for" "MM" "at" "9:30" "a.m." "to" "11:30" "a.m.")
               (token-texts (nlp:make-doc "for pool for MM at 9:30 a.m. to 11:30 a.m."))))
    (ok (equal '("from" "12:30pm" "to" "2:30pm" "for" "GG" ".")
               (token-texts (nlp:make-doc "from 12:30pm to 2:30pm for GG."))))
    ;; the sentence-final period is not swallowed by the hour+meridiem token
    (ok (equal '("at" "7pm" ".") (token-texts (nlp:make-doc "at 7pm."))))
    (ok (equal '("9:30a.m." "and" "7p.m") (token-texts (nlp:make-doc "9:30a.m. and 7p.m")))))

  (testing "ordinals-dates-and-punctuation"
    (ok (equal '("April" "11th" ",") (token-texts (nlp:make-doc "April 11th,"))))
    (ok (equal '("I'd" "like") (token-texts (nlp:make-doc "I'd like"))))
    (ok (equal '("(" "BW" ")") (token-texts (nlp:make-doc "(BW)"))))
    (ok (equal '("back" "around" "11" "?") (token-texts (nlp:make-doc "back around 11?"))))
    (ok (equal '("7" "-" "9pm") (token-texts (nlp:make-doc "7-9pm"))))
    (ok (equal '("4/22" "2026-09-08" "Thursday's")
               (token-texts (nlp:make-doc "4/22 2026-09-08 Thursday's"))))
    (ok (equal '("CG" "&" "BW") (token-texts (nlp:make-doc "CG & BW")))))

  (testing "offsets-round-trip"
    (let ((doc (nlp:make-doc "Can BW take the DGH to the dentist tomorrow, leaving at 8:30?")))
      (loop for tok across (nlp:doc-tokens doc)
            for i from 0
            do (ok (= i (nlp:token-index tok)))
               (ok (string= (nlp:token-text tok)
                            (subseq (nlp:doc-text doc) (nlp:token-start tok) (nlp:token-end tok)))))))

  (testing "typographic-normalisation"
    (let ((text (format nil "I~cd like ~c ~cx~c" (code-char #x2019) (code-char #x2014)
                        (code-char #x201C) (code-char #x201D))))
      (ok (equal '("I'd" "like" "-" "\"" "x" "\"") (token-texts (nlp:make-doc text)))))
    ;; tabs/newlines become spaces; control characters vanish
    (ok (equal '("a" "b" "c")
               (token-texts (nlp:make-doc (format nil "a~cb~cc~c" #\Tab #\Newline (code-char 7)))))))

  (testing "truncation"
    (let ((nlp:*max-input-length* 10))
      (ok (= 2 (length (nlp:doc-tokens (nlp:make-doc "abcde fghij klmno")))))))

  (testing "empty-and-blank"
    (ok (= 0 (nlp:doc-length (nlp:make-doc ""))))
    (ok (= 0 (nlp:doc-length (nlp:make-doc "   ")))))

  (testing "shapes"
    (ok (string= "XX" (nlp:word-shape "CG")))
    (ok (string= "dd:ddxx" (nlp:word-shape "12:30pm")))
    (ok (string= "Xxxxx" (nlp:word-shape "Tuesday")))
    (ok (string= "x.x." (nlp:word-shape "a.m.")))
    (ok (string= "dddd" (nlp:word-shape "20260908")))
    (ok (string= "" (nlp:word-shape ""))))

  (testing "flags"
    (let* ((doc (nlp:make-doc "seven 7 CG Tuesday , swimming"))
           (toks (nlp:doc-tokens doc)))
      (ok (nlp:token-flag-p (aref toks 0) :like-num))
      (ok (nlp:token-flag-p (aref toks 1) :like-num))
      (ng (nlp:token-flag-p (aref toks 2) :like-num))
      (ok (nlp:token-flag-p (aref toks 2) :is-upper))
      (ok (nlp:token-flag-p (aref toks 3) :is-title))
      (ng (nlp:token-flag-p (aref toks 3) :is-upper))
      (ok (nlp:token-flag-p (aref toks 4) :is-punct))
      (ng (nlp:token-flag-p (aref toks 5) :is-upper))
      (ok (nlp:token-flag-p (aref toks 5) :is-alpha))
      (ok (string= "seven" (nlp:token-lower (aref toks 0))))
      (ok (string= "cg" (nlp:token-lower (aref toks 2)))))))

;;; --- Spans ---

(deftest spans
  (testing "span-helpers"
    (let* ((doc (nlp:make-doc "Please add GG to the SHMS trip on Thursday"))
           (a (nlp:make-span :doc doc :start 4 :end 7 :label :x))
           (b (nlp:make-span :doc doc :start 6 :end 8 :label :y))
           (c (nlp:make-span :doc doc :start 7 :end 9 :label :z)))
      (ok (string= "the SHMS trip" (nlp:span-text a)))
      (ok (= 3 (nlp:span-length a)))
      (ok (equal '("the" "SHMS" "trip") (mapcar #'nlp:token-text (nlp:span-tokens a))))
      (ok (nlp:spans-overlap-p a b))
      (ng (nlp:spans-overlap-p a c))
      (ok (nlp:span-covers-p a 5))
      (ng (nlp:span-covers-p a 7))
      (ok (string= "" (nlp:span-text (nlp:make-span :doc doc :start 2 :end 2)))))))

;;; --- Matcher ---

(deftest matcher
  (testing "optional-op"
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "be back by 10 or back at 11 or back")))
      (nlp:add-pattern m :ret '((:lower "back") (:lower ("by" "at") :op "?")))
      (ok (equal '("back by" "back at" "back")
                 (mapcar #'nlp:span-text (nlp:run-matcher m doc))))))

  (testing "plus-op-greedy"
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "with MM DM CG and BW")))
      (nlp:add-pattern m :pax '((:lower "with") (:shape "XX" :op "+")))
      (ok (equal '("with MM DM CG") (mapcar #'nlp:span-text (nlp:run-matcher m doc))))))

  (testing "star-op"
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "the big red van and the van")))
      (nlp:add-pattern m :veh '((:lower "the") (:lower ("big" "red") :op "*") (:lower "van")))
      (ok (equal '("the big red van" "the van")
                 (mapcar #'nlp:span-text (nlp:run-matcher m doc))))))

  (testing "zero-width-patterns-produce-nothing"
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "a b c")))
      (nlp:add-pattern m :none '((:lower "zzz" :op "*")))
      (nlp:add-pattern m :none2 '((:lower "zzz" :op "?")))
      (ok (equal '() (nlp:run-matcher m doc)))))

  (testing "leftmost-longest-non-overlapping"
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "a b a b b a")))
      (nlp:add-pattern m :ab '((:lower "a") (:lower "b" :op "*")))
      (ok (equal '("a b" "a b b" "a") (mapcar #'nlp:span-text (nlp:run-matcher m doc))))))

  (testing "attribute-kinds"
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "CG leaves at 7 pm on Tuesday")))
      (nlp:add-pattern m :time '((:regex "^\\d{1,2}$") (:lower ("am" "pm"))))
      (nlp:add-pattern m :pax '((:shape "XX" :is-upper t)))
      (nlp:add-pattern m :num '((:like-num t)))
      (nlp:add-pattern m :title '((:is-title t)))
      (nlp:add-pattern m :exact '((:text "CG")))
      (let ((spans (nlp:run-matcher m doc)))
        (ok (equal '("7 pm") (spans-with spans :time)))
        (ok (equal '("CG") (spans-with spans :pax)))
        (ok (equal '("7") (spans-with spans :num)))
        (ok (equal '("Tuesday") (spans-with spans :title)))
        (ok (equal '("CG") (spans-with spans :exact)))
        ;; results are sorted by start
        (ok (equal (mapcar #'nlp:span-start spans)
                   (sort (mapcar #'nlp:span-start spans) #'<))))))

  (testing "case-insensitive-lower-and-regex"
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "DGH Tues 7PM")))
      (nlp:add-pattern m :veh '((:lower "dgh")))
      (nlp:add-pattern m :day '((:in ("TUES" "wed"))))
      (nlp:add-pattern m :time '((:regex "^\\d{1,2}pm$")))
      (ok (= 3 (length (nlp:run-matcher m doc))))))

  (testing "label-and-data"
    (let ((m (nlp:make-matcher))
          (doc (nlp:make-doc "take the DGH")))
      (nlp:add-pattern m :vehicle '((:lower "dgh")) :value :dgh :priority 10)
      (let ((s (first (nlp:run-matcher m doc))))
        (ok (eq :vehicle (nlp:span-label s)))
        (ok (eq :dgh (getf (nlp:span-data s) :value)))
        (ok (= 10 (getf (nlp:span-data s) :priority))))))

  (testing "unknown-attribute-errors"
    (let ((tok (aref (nlp:doc-tokens (nlp:make-doc "x")) 0)))
      (ok (signals (nlp:match-tokens '(:bogus 1) tok) 'error))
      (ok (nlp:match-tokens '() tok))
      (ok (nlp:match-tokens '(:lower "x" :op "?") tok)))))

;;; --- Entity ruler ---

(defun with-priority (spans priority)
  (mapcar (lambda (s)
            (setf (nlp:span-data s) (list* :priority priority (nlp:span-data s)))
            s)
          spans))

(deftest entity-ruler
  (testing "longer-wins-within-priority"
    (let ((r (nlp:make-entity-ruler))
          (doc (nlp:make-doc "at 7 pm")))
      (nlp:add-entity-patterns r :time 20
                               '(((:regex "^\\d{1,2}$") (:lower ("am" "pm")))
                                 (:trim-leading 1 (:lower "at") (:regex "^\\d{1,2}$"))))
      (let ((ents (nlp:apply-entity-ruler r doc)))
        (ok (= 1 (length ents)))
        (ok (string= "7 pm" (nlp:span-text (first ents))))
        (ok (eq ents (nlp:doc-ents doc))))))

  (testing "lower-priority-number-wins"
    (let ((r (nlp:make-entity-ruler))
          (doc (nlp:make-doc "the DGH van")))
      (nlp:add-entity-patterns r :vehicle 10 '(((:lower "dgh"))) :value :dgh)
      (nlp:add-entity-patterns r :vehicle-noun 15 '(((:lower "the") (:lower "dgh") (:lower "van"))))
      (let ((ents (nlp:apply-entity-ruler r doc)))
        (ok (= 1 (length ents)))
        (ok (eq :vehicle (nlp:span-label (first ents))))
        (ok (eq :dgh (getf (nlp:span-data (first ents)) :value))))))

  (testing "non-overlapping-and-sorted"
    (let ((r (nlp:make-entity-ruler))
          (doc (nlp:make-doc "CG and BW to SHMS")))
      (nlp:add-entity-patterns r :pax 40 '(((:shape "XX"))))
      (nlp:add-entity-patterns r :dest 0 '(((:lower "shms"))))
      (let ((ents (nlp:apply-entity-ruler r doc)))
        (ok (equal '("CG" "BW" "SHMS") (mapcar #'nlp:span-text ents)))
        (ok (equal '(:pax :pax :dest) (mapcar #'nlp:span-label ents))))))

  (testing "trim-leading"
    (let ((r (nlp:make-entity-ruler))
          (doc (nlp:make-doc "back around 11")))
      (nlp:add-entity-patterns r :time 20 '((:trim-leading 1 (:lower "around") (:regex "^\\d+$"))))
      (let ((ents (nlp:apply-entity-ruler r doc)))
        (ok (= 1 (length ents)))
        (ok (string= "11" (nlp:span-text (first ents))))
        (ok (= 2 (nlp:span-start (first ents)))))))

  (testing "extra-spans-from-gazetteer"
    (let ((r (nlp:make-entity-ruler))
          (doc (nlp:make-doc "to Saint Marys church")))
      (nlp:add-entity-patterns r :pax 40 '(((:is-title t))))
      (let* ((pm (nlp:make-phrase-matcher '("Saint Marys Church") :label :dest))
             (extra (with-priority (nlp:run-phrase-matcher pm doc) 0))
             (ents (nlp:apply-entity-ruler r doc :extra-spans extra)))
        (ok (= 1 (length ents)))
        (ok (eq :dest (nlp:span-label (first ents))))
        (ok (string= "Saint Marys Church" (getf (nlp:span-data (first ents)) :value))))))

  (testing "no-patterns-no-entities"
    (let ((doc (nlp:make-doc "anything at all")))
      (ok (equal '() (nlp:apply-entity-ruler (nlp:make-entity-ruler) doc)))
      (ok (equal '() (nlp:doc-ents doc))))))

;;; --- Phrase matcher ---

(deftest phrase-matcher
  (testing "longest-and-value"
    (let* ((pm (nlp:make-phrase-matcher '("SHMS" "Saint Mary church" "Saint Mary")))
           (doc (nlp:make-doc "to Saint Mary church and SHMS"))
           (spans (nlp:run-phrase-matcher pm doc)))
      (ok (equal '("Saint Mary church" "SHMS") (mapcar #'nlp:span-text spans)))
      (ok (equal '("Saint Mary church" "SHMS")
                 (mapcar (lambda (s) (getf (nlp:span-data s) :value)) spans)))
      (ok (equal '(:phrase :phrase) (mapcar #'nlp:span-label spans)))))

  (testing "punctuation-insensitive"
    (let* ((pm (nlp:make-phrase-matcher '("St. Mary's") :label :dest))
           (doc (nlp:make-doc "going to St Mary's tomorrow"))
           (spans (nlp:run-phrase-matcher pm doc)))
      (ok (= 1 (length spans)))
      (ok (string= "St Mary's" (nlp:span-text (first spans))))
      (ok (eq :dest (nlp:span-label (first spans)))))
    ;; apostrophes are ignored in both directions
    (let ((pm (nlp:make-phrase-matcher '("Saint Mary's Church"))))
      (ok (= 1 (length (nlp:run-phrase-matcher pm (nlp:make-doc "to saint marys church")))))
      (ok (= 1 (length (nlp:run-phrase-matcher pm (nlp:make-doc "to Saint Mary's Church")))))
      (ok (= 0 (length (nlp:run-phrase-matcher pm (nlp:make-doc "to Saint Mary church")))))))

  (testing "case-insensitive"
    (let ((pm (nlp:make-phrase-matcher '("Pool"))))
      (ok (= 1 (length (nlp:run-phrase-matcher pm (nlp:make-doc "for the pool")))))
      (ok (string= "Pool"
                   (getf (nlp:span-data (first (nlp:run-phrase-matcher pm (nlp:make-doc "POOL")))) :value)))))

  (testing "no-match-and-blank-entries"
    (ok (equal '() (nlp:run-phrase-matcher (nlp:make-phrase-matcher '("Shrine"))
                                           (nlp:make-doc "for the pool"))))
    ;; blank / punctuation-only gazetteer entries are ignored, not matched everywhere
    (ok (equal '() (nlp:run-phrase-matcher (nlp:make-phrase-matcher '("" "..."))
                                           (nlp:make-doc "for the pool"))))))
