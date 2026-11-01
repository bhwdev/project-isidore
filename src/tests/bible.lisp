(uiop:define-package #:project-isidore/tests/bible
  (:use #:common-lisp)
  (:import-from #:cl-ppcre)
  (:import-from #:rove #:deftest #:ok #:signals #:skip #:setup #:teardown)
  (:local-nicknames (#:bible-db #:project-isidore/bible/database)
                    (#:bible #:project-isidore/bible/text)
                    (#:reader #:project-isidore/bible/reader))
  (:documentation
   "Tests for the bible: lookups, links and search over the text, and the
reader's paths.  They need the bible store, which only production has (see
PROJECT-ISIDORE/BIBLE/DATABASE), so elsewhere they skip."))

(in-package #:project-isidore/tests/bible)

(setup
  (bible-db:open-database)
  (bible:open-search-index))

(teardown
  (bible:close-search-index)
  (bible-db:close-database))

(defmacro deftest-with-bible (name &body body)
  "A DEFTEST of BODY that is skipped on a machine without the bible store."
  `(deftest ,name
     (if bible-db:*database*
         (progn ,@body)
         (skip "No bible store on this machine."))))

;; Check that Bible data retrieval for pages finishes without error.
(deftest-with-bible generate-data-finish
  (ok (progn
        (bible:get-bible-text (bible:get-bible-uid 1 1 1))
        (bible:get-bible-text (bible:get-bible-uid 73 22 21))
        (bible:search-bible "water")
        t)))

;; Check `*reference-regex*' still works with the version of
;; CL-PPCRE in use.
(deftest-with-bible regex-validity
  (ok (string-equal "1 Esdras 5:1" (car (cl-ppcre:all-matches-as-strings bible:*reference-regex* (bible:get-footnotes-text (bible:get-bible-uid (bible:bible-book-convert-dwim "Zechariah") 1 1)))))))

;;; =====================================================================
;;; bible-book-convert-dwim
;;; =====================================================================

;; Convert book name strings to their integer identifiers.
(deftest-with-bible bible-book-convert-dwim-string-to-number
  (ok (= 1  (bible:bible-book-convert-dwim "Genesis")))
  (ok (= 47 (bible:bible-book-convert-dwim "Matthew")))
  (ok (= 73 (bible:bible-book-convert-dwim "Apocalypse")))
  ;; Case insensitivity
  (ok (= 1  (bible:bible-book-convert-dwim "genesis")))
  (ok (= 47 (bible:bible-book-convert-dwim "MATTHEW")))
  ;; Aliases map to the same number
  (ok (= 6  (bible:bible-book-convert-dwim "Joshua")))
  (ok (= 6  (bible:bible-book-convert-dwim "Josue")))
  (ok (= 9  (bible:bible-book-convert-dwim "I Samuel")))
  (ok (= 9  (bible:bible-book-convert-dwim "1 Kings"))))

;; Convert integer book IDs back to name strings.
(deftest-with-bible bible-book-convert-dwim-number-to-string
  (ok (string= "Genesis" (bible:bible-book-convert-dwim 1)))
  (ok (string= "Matthew" (bible:bible-book-convert-dwim 47)))
  (ok (stringp (bible:bible-book-convert-dwim 73))))

;; Invalid inputs return NIL rather than signaling.
(deftest-with-bible bible-book-convert-dwim-invalid-input
  (ok (eql nil (bible:bible-book-convert-dwim "NotABook")))
  (ok (eql nil (bible:bible-book-convert-dwim 999)))
  (ok (eql nil (bible:bible-book-convert-dwim 0))))

;; String to number to string roundtrip preserves canonical name.
(deftest-with-bible bible-book-convert-dwim-roundtrip
  (ok (string= "Genesis"
               (bible:bible-book-convert-dwim (bible:bible-book-convert-dwim "Genesis"))))
  (ok (string= "Matthew"
               (bible:bible-book-convert-dwim (bible:bible-book-convert-dwim "Matthew")))))

;;; =====================================================================
;;; get-bible-uid
;;; =====================================================================

;; Known book/chapter/verse triples return expected UIDs.
(deftest-with-bible get-bible-uid-known-values
  ;; Genesis 1:1
  (ok (= 10 (bible:get-bible-uid 1 1 1)))
  ;; Last verse: Revelation 22:21
  (ok (= 37197 (bible:get-bible-uid 73 22 21))))

;; Out-of-range book/chapter/verse returns NIL.
(deftest-with-bible get-bible-uid-invalid-returns-nil
  (ok (eql nil (bible:get-bible-uid 12983 29394 2938498)))
  (ok (eql nil (bible:get-bible-uid 999 1 1)))
  (ok (eql nil (bible:get-bible-uid 1 999 999))))

;;; =====================================================================
;;; valid-bible-uid-p
;;; =====================================================================

;; Validate UID existence and type checking.
(deftest-with-bible valid-bible-uid-p-behavior
  ;; Known valid UIDs
  (ok (bible:valid-bible-uid-p (bible:get-bible-uid 1 1 1)))
  (ok (bible:valid-bible-uid-p 8))
  (ok (bible:valid-bible-uid-p 37198))
  ;; Out of range
  (ok (eql nil (bible:valid-bible-uid-p 99999)))
  (ok (eql nil (bible:valid-bible-uid-p -1)))
  ;; Type error for non-integer
  (ok (signals (bible:valid-bible-uid-p "hello") 'type-error))
  (ok (signals (bible:valid-bible-uid-p 3.14) 'type-error)))

;;; =====================================================================
;;; Data retrieval consistency
;;; =====================================================================

;; get-heading-text returns correctly formatted heading strings.
(deftest-with-bible get-heading-text-format
  (ok (string= "Genesis 1:1" (bible:get-heading-text (bible:get-bible-uid 1 1 1))))
  (let ((uid (bible:get-bible-uid 73 22 21)))
    (ok (search "22:21" (bible:get-heading-text uid)))))

;; get-bible-text returns non-empty strings for valid UIDs.
(deftest-with-bible get-bible-text-returns-string
  (let ((text (bible:get-bible-text (bible:get-bible-uid 1 1 1))))
    (ok (stringp text))
    (ok (> (length text) 0))
    ;; Genesis 1:1 should contain "beginning"
    (ok (search "beginning" (string-downcase text)))))

;; Footnotes and cross-references return strings with links.
(deftest-with-bible footnotes-and-cross-references
  ;; Leviticus 2:3 has footnotes referencing Leviticus 6:20
  (let ((uid (bible:get-bible-uid (bible:bible-book-convert-dwim "Leviticus") 2 3)))
    (ok (stringp (bible:get-footnotes-text uid)))
    (ok (search "Leviticus 6:20" (bible:get-footnotes-text uid)))
    ;; with-links version contains anchor tags
    (ok (search "<a href=" (bible:get-footnotes-text-with-links uid)))
    (ok (search "/bible/3-6-20" (bible:get-footnotes-text-with-links uid)))
    ;; Cross references
    (ok (stringp (bible:get-cross-references-text uid)))
    (ok (search "<a href=" (bible:get-cross-references-text-with-links uid)))))

;; make-bible-chapter-url-list returns chapter navigation links.
(deftest-with-bible make-bible-chapter-url-list-returns-list
  (let ((result (bible:make-bible-chapter-url-list
                 (list (bible:get-bible-uid 1 1 1)
                       (bible:get-bible-uid 1 3 24)))))
    (ok (listp result))
    (ok (> (length result) 0))
    ;; Each element is (url-string . label-string)
    (ok (consp (first result)))
    (ok (search "/bible/" (car (first result))))
    (ok (stringp (cdr (first result))))))

;;; =====================================================================
;;; Search
;;; =====================================================================

;; search-bible returns scored results for common terms.
(deftest-with-bible search-bible-returns-results
  (let ((results (bible:search-bible "water")))
    (ok (listp results))
    (ok (> (length results) 0))
    ;; Each result is (uid . score)
    (ok (integerp (caar results)))
    (ok (numberp (cdar results))))
  ;; Nonsense query returns a list (possibly empty)
  (ok (listp (bible:search-bible "xyzzyplugh99999"))))

;;; =====================================================================
;;; resolve-bible-path

;; A reference from the URL is parsed as a string, not interned: a symbol for
;; every distinct path requested would never be freed.  Needs no store.
(deftest verse-reference-not-interned
  (ignore-errors (reader::parse-verse-arg "nosuchbook-1-1"))
  (ok (null (find-symbol "NOSUCHBOOK-1-1" :project-isidore/bible/reader))))
;;; =====================================================================

;; Symbolic single-verse path resolves to a one-element UID list.
(deftest-with-bible resolve-bible-path-symbolic-single-verse
  (let ((result (reader::resolve-bible-path "/bible/matthew-1-2")))
    (ok (listp result))
    (ok (= 1 (length result)))
    (ok (= (bible:get-bible-uid 47 1 2) (first result)))))

;; Symbolic range path resolves to a contiguous UID list.
(deftest-with-bible resolve-bible-path-symbolic-range
  (let* ((result (reader::resolve-bible-path
                  "/bible/genesis-1-1/genesis-1-3"))
         (start  (bible:get-bible-uid 1 1 1))
         (end    (bible:get-bible-uid 1 1 3)))
    (ok (listp result))
    (ok (= (1+ (- end start)) (length result)))
    (ok (= start (first result)))
    (ok (= end (car (last result))))))
