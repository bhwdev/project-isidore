;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/test/tests
  (:use #:common-lisp #:series)
  (:local-nicknames
   (#:pi          #:project-isidore)
   (#:test        #:parachute)
   (#:http-client #:dexador))
  (:export #:master-suite)
  (:documentation
   "Project Isidore Regression Tests.

To run locally evaluate in the listener,

CL-USER> (ql:quickload \"project-isidore/test\")
CL-USER> (asdf:test-system \"project-isidore\")

The test suite is run prior to the build process. See MAKE.LISP."))

(in-package #:project-isidore/test/tests)

(test:define-test master-suite
  :description "The master suite of all Project Isidore tests")

(test:define-test can-app-init-and-exit
  :description "Check that application starts and exits gracefully. Terminate
application must be run otherwise `sb-ext:save-lisp-and-die' cannot save core as
there needs to be one thread, and initializing the application will create a
hunchentoot thread. We select port 3510 as port(s) 8080-8091 will be busy on the
production server."
  :parent master-suite
  (test:skip-on
      (win32) "Hunchentoot has poor Microsoft Windows support."
    (test:true (progn
                 (pi:initialize-application :port 3510)
                 (pi:terminate-application)))))

(test:define-test is-production-server-status-200
  :description "As per Heroku documentation: 'Whenever your app experiences an
  error, Heroku will return a standard error page with the HTTP status code
  503.'. This test serves as an early warning that the previous deploy may be
  successfully built, but there exists a runtime error.

  See Project Isidore documentation for more on the Dexador HTTP client library
  used.

  See https://httpstatuses.com/ for a list of HTTP status codes."
  :parent master-suite
  (test:true
   (= 200 (nth-value 1 (http-client:get "https://www.bhw.name/")))))

(test:define-test generate-data-finish
  :description "Check that Bible data retrieval for pages finishes without error."
  :parent master-suite
  (test:finish (progn
                 (pi:get-bible-text (pi:get-bible-uid 1 1 1))
                 (pi:get-bible-text (pi:get-bible-uid 73 22 21))
                 (pi:search-bible "water"))))

(test:define-test regex-validity
  :description "Check `*reference-regex*' still works with the version of
  CL-PPCRE in use."
  :parent master-suite
  (test:true (string-equal "1 Esdras 5:1" (car (ppcre:all-matches-as-strings pi:*reference-regex* (pi:get-footnotes-text (pi:get-bible-uid (pi:bible-book-convert-dwim "Zechariah") 1 1)))))))

;;; =====================================================================
;;; bible-book-convert-dwim
;;; =====================================================================

(test:define-test bible-book-convert-dwim-string-to-number
  :description "Convert book name strings to their integer identifiers."
  :parent master-suite
  (test:is = 1  (pi:bible-book-convert-dwim "Genesis"))
  (test:is = 47 (pi:bible-book-convert-dwim "Matthew"))
  (test:is = 73 (pi:bible-book-convert-dwim "Apocalypse"))
  ;; Case insensitivity
  (test:is = 1  (pi:bible-book-convert-dwim "genesis"))
  (test:is = 47 (pi:bible-book-convert-dwim "MATTHEW"))
  ;; Aliases map to the same number
  (test:is = 6  (pi:bible-book-convert-dwim "Joshua"))
  (test:is = 6  (pi:bible-book-convert-dwim "Josue"))
  (test:is = 9  (pi:bible-book-convert-dwim "I Samuel"))
  (test:is = 9  (pi:bible-book-convert-dwim "1 Kings")))

(test:define-test bible-book-convert-dwim-number-to-string
  :description "Convert integer book IDs back to name strings."
  :parent master-suite
  (test:is string= "Genesis" (pi:bible-book-convert-dwim 1))
  (test:is string= "Matthew" (pi:bible-book-convert-dwim 47))
  (test:true (stringp (pi:bible-book-convert-dwim 73))))

(test:define-test bible-book-convert-dwim-invalid-input
  :description "Invalid inputs return NIL rather than signaling."
  :parent master-suite
  (test:is eql nil (pi:bible-book-convert-dwim "NotABook"))
  (test:is eql nil (pi:bible-book-convert-dwim 999))
  (test:is eql nil (pi:bible-book-convert-dwim 0)))

(test:define-test bible-book-convert-dwim-roundtrip
  :description "String to number to string roundtrip preserves canonical name."
  :parent master-suite
  (test:is string= "Genesis"
           (pi:bible-book-convert-dwim (pi:bible-book-convert-dwim "Genesis")))
  (test:is string= "Matthew"
           (pi:bible-book-convert-dwim (pi:bible-book-convert-dwim "Matthew"))))

;;; =====================================================================
;;; get-bible-uid
;;; =====================================================================

(test:define-test get-bible-uid-known-values
  :description "Known book/chapter/verse triples return expected UIDs."
  :parent master-suite
  ;; Genesis 1:1
  (test:is = 10 (pi:get-bible-uid 1 1 1))
  ;; Last verse: Revelation 22:21
  (test:is = 37197 (pi:get-bible-uid 73 22 21)))

(test:define-test get-bible-uid-invalid-returns-nil
  :description "Out-of-range book/chapter/verse returns NIL."
  :parent master-suite
  (test:is eql nil (pi:get-bible-uid 12983 29394 2938498))
  (test:is eql nil (pi:get-bible-uid 999 1 1))
  (test:is eql nil (pi:get-bible-uid 1 999 999)))

;;; =====================================================================
;;; valid-bible-uid-p
;;; =====================================================================

(test:define-test valid-bible-uid-p-behavior
  :description "Validate UID existence and type checking."
  :parent master-suite
  ;; Known valid UIDs
  (test:true (pi:valid-bible-uid-p (pi:get-bible-uid 1 1 1)))
  (test:true (pi:valid-bible-uid-p 8))
  (test:true (pi:valid-bible-uid-p 37198))
  ;; Out of range
  (test:is eql nil (pi:valid-bible-uid-p 99999))
  (test:is eql nil (pi:valid-bible-uid-p -1))
  ;; Type error for non-integer
  (test:fail (pi:valid-bible-uid-p "hello") type-error)
  (test:fail (pi:valid-bible-uid-p 3.14) type-error))

;;; =====================================================================
;;; Data retrieval consistency
;;; =====================================================================

(test:define-test get-heading-text-format
  :description "get-heading-text returns correctly formatted heading strings."
  :parent master-suite
  (test:is string= "Genesis 1:1" (pi:get-heading-text (pi:get-bible-uid 1 1 1)))
  (let ((uid (pi:get-bible-uid 73 22 21)))
    (test:true (search "22:21" (pi:get-heading-text uid)))))

(test:define-test get-bible-text-returns-string
  :description "get-bible-text returns non-empty strings for valid UIDs."
  :parent master-suite
  (let ((text (pi:get-bible-text (pi:get-bible-uid 1 1 1))))
    (test:true (stringp text))
    (test:true (> (length text) 0))
    ;; Genesis 1:1 should contain "beginning"
    (test:true (search "beginning" (string-downcase text)))))

(test:define-test footnotes-and-cross-references
  :description "Footnotes and cross-references return strings with links."
  :parent master-suite
  ;; Leviticus 2:3 has footnotes referencing Leviticus 6:20
  (let ((uid (pi:get-bible-uid (pi:bible-book-convert-dwim "Leviticus") 2 3)))
    (test:true (stringp (pi:get-footnotes-text uid)))
    (test:true (search "Leviticus 6:20" (pi:get-footnotes-text uid)))
    ;; with-links version contains anchor tags
    (test:true (search "<a href=" (pi:get-footnotes-text-with-links uid)))
    (test:true (search "/bible/3-6-20" (pi:get-footnotes-text-with-links uid)))
    ;; Cross references
    (test:true (stringp (pi:get-cross-references-text uid)))
    (test:true (search "<a href=" (pi:get-cross-references-text-with-links uid)))))

(test:define-test make-bible-chapter-url-list-returns-list
  :description "make-bible-chapter-url-list returns chapter navigation links."
  :parent master-suite
  (let ((result (pi:make-bible-chapter-url-list
                 (list (pi:get-bible-uid 1 1 1)
                       (pi:get-bible-uid 1 3 24)))))
    (test:true (listp result))
    (test:true (> (length result) 0))
    ;; Each element is (url-string . label-string)
    (test:true (consp (first result)))
    (test:true (search "/bible/" (car (first result))))
    (test:true (stringp (cdr (first result))))))

;;; =====================================================================
;;; Search
;;; =====================================================================

(test:define-test search-bible-returns-results
  :description "search-bible returns scored results for common terms."
  :parent master-suite
  (let ((results (pi:search-bible "water")))
    (test:true (listp results))
    (test:true (> (length results) 0))
    ;; Each result is (uid . score)
    (test:true (integerp (caar results)))
    (test:true (numberp (cdar results))))
  ;; Nonsense query returns a list (possibly empty)
  (test:true (listp (pi:search-bible "xyzzyplugh99999"))))

;;; =====================================================================
;;; resolve-bible-path
;;; =====================================================================

(test:define-test resolve-bible-path-symbolic-single-verse
  :description "Symbolic single-verse path resolves to a one-element UID list."
  :parent master-suite
  (let ((result (project-isidore/webserver::resolve-bible-path "/bible/matthew-1-2")))
    (test:true (listp result))
    (test:is = 1 (length result))
    (test:is = (pi:get-bible-uid 47 1 2) (first result))))

(test:define-test resolve-bible-path-symbolic-range
  :description "Symbolic range path resolves to a contiguous UID list."
  :parent master-suite
  (let* ((result (project-isidore/webserver::resolve-bible-path
                  "/bible/genesis-1-1/genesis-1-3"))
         (start  (pi:get-bible-uid 1 1 1))
         (end    (pi:get-bible-uid 1 1 3)))
    (test:true (listp result))
    (test:is = (1+ (- end start)) (length result))
    (test:is = start (first result))
    (test:is = end (car (last result)))))

;;; =====================================================================
;;; View pages complete without error
;;; =====================================================================

;;; Static page and error page tests removed — page rendering is now done via
;;; CLOG WebSocket handlers which require a browser connection to test.
;;; Integration testing should be done by navigating to the pages in a browser.
