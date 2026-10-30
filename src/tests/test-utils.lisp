;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/test-utils
  (:use #:common-lisp)
  (:export #:with-mocked-functions
           #:with-temp-data-root)
  (:documentation
   "Helpers shared by the Project Isidore test packages."))

(in-package #:project-isidore/tests/test-utils)

(defmacro with-mocked-functions (bindings &body body)
  "Temporarily replace global function definitions while BODY runs.

BINDINGS is a list of (FUNCTION-NAME LAMBDA-FORM) pairs.  Each LAMBDA-FORM is
evaluated and installed as the SYMBOL-FUNCTION of FUNCTION-NAME before BODY
runs; the original definitions are restored on exit, normally or not.  This
replaces the SYMBOL-FUNCTION/UNWIND-PROTECT boilerplate used to stub network
calls (READ-RANGE, WRITE-RANGE, POST-SLACK-MESSAGE, ...) in tests.

  (with-mocked-functions ((vb::read-range (lambda (range &key sheet-name) ...)))
    (vb:book ...))"
  (let ((saved (loop for binding in bindings
                     collect (gensym (format nil "ORIGINAL-~A-" (first binding))))))
    `(let ,(loop for (name nil) in bindings
                 for var in saved
                 collect `(,var (symbol-function ',name)))
       (unwind-protect
            (progn
              ,@(loop for (name fn) in bindings
                      collect `(setf (symbol-function ',name) ,fn))
              ,@body)
         ,@(loop for (name nil) in bindings
                 for var in saved
                 collect `(setf (symbol-function ',name) ,var))))))

(defmacro with-temp-data-root ((&optional (root (gensym "ROOT"))) &body body)
  "Run BODY with PROJECT_ISIDORE_DATA pointing at a fresh temporary directory,
bound to ROOT, and delete that directory afterwards.  Stores BODY opens there
must be closed by the time it returns."
  (let ((saved (gensym "SAVED")))
    `(let ((,root (uiop:subpathname (uiop:temporary-directory)
                                    (format nil "project-isidore-test-~36R/"
                                            (random (expt 36 8) (make-random-state t)))))
           (,saved (uiop:getenv "PROJECT_ISIDORE_DATA")))
       (unwind-protect
            (progn
              (setf (uiop:getenv "PROJECT_ISIDORE_DATA") (namestring ,root))
              ,@body)
         (setf (uiop:getenv "PROJECT_ISIDORE_DATA") (or ,saved ""))
         (uiop:delete-directory-tree ,root :validate t :if-does-not-exist :ignore)))))
