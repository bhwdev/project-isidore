;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/test-utils
  (:use #:common-lisp)
  (:export #:with-mocked-functions)
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
