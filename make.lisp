;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Load this file inside a shell to build a standalone executable binary,
;; bin/ProjectIsidore, with the dependencies of manifest.scm:

;; "$ guix shell -m manifest.scm -- sbcl --dynamic-space-size '8Gb'
;; --control-stack-size '16Mb' --script make.lisp"

;; guix.scm's build phase runs it the same way, with PROJECT_ISIDORE_OFFLINE
;; set (see the test-op of "project-isidore/tests").

(in-package #:cl-user)

;; See weitzCommonLispRecipes2016, page 102.
(setf *read-default-float-format* 'double-float)

;;; I. ASDF CONFIGURATION
;; This file is loaded with `sbcl --script', which skips .sbclrc, so
;; configure ASDF explicitly here.
;;
;; Every dependency comes from a Guix profile (manifest.scm in development,
;; the package inputs inside `guix build'), whose packages each register
;; their systems in $XDG_CONFIG_DIRS/common-lisp/source-registry.conf.d/.
;; Use exactly those plus this project's directory: ASDF's built-in default
;; also scans ~/common-lisp/ recursively, and copies of the same libraries
;; vendored by sibling projects there would warn on every build (and may be
;; picked over the profile's).
(defvar *project-dir*
  (make-pathname :defaults (truename (or *load-pathname* *default-pathname-defaults*))
                 :name nil :type nil :version nil))

(require "asdf") ; load Utilities for Implementation- and OS-Portability.

(asdf:initialize-source-registry
 `(:source-registry
   (:directory ,*project-dir*)
   ,@(mapcar (lambda (directory) `(:include ,directory))
             (uiop:xdg-config-dirs "common-lisp/source-registry.conf.d/"))
   :ignore-inherited-configuration))

(format t "~%~&        ====== MAKE.LISP ======~%")
(declaim (optimize (safety 2) (speed 3) (space 0) (debug 0) (compilation-speed 0)))

;;; II. BUILDING
;; Caddy is used for SSL Termination and as a Reverse Proxy.
(push :hunchentoot-no-ssl *features*)
;; Whitespace to enhance readability in logs.
(format t "~&        *PROJECT-DIR* = ~a" *project-dir*)
(format t "~&        COMMON-LISP-ENV = ~a:~a (Provided ASDF version ~a) on ~a~%"
        (lisp-implementation-type) (lisp-implementation-version)
        (asdf:asdf-version) (machine-type))
(format t "~&        FIXNUM BITS:~a~%" (integer-length most-positive-fixnum))
(format t "~&        FEATURES = ~a~%" *features*)

;; Bundle Slynk into the image for remote debugging. The controller uses
;; (find-package :slynk) guards so it remains optional at runtime.
(asdf:load-system :slynk)

;; CRUD state lives at runtime under $PROJECT_ISIDORE_DATA (or the XDG default
;; $XDG_DATA_HOME/project-isidore/) and is NOT baked into this binary.
(asdf:load-system "project-isidore/tests")
(handler-case (asdf:test-system "project-isidore")
  (error (condition)
    (format t "~&~a~%" condition)
    (format t "~&        ====== Fix Failed Tests | Build Cancelled ======~%")
    (format t "~&        ====== END OF MAKE.LISP ======~%")
    (uiop:quit 100)))

(format t "~&        ====== Build Successful | Deo Gratias ======~%")
(format t "~&        ====== END OF MAKE.LISP ======~%")
;; SBCL refuses to save a core with multiple threads running. graph-db /
;; hunchentoot / slack-bot test setup may still spawn workers.
(let ((main (bordeaux-threads:current-thread)))
  (dolist (th (bordeaux-threads:all-threads))
    (unless (eq th main)
      (ignore-errors (bordeaux-threads:destroy-thread th)))))
(sleep 0.5)
(asdf:make "project-isidore") ; see "project-isidore.asd" for more.
;; SBCL's `save-lisp-and-die' kills the lisp process at the end.
;; However this behaviour is implementation dependent. This command
;; is here in case it does not kill the lisp process.
(uiop:quit 0)
