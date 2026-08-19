;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Load this file inside a shell to build a standalone executable binary,

;; "$ sbcl --dynamic-space-size '8Gb' --control-stack-size '16Mb' --no-userinit
;; --no-sysinit --load /path/to/this/file.lisp"

(in-package #:cl-user)

;; See weitzCommonLispRecipes2016, page 102.
(setf *read-default-float-format* 'double-float)

;;; I. ASDF & OCICL CONFIGURATION
;; This file is loaded with `sbcl --no-userinit --no-sysinit`, which skips
;; .sbclrc, so configure ASDF + ocicl explicitly here.
;;
;; project-isidore and its dependencies are vendored locally via ocicl
;; (project-isidore/ocicl.csv + project-isidore/ocicl/). For a clean,
;; deterministic build, pin CL_SOURCE_REGISTRY to this project's directory
;; *before* (require "asdf"): otherwise ASDF's built-in default scans
;; ~/common-lisp/ recursively and, finding duplicate .asd files vendored by
;; sibling ocicl projects, warns on every build (and may pick their copies).
;; project-isidore.asd is found via this single non-recursive entry;
;; third-party deps resolve through ocicl's searcher (loaded below).
(defvar *project-dir*
  (make-pathname :defaults (truename (or *load-pathname* *default-pathname-defaults*))
                 :name nil :type nil :version nil))
(require :sb-posix)
(sb-posix:setenv "CL_SOURCE_REGISTRY" (namestring *project-dir*) 1)

(require "asdf") ; load Utilities for Implementation- and OS-Portability.

(format t "~%~&        ====== MAKE.LISP ======~%")
(declaim (optimize (safety 2) (speed 3) (space 0) (debug 0) (compilation-speed 0)))

;; Ensure ~/.local/bin is on PATH: ocicl-runtime spawns the `ocicl` binary to
;; install missing systems, and non-interactive shells (ssh, systemd) do not
;; inherit ~/.local/bin from .profile / .bashrc.
(let* ((home (uiop:getenv "HOME"))
       (local-bin (and home (concatenate 'string home "/.local/bin")))
       (path (or (uiop:getenv "PATH") "")))
  (when (and local-bin
             (probe-file (concatenate 'string local-bin "/ocicl"))
             (not (search local-bin path)))
    (setf (uiop:getenv "PATH") (concatenate 'string local-bin ":" path))))

;; ocicl resolves local systems by walking up from the current working
;; directory for ocicl.csv, so build from this project's directory regardless
;; of where the script was invoked.
(uiop:chdir *project-dir*)
(setf *default-pathname-defaults* *project-dir*)

#-ocicl
(when (probe-file #P"~/.local/share/ocicl/ocicl-runtime.lisp")
  (load #P"~/.local/share/ocicl/ocicl-runtime.lisp"))

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
;; slynk-macrostep used to be loaded here too, but ocicl does not mirror
;; joaotavora/sly-macrostep (the upstream source was ultralisp-only). If
;; you want macroexpansion debugging back, vendor the three .lisp files
;; from that repo into lib/slynk-macrostep/ and add an .asd file.
(asdf:load-system :slynk)

;; CRUD state lives at runtime under $PROJECT_ISIDORE_DATA (or the XDG default
;; $XDG_DATA_HOME/project-isidore/) and is NOT baked into this binary.
(asdf:load-system "project-isidore/tests")
(if (asdf:test-system "project-isidore")
    (progn
      (format t "~&        ====== Build Successful | Deo Gratias ======~%")
      (format t "~&        ====== END OF MAKE.LISP ======~%")
      ;; SBCL refuses to save a core with multiple threads running. graph-db /
      ;; hunchentoot / slack-bot test setup may still spawn workers.
      (let ((main (bordeaux-threads:current-thread)))
        (dolist (th (bordeaux-threads:all-threads))
          (unless (eq th main)
            (ignore-errors (bordeaux-threads:destroy-thread th)))))
      (sleep 0.5)
      ;; Strip ocicl-runtime's ASDF searcher before save-lisp-and-die. At
      ;; runtime, asdf:system-relative-pathname re-locates :project-isidore
      ;; via the search functions; if the searcher falls through to ocicl,
      ;; it spawns the `ocicl` binary to install — fatal under systemd where
      ;; ~/.local/bin is not on PATH. The saved image needs no further
      ;; system installation; all dependencies are already loaded.
      (when (find-package :ocicl-runtime)
        (let ((searcher (find-symbol "SYSTEM-DEFINITION-SEARCHER"
                                     :ocicl-runtime)))
          (when searcher
            (setf asdf:*system-definition-search-functions*
                  (remove searcher
                          asdf:*system-definition-search-functions*)))))
      (asdf:make "project-isidore") ; see "project-isidore.asd" for more.
      ;; SBCL's `save-lisp-and-die' kills the lisp process at the end.
      ;; However this behaviour is implementation dependent. This command
      ;; is here in case it does not kill the lisp process.
      (uiop:quit 0))
    (progn
      (format t "~&        ====== Fix Failed Tests | Build Cancelled ======~%")
      (format t "~&        ====== END OF MAKE.LISP ======~%")
      (uiop:quit 100)))
