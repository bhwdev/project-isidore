;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Load this file inside a shell to build a standalone executable binary,

;; To use your local .sbclrc which possibly contains the ocicl-runtime load,

;; "$ sbcl --load /path/to/this/file.lisp"

;; Bypass any SBCL Initialization Files on Production deployments with,
;; http://www.sbcl.org/manual/#Command-Line-Options

;; "$ sbcl --dynamic-space-size 2048 --control-stack-size 4 --no-userinit
;; --no-sysinit --load /path/to/this/file.lisp"

(in-package #:cl-user) ; The buildpack requires cl-user package space.
;; See weitzCommonLispRecipes2016, page 102.
(setf *read-default-float-format* 'double-float)
#+sbcl (require :sb-posix) ; This is needed for "(uiop:getenv)".
(require "asdf") ; The recommended way from the manual to load bundled ASDF.

(format t "~%~&        ====== MAKE.LISP ======~%")
;; I also like to live dangerously.
(declaim (optimize (safety 2) (speed 3) (space 0) (debug 0) (compilation-speed 0)))

;;; I. ENVIRONMENT VARIABLES
(defvar *build-dir* (if (uiop:getenvp "BUILD_DIR")
                        (pathname-directory (pathname (concatenate 'string (uiop:getenv "BUILD_DIR") "/")))
                        ;; Local BUILD_DIR should be similar to
                        ;; /home/$USER/common-lisp/project-isidore/
                        (pathname-directory (uiop:getcwd))))

;;; II. ASDF & OCICL CONFIGURATION
;;; See ASDF manual section 4.1 Configuring ASDF to find your systems.

;; Upgrade > ASDF 3.3.5 to use package local nicknames with package inferred
;; systems. See commit bf210b8.
(load (make-pathname :directory (append *build-dir* '("lib"))
                     :defaults "upgrade-asdf.lisp"))
(provide "asdf")

;; sbcl --script bypasses .sbclrc, so load ocicl-runtime explicitly. The
;; runtime hooks ASDF's missing-component handler so any subsequent
;; (asdf:load-system ...) auto-fetches dependencies from ghcr.io as needed,
;; recording digest-pinned references in ./ocicl.csv (committed alongside
;; the source). Override the default location with the OCICL_RUNTIME env
;; var if your install lives elsewhere.
(let ((runtime (or (uiop:getenvp "OCICL_RUNTIME")
                   (merge-pathnames ".local/share/ocicl/ocicl-runtime.lisp"
                                    (user-homedir-pathname)))))
  (unless (probe-file runtime)
    (error "ocicl-runtime.lisp not found at ~A. Run 'ocicl setup' first."
           runtime))
  (load runtime))

;; Project source tree only. :ignore-inherited-configuration keeps the
;; user's ~/.config/common-lisp/source-registry.conf.d/ entries (e.g. the
;; quicklisp/ultralisp tree contributions installed by qlot/quicklisp) out
;; of the registry, so they don't shadow ocicl's resolution. ocicl-runtime
;; hooks into asdf:*system-definition-search-functions* (a separate list
;; that survives this call), so missing systems still fall through to it
;; for fetching, which gets recorded in ocicl.csv for reproducibility.
(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(make-pathname :directory *build-dir*))
   :ignore-inherited-configuration))

;;; III. BUILDING
;; Caddy is used for SSL Termination and as a Reverse Proxy.
(push :hunchentoot-no-ssl *features*)
;; Whitespace to enhance readability in logs.
(format t "~&        *BUILD-DIR* = ~a" (make-pathname :directory *build-dir*))
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

;; Runtime state (CRUD graph-db + Montezuma search index) lives under
;; $PROJECT_ISIDORE_DATA (or the XDG default $XDG_DATA_HOME/project-isidore/)
;; and is NOT baked into this binary.
(asdf:load-system "project-isidore/test")
(if (asdf:test-system "project-isidore")
    (progn
      (format t "~&        ====== Build Successful | Deo Gratias ======~%")
      (format t "~&        ====== END OF MAKE.LISP ======~%")
      ;; SBCL refuses to save a core with multiple threads running. Without
      ;; qlot's lparallel progress UI this should be a no-op, but graph-db /
      ;; hunchentoot / slack-bot test setup may still spawn workers worth
      ;; defending against.
      (let ((main (bordeaux-threads:current-thread)))
        (dolist (th (bordeaux-threads:all-threads))
          (unless (eq th main)
            (ignore-errors (bordeaux-threads:destroy-thread th)))))
      (sleep 0.5)
      (asdf:make "project-isidore") ; see "project-isidore.asd" for more.
      ;; SBCL's `save-lisp-and-die' kills the lisp process at the end.
      ;; However this behaviour is implementation dependent. This command
      ;; is here in case it does not kill the lisp process.
      (uiop:quit 0))
    (progn
      (format t "~&        ====== Fix Failed Tests | Build Cancelled ======~%")
      (format t "~&        ====== END OF MAKE.LISP ======~%")
      (uiop:quit 100)))
