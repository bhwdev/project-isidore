;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Load this file inside a shell to build a standalone executable binary,

;; To use your local .sbclrc which possibly contains lines `ql:add-to-init-file'.

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
                        ;; /home/$USER/quicklisp/local-projects/project-isidore/
                        (pathname-directory (uiop:getcwd))))

;;; II. ASDF CONFIGURATION
;;; See ASDF manual section 4.1 Configuring ASDF to find your systems.

;; Upgrade > ASDF 3.3.5 to use package local nicknames with package inferred
;; systems. See commit bf210b8.
(load (make-pathname :directory (append *build-dir* '("lib"))
                     :defaults "upgrade-asdf.lisp"))
(provide "asdf")
(asdf:initialize-source-registry `(:source-registry
                                   (:tree ,(make-pathname :directory
                                                          *build-dir*))
                                   :inherit-configuration))

;;; III. QUICKLISP / QLOT CONFIGURATION
;;; On SBCL MAKE.LISP can be called with --no-userinit and --no-sysinit, so we
;;; cannot assume a ".sbclrc" has loaded quicklisp. Bootstrap quicklisp from
;;; the vendored installer in lib/ when missing, then drive qlot as a Lisp
;;; library to materialise the project-local .qlot/ from ./qlfile +
;;; ./qlfile.lock. qlot itself ships in the default Quicklisp dist, so no
;;; additional dist setup is required at this point. The Ultralisp pin lives
;;; in qlfile.lock; (qlot:with-local-quicklisp ...) in Section IV activates it.
(let ((ql-setup
        ;; Return existing quicklisp system pathname if located with ASDF.
        (when (asdf:find-system "quicklisp" nil)
          (uiop:merge-pathnames*
           (uiop:pathname-parent-directory-pathname
            (asdf:system-source-directory "quicklisp")) "setup.lisp"))))
  (if (pathnamep ql-setup)
      ;; Load already existent "quicklisp/setup.lisp".
      (load ql-setup)
      ;; No prior quicklisp system found, load
      ;; project-isidore/lib/quicklisp.lisp and install in present subdirectory.
      (progn
        (load (make-pathname :directory (append *build-dir* '("lib"))
                             :defaults "bootstrap-quicklisp.lisp"))
        (funcall (symbol-function
                  (find-symbol "INSTALL" (find-package "QUICKLISP-QUICKSTART")))
                 :path (make-pathname :directory (append *build-dir* '("lib")))))))

(funcall (symbol-function (find-symbol "QUICKLOAD" (find-package "QL")))
         '(:qlot/install :qlot/utils/repl :lparallel) :silent t)
;; qlot's progress UI binds lparallel:*kernel* dynamically and never calls
;; end-kernel — the workers it spawns leak past install-qlfile and block
;; save-lisp-and-die later. Encapsulate make-kernel before driving qlot so
;; we can capture every kernel it creates and end them all afterwards.
(defparameter *captured-lparallel-kernels* nil)
(let* ((mk-sym (find-symbol "MAKE-KERNEL" (find-package "LPARALLEL")))
       (orig (symbol-function mk-sym)))
  (setf (symbol-function mk-sym)
        (lambda (&rest args)
          (let ((k (apply orig args)))
            (push k *captured-lparallel-kernels*)
            k))))

;; Call qlot's library-level installer directly rather than the user-facing
;; (qlot:install ...) wrapper. The wrapper routes through `run-qlot', which
;; dispatches to scripts/run.sh in qlot's source tree whenever :qlot.project
;; is on *features* (the case after quickloading qlot via ASDF). That script
;; is not reliably executable in a fresh quicklisp dist install, so we skip
;; the dispatcher and call install-qlfile in-process. :install-deps nil also
;; skips the project-introspection sub-step, which adds nothing here because
;; our qlfile only declares `dist' lines (no per-project pins).
(let* ((root (make-pathname :directory *build-dir*))
       (qlfile (merge-pathnames "qlfile" root))
       (local-ql (merge-pathnames ".qlot/" root)))
  (funcall (find-symbol "INSTALL-QLFILE" (find-package "QLOT/INSTALL"))
           qlfile
           :quicklisp-home local-ql
           :install-deps nil))

;; End every kernel qlot created. lparallel:end-kernel only acts on the
;; current dynamic value of *kernel*, so progv-bind it to each captured
;; kernel in turn. :wait t makes us block until workers exit cleanly.
(let ((end-kernel (find-symbol "END-KERNEL" (find-package "LPARALLEL")))
      (k-var (find-symbol "*KERNEL*" (find-package "LPARALLEL"))))
  (dolist (k *captured-lparallel-kernels*)
    (progv (list k-var) (list k)
      (ignore-errors (funcall end-kernel :wait t))))
  (setf *captured-lparallel-kernels* nil))

;;; IV. BUILDING
;;; Run the build inside qlot's local quicklisp so every ql:quickload and
;;; asdf:make below resolves against .qlot/dists/ (pinned by qlfile.lock)
;;; instead of ~/quicklisp/dists/. qlot:with-local-quicklisp is a macro and
;;; qlot is only loaded at run-time within this file, so build the form as
;;; data and eval it; the source-level reader pass therefore only needs
;;; symbols from packages loaded earlier (CL, QL, ASDF, UIOP).
(eval
 `(,(find-symbol "WITH-LOCAL-QUICKLISP" (find-package "QLOT/UTILS/REPL"))
    (,(make-pathname :directory *build-dir*))
   ;; Caddy is used for SSL Termination and as a Reverse Proxy.
   (push :hunchentoot-no-ssl *features*)
   ;; Whitespace to enhance readability in logs.
   (format t "~&        *BUILD-DIR* = ~a" (make-pathname :directory ',*build-dir*))
   (format t "~&        COMMON-LISP-ENV = ~a:~a (Provided ASDF version ~a) on ~a~%"
           (lisp-implementation-type) (lisp-implementation-version)
           (asdf:asdf-version) (machine-type))
   (format t "~&        FIXNUM BITS:~a~%" (integer-length most-positive-fixnum))
   (format t "~&        FEATURES = ~a~%" *features*)
   (format t "~&        QL CLIENT ~a on ULTRALISP DIST VER ~a~%"
           (ql:client-version)
           (ql::version (ql::dist "ultralisp")))
   (pushnew (make-pathname :directory ',*build-dir*) ql:*local-project-directories*)
   (ql:register-local-projects)
   ;; Bundle Slynk into the image for remote debugging. The controller uses
   ;; (find-package :slynk) guards so it remains optional at runtime.
   (ql:quickload '("slynk" "slynk-macrostep") :silent t)
   ;; CRUD state lives at runtime under $PROJECT_ISIDORE_DB (or the XDG default
   ;; $XDG_DATA_HOME/project-isidore/) and is NOT baked into this binary; only
   ;; the vendored bible rucksack under data/rucksack/ ships in the image.
   (ql:quickload "project-isidore/test")
   (if (asdf:test-system "project-isidore")
       (progn
         (format t "~&        ====== Build Successful | Deo Gratias ======~%")
         (format t "~&        ====== END OF MAKE.LISP ======~%")
         ;; SBCL refuses to save a core with multiple threads running. qlot's
         ;; progress UI binds an lparallel kernel dynamically inside its own
         ;; LET, so the workers leak past install-qlfile with no exposed
         ;; handle. Destroy every non-main thread before save-lisp-and-die.
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
         (uiop:quit 100)))))
