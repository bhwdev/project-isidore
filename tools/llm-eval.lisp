;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;;; Run the booking-llm evaluation against a running llama-server:
;;;;
;;;;   VAN_LLM_URL=http://127.0.0.1:8081/v1/chat/completions \
;;;;   sbcl --dynamic-space-size 4Gb --control-stack-size 16Mb \
;;;;        --no-userinit --no-sysinit --non-interactive --load tools/llm-eval.lisp
;;;;
;;;; Exit status 0 when every corpus entry passed, 1 otherwise.  See
;;;; src/tests/booking-llm-eval.lisp for what is measured, and
;;;; tools/llm-eval.sh to run it over several models.  The ASDF/ocicl setup
;;;; mirrors make.lisp.  LLM_EVAL_FASL_DIR, when set, gives this run its own
;;;; fasl cache for the project sources (see the memory note on policy-tainted
;;;; fasls from the cl-mcp worker).

(in-package #:cl-user)

(defvar *project-dir*
  (let ((here (pathname-directory (truename *load-pathname*))))
    (make-pathname :directory (butlast here) :name nil :type nil :version nil
                   :defaults (truename *load-pathname*))))

(require :sb-posix)
(sb-posix:setenv "CL_SOURCE_REGISTRY" (namestring *project-dir*) 1)
(require "asdf")

(let* ((home (uiop:getenv "HOME"))
       (local-bin (and home (concatenate 'string home "/.local/bin")))
       (path (or (uiop:getenv "PATH") "")))
  (when (and local-bin
             (probe-file (concatenate 'string local-bin "/ocicl"))
             (not (search local-bin path)))
    (setf (uiop:getenv "PATH") (concatenate 'string local-bin ":" path))))

(uiop:chdir *project-dir*)
(setf *default-pathname-defaults* *project-dir*)

#-ocicl
(when (probe-file #P"~/.local/share/ocicl/ocicl-runtime.lisp")
  (load #P"~/.local/share/ocicl/ocicl-runtime.lisp"))

(let ((fasl-dir (uiop:getenv "LLM_EVAL_FASL_DIR")))
  (when (and fasl-dir (plusp (length fasl-dir)))
    (asdf:initialize-output-translations
     `(:output-translations
       (,(namestring (merge-pathnames "src/" *project-dir*))
        ,(namestring (uiop:ensure-directory-pathname fasl-dir)))
       :inherit-configuration))))

(push :hunchentoot-no-ssl *features*)

(asdf:load-system "project-isidore/tests/booking-llm-eval")

(let* ((url (or (uiop:getenv "VAN_LLM_URL")
                "http://127.0.0.1:8081/v1/chat/completions"))
       (summary (progn
                  (format t "~&booking-llm eval against ~a~%~%" url)
                  (uiop:symbol-call :project-isidore/tests/booking-llm-eval :run-eval
                                    :url url))))
  (uiop:quit (if (zerop (getf summary :failed)) 0 1)))
