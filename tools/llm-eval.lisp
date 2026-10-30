;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;;; Run the booking-llm evaluation against a running llama-server:
;;;;
;;;;   VAN_LLM_URL=http://127.0.0.1:8081/v1/chat/completions \
;;;;   guix shell -D -f guix.scm -- sbcl --dynamic-space-size 4Gb --control-stack-size 16Mb \
;;;;        --no-userinit --no-sysinit --non-interactive --load tools/llm-eval.lisp
;;;;
;;;; Exit status 0 when every corpus entry passed, 1 otherwise.  See
;;;; src/tests/slack-bot/booking-llm-eval.lisp for what is measured, and
;;;; tools/llm-eval.sh to run it over several models.  ASDF finds the
;;;; dependencies the Guix shell provides, and nothing else.
;;;; LLM_EVAL_FASL_DIR, when set, gives this run its own
;;;; fasl cache for the project sources (see the memory note on policy-tainted
;;;; fasls from the cl-mcp worker).

(in-package #:cl-user)

(defvar *project-dir*
  (let ((here (pathname-directory (truename *load-pathname*))))
    (make-pathname :directory (butlast here) :name nil :type nil :version nil
                   :defaults (truename *load-pathname*))))

(require "asdf")

(asdf:initialize-source-registry
 `(:source-registry
   (:directory ,*project-dir*)
   ,@(mapcar (lambda (directory) `(:include ,directory))
             (uiop:xdg-config-dirs "common-lisp/source-registry.conf.d/"))
   :ignore-inherited-configuration))

(let ((fasl-dir (uiop:getenv "LLM_EVAL_FASL_DIR")))
  (when (and fasl-dir (plusp (length fasl-dir)))
    (asdf:initialize-output-translations
     `(:output-translations
       (,(namestring (merge-pathnames "src/" *project-dir*))
        ,(namestring (uiop:ensure-directory-pathname fasl-dir)))
       :inherit-configuration))))

(asdf:load-system "project-isidore/tests/slack-bot/booking-llm-eval")

(let* ((url (or (uiop:getenv "VAN_LLM_URL")
                "http://127.0.0.1:8081/v1/chat/completions"))
       (summary (progn
                  (format t "~&booking-llm eval against ~a~%~%" url)
                  (uiop:symbol-call :project-isidore/tests/slack-bot/booking-llm-eval :run-eval
                                    :url url))))
  (uiop:quit (if (zerop (getf summary :failed)) 0 1)))
