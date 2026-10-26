;;;; SPDX-FileCopyrightText: 2021  Ben H. W. <foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

#-asdf3.3 (error "Project Isidore requires ASDF 3.3 or later. Please upgrade your ASDF. See commit bf210b8.")

;; graph-db's package.lisp emits a SBCL package-at-variance warning when
;; recompiled against an image that already has the full GRAPH-DB package
;; (later files add exports). ASDF promotes that warning to a fatal
;; COMPILE-FILE-ERROR, breaking (asdf:test-system :project-isidore). Treat it
;; as uninteresting so the build proceeds.
#+sbcl
(let ((variance (find-symbol "PACKAGE-AT-VARIANCE" :sb-kernel)))
  (when variance
    (pushnew variance uiop/lisp-build:*uninteresting-conditions*)))

;; cl-stdutils (a dependency of langutils, used by project-isidore/nlp) bundles
;; an ancient copy of split-sequence whose DEFPACKAGE also exports PARTITION,
;; PARTITION-IF and PARTITION-IF-NOT.  Exporting them from SPLIT-SEQUENCE
;; clashes in every package that uses SPLIT-SEQUENCE and has its own symbol of
;; that name: SERAPEUM/BUNDLE (which re-exports both serapeum and
;; split-sequence), and in some serapeum versions (Guix 1.5.0's) SERAPEUM
;; itself.  The PERFORM methods below resolve the clash around that one file:
;; before it is compiled or loaded, each such package keeps its own symbol as
;; a shadowing symbol; after it loads, the stray exports are removed so a later
;; serapeum load sees the split-sequence 2.x API it expects.
;;
;; DEFPACKAGE takes effect at compile time as well, so the "before" step must
;; wrap COMPILE-OP too, not only LOAD-OP: a machine that has never compiled
;; stdutils (a fresh deploy, or a cleared fasl cache) otherwise hits the
;; NAME-CONFLICT inside COMPILE-FILE, before any LOAD-OP hook can run.
(defun stdutils-split-sequence-file-p (component)
  (and (typep component 'asdf:cl-source-file)
       (string= (asdf:component-name component) "split-sequence")
       (string-equal (asdf:component-name (asdf:component-system component)) "stdutils")))

(defun keep-own-partition-over-split-sequence (component)
  (when (stdutils-split-sequence-file-p component)
    (let ((split-sequence (find-package "SPLIT-SEQUENCE")))
      (when split-sequence
        (dolist (package (package-used-by-list split-sequence))
          (dolist (name '("PARTITION" "PARTITION-IF" "PARTITION-IF-NOT"))
            (let ((symbol (find-symbol name package)))
              (when (and symbol (not (eq (symbol-package symbol) split-sequence)))
                ;; SERAPEUM is locked.
                #+sbcl (sb-ext:without-package-locks (shadowing-import symbol package))
                #-sbcl (shadowing-import symbol package)))))))))

(defmethod asdf:perform :before ((op asdf:compile-op) (component asdf:cl-source-file))
  (keep-own-partition-over-split-sequence component))

(defmethod asdf:perform :before ((op asdf:load-op) (component asdf:cl-source-file))
  (keep-own-partition-over-split-sequence component))

;; SBCL's bundled ASDF makes any full WARNING raised while compiling a file
;; fatal (*COMPILE-FILE-FAILURE-BEHAVIOUR* is :ERROR there; upstream ASDF
;; defaults to :WARN).  Vendored dependencies are not ours to fix -- e.g.
;; cl-langutils' tokenize.lisp warns that a derived type conflicts with its
;; asserted type -- and a fresh machine compiles all of them, so a production
;; build died on that warning.  Keep the strict setting for project-isidore's
;; own files only; dependency warnings are still printed, just not fatal.
(defmethod asdf:perform :around ((op asdf:compile-op) (component asdf:cl-source-file))
  (let ((asdf:*compile-file-failure-behaviour*
          (if (string-equal (asdf:primary-system-name (asdf:component-system component))
                            "project-isidore")
              asdf:*compile-file-failure-behaviour*
              :warn)))
    (call-next-method)))

(defmethod asdf:perform :after ((op asdf:load-op) (component asdf:cl-source-file))
  (when (stdutils-split-sequence-file-p component)
    (let ((package (find-package "SPLIT-SEQUENCE")))
      (when package
        (dolist (name '("PARTITION" "PARTITION-IF" "PARTITION-IF-NOT"))
          (let ((symbol (find-symbol name package)))
            (when symbol
              (unexport symbol package))))))))

(asdf:defsystem "project-isidore"
  :name "Project Isidore"
  :version "1.2.2"
  :author " Ben H. W.<foss@bhw.name>"
  :maintainer " Ben H. W.<foss@bhw.name>"
  :description "Personal Web Application"
  :long-description
  "Project Isidore is personal web application"
  :license  "GNU Affero General Public License 3.0 or later"
  :homepage "https://www.bhw.name/blog/project-isidore-doc.html"
  :bug-tracker "https://github.com/bhwdev/project-isidore/issues"
  :source-control (:git "https://github.com/bhwdev/project-isidore.git")
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("project-isidore/webserver"
               "project-isidore/van-booking"
               "project-isidore/slack-bot"
               )
  :in-order-to ((asdf:test-op (asdf:test-op "project-isidore/tests")))
  :build-operation "program-op"
  :build-pathname "../bin/ProjectIsidore"
  :entry-point "project-isidore/webserver:application-toplevel")

(register-system-packages "clog" '(:clog-connection :clog-web))

(asdf:defsystem "project-isidore/tests"
  :class :package-inferred-system
  :pathname "src/tests"
  :depends-on ("project-isidore/tests/tests"
               "project-isidore/tests/nlp-tests"
               "project-isidore/tests/booking-nlp-tests"
               "project-isidore/tests/booking-llm-tests"
               "project-isidore/tests/slack-bot-tests"
               "project-isidore/tests/van-booking-tests"
               )
  :perform (asdf:test-op
            (op c)
            ;; Unlike a bare PARACHUTE:TEST, fail the operation when a suite
            ;; fails: make.lisp gates the binary on it.  TESTS/TESTS needs the
            ;; bible store under /var/local/ and the network, so it is left
            ;; out when PROJECT_ISIDORE_OFFLINE is set (guix.scm's build).
            (let ((failed
                    (loop :for suite
                            :in `(,@(unless (uiop:getenvp "PROJECT_ISIDORE_OFFLINE")
                                      '(:project-isidore/tests/tests))
                                  :project-isidore/tests/nlp-tests
                                  :project-isidore/tests/booking-nlp-tests
                                  :project-isidore/tests/booking-llm-tests
                                  :project-isidore/tests/slack-bot-tests
                                  :project-isidore/tests/van-booking-tests)
                          :when (eq :failed
                                    (uiop:symbol-call
                                     :parachute :status
                                     (uiop:symbol-call :parachute :test suite)))
                            :collect suite)))
              (when failed
                (error "Failed test suites: ~{~(~A~)~^, ~}" failed)))))
