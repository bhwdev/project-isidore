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
;; PARTITION-IF and PARTITION-IF-NOT.  Those exports clash with
;; SERAPEUM:PARTITION inside SERAPEUM/BUNDLE (which re-exports both serapeum
;; and split-sequence) whichever of serapeum or stdutils loads first.  The
;; PERFORM methods below resolve the clash around that one file: before it is
;; compiled or loaded, SERAPEUM/BUNDLE (if it already exists) keeps serapeum's
;; PARTITION explicitly; after it loads, the stray exports are removed so a
;; later serapeum load sees the split-sequence 2.x API it expects.
;;
;; DEFPACKAGE takes effect at compile time as well, so the "before" step must
;; wrap COMPILE-OP too, not only LOAD-OP: a machine that has never compiled
;; stdutils (a fresh deploy, or a cleared fasl cache) otherwise hits the
;; NAME-CONFLICT inside COMPILE-FILE, before any LOAD-OP hook can run.
(defun stdutils-split-sequence-file-p (component)
  (and (typep component 'asdf:cl-source-file)
       (string= (asdf:component-name component) "split-sequence")
       (string-equal (asdf:component-name (asdf:component-system component)) "stdutils")))

(defun keep-serapeum-partition-in-bundle (component)
  (when (stdutils-split-sequence-file-p component)
    (let ((bundle (find-package "SERAPEUM/BUNDLE"))
          (partition (and (find-package "SERAPEUM")
                          (find-symbol "PARTITION" "SERAPEUM"))))
      (when (and bundle partition)
        (shadowing-import partition bundle)))))

(defmethod asdf:perform :before ((op asdf:compile-op) (component asdf:cl-source-file))
  (keep-serapeum-partition-in-bundle component))

(defmethod asdf:perform :before ((op asdf:load-op) (component asdf:cl-source-file))
  (keep-serapeum-partition-in-bundle component))

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
  :bug-tracker "https://gitlab.com/bhw/project-isidore/-/issues"
  :source-control (:git "https://gitlab.com/bhw/project-isidore.git")
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
            (uiop:symbol-call :parachute :test :project-isidore/tests/tests)
            (uiop:symbol-call :parachute :test :project-isidore/tests/nlp-tests)
            (uiop:symbol-call :parachute :test :project-isidore/tests/booking-nlp-tests)
            (uiop:symbol-call :parachute :test :project-isidore/tests/booking-llm-tests)
            (uiop:symbol-call :parachute :test :project-isidore/tests/slack-bot-tests)
            (uiop:symbol-call :parachute :test :project-isidore/tests/van-booking-tests)
            ))

(asdf:defsystem "project-isidore/infra"
  :serial t
  :pathname "src/infra"
  :depends-on (#:consfigurator)
  :components ((:file "production")))
