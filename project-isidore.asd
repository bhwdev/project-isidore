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
               "project-isidore/tests/slack-bot-tests"
               "project-isidore/tests/van-booking-tests"
               )
  :perform (asdf:test-op
            (op c)
            (uiop:symbol-call :parachute :test :project-isidore/tests/tests)
            (uiop:symbol-call :parachute :test :project-isidore/tests/slack-bot-tests)
            (uiop:symbol-call :parachute :test :project-isidore/tests/van-booking-tests)
            ))

(asdf:defsystem "project-isidore/infra"
  :serial t
  :pathname "src/infra"
  :depends-on (#:consfigurator)
  :components ((:file "production")))
