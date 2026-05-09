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
  :depends-on ("graph-db"
               "project-isidore/packages"
               "project-isidore/van-booking"
               "project-isidore/slack-bot"
               "project-isidore/garmin-org"
               )
  :in-order-to ((asdf:test-op (asdf:test-op "project-isidore/test")))
  :build-operation "program-op"
  :build-pathname "../bin/ProjectIsidore"
  :entry-point "project-isidore:application-toplevel")

(register-system-packages "project-isidore/packages" '(:project-isidore))
(register-system-packages "clog" '(:clog-connection :clog-web))

(asdf:defsystem "project-isidore/test"
  :class :package-inferred-system
  :pathname "src/test"
  :depends-on ("fxml"
               "project-isidore/test/tests"
               "project-isidore/test/slack-bot-tests"
               "project-isidore/test/van-booking-tests"
               )
  :perform (asdf:test-op
            (op c)
            (uiop:symbol-call :parachute :test :project-isidore/test/tests)
            (uiop:symbol-call :parachute :test :project-isidore/test/slack-bot-tests)
            (uiop:symbol-call :parachute :test :project-isidore/test/van-booking-tests)
            ))

(asdf:defsystem "project-isidore/infra"
  :serial t
  :pathname "src/infra"
  :depends-on (#:consfigurator)
  :components ((:file "production")))
