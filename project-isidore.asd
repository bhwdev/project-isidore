;;;; SPDX-FileCopyrightText: 2021  Ben H. W. <foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

#-asdf3.3 (error "Project Isidore requires ASDF 3.3 or later. Please upgrade your ASDF. See commit bf210b8.")

(asdf:defsystem "project-isidore"
  :name "Project Isidore"
  :version "1.2.2"
  :author " Ben H. W.<foss@bhw.name>"
  :maintainer " Ben H. W.<foss@bhw.name>"
  :description "Personal Web Application"
  :long-description
  "Project Isidore is a personal web application: a website, a Douay-Rheims
Bible reader and a Slack bot that books vans, served by one process."
  :license  "GNU Affero General Public License 3.0 or later"
  :homepage "https://www.bhw.name/blog/project-isidore-doc.html"
  :bug-tracker "https://github.com/bhwdev/project-isidore/issues"
  :source-control (:git "https://github.com/bhwdev/project-isidore.git")
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("project-isidore/main")
  :in-order-to ((asdf:test-op (asdf:test-op "project-isidore/tests"))))

(register-system-packages "clog" '(:clog-connection :clog-web))
;; The engine and its replication transport, without the HTTP API that the
;; full "graph-db" system adds (guix.scm builds only these two).
(register-system-packages "graph-db/replication" '(:graph-db))

(asdf:defsystem "project-isidore/tests"
  :class :package-inferred-system
  :pathname "src/tests"
  :depends-on ("rove"
               "project-isidore/tests/main"
               "project-isidore/tests/bible"
               "project-isidore/tests/slack-bot/nlp"
               "project-isidore/tests/slack-bot/booking-nlp"
               "project-isidore/tests/slack-bot/van-booking"
               "project-isidore/tests/slack-bot/bot")
  ;; Run the suite of each test package above by name: ROVE:RUN finds a
  ;; system's suites through the fasls it loaded, which it cannot map back to
  ;; their packages under Guix's output translations, and so runs none in the
  ;; guix.scm build.  And unlike ROVE:RUN, fail the operation when a test
  ;; fails, so that the build stops there.
  :perform (asdf:test-op (op c)
             (let ((failed
                     (loop :for name :in (asdf:system-depends-on c)
                           :when (and (uiop:string-prefix-p "project-isidore/tests/" name)
                                      (not (uiop:symbol-call
                                            :rove :run-suite
                                            (intern (string-upcase name) :keyword))))
                             :collect name)))
               (when failed
                 (error "Failed tests: ~{~A~^, ~}" failed)))))
