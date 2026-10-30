;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/main
  (:use #:common-lisp)
  (:import-from #:rove #:deftest #:ok)
  (:import-from #:project-isidore/tests/test-utils #:with-temp-data-root)
  (:local-nicknames (#:main #:project-isidore/main))
  (:documentation
   "Tests for the application as a whole."))

(in-package #:project-isidore/tests/main)

;; The application starts and stops cleanly, under a data root of its own.
;; Port 3510, as 8080 is the production server's.
(deftest can-app-init-and-exit
  (with-temp-data-root ()
    (ok (main:initialize-application :port 3510))
    (ok (main:terminate-application))))
