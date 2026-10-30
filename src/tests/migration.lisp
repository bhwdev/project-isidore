;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/migration
  (:use #:common-lisp)
  (:import-from #:graph-db)
  (:import-from #:rove #:deftest #:testing #:ok #:ng #:signals)
  (:import-from #:project-isidore/tests/test-utils
                #:with-mocked-functions #:with-temp-data-root)
  (:local-nicknames (#:legacy #:project-isidore/database)
                    (#:store #:project-isidore/store)
                    (#:bible-db #:project-isidore/bible/database)
                    (#:bot-db #:project-isidore/slack-bot/database))
  (:documentation
   "Tests for the one-time copy of the pre-split stores into the bible's and
the Slack bot's (PROJECT-ISIDORE/DATABASE).  Delete with that file."))

(in-package #:project-isidore/tests/migration)

(defparameter *verses*
  '((:unique-id 10 :heading ((:book . 1) (:chapter . 1) (:verse . 1))
     :text "In the beginning God created heaven, and earth."
     :cross-references nil :footnotes "Heaven and earth. That is, ...")
    (:unique-id 11 :heading ((:book . 1) (:chapter . 1) (:verse . 2))
     :text "And the earth was void and empty, ..."
     :cross-references "Psalm 32:6." :footnotes nil))
  "Two verses, as the old store's BIBLE vertices hold them.")

(defun make-old-stores ()
  "Make stores of the old layout under the data root: a bible with *VERSES*
and a metadata vertex, and a store with one subscription and two identities."
  (setf graph-db:*system-directory*
        (namestring (ensure-directories-exist
                     (uiop:subpathname (store:data-root) "graph-db-system/"))))
  (flet ((make (name location)
           (let ((graph (graph-db:make-graph name (ensure-directories-exist location)
                                             :buffer-pool-p nil
                                             :heap-size (* 8 1024 1024)
                                             :index-size (* 8 1024 1024))))
             (graph-db:install-views graph)
             graph)))
    (let ((bible (make :isidore-bible legacy:*bible-store-path*))
          (crud (make :isidore-crud (uiop:subpathname (store:data-root) "graph-db/"))))
      (unwind-protect
           (progn
             (let ((graph-db:*graph* bible))
               (graph-db:with-transaction ()
                 (project-isidore/database::make-bible-metadata
                  :book-numbers '(("Genesis" . 1)) :chapter-urls '(("/bible/1-1-1" . "Genesis 1"))
                  :book-urls '(("/bible/1-1-1/1-1-31" . "Genesis")))
                 (dolist (verse *verses*)
                   (apply #'project-isidore/database::make-bible verse))))
             (let ((graph-db:*graph* crud))
               (graph-db:with-transaction ()
                 (project-isidore/database::make-slack-subscription :user-id "U1" :initial "BW")
                 (project-isidore/database::make-slack-identity :user-id "U1" :initial "BW")
                 (project-isidore/database::make-slack-identity :user-id "U2" :initial "CG"))))
        (store:close-store bible)
        (store:close-store crud)))))

(defmacro with-old-stores ((root) &body body)
  "Run BODY under a temporary data root, ROOT, holding the stores of
MAKE-OLD-STORES, with the bible's old and new paths under it too."
  `(with-temp-data-root (,root)
     (let ((legacy:*bible-store-path* (uiop:subpathname ,root "old-bible/"))
           (bible-db:*store-path* (uiop:subpathname ,root "bible/")))
       (make-old-stores)
       ,@body)))

(deftest old-records-read-back
  ;; Production's graph-db-system/type-registry.log and schema-manifest.dat,
  ;; as they spell the old symbols.  They are read back with the Lisp reader,
  ;; so every one written with a single colon must be external in
  ;; PROJECT-ISIDORE/DATABASE, or the old stores cannot be opened (the
  ;; 2026-10-29 deploy failed on exactly this).
  (dolist (record '("(:SYMBOL PROJECT-ISIDORE/DATABASE:BIBLE :PARENT :VERTEX :ID 1)"
                    "(:SYMBOL PROJECT-ISIDORE/DATABASE:BIBLE-METADATA :PARENT :VERTEX :ID 2)"
                    "(:SYMBOL PROJECT-ISIDORE/DATABASE:SLACK-IDENTITY :PARENT :VERTEX :ID 3)"
                    "(:SYMBOL PROJECT-ISIDORE/DATABASE:SLACK-SUBSCRIPTION :PARENT :VERTEX :ID 4)"
                    "(PROJECT-ISIDORE/DATABASE::BOOK-NUMBERS :ACCESSOR PROJECT-ISIDORE/DATABASE:BOOK-NUMBERS-OF)"
                    "(PROJECT-ISIDORE/DATABASE::CHAPTER-URLS :ACCESSOR PROJECT-ISIDORE/DATABASE:CHAPTER-URLS-OF)"
                    "(PROJECT-ISIDORE/DATABASE::BOOK-URLS :ACCESSOR PROJECT-ISIDORE/DATABASE:BOOK-URLS-OF)"
                    "(PROJECT-ISIDORE/DATABASE::UNIQUE-ID :ACCESSOR PROJECT-ISIDORE/DATABASE:UNIQUE-ID-OF)"
                    "(PROJECT-ISIDORE/DATABASE::HEADING :ACCESSOR PROJECT-ISIDORE/DATABASE:HEADING-OF)"
                    "(PROJECT-ISIDORE/DATABASE::TEXT :ACCESSOR PROJECT-ISIDORE/DATABASE:TEXT-OF)"
                    "(PROJECT-ISIDORE/DATABASE::CROSS-REFERENCES :ACCESSOR PROJECT-ISIDORE/DATABASE:CROSS-REFERENCES-OF)"
                    "(PROJECT-ISIDORE/DATABASE::FOOTNOTES :ACCESSOR PROJECT-ISIDORE/DATABASE:FOOTNOTES-OF)"
                    "(PROJECT-ISIDORE/DATABASE::USER-ID :ACCESSOR PROJECT-ISIDORE/DATABASE:USER-ID-OF)"
                    "(PROJECT-ISIDORE/DATABASE::INITIAL :ACCESSOR PROJECT-ISIDORE/DATABASE:INITIAL-OF)"))
    (ok (consp (ignore-errors (read-from-string record))) record)))

(deftest migrate-stores-copies-everything
  (with-old-stores (root)
    (legacy:migrate-stores)
    (testing "the bible"
      (bible-db:open-database)
      (unwind-protect
           (progn
             (ok (string= "In the beginning God created heaven, and earth."
                          (bible-db:text-of (bible-db:bible-obj-with-id 10))))
             (ok (equal "Psalm 32:6." (bible-db:cross-references-of (bible-db:bible-obj-with-id 11))))
             (ok (null (bible-db:footnotes-of (bible-db:bible-obj-with-id 11))))
             (ok (= 11 (gethash '(1 1 2) bible-db:+bible-heading-to-uid+)))
             (ok (equal '(("Genesis" . 1)) bible-db:+bible-book-numbers-alist+)))
        (bible-db:close-database)))
    (testing "the bot's data"
      (bot-db:open-database)
      (unwind-protect
           (progn
             (ok (equal '(("U1" . "BW")) (bot-db:all-slack-subscriptions)))
             (ok (equal '(("U1" . "BW") ("U2" . "CG"))
                        (sort (bot-db:all-slack-identities) #'string< :key #'car))))
        (bot-db:close-database)))
    (testing "the old stores are left as they were"
      (ok (store:store-exists-p legacy:*bible-store-path*))
      (ok (store:store-exists-p (uiop:subpathname root "graph-db/"))))
    (testing "a second run does nothing"
      (with-mocked-functions ((legacy::read-bible (lambda () (error "read again"))))
        (ok (null (legacy:migrate-stores)))))))

(deftest migrate-stores-leaves-nothing-behind-on-error
  (with-old-stores (root)
    (with-mocked-functions ((bot-db:add-slack-identity
                             (lambda (user-id initial)
                               (declare (ignore user-id initial))
                               (error "disk full"))))
      (ok (signals (legacy:migrate-stores) 'error)))
    (ng (uiop:directory-exists-p (store:system-directory)))
    (ng (uiop:directory-exists-p bible-db:*store-path*))
    (ng (uiop:directory-exists-p (bot-db:store-path)))
    ;; so the next start migrates again
    (legacy:migrate-stores)
    (ok (store:store-exists-p bible-db:*store-path*))))
