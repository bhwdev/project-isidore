;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/migration
  (:use #:common-lisp
        #:series
        #:project-isidore/bible
        #:project-isidore/database)
  ;; No package local nicknames. See commit 1962a26.
  (:documentation
   "Data import: USFM (Unified Standard Format Markers) -> CLOS objects
backed by graph-db (Vivace Graph V3).

Source text: https://github.com/bhw-foss/ENG-B-Haydock1883-pd-PSFM

The functions in this file are sequential one-shots for re-ingesting the
bible corpus. They are not called at runtime.

Two-step import for the SFM source:
  (migrate-data \"/path/to/ENG-B-Haydock1883-pd-PSFM/\")

One-shot import from a previously dumped sexp (used by the rucksack
-> graph-db migration):
  (migrate-from-dump #P\"data/migration-dump/\")"))

(in-package #:project-isidore/migration)

;;; UNIQUE-ID assignment for fresh imports. The bible CLOS class no longer
;;; has a hook analogous to rucksack's INITIALIZE-INSTANCE :AFTER that
;;; auto-assigns unique-id (graph-db's MAKE-VERTEX skips initialize-instance
;;; on the subclass). Callers must pass :unique-id explicitly, using
;;; NEXT-UNIQUE-ID for sequential allocation.

(defvar *unique-id* -1)

(defun next-unique-id ()
  (incf *unique-id*))

(defun create-bible (heading text &key (unique-id (next-unique-id)))
  "Create a BIBLE vertex with the given HEADING and TEXT.
Auto-assigns UNIQUE-ID via NEXT-UNIQUE-ID unless explicitly supplied."
  (with-db-transaction (:context "create-bible" :graph *bible-database*)
    (make-bible :unique-id unique-id
                :heading heading
                :text text)))

(defun set-bible-slot (bible-uid slot value)
  "Update a single slot on the BIBLE vertex with UNIQUE-ID = BIBLE-UID.
Invalidates the bible-obj cache so subsequent reads see the new value."
  (with-db-transaction (:context "set-bible-slot" :graph *bible-database*)
    (let ((obj (bible-obj-with-id bible-uid)))
      (when obj
        (let ((c (graph-db:copy obj)))
          (setf (slot-value c slot) value)
          (graph-db:save c)
          (clear-bible-obj-cache)
          c)))))

(defun create-footnote (bible-uid text)
  (set-bible-slot bible-uid 'footnotes text))

(defun create-cross-reference (bible-uid text)
  (set-bible-slot bible-uid 'cross-references text))

(defun import-bible-text (file)
  "Process USFM text to vertex slots depending on markup tags."
  ;; FIXME There are two Genesis fr 1:1. The later one replaces the earlier.
  (let ((book) (chapter) (verse))
    (iterate ((line (scan (uiop:read-file-lines file))))
             (cond
               ((string-equal "\\h" (car (ppcre:split " " line)))
                (setf book (bible-book-convert-dwim (ppcre:regex-replace-all "\\\\h " line ""))))

               ((string-equal "\\im" (car (ppcre:split " " line)))
                (create-bible (nreverse (pairlis '(project-isidore/bible::book
                                                   project-isidore/bible::chapter
                                                   project-isidore/bible::verse)
                                                 (list book 0 0)))
                              (ppcre:regex-replace-all "\\\\im " line "")))

               ((string-equal "\\mt1" (car (ppcre:split " " line)))
                (setf book (bible-book-convert-dwim (ppcre:regex-replace-all "\\\\mt1 " line ""))))

               ((string-equal "\\cl" (car (ppcre:split " " line)))
                (unless (or (ppcre:all-matches "THE PROLOGUE" line)
                            (ppcre:all-matches "Psalm x. according to the Hebrews" line))
                  (setf chapter (parse-integer (car (last (ppcre:split " " line)))))))

               ((string-equal "\\cd" (car (ppcre:split " " line)))
                (create-bible (nreverse (pairlis '(project-isidore/bible::book
                                                   project-isidore/bible::chapter
                                                   project-isidore/bible::verse)
                                                 (list book chapter 0)))
                              (ppcre:regex-replace-all "\\\\cd " line "")))

               ((string-equal "\\v" (car (ppcre:split " " line)))
                (progn
                  (ppcre:do-matches-as-strings (match "\\\\v [0-9]{1,3}" line)
                    (setf verse (parse-integer (car (last (ppcre:split " " match))))))
                  (create-bible (nreverse (pairlis '(project-isidore/bible::book
                                                     project-isidore/bible::chapter
                                                     project-isidore/bible::verse)
                                                   (list book chapter verse)))
                                (ppcre:regex-replace-all "\\\\v [0-9]{1,3} " line ""))))))))

(defun import-bible-footnotes (file)
  (let ((book) (chapter) (verse) (text))
    (iterate ((line (scan (uiop:read-file-lines file))))
             (cond
               ((string-equal "\\mt1" (car (ppcre:split " " line)))
                (setf book (bible-book-convert-dwim (ppcre:regex-replace-all "\\\\mt1 " line ""))))

               ((string-equal "\\f" (car (ppcre:split " " line)))
                (ppcre:do-matches-as-strings (match "\\\\fr [0-9]{1,3}:[0-9]{1,3}" line)
                  (progn
                    (setf chapter (parse-integer (car (cl-ppcre:split ":" (car (last (ppcre:split " " match)))))))
                    (setf verse (parse-integer (cadr (cl-ppcre:split ":" (car (last (ppcre:split " " match)))))))
                    (setf text (ppcre:regex-replace-all "\\\\f \\+ \\\\fr [0-9]{1,3}:[0-9]{1,3}\\ ?\\\\ft " line ""))
                    (setf text (ppcre:regex-replace-all "\\\\f\\*" text ""))
                    (let* ((uid (get-bible-uid book chapter verse))
                           (existing (footnotes-of (bible-obj-with-id uid))))
                      (if existing
                          (create-footnote uid (concatenate 'string existing " " text))
                          (create-footnote uid text)))
                    (setf text nil))))

               ((string-equal "\\x" (car (ppcre:split " " line)))
                (ppcre:do-matches-as-strings (match "\\\\xo [0-9]{1,3}:[0-9]{1,3}" line)
                  (progn
                    (setf chapter (parse-integer (car (cl-ppcre:split ":" (car (last (ppcre:split " " match)))))))
                    (setf verse (parse-integer (cadr (cl-ppcre:split ":" (car (last (ppcre:split " " match)))))))
                    (setf text (ppcre:regex-replace-all "\\\\x \\+ \\\\xo [0-9]{1,3}:[0-9]{1,3}\\ ?\\\\xt " line ""))
                    (setf text (ppcre:regex-replace-all "\\\\x\\*" text ""))
                    (create-cross-reference (get-bible-uid book chapter verse) text)
                    (setf text nil))))))))

(defun create-search-index ()
  "Run after MIGRATE-DATA. Populates Montezuma index in data/montezuma/ keyed
by the same UNIQUE-ID values as the BIBLE vertices."
  (iterate ((x (scan-range :from 0 :upto 37198)))
           (montezuma:add-document-to-index
            *search-index*
            `(("b" . ,(bible-book-convert-dwim (cdar (heading-of (bible-obj-with-id x)))))
              ("c" . ,(cdadr (heading-of (bible-obj-with-id x))))
              ("v" . ,(cdaddr (heading-of (bible-obj-with-id x))))
              ("t" . ,(get-bible-text x))
              ("f" . ,(get-footnotes-text x))
              ("x" . ,(get-cross-references-text x))))))

(defun migrate-data (source-directory)
  "Verses must be completely imported before footnotes and cross-references."
  (setf *unique-id* -1)
  (iterate ((book-file (scan (directory (concatenate 'string source-directory "*.sfm")))))
           (import-bible-text book-file))
  (iterate ((book-file (scan (directory (concatenate 'string source-directory "*.sfm")))))
           (import-bible-footnotes book-file))
  (create-search-index))


;;; One-shot rucksack -> graph-db import. Run after producing the sexp
;;; dumps under DUMP-DIR. Idempotency: does NOT clear existing data; only
;;; run against a freshly created (empty) graph-db.

(defun read-sexp-file (path)
  (with-open-file (in path :direction :input :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*package* (find-package :keyword))
            (*read-eval* nil))
        (read in)))))

(defun migrate-from-dump (&optional
                            (dump-dir
                             (asdf:system-relative-pathname
                              :project-isidore "data/migration-dump/")))
  "Import bible vertices, bible-metadata, and slack-subscriptions from
the SEXP dump under DUMP-DIR into the currently open *BIBLE-DATABASE*
and *DATABASE* graphs."
  (let ((bible-file (merge-pathnames "bible.sexp" dump-dir))
        (metadata-file (merge-pathnames "bible-metadata.sexp" dump-dir))
        (subs-file (merge-pathnames "slack-subscriptions.sexp" dump-dir)))
    (when (probe-file bible-file)
      (format t "~&Importing bible vertices from ~A~%" bible-file)
      (let ((entries (read-sexp-file bible-file))
            (count 0))
        (dolist (entry entries)
          (with-db-transaction (:context "import-bible" :graph *bible-database*)
            (let ((args (list :unique-id (getf entry :unique-id)
                              :heading   (getf entry :heading)
                              :text      (getf entry :text))))
              (let ((xref (getf entry :cross-references)))
                (unless (eq xref :unbound)
                  (setf args (append args (list :cross-references xref)))))
              (let ((fn (getf entry :footnotes)))
                (unless (eq fn :unbound)
                  (setf args (append args (list :footnotes fn)))))
              (apply #'make-bible args)))
          (incf count)
          (when (zerop (mod count 5000))
            (format t "  ~D imported~%" count)))
        (format t "  Done: ~D bible vertices.~%" count)))

    (when (probe-file metadata-file)
      (format t "~&Importing bible-metadata from ~A~%" metadata-file)
      (let ((md (read-sexp-file metadata-file)))
        (with-db-transaction (:context "import-bible-metadata"
                              :graph *bible-database*)
          (make-bible-metadata :book-numbers (getf md :book-numbers)
                               :chapter-urls (getf md :chapter-urls)
                               :book-urls    (getf md :book-urls)))))

    (when (probe-file subs-file)
      (format t "~&Importing slack-subscriptions from ~A~%" subs-file)
      (let ((subs (read-sexp-file subs-file)))
        (dolist (entry subs)
          (add-slack-subscription (car entry) (cdr entry)))))

    (format t "~&Migration complete.~%")))
