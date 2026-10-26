;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop/package:define-package
    #:project-isidore/bible
    (:use #:common-lisp #:series
          #:project-isidore/database)
  (:local-nicknames (#:search-engine #:montezuma))
  (:export :bible :unique-id-of :heading-of :text-of
           :cross-references-of :footnotes-of
           #:get-bible-uid #:bible-obj-with-id
           #:clear-bible-obj-cache
           #:valid-bible-uid-p #:get-bible-text
           #:get-heading-text #:get-footnotes-text
           #:get-cross-references-text
           #:get-bible-verse-data
           #:get-footnotes-text-with-links
           #:get-cross-references-text-with-links
           #:encode-cross-references-to-html-links
           #:bible-book-convert-dwim
           #:make-bible-chapter-url-list
           #:+bible-book-numbers-alist+
           #:+bible-chapter-url-alist+
           #:+bible-book-url-alist+ #:+bible-heading-to-uid+
           #:+bible-uid-to-heading+ #:search-bible
   :*reference-regex* #:reopen-fd-streams)
  (:documentation "Project Isidore Object Schema.

Data serialization & interchange.
How to represent tree structure? XML, JSON or SEXP?
https://unthought.net/2016/08/16-xml-json-trees-and-lisp/
https://eli.thegreenplace.net/2012/03/04/some-thoughts-on-json-vs-s-expressions

Microsoft Word uses compressed XML. Web browsers render (X)HTML. Jupyter
notebooks use JSON. I see no benefits to inserting an extra layer. CLOS Object >
JSON > (X)HTML.

"))

(in-package #:project-isidore/bible)

(defun get-bible-uid (book chapter verse)
  "Return a unique identifier assigned to each instance of class `bible'.
  Uses a pre-built hash table for O(1) lookup instead of scanning all objects.

  Example:
  (get-bible-uid 47 3 6) => 27917
  (get-bible-uid 12983 29394 2938498) => NIL "
  (gethash (list book chapter verse) +bible-heading-to-uid+))

(defun valid-bible-uid-p (test-uid)
  "If TEST-UID is a valid bible unique ID return T, else NIL.
TEST-UID must be of type INTEGER.

> (valid-bible-uid-p 1234)
T
> (valid-bible-uid-p 23898389)
NIL
> (valid-bible-uid-p '98fj)
SIMPLE-TYPE-ERROR
"
  (check-type test-uid integer)
  (if (gethash test-uid +bible-uid-to-heading+) t nil))

(defun bible-book-convert-dwim (book)
  "Given a Bible book string name or integer, convert to the opposite format.

   Example:
   (bible-book-convert-dwim \"Matthew\") => 47
   (bible-book-convert-dwim 47) => \"Matthew\" "
  (if (stringp book)
      ;; string-equal is case insensitive. string= is case sensitive.
      (cdr (assoc book +bible-book-numbers-alist+ :test #'string-equal))
      (car (rassoc book +bible-book-numbers-alist+))))

(defun get-bible-text (bible-uid)
  "Returns the text slot of object class `bible' in a string. If BIBLE-UID is
invalid return NIL. The BIBLE-UID can be found by calling `get-bible-uid'."
  (text-of (bible-obj-with-id bible-uid)))

(defun get-bible-verse-data (bible-uid)
  "Fetch a bible object once and return all display-relevant data as a plist.
Returns (:heading STRING :text STRING :cross-references STRING-OR-NIL
         :footnotes STRING-OR-NIL :internal-chapter INTEGER)."
  (let* ((obj (bible-obj-with-id bible-uid))
         (heading-alist (heading-of obj))
         (book (bible-book-convert-dwim (cdar heading-alist)))
         (chapter (write-to-string (cdadr heading-alist)))
         (verse (write-to-string (cdaddr heading-alist))))
    (list :heading (concatenate 'string book " " chapter ":" verse)
          :text (text-of obj)
          :cross-references (cross-references-of obj)
          :footnotes (footnotes-of obj)
          :internal-chapter (cdar heading-alist))))

(defun get-heading-text (bible-uid)
  "Returns the title slot of object class `bible' in a string. If BIBLE-UID is
 invalid return NIL. The BIBLE-UID can be found by calling `get-bible-uid'."
  (let* ((heading-alist (heading-of (bible-obj-with-id bible-uid)))
         (book (bible-book-convert-dwim (cdar heading-alist)))
         (chapter (write-to-string (cdadr heading-alist)))
         (verse (write-to-string (cdaddr heading-alist))))
    (concatenate 'string book " " chapter ":" verse )))

(defun get-footnotes-text (bible-uid)
  "Returns a string if bible-uid is valid else return NIL.
  Only get text if commentary exists for given BIBLE-UID.
The bible-uid can be found by calling `get-bible-uid' with valid arguments.

Example:

(get-footnotes-text (get-bible-uid (bible-book-convert-dwim \"Leviticus\") 2 3))

=> \"Holy of holies. That is, most holy; as being dedicated to God, and set
aside by his ordinance for the use of his priests. (Challoner) --- All was to be
eaten or consumed in the tabernacle. The high priest offered a gomor full of
flour and oil, rather baked, every day, Leviticus 6:20. (Calmet)\"

(get-footnotes-text (get-bible-uid (bible-book-convert-dwim \"Leviticus\") 999 999))

=> The value
     NIL
   is not of type
     REAL
      [Condition of type TYPE-ERROR] "
  (let ((obj (bible-obj-with-id bible-uid)))
    (when obj (footnotes-of obj))))

(defun get-cross-references-text (bible-uid)
  "Returns a string if bible-uid is valid else return NIL.
  Only get text if commentary exists for given BIBLE-UID.
The bible-uid can be found by calling `get-bible-uid' with valid arguments."
  (let ((obj (bible-obj-with-id bible-uid)))
    (when obj (cross-references-of obj))))

(defun bible-ref-to-url (string bible-uid &optional internal-chapter)
  "Haydock commentary cross references are in the form BOOK, CHAPTER, VERSE. Ex.
\"Psalm 12:9.\". BIBLE-UID is to handle the special abbreviated case where the
commentary cross references are in the anaphoric form \"C. xxiii. 3.\" where C.
is short form for (same) chapter. When INTERNAL-CHAPTER is supplied, avoids a
DB lookup; otherwise fetches it from the bible object.

Example:

(bible-ref-to-url \"Leviticus 6:20\"
                  (get-bible-uid (bible-book-convert-dwim \"Leviticus\") 2 3))

=> \"/bible/3-6-20\" "
  (let ((internal-chapter (or internal-chapter
                              (cdar (heading-of (bible-obj-with-id bible-uid)))))
        (book nil)
        (chapter nil)
        (verse nil))
    (loop for metadata-list in (cl-ppcre:split "\\s" (remove #\. string))
          for parsed = (parse-integer metadata-list :junk-allowed t)
          ;; 35019
          ;; ("" "Ps." "cxliv." "9.")
          do (cond
               ;; Discard empty string.
               ((string-equal "" metadata-list) nil)
               ;; Book is nil, decimal number found.
               ((and (not book) parsed)
                (setf book (write-to-string parsed)))
               ;; No decimal number found.
               ((not parsed)
                (if book
                    (setf book (concatenate 'string book " "
                                            (string-trim '(#\Space #\Tab #\Newline) metadata-list)))
                    (setf book (string-trim '(#\Space #\Tab #\Newline) metadata-list))))
               ;; Book is not nil, decimal number found.
               ((and book parsed)
                (progn
                  (setf chapter parsed)
                  (setf verse (parse-integer (car (last (cl-ppcre:split ":" metadata-list))) :junk-allowed t))
                  (setf book (cond ((integerp book) book)
                                   ((string-equal "C" book) internal-chapter)
                                   (t (cdr (assoc book +bible-book-numbers-alist+ :test #'string-equal)))))
                  (return-from bible-ref-to-url
                    (if (not book)
                        nil
                        (concatenate
                         'string "/bible/"
                         (write-to-string book) "-"
                         (write-to-string chapter) "-"
                         (write-to-string verse))))))))))

(defparameter *reference-regex*
  (ppcre:create-scanner "([1-4]?\\s?[a-zA-Z]{1,15}\\s[0-9]{1,3}\\:[0-9]{1,3})")
  "Regex explanation.
[1-4]? 4 K. is the highest for 4 Kings. ? signifies the number may or may not be
there.
\s whitespace.
[a-zA-Z]{1,15} Paralipomenon is the longest book name I have found thus far.
\. optional period.
\s whitespace.
[0-9]{1,3} 3 digits that make up a chapter number.
\: colon.
[0-9]{1,3} 3 digits that make up a verse number.
An extra backslash is need to escape the backslash itself.")

(defun encode-cross-references-to-html-links (string bible-uid
                                              &optional internal-chapter)
  "Where STRING can be any footnote or cross-reference text and BIBLE-UID must be
corresponding. When INTERNAL-CHAPTER is supplied, passes it to `bible-ref-to-url'
to avoid redundant DB lookups.

Example:

(encode-cross-references-to-html-links
 (get-cross-references-text (get-bible-uid (bible-book-convert-dwim \"Amos\") 5 18))
 (get-bible-uid (bible-book-convert-dwim \"Amos\") 5 18))

=> \"<a href=\"/bible/28-30-7\">Jeremias 30:7</a>.;<a href=\"/bible/34-2-11\">
Joel 2:11</a>.;<a href=\"/bible/41-1-15\"> Sophonias 1:15</a>.\", T "
  (flet ((convert (match y)
           (let ((url (bible-ref-to-url match bible-uid internal-chapter)))
             (if (not url)
                 (format nil "~a" match)
                 (format nil "<a href=\"~a\">~a</a>" url y)))))
    (ppcre:regex-replace-all *reference-regex* string #'convert :simple-calls t)))

(defun get-footnotes-text-with-links (bible-uid)
  "Unlike `get-footnotes-text', the 'with-links' variation will take the cross
references in the footnote text (if any exist) and
`encode-cross-references-to-html-links'. Return NIL otherwise.

Example:

(get-footnotes-text-with-links
 (get-bible-uid (bible-book-convert-dwim \"Leviticus\") 2 3))

=> \"Holy of holies. That is, most holy; as being dedicated to God, and set
aside by his ordinance for the use of his priests. (Challoner) --- All was to be
eaten or consumed in the tabernacle. The high priest offered a gomor full of
flour and oil, rather baked, every day,<a href=\"/bible/3-6-20\"> Leviticus
6:20</a>. (Calmet)\", T "
  (let ((footnotes-text (get-footnotes-text bible-uid)))
    (when footnotes-text
      (encode-cross-references-to-html-links footnotes-text bible-uid))))

(defun get-cross-references-text-with-links (bible-uid)
  "Unlike `get-cross-references-text', the 'with-links' variation will take the
cross references in the verse itself (if any exist) and
`encode-cross-references-to-html-links'. Return NIL otherwise.

Example:

(get-cross-references-text-with-links
 (get-bible-uid (bible-book-convert-dwim \"Leviticus\") 2 3))

=> \"<a href=\"/bible/26-7-34\">Ecclesiasticus 7:34</a>.\", T"
  (let ((cross-references-text (get-cross-references-text bible-uid)))
    (when cross-references-text
      (encode-cross-references-to-html-links cross-references-text bible-uid))))

(defun make-bible-chapter-url-list (uid-list)
  "Selects the right links from `+bible-chapter-url-alist+' based on the interval
created from the first and last elements of list UID-LIST. Note that for an
interval that is within a single book or single chapter, the function is
recursed to cover all chapters within the book. If this is not done during the
former case of a single book, then the last chapter will be cut off as a result
of an off by one error.

Example:

(project-isidore/bible:make-bible-chapter-url-list '(10 91)) =>

((\"/bible/1-1-1/1-1-31\" . \"Genesis 1\")
 (\"/bible/1-2-1/1-2-25\" . \"Genesis 2\")
 (\"/bible/1-3-1/1-3-24\" . \"Genesis 3\"))"
  (let* ((beginning-uid (first uid-list))
         (ending-uid (first (last uid-list)))
         (beginning-heading (gethash beginning-uid +bible-uid-to-heading+))
         (ending-heading (gethash ending-uid +bible-uid-to-heading+))
         (beginning-book (cdar beginning-heading))
         (ending-book (cdar ending-heading))
         (beginning-chapter (cdadr beginning-heading))
         (ending-chapter (cdadr ending-heading)))
    (flet ((heading-text (uid)
             (let* ((h (gethash uid +bible-uid-to-heading+))
                    (book (bible-book-convert-dwim (cdar h)))
                    (chapter (write-to-string (cdadr h))))
               (concatenate 'string book " " chapter))))
      ;; Special case: last book of the bible.
      (cond ((<= (get-bible-uid 73 0 0) beginning-uid)
             (nthcdr 1312 +bible-chapter-url-alist+))
            ((= beginning-book ending-book)
             (make-bible-chapter-url-list
              (list (get-bible-uid
                     beginning-book
                     ;; call with 1, as `+bible-chapter-url-alist+' has no chapter 0's.
                     1 1)
                    (get-bible-uid
                     (+ 1 ending-book)
                     1 1))))
            ((and (= beginning-book ending-book)
                  (= beginning-chapter ending-chapter))
             (make-bible-chapter-url-list
              (list (get-bible-uid
                     beginning-book
                     1 1)
                    (get-bible-uid
                     (+ 1 ending-book)
                     1 1))))
            (t
             (reverse
              (set-difference
               (nthcdr
                (position
                 (rassoc (heading-text beginning-uid)
                         +bible-chapter-url-alist+
                         :test #'string-equal)
                 +bible-chapter-url-alist+)
                +bible-chapter-url-alist+)
               (nthcdr
                (position
                 (rassoc (heading-text ending-uid)
                         +bible-chapter-url-alist+
                         :test #'string-equal)
                 +bible-chapter-url-alist+)
                +bible-chapter-url-alist+))))))))

(defparameter *search-index*
  ;; Montezuma creates a missing index directory, which a sandboxed build
  ;; cannot do under /var/local/ (see BIBLE-STORE-PRESENT-P).
  (when (uiop:directory-exists-p (index-path))
    (make-instance 'search-engine:index
                   :path (index-path)
                   :default-field "*"
                   :fields '("b" "c" "v" "t" "f" "x")))
  "Used in `search-bible' to query Bible data. Since `sb-ext:save-lisp-and-die'
  closes all open streams, this parameter is set during
  `initialize-application'.")

(defun search-bible (query &optional options)
  "Searches the Bible and Haydock's commentary. Returns an association list of
Bible unique ID's and a relevance score "
  (let ((results '()))
    (search-engine:search-each *search-index* query
                               #'(lambda (doc score)
                                   (declare (optimize)
                                            (fixnum doc))
                                   (push (cons doc score) results))
                               options)
    (nreverse results)))



(defun reopen-fd-streams ()
  " To create an executable binary, MAKE.LISP calls
`sb-ext:save-lisp-and-die', which closes all open file streams. We open
`*search-index*', `*bible-database*', and `*database*' again upon application
start. `reopen-database' re-resolves PROJECT_ISIDORE_DATA so a binary built
locally can run against /var/local/project-isidore/ on production."
  (setf *search-index* (make-instance 'search-engine:index
                                      :path (index-path)
                                      :default-field "*"
                                      :fields '("b" "c" "v" "t" "h")))
  (reopen-bible-database)
  (reopen-database))
