;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/main
  (:use #:common-lisp)
  (:import-from #:rove #:deftest #:ok #:ng)
  (:import-from #:usocket)
  (:import-from #:project-isidore/tests/test-utils #:with-temp-data-root)
  (:local-nicknames (#:main #:project-isidore/main)
                    (#:layout #:project-isidore/site/layout))
  (:documentation
   "Tests for the application as a whole."))

(in-package #:project-isidore/tests/main)

(defun http-status (port request-line &rest header-lines)
  "Send REQUEST-LINE and HEADER-LINES, verbatim, as an HTTP/1.0 request with
no body to localhost:PORT, and return the status code of the reply.  Raw, so
that a path reaches the server exactly as written."
  (let ((socket (usocket:socket-connect "127.0.0.1" port)))
    (unwind-protect
         (let ((stream (usocket:socket-stream socket))
               (crlf (coerce '(#\Return #\Linefeed) 'string)))
           (format stream "~A~A~{~A~A~}~A" request-line crlf
                   (loop for line in header-lines collect line collect crlf)
                   crlf)
           (finish-output stream)
           (parse-integer (read-line stream) :start 9 :end 12))
      (usocket:socket-close socket))))

(deftest safe-paths
  (dolist (path '("/" "/about" "/assets/global.css" "/assets/blog/archive.html"
                  "/bible/genesis-1-1/genesis-1-5" "/bible/47~4~2" "/bible/1.2.10/"
                  "/bible-search" "/js/boot.js" "/clog" "/slack/events"))
    (ok (main::safe-path-p path) path))
  (dolist (path (list "//etc/passwd" "/assets//etc/passwd" "/~/.bashrc" "/~root/x"
                      "/assets/../src/main.lisp" "/.git/config" "/assets/.hidden"
                      "/SYS:SRC;CODE;TARGET-PATHNAME.LISP" "/a\\b" "/a b" "/a*"
                      (format nil "/caf~C" (code-char 233)) "relative" nil))
    (ng (main::safe-path-p path) path)))

;; What a page quotes from the URL cannot open a tag or leave an attribute.
(deftest html-escaping
  (ok (string= "&lt;img src=x onerror=&quot;a(&#39;1&#39;)&quot;&gt; &amp;amp;"
               (layout:escape-html "<img src=x onerror=\"a('1')\"> &amp;")))
  (ok (string= "/bible/genesis-1-1" (layout:escape-html "/bible/genesis-1-1"))))

(deftest request-guard
  (let ((handler (funcall (main::make-request-guard)
                          (lambda (env) (declare (ignore env)) '(200 () ("app"))))))
    (flet ((status (&rest env) (first (funcall handler env))))
      (ok (= 200 (status :path-info "/about")))
      (ok (= 200 (status :path-info "/slack/events" :content-length 1024)))
      (ok (= 400 (status :path-info "//etc/passwd")))
      (ok (= 413 (status :path-info "/about" :content-length (1+ main::*max-request-octets*))))
      (ok (= 413 (status :path-info "/about" :content-length -1)))
      (let ((headers (make-hash-table :test #'equal)))
        (setf (gethash "transfer-encoding" headers) "chunked")
        (ok (= 411 (status :path-info "/slack/events" :headers headers)))))))

;; The application starts and stops cleanly, under a data root of its own,
;; and serves nothing outside its static root.  Port 8293, as 8080 is the
;; production server's.
(deftest can-app-init-and-exit
    (with-temp-data-root ()
      (ok (main:initialize-application :port 8293))
      (unwind-protect
           (progn
             (ok (= 200 (http-status 8293 "GET /assets/global.css HTTP/1.0")))
             ;; Without the guard each of these is a file outside the static
             ;; root: an absolute path (this system's own .asd), the home
             ;; directory, and the checkout's .gitignore.
             (dolist (path (list (format nil "/~A" (namestring
                                                    (asdf:system-relative-pathname
                                                     :project-isidore "project-isidore.asd")))
                                 "/%2Fetc/passwd"
                                 "/~/.profile"
                                 "/.gitignore"))
               (ok (= 400 (http-status 8293 (format nil "GET ~A HTTP/1.0" path))) path))
             ;; CLOG would make a string of this many characters and exhaust
             ;; the heap, taking the process down with it.
             (ok (= 413 (http-status 8293 "POST /about HTTP/1.0"
                                     "Content-Type: application/x-www-form-urlencoded"
                                     "Content-Length: 1000000000000")))
             (ok (= 200 (http-status 8293 "GET /assets/global.css HTTP/1.0"))))
        (ok (main:terminate-application)))))
