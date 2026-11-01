;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/site/layout
  (:use #:common-lisp)
  (:import-from #:clog
                #:create-child #:create-div #:head-element #:html-document #:load-script)
  (:import-from #:clog-web #:clog-web-initialize)
  (:export #:with-page-chrome
           #:escape-html)
  (:documentation
   "The frame every page of the site shares: stylesheet, theme script, navbar
and footer, around content a page fills in."))

(in-package #:project-isidore/site/layout)

(defun escape-html (string)
  "STRING with & < > \" ' written as character references, for text or a
quoted attribute value in HTML.  CLOG's ESCAPE-FOR-HTML does only < and >."
  (with-output-to-string (out)
    (loop for ch across string
          do (case ch
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&#39;" out))
               (t (write-char ch out))))))

(defun create-navbar (parent)
  "Create the site navbar matching the blog archive style. Styled via global.css."
  (create-child parent
                "<div class='header header-fixed'><div class='navbar container'><div class='logo'><a href='/'>BHW</a></div><input type='checkbox' id='navbar-toggle'><label for='navbar-toggle'><i></i></label><nav class='menu'><ul><li><a href='/about'>About</a></li><li><a href='/work'>Work</a></li><li><a href='/assets/blog/archive.html'>Blog</a></li><li><a href='/contact'>Contact</a></li></ul></nav><button type='button' class='theme-toggle' aria-label='Colour theme'></button></div></div>"))

(defun create-footer (parent)
  "Create the site footer."
  (create-child parent
                "<footer><hr><div class='copyright-container'><div class='copyright'>Copyright (c) 2021  Ben H. W.</div></div></footer>"))

(defun with-page-chrome (body content-fn)
  "Set up a page with navbar, main content area, and footer.
CONTENT-FN is called with the main content div as argument."
  (clog-web-initialize body)
  (create-child (head-element (html-document body))
                "<link rel='stylesheet' href='/assets/global.css' type='text/css'>")
  ;; Sets the reader's palette (parchment unless they chose dark) and drives
  ;; the navbar's theme toggle.  Waited for, so the page is built in the
  ;; right palette rather than switching after.
  (load-script (html-document body) "/assets/theme.js")
  (create-navbar body)
  (let ((main (create-div body :class "main")))
    (funcall content-fn main))
  (create-footer body))

