;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/webserver
  ;; http://fare.tunes.org/files/asdf3/asdf3-2014.html#(part._.Package_.Upgrade)
  (:mix #:common-lisp #:series #:alexandria #:serapeum)
  (:mix-reexport #:clog #:clog-web #:hunchentoot)
  (:import-from #:ironclad)
  (:import-from #:bordeaux-threads)
  (:local-nicknames
   (#:conn      #:clog-connection)
   (#:bible     #:project-isidore/bible)
   (#:slack-bot #:project-isidore/slack-bot))
  (:export :*server*
           #:initialize-application
           #:terminate-application
           #:application-toplevel)
  (:documentation
   "Project Isidore Web Interface.

Starting the Web server for the application is defined by
`initialize-application'. All page content is constructed via CLOG WebSocket
handlers using `with-page-chrome' for page layout.

Unifies CLOG and Hunchentoot under one namespace alongside application
controller logic.

1. CLOG provides WebSocket-based connectivity (RFC 6455) and URL routing via
boot-function and set-on-new-window.

2. Hunchentoot provides the underlying HTTP server (used by CLOG via Clack)."))

(in-package #:project-isidore/webserver)

(defvar *server* nil
  "Non-NIL when the CLOG web server is running.")

;; Override CLOG-CONNECTION::RANDOM-HEX-STRING. Upstream's POSIX path uses
;; cl-isaac's RAND32, which unconditionally DECFs an (UNSIGNED-BYTE 32)
;; struct slot before checking for zero. With (SAFETY 2) from make.lisp, the
;; slot writer signals a TYPE-ERROR on the underflow, wedging the PRNG --
;; every subsequent call retraps, so CLOG can no longer mint connection IDs
;; and fresh browser sessions never receive the bootstrap frame. CLOG's own
;; Windows branch already uses ironclad; we extend that to all platforms.
(defun conn::random-hex-string ()
  "Generate a cryptographic 128-bit random hex string for connection IDs."
  (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))

;;; Page Layout Helpers

(defun create-navbar (parent)
  "Create the site navbar matching the blog archive style. Styled via global.css."
  (create-child parent
                "<div class='header header-fixed'><div class='navbar container'><div class='logo'><a href='/'>BHW</a></div><input type='checkbox' id='navbar-toggle'><label for='navbar-toggle'><i></i></label><nav class='menu'><ul><li><a href='/about'>About</a></li><li><a href='/work'>Work</a></li><li><a href='/assets/blog/archive.html'>Blog</a></li><li><a href='/contact'>Contact</a></li></ul></nav></div></div>"))

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
  (create-navbar body)
  (let ((main (create-div body :class "main")))
    (funcall content-fn main))
  (create-footer body))

;;; Static Page Handlers

(defun on-index-handler (body)
  "Index page with background slideshow and typewriter greeting. No navbar."
  (clog-web-initialize body)
  (create-child (head-element (html-document body))
                "<link rel='stylesheet' href='/assets/global.css' type='text/css'>")
  (setf (title (html-document body)) "BHW.name")
  ;; Index page styles for slideshow overlay
  (let ((sb (create-style-block body)))
    ;; global.css constrains BODY to a centred 900px column; the index page is
    ;; full-bleed over the fixed-position slideshow, so widen it back out.
    ;; Without the WIDTH override the 900px stays in force and MARGIN 0 pins
    ;; the content flush left.
    (add-style sb :element "body"
               '(("background" "#000")
                 ("color" "#fff")
                 ("overflow-y" "scroll")
                 ("overflow-x" "hidden")
                 ("width" "100%")
                 ("max-width" "none")
                 ("margin" "0")
                 ("padding" "0")))
    ;; MIN-HEIGHT (not HEIGHT) so a card taller than the viewport grows the
    ;; container instead of overflowing past the top edge; the auto margins on
    ;; the inner HEADER then collapse to 0 and the page simply scrolls.
    (add-style sb :element ".slideshow-content"
               '(("position" "relative")
                 ("z-index" "1")
                 ("text-align" "center")
                 ("display" "flex")
                 ("flex-direction" "column")
                 ("justify-content" "center")
                 ("min-height" "100vh")
                 ("box-sizing" "border-box")
                 ("padding" "2rem 1rem")))
    (add-style sb :element ".slideshow-content header"
               '(("max-width" "30rem")
                 ("margin" "auto")
                 ("padding" "2rem 3rem")
                 ("background-color" "rgba(0,0,0,0.5)")
                 ("border-radius" "80px")
                 ("box-shadow" "0 0 30px 20px rgba(0,0,0,0.5)")
                 ("color" "#fff")
                 ("text-shadow" "1px 1px 1px rgba(0,0,0,0.2)")
                 ("line-height" "1.3")))
    (add-style sb :element ".slideshow-content img"
               '(("width" "150px")
                 ("height" "150px")
                 ("border-radius" "9999px")))
    (add-style sb :element ".slideshow-content h1, .slideshow-content h2"
               '(("font-family" "'Cinzel','Times New Roman',serif")
                 ("font-weight" "400")
                 ("color" "#fff")
                 ("margin" "0.2em 0")))
    (add-style sb :element ".slideshow-content p"
               '(("font-family" "'Times New Roman',serif")
                 ("line-height" "1.4")
                 ("margin" "0.5em 0")))
    (add-style sb :element ".slideshow-content a"
               '(("display" "inline-block")
                 ("border" "1px solid #FFF")
                 ("padding" "4px 10px 3px")
                 ("margin" "2px 3px")
                 ("font-family" "'Cinzel','Times New Roman',serif")
                 ("font-size" "13px")
                 ("color" "#fff")
                 ("border-radius" "5px")
                 ("text-decoration" "none")))
    (add-style sb :element ".slideshow-content a:hover"
               '(("background" "rgba(0,0,0,0.5)")))
    (add-style sb :element ".portfolio-container"
               '(("display" "flex")
                 ("flex-direction" "row")))
    (add-style sb :element ".portfolio-section"
               '(("flex" "50%")))
    (add-style sb :element "@media screen and (max-width: 767px)"
               '((".portfolio-container" "flex-direction:column"))))
  (create-slideshow body
                    '("/assets/pic1.webp" "/assets/pic2.webp" "/assets/pic3.webp"
                      "/assets/pic4.webp" "/assets/pic5.webp" "/assets/pic6.webp"))
  (let ((wrapper (create-div body :class "slideshow-content")))
    (let ((inner (create-child wrapper "<header>")))
      (let ((portfolio (create-div inner :class "portfolio-container")))
        (let ((left (create-div portfolio :class "portfolio-section")))
          (create-img left :url-src "/assets/profile.webp"
                           :alt-text "Author Profile Picture"))
        (let ((right (create-div portfolio :class "portfolio-section")))
          (create-section right :h1 :content "Hey There!"
                                    :html-id "greeting")
          (create-section right :h2 :content "I'm Ben.")
          (create-section right :h2 :content "Nice to meet you.")))
      (create-child inner "<hr>")
      (create-p inner
                :content "Welcome to my personal website! This website was built with <a href=\"https://edicl.github.io/hunchentoot/\" target=\"_blank\" rel=\"noreferrer\">Hunchentoot</a> and <a href=\"/assets/blog/project-isidore-doc.html#Project-History-Credits\" target=\"_blank\" rel=\"noreferrer\">Common Lisp.</a> My resume can be found under the work tab. I hope you find what you're looking for, and may the wind be always at your back.")
      (let ((buttons (create-p inner)))
        (create-a buttons :content "About" :link "/about")
        (create-a buttons :content "Work" :link "/work")
        (create-a buttons :content "Blog" :link "/assets/blog/archive.html")
        (create-a buttons :content "Contact" :link "/contact"))))
  ;; Drive typewriter animation
  (let ((h1 (attach-as-child body "greeting")))
    (loop while (validp h1)
          do (loop for greeting in '("Hey there," "Bonjour." "¡Hola!" "こんにちは." "Hello!" "Guten Tag." "Good Day," "Welcome!" "Konnichiwa,")
                   while (validp h1)
                   do (loop for i from 1 to (length greeting)
                            while (validp h1)
                            do (setf (inner-html h1)
                                     (format nil "~A<span aria-hidden='true'></span>"
                                             (subseq greeting 0 i)))
                               (sleep 0.1))
                      (when (validp h1) (sleep 0.7)))
             (when (validp h1) (sleep 2)))))

(defun on-about-handler (body)
  "About page."
  (with-page-chrome body
    (lambda (main)
      (create-section main :h1 :content "About" :class "title")
      (create-section main :h1 :content "The story so far...")
      (create-p main :content "Hey there. I'm currently a student up in the Great White North. Apart from studies and work, I enjoy swimming, piano, and eating peanut butter straight from the jar.")
      (create-section main :h1 :content "Why does this website exist?")
      (create-p main :content "This website's raison d'etre is partly because of the free time granted by the COVID pandemic of 2020-2021. A historic event, and my heart goes out to those suffering still. I am under no illusion the opportunities my station in life affords me. In the same vein of thought, I built this website because the internet -- in its current form -- has given me countless graces (and temptations, but that's to be expected). So please forgive my amateurish writing, for it is my wish that you will take away something of use, and pay it forwards."))))

(defun on-work-handler (body)
  "Work page."
  (with-page-chrome body
    (lambda (main)
      (create-section main :h1 :content "Work" :class "title")
      (create-section main :h1 :content "Side Projects")
      (create-p main :content "Here one will find my Gitlab repos, my book/course notes, and any other miscellaneous items that can be used at one's leisure. As with the above, <a href=\"/contact\">do contact me</a> if something is missing")
      (let ((ul (create-unordered-list main)))
        (create-list-item ul :content "<a target=\"_blank\" href=\"https://github.com/bhw-foss\">Github Repositories</a>")
        (create-list-item ul :content "<a target=\"_blank\" href=\"https://gitlab.com/bhw\">Gitlab Repositories</a>")
        (create-list-item ul :content "<a href=\"/assets/blog/archive.html\">Collected Notes</a>")))))

(defun on-contact-handler (body)
  "Contact page."
  (with-page-chrome body
    (lambda (main)
      (create-section main :h1 :content "Contact" :class "title")
      (create-section main :h1 :content "Ways to get in contact")
      (create-p main :content "Questions, comments, death threats? Don't hesitate to reach out to me via email at: <a class=\"cryptedmail\" data-name=\"seneschal\" data-domain=\"bhw\" data-tld=\"name\" onclick=\"window.location.href = 'mailto:' + this.dataset.name + '@' + this.dataset.domain + '.' + this.dataset.tld; return false;\" href=\"#\"></a>")
      (create-p main :content "My PGP Key Fingerprint: 65F6 51E8 3146 3420 D414 BEFD C784 4C0B 25E7 A49F")
      (create-p main :content "My PGP Public Key is available <a target=\"_blank\" href=\"pubkey.asc\">here</a> and as a secondary source, at <a target=\"_blank\" href=\"https://keys.openpgp.org\">https://keys.openpgp.org.</a>")
      (create-p main :content "To receive blog article updates please use <a target=\"_blank\" href=\"https://bhw.name/assets/blog/atom.xml\">the Blog RSS Feed</a> or <a href=\"https://bhw.name/subscribe\">Subscribe to the Mailing List.</a>")
      (create-section main :h1 :content "Blog Article Transparency Policy")
      (create-p main :content "All edits made to an article after the initial publication date can be found <a target=\"_blank\" href=\"https://gitlab.com/bhw/project-isidore/tree/master/assets/blog\">in the version-controlled Gitlab repository.</a>"))))

(defun on-subscribe-handler (body)
  "Subscribe page."
  (with-page-chrome body
    (lambda (main)
      (create-section main :h1 :content "Subscribe to Mailing List" :class "title")
      (create-p main :content "The Real Simple Syndication (RSS) protocol best represents the World Wide Web as originally visualized: as an information highway. It does so while respecting your privacy, and maintains a high signal-to-noise ratio. \"Walled gardens\" have a vested interest beyond delivering news, but rather are primarily businesses in advertising and data harvesting.")
      (create-a main :content "Further information on getting started with RSS can be found here."
                     :link "https://www.wired.com/story/rss-readers-feedly-inoreader-old-reader/"
                     :target "_blank")
      (create-a main :content "My blog RSS is here."
                     :link "http://bhw.name/assets/blog/atom.xml"
                     :target "_blank")
      (create-p main :content "I highly recommend the use of RSS for newsgroups and news reading. Still, an option exists to receive new blog articles by E-mail. This mailing list exists for that sole purpose and nothing else. Subscriptions are double-opt in.")
      (create-section main :h1 :content "Sign up" :style "text-align:center;")
      (create-child main
                    "<form style=\"text-align:center;\" action=\"https://HanshenWang.us14.list-manage.com/subscribe/post?u=361768abae01d8b2d0358aa3d&amp;id=1433484afa\" method=\"POST\" id=\"mc-embedded-subscribe-form\" name=\"mc-embedded-subscribe-form\" class=\"validate\" target=\"_blank\"><label for=\"mce-EMAIL\">Email Address</label><input type=\"email\" value=\"\" name=\"EMAIL\" class=\"required email\" id=\"mce-EMAIL\" size=\"50\"><input type=\"submit\" value=\"Subscribe\" name=\"subscribe\" id=\"mc-embedded-subscribe\" class=\"button\"></form>"))))

;;; Error Page Handlers

(defun on-404-handler (body)
  "Default handler for unmatched routes."
  (let ((path (path-name (location body))))
    (with-page-chrome body
      (lambda (main)
        (create-section main :h1 :content "404. That's an error." :class "title")
        (create-p main :content
                  (format nil "The requested URL <code>~A</code> was not found on this server. But that's not all we know..." path))
        (create-p main :content
                  "Please validate the URL. <a href=\"https://www.w3.org/Provider/Style/URI\">Although I try my best to keep permanent, eternal, everlasting URLs,</a> Mea culpa. If you have a vague certainty that something should be here but isn't; <a href=\"https://gitlab.com/bhw/project-isidore/\">please browse the source repository</a> or <a href=\"/contact\">contact me for more information.</a>")))))

(defun render-400-page (obj)
  "Render 400 Bad Request content into OBJ."
  (create-section obj :h1 :content "1883 Haydock Douay Rheims Bible")
  (create-section obj :h4 :content "Error Code 400 - Bad Request")
  (create-p obj :content "Please validate the URL. Resource exists but invalid arguments passed.")
  (create-p obj :content
            "An example of a valid URL is <code>/bible/matthew-4-14/john-3-15</code> where the ending input is optional. Instead of <code>matthew</code> the number 47 may be used instead. This does not hold true for the chapter or verse which must be numeric. <a href=\"https://gitlab.com/bhw/project-isidore/blob/master/src/webserver.lisp\">Consult the source repository on what constitutes a valid URL (N.B. the function PARSE-UID-SYM)</a> or <a href=\"/contact\">contact me for more information.</a>"))

(defun render-negative-range-page (obj invalid-range)
  "Render 416 Range Not Specifiable content into OBJ."
  (let* ((bi (car invalid-range))
         (ei (cadr invalid-range))
         (corrected-link
           (if (symbolp (car invalid-range))
               (concatenate 'string "/bible/"
                            (string-downcase (string (cadr invalid-range))) "/"
                            (string-downcase (string (car invalid-range))) "/")
               (concatenate 'string "/bible/"
                            (write-to-string (cadr invalid-range)) "/"
                            (write-to-string (car invalid-range)) "/"))))
    (create-section obj :h1 :content "1883 Haydock Douay Rheims Bible")
    (create-section obj :h4 :content "Error Code 416 - Range Not Specifiable")
    (create-p obj :content
              (format nil "Please validate the URL. The beginning input <code>~A</code> subtracted from the ending input <code>~A</code> results in a negative interval range. Perhaps swapping the values will provide a valid URL: <a href=\"~A\">~A</a>"
                      bi ei corrected-link corrected-link))
    (create-p obj :content
              "An example of a valid URL is <code>/bible/matthew-4-14/john-3-15</code> where the ending input is optional. Instead of <code>matthew</code> the number 47 may be used instead. This does not hold true for the chapter or verse which must be numeric. <a href=\"https://gitlab.com/bhw/project-isidore/blob/master/src/webserver.lisp\">Consult the source repository on what constitutes a valid URL (N.B. the function PARSE-UID-SYM)</a> or <a href=\"/contact\">contact me for more information.</a>")))

;;; Slideshow Component

(defclass clog-slideshow (clog-div) ()
  (:documentation "Full-screen background image slideshow. Images cross-fade
using jQuery's fadeIn/fadeOut."))

(defgeneric create-slideshow (clog-obj images &key interval fade-duration html-id)
  (:documentation "Create a CLOG-Slideshow as a child of CLOG-OBJ.
IMAGES is a list of URL strings. INTERVAL is the delay in ms between slide
transitions (default 6000). FADE-DURATION is the ms for each cross-fade
(default 1000). The slideshow is prepended (:auto-place :top) to CLOG-OBJ so it sits
behind any pre-rendered SSR content in the stacking order."))

(defmethod create-slideshow ((obj clog-obj) images
                             &key (interval 6000)
                               (fade-duration 1000)
                               (html-id nil))
  (let ((container (create-div obj :class "slideshow"
                                   :html-id html-id
                                   :auto-place :top)))
    (change-class container 'clog-slideshow)
    (set-styles container '(("position" "fixed")
                            ("width"    "100%")
                            ("height"   "100%")
                            ("top"      "0px")
                            ("left"     "0px")
                            ("z-index"  "0")
                            ("margin"   "0")
                            ("padding"  "0")))
    (iterate ((url (scan images))
              (i (scan-range :from 0)))
             (let ((slide (create-div container)))
               (set-styles slide
                           `(("position"            "absolute")
                             ("width"               "100%")
                             ("height"              "100%")
                             ("background-image"    ,(format nil "url(~A)" url))
                             ("background-size"     "cover")
                             ("background-position" "50% 50%")
                             ("display"             ,(if (zerop i) "block" "none"))))))
    (js-execute container
                (format nil "(function() { var $s = $('#~A > div'); var c = 0; setInterval(function() { var n = (c + 1) % $s.length; $($s[c]).fadeOut(~A); $($s[n]).fadeIn(~A); c = n; }, ~A); })();"
                        (html-id container)
                        fade-duration fade-duration interval))
    container))

;;; Bible Route Handling

(defun parse-verse-arg (str)
  "Parse a verse argument as either a numeric UID or a symbolic reference.
Pure integers are returned directly; otherwise `parse-uid-sym' is used."
  (let ((n (parse-integer str :junk-allowed t)))
    (if (and n (string= (write-to-string n) str))
        n
        (parse-uid-sym (intern (string-upcase str))))))

(defun parse-uid-sym (uid-sym)
  "Human readable call to `view:bible-page'. UID-SYM must be of the following format,
BOOK-delimiter-CHAPTER-delimiter-VERSE, where delimiters are the SAME single
NON-alphanumeric unreserved character. CHAPTER and VERSE must be numeric,
whereas BOOK must be alphanumeric. Unreserved characters are defined as
uppercase and lowercase letters, decimal digits, hyphen, period, underscore,
and tilde. See RFC3986 2.3. Unreserved Characters
https://www.ietf.org/rfc/rfc3986.txt

Ex. /bible/genesis-2-10,/bible/47~4~2 etc.

> (parse-uid-sym 'genesis-2-10)
51
> (parse-uid-sym '1.2.10)
51
> (parse-uid-sym '1.2-10)
ARG-COUNT-ERROR
"
  (let ((list (extremum
               (collect (map-fn 'list
                                (lambda (unreserved-char)
                                  (ppcre:split unreserved-char (string uid-sym)))
                                (scan '("-" "\\." "_" "~"))))
               #'> :key #'length)))
    (if (= 3 (length list)) ; Assume a sublist of 3 is properly parsed.
        (destructuring-bind (book chapter verse)
            list
          (bible:get-bible-uid
           (if (parse-integer book :junk-allowed t)
               ;; It has a number, but is it a prefix or by its lonesome?
               (if (ppcre:scan "[0-9]{1}[a-zA-Z]" book)
                   (bible:bible-book-convert-dwim
                    (concatenate 'string
                                 (subseq book 0 1) ; IV Kings will not work.
                                 " "
                                 (cadr (ppcre:split "[0-9]" book))))
                   (parse-integer book))
               (bible:bible-book-convert-dwim book))
           (parse-integer chapter)
           (parse-integer verse)))
        (error "Invalid verse reference: ~A" uid-sym))))

(defun resolve-bible-path (path)
  "Parse /bible/X and /bible/X/Y from PATH and return a uid-list, or a keyword
indicating an error condition:
  :bad-request - invalid arguments
  (:negative-range (begin-str end-str)) - reversed range"
  (handler-case
      (let* ((remainder (subseq path (length "/bible/")))
             (parts (ppcre:split "/" remainder))
             (beginning-str (first parts))
             (ending-str (or (second parts) beginning-str)))
        (let ((beginning-uid (parse-verse-arg beginning-str))
              (ending-uid (parse-verse-arg ending-str)))
          (cond
            ((and (<= 0 beginning-uid)
                  (>= 7 beginning-uid))
             :bad-request)
            ((> beginning-uid ending-uid)
             (list :negative-range (list beginning-str ending-str)))
            ((or (not (bible:valid-bible-uid-p beginning-uid))
                 (not (bible:valid-bible-uid-p ending-uid)))
             :bad-request)
            (t (iota (1+ (- ending-uid beginning-uid))
                     :start beginning-uid)))))
    (error () :bad-request)))

;;; Bible Content Rendering

(defun make-toggle (body button content)
  "Wire a click toggle: clicking BUTTON toggles visibility of CONTENT.
BUTTON and CONTENT are CLOG objects."
  (declare (ignore body))
  (let ((visible nil))
    (set-on-click button
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf visible (not visible))
                    (setf (style content "display")
                          (if visible "block" "none"))))))

(defun wire-font-selector (body select target)
  "Wire a <select> CLOG element to change font-family of TARGET."
  (declare (ignore body))
  (set-on-change select
                 (lambda (obj)
                   (setf (style target "font-family")
                         (value obj)))))

(defun create-font-dropdown (parent)
  "Create a font dropdown selector. Returns the select element."
  (let ((container (create-div parent :class "font-dropdown-menu")))
    (let ((sel (create-select container :html-id "input-font"
                                        :class "input")))
      (create-option sel :content "Times New Roman"
                         :value "Times New Roman"
                         :selected t)
      (dolist (font-name '("Arial" "Courier New" "Garamond" "Verdana"))
        (create-option sel :content font-name :value font-name))
      sel)))

(defun render-bible-content (obj uid-list)
  "Render Bible verse table into OBJ for the given UID-LIST."
  (let ((table (create-table obj :html-id "main-content")))
    (dolist (bible-uid uid-list)
      (let* ((data (bible:get-bible-verse-data bible-uid))
             (row (create-table-row table :style "line-height: 1.5em;"))
             (heading-cell (create-table-column row))
             (text-cell (create-table-column row))
             (footnotes-cell (create-table-column row :style "width:55%"))
             (xref-text (getf data :cross-references))
             (fn-text (getf data :footnotes))
             (int-ch (getf data :internal-chapter)))
        (setf (inner-html heading-cell) (getf data :heading))
        (let ((xref (when xref-text
                      (bible:encode-cross-references-to-html-links
                       xref-text bible-uid int-ch))))
          (setf (inner-html text-cell)
                (format nil "~A~@[<br><br>~A~]"
                        (getf data :text)
                        xref)))
        (when fn-text
          (setf (inner-html footnotes-cell)
                (bible:encode-cross-references-to-html-links
                 fn-text bible-uid int-ch)))))))

(defun on-bible-handler (body)
  "Bible page handler. Parses path, builds book/chapter toggles, search form,
font selector, and verse table via CLOG."
  (let ((path (path-name (location body))))
    (with-page-chrome body
      (lambda (main)
        (let ((result (if (string= path "/bible")
                          (iota 31 :start 8)
                          (resolve-bible-path path))))
          (cond
            ((eq result :bad-request)
             (render-400-page main))
            ((and (listp result) (eq (car result) :negative-range))
             (render-negative-range-page main (cadr result)))
            (t
             (let ((uid-list result))
               (create-section main :h1 :content "1883 Haydock Douay Rheims Bible")
               (create-section main :h4
                               :content "Presents commentary in a tabular format for ease of reading. <a href=\"/assets/blog/tabular-douay-rheims.html\">Click to learn more.</a>")
               ;; Book selector
               (let ((book-btn (create-button main :content "Select Book"))
                     (book-links (create-div main :style "overflow:auto;display:none;")))
                 (make-toggle body book-btn book-links)
                 (dolist (pair bible:+bible-book-url-alist+)
                   (destructuring-bind (link . link-text) pair
                     (create-a (create-div book-links :style "width:200px;float:left")
                               :content link-text :link link))))
               (create-br main)
               ;; Chapter selector
               (let ((chapter-btn (create-button main :content "Select Chapter"))
                     (chapter-links (create-div main :style "overflow:auto;display:none;")))
                 (make-toggle body chapter-btn chapter-links)
                 (dolist (pair (bible:make-bible-chapter-url-list uid-list))
                   (destructuring-bind (link . link-text) pair
                     (create-a (create-div chapter-links :style "width:200px;float:left")
                               :content link-text :link link))))
               (create-br main)
               ;; Search form
               (let ((form-div (create-div main :style "text-align:center;")))
                 (let ((form (create-form form-div)))
                   (create-label form :content "Search: ")
                   (let ((query-input (create-form-element form :text :name "query" :size 50)))
                     (setf (requiredp query-input) t)
                     (create-form-element form :submit :value "Submit")
                     (set-on-submit form
                                    (lambda (f)
                                      (declare (ignore f))
                                      (let ((q (value query-input)))
                                        (setf (url (location body))
                                              (format nil "/bible-search?query=~A" (quri:url-encode q)))))))))
               ;; Font selector + verse table
               (let ((font-sel (create-font-dropdown main))
                     (table-area (create-div main)))
                 (wire-font-selector body font-sel table-area)
                 (render-bible-content table-area uid-list))))))))))



;;; Bible Search

(defun extract-query-param (url param)
  "Extract the value of PARAM from URL query string. Returns NIL if not found."
  (let ((query-start (position #\? url)))
    (when query-start
      (let* ((query-string (subseq url (1+ query-start)))
             (decoded (quri:url-decode query-string))
             (parts (ppcre:split (format nil "~A=" param) decoded :limit 2)))
        (when (= 2 (length parts))
          ;; Strip any trailing &param=value pairs
          (let ((val (second parts)))
            (let ((amp (position #\& val)))
              (if amp (subseq val 0 amp) val))))))))

(defun render-search-results (obj query)
  "Render Bible search results for QUERY into OBJ. Clears OBJ first."
  (setf (inner-html obj) "")
  (let ((table (create-table obj :html-id "main-content")))
    (dolist (result (bible:search-bible query '(:num-docs 100)))
      (destructuring-bind (bible-uid . score) result
        (let* ((data (bible:get-bible-verse-data bible-uid))
               (row (create-table-row table :style "line-height: 1.5em;"))
               (score-cell (create-table-column row))
               (heading-cell (create-table-column row))
               (text-cell (create-table-column row))
               (footnotes-cell (create-table-column row :style "width:50%"))
               (xref-text (getf data :cross-references))
               (fn-text (getf data :footnotes))
               (int-ch (getf data :internal-chapter)))
          (setf (inner-html score-cell)
                (write-to-string (floor score 0.01)))
          (setf (inner-html heading-cell)
                (getf data :heading))
          (let ((xref (when xref-text
                        (bible:encode-cross-references-to-html-links
                         xref-text bible-uid int-ch))))
            (setf (inner-html text-cell)
                  (format nil "~A~@[<br><br>~A~]"
                          (getf data :text)
                          xref)))
          (when fn-text
            (setf (inner-html footnotes-cell)
                  (bible:encode-cross-references-to-html-links
                   fn-text bible-uid int-ch))))))))

(defun on-bible-search-handler (body)
  "Bible search page handler. Builds syntax help, search form with CLOG-native
submission and url-rewrite for shareable URLs."
  (let ((url (url (location body))))
    (with-page-chrome body
      (lambda (main)
        (create-section main :h1 :content "1883 Haydock Douay Rheims Bible")
        ;; Query syntax toggle
        (let ((syntax-heading (create-section main :h2
                                              :content "Query Syntax (click to toggle)"))
              (syntax-div (create-div main :style "display:none;")))
          (setf (style syntax-heading "text-align") "center")
          (setf (cursor syntax-heading) :pointer)
          (make-toggle body syntax-heading syntax-div)
          (setf (inner-html syntax-div)
                "<h2>1. FIELDS</h2><p>Each sentence of the Bible has the following metadata stored in fields,</p><li>(b) The book.</li><li>(c) The chapter number.</li><li>(v) The verse number.</li><li>(t) The text itself.</li><li>(f) If applicable, the footnote (Haydock) commentary.</li><li>(x) If applicable, the listed cross-references.</li><li>(implicit) All of the above fields.</li><h2>2. OPERATOR</h2><p>The fields are combined with Boolean OPERATOR(s),</p><li>(+) AND,</li><li>(implicit) OR</li><li>(!) NOT</li><p>Lastly, a particular predefined set of operations,</p><li>(*) TRUNCATE</li><p>The asterisk serves as the truncation (or wildcard) operator. Unlike the other operators, it should be appended to the word to be affected. Words match if they begin with the word preceding the * operator.</p><h2>3. FORM</h2><p>A query takes the form of,</p><p>[OPERATOR FIELD : EXPRESSION]</p><p>where EXPRESSION may be affixed with an asterisk character. Mixing of explicit and implicit forms as well as multiple forms are allowed. E.g.</p><li>charity</li><li>charity cymbal</li><li>loved much</li><li>mercy +b:matthew</li><li>broad path destruction narrow</li><li>principalities !c:2</li><li>compare 'exact*' with 'exacteth'</li><li>no greater love lay down</li><li>romans 3 23</li><li>+b:john +c:3 +v:15</li>"))
        ;; Return to tabular view link
        (create-p main :content "<a href=\"/bible/1-1-1/1-1-31\" style=\"text-align:center;display:block;\">Return to tabular view.</a>")
        ;; Font selector
        (let ((font-sel (create-font-dropdown main)))
          ;; Search form
          (let ((form-div (create-div main :style "text-align:center;")))
            (let ((form (create-form form-div)))
              (create-label form :content "Search: ")
              (let ((query-input (create-form-element form :text
                                                      :name "query" :size 50)))
                (setf (requiredp query-input) t)
                (create-form-element form :submit :value "Submit")
                ;; Results area (created after form, natural DOM order)
                (let ((results-area (create-div main)))
                  ;; Wire font selector to results
                  (wire-font-selector body font-sel results-area)
                  (set-on-submit form
                                 (lambda (f)
                                   (declare (ignore f))
                                   (let ((q (value query-input)))
                                     (url-rewrite (window body)
                                                  (format nil "/bible-search?query=~A"
                                                          (quri:url-encode q)))
                                     (render-search-results results-area q))))
                  ;; Auto-search if URL had query param
                  (let ((existing-query (extract-query-param url "query")))
                    (when existing-query
                      (setf (value query-input) existing-query)
                      (render-search-results results-area existing-query))))))))))))

;;; Server Lifecycle

(defun wait-for-port-ready (port &key (host "127.0.0.1") (timeout 5.0) (interval 0.05))
  "Block until the web server on HOST:PORT has fully started, or TIMEOUT
seconds elapse.  Returns T on success, NIL on timeout.

CLOG's INITIALIZE returns as soon as clack:clackup hands back a handler
struct, but the underlying hunchentoot acceptor finishes START in a separate
thread, in two steps: START-LISTENING binds the socket, so the port accepts
connections, and only then does EXECUTE-ACCEPTOR store the listener thread in
the taskmaster's ACCEPTOR-PROCESS slot.  If TERMINATE-APPLICATION runs between
those steps, clack's BT2:DESTROY-THREAD cleanup calls HUNCHENTOOT:STOP, whose
SHUTDOWN reads the still-unbound slot and signals UNBOUND-SLOT in that thread;
under \"sbcl --script\" (the production build, which runs the test suite) an
unhandled error in any thread terminates the whole process.  The 2026-09-04
deploy died exactly there.  So this waits for both signs: the port accepting
connections and a \"hunchentoot-listener-HOST:PORT\" thread existing, plus one
more INTERVAL for the slot assignment that immediately follows the thread's
creation."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second)))
        (listener-name (format nil "hunchentoot-listener-~A:~A" host port)))
    (flet ((port-ready-p ()
             (handler-case
                 (let ((sock (usocket:socket-connect host port)))
                   (usocket:socket-close sock)
                   t)
               (error () nil)))
           (listener-thread-p ()
             (find listener-name (bordeaux-threads:all-threads)
                   :key #'bordeaux-threads:thread-name :test #'equal))
           (expired-p ()
             (> (get-internal-real-time) deadline)))
      (loop until (and (port-ready-p) (listener-thread-p))
            do (when (expired-p)
                 (return-from wait-for-port-ready nil))
               (sleep interval))
      (sleep interval)
      t)))

(defun terminate-application (&optional sigint-poll)
  "Stop the web server started by `initialize-application', if it exists. When
called with a non NIL value for SIGINT-POLL, it will listen for SIGINT (Catch a
user's Control-c in the terminal) and gracefully shut down the web server and
exit the lisp process."
  ;; Symmetric to initialize-application starting the scheduler. Stopping it
  ;; here lets save-lisp-and-die proceed after asdf:test-system runs the
  ;; can-app-init-and-exit test, which exercises this lifecycle.
  (slack-bot:stop-reminder-scheduler)
  (when sigint-poll
    (handler-case (loop (sleep 1))
      (#+sbcl sb-sys:interactive-interrupt
       #+ccl  ccl:interrupt-signal-condition
       #+clisp system::simple-interrupt-condition
       #+ecl ext:interactive-interrupt
       #+allegro excl:interrupt-signal
       () (progn
            (format *error-output* "~%Aborting.~&~%")
            (when (find-package :slynk)
              (funcall (intern "STOP-SERVER" :slynk) 4005))
            (shutdown)
            (format t "~%Server successfully stopped.~%")
            (uiop:quit)))
      (error (c) (format t "Whoops, an unknown error occured:~&~a~&" c))))
  (when (or *server* (is-running-p))
    (ignore-errors
     (progn
       (shutdown)
       (setf *server* nil)
       (format t "~%Server successfully stopped.~%")
       (return-from terminate-application t)))))

(defun initialize-application (&key (port 8080))
  "Start a CLOG web server at PORT. All pages are constructed via CLOG-web
WebSocket handlers using the Tutorial 30 pattern.

Slynk server is used to connect to a running production LISP image.

See APPLICATION-TOPLEVEL for the main function or entry point."
  (terminate-application)
  (bible:reopen-fd-streams)
  (slack-bot:load-config)
  (slack-bot:start-reminder-scheduler)
  (initialize 'on-index-handler
              :host "127.0.0.1"
              :port port
              :extended-routing t
              :lack-middleware-list (list (slack-bot:make-slack-middleware))
              :static-root (namestring
                            (asdf:system-relative-pathname :project-isidore "./")))
  ;; Register page routes
  (set-on-new-window 'on-about-handler    :path "/about")
  (set-on-new-window 'on-work-handler     :path "/work")
  (set-on-new-window 'on-contact-handler  :path "/contact")
  (set-on-new-window 'on-subscribe-handler :path "/subscribe")
  ;; Bible needs extended routing for /bible/genesis-1-1 style paths
  (set-on-new-window 'on-bible-handler    :path "/bible")
  (set-on-new-window 'on-bible-search-handler :path "/bible-search")
  ;; Default handler for 404
  (set-on-new-window 'on-404-handler :path :default)
  ;; CLOG's own JS files (boot.js, jquery)
  (conn:add-plugin-path "^/js/"
                        (merge-pathnames "static-files/"
                                         (asdf:system-source-directory :clog)))
  ;; Project static assets
  (conn:add-plugin-path "^/assets/"
                        (namestring (asdf:system-relative-pathname :project-isidore "./")))
  ;; Favicon
  (conn:add-plugin-path "^/favicon\\.ico"
                        (namestring (asdf:system-relative-pathname :project-isidore "assets/")))
  (setf *server* t)
  (format t "~%
========================================
Project Isidore v2.0.0 (CLOG-web)
========================================

Copyright (c) 2021  Ben H. W. <foss@bhw.name>

Project Isidore is free software, provided as is, and comes with ABSOLUTELY NO
WARRANTY. This program is licensed under the GNU AFFERO GENERAL PUBLIC LICENSE
Version 3, 19 November 2007 You are welcome to redistribute the program or any
parts hereof under certain conditions. Please visit
https://www.gnu.org/licenses/agpl-3.0.html for License details.

Homepage: https://www.bhw.name/assets/blog/project-isidore-doc.html

Source code repository: https://gitlab.com/bhw/project-isidore

Navigate to http://localhost:~A to continue... ~%" port)
  ;; clack:clackup with :use-thread t returns before hunchentoot:start
  ;; finishes binding the listen socket and acceptor-process. Tests (and
  ;; any caller that immediately calls terminate-application) need that
  ;; state set or clack's BT2:DESTROY-THREAD cleanup hits UNBOUND-SLOT.
  (wait-for-port-ready port)
  (return-from initialize-application t))

(defun application-toplevel ()
  "Application entry point. Emulate a \"main\" function. Used in
  SAVE-LISP-AND-DIE to save Application as a Lisp image. Note PORT is a keyword
  argument that defaults to 8080. Heroku dynamically sets the PORT variable to
  be binded."
  (let* ((port (if (equalp NIL (uiop:getenv "PORT"))
                   8080
                   (parse-integer (uiop:getenv "PORT")))))
    ;; We only want one connection to a remote lisp.
    (when (= 8080 port)
      (progn
        (when (find-package :slynk)
          (funcall (intern "CREATE-SERVER" :slynk) :port 4005 :dont-close t)
          (setf (symbol-value (intern "*USE-DEDICATED-OUTPUT-STREAM*" :slynk)) nil))))
    (initialize-application :port port)
    (format t "~% Close this window or press Control+C to exit the program...~%")
    (terminate-application t)))
