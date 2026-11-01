;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/site/pages
  (:use #:common-lisp)
  (:import-from #:clog
                #:clog-div #:clog-obj
                #:add-style #:attach-as-child #:create-a #:create-child #:create-div
                #:create-img #:create-list-item #:create-p #:create-section
                #:create-style-block #:create-unordered-list #:head-element
                #:html-document #:html-id #:inner-html #:js-execute #:location
                #:path-name #:set-on-new-window #:set-styles #:title #:validp)
  (:import-from #:clog-web #:clog-web-initialize)
  (:import-from #:project-isidore/site/layout #:escape-html #:with-page-chrome)
  (:export #:on-index-handler
           #:register-routes)
  (:documentation
   "The personal site: the index page with its slideshow, About, Work,
Contact, the mailing-list page and the 404 page."))

(in-package #:project-isidore/site/pages)

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
    (loop for url in images
          for i from 0
          do (set-styles (create-div container)
                         `(("position"            "absolute")
                           ("width"               "100%")
                           ("height"              "100%")
                           ("background-image"    ,(format nil "url(~A)" url))
                           ("background-size"     "cover")
                           ("background-position" "50% 50%")
                           ("display"             ,(if (zerop i) "block" "none")))))
    (js-execute container
                (format nil "(function() { var $s = $('#~A > div'); var c = 0; setInterval(function() { var n = (c + 1) % $s.length; $($s[c]).fadeOut(~A); $($s[n]).fadeIn(~A); c = n; }, ~A); })();"
                        (html-id container)
                        fade-duration fade-duration interval))
    container))


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
      (create-p main :content "All edits made to an article after the initial publication date can be found <a target=\"_blank\" href=\"https://github.com/bhwdev/project-isidore/tree/master/assets/blog\">in the version-controlled GitHub repository.</a>"))))

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

;;; Error Page

(defun on-404-handler (body)
  "Default handler for unmatched routes."
  (let ((path (path-name (location body))))
    (with-page-chrome body
      (lambda (main)
        (create-section main :h1 :content "404. That's an error." :class "title")
        (create-p main :content
                  ;; PATH is whatever the browser reports, so it is escaped.
                  (format nil "The requested URL <code>~A</code> was not found on this server. But that's not all we know..."
                          (escape-html path)))
        (create-p main :content
                  "Please validate the URL. <a href=\"https://www.w3.org/Provider/Style/URI\">Although I try my best to keep permanent, eternal, everlasting URLs,</a> Mea culpa. If you have a vague certainty that something should be here but isn't; <a href=\"https://github.com/bhwdev/project-isidore\">please browse the source repository</a> or <a href=\"/contact\">contact me for more information.</a>")))))


(defun register-routes ()
  "Serve the site's pages.  The index page is the handler CLOG is started with
\(see PROJECT-ISIDORE/MAIN), and any path nothing else serves gets the 404."
  (set-on-new-window 'on-about-handler :path "/about")
  (set-on-new-window 'on-work-handler :path "/work")
  (set-on-new-window 'on-contact-handler :path "/contact")
  (set-on-new-window 'on-subscribe-handler :path "/subscribe")
  (set-on-new-window 'on-404-handler :path :default))
