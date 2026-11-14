;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/plp/console
  (:use #:common-lisp)
  (:import-from #:bordeaux-threads)
  (:import-from #:clog
                #:add-class #:attribute #:create-a #:create-button
                #:create-data-list #:create-div #:create-form-element
                #:create-option #:create-section #:create-select #:create-span
                #:create-text-area #:css-class-name #:focus #:html-id
                #:inner-html #:location #:path-name #:place-holder
                #:remove-class #:scroll-into-view #:set-on-change #:set-on-click
                #:set-on-input #:set-on-key-down #:set-on-new-window #:style
                #:tab-index #:text-value #:url #:validp #:value)
  (:import-from #:bhw/site/layout #:escape-html)
  (:import-from #:bhw/plp/screen
                #:handle-key #:on-screen #:path-code #:setup-page #:slide-html
                #:timezone-offset)
  (:local-nicknames (#:song #:bhw/plp/song)
                    (#:db #:bhw/plp/database)
                    (#:live #:bhw/plp/live))
  (:export #:*terms*
           #:*card-columns*
           #:row-step
           #:card-html
           #:chord-line-html
           #:chart-card-html
           #:chart-font-size
           #:on-front
           #:on-console
           #:register-routes)
  (:documentation
   "The operator's pages.

/plp states the terms the lyrics are shown under and, on agreement, opens a
console on a new room.  /plp-console/CODE is the console of the room CODE:
on the left the setlist and the search, in the middle every slide of the
song as a card in the order it is sung, on the right what the screen shows
and what comes next, and the text of a song to add or correct.

The keys (BHW/PLP/LIVE:PRESS-KEY) act while the middle pane has the focus,
so that typing in a field types; / goes to the search, Escape comes back,
and every button gives the focus back to the pane.  The cards stand in rows
of *CARD-COLUMNS*, a number the server sets, so that it knows which card is
above and below the one showing: the up and down arrows go there.  In the
search they move the mark among the songs found, and Enter takes the marked
one.

New opens a form over the console for a song's credits, its tags and its
words as pasted from anywhere, chords over them or not; under them stands
what BHW/PLP/SONG:OUTLINE makes of their structure, each part with the kind
it is taken for, to be corrected.  Edit opens the same form on the song
showing, its words in the text format.  A tag is added by name: the field
offers the tags there are as it is typed in, and takes a new one as
category=name.  Save stores the song; To text writes it into the text field
instead.

Musician turns the console into a musician's page: the middle pane alone,
and in it the song's slides once each (BHW/PLP/SONG:DECK-CHART) with their
chords over the words, in columns and in type as large as shows the whole
song (CHART-FONT-SIZE).  A tap on a slide shows it, the two buttons in the
corner go back and on, and the key the song is played in moves by semitones
for every page of the room; a setlist is saved with the keys of its songs.

What the console has to say, it says in a box over the page, which is also
where it asks before it deletes.

Anyone can present.  Saving a setlist or a song's text, and deleting one,
asks for the passphrase (BHW/PLP/DATABASE:AUTHORIZED-P)."))

(in-package #:bhw/plp/console)

(defparameter *terms*
  "<p>The words of these songs belong to their authors and publishers, who
are named with each song. They are shown here by permission, for the prayer
and worship of the Companions of the Cross and of those who pray with
them.</p>
<p>By going on you agree to use them for that only: to show them where
people are gathered to pray, under whatever licence your community needs for
it, and not to copy, publish or pass them on.</p>"
  "What /plp says before it opens a console, as HTML.")

(defparameter *key-help*
  "<kbd>Space</kbd> <kbd>→</kbd> next · <kbd>←</kbd> back · <kbd>↑</kbd> <kbd>↓</kbd> the card
above / below · <kbd>1</kbd>–<kbd>9</kbd> verse ·
<kbd>R</kbd> refrain · <kbd>B</kbd> bridge · <kbd>P</kbd> pre-chorus · <kbd>T</kbd> tag ·
<kbd>O</kbd> outro · <kbd>.</kbd> black · <kbd>W</kbd> waiting · <kbd>N</kbd> / <kbd>Shift+N</kbd>
next / previous song · <kbd>/</kbd> search (<kbd>↑</kbd> <kbd>↓</kbd> choose, <kbd>Enter</kbd> adds,
<kbd>Shift+Enter</kbd> shows, <kbd>Esc</kbd> leaves)"
  "The key map, under the console.")

(defparameter *new-song-text*
  "
1.
The first verse, a line of the screen to a line

Refrain:
The refrain
"
  "What the song text has under its headers when a new song is given no
words.")

(defparameter *card-columns* 3
  "How many cards stand in a row of the middle pane.")

(defun row-step (index count columns direction)
  "The index of the card above (DIRECTION -1) or below (1) the INDEXth of
COUNT cards in rows of COLUMNS: under a card that has none below it but a
row is left, the last card; INDEX itself where there is no row to go to."
  (let ((target (+ index (* direction columns))))
    (cond ((< target 0) index)
          ((< target count) target)
          ((< (floor index columns) (floor (1- count) columns)) (1- count))
          (t index))))

(defun on-front (body)
  "/plp: the terms, and a button that opens a console on a new room."
  (setup-page body "Praise lyrics")
  (let* ((front (create-div body :class "plp-front"))
         (button (progn
                   (create-section front :h1 :content "Praise lyrics")
                   (create-div front :content *terms*)
                   (create-button front :content "I agree: open the console"))))
    (set-on-click button
                  (lambda (object)
                    (declare (ignore object))
                    (setf (url (location body))
                          (format nil "/plp-console/~A" (live:new-code)))))))

(defun card-html (slide)
  "The inside of the card of SLIDE: its key and name, and its lines."
  (format nil "<div class='plp-card-label'><kbd>~A</kbd>~A~A</div>~A"
          (escape-html (song:slide-group slide))
          (escape-html (song:slide-label slide))
          (if (> (song:slide-of slide) 1)
              (format nil " (~D/~D)" (song:slide-part slide) (song:slide-of slide))
              "")
          (slide-html slide)))

(defun chord-line-html (line chords &key (semitones 0) flats)
  "LINE as HTML with CHORDS, a list of (offset . chord), over it, each
SEMITONES higher (BHW/PLP/SONG:TRANSPOSE-CHORD): the line is cut where a
chord stands, and each piece carries its chord above it."
  (if (null chords)
      (format nil "<div class='plp-line'>~A</div>" (escape-html line))
      (let ((places '()))
        ;; Chords at one place stand side by side.
        (dolist (chord (stable-sort (copy-list chords) #'< :key #'car))
          (let ((at (max 0 (min (car chord) (length line))))
                (name (song:transpose-chord (cdr chord) semitones :flats flats)))
            (if (and places (= at (car (first places))))
                (setf (cdr (first places)) (format nil "~A ~A" (cdr (first places)) name))
                (push (cons at name) places))))
        (setf places (nreverse places))
        (with-output-to-string (out)
          (write-string "<div class='plp-line plp-chorded'>" out)
          (when (plusp (car (first places)))
            (format out "<span class='plp-seg'>~A</span>"
                    (escape-html (subseq line 0 (car (first places))))))
          (loop for ((at . name) next) on places
                for text = (subseq line at (if next (car next) (length line)))
                do (format out "<span class='plp-seg'><span class='plp-chord'>~A</span>~A</span>"
                           (escape-html name)
                           (if (string= text "") "&nbsp;" (escape-html text))))
          (write-string "</div>" out)))))

(defun chart-card-html (slide &key (semitones 0) flats)
  "The inside of the card of SLIDE on a musician's page: its key and name,
and its lines with their chords over them."
  (format nil "<div class='plp-card-label'><kbd>~A</kbd>~A~A</div>~{~A~}"
          (escape-html (song:slide-group slide))
          (escape-html (song:slide-label slide))
          (if (> (song:slide-of slide) 1)
              (format nil " (~D/~D)" (song:slide-part slide) (song:slide-of slide))
              "")
          (loop with chords = (song:slide-chords slide)
                for line in (song:slide-lines slide)
                collect (chord-line-html line (pop chords)
                                         :semitones semitones :flats flats))))

(defun chart-font-size (slides &key (max-columns 5))
  "The CSS font-size at which the cards of SLIDES (CHART-CARD-HTML) fill a
musician's page without being scrolled, and as a second value the width of a
column of them in em.

The cards stand in columns the browser makes as many of as that width fits,
and no card is cut.  For each number of columns up to MAX-COLUMNS the size
is the smaller of what the page's width allows that many columns and what
its height allows the tallest of them, the cards shared out in order as
evenly as they go; the size is the largest of those, so that a wide page
sets a long song in more columns than a tall one.  A letter is taken to be
half its height wide, a line of words a height and a quarter tall, a line
of chords a height, and the page to leave 96 hundredths of its width and 88
of its height to the cards.  The page running no script, none of
that is measured: a song set too large by it is scrolled."
  (let* ((width (+ 2
                   (* 0.5
                      (reduce #'max
                              (loop for slide in slides
                                    append (loop with chords = (song:slide-chords slide)
                                                 for line in (song:slide-lines slide)
                                                 collect (max (length line)
                                                              (loop for (nil . chord) in (pop chords)
                                                                    sum (1+ (length chord))))))
                              :initial-value 12))))
         (heights (loop for slide in slides
                        collect (+ 2.4
                                   (loop with chords = (song:slide-chords slide)
                                         for nil in (song:slide-lines slide)
                                         sum (if (pop chords) 2.25 1.25)))))
         (count (length heights)))
    (labels ((tallest (heights columns)
               ;; The height of the tallest of COLUMNS columns of HEIGHTS.
               (if (or (= columns 1) (null (rest heights)))
                   (reduce #'+ heights)
                   (loop for rest on heights
                         for sum = (first rest) then (+ sum (first rest))
                         minimize (max sum (if (rest rest)
                                               (tallest (rest rest) (1- columns))
                                               0))))))
      (values
       (format nil "clamp(0.7rem, max(~{min(~,2Fvw, ~,2Fvh)~^, ~}), 2.6rem)"
               (loop for columns from 1 to (max 1 (min max-columns count))
                     append (list (/ 96 (* columns (+ width 1)))
                                  (/ 88 (max 1 (tallest heights columns))))))
       width))))

(defun song-title (id)
  "The title of the song ID, or ID itself when the store has no such song."
  (or (getf (find id (db:catalog) :key (lambda (entry) (getf entry :id)) :test #'string=)
            :title)
      id))

(defun tag-text (tag)
  "TAG, (category name note), as it is written and offered: mass=Communion."
  (format nil "~(~A~)=~A" (first tag) (second tag)))

(defun read-tag (text known)
  "The tag that TEXT names among KNOWN, the tags there are: one of them by
its TAG-TEXT or, when only one has it, by its name; a new one as
category=name; and any other text a new tag of the category TAG.  NIL for no
text; as a second value the tags of KNOWN that share the name TEXT, when it
cannot tell which is meant."
  (let* ((text (string-trim " " text))
         (named (remove text known :key #'second :test-not #'string-equal)))
    (cond ((string= text "") nil)
          ((find text known :key #'tag-text :test #'string-equal))
          ((song:parse-tag text))
          ((null named) (list :tag text nil))
          ((null (rest named)) (first named))
          (t (values nil named)))))

(defun build-console (body room)
  "Build the console of ROOM in BODY, and keep it drawn until its page goes."
  (let* ((offset (timezone-offset body))
         (root (create-div body :class "plp-console"))
         ;; The bar.
         (bar (create-div root :class "plp-bar"))
         (screen-link (progn
                        (create-span bar :content "Praise lyrics")
                        (create-span bar :class "plp-code" :content (live:room-code room))
                        (create-a bar :link (format nil "/plp-screen/~A" (live:room-code room))
                                      :content "Open the screen" :target "_blank")))
         (black (create-button bar :content "Black" :class "plp-keep"))
         (waiting (create-button bar :content "Waiting"))
         (footer (create-button bar :content "Footer"))
         (end-input (create-form-element bar :text :size 5))
         (end-button (create-button bar :content "Set end"))
         (waiting-input (create-form-element bar :text :size 24))
         (musician-button (create-button bar :content "Musician" :class "plp-keep"))
         (passphrase (progn
                       (create-div bar :class "plp-grow")
                       (create-form-element bar :password :size 14)))
         ;; The left column: the setlist and the search.
         (left (create-div root :class "plp-column plp-side"))
         (saved-fields (progn
                         (create-section left :h2 :content "Setlist")
                         (create-div left :class "plp-fields")))
         (saved (create-select saved-fields))
         (load-button (create-button saved-fields :content "Load"))
         (list-area (create-div left))
         (name-fields (create-div left :class "plp-fields"))
         (name-input (create-form-element name-fields :text))
         (save-list (create-button name-fields :content "Save"))
         (delete-list (create-button name-fields :content "Delete"))
         (search-fields (progn
                          (create-section left :h2 :content "Songs")
                          (create-div left :class "plp-fields")))
         (search (create-form-element search-fields :text))
         (tag-fields (create-div left :class "plp-fields"))
         (tag-select (create-select tag-fields))
         (results-area (create-div left))
         ;; The middle: the song's slides.
         (pane (create-div root :class "plp-column plp-pane"))
         (head (create-div pane :class "plp-song-head"))
         (title (create-div head :class "plp-song-title"))
         (transposer (create-div head :class "plp-transpose"))
         (lower (create-button transposer :content "♭"))
         (key-button (create-button transposer :content ""))
         (higher (create-button transposer :content "♯"))
         (writers (create-div pane :class "plp-song-writers"))
         (cards-area (create-div pane :class "plp-cards"))
         ;; The right column: the screen, and a song's text.
         (right (create-div root :class "plp-column plp-side"))
         (now (progn
                (create-section right :h2 :content "On the screen")
                (create-div right :class "plp-preview")))
         (next (progn
                 (create-section right :h2 :content "Next")
                 (create-div right :class "plp-preview plp-next")))
         (editor (progn
                   (create-section right :h2 :content "Song text")
                   (create-text-area right :rows 16)))
         (editor-buttons (create-div right :class "plp-fields"))
         (new-song (create-button editor-buttons :content "New"))
         (edit-song (create-button editor-buttons :content "Edit"))
         (this-song (create-button editor-buttons :content "This song"))
         (check-song (create-button editor-buttons :content "Check"))
         (save-song (create-button editor-buttons :content "Save"))
         (delete-song (create-button editor-buttons :content "Delete"))
         ;; A musician's two buttons, in the corner of the page.
         (nav (create-div root :class "plp-nav"))
         (nav-back (create-button nav :content "◂"))
         (nav-forward (create-button nav :content "▸"))
         ;; The form for a song, over the console.
         (popup (create-div root :class "plp-popup"))
         (dialog (create-div popup :class "plp-dialog"))
         (popup-heading (create-section dialog :h2 :content "New song"))
         (new-title (create-form-element dialog :text))
         (new-writers (create-form-element dialog :text))
         (new-copyright (create-form-element dialog :text))
         (new-order (create-form-element dialog :text :class "plp-order-input"))
         (tags-area (progn
                      (create-section dialog :h2 :content "Tags")
                      (create-div dialog :class "plp-tags")))
         (new-tag-fields (create-div dialog :class "plp-fields"))
         (tag-input (create-form-element new-tag-fields :text))
         (tag-add (create-button new-tag-fields :content "Add"))
         (tag-options (create-data-list dialog))
         (new-words (progn
                      (create-section dialog :h2 :content "Words")
                      (create-text-area dialog :rows 12)))
         (outline-area (progn
                         (create-section dialog :h2 :content "Structure")
                         (create-div dialog :class "plp-outline")))
         (popup-buttons (create-div dialog :class "plp-fields"))
         (popup-save (create-button popup-buttons :content "Save"))
         (done (create-button popup-buttons :content "To text"))
         (cancel (create-button popup-buttons :content "Cancel"))
         ;; What the console says and asks, over everything.
         (alert (create-div root :class "plp-popup plp-alert"))
         (alert-dialog (create-div alert :class "plp-dialog"))
         (alert-text (create-div alert-dialog :class "plp-alert-text"))
         (alert-buttons (create-div alert-dialog :class "plp-fields"))
         (alert-confirm (create-button alert-buttons :content "" :class "plp-confirm"))
         (alert-ok (create-button alert-buttons :content "OK"))
         (alert-open nil) (on-confirm nil)
         (popup-open nil)
         ;; The song the form corrects, or NIL when it makes a new one; the
         ;; tags it gives the song; and a new song's parts.
         (editing nil) (tags '())
         (parts '()) (places '())
         (musician nil)
         (cards '()) (live-card nil) (next-card nil)
         (results '()) (result-rows '()) (selected 0)
         ;; The song text that Save last held back as likely in the store
         ;; already: Save on the same text again stores it.
         (doubted nil)
         ;; Events come each in its own thread: one draws a list at a time.
         (lock (bordeaux-threads:make-recursive-lock "plp-console")))
    (declare (ignore screen-link))
    (create-div root :class "plp-keys" :content *key-help*)
    (setf (tab-index pane) 0
          (place-holder end-input) "20:00"
          (place-holder waiting-input) "Line on the waiting slide"
          (place-holder passphrase) "Passphrase"
          (place-holder name-input) "Name of the setlist"
          (place-holder search) "Title, number, writer, words"
          (place-holder new-title) "Title"
          (place-holder new-writers) "Writers"
          (place-holder new-copyright) "Copyright: © year, publisher"
          (place-holder new-order) "Order: 1 R 2 R (empty: as written)"
          (place-holder tag-input) "Add a tag: its name, or category=name for a new one"
          (attribute tag-input "list") (html-id tag-options)
          (attribute lower "title") "A semitone lower"
          (attribute higher "title") "A semitone higher"
          (attribute key-button "title") "Back to the key it is written in"
          (style cards-area "grid-template-columns")
          (format nil "repeat(~D, minmax(0, 1fr))" *card-columns*))
    (labels ((refocus ()
               ;; The keys go back to the pane, unless a box is open.
               (cond (alert-open (focus alert-ok))
                     (popup-open)
                     (t (focus pane))))
             (open-alert (text confirm action)
               (setf (inner-html alert-text) (escape-html text)
                     (inner-html alert-confirm) (escape-html (or confirm ""))
                     (inner-html alert-ok) (if action "Cancel" "OK")
                     on-confirm action
                     alert-open t)
               (if action
                   (add-class alert "plp-asking")
                   (remove-class alert "plp-asking"))
               (add-class alert "plp-open")
               (focus alert-ok))
             (close-alert ()
               (remove-class alert "plp-open")
               (setf alert-open nil
                     on-confirm nil))
             (say (control &rest arguments)
               (open-alert (apply #'format nil control arguments) nil nil))
             (ask (confirm action control &rest arguments)
               ;; Do ACTION once the button CONFIRM is pressed.
               (open-alert (apply #'format nil control arguments) confirm action))
             (authorized ()
               (or (db:authorized-p (value passphrase))
                   (progn (say "That needs the passphrase.") nil)))
             (on (button on-p)
               (if on-p (add-class button "plp-on") (remove-class button "plp-on")))
             (click (object function)
               ;; Every button gives the keys back to the pane.
               (set-on-click object (lambda (clicked)
                                      (declare (ignore clicked))
                                      (funcall function)
                                      (refocus))))
             (draw-cards ()
               (bordeaux-threads:with-recursive-lock-held (lock)
                 (let* ((state (live:snapshot room))
                        (song (getf state :song))
                        (deck (getf state :deck))
                        (semitones (getf state :transposition))
                        (key (and song (song:song-key song)))
                        (flats (and key (song:flat-key-p (song:transpose-chord key semitones)))))
                   (setf (inner-html title)
                         (escape-html (if song (song:song-title song) "No song yet"))
                         (inner-html writers)
                         (escape-html (or (and song (song:song-writers song))
                                          (if song "" "Search for one on the left.")))
                         (inner-html key-button)
                         (escape-html
                          (format nil "~:[Key~;~:*~A~]~[~:;~:* ~@D~]"
                                  (and key (song:transpose-chord key semitones :flats flats))
                                  (if (> semitones 6) (- semitones 12) semitones)))
                         (inner-html cards-area) ""
                         live-card nil
                         next-card nil)
                   (cond
                     (musician
                      ;; Each slide once: a tap shows it where it next comes.
                      (let ((made (loop for slide in (song:deck-chart deck)
                                        collect (let ((slide slide)
                                                      (card (create-div
                                                             cards-area :class "plp-card"
                                                             :content (chart-card-html
                                                                       slide :semitones semitones
                                                                             :flats flats))))
                                                  (click card
                                                         (lambda ()
                                                           (let ((state (live:snapshot room)))
                                                             (live:go-to
                                                              room
                                                              (song:deck-next (getf state :deck)
                                                                              (getf state :index)
                                                                              slide)))))
                                                  (cons slide card)))))
                        (multiple-value-bind (size width)
                            (chart-font-size (mapcar #'car made))
                          (setf (style cards-area "font-size") size
                                (style cards-area "column-width") (format nil "~,2Fem" width)))
                        (setf cards (loop for slide in deck
                                          collect (cdr (assoc slide made
                                                              :test #'song:same-slide-p))))))
                     (t
                      (setf (style cards-area "font-size") ""
                            (style cards-area "column-width") ""
                            cards (loop for slide in deck
                                        for index from 0
                                        collect (let ((card (create-div cards-area
                                                                        :class "plp-card"
                                                                        :content (card-html slide)))
                                                      (index index))
                                                  (click card (lambda () (live:go-to room index)))
                                                  card))))))))
             (draw-live ()
               (bordeaux-threads:with-recursive-lock-held (lock)
                 (let* ((state (live:snapshot room))
                        (mode (getf state :mode))
                        (card (nth (getf state :index) cards))
                        ;; A musician sees where the next slide is.
                        (after (and musician (nth (1+ (getf state :index)) cards)))
                        (after (and (not (eq after card)) after)))
                   (unless (eq after next-card)
                     (when next-card (remove-class next-card "plp-next-up"))
                     (when after (add-class after "plp-next-up"))
                     (setf next-card after))
                   (unless (eq card live-card)
                     (when live-card (remove-class live-card "plp-live"))
                     (when card
                       (add-class card "plp-live")
                       (scroll-into-view card))
                     (setf live-card card))
                   (on black (eq mode :black))
                   (on waiting (eq mode :waiting))
                   (setf (inner-html now)
                         (cond ((eq mode :black) "(black)")
                               ((eq mode :waiting) "(the waiting slide)")
                               ((getf state :slide) (slide-html (getf state :slide)))
                               (t ""))
                         (inner-html next)
                         (if (getf state :next) (slide-html (getf state :next)) "")))))
             (draw-list ()
               (let ((state (live:snapshot room)))
                 (setf (inner-html list-area) "")
                 (loop for (id . note) in (getf state :items)
                       for place from 0
                       do (let* ((place place)
                                 (row (create-div list-area
                                                  :class (if (eql place (getf state :position))
                                                             "plp-row plp-current"
                                                             "plp-row")))
                                 (name (create-span row :class "plp-name"
                                                        :content (format nil "~D. ~A~@[ <small>~A</small>~]"
                                                                         (1+ place)
                                                                         (escape-html (song-title id))
                                                                         (and (plusp (length note))
                                                                              (escape-html note))))))
                            (click name (lambda () (live:show-item room place)))
                            (click (create-button row :content "↑")
                                   (lambda () (live:move-item room place -1)))
                            (click (create-button row :content "↓")
                                   (lambda () (live:move-item room place 1)))
                            (click (create-button row :content "✕")
                                   (lambda () (live:remove-item room place)))))))
             (draw-session ()
               (let ((state (live:snapshot room)))
                 (on footer (getf state :footer-p))
                 (on end-button (getf state :end-time))
                 (setf (inner-html end-button)
                       (if (getf state :end-time)
                           (format nil "Ends ~A"
                                   (live:clock-text (getf state :end-time) offset))
                           "Set end"))))
             (draw (what)
               (ecase what
                 (:song (draw-cards) (draw-live) (draw-list))
                 (:slide (draw-live))
                 (:list (draw-list))
                 (:session (draw-session))))
             (show (id)
               ;; A song from the search goes after the one showing.
               (live:show-item room (live:add-item room id :after-current t)))
             (select-result (index)
               ;; Mark the INDEXth song found, or the nearest there is.
               (bordeaux-threads:with-recursive-lock-held (lock)
                 (when result-rows
                   (let ((old (nth selected result-rows))
                         (index (max 0 (min index (1- (length result-rows))))))
                     (when old (remove-class old "plp-current"))
                     (setf selected index)
                     (add-class (nth index result-rows) "plp-current")
                     (scroll-into-view (nth index result-rows))))))
             (draw-results ()
               (bordeaux-threads:with-recursive-lock-held (lock)
                 (setf results (if (and (string= (string-trim " " (value search)) "")
                                        (string= (value tag-select) ""))
                                   '()
                                   (db:search-songs (value search)
                                                    :tag (and (string/= (value tag-select) "")
                                                              (value tag-select))))
                       (inner-html results-area) ""
                       selected 0
                       result-rows
                       (loop for entry in results
                             collect (let* ((id (getf entry :id))
                                            (row (create-div results-area :class "plp-row"))
                                            (name (create-span
                                                   row :class "plp-name"
                                                   :content (format nil "~@[~D. ~]~A~@[ <small>~A</small>~]"
                                                                    (getf entry :number)
                                                                    (escape-html (getf entry :title))
                                                                    (and (getf entry :line)
                                                                         (escape-html (getf entry :line)))))))
                                       (click name (lambda () (show id)))
                                       (click (create-button row :content "Show")
                                              (lambda () (show id)))
                                       (click (create-button row :content "+")
                                              (lambda () (live:add-item room id)))
                                       row)))
                 (select-result 0)))
             (draw-tag-options ()
               ;; The tags there are: to search by, and to give a song.
               (let ((known (db:all-tags))
                     (chosen (value tag-select)))
                 (setf (inner-html tag-select) "")
                 (create-option tag-select :content "Any tag" :value "")
                 (dolist (tag known)
                   (create-option tag-select
                                  :value (tag-text tag)
                                  :selected (equal (tag-text tag) chosen)
                                  :content (escape-html
                                            (format nil "~(~A~): ~A~@[ (~A)~]"
                                                    (first tag) (second tag) (third tag)))))
                 (setf (inner-html tag-options)
                       (format nil "~{<option value='~A'>~}"
                               (mapcar (lambda (tag) (escape-html (tag-text tag))) known)))))
             (draw-tags ()
               ;; The song's tags, each a button that takes it off.
               (bordeaux-threads:with-recursive-lock-held (lock)
                 (setf (inner-html tags-area) "")
                 (if (null tags)
                     (create-span tags-area :content "None yet.")
                     (dolist (tag tags)
                       (let ((tag tag))
                         (set-on-click
                          (create-button tags-area
                                         :class "plp-tag"
                                         :content (format nil "~A~@[ (~A)~] ✕"
                                                          (escape-html (tag-text tag))
                                                          (and (third tag)
                                                               (escape-html (third tag)))))
                          (lambda (object)
                            (declare (ignore object))
                            (bordeaux-threads:with-recursive-lock-held (lock)
                              (setf tags (remove tag tags :test #'equal))
                              (draw-tags))
                            (focus tag-input))))))))
             (add-tag ()
               ;; Give the song the tag the field names.
               (multiple-value-bind (tag several) (read-tag (value tag-input) (db:all-tags))
                 (cond (tag
                        (bordeaux-threads:with-recursive-lock-held (lock)
                          (unless (member (tag-text tag) tags :key #'tag-text :test #'string=)
                            (setf tags (append tags (list tag))))
                          (setf (value tag-input) "")
                          (draw-tags)))
                       (several
                        (say "Several tags have that name: write ~{~A~^ or ~}."
                             (mapcar #'tag-text several))))))
             (words-sections ()
               ;; What the form's words write, for a song being corrected.
               (song:parse-words (text-value new-words)))
             (order-keys ()
               (mapcar #'string-upcase
                       (remove "" (uiop:split-string (value new-order) :separator " ,")
                               :test #'string=)))
             (outline-row (key label line &optional times)
               (create-span
                (create-div outline-area :class "plp-row")
                :class "plp-name"
                :content (format nil "<kbd>~A</kbd> ~@[~A: ~]~A~@[ <small>×~D</small>~]"
                                 (escape-html key)
                                 (and label (escape-html label))
                                 (escape-html (or line ""))
                                 times)))
             (draw-outline ()
               ;; What the form's words are taken for, part by part.
               (bordeaux-threads:with-recursive-lock-held (lock)
                 (setf (inner-html outline-area) "")
                 (cond
                   (editing
                    (let* ((sections (words-sections))
                           (keys (mapcar #'song:section-key sections)))
                      (dolist (section sections)
                        (outline-row (song:section-key section) (song:section-name section)
                                     (first (first (song:section-slides section)))))
                      (create-div outline-area
                                  :class "plp-order"
                                  :content (format nil "Sung: ~{~A~^ ~}"
                                                   (mapcar #'escape-html
                                                           (or (order-keys) keys))))))
                   ((null parts)
                    (create-div outline-area
                                :content "The parts of the song will be listed here."))
                   (t
                    (let* ((song (song:outline-song parts places))
                           (sections (song:song-sections song))
                           (keys (mapcar #'song:section-key sections)))
                      (loop for section in sections
                            for index from 0
                            do (let* ((index index)
                                      (kind (song:section-kind section))
                                      (row (create-div outline-area :class "plp-row"))
                                      (select (create-select row)))
                                 (dolist (option (if (member kind song:*outline-kinds*)
                                                     song:*outline-kinds*
                                                     (cons kind song:*outline-kinds*)))
                                   (create-option select
                                                  :value (string-downcase option)
                                                  :selected (eq option kind)
                                                  :content (escape-html
                                                            (or (song:kind-label option)
                                                                (song:section-label section)))))
                                 (create-span
                                  row :class "plp-name"
                                  :content (format nil "<kbd>~A</kbd> ~A~@[ <small>×~D</small>~]"
                                                   (escape-html (nth index keys))
                                                   (escape-html
                                                    (first (first (song:section-slides section))))
                                                   (let ((times (count index places)))
                                                     (and (> times 1) times))))
                                 (set-on-change
                                  select
                                  (lambda (object)
                                    (declare (ignore object))
                                    (let ((kind (find (value select) song:*outline-kinds*
                                                      :test #'string-equal)))
                                      (when kind
                                        (bordeaux-threads:with-recursive-lock-held (lock)
                                          (when (< index (length parts))
                                            (setf (nth index parts)
                                                  (song:relabel (nth index parts) kind)))
                                          (draw-outline))))))))
                      (create-div outline-area
                                  :class "plp-order"
                                  :content (format nil "Sung: ~{~A~^ ~}"
                                                   (mapcar #'escape-html
                                                           (or (song:song-order song) keys)))))))))
             (open-popup (song)
               ;; The form on SONG, or with NIL on a new song.
               (bordeaux-threads:with-recursive-lock-held (lock)
                 (setf editing song
                       tags (and song (copy-list (song:song-tags song)))
                       parts '()
                       places '()
                       (inner-html popup-heading) (if song "Edit song" "New song")
                       (value new-title) (or (and song (song:song-title song)) "")
                       (value new-writers) (or (and song (song:song-writers song)) "")
                       (value new-copyright) (or (and song (song:song-copyright song)) "")
                       (value new-order) (format nil "~{~A~^ ~}" (and song (song:song-order song)))
                       (value tag-input) ""
                       (text-value new-words)
                       (if song (song:print-sections (song:song-sections song)) "")
                       (place-holder new-words)
                       (if song
                           "The words, each part under its label: 1. or Refrain:"
                           "Paste the words here, a blank line between the verses; chords may stand over them"))
                 (if song
                     (add-class dialog "plp-editing")
                     (remove-class dialog "plp-editing"))
                 (draw-tags)
                 (draw-outline)
                 (setf popup-open t))
               (add-class popup "plp-open")
               (focus new-title))
             (close-popup ()
               (remove-class popup "plp-open")
               (setf popup-open nil))
             (popup-text ()
               ;; The song the form holds, in the text format.
               (bordeaux-threads:with-recursive-lock-held (lock)
                 (flet ((filled (field)
                          (let ((text (string-trim " " (value field))))
                            (and (string/= text "") text))))
                   (cond (editing
                          (let ((song (song:copy-song editing)))
                            (setf (song:song-title song) (filled new-title)
                                  (song:song-writers song) (filled new-writers)
                                  (song:song-copyright song) (filled new-copyright)
                                  (song:song-order song) (order-keys)
                                  (song:song-tags song) tags
                                  (song:song-sections song) (words-sections))
                            (song:print-song song)))
                         (parts
                          (song:print-song
                           (song:outline-song parts places
                                              :title (value new-title)
                                              :writers (value new-writers)
                                              :copyright (value new-copyright)
                                              :tags tags)))
                         (t
                          (format nil "~A~A"
                                  (song:print-song
                                   (song:make-song :title (filled new-title)
                                                   :writers (filled new-writers)
                                                   :copyright (filled new-copyright)
                                                   :tags tags))
                                  *new-song-text*))))))
             (save-text (text)
               ;; Store the song TEXT writes.  Its id when it was stored.
               (when (authorized)
                 (let* ((parsed (song:parse-song text))
                        (problems (song:song-problems parsed))
                        ;; A new song is looked for among those stored,
                        ;; unless this very text was held back just before.
                        (similar (and (not problems)
                                      (not (equal text doubted))
                                      (db:similar-songs parsed))))
                   (cond (problems
                          (say "~{~A~^~%~}" problems)
                          nil)
                         (similar
                          (setf doubted text)
                          (say "Not saved: there ~:[are songs~;is a song~] like it already.~%~:{~@[~D. ~]~A (~A)~%~}Save again to add it all the same."
                               (null (rest similar))
                               (mapcar (lambda (entry)
                                         (list (getf entry :number) (getf entry :title)
                                               (getf entry :id)))
                                       similar))
                          nil)
                         (t
                          (let ((id (db:save-song parsed))
                                (showing (getf (live:snapshot room) :song)))
                            (setf doubted nil)
                            (say "Saved as ~A." id)
                            (when (and showing (equal (song:song-id showing) id))
                              (live:show-song room id))
                            (draw-tag-options)
                            (draw-results)
                            id))))))
             (draw-saved ()
               (setf (inner-html saved) "")
               (create-option saved :content "Saved setlists…" :value "")
               (dolist (setlist (db:all-setlists))
                 (create-option saved :value (getf setlist :id)
                                      :content (escape-html
                                                (format nil "~A (~D)" (getf setlist :name)
                                                        (length (getf setlist :items))))))))
      ;; What the console says.
      (set-on-click alert-ok
                    (lambda (object)
                      (declare (ignore object))
                      (close-alert)
                      (refocus)))
      (set-on-click alert-confirm
                    (lambda (object)
                      (declare (ignore object))
                      (let ((action on-confirm))
                        (close-alert)
                        (when action (funcall action))
                        (refocus))))
      (set-on-key-down alert-dialog
                       (lambda (object event)
                         (declare (ignore object))
                         (when (equal (getf event :key) "Escape")
                           (close-alert)
                           (refocus))))
      ;; The bar.
      (click black (lambda () (live:set-mode room :black)))
      (click waiting (lambda () (live:set-mode room :waiting)))
      (click footer (lambda ()
                      (live:set-footer room (not (getf (live:snapshot room) :footer-p)))))
      (click end-button
             (lambda ()
               (let* ((text (string-trim " " (value end-input)))
                      (time (live:parse-clock text (get-universal-time) offset)))
                 (cond ((string= text "") (live:set-end-time room nil))
                       (time (live:set-end-time room time))
                       (t (say "The end is a time of day, as 20:00."))))))
      (set-on-change waiting-input
                     (lambda (object)
                       (declare (ignore object))
                       (live:set-waiting-text room (value waiting-input))))
      ;; The musician's page.
      (click musician-button
             (lambda ()
               (bordeaux-threads:with-recursive-lock-held (lock)
                 (setf musician (not musician))
                 (on musician-button musician)
                 (if musician
                     (add-class root "plp-musician")
                     (remove-class root "plp-musician"))
                 (draw-cards)
                 (draw-live))))
      (click lower (lambda () (live:transpose room -1)))
      (click higher (lambda () (live:transpose room 1)))
      (click key-button (lambda () (live:transpose room nil)))
      (click nav-back (lambda () (live:step-slide room -1)))
      (click nav-forward (lambda () (live:step-slide room 1)))
      ;; The setlist.
      (click load-button
             (lambda ()
               (let ((setlist (db:find-setlist (value saved))))
                 (when setlist
                   (setf (value name-input) (getf setlist :name))
                   (live:set-items room (getf setlist :items)
                                   :transpositions (getf setlist :transpositions))))))
      (click save-list
             (lambda ()
               (when (authorized)
                 (handler-case
                     (let ((items (getf (live:snapshot room) :items)))
                       (db:save-setlist (string-trim " " (value name-input)) items
                                        ;; The keys of its songs, and of no other.
                                        :transpositions
                                        (remove-if-not (lambda (id)
                                                         (assoc id items :test #'equal))
                                                       (live:transpositions room)
                                                       :key #'car))
                       (draw-saved)
                       (say "Saved the setlist ~A." (value name-input)))
                   (error (condition) (say "~A" condition))))))
      (click delete-list
             (lambda ()
               (when (authorized)
                 (let* ((id (song:slug (value name-input)))
                        (setlist (db:find-setlist id)))
                   (if setlist
                       (ask "Delete"
                            (lambda ()
                              (db:delete-setlist id)
                              (draw-saved)
                              (say "Deleted the setlist ~A." (getf setlist :name)))
                            "Delete the setlist ~A, with its ~D song~:P?"
                            (getf setlist :name) (length (getf setlist :items)))
                       (say "There is no setlist of that name."))))))
      ;; The search.
      (draw-tag-options)
      (set-on-input search (lambda (object) (declare (ignore object)) (draw-results)))
      (set-on-change tag-select (lambda (object) (declare (ignore object)) (draw-results)))
      (set-on-key-down search
                       (lambda (object event)
                         (declare (ignore object))
                         (let ((key (getf event :key)))
                           (cond ((equal key "Escape") (focus pane))
                                 ((equal key "ArrowDown") (select-result (1+ selected)))
                                 ((equal key "ArrowUp") (select-result (1- selected)))
                                 ((and (equal key "Enter") (nth selected results))
                                  (let ((id (getf (nth selected results) :id)))
                                    (if (getf event :shift-key)
                                        (progn (show id) (focus pane))
                                        (live:add-item room id)))))))
                       :disable-default '("ArrowUp" "ArrowDown"))
      ;; The keys.  The browser would scroll the pane for those listed.  A
      ;; musician's cards stand in no rows: there the arrows go back and on.
      (set-on-key-down pane
                       (lambda (object event)
                         (declare (ignore object))
                         (let ((key (getf event :key))
                               (state (live:snapshot room)))
                           (cond ((equal key "/") (focus search))
                                 ((and (member key '("ArrowUp" "ArrowDown") :test #'equal)
                                       (not musician)
                                       (eq (getf state :mode) :lyrics)
                                       (not (or (getf event :ctrl-key) (getf event :alt-key)
                                                (getf event :meta-key))))
                                  (live:go-to room (row-step (getf state :index) (length cards)
                                                             *card-columns*
                                                             (if (equal key "ArrowUp") -1 1))))
                                 (t (handle-key room event)))))
                       :disable-default '("ArrowUp" "ArrowDown" "ArrowLeft" "ArrowRight"
                                          " " "PageUp" "PageDown" "/"))
      ;; The form.
      (set-on-click new-song
                    (lambda (object)
                      (declare (ignore object))
                      (open-popup nil)))
      (set-on-click edit-song
                    (lambda (object)
                      (declare (ignore object))
                      ;; The song showing, or else the one in the text field.
                      (let* ((showing (getf (live:snapshot room) :song))
                             (written (song:parse-song (text-value editor)))
                             (song (or showing
                                       (and (song:song-sections written) written))))
                        (if song
                            (open-popup song)
                            (say "No song is showing, and the text field holds none.")))))
      (set-on-input new-words
                    (lambda (object)
                      (declare (ignore object))
                      (bordeaux-threads:with-recursive-lock-held (lock)
                        (unless editing
                          (multiple-value-setq (parts places)
                            (song:outline (text-value new-words))))
                        (draw-outline))))
      (set-on-change new-order (lambda (object) (declare (ignore object)) (draw-outline)))
      ;; A tag of those offered is taken as soon as the field holds it.
      (set-on-input tag-input
                    (lambda (object)
                      (declare (ignore object))
                      (when (find (string-trim " " (value tag-input)) (db:all-tags)
                                  :key #'tag-text :test #'string-equal)
                        (add-tag))))
      (set-on-key-down tag-input
                       (lambda (object event)
                         (declare (ignore object))
                         (when (equal (getf event :key) "Enter")
                           (add-tag))))
      (set-on-click tag-add
                    (lambda (object)
                      (declare (ignore object))
                      (add-tag)
                      (unless alert-open (focus tag-input))))
      (set-on-key-down dialog
                       (lambda (object event)
                         (declare (ignore object))
                         (when (equal (getf event :key) "Escape")
                           (close-popup)
                           (refocus))))
      (click cancel #'close-popup)
      (click done
             (lambda ()
               (setf (text-value editor) (popup-text))
               (close-popup)
               (say "The song is in the text field: Check it, then Save.")))
      (click popup-save
             (lambda ()
               (let ((id (save-text (popup-text))))
                 (when id
                   ;; As stored, with the id it was given.
                   (setf (text-value editor) (song:print-song (db:find-song id)))
                   (close-popup)))))
      ;; The song's text.
      (click this-song
             (lambda ()
               (let ((song (getf (live:snapshot room) :song)))
                 (if song
                     (setf (text-value editor) (song:print-song song))
                     (say "No song is showing.")))))
      (click check-song
             (lambda ()
               (let* ((parsed (song:parse-song (text-value editor)))
                      (problems (song:song-problems parsed)))
                 (if problems
                     (say "~{~A~^~%~}" problems)
                     (say "~D slides: ~{~A~^ ~}" (length (song:song-deck parsed))
                          (mapcar #'song:slide-key (song:song-deck parsed)))))))
      (click save-song (lambda () (save-text (text-value editor))))
      (click delete-song
             (lambda ()
               (when (authorized)
                 (let* ((id (song:song-id (song:parse-song (text-value editor))))
                        (song (and id (db:find-song id))))
                   (cond ((null id) (say "The text names no song: it has no Id: header."))
                         ((null song) (say "There is no song ~A." id))
                         (t
                          (ask "Delete"
                               (lambda ()
                                 (cond ((db:delete-song id)
                                        (draw-results)
                                        (say "Deleted ~A." id))
                                       (t (say "There is no song ~A." id))))
                               "Delete ~@[~D. ~]~A (~A) from the songs?"
                               (song:song-number song) (song:song-title song) id)))))))
      (draw-saved)
      (draw :song)
      (draw :session)
      (focus pane)
      (let ((subscription (live:subscribe room #'draw)))
        (unwind-protect
             (loop while (validp body) do (sleep 5))
          (live:unsubscribe room subscription))))))

(defun on-console (body)
  "/plp-console/CODE: the console of the room in the address."
  (let ((room (live:ensure-room (path-code (path-name (location body)) "/plp-console/"))))
    (setup-page body "Lyrics console")
    (if room
        (build-console body room)
        (create-div body :class "plp-notice"
                         :content "No such console: start from <a href='/plp'>/plp</a>."))))

(defun register-routes ()
  "Serve the presenter's pages.  The console and the screen take the room's
code as the rest of the path, which needs CLOG's :EXTENDED-ROUTING; neither
is under /plp/, whose routes that would shadow."
  (set-on-new-window 'on-front :path "/plp")
  (set-on-new-window 'on-console :path "/plp-console")
  (set-on-new-window 'on-screen :path "/plp-screen"))
