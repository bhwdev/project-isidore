;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/plp/live
  (:use #:common-lisp)
  ;; CL:ROOM reports on memory; here a room is a session of presenting.
  (:shadow #:room)
  (:import-from #:bordeaux-threads)
  (:import-from #:cl-ppcre)
  (:local-nicknames (#:song #:bhw/plp/song)
                    (#:db #:bhw/plp/database))
  (:export #:room #:room-code #:room-song #:room-deck #:room-index #:room-mode
           #:room-items #:room-position #:room-end-time #:room-footer-p
           #:room-waiting-image #:room-waiting-text #:room-transposition
           #:valid-code-p #:new-code #:ensure-room #:find-room #:sweep-rooms
           #:subscribe #:unsubscribe
           #:snapshot
           #:show-song #:go-to #:step-slide #:jump #:set-mode #:press-key
           #:transpose #:transpositions
           #:set-items #:add-item #:move-item #:remove-item #:show-item #:step-song
           #:set-end-time #:set-footer #:set-waiting-text
           #:images-directory #:waiting-images #:image-file
           #:clock-text #:countdown-text #:parse-clock)
  (:documentation
   "What is on the screen, and who is watching.

A ROOM is one session of presenting: the song and its deck of slides, the
slide showing, the mode (:LYRICS, :BLACK or :WAITING), the working setlist,
the session's end time, and how far from its written key each song is played
for the musicians' pages.  Its code is in the address of its console and
of its screens, which is all that ties them together: whoever opens
/plp-screen/CODE sees what /plp-console/CODE drives.  Rooms live in memory
only; one nobody has touched for a day is dropped.

A page subscribes a function to its room and is called, with what changed
(:SLIDE, :SONG, :LIST or :SESSION), after every change, in the thread that
made it; a subscriber that signals an error is dropped, its connection being
gone.

PRESS-KEY is the key map, the same for the console and for a screen that is
driven directly."))

(in-package #:bhw/plp/live)

(defstruct room
  code
  (lock (bordeaux-threads:make-recursive-lock "plp-room"))
  song (deck '()) (index 0) (mode :lyrics)
  (items '()) position
  end-time (footer-p t)
  waiting-image (waiting-text "")
  ;; Song ids to semitones above the written key.
  (transpositions (make-hash-table :test #'equal))
  (subscribers '()) (next-subscriber 0)
  (touched (get-universal-time)))

(defvar *rooms* (make-hash-table :test #'equal)
  "The rooms by code.")

(defvar *rooms-lock* (bordeaux-threads:make-lock "plp-rooms"))

(defparameter *room-lifetime* (* 24 60 60)
  "Seconds after its last change that a room nobody watches is dropped.")

(defun valid-code-p (code)
  "True when CODE can be a room's: four to twelve capitals and digits."
  (and (stringp code) (cl-ppcre:scan "^[A-Z0-9]{4,12}$" code) t))

(defun new-code ()
  "A code no room has: six characters, without the ones read for each other."
  (let ((alphabet "ABCDEFGHJKLMNPQRSTUVWXYZ23456789")
        (state (make-random-state t)))
    (loop for code = (map 'string (lambda (i) (declare (ignore i))
                                    (char alphabet (random (length alphabet) state)))
                          "123456")
          unless (gethash code *rooms*) return code)))

(defun sweep-rooms (&optional (now (get-universal-time)))
  "Drop the rooms that nobody watches and nobody has changed for a day."
  (bordeaux-threads:with-lock-held (*rooms-lock*)
    (maphash (lambda (code room)
               (when (and (null (room-subscribers room))
                          (> (- now (room-touched room)) *room-lifetime*))
                 (remhash code *rooms*)))
             *rooms*)))

(defun find-room (code)
  (bordeaux-threads:with-lock-held (*rooms-lock*)
    (gethash code *rooms*)))

(defun ensure-room (code)
  "The room whose code is CODE, made when there is none: a console's address
outlives a restart of the server.  NIL when CODE is not a VALID-CODE-P."
  (when (valid-code-p code)
    (sweep-rooms)
    (bordeaux-threads:with-lock-held (*rooms-lock*)
      (or (gethash code *rooms*)
          (setf (gethash code *rooms*) (make-room :code code))))))

;;; Subscribers

(defun subscribe (room function)
  "Have FUNCTION called with what changed after every change to ROOM.
Returns what UNSUBSCRIBE takes."
  (bordeaux-threads:with-recursive-lock-held ((room-lock room))
    (let ((id (incf (room-next-subscriber room))))
      (push (cons id function) (room-subscribers room))
      id)))

(defun unsubscribe (room id)
  (bordeaux-threads:with-recursive-lock-held ((room-lock room))
    (setf (room-subscribers room)
          (remove id (room-subscribers room) :key #'car))))

(defun notify (room what)
  (dolist (subscriber (bordeaux-threads:with-recursive-lock-held ((room-lock room))
                        (copy-list (room-subscribers room))))
    (handler-case (funcall (cdr subscriber) what)
      (error () (unsubscribe room (car subscriber))))))

(defmacro changing ((room what) &body body)
  "Run BODY holding ROOM's lock, then tell its subscribers that WHAT changed.
Returns BODY's value."
  (let ((r (gensym "ROOM")))
    `(let ((,r ,room))
       (prog1 (bordeaux-threads:with-recursive-lock-held ((room-lock ,r))
                (setf (room-touched ,r) (get-universal-time))
                ,@body)
         (notify ,r ,what)))))

(defun snapshot (room)
  "ROOM's state as a plist, read under its lock: :SONG, :DECK, :INDEX, :MODE,
:SLIDE (the one showing, or NIL), :NEXT (the one after it), :ITEMS,
:POSITION, :END-TIME, :FOOTER-P, :WAITING-IMAGE, :WAITING-TEXT and
:TRANSPOSITION (see ROOM-TRANSPOSITION)."
  (bordeaux-threads:with-recursive-lock-held ((room-lock room))
    (let ((deck (room-deck room)) (index (room-index room)))
      (list :song (room-song room) :deck deck :index index :mode (room-mode room)
            :slide (nth index deck) :next (nth (1+ index) deck)
            :items (room-items room) :position (room-position room)
            :end-time (room-end-time room) :footer-p (room-footer-p room)
            :waiting-image (room-waiting-image room)
            :waiting-text (room-waiting-text room)
            :transposition (room-transposition room)))))

;;; Slides

(defun show-song (room id)
  "Put the song whose id is ID on ROOM's screen, at its first slide.  Returns
NIL when there is no such song."
  (let ((song (db:find-song id)))
    (when song
      (changing (room :song)
        (setf (room-song room) song
              (room-deck room) (song:song-deck song)
              (room-index room) 0
              (room-mode room) :lyrics
              (room-position room) (or (position id (room-items room)
                                                 :key #'car :test #'string=)
                                       (room-position room))))
      t)))

(defun room-transposition (room)
  "How many semitones above its written key ROOM plays the song showing: 0
to 11."
  (bordeaux-threads:with-recursive-lock-held ((room-lock room))
    (let ((song (room-song room)))
      (or (and song (gethash (song:song-id song) (room-transpositions room)))
          0))))

(defun transpose (room delta)
  "Play the song showing in ROOM DELTA semitones higher than it is played
now, for as long as the room lives; a DELTA of NIL puts it back in its
written key.  Returns NIL when no song is showing."
  (changing (room :song)
    (let ((song (room-song room)))
      (when song
        (setf (gethash (song:song-id song) (room-transpositions room))
              (if delta
                  (mod (+ (room-transposition room) delta) 12)
                  0))
        t))))

(defun transpositions (room)
  "The songs ROOM plays in another key than they are written in, as a list
of (song-id . semitones), by id: what a setlist is saved with."
  (bordeaux-threads:with-recursive-lock-held ((room-lock room))
    (sort (loop for id being the hash-keys of (room-transpositions room)
                  using (hash-value semitones)
                unless (zerop semitones)
                  collect (cons id semitones))
          #'string< :key #'car)))

(defun go-to (room index)
  "Show the slide at INDEX of ROOM's deck, if it has one."
  (changing (room :slide)
    (when (and (integerp index) (< -1 index (length (room-deck room))))
      (setf (room-index room) index
            (room-mode room) :lyrics)
      t)))

(defun step-slide (room delta)
  "Show the slide DELTA after the one showing, staying within the deck.  From
black or waiting, the first step forward only brings the slide back."
  (changing (room :slide)
    (cond ((and (not (eq (room-mode room) :lyrics)) (plusp delta))
           (setf (room-mode room) :lyrics))
          (t
           (setf (room-mode room) :lyrics
                 (room-index room) (max 0 (min (+ (room-index room) delta)
                                               (1- (length (room-deck room))))))))))

(defun jump (room group)
  "Show the slide that the key GROUP (\"2\", \"R\", \"B\") leads to; see
BHW/PLP/SONG:DECK-JUMP.  Returns NIL when the song has no such section."
  (changing (room :slide)
    (let ((index (song:deck-jump (room-deck room)
                                 (and (eq (room-mode room) :lyrics) (room-index room))
                                 group)))
      (when index
        (setf (room-index room) index
              (room-mode room) :lyrics)
        t))))

(defun waiting-image-after (image)
  "The waiting image to show after IMAGE: the next of WAITING-IMAGES, round
again from the last; NIL when there are none."
  (let* ((images (mapcar #'car (waiting-images)))
         (here (position image images :test #'equal)))
    (and images (nth (if here (mod (1+ here) (length images)) 0) images))))

(defun set-mode (room mode)
  "Put ROOM in MODE, or back to :LYRICS when it is in MODE already.  Each
time the waiting slide comes up it shows the next image."
  (changing (room :slide)
    (cond ((eq (room-mode room) mode)
           (setf (room-mode room) :lyrics))
          (t
           (when (eq mode :waiting)
             (setf (room-waiting-image room)
                   (waiting-image-after (room-waiting-image room))))
           (setf (room-mode room) mode)))))

;;; The setlist

(defun set-items (room items &key transpositions)
  "Make ITEMS, a list of (song-id . note), ROOM's setlist, and play its songs
in the keys TRANSPOSITIONS gives, a list of (song-id . semitones): a song of
ITEMS it does not name, in the key it is written in."
  (changing (room :list)
    (dolist (item items)
      (let ((semitones (cdr (assoc (car item) transpositions :test #'equal))))
        (setf (gethash (car item) (room-transpositions room))
              (if (integerp semitones) (mod semitones 12) 0))))
    (setf (room-items room) (copy-list items)
          (room-position room)
          (and (room-song room)
               (position (song:song-id (room-song room)) items
                         :key #'car :test #'string=))))
  ;; The song showing may be in another key now.
  (notify room :song))

(defun add-item (room id &key after-current)
  "Add the song ID to ROOM's setlist: at the end, or with AFTER-CURRENT after
the song showing.  Returns its place."
  (changing (room :list)
    (let* ((items (room-items room))
           (place (if (and after-current (room-position room))
                      (1+ (room-position room))
                      (length items))))
      (setf (room-items room)
            (append (subseq items 0 place) (list (cons id "")) (nthcdr place items)))
      place)))

(defun move-item (room place delta)
  "Move the PLACEth item of ROOM's setlist DELTA places, within the list."
  (changing (room :list)
    (let* ((items (copy-list (room-items room)))
           (to (+ place delta)))
      (when (and (< -1 place (length items)) (< -1 to (length items)))
        (rotatef (nth place items) (nth to items))
        (setf (room-items room) items)
        (let ((position (room-position room)))
          (setf (room-position room)
                (cond ((eql position place) to)
                      ((eql position to) place)
                      (t position))))
        t))))

(defun remove-item (room place)
  "Take the PLACEth item out of ROOM's setlist."
  (changing (room :list)
    (let ((items (room-items room)) (position (room-position room)))
      (when (< -1 place (length items))
        (setf (room-items room) (append (subseq items 0 place) (nthcdr (1+ place) items))
              (room-position room) (cond ((null position) nil)
                                         ((= position place) nil)
                                         ((> position place) (1- position))
                                         (t position)))
        t))))

(defun show-item (room place)
  "Show the song at PLACE of ROOM's setlist.  Returns NIL when the list has
no such place or the store no such song."
  (let ((item (and (integerp place) (>= place 0)
                   (nth place (getf (snapshot room) :items)))))
    (when (and item (show-song room (car item)))
      (changing (room :list)
        (setf (room-position room) place))
      t)))

(defun step-song (room delta)
  "Show the song DELTA places on in ROOM's setlist from the one showing, or
its first when none of it is.  Returns NIL at either end."
  (let ((position (getf (snapshot room) :position)))
    (show-item room (if position (+ position delta) 0))))

;;; The session

(defun set-end-time (room universal-time)
  "Count ROOM's footer down to UNIVERSAL-TIME; NIL for no countdown."
  (changing (room :session)
    (setf (room-end-time room) universal-time)))

(defun set-footer (room show-p)
  (changing (room :session)
    (setf (room-footer-p room) (and show-p t))))

(defun set-waiting-text (room text)
  (changing (room :session)
    (setf (room-waiting-text room) (or text ""))))

;;; Keys

(defun press-key (room key &key shift)
  "Do in ROOM what the key KEY (a DOM key value: \"r\", \"ArrowRight\", \" \")
does, SHIFT held or not.  Returns true when the key is one of the map's:

  Space, ArrowRight, ArrowDown,
  PageDown                      the next slide of the song's order
  ArrowLeft, ArrowUp, PageUp    the one before (the console, whose cards
                                stand in rows, takes ArrowUp and ArrowDown
                                itself, for the card above and below)
  1-9                           verse N
  R or C, B, P, T, O            refrain, bridge, pre-chorus, tag or build,
                                outro or coda; again for the section's next
                                slide or its second (Refrain 2)
  .                             black, and back
  W                             the waiting slide, and back
  N, Shift+N                    the next, the previous song of the setlist"
  (let ((key (if (= (length key) 1) (string-upcase key) key)))
    (cond ((member key '(" " "ArrowRight" "ArrowDown" "PageDown") :test #'string=)
           (step-slide room 1) t)
          ((member key '("ArrowLeft" "ArrowUp" "PageUp") :test #'string=)
           (step-slide room -1) t)
          ((and (= (length key) 1) (find (char key 0) "123456789RCBPTO"))
           (jump room key) t)
          ((string= key ".") (set-mode room :black) t)
          ((string= key "W") (set-mode room :waiting) t)
          ((string= key "N") (step-song room (if shift -1 1)) t))))

;;; Waiting images

(defun images-directory ()
  "<data-root>/plp/images/, the pictures of the waiting slide; credits.txt
there gives a line of credit for each: its file name, a tab, the credit."
  (uiop:subpathname (db:plp-directory) "images/"))

(defun waiting-images ()
  "The waiting slide's pictures as a list of (file-name . credit), by name;
the credit is NIL for a file that credits.txt does not list."
  (let* ((directory (images-directory))
         (credits (let ((file (uiop:subpathname directory "credits.txt")))
                    (and (probe-file file)
                         (loop for line in (uiop:read-file-lines file :external-format :utf-8)
                               for tab = (position #\Tab line)
                               when tab
                                 collect (cons (subseq line 0 tab)
                                               (string-trim " " (subseq line (1+ tab)))))))))
    (sort (loop for file in (uiop:directory-files directory)
                for name = (file-namestring file)
                when (member (pathname-type file) '("jpg" "jpeg" "png" "webp")
                             :test #'string-equal)
                  collect (cons name (cdr (assoc name credits :test #'string=))))
          #'string< :key #'car)))

(defun image-file (path)
  "The file of the waiting image that the request path PATH,
/plp-images/NAME, asks for; NIL when PATH is not such a path or the images
directory has no picture of that name."
  (cl-ppcre:register-groups-bind (name)
      ("^/plp-images/([A-Za-z0-9_][A-Za-z0-9_.-]*\\.(?i:jpe?g|png|webp))$" path)
    (probe-file (uiop:subpathname (images-directory) name))))

;;; The clock

(defun clock-text (universal-time offset-minutes)
  "The time of day of UNIVERSAL-TIME, as \"19:42\", where the clocks are
OFFSET-MINUTES behind UTC (what JavaScript's getTimezoneOffset gives: 300 in
Ottawa in winter)."
  (multiple-value-bind (second minute hour)
      (decode-universal-time (- universal-time (* 60 offset-minutes)) 0)
    (declare (ignore second))
    (format nil "~2,'0D:~2,'0D" hour minute)))

(defun countdown-text (now end-time)
  "What is left of the session at NOW, as \"18 min\" or \"1 h 05 min\"; NIL
when there is no END-TIME or it has passed."
  (when (and end-time (> end-time now))
    (let ((minutes (ceiling (- end-time now) 60)))
      (if (< minutes 60)
          (format nil "~D min" minutes)
          (format nil "~D h ~2,'0D min" (floor minutes 60) (mod minutes 60))))))

(defun parse-clock (text now offset-minutes)
  "The next universal time after NOW at which the clocks OFFSET-MINUTES
behind UTC show TEXT, \"20:00\" or \"8:05\"; NIL when TEXT is not a time."
  (cl-ppcre:register-groups-bind ((#'parse-integer hour minute))
      ("^\\s*(\\d{1,2})[:.h](\\d{2})\\s*$" text)
    (when (and (< hour 24) (< minute 60))
      (let* ((local (- now (* 60 offset-minutes)))
             (midnight (* 86400 (floor local 86400)))
             (at (+ midnight (* 3600 hour) (* 60 minute)))
             (at (if (> at local) at (+ at 86400))))
        (+ at (* 60 offset-minutes))))))
