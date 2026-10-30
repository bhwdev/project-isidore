;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/slack-bot/database
  (:use #:common-lisp)
  (:import-from #:graph-db)
  (:local-nicknames (#:store #:project-isidore/store))
  (:export #:*database*
           #:store-path
           #:open-database
           #:close-database

           #:slack-subscription
           #:make-slack-subscription
           #:find-slack-subscription
           #:all-slack-subscriptions
           #:add-slack-subscription
           #:remove-slack-subscription

           #:slack-identity
           #:make-slack-identity
           #:find-slack-identity
           #:all-slack-identities
           #:add-slack-identity
           #:remove-slack-identity

           #:user-id-of
           #:initial-of)
  (:documentation
   "The Slack bot's graph-db store, at <data-root>/slack-bot/: who has
subscribed to reminder DMs, and which initials each Slack account goes by."))

(in-package #:project-isidore/slack-bot/database)

(defvar *database* nil
  "The open bot store, or NIL before OPEN-DATABASE.")

(defun store-path ()
  "The directory of the bot's store, <data-root>/slack-bot/."
  (uiop:subpathname (store:data-root) "slack-bot/"))

(defun open-database ()
  "Open the bot's store, creating it when it does not exist yet."
  (close-database)
  (setf *database* (store:open-store :slack-bot (store-path))))

(defun close-database ()
  "Close the bot's store if it is open."
  (setf *database* (store:close-store *database*)))

;;; Schema.  SLACK-IDENTITY is deliberately a separate type from
;;; SLACK-SUBSCRIPTION: that one is the opt-in to reminder DMs, and
;;; /unsubscribe must not cost someone their identity.

(graph-db:def-vertex slack-subscription ()
  ((user-id :accessor user-id-of :type string)
   (initial :accessor initial-of :type string))
  :slack-bot)

(graph-db:def-vertex slack-identity ()
  ((user-id :accessor user-id-of :type string)
   (initial :accessor initial-of :type string))
  :slack-bot)

(graph-db:def-view subscription-by-user-ci :lessp (slack-subscription :slack-bot)
  (:map
   (lambda (s)
     (let ((uid (graph-db:node-slot-value s :user-id)))
       (when uid
         (graph-db:yield (string-downcase uid) nil))))))

(graph-db:def-view identity-by-user-ci :lessp (slack-identity :slack-bot)
  (:map
   (lambda (s)
     (let ((uid (graph-db:node-slot-value s :user-id)))
       (when uid
         (graph-db:yield (string-downcase uid) nil))))))

;;; Both registries work alike: one vertex per Slack user, found by a
;;; case-insensitive view on its user id.

(defun find-by-user (type view user-id)
  "The TYPE vertex for USER-ID, looked up through VIEW, or NIL."
  (let* ((rows (graph-db:invoke-graph-view type view
                                           :key (string-downcase user-id)
                                           :graph *database*))
         (id (cdr (assoc :id (first rows)))))
    (when id
      (graph-db:lookup-vertex id :graph *database*))))

(defun all-of-type (type)
  "An alist of (user-id . initial) for every TYPE vertex."
  (let ((acc '()))
    (graph-db:map-vertices (lambda (v) (push (cons (user-id-of v) (initial-of v)) acc))
                           *database*
                           :vertex-type type)
    (nreverse acc)))

(defun upsert (find make user-id initial context)
  "In one transaction, set to INITIAL the initial of the vertex FIND returns
for USER-ID, or MAKE one when there is none.  Returns the persistent vertex."
  (store:with-transaction (*database* :context context)
    (let ((existing (funcall find user-id)))
      (if existing
          (let ((copy (graph-db:copy existing)))
            (setf (initial-of copy) initial)
            (graph-db:save copy)
            copy)
          (funcall make :user-id user-id :initial initial)))))

(defun remove-vertex (find user-id context)
  "In one transaction, delete the vertex FIND returns for USER-ID.  Returns T
when there was one."
  (store:with-transaction (*database* :context context)
    (let ((vertex (funcall find user-id)))
      (when vertex
        (graph-db:mark-deleted vertex)
        t))))

;;; Subscriptions

(defun find-slack-subscription (user-id)
  "The SLACK-SUBSCRIPTION vertex for USER-ID, or NIL."
  (find-by-user 'slack-subscription 'subscription-by-user-ci user-id))

(defun all-slack-subscriptions ()
  "An alist of (user-id . initial) for every subscription."
  (all-of-type 'slack-subscription))

(defun add-slack-subscription (user-id initial)
  "Subscribe USER-ID as INITIAL, or change the initial of their subscription."
  (upsert #'find-slack-subscription #'make-slack-subscription
          user-id initial "add-slack-subscription"))

(defun remove-slack-subscription (user-id)
  "Delete the subscription of USER-ID.  Returns T if there was one."
  (remove-vertex #'find-slack-subscription user-id "remove-slack-subscription"))

;;; Identities

(defun find-slack-identity (user-id)
  "The SLACK-IDENTITY vertex for USER-ID, or NIL."
  (find-by-user 'slack-identity 'identity-by-user-ci user-id))

(defun all-slack-identities ()
  "An alist of (user-id . initial) for every registered identity."
  (all-of-type 'slack-identity))

(defun add-slack-identity (user-id initial)
  "Record that USER-ID goes by INITIAL."
  (upsert #'find-slack-identity #'make-slack-identity
          user-id initial "add-slack-identity"))

(defun remove-slack-identity (user-id)
  "Forget the initials of USER-ID.  Returns T if they were registered."
  (remove-vertex #'find-slack-identity user-id "remove-slack-identity"))
