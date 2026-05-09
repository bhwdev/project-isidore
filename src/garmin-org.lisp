;;;; SPDX-FileCopyrightText: 2026 Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/garmin-org
  (:use #:common-lisp)
  (:import-from #:com.inuoe.jzon)
  (:import-from #:dexador)
  (:import-from #:cl-cookie)
  (:import-from #:cl-base64)
  (:import-from #:cl-ppcre)
  (:import-from #:alexandria
                #:when-let
                #:if-let
                #:hash-table-plist
                #:plist-hash-table)
  (:export
   ;; Client
   #:garmin-client
   #:make-garmin-client
   #:save-tokens
   #:load-tokens
   ;; Conditions
   #:garmin-error
   #:garmin-connection-error
   #:garmin-auth-error
   #:garmin-rate-limit-error
   #:garmin-invalid-file-format-error
   ;; User & Profile
   #:get-full-name
   #:get-unit-system
   #:get-user-profile
   #:get-userprofile-settings
   ;; Daily Health
   #:get-user-summary
   #:get-steps-data
   #:get-floors
   #:get-daily-steps
   #:get-weekly-steps
   #:get-weekly-stress
   #:get-weekly-intensity-minutes
   #:get-heart-rates
   #:get-stats-and-body
   #:get-body-battery
   #:get-body-battery-events
   ;; Sleep, Stress, Wellness
   #:get-sleep-data
   #:get-stress-data
   #:get-all-day-stress
   #:get-all-day-events
   #:get-respiration-data
   #:get-spo2-data
   #:get-intensity-minutes-data
   ;; Advanced Metrics
   #:get-max-metrics
   #:get-rhr-day
   #:get-hrv-data
   #:get-training-readiness
   #:get-endurance-score
   #:get-running-tolerance
   #:get-race-predictions
   #:get-training-status
   #:get-fitnessage-data
   #:get-hill-score
   ;; Body Composition & Weight
   #:get-body-composition
   #:get-weigh-ins
   #:get-daily-weigh-ins
   #:add-weigh-in
   #:delete-weigh-in
   ;; Blood Pressure
   #:get-blood-pressure
   #:set-blood-pressure
   #:delete-blood-pressure
   ;; Hydration & Nutrition
   #:get-hydration-data
   #:add-hydration-data
   #:get-nutrition-daily-food-log
   #:get-nutrition-daily-meals
   #:get-nutrition-daily-settings
   ;; Activities
   #:count-activities
   #:get-activities
   #:get-activities-fordate
   #:get-activities-by-date
   #:get-last-activity
   #:get-activity
   #:get-activity-details
   #:get-activity-splits
   #:get-activity-typed-splits
   #:get-activity-split-summaries
   #:get-activity-weather
   #:get-activity-hr-in-timezones
   #:get-activity-power-in-timezones
   #:get-activity-exercise-sets
   #:get-activity-gear
   #:get-cycling-ftp
   #:set-activity-name
   #:set-activity-type
   #:create-manual-activity
   #:upload-activity
   #:import-activity
   #:download-activity
   #:delete-activity
   ;; Workouts
   #:get-workouts
   #:get-workout-by-id
   #:delete-workout
   #:download-workout
   #:upload-workout
   #:get-scheduled-workouts
   #:get-scheduled-workout-by-id
   #:schedule-workout
   #:unschedule-workout
   ;; Gear
   #:get-gear
   #:get-gear-stats
   #:get-gear-defaults
   #:set-gear-default
   #:get-gear-activities
   #:add-gear-to-activity
   #:remove-gear-from-activity
   ;; Devices
   #:get-devices
   #:get-device-settings
   #:get-primary-training-device
   #:get-device-solar-data
   #:get-device-alarms
   #:get-device-last-used
   ;; Goals, Badges, Challenges
   #:get-personal-record
   #:get-earned-badges
   #:get-available-badges
   #:get-adhoc-challenges
   #:get-badge-challenges
   #:get-available-badge-challenges
   #:get-non-completed-badge-challenges
   #:get-inprogress-virtual-challenges
   #:get-goals
   ;; Progress & Stats
   #:get-progress-summary-between-dates
   #:get-activity-types
   #:request-reload
   ;; Women's Health
   #:get-menstrual-data-for-date
   #:get-menstrual-calendar-data
   #:get-pregnancy-summary
   ;; Training Plans
   #:get-training-plans
   #:get-training-plan-by-id
   #:get-adaptive-training-plan-by-id
   ;; Golf
   #:get-golf-summary
   #:get-golf-scorecard
   #:get-golf-shot-data
   ;; GraphQL
   #:query-garmin-graphql
   ;; Lifestyle
   #:get-lifestyle-logging-data)
  (:documentation
   "Common Lisp client for the Garmin Connect API.

Ported from the Python garminconnect library.

Because authentication requires advanced browser impersonation, please download
and run the Python library
once (https://github.com/cyberjunky/python-garminconnect) to generate tokens in
~/.garminconnect/garmin_tokens.json and then afterwards this library can be
used."))

(in-package #:project-isidore/garmin-org)

;;; ============================================================
;;; Conditions
;;; ============================================================

(define-condition garmin-error (simple-error)
  ((status-code :initarg :status-code :initform nil :reader garmin-error-status-code)
   (url :initarg :url :initform nil :reader garmin-error-url))
  (:report (lambda (c s)
             (format s "Garmin error~@[ (HTTP ~A)~]~@[ for ~A~]: ~?"
                     (garmin-error-status-code c)
                     (garmin-error-url c)
                     (simple-condition-format-control c)
                     (simple-condition-format-arguments c)))))

(define-condition garmin-connection-error (garmin-error) ())
(define-condition garmin-auth-error (garmin-error) ())
(define-condition garmin-rate-limit-error (garmin-error) ())
(define-condition garmin-invalid-file-format-error (garmin-error) ())

;;; ============================================================
;;; Constants
;;; ============================================================

;; DI OAuth
(defparameter *di-token-url*
  "https://diauth.garmin.com/di-oauth2-service/oauth/token")

;; Native API headers
(defparameter *native-user-agent* "GCM-Android-5.23")
(defparameter *native-x-garmin-user-agent*
  "com.garmin.android.apps.connectmobile/5.23; ; Google/sdk_gphone64_arm64/google; Android/33; Dalvik/2.1.0")

;; API Endpoint Paths
(defparameter *url-user-settings*
  "/userprofile-service/userprofile/user-settings")
(defparameter *url-userprofile-settings*
  "/userprofile-service/userprofile/settings")
(defparameter *url-social-profile*
  "/userprofile-service/socialProfile")
(defparameter *url-devices*
  "/device-service/deviceregistration/devices")
(defparameter *url-device*
  "/device-service/deviceservice")
(defparameter *url-primary-device*
  "/web-gateway/device-info/primary-training-device")
(defparameter *url-solar*
  "/web-gateway/solar")
(defparameter *url-weight*
  "/weight-service")
(defparameter *url-daily-summary*
  "/usersummary-service/usersummary/daily")
(defparameter *url-max-metrics*
  "/metrics-service/metrics/maxmet/daily")
(defparameter *url-biometric*
  "/biometric-service/biometric")
(defparameter *url-daily-hydration*
  "/usersummary-service/usersummary/hydration/daily")
(defparameter *url-set-hydration*
  "/usersummary-service/usersummary/hydration/log")
(defparameter *url-daily-stats-steps*
  "/usersummary-service/stats/steps/daily")
(defparameter *url-weekly-stats-steps*
  "/usersummary-service/stats/steps/weekly")
(defparameter *url-weekly-stats-stress*
  "/usersummary-service/stats/stress/weekly")
(defparameter *url-weekly-stats-im*
  "/usersummary-service/stats/im/weekly")
(defparameter *url-personal-record*
  "/personalrecord-service/personalrecord/prs")
(defparameter *url-earned-badges*
  "/badge-service/badge/earned")
(defparameter *url-available-badges*
  "/badge-service/badge/available")
(defparameter *url-adhoc-challenges*
  "/adhocchallenge-service/adHocChallenge/historical")
(defparameter *url-badge-challenges*
  "/badgechallenge-service/badgeChallenge/completed")
(defparameter *url-available-badge-challenges*
  "/badgechallenge-service/badgeChallenge/available")
(defparameter *url-non-completed-badge-challenges*
  "/badgechallenge-service/badgeChallenge/non-completed")
(defparameter *url-inprogress-virtual-challenges*
  "/badgechallenge-service/virtualChallenge/inProgress")
(defparameter *url-daily-sleep*
  "/wellness-service/wellness/dailySleepData")
(defparameter *url-daily-stress*
  "/wellness-service/wellness/dailyStress")
(defparameter *url-hill-score*
  "/metrics-service/metrics/hillscore")
(defparameter *url-daily-body-battery*
  "/wellness-service/wellness/bodyBattery/reports/daily")
(defparameter *url-body-battery-events*
  "/wellness-service/wellness/bodyBattery/events")
(defparameter *url-blood-pressure-range*
  "/bloodpressure-service/bloodpressure/range")
(defparameter *url-blood-pressure*
  "/bloodpressure-service/bloodpressure")
(defparameter *url-endurance-score*
  "/metrics-service/metrics/endurancescore")
(defparameter *url-running-tolerance*
  "/metrics-service/metrics/runningtolerance/stats")
(defparameter *url-menstrual-calendar*
  "/periodichealth-service/menstrualcycle/calendar")
(defparameter *url-menstrual-dayview*
  "/periodichealth-service/menstrualcycle/dayview")
(defparameter *url-pregnancy-snapshot*
  "/periodichealth-service/menstrualcycle/pregnancysnapshot")
(defparameter *url-goals*
  "/goal-service/goal/goals")
(defparameter *url-rhr*
  "/userstats-service/wellness/daily")
(defparameter *url-hrv*
  "/hrv-service/hrv")
(defparameter *url-training-readiness*
  "/metrics-service/metrics/trainingreadiness")
(defparameter *url-race-predictions*
  "/metrics-service/metrics/racepredictions")
(defparameter *url-training-status*
  "/metrics-service/metrics/trainingstatus/aggregated")
(defparameter *url-daily-summary-chart*
  "/wellness-service/wellness/dailySummaryChart")
(defparameter *url-floors-chart-daily*
  "/wellness-service/wellness/floorsChartData/daily")
(defparameter *url-heartrates-daily*
  "/wellness-service/wellness/dailyHeartRate")
(defparameter *url-daily-respiration*
  "/wellness-service/wellness/daily/respiration")
(defparameter *url-daily-spo2*
  "/wellness-service/wellness/daily/spo2")
(defparameter *url-daily-im*
  "/wellness-service/wellness/daily/im")
(defparameter *url-daily-events*
  "/wellness-service/wellness/dailyEvents")
(defparameter *url-activities-search*
  "/activitylist-service/activities/search/activities")
(defparameter *url-activities-count*
  "/activitylist-service/activities/count")
(defparameter *url-activities-base*
  "/activitylist-service/activities/")
(defparameter *url-activity*
  "/activity-service/activity")
(defparameter *url-activity-types*
  "/activity-service/activity/activityTypes")
(defparameter *url-activity-fordate*
  "/mobile-gateway/heartRate/forDate")
(defparameter *url-fitnessstats*
  "/fitnessstats-service/activity")
(defparameter *url-fitnessage*
  "/fitnessage-service/fitnessage")
(defparameter *url-fit-download*
  "/download-service/files/activity")
(defparameter *url-tcx-download*
  "/download-service/export/tcx/activity")
(defparameter *url-gpx-download*
  "/download-service/export/gpx/activity")
(defparameter *url-kml-download*
  "/download-service/export/kml/activity")
(defparameter *url-csv-download*
  "/download-service/export/csv/activity")
(defparameter *url-upload*
  "/upload-service/upload")
(defparameter *url-gear-filter*
  "/gear-service/gear/filterGear")
(defparameter *url-gear-base*
  "/gear-service/gear")
(defparameter *url-request-reload*
  "/wellness-service/wellness/epoch/request")
(defparameter *url-workouts*
  "/workout-service")
(defparameter *url-calendar*
  "/calendar-service")
(defparameter *url-nutrition*
  "/nutrition-service")
(defparameter *url-golf-scorecard-summary*
  "/gcs-golfcommunity/api/v2/scorecard/summary")
(defparameter *url-golf-scorecard-detail*
  "/gcs-golfcommunity/api/v2/scorecard/detail")
(defparameter *url-golf-shot*
  "/gcs-golfcommunity/api/v2/shot/scorecard")
(defparameter *url-graphql*
  "graphql-gateway/graphql")
(defparameter *url-training-plan*
  "/trainingplan-service/trainingplan")
(defparameter *url-daily-lifestyle-logging*
  "/lifestylelogging-service/dailyLog")

;;; ============================================================
;;; Client Class
;;; ============================================================

(defclass garmin-client ()
  ((domain :initarg :domain :initform "garmin.com" :accessor client-domain)
   (di-token :initarg :di-token :initform nil :accessor client-di-token)
   (di-refresh-token :initarg :di-refresh-token :initform nil :accessor client-di-refresh-token)
   (di-client-id :initarg :di-client-id :initform nil :accessor client-di-client-id)
   (display-name :initform nil :accessor client-display-name)
   (full-name :initform nil :accessor client-full-name)
   (unit-system :initform nil :accessor client-unit-system)
   (cookie-jar :initform (cl-cookie:make-cookie-jar) :accessor client-cookie-jar)
   (token-path :initarg :token-path
               :initform (merge-pathnames ".garminconnect/garmin_tokens.json"
                                          (user-homedir-pathname))
               :accessor client-token-path)))

(defun native-headers (&optional extra)
  "Build native API headers alist, optionally merging EXTRA alist."
  (let ((base `(("User-Agent" . ,*native-user-agent*)
                ("X-Garmin-User-Agent" . ,*native-x-garmin-user-agent*)
                ("X-Garmin-Paired-App-Version" . "10861")
                ("X-Garmin-Client-Platform" . "Android")
                ("X-App-Ver" . "10861")
                ("X-Lang" . "en")
                ("X-GCExperience" . "GC5")
                ("Accept-Language" . "en-US,en;q=0.9"))))
    (if extra (append extra base) base)))

(defun build-basic-auth (client-id)
  "Build Basic auth string for a DI client ID."
  (format nil "Basic ~A"
          (cl-base64:string-to-base64-string
           (format nil "~A:" client-id))))

;;; ============================================================
;;; JWT Utilities
;;; ============================================================

(defun decode-jwt-payload (token)
  "Decode the payload portion of a JWT token. Returns a hash-table or NIL."
  (handler-case
      (let* ((parts (cl-ppcre:split "\\." token))
             (payload-b64 (second parts)))
        (when payload-b64
          ;; Add padding
          (let* ((pad (mod (- 4 (mod (length payload-b64) 4)) 4))
                 (padded (if (< pad 4)
                             (concatenate 'string payload-b64
                                          (make-string pad :initial-element #\=))
                             payload-b64))
                 ;; URL-safe base64 to standard
                 (std (substitute #\+ #\- (substitute #\/ #\_ padded)))
                 (decoded (cl-base64:base64-string-to-string std)))
            (com.inuoe.jzon:parse decoded))))
    (error () nil)))

(defun token-expires-soon-p (token &optional (buffer 900))
  "Return T if the JWT TOKEN expires within BUFFER seconds."
  (when-let ((payload (decode-jwt-payload token)))
    (when-let ((exp (gethash "exp" payload)))
      (> (get-universal-time)
         (- (+ exp
               ;; Convert Unix epoch to universal time
               ;; Unix epoch = 1970-01-01, CL epoch = 1900-01-01
               ;; Difference = 2208988800
               2208988800)
            buffer)))))

(defun unix-to-universal (unix-time)
  (+ unix-time 2208988800))

(defun universal-to-unix (universal-time)
  (- universal-time 2208988800))

;;; ============================================================
;;; Token Persistence
;;; ============================================================

(defun save-tokens (client)
  "Save auth tokens to disk as JSON."
  (let ((path (client-token-path client)))
    (ensure-directories-exist path)
    (let ((ht (make-hash-table :test #'equal)))
      (setf (gethash "di_token" ht) (client-di-token client))
      (setf (gethash "di_refresh_token" ht) (client-di-refresh-token client))
      (setf (gethash "di_client_id" ht) (client-di-client-id client))
      (with-open-file (s path :direction :output :if-exists :supersede
                              :external-format :utf-8)
        (com.inuoe.jzon:stringify ht :stream s)))))

(defun load-tokens (client &optional path)
  "Load auth tokens from disk. Returns CLIENT."
  (let ((p (or path (client-token-path client))))
    (unless (probe-file p)
      (error 'garmin-auth-error
             :format-control "Token file not found: ~A"
             :format-arguments (list p)))
    (let ((data (with-open-file (s p :external-format :utf-8)
                  (com.inuoe.jzon:parse s))))
      (setf (client-di-token client) (gethash "di_token" data)
            (client-di-refresh-token client) (gethash "di_refresh_token" data)
            (client-di-client-id client) (gethash "di_client_id" data))
      (unless (client-di-token client)
        (error 'garmin-auth-error
               :format-control "No di_token found in ~A"
               :format-arguments (list p)))
      client)))

;;; ============================================================
;;; Token Refresh
;;; ============================================================

(defun refresh-di-token (client)
  "Refresh the DI Bearer token using the stored refresh token."
  (unless (and (client-di-refresh-token client)
               (client-di-client-id client))
    (error 'garmin-auth-error
           :format-control "No refresh token or client ID available"))
  (multiple-value-bind (body status)
      (dex:request *di-token-url*
                   :method :post
                   :headers (native-headers
                             `(("Authorization" . ,(build-basic-auth
                                                    (client-di-client-id client)))
                               ("Accept" . "application/json")
                               ("Content-Type" . "application/x-www-form-urlencoded")
                               ("Cache-Control" . "no-cache")))
                   :content `(("grant_type" . "refresh_token")
                              ("client_id" . ,(client-di-client-id client))
                              ("refresh_token" . ,(client-di-refresh-token client)))
                   :read-timeout 30
                   :connect-timeout 30)
    (unless (and (<= 200 status) (< status 300))
      (error 'garmin-auth-error
             :format-control "DI token refresh failed: HTTP ~A"
             :format-arguments (list status)))
    (let ((data (com.inuoe.jzon:parse body)))
      (setf (client-di-token client) (gethash "access_token" data))
      (when-let ((rt (gethash "refresh_token" data)))
        (setf (client-di-refresh-token client) rt))
      ;; Try to extract client_id from new JWT
      (when-let ((payload (decode-jwt-payload (client-di-token client))))
        (when-let ((cid (gethash "client_id" payload)))
          (setf (client-di-client-id client) cid)))
      ;; Persist refreshed tokens
      (handler-case (save-tokens client)
        (error () nil))
      client)))

(defun ensure-token-fresh (client)
  "Refresh the token if it's expiring soon."
  (when (and (client-di-token client)
             (token-expires-soon-p (client-di-token client)))
    (refresh-di-token client)))

(defun human-delay (&optional (min-seconds 0.5) (max-seconds 2.5))
  "Sleep for a random duration to simulate human interaction timing.
Uses a right-skewed distribution: most delays cluster near MIN-SECONDS,
with occasional longer pauses — matching how humans mostly act quickly
but sometimes pause to read or think."
  (let* ((r (random 1.0))
         (skewed (* r r))
         (delay (+ min-seconds (* skewed (- max-seconds min-seconds)))))
    (sleep delay)))

(defun human-page-delay ()
  "Longer delay between paginated requests — simulates a human scrolling
through a page of results before requesting the next batch."
  (human-delay 1.5 4.0))

;;; ============================================================
;;; HTTP Request Layer
;;; ============================================================

(defun api-headers (client)
  "Build API request headers with Bearer token."
  (native-headers
   `(("Authorization" . ,(format nil "Bearer ~A" (client-di-token client)))
     ("Accept" . "application/json"))))

(defun build-url (client path &optional params)
  "Build full API URL from path and optional query parameters."
  (let ((base (format nil "https://connectapi.~A/~A"
                      (client-domain client)
                      (string-left-trim "/" path))))
    (if params
        (format nil "~A?~{~A=~A~^&~}"
                base
                (loop for (k . v) in params
                      collect (quri:url-encode (princ-to-string k))
                      collect (quri:url-encode (princ-to-string v))))
        base)))

(defun garmin-request (client path &key (method :get) params content headers
                                     json force-binary)
  "Send an HTTP request to the Garmin Connect API.
Returns parsed JSON (hash-table/vector) or raw octets if FORCE-BINARY."
  (human-delay)
  (ensure-token-fresh client)
  (let* ((url (build-url client path params))
         (all-headers (append (or headers '()) (api-headers client)))
         (body-content (cond
                         (json
                          (push '("Content-Type" . "application/json") all-headers)
                          (com.inuoe.jzon:stringify json))
                         (content content))))
    (flet ((do-request ()
             (dex:request url
                          :method method
                          :headers all-headers
                          :content body-content
                          :cookie-jar (client-cookie-jar client)
                          :read-timeout 30
                          :connect-timeout 30
                          :force-binary force-binary
                          :keep-alive nil)))
      (multiple-value-bind (body status resp-headers)
          (handler-case (do-request)
            (dex:http-request-failed (e)
              (let ((s (dex:response-status e))
                    (b (dex:response-body e)))
                (cond
                  ((= s 401)
                   ;; Try refresh and retry once
                   (refresh-di-token client)
                   (setf all-headers (append (or headers '()) (api-headers client)))
                   (when json
                     (push '("Content-Type" . "application/json") all-headers))
                   (do-request))
                  ((= s 429)
                   (error 'garmin-rate-limit-error
                          :status-code s :url url
                          :format-control "Rate limited: ~A"
                          :format-arguments (list (or b ""))))
                  ((= s 204)
                   (return-from garmin-request (make-hash-table :test #'equal)))
                  (t
                   (error 'garmin-connection-error
                          :status-code s :url url
                          :format-control "HTTP ~A: ~A"
                          :format-arguments (list s (or b ""))))))))
        (declare (ignore resp-headers))
        (cond
          ((= status 204)
           (make-hash-table :test #'equal))
          (force-binary body)
          ((and (stringp body) (> (length body) 0))
           (com.inuoe.jzon:parse body))
          ((and (typep body '(vector (unsigned-byte 8))) (> (length body) 0))
           (com.inuoe.jzon:parse (babel:octets-to-string body :encoding :utf-8)))
          (t (make-hash-table :test #'equal)))))))

(defun garmin-download (client path &key params headers)
  "Download binary content from the API."
  (garmin-request client path :params params :headers headers :force-binary t))

(defun garmin-post (client path &key params json content headers)
  "POST to the API, return parsed JSON."
  (garmin-request client path :method :post :params params
                              :json json :content content :headers headers))

(defun garmin-put (client path &key params json content headers)
  "PUT to the API, return parsed JSON."
  (garmin-request client path :method :put :params params
                              :json json :content content :headers headers))

(defun garmin-delete (client path &key params headers)
  "DELETE from the API, return parsed JSON."
  (garmin-request client path :method :delete :params params :headers headers))

;;; ============================================================
;;; Client Constructor
;;; ============================================================

(defun make-garmin-client (&key (token-path nil token-path-p)
                             (domain "garmin.com"))
  "Create a Garmin client, loading tokens from disk.
TOKEN-PATH defaults to ~/.garminconnect/garmin_tokens.json."
  (let ((client (make-instance 'garmin-client :domain domain)))
    (when token-path-p
      (setf (client-token-path client) token-path))
    (load-tokens client)
    ;; Refresh if token is expiring
    (when (token-expires-soon-p (client-di-token client))
      (refresh-di-token client))
    ;; Load profile
    (handler-case
        (let ((prof (garmin-request client *url-social-profile*)))
          (when (hash-table-p prof)
            (setf (client-display-name client) (gethash "displayName" prof)
                  (client-full-name client) (gethash "fullName" prof))))
      (error (e) (warn "Failed to load profile: ~A" e)))
    ;; Brief pause before loading settings — avoid back-to-back burst
    (human-delay 1.0 3.0)
    ;; Load settings
    (handler-case
        (let ((settings (garmin-request client *url-user-settings*)))
          (when (hash-table-p settings)
            (when-let ((ud (gethash "userData" settings)))
              (setf (client-unit-system client)
                    (gethash "measurementSystem" ud)))))
      (error (e) (warn "Failed to load settings: ~A" e)))
    client))

(defun require-display-name (client)
  (or (client-display-name client)
      (error 'garmin-connection-error
             :format-control "Display name not set. Set one at connect.garmin.com.")))

;;; ============================================================
;;; Date Utilities
;;; ============================================================

(defun validate-date (date-str &optional (name "date"))
  "Validate that DATE-STR matches YYYY-MM-DD format. Returns the string."
  (unless (and (stringp date-str)
               (= (length date-str) 10)
               (cl-ppcre:scan "^\\d{4}-\\d{2}-\\d{2}$" date-str))
    (error "~A must be in YYYY-MM-DD format, got: ~S" name date-str))
  date-str)

(defun parse-date (date-str)
  "Parse YYYY-MM-DD to (values year month day)."
  (values (parse-integer (subseq date-str 0 4))
          (parse-integer (subseq date-str 5 7))
          (parse-integer (subseq date-str 8 10))))

(defun date-to-days (date-str)
  "Convert YYYY-MM-DD to an approximate day number for range math."
  (multiple-value-bind (y m d) (parse-date date-str)
    (+ (* y 365) (* m 30) d)))

(defun add-days (date-str n)
  "Add N days to a YYYY-MM-DD date string."
  (multiple-value-bind (y m d) (parse-date date-str)
    (let* ((encoded (encode-universal-time 0 0 12 d m y))
           (new (+ encoded (* n 86400))))
      (multiple-value-bind (sec min hr day month year)
          (decode-universal-time new)
        (declare (ignore sec min hr))
        (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))))

;;; ============================================================
;;; User & Profile
;;; ============================================================

(defun get-full-name (client)
  (client-full-name client))

(defun get-unit-system (client)
  (client-unit-system client))

(defun get-user-profile (client)
  (garmin-request client *url-user-settings*))

(defun get-userprofile-settings (client)
  (garmin-request client *url-userprofile-settings*))

;;; ============================================================
;;; Daily Health Summaries
;;; ============================================================

(defun get-user-summary (client cdate)
  "Return user activity summary for CDATE (YYYY-MM-DD)."
  (validate-date cdate "cdate")
  (garmin-request client
                  (format nil "~A/~A" *url-daily-summary*
                          (require-display-name client))
                  :params `(("calendarDate" . ,cdate))))

(defun get-steps-data (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client
                  (format nil "~A/~A" *url-daily-summary-chart*
                          (require-display-name client))
                  :params `(("date" . ,cdate))))

(defun get-floors (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/~A" *url-floors-chart-daily* cdate)))

(defun get-daily-steps (client start end)
  "Fetch daily steps with automatic 28-day chunking."
  (validate-date start "start")
  (validate-date end "end")
  (let ((results #())
        (current start))
    (loop while (string<= current end)
          do (human-page-delay)
             (let* ((chunk-end (let ((ce (add-days current 27)))
                                 (if (string< ce end) ce end)))
                    (url (format nil "~A/~A/~A" *url-daily-stats-steps*
                                 current chunk-end))
                    (chunk (garmin-request client url)))
               (when (and chunk (typep chunk 'vector))
                 (setf results (concatenate 'vector results chunk)))
               (setf current (add-days chunk-end 1))))
    results))

(defun get-weekly-steps (client end &optional (weeks 52))
  (validate-date end "end")
  (garmin-request client (format nil "~A/~A/~A" *url-weekly-stats-steps* end weeks)))

(defun get-weekly-stress (client end &optional (weeks 52))
  (validate-date end "end")
  (garmin-request client (format nil "~A/~A/~A" *url-weekly-stats-stress* end weeks)))

(defun get-weekly-intensity-minutes (client start end)
  (validate-date start "start")
  (validate-date end "end")
  (garmin-request client (format nil "~A/~A/~A" *url-weekly-stats-im* start end)))

(defun get-heart-rates (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client
                  (format nil "~A/~A" *url-heartrates-daily*
                          (client-display-name client))
                  :params `(("date" . ,cdate))))

(defun get-stats-and-body (client cdate)
  "Return activity data and body composition combined."
  (validate-date cdate "cdate")
  (let ((stats (get-user-summary client cdate))
        (body (get-body-composition client cdate)))
    (when-let ((avg (and (hash-table-p body)
                         (gethash "totalAverage" body))))
      (when (hash-table-p avg)
        (maphash (lambda (k v) (setf (gethash k stats) v)) avg)))
    stats))

(defun get-body-battery (client startdate &optional enddate)
  (validate-date startdate "startdate")
  (let ((ed (or enddate startdate)))
    (when enddate (validate-date enddate "enddate"))
    (garmin-request client *url-daily-body-battery*
                    :params `(("startDate" . ,startdate) ("endDate" . ,ed)))))

(defun get-body-battery-events (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/~A" *url-body-battery-events* cdate)))

;;; ============================================================
;;; Sleep, Stress, Wellness
;;; ============================================================

(defun get-sleep-data (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client
                  (format nil "~A/~A" *url-daily-sleep*
                          (require-display-name client))
                  :params `(("date" . ,cdate)
                            ("nonSleepBufferMinutes" . "60"))))

(defun get-stress-data (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/~A" *url-daily-stress* cdate)))

(defun get-all-day-stress (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/~A" *url-daily-stress* cdate)))

(defun get-all-day-events (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client *url-daily-events*
                  :params `(("calendarDate" . ,cdate))))

(defun get-respiration-data (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/~A" *url-daily-respiration* cdate)))

(defun get-spo2-data (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/~A" *url-daily-spo2* cdate)))

(defun get-intensity-minutes-data (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/~A" *url-daily-im* cdate)))

;;; ============================================================
;;; Advanced Metrics
;;; ============================================================

(defun get-max-metrics (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/~A/~A" *url-max-metrics* cdate cdate)))

(defun get-rhr-day (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client
                  (format nil "~A/~A" *url-rhr* (require-display-name client))
                  :params `(("fromDate" . ,cdate)
                            ("untilDate" . ,cdate)
                            ("metricId" . "60"))))

(defun get-hrv-data (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/~A" *url-hrv* cdate)))

(defun get-training-readiness (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/~A" *url-training-readiness* cdate)))

(defun get-endurance-score (client startdate &optional enddate)
  (validate-date startdate "startdate")
  (if enddate
      (progn
        (validate-date enddate "enddate")
        (garmin-request client (format nil "~A/stats" *url-endurance-score*)
                        :params `(("startDate" . ,startdate)
                                  ("endDate" . ,enddate)
                                  ("aggregation" . "weekly"))))
      (garmin-request client *url-endurance-score*
                      :params `(("calendarDate" . ,startdate)))))

(defun get-running-tolerance (client startdate enddate &optional (aggregation "weekly"))
  (validate-date startdate "startdate")
  (validate-date enddate "enddate")
  (garmin-request client *url-running-tolerance*
                  :params `(("startDate" . ,startdate)
                            ("endDate" . ,enddate)
                            ("aggregation" . ,aggregation))))

(defun get-race-predictions (client &key startdate enddate type)
  "Return race predictions. Call with no args for latest, or with all three for range."
  (cond
    ((and (null startdate) (null enddate) (null type))
     (garmin-request client
                     (format nil "~A/latest/~A" *url-race-predictions*
                             (require-display-name client))))
    ((and startdate enddate type)
     (validate-date startdate "startdate")
     (validate-date enddate "enddate")
     (garmin-request client
                     (format nil "~A/~A/~A" *url-race-predictions*
                             type (require-display-name client))
                     :params `(("fromCalendarDate" . ,startdate)
                               ("toCalendarDate" . ,enddate))))
    (t (error "Provide all of :startdate, :enddate, :type or none."))))

(defun get-training-status (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/~A" *url-training-status* cdate)))

(defun get-fitnessage-data (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/~A" *url-fitnessage* cdate)))

(defun get-hill-score (client startdate &optional enddate)
  (validate-date startdate "startdate")
  (if enddate
      (progn
        (validate-date enddate "enddate")
        (garmin-request client (format nil "~A/stats" *url-hill-score*)
                        :params `(("startDate" . ,startdate)
                                  ("endDate" . ,enddate)
                                  ("aggregation" . "daily"))))
      (garmin-request client *url-hill-score*
                      :params `(("calendarDate" . ,startdate)))))

;;; ============================================================
;;; Body Composition & Weight
;;; ============================================================

(defun get-body-composition (client startdate &optional enddate)
  (validate-date startdate "startdate")
  (let ((ed (or enddate startdate)))
    (when enddate (validate-date enddate "enddate"))
    (garmin-request client (format nil "~A/weight/dateRange" *url-weight*)
                    :params `(("startDate" . ,startdate) ("endDate" . ,ed)))))

(defun get-weigh-ins (client startdate enddate)
  (validate-date startdate "startdate")
  (validate-date enddate "enddate")
  (garmin-request client
                  (format nil "~A/weight/range/~A/~A" *url-weight* startdate enddate)
                  :params '(("includeAll" . "true"))))

(defun get-daily-weigh-ins (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client
                  (format nil "~A/weight/dayview/~A" *url-weight* cdate)
                  :params '(("includeAll" . "true"))))

(defun add-weigh-in (client weight &key (unit-key "kg") timestamp)
  "Add a weigh-in. WEIGHT is numeric, UNIT-KEY is \"kg\" or \"lbs\"."
  (let* ((now (get-universal-time))
         (ts (or timestamp now)))
    (declare (ignore ts))
    (multiple-value-bind (sec min hr day month year)
        (decode-universal-time (or (and timestamp
                                        (error "Timestamp parsing not yet implemented"))
                                   (get-universal-time)))
      (let ((local-str (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D.000"
                               year month day hr min sec))
            (payload (make-hash-table :test #'equal)))
        (setf (gethash "dateTimestamp" payload) local-str
              (gethash "gmtTimestamp" payload) local-str
              (gethash "unitKey" payload) unit-key
              (gethash "sourceType" payload) "MANUAL"
              (gethash "value" payload) weight)
        (garmin-post client (format nil "~A/user-weight" *url-weight*)
                     :json payload)))))

(defun delete-weigh-in (client weight-pk cdate)
  (validate-date cdate "cdate")
  (garmin-delete client
                 (format nil "~A/weight/~A/byversion/~A" *url-weight* cdate weight-pk)))

;;; ============================================================
;;; Blood Pressure
;;; ============================================================

(defun get-blood-pressure (client startdate &optional enddate)
  (validate-date startdate "startdate")
  (let ((ed (or enddate startdate)))
    (when enddate (validate-date enddate "enddate"))
    (garmin-request client
                    (format nil "~A/~A/~A" *url-blood-pressure-range* startdate ed)
                    :params '(("includeAll" . "true")))))

(defun set-blood-pressure (client systolic diastolic pulse &key notes)
  (multiple-value-bind (sec min hr day month year)
      (decode-universal-time (get-universal-time))
    (let ((local-str (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D.000"
                             year month day hr min sec))
          (payload (make-hash-table :test #'equal)))
      (setf (gethash "measurementTimestampLocal" payload) local-str
            (gethash "measurementTimestampGMT" payload) local-str
            (gethash "systolic" payload) systolic
            (gethash "diastolic" payload) diastolic
            (gethash "pulse" payload) pulse
            (gethash "sourceType" payload) "MANUAL"
            (gethash "notes" payload) (or notes ""))
      (garmin-post client *url-blood-pressure* :json payload))))

(defun delete-blood-pressure (client version cdate)
  (validate-date cdate "cdate")
  (garmin-delete client (format nil "~A/~A/~A" *url-blood-pressure* cdate version)))

;;; ============================================================
;;; Hydration & Nutrition
;;; ============================================================

(defun get-hydration-data (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/~A" *url-daily-hydration* cdate)))

(defun add-hydration-data (client value-in-ml cdate)
  "Add hydration data in ml for CDATE."
  (validate-date cdate "cdate")
  (let ((payload (make-hash-table :test #'equal)))
    (setf (gethash "calendarDate" payload) cdate
          (gethash "timestampLocal" payload)
          (format nil "~AT00:00:00.000" cdate)
          (gethash "valueInML" payload) value-in-ml)
    (garmin-put client *url-set-hydration* :json payload)))

(defun get-nutrition-daily-food-log (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/food/logs/~A" *url-nutrition* cdate)))

(defun get-nutrition-daily-meals (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/meals/~A" *url-nutrition* cdate)))

(defun get-nutrition-daily-settings (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/settings/~A" *url-nutrition* cdate)))

;;; ============================================================
;;; Activities
;;; ============================================================

(defun count-activities (client)
  (let ((resp (garmin-request client *url-activities-count*)))
    (if (hash-table-p resp)
        (or (gethash "totalCount" resp) 0)
        0)))

(defun get-activities (client &key (start 0) (limit 20) activitytype)
  (let ((params `(("start" . ,(princ-to-string start))
                  ("limit" . ,(princ-to-string limit)))))
    (when activitytype
      (push `("activityType" . ,activitytype) params))
    (garmin-request client *url-activities-search* :params params)))

(defun get-activities-fordate (client fordate)
  (validate-date fordate "fordate")
  (garmin-request client (format nil "~A/~A" *url-activity-fordate* fordate)))

(defun get-activities-by-date (client startdate &key enddate activitytype sortorder)
  "Fetch all activities between dates with automatic pagination."
  (validate-date startdate "startdate")
  (when enddate (validate-date enddate "enddate"))
  (let ((all-activities #())
        (start 0)
        (limit 20))
    (loop
      (human-page-delay)
      (let ((params `(("startDate" . ,startdate)
                      ("start" . ,(princ-to-string start))
                      ("limit" . ,(princ-to-string limit)))))
        (when enddate (push `("endDate" . ,enddate) params))
        (when activitytype (push `("activityType" . ,activitytype) params))
        (when sortorder (push `("sortOrder" . ,sortorder) params))
        (let ((batch (garmin-request client *url-activities-search* :params params)))
          (if (and batch (typep batch 'vector) (> (length batch) 0))
              (progn
                (setf all-activities (concatenate 'vector all-activities batch))
                (incf start limit))
              (return all-activities)))))))

(defun get-last-activity (client)
  (let ((activities (get-activities client :start 0 :limit 1)))
    (when (and (typep activities 'vector) (> (length activities) 0))
      (aref activities (1- (length activities))))))

(defun get-activity (client activity-id)
  (garmin-request client (format nil "~A/~A" *url-activity* activity-id)))

(defun get-activity-details (client activity-id &key (maxchart 2000) (maxpoly 4000))
  (garmin-request client (format nil "~A/~A/details" *url-activity* activity-id)
                  :params `(("maxChartSize" . ,(princ-to-string maxchart))
                            ("maxPolylineSize" . ,(princ-to-string maxpoly)))))

(defun get-activity-splits (client activity-id)
  (garmin-request client (format nil "~A/~A/splits" *url-activity* activity-id)))

(defun get-activity-typed-splits (client activity-id)
  (garmin-request client (format nil "~A/~A/typedsplits" *url-activity* activity-id)))

(defun get-activity-split-summaries (client activity-id)
  (garmin-request client (format nil "~A/~A/split_summaries" *url-activity* activity-id)))

(defun get-activity-weather (client activity-id)
  (garmin-request client (format nil "~A/~A/weather" *url-activity* activity-id)))

(defun get-activity-hr-in-timezones (client activity-id)
  (garmin-request client (format nil "~A/~A/hrTimeInZones" *url-activity* activity-id)))

(defun get-activity-power-in-timezones (client activity-id)
  (garmin-request client (format nil "~A/~A/powerTimeInZones" *url-activity* activity-id)))

(defun get-activity-exercise-sets (client activity-id)
  (garmin-request client (format nil "~A/~A/exerciseSets" *url-activity* activity-id)))

(defun get-activity-gear (client activity-id)
  (garmin-request client *url-gear-filter*
                  :params `(("activityId" . ,(princ-to-string activity-id)))))

(defun get-cycling-ftp (client)
  (garmin-request client
                  (format nil "~A/latestFunctionalThresholdPower/CYCLING" *url-biometric*)))

(defun set-activity-name (client activity-id title)
  (let ((payload (make-hash-table :test #'equal)))
    (setf (gethash "activityId" payload) activity-id
          (gethash "activityName" payload) title)
    (garmin-put client (format nil "~A/~A" *url-activity* activity-id)
                :json payload)))

(defun set-activity-type (client activity-id type-id type-key parent-type-id)
  (let ((payload (make-hash-table :test #'equal))
        (type-dto (make-hash-table :test #'equal)))
    (setf (gethash "typeId" type-dto) type-id
          (gethash "typeKey" type-dto) type-key
          (gethash "parentTypeId" type-dto) parent-type-id)
    (setf (gethash "activityId" payload) activity-id
          (gethash "activityTypeDTO" payload) type-dto)
    (garmin-put client (format nil "~A/~A" *url-activity* activity-id)
                :json payload)))

(defun create-manual-activity (client &key type-key start-datetime time-zone
                                        distance-km duration-min activity-name)
  "Create a manual activity."
  (let ((payload (make-hash-table :test #'equal))
        (activity-type-dto (make-hash-table :test #'equal))
        (access-dto (make-hash-table :test #'equal))
        (tz-dto (make-hash-table :test #'equal))
        (meta-dto (make-hash-table :test #'equal))
        (summary-dto (make-hash-table :test #'equal)))
    (setf (gethash "typeKey" activity-type-dto) type-key)
    (setf (gethash "typeId" access-dto) 2
          (gethash "typeKey" access-dto) "private")
    (setf (gethash "unitKey" tz-dto) time-zone)
    (setf (gethash "autoCalcCalories" meta-dto) t)
    (setf (gethash "startTimeLocal" summary-dto) start-datetime
          (gethash "distance" summary-dto) (* distance-km 1000)
          (gethash "duration" summary-dto) (* duration-min 60))
    (setf (gethash "activityTypeDTO" payload) activity-type-dto
          (gethash "accessControlRuleDTO" payload) access-dto
          (gethash "timeZoneUnitDTO" payload) tz-dto
          (gethash "activityName" payload) activity-name
          (gethash "metadataDTO" payload) meta-dto
          (gethash "summaryDTO" payload) summary-dto)
    (garmin-post client *url-activity* :json payload)))

(defun upload-activity (client activity-path)
  "Upload an activity file (FIT, GPX, TCX)."
  (unless (probe-file activity-path)
    (error "File not found: ~A" activity-path))
  (let* ((name (file-namestring activity-path))
         (ext (string-upcase (pathname-type activity-path))))
    (unless (member ext '("FIT" "GPX" "TCX") :test #'string=)
      (error 'garmin-invalid-file-format-error
             :format-control "Invalid format: ~A. Allowed: FIT, GPX, TCX"
             :format-arguments (list ext)))
    (let ((file-bytes (alexandria:read-file-into-byte-vector activity-path)))
      (ensure-token-fresh client)
      (multiple-value-bind (body status)
          (dex:request (build-url client *url-upload*)
                       :method :post
                       :headers (api-headers client)
                       :content `(("file" . ,(cons name file-bytes)))
                       :cookie-jar (client-cookie-jar client)
                       :read-timeout 60
                       :connect-timeout 30)
        (when (>= status 400)
          (error 'garmin-connection-error
                 :status-code status
                 :format-control "Upload failed: ~A"
                 :format-arguments (list body)))
        (if (stringp body) (com.inuoe.jzon:parse body) body)))))

(defun import-activity (client activity-path)
  "Import an activity file (treated as import, not device sync)."
  (upload-activity client activity-path))

(defun download-activity (client activity-id &key (format :tcx))
  "Download activity as bytes. FORMAT is :original, :tcx, :gpx, :kml, or :csv."
  (let ((url (ecase format
               (:original (format nil "~A/~A" *url-fit-download* activity-id))
               (:tcx (format nil "~A/~A" *url-tcx-download* activity-id))
               (:gpx (format nil "~A/~A" *url-gpx-download* activity-id))
               (:kml (format nil "~A/~A" *url-kml-download* activity-id))
               (:csv (format nil "~A/~A" *url-csv-download* activity-id)))))
    (garmin-download client url)))

(defun delete-activity (client activity-id)
  (garmin-delete client (format nil "~A/~A" *url-activity* activity-id)))

;;; ============================================================
;;; Workouts
;;; ============================================================

(defun get-workouts (client &key (start 0) (limit 100))
  (garmin-request client (format nil "~A/workouts" *url-workouts*)
                  :params `(("start" . ,(princ-to-string start))
                            ("limit" . ,(princ-to-string limit)))))

(defun get-workout-by-id (client workout-id)
  (garmin-request client (format nil "~A/workout/~A" *url-workouts* workout-id)))

(defun delete-workout (client workout-id)
  (garmin-delete client (format nil "~A/workout/~A" *url-workouts* workout-id)))

(defun download-workout (client workout-id)
  (garmin-download client (format nil "~A/workout/FIT/~A" *url-workouts* workout-id)))

(defun upload-workout (client workout-json)
  "Upload a workout from a hash-table or JSON string."
  (let ((payload (etypecase workout-json
                   (hash-table workout-json)
                   (string (com.inuoe.jzon:parse workout-json)))))
    (garmin-post client (format nil "~A/workout" *url-workouts*)
                 :json payload)))

(defun get-scheduled-workouts (client year month)
  "Return scheduled workouts. MONTH is 1-indexed."
  (garmin-request client
                  (format nil "~A/year/~A/month/~A" *url-calendar* year (1- month))))

(defun get-scheduled-workout-by-id (client scheduled-workout-id)
  (garmin-request client
                  (format nil "~A/schedule/~A" *url-workouts* scheduled-workout-id)))

(defun schedule-workout (client workout-id date-str)
  (validate-date date-str "date_str")
  (let ((payload (make-hash-table :test #'equal)))
    (setf (gethash "date" payload) date-str)
    (garmin-post client (format nil "~A/schedule/~A" *url-workouts* workout-id)
                 :json payload)))

(defun unschedule-workout (client scheduled-workout-id)
  (garmin-delete client
                 (format nil "~A/schedule/~A" *url-workouts* scheduled-workout-id)))

;;; ============================================================
;;; Gear
;;; ============================================================

(defun get-gear (client user-profile-number)
  (garmin-request client *url-gear-filter*
                  :params `(("userProfilePk" . ,(princ-to-string user-profile-number)))))

(defun get-gear-stats (client gear-uuid)
  (garmin-request client (format nil "~A/stats/~A" *url-gear-base* gear-uuid)))

(defun get-gear-defaults (client user-profile-number)
  (garmin-request client
                  (format nil "~A/user/~A/activityTypes"
                          *url-gear-base* user-profile-number)))

(defun set-gear-default (client activity-type gear-uuid &key (default-gear t))
  (if default-gear
      (garmin-put client
                  (format nil "~A/~A/activityType/~A/default/true"
                          *url-gear-base* gear-uuid activity-type))
      (garmin-delete client
                     (format nil "~A/~A/activityType/~A"
                             *url-gear-base* gear-uuid activity-type))))

(defun get-gear-activities (client gear-uuid &key (limit 1000))
  (garmin-request client
                  (format nil "~A~A/gear?start=0&limit=~A"
                          *url-activities-base* gear-uuid limit)))

(defun add-gear-to-activity (client gear-uuid activity-id)
  (garmin-put client
              (format nil "~A/link/~A/activity/~A"
                      *url-gear-base* gear-uuid activity-id)))

(defun remove-gear-from-activity (client gear-uuid activity-id)
  (garmin-put client
              (format nil "~A/unlink/~A/activity/~A"
                      *url-gear-base* gear-uuid activity-id)))

;;; ============================================================
;;; Devices
;;; ============================================================

(defun get-devices (client)
  (garmin-request client *url-devices*))

(defun get-device-settings (client device-id)
  (garmin-request client
                  (format nil "~A/device-info/settings/~A" *url-device* device-id)))

(defun get-primary-training-device (client)
  (garmin-request client *url-primary-device*))

(defun get-device-solar-data (client device-id startdate &optional enddate)
  (validate-date startdate "startdate")
  (let* ((ed (or enddate startdate))
         (single (null enddate)))
    (when enddate (validate-date enddate "enddate"))
    (garmin-request client
                    (format nil "~A/~A/~A/~A" *url-solar* device-id startdate ed)
                    :params `(("singleDayView" . ,(if single "true" "false"))))))

(defun get-device-alarms (client)
  "Get alarms from all devices."
  (let ((alarms '()))
    (let ((devices (get-devices client)))
      (when (typep devices 'vector)
        (loop for dev across devices
              when (hash-table-p dev)
                do (handler-case
                       (let* ((settings (get-device-settings client
                                                             (gethash "deviceId" dev)))
                              (dev-alarms (and (hash-table-p settings)
                                               (gethash "alarms" settings))))
                         (when (typep dev-alarms 'vector)
                           (loop for a across dev-alarms do (push a alarms))))
                     (error () nil)))))
    (coerce (nreverse alarms) 'vector)))

(defun get-device-last-used (client)
  (garmin-request client (format nil "~A/mylastused" *url-device*)))

;;; ============================================================
;;; Goals, Badges, Challenges
;;; ============================================================

(defun get-personal-record (client)
  (garmin-request client
                  (format nil "~A/~A" *url-personal-record*
                          (client-display-name client))))

(defun get-earned-badges (client)
  (garmin-request client *url-earned-badges*))

(defun get-available-badges (client)
  (garmin-request client *url-available-badges*
                  :params '(("showExclusiveBadge" . "true"))))

(defun get-adhoc-challenges (client start limit)
  (garmin-request client *url-adhoc-challenges*
                  :params `(("start" . ,(princ-to-string start))
                            ("limit" . ,(princ-to-string limit)))))

(defun get-badge-challenges (client start limit)
  (garmin-request client *url-badge-challenges*
                  :params `(("start" . ,(princ-to-string start))
                            ("limit" . ,(princ-to-string limit)))))

(defun get-available-badge-challenges (client start limit)
  (garmin-request client *url-available-badge-challenges*
                  :params `(("start" . ,(princ-to-string start))
                            ("limit" . ,(princ-to-string limit)))))

(defun get-non-completed-badge-challenges (client start limit)
  (garmin-request client *url-non-completed-badge-challenges*
                  :params `(("start" . ,(princ-to-string start))
                            ("limit" . ,(princ-to-string limit)))))

(defun get-inprogress-virtual-challenges (client start limit)
  (garmin-request client *url-inprogress-virtual-challenges*
                  :params `(("start" . ,(princ-to-string start))
                            ("limit" . ,(princ-to-string limit)))))

(defun get-goals (client &key (status "active") (start 0) (limit 30))
  "Fetch goals. STATUS is \"active\", \"future\", or \"past\"."
  (let ((all #()))
    (loop for offset from start by limit
          do (human-page-delay)
             (let ((batch (garmin-request client *url-goals*
                                          :params `(("status" . ,status)
                                                    ("start" . ,(princ-to-string offset))
                                                    ("limit" . ,(princ-to-string limit))
                                                    ("sortOrder" . "asc")))))
               (if (and batch (typep batch 'vector) (> (length batch) 0))
                   (setf all (concatenate 'vector all batch))
                   (return all))))))

;;; ============================================================
;;; Progress & Stats
;;; ============================================================

(defun get-progress-summary-between-dates (client startdate enddate
                                           &key (metric "distance")
                                             (group-by-activities t))
  (validate-date startdate "startdate")
  (validate-date enddate "enddate")
  (garmin-request client *url-fitnessstats*
                  :params `(("startDate" . ,startdate)
                            ("endDate" . ,enddate)
                            ("aggregation" . "lifetime")
                            ("groupByParentActivityType" . ,(if group-by-activities "true" "false"))
                            ("metric" . ,metric))))

(defun get-activity-types (client)
  (garmin-request client *url-activity-types*))

(defun request-reload (client cdate)
  (validate-date cdate "cdate")
  (garmin-post client (format nil "~A/~A" *url-request-reload* cdate)))

;;; ============================================================
;;; Women's Health
;;; ============================================================

(defun get-menstrual-data-for-date (client fordate)
  (validate-date fordate "fordate")
  (garmin-request client (format nil "~A/~A" *url-menstrual-dayview* fordate)))

(defun get-menstrual-calendar-data (client startdate enddate)
  (validate-date startdate "startdate")
  (validate-date enddate "enddate")
  (garmin-request client
                  (format nil "~A/~A/~A" *url-menstrual-calendar* startdate enddate)))

(defun get-pregnancy-summary (client)
  (garmin-request client *url-pregnancy-snapshot*))

;;; ============================================================
;;; Training Plans
;;; ============================================================

(defun get-training-plans (client)
  (garmin-request client (format nil "~A/plans" *url-training-plan*)))

(defun get-training-plan-by-id (client plan-id)
  (garmin-request client (format nil "~A/phased/~A" *url-training-plan* plan-id)))

(defun get-adaptive-training-plan-by-id (client plan-id)
  (garmin-request client (format nil "~A/fbt-adaptive/~A" *url-training-plan* plan-id)))

;;; ============================================================
;;; Golf
;;; ============================================================

(defun get-golf-summary (client &key (start 0) (limit 100))
  (garmin-request client *url-golf-scorecard-summary*
                  :params `(("per-page" . ,(princ-to-string limit))
                            ("start" . ,(princ-to-string start)))))

(defun get-golf-scorecard (client scorecard-id)
  (garmin-request client *url-golf-scorecard-detail*
                  :params `(("scorecard-ids" . ,(princ-to-string scorecard-id))
                            ("include-longest-shot-distance" . "true"))))

(defun get-golf-shot-data (client scorecard-id
                           &key (hole-numbers "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18"))
  (garmin-request client *url-golf-shot*
                  :params `(("scorecard-ids" . ,(princ-to-string scorecard-id))
                            ("hole-numbers" . ,hole-numbers))))

;;; ============================================================
;;; GraphQL
;;; ============================================================

(defun query-garmin-graphql (client query)
  "Execute a GraphQL query. QUERY is a hash-table with \"query\" and optionally \"variables\"."
  (garmin-post client *url-graphql* :json query))

;;; ============================================================
;;; Lifestyle
;;; ============================================================

(defun get-lifestyle-logging-data (client cdate)
  (validate-date cdate "cdate")
  (garmin-request client (format nil "~A/~A" *url-daily-lifestyle-logging* cdate)))
