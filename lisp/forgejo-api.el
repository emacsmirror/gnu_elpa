;;; forgejo-api.el --- HTTP layer for Forgejo API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Async HTTP layer for the Forgejo REST API (v1).
;;
;; All network requests go through `forgejo-api--request'.  Public
;; wrappers (`forgejo-api-get', `forgejo-api-post', etc.) accept a
;; host URL, endpoint, optional parameters, and a callback that
;; receives the parsed JSON response.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)

(declare-function forgejo-token "forgejo.el" (host-url))
(defvar forgejo--api-default-limit)

(defcustom forgejo-api-timeout 30
  "Seconds before an async API request times out.
When nil, no timeout is enforced."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "No timeout" nil))
  :group 'forgejo)

(defcustom forgejo-api-rate-limit-warn-threshold 10
  "Warn when fewer than this many API requests remain."
  :type 'integer
  :group 'forgejo)

(defvar forgejo-api--rate-limit-state (make-hash-table :test 'equal)
  "Per-host rate limit state.
Keys are host URLs, values are plists with :remaining, :limit, :reset.")

(defvar forgejo-api--active-requests nil
  "List of in-flight API requests.
Each entry is (COMPLETED TIMER-CELL . URL-BUF) where COMPLETED and
TIMER-CELL are cons cells used for once-only gating and timer access.")

;;; URL building

(defun forgejo-api--url (host endpoint &optional params)
  "Build a full API URL for HOST and ENDPOINT with optional query PARAMS.
HOST is the base URL (e.g. \"https://codeberg.org\").
ENDPOINT should not have a leading slash.
PARAMS is an alist of (KEY . VALUE) pairs for the query string."
  (let ((base (format "%s/api/v1/%s" host endpoint)))
    (if params
        (concat base "?"
                (mapconcat (lambda (pair)
                             (format "%s=%s"
                                     (url-hexify-string (car pair))
                                     (url-hexify-string
                                      (format "%s" (cdr pair)))))
                           params "&"))
      base)))

;;; Response parsing

(defun forgejo-api--parse-headers (buffer)
  "Parse pagination and rate-limit headers from HTTP response BUFFER.
Returns a plist with :total-count, :link, :rate-limit-remaining,
:rate-limit-limit, and :rate-limit-reset."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (let (total-count link rl-remaining rl-limit rl-reset etag last-modified)
        (while (re-search-forward "^\\([^:\n]+\\): \\(.+\\)\r?$" nil t)
          (let ((name (downcase (match-string 1)))
                (value (string-trim-right (match-string 2))))
            (cond
             ((string= name "x-total-count")
              (setq total-count (string-to-number value)))
             ((string= name "link")
              (setq link value))
             ((string= name "x-ratelimit-remaining")
              (setq rl-remaining (string-to-number value)))
             ((string= name "x-ratelimit-limit")
              (setq rl-limit (string-to-number value)))
             ((string= name "x-ratelimit-reset")
              (setq rl-reset (string-to-number value)))
             ((string= name "etag")
              (setq etag value))
             ((string= name "last-modified")
              (setq last-modified value)))))
        (list :total-count total-count :link link
              :rate-limit-remaining rl-remaining
              :rate-limit-limit rl-limit
              :rate-limit-reset rl-reset
              :etag etag :last-modified last-modified)))))

(defun forgejo-api--parse-response (buffer)
  "Parse the JSON body from HTTP response BUFFER.
Returns the parsed JSON as alists/lists, or nil for empty bodies."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n" nil t)
    (unless (= (point) (point-max))
      (json-parse-buffer :object-type 'alist :array-type 'list))))

(defun forgejo-api--response-status (buffer)
  "Return the HTTP status code from response BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

;;; Rate limit helpers

(defun forgejo-api--format-reset-time (unix-timestamp)
  "Format UNIX-TIMESTAMP as a human-readable relative time string."
  (if unix-timestamp
      (let ((delta (- unix-timestamp (float-time))))
        (if (> delta 0)
            (format "in %d min" (ceiling (/ delta 60)))
          "now"))
    "unknown"))

(defun forgejo-api--check-rate-limit (host headers)
  "Update rate limit state for HOST from HEADERS, warn if low."
  (let ((remaining (plist-get headers :rate-limit-remaining)))
    (when remaining
      (puthash host
               (list :remaining remaining
                     :limit (plist-get headers :rate-limit-limit)
                     :reset (plist-get headers :rate-limit-reset))
               forgejo-api--rate-limit-state)
      (when (< remaining forgejo-api-rate-limit-warn-threshold)
        (message "Forgejo API: %d requests remaining (resets %s)"
                 remaining
                 (forgejo-api--format-reset-time
                  (plist-get headers :rate-limit-reset)))))))

(defun forgejo-api-rate-limit (host)
  "Return the cached rate-limit plist for HOST, or nil."
  (gethash host forgejo-api--rate-limit-state))

;;; Error helpers

(defun forgejo-api--error-plist (http-status method endpoint message data)
  "Build a structured error plist from components.
HTTP-STATUS is the numeric code (or nil for network errors).
METHOD and ENDPOINT identify the request.
MESSAGE is a human-readable error string.
DATA is the parsed response body, if any."
  (list :status http-status :method method :endpoint endpoint
        :message message :data data))

;;; Core request

(defun forgejo-api--build-headers (host json-body if-none-match if-modified-since)
  "Build HTTP headers alist for a request to HOST.
Include Content-Type when JSON-BODY is non-nil.
Include conditional headers IF-NONE-MATCH and IF-MODIFIED-SINCE
when provided."
  `(("Authorization" . ,(encode-coding-string
                         (concat "token " (forgejo-token host))
                         'ascii))
    ("Accept" . "application/json")
    ,@(and json-body '(("Content-Type" . "application/json")))
    ,@(and if-none-match `(("If-None-Match" . ,if-none-match)))
    ,@(and if-modified-since
           `(("If-Modified-Since" . ,if-modified-since)))))

(defun forgejo-api--classify-response (status buffer)
  "Classify the HTTP response in BUFFER given url-retrieve STATUS.
Returns a plist with :kind and associated data.  Pure: does not
call callbacks or modify global state.

Possible :kind values:
  http-error   -- HTTP 4xx/5xx, includes :status :headers :message :data
  net-error    -- network/connection failure, includes :message
  not-modified -- HTTP 304, includes :headers
  success      -- HTTP 2xx, includes :headers :data"
  (with-current-buffer buffer
    (let ((http-status (forgejo-api--response-status buffer))
          (headers (forgejo-api--parse-headers buffer)))
      (cond
       ((and http-status (>= http-status 400))
        (let* ((data (condition-case nil
                         (forgejo-api--parse-response buffer)
                       (json-parse-error nil)))
               (msg (and (listp data) (alist-get 'message data))))
          (list :kind 'http-error :status http-status
                :headers headers :message msg :data data)))
       ((plist-get status :error)
        (list :kind 'net-error
              :message (format "%S" (plist-get status :error))))
       ((and http-status (= http-status 304))
        (list :kind 'not-modified :headers headers))
       (t
        (list :kind 'success :headers headers
              :data (forgejo-api--parse-response buffer)))))))

(defun forgejo-api--report-error (method endpoint result error-callback)
  "Report an error described by classified RESULT for METHOD ENDPOINT.
Call ERROR-CALLBACK with a structured error plist when provided,
otherwise log a human-readable message."
  (let ((error-info (forgejo-api--error-plist
                     (plist-get result :status)
                     method endpoint
                     (plist-get result :message)
                     (plist-get result :data))))
    (if error-callback
        (funcall error-callback error-info)
      (pcase (plist-get result :kind)
        ('http-error
         (message "Forgejo API HTTP %d: %s %s%s"
                  (plist-get result :status) method endpoint
                  (if (plist-get result :message)
                      (concat " - " (plist-get result :message)) "")))
        ('net-error
         (message "Forgejo API error: %s" (plist-get result :message)))
        ('timeout
         (message "Forgejo API timeout: %s %s" method endpoint))))))

(defun forgejo-api--act-on-response (result host method endpoint
                                            callback error-callback)
  "Act on classified RESULT for METHOD ENDPOINT on HOST.
Call CALLBACK on success/not-modified, ERROR-CALLBACK on errors.
Updates rate-limit state when headers are present."
  (let ((headers (plist-get result :headers)))
    (when headers
      (forgejo-api--check-rate-limit host headers))
    (pcase (plist-get result :kind)
      ('success
       (when callback
         (funcall callback (plist-get result :data) headers)))
      ('not-modified
       (when callback
         (funcall callback nil headers)))
      ((or 'http-error 'net-error)
       (forgejo-api--report-error
        method endpoint result error-callback)))))

(defun forgejo-api--dispatch-response (status host method endpoint
                                              callback error-callback
                                              completed timer-cell)
  "Handle a url-retrieve response for METHOD ENDPOINT on HOST.
STATUS is the url-retrieve status plist.
COMPLETED is a cons cell for once-only execution.
TIMER-CELL is a cons cell whose car holds the timeout timer."
  (unless (car completed)
    (setcar completed t)
    (let ((timer (car timer-cell)))
      (when timer (cancel-timer timer)))
    (unwind-protect
        (let ((result (forgejo-api--classify-response
                       status (current-buffer))))
          (forgejo-api--act-on-response
           result host method endpoint callback error-callback))
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))))

(defun forgejo-api--start-timeout (completed url-buf method endpoint
                                             error-callback)
  "Arm a timeout timer for an in-flight request to METHOD ENDPOINT.
COMPLETED is the once-only gate cell.
URL-BUF is the url-retrieve buffer to clean up on timeout.
Returns the timer object."
  (run-at-time
   forgejo-api-timeout nil
   (lambda ()
     (unless (car completed)
       (setcar completed t)
       (when (buffer-live-p url-buf)
         (let ((proc (get-buffer-process url-buf)))
           (when proc (delete-process proc)))
         (kill-buffer url-buf))
       (forgejo-api--report-error
        method endpoint '(:kind timeout) error-callback)))))

;;; Request registry

(defun forgejo-api--register-request (completed timer-cell url-buf)
  "Track an in-flight request for later cancellation.
Prunes already-completed entries to keep the list short."
  (setq forgejo-api--active-requests
        (cons (cons completed (cons timer-cell url-buf))
              (cl-remove-if (lambda (entry) (car (car entry)))
                            forgejo-api--active-requests))))

(defun forgejo-api--cancel-request (entry)
  "Cancel a single in-flight request ENTRY.
ENTRY is (COMPLETED TIMER-CELL . URL-BUF)."
  (let ((completed (car entry))
        (timer-cell (cadr entry))
        (url-buf (cddr entry)))
    (unless (car completed)
      (setcar completed t)
      (let ((timer (car timer-cell)))
        (when timer (cancel-timer timer)))
      (when (buffer-live-p url-buf)
        (let ((proc (get-buffer-process url-buf)))
          (when proc (delete-process proc)))
        (kill-buffer url-buf)))))

(defun forgejo-api-reset ()
  "Cancel all in-flight Forgejo API requests and flush idle connections.
Use after network changes (VPN toggle, route change) to start fresh."
  (interactive)
  (let ((count 0))
    (dolist (entry forgejo-api--active-requests)
      (unless (car (car entry))
        (forgejo-api--cancel-request entry)
        (cl-incf count)))
    (setq forgejo-api--active-requests nil)
    ;; Flush idle keep-alive sockets for known Forgejo hosts
    (when (bound-and-true-p url-http-open-connections)
      (let ((hosts (hash-table-keys forgejo-api--rate-limit-state)))
        (maphash
         (lambda (key procs)
           (let ((conn-host (car key)))
             (when (cl-some (lambda (h)
                              (string-match-p (regexp-quote conn-host) h))
                            hosts)
               (dolist (proc procs)
                 (when (processp proc)
                   (delete-process proc)))
               (remhash key url-http-open-connections))))
         url-http-open-connections)))
    (message "Forgejo API: cancelled %d request%s, connections flushed"
             count (if (= count 1) "" "s"))))

(defun forgejo-api--request (host method endpoint &optional params
                                  json-body callback &rest args)
  "Make an async HTTP request to the Forgejo API.

HOST is the instance base URL (e.g. \"https://codeberg.org\").
METHOD is the HTTP method string (\"GET\", \"POST\", etc.).
ENDPOINT is the API path (without /api/v1/ prefix).
PARAMS is an alist of query parameters.
JSON-BODY, when non-nil, is an alist to encode as the request body.
CALLBACK is called with two arguments: (RESPONSE-DATA HEADERS-PLIST).
  RESPONSE-DATA is the parsed JSON.
  HEADERS-PLIST contains :total-count and :link.

ARGS is a plist of keyword options:
  :error-callback -- function called with an error plist on failure.
    The plist contains :status, :method, :endpoint, :message, :data.
    When omitted, errors are logged to *Messages*.
  :if-none-match -- ETag value for conditional requests.
  :if-modified-since -- date string for conditional requests.
    On 304 Not Modified, CALLBACK receives nil data."
  (let* ((error-callback (plist-get args :error-callback))
         (completed (cons nil nil))
         (timer-cell (cons nil nil))
         (url-request-method method)
         (url-request-extra-headers
          (forgejo-api--build-headers
           host json-body
           (plist-get args :if-none-match)
           (plist-get args :if-modified-since)))
         (url-request-data
          (when json-body
            (encode-coding-string (json-encode json-body) 'utf-8)))
         (url (forgejo-api--url host endpoint params))
         (url-buf
          (url-retrieve
           url #'forgejo-api--dispatch-response
           (list host method endpoint
                 callback error-callback
                 completed timer-cell)
           t)))
    (when url-buf
      (forgejo-api--register-request completed timer-cell url-buf)
      (when forgejo-api-timeout
        (setcar timer-cell
                (forgejo-api--start-timeout
                 completed url-buf method endpoint error-callback))))))

;;; Public wrappers

(defun forgejo-api-get (host endpoint &optional params callback &rest args)
  "GET ENDPOINT on HOST with query PARAMS, call CALLBACK with (data headers).
ARGS accepts :error-callback for failure handling."
  (apply #'forgejo-api--request host "GET" endpoint params nil callback args))

(defun forgejo-api-get-all (host endpoint &optional params callback)
  "GET all pages from ENDPOINT on HOST, call CALLBACK with (all-data headers).
Fetches pages sequentially until all results are collected.
On mid-pagination failure, calls CALLBACK with partial data and
headers tagged with :partial t.
PARAMS should include a \"limit\" entry.  The \"page\" param is
managed automatically."
  (let ((limit (or (cdr (assoc "limit" params)) "30"))
        (accum nil)
        (page 1))
    (cl-labels
        ((fetch-page ()
           (let ((page-params (cons (cons "page" (number-to-string page))
                                    params)))
             (forgejo-api-get
              host endpoint page-params
              (lambda (data headers)
                (setq accum (append accum data))
                (let ((total (plist-get headers :total-count)))
                  (if (and total
                           (< (length accum) total)
                           (>= (length data) (string-to-number limit)))
                      (progn
                        (setq page (1+ page))
                        (fetch-page))
                    (when callback
                      (funcall callback accum headers)))))
              :error-callback
              (lambda (error-info)
                (when callback
                  (funcall callback accum
                           (list :total-count nil :link nil
                                 :partial t :error error-info))))))))
      (fetch-page))))

(defun forgejo-api-get-paged (host endpoint params page-callback
                                   &optional done-callback)
  "GET all pages from ENDPOINT on HOST, calling PAGE-CALLBACK after each.
PAGE-CALLBACK receives (PAGE-DATA HEADERS PAGE-NUMBER).
DONE-CALLBACK receives (ALL-DATA HEADERS) when all pages are fetched.
On mid-pagination failure, calls DONE-CALLBACK with partial data and
headers tagged with :partial t."
  (let ((limit (or (cdr (assoc "limit" params)) "50"))
        (accum nil)
        (page 1))
    (cl-labels
        ((fetch-page ()
           (let ((page-params (cons (cons "page" (number-to-string page))
                                    params)))
             (forgejo-api-get
              host endpoint page-params
              (lambda (data headers)
                (setq accum (append accum data))
                (when page-callback
                  (funcall page-callback data headers page))
                (let ((total (plist-get headers :total-count)))
                  (if (and total
                           (< (length accum) total)
                           (>= (length data) (string-to-number limit)))
                      (progn
                        (setq page (1+ page))
                        (fetch-page))
                    (when done-callback
                      (funcall done-callback accum headers)))))
              :error-callback
              (lambda (error-info)
                (when done-callback
                  (funcall done-callback accum
                           (list :total-count nil :link nil
                                 :partial t :error error-info))))))))
      (fetch-page))))

(defun forgejo-api-post (host endpoint &optional params json-body callback
                              &rest args)
  "POST to ENDPOINT on HOST with PARAMS and JSON-BODY, call CALLBACK.
ARGS accepts :error-callback for failure handling."
  (apply #'forgejo-api--request host "POST" endpoint params json-body
         callback args))

(defun forgejo-api-patch (host endpoint &optional json-body callback &rest args)
  "PATCH ENDPOINT on HOST with JSON-BODY, call CALLBACK.
ARGS accepts :error-callback for failure handling."
  (apply #'forgejo-api--request host "PATCH" endpoint nil json-body
         callback args))

(defun forgejo-api-put (host endpoint &optional json-body callback &rest args)
  "PUT ENDPOINT on HOST with JSON-BODY, call CALLBACK.
ARGS accepts :error-callback for failure handling."
  (apply #'forgejo-api--request host "PUT" endpoint nil json-body
         callback args))

(defun forgejo-api-delete (host endpoint &optional json-body callback &rest args)
  "DELETE ENDPOINT on HOST with optional JSON-BODY, call CALLBACK.
ARGS accepts :error-callback for failure handling."
  (apply #'forgejo-api--request host "DELETE" endpoint nil json-body
         callback args))

;;; Conditional requests

(declare-function forgejo-db-get-cache-headers "forgejo-db.el"
                  (host owner repo endpoint))
(declare-function forgejo-db-set-cache-headers "forgejo-db.el"
                  (host owner repo endpoint etag last-modified))

(defun forgejo-api-get-conditional (host endpoint params
                                         cache-key callback &rest args)
  "GET with conditional headers from DB cache.
CACHE-KEY is a list (HOST OWNER REPO ENDPOINT-NAME) for cache lookup.
On 304, calls CALLBACK with nil data.  On 200, updates cached headers.
ARGS are passed through to `forgejo-api-get'."
  (let* ((cached (apply #'forgejo-db-get-cache-headers cache-key))
         (etag (plist-get cached :etag))
         (last-mod (plist-get cached :last-modified)))
    (apply #'forgejo-api-get host endpoint params
           (lambda (data headers)
             (when data
               (let ((new-etag (plist-get headers :etag))
                     (new-last-mod (plist-get headers :last-modified)))
                 (when (or new-etag new-last-mod)
                   (apply #'forgejo-db-set-cache-headers
                          (append cache-key
                                  (list new-etag new-last-mod))))))
             (when callback (funcall callback data headers)))
           :if-none-match etag
           :if-modified-since last-mod
           args)))

;;; Instance settings

(defun forgejo-api-get-settings (host &optional callback)
  "Fetch API settings from HOST.
Caches `default_paging_num' in `forgejo--api-default-limit'.
Calls CALLBACK with the settings alist when done."
  (forgejo-api-get
   host "settings/api" nil
   (lambda (data _headers)
     (setq forgejo--api-default-limit
           (alist-get 'default_paging_num data))
     (when callback
       (funcall callback data)))))

(defun forgejo-api-default-limit ()
  "Return the cached default page limit, or 50 as fallback."
  (or forgejo--api-default-limit 50))

(provide 'forgejo-api)
;;; forgejo-api.el ends here
