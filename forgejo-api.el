;;; forgejo-api.el --- HTTP layer for Forgejo API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

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
;; wrappers (`forgejo-api-get', `forgejo-api-post', etc.) accept an
;; endpoint, optional parameters, and a callback that receives the
;; parsed JSON response.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)

(declare-function forgejo-token "forgejo.el" ())
(defvar forgejo-host)
(defvar forgejo--api-default-limit)
(defvar forgejo--api-max-items)

;;; URL building

(defun forgejo-api--url (endpoint &optional params)
  "Build a full API URL for ENDPOINT with optional query PARAMS.
ENDPOINT should not have a leading slash.
PARAMS is an alist of (KEY . VALUE) pairs for the query string."
  (let ((base (format "%s/api/v1/%s" forgejo-host endpoint)))
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
  "Parse pagination headers from HTTP response BUFFER.
Returns a plist with :total-count and :link."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (total-count link)
        (while (re-search-forward "^\\([^:]+\\): \\(.+\\)\r?$" nil t)
          (let ((name (downcase (match-string 1)))
                (value (match-string 2)))
            (cond
             ((string= name "x-total-count")
              (setq total-count (string-to-number value)))
             ((string= name "link")
              (setq link value)))))
        (list :total-count total-count :link link)))))

(defun forgejo-api--parse-response (buffer)
  "Parse the JSON body from HTTP response BUFFER.
Returns the parsed JSON as alists/lists."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n" nil t)
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(defun forgejo-api--response-status (buffer)
  "Return the HTTP status code from response BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

;;; Core request

(defun forgejo-api--request (method endpoint &optional params json-body callback)
  "Make an async HTTP request to the Forgejo API.

METHOD is the HTTP method string (\"GET\", \"POST\", etc.).
ENDPOINT is the API path (without /api/v1/ prefix).
PARAMS is an alist of query parameters.
JSON-BODY, when non-nil, is an alist to encode as the request body.
CALLBACK is called with two arguments: (RESPONSE-DATA HEADERS-PLIST).
  RESPONSE-DATA is the parsed JSON.
  HEADERS-PLIST contains :total-count and :link."
  (let ((url-request-method method)
        (url-request-extra-headers
         `(("Authorization" . ,(encode-coding-string
                                (concat "token " (forgejo-token)) 'ascii))
           ("Accept" . "application/json")
           ,@(when json-body
               '(("Content-Type" . "application/json")))))
        (url-request-data
         (when json-body
           (encode-coding-string (json-encode json-body) 'utf-8)))
        (url (forgejo-api--url endpoint params)))
    (url-retrieve
     url
     (lambda (status)
       (if-let* ((err (plist-get status :error)))
           (message "Forgejo API error: %S" err)
         (unwind-protect
             (let ((http-status (forgejo-api--response-status (current-buffer))))
               (if (and http-status (>= http-status 400))
                   (message "Forgejo API HTTP %d: %s %s"
                            http-status method endpoint)
                 (when callback
                   (let ((headers (forgejo-api--parse-headers (current-buffer)))
                         (data (forgejo-api--parse-response (current-buffer))))
                     (funcall callback data headers)))))
           (kill-buffer (current-buffer)))))
     nil t)))

;;; Public wrappers

(defun forgejo-api-get (endpoint &optional params callback)
  "GET ENDPOINT with query PARAMS, call CALLBACK with (data headers)."
  (forgejo-api--request "GET" endpoint params nil callback))

(defun forgejo-api-get-all (endpoint &optional params callback)
  "GET all pages from ENDPOINT, call CALLBACK with (all-data headers).
Fetches pages sequentially until all results are collected.
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
              endpoint page-params
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
                      (funcall callback accum headers)))))))))
      (fetch-page))))

(defun forgejo-api-post (endpoint &optional params json-body callback)
  "POST to ENDPOINT with PARAMS and JSON-BODY, call CALLBACK."
  (forgejo-api--request "POST" endpoint params json-body callback))

(defun forgejo-api-patch (endpoint &optional json-body callback)
  "PATCH ENDPOINT with JSON-BODY, call CALLBACK."
  (forgejo-api--request "PATCH" endpoint nil json-body callback))

(defun forgejo-api-delete (endpoint &optional callback)
  "DELETE ENDPOINT, call CALLBACK."
  (forgejo-api--request "DELETE" endpoint nil nil callback))

;;; Instance settings

(defun forgejo-api-get-settings (&optional callback)
  "Fetch API settings from the Forgejo instance.
Caches `default_paging_num' and `max_response_items' in
`forgejo--api-default-limit' and `forgejo--api-max-items'.
Calls CALLBACK with the settings alist when done."
  (forgejo-api-get
   "settings/api" nil
   (lambda (data _headers)
     (setq forgejo--api-default-limit
           (alist-get 'default_paging_num data))
     (setq forgejo--api-max-items
           (alist-get 'max_response_items data))
     (when callback
       (funcall callback data)))))

(defun forgejo-api-default-limit ()
  "Return the cached default page limit, or 50 as fallback."
  (or forgejo--api-default-limit 50))

;;; Markdown rendering

(defun forgejo-api-render-markdown (text &optional context)
  "Render markdown TEXT to HTML via the Forgejo API.
CONTEXT is an optional \"owner/repo\" string for resolving
references.  Returns the HTML string synchronously."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Authorization" . ,(encode-coding-string
                                (concat "token " (forgejo-token)) 'ascii))
           ("Content-Type" . "application/json")))
        (url-request-data
         (encode-coding-string
          (json-encode `((Context . ,(or context ""))
                         (Mode . "gfm")
                         (Text . ,text)))
          'utf-8)))
    (with-current-buffer
        (url-retrieve-synchronously
         (format "%s/api/v1/markdown" forgejo-host) t)
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n" nil t)
      (let ((html (decode-coding-string
                   (buffer-substring-no-properties (point) (point-max))
                   'utf-8)))
        (kill-buffer (current-buffer))
        html))))

(defun forgejo-api-render-markdown-async (text context callback)
  "Render markdown TEXT to HTML asynchronously.
CONTEXT is \"owner/repo\" for resolving references.
CALLBACK is called with (HTML) on success, nil on failure."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Authorization" . ,(encode-coding-string
                                (concat "token " (forgejo-token)) 'ascii))
           ("Content-Type" . "application/json")))
        (url-request-data
         (encode-coding-string
          (json-encode `((Context . ,(or context ""))
                         (Mode . "gfm")
                         (Text . ,text)))
          'utf-8)))
    (url-retrieve
     (format "%s/api/v1/markdown" forgejo-host)
     (lambda (status)
       (if (plist-get status :error)
           (progn (kill-buffer (current-buffer))
                  (when callback (funcall callback nil)))
         (unwind-protect
             (progn
               (goto-char (point-min))
               (re-search-forward "\r?\n\r?\n" nil t)
               (let ((html (decode-coding-string
                            (buffer-substring-no-properties
                             (point) (point-max))
                            'utf-8)))
                 (when callback (funcall callback html))))
           (kill-buffer (current-buffer)))))
     nil t)))

(provide 'forgejo-api)
;;; forgejo-api.el ends here
