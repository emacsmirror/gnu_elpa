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

;;; Core request

(defun forgejo-api--request (host method endpoint &optional params
				  json-body callback)
  "Make an async HTTP request to the Forgejo API.

HOST is the instance base URL (e.g. \"https://codeberg.org\").
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
                                (concat "token " (forgejo-token host))
                                'ascii))
           ("Accept" . "application/json")
           ,@(when json-body
               '(("Content-Type" . "application/json")))))
        (url-request-data
         (when json-body
           (encode-coding-string (json-encode json-body) 'utf-8)))
        (url (forgejo-api--url host endpoint params)))
    (url-retrieve
     url
     (lambda (status)
       (unwind-protect
           (let ((http-status (forgejo-api--response-status (current-buffer))))
             (cond
              ((and http-status (>= http-status 400))
               (let* ((err-data (condition-case nil
                                    (forgejo-api--parse-response (current-buffer))
                                  (json-parse-error nil)))
                      (err-msg (when (listp err-data)
                                 (alist-get 'message err-data))))
                 (message "Forgejo API HTTP %d: %s %s%s"
                          http-status method endpoint
                          (if err-msg (concat " - " err-msg) ""))))
              ((plist-get status :error)
               (message "Forgejo API error: %S" (plist-get status :error)))
              (t
               (when callback
                 (let ((headers (forgejo-api--parse-headers (current-buffer)))
                       (data (forgejo-api--parse-response (current-buffer))))
                   (funcall callback data headers))))))
         (kill-buffer (current-buffer))))
     nil t)))

;;; Public wrappers

(defun forgejo-api-get (host endpoint &optional params callback)
  "GET ENDPOINT on HOST with query PARAMS, call CALLBACK with (data headers)."
  (forgejo-api--request host "GET" endpoint params nil callback))

(defun forgejo-api-get-all (host endpoint &optional params callback)
  "GET all pages from ENDPOINT on HOST, call CALLBACK with (all-data headers).
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
                      (funcall callback accum headers)))))))))
      (fetch-page))))

(defun forgejo-api-get-paged (host endpoint params page-callback
                                   &optional done-callback)
  "GET all pages from ENDPOINT on HOST, calling PAGE-CALLBACK after each.
PAGE-CALLBACK receives (PAGE-DATA HEADERS PAGE-NUMBER).
DONE-CALLBACK receives (ALL-DATA HEADERS) when all pages are fetched."
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
                      (funcall done-callback accum headers)))))))))
      (fetch-page))))

(defun forgejo-api-post (host endpoint &optional params json-body callback)
  "POST to ENDPOINT on HOST with PARAMS and JSON-BODY, call CALLBACK."
  (forgejo-api--request host "POST" endpoint params json-body callback))

(defun forgejo-api-patch (host endpoint &optional json-body callback)
  "PATCH ENDPOINT on HOST with JSON-BODY, call CALLBACK."
  (forgejo-api--request host "PATCH" endpoint nil json-body callback))

(defun forgejo-api-put (host endpoint &optional json-body callback)
  "PUT ENDPOINT on HOST with JSON-BODY, call CALLBACK."
  (forgejo-api--request host "PUT" endpoint nil json-body callback))

(defun forgejo-api-delete (host endpoint &optional json-body callback)
  "DELETE ENDPOINT on HOST with optional JSON-BODY, call CALLBACK."
  (forgejo-api--request host "DELETE" endpoint nil json-body callback))

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
