;;; forgejo-token.el --- Token creation for Forgejo  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

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

;; Create Forgejo API tokens via Basic Auth and save them to
;; auth-source.

;;; Code:

(require 'auth-source)
(require 'url)
(require 'forgejo)
(require 'forgejo-api)

(defun forgejo-token--request (host-url username password)
  "Create an API token on HOST-URL for USERNAME with PASSWORD.
Returns the token string on success, signals an error otherwise."
  (let* ((url (forgejo-api--url host-url
                                (format "users/%s/tokens" username)))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization"
             . ,(concat "Basic "
                        (base64-encode-string
                         (format "%s:%s" username password) t)))
            ("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string
           (json-encode '((name . "emacs-forgejo") (scopes . ("all"))))
           'utf-8))
         (buf (url-retrieve-synchronously url t)))
    (unwind-protect
        (let ((status (forgejo-api--response-status buf))
              (data (forgejo-api--parse-response buf)))
          (unless (and status (< status 300))
            (user-error "Token creation failed (HTTP %s): %s"
                        (or status "?")
                        (or (alist-get 'message data) "")))
          (alist-get 'sha1 data))
      (kill-buffer buf))))

(defun forgejo-token--save (host username token)
  "Save TOKEN for USERNAME@HOST to auth-source.
Falls back to copying TOKEN to the kill ring."
  (let* ((auth-source-creation-defaults `((secret . ,token)))
         (auth-source-creation-prompts '((secret . "Token: ")))
         (entry (car (auth-source-search
                      :host host :user username
                      :require '(:secret)
                      :create t)))
         (save-fn (plist-get entry :save-function)))
    (if (functionp save-fn)
        (progn
          (funcall save-fn)
          (auth-source-forget+ :host host)
          (message "Token saved to auth-source for %s@%s." username host))
      (kill-new token)
      (message "No writable auth-source; token copied to kill ring."))))

;;;###autoload
(defun forgejo-create-token (&optional host-url)
  "Create a Forgejo API token and save it to auth-source.
HOST-URL is the instance URL; prompted when nil.
Returns the token string."
  (interactive)
  (let* ((host-url (or host-url (forgejo--host-from-hosts-list)))
         (host (url-host (url-generic-parse-url host-url)))
         (username (read-string (format "Username on %s: " host)))
         (password (read-passwd (format "Password for %s@%s: " username host)))
         (token (forgejo-token--request host-url username password)))
    (forgejo-token--save host username token)
    token))

(provide 'forgejo-token)
;;; forgejo-token.el ends here
