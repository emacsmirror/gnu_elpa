;;; auth-source-xoauth2-plugin.el --- authentication source plugin for xoauth2 -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

;; Author: Xiyue Deng <manphiz@gmail.com>
;; Version: 0.2.1
;; Package-Requires: ((emacs "28.1") (oauth2 "0.18"))

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An auth-source plugin to enable xoauth2 support.

;; This package provides a global minor mode for enabling xoauth2 in
;; auth-source.  Once adding information required for xoauth2 authentication in
;; your auth-source file and enabling the global minor mode, one can
;; authenticate through xoauth2 to supported services, e.g. Gmail, etc.

;; See README.org for a more detailed introduction and usages.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'map)
(require 'oauth2)
(require 'smtpmail)

(defun auth-source-xoauth2-plugin--search-backends (orig-fun &rest args)
  "Perform `auth-source-search' and set password as access-token when requested.
Calls ORIG-FUN which would be `auth-source-search-backends' with
ARGS to get the auth-source-entry.  The substitution only happens
if one sets `auth' to `xoauth2' in your auth-source-entry.  It is
expected that `token_url', `client_id', `client_secret', and
`refresh_token' are properly set along `host', `user', and
`port' (note the snake_case)."
  (auth-source-do-trivia "Advising auth-source-search")
  (let (check-secret)
    (when (memq :secret (nth 5 args))
      (auth-source-do-trivia
       "Required fields include :secret.  As we are requesting access token to replace the secret, we'll temporary remove :secret from the require list and check that it's properly set to a valid access token later.")
      (setf (nth 5 args) (remove :secret (nth 5 args)))
      (setq check-secret t))
    (let ((orig-res (apply orig-fun args))
          res)
      (dolist (auth-data orig-res)
        (auth-source-do-trivia "Matched auth data: %s" (pp-to-string auth-data))
        (let ((auth (plist-get auth-data :auth)))
          (when (equal auth "xoauth2")
            (auth-source-do-debug
             ":auth set to `xoauth2'.  Will get access token.")
            (map-let (:auth-url
                      :token-url
                      :scope
                      :client-id
                      :client-secret
                      :redirect-uri
                      :state)
                auth-data
              (auth-source-do-debug "Using oauth2 to auth and store token...")
              (let ((token (oauth2-auth-and-store
                            auth-url token-url scope client-id client-secret
                            redirect-uri state)))
                (auth-source-do-trivia "oauth2 token: %s" (pp-to-string token))
                (auth-source-do-debug "Refreshing token...")
                (oauth2-refresh-access token)
                (auth-source-do-debug "Refresh successful.")
                (auth-source-do-trivia "oauth2 token after refresh: %s"
                                       (pp-to-string token))
                (let ((access-token (oauth2-token-access-token token)))
                  (auth-source-do-trivia
                   "Updating :secret with access-token: %s" access-token)
                  (setq auth-data
                        (plist-put auth-data :secret access-token)))))))

        (unless (and check-secret
                     (not (plist-get auth-data :secret)))
          (auth-source-do-debug "Updating auth-source-search results.")
          (push auth-data res)))
      res)))

(defvar auth-source-xoauth2-plugin--enabled-xoauth2-by-us nil
  "Non-nil means `smtpmail-auth-supported' was set by us.")

(defun auth-source-xoauth2-plugin--enable ()
  "Enable auth-source-xoauth2-plugin."
  (unless (memq 'xoauth2 smtpmail-auth-supported)
    ;; smtpmail considers smtp request with a return value less than 400 to be
    ;; successful, but for Gmail when an xoauth2 request fails it returns 334
    ;; server challenge, and waiting for a subsequent request with the correct
    ;; credentials which will never happen.  Putting 'xoauth2 as the last entry
    ;; in smtpmail-auth-supported so that it is tried last.  See also
    ;; https://debbugs.gnu.org/78366.
    (add-to-list 'smtpmail-auth-supported 'xoauth2 t)
    (setq auth-source-xoauth2-plugin--enabled-xoauth2-by-us t))

  (advice-add #'auth-source-search-backends :around
              #'auth-source-xoauth2-plugin--search-backends))

(defun auth-source-xoauth2-plugin--disable ()
  "Disable auth-source-xoauth2-plugin."
  (when (and auth-source-xoauth2-plugin--enabled-xoauth2-by-us
             (memq 'xoauth2 smtpmail-auth-supported))
    (setq smtpmail-auth-supported (delq 'xoauth2 smtpmail-auth-supported))
    (setq auth-source-xoauth2-plugin--enabled-xoauth2-by-us nil))

  (advice-remove #'auth-source-search-backends
                 #'auth-source-xoauth2-plugin--search-backends))

;;;###autoload
(define-minor-mode auth-source-xoauth2-plugin-mode
  "Toggle auth-source-xoauth2-plugin-mode.
Enable auth-source-xoauth2-plugin-mode to use xoauth2
authentications for emails."
  :global t
  (if auth-source-xoauth2-plugin-mode
      (auth-source-xoauth2-plugin--enable)
    (auth-source-xoauth2-plugin--disable)))

(provide 'auth-source-xoauth2-plugin)

;;; auth-source-xoauth2-plugin.el ends here
