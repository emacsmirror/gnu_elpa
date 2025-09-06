;;; auth-source-xoauth2-plugin.el --- Authentication source plugin for xoauth2 -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

;; Author: Xiyue Deng <manphiz@gmail.com>
;; Homepage: https://gitlab.com/manphiz/auth-source-xoauth2-plugin
;; Version: 0.3.1
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
(require 'org)
(require 'smtpmail)

(defvar auth-source-xoauth2-plugin-predefined-issuers
  '(thunderbird
    (google
     ( :client-id "406964657835-aq8lmia8j95dhl1a2bvharmfk3t1hgqj.apps.googleusercontent.com"
       :client-secret "kSmqreRr0qwBWJgbf5Y-PjSU"
       :auth-url "https://accounts.google.com/o/oauth2/auth"
       :token-url "https://www.googleapis.com/oauth2/v3/token"
       :redirect-uri "http://localhost/"
       :scope "https://mail.google.com/"
       :use-pkce "true" )
     microsoft
     ( :client-id "9e5f94bc-e8a4-4e73-b8be-63364c29d753"
       ;; :client-secret ""
       :auth-url "https://login.microsoftonline.com/common/oauth2/v2.0/authorize"
       :token-url "https://login.microsoftonline.com/common/oauth2/v2.0/token"
       :redirect-uri "http://localhost"
       :scope "https://outlook.office.com/IMAP.AccessAsUser.All https://outlook.office.com/POP.AccessAsUser.All https://outlook.office.com/SMTP.Send offline_access"
       :use-pkce "true" ))))

(defvar auth-source-xoauth2-plugin-default-predefined-source "thunderbird"
  "The default predefined issuers provider.")

(defun auth-source-xoauth2-plugin--get-predefined-credentials (source provider)
  "Helper function to get the predefined credentials of PROVIDER from SOURCE."
  (plist-get (plist-get auth-source-xoauth2-plugin-predefined-issuers
                        (intern source))
             (intern provider)))

(defun auth-source-xoauth2-plugin--search-backends (orig-fun &rest args)
  "Perform `auth-source-search' and set password as access-token when requested.
Calls ORIG-FUN which would be `auth-source-search-backends' with
ARGS to get the auth-source-entry.  The substitution only happens
if one sets `auth' to `xoauth2' in your auth-source-entry.  It is
expected that `token_url', `client_id', `client_secret', and
`refresh_token' are properly set along `host', `user', and
`port' (note the snake_case)."
  (auth-source-do-trivia "[xoauth2-plugin] Advising auth-source-search")
  (let (check-secret)
    (when (memq :secret (nth 5 args))
      (auth-source-do-trivia
       (concat "[xoauth2-plugin] Required fields include :secret.  As we are "
               "requesting access token to replace the secret, we'll "
               "temporary remove :secret from the require list and check that "
               "it's properly set to a valid access token later."))
      (setf (nth 5 args) (remove :secret (nth 5 args)))
      (setq check-secret t))
    (let ((orig-res (apply orig-fun args))
          res)
      (dolist (auth-data orig-res)
        (auth-source-do-trivia "[xoauth2-plugin] Matched auth data: %s"
                               (pp-to-string auth-data))
        (let ((auth (plist-get auth-data :auth))
              (user (plist-get auth-data :user)))
          (when (and (equal auth "xoauth2")
                     ;; When sending mails, some auth-source query results from
                     ;; some smtpmail authentication methods don't contain the
                     ;; :user field (meanwhile queries from Gnus seems to always
                     ;; include :user).  When using predefined provider
                     ;; credentials, only the :user field is different to
                     ;; distinguish among different accounts, which is
                     ;; unfortunately missing in certain cases.  Fortunately,
                     ;; smtpmail may set smtpmail-smtp-user to the user value
                     ;; when X-Message-SMTP-Method is properly set.  Therefore
                     ;; additionally, assuming X-Message-SMTP-Method is set
                     ;; correctly, we need to check whether smtpmail-smtp-user
                     ;; is the same as :user to be sure.
                     (if smtpmail-smtp-user
                         (progn
                           (auth-source-do-trivia
                            "[xoauth2-plugin] user: %s, smtpmail-smtp-user: %s"
                            user smtpmail-smtp-user)
                           (string= smtpmail-smtp-user user))
                       t))
            (auth-source-do-debug
             (concat "[xoauth2-plugin] account \"%s\" has :auth set to "
                     "`xoauth2'.  Will get access token.")
             user)
            (map-let (:auth-source-xoauth2-predefined-service
                      (:auth-source-xoauth2-predefined-source
                       auth-source-xoauth2-predefined-source
                       auth-source-xoauth2-plugin-default-predefined-source))
                auth-data
              (when auth-source-xoauth2-predefined-service
                (auth-source-do-trivia
                 (concat "[xoauth2-plugin] Using service \"%s\" with "
                         "credentials provided by source \"%s\"")
                 auth-source-xoauth2-predefined-service
                 auth-source-xoauth2-predefined-source)
                (setq auth-data
                      (org-combine-plists
                       auth-data
                       (auth-source-xoauth2-plugin--get-predefined-credentials
                        auth-source-xoauth2-predefined-source
                        auth-source-xoauth2-predefined-service)))))
            (map-let (:host
                      :user
                      :auth-url
                      :token-url
                      :scope
                      :client-id
                      :client-secret
                      :redirect-uri
                      :state
                      :use-pkce)
                auth-data
              (auth-source-do-debug
               "[xoauth2-plugin] Using oauth2 to auth and store token...")
              (let ((token (oauth2-auth-and-store
                            auth-url token-url scope client-id client-secret
                            redirect-uri state user host use-pkce)))
                (auth-source-do-trivia "[xoauth2-plugin] oauth2 token: %s"
                                       (pp-to-string token))
                (auth-source-do-debug "[xoauth2-plugin] Refreshing token...")
                (oauth2-refresh-access token host)
                (auth-source-do-debug "[xoauth2-plugin] Refresh successful.")
                (auth-source-do-trivia
                 "[xoauth2-plugin] OAuth2 token after refresh: %s"
                 (pp-to-string token))
                (let ((access-token (oauth2-token-access-token token)))
                  (auth-source-do-trivia
                   "Updating :secret with access-token: %s" access-token)
                  (setq auth-data
                        (plist-put auth-data :secret access-token))
                  ;; Fill fields that may help 3rd party usage,
                  ;; e.g. offlineimap.
                  (setq auth-data
                        (plist-put auth-data :auth-url auth-url))
                  (setq auth-data
                        (plist-put auth-data :token-url token-url))
                  (setq auth-data
                        (plist-put auth-data :client-id client-id))
                  (setq auth-data
                        (plist-put auth-data :client-secret client-secret))
                  (setq auth-data
                        (plist-put auth-data :access-token
                                   (oauth2-token-access-token token)))
                  (setq auth-data
                        (plist-put auth-data :refresh-token
                                   (oauth2-token-refresh-token token))))))))

        (auth-source-do-debug "[xoauth2-plugin] auth-data after processing: %s"
                              (pp-to-string auth-data))
        (unless (and check-secret
                     (not (plist-get auth-data :secret)))
          (auth-source-do-debug
           "[xoauth2-plugin] Updating auth-source-search results.")
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
  :group 'auth-source
  (if auth-source-xoauth2-plugin-mode
      (auth-source-xoauth2-plugin--enable)
    (auth-source-xoauth2-plugin--disable)))

(provide 'auth-source-xoauth2-plugin)

;;; auth-source-xoauth2-plugin.el ends here
