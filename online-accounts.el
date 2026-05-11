;;; online-accounts.el --- Interface to online accounts system services  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: comm, mail
;; URL: https://codeberg.org/astoff/minimail

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

;; This library provides an auth-source to fetch credentials
;; (including OAuth tokens) from GNOME Online Accounts.  It might be
;; expanded to cover other OS-level "online accounts" services.

;;; Code:

(require 'auth-source)
(require 'dbus)

;;;###autoload
(add-hook 'auth-source-backend-parser-functions
          (defun online-accounts-auth-source-backends-parser (entry)
            "Use the `online-accounts' auth-source backend, if applicable."
            (and (eq entry 'online-accounts)
                 (require 'online-accounts nil t)
                 (-auth-source-backends-parser entry))))

(defun -auth-source-backends-parser (entry)
  (and (eq entry 'online-accounts)
       (-goa-available-p)
       (auth-source-backend
        :source "GOA"
        :type 'online-accounts
        :search-function #'-goa-password-search)))

;;; GNOME Online Accounts

(defconst -goa-ns "org.gnome.OnlineAccounts")
(defsubst -goa-ns (iface) (concat -goa-ns "." iface))

(defun -goa-available-p ()
  (and (featurep 'dbusbind)
       (dbus-ping :session -goa-ns)))

(defun -goa-password-search-1 (account user host port)
  (when-let*
      ((general (cdr (assoc (-goa-ns "Account") account)))
       (type (cond ((assoc (-goa-ns "OAuth2Based") account) 'oauth2)
                   ((assoc (-goa-ns "PasswordBased") account) 'password)))
       (found
        (or
         (when-let* ((mail (unless (cdr (assoc "MailDisabled" account))
                             (cdr (assoc (-goa-ns "Mail") account))))
                     (auser (cdr (assoc "ImapUserName" mail)))
                     (ahost (cdr (assoc "ImapHost" mail))))
           (and (or (not user) (equal user auser))
                (or (not host) (equal host ahost))
                (member port '(nil 143 993))
                (setq user auser)
                (setq host ahost)
                'imap))
         (when-let* ((mail (unless (cdr (assoc "MailDisabled" account))
                             (cdr (assoc (-goa-ns "Mail") account))))
                     (auser (cdr (assoc "SmtpUserName" mail)))
                     (ahost (cdr (assoc "SmtpHost" mail))))
           (and (or (not user) (equal user auser))
                (or (not host) (equal host ahost))
                (member port '(nil 465 587 "465" "587"))
                (setq user auser)
                (setq host ahost)
                'smtp))
         (when-let* ((contacts (unless (cdr (assoc "ContactsDisabled" account))
                                 (cdr (assoc (-goa-ns "Contacts") account))))
                     (auser (cdr (assoc "Identity" general)))
                     (uri (cdr (assoc "Uri" contacts)))
                     (ahost (url-host (url-generic-parse-url uri))))
           (and (or (not user) (equal user auser))
                (or (not host) (equal host ahost))
                (member port '(nil 80 443))
                (setq user auser)
                (setq host ahost)
                'carddav))
         (when-let* ((calendar (unless (cdr (assoc "CalendarDisabled" account))
                                 (cdr (assoc (-goa-ns "Calendar") account))))
                     (auser (cdr (assoc "Identity" general)))
                     (uri (cdr (assoc "Uri" calendar)))
                     (ahost (url-host (url-generic-parse-url uri))))
           (and (or (not user) (equal user auser))
                (or (not host) (equal host ahost))
                (member port '(nil 80 443))
                (setq user auser)
                (setq host ahost)
                'caldav))))
       (secret (pcase type
                 ('password
                  (lambda ()
                    (dbus-call-method
                     :session -goa-ns (car account) (-goa-ns "PasswordBased")
                     "GetPassword" (format "%s-password" found))))
                 ('oauth2
                  (lambda (&optional callback)
                    (if callback
                        (dbus-call-method-asynchronously
                         :session -goa-ns (car account) (-goa-ns "OAuth2Based")
                         "GetAccessToken"
                         (lambda (secret _expiry)
                           ;; How about the error case?  bug#80952
                           (funcall callback nil secret)))
                      (car (dbus-call-method
                            :session -goa-ns (car account) (-goa-ns "OAuth2Based")
                            "GetAccessToken"))))))))
    (append
     (when user (list :user user))
     (when host (list :host host))
     (when port (list :port port))
     (list :secret secret)
     (pcase type
       ;; smptmail.el should have called that property
       ;; :sasl-mechanism, as it applies also for IMAP.
       ('oauth2 '(:async t :http-auth bearer :smtp-auth xoauth2))
       ('password '(:http-auth basic :smtp-auth plain))))))

(cl-defun -goa-password-search ( &rest spec
                                 &key ;backend require
                                 max host user port
                                 &allow-other-keys)
  (let ((items (mapcar (lambda (account)
                         (-goa-password-search-1 account user host port))
                       (dbus-get-all-managed-objects
                        :session -goa-ns
                        "/org/gnome/OnlineAccounts/Accounts"))))
    (setq items (delq nil items))
    (when max (setq items (ntake max items)))
    items))

(provide 'online-accounts)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "online-accounts--"))
;; End:

;;; online-accounts.el ends here
