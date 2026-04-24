;;; forgejo-alert.el --- Desktop alerts for Forgejo  -*- lexical-binding: t; -*-

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

;; Desktop alert delivery for Forgejo events.
;; Not yet wired into `forgejo-watch-hooks' automatically.
;; Enable manually with:
;;   (add-hook 'forgejo-watch-hooks #'forgejo-alert-notify)
;;
;; TODO:
;; - Distinguish new issue/PR from comment/update in the alert message
;; - Fetch latest comment body for comment-triggered alerts
;; - Handle mentions (from a future forgejo-mentions.el)
;; - Timer/symbol interaction: ensure `make load' doesn't break the
;;   timer's function reference (use symbol, not #'function)

;;; Code:

(require 'notifications)

(defcustom forgejo-alert-icon "emacs"
  "Icon for desktop alert pop-ups.
The default \"emacs\" is resolved by the system icon theme."
  :type 'string
  :group 'forgejo)

(defcustom forgejo-alert-app "Emacs Forgejo"
  "Application name for desktop alerts."
  :type 'string
  :group 'forgejo)

(defun forgejo-alert-notify (items)
  "Show a desktop alert for each item in ITEMS.
Each item is an API issue/PR alist."
  (dolist (item items)
    (let* ((repo-data (alist-get 'repository item))
           (full-name (or (alist-get 'full_name repo-data) "?/?"))
           (number (alist-get 'number item))
           (user (alist-get 'login (alist-get 'user item)))
           (issue-title (or (alist-get 'title item) ""))
           (type (if (alist-get 'pull_request item) "PR" "Issue"))
           (ref (format "%s/#%d" full-name (or number 0))))
      (forgejo-alert--show
       ref
       (format "%s by %s: %s" type (or user "?") issue-title)))))

(defun forgejo-alert--show (title body)
  "Show a desktop alert with TITLE and BODY."
  (condition-case err
      (notifications-notify
       :title title
       :body body
       :app-icon forgejo-alert-icon
       :app-name forgejo-alert-app
       :category "forgejo.alert")
    (dbus-error
     (message "forgejo-alert: D-Bus error: %s"
              (error-message-string err)))))

(provide 'forgejo-alert)
;;; forgejo-alert.el ends here
