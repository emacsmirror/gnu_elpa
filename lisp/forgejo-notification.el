;;; forgejo-notification.el --- Notification polling and display  -*- lexical-binding: t; -*-

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

;; Notification polling and display for Forgejo instances.
;;
;; Polls the Forgejo notification API and caches results in the local
;; DB.  The hook system is designed to also support future local watch
;; rules (custom per-repo/label filters that generate notifications
;; from the sync process).
;;
;; Enable `forgejo-notification-mode' to start polling.
;; Use `forgejo-notification-list' to browse notifications.

;;; Code:

(require 'cl-lib)
(require 'url-parse)
(require 'forgejo)
(require 'forgejo-api)
(require 'forgejo-tl)
(require 'forgejo-db)
(require 'forgejo-filter)
(require 'forgejo-utils)
(require 'forgejo-buffer)

(declare-function forgejo-issue-view "forgejo-issue.el"
                  (owner repo number))
(declare-function forgejo-pull-view "forgejo-pull.el"
                  (owner repo number))

(defvar forgejo-host)
(defvar forgejo-repo--host)

;;; Customization

(defcustom forgejo-notification-poll-interval 300
  "Seconds between notification polls."
  :type 'integer
  :group 'forgejo)

(defcustom forgejo-notification-hooks nil
  "Hook run when new notifications arrive.
Each function receives one argument: the list of new notification alists.
This hook fires for both API-sourced and future locally-generated
notifications."
  :type 'hook
  :group 'forgejo)

;;; Desktop notifications

(require 'notifications)

(defcustom forgejo-notification-icon "emacs"
  "Icon for desktop notification pop-ups.
The default \"emacs\" is resolved by the system icon theme."
  :type 'string
  :group 'forgejo)

(defcustom forgejo-notification-app "Emacs Forgejo"
  "Application name for desktop notifications."
  :type 'string
  :group 'forgejo)

(defun forgejo-notification--desktop-notify (notifications)
  "Show a desktop notification for new NOTIFICATIONS."
  (let ((count (length notifications)))
    (condition-case err
        (notifications-notify
         :title "Forgejo"
         :body (format "%d new notification%s" count (if (= count 1) "" "s"))
         :app-icon forgejo-notification-icon
         :app-name forgejo-notification-app
         :category "forgejo.notification")
      (dbus-error
       (message "forgejo-notification: D-Bus error: %s"
                (error-message-string err))))))

(add-hook 'forgejo-notification-hooks #'forgejo-notification--desktop-notify)

;;; State

(defvar forgejo-notification--timer nil
  "Timer for periodic notification polling.")

;;; Polling

(defun forgejo-notification--poll ()
  "Fetch unread notifications, save to DB, and run hooks on new ones."
  (let ((host (url-host (url-generic-parse-url forgejo-host))))
    (forgejo-api-get
     "notifications"
     '(("status-types" . "unread"))
     (lambda (data _headers)
       (when (listp data)
         (let ((old-count (forgejo-db-notification-unread-count host)))
           (forgejo-db-save-notifications host data)
           (when (> (length data) old-count)
             (run-hook-with-args 'forgejo-notification-hooks data))))
       (forgejo-notification--refresh-list-buffer host)))))

(defun forgejo-notification--refresh-list-buffer (host)
  "Re-render the notification list buffer for HOST if visible."
  (when-let* ((buf (get-buffer "*forgejo-notifications*")))
    (when (get-buffer-window buf)
      (with-current-buffer buf
        (forgejo-notification--render host)))))

;;; Global minor mode

;;;###autoload
(define-minor-mode forgejo-notification-mode
  "Toggle Forgejo notification polling.
Polls the Forgejo API periodically for unread notifications
and runs `forgejo-notification-hooks' when new ones arrive."
  :global t
  :lighter nil
  :group 'forgejo
  (if forgejo-notification-mode
      (progn
        (setq forgejo-notification--timer
              (run-with-timer 0 forgejo-notification-poll-interval
                              #'forgejo-notification--poll))
        (add-hook 'kill-emacs-hook #'forgejo-notification--cleanup))
    (forgejo-notification--cleanup)))

(defun forgejo-notification--cleanup ()
  "Cancel the polling timer."
  (when forgejo-notification--timer
    (cancel-timer forgejo-notification--timer)
    (setq forgejo-notification--timer nil))
  (remove-hook 'kill-emacs-hook #'forgejo-notification--cleanup))

;;; Notification list buffer

(defvar-local forgejo-notification--host nil
  "Hostname for the current notification list buffer.")

(defvar-local forgejo-notification--filters nil
  "Current filter plist for the notification list.")

(defvar forgejo-notification-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'forgejo-notification-view-at-point)
    (define-key map (kbd "r") #'forgejo-notification-mark-read-at-point)
    (define-key map (kbd "R") #'forgejo-notification-mark-all-read)
    (define-key map (kbd "l") #'forgejo-notification-filter)
    (define-key map (kbd "C") #'forgejo-notification-clear-filters)
    (define-key map (kbd "g") #'forgejo-notification-list-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `forgejo-notification-list-mode'.")

(define-derived-mode forgejo-notification-list-mode tabulated-list-mode
  "Forgejo Notifications"
  "Major mode for browsing Forgejo notifications."
  :group 'forgejo
  (setq tabulated-list-padding 1
        tabulated-list-format (forgejo-filter-list-format
                               forgejo-filter-notification-columns))
  (tabulated-list-init-header))

(defun forgejo-notification--subject-number (url)
  "Extract the issue/PR number from a Forgejo API subject URL.
URL looks like \"https://host/api/v1/repos/owner/repo/issues/42\"."
  (when (and url (string-match "/\\([0-9]+\\)\\'" url))
    (string-to-number (match-string 1 url))))

(defun forgejo-notification--entries (rows)
  "Convert notification ROWS from DB to `tabulated-list-entries'.
Each ROW: 0=id 1=subject_type 2=subject_title 3=subject_url
          4=subject_state 5=repo_owner 6=repo_name 7=status 8=updated_at.
The entry ID is (notification-id . subject-url) so view-at-point
can extract the issue/PR number without re-querying the DB."
  (mapcar
   (lambda (row)
     (list (cons (nth 0 row) (nth 3 row))
           (vector
            (format "%s/%s" (or (nth 5 row) "") (or (nth 6 row) ""))
            (or (nth 1 row) "")
            (or (nth 2 row) "")
            (or (nth 7 row) "")
            (forgejo-buffer--relative-time (nth 8 row)))))
   rows))

(defun forgejo-notification--render (host)
  "Render notifications from DB for HOST into the current buffer."
  (let* ((rows (forgejo-filter-query-notifications
                host forgejo-notification--filters)))
    (setq tabulated-list-entries (forgejo-notification--entries rows))
    (forgejo-tl-print)
    (goto-char (point-min))))

(defun forgejo-notification--fetch-all (host)
  "Fetch all notifications (unread+read) in pages and save to DB.
Re-renders the list buffer after all pages are fetched."
  (forgejo-api-get-paged
   "notifications"
   '(("status-types" . "unread,read")
     ("limit" . "50"))
   (lambda (page-data _headers _page)
     (when (listp page-data)
       (forgejo-db-save-notifications host page-data)))
   (lambda (_all-data _headers)
     (forgejo-notification--refresh-list-buffer host))))

;;;###autoload
(defun forgejo-notification-list ()
  "Browse Forgejo notifications."
  (interactive)
  (let* ((host (url-host (url-generic-parse-url forgejo-host)))
         (buf (get-buffer-create "*forgejo-notifications*")))
    (with-current-buffer buf
      (forgejo-notification-list-mode)
      (setq forgejo-notification--host host
            forgejo-repo--host forgejo-host)
      ;; Render from cache immediately
      (forgejo-notification--render host)
      (switch-to-buffer buf))
    ;; Fetch full history in background
    (forgejo-notification--fetch-all host)))

(defun forgejo-notification-list-refresh ()
  "Refresh the notification list."
  (interactive)
  (forgejo-notification--fetch-all forgejo-notification--host))

;;; Filtering

(defun forgejo-notification-filter ()
  "Filter the notification list."
  (interactive)
  (let* ((completions (forgejo-filter-completions-for-notifications
                       forgejo-notification--host))
         (current (forgejo-filter-serialize
                   forgejo-notification--filters
                   forgejo-filter--notification-key-map))
         (query (forgejo-filter-read current completions))
         (filters (forgejo-filter-parse
                   query forgejo-filter--notification-prefix-map)))
    (setq forgejo-notification--filters filters)
    (forgejo-notification--render forgejo-notification--host)))

(defun forgejo-notification-clear-filters ()
  "Clear all notification filters."
  (interactive)
  (setq forgejo-notification--filters nil)
  (forgejo-notification--render forgejo-notification--host))

;;; Actions

(defun forgejo-notification-view-at-point ()
  "Jump to the issue or PR for the notification at point."
  (interactive)
  (when-let* ((id-cell (tabulated-list-get-id))
              (entry (tabulated-list-get-entry))
              (notif-id (car id-cell))
              (subject-url (cdr id-cell))
              (number (forgejo-notification--subject-number subject-url)))
    (let* ((repo-str (aref entry 0))
           (type (aref entry 1))
           (parts (split-string repo-str "/"))
           (owner (nth 0 parts))
           (repo (nth 1 parts)))
      ;; Mark as read locally and on the server
      (forgejo-db-mark-notification-read forgejo-notification--host notif-id)
      (forgejo-api-patch (format "notifications/threads/%d" notif-id))
      (forgejo-notification--render forgejo-notification--host)
      ;; Navigate
      (forgejo-with-host forgejo-repo--host
        (if (string= type "Pull")
            (forgejo-pull-view owner repo number)
          (forgejo-issue-view owner repo number))))))

(defun forgejo-notification-mark-read-at-point ()
  "Mark the notification at point as read."
  (interactive)
  (when-let* ((id-cell (tabulated-list-get-id))
              (notif-id (car id-cell)))
    (forgejo-db-mark-notification-read forgejo-notification--host notif-id)
    (forgejo-api-put
     (format "notifications/threads/%d" notif-id)
     nil
     (lambda (_data _headers)
       (message "Notification %d marked as read" notif-id)))
    (forgejo-notification--render forgejo-notification--host)))

(defun forgejo-notification-mark-all-read ()
  "Mark all notifications as read."
  (interactive)
  (when (y-or-n-p "Mark all notifications as read? ")
    (let ((host forgejo-notification--host))
      (forgejo-api-put
       "notifications"
       nil
       (lambda (_data _headers)
         (forgejo-db-clear-notifications host)
         (message "All notifications marked as read")
         (forgejo-notification--refresh-list-buffer host))))))

(provide 'forgejo-notification)
;;; forgejo-notification.el ends here
