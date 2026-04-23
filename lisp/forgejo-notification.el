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

;; Notification polling, activity tracking, and mode-line indicator
;; for Forgejo instances.  Follows the hook-based architecture of
;; jabber.el (jabber-activity.el / jabber-modeline.el) adapted from
;; event-driven XMPP to timer-polled REST.
;;
;; Currently polls the Forgejo notification API and caches results in
;; the local DB.  The hook system and DB table are designed to also
;; support future local watch rules (custom per-repo/label filters
;; that generate notifications from the sync process).
;;
;; Enable `forgejo-modeline-mode' to start polling and display the
;; unread count in the mode line.  Use `forgejo-notification-list'
;; to browse notifications in a tabulated-list buffer.

;;; Code:

(require 'cl-lib)
(require 'url-parse)
(require 'forgejo)
(require 'forgejo-api)
(require 'forgejo-tl)
(require 'forgejo-db)
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

;;; Faces

(defface forgejo-notification-face
  '((t :inherit warning))
  "Face for the mode-line notification indicator."
  :group 'forgejo)

;;; State

(defvar forgejo-notification--timer nil
  "Timer for periodic notification polling.")

(defvar forgejo-notification--unread-count 0
  "Cached count of unread notifications.")

(defvar forgejo-notification-mode-string ""
  "Mode-line string showing unread notification count.")

(defvar forgejo-notification--eval-form
  '(:eval forgejo-notification-mode-string)
  "Form added to `global-mode-string' for the mode-line indicator.")

;;; Polling

(defun forgejo-notification--poll ()
  "Fetch unread notifications and update state.
Saves to DB, updates mode-line, runs hooks on new notifications,
and refreshes the list buffer if visible."
  (let ((host (url-host (url-generic-parse-url forgejo-host))))
    (forgejo-api-get
     "notifications"
     '(("status-types" . "unread"))
     (lambda (data _headers)
       (let ((old-count forgejo-notification--unread-count))
         (forgejo-db-save-notifications host data)
         (setq forgejo-notification--unread-count (length data))
         (forgejo-notification--mode-line-update)
         (when (> forgejo-notification--unread-count old-count)
           (run-hook-with-args 'forgejo-notification-hooks data))
         (forgejo-notification--refresh-list-buffer host))))))

(defun forgejo-notification--refresh-list-buffer (host)
  "Re-render the notification list buffer for HOST if visible."
  (when-let* ((buf (get-buffer "*forgejo-notifications*")))
    (when (get-buffer-window buf)
      (with-current-buffer buf
        (forgejo-notification--render host)))))

;;; Mode-line

(defun forgejo-notification--mode-line-update ()
  "Update the mode-line notification string."
  (setq forgejo-notification-mode-string
        (if (> forgejo-notification--unread-count 0)
            (propertize (format " [F:%d]" forgejo-notification--unread-count)
                        'face 'forgejo-notification-face)
          ""))
  (force-mode-line-update 'all))

;;; Global minor mode

;;;###autoload
(define-minor-mode forgejo-modeline-mode
  "Toggle Forgejo notification indicator in the mode line.
Polls the Forgejo API periodically for unread notifications."
  :global t
  :lighter nil
  :group 'forgejo
  (if forgejo-modeline-mode
      (progn
        (unless (member forgejo-notification--eval-form global-mode-string)
          (push forgejo-notification--eval-form global-mode-string))
        (setq forgejo-notification--timer
              (run-with-timer 0 forgejo-notification-poll-interval
                              #'forgejo-notification--poll))
        (add-hook 'kill-emacs-hook #'forgejo-notification--cleanup))
    (forgejo-notification--cleanup)))

(defun forgejo-notification--cleanup ()
  "Cancel the polling timer and clear mode-line state."
  (when forgejo-notification--timer
    (cancel-timer forgejo-notification--timer)
    (setq forgejo-notification--timer nil))
  (setq forgejo-notification--unread-count 0
        forgejo-notification-mode-string "")
  (setq global-mode-string
        (delete forgejo-notification--eval-form global-mode-string))
  (remove-hook 'kill-emacs-hook #'forgejo-notification--cleanup)
  (force-mode-line-update 'all))

;;; Notification list buffer

(defvar-local forgejo-notification--host nil
  "Hostname for the current notification list buffer.")

(defvar forgejo-notification-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'forgejo-notification-view-at-point)
    (define-key map (kbd "r") #'forgejo-notification-mark-read-at-point)
    (define-key map (kbd "R") #'forgejo-notification-mark-all-read)
    (define-key map (kbd "g") #'forgejo-notification-list-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `forgejo-notification-list-mode'.")

(define-derived-mode forgejo-notification-list-mode tabulated-list-mode
  "Forgejo Notifications"
  "Major mode for browsing Forgejo notifications."
  :group 'forgejo
  (setq tabulated-list-padding 1
        tabulated-list-format
        (vector '("Repo" 25 t)
                '("Type" 8 nil)
                '("Title" 50 t)
                '("Status" 8 nil)
                '("Updated" 12 t)))
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
  (let ((rows (forgejo-db-get-notifications host)))
    (setq tabulated-list-entries (forgejo-notification--entries rows))
    (forgejo-tl-print)
    (goto-char (point-min))))

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
      (forgejo-notification--render host)
      (switch-to-buffer buf))
    (forgejo-notification--poll)))

(defun forgejo-notification-list-refresh ()
  "Refresh the notification list."
  (interactive)
  (forgejo-notification--poll))

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
      (forgejo-api-put (format "notifications/threads/%d" notif-id))
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
         (setq forgejo-notification--unread-count 0)
         (forgejo-notification--mode-line-update)
         (message "All notifications marked as read")
         (forgejo-notification--refresh-list-buffer host))))))

(provide 'forgejo-notification)
;;; forgejo-notification.el ends here
