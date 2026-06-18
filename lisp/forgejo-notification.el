;;; forgejo-notification.el --- Server-side notifications for Forgejo  -*- lexical-binding: t; -*-

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

;; Browse and manage server-side Forgejo notifications.
;;
;; Notification metadata is stored in a thin DB table that points to
;; issues in the main `issues' table.  Referenced issues are fetched
;; eagerly during sync.  Views render from DB (instant after first sync).
;;
;; Use `forgejo-notification-list' to open the notification buffer.

;;; Code:

(require 'cl-lib)
(require 'url-parse)
(require 'keymap-popup)
(require 'forgejo)
(require 'forgejo-api)
(require 'forgejo-db)
(require 'forgejo-tl)
(require 'forgejo-buffer)
(require 'forgejo-filter)
(require 'forgejo-view)
(require 'forgejo-utils)

(declare-function forgejo-issue-view "forgejo-issue.el"
                  (owner repo number))
(declare-function forgejo-pull-view "forgejo-pull.el"
                  (owner repo number))

(defvar forgejo-repo--host)

(defcustom forgejo-notification-default-filter "status:unread"
  "Default filter for `forgejo-notification-list'.
Supported prefixes: status: (unread/read/pinned/all), type: (issue/pull)."
  :type 'string
  :group 'forgejo)

(defcustom forgejo-notification-poll-interval 300
  "Seconds between notification polls."
  :type 'integer
  :group 'forgejo)

(defcustom forgejo-notification-hooks nil
  "Hook run when new unread notifications arrive during polling.
Each function receives one argument: a list of unread notification alists."
  :type 'hook
  :group 'forgejo)

;;; Poll state

(defvar forgejo-notification--timer nil
  "Timer for periodic notification polling.")

;;; Buffer-local state

(defvar-local forgejo-notification--host nil
  "Hostname for the current notification buffer.")

(defvar-local forgejo-notification--host-url nil
  "Full URL for the current notification buffer.")

(defvar-local forgejo-notification--filters nil
  "Current filter plist.  Keys: :status, :subject-type.")

;;; Entry builder

(defun forgejo-notification--build-entries (items)
  "Convert notification ITEMS to `tabulated-list-entries'.
Uses the same column format as the watch list.
Each entry key is the notification thread ID."
  (mapcar
   (lambda (item)
     (let* ((thread-id (alist-get 'notification_thread_id item))
            (is-pull (alist-get 'pull_request item))
            (type (if is-pull "PR" "Issue"))
            (owner (or (alist-get 'notification_owner item) ""))
            (repo (or (alist-get 'notification_repo item) ""))
            (number (or (alist-get 'number item) 0))
            (short-ref (format "%s#%d" repo number))
            (full-ref (format "%s/%s#%d" owner repo number)))
       (list thread-id
             (vector
              (propertize type 'face (if is-pull 'success 'warning))
              (propertize short-ref 'face 'forgejo-number-face
                          'forgejo-full-ref full-ref)
              (forgejo-buffer--format-state (or (alist-get 'state item) ""))
              (or (alist-get 'title item) "")
              (forgejo-buffer--format-labels (alist-get 'labels item))
              (propertize (or (forgejo-buffer--login (alist-get 'user item)) "")
                          'face 'forgejo-comment-author-face)
              (propertize (forgejo-buffer--relative-time
                           (or (alist-get 'notification_updated_at item)
                               (alist-get 'updated_at item) ""))
                          'face 'shadow
                          'forgejo-timestamp
                          (or (alist-get 'notification_updated_at item)
                              (alist-get 'updated_at item) ""))))))
   items))

;;; URL parsing

(defun forgejo-notification--parse-subject-url (url)
  "Parse subject URL into (OWNER REPO NUMBER), or nil.
Handles API URLs like /api/v1/repos/OWNER/REPO/issues/N."
  (when (and url (string-match
                  "/repos/\\([^/]+\\)/\\([^/]+\\)/\\(?:issues\\|pulls\\)/\\([0-9]+\\)"
                  url))
    (list (match-string 1 url)
          (match-string 2 url)
          (string-to-number (match-string 3 url)))))

;;; Notification -> DB row conversion

(defun forgejo-notification--to-db-row (notif)
  "Convert API notification NOTIF to a DB row alist."
  (when-let* ((subject (alist-get 'subject notif))
              (type (alist-get 'type subject))
              ((member type '("Issue" "Pull")))
              (parsed (forgejo-notification--parse-subject-url
                       (alist-get 'url subject))))
    (pcase-let ((`(,owner ,repo ,number) parsed))
      `((thread_id . ,(alist-get 'id notif))
        (owner . ,owner)
        (repo . ,repo)
        (number . ,number)
        (unread . ,(if (eq (alist-get 'unread notif) t) 1 0))
        (pinned . ,(if (eq (alist-get 'pinned notif) t) 1 0))
        (updated_at . ,(alist-get 'updated_at notif))))))

;;; Sync

(defvar forgejo-notification-fetch-batch-size 10
  "Number of missing issues to fetch before pausing.
A 3-second pause is inserted between batches to avoid triggering
infrastructure-level rate limits.")

(defvar forgejo-notification-fetch-max-per-sync 30
  "Max missing issues to fetch per sync cycle.
Remaining ones are picked up on subsequent polls.")

(defun forgejo-notification--sync (host-url host &optional callback)
  "Fetch notifications from HOST-URL for HOST and save them to the DB.
Also fetch any missing issues.  CALLBACK is called with no args when
done."
  (let ((since (forgejo-db-get-sync-time host "" "" "notifications"))
        (params (list '("limit" . "50") '("all" . "true"))))
    (when since
      (push (cons "since" since) params))
    (forgejo-api-get-paged
     host-url "notifications" params
     ;; per-page
     (lambda (page-data _headers page-num)
       (forgejo-notification--process-page host-url host page-data)
       (when (= page-num 1)
         (forgejo-notification--refresh-buffer host)))
     ;; done
     (lambda (_all-data _headers)
       (forgejo-db-set-sync-time
        host "" "" "notifications"
        (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
       (forgejo-notification--refresh-buffer host)
       (when callback (funcall callback))))))

(defun forgejo-notification--process-page (host-url host notifications)
  "Process a page of NOTIFICATIONS for HOST, saving them to the DB.
Fetch any missing issues.  HOST-URL is the instance.  Fire
`forgejo-notification-hooks' for unread items in this page."
  (let ((rows (cl-loop for n in notifications
                       for row = (forgejo-notification--to-db-row n)
                       when row collect row))
        (missing nil))
    (when rows
      (forgejo-db-save-notifications host rows)
      (dolist (row rows)
        (let-alist row
          (unless (forgejo-db-get-issue host .owner .repo .number)
            (push (list .owner .repo .number) missing))))
      (when missing
        (forgejo-notification--fetch-missing
         host-url host
         (seq-take (nreverse missing)
                   forgejo-notification-fetch-max-per-sync)))
      (when forgejo-notification-hooks
        (let ((unread (cl-remove-if-not
                       (lambda (r) (= (alist-get 'unread r) 1)) rows)))
          (when unread
            (run-hook-with-args 'forgejo-notification-hooks unread)))))))

(defun forgejo-notification--fetch-missing (host-url host missing &optional count)
  "Fetch MISSING issues ((OWNER REPO NUMBER) ...) sequentially.
HOST-URL is the instance.  HOST is the hostname.  Skip items that
fail (404, private, etc.) and continue the chain.  Pause between
batches of `forgejo-notification-fetch-batch-size'.  COUNT tracks how
many have been processed."
  (when missing
    (let ((count (or count 0)))
      (if (and (> count 0)
               (= (mod count forgejo-notification-fetch-batch-size) 0))
          (run-with-timer 3 nil
                          #'forgejo-notification--fetch-missing
                          host-url host missing (1+ count))
        (pcase-let ((`(,owner ,repo ,number) (car missing))
                    (rest (cdr missing)))
          (forgejo-api-get
           host-url
           (format "repos/%s/%s/issues/%d" owner repo number) nil
           (lambda (data _headers)
             (when data
               (forgejo-db-save-issues host owner repo (list data)))
             (forgejo-notification--fetch-missing host-url host rest (1+ count)))
           :error-callback
           (lambda (_err)
             (forgejo-notification--fetch-missing
              host-url host rest (1+ count)))))))))

(defun forgejo-notification--full-sync (host-url host &optional callback)
  "Clear notification DB for HOST and re-sync everything on HOST-URL.
CALLBACK is called with no args when done."
  (forgejo-db-clear-notifications host)
  (forgejo-notification--sync host-url host callback))

;;; Render

(defun forgejo-notification--render (host)
  "Render notifications for HOST into the current buffer."
  (let* ((items (forgejo-db-get-notifications
                 host forgejo-notification--filters))
         (entries (forgejo-notification--build-entries items)))
    (setq tabulated-list-format (forgejo-view--list-format
                                 forgejo-filter-notification-columns)
          tabulated-list-entries entries)
    (tabulated-list-init-header)
    (forgejo-tl-print t)))

(defun forgejo-notification--refresh-buffer (host)
  "Re-render the notification buffer for HOST if it exists."
  (when-let* ((buf (get-buffer "*forgejo-notifications*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (forgejo-notification--render host)))))

;;; Mode and keymap

(keymap-popup-define forgejo-notification-list-mode-map
  :description (lambda ()
                 (let ((host (bound-and-true-p forgejo-notification--host))
                       (filter-str (and forgejo-notification--filters
                                        (forgejo-filter-serialize
                                         forgejo-notification--filters))))
                   (concat "Notifications"
                           (when host (format " %s" host))
                           (when (and filter-str
                                      (not (string-empty-p filter-str)))
                             (concat " " filter-str)))))
  :parent forgejo-tl-list-mode-map
  :group "Actions"
  "RET" ("View" forgejo-notification-view-at-point)
  "r" ("Mark read" forgejo-notification-mark-read)
  "u" ("Mark unread" forgejo-notification-mark-unread)
  "p" ("Toggle pin" forgejo-notification-toggle-pin)
  "R" ("Mark all read" forgejo-notification-mark-all-read)
  "U" ("Toggle subscription" forgejo-view-toggle-subscription)
  "b" ("Open in browser" forgejo-notification-browse-at-point)
  "g" ("Refresh" forgejo-notification-refresh)
  :group "Navigate"
  "l" ("Filter" forgejo-notification-filter))

(define-derived-mode forgejo-notification-list-mode tabulated-list-mode
  "Forgejo Notifications"
  "Major mode for browsing Forgejo notifications."
  :group 'forgejo
  (setq tabulated-list-padding 1
        tabulated-list-format (forgejo-view--list-format
                               forgejo-filter-notification-columns)
        tabulated-list-sort-key '("Updated" . t))
  (tabulated-list-init-header)
  (run-hook-with-args 'forgejo-buffer-setup-functions (current-buffer)))

;;; Entry point

;;;###autoload
(defun forgejo-notification-list ()
  "Browse server-side notifications.
Renders instantly from DB, then syncs in background."
  (interactive)
  (let* ((host-url (forgejo--resolve-host))
         (host (url-host (url-generic-parse-url host-url)))
         (buf (get-buffer-create "*forgejo-notifications*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'forgejo-notification-list-mode)
        (forgejo-notification-list-mode)
        (setq forgejo-notification--filters
              (forgejo-notification--parse-filter
               forgejo-notification-default-filter)))
      (setq forgejo-notification--host host
            forgejo-notification--host-url host-url
            forgejo-repo--host host-url)
      (forgejo-notification--render host)
      (forgejo-notification--sync host-url host))
    (switch-to-buffer buf)))

;;; Actions

(defun forgejo-notification--thread-id-at-point ()
  "Return the notification thread ID at point."
  (tabulated-list-get-id))

(defun forgejo-notification--parse-ref-at-point ()
  "Parse owner/repo/number from the Ref column at point."
  (when-let* ((entry (tabulated-list-get-entry))
              (ref (aref entry 1))
              (full (get-text-property 0 'forgejo-full-ref ref)))
    (when (string-match "\\`\\([^/]+\\)/\\([^#]+\\)#\\([0-9]+\\)\\'" full)
      (list (match-string 1 full)
            (match-string 2 full)
            (string-to-number (match-string 3 full))))))

(defun forgejo-notification-view-at-point ()
  "Navigate to the subject of the notification at point.
Marks the notification as read on both the local DB and the server."
  (interactive)
  (when-let* ((entry (tabulated-list-get-entry))
              (parsed (forgejo-notification--parse-ref-at-point))
              (thread-id (forgejo-notification--thread-id-at-point)))
    (when (= (forgejo-db-get-notification-unread
              forgejo-notification--host thread-id)
             1)
      (forgejo-notification--set-status thread-id "read"))
    (pcase-let ((`(,owner ,repo ,number) parsed))
      (let ((type (aref entry 0)))
        (if (string= type "PR")
            (forgejo-pull-view owner repo number)
          (forgejo-issue-view owner repo number))))))

(defun forgejo-notification-browse-at-point ()
  "Open the notification subject in the browser."
  (interactive)
  (when-let* ((parsed (forgejo-notification--parse-ref-at-point)))
    (pcase-let ((`(,owner ,repo ,number) parsed))
      (browse-url (format "https://%s/%s/%s/issues/%d"
                          forgejo-notification--host owner repo number)))))

;;; Mark read/unread/pin

(defun forgejo-notification--set-status (thread-id to-status)
  "Update notification THREAD-ID to TO-STATUS locally and on server."
  (let ((unread (if (string= to-status "unread") 1 0))
        (pinned (if (string= to-status "pinned") 1 0))
        (host forgejo-notification--host))
    (forgejo-db-set-notification-status host thread-id unread pinned)
    (forgejo-notification--render host)
    (forgejo-api--request
     forgejo-notification--host-url "PATCH"
     (format "notifications/threads/%d" thread-id)
     `(("to-status" . ,to-status)) nil
     (lambda (_data _headers)
       (message "Notification %d marked %s" thread-id to-status))
     :error-callback
     (lambda (_err)
       (message "Failed to mark notification %d as %s" thread-id to-status)))))

(defun forgejo-notification-mark-read ()
  "Mark notification at point as read."
  (interactive)
  (when-let* ((id (forgejo-notification--thread-id-at-point)))
    (forgejo-notification--set-status id "read")))

(defun forgejo-notification-mark-unread ()
  "Mark notification at point as unread."
  (interactive)
  (when-let* ((id (forgejo-notification--thread-id-at-point)))
    (forgejo-notification--set-status id "unread")))

(defun forgejo-notification-toggle-pin ()
  "Toggle pinned state of notification at point."
  (interactive)
  (when-let* ((id (forgejo-notification--thread-id-at-point))
              (pinned (forgejo-db-get-notification-pinned
                       forgejo-notification--host id)))
    (forgejo-notification--set-status
     id (if (= pinned 1) "read" "pinned"))))

(defun forgejo-notification-mark-all-read ()
  "Mark all visible notifications as read.
Server PUT marks all notifications read (not just visible); local DB
updates only visible IDs.  Next sync reconciles the difference."
  (interactive)
  (let ((ids (mapcar #'car tabulated-list-entries))
        (host forgejo-notification--host))
    (forgejo-db-set-all-notifications-read host ids)
    (forgejo-notification--render host)
    (forgejo-api--request
     forgejo-notification--host-url "PUT" "notifications"
     '(("to-status" . "read")) nil
     (lambda (_data _headers)
       (message "All notifications marked read")))))

;;; Filter

(defun forgejo-notification--parse-filter (query)
  "Parse QUERY string into a filter plist."
  (let ((tokens (split-string (or query "") " " t))
        filters)
    (dolist (token tokens)
      (when (string-match "\\`\\([^:]+\\):\\(.*\\)\\'" token)
        (pcase (match-string 1 token)
          ("status" (setq filters (plist-put filters :status
                                             (match-string 2 token))))
          ("type" (setq filters (plist-put filters :subject-type
                                           (match-string 2 token)))))))
    filters))

(defun forgejo-notification--serialize-filter (filters)
  "Serialize FILTERS plist to a query string."
  (let (parts)
    (when-let* ((status (plist-get filters :status)))
      (push (concat "status:" status) parts))
    (when-let* ((type (plist-get filters :subject-type)))
      (push (concat "type:" type) parts))
    (string-join (nreverse parts) " ")))

(defun forgejo-notification-filter ()
  "Change notification filter and re-render from DB."
  (interactive)
  (let* ((current (forgejo-notification--serialize-filter
                   forgejo-notification--filters))
         (completions '((status . ("unread" "read" "pinned" "all"))
                        (type . ("issue" "pull"))))
         (query (forgejo-utils-read-filter current completions)))
    (setq forgejo-notification--filters
          (forgejo-notification--parse-filter query))
    (forgejo-notification--render forgejo-notification--host)))

;;; Refresh

(defun forgejo-notification-refresh (&optional full)
  "Sync notifications from server, then re-render.
With prefix arg FULL, clear and re-fetch everything."
  (interactive "P")
  (let ((host-url forgejo-notification--host-url)
        (host forgejo-notification--host))
    (message "Syncing notifications%s..." (if full " (full)" ""))
    (if full
        (forgejo-notification--full-sync host-url host)
      (forgejo-notification--sync host-url host))))

;;; Count helper

(defun forgejo-notification-count (host-url callback)
  "Fetch unread notification count from HOST-URL.
CALLBACK receives the count as an integer."
  (forgejo-api-get host-url "notifications/new" nil
                   (lambda (data _headers)
                     (funcall callback (or (alist-get 'new data) 0)))))

;;; Polling

(defun forgejo-notification--poll ()
  "Poll notifications for all configured hosts."
  (dolist (entry forgejo-hosts)
    (let* ((host-url (car entry))
           (host (url-host (url-generic-parse-url host-url))))
      (forgejo-notification--sync host-url host))))

;;;###autoload
(define-minor-mode forgejo-notification-mode
  "Toggle Forgejo notification polling.
Polls notifications periodically and runs `forgejo-notification-hooks'
when new unread items arrive."
  :global t
  :group 'forgejo
  (if forgejo-notification-mode
      (progn
        (setq forgejo-notification--timer
              (run-with-timer 0 forgejo-notification-poll-interval
                              #'forgejo-notification--poll))
        (add-hook 'kill-emacs-hook #'forgejo-notification--cleanup))
    (forgejo-notification--cleanup)))

(defun forgejo-notification--cleanup ()
  "Cancel the notification polling timer."
  (when forgejo-notification--timer
    (cancel-timer forgejo-notification--timer)
    (setq forgejo-notification--timer nil))
  (remove-hook 'kill-emacs-hook #'forgejo-notification--cleanup))

(provide 'forgejo-notification)
;;; forgejo-notification.el ends here
