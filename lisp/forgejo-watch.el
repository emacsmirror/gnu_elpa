;;; forgejo-watch.el --- Watch rules and polling for Forgejo  -*- lexical-binding: t; -*-

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

;; Per-repo watch rules with timer-based polling for Forgejo.
;;
;; Users define filter rules via `forgejo-watch-rules'.  On each poll
;; interval, the system fetches matching issues/PRs from the API with
;; incremental sync (since=), saves to the local DB, and fires hooks
;; for new items.
;;
;; Enable `forgejo-watch-mode' to start polling.
;; Use `forgejo-watch-list' to browse watched items.

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

(defcustom forgejo-watch-poll-interval 300
  "Seconds between watch rule polls."
  :type 'integer
  :group 'forgejo)

(defcustom forgejo-watch-rules nil
  "Per-repo watch rules.
Each element is either a bare string \"owner/repo\" (watch everything)
or (\"owner/repo\" . \"filter-query\") where the filter query uses
the same syntax as the interactive filter prompt.
Example: ((\"guix/guix\" . \"state:open author:thanosapollo\")
          (\"guix/guix\" . \"state:open label:team-vcs\")
          \"thanosapollo/forgejo.el\")"
  :type '(repeat (choice string (cons string string)))
  :group 'forgejo)

(defcustom forgejo-watch-hooks nil
  "Hook run when new watch items arrive.
Each function receives one argument: the list of new issue/PR alists."
  :type 'hook
  :group 'forgejo)

;;; State

(defvar forgejo-watch--timer nil
  "Timer for periodic watch polling.")

;;; Polling

(defun forgejo-watch--poll ()
  "Poll watch rules for new items."
  (let ((host (url-host (url-generic-parse-url forgejo-host))))
    (forgejo-watch--poll-rules host)))

;;; Watch rules

(defun forgejo-watch--poll-rules (host)
  "Poll each watch rule in `forgejo-watch-rules' for HOST."
  (dolist (rule forgejo-watch-rules)
    (forgejo-watch--poll-rule host rule)))

(defun forgejo-watch--poll-rule (host rule)
  "Poll a single watch RULE for HOST.
RULE is \"owner/repo\" or (\"owner/repo\" . \"filter-query\")."
  (let* ((repo-key (if (stringp rule) rule (car rule)))
         (query (if (stringp rule) nil (cdr rule)))
         (parts (split-string repo-key "/"))
         (owner (nth 0 parts))
         (repo (nth 1 parts))
         (filters (forgejo-filter-parse query))
         (since (forgejo-db-get-sync-time host owner repo "watch"))
         (api-filters (if since
                         (plist-put (copy-sequence filters) :since since)
                       filters))
         (endpoint (format "repos/%s/%s/issues" owner repo))
         (params (forgejo-filter-build-params nil api-filters)))
    (forgejo-api-get-paged
     endpoint params
     (lambda (page-data _headers _page-num)
       (when page-data
         (forgejo-db-save-issues host owner repo page-data)))
     (lambda (all-data _headers)
       (when all-data
         (forgejo-db-set-sync-time
          host owner repo "watch"
          (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
         (run-hook-with-args 'forgejo-watch-hooks all-data))))))

(defun forgejo-watch--refresh-list-buffer (host)
  "Re-render the notification list buffer for HOST if visible."
  (when-let* ((buf (get-buffer "*forgejo-watch*")))
    (when (get-buffer-window buf)
      (with-current-buffer buf
        (forgejo-watch--render host)))))

;;; Global minor mode

;;;###autoload
(define-minor-mode forgejo-watch-mode
  "Toggle Forgejo notification polling.
Polls watch rules periodically for new items
and runs `forgejo-watch-hooks' when new ones arrive."
  :global t
  :lighter nil
  :group 'forgejo
  (if forgejo-watch-mode
      (progn
        (setq forgejo-watch--timer
              (run-with-timer 0 forgejo-watch-poll-interval
                              'forgejo-watch--poll))
        (add-hook 'kill-emacs-hook #'forgejo-watch--cleanup))
    (forgejo-watch--cleanup)))

(defun forgejo-watch--cleanup ()
  "Cancel the polling timer."
  (when forgejo-watch--timer
    (cancel-timer forgejo-watch--timer)
    (setq forgejo-watch--timer nil))
  (remove-hook 'kill-emacs-hook #'forgejo-watch--cleanup))

;;; Notification list buffer

(defvar-local forgejo-watch--host nil
  "Hostname for the current notification list buffer.")

(defvar-local forgejo-watch--filters nil
  "Current filter plist for the notification list.")

(declare-function forgejo-watch-actions "forgejo-transient.el" ())

(defvar forgejo-watch-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'forgejo-watch-view-at-point)
    (define-key map (kbd "r") #'forgejo-watch-mark-read-at-point)
    (define-key map (kbd "A") #'forgejo-watch-mark-all-read)
    (define-key map (kbd "b") #'forgejo-watch-browse-at-point)
    (define-key map (kbd "l") #'forgejo-watch-filter)
    (define-key map (kbd "C") #'forgejo-watch-clear-filters)
    (define-key map (kbd "g") #'forgejo-watch-list-refresh)
    (define-key map (kbd "h") #'forgejo-watch-actions)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `forgejo-watch-list-mode'.")

(define-derived-mode forgejo-watch-list-mode tabulated-list-mode
  "Forgejo Watch"
  "Major mode for browsing Forgejo watch items."
  :group 'forgejo
  (setq tabulated-list-padding 1
        tabulated-list-format (forgejo-filter-list-format
                               forgejo-filter-notification-columns))
  (tabulated-list-init-header))

(defun forgejo-watch--render (host)
  "Render watch items from DB for HOST into the current buffer."
  (let* ((items (forgejo-filter-query-watch
                 host forgejo-watch-rules
                 forgejo-watch--filters))
         (entries (forgejo-filter-notification-entries items)))
    (setq tabulated-list-format (forgejo-filter-list-format
                                 forgejo-filter-notification-columns)
          tabulated-list-entries entries)
    (tabulated-list-init-header)
    (forgejo-tl-print t)))

;;;###autoload
(defun forgejo-watch-list ()
  "Browse watched items.
Shows unread items from `forgejo-watch-rules'."
  (interactive)
  (let* ((host (url-host (url-generic-parse-url forgejo-host)))
         (buf (get-buffer-create "*forgejo-watch*")))
    (with-current-buffer buf
      (forgejo-watch-list-mode)
      (setq forgejo-watch--host host
            forgejo-repo--host forgejo-host
            forgejo-watch--filters '(:read "no"))
      (forgejo-watch--render host)
      (switch-to-buffer buf))))

(defun forgejo-watch-list-refresh ()
  "Fetch updates for watch rules, then re-render."
  (interactive)
  (let ((host forgejo-watch--host))
    (forgejo-with-host forgejo-repo--host
      (forgejo-watch--poll-rules host))
    (forgejo-watch--render host)))

;;; Filtering

(defun forgejo-watch-filter ()
  "Filter the watch list."
  (interactive)
  (let* ((current (forgejo-filter-serialize
                   forgejo-watch--filters
                   forgejo-filter--watch-key-map))
         (completions `((state . ("open" "closed"))
                        (read . ("yes" "no"))
                        (type . ("pr" "issue"))
                        (author . nil)
                        (label . nil)
                        (search . nil)))
         (query (forgejo-filter-read current completions))
         (filters (forgejo-filter-parse
                   query forgejo-filter--watch-prefix-map)))
    (setq forgejo-watch--filters filters)
    (forgejo-watch--render forgejo-watch--host)))

(defun forgejo-watch-clear-filters ()
  "Clear all notification filters."
  (interactive)
  (setq forgejo-watch--filters nil)
  (forgejo-watch--render forgejo-watch--host))

;;; Actions

(defun forgejo-watch--parse-ref (ref)
  "Parse owner/repo/number from REF.
Reads the full ref from the `forgejo-full-ref' text property."
  (let ((full (or (get-text-property 0 'forgejo-full-ref ref) ref)))
    (when (string-match "\\`\\([^/]+\\)/\\([^#]+\\)#\\([0-9]+\\)\\'" full)
      (list (match-string 1 full)
            (match-string 2 full)
            (string-to-number (match-string 3 full))))))

(defun forgejo-watch-view-at-point ()
  "Jump to the issue or PR for the notification at point."
  (interactive)
  (when-let* ((entry (tabulated-list-get-entry))
              (type (aref entry 0))
              (ref (aref entry 1))
              (parsed (forgejo-watch--parse-ref ref)))
    (let ((owner (nth 0 parsed))
          (repo (nth 1 parsed))
          (number (nth 2 parsed)))
      (forgejo-db-mark-read forgejo-watch--host owner repo number)
      (forgejo-watch--render forgejo-watch--host)
      (forgejo-with-host forgejo-repo--host
        (if (string= type "PR")
            (forgejo-pull-view owner repo number)
          (forgejo-issue-view owner repo number))))))

(defun forgejo-watch-mark-read-at-point ()
  "Mark the notification at point as read."
  (interactive)
  (when-let* ((entry (tabulated-list-get-entry))
              (ref (aref entry 1))
              (parsed (forgejo-watch--parse-ref ref)))
    (forgejo-db-mark-read forgejo-watch--host
                          (nth 0 parsed) (nth 1 parsed) (nth 2 parsed))
    (forgejo-watch--render forgejo-watch--host)))

(defun forgejo-watch-mark-all-read ()
  "Mark all visible watch items as read."
  (interactive)
  (dolist (entry tabulated-list-entries)
    (let* ((cols (cadr entry))
           (ref (aref cols 1))
           (parsed (forgejo-watch--parse-ref ref)))
      (when parsed
        (forgejo-db-mark-read forgejo-watch--host
                              (nth 0 parsed) (nth 1 parsed) (nth 2 parsed)))))
  (forgejo-watch--render forgejo-watch--host))

(defun forgejo-watch-browse-at-point ()
  "Open the issue or PR at point in the browser."
  (interactive)
  (when-let* ((entry (tabulated-list-get-entry))
              (ref (aref entry 1))
              (parsed (forgejo-watch--parse-ref ref)))
    (forgejo-utils-browse-issue (nth 0 parsed) (nth 1 parsed) (nth 2 parsed))))

(provide 'forgejo-watch)
;;; forgejo-watch.el ends here
