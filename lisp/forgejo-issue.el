;;; forgejo-issue.el --- Issue list and detail views  -*- lexical-binding: t; -*-

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

;; Issue list view using `tabulated-list-mode' with `forgejo-tl' for
;; performance, and issue detail view using EWOC for the timeline.

;;; Code:

(require 'cl-lib)
(require 'url-parse)
(require 'forgejo)
(require 'forgejo-tl)
(require 'forgejo-view)
(require 'forgejo-buffer)
(require 'forgejo-filter)
(require 'forgejo-utils)
(require 'forgejo-api)
(require 'forgejo-db)

(declare-function forgejo-repo-read "forgejo-repo.el" ())

(defvar forgejo-default-sort)
(defvar forgejo-timeline-page-size)
(defvar forgejo-issue-default-filter)
(defvar forgejo-repo--host)
(defvar forgejo-repo--owner)
(defvar forgejo-repo--name)

;;; ---- Issue list view ----

(defvar-local forgejo-issue--filters nil
  "Plist of current filter state.
Keys: :state :labels :milestone :author :query :page")

(defvar-local forgejo-issue--total-count nil
  "Total number of issues matching current filters (from API header).")

(defvar forgejo-issue-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'forgejo-issue-view-at-point)
    (define-key map (kbd "S") #'forgejo-tl-sort)
    (define-key map (kbd "c") #'forgejo-issue-create)
    (define-key map (kbd "g") #'forgejo-issue-refresh)
    (define-key map (kbd "l") #'forgejo-issue-filter)
    (define-key map (kbd "C") #'forgejo-issue-clear-filters)
    (define-key map (kbd "x") #'forgejo-view-toggle-state)
    (define-key map (kbd "b") #'forgejo-issue-browse-at-point)
    map)
  "Keymap for `forgejo-issue-list-mode'.")

(define-derived-mode forgejo-issue-list-mode tabulated-list-mode
  "Forgejo Issues"
  "Major mode for browsing Forgejo issues."
  :group 'forgejo
  (setq tabulated-list-padding 1
        tabulated-list-format (forgejo-view--list-format
                               forgejo-filter-list-columns)
        tabulated-list-sort-key '("#" . t))
  (tabulated-list-init-header))

;;; API interaction

(defun forgejo-issue--build-params (filters)
  "Build API query params from FILTERS plist."
  (forgejo-filter-build-params "issues" filters
                               forgejo-default-sort
                               (forgejo-api-default-limit)))

;;; Cache-first rendering

(defun forgejo-issue--render-from-db (buf-name host-url host owner repo filters)
  "Render cached issues into BUF-NAME from the DB.
HOST-URL is the full instance URL.  HOST is the hostname."
  (forgejo-view--render-from-db buf-name host-url host owner repo filters
                                #'forgejo-filter-query-issues
                                'forgejo-issue-list-mode))

(defun forgejo-issue--sync (host-url host owner repo filters buf-name
                                 &optional force)
  "Fetch issues from API and update DB, then re-render BUF-NAME.
HOST-URL is the instance.  HOST is the hostname.
When FORCE is nil, use incremental sync via the `since' parameter.
When FORCE is non-nil, fetch all and mark missing issues as closed."
  (let* ((since (unless force
                  (forgejo-db-get-sync-time host owner repo "issues")))
         (api-filters (if since
                         (plist-put (copy-sequence filters) :since since)
                       filters)))
    ;; Sync labels in background
    (forgejo-api-get
     host-url (format "repos/%s/%s/labels" owner repo) nil
     (lambda (data _headers)
       (when data (forgejo-db-save-labels host owner repo data))))
    ;; Fetch issues page by page, re-rendering after each
    (let ((endpoint (format "repos/%s/%s/issues" owner repo))
          (params (forgejo-issue--build-params api-filters)))
      (forgejo-api-get-paged
       host-url endpoint params
       ;; Per-page: save to DB, re-render only on first page
       (lambda (page-data _headers page-num)
         (forgejo-db-save-issues host owner repo page-data)
         (when (and (= page-num 1) (buffer-live-p (get-buffer buf-name)))
           (with-current-buffer buf-name
             (forgejo-issue--render-from-db
              buf-name host-url host owner repo forgejo-issue--filters))))
       ;; Done: close missing, set sync time, final re-render
       (lambda (all-data headers)
         (when (and force (equal (plist-get filters :state) "open"))
           (let ((numbers (mapcar (lambda (i) (alist-get 'number i)) all-data)))
             (forgejo-db-close-missing host owner repo numbers)))
         (forgejo-db-set-sync-time host owner repo "issues"
                                   (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                                       nil t))
         (when (buffer-live-p (get-buffer buf-name))
           (with-current-buffer buf-name
             (forgejo-issue--render-from-db
              buf-name host-url host owner repo forgejo-issue--filters)
             (when-let* ((total (plist-get headers :total-count)))
               (setq forgejo-issue--total-count total)))))))))

;;; Entry commands

;;;###autoload
(defun forgejo-issue-list (&optional owner repo)
  "List issues for OWNER/REPO.
Shows cached data immediately, then syncs from the API in the background."
  (interactive)
  (let* ((context (if (and owner repo)
                      (list nil owner repo)
                    (forgejo-repo-read)))
         (host-url (or (nth 0 context) (forgejo--resolve-host)))
         (owner (nth 1 context))
         (repo (nth 2 context))
         (buf-name (format "*forgejo-issues: %s/%s*" owner repo))
         (default-query (forgejo--default-filter-for
                         owner repo forgejo-issue-default-filter))
         (filters (forgejo-filter-parse default-query))
         (host (url-host (url-generic-parse-url host-url))))
    ;; Show cached data immediately
    (switch-to-buffer
     (forgejo-issue--render-from-db buf-name host-url host owner repo filters))
    (with-current-buffer buf-name
      (setq forgejo-issue--filters filters))
    ;; Sync in background
    (forgejo-issue--sync host-url host owner repo filters buf-name)))

(defun forgejo-issue-refresh ()
  "Force a full re-fetch of the current issue list from the API."
  (interactive)
  (when (and forgejo-repo--owner forgejo-repo--name)
    (forgejo-issue--refilter)))

(defun forgejo-issue--refilter ()
  "Re-render from DB with current filters, then sync."
  (let* ((host-url forgejo-repo--host)
         (host (url-host (url-generic-parse-url host-url)))
         (buf (buffer-name))
         (line (line-number-at-pos)))
    (forgejo-issue--render-from-db buf host-url host
                                   forgejo-repo--owner forgejo-repo--name
                                   forgejo-issue--filters)
    (forgejo-issue--sync host-url host forgejo-repo--owner forgejo-repo--name
                         forgejo-issue--filters buf t)
    (goto-char (point-min))
    (forward-line (1- line))))

;;; Filter commands

(defun forgejo-issue-filter ()
  "Filter issues using a query string with prefix completion.
Supported prefixes: state:, label:, milestone:, author:, search:.
Bare words are treated as title search.
Empty input clears all filters."
  (interactive)
  (let* ((host (url-host (url-generic-parse-url forgejo-repo--host)))
         (completions (forgejo-filter-completions
                       host forgejo-repo--owner forgejo-repo--name))
         (current (if forgejo-issue--filters
                     (forgejo-filter-serialize forgejo-issue--filters)
                   (forgejo--default-filter-for
                    forgejo-repo--owner forgejo-repo--name
                    forgejo-issue-default-filter)))
         (query (forgejo-utils-read-filter current completions))
         (filters (forgejo-filter-parse query)))
    (setq forgejo-issue--filters filters)
    (forgejo-issue--refilter)))

(defun forgejo-issue-filter-state ()
  "Filter issues by state."
  (interactive)
  (let ((state (completing-read "State: " '("open" "closed" "all") nil t)))
    (setq forgejo-issue--filters
          (plist-put forgejo-issue--filters :state
                     (unless (string= state "all") state)))
    (setq forgejo-issue--filters
          (plist-put forgejo-issue--filters :page nil))
    (forgejo-issue--refilter)))

(defun forgejo-issue-filter-label ()
  "Filter issues by label."
  (interactive)
  (let* ((host (url-host (url-generic-parse-url forgejo-repo--host)))
         (cached (forgejo-db-get-labels host forgejo-repo--owner
                                        forgejo-repo--name))
         (names (mapcar (lambda (row) (nth 4 row)) cached))
         (label (completing-read "Label: " names nil nil)))
    (setq forgejo-issue--filters
          (plist-put forgejo-issue--filters :labels
                     (unless (string-empty-p label) label)))
    (setq forgejo-issue--filters
          (plist-put forgejo-issue--filters :page nil))
    (forgejo-issue--refilter)))

(defun forgejo-issue-filter-milestone ()
  "Filter issues by milestone."
  (interactive)
  (let ((milestone (read-string "Milestone: ")))
    (setq forgejo-issue--filters
          (plist-put forgejo-issue--filters :milestone
                     (unless (string-empty-p milestone) milestone)))
    (setq forgejo-issue--filters
          (plist-put forgejo-issue--filters :page nil))
    (forgejo-issue--refilter)))

(defun forgejo-issue-filter-search ()
  "Search issues by title."
  (interactive)
  (let ((query (read-string "Search: ")))
    (setq forgejo-issue--filters
          (plist-put forgejo-issue--filters :query
                     (unless (string-empty-p query) query)))
    (setq forgejo-issue--filters
          (plist-put forgejo-issue--filters :page nil))
    (forgejo-issue--refilter)))

(defun forgejo-issue-clear-filters ()
  "Reset filters to the default and refresh."
  (interactive)
  (setq forgejo-issue--filters
        (forgejo-filter-parse
         (forgejo--default-filter-for
          forgejo-repo--owner forgejo-repo--name
          forgejo-issue-default-filter)))
  (forgejo-issue--refilter))

;;; Pagination

(defun forgejo-issue-next-page ()
  "Load the next page of issues."
  (interactive)
  (let ((page (or (plist-get forgejo-issue--filters :page) 1)))
    (setq forgejo-issue--filters
          (plist-put forgejo-issue--filters :page (1+ page)))
    (forgejo-issue--refilter)))

(defun forgejo-issue-prev-page ()
  "Load the previous page of issues."
  (interactive)
  (let ((page (or (plist-get forgejo-issue--filters :page) 1)))
    (when (> page 1)
      (setq forgejo-issue--filters
            (plist-put forgejo-issue--filters :page (1- page)))
      (forgejo-issue--refilter))))

;;; Browse

(defun forgejo-issue-browse-at-point ()
  "Open the issue at point in the browser."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (forgejo-utils-browse-issue forgejo-repo--host
                                forgejo-repo--owner forgejo-repo--name id)))

;;; ---- Issue detail view (EWOC) ----

(declare-function forgejo-issue-actions "forgejo-transient.el" ())

(defvar forgejo-issue-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'forgejo-buffer-follow-link)
    (define-key map (kbd "g") #'forgejo-view-refresh)
    (define-key map (kbd "b") #'forgejo-view-browse)
    (define-key map (kbd "c") #'forgejo-view-comment)
    (define-key map (kbd "r") #'forgejo-issue-reply)
    (define-key map (kbd "e") #'forgejo-view-edit)
    (define-key map (kbd "x") #'forgejo-view-toggle-state)
    (define-key map (kbd "L") #'forgejo-view-add-label)
    (define-key map (kbd "A") #'forgejo-view-add-assignee)
    (define-key map (kbd "M") #'forgejo-view-set-milestone)
    (define-key map (kbd "D") #'forgejo-view-delete-at-point)
    (define-key map (kbd "h") #'forgejo-issue-actions)
    (define-key map (kbd "n") #'ewoc-goto-next)
    (define-key map (kbd "p") #'ewoc-goto-prev)
    map)
  "Keymap for `forgejo-issue-view-mode'.")

(define-derived-mode forgejo-issue-view-mode special-mode
  "Forgejo Issue"
  "Major mode for viewing a single Forgejo issue."
  :group 'forgejo
  (setq-local browse-url-browser-function #'forgejo-buffer--browse-url))

;;; Detail view rendering

(defun forgejo-issue--render-detail (buf-name host-url owner repo issue-alist
                                     timeline-alists)
  "Render issue detail into BUF-NAME from alist data.
HOST-URL is the instance URL."
  (forgejo-view--render-detail buf-name host-url owner repo issue-alist
                               timeline-alists
                               #'forgejo-issue-view-mode
                               #'forgejo-issue--sync-detail
                               #'forgejo-utils-browse-issue))

(defun forgejo-issue--sync-detail (host owner repo number buf-name
                                        &optional restore-line)
  "Sync issue NUMBER from API in background, re-render BUF-NAME if changed.
When RESTORE-LINE is non-nil, go to that line after re-rendering."
  (let ((host-url (forgejo--host-url-for-hostname host))
        (endpoint (format "repos/%s/%s/issues/%d" owner repo number))
        (tl-endpoint (format "repos/%s/%s/issues/%d/timeline"
                             owner repo number)))
    (forgejo-api-get
     host-url endpoint nil
     (lambda (issue-data _headers)
       (forgejo-db-save-issues host owner repo (list issue-data))
       (forgejo-api-get
        host-url tl-endpoint
        (list (cons "limit" (number-to-string forgejo-timeline-page-size)))
        (lambda (timeline _tl-headers)
          (forgejo-db-save-timeline host owner repo number timeline)
          (forgejo-view--re-render
           buf-name host-url host owner repo number
           #'forgejo-issue--render-detail restore-line)))))))

;;; Detail view entry

(defun forgejo-issue-view-at-point ()
  "View the issue at point in the list."
  (interactive)
  (when-let* ((number (tabulated-list-get-id)))
    (forgejo-issue-view forgejo-repo--owner forgejo-repo--name number)))

;;;###autoload
(defun forgejo-issue-view (owner repo number)
  "View issue NUMBER in OWNER/REPO.
Shows cached data from DB instantly, syncs in background."
  (interactive
   (let ((context (forgejo-repo-read)))
     (list (nth 1 context) (nth 2 context) (read-number "Issue number: "))))
  (let* ((host-url (or forgejo-repo--host (forgejo--resolve-host)))
         (host (url-host (url-generic-parse-url host-url)))
         (buf-name (format "*forgejo-issue: %s/%s#%d*" owner repo number))
         (issue-alist (forgejo-db-get-issue host owner repo number))
         (tl-rows (forgejo-db-get-timeline host owner repo number))
         (tl-alists (mapcar #'forgejo-db--row-to-timeline-alist tl-rows)))
    (if issue-alist
        (switch-to-buffer
         (forgejo-issue--render-detail buf-name host-url owner repo
                                       issue-alist tl-alists))
      (with-current-buffer (get-buffer-create buf-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Loading..."))
        (forgejo-issue-view-mode)
        (setq forgejo-repo--host host-url
              forgejo-repo--owner owner
              forgejo-repo--name repo)
        (switch-to-buffer (current-buffer))))
    (forgejo-issue--sync-detail host owner repo number buf-name)))

;;; Issue-specific commands

(defun forgejo-issue-reply ()
  "Reply to the comment at point."
  (interactive)
  (when-let* ((node (forgejo-view--node-at-point))
              (data forgejo-view--data)
              (number (alist-get 'number data))
              (refresh (forgejo--post-action-callback)))
    (let* ((author (plist-get node :author))
           (body (plist-get node :body))
           (quoted (when body
                     (concat "> " (replace-regexp-in-string
                                   "\n" "\n> " (string-trim body))
                             "\n\n"))))
      (forgejo-utils-post-comment
       forgejo-repo--host
       (format "repos/%s/%s/issues/%d/comments"
               forgejo-repo--owner forgejo-repo--name number)
       (format "Reply to %s" (or author ""))
       quoted
       (lambda (_data _headers)
         (message "Reply posted on %s/%s#%d"
                  forgejo-repo--owner forgejo-repo--name number)
         (funcall refresh))))))

(defun forgejo-issue-create ()
  "Create a new issue in the current repository."
  (interactive)
  (forgejo-utils-create-issue forgejo-repo--host
                              forgejo-repo--owner forgejo-repo--name))

(defun forgejo-issue-create-label ()
  "Create a new label in the current repository."
  (interactive)
  (let ((host (url-host (url-generic-parse-url forgejo-repo--host))))
    (forgejo-utils-create-label
     forgejo-repo--host forgejo-repo--owner forgejo-repo--name host nil)))

(provide 'forgejo-issue)
;;; forgejo-issue.el ends here
