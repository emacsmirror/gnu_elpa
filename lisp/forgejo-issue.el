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
(require 'ewoc)
(require 'url-parse)
(require 'forgejo)
(require 'forgejo-tl)
(require 'forgejo-buffer)
(require 'forgejo-utils)
(require 'forgejo-api)
(require 'forgejo-db)

(declare-function forgejo-repo-read "forgejo-repo.el" ())

(defvar forgejo-host)
(defvar forgejo-default-sort)
(defvar forgejo-timeline-page-size)
(defvar forgejo-issue-default-filter)
(defvar forgejo-repo--host)
(defvar forgejo-repo--owner)
(defvar forgejo-repo--name)

;;; ---- Issue list view ----

(defvar-local forgejo-issue--filters nil
  "Plist of current filter state.
Keys: :state :labels :milestone :assignee :query :page")

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
    (define-key map (kbd "x") #'forgejo-issue-toggle-state)
    (define-key map (kbd "b") #'forgejo-issue-browse-at-point)
    map)
  "Keymap for `forgejo-issue-list-mode'.")

(define-derived-mode forgejo-issue-list-mode tabulated-list-mode
  "Forgejo Issues"
  "Major mode for browsing Forgejo issues."
  :group 'forgejo
  (setq tabulated-list-padding 1
        tabulated-list-format
        (vector `("#" 5 forgejo--sort-by-number :right-align t)
                `("State" 8 nil)
                `("Title" ,(/ (window-width) 3) t)
                `("Labels" ,(/ (window-width) 6) nil)
                `("Author" ,(/ (window-width) 8) t)
                `("Updated" ,(/ (window-width) 8) forgejo--sort-by-updated))
        tabulated-list-sort-key '("#" . t))
  (tabulated-list-init-header))

(defun forgejo-issue--entries (issues)
  "Convert ISSUES (list of API alists) to `tabulated-list-entries'."
  (mapcar
   (lambda (issue)
     (let-alist issue
       (list .number
             (vector
              (propertize (number-to-string .number) 'face 'forgejo-number-face)
              (forgejo-buffer--format-state .state)
              .title
              (forgejo-buffer--format-labels .labels)
              (propertize (or (forgejo-buffer--login .user) "")
                          'face 'forgejo-comment-author-face)
              (propertize (forgejo-buffer--relative-time .updated_at)
                          'face 'shadow
                          'forgejo-timestamp (or .updated_at ""))))))
   issues))

;;; API interaction

(defun forgejo-issue--build-params (filters)
  "Build API query params from FILTERS plist."
  (let ((params (list (cons "type" "issues")
                      (cons "sort" forgejo-default-sort)
                      (cons "limit" (number-to-string
                                     (forgejo-api-default-limit))))))
    (when-let* ((state (plist-get filters :state)))
      (push (cons "state" state) params))
    (when-let* ((labels (plist-get filters :labels)))
      (push (cons "labels" labels) params))
    (when-let* ((milestone (plist-get filters :milestone)))
      (push (cons "milestones" milestone) params))
    (when-let* ((assignee (plist-get filters :assignee)))
      (push (cons "created_by" assignee) params))
    (when-let* ((query (plist-get filters :query)))
      (push (cons "q" query) params))
    (when-let* ((page (plist-get filters :page)))
      (push (cons "page" (number-to-string page)) params))
    (when-let* ((since (plist-get filters :since)))
      (push (cons "since" since) params))
    params))

(defun forgejo-issue--fetch (owner repo filters callback)
  "Fetch all issues from API for OWNER/REPO with FILTERS, call CALLBACK."
  (let ((endpoint (format "repos/%s/%s/issues" owner repo))
        (params (forgejo-issue--build-params filters)))
    (forgejo-api-get-all endpoint params callback)))

;;; Cache-first rendering

(defun forgejo-issue--render-from-db (buf-name host owner repo filters)
  "Render cached issues into BUF-NAME from the DB.
HOST, OWNER, REPO identify the repository.  FILTERS is the filter plist."
  (let* ((db-filters (append (list :no-pulls t) filters))
         (rows (forgejo-db-get-issues host owner repo db-filters))
         (alists (mapcar #'forgejo-db--row-to-issue-alist rows))
         (entries (forgejo-issue--entries alists)))
    (with-current-buffer (get-buffer-create buf-name)
      (unless (derived-mode-p 'forgejo-issue-list-mode)
        (forgejo-issue-list-mode))
      (unless forgejo-repo--host
        (setq forgejo-repo--host forgejo-host))
      (setq forgejo-repo--owner owner
            forgejo-repo--name repo
            forgejo-issue--filters filters
            tabulated-list-format
            (vector `("#" 5 forgejo--sort-by-number :right-align t)
                    `("State" 8 nil)
                    `("Title" ,(/ (window-width) 3) t)
                    `("Labels" ,(/ (window-width) 6) nil)
                    `("Author" ,(/ (window-width) 8) t)
                    `("Updated" ,(/ (window-width) 8) forgejo--sort-by-updated))
            tabulated-list-entries entries)
      (unless tabulated-list-sort-key
        (setq tabulated-list-sort-key '("#" . t)))
      (tabulated-list-init-header)
      (forgejo-tl-print t)
      (current-buffer))))

(defun forgejo-issue--sync (host owner repo filters buf-name
                                 &optional force)
  "Fetch issues from API and update DB, then re-render BUF-NAME.
When FORCE is nil, use incremental sync via the `since' parameter.
When FORCE is non-nil, fetch all and mark missing issues as closed."
  (let* ((since (unless force
                  (forgejo-db-get-sync-time host owner repo "issues")))
         (api-filters (if since
                         (plist-put (copy-sequence filters) :since since)
                       filters)))
    ;; Sync labels in background
    (forgejo-api-get
     (format "repos/%s/%s/labels" owner repo) nil
     (lambda (data _headers)
       (when data (forgejo-db-save-labels host owner repo data))))
    ;; Fetch issues page by page, re-rendering after each
    (let ((endpoint (format "repos/%s/%s/issues" owner repo))
          (params (forgejo-issue--build-params api-filters)))
      (forgejo-api-get-paged
       endpoint params
       ;; Per-page: save and re-render
       (lambda (page-data _headers _page-num)
         (forgejo-db-save-issues host owner repo page-data)
         (when (buffer-live-p (get-buffer buf-name))
           (forgejo-issue--render-from-db buf-name host owner repo filters)))
       ;; Done: close missing, set sync time, final re-render
       (lambda (all-data headers)
         (when (and force (equal (plist-get filters :state) "open"))
           (let ((numbers (mapcar (lambda (i) (alist-get 'number i)) all-data)))
             (forgejo-db-close-missing host owner repo numbers)))
         (forgejo-db-set-sync-time host owner repo "issues"
                                   (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                                       nil t))
         (when (buffer-live-p (get-buffer buf-name))
           (forgejo-issue--render-from-db buf-name host owner repo filters)
           (when-let* ((total (plist-get headers :total-count)))
             (with-current-buffer buf-name
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
         (host-url (or (nth 0 context) forgejo-host))
         (owner (nth 1 context))
         (repo (nth 2 context))
         (buf-name (format "*forgejo-issues: %s/%s*" owner repo))
         (default-query (forgejo--default-filter-for
                         owner repo forgejo-issue-default-filter))
         (filters (forgejo-utils-parse-filter default-query))
         (host (url-host (url-generic-parse-url host-url))))
    (forgejo-with-host host-url
      ;; Show cached data immediately
      (switch-to-buffer
       (forgejo-issue--render-from-db buf-name host owner repo filters))
      ;; Sync in background
      (forgejo-issue--sync host owner repo filters buf-name))))

(defun forgejo-issue-refresh ()
  "Force a full re-fetch of the current issue list from the API."
  (interactive)
  (when (and forgejo-repo--owner forgejo-repo--name)
    (let ((host (url-host (url-generic-parse-url
                           (or forgejo-repo--host forgejo-host)))))
      (forgejo-with-host forgejo-repo--host
        (forgejo-issue--sync host forgejo-repo--owner forgejo-repo--name
                             forgejo-issue--filters (buffer-name) t)))))

(defun forgejo-issue--refilter ()
  "Re-render from DB with current filters, then sync.
Forces a full re-fetch to ensure the DB has data matching the filters."
  (let ((host (url-host (url-generic-parse-url
                         (or forgejo-repo--host forgejo-host)))))
    (forgejo-with-host forgejo-repo--host
      (forgejo-issue--render-from-db (buffer-name) host
                                     forgejo-repo--owner forgejo-repo--name
                                     forgejo-issue--filters)
      (forgejo-issue--sync host forgejo-repo--owner forgejo-repo--name
                           forgejo-issue--filters (buffer-name) t))))

;;; Filter commands

(defun forgejo-issue-filter ()
  "Filter issues using a query string with prefix completion.
Supported prefixes: state:, label:, milestone:, author:, search:.
Bare words are treated as title search.
Empty input clears all filters."
  (interactive)
  (let* ((host (url-host (url-generic-parse-url
                          (or forgejo-repo--host forgejo-host))))
         (labels (mapcar (lambda (row) (nth 4 row))
                         (forgejo-db-get-labels host forgejo-repo--owner
                                                forgejo-repo--name)))
         (milestones (mapcar (lambda (row) (nth 3 row))
                             (forgejo-db-get-milestones host forgejo-repo--owner
                                                        forgejo-repo--name)))
         (authors (forgejo-db-get-authors host forgejo-repo--owner
                                          forgejo-repo--name))
         (current (if forgejo-issue--filters
                     (forgejo-utils-serialize-filter forgejo-issue--filters)
                   (forgejo--default-filter-for
                    forgejo-repo--owner forgejo-repo--name
                    forgejo-issue-default-filter)))
         (query (forgejo-utils-read-filter
                 current
                 `((state . ("open" "closed"))
                   (label . ,labels)
                   (milestone . ,milestones)
                   (author . ,authors)
                   (search . nil))))
         (filters (forgejo-utils-parse-filter query)))
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
  (let* ((host (url-host (url-generic-parse-url
                          (or forgejo-repo--host forgejo-host))))
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
        (forgejo-utils-parse-filter
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
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-browse-issue forgejo-repo--owner forgejo-repo--name id))))

;;; ---- Issue detail view (EWOC) ----

(defvar-local forgejo-issue--ewoc nil
  "EWOC instance for the current issue detail view.")

(defvar-local forgejo-issue--data nil
  "Full API response alist for the current issue.")

(declare-function forgejo-issue-actions "forgejo-transient.el" ())

(defvar forgejo-issue-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'forgejo-issue-view-refresh)
    (define-key map (kbd "b") #'forgejo-issue-view-browse)
    (define-key map (kbd "c") #'forgejo-issue-comment)
    (define-key map (kbd "r") #'forgejo-issue-reply)
    (define-key map (kbd "e") #'forgejo-issue-edit)
    (define-key map (kbd "x") #'forgejo-issue-toggle-state)
    (define-key map (kbd "L") #'forgejo-issue-add-label)
    (define-key map (kbd "A") #'forgejo-issue-add-assignee)
    (define-key map (kbd "M") #'forgejo-issue-set-milestone)
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

(defun forgejo-issue--render-detail (buf-name owner repo issue-alist timeline-alists)
  "Render issue detail into BUF-NAME from alist data."
  (let ((nodes (forgejo-buffer--build-nodes issue-alist timeline-alists)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (forgejo-issue-view-mode)
      (unless forgejo-repo--host
        (setq forgejo-repo--host forgejo-host))
      (setq forgejo-repo--owner owner
            forgejo-repo--name repo
            forgejo-issue--data issue-alist)
      (setq forgejo-issue--ewoc
            (ewoc-create #'forgejo-buffer--pp nil nil t))
      (dolist (node nodes)
        (ewoc-enter-last forgejo-issue--ewoc node))
      (setq header-line-format
            (forgejo-buffer--header-line issue-alist))
      (goto-char (point-min))
      (current-buffer))))

(defun forgejo-issue--render-missing-html (host owner repo number
                                                buf-name restore-line)
  "Render markdown to HTML for entries missing body_html.
After rendering, re-render BUF-NAME and restore RESTORE-LINE."
  (let* ((context (format "%s/%s" owner repo))
         (issue (forgejo-db-get-issue host owner repo number))
         (tl-rows (forgejo-db-get-timeline host owner repo number))
         (tl-alists (mapcar #'forgejo-db--row-to-timeline-alist tl-rows))
         (pending 0)
         (render-done
          (lambda ()
            (cl-decf pending)
            (when (<= pending 0)
              (forgejo-buffer--re-render
               buf-name host owner repo number
               #'forgejo-issue--render-detail restore-line)))))
    ;; Issue body
    (when (and issue
               (not (alist-get 'body_html issue))
               (alist-get 'body issue))
      (cl-incf pending)
      (forgejo-api-render-markdown-async
       (alist-get 'body issue) context
       (lambda (html)
         (when html
           (forgejo-db-update-issue-html host owner repo number html))
         (funcall render-done))))
    ;; Timeline comment bodies
    (dolist (evt tl-alists)
      (when (and (string= "comment" (or (alist-get 'type evt) ""))
                 (not (alist-get 'body_html evt))
                 (alist-get 'body evt))
        (let ((evt-id (alist-get 'id evt)))
          (cl-incf pending)
          (forgejo-api-render-markdown-async
           (alist-get 'body evt) context
           (lambda (html)
             (when html
               (forgejo-db-update-timeline-html
                host owner repo number evt-id html))
             (funcall render-done))))))
    ;; Nothing to render: just re-render now
    (when (zerop pending)
      (funcall render-done))))

(defun forgejo-issue--sync-detail (host owner repo number buf-name
                                        &optional restore-line)
  "Sync issue NUMBER from API in background, re-render BUF-NAME if changed.
When RESTORE-LINE is non-nil, go to that line after re-rendering."
  (let ((endpoint (format "repos/%s/%s/issues/%d" owner repo number))
        (tl-endpoint (format "repos/%s/%s/issues/%d/timeline"
                             owner repo number)))
    (forgejo-api-get
     endpoint nil
     (lambda (issue-data _headers)
       (forgejo-db-save-issues host owner repo (list issue-data))
       (forgejo-api-get
        tl-endpoint
        (list (cons "limit" (number-to-string forgejo-timeline-page-size)))
        (lambda (timeline _tl-headers)
          (forgejo-db-save-timeline host owner repo number timeline)
          ;; First render with whatever we have
          (forgejo-buffer--re-render
           buf-name host owner repo number
           #'forgejo-issue--render-detail restore-line)
          ;; Then render missing HTML and re-render
          (forgejo-issue--render-missing-html
           host owner repo number buf-name restore-line)))))))

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
  (let* ((host (url-host (url-generic-parse-url
                          (or forgejo-repo--host forgejo-host))))
         (buf-name (format "*forgejo-issue: %s/%s#%d*" owner repo number))
         (issue-alist (forgejo-db-get-issue host owner repo number))
         (tl-rows (forgejo-db-get-timeline host owner repo number))
         (tl-alists (mapcar #'forgejo-db--row-to-timeline-alist tl-rows)))
    (forgejo-with-host forgejo-repo--host
      (if issue-alist
          (switch-to-buffer
           (forgejo-issue--render-detail buf-name owner repo
                                         issue-alist tl-alists))
        (with-current-buffer (get-buffer-create buf-name)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "Loading..."))
          (forgejo-issue-view-mode)
          (setq forgejo-repo--host forgejo-host
                forgejo-repo--owner owner
                forgejo-repo--name repo)
          (switch-to-buffer (current-buffer))))
      (forgejo-issue--sync-detail host owner repo number buf-name))))

(defun forgejo-issue-view-refresh ()
  "Force sync the current issue detail view."
  (interactive)
  (when-let* ((data forgejo-issue--data)
              (number (alist-get 'number data)))
    (let ((host (url-host (url-generic-parse-url
                           (or forgejo-repo--host forgejo-host))))
          (line (line-number-at-pos)))
      (forgejo-with-host forgejo-repo--host
        (forgejo-issue--sync-detail host forgejo-repo--owner
                                    forgejo-repo--name number
                                    (buffer-name) line)))))

(defun forgejo-issue-view-browse ()
  "Open the current issue in the browser."
  (interactive)
  (when-let* ((data forgejo-issue--data)
              (number (alist-get 'number data)))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-browse-issue forgejo-repo--owner forgejo-repo--name number))))

(defun forgejo-issue-comment ()
  "Post a comment on the current issue."
  (interactive)
  (when-let* ((data forgejo-issue--data)
              (number (alist-get 'number data))
              (refresh (forgejo--post-action-callback)))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-post-comment
       (format "repos/%s/%s/issues/%d/comments"
               forgejo-repo--owner forgejo-repo--name number)
       "Comment"
       nil
       (lambda (_data _headers)
         (message "Comment posted on %s/%s#%d"
                  forgejo-repo--owner forgejo-repo--name number)
         (funcall refresh))))))

(defun forgejo-issue-reply ()
  "Reply to the comment at point."
  (interactive)
  (when-let* ((node (forgejo-buffer--node-at-point forgejo-issue--ewoc))
              (data forgejo-issue--data)
              (number (alist-get 'number data))
              (refresh (forgejo--post-action-callback)))
    (let* ((author (plist-get node :author))
           (body (plist-get node :body))
           (quoted (when body
                     (concat "> " (replace-regexp-in-string
                                   "\n" "\n> " (string-trim body))
                             "\n\n"))))
      (forgejo-with-host forgejo-repo--host
        (forgejo-utils-post-comment
         (format "repos/%s/%s/issues/%d/comments"
                 forgejo-repo--owner forgejo-repo--name number)
         (format "Reply to %s" (or author ""))
         quoted
         (lambda (_data _headers)
           (message "Reply posted on %s/%s#%d"
                    forgejo-repo--owner forgejo-repo--name number)
           (funcall refresh)))))))

(defun forgejo-issue-create ()
  "Create a new issue in the current repository."
  (interactive)
  (forgejo-with-host forgejo-repo--host
    (forgejo-utils-create-issue forgejo-repo--owner forgejo-repo--name)))

(defun forgejo-issue-create-label ()
  "Create a new label in the current repository."
  (interactive)
  (let ((host (url-host (url-generic-parse-url
                         (or forgejo-repo--host forgejo-host)))))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-create-label
       forgejo-repo--owner forgejo-repo--name host nil))))

(defun forgejo-issue-toggle-state ()
  "Toggle the state of the issue at point or in the current view."
  (interactive)
  (let* ((number (or (and (bound-and-true-p forgejo-issue--data)
                          (alist-get 'number forgejo-issue--data))
                     (tabulated-list-get-id)))
         (host (url-host (url-generic-parse-url
                          (or forgejo-repo--host forgejo-host))))
         (issue (forgejo-db-get-issue host forgejo-repo--owner
                                      forgejo-repo--name number))
         (state (alist-get 'state issue)))
    (when (and number state)
      (forgejo-with-host forgejo-repo--host
        (forgejo-utils-toggle-state
         forgejo-repo--owner forgejo-repo--name number state
         (forgejo--post-action-callback))))))

;;; Edit

(defun forgejo-issue-edit ()
  "Edit the body or comment at point."
  (interactive)
  (when-let* ((node (forgejo-buffer--node-at-point forgejo-issue--ewoc))
              (data forgejo-issue--data)
              (number (alist-get 'number data)))
    (forgejo-with-host forgejo-repo--host
      (pcase (plist-get node :type)
        ('header
         (forgejo-utils-edit-body
          forgejo-repo--owner forgejo-repo--name number
          (plist-get node :body)
          (forgejo--post-action-callback)))
        ('comment
         (forgejo-utils-edit-comment
          forgejo-repo--owner forgejo-repo--name
          (plist-get node :id) (plist-get node :body)
          (forgejo--post-action-callback)))
        (_ (user-error "No editable item at point"))))))

;;; Metadata

(defun forgejo-issue-add-label ()
  "Add a label to the current issue."
  (interactive)
  (when-let* ((data forgejo-issue--data)
              (number (alist-get 'number data))
              (host (url-host (url-generic-parse-url
                               (or forgejo-repo--host forgejo-host)))))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-add-label
       forgejo-repo--owner forgejo-repo--name number host
       (forgejo--post-action-callback)))))

(defun forgejo-issue-remove-label ()
  "Remove a label from the current issue."
  (interactive)
  (when-let* ((data forgejo-issue--data)
              (number (alist-get 'number data))
              (labels (alist-get 'labels data))
              (host (url-host (url-generic-parse-url
                               (or forgejo-repo--host forgejo-host)))))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-remove-label
       forgejo-repo--owner forgejo-repo--name number labels host
       (forgejo--post-action-callback)))))

(defun forgejo-issue-add-assignee ()
  "Add an assignee to the current issue."
  (interactive)
  (when-let* ((data forgejo-issue--data)
              (number (alist-get 'number data))
              (host (url-host (url-generic-parse-url
                               (or forgejo-repo--host forgejo-host)))))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-add-assignee
       forgejo-repo--owner forgejo-repo--name number
       (alist-get 'assignees data) host
       (forgejo--post-action-callback)))))

(defun forgejo-issue-remove-assignee ()
  "Remove an assignee from the current issue."
  (interactive)
  (when-let* ((data forgejo-issue--data)
              (number (alist-get 'number data))
              (assignees (alist-get 'assignees data)))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-remove-assignee
       forgejo-repo--owner forgejo-repo--name number assignees
       (forgejo--post-action-callback)))))

(defun forgejo-issue-set-milestone ()
  "Set or clear the milestone on the current issue."
  (interactive)
  (when-let* ((data forgejo-issue--data)
              (number (alist-get 'number data))
              (host (url-host (url-generic-parse-url
                               (or forgejo-repo--host forgejo-host)))))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-set-milestone
       forgejo-repo--owner forgejo-repo--name number host
       (forgejo--post-action-callback)))))

(provide 'forgejo-issue)
;;; forgejo-issue.el ends here
