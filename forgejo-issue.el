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

(declare-function forgejo-api-get "forgejo-api.el"
                  (endpoint &optional params callback))
(declare-function forgejo-api-get-all "forgejo-api.el"
                  (endpoint &optional params callback))
(declare-function forgejo-api-default-limit "forgejo-api.el" ())
(declare-function forgejo-db-save-issues "forgejo-db.el"
                  (host owner repo issues))
(declare-function forgejo-db-save-timeline "forgejo-db.el"
                  (host owner repo number events))
(declare-function forgejo-db-save-labels "forgejo-db.el"
                  (host owner repo labels))
(declare-function forgejo-db-get-issues "forgejo-db.el"
                  (host owner repo &optional filters))
(declare-function forgejo-db-get-timeline "forgejo-db.el"
                  (host owner repo number))
(declare-function forgejo-db-get-labels "forgejo-db.el"
                  (host owner repo))
(declare-function forgejo-db-set-sync-time "forgejo-db.el"
                  (host owner repo endpoint time))
(declare-function forgejo-db-get-sync-time "forgejo-db.el"
                  (host owner repo endpoint))
(declare-function forgejo-db--row-to-issue-alist "forgejo-db.el" (row))
(declare-function forgejo-db--row-to-timeline-alist "forgejo-db.el" (row))
(declare-function forgejo-db-get-issue "forgejo-db.el"
                  (host owner repo number))
(declare-function forgejo-db-update-issue-html "forgejo-db.el"
                  (host owner repo number html))
(declare-function forgejo-db-update-timeline-html "forgejo-db.el"
                  (host owner repo issue-number event-id html))
(declare-function forgejo-api-render-markdown-async "forgejo-api.el"
                  (text context callback))
(declare-function forgejo-repo-read "forgejo-repo.el" ())

(defvar forgejo-host)
(defvar forgejo-default-sort)
(defvar forgejo-timeline-page-size)
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
    (define-key map (kbd "RET") #'forgejo-issue-view-at-point)
    (define-key map (kbd "c") #'forgejo-issue-create)
    (define-key map (kbd "g") #'forgejo-issue-refresh)
    (define-key map (kbd "s") #'forgejo-issue-filter-state)
    (define-key map (kbd "l") #'forgejo-issue-filter-label)
    (define-key map (kbd "m") #'forgejo-issue-filter-milestone)
    (define-key map (kbd "/") #'forgejo-issue-filter-search)
    (define-key map (kbd "C") #'forgejo-issue-clear-filters)
    (define-key map (kbd "n") #'forgejo-issue-next-page)
    (define-key map (kbd "p") #'forgejo-issue-prev-page)
    (define-key map (kbd "b") #'forgejo-issue-browse-at-point)
    map)
  "Keymap for `forgejo-issue-list-mode'.")

(define-derived-mode forgejo-issue-list-mode tabulated-list-mode
  "Forgejo Issues"
  "Major mode for browsing Forgejo issues."
  :group 'forgejo
  (setq tabulated-list-padding 1
        tabulated-list-format
        (vector `("#" 5 t :right-align t)
                `("State" 8 nil)
                `("Title" ,(/ (window-width) 3) t)
                `("Labels" ,(/ (window-width) 6) nil)
                `("Author" ,(/ (window-width) 8) t)
                `("Updated" ,(/ (window-width) 8) t)))
  (tabulated-list-init-header))

(defun forgejo-issue--entries (issues)
  "Convert ISSUES (list of API alists) to `tabulated-list-entries'."
  (mapcar
   (lambda (issue)
     (let-alist issue
       (list .number
             (vector
              (number-to-string .number)
              (forgejo-buffer--format-state .state)
              .title
              (forgejo-buffer--format-labels .labels)
              (or (forgejo-buffer--login .user) "")
              (forgejo-buffer--relative-time .updated_at)))))
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
      (push (cons "assigned_by" assignee) params))
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
            (vector `("#" 5 t :right-align t)
                    `("State" 8 nil)
                    `("Title" ,(/ (window-width) 3) t)
                    `("Labels" ,(/ (window-width) 6) nil)
                    `("Author" ,(/ (window-width) 8) t)
                    `("Updated" ,(/ (window-width) 8) t))
            tabulated-list-entries entries)
      (tabulated-list-init-header)
      (forgejo-tl-print t)
      (current-buffer))))

(defun forgejo-issue--sync (host owner repo filters buf-name
                                 &optional force)
  "Fetch issues from API and update DB, then re-render BUF-NAME.
When FORCE is nil, use incremental sync via the `since' parameter."
  (let* ((since (unless force
                  (forgejo-db-get-sync-time host owner repo "issues")))
         (api-filters (if since
                         (plist-put (copy-sequence filters) :since since)
                       filters)))
    (forgejo-issue--fetch
     owner repo api-filters
     (lambda (data headers)
       (forgejo-db-save-issues host owner repo data)
       (forgejo-db-set-sync-time host owner repo "issues"
                                 (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                                     nil t))
       (when (buffer-live-p (get-buffer buf-name))
         (forgejo-issue--render-from-db buf-name host owner repo filters)
         (when-let* ((total (plist-get headers :total-count)))
           (with-current-buffer buf-name
             (setq forgejo-issue--total-count total))))))))

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
         (filters (list :state "open"))
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
  "Re-render from DB with current filters, then sync incrementally."
  (let ((host (url-host (url-generic-parse-url
                         (or forgejo-repo--host forgejo-host)))))
    (forgejo-with-host forgejo-repo--host
      (forgejo-issue--render-from-db (buffer-name) host
                                     forgejo-repo--owner forgejo-repo--name
                                     forgejo-issue--filters)
      (forgejo-issue--sync host forgejo-repo--owner forgejo-repo--name
                           forgejo-issue--filters (buffer-name)))))

;;; Filter commands

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
  "Clear all filters and refresh."
  (interactive)
  (setq forgejo-issue--filters (list :state "open"))
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
    (define-key map (kbd "h") #'forgejo-issue-actions)
    (define-key map (kbd "n") #'ewoc-goto-next)
    (define-key map (kbd "p") #'ewoc-goto-prev)
    map)
  "Keymap for `forgejo-issue-view-mode'.")

(define-derived-mode forgejo-issue-view-mode special-mode
  "Forgejo Issue"
  "Major mode for viewing a single Forgejo issue."
  :group 'forgejo)

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
            (when (and (<= pending 0) (buffer-live-p (get-buffer buf-name)))
              (let* ((fresh (forgejo-db-get-issue host owner repo number))
                     (rows (forgejo-db-get-timeline host owner repo number))
                     (tl (mapcar #'forgejo-db--row-to-timeline-alist rows)))
                (forgejo-issue--render-detail buf-name owner repo fresh tl)
                (when restore-line
                  (with-current-buffer buf-name
                    (goto-char (point-min))
                    (forward-line (1- restore-line)))))))))
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
          ;; First render with whatever we have (may be raw text)
          (when (buffer-live-p (get-buffer buf-name))
            (let* ((fresh-issue (forgejo-db-get-issue host owner repo number))
                   (tl-rows (forgejo-db-get-timeline host owner repo number))
                   (tl-alists (mapcar #'forgejo-db--row-to-timeline-alist
                                      tl-rows)))
              (forgejo-issue--render-detail buf-name owner repo
                                           fresh-issue tl-alists)
              (when restore-line
                (with-current-buffer buf-name
                  (goto-char (point-min))
                  (forward-line (1- restore-line))))))
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
              (number (alist-get 'number data)))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-comment forgejo-repo--owner forgejo-repo--name number))))

(defun forgejo-issue-create ()
  "Create a new issue in the current repository."
  (interactive)
  (forgejo-with-host forgejo-repo--host
    (forgejo-utils-create-issue forgejo-repo--owner forgejo-repo--name)))

(provide 'forgejo-issue)
;;; forgejo-issue.el ends here
