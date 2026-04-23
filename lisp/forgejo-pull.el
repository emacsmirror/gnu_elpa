;;; forgejo-pull.el --- Pull request list, detail, and AGit-Flow  -*- lexical-binding: t; -*-

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

;; Pull request list view, detail view, and AGit-Flow integration for
;; submitting and fetching PRs via git push options.

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
(require 'forgejo-review)

(declare-function forgejo-repo-read "forgejo-repo.el" ())

(defvar forgejo-host)
(defvar forgejo-default-sort)
(defvar forgejo-timeline-page-size)
(defvar forgejo-pull-default-filter)
(defvar forgejo-repo--host)
(defvar forgejo-repo--owner)
(defvar forgejo-repo--name)

;;; ---- PR list view ----

(defvar-local forgejo-pull--filters nil
  "Plist of current PR filter state.
Keys: :state :milestone :labels :poster :page")

(defvar-local forgejo-pull--total-count nil
  "Total number of PRs matching current filters.")

(defvar forgejo-pull-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'forgejo-pull-view-at-point)
    (define-key map (kbd "S") #'forgejo-tl-sort)
    (define-key map (kbd "g") #'forgejo-pull-refresh)
    (define-key map (kbd "l") #'forgejo-pull-filter)
    (define-key map (kbd "C") #'forgejo-pull-clear-filters)
    (define-key map (kbd "x") #'forgejo-pull-toggle-state)
    (define-key map (kbd "b") #'forgejo-pull-browse-at-point)
    map)
  "Keymap for `forgejo-pull-list-mode'.")



(define-derived-mode forgejo-pull-list-mode tabulated-list-mode
  "Forgejo PRs"
  "Major mode for browsing Forgejo pull requests."
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

(defun forgejo-pull--entries (pulls)
  "Convert PULLS (list of API alists) to `tabulated-list-entries'."
  (mapcar
   (lambda (pr)
     (let-alist pr
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
   pulls))

(defun forgejo-pull--build-params (filters)
  "Build API query params from FILTERS plist for PR endpoint."
  (let ((params (list (cons "sort" forgejo-default-sort)
                      (cons "limit" (number-to-string
                                     (forgejo-api-default-limit))))))
    (when-let* ((state (plist-get filters :state)))
      (push (cons "state" state) params))
    (when-let* ((poster (plist-get filters :poster)))
      (push (cons "poster" poster) params))
    (when-let* ((page (plist-get filters :page)))
      (push (cons "page" (number-to-string page)) params))
    params))

(defun forgejo-pull--fetch (owner repo filters callback)
  "Fetch all PRs from API for OWNER/REPO with FILTERS, call CALLBACK."
  (let ((endpoint (format "repos/%s/%s/pulls" owner repo))
        (params (forgejo-pull--build-params filters)))
    (forgejo-api-get-all endpoint params callback)))

(defun forgejo-pull--render-from-db (buf-name host owner repo filters)
  "Render cached PRs into BUF-NAME from the DB."
  (let* ((db-filters (append (list :is-pull t) filters))
         (rows (forgejo-db-get-issues host owner repo db-filters))
         (alists (mapcar #'forgejo-db--row-to-issue-alist rows))
         (entries (forgejo-pull--entries alists)))
    (with-current-buffer (get-buffer-create buf-name)
      (unless (derived-mode-p 'forgejo-pull-list-mode)
        (forgejo-pull-list-mode))
      (unless forgejo-repo--host
        (setq forgejo-repo--host forgejo-host))
      (setq forgejo-repo--owner owner
            forgejo-repo--name repo
            forgejo-pull--filters filters
            tabulated-list-entries entries)
      (forgejo-tl-print t)
      (current-buffer))))

(defun forgejo-pull--sync (host owner repo filters buf-name
                                &optional force)
  "Fetch PRs from API and update DB, then re-render BUF-NAME.
When FORCE is non-nil, mark missing PRs as closed."
  (let ((endpoint (format "repos/%s/%s/pulls" owner repo))
        (params (forgejo-pull--build-params filters)))
    (forgejo-api-get-paged
     endpoint params
     ;; Per-page: save and re-render
     (lambda (page-data _headers _page-num)
       (forgejo-db-save-issues host owner repo page-data t)
       (when (buffer-live-p (get-buffer buf-name))
         (forgejo-pull--render-from-db buf-name host owner repo filters)))
     ;; Done: close missing, set sync time, final re-render
     (lambda (all-data headers)
       (when (and force (equal (plist-get filters :state) "open"))
         (let ((numbers (mapcar (lambda (p) (alist-get 'number p)) all-data)))
           (forgejo-db-close-missing host owner repo numbers t)))
       (forgejo-db-set-sync-time host owner repo "pulls"
                                 (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                                     nil t))
       (when (buffer-live-p (get-buffer buf-name))
         (forgejo-pull--render-from-db buf-name host owner repo filters)
         (when-let* ((total (plist-get headers :total-count)))
           (with-current-buffer buf-name
             (setq forgejo-pull--total-count total))))))))

;;;###autoload
(defun forgejo-pull-list (&optional owner repo)
  "List pull requests for OWNER/REPO.
Shows cached data immediately, then syncs from the API in the background."
  (interactive)
  (let* ((context (if (and owner repo)
                      (list nil owner repo)
                    (forgejo-repo-read)))
         (host-url (or (nth 0 context) forgejo-host))
         (owner (nth 1 context))
         (repo (nth 2 context))
         (buf-name (format "*forgejo-pulls: %s/%s*" owner repo))
         (default-query (forgejo--default-filter-for
                         owner repo forgejo-pull-default-filter))
         (filters (forgejo-utils-parse-filter default-query))
         (host (url-host (url-generic-parse-url host-url))))
    (forgejo-with-host host-url
      (switch-to-buffer
       (forgejo-pull--render-from-db buf-name host owner repo filters))
      (forgejo-pull--sync host owner repo filters buf-name))))

(defun forgejo-pull-refresh ()
  "Force a full re-fetch of the current PR list from the API."
  (interactive)
  (when (and forgejo-repo--owner forgejo-repo--name)
    (let ((host (url-host (url-generic-parse-url
                           (or forgejo-repo--host forgejo-host)))))
      (forgejo-with-host forgejo-repo--host
        (forgejo-pull--sync host forgejo-repo--owner forgejo-repo--name
                            forgejo-pull--filters (buffer-name) t)))))

(defun forgejo-pull--refilter ()
  "Re-render from DB with current filters, then sync.
Forces a full re-fetch to ensure the DB has data matching the filters."
  (let ((host (url-host (url-generic-parse-url
                         (or forgejo-repo--host forgejo-host)))))
    (forgejo-with-host forgejo-repo--host
      (forgejo-pull--render-from-db (buffer-name) host
                                    forgejo-repo--owner forgejo-repo--name
                                    forgejo-pull--filters)
      (forgejo-pull--sync host forgejo-repo--owner forgejo-repo--name
                          forgejo-pull--filters (buffer-name) t))))

;;; Filter commands

(defun forgejo-pull-filter ()
  "Filter PRs using a query string with prefix completion.
Supported prefixes: state:, label:, poster:, search:.
Bare words are treated as title search.
Empty input clears all filters."
  (interactive)
  (let* ((host (url-host (url-generic-parse-url
                          (or forgejo-repo--host forgejo-host))))
         (labels (mapcar (lambda (row) (nth 4 row))
                         (forgejo-db-get-labels host forgejo-repo--owner
                                                forgejo-repo--name)))
         (authors (forgejo-db-get-authors host forgejo-repo--owner
                                          forgejo-repo--name))
         (current (if forgejo-pull--filters
                     (forgejo-utils-serialize-filter forgejo-pull--filters)
                   (forgejo--default-filter-for
                    forgejo-repo--owner forgejo-repo--name
                    forgejo-pull-default-filter)))
         (query (forgejo-utils-read-filter
                 current
                 `((state . ("open" "closed"))
                   (label . ,labels)
                   (poster . ,authors)
                   (search . nil))))
         (filters (forgejo-utils-parse-filter query)))
    (setq forgejo-pull--filters filters)
    (forgejo-pull--refilter)))

(defun forgejo-pull-filter-state ()
  "Filter PRs by state."
  (interactive)
  (let ((state (completing-read "State: " '("open" "closed" "all") nil t)))
    (setq forgejo-pull--filters
          (plist-put forgejo-pull--filters :state
                     (unless (string= state "all") state)))
    (setq forgejo-pull--filters
          (plist-put forgejo-pull--filters :page nil))
    (forgejo-pull--refilter)))

(defun forgejo-pull-filter-poster ()
  "Filter PRs by author."
  (interactive)
  (let ((poster (read-string "Author: ")))
    (setq forgejo-pull--filters
          (plist-put forgejo-pull--filters :poster
                     (unless (string-empty-p poster) poster)))
    (setq forgejo-pull--filters
          (plist-put forgejo-pull--filters :page nil))
    (forgejo-pull--refilter)))

(defun forgejo-pull-clear-filters ()
  "Reset filters to the default and refresh."
  (interactive)
  (setq forgejo-pull--filters
        (forgejo-utils-parse-filter
         (forgejo--default-filter-for
          forgejo-repo--owner forgejo-repo--name
          forgejo-pull-default-filter)))
  (forgejo-pull--refilter))

;;; Pagination

(defun forgejo-pull-next-page ()
  "Load the next page of PRs."
  (interactive)
  (let ((page (or (plist-get forgejo-pull--filters :page) 1)))
    (setq forgejo-pull--filters
          (plist-put forgejo-pull--filters :page (1+ page)))
    (forgejo-pull--refilter)))

(defun forgejo-pull-prev-page ()
  "Load the previous page of PRs."
  (interactive)
  (let ((page (or (plist-get forgejo-pull--filters :page) 1)))
    (when (> page 1)
      (setq forgejo-pull--filters
            (plist-put forgejo-pull--filters :page (1- page)))
      (forgejo-pull--refilter))))

;;; Browse

(defun forgejo-pull-browse-at-point ()
  "Open the PR at point in the browser."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-browse-pull forgejo-repo--owner forgejo-repo--name id))))

;;; ---- PR detail view ----

(defvar-local forgejo-pull--ewoc nil
  "EWOC instance for the current PR detail view.")

(defvar-local forgejo-pull--data nil
  "Full API response alist for the current PR.")

(declare-function forgejo-pull-actions "forgejo-transient.el" ())

(defvar forgejo-pull-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'forgejo-pull-view-refresh)
    (define-key map (kbd "b") #'forgejo-pull-view-browse)
    (define-key map (kbd "c") #'forgejo-pull-comment)
    (define-key map (kbd "r") #'forgejo-pull-reply)
    (define-key map (kbd "R") #'forgejo-review-submit)
    (define-key map (kbd "l") #'forgejo-pull-view-log)
    (define-key map (kbd "=") #'forgejo-pull-view-diff)
    (define-key map (kbd "f") #'forgejo-pull-view-fetch)
    (define-key map (kbd "e") #'forgejo-pull-edit)
    (define-key map (kbd "x") #'forgejo-pull-toggle-state)
    (define-key map (kbd "L") #'forgejo-pull-add-label)
    (define-key map (kbd "A") #'forgejo-pull-add-assignee)
    (define-key map (kbd "M") #'forgejo-pull-set-milestone)
    (define-key map (kbd "h") #'forgejo-pull-actions)
    (define-key map (kbd "n") #'ewoc-goto-next)
    (define-key map (kbd "p") #'ewoc-goto-prev)
    map)
  "Keymap for `forgejo-pull-view-mode'.")

(define-derived-mode forgejo-pull-view-mode special-mode
  "Forgejo PR"
  "Major mode for viewing a single Forgejo pull request."
  :group 'forgejo
  (setq-local browse-url-browser-function #'forgejo-buffer--browse-url))

(defun forgejo-pull--render-detail (buf-name owner repo pr-alist timeline-alists)
  "Render PR detail into BUF-NAME from alist data."
  (let ((nodes (forgejo-buffer--build-nodes pr-alist timeline-alists)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (forgejo-pull-view-mode)
      (unless forgejo-repo--host
        (setq forgejo-repo--host forgejo-host))
      (setq forgejo-repo--owner owner
            forgejo-repo--name repo
            forgejo-pull--data pr-alist)
      (setq forgejo-pull--ewoc
            (ewoc-create #'forgejo-buffer--pp nil nil t))
      (dolist (node nodes)
        (ewoc-enter-last forgejo-pull--ewoc node))
      (setq header-line-format
            (forgejo-buffer--header-line pr-alist))
      (goto-char (point-min))
      (current-buffer))))

(defun forgejo-pull--render-missing-html (host owner repo number
                                               buf-name restore-line)
  "Render markdown to HTML for PR entries missing body_html.
After rendering, re-render BUF-NAME and restore RESTORE-LINE."
  (let* ((context (format "%s/%s" owner repo))
         (pr (forgejo-db-get-issue host owner repo number))
         (tl-rows (forgejo-db-get-timeline host owner repo number))
         (tl-alists (mapcar #'forgejo-db--row-to-timeline-alist tl-rows))
         (pending 0)
         (render-done
          (lambda ()
            (cl-decf pending)
            (when (<= pending 0)
              (forgejo-buffer--re-render
               buf-name host owner repo number
               #'forgejo-pull--render-detail restore-line)))))
    ;; PR body
    (when (and pr
               (not (alist-get 'body_html pr))
               (alist-get 'body pr))
      (cl-incf pending)
      (forgejo-api-render-markdown-async
       (alist-get 'body pr) context
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
    ;; Nothing to render
    (when (zerop pending)
      (funcall render-done))))

(defun forgejo-pull--sync-detail (host owner repo number buf-name
                                      &optional restore-line)
  "Sync PR NUMBER from API in background, re-render BUF-NAME if changed.
When RESTORE-LINE is non-nil, go to that line after re-rendering."
  (let ((pr-endpoint (format "repos/%s/%s/pulls/%d" owner repo number))
        (tl-endpoint (format "repos/%s/%s/issues/%d/timeline"
                             owner repo number)))
    (forgejo-api-get
     pr-endpoint nil
     (lambda (pr-data _headers)
       (forgejo-db-save-issues host owner repo (list pr-data) t)
       (forgejo-api-get
        tl-endpoint
        (list (cons "limit" (number-to-string forgejo-timeline-page-size)))
        (lambda (timeline _tl-headers)
          (forgejo-db-save-timeline host owner repo number timeline)
          ;; First render with whatever we have
          (forgejo-buffer--re-render
           buf-name host owner repo number
           #'forgejo-pull--render-detail restore-line)
          ;; Fetch review comments, then render missing HTML
          (let ((tl-alists (mapcar #'forgejo-db--row-to-timeline-alist
                                   (forgejo-db-get-timeline host owner repo number))))
            (forgejo-review-sync-comments
             host owner repo number tl-alists
             (lambda ()
               (forgejo-buffer--re-render
                buf-name host owner repo number
                #'forgejo-pull--render-detail restore-line)
               (forgejo-pull--render-missing-html
                host owner repo number buf-name restore-line))))))))))

(defun forgejo-pull-view-at-point ()
  "View the PR at point in the list."
  (interactive)
  (when-let* ((number (tabulated-list-get-id)))
    (forgejo-pull-view forgejo-repo--owner forgejo-repo--name number)))

;;;###autoload
(defun forgejo-pull-view (owner repo number)
  "View pull request NUMBER in OWNER/REPO.
Shows cached data from DB instantly, syncs in background."
  (interactive
   (let ((context (forgejo-repo-read)))
     (list (nth 1 context) (nth 2 context) (read-number "PR number: "))))
  (let* ((host (url-host (url-generic-parse-url
                          (or forgejo-repo--host forgejo-host))))
         (buf-name (format "*forgejo-pr: %s/%s#%d*" owner repo number))
         (pr-alist (forgejo-db-get-issue host owner repo number))
         (tl-rows (forgejo-db-get-timeline host owner repo number))
         (tl-alists (mapcar #'forgejo-db--row-to-timeline-alist tl-rows)))
    (forgejo-with-host forgejo-repo--host
      (if pr-alist
          (switch-to-buffer
           (forgejo-pull--render-detail buf-name owner repo
                                        pr-alist tl-alists))
        (with-current-buffer (get-buffer-create buf-name)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "Loading..."))
          (forgejo-pull-view-mode)
          (setq forgejo-repo--host forgejo-host
                forgejo-repo--owner owner
                forgejo-repo--name repo)
          (switch-to-buffer (current-buffer))))
      (forgejo-pull--sync-detail host owner repo number buf-name))))

(defun forgejo-pull-view-refresh ()
  "Force sync the current PR detail view."
  (interactive)
  (when-let* ((data forgejo-pull--data)
              (number (alist-get 'number data)))
    (let ((host (url-host (url-generic-parse-url
                           (or forgejo-repo--host forgejo-host))))
          (line (line-number-at-pos)))
      (forgejo-with-host forgejo-repo--host
        (forgejo-pull--sync-detail host forgejo-repo--owner
                                   forgejo-repo--name number
                                   (buffer-name) line)))))

(defun forgejo-pull-view-browse ()
  "Open the current PR in the browser."
  (interactive)
  (when-let* ((data forgejo-pull--data)
              (number (alist-get 'number data)))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-browse-pull forgejo-repo--owner forgejo-repo--name number))))

(defun forgejo-pull-comment ()
  "Post a comment on the current PR."
  (interactive)
  (when-let* ((data forgejo-pull--data)
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

(defun forgejo-pull-reply ()
  "Reply to the comment or review at point."
  (interactive)
  (when-let* ((node (forgejo-buffer--node-at-point forgejo-pull--ewoc))
              (data forgejo-pull--data)
              (number (alist-get 'number data))
              (refresh (forgejo--post-action-callback)))
    (pcase (plist-get node :type)
      ('review-link
       ;; Open the thread buffer where reply is available
       (forgejo-buffer--open-review-thread))
      (_
       ;; Regular issue comment reply
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
              (funcall refresh)))))))))

(defun forgejo-pull-toggle-state ()
  "Toggle the state of the PR at point or in the current view."
  (interactive)
  (let* ((number (or (and (bound-and-true-p forgejo-pull--data)
                          (alist-get 'number forgejo-pull--data))
                     (tabulated-list-get-id)))
         (host (url-host (url-generic-parse-url
                          (or forgejo-repo--host forgejo-host))))
         (pr (forgejo-db-get-issue host forgejo-repo--owner
                                   forgejo-repo--name number))
         (state (alist-get 'state pr)))
    (when (and number state)
      (forgejo-with-host forgejo-repo--host
        (forgejo-utils-toggle-state
         forgejo-repo--owner forgejo-repo--name number state
         (forgejo--post-action-callback))))))

(declare-function forgejo-token "forgejo.el" ())
(declare-function forgejo-vc-fetch "forgejo-vc.el" (n))
(declare-function forgejo-vc--repo-from-remote "forgejo-vc.el" ())

(defun forgejo-pull-view-diff ()
  "Show the full diff for the current pull request."
  (interactive)
  (when-let* ((data forgejo-pull--data)
              (number (alist-get 'number data))
              (owner forgejo-repo--owner)
              (repo forgejo-repo--name))
    (forgejo-with-host forgejo-repo--host
      (let* ((url (format "%s/api/v1/repos/%s/%s/pulls/%d.diff"
                          forgejo-host owner repo number))
             (url-request-method "GET")
             (url-request-extra-headers
              `(("Authorization" . ,(encode-coding-string
                                     (concat "token " (forgejo-token)) 'ascii)))))
        (url-retrieve
         url
         (lambda (_status)
           (goto-char (point-min))
           (re-search-forward "\r?\n\r?\n" nil t)
           (let ((diff-text (buffer-substring-no-properties (point) (point-max)))
                 (buf-name (format "*forgejo-diff: %s/%s#%d*" owner repo number)))
             (kill-buffer (current-buffer))
             (with-current-buffer (get-buffer-create buf-name)
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert diff-text))
               (diff-mode)
               (use-local-map forgejo-buffer-diff-map)
               (setq buffer-read-only t
                     forgejo-diff--owner owner
                     forgejo-diff--repo repo
                     forgejo-diff--pr-number number)
               (goto-char (point-min))
               (switch-to-buffer (current-buffer)))))
         nil t)))))

(defun forgejo-pull-view-fetch ()
  "Fetch and check out the current PR branch.
Only works when `default-directory' is inside the same repository."
  (interactive)
  (when-let* ((data forgejo-pull--data)
              (number (alist-get 'number data)))
    (let ((remote (forgejo-vc--repo-from-remote)))
      (if (and remote
               (string= (nth 1 remote) forgejo-repo--owner)
               (string= (nth 2 remote) forgejo-repo--name))
          (forgejo-vc-fetch number)
        (user-error "Not in %s/%s directory; cd there first"
                    forgejo-repo--owner forgejo-repo--name)))))

(defun forgejo-pull-view-log ()
  "Show commits for the current pull request."
  (interactive)
  (when-let* ((data forgejo-pull--data)
              (number (alist-get 'number data))
              (owner forgejo-repo--owner)
              (repo forgejo-repo--name))
    (forgejo-with-host forgejo-repo--host
      (forgejo-api-get
       (format "repos/%s/%s/pulls/%d/commits" owner repo number)
       nil
       (lambda (commits _headers)
         (let ((buf-name (format "*forgejo-log: %s/%s#%d*" owner repo number)))
           (with-current-buffer (get-buffer-create buf-name)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert (propertize
                        (format "Commits for %s/%s#%d\n\n" owner repo number)
                        'face 'bold))
               (dolist (commit commits)
                 (let* ((sha (alist-get 'sha commit))
                        (msg (alist-get 'message
                                        (alist-get 'commit commit)))
                        (author (alist-get 'name
                                           (alist-get 'author
                                                      (alist-get 'commit commit))))
                        (date (alist-get 'date
                                         (alist-get 'author
                                                    (alist-get 'commit commit))))
                        (short-sha (substring sha 0 (min 8 (length sha))))
                        (subject (car (split-string (or msg "") "\n"))))
                   (insert (propertize short-sha
                                       'face 'font-lock-constant-face
                                       'forgejo-commit-sha sha
                                       'keymap forgejo-buffer-commit-map
                                       'mouse-face 'highlight
                                       'help-echo "RET or = : view diff")
                           " "
                           (propertize (or author "") 'face 'shadow)
                           " "
                           (propertize (forgejo-buffer--relative-time date)
                                       'face 'shadow)
                           "\n  "
                           subject
                           "\n\n"))))
             (special-mode)
             (setq-local forgejo-repo--host forgejo-repo--host
                         forgejo-repo--owner owner
                         forgejo-repo--name repo)
             (use-local-map (let ((map (make-sparse-keymap)))
                              (set-keymap-parent map special-mode-map)
                              (define-key map (kbd "=") #'forgejo-buffer-view-commit-diff)
                              (define-key map (kbd "RET") #'forgejo-buffer-view-commit-diff)
                              map))
             (goto-char (point-min))
             (switch-to-buffer (current-buffer)))))))))

;;; Review

(defun forgejo-pull-submit-review ()
  "Submit a review on the current pull request."
  (interactive)
  (when-let* ((data forgejo-pull--data)
              (number (alist-get 'number data)))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-submit-review
       forgejo-repo--owner forgejo-repo--name number
       (forgejo--post-action-callback)))))

;;; Edit

(defun forgejo-pull-edit ()
  "Edit the body or comment at point."
  (interactive)
  (when-let* ((node (forgejo-buffer--node-at-point forgejo-pull--ewoc))
              (data forgejo-pull--data)
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

(defun forgejo-pull-add-label ()
  "Add a label to the current pull request."
  (interactive)
  (when-let* ((data forgejo-pull--data)
              (number (alist-get 'number data))
              (host (url-host (url-generic-parse-url
                               (or forgejo-repo--host forgejo-host)))))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-add-label
       forgejo-repo--owner forgejo-repo--name number host
       (forgejo--post-action-callback)))))

(defun forgejo-pull-remove-label ()
  "Remove a label from the current pull request."
  (interactive)
  (when-let* ((data forgejo-pull--data)
              (number (alist-get 'number data))
              (labels (alist-get 'labels data))
              (host (url-host (url-generic-parse-url
                               (or forgejo-repo--host forgejo-host)))))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-remove-label
       forgejo-repo--owner forgejo-repo--name number labels host
       (forgejo--post-action-callback)))))

(defun forgejo-pull-add-assignee ()
  "Add an assignee to the current pull request."
  (interactive)
  (when-let* ((data forgejo-pull--data)
              (number (alist-get 'number data))
              (host (url-host (url-generic-parse-url
                               (or forgejo-repo--host forgejo-host)))))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-add-assignee
       forgejo-repo--owner forgejo-repo--name number
       (alist-get 'assignees data) host
       (forgejo--post-action-callback)))))

(defun forgejo-pull-remove-assignee ()
  "Remove an assignee from the current pull request."
  (interactive)
  (when-let* ((data forgejo-pull--data)
              (number (alist-get 'number data))
              (assignees (alist-get 'assignees data)))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-remove-assignee
       forgejo-repo--owner forgejo-repo--name number assignees
       (forgejo--post-action-callback)))))

(defun forgejo-pull-set-milestone ()
  "Set or clear the milestone on the current pull request."
  (interactive)
  (when-let* ((data forgejo-pull--data)
              (number (alist-get 'number data))
              (host (url-host (url-generic-parse-url
                               (or forgejo-repo--host forgejo-host)))))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-set-milestone
       forgejo-repo--owner forgejo-repo--name number host
       (forgejo--post-action-callback)))))

(provide 'forgejo-pull)
;;; forgejo-pull.el ends here
