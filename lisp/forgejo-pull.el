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
(require 'url-parse)
(require 'forgejo)
(require 'forgejo-tl)
(require 'forgejo-view)
(require 'forgejo-buffer)
(require 'forgejo-filter)
(require 'forgejo-utils)
(require 'forgejo-api)
(require 'forgejo-db)
(require 'forgejo-review)

(declare-function forgejo-repo-read "forgejo-repo.el" ())

(defvar forgejo-default-sort)
(defvar forgejo-timeline-page-size)
(defvar forgejo-pull-default-filter)
(defvar forgejo-repo--host)
(defvar forgejo-repo--owner)
(defvar forgejo-repo--name)

;;; ---- PR list view ----

(defvar-local forgejo-pull--filters nil
  "Plist of current PR filter state.
Keys: :state :milestone :labels :author :page")

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
    (define-key map (kbd "x") #'forgejo-view-toggle-state)
    (define-key map (kbd "b") #'forgejo-pull-browse-at-point)
    map)
  "Keymap for `forgejo-pull-list-mode'.")

(define-derived-mode forgejo-pull-list-mode tabulated-list-mode
  "Forgejo PRs"
  "Major mode for browsing Forgejo pull requests."
  :group 'forgejo
  (setq tabulated-list-padding 1
        tabulated-list-format (forgejo-filter-list-format
                               forgejo-filter-list-columns)
        tabulated-list-sort-key '("#" . t))
  (tabulated-list-init-header))

(defun forgejo-pull--build-params (filters)
  "Build API query params from FILTERS plist for PR sync."
  (forgejo-filter-build-params "pulls" filters))

(defun forgejo-pull--render-from-db (buf-name host-url host owner repo filters)
  "Render cached PRs into BUF-NAME from the DB.
HOST-URL is the full instance URL.  HOST is the hostname."
  (forgejo-view--render-from-db buf-name host-url host owner repo filters
                                #'forgejo-filter-query-pulls
                                'forgejo-pull-list-mode))

(defun forgejo-pull--sync (host-url host owner repo filters buf-name
                                &optional force)
  "Fetch PRs from API and update DB, then re-render BUF-NAME.
HOST-URL is the instance.  HOST is the hostname.
Uses the issues endpoint with type=pulls for incremental sync.
When FORCE is nil, use `since' for incremental sync.
When FORCE is non-nil, fetch all and mark missing PRs as closed."
  (let* ((since (unless force
                  (forgejo-db-get-sync-time host owner repo "pulls")))
         (api-filters (if since
                         (plist-put (copy-sequence filters) :since since)
                       filters))
         (endpoint (format "repos/%s/%s/issues" owner repo))
         (params (forgejo-pull--build-params api-filters)))
    ;; Sync labels in background
    (forgejo-api-get
     host-url (format "repos/%s/%s/labels" owner repo) nil
     (lambda (data _headers)
       (when data (forgejo-db-save-labels host owner repo data))))
    (forgejo-api-get-paged
     host-url endpoint params
     ;; Per-page: save to DB, re-render only on first page
     (lambda (page-data _headers page-num)
       (forgejo-db-save-issues host owner repo page-data)
       (when (and (= page-num 1) (buffer-live-p (get-buffer buf-name)))
         (with-current-buffer buf-name
           (forgejo-pull--render-from-db
            buf-name host-url host owner repo forgejo-pull--filters))))
     ;; Done: close missing, set sync time, final re-render
     (lambda (all-data headers)
       (when (and force (equal (plist-get filters :state) "open"))
         (let ((numbers (mapcar (lambda (p) (alist-get 'number p)) all-data)))
           (forgejo-db-close-missing host owner repo numbers t)))
       (forgejo-db-set-sync-time host owner repo "pulls"
                                 (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                                     nil t))
       (when (buffer-live-p (get-buffer buf-name))
         (with-current-buffer buf-name
           (forgejo-pull--render-from-db
            buf-name host-url host owner repo forgejo-pull--filters)
           (when-let* ((total (plist-get headers :total-count)))
             (setq forgejo-pull--total-count total))))))))

;;;###autoload
(defun forgejo-pull-list (&optional owner repo)
  "List pull requests for OWNER/REPO.
Shows cached data immediately, then syncs from the API in the background."
  (interactive)
  (let* ((context (if (and owner repo)
                      (list nil owner repo)
                    (forgejo-repo-read)))
         (host-url (or (nth 0 context) (forgejo--resolve-host)))
         (owner (nth 1 context))
         (repo (nth 2 context))
         (buf-name (format "*forgejo-pulls: %s/%s*" owner repo))
         (default-query (forgejo--default-filter-for
                         owner repo forgejo-pull-default-filter))
         (filters (forgejo-filter-parse default-query))
         (host (url-host (url-generic-parse-url host-url))))
    (switch-to-buffer
     (forgejo-pull--render-from-db buf-name host-url host owner repo filters))
    (with-current-buffer buf-name
      (setq forgejo-pull--filters filters))
    (forgejo-pull--sync host-url host owner repo filters buf-name)))

(defun forgejo-pull-refresh ()
  "Force a full re-fetch of the current PR list from the API."
  (interactive)
  (when (and forgejo-repo--owner forgejo-repo--name)
    (forgejo-filter-refresh (buffer-name) forgejo-repo--host
                            forgejo-repo--owner forgejo-repo--name
                            forgejo-pull--filters
                            #'forgejo-pull--render-from-db
                            #'forgejo-pull--sync)))

(defun forgejo-pull--refilter ()
  "Re-render from DB with current filters, then sync."
  (forgejo-filter-refresh (buffer-name) forgejo-repo--host
                          forgejo-repo--owner forgejo-repo--name
                          forgejo-pull--filters
                          #'forgejo-pull--render-from-db
                          #'forgejo-pull--sync))

;;; Filter commands

(defun forgejo-pull-filter ()
  "Filter PRs using a query string with prefix completion."
  (interactive)
  (let* ((host (url-host (url-generic-parse-url forgejo-repo--host)))
         (completions (forgejo-filter-completions
                       host forgejo-repo--owner forgejo-repo--name))
         (current (if forgejo-pull--filters
                     (forgejo-filter-serialize forgejo-pull--filters)
                   (forgejo--default-filter-for
                    forgejo-repo--owner forgejo-repo--name
                    forgejo-pull-default-filter)))
         (query (forgejo-filter-read current completions))
         (filters (forgejo-filter-parse query)))
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

(defun forgejo-pull-filter-author ()
  "Filter PRs by author."
  (interactive)
  (let ((author (read-string "Author: ")))
    (setq forgejo-pull--filters
          (plist-put forgejo-pull--filters :author
                     (unless (string-empty-p author) author)))
    (setq forgejo-pull--filters
          (plist-put forgejo-pull--filters :page nil))
    (forgejo-pull--refilter)))

(defun forgejo-pull-clear-filters ()
  "Reset filters to the default and refresh."
  (interactive)
  (setq forgejo-pull--filters
        (forgejo-filter-parse
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
    (forgejo-utils-browse-pull forgejo-repo--host
                               forgejo-repo--owner forgejo-repo--name id)))

;;; ---- PR detail view ----

(declare-function forgejo-pull-actions "forgejo-transient.el" ())

(defvar forgejo-pull-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'forgejo-view-refresh)
    (define-key map (kbd "b") #'forgejo-view-browse)
    (define-key map (kbd "c") #'forgejo-view-comment)
    (define-key map (kbd "r") #'forgejo-pull-reply)
    (define-key map (kbd "R") #'forgejo-review-submit)
    (define-key map (kbd "l") #'forgejo-pull-view-log)
    (define-key map (kbd "=") #'forgejo-pull-view-diff)
    (define-key map (kbd "f") #'forgejo-pull-view-fetch)
    (define-key map (kbd "e") #'forgejo-view-edit)
    (define-key map (kbd "x") #'forgejo-view-toggle-state)
    (define-key map (kbd "L") #'forgejo-view-add-label)
    (define-key map (kbd "A") #'forgejo-view-add-assignee)
    (define-key map (kbd "M") #'forgejo-view-set-milestone)
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

(defun forgejo-pull--render-detail (buf-name host-url owner repo pr-alist
                                    timeline-alists)
  "Render PR detail into BUF-NAME from alist data.
HOST-URL is the instance URL."
  (forgejo-view--render-detail buf-name host-url owner repo pr-alist
                               timeline-alists
                               #'forgejo-pull-view-mode
                               #'forgejo-pull--sync-detail
                               #'forgejo-utils-browse-pull))

(defun forgejo-pull--sync-detail (host owner repo number buf-name
                                      &optional restore-line)
  "Sync PR NUMBER from API in background, re-render BUF-NAME if changed.
When RESTORE-LINE is non-nil, go to that line after re-rendering."
  (let ((host-url (forgejo--host-url-for-hostname host))
        (pr-endpoint (format "repos/%s/%s/pulls/%d" owner repo number))
        (tl-endpoint (format "repos/%s/%s/issues/%d/timeline"
                             owner repo number)))
    (forgejo-api-get
     host-url pr-endpoint nil
     (lambda (pr-data _headers)
       (forgejo-db-save-issues host owner repo (list pr-data) t)
       (forgejo-api-get
        host-url tl-endpoint
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
             host-url host owner repo number tl-alists
             (lambda ()
               (forgejo-buffer--re-render
                buf-name host owner repo number
                #'forgejo-pull--render-detail restore-line)
               (forgejo-view--render-missing-html
                host-url host owner repo number buf-name restore-line
                #'forgejo-pull--render-detail))))))))))

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
  (let* ((host-url (or forgejo-repo--host (forgejo--resolve-host)))
         (host (url-host (url-generic-parse-url host-url)))
         (buf-name (format "*forgejo-pr: %s/%s#%d*" owner repo number))
         (pr-alist (forgejo-db-get-issue host owner repo number))
         (tl-rows (forgejo-db-get-timeline host owner repo number))
         (tl-alists (mapcar #'forgejo-db--row-to-timeline-alist tl-rows)))
    (if pr-alist
        (switch-to-buffer
         (forgejo-pull--render-detail buf-name host-url owner repo
                                      pr-alist tl-alists))
      (with-current-buffer (get-buffer-create buf-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Loading..."))
        (forgejo-pull-view-mode)
        (setq forgejo-repo--host host-url
              forgejo-repo--owner owner
              forgejo-repo--name repo)
        (switch-to-buffer (current-buffer))))
    (forgejo-pull--sync-detail host owner repo number buf-name)))

;;; PR-specific commands

(defun forgejo-pull-reply ()
  "Reply to the comment or review at point."
  (interactive)
  (when-let* ((node (forgejo-view--node-at-point))
              (data forgejo-view--data)
              (number (alist-get 'number data))
              (refresh (forgejo--post-action-callback)))
    (pcase (plist-get node :type)
      ('review-link
       (forgejo-buffer--open-review-thread))
      (_
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
            (funcall refresh))))))))

(declare-function forgejo-token "forgejo.el" (host-url))
(declare-function forgejo-vc-fetch "forgejo-vc.el" (n))
(declare-function forgejo-vc--repo-from-remote "forgejo-vc.el" ())

(defun forgejo-pull-view-diff ()
  "Show the full diff for the current pull request."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (owner forgejo-repo--owner)
              (repo forgejo-repo--name)
              (host-url forgejo-repo--host))
    (let* ((url (format "%s/api/v1/repos/%s/%s/pulls/%d.diff"
                        host-url owner repo number))
           (url-request-method "GET")
           (url-request-extra-headers
            `(("Authorization" . ,(encode-coding-string
                                   (concat "token " (forgejo-token host-url))
                                   'ascii)))))
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
	     (read-only-mode 1)
             (setq forgejo-diff--owner owner
                   forgejo-diff--repo repo
                   forgejo-diff--pr-number number)
             (goto-char (point-min))
             (switch-to-buffer (current-buffer)))))
       nil t))))

(defun forgejo-pull-view-fetch ()
  "Fetch and check out the current PR branch.
Only works when `default-directory' is inside the same repository."
  (interactive)
  (when-let* ((data forgejo-view--data)
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
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (owner forgejo-repo--owner)
              (repo forgejo-repo--name)
              (host-url forgejo-repo--host))
    (forgejo-api-get
     host-url
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
           (setq-local forgejo-repo--host host-url
                       forgejo-repo--owner owner
                       forgejo-repo--name repo)
           (use-local-map (let ((map (make-sparse-keymap)))
                            (set-keymap-parent map special-mode-map)
                            (define-key map (kbd "=") #'forgejo-buffer-view-commit-diff)
                            (define-key map (kbd "RET") #'forgejo-buffer-view-commit-diff)
                            map))
           (goto-char (point-min))
           (switch-to-buffer (current-buffer))))))))

(provide 'forgejo-pull)
;;; forgejo-pull.el ends here
