;;; forgejo-view.el --- Shared detail-view layer for issues and PRs  -*- lexical-binding: t; -*-

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

;; Shared buffer-locals, render functions, and action commands for issue
;; and PR detail views.  Both `forgejo-issue-view-mode' and
;; `forgejo-pull-view-mode' set the buffer-locals defined here, so
;; action commands can be written once.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'diff-mode)
(require 'ewoc)
(require 'text-property-search)
(require 'url-parse)
(require 'keymap-popup)
(require 'forgejo)
(require 'forgejo-tl)
(require 'forgejo-buffer)
(require 'forgejo-filter)
(require 'forgejo-utils)
(require 'forgejo-api)
(require 'forgejo-db)

(declare-function forgejo-token "forgejo.el" (host-url))

(defvar forgejo-repo--host)
(defvar forgejo-repo--owner)
(defvar forgejo-repo--name)

;;; Shared buffer-locals

(defvar-local forgejo-view--data nil
  "Item alist for the current detail view (issue or PR).")

(defconst forgejo-view--reaction-types
  '("+1" "-1" "laugh" "hooray" "confused" "heart" "rocket" "eyes")
  "The 8 reaction types supported by Forgejo.")

(defvar-local forgejo-view--ewoc nil
  "EWOC instance for the current detail view.")

(defvar-local forgejo-view--sync-fn nil
  "Function to sync the current item from the API.
Called with (HOST OWNER REPO NUMBER BUF-NAME &optional RESTORE-LINE).")

(defvar-local forgejo-view--browse-fn nil
  "Function to open the current item in the browser.
Called with (HOST-URL OWNER REPO NUMBER).")

(defun forgejo-view--popup-description (prefix &optional filters)
  "Return a popup description string: PREFIX owner/repo#NUM.
Falls back to PREFIX owner/repo or just PREFIX.
When FILTERS is a non-nil plist, appends the serialized filter string."
  (let ((owner (and (bound-and-true-p forgejo-repo--owner) forgejo-repo--owner))
        (repo (and (bound-and-true-p forgejo-repo--name) forgejo-repo--name))
        (num (and (bound-and-true-p forgejo-view--data)
                  (alist-get 'number forgejo-view--data)))
        (filter-str (and filters
                         (forgejo-filter-serialize filters))))
    (concat prefix
            (when (and owner repo)
              (concat " "
                      (propertize (format "%s/%s" owner repo)
                                  'face 'font-lock-type-face)
                      (when num
                        (propertize (format "#%d" num)
                                    'face 'font-lock-constant-face))))
            (when (and filter-str (not (string-empty-p filter-str)))
              (concat " " filter-str)))))

;;; Text-property action map

(defvar forgejo-view-action-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'forgejo-view-activate-at-point)
    (define-key map [mouse-1] #'forgejo-view-activate-at-point)
    map)
  "Keymap for interactive text elements in detail views.
Dispatches based on text properties at point.")

(defun forgejo-view-activate-at-point ()
  "Activate the interactive element at point.
Dispatches to the appropriate handler based on text properties."
  (interactive)
  (cond
   ((get-text-property (point) 'forgejo-ref-number)
    (forgejo-view-follow-ref))
   ((get-text-property (point) 'forgejo-commit-sha)
    (forgejo-view-commit-diff))
   ((get-text-property (point) 'forgejo-compare-range)
    (forgejo-view-compare-diff))
   ((get-text-property (point) 'forgejo-review-id)
    (forgejo-view-open-review-thread))
   (t (message "Edit history not yet implemented"))))

;;; Text-property commands

(defun forgejo-view-follow-ref ()
  "Follow the reference link at point.
Uses the ref-repo text property for cross-repo references."
  (interactive)
  (when-let* ((number (get-text-property (point) 'forgejo-ref-number)))
    (let* ((full-name (get-text-property (point) 'forgejo-ref-repo))
           (parts (when full-name (split-string full-name "/")))
           (owner (or (nth 0 parts) forgejo-repo--owner))
           (repo (or (nth 1 parts) forgejo-repo--name)))
      (forgejo-view-item owner repo number))))

(defvar-local forgejo-diff--owner nil "Owner of the repo for this diff.")
(defvar-local forgejo-diff--repo nil "Repo name for this diff.")
(defvar-local forgejo-diff--pr-number nil "PR number for this diff.")

(declare-function forgejo-review-diff-comment "forgejo-review.el" ())

(keymap-popup-define forgejo-view-diff-map
  :description (lambda ()
                 (if (and forgejo-diff--owner forgejo-diff--repo
                          forgejo-diff--pr-number)
                     (concat "Diff "
                             (propertize (format "%s/%s"
                                                 forgejo-diff--owner
                                                 forgejo-diff--repo)
                                         'face 'font-lock-type-face)
                             (propertize (format "#%d" forgejo-diff--pr-number)
                                         'face 'font-lock-constant-face))
                   "Diff"))
  :parent diff-mode-map
  "q" ("Quit" quit-window)
  "c" ("Review comment" forgejo-review-diff-comment))

(defun forgejo-view--local-commit-p (sha)
  "Return non-nil if SHA exists in the local git repository."
  (zerop (call-process "git" nil nil nil "cat-file" "-t" sha)))

(defun forgejo-view--show-diff-buffer (buf-name text)
  "Display TEXT in BUF-NAME using `diff-mode'."
  (with-current-buffer (get-buffer-create buf-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (decode-coding-string text 'utf-8 t)))
    (diff-mode)
    (use-local-map forgejo-view-diff-map)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))))

(defun forgejo-view-commit-diff ()
  "View the diff for the commit at point.
Tries local git first, falls back to the API."
  (interactive)
  (when-let* ((sha (get-text-property (point) 'forgejo-commit-sha)))
    (let ((buf-name (format "*forgejo-diff: %s*"
                            (substring sha 0 (min 8 (length sha))))))
      (if (forgejo-view--local-commit-p sha)
          (let ((text (with-temp-buffer
                        (call-process "git" nil t nil "show" sha)
                        (buffer-string))))
            (forgejo-view--show-diff-buffer buf-name text))
        (forgejo-view--commit-diff-api sha buf-name)))))

(defun forgejo-view--commit-diff-api (sha buf-name)
  "Fetch the diff for SHA via the API and display in BUF-NAME."
  (let* ((host-url forgejo-repo--host)
         (owner forgejo-repo--owner)
         (repo forgejo-repo--name)
         (url (format "%s/api/v1/repos/%s/%s/git/commits/%s.diff"
                      host-url owner repo sha))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(encode-coding-string
                                 (concat "token " (forgejo-token host-url))
                                 'ascii)))))
    (url-retrieve
     url
     (lambda (_status)
       (unwind-protect
           (progn
             (goto-char (point-min))
             (re-search-forward "\r?\n\r?\n" nil t)
             (let ((diff-text (buffer-substring-no-properties (point) (point-max))))
               (if (string-empty-p (string-trim diff-text))
                   (user-error "Commit %s not found" sha)
                 (forgejo-view--show-diff-buffer buf-name diff-text))))
         (forgejo-api--kill-url-buffer (current-buffer))))
     nil t)))

(defun forgejo-view-compare-diff ()
  "View the diff between old and new commits of a force push at point."
  (interactive)
  (when-let* ((range (get-text-property (point) 'forgejo-compare-range))
              (old (car range))
              (new (cdr range)))
    (let* ((host-url forgejo-repo--host)
           (owner forgejo-repo--owner)
           (repo forgejo-repo--name)
           (short-old (substring old 0 (min 8 (length old))))
           (short-new (substring new 0 (min 8 (length new))))
           (buf-name (format "*forgejo-diff: %s..%s*" short-old short-new))
           (url (format "%s/%s/%s/compare/%s..%s.diff"
                        host-url owner repo old new))
           (url-request-method "GET")
           (url-request-extra-headers
            `(("Authorization" . ,(encode-coding-string
                                   (concat "token " (forgejo-token host-url))
                                   'ascii)))))
      (url-retrieve
       url
       (lambda (_status)
         (unwind-protect
             (progn
               (goto-char (point-min))
               (re-search-forward "\r?\n\r?\n" nil t)
               (let ((diff-text (buffer-substring-no-properties (point) (point-max))))
                 (if (string-empty-p (string-trim diff-text))
                     (message "No diff between %s and %s" short-old short-new)
                   (forgejo-view--show-diff-buffer buf-name diff-text))))
           (forgejo-api--kill-url-buffer (current-buffer))))
       nil t))))

;;; URL routing

(defun forgejo-view--parse-forgejo-url (url)
  "Parse a Forgejo issue/PR URL into (OWNER REPO NUMBER), or nil."
  (when (and url (string-match
                  "/\\([^/]+\\)/\\([^/]+\\)/\\(?:issues\\|pulls\\)/\\([0-9]+\\)"
                  url))
    (list (match-string 1 url)
          (match-string 2 url)
          (string-to-number (match-string 3 url)))))

(defun forgejo-view-browse-url (url &rest _args)
  "Open URL in forgejo.el if it's a Forgejo issue/PR, else in browser."
  (if-let* ((parsed (forgejo-view--parse-forgejo-url url)))
      (forgejo-view-item (nth 0 parsed) (nth 1 parsed) (nth 2 parsed))
    (browse-url-default-browser url)))

(defun forgejo-view-follow-link ()
  "Follow the link at point.
Handles #N issue/PR refs and markdown URLs."
  (interactive)
  (cond
   ((get-text-property (point) 'forgejo-ref-number)
    (forgejo-view-follow-ref))
   ((forgejo-buffer--url-at-point)
    (forgejo-view-browse-url (forgejo-buffer--url-at-point)))
   (t (user-error "No link at point"))))

(declare-function forgejo-review-open-thread "forgejo-review.el" ())

(defun forgejo-view-open-review-thread ()
  "Open the review thread for the review link at point."
  (interactive)
  (forgejo-review-open-thread))

;;; EWOC navigation

(defun forgejo-view-next ()
  "Move to the next EWOC node."
  (interactive)
  (ewoc-goto-next forgejo-view--ewoc 1))

(defun forgejo-view-previous ()
  "Move to the previous EWOC node."
  (interactive)
  (ewoc-goto-prev forgejo-view--ewoc 1))

;;; Link navigation

(defun forgejo-view-next-link ()
  "Move to the next link in the buffer."
  (interactive)
  (if-let* ((match (text-property-search-forward 'keymap nil nil t)))
      (goto-char (prop-match-beginning match))
    (message "No next link")))

(defun forgejo-view-previous-link ()
  "Move to the previous link in the buffer."
  (interactive)
  (if-let* ((match (text-property-search-backward 'keymap nil nil t)))
      (goto-char (prop-match-beginning match))
    (message "No previous link")))

;;; Shared view keymap

(keymap-popup-define forgejo-view-mode-map
  :description (lambda () (forgejo-view--popup-description "Forgejo"))
  :parent special-mode-map
  :group "Actions"
  "c" ("Comment" forgejo-view-comment)
  "e" ("Edit at point" forgejo-view-edit)
  "x" ("Toggle open/close" forgejo-view-toggle-state)
  "D" ("Delete at point" forgejo-view-delete-at-point)
  "b" ("Open in browser" forgejo-view-browse)
  "g" ("Refresh" forgejo-view-refresh)
  :group "Metadata"
  "!" ("React" forgejo-view-react)
  "L" ("Labels" forgejo-view-label)
  "A" ("Add assignee" forgejo-view-add-assignee)
  "M" ("Set milestone" forgejo-view-set-milestone)
  "P" ("Toggle pin" forgejo-view-toggle-pin)
  :group "Navigate"
  "RET" ("Follow link" forgejo-view-follow-link)
  "TAB" ("Next link" forgejo-view-next-link)
  "<backtab>" ("Prev link" forgejo-view-previous-link)
  "n" ("Next" forgejo-view-next)
  "p" ("Previous" forgejo-view-previous))

;;; Node access

(defun forgejo-view--node-at-point ()
  "Return the data plist of the EWOC node at point, or nil."
  (forgejo-buffer--node-at-point forgejo-view--ewoc))

;;; Item routing

(declare-function forgejo-issue-view "forgejo-issue.el"
                  (owner repo number))
(declare-function forgejo-pull-view "forgejo-pull.el"
                  (owner repo number))

(defun forgejo-view-item (owner repo number)
  "View issue or PR NUMBER in OWNER/REPO.
Checks the DB first, then the API if uncached."
  (let* ((host (url-host (url-generic-parse-url forgejo-repo--host)))
         (cached (forgejo-db-get-issue host owner repo number)))
    (if cached
        (if (alist-get 'pull_request cached)
            (forgejo-pull-view owner repo number)
          (forgejo-issue-view owner repo number))
      (forgejo-api-get
       forgejo-repo--host
       (format "repos/%s/%s/issues/%d" owner repo number) nil
       (lambda (data _headers)
         (when data
           (forgejo-db-save-issues host owner repo (list data))
           (if (alist-get 'pull_request data)
               (forgejo-pull-view owner repo number)
             (forgejo-issue-view owner repo number))))))))

;;; List-view format

(defun forgejo-view--list-format (columns)
  "Build a `tabulated-list-format' vector from COLUMNS.
Each element of COLUMNS is (NAME WIDTH-OR-RATIO SORT . PROPS).
Float widths are multiplied by `window-width'."
  (let ((w (window-width)))
    (apply #'vector
           (mapcar (lambda (col)
                     (let* ((name (car col))
                            (width-spec (nth 1 col))
                            (sort (nth 2 col))
                            (props (nthcdr 3 col))
                            (width (if (floatp width-spec)
                                       (truncate (* w width-spec))
                                     width-spec)))
                       (append (list name width sort) props)))
                   columns))))

;;; List-view rendering

(defun forgejo-view--render-from-db (buf-name host-url host owner repo
					      filters query-fn list-mode)
  "Render cached items into BUF-NAME from the DB.
HOST-URL is the full instance URL.  HOST is the hostname string.
OWNER, REPO identify the repository.  FILTERS is the filter plist.
QUERY-FN is called with (HOST OWNER REPO FILTERS) to fetch rows.
LIST-MODE is the major mode symbol for the list buffer."
  (let ((entries (forgejo-filter-list-entries
                  (funcall query-fn host owner repo filters))))
    (with-current-buffer (get-buffer-create buf-name)
      (unless (derived-mode-p list-mode)
        (funcall list-mode))
      (setq forgejo-repo--host host-url
            forgejo-repo--owner owner
            forgejo-repo--name repo
            tabulated-list-format (forgejo-view--list-format
                                   forgejo-filter-list-columns)
            tabulated-list-entries entries)
      (unless tabulated-list-sort-key
        (setq tabulated-list-sort-key '("#" . t)))
      (tabulated-list-init-header)
      (forgejo-tl-print t)
      (current-buffer))))

;;; Detail-view rendering

(defun forgejo-view--render-detail (buf-name host-url owner repo item-alist
					     timeline-alists view-mode-fn
					     sync-fn browse-fn)
  "Render detail view into BUF-NAME from alist data.
HOST-URL is the full instance URL.  ITEM-ALIST is the issue or PR data.
TIMELINE-ALISTS is the timeline.  VIEW-MODE-FN activates the major mode.
SYNC-FN and BROWSE-FN are stored as buffer-locals for action commands."
  (let ((nodes (forgejo-buffer--build-nodes item-alist timeline-alists)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (funcall view-mode-fn)
      (forgejo-buffer-setup forgejo-view-action-map)
      (setq forgejo-repo--host host-url
            forgejo-repo--owner owner
            forgejo-repo--name repo
            forgejo-view--data item-alist
            forgejo-view--sync-fn sync-fn
            forgejo-view--browse-fn browse-fn)
      (setq forgejo-view--ewoc
            (ewoc-create #'forgejo-buffer--pp nil nil t))
      (dolist (node nodes)
        (ewoc-enter-last forgejo-view--ewoc node))
      (setq header-line-format
            (forgejo-buffer--header-line item-alist))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (current-buffer))))

;;; Reaction sync

(defun forgejo-view--sync-reactions (host-url host owner repo number
                                              buf-name timeline)
  "Fetch reactions for issue NUMBER and its comments, update EWOC.
Fires all requests in parallel.  TIMELINE is the list of timeline
event alists (used to extract comment IDs)."
  (let* ((comment-ids (cl-loop for event in timeline
                               when (string= "comment"
                                             (alist-get 'type event))
                               collect (alist-get 'id event)))
         (remaining (cons (1+ (length comment-ids)) nil))
         (finish (lambda ()
                   (when (zerop (cl-decf (car remaining)))
                     (when (buffer-live-p (get-buffer buf-name))
                       (with-current-buffer buf-name
                         (forgejo-buffer--update-reactions
                          forgejo-view--ewoc host owner repo number)))))))
    ;; Issue body reactions
    (forgejo-api-get
     host-url
     (format "repos/%s/%s/issues/%d/reactions" owner repo number)
     nil
     (lambda (data _headers)
       (forgejo-db-save-reactions host owner repo number 0 data)
       (funcall finish))
     :error-callback (lambda (_err) (funcall finish)))
    ;; Comment reactions (parallel)
    (dolist (cid comment-ids)
      (forgejo-api-get
       host-url
       (format "repos/%s/%s/issues/comments/%d/reactions" owner repo cid)
       nil
       (lambda (data _headers)
         (forgejo-db-save-reactions host owner repo number cid data)
         (funcall finish))
       :error-callback (lambda (_err) (funcall finish))))))

;;; Re-render from DB

(defun forgejo-view--re-render (buf-name host-url host owner repo number
					 render-fn &optional restore-line)
  "Re-render detail buffer BUF-NAME from fresh DB data.
HOST-URL is the instance.  HOST is the hostname.
RENDER-FN is called with (BUF-NAME HOST-URL OWNER REPO ITEM TIMELINE).
Restores point to RESTORE-LINE if given."
  (when (buffer-live-p (get-buffer buf-name))
    (let* ((issue (forgejo-db-get-issue host owner repo number))
           (tl-rows (forgejo-db-get-timeline host owner repo number))
           (tl-alists (mapcar #'forgejo-db--row-to-timeline-alist tl-rows)))
      (funcall render-fn buf-name host-url owner repo issue tl-alists)
      (when restore-line
        (with-current-buffer buf-name
          (goto-char (point-min))
          (forward-line (1- restore-line)))))))

;;; Action commands

(defun forgejo-view-toggle-state ()
  "Toggle the state of the item at point or in the current view."
  (interactive)
  (let* ((number (or (and (bound-and-true-p forgejo-view--data)
                          (alist-get 'number forgejo-view--data))
                     (tabulated-list-get-id)))
         (host-url forgejo-repo--host)
         (host (url-host (url-generic-parse-url host-url)))
         (item (forgejo-db-get-issue host forgejo-repo--owner
                                     forgejo-repo--name number))
         (state (alist-get 'state item)))
    (when (and number state)
      (forgejo-utils-toggle-state
       host-url forgejo-repo--owner forgejo-repo--name number state
       (forgejo--post-action-callback)))))

(defun forgejo-view-toggle-pin ()
  "Toggle pin state of the current issue or PR.
Works from both detail and list views.
Updates the API, then updates pin_order in the DB."
  (interactive)
  (let* ((number (or (and (bound-and-true-p forgejo-view--data)
                          (alist-get 'number forgejo-view--data))
                     (tabulated-list-get-id)))
         (host-url forgejo-repo--host)
         (host (url-host (url-generic-parse-url host-url)))
         (owner forgejo-repo--owner)
         (repo forgejo-repo--name))
    (when number
      (let* ((item (forgejo-db-get-issue host owner repo number))
             (pin-order (alist-get 'pin_order item))
             (pinned-p (and pin-order (> pin-order 0))))
        (forgejo-utils-toggle-pin
         host-url owner repo number pinned-p
         (lambda ()
           (forgejo-db-set-pin-order host owner repo number
                                     (if pinned-p 0 1))))))))

(defun forgejo-view-comment ()
  "Post a comment on the current item."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (refresh (forgejo--post-action-callback)))
    (forgejo-utils-post-comment
     forgejo-repo--host
     (format "repos/%s/%s/issues/%d/comments"
             forgejo-repo--owner forgejo-repo--name number)
     nil
     (lambda (_data _headers)
       (message "Comment posted on %s/%s#%d"
                forgejo-repo--owner forgejo-repo--name number)
       (funcall refresh)))))

(defun forgejo-view-edit-title ()
  "Edit the title of the current item."
  (interactive)
  (when-let* ((node (forgejo-view--node-at-point))
              (number (alist-get 'number forgejo-view--data)))
    (forgejo-utils-edit-title
     forgejo-repo--host forgejo-repo--owner forgejo-repo--name number
     (plist-get node :title)
     (forgejo--post-action-callback))))

(defun forgejo-view-edit-body ()
  "Edit the body of the current item."
  (interactive)
  (when-let* ((node (forgejo-view--node-at-point))
              (number (alist-get 'number forgejo-view--data)))
    (forgejo-utils-edit-body
     forgejo-repo--host forgejo-repo--owner forgejo-repo--name number
     (plist-get node :body)
     (forgejo--post-action-callback))))

(defun forgejo-view-edit-base ()
  "Change the base branch of the current pull request."
  (interactive)
  (when-let* ((number (alist-get 'number forgejo-view--data)))
    (forgejo-utils-edit-base
     forgejo-repo--host forgejo-repo--owner forgejo-repo--name number
     (forgejo--post-action-callback))))

(keymap-popup-define forgejo-view-edit-map
  :description (lambda ()
                 (if-let* ((num (and (bound-and-true-p forgejo-view--data)
                                     (alist-get 'number forgejo-view--data))))
                     (concat "Edit "
                             (propertize (concat "#" (number-to-string num))
                                         'face 'font-lock-constant-face))
                   "Edit"))
  :group "Actions"
  "t" ("Title" forgejo-view-edit-title)
  "b" ("Body" forgejo-view-edit-body)
  "B" ("Base branch" forgejo-view-edit-base
       :if (lambda () (alist-get 'pull_request forgejo-view--data))))

(defun forgejo-view-edit ()
  "Edit the item at point.
On the header, shows edit menu.  On a comment, edits the body."
  (interactive)
  (when-let* ((node (forgejo-view--node-at-point)))
    (pcase (plist-get node :type)
      ('header (keymap-popup forgejo-view-edit-map))
      ('comment
       (forgejo-utils-edit-comment
        forgejo-repo--host forgejo-repo--owner forgejo-repo--name
        (plist-get node :id) (plist-get node :body)
        (forgejo--post-action-callback)))
      (_ (user-error "No editable item at point")))))

(defun forgejo-view-label ()
  "Add or remove labels on the current item.
Works from both detail and list views."
  (interactive)
  (when-let* ((number (or (and (bound-and-true-p forgejo-view--data)
                               (alist-get 'number forgejo-view--data))
                          (tabulated-list-get-id)))
              (host (url-host (url-generic-parse-url forgejo-repo--host))))
    (forgejo-utils-label
     forgejo-repo--host forgejo-repo--owner forgejo-repo--name
     number host (forgejo--post-action-callback))))

(defun forgejo-view-react ()
  "Add or remove reactions on the comment or issue body at point.
Each reaction type is prefixed with + (to add) or - (to remove)."
  (interactive)
  (when-let* ((node (forgejo-view--node-at-point))
              (type (plist-get node :type))
              (number (alist-get 'number forgejo-view--data))
              (host-url forgejo-repo--host)
              (host (url-host (url-generic-parse-url host-url)))
              (owner forgejo-repo--owner)
              (repo forgejo-repo--name))
    (let* ((comment-id (pcase type
                         ('header 0)
                         ('comment (plist-get node :id))
                         (_ (user-error "No reactable item at point"))))
           (current (plist-get node :reactions))
           (current-types (mapcar #'car current))
           (candidates (mapcar (lambda (r)
                                 (if (member r current-types)
                                     (concat "-" r)
                                   (concat "+" r)))
                               forgejo-view--reaction-types))
           (selected (mapcar #'string-trim
                             (completing-read-multiple
                              "Reactions (+add -remove): "
                              candidates nil nil)))
           (endpoint (if (= comment-id 0)
                         (format "repos/%s/%s/issues/%d/reactions"
                                 owner repo number)
                       (format "repos/%s/%s/issues/comments/%d/reactions"
                               owner repo comment-id)))
           (buf-name (buffer-name)))
      (dolist (sel selected)
        (let ((op (substring sel 0 1))
              (content (substring sel 1)))
          (pcase op
            ("+"
             (forgejo-api-post
              host-url endpoint nil
              `((content . ,content))
              (lambda (_data _headers)
                (message "Added %s reaction" content)
                (forgejo-view--refresh-reactions
                 host-url host owner repo number comment-id buf-name))))
            ("-"
             (forgejo-api-delete
              host-url endpoint
              `((content . ,content))
              (lambda (_data _headers)
                (message "Removed %s reaction" content)
                (forgejo-view--refresh-reactions
                 host-url host owner repo number comment-id buf-name))))
            (_
             (user-error "Invalid reaction: %s (use +name or -name)" sel))))))))

(defun forgejo-view--refresh-reactions (host-url host owner repo number
                                                 comment-id buf-name)
  "Refetch reactions for COMMENT-ID and update the EWOC node."
  (let ((endpoint (if (= comment-id 0)
                      (format "repos/%s/%s/issues/%d/reactions"
                              owner repo number)
                    (format "repos/%s/%s/issues/comments/%d/reactions"
                            owner repo comment-id))))
    (forgejo-api-get
     host-url endpoint nil
     (lambda (data _headers)
       (forgejo-db-save-reactions host owner repo number comment-id data)
       (when (buffer-live-p (get-buffer buf-name))
         (with-current-buffer buf-name
           (forgejo-buffer--update-reactions
            forgejo-view--ewoc host owner repo number)))))))

(defun forgejo-view-add-assignee ()
  "Add an assignee to the current item."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (host (url-host (url-generic-parse-url forgejo-repo--host))))
    (forgejo-utils-add-assignee
     forgejo-repo--host forgejo-repo--owner forgejo-repo--name number
     (alist-get 'assignees data) host
     (forgejo--post-action-callback))))

(defun forgejo-view-remove-assignee ()
  "Remove an assignee from the current item."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (assignees (alist-get 'assignees data)))
    (forgejo-utils-remove-assignee
     forgejo-repo--host forgejo-repo--owner forgejo-repo--name number
     assignees (forgejo--post-action-callback))))

(defun forgejo-view-set-milestone ()
  "Set or clear the milestone on the current item."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (host (url-host (url-generic-parse-url forgejo-repo--host))))
    (forgejo-utils-set-milestone
     forgejo-repo--host forgejo-repo--owner forgejo-repo--name number host
     (forgejo--post-action-callback))))

(defun forgejo-view-refresh ()
  "Force sync the current detail view."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (sync-fn forgejo-view--sync-fn))
    (let ((host (url-host (url-generic-parse-url forgejo-repo--host)))
          (line (line-number-at-pos)))
      (funcall sync-fn host forgejo-repo--owner
               forgejo-repo--name number
               (buffer-name) line))))

(defun forgejo-view-browse ()
  "Open the current item in the browser."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (browse-fn forgejo-view--browse-fn))
    (let ((browse-url-browser-function #'browse-url-default-browser))
      (funcall browse-fn forgejo-repo--host forgejo-repo--owner
               forgejo-repo--name number))))

;;; Delete

(defun forgejo-view-delete-at-point ()
  "Delete the item at point.
On a comment, deletes the comment.  On the header, deletes the issue/PR."
  (interactive)
  (when-let* ((node (forgejo-view--node-at-point))
              (type (plist-get node :type)))
    (pcase type
      ('comment
       (forgejo-view--delete
        (format "repos/%s/%s/issues/comments/%d"
                forgejo-repo--owner forgejo-repo--name (plist-get node :id))
        (format "comment %d" (plist-get node :id))
        (forgejo--post-action-callback)))
      ('header
       (let ((number (alist-get 'number forgejo-view--data))
             (owner forgejo-repo--owner)
             (repo forgejo-repo--name)
             (host (url-host (url-generic-parse-url forgejo-repo--host))))
         (forgejo-view--delete
          (format "repos/%s/%s/issues/%d" owner repo number)
          (format "%s/%s#%d" owner repo number)
          (lambda ()
            (forgejo-db-delete-issue host owner repo number)
            (quit-window 'kill)))))
      (_ (user-error "No deletable item at point")))))

(defun forgejo-view-delete-issue ()
  "Delete the issue or PR at point in a list view.
Removes from API, then from DB, then re-renders the list in place."
  (interactive)
  (when-let* ((number (tabulated-list-get-id)))
    (let ((owner forgejo-repo--owner)
          (repo forgejo-repo--name)
          (host (url-host (url-generic-parse-url forgejo-repo--host)))
          (buf (current-buffer)))
      (forgejo-view--delete
       (format "repos/%s/%s/issues/%d" owner repo number)
       (format "%s/%s#%d" owner repo number)
       (lambda ()
         (forgejo-db-delete-issue host owner repo number)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (setq tabulated-list-entries
                   (cl-remove-if (lambda (e) (= (car e) number))
                                 tabulated-list-entries))
             (forgejo-tl-print t))))))))

(defun forgejo-view--delete (endpoint description callback)
  "Delete ENDPOINT after confirming with DESCRIPTION.  Call CALLBACK on success."
  (when (y-or-n-p (format "Delete %s? " description))
    (forgejo-api-delete
     forgejo-repo--host endpoint nil
     (lambda (_data _headers)
       (message "Deleted %s" description)
       (when callback (funcall callback))))))

(provide 'forgejo-view)
;;; forgejo-view.el ends here
