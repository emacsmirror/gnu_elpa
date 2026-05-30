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
(require 'bug-reference)
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
Called with (HOST OWNER REPO NUMBER BUF-NAME &optional RESTORE-LINE COMMENT-ID).")

(defvar-local forgejo-view--browse-fn nil
  "Function to open the current item in the browser.
Called with (HOST-URL OWNER REPO NUMBER).")

(defvar-local forgejo-view--target-comment-id nil
  "Comment id to scroll to on (re-)render of the current detail view.
Set when entering the view from a #issuecomment-N deeplink, and kept
sticky so async sync re-renders land back on the same comment.")

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

(defvar-local forgejo-diff--pr-number nil "PR number for this diff.")

(declare-function forgejo-review-diff-comment "forgejo-review.el" ())

(keymap-popup-define forgejo-view-diff-map
  :description (lambda ()
                 (if (and forgejo-repo--owner forgejo-repo--name
                          forgejo-diff--pr-number)
                     (concat "Diff "
                             (propertize (format "%s/%s"
                                                 forgejo-repo--owner
                                                 forgejo-repo--name)
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

(defun forgejo-view--show-diff-buffer (buf-name text host-url owner repo
                                                &optional pr-number)
  "Display TEXT in BUF-NAME using `diff-mode'.
HOST-URL, OWNER, and REPO set the repo context for the diff buffer.
PR-NUMBER, when non-nil, marks this as a PR diff for review comments."
  (with-current-buffer (get-buffer-create buf-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (decode-coding-string text 'utf-8 t)))
    (setq buffer-read-only t)
    (diff-mode)
    (use-local-map forgejo-view-diff-map)
    ;; HACK: diff-mode-read-only-map shadows our popup key "h" with
    ;; describe-mode.  Override just that binding via the higher-priority
    ;; minor-mode-overriding-map-alist, keyed on buffer-read-only.
    (let ((map (make-sparse-keymap)))
      (keymap-set map "h" (lambda () (interactive) (keymap-popup forgejo-view-diff-map)))
      (setq-local minor-mode-overriding-map-alist
                  (list (cons 'buffer-read-only map))))
    (setq forgejo-repo--host host-url
          forgejo-repo--owner owner
          forgejo-repo--name repo
          forgejo-diff--pr-number pr-number)
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))))

(defun forgejo-view-commit-diff ()
  "View the diff for the commit at point.
Tries local git first, falls back to the API."
  (interactive)
  (when-let* ((sha (get-text-property (point) 'forgejo-commit-sha)))
    (let ((buf-name (format "*forgejo-diff: %s*"
                            (substring sha 0 (min 8 (length sha)))))
          (host-url forgejo-repo--host)
          (owner forgejo-repo--owner)
          (repo forgejo-repo--name))
      (if (forgejo-view--local-commit-p sha)
          (let ((text (with-temp-buffer
                        (call-process "git" nil t nil "show" sha)
                        (buffer-string))))
            (forgejo-view--show-diff-buffer buf-name text host-url owner repo))
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
                 (forgejo-view--show-diff-buffer
                  buf-name diff-text host-url owner repo))))
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
                   (forgejo-view--show-diff-buffer
                    buf-name diff-text host-url owner repo))))
           (forgejo-api--kill-url-buffer (current-buffer))))
       nil t))))

;;; URL routing

(defun forgejo-view--parse-forgejo-url (url)
  "Parse a Forgejo issue/PR URL into (OWNER REPO NUMBER COMMENT-ID).
COMMENT-ID is nil unless URL has a #issuecomment-N fragment."
  (and url
       (string-match
        (concat "/\\([^/]+\\)/\\([^/]+\\)/\\(?:issues\\|pulls\\)/\\([0-9]+\\)"
                "\\(?:/?#issuecomment-\\([0-9]+\\)\\)?")
        url)
       (list (match-string 1 url)
             (match-string 2 url)
             (string-to-number (match-string 3 url))
             (and (match-string 4 url)
                  (string-to-number (match-string 4 url))))))

(defun forgejo-view--url-target-at-point ()
  "Return (HOST-URL OWNER REPO NUMBER COMMENT-ID) for Forgejo URL at point."
  (when-let* ((url (forgejo-buffer--url-at-point))
              (parsed (forgejo-view--parse-forgejo-url url))
              (url-obj (url-generic-parse-url url))
              (protocol (url-type url-obj))
              (host (url-host url-obj)))
    (cons (format "%s://%s" protocol host) parsed)))

(defun forgejo-view-browse-url (url &rest _args)
  "Open URL in forgejo.el if it's a Forgejo issue/PR, else in browser."
  (if-let* ((parsed (forgejo-view--parse-forgejo-url url))
            (url-obj (url-generic-parse-url url))
            (forgejo-repo--host (format "%s://%s"
                                        (url-type url-obj)
                                        (url-host url-obj))))
      (forgejo-view-item (nth 0 parsed) (nth 1 parsed) (nth 2 parsed)
                         (nth 3 parsed))
    (browse-url-default-browser url)))

(defun forgejo-view--browse-url-handler (url &rest _args)
  "Handle URL if it points to a Forgejo issue or PR.
Returns non-nil if handled, nil otherwise.  Only matches hosts
configured in `forgejo-hosts'."
  (when-let* ((parsed (forgejo-view--parse-forgejo-url url))
              (url-obj (url-generic-parse-url url))
              (host (url-host url-obj))
              (known (cl-find host forgejo-hosts
                              :key (lambda (e)
                                     (url-host
                                      (url-generic-parse-url (car e))))
                              :test #'string=))
              (forgejo-repo--host (format "%s://%s"
                                          (url-type url-obj) host)))
    (forgejo-view-item (nth 0 parsed) (nth 1 parsed) (nth 2 parsed)
                       (nth 3 parsed))
    t))

;;;###autoload
(define-minor-mode forgejo-browse-mode
  "Open Forgejo issue/PR URLs in forgejo.el instead of the browser.
When enabled, URLs matching configured `forgejo-hosts' are opened
in forgejo.el buffers rather than the web browser."
  :lighter " Forgejo"
  (if forgejo-browse-mode
      (add-function :before-until (local 'browse-url-browser-function)
                    #'forgejo-view--browse-url-handler)
    (remove-function (local 'browse-url-browser-function)
                     #'forgejo-view--browse-url-handler)))

;;;###autoload
(define-globalized-minor-mode global-forgejo-browse-mode
  forgejo-browse-mode forgejo-browse-mode
  :group 'forgejo)

;;; bug-reference-forge-alist integration

(defvar forgejo-view--added-forge-hosts nil
  "Hosts added to `bug-reference-forge-alist' by forgejo.el.")

(defun forgejo-view--bug-reference-setup ()
  "Add `forgejo-hosts' entries to `bug-reference-forge-alist'."
  (dolist (entry forgejo-hosts)
    (let* ((url-obj (url-generic-parse-url (car entry)))
           (host-domain (url-host url-obj))
           (protocol (url-type url-obj)))
      (unless (assoc host-domain bug-reference-forge-alist)
        (push host-domain forgejo-view--added-forge-hosts)
        (push (list host-domain 'gitea protocol)
              bug-reference-forge-alist)))))

(defun forgejo-view-follow-link ()
  "Follow the link at point.
Handles #N issue/PR refs and markdown URLs."
  (interactive)
  (if-let* ((target (forgejo-view--url-target-at-point)))
      (pcase-let ((`(,host-url ,owner ,repo ,number ,comment-id) target))
        (let ((forgejo-repo--host host-url))
          (forgejo-view-item owner repo number comment-id)))
    (cond
     ((get-text-property (point) 'forgejo-ref-number)
      (forgejo-view-follow-ref))
     ((forgejo-buffer--url-at-point)
      (forgejo-view-browse-url (forgejo-buffer--url-at-point)))
     (t (user-error "No link at point")))))

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
  "u" ("Copy URL at point" forgejo-view-copy-url)
  "U" ("Toggle subscription" forgejo-view-toggle-subscription)
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

(defun forgejo-view--comment-id-at-point ()
  "Return the id of the comment node at point, or nil."
  (when-let* ((node (forgejo-view--node-at-point)))
    (and (eq (plist-get node :type) 'comment)
         (plist-get node :id))))

;;; Item routing

(declare-function forgejo-issue-view "forgejo-issue.el"
                  (owner repo number &optional comment-id))
(declare-function forgejo-pull-view "forgejo-pull.el"
                  (owner repo number &optional comment-id))

(defun forgejo-view--pr-p (alist)
  "Return non-nil if ALIST describes a pull request.
Handles both raw API form (`pull_request' is an object or :null) and
DB-normalised form (`pull_request' is t or nil)."
  (let ((pr (alist-get 'pull_request alist)))
    (and pr (not (eq pr :null)))))

(defun forgejo-view-item (owner repo number &optional comment-id)
  "View issue or PR NUMBER in OWNER/REPO.
Checks the DB first, then the API if uncached.  When COMMENT-ID is
non-nil, jump to that comment after the detail view renders."
  (let* ((host-url forgejo-repo--host)
         (host (url-host (url-generic-parse-url host-url)))
         (cached (forgejo-db-get-issue host owner repo number))
         (view (lambda (item)
                 (let ((forgejo-repo--host host-url))
                   (if (forgejo-view--pr-p item)
                       (forgejo-pull-view owner repo number comment-id)
                     (forgejo-issue-view owner repo number comment-id))))))
    (if cached
        (funcall view cached)
      (forgejo-api-get
       host-url
       (format "repos/%s/%s/issues/%d" owner repo number) nil
       (lambda (data _headers)
         (when data
           (forgejo-db-save-issues host owner repo (list data))
           (funcall view data)))))))

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

(defun forgejo-view--comment-node (ewoc comment-id)
  "Return the EWOC comment node for COMMENT-ID, or nil."
  (and ewoc
       (cl-loop for node = (ewoc-nth ewoc 0) then (ewoc-next ewoc node)
                while node
                for data = (ewoc-data node)
                when (and (eq 'comment (plist-get data :type))
                          (eql comment-id (plist-get data :id)))
                return node)))

(defun forgejo-view--goto-comment (ewoc comment-id)
  "Move point to the EWOC comment node for COMMENT-ID.
Returns non-nil when a matching comment node was found."
  (when-let* ((node (forgejo-view--comment-node ewoc comment-id)))
    (ewoc-goto-node ewoc node)
    t))

(defun forgejo-view--goto-target-comment (comment-id)
  "Move point to COMMENT-ID when non-nil."
  (and comment-id
       (forgejo-view--goto-comment forgejo-view--ewoc comment-id)))

(defun forgejo-view--comment-target-for-sync (ewoc comment-id)
  "Return COMMENT-ID when EWOC does not already contain it."
  (and comment-id
       (not (forgejo-view--comment-node ewoc comment-id))
       comment-id))

(defun forgejo-view--render-detail (buf-name host-url owner repo item-alist
					     timeline-alists view-mode-fn
					     sync-fn browse-fn
					     &optional comment-id)
  "Render detail view into BUF-NAME from alist data.
HOST-URL is the full instance URL.  OWNER and REPO identify the repo.
ITEM-ALIST is the issue or PR data.  TIMELINE-ALISTS is the timeline.
VIEW-MODE-FN activates the major mode.  SYNC-FN and BROWSE-FN are stored
as buffer-locals for action commands.  When COMMENT-ID is non-nil, jump
to that comment after rendering.

First call into a buffer does a full paint.  Subsequent calls reconcile
the existing EWOC against the new nodes, avoiding any work for
unchanged nodes."
  (let* ((nodes (forgejo-buffer--build-nodes item-alist timeline-alists))
         (existing-buf (get-buffer buf-name))
         (existing-ewoc (and existing-buf
                             (buffer-local-value 'forgejo-view--ewoc
                                                 existing-buf)))
         (host (url-host (url-generic-parse-url host-url)))
         (number (alist-get 'number item-alist)))
    (if (and existing-ewoc (ewoc-buffer existing-ewoc))
        (forgejo-view--reconcile-into existing-buf nodes item-alist
                                      host owner repo number comment-id)
      (forgejo-view--first-paint buf-name nodes item-alist host-url host
                                 owner repo number view-mode-fn
                                 sync-fn browse-fn comment-id))))

(defun forgejo-view--resolve-sticky-target (buf-name comment-id)
  "Return COMMENT-ID or BUF-NAME's existing sticky comment target."
  (or comment-id
      (and (get-buffer buf-name)
           (buffer-local-value 'forgejo-view--target-comment-id
                               (get-buffer buf-name)))))

(defun forgejo-view--install-detail-locals (host-url owner repo item-alist
                                                     sync-fn browse-fn
                                                     sticky-target)
  "Set detail locals for HOST-URL OWNER REPO ITEM-ALIST.
SYNC-FN, BROWSE-FN, and STICKY-TARGET are stored as buffer-local state."
  (setq forgejo-repo--host host-url
        forgejo-repo--owner owner
        forgejo-repo--name repo
        forgejo-view--data item-alist
        forgejo-view--sync-fn sync-fn
        forgejo-view--browse-fn browse-fn
        forgejo-view--target-comment-id sticky-target))

(defun forgejo-view--populate-ewoc (nodes)
  "Create the detail EWOC in the current buffer and insert NODES.
Stores the EWOC in `forgejo-view--ewoc' and returns it."
  (setq forgejo-view--ewoc
        (ewoc-create #'forgejo-buffer--pp nil nil t))
  (dolist (node nodes)
    (ewoc-enter-last forgejo-view--ewoc node))
  forgejo-view--ewoc)

(defun forgejo-view--first-paint (buf-name nodes item-alist host-url host
                                           owner repo number view-mode-fn
                                           sync-fn browse-fn comment-id)
  "Build a fresh detail view in BUF-NAME from NODES and ITEM-ALIST.
HOST-URL, HOST, OWNER, REPO, and NUMBER identify the issue or PR.
VIEW-MODE-FN, SYNC-FN, BROWSE-FN, and COMMENT-ID configure the view."
  (forgejo-buffer--fontify-node-bodies nodes)
  (let ((sticky-target (forgejo-view--resolve-sticky-target buf-name comment-id)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (funcall view-mode-fn)
      (forgejo-buffer-setup forgejo-view-action-map)
      (forgejo-view--install-detail-locals host-url owner repo item-alist
                                           sync-fn browse-fn sticky-target)
      (forgejo-view--populate-ewoc nodes)
      (forgejo-buffer--update-reactions
       forgejo-view--ewoc host owner repo number)
      (setq header-line-format (forgejo-buffer--header-line item-alist))
      (unless (and sticky-target
                   (forgejo-view--goto-target-comment sticky-target))
        (goto-char (point-min)))
      (set-buffer-modified-p nil)
      (current-buffer))))

(defun forgejo-view--reconcile-into (buf nodes item-alist host owner repo number
                                         &optional comment-id)
  "Reconcile EWOC in BUF against NODES for ITEM-ALIST.
HOST, OWNER, REPO, and NUMBER identify the issue or PR.  Point is left
untouched unless COMMENT-ID is non-nil.  Updates `forgejo-view--data',
reactions, and the header line only if it changed."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (setq forgejo-view--data item-alist)
      (forgejo-buffer--reconcile-ewoc forgejo-view--ewoc nodes)
      (forgejo-buffer--update-reactions
       forgejo-view--ewoc host owner repo number)
      (let ((new-header (forgejo-buffer--header-line item-alist)))
        (unless (equal header-line-format new-header)
          (setq header-line-format new-header)))
      (set-buffer-modified-p nil)
      (forgejo-view--goto-target-comment comment-id))
    buf))

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
                                         render-fn &optional restore-line comment-id)
  "Re-render detail buffer BUF-NAME from fresh DB data.
HOST-URL is the instance.  HOST is the hostname.  OWNER, REPO, and
NUMBER identify the issue or PR.  RENDER-FN is called with the cached
item, timeline, and COMMENT-ID.  COMMENT-ID takes precedence over
RESTORE-LINE when both are non-nil."
  (when (buffer-live-p (get-buffer buf-name))
    (let* ((issue (forgejo-db-get-issue host owner repo number))
           (tl-rows (forgejo-db-get-timeline host owner repo number))
           (tl-alists (mapcar #'forgejo-db--row-to-timeline-alist tl-rows)))
      (funcall render-fn buf-name host-url owner repo issue tl-alists comment-id)
      (when (and restore-line (not comment-id))
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

(declare-function forgejo-notification--parse-ref-at-point
                  "forgejo-notification.el" ())
(defvar forgejo-notification--host-url)

(defun forgejo-view--subscription-context ()
  "Return (HOST-URL OWNER REPO NUMBER) for the item at point.
Resolves from the notification list (per-row ref), issue/PR list
(entry id + buffer-local repo), or detail view (buffer-local repo +
`forgejo-view--data')."
  (cond
   ((derived-mode-p 'forgejo-notification-list-mode)
    (when-let* ((parsed (forgejo-notification--parse-ref-at-point)))
      (pcase-let ((`(,owner ,repo ,number) parsed))
        (list forgejo-notification--host-url owner repo number))))
   ((bound-and-true-p forgejo-view--data)
    (when-let* ((number (alist-get 'number forgejo-view--data)))
      (list forgejo-repo--host forgejo-repo--owner
            forgejo-repo--name number)))
   (t
    (when-let* ((number (tabulated-list-get-id)))
      (list forgejo-repo--host forgejo-repo--owner
            forgejo-repo--name number)))))

(defun forgejo-view--apply-subscription (host-url owner repo number
                                                  user subscribed)
  "Subscribe or unsubscribe USER based on SUBSCRIBED state."
  (if subscribed
      (forgejo-api-issue-unsubscribe
       host-url owner repo number user
       (lambda ()
         (message "Unsubscribed from %s/%s#%d" owner repo number)))
    (forgejo-api-issue-subscribe
     host-url owner repo number user
     (lambda ()
       (message "Subscribed to %s/%s#%d" owner repo number)))))

(defun forgejo-view-toggle-subscription ()
  "Toggle subscription for the issue or PR at point.
Works from issue, PR, and notification list views, and from the
detail view."
  (interactive)
  (pcase (forgejo-view--subscription-context)
    (`(,host-url ,owner ,repo ,number)
     (forgejo-api-current-user
      host-url
      (lambda (user)
        (if (not user)
            (message "Cannot resolve current user on %s" host-url)
          (forgejo-api-issue-subscription-check
           host-url owner repo number
           (lambda (subscribed)
             (forgejo-view--apply-subscription
              host-url owner repo number user subscribed)))))))
    (_ (user-error "No issue or PR at point"))))

(defun forgejo-view-comment ()
  "Post a comment on the current item."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (host forgejo-repo--host)
              (owner forgejo-repo--owner)
              (name forgejo-repo--name)
              (refresh (forgejo--post-action-callback)))
    (forgejo-utils-post-comment
     host
     (format "repos/%s/%s/issues/%d/comments" owner name number)
     nil
     (lambda (_data _headers)
       (message "Comment posted on %s/%s#%d" owner name number)
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

(defun forgejo-view--react-mine (current me)
  "Return reaction types in CURRENT that ME has placed.
CURRENT is the grouped reactions alist for a node, each entry
shaped as (CONTENT USER...).  Returns nil when ME is nil."
  (and me
       (cl-loop for (content . users) in current
                when (member me users) collect content)))

(defun forgejo-view--react-candidates (mine)
  "Build the +/- reaction candidate list given MINE.
MINE is the list of reaction types the current user has placed;
those get a leading \"-\" (remove), the rest get \"+\" (add)."
  (mapcar (lambda (r) (concat (if (member r mine) "-" "+") r))
          forgejo-view--reaction-types))

(defun forgejo-view--react-parse-selection (raw)
  "Parse RAW selection strings into (VERB . CONTENT) pairs.
VERB is `add' or `remove'.  Signals `user-error' on garbage."
  (mapcar (lambda (sel)
            (pcase (and (> (length sel) 1) (substring sel 0 1))
              ("+" (cons 'add (substring sel 1)))
              ("-" (cons 'remove (substring sel 1)))
              (_ (user-error
                  "Invalid reaction: %s (use +name or -name)" sel))))
          raw))

(defun forgejo-view--react-endpoint (owner repo number comment-id)
  "Return the reactions endpoint for COMMENT-ID, or the issue body if 0."
  (if (= comment-id 0)
      (format "repos/%s/%s/issues/%d/reactions" owner repo number)
    (format "repos/%s/%s/issues/comments/%d/reactions"
            owner repo comment-id)))

(defun forgejo-view--react-dispatch (host-url endpoint ops finish)
  "Dispatch reaction OPS asynchronously against ENDPOINT on HOST-URL.
OPS is a list of (VERB . CONTENT) where VERB is `add' or `remove'.
FINISH is invoked once per completed request."
  (dolist (op ops)
    (pcase-let ((`(,verb . ,content) op))
      (pcase verb
        ('add
         (forgejo-api-post
          host-url endpoint nil
          `((content . ,content))
          (lambda (_data _headers)
            (message "Added %s reaction" content)
            (funcall finish))))
        ('remove
         (forgejo-api-delete
          host-url endpoint
          `((content . ,content))
          (lambda (_data _headers)
            (message "Removed %s reaction" content)
            (funcall finish))))))))

(defun forgejo-view--react-prompt (host-url host owner repo number
                                            comment-id current me buf-name)
  "Prompt for reactions on COMMENT-ID and dispatch them as ME.
Only types ME has placed in CURRENT show the remove (-) option;
when ME is nil, only the add (+) option is offered.  Fires one
refresh after all dispatched requests complete."
  (unless me
    (message "Cannot resolve current user; remove (-) options disabled"))
  (let* ((mine (forgejo-view--react-mine current me))
         (candidates (forgejo-view--react-candidates mine))
         (raw (mapcar #'string-trim
                      (completing-read-multiple
                       "Reactions (+add -remove): "
                       candidates nil nil)))
         (ops (forgejo-view--react-parse-selection raw))
         (endpoint (forgejo-view--react-endpoint owner repo number comment-id))
         (remaining (cons (length ops) nil))
         (finish (lambda ()
                   (when (zerop (cl-decf (car remaining)))
                     (forgejo-view--refresh-reactions
                      host-url host owner repo number comment-id buf-name)))))
    (when (null ops)
      (user-error "No reactions selected"))
    (forgejo-view--react-dispatch host-url endpoint ops finish)))

(defun forgejo-view-react ()
  "Add or remove reactions on the comment or issue body at point.
Each reaction type is prefixed with + (to add) or - (to remove).
Only types you have already reacted with show the remove option."
  (interactive)
  (when-let* ((node (forgejo-view--node-at-point))
              (type (plist-get node :type))
              (number (alist-get 'number forgejo-view--data))
              (host-url forgejo-repo--host)
              (host (url-host (url-generic-parse-url host-url)))
              (owner forgejo-repo--owner)
              (repo forgejo-repo--name))
    (let ((comment-id (pcase type
                        ('header 0)
                        ('comment (plist-get node :id))
                        (_ (user-error "No reactable item at point"))))
          (current (plist-get node :reactions))
          (buf-name (buffer-name)))
      (forgejo-api-current-user
       host-url
       (lambda (me)
         (forgejo-view--react-prompt
          host-url host owner repo number comment-id current me buf-name))))))

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
  "Force sync the current detail view.
Refreshes wall-clock-dependent display (relative timestamps) from
cached data immediately, then fires the async sync.  Per-node
invalidation keeps point on the current comment; the sync render uses
that comment id as its explicit target."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (sync-fn forgejo-view--sync-fn))
    (let ((comment-id (forgejo-view--comment-id-at-point)))
      (when forgejo-view--ewoc
        (forgejo-buffer--ewoc-invalidate-all forgejo-view--ewoc))
      (let ((host (url-host (url-generic-parse-url forgejo-repo--host))))
        (funcall sync-fn host forgejo-repo--owner
                 forgejo-repo--name number
                 (buffer-name) nil comment-id)))))

(defun forgejo-view-copy-url ()
  "Copy the URL at point to the kill ring."
  (interactive)
  (if-let* ((url (forgejo-buffer--url-at-point)))
      (progn (kill-new url)
             (message "Copied: %s" url))
    (user-error "No URL at point")))

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

(forgejo-view--bug-reference-setup)

(provide 'forgejo-view)
;;; forgejo-view.el ends here
