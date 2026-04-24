;;; forgejo-filter.el --- Filter parsing, query, and formatting for Forgejo  -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure data layer for filter operations.  Provides parsing, serialization,
;; DB query pipelines, and tabulated-list entry formatting.  No buffer-local
;; state mutation -- callers own their filter variables.

;;; Code:

(require 'cl-lib)
(require 'url-parse)
(require 'forgejo-db)

(defvar crm-separator)

;; Buffer formatting helpers (loaded at runtime by consumer modules).
(declare-function forgejo-buffer--format-state "forgejo-buffer.el" (state))
(declare-function forgejo-buffer--format-labels "forgejo-buffer.el" (labels))
(declare-function forgejo-buffer--relative-time "forgejo-buffer.el" (time-string))
(declare-function forgejo-buffer--login "forgejo-buffer.el" (user-alist))

;;; ---- Prefix / key maps ----

(defconst forgejo-filter--prefix-map
  '(("state" . :state)
    ("label" . :labels)
    ("milestone" . :milestone)
    ("author" . :author)
    ("search" . :query))
  "Prefix map for parsing issue and PR filter queries.")

(defconst forgejo-filter--key-map
  '((:state . "state")
    (:labels . "label")
    (:milestone . "milestone")
    (:author . "author"))
  "Key map for serializing issue and PR filter plists.")

(defconst forgejo-filter--notification-prefix-map
  '(("status" . :status)
    ("type" . :type)
    ("repo" . :repo))
  "Prefix map for parsing notification filter queries.")

(defconst forgejo-filter--notification-key-map
  '((:status . "status")
    (:type . "type")
    (:repo . "repo"))
  "Key map for serializing notification filter plists.")

;;; ---- Parse / serialize ----

(defun forgejo-filter-parse (query-string &optional prefix-map)
  "Parse QUERY-STRING into a filter plist.
Each token of the form PREFIX:VALUE is mapped through PREFIX-MAP
\(an alist of (STRING . KEYWORD)).  Bare words are collected as :query.
PREFIX-MAP defaults to `forgejo-filter--prefix-map'."
  (let ((map (or prefix-map forgejo-filter--prefix-map))
        (tokens (split-string (or query-string "") " " t))
        result bare-words)
    (dolist (token tokens)
      (if (string-match "\\`\\([^:]+\\):\\(.*\\)\\'" token)
          (let* ((prefix (match-string 1 token))
                 (value (match-string 2 token))
                 (key (cdr (assoc prefix map))))
            (when (and key (not (string-empty-p value)))
              (setq result (plist-put result key value))))
        (push token bare-words)))
    (when bare-words
      (setq result (plist-put result :query
                              (mapconcat #'identity
                                         (nreverse bare-words) " "))))
    result))

(defun forgejo-filter-serialize (filters &optional key-map)
  "Serialize FILTERS plist to a query string.
KEY-MAP is an alist of (KEYWORD . PREFIX-STRING).
Defaults to `forgejo-filter--key-map'.
The :query value, if present, is appended as bare words."
  (let ((map (or key-map forgejo-filter--key-map))
        parts)
    (cl-loop for (key . prefix) in map
             for val = (plist-get filters key)
             when val do (push (concat prefix ":" val) parts))
    (when-let* ((query (plist-get filters :query)))
      (push query parts))
    (mapconcat #'identity (nreverse parts) " ")))

;;; ---- Minibuffer prompt ----

(defun forgejo-filter-read (default-query completions-alist)
  "Read a filter query string with prefix-aware completion.
DEFAULT-QUERY is the initial input.  COMPLETIONS-ALIST is an alist
of (PREFIX . VALUES) where VALUES is a list of strings or nil for
free-text prefixes.  Returns the query string."
  (let* ((candidates
          (cl-loop for (prefix . values) in completions-alist
                   if values
                   append (mapcar (lambda (v)
                                    (concat (symbol-name prefix) ":" v))
                                  values)
                   else collect (concat (symbol-name prefix) ":")))
         (crm-separator ",")
         (initial (replace-regexp-in-string " " ","
                                            (or default-query "")))
         (selections (completing-read-multiple
                      "Filter: " candidates nil nil initial)))
    (mapconcat #'identity selections " ")))

;;; ---- DB query pipelines ----

(defun forgejo-filter-query-issues (host owner repo filters)
  "Return issue alists for HOST/OWNER/REPO matching FILTERS.
Excludes pull requests.  Result is a list of API-shaped alists."
  (let ((db-filters (append (list :no-pulls t) filters)))
    (mapcar #'forgejo-db--row-to-issue-alist
            (forgejo-db-get-issues host owner repo db-filters))))

(defun forgejo-filter-query-pulls (host owner repo filters)
  "Return PR alists for HOST/OWNER/REPO matching FILTERS.
Result is a list of API-shaped alists."
  (let ((db-filters (append (list :is-pull t) filters)))
    (mapcar #'forgejo-db--row-to-issue-alist
            (forgejo-db-get-issues host owner repo db-filters))))

(defun forgejo-filter--match-notification (row filters)
  "Return non-nil if notification ROW matches FILTERS plist.
ROW layout: 0=id 1=subject_type 2=subject_title 3=subject_url
            4=subject_state 5=repo_owner 6=repo_name 7=status 8=updated_at."
  (and (or (null (plist-get filters :status))
           (string= (or (nth 7 row) "") (plist-get filters :status)))
       (or (null (plist-get filters :type))
           (string= (downcase (or (nth 1 row) ""))
                    (downcase (plist-get filters :type))))
       (or (null (plist-get filters :repo))
           (string= (format "%s/%s" (nth 5 row) (nth 6 row))
                    (plist-get filters :repo)))))

(defun forgejo-filter-query-notifications (host &optional filters)
  "Return notification rows for HOST matching FILTERS.
When FILTERS is nil, returns all notifications."
  (let ((rows (forgejo-db-get-notifications host)))
    (if filters
        (cl-remove-if-not
         (lambda (row) (forgejo-filter--match-notification row filters))
         rows)
      rows)))

;;; ---- Tabulated-list entries ----

(defun forgejo-filter-list-entries (items)
  "Convert ITEMS (list of API alists) to `tabulated-list-entries'.
Each entry is (NUMBER . [# STATE TITLE LABELS AUTHOR UPDATED])."
  (mapcar
   (lambda (item)
     (let-alist item
       (list .number
             (vector
              (propertize (number-to-string .number)
                          'face 'forgejo-number-face)
              (forgejo-buffer--format-state .state)
              .title
              (forgejo-buffer--format-labels .labels)
              (propertize (or (forgejo-buffer--login .user) "")
                          'face 'forgejo-comment-author-face)
              (propertize (forgejo-buffer--relative-time .updated_at)
                          'face 'shadow
                          'forgejo-timestamp (or .updated_at ""))))))
   items))

;;; ---- Refresh ----

(defvar forgejo-host)

(defun forgejo-filter-refresh (buf-name host-url owner repo
                               filters render-fn sync-fn)
  "Re-render and sync a list buffer, preserving point.
BUF-NAME is the target buffer.  HOST-URL is the full Forgejo URL.
OWNER and REPO identify the repository.  FILTERS is the current
filter plist.  RENDER-FN is called as (RENDER-FN BUF HOST OWNER
REPO FILTERS) for immediate DB render.  SYNC-FN is called as
\(SYNC-FN HOST OWNER REPO FILTERS BUF-NAME FORCE) for async sync."
  (let* ((forgejo-host (or host-url forgejo-host))
         (host (url-host (url-generic-parse-url forgejo-host)))
         (line (line-number-at-pos)))
    (funcall render-fn buf-name host owner repo filters)
    (funcall sync-fn host owner repo filters buf-name t)
    (with-current-buffer buf-name
      (goto-char (point-min))
      (forward-line (1- line)))))

;;; ---- Completion candidates ----

(defun forgejo-filter-completions (host owner repo)
  "Return completions-alist for issue/PR filter in HOST/OWNER/REPO."
  (let ((labels (mapcar (lambda (row) (nth 4 row))
                        (forgejo-db-get-labels host owner repo)))
        (milestones (mapcar (lambda (row) (nth 3 row))
                            (forgejo-db-get-milestones host owner repo)))
        (authors (forgejo-db-get-authors host owner repo)))
    `((state . ("open" "closed"))
      (label . ,labels)
      (milestone . ,milestones)
      (author . ,authors)
      (search . nil))))

(defun forgejo-filter-completions-for-notifications (host)
  "Return completions-alist for notification filter on HOST."
  (let* ((rows (forgejo-db-get-notifications host))
         (repos (delete-dups
                 (mapcar (lambda (row)
                           (format "%s/%s" (nth 5 row) (nth 6 row)))
                         rows))))
    `((status . ("unread" "read"))
      (type . ("Issue" "Pull"))
      (repo . ,repos))))

(provide 'forgejo-filter)
;;; forgejo-filter.el ends here
