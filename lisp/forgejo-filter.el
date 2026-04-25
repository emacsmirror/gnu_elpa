;;; forgejo-filter.el --- Filter parsing, query, and formatting for Forgejo  -*- lexical-binding: t; -*-

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

;; Pure data layer for filter operations.  Provides parsing, serialization,
;; DB query pipelines, and tabulated-list entry formatting.  No buffer
;; mutation, no interactive prompts, no global variable reads.

;;; Code:

(require 'cl-lib)
(require 'forgejo-db)

;; Buffer formatting helpers (loaded at runtime by consumer modules).
(declare-function forgejo-buffer--format-state "forgejo-buffer.el" (state))
(declare-function forgejo-buffer--format-labels "forgejo-buffer.el" (labels))
(declare-function forgejo-buffer--relative-time "forgejo-buffer.el" (time-string))
(declare-function forgejo-buffer--login "forgejo-buffer.el" (user-alist))

;;; Prefix / key maps

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

(defconst forgejo-filter--watch-prefix-map
  (append forgejo-filter--prefix-map
          '(("read" . :read)
            ("type" . :type)))
  "Prefix map for parsing watch filter queries.")

(defconst forgejo-filter--watch-key-map
  (append forgejo-filter--key-map
          '((:read . "read")
            (:type . "type")))
  "Key map for serializing watch filter plists.")

;;; Parse / serialize

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

;;; API param building

(defconst forgejo-filter--api-param-map
  '((:state     . "state")
    (:labels    . "labels")
    (:milestone . "milestones")
    (:author    . "created_by")
    (:query     . "q")
    (:since     . "since"))
  "Map from filter plist keys to Forgejo API query parameter names.
The :page key is handled separately (needs number-to-string).")

(defun forgejo-filter-build-params (type filters sort limit)
  "Build API query params from FILTERS for the issues endpoint.
TYPE is \"issues\", \"pulls\", or nil (both).
SORT is the sort order string.  LIMIT is the page size integer.
Returns an alist of (PARAM . VALUE) pairs."
  (let ((params (list (cons "sort" sort)
                      (cons "limit" (number-to-string limit)))))
    (when type (push (cons "type" type) params))
    (cl-loop for (key . param) in forgejo-filter--api-param-map
             for val = (plist-get filters key)
             when val do (push (cons param val) params))
    (when-let* ((page (plist-get filters :page)))
      (push (cons "page" (number-to-string page)) params))
    params))

;;; DB query pipelines

(defun forgejo-filter-query-watch (host rules filters)
  "Return issue/PR alists matching watch RULES with FILTERS.
RULES is `forgejo-watch-rules'.  FILTERS is an
additional filter plist (e.g. (:read \"no\")).  Returns alists
enriched with `watch-owner' and `watch-repo' keys."
  (let (result)
    (dolist (rule rules)
      (let* ((repo-key (if (stringp rule) rule (car rule)))
             (query (if (stringp rule) nil (cdr rule)))
             (parts (split-string repo-key "/"))
             (owner (nth 0 parts))
             (repo (nth 1 parts))
             (rule-filters (forgejo-filter-parse query))
             (merged (append rule-filters filters))
             (rows (forgejo-db-get-issues host owner repo merged))
             (alists (mapcar #'forgejo-db--row-to-issue-alist rows)))
        (dolist (alist alists)
          (push (append `((watch-owner . ,owner)
                          (watch-repo . ,repo))
                        alist)
                result))))
    (nreverse result)))

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

;;; Column specs

(declare-function forgejo--sort-by-number "forgejo.el" (a b))
(declare-function forgejo--sort-by-updated "forgejo.el" (a b))

(defconst forgejo-filter-list-columns
  `(("#"       5    forgejo--sort-by-number :right-align t)
    ("State"   6    nil)
    ("Title"   ,(/ 1.0 3) t)
    ("Labels"  ,(/ 1.0 6) nil)
    ("Author"  ,(/ 1.0 8) t)
    ("Updated" ,(/ 1.0 8) forgejo--sort-by-updated))
  "Default column spec for issue and PR list views.
Each element is (NAME WIDTH-OR-FLOAT SORT . PROPS).")

(defconst forgejo-filter-notification-columns
  `(("Type"    5    nil)
    ("Ref"     ,(/ 1.0 10) t)
    ("State"   6    nil)
    ("Title"   ,(/ 1.0 3) t)
    ("Labels"  ,(/ 1.0 6) nil)
    ("Author"  ,(/ 1.0 8) t)
    ("Updated" ,(/ 1.0 8) forgejo--sort-by-updated))
  "Column spec for notification list views.")

;;; Tabulated-list entries

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

(defun forgejo-filter-notification-entries (items)
  "Convert ITEMS (watch query alists) to `tabulated-list-entries'.
Each entry is (NUMBER . [TYPE REF STATE TITLE LABELS AUTHOR UPDATED]).
Items must have `watch-owner' and `watch-repo' keys."
  (mapcar
   (lambda (item)
     (let-alist item
       (let ((type (if .pull_request "PR" "Issue"))
             (short-ref (format "%s#%d" (or .watch-repo "") (or .number 0)))
             (full-ref (format "%s/%s#%d"
                               (or .watch-owner "") (or .watch-repo "")
                               (or .number 0))))
         (list full-ref
               (vector
                (propertize type 'face (if .pull_request 'success 'warning))
                (propertize short-ref 'face 'forgejo-number-face
                            'forgejo-full-ref full-ref)
                (forgejo-buffer--format-state .state)
                .title
                (forgejo-buffer--format-labels .labels)
                (propertize (or (forgejo-buffer--login .user) "")
                            'face 'forgejo-comment-author-face)
                (propertize (forgejo-buffer--relative-time .updated_at)
                            'face 'shadow
                            'forgejo-timestamp (or .updated_at "")))))))
   items))

;;; Completion candidates

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

(provide 'forgejo-filter)
;;; forgejo-filter.el ends here
