;;; forgejo-db.el --- SQLite cache for Forgejo API data  -*- lexical-binding: t; -*-

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

;; Local SQLite cache for Forgejo API responses.
;;
;; Stores issues, pull requests, timeline events, labels, and
;; milestones.  Tracks sync timestamps per (host, owner, repo,
;; endpoint) for incremental updates via the API's `since' parameter.

;;; Code:

(require 'cl-lib)
(require 'json)

(defvar forgejo-db)
(defvar forgejo-db-dir)

;;; Schema

(defconst forgejo-db--schema
  '("CREATE TABLE IF NOT EXISTS issues (
       id INTEGER NOT NULL,
       host TEXT NOT NULL,
       owner TEXT NOT NULL,
       repo TEXT NOT NULL,
       number INTEGER NOT NULL,
       title TEXT NOT NULL,
       state TEXT NOT NULL,
       body TEXT,
       body_html TEXT,
       previous_body TEXT,
       user TEXT,
       labels TEXT,
       milestone TEXT,
       assignees TEXT,
       comments_count INTEGER DEFAULT 0,
       created_at TEXT,
       updated_at TEXT,
       closed_at TEXT,
       is_pull INTEGER DEFAULT 0,
       read INTEGER DEFAULT 0,
       PRIMARY KEY (host, owner, repo, number))"
    "CREATE TABLE IF NOT EXISTS timeline_events (
       id INTEGER NOT NULL,
       host TEXT NOT NULL,
       owner TEXT NOT NULL,
       repo TEXT NOT NULL,
       issue_number INTEGER NOT NULL,
       type TEXT,
       body TEXT,
       body_html TEXT,
       user TEXT,
       created_at TEXT,
       data TEXT,
       PRIMARY KEY (host, owner, repo, issue_number, id))"
    "CREATE TABLE IF NOT EXISTS labels (
       id INTEGER NOT NULL,
       host TEXT NOT NULL,
       owner TEXT NOT NULL,
       repo TEXT NOT NULL,
       name TEXT NOT NULL,
       color TEXT,
       description TEXT,
       PRIMARY KEY (host, owner, repo, id))"
    "CREATE TABLE IF NOT EXISTS milestones (
       id INTEGER NOT NULL,
       host TEXT NOT NULL,
       owner TEXT NOT NULL,
       repo TEXT NOT NULL,
       title TEXT NOT NULL,
       state TEXT,
       open_issues INTEGER DEFAULT 0,
       closed_issues INTEGER DEFAULT 0,
       PRIMARY KEY (host, owner, repo, id))"
    "CREATE TABLE IF NOT EXISTS repos (
       host TEXT NOT NULL,
       owner TEXT NOT NULL,
       name TEXT NOT NULL,
       description TEXT,
       is_user_repo INTEGER DEFAULT 0,
       PRIMARY KEY (host, owner, name))"
    "CREATE TABLE IF NOT EXISTS sync_state (
       host TEXT NOT NULL,
       owner TEXT NOT NULL,
       repo TEXT NOT NULL,
       endpoint TEXT NOT NULL,
       last_synced TEXT,
       PRIMARY KEY (host, owner, repo, endpoint))"
)
  "SQL statements to initialize the database schema.")

;;; Connection management

(defconst forgejo-db--migrations
  '("ALTER TABLE issues ADD COLUMN body_html TEXT"
    "ALTER TABLE issues ADD COLUMN previous_body TEXT"
    "ALTER TABLE timeline_events ADD COLUMN body_html TEXT"
    "ALTER TABLE issues ADD COLUMN read INTEGER DEFAULT 0"
    "DROP TABLE IF EXISTS notifications")
  "Migration statements for existing databases.
Each is run inside condition-case so duplicates are ignored.")

(defun forgejo-db--ensure ()
  "Open or create the SQLite database, run schema if needed.
Returns the database connection."
  (unless (and forgejo-db (sqlitep forgejo-db))
    (let ((dir (file-name-as-directory forgejo-db-dir)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (setq forgejo-db (sqlite-open (expand-file-name "forgejo.db" dir)))
      (dolist (stmt forgejo-db--schema)
        (sqlite-execute forgejo-db stmt))
      (dolist (stmt forgejo-db--migrations)
        (condition-case nil
            (sqlite-execute forgejo-db stmt)
          (error nil)))))
  forgejo-db)

(defun forgejo-db--close ()
  "Close the database connection."
  (when (and forgejo-db (sqlitep forgejo-db))
    (sqlite-close forgejo-db)
    (setq forgejo-db nil)))

;;; Low-level wrappers

(defun forgejo-db--execute (sql &rest args)
  "Execute SQL with ARGS on the forgejo database."
  (apply #'sqlite-execute (forgejo-db--ensure) sql args))

(defun forgejo-db--select (sql &rest args)
  "Execute SQL query with ARGS, return result rows."
  (apply #'sqlite-select (forgejo-db--ensure) sql args))

(defmacro forgejo-db--with-transaction (&rest body)
  "Execute BODY inside a single SQLite transaction."
  (declare (indent 0))
  `(let ((db (forgejo-db--ensure)))
     (sqlite-execute db "BEGIN")
     (condition-case err
         (prog1 (progn ,@body)
           (sqlite-execute db "COMMIT"))
       (error
        (sqlite-execute db "ROLLBACK")
        (signal (car err) (cdr err))))))

;;; JSON helpers

(defun forgejo-db--nullable (value)
  "Convert VALUE to nil if it is :null or :false."
  (if (memq value '(:null :false)) nil value))

(defun forgejo-db--encode-json (value)
  "Encode VALUE as a JSON string for storage."
  (if value (json-encode value) "null"))

(defun forgejo-db--decode-json (text)
  "Decode JSON TEXT from storage.  Returns nil for null/empty."
  (when (and text (not (string= text "null")) (not (string-empty-p text)))
    (json-parse-string text :object-type 'alist :array-type 'list)))

;;; Issues

(defun forgejo-db--track-edit (db host owner repo number new-body)
  "If issue body changed, append old body to previous_body history.
Returns nil if no change or no existing row."
  (when-let* ((new-body)
              (existing (car (sqlite-select
                              db
                              "SELECT body, previous_body FROM issues
                               WHERE host = ? AND owner = ? AND repo = ? AND number = ?"
                              (list host owner repo number))))
              (old-body (nth 0 existing))
              ((and old-body (not (string= old-body new-body)))))
    (let* ((history-json (nth 1 existing))
           (history (if history-json
                        (forgejo-db--decode-json history-json)
                      nil))
           (updated (append history (list old-body))))
      (sqlite-execute
       db
       "UPDATE issues SET previous_body = ? WHERE host = ? AND owner = ? AND repo = ? AND number = ?"
       (list (forgejo-db--encode-json updated) host owner repo number)))))

(defun forgejo-db-save-issues (host owner repo issues &optional is-pull)
  "Upsert ISSUES (list of API alists) for HOST/OWNER/REPO.
Tracks body edits in previous_body.  Stores body_html from the API response
when available; otherwise preserves existing body_html.
When IS-PULL is non-nil, mark all entries as pull requests regardless of
whether a `pull_request' field is present in the data."
  (forgejo-db--with-transaction
    (let ((db (forgejo-db--ensure)))
    (dolist (issue issues)
      (let-alist issue
        (let ((body (forgejo-db--nullable .body))
              (body-html (forgejo-db--nullable .body_html))
              (labels (forgejo-db--nullable .labels))
              (milestone (forgejo-db--nullable .milestone))
              (assignees (forgejo-db--nullable .assignees))
              (closed (forgejo-db--nullable .closed_at))
              (pr (or is-pull (forgejo-db--nullable .pull_request))))
          ;; Track edits before overwriting
          (when body
            (forgejo-db--track-edit db host owner repo .number body))
          ;; Upsert: update body_html only when API provides it
          (sqlite-execute
           db
           "INSERT INTO issues
              (id, host, owner, repo, number, title, state, body, body_html,
               user, labels, milestone, assignees, comments_count,
               created_at, updated_at, closed_at, is_pull, read)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0)
            ON CONFLICT(host, owner, repo, number) DO UPDATE SET
              title=excluded.title, state=excluded.state, body=excluded.body,
              body_html=COALESCE(excluded.body_html, issues.body_html),
              user=excluded.user, labels=excluded.labels, milestone=excluded.milestone,
              assignees=excluded.assignees, comments_count=excluded.comments_count,
              updated_at=excluded.updated_at, closed_at=excluded.closed_at,
              is_pull=excluded.is_pull, read=0"
           (list .id host owner repo .number .title .state body body-html
                 (alist-get 'login .user)
                 (forgejo-db--encode-json (when (listp labels) labels))
                 (when (listp milestone) (alist-get 'title milestone))
                 (forgejo-db--encode-json
                  (when (listp assignees)
                    (mapcar (lambda (a) (alist-get 'login a)) assignees)))
                 .comments
                 .created_at .updated_at closed
                 (if pr 1 0)))))))))

(defun forgejo-db-get-issues (host owner repo &optional filters)
  "Query cached issues for HOST/OWNER/REPO with optional FILTERS.
FILTERS is a plist with keys:
  :state      - \"open\", \"closed\", or nil for all
  :label      - label name substring to match
  :milestone  - milestone title to match
  :author     - author/poster login to match
  :query      - search string for title
  :read       - \"yes\" or \"no\" for read/unread status
  :is-pull    - when non-nil, filter to pull requests only
  :no-pulls   - when non-nil, exclude pull requests"
  (let ((where (list "host = ?" "owner = ?" "repo = ?"))
        (args (list host owner repo)))
    (when-let* ((state (plist-get filters :state)))
      (setq where (append where (list "state = ?"))
            args (append args (list state))))
    (when-let* ((label (or (plist-get filters :labels)
                          (plist-get filters :label))))
      (setq where (append where (list "labels LIKE ?"))
            args (append args (list (format "%%%s%%" label)))))
    (when-let* ((milestone (plist-get filters :milestone)))
      (setq where (append where (list "milestone = ?"))
            args (append args (list milestone))))
    (when-let* ((author (plist-get filters :author)))
      (setq where (append where (list "user LIKE ?"))
            args (append args (list (format "%%%s%%" author)))))
    (when-let* ((query (plist-get filters :query)))
      (setq where (append where (list "title LIKE ?"))
            args (append args (list (format "%%%s%%" query)))))
    (when-let* ((read-val (plist-get filters :read)))
      (setq where (append where (list "read = ?"))
            args (append args (list (if (equal read-val "yes") 1 0)))))
    (when-let* ((type (plist-get filters :type)))
      (setq where (append where (list (if (equal type "pr")
                                          "is_pull = 1"
                                        "is_pull = 0")))))
    (when (plist-get filters :is-pull)
      (setq where (append where (list "is_pull = 1"))))
    (when (plist-get filters :no-pulls)
      (setq where (append where (list "is_pull = 0"))))
    (forgejo-db--select
     (format "SELECT %s FROM issues WHERE %s ORDER BY updated_at DESC"
             forgejo-db--issue-columns
             (mapconcat #'identity where " AND "))
     args)))

;;; Timeline events

(defun forgejo-db-save-timeline (host owner repo number events)
  "Upsert timeline EVENTS for issue NUMBER in HOST/OWNER/REPO.
Stores body_html from API response when available."
  (when (and events (listp events))
    (forgejo-db--with-transaction
      (let ((db (forgejo-db--ensure)))
        (dolist (event events)
        (let-alist event
          (sqlite-execute
           db
           "INSERT INTO timeline_events
              (id, host, owner, repo, issue_number, type, body, body_html,
               user, created_at, data)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(host, owner, repo, issue_number, id) DO UPDATE SET
              type=excluded.type, body=excluded.body,
              body_html=COALESCE(excluded.body_html, timeline_events.body_html),
              user=excluded.user, created_at=excluded.created_at,
              data=excluded.data"
           (list .id host owner repo number
                 (forgejo-db--nullable .type)
                 (forgejo-db--nullable .body)
                 (forgejo-db--nullable .body_html)
                 (alist-get 'login (forgejo-db--nullable .user))
                 (forgejo-db--nullable .created_at)
                 (forgejo-db--encode-json event)))))))))

(defun forgejo-db-get-timeline (host owner repo number)
  "Get cached timeline events for issue NUMBER in HOST/OWNER/REPO."
  (forgejo-db--select
   (format "SELECT %s FROM timeline_events
            WHERE host = ? AND owner = ? AND repo = ? AND issue_number = ?
            ORDER BY created_at ASC"
           forgejo-db--timeline-columns)
   (list host owner repo number)))

;;; Labels

(defun forgejo-db-save-labels (host owner repo labels)
  "Upsert LABELS for HOST/OWNER/REPO."
  (let ((db (forgejo-db--ensure)))
    (dolist (label labels)
      (let-alist label
        (sqlite-execute
         db
         "INSERT OR REPLACE INTO labels
            (id, host, owner, repo, name, color, description)
          VALUES (?, ?, ?, ?, ?, ?, ?)"
         (list .id host owner repo .name .color .description))))))

(defun forgejo-db-get-labels (host owner repo)
  "Get cached labels for HOST/OWNER/REPO."
  (forgejo-db--select
   "SELECT * FROM labels WHERE host = ? AND owner = ? AND repo = ?
    ORDER BY name"
   (list host owner repo)))

(defun forgejo-db-get-label-id (host owner repo name)
  "Return the label ID for NAME in HOST/OWNER/REPO, or nil."
  (caar (forgejo-db--select
         "SELECT id FROM labels WHERE host = ? AND owner = ? AND repo = ? AND name = ?"
         (list host owner repo name))))

;;; Milestones

(defun forgejo-db-save-milestones (host owner repo milestones)
  "Upsert MILESTONES for HOST/OWNER/REPO."
  (let ((db (forgejo-db--ensure)))
    (dolist (ms milestones)
      (let-alist ms
        (sqlite-execute
         db
         "INSERT OR REPLACE INTO milestones
            (id, host, owner, repo, title, state, open_issues, closed_issues)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
         (list .id host owner repo .title .state
               .open_issues .closed_issues))))))

(defun forgejo-db-get-milestones (host owner repo)
  "Get cached milestones for HOST/OWNER/REPO."
  (forgejo-db--select
   "SELECT * FROM milestones WHERE host = ? AND owner = ? AND repo = ?
    ORDER BY title"
   (list host owner repo)))

(defun forgejo-db-get-milestone-id (host owner repo title)
  "Return the milestone ID for TITLE in HOST/OWNER/REPO, or nil."
  (caar (forgejo-db--select
         "SELECT id FROM milestones WHERE host = ? AND owner = ? AND repo = ? AND title = ?"
         (list host owner repo title))))

;;; Repos

(defun forgejo-db-save-user-repos (host repos)
  "Save REPOS (list of API alists) as user repos for HOST."
  (let ((db (forgejo-db--ensure)))
    (dolist (repo repos)
      (let-alist repo
        (sqlite-execute
         db
         "INSERT OR REPLACE INTO repos (host, owner, name, description, is_user_repo)
          VALUES (?, ?, ?, ?, 1)"
         (list host
               (alist-get 'login .owner)
               .name
               (forgejo-db--nullable .description)))))))

(defun forgejo-db-get-user-repos (host)
  "Get cached user repo names for HOST as list of \"owner/name\" strings."
  (mapcar (lambda (row) (format "%s/%s" (nth 0 row) (nth 1 row)))
          (forgejo-db--select
           "SELECT owner, name FROM repos WHERE host = ? AND is_user_repo = 1
            ORDER BY name"
           (list host))))

;;; Sync state tracking

(defun forgejo-db-get-sync-time (host owner repo endpoint)
  "Get the last sync timestamp for ENDPOINT in HOST/OWNER/REPO."
  (caar (forgejo-db--select
         "SELECT last_synced FROM sync_state
          WHERE host = ? AND owner = ? AND repo = ? AND endpoint = ?"
         (list host owner repo endpoint))))

(defun forgejo-db-set-sync-time (host owner repo endpoint time)
  "Set the last sync TIME for ENDPOINT in HOST/OWNER/REPO."
  (forgejo-db--execute
   "INSERT OR REPLACE INTO sync_state
      (host, owner, repo, endpoint, last_synced)
    VALUES (?, ?, ?, ?, ?)"
   (list host owner repo endpoint time)))

;;; Row-to-alist conversion (explicit column queries)

(defconst forgejo-db--issue-columns
  "number, title, state, body, body_html, user, labels, milestone,
   assignees, comments_count, created_at, updated_at, closed_at, is_pull,
   previous_body, read"
  "Column list for issue queries (deterministic order).")

(defun forgejo-db--row-to-issue-alist (row)
  "Convert an issue ROW to an API-shaped alist.
ROW must come from a query using `forgejo-db--issue-columns':
  0=number 1=title 2=state 3=body 4=body_html 5=user 6=labels
  7=milestone 8=assignees 9=comments_count 10=created_at
  11=updated_at 12=closed_at 13=is_pull 14=previous_body 15=read"
  (let ((labels (forgejo-db--decode-json (nth 6 row)))
        (assignees-raw (forgejo-db--decode-json (nth 8 row))))
    `((number . ,(nth 0 row))
      (title . ,(nth 1 row))
      (state . ,(nth 2 row))
      (body . ,(nth 3 row))
      (body_html . ,(nth 4 row))
      (user . ((login . ,(nth 5 row))))
      (labels . ,labels)
      (milestone . ,(when (nth 7 row) `((title . ,(nth 7 row)))))
      (assignees . ,(mapcar (lambda (login) `((login . ,login)))
                            (if (listp assignees-raw) assignees-raw nil)))
      (comments . ,(nth 9 row))
      (created_at . ,(nth 10 row))
      (updated_at . ,(nth 11 row))
      (closed_at . ,(nth 12 row))
      (pull_request . ,(when (= (or (nth 13 row) 0) 1) t))
      (previous_body . ,(nth 14 row))
      (read . ,(or (nth 15 row) 0)))))

(defconst forgejo-db--timeline-columns
  "id, type, body, body_html, user, created_at, data"
  "Column list for timeline queries (deterministic order).")

(defun forgejo-db--row-to-timeline-alist (row)
  "Convert a timeline ROW to an alist.
ROW must come from a query using `forgejo-db--timeline-columns':
  0=id 1=type 2=body 3=body_html 4=user 5=created_at 6=data
The `data' JSON blob is merged into the result to provide event-specific
fields like label, assignee, old_title, new_title."
  (let* ((base `((id . ,(nth 0 row))
                 (type . ,(nth 1 row))
                 (body . ,(nth 2 row))
                 (body_html . ,(nth 3 row))
                 (user . ((login . ,(nth 4 row))))
                 (created_at . ,(nth 5 row))))
         (data-json (nth 6 row))
         (data (when data-json (forgejo-db--decode-json data-json))))
    (if data
        (append base data)
      base)))

(defun forgejo-db-get-issue (host owner repo number)
  "Get a single issue alist from the DB, or nil."
  (when-let* ((rows (forgejo-db--select
                     (format "SELECT %s FROM issues
                              WHERE host = ? AND owner = ? AND repo = ? AND number = ?"
                             forgejo-db--issue-columns)
                     (list host owner repo number)))
              (row (car rows)))
    (forgejo-db--row-to-issue-alist row)))

(defun forgejo-db-mark-read (host owner repo number)
  "Mark issue/PR NUMBER as read for HOST/OWNER/REPO."
  (forgejo-db--execute
   "UPDATE issues SET read = 1
    WHERE host = ? AND owner = ? AND repo = ? AND number = ?"
   (list host owner repo number)))

(defun forgejo-db-close-missing (host owner repo numbers &optional is-pull)
  "Mark issues NOT in NUMBERS as closed for HOST/OWNER/REPO.
When IS-PULL is non-nil, only affect pull requests."
  (when numbers
    (let ((placeholders (mapconcat (lambda (_) "?") numbers ","))
          (pull-filter (if is-pull "AND is_pull = 1" "AND is_pull = 0")))
      (forgejo-db--execute
       (format "UPDATE issues SET state = 'closed'
                WHERE host = ? AND owner = ? AND repo = ?
                AND state = 'open' %s
                AND number NOT IN (%s)"
               pull-filter placeholders)
       (append (list host owner repo) numbers)))))

(defun forgejo-db-get-authors (host owner repo)
  "Get distinct author logins for HOST/OWNER/REPO."
  (mapcar #'car
          (forgejo-db--select
           "SELECT DISTINCT user FROM issues
            WHERE host = ? AND owner = ? AND repo = ? AND user IS NOT NULL
            ORDER BY user"
           (list host owner repo))))

;;; Issue/PR titles for completion

(defun forgejo-db-get-issue-titles (host owner repo)
  "Return alist of (NUMBER . TITLE) for all issues/PRs in HOST/OWNER/REPO."
  (mapcar (lambda (row) (cons (nth 0 row) (nth 1 row)))
          (forgejo-db--select
           "SELECT number, title FROM issues
            WHERE host = ? AND owner = ? AND repo = ?
            ORDER BY number DESC"
           (list host owner repo))))

;;; Update body_html

(defun forgejo-db-update-issue-html (host owner repo number html)
  "Set body_html for issue NUMBER."
  (forgejo-db--execute
   "UPDATE issues SET body_html = ?
    WHERE host = ? AND owner = ? AND repo = ? AND number = ?"
   (list html host owner repo number)))

(defun forgejo-db-update-timeline-html (host owner repo issue-number event-id html)
  "Set body_html for timeline event EVENT-ID."
  (forgejo-db--execute
   "UPDATE timeline_events SET body_html = ?
    WHERE host = ? AND owner = ? AND repo = ? AND issue_number = ? AND id = ?"
   (list html host owner repo issue-number event-id)))

(provide 'forgejo-db)
;;; forgejo-db.el ends here
