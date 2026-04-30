;;; gnosis-db.el --- Database layer for gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

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

;; Database connection, query wrappers, schema definitions,
;; migrations, and ID generation.  This is the foundation module
;; that all other gnosis modules build on.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vc-git)
(require 'gnosis-sqlite)
(require 'gnosis-algorithm)

(defcustom gnosis-dir (locate-user-emacs-file "gnosis")
  "Gnosis directory."
  :type 'directory
  :group 'gnosis)

;; Directory creation deferred to gnosis--ensure-db

(defvar gnosis-db nil
  "Gnosis database connection.
Initialized lazily by `gnosis--ensure-db' on first use.")

(defvar gnosis-testing nil
  "Change this to non-nil when running manual tests.")

(defconst gnosis-db-version 8
  "Gnosis database version.")

(defvar gnosis--id-cache nil
  "Hash table of existing thema IDs, bound during batch import.
When non-nil, `gnosis-generate-id' and `gnosis-update-thema' use this
for O(1) lookups instead of querying the database per thema.")

;;; Connection

(defun gnosis--ensure-db ()
  "Return the gnosis database connection, opening it if necessary.
Creates `gnosis-dir' and runs schema initialization on first use."
  (unless gnosis-db
    (unless (file-directory-p gnosis-dir)
      (make-directory gnosis-dir))
    (setq gnosis-db (gnosis-sqlite-open (expand-file-name "gnosis.db" gnosis-dir)))
    (gnosis-db-init))
  gnosis-db)

;;; Query wrappers

(defun gnosis-select (value table &optional restrictions flatten)
  "Select VALUE from TABLE, optionally with RESTRICTIONS.

Optional argument FLATTEN, when non-nil, flattens the result."
  (let* ((db (gnosis--ensure-db))
	 (cols (gnosis-sqlite--compile-columns value))
	 (where (gnosis-sqlite--compile-expr (or restrictions '(= 1 1))))
	 (sql (format "SELECT %s FROM %s WHERE %s"
		      cols (gnosis-sqlite--ident table) (car where)))
	 (output (gnosis-sqlite--select-compiled db sql (cdr where))))
    (if flatten (apply #'append output) output)))

(defun gnosis-table-exists-p (table)
  "Check if TABLE exists."
  (let* ((db (gnosis--ensure-db))
	 (tables (mapcar #'car
			 (sqlite-select db
					"SELECT name FROM sqlite_master WHERE type = 'table'"))))
    (member (gnosis-sqlite--ident table) tables)))

(defun gnosis--create-table (table &optional values)
  "Create TABLE for VALUES."
  (unless (gnosis-table-exists-p table)
    (let ((sql (format "CREATE TABLE %s (%s)"
		       (gnosis-sqlite--ident table)
		       (gnosis-sqlite--compile-schema values))))
      (gnosis-sqlite-execute (gnosis--ensure-db) sql))))

(defun gnosis--drop-table (table)
  "Drop TABLE from `gnosis-db'."
  (gnosis-sqlite-execute (gnosis--ensure-db)
			 (format "DROP TABLE %s" (gnosis-sqlite--ident table))))

(defun gnosis-drop-table (table)
  "Drop TABLE from `gnosis-db'."
  (when (gnosis-table-exists-p table)
    (gnosis--drop-table table)))

(defun gnosis--insert-into (table values &optional or-ignore)
  "Insert VALUES to TABLE.
When OR-IGNORE is non-nil, use INSERT OR IGNORE to silently skip
rows that violate a UNIQUE constraint."
  (let* ((compiled (gnosis-sqlite--compile-values values))
	 (sql (format "INSERT%s INTO %s VALUES %s"
		      (if or-ignore " OR IGNORE" "")
		      (gnosis-sqlite--ident table) (car compiled))))
    (gnosis-sqlite--execute-compiled (gnosis--ensure-db) sql (cdr compiled))))

(defun gnosis-update (table value where)
  "Update records in TABLE with to new VALUE based on the given WHERE condition.

Example:
 (gnosis-update ='themata ='(= keimenon \"NEW VALUE\") ='(= id 12))"
  (let* ((set-clause (gnosis-sqlite--compile-expr value))
	 (where-clause (gnosis-sqlite--compile-expr where))
	 (sql (format "UPDATE %s SET %s WHERE %s"
		      (gnosis-sqlite--ident table)
		      (car set-clause)
		      (car where-clause))))
    (gnosis-sqlite--execute-compiled (gnosis--ensure-db) sql
				     (append (cdr set-clause) (cdr where-clause)))))

(defun gnosis-get (value table &optional restrictions)
  "Return caar of VALUE from TABLE, optionally with where RESTRICTIONS."
  (caar (gnosis-select value table restrictions)))

(defun gnosis--delete (table &optional where)
  "Delete from TABLE, optionally restricted by WHERE clause."
  (if where
      (let ((compiled (gnosis-sqlite--compile-expr where)))
	(gnosis-sqlite--execute-compiled (gnosis--ensure-db)
					 (format "DELETE FROM %s WHERE %s"
						 (gnosis-sqlite--ident table)
						 (car compiled))
					 (cdr compiled)))
    (gnosis-sqlite--execute-compiled (gnosis--ensure-db)
				     (format "DELETE FROM %s" (gnosis-sqlite--ident table)))))

;;; Date utilities

(defun gnosis--date-to-int (date)
  "Convert DATE list (year month day) to YYYYMMDD integer for fast comparison."
  (+ (* (nth 0 date) 10000) (* (nth 1 date) 100) (nth 2 date)))

(defun gnosis--int-to-date (int)
  "Convert YYYYMMDD integer INT to (year month day) list."
  (list (/ int 10000) (% (/ int 100) 100) (% int 100)))

(defun gnosis--today-int ()
  "Return today as a YYYYMMDD integer.
Respects `gnosis-algorithm-day-start-hour'."
  (gnosis--date-to-int (gnosis-algorithm-date)))

;;; ID generation

(defun gnosis-generate-id (&optional length)
  "Generate a unique gnosis ID.

When `gnosis--id-cache' is bound, uses hash table lookup instead of DB query.

LENGTH: length of id, default to 18."
  ;; NOTE: length must not exceed 18; 19-digit+ values can overflow sqlite.
  (let* ((length (or length 18))
         (max-val (expt 10 length))
         (min-val (expt 10 (1- length)))
         (id (+ (random (- max-val min-val)) min-val))
	 (exists (if gnosis--id-cache
		     (gethash id gnosis--id-cache)
		   (gnosis-select 'id 'themata `(= id ,id) t))))
    (if exists
        (gnosis-generate-id length)
      (when gnosis--id-cache
        (puthash id t gnosis--id-cache))
      id)))

(defun gnosis-generate-ids (n &optional length)
  "Generate N unique gnosis IDs as a list.
Uses `gnosis--id-cache' for O(1) collision checking when bound."
  (let ((ids nil) (count 0))
    (while (< count n)
      (let* ((len (or length 18))
             (max-val (expt 10 len))
             (min-val (expt 10 (1- len)))
             (id (+ (random (- max-val min-val)) min-val))
             (exists (if gnosis--id-cache
                         (gethash id gnosis--id-cache)
                       (gnosis-select 'id 'themata `(= id ,id) t))))
        (unless exists
          (when gnosis--id-cache (puthash id t gnosis--id-cache))
          (push id ids)
          (cl-incf count))))
    (nreverse ids)))

;;; Schema

(defconst gnosis-db--schemata
  '((themata
     ([(id integer :primary-key)
       (type text :not-null)
       (keimenon text :not-null)
       (hypothesis text :not-null)
       (answer text :not-null)
       (source-guid text)]))
    (review
     ([(id integer :primary-key :not-null) ;; thema-id
       (gnosis integer :not-null)
       (amnesia integer :not-null)]
      (:foreign-key [id] :references themata [id]
		    :on-delete :cascade)))
    (review-log
     ([(id integer :primary-key :not-null) ;; thema-id
       (last-rev integer :not-null)  ;; Last review date
       (next-rev integer :not-null)  ;; Next review date
       (c-success integer :not-null) ;; Consecutive successful reviews
       (t-success integer :not-null) ;; Total successful reviews
       (c-fails integer :not-null)   ;; Consecutive failed reviewss
       (t-fails integer :not-null)   ;; Total failed reviews
       (suspend integer :not-null)   ;; Binary value, 1=suspended
       (n integer :not-null)]        ;; Number of reviews
      (:foreign-key [id] :references themata [id]
		    :on-delete :cascade)))
    (activity-log
     ([(date integer :not-null)
       (reviewed-total integer :not-null)
       (reviewed-new integer :not-null)]))
    (extras
     ([(id integer :primary-key :not-null)
       (parathema string)
       (review-image string)]
      (:foreign-key [id] :references themata [id]
		    :on-delete :cascade)))
    (thema-tag
     ([(thema-id integer :not-null)
       (tag text :not-null)]
      (:foreign-key [thema-id] :references themata [id]
		    :on-delete :cascade)
      (:unique [thema-id tag])))
    (thema-links
     ([(source integer)
       (dest text)]
      (:foreign-key [source] :references themata [id]
		    :on-delete :cascade)
      (:unique [source dest])))
    ;; Node tables (merged from org-gnosis)
    (nodes
     ([(id text :not-null :primary-key)
       (file text :not-null)
       (title text :not-null)
       (level text :not-null)
       (tags text)
       (mtime text)
       (hash text)]))
    (journal
     ([(id text :not-null :primary-key)
       (file text :not-null)
       (title text :not-null)
       (level text :not-null)
       (tags text)
       (mtime text)
       (hash text)]))
    (node-tag
     ([(node-id text :not-null)
       (tag text :not-null)]
      (:foreign-key [node-id] :references nodes [id]
		    :on-delete :cascade)
      (:unique [node-id tag])))
    (node-links
     ([(source text)
       (dest text)]
      (:foreign-key [source] :references nodes [id]
		    :on-delete :cascade)
      (:unique [source dest])))))

;;; Table creation

(defun gnosis--db-version ()
  "Return the current user_version pragma from the database."
  (caar (gnosis-sqlite-select (gnosis--ensure-db) "PRAGMA user_version")))

(defun gnosis--db-set-version (version)
  "Set the database user_version pragma to VERSION."
  (gnosis-sqlite-execute (gnosis--ensure-db)
			 (format "PRAGMA user_version = %d" version)))

(defun gnosis--db-create-tables ()
  "Create all tables and set version to current.
Used for fresh databases only."
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-with-transaction db
      (pcase-dolist (`(,table ,schema) gnosis-db--schemata)
	(gnosis-sqlite-execute db
			       (format "CREATE TABLE %s (%s)"
				       (gnosis-sqlite--ident table)
				       (gnosis-sqlite--compile-schema schema))))
      (gnosis--db-create-indexes db)
      (gnosis--db-set-version gnosis-db-version))))

(defun gnosis--db-create-indexes (db)
  "Create all performance indexes on DB."
  (dolist (stmt '("CREATE INDEX IF NOT EXISTS idx_review_log_due
                   ON review_log(n, suspend, next_rev)"
		  "CREATE INDEX IF NOT EXISTS idx_thema_tag_thema_id
                   ON thema_tag(thema_id)"
		  "CREATE INDEX IF NOT EXISTS idx_thema_tag_tag
                   ON thema_tag(tag)"
		  "CREATE INDEX IF NOT EXISTS idx_thema_links_source
                   ON thema_links(source)"
		  "CREATE INDEX IF NOT EXISTS idx_thema_links_dest
                   ON thema_links(dest)"
		  "CREATE INDEX IF NOT EXISTS idx_node_links_source
                   ON node_links(source)"
		  "CREATE INDEX IF NOT EXISTS idx_node_links_dest
                   ON node_links(dest)"
		  "CREATE INDEX IF NOT EXISTS idx_activity_log_date
                   ON activity_log(date)"
		  "CREATE INDEX IF NOT EXISTS idx_nodes_file
                   ON nodes(file)"
		  "CREATE INDEX IF NOT EXISTS idx_journal_file
                   ON journal(file)"))
    (gnosis-sqlite-execute db stmt))
  ;; source_guid index: created by v8 migration for existing DBs,
  ;; or here for fresh DBs where the column already exists
  (ignore-errors
    (gnosis-sqlite-execute db
			   "CREATE INDEX IF NOT EXISTS idx_themata_source_guid ON themata(source_guid)")))

(defun gnosis--db-has-tables-p ()
  "Return non-nil if the database has user tables."
  (let ((tables (gnosis-sqlite-select (gnosis--ensure-db)
				      "SELECT name FROM sqlite_master WHERE type = 'table'")))
    (length> tables 0)))

;;; Migrations

(defun gnosis--migrate-make-list (column)
  "Make COLUMN values into a list."
  (let ((col-name (gnosis-sqlite--ident column))
	(results (gnosis-select `[id ,column] 'themata)))
    (dolist (row results)
      (let ((id (car row))
            (old-value (cadr row)))
	(unless (listp old-value)
	  (gnosis-sqlite-execute (gnosis--ensure-db)
				 (format "UPDATE themata SET %s = ? WHERE id = ?" col-name)
				 (list (list old-value) id)))))))

(defun gnosis-db--migrate-v1 ()
  "Migration v1: rename notes table to themata."
  (gnosis-sqlite-execute (gnosis--ensure-db) "ALTER TABLE notes RENAME TO themata")
  (gnosis--db-set-version 1))

(defun gnosis--migrate-get-tags-from-column ()
  "Read unique tags from the serialized themata.tags column.
Used by migrations that run before the thema-tag junction table exists."
  (cl-loop for tags in (apply 'append
			      (gnosis-sqlite-select (gnosis--ensure-db)
						    "SELECT DISTINCT tags FROM themata"))
	   nconc tags into all-tags
	   finally return (delete-dups all-tags)))

(defun gnosis-db--migrate-v2 ()
  "Migration v2: add deck algorithm columns and activity log."
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-execute db "ALTER TABLE decks ADD COLUMN failure_factor FLOAT")
    (gnosis-sqlite-execute db "ALTER TABLE decks ADD COLUMN ef_increase FLOAT")
    (gnosis-sqlite-execute db "ALTER TABLE decks ADD COLUMN ef_decrease FLOAT")
    (gnosis-sqlite-execute db "ALTER TABLE decks ADD COLUMN ef_threshold INTEGER")
    (gnosis-sqlite-execute db "ALTER TABLE decks ADD COLUMN initial_interval TEXT")
    (gnosis-sqlite-execute db
			   "CREATE TABLE IF NOT EXISTS activity_log (
         date TEXT NOT NULL, reviewed_total INTEGER NOT NULL, reviewed_new INTEGER NOT NULL)"))
  (gnosis--db-set-version 2))

(defun gnosis-db--migrate-v3 ()
  "Migration v3: drop deck columns, rename review columns."
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-execute db "ALTER TABLE decks DROP COLUMN failure_factor")
    (gnosis-sqlite-execute db "ALTER TABLE decks DROP COLUMN ef_increase")
    (gnosis-sqlite-execute db "ALTER TABLE decks DROP COLUMN ef_decrease")
    (gnosis-sqlite-execute db "ALTER TABLE decks DROP COLUMN ef_threshold")
    (gnosis-sqlite-execute db "ALTER TABLE decks DROP COLUMN initial_interval")
    (gnosis-sqlite-execute db "ALTER TABLE review RENAME COLUMN ef TO gnosis")
    (gnosis-sqlite-execute db "ALTER TABLE review RENAME COLUMN ff TO amnesia")
    (gnosis-sqlite-execute db "ALTER TABLE review DROP COLUMN interval")
    (gnosis-sqlite-execute db
			   "CREATE TABLE IF NOT EXISTS activity_log (
         date TEXT NOT NULL, reviewed_total INTEGER NOT NULL, reviewed_new INTEGER NOT NULL)"))
  (gnosis--db-set-version 3))

(defun gnosis-db--migrate-v4 ()
  "Migration v4: column renames, tags/links tables, data conversions."
  (let ((db (gnosis--ensure-db)))
    ;; 1. Rename notes -> themata (no-op if v1 already did it)
    (ignore-errors
      (gnosis-sqlite-execute db "ALTER TABLE notes RENAME TO themata"))
    ;; 2. Create tags and links tables (v5 will rename them)
    (gnosis-sqlite-execute db
			   "CREATE TABLE IF NOT EXISTS tags (tag text PRIMARY KEY, UNIQUE (tag))")
    (gnosis-sqlite-execute db
			   "CREATE TABLE IF NOT EXISTS links (source integer, dest text,
         FOREIGN KEY (source) REFERENCES themata (id) ON DELETE CASCADE,
         UNIQUE (source, dest))")
    ;; 3. Populate tags from serialized themata.tags column
    (let ((tags (gnosis--migrate-get-tags-from-column)))
      (cl-loop for tag in tags
               do (gnosis-sqlite-execute db
					 "INSERT OR IGNORE INTO tags VALUES (?)" (list tag))))
    ;; 4. Column renames
    (gnosis-sqlite-execute db "ALTER TABLE themata RENAME COLUMN main TO keimenon")
    (gnosis-sqlite-execute db "ALTER TABLE themata RENAME COLUMN options TO hypothesis")
    (gnosis-sqlite-execute db "ALTER TABLE extras RENAME COLUMN extra_notes TO parathema")
    (gnosis-sqlite-execute db "ALTER TABLE extras RENAME COLUMN images TO review_image")
    (gnosis-sqlite-execute db "ALTER TABLE extras DROP COLUMN extra_image")
    ;; 5. Make sure all hypothesis & answer values are lists
    (gnosis--migrate-make-list 'hypothesis)
    (gnosis--migrate-make-list 'answer)
    ;; 6. Fix MCQ integer answers
    (cl-loop for thema in (gnosis-select 'id 'themata '(= type "mcq") t)
             do (let* ((data (gnosis-select '[hypothesis answer] 'themata
					    `(= id ,thema) t))
                       (hypothesis (nth 0 data))
                       (old-answer (car (nth 1 data)))
                       (new-answer (when (integerp old-answer)
                                     (list (nth (1- old-answer) hypothesis)))))
                  (when (integerp old-answer)
                    (gnosis-update 'themata `(= answer ',new-answer)
                                   `(= id ,thema)))))
    ;; 7. Replace y-or-n with MCQ
    (cl-loop for thema in (gnosis-select 'id 'themata '(= type "y-or-n") t)
             do (let ((data (gnosis-select '[type hypothesis answer]
					   'themata `(= id ,thema) t)))
                  (when (string= (nth 0 data) "y-or-n")
                    (gnosis-update 'themata '(= type "mcq") `(= id ,thema))
                    (gnosis-update 'themata '(= hypothesis '("Yes" "No"))
                                   `(= id ,thema))
                    (if (= (car (nth 2 data)) 121)
                        (gnosis-update 'themata '(= answer '("Yes"))
                                       `(= id ,thema))
                      (gnosis-update 'themata '(= answer '("No"))
                                     `(= id ,thema))))))
    ;; 8. Replace - with _ in tags (org does not support tags with dash)
    (cl-loop for tag in (gnosis--migrate-get-tags-from-column)
             if (string-match-p "-" tag)
             do (let ((new-tag (replace-regexp-in-string "-" "_" tag)))
                  (cl-loop for thema in (gnosis-select 'id 'themata
						       `(like tags ',(format "%%\"%s\"%%" tag)) t)
                           do (let* ((tags-val (car (gnosis-select '[tags] 'themata `(= id ,thema) t)))
                                     (new-tags (cl-substitute new-tag tag tags-val :test #'string-equal)))
                                (gnosis-update 'themata `(= tags ',new-tags) `(= id ,thema))))
                  (gnosis-sqlite-execute db "DELETE FROM tags")
                  (cl-loop for tag-item in (gnosis--migrate-get-tags-from-column)
                           do (gnosis-sqlite-execute db "INSERT OR IGNORE INTO tags VALUES (?)"
                                                     (list tag-item))))))
  (gnosis--db-set-version 4))

(defun gnosis-db--migrate-v5 ()
  "Migration v5: rename notes table to themata."
  (ignore-errors
    (gnosis-sqlite-execute (gnosis--ensure-db) "ALTER TABLE notes RENAME TO themata"))
  (gnosis--db-set-version 5))

(defun gnosis-db--migrate-v6 ()
  "Migration v6: merge org-gnosis tables, rename links/tags."
  (let ((db (gnosis--ensure-db)))
    ;; 0. Move deck names into thema tags, then drop decks
    (ignore-errors
      (let ((rows (gnosis-sqlite-select db
					"SELECT t.id, t.tags, d.name
                     FROM themata t JOIN decks d ON t.deck_id = d.id")))
        (dolist (row rows)
          (let* ((thema-id (nth 0 row))
                 (tags (nth 1 row))
                 (deck-name (nth 2 row))
                 (new-tags (if (listp tags)
                               (append tags (list deck-name))
                             (list tags deck-name))))
            (gnosis-sqlite-execute db
				   "UPDATE themata SET tags = ? WHERE id = ?"
				   (list new-tags thema-id))))))
    (ignore-errors
      (gnosis-sqlite-execute db "ALTER TABLE themata DROP COLUMN deck_id"))
    (ignore-errors
      (gnosis-sqlite-execute db "DROP TABLE IF EXISTS decks"))
    ;; 1. Rename existing tables (idempotent for PRAGMA 5 users)
    (ignore-errors
      (gnosis-sqlite-execute db "ALTER TABLE links RENAME TO thema_links"))
    (ignore-errors
      (gnosis-sqlite-execute db "ALTER TABLE tags RENAME TO thema_tags"))
    ;; 2. Create thema-tag junction table & populate from themata.tags
    (let ((schema (cadr (assq 'thema-tag gnosis-db--schemata))))
      (gnosis-sqlite-execute db
			     (format "CREATE TABLE IF NOT EXISTS %s (%s)"
				     (gnosis-sqlite--ident 'thema-tag)
				     (gnosis-sqlite--compile-schema schema))))
    (let ((rows (gnosis-sqlite-select db "SELECT id, tags FROM themata")))
      (dolist (row rows)
	(let ((thema-id (car row))
	      (tags (cadr row)))
	  (when (listp tags)
	    (dolist (tag tags)
	      (ignore-errors
		(gnosis-sqlite-execute db
				       "INSERT OR IGNORE INTO thema_tag (thema_id, tag) VALUES (?, ?)"
				       (list thema-id tag))))))))
    ;; Drop serialized tags column (now replaced by thema-tag junction table)
    (ignore-errors
      (gnosis-sqlite-execute db "ALTER TABLE themata DROP COLUMN tags"))
    ;; Drop thema-tags and node-tags lookup tables
    (ignore-errors
      (gnosis-sqlite-execute db "DROP TABLE IF EXISTS thema_tags"))
    (ignore-errors
      (gnosis-sqlite-execute db "DROP TABLE IF EXISTS node_tags"))
    ;; 3. Create node tables
    (dolist (table '(nodes journal node-tag node-links))
      (let ((schema (cadr (assq table gnosis-db--schemata))))
        (gnosis-sqlite-execute db
			       (format "CREATE TABLE IF NOT EXISTS %s (%s)"
				       (gnosis-sqlite--ident table)
				       (gnosis-sqlite--compile-schema schema)))))
    ;; 3. Create indexes
    (gnosis-sqlite-execute db
			   "CREATE INDEX IF NOT EXISTS idx_nodes_file ON nodes (file)")
    (gnosis-sqlite-execute db
			   "CREATE INDEX IF NOT EXISTS idx_journal_file ON journal (file)")
    ;; 4. Import from org-gnosis.db if it exists
    (let ((org-db-file (locate-user-emacs-file "org-gnosis.db")))
      (when (file-exists-p org-db-file)
        (gnosis-sqlite-execute db
			       (format "ATTACH DATABASE '%s' AS org_gnosis"
				       (expand-file-name org-db-file)))
        (ignore-errors
          (gnosis-sqlite-execute db
				 "INSERT OR IGNORE INTO nodes SELECT * FROM org_gnosis.nodes")
          (gnosis-sqlite-execute db
				 "INSERT OR IGNORE INTO journal SELECT * FROM org_gnosis.journal")
          (gnosis-sqlite-execute db
				 "INSERT OR IGNORE INTO node_tag SELECT * FROM org_gnosis.node_tag")
          (gnosis-sqlite-execute db
				 "INSERT OR IGNORE INTO node_links SELECT * FROM org_gnosis.links"))
        (gnosis-sqlite-execute db "DETACH DATABASE org_gnosis")
        (message "Imported org-gnosis data into unified database"))))
  (gnosis--db-set-version 6))

(defun gnosis--migrate-date-to-int (value)
  "Convert VALUE to YYYYMMDD integer for migration.
Handles both Lisp list dates and already-converted integers."
  (cond
   ((integerp value) value)
   ((and (listp value) (= (length value) 3))
    (gnosis--date-to-int value))
   ((null value) nil)
   (t (warn "gnosis: unexpected date value during migration: %S" value)
      nil)))

(defun gnosis-db--migrate-v7 ()
  "Migration v7: convert date columns from Lisp lists to YYYYMMDD integers."
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-with-transaction db
      ;; 1. Convert review_log.last_rev and next_rev
      (dolist (row (gnosis-sqlite-select db
					 "SELECT id, last_rev, next_rev FROM review_log"))
        (let ((new-last (gnosis--migrate-date-to-int (nth 1 row)))
              (new-next (gnosis--migrate-date-to-int (nth 2 row))))
          (when (and new-last new-next
                     (or (not (equal (nth 1 row) new-last))
                         (not (equal (nth 2 row) new-next))))
            (gnosis-sqlite-execute db
				   "UPDATE review_log SET last_rev = ?, next_rev = ? WHERE id = ?"
				   (list new-last new-next (nth 0 row))))))
      ;; 2. Convert activity_log.date
      (dolist (row (gnosis-sqlite-select db
					 "SELECT rowid, date FROM activity_log"))
        (let ((new-date (gnosis--migrate-date-to-int (nth 1 row))))
          (when (and new-date (not (equal (nth 1 row) new-date)))
            (gnosis-sqlite-execute db
				   "UPDATE activity_log SET date = ? WHERE rowid = ?"
				   (list new-date (nth 0 row))))))
      ;; 3. Create indexes
      (gnosis--db-create-indexes db)))
  (gnosis--db-set-version 7))

(defun gnosis-db--migrate-v8 ()
  "Add source_guid column to themata for Anki GUID-based dedup."
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-execute db
			   "ALTER TABLE themata ADD COLUMN source_guid TEXT")
    (gnosis-sqlite-execute db
			   "CREATE INDEX IF NOT EXISTS idx_themata_source_guid ON themata(source_guid)"))
  (gnosis--db-set-version 8))

(defconst gnosis-db--migrations
  `((1 . gnosis-db--migrate-v1)
    (2 . gnosis-db--migrate-v2)
    (3 . gnosis-db--migrate-v3)
    (4 . gnosis-db--migrate-v4)
    (5 . gnosis-db--migrate-v5)
    (6 . gnosis-db--migrate-v6)
    (7 . gnosis-db--migrate-v7)
    (8 . gnosis-db--migrate-v8))
  "Alist of (VERSION . FUNCTION).
Each migration brings the DB from VERSION-1 to VERSION.")

(defun gnosis--db-run-migrations (current-version)
  "Run all pending migrations from CURRENT-VERSION to `gnosis-db-version'.
Commits the database after all migrations complete."
  (let ((migrated nil))
    (cl-loop for (version . func) in gnosis-db--migrations
	     when (> version current-version)
	     do (progn
		  (message "Gnosis: running migration to v%d..." version)
		  (funcall func)
		  (message "Gnosis: migration to v%d complete" version)
		  (setq migrated version)))
    (when migrated
      (gnosis--commit-migration current-version migrated))))

(defun gnosis--commit-migration (from to)
  "Commit database after migrating from version FROM to TO.
Uses synchronous git operations because migration must complete
before database initialization continues."
  (let ((default-directory gnosis-dir))
    (unless gnosis-testing
      (when (file-exists-p (expand-file-name ".git" gnosis-dir))
        (call-process (executable-find "git") nil nil nil "add" "gnosis.db")
        (call-process (executable-find "git") nil nil nil
                      "commit" "-m"
                      (format "Migrate database v%d -> v%d" from to))))))

(defun gnosis-db-init ()
  "Initialize database: create tables if fresh, run pending migrations."
  (let ((version (gnosis--db-version)))
    (if (and (zerop version) (not (gnosis--db-has-tables-p)))
	;; Fresh database: create all tables at current version
	(gnosis--db-create-tables)
      ;; Existing database: run any pending migrations
      (gnosis--db-run-migrations version))))

(provide 'gnosis-db)
;;; gnosis-db.el ends here
