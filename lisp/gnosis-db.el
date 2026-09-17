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
(require 'gnosis-sqlite)
(require 'gnosis-logical-day)
(require 'gnosis-fsrs)

(declare-function gnosis-vc--auto-commit "gnosis-vc"
                  (message &optional existing-only no-push))

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

(defconst gnosis-db-version 11
  "Gnosis database version.")

(defvar gnosis--id-cache nil
  "Hash table of existing and reserved thema IDs during batch import.
When non-nil, `gnosis-generate-id' checks and reserves IDs here before
asynchronous insertion.  Reservations do not establish stored content.")

;;; Connection

(defun gnosis-db--open (directory)
  "Return a validated database connection for DIRECTORY without publishing it.
Create DIRECTORY if needed.  Close the candidate on any nonlocal exit."
  (let ((gnosis-dir (expand-file-name directory)))
    (unless (file-directory-p gnosis-dir)
      (make-directory gnosis-dir t))
    (let ((candidate (gnosis-sqlite-open
                      (expand-file-name "gnosis.db" gnosis-dir)))
          ready)
      (unwind-protect
          (progn
            ;; Recursive query helpers see the candidate only during init.
            (let ((gnosis-db candidate)) (gnosis-db-init))
            (setq ready t)
            candidate)
        (unless ready (gnosis-sqlite-close candidate))))))

(defun gnosis--ensure-db ()
  "Return the gnosis database connection, opening it if necessary.
Create `gnosis-dir' and validate storage before publishing a new connection."
  (or gnosis-db
      (setq gnosis-db (gnosis-db--open gnosis-dir))))

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

(defun gnosis-db-review-activity (&optional db)
  "Return (DATE REVIEWED-TOTAL REVIEWED-NEW) activity rows from DB."
  (gnosis-sqlite-select
   (or db (gnosis--ensure-db))
   "SELECT date, SUM(reviewed_total), SUM(reviewed_new)
      FROM
        (SELECT date, reviewed_total, reviewed_new
           FROM review_activity_baseline
         UNION ALL
         SELECT review_day, COUNT(*), SUM(new_p)
           FROM review_events WHERE event_id NOT IN (SELECT event_id FROM review_voids)
           GROUP BY review_day)
     GROUP BY date ORDER BY date"))

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
rows that violate a UNIQUE constraint.
Historical six-field thema rows name their original columns in schema 9;
new metadata keeps its default, including on retained archive layouts."
  (let* ((rows (if (vectorp values) (list values) values))
         (columns (if (and (eq table 'themata) rows
                           (seq-every-p (lambda (row) (and (vectorp row) (= (length row) 6))) rows)
                           (>= (gnosis--db-version) 9))
                      " (id, type, keimenon, hypothesis, answer, source_guid)" ""))
         (compiled (gnosis-sqlite--compile-values values))
	 (sql (format "INSERT%s INTO %s%s VALUES %s"
		      (if or-ignore " OR IGNORE" "")
		      (gnosis-sqlite--ident table)
                      columns (car compiled))))
    (gnosis-sqlite--execute-compiled (gnosis--ensure-db) sql (cdr compiled))))

(defun gnosis-update (table value where)
  "Update TABLE records with VALUE based on WHERE condition.

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
  "Convert DATE list (year month day) to YYYYMMDD integer."
  (+ (* (nth 0 date) 10000) (* (nth 1 date) 100) (nth 2 date)))

(defun gnosis--int-to-date (int)
  "Convert YYYYMMDD integer INT to (year month day) list."
  (list (/ int 10000) (% (/ int 100) 100) (% int 100)))

(defun gnosis--today-int ()
  "Return today as a YYYYMMDD integer.
Respects `gnosis-day-start-hour'."
  (gnosis--date-to-int (gnosis-date)))

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
Each ID has optional LENGTH, defaulting to 18 digits.
Uses `gnosis--id-cache' for O(1) collision checking when bound."
  (cl-loop repeat n collect (gnosis-generate-id length)))

;;; Schema

(defconst gnosis-db--schemata
  '((themata
     ([(id integer :primary-key)
       (type text :not-null)
       (keimenon text :not-null)
       (hypothesis text :not-null)
       (answer text :not-null)
       (source-guid text)
       (accepted-aliases text)
       (rubric text)]))
    (scheduler-config
     ([(id integer :primary-key :not-null)
       (algorithm text :not-null)
       (model text :not-null)
       (implementation text :not-null)
       (desired-retention real :not-null)
       (parameters text :not-null)]))
    (scheduler-baseline
     ([(thema-id integer :primary-key :not-null)
       (due-day integer :not-null)
       (reps integer :not-null)
       (lapses integer :not-null)]
      (:foreign-key [thema-id] :references themata [id]
                    :on-delete :cascade)))
    (scheduler-state
     ([(thema-id integer :primary-key :not-null)
       (config-id integer :not-null)
       (stability real)
       (difficulty real)
       (last-reviewed-at-us integer)
       (last-review-day integer)
       (due-day integer :not-null)
       (reps integer :not-null)
       (lapses integer :not-null)
       (suspended integer :not-null)]
      (:foreign-key [thema-id] :references scheduler-baseline [thema-id]
                    :on-delete :cascade)
      (:foreign-key [config-id] :references scheduler-config [id])))
    (review-events
     ([(event-id text :primary-key :not-null)
       (thema-id integer :not-null)
       (config-id integer :not-null)
       (reviewed-at-us integer :not-null)
       (review-day integer :not-null)
       (rating integer :not-null)
       (elapsed-days integer :not-null)
       (prior-stability real)
       (prior-difficulty real)
       (stability real :not-null)
       (difficulty real :not-null)
       (raw-interval-days real :not-null)
       (calendar-interval-days integer :not-null)
       (due-day integer :not-null)
       (reps-before integer :not-null)
       (reps-after integer :not-null)
       (lapses-before integer :not-null)
       (lapses-after integer :not-null)
       (new-p integer :not-null)]
      (:foreign-key [thema-id] :references scheduler-baseline [thema-id]
                    :on-delete :cascade)
      (:foreign-key [config-id] :references scheduler-config [id])
      (:check "rating IN (1, 3)")
      (:check "elapsed_days >= 0")
      (:check "(prior_stability IS NULL) = (prior_difficulty IS NULL)")
      (:check "stability > 0")
      (:check "difficulty BETWEEN 1 AND 10")
      (:check "raw_interval_days >= 0")
      (:check "calendar_interval_days >= 1")
      (:check "reps_before >= 0 AND reps_after = reps_before + 1")
      (:check "lapses_before >= 0")
      (:check "lapses_after = lapses_before + CASE rating WHEN 1 THEN 1 ELSE 0 END")
      (:check "new_p IN (0, 1)")
      (:check "new_p = CASE reps_before WHEN 0 THEN 1 ELSE 0 END")))
    (study-history
     ([(session-id text :primary-key :not-null)
       (data text :not-null)]))
    (study-session
     ([(id integer :primary-key :not-null)
       (data text :not-null)]
      (:check "id = 1")))
    (scheduler-active
     ([(id integer :primary-key :not-null)
       (config-id integer :not-null)]
      (:check "id = 1")
      (:foreign-key [config-id] :references scheduler-config [id])))
    (review-voids
     ([(correction-id text :primary-key :not-null)
       (event-id text :not-null)]
      (:unique [event-id])
      (:foreign-key [event-id] :references review-events [event-id]
                    :on-delete :cascade)))
    (practice-voids
     ([(correction-id text :primary-key :not-null)
       (event-id text :not-null)]
      (:unique [event-id])
      (:foreign-key [event-id] :references practice-events [event-id]
                    :on-delete :cascade)))
    (practice-encounters
     ([(event-id text :primary-key :not-null)
       (data text :not-null)]
      (:foreign-key [event-id] :references practice-events [event-id]
                    :on-delete :cascade)))
    (practice-events
     ([(event-id text :primary-key :not-null)
       (thema-id integer :not-null)
       (session-id text :not-null)
       (attempt integer :not-null)
       (reviewed-at-us integer :not-null)
       (rating integer :not-null)]
      (:foreign-key [thema-id] :references themata [id]
                    :on-delete :cascade)
      (:unique [session-id attempt])
      (:check "attempt > 0")
      (:check "reviewed_at_us > 0")
      (:check "rating IN (1, 3)")))
    (review-activity-baseline
     ([(date integer :primary-key :not-null)
       (reviewed-total integer :not-null)
       (reviewed-new integer :not-null)]
      (:check "reviewed_total >= 0")
      (:check "reviewed_new BETWEEN 0 AND reviewed_total")))
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

(defun gnosis-db--install-default-scheduler-config (db)
  "Install the pinned default scheduler configuration into DB."
  (gnosis-sqlite-execute
   db
   "INSERT INTO scheduler_config
      (id, algorithm, model, implementation, desired_retention, parameters)
    VALUES (?, ?, ?, ?, ?, ?)"
   (list 1 gnosis-fsrs--algorithm gnosis-fsrs--model
         gnosis-fsrs--implementation
         gnosis-fsrs-default-retention
         gnosis-fsrs-default-parameters)))

(defconst gnosis-db--scheduler-guards
  '("CREATE TRIGGER scheduler_config_no_replace
        BEFORE INSERT ON scheduler_config
        WHEN EXISTS (SELECT 1 FROM scheduler_config WHERE id = NEW.id)
        BEGIN
          SELECT RAISE(ABORT, 'scheduler config already exists');
        END"
    "CREATE TRIGGER scheduler_config_no_update
        BEFORE UPDATE ON scheduler_config
        BEGIN
          SELECT RAISE(ABORT, 'scheduler config is immutable');
        END"
    "CREATE TRIGGER scheduler_config_no_delete
        BEFORE DELETE ON scheduler_config
        BEGIN
          SELECT RAISE(ABORT, 'scheduler config is immutable');
        END"
    "CREATE TRIGGER scheduler_baseline_no_replace
        BEFORE INSERT ON scheduler_baseline
        WHEN EXISTS
          (SELECT 1 FROM scheduler_baseline
             WHERE thema_id = NEW.thema_id)
        BEGIN
          SELECT RAISE(ABORT, 'scheduler baseline already exists');
        END"
    "CREATE TRIGGER scheduler_baseline_no_update
        BEFORE UPDATE ON scheduler_baseline
        BEGIN
          SELECT RAISE(ABORT, 'scheduler baseline is immutable');
        END"
    "CREATE TRIGGER review_events_no_replace
        BEFORE INSERT ON review_events
        WHEN EXISTS
          (SELECT 1 FROM review_events WHERE event_id = NEW.event_id)
        BEGIN
          SELECT RAISE(ABORT, 'review event already exists');
        END"
    "CREATE TRIGGER review_activity_baseline_no_replace
        BEFORE INSERT ON review_activity_baseline
        WHEN EXISTS
          (SELECT 1 FROM review_activity_baseline WHERE date = NEW.date)
        BEGIN
          SELECT RAISE(ABORT, 'review activity baseline already exists');
        END"
    "CREATE TRIGGER review_activity_baseline_no_update
        BEFORE UPDATE ON review_activity_baseline
        BEGIN
          SELECT RAISE(ABORT, 'review activity baseline is immutable');
        END"
    "CREATE TRIGGER review_activity_baseline_no_delete
        BEFORE DELETE ON review_activity_baseline
        BEGIN
          SELECT RAISE(ABORT, 'review activity baseline is immutable');
        END"
    "CREATE TRIGGER scheduler_baseline_no_direct_delete
        BEFORE DELETE ON scheduler_baseline
        WHEN EXISTS (SELECT 1 FROM themata WHERE id = OLD.thema_id)
        BEGIN
          SELECT RAISE(ABORT, 'scheduler baseline requires hard thema deletion');
        END"
    "CREATE TRIGGER review_events_no_update
        BEFORE UPDATE ON review_events
        BEGIN
          SELECT RAISE(ABORT, 'review events are immutable');
        END"
    "CREATE TRIGGER review_events_no_direct_delete
        BEFORE DELETE ON review_events
        WHEN EXISTS
          (SELECT 1 FROM scheduler_baseline
             WHERE thema_id = OLD.thema_id)
        BEGIN
          SELECT RAISE(ABORT, 'review events require hard thema deletion');
        END")
  "Supported scheduler evidence guards, shared by creation and validation.")

(defun gnosis-db--create-scheduler-guards (db)
  "Create append-only review-event guards on DB when available."
  (when (gnosis-table-exists-p 'review-events)
    (dolist (sql gnosis-db--scheduler-guards)
      (gnosis-sqlite-execute
       db (string-replace "CREATE TRIGGER " "CREATE TRIGGER IF NOT EXISTS " sql)))))

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
      (gnosis-db--install-default-scheduler-config db)
      (gnosis-sqlite-execute db "INSERT INTO scheduler_active VALUES (1, 1)")
      (gnosis-db--create-study-guards db)
      (gnosis-db--create-encounter-guards db)
      (gnosis--db-create-indexes db)
      (gnosis-db--create-scheduler-guards db)
      (gnosis--db-set-version gnosis-db-version))))

(defun gnosis--db-create-indexes (db)
  "Create all performance indexes on DB."
  (dolist (stmt '("CREATE INDEX IF NOT EXISTS idx_thema_tag_thema_id
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
		  "CREATE INDEX IF NOT EXISTS idx_nodes_file
                   ON nodes(file)"
		  "CREATE INDEX IF NOT EXISTS idx_journal_file
                   ON journal(file)"))
    (gnosis-sqlite-execute db stmt))
  (when (gnosis-table-exists-p 'scheduler-state)
    (gnosis-sqlite-execute
     db
     "CREATE INDEX IF NOT EXISTS idx_scheduler_state_due
        ON scheduler_state(suspended, due_day, reps)"))
  (when (gnosis-table-exists-p 'review-events)
    (gnosis-sqlite-execute
     db
     "CREATE INDEX IF NOT EXISTS idx_review_events_replay
        ON review_events(thema_id, reviewed_at_us, event_id)")
    (gnosis-sqlite-execute
     db
     "CREATE INDEX IF NOT EXISTS idx_review_events_day
        ON review_events(review_day)"))
  ;; source_guid index: created by v8 migration for existing DBs,
  ;; or here for fresh DBs where the column already exists
  (gnosis-sqlite-execute db
                         "CREATE INDEX IF NOT EXISTS idx_themata_source_guid ON themata(source_guid)"))

(defun gnosis--db-has-tables-p ()
  "Return non-nil if the database has user tables."
  (let ((tables (gnosis-sqlite-select (gnosis--ensure-db)
				      "SELECT name FROM sqlite_master WHERE type = 'table'")))
    (length> tables 0)))

;;; Migrations

(defun gnosis-db--migrate-v9 ()
  "Upgrade released schema 8 directly to the complete schema 9."
  (unless (= 8 (gnosis--db-version))
    (error "Only released Gnosis schema 8 can be migrated"))
  (gnosis-db--check-schema (gnosis--ensure-db) 8)
  (let ((db (gnosis--ensure-db))
        (tables '(scheduler-config scheduler-baseline scheduler-state
                  review-events review-activity-baseline practice-events
                  study-session scheduler-active review-voids practice-voids
                  study-history)))
    (gnosis-sqlite-with-transaction db
      (dolist (table tables)
        (let ((schema (cadr (assq table gnosis-db--schemata))))
          (gnosis-sqlite-execute
           db (format "CREATE TABLE %s (%s)"
                      (gnosis-sqlite--ident table)
                      (gnosis-sqlite--compile-schema schema)))))
      (gnosis-sqlite-execute db "ALTER TABLE themata ADD COLUMN accepted_aliases TEXT")
      (gnosis-db--install-default-scheduler-config db)
      (gnosis-sqlite-execute db "INSERT INTO scheduler_active VALUES (1, 1)")
      (gnosis-db--create-study-guards db)
      (when (> (caar (gnosis-sqlite-select
                      db
                      "SELECT COUNT(*) FROM themata AS t
                         LEFT JOIN review_log AS r ON r.id = t.id
                        WHERE r.id IS NULL"))
               0)
        (error "Gnosis: v8 thema lacks scheduler history"))
      (gnosis-sqlite-execute
       db
       "INSERT INTO scheduler_baseline (thema_id, due_day, reps, lapses)
        SELECT id, next_rev, n, t_fails FROM review_log")
      (gnosis-sqlite-execute
       db
       "INSERT INTO scheduler_state
          (thema_id, config_id, stability, difficulty,
           last_reviewed_at_us, last_review_day, due_day,
           reps, lapses, suspended)
        SELECT id, 1, NULL, NULL, NULL, NULL, next_rev, n, t_fails, suspend
          FROM review_log")
      (gnosis-sqlite-execute
       db
       "INSERT INTO review_activity_baseline
          (date, reviewed_total, reviewed_new)
        SELECT date, SUM(reviewed_total), SUM(reviewed_new)
          FROM activity_log GROUP BY date")
      (dolist (table '(review review-log activity-log))
        (gnosis-sqlite-execute
         db (format "DROP TABLE %s" (gnosis-sqlite--ident table))))
      (gnosis--db-create-indexes db)
      (gnosis-db--create-scheduler-guards db)
      (gnosis--db-set-version 9))))

(defconst gnosis-db--study-guards
  (cl-loop
   for (table parent parent-key key conflict)
   in '(("practice_events" "themata" "id" "thema_id"
         "event_id = NEW.event_id OR (session_id = NEW.session_id AND attempt = NEW.attempt)")
        ("review_voids" "review_events" "event_id" "event_id"
         "correction_id = NEW.correction_id OR event_id = NEW.event_id")
        ("practice_voids" "practice_events" "event_id" "event_id"
         "correction_id = NEW.correction_id OR event_id = NEW.event_id"))
   append
   (list
    (format "CREATE TRIGGER %s_no_update BEFORE UPDATE ON %s
               BEGIN SELECT RAISE(ABORT, 'immutable study evidence'); END" table table)
    ;; REPLACE can delete conflicts without firing DELETE triggers.
    (format "CREATE TRIGGER %s_no_replace BEFORE INSERT ON %s
               WHEN EXISTS (SELECT 1 FROM %s WHERE %s)
               BEGIN SELECT RAISE(ABORT, 'study identity exists'); END"
            table table table conflict)
    (format "CREATE TRIGGER %s_no_direct_delete BEFORE DELETE ON %s
               WHEN EXISTS (SELECT 1 FROM %s WHERE %s = OLD.%s)
               BEGIN SELECT RAISE(ABORT, 'hard deletion required'); END"
            table table parent parent-key key)))
  "Supported study evidence guards, shared by creation and validation.")

(defun gnosis-db--create-study-guards (db)
  "Protect immutable practice and correction evidence in DB."
  (dolist (sql gnosis-db--study-guards)
    (gnosis-sqlite-execute db sql)))

(defconst gnosis-db--encounter-guards
  '("CREATE TRIGGER practice_encounters_no_update BEFORE UPDATE ON practice_encounters
       BEGIN SELECT RAISE(ABORT, 'immutable practice encounter'); END"
    "CREATE TRIGGER practice_encounters_no_replace BEFORE INSERT ON practice_encounters
       WHEN EXISTS (SELECT 1 FROM practice_encounters WHERE event_id = NEW.event_id)
       BEGIN SELECT RAISE(ABORT, 'practice encounter identity exists'); END"
    "CREATE TRIGGER practice_encounters_no_direct_delete BEFORE DELETE ON practice_encounters
       WHEN EXISTS (SELECT 1 FROM practice_events WHERE event_id = OLD.event_id)
       BEGIN SELECT RAISE(ABORT, 'hard deletion required'); END")
  "Schema 10 guards for minimal accepted practice encounter evidence.")

(defun gnosis-db--create-encounter-guards (db)
  "Protect accepted practice encounter evidence in DB."
  (dolist (sql gnosis-db--encounter-guards) (gnosis-sqlite-execute db sql)))

(defun gnosis-db--migrate-v10 ()
  "Upgrade validated released schema 9 to schema 10 without inventing evidence."
  (unless (= 9 (gnosis--db-version)) (error "Expected released schema 9"))
  (let ((db (gnosis--ensure-db))
        (schema (cadr (assq 'practice-encounters gnosis-db--schemata))))
    (gnosis-db--check-schema db 9)
    (gnosis-sqlite-with-transaction db
      (gnosis-sqlite-execute
       db (format "CREATE TABLE practice_encounters (%s)"
                  (gnosis-sqlite--compile-schema schema)))
      (gnosis-db--create-encounter-guards db)
      (gnosis--db-set-version 10))))

(defun gnosis-db--migrate-v11 ()
  "Upgrade validated released schema 10 with independent nullable rubric storage."
  (unless (= 10 (gnosis--db-version)) (error "Expected released schema 10"))
  (let ((db (gnosis--ensure-db)))
    (gnosis-db--check-schema db 10)
    (gnosis-sqlite-with-transaction db
      (gnosis-sqlite-execute db "ALTER TABLE themata ADD COLUMN rubric TEXT")
      (gnosis--db-set-version 11))))

(defun gnosis--db-run-migrations (current-version &optional no-commit)
  "Upgrade released CURRENT-VERSION to `gnosis-db-version'.
Commit afterwards unless NO-COMMIT defers that until outer validation."
  (gnosis-sqlite-with-transaction (gnosis--ensure-db)
    (pcase current-version
      (8 (gnosis-db--migrate-v9) (gnosis-db--migrate-v10) (gnosis-db--migrate-v11))
      (9 (gnosis-db--migrate-v10) (gnosis-db--migrate-v11))
      (10 (gnosis-db--migrate-v11))
      (11 nil)
      (_ (error "Unsupported Gnosis migration source %s" current-version))))
  (when (and (not no-commit) (< current-version gnosis-db-version))
    (gnosis--commit-migration current-version gnosis-db-version)))

(defun gnosis--commit-migration (from to)
  "Optionally commit the completed migration from version FROM to TO.
Use an existing repository without pushing.  Git runs asynchronously;
its absence or failure does not invalidate the database upgrade."
  (require 'gnosis-vc)
  (gnosis-vc--auto-commit (format "Migrate database v%d -> v%d" from to) t t))

(defconst gnosis-db-min-version 8
  "Oldest supported schema: released Gnosis 0.10.6.
Released schema 8 upgrades through schemas 9 and 10 to schema 11.
Private development schemas require a separate, verified conversion.")

(defconst gnosis-db--legacy-schemata
  '((review
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
       (c-fails integer :not-null)   ;; Consecutive failed reviews
       (t-fails integer :not-null)   ;; Total failed reviews
       (suspend integer :not-null)   ;; Binary value, 1=suspended
       (n integer :not-null)]        ;; Number of reviews
      (:foreign-key [id] :references themata [id]
		    :on-delete :cascade)))
    (activity-log
     ([(date integer :not-null)
       (reviewed-total integer :not-null)
       (reviewed-new integer :not-null)])))
  "Legacy scheduler tables declared by released Gnosis 0.10.6.")

(defun gnosis-db--schemata-for-version (version)
  "Return required table declarations for supported VERSION."
  (pcase version
    (8 (append gnosis-db--legacy-schemata
               (seq-filter
                (lambda (entry)
                  (memq (car entry) '(themata extras thema-tag thema-links nodes
                                     journal node-tag node-links)))
                gnosis-db--schemata)))
    (9 (assq-delete-all 'practice-encounters (copy-sequence gnosis-db--schemata)))
    ((or 10 11) gnosis-db--schemata)
    (_ (error "Unsupported Gnosis schema %s" version))))

(defun gnosis-db--compatible-columns-p (table schema actual &optional version)
  "Check ACTUAL column metadata against TABLE's SCHEMA and retained layouts.
Keep column order: positional readers and writers rely on it.  Only themata's
observed nullable archive field, the equivalent composite tag key, and the
historical text-affinity link source may differ from fresh storage.
VERSION defaults to the current schema.  Schemas before 9 lack aliases;
schemas before 11 lack rubric."
  (let* ((version (or version gnosis-db-version))
         (columns (append (car schema) nil))
         (columns (if (eq table 'themata)
                      (seq-remove (lambda (column)
                                    (or (and (< version 9) (eq (car column) 'accepted-aliases))
                                        (and (< version 11) (eq (car column) 'rubric))))
                                  columns)
                    columns))
         (expected
         (mapcar (lambda (column)
                   (list (gnosis-sqlite--ident (car column))
                         (upcase (symbol-name (cadr column)))
                         (if (memq :not-null column) 1 0)
                         nil (if (memq :primary-key column) 1 0)))
                 columns)))
    (or (equal actual expected)
        (pcase table
          ('themata
           (let ((archive '("archived_at_us" "INTEGER" 0 nil 0)))
             (and (= (length actual) (1+ (length expected)))
                  (member archive (nthcdr 6 actual))
                  (equal expected (remove archive actual)))))
          ('thema-tag
           (equal actual '(("thema_id" "INTEGER" 1 nil 1)
                           ("tag" "TEXT" 1 nil 2))))
          ;; a63dbe8 changed the fresh declaration, not existing link tables.
          ('thema-links
           (equal actual '(("source" "TEXT" 0 nil 0)
                           ("dest" "TEXT" 0 nil 0))))))))

(defun gnosis-db--guard-tokens (sql)
  "Return comparable tokens for a supported evidence guard SQL declaration.
Ignore whitespace and unquoted case; preserve string literals.  Accept the
quoted identifiers SQLite emits when renaming tables.  This is deliberately
not a general SQL equivalence test: unknown guard definitions are refused."
  (with-temp-buffer
    (insert sql)
    (goto-char (point-min))
    (cl-loop
     while (re-search-forward
            (rx (or (seq "'" (* (or "''" (not (any "'")))) "'")
                    (seq "\"" (* (or "\"\"" (not (any "\"")))) "\"")
                    (+ (any alnum "_"))
                    (not (any " \t\r\n"))))
            nil t)
     for token = (match-string-no-properties 0)
     collect (pcase (aref token 0)
               (?' token)
               (?\" (downcase (string-replace "\"\"" "\"" (substring token 1 -1))))
               (_ (downcase token))))))

(defun gnosis-db--check-schema (db version)
  "Check DB's tables, columns, ownership and evidence guards at VERSION.
This is a compatibility check, not an exact DDL fingerprint or a check of
all application values.  Reject damaged required objects before any writes."
  (let ((tables (mapcar #'car (sqlite-select db
                  "SELECT name FROM sqlite_master WHERE type = 'table'")))
        (triggers (sqlite-select db
                   "SELECT name, sql FROM sqlite_master WHERE type = 'trigger'")))
    (when (< version 10)
      (dolist (entry gnosis-db--schemata)
        (unless (assq (car entry) (gnosis-db--schemata-for-version version))
          (when (member (gnosis-sqlite--ident (car entry)) tables)
            (error "Unexpected current table in released schema %d: %s" version (car entry))))))
    (pcase-dolist (`(,table ,schema) (gnosis-db--schemata-for-version version))
        (let ((name (gnosis-sqlite--ident table)))
          (unless (and (member name tables)
                       (gnosis-db--compatible-columns-p
                        table schema
                        (mapcar (lambda (row)
                                  (list (nth 1 row) (upcase (nth 2 row))
                                        (nth 3 row) (nth 4 row) (nth 5 row)))
                                (sqlite-select db (format "PRAGMA table_info(%s)" name)))
                        version))
            (error "Invalid Gnosis schema %d: required table/columns %s" version name))))
    (when (>= version 9)
      (dolist (sql (append gnosis-db--scheduler-guards gnosis-db--study-guards
                           (when (>= version 10) gnosis-db--encounter-guards)))
        (let* ((expected (gnosis-db--guard-tokens sql))
               (name (nth 2 expected))
               (actual (cadr (assoc-string name triggers t))))
          (unless (and actual (equal expected (gnosis-db--guard-tokens actual)))
            (error "Invalid Gnosis schema %d: missing or changed guard %s"
                   version name))))))
  ;; Row integrity alone cannot detect a missing deletion cascade.
  (when (>= version 9)
    (pcase-dolist
        (`(,table ,parent ,column ,parent-column)
         (append '(("scheduler_baseline" "themata" "thema_id" "id")
                   ("scheduler_state" "scheduler_baseline" "thema_id" "thema_id")
                   ("review_events" "scheduler_baseline" "thema_id" "thema_id")
                   ("review_voids" "review_events" "event_id" "event_id")
                   ("practice_events" "themata" "thema_id" "id")
                   ("practice_voids" "practice_events" "event_id" "event_id"))
                 (when (>= version 10)
                   '(("practice_encounters" "practice_events" "event_id" "event_id")))))
      (let ((expected
             (cons (list 0 parent column parent-column "NO ACTION" "CASCADE" "NONE")
                   (when (member table '("scheduler_state" "review_events"))
                     '((0 "scheduler_config" "config_id" "id" "NO ACTION" "NO ACTION" "NONE")))))
            ;; Constraint numbers and declaration order are not ownership.
            ;; Keep sequence numbers so composite keys cannot pass as singles.
            (actual (mapcar #'cdr (sqlite-select
                                  db (format "PRAGMA foreign_key_list(%s)" table)))))
        (unless (and (= (length expected) (length actual))
                     (seq-every-p (lambda (key) (member key actual)) expected))
          (error "Invalid %s ownership constraint" table)))))
  (when (and (>= version 9)
             (not (equal '((1)) (sqlite-select db "SELECT id FROM scheduler_config WHERE id = 1"))))
    (error "Gnosis database is missing its baseline scheduler configuration"))
  (when (and (>= version 9)
             (not (equal '((1)) (sqlite-select db "SELECT id FROM scheduler_active"))))
    (error "Gnosis database must have one active scheduler configuration"))
  (unless (equal '(("ok")) (sqlite-select db "PRAGMA quick_check"))
    (error "Gnosis database integrity check failed"))
  (when (sqlite-select db "PRAGMA foreign_key_check")
    (error "Gnosis database has foreign-key violations")))

(defun gnosis-db-init ()
  "Initialize a fresh database or validate and upgrade a supported schema.
Reject unknown versions and incomplete required schemas without migrating."
  (let ((version (gnosis--db-version))
        (db (gnosis--ensure-db)))
    (if (and (zerop version) (not (gnosis--db-has-tables-p)))
        (gnosis--db-create-tables)
      (unless (<= gnosis-db-min-version version gnosis-db-version)
        (user-error "Unsupported Gnosis schema %d (supported %d–%d); preserve a backup and use matching source"
                    version gnosis-db-min-version gnosis-db-version))
      (gnosis-db--check-schema db version)
      (gnosis-sqlite-with-transaction db
        (gnosis--db-run-migrations version t)
        (gnosis-db--check-schema db gnosis-db-version))
      (when (< version gnosis-db-version)
        (gnosis--commit-migration version gnosis-db-version)))))

(provide 'gnosis-db)
;;; gnosis-db.el ends here
