;;; gnosis-test-db-safety.el --- Open and backup safety -*- lexical-binding: t; -*-

(require 'ert)
(require 'gnosis-study)
(require 'gnosis-review)
(require 'gnosis-test-schema-v9)

(defmacro gnosis-test-safety (&rest body)
  "Run BODY without a published connection, in a disposable directory."
  (declare (indent 0) (debug t))
  `(let* ((gnosis-dir (make-temp-file "gnosis-safety-" t))
          (gnosis-db nil)
          (gnosis-testing t)
          (gnosis-vc-auto-push nil)
          (gnosis-vc--pull-owner nil))
     (unwind-protect (progn ,@body)
       (when gnosis-db (sqlite-close gnosis-db))
       (delete-directory gnosis-dir t))))

(defun gnosis-test-safety-v9 ()
  "Create a closed historical v9 database with nonempty retained facts."
  (let ((gnosis-db (gnosis-sqlite-open (expand-file-name "gnosis.db" gnosis-dir))))
    (unwind-protect
        (progn
          (gnosis-test--create-v9-schema)
          (gnosis--insert-into 'themata '([1 "basic" "Question λ" ("") ("Answer") "source"]))
          (gnosis--insert-into 'extras '([1 "Context" "image.png"]))
          (gnosis--insert-into 'thema-tag '([1 "needs_work"]))
          (gnosis--insert-into 'thema-links '([1 "source-id"]))
          (gnosis--insert-into 'nodes '(["source-id" "source.org" "Topic" "1" nil nil nil]))
          (gnosis--insert-into 'scheduler-baseline '([1 20260907 3 1]))
          (gnosis--insert-into 'scheduler-state '([1 1 nil nil nil nil 20260907 3 1 0]))
          (gnosis--insert-into 'review-activity-baseline '([20260906 3 1])))
      (sqlite-close gnosis-db))))

(defun gnosis-test-safety-snapshot (file)
  "Return schema/version and all table rows of FILE without initialization."
  (let ((db (sqlite-open file)))
    (unwind-protect
        (list (sqlite-select db "PRAGMA user_version")
              (sqlite-select db "SELECT type, name, tbl_name, sql FROM sqlite_master ORDER BY type, name")
              (mapcar (lambda (row)
                        (cons (car row) (sqlite-select db (format "SELECT * FROM %s ORDER BY 1" (car row)))))
                      (sqlite-select db "SELECT name FROM sqlite_master WHERE type = 'table' ORDER BY name")))
      (sqlite-close db))))

(ert-deftest gnosis-db-safety-unsupported-and-malformed-retry ()
  "Reject unknown versions and missing required objects, twice, without writes."
  (dolist (sql '("PRAGMA user_version = 13" "PRAGMA user_version = 8" "PRAGMA user_version = 7"
                 "PRAGMA user_version = 1" "PRAGMA user_version = 0"
                 "DROP TABLE study_history"
                 "ALTER TABLE study_history RENAME COLUMN data TO broken"
                 "DROP TRIGGER practice_events_no_replace"
                 "DELETE FROM scheduler_active"))
    (gnosis-test-safety
      (gnosis--ensure-db)
      (sqlite-execute gnosis-db sql)
      (sqlite-close gnosis-db)
      (setq gnosis-db nil)
      (let* ((file (expand-file-name "gnosis.db" gnosis-dir))
             (before (gnosis-test-safety-snapshot file)))
        (dotimes (_ 2)
          (should-error (gnosis--ensure-db))
          (should-not gnosis-db)
          (should (equal before (gnosis-test-safety-snapshot file))))))))

(ert-deftest gnosis-db-safety-required-column-constraints ()
  (dolist (ddl '("CREATE TABLE study_history (session_id TEXT NOT NULL, data TEXT NOT NULL)"
                 "CREATE TABLE study_history (session_id TEXT PRIMARY KEY NOT NULL, data TEXT)"
                 "CREATE VIEW study_history AS SELECT 'id' AS session_id, 'data' AS data"))
    (gnosis-test-safety
      (gnosis--ensure-db)
      (sqlite-execute gnosis-db "DROP TABLE study_history")
      (sqlite-execute gnosis-db ddl)
      (sqlite-close gnosis-db)
      (setq gnosis-db nil)
      (let* ((file (expand-file-name "gnosis.db" gnosis-dir))
             (before (gnosis-test-safety-snapshot file)))
        (dotimes (_ 2)
          (should-error (gnosis--ensure-db))
          (should-not gnosis-db)
          (should (equal before (gnosis-test-safety-snapshot file))))))))

(ert-deftest gnosis-db-safety-foreign-key-corruption ()
  (gnosis-test-safety
    (gnosis--ensure-db)
    (sqlite-execute gnosis-db "PRAGMA foreign_keys = OFF")
    (sqlite-execute gnosis-db "INSERT INTO extras VALUES (999, NULL, NULL)")
    (sqlite-close gnosis-db)
    (setq gnosis-db nil)
    (should-error (gnosis--ensure-db))
    (should-not gnosis-db)))

(ert-deftest gnosis-db-safety-initialization-nonlocal-exits ()
  "Close candidates, roll back all migration steps, and retry on errors/quit."
  (dolist (fault '(error quit))
    (dolist (stage '(gnosis-db--migrate-v11 gnosis-db--migrate-v12 gnosis-db--check-schema))
      (gnosis-test-safety
        (gnosis-test-safety-v9)
        (let* ((file (expand-file-name "gnosis.db" gnosis-dir))
               (before (gnosis-test-safety-snapshot file))
               (open (symbol-function 'gnosis-sqlite-open))
               (original (symbol-function stage))
               handles)
          (cl-letf (((symbol-function 'gnosis-sqlite-open)
                     (lambda (path) (let ((db (funcall open path))) (push db handles) db)))
                    ((symbol-function stage)
                     (lambda (&rest args)
                       ;; Postflight fault proves all migrations roll back to v9.
                       (if (or (null args) (= (cadr args) 12))
                           (signal fault '("Injected initialization fault"))
                         (apply original args)))))
            (dotimes (_ 2)
              (should (eq fault (condition-case err
                                   (progn (gnosis--ensure-db) 'accepted)
                                 (error (car err)) (quit 'quit))))
              (should-not gnosis-db)
              (should-error (sqlite-select (car handles) "SELECT 1"))
              (should (equal before (gnosis-test-safety-snapshot file)))))
          (should (= 2 (length handles)))
          (should (gnosis--ensure-db))
          (should (= 12 (gnosis--db-version))))))))

(ert-deftest gnosis-db-safety-fresh-creation-quit-retries ()
  (gnosis-test-safety
    (let (candidate)
      (cl-letf (((symbol-function 'gnosis--db-set-version)
                 (lambda (&rest _) (setq candidate gnosis-db) (signal 'quit nil))))
        (should (eq 'quit (condition-case nil (gnosis--ensure-db) (quit 'quit)))))
      (should-not gnosis-db)
      (should-error (sqlite-select candidate "SELECT 1"))
      (should (gnosis--ensure-db))
      (should (= 12 (gnosis--db-version))))))

(ert-deftest gnosis-db-safety-connection-setup-cleanup ()
  (dolist (fault '(error quit))
    (gnosis-test-safety
      (let ((open (symbol-function 'sqlite-open)) candidate)
        (cl-letf (((symbol-function 'sqlite-open)
                   (lambda (&rest args) (setq candidate (apply open args))))
                  ((symbol-function 'sqlite-execute)
                   (lambda (&rest _) (signal fault '("Injected connection setup fault")))))
          (should (eq fault (condition-case err
                               (gnosis--ensure-db)
                             (error (car err)) (quit 'quit)))))
        (should-not gnosis-db)
        (should-error (sqlite-select candidate "SELECT 1"))))))

(ert-deftest gnosis-db-safety-backup-before-upgrade-and-restore ()
  "Preserve the historical source DB and all facts through backup and upgrade."
  (dolist (version '(9 10))
    (gnosis-test-safety
      (gnosis-test-safety-v9)
      (when (= version 10)
        (let ((gnosis-db (gnosis-sqlite-open (expand-file-name "gnosis.db" gnosis-dir))))
          (unwind-protect
              (progn
                (gnosis-db--migrate-v10)
                (gnosis--insert-into 'practice-events '(["exposure" 1 "session" 1 1000 1]))
                (gnosis--insert-into 'study-session '([1 (:session-id "session" :mode practice)])))
            (sqlite-close gnosis-db))))
      (let* ((file (expand-file-name "gnosis.db" gnosis-dir))
             (backup (expand-file-name "before.db" gnosis-dir))
             (before (gnosis-test-safety-snapshot file)))
        (cl-letf (((symbol-function 'gnosis--ensure-db)
                   (lambda () (error "Backup must never initialize"))))
          (gnosis-backup-db backup))
        (should-not gnosis-db)
        (should (equal before (gnosis-test-safety-snapshot file)))
        (should (equal before (gnosis-test-safety-snapshot backup)))
        (gnosis--ensure-db)
        (should (= 12 (gnosis--db-version)))
        (dolist (table (nth 2 before))
          (should (equal (if (equal (car table) "themata")
                             (mapcar (lambda (row) (append row '(nil))) (cdr table))
                           (cdr table))
                         (sqlite-select gnosis-db (format "SELECT * FROM %s ORDER BY 1" (car table))))))
        (when (= version 10)
          (should (equal '(:session-id "session" :mode practice)
                         (gnosis-get 'data 'study-history '(= session-id "session")))))
        (sqlite-close gnosis-db)
        (setq gnosis-db nil)
        ;; Restore only with the database closed; no new-code open of rollback.
        (copy-file backup file t)
        (should (equal before (gnosis-test-safety-snapshot file)))))))

(ert-deftest gnosis-db-safety-backup-unknown-version-and-wal ()
  "Backup does not depend on supported schema or discard committed WAL data."
  (gnosis-test-safety
    (let* ((file (expand-file-name "gnosis.db" gnosis-dir))
           (backup (expand-file-name "before.db" gnosis-dir))
           (db (sqlite-open file)))
      (unwind-protect
          (progn
            (sqlite-execute db "PRAGMA journal_mode = WAL")
            (sqlite-execute db "PRAGMA wal_autocheckpoint = 0")
            (sqlite-execute db "CREATE TABLE retained (fact TEXT)")
            (sqlite-execute db "INSERT INTO retained VALUES ('latest committed fact')")
            (sqlite-execute db "PRAGMA user_version = 99")
            (let ((before (gnosis-test-safety-snapshot file)))
              (gnosis-backup-db backup)
              (should-not gnosis-db)
              (should (equal before (gnosis-test-safety-snapshot file)))
              (should (equal before (gnosis-test-safety-snapshot backup))))
            (should-error (gnosis-backup-db backup)))
        (sqlite-close db)))))

(ert-deftest gnosis-db-safety-backup-missing-and-interrupted ()
  (gnosis-test-safety
    (let ((file (expand-file-name "gnosis.db" gnosis-dir))
          (backup (expand-file-name "before.db" gnosis-dir)))
      (should-error (gnosis-backup-db backup))
      (should-not (file-exists-p file))
      (gnosis-test-safety-v9)
      (dolist (fault '(error quit))
        (let ((open (symbol-function 'sqlite-open)) candidate)
          (cl-letf (((symbol-function 'sqlite-open)
                     (lambda (&rest args) (setq candidate (apply open args))))
                    ((symbol-function 'sqlite-execute)
                     (lambda (&rest _) (signal fault '("Injected backup fault")))))
            (should (eq fault (condition-case err
                                 (gnosis-backup-db backup)
                               (error (car err)) (quit 'quit)))))
          (should-not gnosis-db)
          (should-error (sqlite-select candidate "SELECT 1")))))))

(defun gnosis-test-safety-retained-v9 ()
  "Create synthetic v9 data with the observed retained content layout.
Only schema metadata, never learner rows, was used to build this fixture.
The extra archive column and composite tag key are observed variants, not
attributed to a historical source commit."
  (gnosis-test-safety-v9)
  (let ((db (sqlite-open (expand-file-name "gnosis.db" gnosis-dir))))
    (unwind-protect
        (progn
          (sqlite-execute db "ALTER TABLE themata ADD COLUMN archived_at_us INTEGER
            CHECK (archived_at_us IS NULL OR
                   (typeof(archived_at_us) = 'integer' AND archived_at_us > 0))")
          (sqlite-execute db "UPDATE themata SET archived_at_us = 1000 WHERE id = 1")
          (sqlite-execute db "CREATE TABLE retained_tags (
            thema_id INTEGER NOT NULL, tag TEXT NOT NULL,
            PRIMARY KEY (thema_id, tag),
            FOREIGN KEY (thema_id) REFERENCES themata (id) ON DELETE CASCADE)")
          (sqlite-execute db "INSERT INTO retained_tags SELECT * FROM thema_tag")
          (sqlite-execute db "DROP TABLE thema_tag")
          (sqlite-execute db "ALTER TABLE retained_tags RENAME TO thema_tag")
          (sqlite-execute db "CREATE TABLE retained_links (source TEXT, dest TEXT,
            FOREIGN KEY (source) REFERENCES themata (id) ON DELETE CASCADE,
            UNIQUE (source, dest))")
          (sqlite-execute db "INSERT INTO retained_links SELECT * FROM thema_links")
          (sqlite-execute db "DROP TABLE thema_links")
          (sqlite-execute db "ALTER TABLE retained_links RENAME TO thema_links")
          ;; SQLite preserves case for non-keyword declared types.
          (sqlite-execute db "CREATE TABLE retained_extras (
            id INTEGER PRIMARY KEY NOT NULL, parathema string, review_image string,
            FOREIGN KEY (id) REFERENCES themata (id) ON DELETE CASCADE)")
          (sqlite-execute db "INSERT INTO retained_extras SELECT * FROM extras")
          (sqlite-execute db "DROP TABLE extras")
          (sqlite-execute db "ALTER TABLE retained_extras RENAME TO extras"))
      (sqlite-close db))))

(ert-deftest gnosis-db-safety-retained-layout-upgrade-and-authoring ()
  "Preserve retained rows, then create/edit/practice with an absent hint."
  (gnosis-test-safety
    (gnosis-test-safety-retained-v9)
    (let ((before (gnosis-test-safety-snapshot
                   (expand-file-name "gnosis.db" gnosis-dir))))
      (gnosis--ensure-db)
      (should (= 12 (gnosis--db-version)))
      (dolist (table (nth 2 before))
        (should (equal (if (equal (car table) "themata")
                           (mapcar (lambda (row) (append row '(nil))) (cdr table))
                         (cdr table))
                       (sqlite-select gnosis-db
                        (format "SELECT * FROM %s ORDER BY 1" (car table))))))
      (gnosis-add-thema-fields "basic" "Synthetic question" nil '("answer")
                               "" '("needs_work") 0 '("source-id") nil 2)
      (should-not (gnosis-get 'hypothesis 'themata '(= id 2)))
      (gnosis-update-thema 2 "Edited question" '("hint") '("answer")
                           "" '("needs_work") '("source-id"))
      (should (equal '("hint") (gnosis-get 'hypothesis 'themata '(= id 2))))
      (gnosis-update-thema 2 "Edited question" nil '("answer")
                           "" '("needs_work") '("source-id"))
      (should-not (gnosis-get 'hypothesis 'themata '(= id 2)))
      (should (equal '((nil)) (sqlite-select gnosis-db
                               "SELECT archived_at_us FROM themata WHERE id = 2")))
      (let ((scheduler (gnosis-select '* 'scheduler-state)))
        (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                   (lambda (&rest _) "answer"))
                  ((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
          (gnosis-review-loop '(2) 'practice))
        (should (= 1 (length (gnosis-select '* 'practice-events))))
        (should (equal scheduler (gnosis-select '* 'scheduler-state))))
      (should (equal '((1000)) (sqlite-select gnosis-db
                                "SELECT archived_at_us FROM themata WHERE id = 1")))
      (sqlite-close gnosis-db)
      (setq gnosis-db nil)
      (should (gnosis--ensure-db)))))

(ert-deftest gnosis-db-safety-retained-layout-rejects-malformed ()
  "Do not turn the retained layouts into arbitrary column/key tolerance."
  (dolist (sql '("ALTER TABLE themata RENAME COLUMN archived_at_us TO unknown"
                 "ALTER TABLE themata ADD COLUMN unknown TEXT"
                 "ALTER TABLE thema_tag RENAME COLUMN tag TO unknown"
                 "DROP TABLE thema_tag;
                  CREATE TABLE thema_tag (thema_id INTEGER NOT NULL, tag TEXT NOT NULL,
                                          PRIMARY KEY (tag, thema_id))"
                 "DROP TABLE thema_tag;
                  CREATE TABLE thema_tag (thema_id INTEGER PRIMARY KEY NOT NULL, tag TEXT NOT NULL)"
                 "ALTER TABLE themata DROP COLUMN archived_at_us;
                  ALTER TABLE themata ADD COLUMN archived_at_us TEXT"
                 "ALTER TABLE themata DROP COLUMN archived_at_us;
                  ALTER TABLE themata ADD COLUMN archived_at_us INTEGER NOT NULL DEFAULT 1"
                 "ALTER TABLE themata DROP COLUMN archived_at_us;
                  ALTER TABLE themata ADD COLUMN archived_at_us INTEGER DEFAULT 1"
                 "DROP TABLE thema_links;
                  CREATE TABLE thema_links (dest TEXT, source TEXT)"))
    (gnosis-test-safety
      (gnosis-test-safety-retained-v9)
      (let* ((file (expand-file-name "gnosis.db" gnosis-dir))
             (db (sqlite-open file)))
        (unwind-protect
            (dolist (statement (split-string sql ";" t)) (sqlite-execute db statement))
          (sqlite-close db))
        (let ((before (gnosis-test-safety-snapshot file)))
          (dotimes (_ 2)
            (should-error (gnosis--ensure-db))
            (should-not gnosis-db)
            (should (equal before (gnosis-test-safety-snapshot file)))))))))

(defun gnosis-test-safety-finish-pull (callback &optional status code)
  "Invoke captured pull CALLBACK with terminal STATUS and exit CODE."
  (cl-letf (((symbol-function 'process-status) (lambda (_) (or status 'exit)))
            ((symbol-function 'process-exit-status) (lambda (_) (or code 0))))
    (funcall callback 'scratch-process
             (if (or (eq status 'signal) (and code (/= code 0)))
                 "exited abnormally\n" "finished\n"))))

(ert-deftest gnosis-db-safety-pull-rejects-schema-before-publication ()
  "A rejected post-pull database must stay unwritable through normal helpers."
  (gnosis-test-safety
    (gnosis--ensure-db)
    (let ((old gnosis-db) callback)
      (cl-letf (((symbol-function 'gnosis--git-cmd)
                 (lambda (_args sentinel) (setq callback sentinel))))
        (gnosis-vc-pull))
      (sqlite-execute old "PRAGMA user_version = 13")
      (gnosis-test-safety-finish-pull callback)
      (should-not gnosis-db)
      (should-error (sqlite-select old "SELECT 1"))
      (let* ((file (expand-file-name "gnosis.db" gnosis-dir))
             (before (gnosis-test-safety-snapshot file)))
        (should-error (gnosis--ensure-db))
        (should-error (gnosis--insert-into 'themata
                       '([99 "basic" "Rejected" ("") ("answer") nil])))
        (should-not gnosis-db)
        (should (equal before (gnosis-test-safety-snapshot file)))))))

(ert-deftest gnosis-db-safety-pull-initialization-nonlocal-exits ()
  "Close rejected candidates after both error and cancellation, then retry."
  (dolist (fault '(error quit))
    (gnosis-test-safety
      (gnosis--ensure-db)
      (let ((open (symbol-function 'gnosis-sqlite-open)) candidate callback)
        (cl-letf (((symbol-function 'gnosis--git-cmd)
                   (lambda (_args sentinel) (setq callback sentinel))))
          (gnosis-vc-pull))
        (cl-letf (((symbol-function 'gnosis-sqlite-open)
                   (lambda (file) (setq candidate (funcall open file))))
                  ((symbol-function 'gnosis-db-init)
                   (lambda () (signal fault '("Injected post-pull fault")))))
          (condition-case nil (gnosis-test-safety-finish-pull callback)
            (quit nil)))
        (should candidate)
        (should-not gnosis-db)
        (should-error (sqlite-select candidate "SELECT 1"))
        (should (gnosis--ensure-db))
        (should (= 12 (gnosis--db-version)))))))

(ert-deftest gnosis-db-safety-pull-rejects-changed-owner ()
  "A late pull must not close a replacement connection or use another directory."
  (dolist (change '(connection directory))
    (gnosis-test-safety
      (gnosis--ensure-db)
      (let* ((old gnosis-db)
             (directory gnosis-dir)
             (other (make-temp-file "gnosis-other-owner-" t))
             callback)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'gnosis--git-cmd)
                         (lambda (_args sentinel) (setq callback sentinel))))
                (gnosis-vc-pull))
              (if (eq change 'connection)
                  (setq gnosis-db (gnosis-sqlite-open
                                   (expand-file-name "gnosis.db" directory)))
                (setq gnosis-dir other))
              (let ((current gnosis-db))
                (gnosis-test-safety-finish-pull callback)
                (should (eq gnosis-db current))
                (should (equal '((1)) (sqlite-select current "SELECT 1")))
                (should-not (file-exists-p (expand-file-name "gnosis.db" other)))))
          (unless (eq old gnosis-db) (sqlite-close old))
          (setq gnosis-dir directory)
          (delete-directory other t))))))

(ert-deftest gnosis-db-safety-pull-supersedes-unopened-owner ()
  "The first of two pulls cannot publish after a newer pull was admitted."
  (gnosis-test-safety
    (let (callbacks)
      (cl-letf (((symbol-function 'gnosis--git-cmd)
                 (lambda (_args sentinel) (push sentinel callbacks))))
        (gnosis-vc-pull)
        (gnosis-vc-pull))
      (gnosis-test-safety-finish-pull (cadr callbacks))
      (should-not gnosis-db)
      (should-not (file-exists-p (expand-file-name "gnosis.db" gnosis-dir)))
      (gnosis-test-safety-finish-pull (car callbacks))
      (should gnosis-db)
      (should (= 12 (gnosis--db-version))))))

(ert-deftest gnosis-db-safety-pull-retains-relative-directory-context ()
  "Successful completion uses the initiating directory, not its current buffer."
  (gnosis-test-safety
    (gnosis--ensure-db)
    (let ((old gnosis-db)
          (default-directory (file-name-as-directory gnosis-dir))
          (gnosis-dir "./")
          callback)
      (cl-letf (((symbol-function 'gnosis--git-cmd)
                 (lambda (_args sentinel) (setq callback sentinel))))
        (gnosis-vc-pull))
      (let ((default-directory temporary-file-directory))
        (gnosis-test-safety-finish-pull callback))
      (should gnosis-db)
      (should-not (eq old gnosis-db))
      (should-error (sqlite-select old "SELECT 1"))
      (should (= 12 (gnosis--db-version))))))

(ert-deftest gnosis-db-safety-printer-roundtrip-reopen ()
  "Nested compiled parameters survive a real database close and reopen."
  (gnosis-test-safety
    (gnosis--ensure-db)
    (let ((answer '("A" "B" "C"))
          (checkpoint '(:session-id "scratch" :queue (1 2 3)
                        :history ((1 (:answer ("A" "B" "C")))))))
      (let ((print-length 2) (print-level 2))
        (gnosis--insert-into 'themata `([1 "basic" "Question" ("") ,answer nil]))
        (gnosis--insert-into 'study-history `(["scratch" ,checkpoint])))
      (sqlite-close gnosis-db)
      (setq gnosis-db nil)
      (should (equal answer (gnosis-get 'answer 'themata '(= id 1))))
      (should (equal checkpoint
                     (gnosis-get 'data 'study-history '(= session-id "scratch")))))))

(ert-deftest gnosis-db-safety-pull-rechecks-publication-owner ()
  "A validated candidate must not displace a replacement admitted during open."
  (gnosis-test-safety
    (gnosis--ensure-db)
    (let ((replacement (gnosis-sqlite-open (expand-file-name "gnosis.db" gnosis-dir)))
          (open (symbol-function 'gnosis-db--open))
          candidate callback)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'gnosis--git-cmd)
                       (lambda (_args sentinel) (setq callback sentinel))))
              (gnosis-vc-pull))
            (cl-letf (((symbol-function 'gnosis-db--open)
                       (lambda (directory)
                         (setq candidate (funcall open directory))
                         (setq gnosis-db replacement)
                         candidate)))
              (gnosis-test-safety-finish-pull callback))
            (should (eq replacement gnosis-db))
            (should (equal '((1)) (sqlite-select replacement "SELECT 1")))
            (should-error (sqlite-select candidate "SELECT 1")))
        (unless (eq replacement gnosis-db) (sqlite-close replacement))))))

(ert-deftest gnosis-db-safety-pull-failure-preserves-connection ()
  "Failed or killed pulls leave the existing connection usable."
  (dolist (status '(exit signal))
    (gnosis-test-safety
      (gnosis--ensure-db)
      (let ((old gnosis-db) callback)
        (cl-letf (((symbol-function 'gnosis--git-cmd)
                   (lambda (_args sentinel) (setq callback sentinel))))
          (gnosis-vc-pull))
        (gnosis-test-safety-finish-pull callback status 1)
        (should (eq old gnosis-db))
        (should (equal '((1)) (sqlite-select old "SELECT 1")))))))

(provide 'gnosis-test-db-safety)
;;; gnosis-test-db-safety.el ends here
