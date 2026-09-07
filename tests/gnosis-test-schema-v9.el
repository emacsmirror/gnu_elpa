;;; gnosis-test-schema-v9.el --- Frozen v9 schema fixture -*- lexical-binding: t; -*-
;; Schema from f0d7301; do not derive historical fixtures from current tables.
(require 'gnosis-db)
(defconst gnosis-test--v9-schemata
  '((themata
     ([(id integer :primary-key)
       (type text :not-null)
       (keimenon text :not-null)
       (hypothesis text :not-null)
       (answer text :not-null)
       (source-guid text)]))
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


(defun gnosis-test--create-v9-schema (&optional content-only)
  "Create the historical v9 schema, or CONTENT-ONLY tables for v8 fixtures."
  (dolist (spec gnosis-test--v9-schemata)
    (unless (and content-only (memq (car spec) '(scheduler-config scheduler-baseline
                          scheduler-state review-events review-activity-baseline)))
      (gnosis-sqlite-execute gnosis-db
        (format "CREATE TABLE %s (%s)" (gnosis-sqlite--ident (car spec))
                (gnosis-sqlite--compile-schema (cadr spec))))))
  (unless content-only
    (gnosis-db--install-default-scheduler-config gnosis-db)
    (gnosis-db--create-scheduler-guards gnosis-db)
    (gnosis--db-create-indexes gnosis-db)
    (gnosis--db-set-version 9)))
(defmacro gnosis-test-with-old-db (&rest body)
  "Run BODY in an empty isolated database without current initialization."
  (declare (indent 0) (debug t))
  `(let* ((gnosis-test--dir (make-temp-file "gnosis-old-" t))
          (gnosis-dir (file-name-as-directory gnosis-test--dir))
          (gnosis-test--db-file (expand-file-name "gnosis.db" gnosis-dir))
          (gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (gnosis-testing t) (gnosis-vc-auto-push nil))
     (unwind-protect (progn ,@body)
       (gnosis-sqlite-close gnosis-db)
       (delete-directory gnosis-test--dir t))))
(provide 'gnosis-test-schema-v9)
