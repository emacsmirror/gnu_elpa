;;; gnosis-test-schema-v8.el --- Released 0.10.6 fixture -*- lexical-binding: t; -*-
;; Schema copied verbatim from tag 0.10.6:lisp/gnosis-db.el.
(require 'gnosis-db)
(defconst gnosis-test--v8-schemata
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

(defun gnosis-test--create-v8-indexes (db)
  "Create the indexes declared by released 0.10.6 on DB."
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
  (gnosis-db--migrate-step "create source_guid index"
			   (gnosis-sqlite-execute db
						  "CREATE INDEX IF NOT EXISTS idx_themata_source_guid ON themata(source_guid)")))

(defun gnosis-test--create-v8-schema ()
  "Create all tables declared by released Gnosis 0.10.6, at schema 8."
  (pcase-dolist (`(,table ,schema) gnosis-test--v8-schemata)
    (gnosis-sqlite-execute gnosis-db
      (format "CREATE TABLE %s (%s)" (gnosis-sqlite--ident table)
              (gnosis-sqlite--compile-schema schema))))
  (gnosis-test--create-v8-indexes gnosis-db)
  (gnosis--db-set-version 8))

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
(provide 'gnosis-test-schema-v8)
