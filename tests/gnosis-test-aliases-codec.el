;;; gnosis-test-aliases-codec.el --- Alias exchange tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'gnosis-export-import)

(ert-deftest gnosis-test-aliases-heading-order ()
  (with-temp-buffer
    (org-mode)
    (insert "* Thema\n:PROPERTIES:\n:GNOSIS_ID: NEW\n:GNOSIS_TYPE: basic\n:END:\n** Answer\n- canonical\n** Accepted aliases\n- --flag\n- α β\n** Keimenon\nQuestion\n** Parathema\nExplanation\n** Hypothesis\n")
    (let ((entry (car (gnosis-export-parse-themata))))
      (should (equal (nth 2 entry) "Question"))
      (should (equal (nth 4 entry) '("canonical")))
      (should (equal (nth 8 entry) '("--flag" "α β"))))))

(ert-deftest gnosis-test-aliases-reject-unknown-duplicate ()
  (dolist (heading '("Answer" "Unknown"))
    (with-temp-buffer
      (org-mode)
      (gnosis-export--insert-thema "NEW" "basic" "Q" nil "A")
      (goto-char (point-max))
      (insert "** " heading "\nwrong\n")
      (should-error (gnosis-export-parse-themata)))))

(ert-deftest gnosis-test-aliases-native-lossless ()
  (dolist (aliases '(nil ("--flag" "- literal" "-" " α β " "[x] literal")))
    (with-temp-buffer
      (org-mode)
      (gnosis-export--insert-thema "NEW" "basic" "Q" nil "canonical"
                                   "Explanation" '("tag") nil aliases)
      (let ((entry (car (gnosis-export-parse-themata))))
        (should (equal aliases (nth 8 entry)))
        (should (equal '("canonical") (nth 4 entry)))
        (let* (saved
               (gnosis-thema-types
                `(("basic" . ,(lambda (&rest args) (setq saved args))))))
          (should-not (gnosis-save-thema entry))
          (should (= 10 (length saved)))
          (should (equal aliases (nth 9 saved))))))))

(ert-deftest gnosis-test-aliases-preview-clear ()
  (let* ((before '(1 "basic" "Q" nil ("A") "" nil ("tag") ("Alias")))
         (after '(1 "basic" "Q" nil ("A") "" nil ("tag") nil))
         (plan (gnosis-import--change-plan (list after) (list before))))
    (should (equal (nth 3 (car (cadr plan)))
                   '(("accepted_aliases" ("Alias") nil))))
    (with-temp-buffer
      (gnosis-import--render-detail 1 "CHANGED" (list before after))
      (should (string-match-p "Accepted aliases:" (buffer-string)))
      (should (string-match-p "Alias" (buffer-string))))))

(require 'gnosis-test-helpers)

(ert-deftest gnosis-test-aliases-save-reopen ()
  (gnosis-test-with-db
    (let ((id (gnosis-test--add-basic-thema "Question" "Canonical" '("tag"))))
      (with-temp-buffer
        (org-mode)
        (gnosis-export--insert-thema (number-to-string id) "basic" "Question"
                                     nil "Canonical" "" '("tag") nil '("Alias"))
        (should-not (gnosis-save-thema (car (gnosis-export-parse-themata)))))
      (gnosis-sqlite-close gnosis-db)
      (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
      (with-temp-buffer
        (org-mode)
        (gnosis-export--insert-themata (list id))
        (should (equal '("Alias") (nth 8 (car (gnosis-export-parse-themata))))))
      (with-temp-buffer
        (org-mode)
        (gnosis-export--insert-thema (number-to-string id) "basic" "Question"
                                     nil "Canonical" "" '("tag"))
        (should-not (gnosis-save-thema (car (gnosis-export-parse-themata)))))
      (should-not (gnosis-get 'accepted-aliases 'themata `(= id ,id)))
      (should (equal '("Canonical") (gnosis-get 'answer 'themata `(= id ,id)))))))

(ert-deftest gnosis-test-aliases-content-drift-and-clear ()
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Question" "Canonical" '("tag")))
           (db (gnosis--ensure-db))
           (file (make-temp-file "gnosis-alias-content-" nil ".db")))
      (unwind-protect
          (progn
            (gnosis-sqlite-execute db "UPDATE themata SET accepted_aliases = ? WHERE id = ?"
                                   (list '("Alias") id))
            (gnosis-export-db file)
            (should (= 5 (gnosis-import--format-version file)))
            (gnosis-sqlite-execute db "UPDATE themata SET accepted_aliases = NULL WHERE id = ?" (list id))
            (let ((diff (gnosis-import--diff file)))
              (should (equal (nth 3 (car (cadr diff)))
                             '(("accepted_aliases" nil ("Alias")))))
              (gnosis-sqlite-execute db "UPDATE themata SET accepted_aliases = ? WHERE id = ?"
                                     (list '("Drift") id))
              (should-error (gnosis-import--apply-changes file nil (list id)
                                                         (nth 2 diff) (nth 3 diff)))
              (gnosis-sqlite-execute db "UPDATE themata SET accepted_aliases = NULL WHERE id = ?" (list id))
              (gnosis-import--apply-changes file nil (list id) (nth 2 diff) (nth 3 diff))
              (should (equal '("Alias") (gnosis-get 'accepted-aliases 'themata `(= id ,id)))))
            ;; Retained file identity must cover alias-only source edits.
            (let ((diff (gnosis-import--diff file))
                  (source (gnosis-sqlite-open file)))
              (unwind-protect
                  (gnosis-sqlite-execute source "UPDATE themata SET accepted_aliases = NULL")
                (sqlite-close source))
              (should-error (gnosis-import--apply-changes file nil (list id) (nth 2 diff))))
            (let ((diff (gnosis-import--diff file)))
              (gnosis-import--apply-changes file nil (list id) (nth 2 diff) (nth 3 diff))
              (should-not (gnosis-get 'accepted-aliases 'themata `(= id ,id)))))
        (delete-file file)))))

(ert-deftest gnosis-test-aliases-content-formats ()
  "Read real portable SQLite formats independently of application migration."
  (dolist (version '(1 2 3))
    (let ((db (gnosis-sqlite-open nil)))
      (unwind-protect
          (progn
            (sqlite-execute db "ATTACH DATABASE ':memory:' AS import_db")
            (sqlite-execute db
                            (concat "CREATE TABLE import_db.themata (id INTEGER, type TEXT, keimenon TEXT, hypothesis TEXT, answer TEXT"
                                    (if (= version 3) ", accepted_aliases TEXT)" ")")))
            (sqlite-execute db "CREATE TABLE import_db.extras (id INTEGER, parathema TEXT, review_image TEXT)")
            (sqlite-execute db "CREATE TABLE import_db.thema_tag (thema_id INTEGER, tag TEXT)")
            (when (> version 1)
              (sqlite-execute db "CREATE TABLE import_db.gnosis_meta (key TEXT PRIMARY KEY, value TEXT)")
              (sqlite-execute db "INSERT INTO import_db.gnosis_meta VALUES ('format_version', ?)"
                              (list (number-to-string version))))
            (gnosis-sqlite-execute db
                                   "INSERT INTO import_db.themata (id, type, keimenon, hypothesis, answer) VALUES (?, ?, ?, ?, ?)"
                                   '(1 "basic" "Q" nil ("A")))
            (when (= version 3)
              (gnosis-sqlite-execute db "UPDATE import_db.themata SET accepted_aliases = ?"
                                     '(("--flag" " α β " "[x] literal"))))
            (should (= version (gnosis-import--format-version-in-db db "import_db")))
            (should (equal (nth 8 (car (gnosis-import--content-rows db "import_db" '(1))))
                           (when (= version 3) '("--flag" " α β " "[x] literal"))))
            (when (= version 3)
              (gnosis-sqlite-execute db "UPDATE import_db.themata SET accepted_aliases = ?" '(("")))
              (should-error (gnosis-import--content-rows db "import_db" '(1)))
              (gnosis-sqlite-execute db "UPDATE import_db.themata SET accepted_aliases = ?, type = ?"
                                     '(("alias") "mcq"))
              (should-error (gnosis-import--content-rows db "import_db" '(1))))
            (gnosis-sqlite-execute db "UPDATE import_db.themata SET type = ?" '("model-name"))
            (should-error (gnosis-import--format-version-in-db db "import_db")))
        (sqlite-close db)))))

(ert-deftest gnosis-test-aliases-legacy-handler ()
  (let* (called
         (gnosis-thema-types
          `(("external" . ,(lambda (_id _type _k _h _a _p _tags _s _links)
                              (setq called t))))))
    (should-not (gnosis-save-thema '("NEW" "external" "Q" nil ("A") "" nil 1)))
    (should called)
    (setq called nil)
    (should (stringp (gnosis-save-thema
                     '("NEW" "external" "Q" nil ("A") "" nil 1 ("Alias")))))
    (should-not called)))

(ert-deftest gnosis-test-aliases-export-failure-preserves-file ()
  (gnosis-test-with-db
    (let ((id (gnosis-test--add-basic-thema "Q" "A"))
          (file (expand-file-name "existing.db" gnosis-dir)))
      (with-temp-file file (insert "Original bytes"))
      (dolist (aliases '(("") ("[[gnosis-image:malformed]]")))
        (gnosis-sqlite-execute gnosis-db
                               "UPDATE themata SET accepted_aliases = ? WHERE id = ?"
                               (list aliases id))
        (should-error (gnosis-export-db file))
        (should (equal "Original bytes"
                       (with-temp-buffer (insert-file-contents file) (buffer-string))))))))

(ert-deftest gnosis-test-aliases-legacy-import-clears ()
  (dolist (version '(1 2))
    (gnosis-test-with-db
      (let* ((id (gnosis-test--add-basic-thema "Q" "A"))
             (file (expand-file-name "legacy.db" gnosis-dir)))
        (gnosis-export-db file)
        (let ((db (gnosis-sqlite-open file)))
          (unwind-protect
              (progn
                (sqlite-execute db "ALTER TABLE themata DROP COLUMN accepted_aliases")
                (sqlite-execute db "UPDATE gnosis_meta SET value = ? WHERE key = 'format_version'"
                                (list (number-to-string version))))
            (sqlite-close db)))
        (gnosis-sqlite-execute gnosis-db "UPDATE themata SET accepted_aliases = ? WHERE id = ?"
                               (list '("Old spelling") id))
        (let ((diff (gnosis-import--diff file)))
          (should (equal (nth 3 (car (cadr diff)))
                         '(("accepted_aliases" ("Old spelling") nil))))
          (gnosis-import--apply-changes file nil (list id) (nth 2 diff) (nth 3 diff))
          (should-not (gnosis-get 'accepted-aliases 'themata `(= id ,id))))))))

(provide 'gnosis-test-aliases-codec)
;;; gnosis-test-aliases-codec.el ends here
