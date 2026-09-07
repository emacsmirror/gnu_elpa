;;; gnosis-test-exchange-hardening.el --- Safe content exchange -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Exercise file replacement and reviewed import boundaries on disposable data.

;;; Code:

(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-export-import)

(defun gnosis-test-exchange--history (db)
  "Return schema and study evidence from DB."
  (mapcar (lambda (sql) (sqlite-select db sql))
          '("PRAGMA user_version"
            "SELECT type, name, sql FROM sqlite_master ORDER BY type, name"
            "SELECT * FROM scheduler_state ORDER BY thema_id"
            "SELECT * FROM review_events ORDER BY event_id")))

(ert-deftest gnosis-exchange-export-rejects-active-file-aliases ()
  "Reject direct, symlink and hardlink destinations without losing history."
  (dolist (kind '(direct symlink hardlink))
    (gnosis-test-with-db
      (let* ((id (gnosis-test--add-basic-thema "Question" "Answer"))
             (file (if (eq kind 'direct) gnosis-test--db-file
                     (expand-file-name "alias.db" gnosis-dir))))
        (gnosis-scheduler-accept-review
         (make-string 64 ?a) id 'success 1000000 (gnosis--today-int))
        (pcase kind
          ('symlink (make-symbolic-link gnosis-test--db-file file))
          ('hardlink (add-name-to-file gnosis-test--db-file file)))
        (let* ((before (gnosis-test-exchange--history gnosis-db))
               (bytes (gnosis-import--file-sha256 gnosis-test--db-file))
               (outcome (condition-case err (gnosis-export-db file)
                          (error err))))
          (should (equal bytes (gnosis-import--file-sha256 gnosis-test--db-file)))
          (let ((reopened (gnosis-sqlite-open gnosis-test--db-file)))
            (unwind-protect
                (should (equal before (gnosis-test-exchange--history reopened)))
              (sqlite-close reopened)))
          (should (eq 'user-error (car-safe outcome)))
          (should (file-equal-p file gnosis-test--db-file))
          (should-not (assoc 2 (sqlite-select gnosis-db "PRAGMA database_list"))))))))

(ert-deftest gnosis-exchange-export-failure-preserves-previous-output ()
  "An error or quit during export leaves the old file and no scratch DB."
  (dolist (failure '(error quit))
    (gnosis-test-with-db
      (gnosis-test--add-basic-thema "Question" "Answer")
      (let* ((file (expand-file-name "previous.db" gnosis-dir))
             (execute (symbol-function 'gnosis-sqlite-execute)))
        (with-temp-file file (insert "Previous export bytes\n"))
        (let ((before (gnosis-import--file-sha256 file))
              (files (directory-files gnosis-dir))
              (injected nil))
          (cl-letf (((symbol-function 'gnosis-sqlite-execute)
                     (lambda (db sql &optional params)
                       (when (equal sql gnosis-export--extras-schema)
                         (setq injected t)
                         (signal failure '("Controlled interrupted export")))
                       (funcall execute db sql params))))
            (should
             (eq failure
                 (condition-case err (gnosis-export-db file)
                   ((error quit) (car err))))))
          (should injected)
          (should (equal before (gnosis-import--file-sha256 file)))
          (should (equal files (directory-files gnosis-dir)))
          (should-not (assoc 2 (sqlite-select gnosis-db "PRAGMA database_list"))))))))

(ert-deftest gnosis-exchange-export-overwrites-with-complete-content ()
  "Successful replacement is a reopenable, complete content-only export."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Question" "Answer" '("tag")))
           (file (expand-file-name "previous.db" gnosis-dir)))
      (with-temp-file file (insert "Previous bytes"))
      (let ((files (directory-files gnosis-dir)))
        (gnosis-export-db file)
        (let ((db (gnosis-sqlite-open file)))
          (unwind-protect
              (progn
                (should (equal '(("ok")) (sqlite-select db "PRAGMA integrity_check")))
                (should-not (sqlite-select db "PRAGMA foreign_key_check"))
                (should (equal (list (list id)) (sqlite-select db "SELECT id FROM themata")))
                (should (equal '(("1")) (sqlite-select db "SELECT value FROM gnosis_meta WHERE key = 'thema_count'")))
                (should (= 2 (gnosis-import--format-version-in-db db "main"))))
            (sqlite-close db)))
        (should (equal files (directory-files gnosis-dir)))))))

(ert-deftest gnosis-exchange-import-image-diff-apply-and-detail ()
  "An image-only update appears in the preview, detail and applied content."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Question" "Answer"))
           (file (expand-file-name "source.db" gnosis-dir)))
      (gnosis-sqlite-execute gnosis-db "UPDATE extras SET review_image = ?" '("old.png"))
      (gnosis-export-db file)
      (let ((db (gnosis-sqlite-open file)))
        (unwind-protect
            (gnosis-sqlite-execute db "UPDATE extras SET review_image = ?" '("new.png"))
          (sqlite-close db)))
      (let* ((diff (gnosis-import--diff file))
             (row (car (cadr diff))))
        (should (equal (list id "basic" "Question" '(("review_image" "old.png" "new.png"))) row))
        (with-temp-buffer
          (gnosis-import--render-detail
           id "CHANGED" (cdr (assoc id (nth 4 diff))))
          (should (string-search "old.png" (buffer-string)))
          (should (string-search "new.png" (buffer-string))))
        (gnosis-import--apply-changes file nil (list id) (nth 2 diff))
        (should (equal "new.png" (gnosis-get 'review-image 'extras `(= id ,id))))))))

(defun gnosis-test-exchange--preview (file)
  "Display the real review buffer for FILE without changing windows."
  (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
    (gnosis-import-db file))
  (get-buffer "*Gnosis Import*"))

(defun gnosis-test-exchange--apply (buffer)
  "Accept the reviewed import in BUFFER without changing windows."
  (with-current-buffer buffer
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'quit-window) #'ignore))
      (gnosis-import-apply))))

(ert-deftest gnosis-exchange-import-stale-local-edit-is-atomic ()
  "Reject stale reviewed content before inserting any of the new themata."
  (dolist (field '(keimenon tags image deleted))
    (gnosis-test-with-db
      (let* ((id (gnosis-test--add-basic-thema "Reviewed" "Answer"))
             (new (gnosis-test--add-basic-thema "New" "Answer"))
             (file (expand-file-name "source.db" gnosis-dir))
             buffer)
        (unwind-protect
            (progn
              (gnosis-export-db file)
              (gnosis-sqlite-execute gnosis-db "DELETE FROM themata WHERE id = ?" (list new))
              (gnosis-sqlite-execute gnosis-db "UPDATE themata SET keimenon = ? WHERE id = ?" (list "Before review" id))
              (setq buffer (gnosis-test-exchange--preview file))
              (pcase field
                ('keimenon (gnosis-sqlite-execute gnosis-db "UPDATE themata SET keimenon = ? WHERE id = ?" (list "New local edit" id)))
                ('tags (gnosis-sqlite-execute gnosis-db "INSERT INTO thema_tag VALUES (?, ?)" (list id "local-tag")))
                ('image (gnosis-sqlite-execute gnosis-db "UPDATE extras SET review_image = ? WHERE id = ?" (list "local.png" id)))
                ('deleted (gnosis-sqlite-execute gnosis-db "DELETE FROM themata WHERE id = ?" (list id))))
              (let ((before (mapcar (lambda (table) (sqlite-select gnosis-db (format "SELECT * FROM %s" table)))
                                    '(themata extras thema_tag scheduler_state))))
                (should-error (gnosis-test-exchange--apply buffer) :type 'user-error)
                (should (equal before
                               (mapcar (lambda (table) (sqlite-select gnosis-db (format "SELECT * FROM %s" table)))
                                       '(themata extras thema_tag scheduler_state)))))
              (should-not (assoc 2 (sqlite-select gnosis-db "PRAGMA database_list"))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest gnosis-exchange-import-unrelated-edit-and-review-are-allowed ()
  "Only overwritten content is pinned; other edits and study can proceed."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Imported" "Answer" '("shared")))
           (other (gnosis-test--add-basic-thema "Other" "Answer" '("private")))
           (file (expand-file-name "source.db" gnosis-dir))
           buffer)
      (unwind-protect
          (progn
            (gnosis-export-db file '("shared"))
            (gnosis-sqlite-execute gnosis-db "UPDATE themata SET keimenon = ? WHERE id = ?" (list "Local" id))
            (setq buffer (gnosis-test-exchange--preview file))
            (gnosis-sqlite-execute gnosis-db "UPDATE themata SET keimenon = ? WHERE id = ?" (list "Other edited" other))
            (gnosis-scheduler-accept-review (make-string 64 ?b) id 'success 1000000 (gnosis--today-int))
            (let ((history (gnosis-test-exchange--history gnosis-db)))
              (gnosis-test-exchange--apply buffer)
              (should (equal history (gnosis-test-exchange--history gnosis-db))))
            (should (equal "Imported" (gnosis-get 'keimenon 'themata `(= id ,id))))
            (should (equal "Other edited" (gnosis-get 'keimenon 'themata `(= id ,other)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest gnosis-exchange-import-rejects-different-destination ()
  "An existing preview cannot apply to a newly opened database owner."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Imported" "Answer"))
           (file (expand-file-name "source.db" gnosis-dir))
           buffer)
      (unwind-protect
          (progn
            (gnosis-export-db file)
            (gnosis-sqlite-execute gnosis-db "UPDATE themata SET keimenon = ?" '("Local"))
            (setq buffer (gnosis-test-exchange--preview file))
            (gnosis-test-with-db
              (gnosis-test--add-basic-thema "Local" "Answer" nil nil id)
              (should-error (gnosis-test-exchange--apply buffer) :type 'user-error)
              (should (equal "Local" (gnosis-get 'keimenon 'themata `(= id ,id))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest gnosis-exchange-import-restores-missing-extras ()
  "Reviewed image and parathema values survive a missing local extras row."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Question" "Answer" nil "Passage"))
           (file (expand-file-name "source.db" gnosis-dir)))
      (gnosis-sqlite-execute gnosis-db "UPDATE extras SET review_image = ?" '("image.png"))
      (gnosis-export-db file)
      (sqlite-execute gnosis-db "DELETE FROM extras")
      (let ((diff (gnosis-import--diff file)))
        (should (equal '("parathema" "review_image") (mapcar #'car (nth 3 (car (cadr diff))))))
        (gnosis-import--apply-changes file nil (list id) (nth 2 diff) (nth 3 diff))
        (should (equal '(("Passage" "image.png"))
                       (gnosis-sqlite-select gnosis-db "SELECT parathema, review_image FROM extras")))))))

(ert-deftest gnosis-exchange-import-preserves-null-versus-empty-extras ()
  "Import distinguishes NULL from empty optional content in both directions."
  (dolist (pair '((nil "") ("" nil)))
    (gnosis-test-with-db
      (let* ((id (gnosis-test--add-basic-thema "Question" "Answer"))
             (file (expand-file-name "source.db" gnosis-dir)))
        ;; Raw empty SQL text is a supported legacy representation too.
        (sqlite-execute gnosis-db "UPDATE extras SET parathema = ?, review_image = ?"
                        (list (car pair) (car pair)))
        (gnosis-export-db file)
        (sqlite-execute gnosis-db "UPDATE extras SET parathema = ?, review_image = ?"
                        (list (cadr pair) (cadr pair)))
        (let ((diff (gnosis-import--diff file)))
          (should (equal '("parathema" "review_image") (mapcar #'car (nth 3 (car (cadr diff))))))
          (gnosis-import--apply-changes file nil (list id) (nth 2 diff) (nth 3 diff))
          (should (equal (list (list (car pair) (car pair)))
                         (sqlite-select gnosis-db "SELECT parathema, review_image FROM extras"))))))))

(ert-deftest gnosis-exchange-export-invalid-completion-preserves-output ()
  "Validate the completed sibling before replacing a prior export."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Question" "Answer")
    (let ((file (expand-file-name "previous.db" gnosis-dir))
          (execute (symbol-function 'sqlite-execute)))
      (with-temp-file file (insert "Previous bytes"))
      (let ((before (gnosis-import--file-sha256 file))
            (files (directory-files gnosis-dir))
            injected)
        (cl-letf (((symbol-function 'sqlite-execute)
                   (lambda (db sql &optional params)
                     (if (equal (car params) "thema_count")
                         (progn (setq injected t)
                                (funcall execute db sql '("thema_count" "999")))
                       (funcall execute db sql params)))))
          (should-error (gnosis-export-db file)))
        (should injected)
        (should (equal before (gnosis-import--file-sha256 file)))
        (should (equal files (directory-files gnosis-dir)))
        (should-not (assoc 2 (sqlite-select gnosis-db "PRAGMA database_list")))))))

(ert-deftest gnosis-exchange-import-checks-after-write-lock-acquisition ()
  "Catch content changed after file validation but before the write lock."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Imported" "Answer"))
           (file (expand-file-name "source.db" gnosis-dir))
           (execute (symbol-function 'sqlite-execute))
           buffer)
      (unwind-protect
          (progn
            (gnosis-export-db file)
            (gnosis-sqlite-execute gnosis-db "UPDATE themata SET keimenon = ?" '("Reviewed local"))
            (setq buffer (gnosis-test-exchange--preview file))
            (let (injected)
              (cl-letf (((symbol-function 'sqlite-execute)
                         (lambda (db sql &optional params)
                           (when (and (equal sql "BEGIN IMMEDIATE") (not injected))
                             (setq injected t)
                             (funcall execute db "UPDATE themata SET keimenon = ?"
                                      '("\"Last local edit\"")))
                           (funcall execute db sql params))))
                (should-error (gnosis-test-exchange--apply buffer) :type 'user-error))
              (should injected)
              (should (equal "Last local edit" (gnosis-get 'keimenon 'themata `(= id ,id))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest gnosis-exchange-change-plan-keeps-named-old-new-values ()
  "A plan retains changed content as data, not a display string."
  (let ((old '((1 "basic" "Question" ("") ("Answer") nil "old.png" ("a"))))
        (new '((1 "basic" "Question" ("") ("Answer") "" "new.png" ("a" "b")))))
    (should
     (equal
      '(nil ((1 "basic" "Question"
                (("parathema" nil "")
                 ("review_image" "old.png" "new.png")
                 ("tags" ("a") ("a" "b"))))))
      (gnosis-import--change-plan new old)))
    (should (equal '((1 "basic" "Question" ("") ("Answer") nil "old.png" ("a"))) old))))

(ert-deftest gnosis-exchange-detail-distinguishes-absent-and-empty ()
  "Render an absent value differently from an explicitly empty value."
  (with-temp-buffer
    (gnosis-import--insert-field "Parathema" nil "")
    (should (string-search "- nil" (buffer-string)))
    (should (string-search "+ \"\"" (buffer-string)))))

(ert-deftest gnosis-exchange-export-rejects-active-companions ()
  "Preserve active SQLite companions and their direct and aliased names."
  (dolist (suffix '("-wal" "-shm" "-journal"))
    (dolist (kind '(direct symlink hardlink))
      (gnosis-test-with-db
        (sqlite-execute gnosis-db
                        (if (equal suffix "-journal")
                            "PRAGMA journal_mode=PERSIST"
                          "PRAGMA journal_mode=WAL"))
        (gnosis-test--add-basic-thema "Retained content" "Answer")
        (let* ((companion (concat gnosis-test--db-file suffix))
               (file (if (eq kind 'direct) companion
                       (expand-file-name "alias.db" gnosis-dir)))
               (other (sqlite-open gnosis-test--db-file)))
          (unwind-protect
              (progn
                (should (file-exists-p companion))
                (pcase kind
                  ('symlink (make-symbolic-link companion file))
                  ('hardlink (add-name-to-file companion file)))
                (let ((before (sqlite-select gnosis-db "SELECT * FROM themata"))
                      (bytes (gnosis-import--file-sha256 companion)))
                  (should (equal before (sqlite-select other "SELECT * FROM themata")))
                  (should-error (gnosis-export-db file) :type 'user-error)
                  (should (equal bytes (gnosis-import--file-sha256 companion)))
                  (should (file-equal-p companion file))
                  (should (equal before (sqlite-select gnosis-db "SELECT * FROM themata")))
                  (should (equal before (sqlite-select other "SELECT * FROM themata")))))
            (sqlite-close other)))))))

(ert-deftest gnosis-exchange-export-rejects-absent-active-companions ()
  "Reserve active companion names even before SQLite creates those files."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Question" "Answer")
    (dolist (suffix '("-wal" "-shm" "-journal"))
      (let ((file (concat gnosis-test--db-file suffix))
            (alias (expand-file-name "dangling-alias" gnosis-dir)))
        (should-not (file-exists-p file))
        (make-symbolic-link file alias)
        (unwind-protect
            (progn
              (should-error (gnosis-export-db file) :type 'user-error)
              (should-error (gnosis-export-db alias) :type 'user-error)
              (should-not (file-exists-p file))
              (should (equal file (file-symlink-p alias))))
          (delete-file alias))))))

(ert-deftest gnosis-exchange-export-preserves-abrupt-writer-wal ()
  "Refuse replacement of an export with committed, uncheckpointed WAL data."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Previous export" "Answer")
    (let ((file (expand-file-name "previous.db" gnosis-dir)))
      (gnosis-export-db file)
      ;; Kill a disposable native SQLite writer without closing its connection.
      (should
       (stringp
        (call-process
         (expand-file-name invocation-name invocation-directory)
         nil nil nil "-Q" "--batch" "--eval"
         (prin1-to-string
          `(let ((db (sqlite-open ,file)))
             (sqlite-execute db "PRAGMA journal_mode=WAL")
             (sqlite-execute db "PRAGMA wal_checkpoint(TRUNCATE)")
             (sqlite-execute db "UPDATE themata SET keimenon = ?"
                             '("\"Uncheckpointed content\""))
             (signal-process (emacs-pid) 'SIGKILL))))))
      (should (file-exists-p (concat file "-wal")))
      (gnosis-sqlite-execute gnosis-db "UPDATE themata SET keimenon = ?"
                            '("New exported content"))
      (let* ((files (directory-files gnosis-dir t directory-files-no-dot-files-regexp))
             (before (mapcar #'gnosis-import--file-sha256 files)))
        (should-error (gnosis-export-db file) :type 'user-error)
        (should (equal before (mapcar #'gnosis-import--file-sha256 files)))
        (should (equal files (directory-files gnosis-dir t directory-files-no-dot-files-regexp))))
      (let ((db (gnosis-sqlite-open file)))
        (unwind-protect
            (should (equal '(("Uncheckpointed content"))
                           (gnosis-sqlite-select db "SELECT keimenon FROM themata")))
          (sqlite-close db))))))

(ert-deftest gnosis-exchange-export-rechecks-destination-companions ()
  "Reject companions present initially or appearing just before replacement."
  (dolist (suffix '("-wal" "-shm" "-journal"))
    (dolist (late '(nil t))
      (gnosis-test-with-db
        (gnosis-test--add-basic-thema "Question" "Answer")
        (let* ((file (expand-file-name "previous.db" gnosis-dir))
               (companion (concat file suffix))
               (validate (symbol-function 'gnosis-export--validate)))
          (with-temp-file file (insert "Previous export bytes"))
          ;; A dangling companion is still owned by somebody else.
          (unless late (make-symbolic-link "absent-companion" companion))
          (let ((before (gnosis-import--file-sha256 file))
                validated)
            (cl-letf (((symbol-function 'gnosis-export--validate)
                       (lambda (scratch count)
                         (funcall validate scratch count)
                         (setq validated t)
                         (when late
                           (make-symbolic-link "absent-companion" companion)))))
              (should-error (gnosis-export-db file) :type 'user-error))
            (should (eq late validated))
            (should (equal before (gnosis-import--file-sha256 file)))
            (should (equal "absent-companion" (file-symlink-p companion)))
            (should-not (directory-files gnosis-dir nil "\\`\\.gnosis-export-"))
            (should-not (assoc 2 (sqlite-select gnosis-db "PRAGMA database_list")))))))))

(defun gnosis-test-exchange--detail (buffer id)
  "Return detail text for ID using the public RET binding in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (while (and (not (eobp)) (not (equal id (tabulated-list-get-id))))
      (forward-line 1))
    (should (equal id (tabulated-list-get-id)))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (call-interactively (key-binding (kbd "RET")))))
  (with-current-buffer "*Gnosis Import Detail*" (buffer-string)))

(ert-deftest gnosis-exchange-import-detail-keeps-source-snapshot ()
  "RET keeps reviewed values across source A-to-B-to-A before applying A."
  (dolist (new-p '(nil t))
    (gnosis-test-with-db
      (let* ((id (gnosis-test--add-basic-thema "Incoming" "Original answer"))
             (file (expand-file-name "source.db" gnosis-dir))
             (original (expand-file-name "original.db" gnosis-dir))
             buffer)
        (unwind-protect
            (progn
              (gnosis-export-db file)
              (copy-file file original)
              (if new-p
                  (gnosis-sqlite-execute gnosis-db "DELETE FROM themata")
                (gnosis-sqlite-execute gnosis-db "UPDATE themata SET answer = ?"
                                      '(("Local answer"))))
              (setq buffer (gnosis-test-exchange--preview file))
              (let ((db (gnosis-sqlite-open file)))
                (unwind-protect
                    (gnosis-sqlite-execute db "UPDATE themata SET answer = ?"
                                          '(("Unreviewed replacement")))
                  (sqlite-close db)))
              (let ((detail (gnosis-test-exchange--detail buffer id)))
                (should (string-search "Original answer" detail))
                (should-not (string-search "Unreviewed replacement" detail))
                (unless new-p (should (string-search "Local answer" detail)))
                (copy-file original file t)
                (with-current-buffer buffer
                  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                            ((symbol-function 'quit-window) #'ignore))
                    (call-interactively (key-binding (kbd "a")))))
                (should (equal '("Original answer")
                               (gnosis-get 'answer 'themata `(= id ,id))))
                (should (equal detail (with-current-buffer "*Gnosis Import Detail*"
                                        (buffer-string))))))
          (when (buffer-live-p buffer) (kill-buffer buffer))
          (when (get-buffer "*Gnosis Import Detail*")
            (kill-buffer "*Gnosis Import Detail*")))))))

(ert-deftest gnosis-exchange-import-detail-keeps-destination-snapshot ()
  "RET keeps original old values after a local edit or database-owner swap."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Incoming" "Incoming answer"))
           (file (expand-file-name "source.db" gnosis-dir))
           buffer)
      (unwind-protect
          (progn
            (gnosis-export-db file)
            (gnosis-sqlite-execute gnosis-db "UPDATE themata SET answer = ?"
                                  '(("Reviewed local answer")))
            (setq buffer (gnosis-test-exchange--preview file))
            (gnosis-sqlite-execute gnosis-db "UPDATE themata SET answer = ?"
                                  '(("Later local answer")))
            (let ((detail (gnosis-test-exchange--detail buffer id)))
              (should (string-search "Reviewed local answer" detail))
              (should-not (string-search "Later local answer" detail))
              (gnosis-test-with-db
                (gnosis-test--add-basic-thema "Other owner" "Other answer" nil nil id)
                (should (equal detail (gnosis-test-exchange--detail buffer id)))
                (should-error (gnosis-test-exchange--apply buffer) :type 'user-error)
                (should (equal '("Other answer") (gnosis-get 'answer 'themata `(= id ,id)))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (when (get-buffer "*Gnosis Import Detail*")
          (kill-buffer "*Gnosis Import Detail*"))))))

(provide 'gnosis-test-exchange-hardening)
;;; gnosis-test-exchange-hardening.el ends here
