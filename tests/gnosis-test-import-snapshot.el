;;; gnosis-test-import-snapshot.el --- Import file-set safety -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Refuse incomplete SQLite sources without recovering another owner's files.

;;; Code:

(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-export-import)

(defun gnosis-test-import-snapshot--source ()
  "Return a closed WAL-mode export with a changed local counterpart."
  (gnosis-test--add-basic-thema "Exported" "Answer" nil nil 101)
  (let ((file (expand-file-name "source.db" gnosis-dir)))
    (gnosis-export-db file)
    (let ((db (sqlite-open file)))
      (unwind-protect
          (sqlite-execute db "PRAGMA journal_mode=WAL")
        (sqlite-close db)))
    (gnosis-sqlite-execute gnosis-db "UPDATE themata SET keimenon = ?"
                          '("Local"))
    file))

(defun gnosis-test-import-snapshot--wal-update (db)
  "Commit a changed question on DB without checkpointing it."
  (sqlite-execute db "PRAGMA wal_autocheckpoint=0")
  (gnosis-sqlite-execute db "UPDATE themata SET keimenon = ?"
                        '("Committed in WAL")))

(defun gnosis-test-import-snapshot--files ()
  "Return exact file bytes and symlink targets in the fixture directory."
  (mapcar (lambda (file)
            (list (file-name-nondirectory file)
                  (file-symlink-p file)
                  (and (file-exists-p file)
                       (gnosis-import--file-sha256 file))))
          (directory-files gnosis-dir t directory-files-no-dot-files-regexp)))

(defun gnosis-test-import-snapshot--apply (file diff)
  "Apply reviewed DIFF from FILE."
  (gnosis-import--apply-changes file nil '(101) (nth 2 diff) (nth 3 diff)))

(defun gnosis-test-import-snapshot--refusal (action)
  "Assert ACTION refuses companions and preserves destination and files."
  (let ((files (gnosis-test-import-snapshot--files))
        (rows (sqlite-select gnosis-db "SELECT * FROM themata")))
    (should (string-match-p
             "SQLite companions"
             (error-message-string (should-error (funcall action)
                                                 :type 'user-error))))
    (should (equal files (gnosis-test-import-snapshot--files)))
    (should (equal rows (sqlite-select gnosis-db "SELECT * FROM themata")))
    (should-not (assoc 2 (sqlite-select gnosis-db "PRAGMA database_list")))))

(ert-deftest gnosis-import-snapshot-refuses-committed-wal-preview ()
  "Public preview refuses genuinely committed uncheckpointed source rows."
  (gnosis-test-with-db
    (let* ((file (gnosis-test-import-snapshot--source))
           (hash (gnosis-import--file-sha256 file))
           (source (sqlite-open file)))
      (unwind-protect
          (progn
            (gnosis-test-import-snapshot--wal-update source)
            (should (equal hash (gnosis-import--file-sha256 file)))
            (should (< 0 (file-attribute-size
                          (file-attributes (concat file "-wal")))))
            (gnosis-test-import-snapshot--refusal
             (lambda () (gnosis-import-db file)))
            (should (equal '(("Committed in WAL"))
                           (gnosis-sqlite-select source
                                                 "SELECT keimenon FROM themata"))))
        (sqlite-close source)))))

(ert-deftest gnosis-import-snapshot-refuses-abrupt-writer-wal ()
  "Preserve an abruptly exited writer's committed WAL without recovery."
  (skip-unless (eq system-type 'gnu/linux))
  (gnosis-test-with-db
    (let* ((file (gnosis-test-import-snapshot--source))
           (diff (gnosis-import--diff file)))
      (should
       (stringp
        (call-process
         (expand-file-name invocation-name invocation-directory)
         nil nil nil "-Q" "--batch" "--eval"
         (prin1-to-string
          `(let ((db (sqlite-open ,file)))
             (sqlite-execute db "PRAGMA wal_autocheckpoint=0")
             (sqlite-execute db "UPDATE themata SET keimenon = ?"
                             '("\"Committed in WAL\""))
             (signal-process (emacs-pid) 'SIGKILL))))))
      (should (equal (nth 2 diff) (gnosis-import--file-sha256 file)))
      (should (< 0 (file-attribute-size
                    (file-attributes (concat file "-wal")))))
      (gnosis-test-import-snapshot--refusal
       (lambda () (gnosis-import--diff file)))
      (gnosis-test-import-snapshot--refusal
       (lambda () (gnosis-test-import-snapshot--apply file diff)))
      (let ((source (sqlite-open file)))
        (unwind-protect
            (should (equal '(("Committed in WAL"))
                           (gnosis-sqlite-select source
                                                 "SELECT keimenon FROM themata")))
          (sqlite-close source))))))

(ert-deftest gnosis-import-snapshot-refuses-wal-after-preview ()
  "WAL-only source changes after preview cannot overwrite local content."
  (gnosis-test-with-db
    (let* ((file (gnosis-test-import-snapshot--source))
           (diff (gnosis-import--diff file))
           (source (sqlite-open file)))
      (unwind-protect
          (progn
            (gnosis-test-import-snapshot--wal-update source)
            (should (equal (nth 2 diff) (gnosis-import--file-sha256 file)))
            (with-temp-buffer
              (gnosis-import-diff-mode)
              (setq gnosis-import--file file
                    gnosis-import--source-id (nth 2 diff)
                    gnosis-import--destination (nth 3 diff)
                    gnosis-import--changed-ids '(101))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                        ((symbol-function 'quit-window) #'ignore))
                (gnosis-test-import-snapshot--refusal
                 (lambda () (call-interactively (key-binding (kbd "a")))))))
            (should (equal '(("Committed in WAL"))
                           (gnosis-sqlite-select source
                                                 "SELECT keimenon FROM themata"))))
        (sqlite-close source)))))

(ert-deftest gnosis-import-snapshot-refuses-companion-names-and-aliases ()
  "Refuse each companion, including dangling links and canonical sources."
  (dolist (suffix '("-wal" "-shm" "-journal"))
    (dolist (kind '(regular dangling source-alias))
      (gnosis-test-with-db
        (let* ((file (gnosis-test-import-snapshot--source))
               (diff (gnosis-import--diff file))
               (companion (concat file suffix))
               (input (if (eq kind 'source-alias)
                          (expand-file-name "alias.db" gnosis-dir)
                        file)))
          (if (eq kind 'dangling)
              (make-symbolic-link "missing-companion" companion)
            (write-region "Retained companion" nil companion nil 'silent))
          (when (eq kind 'source-alias) (make-symbolic-link file input))
          (gnosis-test-import-snapshot--refusal
           (lambda () (gnosis-import--diff input)))
          (gnosis-test-import-snapshot--refusal
           (lambda () (gnosis-test-import-snapshot--apply input diff))))))))

(ert-deftest gnosis-import-snapshot-refuses-companion-during-copy ()
  "Recheck preview and apply after the private source copy is acquired."
  (dolist (apply-p '(nil t))
    (gnosis-test-with-db
      (let* ((file (gnosis-test-import-snapshot--source))
             (diff (gnosis-import--diff file))
             (copy (symbol-function 'copy-file))
             (before (gnosis-import--file-sha256 gnosis-test--db-file))
             injected)
        (cl-letf (((symbol-function 'copy-file)
                   (lambda (from to &rest args)
                     (prog1 (apply copy from to args)
                       (when (equal from file)
                         (setq injected t)
                         (write-region "Retained companion" nil
                                       (concat file "-journal") nil 'silent))))))
          (should-error
           (if apply-p (gnosis-test-import-snapshot--apply file diff)
             (gnosis-import--diff file))
           :type 'user-error))
        (should injected)
        (should (equal before (gnosis-import--file-sha256 gnosis-test--db-file)))
        (should (equal "Local" (gnosis-get 'keimenon 'themata '(= id 101))))))))

(ert-deftest gnosis-import-snapshot-refuses-companion-before-preview-return ()
  "Do not publish a preview if a companion appeared while reading rows."
  (gnosis-test-with-db
    (let* ((file (gnosis-test-import-snapshot--source))
           (read (symbol-function 'gnosis-import--diff-snapshot))
           (before (gnosis-import--file-sha256 gnosis-test--db-file)))
      (cl-letf (((symbol-function 'gnosis-import--diff-snapshot)
                 (lambda (snapshot)
                   (prog1 (funcall read snapshot)
                     (write-region "Retained companion" nil
                                   (concat file "-journal") nil 'silent)))))
        (should-error (gnosis-import--diff file) :type 'user-error))
      (should (equal before (gnosis-import--file-sha256 gnosis-test--db-file))))))

(ert-deftest gnosis-import-snapshot-refuses-wal-during-apply ()
  "Refuse source WAL drift at write-lock acquisition and before commit."
  (dolist (boundary '(lock write))
    (gnosis-test-with-db
      (let* ((file (gnosis-test-import-snapshot--source))
             (diff (gnosis-import--diff file))
             (execute (symbol-function 'sqlite-execute))
             (write (symbol-function 'gnosis-import--write-changes))
             (before (gnosis-import--file-sha256 gnosis-test--db-file))
             source source-files)
        (unwind-protect
            (cl-labels ((change-source ()
                          (setq source (sqlite-open file))
                          (gnosis-test-import-snapshot--wal-update source)
                          (setq source-files
                                (mapcar #'gnosis-import--file-sha256
                                        (list file (concat file "-wal")
                                              (concat file "-shm"))))))
              (cl-letf (((symbol-function 'sqlite-execute)
                         (lambda (db sql &optional params)
                           (prog1 (funcall execute db sql params)
                             (when (and (eq boundary 'lock) (eq db gnosis-db)
                                        (equal sql "BEGIN IMMEDIATE") (not source))
                               (change-source)))))
                        ((symbol-function 'gnosis-import--write-changes)
                         (lambda (&rest args)
                           (prog1 (apply write args)
                             (when (eq boundary 'write) (change-source))))))
                (should-error (gnosis-test-import-snapshot--apply file diff)
                              :type 'user-error))
              (should source)
              (should (equal source-files
                             (mapcar #'gnosis-import--file-sha256
                                     (list file (concat file "-wal")
                                           (concat file "-shm")))))
              (should (equal before
                             (gnosis-import--file-sha256 gnosis-test--db-file)))
              (should (equal "Local" (gnosis-get 'keimenon 'themata '(= id 101))))
              (should (equal '(("Committed in WAL"))
                             (gnosis-sqlite-select source
                                                   "SELECT keimenon FROM themata")))
              (should-not (assoc 2 (sqlite-select gnosis-db "PRAGMA database_list"))))
          (when source (sqlite-close source)))))))

(ert-deftest gnosis-import-snapshot-closed-source-remains-supported ()
  "A closed standalone export still supports preview and apply."
  (gnosis-test-with-db
    (let* ((file (gnosis-test-import-snapshot--source))
           (before (gnosis-import--file-sha256 file))
           (diff (gnosis-import--diff file)))
      (should (equal '(101) (mapcar #'car (cadr diff))))
      (gnosis-test-import-snapshot--apply file diff)
      (should (equal "Exported" (gnosis-get 'keimenon 'themata '(= id 101))))
      (should (equal before (gnosis-import--file-sha256 file)))
      (dolist (suffix '("-wal" "-shm" "-journal"))
        (should-not (file-exists-p (concat file suffix)))))))

(provide 'gnosis-test-import-snapshot)
;;; gnosis-test-import-snapshot.el ends here
