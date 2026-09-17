;;; gnosis-test-authoring-quality-storage.el --- Export and open safety -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Preserve export operation ownership and propagate index initialization faults.

;;; Code:

(require 'ert)
(require 'gnosis-test-authoring-quality)
(require 'gnosis-test-db-safety)

(ert-deftest gnosis-test-authoring-quality-export-owner-replacement ()
  "Callbacks cannot retarget export, even onto an unrelated destination."
  (dolist (boundary '(tags filename suspended confirmation validation destination))
    (dolist (destination '(database symlink hardlink unrelated))
      (gnosis-test-with-db
        (gnosis-test-authoring-quality--seed)
        (let* ((owner gnosis-db)
               (other-file (expand-file-name "other.db" gnosis-dir))
               (other (gnosis-sqlite-open other-file))
               (file (if (eq destination 'database) other-file
                       (expand-file-name "export.gnosis" gnosis-dir)))
               (validate (symbol-function 'gnosis-export--validate))
               (check (symbol-function 'gnosis-export--check-destination))
               validated replaced)
          (unwind-protect
              (progn
                (let ((gnosis-db other))
                  (gnosis-db-init)
                  (gnosis-test-authoring-quality--seed)
                  (gnosis-update-thema 111 "Other question" nil '("Answer") "Other extra"
                                       '("other") '("different")))
                (pcase destination
                  ('symlink (make-symbolic-link other-file file))
                  ('hardlink (add-name-to-file other-file file))
                  ('unrelated (with-temp-file file (insert "Prior destination"))))
                (let ((before (mapcar #'gnosis-test-safety-snapshot
                                     (list gnosis-test--db-file other-file)))
                      (bytes (mapcar #'gnosis-import--file-sha256
                                    (list gnosis-test--db-file other-file file))))
                  (cl-labels ((replace-owner (at)
                                (when (eq boundary at)
                                  (setq replaced t gnosis-db other))))
                    (cl-letf (((symbol-function 'gnosis-tags-filter-prompt)
                               (lambda () (replace-owner 'tags) '(nil . nil)))
                              ((symbol-function 'read-file-name)
                               (lambda (&rest _) (replace-owner 'filename) file))
                              ((symbol-function 'y-or-n-p)
                               (lambda (prompt)
                                 (replace-owner (if (string-prefix-p "Export " prompt)
                                                    'confirmation 'suspended))
                                 t))
                              ((symbol-function 'gnosis-export--validate)
                               (lambda (&rest args)
                                 (apply validate args)
                                 (setq validated t)
                                 (replace-owner 'validation)))
                              ((symbol-function 'gnosis-export--check-destination)
                               (lambda (&rest args)
                                 (apply check args)
                                 (when validated (replace-owner 'destination)))))
                      (should-error (call-interactively #'gnosis-export-db) :type 'user-error)))
                  (should replaced)
                  ;; Assert callback selection before fixture cleanup restores A.
                  (should (eq gnosis-db other))
                  (should (equal bytes (mapcar #'gnosis-import--file-sha256
                                              (list gnosis-test--db-file other-file file))))
                  (should (equal before (mapcar #'gnosis-test-safety-snapshot
                                               (list gnosis-test--db-file other-file))))
                  (when (eq destination 'symlink) (should (file-symlink-p file)))
                  (when (eq destination 'hardlink) (should (file-equal-p file other-file)))
                  (should-not (member "export_db" (mapcar #'cadr (sqlite-select owner "PRAGMA database_list"))))
                  (should-not (directory-files gnosis-dir nil "\\`\\.gnosis-export-"))))
            (setq gnosis-db owner)
            (sqlite-close other)))))))

(ert-deftest gnosis-test-authoring-quality-index-fault-rolls-back ()
  "Fresh and retained-v8 index faults close the unpublished opener candidate."
  (dolist (retained '(nil t))
    (dolist (fault '(error quit))
      (gnosis-test-safety
        (when retained (gnosis-test-safety-v8))
        (let* ((file (expand-file-name "gnosis.db" gnosis-dir))
               (before (gnosis-test-safety-snapshot file))
               (execute (symbol-function 'gnosis-sqlite-execute))
               candidate condition)
          (cl-letf (((symbol-function 'gnosis-sqlite-execute)
                     (lambda (db sql &optional params)
                       (if (equal sql "CREATE INDEX IF NOT EXISTS idx_themata_source_guid ON themata(source_guid)")
                           (progn (setq candidate db) (signal fault '("Index failure")))
                         (funcall execute db sql params)))))
            (setq condition (condition-case err (gnosis--ensure-db) ((error quit) err))))
          (should candidate)
          (should (consp condition))
          (should (eq fault (car condition)))
          (should-not gnosis-db)
          (should-error (sqlite-select candidate "SELECT 1"))
          (should (equal before (gnosis-test-safety-snapshot file)))
          (gnosis--ensure-db)
          (should (= gnosis-db-version (gnosis--db-version)))
          (should (equal '(("idx_themata_source_guid"))
                         (sqlite-select gnosis-db "SELECT name FROM sqlite_master WHERE name = 'idx_themata_source_guid'")))
          (let ((initialized (gnosis-test-safety-snapshot file)))
            (dotimes (_ 2) (gnosis--db-create-indexes gnosis-db))
            (sqlite-close gnosis-db)
            (setq gnosis-db nil)
            (gnosis--ensure-db)
            (should (equal initialized (gnosis-test-safety-snapshot file)))))))))

(provide 'gnosis-test-authoring-quality-storage)
;;; gnosis-test-authoring-quality-storage.el ends here
