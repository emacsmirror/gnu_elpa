;;; gnosis-test-node-paths.el --- Node path identity -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Save, rebuild, navigate and delete same-basename files in a nested vault.

;;; Code:

(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-nodes)
(require 'gnosis-journal)

(defmacro gnosis-test-node-paths--with-vault (&rest body)
  "Run BODY with root A and nested B sharing a basename, and journal J."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-db
     (let* ((gnosis-nodes-dir (expand-file-name "nodes/" gnosis-dir))
            (gnosis-journal-dir (expand-file-name "journal/" gnosis-nodes-dir))
            (gnosis-journal-file nil)
            (org-id-track-globally nil)
            (make-backup-files nil)
            (create-lockfiles nil)
            (a (expand-file-name "same.org" gnosis-nodes-dir))
            (b (expand-file-name "sub/same.org" gnosis-nodes-dir))
            (j (expand-file-name "same.org" gnosis-journal-dir)))
       (dolist (entry (list (cons a "root") (cons b "nested") (cons j "journal")))
         (make-directory (file-name-directory (car entry)) t)
         (with-temp-file (car entry)
           (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: %s\n#+filetags: :kept:\nNeedle %s\n"
                           (cdr entry) (cdr entry) (cdr entry)))))
       (unwind-protect
           (save-window-excursion ,@body)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (or (and buffer-file-name
                            (file-in-directory-p buffer-file-name gnosis-dir))
                       gnosis-nodes--deleted-file)
               (set-buffer-modified-p nil)
               (let ((kill-buffer-query-functions nil))
                 (kill-buffer buffer)))))))))

(defun gnosis-test-node-paths--bytes (file)
  "Return literal bytes of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun gnosis-test-node-paths--index ()
  "Return the node, journal, tag and link indexes."
  (mapcar (lambda (table) (gnosis-select '* table))
          '(nodes journal node-tag node-links)))

(ert-deftest gnosis-test-node-paths-native-save-navigation-rebuild ()
  "Native saves and rebuilds retain both paths and visit the actual IDs."
  (gnosis-test-node-paths--with-vault
    (let ((root-bytes (gnosis-test-node-paths--bytes a)))
      (dolist (file (list a b))
        (with-current-buffer (find-file-noselect file)
          (should gnosis-nodes-mode)
          (goto-char (point-max))
          (insert "Saved.\n")
          (call-interactively #'save-buffer)))
      (should (equal '(("root" "same.org") ("nested" "sub/same.org"))
                     (gnosis-select '[id file] 'nodes)))
      (should (equal (concat root-bytes "Saved.\n")
                     (gnosis-test-node-paths--bytes a)))
      (dolist (entry (list (cons a "root") (cons b "nested")))
        (gnosis-nodes-goto-id (cdr entry))
        (should (equal buffer-file-name (car entry)))
        (should (equal (org-id-get) (cdr entry)))
        (should (eq (key-binding (kbd "C-c C-o")) #'gnosis-nodes-goto-id)))
      (let ((bytes (mapcar #'gnosis-test-node-paths--bytes (list a b j))))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (call-interactively #'gnosis-nodes-db-force-sync))
        (should (equal bytes (mapcar #'gnosis-test-node-paths--bytes (list a b j))))
        (should (equal '("nested" "root")
                       (sort (gnosis-select 'id 'nodes nil t) #'string<)))
        (should (equal '("journal") (gnosis-select 'id 'journal nil t)))
        (should (equal '("nested" "root")
                       (sort (gnosis-nodes-search-content "Needle") #'string<)))
        (gnosis-nodes-find "nested")
        (should (equal buffer-file-name b))))))

(ert-deftest gnosis-test-node-paths-incremental-discovery ()
  "Ordinary sync discovers descendants but skips journal and lock files."
  (gnosis-test-node-paths--with-vault
    (let ((lock (expand-file-name "sub/.#same.org" gnosis-nodes-dir)))
      (with-temp-file lock (insert ":PROPERTIES:\n:ID: lock\n:END:\n"))
      (gnosis-nodes-db-sync)
      (should (equal '("nested" "root")
                     (sort (gnosis-select 'id 'nodes nil t) #'string<)))
      (should (equal '("journal") (gnosis-select 'id 'journal nil t)))
      (let ((before (gnosis-test-node-paths--index)))
        (gnosis-nodes-db-sync)
        (should (equal before (gnosis-test-node-paths--index)))))))

(ert-deftest gnosis-test-node-paths-legacy-force-sync ()
  "A full rebuild repairs old basename-only rows without rewriting Org."
  (gnosis-test-node-paths--with-vault
    ;; Reproduce the old index after saving the descendant last.
    (gnosis-nodes--insert-file-data
     'nodes "same.org" "0" (gnosis-nodes--file-info b))
    (let ((bytes (mapcar #'gnosis-test-node-paths--bytes (list a b j))))
      (gnosis-nodes-db-sync t)
      (should (equal bytes (mapcar #'gnosis-test-node-paths--bytes (list a b j))))
      (should (equal "same.org" (gnosis-get 'file 'nodes '(= id "root"))))
      (should (equal "sub/same.org" (gnosis-get 'file 'nodes '(= id "nested"))))
      (gnosis-nodes-goto-id "nested")
      (should (equal buffer-file-name b)))))

(ert-deftest gnosis-test-node-paths-save-and-sync-rollback ()
  "Storage failures retain both indexes; native save still owns its bytes."
  (gnosis-test-node-paths--with-vault
    (gnosis-nodes-db-sync t)
    (let ((before (gnosis-test-node-paths--index))
          (root-bytes (gnosis-test-node-paths--bytes a)))
      (sqlite-execute gnosis-db
                      "CREATE TRIGGER fail_paths BEFORE INSERT ON nodes BEGIN SELECT RAISE(ABORT, 'index fault'); END")
      (with-current-buffer (find-file-noselect b)
        (goto-char (point-max))
        (insert "Saved despite indexing failure.\n")
        ;; A hook error reports the failed index after the file was saved.
        (should-error (call-interactively #'save-buffer))
        (should-not (buffer-modified-p)))
      (should (equal before (gnosis-test-node-paths--index)))
      (should (equal root-bytes (gnosis-test-node-paths--bytes a)))
      (should-error (gnosis-nodes-db-sync t))
      (should (equal before (gnosis-test-node-paths--index)))
      (sqlite-execute gnosis-db "DROP TRIGGER fail_paths")
      (gnosis-nodes-db-sync t)
      (should (gnosis-get 'id 'nodes '(= id "root")))
      (should (gnosis-get 'id 'nodes '(= id "nested"))))))

(ert-deftest gnosis-test-node-paths-delete-cancel-fault-retry ()
  "Nested deletion preserves its namesake across cancel and file/SQL faults."
  (gnosis-test-node-paths--with-vault
    (gnosis-nodes-db-sync t)
    (let ((before (gnosis-test-node-paths--index))
          (root-bytes (gnosis-test-node-paths--bytes a)))
      (switch-to-buffer (find-file-noselect b))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (call-interactively #'gnosis-nodes-delete-file))
      (should (file-exists-p b))
      (should (equal before (gnosis-test-node-paths--index)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'delete-file) (lambda (&rest _) (error "File fault"))))
        (should-error (call-interactively #'gnosis-nodes-delete-file)))
      (should (file-exists-p b))
      (should (equal before (gnosis-test-node-paths--index)))
      (sqlite-execute gnosis-db
                      "CREATE TRIGGER fail_paths BEFORE DELETE ON nodes BEGIN SELECT RAISE(ABORT, 'index fault'); END")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (should-error (call-interactively #'gnosis-nodes-delete-file)))
      (should-not (file-exists-p b))
      (should-not buffer-file-name)
      (should (equal before (gnosis-test-node-paths--index)))
      (sqlite-execute gnosis-db "DROP TRIGGER fail_paths")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (call-interactively #'gnosis-nodes-delete-file))
      (should (equal root-bytes (gnosis-test-node-paths--bytes a)))
      (should (equal '("root") (gnosis-select 'id 'nodes nil t)))
      (should (equal '("journal") (gnosis-select 'id 'journal nil t)))
      (should (gnosis-select '* 'node-tag '(= node-id "root"))))))

(provide 'gnosis-test-node-paths)
;;; gnosis-test-node-paths.el ends here
