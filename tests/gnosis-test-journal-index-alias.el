;;; gnosis-test-journal-index-alias.el --- Journal alias saves -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Ordinary file saves reconcile only physically identical journal indexes.
;; The journey helpers also run with native keys in disposable interactive Emacs.

;;; Code:

(require 'gnosis-test-journal-capture-alias)

(defun gnosis-test-journal-index-alias--save ()
  "Save with the ordinary command, using native keys in interactive Emacs."
  (should (memq #'gnosis-nodes-update-file after-save-hook))
  (if noninteractive (save-buffer) (execute-kbd-macro (kbd "C-x C-s"))))

(defun gnosis-test-journal-index-alias--reopen ()
  "Close and reopen the fixture's real SQLite database."
  (gnosis-sqlite-close gnosis-db)
  (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
  (gnosis-db-init))

(defun gnosis-test-journal-index-alias--seed (kind external)
  "Write a journal, configure KIND's alias and return both paths.
When EXTERNAL is non-nil, put the physical path outside the node directory."
  (let ((file (expand-file-name "physical.org"
                                (if external gnosis-dir gnosis-journal-dir))))
    (with-temp-file file
      (insert "#+title: Journal\n* 2001-02-03\n:PROPERTIES:\n:ID: day\n:END:\nOriginal Ελληνικά\n* 2001-02-04\n:PROPERTIES:\n:ID: removed\n:END:\nOld sibling\n"))
    (list file (gnosis-test-journal-capture-alias--configure kind file))))

(defun gnosis-test-journal-index-alias--unrelated ()
  "Index an unrelated journal and an ordinary node with an incoming link."
  (gnosis-test-journal-selection--file "unrelated.org" "1999-01-01" "unrelated")
  (let ((file (expand-file-name "unrelated.org" gnosis-nodes-dir)))
    (with-temp-file file
      (insert ":PROPERTIES:\n:ID: note\n:END:\n#+title: Note\n#+filetags: :keep:\n[[id:day][Journal]]\n"))
    (gnosis-nodes-update-file file)))

(defun gnosis-test-journal-index-alias--snapshot ()
  "Return index rows, including unrelated nodes, tags and links."
  (mapcar (lambda (table) (gnosis-nodes-select '* table))
          '(journal nodes node-tag node-links)))

(defun gnosis-test-journal-index-alias--journey (kind reverse external)
  "Save KIND's journal through both names, reversing order when REVERSE.
EXTERNAL puts the physical path outside the journal directory."
  (gnosis-test-journal-selection--with-files
    (pcase-let* ((`(,file ,alias) (gnosis-test-journal-index-alias--seed kind external))
                 (first (if reverse file alias))
                 (second (if reverse alias file)))
      (gnosis-test-journal-index-alias--unrelated)
      (find-file first)
      (gnosis-nodes-mode 1)
      (should (equal buffer-file-name first))
      (goto-char (point-max))
      (insert "First edit\n")
      (gnosis-test-journal-index-alias--save)
      (should (equal (gnosis-nodes-select 'file 'journal '(= id "day") t)
                     (list (gnosis-nodes--file-key first t))))
      (kill-buffer)
      (gnosis-test-journal-index-alias--reopen)
      (let ((unrelated (gnosis-nodes-select '* 'journal '(= id "unrelated")))
            (nodes (gnosis-nodes-select '* 'nodes))
            (tags (gnosis-nodes-select '* 'node-tag))
            (links (gnosis-nodes-select '* 'node-links))
            (before (gnosis-test-journal-index-alias--snapshot)))
        (find-file second)
        (gnosis-nodes-mode 1)
        (should (equal buffer-file-name second))
        (goto-char (point-min))
        (search-forward "2001-02-03")
        (replace-match "2001-02-05")
        (search-forward "* 2001-02-04")
        (beginning-of-line)
        (delete-region (point) (point-max))
        (insert "* 2001-02-06\n:PROPERTIES:\n:ID: added\n:END:\nSecond edit\n")
        (let ((text (buffer-string)))
          (goto-char (point-min))
          (search-forward "* 2001-02-05")
          (beginning-of-line)
          (org-narrow-to-subtree)
          (should (buffer-modified-p))
          (should (equal (nth 2 (gnosis-journal--unique-entry "2001-02-05")) alias))
          (should (equal before (gnosis-test-journal-index-alias--snapshot)))
          (let ((view (list (point) (point-min) (point-max))))
            (gnosis-test-journal-index-alias--save)
            (should (equal view (list (point) (point-min) (point-max)))))
          (should-not (buffer-modified-p))
          (should (file-equal-p file alias))
          (should (equal text (gnosis-test-journal-selection--read file)))
          (should (equal text (gnosis-test-journal-selection--read alias)))
          (should (equal text (save-restriction (widen) (buffer-string))))
          (kill-buffer)
          (gnosis-test-journal-index-alias--reopen)
          (should (equal (sort (gnosis-nodes-select 'id 'journal nil t) #'string<)
                         '("added" "day" "unrelated")))
          (should (equal (gnosis-nodes-select 'title 'journal '(= id "day") t)
                         '("2001-02-05")))
          (should (equal unrelated (gnosis-nodes-select '* 'journal '(= id "unrelated"))))
          (should (equal nodes (gnosis-nodes-select '* 'nodes)))
          (should (equal tags (gnosis-nodes-select '* 'node-tag)))
          (should (equal links (gnosis-nodes-select '* 'node-links)))
          (should (equal (nth 2 (gnosis-journal--unique-entry "2001-02-05")) alias))
          (gnosis-journal-find "2001-02-05")
          (should (equal (org-id-get) "day"))
          (should (equal text (buffer-string)))
          ;; The replacement's exact visited key remains usable for deletion.
          (kill-buffer)
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (gnosis-nodes-delete-file second))
          (should-not (file-exists-p second))
          (gnosis-test-journal-index-alias--reopen)
          (should (equal unrelated (gnosis-nodes-select '* 'journal)))
          (should (equal nodes (gnosis-nodes-select '* 'nodes)))
          (should (equal tags (gnosis-nodes-select '* 'node-tag)))
          (should-not (gnosis-nodes-select '* 'node-links)))))))

(ert-deftest gnosis-test-journal-index-alias-save ()
  "Native save reconciles retained aliases in both directions and locations."
  (dolist (kind '(canonical symlink hardlink))
    (dolist (reverse '(nil t))
      (dolist (external '(nil t))
        (gnosis-test-journal-index-alias--journey kind reverse external)))))

(ert-deftest gnosis-test-journal-index-alias-distinct-file ()
  "Equal IDs and bytes in distinct files do not confer index ownership."
  (dolist (kind '(symlink hardlink))
    (gnosis-test-journal-selection--with-files
      (pcase-let* ((`(,file ,alias) (gnosis-test-journal-index-alias--seed kind nil))
                   (copy (expand-file-name "copy.org" gnosis-journal-dir)))
        (find-file alias)
        (gnosis-nodes-mode 1)
        (goto-char (point-max))
        (insert "First edit\n")
        (gnosis-test-journal-index-alias--save)
        (kill-buffer)
        (copy-file file copy)
        (let ((before (gnosis-test-journal-index-alias--snapshot))
              (text (gnosis-test-journal-selection--read file)))
          (find-file copy)
          (gnosis-nodes-mode 1)
          (goto-char (point-max))
          (insert "Separate edit\n")
          (should-error (gnosis-test-journal-index-alias--save))
          (should (equal text (gnosis-test-journal-selection--read file)))
          (should (string-match-p "Separate edit" (gnosis-test-journal-selection--read copy)))
          (gnosis-test-journal-index-alias--reopen)
          (should (equal before (gnosis-test-journal-index-alias--snapshot))))))))

(ert-deftest gnosis-test-journal-index-alias-rollback ()
  "Error and quit restore alias rows when replacement insertion is interrupted."
  (dolist (kind '(symlink hardlink))
    (dolist (failure '(error quit))
      (gnosis-test-journal-selection--with-files
        (pcase-let ((`(,file ,alias) (gnosis-test-journal-index-alias--seed kind nil)))
          (gnosis-test-journal-index-alias--unrelated)
          (find-file alias)
          (gnosis-nodes-mode 1)
          (goto-char (point-max)) (insert "First edit\n")
          (gnosis-test-journal-index-alias--save)
          (kill-buffer)
          (let ((before (gnosis-test-journal-index-alias--snapshot))
                (insert-data (symbol-function 'gnosis-nodes--insert-file-data)))
            (find-file file)
            (gnosis-nodes-mode 1)
            (goto-char (point-max)) (insert "Second edit\n")
            (cl-letf (((symbol-function 'gnosis-nodes--insert-file-data)
                       (lambda (&rest args)
                         (funcall insert-data (car args) (cadr args)
                                  (nth 2 args) (nth 3 args))
                         (signal failure '("Interrupted journal replacement")))))
              (should (condition-case err
                          (progn (gnosis-test-journal-index-alias--save) nil)
                        ((error quit) (eq (car err) failure)))))
            (gnosis-test-journal-index-alias--reopen)
            (should (equal before (gnosis-test-journal-index-alias--snapshot)))
            ;; The ordinary after-save failure does not undo saved Org bytes.
            (should (string-match-p "Second edit" (gnosis-test-journal-selection--read file)))
            (set-buffer-modified-p t)
            (gnosis-test-journal-index-alias--save)
            (should (equal (gnosis-nodes-select 'file 'journal '(= id "day") t)
                           (list (gnosis-nodes--file-key file t))))))))))

(ert-deftest gnosis-test-journal-index-alias-sync-dirty ()
  "Disk sync reconciles aliases without parsing or changing a dirty owner."
  (dolist (kind '(symlink hardlink))
    (gnosis-test-journal-selection--with-files
      (pcase-let ((`(,file ,alias) (gnosis-test-journal-index-alias--seed kind t)))
        (find-file file)
        (gnosis-nodes-mode 1)
        (goto-char (point-max)) (insert "First edit\n")
        (gnosis-test-journal-index-alias--save)
        (goto-char (point-min))
        (search-forward "2001-02-03") (replace-match "2001-02-05")
        (narrow-to-region (point) (point-max))
        (let ((before (gnosis-test-journal-capture-alias--snapshot))
              (disk (gnosis-test-journal-selection--read file)))
          (gnosis-journal-db-sync)
          (should (equal before (gnosis-test-journal-capture-alias--snapshot)))
          (should (equal disk (gnosis-test-journal-selection--read file)))
          (should (equal (gnosis-nodes-select 'title 'journal '(= id "day") t)
                         '("2001-02-03")))
          (should (equal (nth 2 (gnosis-journal--unique-entry "2001-02-05")) alias))
          (gnosis-test-journal-index-alias--save)
          (gnosis-test-journal-index-alias--reopen)
          (should (equal (gnosis-nodes-select 'title 'journal '(= id "day") t)
                         '("2001-02-05"))))))))

(ert-deftest gnosis-test-journal-index-alias-delete-recovery ()
  "Deleting the other name keeps ownership through unlink and failed cleanup."
  (dolist (kind '(symlink hardlink))
    (dolist (reverse '(nil t))
      (dolist (failure '(nil error quit changed))
        (gnosis-test-journal-selection--with-files
          (pcase-let* ((`(,file ,alias) (gnosis-test-journal-index-alias--seed kind t))
                       (target (if reverse file alias))
                       (indexed (if reverse alias file)))
            (gnosis-test-journal-index-alias--unrelated)
            ;; Save both names first; delete the one no longer owning the key.
            (dolist (path (list target indexed))
              (find-file path)
              (gnosis-nodes-mode 1)
              (goto-char (point-max)) (insert "Saved edit\n")
              (gnosis-test-journal-index-alias--save)
              (kill-buffer))
            (let ((before (gnosis-test-journal-index-alias--snapshot))
                  (unrelated (gnosis-nodes-select '* 'journal '(= id "unrelated"))))
              (find-file target)
              (let ((owner (current-buffer)))
                (unwind-protect
                    (progn
                      (when failure
                        (sqlite-execute
                         gnosis-db
                         "CREATE TRIGGER fail_alias_delete BEFORE DELETE ON journal BEGIN SELECT RAISE(ABORT, 'alias cleanup failed'); END"))
                      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                        (if failure
                            (let ((delete-row (symbol-function 'gnosis-nodes--delete)))
                              (cl-letf (((symbol-function 'gnosis-nodes--delete)
                                         (lambda (&rest args)
                                           (if (eq failure 'quit) (signal 'quit nil)
                                             (apply delete-row args)))))
                                (should (condition-case nil
                                            (progn (gnosis-nodes-delete-file) nil)
                                          ((error quit) t)))))
                          (gnosis-nodes-delete-file)))
                      (should-not (file-exists-p target))
                      (when failure
                        (should (equal before (gnosis-test-journal-index-alias--snapshot)))
                        (should-not (buffer-file-name owner))
                        (should (buffer-live-p owner))
                        (sqlite-execute gnosis-db "DROP TRIGGER fail_alias_delete")
                        (when (eq failure 'changed)
                          ;; A surviving hardlink may now own a different snapshot.
                          (when (file-symlink-p indexed) (delete-file indexed))
                          (with-temp-file indexed
                            (insert "#+title: Journal\n* 2002-01-01\n:PROPERTIES:\n:ID: successor\n:END:\nSuccessor\n"))
                          (setq gnosis-journal-file indexed)
                          (gnosis-nodes-update-file indexed))
                        (with-current-buffer owner
                          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                            (if (eq failure 'changed)
                                (let ((after (gnosis-test-journal-index-alias--snapshot)))
                                  (should-error (gnosis-nodes-delete-file) :type 'user-error)
                                  (should (equal after (gnosis-test-journal-index-alias--snapshot))))
                              (with-temp-buffer (gnosis-nodes-delete-file target))))))
                      (gnosis-test-journal-index-alias--reopen)
                      (unless (eq failure 'changed)
                        (should (equal unrelated (gnosis-nodes-select '* 'journal)))
                        (should-not (gnosis-nodes-select '* 'node-links)))
                      (should (equal (nth 1 before) (gnosis-nodes-select '* 'nodes)))
                      (should (equal (nth 2 before) (gnosis-nodes-select '* 'node-tag))))
                  (when (buffer-live-p owner)
                    (with-current-buffer owner (set-buffer-modified-p nil))
                    (kill-buffer owner)))))))))))

(ert-deftest gnosis-test-journal-index-alias-cold-delete-retry ()
  "The saved index key supports explicit missing-file retry after restart."
  (dolist (kind '(symlink hardlink))
    (dolist (reverse '(nil t))
      (gnosis-test-journal-selection--with-files
        (pcase-let* ((`(,file ,alias) (gnosis-test-journal-index-alias--seed kind t))
                     (target (if reverse alias file)))
          (gnosis-test-journal-index-alias--unrelated)
          (dolist (path (list (if reverse file alias) target))
            (find-file path)
            (gnosis-nodes-mode 1)
            (goto-char (point-max)) (insert "Saved edit\n")
            (gnosis-test-journal-index-alias--save)
            (kill-buffer))
          (let ((before (gnosis-test-journal-index-alias--snapshot))
                (unrelated (gnosis-nodes-select '* 'journal '(= id "unrelated"))))
            (find-file target)
            (sqlite-execute
             gnosis-db
             "CREATE TRIGGER fail_alias_delete BEFORE DELETE ON journal BEGIN SELECT RAISE(ABORT, 'cleanup failed'); END")
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (should-error (gnosis-nodes-delete-file)))
            (should-not buffer-file-name)
            (should-not (file-exists-p target))
            (should (equal before (gnosis-test-journal-index-alias--snapshot)))
            (kill-buffer)
            (sqlite-execute gnosis-db "DROP TRIGGER fail_alias_delete")
            (gnosis-sqlite-close gnosis-db)
            (unwind-protect
                (with-temp-buffer
                  (let* ((form
                          `(progn
                             (setq load-path ',load-path gnosis-dir ,gnosis-dir
                                   gnosis-nodes-dir ,gnosis-nodes-dir
                                   gnosis-journal-dir ,gnosis-journal-dir
                                   gnosis-journal-file ,gnosis-journal-file
                                   gnosis-testing t gnosis-vc-auto-push nil
                                   org-id-track-globally nil native-comp-jit-compilation nil)
                             (require 'gnosis-journal)
                             (gnosis--ensure-db)
                             (when (gnosis-nodes--file-buffer ,target)
                               (error "Unexpected recovery buffer in fresh process"))
                             (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                               (gnosis-nodes-delete-file ,target))
                             (unless (equal ',unrelated (gnosis-nodes-select '* 'journal))
                               (error "Missing journal cleanup did not complete"))
                             (gnosis-sqlite-close gnosis-db)))
                         (status (call-process
                                  (expand-file-name invocation-name invocation-directory)
                                  nil t nil "-Q" "--batch" "--eval" (prin1-to-string form))))
                    (ert-info ((buffer-string)) (should (equal status 0)))))
              (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file)))
            (should (equal unrelated (gnosis-nodes-select '* 'journal)))
            (should (equal (nth 1 before) (gnosis-nodes-select '* 'nodes)))
            (should (equal (nth 2 before) (gnosis-nodes-select '* 'node-tag)))
            (should-not (gnosis-nodes-select '* 'node-links))))))))

(ert-deftest gnosis-test-journal-index-alias-delete-owner-callback ()
  "A callback's replacement index cannot be erased by retained alias keys."
  (gnosis-test-journal-selection--with-files
    (pcase-let ((`(,file ,alias) (gnosis-test-journal-index-alias--seed 'hardlink t)))
      (dolist (path (list file alias))
        (find-file path)
        (gnosis-nodes-mode 1)
        (goto-char (point-max)) (insert "Saved edit\n")
        (gnosis-test-journal-index-alias--save)
        (kill-buffer))
      (let ((before (gnosis-test-journal-index-alias--snapshot))
            (ownership (symbol-function 'gnosis-nodes--journal-ownership))
            (replacement "#+title: Journal\n* 2002-01-01\n:PROPERTIES:\n:ID: successor\n:END:\nSuccessor\n")
            injected)
        (find-file file)
        (let ((owner (current-buffer)))
          (unwind-protect
              (progn
                ;; Simulate an Org callback at the ownership parser's return.
                (cl-letf (((symbol-function 'gnosis-nodes--journal-ownership)
                           (lambda (&rest args)
                             (prog1 (apply ownership args)
                               (when (and (not injected) (not (file-exists-p file)))
                                 (setq injected t)
                                 (with-temp-file alias (insert replacement))
                                 (gnosis-nodes-update-file alias)))))
                          ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                  (should-error (gnosis-nodes-delete-file)))
                (should injected)
                (should-not (file-exists-p file))
                (should-not (buffer-file-name owner))
                (should (equal before (gnosis-test-journal-index-alias--snapshot)))
                (should (equal replacement (gnosis-test-journal-selection--read alias)))
                (gnosis-journal-db-sync)
                (should (equal (gnosis-nodes-select 'id 'journal nil t) '("successor"))))
            (when (buffer-live-p owner)
              (with-current-buffer owner (set-buffer-modified-p nil))
              (kill-buffer owner))))))))

(ert-deftest gnosis-test-journal-index-alias-coalesced-delete ()
  "Refuse alias unlink when native visiting coalesces onto another filename."
  (dolist (kind '(symlink hardlink))
    (gnosis-test-journal-selection--with-files
      (pcase-let* ((`(,file ,alias) (gnosis-test-journal-index-alias--seed kind t))
                   (find-file-existing-other-name t))
        (find-file file)
        (gnosis-nodes-mode 1)
        (goto-char (point-max)) (insert "Saved edit\n")
        (gnosis-test-journal-index-alias--save)
        (insert "Unsaved draft\n")
        (narrow-to-region (1- (point-max)) (point-max))
        (let ((owner (current-buffer))
              (before (gnosis-test-journal-index-alias--snapshot))
              (view (gnosis-test-journal-capture-alias--snapshot))
              (disk (gnosis-test-journal-selection--read file)))
          (should (eq owner (find-file-noselect alias)))
          (sqlite-execute
           gnosis-db
           "CREATE TRIGGER fail_alias_delete BEFORE DELETE ON journal BEGIN SELECT RAISE(ABORT, 'cleanup failed'); END")
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (should-error (gnosis-nodes-delete-file alias) :type 'user-error))
          (sqlite-execute gnosis-db "DROP TRIGGER fail_alias_delete")
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (should-error (gnosis-nodes-delete-file alias) :type 'user-error))
          (should (file-equal-p file alias))
          (should (equal disk (gnosis-test-journal-selection--read alias)))
          (should (equal before (gnosis-test-journal-index-alias--snapshot)))
          (with-current-buffer owner
            (should (equal view (gnosis-test-journal-capture-alias--snapshot)))
            ;; Following the error's guidance keeps normal deletion available.
            (gnosis-test-journal-index-alias--save)
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (gnosis-nodes-delete-file file)))
          (should-not (buffer-live-p owner))
          (should-not (file-exists-p file))
          (should-not (gnosis-nodes-select '* 'journal)))))))

(provide 'gnosis-test-journal-index-alias)
;;; gnosis-test-journal-index-alias.el ends here
