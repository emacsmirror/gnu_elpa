;;; gnosis-test-campaign-nodes.el --- Node ownership regressions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Exercise confirmation, physical aliases, narrowed owners and exact selection.

;;; Code:

(require 'gnosis-test-node-selection)
(require 'gnosis-test-journal-index-alias)

(ert-deftest gnosis-test-campaign-nodes-delete-replacement ()
  "A confirmation never authorizes replacement bytes at the same pathname."
  (dolist (journal '(nil t))
    (dolist (replacement '(rename rewrite))
      (gnosis-test-node-selection--with-vault
        (let* ((file (if journal
                         (gnosis-test-journal-selection--file
                          "journal.org" "2001-02-03" "old")
                       (gnosis-test-node-selection--file "node.org" "old" "Old")))
               (table (if journal 'journal 'nodes))
               (rows (gnosis-select '* table))
               (bytes ":PROPERTIES:\n:ID: replacement\n:END:\n#+title: Replacement\n"))
          (when-let* ((buffer (get-file-buffer file))) (kill-buffer buffer))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _)
                       (if (eq replacement 'rename)
                           (let ((other (concat file ".new")))
                             (with-temp-file other (insert bytes))
                             (rename-file other file t))
                         (with-temp-file file (insert bytes)))
                       t)))
            (should-error (gnosis-nodes-delete-file file) :type 'user-error))
          (should (equal rows (gnosis-select '* table)))
          (should (equal bytes (with-temp-buffer
                                 (insert-file-contents file) (buffer-string)))))))))

(ert-deftest gnosis-test-campaign-nodes-delete-missing-replacement ()
  "Index-only confirmation cannot unlink a file that appears during input."
  (dolist (journal '(nil t))
    (gnosis-test-node-selection--with-vault
      (let* ((file (if journal
                       (gnosis-test-journal-selection--file
                        "journal.org" "2001-02-03" "old")
                     (gnosis-test-node-selection--file "node.org" "old" "Old")))
             (table (if journal 'journal 'nodes))
             (rows (gnosis-select '* table))
             (bytes ":PROPERTIES:\n:ID: replacement\n:END:\n#+title: Replacement\n"))
        (when-let* ((buffer (get-file-buffer file))) (kill-buffer buffer))
        (delete-file file)
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (with-temp-file file (insert bytes))
                     t)))
          (should-error (gnosis-nodes-delete-file file) :type 'user-error))
        (should (equal rows (gnosis-select '* table)))
        (should (equal bytes (with-temp-buffer
                              (insert-file-contents file) (buffer-string))))))))

(ert-deftest gnosis-test-campaign-nodes-delete-visit-replacement ()
  "File-visiting hooks cannot substitute an unconfirmed same-content file."
  (gnosis-test-node-selection--with-vault
    (let* ((file (gnosis-test-journal-selection--file
                  "journal.org" "2001-02-03" "old"))
           (rows (gnosis-select '* 'journal))
           (bytes (with-temp-buffer
                    (insert-file-contents file) (buffer-string)))
           (changed nil))
      (when-let* ((buffer (get-file-buffer file))) (kill-buffer buffer))
      (let ((find-file-hook
             (list (lambda ()
                     (when (equal buffer-file-name file)
                       (let ((other (concat file ".new")))
                         (with-temp-file other (insert bytes))
                         (rename-file other file t)
                         (setq changed t)))))))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (should-error (gnosis-nodes-delete-file file) :type 'user-error)))
      (should changed)
      (should (equal rows (gnosis-select '* 'journal)))
      (should (equal bytes (with-temp-buffer
                            (insert-file-contents file) (buffer-string)))))))

(ert-deftest gnosis-test-campaign-nodes-delete-controls ()
  "Unchanged confirmation deletes; decline and quit retain file and rows."
  (dolist (answer '(yes no quit))
    (gnosis-test-node-selection--with-vault
      (let* ((file (gnosis-test-node-selection--file "node.org" "old" "Old"))
             (rows (gnosis-select '* 'nodes)))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (if (eq answer 'quit) (signal 'quit nil) (eq answer 'yes)))))
          (if (eq answer 'quit)
              (should (eq 'quit (condition-case nil
                                   (gnosis-nodes-delete-file file)
                                 (quit 'quit))))
            (gnosis-nodes-delete-file file)))
        (if (eq answer 'yes)
            (progn (should-not (file-exists-p file))
                   (should-not (gnosis-select '* 'nodes)))
          (should (file-exists-p file))
          (should (equal rows (gnosis-select '* 'nodes))))))))

(ert-deftest gnosis-test-campaign-nodes-narrowed-owner ()
  "Tag and backlinks resolve outside a restriction without moving its origin."
  (dolist (parent '(nil t))
    (gnosis-test-node-selection--with-vault
      (let ((file (gnosis-test-node-selection--file
                   "tags.org" "root" "Root" nil
                   (concat "* Parent\n"
                           (when parent ":PROPERTIES:\n:ID: parent\n:END:\n")
                           "** Child\nBody stays\n* Sibling\nUnrelated\n"))))
        (find-file file)
        (search-forward "** Child") (org-narrow-to-subtree)
        (let ((text (buffer-string)) (offset (- (point) (point-min))))
          (should (equal (gnosis-org-get-id) (if parent "parent" "root")))
          (cl-letf (((symbol-function 'completing-read-multiple)
                     (lambda (&rest _) '("added"))))
            (call-interactively (key-binding (kbd "C-c C-q"))))
          (should (buffer-narrowed-p))
          (should (equal text (buffer-string)))
          (should (= offset (- (point) (point-min))))
          (gnosis-nodes-visit-backlinks)
          (save-buffer)
          (should (member "added"
                          (gnosis-select 'tag 'node-tag
                                         `(= node-id ,(if parent "parent" "root")) t)))
          (save-restriction
            (widen) (goto-char (point-min))
            (search-forward "* Sibling")
            (should-not (org-get-tags nil t)))
          (revert-buffer t t)
          (should (string-match-p "Body stays" (buffer-string))))))))

(ert-deftest gnosis-test-campaign-nodes-rootless-owner ()
  "A parentless level-two heading terminates without inventing an ID."
  (with-temp-buffer
    (org-mode) (insert "#+title: Rootless\n** Child\nBody\n")
    (goto-char (point-max))
    (should-not (gnosis-org-get-id))))

(ert-deftest gnosis-test-campaign-nodes-ambiguous-title ()
  "Raw duplicate titles cannot navigate, create or insert an arbitrary ID."
  (gnosis-test-node-selection--with-vault
    (gnosis-test-node-selection--file "one.org" "one" "Shared")
    (gnosis-test-node-selection--file "two.org" "two" "Shared")
    (with-temp-buffer
      (org-mode) (insert "Origin")
      (let ((gnosis-nodes-completing-read-func (lambda (&rest _) "Shared"))
            (origin (current-buffer)))
        (should-error (gnosis-nodes-find) :type 'user-error)
        (should (eq origin (current-buffer)))
        (should-error (gnosis-nodes-find "Shared") :type 'user-error)
        (should-error (gnosis-nodes-insert nil) :type 'user-error)
        (should (equal "Origin" (buffer-string)))
        (should (= 2 (length (gnosis-nodes--files))))))
    (dolist (id '("one" "two"))
      (let ((gnosis-nodes-completing-read-func
             (lambda (_prompt choices)
               (seq-find (lambda (label) (string-match-p (concat id ".org") label))
                         choices))))
        (gnosis-nodes-find)
        (should (equal id (org-id-get)))))))

(ert-deftest gnosis-test-campaign-nodes-physical-alias ()
  "Sync retains the indexed physical owner through aliases and forced rebuilds."
  (dolist (kind '(symlink hardlink))
    (gnosis-test-node-selection--with-vault
      (let* ((file (gnosis-test-node-selection--file "z-source.org" "root" "Root"))
             (alias (expand-file-name "a-alias.org" gnosis-nodes-dir))
             (buffer (find-file-noselect file)))
        (if (eq kind 'symlink) (make-symbolic-link file alias)
          (add-name-to-file file alias))
        (gnosis-nodes-db-sync)
        (should (equal '("z-source.org") (gnosis-select 'file 'nodes nil t)))
        (with-current-buffer buffer
          (goto-char (point-max)) (insert "Unsaved draft\n")
          (let ((text (buffer-string)) (pos (point)))
            (gnosis-nodes-db-sync t)
            (should (equal text (buffer-string)))
            (should (= pos (point)))
            (should (buffer-modified-p))))
        (should (equal '("z-source.org") (gnosis-select 'file 'nodes nil t)))
        (gnosis-nodes-goto-id "root")
        (should (eq buffer (current-buffer)))
        (should (equal "root" (org-id-get)))
        ;; Native alias save must keep the same indexed owner as well.
        (gnosis-nodes-update-file alias)
        (should (equal '("z-source.org") (gnosis-select 'file 'nodes nil t)))
        (copy-file file (expand-file-name "copy.org" gnosis-nodes-dir))
        (let ((before (gnosis-select '* 'nodes)))
          (dolist (force '(nil t))
            (should-error (gnosis-nodes-db-sync force))
            (should (equal before (gnosis-select '* 'nodes)))))))))

(ert-deftest gnosis-test-campaign-nodes-journal-force-owner ()
  "Forced rebuild uses physical live text; ordinary sync uses saved bytes."
  (dolist (kind '(canonical symlink hardlink))
    (gnosis-test-journal-selection--with-files
      (pcase-let* ((`(,file ,_alias)
                    (gnosis-test-journal-index-alias--seed
                     (if (eq kind 'canonical) 'symlink kind) t)))
        (when (eq kind 'canonical) (setq gnosis-journal-file file))
        (find-file file) (gnosis-nodes-mode 1)
        (goto-char (point-min)) (search-forward "2001-02-03")
        (replace-match "2001-02-05") (org-narrow-to-subtree)
        (let ((text (buffer-string)) (pos (point))
              (bounds (cons (point-min) (point-max))))
          (gnosis-nodes-db-sync)
          (should (equal '("2001-02-03") (gnosis-select 'title 'journal '(= id "day") t)))
          (gnosis-nodes-db-sync t)
          (should (equal '("2001-02-05") (gnosis-select 'title 'journal '(= id "day") t)))
          (should (equal bounds (cons (point-min) (point-max))))
          (should (equal text (buffer-string))) (should (= pos (point)))
          (should (buffer-modified-p))
          (gnosis-sqlite-close gnosis-db)
          (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (should (equal '("2001-02-05") (gnosis-select 'title 'journal '(= id "day") t))))))))

(provide 'gnosis-test-campaign-nodes)
;;; gnosis-test-campaign-nodes.el ends here
