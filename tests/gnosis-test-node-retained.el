;;; gnosis-test-node-retained.el --- Retained node owners -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Exercise pre-rebuild basename rows through the former native save hook.

;;; Code:

(require 'gnosis-test-node-paths)

(defun gnosis-test-node-retained--save (file)
  "Save FILE through the native basename-index writer.
When GNOSIS_TEST_BASENAME_NODES names the old module, load that module
for independent compatibility proof.  Otherwise use the former filename
policy with the compatibility guard disabled in the native save pipeline."
  (let ((source (symbol-file 'gnosis-nodes-update-file))
        (base (getenv "GNOSIS_TEST_BASENAME_NODES")))
    (unwind-protect
        (if base
            (progn (load base nil t)
                   (gnosis-test-node-retained--native-save file))
          (cl-letf (((symbol-function 'gnosis-nodes--file-key)
                     (lambda (name journal)
                       (if journal
                           (file-relative-name (expand-file-name name)
                                               (gnosis-nodes--journal-dir))
                         (file-name-nondirectory name))))
                    ;; The released writer predates the compatibility guard.
                    ((symbol-function 'gnosis-nodes--check-node-ownership) #'ignore))
            (gnosis-test-node-retained--native-save file)))
      (when base (load source nil t)))))

(defun gnosis-test-node-retained--native-save (file)
  "Visit FILE and index it via the real mode's save hook."
  (with-current-buffer (find-file-noselect file)
    (should gnosis-nodes-mode)
    (goto-char (point-max))
    (insert "Native saved.\n")
    (call-interactively #'save-buffer)))

(defun gnosis-test-node-retained--refused (action)
  "Assert ACTION refuses with explicit index-only rebuild guidance."
  (let ((err (should-error (funcall action) :type 'user-error)))
    (should (string-match-p "gnosis-nodes-db-force-sync"
                            (error-message-string err)))))

(ert-deftest gnosis-test-node-retained-save-sync-navigation ()
  "Retained descendants refuse safely before rebuild, then work normally."
  (dolist (namesake '(nil root descendant))
    (gnosis-test-node-paths--with-vault
      (delete-file j)
      (unless (eq namesake 'root) (delete-file a))
      (when (eq namesake 'descendant)
        (setq a (expand-file-name "other/same.org" gnosis-nodes-dir))
        (make-directory (file-name-directory a) t)
        (with-temp-file a
          (insert ":PROPERTIES:\n:ID: other\n:END:\n#+title: Other\n")))
      (gnosis-test-node-retained--save b)
      (should (equal '(("nested" "same.org")) (gnosis-select '[id file] 'nodes)))
      (let ((before (gnosis-test-node-paths--index))
            (origin (current-buffer)))
        (gnosis-test-node-retained--refused
         (lambda () (gnosis-test-node-retained--native-save b)))
        (should (string-suffix-p "Native saved.\nNative saved.\n"
                                (gnosis-test-node-paths--bytes b)))
        (gnosis-test-node-retained--refused #'gnosis-nodes-db-sync)
        (should (member "nested" (gnosis-nodes-search-content "Needle nested")))
        (gnosis-test-node-retained--refused
         (lambda () (gnosis-nodes-goto-id "nested")))
        (should (eq origin (current-buffer)))
        (should (equal before (gnosis-test-node-paths--index))))
      (let ((bytes (gnosis-test-node-paths--bytes b)))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (call-interactively #'gnosis-nodes-db-force-sync))
        (should (equal bytes (gnosis-test-node-paths--bytes b)))
        (gnosis-test-node-retained--native-save b)
        (gnosis-nodes-db-sync)
        (gnosis-nodes-goto-id "nested")
        (should (equal buffer-file-name b))))))

(ert-deftest gnosis-test-node-retained-delete-refusal-and-recovery ()
  "Deletion must refuse before removing bytes or consuming another owner."
  (gnosis-test-node-paths--with-vault
    (delete-file j)
    (gnosis-test-node-retained--save b)
    (let ((before (gnosis-test-node-paths--index))
          (bytes (mapcar #'gnosis-test-node-paths--bytes (list a b))))
      (dolist (file (list a b))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
          (gnosis-nodes-delete-file file))
        (let ((quit (catch 'cancel
                      (cl-letf (((symbol-function 'y-or-n-p)
                                 (lambda (&rest _) (signal 'quit nil))))
                        (condition-case nil (gnosis-nodes-delete-file file)
                          (quit (throw 'cancel t)))))))
          (should quit))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'delete-file)
                   (lambda (&rest _) (ert-fail "Deletion preceded ownership check"))))
          (gnosis-test-node-retained--refused
           (lambda () (gnosis-nodes-delete-file file)))))
      (should (equal before (gnosis-test-node-paths--index)))
      (should (equal bytes (mapcar #'gnosis-test-node-paths--bytes (list a b)))))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (gnosis-nodes-db-force-sync)
      (gnosis-nodes-delete-file b)
      (gnosis-nodes-delete-file b))
    (should-not (file-exists-p b))
    (should (equal '("root") (gnosis-select 'id 'nodes nil t)))
    (should (equal '(("root" "kept")) (gnosis-select '* 'node-tag)))))

(ert-deftest gnosis-test-node-retained-replaced-ids ()
  "Removed IDs and their stale tags/links survive refusal until rebuild."
  (gnosis-test-node-paths--with-vault
    (delete-file j)
    (with-temp-file b
      (insert ":PROPERTIES:\n:ID: nested\n:END:\n#+title: Nested\n#+filetags: :kept:\n[[id:root]]\n* Old\n:PROPERTIES:\n:ID: removed\n:END:\n"))
    (gnosis-test-node-retained--save b)
    (let ((before (gnosis-test-node-paths--index)))
      (with-current-buffer (find-file-noselect b)
        (erase-buffer)
        (insert ":PROPERTIES:\n:ID: replacement\n:END:\n#+title: New\n")
        (gnosis-test-node-retained--refused
         (lambda () (call-interactively #'save-buffer))))
      (gnosis-test-node-retained--refused #'gnosis-nodes-db-sync)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (gnosis-test-node-retained--refused
         (lambda () (gnosis-nodes-delete-file b))))
      (should (equal before (gnosis-test-node-paths--index))))
    (gnosis-nodes-db-sync t)
    (should (equal '("replacement" "root")
                   (sort (gnosis-select 'id 'nodes nil t) #'string<)))
    (should-not (gnosis-select '* 'node-tag '(= node-id "removed")))
    (should-not (gnosis-select '* 'node-links '(= source "nested")))))

(ert-deftest gnosis-test-node-retained-missing-descendant ()
  "A missing legacy descendant must not leave an index-success receipt."
  (gnosis-test-node-paths--with-vault
    (gnosis-test-node-retained--save b)
    (let ((before (gnosis-test-node-paths--index))
          (buffer (get-file-buffer b)))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (delete-file b)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (gnosis-test-node-retained--refused
         (lambda () (gnosis-nodes-delete-file b))))
      (should (equal before (gnosis-test-node-paths--index))))
    (gnosis-nodes-db-sync t)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (gnosis-nodes-delete-file b))
    (should (equal '("root") (gnosis-select 'id 'nodes nil t)))))

(ert-deftest gnosis-test-node-retained-root-save-preserves-snapshots ()
  "Recognized old root snapshots remain usable without a forced rebuild."
  (gnosis-test-node-paths--with-vault
    (with-temp-file a
      (insert ":PROPERTIES:\n:ID: root\n:END:\n#+title: Root\n#+filetags: :kept:\n* Old\n:PROPERTIES:\n:ID: removed\n:END:\n"))
    (gnosis-test-node-retained--save a)
    (gnosis-nodes-update-file b)
    (let* ((c (expand-file-name "unrelated.org" gnosis-nodes-dir))
           (nested (gnosis-select '* 'nodes '(= id "nested"))))
      (with-temp-file c
        (insert ":PROPERTIES:\n:ID: source\n:END:\n#+title: Source\n#+filetags: :source:\n[[id:root]] [[id:nested]]\n"))
      (gnosis-nodes-update-file c)
      (let ((source (gnosis-select '* 'nodes '(= id "source")))
            (incoming (gnosis-select '* 'node-links '(= source "source"))))
        (with-current-buffer (find-file-noselect a)
          (goto-char (point-min))
          (search-forward "* Old")
          (beginning-of-line)
          (delete-region (point) (point-max))
          (call-interactively #'save-buffer))
        (should-not (gnosis-select '* 'nodes '(= id "removed")))
        (should (equal nested (gnosis-select '* 'nodes '(= id "nested"))))
        (should (equal source (gnosis-select '* 'nodes '(= id "source"))))
        (should (equal incoming (gnosis-select '* 'node-links '(= source "source"))))
        (gnosis-nodes-db-sync)
        (gnosis-nodes-goto-id "root")
        (should (equal buffer-file-name a))))))

(ert-deftest gnosis-test-node-retained-recovery-faults ()
  "A recognized retained root keeps filesystem/SQL recovery without rebuild."
  (dolist (fault '(error quit))
    (gnosis-test-node-paths--with-vault
      (gnosis-test-node-retained--save a)
      (gnosis-nodes-update-file b)
      (switch-to-buffer (find-file-noselect a))
      (let ((before (gnosis-test-node-paths--index))
            (nested-bytes (gnosis-test-node-paths--bytes b)))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'delete-file)
                   (lambda (&rest _) (signal fault '("File fault")))))
          (condition-case err (gnosis-nodes-delete-file)
            ((error quit) (should (eq (car err) fault)))))
        (should (file-exists-p a))
        (should (equal before (gnosis-test-node-paths--index)))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'gnosis-nodes--delete-file)
                   (lambda (&rest _) (signal fault '("SQL fault")))))
          (condition-case err (gnosis-nodes-delete-file)
            ((error quit) (should (eq (car err) fault)))))
        (should-not (file-exists-p a))
        (should-not buffer-file-name)
        (should (equal before (gnosis-test-node-paths--index)))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (call-interactively #'gnosis-nodes-delete-file))
        (should (equal '("nested") (gnosis-select 'id 'nodes nil t)))
        (should (equal '(("nested" "kept")) (gnosis-select '* 'node-tag)))
        (should (equal nested-bytes (gnosis-test-node-paths--bytes b)))))))

(ert-deftest gnosis-test-node-retained-successor-ownership ()
  "After explicit rebuild, a descendant recovery cannot own its successor."
  (gnosis-test-node-paths--with-vault
    (gnosis-test-node-retained--save b)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (gnosis-test-node-retained--refused
       (lambda () (gnosis-nodes-delete-file b))))
    (gnosis-nodes-db-sync t)
    (let ((buffer (find-file-noselect b))
          (successor (expand-file-name "successor.org" gnosis-nodes-dir)))
      (with-current-buffer buffer
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'gnosis-nodes--delete-file)
                   (lambda (&rest _) (error "SQL fault"))))
          (should-error (gnosis-nodes-delete-file)))
        (should-not buffer-file-name)
        ;; Preserve the old ID: the successor save can still hit the old
        ;; UNIQUE row until explicit missing-original cleanup succeeds.
        (condition-case nil (write-file successor) (error nil))
        (goto-char (point-max))
        (insert "Unsaved successor\n"))
      (let ((text (with-current-buffer buffer (buffer-string))))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (gnosis-nodes-delete-file b))
        (should (buffer-live-p buffer))
        (with-current-buffer buffer
          (should (equal buffer-file-name successor))
          (should (equal text (buffer-string)))
          (should (buffer-modified-p))
          (call-interactively #'save-buffer))
        (should (equal "successor.org" (gnosis-get 'file 'nodes '(= id "nested"))))))))

(ert-deftest gnosis-test-node-retained-root-replaced-ids ()
  "Replacing all root IDs requires explicit reconciliation, not a guess."
  (gnosis-test-node-paths--with-vault
    (gnosis-test-node-retained--save a)
    (let ((before (gnosis-test-node-paths--index)))
      (with-current-buffer (find-file-noselect a)
        (erase-buffer)
        (insert ":PROPERTIES:\n:ID: new-root\n:END:\n#+title: New root\n")
        (gnosis-test-node-retained--refused
         (lambda () (call-interactively #'save-buffer))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (gnosis-test-node-retained--refused
         (lambda () (gnosis-nodes-delete-file a))))
      (should (file-exists-p a))
      (should (equal before (gnosis-test-node-paths--index))))
    (gnosis-nodes-db-sync t)
    (gnosis-nodes-goto-id "new-root")
    (should (equal buffer-file-name a))
    (should-not (gnosis-select '* 'nodes '(= id "root")))))

(defmacro gnosis-test-node-retained--with-journal (relative &rest body)
  "Run BODY with retained journal B and ordinary namesake C at RELATIVE.
Keep unrelated source D, its tags, incoming/outgoing links, and TODO bytes."
  (declare (indent 1) (debug t))
  `(gnosis-test-node-paths--with-vault
     (delete-file a)
     (delete-file j)
     (let* ((c (expand-file-name ,relative gnosis-nodes-dir))
            (d (expand-file-name "source.org" gnosis-nodes-dir))
            (gnosis-journal-todo-files (list d)))
       (with-temp-file b
         (insert ":PROPERTIES:\n:ID: nested\n:END:\n#+title: Journal\n#+filetags: :kept:\n[[id:source]]\n* Goals\n+ [X] Exercise\n"))
       (with-temp-file d
         (insert ":PROPERTIES:\n:ID: source\n:END:\n#+title: Source\n#+filetags: :source:\n[[id:nested]]\n* TODO Exercise\n"))
       (gnosis-test-node-retained--save b)
       (gnosis-test-node-retained--save d)
       (should (equal "same.org" (gnosis-get 'file 'nodes '(= id "nested"))))
       ;; Change configuration only after the real former native save.  Do not
       ;; sync/rebuild first: that would hide the retained-owner transition.
       (setq gnosis-journal-file b)
       (make-directory (file-name-directory c) t)
       (with-temp-file c
         (insert ":PROPERTIES:\n:ID: other\n:END:\n#+title: Other\n#+filetags: :ordinary:\n[[id:nested]]\n"))
       (let ((journal-bytes (gnosis-test-node-paths--bytes b))
             (source-bytes (gnosis-test-node-paths--bytes d))
             (source-row (gnosis-select '* 'nodes '(= id "source")))
             (source-tags (gnosis-select '* 'node-tag '(= node-id "source")))
             (links (gnosis-select '* 'node-links '(= source "source"))))
         ,@body
         (should (equal journal-bytes (gnosis-test-node-paths--bytes b)))
         (should (equal source-bytes (gnosis-test-node-paths--bytes d)))
         (should (equal source-row (gnosis-select '* 'nodes '(= id "source"))))
         (should (equal source-tags (gnosis-select '* 'node-tag '(= node-id "source"))))
         (dolist (link links) (should (member link (gnosis-select '* 'node-links))))))))

(defun gnosis-test-node-retained--journal-adopted ()
  "Assert the retained journal is adopted with its tags, not ordinary rows."
  (should-not (gnosis-select 'id 'nodes '(= id "nested")))
  (should (equal '(("nested" "../sub/same.org"))
                 (gnosis-select '[id file] 'journal)))
  ;; Journal tags live in the row, not the nodes-only junction table.
  (should (equal '("kept")
                 (read (gnosis-get 'tags 'journal '(= id "nested"))))))

(ert-deftest gnosis-test-node-retained-journal-namesake-save ()
  "Native namesake saves adopt recognized journals without TODO effects."
  (dolist (relative '("same.org" "other/same.org"))
    (gnosis-test-node-retained--with-journal relative
      (cl-letf (((symbol-function 'gnosis-journal--update-todos)
                 (lambda (&rest _) (ert-fail "Index adoption ran TODO hook"))))
        (gnosis-test-node-retained--native-save c))
      (should (equal relative (gnosis-get 'file 'nodes '(= id "other"))))
      (gnosis-test-node-retained--journal-adopted))))

(ert-deftest gnosis-test-node-retained-journal-namesake-delete ()
  "Public deletion adopts a known journal before any prior save or rebuild."
  (dolist (relative '("same.org" "other/same.org"))
    (gnosis-test-node-retained--with-journal relative
      (switch-to-buffer (find-file-noselect c))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'gnosis-journal--update-todos)
                 (lambda (&rest _) (ert-fail "Index adoption ran TODO hook"))))
        (call-interactively #'gnosis-nodes-delete-file))
      (should-not (file-exists-p c))
      (should (equal '("source") (gnosis-select 'id 'nodes nil t)))
      (gnosis-test-node-retained--journal-adopted))))

(ert-deftest gnosis-test-node-retained-journal-namesake-cancel ()
  "Declined and quit deletion leave recognized retained owners untouched."
  (dolist (relative '("same.org" "other/same.org"))
    (gnosis-test-node-retained--with-journal relative
      (switch-to-buffer (find-file-noselect c))
      (let ((before (gnosis-test-node-paths--index))
            (bytes (gnosis-test-node-paths--bytes c)))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
          (call-interactively #'gnosis-nodes-delete-file))
        (should (condition-case nil
                    (cl-letf (((symbol-function 'y-or-n-p)
                               (lambda (&rest _) (signal 'quit nil))))
                      (call-interactively #'gnosis-nodes-delete-file)
                      nil)
                  (quit t)))
        (should (equal before (gnosis-test-node-paths--index)))
        (should (equal bytes (gnosis-test-node-paths--bytes c)))
        (should (equal c buffer-file-name))))))

(ert-deftest gnosis-test-node-retained-journal-namesake-storage-retry ()
  "Real SQL failures roll back journal adoption for save and deletion retry."
  (dolist (relative '("same.org" "other/same.org"))
    (dolist (action '(save delete))
      (gnosis-test-node-retained--with-journal relative
        (switch-to-buffer (find-file-noselect c))
        (let ((before (gnosis-test-node-paths--index))
              (buffer (current-buffer)))
          (sqlite-execute gnosis-db
                          "CREATE TRIGGER fail_adoption AFTER INSERT ON journal BEGIN SELECT RAISE(ABORT, 'journal adoption fault'); END")
          (let ((err
                 (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                   (should-error
                    (if (eq action 'save)
                        (gnosis-test-node-retained--native-save c)
                      (call-interactively #'gnosis-nodes-delete-file))))))
            (should (string-match-p "journal adoption fault" (error-message-string err))))
          (should (equal before (gnosis-test-node-paths--index)))
          (if (eq action 'save)
              (progn
                (should (file-exists-p c))
                (should-not (buffer-modified-p))
                (should (string-suffix-p "Native saved.\n" (gnosis-test-node-paths--bytes c))))
            (should-not (file-exists-p c))
            (should-not buffer-file-name)
            (should (equal c (car gnosis-nodes--deleted-file)))
            (should (string-match-p "ID: other" (buffer-string))))
          (sqlite-execute gnosis-db "DROP TRIGGER fail_adoption")
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (if (eq action 'save)
                (gnosis-test-node-retained--native-save c)
              (call-interactively #'gnosis-nodes-delete-file)))
          (if (eq action 'save)
              (should (equal relative (gnosis-get 'file 'nodes '(= id "other"))))
            (should-not (file-exists-p c))
            (should-not (buffer-live-p buffer)))
          (gnosis-test-node-retained--journal-adopted))))))

(provide 'gnosis-test-node-retained)
;;; gnosis-test-node-retained.el ends here
