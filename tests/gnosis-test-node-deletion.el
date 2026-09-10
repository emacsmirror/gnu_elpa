;;; gnosis-test-node-deletion.el --- Node deletion boundaries -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Exercise one-target deletion and the filesystem/index failure boundary.

;;; Code:

(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-nodes)

(defmacro gnosis-test-node-deletion--with-files (&rest body)
  "Run BODY with two indexed files A and B and their visiting buffers."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-db
     (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
            (gnosis-journal-file nil)
            (org-id-track-globally nil)
            (a (expand-file-name "a.org" gnosis-nodes-dir))
            (b (expand-file-name "b.org" gnosis-nodes-dir)))
       (make-directory gnosis-nodes-dir)
       (dolist (entry (list (cons a "a") (cons b "b")))
         (with-temp-file (car entry)
           (insert (format ":PROPERTIES:\n:ID: node-%s\n:END:\n#+title: %s\n#+filetags: :kept:\n[[id:node-%s]]\n"
                           (cdr entry) (cdr entry) (cdr entry))))
         (gnosis-nodes-update-file (car entry)))
       (let ((ab (find-file-noselect a))
             (bb (find-file-noselect b)))
         (unwind-protect
             (progn ,@body)
           (dolist (buffer (list ab bb))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (set-buffer-modified-p nil)
                 (let ((kill-buffer-query-functions nil))
                   (kill-buffer buffer))))))))))

(defun gnosis-test-node-deletion--index ()
  "Return the complete node, tag and link index."
  (mapcar (lambda (table) (gnosis-select '* table))
          '(nodes node-tag node-links)))

(ert-deftest gnosis-test-node-deletion-explicit-target ()
  "Noncurrent and nonfile callers delete only the explicit target."
  (dolist (nonfile '(nil t))
    (gnosis-test-node-deletion--with-files
      (with-temp-buffer
        (with-current-buffer (if nonfile (current-buffer) ab)
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (prompt)
                       (should (string-match-p (regexp-quote b) prompt)) t)))
            (gnosis-nodes-delete-file b))))
      (should (file-exists-p a))
      (should (buffer-live-p ab))
      (should (gnosis-get 'id 'nodes '(= id "node-a")))
      (should (gnosis-select '* 'node-tag '(= node-id "node-a")))
      (should (gnosis-select '* 'node-links '(= source "node-a")))
      (should-not (file-exists-p b))
      (should-not (buffer-live-p bb))
      (should-not (gnosis-get 'id 'nodes '(= id "node-b")))
      (should-not (gnosis-select '* 'node-tag '(= node-id "node-b")))
      (should-not (gnosis-select '* 'node-links '(= source "node-b"))))))

(ert-deftest gnosis-test-node-deletion-cancel ()
  "Declined confirmation and prompt quit preserve both files and index."
  (dolist (quit '(nil t))
    (gnosis-test-node-deletion--with-files
      (let ((before (gnosis-test-node-deletion--index)))
        (with-current-buffer ab
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (when quit (signal 'quit nil)) nil)))
            (condition-case nil (call-interactively #'gnosis-nodes-delete-file)
              (quit nil))))
        (should (equal before (gnosis-test-node-deletion--index)))
        (should (file-exists-p a))
        (should (file-exists-p b))
        (should (buffer-live-p ab))
        (should (buffer-live-p bb))))))

(ert-deftest gnosis-test-node-deletion-pre-effect-fault ()
  "File errors and quits leave the entire old index intact."
  (dolist (fault '(error quit))
    (gnosis-test-node-deletion--with-files
      (let ((before (gnosis-test-node-deletion--index)))
        (with-current-buffer ab
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'delete-file)
                     (lambda (&rest _) (signal fault '("File fault")))))
            (condition-case nil (call-interactively #'gnosis-nodes-delete-file)
              (error nil) (quit nil))))
        (should (equal before (gnosis-test-node-deletion--index)))
        (should (file-exists-p a))
        (should (buffer-live-p ab))))))

(ert-deftest gnosis-test-node-deletion-post-effect-sql-retry ()
  "A real SQL fault reports partial deletion and permits index-only retry."
  (gnosis-test-node-deletion--with-files
    (let ((before (gnosis-test-node-deletion--index)))
      (sqlite-execute gnosis-db
                      "CREATE TRIGGER fail_delete BEFORE DELETE ON node_tag BEGIN SELECT RAISE(ABORT, 'index fault'); END")
      (with-current-buffer ab
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (let ((err (should-error (call-interactively #'gnosis-nodes-delete-file))))
            (should (string-match-p "reconcil" (error-message-string err))))))
      (should-not (file-exists-p a))
      (should (buffer-live-p ab))
      (should (equal before (gnosis-test-node-deletion--index)))
      (sqlite-execute gnosis-db "DROP TRIGGER fail_delete")
      (with-current-buffer ab
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (prompt) (should (string-match-p "index" prompt)) t))
                  ((symbol-function 'delete-file)
                   (lambda (&rest _) (ert-fail "Retry must not delete another file"))))
          (call-interactively #'gnosis-nodes-delete-file)))
      (should-not (gnosis-get 'id 'nodes '(= id "node-a")))
      (should-not (buffer-live-p ab))
      (should (file-exists-p b))
      (should (buffer-live-p bb))
      (should (gnosis-get 'id 'nodes '(= id "node-b"))))))

(ert-deftest gnosis-test-node-deletion-unvisited-target ()
  "A nonfile caller can remove a target with no visiting buffer."
  (gnosis-test-node-deletion--with-files
    (kill-buffer bb)
    (with-temp-buffer
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (gnosis-nodes-delete-file b)))
    (should-not (file-exists-p b))
    (should-not (gnosis-get 'id 'nodes '(= id "node-b")))
    (should (file-exists-p a))
    (should (buffer-live-p ab))))

(ert-deftest gnosis-test-node-deletion-post-effect-file-fault ()
  "A file handler error or quit after deletion has an index-only retry."
  (dolist (fault '(error quit))
    (gnosis-test-node-deletion--with-files
      (let ((before (gnosis-test-node-deletion--index))
            (delete (symbol-function 'delete-file)))
        (with-current-buffer ab
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'delete-file)
                     (lambda (&rest args)
                       (apply delete args)
                       (signal fault '("After physical deletion")))))
            (condition-case nil (call-interactively #'gnosis-nodes-delete-file)
              (error nil) (quit nil))))
        (should-not (file-exists-p a))
        (should (equal before (gnosis-test-node-deletion--index)))
        (should (buffer-live-p ab))
        (with-current-buffer ab
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (call-interactively #'gnosis-nodes-delete-file)))
        (should-not (gnosis-get 'id 'nodes '(= id "node-a")))
        (should (gnosis-get 'id 'nodes '(= id "node-b")))))))

(ert-deftest gnosis-test-node-deletion-post-effect-sql-quit ()
  "A quit after an index write rolls back SQL and permits cleanup retry."
  (gnosis-test-node-deletion--with-files
    (let ((before (gnosis-test-node-deletion--index))
          (delete (symbol-function 'gnosis-nodes--delete)))
      (with-current-buffer ab
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'gnosis-nodes--delete)
                   (lambda (&rest args)
                     (apply delete args)
                     (signal 'quit nil))))
          (should (eq (condition-case nil
                          (call-interactively #'gnosis-nodes-delete-file)
                        (quit 'interrupted))
                      'interrupted))))
      (should-not (file-exists-p a))
      (should (equal before (gnosis-test-node-deletion--index)))
      (should (buffer-live-p ab))
      (with-current-buffer ab
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (call-interactively #'gnosis-nodes-delete-file)))
      (should-not (gnosis-get 'id 'nodes '(= id "node-a")))
      (should (gnosis-get 'id 'nodes '(= id "node-b"))))))

(ert-deftest gnosis-test-node-deletion-modified-buffer-refusal ()
  "A refused buffer kill preserves edits without silently recreating FILE."
  (gnosis-test-node-deletion--with-files
    (with-current-buffer ab
      (goto-char (point-max))
      (insert "Unsaved recovery text\n")
      (setq-local kill-buffer-query-functions (list (lambda () nil)))
      (let ((text (buffer-string))
            (mode major-mode))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (call-interactively #'gnosis-nodes-delete-file))
        (should (buffer-live-p ab))
        (should-not (buffer-file-name))
        (should (buffer-modified-p))
        (should (eq mode major-mode))
        (should (equal text (buffer-string)))
        (should-not (file-exists-p a))
        (should-not (gnosis-get 'id 'nodes '(= id "node-a")))
        ;; Native save must request a destination, not reuse the deleted path.
        (cl-letf (((symbol-function 'read-file-name)
                   (lambda (&rest _) (signal 'quit nil))))
          (condition-case nil (save-buffer) (quit nil)))
        (should-not (file-exists-p a))))))

(ert-deftest gnosis-test-node-deletion-recovery-save-as ()
  "Reconciliation must not reclaim a recovery buffer saved elsewhere."
  (gnosis-test-node-deletion--with-files
    (let ((recovered (expand-file-name "recovered.org" gnosis-nodes-dir))
          text)
      (sqlite-execute gnosis-db
                      "CREATE TRIGGER fail_delete BEFORE DELETE ON node_tag BEGIN SELECT RAISE(ABORT, 'index fault'); END")
      (with-current-buffer ab
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (should-error (gnosis-nodes-delete-file)))
        (sqlite-execute gnosis-db "DROP TRIGGER fail_delete")
        ;; The retained ID can make the index hook refuse after bytes were
        ;; saved.  Either way, native write-file has reassociated the buffer.
        (condition-case nil (write-file recovered) (error nil))
        (should (equal buffer-file-name recovered))
        (should (file-exists-p recovered))
        (goto-char (point-max))
        (insert "Unsaved successor text\n")
        (setq text (buffer-string)))
      (with-temp-buffer
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (gnosis-nodes-delete-file a)))
      (should-not (gnosis-get 'id 'nodes '(= id "node-a")))
      (should (file-exists-p recovered))
      (should (buffer-live-p ab))
      (with-current-buffer ab
        (should (equal buffer-file-name recovered))
        (should (equal text (buffer-string)))
        (should (buffer-modified-p)))
      (should (file-exists-p b))
      (should (buffer-live-p bb))
      (should (gnosis-get 'id 'nodes '(= id "node-b"))))))

(ert-deftest gnosis-test-node-deletion-recovery-owner ()
  "A detached recovery buffer cannot clean up a replacement database."
  (gnosis-test-node-deletion--with-files
    (with-current-buffer ab
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'gnosis-nodes--delete-file)
                 (lambda (&rest _) (error "SQL unavailable"))))
        (should-error (gnosis-nodes-delete-file)))
      (should-not (buffer-file-name))
      (should (equal (car gnosis-nodes--deleted-file) a))
      (gnosis-test-with-db
        (should-error (gnosis-nodes-delete-file) :type 'user-error)))))

(ert-deftest gnosis-test-node-deletion-transaction-refusal ()
  "Do not put irreversible deletion inside a caller's transaction."
  (gnosis-test-node-deletion--with-files
    (gnosis-sqlite-with-transaction gnosis-db
      (with-current-buffer ab
        (should-error (gnosis-nodes-delete-file) :type 'user-error)))
    (should (file-exists-p a))
    (should (gnosis-get 'id 'nodes '(= id "node-a")))))

(ert-deftest gnosis-test-node-deletion-invalid-target ()
  "Nonfile default and explicit outside paths fail without effects."
  (gnosis-test-node-deletion--with-files
    (let ((before (gnosis-test-node-deletion--index)))
      (with-temp-buffer
        (should-error (gnosis-nodes-delete-file) :type 'user-error))
      (with-current-buffer ab
        (should-error (gnosis-nodes-delete-file
                       (expand-file-name "outside.org" gnosis-dir))
                      :type 'user-error))
      (should (equal before (gnosis-test-node-deletion--index)))
      (should (file-exists-p a))
      (should (file-exists-p b)))))

(provide 'gnosis-test-node-deletion)
;;; gnosis-test-node-deletion.el ends here
