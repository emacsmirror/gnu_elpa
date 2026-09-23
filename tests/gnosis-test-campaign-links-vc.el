;;; gnosis-test-campaign-links-vc.el --- Link and commit boundaries -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Regress identity selection, nullable link repair and real Git isolation.

;;; Code:

(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-links)
(require 'gnosis-nodes)
(require 'gnosis-test-vc)

(defun gnosis-test-campaign-links--nodes ()
  "Install duplicate and literal label-like node titles."
  (gnosis--insert-into
   'nodes '(["one" "one.org" "Shared" 0 nil nil nil]
            ["two" "two.org" "Shared" 0 nil nil nil]
            ["literal" "literal.org" "Shared — one.org" 0 nil nil nil]
            ["unique" "unique.org" "Unique" 0 nil nil nil])))

(ert-deftest gnosis-test-campaign-links-picker-identities ()
  (dolist (target '("one" "two" "literal" "unique"))
    (gnosis-test-with-db
      (gnosis-test-campaign-links--nodes)
      (let* ((id (gnosis-test--add-basic-thema "Needle" "Answer"))
             (rows (gnosis-select '[id title file] 'nodes))
             (candidates (gnosis-nodes--completion-candidates rows)))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Needle"))
                  ((symbol-function 'gnosis-completing-read)
                   (lambda (_prompt choices &rest _)
                     (should (= (length (delete-dups (copy-sequence choices))) 4))
                     (car (rassoc target candidates))))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (call-interactively #'gnosis-bulk-link-string))
        (should (equal (gnosis-select '[source dest] 'thema-links)
                       (list (list id target))))
        (should (equal (gnosis-extract-id-links
                        (caar (gnosis-select 'keimenon 'themata)))
                       (list target)))))))

(ert-deftest gnosis-test-campaign-links-null-repair-converges ()
  (dolist (command '(gnosis-links-cleanup gnosis-links-sync))
    (gnosis-test-with-db
      (let ((id (gnosis-test--add-basic-thema "[[id:nil]] [[id:unavailable]]" "Answer")))
        (gnosis--insert-into 'nodes '(["nil" "nil.org" "Literal nil" 0 nil nil nil]))
        (gnosis--insert-into 'thema-links `([,id nil] [,id nil] [,id "nil"]))
        (gnosis--insert-into 'node-links '([nil "nil"] ["nil" nil] [nil nil]
                                          ["nil" "nil"]))
        (should (= (length (gnosis--orphaned-links)) 2))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (funcall command))
        (should (equal (gnosis-select '[source dest] 'thema-links)
                       (list (list id "nil"))))
        (should (equal (gnosis-select '[source dest] 'node-links) '(("nil" "nil"))))
        (let ((before (gnosis--links-plan-state gnosis-db))
              (version (sqlite-select gnosis-db "PRAGMA user_version")))
          (gnosis-sqlite-close gnosis-db)
          (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (should (equal version (sqlite-select gnosis-db "PRAGMA user_version")))
          (should-not (sqlite-select gnosis-db "PRAGMA foreign_key_check"))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (ert-fail "Repeated repair confirmation"))))
            (funcall command))
          (should (equal before (gnosis--links-plan-state gnosis-db))))))))

(ert-deftest gnosis-test-campaign-links-null-does-not-mask-literal ()
  (gnosis-test-with-db
    (let ((id (gnosis-test--add-basic-thema "[[id:nil]]" "Answer")))
      (gnosis--insert-into 'nodes '(["nil" "nil.org" "Literal nil" 0 nil nil nil]))
      (gnosis--insert-into 'thema-links `([,id nil]))
      (should (equal (gnosis--missing-links) (list (list id "nil"))))
      (let ((audit (gnosis--link-audit-new)))
        (while (not (gnosis--link-audit-page gnosis-db audit)))
        (should (= (plist-get audit :count) 3)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (gnosis-links-sync))
      (should (equal (gnosis-select '[source dest] 'thema-links)
                     (list (list id "nil")))))))

(defun gnosis-test-campaign-vc--git (&rest args)
  "Run real Git ARGS in the fixture directory and return trimmed output."
  (with-temp-buffer
    (should (zerop (apply #'call-process "git" nil t nil args)))
    (string-trim (buffer-string))))

(ert-deftest gnosis-test-campaign-vc-commit-isolation ()
  (dolist (existing '(nil t))
    (gnosis-test-vc--with-directories
      (let ((gnosis-testing nil)
            (gnosis-vc-auto-push t)
            (pushes nil))
        (gnosis-test-campaign-vc--git "-c" "init.templateDir=" "init" "-q")
        (gnosis-test-campaign-vc--git "config" "user.name" "Fixture")
        (gnosis-test-campaign-vc--git "config" "user.email" "fixture@example.invalid")
        (gnosis-test-campaign-vc--git "config" "commit.gpgsign" "false")
        (with-temp-file (expand-file-name "gnosis.db" a) (insert "old database"))
        (when existing
          (gnosis-test-campaign-vc--git "add" "gnosis.db")
          (gnosis-test-campaign-vc--git "commit" "-qm" "initial"))
        (with-temp-file (expand-file-name "private" a) (insert "staged"))
        (gnosis-test-campaign-vc--git "add" "private")
        (with-temp-file (expand-file-name "private" a) (insert "unstaged"))
        (with-temp-file (expand-file-name "gnosis.db" a) (insert "new database"))
        (cl-letf (((symbol-function 'gnosis-vc-push)
                   (lambda () (push (list gnosis-dir default-directory) pushes))))
          (gnosis-vc--auto-commit "database")
          (setq gnosis-dir b default-directory b)
          (gnosis-test-vc--wait (lambda () pushes))
          (let ((default-directory a))
            (should (equal (gnosis-test-campaign-vc--git
                            "diff-tree" "--root" "--no-commit-id" "--name-only" "-r" "HEAD")
                           "gnosis.db"))
            (should (equal (gnosis-test-campaign-vc--git "diff" "--cached" "--name-only")
                           "private"))
            (should (equal (gnosis-test-campaign-vc--git "show" ":private") "staged"))
            (should (equal (with-temp-buffer
                             (insert-file-contents (expand-file-name "private" a))
                             (buffer-string)) "unstaged")))
          (should (equal pushes (list (list a a))))
          ;; A database with no further changes must not consume the index
          ;; or push merely because unrelated staged changes remain.
          (let ((gnosis-dir a) (before (length processes)))
            (gnosis-vc--auto-commit "empty")
            (gnosis-test-vc--wait
             (lambda () (and (= (length processes) (+ before 2))
                             (eq (process-status (car processes)) 'exit))))
            (should-not (zerop (process-exit-status (car processes))))
            (should (= (length pushes) 1)))
          (let ((gnosis-dir a) (default-directory a))
            (should (equal (gnosis-test-campaign-vc--git "show" ":private") "staged"))
            (with-temp-file (expand-file-name "gnosis.db" a) (insert "later database"))
            (let ((hook (expand-file-name ".git/hooks/pre-commit" a)))
              (make-directory (file-name-directory hook) t)
              (with-temp-file hook (insert "#!/bin/sh\nexit 1\n"))
              (set-file-modes hook #o700)
              (let ((before (length processes))
                    (head (gnosis-test-campaign-vc--git "rev-parse" "HEAD")))
                (gnosis-vc--auto-commit "refused")
                (gnosis-test-vc--wait
                 (lambda () (and (= (length processes) (+ before 2))
                                 (eq (process-status (car processes)) 'exit))))
                (should-not (zerop (process-exit-status (car processes))))
                (should (equal head (gnosis-test-campaign-vc--git "rev-parse" "HEAD")))
                (should (equal (gnosis-test-campaign-vc--git "show" ":private") "staged"))
                (should (= (length pushes) 1)))
              (delete-file hook))
            (let ((before (length processes)))
              (gnosis-vc--auto-commit "no push" nil t)
              (gnosis-test-vc--wait
               (lambda () (and (= (length processes) (+ before 2))
                               (eq (process-status (car processes)) 'exit))))
              (should (zerop (process-exit-status (car processes))))
              (should (= (length pushes) 1))
              (should (equal (gnosis-test-campaign-vc--git "diff" "--cached" "--name-only")
                             "private")))))))))

(ert-deftest gnosis-test-campaign-links-null-rollback-and-decline ()
  (dolist (command '(gnosis-links-cleanup gnosis-links-sync))
    (gnosis-test-with-db
      (let ((id (gnosis-test--add-basic-thema "Question" "Answer")))
        (gnosis--insert-into 'thema-links `([,id nil]))
        (gnosis--insert-into 'node-links '([nil nil]))
        (let ((before (gnosis--links-plan-state gnosis-db)))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
            (funcall command))
          (should (equal before (gnosis--links-plan-state gnosis-db)))
          (sqlite-execute
           gnosis-db
           (concat "CREATE TRIGGER refuse_null_delete BEFORE DELETE ON node_links "
                   "BEGIN SELECT RAISE(ABORT, 'refuse'); END"))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (should-error (funcall command) :type 'sqlite-error))
          (should (equal before (gnosis--links-plan-state gnosis-db))))))))

(ert-deftest gnosis-test-campaign-links-picker-refuses-changed-owner ()
  (dolist (phase '(string selection confirmation))
    (gnosis-test-with-db
      (gnosis-test-campaign-links--nodes)
      (gnosis-test--add-basic-thema "Needle" "Answer")
      (let ((owner gnosis-db)
            (before (gnosis--links-plan-state gnosis-db)))
        (unwind-protect
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _)
                         (when (eq phase 'string) (setq gnosis-db nil))
                         "Needle"))
                      ((symbol-function 'gnosis-completing-read)
                       (lambda (&rest _)
                         (when (eq phase 'selection) (setq gnosis-db nil))
                         "Unique"))
                      ((symbol-function 'y-or-n-p)
                       (lambda (&rest _)
                         (when (eq phase 'confirmation) (setq gnosis-db nil)) t)))
              (should-error (call-interactively #'gnosis-bulk-link-string)
                            :type 'user-error)
              (should (equal before (gnosis--links-plan-state owner))))
          (setq gnosis-db owner))))))

(ert-deftest gnosis-test-campaign-links-native-picker ()
  "Select each identity and decline/cancel through real native input."
  (skip-when noninteractive)
  (dolist (target '("one" "two" "literal" "unique" decline cancel))
    (gnosis-test-with-db
      (gnosis-test-campaign-links--nodes)
      (let* ((id (gnosis-test--add-basic-thema "Needle" "Answer"))
             (candidates (gnosis-nodes--completion-candidates
                          (gnosis-select '[id title file] 'nodes)))
             (label (if (stringp target) (car (rassoc target candidates)) "Unique"))
             (old (lookup-key global-map [f12])))
        (unwind-protect
            (progn
              (define-key global-map [f12] #'gnosis-bulk-link-string)
              (condition-case err
                  (execute-kbd-macro
                   (vconcat [f12] "Needle" [return]
                            (if (eq target 'cancel) [7]
                              (vconcat label [return]
                                       (if (eq target 'decline) "n" "y")))))
                (quit (unless (eq target 'cancel) (signal (car err) (cdr err)))))
              (if (stringp target)
                  (progn
                    (should (equal (gnosis-select '[source dest] 'thema-links)
                                   (list (list id target))))
                    (should (equal (gnosis-extract-id-links
                                    (caar (gnosis-select 'keimenon 'themata)))
                                   (list target))))
                (should-not (gnosis-select '* 'thema-links))
                (should (equal (gnosis-select 'keimenon 'themata) '(("Needle"))))))
          (define-key global-map [f12] old))))))

(provide 'gnosis-test-campaign-links-vc)
;;; gnosis-test-campaign-links-vc.el ends here
