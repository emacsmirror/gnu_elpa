;;; gnosis-test-journal.el --- Tests for gnosis-journal.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for journal TODO extraction, checked-item detection,
;; heading search, and impure operations (mark-done, update-todos).

;;; Code:

(require 'ert)
(require 'org)
(require 'gnosis)
(require 'gnosis-journal)

(require 'gnosis-test-helpers)

;;; Test helpers

(defvar gnosis-test-journal--temp-dir nil
  "Temporary directory for test files.")

(defun gnosis-test-journal--setup ()
  "Create temp directory for journal tests."
  (setq gnosis-test-journal--temp-dir (make-temp-file "gnosis-test-journal-" t)))

(defun gnosis-test-journal--teardown ()
  "Remove temp directory."
  (when (and gnosis-test-journal--temp-dir
             (file-directory-p gnosis-test-journal--temp-dir))
    (delete-directory gnosis-test-journal--temp-dir t)))

(defun gnosis-test-journal--create-file (name content)
  "Create file NAME with CONTENT in temp dir.  Return full path."
  (let ((path (expand-file-name name gnosis-test-journal--temp-dir)))
    (with-temp-file path (insert content))
    path))

(defun gnosis-test-journal--kill-file-buffer (file)
  "Kill buffer visiting FILE if one exists."
  (let ((buf (find-buffer-visiting file)))
    (when buf (kill-buffer buf))))

;;; ---- Group 1: gnosis-journal-get--todos ----

(ert-deftest gnosis-test-journal-get--todos-basic ()
  "Basic TODO extraction from a file with 2 TODO items."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let ((gnosis-journal-todo-keywords '("TODO" "NEXT"))
            (file (gnosis-test-journal--create-file
                   "todos.org"
                   "#+title: Tasks\n\n* TODO Buy groceries\n\n* TODO Read book\n")))
        (let ((todos (gnosis-journal-get--todos file)))
          (should (= 2 (length todos)))
          (should (string= "Buy groceries" (car (nth 0 todos))))
          (should (string= "Read book" (car (nth 1 todos))))
          (should (string= file (nth 2 (nth 0 todos))))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-get--todos-scheduled ()
  "Scheduled TODO has timestamp captured."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let ((gnosis-journal-todo-keywords '("TODO"))
            (file (gnosis-test-journal--create-file
                   "scheduled.org"
                   "#+title: Tasks\n\n* TODO Dentist\nSCHEDULED: <2026-03-07 Sat>\n")))
        (let ((todos (gnosis-journal-get--todos file)))
          (should (= 1 (length todos)))
          (should (string= "Dentist" (car (car todos))))
          (should (string-match-p "2026-03-07" (cadr (car todos))))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-get--todos-done-skipped ()
  "DONE items are not collected."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let ((gnosis-journal-todo-keywords '("TODO"))
            (file (gnosis-test-journal--create-file
                   "mixed.org"
                   "#+title: Tasks\n\n* TODO Active task\n\n* DONE Finished task\n")))
        (let ((todos (gnosis-journal-get--todos file)))
          (should (= 1 (length todos)))
          (should (string= "Active task" (car (car todos))))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-get--todos-empty-file ()
  "Empty file returns nil."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let ((gnosis-journal-todo-keywords '("TODO"))
            (file (gnosis-test-journal--create-file "empty.org" "")))
        (should (null (gnosis-journal-get--todos file))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-get--todos-only-configured-keywords ()
  "Only configured keywords are collected."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let ((gnosis-journal-todo-keywords '("TODO"))
            (file (gnosis-test-journal--create-file
                   "keywords.org"
                   "#+title: Tasks\n\n* TODO Active\n\n* DONE Finished\n\n* NEXT Other\n")))
        (let ((todos (gnosis-journal-get--todos file)))
          (should (= 1 (length todos)))
          (should (string= "Active" (car (car todos))))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-get--todos-multiple-keywords ()
  "Multiple configured keywords are all collected."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let ((gnosis-journal-todo-keywords '("TODO" "NEXT"))
            (file (gnosis-test-journal--create-file
                   "multi.org"
                   "#+title: Tasks\n#+todo: TODO NEXT | DONE\n\n* TODO First\n\n* NEXT Second\n\n* DONE Third\n")))
        (let ((todos (gnosis-journal-get--todos file)))
          (should (= 2 (length todos)))
          (should (string= "First" (car (nth 0 todos))))
          (should (string= "Second" (car (nth 1 todos))))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-manual-keyword-examples ()
  "Collect TODO and NEXT, but not DONE, with each manual example."
  (let ((examples
         (with-temp-buffer
           (insert-file-contents
            (expand-file-name "../docs/gnosis.org"
                              (file-name-directory
                               (locate-library "gnosis-test-journal"))))
           (cl-loop while (re-search-forward
                           "(\\(?:setopt \\)?gnosis-journal-todo-keywords\\_>"
                           nil t)
                    collect (progn
                              (goto-char (match-beginning 0))
                              (read (current-buffer)))))))
    ;; Cover both the standalone setopt and the use-package :custom entry.
    (should (= (length examples) 2))
    (gnosis-test-journal--setup)
    (unwind-protect
        (let ((file (gnosis-test-journal--create-file
                     "manual-keywords.org"
                     (concat "#+todo: TODO NEXT | DONE\n"
                             "* TODO First\n* NEXT Second\n* DONE Third\n")))
              (org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE"))))
          (dolist (example examples)
            (ert-info ((format "Manual configuration: %S" example))
              (let ((gnosis-journal-todo-keywords nil))
                ;; Evaluate only the keyword option, not unrelated setup.
                (eval (if (eq (car example) 'setopt)
                          example
                        (cons 'setopt example)) t)
                (should (equal (mapcar #'car (gnosis-journal-get--todos file))
                               '("First" "Second")))))))
      (gnosis-test-journal--teardown))))

;;; ---- Group 2: gnosis-journal-get-todos ----

(ert-deftest gnosis-test-journal-get-todos-multiple-files ()
  "TODOs from multiple files are combined."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO"))
             (f1 (gnosis-test-journal--create-file
                  "file1.org" "#+title: F1\n\n* TODO Task A\n"))
             (f2 (gnosis-test-journal--create-file
                  "file2.org" "#+title: F2\n\n* TODO Task B\n")))
        (let ((todos (gnosis-journal-get-todos (list f1 f2))))
          (should (= 2 (length todos)))
          (should (cl-some (lambda (td) (string= "Task A" (car td))) todos))
          (should (cl-some (lambda (td) (string= "Task B" (car td))) todos))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-get-todos-empty-list ()
  "Empty file list returns nil."
  (let ((gnosis-journal-todo-keywords '("TODO")))
    (should (null (gnosis-journal-get-todos '())))))

(ert-deftest gnosis-test-journal-get-todos-mixed-files ()
  "Only TODO items are collected across files."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO"))
             (f1 (gnosis-test-journal--create-file
                  "has-todo.org" "#+title: F1\n\n* TODO Task A\n"))
             (f2 (gnosis-test-journal--create-file
                  "no-todo.org" "#+title: F2\n\n* DONE Finished\n")))
        (let ((todos (gnosis-journal-get-todos (list f1 f2))))
          (should (= 1 (length todos)))
          (should (string= "Task A" (car (car todos))))))
    (gnosis-test-journal--teardown)))

;;; ---- Group 3: gnosis-journal-todos ----

(ert-deftest gnosis-test-journal-todos-formats-checkboxes ()
  "TODOs are formatted as checkboxes."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO"))
             (gnosis-journal-bullet-point-char "+")
             (f (gnosis-test-journal--create-file
                 "todos.org" "#+title: T\n\n* TODO Unscheduled task\n"))
             (gnosis-journal-todo-files (list f)))
        (let ((result (gnosis-journal-todos)))
          (should (string-match-p (regexp-quote "+ [ ] Unscheduled task") result))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-todos-filters-by-date ()
  "Only today-scheduled or unscheduled TODOs are included."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO"))
             (gnosis-journal-bullet-point-char "+")
             (today (format-time-string "%Y-%m-%d"))
             (f (gnosis-test-journal--create-file
                 "dated.org"
                 (format "#+title: T\n\n* TODO Today task\nSCHEDULED: <%s>\n\n* TODO Old task\nSCHEDULED: <2020-01-01>\n\n* TODO No date task\n"
                         today)))
             (gnosis-journal-todo-files (list f)))
        (let ((result (gnosis-journal-todos)))
          (should (string-match-p "Today task" result))
          (should (string-match-p "No date task" result))
          (should-not (string-match-p "Old task" result))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-todos-no-todos-returns-empty ()
  "No TODOs returns empty string."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO"))
             (f (gnosis-test-journal--create-file
                 "empty.org" "#+title: T\n\n* DONE Already done\n"))
             (gnosis-journal-todo-files (list f)))
        (should (string= "" (gnosis-journal-todos))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-todos-custom-bullet ()
  "Custom bullet point character is used."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO"))
             (gnosis-journal-bullet-point-char "-")
             (f (gnosis-test-journal--create-file
                 "todos.org" "#+title: T\n\n* TODO My task\n"))
             (gnosis-journal-todo-files (list f)))
        (let ((result (gnosis-journal-todos)))
          (should (string-match-p "^- \\[ \\] My task" result))))
    (gnosis-test-journal--teardown)))

;;; ---- Group 4: gnosis-journal-get-checked-items ----

(ert-deftest gnosis-test-journal-get-checked-items-single ()
  "Single checked item is extracted."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n+ [X] Done item\n+ [ ] Not done\n")
    (let* ((parsed (org-element-parse-buffer))
           (items (gnosis-journal-get-checked-items parsed)))
      (should (= 1 (length items)))
      (should (string= "Done item" (car items))))))

(ert-deftest gnosis-test-journal-get-checked-items-unchecked-skipped ()
  "Unchecked items are not included."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n+ [ ] Item one\n+ [ ] Item two\n")
    (let* ((parsed (org-element-parse-buffer))
           (items (gnosis-journal-get-checked-items parsed)))
      (should (null items)))))

(ert-deftest gnosis-test-journal-get-checked-items-multiple ()
  "Multiple checked items are all collected in order."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n+ [X] First\n+ [X] Second\n+ [X] Third\n")
    (let* ((parsed (org-element-parse-buffer))
           (items (gnosis-journal-get-checked-items parsed)))
      (should (= 3 (length items)))
      (should (string= "First" (nth 0 items)))
      (should (string= "Second" (nth 1 items)))
      (should (string= "Third" (nth 2 items))))))

(ert-deftest gnosis-test-journal-get-checked-items-none ()
  "No items returns nil."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nSome text without checkboxes.\n")
    (let* ((parsed (org-element-parse-buffer))
           (items (gnosis-journal-get-checked-items parsed)))
      (should (null items)))))

(ert-deftest gnosis-test-journal-get-checked-items-nested ()
  "Nested checked items are extracted."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n+ [X] Parent item\n  + [X] Child item\n")
    (let* ((parsed (org-element-parse-buffer))
           (items (gnosis-journal-get-checked-items parsed)))
      (should (= 2 (length items)))
      (should (member "Parent item" items))
      (should (member "Child item" items)))))

;;; ---- Group 5: gnosis-journal-find-file-with-heading ----

(ert-deftest gnosis-test-journal-find-file-heading-found ()
  "Returns file path when heading is found."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let ((file (gnosis-test-journal--create-file
                   "notes.org" "#+title: Notes\n\n* My Heading\nContent.\n")))
        (should (string= file
                         (gnosis-journal-find-file-with-heading
                          "My Heading" (list file)))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-find-file-heading-not-found ()
  "Returns nil when heading is not in any file."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let ((file (gnosis-test-journal--create-file
                   "notes.org" "#+title: Notes\n\n* Other Heading\n")))
        (should (null (gnosis-journal-find-file-with-heading
                       "Missing" (list file)))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-find-file-heading-in-second ()
  "Finds heading in second file when first doesn't have it."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let ((f1 (gnosis-test-journal--create-file
                 "a.org" "#+title: A\n\n* Heading A\n"))
            (f2 (gnosis-test-journal--create-file
                 "b.org" "#+title: B\n\n* Target Heading\n")))
        (should (string= f2
                         (gnosis-journal-find-file-with-heading
                          "Target Heading" (list f1 f2)))))
    (gnosis-test-journal--teardown)))

;;; ---- Group 6: gnosis-journal--dir ----

(ert-deftest gnosis-test-journal--dir-creates-missing ()
  "Creates directory if it does not exist."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((new-dir (expand-file-name "new-journal"
                                        gnosis-test-journal--temp-dir))
             (gnosis-journal-dir new-dir))
        (should-not (file-directory-p new-dir))
        (let ((result (gnosis-journal--dir)))
          (should (file-directory-p new-dir))
          (should (string= new-dir result))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal--dir-returns-existing ()
  "Returns existing directory without error."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let ((gnosis-journal-dir gnosis-test-journal--temp-dir))
        (should (file-directory-p gnosis-test-journal--temp-dir))
        (let ((result (gnosis-journal--dir)))
          (should (string= gnosis-test-journal--temp-dir result))))
    (gnosis-test-journal--teardown)))

;;; ---- Group 7: gnosis-journal-mark-todo-as-done ----

(ert-deftest gnosis-test-journal-mark-todo-as-done-marks ()
  "Obsolete title completion refuses instead of rewriting the source."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "tasks.org"
                    "#+title: Tasks\n\n* TODO Exercise\nSCHEDULED: <2026-03-07>\n"))
             (gnosis-journal-todo-files (list file)))
        (should-error (gnosis-journal-mark-todo-as-done "Exercise")
                      :type 'user-error)
        (gnosis-test-journal--kill-file-buffer file)
        (with-temp-buffer
          (insert-file-contents file)
          (should (string-match-p "TODO Exercise" (buffer-string)))
          (should-not (string-match-p "LAST_DONE_DATE" (buffer-string)))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-mark-todo-as-done-already-done ()
  "Obsolete title completion refuses an already-done heading."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "tasks.org"
                    "#+title: Tasks\n\n* DONE Finished\n"))
             (gnosis-journal-todo-files (list file)))
        (should-error (gnosis-journal-mark-todo-as-done "Finished")
                      :type 'user-error)
        (gnosis-test-journal--kill-file-buffer file)
        (with-temp-buffer
          (insert-file-contents file)
          (should (string-match-p "DONE Finished" (buffer-string)))
          (should-not (string-match-p "LAST_DONE_DATE" (buffer-string)))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-mark-todo-as-done-nonexistent ()
  "Obsolete title completion refuses a missing heading."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "tasks.org"
                    "#+title: Tasks\n\n* TODO Real task\n"))
             (gnosis-journal-todo-files (list file)))
        (should-error (gnosis-journal-mark-todo-as-done "Nonexistent heading")
                      :type 'user-error)
        (with-temp-buffer
          (insert-file-contents file)
          (should (string-match-p "TODO Real task" (buffer-string)))))
    (gnosis-test-journal--teardown)))

;;; ---- Group 8: gnosis-journal--update-todos ----

(ert-deftest gnosis-test-journal-update-todos-checks-mark-done ()
  "Obsolete save-hook completion does not mark source TODOs."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO"))
             (gnosis-journal-file nil)
             (today (format-time-string "%Y-%m-%d"))
             (todo-file (gnosis-test-journal--create-file
                         "tasks.org"
                         (format "#+title: Tasks\n\n* TODO Exercise\nSCHEDULED: <%s>\n" today)))
             (journal-file (gnosis-test-journal--create-file
                            (concat today ".org")
                            (format "#+title: %s\n\n* Goals\n+ [X] Exercise\n+ [ ] Meditate\n"
                                    today)))
             (gnosis-journal-todo-files (list todo-file)))
        (gnosis-journal--update-todos journal-file)
        (gnosis-test-journal--kill-file-buffer todo-file)
        (with-temp-buffer
          (insert-file-contents todo-file)
          (should (string-match-p "TODO Exercise" (buffer-string)))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-update-todos-single-file-scopes-today ()
  "Obsolete save-hook completion leaves every source TODO unchanged."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO"))
             (today (format-time-string "%Y-%m-%d"))
             (journal-file (gnosis-test-journal--create-file
                            "journal.org"
                            (format "#+title: Journal\n\n* 2020-01-01\n+ [X] OldTask\n\n* %s\n+ [X] TodayTask\n"
                                    today)))
             (gnosis-journal-file journal-file)
             (todo-file (gnosis-test-journal--create-file
                         "tasks.org"
                         "#+title: Tasks\n\n* TODO OldTask\n\n* TODO TodayTask\n"))
             (gnosis-journal-todo-files (list todo-file)))
        (gnosis-journal--update-todos journal-file)
        (gnosis-test-journal--kill-file-buffer todo-file)
        (with-temp-buffer
          (insert-file-contents todo-file)
          (should (string-match-p "TODO TodayTask" (buffer-string)))
          (should (string-match-p "TODO OldTask" (buffer-string)))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-update-todos-previous-date-no-false-positive ()
  "Obsolete save-hook completion does not treat yesterday's checkboxes as tasks."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO"))
             (today (format-time-string "%Y-%m-%d"))
             (journal-file (gnosis-test-journal--create-file
                            "journal.org"
                            (format "#+title: Journal\n\n* 2026-03-06\n+ [X] YesterdayTask\n\n* %s\n+ [X] TodayTask\n"
                                    today)))
             (gnosis-journal-file journal-file)
             (todo-file (gnosis-test-journal--create-file
                         "tasks.org"
                         "#+title: Tasks\n\n* TODO YesterdayTask\n\n* TODO TodayTask\n"))
             (gnosis-journal-todo-files (list todo-file)))
        (gnosis-journal--update-todos journal-file)
        (gnosis-test-journal--kill-file-buffer todo-file)
        (with-temp-buffer
          (insert-file-contents todo-file)
          (should (string-match-p "TODO TodayTask" (buffer-string)))
          (should (string-match-p "TODO YesterdayTask" (buffer-string)))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-update-todos-no-today-heading ()
  "Obsolete save-hook completion does not mark TODOs without a today heading."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO"))
             (journal-file (gnosis-test-journal--create-file
                            "journal.org"
                            "#+title: Journal\n\n* 2020-01-01\n+ [X] OldTask\n"))
             (gnosis-journal-file journal-file)
             (todo-file (gnosis-test-journal--create-file
                         "tasks.org"
                         "#+title: Tasks\n\n* TODO OldTask\n"))
             (gnosis-journal-todo-files (list todo-file)))
        (gnosis-journal--update-todos journal-file)
        (with-temp-buffer
          (insert-file-contents todo-file)
          (should (string-match-p "TODO OldTask" (buffer-string)))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-update-todos-separate-file-all-items ()
  "Obsolete save-hook completion does not mark TODOs in separate-file mode."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO"))
             (gnosis-journal-file nil)
             (today (format-time-string "%Y-%m-%d"))
             (journal-file (gnosis-test-journal--create-file
                            (concat today ".org")
                            (format "#+title: %s\n\n* Notes\n+ [X] TaskA\n\n* Goals\n+ [X] TaskB\n"
                                    today)))
             (todo-file (gnosis-test-journal--create-file
                         "tasks.org"
                         "#+title: Tasks\n\n* TODO TaskA\n\n* TODO TaskB\n"))
             (gnosis-journal-todo-files (list todo-file)))
        (gnosis-journal--update-todos journal-file)
        (gnosis-test-journal--kill-file-buffer todo-file)
        (with-temp-buffer
          (insert-file-contents todo-file)
          (should (string-match-p "TODO TaskA" (buffer-string)))
          (should (string-match-p "TODO TaskB" (buffer-string)))))
    (gnosis-test-journal--teardown)))

;;; ---- Group 9: path resolution, default, and creation ----

(defun gnosis-test-journal--empty-templates ()
  "Return a one-item journal template that inserts nothing."
  (list (cons "Empty" (lambda () ""))))

(defun gnosis-test-journal--kill-files (files)
  "Kill visiting buffers for FILES without saving."
  (dolist (file files)
    (when-let* ((buf (get-file-buffer file)))
      (with-current-buffer buf
        (set-buffer-modified-p nil))
      (kill-buffer buf))))

(defmacro gnosis-test-journal--without-epa (&rest body)
  "Evaluate BODY without EPA filename handlers.
Name-only `.org.gpg' fixtures are not encryption proof."
  `(let ((file-name-handler-alist
          (let ((alist (copy-sequence file-name-handler-alist)))
            (rassq-delete-all 'epa-file-handler alist))))
     ,@body))

(ert-deftest gnosis-test-journal-default-is-single-file-under-journal-dir ()
  "Default single file is journal.org resolved against gnosis-journal-dir, not cwd."
  (should (eq (default-value 'gnosis-journal-file) t))
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (cwd (expand-file-name "cwd" gnosis-dir))
           (default-directory cwd)
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (expected (expand-file-name "journal.org" gnosis-journal-dir))
           (cwd-file (expand-file-name "journal.org" cwd)))
      (make-directory gnosis-journal-dir t)
      (make-directory cwd t)
      (unwind-protect
          (save-window-excursion
            (gnosis-journal-find "2026-01-02")
            (should (equal (buffer-file-name) expected))
            (should-not (file-exists-p cwd-file))
            (should (org-find-exact-headline-in-buffer "2026-01-02")))
        (gnosis-test-journal--kill-files (list expected cwd-file))))))

(ert-deftest gnosis-test-journal-nil-preserves-separate-files ()
  "Nil keeps one file per entry in gnosis-journal-dir."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file nil)
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (gnosis-nodes-timestring nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (single (expand-file-name "journal.org" gnosis-journal-dir)))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (gnosis-journal-find "Separate Title")
            (should (equal (file-name-nondirectory (buffer-file-name))
                           "Separate_Title.org"))
            (should-not (equal (buffer-file-name) single))
            (should-not (file-exists-p single)))
        (gnosis-test-journal--kill-files
         (directory-files gnosis-journal-dir t "\\.org\\'" t))))))

(ert-deftest gnosis-test-journal-relative-file-ignores-cwd ()
  "A relative gnosis-journal-file is resolved against the journal directory."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (cwd (expand-file-name "cwd" gnosis-dir))
           (default-directory cwd)
           (gnosis-journal-file "daily.org")
           (gnosis-journal-as-gpg nil)
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (expected (expand-file-name "daily.org" gnosis-journal-dir))
           (cwd-file (expand-file-name "daily.org" cwd))
           (plain (expand-file-name "journal.org" gnosis-journal-dir)))
      (make-directory gnosis-journal-dir t)
      (make-directory cwd t)
      (unwind-protect
          (save-window-excursion
            (gnosis-journal-find "Encrypted")
            (should (equal (buffer-file-name) expected))
            (should-not (file-exists-p cwd-file))
            (should-not (file-exists-p plain)))
        (gnosis-test-journal--kill-files (list expected cwd-file plain))))))

(ert-deftest gnosis-test-journal-absolute-file-is-used-as-is ()
  "An absolute gnosis-journal-file is not re-rooted under gnosis-journal-dir."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (outside (expand-file-name "outside.org" gnosis-dir))
           (gnosis-journal-file outside)
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (inside (expand-file-name "journal.org" gnosis-journal-dir)))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (gnosis-journal-find "Outside")
            (should (equal (buffer-file-name) outside))
            (should-not (file-exists-p inside)))
        (gnosis-test-journal--kill-files (list outside inside))))))

(ert-deftest gnosis-test-journal-as-gpg-does-not-convert-single-file ()
  "Single-file encryption is an explicit .gpg path, never a silent rename."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file "journal.org")
           (gnosis-journal-as-gpg t)
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (plain (expand-file-name "journal.org" gnosis-journal-dir))
           (encrypted (expand-file-name "journal.org.gpg" gnosis-journal-dir)))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (gnosis-journal-find "Plain")
            (should (equal (buffer-file-name) plain))
            (should-not (file-exists-p encrypted)))
        (gnosis-test-journal--kill-files (list plain encrypted))))))

(ert-deftest gnosis-test-journal-rejects-directory-before-navigation-or-db ()
  "A directory-valued single-file setting errors before DB, files, or Dired."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((dir (expand-file-name "journal" gnosis-test-journal--temp-dir))
             (gnosis-journal-dir dir)
             (gnosis-journal-file dir)
             (buffers (buffer-list))
             (db-called nil)
             (visited nil))
        (make-directory dir t)
        (cl-letf (((symbol-function 'gnosis-nodes-select)
                   (lambda (&rest _)
                     (setq db-called t)
                     (error "Database should not be queried")))
                  ((symbol-function 'find-file)
                   (lambda (file &rest _)
                     (push file visited)
                     (error "find-file should not run")))
                  ((symbol-function 'find-file-noselect)
                   (lambda (file &rest _)
                     (push file visited)
                     (error "find-file-noselect should not run"))))
          (should-error (gnosis-journal) :type 'user-error)
          (should-error (gnosis-journal-find "Topic") :type 'user-error)
          (should-error (gnosis-journal-db-sync t) :type 'user-error))
        (should-not db-called)
        (should-not visited)
        (should (equal (buffer-list) buffers))
        (should (file-directory-p dir))
        (should (equal (directory-files dir) '("." ".."))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-file-p-uses-journal-dir-not-cwd ()
  "Journal ownership compares against the journal-dir resolution, not cwd."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-dir (expand-file-name "journal"
                                                   gnosis-test-journal--temp-dir))
             (cwd (expand-file-name "cwd" gnosis-test-journal--temp-dir))
             (default-directory cwd)
             (gnosis-journal-file "journal.org")
             (real (expand-file-name "journal.org" gnosis-journal-dir))
             (cwd-file (expand-file-name "journal.org" cwd)))
        (make-directory gnosis-journal-dir t)
        (make-directory cwd t)
        (with-temp-file real (insert "#+title: Real\n"))
        (with-temp-file cwd-file (insert "#+title: Cwd\n"))
        (should (gnosis-nodes--journal-file-p real))
        (should-not (gnosis-nodes--journal-file-p cwd-file)))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-find-creates-arbitrary-single-file-titles ()
  "Single-file mode creates requested titles, not only today's date."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (separate (expand-file-name "Topic.org" gnosis-journal-dir)))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (gnosis-journal-find "Topic")
            (should (equal (buffer-file-name) gnosis-journal-file))
            (should-not (file-exists-p separate))
            (should (equal (buffer-file-name) gnosis-journal-file))
            (goto-char (point-min))
            (should (re-search-forward "^\\* Topic$" nil t))
            (should (org-id-get)))
        (gnosis-test-journal--kill-files
         (list gnosis-journal-file separate))))))

(ert-deftest gnosis-test-journal-find-reuses-unsaved-heading ()
  "Finding an existing unsaved heading does not duplicate it or drop dirty text."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (gnosis-journal-find "Draft")
            (let ((id (org-id-get)))
              (insert "unsaved body\n")
              (should (buffer-modified-p))
              (gnosis-journal-find "Draft")
              (should (equal (org-id-get) id))
              (should (eq (count-matches "^\\* Draft$" (point-min) (point-max)) 1))
              (should (string-match-p "unsaved body" (buffer-string)))
              (should (buffer-modified-p))))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-find-scans-level-one-headings-with-root-id ()
  "Today's date matches the level-1 heading even when a file-level ID exists."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (today (format-time-string "%Y-%m-%d")))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert (format ":PROPERTIES:\n:ID: journal-root\n:END:\n#+title: Journal\n#+filetags:\n\n* %s\n:PROPERTIES:\n:ID: today-id\n:END:\nbody\n"
                        today)))
      (gnosis-nodes-update-file gnosis-journal-file t)
      (unwind-protect
          (save-window-excursion
            (gnosis-journal)
            (should (equal (buffer-file-name) gnosis-journal-file))
            (should (equal (org-entry-get nil "ID") "today-id"))
            (should (eq (count-matches (concat "^\\* " (regexp-quote today) "$")
                                       (point-min) (point-max))
                        1)))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-template-cancel-leaves-no-half-entry ()
  "Canceling template selection does not insert a heading."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'gnosis-nodes-select-template)
                       (lambda (&rest _) (user-error "Canceled"))))
              (should-error (gnosis-journal-find "Canceled") :type 'user-error))
            (when (file-exists-p gnosis-journal-file)
              (with-temp-buffer
                (insert-file-contents gnosis-journal-file)
                (should-not (re-search-forward "^\\* Canceled$" nil t))))
            (when-let* ((buf (get-file-buffer gnosis-journal-file)))
              (with-current-buffer buf
                (should-not (re-search-forward "^\\* Canceled$" nil t)))))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-template-cannot-retarget-destination ()
  "Template callbacks cannot redirect writes by changing config, DB, or buffer."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (target (expand-file-name "journal.org" gnosis-journal-dir))
           (other (expand-file-name "other.org" gnosis-dir))
           (gnosis-journal-file target)
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (original-db gnosis-db)
           (decoy (get-buffer-create " *gnosis-journal-decoy*")))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (with-temp-file other (insert "#+title: Other\n"))
            (let ((gnosis-journal-templates
                   (list (cons "Evil"
                               (lambda ()
                                 (setq gnosis-journal-file other)
                                 (setq gnosis-db nil)
                                 (set-buffer decoy)
                                 "")))))
              (should-error (gnosis-journal-find "Scoped") :type 'user-error))
            (when (file-exists-p target)
              (with-temp-buffer
                (insert-file-contents target)
                (should-not (re-search-forward "^\\* Scoped$" nil t))))
            (with-temp-buffer
              (insert-file-contents other)
              (should-not (re-search-forward "^\\* Scoped$" nil t)))
            (with-current-buffer decoy
              (should-not (re-search-forward "^\\* Scoped$" nil t))))
        (setq gnosis-db original-db)
        (when (buffer-live-p decoy) (kill-buffer decoy))
        (gnosis-test-journal--kill-files (list target other))))))

(ert-deftest gnosis-test-journal-stale-index-still-visits-live-heading ()
  "A stale index row does not block visiting the live heading or create a duplicate."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert "#+title: Journal\n* Live\n:PROPERTIES:\n:ID: live-id\n:END:\n"))
      (gnosis--insert-into
       'journal '(["stale-id" "missing.org" "Live" 1 nil "0" "hash"]))
      (unwind-protect
          (save-window-excursion
            (gnosis-journal-find "Live")
            (should (equal (buffer-file-name) gnosis-journal-file))
            (should (equal (org-entry-get nil "ID") "live-id"))
            (should (eq (count-matches "^\\* Live$" (point-min) (point-max)) 1)))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-insert-creates-single-file-heading ()
  "Journal link insertion creates a heading in the single file, not a sibling file."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (separate (expand-file-name "Linked.org" gnosis-journal-dir)))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'gnosis-nodes--find)
                       (lambda (&rest _) "Linked")))
              (with-temp-buffer
                (org-mode)
                (gnosis-journal-insert nil)
                (should (string-match-p "id:" (buffer-string)))))
            (should (get-file-buffer gnosis-journal-file))
            (should-not (file-exists-p separate))
            (with-current-buffer (get-file-buffer gnosis-journal-file)
              (goto-char (point-min))
              (should (re-search-forward "^\\* Linked$" nil t))))
        (gnosis-test-journal--kill-files
         (list gnosis-journal-file separate))))))

(ert-deftest gnosis-test-journal-insert-same-file-keeps-origin ()
  "Public insert into the same journal file keeps origin line and target heading."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (target-block (concat "* 2026-01-01\n:PROPERTIES:\n"
                                 ":ID: day-one\n:END:\nalpha body\n")))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert (concat "#+title: Journal\n" target-block
                        "* 2026-01-03\n:PROPERTIES:\n:ID: day-three\n:END:\n"
                        "HERE\n")))
      (unwind-protect
          (save-window-excursion
            (find-file gnosis-journal-file)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^HERE$")
            (beginning-of-line)
            (should (equal (org-get-heading t t t t) "2026-01-03"))
            (cl-letf (((symbol-function 'gnosis-nodes--find)
                       (lambda (&rest _) "2026-01-01")))
              (gnosis-journal-insert nil))
            (should (equal (org-get-heading t t t t) "2026-01-03"))
            (should (string-match-p
                     "\\[\\[id:day-one\\]\\[2026-01-01\\]\\]"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
            (should (string-match-p
                     "HERE"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
            (goto-char (point-min))
            (should (search-forward target-block nil t))
            (goto-char (point-min))
            (should (re-search-forward "^\\* 2026-01-01$" nil t))
            (should (equal (org-entry-get nil "ID") "day-one"))
            (should (equal (org-entry-get nil "ID" nil t) "day-one")))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-insert-same-file-preserves-restriction ()
  "Same-file insert restores origin narrowing after visiting the target."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert (concat "#+title: Journal\n"
                        "* 2026-01-01\n:PROPERTIES:\n:ID: day-one\n:END:\n"
                        "alpha body\n"
                        "* 2026-01-03\n:PROPERTIES:\n:ID: day-three\n:END:\n"
                        "HERE\n")))
      (unwind-protect
          (save-window-excursion
            (find-file gnosis-journal-file)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^\\* 2026-01-03$")
            (org-narrow-to-subtree)
            (goto-char (point-min))
            (re-search-forward "^HERE$")
            (beginning-of-line)
            (cl-letf (((symbol-function 'gnosis-nodes--find)
                       (lambda (&rest _) "2026-01-01")))
              (gnosis-journal-insert nil))
            (should (equal (org-get-heading t t t t) "2026-01-03"))
            (should (string-match-p
                     "\\[\\[id:day-one\\]\\[2026-01-01\\]\\]"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
            (save-excursion
              (goto-char (point-min))
              (should-not (re-search-forward "^\\* 2026-01-01$" nil t)))
            (widen)
            (goto-char (point-min))
            (should (re-search-forward "^\\* 2026-01-01$" nil t))
            (should (equal (org-entry-get nil "ID") "day-one")))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-find-create-while-narrowed-preserves-siblings ()
  "Creating a missing title from a narrowed mid-file subheading appends at EOF."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert (concat "#+title: Journal\n"
                        "* 2026-01-01\n:PROPERTIES:\n:ID: d1\n:END:\n"
                        "** Goals\nkeep goals\n"
                        "** Notes\nkeep notes\n"
                        "* 2026-01-05\n:PROPERTIES:\n:ID: d5\n:END:\n"
                        "HERE\n")))
      (unwind-protect
          (save-window-excursion
            (find-file gnosis-journal-file)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^\\*\\* Goals$")
            (org-narrow-to-subtree)
            (goto-char (point-min))
            (re-search-forward "keep goals")
            (end-of-line)
            (gnosis-journal-find "2026-01-03")
            (should-not (buffer-narrowed-p))
            (should (equal (org-get-heading t t t t) "2026-01-03"))
            (should (org-id-get))
            (save-restriction
              (widen)
              (goto-char (point-min))
              (should (re-search-forward "^\\*\\* Notes$" nil t))
              (should (org-up-heading-safe))
              (should (equal (org-get-heading t t t t) "2026-01-01"))
              (should (equal
                       (org-element-map (org-element-parse-buffer) 'headline
                         (lambda (headline)
                           (and (= (org-element-property :level headline) 1)
                                (org-element-property :raw-value headline)))
                         nil nil 'headline)
                       '("2026-01-01" "2026-01-05" "2026-01-03")))))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-insert-create-while-narrowed-preserves-siblings ()
  "Same-buffer insert create restores origin restriction and keeps sibling parents."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           origin-point)
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert (concat "#+title: Journal\n"
                        "* 2026-01-01\n:PROPERTIES:\n:ID: d1\n:END:\n"
                        "** Goals\nkeep goals\n"
                        "** Notes\nkeep notes\n"
                        "* 2026-01-05\n:PROPERTIES:\n:ID: d5\n:END:\n"
                        "HERE\n")))
      (unwind-protect
          (save-window-excursion
            (find-file gnosis-journal-file)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^\\*\\* Goals$")
            (org-narrow-to-subtree)
            (goto-char (point-min))
            (re-search-forward "keep goals")
            (end-of-line)
            (setq origin-point (point))
            (cl-letf (((symbol-function 'gnosis-nodes--find)
                       (lambda (&rest _) "Linked")))
              (gnosis-journal-insert nil))
            (should (buffer-narrowed-p))
            (should (equal (org-get-heading t t t t) "Goals"))
            (should (<= origin-point (point)))
            (should (string-match-p
                     "keep goals"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
            (should (string-match-p
                     "id:"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
            (save-excursion
              (goto-char (point-min))
              (should-not (re-search-forward "^\\* 2026-01-05$" nil t)))
            (save-restriction
              (widen)
              (goto-char (point-min))
              (should (re-search-forward "^\\*\\* Notes$" nil t))
              (should (org-up-heading-safe))
              (should (equal (org-get-heading t t t t) "2026-01-01"))
              (should (equal
                       (org-element-map (org-element-parse-buffer) 'headline
                         (lambda (headline)
                           (and (= (org-element-property :level headline) 1)
                                (org-element-property :raw-value headline)))
                         nil nil 'headline)
                       '("2026-01-01" "2026-01-05" "Linked")))))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-sync-deduplicates-and-keeps-separate-files ()
  "Sync indexes the resolved single file once and retains sibling journal files."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (old (expand-file-name "old.org" gnosis-journal-dir))
           (gnosis-journal-todo-files nil)
           (seen nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert "#+title: Journal\n* One\n:PROPERTIES:\n:ID: one\n:END:\n"))
      (with-temp-file old
        (insert "#+title: 2000-01-01\n:PROPERTIES:\n:ID: old\n:END:\n"))
      (cl-letf (((symbol-function 'gnosis-nodes-update-file)
                 (lambda (file &optional _)
                   (push (expand-file-name file) seen))))
        (gnosis-journal-db-sync t))
      (should (= 2 (length seen)))
      (should (equal (sort seen #'string<)
                     (sort (list gnosis-journal-file old) #'string<))))))

;;; ---- Group 10: TODO extraction and write-back ----

(ert-deftest gnosis-test-journal-todos-expand-agenda-directories ()
  "Agenda directories contribute org files instead of raising Is a directory."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((agenda-dir (expand-file-name "agenda" gnosis-test-journal--temp-dir))
             (file (gnosis-test-journal--create-file
                    "todos.org" "#+title: T\n* TODO From file\n"))
             (gnosis-journal-todo-keywords '("TODO"))
             (gnosis-journal-bullet-point-char "+")
             (gnosis-journal-todo-files (list agenda-dir file)))
        (make-directory agenda-dir)
        (with-temp-file (expand-file-name "inside.org" agenda-dir)
          (insert "#+title: Inside\n* TODO From agenda dir\n"))
        (let ((result (gnosis-journal-todos)))
          (should (string-match-p "From file" result))
          (should (string-match-p "From agenda dir" result))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-todos-use-live-source-buffer ()
  "TODO extraction reads a live visiting buffer, not only the last save."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "todos.org" "#+title: T\n* TODO DiskTitle\n"))
             (gnosis-journal-todo-keywords '("TODO"))
             (gnosis-journal-todo-files (list file)))
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (re-search-forward "DiskTitle")
          (replace-match "LiveTitle")
          (should (buffer-modified-p)))
        (let ((result (gnosis-journal-todos)))
          (should (string-match-p "LiveTitle" result))
          (should-not (string-match-p "DiskTitle" result)))
        (gnosis-test-journal--kill-file-buffer file))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-mark-todo-ambiguous-title-is-safe ()
  "Ambiguous matching titles are left untouched rather than picking one."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "tasks.org"
                    "#+title: Tasks\n\n* TODO Exercise\n\n* TODO Exercise\n"))
             (gnosis-journal-todo-files (list file)))
        (should-error (gnosis-journal-mark-todo-as-done "Exercise")
                      :type 'user-error)
        (gnosis-test-journal--kill-file-buffer file)
        (with-temp-buffer
          (insert-file-contents file)
          (should (= 2 (count-matches "TODO Exercise" (point-min) (point-max))))
          (should-not (re-search-forward "DONE Exercise" nil t))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-complete-task-uses-source-id ()
  "Explicit completion marks the linked source ID and leaves the buffer dirty."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "tasks.org"
                    (concat "#+title: Tasks\n#+todo: TODO NEXT | DONE\n"
                            "* NEXT Focus\n:PROPERTIES:\n:ID: task-focus\n:END:\n"
                            "* TODO Other\n:PROPERTIES:\n:ID: task-other\n:END:\n")))
             (gnosis-journal-todo-files (list file))
             (gnosis-journal-todo-keywords '("TODO" "NEXT"))
             (org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE")))
             (org-id-track-globally nil))
        (with-temp-buffer
          (org-mode)
          (insert "+ [X] [[id:task-focus][Focus]]\n")
          (goto-char (point-min))
          (re-search-forward "id:task-focus")
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (gnosis-journal-complete-task)))
        (with-current-buffer (find-file-noselect file)
          (should (buffer-modified-p))
          (goto-char (point-min))
          (should (re-search-forward "DONE Focus" nil t))
          (should (re-search-forward "TODO Other" nil t)))
        (gnosis-test-journal--kill-file-buffer file))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-today-does-not-read-archived-files ()
  "Opening today reads only the configured single file, not retained archives."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file t)
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (reads nil)
           (orig (symbol-function 'insert-file-contents)))
      (make-directory gnosis-journal-dir t)
      (dotimes (i 8)
        (with-temp-file (expand-file-name (format "old-%d.org" i)
                                          gnosis-journal-dir)
          (insert (format "#+title: 2000-01-%02d\n:PROPERTIES:\n:ID: old-%d\n:END:\n"
                          (1+ i) i))))
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'insert-file-contents)
                       (lambda (file &rest args)
                         (push (expand-file-name file) reads)
                         (apply orig file args))))
              (gnosis-journal-find "2026-02-02"))
            (should (cl-every (lambda (file)
                                (not (string-match-p "old-" file)))
                              reads)))
        (gnosis-test-journal--kill-files
         (directory-files gnosis-journal-dir t "\\.org\\'" t))))))

(ert-deftest gnosis-test-journal-invalid-file-setting-is-rejected ()
  "Non-nil, non-t, non-string gnosis-journal-file is not separate-file mode."
  (let ((gnosis-journal-file 1))
    (should-error (gnosis-journal--file) :type 'user-error)))

(ert-deftest gnosis-test-journal-default-t-respects-as-gpg ()
  "Automatic t uses journal.org.gpg when gnosis-journal-as-gpg is set."
  (let* ((gnosis-journal-dir "/tmp/gnosis-journal-dir-test")
         (gnosis-journal-file t)
         (gnosis-journal-as-gpg t))
    (should (string-suffix-p "journal.org.gpg"
                             (gnosis-journal--configured-file)))))

(ert-deftest gnosis-test-journal-complete-task-rejects-non-todo ()
  "An ID on an ordinary heading is not completed."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "note.org"
                    "* Notes\n:PROPERTIES:\n:ID: not-a-task\n:END:\n"))
             (gnosis-journal-todo-files (list file))
             (org-id-track-globally nil))
        (with-temp-buffer
          (org-mode)
          (insert "+ [X] [[id:not-a-task][Notes]]\n")
          (goto-char (point-min))
          (re-search-forward "id:not-a-task")
          (should-error (gnosis-journal-complete-task) :type 'user-error))
        (with-temp-buffer
          (insert-file-contents file)
          (should-not (string-match-p "DONE" (buffer-string))))
        (gnosis-test-journal--kill-file-buffer file))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-capture-appends-timestamp ()
  "Capture appends a timestamped note to today's entry."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file t)
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (today (format-time-string "%Y-%m-%d")))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (gnosis-journal-capture "hello capture")
            (should (org-find-exact-headline-in-buffer today))
            (should (string-match-p "hello capture" (buffer-string))))
        (gnosis-test-journal--kill-files
         (list (gnosis-journal--file)))))))

(ert-deftest gnosis-test-journal-date-rejects-invalid ()
  "gnosis-journal-date requires a real calendar date."
  (should-error (gnosis-journal-date "nope") :type 'user-error)
  (should-error (gnosis-journal-date "2026-02-31") :type 'user-error))

(ert-deftest gnosis-test-journal-previous-does-not-create ()
  "Previous visits an existing date and does not create a missing one."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert "#+title: Journal\n* 2026-01-01\n:PROPERTIES:\n:ID: d1\n:END:\n* 2026-01-03\n:PROPERTIES:\n:ID: d3\n:END:\n"))
      (unwind-protect
          (save-window-excursion
            (gnosis-journal-find "2026-01-03")
            (gnosis-journal-previous)
            (should (equal (org-entry-get nil "ID") "d1"))
            (should-error (gnosis-journal-previous) :type 'user-error)
            (should (eq (count-matches "^\\* " (point-min) (point-max)) 2)))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

;;; ---- Correction regressions ----

(ert-deftest gnosis-test-journal-title-date-preserves-root-prefix ()
  "Root-id titles yield the trailing ISO date and leave caller match data."
  (string-match "\\(foo\\)" "foo")
  (should (equal (gnosis-journal--title-date "Journal:2001-02-03")
                 "2001-02-03"))
  (should (equal (match-string 1 "foo") "foo"))
  (should (equal (gnosis-journal--title-date "2001-02-03") "2001-02-03"))
  (should-not (gnosis-journal--title-date "Journal:2001-02-31")))

(ert-deftest gnosis-test-journal-indexed-root-id-dates-navigate ()
  "Indexed root-id titles participate in find/previous/next without creation."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (archive (expand-file-name "old.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert "#+title: Daily\n* 2001-02-03\n:PROPERTIES:\n:ID: d3\n:END:\n"))
      (with-temp-file archive
        (insert (concat ":PROPERTIES:\n:ID: archive-root\n:END:\n"
                        "#+title: Journal\n#+filetags:\n\n"
                        "* 2001-02-01\n:PROPERTIES:\n:ID: arch-d1\n:END:\n")))
      (gnosis-nodes-update-file gnosis-journal-file)
      (gnosis-nodes-update-file archive)
      (unwind-protect
          (save-window-excursion
            (should (cl-some (lambda (title)
                               (string-match-p ":2001-02-01\\'" title))
                             (gnosis-nodes-select 'title 'journal nil t)))
            (gnosis-test-journal--kill-files (list archive))
            (should (member "2001-02-01" (gnosis-journal--dated-titles)))
            (should-not (member "Jour" (gnosis-journal--dated-titles)))
            (gnosis-journal-find "2001-02-01")
            (should (equal (expand-file-name (buffer-file-name))
                           (expand-file-name archive)))
            (should (equal (org-entry-get nil "ID") "arch-d1"))
            (gnosis-journal-find "2001-02-03")
            (gnosis-journal-previous)
            (should (equal (org-entry-get nil "ID") "arch-d1"))
            (gnosis-journal-next)
            (should (equal (org-entry-get nil "ID") "d3"))
            (should-not (org-find-exact-headline-in-buffer "2001-02-01")))
        (gnosis-test-journal--kill-files (list gnosis-journal-file archive))))))

(ert-deftest gnosis-test-journal-configured-file-preserves-literal-name ()
  "Explicit journal filenames keep surrounding spaces; empty values error."
  (let ((gnosis-journal-dir "/tmp/gnosis-journal-literal")
        (gnosis-journal-file " spaced.org "))
    (should (string-suffix-p "/ spaced.org "
                             (gnosis-journal--configured-file))))
  (let ((gnosis-journal-file ""))
    (should-error (gnosis-journal--configured-file) :type 'user-error)))

(ert-deftest gnosis-test-journal-rejects-fifo-before-navigation ()
  "An existing nonregular journal path errors before read or navigation."
  (skip-unless (executable-find "mkfifo"))
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((dir (expand-file-name "journal" gnosis-test-journal--temp-dir))
             (fifo (expand-file-name "journal.fifo" dir))
             (gnosis-journal-dir dir)
             (gnosis-journal-file fifo)
             (db-called nil)
             (visited nil))
        (make-directory dir t)
        (should (zerop (call-process "mkfifo" nil nil nil fifo)))
        (cl-letf (((symbol-function 'gnosis-nodes-select)
                   (lambda (&rest _)
                     (setq db-called t)
                     (error "Database should not be queried")))
                  ((symbol-function 'find-file)
                   (lambda (file &rest _)
                     (push file visited)
                     (error "find-file should not run")))
                  ((symbol-function 'insert-file-contents)
                   (lambda (file &rest _)
                     (push file visited)
                     (error "insert-file-contents should not run"))))
          (should-error (gnosis-journal) :type 'user-error)
          (should-error (gnosis-journal-find "Topic") :type 'user-error))
        (should-not db-called)
        (should-not visited))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-source-supersedes-renamed-index-date ()
  "A live renamed date does not open the stale indexed ID."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert "#+title: Journal\n* 2026-01-01\n:PROPERTIES:\n:ID: d1\n:END:\n"))
      (gnosis-nodes-update-file gnosis-journal-file)
      (unwind-protect
          (save-window-excursion
            (find-file gnosis-journal-file)
            (goto-char (point-min))
            (re-search-forward "2026-01-01")
            (replace-match "2026-01-05")
            (should-error (gnosis-journal--visit-existing-date "2026-01-01")
                          :type 'user-error)
            (should (equal (org-entry-get nil "ID") "d1"))
            (should-not (org-find-exact-headline-in-buffer "2026-01-01"))
            (gnosis-journal-find "2026-01-05")
            (should (equal (org-entry-get nil "ID") "d1")))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-duplicate-source-ids-fail ()
  "Duplicate IDs in source fail rather than choosing the first heading."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert (concat "#+title: Journal\n"
                        "* 2026-01-01\n:PROPERTIES:\n:ID: dup\n:END:\n"
                        "* 2026-01-02\n:PROPERTIES:\n:ID: dup\n:END:\n")))
      (unwind-protect
          (save-window-excursion
            (should-error (gnosis-journal-find "2026-01-01") :type 'user-error)
            (find-file gnosis-journal-file)
            (should (eq (count-matches "^\\* " (point-min) (point-max)) 2)))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-duplicate-noid-dates-are-ambiguous ()
  "Duplicate no-ID date headings are not collapsed into one entry."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert "#+title: Journal\n* 2026-01-01\nfirst\n* 2026-01-01\nsecond\n"))
      (unwind-protect
          (save-window-excursion
            (should-error (gnosis-journal-find "2026-01-01") :type 'user-error)
            (find-file gnosis-journal-file)
            (should (eq (count-matches "^\\* 2026-01-01$" (point-min) (point-max))
                        2)))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-goto-entry-widens ()
  "Visiting an entry searches the widened source, not a narrowed region."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert (concat "#+title: Journal\n"
                        "* 2026-01-01\n:PROPERTIES:\n:ID: d1\n:END:\n"
                        "* 2026-01-03\n:PROPERTIES:\n:ID: d3\n:END:\n")))
      (unwind-protect
          (save-window-excursion
            (find-file gnosis-journal-file)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^\\* 2026-01-03$")
            (org-narrow-to-subtree)
            (gnosis-journal--goto-entry
             (list "2026-01-01" "2026-01-01" gnosis-journal-file "d1"))
            (should (equal (org-entry-get nil "ID") "d1")))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-missing-id-does-not-fallback-to-title ()
  "A captured ID that disappeared is not replaced by another same-title heading."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert "#+title: Journal\n* Topic\n:PROPERTIES:\n:ID: other\n:END:\n"))
      (unwind-protect
          (save-window-excursion
            (find-file gnosis-journal-file)
            (should-error
             (gnosis-journal--goto-entry
              (list nil "Topic" gnosis-journal-file "missing-id"))
             :type 'user-error)
            (goto-char (point-min))
            (re-search-forward "^\\* Topic$")
            (should (equal (org-entry-get nil "ID") "other"))
            (should (eq (count-matches "^\\* Topic$" (point-min) (point-max)) 1)))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-invalid-template-leaves-no-half-entry ()
  "A non-string template result does not insert a heading or write the file."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (list (cons "Bad" (lambda () 7))))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (should-error (gnosis-journal-find "Half") :type 'user-error)
            (should-not (file-exists-p gnosis-journal-file))
            (when-let* ((buf (get-file-buffer gnosis-journal-file)))
              (with-current-buffer buf
                (goto-char (point-min))
                (should-not (re-search-forward "^\\* Half$" nil t)))))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-expand-error-leaves-no-half-entry ()
  "Heading expansion errors leave the destination without a half entry."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates
            (list (cons "Notes" (lambda () "{*} Daily Notes"))))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'gnosis-org-expand-headings)
                       (lambda (&rest _) (error "expand failed"))))
              (should-error (gnosis-journal-find "Half")))
            (should-not (file-exists-p gnosis-journal-file))
            (when-let* ((buf (get-file-buffer gnosis-journal-file)))
              (with-current-buffer buf
                (goto-char (point-min))
                (should-not (re-search-forward "^\\* Half$" nil t)))))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-failed-create-preserves-unsaved-draft ()
  "A visiting unsaved new-file draft is left intact when creation fails."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (buf nil))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (setq buf (find-file-noselect gnosis-journal-file))
            (with-current-buffer buf
              (insert "unsaved draft\n")
              (should (buffer-modified-p)))
            (cl-letf (((symbol-function 'gnosis-nodes-select-template)
                       (lambda (&rest _) 7)))
              (should-error (gnosis-journal-find "Nope") :type 'user-error))
            (should-not (file-exists-p gnosis-journal-file))
            (with-current-buffer buf
              (should (string-match-p "unsaved draft" (buffer-string)))
              (goto-char (point-min))
              (should-not (re-search-forward "^\\* Nope$" nil t))
              (should (buffer-modified-p))))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-separate-invalid-template-leaves-no-file ()
  "Separate-file mode also validates the template before touching destination."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file nil)
           (gnosis-journal-templates (list (cons "Bad" (lambda () 7))))
           (gnosis-journal-todo-files nil)
           (gnosis-nodes-timestring nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (should-error (gnosis-journal-find "SeparateHalf") :type 'user-error)
            (should (equal (directory-files gnosis-journal-dir nil "\\.org\\'")
                           nil)))
        (gnosis-test-journal--kill-files
         (directory-files gnosis-journal-dir t "\\.org\\'" t))))))

(ert-deftest gnosis-test-journal-separate-template-cannot-retarget-directory ()
  "Separate-file template callbacks cannot redirect writes by changing journal-dir."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (other-dir (expand-file-name "other-journal" gnosis-dir))
           (gnosis-journal-file nil)
           (gnosis-journal-todo-files nil)
           (gnosis-nodes-timestring nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (original-dir nil))
      (make-directory gnosis-journal-dir t)
      (make-directory other-dir t)
      (setq original-dir gnosis-journal-dir)
      (unwind-protect
          (save-window-excursion
            (let ((gnosis-journal-templates
                   (list (cons "Evil"
                               (lambda ()
                                 (setq gnosis-journal-dir other-dir)
                                 "")))))
              (should-error (gnosis-journal-find "Scoped") :type 'user-error))
            (should (equal (directory-files original-dir nil "\\.org\\'") nil))
            (should (equal (directory-files other-dir nil "\\.org\\'") nil)))
        (setq gnosis-journal-dir original-dir)
        (gnosis-test-journal--kill-files
         (append (directory-files original-dir t "\\.org\\'" t)
                 (directory-files other-dir t "\\.org\\'" t)))))))

(ert-deftest gnosis-test-journal-insert-unsaved-link-is-followable ()
  "Public insert makes an unsaved journal ID followable without tracking."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (origin nil))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (setq origin (get-buffer-create " *gnosis-journal-link-origin*"))
            (cl-letf (((symbol-function 'gnosis-nodes--find)
                       (lambda (&rest _) "Unsaved link target")))
              (with-current-buffer origin
                (org-mode)
                (gnosis-journal-insert nil)
                (should (string-match-p "id:" (buffer-string)))
                (should-not (file-exists-p gnosis-journal-file))
                (goto-char (point-min))
                (org-open-at-point)
                (should (equal (org-get-heading t t t t)
                               "Unsaved link target"))
                (should (org-id-get)))
              (with-current-buffer origin
                (goto-char (point-min))
                (gnosis-nodes-goto-id)
                (should (equal (org-get-heading t t t t)
                               "Unsaved link target")))))
        (when (buffer-live-p origin) (kill-buffer origin))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-open-at-point-widens-narrowed-live-target ()
  "Following a live journal ID lands on the heading even if the target is narrowed."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (origin nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert (concat "#+title: Journal\n"
                        "* 2026-01-01\n:PROPERTIES:\n:ID: jan-one\n:END:\n"
                        "** Goals\nkeep goals\n"
                        "* 2026-02-05\n:PROPERTIES:\n:ID: feb-five\n:END:\n")))
      (unwind-protect
          (save-window-excursion
            (find-file gnosis-journal-file)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^\\*\\* Goals$")
            (org-narrow-to-subtree)
            (setq origin (get-buffer-create " *gnosis-journal-follow-origin*"))
            (with-current-buffer origin
              (org-mode)
              (insert "[[id:feb-five][Feb]]\n")
              (goto-char (point-min))
              (org-open-at-point))
            (should (eq (current-buffer) (get-file-buffer gnosis-journal-file)))
            (should-not (buffer-narrowed-p))
            (should (equal (org-entry-get nil "ID") "feb-five"))
            (should (equal (org-get-heading t t t t) "2026-02-05")))
        (when (buffer-live-p origin) (kill-buffer origin))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-open-task-id-widens-narrowed-source ()
  "Following a task ID widens a live narrowed source onto that heading."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (task-file (expand-file-name "tasks.org" gnosis-dir))
           (gnosis-journal-todo-files (list task-file))
           (gnosis-journal-todo-keywords '("TODO"))
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert "#+title: Journal\n* 2026-01-03\n[[id:task-one][Task]]\n"))
      (with-temp-file task-file
        (insert (concat "#+title: Tasks\n"
                        "* Other\nkeep other\n"
                        "* TODO Target\n:PROPERTIES:\n:ID: task-one\n:END:\n")))
      (unwind-protect
          (save-window-excursion
            (find-file task-file)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^\\* Other$")
            (org-narrow-to-subtree)
            (find-file gnosis-journal-file)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "id:task-one")
            (org-open-at-point)
            (should (equal (expand-file-name (buffer-file-name))
                           (expand-file-name task-file)))
            (should-not (buffer-narrowed-p))
            (should (equal (org-entry-get nil "ID") "task-one"))
            (should (equal (org-get-heading t t t t) "Target")))
        (gnosis-test-journal--kill-files (list gnosis-journal-file task-file))))))

(ert-deftest gnosis-test-journal-missing-id-does-not-read-unopened-journal ()
  "A missing non-journal ID does not visit the unopened journal file."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (visited nil)
           (orig-find (symbol-function 'find-file))
           (orig-insert (symbol-function 'insert-file-contents)))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert "#+title: Journal\n* 2026-01-01\n:PROPERTIES:\n:ID: d1\n:END:\n"))
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'find-file)
                       (lambda (file &rest args)
                         (when (equal (expand-file-name file)
                                      (expand-file-name gnosis-journal-file))
                           (push file visited))
                         (apply orig-find file args)))
                      ((symbol-function 'insert-file-contents)
                       (lambda (file &rest args)
                         (when (equal (expand-file-name file)
                                      (expand-file-name gnosis-journal-file))
                           (push file visited))
                         (apply orig-insert file args))))
              (with-temp-buffer
                (org-mode)
                (insert "[[id:not-a-journal-id][x]]\n")
                (goto-char (point-min))
                (let ((org-id-track-globally nil))
                  (should-error (org-open-at-point)))
                (should-error (gnosis-nodes-goto-id "not-a-journal-id"))))
            (should-not visited)
            (should-not (get-file-buffer gnosis-journal-file)))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-insert-separate-layout-uses-journal-path ()
  "Nil layout insert does not save or go through gnosis-nodes-insert."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file nil)
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (gnosis-nodes-timestring nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (nodes-insert-called nil)
           (origin nil))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (setq origin (get-buffer-create " *gnosis-journal-sep-origin*"))
            (cl-letf (((symbol-function 'gnosis-nodes--find)
                       (lambda (&rest _) "SeparateTarget"))
                      ((symbol-function 'gnosis-nodes-insert)
                       (lambda (&rest _)
                         (setq nodes-insert-called t)
                         (error "nodes-insert should not run"))))
              (with-current-buffer origin
                (org-mode)
                (gnosis-journal-insert nil)
                (should (string-match-p "id:" (buffer-string)))
                (goto-char (point-min))
                (org-open-at-point)
                (should (equal (cadar (org-collect-keywords '("TITLE")))
                               "SeparateTarget"))
                (should (buffer-modified-p))
                (should-not (file-exists-p (buffer-file-name)))))
            (should-not nodes-insert-called))
        (when (buffer-live-p origin) (kill-buffer origin))
        (gnosis-test-journal--kill-files
         (directory-files gnosis-journal-dir t "\\.org\\'" t))))))

(ert-deftest gnosis-test-journal-get-todos-does-not-change-live-non-org ()
  "Collection does not switch a live non-Org visiting buffer to org-mode."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "todos.org" "#+title: T\n* TODO LiveTitle\n"))
             (gnosis-journal-todo-keywords '("TODO"))
             (gnosis-journal-todo-files (list file))
             (buf (find-file-noselect file)))
        (with-current-buffer buf
          (fundamental-mode)
          (goto-char (point-min))
          (let ((pos (point))
                (narrow-end (point-max)))
            (narrow-to-region (point-min) (point-max))
            (let ((todos (gnosis-journal-get--todos file)))
              (should (equal (caar todos) "LiveTitle"))
              (should (eq major-mode 'fundamental-mode))
              (should (= (point) pos))
              (should (= (point-max) narrow-end)))))
        (gnosis-test-journal--kill-file-buffer file))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-create-task-id-rejects-selection-mutation ()
  "Mutating the source during task selection does not create an ID."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "tasks.org" "#+title: Tasks\n* TODO Alpha\n* TODO Beta\n"))
             (gnosis-journal-todo-files (list file))
             (gnosis-journal-todo-keywords '("TODO"))
             (gnosis-journal-bullet-point-char "+")
             (org-id-track-globally nil)
             (buf (find-file-noselect file))
             (gnosis-nodes-completing-read-func
              (lambda (_prompt candidates)
                (with-current-buffer buf
                  (erase-buffer)
                  (insert "#+title: Tasks\n* TODO Alpha\n"))
                (car candidates))))
        (with-current-buffer buf (org-mode))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (with-temp-buffer
            (org-mode)
            (should-error (gnosis-journal-insert-task) :type 'user-error)))
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "TODO Alpha")
          (should-not (org-id-get)))
        (gnosis-test-journal--kill-file-buffer file))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-create-task-id-rejects-confirmation-mutation ()
  "Mutating the source during ID-creation confirmation does not create an ID."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "tasks.org" "#+title: Tasks\n* TODO Alpha\n"))
             (gnosis-journal-todo-files (list file))
             (gnosis-journal-todo-keywords '("TODO"))
             (gnosis-journal-bullet-point-char "+")
             (org-id-track-globally nil)
             (buf (find-file-noselect file))
             (gnosis-nodes-completing-read-func
              (lambda (_prompt candidates) (car candidates))))
        (with-current-buffer buf (org-mode))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert "#+title: Tasks\n* TODO Alpha\n"))
                     t)))
          (with-temp-buffer
            (org-mode)
            (should-error (gnosis-journal-insert-task) :type 'user-error)))
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "TODO Alpha")
          (should-not (org-id-get)))
        (gnosis-test-journal--kill-file-buffer file))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-create-task-id-positive-control ()
  "An unchanged live heading receives the created ID."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "tasks.org" "#+title: Tasks\n* TODO Alpha\n"))
             (gnosis-journal-todo-files (list file))
             (gnosis-journal-todo-keywords '("TODO"))
             (gnosis-journal-bullet-point-char "+")
             (org-id-track-globally nil)
             (buf (find-file-noselect file))
             (gnosis-nodes-completing-read-func
              (lambda (_prompt candidates) (car candidates))))
        (with-current-buffer buf (org-mode))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (with-temp-buffer
            (org-mode)
            (gnosis-journal-insert-task)
            (should (string-match-p "id:" (buffer-string)))))
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "TODO Alpha")
          (should (org-id-get))
          (should (buffer-modified-p)))
        (gnosis-test-journal--kill-file-buffer file))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-insert-task-same-file-new-id-retains-origin ()
  "Same-file confirmed ID creation inserts at origin and leaves the source task."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "journal.org"
                    (concat "#+title: Journal\n"
                            "* 2026-01-01\n:PROPERTIES:\n:ID: source-day\n:END:\n"
                            "** TODO Task inside journal\nBody.\n"
                            "* 2026-01-03\n:PROPERTIES:\n:ID: origin-day\n:END:\n"
                            "HERE\n")))
             (gnosis-journal-todo-files (list file))
             (gnosis-journal-todo-keywords '("TODO"))
             (gnosis-journal-bullet-point-char "+")
             (org-id-track-globally nil)
             (create-lockfiles nil)
             (buf (find-file-noselect file))
             (gnosis-nodes-completing-read-func
              (lambda (_prompt candidates) (car candidates)))
             (on-disk (with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string))))
        (with-current-buffer buf
          (org-mode)
          (goto-char (point-min))
          (search-forward "HERE")
          (beginning-of-line)
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (gnosis-journal-insert-task))
          (should (equal (org-get-heading t t t t) "2026-01-03"))
          (should (looking-at "HERE"))
          (save-excursion
            (forward-line -1)
            (should (string-match-p
                     "id:"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
          (goto-char (point-min))
          (should (re-search-forward "^\\*\\* TODO Task inside journal$" nil t))
          (should (org-id-get))
          (should (org-up-heading-safe))
          (should (equal (org-get-heading t t t t) "2026-01-01"))
          (should (equal
                   (org-element-map (org-element-parse-buffer) 'headline
                     (lambda (headline)
                       (and (= (org-element-property :level headline) 1)
                            (org-element-property :raw-value headline)))
                     nil nil 'headline)
                   '("2026-01-01" "2026-01-03")))
          (should (buffer-modified-p))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         on-disk)))
        (gnosis-test-journal--kill-file-buffer file))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-create-task-id-disk-source-preserves-point ()
  "Disk-source ID creation does not move point in the same visiting buffer."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "journal.org"
                    (concat "#+title: Journal\n"
                            "* 2026-01-01\n:PROPERTIES:\n:ID: source-day\n:END:\n"
                            "** TODO Task inside journal\nBody.\n"
                            "* 2026-01-03\n:PROPERTIES:\n:ID: origin-day\n:END:\n"
                            "HERE\n")))
             (org-id-track-globally nil)
             (create-lockfiles nil)
             (buf (find-file-noselect file))
             (on-disk (with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string))))
        (with-current-buffer buf
          (org-mode)
          (goto-char (point-min))
          (re-search-forward "^\\*\\* TODO Task inside journal$")
          (beginning-of-line)
          (let ((begin (point))
                (line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))
                (digest (secure-hash 'sha1 (current-buffer))))
            (goto-char (point-min))
            (search-forward "HERE")
            (beginning-of-line)
            (should (gnosis-journal--create-task-id
                     "Task inside journal" file begin nil nil line digest))
            (should (equal (org-get-heading t t t t) "2026-01-03"))
            (should (looking-at "HERE"))
            (goto-char begin)
            (should (org-id-get)))
          (should (buffer-modified-p))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         on-disk)))
        (gnosis-test-journal--kill-file-buffer file))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-task-id-at-point-ignores-other-link ()
  "Point on a non-ID link does not fall back to another ID on the line."
  (with-temp-buffer
    (org-mode)
    (insert "+ [ ] [[https://example.com][site]] [[id:task-one][One]]\n")
    (goto-char (point-min))
    (re-search-forward "https:")
    (should-not (gnosis-journal--task-id-at-point))))

(ert-deftest gnosis-test-journal-task-id-at-point-unambiguous-line ()
  "Outside a link, a single ID on the line is used; two IDs are not."
  (with-temp-buffer
    (org-mode)
    (insert "+ [ ] [[id:task-one][One]]\n")
    (goto-char (point-min))
    (should (equal (gnosis-journal--task-id-at-point) "task-one"))
    (erase-buffer)
    (insert "+ [ ] [[id:task-one][One]] [[id:task-two][Two]]\n")
    (goto-char (point-min))
    (should-not (gnosis-journal--task-id-at-point))))

(ert-deftest gnosis-test-journal-todo-files-deduplicate-physical-sources ()
  "A file listed both directly and via a directory is one source."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((agenda-dir (expand-file-name "agenda" gnosis-test-journal--temp-dir))
             (file nil)
             (gnosis-journal-todo-keywords '("TODO")))
        (make-directory agenda-dir)
        (setq file (expand-file-name "inside.org" agenda-dir))
        (with-temp-file file
          (insert "#+title: Inside\n* TODO One\n:PROPERTIES:\n:ID: only-once\n:END:\n"))
        (let ((gnosis-journal-todo-files (list agenda-dir file)))
          (should (= 1 (length (gnosis-journal--todo-files))))
          (should (file-equal-p (gnosis-journal--locate-task-id "only-once")
                                file))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-duplicate-id-across-unique-sources-still-fails ()
  "Distinct files that share an ID still error."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((one (gnosis-test-journal--create-file
                   "one.org"
                   "#+title: One\n* TODO A\n:PROPERTIES:\n:ID: shared\n:END:\n"))
             (two (gnosis-test-journal--create-file
                   "two.org"
                   "#+title: Two\n* TODO B\n:PROPERTIES:\n:ID: shared\n:END:\n"))
             (gnosis-journal-todo-files (list one two)))
        (should-error (gnosis-journal--locate-task-id "shared") :type 'user-error))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-sync-deduplicates-symlink-single-file ()
  "A single-file symlink that aliases a listed archive is synced once."
  (skip-unless (fboundp 'make-symbolic-link))
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (old (expand-file-name "old.org" gnosis-journal-dir))
           (link (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-file link)
           (gnosis-journal-todo-files nil)
           (seen nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file old
        (insert "#+title: 2000-01-01\n:PROPERTIES:\n:ID: old\n:END:\n"))
      (make-symbolic-link old link)
      (cl-letf (((symbol-function 'gnosis-nodes-update-file)
                 (lambda (file &optional _)
                   (push (file-truename file) seen))))
        (gnosis-journal-db-sync t))
      (should (= 1 (length (delete-dups seen)))))))

(ert-deftest gnosis-test-journal-separate-sections-retain-date-identity ()
  "Capture and gap navigation retain a daily file with ordinary sections."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file nil)
           (gnosis-journal-as-gpg nil)
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           (selections 0))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'gnosis-nodes-select-template)
                       (lambda (&rest _) (cl-incf selections) "")))
              (gnosis-journal-date "2003-04-05")
              (let ((file buffer-file-name)
                    (id (org-entry-get (point-min) "ID")))
                (goto-char (point-max))
                (insert "\n* Reflection\nAn unsaved section.\n")
                (gnosis-journal-capture "A later reflection.")
                (should (equal buffer-file-name file))
                (should (= selections 1))
                (should (equal (org-entry-get (point-min) "ID") id))
                (should (string-match-p "An unsaved section" (buffer-string)))
                (should (string-match-p "A later reflection" (buffer-string)))
                (save-buffer)
                (gnosis-journal-date "2003-04-08")
                (gnosis-journal-previous)
                (should (equal buffer-file-name file))
                (should (equal (org-entry-get (point-min) "ID") id))
                (should (= selections 2)))))
        (gnosis-test-journal--kill-files
         (directory-files gnosis-journal-dir t "\\.org\\'"))))))

(ert-deftest gnosis-test-journal-separate-root-reopens-by-id ()
  "Empty dated files and named files with sections retain their root IDs."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file nil)
           (gnosis-journal-as-gpg nil)
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (unwind-protect
          (save-window-excursion
            (dolist (fixture '(("2003-04-05" . "")
                               ("A named journal" . "* Reflection\nNotes.\n")))
              (cl-letf (((symbol-function 'gnosis-nodes-select-template)
                         (lambda (&rest _) (cdr fixture))))
                (gnosis-journal-find (car fixture)))
              (let ((file buffer-file-name)
                    (id (org-entry-get (point-min) "ID")))
                (save-buffer)
                (kill-buffer (current-buffer))
                (cl-letf (((symbol-function 'gnosis-nodes-select-template)
                           (lambda (&rest _) (ert-fail "Existing entry was recreated"))))
                  (gnosis-journal-find (car fixture)))
                (should (equal buffer-file-name file))
                (should (org-before-first-heading-p))
                (should (equal (org-entry-get nil "ID") id)))))
        (gnosis-test-journal--kill-files
         (directory-files gnosis-journal-dir t "\\.org\\'"))))))

(ert-deftest gnosis-test-journal-unindexed-daily-file-keeps-identity ()
  "Opening retained daily files before sync must not create a second day."
  (dolist (name '("2026-01-01.org" "202601011200--2026-01-01.org"))
    (gnosis-test-with-db
      (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
             (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
             (gnosis-journal-file t)
             (gnosis-journal-as-gpg nil)
             (gnosis-journal-todo-files nil)
             (org-id-track-globally nil)
             (create-lockfiles nil)
             (file (expand-file-name name gnosis-journal-dir))
             (single (expand-file-name "journal.org" gnosis-journal-dir)))
        (make-directory gnosis-journal-dir t)
        (with-temp-file file
          (insert ":PROPERTIES:\n:ID: legacy-root\n:END:\n#+title: 2026-01-01\n"
                  "* Reflection\nKeep this dated file.\n"))
        (unwind-protect
            (save-window-excursion
              (cl-letf (((symbol-function 'gnosis-nodes-select-template)
                         (lambda (&rest _) "")))
                (gnosis-journal-find "2026-01-01"))
              (should (equal buffer-file-name file))
              (should (equal (org-id-get) "legacy-root"))
              (save-buffer)
              (gnosis-journal-db-sync t)
              (kill-buffer (current-buffer))
              (gnosis-journal-date "2026-01-01")
              (should (equal buffer-file-name file))
              (should (equal (org-id-get) "legacy-root"))
              (should-not (get-file-buffer single))
              (should-not (file-exists-p single)))
          (gnosis-test-journal--kill-files
           (directory-files gnosis-journal-dir t "\\.org\\'" t)))))))

(ert-deftest gnosis-test-journal-unindexed-noid-daily-file-keeps-identity ()
  "Dated files without a file-level ID still own that day.
Ordinary sections must not hide the dated TITLE root or mint a second
day in journal.org.  `.org.gpg' here is a native name, not encryption."
  (dolist (spec '(("2026-01-01.org" . "2026-01-01")
                  ("202601011200--2026-01-01.org" . "2026-01-01")
                  ("2026-01-01.org.gpg" . "2026-01-01")
                  (today . today)))
    (gnosis-test-with-db
      (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
             (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
             (gnosis-journal-file t)
             (gnosis-journal-as-gpg nil)
             (gnosis-journal-todo-files nil)
             (org-id-track-globally nil)
             (create-lockfiles nil)
             (today (format-time-string "%Y-%m-%d"))
             (title (if (eq (cdr spec) 'today) today (cdr spec)))
             (name (if (eq (car spec) 'today)
                       (concat today ".org")
                     (car spec)))
             (file (expand-file-name name gnosis-journal-dir))
             (single (expand-file-name "journal.org" gnosis-journal-dir))
             (original (format "#+title: %s\n* Reflection\nKeep this dated file.\n"
                               title)))
        (make-directory gnosis-journal-dir t)
        (gnosis-test-journal--without-epa
         (with-temp-file file
           (insert original)))
        (unwind-protect
            (save-window-excursion
              (gnosis-test-journal--without-epa
               (cl-letf (((symbol-function 'gnosis-nodes-select-template)
                          (lambda (&rest _) "")))
                 (if (eq (car spec) 'today)
                     (gnosis-journal)
                   (gnosis-journal-find title)))
               (should (equal buffer-file-name file))
               (should (org-before-first-heading-p))
               (should (equal (gnosis-journal--date-at-point) title))
               (should-not (org-id-get))
               (should-not (org-entry-get (point-min) "ID"))
               (should-not (buffer-modified-p))
               (should-not (string-match-p ":ID:" (buffer-string)))
               (goto-char (point-max))
               (gnosis-journal--goto-entry (list title title file nil))
               (should (org-before-first-heading-p))
               (should-not (org-id-get))
               (should-not (buffer-modified-p))
               (gnosis-journal-db-sync t)
               (should-not (org-id-get))
               (should-not (buffer-modified-p))
               (should-not (string-match-p ":ID:" (buffer-string)))
               (kill-buffer (current-buffer))
               (if (eq (car spec) 'today)
                   (gnosis-journal)
                 (gnosis-journal-date title))
               (should (equal buffer-file-name file))
               (should (org-before-first-heading-p))
               (should (equal (gnosis-journal--date-at-point) title))
               (should-not (org-id-get))
               (should-not (get-file-buffer single))
               (should-not (file-exists-p single))
               (with-temp-buffer
                 (insert-file-contents file)
                 (should (equal (buffer-string) original)))))
          (gnosis-test-journal--kill-files
           (directory-files gnosis-journal-dir t "\\.org" t)))))))

(ert-deftest gnosis-test-journal-narrowed-date-context-preserves-state ()
  "Date lookup widens heading and TITLE context without changing restriction."
  (with-temp-buffer
    (org-mode)
    (insert (concat "* 2026-01-01\n* 2026-01-03\n** Goals\nFocus here.\n"
                    "* 2026-01-05\n"))
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Goals$")
    (forward-line 1)
    (org-narrow-to-subtree)
    (let ((origin (point))
          (beg (point-min))
          (end (point-max)))
      (should (buffer-narrowed-p))
      (should (equal (gnosis-journal--date-at-point) "2026-01-03"))
      (should (= (point) origin))
      (should (= (point-min) beg))
      (should (= (point-max) end))
      (should-not (save-excursion
                    (goto-char (point-min))
                    (re-search-forward "^\\* 2026-01-03$" nil t)))))
  (with-temp-buffer
    (org-mode)
    (insert "#+title: 2026-01-03\n* Reflection\nKeep this dated file.\n")
    (goto-char (point-min))
    (re-search-forward "^\\* Reflection$")
    (forward-line 1)
    (org-narrow-to-subtree)
    (let ((origin (point))
          (beg (point-min))
          (end (point-max)))
      (should (buffer-narrowed-p))
      (should (equal (gnosis-journal--date-at-point) "2026-01-03"))
      (should (= (point) origin))
      (should (= (point-min) beg))
      (should (= (point-max) end))
      (should-not (org-entry-get (point-min) "ID"))
      (should-not (save-excursion
                    (goto-char (point-min))
                    (re-search-forward "2026-01-03" nil t))))))

(ert-deftest gnosis-test-journal-narrowed-previous-next-dispatch ()
  "Prefix p/n from a narrowed section visit the enclosing day's neighbors."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil))
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert (concat "#+title: Journal\n"
                        "* 2026-01-01\n:PROPERTIES:\n:ID: d1\n:END:\n"
                        "* 2026-01-03\n:PROPERTIES:\n:ID: d3\n:END:\n"
                        "** Goals\nFocus here.\n"
                        "* 2026-01-05\n:PROPERTIES:\n:ID: d5\n:END:\n")))
      (unwind-protect
          (save-window-excursion
            (find-file gnosis-journal-file)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^\\*\\* Goals$")
            (forward-line 1)
            (org-narrow-to-subtree)
            (call-interactively
             (keymap-lookup gnosis-journal-prefix-map "p"))
            (should (equal (org-entry-get nil "ID") "d1"))
            (should (equal (org-get-heading t t t t) "2026-01-01"))
            (goto-char (point-min))
            (re-search-forward "^\\*\\* Goals$")
            (forward-line 1)
            (org-narrow-to-subtree)
            (call-interactively
             (keymap-lookup gnosis-journal-prefix-map "n"))
            (should (equal (org-entry-get nil "ID") "d5"))
            (should (equal (org-get-heading t t t t) "2026-01-05")))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(ert-deftest gnosis-test-journal-narrowed-capture-and-study-date ()
  "Capture and study from a narrowed section keep the enclosing journal date."
  (require 'gnosis-study)
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-journal-dir))
           (gnosis-journal-templates (gnosis-test-journal--empty-templates))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (create-lockfiles nil)
           seen)
      (make-directory gnosis-journal-dir t)
      (with-temp-file gnosis-journal-file
        (insert (concat "#+title: Journal\n"
                        "* 2026-01-01\n:PROPERTIES:\n:ID: d1\n:END:\n"
                        "* 2026-01-03\n:PROPERTIES:\n:ID: d3\n:END:\n"
                        "** Goals\nFocus here.\n"
                        "* 2026-01-05\n:PROPERTIES:\n:ID: d5\n:END:\n")))
      (unwind-protect
          (save-window-excursion
            (find-file gnosis-journal-file)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^\\*\\* Goals$")
            (forward-line 1)
            (org-narrow-to-subtree)
            (cl-letf (((symbol-function 'gnosis-study-day)
                       (lambda (date &rest _) (setq seen date))))
              (call-interactively
               (keymap-lookup gnosis-journal-prefix-map "s")))
            (should (equal seen 20260103))
            (should (buffer-narrowed-p))
            (gnosis-journal-capture "from goals")
            (should (string-match-p "from goals" (buffer-string)))
            (should (= (how-many "^\\* " (point-min) (point-max)) 3))
            (goto-char (point-min))
            (re-search-forward "from goals")
            (should (equal (gnosis-journal--date-at-point) "2026-01-03")))
        (gnosis-test-journal--kill-files (list gnosis-journal-file))))))

(provide 'gnosis-test-journal)

;;; gnosis-test-journal.el ends here
