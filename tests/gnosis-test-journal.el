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

(load (expand-file-name "gnosis-test-helpers.el"
       (file-name-directory (or load-file-name buffer-file-name))))

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
      (let ((gnosis-journal-todo-keywords '("TODO" "NEXT" "|" "DONE"))
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
      (let ((gnosis-journal-todo-keywords '("TODO" "|" "DONE"))
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
      (let ((gnosis-journal-todo-keywords '("TODO" "|" "DONE"))
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
      (let ((gnosis-journal-todo-keywords '("TODO" "|" "DONE"))
            (file (gnosis-test-journal--create-file "empty.org" "")))
        (should (null (gnosis-journal-get--todos file))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-get--todos-keywords-after-pipe ()
  "Keywords after pipe are not collected."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let ((gnosis-journal-todo-keywords '("TODO" "|" "DONE" "CANCELLED"))
            (file (gnosis-test-journal--create-file
                   "pipe.org"
                   "#+title: Tasks\n\n* TODO Active\n\n* DONE Finished\n\n* CANCELLED Dropped\n")))
        (let ((todos (gnosis-journal-get--todos file)))
          (should (= 1 (length todos)))
          (should (string= "Active" (car (car todos))))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-get--todos-multiple-keywords ()
  "NEXT keyword is collected when before the pipe."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let ((gnosis-journal-todo-keywords '("TODO" "NEXT" "|" "DONE"))
            (org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE")))
            (file (gnosis-test-journal--create-file
                   "multi.org"
                   "#+title: Tasks\n\n* TODO First\n\n* NEXT Second\n\n* DONE Third\n")))
        (let ((todos (gnosis-journal-get--todos file)))
          (should (= 2 (length todos)))
          (should (string= "First" (car (nth 0 todos))))
          (should (string= "Second" (car (nth 1 todos))))))
    (gnosis-test-journal--teardown)))

;;; ---- Group 2: gnosis-journal-get-todos ----

(ert-deftest gnosis-test-journal-get-todos-multiple-files ()
  "TODOs from multiple files are combined."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO" "|" "DONE"))
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
  (let ((gnosis-journal-todo-keywords '("TODO" "|" "DONE")))
    (should (null (gnosis-journal-get-todos '())))))

(ert-deftest gnosis-test-journal-get-todos-mixed-files ()
  "Only TODO items are collected across files."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO" "|" "DONE"))
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
      (let* ((gnosis-journal-todo-keywords '("TODO" "|" "DONE"))
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
      (let* ((gnosis-journal-todo-keywords '("TODO" "|" "DONE"))
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
      (let* ((gnosis-journal-todo-keywords '("TODO" "|" "DONE"))
             (f (gnosis-test-journal--create-file
                 "empty.org" "#+title: T\n\n* DONE Already done\n"))
             (gnosis-journal-todo-files (list f)))
        (should (string= "" (gnosis-journal-todos))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-todos-custom-bullet ()
  "Custom bullet point character is used."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO" "|" "DONE"))
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
  "Marks TODO as DONE and sets LAST_DONE_DATE."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "tasks.org"
                    "#+title: Tasks\n\n* TODO Exercise\nSCHEDULED: <2026-03-07>\n"))
             (gnosis-journal-todo-files (list file)))
        (gnosis-journal-mark-todo-as-done "Exercise")
        (gnosis-test-journal--kill-file-buffer file)
        (with-temp-buffer
          (insert-file-contents file)
          (should (string-match-p "DONE Exercise" (buffer-string)))
          (should (string-match-p "LAST_DONE_DATE" (buffer-string)))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-mark-todo-as-done-already-done ()
  "Already-done item is not re-marked."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "tasks.org"
                    "#+title: Tasks\n\n* DONE Finished\n"))
             (gnosis-journal-todo-files (list file)))
        (gnosis-journal-mark-todo-as-done "Finished")
        (gnosis-test-journal--kill-file-buffer file)
        (with-temp-buffer
          (insert-file-contents file)
          (should (string-match-p "DONE Finished" (buffer-string)))
          (should-not (string-match-p "LAST_DONE_DATE" (buffer-string)))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-mark-todo-as-done-nonexistent ()
  "Nonexistent heading causes no error."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((file (gnosis-test-journal--create-file
                    "tasks.org"
                    "#+title: Tasks\n\n* TODO Real task\n"))
             (gnosis-journal-todo-files (list file)))
        (gnosis-journal-mark-todo-as-done "Nonexistent heading")
        (with-temp-buffer
          (insert-file-contents file)
          (should (string-match-p "TODO Real task" (buffer-string)))))
    (gnosis-test-journal--teardown)))

;;; ---- Group 8: gnosis-journal--update-todos ----

(ert-deftest gnosis-test-journal-update-todos-checks-mark-done ()
  "Checked items in journal mark source TODOs as done."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO" "|" "DONE"))
             (gnosis-journal-file nil)
             (todo-file (gnosis-test-journal--create-file
                         "tasks.org"
                         "#+title: Tasks\n\n* TODO Exercise\nSCHEDULED: <2026-03-07>\n"))
             (journal-file (gnosis-test-journal--create-file
                            "2026-03-07.org"
                            "#+title: 2026-03-07\n\n* Goals\n+ [X] Exercise\n+ [ ] Meditate\n"))
             (gnosis-journal-todo-files (list todo-file)))
        (gnosis-journal--update-todos journal-file)
        (gnosis-test-journal--kill-file-buffer todo-file)
        (with-temp-buffer
          (insert-file-contents todo-file)
          (should (string-match-p "DONE Exercise" (buffer-string)))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-update-todos-single-file-scopes-today ()
  "Single-file journal scopes checked items to today's heading only."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO" "|" "DONE"))
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
          (should (string-match-p "DONE TodayTask" (buffer-string)))
          (should (string-match-p "TODO OldTask" (buffer-string)))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-update-todos-previous-date-no-false-positive ()
  "Previous date checked items do NOT mark TODOs as done."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO" "|" "DONE"))
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
          (should (string-match-p "DONE TodayTask" (buffer-string)))
          (should (string-match-p "TODO YesterdayTask" (buffer-string)))))
    (gnosis-test-journal--teardown)))

(ert-deftest gnosis-test-journal-update-todos-no-today-heading ()
  "No today heading in single-file journal marks nothing."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO" "|" "DONE"))
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
  "Separate-file mode processes all checked items."
  (gnosis-test-journal--setup)
  (unwind-protect
      (let* ((gnosis-journal-todo-keywords '("TODO" "|" "DONE"))
             (gnosis-journal-file nil)
             (journal-file (gnosis-test-journal--create-file
                            "2026-03-07.org"
                            "#+title: 2026-03-07\n\n* Notes\n+ [X] TaskA\n\n* Goals\n+ [X] TaskB\n"))
             (todo-file (gnosis-test-journal--create-file
                         "tasks.org"
                         "#+title: Tasks\n\n* TODO TaskA\n\n* TODO TaskB\n"))
             (gnosis-journal-todo-files (list todo-file)))
        (gnosis-journal--update-todos journal-file)
        (gnosis-test-journal--kill-file-buffer todo-file)
        (with-temp-buffer
          (insert-file-contents todo-file)
          (should (string-match-p "DONE TaskA" (buffer-string)))
          (should (string-match-p "DONE TaskB" (buffer-string)))))
    (gnosis-test-journal--teardown)))

(provide 'gnosis-test-journal)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-journal.el ends here
