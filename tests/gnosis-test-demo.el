;;; gnosis-test-demo.el --- Bundled demo regression tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Exercise the shipped SQLite collection, not a reconstruction of its rows.

;;; Code:

(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-export-import)
(require 'gnosis-review)

(defconst gnosis-test-demo--file
  (expand-file-name "../collections/demo.gnosis"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Repository fixture resolved independently of the working directory.")

(defconst gnosis-test-demo--content
  '((1 "basic" "Translate into English: repetitio est mater memoriae."
       ("Use the literal translation.") ("repetition is the mother of memory")
       "The Latin words repetitio, mater, and memoriae mean repetition, mother, and of memory."
       nil)
    (2 "cloze" "Richard Stallman began developing GNU Emacs in 1984."
       ("year") ("1984")
       "Development began in 1984; the first public release followed in 1985."
       nil)
    (3 "cloze" "Richard Stallman began developing GNU Emacs in 1984."
       ("first name" "surname") ("Richard" "Stallman")
       "Richard Stallman started GNU Emacs.  GNU Emacs is distinct from the earlier TECO-based Emacs."
       nil)
    (4 "mcq" "Which route delivers vancomycin for a systemic infection, rather than an intestinal infection?"
       ("Intramuscular (IM)" "Intravenous (IV)" "Oral") ("Intravenous (IV)")
       "Oral vancomycin is poorly absorbed and acts in the intestine, for example in C. difficile infection.  Intravenous vancomycin reaches the bloodstream."
       nil)
    (5 "mc-cloze" "Thiazide diuretics may cause hyponatremia as an adverse effect."
       ("hyponatremia" "hypernatremia") ("hyponatremia")
       "Thiazides impair urinary dilution, reducing the kidney's ability to excrete free water.  Hyponatremia means a low blood sodium concentration."
       nil))
  "Expected ID, type, question, hints/choices, answers, explanation and image.")

(defmacro gnosis-test-demo--with-fixture (&rest body)
  "Run BODY with a private copy of the shipped demo and a fresh database."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-db
     (let* ((before (gnosis-import--file-sha256 gnosis-test-demo--file))
            (file (expand-file-name "demo.gnosis" gnosis-dir))
            (buffers (buffer-list))
            (default-directory gnosis-dir)
            (register-alist nil)
            (gnosis-review-buffer-name "*Gnosis Demo Test*")
            (gnosis-review-basic-input 'typed))
       (copy-file gnosis-test-demo--file file)
       (unwind-protect
           (save-window-excursion ,@body)
         (dolist (buffer (seq-difference (buffer-list) buffers))
           (when (string-prefix-p "*Gnosis" (buffer-name buffer))
             (kill-buffer buffer)))
         (should (equal before (gnosis-import--file-sha256
                                gnosis-test-demo--file)))))))

(defun gnosis-test-demo--import (file)
  "Preview FILE through the public command and apply its native binding."
  (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) file)))
    (call-interactively #'gnosis-import-db))
  (should (derived-mode-p 'gnosis-import-diff-mode))
  (should (equal '(1 2 3 4 5) (sort (copy-sequence gnosis-import--new-ids) #'<)))
  (should-not gnosis-import--changed-ids)
  (should (eq (key-binding (kbd "a")) #'gnosis-import-apply))
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
    (call-interactively (key-binding (kbd "a")))))

(defun gnosis-test-demo--rows ()
  "Return decoded portable content in stable ID order."
  (gnosis-sqlite-select
   gnosis-db "SELECT t.id, t.type, t.keimenon, t.hypothesis, t.answer,
                     e.parathema, e.review_image
                FROM themata t JOIN extras e ON e.id = t.id ORDER BY t.id"))

(defun gnosis-test-demo--evidence ()
  "Read scheduler and accepted study evidence in stable order."
  (mapcar (lambda (table)
            (sqlite-select gnosis-db (concat "SELECT * FROM " table " ORDER BY 1")))
          '("scheduler_baseline" "scheduler_state" "review_events" "practice_events")))

(ert-deftest gnosis-test-demo-content-and-roundtrip ()
  "Import the exact demo, reopen it, and round-trip only its portable content."
  (gnosis-test-demo--with-fixture
    (gnosis-test-demo--import file)
    (should (equal gnosis-test-demo--content (gnosis-test-demo--rows)))
    (should (equal '((1 nil) (2 nil) (3 nil) (4 nil) (5 nil))
                   (gnosis-sqlite-select gnosis-db
                     "SELECT id, accepted_aliases FROM themata ORDER BY id")))
    (dolist (row (gnosis-test-demo--rows))
      (should-not (string-search "\\" (nth 5 row))))
    (let ((today (gnosis--today-int))
          (content (gnosis-test-demo--rows))
          (tags (gnosis-sqlite-select gnosis-db
                  "SELECT * FROM thema_tag ORDER BY thema_id, tag"))
          (export (expand-file-name "roundtrip.gnosis" gnosis-dir)))
      (should (equal (mapcar (lambda (id) (list id today 0 0)) '(1 2 3 4 5))
                     (gnosis-sqlite-select gnosis-db
                       "SELECT * FROM scheduler_baseline ORDER BY thema_id")))
      (should (equal (mapcar (lambda (id) (list id 0 0 0 today)) '(1 2 3 4 5))
                     (gnosis-sqlite-select gnosis-db
                       "SELECT thema_id, reps, lapses, suspended, due_day
                          FROM scheduler_state ORDER BY thema_id")))
      (gnosis-sqlite-close gnosis-db)
      (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
      (gnosis-db-init)
      (should (equal content (gnosis-test-demo--rows)))
      (should (equal '(("ok")) (sqlite-select gnosis-db "PRAGMA integrity_check")))
      (should-not (sqlite-select gnosis-db "PRAGMA foreign_key_check"))
      (gnosis-export-db export)
      (gnosis-test-with-db
        (gnosis-test-demo--import export)
        (should (equal content (gnosis-test-demo--rows)))
        (should (equal tags (gnosis-sqlite-select gnosis-db
                             "SELECT * FROM thema_tag ORDER BY thema_id, tag")))))))

(ert-deftest gnosis-test-demo-import-cancel ()
  "Declining Apply or quitting the exact demo preview imports nothing."
  (gnosis-test-demo--with-fixture
    (let ((before (gnosis-test-demo--evidence)))
      (gnosis-import-db file)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (should-error (call-interactively (key-binding (kbd "a"))) :type 'user-error))
      (should-not (gnosis-test-demo--rows))
      (should (equal before (gnosis-test-demo--evidence)))
      (call-interactively (key-binding (kbd "q")))
      (should-not (gnosis-test-demo--rows))
      (should (equal before (gnosis-test-demo--evidence))))))

(ert-deftest gnosis-test-demo-review-cancel-resume-and-reimport ()
  "Cancel input without a grade, resume all five kinds and reimport as a no-op."
  (dolist (mode '(due practice))
    (gnosis-test-demo--with-fixture
      (gnosis-test-demo--import file)
      (let ((before (gnosis-test-demo--evidence))
            (inputs '("repetition is the mother of memory" "1984" "Richard" "Stallman"))
            (choices '("Intravenous (IV)" "hyponatremia"))
            (actions 0))
        (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                   (lambda (&rest _) (signal 'quit nil))))
          (should (eq 'cancelled
                      (condition-case nil
                          (gnosis-review-loop '(1 2 3 4 5) mode)
                        (quit 'cancelled)))))
        (should (equal before (gnosis-test-demo--evidence)))
        (should (equal '(1 2 3 4 5)
                       (gnosis-review-state-remaining (gnosis-review--read-session))))
        (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                   (lambda (&rest _) (or (pop inputs) (ert-fail "Unexpected text input"))))
                  ((symbol-function 'gnosis-completing-read)
                   (lambda (_prompt options &rest _)
                     (let ((choice (or (pop choices) (ert-fail "Unexpected choice"))))
                       (should (member choice options)) choice)))
                  ((symbol-function 'read-char-choice)
                   (lambda (&rest _)
                     (cl-incf actions)
                     (should (string-search
                              (if (eq mode 'practice) "Practice: schedule unchanged"
                                "Next review:")
                              (buffer-string)))
                     (should-not (string-search "\\" (buffer-string)))
                     ?n)))
          (call-interactively #'gnosis-review-resume))
        (should-not inputs)
        (should-not choices)
        (should (= actions 5))
        (should-not (gnosis-review-state-remaining (gnosis-review--read-session)))
        (should (equal '((1 3) (2 3) (3 3) (4 3) (5 3))
                       (sqlite-select gnosis-db
                         (concat "SELECT thema_id, rating FROM "
                                 (if (eq mode 'due) "review_events" "practice_events")
                                 " ORDER BY thema_id"))))
        (if (eq mode 'practice)
            (should (equal (seq-take before 2) (seq-take (gnosis-test-demo--evidence) 2)))
          (should (equal '((1 1) (2 1) (3 1) (4 1) (5 1))
                         (sqlite-select gnosis-db
                           "SELECT thema_id, reps FROM scheduler_state ORDER BY thema_id"))))
        (let ((accepted (gnosis-test-demo--evidence))
              (checkpoint (gnosis-review--state-data (gnosis-review--read-session))))
          (gnosis-import-db file)
          (should-not (get-buffer "*Gnosis Import*"))
          (should (equal accepted (gnosis-test-demo--evidence)))
          (should (equal checkpoint (gnosis-review--state-data
                                     (gnosis-review--read-session)))))))))

(ert-deftest gnosis-test-demo-wrong-answers ()
  "Every shipped kind rejects an incorrect response through native dispatch."
  (gnosis-test-demo--with-fixture
    (gnosis-test-demo--import file)
    (let ((before (gnosis-test-demo--evidence)))
      (with-current-buffer (get-buffer-create gnosis-review-buffer-name)
        (gnosis-mode)
        (dolist (case '((1 "basic" "wrong") (2 "cloze" "wrong")
                        (3 "cloze" "wrong") (4 "mcq" "Oral")
                        (5 "mc-cloze" "hypernatremia")))
          (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                     (lambda (&rest _) (nth 2 case)))
                    ((symbol-function 'gnosis-completing-read)
                     (lambda (&rest _) (nth 2 case))))
            (pcase-let ((`(,kind ,answer) (gnosis-review--display-thema (car case))))
              (should (equal kind (nth 1 case)))
              (should-not (car answer))))))
      (should (equal before (gnosis-test-demo--evidence))))))

(provide 'gnosis-test-demo)
;;; gnosis-test-demo.el ends here
