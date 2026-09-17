;;; gnosis-test-authoring-quality-text.el --- Lossless draft saves -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Canonical Answer preservation and empty-cloze draft recovery.

;;; Code:

(require 'ert)
(require 'gnosis-test-authoring-quality)

(ert-deftest gnosis-test-authoring-quality-checkbox-answer-reopen ()
  "No-op edits preserve canonical checkbox-looking text through reopen."
  (dolist (answer '("[x] literal α" "[X] Literal" "[ ] pending" "[-] partial"
                    "Ελληνικά\nδεύτερη γραμμή" "- literal hyphen"))
    (gnosis-test-with-db
      (gnosis-test-authoring-quality--seed)
      (gnosis-add-thema-fields "basic" "Question [[id:source][Source]]" '("")
                               (list answer) "Explanation" '("tag") 0 '("source")
                               nil 222 '("Alias"))
      (let ((before (gnosis-test-draft--rows)))
        (gnosis-test-draft--with-editor
          (gnosis-edit-thema 222)
          (gnosis-test-authoring-quality--key (current-buffer) "C-c C-c")
          (gnosis-sqlite-close gnosis-db)
          (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (should (equal (list answer) (gnosis-get 'answer 'themata '(= id 222))))
          (should (equal before (gnosis-test-draft--rows)))
          (gnosis-edit-thema 222)
          (should (equal (list answer) (nth 4 (car (gnosis-export-parse-themata)))))
          (gnosis-test-authoring-quality--key (current-buffer) "C-c C-k"))))))

(ert-deftest gnosis-test-authoring-quality-empty-cloze-retains-draft ()
  "Zero-group creation refuses with complete draft and prior rows intact."
  (dolist (question '("No blank here" "Incomplete {{c1::answer" "{c2:unfinished"))
    (dolist (mixed '(nil t))
      (gnosis-test-with-db
        (gnosis-test-authoring-quality--seed)
        (let ((before (gnosis-test-draft--rows)))
          (gnosis-test-draft--with-editor
            (gnosis-add-thema "cloze" question nil nil "Keep this explanation")
            ;; Parser returns reverse document order: this valid thema writes
            ;; first, so refusal must roll it back along with dependent rows.
            (when mixed
              (gnosis-export--insert-thema "NEW" "basic" "Valid sibling" nil "Answer"))
            (let ((draft (current-buffer)) (text (buffer-string))
                  (position (point)) (owner gnosis--draft-db))
              (should-error (gnosis-test-authoring-quality--key draft "C-c C-c")
                            :type 'user-error)
              (should (eq draft (current-buffer)))
              (should (= position (point)))
              (should (equal-including-properties text (buffer-string)))
              (should (eq owner gnosis--draft-db))
              (should-not gnosis--draft-saved-p)
              (should (equal before (gnosis-test-draft--rows)))
              (should-not (gnosis-test-authoring-quality--rows)))))))))

(ert-deftest gnosis-test-authoring-quality-cloze-supported-creation ()
  "Explicit answers and one or multiple inline groups still persist."
  (dolist (case '(("Explicit alpha" "alpha" 1)
                  ("{{c1::άλφα}}" nil 1)
                  ("{{c1::άλφα}} and {{c2::βήτα}}" nil 2)))
    (gnosis-test-with-db
      (gnosis-test-draft--with-editor
        (gnosis-add-thema "cloze" (nth 0 case) nil (nth 1 case) "Explanation")
        (gnosis-test-authoring-quality--key (current-buffer) "C-c C-c")
        (gnosis-sqlite-close gnosis-db)
        (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
        (should (= (nth 2 case) (length (gnosis-select 'id 'themata))))
        (dolist (id (gnosis-select 'id 'themata nil t))
          (gnosis-edit-thema id)
          (should (nth 4 (car (gnosis-export-parse-themata))))
          (gnosis-test-authoring-quality--key (current-buffer) "C-c C-k"))))))

(provide 'gnosis-test-authoring-quality-text)
;;; gnosis-test-authoring-quality-text.el ends here
