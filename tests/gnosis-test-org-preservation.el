;;; gnosis-test-org-preservation.el --- Native Org preservation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Exercise native draft saves and file tagging on disposable data.

;;; Code:

(require 'ert)
(require 'gnosis)
(require 'gnosis-export-import)
(require 'gnosis-nodes)
(require 'gnosis-test-helpers)

(defun gnosis-test-org-preservation--snapshot ()
  "Return content and nonempty study evidence for the fixture database."
  (mapcar (lambda (table)
            (sqlite-select gnosis-db (format "SELECT * FROM %s" table)))
          '(themata extras thema_tag thema_links scheduler_baseline
            scheduler_state review_events practice_events)))

(defun gnosis-test-org-preservation--edit (text refuse &optional heading)
  "Save stored TEXT, expecting REFUSE for unsupported structure.
When HEADING is non-nil, narrow to its subtree before saving."
  (gnosis-test-with-db
    (gnosis-add-thema-fields "basic" "Question" '("") '("Answer")
                             text '("test") 0 '("target") nil 12345 '("Alias"))
    (gnosis-scheduler-accept-review (make-string 64 ?a) 12345 'success
                                    1000000 (gnosis--today-int))
    (sqlite-execute gnosis-db
                    "INSERT INTO practice_events VALUES ('practice', 12345, 'session', 1, 1000000, 3)")
    (let ((before (gnosis-test-org-preservation--snapshot)))
      (unwind-protect
          (save-window-excursion
            (gnosis-edit-thema 12345)
            (let ((draft (current-buffer)) (rendered (buffer-string)))
              (when heading
                (goto-char (point-min))
                (search-forward heading)
                (beginning-of-line)
                (org-narrow-to-subtree))
              (should (eq (key-binding (kbd "C-c C-c")) 'gnosis-save))
              (if refuse
                  (progn
                    (should-error (call-interactively (key-binding (kbd "C-c C-c")))
                                  :type 'user-error)
                    (should (eq (current-buffer) draft))
                    (when heading (should (buffer-narrowed-p)))
                    (should (equal (save-restriction (widen) (buffer-string))
                                   rendered))
                    (should (equal before (gnosis-test-org-preservation--snapshot)))
                    (should (buffer-modified-p))
                    (should (eq (key-binding (kbd "C-c C-k")) 'gnosis-edit-quit))
                    (call-interactively (key-binding (kbd "C-c C-k"))))
                (call-interactively (key-binding (kbd "C-c C-c"))))
              (should-not (buffer-live-p draft)))
            (should (equal before (gnosis-test-org-preservation--snapshot))))
        (when-let* ((buffer (get-buffer "*Gnosis Edit*")))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest gnosis-test-org-preservation-heading-refuses-save ()
  "Refuse an unowned peer heading rather than truncate persisted explanation."
  (gnosis-test-org-preservation--edit
   "Intro\n* Important detail\nKeep this explanation [[id:target]]" t))

(ert-deftest gnosis-test-org-preservation-narrowed-heading-refuses-save ()
  "Hidden unowned headings must not bypass native save validation."
  (gnosis-test-org-preservation--edit
   "Intro\n* Important detail\nKeep this explanation [[id:target]]" t "* Thema"))

(ert-deftest gnosis-test-org-preservation-narrowed-field-save ()
  "Saving from a field subtree retains the entire valid authored draft."
  (gnosis-test-org-preservation--edit
   "Intro\n*** Detail\nΕξήγηση [[id:target]]\n-leading hyphen" nil "** Parathema"))

(ert-deftest gnosis-test-org-preservation-multiline-save ()
  "Ordinary multiline content, aliases, links and study evidence survive save."
  (gnosis-test-org-preservation--edit "Intro\nDetail [[id:target]]" nil))

(ert-deftest gnosis-test-org-preservation-unowned-content ()
  "Refuse authored content that would otherwise be silently skipped."
  (dolist (text '("* Notes\nAuthored [[id:target]]\n"
                  "** Keimenon\nOrphaned field\n"
                  "Authored preamble [[id:target]]\n"))
    (with-temp-buffer
      (org-mode)
      (insert text)
      (gnosis-export--insert-thema "NEW" "basic" "Question" nil "Answer")
      (should-error (gnosis-export-parse-themata) :type 'user-error)))
  (with-temp-buffer
    (org-mode)
    (gnosis-export--insert-thema "NEW" "basic" "Question" nil "Answer")
    (goto-char (point-min))
    (search-forward ":END:\n")
    (insert "Unassigned explanation\n")
    (should-error (gnosis-export-parse-themata) :type 'user-error)))

(ert-deftest gnosis-test-org-preservation-multiple-and-nested ()
  "Keep multiple themata and deeper headings inside their owning field."
  (with-temp-buffer
    (org-mode)
    (gnosis-export--insert-thema "NEW" "basic" "First" nil "A"
                                 "Intro\n*** Detail\nBody [[id:target]]")
    (gnosis-export--insert-thema "NEW" "basic" "Second" nil "B"
                                 "Other" nil nil '("Alias"))
    (let ((rows (reverse (gnosis-export-parse-themata))))
      (should (= (length rows) 2))
      (should (equal (nth 5 (car rows)) "Intro\n*** Detail\nBody [[id:target]]"))
      (should (equal (nth 2 (cadr rows)) "Second"))
      (should (equal (nth 8 (cadr rows)) '("Alias"))))))

(defun gnosis-test-org-preservation--tag-file (metadata &optional heading cancel block)
  "Tag METADATA, optionally targeting HEADING or testing CANCEL.
When BLOCK is non-nil, place it immediately after METADATA and preserve it."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-dir))
           (gnosis-journal-file nil)
           (org-id-track-globally nil)
           (org-id-locations nil)
           (file (expand-file-name "topic.org" gnosis-nodes-dir))
           (body (concat block "\nBody [[id:target]]\n* Child :old:\n:PROPERTIES:\n:ID: child\n:END:\nChild text\n"))
           (original (concat ":PROPERTIES:\n:ID: root\n:END:\n" metadata body)))
      (make-directory gnosis-nodes-dir t)
      (with-temp-file file (insert original))
      (unwind-protect
          (save-window-excursion
            (find-file file)
            (gnosis-nodes-mode 1)
            (gnosis-nodes-update-file)
            (goto-char (point-min))
            (search-forward (if heading "Child text" "Body"))
            (let ((before (sqlite-select gnosis-db "SELECT * FROM nodes")))
              (should (eq (key-binding (kbd "C-c C-q")) 'gnosis-nodes-insert-tags))
              (cl-letf (((symbol-function 'completing-read-multiple)
                         (lambda (&rest _)
                           (if cancel (signal 'quit nil) '("added")))))
                (if cancel
                    (should (eq 'quit (condition-case nil
                                         (call-interactively (key-binding (kbd "C-c C-q")))
                                       (quit 'quit))))
                  (call-interactively (key-binding (kbd "C-c C-q")))))
              (if cancel
                  (progn
                    (should (equal (buffer-string) original))
                    (should-not (buffer-modified-p))
                    (should (equal before (sqlite-select gnosis-db "SELECT * FROM nodes"))))
                (save-buffer)
                (kill-buffer)
                (find-file file)
                (goto-char (point-min))
                (should (equal (org-id-get) "root"))
                (should (equal (nth 2 (gnosis-org-get-data--topic)) "root"))
                (when block
                  (should (equal (cdr (assoc "FILETAGS"
                                            (org-collect-keywords '("FILETAGS"))))
                                 '(":added:"))))
                (should (string-suffix-p (if heading "Child text\n" body) (buffer-string)))
                (should (equal (gnosis-select 'dest 'node-links '(= source "root") t)
                               '("target")))
                (should (equal (sort (gnosis-select 'tag 'node-tag
                                                   `(= node-id ,(if heading "child" "root")) t)
                                     #'string<)
                               (if heading '("added" "old")
                                 (if (string-match-p ":existing:" metadata)
                                     '("added" "existing") '("added")))))
                (gnosis-nodes-update-file)
                (when block
                  (should (equal (gnosis-select 'tag 'node-tag '(= node-id "root") t)
                                 '("added")))
                  (should (equal (gnosis-select 'dest 'node-links '(= source "root") t)
                                 '("target"))))
                (should (= (length (gnosis-select 'id 'nodes)) 2)))))
        (when-let* ((buffer (get-file-buffer file)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest gnosis-test-org-preservation-first-filetag ()
  "First file tags preserve native root properties across save and reopen."
  (gnosis-test-org-preservation--tag-file "#+title: Topic\n"))

(ert-deftest gnosis-test-org-preservation-first-filetag-source-block ()
  "Native file tagging preserves an adjacent source block and its index."
  (gnosis-test-org-preservation--tag-file
   "#+title: Topic\n" nil nil "#+begin_src text\ncontent\n#+end_src\n"))

(ert-deftest gnosis-test-org-preservation-first-filetag-example-block ()
  "Native file tagging preserves an adjacent example block and its index."
  (gnosis-test-org-preservation--tag-file
   "#+title: Topic\n" nil nil "#+begin_example\ncontent\n#+end_example\n"))

(ert-deftest gnosis-test-org-preservation-first-filetag-without-keywords ()
  "Filetag insertion without a leading keyword block retains the root ID."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: root\n:END:\n\nBody [[id:target]]\n")
    (gnosis-nodes-insert-filetag "added")
    (goto-char (point-min))
    (should (equal (org-id-get) "root"))
    (should (equal (gnosis-org-get-filetags) '("added")))
    (should (string-suffix-p "\nBody [[id:target]]\n" (buffer-string)))))

(ert-deftest gnosis-test-org-preservation-existing-filetags ()
  "Existing empty and nonempty FILETAGS retain their metadata owner."
  (dolist (metadata '("#+title: Topic\n#+filetags:\n"
                      "#+title: Topic\n#+filetags: :existing:\n"))
    (gnosis-test-org-preservation--tag-file metadata)))

(ert-deftest gnosis-test-org-preservation-heading-tags ()
  "The public tag binding still targets an ID-bearing heading."
  (gnosis-test-org-preservation--tag-file "#+title: Topic\n" t))

(ert-deftest gnosis-test-org-preservation-tag-cancel ()
  "Cancelled tag input changes neither source nor index."
  (gnosis-test-org-preservation--tag-file "#+title: Topic\n" nil t))

(ert-deftest gnosis-test-org-preservation-narrowed-filetags ()
  "Read existing root tags even when direct insertion starts narrowed."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: root\n:END:\n#+filetags: :existing:\n\nBody\n")
    (goto-char (point-max))
    (forward-line -1)
    (narrow-to-region (point) (point-max))
    (gnosis-nodes-insert-filetag "added")
    (widen)
    (goto-char (point-min))
    (should (equal (org-id-get) "root"))
    (should (equal (gnosis-org-get-filetags) '("existing" "added")))))

(provide 'gnosis-test-org-preservation)
;;; gnosis-test-org-preservation.el ends here
