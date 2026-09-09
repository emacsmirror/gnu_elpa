;;; gnosis-test-from-node.el --- Node-to-thema workflow tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Exercise creation and saving through the ordinary editor with isolated data.

;;; Code:

(require 'ert)
(require 'gnosis)
(require 'gnosis-export-import)
(require 'gnosis-test-helpers)

(defmacro gnosis-test-from-node--with-source (text &rest body)
  "Show an Org buffer containing TEXT and run BODY at its end."
  (declare (indent 1) (debug t))
  `(save-window-excursion
     (let ((register-alist nil)
           (gnosis-save-hook nil)
           (gnosis-review-editing-p nil))
       (with-temp-buffer
         (org-mode)
         (insert ,text)
         (switch-to-buffer (current-buffer))
         (unwind-protect
             (progn ,@body)
           (when-let* ((draft (get-buffer "*Gnosis NEW*")))
             (kill-buffer draft)))))))

(ert-deftest gnosis-test-from-node-save-journey ()
  "Compose beside the source and save its exact backlink through the editor."
  (gnosis-test-with-db
    (gnosis-test-from-node--with-source
        ":PROPERTIES:\n:ID: root\n:END:\n* Source\n:PROPERTIES:\n:ID: child\n:END:\nPassage"
      (let ((source (current-buffer)) (position (point))
            (text (buffer-string)))
        (gnosis-add-thema-from-node)
        (should (derived-mode-p 'gnosis-edit-mode))
        (should (get-buffer-window source))
        (should (equal (nth 2 (car (gnosis-export-parse-themata))) nil))
        (should-not (nth 4 (car (gnosis-export-parse-themata))))
        (insert "What is recalled?")
        (search-forward "** Answer")
        (forward-line)
        (insert "The answer")
        (gnosis-save)
        (should (eq (current-buffer) source))
        (should (= (point) position))
        (should (equal (buffer-string) text))
        (should-not (get-buffer "*Gnosis NEW*"))
        (should (equal (gnosis-select '[type keimenon answer] 'themata)
                       '(("basic" "What is recalled?" ("The answer")))))
        (should (equal (gnosis-select 'parathema 'extras nil t)
                       '("[[id:child][Source]]")))
        (should (equal (gnosis-select 'dest 'thema-links nil t)
                       '("child")))))))

(ert-deftest gnosis-test-from-node-nearest-id ()
  "Resolve headings, non-ID descendants, root bodies and narrowed subtrees."
  (dolist (case '((":PROPERTIES:\n:ID: root\n:END:\nBody" "root" nil)
                  (":PROPERTIES:\n:ID: root\n:END:\n* No ID\nBody" "root" nil)
                  ("* Parent\n:PROPERTIES:\n:ID: parent\n:END:\n** Child\nBody" "parent" nil)
                  ("* Parent\n:PROPERTIES:\n:ID: parent\n:END:\n** Child\nBody" "parent" t)
                  ("* Parent\n:PROPERTIES:\n:ID: parent\n:END:\n** Child\n:PROPERTIES:\n:ID: child\n:END:\nBody" "child" nil)))
    (gnosis-test-from-node--with-source (car case)
      (when (nth 2 case) (org-narrow-to-subtree))
      (gnosis-add-thema-from-node)
      (let ((thema (car (gnosis-export-parse-themata))))
        (should-not (nth 2 thema))
        (should (equal (gnosis-extract-id-links (nth 5 thema))
                       (list (nth 1 case))))))))

(ert-deftest gnosis-test-from-node-region-answer ()
  "Prefill a selected multiline Unicode answer, never the question."
  (gnosis-test-with-db
    (gnosis-test-from-node--with-source ":PROPERTIES:\n:ID: root\n:END:\n"
      (let ((transient-mark-mode t)
            (answer "Απάντηση — café\nΔεύτερη γραμμή"))
        (push-mark (point) t t)
        (insert (propertize answer 'face 'bold))
        (activate-mark)
        (gnosis-add-thema-from-node)
        (let ((thema (car (gnosis-export-parse-themata))))
          (should-not (nth 2 thema))
          (should (equal (nth 4 thema) (list answer))))
        (insert "Question")
        (gnosis-save)
        (should (equal (gnosis-select 'answer 'themata nil t)
                       (list (list answer))))))))

(ert-deftest gnosis-test-from-node-invalid-context-no-effects ()
  "Reject missing IDs and non-Org buffers before opening or changing anything."
  (dolist (org-p '(nil t))
    (gnosis-test-with-db
      (gnosis-test-from-node--with-source "* No ID\nBody"
        (unless org-p (fundamental-mode))
        (let ((text (buffer-string)) (position (point))
              (windows (current-window-configuration)))
          (should-error (gnosis-add-thema-from-node) :type 'user-error)
          (should (equal text (buffer-string)))
          (should (= position (point)))
          (should-not register-alist)
          (should-not (get-buffer "*Gnosis NEW*"))
          (should (compare-window-configurations
                   windows (current-window-configuration)))
          (should-not (gnosis-select 'id 'themata)))))))

(ert-deftest gnosis-test-from-node-preserve-draft ()
  "Neither creation entry point may erase an existing unsaved draft."
  (gnosis-test-from-node--with-source ":PROPERTIES:\n:ID: root\n:END:\nBody"
    (gnosis-add-thema-from-node)
    (insert "Keep this draft")
    (let ((text (buffer-string))
          (saved-register (get-register :gnosis-edit)))
      (should-error (gnosis-add-thema "basic") :type 'user-error)
      (should (equal text (buffer-string)))
      (should (eq saved-register (get-register :gnosis-edit)))
      (select-window (next-window))
      (should-error (gnosis-add-thema-from-node) :type 'user-error)
      (should (equal text (with-current-buffer "*Gnosis NEW*" (buffer-string))))
      (should (eq saved-register (get-register :gnosis-edit))))))

(ert-deftest gnosis-test-from-node-cancel ()
  "Cancel returns to the unmodified source and does not create a thema."
  (gnosis-test-with-db
    (gnosis-test-from-node--with-source ":PROPERTIES:\n:ID: root\n:END:\nBody"
      (set-buffer-modified-p nil)
      (let ((source (current-buffer)) (position (point)))
        (gnosis-add-thema-from-node)
        (insert "Abandoned question")
        (gnosis-edit-quit)
        (should (eq source (current-buffer)))
        (should (= position (point)))
        (should-not (buffer-modified-p))
        (should-not (get-buffer "*Gnosis NEW*"))
        (should-not (gnosis-select 'id 'themata))))))

(ert-deftest gnosis-test-from-node-no-sibling-id ()
  "Do not mistake an earlier sibling's ID for a file-level source ID."
  (gnosis-test-from-node--with-source
      "* Sibling\n:PROPERTIES:\n:ID: sibling\n:END:\n* No ID\nBody"
    (should-error (gnosis-add-thema-from-node) :type 'user-error)
    (should-not (get-buffer "*Gnosis NEW*"))
    (should-not register-alist)))

(defconst gnosis-test-from-node--lossy-passages
  '("Alpha\n- Beta" "Alpha\n- \nBeta" "- Alpha"
    "Alpha\n** Nested\nBeta" "Alpha\n** Parathema\nInjected"
    "Alpha\n* Thema\n:PROPERTIES:\n:GNOSIS_ID: NEW\n:GNOSIS_TYPE: basic\n:END:\n** Answer\nInjected"
    "   \n  ")
  "Selections that cannot survive as one unchanged basic answer.")

(ert-deftest gnosis-test-from-node-lossy-codec ()
  "Exercise the real codec for separator, dash, and heading corruption."
  (dolist (answer gnosis-test-from-node--lossy-passages)
    (with-temp-buffer
      (org-mode)
      (gnosis-export--insert-thema
       "NEW" "basic" nil nil answer "[[id:root][Source]]")
      (let ((parsed (condition-case nil (gnosis-export-parse-themata)
                      (user-error nil))))
        (should-not
         (and (= (length parsed) 1)
              (equal (butlast (car parsed))
                     (list "NEW" "basic" nil nil (list (string-trim answer))
                           "[[id:root][Source]]" nil))))))))

(ert-deftest gnosis-test-from-node-lossy-selection-no-effects ()
  "Reject lossy passages without changing source, draft, windows or data."
  (dolist (answer gnosis-test-from-node--lossy-passages)
    (dolist (existing-draft '(nil t))
      (gnosis-test-with-db
        (gnosis-test-from-node--with-source ":PROPERTIES:\n:ID: root\n:END:\n"
          (let ((source (current-buffer))
                (transient-mark-mode t))
            (when existing-draft
              (gnosis-add-thema-from-node)
              (insert "Keep this draft")
              (select-window (get-buffer-window source)))
            (let ((begin (point)))
              (insert answer)
              (push-mark (point) t t)
              (goto-char begin)
              (activate-mark))
            (let ((text (buffer-string))
                  (position (point))
                  (mark-position (mark))
                  (modified (buffer-modified-p))
                  (windows (current-window-configuration))
                  (registers (copy-tree register-alist))
                  (draft (get-buffer "*Gnosis NEW*"))
                  (draft-text (when existing-draft
                                (with-current-buffer "*Gnosis NEW*"
                                  (buffer-string)))))
              (should (string-match-p
                       "select a plain passage"
                       (error-message-string
                        (should-error (gnosis-add-thema-from-node)
                                      :type 'user-error))))
              (should (eq source (current-buffer)))
              (should (equal text (buffer-string)))
              (should (= position (point)))
              (should (= mark-position (mark)))
              (should mark-active)
              (should (eq modified (buffer-modified-p)))
              (should (equal registers register-alist))
              (should (compare-window-configurations
                       windows (current-window-configuration)))
              (should (eq draft (get-buffer "*Gnosis NEW*")))
              (when draft
                (should (equal draft-text
                               (with-current-buffer draft (buffer-string)))))
              (should-not (gnosis-select 'id 'themata))
              (should-not (gnosis-select 'id 'extras))
              (should-not (gnosis-select 'dest 'thema-links)))))))))

(ert-deftest gnosis-test-from-node-plain-paragraph-save ()
  "Save Unicode paragraphs with only outer whitespace trimmed."
  (gnosis-test-with-db
    (gnosis-test-from-node--with-source ":PROPERTIES:\n:ID: root\n:END:\n"
      (let ((transient-mark-mode t)
            (answer "Απάντηση — café\nΔεύτερη γραμμή\n\nAnother paragraph."))
        (push-mark (point) t t)
        (insert "\n" answer "\n\n")
        (activate-mark)
        (gnosis-add-thema-from-node)
        (should (equal (nth 4 (car (gnosis-export-parse-themata)))
                       (list answer)))
        (insert "Question")
        (gnosis-save)
        (should (equal (gnosis-select 'answer 'themata nil t)
                       (list (list answer))))
        (should (equal (gnosis-select 'parathema 'extras nil t)
                       '("[[id:root][Source]]")))))))

(ert-deftest gnosis-test-from-node-leading-hyphen-save-edit ()
  "Keep negative answers and literal hyphens through creation and editing."
  (dolist (answer '("-90" "-Alpha" "--flag"))
    (gnosis-test-with-db
      (gnosis-test-from-node--with-source ":PROPERTIES:\n:ID: root\n:END:\n"
        (let ((transient-mark-mode t)
              (gnosis-save-hook nil))
          (push-mark (point) t t)
          (insert answer)
          (activate-mark)
          (gnosis-add-thema-from-node)
          (insert "Question")
          (call-interactively (key-binding (kbd "C-c C-c")))
          (let ((id (car (gnosis-select 'id 'themata nil t))))
            (should (equal (list answer) (gnosis-get 'answer 'themata `(= id ,id))))
            (unwind-protect
                (progn
                  (gnosis-edit-thema id)
                  (call-interactively (key-binding (kbd "C-c C-c")))
                  (should (equal (list answer) (gnosis-get 'answer 'themata `(= id ,id)))))
              (when (get-buffer "*Gnosis Edit*") (kill-buffer "*Gnosis Edit*")))))))))

(provide 'gnosis-test-from-node)
;;; gnosis-test-from-node.el ends here
