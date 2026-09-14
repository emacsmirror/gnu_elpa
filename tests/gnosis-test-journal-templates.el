;;; gnosis-test-journal-templates.el --- Journal writing tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Exercise optional templates and free writing with disposable journals.

;;; Code:

(require 'ert)
(require 'gnosis-journal)
(require 'gnosis-test-helpers)

(defvar gnosis-journal-new-entry-template)

(defmacro gnosis-test-journal-templates--with-layouts (&rest body)
  "Run BODY with fresh single-file and separate-file journals."
  (declare (indent 0) (debug t))
  `(dolist (layout '(t nil))
     (gnosis-test-with-db
       (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
              (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
              (gnosis-journal-file layout)
              (gnosis-journal-as-gpg nil)
              (gnosis-journal-new-entry-template nil)
              (gnosis-nodes-completing-read-func (lambda (&rest _) "Default"))
              (gnosis-journal-todo-files nil)
              (gnosis-nodes-timestring nil)
              (org-id-track-globally nil)
              (create-lockfiles nil))
         (make-directory gnosis-journal-dir t)
         (unwind-protect
             (save-window-excursion ,@body)
           (dolist (buffer (buffer-list))
             (when-let* ((file (buffer-file-name buffer)))
               (when (file-in-directory-p file gnosis-dir)
                 (with-current-buffer buffer (set-buffer-modified-p nil))
                 (kill-buffer buffer)))))))))

(ert-deftest gnosis-test-journal-templates-free-form-default ()
  "A new date has no generated body or template prompt, even on reopen."
  (gnosis-test-journal-templates--with-layouts
    (let ((gnosis-nodes-completing-read-func
           (lambda (&rest _) (ert-fail "Unexpected template prompt"))))
      (gnosis-journal-date "2001-02-03")
      (should-not (string-match-p "Daily Notes\\|Goals\\|Study" (buffer-string)))
      (gnosis-journal-capture "Free writing Ελληνικά")
      (let ((text (buffer-string)) (file buffer-file-name))
        (save-buffer)
        (kill-buffer (current-buffer))
        (gnosis-journal-date "2001-02-03")
        (should (equal buffer-file-name file))
        (should (equal (buffer-string) text))))))

(ert-deftest gnosis-test-journal-templates-auto-once-and-legacy-function ()
  "Configured zero-argument functions run only when creating the date."
  (gnosis-test-journal-templates--with-layouts
    (let* ((calls 0)
           (gnosis-journal-templates
            (list (cons "Other" (lambda () "Wrong template"))
                  (cons "Mine" (lambda ()
                                 (cl-incf calls)
                                 "{*} My section\nKept\n{**} Detail\n"))))
           (gnosis-journal-new-entry-template "Mine")
           (gnosis-nodes-completing-read-func
            (lambda (&rest _) (ert-fail "Unexpected prompt"))))
      (gnosis-journal-date "2001-02-03")
      (should (= calls 1))
      (should (string-match-p (if layout "^\\*\\* My section" "^\\* My section")
                              (buffer-string)))
      (let ((text (buffer-string)))
        (gnosis-journal-date "2001-02-03")
        (should (= calls 1))
        (should (equal text (buffer-string)))
        (save-buffer)
        (kill-buffer (current-buffer))
        (gnosis-journal-date "2001-02-03")
        (should (= calls 1))
        (should (equal text (buffer-string)))))))

(ert-deftest gnosis-test-journal-templates-date-correct-tasks ()
  "Default and Goals templates collect selected-date tasks, never today's."
  (gnosis-test-journal-templates--with-layouts
    (let* ((tasks (expand-file-name "tasks.org" gnosis-dir))
           (gnosis-journal-todo-files (list tasks)))
      (with-temp-file tasks
        (insert "* TODO Selected\nSCHEDULED: <2001-02-03 Sat>\n"
                "* TODO Undated\n"
                "* TODO Today only\nSCHEDULED: <"
                (format-time-string "%Y-%m-%d") ">\n"))
      (let ((gnosis-journal-new-entry-template "Default"))
        (gnosis-journal-date "2001-02-03"))
      (should (string-match-p "Selected" (buffer-string)))
      (should (string-match-p "Undated" (buffer-string)))
      (should-not (string-match-p "Today only" (buffer-string)))
      (gnosis-journal-insert-template "Goals")
      (should (= (how-many "Selected" (point-min) (point-max)) 2))
      (should-not (string-match-p "Today only" (buffer-string))))))

(ert-deftest gnosis-test-journal-templates-explicit-depth-and-save ()
  "Insertion from a nested section preserves prose, IDs and sibling dates."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (gnosis-journal-capture "Retain free prose")
    (let ((id (org-entry-get (if layout (point) (point-min)) "ID")))
      (gnosis-journal-insert-template "Goals")
      (goto-char (point-max))
      (insert (if layout "*** Nested\nretained\n" "** Nested\nretained\n"))
      (org-narrow-to-subtree)
      (let ((gnosis-nodes-completing-read-func (lambda (&rest _) "Study")))
        (call-interactively (keymap-lookup gnosis-journal-prefix-map "i")))
      (widen)
      (should (string-match-p (if layout "^\\*\\* Study$" "^\\* Study$")
                              (buffer-string)))
      (gnosis-journal-insert-template "Day reflection")
      (should (string-match-p "Retain free prose" (buffer-string)))
      (should (string-match-p "retained" (buffer-string)))
      (let ((text (buffer-string)))
        (save-buffer)
        (kill-buffer (current-buffer))
        (gnosis-journal-date "2001-02-03")
        (should (equal (org-id-get) id))
        (should (equal text (buffer-string)))))))

(ert-deftest gnosis-test-journal-templates-capture-free-writing-section ()
  "Capture uses Daily Notes when present, otherwise body before first child."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (gnosis-journal-insert-template "Goals")
    (gnosis-journal-capture "Before goals")
    (goto-char (point-min))
    (search-forward "Before goals")
    (should (if layout (equal (org-get-heading t t t t) "2001-02-03")
              (org-before-first-heading-p)))
    (gnosis-journal-insert-template "Default")
    (gnosis-journal-capture "Inside daily notes")
    (goto-char (point-min))
    (search-forward "Inside daily notes")
    (should (equal (org-get-heading t t t t) "Daily Notes"))
    (should (string-match-p "Before goals" (buffer-string)))))

(ert-deftest gnosis-test-journal-templates-invalid-and-quit-preserve-draft ()
  "Invalid names/results and quit do not change an existing draft."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (gnosis-journal-capture "Keep this draft")
    (let ((text (buffer-string)) (pos (point)))
      (should-error (gnosis-journal-insert-template "Missing") :type 'user-error)
      (let ((gnosis-journal-templates '(("Bad" . ignore))))
        (should-error (gnosis-journal-insert-template "Bad") :type 'user-error))
      (let ((gnosis-nodes-completing-read-func
             (lambda (&rest _) (signal 'quit nil))))
        (should (eq (condition-case nil
                        (progn (call-interactively #'gnosis-journal-insert-template) nil)
                      (quit 'quit))
                    'quit)))
      (should (equal text (buffer-string)))
      (should (= pos (point)))
      (should (buffer-modified-p)))))

(ert-deftest gnosis-test-journal-templates-auto-failure-preserves-draft ()
  "Invalid automatic templates neither add dates nor replace unsaved text."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (gnosis-journal-capture "Keep")
    (let ((buffer (current-buffer)) (text (buffer-string)))
      (dolist (name '("Missing" "Bad" "Quit"))
        (let ((gnosis-journal-new-entry-template name)
              (gnosis-journal-templates
               (list (cons "Bad" (lambda () 7))
                     (cons "Quit" (lambda () (signal 'quit nil))))))
          (should (condition-case nil
                      (progn (gnosis-journal-date "2001-02-04") nil)
                    ((user-error quit) t)))))
      (with-current-buffer buffer (should (equal text (buffer-string))))
      (should-not (gnosis-journal--unique-entry "2001-02-04")))))

(ert-deftest gnosis-test-journal-templates-prompt-owns-entry ()
  "Moving point during a prompt cannot retarget insertion or capture."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (gnosis-journal-capture "First day")
    (let ((first (current-buffer)))
      (gnosis-journal-date "2001-02-04")
      (gnosis-journal-capture "Second day")
      (gnosis-journal-date "2001-02-03")
      (let ((gnosis-nodes-completing-read-func
             (lambda (&rest _)
               (gnosis-journal-date "2001-02-04") "Study")))
        (call-interactively #'gnosis-journal-insert-template))
      (with-current-buffer first
        (goto-char (point-min))
        (search-forward "Study")
        (should (equal (gnosis-journal--date-at-point) "2001-02-03")))
      (gnosis-journal-date "2001-02-03")
      (cl-letf (((symbol-function 'read-string-from-buffer)
                 (lambda (&rest _)
                   (gnosis-journal-date "2001-02-04") "Owned note")))
        (call-interactively #'gnosis-journal-capture))
      (goto-char (point-min))
      (search-forward "Owned note")
      (should (equal (gnosis-journal--date-at-point) "2001-02-03")))))

(ert-deftest gnosis-test-journal-templates-stale-buffer-and-destination ()
  "Changed destination, mode, file or contents reject delayed input."
  (dolist (change '(destination mode file text))
    (gnosis-test-journal-templates--with-layouts
      (gnosis-journal-date "2001-02-03")
      (gnosis-journal-capture "Keep")
      (let* ((buffer (current-buffer))
             (file buffer-file-name)
             (gnosis-nodes-completing-read-func
              (lambda (&rest _)
                (pcase change
                  ('destination (setq gnosis-journal-file "elsewhere.org"))
                  ('mode (fundamental-mode))
                  ('file (set-visited-file-name (concat file ".other") t))
                  ('text (insert "Successor draft")))
                "Study")))
        (should-error (call-interactively #'gnosis-journal-insert-template)
                      :type 'user-error)
        (with-current-buffer buffer
          (should-not (string-match-p "Study" (buffer-string)))
          (should (string-match-p "Keep" (buffer-string))))))))

(ert-deftest gnosis-test-journal-templates-capture-retained-sections ()
  "Free writing in a retained entry must not become a Goals item."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (goto-char (point-max))
    (insert (if layout "\n** Daily Notes\nOld prose\n** Goals\nKeep goals\n"
              "\n* Daily Notes\nOld prose\n* Goals\nKeep goals\n"))
    (gnosis-journal-capture "New prose")
    (goto-char (point-min))
    (search-forward "New prose")
    (should (equal (org-get-heading t t t t) "Daily Notes"))))

(ert-deftest gnosis-test-journal-templates-capture-buffer-input ()
  "Native buffer input preserves multiline prose without saving or reprompting."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (let ((note "  First thought\nSecond thought Ελληνικά  \n")
          (calls 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (ert-fail "Unexpected minibuffer input")))
                ((symbol-function 'read-string-from-buffer)
                 (lambda (prompt initial)
                   (should (equal prompt "Thought "))
                   (should (equal initial ""))
                   (cl-incf calls)
                   note)))
        (call-interactively (keymap-lookup gnosis-journal-prefix-map "c"))
        (should (= calls 1))
        (should (string-suffix-p (concat note "\n") (buffer-string)))
        (gnosis-journal-capture note)
        (should (= calls 1))
        (should (string-suffix-p (concat note "\n") (buffer-string)))
        (should (buffer-modified-p))
        (should-not (file-exists-p buffer-file-name))))))

(ert-deftest gnosis-test-journal-templates-capture-prompt-date ()
  "The date is captured before reading a quick note."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (gnosis-journal-date "2001-02-04")
    (gnosis-journal-date "2001-02-03")
    (cl-letf (((symbol-function 'read-string-from-buffer)
               (lambda (&rest _)
                 (gnosis-journal-date "2001-02-04") "Owned capture")))
      (call-interactively #'gnosis-journal-capture))
    (goto-char (point-min))
    (search-forward "Owned capture")
    (should (equal (gnosis-journal--date-at-point) "2001-02-03"))))

(ert-deftest gnosis-test-journal-templates-capture-cancel-and-stale ()
  "Canceled or stale quick capture leaves new and existing dates untouched."
  (gnosis-test-journal-templates--with-layouts
    (cl-letf (((symbol-function 'read-string-from-buffer)
               (lambda (&rest _) (signal 'quit nil))))
      (should (condition-case nil
                  (call-interactively #'gnosis-journal-capture)
                (quit t))))
    (should-not (gnosis-journal--entries))
    (gnosis-journal-date "2001-02-03")
    (gnosis-journal-capture "Existing draft")
    (let ((text (buffer-string)) (buffer (current-buffer)))
      (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (&rest _) "   ")))
        (should-error (call-interactively #'gnosis-journal-capture)
                      :type 'user-error))
      (cl-letf (((symbol-function 'read-string-from-buffer)
                 (lambda (&rest _) (signal 'quit nil))))
        (should (condition-case nil
                    (call-interactively #'gnosis-journal-capture)
                  (quit t))))
      (should (equal text (buffer-string)))
      (cl-letf (((symbol-function 'read-string-from-buffer)
                 (lambda (&rest _)
                   (setq gnosis-journal-file "other.org") "Rejected")))
        (should-error (call-interactively #'gnosis-journal-capture)
                      :type 'user-error))
      (with-current-buffer buffer (should (equal text (buffer-string)))))))

(ert-deftest gnosis-test-journal-templates-auto-callback-retarget ()
  "Automatic template callbacks cannot redirect new-date creation."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (gnosis-journal-capture "Existing draft")
    (let* ((text (buffer-string))
           (buffer (current-buffer))
           (gnosis-journal-new-entry-template "Move")
           (gnosis-journal-templates
            (list (cons "Move" (lambda ()
                                 (setq gnosis-journal-file "elsewhere.org")
                                 "{*} Rejected\n")))))
      (should-error (gnosis-journal-date "2001-02-04") :type 'user-error)
      (with-current-buffer buffer (should (equal text (buffer-string)))))))

(ert-deftest gnosis-test-journal-templates-sibling-date-boundaries ()
  "Insertion and capture preserve the following date and its ID and prose."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (gnosis-journal-date "2001-02-04")
    (gnosis-journal-capture "Following date")
    (let ((id (org-entry-get (if layout (point) (point-min)) "ID")))
      (gnosis-journal-date "2001-02-03")
      (let ((gnosis-journal-templates
             '(("No newline" . (lambda () "{*} Section\n{**} Nested\nText")))))
        (gnosis-journal-insert-template "No newline"))
      (gnosis-journal-capture "First date")
      (gnosis-journal-date "2001-02-04")
      (should (equal (org-id-get) id))
      (should (string-match-p "Following date" (buffer-string)))
      (gnosis-journal-date "2001-02-03")
      (should (string-match-p "First date" (buffer-string))))))

(provide 'gnosis-test-journal-templates)
;;; gnosis-test-journal-templates.el ends here
