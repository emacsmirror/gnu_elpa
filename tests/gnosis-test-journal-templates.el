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
      (let ((text (buffer-string)))
        (should-error (gnosis-journal-insert-template "Goals") :type 'user-error)
        (should (equal text (buffer-string))))
      (should (string-match-p "Selected"
                              (gnosis-journal--template-text "Goals" "2001-02-03")))
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
      (let ((text (buffer-string)))
        (should-error (gnosis-journal-insert-template "Day reflection") :type 'user-error)
        (should (equal text (buffer-string))))
      (should (string-match-p "Retain free prose" (buffer-string)))
      (should (string-match-p "retained" (buffer-string)))
      (let ((text (buffer-string)))
        (save-buffer)
        (kill-buffer (current-buffer))
        (gnosis-journal-date "2001-02-03")
        (should (equal (org-id-get) id))
        (should (equal text (buffer-string)))))))

(ert-deftest gnosis-test-journal-templates-capture-free-writing-section ()
  "Capture uses marked sections, otherwise body before the first child."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (gnosis-journal-insert-template "Goals")
    (gnosis-journal-capture "Before goals")
    (goto-char (point-min))
    (search-forward "Before goals")
    (should (if layout (equal (org-get-heading t t t t) "2001-02-03")
              (org-before-first-heading-p)))
    (should-error (gnosis-journal-insert-template "Default") :type 'user-error)
    (gnosis-journal-insert-template "Study")
    (gnosis-journal-capture "Inside daily notes")
    (goto-char (point-min))
    (search-forward "Inside daily notes")
    (should (equal (org-get-heading t t t t) "Study"))
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
    (should (if layout (equal (org-get-heading t t t t) "2001-02-03")
              (org-before-first-heading-p)))))

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

(ert-deftest gnosis-test-journal-routing-renamed-nested-thought ()
  "Thoughts use date-local properties, not literal heading titles."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (let ((gnosis-journal-templates
           '(("Mine" . (lambda ()
                         "{*} Envelope\n{***} Σκέψεις\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: thoughts\n:END:\nKept\n{****} Child\nChild prose\n{*} Daily Notes\nNot here\n")))))
      (gnosis-journal-insert-template "Mine"))
    (gnosis-journal-capture "Routed Ελληνικά\nSecond line")
    (goto-char (point-min))
    (search-forward "Routed Ελληνικά")
    (should (equal (org-get-heading t t t t) "Σκέψεις"))
    (should (equal (gnosis-journal--date-at-point) "2001-02-03"))
    (should-not (file-exists-p buffer-file-name))))

(ert-deftest gnosis-test-journal-routing-local-todo-command ()
  "The public local checkbox command is discoverable and never selects a task."
  (should (commandp 'gnosis-journal-add-todo))
  (should (eq (keymap-lookup gnosis-journal-prefix-map "a")
              'gnosis-journal-add-todo)))

(ert-deftest gnosis-test-journal-routing-local-todo-text ()
  "The a binding preserves multiline input and keeps local todos local."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (gnosis-journal-insert-template "Goals")
    (let ((gnosis-journal-bullet-point-char "-")
          (note "Greek Ελληνικά\n  continuation  "))
      (cl-letf (((symbol-function 'read-string-from-buffer)
                 (lambda (prompt initial)
                   (should (equal prompt "Todo "))
                   (should (equal initial "")) note))
                ((symbol-function 'gnosis-journal-get-todos)
                 (lambda (&rest _) (ert-fail "Local todo collected source tasks"))))
        (call-interactively (keymap-lookup gnosis-journal-prefix-map "a")))
      (should (string-match-p (regexp-quote (concat "- [ ] " note "\n"))
                              (buffer-string)))
      (goto-char (point-min))
      (search-forward "Greek")
      (should (equal (org-get-heading t t t t) "Goals"))
      (should-not (string-match-p "\\[\\[id:" (buffer-string)))
      (should-not (file-exists-p buffer-file-name)))))

(ert-deftest gnosis-test-journal-routing-invalid-declarations ()
  "Invalid tokens, duplicate keys and destinations refuse without edits."
  (dolist (body '("{*} A\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: \n:END:\n"
                  "{*} A\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: thoughts thoughts\n:END:\n"
                  "{*} A\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: thoughts nope\n:END:\n"
                  "{*} A\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: thoughts\n:GNOSIS_JOURNAL_ROLES: todos\n:END:\n"
                  "{*} A\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: thoughts\n:END:\n{*} B\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: thoughts\n:END:\n"
                  "{*} A\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: todos\n:END:\n{*} B\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: todos\n:END:\n"
                  "{*} A\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: thoughts\n"))
    (gnosis-test-journal-templates--with-layouts
      (gnosis-journal-date "2001-02-03")
      (goto-char (point-max))
      (insert (gnosis-org-expand-headings body (if layout 2 1)))
      (let ((text (buffer-string)))
        (dolist (command '(gnosis-journal-capture gnosis-journal-add-todo))
          (should-error (funcall command "Rejected") :type 'user-error)
          (should (equal text (buffer-string))))))))

(ert-deftest gnosis-test-journal-routing-native-opaque-and-prose ()
  "Native containers stay opaque and unmatched metaphor prose is allowed."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (let ((gnosis-journal-templates
           '(("Native" . (lambda ()
                           (concat
                            "{*} Inbox\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: thoughts todos\n:END:\n"
                            ":metaphor:\nOrdinary prose\n"
                            "#+begin_example\n,* Fake\n:GNOSIS_JOURNAL_ROLES: nonsense\n#+end_example\n"
                            "#+begin_src text\n,* Fake\n:GNOSIS_JOURNAL_ROLES: nonsense\n#+end_src\n"
                            "#+begin_verse\n:metaphor:\n:GNOSIS_JOURNAL_ROLES: nonsense\n#+end_verse\n"
                            ":LOGBOOK:\n:GNOSIS_JOURNAL_ROLES: nonsense\n:END:\n"
                            "{**} Child\nChild text\n"))))))
      (gnosis-journal-insert-template "Native"))
    (gnosis-journal-capture "Thought")
    (gnosis-journal-add-todo "Todo")
    (dolist (text '("- " "+ [ ] Todo"))
      (goto-char (point-min))
      (search-forward text)
      (should (equal (org-get-heading t t t t) "Inbox")))))

(ert-deftest gnosis-test-journal-routing-sibling-root-defense ()
  "The resolver refuses a restriction containing two date-root siblings."
  (with-temp-buffer
    (org-mode)
    (insert "* 2001-02-03\n* 2001-02-04\n** Inbox\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: thoughts\n:END:\n")
    (let ((text (buffer-string)) (tick (buffer-chars-modified-tick)) (pos (point)))
      (should-error (gnosis-journal--role-position 'thoughts t) :type 'user-error)
      (should (equal text (buffer-string)))
      (should (= tick (buffer-chars-modified-tick)))
      (should (= pos (point))))))

(ert-deftest gnosis-test-journal-routing-late-mutations-retain-input ()
  "Late owner and configuration changes refuse while preserving accepted text."
  (dolist (command '(gnosis-journal-capture gnosis-journal-add-todo))
    (dolist (change '(text mode file replacement config template))
      (gnosis-test-journal-templates--with-layouts
        (gnosis-journal-date "2001-02-03")
        (let ((owner (current-buffer)) (file buffer-file-name)
              (note "Retained Ελληνικά\nsecond line")
              (display (symbol-function 'display-buffer))
              recovery)
          (cl-letf (((symbol-function 'read-string-from-buffer)
                     (lambda (&rest _)
                       (pcase change
                         ('text (insert "Unrelated edit\n"))
                         ('mode (fundamental-mode))
                         ('file (set-visited-file-name (concat file ".new") t))
                         ('replacement
                          (set-buffer-modified-p nil)
                          (kill-buffer owner)
                          (find-file file)
                          (insert "Replacement draft\n"))
                         ('config (setq gnosis-journal-as-gpg t))
                         ('template (setq gnosis-journal-new-entry-template "Empty")))
                       note))
                    ((symbol-function 'display-buffer)
                     (lambda (buffer &rest args)
                       (when (and (bufferp buffer)
                                  (string-prefix-p "*Gnosis journal recovery*"
                                                   (buffer-name buffer)))
                         (setq recovery buffer))
                       (apply display buffer args))))
            (should-error (call-interactively command) :type 'user-error))
          (should (buffer-live-p recovery))
          (with-current-buffer recovery
            (should (equal note (buffer-string)))
            (set-buffer-modified-p nil))
          (kill-buffer recovery)
          (when (buffer-live-p owner)
            (with-current-buffer owner
              (should-not (string-match-p "Retained" (buffer-string)))
              (when (eq change 'text)
                (should (string-match-p "Unrelated edit" (buffer-string)))))))))))

(ert-deftest gnosis-test-journal-routing-new-date-cancel-empty-and-template ()
  "New-date failure creates no date; successful templates run once."
  (dolist (command '(gnosis-journal-capture gnosis-journal-add-todo))
    (gnosis-test-journal-templates--with-layouts
      (dolist (answer '("" "   " quit))
        (cl-letf (((symbol-function 'read-string-from-buffer)
                   (lambda (&rest _)
                     (if (eq answer 'quit) (signal 'quit nil) answer))))
          (should (condition-case nil
                      (progn (call-interactively command) nil)
                    ((user-error quit) t))))
        (should-not (gnosis-journal--entries)))
      (let* ((calls 0)
             (gnosis-journal-new-entry-template "Mine")
             (gnosis-journal-templates
              (list (cons "Mine" (lambda ()
                                   (cl-incf calls)
                                   "{*} Inbox\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: thoughts todos\n:END:\n")))))
        (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (&rest _) "Accepted")))
          (call-interactively command))
        (should (= calls 1))
        (goto-char (point-min))
        (search-forward "Accepted")
        (should (equal (org-get-heading t t t t) "Inbox"))
        (should (equal (gnosis-journal--date-at-point) (format-time-string "%Y-%m-%d")))
        (should-not (file-exists-p buffer-file-name))))))

(ert-deftest gnosis-test-journal-routing-new-template-refusal ()
  "Bad or late-mutating automatic templates do not create shell dates."
  (dolist (change '(invalid mutation config))
    (gnosis-test-journal-templates--with-layouts
      (gnosis-journal-date "2001-02-03")
      (let* ((owner (current-buffer))
             (gnosis-journal-new-entry-template "Mine")
             (gnosis-journal-templates
              (list (cons "Mine" (lambda ()
                                   (pcase change
                                     ('invalid "{*} X\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: wrong\n:END:\n")
                                     ('mutation (with-current-buffer owner (insert "Other edit")) "")
                                     ('config (setq gnosis-journal-new-entry-template "Empty") "")))))))
        (if (and (eq change 'mutation) (not layout))
            ;; An edit in another separate date does not retire this destination.
            (with-temp-buffer (gnosis-journal-capture "Accepted"))
          (with-temp-buffer
            (should-error (gnosis-journal-capture "Accepted") :type 'user-error))
          (should-not (gnosis-journal--unique-entry (format-time-string "%Y-%m-%d"))))))))

(ert-deftest gnosis-test-journal-routing-source-id-same-and-foreign ()
  "Actual confirmed ID creation routes to todos without saving either file."
  (dolist (same '(t nil))
    (gnosis-test-journal-templates--with-layouts
      (gnosis-journal-date "2001-02-03")
      (gnosis-journal-insert-template "Goals")
      (let* ((owner (current-buffer))
             (source (if same buffer-file-name (expand-file-name "tasks.org" gnosis-dir)))
             (gnosis-journal-todo-files (list source))
             (gnosis-nodes-completing-read-func (lambda (_prompt choices) (car choices))))
        (if same
            (progn
              (goto-char (point-max))
              (insert (if layout "*** TODO Source\nBody\n" "** TODO Source\nBody\n"))
              (save-buffer))
          (with-temp-file source (insert "* TODO Source\nBody\n")))
        (let ((before (with-temp-buffer (insert-file-contents source) (buffer-string))))
          (gnosis-journal-date "2001-02-03")
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (call-interactively (keymap-lookup gnosis-journal-prefix-map "t")))
          (should (eq (current-buffer) owner))
          (goto-char (point-min))
          (search-forward "+ [ ] [[id:")
          (should (equal (org-get-heading t t t t) "Goals"))
          (let ((id (gnosis-journal--task-id-at-point)))
            (should id)
            (with-current-buffer (get-file-buffer source)
              (widen)
              (should (gnosis-journal--goto-id id))
              (should (equal (org-get-todo-state) "TODO"))
              (should (buffer-modified-p))))
          (should (equal before (with-temp-buffer (insert-file-contents source) (buffer-string)))))))))

(ert-deftest gnosis-test-journal-routing-source-id-delta-not-unrelated-edits ()
  "The ID receipt cannot bless additional owner writes during native ID creation."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (gnosis-journal-insert-template "Goals")
    (goto-char (point-max))
    (insert (if layout "** TODO Source\n" "* TODO Source\n"))
    (save-buffer)
    (let ((gnosis-journal-todo-files (list buffer-file-name))
          (gnosis-nodes-completing-read-func (lambda (_prompt choices) (car choices)))
          (create (symbol-function 'org-id-get-create)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'org-id-get-create)
                 (lambda (&rest args)
                   (prog1 (apply create args)
                     (save-excursion (goto-char (point-max)) (insert "Unrelated edit\n"))))))
        (should-error (gnosis-journal-insert-task) :type 'user-error))
      (should (string-match-p "Unrelated edit" (buffer-string)))
      (should-not (string-match-p (regexp-quote "+ [ ] [[id:") (buffer-string))))))

(ert-deftest gnosis-test-journal-routing-unvisited-owner-and-last-prompt ()
  "An existing unvisited destination is owned before the final source-ID prompt."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal)
    (save-buffer)
    (let ((target buffer-file-name)
          (source (expand-file-name "tasks.org" gnosis-dir)))
      (kill-buffer (current-buffer))
      (with-temp-file source (insert "* TODO Source\n"))
      (let ((gnosis-journal-todo-files (list source))
            (gnosis-nodes-completing-read-func (lambda (_prompt choices) (car choices))))
        (with-temp-buffer
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _)
                       (should (get-file-buffer target))
                       (with-current-buffer (get-file-buffer target)
                         (goto-char (point-max)) (insert "Late edit\n"))
                       t)))
            (should-error (gnosis-journal-insert-task) :type 'user-error)))
        (with-current-buffer (get-file-buffer target)
          (should (string-match-p "Late edit" (buffer-string)))
          (should-not (string-match-p "\\[\\[id:" (buffer-string))))
        (should-not (get-file-buffer source))))))


(ert-deftest gnosis-test-journal-routing-mutated-directory-string ()
  "In-place configuration edits cannot redirect a new separate date."
  (gnosis-test-journal-templates--with-layouts
    (let ((original (copy-sequence gnosis-journal-dir)))
      (cl-letf (((symbol-function 'read-string-from-buffer)
                 (lambda (&rest _)
                   (aset gnosis-journal-dir (1- (length gnosis-journal-dir)) ?x)
                   "Accepted draft")))
        (should-error (gnosis-journal-capture) :type 'user-error))
      (should-not (gnosis-journal--entries))
      (let ((gnosis-journal-dir original))
        (should-not (gnosis-journal--entries))))))

(ert-deftest gnosis-test-journal-routing-source-date-root-id ()
  "Confirmed ID creation also works when the selected task is the date root."
  (gnosis-test-with-db
    (let* ((gnosis-journal-dir (expand-file-name "journal" gnosis-dir))
           (gnosis-journal-file t)
           (gnosis-journal-new-entry-template nil)
           (org-id-track-globally nil)
           (gnosis-nodes-completing-read-func (lambda (_prompt choices) (car choices)))
           (file (gnosis-journal--file))
           (gnosis-journal-todo-files (list file)))
      (make-directory gnosis-journal-dir t)
      (with-temp-file file (insert "* TODO 2001-02-03\nBody\n"))
      (unwind-protect
          (save-window-excursion
            (gnosis-journal-date "2001-02-03")
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (gnosis-journal-insert-task))
            (should (string-match-p (regexp-quote "+ [ ] [[id:") (buffer-string)))
            (goto-char (point-min))
            (should (org-id-get))
            (should (equal (org-get-todo-state) "TODO")))
        (when-let* ((buffer (get-file-buffer file)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest gnosis-test-journal-routing-fallback-and-noninheritance ()
  "Unmarked dates report fallback; inherited properties never duplicate roles."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (let ((org-use-property-inheritance t)
          messages)
      (goto-char (point-max))
      (insert (if layout "** Inbox\n" "* Inbox\n")
              ":PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: todos\n:END:\n"
              (if layout "*** Child\n" "** Child\n"))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (gnosis-journal-capture "Fallback thought"))
      (should (member "No thoughts journal role; using the date body" messages))
      (goto-char (point-min))
      (search-forward "Fallback thought")
      (should (if layout (equal (org-get-heading t t t t) "2001-02-03")
                (org-before-first-heading-p)))
      (gnosis-journal-add-todo "Noninherited todo")
      (goto-char (point-min))
      (search-forward "Noninherited todo")
      (should (equal (org-get-heading t t t t) "Inbox")))))

(ert-deftest gnosis-test-journal-routing-additive-refusal ()
  "Native additive properties must be consolidated, never silently ignored."
  (dolist (addition '("nonsense" "thoughts" "todos"))
    (gnosis-test-journal-templates--with-layouts
      (let* ((body (concat "{*} Inbox\n:PROPERTIES:\n"
                           ":GNOSIS_JOURNAL_ROLES: thoughts\n"
                           ":GNOSIS_JOURNAL_ROLES+: " addition "\n:END:\n"))
             (gnosis-journal-templates (list (cons "Additive" (lambda () body))))
             (gnosis-journal-new-entry-template "Additive"))
        (dolist (key '("c" "a" "t"))
          (let* ((source (expand-file-name "tasks.org" gnosis-dir))
                 (gnosis-journal-todo-files (list source))
                 (gnosis-nodes-completing-read-func (lambda (_prompt choices) (car choices))))
            (with-temp-file source (insert "* TODO Source\n"))
            (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (&rest _) "Rejected"))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (let ((err (should-error
                          (call-interactively (keymap-lookup gnosis-journal-prefix-map key))
                          :type 'user-error)))
                (should (string-match-p "Consolidate.*one GNOSIS_JOURNAL_ROLES"
                                        (error-message-string err)))))
            (should-not (gnosis-journal--entries))
            (should-not (get-file-buffer source))))
        (let ((gnosis-journal-new-entry-template nil))
          (gnosis-journal-date "2001-02-03"))
        (let ((before (buffer-string)))
          (should-error (gnosis-journal-insert-template "Additive") :type 'user-error)
          (should (equal before (buffer-string))))
        (goto-char (point-max))
        (insert (gnosis-org-expand-headings body (if layout 2 1)))
        (let ((before (buffer-string)))
          (dolist (command '(gnosis-journal-capture gnosis-journal-add-todo))
            (should-error (funcall command "Rejected") :type 'user-error)
            (should (equal before (buffer-string)))))))))

(ert-deftest gnosis-test-journal-routing-initialization-refusal-and-retry ()
  "Native initialization edits survive refusal without a generated shell date."
  (dolist (key '("c" "a" "t"))
    (gnosis-test-journal-templates--with-layouts
      (let* ((source (expand-file-name "tasks.org" gnosis-dir))
             (gnosis-journal-todo-files (list source))
             (gnosis-nodes-completing-read-func (lambda (_prompt choices) (car choices)))
             (calls 0)
             (gnosis-journal-templates
              (list (cons "Mine" (lambda ()
                                   (cl-incf calls)
                                   "{*} Inbox\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: thoughts todos\n:END:\n"))))
             (gnosis-journal-new-entry-template "Mine")
             target recovery)
        (with-temp-file source (insert "* TODO Source\nBody\n"))
        (let ((gnosis-nodes-mode-hook
               (list (lambda ()
                       (when (and buffer-file-name
                                  (file-in-directory-p buffer-file-name gnosis-journal-dir))
                         (setq target (current-buffer))
                         (goto-char (point-max))
                         (insert "Unrelated hook edit\n")
                         (setq gnosis-journal-new-entry-template "Empty"))))))
          (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (&rest _) "Recovered draft"))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (should-error (call-interactively (keymap-lookup gnosis-journal-prefix-map key))
                          :type 'user-error)))
        (should target)
        (should-not (gnosis-journal--unique-entry (format-time-string "%Y-%m-%d")))
        (with-current-buffer target
          (should (string-match-p "Unrelated hook edit" (buffer-string)))
          (should-not (string-match-p "Inbox\\|:ID:" (buffer-string)))
          (should-not (file-exists-p buffer-file-name)))
        (if (equal key "t")
            (with-current-buffer (get-file-buffer source)
              (goto-char (point-min))
              (should (org-id-get))
              (should (buffer-modified-p)))
          (setq recovery
                (seq-find (lambda (buffer)
                            (and (string-prefix-p "*Gnosis journal recovery*" (buffer-name buffer))
                                 (with-current-buffer buffer (equal (buffer-string) "Recovered draft"))))
                          (buffer-list)))
          (should recovery)
          (with-current-buffer recovery (set-buffer-modified-p nil))
          (kill-buffer recovery))
        ;; Retain callback prose, but remove it from the separate new file before
        ;; retry: a nonempty undated separate file must never be overwritten.
        (unless layout
          (with-current-buffer target
            (write-region (point-min) (point-max)
                          (expand-file-name "hook-prose.txt" gnosis-dir))
            (erase-buffer)))
        (setq gnosis-journal-new-entry-template "Mine")
        (with-temp-buffer
          (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (&rest _) "Recovered draft"))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (call-interactively (keymap-lookup gnosis-journal-prefix-map key))))
        (should (= calls 2))
        (gnosis-journal)
        (should (string-match-p "Inbox" (buffer-string)))
        (should (string-match-p (if (equal key "t") "\\[\\[id:" "Recovered draft")
                                (buffer-string)))
        (when layout (should (string-match-p "Unrelated hook edit" (buffer-string))))))))

(ert-deftest gnosis-test-journal-routing-id-callback-owner-refusal ()
  "Native ID callbacks cannot redirect a new date or lose accepted drafts."
  (dolist (change '(file mode successor))
    (dolist (key '("c" "a" "t"))
      (gnosis-test-journal-templates--with-layouts
        (ert-info ((format "Callback %s, key %s, layout %S" change key layout))
          (let* ((source (expand-file-name "tasks.org" gnosis-dir))
                 (replacement (expand-file-name "replacement.org" gnosis-dir))
                 (gnosis-journal-todo-files (list source))
                 (gnosis-nodes-completing-read-func (lambda (_prompt choices) (car choices)))
                 (gnosis-journal-new-entry-template "Mine")
                 (gnosis-journal-templates
                  '(("Mine" . (lambda () "{*} Inbox\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: thoughts todos\n:END:\n"))))
                 (buffers (buffer-list))
                 target file successor)
            (with-temp-file source (insert "* TODO Source\nBody\n"))
            (let ((org-property-changed-functions
                   (list (lambda (property _value)
                           (when (and (equal property "ID") buffer-file-name
                                      (file-in-directory-p buffer-file-name gnosis-journal-dir))
                             (setq target (current-buffer) file buffer-file-name)
                             (if (eq change 'mode)
                                 (fundamental-mode)
                               (set-visited-file-name replacement t))
                             (setq-local header-line-format "Callback state retained")
                             (when (eq change 'successor)
                               (setq successor (find-file-noselect file))
                               (with-current-buffer successor
                                 (insert "Successor draft\n"))))))))
              (cl-letf (((symbol-function 'read-string-from-buffer)
                         (lambda (&rest _) "Recovered Ελληνικά\nsecond line"))
                        ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                (should-error (call-interactively (keymap-lookup gnosis-journal-prefix-map key))
                              :type 'user-error)))
            (should (buffer-live-p target))
            (with-current-buffer target
              (should (equal (buffer-string) ""))
              (should (equal buffer-file-name (if (eq change 'mode) file replacement)))
              (should (eq major-mode (if (eq change 'mode) 'fundamental-mode 'org-mode)))
              (should (equal header-line-format "Callback state retained")))
            (when successor
              (with-current-buffer successor
                (should (equal buffer-file-name file))
                (should (equal (buffer-string) "Successor draft\n"))))
            (should-not (file-exists-p file))
            (should-not (file-exists-p replacement))
            (should-not (gnosis-journal--unique-entry (format-time-string "%Y-%m-%d")))
            (if (equal key "t")
                (with-current-buffer (get-file-buffer source)
                  (goto-char (point-min))
                  (should (org-id-get))
                  (should (buffer-modified-p))
                  (should (equal "* TODO Source\nBody\n"
                                 (with-temp-buffer
                                   (insert-file-contents source)
                                   (buffer-string)))))
              (let ((recovery
                     (seq-find (lambda (buffer)
                                 (and (not (memq buffer buffers))
                                      (string-prefix-p "*Gnosis journal recovery*" (buffer-name buffer))))
                               (buffer-list))))
                (should recovery)
                (should (get-buffer-window recovery))
                (with-current-buffer recovery
                  (should (equal (buffer-string) "Recovered Ελληνικά\nsecond line"))
                  (set-buffer-modified-p nil))
                (kill-buffer recovery)))))))))

(ert-deftest gnosis-test-journal-routing-initialization-before-date ()
  "Successful initialization observes no generated date and is not repeated."
  (dolist (key '("c" "a" "t"))
    (gnosis-test-journal-templates--with-layouts
      (let* ((source (expand-file-name "tasks.org" gnosis-dir))
             (gnosis-journal-todo-files (list source))
             (gnosis-nodes-completing-read-func (lambda (_prompt choices) (car choices)))
             (calls 0)
             (gnosis-nodes-mode-hook
              (list (lambda ()
                      (when (and buffer-file-name
                                 (file-in-directory-p buffer-file-name gnosis-journal-dir))
                        (cl-incf calls)
                        (should-not (gnosis-journal--date-at-point)))))))
        (with-temp-file source (insert "* TODO Source\n"))
        (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (&rest _) "Accepted"))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (call-interactively (keymap-lookup gnosis-journal-prefix-map key)))
        (should (= calls 1))
        (should (gnosis-journal--unique-entry (format-time-string "%Y-%m-%d")))
        (should-not (file-exists-p buffer-file-name))))))

(define-derived-mode gnosis-test-journal-org-mode org-mode "Journal test")

(ert-deftest gnosis-test-journal-routing-source-mode-refusal ()
  "Mode-only source changes at ID confirmation must not partly insert a drawer."
  (dolist (initial '(org fundamental derived))
    (gnosis-test-journal-templates--with-layouts
      (gnosis-journal-date "2001-02-03")
      (let* ((owner (current-buffer))
             (before (buffer-string))
             (source (expand-file-name "tasks.org" gnosis-dir))
             (gnosis-journal-todo-files (list source))
             (gnosis-nodes-completing-read-func (lambda (_prompt choices) (car choices))))
        (with-temp-file source (insert "* TODO Source\nBody\n"))
        (let* ((buffer (find-file-noselect source))
               (text (with-current-buffer buffer
                       (when (eq initial 'fundamental) (fundamental-mode))
                       (buffer-string))))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _)
                       (with-current-buffer buffer
                         (if (eq initial 'derived)
                             (gnosis-test-journal-org-mode)
                           (fundamental-mode)))
                       t)))
            (should-error (gnosis-journal-insert-task) :type 'user-error))
          (with-current-buffer buffer (should (equal text (buffer-string))))
          (with-current-buffer owner (should (equal before (buffer-string)))))))))

(ert-deftest gnosis-test-journal-routing-source-id-atomic-failure ()
  "A native property hook error rolls back the incomplete source ID insertion."
  (gnosis-test-journal-templates--with-layouts
    (gnosis-journal-date "2001-02-03")
    (let* ((owner (current-buffer))
           (before (buffer-string))
           (source (expand-file-name "tasks.org" gnosis-dir))
           (gnosis-journal-todo-files (list source))
           (gnosis-nodes-completing-read-func (lambda (_prompt choices) (car choices))))
      (with-temp-file source (insert "* TODO Source\nBody\n"))
      (let* ((buffer (find-file-noselect source))
             (text (with-current-buffer buffer (buffer-string)))
             (org-property-changed-functions
              (list (lambda (property _value)
                      (when (and (equal property "ID") (eq (current-buffer) buffer))
                        (user-error "Refuse ID property"))))))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (should-error (gnosis-journal-insert-task) :type 'user-error))
        (with-current-buffer buffer (should (equal text (buffer-string))))
        (with-current-buffer owner (should (equal before (buffer-string))))))))


(provide 'gnosis-test-journal-templates)
;;; gnosis-test-journal-templates.el ends here
