;;; gnosis-test-journal-capture-alias.el --- Physical journal owners -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Public capture, template and source-task insertion through file aliases.

;;; Code:

(require 'gnosis-test-journal-selection)

(defun gnosis-test-journal-capture-alias--configure (kind file)
  "Configure KIND of alias to FILE and return its name."
  (let ((alias (expand-file-name "configured.org" gnosis-journal-dir)))
    (setq gnosis-journal-file
          (pcase kind
            ('symlink (make-symbolic-link file alias) alias)
            ('hardlink (add-name-to-file file alias) alias)
            (_ file)))))

(defun gnosis-test-journal-capture-alias--prepare (file)
  "Visit FILE, rename its date in a dirty buffer, and narrow within it."
  (find-file file)
  (rename-buffer (generate-new-buffer-name "Journal owner"))
  (goto-char (point-min))
  (search-forward "2001-02-03")
  (replace-match "2001-02-04")
  (goto-char (point-max))
  (insert "** Detail\nUnsaved prose\n* TODO Source task\n* 2001-02-05\nSibling\n")
  (goto-char (point-min))
  (search-forward "** Detail")
  (beginning-of-line)
  (org-narrow-to-subtree))

(defun gnosis-test-journal-capture-alias--snapshot ()
  "Return full text and the current buffer's edit and view state."
  (list (save-restriction (widen) (buffer-string))
        buffer-file-name major-mode (point) (point-min) (point-max)
        (buffer-modified-p) (buffer-chars-modified-tick)))

(ert-deftest gnosis-test-journal-capture-alias-public-insertion ()
  "All public insertion routes use the dirty physical owner, not disk text."
  (dolist (kind '(canonical symlink hardlink))
    (dolist (command '(gnosis-journal-capture gnosis-journal-add-todo
                      gnosis-journal-insert-template gnosis-journal-insert-task))
      (gnosis-test-journal-selection--with-files
        (let* ((file (gnosis-test-journal-selection--file "journal.org" "2001-02-03" "day"))
               (alias (gnosis-test-journal-capture-alias--configure kind file))
               (disk (gnosis-test-journal-selection--read file))
               (index (gnosis-nodes-select '* 'journal))
               (gnosis-journal-todo-files (list alias))
               (gnosis-journal-templates '(("Prose" . (lambda () "Template prose\n"))))
               (gnosis-nodes-completing-read-func (lambda (_ choices &rest _) (car choices))))
          (gnosis-test-journal-capture-alias--prepare file)
          (let ((owner (current-buffer)))
            (should (equal (nth 2 (gnosis-journal--unique-entry "2001-02-04")) alias))
            (cl-letf (((symbol-function 'read-string-from-buffer)
                       (lambda (&rest _) "Accepted prose\nSecond line Ελληνικά"))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (call-interactively command))
            (should (eq (current-buffer) owner))
            (should (equal buffer-file-name file))
            (should (equal (gnosis-journal--date-at-point) "2001-02-04"))
            (should (buffer-modified-p))
            (should (string-match-p "Unsaved prose" (buffer-string)))
            (pcase command
              ('gnosis-journal-insert-template
               (insert "Writable after template\n")
               (should (equal (gnosis-journal--date-at-point) "2001-02-04")))
              ('gnosis-journal-insert-task
               (let ((id (caar (gnosis-test-journal-selection--links))))
                 (should id)
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "* TODO Source task")
                   (should (equal (org-id-get) id)))))
              (_ (should (string-match-p "Accepted prose\nSecond line Ελληνικά" (buffer-string)))))
            (should (equal disk (gnosis-test-journal-selection--read file)))
            (should (equal disk (gnosis-test-journal-selection--read alias)))
            (should (equal index (gnosis-nodes-select '* 'journal)))
            (let ((text (buffer-string)))
              (save-buffer)
              (kill-buffer)
              (find-file file)
              (should (equal text (buffer-string))))))))))

(ert-deftest gnosis-test-journal-capture-alias-new-date ()
  "Creating today's date uses an already visited canonical journal."
  (dolist (kind '(canonical symlink hardlink))
    (gnosis-test-journal-selection--with-files
      (let* ((file (gnosis-test-journal-selection--file "journal.org" "2001-02-03" "day"))
             (alias (gnosis-test-journal-capture-alias--configure kind file))
             (gnosis-journal-new-entry-template "Prose")
             (gnosis-journal-templates '(("Prose" . (lambda () "Template prose\n")))))
        (gnosis-test-journal-capture-alias--prepare file)
        (let ((owner (current-buffer))
              (disk (gnosis-test-journal-selection--read file)))
          (with-temp-buffer (gnosis-journal-capture "New date prose"))
          (with-current-buffer owner
            (should (equal buffer-file-name file))
            (should (string-match-p "New date prose" (buffer-string)))
            (should (string-match-p "Template prose" (buffer-string)))
            (should (string-match-p "Unsaved prose" (buffer-string))))
          (should (equal (nth 2 (gnosis-journal--unique-entry
                                (format-time-string "%Y-%m-%d"))) alias))
          (should (equal disk (gnosis-test-journal-selection--read file))))))))

(ert-deftest gnosis-test-journal-capture-alias-cancel ()
  "Cancellation preserves dirty narrowed owners and never writes to disk."
  (dolist (kind '(canonical symlink hardlink))
    (dolist (command '(gnosis-journal-capture gnosis-journal-insert-template
                      gnosis-journal-insert-task))
      (gnosis-test-journal-selection--with-files
        (let* ((file (gnosis-test-journal-selection--file "journal.org" "2001-02-03" "day"))
               (alias (gnosis-test-journal-capture-alias--configure kind file))
               (gnosis-journal-todo-files (list alias))
               (gnosis-nodes-completing-read-func (lambda (&rest _) (signal 'quit nil)))
               (disk (gnosis-test-journal-selection--read file))
               (index (gnosis-nodes-select '* 'journal)))
          (gnosis-test-journal-capture-alias--prepare file)
          (let ((before (gnosis-test-journal-capture-alias--snapshot)))
            (cl-letf (((symbol-function 'read-string-from-buffer)
                       (lambda (&rest _) (signal 'quit nil))))
              (should (condition-case nil (call-interactively command) (quit t))))
            (should (equal before (gnosis-test-journal-capture-alias--snapshot)))
            (should (equal disk (gnosis-test-journal-selection--read file)))
            (should (equal index (gnosis-nodes-select '* 'journal)))))))))

(ert-deftest gnosis-test-journal-capture-alias-prompt-ownership ()
  "Input cannot retarget an alias owner, even to another name of its file."
  (dolist (kind '(symlink hardlink))
    (dolist (command '(gnosis-journal-capture gnosis-journal-insert-template
                      gnosis-journal-insert-task))
      (dolist (change '(destination database file mode text target))
        (gnosis-test-journal-selection--with-files
          (let* ((file (gnosis-test-journal-selection--file "journal.org" "2001-02-03" "day"))
                 (alias (gnosis-test-journal-capture-alias--configure kind file))
                 (gnosis-journal-todo-files (list alias))
                 (disk (gnosis-test-journal-selection--read file))
                 (index (gnosis-nodes-select '* 'journal))
                 (original-db gnosis-db)
                 (foreign-db (gnosis-sqlite-open (expand-file-name "other.db" gnosis-dir))))
            (unwind-protect
                (progn
                  (let ((gnosis-db foreign-db)) (gnosis-db-init))
                  (gnosis-test-journal-capture-alias--prepare file)
                  (let* ((owner (current-buffer))
                         after
                         (change-owner
                          (lambda ()
                            (with-current-buffer owner
                              (pcase change
                                ('destination (setq gnosis-journal-file "other.org"))
                                ('database (setq gnosis-db foreign-db))
                                ('file (set-visited-file-name alias t))
                                ('mode (fundamental-mode))
                                ('text (insert "Successor prose\n"))
                                ('target
                                 (let ((replacement (expand-file-name "replacement.org" gnosis-nodes-dir))
                                       (text (car (gnosis-test-journal-capture-alias--snapshot))))
                                   (with-temp-file replacement (insert text))
                                   (delete-file alias)
                                   (if (eq kind 'symlink)
                                       (make-symbolic-link replacement alias)
                                     (add-name-to-file replacement alias)))))
                              (setq after (gnosis-test-journal-capture-alias--snapshot)))))
                         (gnosis-nodes-completing-read-func
                          (lambda (_ choices &rest _)
                            (funcall change-owner)
                            (car choices))))
                    (cl-letf (((symbol-function 'read-string-from-buffer)
                               (lambda (&rest _) (funcall change-owner) "Refused prose")))
                      (should-error (call-interactively command) :type 'user-error))
                    (with-current-buffer owner
                      (should (equal after (gnosis-test-journal-capture-alias--snapshot))))
                    (when (eq change 'target)
                      (should (equal (car after) (gnosis-test-journal-selection--read alias)))))
                  (should (equal disk (gnosis-test-journal-selection--read file)))
                  (let ((gnosis-db original-db))
                    (should (equal index (gnosis-nodes-select '* 'journal))))
                  (let ((gnosis-db foreign-db))
                    (should-not (gnosis-nodes-select '* 'journal))))
              (setq gnosis-db original-db)
              (gnosis-sqlite-close foreign-db))))))))

(ert-deftest gnosis-test-journal-capture-alias-template-callback ()
  "Template callbacks keep unchanged owners and refuse retired owners."
  (dolist (kind '(canonical symlink hardlink))
    (dolist (change '(rename file mode))
      (gnosis-test-journal-selection--with-files
        (let* ((file (gnosis-test-journal-selection--file "journal.org" "2001-02-03" "day"))
               (alias (gnosis-test-journal-capture-alias--configure kind file))
               (disk (gnosis-test-journal-selection--read file))
               (gnosis-journal-templates
                (list (cons "Callback"
                            (lambda ()
                              (pcase change
                                ('rename (rename-buffer (generate-new-buffer-name "Still owned")))
                                ('file (set-visited-file-name (if (equal file alias)
                                                                 (concat file ".other") alias) t))
                                ('mode (fundamental-mode)))
                              "Template callback prose\n")))))
          (gnosis-test-journal-capture-alias--prepare file)
          (if (eq change 'rename)
              (progn
                (gnosis-journal-insert-template "Callback")
                (should (string-match-p "Template callback prose" (buffer-string))))
            (let ((before (car (gnosis-test-journal-capture-alias--snapshot))))
              (should-error (gnosis-journal-insert-template "Callback") :type 'user-error)
              (should (equal before (car (gnosis-test-journal-capture-alias--snapshot))))))
          (should (equal disk (gnosis-test-journal-selection--read file))))))))

(ert-deftest gnosis-test-journal-capture-alias-id-callback ()
  "New-date ID callbacks preserve exact file association and mode checks."
  (dolist (kind '(canonical symlink hardlink))
    (dolist (change '(rename file mode))
      (gnosis-test-journal-selection--with-files
        (let* ((file (gnosis-test-journal-selection--file "journal.org" "2001-02-03" "day"))
               (alias (gnosis-test-journal-capture-alias--configure kind file))
               (disk (gnosis-test-journal-selection--read file)))
          (gnosis-test-journal-capture-alias--prepare file)
          (let* ((owner (current-buffer))
                 (before (car (gnosis-test-journal-capture-alias--snapshot)))
                 (org-property-changed-functions
                  (list (lambda (property _value)
                          (when (and (equal property "ID") (eq (current-buffer) owner))
                            (pcase change
                              ('rename (rename-buffer (generate-new-buffer-name "Still owned")))
                              ('file (set-visited-file-name (if (equal file alias)
                                                               (concat file ".other") alias) t))
                              ('mode (fundamental-mode))))))))
            (with-temp-buffer
              (if (eq change 'rename)
                  (gnosis-journal-capture "ID callback prose")
                (should-error (gnosis-journal-capture "ID callback prose") :type 'user-error)))
            (with-current-buffer owner
              (if (eq change 'rename)
                  (should (string-match-p "ID callback prose" (buffer-string)))
                (should (equal before (car (gnosis-test-journal-capture-alias--snapshot))))))
            (should (equal disk (gnosis-test-journal-selection--read file)))))))))

(ert-deftest gnosis-test-journal-capture-alias-source-association ()
  "An aliased task source retains its original file and mode during input."
  (dolist (kind '(symlink hardlink))
    (dolist (change '(rename file mode))
      (gnosis-test-journal-selection--with-files
        (let* ((file (gnosis-test-journal-selection--file "journal.org" "2001-02-03" "day"))
               (tasks (expand-file-name "tasks.org" gnosis-nodes-dir))
               (alias (expand-file-name "tasks-alias.org" gnosis-nodes-dir))
               (gnosis-journal-todo-files (list alias)))
          (with-temp-file tasks (insert "* TODO Source task\n"))
          (if (eq kind 'symlink) (make-symbolic-link tasks alias)
            (add-name-to-file tasks alias))
          (let* ((source (find-file-noselect tasks))
                 (gnosis-nodes-completing-read-func
                  (lambda (_ choices &rest _)
                    (with-current-buffer source
                      (pcase change
                        ('rename (rename-buffer (generate-new-buffer-name "Source owner")))
                        ('file (set-visited-file-name alias t))
                        ('mode (fundamental-mode))))
                    (car choices))))
            (gnosis-test-journal-capture-alias--prepare file)
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (if (eq change 'rename)
                  (progn
                    (call-interactively #'gnosis-journal-insert-task)
                    (should (caar (gnosis-test-journal-selection--links))))
                (should-error (call-interactively #'gnosis-journal-insert-task) :type 'user-error)
                (with-current-buffer source
                  (should (equal (buffer-string) "* TODO Source task\n")))))
            (should (equal (gnosis-test-journal-selection--read tasks)
                           "* TODO Source task\n"))))))))

(defun gnosis-test-journal-capture-alias--external-insertion (kind command)
  "Exercise COMMAND in KIND's physical owner outside the journal directory.
Use native prefix bindings and readers in interactive Emacs; batch runs
replace only input readers.  Assert the same saved content in both cases."
  (gnosis-test-journal-selection--with-files
    (let* ((file (expand-file-name "external-journal.org" gnosis-nodes-dir))
           (alias
            (progn
              (with-temp-file file
                (insert "#+title: Journal\n* 2001-02-03\n:PROPERTIES:\n:ID: day\n:END:\nBody\n"))
              (gnosis-test-journal-capture-alias--configure kind file)))
           (gnosis-journal-todo-files (list alias))
           (gnosis-journal-templates '(("Prose" . (lambda () "Template prose\n"))))
           (gnosis-nodes-completing-read-func #'completing-read)
           (global-map (copy-keymap global-map)))
      (gnosis-test-journal-capture-alias--prepare file)
      (gnosis-nodes-mode 1)
      (let ((owner (current-buffer))
            (disk (gnosis-test-journal-selection--read file))
            (index (gnosis-nodes-select '* 'journal)))
        (should-not (file-in-directory-p file gnosis-journal-dir))
        (should (eq (find-buffer-visiting alias) owner))
        (should (equal (nth 2 (gnosis-journal--unique-entry "2001-02-04")) alias))
        (should (equal (gnosis-journal--date-at-point) "2001-02-04"))
        (should (buffer-narrowed-p))
        (should (buffer-modified-p))
        (if noninteractive
            (let ((gnosis-nodes-completing-read-func
                   (lambda (_ choices &rest _) (car choices))))
              (cl-letf (((symbol-function 'read-string-from-buffer)
                         (lambda (&rest _) "Accepted prose\nSecond line"))
                        ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                (call-interactively command)))
          (let ((original-map (current-global-map)))
            (unwind-protect
                (progn
                  (define-key global-map (kbd "C-c j") gnosis-journal-prefix-map)
                  (use-global-map global-map)
                  (execute-kbd-macro
                   (pcase command
                     ('gnosis-journal-capture
                      (vconcat (kbd "C-c j c") "Accepted prose" (kbd "RET")
                               "Second line" (kbd "C-c C-c")))
                     ('gnosis-journal-add-todo
                      (vconcat (kbd "C-c j a") "Accepted prose" (kbd "RET")
                               "Second line" (kbd "C-c C-c")))
                     ('gnosis-journal-insert-template
                      (vconcat (kbd "C-c j i") "Prose" (kbd "RET")))
                     ('gnosis-journal-insert-task
                      (vconcat (kbd "C-c j t") "Source" (kbd "TAB RET y"))))))
              (use-global-map original-map))))
        (should (eq (current-buffer) owner))
        (should (equal buffer-file-name file))
        (should (equal (gnosis-journal--date-at-point) "2001-02-04"))
        (should-not (buffer-narrowed-p))
        (should (buffer-modified-p))
        (should (equal (nth 2 (gnosis-journal--unique-entry "2001-02-04")) alias))
        (should-not (gnosis-journal--unique-entry (format-time-string "%Y-%m-%d")))
        (save-excursion
          (goto-char (point-min))
          (search-forward
           (pcase command
             ('gnosis-journal-insert-template "Template prose")
             ('gnosis-journal-insert-task "[[id:")
             ('gnosis-journal-add-todo "[ ] Accepted prose\nSecond line")
             (_ "Accepted prose\nSecond line")))
          (should (equal (gnosis-journal--date-at-point) "2001-02-04")))
        (when (eq command 'gnosis-journal-insert-template)
          (if noninteractive (insert "Writable after template\n")
            (execute-kbd-macro (vconcat "Writable after template" (kbd "RET"))))
          (should (equal (gnosis-journal--date-at-point) "2001-02-04")))
        (when (eq command 'gnosis-journal-insert-task)
          (let ((links (gnosis-test-journal-selection--links)))
            (should (= (length links) 1))
            (should (equal (cadar links) "Source task"))
            (save-excursion
              (goto-char (point-min))
              (search-forward "* TODO Source task")
              (should (equal (org-id-get) (caar links))))))
        (save-excursion
          (goto-char (point-min))
          (search-forward "** Detail\nUnsaved prose\n")
          (forward-line -1)
          (should (equal (gnosis-journal--date-at-point) "2001-02-04"))
          (search-forward "* 2001-02-05")
          (beginning-of-line)
          (should (equal (buffer-substring-no-properties (point) (point-max))
                         "* 2001-02-05\nSibling\n"))
          (should (= (org-current-level) 1)))
        (should (= (count-matches "^\\* " (point-min) (point-max)) 3))
        (should (equal disk (gnosis-test-journal-selection--read file)))
        (should (equal disk (gnosis-test-journal-selection--read alias)))
        (should (equal index (gnosis-nodes-select '* 'journal)))
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (if noninteractive (save-buffer) (execute-kbd-macro (kbd "C-x C-s")))
          (kill-buffer)
          (find-file alias)
          (should (equal text (buffer-substring-no-properties (point-min) (point-max))))
          (should (equal text (gnosis-test-journal-selection--read file)))
          (should (equal text (gnosis-test-journal-selection--read alias)))
          (should (equal (nth 2 (gnosis-journal--unique-entry "2001-02-04")) alias))
          (should-not (gnosis-journal--unique-entry (format-time-string "%Y-%m-%d"))))))))

(ert-deftest gnosis-test-journal-capture-alias-external-canonical-capture ()
  (gnosis-test-journal-capture-alias--external-insertion 'canonical #'gnosis-journal-capture))

(ert-deftest gnosis-test-journal-capture-alias-external-canonical-todo ()
  (gnosis-test-journal-capture-alias--external-insertion 'canonical #'gnosis-journal-add-todo))

(ert-deftest gnosis-test-journal-capture-alias-external-canonical-template ()
  (gnosis-test-journal-capture-alias--external-insertion 'canonical #'gnosis-journal-insert-template))

(ert-deftest gnosis-test-journal-capture-alias-external-canonical-task ()
  (gnosis-test-journal-capture-alias--external-insertion 'canonical #'gnosis-journal-insert-task))

(ert-deftest gnosis-test-journal-capture-alias-external-symlink-capture ()
  (gnosis-test-journal-capture-alias--external-insertion 'symlink #'gnosis-journal-capture))

(ert-deftest gnosis-test-journal-capture-alias-external-symlink-todo ()
  (gnosis-test-journal-capture-alias--external-insertion 'symlink #'gnosis-journal-add-todo))

(ert-deftest gnosis-test-journal-capture-alias-external-symlink-template ()
  (gnosis-test-journal-capture-alias--external-insertion 'symlink #'gnosis-journal-insert-template))

(ert-deftest gnosis-test-journal-capture-alias-external-symlink-task ()
  (gnosis-test-journal-capture-alias--external-insertion 'symlink #'gnosis-journal-insert-task))

(ert-deftest gnosis-test-journal-capture-alias-external-hardlink-capture ()
  (gnosis-test-journal-capture-alias--external-insertion 'hardlink #'gnosis-journal-capture))

(ert-deftest gnosis-test-journal-capture-alias-external-hardlink-todo ()
  (gnosis-test-journal-capture-alias--external-insertion 'hardlink #'gnosis-journal-add-todo))

(ert-deftest gnosis-test-journal-capture-alias-external-hardlink-template ()
  (gnosis-test-journal-capture-alias--external-insertion 'hardlink #'gnosis-journal-insert-template))

(ert-deftest gnosis-test-journal-capture-alias-external-hardlink-task ()
  (gnosis-test-journal-capture-alias--external-insertion 'hardlink #'gnosis-journal-insert-task))

(provide 'gnosis-test-journal-capture-alias)
;;; gnosis-test-journal-capture-alias.el ends here
