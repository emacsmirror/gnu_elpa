;;; gnosis-test-campaign-authoring.el --- Native authoring regressions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Exercise disjoint clozes, editor acquisition and committed save identities.

;;; Code:

(require 'gnosis-test-draft-ownership)

(ert-deftest gnosis-test-campaign-authoring-cloze-spans ()
  (dolist (case '(("alpha" ("alpha" "alpha") nil)
                  ("alpha" ("alpha" "pha") nil)
                  ("alpha alpha" ("alpha" "alpha") t)
                  ("ababa" ("ab" "aba") t)
                  ("12.2 and 2" ("2" "2") t)
                  ("transporter" ("port") t)))
    (should (eq (gnosis-cloze-check (car case) (cadr case)) (nth 2 case)))
  (dolist (case '(("alpha alpha" ("alpha" "alpha"))
                  ("ababa" ("ab" "aba"))
                  ("12.2 and 2" ("2" "2"))
                  ("transporter" ("port"))))
    (gnosis-test-with-db
      (gnosis-test-draft--with-editor
        (gnosis-add-thema "cloze" (car case) nil
                          (mapconcat #'identity (cadr case) gnosis-export-separator))
        (call-interactively (key-binding (kbd "C-c C-c")))
        (should (equal (gnosis-get 'answer 'themata) (cadr case)))
        (should (cl-every #'identity
                          (gnosis-cloze--spans (gnosis-get 'keimenon 'themata)
                                               (gnosis-get 'answer 'themata)))))))))

(ert-deftest gnosis-test-campaign-authoring-cloze-refusal ()
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Unrelated" "A" nil nil 111)
    (gnosis-scheduler-accept-review (gnosis-scheduler-event-id) 111 'success
                                   1780000000000000 (gnosis--today-int))
    (dolist (edit '(nil t))
      (dolist (answer '("alpha\n- alpha" "alpha\n- pha" "hint"))
        (gnosis-test-draft--with-editor
          (if edit (gnosis-edit-thema 111)
            (gnosis-add-thema "cloze" "alpha" nil "alpha"))
          (let ((inhibit-read-only t)) (erase-buffer))
          (gnosis-export--insert-thema
           (if edit "111" "NEW") "cloze" "{c1:alpha::hint}" nil answer "")
          (let ((rows (gnosis-test-draft--rows)) (text (buffer-string)) seen)
            (let ((gnosis-save-hook (list (lambda (id) (push id seen)))))
              (should-error (call-interactively (key-binding (kbd "C-c C-c")))
                            :type 'user-error))
            (should-not seen)
            (should (equal rows (gnosis-test-draft--rows)))
            (should (equal text (buffer-string)))
            (call-interactively (key-binding (kbd "C-c C-k")))))))))

(ert-deftest gnosis-test-campaign-authoring-native-inline-refusal ()
  (skip-unless (not noninteractive))
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Unrelated" "A" nil nil 111)
    (gnosis-scheduler-accept-review (gnosis-scheduler-event-id) 111 'success
                                   1780000000000000 (gnosis--today-int))
    (dolist (question '("{c1:a}x {c1:a b}"
                        "{c1:valid} {c2:a}x {c2:a b}"))
      (gnosis-test-draft--with-editor
        (gnosis-add-thema "cloze" question)
        (let ((draft (current-buffer))
              (text (buffer-string))
              (rows (gnosis-test-draft--rows))
              seen)
          (let ((gnosis-save-hook (list (lambda (id) (push id seen)))))
            (should-error (execute-kbd-macro (kbd "C-c C-c"))
                          :type 'user-error))
          (should-not seen)
          (should (equal rows (gnosis-test-draft--rows)))
          (should (buffer-live-p draft))
          (should (eq draft (current-buffer)))
          (should (equal text (buffer-string)))
          (should-not buffer-read-only)
          (should-not gnosis--draft-saved-p)
          (execute-kbd-macro (kbd "C-c C-k"))
          (should (equal rows (gnosis-test-draft--rows))))))))

(ert-deftest gnosis-test-campaign-authoring-native-inline-success ()
  (skip-unless (not noninteractive))
  (dolist (case '(("{c1:alpha} {c1:alpha}" 1)
                  ("{c1:ab}{c1:aba}" 1)
                  ("{c1:alpha} {c1:alpha} {c2:beta}" 2)))
    (gnosis-test-with-db
      (gnosis-test-draft--with-editor
        (gnosis-add-thema "cloze" (car case))
        (let ((draft (current-buffer)) seen)
          (let ((gnosis-save-hook (list (lambda (id) (push id seen)))))
            (execute-kbd-macro (kbd "C-c C-c")))
          (should-not (buffer-live-p draft))
          (should (= (length seen) (cadr case)))
          (should (equal (sort seen #'<)
                         (sort (gnosis-select 'id 'themata nil t) #'<)))
          (dolist (id seen)
            (let ((question (gnosis-get 'keimenon 'themata `(= id ,id)))
                  (answer (gnosis-get 'answer 'themata `(= id ,id))))
              (should (gnosis-cloze-check question answer))
              (should (cl-every #'identity
                                (gnosis-cloze--spans question answer)))))
          (should-not (gnosis-select '* 'review-events))
          (should-not (gnosis-select '* 'practice-events)))))))

(ert-deftest gnosis-test-campaign-authoring-foreign-editor ()
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Question" "Answer" nil nil 123)
    (dolist (file-p '(nil t))
      (dolist (modified '(nil t))
        (gnosis-test-draft--with-editor
          (let* ((file (expand-file-name "foreign.org" gnosis-dir))
                 (foreign (get-buffer-create "*Gnosis Edit*")))
            (with-current-buffer foreign
              (text-mode)
              (insert "Foreign text\n")
              (when file-p
                (set-visited-file-name file t)
                (rename-buffer "*Gnosis Edit*" t)
                (save-buffer))
              (text-mode)
              (set-buffer-modified-p modified))
            (should-error (gnosis-edit-thema 123) :type 'user-error)
            (with-current-buffer foreign
              (should (equal (buffer-string) "Foreign text\n"))
              (should (eq major-mode 'text-mode))
              (should (eq (buffer-modified-p) modified))
              (should (equal buffer-file-name (and file-p file)))
              (set-buffer-modified-p nil))
            (when file-p
              (should (equal (with-temp-buffer (insert-file-contents file)
                                              (buffer-string))
                             "Foreign text\n")))))))))

(ert-deftest gnosis-test-campaign-authoring-save-identities ()
  (dolist (case '(("basic" "Question" "Answer" 1)
                  ("double" "Question" "Answer" 2)
                  ("cloze" "{c1:alpha} {c2:beta}" nil 2)))
    (gnosis-test-with-db
      (gnosis-test-draft--with-editor
        (gnosis-add-thema (nth 0 case) (nth 1 case) nil (nth 2 case))
        (let (seen)
          (let ((gnosis-save-hook
                 (list (lambda (id)
                         (should (gnosis-get 'id 'themata `(= id ,id)))
                         (push id seen)))))
            (call-interactively (key-binding (kbd "C-c C-c"))))
          (should (= (length seen) (nth 3 case)))
          (should (equal (sort seen #'<)
                         (sort (gnosis-select 'id 'themata nil t) #'<)))
          (let ((id (car seen)))
            (setq seen nil)
            (gnosis-edit-thema id)
            (let ((gnosis-save-hook (list (lambda (saved) (push saved seen)))))
              (call-interactively (key-binding (kbd "C-c C-c"))))
            (should (equal seen (list id)))))))))

(ert-deftest gnosis-test-campaign-authoring-save-failure-and-cancel ()
  (gnosis-test-with-db
    (gnosis-test-draft--with-editor
      (let (seen)
        (gnosis-add-thema "double" "Forward" nil "Reverse")
        (sqlite-execute gnosis-db
                        "CREATE TRIGGER fail_reverse BEFORE INSERT ON themata
                         WHEN NEW.keimenon = '\"Reverse\"'
                         BEGIN SELECT RAISE(ABORT, 'controlled failure'); END")
        (let ((gnosis-save-hook (list (lambda (id) (push id seen))))
              (rows (gnosis-test-draft--rows)))
          (should-error (call-interactively (key-binding (kbd "C-c C-c")))
                        :type 'user-error)
          (should-not seen)
          (should (equal rows (gnosis-test-draft--rows)))
          (call-interactively (key-binding (kbd "C-c C-k")))
          (should-not seen)
          (should (equal rows (gnosis-test-draft--rows))))))))

(ert-deftest gnosis-test-campaign-authoring-editor-detached-and-unmodified ()
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Question" "Answer" nil nil 123)
    (gnosis-test-draft--with-editor
      (gnosis-edit-thema 123)
      (let ((draft (current-buffer)) (text (buffer-string)))
        (set-buffer-modified-p nil)
        (should-error (gnosis-edit-thema 123) :type 'user-error)
        (should (equal text (buffer-string)))
        (set-visited-file-name (expand-file-name "detached.org" gnosis-dir) t)
        (set-visited-file-name nil t)
        (rename-buffer "*Gnosis Edit*")
        (set-buffer-modified-p nil)
        (should-error (gnosis-edit-thema 123) :type 'user-error)
        (should (eq draft (get-buffer "*Gnosis Edit*")))
        (should (equal text (buffer-string)))))))

(ert-deftest gnosis-test-campaign-authoring-editor-rename-collision ()
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Question" "Answer" nil nil 123)
    (gnosis-test-draft--with-editor
      (let (owner successor)
        (unwind-protect
            (let ((display-buffer-overriding-action
                   (list (lambda (buffer _alist)
                           (setq owner buffer)
                           (with-current-buffer buffer
                             (rename-buffer " *renamed-authoring*"))
                           (setq successor (get-buffer-create "*Gnosis Edit*"))
                           (with-current-buffer successor
                             (text-mode)
                             (insert "Successor text"))
                           (set-window-buffer (selected-window) buffer)
                           (selected-window)))))
              (gnosis-edit-thema 123)
              (should (eq (current-buffer) owner))
              (should (string-match-p "Question" (buffer-string)))
              (should (equal (with-current-buffer successor (buffer-string))
                             "Successor text"))
              (execute-kbd-macro (kbd "C-c C-k"))
              (should-not (buffer-live-p owner))
              (should (buffer-live-p successor)))
          (when (buffer-live-p owner) (kill-buffer owner)))))))

(ert-deftest gnosis-test-campaign-authoring-native-recursive-save ()
  (skip-unless (not noninteractive))
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Question" "Answer" nil nil 123)
    (gnosis-test-draft--with-editor
      (dolist (key '("C-c C-c" "C-c C-k"))
        (gnosis-edit-thema 123)
        (let ((gnosis-review-editing-p t)
              (before (gnosis-test-draft--rows))
              seen)
          (let ((gnosis-save-hook (list (lambda (id) (push id seen))))
                (unread-command-events (listify-key-sequence (kbd key))))
            (recursive-edit))
          (should (equal seen (and (equal key "C-c C-c") '(123))))
          (should (equal before (gnosis-test-draft--rows)))
          (should-not (get-buffer "*Gnosis Edit*")))))))

(ert-deftest gnosis-test-campaign-authoring-native-cloze-review ()
  (skip-unless (not noninteractive))
  (require 'gnosis-review)
  (gnosis-test-with-db
    (gnosis-test-draft--with-editor
      (gnosis-add-thema "cloze" "alpha alpha" nil "alpha\n- alpha")
      (execute-kbd-macro (kbd "C-c C-c"))
      (let* ((id (gnosis-get 'id 'themata))
             (answer (gnosis-get 'answer 'themata `(= id ,id)))
             (spans (gnosis-cloze--spans "alpha alpha" answer))
             (gnosis-latex-preview nil)
             (gnosis-script-input-method-alist nil)
             (unread-command-events
              (listify-key-sequence (kbd "alpha RET alpha RET q"))))
        (should (equal spans '((0 . 5) (6 . 11))))
        (should (equal (substring-no-properties
                        (car (gnosis-cloze--render "alpha alpha" answer '(0 1) nil)))
                       (concat gnosis-cloze-string " " gnosis-cloze-string)))
        (gnosis-review-loop (list id) 'practice)
        (should (= (length (gnosis-select '* 'practice-events)) 1))
        (let* ((encounter (gnosis-get 'data 'practice-encounters))
               (inputs (plist-get (plist-get encounter :response) :inputs)))
          (should (equal (mapcar (lambda (input)
                                  (plist-get input :matched-blank-index)) inputs)
                         '(0 1))))
        (should-not (gnosis-select '* 'review-events))))))

(ert-deftest gnosis-test-campaign-authoring-native-review-edit ()
  (skip-unless (not noninteractive))
  (require 'gnosis-review)
  (dolist (mode '(due practice))
    (dolist (key '("C-c C-c" "C-c C-k"))
      (gnosis-test-with-db
        (gnosis-test-draft--with-editor
          (gnosis-test--add-basic-thema "Question" "Answer" nil nil 123)
          (let* ((gnosis-latex-preview nil)
                 (gnosis-script-input-method-alist nil)
                 (unread-command-events
                  (listify-key-sequence
                   (kbd (concat "Answer RET e " key " q"))))
                 seen
                 (gnosis-save-hook (list (lambda (id) (push id seen)))))
            (gnosis-review-loop (list 123) mode)
            (should (equal seen (and (equal key "C-c C-c") '(123))))
            (should (= (length (gnosis-select
                                '* (if (eq mode 'due) 'review-events
                                     'practice-events))) 1))
            (should-not (get-buffer "*Gnosis Edit*"))))))))

(provide 'gnosis-test-campaign-authoring)
;;; gnosis-test-campaign-authoring.el ends here
