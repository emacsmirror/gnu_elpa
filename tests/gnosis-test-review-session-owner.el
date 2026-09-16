;;; gnosis-test-review-session-owner.el --- Session navigation owners -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Keep a batch's original lifetime through native navigation and source input.

;;; Code:

(require 'gnosis-test-summary-owner)
(require 'gnosis-test-review-active-owner)

(defun gnosis-test-session-owner--replace (buffer mutation)
  "Apply MUTATION to BUFFER and return its successor view snapshot."
  (with-current-buffer buffer
    (pcase mutation
      ('roundtrip (fundamental-mode) (gnosis-mode))
      ('setup (gnosis-review--setup-buffer '(222) 'due))
      ('detach (set-visited-file-name (expand-file-name "successor" gnosis-dir) t)
               (set-visited-file-name nil t))
      ('rename (rename-buffer (concat (buffer-name) " renamed"))
               (with-current-buffer (get-buffer-create gnosis-review-buffer-name)
                 (insert "Foreign same-name draft"))))
    (unless (eq mutation 'rename)
      (let ((inhibit-read-only t)) (erase-buffer) (insert "Successor draft"))
      (use-local-map (make-sparse-keymap))
      (setq-local header-line-format "Successor header"))
    (list (buffer-string) (current-local-map) header-line-format)))

(defun gnosis-test-session-owner--assert-view (buffer view)
  "Require BUFFER to retain successor VIEW exactly."
  (with-current-buffer buffer
    (should (equal (car view) (buffer-string)))
    (should (eq (nth 1 view) (current-local-map)))
    (should (equal (nth 2 view) header-line-format))))

(defun gnosis-test-session-owner--resume (mode mutation)
  "Resume MODE through an actual window hook applying MUTATION."
  (gnosis-test-study
    (let* ((id (gnosis-test--add-basic-thema "A" "A"))
           (summary (gnosis-test-study-summary (list id id) mode))
           (gnosis-review-buffer-name "*Session owner resume*")
           (before (gnosis-test-summary-owner-evidence))
           fired entered caught owner view)
      (let ((buffer-list-update-hook
             (list (lambda ()
                     (when (and (not fired) gnosis-review--running
                                (equal (buffer-name) gnosis-review-buffer-name))
                       (setq fired t owner (current-buffer)
                             view (gnosis-test-session-owner--replace owner mutation)))))))
        (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                   (lambda (&rest _) (setq entered t) "A"))
                  ((symbol-function 'read-char-choice) (lambda (&rest _) ?q)))
          (with-current-buffer summary
            (condition-case err (call-interactively (local-key-binding (kbd "r")))
              (user-error (setq caught err))))))
      (should fired)
      (if (eq mutation 'rename)
          (progn (should entered) (should-not caught)
                 (should (= 2 (length (gnosis-select '* (if (eq mode 'due)
                                                          'review-events 'practice-events)))))
                 (with-current-buffer gnosis-review-buffer-name
                   (should (equal "Foreign same-name draft" (buffer-string)))))
        (should caught)
        (should-not entered)
        (gnosis-test-session-owner--assert-view owner view)
        (should (equal before (gnosis-test-summary-owner-evidence)))))))

(ert-deftest gnosis-session-owner-resume-native-window-retirement ()
  (dolist (mode '(due practice))
    (dolist (mutation '(roundtrip setup detach))
      (gnosis-test-session-owner--resume mode mutation))))

(ert-deftest gnosis-session-owner-resume-native-window-rename ()
  (dolist (mode '(due practice))
    (gnosis-test-session-owner--resume mode 'rename)))

(defun gnosis-test-session-owner--navigation (mode operation occurrence)
  "Retire MODE's batch after OPERATION at OCCURRENCE, before later input."
  (gnosis-test-study
    (let* ((id (gnosis-test--add-basic-thema "A" "A"))
           (_seed (gnosis-test-study-summary (list id id) mode))
           (second (gnosis-test--add-basic-thema "B" "A"))
           (gnosis-review-buffer-name "*Session owner navigation*")
           (native (symbol-function operation))
           (calls 0) (inputs 0) fired before view owner caught)
      (cl-letf (((symbol-function operation)
                 (lambda (&rest args)
                   (prog1 (apply native args)
                     (when (and gnosis-review--running
                                (eq major-mode 'gnosis-mode)
                                (= (cl-incf calls) occurrence))
                       (setq fired t owner (current-buffer)
                             before (gnosis-test-summary-owner-evidence)
                             view (gnosis-test-session-owner--replace owner 'roundtrip))))))
                ((symbol-function 'gnosis--read-string-with-input-method)
                 (lambda (&rest _)
                   (when (> (cl-incf inputs) 3) (user-error "Unbounded stale session"))
                   "A"))
                ((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
        (condition-case err (gnosis-review-loop (list id second) mode)
          (user-error (setq caught err))))
      (should fired)
      (should caught)
      (should (= inputs (if (or (eq operation 'jump-to-register)
                                (= occurrence 3)) 1 0)))
      (gnosis-test-session-owner--assert-view owner view)
      (should (equal before (gnosis-test-summary-owner-evidence))))))

(ert-deftest gnosis-session-owner-start-next-and-register ()
  (dolist (mode '(due practice))
    (dolist (operation '(pop-to-buffer-same-window delete-other-windows))
      (dolist (occurrence '(1 2 3))
        (gnosis-test-session-owner--navigation mode operation occurrence)))
    (gnosis-test-session-owner--navigation mode 'jump-to-register 1)))

(defun gnosis-test-session-owner--link (mode boundary mutation)
  "Visit a real source in MODE; apply MUTATION at BOUNDARY."
  (gnosis-test-study
    (gnosis-test-active-owner--seed mode)
    (let* ((gnosis-nodes-dir gnosis-dir)
           (org-id-track-globally nil)
           (file (expand-file-name "source.org" gnosis-dir))
           (gnosis-review-buffer-name "*Session linked source*")
           (owner (gnosis-review--setup-buffer '(222) mode))
           before answer fired caught view (inputs 0) (prompts 0)
           (mutate (lambda ()
                     (unless fired
                       (setq fired t
                             view (gnosis-test-session-owner--replace owner mutation))))))
      (with-temp-file file
        (insert "#+title: Source\n* Source\n:PROPERTIES:\n:ID: source\n:END:\nPassage\n"))
      (gnosis-nodes-update-file file)
      (gnosis--insert-into 'thema-links '([222 "source"]))
      (switch-to-buffer owner)
      (gnosis-test-content--state mode)
      (setq answer (gnosis-test-content--answer "basic")
            before (gnosis-test-summary-owner-evidence))
      (let ((find-file-hook (when (eq boundary 'navigation) (list mutate)))
            (gnosis-link-view-mode-hook (when (eq boundary 'mode) (list mutate))))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (when (eq boundary 'completion) (funcall mutate))
                     (caar candidates)))
                  ((symbol-function 'read-char-choice)
                   (lambda (&rest _)
                     (cl-incf prompts) (if (= prompts 1) ?v ?n)))
                  ((symbol-function 'recursive-edit)
                   (lambda ()
                     (cl-incf inputs)
                     (when (eq boundary 'recursive) (funcall mutate))
                     (switch-to-buffer owner))))
          (condition-case err
              (gnosis-review-actions (car answer) 222 (cdr answer))
            (user-error (setq caught err)))))
      (should fired)
      (if (eq mutation 'rename)
          (progn (should-not caught) (should (= inputs 1)) (should (= prompts 2))
                 (should (= 2 (length (gnosis-select '* (if (eq mode 'due)
                                                          'review-events 'practice-events)))))
                 (with-current-buffer gnosis-review-buffer-name
                   (should (equal "Foreign same-name draft" (buffer-string)))))
        (should caught)
        (should (= inputs (if (eq boundary 'recursive) 1 0)))
        (should (= prompts 1))
        (gnosis-test-session-owner--assert-view owner view)
        (should (equal before (gnosis-test-summary-owner-evidence)))))))

(ert-deftest gnosis-session-owner-linked-source-no-stale-input ()
  (dolist (mode '(due practice))
    (dolist (boundary '(completion navigation mode recursive))
      (dolist (mutation '(detach roundtrip setup))
        (gnosis-test-session-owner--link mode boundary mutation)))))

(ert-deftest gnosis-session-owner-linked-source-rename-return ()
  (dolist (mode '(due practice))
    (dolist (boundary '(completion navigation mode recursive))
      (gnosis-test-session-owner--link mode boundary 'rename))))

(ert-deftest gnosis-session-owner-skip-refuses-retired-lifetime ()
  (dolist (mode '(due practice))
    (gnosis-test-study
      (let* ((id (gnosis-test--add-basic-thema "A" "A"))
             (_seed (gnosis-test-study-summary (list id id) mode))
             (owner (gnosis-test-study-state (list id) mode))
             (_suspend (gnosis-toggle-suspend-themata (list id) 1 t))
             (before (gnosis-test-summary-owner-evidence))
             (eligible (symbol-function 'gnosis-study-eligible-p))
             view)
        (with-current-buffer owner
          (cl-letf (((symbol-function 'gnosis-study-eligible-p)
                     (lambda (thema)
                       (prog1 (funcall eligible thema)
                         (setq view (gnosis-test-session-owner--replace owner 'roundtrip))))))
            (should-error (gnosis-review-session gnosis-review--state) :type 'user-error)))
        (gnosis-test-session-owner--assert-view owner view)
        (should (equal before (gnosis-test-summary-owner-evidence)))))))

(ert-deftest gnosis-session-owner-cleanup-keeps-successor-preparation ()
  (dolist (mode '(due practice))
    (gnosis-test-study
      (let* ((id (gnosis-test--add-basic-thema "A" "A"))
             (summary (gnosis-test-study-summary (list id id) mode))
             (gnosis-review-buffer-name "*Cleanup owner*")
             (before (gnosis-test-summary-owner-evidence))
             (cancel (symbol-function 'gnosis-review--lookahead-cancel))
             fired owner view)
        (let ((buffer-list-update-hook
               (list (lambda ()
                       (when (and (not fired) gnosis-review--running
                                  (equal (buffer-name) gnosis-review-buffer-name))
                         (setq fired t owner (current-buffer)
                               view (gnosis-test-session-owner--replace owner 'roundtrip))
                         (setq-local gnosis-review--lookahead '(:successor t)))))))
          (cl-letf (((symbol-function 'gnosis-review--lookahead-cancel)
                     (lambda ()
                       (should-not (and view (eq (current-buffer) owner)))
                       (funcall cancel))))
            (with-current-buffer summary
              (should-error (gnosis-review-resume) :type 'user-error))))
        (should fired)
        (gnosis-test-session-owner--assert-view owner view)
        (with-current-buffer owner
          (should (equal gnosis-review--lookahead '(:successor t)))
          (setq gnosis-review--lookahead nil))
        (should (equal before (gnosis-test-summary-owner-evidence)))))))

(ert-deftest gnosis-session-owner-navigation-cannot-adopt-foreign-buffer ()
  (dolist (mode '(due practice))
    (gnosis-test-study
      (let* ((id (gnosis-test--add-basic-thema "A" "A"))
             (summary (gnosis-test-study-summary (list id id) mode))
             (foreign (generate-new-buffer "*Foreign destination*"))
             (before (gnosis-test-summary-owner-evidence))
             (pop (symbol-function 'pop-to-buffer-same-window))
             view entered)
        (with-current-buffer foreign
          (gnosis-mode)
          (insert "Foreign draft")
          (setq view (list (buffer-string) (current-local-map) header-line-format)))
        (cl-letf (((symbol-function 'pop-to-buffer-same-window)
                   (lambda (&rest args)
                     (prog1 (apply pop args) (switch-to-buffer foreign))))
                  ((symbol-function 'gnosis--read-string-with-input-method)
                   (lambda (&rest _) (setq entered t) "A")))
          (with-current-buffer summary
            (should-error (gnosis-review-resume) :type 'user-error)))
        (should-not entered)
        (gnosis-test-session-owner--assert-view foreign view)
        (should (equal before (gnosis-test-summary-owner-evidence)))))))

(provide 'gnosis-test-review-session-owner)
;;; gnosis-test-review-session-owner.el ends here
