;;; gnosis-test-edit-recovery.el --- Pending edit recovery -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Returning to the review must not strand input or accept a pending grade.

;;; Code:
(require 'ert)
(require 'gnosis-review-test-support)

(ert-deftest gnosis-edit-recovery-preserves-draft-and-restores-bindings ()
  (save-window-excursion
    (let ((origin (generate-new-buffer " *review recovery*"))
          (draft (generate-new-buffer " *review draft*")))
      (unwind-protect
          (progn
            (with-current-buffer draft
              (gnosis-edit-mode)
              (insert "Unsaved draft"))
            (with-current-buffer origin
              (gnosis-mode)
              (should (eq (key-binding "?") 'describe-mode)))
            (cl-letf (((symbol-function 'recursive-edit)
                       (lambda ()
                         (switch-to-buffer origin)
                         (cl-letf (((symbol-function 'recursion-depth) (lambda () 1)))
                           (dolist (key '("?" "h"))
                             (should (eq (key-binding key) 'gnosis-review-edit-pending-menu))
                             (call-interactively (key-binding key))
                             (should (keymap-popup--popup-buffer))
                             (keymap-popup-dismiss))
                           (call-interactively (key-binding "n"))
                           (should (eq (current-buffer) draft))
                           (should (equal (buffer-string) "Unsaved draft"))
                           (should (buffer-modified-p)))))
                      ((symbol-function 'gnosis-review--accept)
                       (lambda (&rest _) (ert-fail "Recovery accepted a grade"))))
              (gnosis-review--wait-for-edit origin draft))
            (with-current-buffer origin
              (should-not gnosis-review-edit-pending-mode)
              (should-not gnosis-review--pending-edit)
              (should (eq (key-binding "?") 'describe-mode))))
        (keymap-popup-dismiss)
        (kill-buffer origin)
        (kill-buffer draft)))))

(ert-deftest gnosis-edit-recovery-dead-draft-does-not-adopt-replacement ()
  (with-temp-buffer
    (gnosis-mode)
    (let* ((draft (generate-new-buffer " *dead draft*"))
           (gnosis-review--pending-edit
            (list :origin (current-buffer) :draft draft :depth 0))
           exited)
      (kill-buffer draft)
      (let ((replacement (generate-new-buffer " *dead draft*")))
        (unwind-protect
            (cl-letf (((symbol-function 'exit-recursive-edit) (lambda () (setq exited t))))
              (gnosis-review-finish-edit)
              (should exited)
              (should-not (eq (current-buffer) replacement))
              (setf (plist-get gnosis-review--pending-edit :depth) 1)
              (should-error (gnosis-review-finish-edit) :type 'user-error))
          (kill-buffer replacement))))))

(ert-deftest gnosis-edit-recovery-native-save-cancel-feedback ()
  (skip-unless (not noninteractive))
  (dolist (mode '(due practice))
    (dolist (finish '("C-c C-c" "C-c C-k"))
      (gnosis-test-with-db
        (gnosis-test-content--add "basic")
        (let* ((origin (gnosis-review--setup-buffer '(222) 'due))
               (before (gnosis-test-content--evidence))
               (pending nil)
               (returned nil))
          (unwind-protect
              (progn
                (switch-to-buffer origin)
                (gnosis-test-content--state mode)
                (setq before (gnosis-test-content--evidence)
                      pending (cdr (gnosis-test-content--answer "basic")))
                ;; A native command in the draft returns to its paused review.
                ;; Queue actual help/dismiss/reopen/Next/editor-finish keys.
                (let ((gnosis-edit-mode-hook
                       (list (lambda ()
                               (local-set-key
                                (kbd "<f12>")
                                (lambda ()
                                  (interactive)
                                  (gnosis-test-content--edit-field "Parathema" "Revised explanation")
                                  (switch-to-buffer origin))))))
                      (unread-command-events
                       (append (listify-key-sequence
                                (kbd (concat "<f12> ? C-g h n " finish))) nil)))
                  (setq returned (gnosis-review-action--edit t 222 pending)))
                (should (car returned))
                (should-not (get-buffer "*Gnosis Edit*"))
                (should (equal before (gnosis-test-content--evidence)))
                (switch-to-buffer origin)
                (should-not gnosis-review-edit-pending-mode)
                (let ((unread-command-events
                       (listify-key-sequence (kbd "C-g ? C-g h n"))))
                  (should (= ?n (gnosis-review--read-action
                                 (list :id 222 :success t :result (cdr returned)
                                       :alternate nil)))))
                (should (equal before (gnosis-test-content--evidence)))
                (let ((unread-command-events
                       (listify-key-sequence
                        (kbd (if (equal finish "C-c C-c") "n" "q")))))
                  (catch 'review-loop
                    (gnosis-review-actions t 222 (cdr returned))))
                (should-not (equal before (gnosis-test-content--evidence))))
            (keymap-popup-dismiss)
            (when (get-buffer "*Gnosis Edit*") (kill-buffer "*Gnosis Edit*"))
            (when (buffer-live-p origin) (kill-buffer origin))))))))

(ert-deftest gnosis-edit-recovery-retires-draft-occurrences ()
  (dolist (transition '(mode roundtrip file))
    (save-window-excursion
      (with-temp-buffer
        (gnosis-mode)
        (let ((origin (current-buffer))
              (draft (generate-new-buffer " *retired draft*"))
              exited)
          (unwind-protect
              (progn
                (with-current-buffer draft (gnosis-edit-mode) (insert "Keep me"))
                (cl-letf (((symbol-function 'recursive-edit)
                           (lambda ()
                             (with-current-buffer draft
                               (pcase transition
                                 ('mode (text-mode))
                                 ('roundtrip (text-mode) (gnosis-edit-mode))
                                 ('file
                                  (set-visited-file-name
                                   (expand-file-name "retired.org" temporary-file-directory) t)
                                  (set-visited-file-name nil t))))
                             (switch-to-buffer origin)
                             (cl-letf (((symbol-function 'recursion-depth) (lambda () 1))
                                       ((symbol-function 'exit-recursive-edit)
                                        (lambda () (setq exited t))))
                               (gnosis-review-finish-edit)
                               (should exited)
                               (should (eq (current-buffer) origin))))))
                  (gnosis-review--wait-for-edit origin draft))
                (with-current-buffer draft (should (equal (buffer-string) "Keep me"))))
            (kill-buffer draft)))))))

(ert-deftest gnosis-edit-recovery-enable-hook-cannot-start-retired-input ()
  (dolist (transition '(mode roundtrip file switch kill))
    (with-temp-buffer
      (gnosis-mode)
      (let* ((origin (current-buffer))
             (draft (generate-new-buffer " *enable draft*"))
             (gnosis-review-edit-pending-mode-hook
              (list (lambda ()
                      (when gnosis-review-edit-pending-mode
                        (pcase transition
                          ('mode (text-mode))
                          ('roundtrip (text-mode) (gnosis-mode))
                          ('file
                           (set-visited-file-name
                            (expand-file-name "origin.org" temporary-file-directory) t)
                           (set-visited-file-name nil t))
                          ('switch (set-buffer draft))
                          ('kill (kill-buffer origin)))))))
             entered)
        (unwind-protect
            (progn
              (with-current-buffer draft (gnosis-edit-mode))
              (cl-letf (((symbol-function 'recursive-edit) (lambda () (setq entered t))))
                (should-error (gnosis-review--wait-for-edit origin draft) :type 'user-error)
                (should-not entered)))
          (kill-buffer draft))))))

(ert-deftest gnosis-edit-recovery-cleanup-preserves-hook-successors ()
  (dolist (effect '(successor switch kill))
    (with-temp-buffer
      (gnosis-mode)
      (let* ((origin (current-buffer))
             (draft (generate-new-buffer " *cleanup draft*"))
             (successor (list :successor t))
             (gnosis-review-edit-pending-mode-hook
              (list (lambda ()
                      (unless gnosis-review-edit-pending-mode
                        (pcase effect
                          ('successor
                           (setq gnosis-review--pending-edit successor)
                           (gnosis-review-edit-pending-mode 1))
                          ('switch (set-buffer draft))
                          ('kill (kill-buffer origin))))))))
        (unwind-protect
            (progn
              (with-current-buffer draft (gnosis-edit-mode))
              (cl-letf (((symbol-function 'recursive-edit) #'ignore))
                (gnosis-review--wait-for-edit origin draft))
              (when (buffer-live-p origin)
                (with-current-buffer origin
                  (if (eq effect 'successor)
                      (progn (should (eq gnosis-review--pending-edit successor))
                             (should gnosis-review-edit-pending-mode))
                    (should-not gnosis-review--pending-edit))))
              (with-current-buffer draft (should-not gnosis-review--pending-edit)))
          (kill-buffer draft))))))

(ert-deftest gnosis-edit-recovery-popup-unwind-is-owned ()
  (dolist (condition '(error quit replacement))
    (save-window-excursion
      (with-temp-buffer
        (gnosis-mode)
        (let ((origin (current-buffer))
              (draft (generate-new-buffer " *popup draft*"))
              popup replacement caught)
          (unwind-protect
              (progn
                (with-current-buffer draft (gnosis-edit-mode))
                (cl-letf (((symbol-function 'recursive-edit)
                           (lambda ()
                             (switch-to-buffer origin)
                             (cl-letf (((symbol-function 'recursion-depth) (lambda () 1)))
                               (gnosis-review-edit-pending-menu))
                             (setq popup (keymap-popup--popup-buffer))
                             (should (buffer-live-p popup))
                             (when (eq condition 'replacement)
                               (keymap-popup-dismiss)
                               (keymap-popup gnosis-review-feedback-mode-map)
                               (setq replacement (keymap-popup--popup-buffer)))
                             (signal (if (eq condition 'quit) 'quit 'error) '("Injected")))))
                  (condition-case nil (gnosis-review--wait-for-edit origin draft)
                    ((error quit) (setq caught t))))
                (should caught)
                (should-not (eq popup (keymap-popup--popup-buffer)))
                (when replacement
                  (should (eq replacement (keymap-popup--popup-buffer))))
                (with-current-buffer origin
                  (should-not gnosis-review--pending-edit)
                  (should-not gnosis-review-edit-pending-mode)))
            (keymap-popup-dismiss)
            (kill-buffer draft)))))))

(ert-deftest gnosis-edit-recovery-native-unavailable-and-abort ()
  (skip-unless (not noninteractive))
  (dolist (mode '(due practice))
    (dolist (scenario '(dead mode roundtrip file abort))
      (gnosis-test-with-db
        (gnosis-test-content--add "basic")
        (let* ((origin (gnosis-review--setup-buffer '(222) mode))
               draft before pending returned aborted)
          (unwind-protect
              (progn
                (switch-to-buffer origin)
                (gnosis-test-content--state mode)
                (setq before (gnosis-test-content--evidence)
                      pending (cdr (gnosis-test-content--answer "basic")))
                (let ((gnosis-edit-mode-hook
                       (list
                        (lambda ()
                          (local-set-key
                           (kbd "<f12>")
                           (lambda ()
                             (interactive)
                             (should (> (recursion-depth) 0))
                             (setq draft (current-buffer))
                             (gnosis-test-content--edit-field "Parathema" "Unsaved explanation")
                             (pcase scenario
                               ('dead (set-buffer-modified-p nil) (kill-buffer draft))
                               ('mode (text-mode))
                               ('roundtrip (text-mode) (gnosis-edit-mode))
                               ('file
                                (set-visited-file-name (expand-file-name "other.org" gnosis-dir) t)
                                (set-visited-file-name nil t)))
                             (switch-to-buffer origin))))))
                      (unread-command-events
                       (listify-key-sequence
                        (kbd (concat "<f12> ? C-g h " (if (eq scenario 'abort) "q" "n"))))))
                  (condition-case nil
                      (setq returned (gnosis-review-action--edit t 222 pending))
                    (quit (setq aborted t))))
                (should (eq aborted (eq scenario 'abort)))
                (unless aborted (should (equal returned (cons t pending))))
                (should (equal before (gnosis-test-content--evidence)))
                (should (equal (gnosis-get 'parathema 'extras '(= id 222)) "Explanation"))
                (when (buffer-live-p draft)
                  (with-current-buffer draft
                    (should (string-match-p "Unsaved explanation" (buffer-string)))))
                (switch-to-buffer origin)
                (should-not gnosis-review--pending-edit)
                (should-not (keymap-popup--popup-buffer))
                ;; Recovery returns to the real feedback reader, not a grade.
                (unless aborted
                  (let ((unread-command-events (listify-key-sequence (kbd "C-g h n"))))
                    (should (= ?n (gnosis-review--read-action
                                   (list :id 222 :success t :result pending)))))
                  (should (equal before (gnosis-test-content--evidence)))))
            (keymap-popup-dismiss)
            (when (buffer-live-p draft) (kill-buffer draft))
            (when (buffer-live-p origin) (kill-buffer origin))))))))

(ert-deftest gnosis-edit-recovery-native-source-revisit ()
  (skip-unless (not noninteractive))
  (dolist (mode '(due practice))
    (dolist (scenario '(ordinary dead repurposed))
      (gnosis-test-with-db
        (let* ((gnosis-nodes-dir gnosis-dir)
               (file (expand-file-name "source.org" gnosis-dir))
               origin source pending before)
          (with-temp-file file
            (insert ":PROPERTIES:\n:ID: recovery-source\n:END:\n#+title: Recovery source\n\nSource prose.\n"))
          (gnosis-nodes-update-file file)
          (gnosis-add-thema-fields "basic" "Question" nil '("old")
                                   "[[id:recovery-source][Source]]" nil 0
                                   '("recovery-source") nil 222)
          (setq origin (gnosis-review--setup-buffer '(222) mode))
          (unwind-protect
              (progn
                (switch-to-buffer origin)
                (gnosis-test-content--state mode)
                (setq pending (cdr (gnosis-test-content--answer "basic"))
                      before (gnosis-test-content--evidence))
                (let ((gnosis-link-view-mode-hook
                       (list
                        (lambda ()
                          (when gnosis-link-view-mode
                            (local-set-key
                             (kbd "<f12>")
                             (lambda ()
                               (interactive)
                               (should (> (recursion-depth) 0))
                               (setq source (current-buffer))
                               (pcase scenario
                                 ('dead (kill-buffer source))
                                 ('repurposed (text-mode) (insert "Keep source draft")))
                               (switch-to-buffer origin)))))))
                      (unread-command-events
                       (listify-key-sequence (kbd "<f12> ? C-g h n"))))
                  (should (equal (gnosis-review-action--view-link t 222 pending)
                                 (cons t pending))))
                (should source)
                (should (equal before (gnosis-test-content--evidence)))
                (when (buffer-live-p source)
                  (with-current-buffer source
                    (should-not gnosis-link-view-mode)
                    (when (eq scenario 'repurposed)
                      (should (string-match-p "Keep source draft" (buffer-string))))))
                (switch-to-buffer origin)
                (should-not gnosis-review--pending-edit)
                (let ((unread-command-events (listify-key-sequence (kbd "C-g ? C-g h q"))))
                  (should (= ?q (gnosis-review--read-action
                                 (list :id 222 :success t :result pending)))))
                (should (equal before (gnosis-test-content--evidence))))
            (keymap-popup-dismiss)
            (when (buffer-live-p source)
              (with-current-buffer source (set-buffer-modified-p nil))
              (kill-buffer source))
            (when (buffer-live-p origin) (kill-buffer origin))))))))

(ert-deftest gnosis-edit-recovery-native-retired-review ()
  (skip-unless (not noninteractive))
  (dolist (action '(gnosis-review-action--edit gnosis-review-action--view-link))
    (dolist (transition '(mode file dead nested))
      (gnosis-test-with-db
        (let* ((gnosis-nodes-dir gnosis-dir)
               (file (expand-file-name "retired-source.org" gnosis-dir))
               origin destination before pending stopped nested-returned
               (prior-hook (default-value 'post-command-hook))
               (return
                (lambda ()
                  (interactive)
                  (should (> (recursion-depth) 0))
                  (setq destination (current-buffer))
                  (switch-to-buffer origin)
                  (pcase transition
                    ((or 'mode 'nested) (text-mode) (gnosis-mode))
                    ('file
                     (set-visited-file-name (expand-file-name "foreign.org" gnosis-dir) t)
                     (set-visited-file-name nil t))
                    ('dead (kill-buffer origin)))
                  (when (buffer-live-p origin)
                    (let ((inhibit-read-only t))
                      (erase-buffer) (insert "Successor review text")))
                  (when (eq transition 'nested)
                    (let ((unread-command-events (listify-key-sequence (kbd "C-M-c"))))
                      (recursive-edit)
                      (setq nested-returned t)))))
               (bind-return (lambda () (local-set-key (kbd "<f12>") return))))
          (with-temp-file file
            (insert ":PROPERTIES:\n:ID: retired-source\n:END:\n#+title: Retired source\n\nText.\n"))
          (gnosis-nodes-update-file file)
          (gnosis-add-thema-fields "basic" "Question" nil '("old")
                                   "[[id:retired-source][Source]]" nil 0
                                   '("retired-source") nil 222)
          (setq origin (gnosis-review--setup-buffer '(222) 'practice))
          (unwind-protect
              (progn
                (switch-to-buffer origin)
                (gnosis-test-content--state 'practice)
                (setq pending (cdr (gnosis-test-content--answer "basic"))
                      before (gnosis-test-content--evidence))
                (let ((gnosis-edit-mode-hook (list bind-return))
                      (gnosis-link-view-mode-hook (list bind-return))
                      (unread-command-events (listify-key-sequence (kbd "<f12> C-g"))))
                  (condition-case nil (funcall action t 222 pending)
                    ((error quit) (setq stopped t))))
                (should stopped)
                (should destination)
                (should (eq nested-returned (eq transition 'nested)))
                (should (equal prior-hook (default-value 'post-command-hook)))
                (should (equal before (gnosis-test-content--evidence)))
                (should (= (recursion-depth) 0))
                (when (buffer-live-p origin)
                  (with-current-buffer origin
                    (should (equal (buffer-string) "Successor review text")))))
            (keymap-popup-dismiss)
            (when (buffer-live-p destination) (kill-buffer destination))
            (when (buffer-live-p origin) (kill-buffer origin))))))))

(provide 'gnosis-test-edit-recovery)
;;; gnosis-test-edit-recovery.el ends here
