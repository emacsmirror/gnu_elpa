;;; keymap-popup-native-tests.el --- Native reader tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Run `make test-native' in a disposable terminal Emacs.  Batch Emacs reads
;; minibuffer input from stdin, so it cannot exercise this command-loop path.
;; Mouse positions come from redisplay; no reader or mouse command is mocked.

;;; Code:

(require 'ert)
(require 'keymap-popup)

(defun keymap-popup-native-test--reader (target persistent nested abort)
  "Exercise a reader with TARGET clicks, PERSISTENT, NESTED and ABORT."
  (let ((overriding-terminal-local-map nil)
        ;; Isolate native startup hooks (including tooltip mouse handling).
        (pre-command-hook nil)
        (post-command-hook nil)
        (minibuffer-setup-hook nil)
        (minibuffer-exit-hook nil)
        (unread-command-events nil)
        (original-global-map (current-global-map))
        (global-map (copy-keymap (current-global-map)))
        (keymap-popup-persistent persistent)
        (keymap-popup-backend #'keymap-popup-backend-side-window)
        (root (make-sparse-keymap))
        (child (make-sparse-keymap))
        (outside (generate-new-buffer " *popup-outside*"))
        errors answer aborted observation outside-selected action-buffer)
    (let ((command-error-function
           (lambda (err &rest _) (push err errors))))
      (save-window-excursion
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (delete-other-windows)
          (buffer-enable-undo)
          (insert "Draft")
          (undo-boundary)
          (let* ((source (current-buffer))
                 (source-window (selected-window))
                 (undo (copy-tree buffer-undo-list))
                 (outside-window (split-window-right))
                 (action (lambda ()
                           (interactive)
                           (setq action-buffer (current-buffer))
                           (insert "!")))
                 (prompt (lambda ()
                           (interactive)
                           (should (eq (current-buffer) source))
                           (condition-case nil
                               (setq answer (read-string "Value: "))
                             (quit (setq aborted t))))))
            (set-window-buffer outside-window outside)
            (with-current-buffer outside (insert "Outside"))
            (keymap-set global-map "<f11>"
                        (lambda ()
                          (interactive)
                          (setq outside-selected
                                (eq (selected-window) outside-window))))
            (keymap-set global-map "<f12>"
                        (lambda ()
                          (interactive)
                          (setq observation
                                (list (eq (selected-window)
                                          (active-minibuffer-window))
                                      (keymap-popup--session-get
                                       (get-buffer keymap-popup--buffer-name)
                                       :suspended-depth)
                                      overriding-terminal-local-map
                                      (with-current-buffer
                                          (window-buffer (active-minibuffer-window))
                                        (minibuffer-contents-no-properties))))
                          ;; Recover a stolen selection only after typing, so
                          ;; a failure still terminates and reports lost input.
                          (select-window (active-minibuffer-window))))
            (dolist (map (list root child))
              (keymap-set map "p" prompt)
              (keymap-set map "a" action)
              (keymap-set map "<f10>" #'exit-recursive-edit)
              (keymap-popup-attach
               map `("p" ("Prompt" ,prompt :stay-open t)
                     "a" ("Action" ,action :stay-open t)
                     "<f10>" ("Finish test" exit-recursive-edit :stay-open t))))
            (when nested
              (keymap-set root "s" child)
              (keymap-popup-attach root `("s" ("Child" :keymap ,child))))
            (unwind-protect
                (progn
                  (use-global-map global-map)
                  (keymap-popup root)
                  (when nested (execute-kbd-macro (kbd "s")))
                  (let* ((buf (get-buffer keymap-popup--buffer-name))
                         (popup-window (get-buffer-window buf))
                         (stack (keymap-popup--session-get buf :stack))
                         (minibuffer-setup-hook
                          (append
                           minibuffer-setup-hook
                           (list
                            (lambda ()
                              (redisplay t)
                              (let ((popup-pos (posn-at-point 1 popup-window))
                                    (reader-pos (posn-at-point (point)))
                                    (outside-pos
                                     (posn-at-point 1 outside-window)))
                                (should popup-pos)
                                (should reader-pos)
                                (should outside-pos)
                                (setq unread-command-events
                                      (append
                                       (pcase target
                                         ('popup
                                          (list (list 'down-mouse-1 popup-pos)
                                                (list 'mouse-1 popup-pos)))
                                         ('reader (list (list 'mouse-1 reader-pos)))
                                         ('outside
                                          (list (list 'mouse-1 outside-pos)
                                                'f11
                                                (list 'mouse-1 reader-pos))))
                                       (list ?x 'f12
                                             (if abort ?\C-g ?\r) 'f10)))))))))
                    (setq unread-command-events (list ?p))
                    (recursive-edit)
                    (should-not errors)
                    (should (equal observation '(t 1 nil "x")))
                    (if abort
                        (progn (should aborted) (should-not answer))
                      (should-not aborted)
                      (should (equal answer "x")))
                    (when (eq target 'outside) (should outside-selected))
                    (should (equal (with-current-buffer outside (buffer-string))
                                   "Outside"))
                    (should (eq (current-buffer) source))
                    (should (eq (selected-window) source-window))
                    (should (equal (buffer-string) "Draft"))
                    (should (= (point) 6))
                    (should (equal buffer-undo-list undo))
                    (should (eq (keymap-popup--session-get buf :source) source))
                    (should (eq (keymap-popup--session-get buf :stack) stack))
                    (should (eq (keymap-popup--active-get buf :keymap)
                                (if nested child root)))
                    (should-not (keymap-popup--session-get buf :suspended-depth))
                    (execute-kbd-macro (kbd "a"))
                    (should (eq action-buffer source))
                    (should (equal (buffer-string) "Draft!"))
                    (when nested
                      (execute-kbd-macro (kbd "q"))
                      (should (eq (keymap-popup--active-get buf :keymap) root)))
                    (execute-kbd-macro (kbd "q"))
                    (should-not (buffer-live-p buf))
                    (should-not overriding-terminal-local-map)
                    (should-not pre-command-hook)
                    (should-not post-command-hook)
                    ;; This let owns the extra setup hook, not the popup.
                    (should-not (memq #'keymap-popup--suspend
                                      minibuffer-setup-hook))
                    (should-not minibuffer-exit-hook)
                    (should (zerop (minibuffer-depth)))))
              (keymap-popup-dismiss)
              (use-global-map original-global-map)
              (kill-buffer outside))))))))

(ert-deftest keymap-popup-native-test-reader-mouse ()
  "Popup clicks preserve native readers; other targets retain native dispatch."
  (skip-unless (not noninteractive))
  (dolist (target '(none popup reader outside))
    (dolist (persistent '(nil t))
      (dolist (nested '(nil t))
        (dolist (abort '(nil t))
          (ert-info ((format "target=%S persistent=%S nested=%S abort=%S"
                             target persistent nested abort))
            (keymap-popup-native-test--reader target persistent nested abort)))))))

(defun keymap-popup-native-test-run ()
  "Run native ERT tests and exit the disposable Emacs with their status."
  (let ((timeout (run-at-time
                  30 nil (lambda ()
                           (message "Native tests timed out")
                           (kill-emacs 2)))))
    (unwind-protect
        (let* ((stats (ert-run-tests-batch "keymap-popup-native-test-"))
               (passed (and (zerop (ert-stats-completed-unexpected stats))
                            (zerop (ert-stats-skipped stats))
                            (= (ert-stats-completed-expected stats)
                               (ert-stats-total stats)))))
          (princ (with-current-buffer "*Messages*" (buffer-string))
                 #'external-debugging-output)
          (kill-emacs (if passed 0 1)))
      (cancel-timer timeout)
      (kill-emacs 2))))

(provide 'keymap-popup-native-tests)
;;; keymap-popup-native-tests.el ends here
