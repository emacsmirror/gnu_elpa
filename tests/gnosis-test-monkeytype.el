;;; gnosis-test-monkeytype.el --- Typing exercise tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion must not supply characters during a copying exercise.

;;; Code:

(require 'ert)
(require 'gnosis-monkeytype)

(defvar gnosis-script-input-method-alist)
(defvar completion-preview-mode)
(defvar global-completion-preview-mode)
(declare-function global-completion-preview-mode "completion-preview" (&optional arg))

(ert-deftest gnosis-monkeytype-completion-is-buffer-local ()
  (let* ((calls 0)
         (capf (lambda ()
                 (setq calls (1+ calls))
                 (list (point-min) (point) '("alphabet"))))
         (completion-at-point-functions (list capf))
         (text-mode-hook
          (list (lambda ()
                  (setq-local completion-at-point-functions (list capf)))))
         (global-capfs (copy-sequence
                        (default-value 'completion-at-point-functions))))
    (with-temp-buffer
      (gnosis-monkeytype-mode)
      (insert "alpha")
      ;; Direct CAPF invocation as well as key dispatch must be inert.
      (completion-at-point)
      (should (equal (buffer-string) "alpha"))
      (should (= calls 0))
      (should-not completion-at-point-functions)
      (text-mode)
      (completion-at-point)
      (should (equal (buffer-string) "alphabet")))
    (with-temp-buffer
      (text-mode)
      (insert "alpha")
      (completion-at-point)
      (should (equal (buffer-string) "alphabet")))
    (should (= calls 2))
    (should (equal global-capfs
                   (default-value 'completion-at-point-functions)))))

(ert-deftest gnosis-monkeytype-completion-commands-are-inert ()
  (let ((global-map (copy-keymap global-map)))
    ;; Include user bindings, not just the stock M-/ and C-M-i.
    (dolist (command '(dabbrev-expand dabbrev-completion hippie-expand
                      completion-at-point complete-symbol))
      (keymap-set global-map "C-c /" command)
      (with-temp-buffer
        (gnosis-monkeytype-mode)
        (insert "alphabet alpha")
        (let ((before (buffer-string))
              (position (point)))
          (call-interactively (key-binding (kbd "C-c /")))
          (should (equal before (buffer-string)))
          (should (= position (point))))))
    (with-temp-buffer
      (gnosis-monkeytype-mode)
      (should (eq (key-binding (kbd "M-/")) #'ignore))
      (should (eq (key-binding (kbd "C-M-i")) #'ignore)))
    (with-temp-buffer
      (text-mode)
      (should (eq (key-binding (kbd "M-/")) #'dabbrev-expand))
      (should (eq (key-binding (kbd "C-M-i")) #'complete-symbol)))))

(ert-deftest gnosis-monkeytype-disables-global-completion-preview ()
  (skip-unless (require 'completion-preview nil t))
  (let ((was-enabled global-completion-preview-mode)
        ;; Simulate a text-mode hook supplying a completion backend.
        (text-mode-hook
         (list (lambda ()
                 (setq-local completion-at-point-functions
                             (list (lambda ()
                                     (list (point-min) (point)
                                           '("alphabet")))))))))
    (unwind-protect
        (progn
          (global-completion-preview-mode 1)
          (let ((hooks (copy-sequence after-change-major-mode-hook)))
            (with-temp-buffer
              (gnosis-monkeytype-mode)
              ;; The global frontend starts after ordinary mode hooks.
              (should-not completion-preview-mode)
              (should-not completion-at-point-functions)
              (should global-completion-preview-mode)
              (text-mode)
              (should completion-preview-mode))
            (with-temp-buffer
              (text-mode)
              (should completion-preview-mode))
            (should (equal hooks after-change-major-mode-hook))))
      (global-completion-preview-mode (if was-enabled 1 -1)))))

(ert-deftest gnosis-monkeytype-preserves-typing-input-method-and-exit ()
  (let ((gnosis-monkeytype-buffer-name " *gnosis-test-monkeytype*")
        (gnosis-script-input-method-alist '((greek . "greek")))
        (gnosis-monkeytype-string nil)
        (gnosis-monkeytype--start-time nil)
        method)
    (save-window-excursion
      (unwind-protect
          (cl-letf (((symbol-function 'recursive-edit)
                     (lambda ()
                       (setq method current-input-method)
                       (should (eq (key-binding (kbd "C-g")) #'keyboard-quit))
                       (should (eq (key-binding (kbd "RET")) #'forward-line))
                       (let ((this-command 'self-insert-command)
                             (last-command-event ?α))
                         (call-interactively (key-binding "α")))
                       (should (equal (buffer-string) "αβ"))
                       (should (= (point) 2))
                       (should (eq (get-text-property 1 'face)
                                   'gnosis-monkeytype-face-correct))
                       (call-interactively (key-binding (kbd "C-c C-k")))))
                    ((symbol-function 'exit-recursive-edit) #'ignore))
            (gnosis-monkeytype "αβ")
            (should (equal method "greek"))
            (should-not (get-buffer gnosis-monkeytype-buffer-name)))
        (when-let* ((buffer (get-buffer gnosis-monkeytype-buffer-name)))
          (kill-buffer buffer))))))

(defun gnosis-test-monkeytype--type (character)
  "Type CHARACTER through the exercise's self-insert binding."
  (let ((this-command 'self-insert-command)
        (last-command-event character))
    (call-interactively (key-binding (vector character)))))

(ert-deftest gnosis-monkeytype-completes-before-trailing-newlines ()
  (dolist (text '("abc" "abc\n" "abc\n\n" "αβ\n\n"))
    (let ((gnosis-monkeytype-buffer-name " *gnosis-test-monkeytype*")
          (gnosis-script-input-method-alist nil)
          (original (copy-sequence text))
          (completed 0)
          measured)
      (save-window-excursion
        (unwind-protect
            (cl-letf (((symbol-function 'recursive-edit)
                       (lambda ()
                         (should (equal text (buffer-string)))
                         (catch 'finished
                           (mapc #'gnosis-test-monkeytype--type
                                 (string-trim-right text "\n+")))))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda ()
                         (setq completed (1+ completed))
                         (throw 'finished t)))
                      ((symbol-function 'gnosis-monkeytype--calculate-wpm)
                       (lambda (target _start)
                         (setq measured target)
                         (should (equal text (buffer-string)))
                         (should (eq (get-text-property (1- (point)) 'face)
                                     'gnosis-monkeytype-face-correct)))))
              (gnosis-monkeytype text)
              (should (= completed 1))
              (should (equal measured text))
              (should (equal text original))
              (should-not (get-buffer gnosis-monkeytype-buffer-name)))
          (when-let* ((buffer (get-buffer gnosis-monkeytype-buffer-name)))
            (kill-buffer buffer)))))))

(ert-deftest gnosis-monkeytype-keeps-internal-blank-lines-and-rejects-mistakes ()
  (let ((gnosis-monkeytype-buffer-name " *gnosis-test-monkeytype*")
        (gnosis-script-input-method-alist nil)
        completed)
    (save-window-excursion
      (unwind-protect
          (cl-letf (((symbol-function 'recursive-edit)
                     (lambda ()
                       (catch 'finished
                         (mapc #'gnosis-test-monkeytype--type "ab")
                         ;; Advance one line as before, not over internal blanks.
                         (should (= (point) 4))
                         (should-not completed)
                         (call-interactively (key-binding (kbd "RET")))
                         (should (= (point) 5))
                         (gnosis-test-monkeytype--type ?c)
                         (gnosis-test-monkeytype--type ?x)
                         (should (= (point) 6))
                         (should-not completed)
                         (should (equal (buffer-string) "ab\n\ncd\n\n"))
                         (gnosis-test-monkeytype--type ?d))))
                    ((symbol-function 'exit-recursive-edit)
                     (lambda () (setq completed t) (throw 'finished t))))
            (gnosis-monkeytype "ab\n\ncd\n\n")
            (should completed)
            (should-not (get-buffer gnosis-monkeytype-buffer-name)))
        (when-let* ((buffer (get-buffer gnosis-monkeytype-buffer-name)))
          (kill-buffer buffer))))))

(ert-deftest gnosis-monkeytype-empty-targets-remain-cancellable ()
  ;; No new typing or automatic-completion policy for empty exercises.
  (dolist (text '("" "\n" "\n\n"))
    (let ((gnosis-monkeytype-buffer-name " *gnosis-test-monkeytype*")
          (gnosis-script-input-method-alist nil)
          entered)
      (save-window-excursion
        (unwind-protect
            (cl-letf (((symbol-function 'recursive-edit)
                       (lambda ()
                         (setq entered t)
                         (should (equal text (buffer-string)))
                         (call-interactively (key-binding (kbd "RET")))
                         (call-interactively (key-binding (kbd "C-c C-k")))))
                      ((symbol-function 'gnosis-monkeytype--calculate-wpm)
                       (lambda (&rest _) (ert-fail "Empty target was scored"))))
              (should (catch 'monkeytype-loop (gnosis-monkeytype text)))
              (should entered)
              (should-not (get-buffer gnosis-monkeytype-buffer-name)))
          (when-let* ((buffer (get-buffer gnosis-monkeytype-buffer-name)))
            (kill-buffer buffer)))))))

(provide 'gnosis-test-monkeytype)
;;; gnosis-test-monkeytype.el ends here
