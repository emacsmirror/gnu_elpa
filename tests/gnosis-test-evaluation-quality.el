;;; gnosis-test-evaluation-quality.el --- Cancellation boundaries -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Public response bindings with synchronous, delayed and reentrant cancellation.

;;; Code:
(require 'gnosis-test-agent-eval)
(require 'gnosis-test-review-quality)

(ert-deftest gnosis-test-evaluation-quality-cancel-buffer-and-retry ()
  "Cancelling may select another buffer, but only the response becomes editable."
  (dolist (mode '(due practice))
    (gnosis-test-eval--with-review mode
      (gnosis-test-review-quality--seed)
      (let* ((before (gnosis-test-content--evidence))
             (other (generate-new-buffer "*unrelated response draft*"))
             (literal "Full response α\nSecond line 🧠")
             (cancels 0) resolve stale
             (gnosis-agent-eval-function
              (lambda (_request yes _no)
                (setq resolve yes)
                (lambda ()
                  (cl-incf cancels)
                  (funcall yes '(:verdict fail :explanation "Reentrant stale"))
                  (set-buffer other)))))
        (unwind-protect
            (progn
              (with-current-buffer other (insert "Unrelated draft") (setq buffer-read-only t))
              (cl-letf (((symbol-function 'recursive-edit)
                         (lambda ()
                           (let ((response (current-buffer)))
                             (insert literal)
                             (gnosis-test-eval--key "C-c C-c")
                             (setq stale resolve)
                             (gnosis-test-eval--key "C-c C-k")
                             (should (eq response (current-buffer)))
                             (should-not buffer-read-only)
                             (should-not (plist-get gnosis-agent-eval--context :result))
                             (should (buffer-local-value 'buffer-read-only other))
                             (should (equal literal (buffer-string)))
                             (insert " retry")
                             (gnosis-test-eval--key "C-c C-c")
                             (funcall stale '(:verdict fail :explanation "Late stale"))
                             (should-not (plist-get gnosis-agent-eval--context :result))
                             (funcall resolve '(:verdict pass :explanation "Current"))
                             (should (equal (concat literal " retry") (buffer-string)))
                             (should (buffer-local-value 'buffer-read-only other))))))
                (should (car (gnosis-review-agent-eval id))))
              (should (= cancels 2))
              (should (equal "Unrelated draft" (with-current-buffer other (buffer-string))))
              (should (equal before (gnosis-test-content--evidence))))
          (kill-buffer other))))))

(ert-deftest gnosis-test-evaluation-quality-sync-delayed-cleanup-policy ()
  "Only cancellation thunk errors and quits are contained, exactly once."
  (dolist (mode '(due practice))
    (dolist (sync '(nil t))
      (dolist (condition '(error quit))
        (gnosis-test-eval--with-review mode
          (let ((cancels 0) resolve escaped
                (before (gnosis-test-eval--evidence)))
            (let ((gnosis-agent-eval-function
                   (lambda (_request yes _no)
                     (setq resolve yes)
                     (when sync (funcall yes '(:verdict pass :explanation "Provisional")))
                     (lambda () (cl-incf cancels) (signal condition '("Cleanup"))))))
              (cl-letf (((symbol-function 'recursive-edit)
                         (lambda ()
                           (insert "Response α\nExact second line")
                           (condition-case nil
                               (progn
                                 (gnosis-test-eval--key "C-c C-c")
                                 (unless sync (funcall resolve '(:verdict pass :explanation "Provisional"))))
                             (quit (setq escaped t)))
                           (should (equal (buffer-string) "Response α\nExact second line")))))
                (should (car (gnosis-review-agent-eval id)))))
            (should-not escaped)
            (should (= cancels 1))
            (should (equal before (gnosis-test-eval--evidence)))))))))

(provide 'gnosis-test-evaluation-quality)
;;; gnosis-test-evaluation-quality.el ends here
