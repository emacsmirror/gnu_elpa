;;; gnosis-tooling-runner.el --- Batch completion receipt -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:
;; A zero process exit is insufficient: write a receipt only after ERT returns.

;;; Code:

(require 'ert)

(defun gnosis-tooling-run-tests (&optional required)
  "Run ERT and write a completion receipt for the Make worker.
Require every test in REQUIRED to be present and pass without skipping."
  (dolist (name required)
    (unless (ert-test-boundp name)
      (error "Required integration test is missing: %s" name)))
  (let ((stats (ert-run-tests-batch t)))
    (unless (= (ert-stats-completed stats) (ert-stats-total stats))
      (error "ERT did not complete its selected tests"))
    (dolist (name required)
      (unless (ert-test-passed-p (ert-test-most-recent-result (ert-get-test name)))
        (error "Required integration test did not pass: %s" name)))
    (with-temp-file (getenv "GNOSIS_TEST_RECEIPT")
      (insert (format "completed %d\n" (ert-stats-total stats))))
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(provide 'gnosis-tooling-runner)
;;; gnosis-tooling-runner.el ends here
