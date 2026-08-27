;;; gnosis-test-logical-day-cutover.el --- Logical-day cutover tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/gnosis
;; Version: 0.0.1

;;; Commentary:

;; Verify production uses the scheduler-neutral logical-day authority.

;;; Code:

(require 'ert)
(require 'gnosis-logical-day)
(require 'gnosis-db)

(defconst gnosis-test-logical-day-cutover--root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Gnosis source root used by cutover inventory tests.")

(ert-deftest gnosis-test-logical-day-cutover-option-alias ()
  "Keep the old day-boundary option as an alias to the neutral option."
  (should (eq (indirect-variable 'gnosis-algorithm-day-start-hour)
              'gnosis-day-start-hour))
  (let ((gnosis-algorithm-day-start-hour 6))
    (should (= gnosis-day-start-hour 6))))

(ert-deftest gnosis-test-logical-day-cutover-preload-option ()
  "Preserve an old option value set before the neutral module loads."
  (let ((emacs (expand-file-name invocation-name invocation-directory))
        (lisp (expand-file-name
               "lisp" gnosis-test-logical-day-cutover--root)))
    (with-temp-buffer
      (should
       (= 0
          (call-process
           emacs nil t nil "-Q" "--batch" "-L" lisp
           "--eval" "(setq load-prefer-newer t)"
           "--eval" "(setq gnosis-algorithm-day-start-hour 6)"
           "-l" "gnosis-logical-day"
           "--eval"
           "(kill-emacs (if (= gnosis-day-start-hour 6) 0 1))"))))))

(ert-deftest gnosis-test-logical-day-cutover-today-int ()
  "Derive integer review days only through the neutral date API."
  (cl-letf (((symbol-function 'gnosis-date)
             (lambda (&rest _) '(2025 6 15)))
            ((symbol-function 'gnosis-algorithm-date)
             (lambda (&rest _) (ert-fail "Legacy date authority called"))))
    (should (= 20250615 (gnosis--today-int)))))

(ert-deftest gnosis-test-logical-day-cutover-production-inventory ()
  "Reject old date API calls in production."
  (let ((regexp (rx symbol-start
                    "gnosis-algorithm-"
                    (or "date" "date-diff" "-date-later-p")
                    symbol-end)))
    (dolist (file (directory-files
                   (expand-file-name "lisp" gnosis-test-logical-day-cutover--root)
                   t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (should-not (re-search-forward regexp nil t))))))

(ert-deftest gnosis-test-legacy-scheduler-writer-inventory ()
  "Reject toy constants and legacy scheduler inserts in production."
  (let ((regexp
         (rx (or "gnosis-algorithm-gnosis-value"
                 "gnosis-algorithm-amnesia-value"
                 "(gnosis--insert-into 'review"
                 (seq "INSERT" (optional " OR IGNORE") " INTO review"
                      (optional "_log") (any " ("))))))
    (dolist (file (directory-files
                   (expand-file-name "lisp" gnosis-test-logical-day-cutover--root)
                   t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (should-not (re-search-forward regexp nil t))))))

(ert-deftest gnosis-test-logical-day-cutover-manual-option ()
  "Document the neutral day-boundary option without the old name."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "docs/gnosis.org"
                       gnosis-test-logical-day-cutover--root))
    (should (search-forward "=gnosis-day-start-hour=" nil t))
    (goto-char (point-min))
    (should (search-forward "* Logical Review Day" nil t))
    (goto-char (point-min))
    (should-not (search-forward "gnosis-algorithm-" nil t))
    (goto-char (point-min))
    (should (search-forward
             "By default, a new review day begins at 03:00." nil t))
    (goto-char (point-min))
    (should (search-forward "When set to 0, review days begin at midnight."
                            nil t))
    (goto-char (point-min))
    (should-not (search-forward "gnosis-algorithm-day-start-hour" nil t))))

(provide 'gnosis-test-logical-day-cutover)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-logical-day-cutover.el ends here
