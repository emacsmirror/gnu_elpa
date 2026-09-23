;;; gnosis-test-tooling.el --- Make completion contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:
;; Exercise the public Make runner in disposable projects with real Emacs.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst gnosis-test-tooling--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Source tree whose Makefile is tested.")

(defun gnosis-test-tooling--run (fixtures jobs verify &optional wrapper required)
  "Run FIXTURES with JOBS workers, then call VERIFY with status and output.
Each fixture is a (NAME . BODY) pair.  WRAPPER optionally replaces Emacs.
REQUIRED names a test that must pass without skipping."
  (let ((directory (make-temp-file "gnosis-tooling-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory directory))
              (process-environment (copy-sequence process-environment)))
          ;; Do not inherit the outer Make selection or jobserver in this fixture.
          (dolist (name '("MAKEFLAGS" "MFLAGS" "MAKELEVEL")) (setenv name nil))
          (copy-file (expand-file-name "Makefile" gnosis-test-tooling--root)
                     "Makefile")
          (make-directory "tests")
          (make-directory "lisp")
          (let ((helpers (expand-file-name "tests/tooling" gnosis-test-tooling--root)))
            (when (file-directory-p helpers)
              (copy-directory helpers "tests/tooling")))
          (dolist (fixture fixtures)
            (with-temp-file (format "tests/%s.el" (car fixture))
              (insert "(require 'ert)\n" (cdr fixture)
                      (format "\n(provide '%s)\n" (car fixture)))))
          (when wrapper
            (with-temp-file "emacs-wrapper" (insert "#!/bin/sh\n" wrapper))
            (set-file-modes "emacs-wrapper" #o700))
          (with-temp-buffer
            (let ((status (call-process
                           "make" nil t nil "--no-print-directory"
                           "GNOSIS_ENV_WRAPPED=1" (format "JOBS=%s" jobs)
                           (concat "ERT_REQUIRED_TESTS=" required)
                           (concat "EMACS=" (if wrapper
                                               (expand-file-name "emacs-wrapper")
                                             (expand-file-name invocation-name
                                                               invocation-directory)))
                           "test")))
              (funcall verify status (buffer-string)))))
      (delete-directory directory t))))

(ert-deftest gnosis-test-tooling-completed-and-empty ()
  "Real completed tests and zero-test support libraries both succeed."
  (dolist (jobs '(1 2))
    (gnosis-test-tooling--run
     '((gnosis-test-positive . "(ert-deftest positive () (should t))")
       (gnosis-test-empty . "")) jobs
     (lambda (status output)
       (should (equal status 0))
       (should (string-match-p
                (regexp-quote
                 (concat "1 tests across 2 files: 1 passed, 0 skipped, "
                         "0 unexpected, 0 expected failures; 2 files OK, 0 failed"))
                output))
       (should-not (file-exists-p ".test-results"))))))

(ert-deftest gnosis-test-tooling-failed-and-interrupted ()
  "Assertion failures and early zero exits retain diagnostics, not OK stamps."
  (dolist (body '("(ert-deftest failure () (should nil))"
                  "(kill-emacs 0)"
                  "(ert-deftest a-exit () (kill-emacs 0))
                   (ert-deftest z-unreached () (with-temp-file \"late\"))"))
    (gnosis-test-tooling--run
     `((gnosis-test-fault . ,body)
       (gnosis-test-positive . "(ert-deftest positive () (should t))")) 2
     (lambda (status output)
       (should-not (equal status 0))
       (should
        (string-match-p
         (regexp-quote
          (if (string-match-p "should nil" body)
              (concat "2 tests across 2 files: 1 passed, 0 skipped, "
                      "1 unexpected, 0 expected failures; 1 files OK, 1 failed")
            (concat "1 tests across 2 files: 1 passed, 0 skipped, "
                    "0 unexpected, 0 expected failures; 1 files OK, 1 failed")))
         output))
       (should-not (file-exists-p "late"))
       (should (file-exists-p ".test-results/gnosis-test-fault.log"))
       (with-temp-buffer
         (insert-file-contents ".test-results/gnosis-test-fault.stamp")
         (should (looking-at "FAIL ")))))))

(ert-deftest gnosis-test-tooling-invalid-completion-receipt ()
  "Zero-exit workers with missing or malformed receipts cannot pass."
  (dolist (receipt '(nil "completed nope\n" "completed 1\n"
                        "completed 1 1 0 0 0\nextra\n"
                        "completed 1 1 0 0 0\nextra"
                        "completed 1 1 0 0 0"
                        "completed 1 1 0 0 0 extra\n"
                        "completed 1 0 0 0 0\n"
                        "completed 0 1 0 0 0\n"
                        "completed 1 -1 2 0 0\n"
                        "completed 1 01 0 0 0\n"
                        "completed 99999999999999999999 1 0 0 0\n"))
    (gnosis-test-tooling--run
     '((gnosis-test-fault . "")) 1
     (lambda (status output)
       (should-not (equal status 0))
       (should (string-match-p
                (regexp-quote
                 (concat "0 tests across 1 files: 0 passed, 0 skipped, "
                         "0 unexpected, 0 expected failures; 0 files OK, 1 failed"))
                output))
       (should (file-exists-p ".test-results/gnosis-test-fault.log")))
     (concat (when receipt
               (format "if test -n \"$GNOSIS_TEST_RECEIPT\"; then printf '%%s' '%s' > \"$GNOSIS_TEST_RECEIPT\"; fi\n"
                       receipt))
             "exit 0\n"))))

(ert-deftest gnosis-test-tooling-required-integration ()
  "Required integration cannot be absent, skipped, failed or interrupted."
  (dolist (body '("" "(ert-deftest integration () (ert-skip \"Unavailable\"))"
                  "(ert-deftest integration () (should nil))"
                  "(ert-deftest integration () (kill-emacs 0))"
                  "(ert-deftest integration () (should t))"))
    (gnosis-test-tooling--run
     `((gnosis-test-integration . ,body)) 1
     (lambda (status output)
       (if (string-match-p "should t" body)
           (progn
             (should (equal status 0))
             (should (string-match-p "passed +1/1 +integration" output)))
         (should-not (equal status 0))
         (when (string-match-p "ert-skip" body)
           (should (string-match-p
                    (regexp-quote
                     (concat "1 tests across 1 files: 0 passed, 1 skipped, "
                             "0 unexpected, 0 expected failures; "
                             "0 files OK, 1 failed")) output)))
         (should (file-exists-p ".test-results/gnosis-test-integration.log"))))
     nil "integration")))

(ert-deftest gnosis-test-tooling-result-counts ()
  "Report exercised and skipped tests separately without requiring capabilities."
  (dolist (case '(("(ert-deftest pass () (should t))" 1 0)
                  ("(ert-deftest skip () (ert-skip \"Optional capability\"))" 0 1)
                  ("(ert-deftest pass () (should t))
                    (ert-deftest skip () (ert-skip \"Optional capability\"))" 1 1)))
    (gnosis-test-tooling--run
     `((gnosis-test-results . ,(car case))) 1
     (lambda (status output)
       (let ((counts (format "%d passed, %d skipped, 0 unexpected, 0 expected failures"
                             (nth 1 case) (nth 2 case))))
         (should (equal status 0))
         (should (string-match-p
                  (regexp-quote
                   (format "%d tests across 1 files: %s; 1 files OK, 0 failed"
                           (+ (nth 1 case) (nth 2 case)) counts)) output))
         ;; Successful logs may go away: per-suite output retains skip counts.
         (should (string-match-p
                  (regexp-quote
                   (concat "tests/gnosis-test-results.el (" counts ")")) output))
         (should-not (file-exists-p ".test-results")))))))

(ert-deftest gnosis-test-tooling-mixed-suites ()
  "Aggregate distinct pass, skip and unexpected results from multiple suites."
  (gnosis-test-tooling--run
   '((gnosis-test-pass . "(ert-deftest pass () (should t))")
     (gnosis-test-skip . "(ert-deftest skip () (ert-skip \"Optional capability\"))")
     (gnosis-test-fail . "(ert-deftest fail () (should nil))")) 1
   (lambda (status output)
     (should-not (equal status 0))
     (should (string-match-p
              (regexp-quote
               (concat "3 tests across 3 files: 1 passed, 1 skipped, "
                       "1 unexpected, 0 expected failures; 2 files OK, 1 failed"))
              output)))))

(ert-deftest gnosis-test-tooling-expected-failures ()
  "Keep ERT expected failures distinct from passes, skips and unexpected passes."
  (dolist (pass '(nil t))
    (gnosis-test-tooling--run
     `((gnosis-test-expected .
        ,(format "(ert-deftest expected () :expected-result :failed (should %S))"
                 pass))) 1
     (lambda (status output)
       (should (eq (equal status 0) (not pass)))
       (should (string-match-p
                (regexp-quote
                 (format (concat "1 tests across 1 files: 0 passed, 0 skipped, "
                                 "%d unexpected, %d expected failures; "
                                 "%d files OK, %d failed")
                         (if pass 1 0) (if pass 0 1)
                         (if pass 0 1) (if pass 1 0))) output))))))

(ert-deftest gnosis-test-tooling-unexpected-zero-exit ()
  "Even a zero process exit cannot pass a receipt with unexpected results."
  (gnosis-test-tooling--run
   '((gnosis-test-fault . "")) 1
   (lambda (status output)
     (should-not (equal status 0))
     (should (string-match-p
              (regexp-quote
               (concat "1 tests across 1 files: 0 passed, 0 skipped, "
                       "1 unexpected, 0 expected failures; 0 files OK, 1 failed"))
              output)))
   (concat "if test -n \"$GNOSIS_TEST_RECEIPT\"; then "
           "printf 'completed 1 0 0 1 0\\n' > \"$GNOSIS_TEST_RECEIPT\"; fi\n"
           "exit 0\n")))

(provide 'gnosis-test-tooling)
;;; gnosis-test-tooling.el ends here
