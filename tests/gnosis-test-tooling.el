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
       (should (string-match-p "1 tests across 2 files: 2 passed, 0 failed" output))
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
       (should (string-match-p "2 files: 1 passed, 1 failed" output))
       (should-not (file-exists-p "late"))
       (should (file-exists-p ".test-results/gnosis-test-fault.log"))
       (with-temp-buffer
         (insert-file-contents ".test-results/gnosis-test-fault.stamp")
         (should (looking-at "FAIL ")))))))

(ert-deftest gnosis-test-tooling-invalid-completion-receipt ()
  "Zero-exit workers with missing or malformed receipts cannot pass."
  (dolist (receipt '(nil "completed nope\n" "completed 1\nextra\n"
                        "completed 1\nextra"))
    (gnosis-test-tooling--run
     '((gnosis-test-fault . "")) 1
     (lambda (status output)
       (should-not (equal status 0))
       (should (string-match-p "1 files: 0 passed, 1 failed" output))
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
         (should (file-exists-p ".test-results/gnosis-test-integration.log"))))
     nil "integration")))

(provide 'gnosis-test-tooling)
;;; gnosis-test-tooling.el ends here
