;;; gnosis-test-runner.el --- Runner contract tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:
;; Test library composition and Make phase ordering without real compilation.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst gnosis-test-runner--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Source tree whose runner is under test.")

(ert-deftest gnosis-test-runner-composable-libraries ()
  "Requiring ordinary suites must not run ERT or exit Emacs."
  (cl-letf (((symbol-function 'ert-run-tests-batch-and-exit)
             (lambda (&rest _) (ert-fail "Library attempted to run ERT")))
            ((symbol-function 'kill-emacs)
             (lambda (&rest _) (ert-fail "Library attempted to exit"))))
    (dolist (file (directory-files
                   (expand-file-name "tests" gnosis-test-runner--root)
                   nil "\\`gnosis-test-.*\\.el\\'"))
      (require (intern (file-name-base file)))))
  (should (ert-test-boundp 'gnosis-test-org-adjust-title-plain))
  (should (ert-test-boundp 'gnosis-test-detect-script-greek)))

(ert-deftest gnosis-test-runner-compile-before-tests ()
  "Even reverse scheduling must finish compilation before starting tests."
  (skip-unless (executable-find "make"))
  ;; GNU Make added deterministic shuffle probes in 4.4.
  (skip-unless (= 0 (call-process "make" nil nil nil
                                  "--shuffle=none" "--version")))
  (let ((directory (make-temp-file "gnosis-runner-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory directory)))
          (copy-file (expand-file-name "Makefile" gnosis-test-runner--root)
                     "Makefile")
          (with-temp-file "probe.mk"
            (insert "_autoload _autoload-smoke:\n\t@:\n"
                    "_compile:\n\t@printf 'compile\\n' >> trace; touch compiled\n"
                    "_test:\n\t@test -f compiled\n\t@printf 'test\\n' >> trace\n"))
          (with-temp-buffer
            ;; Reverse scheduling exposes the missing dependency without relying
            ;; on compilation duration or a probabilistic process race.
            (should (= 0 (call-process
                          "make" nil t nil "--no-print-directory" "-j4"
                          "--shuffle=reverse" "-f" "Makefile" "-f" "probe.mk"
                          "GNOSIS_ENV_WRAPPED=1" "JOBS=1" "_check"))))
          (should (equal (with-temp-buffer
                           (insert-file-contents "trace") (buffer-string))
                         "compile\ntest\n")))
      (delete-directory directory t))))

(provide 'gnosis-test-runner)
;;; gnosis-test-runner.el ends here
