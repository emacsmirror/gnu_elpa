;;; keymap-popup-tooling-tests.el --- Tooling regressions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; This file is part of keymap-popup, distributed under the GNU GPL v3 or later.

;;; Commentary:

;; Exercise the Make recipes in disposable source fixtures, without Nix recursion.

;;; Code:

(require 'ert)

(defconst keymap-popup-tooling-test--root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the source tree whose Makefile is under test.")

(defun keymap-popup-tooling-test--make (target setup)
  "Run Make TARGET after SETUP prepares a disposable source fixture.
Call SETUP with the fixture as `default-directory'.
Return the exit status and captured output as a cons cell."
  (let ((directory (make-temp-file "keymap-popup-tooling-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory directory))
              (process-environment (copy-sequence process-environment)))
          ;; The caller may have selected this suite through Make variables.
          (dolist (name '("MAKEFLAGS" "MFLAGS" "MAKEOVERRIDES"))
            (setenv name nil))
          (dolist (name '("Makefile" "keymap-popup.el"))
            (with-temp-file name
              (insert-file-contents
               (expand-file-name name keymap-popup-tooling-test--root))))
          (funcall setup)
          (with-temp-buffer
            (cons (process-file
                   "make" nil (list (current-buffer) t) nil
                   "--no-print-directory" "USE_NIX=0"
                   (concat "EMACS_CMD="
                           (expand-file-name invocation-name invocation-directory))
                   target)
                  (buffer-string))))
      (delete-directory directory t))))

(ert-deftest keymap-popup-tooling-test-clean-lint ()
  (let ((result (keymap-popup-tooling-test--make "lint" #'ignore)))
    (should (equal (car result) 0))
    (should (string-match-p "Running package-lint" (cdr result)))))

(ert-deftest keymap-popup-tooling-test-checkdoc-fails-lint ()
  (let ((result
         (keymap-popup-tooling-test--make
          "lint"
          (lambda ()
            (with-temp-buffer
              (insert-file-contents "keymap-popup.el")
              (goto-char (point-min))
              (search-forward "Get popup metadata PROP from KEYMAP via pseudo-key lookup.")
              (replace-match "Get popup metadata." t t)
              (write-region (point-min) (point-max) "keymap-popup.el" nil 'silent))))))
    (should (equal (car result) 2))
    (should (string-match-p "Argument .*keymap.* should appear" (cdr result)))
    (should-not (string-match-p "Running package-lint" (cdr result)))))

(ert-deftest keymap-popup-tooling-test-package-lint-still-required ()
  (let ((result
         (keymap-popup-tooling-test--make
          "lint"
          (lambda ()
            (with-temp-buffer
              (insert-file-contents "keymap-popup.el")
              (goto-char (point-min))
              (search-forward "(provide 'keymap-popup)")
              (beginning-of-line)
              (insert "(defvar tooling-wrong-prefix nil \"A tooling test variable.\")\n\n")
              (write-region (point-min) (point-max) "keymap-popup.el" nil 'silent))))))
    (should (equal (car result) 2))
    (should (string-match-p "Running package-lint" (cdr result)))
    (should (string-match-p "tooling-wrong-prefix" (cdr result)))))

(ert-deftest keymap-popup-tooling-test-discovers-and-loads-each-suite ()
  (let ((result
         (keymap-popup-tooling-test--make
          "test"
          (lambda ()
            (make-directory "tests")
            (with-temp-file "tests/first-tests.el"
              (insert "(ert-deftest tooling-first-suite () (should t))\n"))
            (with-temp-file "tests/second-tests.el"
              (insert "(ert-deftest tooling-second-suite () (should nil))\n"))))))
    (should (equal (car result) 2))
    (should (string-match-p "passed.*tooling-first-suite" (cdr result)))
    (should (string-match-p "FAILED.*tooling-second-suite" (cdr result)))))

(provide 'keymap-popup-tooling-tests)
;;; keymap-popup-tooling-tests.el ends here
