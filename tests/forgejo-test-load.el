;;; forgejo-test-load.el --- Tests for module loading  -*- lexical-binding: t; -*-

;;; Commentary:

;; Verify that each module can be loaded independently and that all
;; functions referenced via declare-function are actually available
;; after loading.  Catches missing require statements that cause
;; "Symbol's function definition is void" errors on first launch.

;;; Code:

(require 'ert)

(defvar forgejo-test-load--dir
  (expand-file-name "../lisp/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the lisp/ directory.")

(defun forgejo-test-load--declared-functions (file)
  "Extract (SYMBOL . SOURCE) pairs from declare-function forms in FILE."
  (let ((results nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward
              "(declare-function \\(\\S-+\\) \"\\(\\S-+\\)\"" nil t)
        (push (cons (intern (match-string 1))
                    (replace-regexp-in-string "\\.el$" "" (match-string 2)))
              results)))
    (nreverse results)))

(defun forgejo-test-load--module-files ()
  "Return list of .el files in the lisp directory.
Excludes generated files like autoloads and pkg descriptors."
  (cl-remove-if (lambda (f)
                  (let ((name (file-name-nondirectory f)))
                    (or (string-prefix-p "." name)
                        (string-match-p "-autoloads\\.el$" name)
                        (string-match-p "-pkg\\.el$" name))))
                (directory-files forgejo-test-load--dir t "\\.el$")))

(ert-deftest forgejo-test-load-all-modules ()
  "Loading all modules should not signal an error."
  (let ((load-path (cons forgejo-test-load--dir load-path)))
    (dolist (file (forgejo-test-load--module-files))
      (load file nil t))))

(ert-deftest forgejo-test-load-declared-functions-available ()
  "All declare-function symbols should be fboundp after loading all modules."
  (let ((load-path (cons forgejo-test-load--dir load-path)))
    ;; Load all modules first
    (dolist (file (forgejo-test-load--module-files))
      (load file nil t))
    ;; Check every declared function is available
    (dolist (file (forgejo-test-load--module-files))
      (dolist (pair (forgejo-test-load--declared-functions file))
        (let ((sym (car pair))
              (source (cdr pair)))
          (should (fboundp sym)))))))

(ert-deftest forgejo-test-load-isolated-require-chain ()
  "Each module's require chain should resolve without error.
Tests that loading a single module pulls in all its dependencies."
  (let ((modules '("forgejo" "forgejo-api" "forgejo-db" "forgejo-utils"
                    "forgejo-buffer" "forgejo-view" "forgejo-tl"
                    "forgejo-repo" "forgejo-issue" "forgejo-pull"
                    "forgejo-vc" "forgejo-watch")))
    (dolist (mod modules)
      (let ((load-path (cons forgejo-test-load--dir load-path)))
        ;; Unload all forgejo features to test in isolation
        (dolist (feat features)
          (when (and (string-prefix-p "forgejo" (symbol-name feat))
                     (not (string-prefix-p "forgejo-test" (symbol-name feat))))
            (unload-feature feat t)))
        (condition-case err
            (require (intern mod))
          (error
           (ert-fail (format "Failed to require %s: %S" mod err))))))))

(provide 'forgejo-test-load)
;;; forgejo-test-load.el ends here
