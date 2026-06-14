;;; forgejo-test-load.el --- Tests for module loading  -*- lexical-binding: t; -*-

;;; Commentary:

;; Verify that each module can be loaded independently and that all
;; functions referenced via declare-function are actually available
;; after loading.  Catches missing require statements that cause
;; "Symbol's function definition is void" errors on first launch.

;;; Code:

(require 'ert)

(defvar forgejo-markdown-mode)
(setq forgejo-markdown-mode 'text-mode)

(defun forgejo-test-load--dir ()
  "Return the directory containing the Forgejo sources."
  (file-name-directory
   (or (locate-library "forgejo")
       (error "Cannot locate forgejo in load-path"))))

(defun forgejo-test-load--declared-functions (file)
  "Extract (SYMBOL . SOURCE) pairs from `declare-function' forms in FILE."
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
  "Return list of .el files in the Lisp directory.
Excludes generated files like autoloads and pkg descriptors."
  (cl-remove-if (lambda (f)
                  (let ((name (file-name-nondirectory f)))
                    (or (string-prefix-p "." name)
                        (string-match-p "-autoloads\\.el$" name)
                        (string-match-p "-pkg\\.el$" name))))
                (directory-files (forgejo-test-load--dir) t "\\.el$")))

(ert-deftest forgejo-test-load-all-modules ()
  "Loading all modules should not signal an error."
  (dolist (file (forgejo-test-load--module-files))
    (load file nil t)))

(ert-deftest forgejo-test-load-declared-functions-available ()
  "All `declare-function' symbols should be fboundp after loading all modules."
  (dolist (file (forgejo-test-load--module-files))
    (load file nil t))
  (dolist (file (forgejo-test-load--module-files))
    (dolist (pair (forgejo-test-load--declared-functions file))
      (should (fboundp (car pair))))))

(ert-deftest forgejo-test-load-isolated-require-chain ()
  "Each module's require chain should resolve without error.
Tests that loading a single module pulls in all its dependencies."
  (let ((modules '("forgejo" "forgejo-api" "forgejo-db" "forgejo-utils"
                   "forgejo-buffer" "forgejo-view" "forgejo-tl"
                   "forgejo-repo" "forgejo-issue" "forgejo-pull"
                   "forgejo-vc" "forgejo-watch")))
    (dolist (mod modules)
      ;; Unload all forgejo features to test in isolation
      (dolist (feat features)
        (when (and (string-prefix-p "forgejo" (symbol-name feat))
                   (not (string-prefix-p "forgejo-test" (symbol-name feat))))
          (unload-feature feat t)))
      (condition-case err
          (require (intern mod))
        (error
         (ert-fail (format "Failed to require %s: %S" mod err)))))))

(provide 'forgejo-test-load)
;;; forgejo-test-load.el ends here
