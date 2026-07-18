;;; r-ts-setup.el --- Emacs tree-sitter setup from R package -*- lexical-binding: t; -*-
;; Copyright (C) 2026  Manuel Teodoro Tenango

;; Author: Manuel Teodoro <ttm@teoten.me>
;; URL: https://codeberg.org/R-for-emacs/r-ts-mode
;; Version: 1.1.1
;; Assisted-by: Sonet:4.6
;; Package-Requires: ((emacs "30.1"))
;; Created: June, 2026

;; License
;; R-TS-SETUP is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; R-TS-SETUP is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with R-TS-MODE. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Functionality to prepare the binaries for R-TS-MODE from the R
;; package "treesitter.r"

;;; Code:

;; This module is not automatically loaded, thus it has to be required

;;;; =========================================================================
;;;; Groups, Custom Variables, General Variables
;;;; =========================================================================

(defgroup r-ts-setup nil
  "R's roxygen support for `r-ts-mode'."
  :group 'r-ts-mode
  :version "30.1")

(defcustom r-ts-setup-r-program "R"
  "Program name or path for invoking R."
  :type '(choice string file)
  :group 'r-ts-setup)

(defcustom r-ts-setup-create-treesitter-dir t
  "When non-nil, automatically create `~/<user-emacs-directory>/tree-sitter/' if missing.
When nil, signal an error if the target directory does not exist."
  :type 'boolean
  :group 'r-ts-setup)


;;;; =========================================================================
;;;; Grammar / Binary Preparation — Pure Helpers
;;;; =========================================================================
(defun r-ts-setup--build-r-find-package-command (r-program)
  "Return the shell command that prints the path of the \='treesitter.r\=' R package.
R-PROGRAM is the executable name or path.  Pure function — no side effects."
  (if (string-match-p "\\.exe\\'" r-program)
      (format "%s --no-echo -q -e print(find.package('treesitter.r'))" r-program)
    (format "%s --no-echo -q -e 'print(find.package(\"treesitter.r\"))'" r-program)))

(defun r-ts-setup--parse-r-find-package-output (output)
  "Extract a file path from R's printed OUTPUT string.
OUTPUT is expected to contain a quoted path, e.g. [1] \"/some/path\".
Returns the path string, or signals an error if OUTPUT starts with \"Error\".
Pure function — no side effects."
  (when (string-match-p "\\`Error" output)
    (error "R signalled an error: %s" output))
  (if (string-match "\"\\([^\"]+\\)\"" output)
      (match-string 1 output)
    (error "Could not parse R output: %s" output)))

(defun r-ts-setup--find-treesitter-r-package-path ()
  "Run R to find the installed path of the \='treesitter.r\=' package.
Returns the path string.  Signals an error on failure."
  (let* ((cmd (r-ts-setup--build-r-find-package-command r-ts-setup-r-program))
         (output (progn
                   (shell-command cmd)
                   (with-current-buffer "*Shell Command Output*"
                     (buffer-substring-no-properties (point-min) (point-max))))))
    (kill-buffer "*Shell Command Output*")
    (r-ts-setup--parse-r-find-package-output output)))

;; Path construction and validation
(defun r-ts-setup--binary-path-unix (package-path)
  "Return the expected .so path for PACKAGE-PATH on Unix.  Pure."
  (format "%s/libs/treesitter.r.so" package-path))

(defun r-ts-setup--binary-path-win (package-path)
  "Return candidate .dll paths for PACKAGE-PATH on Windows as a list.  Pure."
  (let ((base (format "%s/libs/" package-path)))
    (list (format "%streesitter.r.dll" base)
          (format "%sx64/treesitter.r.dll" base))))

(defun r-ts-setup--validate-path-exists (path)
  "Return PATH if it exists on disk, otherwise signal an error."
  (if (file-exists-p path)
      path
    (error "File not found: %s" path)))

(defun r-ts-setup--resolve-binary-path-unix (package-path)
  "Return the validated .so path under PACKAGE-PATH, or signal an error."
  (r-ts-setup--validate-path-exists
   (r-ts-setup--binary-path-unix package-path)))

(defun r-ts-setup--resolve-binary-path-win (package-path)
  "Return the first existing .dll path under PACKAGE-PATH, or signal an error."
  (let ((found (seq-find #'file-exists-p
                         (r-ts-setup--binary-path-win package-path))))
    (or found
        (error "treesitter.r.dll not found under %s.  Please report this issue."
               package-path))))

(defun r-ts-setup--resolve-binary-path (package-path)
  "Return the validated grammar binary path under PACKAGE-PATH for this OS."
  (if (eq system-type 'windows-nt)
      (r-ts-setup--resolve-binary-path-win package-path)
    (r-ts-setup--resolve-binary-path-unix package-path)))

;; Directory preparation
(defun r-ts-setup--ensure-directory (path)
  "Ensure that PATH exists as a directory.
If missing, create it when `r-ts-setup-create-treesitter-dir' is non-nil,
otherwise signal an error."
  (let ((expanded (expand-file-name path)))
    (unless (file-exists-p expanded)
      (if r-ts-setup-create-treesitter-dir
          (make-directory expanded t)
        (error "Directory not found: %s" expanded)))))


;;;; =========================================================================
;;;; API
;;;; =========================================================================
;;;###autoload
(defun r-ts-setup-prepare-binaries-from-r-library (&optional package-path emacs-ts-path)
  "Copy the tree-sitter R grammar from the \='treesitter.r\=' R package to Emacs.
Searches for the package in PACKAGE-PATH (or auto-detects via R) and copies
the compiled binary to EMACS-TS-PATH (default: ~/<user-emacs-directory>/tree-sitter/)."
  (interactive)
  (let* ((binary-ext (if (eq system-type 'windows-nt) "dll" "so"))
         (ts-path (file-name-as-directory
                   (or emacs-ts-path
                       (file-name-as-directory (concat user-emacs-directory "tree-sitter")))))
         (pkg-path (or package-path (r-ts-setup--find-treesitter-r-package-path)))
         (binary-path (r-ts-setup--resolve-binary-path pkg-path)))
    (r-ts-setup-ensure-directory ts-path)
    (copy-file binary-path
               (format "%slibtree-sitter-r.%s" ts-path binary-ext)
               t)))


(provide 'r-ts-setup)
;;; r-ts-setup.el ends here
