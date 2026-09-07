;;; r-ts-roxygen.el --- Roxygen functionality for R tree-sitter mode -*- lexical-binding: t; -*-
;; Copyright (C) 2026  Manuel Teodoro Tenango

;; Author: Manuel Teodoro <ttm@teoten.me>
;; URL: https://codeberg.org/R-for-emacs/r-ts-mode
;; Version: 1.1.3
;; Package-Requires: ((emacs "30.1"))
;; Created: June, 2026

;; License
;; R-TS-ROXYGEN is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; R-TS-ROXYGEN is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with R-TS-MODE. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Provides syntax highlighting for Roxygen text within `r-ts-mode'.  This module
;; is not loaded by default.  Thus, it should be explicitly called after
;; `r-ts-mode' has been loaded (i.e., with `require').

;;; Code:

;;;; Groups, Custom Variables, General Variables
;; =============================================================================
(defgroup r-ts-roxygen nil
  "R's roxygen support for `r-ts-mode'."
  :group 'r-ts-mode
  :version "30.1")

(defcustom r-ts-roxygen-tags-param
  '("author" "aliases" "concept" "details"
    "example" "examples" "examplesIf"
    "format" "keywords"
    "method" "exportMethod"
    "name" "note" "param"
    "include" "references" "return" "returns"
    "seealso" "source" "docType"
    "title" "TODO" "usage" "import"
    "exportClass" "exportPattern"
    "exportS3Method" "S3method"
    "inherit" "inheritParams" "inheritSection"
    "importFrom" "importClassesFrom"
    "importMethodsFrom" "useDynLib"
    "rawNamespace"
    "rdname" "section" "slot" "description"
    "md" "eval" "evalNamespace" "family")
  "Roxygen tags that require a parameter.
Used to decide highlighting and tag completion."
  :group 'r-ts-roxygen
  :type '(repeat string))

(defcustom r-ts-roxygen-tags-noparam '("export" "noRd")
  "Roxygen tags that can be used without a parameter.
Used to decide highlighting and tag completion."
  :group 'r-ts-roxygen
  :type '(repeat string))

(defconst r-ts-roxygen--initial-regex "^[ \t]*#+'"
  "Regexp matching the start of a roxygen comment line.")

(defconst r-ts-roxygen--param-name-regexp
  "\\(?:\\(?:\\sw\\|\\s_\\)+,?\\)+"
  "Regexp matching a parameter name, including symbols and commas.")


;;;; Roxygen Supportive Functions
;; =============================================================================
(defun r-ts-roxygen--build-keywords ()
  "Return a font-lock keyword list for roxygen comments.
Pure function — reads only `defcustom' values, produces no side effects."
  `(;; Highlight entire roxygen lines
    (,(concat r-ts-roxygen--initial-regex ".*")
     (0 'font-lock-doc-face prepend))
    ;; Tags that take a parameter
    (,(concat r-ts-roxygen--initial-regex " *\\([@\\]"
              (regexp-opt r-ts-roxygen-tags-param t)
              "\\)\\>")
     (1 'font-lock-keyword-face prepend))
    ;; @param / @importFrom / etc. — highlight the argument name too
    (,(concat r-ts-roxygen--initial-regex " *\\(@"
              (regexp-opt '("param" "importFrom" "importClassesFrom"
                            "importMethodsFrom" "describeIn")
                          'words)
              "\\)\\(?:[ \t]+\\(" r-ts-roxygen--param-name-regexp "\\)\\)")
     (1 'font-lock-keyword-face prepend)
     (3 'font-lock-variable-name-face prepend))
    ;; Tags that take no parameter
    (,(concat "[@\\]" (regexp-opt r-ts-roxygen-tags-noparam t) "\\>")
     (0 'font-lock-variable-name-face prepend))
    ;; Bold the #' prefix itself
    (,(concat r-ts-roxygen--initial-regex)
     (0 'bold prepend))))

(defvar-local r-ts-roxygen--active-keywords nil
  "Font-lock keywords currently installed by `r-ts-roxygen-mode'.")

;; Does not work with ESS active
(defun r-ts-roxygen-complete-tag ()
  "Auto completion for Roxygen tags."
  (let* ((boundaries (bounds-of-thing-at-point 'symbol))
         (beg (car boundaries))
         (end (cdr boundaries)))
    (when (and boundaries
               (save-excursion
                 (goto-char beg)
                 (eq (following-char) ?@)))
      (list (1+ beg)
            end
            (append r-ts-roxygen-tags-param
                    r-ts-roxygen-tags-noparam)
            :exclusive 'no))))

(defun r-ts-roxygen--enable ()
  "Install roxygen font-lock keywords and completion in the current buffer."
  (setq r-ts-roxygen--active-keywords
        (r-ts-roxygen--build-keywords))
  (font-lock-add-keywords nil r-ts-roxygen--active-keywords)
  (add-hook 'completion-at-point-functions
            #'r-ts-roxygen-complete-tag nil t))

(defun r-ts-roxygen--disable ()
  "Remove roxygen font-lock keywords and completion from the current buffer."
  (when r-ts-roxygen--active-keywords
    (font-lock-remove-keywords nil r-ts-roxygen--active-keywords)
    (setq r-ts-roxygen--active-keywords nil))
  (remove-hook 'completion-at-point-functions
               #'r-ts-roxygen-complete-tag t))


;;;; Roxygen Minor Mode
;; =============================================================================

;;;###autoload
(define-minor-mode r-ts-roxygen-mode
  "Minor mode for roxygen documentation in R buffers."
  :init-value nil
  (if r-ts-roxygen-mode
      (r-ts-roxygen--enable)
    (r-ts-roxygen--disable))
  (when font-lock-mode
    (font-lock-flush)))


(provide 'r-ts-roxygen)
;;; r-ts-roxygen.el ends here
