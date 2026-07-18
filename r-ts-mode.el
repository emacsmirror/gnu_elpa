;;; r-ts-mode.el --- R treesitter mode  -*- lexical-binding: t -*-
;; Copyright (C) 2025  Manuel Teodoro Tenango

;; Author: Manuel Teodoro <ttm@teoten.me>
;; URL: https://codeberg.org/R-for-emacs/r-ts-mode
;; Assisted-by: Sonet:4.6
;; Package-Requires: ((emacs "30.1"))
;; Created: 2025-09-05

;; License
;; R-TS-MODE is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; R-TS-MODE is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with R-TS-MODE. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Major mode for editing R code using treesitter.

;;; Code:

(require 'treesit)
(declare-function r-ts-mode-parent-mode 'r-ts-mode)


;;;; =========================================================================
;;;; Groups, Custom Variables, General Variables
;;;; =========================================================================
(defgroup r-ts-mode nil
  "R support for Emacs using tree-sitter."
  :group 'languages
  :version "30.1")

(defcustom r-ts-mode-inherit-ess nil
  "When non-nil, inherit from `ess-r-mode' for R process interaction."
  :type 'boolean
  :group 'r-ts-mode)

(defcustom r-ts-mode-indent-level 2
  "Number of spaces per indentation level."
  :type 'integer
  :group 'r-ts-mode)

(defvar r-ts-mode--debug nil
  "When non-nil, enable verbose debugging messages.  For development use.")

(defvar r-ts-mode-font-lock-keywords nil
  "Replacement for ESS variable `ess-R-font-lock-keywords'
to silence ESS fontification.")


;;;; =========================================================================
;;;; Constants and Syntax Table
;;;; =========================================================================
(defvar r-ts-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?`  "\"" table)
    ;; Symbols
    (modify-syntax-entry ?_  "_"  table)
    (modify-syntax-entry ?.  "_"  table)
    (modify-syntax-entry ?$  "_"  table)
    (modify-syntax-entry ?@  "_"  table)
    (modify-syntax-entry ?:  "_"  table)
    ;; Punctuation
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?/  "."  table)
    (modify-syntax-entry ?^  "."  table)
    (modify-syntax-entry ?%  "."  table)
    (modify-syntax-entry ?<  "."  table)
    (modify-syntax-entry ?>  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?!  "."  table)
    (modify-syntax-entry ?&  "."  table)
    (modify-syntax-entry ?|  "."  table)
    (modify-syntax-entry ?\\ "."  table)
    table)
  "Syntax table for R source code.")


;;;; =========================================================================
;;;; Faces
;;;; =========================================================================
(defgroup r-ts-mode-faces nil
  "Faces for `r-ts-mode' syntax highlighting."
  :prefix "r-ts-mode-face-"
  :group 'r-ts-mode)

(defface r-ts-mode-face-string
  '((default (:inherit font-lock-string-face)))
  "Face for R string literals."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-escape
  '((default (:inherit font-lock-escape-face)))
  "Face for escape sequences inside strings."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-comment
  '((default (:inherit font-lock-comment-face)))
  "Face for R comments."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-bracket
  '((default (:inherit font-lock-bracket-face)))
  "Face for brackets and parentheses."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-delimiter
  '((default (:inherit font-lock-delimiter-face)))
  "Face for delimiters such as commas."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-function
  '((default (:inherit font-lock-function-name-face)))
  "Face for function definition names."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-function-call
  '((default (:inherit font-lock-function-call-face)))
  "Face for function call identifiers."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-type
  '((default (:inherit font-lock-type-face)))
  "Face for type names (S3, S4, R6, S7)."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-variable
  '((default (:inherit font-lock-variable-use-face)))
  "Face for variable names."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-keyword
  '((default (:inherit font-lock-keyword-face)))
  "Face for language keywords."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-constant
  '((default (:inherit font-lock-constant-face)))
  "Face for built-in constants (NULL, NA, Inf, NaN, …)."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-operator
  '((default (:inherit font-lock-operator-face)))
  "Face for operators."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-number
  '((default (:inherit font-lock-number-face)))
  "Face for numeric literals."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-boolean
  '((default (:inherit font-lock-constant-face)))
  "Face for boolean literals (TRUE, FALSE)."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-warning
  '((default (:inherit font-lock-warning-face)))
  "Face for syntax errors reported by the tree-sitter parser."
  :group 'r-ts-mode-faces)


;;;; =========================================================================
;;;; Parent Mode (ESS or prog-mode)
;;;; =========================================================================

;; ESS declarations — kept here so the byte-compiler is happy,
;; but ESS-specific logic is intentionally left untouched for now.
(declare-function ess-r-mode                'r-ts-mode)
(declare-function ess-mode                  'r-ts-mode)
(declare-function ess-setq-vars-local       'r-ts-mode)
(declare-function ess-set-style             'r-ts-mode)
(declare-function ess--setup-eldoc          'r-ts-mode)
(declare-function ess--setup-auto-complete  'r-ts-mode)
(declare-function ess--setup-company        'r-ts-mode)
(declare-function ess-r-eldoc-function      'r-ts-mode)
(declare-function ess-filename-completion   'r-ts-mode)
(declare-function ess-r-project             'r-ts-mode)
(declare-function ess-r-xref-backend        'r-ts-mode)
(declare-function ess-r-package-completion  'r-ts-mode)
(declare-function ess-r-object-completion   'r-ts-mode)
(defvar ess-r-mode-syntax-table)
(defvar ess-r-customize-alist)
(defvar ess-font-lock-keywords)
(defvar ess-r--syntax-propertize-function)
(defvar ess-r-ac-sources)
(defvar ess-r-company-backends)
(defvar ess-mode-map)

(if r-ts-mode-inherit-ess
    (if (not (fboundp 'ess-r-mode))
        (error "ESS is not available.  Is it installed?")
      (progn
        (require 'ess-mode)
        (require 'ess-r-mode)
        (require 'ess-r-completion)
        (require 'ess-utils)
        (defvar r-ts-mode-parent-mode-map
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map ess-mode-map)
            map)
          "Keymap for `r-ts-mode-parent-mode' when inheriting from ESS.")

        (define-derived-mode r-ts-mode-parent-mode ess-mode "" ""
          :group 'r-ts-mode
          :keymap r-ts-mode-parent-mode-map
          (set-syntax-table ess-r-mode-syntax-table)
          (ess-setq-vars-local ess-r-customize-alist)
          (setq-local ess-font-lock-keywords 'r-ts-mode-font-lock-keywords)
          (setq-local add-log-current-defun-header-regexp
                      "^\\(.+\\)\\s-+<-[ \t\n]*function")
          (setq-local syntax-propertize-function ess-r--syntax-propertize-function)
          (add-hook 'hack-local-variables-hook #'ess-set-style nil t)
          (ess--setup-eldoc #'ess-r-eldoc-function)
          (ess--setup-auto-complete ess-r-ac-sources)
          (ess--setup-company ess-r-company-backends)
          (remove-hook 'completion-at-point-functions #'ess-filename-completion 'local)
          (add-hook 'completion-at-point-functions #'ess-r-object-completion  nil 'local)
          (add-hook 'completion-at-point-functions #'ess-r-package-completion nil 'local)
          (add-hook 'completion-at-point-functions #'ess-filename-completion   nil 'local)
          (add-hook 'xref-backend-functions #'ess-r-xref-backend nil 'local)
          (add-hook 'project-find-functions  #'ess-r-project       nil 'local))))
  (progn
    (defalias 'r-ts-mode-parent-mode-map 'prog-mode-map
      "Alias to `prog-mode-map' when not inheriting from ESS.")
    (define-derived-mode r-ts-mode-parent-mode prog-mode "R-ts Parent"
      "Parent mode for `r-ts-mode' when ESS is not used."
      :group 'r-ts-mode
      (set-syntax-table r-ts-mode-syntax-table)
      (setq-local comment-start "#")
      (setq-local comment-end ""))))


;;;; =========================================================================
;;;; Tree-sitter Node Utilities — Pure Predicates and Accessors
;;;; =========================================================================
(defun r-ts-mode--node-is-fun-def-p (node)
  "Return non-nil if the last child of NODE is a `function_definition'."
  (treesit-node-match-p (treesit-node-child node -1) "function_definition"))

(defun r-ts-mode--node-is-assignment-p (node)
  "Return non-nil if NODE is a `binary_operator' using `<-' or `='."
  (and (treesit-node-match-p node "binary_operator")
       (string-match-p (regexp-opt '("<-" "="))
                       (treesit-node-text (treesit-node-child node -2) t))))

(defun r-ts-mode--node-is-simple-object-p (node)
  "Return non-nil if NODE assigns a non-function at top level.
That is: a `binary_operator' using `<-' or `=' whose RHS is not a
`function_definition', and which is not nested inside a function."
  (and (r-ts-mode--node-is-assignment-p node)
       (not (treesit-node-match-p (treesit-node-child node -1) "function_definition"))
       (not (treesit-parent-until node "function_definition"))))

(defun r-ts-mode--node-lhs-text (node)
  "Return the text of the left-hand side identifier of binary NODE.
Returns nil if NODE is not a `binary_operator'."
  (when (treesit-node-match-p node "binary_operator")
    (treesit-node-text (treesit-node-child node -3) t)))

;; Public aliases with the names expected by treesit settings
(defalias 'r-ts-mode--is-fun-def   #'r-ts-mode--node-is-fun-def-p
  "Predicate: is NODE a function definition assignment?  See
  `r-ts-mode--node-is-fun-def-p'.")

(defalias 'r-ts-mode--is-simple-object #'r-ts-mode--node-is-simple-object-p
  "Predicate: is NODE a simple (non-function) assignment?  See
  `r-ts-mode--node-is-simple-object-p'.")

(defun r-ts-mode--defun-name (node)
  "Return the name of the function defined at NODE, or nil.
Expected by `treesit-defun-name-function'."
  (when (r-ts-mode--node-is-fun-def-p node)
    (r-ts-mode--node-lhs-text node)))

(defun r-ts-mode--object-name (node)
  "Return the name of the object assigned at NODE, or nil.
Expected by `treesit-simple-imenu-settings' for non-function objects."
  (when (treesit-node-match-p node "binary_operator")
    (r-ts-mode--node-lhs-text node)))


;;;; =========================================================================
;;;; Node Navigation Utilities
;;;; =========================================================================
(defun r-ts-mode--node-ancestor-matching (node type)
  "Walk up the tree from NODE, returning the first ancestor matching TYPE.
Returns nil if the `program' root is reached without a match.
TYPE must not be \"program\" itself."
  (cond
   ((treesit-node-match-p node type) node)
   ((treesit-node-match-p node "program") nil)
   (t (r-ts-mode--node-ancestor-matching (treesit-node-parent node) type))))

(defun r-ts-mode--inside-fun-def-p ()
  "Return non-nil if point is inside or immediately before a function definition."
  (let ((node (treesit-node-at (point))))
    (or (r-ts-mode--node-ancestor-matching node "function_definition")
        (treesit-node-match-p (treesit-node-next-sibling node) "function_definition")
        (treesit-node-match-p (treesit-node-next-sibling
                               (treesit-node-next-sibling node))
                              "function_definition"))))

(defalias 'r-ts-mode--inside-fun-def? #'r-ts-mode--inside-fun-def-p)

(defun r-ts-mode--argument-function-name ()
  "Return the function name when point is inside a call's argument list.
Returns nil if point is not inside an `arguments' or `argument' node."
  (let* ((node-at-point (treesit-node-parent (treesit-node-at (point))))
         (potential-call (treesit-node-parent node-at-point))
         (call-node (pcase (treesit-node-type node-at-point)
                      ("arguments" potential-call)
                      ("argument"  (treesit-node-parent potential-call)))))
    (when call-node
      (treesit-node-text
       (treesit-node-child-by-field-name call-node "function") t))))

(defun r-ts-mode--buffer-function-positions (buffer-or-name)
  "Return an alist of (name . position) for all function definitions
in BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    (let* ((query (treesit-query-compile 'r '((function_definition name: "function" @val))))
           (ranges (mapcar #'car (treesit-query-range 'r query))))
      (delq nil
            (mapcar (lambda (pos)
                      (let ((parent (treesit-node-parent
                                     (treesit-node-parent (treesit-node-at pos)))))
                        (when (treesit-node-match-p parent "binary_operator")
                          (cons (treesit-node-text (treesit-node-child parent -3) t)
                                pos))))
                    ranges)))))


;;;; =========================================================================
;;;; Tree-sitter Font-lock Settings
;;;; =========================================================================
(defvar r-ts-mode--operators
  '("?" ":=" "=" "<-" "<<-" "->" "->>"
    "~" "|>" "||" "|" "&&" "&"
    "<" "<=" ">" ">=" "==" "!="
    "+" "-" "*" "/" "::" ":::"
    "**" "^" "$" "@" ":"
    "special")
  "R operators recognised by the tree-sitter font-lock rules.")

(defvar r-ts-mode-settings
  (treesit-font-lock-rules
   :default-language 'r

   :feature 'number
   '([(integer) (float) (complex)] @r-ts-mode-face-number)

   :feature 'string
   '((string) @r-ts-mode-face-string)

   :feature 'string-escape
   :override t
   '((string (string_content (escape_sequence) @r-ts-mode-face-escape)))

   :feature 'comment
   '((comment) @r-ts-mode-face-comment)

   :feature 'operator
   :override t
   `([,@r-ts-mode--operators] @r-ts-mode-face-operator)

   :feature 'punctuation-bracket
   '(["(" ")" "[" "]" "{" "}" "[[" "]]"] @r-ts-mode-face-bracket)

   :feature 'punctuation-delimiter
   :override t
   '((comma) @r-ts-mode-face-delimiter)

   :feature 'function
   :override t
   '((binary_operator
      lhs: (identifier) @r-ts-mode-face-function
      operator: "<-"
      rhs: (function_definition))
     (binary_operator
      lhs: (identifier) @r-ts-mode-face-function
      operator: "="
      rhs: (function_definition)))

   :feature 'function-call
   :override t
   '((call function: (identifier) @r-ts-mode-face-function-call))

   :feature 'variable-parameter
   :override t
   '((parameters (parameter name: (identifier) @r-ts-mode-face-variable))
     (arguments  (argument  name: (identifier) @r-ts-mode-face-variable)))

   :feature 'variable
   '((binary_operator
      lhs: (identifier) @r-ts-mode-face-variable
      operator: "="
      rhs: (_))
     (binary_operator
      lhs: (identifier) @r-ts-mode-face-variable
      operator: "<-"
      rhs: (_)))

   :feature 'namespace
   :override t
   '((namespace_operator lhs: (identifier) @r-ts-mode-face-keyword))

   :feature 'keyword-function
   :override t
   '((function_definition name: "function" @r-ts-mode-face-keyword))

   :feature 'keyword
   :override t
   '(["in" (return) (next) (break)] @r-ts-mode-face-keyword)

   :feature 'conditional
   :override t
   '(["if" "else"] @r-ts-mode-face-keyword)

   :feature 'repeat
   :override t
   '(["while" "repeat" "for"] @r-ts-mode-face-keyword)

   :feature 'boolean
   :override t
   '([(true) (false)] @r-ts-mode-face-boolean)

   :feature 'constant-builtin
   :override t
   '([(null) (inf) (nan) (na) (dots) (dot_dot_i)] @r-ts-mode-face-constant)

   :feature 'type
   :override t
   '(;; R6 and S7
     (binary_operator
      lhs: (identifier) @r-ts-mode-face-type
      operator: "<-"
      rhs: (call function: (identifier) @fn-name
                 (:match "\\(?:R6Class\\|new_class\\)" @fn-name)))
     (binary_operator
      lhs: (identifier) @r-ts-mode-face-type
      operator: "="
      rhs: (call function: (identifier) @fn-name
                 (:match "\\(?:R6Class\\|new_class\\)" @fn-name)))
     ;; S4
     (call function:
           (identifier) @fn-name
           arguments: (arguments :anchor (argument value: (string) @r-ts-mode-face-type))
           (:match "\\`setClass\\'" @fn-name))
     ;; S3
     (call function:
           (identifier) @fn-name
           arguments: (arguments
                       (argument name:  (identifier) @arg-name
                                 (:match "\\`class\\'" @arg-name)
                                 value: (_) @r-ts-mode-face-type))
           (:match "\\`structure\\'" @fn-name)))

   :feature 'error
   :override t
   '((ERROR) @r-ts-mode-face-warning))
  "Tree-sitter font-lock rules for `r-ts-mode'.")


;;;; =========================================================================
;;;; Indentation, Navigation, Imenu Settings
;;;; =========================================================================
(defvar r-ts-mode--indent-rules
  `((r
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((parent-is "binary_operator")   parent-bol r-ts-mode-indent-level)
     ((parent-is "braced_expression") parent-bol r-ts-mode-indent-level)
     ((parent-is "arguments")         parent-bol r-ts-mode-indent-level)
     ((parent-is "parameters")        parent-bol r-ts-mode-indent-level)))
  "Tree-sitter indentation rules for R.")

(defvar r-ts-mode--imenu-settings
  `(("Function" "binary_operator" r-ts-mode--is-fun-def      r-ts-mode--defun-name)
    ("Object"   "binary_operator" r-ts-mode--is-simple-object r-ts-mode--object-name))
  "Imenu configuration for `r-ts-mode'.")

(defun r-ts-mode--walk-to-definition (backwards)
  "Move point to the next (or previous, when BACKWARDS is non-nil)
object definition."
  (treesit-search-forward-goto
   (treesit-node-at (point))
   #'r-ts-mode--node-is-simple-object-p
   nil backwards nil))

(defun r-ts-mode-goto-previous-definition ()
  "Move point to the previous R object definition (`<-' or `=')."
  (interactive)
  (r-ts-mode--walk-to-definition t))

(defun r-ts-mode-goto-next-definition ()
  "Move point to the next R object definition (`<-' or `=')."
  (interactive)
  (r-ts-mode--walk-to-definition nil))


;;;; =========================================================================
;;;; Major Mode Definition
;;;; =========================================================================

;;;###autoload
(define-derived-mode r-ts-mode r-ts-mode-parent-mode "R"
  "Major mode for editing R code, powered by tree-sitter."
  :group 'r-ts-mode

  (unless (treesit-ready-p 'r)
    (error "Tree-sitter grammar for R is not available"))

  (treesit-parser-create 'r)

  ;; Debugging
  (when r-ts-mode--debug
    (setq-local treesit--indent-verbose t)
    (setq-local treesit--font-lock-verbose t)
    (treesit-inspect-mode))

  ;; Font-lock
  (setq-local treesit-font-lock-feature-list
              '((comment)
                (operator string repeat)
                (punctuation-bracket boolean conditional function function-call
                 keyword number constant-builtin variable)
                (punctuation-delimiter string-escape variable-parameter error
                 namespace keyword-function type)))
  (setq-local treesit-font-lock-level 4)
  (setq-local treesit-font-lock-settings r-ts-mode-settings)

  ;; Navigation
  (setq-local treesit-defun-type-regexp
              (cons (rx "binary_operator") #'r-ts-mode--node-is-fun-def-p))
  (setq-local treesit-defun-name-function #'r-ts-mode--defun-name)

  ;; Indentation
  (setq-local treesit-simple-indent-rules r-ts-mode--indent-rules)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings r-ts-mode--imenu-settings)

  ;; Finalise tree-sitter setup
  (treesit-major-mode-setup))

;;;###autoload
(defalias 'R-ts-mode #'r-ts-mode
  "Alias for `r-ts-mode' matching the capitalisation convention for R files.")


(provide 'r-ts-mode)
;;; r-ts-mode.el ends here
