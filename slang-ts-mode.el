;;; slang-ts-mode.el --- tree-sitter support for Slang -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Hikari

;; Author: Hikari <aneris@disroot.org>
;; URL: https://codeberg.org/hikari/slang-ts-mode
;; Version: 0.1
;; Package-Requires: ((emacs "30"))
;; Keywords: slang languages tree-sitter

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a tree-sitter mode for the Slang shader
;; language, see https://shader-slang.org.
;;
;; To use the package, using use-package simply:
;; (use-package slang-ts-mode :ensure t)

;;; Code:

(require 'c-ts-common) ;; For comments.
(require 'treesit)

;;;; Declare those functions so we don't get errors during byte compilation.
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")

;;;; Add tree-sitter-slang to the treesit-language-source-alist
(add-to-list
 'treesit-language-source-alist
 '(slang "https://github.com/tree-sitter-grammars/tree-sitter-slang"
         :commit "1dbcc4abc7b3cdd663eb03d93031167d6ed19f56")
 t)

;;;; Customization
(defgroup slang nil
  "Major mode for editing slang code."
  :group 'languages)

(defcustom slang-ts-indent-offset 4
  "Number of spaces for each indentation step in `slang-ts-mode'."
  :package-version '(slang-ts-mode . "0.1")
  :type 'natnum
  :safe #'natnump)

;;; Syntax Table (mostly derived from C)
(defvar slang-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+   "."      table)
    (modify-syntax-entry ?-   "."      table)
    (modify-syntax-entry ?=   "."      table)
    (modify-syntax-entry ?%   "."      table)
    (modify-syntax-entry ?&   "."      table)
    (modify-syntax-entry ?|   "."      table)
    (modify-syntax-entry ?^   "."      table)
    (modify-syntax-entry ?!   "."      table)
    (modify-syntax-entry ?@   "."      table)
    (modify-syntax-entry ?~   "."      table)
    (modify-syntax-entry ?<   "."      table)
    (modify-syntax-entry ?>   "."      table)
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23"   table)
    (modify-syntax-entry ?\n  "> b"    table)
    (modify-syntax-entry ?\^m "> b"    table)
    (modify-syntax-entry ?\` "." table)
    (modify-syntax-entry ?$  "." table)
    table)
  "Syntax table for `slang-ts-mode'.")

;;; Private
(defvar slang-ts-mode--keywords
  '("struct" "class" "return" "static" "import" "module" "interface" "dyn"
    "some" "associatedtype" "var" "let" "is" "as" "extension" "property"
    "namespace" "where" "switch" "case" "for" "enum" "if" "else" "while"
    "do" "break" "continue" "const" "default" "in" "out" "inout" "export"
    "groupshared" "noperspective" "nointerpolation" "point" "precise"
    "register" "row_major" "column_major" "sample" "shared" "snorm"
    "triangle" "triangleadj" "uniform" "unorm" "linear" "typedef" "union"
    "inline")
  "Slang keywords for tree-sitter font-locking.")

(defvar slang-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'slang
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'slang
   :feature 'operator
   `((binary_expression operator: _  @font-lock-operator-face)
     (update_expression operator: _  @font-lock-operator-face)
     (assignment_expression operator: _  @font-lock-operator-face)
     (pointer_expression operator: _  @font-lock-operator-face)
     (unary_expression operator: _  @font-lock-operator-face))

   :language 'slang
   :feature 'keyword
   `([,@slang-ts-mode--keywords] @font-lock-keyword-face)

   :language 'slang
   :feature 'builtin
   `((bitfield_clause ":" (identifier) @font-lock-builtin-face)
     (semantics ":" (identifier) @font-lock-builtin-face))

   :language 'slang
   :feature 'constant
   '((declaration (type_qualifier) declarator: (init_declarator declarator: (identifier) @font-lock-constant-face)))

   :language 'slang
   :feature 'variable
   '((declaration declarator: (identifier) @font-lock-variable-name-face)
     (declaration declarator: (init_declarator declarator: (identifier) @font-lock-variable-name-face))
     (array_declarator declarator: (identifier) @font-lock-variable-name-face))

   :language 'slang
   :feature 'subscript
   '((subscript_expression argument: (identifier) @font-lock-variable-use-face))

   :language 'slang
   :feature 'type
   '((type_identifier) @font-lock-type-face
     (primitive_type) @font-lock-type-face)

   :language 'slang
   :feature 'number
   '((number_literal) @font-lock-number-face)

   :language 'slang
   :feature 'string
   '((string_literal) @font-lock-string-face)

   :language 'slang
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :language 'slang
   :feature 'attribute
   '((hlsl_attribute (call_expression function: (identifier) @font-lock-function-call-face)))

   :language 'slang
   :feature 'function
   '((function_declarator declarator: (identifier) @font-lock-function-name-face)
     (function_declarator declarator: (field_identifier) @font-lock-function-name-face))

   :language 'slang
   :feature 'function-call
   '((call_expression function: (identifier) @font-lock-function-call-face))

   :language 'slang
   :feature 'namespace
   '((qualified_identifier scope: (namespace_identifier) @font-lock-constant-face)))
  "Tree-sitter font-lock settings for `slang-ts-mode`.")

(defvar slang-ts-mode--indent-rules
  `((slang
     ((parent-is "translation_unit") column-0 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "{") (and parent parent-bol) 0)
     ((node-is "}") (and parent parent-bol) 0)
     ((and (parent-is "comment") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "binary_expression") parent-bol slang-ts-indent-offset)
     ((parent-is "field_declaration_list") parent-bol slang-ts-indent-offset)
     ((parent-is "compound_statement") parent-bol slang-ts-indent-offset)
     ((parent-is "case_statement") parent-bol slang-ts-indent-offset)
     ((parent-is "declaration") parent-bol slang-ts-indent-offset)
     ((parent-is "init_declarator") parent-bol slang-ts-indent-offset)
     ((parent-is "argument_list") parent-bol slang-ts-indent-offset)
     ((parent-is "parameter_list") parent-bol slang-ts-indent-offset)
     ((parent-is "function_declarator") parent-bol slang-ts-indent-offset)
     ((parent-is "where_clause") parent-bol slang-ts-indent-offset)
     ((parent-is "assignment_expression") parent-bol slang-ts-indent-offset)))
  "Tree-sitter indent rules for `slang-ts-mode`.")

(defun slang-ts-mode--defun-name (node)
  "Return the name text of the treesit NODE."
  (pcase (treesit-node-type node)
    ("module_declaration"
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("struct_specifier"
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("class_specifier"
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("function_declarator"
     (treesit-node-text
      (treesit-node-child-by-field-name node "declarator") t))))

;;;; Mode
;;;###autoload
(define-derived-mode slang-ts-mode prog-mode "Slang"
  "Major mode for editing Slang, powered by tree-sitter."
  :syntax-table slang-ts-mode--syntax-table

  (when (and (treesit-ensure-installed 'slang)
             (treesit-ready-p 'slang))
    (setq treesit-primary-parser (treesit-parser-create 'slang)))

  (setq-local treesit-defun-name-function #'slang-ts-mode--defun-name)

  ;; Comments
  (c-ts-common-comment-setup)

  ;; Font lock
  (setq-local treesit-font-lock-settings slang-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '(( comment)
                ( keyword string)
                (attribute
                 type variable constant builtin namespace)
                ( bracket number function operator subscript function-call)))

  (setq-local treesit-simple-indent-rules slang-ts-mode--indent-rules)

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;,#" electric-indent-chars))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              `(("Module" "\\`module_declaration\\'" nil nil)
                ("Struct" "\\`struct_specifier\\'" nil nil)
                ("Class" "\\`class_specifier\\'" nil nil)
                ("Function" "\\`function_declarator\\'" nil nil)))

  ;; Navigation
  (setq-local treesit-defun-type-regexp
              (regexp-opt '("function_definition" "struct_specifier" "class_specifier" "module_declaration")))

  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.slang\\'" . slang-ts-mode))

(provide 'slang-ts-mode)

;;; slang-ts-mode.el ends here
