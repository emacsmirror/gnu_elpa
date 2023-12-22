;;; devicetree-ts-mode.el --- Tree-sitter support for DTS   -*- lexical-binding: t; -*-

;; Copyright © 2023  Free Software Foundation, Inc.

;; Author: Aleksandr Vityazev <avityazew@gmail.com>
;; Keywords: languages devicetree tree-sitter
;; Version: 0.3
;; Homepage: https://sr.ht/~akagi/devicetree-ts-mode
;; Package-Requires: ((emacs "29.1"))

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The "Open Firmware Device Tree", or simply Devicetree, is a
;; data structure and language for describing hardware.  More
;; specifically, it is a description of hardware that is readable by
;; an operating system so that the operating system doesn't need to
;; hard code details of the machine.
;;
;; A grammar for Devicetree can be found at
;; https://github.com/joelspadin/tree-sitter-devicetree
;;
;; Thist package provides tree-sitter major mode for Devicetree files.

;; Features
;;
;; * Indent
;; * IMenu
;; * Font Lock


;;; Code:


(require 'treesit)
(require 'c-ts-common)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")

(defgroup devicetree ()
  "Tree-sitter support for DTS."
  :prefix "devicetree-ts-"
  :group 'languages)

(defcustom devicetree-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `devicetree-ts-mode'."
  :type 'natnum
  :safe 'natnump)

;; Taken from the dts-mode
(defvar devicetree-ts-syntax-mode-table
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?<  "(>" table)
    (modify-syntax-entry ?>  ")<" table)

    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?~  "." table)

    ;; _ and , are both symbol constituents.
    (modify-syntax-entry ?,  "_" table)
    (modify-syntax-entry ?_  "_" table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table)
  "Syntax table for `devicetree-ts-mode'.")

(defun devicetree-ts-mode--indent-rules ()
  "Tree-sitter indent rules for `devicetree-ts-mode'."
  (let ((offset devicetree-ts-mode-indent-offset))
    `((devicetree
       ((node-is ">") parent-bol 0)
       ((node-is "]") parent-bol 0)
       ((node-is "}") standalone-parent 0)
       ((and (parent-is "comment") c-ts-common-looking-at-star)
        c-ts-common-comment-start-after-first-star -1)
       ((parent-is "node") parent-bol ,offset)
       ((parent-is "property") parent-bol ,offset)
       ((parent-is "integer_cells") first-sibling 1)
       (no-node parent-bol 0)))))

(defconst devicetree-ts-mode--treesit-keywords
  '("/delete-node/" "/delete-property/" "#define" "#include"
    "/omit-if-no-ref/" "/dts-v1/"))

(defconst devicetree-ts-mode--treesit-operators
  '("!" "~" "-" "+" "*" "/" "%" "||" "&&" "|"
    "^" "&" "==" "!=" ">" ">=" "<=" ">" "<<" ">>"))

(defvar devicetree-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'devicetree
   :feature 'comment
   `((comment) @font-lock-comment-face
     (comment) @contextual)

   :language 'devicetree
   :feature 'string
   `((string_literal) @font-lock-string-face
     (system_lib_string) @font-lock-string-face)

   :language 'devicetree
   :feature 'keyword
   `([,@devicetree-ts-mode--treesit-keywords]
     @font-lock-keyword-face)

   :language 'devicetree
   :feature 'operator
   `([,@devicetree-ts-mode--treesit-operators]
     @font-lock-operator-face)

   :language 'devicetree
   :override t
   :feature 'node
   `((node name: (identifier) @font-lock-type-face)
     (node name: (reference "&" (identifier)) @font-lock-type-face))

   :language 'devicetree
   :feature 'label
   `((labeled_item label: (identifier) @font-lock-constant-face))

   :language 'devicetree
   :override t
   :feature 'reference
   `(((reference "&" (identifier)) @font-lock-property-use-face))

   :language 'devicetree
   :feature 'bracket
   '((["[" "]" "<" ">" "{" "}"]) @font-lock-bracket-face)

   :language 'devicetree
   :feature 'delimiter
   '((["," ";"]) @font-lock-delimiter-face)

   :language 'devicetree
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings.")

(defun devicetree-ts-mode--node-addresses (node)
  "List of addresses for NODE."
  (reverse
   (seq-reduce
    (lambda (acc children)
      (if (string-equal (treesit-node-field-name children)
                        "address")
          (cons (treesit-node-text children t) acc)
        acc))
    (treesit-node-children node)
    '())))

(defun devicetree-ts-mode--name-function (node)
  "Return name of NODE to use for in imenu."
  (let ((name (treesit-node-child-by-field-name node "name")))
    (concat (treesit-node-text name t)
            (apply #'concat
                   (seq-take (devicetree-ts-mode--node-addresses node)
                             2)))))

;;;###autoload
(define-derived-mode devicetree-ts-mode prog-mode "DTS"
  "Major mode for editing devicetree, powered by tree-sitter."

  (when (treesit-ready-p 'devicetree)
    (treesit-parser-create 'devicetree)

    ;; Comments.
    (setq-local comment-start "/* ")
    (setq-local comment-end " */")

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `((nil ,(rx bos "node" eos)
                       nil devicetree-ts-mode--name-function)))
    (setq-local which-func-functions nil)

    ;; Indent.
    (setq-local treesit-simple-indent-rules
                (devicetree-ts-mode--indent-rules))

    ;; (setq-local indent-tabs-mode t)

    ;; Electric
    (setq-local electric-indent-chars
                (append "{}<>[]" electric-indent-chars))

    ;; Navigation
    (setq-local treesit-thing-settings
                `((devicetree
                   (sexp (not ,(rx (or "{" "}" "<" ">" "[" "]" "," ";")))))))

    ;; Font-lock.
    (setq-local treesit-font-lock-settings
                devicetree-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment node)
                  (keyword string)
                  (bracket delimiter error operator)
                  (label reference)))

    (treesit-major-mode-setup)))

(when (treesit-ready-p 'devicetree)
  (add-to-list 'auto-mode-alist '("\\.dtsi?\\'" . devicetree-ts-mode)))

(provide 'devicetree-ts-mode)
;;; devicetree-ts-mode.el ends here
