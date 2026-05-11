;;; r-ts-mode.el --- Emacs Speaks R  -*- lexical-binding: t -*-
;; Copyright (C) 2025  Manuel Teodoro Tenango

;; Author: Manuel Teodoro <ttm@teoten.me>
;; URL: https://codeberg.org/teoten/r-ts-mode
;; Version: 0.3.0
;; Package-Requires: ((emacs "29.1"))
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
;; (require 'r-ts-mode-roxygen)
;; (defvar r-ts-mode-inherit-ess)
(declare-function r-ts-mode-parent-mode 'r-ts-mode)


;; ----- Variables: groups, custom, general
(defgroup r-ts-mode nil
  "R support for Emacs and treesitter."
  :group 'languages
  :version "30.1")

(defcustom r-ts-mode-inherit-ess t
  "Should r-ts-mode inherit ess-r-mode?"
  :type 'boolean
  :group 'r-ts-mode)

(defcustom r-ts-mode-r-program "R"
  "Program name for invoking R"
  :type '(choice (string) file)
  :group 'r-ts-mode)

(defcustom r-ts-mode-indent-level 2
  "Indentation level for r-ts-mode"
  :type 'integer
  :group 'r-ts-mode)

(defcustom r-ts-mode-roxygen-tags-param
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
  "The tags used in roxygen fields that require a parameter.
Used to decide highlighting and tag completion."
  :group 'r-ts-mode
  :type '(repeat string))

(defcustom r-ts-mode-roxygen-tags-noparam '("export" "noRd")
  "The tags used in roxygen fields that can be used alone.
Used to decide highlighting and tag completion."
  :group 'r-ts-mode
  :type '(repeat string))

(defvar r-ts-mode--debug nil
  "When not `nil' enables debugging messages. Used for development.")

(defvar r-ts-mode-font-lock-keywords nil
  "Replacement for ESS variable `ess-R-font-lock-keywords' to silence ESS fontification.")

(defconst r-ts-mode-roxygen--initial-regex "^[ \t]*#+'"
  "Regular expression to recognize roxygen blocks.")

(defconst r-ts-mode-roxygen--param-name-regexp
  "\\(?:\\(?:\\sw\\|\\s_\\)+,?\\)+"
  "Regexp to match a parameter name, which can include symbols.")

(defvar r-ts-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?`  "\"" table)  ; ` for strings (in R 4.0+)
    ;; Symbol
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
    (modify-syntax-entry ?% "."  table)
    (modify-syntax-entry ?<  "."  table)
    (modify-syntax-entry ?>  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?!  "."  table)
    (modify-syntax-entry ?&  "."  table)
    (modify-syntax-entry ?|  "."  table)
    (modify-syntax-entry ?\\ "."  table)
    table)
  "Syntax table for R code.")


;; ----- Faces
(defgroup r-ts-mode-faces nil
  "Faces for highlighting text."
  :prefix "r-ts-mode-font-lock-"
  :group 'font-lock)

(defface r-ts-mode-face-string
  '((default (:inherit font-lock-string-face)))
  "R-TS-MODE Font Lock string Face."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-escape
  '((default (:inherit font-lock-escape-face)))
  "R-TS-MODE Font Lock escape Face."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-comment
  '((default (:inherit font-lock-comment-face)))
  "R-TS-MODE Font Lock comment Face."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-bracket
  '((default (:inherit font-lock-bracket-face)))
  "R-TS-MODE Font Lock bracket Face."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-delimiter
  '((default (:inherit font-lock-delimiter-face)))
  "R-TS-MODE Font Lock delimiter Face."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-function
  '((default (:inherit font-lock-function-name-face)))
  "R-TS-MODE Font Lock function Face."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-function-call
  '((default (:inherit font-lock-function-call-face)))
  "R-TS-MODE Font Lock function Face."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-type
  '((default (:inherit font-lock-type-face)))
  "R-TS-MODE Font Lock Type Face."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-variable
  '((default (:inherit font-lock-variable-use-face)))
  "R-TS-MODE Font Lock variable Face."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-keyword
  '((default (:inherit font-lock-keyword-face)))
  "R-TS-MODE Font Lock keyword Face."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-constant
  '((default (:inherit font-lock-constant-face)))
  "R-TS-MODE Font Lock constant Face."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-operator
  '((default (:inherit font-lock-operator-face)))
  "R-TS-MODE Font Lock Operators Face."
  :group 'r-ts-mode-faces)

(defface r-ts-mode-face-number
  '((default (:inherit font-lock-number-face)))
  "R-TS-MODE Font Lock Number Face,")

(defface r-ts-mode-face-boolean
  '((default (:inherit font-lock-constant-face)))
  "R-TS-MODE Font Lock Number Face,")

(defface r-ts-mode-face-warning
  '((default (:inherit font-lock-warning-face)))
  "R-TS-MODE Font Lock warning Face."
  :group 'r-ts-mode-faces)

;; ----- Parent mode
;; ESS specific
(declare-function ess-r-mode 'r-ts-mode)
(declare-function ess-mode 'r-ts-mode)
(declare-function ess-setq-vars-local 'r-ts-mode)
(declare-function ess-set-style 'r-ts-mode)
(declare-function ess--setup-eldoc 'r-ts-mode)
(declare-function ess--setup-auto-complete 'r-ts-mode)
(declare-function ess--setup-company 'r-ts-mode)
(declare-function ess-r-eldoc-function 'r-ts-mode)
(declare-function ess-filename-completion 'r-ts-mode)
(declare-function ess-r-project 'r-ts-mode)
(declare-function ess-r-xref-backend 'r-ts-mode)
(declare-function ess-r-package-completion 'r-ts-mode)
(declare-function ess-package-completion 'r-ts-mode)
(declare-function ess-r-object-completion 'r-ts-mode)
(defvar ess-r-mode-syntax-table)
(defvar ess-r-customize-alist)
(defvar ess-font-lock-keywords)
(defvar ess-r--syntax-propertize-function)
(defvar ess-r-ac-sources)
(defvar ess-r-company-backends)

(if r-ts-mode-inherit-ess
    (if (not (fboundp 'ess-r-mode))
	(error "ESS is not available. Is it installed?")
      (progn
	(require 'ess-mode)
	(require 'ess-r-mode)
	(require 'ess-r-completion)
	(require 'ess-utils)
	(defvar r-ts-mode-parent-mode-map
	  (let ((map (make-sparse-keymap)))
	    (set-keymap-parent map ess-mode-map)
	    map)
	  "")

	(define-derived-mode r-ts-mode-parent-mode ess-mode "" ""
	  :group 'r-ts-mode
	  :keymap r-ts-mode-parent-mode-map
	  (set-syntax-table ess-r-mode-syntax-table)
	  (ess-setq-vars-local ess-r-customize-alist)
	  (setq-local ess-font-lock-keywords 'r-ts-mode-font-lock-keywords)
	  ;; (setq-local paragraph-start (concat "\\s-*$\\|" page-delimiter))
	  ;; (setq-local paragraph-separate (concat "\\s-*$\\|" page-delimiter))
	  ;; (setq-local paragraph-ignore-fill-prefix t)
	  ;; (setq-local indent-line-function #'ess-r-indent-line)
	  ;; (setq-local comment-indent-function #'ess-calculate-indent)

	  (setq-local add-log-current-defun-header-regexp "^\\(.+\\)\\s-+<-[ \t\n]*function")
	  (setq-local syntax-propertize-function ess-r--syntax-propertize-function)

	  ;; indentation
	  (add-hook 'hack-local-variables-hook #'ess-set-style nil t)
	  ;; eldoc
	  (ess--setup-eldoc #'ess-r-eldoc-function)
	  ;; auto-complete
	  (ess--setup-auto-complete ess-r-ac-sources)
	  ;; company
	  (ess--setup-company ess-r-company-backends)
	  ;; (setq-local prettify-symbols-alist ess-r-prettify-symbols)
	  ;; (setq font-lock-defaults '(ess-build-font-lock-keywords nil nil ((?\. . "w") (?\_ . "w"))))
	  (remove-hook 'completion-at-point-functions #'ess-filename-completion 'local) ;; should be first
	  (add-hook 'completion-at-point-functions #'ess-r-object-completion nil 'local)
	  (add-hook 'completion-at-point-functions #'ess-r-package-completion nil 'local)
	  (add-hook 'completion-at-point-functions #'ess-filename-completion nil 'local)
	  (add-hook 'xref-backend-functions #'ess-r-xref-backend nil 'local)
	  (add-hook 'project-find-functions #'ess-r-project nil 'local))))
  (progn
    (defalias 'r-ts-mode-parent-mode-map 'prog-mode-map "prog-mode-map")
    (define-derived-mode r-ts-mode-parent-mode prog-mode "R-ts-mode Parent mode"
      "r-ts-mode parent Mode independent from ESS"
      :group 'r-ts-mode
      :syntax-table r-ts-mode-syntax-table
      (setq-local comment-start "#")
      (setq-local comment-end ""))))


;; ----- Preparing the grammar
(defun r-ts-mode--find-treesitter-r-package-path ()
  "Find the path to the R package 'treesitter.r'."
  (let* ((r-exec "--no-echo -q -e")
         (r-exec-command (if (string-match ".exe" r-ts-mode-r-program)
                             (format
                              "%s %s print(find.package('treesitter.r'))"
                              r-ts-mode-r-program r-exec)
                           (format
                            "%s %s 'print(find.package(\"treesitter.r\"))'"
                            r-ts-mode-r-program r-exec)))
         (r-output (progn
                     (shell-command r-exec-command)
                     (with-current-buffer
                         "*Shell Command Output*"
                       (append (buffer-substring-no-properties
                                (point-min) (point-max)))))))
    (kill-buffer "*Shell Command Output*")
    (if (string-match "^Error" r-output)
        (error r-output)
      (let* ((string-start (string-match "\"" r-output))
             (string-end (string-match "\"" r-output (+ 1 string-start))))
        (substring r-output (+ 1 string-start) string-end)))))

(defun r-ts-mode--r-package-binary-path-unix (package-path)
  "Search within `PACKAGE-PATH' /libs/ for treesitter.r.so"
  (let ((full-path (format "%s/libs/treesitter.r.so" package-path)))
    (if (not (file-exists-p full-path))
      (error "File not found: %s" full-path)
      full-path)))

(defun r-ts-mode--r-package-binary-path-win (package-path)
  "Search within `PACKAGE-PATH' /libs/ and /libs/x64/ for treesitter.r.dll"
  (let* ((rts-lib-path (format "%s/libs/" package-path))
       (potential-paths (mapcar (lambda (x) (format "%s%s" rts-lib-path x))
                          '("/treesitter.r.dll" "/x64/treesitter.r.dll"))))
    (if-let (file-path (remq nil (mapcar (lambda (x) (when (file-exists-p x) x)) potential-paths)))
      (car file-path)
      (error "File treesitter.r.dll not found at %s. %s %s"
             package-path
             "If you are sure it is somewhere there"
             "report this issue."))))

(defun r-ts-mode--r-package-binary-path (package-path)
  "Search for the compiled library, treesitter.r, at the potential
locations within `PACKAGE-PATH'"
  (if (eq system-type 'windows-nt)
      (r-ts-mode--r-package-binary-path-win package-path)
    (r-ts-mode--r-package-binary-path-unix package-path)))

(defun r-ts-mode--prepare-emacs-path (path)
  "Ensure that PATH exists and is accessible to emacs."
  (let ((path (file-name-as-directory path)))
    (when (not (file-exists-p path))
      (if (string-equal "~/.emacs.d/tree-sitter/" path)
          (make-directory path nil)
        (error "Path not found: %s" path)))))

;;;###autoload
(defun r-ts-mode-prepare-binaries-from-r-library (&optional package-path emacs-ts-path)
  "Attempt to copy the tree sitter binaries from the R package 'treesitter.r' to
EMACS-TS-PATH or '~/.emacs.d/tree-sitter/' by searching in the default
directories where R installed the library or in PACKAGE-PATH."
  (interactive)
  (let* ((binary-ext (if (eq system-type 'windows-nt) "dll" "so"))
         (emacs-treesitter-path (file-name-as-directory
                                 (or emacs-ts-path "~/.emacs.d/tree-sitter/")))
         (p-path (or package-path (r-ts-mode--find-treesitter-r-package-path)))
         (binary-path (r-ts-mode--r-package-binary-path p-path)))
    (r-ts-mode--prepare-emacs-path emacs-treesitter-path)
    (copy-file
     binary-path
     (format "%slibtree-sitter-r.%s" emacs-treesitter-path binary-ext))))


;;; ----- Utils
(defun r-ts-mode--argument-function-name ()
  "When point is at a node 'argument' or 'arguments', returns the function name."
  (let* ((node-atp (treesit-node-parent (treesit-node-at (point))))
   (potential-call-node (treesit-node-parent node-atp))
   (call-node (pcase (treesit-node-type node-atp)
          ("arguments" potential-call-node)
          ("argument" (treesit-node-parent potential-call-node)))))
    (when call-node
      (treesit-node-text (treesit-node-child-by-field-name call-node "function") t))))

(defun r-ts-mode--alist-buffer-functions (buffer-or-name)
  "Generates an alist of the form (function-name . position) for all the
function definitions found in BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    (let* ((positions '())
     (ts-query (treesit-query-compile 'r '((function_definition name: "function" @val))))
    (range-alist (treesit-query-range 'r ts-query))
    (ranges (mapcar #'car range-alist)))
      (when ranges
  (dolist (pos ranges)
    (let* ((parent-node (treesit-node-parent (treesit-node-parent (treesit-node-at pos)))))
      (when (treesit-node-match-p parent-node "binary_operator")
        (push
         (cons (treesit-node-text (treesit-node-child parent-node -3) t) pos)
         positions))))
  positions))))

(defun r-ts-mode--recursively-match-node-type (node type)
  "Check if NODE matches TYPE and return it. If not, check recursively the
parents, until reaching the 'program' node."
  (cond
   ((treesit-node-match-p node type) node)
   ((and (not (string-equal type "program"))
         (treesit-node-match-p node "program")) nil)
   (t (r-ts-mode--recursively-match-node-type (treesit-node-parent node) type))))

(defun r-ts-mode--inside-fun-def? ()
  "Retun true if cursor is inside a function definition, including function name
and assignment symbol."
  (let ((node-ap (treesit-node-at (point)))
        (type "function_definition"))
    (when
        (or (r-ts-mode--recursively-match-node-type node-ap type)
            (treesit-node-match-p (treesit-node-next-sibling (treesit-node-next-sibling node-ap)) type)
            (treesit-node-match-p (treesit-node-next-sibling node-ap) type))
      t)))

(defun r-ts-mode--is-fun-def (node)
  "Is NODE a function_definition?"
  (treesit-node-match-p (treesit-node-child node -1) "function_definition"))

(defun r-ts-mode--defun-name (node)
  "Return the defun name of NODE.

Return nil if there is no name or if NODE is not a defun node."
  (let ((get-node-text-at-point
       (lambda () (treesit-node-text (treesit-node-at (point)) t))))
    (when (r-ts-mode--is-fun-def node)
      (pcase (treesit-node-type node)
      ("binary_operator"
       (treesit-node-text (treesit-node-child node -3) t))))))

(defun r-ts-mode--is-simple-object (node)
  "Is NODE a binary_operator which is not function_definition?"
  (and
   (treesit-node-match-p node "binary_operator")
   (string-match-p (regexp-opt '("<-" "=")) (treesit-node-text (treesit-node-child node -2) t))
   (not (treesit-node-match-p (treesit-node-child node -1) "function_definition"))
   (not (treesit-parent-until node "function_definition"))))

(defun r-ts-mode--object-name (node)
  "Return the name of the R object generated at NODE with '<-' or '=' if it is
not a function."
  (when (treesit-node-match-p node "binary_operator")
    (treesit-node-text (treesit-node-child node -3) t)))


;; ----- Treesitter settings
(defvar r-ts-mode--operators
  '("?" ":=" "=" "<-" "<<-" "->" "->>"
    "~" "|>" "||" "|" "&&" "&"
    "<" "<=" ">" ">=" "==" "!="
    "+" "-" "*" "/" "::" ":::"
    "**" "^" "$" "@" ":"
    "special")
  "R operators to support tree sitter.")

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
     (arguments (argument name: (identifier) @r-ts-mode-face-variable)))

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
                   (argument name: (identifier) @arg-name
                         (:match "\\`class\\'" @arg-name)
                         value: (_) @r-ts-mode-face-type))
         (:match "\\`structure\\'" @fn-name)))

   :feature 'error
   :override t
   '((ERROR) @r-ts-mode-face-warning))
  "R tree sitter settings.")


;;; ----- Roxygen
(defun r-ts-mode-roxygen-generate-keywords ()
  "Generate a list of keywords suitable for `font-lock-add-keywords'."
  (setq-local r-ts-mode-roxygen-font-lock-keywords
              `(
		        (,(concat r-ts-mode-roxygen--initial-regex ".*")
		         (0 'font-lock-doc-face prepend))
		        (,(concat r-ts-mode-roxygen--initial-regex " *\\([@\\]"
                          (regexp-opt r-ts-mode-roxygen-tags-param t)
                          "\\)\\>")
                 (1 'font-lock-keyword-face prepend))
                (,(concat r-ts-mode-roxygen--initial-regex " *\\(@"
                          (regexp-opt '("param" "importFrom" "importClassesFrom"
                                        "importMethodsFrom" "describeIn")
                                      'words)
			              "\\)\\(?:[ \t]+\\(" r-ts-mode-roxygen--param-name-regexp "\\)\\)")
                 (1 'font-lock-keyword-face prepend)
                 (3 'font-lock-variable-name-face prepend))
                (,(concat "[@\\]" (regexp-opt r-ts-mode-roxygen-tags-noparam t) "\\>")
                 (0 'font-lock-variable-name-face prepend))
                (,(concat r-ts-mode-roxygen--initial-regex)
                 (0 'bold prepend)))))


;; ----- MAJOR MODE DEFINITION
(defvar r-ts-mode--indent-rules
  `((r
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((parent-is "binary_operator") parent-bol r-ts-mode-indent-level)
     ((parent-is "braced_expression") parent-bol r-ts-mode-indent-level)
     ((parent-is "arguments") parent-bol r-ts-mode-indent-level)
     ((parent-is "parameters") parent-bol r-ts-mode-indent-level)))
  "Tree sitter indentation rules.")

(defvar r-ts-mode--thing-settings
  `((r
     (sentence (or "binary_operator" "function" "call"))
     (comment "comment")
     (text (or "comment" "string"))))
  "Thing settings for tree sitter.")

(defvar r-ts-mode--imenu-settings
  `(("Function" "binary_operator" r-ts-mode--is-fun-def r-ts-mode--defun-name)
    ("Object" "binary_operator" r-ts-mode--is-simple-object r-ts-mode--object-name))
  "Imenu settings for tree sitter.")

;;;###autoload
(define-derived-mode r-ts-mode r-ts-mode-parent-mode "R"
  "Major mode for editing R code using tree sitter"
  :group 'r-ts-mode

  (unless (treesit-ready-p 'r)
    (error "Tree-sitter for R is not available"))

  (treesit-parser-create 'r)

  ;; Debug
  (when r-ts-mode--debug
    (setq-local treesit--indent-verbose t)
    (setq-local treesit--font-lock-verbose t)
    (treesit-inspect-mode))

  ;; Font-lock
  (setq-local treesit-font-lock-feature-list
	          '(( comment)
		        ( operator string repeat)
		        ( punctuation-bracket boolean conditional function function-call
		        keyword number constant-builtin variable)
		        ( punctuation-delimiter string-escape variable-parameter error
		          namespace keyword-function type)))
  ;; TODO: Make below as defcustom
  (setq-local treesit-font-lock-settings r-ts-mode-settings)  

  ;; Navigation
  (setq-local treesit-defun-type-regexp
	      (cons (rx "binary_operator")
		    #'r-ts-mode--is-fun-def))
  (setq-local treesit-defun-name-function #'r-ts-mode--defun-name)
  ;; (setq-local treesit-thing-settings r-ts-mode--thing-settings)

  ;; Indent
  (setq-local treesit-simple-indent-rules r-ts-mode--indent-rules)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings r-ts-mode--imenu-settings)

  ;; Finalize
  (treesit-major-mode-setup)
  (r-ts-mode-roxygen-mode))

;;;###autoload
(defalias 'R-ts-mode #'r-ts-mode)

;;;###autoload
(define-minor-mode r-ts-mode-roxygen-mode
  "Minor mode for editing Roxygen documentation."
  ;; :keymap r-ts-mode-roxygen-mode-map
  :init-value nil
  (if r-ts-mode-roxygen-mode
      ;; Turn on `r-ts-mode-roxygen-mode'
      (progn
        (font-lock-add-keywords nil (r-ts-mode-roxygen-generate-keywords))
	    (add-hook 'completion-at-point-functions #'r-ts-mode-roxygen-complete-tag nil t))
    ;; Turn off `r-ts-mode-roxygen-mode'
    (font-lock-remove-keywords nil r-ts-mode-roxygen-font-lock-keywords))
  ;; Regardless of turning on or off we need to re-fontify the buffer:
  (when font-lock-mode
    (font-lock-flush)))

;;; ----- Other Utils: Navigation
(defun r-ts-mode--walk-to-definition (backwards)
  "Go to the next object definition by binary_operator. If BACKWARDS, walk
backwards."
  (treesit-search-forward-goto
   (treesit-node-at (point))
   #'r-ts-mode--is-simple-object
   (point) backwards nil))

(defun r-ts-mode-goto-previous-definition ()
  "Go to previous R object definition generated by '<-' or '='."
  (interactive)
  (r-ts-mode--walk-to-definition t))

(defun r-ts-mode-goto-next-definition ()
  "Go to next R object definition generated by '<-' or '='."
  (interactive)
  (r-ts-mode--walk-to-definition nil))


(provide 'r-ts-mode)
