;;; poke-mode.el --- Major mode for editing Poke programs  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Free Software Foundation, Inc.

;; SMIE grammar and help from Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 3.1
;; Author: Aurelien Aptel <aaptel@suse.com>
;; Maintainer: Jose E. Marchesi <jemarch@gnu.org>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A major mode for editing Poke programs.

;; TODO: xref source for goto def/list refs
;; TODO: be smarter about types vs identifiers (e.g. offset)
;; TODO: highlight user-defined types/funcs/var/units

;;; Code:

(require 'smie)

(defgroup poke-mode nil
  "Poke PK (pickle) editing mode."
  :group 'languages)

(defface poke-unit
  '((t (:inherit font-lock-constant-face)))
  "Face used to highlight units (#unit).")

(defface poke-attribute
  '((t (:inherit font-lock-builtin-face)))
  "Face used to highlight attributes (var'attribute).")

(defface poke-type
  '((t (:inherit font-lock-type-face)))
  "Face used to highlight builtin types.")

(defface poke-type-constructor-integral
  '((t (:inherit font-lock-type-face)))
  "Face used to highlight type constructors.")

(defface poke-type-constructor-offset
  '((t (:inherit poke-unit)))
  "Face used to highlight offset type constructors.")

(defface poke-function
  '((t (:inherit font-lock-function-name-face)))
  "Face used to highlight builtin functions.")

(defface poke-constant
  '((t (:inherit font-lock-constant-face)))
  "Face used to highlight builtin constants.")

(defface poke-exception
  '((t (:inherit error)))
  "Face used to highlight builtin exceptions.")

(defface poke-declaration-name
  '((t (:inherit font-lock-variable-name-face)))
  "Face used to highlight declaration names.")

(defface poke-field-name
  '((t (:inherit font-lock-variable-name-face)))
  "Face used to highlight field names.")

;; from libpoke/pkl-lex.l
(defconst poke-keywords
  '("pinned" "struct" "union" "else" "while" "until" "for" "in" "where" "if"
    "sizeof" "fun" "method" "type" "var" "unit" "break" "return"
    "as" "try" "catch" "raise" "any" "print" "printf" "isa"
    "unmap" "big" "little" "load" "computed")
  "List of the main keywords of the Poke language.")

;; from libpoke/pkl-lex.l
;; from perl -nE 'say qq{"$1"} if /^type (\S+)/' libpoke/*.pk
(defconst poke-builtin-types
  '("string" "void" "int" "uint"
    "Exception"
    "bit" "nibble" "byte" "char"
    "ushort" "short"
    "ulong" "long"
    "uint8" "uint16" "uint32" "uint64"
    "int8" "int16" "int32" "int64"
    "off64" "uoff64"
    "Comparator"
    "POSIX_Time32" "POSIX_Time64")
  "List of Poke builtin types.")

;; from perl -nE 'say qq{"$1"} if /^fun (\S+)/ && $1 !~ /_pkl/' libpoke/*.pk
(defconst poke-builtin-functions
  '("rand" "get_endian" "set_endian" "get_ios" "set_ios" "open"
    "close" "iosize" "getenv" "exit"
    "catos" "stoca" "atoi" "strchr" "ltrim" "rtrim" "qsort"
    "crc32" "ptime")
  "List of Poke builtin functions.")

;; from perl -nE 'say qq{"$1"} if /^var (\S+)/ && $1 !~ /^EC?_/' libpoke/*.pk
(defconst poke-builtin-constants
  '("ENDIAN_LITTLE" "ENDIAN_BIG"
    "IOS_F_READ" "IOS_F_WRITE" "IOS_F_CREATE"
    "IOS_M_RDONLY" "IOS_M_WRONLY" "IOS_M_RDWR"
    "load_path" "NULL")
  "List of Poke builtin constants and variables.")

;; from perl -nE 'say qq{"$1"} if /var (EC?_\S+)/' libpoke/*.pk
(defconst poke-builtin-exceptions
  '("EC_generic" "EC_div_by_zero" "EC_no_ios" "EC_no_return" "EC_out_of_bounds"
    "EC_map_bounds" "EC_eof" "EC_map" "EC_conv" "EC_elem" "EC_constraint"
    "EC_io" "EC_signal"  "EC_io_flags" "EC_inval" "EC_exit" "E_generic"
    "E_div_by_zero" "E_no_ios" "E_no_return" "E_out_of_bounds" "E_map_bounds"
    "E_eof" "E_map" "E_conv" "E_elem" "E_constraint" "E_io" "E_signal"
    "E_io_flags" "E_inval" "E_exit")
  "List of Poke builtin exceptions.")

(defvar poke-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c /") #'smie-close-block)
    map)
  "Keymap used in `poke-mode'.")

(defvar poke-mode-syntax-table
  ;; FIXME: Try and recognize <...> as parens, via `syntax-propertize'.
  (let ((st (make-syntax-table)))
    ;; symbol
    (modify-syntax-entry ?_  "_" st)
    ;; escape
    (modify-syntax-entry ?\\ "\\" st)
    ;; punctuation
    (modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?%  "." st)
    (modify-syntax-entry ?<  "." st)
    (modify-syntax-entry ?>  "." st)
    (modify-syntax-entry ?&  "." st)
    (modify-syntax-entry ?|  "." st)
    (modify-syntax-entry ?@  "." st)
    (modify-syntax-entry ?\240 "." st)
    ;; string
    (modify-syntax-entry ?\" "\"" st)
    ;; // and /* */ comments
    (modify-syntax-entry ?\/ ". 124b" st)
    (modify-syntax-entry ?\* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    ;; balenced punct {} []
    ;; (parens are already done from inherited standard syntax)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    st))

(defconst poke-font-lock-keywords
  (list
   ;; shebang
   `(,(rx bol "#!" (* nonl) eol)
     0 'font-lock-comment-face)
   ;; units
   `(,(rx "#" (group (or (+ (any "_0123456789"))
			 (: "0b" (+ (any "_01")))
			 (: "0o" (+ (any "_01234567")))
			 (: "0x" (+ (any "_0123456789abdcdefABCDEF")))
			 (: (any "A-Z" "a-z" "_") (* (any "A-Z" "a-z" "0-9" "_"))))))
     0 'poke-unit)
   ;; char
   `(,(rx "'" (or nonl
		  (: "\\" (any "ntr\\"))
		  (: "\\" (repeat 1 3 (any "01234567"))))
	  "'")
     0 'font-lock-string-face)
   ;; Names of declared entities
   `(,(rx (regexp (regexp-opt '("type" "unit" "fun" "var" "method")))
          (+ space)
          (group (any "A-Z" "a-z" "_") (* (any "A-Z" "a-z" "_" "0-9")))
          (+ space)
          "=")
     1 'poke-declaration-name)
   ;; This one is to cover the case of chained declarations of the form
   ;;   var foo = 10, bar = 20,
   ;;       baz = 30,
   `(,(rx (group (any "A-Z" "a-z" "_") (* (any "A-Z" "a-z" "_" "0-9")))
          (* space)
          "="
          (+? anychar)
          "," (* space) eol)
     1 'poke-declaration-name)
   ;; Field names
   `(,(rx (any "A-Z" "a-z" "_" "<" ">" "," "[" "]") (* (any "A-Z" "a-z" "_" "0-9"))
          (+ space)
          (group (any "A-Z" "a-z" "_") (* (any "A-Z" "a-z" "_" "0-9")))
          (* space)
          (opt (or ":" "@") (+? anychar))
          ";"
          (* space)
          eol)
     1 'poke-field-name)
   ;; attributes
   `(,(rx "'" (group (any "A-Z" "a-z" "_") (* (any "A-Z" "a-z" "0-9" "_"))))
     0 'poke-attribute)
   ;; keywords
   `(,(rx symbol-start (regexp (regexp-opt poke-keywords)) symbol-end)
     0 'font-lock-keyword-face)
   ;; offset type constructors
   `(,(rx "offset<"
          (* space)
          (+ (not ","))
          (* space)
          ","
          (* space)
          (+ (any "A-Z" "a-z" "_" "0-9"))
          (* space)
          ">")
     0 'poke-type-constructor-offset)
   ;; integral type constructors
   `(,(rx (regexp (regexp-opt '("int" "uint"))) "<"
          (* space) (+ (any "0-9")) (* space) ">")
     (0 'poke-type-constructor-integral t))
   ;; builtin types
   `(,(rx symbol-start (regexp (regexp-opt poke-builtin-types)) symbol-end)
     0 'poke-type)
   ;; builtin functions
   `(,(rx symbol-start (regexp (regexp-opt poke-builtin-functions)) symbol-end)
     0 'poke-function)
   ;; builtin constants
   `(,(rx symbol-start (regexp (regexp-opt poke-builtin-constants)) symbol-end)
     0 'poke-constant)
   ;; builtin exceptions
   `(,(rx symbol-start (regexp (regexp-opt poke-builtin-exceptions)) symbol-end)
     0 'poke-exception)))

;;;; SMIE (indentation and navigation)

(defcustom poke-indent-basic 2
  "Basic indentation step."
  :type 'integer)

(defvar poke-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      ;; defFOOs
      (deffun
        ("fun" id "=" id ":" stmt))
      (def
       ("var" id "=" decls)
       ("type" id "=" decls)
       ("unit" id "=" decls)
       ("method" id "=" decls))
      (defs (defs ";" defs) (def))
      ;; Declarations
      (decl
       ("struct" comp-stmt)
       ("union" comp-stmt)
       (exp))
      (decls (decls "," decls) (decl))
      ;; Expressions
      (exp ("[" exps "]")
           ("(" exps ")")
           (exp "+" exp)
           (exp "-" exp))
      (exps (exps "," exps) (exp))
      ;; Statements
      (comp-stmt ("{" stmts "}"))
      (stmt (comp-stmt)
            (id "=" exp)
            ("return" exp)
            ("else" exp-if)
            ("while" "while-(" exp "while-)" stmt)
            ("for" "for-(" stmts "for-)")
            ("for" "for-(" exp "in" exp "for-)")
            ("try" stmt "catch" comp-stmt)
            ("try" stmt "catch" "catch-(" exp "catch-)" comp-stmt)
            (exp))
      (stmts (stmts ";" stmts) (stmt))
      (exp-if ("if" "if-(" exp "if-)" stmt)))
    '((assoc ";") (assoc ",")
      (assoc "+") (assoc "-")))))

(defun poke--smie-forward-token ()
  "Move one token ahead in a Poke buffer."
  (let ((case-fold-search nil)
        (token (smie-default-forward-token))
	(lbp (line-beginning-position)))
    (cond
     ;; Handle if-(, while-(, for-(, catch-(
     ((and (or (equal token "") (not token)) (looking-at "("))
      (cond
       ((looking-back "if[ \t\n]*" lbp)
        (forward-char 1)
        "if-(")
       ((looking-back "while[ \t\n]*" lbp)
        (forward-char 1)
        "while-(")
       ((looking-back "for[ \t\n]*" lbp)
        (forward-char 1)
        "for-(")
       ((looking-back "catch[ \t\n]*" lbp)
        (forward-char 1)
        "catch-(")
       (t
        nil)))
     ;; Handle if-), while-), for-), catch-)
     ((and (or (equal token "") (not token)) (looking-at ")"))
      (condition-case nil
          (let ((entity (save-excursion
                          (forward-char 1)
                          (let ((forward-sexp-function nil))
                            (backward-sexp))
                          (cond
                           ((looking-back "if[ \t\n]*" lbp)
                            "if-)")
                           ((looking-back "while[ \t\n]*" lbp)
                            "while-)")
                           ((looking-back "for[ \t\n]*" lbp)
                            "for-)")
                           ((looking-back "catch[ \t\n]*" lbp)
                            "catch-)")
                           (t
                            nil)))))
            (when entity
              (forward-char 1)
              entity))
        (scan-error
         nil)))
     (t
      token))))

(defun poke--smie-backward-token ()
  "Move back one token in a Poke buffer."
  (forward-comment (- (point)))
  (cond
   ;; Don't merge ":" or ";" with some preceding punctuation such as ">".
   ((memq (char-before) '(?: ?\;))
    (forward-char -1)
    (string (char-after)))
   (t
    (let ((case-fold-search nil)
          (token (smie-default-backward-token))
	  (lbp (line-beginning-position)))
      (cond
       ;; Handle if-(
       ((and (or (equal token "") (not token)) (looking-back "if[ \t\n]*(" lbp))
        (forward-char -1)
        "if-(")
       ;; Handle while-(
       ((and (or (equal token "") (not token)) (looking-back "while[ \t\n]*(" lbp))
        (forward-char -1)
        "while-(")
       ;; Handle for-(
       ((and (or (equal token "") (not token)) (looking-back "for[ \t\n]*(" lbp))
        (forward-char -1)
        "for-(")
       ;; Handle catch-(
       ((and (or (equal token "") (not token)) (looking-back "catch[ \t\n]*(" lbp))
        (forward-char -1)
        "catch-(")
       ;; Handle if-), while-), for-), catch-)
       ((and (or (equal token "") (not token)) (looking-back ")" lbp))
        (condition-case nil
            (let ((entity (save-excursion
                            (let ((forward-sexp-function nil))
                              (backward-sexp))
                            (cond
                             ((looking-back "if[ \t\n]*" lbp)
                              "if-)")
                             ((looking-back "while[ \t\n]*" lbp)
                              "while-)")
                             ((looking-back "for[ \t\n]*" lbp)
                              "for-)")
                             ((looking-back "catch[ \t\n]*" lbp)
                              "catch-)")
                             (t
                              nil)))))
              (when entity
                (forward-char -1)
                entity))
          (scan-error
           nil)))
       (t
        token))))))

(defun poke-smie-rules (token kind)
  "Provide indentation rules for SMIE.
For more details on TOKEN and KIND see `smie-rules-function'."
  (pcase (cons token kind)
    (`(:elem . basic) poke-indent-basic)
    ;; (`(:list-intro . "=") t)
    (`(:after . "=") poke-indent-basic)
    ((and `(:before . "{") (guard (smie-rule-parent-p "fun" "struct" "union" "method")))
     (smie-rule-parent 0))
    (`(:before . ,(or `"(" `"{"))
     (if (smie-rule-hanging-p) (smie-rule-parent)))
    (`(:before . "if")
     (and (not (smie-rule-bolp)) (smie-rule-prev-p "else")
          (smie-rule-parent)))))


;;;###autoload (add-to-list 'auto-mode-alist '("\\.pk\\'" . poke-mode))
;;;###autoload
(define-derived-mode poke-mode prog-mode "Poke"
  "Major mode for editing Poke programs."

  ;; for comment-region
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")

  (smie-setup poke-smie-grammar #'poke-smie-rules
              :forward-token #'poke--smie-forward-token
              :backward-token #'poke--smie-backward-token)

  ;; font-lock
  (setq-local font-lock-defaults
              '(poke-font-lock-keywords
	        nil ;; do string and comment font-lock from syntax table
	        nil ;; case-sensitive
	        nil)))

(provide 'poke-mode)
;;; poke-mode.el ends here
