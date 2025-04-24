;;; a68-mode.el --- Major mode for editing Algol 68 code -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2025  Free Software Foundation, Inc.

;; Author: Jose E. Marchesi
;;         Omar Polo <op@omarpolo.com>
;; Maintainer: Jose E. Marchesi <jemarch@gnu.org>
;; URL: https://git.sr.ht/~jemarch/a68-mode
;; Keywords: languages
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; A major mode for editing Algol 68 code.

;;; Code:

(require 'font-lock)
(require 'smie)
(require 'syntax)

(eval-when-compile
  (require 'rx))

(defgroup a68 nil
  "Major mode for editing Algol68 code."
  :prefix "a68-"
  :group 'languages)

(defcustom a68-indent-level 3
  "Indentation step for Algol 68."
  :type 'integer
  :safe #'integerp)

(defcustom a68-comment-style "#"
  "Default comment style used by e.g. `comment-dwim'."
  :type '(choice (const "#")
                 (const "CO")
                 (const "COMMENT"))
  :safe #'consp)

(defface a68-string-break-face '((t :inherit font-lock-string-face))
  "Face for printing Algol 64 string breaks.")

;;;; Stuff common to all stroppings

(defvar a68-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") #'newline-and-indent)
    (define-key map (kbd "#") #'a68-comment-hash)
    ;; (define-key map (kbd "RET") #'a68-electric-terminate-line)
    map)
  "Keymap for Algol 68 major mode.")

(defun a68-within-string ()
  (nth 3 (syntax-ppss)))

(defun a68-within-comment ()
  (nth 4 (syntax-ppss)))

(defun a68-within-string-or-comment ()
  (nth 8 (syntax-ppss)))

(defvar a68--keywords-regexp
  (regexp-opt '("+" "*" ";" ">" "<" ":=" "=" "," ":")))

(defun a68--smie-forward-token ()
  (forward-comment (point-max))
  (cond
   ((looking-at a68--keywords-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties (point)
                                      (progn (skip-syntax-forward "w_")
                                             (point))))))

(defun a68--smie-backward-token ()
  (forward-comment (- (point)))
  (cond
   ((looking-back a68--keywords-regexp (- (point) 2) t)
    (goto-char (match-beginning 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties (point)
                                      (progn (skip-syntax-backward "w_")
                                             (point))))))

(defvar a68-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?, "." st)
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?_ "w" st)
    ;; define parentheses to match
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    st))

(defvar a68-mode-abbrev-table nil
  "Abbreviation table used in `a68-mode' buffers.")

(define-abbrev-table 'a68-mode-abbrev-table
  '())

(defun a68-comment-hash ()
  "Smart insert a # ... # style comment."
  (interactive)
  (if (a68-within-string-or-comment)
      (insert "#")
    (save-excursion
      (insert "#   #"))
    (goto-char (+ (point) 2))))

(defun a68--figure-out-stropping-regime ()
  (save-excursion
    (goto-char (point-min))
    (if (let ((case-fold-search nil))
          (and (re-search-forward "\\<pr UPPER pr\\>" nil t)
               (not (a68-within-comment))
               (not (a68-within-string))))
        'upper
      'supper)))

;;;; Lists of keywords and modes.

(eval-and-compile
  ;; Those vars are used during macroexpansion (and hence compilation).

  ;; UPPER stropping.
  (defconst a68-std-modes-upper
    '("SHORT" "LONG" "INT" "REAL" "BITS" "BYTES"
      "COMPL" "STRING" "REF" "FLEX" "VOID")
    "List of Algol 68 standard modes and shortety in UPPER stropping.")

  (defconst a68-keywords-upper
    '("DECS" "PROGRAM" "CONTEXT" "USE" "KEEP"
      "ALIEN" "RE" "IM"
      "MODE" "OP" "PRIO" "PROC"
      "OF" "AT" "IS" "ISNT" "EMPTY" "SKIP"
      "PR" "PRAGMAT" "STRUCT" "UNION"
      "CASE" "IN" "OUSE" "OUT" "ESAC"
      "FOR" "FORALL" "FROM" "TO" "BY" "WHILE" "DO" "OD"
      "EQ" "NE" "LT" "GT" "LE" "GE"
      "IF" "THEN" "ELIF" "THEN" "ELSE" "FI"
      "PAR" "BEGIN" "END" "GOTO" "GO" "TO" "EXIT"
      "LWB" "UPB" "ELEMS" "NOT" "ABS" "BIN" "REPR" "LENG"
      "SHORTEN" "ODD" "SIGN" "ROUND" "ENTIER" "AND" "OR" "XOR"
      "ANDTH" "OREL"
      "DIV" "OVER" "MOD" "ELEM" "SHL" "SHR" "OVERAB" "DIVAB" "MODAB"
      "UP" "DOWN"
      "NIL" "TRUE" "FALSE"
      "MODULE" "DEF" "FED" "POSTLUDE" "ACCESS" "PUB"
      "UNSAFE" "ASSERT")
    "List of Algol 68 keywords in UPPER stropping.")

  ;; SUPPER stropping.
  (defconst a68-std-modes-supper
    '("int" "real" "bool" "char" "format" "void"
      "compl" "bits" "bytes" "string" "sema" "file" "channel")
    "List of Algol 68 standard modes in SUPPER stropping.")

  (defconst a68-keywords-supper
    '("true" "false" "empty" "at"
      "pr" "PR" "pragmat" "PRAGMAT"
      "andth" "orel" "is" "isnt"
      "long" "short" "ref" "loc" "heap" "struct" "flex" "proc"
      "union" "op" "prio" "mode" "begin" "end" "exit" "par" "if"
      "then" "elif" "else" "fi" "case" "in" "ouse" "out" "esac"
      "nil" "of" "go" "goto" "skip" "for" "from" "by" "to" "while"
      "do" "od" "unsafe" "assert")
    "List of Algol 68 keywords in SUPPER stropping."))

;;;; Font-lock keywords.

(defconst a68-font-lock-keywords-common
  (list
   ;; String breaks.  Apostrophe is not (currently) a worthy character
   ;; out of strings, so for now we can just match it anywhere.
   '("\\('[nrft']\\)\\|\\('(.*?)\\)" 0 ''a68-string-break-face prepend)
   '("\\(''\\|[^']\\)\\('[^nrft'(]\\)" 2 ''font-lock-warning-face prepend)
   ;; Two or more consecutive underscore characters are always
   ;; illegal in this stropping regime.
   (cons "_[_]+" ''font-lock-warning-face))
  "Font-lock keywords expressions common to all stropping regimes. ")

(defconst a68-font-lock-keywords-upper
  (append
   a68-font-lock-keywords-common
   (list (cons (rx word-start
                   (eval `(or ,@a68-keywords-upper))
                   word-end)
               ''font-lock-keyword-face)
         (cons (rx word-start
                   (eval `(or ,@a68-std-modes-upper))
                   word-end)
               ''font-lock-type-face)
         (cons (rx word-start
                   (or "TRUE" "FALSE")
                   word-end)
               ''font-lock-constant-face)
         '("\\<\\([A-Z]+[A-Z_]*\\>\\)\\(_+\\)?"
           (1 'font-lock-type-face)
           (2 'font-lock-warning-face nil t))
         (cons "\\('\\w*'\\)"
               ''font-lock-variable-name-face)))
   "Highlighting expressions for Algol 68 mode in UPPER stropping.")

(defconst a68-font-lock-keywords-supper
  (append
   a68-font-lock-keywords-common
   (list
    (cons (rx word-start
              (eval `(or ,@a68-keywords-supper))
              word-end)
          ''font-lock-keyword-face)
    (cons (rx word-start
              (eval `(or ,@a68-std-modes-supper))
              word-end)
          ''font-lock-type-face)
    (cons (rx word-start
              (or "true" "false")
              word-end)
          ''font-lock-constant-face)
    ;; Tags.
    (cons "\\<[a-z]\\([a-z]_\\)*\\>" ''font-lock-variable-name-face)
    ;; By convention operators have only upper-letter names.
    (cons "\\<\\([A-Z]+\\>\\)" ''font-lock-keyword-face)
    ;; Mode names use ThisCase.
    (cons "\\<\\([A-Z][A-Za-z_]*\\>\\)" ''font-lock-type-face)))
   "Highlighting expressions for Algol 68 mode in SUPPER stropping.")

;;;; Syntax-based text properties.

(defun a68-syntax-propertize-function-upper (start end)
  (let ((case-fold-search nil))
    (goto-char start)
    (funcall
     (syntax-propertize-rules
      ((rx (group "#")
           (*? anychar)
           (group "#"))
       (1 (when (not (a68-within-string)) (string-to-syntax "<")))
       (2 (when (not (a68-within-string)) (string-to-syntax ">")))
       (0 (ignore (put-text-property (match-beginning 0) (match-end 0)
                                     'syntax-multiline t))))
      ((rx bow (group "C") "OMMENT" eow
           (*? anychar)
           bow "COMMEN" (group "T") eow)
       (1 (when (not (a68-within-string)) (string-to-syntax "< b")))
       (2 (when (not (a68-within-string)) (string-to-syntax "> b")))
       (0 (ignore (put-text-property (match-beginning 0) (match-end 0)
                                     'syntax-multiline t))))
      ((rx bow (group "C") "O" eow
           (*? anychar)
           bow "C" (group "O") eow)
       (1 (when (not (a68-within-string)) (string-to-syntax "< c")))
       (2 (when (not (a68-within-string)) (string-to-syntax "> c")))
       (0 (ignore (put-text-property (match-beginning 0) (match-end 0)
                                     'syntax-multiline t)))))
     (point) end)))

(defun a68-syntax-propertize-function-supper (start end)
  (let ((case-fold-search nil))
    (goto-char start)
    (funcall
     (syntax-propertize-rules
      ((rx (group "#")
           (*? anychar)
           (group "#"))
       (1 (when (not (a68-within-string)) (string-to-syntax "<")))
       (2 (when (not (a68-within-string)) (string-to-syntax ">")))
       (0 (ignore (put-text-property (match-beginning 0) (match-end 0)
                                     'syntax-multiline t))))
      ((rx bow (group "c") "omment" eow
           (*? anychar)
           bow "commen" (group "t") eow)
       (1 (when (not (a68-within-string)) (string-to-syntax "< b")))
       (2 (when (not (a68-within-string)) (string-to-syntax "> b")))
       (0 (ignore (put-text-property (match-beginning 0) (match-end 0)
                                     'syntax-multiline t))))
      ((rx bow (group "c") "o" eow
           (*? anychar)
           bow "c" (group "o") eow)
       (1 (when (not (a68-within-string)) (string-to-syntax "< c")))
       (2 (when (not (a68-within-string)) (string-to-syntax "> c")))
       (0 (ignore (put-text-property (match-beginning 0) (match-end 0)
                                     'syntax-multiline t)))))
     (point) end)))

;;;; Functions to move up and down a procedure.

(defun a68-beginning-of-defun-upper (&optional count)
  "Algol 68 specific `beginning-of-defun-function' for UPPER stropping."
  (let ((count (or count 1))
        (case-fold-search nil)
        res)
    (while (> count 0)
      (goto-char (save-excursion
                   (while (and (re-search-backward (rx bow (or "PROC" "OP") eow) nil t)
                               (a68-within-string-or-comment)))
                   (setq res (point))))
      (setq count (1- count )))
    res))

(defun a68-beginning-of-defun-supper (&optional count)
  "Algol 68 specific `beginning-of-defun-function' for SUPPER stropping."
  (let ((count (or count 1))
        (case-fold-search nil)
        res)
    (while (> count 0)
      (goto-char (save-excursion
                   (while (and (re-search-backward (rx bow (or "proc" "op") eow) nil t)
                               (a68-within-string-or-comment)))
                   (setq res (point))))
      (setq count (1- count )))
    res))

;;;; SMIE grammar

(defvar a68--smie-grammar-upper
  (smie-prec2->grammar
   (smie-bnf->prec2 '((id)
                      (ids (id "-anchor-" id))
                      (fields (fields "," fields)
                              (ids))
                      (args ("(" fargs ")"))
                      (fargs (fargs "," fargs)
                             (exp))
                      (conformity-cases)
                      (exp (ids)
                           (exp "OF" exp)
                           (exp "[" exp "]")
                           ("(" exp ")")
                           ("BEGIN" exp "END")
                           ("MODULE" exp "DEF" exp "FED")
                           ("MODULE" exp "DEF" exp "POSTLUDE" exp "FED"))
                      (type-decl ("MODE" type-decl*))
                      (type-decl* (type-decl* "," type-decl*)
                                  (id "=" type-decl**))
                      (type-decl** ("STRUCT" args)
                                   ("UNION" args)
                                   ("PROC" args "-archor-" ids))
                      (op-decl (op-decl "," op-decl)
                               ("OP" ids "=" args ids ":" exp))
                      (proc-decl (proc-decl "," proc-decl)
                                 ("OP" ids "=" args ids ":" exp)
                                 ("PROC" ids "=" ids ":" exp))
                      (program ("PROGRAM" exp))
                      ;; TODO: this don't cover all the loop
                      ;; possibilities.
                      (loop ("-do-" "DO" exp "OD")
                            ("FOR" exp "FROM" exp "TO" exp "BY" exp
                             "DO" exp "OD")
                            ("FOR" exp "FROM" exp "TO" exp
                             "DO" exp "OD")
                            ("FOR" exp "BY" exp "TO" exp
                             "DO" exp "OD")
                            ("-to-" "TO" exp "DO" exp "OD")
                            ("WHILE" exp "DO" exp "OD"))
                      (insts (insts ";" insts)
                             (id ":=" exp)
                             ("IF" exp "THEN" insts "FI")
                             ("IF" exp "THEN" insts "ELSE" insts "FI")
                             ("IF" exp "THEN" insts
                              "ELIF" exp "THEN" insts "ELSE" insts "FI")
                             ("IF" exp "THEN" insts
                              "ELIF" exp "THEN" insts
                              "ELIF" exp "THEN" insts "ELSE" insts "FI")
                             ;; TODO OUSE for both case and conformity case
                             ("CASE" exp "IN" fargs "ESAC")
                             ("CASE" exp "IN" conformity-cases "ESAC")
                             ("CASE" exp "IN" fargs "OUT" exp "ESAC")
                             (op-decl)
                             (type-decl)
                             (proc-decl)
                             (loop)))
                    '((assoc "OF" "[")
                      (assoc ";")
                      (assoc "|" "|:")
                      (assoc ","))
                    '((assoc "=" "/" ":=" ":=:" ":/=:"
                             "+" "-" "*" "/")))))

(defvar a68--smie-grammar-supper
  (smie-prec2->grammar
   (smie-bnf->prec2 '((id)
                      (ids (id "-anchor-" id))
                      (fields (fields "," fields)
                              (ids))
                      (args ("(" fargs ")"))
                      (fargs (fargs "," fargs)
                             (exp))
                      (conformity-cases)
                      (exp (ids)
                           (exp "of" exp)
                           (exp "[" exp "]")
                           ("(" exp ")")
                           ("begin" exp "end")
                           ("module" exp "def" exp "fed")
                           ("module" exp "def" exp "postlude" exp "fed"))
                      (type-decl ("mode" type-decl*))
                      (type-decl* (type-decl* "," type-decl*)
                                  (id "=" type-decl**))
                      (type-decl** ("struct" args)
                                   ("union" args)
                                   ("proc" args "-archor-" ids))
                      (op-decl (op-decl "," op-decl)
                               ("op" ids "=" args ids ":" exp))
                      (proc-decl (proc-decl "," proc-decl)
                                 ("op" ids "=" args ids ":" exp)
                                 ("proc" ids "=" ids ":" exp))
                      (program ("program" exp))
                      ;; TODO: this don't cover all the loop
                      ;; possibilities.
                      (loop ("-do-" "do" exp "od")
                            ("for" exp "from" exp "to" exp "by" exp
                             "do" exp "od")
                            ("for" exp "from" exp "to" exp
                             "do" exp "od")
                            ("for" exp "by" exp "to" exp
                             "do" exp "od")
                            ("-to-" "to" exp "do" exp "od")
                            ("while" exp "do" exp "od"))
                      (insts (insts ";" insts)
                             (id ":=" exp)
                             ("if" exp "then" insts "fi")
                             ("if" exp "then" insts "else" insts "fi")
                             ("if" exp "then" insts
                              "elif" exp "then" insts "else" insts "fi")
                             ("if" exp "then" insts
                              "elif" exp "then" insts
                              "elif" exp "then" insts "else" insts "fi")
                             ;; TODO OUSE for both case and conformity case
                             ("case" exp "in" fargs "esac")
                             ("case" exp "in" conformity-cases "esac")
                             ("case" exp "in" fargs "out" exp "esac")
                             (op-decl)
                             (type-decl)
                             (proc-decl)
                             (loop)))
                    '((assoc "of" "[")
                      (assoc ";")
                      (assoc "|" "|:")
                      (assoc ","))
                    '((assoc "=" "/" ":=" ":=:" ":/=:"
                             "+" "-" "*" "/")))))

;;;; SMIE indentation rules.

(defun a68--smie-rules-upper (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) a68-indent-level)
    ;; (`(,_ . ",") (smie-rule-separator kind))
    (`(,_ . ",") (smie-rule-separator kind))
    (`(,_ . ";") (when (smie-rule-parent-p)
                   (smie-rule-parent)))
    (`(:after . ":=") a68-indent-level)
    (`(:after . "=") a68-indent-level)
    (`(:before . "BEGIN")
     (when (or (smie-rule-hanging-p)
               (or
                (and (or (smie-rule-parent-p "PROC")
                         (smie-rule-parent-p "OP"))
                     (smie-rule-prev-p ":"))
                (smie-rule-parent-p "PROGRAM")))
       (smie-rule-parent)))
    (`(:before . "THEN")
     (when (or (smie-rule-hanging-p)
               (smie-rule-parent-p "IF"))
       (smie-rule-parent)))
    (`(:before . "(")
     (when (smie-rule-hanging-p)
       (smie-rule-parent)))
    (`(:before . "IF")
     (and (not (smie-rule-bolp))
          (smie-rule-prev-p "ELSE")
          (smie-rule-parent)))))

(defun a68--smie-rules-supper (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) a68-indent-level)
    ;; (`(,_ . ",") (smie-rule-separator kind))
    (`(,_ . ",") (smie-rule-separator kind))
    (`(,_ . ";") (when (smie-rule-parent-p)
                   (smie-rule-parent)))
    (`(:after . ":=") a68-indent-level)
    (`(:after . "=") a68-indent-level)
    (`(:before . "begin")
     (when (or (smie-rule-hanging-p)
               (or
                (and (or (smie-rule-parent-p "proc")
                         (smie-rule-parent-p "op"))
                     (smie-rule-prev-p ":"))
                (smie-rule-parent-p "program")))
       (smie-rule-parent)))
    (`(:before . "then")
     (when (or (smie-rule-hanging-p)
               (smie-rule-parent-p "if"))
       (smie-rule-parent)))
    (`(:before . "(")
     (when (smie-rule-hanging-p)
       (smie-rule-parent)))
    (`(:before . "if")
     (and (not (smie-rule-bolp))
          (smie-rule-prev-p "else")
          (smie-rule-parent)))))

;;;; Stropping utilities and commands.

(defun a68-supperize-buffer ()
  "Translate code from UPPER stropping to SUPPER stropping."
  (interactive)
  (let* ((keywords (append a68-std-modes-supper
                           a68-keywords-supper))
         (replacements (mapcar (lambda (keyword)
                                 (list keyword
                                       (downcase keyword)))
                               keywords)))
    ;; Apply replacements
    (save-excursion
      (mapcar (lambda (pair)
                (goto-char (point-min))
                (while (re-search-forward (concat "\\<" (car pair) "\\>") nil t)
                  (replace-match (cadr pair) t t)))
              replacements))))

;;;; The major mode.

;;;###autoload
(define-derived-mode a68-mode prog-mode "Algol68"
  "Major mode for editing Alogl68 files."
  :abbrev-table a68-mode-abbrev-table
  ;; First determine the stropping regime
  (setq-local a68--stropping-regime
              (a68--figure-out-stropping-regime))
  (if (equal a68--stropping-regime 'supper)
      ;; SUPPER stropping.
      (progn
        (setq-local font-lock-defaults '(a68-font-lock-keywords-supper))
        (smie-setup a68--smie-grammar-supper #'a68--smie-rules-supper
                    :forward-token #'a68--smie-forward-token
                    :backward-token #'a68--smie-backward-token)
        (setq-local beginning-of-defun-function #'a68-beginning-of-defun-supper)
        (setq-local syntax-propertize-function #'a68-syntax-propertize-function-supper))
    ;; UPPER stropping, the default.
    (setq-local font-lock-defaults '(a68-font-lock-keywords-upper))
    (smie-setup a68--smie-grammar-upper #'a68--smie-rules-upper
                :forward-token #'a68--smie-forward-token
                :backward-token #'a68--smie-backward-token)
    (setq-local beginning-of-defun-function #'a68-beginning-of-defun-upper)
    (setq-local syntax-propertize-function #'a68-syntax-propertize-function-upper))
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-multiline 'append 'local)
  (setq-local comment-start a68-comment-style)
  (setq-local comment-end a68-comment-style))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.a68\\'" . a68-mode))

(provide 'a68-mode)

;;; a68-mode.el ends here
