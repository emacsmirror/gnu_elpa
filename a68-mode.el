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

;; This mode uses SMIE in order to implement syntax-driven
;; highlighting and automatic indentation.  SMIE is based on operator
;; precedence grammars, which often makes it difficult to express the
;; syntax of programming languages due to their many restrictions.
;;
;; Fortunately, the parsing of Algol 68 by the means of an operator
;; precedence grammar has been extensively studied by Meertens and van
;; Vliet, and documented in two main works:
;;
;; - "An operator-priority grammar for Algol 68+"
;;   L.G.L.T Meertens & J.C. van Vliet
;;   https://ir.cwi.nl/pub/9325
;;
;; - "Making ALGOL 68+ texts conform to an operator-priority grammar"
;;   L.G.L.T Meertens & J.C. van Vliet
;;   https://ir.cwi.nl/pub/9318
;;
;; The first article provides an operator-priority grammar for the
;; language, and indicates what inserts are necessary in order to
;; comply with the grammar's structural restrictions.  This is the
;; basis of many of the rules in the SMIE grammar used in this file,
;; particularly the tricky cases like loop clauses.
;;
;; The second article provides rules to determine when the several
;; inserts must be inserted by the lexer.  This is the basis of the
;; SMIE lexer used in this file.

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

;;;; Syntax table for the a68-mode.

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
      "pr" "pragmat"
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
    (cons "\\<\\([a-z][a-z]+_?\\)+\\>" ''font-lock-variable-name-face)
    ;; Mode names start with an upper case letter.
    ;; To distinguish from operator indications in highlighting,
    ;; we mandate type faced strings to have at least one
    ;; lower-case letter.
    (cons "\\<\\([A-Z][A-Za-z_]*[a-z][A-Za-z_]*\\)\\>" ''font-lock-type-face)
    (cons "\\<\\([A-Z][A-Z_]*\\)\\>" ''font-lock-keyword-face)))
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

(defun a68--upcase-strings-in-tree (tree)
  "Return a copy of the given tree with all strings replaced
with the equivalent upcased form."
  (cond
   ((listp tree)
    (mapcar (lambda (t) (a68--upcase-strings-in-tree t)) tree))
   ((and (stringp tree) (not (string-match "-.*-" tree)))
    (upcase tree))
   (t
    tree)))

(defconst a68--bnf-grammar
  '((id)
    (ids (id "-anchor-" id))
    (fields (fields "," fields)
            (ids))
    (args ("(" fargs ")"))
    (spec ("(" fargs "):")
          (exp))
    (fargs (fargs "," fargs)
           (exp))
    (specs (specs "," specs)
           (spec))
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
    ;; Enquiry clause:
    ;;  enquiry clause :
    ;;   series.
    (enquiry-clause (insts))
    ;; Choice clauses
    ;;   choice clause :
    ;;     choice start, chooser choice clause, choice finish.
    ;;   chooser choice clause :
    ;;     enquiry clause, alternate choice clause.
    ;;   enquiry clause :
    ;;     series.
    ;;   alternate choice clause :
    ;;     in choice clause, (out choice clause).
    ;;   in choice clause :
    ;;     choice in, in part of choice.
    ;;   in part of choice :
    ;;     serial clause ; case part list proper ; united case part.
    ;;   case part list proper :
    ;;     case part list, and also token, case part.
    ;;   case part list :
    ;;     (case part list, and also token), case part.
    ;;   case part :
    ;;     unit ; united case part.
    ;;   united case part :
    ;;     specification, unit.
    ;;   specification :
    ;;     single declaration brief pack, specification token.
    ;;   single declaration brief pack :
    ;;     brief begin token, single declaration, brief end token.
    ;;   single declaration :
    ;;     declarer, (dectag insert, identifier).
    ;;   out choice clause :
    ;;     choice out, serial clause ;
    ;;     choice again, chooser choice clause.
    (choice-clause ("if" enquiry-clause "then" insts "fi")
                   ("if" enquiry-clause "then" insts "else" insts "fi")
                   ("if" enquiry-clause "then" insts
                    "elif" enquiry-clause "then" insts "fi")
                   ("(" enquiry-clause "|" insts ")")
                   ("(" enquiry-clause "|" insts "|" insts ")")
                   ("(" enquiry-clause "|" insts
                    "|:" enquiry-clause "|" insts ")")
                   ("case" enquiry-clause "in" specs "esac")
                   ("case" enquiry-clause "in" specs "out" insts "esac")
                   ("case" enquiry-clause "in" specs "ouse" insts "esac")
                   ("(" enquiry-clause "|" specs ")")
                   ("(" enquiry-clause "|" specs "|" insts ")")
                   ("(" enquiry-clause "|" specs "|:" insts ")"))
    ;; Loop clauses.
    ;;   loop clause :
    ;;     loop insert, for part, (from part), (by part), (to part), repeating part.
    ;;   for part :
    ;;     (for token, identifier).
    ;;   from part :
    ;;     from token, unit.
    ;;   by part :
    ;;     by token, unit.
    ;;   to part :
    ;;     to token, unit.
    ;;   repeating part :
    ;;     (while part), do part.
    ;;   while part :
    ;;     while token, enquiry clause.
    ;;   do part :
    ;;     do token, serial clause, od token.
    (loop-clause ("for" id "do" exp "od")
                 ("for" id "from" exp "do" exp "od")
                 ("for" id "from" exp "by" exp "do" exp "od")
                 ("for" id "from" exp "by" exp "to" exp "do" exp "od")
                 ("for" id "from" exp "by" exp "to" exp "while" exp "do" exp "od")
                 ("-from-" exp "by" exp "to" exp "while" exp "do" exp "od")
                 ("-from-" exp "by" exp "to" exp "do" exp "od")
                 ("-from-" exp "by" exp "do" exp "od")
                 ("-from-" exp "do" exp "od")
                 ("-by-" exp "to" exp "while" exp "do" exp "od")
                 ("-by-" exp "while" exp "do" exp "od")
                 ("-by-" exp "do" exp "od")
                 ("-to-" exp "while" exp "do" exp "od")
                 ("-to-" exp "do" exp "od")
                 ("-while-" exp "do" exp "od")
                 ("-do-" exp "od"))
    (insts (insts ";" insts)
           (id ":=" exp)
           (op-decl)
           (type-decl)
           (proc-decl)
           (choice-clause)
           (loop-clause)))
  "Algol 68 BNF operator precedence grammar to use with SMIE")

(defvar a68--smie-grammar-upper
  (smie-prec2->grammar
   (smie-bnf->prec2 (a68--upcase-strings-in-tree a68--bnf-grammar)
                    '((assoc "OF" "[")
                      (assoc ";")
                      (assoc "|" "|:")
                      (assoc ","))
                    '((assoc "=" "/" ":=" ":=:" ":/=:"
                             "+" "-" "*" "/")))))

(defvar a68--smie-grammar-supper
  (smie-prec2->grammar
   (smie-bnf->prec2 a68--bnf-grammar
                    '((assoc "of" "[")
                      (assoc ";")
                      (assoc ","))
                    '((assoc "=" "/" ":=" ":=:" ":/=:"
                             "+" "-" "*" "/")))))

;;;; SMIE lexer

(defvar a68--keywords-regexp
  (regexp-opt '("|:" "(" ")" "+" "*" ";" ">" "<" ":=" "=" "," ":" "~")))

(defun a68-at-strong-void-enclosed-clause ()
  "Return whether the point is at the beginning of a VOID enclosed clause."
  (save-excursion
    (forward-comment (- (point)))
    (or
     ;; A VOID enclosed-clause may be preceded by one of the following
     ;; symbols.
     ;;
     ;; Note the following symbols would have also be included if we
     ;; were detecting a SORT MODE enclosed-clause: := :=: :/=: = [
     ;; @ of from by to ) operator.
     (looking-back (regexp-opt '(":" "," ";" "begin" "if" "then" "elif"
                                    "else" "case" "in" "ouse" "out"
                                    "while" "do" "(" "|" "|:" "def" "postlude")))
        ;; tag denotation or mode indication
        (and (looking-back "[A-Z][A-Za-z_]+")
             ;; Given the context at hand, i.e. a bold word followed
             ;; by "from", "to", "by", "while" or "do", we are at the
             ;; beginning of an enclosed clause if we are part of:
             ;;
             ;; - An access-clause: ... access <bold-word> to ...
             ;; - Or a cast:        ... ; <bold-word> to ...
             (save-excursion
               (forward-comment (- (point)))
               (or
                ;; In the case of an access-clause, the
                ;; module-indication is preceded by one of the
                ;; following symbols:
                (looking-back (regexp-opt '("access" "," "pub")))
                ;; The symbols that may precede a cast are the same
                ;; as those that may precede an enclosed-clause, with
                ;; the exception of the close-symbol, mode-indication
                ;; and module-indication.
                (looking-back (regexp-opt '(":" ":=" ":/=:" "=" "," ";" "["
                                            "@" "begin" "if" "then" "elif"
                                            "else" "case" "in" "ouse" "out"
                                            "of" "from" "by" "to" "while"
                                            "do" "(" "|" "def" "postlude")))
                ;; operator, so any nomad or monad.
                (looking-back (regexp-opt '("%" "^" "&" "+" "-" "~" "!" "?"
                                            ">" "<" "/" "=" "*")))))))))

(defun a68-at-post-unit ()
  "Return whether the point is immediately after an unit."
  (save-excursion
    (forward-comment (- (point)))
    (or (looking-back (regexp-opt '("end" "fi" "esac" "]" "nil" "od" ")"
                                    "skip" "~")))
        ;; This cover the end of denotations.
        (looking-back "\\([0-9]+\\|[\"]\\)")
        ;; tags
        (looking-back "\\<[a-z][a-z_]*\\>")
        ;; A bold word finishes an unit if it is part of a generator,
        ;; like in: ... loc <mode-indication> ...
        ;;
        ;; In this case, the set of symbols which may precede the
        ;; mode-indication consists of the symbols "loc" and "heap",
        ;; plus those symbols which may immediately precede a
        ;; mode-indication in an actual-MODE-declarer.
        (and (looking-back "[A-Z][A-Za-z_]+")
             (looking-back (regexp-opt '("loc" "heap"
                                         "ref" ")" "]"
                                         "proc" "flex")))))))

(defun a68--smie-forward-token ()
  (forward-comment (point-max))
  (cond
   ((looking-at "):")
    (goto-char (+ (point) 2))
    "):")
   ;; The symbols "by", "from", "to", "while" and "do" mark the start
   ;; of a loop-clause if they are the first symbol of an
   ;; enclosed-clause, and is thus preceded by a symbol which may
   ;; appear just before an enclosed-clause.
   ;;
   ;; On the other hand, they do not mark the start of a loop-clause
   ;; if they are preceded by symbols that mark the end of an unit.
   ;;
   ;; In case a decisive answer cannot be determined, probably due
   ;; to a syntax error, Meertens and van Vliet decided to assume
   ;; the beginning of a loop, provisionally, so it could be
   ;; corrected later by a top-down parser.  We proceed the same way
   ;; here, only our decision is final, be it right or wrong ;)
   ((looking-at "\\<from\\>")
    (cond
     ((a68-at-strong-void-enclosed-clause)
      (goto-char (+ (point) 4))
      "-from-")
     ((a68-at-post-unit)
      (goto-char (+ (point) 4))
      "from")
     (t
      (goto-char (+ (point) 4))
      "-from-")))
   ((looking-at "\\<by\\>")
    (cond
     ((a68-at-strong-void-enclosed-clause)
      (goto-char (+ (point) 2))
      "-by-")
     ((a68-at-post-unit)
      (goto-char (+ (point) 2))
      "by")
     (t
      (goto-char (+ (point) 2))
      "-by-")))
   ((looking-at "\\<to\\>")
    (cond
     ((a68-at-strong-void-enclosed-clause)
      (goto-char (+ (point) 2))
      "-to-")
     ((a68-at-post-unit)
      (goto-char (+ (point) 2))
      "to")
     (t
      (goto-char (+ (point) 2))
      "-to-")))
   ((looking-at "\\<while\\>")
    (cond
     ((a68-at-strong-void-enclosed-clause)
      (goto-char (+ (point) 5))
      "-while-")
     ((a68-at-post-unit)
      (goto-char (+ (point) 5))
      "while")
     (t
      (goto-char (+ (point) 5))
      "-while-")))
   ((looking-at "\\<do\\>")
    (cond
     ((a68-at-strong-void-enclosed-clause)
      (goto-char (+ (point) 2))
      "-do-")
     ((a68-at-post-unit)
      (goto-char (+ (point) 2))
      "do")
     (t
      (goto-char (+ (point) 2))
      "-to-")))
   ;; Keywords.
   ((looking-at a68--keywords-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   ;; Words.
   (t (buffer-substring-no-properties (point)
                                      (progn (skip-syntax-forward "w_")
                                             (point))))))

(defun a68--smie-backward-token ()
  (forward-comment (- (point)))
  (cond
   ((looking-back "):")
    (goto-char (- (point) 2))
    "):")
   ;; See comments in a68--smie-forward-token for an explanation of
   ;; the handling of loop insertions -from- -to- -by- -while-.
   ((looking-back "\\<from\\>")
     (goto-char (- (point) 4))
     (cond
      ((a68-at-strong-void-enclosed-clause)
       "-from-")
      ((a68-at-post-unit)
       "from")
      (t
       "-from-")))
   ((looking-back "\\<by\\>")
    (goto-char (- (point) 2))
    (cond
     ((a68-at-strong-void-enclosed-clause)
      "-by-")
     ((a68-at-post-unit)
      "by")
     (t
      "-by-")))
   ((looking-back "\\<to\\>")
    (goto-char (- (point) 2))
    (cond
     ((a68-at-strong-void-enclosed-clause)
      "-to-")
     ((a68-at-post-unit)
      "to")
     (t
      "-to-")))
   ((looking-back "\\<while\\>")
    (goto-char (- (point) 5))
    (cond
     ((a68-at-strong-void-enclosed-clause)
      "-while-")
     ((a68-at-post-unit)
      "while")
     (t
      "-while-")))
   ((looking-back "\\<do\\>")
    (goto-char (- (point) 2))
    (cond
     ((a68-at-strong-void-enclosed-clause)
      "-do-")
     ((a68-at-post-unit)
      "do")
     (t
      "-do-")))
   ((looking-back a68--keywords-regexp (- (point) 2) t)
    (goto-char (match-beginning 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties (point)
                                      (progn (skip-syntax-backward "w_")
                                             (point))))))

;;;; SMIE indentation rules.

(defun a68--smie-rules-upper (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) a68-indent-level)
    (`(,_ . ",") (smie-rule-separator kind))
    (`(,_ . ";") (smie-rule-separator kind))
    (`(:after . ":=") a68-indent-level)
    (`(:after . "=") a68-indent-level)
    ;; Since "|" is in the same BNF rule as "(" in choice-clauses,
    ;; SMIE by default aligns it with it.
    (`(:before . "|")
     (if (not smie-rule-sibling-p) 3))
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
    (`(,_ . ",") (smie-rule-separator kind))
    (`(,_ . ";") (smie-rule-separator kind))
    ;; Since "|" is in the same BNF rule as "(" in choice-clauses,
    ;; SMIE by default aligns it with it.
    (`(:before . ,(or "|" "|:"))
     (if (not (smie-rule-sibling-p)) 1))
    (`(:after . ":=") a68-indent-level)
    (`(:after . "=") a68-indent-level)
    (`(:after . "begin") 6)
    (`(:after . "then") 5)
    (`(:after . "elif") 5)
    (`(:after . "case") 5)
    (`(:after . "ouse") 5)
    (`(:after . "out") 4)
    (`(:after . "in") 3)
    (`(:after . "for") 4)
    (`(:after . "do") 3)
    (`(:after . "from") 5)
    (`(:after . "by") 3)
    (`(:after . "to") 3)
    (`(:after . "while") 3)
    (`(:after . "def") 4)
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
