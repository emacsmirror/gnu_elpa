;;; a68-mode.el --- Major mode for editing Algol 68 code -*- lexical-binding: t; -*-

;; Copyright (C) 2011, 2024 Jose E. Marchesi
;; Copyright (C) 2021 Omar Polo <op@omarpolo.com>

;; Author: Jose E. Marchesi
;;         Omar Polo <op@omarpolo.com>
;; Maintainer: Omar Polo
;; URL: https://git.omarpolo.com/a68-mode
;; Keywords: languages
;; Version: 0
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

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A major mode for editing Algol 68 code.
;;
;; This is an improved and modernized version of the a68-mode written
;; by Jose E. Marchesi.  The original code was taken from
;;
;; https://github.com/lachrymology/me/blob/master/.emacs.d/extras/algol-mode.el
;;
;; TODO: support quote and dot stropping.

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

(defvar a68-mode-hook '()
  "Hook run when entering Algol68 mode.")

(defvar a68-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") #'newline-and-indent)
    ;; (define-key map (kbd "RET") #'a68-electric-terminate-line)
    map)
  "Keymap for Algol 68 major mode.")

(defconst a68-font-lock-keywords
  (list
   (cons (rx word-start
             (or "DECS" "PROGRAM" "CONTEXT" "USE" "FINISH" "KEEP"
                 "ALIEN"
                 "MODE" "OP" "PRIO" "PROC"
                 "OF" "AT" "IS" "ISNT" "EMPTY" "SKIP"
                 "PR" "PRAGMAT"
                 "CASE" "IN" "OUSE" "OUT" "ESAC"
                 "FOR" "FORALL" "FROM" "TO" "BY" "WHILE" "DO" "OD"
                 "IF" "THEN" "ELIF" "THEN" "ELSE" "FI"
                 "PAR" "BEGIN" "END" "GOTO" "EXIT"
                 "LWB" "UPB" "NOT" "ABS" "BIN" "REPR" "LENG"
                 "SHORTEN" "ODD" "SIGN" "ROUND" "ENTIER" "AND" "OR"
                 "THEF" "ANDF" "ANDTH"
                 "ELSF" "ORF" "OREL"
                 "DIV" "OVER" "MOD" "ELEM" "SHL" "SHR" "OVERAB" "DIVAB" "MODAB"
                 "REF")
             word-end)
         'font-lock-keyword-face)
   (cons (rx word-start
             (or "TRUE" "FALSE")
             word-end)
         'font-lock-constant-face)
   ;; only valid for bold stropping
   (cons (concat "\\<[A-Z]+\\>") 'font-lock-type-face)
   (cons "\\('\\w*'\\)"
         'font-lock-variable-name-face))
  "Highlighting expressions for Algol 68 mode.")

(defvar a68--keywords-regexp
  (regexp-opt '("+" "*" ";" ">" "<" ":=" "=" "," ":")))

(defvar a68--smie-grammar
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
                           ("BEGIN" exp "END"))
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
                      ;; TODO: this don't cover all the loop
                      ;; possibilities.
                      (loop ("FOR" exp "FROM" exp "TO" exp "BY" exp
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
                              "ELSIF" exp "THEN" insts "ELSE" insts "FI")
                             ("IF" exp "THEN" insts
                              "ELSIF" exp "THEN" insts
                              "ELSIF" exp "THEN" insts "ELSE" insts "FI")
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

(defun a68--smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) a68-indent-level)
    ;; (`(,_ . ",") (smie-rule-separator kind))
    (`(,_ . ",") (smie-rule-separator kind))
    (`(,_ . ";") (when (smie-rule-parent-p)
                   (smie-rule-parent)))
    (`(:after . ":=") a68-indent-level)
    (`(:after . "=") a68-indent-level)
    (`(:before . ,(or `"BEGIN" '"(")) (when (smie-rule-hanging-p)
                                        (smie-rule-parent)))
    (`(:before . "IF")
     (and (not (smie-rule-bolp))
          (smie-rule-prev-p "ELSE")
          (smie-rule-parent)))))

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
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?# ">" st)
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?, "." st)
    (modify-syntax-entry ?: "." st)
    ;; define parentheses to match
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    st))

(defvar a68-mode-abbrev-table nil
  "Abbreviation table used in `a68-mode' buffers.")

(define-abbrev-table 'a68-mode-abbrev-table
  '())

;;;###autoload
(define-derived-mode a68-mode prog-mode "Algol68"
  "Major mode for editing Alogl68 files."
  :abbrev-table a68-mode-abbrev-table
  (setq-local font-lock-defaults '(a68-font-lock-keywords))
  (smie-setup a68--smie-grammar #'a68--smie-rules
              :forward-token #'a68--smie-forward-token
              :backward-token #'a68--smie-backward-token)
  (setq-local comment-start a68-comment-style)
  (setq-local comment-end a68-comment-style)
  (setq-local syntax-propertize-function
              (syntax-propertize-rules
               ((rx (group bow "COMMENT" eow)
                    (group (*? anychar))
                    (group bow "COMMENT" eow))
                (1 "<")
                (3 ">"))
               ((rx (group bow "CO" eow)
                    (group (*? anychar))
                    (group bow "CO" eow))
                (1 "<")
                (3 ">"))
               ;; a comment is # ... #, but I don't want the
               ;; (eventual) shebang #! to be considered the start of
               ;; the comment.
               ((rx (group "#" (not "!"))
                    (group (*? anychar))
                    (group "#"))
                (1 "<")
                (3 ">")))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.a68\\'" . a68-mode))

(provide 'a68-mode)
;;; a68-mode.el ends here
