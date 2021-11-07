;;; a68-mode.el --- Major mode for editing Algol 68 code -*- lexical-binding: t; -*-

;; Copyright (C) 2011 Jose E. Marchesi
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

(require 'syntax)
(require 'font-lock)

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
    (define-key map (kbd "RET") #'a68-electric-terminate-line)
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

(defsubst a68-within-string ()
  "Check if inside a string."
  (nth 3 (syntax-ppss)))

(defsubst a68-within-comment ()
  "Check if inside a comment."
  (nth 4 (syntax-ppss)))

;; Indentation rules:
;;
;; - If we are at the beginning of the buffer, or looking at some
;;   indent-0 content, indent to column 0.
;;
;; - If we are currently at an END, ), FI or OD, then de-indent
;;   relative to the previous line.
;;
;; - If we first see and "end line" before our current line,
;;   then we should indent our current line to the same indentation as
;;   the end line.
;;
;; - If we first see a "start line" like IF, then we need to increase
;;   our indentation relative to that start line.
;;
;; - If into a balanced expression, we should indent to the column
;;   where the start of the innermost parenthetical group.
;;
;; - If none of the above apply, then do not indent at all.

(defun a68-indent-line ()
  "Indent current line as Algol 68 code."
  (interactive)
  (let ((case-fold-search nil))
    (save-excursion
      (beginning-of-line)
      (if (nth 1 (syntax-ppss)) ; Check for rule 5
          (let ((offset (save-excursion (goto-char (+ (nth 1 (syntax-ppss)) 1))
                                        (current-column))))
            (indent-line-to offset))
        (if (or (bobp) ; Check for rule 1
                (looking-at "^[ \t]*\\<\\(KEEP\\|FINISH\\|DECS\\|USE\\|PROGRAM\\)\\>"))
            (indent-line-to 0)
          (let ((not-indented t)
                (begin-indent-re "^[ \t]*\\<\\(PAR\\|BEGIN\\|KEEP\\|IF\\|DO\\|ELSE\\|ELIF\\|THEN\\)")
                (deindent-line-re "^[ \t]*\\<\\(END\\|FI\\|OD\\|ELSE\\|ELIF\\)\\>")
                (eqindent-line-re "^[ \t]*\\<\\(THEN\\)\\>")
                (end-line-re "^[ \t]*\\(END\\|FI\\|OD\\)")
                cur-indent)
            (if (looking-at eqindent-line-re)
                (save-excursion
                  (forward-line -1)
                  (setq cur-indent (current-indentation)))
              (if (looking-at deindent-line-re) ; Check for rule 2
                  (progn
                    (save-excursion
                      (forward-line -1)
                      (setq cur-indent (- (current-indentation) a68-indent-level)))
                    (when (< cur-indent 0)
                      (setq cur-indent 0)))
                (save-excursion
                  (while not-indented
                    (forward-line -1)
                    (if (looking-at end-line-re) ; Check for rule 3
                        (progn
                          (setq cur-indent (current-indentation))
                          (setq not-indented nil))
                      ;; Check for rule 4
                      (if (looking-at begin-indent-re)
                          (progn
                            (setq cur-indent (+ (current-indentation) a68-indent-level))
                            (setq not-indented nil))
                        (when (bobp) ; Check for rule 5
                          (setq not-indented nil))))))))
            (if cur-indent
                (indent-line-to cur-indent)
              ;; If we didn't see an indentation hint, then allow no
              ;; indentation.
              (indent-line-to 0)))))))
  (when (< (current-column) (current-indentation))
    (move-to-column (current-indentation))))

(defvar a68-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?# ">" st)
    (modify-syntax-entry ?\\ "." st)
    ;; define parentheses to match
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    st))

(defconst a68-autoindent-lines-re
  (rx word-start
      (or "BEGIN" "END" "ELSE" "ELIF" "DO" "OD" "CASE" "ESAC" "IN" "OUT")
      word-end))

(defun a68-electric-terminate-line ()
  "Terminate line and indent next line."
  (interactive)
  ;; First, check if current line should be indented
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (when (looking-at a68-autoindent-lines-re)
      (a68-indent-line)))
  (delete-horizontal-space) ; Removes triling whitespaces
  (newline)
  ;; Indent next line if we are not in a string
  (unless (a68-within-string)
    (a68-indent-line)))

(defvar a68-mode-abbrev-table nil
  "Abbreviation table used in `a68-mode' buffers.")

(define-abbrev-table 'a68-mode-abbrev-table
  '())

;;;###autoload
(define-derived-mode a68-mode prog-mode "Algol68"
  "Major mode for editing Alogl68 files."
  :abbrev-table a68-mode-abbrev-table
  (setq-local font-lock-defaults '(a68-font-lock-keywords))
  (setq-local indent-line-function #'a68-indent-line)
  (setq-local comment-start a68-comment-style)
  (setq-local comment-end a68-comment-style)
  (setq-local syntax-propertize-function
              (syntax-propertize-rules ((rx (group bow "COMMENT" eow)
                                            (group (*? anychar))
                                            (group bow "COMMENT" eow))
                                        (1 "<")
                                        (3 ">"))
                                       ((rx (group bow "CO" eow)
                                            (group (*? anychar))
                                            (group bow "CO" eow))
                                        (1 "<")
                                        (3 ">"))
                                       ((rx (group "#")
                                            (group (*? anychar))
                                            (group "#"))
                                        (1 "<")
                                        (3 ">")))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.a68\\'" . a68-mode))

(provide 'a68-mode)
;;; a68-mode.el ends here
