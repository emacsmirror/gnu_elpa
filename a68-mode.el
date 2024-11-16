;;; a68-mode.el --- Major mode for editing Algol 68 code -*- lexical-binding: t; -*-

;; Copyright (C) 2011, 2024 Jose E. Marchesi
;; Copyright (C) 2021 Omar Polo <op@omarpolo.com>

;; Author: Jose E. Marchesi
;;         Omar Polo <op@omarpolo.com>
;; Maintainer: Jose E. Marchesi
;; URL: https://git.sr.ht/~jemarch/a68-mode
;; Keywords: languages
;; Version: 0.1
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
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

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
    (define-key map (kbd "#") #'a68-comment-hash)
    ;; (define-key map (kbd "RET") #'a68-electric-terminate-line)
    map)
  "Keymap for Algol 68 major mode.")

(defconst a68-std-modes
  '("SHORT" "LONG" "INT" "REAL" "BITS" "BYTES"
    "COMPLEX" "STRING")
  "List of Algol 68 standard modes and shortety.")

(defconst a68-keywords
  '("DECS" "PROGRAM" "CONTEXT" "USE" "FINISH" "KEEP"
    "ALIEN" "UNTIL"
    "MODE" "OP" "PRIO" "PROC"
    "OF" "AT" "IS" "ISNT" "EMPTY" "SKIP"
    "PR" "PRAGMAT" "STRUCT" "UNION"
    "CASE" "IN" "OUSE" "OUT" "ESAC"
    "FOR" "FORALL" "FROM" "TO" "BY" "WHILE" "DO" "OD"
    "IF" "THEN" "ELIF" "THEN" "ELSE" "FI"
    "PAR" "BEGIN" "END" "GOTO" "EXIT"
    "LWB" "UPB" "ELEMS" "NOT" "ABS" "BIN" "REPR" "LENG"
    "SHORTEN" "ODD" "SIGN" "ROUND" "ENTIER" "AND" "OR" "XOR"
    "THEF" "ANDF" "ANDTH"
    "ELSF" "ORF" "OREL"
    "DIV" "OVER" "MOD" "ELEM" "SHL" "SHR" "OVERAB" "DIVAB" "MODAB"
    "REF" "NIL" "TRUE" "FALSE")
  "List of Algol 68 keywords.")

(defconst a68-font-lock-keywords
  (list
   (cons (rx word-start
             (eval `(or ,@(mapcar (lambda (kw) kw) a68-keywords)))
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

(defun a68-within-string ()
  (nth 3 (syntax-ppss)))

(defun a68-within-comment ()
  (nth 4 (syntax-ppss)))

(defun a68-within-string-or-comment ()
  (nth 8 (syntax-ppss)))

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
                      (exports ("KEEP" fields "FINISH"))
                      (program ("PROGRAM" exp "FINISH"))
                      ;; TODO: this don't cover all the loop
                      ;; possibilities.
                      (loop ("FOR" exp "FROM" exp "TO" exp "BY" exp
                             "DO" exp "OD")
                            ("FOR" exp "FROM" exp "TO" exp
                             "DO" exp "OD")
                            ("FOR" exp "BY" exp "TO" exp
                             "DO" exp "OD")
                            ("-to-" "TO" exp "DO" exp "OD")
                            ("WHILE" exp "DO" exp "OD")
                            ("WHILE" exp "UNTIL" exp "DO" exp "OD")
                            ("-until" "UNTIL" exp "DO" exp "OD"))
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

(defun a68--smie-rules (kind token)
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
    (`(:before . "(")
     (when (smie-rule-hanging-p)
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

(defun a68-comment-hash ()
  "Smart insert a # ... # style comment."
  (interactive)
  (if (a68-within-comment)
      (insert "#")
    (save-excursion
      (insert "#   #"))
    (goto-char (+ (point) 2))))

(defun a68-beginning-of-defun (&optional arg)
  "Algol 68 specific `beginning-of-defun-function'."
  (goto-char (save-excursion
               (while (and (re-search-backward (rx bow (or "PROC" "OP")) nil t)
                           (a68-within-string-or-comment)))
               (point))))

(defun a68-syntax-propertize-function (start end)
  (let ((case-fold-search nil))
    (goto-char start)
    (funcall
     (syntax-propertize-rules
      ;; a comment is # ... #, but I don't want the
      ;; (eventual) shebang #! to be considered the start of
      ;; the comment.
      ((rx (group "#" (not "!"))
           (group (*? anychar))
           (group "#"))
       (1 "<")
       (3 ">"))
      ((rx bow (group "C") "OMMENT" eow
           (*? anychar)
           bow "COMMEN" (group "T") eow)
       (1 "< b")
       (2 "> b"))
      ((rx bow (group "C") "O" eow
           (*? anychar)
           bow "C" (group "O") eow)
       (1 "< c")
       (2 "> c")))
     (point) end)))

;;;###autoload
(define-derived-mode a68-mode prog-mode "Algol68"
  "Major mode for editing Alogl68 files."
  :abbrev-table a68-mode-abbrev-table
  (setq-local font-lock-defaults '(a68-font-lock-keywords))
  (smie-setup a68--smie-grammar #'a68--smie-rules
              :forward-token #'a68--smie-forward-token
              :backward-token #'a68--smie-backward-token)
  (add-hook 'after-change-functions 'a68--after-change-function nil t)
  (setq-local comment-start a68-comment-style)
  (setq-local comment-end a68-comment-style)
  (setq-local beginning-of-defun-function 'a68-beginning-of-defun)
  (setq-local syntax-propertize-function #'a68-syntax-propertize-function))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.a68\\'" . a68-mode))

(provide 'a68-mode)

;;;; Pretty-printing of bold tags (minor mode).

(defface a68-bold-tag-face '((t :inherit font-lock-keyword-face))
  "Face for ALGOL 68 bold tags")

;;;###autoload(defvar a68-pretty-bold-tags-mode nil "Non-nil if A68 pretty print bold tags mode is enabled.")
;;;###autoload
(define-minor-mode a68-pretty-bold-tags-mode
  "Toggle pretty-printing of bold tags in a68-mode."
  :group a68
  (if a68-pretty-bold-tags-mode
      (a68--pretty-print-bold-tags-on)
    (a68--pretty-print-bold-tags-off)))

(defun a68--pretty-print-bold-tags-on ()
  (save-excursion
    (goto-char (point-min))
    (a68--pretty-print-bold-tags (point-min) (point-max))
    (add-hook 'after-change-functions 'a68--after-change-function nil t)))

(defun a68--pretty-print-bold-tags-off ()
  (remove-hook 'after-change-functions 'a68--after-change-function t)
  (save-excursion
    (goto-char (point-min))
    (let (match)
      (while (not (equal (setq match (next-overlay-change (point)))
                         (point-max)))
        (let ((propandmore (get-char-property-and-overlay (point) 'display)))
          (when (cdr propandmore) (delete-overlay (cdr propandmore))))
        (goto-char match)))))

(defun a68--pretty-print-bold-tag ()
  "Pretty-print an ALGOL 68 bold tag."
  (save-excursion
    (unless (or (a68-within-comment)
                (a68-within-string))
      (skip-chars-forward "ABCDEFGHIJKLMNOPQRSTUVWXYZ_")
      (let* ((bold-tag-end (point))
             (bold-tag-begin (save-excursion
                               (skip-chars-backward "ABCDEFGHIJKLMNOPQRSTUVWXYZ_")
                               (point))))
        (let ((replacedtext (downcase (buffer-substring bold-tag-begin bold-tag-end)))
              (overlay (make-overlay bold-tag-begin bold-tag-end)))
          (let ((old-overlay (get-char-property-and-overlay bold-tag-begin 'display)))
            (when (cdr old-overlay) (delete-overlay (cdr old-overlay))))
          (overlay-put overlay 'face 'a68-bold-tag-face)
          (overlay-put overlay 'display replacedtext)
          (overlay-put overlay 'evaporate t))))))

(defun a68--pretty-print-bold-tags (beginning end)
  "Pretty-print ALGOL 68 bold tags in the given region."
  (unless (or (a68-within-comment)
              (a68-within-string))
    (save-excursion
      (goto-char beginning)
      (while (let ((case-fold-search nil))
               (re-search-forward (rx word-start upper (zero-or-more upper) word-end)
                                  nil t))
        (unless (or (a68-within-comment)
                    (a68-within-string))
          (let* ((bold-tag-end (match-end 0))
                 (bold-tag-begin (match-beginning 0)))
            (let ((replacedtext (downcase (buffer-substring bold-tag-begin bold-tag-end)))
                  (overlay (make-overlay bold-tag-begin bold-tag-end)))
              (let ((old-overlay (get-char-property-and-overlay bold-tag-begin 'display)))
                (when (cdr old-overlay) (delete-overlay (cdr old-overlay))))
              (overlay-put overlay 'face 'a68-bold-tag-face)
              (overlay-put overlay 'display replacedtext)
              (overlay-put overlay 'evaporate t))))))))

(defun a68--after-change-function (start stop _len)
  "Save the current buffer and point for the mode's post-command hook."
  (when a68-pretty-bold-tags-mode
    (let* ((pos (point))
           (in-bold-tag-already (get-char-property pos 'display)))
      (save-match-data
        (if (equal _len 0)
            (a68--pretty-print-bold-tag)
          (a68--pretty-print-bold-tags start stop)))
      (when (and (equal _len 0) in-bold-tag-already (backward-char))))))

;;;; Auto-stropping (minor mode).

;;;###autoload(defvar a68-auto-stropping-mode nil "Non-nil if A68 auto stropping mode is enabled.")
;;;###autoload
(define-minor-mode a68-auto-stropping-mode
  "Toggle auto-stropping in a68-mode."
  :group a68
  (if a68-auto-stropping-mode
      (progn
        (a68--collect-modes)
        (run-with-idle-timer 0.5 t #'a68--collect-modes)
        (add-hook 'post-self-insert-hook
                  #'a68--do-auto-stropping 'append 'local))
    (remove-hook 'post-self-insert-hook
                 #'a68--do-auto-stropping)
    (setq a68--mode-indicants nil)))

(defvar a68--mode-indicants
  nil
  "List of mode indicants declared in current buffer.")

(defun a68--collect-modes ()
  "Collect mode-indicants of modes defined in the current buffer
into a68--mode-indicants."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (setq a68--mode-indicants nil)
      (while (re-search-forward (rx bow "MODE" eow
                                    (one-or-more white)
                                    (group (any "A-Z") (zero-or-more (any "A-Z0-9_")))
                                    (zero-or-more white)
                                    "=") nil t)
        (setq a68--mode-indicants
              (cons (buffer-substring-no-properties (match-beginning 1)
                                                    (match-end 1))
                    a68--mode-indicants)))))
  a68--mode-indicants)

(defun a68--do-auto-stropping ()
  (when (or (eq (char-before) ?\s)
            (eq (char-before) ?\n))
    (let (id beginning end)
      (save-excursion
        (goto-char (- (point) 1))
        (when (looking-back (rx bow (group (any "a-z") (zero-or-more (any "a-z0-9_"))))
                            nil t)
          (setq beginning (match-beginning 1))
          (setq end (match-end 1))
          (setq id (upcase (buffer-substring-no-properties beginning end)))
          (when (member id (append a68-std-modes a68-keywords a68--mode-indicants))
            (goto-char end)
            (delete-region beginning end)
            (insert id)))))))

;;; a68-mode.el ends here
