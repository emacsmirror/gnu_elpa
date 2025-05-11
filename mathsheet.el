;;; mathsheet.el --- Generate dynamic math worksheets  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Ian Martins <ianxm@jhu.edu>
;; Keywords: tools, education, math
;; Homepage: https://gitlab.com/ianxm/mathsheet
;; Version: 1.1
;; Package-Requires: ((peg "1.0")
;;                    (emacs "28.1")
;;                    calc)

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package generates dynamic math worksheets.  The types and
;; distribution of problems is highly customizable.  Problem sets are
;; defined using templates and exported to PDF for printing.

;;; Code:

(require 'forms)
(require 'peg)
(require 'calc)

(declare-function math-read-expr "calc-ext")

(defgroup mathsheet nil
  "Options for customizing Mathsheet."
  :prefix "mathsheet-"
  :group 'applications
  :tag "mathsheet")

(defcustom mathsheet-data-file
  (expand-file-name "mathsheet.dat" user-emacs-directory)
  "Where to store saved mathsheet configurations.

The default is to save them to a file in the private emacs
configuration directory."
  :type 'file
  :group 'mathsheet)

(defcustom mathsheet-output-directory
  (expand-file-name "~")
  "Where to write generated worksheets.

The default is to write the to the home directory."
  :type 'directory
  :group 'mathsheet)

(let ((page '".VM 0 -0.5i                                     \\\" reduce bottom margin
.PH \"'Name: \\l'20\\_'''Date:\\l'10\\_''\"           \\\" header
<<instruction>>
.SP 1
.fam C                                          \\\" set font
<<layout>>
.MC \\n[clen]p 10p                               \\\" start columns mode
<<problems>>
.1C 1                                           \\\" end of columns mode
.BS                                             \\\" floating bottom box
\\l'\\n(.lu'                                      \\\" horizontal rule
.S 8 10                                         \\\" reduce font and vertical space
.ss 6                                           \\\" reduce horizontal space
.gcolor grey                                    \\\" answers color
<<answers>>
.BE
"))
(defvar mathsheet--var-list '()
  "List of variables used within a problem.")

(defconst mathsheet--worksheet-template page
  "Groff template for the worksheet.")

(defconst mathsheet--num-pat (rx string-start (+ num) string-end)
  "Pattern for integers.")

(defvar mathsheet--field-sheet-name nil
  "The form record name field.")

(defvar mathsheet--field-count nil
  "The form record count field.")

(defvar mathsheet--field-cols nil
  "The form record cols field.")

(defvar mathsheet--field-instruction nil
  "The form record instruction field.")

(defvar mathsheet--field-problems nil
  "The form record problems field.")

)

(setq forms-file mathsheet-data-file)

(setq forms-number-of-fields
      (forms-enumerate
       '(mathsheet--field-sheet-name
         mathsheet--field-count
         mathsheet--field-cols
         mathsheet--field-instruction
         mathsheet--field-problems)))

(setq forms-field-sep "||")

(defun mathsheet--new-record-filter (record)
  "Set defaults in new RECORD."
  (aset record 2 "20")                  ; default
  (aset record 3 "2")                   ; default
  (aset record 4 "Find the answer.")    ; default
  (aset record 5 "1 | 1 | ")            ; lay out structure
  record)

(setq forms-new-record-filter 'mathsheet--new-record-filter)

(defun mathsheet--format-templates (record)
  "Format the template rows in RECORD to line up with the header."
  (let ((rows (string-split (aref record 5) "\n"))
        (pat (rx (* space) (group (+ alnum)) (* space) "|"
                 (* space) (group (+ alnum)) (* space) "|"
                 (* space) (group (+ nonl)))))
    (setq rows (mapconcat
                (lambda (row)
                  (string-match pat row)
                  (format "%s | %s | %s"
                          (match-string 1 row)
                          (match-string 2 row)
                          (match-string 3 row)))
                rows
                "\n"))
    (aset record 5 rows))
  record)
(setq forms-modified-record-filter 'mathsheet--format-templates)

(setq forms-format-list
      (list
       "====== Math Sheet Generator ======"
       "\nSee https://gitlab.com/ianxm/mathsheet for details."

       "\n\nThe base-name of the mathsheet file to write, not including extension."
       "\nName: " mathsheet--field-sheet-name

       "\n\nThe total number of problems to put on the sheet."
       "\nCount: " mathsheet--field-count

       "\n\nThe number of columns the sheet should have."
       "\nColumns: " mathsheet--field-cols

       "\n\nThe instruction to give at the top of the sheet."
       "\nInstruction: " mathsheet--field-instruction

       "\n\nThe problem templates from which to generate problems for the sheet."
       "\nOne per line, formatted as \"(w)eight | (o)rder | template\".\n\n"

       "w | o | template\n"
       "--+---+------------------------------------\n"
       mathsheet--field-problems
       "\n"))

(defmacro mathsheet--validate (field-name field-str checks)
  "Add specified checks to validate field input.

FIELD-NAME is the name of the field.  FIELD-STR is the string
value in the record.  CHECKS is a list of symbols specifying
which validation checks to perform."
  (let (ret)
    (dolist (check checks)
      (pcase check
        ('not-null-p
         (push
          `(when (null ,field-str)
             (error (format "`%s' cannot be empty" ,field-name)))
          ret))
        ('is-num-p
         (when (not (null field-str))
           (push
            `(when (not (string-match-p mathsheet--num-pat ,field-str))
               (error (format "`%s' must be a number" ,field-name)))
            ret)))
        (`(in-range-p ,min ,max)
         (push
          `(when
               (or
                (< (string-to-number ,field-str) ,min)
                (> (string-to-number ,field-str) ,max))
             (error (format "`%s' must be between %s and %s, inclusive"
                            ,field-name ,min ,max)))
          ret))
        (_
         (push
          `(error (format "Unknown check: %s" ,check))
          ret))
        ))
    (append '(progn) ret)))

(defun mathsheet--parse (record)
  "Parse all of the fields of the current RECORD into an alist."
  (let (count cols problems)

    (pcase record
      (`(,name ,count-str ,cols-str ,instruction ,problems-str)

       ;; validate the form fields
       (mathsheet--validate "name" name (not-null-p))
       (mathsheet--validate "count" count-str (not-null-p is-num-p (in-range-p 1 50)))
       (mathsheet--validate "cols" cols-str (not-null-p is-num-p (in-range-p 1 4)))
       (mathsheet--validate "problems" problems-str (not-null-p))

       ;; convert the numbers and parse the problems field
       (setq count (string-to-number count-str)
             cols (string-to-number cols-str)
             problems (mapcar           ; parse rows
                       #'mathsheet--parse-problem-row
                       (seq-filter      ; remove possible trailing empty line
                        (lambda (x) (not (string-empty-p x)))
                        (string-split   ; split lines
                         problems-str
                         "\n"))))

       `((:name . ,name)
         (:count . ,count)
         (:cols . ,cols)
         (:instr . ,instruction)
         (:probs .  ,problems)))
      (_ (error "Invalid form data")))))

(defun mathsheet--parse-problem-row (row)
  "Parse one ROW of the problem field into a list."
  (let* ((fields (mapcar                ; trim whitespace
                  #'string-trim
                  (split-string         ; split fields
                   row
                   "|")))
         (weight-str (nth 0 fields))
         (order-str (nth 1 fields))
         (template (nth 2 fields))
         weight order)
    (mathsheet--validate "weight" weight-str (not-null-p is-num-p))
    (mathsheet--validate "order" order-str (not-null-p is-num-p))
    (mathsheet--validate "template" template (not-null-p))
    (setq weight (string-to-number weight-str)
          order (string-to-number order-str))
    (list weight order template)))

(defun mathsheet-generate-sheet ()
  "Generate sheet for current form data."
  (interactive)
  (when (not (string= major-mode "forms-mode"))
    (error "Mathsheet must be open to generate a sheet"))
  (let ((config (mathsheet--parse forms--the-record-list)))
    (let ((problems (mathsheet--generate-problems
                     (alist-get :probs config)
                     (alist-get :count config)))
          ;; absolute path without extension
          (fname (concat
                  (file-name-as-directory mathsheet-output-directory)
                  (string-replace " " "-" (alist-get :name config)))))
      (mathsheet--write-worksheet
       fname
       (alist-get :instr config)
       problems
       (alist-get :cols config))
      (message "Wrote %s problems to %s.pdf"
               (alist-get :count config)
               fname))))

(defun mathsheet--scan-problem ()
  "Scan a problem.

This parses the problem and produces a list containing info about
its fields.  For each field it returns a list containing:
1. a symbol for the assigned variable or a unique placeholder
2. a list of variables this field depends on
3. a cons containing start and end markers for the field in the current buffer
4. nil which is used by `dfs-visit' later"
  (let ((field-index 0)
        open-fields ; stack
        closed-fields ; list
        alg-vars)

    (with-peg-rules
        ((stuff (* (or asn-var math-func alg-var digit symbol field space)))
         (field open (opt assignment) stuff close)
         (space (* [space]))
         (open (region "[")
               `(l _ -- (progn
                          (push (list
                                 (intern (concat "_" (number-to-string field-index))) ; asn-var
                                 nil ; deps
                                 (cons (copy-marker l) nil) ; start and end markers
                                 nil) ; not visited
                                open-fields)
                          (setq field-index (1+ field-index))
                          ".")))
         (assignment (substring letter) "="
                     `(v -- (progn
                              (setcar
                               (car open-fields)
                               (intern v))
                              ".")))
         (asn-var "$" (substring letter)
                  `(v -- (progn
                           (push (intern v) (cadar open-fields))
                           ".")))
         (alg-var (substring letter)
                  `(v -- (progn
                           (push v alg-vars)
                           ".")))
         (close (region "]")
                `(l _ -- (progn
                           (setcdr (caddar open-fields) (copy-marker l t))
                           (when (> (length open-fields) 1) ; add parent to child dependency
                             (push (caar open-fields) (cadadr open-fields)))
                           (push (pop open-fields) closed-fields)
                           ".")))
         (math-func (or "sqrt" "sin" "cos" "tan" "asin" "acos" "atan" "floor" "ceil" "round"))
         (letter [a-z])
         (digit [0-9])
         (symbol (or "." "," "+" "-" "*" "/" "^" "(" ")" "=")))

      (peg-run (peg stuff)
               (lambda (x) (message "Failed %s" x))
               (lambda (x)
                 (funcall x)
                 `((:fields . ,closed-fields)
                   (:alg-vars . ,alg-vars)))))))

(defun mathsheet--reduce-field ()
  "Reduce the field to a number.

Parse the field again, replacing spans with random numbers and
evaluating arithmetic operations.  The field shouldn't have any
internal fields so this should result in a single number.  Return
that number."
  (with-peg-rules
      ((field "[" space (or math-func expression sequence assignment value) space "]")
       (expression (list value space operation space value (* space operation space value))
                   `(vals -- (string-to-number
                              (calc-eval
                               (list
                                (mapconcat
                                 (lambda (x) (if (numberp x) (number-to-string x) x))
                                 vals
                                 " "))
                               calc-prefer-frac nil))))
       (operation (substring (or "+" "-" "*" "/")))
       (assignment var-lhs space "=" space (or range sequence)
                   `(v r -- (progn
                              (push (cons (intern v) r) mathsheet--var-list)
                              r)))
       (sequence (list (or range value) (* "," space (or range value)))
                 `(vals -- (seq-random-elt vals)))
       (range value ".." value
              `(min max -- (if (>= min max)
                               (error "Range bounds must be increasing")
                             (+ (random (- max min)) min))))
       (value (or (substring (opt "-") (+ digit)) var-rhs parenthetical)
              `(v -- (if (stringp v) (string-to-number v) v)))
       (parenthetical "(" (or expression value) ")")
       (var-lhs (substring letter)) ; var for assignment
       (var-rhs "$" (substring letter) ; var for use
                `(v -- (let ((val (alist-get (intern v) mathsheet--var-list)))
                         (or val (error "Var %s not set" v)))))
       (math-func (substring (or "sqrt" "sin" "cos" "tan" "asin" "acos" "atan" "floor" "ceil" "round"))
                  parenthetical
                  `(f v -- (string-to-number (calc-eval (format "%s(%s)" f v)))))
       (space (* [space]))
       (letter [a-z])
       (digit [0-9]))

    (peg-run (peg field)
             (lambda (x) (message "Failed %s" x))
             (lambda (x) (car (funcall x))))))

(defun mathsheet--replace-field (node)
  "Replace a field in NODE with the number to which it reduces.

Update the current buffer by replacing the field at point in the
current buffer with the number it reduces to.  NODE contains the
info for the current field."
  (let ((start (caaddr node))
        (end (1+ (cdaddr node)))
        val)
    (goto-char start)
    (when (looking-at "\\[")
      (setq val (mathsheet--reduce-field))
      (goto-char start)
      (delete-char (- end start) t)
      (insert (number-to-string val)))))

(defun mathsheet--dfs-visit (node fields)
  "Visit NODE as part of a DFS of the problem.

Traverse the fields of a problem using depth first search to
ensure that field replacement happens in dependency order.
FIELDS is a list of all fields in the problem."
  (pcase (cadddr node)
    (1 (error "Cycle detected")) ; cycle
    (2)                          ; skip
    (_                           ; process
     (setcar (cdddr node) 1)     ; started
     (dolist (dep (cadr node))
       (mathsheet--dfs-visit
        (assq dep fields)
        fields))
     (mathsheet--replace-field node) ; visit
     (setcar (cdddr node) 2)))) ; mark done

(defun mathsheet--fill-problem (full-problem)
  "Replace all fields in FULL-PROBLEM.

Goes through all fields in the given problem in dependency order
and replaces fields with numbers.  When this completes the problem
will be ready to solve."
    (with-temp-buffer
      ;; stage problem in temp buffer
      (insert full-problem)
      (goto-char (point-min))

      ;; find fields, assignment variables, algebraic variables, dependencies
      (let* ((scan-ret (mathsheet--scan-problem))
             (fields (alist-get :fields scan-ret))
             (alg-vars (alist-get :alg-vars scan-ret)))

        ;; visit fields ordered according to dependencies
        (dolist (node fields)
          (mathsheet--dfs-visit node fields))
        (setq mathsheet--var-list '())

        ;; return filled problem
        `((:problem . ,(buffer-string))
          (:alg-vars . ,alg-vars)))))

(defun mathsheet--generate-problems (templates count)
  "Use TEMPLATES to generate COUNT problems.

Generate problems and answers based on what is defined in the
given template table.  The template table defines problem
templates as well as relative weights and how they should be
ordered."
  (let (total-weight problems)
    ;; sort by weight (low to high)
    (setq templates (sort templates #'car-less-than-car)
          ;; calc total weight
          total-weight (seq-reduce (lambda (total item) (+ total (car item)))
                                   templates
                                   0.0))

    ;; calculate number for each row
    (dotimes (ii (length templates))
      (let* ((item (nth ii templates))
             (weight (car item))
             (needed (cond ; number of problems to add for this template
                      ((= weight 0)
                       0)
                      ((= ii (1- (length templates)))
                       (- count (length problems)))
                      (t
                       (max (round (* (/ weight total-weight) count) ) 1))))
             (added 0)
             (dup-count 0)
             problem-set)
        (while (< added needed) ; add until "needed" are kept
          (let* ((fill-ret (mathsheet--fill-problem (caddr item)))
                 (problem (alist-get :problem fill-ret))
                 (alg-vars (alist-get :alg-vars fill-ret))
                 (calc-string (if (not alg-vars)
                                  problem
                                (format "solve(%s,[%s])"
                                        problem
                                        (string-join (seq-uniq alg-vars) ","))))
                 (solution
                  (replace-regexp-in-string (rx (or "[" ".]" "]"))
                                            ""
                                            (calc-eval `(,calc-string
                                                         calc-prefer-frac t
                                                         calc-frac-format ("/" nil))))))
            (cond
             ((member problem problem-set) ; dedup problems
              (setq dup-count (1+ dup-count))
              (when (> dup-count 100)
                ;; high number of dups indicates a narrow problem space relative to problem count
                (error "Giving up, too many dups")))
             (t
              (push problem problem-set)
              (push (list problem ; problem
                          solution ; solution
                          (cadr item) ; order
                          (not (null alg-vars))) ; true if algebraic variables exist
                    problems)
              (setq added (1+ added))))))))

    ;; shuffle
    (dotimes (ii (- (length problems) 1))
      (let ((jj (+ (random (- (length problems) ii)) ii)))
        (cl-psetf (elt problems ii) (elt problems jj)
                  (elt problems jj) (elt problems ii))))

    ;; sort by order
    (setq problems (sort problems (lambda (a b) (< (caddr a) (caddr b)))))

    ;; return problems and answers, drop header
    problems))

(defun mathsheet--convert-to-eqn (expr)
  "Format the given calc expression EXPR for groff.

  EXPR should be in normal calc format.  The result is the same
  expression (not simplified) but in eqn format for groff."
  (let ((current-language calc-language))
    (calc-set-language 'eqn)
    (let* ((calc-expr (math-read-expr expr))
           (eqn-expr (math-format-stack-value (list calc-expr 1 nil)))
           (eqn-expr-cleaned (replace-regexp-in-string (rx "1:" (* space)) "" eqn-expr)))
      (calc-set-language current-language)
      eqn-expr-cleaned)))

(defun mathsheet--write-worksheet (fname instruction problems prob-cols)
  "Write a worksheet to FNAME with INSTRUCTION and PROBLEMS.

  Write a file named FNAME.  Include the INSTRUCTION line at the
  top.  The problems will be arranged in PROB-COLS columns.  The
  answers will be in 5 columns."
  (with-temp-buffer
    (insert mathsheet--worksheet-template)

    (let ((probs-per-col (ceiling (/ (float (length problems)) prob-cols))))
      (goto-char (point-min))
      (search-forward "<<instruction>>")
      (replace-match
       (if (null instruction)
           ""
         (concat ".B \"" instruction "\"")))

      (goto-char (point-min))
      (search-forward "<<layout>>")
      (replace-match "")
      (insert (format ".nr ncols %d\n" prob-cols))
      (insert ".nr clen ((\\n[.l]/1000)-((\\n[ncols]-1)*10)/\\n[ncols])\n")
      (insert (format ".nr cl %d\n" probs-per-col))
      (insert ".nr vs (\\n[.p]/1000-(2*72)-20-(12*\\n[cl]))/(\\n[cl]+1)")

      (goto-char (point-min))
      (search-forward "<<problems>>")
      (replace-match "")
      (insert ".AL\n")
      (let ((colsize probs-per-col))
        (seq-do-indexed
         (lambda (group index)
           (unless (= index 0)
             (insert ".NCOL\n"))
           (dolist (row group)
             (message "convert to eqn %s -> %s" (car row) (mathsheet--convert-to-eqn (car row)))
             (insert (format (if (nth 3 row)
                                 ".LI\n.EQ\n%s\n.EN\n.SP \\n[vs]p\n"
                               ".LI\n.EQ\n%s =\n.EN\n\\l'5\\_'\n.SP \\n[vs]p\n")
                             (mathsheet--convert-to-eqn (car row))))))
         (seq-partition problems colsize)))
      (insert ".LE")

      (goto-char (point-min))
      (search-forward "<<answers>>")
      (replace-match "")
      (let ((index 0))
        (dolist (row problems)
          (setq index (1+ index))
          (insert
           (format ".EQ\n%d. %s%s\n.EN%s"
                   index
                   (mathsheet--convert-to-eqn (cadr row))
                   (if (< index (length problems)) "\",\"~" "")
                   (if (< index (length problems)) "\n" ""))))))

    ;; write the groff file for debugging
    ;; (write-region (point-min) (point-max) (concat fname ".mm"))

    ;; run groff to generate the pdf
    (let* ((default-directory mathsheet-output-directory)
           (ret (shell-command-on-region
                 (point-min) (point-max)
                 (format "groff -mm -e -Tpdf - > %s" (concat fname ".pdf")))))
      (unless (eq ret 0)
        (error "PDF generation failed")))))

(when (null forms-mode-map)
  (add-to-list
   'forms-mode-hook
   (lambda ()
     (when (string= "mathsheet.el" (buffer-name))
       (define-key forms-mode-map "\C-r" #'mathsheet-generate-sheet)))))

;;;###autoload
(defun mathsheet-open ()
  "Open mathsheet."
  (interactive)
  (forms-find-file (locate-file "mathsheet.el" load-path)))

(provide 'mathsheet)

;;; mathsheet.el ends here
