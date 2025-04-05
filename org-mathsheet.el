;;; org-mathsheet.el --- Generate dynamic math worksheets  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Ian Martins <ianxm@jhu.edu>
;; Keywords: tools, education, math
;; Homepage: https://gitlab.com/ianxm/org-mathsheet
;; Version: 1.0
;; Package-Requires: ((peg "1.0"))

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
;; defined in org tables, generated in dynamic blocks for review, and
;; exported to PDF for printing.

;;; Code:

(require 'peg)

(let ((page '"\\documentclass[12pt]{exam}
\\usepackage[top=1in, bottom=0.5in, left=0.8in, right=0.8in]{geometry}
\\usepackage{multicol}
\\usepackage{rotating}
\\usepackage{xcolor}

\\pagestyle{head}
\\header{Name:\\enspace\\makebox[2.2in]{\\hrulefill}}{}{Date:\\enspace\\makebox[2.2in]{\\hrulefill}}

\\begin{document}

  \\noindent <<instruction>>

  \\begin{questions}
    <<problems>>
  \\end{questions}

  \\vspace*{\\fill}

  \\vspace*{0.1cm}
  \\noindent\\rule{\\linewidth}{0.4pt}
  \\vspace*{0.1cm}

  \\begin{turn}{180}
    \\begin{minipage}{\\linewidth}
      \\color{gray}
      \\footnotesize
      \\begin{questions}
        <<answers>>
      \\end{questions}
    \\end{minipage}
  \\end{turn}

\\end{document}"))
(defvar org-mathsheet--var-list '()
  "List of variables used in a problem.")

(defconst org-mathsheet--worksheet-template page
  "LaTeX template for the worksheet.")
)

(defun org-mathsheet--scan-problem ()
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

(defun org-mathsheet--reduce-field ()
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
                              (push (cons (intern v) r) org-mathsheet--var-list)
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
                `(v -- (let ((val (alist-get (intern v) org-mathsheet--var-list)))
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

(defun org-mathsheet--replace-field (node)
  "Replace a field with the number to which it reduces.

Update the current buffer by replacing the field at point in the
current buffer with the number it reduces to.  NODE contains the
info for the current field."
  (let ((start (caaddr node))
        (end (1+ (cdaddr node)))
        val)
    (goto-char start)
    (when (looking-at "\\[")
      (setq val (org-mathsheet--reduce-field))
      (goto-char start)
      (delete-char (- end start) t)
      (insert (number-to-string val)))))

(defun org-mathsheet--dfs-visit (node fields)
  "Visit NODE as part of a DFS of the problem.

Traverse the fields of a problem using depth first search to
ensure that field replacement happens in dependency order.
FIELDS is a list of all fields in the problem."
  (pcase (cadddr node)
    (1 (error "Cycle detected")) ; cycle
    (2)                          ; skip
    (_                           ; process
     (setcar (cdddr node) 1)     ; started
     (let ((deps (cadr node)))
       (dolist (dep deps)
         (org-mathsheet--dfs-visit
          (assq dep fields)
          fields)))
     (org-mathsheet--replace-field node) ; visit
     (setcar (cdddr node) 2)))) ; mark done

(defun org-mathsheet--fill-problem (full-problem)
  "Replace all fields in FULL-PROBLEM.

Goes through all fields in the given problem in dependency order
and replaces fields with numbers.  When this completes the problem
will be ready to solve."
    (with-temp-buffer
      ;; stage problem in temp buffer
      (insert full-problem)
      (goto-char (point-min))

      ;; find fields, assignment variables, algebraic variables, dependencies
      (let* ((scan-ret (org-mathsheet--scan-problem))
             (fields (alist-get :fields scan-ret))
             (alg-vars (alist-get :alg-vars scan-ret)))

        ;; visit fields ordered according to dependencies
        (dolist (node fields)
          (org-mathsheet--dfs-visit node fields))
        (setq org-mathsheet--var-list '())

        ;; return filled problem
        `((:problem . ,(buffer-string))
          (:alg-vars . ,alg-vars)))))

(defun org-mathsheet--generate-problems (template-name count)
  "Use templates from TEMPLATE-NAME to generate COUNT problems.

Generate problems and answers based on what is defined in the
given template table.  The template table defines problem
templates as well as relative weights and how they should be
ordered."
  (let (total-weight templates problems)
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp (org-babel-named-data-regexp-for-name template-name) nil t)

      ;; read table from buffer, drop header, convert fields to numbers or strings
      (setq templates (mapcar
                       (lambda (row) (list (string-to-number (nth 0 row))
                                           (string-to-number (nth 1 row))
                                           (substring-no-properties (nth 2 row))))
                       (seq-drop (org-table-to-lisp) 2)))) ; load the table, drop the header

    ;; sort by weight (low to high)
    (setq templates (sort templates (lambda (a b) (< (car a) (car b))))
          ;; calc total weight
          total-weight (float
                        (seq-reduce (lambda (total item) (+ total (car item)))
                                    templates
                                    0)))

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
          (let* ((fill-ret (org-mathsheet--fill-problem (caddr item)))
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
    (sort problems (lambda (a b) (< (caddr a) (caddr b))))

    ;; return problems and answers, drop header
    problems))

;;;###autoload
(defun org-dblock-write:problem-set (params)
  "Update problem-set block and optionally write a worksheet.

PARAMS is a plist with the properties set on the dynamic block
header, which includes `:tempates' which is the name of the
templates table, `:count' which is the number of problems to put
on the worksheet, `:prob-cols' for the number of columns to use
for problems, and `:instruction' which is the content of the
instruction line at the top of the page."

  ;; write the table header
  (insert "| problem | answer |\n")
  (insert "|-\n")

  ;; generate problem set
  (let ((problems (org-mathsheet--generate-problems
                   (plist-get params :templates)
                   (plist-get params :count))))

    ;; for each problem, write a row to the table
    (insert
     (mapconcat
      (lambda (problem) (format "| %s | %s |"
                                (car problem)
                                (cadr problem)))
      problems
      "\n"))

    ;; align table
    (org-table-align)

    ;; should we generate the sheet?
    (when (y-or-n-p "Write worksheet? ")
      (org-mathsheet--gen-worksheet
       (plist-get params :templates)
       (plist-get params :instruction)
       problems
       (plist-get params :prob-cols)))))

(defun org-mathsheet--convert-to-latex (expr)
  "Format the given calc expression EXPR for LaTeX.

EXPR should be in normal calc format.  The result is the same
expression (not simplified) but in LaTeX format."
  (let* ((calc-language 'latex)
         (calc-expr (math-read-expr expr))
         (latex-expr (math-format-stack-value (list calc-expr 1 nil)))
         (latex-expr-cleaned (replace-regexp-in-string (rx "1:" (* space)) "" latex-expr)))
    (concat "$" latex-expr-cleaned "$")))

(defun org-mathsheet--gen-worksheet (file-name instruction problems prob-cols)
  "Generate a worksheet with PROBLEMS.

Write a file named FILE-NAME.  Include the INSTRUCTION line at the
top.  The problems will be arranged in PROB-COLS columns.  The
answers will be in 4 columns."
  (with-temp-file (concat file-name ".tex")
    (insert org-mathsheet--worksheet-template)

    (goto-char (point-min))
    (search-forward "<<instruction>>")
    (replace-match "")
    (insert instruction)

    (let ((answ-cols 5))
      (goto-char (point-min))
      (search-forward "<<problems>>")
      (replace-match "")
      (dolist (group (seq-partition problems prob-cols))
        (insert (format "\\begin{multicols}{%d}\n" prob-cols))
        (dolist (row group)
          (if (cadddr row)
              (insert (format"\\question %s\n"
                             (org-mathsheet--convert-to-latex (car row))))
            (insert (format"\\question %s = \\rule[-.2\\baselineskip]{2cm}{0.4pt}\n"
                           (org-mathsheet--convert-to-latex (car row))))))
        (insert "\\end{multicols}\n")
        (insert "\\vspace{\\stretch{1}}\n"))

      (goto-char (point-min))
      (search-forward "<<answers>>")
      (replace-match "")
      (dolist (group (seq-partition problems answ-cols))
        (insert (format "\\begin{multicols}{%s}\n" answ-cols))
        (dolist (row group)
          (insert (format "\\question %s\n"
                          (org-mathsheet--convert-to-latex (cadr row)))))
        (insert "\\end{multicols}\n"))))
  (shell-command (concat "texi2pdf " file-name ".tex")
                 (get-buffer-create "*Standard output*")))

(provide 'org-mathsheet)

;;; org-mathsheet.el ends here
