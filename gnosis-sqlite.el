;;; gnosis-sqlite.el --- Built-in SQLite backend for gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Drop-in replacement for emacsql using Emacs 29+ built-in SQLite.
;;
;; Value encoding/decoding is compatible with emacsql's format:
;; - Strings are stored with Lisp double-quote delimiters (prin1-to-string)
;; - Reading uses read-from-string, matching emacsql-sqlite-read-column
;; - nil encodes as SQL NULL, numbers pass through unchanged
;;
;; This module provides:
;; - Connection management (open, close, live-p)
;; - Value encoding/decoding (emacsql-compatible)
;; - Query execution (select, execute)
;; - Transaction macro with nesting support
;; - S-expression mini-compiler for WHERE/SET clauses
;; - Schema compiler for CREATE TABLE from emacsql schema S-expressions

;;; Code:

(require 'cl-lib)

;;; Connection management

(defun gnosis-sqlite-open (file)
  "Open SQLite database FILE and return the handle.
Enables foreign keys and sets a busy timeout."
  (let ((db (sqlite-open file)))
    (sqlite-execute db "PRAGMA foreign_keys = ON")
    (sqlite-execute db "PRAGMA busy_timeout = 5000")
    db))

(defun gnosis-sqlite-close (db)
  "Close SQLite database handle DB."
  (sqlite-close db))

(defun gnosis-sqlite-live-p (db)
  "Return non-nil if DB is a live SQLite handle."
  (sqlitep db))

;;; Value encoding (emacsql-compatible)

(defun gnosis-sqlite--encode-param (value)
  "Encode VALUE for binding as a SQL parameter.
nil -> :null, numbers pass through, everything else -> prin1-to-string."
  (cond
   ((null value) nil)
   ((numberp value) value)
   (t (prin1-to-string value))))

(defun gnosis-sqlite--decode (value)
  "Decode a single SQL result VALUE to a Lisp object.
nil -> nil, numbers pass through, empty string -> empty string,
other strings -> read-from-string (emacsql-compatible)."
  (cond
   ((null value) nil)
   ((numberp value) value)
   ((and (stringp value) (string-empty-p value)) "")
   ((stringp value)
    (condition-case nil
        (car (read-from-string value))
      (error value)))
   (t value)))

(defun gnosis-sqlite--decode-rows (rows)
  "Decode all columns in all ROWS."
  (mapcar (lambda (row)
            (mapcar #'gnosis-sqlite--decode row))
          rows))

;;; Query execution

(defun gnosis-sqlite--encode-params (params)
  "Encode a list of PARAMS for SQL binding."
  (mapcar #'gnosis-sqlite--encode-param params))

(defun gnosis-sqlite-select (db sql &optional params)
  "Execute SQL select query on DB with optional PARAMS.
PARAMS are encoded via `gnosis-sqlite--encode-param' before binding.
Returns decoded rows."
  (let ((encoded (gnosis-sqlite--encode-params params)))
    (gnosis-sqlite--decode-rows
     (sqlite-select db sql encoded))))

(defun gnosis-sqlite-execute (db sql &optional params)
  "Execute SQL statement on DB with optional PARAMS.
PARAMS are encoded via `gnosis-sqlite--encode-param' before binding.
For INSERT, UPDATE, DELETE, CREATE, etc."
  (let ((encoded (gnosis-sqlite--encode-params params)))
    (sqlite-execute db sql encoded)))

(defun gnosis-sqlite--select-compiled (db sql &optional params)
  "Execute SQL select on DB with pre-encoded PARAMS from the compiler.
Returns decoded rows.  Used internally by `gnosis-select' and friends."
  (gnosis-sqlite--decode-rows (sqlite-select db sql params)))

(defun gnosis-sqlite--execute-compiled (db sql &optional params)
  "Execute SQL statement on DB with pre-encoded PARAMS from the compiler.
Used internally by `gnosis--insert-into', `gnosis-update', etc."
  (sqlite-execute db sql params))

;;; Transactions

(defvar gnosis-sqlite--in-transaction nil
  "Non-nil when inside a `gnosis-sqlite-with-transaction' block.")

(defmacro gnosis-sqlite-with-transaction (db &rest body)
  "Execute BODY inside a transaction on DB.
Only the outermost invocation issues BEGIN/COMMIT.
Rolls back on error via `unwind-protect'."
  (declare (indent 1) (debug t))
  (let ((db-sym (gensym "db"))
        (outer-sym (gensym "outer"))
        (result-sym (gensym "result")))
    `(let* ((,db-sym ,db)
            (,outer-sym (not gnosis-sqlite--in-transaction))
            (gnosis-sqlite--in-transaction t)
            (,result-sym nil))
       (when ,outer-sym
         (sqlite-execute ,db-sym "BEGIN IMMEDIATE"))
       (unwind-protect
           (progn
             (setq ,result-sym (progn ,@body))
             (when ,outer-sym
               (sqlite-execute ,db-sym "COMMIT"))
             ,result-sym)
         (when (and ,outer-sym
                    (condition-case nil
                        (progn (sqlite-execute ,db-sym "ROLLBACK") t)
                      (error nil)))
           nil)))))

;;; Batch execution helpers

(defvar gnosis-sqlite--max-vars nil
  "Cached value of SQLITE_MAX_VARIABLE_NUMBER for batch operations.")

(defun gnosis-sqlite--max-variable-number (db)
  "Return SQLITE_MAX_VARIABLE_NUMBER for DB, cached after first call."
  (or gnosis-sqlite--max-vars
      (setq gnosis-sqlite--max-vars
            (let ((opts (sqlite-select db "PRAGMA compile_options")))
              (cl-loop for (opt) in opts
                       when (string-match "MAX_VARIABLE_NUMBER=\\([0-9]+\\)" opt)
                       return (string-to-number (match-string 1 opt))
                       finally return 999)))))

(defun gnosis-sqlite-execute-batch (db sql ids &optional extra-params)
  "Execute SQL on DB for each batch of IDS, staying within variable limits.
SQL must contain a single %s placeholder for the IN clause.
EXTRA-PARAMS are additional bound parameters prepended to each batch
\(e.g. a SET value).  They reduce the available batch capacity.
No-op when IDS is nil."
  (when ids
    (let* ((max-vars (gnosis-sqlite--max-variable-number db))
           (batch-size (- max-vars (length extra-params)))
           (offset 0)
           (total (length ids)))
      (while (< offset total)
        (let* ((end (min (+ offset batch-size) total))
               (chunk (cl-subseq ids offset end))
               (placeholders (mapconcat (lambda (_) "?") chunk ", "))
               (params (append (gnosis-sqlite--encode-params extra-params)
                               (gnosis-sqlite--encode-params chunk))))
          (sqlite-execute db (format sql placeholders) params)
          (setq offset end))))))

(defun gnosis-sqlite-select-batch (db sql ids &optional extra-params)
  "Execute SELECT SQL on DB for batched IDS, accumulating results.
SQL must contain a single %s placeholder for the IN clause.
EXTRA-PARAMS are additional bound parameters prepended to each batch.
Returns all decoded rows concatenated across batches."
  (when ids
    (let* ((max-vars (gnosis-sqlite--max-variable-number db))
           (batch-size (- max-vars (length extra-params)))
           (offset 0)
           (total (length ids))
           (all-rows nil))
      (while (< offset total)
        (let* ((end (min (+ offset batch-size) total))
               (chunk (cl-subseq ids offset end))
               (placeholders (mapconcat (lambda (_) "?") chunk ", "))
               (params (append (gnosis-sqlite--encode-params extra-params)
                               (gnosis-sqlite--encode-params chunk)))
               (rows (gnosis-sqlite--decode-rows
                      (sqlite-select db (format sql placeholders) params))))
          (setq all-rows (nconc all-rows rows))
          (setq offset end)))
      all-rows)))

;;; S-expression compiler: identifiers

(defun gnosis-sqlite--ident (sym)
  "Convert symbol SYM to a SQL identifier string.
Replaces `-' with `_' and `table:col' with `table.col'."
  (let ((name (symbol-name sym)))
    (setq name (replace-regexp-in-string "-" "_" name))
    (setq name (replace-regexp-in-string ":" "." name))
    name))

;;; S-expression compiler: expressions

(defun gnosis-sqlite--compile-expr (expr)
  "Compile S-expression EXPR into (SQL-STRING . PARAMS-LIST).

Supported patterns:
  (= col val)           -> \"col = ?\"       with (val)
  (= num num)           -> \"num = num\"     literal
  (and e1 e2 ...)       -> \"(e1) AND (e2)\"
  (or e1 e2 ...)        -> \"(e1) OR (e2)\"
  (like col val)        -> \"col LIKE ?\"    with (val)
  (in col vec)          -> \"col IN (?,?)\"  with vector elements
  (> col val), etc.     -> \"col > ?\"       with (val)
  (not expr)            -> \"NOT (expr)\"
  (- n col)             -> \"n - col\"       literal arithmetic"
  (pcase expr
    ;; Literal number equality: (= 1 1)
    (`(= ,(and (pred numberp) a) ,(and (pred numberp) b))
     (cons (format "%s = %s" a b) nil))
    ;; Column = value
    (`(= ,(and (pred symbolp) col) ,val)
     (if (and (listp val) (eq (car val) 'quote))
         (cons (format "%s = ?" (gnosis-sqlite--ident col))
               (list (gnosis-sqlite--encode-param (cadr val))))
       (if (symbolp val)
           (cons (format "%s = %s" (gnosis-sqlite--ident col) (gnosis-sqlite--ident val))
                 nil)
         (cons (format "%s = ?" (gnosis-sqlite--ident col))
               (list (gnosis-sqlite--encode-param val))))))
    ;; AND / OR
    (`(and . ,exprs)
     (let ((parts (mapcar #'gnosis-sqlite--compile-expr exprs)))
       (cons (mapconcat (lambda (p) (format "(%s)" (car p))) parts " AND ")
             (apply #'append (mapcar #'cdr parts)))))
    (`(or . ,exprs)
     (let ((parts (mapcar #'gnosis-sqlite--compile-expr exprs)))
       (cons (mapconcat (lambda (p) (format "(%s)" (car p))) parts " OR ")
             (apply #'append (mapcar #'cdr parts)))))
    ;; LIKE
    (`(like ,(and (pred symbolp) col) ,val)
     (if (and (listp val) (eq (car val) 'quote))
         (cons (format "%s LIKE ?" (gnosis-sqlite--ident col))
               (list (cadr val)))
       (cons (format "%s LIKE ?" (gnosis-sqlite--ident col))
             (list val))))
    ;; IN with vector
    (`(in ,(and (pred symbolp) col) ,(and (pred vectorp) vec))
     (let* ((elts (append vec nil))
            (placeholders (mapconcat (lambda (_) "?") elts ", ")))
       (cons (format "%s IN (%s)" (gnosis-sqlite--ident col) placeholders)
             (mapcar #'gnosis-sqlite--encode-param elts))))
    ;; Comparison operators: > < >= <=
    (`(,(and (pred symbolp) op) ,(and (pred symbolp) col) ,val)
     (when (memq op '(> < >= <=))
       (if (numberp val)
           (cons (format "%s %s ?" (gnosis-sqlite--ident col) op)
                 (list val))
         (if (and (listp val) (eq (car val) 'quote))
             (cons (format "%s %s ?" (gnosis-sqlite--ident col) op)
                   (list (gnosis-sqlite--encode-param (cadr val))))
           (cons (format "%s %s ?" (gnosis-sqlite--ident col) op)
                 (list (gnosis-sqlite--encode-param val)))))))
    ;; NOT
    (`(not ,inner)
     (let ((compiled (gnosis-sqlite--compile-expr inner)))
       (cons (format "NOT (%s)" (car compiled))
             (cdr compiled))))
    ;; Subtraction: (- n col)
    (`(- ,(and (pred numberp) n) ,(and (pred symbolp) col))
     (cons (format "%s - %s" n (gnosis-sqlite--ident col)) nil))
    (_ (error "gnosis-sqlite: unsupported expression: %S" expr))))

;;; S-expression compiler: columns

(defun gnosis-sqlite--compile-columns (spec)
  "Compile column SPEC to a SQL column list string.
SPEC can be a symbol (single column), a vector of symbols, or `*'."
  (cond
   ((eq spec '*) "*")
   ((vectorp spec)
    (mapconcat #'gnosis-sqlite--ident (append spec nil) ", "))
   ((symbolp spec) (gnosis-sqlite--ident spec))
   (t (error "gnosis-sqlite: unsupported column spec: %S" spec))))

;;; S-expression compiler: schema

(defun gnosis-sqlite--compile-col-def (col-spec)
  "Compile a single column definition COL-SPEC.
COL-SPEC is like (name type :constraint1 :constraint2 ...)."
  (let* ((col-name (gnosis-sqlite--ident (nth 0 col-spec)))
         (col-type (upcase (symbol-name (nth 1 col-spec))))
         (constraints (cddr col-spec))
         (parts (list col-type col-name)))
    (while constraints
      (let ((kw (pop constraints)))
        (pcase kw
          (:primary-key (push "PRIMARY KEY" parts))
          (:not-null (push "NOT NULL" parts))
          (:unique (push "UNIQUE" parts))
          (:default
           (let ((val (pop constraints)))
             (push (format "DEFAULT %s"
                           (if (stringp val) (format "'%s'" val)
                             (format "%s" val)))
                   parts))))))
    (mapconcat #'identity (nreverse parts) " ")))

(defun gnosis-sqlite--compile-constraint (constraint)
  "Compile a table-level CONSTRAINT to SQL string."
  (pcase constraint
    (`(:unique ,cols)
     (format "UNIQUE (%s)"
             (mapconcat #'gnosis-sqlite--ident (append cols nil) ", ")))
    (`(:foreign-key ,cols :references ,ref-table ,ref-cols . ,rest)
     (let ((sql (format "FOREIGN KEY (%s) REFERENCES %s (%s)"
                        (mapconcat #'gnosis-sqlite--ident (append cols nil) ", ")
                        (gnosis-sqlite--ident ref-table)
                        (mapconcat #'gnosis-sqlite--ident (append ref-cols nil) ", "))))
       (while rest
         (let ((kw (pop rest)))
           (pcase kw
             (:on-delete
              (let ((action (pop rest)))
                (setq sql (concat sql " ON DELETE "
                                  (upcase (substring (symbol-name action) 1)))))))))
       sql))
    (_ (error "gnosis-sqlite: unsupported constraint: %S" constraint))))

(defun gnosis-sqlite--compile-schema (schema)
  "Compile emacsql SCHEMA S-expression to SQL column/constraint definitions.
SCHEMA is like ([col-defs ...] constraint1 constraint2 ...).
Returns the string inside CREATE TABLE ... (HERE)."
  (let* ((col-vec (nth 0 schema))
         (col-defs (append col-vec nil))
         (constraints (cdr schema))
         (col-strings (mapcar #'gnosis-sqlite--compile-col-def col-defs))
         (constraint-strings (mapcar #'gnosis-sqlite--compile-constraint constraints)))
    (mapconcat #'identity (append col-strings constraint-strings) ", ")))

;;; High-level helpers for gnosis core

(defun gnosis-sqlite--compile-values (values)
  "Compile VALUES vector or list-of-vectors into (SQL-FRAGMENT . PARAMS).
A single vector [a b c] -> \"(?, ?, ?)\" with params (a b c).
A list of vectors [[a b] [c d]] -> \"(?, ?), (?, ?)\" with params (a b c d).
Params are pre-encoded for use with `gnosis-sqlite--execute-compiled'."
  (let* ((rows (if (vectorp values)
                   (list values)
                 ;; Could be ([...]) - a list containing a single vector
                 (if (and (listp values) (vectorp (car values)))
                     values
                   (list values))))
         (param-list nil)
         (row-strings
          (mapcar
           (lambda (row)
             (let* ((elts (append row nil))
                    (placeholders (mapconcat (lambda (_) "?") elts ", ")))
               (setq param-list (append param-list
                                        (mapcar #'gnosis-sqlite--encode-param elts)))
               (format "(%s)" placeholders)))
           rows)))
    (cons (mapconcat #'identity row-strings ", ")
          param-list)))

(provide 'gnosis-sqlite)
;;; gnosis-sqlite.el ends here
