;;; ilist.el --- Display a list in an ibuffer way.   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Durand <mmemmew@gmail.com>
;; Keywords: convenience
;; URL: https://gitlab.com/mmemmew/ilist
;; Version: 0.3

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

;; This is a library package.  That is to say, it should be used by
;; other packages, instead of by end users directly.  So the package
;; defines no interactive functions that can be called by keychords:
;; this is the job of the user-package of this package.

;; This provides a function to display a list in a similar manner to
;; ibuffer.  This means that we can use filters to filter the elements
;; of the list, and to group the elements in various ways.  The
;; original intention is to display bookmarks.

;;; Code:

;;; dependencies

(require 'text-property-search)

;;; column struct

;; This is similar to using `cl-defstruct', but this struct is so
;; simple that depending on the cl library seems unnecessary to me.

;;;; helper to define a column

(defun ilist-define-column (name fun &optional min max align elide)
  "Define a column to be displayed.
NAME is the name of the column.

FUN should be a function with one argument, the element of the
list, which returns the string to display.

MIN specifies the minimal width of the column.  This should be a
nonnegative integer.  Negative values will be converted to
positive values without queries.  If it is omitted or nil, it is
the same as 0.

MAX specifies the maximal width of the column.  This should be an
integer.  If it is less than or equal to zero, then there is no
limit on the width.  If it is omitted or nil, it is the same as
0.

ALIGN specifies how to align the column.  It should be one of
:left, :right, and :center.  If it is omitted or nil, it is the
same as :left.

ELIDE specifies how to elide the column if it is too long.  If it
is a string that string is used.  Any other value means to not
elide.

Note that if the length of ELIDE is greater than MIN, then it is
undefined behaviour.

Also, if the length of NAME is less than the width of the column,
then it is undefined behaviour.  Hence it is recommended to set
the minimum width to be greater than or equal to the length of
NAME, unless one is sure that the lengths of the elements of that
column will be sufficently long."
  (declare (pure t) (side-effect-free t))
  (list
   name
   fun
   (cond
    ((null min) 0)
    ((integerp min) (abs min))
    ((user-error "MIN should be an integer, but got %S" min)))
   (cond
    ((null max) 0)
    ((and (integerp max)
          (> max 0))
     max)
    ((integerp max)
     0)
    ((user-error "MAX should be an integer, but got %S" max)))
   (cond
    ((null align) :left)
    ((car (memq align (list :left :right :center))))
    ((user-error "ALIGN should be :left, :right, or :center, \
but got %S" align)))
   (cond ((stringp elide) elide))))

;;;; column accessors

(defalias 'ilist-column-name #'car
  "Return the NAME of COLUMN.

\(fn COLUMN)")

(defalias 'ilist-column-fun #'cadr
  "Return the FUN in COLUMN.

\(fn COLUMN)")

(defalias 'ilist-column-min #'caddr
  "Return the MIN in COLUMN.

\(fn COLUMN)")

(defalias 'ilist-column-max #'cadddr
  "Return the MAX in COLUMN.

\(fn COLUMN)")

;; Unfortunately (or not) there is no caddddr and cadddddr.
(defun ilist-column-align (column)
  "Return the ALIGN in COLUMN."
  (nth 4 column))

(defun ilist-column-elide (column)
  "Return the ELIDE in COLUMN."
  (nth 5 column))

;;; Whether to operate at the pixel-level precisions

(defvar ilist-pixel-precision nil
  "If non-nil, the string truncation and length related operations
will perform pixel-level calculations.  If nil, they will use
`truncate-string-to-width' and `string-width' as approximations.

Also, if this is the symbol `precise', then the the padding will
use display properties to produce spaces that occupy as many
pixels as exactly needed, so that the alignment will be perfect
at the level of pixels.  For any other non-nil value, the padding
will use `round' to produce approximate paddings.

Pixel-level calculations are more accurate but more expensive, so
users should intentionally decide to enable this feature, in
order not to hang Emacs unexpectedly.")

;;; Calculate the length of a string correctly

;; NOTE: This used to be obsolete, but is now given a new life.

(defun ilist-string-width (str)
  "Return the width of STR.
If the variable `ilist-pixel-precision' is non-nil, use
`string-pixel-width'; otherwise use `string-width'."
  ;; (declare (side-effect-free t) (pure t)
  ;;          (obsolete string-width "2021-12-19 23:53:42.283857"))
  (cond (ilist-pixel-precision (string-pixel-width str))
        ((string-width str))))

(defun ilist-fake-round (arg &optional divisor)
  "Either use round or just return ARG, depending on if
`ilist-pixel-precision' is non-nil."
  (cond (ilist-pixel-precision (cond (divisor (round arg divisor))
                                     ((round arg))))
        (arg)))

;;; Produce space with the specified width

(defun ilist-space-with-width (width &optional space-width)
  "Return a blank string with width exactly equal to WIDTH .
If `ilist-pixel-precision' is nil, return a string consisting of
WIDTH many spaces.

If `ilist-pixel-precision' is the symbol `precise', produce a
string which occupies exactly WIDTH many pixels.

In all other cases, produce a string consisting of X many spaces,
where X is given by the form (round WIDTH S) and S is the pixel
width of a space character.

If the optional argument SPACE-WIDTH is a positive integer, it is
interpreted as the pixel width of a space character, otherwise
the function will calculate the width of the space character on
each call."
  (cond
   ((and (integerp space-width)
         (> space-width 0)))
   ((setq space-width (string-pixel-width (string #x20)))))
  (cond
   ((null ilist-pixel-precision) (make-string width #x20))
   ((eq ilist-pixel-precision 'precise)
    (propertize
     (string #x20)
     'display (list 'space :width (list width))))
   ((make-string (round width space-width) #x20))))

;;; Correctly truncate strings

(defun ilist-truncate (str limit &optional ellipsis)
  "Truncate the string STR to no longer than LIMIT columns.
ELLIPSIS has the same meaning as for `truncate-string-to-width'.

This function calculates widths by `string-pixel-width'.

This function imitates `truncate-string-to-width' to some extent,
and fallbacks to that function if `ilist-pixel-precision' is nil."
  (cond
   (ilist-pixel-precision
    (setq ellipsis
          (cond ((or (null ellipsis) (stringp ellipsis)) ellipsis)
                ((truncate-string-ellipsis))))
    (let* ((substring-list (string-glyph-split str))
           (str-len (length str))
           (str-pixels (string-pixel-width str))
           (ellipsis-pixels
            (cond (ellipsis (string-pixel-width ellipsis)) (0)))
           (column 0)
           (idx 0)
           (space-width (string-pixel-width (string #x20)))
           (limit (* space-width limit))
           last-column last-idx temp-str)
      (with-current-buffer (get-buffer-create
                            " *ilist-truncate-to-pixels*")
        (when (bound-and-true-p display-line-numbers-mode)
          (display-line-numbers-mode -1))
        (delete-region (point-min) (point-max))
        (setq line-prefix nil)
        (setq wrap-prefix nil)
        (cond ((and (< limit str-pixels)
                    (< ellipsis-pixels str-pixels))
               (setq limit (- limit ellipsis-pixels)))
              ((setq ellipsis (string))))
        (condition-case nil
            (while (< column limit)
              (setq last-column column)
              (setq last-idx idx)
              (setq temp-str (nth idx substring-list))
              (insert (propertize
                       temp-str
                       'line-prefix nil 'wrap-prefix nil))
              (setq column (car (buffer-text-pixel-size nil nil t)))
              (setq idx (+ idx (length temp-str))))
          (t (setq idx str-len)))
        (cond
         ((> column limit)
          (setq column last-column)
          (setq idx last-idx)))
        (concat (substring str 0 idx) ellipsis))))
   ((truncate-string-to-width str limit 0 nil ellipsis))))

;;; display a row

(defun ilist-display (ls columns &optional no-trailing-space)
  "Return a list of lists of strings displaying the list LS.
COLUMNS is a list of column specificationss, which will be passed
to `ilist-define-column'.  See the documentation string of
`ilist-define-column' for the formats of COLUMNS.

The return value is a cons cell, the `car' of which is the list
of lists of strings and the `cdr' of which is a list of widths of
columns.

If NO-TRAILING-SPACE is non-nil, the last column will not have
trailing spaces."
  (declare (pure t) (side-effect-free t))
  (setq columns (mapcar (lambda (column)
                          (apply #'ilist-define-column column))
                        columns))
  (let ((column-len (length columns))
        (column-mins (mapcar #'ilist-column-min columns))
        (column-aligns (mapcar #'ilist-column-align columns))
        (space-width (ilist-string-width (string #x20)))
        result column-widths)
    ;; result will be a list each of whose elements corresponds to an
    ;; element in LS.  Each element corresponds to a list, each of
    ;; whose elements corresponds to a column.  Each column
    ;; corresponds to a cons cell of the form (LEN . STR), where LEN
    ;; is the length of STR, and where STR is already elided or
    ;; truncated.  So result is of the form
    ;;
    ;; (((LEN11 . STR11) (LEN12 . STR12) ...)
    ;;  ((LEN21 . STR21) (LEN22 . STR22) ...)
    ;;  ...)
    ;;
    ;; NOTE: I was using the function `string-width' to measure the
    ;; widths of strings, but as it turns out, this function is only
    ;; an approximation of the real width for non-Latin letters, as is
    ;; correctly pointed out by the documentation string.  As a
    ;; consequence I now turn to the function `string-pixel-width' to
    ;; correctly measure the widths, but this function measures in
    ;; pixels, so I need to deal with alignment using "hacky hacks".
    (setq
     result
     (mapcar
      (lambda (element)
        (mapcar
         (lambda (column)
           (let* ((str (or
                        (funcall
                         (ilist-column-fun column)
                         element)
                        (string)))
                  (str-len (string-width str))
                  (max-len (ilist-column-max column))
                  (elide (ilist-column-elide column))
                  (str
                   (cond
                    ((and (> max-len 0)
                          (> str-len max-len))
                     (cond
                      ((stringp elide)
                       (ilist-truncate str max-len elide))
                      ((ilist-truncate str max-len))))
                    (str))
                   ))
             (cons (ilist-string-width str) str)))
         columns))
      ls))
    ;; The list column-widths has a special convention: if a width is
    ;; negative, don't leave trailing whitespaces in that column.
    (setq
     column-widths
     (mapcar
      (lambda (n)
        (let ((len 0))
          (mapc
           (lambda (element)
             (cond
              ((> (car (nth n element)) len)
               (setq len (car (nth n element))))))
           result)
          (cond
           ((and (= n (1- column-len))
                 no-trailing-space)
            (- len))
           (len))))
      (number-sequence 0 (1- column-len))))
    (cons
     (mapcar
      (lambda (element)
        ;; we loop from the end of the columns, so that we can keep
        ;; pushing elements to the front, without having to reverse
        ;; the list afterwards.
        (let ((index (1- column-len))
              temp temp-width temp-align row)
          (while (>= index 0)
            (setq temp (nth index element))
            (setq temp-align (nth index column-aligns))
            ;; if the width is < min-width, then set the width to the
            ;; min-width.
            (cond
             ((< (abs (nth index column-widths))
                 (* space-width (nth index column-mins)))
              (setq temp-width
                    (* space-width (nth index column-mins)))
              (setcar (nthcdr index column-widths)
                      (* temp-width
                         ;; plus or minus
                         (floor (nth index column-widths)
                                (abs (nth index column-widths))))))
             ((setq temp-width (abs (nth index column-widths)))))
            ;; pad according to the alignment.
            (cond
             ((eq temp-align :left)
              (setq
               row
               (cons
                (cond
                 ((>= (nth index column-widths) 0)
                  (concat (cdr temp)
                          (ilist-space-with-width
                           (- temp-width (car temp))
                           space-width)))
                 ((cdr temp)))
                row)))
             ((eq temp-align :right)
              (setq
               row
               (cons
                (concat (ilist-space-with-width
                         (- temp-width (car temp))
                         space-width)
                        (cdr temp))
                row)))
             ((setq
               row
               (cons
                (let ((pad-left-len
                       (floor (- temp-width (car temp))
                              2)))
                  (concat
                   (ilist-space-with-width pad-left-len space-width)
                   (cdr temp)
                   (cond
                    ((>= (nth index column-widths) 0)
                     (ilist-space-with-width
                      (- temp-width pad-left-len
                         (car temp))
                      space-width))
                    (""))))
                row))))
            (setq index (1- index)))
          row))
      result)
     (mapcar #'abs column-widths))))

;;; produce the string

;;;; The helper function

(defun ilist-classify (sequence &rest args)
  "Return a copy of SEQUENCE with duplicate elements removed.
ARGS should be a property list specifying tests and keys.

If the keyword argument TEST is non-nil, it should be a function
with two arguments which tests for equality of elements in the
sequence.  The default is the function `equal'.

If the keyword argument KEY is non-nil, it should be a function
with one argument which returns the key of the element in the
sequence to be compared by the test function.  The default is the
function `identity'.

If the keyword argument DEFAULT is non-nil, when the KEY function
returns nil for an element, it will be replaced by DEFAULT.

Note that this function is not supposed to change global state,
including match data, so the functions in TEST and KEY are
supposed to leave the global state alone as well.

\(fn SEQUENCE &key TEST KEY DEFAULT)"
  (declare (pure t) (side-effect-free t))
  (let* ((len (length sequence))
         (temp-obarray (obarray-make len))
         (valid-key-num (+ (cond ((plist-member args :key) 2) (0))
                           (cond ((plist-member args :default) 2) (0))
                           (cond ((plist-member args :test) 2) (0))))
         (key (cond ((cadr (plist-member args :key))) (#'identity)))
         (default (cadr (plist-member args :default)))
         (test-fn (cond ((cadr (plist-member args :test))) (#'equal)))
         obj-table result)
    (cond ((or (= (mod (length args) 2) 1)
               (> (length args) valid-key-num))
           (user-error
            (concat
             "Invalid keyword arguments.  "
             "Only :key and :test are allowed, but got %S")
            args)))
    ;; Note: This just puts a property to the symbol.
    (define-hash-table-test 'ilist-classify-test
      test-fn
      (function
       (lambda (obj)
         (intern (format "%S" obj) temp-obarray))))
    (setq
     obj-table
     (make-hash-table :test 'ilist-classify-test :size len))
    (mapc
     (function
      (lambda (element)
        (let ((get-hash (gethash
                         (or (funcall key element) default)
                         obj-table)))
          (cond (get-hash
                 (puthash
                  (or (funcall key element) default)
                  (cons element get-hash) obj-table))
                ((puthash
                  (or (funcall key element) default)
                  (list element) obj-table))))))
     sequence)
    (maphash
     ;; key is abused
     (lambda (key value)
       (setq result (cons (cons key (nreverse value)) result)))
     obj-table)
    (nreverse result)))

;;;; The real function

(defun ilist-string
    (ls columns groups
        &optional discard-empty-p sorter no-trailing-space)
  "Display list LS as the returned string.
COLUMNS will be passed to `ilist-define-column'.

GROUPS is a filter group specification.  A filter group
specification has two types: a fixed filter group, or an
automatic filter group.

A fixed filter group specification is a list of the following
form:

\((NAME1 . FUN1) (NAME2 . FUN2) ...)

Here NAMEs are the strings to display as the label of the groups.

FUNs are the functions to determine if an element belongs to the
group.  It should accept one argument, the element under
consideration, and should return non-nil if that element belongs
to the group.  The group that occurs first in the list GROUPS has
higher priority over those that occur later.

An automatic filter group specification is simply a function.
This function serves multiple purposes.  Its argument list should
be compatible with the following:

\(ARG &optional TYPE)

When TYPE is omitted or nil, this function will receive an
element of the list as ARG, and should return a string.  This
return value will be used as the group label, and elements with
the same group label will be grouped together automatically.

When TYPE is 'default, it should ignore the ARG and return a
default label, which will be used as the label for those elements
that this function returns nil as the label.

When TYPE is 'sorter, it should ignore the ARG again and return a
function to sort the labels.  This sorter should accept two
arguments, X and Y, and should return non-nil if and only if
label X should come before label Y.  If the function returns
anything else, then the labels will not be sorted.

The macro `ilist-define-automatic-group' might come in handy for
defining group functions.  See its documentation for details.

The display of each group is done by `ilist-display'.

If DISCARD-EMPTY-P is non-nil, then empty groups will not be
displayed.

If SORTER is non-nil, it should be a function with two arguments,
X and Y, and should return non-nil if X should come before Y.

If NO-TRAILING-SPACE is non-nil, the last column will not have
trailing spaces.

As a note, this function is not supposed to change global state,
so the functions used, such as the automatic group or the sorter,
should not change the global states either.  This includes the
matched data, the cursor position, etc."
  (declare (pure t) (side-effect-free t))
  ;; normalize SORTER
  (cond
   ((null sorter))
   ((not (functionp sorter))
    (user-error "SORTER should be a function, but got %S"
                sorter)))
  (let* ((ls (copy-tree ls))
         (temp-groups
          ;; normalize GROUPS
          (cond
           ;; A function closure is a list as well, for some reason.
           ;; So we test if GROUPS is a function first.
           ((functionp groups) groups)
           ((consp groups) (copy-tree groups))
           ((user-error
             (concat
              "GROUPS should be either a list or a function, "
              "but got %S")
             (type-of groups)))))
         (space-width (ilist-string-width (string #x20)))
         column-widths temp-group group-results group-strs
         all-cols all-cols-indices header title-sep)
    ;; If we want to operate on the displayed list, then we should
    ;; store the original list, and the indices of each displayed
    ;; element.  But we re-order the elements while preparing the
    ;; display (that is sort of the whole point of the preparation),
    ;; so we must store the original indices as well.  As to the
    ;; original list, it is the responsibility of the user-package to
    ;; store the list and do something with it; we are only
    ;; responsible for the indices.
    (setq
     ls
     (let ((index -1))
       (mapcar
        (lambda (element)
          ;; I cannot resist using hacks.
          (cons (setq index (1+ index)) element))
        ls)))
    ;; We sort the list after the indices are stored.
    (cond ((null sorter))
          ((setq
            ls
            (sort
             ls (lambda (x y) (funcall sorter (cdr x) (cdr y)))))))
    (cond
     ((not (functionp groups))
      (while (consp temp-groups)
        (setq temp-group (car temp-groups))
        ;; NOTE: The order of group-results is reverse to the order we
        ;; want, and we will reverse the order again when we convert
        ;; that to a list of strings later.
        (setq
         group-results
         (cons
          (let ((fun (cdr temp-group))
                res remain)
            (mapc
             (lambda (element)
               (cond
                ;; the car is the original index, and the cdr is the
                ;; original element
                ((funcall fun (cdr element))
                 (setq res (cons element res)))
                ((setq remain (cons element remain)))))
             ls)
            (setq ls (reverse remain))
            ;; endow it with a text property so that we can
            ;; distinguish a group header from a normal line
            (list (propertize
                   (format "[ %s ]" (car temp-group))
                   'ilist-group-header (car temp-group))
                  (reverse res)))
          group-results))
        (setq temp-groups (cdr temp-groups))))
     (t ;; function groups case
      (setq group-results (ilist-classify
                           ls
                           :key (lambda (x)
                                  (funcall groups (cdr x)))
                           :default
                           (format "%s" (funcall groups t 'default))
                           :test #'string=))
      ;; sort the groups if needed
      (let ((sorter (funcall groups t 'sorter)))
        (cond
         ((functionp sorter)
          (setq group-results
                (sort group-results
                      (lambda (x y)
                        (funcall sorter (car x) (car y))))))))
      ;; transform the group titles
      (setq
       group-results
       ;; to conform with the other case, we manually reverse the list
       (nreverse
        (mapcar
         (lambda (result)
           (cons
            (propertize
             (format "[ %s ]" (car result))
             'ilist-group-header (car result))
            (list (cdr result))))
         group-results)))))
    ;; group-strs will not be in the final format yet, after this
    ;; `while'.
    (while (consp group-results)
      (setq temp-group (car group-results))
      (setq group-results (cdr group-results))
      (setq all-cols
            ;; NOTE: since the order of group-results is reverse, we
            ;; append it to the front instead to the end.  This
            ;; ensures the worst time complexity is linear.  After
            ;; this step the order of group-strs is the order we want.
            (append (cadr temp-group) all-cols))
      ;; here group-strs only contains the number of elements to
      ;; possess
      (setq
       group-strs
       (cons
        (cons (car temp-group)
              (length (cadr temp-group)))
        group-strs)))
    ;; `ilist-display' has nothing to do with the indices, so we first
    ;; separate the indices from the elements, and then zip them
    ;; together later.
    (setq all-cols-indices (mapcar #'car all-cols))
    (setq all-cols (mapcar #'cdr all-cols))
    ;; `ilist-display' is called on all elements, so that it can
    ;; calculate the maximal width correctly.
    (setq all-cols (ilist-display all-cols columns no-trailing-space))
    (setq column-widths (cdr all-cols))
    ;; we zip the indices back
    (setq all-cols
          (let* ((car-all-cols (car all-cols))
                 (len (length car-all-cols))
                 (index (1- len))
                 res)
            (while (>= index 0)
              (setq res
                    (cons
                     (cons
                      (nth index all-cols-indices)
                      (nth index car-all-cols))
                     res))
              (setq index (1- index)))
            res))
    ;; after the following group-strs has indices and strings in the
    ;; cells
    (let ((index 0) step)
      (mapc
       (lambda (cell)
         (setq step (cdr cell))
         (setcdr cell
                 (mapcar (lambda (n) (nth n all-cols))
                         (number-sequence
                          index (+ index -1 step))))
         (setq index (+ index step)))
       group-strs))
    ;; calculate the headers and the titles
    (let ((column-len (length columns))
          (index 0))
      (setq
       header
       ;; `mapconcat' uses a `mapcar' under the hood, so the order of
       ;; elements will be preserved
       (mapconcat
        (lambda (col)
          ;; pad according to the alignment
          (let* ((width (nth index column-widths))
                 (alignment (ilist-column-align col))
                 (name (ilist-column-name col))
                 (complement (- width (ilist-string-width name)))
                 (floor-len (floor complement 2)))
            ;; we increase the index before the end of the form
            (setq index (1+ index))
            (cond
             ((eq alignment :left)
              (cond
               ((< index column-len)
                (concat
                 name
                 (ilist-space-with-width complement space-width)))
               (name)))
             ((eq alignment :right)
              (concat
               (ilist-space-with-width complement space-width)
               name))
             ;; :center
             ((concat
               (ilist-space-with-width floor-len space-width)
               name
               (cond
                ((< index column-len)
                 (ilist-space-with-width
                  (- complement floor-len) space-width))
                ("")))))))
        columns
        (string #x20)))
      ;; don't forget to reset the index
      (setq index 0)
      ;; mutatis mutandis
      (setq
       title-sep
       (mapconcat
        (lambda (col)
          (let* ((width (nth index column-widths))
                 (alignment (ilist-column-align col))
                 (name (ilist-column-name col))
                 (name-len (ilist-string-width name))
                 (name-sep (make-string
                            (ilist-fake-round name-len space-width)
                            ?-))
                 (complement (- width name-len))
                 (floor-len (floor complement 2)))
            (setq index (1+ index))
            (cond
             ((eq alignment :left)
              (cond
               ((< index column-len)
                (concat
                 name-sep
                 (ilist-space-with-width complement space-width)))
               (name-sep)))
             ((eq alignment :right)
              (concat
               (ilist-space-with-width complement space-width)
               name-sep))
             ((concat
               (ilist-space-with-width floor-len space-width)
               name-sep
               (cond
                ((< index column-len)
                 (ilist-space-with-width
                  (- complement floor-len) space-width))
                ("")))))))
        columns
        (string #x20))))
    ;; delete empty groups if demanded
    (cond
     (discard-empty-p
      (setq
       group-strs
       (delq
        nil
        (mapcar
         (lambda (group)
           (cond
            ;; one way of testing if a list has length = 1
            ((null (cdr group)) nil)
            (group)))
         group-strs)))))
    (setq group-strs
          (append
           ;; special properties
           (list (propertize
                  (concat header (string #xa))
                  'ilist-header t)
                 (propertize
                  (concat title-sep (string #xa))
                  'ilist-title-sep t))
           ;; transform back to the format we want
           (let ((index 0))
             (mapcar
              (lambda (element)
                (setq index (1+ index))
                (concat
                 ;; title
                 (car element)
                 ;; for empty groups don't add a newline
                 (cond ((cdr element)
                        (propertize
                         (string #xa)
                         'ilist-group-header
                         (get-text-property
                          0 'ilist-group-header (car element)))))
                 ;; rows
                 (mapconcat
                  (lambda (row)
                    (propertize
                     (concat
                      (mapconcat
                       #'identity (cdr row) (string #x20))
                      (string #xa))
                     'ilist-index (car row)
                     'invisible (intern (car element))))
                  (cdr element)
                  (string))))
              group-strs))))
    (mapconcat #'identity group-strs (string))))

;;;; Macro for defining automatic filter groups

(defmacro ilist-define-automatic-group
    (name default sorter &rest body)
  "Define an automatic group for `ilist-string' to use.
NAME will be used to name the resulting function as
\"ilist-automatic-group-NAME\".

DEFAULT will be the default label when the function returns nil
as the label.

SORTER can be a symbol, or an S-expression.  If it is a symbol,
it will be used as the sorting function of the group labels.  If
it is an S-expression., it will be used to define a function
\"ilist-automatic-group-NAME-sorter\", which then becomes the
sorting function.

Note that one does not have to quote SORTER.

BODY will be evaluated with \"ELEMENT\" bound to the element
under consideration, and should return a string as the label of
that element, or nil, to use the default label.

Note that if DEFAULT is not a string, it will be evaluated and
the result will be used.  If there are errors in the evaluation,
it will simply be converted to a string silently."
  (declare (indent 3))
  (let ((fn (intern (format "ilist-automatic-group-%s" name)))
        (sorter-symbol
         (cond
          ((or (symbolp sorter) (functionp sorter)) sorter)
          ((and (memq (car sorter) (list 'function 'quote))
                (cadr sorter)))
          ((error
            "SORTER should be a function, but got %S" sorter)))))
    (list
     'defun fn (list 'element (quote &optional) 'type)
     "A filter group defined by `ilist-define-automatic-group'.
This should be used by `ilist-string' as an automatic filter group."
     (list
      'cond
      (list (list 'eq 'type ''default)
            default)
      (list (list 'eq 'type ''sorter)
            (list 'function sorter-symbol))
      (cons t body)))))

;; for saving some key-strokes
(defalias 'ilist-dag #'ilist-define-automatic-group)

;;; map over lines

(defun ilist-map-lines (fun &optional predicate start end
                            no-skip-invisible)
  "Execute FUN over lines.
If PREDICATE is non-nil, it should be a function to determine
whether to execute FUN on the line.

If START or END is non-nil, it specifies the boundaries of the
execution lines.  It can be an integer or a marker.  If it is a
marker, the buffer of the marker should be the current buffer.

The return value is the list of execution results on the lines
over which the function is executed.

If NO-SKIP-INVISIBLE is non-nil, then we don't skip invisible
lines."
  ;; normalizations
  (cond
   ((not (functionp predicate))
    (setq predicate nil)))
  (cond
   ((not (integer-or-marker-p start))
    (setq start nil))
   ((and
     (markerp start)
     (not (equal (marker-buffer start) (current-buffer))))
    (setq start nil)))
  (cond
   ((not (integer-or-marker-p end))
    (setq end nil))
   ((and
     (markerp end)
     (not (equal (marker-buffer end) (current-buffer))))
    (setq end nil)))
  (save-excursion
    (goto-char (cond (start) ((point-min))))
    (let (res)
      (while (and (not (ilist-boundary-buffer-p t))
                  (or (null end)
                      (< (point) end)))
        (cond
         ((or (null predicate)
              (funcall predicate))
          (setq res
                (cons
                 (funcall fun)
                 res))))
        (ilist-forward-line 1 nil nil no-skip-invisible))
      (nreverse res))))

;;; Point at the end of line

(defun ilist-point-at-eol (&optional pos no-skip-invisible)
  "The point at the end of line containing POS or the current point.
If NO-SKIP-INVISIBLE is non-nil, consider invisible lines as
well."
  (save-excursion
    (cond (pos (goto-char pos)))
    (cond
     (no-skip-invisible
      (line-end-position))
     (t
      (while (progn
               (skip-chars-forward "^\n")
               (invisible-p (point)))
        (goto-char
         (next-single-char-property-change
          (point) 'invisible)))
      (point)))))

;;; Get property at point

(defun ilist-get-property (pos property &optional no-skip-invisible)
  "Get PROPERTY at POS.
If NO-SKIP-INVISIBLE is non-nil, consider invisible lines as
well."
  (declare (side-effect-free t))
  (get-text-property
   (ilist-point-at-eol pos no-skip-invisible) property))

;;; Get index at point

(defun ilist-get-index (&optional no-skip-invisible)
  "Return the index of the element at point.
If point is not at an element, return nil.

If NO-SKIP-INVISIBLE is non-nil, consider invisible lines as
well."
  (declare (side-effect-free t))
  (ilist-get-property (point) 'ilist-index no-skip-invisible))

(defun ilist-get-real-index ()
  "Return the index of the element at point.
If point is not at an element, return nil.

Never skip invisible text."
  (declare (side-effect-free t))
  (ilist-get-property (point) 'ilist-index t))

;;; Get group header

(defun ilist-get-group (&optional no-skip-invisible)
  "Return the group header at point.
If point is not at a group header return nil.

If NO-SKIP-INVISIBLE is non-nil, consider invisible lines as
well."
  (declare (side-effect-free t))
  (ilist-get-property (point) 'ilist-group-header no-skip-invisible))

;;; Whether the line is hidden

(defun ilist-hidden-group-p ()
  "Return t if the group at point is hidden."
  (declare (side-effect-free t))
  (invisible-p
   (intern
    (format
     "[ %s ]"
     (ilist-get-property (point) 'ilist-group-header)))))

;;; marks related

;; It is possible that some user-package does not need the
;; capibilities of marking, so this is provided as an optional
;; feature.  An advantage of this approach is that the user-packages
;; are thus permitted (or encouraged) to implement their own versions
;; of marking, which might better suit their needs.

;;;; mark column

(defun ilist-mark-column-fun (_el)
  "The function that displays the mark.
The function just returns a propertized string, indicating that
the mark column is present.

EL is ignored."
  (declare (pure t) (side-effect-free t))
  (propertize (string #x20) 'ilist-mark-column t))

(defvar ilist-mark-column
  '("" ilist-mark-column-fun nil 1)
  "The column that displays the mark status.
Add this to the list of columns to display marks.")

;;;; find mark columns

(defun ilist-mark-columns (position &optional end)
  "Return the list of mark columns on the same line as POSITION.
The result is in descending order, so the later positions come
first.

If END is non-nil, it specifies the end of the search."
  (declare (side-effect-free t))
  (save-excursion
    (goto-char position)
    (goto-char (line-beginning-position))
    (save-restriction
      (narrow-to-region (point) (or end (line-end-position)))
      (let (res prop-match)
        (while (setq
                prop-match
                ;; search for a non-nil value, and the region ends if
                ;; the value of the property changes, according to the
                ;; doc of the function
                (text-property-search-forward
                 'ilist-mark-column))
          (setq
           res
           (cons
            (cons
             (prop-match-beginning prop-match)
             (prop-match-end prop-match))
            res)))
        res))))

;;;; find marks

(defun ilist-get-marks ()
  "Return the list of mark characters on the line.
The marks are in descending order, i.e. the mark that occurs
later on the line come earlier in the result list.

It is considered a mark only if the value is not t or nil.
Non-mark values are simply ignored."
  (delq
   nil
   (mapcar
    (lambda (cons-cell)
      (let ((value (get-text-property
                    (car cons-cell) 'ilist-mark-column)))
        (cond
         ((and value
               (not (eq value t)))
          value))))
    (ilist-mark-columns (point)))))

;;;; mark

(defun ilist-mark-with-char (char)
  "Mark the elements under point by CHAR.
This function simply sets the text property of
`ilist-mark-column' to CHAR, and the display property to the
character.  If CHAR is not a character, this removes the display
property.

The return value is the same as `ilist-mark-columns'."
  (let ((position-list (ilist-mark-columns (point))))
    (mapc
     ;; each element of the position is a cons cell
     (lambda (cell)
       (add-text-properties
        (car cell)
        (cdr cell)
        (list 'ilist-mark-column char))
       (cond
        ((characterp char)
         (add-text-properties
          (car cell)
          (cdr cell)
          (list 'display
                (make-string (- (cdr cell)
                                (car cell))
                             char))))
        ((remove-text-properties
          (car cell)
          (cdr cell)
          (list 'display nil)))))
     position-list)))

;;;; unmark

(defun ilist-unmark ()
  "Unmark the line under point.
This just sets the text property of `ilist-mark-column' to t."
  ;; we can mark with "CHAR t"
  (ilist-mark-with-char t))

;;;; list of items and their marks

(defun ilist-current-status ()
  "Return the list of items and their mark characters."
  (ilist-map-lines
   (lambda ()
     (cons (get-text-property
            (point) 'ilist-index)
           (get-text-property
            (point) 'ilist-mark-column)))
   (lambda ()
     (get-text-property
      (point) 'ilist-index))))

;;;; map over marked items

;; Nothing to do here, since we can call `ilist-map-lines' with a
;; predicate function that tests whether the text property of
;; 'ilist-mark-column is a non-nil value not equal to t.

(defun ilist-is-marked ()
  "Return t if the current line is marked."
  ;; REVIEW: Maybe we shall not find all columns on the line
  (let* ((columns (ilist-mark-columns (point)))
         (value (cond
                 ((consp columns)
                  (get-text-property (caar columns) 'ilist-mark-column)))))
    (and value (not (eq value t)))))

;;; rounded movements

;;;; boundaries of buffer

(defun ilist-boundary-buffer-p (forward-p)
  "Return whether the point is at a boundary of the buffer.
If FORWARD-P is non-nil, then test whether we are at the end of
the buffer.  Otherwise test whether we are at the beginning of
the buffer."
  (declare (side-effect-free t))
  (cond
   (forward-p (eobp))
   ((or (get-text-property (point) 'ilist-header)
        (get-text-property (point) 'ilist-title-sep)
        (bobp)))))

;;;; skip the boundary

;; REVIEW: Maybe we should call it "round-boundary" instead?
(defun ilist-skip-boundary (rounded forwardp other-end
                                    &optional no-skip-invisible)
  "Skip the boundary of the buffer if needed.
If ROUNDED is non-nil, then try not to stay at the boundary of
the buffer.

FORWARDP determines in which direction to move.

OTHER-END specifies where to go when the boundary is
encountered.

If NO-SKIP-INVISIBLE is non-nil, then invisible lines will not be
skipped."
  (cond
   ((and rounded (ilist-boundary-buffer-p forwardp))
    (goto-char other-end)
    (let ((continuep t))
      (while (and
              continuep
              (or
               (and (not no-skip-invisible)
                    (memq (get-text-property (point) 'invisible)
                          buffer-invisibility-spec))
               (ilist-boundary-buffer-p (not forwardp))))
        (forward-line (cond (forwardp 1) (-1)))
        (cond
         ((ilist-boundary-buffer-p forwardp)
          ;; nowhere to stay, so we just stop
          (setq continuep nil))))))))

;;;; skip properties

(defun ilist-skip-properties (skip-groups
                              forwardp properties
                              &optional no-skip-invisible)
  "Try to skip text PROPERTIES if SKIP-GROUPS is non-nil.
PROPERTIES is a list of text properties to skip.

FORWARDP determines the direction to test for the boundary.

If NO-SKIP-INVISIBLE is non-nil, then invisible lines will not be
skipped."
  (while (and skip-groups
              (or
               (and (not no-skip-invisible)
                    (invisible-p (point)))
               (let ((fake-properties properties)
                     res)
                 (while (and (not res)
                             (consp fake-properties))
                   (setq res
                         (get-text-property
                          (point) (car fake-properties)))
                   (setq fake-properties (cdr fake-properties)))
                 res))
              ;; check boundaries to prevent infinite loops
              (not (ilist-boundary-buffer-p forwardp)))
    (forward-line (cond (forwardp 1) (-1)))))

;;;; moving between lines

(defun ilist-forward-line
    (&optional arg rounded skip-groups no-skip-invisible)
  "Go to ARG th next line.
If ROUNDED is non-nil, assume the top of the buffer is connected
to the bottom of the buffer.

If SKIP-GROUPS is non-nil, try not to stop point on a group
header.

If NO-SKIP-INVISIBLE is non-nil, invisible lines will not be
skipped."
  ;; make sure ARG is a number
  (setq arg (prefix-numeric-value arg))
  (let* ((forwardp (> arg 0))
         (other-end (cond (forwardp (point-min))
                          ((save-excursion
                             (goto-char (point-max))
                             (line-beginning-position)))))
         (original-point (point))
         (arg (abs arg)))
    ;; if we are moving forwards, and if we are at an invisible
    ;; boundary, add one to arg.
    (cond
     ((and forwardp
           (not skip-groups)
           (ilist-boundary-buffer-p nil)
           (invisible-p (point)))
      (setq arg (1+ arg))))
    (ilist-skip-properties t forwardp
                           '(ilist-header ilist-title-sep) t)
    (ilist-skip-properties skip-groups forwardp
                           '(ilist-group-header) t)
    (cond ((and
            (/= original-point (point))
            (not
             (invisible-p (point)))
            (or (null skip-groups)
                (not (ilist-get-group t))))
           (setq arg (1- arg))))
    (setq original-point (point))
    ;; if point is invisible right now, first skip out of it.
    (while (and (not no-skip-invisible)
                (not (ilist-boundary-buffer-p forwardp))
                (invisible-p (point)))
      (forward-line (cond (forwardp 1) (-1))))
    ;; if we are moving backwards, subtract an arg if necessary
    (cond
     ((and (not forwardp)
           (/= original-point (point)))
      (setq arg (1- arg))))
    (while (and (> arg 0) (not (ilist-boundary-buffer-p forwardp)))
      (forward-line (cond (forwardp 1) (-1)))
      ;; skip invisible lines if needed
      (while (and (not no-skip-invisible)
                  (not (ilist-boundary-buffer-p forwardp))
                  (invisible-p (point)))
        (forward-line (cond (forwardp 1) (-1))))
      ;; skip the group and the boundary twice to ensure that we avoid
      ;; the edges as much as possible.
      (ilist-skip-boundary rounded forwardp other-end no-skip-invisible)
      (ilist-skip-properties skip-groups forwardp '(ilist-group-header)
                             no-skip-invisible)
      (ilist-skip-boundary rounded forwardp other-end no-skip-invisible)
      (ilist-skip-properties skip-groups forwardp '(ilist-group-header)
                             no-skip-invisible)
      (setq arg (1- arg)))
    ;; paranoia: but it proves to be needed sometimes
    (ilist-skip-boundary rounded forwardp other-end no-skip-invisible)
    (ilist-skip-properties skip-groups forwardp '(ilist-group-header)
                           no-skip-invisible)
    (ilist-skip-boundary rounded forwardp other-end no-skip-invisible)
    (ilist-skip-properties skip-groups forwardp '(ilist-group-header)
                           no-skip-invisible)))

(defun ilist-backward-line (&optional arg rounded skip-groups)
  "Go to ARG th previous line.
If ROUNDED is non-nil, assume the top of the buffer is connected
to the bottom of the buffer.

If SKIP-GROUPS is non-nil, try not to stop point on a group
header."
  (ilist-forward-line (- (prefix-numeric-value arg))
                      rounded skip-groups))

;;;; moving between group headers

(defun ilist-forward-group-header
    (&optional arg rounded no-skip-invisible)
  "Go to ARG th next group header.
If ROUNDED is non-nil, assume the top of the buffer is connected
to the bottom of the buffer.

If NO-SKIP-INVISIBLE is non-nil, consider invisible lines as
well."
  ;; make sure ARG is a number
  (setq arg (prefix-numeric-value arg))
  (let* ((forwardp (> arg 0))
         (other-end (cond (forwardp (point-min))
                          ((save-excursion
                             (goto-char (point-max))
                             (line-beginning-position)))))
         (original-point (point))
         (arg (abs arg)))
    ;; if we are moving forwards, and if we are at an invisible
    ;; boundary, add one to arg.
    (cond
     ((and forwardp
           (ilist-boundary-buffer-p nil)
           (invisible-p (point)))
      (setq arg (1+ arg))))
    (ilist-skip-properties
     t forwardp '(ilist-header ilist-title-sep) t)
    (cond ((and
            (/= original-point (point))
            (not
             (invisible-p (point)))
            (ilist-get-group t))
           (setq arg (1- arg))))
    (setq original-point (point))
    ;; if point is invisible right now, first skip out of it.
    (while (and (not no-skip-invisible)
                (not (ilist-boundary-buffer-p forwardp))
                (invisible-p (point)))
      (forward-line (cond (forwardp 1) (-1))))
    ;; if we are moving backwards, subtract an arg if necessary
    (cond
     ((and (not forwardp)
           (/= original-point (point)))
      (setq arg (1- arg))))
    (while (and (> arg 0) (not (ilist-boundary-buffer-p forwardp)))
      (forward-line (cond (forwardp 1) (-1)))
      ;; skip invisible lines if needed
      (while (and (not no-skip-invisible)
                  (not (ilist-boundary-buffer-p forwardp))
                  (invisible-p (point)))
        (forward-line (cond (forwardp 1) (-1))))
      ;; skip the group and the boundary twice to ensure that we avoid
      ;; the edges as much as possible.
      (ilist-skip-boundary rounded forwardp
                           other-end no-skip-invisible)
      ;; skip index so that we skip "normal" lines
      (ilist-skip-properties t forwardp '(ilist-index)
                             no-skip-invisible)
      (ilist-skip-boundary rounded forwardp
                           other-end no-skip-invisible)
      (ilist-skip-properties t forwardp '(ilist-index)
                             no-skip-invisible)
      (setq arg (1- arg)))
    ;; paranoia
    (ilist-skip-boundary rounded forwardp
                         other-end no-skip-invisible)
    (ilist-skip-properties t forwardp '(ilist-index)
                           no-skip-invisible)))

(defun ilist-backward-group-header (&optional arg rounded)
  "Go to ARG th previous group header.
If ROUNDED is non-nil, assume the top of the buffer is connected
to the bottom of the buffer."
  (ilist-forward-group-header
   (- (prefix-numeric-value arg)) rounded))

;;; Delete from ALIST

(defun ilist-delete-from-list (ls elements)
  "Remove ELEMENTS from LS.
ELEMENTS are indices of elements to be removed in LS.

Assumes that ELEMENTS is sorted, so that the larger indices come
later.

And the indices are zero-based.

This does not modify LS or ELEMENTS.  It returns a copy of LS
with ELEMENTS removed."
  (declare (pure t) (side-effect-free t))
  ;; REVIEW: In our case, since both LS and ELEMENTS are sorted, we
  ;; might have a faster implementation which employs the sorted-ness
  ;; of the arguments, but I think it is pre-mature optimisation.
  (let* ((temp (copy-tree ls)))
    ;; NOTE: Using `mapc' is faster than a while loop, as the manual
    ;; says. Since `dolist' is in essence a while loop, using `mapc'
    ;; will be faster. Of course for our purposes this is premature
    ;; optimisation.
    (mapc
     (lambda (index)
       (cond
        ((> index 0)
         ;; Using `setcdr' is more efficient but destructively
         ;; modifies the list. So we used `copy-tree' to prevent the
         ;; destructions.
         (setcdr (nthcdr (1- index) temp)
                 (nthcdr (1+ index) temp)))
        ((setq temp (cdr temp)))))
     (reverse elements))
    temp))

;;; major mode

;; This major mode is the basis that should be derived by
;; user-packages.

(define-derived-mode ilist-mode special-mode "IList"
  "Display a list in a similar fashion to ibuffer."
  (setq truncate-lines t)
  ;; This is to make sure that by default the bookmarks are not
  ;; hidden.
  (setq buffer-invisibility-spec (list t)))

;; It is intentional that no key-bindings are defined.

(provide 'ilist)
;;; ilist.el ends here
