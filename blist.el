;;; blist.el --- Display bookmarks in an ibuffer way  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022, 2023, 2024  Free Software Foundation, Inc.

;; Author: Durand <durand@jsdurand.xyz>
;; Keywords: convenience
;; URL: https://gitlab.com/mmemmew/blist
;; Package-Requires: ((ilist "0.4") (emacs "24"))
;; Version: 0.4

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

;; This package uses the package `ilist' to display bookmarks in a
;; similar fashion to ibuffer.  The core functionalities are provided
;; by `ilist'.  This pacakge solely provides interactive functions
;; that are built upon `ilist'.

;;; Code:

;;; Dependencies

(require 'bookmark) ;; of course
(require 'ilist)    ;; core engine

;;; Customization options

;;;; Group

(defgroup blist ()
  "Display bookmarks in an Ibuffer manner."
  :group 'bookmark)

;;;; Buffer name

(defcustom blist-buffer-name "*Bookmark List*"
  "The name of the buffer used to display bookmarks."
  :type 'string)

;;;; Display location or not

(defcustom blist-display-location-p t
  "Whether or not display the locations of bookmarks."
  :type 'boolean
  :local t)

;;;; No confirm deletion

(defcustom blist-expert nil
  "If non-nil, don't ask to confirm the deletion of bookmarks."
  :type 'boolean)

;;;; Filter groups

;;;;; Default label

(defcustom blist-filter-default-label "Default"
  "The label used for the default group automatically added."
  :type 'string)

;;;;; fixed filter groups

(defcustom blist-filter-groups (list
                                (cons "Eshell" #'blist-eshell-p)
                                (cons "Default" #'blist-default-p))
  "The filter groups for display.
This specifies a fixed filter group.  For the automatic filter
groups, see the user option `blist-automatic-filter-groups'.

See the documentation string of `ilist-string' to know the
differences between the two.

And see the user option `blist-filter-features' for how to
combine fixed groups and automatic ones.

If `blist-default-p' is not used as a criterion, then a default
filter group will be appended at the end."
  :type '(repeat (cons string function)))

;;;;; automatic filter groups

(defcustom blist-automatic-filter-groups
  #'blist-automatic-filter-groups-default
  "The automatic filter group for display.
See the documentation string of `ilist-string' for how an
automatic filter group should behave.

And see the user option `blist-filter-features' for how to
combine fixed groups and automatic ones."
  :type 'function)

;;;;; Default sorter

(defun blist-filter-sorter-default (x y)
  "The function used to sort group labels.
Return non-nil if and only if group X should come before group
Y.

Used by `ilist-dag' to define an automatic filter group."
  (cond
   ((string= x blist-filter-default-label) nil)
   ((string= y blist-filter-default-label))
   ((string-lessp x y))))

;;;;; Default automatic grouping

;; See the documentation string of `ilist-dag' for more details.

(ilist-dag "blist-default" blist-filter-default-label
           #'blist-filter-sorter-default
  (save-match-data
    (let* ((handler (bookmark-get-handler element))
           (handler-name (and handler (format "%S" handler)))
           (handler-type
            (cond
             ;; This is added in Emacs 29, so we test if this function
             ;; is defined.
             ((functionp #'bookmark-type-from-full-record)
              (bookmark-type-from-full-record element))))
           (location (bookmark-location element)))
      ;; replace repeating parts
      (cond
       ((and handler-name
             (string-match "\\([^z-a]+\\)-jump" handler-name))
        (setq
         handler-name
         (replace-regexp-in-string
          "-bookmark$" "" (match-string 1 handler-name)))))
      ;; take case of file extensions
      (cond
       ((and (null handler-name) location
             (string-match "\\.\\([^.]+\\)\\'" location))
        (setq handler-name (match-string 1 location))))
      (cond
       ;; If the type is available, we use it as the group header.
       (handler-type)
       (handler-name
        (cond
         ;; Some special cases
         ((string-match-p "pdf" handler-name) "PDF")
         ((string-match-p "^el$" handler-name) "ELisp")
         ((<= (length handler-name) 3) (upcase handler-name))
         ((capitalize handler-name))))))))

;;;;; An alternative, type-only, automatic grouping

;; An alternative automatic group that only uses types of handlers as
;; group headers.

;; NOTE: bookmark-type-from-full-record is added in Emacs 29, so it is
;; unavailable before this version, and hence this automatic filter
;; group would always use the default group.

(ilist-dag "blist-type-only" "Default" #'blist-filter-sorter-default
  (cond
   ((functionp #'bookmark-type-from-full-record)
    (bookmark-type-from-full-record element))))

;; For discoverability
(defalias 'blist-automatic-filter-groups-default
  #'ilist-automatic-group-blist-default)

;;;;; Method to combine fixed and automatic groups

(defcustom blist-filter-features '(manual)
  "How to combine the fixed filter groups and the automatic one.
This is a list of symbols.

If the symbol 'manual' is an element of this list, then the fixed
filter group specified by the variable `blist-filter-groups' will
be used.

If the symbol 'auto' is an element of this list, then the
automatic filter group specified by the variable
`blist-automatic-filter-groups' will be used.

If both symbols 'auto' and 'manual' are elements of the list,
then the elements will be grouped by the fixed groups first.
Those elements that are not classified as belonging to any of the
fixed groups will then be grouped by the automatic filter
groups.

If neither of the symbols 'auto' and 'manual' is an element of
the list, then every bookmark will be displayed under the
group labelled with `blist-filter-default-label'."
  :type '(repeat
          (choice
           (const :tag "use fixed filter groups" manual)
           (const :tag "use automatic filter groups" auto))))

;;;;; Combine both fixed grouping and automatic grouping

(defun blist-filter-groups (element &optional type)
  "The actual filter group that is passed to `ilist-string'.
See the documentation strings of the variables
`blist-filter-groups', `blist-automatic-filter-groups', and
`blist-filter-features' for details.

See the documentation string of `ilist-string' for the meaning of
ELEMENT and TYPE."
  (let ((level 0)
        (fixed-group-labels (mapcar #'car blist-filter-groups)))
    ;; convert the list of features to a number for easier
    ;; identification of the situation
    (cond ((memq 'manual blist-filter-features)
           (setq level (1+ level))))
    (cond ((memq 'auto blist-filter-features)
           (setq level (+ level 2))))
    (cond
     ((= level 0)
      ;; no user filtering in effect: we just return the default label
      ;; for the default, and return nil for the rest.  This works
      ;; because if type is nil, then this function will return nil,
      ;; and then `ilist-string' will use the default label.  When it
      ;; asks for the sorter, this returns nil as well, which tells
      ;; `ilist-string' that groups should not be sorted.
      ;;
      ;; Perfect design, right?
      (cond ((eq type 'default) blist-filter-default-label)))
     ((= level 1)
      ;; Only manual filtering is in effect.
      (cond
       ;; Return the default label no matter what
       ((eq type 'default) blist-filter-default-label)
       ((eq type 'sorter)
        (lambda (x y)
          ;; If member returns a longer string, that means the element
          ;; comes first.  This includes the case if the label is not
          ;; on the list.
          (<= (length (member y fixed-group-labels))
              (length (member x fixed-group-labels)))))
       ;; Emulate fixed grouping by automatic grouping
       ((let ((temp-ls blist-filter-groups)
              result)
          (while (and (null result) (consp temp-ls))
            (cond
             ((funcall (cdar temp-ls) element)
              (setq result (caar temp-ls))))
            (setq temp-ls (cdr temp-ls)))
          result))))
     ((= level 2)
      ;; Only automatic grouping in effect.
      (funcall blist-automatic-filter-groups element type))
     ((= level 3)
      ;; Both the manual and the automatic grouping are in effect.
      (cond
       ((eq type 'default)
        ;; Why not use or, you ask?  Because I like cond.  :D
        (cond
         ;; If the fixed group has a default group, then why use
         ;; automatic grouping as well?  But who am I to judge?
         ((car (rassq #'blist-default-p blist-filter-groups)))
         ((funcall blist-automatic-filter-groups t 'default))
         (blist-filter-default-label)))
       ((eq type 'sorter)
        ;; Manual groups should come before automatic groups.
        (lambda (x y)
          (cond
           ((and (member x fixed-group-labels)
                 (member y fixed-group-labels))
            (<= (length (member y fixed-group-labels))
                (length (member x fixed-group-labels))))
           ((member x fixed-group-labels))
           ((member y fixed-group-labels) nil)
           ((functionp
             (funcall blist-automatic-filter-groups t 'sorter))
            (funcall
             (funcall blist-automatic-filter-groups t 'sorter)
             x y)))))
       ((let ((temp-ls blist-filter-groups)
              result)
          (while (and (null result) (consp temp-ls))
            (cond
             ((funcall (cdar temp-ls) element)
              (setq result (caar temp-ls))))
            (setq temp-ls (cdr temp-ls)))
          (cond
           (result)
           ((funcall blist-automatic-filter-groups element))))))))))

;;;; Empty groups

(defcustom blist-discard-empty-p t
  "Whether to display empty groups or not."
  :type 'boolean)

;;;; Cycle movement

(defcustom blist-movement-cycle t
  "Whether \"round\" the buffer when moving.
To round the buffer means to assume that the top of the buffer is
identified with the bottom of the buffer."
  :type 'boolean)

;;;; Maximal displayed bookmark name length

(defcustom blist-maximal-name-len 0.4
  "The maximal length of the displayed bookmark name."
  :type '(choice
          (integer :tag "Number of characters")
          (float :tag "Fraction of window body width")))

;;;; Eliding string

(defcustom blist-elide-string "..."
  "The string to put at the end of a string that is too long."
  :type 'string)

;;;; Deletion mark character

(defcustom blist-deletion-mark ?D
  "The character to mark a bookmark for deletion."
  :type 'character)

;;;; Default mark character

(defcustom blist-default-mark ?>
  "The character that is used for mark by default."
  :type 'character)

;;;; Sorter

(defcustom blist-default-sorter nil
  "The default sorter.
See `blist-sorter'."
  :type '(choice
          (const nil :tag "No sorting")
          (function :tag "Sorting function")))

;;;; Annotation Column

;;;;; Annotation column getter

(defun blist-get-annotation (bookmark)
  "Return if BOOKMARK has annotation.
If BOOKMARK has no annotation, return a space string."
  (cond
   ((let ((annotation (bookmark-get-annotation bookmark)))
      (and (stringp annotation) (not (string= annotation ""))))
    "*")
   (" ")))

;;;;; Real column definition

(defcustom blist-annotation-column-name "A"
  "The name of the column showing whether a bookmark has \
annotations.

Only the first letter will be shown."
  :type 'string)

(defun blist-annotation-column ()
  "The specification of the ANNOTATION column."
  (declare (side-effect-free t))
  (list blist-annotation-column-name #'blist-get-annotation
        1 1 :left nil))

;;;; How to open multiple bookmarks?

(defcustom blist-select-manner (list 'vertical)
  "How to select multiple bookmarks.
The value should be a list of symbols.  Allowed symbols are as
follows.

- vertical:        open bookmarks vertically.
- horizontal:      open bookmarks horizontally.  Vertical takes
                   precedence.
- spiral:          open bookmark in a spiral manner.  If vertical
                   is also present, spiral vertically; otherwise
                   spiral horizontally.  By default toward right
                   / down, unless left / up is present.
- main-side:       open bookmarks with the first in a main
                   window, and the other windows are put in side
                   splits.  If vertical, then the side splits are
                   either at the bottom or at the top; if
                   horizontal, then the sides are at the left or
                   the right.  This overrides spiral.
- left:            the direction to open.  The bookmarks that are
                   higher on the list are opened on the right.
- right, up, down: similar to left.  Left takes precedence over
                   right, and up takes precedence over down.
- tab:             open the bookmarks in a new tab.  Requires
                   'tab-bar if used.

As a quick shortcut, if the list does not contain left, it means
to use right; if no up, it means down; if no vertical, it means
horizontal.

There will be no errors if there are unrecognized symbols in the
list; they are simply ignored."
  :type '(repeat
          (choice
           (const :tag "Vertically" vertical)
           (const :tag "Horizontally" horizontal)
           (const :tag "Spirally" spiral)
           (const
            :tag "A main window along with side splits"
            main-side)
           (const :tag "Towards Left" left)
           (const :tag "Towards Right" right)
           (const :tag "Towards Up" up)
           (const :tag "Towards Down" down)
           (const :tag "In a new tab" tab))))

;;;; Edit bookmark annotation buffer name

(defcustom blist-edit-annotation-buffer-name
  "*Edit Bookmark Annotation*"
  "The name of the buffer used for editing bookmark annotation."
  :type 'string)

;;;; Whether to use header or not

(defcustom blist-use-header-p nil
  "If non-nil, show a header of column names as well."
  :type 'boolean)

;;;; Whether to show the number of items

(defcustom blist-show-item-num-p nil
  "If non-nil, the number of items is shown in the group header.")

;;; Variables

;;;; Sorter

(defvar blist-sorter blist-default-sorter
  "The function used to sort the bookmark list.
See `ilist-string' for how the sorter should behave.")

;;;; Rename history

(defvar blist-rename-history nil
  "The variable that stores the input history of `blist-rename'.")

;;;; Deleted entries

(defvar-local blist-deleted-indices nil
  "The variable that stores deleted indices.
This is used to record which indices have been deleted in
re-building the list of bookmarks.")

;;; Display

;;;; Columns

;;;;; Name Column

(defun blist-name-column ()
  "Return the column specification for NAME."
  (declare (pure t) (side-effect-free t))
  (list
   "Name" #'bookmark-name-from-full-record
   5 (cond
      ((integerp blist-maximal-name-len)
       blist-maximal-name-len)
      ((and (floatp blist-maximal-name-len)
            (< 0 blist-maximal-name-len 1))
       (floor
        (* (window-body-width) blist-maximal-name-len)))
      ((user-error "`blist-maximal-name-len' should be either \
an integer or a float between 0 and 1")))
   :left
   "..."))

;;;;; Location column

(defun blist-get-location (bookmark)
  "Return the location of BOOKMARK.
The location of a bookmark is either its filename, or its
location, in case it is not associated with a file."
  (declare (pure t) (side-effect-free t))
  (or (bookmark-get-filename bookmark)
      (bookmark-prop-get bookmark 'location)))

(defvar blist-location-column
  (list "Location" #'blist-get-location
        9 nil :left nil)
  "The specification for the LOCATION column.")

;;;; Groups

;;;;; Convenient macro

;; The user is expected to define groups through this macro.

(defmacro blist-define-criterion (name doc-name &rest forms)
  "Define a function to determine whether a bookmark is of the \
required type.

NAME is part of the name of the resulting function; specifically,
the resulting function is named blist-NAME-p.

Its documentation string uses DOC-NAME to describe its effect.

FORMS are ELisp forms that should return non-nil when BOOKMARK is
of the required type."
  (declare (indent 2))
  (append
   (list
    'defun (intern (format "blist-%s-p" name))
    (list 'bookmark)
    (format "Determine whether BOOKMARK is of type %s." doc-name)
    (list 'declare (list 'pure t) (list 'side-effect-free t)))
   forms))

;;;;; Eshell group

;; As an example, a group is defined by default

(blist-define-criterion "eshell" "Eshell"
  (eq (bookmark-get-handler bookmark) #'eshell-bookmark-jump))

;;;;; Default group

(blist-define-criterion "default" "DEFAULT" (ignore bookmark) t)

;;;; List function

;; an alias for discoverability

(defalias 'blist #'blist-list-bookmarks)

;; REVIEW: Is it a good idea to preserve the hidden status of groups?

;;;###autoload
(defun blist-list-bookmarks (&rest _args)
  "List bookmarks in an ibuffer fashion.
The bookmarks are taken from `bookmark-alist'.

The ARGS is there so as to accept arguments in order for it to be
used as a `revert-buffer-function'."
  (interactive)
  ;; load the bookmark if needed
  (bookmark-maybe-load-default-file)
  (let ((buffer (get-buffer-create blist-buffer-name))
        (first-time-generated t)
        blist-header-line-format)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            front rear group pos)
        (widen)
        (cond
         ((and (derived-mode-p 'blist-mode)
               (or (ilist-get-index)
                   (ilist-get-group)))
          (setq first-time-generated nil)
          ;; already in the bookmark; find the previous and the next
          ;; bookmarks that still exist, only if the point is on a
          ;; group or an item
          (let ((orig (line-beginning-position)))
            (ilist-map-lines
             (lambda ()
               (cond
                ((and
                  (ilist-get-index)
                  (memq (ilist-get-index) blist-deleted-indices)))
                ;; we don't use the index of the line to find the
                ;; entry since that index, after possibly a previous
                ;; deletion, might no longer be valid
                ((setq front
                       (buffer-substring-no-properties
                        (+ (line-beginning-position) 4)
                        (line-end-position))))))
             nil nil orig)
            (save-excursion
              (goto-char orig)
              (forward-line 1)
              (while (and (not (eobp)) (null rear))
                (cond
                 ((and
                   (ilist-get-index)
                   (memq (ilist-get-index) blist-deleted-indices)))
                 ((setq
                   rear
                   (buffer-substring-no-properties
                    (+ (line-beginning-position) 4)
                    (line-end-position)))))
                (forward-line 1)))
            ;; reset the indices
            (setq blist-deleted-indices nil))
          ;; if on a group, try to restore the position at that group
          (setq group (ilist-get-group)))
         ((derived-mode-p 'blist-mode)
          (setq first-time-generated nil)
          ;; not on a group or an item: simply record the position
          (setq pos (line-beginning-position))))
        (delete-region (point-min) (point-max))
        (insert
         (ilist-string
          bookmark-alist
          (append
           (list ilist-mark-column
                 (blist-annotation-column)
                 (blist-name-column))
           (cond
            (blist-display-location-p (list blist-location-column))))
          #'blist-filter-groups
          blist-discard-empty-p
          blist-sorter
          t
          blist-show-item-num-p))
        (goto-char (point-min))
        ;; set the header if necessary
        (cond
         (blist-use-header-p
          (setq blist-header-line-format
                (buffer-substring-no-properties
                 (point) (line-end-position)))))
        (cond
         ((and
           (stringp rear)
           (search-forward rear nil t))
          (goto-char (match-beginning 0)))
         ;; if no rear is found, but there is a front, then go to the
         ;; end of the buffer so that we can search the front
         ((stringp front) (goto-char (point-max))))
        (cond
         ((and
           (stringp front)
           (search-backward front nil t))
          (goto-char (1+ (match-end 0)))))
        (cond
         ((stringp group)
          ;; ignore errors
          (condition-case nil
              (blist-jump-to-group group)
            (user-error))))
        (cond ((integer-or-marker-p pos) (goto-char pos))
              ((goto-char (line-beginning-position)))))
      (blist-mode))
    (display-buffer buffer)
    (select-window (get-buffer-window blist-buffer-name))
    ;; if generated for the first time, advance a line
    (cond
     (first-time-generated (ilist-forward-line 1 nil t)))
    (cond (blist-header-line-format
           (setq header-line-format blist-header-line-format)
           (with-silent-modifications
             (add-text-properties
              (point-min)
              (save-excursion
                (goto-char (point-min)) (forward-line 2) (point))
              (list (intern "invisible") t)))))))

;;; Major mode

(define-derived-mode blist-mode ilist-mode "BList"
  "Major mode used in the display of `blist-list-bookmarks'.

\\{blist-mode-map}"
  :group 'blist
  (setq-local revert-buffer-function #'blist-list-bookmarks))

;;;; Key-bindings

;; TODO: Sorting

(let ((map blist-mode-map))
  (define-key map (vector ?n) #'blist-next-line)
  (define-key map (vector #x20) #'next-line)
  (define-key map (vector ?p) #'blist-prev-line)
  (define-key map (vector ?N) #'blist-next-item)
  (define-key map (vector ?P) #'blist-prev-item)
  (define-key map (vector ?\M-n) #'blist-next-group)
  (define-key map (vector ?\M-p) #'blist-prev-group)
  (define-key map (vector 'tab) #'blist-next-group)
  (define-key map (vector 'backtab) #'blist-prev-group)
  (define-key map (vector 'return) #'blist-return)
  (define-key map (vector 'S-return) #'blist-toggle-other-groups)
  (define-key map (vector ?\C-m) #'blist-return)
  (define-key map (vector ?\C-\S-m) #'blist-toggle-other-groups)
  (define-key map (vector ?o) #'blist-open-other-window)
  (define-key map (vector ?v) #'blist-select)
  (define-key map (vector ?r) #'blist-rename)
  (define-key map (vector ?w) #'blist-locate)
  (define-key map (vector ?R) #'blist-relocate)
  (define-key map (vector ?a) #'blist-show-annotation)
  (define-key map (vector ?A) #'blist-show-all-annotations)
  (define-key map (vector ?e) #'blist-edit-annotation)
  (define-key map (vector ?s) #'bookmark-save)
  (define-key map (vector ?\C-x ?\C-s) #'bookmark-save)
  (define-key map (vector ?l) #'blist-load)
  (define-key map (vector #x29) #'blist-next-marked)
  (define-key map (vector #x28) #'blist-prev-marked)
  (define-key map (vector ?\M-}) #'blist-next-marked)
  (define-key map (vector ?\M-{) #'blist-prev-marked)
  (define-key map (vector ?m) #'blist-mark)
  (define-key map (vector ?d) #'blist-mark-for-deletion)
  (define-key map (vector ?\C-d) #'blist-mark-for-deletion-backward)
  (define-key map (vector ?k) #'blist-mark-for-deletion)
  (define-key map (vector ?D) #'blist-delete-marked)
  (define-key map (vector ?x) #'blist-do-delete)
  (define-key map (vector ?u) #'blist-unmark-forward)
  (define-key map (vector 'backspace) #'blist-unmark-backward)
  (define-key map (vector 'M-backspace) #'blist-unmark-all-mark)
  (define-key map (vector ?U) #'blist-unmark-all)
  (define-key map (vector ?t) #'blist-toggle-marks)
  (define-key map (vector ?T) #'blist-toggle-location)
  (define-key map (vector ?* ?*) #'blist-unmark-all-mark)
  (define-key map (vector ?* ?c) #'blist-change-marks)
  (define-key map (vector ?% ?n) #'blist-mark-by-name)
  (define-key map (vector ?% ?l) #'blist-mark-by-location)
  (define-key map (vector ?j) #'blist-jump-to-line)
  (define-key map (vector ?J) #'blist-jump-to-group)
  (define-key map (vector ?i) #'blist-show-info)
  (define-key map (vector ?\M-j) #'blist-jump-to-group)
  (define-key map (vector ?\M-g) #'blist-jump-to-line)
  (define-key map (vector ?\M-G) #'blist-jump-to-group))

;;; Major mode functions

;;;; Assert

(defun blist-assert-mode ()
  "Ensure we are in `blist-mode' derived buffer."
  (cond
   ((derived-mode-p 'blist-mode))
   ((user-error "Not in `blist-mode'"))))

;;;; The range of operation

(defun blist-operate-range (arg
                            &optional
                            use-default-p
                            default-start
                            default-end)
  "Return the range for the operation.
If region is active, use the region.

If point is on a group header, use ARG groups.

Otherwise use ARG lines.

If USE-DEFAULT-P is non-nil, use DEFAULT-START and DEFAULT-END
instead of using ARG lines in the last case.

Return a cons cell (start . end) of start and end of the range.

Note that this function moves point to the other end of the
range, unless region is active.

Note that invisible lines will not be considered here."
  (let (start end no-sort-p)
    (cond
     ((use-region-p)
      (setq start (region-beginning))
      (setq end (region-end)))
     ((get-text-property (point) 'ilist-group-header)
      (setq start (point))
      (setq end (progn
                  (ilist-forward-group-header arg)
                  (point))))
     (use-default-p
      (setq no-sort-p t)
      (setq start default-start)
      (setq end default-end))
     ((setq start (point))
      (setq end (progn
                  (ilist-forward-line arg nil t)
                  (point)))))
    (cond
     (no-sort-p (cons start end))
     ((cons (min start end) (max start end))))))

;;;; Open bookmarks

;; I almost forgot that I want to open bookmarks.

(defun blist-open ()
  "Open the bookmark at point."
  (interactive)
  (blist-assert-mode)
  (cond
   ((ilist-get-index)
    (bookmark-jump
     (bookmark-name-from-full-record
      (nth (ilist-get-index) bookmark-alist))))
   ((user-error "No bookmark to open on this line"))))

;;;; Open in another window

(defun blist-open-other-window ()
  "Open the bookmark at point in another window."
  (interactive)
  (blist-assert-mode)
  (cond ((ilist-get-index)
         (bookmark--jump-via
          (nth (ilist-get-index) bookmark-alist)
          #'switch-to-buffer-other-window))
        ((user-error "No bookmark to open on this line"))))

;;;; blist-select

;;;;; Prepare windows

(defun blist-prepare-select-windows (num manner)
  "Create and return NUM windows according to MANNER.

NUM should be a positive integer.

See `blist-select-manner' for what MANNER should look like."
  (cond
   ((or (not (integerp num))
        (<= num 0))
    (error "NUM should be a positive integer, but got %S" num)))
  (let* ((mainp (memq 'main-side manner))
         (tabp (cond (mainp nil) ((memq 'tab manner))))
         (verticalp (memq 'vertical manner))
         (leftp (memq 'left manner))
         (upp (memq 'up manner))
         (spiralp (memq 'spiral manner))
         (size (cond
                ;; spirals split in half
                (spiralp nil)
                ((and mainp (> num 1) verticalp)
                 (floor (frame-width) (1- num)))
                ((and mainp (> num 1))
                 (floor (frame-height) (1- num)))
                (mainp nil)
                (verticalp (floor (frame-height) num))
                ((floor (frame-width) num))))
         (current-direction verticalp)
         (orig-window (selected-window))
         temp-window windows main-side-splitted-p)
    (cond (tabp (require 'tab-bar) (tab-bar-new-tab)))
    ;; create a new window so that we are not inside some window that
    ;; cannot be splitted, like a side window
    (select-window (split-window (frame-root-window) nil 'below))
    (delete-other-windows)
    (setq orig-window (selected-window))
    (setq windows (cons orig-window windows))
    (setq temp-window orig-window)
    (setq num (1- num))
    (while (> num 0)
      (setq
       temp-window
       (split-window temp-window
                     (cond
                      ((and mainp (not main-side-splitted-p))
                       nil)
                      (size))
                     (cond
                      (current-direction
                       ;; vertical
                       (cond (upp 'above) ('below)))
                      ;; horizontal
                      (leftp 'left)
                      ('right))))
      (setq windows (cons temp-window windows))
      ;; change direction for spirals and change direction only once
      ;; for main-side
      (cond (spiralp
             (setq current-direction (not current-direction))
             ;; spirals change the horizontal / vertical directions as
             ;; well
             (cond
              (current-direction
               (setq leftp (not leftp)))
              ((setq upp (not upp)))))
            ((and mainp (not main-side-splitted-p))
             (setq current-direction (not current-direction))
             (setq main-side-splitted-p t)))
      (setq num (1- num)))
    (reverse windows)))

;;;;; select function

(defun blist-select (&optional arg)
  "Open all marked bookmarks.
If there are no marked bookmarks, and if the point is on a group
header, open all bookmarks of the group.

If there are no marked bookmarks, and if the point is on a
bookmark line, then open the bookmark on that line.

Otherwise signal an error.

If ARG is non-nil, i.e. if called with \\[universal-argument] in
interactive uses, read a list for how to select multiple
bookmarks.  Otherwise, the variable `blist-select-manner'
controls how multiple bookmarks are selected."
  (interactive "P")
  (blist-assert-mode)
  (let* ((marked-items (ilist-map-lines #'ilist-get-index
                                        #'ilist-is-marked))
         (marked-items
          (cond
           (marked-items)
           ((ilist-get-group)
            (let ((start (point))
                  (end (save-excursion
                         (ilist-forward-group-header 1)
                         (point))))
              (ilist-map-lines
               #'ilist-get-index #'ilist-get-index start end)))
           ;; HACK: if not on a normal line, it will return nil, so
           ;; that cond will skip this clause
           ((delq nil (list (ilist-get-index))))
           ((user-error "No bookmarks to open"))))
         (marked-items (mapcar
                        (lambda (index) (nth index bookmark-alist))
                        marked-items))
         (manner (cond
                  (arg
                   (mapcar
                    #'intern
                    (completing-read-multiple
                     "How to select multiple bookmarks: "
                     (list
                      'vertical 'horizontal 'spiral 'main-side
                      'left 'right 'up 'down 'tab)
                     nil t)))
                  (blist-select-manner)))
         (windows (blist-prepare-select-windows
                   (length marked-items) manner))
         (orig-window (car windows)))
    (while (consp windows)
      (select-window (car windows))
      (bookmark-jump
       (bookmark-name-from-full-record (car marked-items)))
      (setq marked-items (cdr marked-items))
      (setq windows (cdr windows)))
    (select-window orig-window)))

;;;; rename

(defun blist-rename (old new)
  "Rename the bookmark at point by NEW.
If the current buffer is in `blist-mode', this also runs
`revert-buffer' afterwards.

If called with \\[universal-argument], or if there is no bookmark
at point, prompt for the OLD bookmark to rename."
  (interactive
   (list
    (cond
     ((or current-prefix-arg
          (null (ilist-get-index)))
      (let* ((names (mapcar
                     #'bookmark-name-from-full-record
                     bookmark-alist))
             (default (cond
                       ((ilist-get-index)
                        (bookmark-name-from-full-record
                         (nth (ilist-get-index)
                              bookmark-alist)))))
             (prompt (cond
                      (default (format "Rename bookmark (%s): "
                                       default))
                      ("Rename bookmark: "))))
        (completing-read prompt names nil t nil
                         'blist-rename-history default)))
     ((bookmark-name-from-full-record
       (nth (ilist-get-index) bookmark-alist))))
    (read-string "Rename bookmark to: " nil 'blist-rename-history
                 (cond
                  ((ilist-get-index)
                   (bookmark-name-from-full-record
                    (nth (ilist-get-index) bookmark-alist)))))))
  (blist-assert-mode)
  ;; error out for invalid input
  (cond
   ((or (not (and
              (stringp old)
              (not (string= old ""))))
        (not (and
              (stringp new)
              (not (string= new "")))))
    (error "OLD and NEW should be non-empty strings, \
but got %S and %S"
           old new)))
  (bookmark-set-name old new)
  (cond ((derived-mode-p 'blist-mode) (revert-buffer)))
  ;; increase modification count
  (setq bookmark-alist-modification-count
        (1+ bookmark-alist-modification-count))
  ;; save if the user wants to
  (cond ((bookmark-time-to-save-p) (bookmark-save))))

;;;; locate

(defun blist-locate ()
  "Display the location of the bookmark at point in the echo area."
  (interactive)
  (blist-assert-mode)
  (cond
   ((and (ilist-get-index)
         (blist-get-location
          (nth (ilist-get-index) bookmark-alist)))
    (message (blist-get-location
              (nth (ilist-get-index) bookmark-alist))))
   ((user-error "Unknown location"))))

;;;; relocate

(defvar blist-relocate-history nil
  "The history variable of `blist-relocate'.")

(defun blist-relocate (bookmark)
  "Relocate BOOKMARK to another location.

If the BOOKMARK has a attribute called relocater, call the value
of the attribute, which should be a function with one argument:
the BOOKMARK itself, to ask for the new location.  Custom
bookmark records can set this attribute to offer customized
relocating behaviour.

If not, use `read-file-name' to specify the new file name to
point to.

Otherwise, signal an error.

If called with \\[universal-argument], or if point is not at a
bookmark, use `completing-read' to let the user choose which
bookmark to relocate.

Otherwise, if point is at a bookmark, relocate that bookmark."
  (interactive
   (list
    (let* ((default (cond
                     ((ilist-get-index)
                      (bookmark-name-from-full-record
                       (nth (ilist-get-index) bookmark-alist)))))
           (prompt (cond (default
                           (format "Bookmark to relocate [%s]: "
                                   default))
                         ("Bookmark to relocate: "))))
      (cond
       (current-prefix-arg
        (completing-read
         prompt
         ;; copied from `bookmark-relocate'
         (lambda (str pred action)
           (if (eq action 'metadata)
               '(metadata (category . bookmark))
             (complete-with-action action bookmark-alist str pred)))
         nil t nil 'blist-relocate-history default))
       (default)
       ((user-error "No bookmark to relocate"))))))
  (blist-assert-mode)
  ;; paranoid
  (bookmark-maybe-load-default-file)
  (let* ((file-name (bookmark-get-filename bookmark))
         (location (bookmark-prop-get bookmark 'location))
         (file-or-location (or file-name location))
         (relocater (bookmark-prop-get bookmark 'relocater))
         (prompt (format "Relocate %s to: " bookmark))
         (new-location
          (cond
           ((functionp relocater)
            (funcall relocater bookmark))
           ((read-file-name
             prompt (file-name-directory file-or-location)
             file-or-location)))))
    (cond
     (file-name (bookmark-set-filename bookmark new-location))
     (location (bookmark-prop-set bookmark 'location new-location)))
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (cond ((bookmark-time-to-save-p) (bookmark-save)))
    (cond ((derived-mode-p 'blist-mode) (revert-buffer)))))

;;;; show annotations

(defun blist-show-annotation (&optional arg)
  "Show the annotation of the bookmark(s) in another window.
Only the annotations of bookmarks with annotations will be shown.
So empty annotations are ignored.

If there are marked bookmarks, show the annotations of the marked
bookmarks; otherwise show the annotations of the bookmark at
point.  If ARG is non-nil, i.e. if called with
\\[universal-argument] in interactive calls, and if there is no
bookmark at point, use `completing-read' to choose one."
  (interactive "P")
  (blist-assert-mode)
  (let* ((marked-items
          (mapcar
           (lambda (index)
             (bookmark-name-from-full-record
              (nth index bookmark-alist)))
           (ilist-map-lines
            #'ilist-get-index
            (lambda ()
              (and (ilist-is-marked) (blist-is-annotated-p))))))
         (targets
          (cond
           (marked-items)
           ((mapcar
             (lambda (index)
               (bookmark-name-from-full-record
                (nth index bookmark-alist)))
             (delq nil (list (cond ((blist-is-annotated-p)
                                    (ilist-get-index)))))))
           (arg
            (let ((items (delq
                          nil
                          (mapcar
                           (lambda (index)
                             (cond
                              ((let* ((bookmark
                                       (nth index bookmark-alist))
                                      (annotation
                                       (bookmark-get-annotation
                                        bookmark)))
                                 (and (stringp annotation)
                                      (not (string= annotation ""))))
                               (bookmark-name-from-full-record
                                (nth index bookmark-alist)))))
                           (blist-all-bookmarks)))))
              (cond
               (items
                (list
                 (completing-read
                  "Choose a bookmark to show annotation: "
                  (lambda (str pred action)
                    (if (eq action 'metadata)
                        '(metadata (category . bookmark))
                      (complete-with-action
                       action items str pred)))
                  nil t)))
               ((user-error
                 (concat
                  "No bookmarks selected or the selected "
                  "bookmarks have no annotations"))))))
           ((user-error
             (concat
              "No bookmarks selected or the selected "
              "bookmarks have no annotations"))))))
    (blist-show-all-annotations targets)))

;;;; show all annotations

(defun blist-show-all-annotations (targets)
  "Show the annotation of all bookmarks of TARGETS in another \
window.

If called interactively, show all annotations of bookmarks with
annotations."
  (interactive
   (list
    (delq
     nil
     (mapcar
      (lambda (index)
        (cond
         ((let* ((bookmark (nth index bookmark-alist))
                 (annotation (bookmark-get-annotation bookmark)))
            (and (stringp annotation) (not (string= annotation ""))))
          (bookmark-name-from-full-record
           (nth index bookmark-alist)))))
      (blist-all-bookmarks)))))
  (blist-assert-mode)
  (cond
   ((null targets)
    (user-error "No annotations to show"))
   ((save-selected-window
      (pop-to-buffer (get-buffer-create "*Bookmark Annotation*"))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (mapc
         (lambda (bookmark)
           ;; make sure we are dealing with records
           (let* ((bookmark (bookmark-get-bookmark bookmark))
                  (name (bookmark-name-from-full-record
                         bookmark))
                  (anno (bookmark-get-annotation bookmark))
                  (anno (cond ((and anno (stringp anno)
                                    (not (string= anno "")))
                               (concat
                                (mapconcat
                                 (lambda (str)
                                   (concat (make-string 4 #x20) str))
                                 (split-string (format "%s" anno)
                                               (string #xa))
                                 (string #xa))
                                "\n"))
                              (""))))
             (insert (format "%s:\n%s" name anno))))
         targets))
      (goto-char (point-min))
      (special-mode)))))

;;;; edit annotations

;;;;; Edit mode

(define-derived-mode blist-edit-annotation-mode
  bookmark-edit-annotation-mode "BListea"
  "The major mode for editing bookmark annotations.
\\<blist-edit-annotation-mode-map>\
When editing is done, type \\[blist-send-edit-annotation].

Simply delete the buffer if you want to cancel this edit.  Or you
can type \\[blist-abort-edit-annotation] to kill the buffer and
quit the window at the same time.

This differs from `bookmark-edit-annotation-mode' only in that it
will always regenerate the list of bookmarks and go to the list
of bookmarks after the edit is done, since this mode is not used
for editing annotation in other situations.

\\{blist-edit-annotation-mode-map}"
  :group 'blist)

(let ((map blist-edit-annotation-mode-map))
  (define-key map (vector 3 3) #'blist-send-edit-annotation)
  (define-key map (vector 3 11) #'blist-abort-edit-annotation))

;;;;;; Send edit

(defun blist-send-edit-annotation ()
  "Use buffer contents as annotation for a bookmark.
Lines beginning with `#' are ignored."
  (interactive)
  (cond
   ((not (derived-mode-p 'blist-edit-annotation-mode))
    (user-error "Not in blist-edit-annotation-mode")))
  (goto-char (point-min))
  (while (not (eobp))
    (cond
     ((= (following-char) ?#) (bookmark-kill-line t))
     ((forward-line 1))))
  (let ((annotation (buffer-substring-no-properties
                     (point-min) (point-max)))
        (bookmark-name bookmark-annotation-name))
    ;; an empty annotation is nihil
    (cond ((string= annotation "") (setq annotation nil)))
    (bookmark-set-annotation bookmark-name annotation)
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (cond ((bookmark-time-to-save-p) (bookmark-save)))
    (message "Annotation updated for \"%s\"" bookmark-name)
    (quit-window t)
    (blist-list-bookmarks)))

;;;;;; Abort edit

(defun blist-abort-edit-annotation ()
  "Kill the current buffer and quit the window.
This only does something if the major mode is derived from
`blist-edit-annotation-mode'."
  (interactive)
  (cond
   ((not (derived-mode-p 'blist-edit-annotation-mode))
    (user-error "Not in blist-edit-annotation-mode")))
  (quit-window t))

;;;;; Is it annotated?

(defun blist-is-annotated-p ()
  "Return non-nil if the bookmark at point has non-empty annotations."
  (let* ((index (ilist-get-index))
         (bookmark (cond (index (nth index bookmark-alist))))
         annotation)
    (and bookmark
         (setq annotation (bookmark-get-annotation bookmark))
         (stringp annotation)
         (not (string= annotation "")))))

;;;;; Edit function

(defun blist-edit-annotation (bookmark)
  "Edit annotation for BOOKMARK.
If called with \\[universal-argument], prompt for the bookmark to
edit with completion."
  (interactive
   (list
    (cond
     (current-prefix-arg
      (completing-read "Edit bookmark: "
                       (mapcar #'bookmark-name-from-full-record
                               bookmark-alist)
                       nil t))
     ((ilist-get-index)
      (nth (ilist-get-index) bookmark-alist))
     ((user-error "No bookmark to edit")))))
  (blist-assert-mode)
  (setq bookmark (bookmark-name-from-full-record
                  (bookmark-get-bookmark bookmark t)))
  (pop-to-buffer
   (generate-new-buffer blist-edit-annotation-buffer-name))
  (bookmark-insert-annotation bookmark)
  (blist-edit-annotation-mode)
  (setq bookmark-annotation-name bookmark))

;;;; load

;;;;; Import

;; The original import function looks quite weird to me, so I define
;; my own version.

(defun blist-import-new-list (new-list &optional reporter)
  "Add NEW-LIST of bookmarks to `bookmark-alist'.
Rename new bookmarks as needed using suffix \"<N>\" (N=1,2,3...),
when they conflict with existing bookmark names.

If REPORTER is non-nil, it should be a reporter created by
`make-progress-reporter', and will be used to report the
progress."
  (let* ((names (mapcar
                 #'bookmark-name-from-full-record
                 bookmark-alist))
         (name-len (length names))
         (new-len (length new-list))
         (table (make-hash-table
                 :test 'equal :size (+ name-len new-len)))
         (count 0))
    ;; use a hash table so that testing for membership is a constant
    ;; time operation
    (mapc
     (lambda (name) (puthash name t table))
     names)
    (cond (reporter
           (progress-reporter-force-update reporter count)))
    (mapc
     (lambda (new-record)
       (setq count (1+ count))
       (cond (reporter
              (progress-reporter-update
               reporter (floor count new-len))))
       ;; rename the new bookmark if needed
       (let* ((temp-name (bookmark-name-from-full-record new-record))
              (new-name temp-name)
              (suffix-count 2))
         (while (gethash new-name table)
           (setq new-name (format "%s<%d>" temp-name suffix-count))
           (setq suffix-count (1+ suffix-count)))
         (bookmark-set-name new-record new-name)
         (setq bookmark-alist (cons new-record bookmark-alist))
         (puthash new-name t table)))
     new-list)))

;;;;; Load function

(defun blist-load (file &optional overwrite no-msg default)
  "Load bookmarks from FILE (which must be in bookmark format).
Appends the loaded bookmarks to the front of the list of bookmarks.

If argument OVERWRITE is non-nil, existing bookmarks are
destroyed.

Optional third arg NO-MSG means don't display any messages while
loading.

If DEFAULT is non-nil make FILE the new bookmark file to watch.

Interactively, a prefix arg makes OVERWRITE and DEFAULT non-nil.

If you load a file that doesn't contain a proper bookmark alist,
you will corrupt Emacs's bookmark list.  Generally, you should
only load in files that were created with the bookmark functions
in the first place.  Your own personal bookmark file, specified
by the variable `bookmark-default-file', is maintained
automatically by Emacs; you shouldn't need to load it explicitly.

If you load a file containing bookmarks with the same names as
bookmarks already present in your Emacs, the new bookmarks will
get unique numeric suffixes \"<2>\", \"<3>\", etc."
  (interactive
   (let ((default (abbreviate-file-name
		   (or (car bookmark-bookmarks-timestamp)
		       (expand-file-name bookmark-default-file)))))
     (list
      (read-file-name
       (format "Load bookmarks from: [%s] " default)
       (file-name-directory default) default t)
      current-prefix-arg nil current-prefix-arg)))
  (blist-assert-mode)
  (let ((file (expand-file-name file)))
    (cond
     ((not (file-readable-p file))
      (user-error "Cannot read bookmark file %s"
                  (abbreviate-file-name file))))
    (let ((reporter
           (cond ((null no-msg)
                  (make-progress-reporter
                   (format "Loading bookmarks from %s..."
                           file)
                   0 100 nil nil 0.01))))
          bookmark-list)
      (with-temp-buffer
        (setq buffer-undo-list t)
        (insert-file-contents file)
        (goto-char (point-min))
        (setq bookmark-list (bookmark-alist-from-buffer))
        (cond ((not (listp bookmark-list))
               (user-error "Invalid bookmark list in %s" file)))
        (cond
         (overwrite
          (setq bookmark-alist bookmark-list)
          (setq bookmark-alist-modification-count 0))
         (t
          (blist-import-new-list bookmark-list reporter)
          (setq bookmark-alist-modification-count
                (1+ bookmark-alist-modification-count))))
        (cond
         ((or default
	      (string=
               file
               (or (car bookmark-bookmarks-timestamp)
		   (expand-file-name bookmark-default-file))))
	  (setq bookmark-bookmarks-timestamp
		(cons file (nth 5 (file-attributes file)))))))
      (blist-list-bookmarks))))

;;;; blist-toggle-group

;; REVIEW: This is implemented by the variable
;; `buffer-invisibility-spec'.  It can be a list of atoms which means
;; that the texts whose `invisible' property is a member of the list
;; should be invisible.  Setting this is much faster than manually
;; deleting and/or inserting texts in the buffer, and is much more
;; user-friendly: the texts are not altered, and the user can be sure
;; that no data are lost.

(defun blist-toggle-group ()
  "Toggle the visibility of the group at point."
  (interactive)
  (let* ((group-header (ilist-get-group))
         (group-symbol (intern (format "[ %s ]" group-header)))
         (group-items-num
          (ilist-get-property (point) 'ilist-item-num))
         (hidden-p (ilist-get-property (point) 'blist-hidden))
         (hidden-ellipsis (cond (hidden-p "") (" ...")))
         (group-title
          (cond
           (blist-show-item-num-p
            (format "[ %s - %d%s ]\n"
                    group-header group-items-num hidden-ellipsis))
           ((format "[ %s%s ]\n" group-header hidden-ellipsis))))
         (inhibit-read-only t))
    (blist-assert-mode)
    (cond
     ((null group-header)
      ;; not at group
      (user-error "Not at a group to toggle"))
     (hidden-p
      ;; hidden group
      (goto-char (ilist-point-at-eol))
      (delete-region (line-beginning-position)
                     (min (1+ (line-end-position)) (point-max)))
      ;; this character was inserted by hiding the group previously
      (delete-char 1)
      (save-excursion
        (insert
         (propertize
          group-title
          'ilist-group-header group-header
          'ilist-item-num group-items-num)))
      (remove-from-invisibility-spec group-symbol))
     ;; not hidden
     ((goto-char (ilist-point-at-eol))
      (with-silent-modifications
        (let* ((start (line-beginning-position))
               (end (min (1+ (line-end-position)) (point-max))))
          (delete-region start end)
          (insert
           (propertize
            group-title
            'ilist-group-header group-header
            'blist-hidden t
            'ilist-item-num group-items-num))
          ;; Emacs has a bug that if an invisible character right next
          ;; to the visible part has a display property, then it will
          ;; turn out to be visible.  So we insert an invisible
          ;; character to avoid this phenomenon.
          (insert (propertize (string #x20) 'invisible t))
          (goto-char start)))
      (add-to-invisibility-spec group-symbol)))))

;;;; Generic return

(defun blist-return ()
  "Either open the bookmark or toggle the group."
  (interactive)
  (blist-assert-mode)
  (cond
   ((ilist-get-group) (blist-toggle-group))
   ((ilist-get-index) (blist-open))
   ((user-error "Nothing to do here"))))

;;;; Hide all other groups than the current group

(defun blist-toggle-other-groups ()
  "Toggle all other groups than the current one."
  (interactive)
  (blist-assert-mode)
  (let ((current-group (ilist-get-group)))
    (cond
     (current-group
      (ilist-map-lines
       (lambda ()
         (blist-toggle-group))
       (lambda ()
         (let ((group (ilist-get-group)))
           (and group (not (string= group current-group)))))))
     ((ilist-get-index)
      (save-excursion
        (blist-next-group -1)
        (blist-toggle-other-groups)))
     ((user-error "Not at a group header or item")))))

;;;; Toggle location display

(defun blist-toggle-location ()
  "Toggle the display of locations of bookmarks."
  (interactive)
  (blist-assert-mode)
  (let (temp)
    ;; a little C-like hacky style
    (setq
     temp
     (setq-local blist-display-location-p
                 (not blist-display-location-p)))
    (blist-list-bookmarks)
    ;; restore the value of the variable
    (setq-local blist-display-location-p temp)))

;;;; All bookmarks, including hidden ones

(defun blist-all-bookmarks ()
  "Return the list of all bookmark indices, even the hidden ones."
  (ilist-map-lines #'ilist-get-real-index #'ilist-get-real-index
                   nil nil t))

;;;; Jumping around

(defun blist-jump-to-line (name)
  "Jump to the line containing the bookmark with NAME."
  (interactive
   (list
    (completing-read
     "Jump to bookmark: "
     (mapcar
      (lambda (index)
        (bookmark-name-from-full-record
         (nth index bookmark-alist)))
      (blist-all-bookmarks))
     nil t)))
  (blist-assert-mode)
  (let (res pos)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (ilist-boundary-buffer-p t))
                  (not res))
        (cond
         ((ilist-get-real-index)
          (setq
           res
           (string=
            (bookmark-name-from-full-record
             (nth (ilist-get-real-index) bookmark-alist))
            name))
          (cond
           (res
            (setq pos (point))
            ;; per chance the line is hidden
            (cond ((invisible-p pos)
                   (blist-prev-group 1)
                   (blist-toggle-group)))))))
        ;; don't skip invisible lines here
        (ilist-forward-line 1 nil nil t)))
    (cond
     (pos (goto-char pos))
     ((user-error "No bookmark named %s" name)))))

;;;; blist-jump-to-group

(defun blist-jump-to-group (name)
    "Jump to the line containing the bookmark with NAME."
  (interactive
   (list
    (completing-read
     "Jump to group: "
     (ilist-map-lines #'ilist-get-group #'ilist-get-group)
     nil t)))
  (blist-assert-mode)
  (let (res pos)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (ilist-boundary-buffer-p t))
                  (not res))
        (cond
         ((ilist-get-group)
          (setq
           res
           (string=
            (ilist-get-group)
            name))
          (cond (res (setq pos (point))))))
        (ilist-forward-group-header 1)))
    (cond
     (pos (goto-char pos))
     ((user-error "No group named %s" name)))))

;;;; toggle marks

(defun blist-toggle-marks ()
   "Toggle the mark statuses in the buffer.
Lines marked with `blist-default-mark' become unmarked, and lines
not marked become marked with `blist-default-mark'.

If region is active, operate only on the region.

If the point is on a group header, then only operate on that group."
  (interactive)
  (blist-assert-mode)
  (let* ((inhibit-read-only t)
         (temp (save-excursion (blist-operate-range 1 t)))
         (start (car temp))
         (end (cdr temp)))
    (ilist-map-lines
     (lambda ()
       (let* ((marks (ilist-get-marks))
              (mark-value (car-safe marks)))
         (cond
          ((null marks)
           (ilist-mark-with-char blist-default-mark))
          ((eq mark-value blist-default-mark)
           (ilist-unmark)))))
     #'ilist-get-index
     start end)))

;;;; change marks

(defun blist-change-marks (old new)
  "Change all OLD mark to NEW mark.
OLD and NEW are both characters used to mark buffers."
  (interactive
   (let* ((cursor-in-echo-area t)
	  (old (progn (message "Change (old mark): ") (read-char)))
	  (new (progn (message  "Change %c marks to (new mark): " old)
		      (read-char))))
     (list old new)))
  (blist-assert-mode)
  (let ((inhibit-read-only t))
    (ilist-map-lines
     (lambda ()
       (ilist-mark-with-char new))
     (lambda ()
       (let* ((marks (ilist-get-marks))
              (mark-value (car-safe marks)))
         (eq mark-value old))))))

;;;; blist-unmark-forward

(defun blist-unmark-forward (&optional arg)
  "Unmark bookmarks.
If the region is active, unmark the region.

If the region is not active, then unmark ARG lines forward (or
backward).

If the point is on a header, then ARG specifies the number of
groups to unmark."
  (interactive "p")
  (blist-assert-mode)
  (let* ((inhibit-read-only t)
         (temp (blist-operate-range (prefix-numeric-value arg)))
         (start (car temp))
         (end (cdr temp)))
    (ilist-map-lines
     (lambda () (ilist-mark-with-char t))
     nil start end)))

;;;; blist-unmark-backward

(defun blist-unmark-backward (&optional arg)
  "Unmark bookmarks.
If the region is active, unmark the region.

If the region is not active, then unmark ARG lines backward (or
forward).

If the point is on a header, then ARG specifies the number of
groups to unmark."
  (interactive "p")
  (blist-assert-mode)
  (blist-unmark-forward (- (prefix-numeric-value arg))))

;;;; blist-unmark-all

(defun blist-unmark-all ()
  "Unmark all bookmarks."
  (interactive)
  (blist-assert-mode)
  (let ((inhibit-read-only t))
    (ilist-map-lines #'ilist-unmark #'ilist-is-marked)))

;;;; blist-unmark-all-mark

(defun blist-unmark-all-mark (a-mark)
  "Unmark all lines marked with a specific A-MARK.
If A-MARK is RET, then unmark all marks."
  (interactive "cUnmark marks (RET means all): ")
  (blist-assert-mode)
  (cond
   ((= a-mark #xd)
    (blist-unmark-all))
   ((let ((inhibit-read-only t))
      (ilist-map-lines
       #'ilist-unmark
       (lambda ()
         (memq a-mark (ilist-get-marks))))))))

;;;; blist-delete-marked

(defun blist-delete-marked ()
  "Delete marked bookmarks from `bookmark-alist'."
  (interactive)
  (blist-assert-mode)
  (let ((inhibit-read-only t)
        (marked-list
         (sort
          (ilist-map-lines
           #'ilist-get-index
           (lambda ()
             (let* ((marks (ilist-get-marks))
                    (mark-value (car-safe marks)))
               (eq mark-value blist-default-mark))))
          #'<)))
    (cond
     ((or blist-expert
          (y-or-n-p "Confirm deletion? "))
      (cond
       (marked-list
        (setq bookmark-alist
              (ilist-delete-from-list bookmark-alist marked-list))
        (setq blist-deleted-indices
              (append marked-list blist-deleted-indices)))
       ((ilist-get-index)
        (setq blist-deleted-indices
              (cons (ilist-get-index) blist-deleted-indices))
        (setq
         bookmark-alist
         (ilist-delete-from-list
          bookmark-alist (list (ilist-get-index))))))
      (setq bookmark-alist-modification-count
            (1+ bookmark-alist-modification-count))
      (cond ((bookmark-time-to-save-p) (bookmark-save)))
      (blist-list-bookmarks)))))

;;;; Is it marked for deletion?

(defun blist-is-marked-for-deletion-p ()
  "Return non-nil if the bookmark at point is marked for deletion."
  (declare (side-effect-free t))
  (memq blist-deletion-mark
        (mapcar
         (lambda (range)
           (get-text-property (car range) 'ilist-mark-column))
         (ilist-mark-columns (point)))))

;;;; blist-do-delete

(defun blist-do-delete ()
  "Delete bookmarks marked for deletion."
  (interactive)
  (blist-assert-mode)
  (let ((inhibit-read-only t)
        (marked-list
         (sort
          (ilist-map-lines
           #'ilist-get-index #'blist-is-marked-for-deletion-p)
          #'<)))
    (cond
     ((null marked-list)
      (user-error "No bookmarks marked for deletion"))
     ((or blist-expert
          (y-or-n-p "Confirm deletion? "))
      (setq blist-deleted-indices
            (append marked-list blist-deleted-indices))
      (setq
       bookmark-alist
       (ilist-delete-from-list bookmark-alist marked-list))
      (setq bookmark-alist-modification-count
            (1+ bookmark-alist-modification-count))
      (cond ((bookmark-time-to-save-p) (bookmark-save)))
      (blist-list-bookmarks)))))

;;;; blist-mark-for-deletion

(defun blist-mark-for-deletion (arg)
  "Mark for deletion.
If the region is active, mark the region for deletion.

If the region is not active, then mark ARG lines forward (or
backward) for deletion.

If the point is on a header, then ARG specifies the number of
groups to mark for deletion."
  (interactive "p")
  (blist-assert-mode)
  (let* ((inhibit-read-only t)
         (temp (blist-operate-range (prefix-numeric-value arg)))
         (start (car temp))
         (end (cdr temp)))
  (ilist-map-lines
   (lambda () (ilist-mark-with-char blist-deletion-mark))
   nil start end)))

(defun blist-mark-for-deletion-backward (arg)
  "Mark for deletion.
Like `blist-mark-for-deletion', but go backwards.

The negative of ARG is send to `blist-mark-for-deletion'."
  (interactive "p")
  (blist-mark-for-deletion
   (- (prefix-numeric-value arg))))

;;;; blist-mark

(defun blist-mark (arg)
  "Mark bookmarks.
If the region is active, mark the region.

If the region is not active, then mark ARG lines forward (or
backward).

If the point is on a header, then ARG specifies the number of
groups to mark."
  (interactive "p")
  (blist-assert-mode)
  (let* ((inhibit-read-only t)
         (temp (blist-operate-range (prefix-numeric-value arg)))
         (start (car temp))
         (end (cdr temp)))
    (ilist-map-lines
     (lambda () (ilist-mark-with-char blist-default-mark))
     nil start end)))

;;;; blist-mark-by-name

(defun blist-mark-by-name (name &optional mark-char)
  "Mark lines matching NAME with MARK-CHAR.
Interactively, query the user for NAME.  And if called with
\\[universal-argument], also query for MARK-CHAR."
  (interactive
   (list
    (read-string "Mark by name (regexp): ")
    (cond
     (current-prefix-arg
      (let ((cursor-in-echo-area t))
        (read-char "Mark with character: "))))))
  (blist-assert-mode)
  (cond
   ((not (characterp mark-char))
    (setq mark-char blist-default-mark)))
  (ilist-map-lines
   (lambda ()
     (let ((inhibit-read-only t))
       (ilist-mark-with-char mark-char)))
   (lambda ()
     (cond
      ((ilist-get-index)
       (string-match-p
        name
        (bookmark-name-from-full-record
         (nth (ilist-get-index)
              bookmark-alist))))))))

;;;; blist-mark-by-location

(defun blist-mark-by-location (name &optional mark-char)
  "Mark lines with location matching NAME with MARK-CHAR.
Interactively, query the user for NAME.  And if called with
\\[universal-argument], also query for MARK-CHAR."
  (interactive
   (list
    (read-string "Mark by name (regexp): ")
    (cond
     (current-prefix-arg
      (let ((cursor-in-echo-area t))
        (read-char "Mark with character: "))))))
  (blist-assert-mode)
  (cond
   ((not (characterp mark-char))
    (setq mark-char blist-default-mark)))
  (ilist-map-lines
   (lambda ()
     (let ((inhibit-read-only t))
       (ilist-mark-with-char mark-char)))
   (lambda ()
     (cond
      ((ilist-get-index)
       (string-match-p
        name
        (blist-get-location
         (nth (ilist-get-index) bookmark-alist))))))))

;;;; blist-next-marked

(defun blist-next-marked (arg)
  "Go to next ARG marked line."
  (interactive "p")
  (blist-assert-mode)
  (let ((orig (line-beginning-position))
        (direction (cond ((> arg 0) 1)
                         (-1)))
        (forwardp (> arg 0))
        (arg (abs arg)))
    (while (> arg 0)
      (let (res pos)
        (save-excursion
          (ilist-forward-line direction blist-movement-cycle)
          (while (and (not (ilist-boundary-buffer-p forwardp))
                      (not (= (line-beginning-position)
                              orig))
                      (not res))
            (setq res (ilist-is-marked))
            (cond (res (setq pos (point))))
            (ilist-forward-line direction blist-movement-cycle)))
        (cond
         (pos (goto-char pos))
         ((user-error "No marked bookmark"))))
      (setq arg (1- arg)))))

;;;; blist-prev-marked

(defun blist-prev-marked (arg)
  "Go to previous ARG marked line."
  (interactive "p")
  (blist-next-marked
   (- (prefix-numeric-value arg))))

;;;; blist-prev-group

(defun blist-prev-group (arg)
  "Go to previous ARG group."
  (interactive "p")
  (blist-assert-mode)
  (ilist-backward-group-header arg blist-movement-cycle))

;;;; blist-next-group

(defun blist-next-group (arg)
  "Go to next ARG group."
  (interactive "p")
  (blist-assert-mode)
  (ilist-forward-group-header arg blist-movement-cycle))

;;;; blist-next-item

(defun blist-next-item (arg)
  "Go to next ARG item.
An item means a line that is not a group header.

It might stop at a non-item line, if there is no such line to
stop at."
  (interactive "p")
  (blist-assert-mode)
  (ilist-forward-line arg blist-movement-cycle t))

;;;; blist-prev-item

(defun blist-prev-item (arg)
  "Go to previous ARG item.
An item means a line that is not a group header.

It might stop at a non-item line, if there is no such line to
stop at."
  (interactive "p")
  (blist-assert-mode)
  (ilist-backward-line arg blist-movement-cycle t))


;;;; blist-prev-line

(defun blist-prev-line (arg)
  "Go to previous ARG line."
  (interactive "p")
  (blist-assert-mode)
  (ilist-backward-line arg blist-movement-cycle nil))

;;;; blist-next-line

(defun blist-next-line (arg)
  "Go to next ARG line."
  (interactive "p")
  (blist-assert-mode)
  (ilist-forward-line arg blist-movement-cycle nil))

;;;; blist-show-info

;; TODO: Some information should be inserted first, with outstanding
;; faces, and the rest should be inserted at the end.

(defun blist-show-info (&optional arg)
  "Pop a buffer showing detailed information about the bookmarks.
If there are marked bookmarks, show the information of those
bookmarks.

Otherwise, if the point is at a bookmark, show the information of
that bookmark.  If, and only if, ARG is an integer, show ARG many
following bookmarks; if ARG is negative, show minus ARG many
preceding bookmarks instead.

Otherwise, if the point is at a group header, show the
information of the bookmarks of that group."
  (interactive "p")
  (blist-assert-mode)
  (let* ((marked-items (ilist-map-lines #'ilist-get-index
                                        #'ilist-is-marked))
         (items
          (cond
           (marked-items)
           ((ilist-get-group)
            (let ((start (point))
                  (end (save-excursion
                         (ilist-forward-group-header 1)
                         (point))))
              (ilist-map-lines
               #'ilist-get-index #'ilist-get-index start end)))
           ;; HACK: if not on a normal line, it will return nil, so
           ;; that this clause is skipped
           ((delq nil (list (ilist-get-index))))
           ((user-error "No bookmarks to open"))))
         (items (mapcar
                 (lambda (index) (nth index bookmark-alist))
                 items))
         (items-len (length items))
         (seperator (make-string (1- (window-body-width)) ?-))
         (buffer (get-buffer-create "*blist-info*"))
         (durand-window-max-height 0.5))
    (with-current-buffer buffer
      (erase-buffer)
      (let ((count 0) temp)
        (while (consp items)
          (setq count (1+ count))
          (setq temp (car items))
          (setq items (cdr items))
          (insert "name: " (car temp))
          (newline)
          (mapc
           (lambda (cell)
             (cond
              ((and
                (consp cell)
                (cdr cell)
                (eq (car cell) 'last-modified))
               (insert
                (format
                 "%S: [%s]"
                 (car cell)
                 (format-time-string "%F %a %R" (cdr cell))))
               (newline))
              ((and (consp cell) (cdr cell))
               (insert (format "%S: %S" (car cell) (cdr cell)))
               (newline))))
           temp)
          (cond ((< count items-len)
                 (insert seperator)
                 (newline))))
        (goto-char (point-min))))
    (display-buffer
     buffer
     (list (list #'display-buffer-in-side-window)
           (cons 'side 'bottom)
           (cons 'window-height
                 #'durand-fit-window-to-buffer-with-max)))))

;; TODO: Display counts about each groups.

;; TODO: Navigate between each fields.


(provide 'blist)
;;; blist.el ends here
