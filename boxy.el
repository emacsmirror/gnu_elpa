;;; boxy.el --- A boxy layout framework -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Amy Grinn <grinn.amy@gmail.com>
;; Version: 1.1.3
;; File: boxy.el
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools
;; URL: https://gitlab.com/tygrdev/boxy

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

;;   Boxy provides an interface to create a 3D representation of
;;   boxes.  Each box has a relationship with one other box.  Multiple
;;   boxes can be related to one box.  The relationship can be any of
;;   the following:
;;
;;   - in
;;   - on
;;   - behind
;;   - on top of
;;   - in front of
;;   - above
;;   - below
;;   - to the right of
;;   - to the left of
;;
;;   The relationship determines the ordering and structure of the
;;   resulting boxy diagram.
;;
;;   Only boxes which have their :name slot set will be drawn to the
;;   buffer.  Boxes without names still take up space and can have
;;   children, so can be used for grouping.  All diagrams have one top
;;   level unnamed box called a `world'.
;;
;;   Each box should have either a list of markers or an action
;;   function.  When viewing a box that has a list of markers, the
;;   following keybindings are available:
;;
;;   RET/mouse-1   - Jump to the first marker
;;   o             - Open next marker in other window.
;;                     Pressed multiple times, cycle through markers.
;;   M-RET         - Open all markers as separate buffers.
;;                     This will split the current window as needed.
;;
;;   When viewing a box with an action function, RET and <mouse-1>
;;   will be bound to that function.
;;
;;   Additionally, all boxes have the following keybindings defined:
;;
;;   r     - Jump to the box directly related to the current box.
;;             Repeated presses will eventually take you to the
;;             top level box.
;;   TAB   - Cycle visibility of box's children
;;
;;   See the class definition for `boxy-box' for all other available
;;   properties.
;;
;;   To start, create an empty box named `world'.
;;
;;     (let ((world (boxy-box)))
;;
;;   Use the method `boxy-add-next' to add top-level boxes to the
;;   world, without relationships:
;;
;;     (let ((cyprus (boxy-box :name "Cyprus"))
;;           (greece (boxy-box :name "Greece")))
;;       (boxy-add-next cyprus world)
;;       (boxy-add-next greece world)
;;
;;   To ease the boxy renderer, use the :expand-siblings and
;;   :expand-children slots.  These should be list of functions which
;;   take the current box as an argument and call `boxy-add-next' to
;;   add sibling boxes and children boxes respectively.  Children
;;   boxes are defined as any box with a relationship of in, on,
;;   behind, in front of, or on top of.  Sibling boxes are defined as
;;   any box with a relationship of above, below, to the left of, or
;;   to the right of.
;;
;;      (object-add-to-list cyprus :expand-siblings
;;                          '(lambda (box)
;;                             (boxy-add-next
;;                              (boxy-box :name "Lebanon" :rel "below")
;;                              box)))
;;
;;   The expansion slots will be called when the user toggles the
;;   box's visibility.
;;
;;   To display a box in a popup buffer, use the function `boxy-pp'.
;;
;;   The methods `boxy-merge' and `boxy-merge-into' should be
;;   used to merge boxes together.  `boxy-merge' takes a list of
;;   boxes and merges them into one box.  `boxy-merge-into' takes
;;   two boxes and merges the first into the second.
;;

;;; Code:

;;;; Requirements

(require 'easy-mmode)
(require 'eieio)
(require 'cl-lib)
(require 'subr-x)

;;;; Options

(defgroup boxy nil
  "Customization options for boxy."
  :group 'applications)

(defcustom boxy-default-margin-x 2
  "Default horizontal margin to be used when displaying boxes."
  :type 'number)

(defcustom boxy-default-margin-y 1
  "Default vertical margin to be used when displaying boxes."
  :type 'number)

(defcustom boxy-default-padding-x 2
  "Default horizontal padding to be used when displaying boxes."
  :type 'number)

(defcustom boxy-default-padding-y 1
  "Default vertical padding to be used when displaying boxes."
  :type 'number)

(defcustom boxy-flex-width 80
  "When flexibly displaying boxes, try to keep width below this."
  :type 'number)

(defcustom boxy-default-visibility 2
  "Default level to display boxes."
  :type 'number)

(defcustom boxy-tooltips t
  "Show tooltips in a boxy diagram."
  :type 'boolean)

(defcustom boxy-tooltip-timeout 0.5
  "Idle time before showing tooltip in a boxy diagram."
  :type 'number)

(defcustom boxy-tooltip-max-width 30
  "Maximum width of all tooltips."
  :type 'number)

;;;; Variables

(defvar boxy-tooltip-show-function #'boxy--tooltip-show
  "A function which takes a multi-line string and displays it immediately.")

;;;; Faces

(defface boxy-default nil
  "Default face used in Boxy mode.")

(defface boxy-primary
   '((((background dark)) (:foreground "turquoise"))
     (t (:foreground "dark cyan")))
   "Face for highlighting the name of a box.")

(defface boxy-selected
   '((t :foreground "light slate blue"))
  "Face for the current box border under cursor.")

(defface boxy-rel
  '((t :foreground "hot pink"))
  "Face for the box which is related to the box under the cursor.")

(defface boxy-tooltip
 '((((background dark)) (:background "gray30" :foreground "gray"))
   (t (:background "gainsboro" :foreground "dim gray")))
 "Face for tooltips in a boxy diagram.")

;;;; Constants

(defconst boxy-relationships
  '("in" "on" "behind" "in front of" "above" "below" "to the left of" "to the right of" "on top of")
  "List of available relationships for boxes.")

(defconst boxy-children-relationships
  '("in" "on" "behind" "in front of" "on top of")
  "List of relationships which are rendered as children.")

(defconst boxy-sibling-relationships
  '("above" "below" "to the left of" "to the right of")
  "List of relationships which are rendered as siblings.")

(defconst boxy-flex-relationships
  '("in" "on" "behind")
  "List of relationships for which boxes are flexibly added to their parent.")

;;;; Custom types

(cl-deftype boxy-relationship (&optional _)
  '(satisfies (lambda (rel) (member rel boxy-relationships))))

;;;; Boxy mode

(defvar-local boxy--box-ring '()
  "List of buffer positions of buttons in a boxy diagram.")

(defvar-local boxy--world nil
  "Current top-level box the buffer is displaying.")

(defvar-local boxy--header '()
  "Current containers the buffer is displaying.")

(defvar-local boxy--offset '(0 . 0)
  "Current offset in row-column order for the boxy diagram.")

(defvar-local boxy--visibility boxy-default-visibility
  "Visibility of children in the current boxy diagram.")

(defvar-local boxy--max-visibility 3
  "Maximum visibility setting allowed when cycling all children.")

(defvar-local boxy--default-margin-x boxy-default-margin-x
  "Default horizontal margin to be used when displaying-boxes.")

(defvar-local boxy--default-margin-y boxy-default-margin-y
  "Default vertical margin to be used when displaying boxes.")

(defvar-local boxy--default-padding-x boxy-default-padding-x
  "Default horizontal padding to be used when displaying boxes.")

(defvar-local boxy--default-padding-y boxy-default-padding-y
  "Default vertical padding to be used when displaying boxes.")

(defvar-local boxy--flex-width boxy-flex-width
  "When flexibly displaying boxes, try to keep width below this.")

(defvar-local boxy--default-visibility boxy-default-visibility
  "Default level to display boxes.")

(defvar-local boxy--tooltips boxy-tooltips
  "Show tooltips in a boxy diagram.")

(defvar-local boxy--tooltip-timeout boxy-tooltip-timeout
  "Idle time before showing tooltip in a boxy diagram.")

(defvar-local boxy--tooltip-max-width boxy-tooltip-max-width
  "Maximum width of all tooltips.")

(defvar-local boxy--default-face 'boxy-default
  "Face to use when drawing boxes.")

(defvar-local boxy--tooltip-face 'boxy-tooltip
  "Face to use when drawing tooltips.")

(defvar-local boxy--primary-face 'boxy-primary
  "Face to use when highlighting a box's name.")

(defvar-local boxy--selected-face 'boxy-selected
  "Face to use for highlighting the currently selected box.")

(defvar-local boxy--rel-face 'boxy-rel
  "Face to use for highlighting the box directly related to the selected box.")

(defun boxy-mode-cycle ()
  "Cycle through buttons in the current boxy buffer."
  (interactive)
  (if-let ((pos (seq-find (lambda (pos) (> pos (point))) boxy--box-ring)))
      (goto-char pos)))

(defun boxy-mode-uncycle ()
  "Cycle through buttons in the current boxy buffer in reverse."
  (interactive)
  (if-let ((pos (seq-find (lambda (pos) (< pos (point))) (reverse boxy--box-ring))))
      (goto-char pos)))

(defun boxy-mode-cycle-down ()
  "Cycle to the next button on the row below."
  (interactive)
  (let ((coords (cons (line-number-at-pos) (current-column))))
    (goto-char (seq-reduce
                (lambda (closest pos)
                  (goto-char pos)
                  (if (<= (line-number-at-pos) (car coords))
                      closest
                    (let* ((pos-coords (cons (line-number-at-pos) (current-column)))
                           (pos-dist (sqrt (+ (expt (- (car pos-coords) (car coords)) 2)
                                              (expt (- (cdr pos-coords) (cdr coords)) 2))))
                           (closest-coords (and (goto-char closest) (cons (line-number-at-pos) (current-column))))
                           (closest-dist (sqrt (+ (expt (- (car closest-coords) (car coords)) 2)
                                                  (expt (- (cdr closest-coords) (cdr coords)) 2)))))
                      (if (< pos-dist closest-dist)
                          pos
                        closest))))
                boxy--box-ring
                (point-max)))))

(defun boxy-mode-cycle-up ()
  "Cycle to the next button on the row above."
  (interactive)
  (let ((coords (cons (line-number-at-pos) (current-column))))
    (goto-char (seq-reduce
                (lambda (closest pos)
                  (goto-char pos)
                  (if (>= (line-number-at-pos) (car coords))
                      closest
                    (let* ((pos-coords (cons (line-number-at-pos) (current-column)))
                           (pos-dist (sqrt (+ (expt (- (car pos-coords) (car coords)) 2)
                                              (expt (- (cdr pos-coords) (cdr coords)) 2))))
                           (closest-coords (and (goto-char closest) (cons (line-number-at-pos) (current-column))))
                           (closest-dist (sqrt (+ (expt (- (car closest-coords) (car coords)) 2)
                                                  (expt (- (cdr closest-coords) (cdr coords)) 2)))))
                      (if (< pos-dist closest-dist)
                          pos
                        closest))))
                boxy--box-ring
                (point-min)))))

(defun boxy-mode-cycle-visibility ()
  "Cycle visibility on all children in the current buffer."
  (interactive)
  (setq boxy--visibility (mod (+ 1 boxy--visibility)
                              (+ 1 boxy--max-visibility)))
  (if (= 0 boxy--visibility)
      (setq boxy--visibility 1))
  (cond
   ((= 1 boxy--visibility) (message "OVERVIEW"))
   ((= 2 boxy--visibility) (message "CONTENTS"))
   ((= 3 boxy--visibility) (message "MORE CONTENTS")))
  (boxy-mode-update-visibility)
  (boxy-mode-redraw))

(defun boxy-mode-redraw ()
  "Redraw `boxy--world' in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (when boxy--header
      (insert boxy--header)
      (fill-paragraph)
      (insert "\n"))
    (setq boxy--offset (cons (- (line-number-at-pos)
                                         2
                                         (* 2 (boxy--padding-y boxy--world)))
                             (- 0 1 (boxy--padding-x boxy--world))))
    (boxy-draw boxy--world)
    (boxy-mode-recalculate-box-ring)
    (goto-char (point-max))
    (insert "\n")
    (goto-char (point-min))))

(defun boxy-mode-recalculate-box-ring ()
  "Recalculate the position of all boxes in `boxy--world'."
  (setq boxy--box-ring
        (seq-sort
         #'<
         (seq-filter
          #'identity
          (mapcar
           #'boxy--get-position
           (seq-filter
            (lambda (box) (boxy-is-visible box t))
            (boxy--get-all boxy--world)))))))

(defun boxy-mode-update-visibility ()
  "Update visibility of all boxes in `boxy--world'."
  (boxy--update-visibility boxy--world)
  (boxy--flex-adjust boxy--world boxy--world))

(defun boxy-mode-make-dirty ()
  "Clear all TOP LEFT WIDTH and HEIGHT coordinates from `boxy--world'."
  (boxy--make-dirty boxy--world))

(defvar boxy-mode-map
  (easy-mmode-define-keymap
    (mapcar (lambda (key) (cons (kbd (car key)) (cdr key)))
            '(("TAB"       . boxy-mode-cycle)
              ("<right>"   . boxy-mode-cycle)
              ("C-f"       . boxy-mode-cycle)
              ("M-f"       . boxy-mode-cycle)
              ("f"         . boxy-mode-cycle)
              ("<left>"    . boxy-mode-uncycle)
              ("C-b"       . boxy-mode-uncycle)
              ("M-b"       . boxy-mode-uncycle)
              ("b"         . boxy-mode-uncycle)
              ("<up>"      . boxy-mode-cycle-up)
              ("C-p"       . boxy-mode-cycle-up)
              ("p"         . boxy-mode-cycle-up)
              ("<down>"    . boxy-mode-cycle-down)
              ("C-n"       . boxy-mode-cycle-down)
              ("n"         . boxy-mode-cycle-down)
              ("<backtab>" . boxy-mode-cycle-visibility)))))

(define-derived-mode boxy-mode special-mode
  "Boxy"
  "Mode for viewing an boxy diagram."
  (visual-line-mode -1)
  (setq indent-tabs-mode nil)
  (cursor-sensor-mode t)
  (setq truncate-lines t)
  (buffer-face-set 'fixed-pitch))

(cl-defun boxy-pp (box
                   &key
                   (display-buffer-fn 'display-buffer-pop-up-window)
                   (visibility boxy-default-visibility)
                   (max-visibility 3)
                   select
                   header
                   (default-margin-x boxy-default-margin-x)
                   (default-margin-y boxy-default-margin-y)
                   (default-padding-x boxy-default-padding-x)
                   (default-padding-y boxy-default-padding-y)
                   (flex-width boxy-flex-width)
                   (tooltips boxy-tooltips)
                   (tooltip-timeout boxy-tooltip-timeout)
                   (tooltip-max-width boxy-tooltip-max-width)
                   (default-face 'boxy-default)
                   (primary-face 'boxy-primary)
                   (tooltip-face 'boxy-tooltip)
                   (rel-face 'boxy-rel)
                   (selected-face 'boxy-selected))
  "Pretty print BOX in a popup buffer.

If HEADER is passed in, it will be printed above the diagram.

DISPLAY-BUFFER-FN is used to display the diagram, by
default `display-buffer-pop-up-window'.

If SELECT is non-nil, select the boxy window after displaying
it.

VISIBILITY is the initial visibility of children and
MAX-VISIBILITY is the maximum depth to display when cycling
visibility.

DEFAULT-MARGIN-X, DEFAULT-MARGIN-Y, DEFAULT-PADDING-X and
DEFAULT-PADDING-Y will be the fallback values to use if a box's
margin and padding slots are not set.

When adding boxes, boxy will try to keep the width below
FLEX-WIDTH.

If TOOLTIPS is nil, don't show any tooltips.

TOOLTIP-TIMEOUT is the idle time to wait before showing a
tooltip.

TOOLTIP-MAX-WIDTH is the maximum width of a tooltip.  Lines
longer than this will be truncated.

DEFAULT-FACE, PRIMARY-FACE, TOOLTIP-FACE, REL-FACE, and
SELECTED-FACE can be set to change the appearance of the boxy
diagram."
  (when-let ((buffer (get-buffer "*Boxy*")))
    (kill-buffer buffer)
    (if-let ((window (get-buffer-window buffer t)))
        (delete-window window)))
  (let ((buffer (get-buffer-create "*Boxy*")))
    (with-current-buffer buffer
      (boxy-mode)
      (setq boxy--header header)
      (setq boxy--world box)
      (setq boxy--visibility visibility)
      (setq boxy--max-visibility max-visibility)
      (setq boxy--default-margin-x default-margin-x)
      (setq boxy--default-margin-y default-margin-y)
      (setq boxy--default-padding-x default-padding-x)
      (setq boxy--default-padding-y default-padding-y)
      (setq boxy--flex-width flex-width)
      (setq boxy--tooltips tooltips)
      (setq boxy--tooltip-timeout tooltip-timeout)
      (setq boxy--tooltip-max-width tooltip-max-width)
      (setq boxy--default-face default-face)
      (setq boxy--primary-face primary-face)
      (setq boxy--tooltip-face tooltip-face)
      (setq boxy--rel-face rel-face)
      (setq boxy--selected-face selected-face)
      (boxy-mode-update-visibility)
      (boxy-mode-redraw)
      (let* ((width (apply #'max (mapcar #'length (split-string (buffer-string) "\n"))))
             (height (count-lines (point-min) (point-max)))
             (window (or (get-buffer-window buffer)
                         (display-buffer buffer
                                         `(,display-buffer-fn
                                           (window-width . ,width)
                                           (window-height . ,height))))))
        (if select (select-window window))))))


;;;; Class definitions and public methods

(defclass boxy-box ()
  ((name :initarg :name
         :type string
         :documentation "The name of the box to be displayed.  If
not set, the box will not be drawn to the buffer.")
   (rel :initarg :rel
        :type boxy-relationship
        :documentation "The relationship between this box and
its rel-box. Must be a member of `boxy-relationships'")
   (primary :initarg :primary
            :initform nil
            :type boolean
            :documentation "Whether to apply the `boxy-primary'
face to the box's name.")
   (action :initarg :action
           :type command
           :documentation "A command executed when this box is
clicked.  If not set, use :markers.")
   (markers :initarg :markers
            :type list
            :documentation "A list of buffer markers that this
box should link back to when clicked.")
   (post-jump-hook :initarg :post-jump-hook
                   :type function
                   :documentation "A hook which is called after
jumping to a marker.")
   (tooltip :initarg :tooltip
            :type string
            :documentation "A tooltip to be displayed when the
cursor is within the box's name.

Each line will be truncated to `boxy-tooltip-max-width'")
   (help-echo :initarg :help-echo
              :type string
              :documentation "A string to be displayed in the
minibuffer when the cursor is within the box's name.")
   (expand-siblings :initarg :expand-siblings
                    :type list
                    :initform '()
                    :documentation "A list of functions which add
siblings to this box using the function `boxy-add-next'.")
   (expand-children :initarg :expand-children
                    :type list
                    :initform '()
                    :documentation "A list of functions which add
children to this box using the function `boxy-add-next'.")
   (padding-x :initarg :padding-x
              :type number
              :documentation "Override default horizontal padding
for this box and its children.")
   (padding-y :initarg :padding-y
              :type number
              :documentation "Override default vertical padding
for this box and its children.")
   (margin-x :initarg :margin-x
             :type number
             :documentation "Override default horizontal margin
for this box and its children.")
   (margin-y :initarg :margin-y
             :type number
             :documentation "Override default vertical margin for
this box and its children.")
   (rel-box :initarg :rel-box
            :type boxy-box
            :documentation "The box directly related to this box.

Usually set using the functions `boxy-add-next',
`boxy-merge-boxes', or `boxy-merge-into'")
   (display-rel :initarg :display-rel
                :type string
                :documentation "If relationship is changed during
creation of the box, the original relationship
will be saved here for display purposes.")
   (display-rel-box :initarg :display-rel-box
                    :type boxy-box
                    :documentation "If the relationship is
changed during the creation of the box, the original rel-box is
saved here for display purposes.")
   (x-order :initarg :x-order
            :initform 0
            :type number
            :documentation "Relative horizontal ordering of box
within its parent.")
   (y-order :initarg :y-order
            :initform 0
            :type number
            :documentation "Relative vertical ordering of box
within its parent.")
   (in-front :initarg :in-front
             :initform nil
             :type boolean
             :documentation "Whether the box should be rendered
in front of its parent box.")
   (behind :initarg :behind
           :initform nil
           :type boolean
           :documentation "Whether the box should be rendered as
behind its parent box.")
   (on-top :initarg :on-top
           :initform nil
           :type boolean
           :documentation "Whether the box should be rendered as
on top of its parent box.")
   (parent :initarg :parent
           :type boxy-box
           :documentation "The parent of this box.")
   (children :initarg :children
             :initform '()
             :type list
             :documentation "The visible children of this box.")
   (hidden-children :initarg :hidden-children
                    :initform '()
                    :type list
                    :documentation "The hidden children of this box.")
   (level :initarg :level
          :initform 0
          :type number
          :documentation "Number of nested generations this box
is currently at.")
   (top :initarg :top
        :type number
        :documentation "Calculated top row for this box.")
   (left :initarg :left
         :type number
         :documentation "Calculated left column for this box.")
   (width :initarg :width
          :type number
          :documentation "Calculated width of this box.")
   (height :initarg :height
           :type number
           :documentation "Calculated height of this box.")
   (flex :initarg :flex
         :initform nil
         :type boolean
         :documentation "Whether or not this box should be
flexibly added to its parent.  Should not be set manually."))
  "A representation of a box in 3D space.")

(defun boxy-merge (boxes)
  "Merge BOXES into a single box."
  (if (< (length boxes) 2)
      (if (= 0 (length boxes))
          (boxy-box)
        (car boxes))
    (let ((world (boxy-box)))
      (while boxes
        (boxy-merge-into (pop boxes) world))
      world)))

(defun boxy-merge-into (from to)
  "Merge FROM box into TO box."
  (let (match-found)
    (mapc
     (lambda (from-box)
       (let ((match (boxy-find-matching from-box to)))
         (while (and (not match) (slot-boundp from-box :rel-box))
           (setq from-box (oref from-box rel-box))
           (setq match (boxy-find-matching from-box to)))
         (when match
           (setq match-found t)
           (boxy--add-matching from-box match))))
     (boxy--primary-boxes from))
    (unless match-found
      (let ((all-from-children (append (oref from children)
                                       (oref from hidden-children))))
        (if (= 1 (length all-from-children))
            (progn
              (oset (car all-from-children) flex t)
              (boxy--add-child to (car all-from-children)))
          (oset from flex t)
          (boxy--add-child to from))))))

(defun boxy-is-visible (box &optional calculate)
  "Determine if BOX is visible according to `boxy--visibility'.

If CALCULATE, determine if the box has been expanded manually."
  (if calculate
      (if (not (slot-boundp box :parent))
          t
        (seq-find
         (lambda (sibling) (eq sibling box))
         (oref (oref box parent) children)))
    (or (= 0 boxy--visibility)
        (<= (oref box level) boxy--visibility))))

(defun boxy-jump-to-box (box)
  "Jump cursor to the first character in the label of BOX."
  (if (not (boxy-is-visible box t))
      (let ((top (oref box parent)))
        (boxy--cycle-children top)
        (while (not (boxy-is-visible top t))
          (setq top (oref top parent))
          (boxy--cycle-children top))
        (boxy--flex-adjust top (boxy--get-world top))
        (boxy-mode-redraw)
        (run-with-timer 0 nil
                        (lambda ()
                          (let ((top (boxy--get-top box))
                                (left (boxy--get-left box)))
                            (forward-line (- (+ (car boxy--offset) top 1 (boxy--padding-y box))
                                             (line-number-at-pos)))
                            (move-to-column (+ (cdr boxy--offset) left 1 (boxy--padding-x box)))))))
    (let ((top (boxy--get-top box))
          (left (boxy--get-left box)))
      (forward-line (- (+ (car boxy--offset) top 1 (boxy--padding-y box))
                       (line-number-at-pos)))
      (move-to-column (+ (cdr boxy--offset) left 1 (boxy--padding-x box))))))

(defun boxy-find-matching (search-box world)
  "Find a box in WORLD with a matching name as SEARCH-BOX."
  (when (slot-boundp search-box :name)
    (with-slots ((search-name name)) search-box
      (seq-find
       (lambda (box)
         (and (slot-boundp box :name)
              (string= search-name (oref box name))))
       (boxy--expand world)))))

(defun boxy-add-next (next prev &optional force-visible skip-next)
  "Add NEXT to world according to its relationship to PREV.

If FORCE-VISIBLE, show the box regardless of
`boxy--visibility'

If SKIP-NEXT, don't add expansion slots for boxes related to
NEXT."
  (with-slots (rel) next
    (if-let ((match (boxy-find-matching next prev)))
        (boxy--add-matching next match)
      (if (not (slot-boundp prev :parent))
          (progn
            (oset next flex t)
            (oset next level (+ 1 (oref prev level)))
            (boxy--add-child prev next force-visible))
        (if (slot-boundp next :display-rel-box)
            (oset next display-rel-box
                  (boxy-find-matching
                   (oref next display-rel-box)
                   (boxy--get-world prev))))
        (if (string= rel "on top of")
            (oset next on-top t))
        (if (string= rel "in front of")
            (oset next in-front t))
        (let* ((next-boxes (boxy--next next))
               (partitioned (seq-group-by
                             (lambda (next-next)
                               (if (member (oref next-next rel)
                                           boxy-children-relationships)
                                   'children
                                 'siblings))
                             next-boxes))
               (children-boxes (alist-get 'children partitioned))
               (sibling-boxes (alist-get 'siblings partitioned))
               update-visibility)
          (if-let ((match (boxy-find-matching next prev)))
              (boxy--add-matching next match)
            (cond
             ((member rel '("to the left of" "to the right of"))
              (oset next level (oref prev level))
              (oset next behind (oref prev behind))
              (oset next in-front (oref prev in-front))
              (oset next on-top (oref prev on-top)))
             ((member rel '("above" "below"))
              (oset next behind (oref prev behind))
              (cond
               ((and (oref prev in-front) (string= rel "below"))
                (setq update-visibility t)
                (oset next display-rel-box prev)
                (while (oref prev in-front)
                  (setq prev (oref prev parent)))
                (oset next level (oref prev level)))
               ((and (oref prev on-top) (string= rel "above"))
                (setq update-visibility t)
                (oset next display-rel-box prev)
                (while (oref prev on-top)
                  (setq prev (oref prev parent)))
                (oset next level (oref prev level)))
               ((and (oref prev on-top) (string= rel "below"))
                (setq update-visibility t)
                (oset next display-rel rel)
                (oset next display-rel-box prev)
                (setq rel "in")
                (setq prev (oref prev parent))
                (oset next level (+ 1 (oref prev level))))
               (t
                (oset next level (oref prev level)))))
             ((or (oref next on-top) (oref next in-front))
              (oset next level (+ 1 (oref prev level)))
              (oset next behind (oref prev behind)))
             ((member rel '("in" "on"))
              (oset next flex t)
              (oset next behind (oref prev behind))
              (oset next level (+ 1 (oref prev level))))
             ((string= rel "behind")
              (oset next flex t)
              (oset next level (+ 1 (oref prev level)))
              (oset next behind t)))
            (oset next rel-box prev)
            (if (member rel boxy-children-relationships)
                (boxy--add-child prev next force-visible)
              (boxy--add-child (oref prev parent) next force-visible))
            (unless skip-next
              (if children-boxes
                  (object-add-to-list next :expand-children
                                      `(lambda (box)
                                         (mapc
                                          (lambda (child) (boxy-add-next child box))
                                          ',children-boxes))))
              (if sibling-boxes
                  (object-add-to-list next :expand-siblings
                                      `(lambda (box)
                                         (mapc
                                          (lambda (sibling)
                                            (boxy-add-next sibling box t))
                                          ',sibling-boxes))))
              (if update-visibility
                  (boxy--update-visibility (boxy--get-world prev))))))))))

;;;; Drawing

(defun boxy-draw (box &optional border-face)
  "Insert an ascii drawing of BOX into the current buffer.

If BORDER-FACE is non-nil, skip drawing children boxes and only
update text properties on the border.  If BORDER-FACE is t, use
the `boxy-default' face, otherwise, use BORDER-FACE.

Uses `boxy--offset' to determine row and column offsets."
  (let (box-coords)
    (when (slot-boundp box :name)
      (let* ((top (+ (car boxy--offset) (boxy--get-top box)))
             (left (+ (cdr boxy--offset) (boxy--get-left box)))
             (width (boxy--get-width box))
             (height (boxy--get-height box))
             (double (or (oref box hidden-children) (oref box expand-children)))
             (align-bottom (or (oref box in-front) (oref box on-top)))
             (dashed (oref box behind)))
        (cl-flet* ((draw (coords str)
                         (forward-line (- (car coords) (line-number-at-pos)))
                         (when (< (line-number-at-pos) (car coords))
                           (insert (make-string (- (car coords) (line-number-at-pos)) ?\n)))
                         (move-to-column (cdr coords) t)
                         (if border-face
                             (put-text-property (point) (+ (length str) (point))
                                                'face (if (eq border-face t)
                                                          boxy--default-face
                                                        border-face))
                           (put-text-property 0 (length str)
                                              'face boxy--default-face
                                              str)
                           (insert str)
                           (let ((remaining-chars (- (save-excursion (end-of-line) (current-column))
                                                     (current-column))))
                             (delete-char (min (length str) remaining-chars)))))
                   (draw-name (coords str)
                              (when (not border-face)
                                (forward-line (- (car coords) (line-number-at-pos)))
                                (when (< (line-number-at-pos) (car coords))
                                  (insert (make-string (- (car coords) (line-number-at-pos)) ?\n)))
                                (move-to-column (cdr coords) t)
                                (setq box-coords coords)
                                (put-text-property 0 (length str)
                                                   'face (if (oref box primary)
                                                             boxy--primary-face
                                                           boxy--default-face)
                                                   str)
                                (put-text-property 0 (length str)
                                                   'cursor-sensor-functions
                                                   (list (boxy-button-cursor-sensor box))
                                                   str)
                                (insert-button str
                                               'help-echo "Jump to first occurence"
                                               'keymap (boxy-button-create-keymap box))
                                (let ((remaining-chars (- (save-excursion (end-of-line)
                                                                          (current-column))
                                                          (current-column))))
                                  (delete-char (min (string-width str) remaining-chars))))))
          (draw (cons top left)
                (concat (cond ((and double dashed) "┏")
                              (double "╔")
                              (t "╭"))
                        (make-string (- width 2) (cond ((and double dashed) #x2505)
                                                       (dashed #x254c)
                                                       (double #x2550)
                                                       (t #x2500)))
                        (cond ((and double dashed) "┓")
                              (double "╗")
                              (t "╮"))))
          (if align-bottom
              (draw (cons (+ top height) left)
                    (concat (cond ((and double dashed) "┸")
                                  (double "╨")
                                  (t "┴"))
                            (make-string (- width 2) (cond (dashed #x254c)
                                                           (t #x2500)))
                            (cond ((and double dashed) "┸")
                                  (double "╨")
                                  (t "┴"))))
            (draw (cons (+ top height -1) left)
                  (concat (cond ((and double dashed) "┗")
                                (double "╚")
                                (t "╰"))
                          (make-string (- width 2) (cond ((and double dashed) #x2505)
                                                         (dashed #x254c)
                                                         (double #x2550)
                                                         (t #x2500)))
                          (cond ((and double dashed) "┛")
                                (double "╝")
                                (t "╯")))))
          (draw-name (cons (+ top 1 (boxy--padding-y box))
                           (+ left 1 (boxy--padding-x box)))
                     (oref box name))
          (let ((r (+ top 1))
                (c1 left)
                (c2 (+ left width -1)))
            (dotimes (_ (- height (if align-bottom 1 2)))
              (draw (cons r c1) (cond ((and double dashed) "┇")
                                      (dashed "╎")
                                      (double "║")
                                      (t "│")))
              (draw (cons r c2) (cond ((and double dashed) "┇")
                                      (dashed "╎")
                                      (double "║")
                                      (t "│")))
              (setq r (+ r 1)))))))
    (if border-face
        (if box-coords (list box-coords) nil)
      (apply #'append
             (if box-coords (list box-coords) nil)
             (mapcar
              #'boxy-draw
              (oref box children))))))

(defun boxy--get-width (box)
  "Get the width of BOX."
  (if (slot-boundp box :width)
      (oref box width)
    (let* ((margin (boxy--margin-x box))
           (padding (boxy--padding-x box))
           (base-width (+ 2           ; box walls
                          (* 2 padding)))
           (width (+ base-width
                     (if (slot-boundp box :name)
                         (string-width (oref box name))
                       0)))
           (children (oref box children)))
      (oset box width
            (if (not children)
                width
              (let* ((row-indices (cl-delete-duplicates
                                   (mapcar
                                    (lambda (child) (oref child y-order))
                                    children)))
                     (rows (mapcar
                            (lambda (r)
                              (cl-delete-duplicates
                               (seq-filter
                                (lambda (child) (= r (oref child y-order)))
                                children)
                               :test #'(lambda (a b)
                                         (and (slot-boundp a :name)
                                              (slot-boundp b :name)
                                              (string= (oref a name) (oref b name))))))
                            row-indices))
                     (children-width (apply #'max
                                            (mapcar
                                             (lambda (row)
                                               (seq-reduce
                                                (lambda (sum width)
                                                  (+ sum width margin))
                                                (mapcar #'boxy--get-width row)
                                                (* -1 margin)))
                                             rows))))
                (if (> width (+ 1 (* 2 padding) children-width))
                    width
                  (+ base-width children-width))))))))

(defun boxy--get-on-top-height (box)
  "Get the height of any boxes on top of BOX."
  (apply #'max 0
         (mapcar
          #'boxy--get-on-top-height-helper
          (seq-filter
           (lambda (child)
             (and (slot-boundp child :rel)
                  (string= (oref child rel) "on top of")))
           (oref box children)))))

(defun boxy--get-on-top-height-helper (child)
  "Get the height of any boxes on top of CHILD, including child."
  (+ (boxy--get-height child)
     (apply #'max 0
            (mapcar
             #'boxy--get-on-top-height-helper
             (seq-filter
              (lambda (grandchild)
                (and (slot-boundp grandchild :rel)
                     (string= "on top of" (oref grandchild rel))))
              (oref child children))))))

(defun boxy--get-height (box &optional include-on-top)
  "Get the height of BOX.

If INCLUDE-ON-TOP is non-nil, also include height on top of box."
  (let ((on-top-height (if include-on-top (boxy--get-on-top-height box) 0)))
    (if (slot-boundp box :height)
        (+ (oref box height) on-top-height)
      (let* ((margin (boxy--margin-y box))
             (padding (boxy--padding-y box))
             (align-bottom (or (oref box in-front) (oref box on-top)))
             (height (+ (if align-bottom -1 0)
                        3 ; box walls + text
                        (* 2 padding)))
             (children (seq-filter
                        (lambda (child) (not (oref child on-top)))
                        (oref box children))))
        (if (not children)
            (+ on-top-height
               (oset box height height))
          (let* ((row-indices (cl-delete-duplicates
                               (mapcar
                                (lambda (child) (oref child y-order))
                                children)))
                 (children-height (seq-reduce
                                   (lambda (sum row)
                                     (+ sum margin row))
                                   (mapcar
                                    (lambda (r)
                                      (apply #'max 0
                                             (mapcar
                                              (lambda (child) (boxy--get-height child t))
                                              (seq-filter
                                               (lambda (child) (= r (oref child y-order)))
                                               children))))
                                    row-indices)
                                   (* -1 margin))))
            (+ on-top-height
               (oset box height (+ height children-height)))))))))

(defun boxy--get-top (box)
  "Get the top row index of BOX."
  (if (slot-boundp box :top)
      (oref box top)
    (cond
     ((slot-boundp box :top) (oref box top))
     ((oref box on-top) (- (boxy--get-top (oref box parent)) (boxy--get-height box)))
     (t
      (let ((on-top-height (boxy--get-on-top-height box))
            (margin (boxy--margin-y box))
            (padding (boxy--padding-y box)))
        (if (not (slot-boundp box :parent))
            (oset box top (+ on-top-height margin))
          (let* ((siblings (seq-filter
                            (lambda (sibling)
                              (not (or (oref sibling in-front)
                                       (oref sibling on-top))))
                            (oref (oref box parent) children)))
                 (offset (+ 2 (* 2 padding)))
                 (top (+ on-top-height offset (boxy--get-top (oref box parent)))))
            (if-let* ((directly-above (seq-reduce
                                       (lambda (above sibling)
                                         (with-slots ((sibling-y y-order)) sibling
                                           (if (< sibling-y (oref box y-order))
                                               (if above
                                                   (with-slots ((max-y y-order)) (car above)
                                                     (if (> sibling-y max-y)
                                                         (list sibling)
                                                       (if (= sibling-y max-y)
                                                           (push sibling above)
                                                         above)))
                                                 (list sibling))
                                             above)))
                                       siblings
                                       '()))
                      (above-bottom (+ margin
                                       (apply #'max
                                              (mapcar
                                               (lambda (sibling)
                                                 (+ (boxy--get-top sibling)
                                                    (boxy--get-height sibling)))
                                               directly-above)))))
                (oset box top (+ on-top-height above-bottom))
              (oset box top top)))))))))

(defun boxy--get-left (box)
  "Get the left column index of BOX."
  (if (slot-boundp box :left)
      (oref box left)
    (let ((margin (boxy--margin-x box))
          (padding (boxy--padding-x box)))
      (if (not (slot-boundp box :parent))
          (oset box left margin)
        (let* ((left (+ 1
                        padding
                        (boxy--get-left (oref box parent))))
               (to-the-left (seq-filter
                             (lambda (child)
                               (and (= (oref box y-order) (oref child y-order))
                                    (< (oref child x-order) (oref box x-order))))
                             (oref (oref box parent) children)))
               (directly-left (and to-the-left
                                   (seq-reduce
                                    (lambda (max child)
                                      (if (> (oref child x-order) (oref max x-order))
                                          child
                                        max))
                                    to-the-left
                                    (boxy-box :x-order -1.0e+INF)))))
          (if directly-left
              (oset box left (+ (boxy--get-left directly-left)
                                   (boxy--get-width directly-left)
                                   margin))
            (if (and (slot-boundp box :rel)
                     (or (string= "above" (oref box rel))
                         (string= "below" (oref box rel))))
                (oset box left (boxy--get-left (oref box rel-box)))
              (oset box left left))))))))

;;;; Boxy mode buttons

(defun boxy-button-cursor-sensor (box)
  "Create cursor functions for entering and leaving BOX."
  (let (tooltip-timer)
    (lambda (_window _oldpos dir)
      (let* ((rel-box (and (slot-boundp box :rel-box)
                           (if (slot-boundp box :display-rel-box)
                               (oref box display-rel-box)
                             (oref box rel-box))))
             (visible-rel-box rel-box))
        (while (and visible-rel-box (not (boxy-is-visible visible-rel-box t)))
          (setq visible-rel-box (oref visible-rel-box parent)))
        (when (eq dir 'entered)
          (save-excursion
            (let ((inhibit-read-only t))
              (if visible-rel-box (boxy-draw visible-rel-box boxy--rel-face))
              (boxy-draw box boxy--selected-face)))
          (if (slot-boundp box :help-echo) (message (oref box help-echo)))
          (if (slot-boundp box :tooltip)
              (setq tooltip-timer (boxy--tooltip (oref box tooltip)))))
        (when (eq dir 'left)
          (save-excursion
            (let ((inhibit-read-only t))
              (if visible-rel-box (boxy-draw visible-rel-box t))
              (boxy-draw box t)))
          (when tooltip-timer
            (cancel-timer tooltip-timer)))))))

(defun boxy-button-jump-other-window (box)
  "Jump to location of link for BOX in other window."
  (lambda ()
    (interactive)
    (let ((marker (car (oref box markers)))
          (post-jump-hook (if (slot-boundp box :post-jump-hook)
                              (oref box post-jump-hook))))
      (save-selected-window
        (switch-to-buffer-other-window (marker-buffer marker))
        (goto-char (marker-position marker))
        (if post-jump-hook (funcall post-jump-hook)))
      (object-remove-from-list box :markers marker)
      (object-add-to-list box :markers marker t))))

(defun boxy-button-jump-to (box)
  "Jump to the first occurrence of a link for BOX in the same window."
  (lambda ()
    (interactive)
    (let* ((marker (car (oref box markers)))
           (post-jump-hook (if (slot-boundp box :post-jump-hook)
                               (oref box post-jump-hook)))
           (buffer (marker-buffer marker)))
      (if-let ((window (get-buffer-window buffer)))
            (select-window window)
          (switch-to-buffer buffer))
      (goto-char (marker-position marker))
      (if post-jump-hook (funcall post-jump-hook)))))

(defun boxy-button-jump-all (box)
  "View all occurrences of links from BOX in the same window."
  (lambda ()
    (interactive)
    (let* ((markers (oref box markers))
           (post-jump-hook (if (slot-boundp box :post-jump-hook)
                               (oref box post-jump-hook)))
           (size (/ (window-height) (length markers)))
           (marker (car markers)))
      (or (<= window-min-height size)
          (error "To many buffers to visit simultaneously"))
      (switch-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker))
      (dolist (marker (cdr markers))
        (select-window (split-window nil size))
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))
        (if post-jump-hook (funcall post-jump-hook))))))

(defun boxy-button-jump-rel (box)
  "Jump to the box directly related to BOX."
  (if (not (slot-boundp box :rel-box))
      (lambda () (interactive))
    (if (slot-boundp box :display-rel-box)
        (lambda ()
          (interactive)
          (boxy-jump-to-box (oref box display-rel-box)))
      (lambda ()
        (interactive)
        (boxy-jump-to-box (oref box rel-box))))))

(defun boxy-button-cycle-children (box)
  "Cycle visibility of children of BOX."
  (lambda ()
    (interactive)
    (boxy--cycle-children box)
    (let ((world (boxy--get-world box)))
      (boxy--flex-adjust world world))
    (boxy-mode-redraw)
    (boxy-jump-to-box box)))

(defun boxy-button-create-keymap (box)
  "Create a keymap for a button in Boxy mode.

BOX is the box the button is being made for."
  (easy-mmode-define-keymap
   (mapcar
    (lambda (key) (cons (kbd (car key)) (cdr key)))
    (append
     `(("TAB"       . ,(boxy-button-cycle-children box))
       ("r"         . ,(boxy-button-jump-rel box)))
     (if (slot-boundp box :action)
         (with-slots (action) box
           `(("<mouse-1>" . ,action)
             ("RET"       . ,action)))
       (if (slot-boundp box :markers)
           `(("o"         . ,(boxy-button-jump-other-window box))
             ("<mouse-1>" . ,(boxy-button-jump-to box))
             ("RET"       . ,(boxy-button-jump-to box))
             ("M-RET"     . ,(boxy-button-jump-all box)))))))))

;;;; Private class methods

(defun boxy--make-dirty (box)
  "Clear all coordinates from BOX and its children."
  (if (slot-boundp box :top) (slot-makeunbound box :top))
  (if (slot-boundp box :left) (slot-makeunbound box :left))
  (if (slot-boundp box :width) (slot-makeunbound box :width))
  (if (slot-boundp box :height) (slot-makeunbound box :height))
  (mapc #'boxy--make-dirty (oref box children)))

(defun boxy--expand-box (box)
  "Expand all siblings and children of BOX."
  (with-slots (children hidden-children expand-children) box
    (let (fully-expanded)
      (while (not fully-expanded)
        (setq fully-expanded t)
        (while expand-children
          (setq fully-expanded nil)
          (funcall (pop expand-children) box))
        (if hidden-children (cl-rotatef children hidden-children))
        (mapc
         (lambda (child)
           (with-slots (expand-siblings) child
             (while expand-siblings
               (setq fully-expanded nil)
               (funcall (pop expand-siblings) child))))
         children)))))

(defun boxy--cycle-children (box)
  "Cycle visibility of children of BOX."
  (with-slots (children hidden-children) box
    (if (or children hidden-children)
        (progn
          (cl-rotatef children hidden-children)
          (when hidden-children
            (mapc
             (lambda (child)
               (with-slots ((grandchildren children)) child
                 (if grandchildren
                     (boxy--cycle-children child))))
             hidden-children)))
      (boxy--expand-box box))))

(defun boxy--update-visibility (box)
  "Update visibility of BOX based on `boxy--visibility'.

Also applies to children."
  (with-slots (children hidden-children) box
    (if (not (boxy-is-visible box))
        (if children (cl-rotatef children hidden-children))
      (boxy--expand-box box))
    (mapc #'boxy--update-visibility children)))

(defun boxy--get-position (box)
  "Get the buffer position of the names of BOX and its children."
  (when (slot-boundp box :name)
    (boxy-jump-to-box box)
    (point)))

(defun boxy--margin-x (box)
  "Get the inherited property :margin-x from BOX."
  (if (slot-boundp box :margin-x)
      (oref box margin-x)
    (if (slot-boundp box :parent)
        (boxy--margin-x (oref box parent))
      boxy--default-margin-x)))

(defun boxy--margin-y (box)
  "Get the inherited property :margin-y from BOX."
  (if (slot-boundp box :margin-y)
      (oref box margin-y)
    (if (slot-boundp box :parent)
        (boxy--margin-y (oref box parent))
      boxy--default-margin-y)))

(defun boxy--padding-x (box)
  "Get the inherited property :padding-x from BOX."
  (if (slot-boundp box :padding-x)
      (oref box padding-x)
    (if (slot-boundp box :parent)
        (boxy--padding-x (oref box parent))
      boxy--default-padding-x)))

(defun boxy--padding-y (box)
  "Get the inherited property :padding-y from BOX."
  (if (slot-boundp box :padding-y)
      (oref box padding-y)
    (if (slot-boundp box :parent)
        (boxy--padding-y (oref box parent))
      boxy--default-padding-y)))

(defun boxy--add-child (parent child &optional force-visible)
  "Add CHILD to PARENT according to its visibility.

If FORCE-VISIBLE, always make CHILD visible in PARENT."
  (oset child parent parent)
  (with-slots (children hidden-children) parent
    (if hidden-children
        (progn
          (object-add-to-list parent :hidden-children child t)
          (if (or force-visible (boxy-is-visible child))
              (cl-rotatef children hidden-children)))
      (if (or force-visible (boxy-is-visible child))
          (object-add-to-list parent :children child t)
        (object-add-to-list parent :hidden-children child t)))))

(defun boxy--get-world (box)
  "Get the top most box related to BOX."
  (if (slot-boundp box :parent)
      (boxy--get-world (oref box parent))
    box))

(defun boxy--primary-boxes (box)
  "Get a list of boxes from BOX which have no further relatives."
  (if (slot-boundp box :parent)
      (if-let ((next-boxes (boxy--next box)))
          (apply #'append (mapcar #'boxy--primary-boxes next-boxes))
        (list box))
    (apply #'append
           (mapcar
            #'boxy--primary-boxes
            (append (oref box children)
                    (oref box hidden-children))))))

(defun boxy--expand (box)
  "Get a list of all boxes, including BOX, that are related to BOX."
  (if (slot-boundp box :parent)
      (apply #'append (list box) (mapcar #'boxy--expand (boxy--next box)))
    (apply #'append
           (mapcar
            #'boxy--expand
            (append (oref box children)
                    (oref box hidden-children))))))

(defun boxy--get-all (box)
  "Get all boxes, including BOX, that are children of BOX."
  (apply #'append
         (list box)
         (mapcar
          #'boxy--get-all
          (append (oref box children)
                  (oref box hidden-children)))))

(defun boxy--next (box &optional exclude-children)
  "Retrieve any boxes for which the :rel-box slot is BOX.

If EXCLUDE-CHILDREN, only retrieve sibling boxes."
  (let ((relatives (append (if exclude-children '() (append (oref box children)
                                                            (oref box hidden-children)))
                           (if (slot-boundp box :parent)
                               (with-slots (parent) box
                                 (append (oref parent children)
                                         (oref parent hidden-children)))
                             '()))))
    (seq-filter
     (lambda (relative)
       (with-slots (rel-box) relative
         (and (slot-boundp relative :rel-box)
              (eq rel-box box))))
     relatives)))

(defun boxy--apply-level (box level)
  "Apply LEVEL to BOX and update all of its children."
  (oset box level level)
  (mapc
   (lambda (child) (boxy--apply-level child (+ 1 level)))
   (append (oref box children)
           (oref box hidden-children))))

(defun boxy--add-matching (box match)
  "Add relatives of BOX to MATCH."
  (oset match primary (or (oref match primary)
                          (oref box primary)))
  (if (or (slot-boundp match :markers)
          (slot-boundp box :markers))
      (oset match markers (append (and (slot-boundp match :markers) (oref match markers))
                                   (and (slot-boundp box :markers) (oref box markers)))))
  (if (and (not (slot-boundp match :action)) (slot-boundp box :action))
    (oset match action (oref box action)))
  (mapc
   (lambda (next) (boxy-add-next next match))
   (boxy--next box))
  (oset match expand-siblings (append (oref match expand-siblings)
                                      (oref box expand-siblings)))
  (oset match expand-children (append (oref match expand-children)
                                      (oref box expand-children))))

(defun boxy--position-box (box)
  "Adjust BOX's position."
  (with-slots (rel-box rel parent) box
    (unless (boxy-find-matching box rel-box)
      (if (oref box on-top)
          (oset box y-order -1.0e+INF))
      (if (oref box in-front)
          (oset box y-order 1.0e+INF))
      (cond
       ((member rel '("to the left of" "to the right of"))
        (oset box y-order (oref rel-box y-order))
        (if (string= rel "to the left of")
            (oset box x-order (oref rel-box x-order))
          (oset box x-order (+ 1 (oref rel-box x-order))))
        (let ((row-siblings (seq-filter
                             (lambda (sibling)
                               (= (oref sibling y-order) (oref rel-box y-order)))
                             (oref parent children))))
          (mapc
           (lambda (sibling)
             (with-slots ((sibling-x x-order)) sibling
                (if (>= sibling-x (oref box x-order))
                    (setq sibling-x (+ 1 sibling-x)))))
           row-siblings)))
       ((member rel '("above" "below"))
        (oset box x-order (oref rel-box x-order))
        (let ((sibling-y-orders (mapcar
                                 (lambda (sibling) (oref sibling y-order))
                                 (seq-filter
                                  (lambda (sibling)
                                    (not (or (oref sibling in-front)
                                             (oref sibling on-top))))
                                  (oref parent children)))))
          (if (string= rel "above")
              (oset box y-order (- (apply #'min 0 sibling-y-orders) 1))
            (oset box y-order (+ 1 (apply #'max 0 sibling-y-orders))))))
       ((or (oref box on-top) (oref box in-front))
        (oset box x-order
              (+ 1 (apply #'max 0
                          (mapcar
                           (lambda (child) (oref child x-order))
                           (seq-filter
                            (lambda (child)
                              (and (eq (oref box in-front) (oref child in-front))
                                   (eq (oref box on-top) (oref child on-top))))
                            (oref rel-box children))))))))
      (boxy--add-child parent box t))))


(defun boxy--flex-add (box parent world)
  "Add BOX to a PARENT box flexibly.

This function ignores the :rel slot and adds BOX in such a way
that the width of the WORLD is kept below `boxy--flex-width'
characters if possible."
  (let ((cur-width (boxy--get-width world)))
    (boxy--make-dirty world)
    (let* ((level (+ 1 (oref parent level)))
           (all-siblings (seq-filter
                          (lambda (sibling)
                            (not (or (oref sibling in-front) (oref sibling on-top))))
                          (oref parent children)))
           (last-sibling (and all-siblings
                              (seq-reduce
                               (lambda (max sibling)
                                 (if (> (oref sibling y-order) (oref max y-order))
                                     sibling
                                   (if (and (= (oref sibling y-order) (oref max y-order))
                                            (> (oref sibling x-order) (oref max x-order)))
                                       sibling
                                     max)))
                               all-siblings
                               (boxy-box :y-order -1.0e+INF)))))
      (boxy--apply-level box level)
      (boxy--add-child parent box t)
      (boxy--flex-adjust box world)
      (when last-sibling
        (oset box y-order (oref last-sibling y-order))
        (oset box x-order (+ 1 (oref last-sibling x-order)))
        (let ((new-width (boxy--get-width world)))
          (boxy--make-dirty world)
          (when (and (> new-width cur-width) (> new-width boxy--flex-width))
            (oset box y-order (+ 1 (oref last-sibling y-order)))
            (oset box x-order 0)
            (boxy--flex-adjust box world)))))))

(defun boxy--flex-adjust (box world)
  "Adjust BOX x and y orders to try to fit WORLD within `boxy--flex-width'."
  (with-slots (children) box
    (let* ((partitioned (seq-group-by
                         (lambda (child)
                           (if (oref child flex) 'flex 'absolute))
                         (oref box children)))
           (flex-children (alist-get 'flex partitioned))
           (absolute-children (alist-get 'absolute partitioned)))
      (boxy--make-dirty world)
      (oset box children '())
      (mapc
       (lambda (flex-child)
         (boxy--flex-add flex-child box world))
       flex-children)
      (mapc
       (lambda (absolute-child)
         (if (not (slot-boundp absolute-child :rel-box))
             (boxy--flex-add absolute-child box world)
           (boxy--position-box absolute-child)
           (boxy--flex-adjust absolute-child world)))
       absolute-children))))

;;;; Utility expressions

(defun boxy-fill-tooltip (str)
  "Format STR so that each line fits within `boxy-tooltip-max-width'."
  (with-temp-buffer
    (insert str)
    (let ((fill-column boxy-tooltip-max-width))
      (fill-paragraph t))
    (buffer-string)))

(defun boxy--tooltip (content)
  "Show popup tooltip with CONTENT after `boxy-tooltip-timeout' idle time."
  (when (and boxy--tooltips (not (string-empty-p content)))
    (let ((marker (point-marker)))
      (run-with-idle-timer
       boxy--tooltip-timeout nil
       (lambda ()
         (if (and (eq (marker-buffer marker)
                      (current-buffer))
                  (eq (marker-position marker)
                      (point)))
             (funcall boxy-tooltip-show-function content)))))))

(defun boxy--tooltip-show (content)
  "Show tooltip with CONTENT at point immediately."
  (let* ((cur-line (line-number-at-pos))
         (cur-column (current-column))
         (min-line (save-excursion
                    (goto-char (window-start))
                    (line-number-at-pos)))
         (max-column (+ (window-hscroll) (window-body-width)))
         (rows (split-string content "\n"))
         (height (length rows))
         (width (+ 2 (min boxy--tooltip-max-width
                          (apply #'max 0 (mapcar #'string-width rows)))))
         (top (if (< (- cur-line 2 height) min-line)
                  (+ cur-line 2)
                (- cur-line 1 height)))
         (left (if (> (+ cur-column width 1) max-column)
                   (- max-column width 1)
                 cur-column))
         overlays)
    (dolist (str rows)
      (let ((left-margin 0)
            (right-margin 0)
            start end cur-column)
        (save-excursion
          (let ((inhibit-read-only t))
            (forward-line (- top (line-number-at-pos)))
            (move-to-column left t)
            (setq cur-column (current-column))
            (if (= cur-column left)
                (setq start (point))
              (setq left-margin (- cur-column left))
              (setq start (- (point) left-margin)))
            (move-to-column (+ left width) t)
            (setq end (point))
            (setq cur-column (current-column))
            (if (not (= cur-column (+ left width)))
                (setq right-margin (- cur-column (+ left width))))))
        (setq str (format
                   (concat " %-" (number-to-string (- width 2)) "s ")
                   (truncate-string-to-width str boxy--tooltip-max-width nil nil t)))
        (let ((overlay (make-overlay start end)))
          (overlay-put overlay 'face boxy--tooltip-face)
          (overlay-put overlay 'display `((margin nil) ,str))
          (overlay-put overlay 'before-string (make-string left-margin ?\s))
          (overlay-put overlay 'after-string (make-string right-margin ?\s))
          (push overlay overlays))
        (setq top (+ top 1))))
    (boxy--make-dirty boxy--world)
    (save-excursion (boxy-mode-recalculate-box-ring))
    (push (read-event nil) unread-command-events)
    (mapc #'delete-overlay overlays)))

(provide 'boxy)

;;; boxy.el ends here
