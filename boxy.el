;;; boxy.el --- A boxy layout framework -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Tyler Grinn <tylergrinn@gmail.com>
;; Version: 1.0.2
;; File: boxy.el
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools
;; URL: https://gitlab.com/tygrdev/boxy.el

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

;;;; Options

(defgroup boxy nil
  "Customization options for boxy"
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

;;;; Faces

(defface boxy-default nil
  "Default face used in Boxy mode.")

(defface boxy-primary nil
  "Face for highlighting the name of a box.")

(face-spec-set
 'boxy-primary
 '((((background dark)) (:foreground "turquoise"))
   (t (:foreground "dark cyan")))
 'face-defface-spec)

(defface boxy-selected nil
  "Face for the current box border under cursor.")

(face-spec-set
 'boxy-selected
 '((t :foreground "light slate blue"))
 'face-defface-spec)

(defface boxy-rel nil
  "Face for the box which is related to the box under the cursor.")

(face-spec-set
 'boxy-rel
 '((t :foreground "hot pink"))
 'face-defface-spec)

(defface boxy-tooltip nil
  "Face for tooltips in a boxy diagram.")

(face-spec-set
 'boxy-tooltip
 '((((background dark)) (:background "gray30" :foreground "gray"))
   (t (:background "gainsboro" :foreground "dim gray")))
 'face-defface-spec)

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

(defun boxy-relationshipp (str)
  "Return t if STR is a boxy relationship, nil otherwise."
  (member str boxy-relationships))

;;;; Boxy mode

(defvar-local boxy--box-ring '()
  "List of buffer positions of buttons in a boxy diagram.")

(defvar-local boxy--world nil
  "Current top-level box the buffer is displaying.")

(defvar-local boxy--boxes '()
  "All boxes currently in a boxy diagram.")

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

(defun boxy-mode-reset-boxes ()
  "Set `boxy--boxes' to be a list of all boxes in the current boxy diagram."
  (setq boxy--boxes (boxy--get-all boxy--world)))

(defun boxy-mode-recalculate-box-ring ()
  "Recalculate the position of all boxes in `boxy--boxes'."
  (setq boxy--box-ring
        (seq-sort
         #'<
         (seq-filter
          #'identity
          (mapcar
           #'boxy--get-position
           (seq-filter
            (lambda (box) (boxy-is-visible box t))
            boxy--boxes))))))

(defun boxy-mode-update-visibility ()
  "Update visibility of all boxes in `boxy--boxes'."
  (boxy--update-visibility boxy--world)
  (boxy-mode-reset-boxes)
  (boxy--flex-adjust boxy--world boxy--world))

(defun boxy-mode-make-dirty ()
  "Clear all TOP LEFT WIDTH and HEIGHT coordinates from `boxy--boxes'."
  (mapc
   (lambda (box)
     (if (slot-boundp box :top) (slot-makeunbound box :top))
     (if (slot-boundp box :left) (slot-makeunbound box :left))
     (if (slot-boundp box :width) (slot-makeunbound box :width))
     (if (slot-boundp box :height) (slot-makeunbound box :height)))
   boxy--boxes))

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
  (let ((inhibit-message t))     ;FIXME: Please report the message as an error.
    (setq indent-tabs-mode nil)
    (cursor-sensor-mode t)
    (toggle-truncate-lines t)))

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
           (setq from-box (with-slots (rel-box) from-box rel-box))
           (setq match (boxy-find-matching from-box to)))
         (when match
           (setq match-found t)
           (boxy--add-matching from-box match))))
     (boxy--primary-boxes from))
    (unless match-found
      (let ((all-from-children (boxy--get-children from 'all)))
        (if (= 1 (length all-from-children))
            (progn
              (oset (car all-from-children) :flex t)
              (boxy--add-child to (car all-from-children)))
          (oset from :flex t)
          (boxy--add-child to from))))))

(defun boxy-is-visible (box &optional calculate)
  "Determine if BOX is visible according to `boxy--visibility'.

If CALCULATE, determine if the box has been expanded manually."
  (if calculate
      (if (not (slot-boundp box :parent))
          t
        (with-slots (parent) box
          (seq-find
           (lambda (sibling) (eq sibling box))
           (boxy--get-children parent))))
    (with-slots (level) box
      (or (= 0 boxy--visibility)
          (<= level boxy--visibility)))))


(defun boxy-jump-to-box (box)
  "Jump cursor to the first character in the label of BOX."
  (if (not (boxy-is-visible box t))
      (let ((top (with-slots (parent) box parent)))
        (boxy--cycle-children top)
        (while (not (boxy-is-visible top t))
          (setq top (with-slots (parent) top parent))
          (boxy--cycle-children top))
        (boxy-mode-reset-boxes)
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
              (string= search-name
                       (with-slots (name) box name))))
       (boxy--expand world)))))

(defun boxy-add-next (next prev &optional force-visible skip-next)
  "Add NEXT to world according to its relationship to PREV.

If FORCE-VISIBLE, show the box regardless of
`boxy--visibility'

If SKIP-NEXT, don't add expansion slots for boxes related to
NEXT."
  (if-let ((match (boxy-find-matching next prev)))
      (boxy--add-matching next match)
    (with-slots
        (children
         hidden-children
         (prev-level level)
         (prev-primary primary)
         (prev-behind behind)
         (prev-in-front in-front)
         (prev-on-top on-top))
        prev
      (with-slots
          (rel
           rel-box
           flex
           display-rel
           display-rel-box
           (next-level level)
           (next-behind behind)
           (next-in-front in-front)
           (next-on-top on-top))
          next
        (if (not (slot-boundp prev :parent))
            (progn
              (setq flex t)
              (setq next-level (+ 1 prev-level))
              (boxy--add-child prev next force-visible))
          (let ((parent (with-slots (parent) prev parent)))
            (if (slot-boundp next :display-rel-box)
                (setq display-rel-box
                      (boxy-find-matching
                       display-rel-box
                       (boxy--get-world prev))))
            (if (string= rel "on top of")
                (setq next-on-top t))
            (if (string= rel "in front of")
                (setq next-in-front t))
            (let* ((next-boxes (boxy--next next))
                   (partitioned (seq-group-by
                                 (lambda (next-next)
                                   (with-slots (rel) next-next
                                     (if (member rel boxy-children-relationships)
                                         'children
                                       'siblings)))
                                 next-boxes))
                   (children-boxes (alist-get 'children partitioned))
                   (sibling-boxes (alist-get 'siblings partitioned))
                   update-visibility)
              (if-let ((match (boxy-find-matching next prev)))
                  (boxy--add-matching next match)
                (cond
                 ((member rel '("to the left of" "to the right of"))
                  (setq next-level prev-level)
                  (setq next-behind prev-behind)
                  (setq next-in-front prev-in-front)
                  (setq next-on-top prev-on-top))
                 ((member rel '("above" "below"))
                  (setq next-behind prev-behind)
                  (cond
                   ((and prev-in-front (string= rel "below"))
                    (setq update-visibility t)
                    (setq display-rel-box prev)
                    (while (with-slots (in-front) prev in-front)
                      (setq prev (with-slots (parent) prev parent)))
                    (setq parent (with-slots (parent) prev parent))
                    (setq next-level (with-slots (level) prev level)))
                   ((and prev-on-top (string= rel "above"))
                    (setq update-visibility t)
                    (setq display-rel-box prev)
                    (while (with-slots (on-top) prev on-top)
                      (setq prev (with-slots (parent) prev parent)))
                    (setq parent (with-slots (parent) prev parent))
                    (setq next-level (with-slots (level) prev level)))
                   ((and prev-on-top (string= rel "below"))
                    (setq update-visibility t)
                    (setq display-rel rel)
                    (setq display-rel-box prev)
                    (setq rel "in")
                    (setq prev parent)
                    (setq next-level (+ 1 (with-slots (level) prev level))))
                   (t
                    (setq next-level prev-level))))
                 ((or next-on-top next-in-front)
                  (setq next-level (+ 1 prev-level))
                  (setq next-behind prev-behind))
                 ((member rel '("in" "on"))
                  (setq flex t)
                  (setq next-behind prev-behind)
                  (setq next-level (+ 1 prev-level)))
                 ((string= rel "behind")
                  (setq flex t)
                  (setq next-level (+ 1 prev-level))
                  (setq next-behind t)))
                (oset next :rel-box prev)
                (if (member rel boxy-children-relationships)
                    (boxy--add-child prev next force-visible)
                  (boxy--add-child parent next force-visible))
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
                  (if update-visibility (boxy--update-visibility (boxy--get-world prev))))))))))))

;;;; Drawing

(defun boxy-draw (box &optional border-face)
  "Insert an ascii drawing of BOX into the current buffer.

If BORDER-FACE is non-nil, skip drawing children boxes and only
update text properties on the border.  If BORDER-FACE is t, use
the `boxy-default' face, otherwise, use BORDER-FACE.

Uses `boxy--offset' to determine row and column offsets."
  (let (box-coords)
    (with-slots
        (name
         behind
         in-front
         on-top
         (dashed behind)
         primary
         markers
         hidden-children
         expand-children)
        box
      (when (slot-boundp box :name)
        (let* ((top (+ (car boxy--offset) (boxy--get-top box)))
               (left (+ (cdr boxy--offset) (boxy--get-left box)))
               (width (boxy--get-width box))
               (height (boxy--get-height box))
               (double (or hidden-children expand-children))
               (align-bottom (or in-front on-top)))
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
                                                     'face (if primary
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
                                    (delete-char (min (length str) remaining-chars))))))
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
                       name)
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
                (setq r (+ r 1))))))))
    (if border-face
        (if box-coords (list box-coords) nil)
      (apply #'append
             (if box-coords (list box-coords) nil)
             (mapcar
              #'boxy-draw
              (boxy--get-children box))))))

(defun boxy--get-width (box)
  "Get the width of BOX."
  (with-slots ((stored-width width)) box
    (if (slot-boundp box :width)
        stored-width
      (let* ((margin (boxy--margin-x box))
             (padding (boxy--padding-x box))
             (base-width (+ 2           ; box walls
                            (* 2 padding)))
             (width (+ base-width
                       (if (slot-boundp box :name)
                           (with-slots (name) box (length name))
                         0)))
             (children (boxy--get-children box)))
        (setq stored-width
              (if (not children)
                  width
                (let* ((row-indices (cl-delete-duplicates
                                     (mapcar
                                      (lambda (child) (with-slots (y-order) child y-order))
                                      children)))
                       (rows (mapcar
                              (lambda (r)
                                (cl-delete-duplicates
                                 (seq-filter
                                  (lambda (child) (with-slots (y-order) child (= r y-order)))
                                  children)
                                 :test #'(lambda (a b)
                                           (and (slot-boundp a :name)
                                                (slot-boundp b :name)
                                                (string= (with-slots (name) a name)
                                                         (with-slots (name) b name))))))
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
                  (if (> width (+ (* 2 padding) children-width))
                      width
                    (+ base-width children-width)))))))))

(defun boxy--get-on-top-height (box)
  "Get the height of any boxes on top of BOX."
  (apply #'max 0
         (mapcar
          #'boxy--get-on-top-height-helper
          (seq-filter
           (lambda (child) (with-slots (rel) child
                        (and (slot-boundp child :rel)
                             (string= rel "on top of"))))
           (boxy--get-children box)))))

(defun boxy--get-on-top-height-helper (child)
  "Get the height of any boxes on top of CHILD, including child."
  (with-slots (rel) child
    (+
     (boxy--get-height child)
     (apply #'max 0
            (mapcar
             #'boxy--get-on-top-height-helper
             (seq-filter
              (lambda (grandchild)
                (with-slots ((grandchild-rel rel)) grandchild
                  (and (slot-boundp grandchild :rel)
                       (string= "on top of" grandchild-rel))))
              (boxy--get-children child)))))))

(defun boxy--get-height (box &optional include-on-top)
  "Get the height of BOX.

If INCLUDE-ON-TOP is non-nil, also include height on top of box."
  (let ((on-top-height (if include-on-top (boxy--get-on-top-height box) 0)))
    (with-slots ((stored-height height) in-front on-top) box
      (if (slot-boundp box :height)
          (+ stored-height on-top-height)
        (let* ((margin (boxy--margin-y box))
               (padding (boxy--padding-y box))
               (height (+ (if (or in-front on-top) -1 0)
                          3 ; box walls + text
                          (* 2 padding)))
               (children (seq-filter
                          (lambda (child) (with-slots (on-top) child (not on-top)))
                          (boxy--get-children box))))
          (if (not children)
              (progn
                (setq stored-height height)
                (+ height on-top-height))
            (let* ((row-indices (cl-delete-duplicates
                                 (mapcar
                                  (lambda (child) (with-slots (y-order) child y-order))
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
                                                 (lambda (child)
                                                   (with-slots (y-order) child (= r y-order)))
                                                 children))))
                                      row-indices)
                                     (* -1 margin))))

              (setq stored-height (+ height children-height))
              (+ stored-height on-top-height))))))))

(defun boxy--get-top (box)
  "Get the top row index of BOX."
  (with-slots ((stored-top top) on-top parent x-order y-order rel rel-box) box
    (cond ((slot-boundp box :top) stored-top)
          (on-top (- (boxy--get-top parent) (boxy--get-height box)))
          (t
           (let ((on-top-height (boxy--get-on-top-height box))
                 (margin (boxy--margin-y box))
                 (padding (boxy--padding-y box)))
             (if (not (slot-boundp box :parent))
                 (setq stored-top (+ on-top-height margin))
               (let* ((siblings (seq-filter
                                 (lambda (sibling)
                                   (with-slots (on-top in-front) sibling
                                     (not (or on-top in-front))))
                                 (boxy--get-children parent)))
                      (offset (+ 2 (* 2 padding)))
                      (top (+ on-top-height offset (boxy--get-top parent))))
                 (if-let* ((directly-above (seq-reduce
                                            (lambda (above sibling)
                                              (with-slots ((sibling-y y-order)) sibling
                                                (if (< sibling-y y-order)
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
                     (setq stored-top (+ on-top-height above-bottom))
                   (setq stored-top top)))))))))

(defun boxy--get-left (box)
  "Get the left column index of BOX."
  (with-slots ((stored-left left) parent x-order y-order) box
    (if (slot-boundp box :left)
        stored-left
      (let ((margin (boxy--margin-x box))
            (padding (boxy--padding-x box)))
        (if (not (slot-boundp box :parent))
            (setq stored-left margin)
          (let* ((left (+ 1
                          padding
                          (boxy--get-left parent)))
                 (to-the-left (seq-filter
                               (lambda (child)
                                 (with-slots ((child-y y-order) (child-x x-order)) child
                                   (and (= y-order child-y)
                                        (< child-x x-order))))
                               (boxy--get-children parent)))
                 (directly-left (and to-the-left
                                     (seq-reduce
                                      (lambda (max child)
                                        (with-slots ((max-x x-order)) max
                                          (with-slots ((child-x x-order)) child
                                            (if (> child-x max-x)
                                                child
                                              max))))
                                      to-the-left
                                      (boxy-box :x-order -1.0e+INF)))))
            (if directly-left
                (setq stored-left (+ (boxy--get-left directly-left)
                                     (boxy--get-width directly-left)
                                     margin))
              (with-slots (rel rel-box) box
                (if (and (slot-boundp box :rel)
                         (or (string= "above" rel)
                             (string= "below" rel)))
                    (setq stored-left (boxy--get-left rel-box))
                  (setq stored-left left))))))))))

;;;; Boxy mode buttons

(defun boxy-button-cursor-sensor (box)
  "Create cursor functions for entering and leaving BOX."
  (let (tooltip-timer)
    (lambda (_window _oldpos dir)
       (with-slots
           ((actual-rel rel)
            (actual-rel-box rel-box)
            display-rel-box
            display-rel
            name
            tooltip
            help-echo)
           box
         (let* ((rel-box (and (slot-boundp box :rel-box)
                              (if (slot-boundp box :display-rel-box)
                                  display-rel-box
                                actual-rel-box)))
                (visible-rel-box rel-box))
           (while (and visible-rel-box (not (boxy-is-visible visible-rel-box t)))
             (setq visible-rel-box (with-slots (parent) visible-rel-box parent)))
           (when (eq dir 'entered)
             (save-excursion
               (let ((inhibit-read-only t))
                 (if visible-rel-box (boxy-draw visible-rel-box boxy--rel-face))
                 (boxy-draw box boxy--selected-face)))
             (if (slot-boundp box :help-echo) (message help-echo))
             (if (slot-boundp box :tooltip)
                 (setq tooltip-timer (boxy--tooltip tooltip))))
           (when (eq dir 'left)
             (save-excursion
               (let ((inhibit-read-only t))
                 (if visible-rel-box (boxy-draw visible-rel-box t))
                 (boxy-draw box t)))
             (when tooltip-timer
               (cancel-timer tooltip-timer))))))))

(defun boxy-button-jump-other-window (box)
  "Jump to location of link for BOX in other window."
  (with-slots (markers) box
    (lambda ()
      (interactive)
      (let ((first (car markers)))
        (object-remove-from-list box :markers first)
        (object-add-to-list box :markers first t))
      (let* ((marker (car markers))
             (buffer (marker-buffer marker))
             (pos (marker-position marker)))
        (save-selected-window
          (switch-to-buffer-other-window buffer)
          (goto-char pos))))))

(defun boxy-button-jump-to (box)
  "Jump to the first occurrence of a link for BOX in the same window."
  (with-slots (markers) box
    (lambda ()
      (interactive)
      (let* ((marker (car markers))
             (buffer (marker-buffer marker))
             (pos (marker-position marker)))
        (if-let ((window (get-buffer-window buffer)))
            (select-window window)
          (switch-to-buffer buffer))
        (goto-char pos)))))

(defun boxy-button-jump-all (box)
  "View all occurrences of links from BOX in the same window."
  (with-slots (markers) box
    (lambda ()
      (interactive)
      (let* ((size (/ (window-height) (length markers)))
             (marker (car markers)))
        (or (<= window-min-height size)
            (error "To many buffers to visit simultaneously"))
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))
        (dolist (marker (cdr markers))
          (select-window (split-window nil size))
          (switch-to-buffer (marker-buffer marker))
          (goto-char (marker-position marker)))))))

(defun boxy-button-jump-rel (box)
  "Jump to the box directly related to BOX."
  (with-slots (rel-box display-rel-box) box
    (if (not (slot-boundp box :rel-box))
        (lambda () (interactive))
      (if (slot-boundp box :display-rel-box)
          (lambda ()
            (interactive)
            (boxy-jump-to-box display-rel-box))
        (lambda ()
          (interactive)
          (boxy-jump-to-box rel-box))))))

(defun boxy-button-cycle-children (box)
  "Cycle visibility of children of BOX."
  (lambda ()
    (interactive)
    (boxy--cycle-children box)
    (boxy-mode-reset-boxes)
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
  (with-slots (children hidden-children expand-children expanded parent) box
    (if (or children hidden-children)
        (cl-rotatef children hidden-children)
      (boxy--expand-box box))))

(defun boxy--update-visibility (box)
  "Update visibility of BOX based on `boxy--visibility'."
  (with-slots (level children hidden-children expand-children) box
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
      (with-slots (margin-x) box margin-x)
    (if (slot-boundp box :parent)
        (boxy--margin-x (with-slots (parent) box parent))
      boxy--default-margin-x)))

(defun boxy--margin-y (box)
  "Get the inherited property :margin-y from BOX."
  (if (slot-boundp box :margin-y)
      (with-slots (margin-y) box margin-y)
    (if (slot-boundp box :parent)
        (boxy--margin-y (with-slots (parent) box parent))
      boxy--default-margin-y)))

(defun boxy--padding-x (box)
  "Get the inherited property :padding-x from BOX."
  (if (slot-boundp box :padding-x)
      (with-slots (padding-x) box padding-x)
    (if (slot-boundp box :parent)
        (boxy--padding-x (with-slots (parent) box parent))
      boxy--default-padding-x)))

(defun boxy--padding-y (box)
  "Get the inherited property :padding-y from BOX."
  (if (slot-boundp box :padding-y)
      (with-slots (padding-y) box padding-y)
    (if (slot-boundp box :parent)
        (boxy--padding-y (with-slots (parent) box parent))
      boxy--default-padding-y)))

(defun boxy--get-children (box &optional arg)
  "Get all visible children of BOX.

If optional ARG is 'all, include hidden children.

If optional ARG is 'hidden, only return hidden children"
  (with-slots (children hidden-children) box
    (cond
     ((eq 'all arg)
      (append children hidden-children))
     ((eq 'hidden arg)
      hidden-children)
     (t
      children))))

(defun boxy--add-child (parent child &optional force-visible)
  "Add CHILD to PARENT according to its visibility.

If FORCE-VISIBLE, always make CHILD visible in PARENT."
  (oset child :parent parent)
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
  (with-slots (parent) box
    (if (slot-boundp box :parent)
        (boxy--get-world parent)
      box)))

(defun boxy--primary-boxes (box)
  "Get a list of boxes from BOX which have no further relatives."
  (if (slot-boundp box :parent)
      (if-let ((next-boxes (boxy--next box)))
          (apply #'append (mapcar #'boxy--primary-boxes next-boxes))
        (list box))
    (apply #'append (mapcar #'boxy--primary-boxes (boxy--get-children box 'all)))))

(defun boxy--expand (box)
  "Get a list of all boxes, including BOX, that are related to BOX."
  (if (slot-boundp box :parent)
      (apply #'append (list box) (mapcar #'boxy--expand (boxy--next box)))
    (apply #'append (mapcar #'boxy--expand (boxy--get-children box 'all)))))

(defun boxy--get-all (box)
  "Get all boxes, including BOX, that are children of BOX."
  (apply #'append (list box) (mapcar #'boxy--get-all (boxy--get-children box 'all))))

(defun boxy--next (box &optional exclude-children)
  "Retrieve any boxes for which the :rel-box slot is BOX.

If EXCLUDE-CHILDREN, only retrieve sibling boxes."
  (let ((relatives (append (if exclude-children '() (boxy--get-children box 'all))
                           (if (slot-boundp box :parent)
                               (with-slots (parent) box
                                 (boxy--get-children parent 'all))
                             '()))))
    (seq-filter
     (lambda (relative)
       (with-slots (rel-box) relative
         (and (slot-boundp relative :rel-box)
              (eq rel-box box))))
     relatives)))

(defun boxy--apply-level (box level)
  "Apply LEVEL to BOX and update all of its children."
  (oset box :level level)
  (mapc
   (lambda (child) (boxy--apply-level child (+ 1 level)))
   (boxy--get-children box 'all)))

(defun boxy--add-matching (box match)
  "Add relatives of BOX to MATCH."
  (oset match :primary (or (with-slots (primary) match primary)
                           (with-slots (primary) box primary)))
  (if (or (slot-boundp match :markers)
          (slot-boundp box :markers))
      (oset match :markers (append (and (slot-boundp match :markers)
                                        (with-slots (markers) match markers))
                                   (and (slot-boundp box :markers)
                                        (with-slots (markers) box markers)))))
  (if (not (slot-boundp match :action)) (slot-boundp box :action)
    (oset match :action (with-slots (action) box action)))
  (mapc
   (lambda (next) (boxy-add-next next match))
   (boxy--next box))
  (oset match :expand-siblings (append (with-slots (expand-siblings) match expand-siblings)
                                       (with-slots (expand-siblings) box expand-siblings)))
  (oset match :expand-children (append (with-slots (expand-children) match expand-children)
                                       (with-slots (expand-children) box expand-children))))

(defun boxy--position-box (box)
  "Adjust BOX's position."
  (with-slots (rel-box rel parent x-order y-order on-top in-front parent) box
    (with-slots ((rel-y y-order) (rel-x x-order)) rel-box
      (unless (boxy-find-matching box rel-box)
        (if on-top
            (setq y-order -1.0e+INF))
        (if in-front
            (setq y-order 1.0e+INF))
        (cond
         ((member rel '("to the left of" "to the right of"))
          (setq y-order rel-y)
          (if (string= rel "to the left of")
              (setq x-order rel-x)
            (setq x-order (+ 1 rel-x)))
          (let ((row-siblings (seq-filter
                               (lambda (sibling)
                                 (with-slots ((sibling-y y-order)) sibling
                                   (= sibling-y rel-y)))
                               (boxy--get-children parent))))
            (mapc
             (lambda (sibling)
               (with-slots ((sibling-x x-order)) sibling
                 (if (>= sibling-x x-order)
                     (setq sibling-x (+ 1 sibling-x)))))
             row-siblings)))
         ((member rel '("above" "below"))
          (setq x-order rel-x)
          (let ((sibling-y-orders (mapcar
                                   (lambda (sibling) (with-slots (y-order) sibling y-order))
                                   (seq-filter
                                    (lambda (sibling)
                                      (with-slots (in-front on-top) sibling
                                        (not (or in-front on-top))))
                                    (boxy--get-children parent)))))
            (if (string= rel "above")
                (setq y-order (- (apply #'min 0 sibling-y-orders) 1))
              (setq y-order (+ 1 (apply #'max 0 sibling-y-orders))))))
         ((or on-top in-front)
          (setq x-order (+ 1 (apply #'max 0
                                    (mapcar
                                     (lambda (child) (with-slots (x-order) child x-order))
                                     (seq-filter
                                      (lambda (child)
                                        (with-slots ((child-in-front in-front) (child-on-top on-top)) child
                                           (and (eq in-front child-in-front)
                                                (eq on-top child-on-top))))
                                      (boxy--get-children rel-box))))))))
        (boxy--add-child parent box t)))))


(defun boxy--flex-add (box parent world)
  "Add BOX to a PARENT box flexibly.

This function ignores the :rel slot and adds BOX in such a way
that the width of the WORLD is kept below `boxy--flex-width'
characters if possible."
  (let ((cur-width (boxy--get-width world)))
    (boxy-mode-make-dirty)
    (with-slots ((parent-level level) (parent-behind behind)) parent
      (let* ((level (+ 1 parent-level))
             (all-siblings (seq-filter
                            (lambda (sibling)
                              (with-slots (in-front on-top) sibling
                                (not (or in-front on-top))))
                            (boxy--get-children parent)))
             (last-sibling (and all-siblings
                                (seq-reduce
                                 (lambda (max sibling)
                                   (with-slots ((max-x x-order) (max-y y-order)) max
                                     (with-slots ((sibling-x x-order) (sibling-y y-order)) sibling
                                       (if (> sibling-y max-y)
                                           sibling
                                         (if (and (= max-y sibling-y) (> sibling-x max-x))
                                             sibling
                                           max)))))
                                 all-siblings
                                 (boxy-box :y-order -1.0e+INF)))))
        (boxy--apply-level box level)
        (boxy--add-child parent box t)
        (boxy--flex-adjust box world)
        (when last-sibling
          (with-slots
              ((last-sibling-y y-order)
               (last-sibling-x x-order))
              last-sibling
            (oset box :y-order last-sibling-y)
            (oset box :x-order (+ 1 last-sibling-x))
            (let ((new-width (boxy--get-width world)))
              (boxy-mode-make-dirty)
              (when (and (> new-width cur-width) (> new-width boxy--flex-width))
                (oset box :y-order (+ 1 last-sibling-y))
                (oset box :x-order 0)
                (boxy--flex-adjust box world)))))))))

(defun boxy--flex-adjust (box world)
  "Adjust BOX x and y orders to try to fit WORLD within `boxy--flex-width'."
  (with-slots (children) box
    (let* ((partitioned (seq-group-by
                         (lambda (child)
                           (if (with-slots (flex) child flex)
                               'flex
                             'absolute))
                         children))
           (flex-children (alist-get 'flex partitioned))
           (other-children (alist-get 'absolute partitioned)))
      (setq children '())
      (boxy-mode-make-dirty)
      (mapc
       (lambda (flex-child)
         (boxy--flex-add flex-child box world))
       flex-children)
      (mapc
       (lambda (other-child)
         (if (not (slot-boundp other-child :rel-box))
             (boxy--flex-add other-child box world)
           (boxy--position-box other-child)
           (boxy--flex-adjust other-child world)))
       other-children))))

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
             (boxy--tooltip-show content)))))))

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
                          (apply #'max 0 (mapcar #'length rows)))))
         (top (if (< (- cur-line 2 height) min-line)
                  (+ cur-line 2)
                (- cur-line 1 height)))
         (left (if (> (+ cur-column width 1) max-column)
                   (- max-column width 1)
                 cur-column))
         overlay overlays)
    (dolist (str rows)
      (let* ((pos (save-excursion
                    (forward-line (- top (line-number-at-pos)))
                    (let ((inhibit-read-only t))
                      (move-to-column left t))
                    (point)))
             (remaining-chars (save-excursion
                                (goto-char pos)
                                (- (save-excursion
                                     (end-of-line)
                                     (current-column))
                                   (current-column)))))
        (setq str (format
                   (concat " %-" (number-to-string (- width 2)) "s ")
                   (truncate-string-to-width str boxy--tooltip-max-width nil nil t)))
        (when (= 0 remaining-chars)
          (save-excursion (goto-char pos) (let ((inhibit-read-only t)) (insert " ")))
          (setq remaining-chars 1))
        (setq overlay (make-overlay pos (+ pos (min remaining-chars width))))
        (overlay-put overlay 'face boxy--tooltip-face)
        (overlay-put overlay 'display `((margin nil) ,str))
        (push overlay overlays)
        (setq top (+ top 1))))
    (save-excursion (boxy-mode-recalculate-box-ring))
    (push (read-event nil) unread-command-events)
    (mapc #'delete-overlay overlays)))

(provide 'boxy)

;;; boxy.el ends here
