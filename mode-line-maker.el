;;; mode-line-maker.el --- Mode-line helper tools -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/mode-line-maker
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, mode-line, header-line

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; mode-line-maker is a package to ease the creation of mode-line,
;; header-line or tab-line.  It allows to define the precise alignment
;; of the mode-line (on left and right sides) and splits the mode-line
;; into left and right parts, with automatic truncation (when there is
;; too much information to display).
;;
;; These features come with a price in rendering speed because
;; everything is computed dynamically.  From early benchmarks, you can
;; expect a significant slowdown (between x2 and x3).  Things can get
;; worse if you use pixel-wise alignment because of the
;; string-pixel-width call.
;;
;; If you only need alignment on left and right, you can directly use
;; the 'nano-line-make--padding function to get the relevant
;; prefix/suffix to be prepended/appended to your mode-line.

;; Usage:
;;
;; (setq mode-line-format (mode-line-maker '("%b") '("%3c:%3l")))

;;; NEWS:
;;
;; Version  0.1
;; - First version

;;; Code:

(defface mode-line-maker-padding-face
  `((t (:foreground ,(face-foreground 'default nil 'default)
        :background ,(face-background 'default nil 'default)
        :box nil
        :overline nil
        :underline nil
        :inverse-video nil
        :strike-through nil)))
  "Face for mode-line padding.")

(defcustom mode-line-maker-alignment '(window . window)
  "Left and right alignment of the mode-line.

Depending on the position of the fringe (outside or inside margins), the
semantic of the alignment changes (see below).

'fringes-outside-margins' t
   ┌───┬────────┬───────────────────────────────────────┬────────┬───┬───┐
   │                         'window alignment                           │
   └───┴────────┴───────────────────────────────────────┴────────┴───┴───┘
   ┌───┬────────┬───────────────────────────────────────┬────────┬───┐
   │                         'fringe alignment                       │
   └───┴────────┴───────────────────────────────────────┴────────┴───┘
       ┌────────┬───────────────────────────────────────┬────────┐
       │                     'margin alignment                   │
       └────────┴───────────────────────────────────────┴────────┘
                ┌───────────────────────────────────────┐
                │             'text alignment           │
                └───────────────────────────────────────┘
   ┌───┬────────┬───────────────────────────────────────┬────────┬───┬───┐
   │   │        │                                       │        │   │   │
   │ • │    •   │               Text area               │   •    │ • │ • │
   │ │ │    │   │                                       │   │    │ │ │ │ │
   └─┼─┴────┼───┴───────────────────────────────────────┴───┼────┴─┼─┴─┼─┘
     │ Left margin                                    Right margin │   │
   Left fringe                                            Right fringe │
                                                                Scroll bar

'fringes-outside-margins' nil
   ┌───┬────────┬───────────────────────────────────────┬────────┬───┬───┐
   │                         'window alignment                           │
   └───┴────────┴───────────────────────────────────────┴────────┴───┴───┘
   ┌────────┬───┬───────────────────────────────────────┬───┬────────┐
   │                         'margin alignment                       │
   └────────┴───┴───────────────────────────────────────┴───┴────────┘
            ┌───┬───────────────────────────────────────┬───┐
            │                'fringe alignment              │
            └───┴───────────────────────────────────────┴───┘
                ┌───────────────────────────────────────┐
                │             'text alignment           │
                └───────────────────────────────────────┘
   ┌────────┬───┬───────────────────────────────────────┬───┬────────┬───┐
   │        │   │                                       │   │        │   │
   │   •    │ • │               Text area               │ • │    •   │ • │
   │   │    │ │ │                                       │ │ │    │   │ │ │
   └───┼────┴─┼─┴───────────────────────────────────────┴─┼─┴────┼───┴─┼─┘
       │ Left fringe                                Right fringe │     │
    Left margin                                           Right margin │
                                                               Scroll bar."
  :type '(cons (choice :tag "Left"
                       (const window)
                       (const margin)
                       (const fringe)
                       (const text))
               (choice :tag "Right"
                       (const window)
                       (const margin)
                       (const fringe)
                       (const text))))

(defcustom mode-line-maker-truncation-rule 'left
  "This variable defines the truncation rules to be applied.
When the concatenation of the left and right parts does not fit the
mode-line, they needs to be truncated.  Depending on the truncation
rule, left part, right part or both can be truncated.  Right part is
truncated on the left and left part is truncated on the right."
  :type  '(choice (const left)
                  (const right)
                  (const both)))

(defun mode-line-maker--truncate-string (string size &optional ellipsis direction)
  "This function truncate a STRING to SIZE characters.

The resulting string may be appended or prepended with an ELLIPSIS depending
on the DIRECTION ('left or 'right)."

  (let* ((ellipsis (or ellipsis (truncate-string-ellipsis)))
         (direction (or direction 'right)))

    (cond ;; string is small enough
          ((<= (length string) size)
           string)

          ;; ellipsis is already too big
          ((> (length ellipsis) size)
           (substring ellipsis 0 size))

          ;; Truncate on right
          ((eq direction 'right)
           (concat (substring string 0 (- size (length ellipsis)))
                   ellipsis))

          ;; Truncate on left
          ((eq direction 'left)
           (concat ellipsis
                   (substring string (- (length ellipsis) size))))

          ;; Unknown case
          (t string))))
  
(defun mode-line-maker--align-to (direction what &optional char-size pixel-size)
  "This methods return a display space specification to align text.

Alignment is made with respect to the DIRECTION ('left or 'right) of
WHAT ('window, 'margin, 'fringe or 'text) with an additional (and
optional) CHAR-SIZE and PIXEL-SIZE.  PIXEL-SIZE alignment is taken into
account only for graphics display."

  (let* ((char-size (or char-size 0))
         (pixel-size (if (display-graphic-p)
                         (or pixel-size 0)
                       0)))
    (cond ((eq 'left direction)
           (cond ((eq what 'window) (if fringes-outside-margins
                                        `(space :align-to (+ left-fringe
                                                             (,pixel-size)
                                                             ,char-size))
                                      `(space :align-to (+ left-margin
                                                           (,pixel-size)
                                                           ,char-size))))
                 ((eq what 'fringe) `(space :align-to (+ left-fringe
                                                         (,pixel-size)
                                                         ,char-size)))
                 ((eq what 'margin) `(space :align-to (+ left-margin
                                                         (,pixel-size)
                                                         ,char-size)))
                 (t                 `(space :align-to (+ left
                                                         (,pixel-size)
                                                         ,char-size)))))
          ((eq 'right direction)
           (cond ((eq what 'window) `(space :align-to (+ scroll-bar
                                                         (1.0 . scroll-bar)
                                                         (,pixel-size)
                                                         ,char-size)))
                 ((eq what 'fringe) `(space :align-to (+ right-fringe
                                                         (1.0 . right-fringe)
                                                         (,pixel-size)
                                                         ,char-size)))
                 ((eq what 'margin) `(space :align-to (+ right-margin
                                                         (1.0 . right-margin)
                                                         (,pixel-size)
                                                         ,char-size)))
                 (t                 `(space :align-to (+ right
                                                         (,pixel-size)
                                                         ,char-size))))))))
  
(defun mode-line-maker--padding-to (&optional left right face)
  "Return the left and right padding for alignment.

Padding allows to precisely align the mode-line (or header-line) to
window, margin, fringe or text extents, depending on LEFT and RIGHT.
LEFT and RIGHT can be constant ('window, 'fringe, 'margin or 'text) or a
cons specifying (what . (char-size . pixel-size)).  It returns two
strings that must be respectively prepended and appended to the
mode-line (or header-line).  An optional FACE can be given to be used
for the prefix and the suffix."

  (let* ((left (or left (car mode-line-maker-alignment)))
         (left (if (not (consp left))
                   (cons left '(0 . 0))
                 left))
         (right (or right (cdr mode-line-maker-alignment)))
         (right (if (not (consp right))
                    (cons right '(0 . 0))
                  right))
         (face (or face 'mode-line-maker-padding-face)))
    (cons
     (concat
      (propertize " "
                  'display (mode-line-maker--align-to 'left 'window))
      (propertize " "
                  'display (mode-line-maker--align-to 'left
                                                (car left)
                                                (cadr left)
                                                (cddr left))
                  'face face))
     (concat
      (propertize " "
                  'display (mode-line-maker--align-to 'right
                                                (car right)
                                                (cadr right)
                                                (cddr right)))
      (propertize " "
                  'display (mode-line-maker--align-to 'right 'window)
                  'face face)))))

(defun mode-line-maker--make (left &optional right alignment pixelwise)
  "Builds a mode-line with LEFT on the left and RIGHT on the right.

It takes care of truncating the left part, the right part or both
depending on the 'mode-line-maker-truncation-rule'.  ALIGNMENT can be
specified to replace the default 'mode-line-maker-alignment'.  PIXELWISE
specified whether pixel perfect alignment shoudl be computed (slower)."

  (let* ((right (format-mode-line right))
         (right-width (string-width right))

         (left (format-mode-line left))
         (left-width (string-width left))

         (rule mode-line-maker-truncation-rule)

         (alignment (or alignment mode-line-maker-alignment))
         (alignment-left (car alignment))
         (alignment-right (cdr alignment))
         
         (margins (window-margins))
         (margin-left (* (or (car margins) 0)  (frame-char-width)))
         (margin-right (* (or (cdr margins) 0) (frame-char-width)))

         (fringes (window-fringes))
         (fringe-left (nth 0 fringes))
         (fringe-right (nth 1 fringes))
         (fringes-outside (nth 2 fringes))

         (scroll-bar-width (or scroll-bar-width (frame-scroll-bar-width)))
    
         ;; This does not take margins nor fringes into account
         (width (window-width))

         ;; Adjust width with margins, fringes and scroll-bar depending on alignment
         (extra (+ (if fringes-outside
                       (+ (cond ((or (eq alignment-left 'window)
                                     (eq alignment-left 'fringe))
                                 (+ margin-left fringe-left))
                                ((eq alignment-left 'margin)
                                 (+ margin-left))
                                (t 0))
                          (cond ((eq alignment-right 'window)
                                 (+ margin-right fringe-right scroll-bar-width))
                                ((eq alignment-right 'fringe)
                                 (+ margin-right fringe-right))
                                ((eq alignment-right 'fringe)
                                 (+ margin-right))
                                (t 0)))
                     (+ (cond ((or (eq alignment-left 'window)
                                   (eq alignment-left 'margin))
                                 (+ margin-left fringe-left))
                                ((eq alignment-left 'fringe)
                                 (+ fringe-left))
                                (t 0))
                          (cond ((eq alignment-right 'window)
                                 (+ margin-right fringe-right scroll-bar-width))
                                ((eq alignment-right 'margin)
                                 (+ margin-right fringe-right))
                                ((eq alignment-right 'fringe)
                                 (+ fringe-right))
                                (t 0))))))

         ;; Add extra pixel space (converted to char) to the available width
         (width (+ width (floor (/ extra (frame-char-width)))))

         ;; Space needed vs space available
         (delta (- width (+ right-width 1 left-width))))
    
    (when (< delta 0) ;; Truncation is needed
      (cond
       
       ;; Truncate left part
         ((eq rule 'left)
          ;; mode-line is big enough
          ;; -> 2 is minimum truncated size for left part
          ;; -> 1 is for a space between left and right parts
          (if (<= (+ (length right) 2 1) width)
              (let ((w (- width right-width 1)))
                (setq left (mode-line-maker--truncate-string left w nil 'right)))
            ;; mode-line is not big enough, right part needs to be truncated
            (let ((w (- width 2 1)))
              (setq left (mode-line-maker--truncate-string left 2 nil 'right)
                    right (mode-line-maker--truncate-string right w nil 'left)))))
         
         ;; Truncate right part
         ((eq rule 'right)
          ;; mode-line is big enough
          ;; -> 2 is minimum truncated size for right part
          ;; -> 1 is for a space between left and right parts
          (if (<= (+ (length left) 2 1) width)
              (let ((w (- width left-width 1)))
                (setq right (mode-line-maker--truncate-string right w nil 'left)))
            ;; mode-line is not big enough, left part needs to be truncated
            (let ((w (- width 2 1)))
              (setq left (mode-line-maker--truncate-string left w nil 'right)
                    right (mode-line-maker--truncate-string right 2 nil 'left)))))
         
         ;; Truncate both parts (where necessary)
         ;;  2 x (first letter + ellipsis) and one space between = 5 chars minimum
         ((and (eq rule 'both) (>= width 5))
          (if (< left-width right-width)
              (let* ((lw (min (/ (1- width) 2) left-width))
                     (rw (- (1- width) lw)))
                (setq left (mode-line-maker--truncate-string left lw nil 'right)
                      right (mode-line-maker--truncate-string right rw nil 'left)))
            (let* ((rw (min (/ (1- width) 2) right-width))
                   (lw (- (1- width) rw)))
              (setq left (mode-line-maker--truncate-string left lw nil 'right)
                    right (mode-line-maker--truncate-string right rw nil 'left)))))

         ;; We cannot build the mode-line (too constrained)
         ;; -> return a string made of "!"
         (t (make-string width ?!))))

    (let* ((padding (mode-line-maker--padding-to (car alignment) (cdr alignment)))
           (pixel-size (if (and pixelwise (display-graphic-p))
                           (- (string-pixel-width right))
                         0))
           (char-size (if (and pixelwise (display-graphic-p))
                          0
                        (- (string-width right))))
           (space (mode-line-maker--align-to 'right (cdr alignment) char-size pixel-size)))
      (list (car padding)
            left
            (propertize " " 'display space)
            right
            (cdr padding)))))


(defun mode-line-maker (left &optional right alignment pixelwise)
  "Return a mode-line made of LEFT and RIGHT parts.

LEFT and RIGHT parts must be list of mode-line constructs.  The
optional ALIGNMENT can be specified to replace the default
'mode-line-maker-alignment'.  PIXELWISE specified whether pixel
perfect alignment should be computed (slower)."
  
  `(:eval (mode-line-maker--make ',left ',right ',alignment ,pixelwise)))

(provide 'mode-line-maker)
;;; mode-line-maker.el ends here
