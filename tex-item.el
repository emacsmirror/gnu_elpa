;;; tex-item.el --- Commands for working with tex items  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/tex-item.el
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tex, convenience

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

;; This package provides commands for working with \\item expressions
;; in a TeX document.  See README.org for a sample configuration.

;;; Code:

(require 'tex-mode)

(defgroup tex-item ()
  "Commands for working with tex items."
  :group 'tex)

(defcustom tex-item-items '("\\item" "\\bibitem" "\\task")
  "List of TeX item-like macros."
  :type '(repeat string))

(defcustom tex-item-envs '("itemize" "enumerate" "description")
  "List of TeX list-like environments."
  :type '(repeat string))

(defun tex-item--forward-1 (next)
  "Move to the next or previous item.
Move to the next if NEXT is non-nil, else to the previous."
  (interactive)
  (unless (bolp)
    (when-let ((pos (save-excursion
                      (beginning-of-line)
                      (re-search-forward (regexp-opt
                                          tex-item-items)
                                         (line-end-position) t))))
      (goto-char pos)))
  (let* ((start (point))
         (next-min (line-end-position))
         (begins (mapcar (lambda (env)
                           (format "\\begin{%s}" env))
                         tex-item-envs))
         (ends (mapcar (lambda (env)
                         (format "\\end{%s}" env))
                       tex-item-envs))
         (search-fn
          (lambda ()
            (when (tex-search-noncomment
                   (funcall
                    (if next #'re-search-forward #'re-search-backward)
                    (regexp-opt (append begins ends tex-item-items))
                    nil t))
              (match-string 0))))
         (delim (funcall search-fn))
         (count 0)
         success)
    (while (and delim
                (if next (>= count 0) (<= count 0))
                (not (setq success
                           (and (eq count 0)
                                (if next
                                    (> (point) next-min)
                                  t)
                                (or
                                 (member delim tex-item-items)
                                 (and next (member delim ends) 'end))))))
      (setq count (+ count (cond
                             ((member delim begins) 1)
                             ((member delim ends) -1)
                             (t 0))))
      (setq delim (funcall search-fn)))
    (cond
      (success
       (beginning-of-line))
      (t (goto-char start)
         (user-error
          (concat "No " (if next "next" "previous") " item"))))))

;;;###autoload
(defun tex-item-forward (&optional arg)
  "Move forward to the next item at the same level.
With ARG, do it that many times.  Negative arg -N means move backward
across N items."
  (interactive "^p")
  (unless arg (setq arg 1))
  (while (> arg 0)
    (tex-item--forward-1 t)
    (setq arg (1- arg)))
  (while (< arg 0)
    (tex-item--forward-1 nil)
    (setq arg (1+ arg))))

;;;###autoload
(defun tex-item-backward (&optional arg)
  "Move backward to the previous item at the same level.
With ARG, do it that many times.  Negative arg -N means move forward
across N items."
  (interactive "^p")
  (tex-item-forward (- (or arg 1))))

;;; The following is adapted from lisp.el and simple.el

(defun tex-item-mark (&optional arg allow-extend)
  "Set mark ARG items from point or move mark one item.
When called from Lisp with ALLOW-EXTEND omitted or nil, mark is set ARG
items from point.  With ARG and ALLOW-EXTEND both
non-nil (interactively, with prefix argument), the place to which mark
goes is the same place \\[tex-item-forward] would move to with the same
argument; if the mark is active, it moves ARG items from its current
position, otherwise it is set ARG items from point.  When invoked
interactively without a prefix argument and no active region, mark moves
one item forward.  When invoked interactively without a prefix argument,
and region is active, mark moves one item away of point (i.e., forward
if mark is at or after point, back if mark is before point), thus
extending the region by one item.  Since the direction of region
extension depends on the relative position of mark and point, you can
change the direction by \\[exchange-point-and-mark].  This command
assumes point is not in a string or comment."
  (interactive "P\np")
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (and transient-mark-mode mark-active)))
         (setq arg (if arg (prefix-numeric-value arg)
                     (if (< (mark) (point)) -1 1)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (condition-case error
                (progn (tex-item-forward arg))
              (scan-error
               (user-error (if (equal (cadr error)
                                      "Containing expression ends prematurely")
                               "No more item to select"
                             (cadr error)))))
            (point))))
        (t
         (unless (bolp)
           (tex-item-backward))
         (push-mark
          (save-excursion
            (condition-case error
                (progn
                  (tex-item-forward (prefix-numeric-value arg)))
              (scan-error
               (user-error (if (equal (cadr error)
                                      "Containing expression ends prematurely")
                               "No item to select"
                             (cadr error)))))
            (point))
          nil t))))

(defun tex-item-kill (&optional arg interactive)
  "Kill the item following point.
With ARG, kill that many items after point.
Negative arg -N means kill N items before point.
This command assumes point is not in a string or comment.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "p\nd")
  (if interactive
      (condition-case _
          (tex-item-kill arg nil)
        (scan-error (user-error (if (> arg 0)
                                    "No next item"
                                  "No previous item"))))
    (let ((opoint (point)))
      (tex-item-forward (or arg 1))
      (kill-region opoint (point)))))

(defun tex-item-backward-kill (&optional arg interactive)
  "Kill the item preceding point.
With ARG, kill that many items before point.
Negative arg -N means kill N items after point.
This command assumes point is not in a string or comment.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "p\nd")
  (tex-item-kill (- (or arg 1)) interactive))

(defun tex-item-transpose (arg &optional interactive)
  "Like \\[transpose-chars] (`transpose-chars'), but applies to items.
Unlike `transpose-words', point must be between the two items and not
in the middle of an item to be transposed.
With non-zero prefix arg ARG, effect is to take the item before point
and drag it forward past ARG other items (backward if ARG is negative).
If ARG is zero, the items ending at or after point and at or after mark
are interchanged.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "*p\nd")
  (if interactive
      (condition-case nil
          (tex-item-transpose arg nil)
        (scan-error (user-error "Not between two complete items")))
    (transpose-subr #'tex-item--transpose-function arg 'special)))

(defun tex-item--transpose-function (arg)
  "Method to locate a pair of points for `tex-item-transpose'.
ARG is as in the docstring for `transpose-sexps'."
  (cons (progn (tex-item-forward arg) (point))
        (progn (tex-item-forward (- arg)) (point))))

(defun tex-item--move (down)
  "Move the item at point up or down.
DOWN is non-nil to move down, nil to move up."
  (interactive)
  (tex-item-forward)
  (tex-item-transpose (if down 1 -1))
  (tex-item-backward))

(defun tex-item-move-down (&optional arg)
  "Move the item at point down.
With ARG, do it that many times.  Negative arg -N means move up N times."
  (interactive)
  (unless arg (setq arg 1))
  (while (> arg 0)
    (tex-item--move t)
    (setq arg (1- arg)))
  (while (< arg 0)
    (tex-item--move nil)
    (setq arg (1+ arg))))

(defun tex-item-move-up (&optional arg)
  "Move the item at point up.
With ARG, do it that many times.  Negative arg -N means move down N
times."
  (interactive)
  (tex-item-move-down (- (or arg 1))))

(provide 'tex-item)
;;; tex-item.el ends here
