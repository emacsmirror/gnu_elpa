;;; wrap-search.el --- wrapped, non-incremental search -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Free Software Foundation, Inc.
;;
;; Author: Emanuel Berg <incal@dataswamp.org>
;; Created: 2021-05-23
;; Git: git clone https://dataswamp.org/~incal/wrap-search.git
;; Keywords: matching
;; License: GPL3+
;; URL: https://dataswamp.org/~incal/elpa/wrap-search.el
;; Version: 4.16.18
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The `wrap-search' package offers non-incremental search.
;; It shows hitss (hits and only hits) and search only starts
;; when one presses RET.
;;
;; `wrap-search' searches forward by default, but it wraps
;; and continues up and until the search start position if
;; necessary. This is so one doesn't have to wonder if the
;; target is north or south in the buffer.
;;
;; The philosophy is that `wrap-search' mostly shouldn't be
;; used for searching - that is possible as well, of course,
;; using the same command - rather, it is used to quickly
;; navigate a buffer.
;;
;; To use the package without keyboard shortcuts isn't
;; recommended. Instead, do for example
;;
;;   (keymap-global-set "C-s" #'wrap-search)
;;   (keymap-global-set "C-r" #'wrap-search-again)
;;
;; One can use the `universal-argument' key, which is
;; typically C-u, before those functions to set two search
;; options before the search. If we assume the above keys are
;; used, then, for `wrap-search', here are the keys and what
;; they do,
;;
;;         C-u C-s  do case-sensitive search
;;     C-u C-u C-s  do reverse search, direction north from point
;; C-u C-u C-u C-s  do case-sensitive, reverse search
;;
;; and for `wrap-search-again', using a corresponding interface
;;
;;         C-u C-r  reverse previous search case-sensitive setting
;;     C-u C-u C-r  reverse previous search reverse setting
;; C-u C-u C-u C-r  reverse previous search case-sensitive and reverse settings
;;
;; and the search is done again, with those settings.
;;
;; See the docstrings for `wrap-search' and
;; `wrap-search-again'.
;;
;;; Code:

(defcustom wrap-search-echo-point nil
  "Whether to echo point after a search hit."
  :group 'wrap-search
  :type  'boolean)

(let ((prev-str "dummy search string")
      (prev-case)
      (prev-rev)
      (prev-beg)
      (prev-end))
  (defun wrap-search-again (&optional rev-case rev-rev)
    "Repeat the search previously done with `wrap-search'.

Interactively, use a preceding
  \\[universal-argument] to set REV-CASE (reverse current case-sensitivity setting)
  \\[universal-argument] \\[universal-argument] to set REV-REV (ditto search direction)
  \\[universal-argument] \\[universal-argument] \\[universal-argument] to set both"
    (interactive
      (list (member current-prefix-arg '( (4) (64)))
            (member current-prefix-arg '((16) (64)))))
    (when rev-case
      (setq prev-case (not prev-case)))
    (when rev-rev
      (setq prev-rev (not prev-rev)))
    (wrap-search prev-str prev-case prev-rev prev-beg prev-end))
  (declare-function wrap-search-again nil)

  (defun wrap-search-show-default ()
    (truncate-string-to-width prev-str 10 nil nil t))
  (declare-function wrap-search-show-default nil)

  (defun wrap-search (str &optional case rev beg end)
    "Search for STR in the current buffer.

With CASE the search is case-sensitive.
With REV the search direction is reversed, so north in the buffer from point.
BEG and END, or a region, delimits the search area, defaults to whole buffer.

Interactively, use a preceeding
  \\[universal-argument] to set CASE (case-sensitive search)
  \\[universal-argument] \\[universal-argument] to set REV (search direction north)
  \\[universal-argument] \\[universal-argument] \\[universal-argument] to set both CASE and REV

Do \\[wrap-search-again] to repeat, with `wrap-search-again'."
    (interactive
     `(,(read-string (format-prompt "search" (wrap-search-show-default)))
       ,(member current-prefix-arg '( (4) (64)))
       ,(member current-prefix-arg '((16) (64)))
       ,@(when (use-region-p)
           (list (region-beginning) (region-end)))))
    (let ((pos (point)))
      (when (or (not beg)
                (and rev (< pos beg)))
        (setq beg (point-min)))
      (when (or (not end)
                (and (not rev) (> pos end)))
        (setq end (point-max)))
      (if (string= "" str)
          (wrap-search-again)
        (setq prev-str  str)
        (setq prev-case case)
        (setq prev-rev  rev)
        (setq prev-beg  beg)
        (setq prev-end  end)
        (pcase-let ((case-fold-search (not case))
                    (`(,search-f ,search-beg ,search-end)
                     (if rev
                         (list #'search-backward end beg)
                       (list #'search-forward beg end))))
          (if (funcall search-f str search-end t)
              (when wrap-search-echo-point
                (message "hit: %s" (point)))
            (goto-char search-beg)
            (if (funcall search-f str (+ pos (if rev 0 (length str))) t)
                (if (= pos (point))
                    (message "this is the only occurrence")
                  (message "wrap%s" (if wrap-search-echo-point
                                        (format ": %s" (point))
                                      "")))
              (goto-char pos)
              (message "no hit")))))))
  (declare-function wrap-search nil))

(provide 'wrap-search)
;;; wrap-search.el ends here
