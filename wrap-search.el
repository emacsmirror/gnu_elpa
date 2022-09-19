;;; wrap-search.el --- wrapped, non-incremental search -*- lexical-binding: t -*-
;;
;; Author: Emanuel Berg <incal@dataswamp.org>
;; Created: 2021-05-23
;; Keywords: matching
;; License: GPL3+
;; URL: https://dataswamp.org/~incal/emacs-init/wrap-search.el
;; Version: 4.10.10
;;
;;; Commentary:
;;
;; The `wrap-search' package offers non-incremental search.
;; It shows hitss (hits and only hits) and search only starts
;; when one hits RET.
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
;;   (global-set-key "\C-s" #'wrap-search)
;;   (global-set-key "\C-r" #'wrap-search-again)
;;
;;; Code:

(let ((prev-str "dummy search string")
      (prev-case)
      (prev-rev)
      (prev-beg)
      (prev-end) )
  (defun wrap-search-again (&optional rev-case rev-rev)
    "Repeat the search previously done with `wrap-search'.

Interactively, use a preceding
  \\[universal-argument] to set REV-CASE (reverse current case-sensitivity setting)
  \\[universal-argument] \\[universal-argument] to set REV-REV (ditto search direction)
  \\[universal-argument] \\[universal-argument] \\[universal-argument] to set both"
    (interactive
     (list (member current-prefix-arg '( (4) (64)))
           (member current-prefix-arg '((16) (64))) ))
    (when rev-case
      (setq prev-case (not prev-case)) )
    (when rev-rev
      (setq prev-rev (not prev-rev)) )
    (wrap-search prev-str prev-case prev-rev prev-beg prev-end) )
  (declare-function wrap-search-again nil)

  (defun wrap-search-show-default ()
    (truncate-string-to-width prev-str 10 nil nil t) )
  (declare-function wrap-search-show-default nil)

  (defun wrap-search (str &optional case rev beg end)
    "Search for STR.

With CASE the search is case-sensitive.
With REV the search direction is reversed, i.e. north in the buffer from point.
BEG and END, or a region, delimits the search area. (default: whole buffer)

Interactively, use a preceeding
  \\[universal-argument] to set CASE (case-sensitive search)
  \\[universal-argument] \\[universal-argument] to set REV (search direction north)
  \\[universal-argument] \\[universal-argument] \\[universal-argument] to set both

Do \\[wrap-search-again] to repeat, with `wrap-search-again'."
    (interactive
     `(,(read-string (format-prompt "search" (wrap-search-show-default)))
       ,(member current-prefix-arg '( (4) (64)))
       ,(member current-prefix-arg '((16) (64)))
       ,@(when (use-region-p)
           (list (region-beginning) (region-end)) )))
    (let ((pos (point)))
      (when (or (not beg)
                (and rev (< pos beg)) )
        (setq beg (point-min)) )
      (when (or (not end)
                (and (not rev) (> pos end)) )
        (setq end (point-max)) )
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
                       (list #'search-forward beg end) )))
          (if (funcall search-f str search-end t)
              (message "hit: %s" (point))
            (goto-char search-beg)
            (if (funcall search-f str (+ pos (if rev 0 (length str))) t)
                (if (= pos (point))
                    (message "this is the one occurrence")
                  (message "hit: %s (wrap)" (point)) )
              (goto-char pos)
              (message "no hit") ))) )))
  (declare-function wrap-search nil) )

(provide 'wrap-search)
;;; wrap-search.el ends here
