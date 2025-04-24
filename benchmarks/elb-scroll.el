;;; elb-scroll.el --- Benchmark scrolling performance  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

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

;; Benchmarking the scrolling of a font-locked buffer.
;; This is hard to do in a way that reflects real use because it
;; requires drawing and such, which in turns requires an interactive
;; session, whereas we usually want to run benchmarks in batch mode.
;;
;; We use `redisplay-skip-initial-frame' and (redisplay 'force) to
;; get the redisplay to do its job when running in batch mode, but
;; this benchmark will still give very different results in an interactive
;; session.

;;; Code:

(defun elb--scroll-file (file)
  ;; FIXME: This relies on `elb-smie.el' being compiled already which is
  ;; not necessarily the case if we're only running some of the benchmarks.
  (load (expand-file-name "elb-smie" elb-bench-directory) nil 'nomessage)
  (setq redisplay-skip-initial-frame nil)
  (with-temp-buffer
    (rename-buffer (generate-new-buffer-name "elb-scroll"))
    (switch-to-buffer (current-buffer))
    (insert-file-contents (expand-file-name file elb-bench-directory))
    (redisplay 'force) ;; Refresh the window dimensions.
    (elb--set-win-size 23 80)
    (dotimes (_ 10)
      ;; Use our own C major mode (copied from GNU ELPA's sm-c-mode),
      ;; so it's not impacted by changes to Emacs's own C mode.
      (elb-smie-mode)
      (goto-char (point-min))
      (condition-case nil
          (while t (scroll-up nil) (redisplay 'force))
        (end-of-buffer nil)))))

(unless (and noninteractive (not (boundp 'redisplay-skip-initial-frame)))
  (defun elb-scroll-entry ()
    (elb--scroll-file "../resources/xmenu.c"))

  (defun elb-scroll-nonascii-entry ()
    (elb--scroll-file "../resources/xmenu-nonascii.c")))

(defun elb--set-win-size (height width &optional no-retry)
  (enlarge-window (- height (window-height)))
  (enlarge-window (- width (window-width)) 'horiz)
  (redisplay 'force) ;; Refresh the window dimensions.
  (unless (and (equal height (window-height))
               (equal width (window-width)))
    (if no-retry
        (error "Window size %S x %S not as stipulated by the benchmark"
               (window-height) (window-width))
      (delete-other-windows)
      (elb--set-win-size height width 'no-retry))))

(provide 'elb-scroll)
;;; elb-scroll.el ends here
