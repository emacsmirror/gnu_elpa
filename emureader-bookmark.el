;;; reader-bookmark.el --- bookmarks integration for the emacs-reader -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Tushar <tusharhero@sdf.org>

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

;;; Code:

(require 'reader)
(require 'bookmark)

;;;###autoload
(defun reader-bookmark-make-record  ()
  "Create a bookmark reader doc record."
  (let ((win (reader--current-doc-window)))
    (append (bookmark-make-record-default nil t 1)
            (list (cons 'page (reader-current-doc-pagenumber win))
		  (cons 'scale (reader-current-doc-scale-value win))
		  (cons 'hscroll (and win (reader--window-hscroll win)))
		  (cons 'vscroll (and win (window-vscroll win)))
		  '(handler . reader-bookmark-jump)))))

;;;###autoload
(defun reader-bookmark-jump (bookmark)
  "Handles BOOKMARK for record type returned by `reader-bookmark-make-record'."
  (let ((page (bookmark-prop-get bookmark 'page))
        (file (bookmark-prop-get bookmark 'filename))
        (scale (bookmark-prop-get bookmark 'scale))
	(hscroll (bookmark-prop-get bookmark 'hscroll))
	(vscroll (bookmark-prop-get bookmark 'vscroll)))
    (find-file file)
    (reader--manage-window-overlays)
    (when scale (reader-doc-scale-page scale))
    (when page (reader-goto-page page))
    (when hscroll (reader--set-window-hscroll nil hscroll))
    (when vscroll (reader--set-window-vscroll nil vscroll))))

(put 'reader-bookmark-jump 'bookmark-handler-type "Reader")

(provide 'reader-bookmark)
;;; reader-bookmark.el ends here
