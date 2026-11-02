;;; emureader-bookmark.el --- bookmarks integration for the EMuReader -*- lexical-binding: t; -*-

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

(require 'emureader)
(require 'bookmark)

;;;###autoload
(defun emureader-bookmark-make-record  ()
  "Create a bookmark emureader doc record."
  (let ((win (emureader--current-doc-window)))
    (append (bookmark-make-record-default nil t 1)
            (list (cons 'page (emureader-current-doc-pagenumber win))
		  (cons 'scale (emureader-current-doc-scale-value win))
		  (cons 'hscroll (and win (emureader--window-hscroll win)))
		  (cons 'vscroll (and win (window-vscroll win)))
		  '(handler . emureader-bookmark-jump)))))

;;;###autoload
(defun emureader-bookmark-jump (bookmark)
  "Handles BOOKMARK for record type returned by `emureader-bookmark-make-record'."
  (let ((page (bookmark-prop-get bookmark 'page))
        (file (bookmark-prop-get bookmark 'filename))
        (scale (bookmark-prop-get bookmark 'scale))
	(hscroll (bookmark-prop-get bookmark 'hscroll))
	(vscroll (bookmark-prop-get bookmark 'vscroll)))
    (find-file file)
    (emureader--manage-window-overlays)
    (when scale (emureader-doc-scale-page scale))
    (when page (emureader-goto-page page))
    (when hscroll (emureader--set-window-hscroll nil hscroll))
    (when vscroll (emureader--set-window-vscroll nil vscroll))))

(put 'emureader-bookmark-jump 'bookmark-handler-type "EMuReader")

(provide 'emureader-bookmark)
;;; emureader-bookmark.el ends here
