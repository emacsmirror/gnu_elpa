;;; reader-saveplace.el --- Saveplace integration with emacs-reader  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Divya Ranjan Pattanaik
;; Copyright (C) 2025  Tushar

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

;; Bookmarks need to be stored inside a vector due to hard-coded logic
;; within `save-place--normalize-alist'.  Specifically, it performs
;; operations on an element assuming it's a list, and this leads to
;; issues when we try to use it like a bookmark.

;;; Acknowledgments:

;; `saveplace-pdf-view' by Nicolai Singh <nicolaisingh@pm.me> was used
;; for reference.

;;; Code:

(require 'bookmark)
(require 'saveplace)
(require 'reader-bookmark)

;;;###autoload
(defun reader--saveplace-find-file ()
  "Restore saved place in `reader-mode' buffers.

To be added to `save-place-after-find-file-hook'.  See also `reader--saveplace-to-alist'."
  (or save-place-loaded (save-place-load-alist-from-file))
  (when (derived-mode-p 'reader-mode)
    (if-let* ((place (assoc buffer-file-name save-place-alist))
	      (bookmark (aref (cdr place)  0))) ; get back the bookmark
        (reader-bookmark-jump bookmark))))

;;;###autoload
(add-hook 'save-place-after-find-file-hook #'reader--saveplace-find-file)

;;;###autoload
(defun reader--saveplace-to-alist (orig-fun &rest args)
  "Save place in `reader-mode' buffers or call ORIG-FUN with ARGS.

Advice around `save-place-to-alist'.  See also `reader--saveplace-find-file'."
  (if (derived-mode-p 'reader-mode)
      (when-let* ((filename buffer-file-name)
		  (bookmark-record (reader-bookmark-make-record))
		  (bookmark (cons "reader-saveplace" bookmark-record)))
        (setq save-place-alist
              (assoc-delete-all filename save-place-alist))
	;; store bookmark inside a vector
	(push (cons filename (vector bookmark)) save-place-alist))
    (apply orig-fun args)))

;;;###autoload
(advice-add 'save-place-to-alist :around #'reader--saveplace-to-alist)

(provide 'reader-saveplace)
;;; reader-saveplace.el ends here
