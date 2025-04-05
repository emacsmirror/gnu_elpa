;;; doc-view-follow-pdf-tools.el --- Support for doc-view-follow-mode with pdf-tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Keywords: convenience

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

;; Support for `doc-view-follow-mode' with `pdf-tools'.

;;; Code:

(require 'doc-view-follow)
(require 'pdf-view)

(declare-function doc-view-follow-sync-pages "doc-view-follow")

(cl-defmethod doc-view-follow-supported-p (&context (major-mode pdf-view-mode))
  "When MAJOR-MODE is `pdf-view-mode', return t."
  t)

(cl-defmethod doc-view-follow-setup (&context (major-mode pdf-view-mode))
  "When MAJOR-MODE is `pdf-view-mode', setup `doc-view-follow-mode'."
  (add-hook 'pdf-view-after-change-page-hook
            #'doc-view-follow-sync-pages nil 'local))

(cl-defmethod doc-view-follow-teardown (&context (major-mode pdf-view-mode))
  "When MAJOR-MODE is `pdf-view-mode', teardown `doc-view-follow-mode'."
  (remove-hook 'pdf-view-after-change-page-hook
               #'doc-view-follow-sync-pages 'local))

(cl-defmethod doc-view-follow-set-page (page &context (major-mode pdf-view-mode))
  "When MAJOR-MODE is `pdf-view-mode', go to PAGE in the document buffer."
  (let ((pdf-view-inhibit-redisplay nil)
        (page (max 1 (min page (pdf-cache-number-of-pages)))))
    (pdf-view-goto-page page)))

(cl-defmethod doc-view-follow-get-page (&context (major-mode pdf-view-mode))
  "When MAJOR-MODE is `pdf-view-mode', return the current page number."
  (pdf-view-current-page))

(provide 'doc-view-follow-pdf-tools)
;;; doc-view-follow-pdf-tools.el ends here
