;;; project-jj.el --- A minimal project.el backend for Jujutsu VCS  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

;; Author: Wojciech Siewierski

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

;; A minimal project.el backend for Jujutsu VCS.

;;; Code:

(require 'project)

(cl-defmethod project-root ((project (head jj)))
  (cdr project))

(cl-defmethod project-files ((project (head jj)) &optional dirs)
  "Return a list of files in directories DIRS in PROJECT."
  ;; There is a bit of filename frobbing going on in this method.  The
  ;; reason is that while jj reads and writes relative filenames, we
  ;; get passed absolute filenames in DIRS and must return absolute
  ;; (tilde-expanded) filenames.
  (let* ((default-directory (expand-file-name (project-root project)))
         (args (cons "--" (mapcar #'file-relative-name dirs)))
         (absolutify (or (not project-files-relative-names)
                         (> (length dirs) 1)))
         (files (apply #'process-lines "jj" "file" "list" args)))
    (if absolutify
        (mapcar #'expand-file-name files)
      files)))

(defun project-try-jj (dir)
  (when-let* ((root (locate-dominating-file dir ".jj")))
    (cons 'jj root)))

(with-eval-after-load 'project
  (add-hook 'project-find-functions #'project-try-jj))


(provide 'project-jj)
;;; project-jj.el ends here
