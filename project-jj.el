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

;;;###autoload
(cl-defmethod project-files :around ((project (head vc)) &optional dirs)
  "Return a list of files in directories DIRS in PROJECT."
  ;; Intercept the primary/default `project-files' method.  Vc-jj does
  ;; not register itself as a new project backend: it hooks into the
  ;; existing VC integration into project.el (see `project-try-vc' in
  ;; `project-find-functions').  Because of that, we cannot provide a
  ;; standalone `project-files' method for a distinct backend class.
  ;; Therefore, we wrap the primary method with an :around method and
  ;; selectively override its behavior when the VC backend is JJ.
  (if (eq (cadr project) 'JJ)
      (progn
        (require 'vc-jj)
        (let* ((default-directory (expand-file-name (project-root project)))
               (files (vc-jj--process-lines (mapcar #'file-relative-name dirs)
                                            "file" "list")))
          (mapcar #'expand-file-name files)))
    (cl-call-next-method)))

;;;###autoload
(with-eval-after-load 'project
  (add-to-list 'project-vc-backend-markers-alist '(JJ . ".jj")))

(provide 'project-jj)
;;; project-jj.el ends here
