;;; project-jj.el --- A minimal project.el backend for Jujutsu VCS  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Wojciech Siewierski

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
  (mapcan
   (lambda (dir)
     (let ((default-directory dir))
       (mapcar
        #'expand-file-name
        (process-lines "jj" "file" "list"))))
   (or dirs
       (list (project-root project)))))

(defun project-try-jj (dir)
  (when-let ((root (locate-dominating-file dir ".jj")))
    (cons 'jj root)))

(with-eval-after-load 'project
  (add-hook 'project-find-functions #'project-try-jj))


(provide 'project-jj)
;;; project-jj.el ends here
