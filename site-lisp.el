;;; site-lisp.el --- Manage site-lisp directories  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022, 2023, 2024  Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <~pkal/public-inbox@lists.sr.ht>
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.1.2
;; URL: https://git.sr.ht/~pkal/site-lisp
;; Keywords: lisp, local

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

;; This package automates the management of local Lisp code.  Similar
;; to package.el, it will byte-compile files, scrape for autoload
;; cookies and update the `load-path', but "installing" a package just
;; involves placing a file or a directory into a "site-lisp"
;; directory, inside your Emacs configuration directory (see
;; `site-lisp-directory' if you wish to modify this behaviour).

;; New files will automatically be registered when
;; `site-lisp-initialise' is invoked (the function is autoloaded, so
;; you can add it directly to your init.el), but you can invoke the
;; command `site-lisp-reload' at any time to avoid restarting Emacs.

;;; News:

;; * Allow `site-lisp-directory' to be a list of directories.

;;; Code:

(require 'seq)

(defgroup site-lisp ()
  "Manage site-lisp directories."
  :group 'lisp)

(defcustom site-lisp-autoload-file ".auto-site.el"
  "Name of file to store autoload forms in."
  :type 'string)

(defcustom site-lisp-collect-recursivly nil
  "Non-nil means that all files should be recursively scraped."
  :type 'boolean)

(defmacro site-lisp-generate-autoloads (dir file)
  "Generate autoloads for DIR as appropriate for the current version.
The result should be stored in FILE."
  (cond
   ((version<= "29" emacs-version)
    `(loaddefs-generate ,dir ,file))
   ((version<= "28" emacs-version)
    `(make-directory-autoloads ,dir ,file))
   (`(let ((generated-autoload-file ,file))
       (update-directory-autoloads ,dir)))))

(defvar generated-autoload-file)

(defvar site-lisp--autoload-file)

;;;###autoload
(defun site-lisp-prepare (dir)
  "Byte-compile, prepare autoloads and note each directory in DIR.
If DIR is a list, the function will be applied to every element
of the list."
  (interactive "DPrepare: ")
  (if (listp dir)
      (mapc #'site-lisp-prepare dir)
    (let* ((backup-inhibited t)
           (site-lisp--autoload-file
            (or (bound-and-true-p site-lisp--autoload-file)
                (expand-file-name site-lisp-autoload-file dir)))
           (old-modtime
            ;; we check whatever the mtime of the autoload file was
            ;; before we touched anything...
            (file-attribute-modification-time
             (file-attributes site-lisp--autoload-file))))
      (dolist (dir (directory-files dir t "^[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir)
          (site-lisp-generate-autoloads
           dir site-lisp--autoload-file)
          (when (and site-lisp-collect-recursivly)
            (site-lisp-prepare dir))))
      (add-to-list 'load-path dir)
      ;; ... and reset that after returning from a possible recursive
      ;; descent through the file system, so that we don't ignore
      ;; files that are now "accidentally" older than the just updated
      ;; auto-load file.
      (set-file-times site-lisp--autoload-file old-modtime)
      (site-lisp-generate-autoloads
       dir site-lisp--autoload-file)
      (byte-recompile-directory dir)
      (load site-lisp--autoload-file nil t))))

(defun site-lisp-unprepare (dir)
  "Remove every directory in DIR from `load-path'.
If DIR is a list, the function will be applied to every element
of the list."
  (interactive "DUnprepare: ")
  (if (listp dir)
      (mapc #'site-lisp-unprepare dir)
    (dolist (sub-dir (directory-files dir t "^[^.]"))
      (when (seq-find (apply-partially #'file-equal-p sub-dir)
                      load-path)
        (when (memq dir load-path)
          (setq load-path (delq dir load-path))
          (dolist (ent (alist-get sub-dir load-history
                                  nil nil #'file-equal-p))
            (when (eq (car-safe ent) 'provide)
              (with-demoted-errors "Error while unloading: %S"
                (unload-feature (cdr ent))))))))))

;;;###autoload
(defcustom site-lisp-directory
  (file-name-as-directory (locate-user-emacs-file "site-lisp"))
  "Directory use for site-local Lisp code.
If this directory doesn't exist, nothing is done."
  :set (lambda (var val)
         (when (bound-and-true-p site-lisp-directory)
           (site-lisp-unprepare site-lisp-directory))
         (site-lisp-prepare val)
         (custom-set-default var val))
  :initialize #'custom-initialize-default
  :type '(choice (repeat directory) directory))

;;;###autoload
(defun site-lisp-reload ()
  "Reload the contents of `site-lisp-directory'."
  (interactive)
  (unless (if (listp site-lisp-directory)
              (seq-every-p #'file-directory-p site-lisp-directory)
            (file-directory-p site-lisp-directory))
    (user-error "%s is not an existing directory"
                site-lisp-directory))
  (site-lisp-prepare site-lisp-directory))

;;;###autoload
(defalias 'site-lisp-initialise #'site-lisp-reload)

(provide 'site-lisp)
;;; site-lisp.el ends here
