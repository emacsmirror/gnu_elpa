;;; denote-silo.el --- Convenience functions for using Denote in multiple silos  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Protesilaos <info@protesilaos.com>
;; Maintainer: Protesilaos <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote-silo
;; Version: 0.3.2
;; Package-Requires: ((emacs "28.1") (denote "4.0.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a set of convenience functions that used to be provided in
;; the Denote manual.  A "silo" is a `denote-directory' that is
;; self-contained.  Users can maintain multiple silos.  Consult the
;; manual for the details.  With the Denote package installed,
;; evaluate the following to read the relevant node:
;;
;;     (info "(denote) Maintain separate directory silos for notes")

;;; Code:

(require 'denote)

(defgroup denote-silo nil
  "Make it easier to use Denote across Silos."
  :group 'denote
  :link '(info-link "(denote) Top")
  :link '(info-link "(denote-silo) Top")
  :link '(url-link :tag "Denote homepage" "https://protesilaos.com/emacs/denote")
  :link '(url-link :tag "Denote Silo homepage" "https://protesilaos.com/emacs/denote-silo"))

(defcustom denote-silo-directories (denote-directories)
  "List of file paths pointing to Denote silos.
Each file path is a directory, which takes the same value as the
variable `denote-directory'."
  :group 'denote-silo
  :link '(info-link "(denote) Maintain separate directories for notes")
  :type '(repeat directory)
  :package-version '(denote-silo . "0.4.0"))

(defvar denote-silo-directory-history nil
  "Minibuffer history for `denote-silo-directory-prompt'.")

(defalias 'denote-silo--directory-history 'denote-silo-directory-history
  "Compatibility alias for `denote-silo-directory-history'.")

(define-obsolete-function-alias
  'denote-silo--directory-prompt
  'denote-silo-directory-prompt
  "3.1.0")

(defun denote-silo-directory-prompt ()
  "Prompt for directory among `denote-silo-directories'."
  (let ((default (car denote-silo-directory-history)))
    (completing-read
     (format-prompt "Select a silo" default)
     (denote-get-completion-table denote-silo-directories '(category . file))
     nil :require-match nil 'denote-silo-directory-history default)))

(defun denote-silo-path-is-silo-p (path)
  "Return non-nil if PATH is among `denote-silo-directories'."
  (member path denote-silo-directories))

(defun denote-silo--is-silo-p (directory)
  "Return non-nil if DIRECTORY has local binding for variable `denote-directory'.
Do so by reading the .dir-locals.el file in DIRECTORY."
  (let* ((path (file-name-as-directory (expand-file-name directory)))
         (dir-locals (expand-file-name ".dir-locals.el" path)))
    (when (file-exists-p dir-locals)
      (with-temp-buffer
        (insert-file-contents dir-locals)
        (and-let* ((content (read (current-buffer)))
                    (_ (listp content))
                    (all-modes-entry (assoc nil content))
                    (denote-directory-entry (alist-get 'denote-directory all-modes-entry))
                    (_ (stringp denote-directory-entry))
                    (_ (file-equal-p (expand-file-name denote-directory-entry path) path))))))))

(defun denote-silo-maybe-make (directory)
  "Add local binding for DIRECTORY to the variable `denote-directory'.
Create the .dir-local.el if necessary.  If DIRECTORY already has a
binding for the variable `denote-directory' then do nothing."
  (when-let* ((path (file-name-as-directory (expand-file-name directory)))
              (dir-locals (expand-file-name ".dir-locals.el" path))
              (default-directory directory))
    (unless (denote-silo--is-silo-p path)
      (save-window-excursion
        (add-dir-local-variable nil 'denote-directory default-directory)
        (when-let* ((buffer (get-file-buffer dir-locals)))
          (save-buffer buffer)
          (kill-buffer buffer)))
      (message "Made `%s' a Denote silo by writing to `%s'" path dir-locals))))

(defmacro denote-silo-with-silo (silo &rest body)
  "Run BODY if SILO satisfies `denote-silo-path-is-silo-p'.
`let' bind SILO to the variable `denote-directory'."
  (declare (indent defun))
  `(if (denote-silo-path-is-silo-p ,silo)
       (let ((denote-directory ,silo))
         ,@body)
     (user-error "`%s' is not among the `denote-silo-directories'" ,silo)))

;;;###autoload
(defun denote-silo-create-note (silo)
  "Select SILO and run `denote' in it.
SILO is a file path from `denote-silo-directories'.  In interactive use,
prompt for SILO.

When called from Lisp, SILO is a file system path to a directory that
conforms with `denote-silo-path-is-silo-p'."
  (interactive (list (denote-silo-directory-prompt)))
  (denote-silo-with-silo silo
    (call-interactively #'denote)))

;;;###autoload
(defun denote-silo-open-or-create (silo)
  "Select SILO and run `denote-open-or-create' in it.
SILO is a file path from `denote-silo-directories'.  In interactive use,
prompt for SILO.

When called from Lisp, SILO is a file system path to a directory that
conforms with `denote-silo-path-is-silo-p'."
  (interactive (list (denote-silo-directory-prompt)))
  (denote-silo-with-silo silo
    (call-interactively #'denote-open-or-create)))

;;;###autoload
(defun denote-silo-select-silo-then-command (silo command)
  "Select SILO and run Denote COMMAND in it.
SILO is a file path from `denote-silo-directories'.  COMMAND is one
among `denote-commands-for-new-notes'.  In interactive use, prompt for
SILO and COMMAND.

When called from Lisp, SILO is a file system path to a directory that
conforms with `denote-silo-path-is-silo-p' and COMMAND is the symbol of
a command that is passed to `call-interactively'."
  (interactive
   (list
    (denote-silo-directory-prompt)
    (denote-command-prompt)))
  (denote-silo-with-silo silo
    (call-interactively command)))

;;;###autoload
(defun denote-silo-dired (silo)
  "Switch to SILO directory using `dired'.
SILO is a file path from `denote-silo-directories'.  In interactive use,
prompt for SILO.

When called from Lisp, SILO is a file system path to a directory that
conforms with `denote-silo-path-is-silo-p'."
  (interactive (list (denote-silo-directory-prompt)))
  (denote-silo-with-silo silo
    (denote-directories--make-paths (list silo))
    (dired silo)))

;;;###autoload
(defun denote-silo-cd (silo)
  "Switch to SILO directory using `cd'.
SILO is a file path from `denote-silo-directories'.  In interactive use,
prompt for SILO.

When called from Lisp, SILO is a file system path to a directory that
conforms with `denote-silo-path-is-silo-p'."
  (interactive (list (denote-silo-directory-prompt)))
  (denote-silo-with-silo silo
    (denote-directories--make-paths (list silo))
    (cd silo)))

(provide 'denote-silo)
;;; denote-silo.el ends here
