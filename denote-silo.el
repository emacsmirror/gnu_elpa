;;; denote-silo.el --- Convenience functions for using Denote in multiple silos  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote-silo
;; Version: 0.1.2
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

(defcustom denote-silo-directories
  `(,denote-directory)
  "List of file paths pointing to Denote silos.
Each file path points to a directory, which takes the same value
as the variable `denote-directory'."
  :group 'denote-silo
  :link '(info-link "(denote) Maintain separate directories for notes")
  :type '(repeat directory))

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
     (denote--completion-table 'file denote-silo-directories)
     nil :require-match nil 'denote-silo-directory-history default)))

(defun denote-silo-path-is-silo-p (path)
  "Return non-nil if PATH is among `denote-silo-directories'."
  (member path denote-silo-directories))

(defmacro denote-silo-with-silo (silo &rest args)
  "Run ARGS with SILO bound, if SILO satisfies `denote-silo-path-is-silo-p'."
  (declare (indent defun))
  `(if (denote-silo-path-is-silo-p ,silo)
       (progn
         ,@args)
     (user-error "`%s' is not among the `denote-silo-directories'" ,silo)))

;;;###autoload
(defun denote-silo-create-note (silo)
  "Select SILO and run `denote' in it.
SILO is a file path from `denote-silo-directories'.

When called from Lisp, SILO is a file system path to a directory that
conforms with `denote-silo-path-is-silo-p'."
  (interactive (list (denote-silo-directory-prompt)))
  (denote-silo-with-silo silo
    (let ((denote-directory silo))
      (call-interactively #'denote))))

;;;###autoload
(defun denote-silo-open-or-create (silo)
  "Select SILO and run `denote-open-or-create' in it.
SILO is a file path from `denote-silo-directories'.

When called from Lisp, SILO is a file system path to a directory that
conforms with `denote-silo-path-is-silo-p'."
  (interactive (list (denote-silo-directory-prompt)))
  (denote-silo-with-silo silo
    (let ((denote-directory silo))
      (call-interactively #'denote-open-or-create))))

;;;###autoload
(defun denote-silo-select-silo-then-command (silo command)
  "Select SILO and run Denote COMMAND in it.
SILO is a file path from `denote-silo-directories', while
COMMAND is one among `denote-silo-commands'.

When called from Lisp, SILO is a file system path to a directory that
conforms with `denote-silo-path-is-silo-p'."
  (interactive
   (list
    (denote-silo-directory-prompt)
    (denote-command-prompt)))
  (denote-silo-with-silo silo
    (let ((denote-directory silo))
      (call-interactively command))))

;;;###autoload
(defun denote-silo-dired (silo)
  "Switch to SILO directory using `dired'.
SILO is a file path from `denote-silo-directories'.

When called from Lisp, SILO is a file system path to a directory that
conforms with `denote-silo-path-is-silo-p'."
  (interactive (list (denote-silo-directory-prompt)))
  (denote-silo-with-silo silo
    (dired silo)))

;;;###autoload
(defun denote-silo-cd (silo)
  "Switch to SILO directory using `cd'.
SILO is a file path from `denote-silo-directories'.

When called from Lisp, SILO is a file system path to a directory that
conforms with `denote-silo-path-is-silo-p'."
  (interactive (list (denote-silo-directory-prompt)))
  (denote-silo-with-silo silo
    (cd silo)))

(provide 'denote-silo)
;;; denote-silo.el ends here
