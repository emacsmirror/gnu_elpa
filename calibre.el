;;; calibre.el --- Interact with Calibre libraries from Emacs -*- lexical-binding:t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Kjartan Oli Agustsson <kjartanoli@disroot.org>
;; Maintainer: Kjartan Oli Agustsson <kjartanoli@disroot.org>
;; Version: 1.4.1
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.1"))
;; URL: https://git.disroot.org/kjartanoli/calibre.el

;; This file is part of calibre.el.

;; calibre.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; calibre.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with calibre.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Interact with Calibre libraries from within Emacs.
;; View the contents of your library, including much of the metadata
;; associated with each book, and read them, all from within Emacs.

;;; Code:

(defgroup calibre nil
  "Interact with a Calibre library."
  :group 'emacs)

(defcustom calibre-calibredb-executable "calibredb"
  "The calibredb executable to use."
  :type 'string
  :package-version '("calibre" . "1.0.4"))

(defcustom calibre-ebook-device-executable "ebook-device"
  "The ebook-device executable to use."
  :type 'string
  :package-version '("calibre" . "1.5.0"))

(defcustom calibre-libraries nil
  "An alist mapping library names to directories."
  :type '(repeat :tag "Libraries" (cons :tag "Library"
                                   (string :tag "Name")
                                   (directory :tag "Location")))
  :package-version '("calibre" . "0.1.0"))

(defcustom calibre-default-tags nil
  "A list of tags to add to books when they are added."
  :type '(repeat :tag "Tag" (string :tag "Tag"))
  :package-version '("calibre" . "1.5.0"))

(defun calibre--library-names ()
  "Return a list of the names of defined libraries."
  (mapcar #'car calibre-libraries))

(defconst calibre-mark-marker ?*
  "Character used to flag books for further operations.")
(defconst calibre-del-marker ?D
  "Character used to flag books for deletion.")
(defconst calibre-mod-marker ?M
  "Character used to flag books that have been modified.")


;;; Faces

(defgroup calibre-faces nil
  "Faces used by Calibre."
  :group 'calibre
  :group 'faces)

(defface calibre-modified
  '((t (:inherit warning)))
  "Face used for marked as modified."
  :group 'calibre-faces
  :package-version '("calibre" . "1.2.0"))

(defvar calibre-modified-face 'calibre-modified
  "Face used for marked as modified.")

(defface calibre-flagged
  '((t (:inherit error)))
  "Face used for books flagged for deletion."
  :group 'calibre-faces
  :package-version '("calibre" . "1.2.0"))

(defvar calibre-flagged-face 'calibre-flagged
  "Face used for books flagged for deletion.")

(defface calibre-marked
  '((t (:inherit dired-marked)))
  "Face used for books flagged for further operations."
  :group 'calibre-faces
  :package-version '("calibre" . "1.5.0"))

(defvar calibre-marked-face 'calibre-marked
  "Face used for books flagged for further operations.")

(defvar calibre-font-lock-keywords
  (list
   (list (rx-to-string `(seq line-start ,calibre-del-marker))
         '(".+" (beginning-of-line) nil (0 calibre-flagged-face)))
   (list (rx-to-string `(seq line-start ,calibre-mod-marker))
         '(".+" (beginning-of-line) nil (0 calibre-modified-face)))
   (list (rx-to-string `(seq line-start ,calibre-mark-marker))
         '(".+" (beginning-of-line) nil (0 calibre-marked-face)))))

(provide 'calibre)
;;; calibre.el ends here
