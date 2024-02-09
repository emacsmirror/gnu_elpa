;;; htmlize-hack.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

;; This file is part of Emacs Muse.  It is not part of GNU Emacs.

;; Emacs Muse is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; Emacs Muse is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a fix for htmlize.el and Emacs 23.
;; To use it, add the path to this directory to your load path and
;; add (require 'htmlize-hack) to your Emacs init file.

;;; Code:

(require 'cl-lib)
(if t (require 'htmlize))               ; Don't load during compilation.

(when (equal htmlize-version "1.34")
  (defun htmlize-face-size (face)
    ;; The size (height) of FACE, taking inheritance into account.
    ;; Only works in Emacs 21 and later.
    (let ((size-list
           (cl-loop
            for f = face then (face-attribute f :inherit)
            until (or (null f) (eq f 'unspecified))
            for h = (face-attribute f :height)
            collect (if (eq h 'unspecified) nil h))))
      (cl-reduce #'htmlize-merge-size (cons nil size-list)))))

(provide 'htmlize-hack)
