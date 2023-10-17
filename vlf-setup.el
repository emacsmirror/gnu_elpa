;;; vlf-setup.el --- VLF integration with other packages  -*- lexical-binding: t -*-

;; Copyright (C) 2014-2023 Free Software Foundation, Inc.

;; Keywords: large files, integration
;; Author: Andrey Kotlarski <m00naticus@gmail.com>
;; URL: https://github.com/m00natic/vlfi

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; This package enables VLF play seamlessly with rest of Emacs.

;;; Code:

(defgroup vlf nil "View Large Files in Emacs."
  :prefix "vlf-" :group 'files)

(defcustom vlf-batch-size 1000000
  "Defines how large each batch of file data initially is (in bytes)."
  :type 'integer)

(defcustom vlf-application 'ask
  "Determines when `vlf' will be offered on opening files.
Possible values are: nil to never use it;
`ask' offer `vlf' when file size is beyond `large-file-warning-threshold';
`dont-ask' automatically use `vlf' for large files;
`always' use `vlf' for all files."
  :type '(radio (const :format "%v " nil)
                (const :format "%v " ask)
                (const :format "%v " dont-ask)
                (const :format "%v " always)))

(defcustom vlf-forbidden-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                 doc-view-mode doc-view-mode-maybe ebrowse-tree-mode)
  "Major modes which VLF will not be automatically applied to."
  :type '(list symbol))

(defvar dired-mode-map)
(declare-function dired-get-file-for-visit "dired")

(defun vlf-determine-major-mode (filename)
  "Determine major mode from FILENAME."
  (let ((name filename)
        (remote-id (file-remote-p filename))
        mode)
    ;; Remove backup-suffixes from file name.
    (setq name (file-name-sans-versions name))
    ;; Remove remote file name identification.
    (and (stringp remote-id)
         (string-match (regexp-quote remote-id) name)
         (setq name (substring name (match-end 0))))
    (setq mode
          (if (memq system-type '(windows-nt cygwin))
              ;; System is case-insensitive.
              (let ((case-fold-search t))
                (assoc-default name auto-mode-alist #'string-match))
            ;; System is case-sensitive.
            (or ;; First match case-sensitively.
             (let ((case-fold-search nil))
               (assoc-default name auto-mode-alist #'string-match))
             ;; Fallback to case-insensitive match.
             (and auto-mode-case-fold
                  (let ((case-fold-search t))
                    (assoc-default name auto-mode-alist
                                   #'string-match))))))
    (if (and mode (consp mode))
        (cadr mode)
      mode)))

(autoload 'vlf "vlf" "View Large FILE in batches." t)

(advice-add 'abort-if-file-too-large :around #'vlf--if-file-too-large)
(defun vlf--if-file-too-large (orig-fun size op-type filename &rest args)
  "If file is too large, prompt user to view file with `vlf'.
\"Too large\" is defined by `large-file-warning-threshold'.
OP-TYPE specifies the file operation being performed over FILENAME."
  (cond
   ((or (not size) (zerop size)))
   ((or (not vlf-application)
        (not filename)
        (memq (vlf-determine-major-mode filename)
              vlf-forbidden-modes-list))
    (apply orig-fun size op-type filename args))
   ((eq vlf-application 'always)
    (vlf filename)
    (error ""))
   ((and large-file-warning-threshold
         (< large-file-warning-threshold size)
         (< vlf-batch-size size))
    (if (eq vlf-application 'dont-ask)
        (progn (vlf filename)
               (error ""))
      (let ((char nil))
        (while (not (memq (setq char
                                (read-event
                                 (propertize
                                  (format
                                   "File %s is large (%s): \
%s normally (o), %s with vlf (v) or abort (a)"
                                   (if filename
                                       (file-name-nondirectory filename)
                                     "")
                                   (file-size-human-readable size)
                                   op-type op-type)
                                  'face 'minibuffer-prompt)))
                          '(?o ?O ?v ?V ?a ?A))))
        (cond ((memq char '(?v ?V))
               (vlf filename)
               (error ""))
              ((memq char '(?a ?A))
               (error "Aborted"))))))))

;; disable for some functions
(defun vlf--disabled (orig-fun &rest args)
  "Temporarily disable `vlf-mode'."
  (let ((vlf-application nil))
    (apply orig-fun args)))

(dolist (func '(tags-verify-table
                tag-find-file-of-tag-noselect
                helm-etags-create-buffer))
  (advice-add func :around #'vlf--disabled))

(defun dired-vlf ()
  "In Dired, visit the file on this line in VLF mode."
  (interactive)
  (vlf (dired-get-file-for-visit)))

(eval-after-load "dired"
  '(define-key dired-mode-map "V" #'dired-vlf))

(provide 'vlf-setup)

;;; vlf-setup.el ends here
