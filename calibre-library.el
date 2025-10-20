;;; calibre-library.el --- View and interact with the Calibre library -*- lexical-binding:t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Kjartan Oli Agustsson <kjartanoli@disroot.org>
;; Maintainer: Kjartan Oli Agustsson <kjartanoli@disroot.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; View the contents of a Library, add new books, remove books, and
;; open books in Emacs.

;;; Code:
(require 'dired)
(require 'compat)
(require 'calibre-db)
(require 'calibre-book)
(require 'calibre-search)
(require 'calibre-virtual-library)
(require 'calibre-edit)
(require 'calibre-info)

(defcustom calibre-external-programs nil
  "A mapping of formats to external programs used to read them.

This is an alist, where each entry is of the form (FORMAT
. PROGRAM).  FORMAT is a symbol identifying a book format and
PROGRAM is a string naming an external program to use when
opening books in that format."
  :type '(repeat (cons (symbol :tag "Format") (string :tag "Program")))
  :group 'calibre
  :package-version '("calibre" . "1.4.0"))

(defun calibre--read-tags ()
  "Prompt the user for a list of tags."
  (completing-read-multiple "Tags: " calibre-tags-completion-table))

(defun calibre--read-authors ()
  "Prompt the user for a list of authors."
  (completing-read-multiple "Authors: " calibre-authors-completion-table))

;;;###autoload
(defun calibre-library-add-book (file &optional tags)
  "Add FILE to the Calibre library.

TAGS should be a list of strings to add to FILE."
  (interactive (list (read-file-name "File: " nil nil t)
                     (if current-prefix-arg (calibre--read-tags) nil))
               calibre-library-mode)
  (calibre-library-add-books (list file) tags))

(defun calibre-library-add-books (files &optional tags)
  "Add FILES to the Calibre library.

TAGS should be a list of strings to add to FILE."
  (calibre-exec--queue-command
   `("add" "--recurse"
     "--automerge" ,calibre-automerge-policy
     ,@(mapcar #'expand-file-name files)
     ,@(if (or tags calibre-default-tags)
           (list "--tags" (string-join (append tags calibre-default-tags) ","))
         nil)))
  (calibre-exec--start-execution))

(defun calibre-library-add-format (book file)
  "Add FILE as a format for BOOK."
  (interactive (list (tabulated-list-get-id)
                     (read-file-name "File: " nil nil t))
               calibre-library-mode)
  (calibre-exec--queue-command
     `("add_format"
       ,(number-to-string (calibre-book-id book))
       ,(expand-file-name file)))
    (calibre-exec--start-execution))

;;;###autoload
(defun calibre-dired-add (&optional tags)
  "Add marked files to the Calibre library.

TAGS should be a list of strings to add to FILE."
  (interactive (list (if current-prefix-arg (calibre--read-tags) nil))
               dired-mode)
    (if (derived-mode-p 'dired-mode)
        (calibre-library-add-books (dired-get-marked-files) tags)))

(defun calibre-library-book-at-point ()
  "Get the book at point."
  (tabulated-list-get-id))

(defun calibre--get-active-books ()
  "Get the list of books to operate on.

Get the list of books a library command should operate on.  If any books
are marked return those books otherwise return the book at point."
  (or (calibre-library-get-marked) (list (calibre-library-book-at-point))))

(defun calibre-library-add-tags (tags books)
  "Add TAGS to BOOKS if not already present."
  (interactive (list (calibre--read-tags)
                     (calibre--get-active-books))
               calibre-library-mode)
  (dolist (book books)
      (calibre-edit-add-tags tags book))
  (calibre-library--refresh))

(defun calibre-library-remove-tags (tags books)
  "Remove TAGS from BOOKS if present."
  (interactive (list (calibre--read-tags)
                     (calibre--get-active-books))
               calibre-library-mode)
  (dolist (book books)
    (calibre-edit-remove-tags tags book))
  (calibre-library--refresh))

(defun calibre-library-add-authors (authors books)
  "Add AUTHORS to BOOKS if not already present."
  (interactive (list (calibre--read-authors)
                     (calibre--get-active-books))
               calibre-library-mode)
  (dolist (book books)
      (calibre-edit-add-authors authors book))
  (calibre-library--refresh))

(defun calibre-library-remove-authors (authors books)
  "Remove AUTHORS from BOOKS if present."
  (interactive (list (calibre--read-authors)
                     (calibre--get-active-books))
               calibre-library-mode)
  (dolist (book books)
    (calibre-edit-remove-authors authors book))
  (calibre-library--refresh))

(defun calibre-library-remove-books (books)
  "Remove BOOKS from the Calibre library."
  (let ((ids (mapcar #'int-to-string (mapcar #'calibre-book-id books))))
    (calibre-exec--queue-command `("remove" ,(string-join ids ",")))))

(defun calibre-library-mark (&optional _num)
  "Mark a book for further operations and move to the next line."
  (interactive "p" calibre-library-mode)
  (tabulated-list-put-tag (char-to-string calibre-mark-marker) t))

(defun calibre-library-mark-remove (&optional _num)
  "Mark a book for removal and move to the next line."
  (interactive "p" calibre-library-mode)
  (tabulated-list-put-tag (char-to-string calibre-del-marker) t))

(defun calibre-library-mark-unmark (&optional _num)
  "Clear any marks on a book and move to the next line."
  (interactive "p" calibre-library-mode)
  (let ((book (calibre-library-book-at-point)))
    (when book
      (beginning-of-line)
      (let ((mark (char-after)))
        (unless (char-equal mark 32)
          (cond
           ((char-equal mark calibre-mod-marker)
            (calibre-edit-revert book)
            (tabulated-list-put-tag " " t))
           ((char-equal mark calibre-mark-marker)
            (if (calibre-edit-modified-p book)
                (calibre-edit-mark-modified book)
              (tabulated-list-put-tag " " t)))))))))

(defun calibre-library-unmark-all (&optional mark)
  "Clear all marks from the library buffer.

If MARK is provided clear only that mark.  When called with a prefix
argument prompts for MARK."
  (interactive (list (if current-prefix-arg (read-char "Mark: ") nil)))
  (let ((book (calibre-library-book-at-point)))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((current-mark (char-after)))
        (when (or (and mark (char-equal current-mark mark))
                  (and (not mark) (not (char-equal current-mark 32))))
          (tabulated-list-put-tag " " nil)))
      (forward-line))
    (if book
        (calibre-library--find-book book)
      (goto-char (point-max)))))

(defun calibre-library-get-marked (&optional mark)
  "Return books marked with MARK.

If MARK is not specified it defaults to `calibre-mod-marker'."
  (let ((mark (or mark calibre-mark-marker))
        books)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((book (calibre-library-book-at-point))
              (book-mark (char-after)))
          (when (eql mark book-mark) (push book books)))
        (forward-line))
      (nreverse books))))

(defun calibre-library-execute ()
  "Performed marked Library actions."
  (interactive nil calibre-library-mode)
  (let (remove-list modified-list)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((book (calibre-library-book-at-point))
              (mark (char-after)))
          (cond
            ((eql mark calibre-del-marker) (push book remove-list))
            ((eql mark calibre-mod-marker) (push book modified-list))))
        (forward-line)))
    (when remove-list (calibre-library-remove-books remove-list))
    (when modified-list (calibre-edit-commit-edits modified-list)))
  (calibre-library-revert)
  (setf calibre-edit--edited-books nil)
  (calibre-exec--start-execution)
  (tabulated-list-clear-all-tags))

(defun calibre-library-revert (&rest _IGNORED)
  "Revert the *LIBRARY* buffer."
  (let ((pos (calibre-library-book-at-point)))
    (calibre-library--refresh t)
    (if (not pos)
        (goto-char (point-max))
      (calibre-library--find-book pos))))

(defun calibre-library-open-book (book &optional arg)
  "Open BOOK in its preferred format.
If called with a prefix argument prompt the user for the format."
  (interactive (list (calibre-library-book-at-point)
                     current-prefix-arg)
               calibre-library-mode)
  (let ((format (if arg
                    (completing-read "Format: " (calibre-book-formats book) nil t)
                  (calibre-book--pick-format book))))
    (let ((program (alist-get format calibre-external-programs)))
      (if program
          (calibre-library-open-book-external book program)
        (find-file (calibre-book--file book format))))))

(defun calibre-library-open-book-other-window (book &optional arg)
  "Open BOOK in its preferred format, in another window.
If called with a prefix argument prompt the user for the format."
  (interactive (list (calibre-library-book-at-point)
                     current-prefix-arg)
               calibre-library-mode)
  (let ((format (if arg
                    (completing-read "Format: " (calibre-book-formats book) nil t)
                  (calibre-book--pick-format book))))
    (find-file-other-window (calibre-book--file book format))))

(defun calibre-library-open-book-external (book &optional command arg)
  "Open BOOK in an external program.
If called with a prefix argument prompt the user for the format."
  (interactive (list (calibre-library-book-at-point)
                     nil
                     current-prefix-arg)
               calibre-library-mode)
  (let* ((format (if arg
                    (completing-read "Format: " (calibre-book-formats book) nil t)
                  (calibre-book--pick-format book)))
         (command (if command
                      command
                    (read-shell-command "Open with: " (alist-get format calibre-external-programs)))))
    (start-process (format "%s-external" (calibre-book-title book))
                   nil
                   command
                   (calibre-book--file book format))))

(defvar-keymap calibre-library-mode-map
  :doc "Local keymap for Calibre Library buffers."
  :parent tabulated-list-mode-map
  "m" #'calibre-library-mark
  "d" #'calibre-library-mark-remove
  "u" #'calibre-library-mark-unmark
  "U" #'calibre-library-unmark-all
  "e" #'calibre-edit-book
  "x" #'calibre-library-execute
  "a" #'calibre-library-add-book
  "v" #'calibre-select-virtual-library
  "s" #'calibre-search
  "i" #'calibre-info-view-book
  "RET" #'calibre-library-open-book
  "o" #'calibre-library-open-book-other-window)

(define-derived-mode calibre-library-mode tabulated-list-mode
  "Library Mode"
  (setf tabulated-list-padding 2
        mode-line-process '((calibre-exec--executing ":Updating")
                            (calibre-device--transferring ":Transferring")))
  (setq-local revert-buffer-function #'calibre-library-revert)
  (setq-local font-lock-defaults
              '(calibre-font-lock-keywords t nil nil beginning-of-line))
  (calibre-library--set-header))

;;;###autoload
(defun calibre-library ()
  "List all books in the active Calibre Library."
  (interactive)
  (let ((buffer (get-buffer-create calibre-library-buffer)))
    (with-current-buffer buffer
      (calibre-library-mode)
      (calibre-library--refresh t)
      (display-buffer buffer))))

(provide 'calibre-library)
;;; calibre-library.el ends here
