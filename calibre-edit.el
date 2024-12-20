;;; calibre-edit.el --- Edit Book metadata  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; This file is part of calibre.el.

;; Author: Kjartan Oli Agustsson <kjartanoli@disroot.org>

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
;; This file contains the code required to edit the metadata of books
;; in a calibre library.

;;; Code:
(require 'compat) ; for defvar-keymap
(require 'wid-edit)
(require 'calibre-widgets)
(require 'calibre-core)
(require 'calibre-util)
(require 'calibre-exec)

(defvar calibre-edit--edited-books nil
  "A list containing the original copies of edited books.")

;; Declare these variables to prevent free variable warnings
(defvar-local calibre-edit--book nil
  "The book being edited in the current buffer.")
(defvar-local calibre-edit--title nil
  "The title widget in the current buffer.")
(defvar-local calibre-edit--authors nil
  "The authors widget in the current buffer.")
(defvar-local calibre-edit--publisher nil
  "The publisher widget in the current buffer.")
(defvar-local calibre-edit--series nil
  "The series widget in the current buffer.")
(defvar-local calibre-edit--tags nil
  "The tags widget in the current buffer.")

(defun calibre-edit-mark-modified (book)
  "Mark BOOK as modified in the *Library* buffer."
  (with-current-buffer (get-buffer calibre-library-buffer)
    (calibre-library--find-book book)
    (tabulated-list-put-tag (char-to-string calibre-mod-marker))))

(defun calibre-edit-apply (&rest _)
  "Apply any edits to the book in the current buffer."
  (interactive)
  (setf (calibre-book-title calibre-edit--book) (widget-value calibre-edit--title)
        (calibre-book-authors calibre-edit--book) (widget-value calibre-edit--authors)
        (calibre-book-publisher calibre-edit--book) (widget-value calibre-edit--publisher)
        (calibre-book-series calibre-edit--book) (if (consp (widget-value calibre-edit--series))
                                                     (car (widget-value calibre-edit--series))
                                                   nil)
        (calibre-book-series-index calibre-edit--book) (if (consp (widget-value calibre-edit--series))
                                                           (cdr (widget-value calibre-edit--series))
                                                         1)
        (calibre-book-tags calibre-edit--book) (widget-value calibre-edit--tags))
  (calibre-library--refresh)
  (calibre-edit-mark-modified calibre-edit--book))

(defun calibre-edit-abort (&rest _)
  "Abort any changes made in the current buffer."
  (interactive)
  (let ((book calibre-edit--book))
    (unless (calibre-edit--different-fields book (calibre-edit--find-original book))
      (setf calibre-edit--edited-books (seq-remove (lambda (b)
                                                     (= (calibre-book-id b)
                                                        (calibre-book-id book)))
                                                   calibre-edit--edited-books))))
  (quit-window t))

(defun calibre-edit-reset (&rest _)
  "Undo any changes made during this editing session."
  (interactive)
  (calibre-edit-book calibre-edit--book))

(defun calibre-edit-confirm (&rest _)
  "Apply any changes and exit."
  (interactive)
  (calibre-edit-apply)
  (quit-window t))

(defvar-keymap calibre-edit-mode-map
  :doc "Keymap for `calibre-edit-mode'."
  :parent widget-keymap
  "C-c C-c" #'calibre-edit-confirm
  "C-c C-a" #'calibre-edit-apply
  "C-c C-r" #'calibre-edit-reset
  "C-c C-k" #'calibre-edit-abort)

(defvar-keymap calibre-edit-field-keymap
  :doc "Keymap used inside editable fields in calibre edit buffers."
  :parent widget-field-keymap
  "C-c C-c" #'calibre-edit-confirm
  "C-c C-a" #'calibre-edit-apply
  "C-c C-r" #'calibre-edit-reset
  "C-c C-k" #'calibre-edit-abort)

(define-derived-mode calibre-edit-mode nil "Calibre Edit"
  :group 'calibre
  (widget-put (get 'editable-field 'widget-type) :keymap calibre-edit-field-keymap))

(defun calibre-edit--preserve-original (book)
  "Preserve the original metadata of BOOK.

Store a copy of BOOK's original metadata in
`calibre-edit--edited-books', so that any changes can be reverted later.
If BOOK already has an entry in `calibre-edit--edited-books' this
function does nothing."
  (unless (calibre-util-find-book book calibre-edit--edited-books)
    (push (copy-calibre-book book) calibre-edit--edited-books)))

(defmacro calibre-edit--metadata-adder (field)
  "Create a function to add items to FIELD of a given book."
  (let ((name (intern (format "calibre-edit-add-%s" field)))
        (getter (intern (format "calibre-book-%s" field))))
    `(defun ,name (,field book)
       ,(format "Add %s to BOOK's list of %s."
                (upcase (prin1-to-string field))
                field)
       (calibre-edit--preserve-original book)
       (when (seq-difference ,field (,getter book))
         (setf (,getter book) (seq-union ,field (,getter book)))
         (calibre-edit-mark-modified book)))))

(defmacro calibre-edit--metadata-remover (field)
  "Create a function to remove items from FIELD of a given book."
  (let ((name (intern (format "calibre-edit-remove-%s" field)))
        (getter (intern (format "calibre-book-%s" field))))
    `(defun ,name (,field book)
       ,(format "Remove %s to BOOK's list of %s."
                (upcase (prin1-to-string field))
                field)
       (calibre-edit--preserve-original book)
       (let ((difference (seq-difference (,getter book) ,field)))
         (unless (seq-set-equal-p (,getter book) difference)
           (setf (,getter book) difference)
           (calibre-edit-mark-modified book))))))

(defmacro calibre-edit--metadata-modifier-pair (field)
  "Create a pair of add/remove functions to modify FIELD of a given book."
  `(progn
     (calibre-edit--metadata-adder ,field)
     (calibre-edit--metadata-remover ,field)))

(defmacro calibre-edit--metadata-setter (field)
  "Create a function to set FIELD of a given book."
  (let ((name (intern (format "calibre-edit-set-%s" field)))
        (getter (intern (format "calibre-book-%s" field))))
    `(defun ,name (,field book)
       ,(format "Set BOOK's %s to %s"
                field
                (upcase (prin1-to-string field)))
       (calibre-edit--preserve-original book)
       (setf (,getter book) ,field))))

(defmacro calibre-edit--metadata-unsetter (field)
  "Create a function to unset FIELD of a given book."
  (let ((name (intern (format "calibre-edit-unset-%s" field)))
        (setter (intern (format "calibre-edit-set-%s" field))))
    `(defun ,name (book)
       ,(format "Unset BOOK's %s" field)
       (,setter nil book))))

(defmacro calibre-edit--metadata-setter-pair (field)
  "Create a pair of set/unset functions to modify FIELD of a given book."
  `(progn
     (calibre-edit--metadata-setter ,field)
     (calibre-edit--metadata-unsetter ,field)))

(calibre-util--macro-map
 calibre-edit--metadata-modifier-pair (tags authors))
(calibre-util--macro-map
 calibre-edit--metadata-setter-pair (title publisher series))

(defun calibre-edit-modified-p (book)
  "Return non-nil if BOOK has been modified, nil otherwise."
  (calibre-edit--find-original book))

(defun calibre-edit-add-tag (tag book)
  "Add TAG to BOOK."
  (calibre-edit-add-tags (list tag) book))

(defun calibre-edit-remove-tag (tag book)
  "Remove TAG from BOOK."
  (calibre-edit-remove-tags (list tag) book))

(defun calibre-edit-book (book)
  "Edit the metadata of BOOK."
  (interactive (list (tabulated-list-get-id)) calibre-library-mode)
  (calibre-edit--preserve-original book)
  (let ((buffer (calibre-edit--create-buffer book)))
    (pop-to-buffer buffer)))

(defun calibre-edit--create-buffer (book)
  "Create a buffer for editing BOOK.
Returns a buffer for editing BOOK, creating it if necessary."
  (let ((buffer (get-buffer-create (format "*%s - Metadata*" (calibre-book-title book)))))
    (with-current-buffer buffer
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)
      (calibre-edit-mode)
      (setf calibre-edit--book book)
      (setq-local
       header-line-format
       (substitute-command-keys
        "\\<calibre-edit-mode-map>Finish `\\[calibre-edit-confirm]', Apply `\\[calibre-edit-apply]', Reset `\\[calibre-edit-reset]', Cancel `\\[calibre-edit-abort]'"))
      (setf calibre-edit--title
            (widget-create 'string
                           :value (calibre-book-title book)
                           :tag "Title"
                           :doc "The title of the book."
                           :format "%t: %v%d"))
      (widget-insert "\n")
      (setf calibre-edit--authors
            (widget-create 'repeat
                           :value (calibre-book-authors book)
                           :tag "Authors"
                           :doc "The authors of the book."
                           :format "%t:\n%v%i\n%d"
                           'calibre-author))
      (widget-insert "\n")
      (setf calibre-edit--publisher
            (widget-create 'choice
                           :value (calibre-book-publisher book)
                           :tag "Publisher"
                           :doc "The publisher of the book."
                           :format "%[%t%]%v%d"
                           '(const :tag ": None" :menu-tag "None" nil)
                           '(calibre-publisher :tag "" :menu-tag "Publisher")))
      (widget-insert "\n")
      (setf calibre-edit--series
            (widget-create 'choice
                           :value (if (calibre-book-series book)
                                      (cons (calibre-book-series book)
                                            (calibre-book-series-index book))
                                    nil)
                           :tag "Series"
                           :format "%[%t%]%v%d"
                           '(const :tag ": None" :menu-tag "None" nil)
                           `(cons :tag ""
                                  :menu-tag "Series"
                                  (calibre-series :tag "Name"
                                                  :doc "The series the book is part of."
                                                  :format "%t:  %v%d")
                                  (number :tag "Index"
                                          :doc "The book's position within its series"
                                          :format "%t: %v%d"
                                          :value ,(calibre-book-series-index book)))))
      (widget-insert "\n")
      (setf calibre-edit--tags
            (widget-create 'repeat
                           :value (calibre-book-tags book)
                           :tag "Tags"
                           :doc "Tags associated with the book."
                           :format "%t:\n%v%i\n%d"
                           'calibre-tag))
      (widget-insert "\n")
      (widget-create 'push-button
                     :tag "Confirm"
                     :help-echo "Apply any changes and exit."
                     :notify (lambda (&rest _)
                               (calibre-edit-confirm)
                               (kill-buffer)))
      (widget-insert "   ")
      (widget-create 'push-button
                     :tag "Apply"
                     :help-echo "Apply any changes."
                     :notify #'calibre-edit-apply)
      (widget-insert "   ")
      (widget-create 'push-button
                     :tag "Reset"
                     :help-echo "Undo any changes."
                     :notify #'calibre-edit-reset)
      (widget-insert "   ")
      (widget-create 'push-button
                     :tag "Cancel"
                     :help-echo "Abort all changes."
                     :notify #'calibre-edit-abort)
      (widget-setup)
      (goto-char (point-min)))
    buffer))

(defun calibre-edit-revert (book)
  "Undo any edits performed to BOOK in this session."
  (let ((compare-ids (lambda (b)
                            (= (calibre-book-id book)
                               (calibre-book-id b)))))
    (setf calibre--books
        (cl-substitute-if (calibre-util-find-book book calibre-edit--edited-books)
                          compare-ids
                          calibre--books))
    (calibre-library--refresh)))

(defun calibre-edit--find-original (book)
  "Return BOOK absent any modifications performed in this session."
  (calibre-util-find-book book calibre-edit--edited-books))

(defun calibre-edit--different-fields (a b)
  "Return a list of slot names whose values differ in A and B."
  (let (diff)
    (dolist (descriptor (cdr (cl-struct-slot-info 'calibre-book)))
      (let* ((slot-name (car descriptor))
             (a (cl-struct-slot-value 'calibre-book slot-name a))
             (b (cl-struct-slot-value 'calibre-book slot-name b)))
        (unless (equal a b)
          (push slot-name diff))))
    diff))

(defun calibre-util-uglify-field-name (field)
  "Return FIELD replacing - with _."
  (string-join (split-string (symbol-name field) "-") "_"))

(defun calibre-edit--command (book)
  "Return the command to commit edits to BOOK to disk."
  `("set_metadata"
    ,@(flatten-list (mapcar (lambda (field)
                              `("-f" ,(format "%s:%s" (calibre-util-uglify-field-name field)
                                             (cl-case field
                                               (authors (string-join (calibre-book-authors book) " & "))
                                               (title (calibre-book-title book))
                                               (publisher (calibre-book-publisher book))
                                               (series (calibre-book-series book))
                                               (series-index (calibre-book-series-index book))
                                               (tags (string-join (calibre-book-tags book) ","))))))
                           (calibre-edit--different-fields book (calibre-edit--find-original book))))
    ,(int-to-string (calibre-book-id book))))

(defun calibre-edit-commit-edits (books)
  "Commit edits to BOOKS to disk."
  (calibre-exec--queue-commands (mapcar #'calibre-edit--command books)))

(provide 'calibre-edit)
;;; calibre-edit.el ends here
