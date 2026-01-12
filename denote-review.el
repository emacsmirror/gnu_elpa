;;; denote-review --- implements review process for denote notes -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Matto Fransen

;; Author:  Matto Fransen <matto@matto.nl>
;; Maintainer:  Matto Fransen <matto@matto.nl>
;; Version: 1.0.0
;; Keywords: files
;; Package-Requires: ((emacs "28.1") (denote "4.0.0"))

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

;; `denote-review' aims to provide a practical and simple manner to
;; implement a review process for your denote notes.

;; It is soleley made for denote notes in org mode format with the
;; default filenaming scheme.

;; `denote-review' adds a single line to the frontmatter, f.e.:
;; #+reviewdate: [2024-06-12]

;; In `tabulated list mode' the notes are shown with their last
;; review date, sorted from oldest to newest review date.
;; Click on a column header to change the order.

;; See the README for full explanation.

;;; Code:

(require 'denote)

(defgroup denote-review nil
  "implements review process for denote notes"
  :prefix "denote-review-"
  :link '(custom-manual "(denote-review) Top")
  :group 'denote)

(defcustom denote-review-max-search-point 300
  "Point to stop `re-search-forward' after some lines."
  :type 'natnum)

(defcustom denote-review-insert-after "identifier"
  "Frontmatter after which to insert review date line."
  :type 'string)

;; Regexps for different filetypes

(defun denote-review-search-regexp-for-filetype ()
  "Regexp to search for the reviewdate.
Defaults to regexp for org filetype."
  (cond ((eq denote-file-type 'markdown-yaml)
         "\\(^reviewdate:[ \t]\\)\\([^\t\n]+\\)")
        ((eq denote-file-type 'markdown-toml)
         "\\(^reviewdate[ \t]\\)= \\([^\t\n]+\\)")
        ((eq denote-file-type 'text)
         "\\(^reviewdate:[ \t]\\)\\([^\t\n]+\\)")
        (t "\\(^#\\+reviewdate:[ \t]\\[\\)\\([^\t\n]+\\)\\]")))

(defun denote-review-insert-regexp-location-for-filetype ()
  "Regexp to search for the identifier string in frontmatter."
  (if (or
       (eq denote-file-type 'markdown-yaml)
       (eq denote-file-type 'markdown-toml)
       (eq denote-file-type 'text))
      (format "^%s" denote-review-insert-after)
    (format "^#\\+%s" denote-review-insert-after)))

;; Setting and getting the reviewdate

(defun denote-review-insert-reviewdate-line (mydate)
  "Insert the review date MYDATE frontmatter line.
Format according to variable `denote-file-type'.
Insert just after the identifier line."
  (cond ((eq denote-file-type 'markdown-yaml)
         (format "reviewdate: %s" mydate))
        ((eq denote-file-type 'markdown-toml)
         (format "reviewdate = %s" mydate))
        ((eq denote-file-type 'text)
         (format "reviewdate: %s" mydate))
        (t (format "#+reviewdate: [%s]" mydate))))

(defun denote-review-insert-date (&optional thisdate insert-regexp)
  "Insert current date in ISO 8601 format as reviewdate.
Or use THISDATE, when not nil.
INSERT-REGEXP is regepx to search for appropriate insert location."
  (goto-char (point-min))
  (let ((mydate (format-time-string "%F")))
    (unless (null thisdate)
      (setq mydate thisdate))
    (re-search-forward insert-regexp nil t)
    (end-of-line)
    (newline)
    (insert (denote-review-insert-reviewdate-line mydate))))

(defun denote-review-set-date ()
"Set the reviewdate in the current buffer.
Replace an existing reviewdate."
(interactive)
(save-excursion
 (goto-char (point-min))
 (if (re-search-forward (denote-review-search-regexp-for-filetype)
                        denote-review-max-search-point t)
     (replace-match
      (denote-review-insert-reviewdate-line (format-time-string "%F")))
   (denote-review-insert-date
    nil
    (denote-review-insert-regexp-location-for-filetype)))))

(defun denote-review-get-date (search-regexp)
  "Get the reviewdate from current buffer.
SEARCH-REGEXP set to match format based on variable `denote-file-type'"
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward search-regexp
                               denote-review-max-search-point t)
        (match-string-no-properties 2))))

;; Bulk operation, to be run from Dired.

(defun denote-review-set-initial-date (thisdate search-regexp insert-regexp)
  "Insert reviewdate with THISDATE.
Only do this when no reviewdate already exist.
SEARCH-REGEXP is regexp to search for existing reviewdate.
INSERT-REGEXP is regepx to search for appropriate insert location.
Both regexp's set to match format based on variable `denote-file-type'"
  (when (null (denote-review-get-date search-regexp))
    (denote-review-insert-date thisdate insert-regexp)))

(defun denote-review-get-date-from-filename (filename)
  "Convert identifier in FILENAME into a date."
  (denote-id-to-date (substring filename 0 15)))

(defun denote-review-bulk-set-date (filename current-date-p)
  "Opens FILENAME and insert a reviewdate.
When CURRENT-DATE-P is not null, use current date."
  (let ((fpath filename)
        (fname (file-name-nondirectory filename))
        (search-regexp (denote-review-search-regexp-for-filetype))
        (insert-regexp (denote-review-insert-regexp-location-for-filetype)))
    (find-file fpath)
    (if (null current-date-p)
        (denote-review-set-initial-date
         (denote-review-get-date-from-filename fname)
         search-regexp insert-regexp)
      (denote-review-set-initial-date (format-time-string "%F")
                                         search-regexp
                                         insert-regexp))
    (save-buffer)
    (kill-buffer fname)))

(defun denote-review-set-date-dired-marked-files ()
  "Insert a reviewdate in the marked files.
Set a reviewdate according the identifier in the filename,
when called with the Universal Argument use current date.
Does not overwrite existing reviewdates."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((marked-files (dired-get-marked-files)))
        (mapcar (lambda (file)
                  (when (denote-file-is-writable-and-supported-p file)
                    (denote-review-bulk-set-date file current-prefix-arg)))
                marked-files))
    (error (format "Command can only be used in a Dired buffer."))))

;; Collect keywords and prompt for a keyword to filter by.

(defun denote-review-get-path ()
  "Prompt for a path when needed."
  (let ((mypath '()))
    (when (boundp 'denote-directory)
      (setq mypath (append mypath denote-directory)))
    (when (boundp 'denote-silo-directories)
      (setq mypath (append mypath denote-silo-directories)))
    (if (listp mypath)
    (completing-read
     "Select a directory (using completion): " mypath)
    denote-directory)))

(defun denote-review-get-keyword-list (denotepath)
  "Fetch keywords from the filenames in directory DENOTEPATH."
  (let ((keyword-list '())
        (denote-directory denotepath))
    (mapc
      (lambda (myfile)
        (dolist (mykeyword
                 (denote-extract-keywords-from-path myfile))
          (add-to-list 'keyword-list mykeyword)))
      (denote-directory-files nil t nil))
    (sort keyword-list)))

(defun denote-review-select-keyword ()
  "Select a keyword or `All' using completion."
  (let ((denotepath (denote-review-get-path)))
    (cons denotepath
          (completing-read
           "Select a keyword (using completion) :"
           (append (list "All")
                   (denote-review-get-keyword-list denotepath))))))

;; Collect data to fill the tabular mode list

(defun denote-review-check-date-of-file (myfile search-regexp)
  "Get the reviewdate of MYFILE.
SEARCH-REGEXP is regexp to search for reviewdate.
It is set to match format based on variable `denote-file-type'"
  (let ((mybuffer (find-file myfile))
        myreviewdate)
    (setq myreviewdate (denote-review-get-date
                        search-regexp))
    (kill-buffer mybuffer)
    myreviewdate))

(defun denote-review-collect-files (denotepath-and-keyword)
  "Fetch reviewdate from the files in DENOTEPATH-AND-KEYWORD.
Filter filenames according to DENOTEPATH-AND-KEYWORD.
DENOTEPATH-AND-KEYWORD is a cons of a path and a keyword.
Create a list in the format required by `tabulated-list-mode'."
  (let ((list-of-files '())
        (search-regexp (denote-review-search-regexp-for-filetype))
        (denote-directory (car denotepath-and-keyword)))
    (save-excursion
      (mapc
       (lambda (myfile)
         (when (or (string= (cdr denotepath-and-keyword) "All")
                   (string-match
                    (format "_%s" (cdr denotepath-and-keyword)) myfile))
           (let ((reviewdate (denote-review-check-date-of-file
                              myfile
                              search-regexp)))
             (unless (null reviewdate)
               (push (list myfile
                           (vector
                            reviewdate
                            (file-name-nondirectory myfile)))
                     list-of-files)))))
       (denote-directory-files nil t nil)))
    (when (null list-of-files)
      (error (format
              "No files with a reviewdate found (filter: keyword %s)"
              (cdr denotepath-and-keyword))))
    list-of-files))

(defun denote-review-collect-files--revert (denotepath-and-keyword)
  "Re-populated `tabulated-list-entries'."
  (let ((list-of-files '())
        (search-regexp (denote-review-search-regexp-for-filetype))
        (denote-directory (car denotepath-and-keyword)))
    (save-excursion
      (mapc
       (lambda (myfile)
         (when (or (string= (cdr denotepath-and-keyword) "All")
                   (string-match
                    (format "_%s" (cdr denotepath-and-keyword)) myfile))
           (let ((reviewdate (denote-review-check-date-of-file
                              myfile
                              search-regexp)))
             (unless (null reviewdate)
               (push (list myfile
                           (vector
                            reviewdate
                            (file-name-nondirectory myfile)))
                     list-of-files)))))
       (denote-directory-files nil t nil)))
    (when (null list-of-files)
      (error (format
              "No files with a reviewdate found (filter: keyword %s)"
              (cdr denotepath-and-keyword))))
    (setq tabulated-list-entries list-of-files)
    (setq tabulated-list-sort-key (cons "Reviewdate" nil))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

;; Mode map for tabulated list and actions.

(defun denote-review-goto-file ()
  "Open the selected file in other window.
Must be called from the tabulated list view."
  (interactive nil denote-review-mode)
  (find-file-other-window (tabulated-list-get-id)))

(defun denote-review-edit-file ()
  "Open the selected file in other window.
Must be called from the tabulated list view."
  (interactive nil denote-review-mode)
  (find-file (tabulated-list-get-id)))

(defun denote-review-read-only-goto-file ()
  "Open the selected file in read-only mode in other window.
Must be called from the tabulated list view"
  (interactive nil denote-review-mode)
  (find-file-read-only-other-window (tabulated-list-get-id)))

(defun denote-review-goto-random-file ()
  "Open a random file in other window.
Must be called from the tabulated list view."
  (interactive nil denote-review-mode)
  (let ((k (random (length tabulated-list-entries))))
    (find-file-other-window (car (nth k tabulated-list-entries)))))

(defvar-keymap denote-review-mode-map
  :doc "Keymap for `denote-review-mode-map'."
  :parent tabulated-list-mode-map
  "RET" #'denote-review-goto-file
  "e" #'denote-review-edit-file
  "o" #'denote-review-read-only-goto-file
  "r" #'denote-review-goto-random-file)

;; Tabulated list.

(define-derived-mode denote-review-mode
  tabulated-list-mode
  "denote-review-mode"
  "Display two-column tabulated list with the reviewdate per file.
Initially sort by reviewdate."
  (setq tabulated-list-format
        [("Reviewdate" 12 t)
         ("Filename" 60 t)])
  (setq tabulated-list-sort-key (cons "Reviewdate" nil))
  (tabulated-list-init-header))

(defun denote-review-display-list (denotepath-and-keyword)
  "Show buffer with reviewdates.
DENOTEPATH-AND-KEYWORD is a cons of a path and a keyword.
 Filter by keyword."
  (interactive (list (denote-review-select-keyword)))
  (with-current-buffer (get-buffer-create "*denote-review-results*")
      (denote-review-mode)
      (setq tabulated-list-entries (denote-review-collect-files
                                    denotepath-and-keyword))
      (add-hook 'tabulated-list-revert-hook
                (lambda ()
                  (denote-review-collect-files--revert denotepath-and-keyword)) 0 t)
      (tabulated-list-print t)
      (display-buffer (current-buffer))
      (setq mode-line-buffer-identification
            (format "*denote-review-results* [%s | %s]"
                    (car denotepath-and-keyword)
                    (cdr denotepath-and-keyword)))
      (force-mode-line-update)))

(provide 'denote-review)
;;; denote-review.el ends here
