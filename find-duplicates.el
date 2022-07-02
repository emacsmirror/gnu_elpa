;;; find-duplicates.el --- Find duplicate files locally and remotely  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Harald Judt

;; Author: Harald Judt <h.judt@gmx.at>
;; Maintainer: Harald Judt <h.judt@gmx.at>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: files
;; Homepage: https://codeberg.org/hjudt/find-duplicates

;; This file is not part of GNU Emacs.

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

;; This package helps to find duplicate files on local and remote filesystems.
;; It is similar to the fdupes command-line utility but written in Emacs Lisp
;; and should also work on every remote filesystem that TRAMP supports and
;; where executable commands can be called remotely.  The only external
;; requirement is a checksum program like md5 or sha256sum that generates a
;; hash value from the contents of a file used for comparison, because Emacs
;; cannot do that in a performance-efficient way.
;;
;; find-duplicates works by first searching files of the same size, then
;; invoking the calculation of the checksum for these files, and presents the
;; grouped results in a Dired buffer that the user can work with similarly to
;; a regular Dired buffer.

;;; Code:

(require 'cl-lib)
(require 'dired)

(defgroup find-duplicates
  nil
  "Find duplicate files on local and/or remote filesystems."
  :tag "Find Duplicate Files"
  :group 'dired)

(defcustom find-duplicates-use-separators
  t
  "Whether to use a separator dummy file for separating search results."
  :group 'find-duplicates
  :tag "Separate search results"
  :type 'boolean)

(defcustom find-duplicates-separator-file
  (concat "/tmp/-" (make-string 40 ?-))
  "Path and name of the separator file used for making search
results easier to discern.  It will be created immediately before
and deleted as soon as possible after the search operation
finishes."
  :group 'find-duplicates
  :tag "Separator dummy file"
  :type 'string)

(defcustom find-duplicates-checksum-exec
  "sha256sum"
  "Name of the executable used for creating file checksums for
comparison."
  :group 'find-duplicates
  :tag "Checksum executable"
  :type 'string)

(defcustom find-duplicates-size-comparison-function
  '<
  "The comparison function used for sorting grouped results in
ascending or descending order."
  :group 'find-duplicates
  :tag "Ascending or descending file size sort order"
  :type '(choice (const :tag "Ascending" :value <)
                 (const :tag "Descending" :value >)))

(defcustom find-duplicates-file-filter-functions
  nil
  "Filter functions applied to all files found in a directory.  A
filter function must accept as its single argument the file and
return boolean t if the file matches a criteria, otherwise nil."
  :group 'find-duplicates
  :tag "File filter functions"
  :type 'hook)

(defcustom find-duplicates-search-directories-recursively
  t
  "Search directories recursively."
  :group 'find-duplicates
  :tag "Search directories recursively"
  :type 'boolean)

(defvar find-duplicates-directories nil
  "List of directories that will be searched for duplicate files.")

(defun find-duplicates-checksum-file (file)
  "Create a checksum for FILE, using executable defined by
`find-duplicates-checksum-exec'."
  (let* ((default-directory (file-name-directory (expand-file-name file)))
         (exec (executable-find find-duplicates-checksum-exec t)))
    (unless exec
      (error "Checksum program %s not found in exec-path!" exec))
    (car (split-string
          (shell-command-to-string
           (concat exec " \"" (expand-file-name (file-local-name file)) "\""))
          nil
          t))))

(defun find-duplicates--ensure-separator-file ()
  "Ensure that the separator file specified by
`find-duplicates-separator-file' exists."
  (unless (file-exists-p find-duplicates-separator-file)
    (make-empty-file find-duplicates-separator-file)))

(defun find-duplicates--remove-separator-file ()
  "Remove the separator file specified by `find-duplicates-separator-file'."
  (when (file-exists-p find-duplicates-separator-file)
    (delete-file find-duplicates-separator-file nil)))

(defmacro find-duplicates-with-separator-file (&rest body)
  "Ensure separator file gets created and cleaned up before and after BODY."
  `(unwind-protect
       (progn
         (when find-duplicates-use-separators
           (find-duplicates--ensure-separator-file))
         ,@body)
     (when find-duplicates-use-separators
       (find-duplicates--remove-separator-file))))

(defun find-duplicates--apply-file-filter-functions (files)
  "Apply file filter functions to FILES, returning the resulting list."
  (if (and find-duplicates-file-filter-functions files)
      (dolist (filter-func find-duplicates-file-filter-functions files)
        (setf files (cl-delete-if-not filter-func files)))
    files))

(defun find-duplicates--find-and-filter-files (directories)
  "Given one or more root DIRECTORIES, search below the directories
for duplicate files.  Returns a hash-table with the checksums as
keys and a list of size and duplicate files as values."
  (cl-loop with files = (find-duplicates--apply-file-filter-functions
                         (mapcan
                          #'(lambda (d)
                              (if find-duplicates-search-directories-recursively
                                  (directory-files-recursively d ".*")
                                (cl-remove-if #'file-directory-p (directory-files d t nil t))))
                          (ensure-list directories)))
           and same-size-table = (make-hash-table)
           and checksum-table = (make-hash-table :test 'equal)
           for f in files
           for size = (file-attribute-size (file-attributes f))
           do (setf (gethash size same-size-table)
                    (append (gethash size same-size-table) (list f)))
           finally
           (cl-loop for same-size-files being the hash-values in same-size-table
                    if (> (length same-size-files) 1) do
                    (cl-loop for f in same-size-files
                             for checksum = (find-duplicates-checksum-file f)
                             do (setf (gethash checksum checksum-table)
                                      (append (gethash checksum checksum-table) (list f)))))
           (cl-loop for same-files being the hash-value in checksum-table using (hash-key checksum)
                    do
                    (if (> (length same-files) 1)
                        (setf (gethash checksum checksum-table)
                              (cons (file-attribute-size (file-attributes (car same-files)))
                                    (sort same-files #'string<)))
                      (remhash checksum checksum-table)))
           (cl-return checksum-table)))

(defun find-duplicates--generate-dired-list (&optional directories)
  "Generate a list of grouped duplicate files in DIRECTORIES,
optionally separated by a separator file specified by
`find-duplicates-separator-file'."
  (cl-loop with dupes-table = (find-duplicates--find-and-filter-files
                               (or directories
                                   find-duplicates-directories))
           with sorted-sums = (cl-sort
                               (cl-loop for k being the hash-key in dupes-table using (hash-value v)
                                        collect (list k (car v)))
                               find-duplicates-size-comparison-function
                               :key #'cl-second)
           for (checksum) in sorted-sums
           append (cdr (gethash checksum dupes-table))
           when find-duplicates-use-separators append (list find-duplicates-separator-file)))

(defun find-duplicates-dired-revert (&optional arg noconfirm)
  "Revert function used instead of `dired-revert' for Dired buffers
generated by `find-duplicates-dired'.

The args ARG and NOCONFIRM are passed through from
`revert-buffer' to `dired-revert'."
  (message "Looking for remaining duplicate files...")
  (setq-local dired-directory
              (append (list (car dired-directory))
                      (find-duplicates--generate-dired-list)))
  (message "Reverting buffer complete.")
  (find-duplicates-with-separator-file
   (dired-revert arg noconfirm)))

;;;###autoload
(defun find-duplicates-dired (directories)
  "Find a list of duplicate files inside one or more DIRECTORIES
and show them in a `dired' buffer."
  (interactive (list (completing-read-multiple "Directories: "
                                               #'read-file-name-internal
                                               #'file-directory-p
                                               t
                                               default-directory
                                               nil
                                               default-directory)))
  (let ((default-directory "/")
        (truncated-dirs (truncate-string-to-width (string-join directories ", ") 40 0 nil t)))
    (message "Finding duplicate files in %s..." truncated-dirs)
    (if-let ((results (find-duplicates--generate-dired-list directories)))
        (progn
          (message "Finding duplicate files in %s completed." truncated-dirs)
          (find-duplicates-with-separator-file
           (dired (cons "/" results))
           (setq-local find-duplicates-directories directories)
           (setq-local revert-buffer-function 'find-duplicates-dired-revert)))
      (message "No duplicate files found in %s." truncated-dirs))))

(provide 'find-duplicates)

;;; find-duplicates.el ends here
