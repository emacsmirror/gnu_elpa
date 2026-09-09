;;; gnosis-assets.el --- Immutable managed asset files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Store explicit sets of files beside the connected database.  Domain modules
;; interpret manifests; this module owns byte identity and staged publication.
;; These checks are not cross-process exclusion against hostile filesystem edits.

;;; Code:

(require 'gnosis-db)
(require 'seq)
(require 'subr-x)

(defun gnosis-assets--local-path (path)
  "Return absolute local PATH, rejecting a symlink at PATH itself."
  (when (file-remote-p path) (user-error "Assets must be local"))
  (let ((absolute (expand-file-name path)))
    (when (file-symlink-p (directory-file-name absolute))
      (user-error "Asset path must not be a symlink: %s" path))
    absolute))

(defun gnosis-assets-root (&optional database)
  "Return the asset root beside the connected main database.
If DATABASE is non-nil, require it to remain the current connection.
Use the canonical database parent; reject a nonlocal or symlink asset root.
Do not create directories."
  (let* ((current (gnosis--ensure-db))
         (file (nth 2 (seq-find (lambda (row) (equal (nth 1 row) "main"))
                               (sqlite-select current "PRAGMA database_list")))))
    (when (and database (not (eq database current)))
      (user-error "Asset database changed"))
    (unless (and (stringp file) (file-name-absolute-p file))
      (user-error "Assets require a file-backed database"))
    (gnosis-assets--local-path
     (expand-file-name "assets" (file-truename (file-name-directory file))))))

(defun gnosis-assets--name (name)
  "Validate and return simple relative basename NAME."
  (unless (and (stringp name)
               (string-match-p "\\`[[:alnum:]_-][[:alnum:]_.-]*\\'" name))
    (user-error "Asset files must have simple relative names"))
  (substring-no-properties name))

(defun gnosis-assets-file (directory name)
  "Return readable local regular file NAME confined to DIRECTORY.
Reject non-simple basenames and symlinks at DIRECTORY or the file."
  (let ((file (gnosis-assets--local-path
               (expand-file-name (gnosis-assets--name name)
                                 (gnosis-assets--local-path directory)))))
    (unless (and (file-regular-p file) (file-readable-p file))
      (user-error "Asset file unavailable: %s" name))
    file))

(defun gnosis-assets-hash (file)
  "Return the SHA256 of literal bytes in local regular FILE.
Reject unreadable files and symlinks at FILE or its immediate directory."
  (let ((file (gnosis-assets-file (file-name-directory (expand-file-name file))
                                 (file-name-nondirectory file))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (secure-hash 'sha256 (current-buffer)))))

(defun gnosis-assets-revision (directory names)
  "Return the content digest for explicit basenames NAMES in DIRECTORY.
Hash the owned Lisp serialization of sorted unique (NAME SHA256) lists.
Do not mutate NAMES or include unlisted directory entries."
  (unless (and (proper-list-p names) names)
    (user-error "Asset revision requires filenames"))
  (let ((names (sort (delete-dups (mapcar #'gnosis-assets--name names)) #'string<)))
    (secure-hash
     'sha256 (gnosis-sqlite--serialize
              (mapcar (lambda (name)
                        (list name (gnosis-assets-hash
                                    (gnosis-assets-file directory name)))) names)))))

(defun gnosis-assets-validate (root revision names)
  "Return directory under ROOT matching REVISION for explicit NAMES.
Require REVISION to be a lowercase SHA256 and verify all named bytes.
Reject symlinks; do not inspect unrelated directory entries."
  (unless (and (stringp revision)
               (string-match-p "\\`[0-9a-f]\\{64\\}\\'" revision))
    (user-error "Invalid asset revision"))
  (let ((directory (gnosis-assets--local-path
                    (expand-file-name revision (gnosis-assets--local-path root)))))
    (unless (equal revision (gnosis-assets-revision directory names))
      (user-error "Existing asset revision is corrupt"))
    directory))

(defun gnosis-assets-import (directory names &optional generated)
  "Publish copied NAMES from DIRECTORY and return their managed revision.
GENERATED is an alist of (BASENAME . TEXT) to write as UTF-8 Unix files;
its names must not overlap copied NAMES.  Callers own manifest semantics.
Verify copied bytes and exact retry content.  Reject a changed database.
On error or quit remove only the privately owned unpublished stage."
  (let* ((database (gnosis--ensure-db))
         (root (gnosis-assets-root database))
         (names (delete-dups (mapcar #'gnosis-assets--name names)))
         (generated-names (mapcar (lambda (entry) (gnosis-assets--name (car entry)))
                                  generated))
         (all-names (append names generated-names)))
    (unless (and (= (length all-names) (length (delete-dups (copy-sequence all-names))))
                 (seq-every-p (lambda (entry) (stringp (cdr entry))) generated))
      (user-error "Invalid or overlapping generated asset files"))
    (make-directory root t)
    (let* ((stage (make-temp-file (expand-file-name ".import-" root) t))
           (identity (file-attribute-inode-number (file-attributes stage))))
      (unwind-protect
          (progn
            (dolist (name names)
              (let* ((source (gnosis-assets-file directory name))
                     (hash (gnosis-assets-hash source))
                     (destination (expand-file-name name stage)))
                (copy-file source destination)
                (unless (equal hash (gnosis-assets-hash destination))
                  (user-error "Asset changed during import"))))
            (dolist (entry generated)
              (let ((coding-system-for-write 'utf-8-unix))
                (with-temp-file (expand-file-name (car entry) stage)
                  (insert (cdr entry)))))
            (let* ((revision (gnosis-assets-revision stage all-names))
                   (destination (expand-file-name revision root)))
              (unless (equal root (gnosis-assets-root database))
                (user-error "Asset database path changed"))
              (if (or (file-exists-p destination) (file-symlink-p destination))
                  (gnosis-assets-validate root revision all-names)
                (rename-file stage destination))
              revision))
        (when (and (not (file-symlink-p stage))
                   (equal identity (file-attribute-inode-number (file-attributes stage))))
          (delete-directory stage t))))))

(provide 'gnosis-assets)
;;; gnosis-assets.el ends here
