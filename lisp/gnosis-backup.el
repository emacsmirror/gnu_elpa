;;; gnosis-backup.el --- Database and managed media recovery -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Snapshot the connected SQLite database and all managed asset bytes.  Restore
;; only to new directories, never into an active database.  Org vaults are not
;; included.  Hashes prove byte completeness, not authenticity or domain validity.
;; Use a trusted, quiescent local filesystem: repeated checks detect observed
;; drift but do not provide exclusion against concurrent filesystem writers.

;;; Code:

(require 'gnosis-assets)
(require 'cl-lib)
(require 'json)

(defconst gnosis-backup--manifest-limit (* 16 1024 1024)
  "Maximum manifest size in bytes.")

(defconst gnosis-backup--entry-limit 100000
  "Maximum number of directories and files in a snapshot.")

(defun gnosis-backup--path (path)
  "Return absolute local PATH without a trailing slash or symlink ancestry."
  (when (file-remote-p path) (user-error "Backup paths must be local"))
  (let* ((absolute (directory-file-name (expand-file-name path)))
         (ancestor absolute))
    (while (not (equal ancestor (directory-file-name (file-name-directory ancestor))))
      (when (file-symlink-p ancestor)
        (user-error "Backup paths must not contain symlinks: %s" path))
      (setq ancestor (directory-file-name (file-name-directory ancestor))))
    absolute))

(defun gnosis-backup--owner ()
  "Return current connection, main filename and managed root, or nil.
Never initialize a database.  Reject a connected non-file database."
  (when gnosis-db
    (let ((file (nth 2 (seq-find (lambda (row) (equal (nth 1 row) "main"))
                                (sqlite-select gnosis-db "PRAGMA database_list")))))
      (unless (and (stringp file) (file-name-absolute-p file)
                   (not (file-remote-p file)))
        (user-error "Backup requires a connected local file database"))
      (list gnosis-db (file-truename file) (gnosis-assets-root gnosis-db)))))

(defun gnosis-backup--same-owner (owner)
  "Reject a changed connection or main path compared with OWNER."
  (unless (equal owner (gnosis-backup--owner))
    (user-error "Backup database connection changed")))

(defun gnosis-backup--overlap-p (left right)
  "Return non-nil when LEFT and RIGHT coincide or contain one another."
  (let ((left (file-name-as-directory (file-truename left)))
        (right (file-name-as-directory (file-truename right))))
    (or (string-prefix-p left right) (string-prefix-p right left))))

(defun gnosis-backup--destination (destination owner &optional source)
  "Validate new DESTINATION against OWNER and optional SOURCE snapshot.
Require an existing parent; do not create anything during preflight."
  (let ((destination (gnosis-backup--path destination)))
    (when (or (file-exists-p destination) (file-symlink-p destination))
      (user-error "Backup destination already exists"))
    (unless (file-directory-p (file-name-directory destination))
      (user-error "Backup destination parent must already exist"))
    (dolist (protected (append (when owner
                                (cons (nth 2 owner)
                                      (mapcar (lambda (suffix) (concat (nth 1 owner) suffix))
                                              '("" "-wal" "-shm" "-journal"))))
                              (when source (list source))))
      (when (gnosis-backup--overlap-p destination protected)
        (user-error "Backup destination overlaps protected data: %s" protected)))
    destination))

(defun gnosis-backup--inventory (root &optional manifest)
  "Return sorted exact directory/file byte inventory below ROOT.
When MANIFEST is non-nil, omit only the root manifest.json file.
Reject symlinks, nonregular files, unsafe names and excessive depth or count."
  (let ((root (gnosis-backup--path root))
        (pending '("")) (entries nil) (count 0))
    (unless (file-directory-p root) (user-error "Missing backup directory: %s" root))
    (while pending
      (let* ((relative (pop pending))
             (directory (expand-file-name relative root)))
        (gnosis-backup--path directory)
        (dolist (name (directory-files directory nil directory-files-no-dot-files-regexp))
          (gnosis-assets--name name)
          (let* ((path (if (equal relative "") name (concat relative "/" name)))
                 (file (gnosis-backup--path (expand-file-name path root)))
                 (attributes (file-attributes file 'integer)))
            (when (or (> (length path) 1024) (> (length (split-string path "/")) 32)
                      (> (cl-incf count) gnosis-backup--entry-limit))
              (user-error "Backup inventory limit exceeded"))
            (cond
             ((eq (file-attribute-type attributes) t)
              (push (list path "directory") entries)
              (push path pending))
             ((and attributes (file-regular-p file))
              (unless (and manifest (equal path "manifest.json"))
                (push (list path "file" (file-attribute-size attributes)
                            (gnosis-assets-hash file)) entries)))
             (t (user-error "Backup requires regular files: %s" file)))))))
    (sort entries (lambda (left right) (string< (car left) (car right))))))

(defun gnosis-backup--assets-inventory (root)
  "Return complete asset inventory at ROOT, or nil for an absent root."
  (gnosis-backup--path root)
  (when (file-exists-p root) (gnosis-backup--inventory root)))

(defun gnosis-backup--copy (source destination inventory)
  "Copy exact INVENTORY from SOURCE into owned DESTINATION.
Recheck copied bytes; never overwrite an existing path."
  (dolist (entry inventory)
    (let ((from (gnosis-backup--path (expand-file-name (car entry) source)))
          (to (expand-file-name (car entry) destination)))
      (if (equal (cadr entry) "directory")
          (make-directory to)
        (unless (file-regular-p from) (user-error "Backup source file disappeared"))
        (copy-file from to nil)
        (unless (and (= (nth 2 entry) (file-attribute-size (file-attributes to)))
                     (equal (nth 3 entry) (gnosis-assets-hash to)))
          (user-error "Backup bytes changed during copy"))))))

(defun gnosis-backup--write-manifest (root)
  "Write a versioned bounded manifest for the owned stage ROOT."
  (let* ((inventory (gnosis-backup--inventory root))
         (text (json-serialize (vector "gnosis-data" 1
                                       (vconcat (mapcar #'vconcat inventory))))))
    (when (> (string-bytes text) gnosis-backup--manifest-limit)
      (user-error "Backup manifest limit exceeded"))
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file (expand-file-name "manifest.json" root) (insert text)))))

(defun gnosis-backup--read-manifest (root)
  "Read the bounded versioned manifest from ROOT and return its inventory."
  (let ((file (gnosis-assets-file root "manifest.json")))
    (when (> (file-attribute-size (file-attributes file)) gnosis-backup--manifest-limit)
      (user-error "Backup manifest limit exceeded"))
    (let ((data (with-temp-buffer
                  (insert-file-contents file nil 0 (1+ gnosis-backup--manifest-limit))
                  (when (> (buffer-size) gnosis-backup--manifest-limit)
                    (user-error "Backup manifest limit exceeded"))
                  (let ((value (json-parse-buffer :array-type 'list)))
                    (skip-chars-forward " \t\r\n")
                    (unless (eobp) (user-error "Trailing backup manifest data"))
                    value))))
      (unless (and (proper-list-p data) (= (length data) 3)
                   (equal (car data) "gnosis-data") (equal (cadr data) 1)
                   (proper-list-p (nth 2 data))
                   (<= (length (nth 2 data)) gnosis-backup--entry-limit))
        (user-error "Unsupported backup manifest"))
      (nth 2 data))))

(defun gnosis-backup--check-database (file)
  "Check SQLite physical and foreign-key integrity of standalone FILE.
Do not initialize, migrate or claim application schema validation."
  ;; Emacs 29/30 has no read-only sqlite-open.  Inspect a private copy so
  ;; opening a WAL-mode header cannot create companions in the source snapshot.
  (unless (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally file nil 0 16)
            (equal (buffer-string) "SQLite format 3\0"))
    (user-error "Backup database has no SQLite header"))
  (let ((directory (make-temp-file "gnosis-backup-check-" t)))
    (unwind-protect
        (let ((copy (expand-file-name "database.sqlite" directory)))
          (copy-file file copy nil)
          (let ((database (sqlite-open copy)))
            (unwind-protect
                (progn
                  (sqlite-execute database "PRAGMA query_only = ON")
                  (sqlite-execute database "PRAGMA trusted_schema = OFF")
                  (unless (and (equal (sqlite-select database "PRAGMA integrity_check")
                                      '(("ok")))
                               (null (sqlite-select database "PRAGMA foreign_key_check")))
                    (user-error "Backup database integrity check failed")))
              (when database (sqlite-close database)))))
      (delete-directory directory t))))

;;;###autoload
(defun gnosis-backup-verify (source)
  "Verify offline data snapshot SOURCE; return t or signal an error.
Require exact file/directory inventory, byte hashes and SQLite integrity.
Reject extra, missing or unsafe paths.  Do not connect Gnosis, migrate data,
validate media semantics or authenticate the backup.  Org vaults are absent."
  (interactive "DVerify data snapshot: ")
  (let* ((source (gnosis-backup--path source))
         (manifest (gnosis-backup--read-manifest source))
         (inventory (gnosis-backup--inventory source t)))
    (unless (and (equal (directory-files source nil directory-files-no-dot-files-regexp)
                        '("assets" "database.sqlite" "manifest.json"))
                 (file-directory-p (expand-file-name "assets" source))
                 (file-regular-p (expand-file-name "database.sqlite" source))
                 (equal manifest inventory))
      (user-error "Backup inventory or bytes do not match manifest"))
    (gnosis-backup--check-database (expand-file-name "database.sqlite" source))
    (unless (and (equal manifest (gnosis-backup--read-manifest source))
                 (equal inventory (gnosis-backup--inventory source t)))
      (user-error "Backup changed during verification"))
    (when (called-interactively-p 'interactive) (message "Data snapshot verified: %s" source))
    t))

(defun gnosis-backup--publish (destination owner source build check-source)
  "Build, verify and publish new DESTINATION using BUILD.
OWNER pins the current connection; SOURCE is the optional protected snapshot.
Call BUILD with the owned stage, then CHECK-SOURCE with it before publication.
Remove only that unpublished stage on exit."
  (let* ((destination (gnosis-backup--destination destination owner source))
         (stage (make-temp-file (expand-file-name ".gnosis-backup-"
                                                  (file-name-directory destination)) t))
         (identity (file-attributes stage 'integer)))
    (unwind-protect
        (progn
          (funcall build stage)
          (gnosis-backup-verify stage)
          (funcall check-source stage)
          (gnosis-backup--same-owner owner)
          (gnosis-backup--destination destination owner source)
          (rename-file stage destination nil)
          destination)
      (let ((current (file-attributes stage 'integer)))
        (when (and (not (file-symlink-p stage))
                   (eq (file-attribute-type current) t)
                   (equal (file-attribute-inode-number identity)
                          (file-attribute-inode-number current))
                   (equal (file-attribute-device-number identity)
                          (file-attribute-device-number current)))
          (delete-directory stage t))))))

;;;###autoload
(defun gnosis-backup-data (destination)
  "Snapshot the connected database and managed assets into new DESTINATION.
Use SQLite VACUUM INTO, preserving schedules and study evidence.  Include all
managed asset bytes, even unreferenced revisions; reject observed source drift.
Require a new local directory with an existing parent.  Publish atomically;
never overwrite, switch connections or include the separate Org notes vault.
External legacy file links and review-image files are not copied.
Return the absolute destination path."
  (interactive "GNew database and managed media snapshot directory: ")
  (let* ((owner (or (gnosis-backup--owner) (user-error "Connect Gnosis before snapshotting")))
         (root (nth 2 owner)))
    (gnosis-backup--destination destination owner)
    (let ((inventory (gnosis-backup--assets-inventory root)) database-hash)
      (gnosis-backup--publish
       destination owner nil
       (lambda (stage)
         (sqlite-execute (car owner) "VACUUM INTO ?"
                         (list (expand-file-name "database.sqlite" stage)))
         (setq database-hash (gnosis-assets-hash (expand-file-name "database.sqlite" stage)))
         (let ((assets (expand-file-name "assets" stage)))
           (make-directory assets)
           (gnosis-backup--copy root assets inventory))
         (gnosis-backup--write-manifest stage))
       (lambda (stage)
         (unless (and (equal database-hash
                             (gnosis-assets-hash (expand-file-name "database.sqlite" stage)))
                      (equal inventory (gnosis-backup--assets-inventory root))
                      (equal inventory (gnosis-backup--inventory
                                        (expand-file-name "assets" stage))))
           (user-error "Source assets or staged snapshot changed during backup")))))))

;;;###autoload
(defun gnosis-backup-restore (source destination)
  "Restore verified SOURCE into new DESTINATION; return its absolute path.
Validate before copying and recheck copied bytes before atomic publication.
Never overwrite, merge, activate or switch a database.  The restored main
file is database.sqlite beside assets/ and manifest.json.  Use the matching
Gnosis version and restore the separate Org notes vault independently."
  (interactive "DData snapshot to restore: \nGNew restored data directory: ")
  (let* ((source (gnosis-backup--path source))
         (owner (gnosis-backup--owner)))
    (gnosis-backup--destination destination owner source)
    (let ((inventory (gnosis-backup--inventory source)))
      (gnosis-backup-verify source)
      (gnosis-backup--publish
       destination owner source
       (lambda (stage)
         (gnosis-backup--copy source stage inventory))
       (lambda (stage)
         (unless (and (equal inventory (gnosis-backup--inventory source))
                      (equal inventory (gnosis-backup--inventory stage)))
           (user-error "Backup or copied data changed during restore")))))))

(provide 'gnosis-backup)
;;; gnosis-backup.el ends here
