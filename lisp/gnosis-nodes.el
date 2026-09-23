;;; gnosis-nodes.el --- Node management  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions

;;; Commentary:

;; Node CRUD, sync, file management, and interactive commands.
;; Uses gnosis-sqlite for DB and gnosis-org for parsing.
;; All node tables live in the unified gnosis database.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'gnosis-org)
(require 'gnosis-db)
(require 'gnosis-sqlite)
(declare-function gnosis-journal--dir "gnosis-journal")
(declare-function gnosis-journal--configured-file "gnosis-journal")
(declare-function gnosis-journal--file-p "gnosis-journal")
(declare-function gnosis-journal--goto-live-id "gnosis-journal")
(defvar gnosis-journal-file)
(defvar gnosis-journal-templates)

(defun gnosis-nodes--journal-dir ()
  "Return the journal directory, loading its owning module on demand."
  (require 'gnosis-journal)
  (gnosis-journal--dir))

(defgroup gnosis-nodes nil
  "Gnosis node management."
  :group 'gnosis)

(defcustom gnosis-nodes-dir (expand-file-name "Notes" "~")
  "Directory with gnosis nodes, including files in subdirectories."
  :type 'directory)

(defcustom gnosis-nodes-show-tags nil
  "Display tags with `gnosis-nodes-find'."
  :type 'boolean)

(defcustom gnosis-nodes-timestring "%Y%m%d%H%M%S"
  "Timestring used for the creation of node files.
When nil, filenames use just the title (e.g. \"my_title.org\")."
  :type '(choice (string :tag "Time format string")
		 (const :tag "Title only (no prefix)" nil)))

(defcustom gnosis-nodes-create-as-gpg nil
  "When non-nil, create all node files with a .gpg suffix."
  :type 'boolean)

(defcustom gnosis-nodes-templates
  (list (cons "Empty" (lambda () ""))
        (cons "Annotated"
              (lambda ()
                (concat "{*} Summary\n\n"
                        "{*} Notes\n\n"
                        "{*} References\n")))
        (cons "Reading"
              (lambda ()
                (let ((author (read-string "Author: ")))
                  (concat "{*} Key Ideas\n\n"
                          "{*} Quotes\n\n"
                          "{*} Notes\n"
                          (unless (string-empty-p author)
                            (format "\nAuthor: %s\n" author)))))))
  "Templates for nodes.
Template functions return strings.  Use \"{*}\" as a heading
placeholder; it will be expanded to org heading stars relative to
the insertion context.  \"{**}\" adds one extra level, \"{***}\"
adds two, etc."
  :type '(alist :key-type (string :tag "Name")
                :value-type (function :tag "Template Function")))

(defcustom gnosis-nodes-completing-read-func #'org-completing-read
  "Function to use for `completing-read' in node operations."
  :type 'function)

(defface gnosis-nodes-face-tags
  '((t :inherit font-lock-type-face))
  "Face for displaying tags with `gnosis-nodes-find'.")

;;; DB access wrappers

(defun gnosis-nodes-select (value table &optional restrictions flatten)
  "Select VALUE from TABLE, optionally with RESTRICTIONS.
Optional argument FLATTEN, when non-nil, flattens the result.
Delegates to `gnosis-select'."
  (gnosis-select value table restrictions flatten))

(defun gnosis-nodes--insert-into (table values &optional or-ignore)
  "Insert VALUES to TABLE.
Delegates to `gnosis--insert-into' (unified DB).
When OR-IGNORE, skip rows that violate UNIQUE constraints."
  (gnosis--insert-into table values or-ignore))

(defun gnosis-nodes--delete (table value)
  "From TABLE use where to delete VALUE.
Delegates to `gnosis--delete' (unified DB)."
  (gnosis--delete table value))

(defun gnosis-nodes--all-tags ()
  "Return all unique node tags from the junction table."
  (mapcar #'car
	  (gnosis-sqlite-select (gnosis--ensure-db)
				"SELECT DISTINCT tag FROM node_tag")))

;;; Ensure directories

(defun gnosis-nodes-ensure-directories ()
  "Create node directories if they do not exist."
  (unless (file-directory-p gnosis-nodes-dir)
    (make-directory gnosis-nodes-dir t)))

;;; File operations

(defun gnosis-nodes-search-content (query &optional node-ids)
  "Search node files for QUERY and return enclosing node IDs.
When NODE-IDS is non-nil, return only IDs in that list."
  (unless (and (stringp query) (not (string-empty-p query)))
    (error "Search query must be a non-empty string"))
  (let (matches)
    (dolist (file (gnosis-nodes--files))
      (when (file-regular-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (dolist (id (gnosis-org-matching-node-ids query node-ids))
            (unless (member id matches)
              (push id matches))))))
    (nreverse matches)))

(defun gnosis-nodes--insert-file-data (table filename mtime info)
  "Insert parsed INFO for FILENAME with MTIME into TABLE and its indexes."
  (let ((hash (car (last info))))
    (dolist (item (butlast info 2))
      (when-let* ((id (plist-get item :id)))
        (gnosis-nodes--insert-into
         table `([,id ,filename ,(plist-get item :title)
                      ,(plist-get item :level)
                      ;; Preserve the nested Lisp-string tag representation.
                      ,(gnosis-sqlite--serialize (plist-get item :tags))
                      ,mtime ,hash]))
        ;; The junction table references nodes, not journal entries.
        (when (eq table 'nodes)
          (dolist (tag (plist-get item :tags))
            (gnosis-nodes--insert-into 'node-tag `([,id ,tag]) t)))
        (when (and (eq table 'nodes) (stringp (plist-get item :master)))
          (gnosis-nodes--insert-into
           'node-links `([,id ,(plist-get item :master)]) t))))
    (when (eq table 'nodes)
      (dolist (link (car (last (butlast info))))
        (gnosis-nodes--insert-into
         'node-links `[,(cdr link) ,(car link)] t)))))

(defun gnosis-nodes--file-info (file &optional buffer)
  "Parse FILE, or the widened contents of BUFFER when non-nil."
  (if buffer
      (with-temp-buffer
        (insert (with-current-buffer buffer
                  (save-restriction (widen) (buffer-string))))
        (gnosis-org-get-buffer-info))
    (gnosis-org-get-file-info file)))

(defvar-local gnosis-nodes--deleted-file nil
  "File and database of a deleted node retained in this recovery buffer.
The value is (FILE DATABASE), optionally followed by journal index
filenames and their rows before deletion.  The buffer no longer visits
FILE, so an ordinary save cannot silently recreate the deleted file.")

(defun gnosis-nodes--file-buffer (file)
  "Return FILE's visiting buffer or its detached deletion recovery buffer."
  (or (get-file-buffer file)
      (seq-find (lambda (buffer)
                  (with-current-buffer buffer
                    (and (not buffer-file-name)
                         (equal (seq-take gnosis-nodes--deleted-file 2)
                                (list (expand-file-name file) gnosis-db)))))
                (buffer-list))))

(defun gnosis-nodes--legacy-journal-ids (file ids)
  "Return legacy node IDs owned by journal FILE containing current IDS.
Legacy rows store only a basename and the hash of their whole file.
A surviving ID identifies that old snapshot, including removed headings.
Keep snapshots identified by the ordinary same-basename file.  If any
snapshot has no unambiguous owner, require an index-only full rebuild."
  (let* ((basename (file-name-nondirectory file))
         (rows (gnosis-nodes-select '[id hash] 'nodes `(= file ,basename)))
         (other-file (expand-file-name basename gnosis-nodes-dir)))
    (when rows
      (let* ((other-info (when (and (or (gnosis-nodes--file-buffer other-file)
                                         (file-exists-p other-file))
                                    (not (equal (expand-file-name file) other-file))
                                    (not (file-equal-p file other-file)))
                           (gnosis-nodes--file-info
                            other-file (gnosis-nodes--file-buffer other-file))))
             (other-ids (mapcar (lambda (item) (plist-get item :id))
                                (butlast other-info 2)))
             (hashes (delete-dups
                      (cl-loop for (id hash) in rows
                               when (member id ids) collect hash)))
             (other-hashes (delete-dups
                            (cl-loop for (id hash) in rows
                                     when (member id other-ids) collect hash))))
        (dolist (row rows)
          (let ((hash (cadr row)))
            (unless (and (stringp hash) (not (string-empty-p hash))
                         (if (member hash hashes)
                             (not (member hash other-hashes))
                           (member hash other-hashes)))
              (user-error "Ambiguous legacy journal index; run gnosis-nodes-db-force-sync"))))
        (cl-loop for (id hash) in rows
                 when (member hash hashes) collect id)))))

(defun gnosis-nodes--check-node-ownership (file &optional excluded cleanup)
  "Require unambiguous ordinary basename rows before acting on FILE.
EXCLUDED contains IDs already assigned to the configured journal.
Old descendant saves used basenames: a surviving root ID identifies its
whole snapshot, including removed headings.  Unidentified snapshots need
an explicit index-only rebuild, not a guess based on the requested path.
When CLEANUP is non-nil, permit missing-root index-only cleanup if no
same-basename source survives.  Never use a successor buffer as evidence."
  (let* ((basename (file-name-nondirectory file))
         (rows (seq-remove
                (lambda (row) (member (car row) excluded))
                (gnosis-nodes-select '[id hash] 'nodes `(= file ,basename)))))
    (when rows
      (let* ((root (expand-file-name basename gnosis-nodes-dir))
             (buffer (gnosis-nodes--file-buffer root))
             (info (when (or buffer (file-exists-p root))
                     (gnosis-nodes--file-info root buffer)))
             (ids (mapcar (lambda (item) (plist-get item :id))
                          (butlast info 2)))
             (hashes (cl-loop for (id hash) in rows
                              when (and (member id ids)
                                        (stringp hash)
                                        (not (string-empty-p hash)))
                              collect hash)))
        (unless (or (seq-every-p
                     (lambda (row)
                       (or (member (car row) ids)
                           (member (cadr row) hashes)))
                     rows)
                    (and cleanup (not info)
                         (equal (expand-file-name file) root)
                         (not (seq-some
                               (lambda (other)
                                 (equal (file-name-nondirectory other) basename))
                               (gnosis-nodes--files)))))
          (user-error "Unresolved basename node index; run gnosis-nodes-db-force-sync (index-only rebuild)"))))))

(defun gnosis-nodes--journal-ownership (file &optional info journal)
  "Resolve ordinary rows belonging to the journal affected by FILE.
Return (JOURNAL-FILE OWNED-IDS CURRENT-IDS), or nil if no related ordinary
rows exist.  INFO is FILE's parsed replacement, when available.
An explicit relative path owns its whole snapshot even after IDs change
or the file disappears.  Basename rows still need legacy owner evidence.
Use this same resolution before physical deletion and index cleanup.
JOURNAL retains FILE's journal classification across physical deletion."
  (let* ((journal-p (or journal (gnosis-nodes--journal-file-p file)))
         (journal-file
          (if journal-p file
            (when-let* ((single (progn
                                  (require 'gnosis-journal)
                                  (gnosis-journal--configured-file))))
              (when (equal (file-name-nondirectory file)
                           (file-name-nondirectory single))
                single)))))
    (when journal-file
      (let* ((basename (file-name-nondirectory journal-file))
             (key (gnosis-nodes--file-key journal-file nil))
             (explicit (unless (equal key basename)
                         (gnosis-nodes-select 'id 'nodes `(= file ,key) t))))
        (when (or explicit (gnosis-nodes-select 'id 'nodes `(= file ,basename)))
          (let* ((journal-info
                  (or (and journal-p info)
                      (when (or (gnosis-nodes--file-buffer journal-file)
                                (file-exists-p journal-file))
                        (gnosis-nodes--file-info
                         journal-file (gnosis-nodes--file-buffer journal-file)))))
                 (ids (mapcar (lambda (item) (plist-get item :id))
                              (butlast journal-info 2)))
                 (owned (append explicit
                                (gnosis-nodes--legacy-journal-ids journal-file ids))))
            (list journal-file owned ids)))))))

(cl-defun gnosis-nodes--node-file-owner
    (file &optional (owners (gnosis-sqlite-select
                            (gnosis--ensure-db)
                            "SELECT DISTINCT file FROM nodes ORDER BY file")))
  "Return the retained ordinary index pathname physically identical to FILE.
OWNERS contains index filename rows, defaulting to the current node index.
Without an indexed physical owner, return FILE unchanged.  IDs alone never
establish physical ownership."
  (or (seq-find
       (lambda (owner) (file-equal-p file owner))
       (mapcar (lambda (row) (expand-file-name (car row) gnosis-nodes-dir)) owners))
      file))

(defun gnosis-nodes--update-file (file &optional journal buffer)
  "Replace the index of FILE atomically, preserving incoming links.
If JOURNAL is non-nil, index journal entries instead of regular nodes.
When BUFFER is non-nil, parse its widened contents instead of reading FILE.
Signal parsing or storage errors without changing the previous index."
  (let* ((info (gnosis-nodes--file-info file buffer))
         (full-path (let ((path (expand-file-name
                                 file (if journal (gnosis-nodes--journal-dir)
                                        gnosis-nodes-dir))))
                      (if journal path (gnosis-nodes--node-file-owner path))))
         (mtime (format-time-string
                 "%s" (file-attribute-modification-time
                       (file-attributes full-path)))))
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (gnosis-nodes--delete-file full-path t info)
      (gnosis-nodes--insert-file-data
       (if journal 'journal 'nodes) (gnosis-nodes--file-key full-path journal)
       mtime info))))

(defun gnosis-nodes--journal-indexed-file-p (file)
  "Return non-nil for journal FILE or its missing retained index path.
An exact index filename remains evidence after unlinking an external alias;
do not infer ownership from an ID shared with another physical file."
  (or (gnosis-nodes--journal-file-p file)
      (and file (not (file-exists-p file))
           (gnosis-nodes-select 'id 'journal
                                `(= file ,(gnosis-nodes--file-key file t))))))

(defun gnosis-nodes--journal-index-files (file)
  "Return index filenames owned by journal FILE, including physical aliases.
Keep FILE's own key even without rows, for cleanup after physical deletion.
Only filesystem identity, never shared IDs or hashes, establishes an alias."
  (let ((key (gnosis-nodes--file-key file t))
        (directory (gnosis-nodes--journal-dir)))
    (cons key
          (seq-filter
           (lambda (other)
             (and (not (equal key other))
                  (file-equal-p file (expand-file-name other directory))))
           (mapcar #'car (gnosis-sqlite-select
                          (gnosis--ensure-db) "SELECT DISTINCT file FROM journal"))))))

(defun gnosis-nodes--delete-file (&optional file preserve-incoming info journal-owner)
  "Delete contents for FILE in database.
Removes node rows, associated links, and tags.
When PRESERVE-INCOMING is non-nil, retain links from other files.
INFO, when non-nil, is FILE's parsed replacement for journal adoption.
JOURNAL-OWNER is (FILENAMES ROWS), captured before physical deletion.
Reconcile journal ownership and check ordinary basename ownership before
erasing evidence; refuse unresolved ownership without changing the index."
  (let* ((file (or file (buffer-file-name)))
         (journal-p (or journal-owner (gnosis-nodes--journal-indexed-file-p file))))
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (let* ((ownership (gnosis-nodes--journal-ownership file info journal-p))
             ;; Ownership parsing can run Org hooks.  Resolve aliases afterward.
             (filenames (if journal-p
                            (or (car journal-owner) (gnosis-nodes--journal-index-files file))
                          (list (gnosis-nodes--file-key file nil)))))
        (when (and journal-owner
                   (not (equal (cadr journal-owner)
                               (gnosis-nodes-select '* 'journal
                                                    `(in file ,(vconcat filenames))))))
          (user-error "Journal index changed since deletion; sync surviving files"))
        (pcase ownership
          (`(,journal-file ,owned ,ids)
           (if journal-p
               (dolist (id owned)
                 ;; Tags and outgoing links cascade; only survivors keep backlinks.
                 (gnosis-nodes--delete 'nodes `(= id ,id))
                 (unless (and preserve-incoming (member id ids))
                   (gnosis-nodes--delete 'node-links `(= dest ,id))))
             (when owned
               ;; Use the index operation, never the journal's TODO save hook.
               (gnosis-nodes--update-file
                journal-file t (gnosis-nodes--file-buffer journal-file))))))
        ;; Resolve rows only after adoption, so an ordinary namesake cannot
        ;; delete the journal rows or their surviving incoming links.
        (unless journal-p
          (gnosis-nodes--check-node-ownership file nil (not info)))
        (dolist (node (gnosis-nodes-select
                      'id (if journal-p 'journal 'nodes)
                      `(in file ,(vconcat filenames)) t))
          (gnosis-nodes--delete (if journal-p 'journal 'nodes) `(= id ,node))
          (gnosis-nodes--delete 'node-tag `(= node-id ,node))
          (gnosis-nodes--delete 'node-links `(= source ,node))
          (unless preserve-incoming
            (gnosis-nodes--delete 'node-links `(= dest ,node))))))))

(defun gnosis-nodes-update-file (&optional file index-only)
  "Update contents of FILE in database.
Removes all contents of FILE in database, adding them anew.
When FILE is the current buffer's file, parses the buffer directly
instead of re-reading from disk (avoids re-decrypting .gpg files).
INDEX-ONLY is accepted for compatibility and ignored; journal
saves never complete external tasks.
Unresolved basename ownership requires `gnosis-nodes-db-force-sync'."
  (ignore index-only)
  (let* ((file (or file (buffer-file-name)))
	 (journal-p (gnosis-nodes--journal-file-p file))
	 (buf (and file (find-buffer-visiting file))))
    (gnosis-nodes--update-file file journal-p buf)))

(defun gnosis-nodes--deletion-state (file)
  "Return FILE's identity and raw byte digest for deletion confirmation.
Exclude access time, which reading FILE can change.  Retain symlink identity
as well as target bytes.  A missing file returns nil."
  (when-let* ((attributes (file-attributes file 'integer)))
    (setf (nth 4 attributes) nil)
    (list attributes
          (when (file-regular-p file)
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally file)
              (secure-hash 'sha256 (current-buffer)))))))

(defun gnosis-nodes--check-delete-ownership (file)
  "Validate retained index ownership before physically deleting FILE."
  (let* ((journal (gnosis-nodes--journal-indexed-file-p file))
         (ownership (gnosis-nodes--journal-ownership file nil journal)))
    (unless journal
      (gnosis-nodes--check-node-ownership file (cadr ownership) t))))

;;;###autoload
(defun gnosis-nodes-delete-file (&optional file)
  "Confirm and delete FILE and its node index, then close its buffer.
Default FILE to the current buffer's file.  Explicit FILE need not be
visited or current.  Other files and their buffers are not deleted.
If Emacs visits a journal through another name, delete from that buffer
instead; unlinking its alias would lose ownership needed for cleanup retry.
Filesystem deletion precedes the index transaction: these are not atomic.
A file error or quit before deletion leaves the index intact.  If the file
is gone but index cleanup fails, retain its buffer and report reconciliation
instructions.  Detach retained buffers from FILE so saving cannot silently
recreate it.  Calling again from that buffer or for the missing FILE
confirms index-only cleanup; a replacement file needs new confirmation.
Unresolved basename ownership refuses before physical deletion and
requires `gnosis-nodes-db-force-sync'."
  (interactive)
  (let* ((recovery (and (not file) (not (buffer-file-name))
                        gnosis-nodes--deleted-file))
         (target (or file (buffer-file-name) (car recovery)))
         (file (and target (expand-file-name target)))
         (recovery
          (or recovery
              (when-let* ((buffer (and file (gnosis-nodes--file-buffer file))))
                (with-current-buffer buffer
                  (and (not buffer-file-name) gnosis-nodes--deleted-file))))))
    (unless (and file
                 (or (file-in-directory-p file gnosis-nodes-dir)
                     (and recovery (equal file (car recovery)))
                     (gnosis-nodes--journal-indexed-file-p file)))
      (user-error "%s is not a gnosis node file" target))
    (when (file-directory-p file)
      (user-error "%s is a directory, not a node file" file))
    (let ((gnosis-db (gnosis--ensure-db))
          (exists (or (file-exists-p file) (file-symlink-p file)))
          (confirmed-state (gnosis-nodes--deletion-state file)))
      (when (and recovery (not (eq (cadr recovery) gnosis-db)))
        (user-error "Deletion recovery belongs to another database"))
      (when (memq gnosis-db gnosis-sqlite--transaction-dbs)
        (user-error "Cannot delete a node file inside a database transaction"))
      (when (y-or-n-p (if exists (format "Delete file: %s? " file)
                       (format "File missing; reconcile index for %s? " file)))
        (unless (equal confirmed-state (gnosis-nodes--deletion-state file))
          (user-error "File changed; retry deletion of %s" file))
        ;; Retain source evidence for legacy ownership and recovery until the
        ;; index commits, including when FILE was not previously visited.
        (let* ((buffer (or (gnosis-nodes--file-buffer file)
                           (and exists (find-file-noselect file))))
               ;; Resolve aliases after file/Org callbacks, before unlinking
               ;; destroys physical identity.
               (journal-files
                (progn
                  (gnosis-nodes--check-delete-ownership file)
                  (when (and (gnosis-nodes--journal-indexed-file-p file)
                             buffer (buffer-file-name buffer)
                             (not (equal file (buffer-file-name buffer))))
                    (user-error "Journal is visited as %s; delete from that buffer"
                                (buffer-file-name buffer)))
                  (or (when (and (not exists) buffer)
                        (with-current-buffer buffer
                          (nth 2 gnosis-nodes--deleted-file)))
                      (when (gnosis-nodes--journal-indexed-file-p file)
                        (gnosis-nodes--journal-index-files file)))))
               (journal-rows
                (and journal-files
                     (gnosis-nodes-select '* 'journal
                                          `(in file ,(vconcat journal-files))))))
          (when (and (not exists) buffer)
            (with-current-buffer buffer
              (when (and (nth 2 gnosis-nodes--deleted-file)
                         (not (equal journal-rows (nth 3 gnosis-nodes--deleted-file))))
                (user-error "Journal index changed since deletion; sync surviving files"))))
          (unless (equal confirmed-state (gnosis-nodes--deletion-state file))
            (user-error "File changed; retry deletion of %s" file))
          (unwind-protect
              (condition-case err
                  (progn
                    (when exists (delete-file file))
                    (gnosis-nodes--delete-file
                     file nil nil (and journal-files (list journal-files journal-rows))))
                ((error quit)
                 (if (or (file-exists-p file) (file-symlink-p file))
                     (signal (car err) (cdr err))
                   (if (eq (car err) 'quit)
                       (progn
                         (message "File %s is gone; reconcile its index by retrying gnosis-nodes-delete-file" file)
                         (signal (car err) (cdr err)))
                     (error "File %s is gone; reconcile its index by retrying gnosis-nodes-delete-file: %s"
                            file (error-message-string err))))))
            (when (and (buffer-live-p buffer)
                       (eq buffer (gnosis-nodes--file-buffer file))
                       (not (or (file-exists-p file) (file-symlink-p file))))
              (with-current-buffer buffer
                (let ((inhibit-quit t)
                      (change-major-mode-with-file-name nil))
                  (when (buffer-file-name)
                    (set-visited-file-name nil t))
                  (setq gnosis-nodes--deleted-file
                        (append (list file gnosis-db)
                                (and journal-files
                                     (list journal-files journal-rows))))))))
          (if (and (buffer-live-p buffer)
                   (eq buffer (gnosis-nodes--file-buffer file))
                   (not (kill-buffer buffer)))
              (message "Deleted node file and index: %s; detached buffer retained" file)
            (message "Deleted node file and index: %s" file)))))))

;;; Find/create operations

(defun gnosis-nodes-find--tag-with-tag-prop (lst)
  "Combine each sublist of strings in LST into a single string."
  (mapcar (lambda (item)
            (let* ((title (car item))
                   (tags (cadr item))
                   (propertized-tags
		    (when tags
		      (let ((tag-list (if (stringp tags) (read tags) tags)))
			(when (and tag-list (not (equal tag-list '())))
                          (concat
                           (propertize "#" 'face 'gnosis-nodes-face-tags)
                           (propertize
                            (mapconcat #'identity tag-list "#")
                            'face 'gnosis-nodes-face-tags)))))))
              (if propertized-tags
                  (format "%s  %s" title propertized-tags)
		title)))
          lst))

(cl-defun gnosis-nodes--create-file (title &optional directory extras)
  "Create a node FILE for TITLE.
Insert initial Org metadata if the buffer is new or empty.
DIRECTORY: Directory where the file is created.
EXTRAS: The template to be inserted at the start."
  (let* ((file (expand-file-name
		(gnosis-org--create-name
		 title nil
		 (and directory
                      (equal (file-name-as-directory (expand-file-name directory))
                             (file-name-as-directory
                              (expand-file-name (gnosis-nodes--journal-dir))))
                      (bound-and-true-p gnosis-journal-as-gpg))
		 gnosis-nodes-create-as-gpg
		 gnosis-nodes-timestring)
		(or directory gnosis-nodes-dir)))
	 (org-id-track-globally nil))
    (when (and (file-exists-p file)
	       (not gnosis-nodes-timestring))
      (if (y-or-n-p (format "Node file already exists: %s.  Visit it?"
			    (file-name-nondirectory file)))
	  (progn (pop-to-buffer (find-file-noselect file))
		 (cl-return-from gnosis-nodes--create-file file))
	(user-error "Aborted: duplicate filename")))
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer
	(unless (or (file-exists-p file)
		    (> (buffer-size) 0))
	  (insert (format "#+title: %s\n#+filetags: \n" title))
	  (org-mode)
	  (org-id-get-create)
	  (when extras
	    (insert (gnosis-org-expand-headings extras)))))
      (switch-to-buffer buffer)
      (gnosis-nodes-mode 1))))

(defun gnosis-nodes-find--with-tags (&optional prompt entries)
  "Select a node title from tagged ENTRIES using PROMPT.
ENTRIES contains (TITLE TAGS) rows.  Preserve literal title text."
  (let* ((rows (or entries (gnosis-nodes-select '[title tags] 'nodes)))
         (candidates (cl-mapcar #'cons
                               (gnosis-nodes-find--tag-with-tag-prop rows)
                               (mapcar #'car rows)))
         (choice (funcall gnosis-nodes-completing-read-func
                          (or prompt "Select gnosis node: ")
                          (mapcar #'car candidates))))
    (or (cdr (assoc choice candidates)) choice)))

(defun gnosis-nodes--completion-candidates (rows &optional show-tags)
  "Return an alist of unique labels and IDs for ROWS.
ROWS contains (ID TITLE FILE TAGS) lists; TAGS may be omitted.
When SHOW-TAGS is non-nil, include tags in labels.  Distinguish duplicate
labels by file, then occurrence, reserving literal labels first."
  (let* ((labels (if show-tags
                     (gnosis-nodes-find--tag-with-tag-prop
                      (mapcar (lambda (row) (list (cadr row) (nth 3 row))) rows))
                   (mapcar #'cadr rows)))
         (counts (make-hash-table :test #'equal))
         (used (make-hash-table :test #'equal)))
    (dolist (label labels)
      (puthash label (1+ (gethash label counts 0)) counts)
      (puthash label t used))
    (cl-mapcar
     (lambda (row title)
       (cons (if (= 1 (gethash title counts)) title
               (let* ((base (format "%s — %s" title (nth 2 row)))
                      (label (cl-loop for n from 1
                                      for candidate = (if (= n 1) base
                                                        (format "%s <%d>" base n))
                                      unless (gethash candidate used)
                                      return candidate)))
                 (puthash label t used)
                 label))
             (car row)))
     rows labels)))

(defun gnosis-nodes--unique-title (title rows)
  "Return the unique row in ROWS whose title is TITLE, or nil.
Refuse ambiguous literal titles rather than selecting an arbitrary ID."
  (let ((matches (seq-filter (lambda (row) (equal title (cadr row))) rows)))
    (when (cdr matches)
      (user-error "Ambiguous node title %s; select a disambiguated label" title))
    (car matches)))

(defun gnosis-nodes--read-node (prompt rows &optional require-match)
  "Read a node with PROMPT, retaining the selected row from ROWS.
ROWS contains (ID TITLE FILE TAGS) lists.  Also accept a literal title
from ROWS for existing completion customizations.  Return (nil NEW-TITLE)
for unmatched input unless REQUIRE-MATCH is non-nil."
  (let* ((candidates (gnosis-nodes--completion-candidates rows gnosis-nodes-show-tags))
         (choice (funcall gnosis-nodes-completing-read-func
                          prompt (mapcar #'car candidates)))
         (id (cdr (assoc choice candidates))))
    (or (assoc id rows)
        (gnosis-nodes--unique-title choice rows)
        (if require-match (user-error "No node selected")
          (list nil choice)))))

(defun gnosis-nodes--find (prompt entries-with-tags entries)
  "PROMPT user to select from ENTRIES.
If `gnosis-nodes-show-tags' is non-nil, ENTRIES-WITH-TAGS will be used
instead."
  (let* ((entry (if gnosis-nodes-show-tags
                    (gnosis-nodes-find--with-tags
                     prompt entries-with-tags)
                  (funcall gnosis-nodes-completing-read-func
                           prompt entries))))
    entry))

;;;###autoload
(defun gnosis-nodes-find (&optional title file id directory templates)
  "Select a node by ID, or create a new FILE for an unmatched TITLE.
Use DIRECTORY and TEMPLATES for creation.  An explicit ID takes precedence
over TITLE and never creates a new node."
  (interactive)
  (gnosis-nodes-ensure-directories)
  (let* ((node (cond (id (car (gnosis-nodes-select
                              '[id title file tags] 'nodes `(= id ,id))))
                     (title (gnosis-nodes--unique-title
                             title (gnosis-nodes-select
                                    '[id title file tags] 'nodes `(= title ,title))))
                     (t (gnosis-nodes--read-node
                         "Select gnosis node: "
                         (gnosis-nodes-select '[id title file tags] 'nodes)))))
         (title (or (cadr node) title))
         (file (or file (nth 2 node)))
         (id (or id (car node)))
         (directory (or directory gnosis-nodes-dir)))
    (cond ((and id (not file)) (user-error "No indexed file for node %s" id))
          ((null file)
           (gnosis-nodes--create-file
            title directory (gnosis-nodes-select-template
                             (or templates gnosis-nodes-templates))))
          ((file-exists-p (expand-file-name file directory))
           (gnosis-nodes-goto-id id))
          (t (error "File %s does not exist.  \
Try `gnosis-nodes-db-force-sync' to resolve this" file)))))

(defun gnosis-nodes--nodes-by-tag (tag)
  "Return all node IDs associated with TAG.
Uses the node-tag junction table for proper querying."
  (gnosis-nodes-select 'node-id 'node-tag `(= tag ,tag) t))

;;;###autoload
(defun gnosis-nodes-find-by-tag (&optional tag)
  "Find node under TAG."
  (interactive)
  (let* ((tag (or tag (funcall gnosis-nodes-completing-read-func
			       "Select tag: "
			       (gnosis-nodes--all-tags))))
	 (nodes-ids (gnosis-nodes--nodes-by-tag tag))
         (node (gnosis-nodes--read-node
                "Select node: "
                (gnosis-nodes-select '[id title file tags] 'nodes
                                     `(in id ,(vconcat nodes-ids))) t)))
    (gnosis-nodes-find (cadr node) (nth 2 node) (car node))))

(defun gnosis-nodes--journal-file-p (file)
  "Return non-nil if FILE belongs to the configured journal."
  (and file
       (or (file-in-directory-p file (gnosis-nodes--journal-dir))
           (progn
             (require 'gnosis-journal)
             (gnosis-journal--file-p file)))))

(defun gnosis-nodes--journal-buffer-p ()
  "Return non-nil if current buffer is a journal file."
  (gnosis-nodes--journal-file-p buffer-file-name))

(defun gnosis-nodes--file-key (file journal)
  "Return the index filename for FILE.
Retain the path relative to `gnosis-nodes-dir', or to the journal
directory when JOURNAL is non-nil.  A configured single journal file
may be outside that directory."
  (file-relative-name (expand-file-name file)
                      (if journal (gnosis-nodes--journal-dir)
                        gnosis-nodes-dir)))

(defun gnosis-nodes-select-template (&optional templates)
  "Select and evaluate a template from TEMPLATES.
When TEMPLATES is nil, detect whether the current buffer is a
journal file and use `gnosis-journal-templates' or
`gnosis-nodes-templates' accordingly.

Template functions may use \"{*}\" as a heading placeholder.
The caller expands markers via `gnosis-org-expand-headings'.
\"{*}\" becomes the base level, \"{**}\" one level deeper, etc."
  (let* ((templates (or templates
                        (if (gnosis-nodes--journal-buffer-p)
                            gnosis-journal-templates
                          gnosis-nodes-templates)))
         (template (if (= (length templates) 1)
                       (cdar templates)
                     (cdr (assoc
			   (funcall
			    gnosis-nodes-completing-read-func
			    "Select template: "
			    (mapcar #'car templates))
                           templates)))))
    (unless (functionp template)
      (user-error
       "Template is not a valid function; \
check `gnosis-nodes-templates'"))
    (funcall template)))

;;;###autoload
(defun gnosis-nodes-insert-template ()
  "Insert a template at point with context-aware headings.
Detects whether the current buffer is a journal file and uses
the appropriate template list."
  (interactive)
  (insert (gnosis-org-expand-headings (gnosis-nodes-select-template))))

;;;###autoload
(defun gnosis-nodes-insert (arg &optional journal-p)
  "Insert a gnosis node link with its full indexed title as description.
If called with ARG, prompt for a custom description; an active region
supplies the description instead.  Preserve the insertion origin.
If JOURNAL-P is non-nil, retrieve/create node as a journal entry."
  (interactive "P")
  (let* ((table (if journal-p 'journal 'nodes))
         (node (gnosis-nodes--read-node
                "Select gnosis node: "
                (gnosis-nodes-select '[id title file tags] table)))
         (id (car node))
         (title (cadr node))
         (desc (cond ((use-region-p)
                      (buffer-substring-no-properties
                       (region-beginning) (region-end)))
                     (arg (read-string "Description: "))
                     (t title))))
    (unless id
      (save-window-excursion
        (save-excursion
          (save-restriction
            (gnosis-nodes--create-file
             title (if journal-p (gnosis-nodes--journal-dir) gnosis-nodes-dir))
            (save-buffer)
            (widen)
            (goto-char (point-min))
            (setq id (org-id-get))))))
    (unless id (user-error "Node %s has no root ID" title))
    (org-insert-link nil (format "id:%s" id) desc)))

(defun gnosis-nodes--filetags ()
  "Return list of current filetags, or nil, ignoring narrowing."
  (org-with-wide-buffer
   (gnosis-org-get-filetags)))

(defun gnosis-nodes--write-filetags (tags)
  "Write TAGS in one FILETAGS keyword, preserving literal text and root ID."
  (org-with-wide-buffer
   (let ((keywords (gnosis-org--filetag-keywords))
         (line (format "#+filetags: :%s:" (mapconcat #'identity tags ":"))))
     (if keywords
         ;; Edit backwards so the parser's positions stay valid.  Remove only
         ;; keyword lines, not their affiliated keywords or trailing blanks.
         (dolist (keyword (reverse keywords))
           (goto-char (org-element-property :post-affiliated keyword))
           (delete-region (point) (line-end-position))
           (if (eq keyword (car keywords))
               (insert line)
             (when (eq (char-after) ?\n) (delete-char 1))))
       (goto-char (point-min))
       ;; Org owns file-level drawer placement, including leading comments.
       (when-let* ((drawer (org-get-property-block (point-min))))
         (goto-char (cdr drawer))
         (forward-line))
       ;; Insert before keywords or body: a #+ line may open an Org block.
       (unless (bolp) (insert "\n"))
       (insert line "\n")))))

(defun gnosis-nodes-insert-filetag (&optional tag)
  "Insert TAG as filetag.
At a heading, add TAG to heading tags.  Otherwise, add to #+FILETAGS."
  (interactive)
  (let ((tag (or tag (funcall gnosis-nodes-completing-read-func
                              "Select tag: " (gnosis-nodes--all-tags)))))
    (if (org-at-heading-p)
        (org-set-tags
         (cl-union (list tag) (org-get-tags nil t)
                   :test #'string=))
      (let ((existing (gnosis-nodes--filetags)))
        (unless (member tag existing)
          (gnosis-nodes--write-filetags (append existing (list tag))))))))

;;;###autoload
(defun gnosis-nodes-insert-tags (tags)
  "Insert TAGS as filetags."
  (interactive
   (list (completing-read-multiple
	  "Select tags (separated by ,): "
	  (gnosis-nodes--all-tags))))
  (let ((id (gnosis-org-get-id))
	(org-id-track-globally nil))
    (unless id (user-error "No enclosing node ID"))
    (org-with-wide-buffer
     (org-id-goto id)
     (if (org-current-level)
         (org-set-tags (cl-union tags (org-get-tags nil t) :test #'string=))
       (dolist (tag tags)
         (gnosis-nodes-insert-filetag tag))))))

;;;###autoload
(defun gnosis-nodes-visit-backlinks ()
  "Visit backlinks for current node."
  (interactive)
  (let* ((id (gnosis-org-get-id))
	 (source-ids (gnosis-nodes-select 'source 'node-links `(= dest ,id) t))
	 (rows (when source-ids
                 (gnosis-sqlite-select-batch
                  (gnosis--ensure-db)
                  "SELECT id, title, file, tags FROM nodes WHERE id IN (%s)"
                  source-ids))))
    (if rows
        (let ((node (gnosis-nodes--read-node "Backlink: " rows t)))
          (gnosis-nodes-find (cadr node) (nth 2 node) (car node)))
      (message "No backlinks found for current node"))))

(defun gnosis-nodes-get-nodes-data (&optional node-ids)
  "Fetch node data for NODE-IDS or all nodes if not specified.
Returns a list of (ID TITLE BACKLINK-COUNT) for each node."
  (let* ((nodes (if node-ids
                    (gnosis-nodes-select '[id title] 'nodes
                                         `(in id ,(vconcat node-ids)))
                  (gnosis-nodes-select '[id title] 'nodes)))
         (backlinks (if node-ids
                        (gnosis-nodes-select '[dest source] 'node-links
                                             `(in dest ,(vconcat node-ids)))
                      (gnosis-nodes-select '[dest source] 'node-links)))
         (backlinks-count-hash (let ((hash (make-hash-table :test 'equal)))
                                 (dolist (link backlinks hash)
                                   (let ((dest (nth 0 link)))
                                     (puthash dest
                                              (1+ (or (gethash dest hash) 0))
                                              hash))))))
    (mapcar (lambda (node)
              (let ((id (nth 0 node))
                    (title (nth 1 node)))
                (list id title (or (gethash id backlinks-count-hash) 0))))
            nodes)))

;;; Navigation

(defun gnosis-nodes--get-id-at-point ()
  "Return the Org ID link at point, if any."
  (let* ((element (org-element-context))
         (id-link
          (when (and (eq (org-element-type element) 'link)
                     (string= (org-element-property :type element)
                              "id"))
            (org-element-property :path element))))
    id-link))

(defun gnosis-nodes-goto-id (&optional id)
  "Visit file for ID.
Enable `gnosis-nodes-mode' and widen a resolved node or journal destination.
If file or id are not found, use `org-open-at-point' without changing modes.
Refuse unresolved basename ownership before visiting another file."
  (interactive)
  (let* ((id (or id (gnosis-nodes--get-id-at-point)))
	 (org-id-track-globally nil))
    (if (not id)
        (org-open-at-point)
      (cond ((gnosis-nodes-select 'file 'nodes `(= id ,id))
	     (gnosis-nodes--check-node-ownership
              (expand-file-name
               (car (gnosis-nodes-select 'file 'nodes `(= id ,id) t))
               gnosis-nodes-dir))
	     (find-file
	      (expand-file-name
               (car (gnosis-nodes-select 'file 'nodes `(= id ,id) t))
	       gnosis-nodes-dir))
             (widen)
	     (org-id-goto id)
             (gnosis-nodes-mode 1))
	    ((gnosis-nodes-select 'file 'journal `(= id ,id))
	     (find-file
	      (expand-file-name
	       (car (gnosis-nodes-select 'file 'journal
				         `(= id ,id) t))
	       (gnosis-nodes--journal-dir)))
             (widen)
	     (org-id-goto id)
             (gnosis-nodes-mode 1))
            ((progn
               (require 'gnosis-journal)
               (gnosis-journal--goto-live-id id)))
	    (t (org-open-at-point))))))

;;; Sync

(defun gnosis-nodes--file-changed-p (file table)
  "Return non-nil if FILE's contents differ from its index in TABLE.
TABLE is either \\='nodes or \\='journal.  Compare hashes even when mtimes
agree: retained timestamps and some filesystems have only second precision.
Reading errors propagate so sync cannot silently accept an unreadable file."
  (let* ((filename (gnosis-nodes--file-key file (eq table 'journal)))
         (db-hash (caar (gnosis-nodes-select
                         'hash table `(= file ,filename)))))
    (or (not db-hash)
        (not (equal (gnosis-org--file-hash file) db-hash)))))

(defun gnosis-nodes--org-file-p (file)
  "Return non-nil for regular Org FILE inputs, excluding Emacs lockfiles."
  (and (not (string-prefix-p ".#" (file-name-nondirectory file)))
       (string-match-p "\\.org\\(?:\\.gpg\\)?$" file)
       (file-regular-p file)))

(defun gnosis-nodes--files ()
  "Return distinct physical Org sources, except journals.
Prefer retained indexed pathnames over newly discovered aliases."
  (let ((owners (gnosis-sqlite-select
                 (gnosis--ensure-db) "SELECT DISTINCT file FROM nodes ORDER BY file")))
   (cl-delete-duplicates
    (mapcar
     (lambda (file) (gnosis-nodes--node-file-owner file owners))
    (seq-filter
     (lambda (file)
       (and (gnosis-nodes--org-file-p file)
            (not (gnosis-nodes--journal-file-p file))))
     (directory-files-recursively
      gnosis-nodes-dir "\\.org\\(?:\\.gpg\\)?$" nil
      (lambda (directory)
        (not (gnosis-nodes--journal-file-p
              (file-name-as-directory directory)))))))
    :test #'file-equal-p :from-end t)))

(defun gnosis-nodes-db-update-files (&optional force source-files)
  "Sync node files with progress reporting.
Normally index changed files from disk, leaving visiting buffers untouched.
When FORCE, rebuild all files using visiting contents when available.
Only rebuild indexes; do not complete journal TODOs.
SOURCE-FILES, when non-nil, retains discovery made before a forced purge."
  (gnosis-nodes-ensure-directories)
  (let* ((all-files (or source-files (gnosis-nodes--files)))
         (files (if force
                    all-files
                  (cl-remove-if-not
                   (lambda (file) (gnosis-nodes--file-changed-p file 'nodes))
                   all-files))))
    (if (zerop (length files))
        (message "No files to sync")
      (let ((progress (make-progress-reporter
                       (format "Processing %d/%d files..."
                               (length files)
                               (length all-files))
                       0 (length files))))
        (cl-loop for file in files
                 for i from 0
                 do (progn
                      ;; External sync indexes disk.  Explicit rebuilds retain
                      ;; the live-buffer recovery behavior of the save writer.
                      (if force
                          (gnosis-nodes-update-file file t)
                        (gnosis-nodes--update-file file))
                      (progress-reporter-update progress i)))
        (progress-reporter-done progress)))))

(defun gnosis-nodes--purge-tables ()
  "Delete all rows from node and journal tables for full rebuild."
  (dolist (table '(nodes journal node-links node-tag))
    (gnosis--delete table)))

;;;###autoload
(defun gnosis-nodes-db-sync (&optional force)
  "Sync node database with progress reporting.
When FORCE (prefix arg), rebuild from scratch."
  (interactive "P")
  (let ((gc-cons-threshold most-positive-fixnum))
    (gnosis-nodes-ensure-directories)
    (message "Syncing nodes database...")
    (let ((source-files (gnosis-nodes--files)))
     (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (when force
	(gnosis-nodes--purge-tables)
	(message "Purged all node/journal tables for rebuild."))
      ;; Reconcile legacy journal snapshots before ordinary basename-wide
      ;; updates can erase their ownership evidence.  Keep both in this
      ;; transaction so ambiguous ownership leaves the entire index unchanged.
      (message "Syncing journal files...")
      (require 'gnosis-journal)
      (gnosis-journal-db-sync force)
      (gnosis-nodes-db-update-files force source-files)))
    (message "Node sync complete!")))

;;;###autoload
(defun gnosis-nodes-db-force-sync ()
  "Force rebuild node data from files."
  (interactive)
  (when (y-or-n-p "Force rebuild node database from files?")
    (gnosis-nodes-db-sync 'force)))

;;; Minor mode

(defvar-keymap gnosis-nodes-mode-map
  :doc "gnosis-nodes keymap"
  "C-c C-o" #'gnosis-nodes-goto-id
  "C-c C-q" #'gnosis-nodes-insert-tags)

(define-minor-mode gnosis-nodes-mode
  "Gnosis nodes mode."
  :lighter " gnosis-nodes"
  :keymap gnosis-nodes-mode-map
  :global nil
  :group 'gnosis-nodes
  (if gnosis-nodes-mode
      (add-hook 'after-save-hook #'gnosis-nodes-update-file nil t)
    (remove-hook 'after-save-hook #'gnosis-nodes-update-file t)))

(defun gnosis-nodes--find-file-h ()
  "Enable `gnosis-nodes-mode' for org files in gnosis directories.
Added to `org-mode-hook'."
  (when (and buffer-file-name
             (derived-mode-p 'org-mode)
             (or (file-in-directory-p
                  buffer-file-name gnosis-nodes-dir)
                 (gnosis-nodes--journal-buffer-p)))
    (gnosis-nodes-mode 1)))

(add-hook 'org-mode-hook #'gnosis-nodes--find-file-h)

;; Forward declarations for journal functions
(declare-function gnosis-journal-db-sync "gnosis-journal")

(provide 'gnosis-nodes)
;;; gnosis-nodes.el ends here
