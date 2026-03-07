;;; gnosis-nodes.el --- Node management for gnosis  -*- lexical-binding: t; -*-

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

;; Forward declarations for gnosis.el functions
(declare-function gnosis-select "gnosis")
(declare-function gnosis--insert-into "gnosis")
(declare-function gnosis--delete "gnosis")
(declare-function gnosis--ensure-db "gnosis")
(declare-function gnosis-update "gnosis")
(declare-function gnosis-sqlite-execute "gnosis-sqlite")
(declare-function gnosis-sqlite-select "gnosis-sqlite")
(declare-function gnosis-sqlite-with-transaction "gnosis-sqlite")
(declare-function gnosis-journal--dir "gnosis-journal")

(defvar gnosis-journal-dir)
(defvar gnosis-journal-file)
(defvar gnosis-journal-templates)

(defgroup gnosis-nodes nil
  "Gnosis node management."
  :group 'gnosis)

(defcustom gnosis-nodes-dir (expand-file-name "Notes" "~")
  "Directory with gnosis nodes."
  :type 'directory)

(defcustom gnosis-nodes-show-tags nil
  "Display tags with `gnosis-nodes-find'."
  :type 'boolean)

(defcustom gnosis-nodes-timestring "%Y%m%d%H%M%S"
  "Timestring used for the creation of node files."
  :type 'string)

(defcustom gnosis-nodes-create-as-gpg nil
  "When non-nil, create all node files with a .gpg suffix."
  :type 'boolean)

(defcustom gnosis-nodes-templates
  '(("Default" (lambda () ""))
    ("test" (lambda () "" "\n{*} insert this")))
  "Templates for nodes.
Template functions return strings.  Use \"{*}\" as a heading
placeholder; it will be expanded to org heading stars relative to
the insertion context.  \"{**}\" adds one extra level, \"{***}\"
adds two, etc."
  :type '(repeat (cons (string :tag "Name")
                       (function :tag "Template Function"))))

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
Delegates to `gnosis-select' (unified DB)."
  (gnosis-select value table restrictions flatten))

(defun gnosis-nodes--insert-into (table values)
  "Insert VALUES to TABLE.
Delegates to `gnosis--insert-into' (unified DB)."
  (gnosis--insert-into table values))

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

(defun gnosis-nodes--update-file (file &optional journal)
  "Add contents of FILE to database.
If JOURNAL is non-nil, update file as a journal entry."
  (condition-case err
      (let* ((info (gnosis-org-get-file-info file))
	     (data (butlast info))
	     (table (if journal 'journal 'nodes))
	     (filename (file-name-nondirectory file))
	     (full-path (expand-file-name file (if journal
						   (gnosis-journal--dir)
						 gnosis-nodes-dir)))
	     (mtime (format-time-string "%s" (file-attribute-modification-time
					      (file-attributes full-path))))
	     (hash (gnosis-org--file-hash full-path))
	     (links (and (> (length info) 1) (apply #'append (last info)))))
	(message "Parsing: %s" filename)
	(gnosis-sqlite-with-transaction (gnosis--ensure-db)
	  (cl-loop for item in data
		   when (plist-get item :id)
		   do (let ((title (plist-get item :title))
			    (id (plist-get item :id))
			    (tags (plist-get item :tags))
			    (level (plist-get item :level)))
			(gnosis-nodes--insert-into table
			  `([,id ,filename ,title ,level
			     ,(prin1-to-string tags) ,mtime ,hash]))
			;; Insert tags
			(cl-loop for tag in tags
				 do (gnosis-nodes--insert-into 'node-tag `([,id ,tag])))
			;; Insert master relationship as link (nodes only)
			(when (and (not journal)
				   (plist-get item :master)
				   (stringp (plist-get item :master)))
			  (gnosis-nodes--insert-into 'node-links `([,id ,(plist-get item :master)])))))
	  ;; Insert ID links (nodes only)
	  (unless journal
	    (cl-loop for link in links
		     do (gnosis-nodes--insert-into 'node-links `[,(cdr link) ,(car link)])))))
    (file-error
     (message "File error updating %s: %s.  Try M-x gnosis-nodes-db-force-sync to rebuild database."
              file (error-message-string err)))
    (error
     (message "Error updating %s: %s.  Try M-x gnosis-nodes-db-force-sync if issue persists."
              file (error-message-string err)))))

(defun gnosis-nodes--delete-file (&optional file)
  "Delete contents for FILE in database."
  (let* ((file (or file (file-name-nondirectory (buffer-file-name))))
	 (filename (file-name-nondirectory file))
	 (journal-p (file-in-directory-p file (gnosis-journal--dir)))
	 (nodes (if journal-p
		    (gnosis-nodes-select 'id 'journal `(= file ,filename) t)
		  (gnosis-nodes-select 'id 'nodes `(= file ,filename) t))))
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (cl-loop for node in nodes
	       do (if journal-p
		      (gnosis-nodes--delete 'journal `(= id ,node))
		    (gnosis-nodes--delete 'nodes `(= id ,node)))))))

(defun gnosis-nodes-update-file (&optional file)
  "Update contents of FILE in database.
Removes all contents of FILE in database, adding them anew."
  (let* ((file (or file (buffer-file-name)))
	 (journal-p (file-in-directory-p file (gnosis-journal--dir))))
    (gnosis-nodes--delete-file file)
    (gnosis-nodes--update-file file journal-p)
    ;; Update todos
    (when (and journal-p file)
      (gnosis-journal--update-todos file))))

;;;###autoload
(defun gnosis-nodes-delete-file (&optional file)
  "Delete FILE.
Delete file contents in database & file."
  (interactive)
  (let ((file (or file (file-name-nondirectory (buffer-file-name)))))
    (if (or (file-in-directory-p (buffer-file-name) gnosis-nodes-dir)
	    (file-in-directory-p (buffer-file-name) (gnosis-journal--dir)))
	(progn
	  (when (y-or-n-p (format "Delete file: %s?" file))
	    (gnosis-nodes--delete-file file)
	    (delete-file (buffer-file-name))
	    (kill-buffer (buffer-name))))
      (error "%s is not a gnosis node file" file))))

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
                          (concat (propertize "#" 'face 'gnosis-nodes-face-tags)
                                  (propertize (mapconcat #'identity tag-list "#")
					      'face 'gnosis-nodes-face-tags)))))))
              (if propertized-tags
                  (format "%s  %s" title propertized-tags)
		title)))
          lst))

(defun gnosis-nodes--create-file (title &optional directory extras)
  "Create a node FILE for TITLE.
Insert initial Org metadata if the buffer is new or empty.
DIRECTORY: Directory where the file is created.
EXTRAS: The template to be inserted at the start."
  (let* ((file (expand-file-name
		(gnosis-org--create-name
		 title nil
		 (and (eq directory (gnosis-journal--dir))
		      (bound-and-true-p gnosis-journal-as-gpg))
		 gnosis-nodes-create-as-gpg
		 gnosis-nodes-timestring)
		(or directory gnosis-nodes-dir)))
	 (buffer (find-file-noselect file))
	 (org-id-track-globally nil))
    (with-current-buffer buffer
      (unless (or (file-exists-p file)
		  (> (buffer-size) 0))
	(insert (format "#+title: %s\n#+filetags: \n" title))
	(org-mode)
	(org-id-get-create)
	(when extras
	  (insert (gnosis-org-expand-headings extras)))))
    (switch-to-buffer buffer)
    (gnosis-nodes-mode 1)))

(defun gnosis-nodes-find--with-tags (&optional prompt entries)
  "Select gnosis node with tags from ENTRIES.
PROMPT: Prompt message."
  (replace-regexp-in-string
   "  #[^[:space:]]+" ""
   (funcall gnosis-nodes-completing-read-func (or prompt "Select gnosis node: ")
	    (gnosis-nodes-find--tag-with-tag-prop
	     (or entries (gnosis-nodes-select '[title tags] 'nodes))))))

(defun gnosis-nodes--find (prompt entries-with-tags entries)
  "PROMPT user to select from ENTRIES.
If `gnosis-nodes-show-tags' is non-nil, ENTRIES-WITH-TAGS will be used
instead."
  (let* ((entry (if gnosis-nodes-show-tags
                    (gnosis-nodes-find--with-tags prompt entries-with-tags)
                  (funcall gnosis-nodes-completing-read-func prompt entries))))
    entry))

;;;###autoload
(defun gnosis-nodes-find (&optional title file id directory templates)
  "Select gnosis node.
If there is no ID for TITLE, create a new FILE with TITLE as TOPIC in
DIRECTORY."
  (interactive)
  (gnosis-nodes-ensure-directories)
  (let* ((title (or title (if gnosis-nodes-show-tags
			      (gnosis-nodes-find--with-tags)
			    (funcall gnosis-nodes-completing-read-func
				     "Select gnosis node: "
				     (gnosis-nodes-select 'title 'nodes)))))
	 (file (or file (caar (gnosis-nodes-select 'file 'nodes `(= title ,title)))))
	 (id (or id (caar (gnosis-nodes-select 'id 'nodes `(= title ,title)))))
	 (directory (or directory gnosis-nodes-dir))
	 (templates (or templates gnosis-nodes-templates)))
    (cond ((null file)
	   (gnosis-nodes--create-file title directory
				      (gnosis-nodes-select-template templates)))
	  ((file-exists-p (expand-file-name file directory))
	   (gnosis-nodes-goto-id id))
	  (t (error
	      "File %s does not exist.  Try running `gnosis-nodes-db-force-sync' to resolve this"
	      file)))))

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
	 (node-titles (gnosis-nodes-select 'title 'nodes `(in id ,(vconcat nodes-ids)) t))
	 (node (completing-read "Select node: " node-titles nil t)))
    (gnosis-nodes-find node)))

(defun gnosis-nodes--journal-buffer-p ()
  "Return non-nil if current buffer is a journal file."
  (and buffer-file-name
       (or (file-in-directory-p buffer-file-name (gnosis-journal--dir))
           (and gnosis-journal-file
                (string= (expand-file-name buffer-file-name)
                         (expand-file-name gnosis-journal-file))))))

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
			   (funcall gnosis-nodes-completing-read-func "Select template:"
                                    (mapcar #'car templates))
                           templates)))))
    (funcall (apply #'append template))))

;;;###autoload
(defun gnosis-nodes-insert-template ()
  "Insert a template at point with context-aware headings.
Detects whether the current buffer is a journal file and uses
the appropriate template list."
  (interactive)
  (insert (gnosis-org-expand-headings (gnosis-nodes-select-template))))

;;;###autoload
(defun gnosis-nodes-insert (arg &optional journal-p)
  "Insert gnosis node link.
If called with ARG, prompt for custom link description.
If JOURNAL-P is non-nil, retrieve/create node as a journal entry."
  (interactive "P")
  (let* ((table (if journal-p 'journal 'nodes))
         (node (gnosis-nodes--find "Select gnosis node: "
                                   (gnosis-nodes-select '[title tags] table)
                                   (gnosis-nodes-select 'title table)))
         (id (car (gnosis-nodes-select 'id table `(= title ,node) t)))
	 (title (car (last (split-string node ":"))))
         (desc (cond ((use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end)))
                     (arg (read-string "Description: "))
                     (t title))))
    (unless id
      (save-window-excursion
        (gnosis-nodes--create-file node (if journal-p (gnosis-journal--dir) gnosis-nodes-dir))
        (save-buffer)
        (setf id (car (gnosis-nodes-select 'id table `(= title ,node) t)))))
    (org-insert-link nil (format "id:%s" id) desc)
    (unless id (message "Created new node: %s" node))))

(defun gnosis-nodes-insert-filetag (&optional tag)
  "Insert TAG as filetag."
  (interactive)
  (let* ((filetags (gnosis-nodes--all-tags))
         (tag (or tag (funcall gnosis-nodes-completing-read-func "Select tag: " filetags))))
    (save-excursion
      (if (org-at-heading-p)
	  (org-set-tags tag)
	(goto-char (point-min))
	(if (re-search-forward "^#\\+FILETAGS:" nil t)
            (progn
              (end-of-line)
              (insert (if (looking-back ":" nil) "" ":") tag ":"))
          (progn
            (insert "#+FILETAGS: :" tag ":")
            (newline)))))))

;;;###autoload
(defun gnosis-nodes-insert-tags (tags)
  "Insert TAGS as filetags."
  (interactive
   (list (completing-read-multiple
	  "Select tags (separated by ,): "
	  (gnosis-nodes--all-tags))))
  (let ((id (gnosis-org-get-id)))
    (org-id-goto id)
    (if (org-current-level)
	(org-set-tags tags)
      (dolist (tag tags)
	(gnosis-nodes-insert-filetag tag)))))

;;;###autoload
(defun gnosis-nodes-visit-backlinks ()
  "Visit backlinks for current node."
  (interactive)
  (let* ((id (gnosis-org-get-id))
	 (backlinks (gnosis-nodes-select '[source] 'node-links `(= dest ,id)))
	 (titles (cl-loop for backlink in backlinks
			  for source-id = (car backlink)
			  for title = (caar (gnosis-nodes-select 'title 'nodes
								 `(= id ,source-id)))
			  when title collect title)))
    (if titles
	(gnosis-nodes-find
	 (completing-read "Backlink: " titles))
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
         (id-link (when (and (eq (org-element-type element) 'link)
                             (string= (org-element-property :type element) "id"))
                    (org-element-property :path element))))
    id-link))

(defun gnosis-nodes-goto-id (&optional id)
  "Visit file for ID.
If file or id are not found, use `org-open-at-point'."
  (interactive)
  (let* ((id (or id (gnosis-nodes--get-id-at-point)))
	 (org-id-track-globally nil))
    (cond ((gnosis-nodes-select 'file 'nodes `(= id ,id))
	   (find-file
	    (expand-file-name (car (gnosis-nodes-select 'file 'nodes `(= id ,id) t))
			      gnosis-nodes-dir))
	   (org-id-goto id))
	  ((gnosis-nodes-select 'file 'journal `(= id ,id))
	   (find-file
	    (expand-file-name (car (gnosis-nodes-select 'file 'journal `(= id ,id) t))
			      (gnosis-journal--dir)))
	   (org-id-goto id))
	  (t (org-open-at-point)))
    (gnosis-nodes-mode 1)))

;;; Sync

(defun gnosis-nodes--file-changed-p (file table)
  "Check if FILE changed since last sync using mtime then hash.
TABLE is either \\='nodes or \\='journal."
  (let* ((filename (file-name-nondirectory file))
         (file-mtime (format-time-string "%s" (file-attribute-modification-time
					       (file-attributes file))))
         (db-data (car (gnosis-nodes-select '[mtime hash] table `(= file ,filename))))
         (db-mtime (car db-data))
         (db-hash (cadr db-data)))
    (or (not db-mtime)
        (and (not (string= file-mtime db-mtime))
             (not (string= (gnosis-org--file-hash file) db-hash))))))

(defun gnosis-nodes-db-update-files (&optional force)
  "Sync node files with progress reporting.
When FORCE, update all files.  Otherwise, only update changed files."
  (gnosis-nodes-ensure-directories)
  (let* ((all-files (cl-remove-if-not
                     (lambda (file)
                       (and (string-match-p "^[0-9]"
                                            (file-name-nondirectory file))
                            (not (file-directory-p file))))
                     (directory-files gnosis-nodes-dir t nil t)))
         (files (if force
                    all-files
                  (cl-remove-if-not
                   (lambda (file) (gnosis-nodes--file-changed-p file 'nodes))
                   all-files))))
    (if (zerop (length files))
        (message "No files to sync")
      (let ((progress (make-progress-reporter
                       (format "Processing %d/%d files..." (length files) (length all-files))
                       0 (length files))))
        (cl-loop for file in files
                 for i from 0
                 do (progn
                      (gnosis-nodes-update-file file)
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
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (when force
	(gnosis-nodes--purge-tables)
	(message "Purged all node/journal tables for rebuild."))
      (gnosis-nodes-db-update-files force))
    ;; Sync journal files
    (message "Syncing journal files...")
    (gnosis-journal-db-sync force)
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
  "C-c C-o" #'gnosis-nodes-goto-id)

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
             (or (file-in-directory-p buffer-file-name gnosis-nodes-dir)
                 (file-in-directory-p buffer-file-name (gnosis-journal--dir))))
    (gnosis-nodes-mode 1)))

(add-hook 'org-mode-hook #'gnosis-nodes--find-file-h)

;; Forward declaration for journal directory
(declare-function gnosis-journal--dir "gnosis-journal")
(declare-function gnosis-journal-db-sync "gnosis-journal")
(declare-function gnosis-journal--update-todos "gnosis-journal")

(provide 'gnosis-nodes)
;;; gnosis-nodes.el ends here
