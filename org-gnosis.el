;;; org-gnosis.el --- Learning-focused note-taking for Gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: outlines, extensions, org-mode
;; URL: https://thanosapollo.org/projects/org-gnosis/
;; Version: 0.2.0

;; Package-Requires: ((emacs "27.2") (emacsql "4.0.0") (compat "29.1.4.2"))

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

;; Org-gnosis provides a minimal note-taking system with linked notes.
;; It functions as an independent module, designed to integrate with
;; Gnosis (a spaced repetition system) but usable standalone.
;;
;; The package implements Zettelkasten-style note-taking: notes have
;; unique IDs, link to other notes, and the system tracks backlinks.
;; Notes are stored as org files and indexed in an SQLite database.
;;
;; When used with Gnosis, notes can link to themata (question subjects),
;; connecting note-taking with spaced repetition.  Without Gnosis, it
;; provides a lightweight system for linked notes and journaling.
;;
;; Features:
;; - Unique IDs for each note
;; - Backlinks and forward link tracking
;; - Tag-based organization
;; - Daily journaling (single file or separate files per day)
;; - SQLite database for indexing
;; - Hierarchical note structures
;; - Optional .gpg encryption
;; - Optional integration with Gnosis themata
;;
;; Main interactive commands:
;; - `org-gnosis-find': Find or create a note
;; - `org-gnosis-insert': Insert a link to a note
;; - `org-gnosis-journal': Open today's journal entry
;; - `org-gnosis-find-by-tag': Find notes by tag
;; - `org-gnosis-visit-backlinks': Visit notes linking to current note
;; - `org-gnosis-db-sync': Sync database with files
;;
;;; Code:

(require 'cl-lib)
(require 'emacsql-sqlite)
(require 'org-element)

(defgroup org-gnosis nil
  "Note Taking System."
  :group 'external)
;; is ~/Notes appropriate for all Operating Systems?
(defcustom org-gnosis-dir (expand-file-name "Notes" "~")
  "Directory with gnosis notes."
  :type 'directory)

(defcustom org-gnosis-journal-templates
  '(("Default" (lambda () (format "** Daily Notes\n\n** Goals\n%s" (org-gnosis-todos))))
    ("Empty" (lambda () "")))
  "Templates for journaling."
  :type '(repeat (cons (string :tag "Name")
                       (function :tag "Template Function"))))

(defcustom org-gnosis-node-templates
  '(("Default" (lambda () "")))
  "Templates for nodes."
  :type '(repeat (cons (string :tag "Name")
                       (function :tag "Template Function"))))

(defcustom org-gnosis-journal-dir (expand-file-name "journal" org-gnosis-dir)
  "Gnosis journal directory."
  :type 'directory)

(defun org-gnosis-ensure-directories ()
  "Create note and journal directories if they do not exist."
  (dolist (dir (list org-gnosis-dir org-gnosis-journal-dir))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defcustom org-gnosis-show-tags nil
  "Display tags with `org-gnosis-find'."
  :type 'boolean)

(defcustom org-gnosis-timestring "%Y%m%d%H%M%S"
  "Timestring used for the creation of file."
  :type 'string)

(defcustom org-gnosis-create-as-gpg nil
  "When non-nil, create all files with a .gpg suffix."
  :type 'boolean)

(defcustom org-gnosis-journal-as-gpg nil
  "When non-nil, create journal entries with a .gpg suffix."
  :type 'boolean)

(defcustom org-gnosis-todo-files org-agenda-files
  "TODO files used for the journal entries."
  :type '(repeat string))

(defcustom org-gnosis-todo-keywords org-todo-keywords
  "TODO Keywords used for parsing `org-gnosis-todo-files'.

All items after the vertical bar \"|\" will be ignored, for
compatability with `org-todo-keywords'."
  :type '(repeat string))

(defcustom org-gnosis-completing-read-func #'org-completing-read
  "Function to use for `completing-read'."
  :type 'function)

(defcustom org-gnosis-bullet-point-char "+"
  "String to indicate a bullet point."
  :type 'string)

(defface org-gnosis-face-tags
  '((t :inherit font-lock-type-face))
  "Face for displaying gnosis with `org-gnosis-find'.")

(defcustom org-gnosis-database-file (locate-user-emacs-file "org-gnosis.db")
  "Path to database file."
  :type 'file
  :group 'org-gnosis)

(defcustom org-gnosis-journal-file (expand-file-name "journal.org" org-gnosis-journal-dir)
  "When non-nil, use this file for journal entries as level 1 headings.

If nil, journal entries are created as separate files in
`org-gnosis-journal-dir'."
  :type '(choice (const :tag "Use separate files" nil)
                 (file :tag "Single journal file")))

(defun org-gnosis-journal--create-file ()
  "Create `org-gnosis-journal' when non-nil and file it does not exists."
  (when (and org-gnosis-journal-file
	     (not (file-exists-p org-gnosis-journal-file)))
    (with-current-buffer (find-file-noselect org-gnosis-journal-file)
      (insert (format "#+title: %s Journal \n#+filetags: \n" (or user-full-name "")))
      (org-gnosis-mode)
      (save-buffer)
      (message "Created journal file."))))

(defun org-gnosis-journal--add-entry (title)
  "Add entry for TITLE to `org-gnosis-journal-file'."
  (when org-gnosis-journal-file
    (org-gnosis-journal--create-file)
    (find-file org-gnosis-journal-file)
    (goto-char (point-max))
    (insert (format "* %s\n" title))
    (org-id-get-create)
    (insert (org-gnosis-select-template org-gnosis-journal-templates))))

(defvar org-gnosis-db--connection nil)

(defun org-gnosis-db-get ()
  "Get or create the database connection."
  (unless (and org-gnosis-db--connection
               (emacsql-live-p org-gnosis-db--connection)
	       (file-exists-p org-gnosis-database-file))
    (setq org-gnosis-db--connection
          (emacsql-sqlite-open org-gnosis-database-file)))
  org-gnosis-db--connection)



(defun org-gnosis--combine-tags (inherited-tags headline-tags)
  "Combine INHERITED-TAGS and HEADLINE-TAGS, removing duplicates."
  (delete-dups (append (or inherited-tags '()) (or headline-tags '()))))

(defun org-gnosis-select (value table &optional restrictions flatten)
  "Select VALUE from TABLE, optionally with RESTRICTIONS.

Optional argument FLATTEN, when non-nil, flattens the result."
  (org-gnosis-db-init-if-needed) ;; Init database if needed
  ;; Check for database upgrades
  (let* ((restrictions (or restrictions '(= 1 1)))
	 (flatten (or flatten nil))
	 (output (emacsql (org-gnosis-db-get)
			  `[:select ,value :from ,table :where ,restrictions])))
    (if flatten (apply #'append output) output)))

(defun org-gnosis--insert-into (table values)
  "Insert VALUES to TABLE."
  (emacsql (org-gnosis-db-get) `[:insert :into ,table :values ,values]))

(defun org-gnosis--delete (table value)
  "From TABLE use where to delete VALUE."
  (emacsql (org-gnosis-db-get) `[:delete :from ,table :where ,value]))

(defun org-gnosis--drop-table (table)
  "Drop TABLE from `gnosis-db'."
  (emacsql (org-gnosis-db-get) `[:drop-table ,table]))

(defun org-gnosis-adjust-title (input)
  "Strip org link markup from INPUT, keeping only link descriptions.
Converts [[id:xxx][Description]] to Description."
  (replace-regexp-in-string
   "\\[\\[id:[^]]+\\]\\[\\(.*?\\)\\]\\]"
   "\\1"
   input))

(defun org-gnosis-get-id ()
  "Return id for heading at point."
  (save-excursion
    (let ((heading-level (org-current-level))
	  (id (org-id-get)))
      (cond (id id)
	    ((and (null id) (= heading-level 1))
	     (goto-char (point-min))
	     (org-id-get))
	    (t
	     (outline-up-heading 1 t)
	     (org-gnosis-get-id))))))

(defun org-gnosis-collect-id-links ()
  "Collect ID links and current headline ID as (link-id . headline-id) pairs."
  (let ((links nil)
        (begin (point-min))
        (end (point-max)))
    (save-excursion
      (goto-char begin)
      (while (re-search-forward org-link-any-re end t)
        (let ((link (match-string-no-properties 0)))
          (when (string-match "id:\\([^]]+\\)" link)
            (let ((target-id (match-string 1 link))
                  (source-id (org-gnosis-get-id)))
              (when (and target-id source-id)
                (push (cons target-id source-id) links)))))))
    (nreverse links)))

(defun org-gnosis-get-data--topic (&optional parsed-data)
  "Retrieve the title and ID from the current org buffer or given PARSED-DATA.
Returns (title tags id).  ID will be nil if no file-level ID exists."
  (unless parsed-data
    (setq parsed-data (org-element-parse-buffer)))
  (let* ((id (org-element-map parsed-data 'property-drawer
               (lambda (drawer)
                 ;; Only consider file-level property drawer (before first headline)
                 (let ((parent (org-element-property :parent drawer)))
                   (when (and parent
                              (eq (org-element-type parent) 'section)
                              (let ((section-parent (org-element-property :parent parent)))
                                (eq (org-element-type section-parent) 'org-data)))
                     (org-element-map (org-element-contents drawer) 'node-property
                       (lambda (prop)
                         (when (string= (org-element-property :key prop) "ID")
                           (org-element-property :value prop)))
                       nil t))))
               nil t))
	 (title-raw (org-element-map parsed-data 'keyword
                      (lambda (kw)
                        (when (string= (org-element-property :key kw) "TITLE")
                          (org-element-property :value kw)))
                      nil t))
	 (title (when title-raw (org-gnosis-adjust-title title-raw)))
	 (tags (org-gnosis-get-filetags parsed-data)))
    ;; Only validate title, ID is optional
    (unless (and title (not (string-empty-p title)))
      (error "Org buffer must have a non-empty TITLE"))
    (list title tags id)))

(defun org-gnosis-get-filetags (&optional parsed-data)
  "Return the filetags of the buffer's PARSED-DATA as a list of strings."
  (let* ((parsed-data (or parsed-data (org-element-parse-buffer)))
         (filetags (org-element-map parsed-data 'keyword
                     (lambda (kw)
                       (when (string-equal (org-element-property :key kw) "FILETAGS")
                         (org-element-property :value kw)))
                     nil t)))
    (when (and filetags (not (string-empty-p (string-trim filetags))))
      (remove "" (split-string filetags ":")))))

(defun org-gnosis--parse-headlines-recursive (element parent-id parent-title parent-tags)
  "Recursively parse headlines from ELEMENT.
ELEMENT can be the parsed-data (org-data) or a headline element.
PARENT-ID is the ID of nearest ancestor with ID (or 0).
PARENT-TITLE is the hierarchical title path (only from ancestors with IDs).
PARENT-TAGS are the inherited tags from ancestors."
  (let (results)
    (org-element-map (org-element-contents element) 'headline
      (lambda (headline)
        (let* ((current-id (org-element-property :ID headline))
               (title (org-element-property :raw-value headline))
               (level (org-element-property :level headline))
               (headline-tags (org-element-property :tags headline))
               (combined-tags (org-gnosis--combine-tags parent-tags headline-tags)))

          (if current-id
              ;; This headline has an ID - process it
              (let* ((clean-title (org-gnosis-adjust-title (string-trim title)))
                     (full-title (if parent-title
                                     (concat parent-title ":" clean-title)
                                   clean-title))
                     (entry (list :id current-id
                                  :title full-title
                                  :tags combined-tags
                                  :master (or parent-id 0)
                                  :level level))
                     ;; Recursively process children with THIS as parent
                     (children (org-gnosis--parse-headlines-recursive
				headline
				current-id
				full-title
				combined-tags)))
                (setq results (append results (cons entry children))))

            ;; No ID - skip this headline but process children
            ;; Children inherit from the same parent context
            (let ((children (org-gnosis--parse-headlines-recursive
                             headline
                             parent-id
                             parent-title
                             combined-tags)))
              (setq results (append results children))))))
      nil nil 'headline)
    results))

(defun org-gnosis-buffer-data (&optional data)
  "Parse DATA in current buffer for topics & headlines with their ID, tags, links."
  (let* ((parsed-data (or data (org-element-parse-buffer)))
         (topic-info (org-gnosis-get-data--topic parsed-data))
         (topic-title (nth 0 topic-info))
         (topic-tags (nth 1 topic-info))
         (topic-id (nth 2 topic-info))
         ;; Recursively parse all headlines
         (headlines (org-gnosis--parse-headlines-recursive
                     parsed-data
                     topic-id
                     (when topic-id topic-title)
                     topic-tags)))
    ;; Only include topic if it has an ID
    (if topic-id
        (cons (list :title topic-title
                    :id topic-id
                    :tags topic-tags
                    :master 0
                    :level 0)
              headlines)
      headlines)))

(defun org-gnosis-get-file-info (filename)
  "Get data for FILENAME.

Returns file data with FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (org-mode)
    (org-set-regexps-and-options 'tags-only)
    (let* ((data (org-gnosis-buffer-data))
	   (links (org-gnosis-collect-id-links)))
      ;; Append links even if they are nil
      (append data (list links)))))

(defun org-gnosis--update-file (file &optional journal)
  "Add contents of FILE to database.

If JOURNAL is non-nil, update file as a journal entry."
  (condition-case err
      (let* ((info (org-gnosis-get-file-info file))
	     (data (butlast info))
	     (table (if journal 'journal 'nodes))
	     (filename (file-name-nondirectory file))
	     (full-path (expand-file-name file (if journal org-gnosis-journal-dir org-gnosis-dir)))
	     (mtime (format-time-string "%s" (file-attribute-modification-time (file-attributes full-path))))
	     (hash (org-gnosis--file-hash full-path))
	     (links (and (> (length info) 1) (apply #'append (last info)))))
	;; Add gnosis topic and nodes
	(message "Parsing: %s" filename)
	(emacsql-with-transaction (org-gnosis-db-get)
	  (cl-loop for item in data
		   when (plist-get item :id)
		   do (let ((title (plist-get item :title))
			    (id (plist-get item :id))
			    (master (plist-get item :master))
			    (tags (plist-get item :tags))
			    (level (plist-get item :level)))
			(org-gnosis--insert-into table `([,id ,filename ,title ,level
							      ,(prin1-to-string tags) ,mtime ,hash]))
			;; Insert tags
			(cl-loop for tag in tags
				 do
				 (ignore-errors (org-gnosis--insert-into 'tags `([,tag])))
				 (org-gnosis--insert-into 'node-tag `([,id ,tag])))
			;; Insert master relationship as link
			(when (and master (stringp master))
			  (org-gnosis--insert-into 'links `([,id ,master])))))
	  ;; Insert ID links
	  (cl-loop for link in links
		   do (org-gnosis--insert-into 'links `[,(cdr link) ,(car link)]))))
    (file-error
     (message "File error updating %s: %s.  Try M-x org-gnosis-db-force-sync to rebuild database."
              file (error-message-string err)))
    (error
     (message "Error updating %s: %s.  Try M-x org-gnosis-db-force-sync if issue persists."
              file (error-message-string err)))))

(defun org-gnosis--delete-file (&optional file)
  "Delete contents for FILE in database."
  (let* ((file (or file (file-name-nondirectory (buffer-file-name))))
	 (filename (file-name-nondirectory file))
	 (journal-p (file-in-directory-p file org-gnosis-journal-dir))
	 (nodes (if journal-p
		    (org-gnosis-select 'id 'journal `(= file ,filename) t)
		  (org-gnosis-select 'id 'nodes `(= file ,filename) t))))
    (emacsql-with-transaction (org-gnosis-db-get)
      (cl-loop for node in nodes
	       do (if journal-p
		      (org-gnosis--delete 'journal `(= id ,node))
		    (org-gnosis--delete 'nodes `(= id ,node)))))))

(defun org-gnosis-tags--cleanup-orphaned ()
  "Remove orphaned tags that have no associated nodes.
Efficient: only checks tags not in node-tag table."
  (let* ((all-tags (org-gnosis-select 'tag 'tags nil t))
         (used-tags (org-gnosis-select 'tag 'node-tag nil t))
         (orphaned (cl-set-difference all-tags used-tags :test #'equal)))
    (when orphaned
      (dolist (tag orphaned)
        (emacsql (org-gnosis-db-get) [:delete :from tags :where (= tag $s1)] tag)))))

(defun org-gnosis-update-file (&optional file)
  "Update contents of FILE in database.

Removes all contents of FILE in database, adding them anew."
  (let* ((file (or file (file-name-nondirectory (buffer-file-name))))
	 (journal-p (file-in-directory-p file org-gnosis-journal-dir)))
    ;; Delete all contents for file
    (org-gnosis--delete-file file)
    ;; Reinsert them anew
    (org-gnosis--update-file file journal-p)
    ;; Update todos
    (when (and journal-p file)
      (let* ((today (format-time-string "%Y-%m-%d"))
             (parsed-buffer (org-element-parse-buffer))
             (done-todos (if (and org-gnosis-journal-file
                                  (string= file (file-name-nondirectory org-gnosis-journal-file)))
                             ;; For single journal file, only get items from today's heading
                             (let ((today-heading
                                    (org-element-map parsed-buffer 'headline
                                      (lambda (headline)
                                        (when (string= (org-element-property :raw-value headline) today)
                                          headline))
                                      nil t)))
                               (if today-heading
                                   (org-gnosis-get-checked-items today-heading)
                                 nil))
                           ;; For separate journal files, get all items
                           (org-gnosis-get-checked-items parsed-buffer))))
        (cl-loop for done-todo in done-todos
		 do (org-gnosis-mark-todo-as-done done-todo))))
    ;; Cleanup orphaned tags
    (org-gnosis-tags--cleanup-orphaned)))

;;;###autoload
(defun org-gnosis-delete-file (&optional file)
  "Delete FILE.

Delete file contents in database & file."
  (interactive)
  (let ((file (or file (file-name-nondirectory (buffer-file-name)))))
    (if (or (file-in-directory-p (buffer-file-name) org-gnosis-dir)
	    (file-in-directory-p (buffer-file-name) org-gnosis-journal-dir))
	(progn
	  (when (y-or-n-p (format "Delete file: %s?" file))
	    (org-gnosis--delete-file file)
	    (delete-file (buffer-file-name))
	    (kill-buffer (buffer-name))))
      (error "%s is not an org-gnosis file" file))))

(defun org-gnosis-find--tag-with-tag-prop (lst)
  "Combine each sublist of strings in LST into a single string."
  (mapcar (lambda (item)
            (let* ((title (car item))
                   (tags (cadr item))
                   (propertized-tags
		    (when tags
		      (let ((tag-list (if (stringp tags) (read tags) tags)))
			(when (and tag-list (not (equal tag-list '())))
                          (concat (propertize "#" 'face 'org-gnosis-face-tags)
                                  (propertize (mapconcat #'identity tag-list "#")
					      'face 'org-gnosis-face-tags)))))))
              (if propertized-tags
                  (format "%s  %s" title propertized-tags)
		title)))
          lst))

(defun org-gnosis--create-name (title &optional timestring gpg-p)
  "Create filename for TITLE.

TIMESTRING defaults to `org-gnosis-timestring'
GPG-P: when non-nil, add .gpg suffix (overrides `org-gnosis-create-as-gpg')."
  (let ((timestring (or timestring org-gnosis-timestring))
	(filename (replace-regexp-in-string "#" ""
					    (replace-regexp-in-string " " "_" title)))
	(use-gpg (if (eq gpg-p 'default)
		     org-gnosis-create-as-gpg
		   (or gpg-p org-gnosis-create-as-gpg))))
    (format "%s--%s.org%s" (format-time-string timestring) filename
	    (if use-gpg ".gpg" ""))))

(defun org-gnosis--create-file (title &optional directory extras)
  "Create a node FILE for TITLE.

Insert initial Org metadata if the buffer is new or empty.

DIRECTORY: Directory where the file is created.
EXTRAS: The template to be inserted at the start."
  (let* ((file (expand-file-name
		(org-gnosis--create-name
		 title nil
		 (and (eq directory org-gnosis-journal-dir)
		      org-gnosis-journal-as-gpg))
		(or directory org-gnosis-dir)))
	 (buffer (find-file-noselect file))
	 ;; `org-id-track-globally' can cause unexpected issues.
	 (org-id-track-globally nil))
    (with-current-buffer buffer
      (unless (or (file-exists-p file)
		  (> (buffer-size) 0))
	(insert (format "#+title: %s\n#+filetags: \n" title))
	(org-mode)
	(org-id-get-create)
	(when extras (insert extras))))
    (switch-to-buffer buffer)
    (org-gnosis-mode 1)))

(defun org-gnosis-find--with-tags (&optional prompt entries)
  "Select gnosis node with tags from ENTRIES.

PROMPT: Prompt message."
  (replace-regexp-in-string
   "  #[^[:space:]]+" ""
   (funcall org-gnosis-completing-read-func (or prompt "Select gnosis node: ")
	    (org-gnosis-find--tag-with-tag-prop
	     (or entries (org-gnosis-select '[title tags] 'nodes))))))

(defun org-gnosis--find (prompt entries-with-tags entries)
  "PROMPT user to select from ENTRIES.

If `org-gnosis-show-tags' is non-nil, ENTRIES-WITH-TAGS will be used
instead."
  (let* ((entry (if org-gnosis-show-tags
                    (org-gnosis-find--with-tags prompt entries-with-tags)
                  (funcall org-gnosis-completing-read-func prompt entries))))
    entry))

;;;###autoload
(defun org-gnosis-find (&optional title file id directory templates)
  "Select gnosis node.

If there is no ID for TITLE, create a new FILE with TITLE as TOPIC in
DIRECTORY."
  (interactive)
  (org-gnosis-db-rebuild)
  (let* ((title (or title (if org-gnosis-show-tags
			      (org-gnosis-find--with-tags)
			    (funcall org-gnosis-completing-read-func
				     "Select gnosis node: "
				     (org-gnosis-select 'title 'nodes)))))
	 (file (or file (caar (org-gnosis-select 'file 'nodes `(= title ,title)))))
	 (id (or id (caar (or id (org-gnosis-select 'id 'nodes `(= title ,title))))))
	 (directory (or directory org-gnosis-dir))
	 (templates (or templates org-gnosis-node-templates)))
    (cond ((null file)
	   (org-gnosis--create-file title directory
				    (org-gnosis-select-template templates)))
	  ((file-exists-p (expand-file-name file directory))
	   (org-gnosis-goto-id id))
	  (t (error
	      "File %s does not exist.  Try running `org-gnosis-db-sync' to resolve this"
	      file)))))

(defun org-gnosis--nodes-by-tag (tag)
  "Return all nodes associated with TAG.
Returns a list of (ID) pairs for nodes that have TAG."
  (org-gnosis-select 'id 'nodes `(like tags ',(format "%%\\\"%s\\\"%%" tag)) t))

;;;###autoload
(defun org-gnosis-find-by-tag (&optional tag)
  "Find node under TAG."
  (interactive)
  (let* ((tag (or tag (funcall org-gnosis-completing-read-func
			       "Select tag: "
			       (org-gnosis-select 'tag 'tags nil t))))
	 (nodes-ids (org-gnosis--nodes-by-tag tag))
	 (node-titles (org-gnosis-select 'title 'nodes `(in id ,(vconcat nodes-ids)) t))
	 (node (completing-read "Select node: " node-titles nil t)))
    (org-gnosis-find node)))

(defun org-gnosis-select-template (templates)
  "Select journal template from TEMPLATES.

If templates is only item, return it without a prompt."
  (let* ((template (if (= (length templates) 1)
                       (cdar templates)
                     (cdr (assoc
			   (funcall org-gnosis-completing-read-func "Select template:"
                                    (mapcar #'car templates))
                           templates)))))
    (funcall (apply #'append template))))

;;;###autoload
(defun org-gnosis-insert (arg &optional journal-p)
  "Insert gnosis node.

If called with ARG, prompt for custom link description.
If JOURNAL-P is non-nil, retrieve/create node as a journal entry."
  (interactive "P")
  (let* ((table (if journal-p 'journal 'nodes))
         (node (org-gnosis--find "Select gnosis node: "
                                 (org-gnosis-select '[title tags] table)
                                 (org-gnosis-select 'title table)))
         (id (car (org-gnosis-select 'id table `(= ,node title) t)))
	 (title (car (last (split-string node ":"))))
         (desc (cond ((use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end)))
                     (arg (read-string "Description: "))
                     (t title))))
    (unless id
      (save-window-excursion
        (org-gnosis--create-file node (if journal-p org-gnosis-journal-dir org-gnosis-dir))
        (save-buffer)
        (setf id (car (org-gnosis-select 'id table `(= ,node title) t)))))
    (org-insert-link nil (format "id:%s" id) desc)
    (unless id (message "Created new node: %s" node))))

(defun org-gnosis-insert-filetag (&optional tag)
  "Insert TAG as filetag."
  (interactive)
  (let* ((filetags (org-gnosis-select 'tag 'tags nil t))
         (tag (or tag (funcall org-gnosis-completing-read-func "Select tag: " filetags))))
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
(defun org-gnosis-insert-tags (tags)
  "Insert TAGS as filetags."
  (interactive
   (list (completing-read-multiple
	  "Select tags (separated by ,): "
	  (org-gnosis-select 'tag 'tags nil t))))
  (let ((id (and (org-gnosis-get-id))))
    (org-id-goto id)
    (if (org-current-level)
	(org-set-tags tags)
      (dolist (tag tags)
	(org-gnosis-insert-filetag tag)))))

;;;###autoload
(defun org-gnosis-visit-backlinks ()
  "Visit backlinks for current node."
  (interactive)
  (let* ((id (org-gnosis-get-id))
	 (backlinks (org-gnosis-select '[source] 'links `(= dest ,id)))
	 (titles (cl-loop for backlink in backlinks
			  for source-id = (car backlink)
			  for title = (caar (org-gnosis-select 'title 'nodes
							       `(= id ,source-id)))
			  when title collect title)))
    (if titles
	(org-gnosis-find
	 (completing-read "Backlink: " titles))
      (message "No backlinks found for current node"))))

(defun org-gnosis-get-nodes-data (&optional node-ids)
  "Fetch node data for NODE-IDS or all nodes if not specified.

Returns a list of (ID TITLE BACKLINK-COUNT) for each node."
  (let* ((nodes (if node-ids
                    (org-gnosis-select '[id title] 'nodes
                                       `(in id ,(vconcat node-ids)))
                  (org-gnosis-select '[id title] 'nodes)))
         (backlinks (if node-ids
                        (org-gnosis-select '[dest source] 'links
                                           `(in dest ,(vconcat node-ids)))
                      (org-gnosis-select '[dest source] 'links)))
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

;;;###autoload
(defun org-gnosis-journal-find (&optional title)
  "Find journal entry for TITLE."
  (interactive)
  (let* ((title (or title (org-gnosis--find
			   "Select journal entry: "
			   (org-gnosis-select '[title tags] 'journal)
			   (org-gnosis-select 'title 'journal))))
	 (id (car (org-gnosis-select 'id 'journal `(= title ,title) t)))
	 (file (car (org-gnosis-select 'file 'journal `(= title ,title) t))))
    (cond
     ((and org-gnosis-journal-file (not id))
      (org-gnosis-journal--add-entry title))
     ((and id file)
      (org-gnosis-find
       title file id org-gnosis-journal-dir org-gnosis-journal-templates))
     (t
      (org-gnosis--create-file
       title org-gnosis-journal-dir
       (org-gnosis-select-template org-gnosis-journal-templates))))))

;;;###autoload
(defun org-gnosis-journal-insert (arg)
  "Insert journal entry.

If called with prefix ARG, use custom link description."
  (interactive "P")
  (org-gnosis-insert arg t))

;;;###autoload
(defun org-gnosis-journal ()
  "Journal for current date."
  (interactive)
  (let* ((date (format-time-string "%Y-%m-%d")))
    (org-gnosis-journal-find date)))

(defun org-gnosis--get-id-at-point ()
  "Return the Org ID link at point, if any."
  (let* ((element (org-element-context))
         (id-link (when (and (eq (org-element-type element) 'link)
                             (string= (org-element-property :type element) "id"))
                    (org-element-property :path element))))
    id-link))

(defun org-gnosis-goto-id (&optional id)
  "Visit file for ID.

If file or id are not found, use `org-open-at-point'."
  (interactive)
  (let* ((id (or id (org-gnosis--get-id-at-point)))
	 (org-id-track-globally nil))
    (cond ((org-gnosis-select 'file 'nodes `(= id ,id))
	   (find-file
	    (expand-file-name (car (org-gnosis-select 'file 'nodes `(= id ,id) t))
			      org-gnosis-dir))
	   (org-id-goto id))
	  ((org-gnosis-select 'file 'journal `(= id ,id))
	   (find-file
	    (expand-file-name (car (org-gnosis-select 'file 'journal `(= id ,id) t))
			      org-gnosis-journal-dir))
	   (org-id-goto id))
	  (t (org-open-at-point)))
    (org-gnosis-mode 1)))

(defun org-gnosis-get--todos (file)
  "Get TODO items for FILE."
  (let ((todos))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (headline)
          (when (member (org-element-property :todo-keyword headline)
			(cl-loop for keyword in org-gnosis-todo-keywords
				 until (and (stringp keyword) (string= keyword "|"))
				 collect keyword))
            (let* ((title (org-element-property :raw-value headline))
                   (timestamp (org-element-property
			       :raw-value (org-element-property :scheduled headline))))
              (push `(,title ,timestamp ,file) todos))))))
    (nreverse todos)))

(defun org-gnosis-find-file-with-heading (title files)
  "Find first org file in FILES containing heading TITLE."
  (catch 'found
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (goto-char (point-min))
        (when (org-find-exact-headline-in-buffer title)
          (throw 'found file))))))

(defun org-gnosis-get-todos (&optional files)
  "Get TODO items for FILES.

If TITLE is non-nil, return the file that has a TODO TITLE."
  (let ((files (or files org-gnosis-todo-files))
	todos)
    (cl-loop for file in files
	     do (push (org-gnosis-get--todos file) todos))
    (nreverse (apply #'append todos))))

(defun org-gnosis-todos ()
  "Output todos as checkboxes in a string for current date."
  (let ((todos (org-gnosis-get-todos))
	(current-date (format-time-string "%Y-%m-%d"))
	todos-string)
    (cl-loop for todo in todos
	     do
	     (let ((todo-title (car todo))
		   (todo-timestamp (cadr todo)))
	       (when (or
		      (null todo-timestamp)
		      (string-match-p (regexp-quote current-date) todo-timestamp))
		 (setq todos-string
		       (concat todos-string
			       (format "%s [ ] %s\n" org-gnosis-bullet-point-char
				       todo-title))))))
    (or todos-string "")))

(defun org-gnosis-get-checked-items (element)
  "Get checked items for org ELEMENT.

ELEMENT should be the output of `org-element-parse-buffer'."
  (let ((checked-items))
    (org-element-map element 'item
      (lambda (item)
        (when (eq (org-element-property :checkbox item) 'on)
          (push (car (split-string
                      (substring-no-properties
                       (string-trim
			(org-element-interpret-data
                         (org-element-contents item))))
                      "\n"))
                checked-items))))
    (nreverse checked-items)))

(defun org-gnosis-mark-todo-as-done (todo-title)
  "Mark scheduled TODO with TODO-TITLE as DONE if not already done today."
  (let* ((file (org-gnosis-find-file-with-heading todo-title org-gnosis-todo-files))
         (today (format-time-string "%Y-%m-%d")))
    (when file
      (save-current-buffer
        (with-current-buffer (find-file-noselect file)
          (let ((found nil))
            (save-excursion
              (org-element-map (org-element-parse-buffer) 'headline
                (lambda (headline)
                  (when (and (not found)
                             (string= (org-element-property :raw-value headline)
                                      todo-title)
                             (string= (org-element-property :todo-keyword headline)
                                      "TODO")
			     ;; Check if not done today
                             (not (org-entry-get (org-element-property :begin headline)
						 "LAST_DONE_DATE")))
                    (org-with-point-at (org-element-property :begin headline)
                      (org-todo 'done)
                      (org-entry-put nil "LAST_DONE_DATE" today))
                    (setq found t))))))
          (save-buffer))))))

(defvar-keymap org-gnosis-mode-map
  :doc "org-gnosis keymap"
  "C-c C-o" #'org-gnosis-goto-id)

(define-minor-mode org-gnosis-mode
  "Org gnosis mode."
  :lighter " org-gnosis"
  :keymap org-gnosis-mode-map
  :global nil
  :group 'org-gnosis
  (if org-gnosis-mode
      (add-hook 'after-save-hook #'org-gnosis-update-file nil t) ;; buffer local hook
    (remove-hook 'after-save-hook #'org-gnosis-update-file)))

;; Org-Gnosis Database

;; Org-Gnosis Database

(defun org-gnosis--file-hash (file)
  "Compute SHA1 hash of FILE content."
  (with-temp-buffer
    (insert-file-contents file)
    (secure-hash 'sha1 (current-buffer))))

(defconst org-gnosis-db-version 3)

(defconst org-gnosis-db--table-schemata
  '((nodes
     ([(id :not-null :primary-key)
       (file :not-null)
       (title text :not-null)
       (level text :not-null)
       tags
       mtime
       hash]))
    (tags
     ([(tag text :primary-key)]
      (:unique [tag])))
    (journal
     ([(id :not-null :primary-key)
       (file :not-null)
       (title text :not-null)
       (level text :not-null)
       tags
       mtime
       hash]))
    (node-tag
     ([(node-id :not-null)
       (tag :not-null)]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)
      (:unique [node-id tag])))
    (links
     ([(source text)
       (dest text)]
      (:foreign-key [source] :references nodes [id] :on-delete :cascade)
      (:unique [source dest])))))

(defun org-gnosis-db-delete-tables ()
  "Drop all tables."
  (ignore-errors
    (emacsql-with-transaction (org-gnosis-db-get)
      ;; Maybe use sql for version upgrades that change schemata?
      (dolist (table (mapcar #'car org-gnosis-db--table-schemata))
	(org-gnosis--drop-table table)))))

(defun org-gnosis-db-sync--journal (&optional force)
  "Sync journal entries in database.
When FORCE, update all files.  Otherwise, only update changed files."
  (let* ((journal-files (cl-remove-if-not
                         (lambda (file)
                           (and (string-match-p "^[0-9]"
                                                (file-name-nondirectory file))
                                (not (file-directory-p file))))
                         (directory-files org-gnosis-journal-dir t nil t)))
         ;; Add single journal file if it exists
         (all-files (if (and org-gnosis-journal-file
                             (file-exists-p org-gnosis-journal-file))
                        (cons org-gnosis-journal-file journal-files)
                      journal-files))
         (files (if force
                    all-files
                  (cl-remove-if-not
                   (lambda (file) (org-gnosis--file-changed-p file 'journal))
                   all-files))))
    (when (> (length files) 0)
      (let ((progress (make-progress-reporter
                       (format "Processing %d/%d journal files..." (length files) (length all-files))
                       0 (length files))))
        (cl-loop for file in files
                 for i from 0
                 do (progn
                      (org-gnosis-update-file file)
                      (progress-reporter-update progress i)))
        (progress-reporter-done progress)))))

;;;###autoload
(defun org-gnosis-db-sync (&optional force)
  "Sync `org-gnosis-db' with progress reporting.

When FORCE (prefix arg), rebuild database from scratch."
  (interactive "P")
  (let ((gc-cons-threshold most-positive-fixnum)) ; Optimize GC during sync
    (org-gnosis-ensure-directories)
    (when force
      ;; Close connection and delete database file for full rebuild
      (when (and org-gnosis-db--connection
                 (emacsql-live-p org-gnosis-db--connection))
        (emacsql-close org-gnosis-db--connection)
        (setq org-gnosis-db--connection nil))
      (when (file-exists-p org-gnosis-database-file)
        (delete-file org-gnosis-database-file)))
    (org-gnosis-db-init-if-needed)
    (message "Syncing org-gnosis database...")
    (emacsql-with-transaction (org-gnosis-db-get)
      (when force
        ;; Full rebuild: drop and recreate tables
        (org-gnosis-db-delete-tables)
        (pcase-dolist (`(,table ,schema) org-gnosis-db--table-schemata)
          (emacsql (org-gnosis-db-get) [:create-table $i1 $S2] table schema)))
      ;; Sync all files with progress reporting
      (org-gnosis-db-update-files force)
      ;; Set current version
      (emacsql (org-gnosis-db-get)
               `[:pragma (= user-version ,org-gnosis-db-version)]))
    (message "Database sync complete!")))

(defun org-gnosis--file-changed-p (file table)
  "Check if FILE changed since last sync using mtime then hash.
TABLE is either \\='nodes or \\='journal."
  (let* ((filename (file-name-nondirectory file))
         (file-mtime (format-time-string "%s" (file-attribute-modification-time (file-attributes file))))
         (db-data (car (org-gnosis-select '[mtime hash] table `(= file ,filename))))
         (db-mtime (car db-data))
         (db-hash (cadr db-data)))
    (or (not db-mtime) ; New file
        (and (not (string= file-mtime db-mtime)) ; Mtime changed
             (not (string= (org-gnosis--file-hash file) db-hash))))))  ; AND hash changed

(defun org-gnosis-db-update-files (&optional force)
  "Sync `org-gnosis-db' files with progress reporting.
When FORCE, update all files.  Otherwise, only update changed files."
  (org-gnosis-db-init-if-needed)
  (let* ((all-files (cl-remove-if-not
                     (lambda (file)
                       (and (string-match-p "^[0-9]"
                                            (file-name-nondirectory file))
                            (not (file-directory-p file))))
                     (directory-files org-gnosis-dir t nil t)))
         (files (if force
                    all-files
                  (cl-remove-if-not
                   (lambda (file) (org-gnosis--file-changed-p file 'nodes))
                   all-files))))
    (if (zerop (length files))
        (message "No files to sync")
      ;; Process files with progress reporter
      (let ((progress (make-progress-reporter
                       (format "Processing %d/%d files..." (length files) (length all-files))
                       0 (length files))))
        (cl-loop for file in files
                 for i from 0
                 do (progn
                      (org-gnosis-update-file file)
                      (progress-reporter-update progress i)))
        (progress-reporter-done progress))))
  ;; Sync journal files
  (message "Syncing journal files...")
  (org-gnosis-db-sync--journal force)
  (message "File sync complete"))

;;;###autoload
(defun org-gnosis-db-force-sync ()
  "Force rebuild database from files.
Unconditionally rebuilds the entire database by dropping all tables
and re-syncing all files."
  (interactive)
  (when (y-or-n-p "Force rebuild database from files?")
    (org-gnosis-db-sync 'force)))

(defun org-gnosis-db-rebuild ()
  "Rebuild database if version is outdated.
Checks database version and prompts user for rebuild if needed."
  (let ((current-version (caar (emacsql (org-gnosis-db-get) [:pragma user-version]))))
    (when (and (< current-version org-gnosis-db-version)
	       (y-or-n-p
		(format
		 "Database version %d is outdated (current: %d).  Rebuild database from files? "
		 current-version org-gnosis-db-version)))
      (org-gnosis-db-sync 'force))))

(defun org-gnosis-db-init ()
  "Initialize database.

Create all tables and set version for new database."
  (message "Creating new org-gnosis database...")
  (emacsql-with-transaction (org-gnosis-db-get)
    (pcase-dolist (`(,table ,schema) org-gnosis-db--table-schemata)
      (emacsql (org-gnosis-db-get) [:create-table $i1 $S2] table schema))
    (emacsql (org-gnosis-db-get) [:pragma (= user-version org-gnosis-db-version)])))

(defun org-gnosis-db-init-if-needed ()
  "Init database if it has not been initialized."
  (let ((tables (emacsql (org-gnosis-db-get)
			 [:select name :from sqlite-master :where (= type 'table)])))
    (when (< (length tables) 3)
      (message "Creating org-gnosis database...")
      (org-gnosis-db-init))))

(provide 'org-gnosis)
;;; org-gnosis.el ends here
