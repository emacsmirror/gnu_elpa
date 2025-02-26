;;; org-gnosis.el --- Roam-like Knowledge Management System  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/org-gnosis/
;; Version: 0.0.7

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

;; Org Gnosis is a knowledge management tool that leverages Org mode
;; for storing notes & journal entries, integrating them with an
;; SQLite database for efficient retrieval and relationship mapping.

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
  '(("Default" (lambda () (format "* Daily Notes\n\n* Goals\n%s" (org-gnosis-todos))))
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

(defcustom org-gnosis-show-tags nil
  "Display tags with `org-gnosis-find'."
  :type 'boolean)

(defcustom org-gnosis-timestring "%Y%m%d%H%M%S"
  "Timestring used for the creation of file."
  :type 'string)

(defcustom org-gnosis-create-as-gpg nil
  "When non-nil, create notes with a .gpg suffix."
  :type 'boolean)

(defcustom org-gnosis-todo-files org-agenda-files
  "TODO files used for the journal entries."
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

(defvar org-gnosis-db (emacsql-sqlite-open (locate-user-emacs-file "org-gnosis.db")))

;; Create notes & journal directories.
(dolist (dir `(,org-gnosis-dir ,org-gnosis-journal-dir))
  (unless (file-directory-p dir)
    (make-directory dir)))

(defun org-gnosis-select (value table &optional restrictions flatten)
  "Select VALUE from TABLE, optionally with RESTRICTIONS.

Optional argument FLATTEN, when non-nil, flattens the result."
  (org-gnosis-db-init-if-needed) ;; Init database if needed
  (let* ((restrictions (or restrictions '(= 1 1)))
	 (flatten (or flatten nil))
	 (output (emacsql org-gnosis-db
			  `[:select ,value :from ,table :where ,restrictions])))
    (if flatten (apply #'append output) output)))

(defun org-gnosis--insert-into (table values)
  "Insert VALUES to TABLE."
  (emacsql org-gnosis-db `[:insert :into ,table :values ,values]))

(defun org-gnosis--delete (table value)
  "From TABLE use where to delete VALUE."
  (emacsql org-gnosis-db `[:delete :from ,table :where ,value]))

(defun org-gnosis--drop-table (table)
  "Drop TABLE from `gnosis-db'."
  (emacsql org-gnosis-db `[:drop-table ,table]))

(defun org-gnosis-adjust-title (input &optional node-id)
  "Adjust the INPUT string to replace id link structures with plain text.

If node title contains an id link, it's inserted as link for NODE-ID
in the database."
  (when (stringp input)
    (let* ((id-links '())
	   (new-input (replace-regexp-in-string
                       "\\[\\[id:[^]]+\\]\\[\\(.*?\\)\\]\\]"
                       (lambda (match)
                         (push (match-string 1 match) id-links)
                         (match-string 1 match))
                       input)))
      (when (and node-id id-links)
	(emacsql-with-transaction org-gnosis-db
	  (cl-loop for link in (reverse id-links)
		   do (org-gnosis--insert-into 'links `([,node-id ,link])))))
      new-input)))

(defun org-gnosis-parse-headline (headline inherited-tags topic-id)
  "Parse a HEADLINE and return a plist with its info.

INHERITED-TAGS: Upper level headline tags.
TOPIC-ID: Topic hash id."
  (let* ((title (org-element-property :raw-value headline))
         (id (org-element-property :ID headline))
         (level (org-element-property :level headline))
         (tags (or (org-element-property :tags headline) inherited-tags))
         (all-tags (delete-dups (append inherited-tags tags)))
         (master (if (= level 1) topic-id
                   (org-element-property :ID (org-element-property :parent headline)))))
    (and id
         (list :title title :id id :tags all-tags :master master :level level))))

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
            (push (cons (match-string 1 link) (org-gnosis-get-id)) links)))))
    (nreverse links)))

(defun org-gnosis-get-data--topic (&optional parsed-data)
  "Retrieve the title and ID from the current org buffer or given PARSED-DATA."
  (let* ((parsed-data (or parsed-data (org-element-parse-buffer)))
         (id (org-element-map parsed-data 'property-drawer
               (lambda (drawer)
                 (org-element-map (org-element-contents drawer) 'node-property
                   (lambda (prop)
                     (when (string= (org-element-property :key prop) "ID")
                       (org-element-property :value prop)))
                   nil t))
               nil t))
	 (title (org-gnosis-adjust-title
		 (org-element-map parsed-data 'keyword
                  (lambda (kw)
                    (when (string= (org-element-property :key kw) "TITLE")
                      (org-element-property :value kw)))
                  nil t)
		 id))
	 (tags (org-gnosis-get-filetags)))
    (list title tags id)))

;; This one is used for topics
(defun org-gnosis-get-filetags (&optional parsed-data)
  "Return the filetags of the buffer's PARSED-DATA as a comma-separated string."
  (let* ((parsed-data (or parsed-data (org-element-parse-buffer)))
         (filetags (org-element-map parsed-data 'keyword
                     (lambda (kw)
                       (when (string-equal (org-element-property :key kw) "FILETAGS")
                         (org-element-property :value kw)))
                     nil t)))
    (and filetags (remove "" (split-string filetags ":")))))

(defun org-gnosis-parse-topic (parsed-data)
  "Parse topic information from the PARSED-DATA."
  (let* ((topic-info (org-gnosis-get-data--topic parsed-data))
         (topic-title (nth 0 topic-info))
         (topic-tags (nth 1 topic-info))
         (topic-id (nth 2 topic-info)))
    (when topic-id
      (list :title topic-title
	    :id topic-id :tags topic-tags :master 0 :level 0))))

(defun org-gnosis-buffer-data (&optional data)
  "Parse DATA in current buffer for topics & headlines with their ID, tags, links."
  (let* ((parsed-data (or data (org-element-parse-buffer)))
         (topic (org-gnosis-parse-topic parsed-data))
         (all-ids (when topic (list (plist-get topic :id))))
         (inherited-tags (plist-get topic :tags))
         (headlines '()))
    (org-element-map parsed-data 'headline
      (lambda (headline)
        (let ((parsed-headline (org-gnosis-parse-headline
				headline inherited-tags (plist-get topic :id))))
          (when parsed-headline
            (push parsed-headline headlines)
            (push (plist-get parsed-headline :id) all-ids)))))
    (nreverse (cons topic headlines))))

(defun org-gnosis-get-file-info (filename)
  "Get data for FILENAME.

Returns file data with FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (org-mode)
    (let* ((data (org-gnosis-buffer-data))
	   (links (org-gnosis-collect-id-links)))
      ;; Append links even if they are nil
      (append data (list links)))))

(defun org-gnosis--update-file (file &optional journal)
  "Add contents of FILE to database.

If JOURNAL is non-nil, update file as a journal entry."
  (let* ((info (org-gnosis-get-file-info file))
	 (data (butlast info))
	 (table (if journal 'journal 'nodes))
	 (filename (file-name-nondirectory file))
	 (links (and (> (length info) 1) (apply #'append (last info))))
	 (titles (org-gnosis-select 'title table nil t)))
    ;; Add gnosis topic
    (message "Parsing: %s" filename)
    (cl-loop for item in data
	     do (let ((title (org-gnosis-adjust-title
			      (plist-get item :title)))
		      (id (plist-get item :id))
		      (master (plist-get item :master))
		      (tags (plist-get item :tags))
		      (level (plist-get item :level)))
		  (when (member title titles)
		    (error "Title: '%s' already exists" title))
		  (org-gnosis--insert-into table `([,id ,filename ,title ,level ,tags]))
		  (cl-loop for tag in tags
			   do
			   (org-gnosis--insert-into 'tags `([,tag]))
			   (org-gnosis--insert-into 'node-tag `([,id ,tag]))
			   (when (stringp master)
			     (org-gnosis--insert-into 'links `([,id ,master]))))))
    (cl-loop for link in links
	     do (org-gnosis--insert-into 'links `[,(cdr link) ,(car link)]))))

(defun org-gnosis--delete-file (&optional file)
  "Delete contents for FILE in database."
  (let* ((file (or file (file-name-nondirectory (buffer-file-name))))
	 (journal-p (file-in-directory-p file org-gnosis-journal-dir))
	 (nodes (if journal-p
		    (org-gnosis-select 'id 'journal `(= file ,file) t)
		  (org-gnosis-select 'id 'nodes `(= file ,file) t))))
    (emacsql-with-transaction org-gnosis-db
      (cl-loop for node in nodes
	       do (if journal-p
		      (org-gnosis--delete 'journal `(= id ,node))
		    (org-gnosis--delete 'nodes `(= id ,node)))))))

(defun org-gnosis-update-file (&optional file)
  "Update contents of FILE in databse.

Removes all contents of FILE in database, adding them anew."
  (let* ((file (or file (file-name-nondirectory (buffer-file-name))))
	 (journal-p (file-in-directory-p file org-gnosis-journal-dir)))
    ;; Delete all contents for file
    (org-gnosis--delete-file file)
    ;; Reinsert them anew
    (org-gnosis--update-file file journal-p)
    ;; Update todos
    (when (and journal-p file)
      (let ((done-todos (org-gnosis-get-checked-items (org-element-parse-buffer))))
        (cl-loop for done-todo in done-todos
		 do (org-gnosis-mark-todo-as-done done-todo))))))

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
                      (concat (propertize "#" 'face 'org-gnosis-face-tags)
                              (propertize (mapconcat #'identity tags "#")
					  'face 'org-gnosis-face-tags)))))
              (if propertized-tags
                  (format "%s  %s" title propertized-tags)
		title)))
          lst))

(defun org-gnosis--create-name (title &optional timestring)
  "Create filename for TITLE.

TIMESTRING defaults to `org-gnosis-timestring'"
  (let ((timestring (or timestring org-gnosis-timestring))
	(filename (replace-regexp-in-string "#" ""
					    (replace-regexp-in-string " " "_" title))))
    (format "%s--%s.org%s" (format-time-string timestring) filename
	    (if org-gnosis-create-as-gpg ".gpg" ""))))
;; TODO: Add filetags
(defun org-gnosis--create-file (title &optional directory extras)
  "Create a node FILE for TITLE.

Insert initial Org metadata if the buffer is new or empty.

DIRECTORY: Directory where the file is created.
EXTRAS: The template to be inserted at the start."
  (let* ((file (expand-file-name
		(org-gnosis--create-name title)
		(or directory org-gnosis-dir)))
	 (buffer (find-file-noselect file)))
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

;;;###autoload
(defun org-gnosis-find-by-tag (&optional tag)
  "Find node under TAG."
  (interactive)
  (let* ((tag (or tag (funcall org-gnosis-completing-read-func
			       "Select tag: "
			       (org-gnosis-select 'tag 'tags nil t))))
	 (node
	  (funcall org-gnosis-completing-read-func
		   "Select node: "
		   (org-gnosis-select 'title 'nodes
				      `(like tags ',(format "%%\"%s\"%%" tag)) t))))
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
	 (id (concat "id:" (car (org-gnosis-select 'id table `(= ,node title) t)))))
    (cond ((< (length id) 4) ; if less that 4 then `org-gnosis-select' returned nil, (id:)
	   (save-window-excursion
	     (org-gnosis--create-file
	      node (if journal-p org-gnosis-journal-dir org-gnosis-dir))
	     ;; Save buffer to store new node id
	     (save-buffer)
	     (setf id (concat
		       "id:"
		       (car (org-gnosis-select 'id table `(= ,node title) t)))))
	   (org-insert-link nil id node)
	   (message "Created new node: %s" node))
	  (t (org-insert-link nil id (if arg (read-string "Description: ") node))))))


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
	 (links (org-gnosis-select 'source 'links `(= dest ,id) t))
	 (titles (cl-loop for link in links
			  collect (org-gnosis-select 'title 'nodes `(= id ,link) t))))
    (org-gnosis-find
     (completing-read "Backlink: " (apply #'append titles)))))

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
    (if (and id file)
	(org-gnosis-find
	 title file id org-gnosis-journal-dir org-gnosis-journal-templates)
      (org-gnosis--create-file
       title org-gnosis-journal-dir
       (org-gnosis-select-template org-gnosis-journal-templates)))))

;;;###autoload
(defun org-gnosis-journal-insert ()
  "Insert journal entry."
  (interactive)
  (org-gnosis-insert t))

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
  (let* ((id (or id (org-gnosis--get-id-at-point))))
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

;; Should we use `org-get'?
(defun org-gnosis-get--todos (file)
  "Get TODO items for FILE."
  (let ((todos))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (headline)
          (when (string= (org-element-property :todo-keyword headline) "TODO")
            (let* ((title (org-element-property :raw-value headline))
                   (timestamp (org-element-property :raw-value
                           (org-element-property :scheduled headline))))
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
  "Mark scheduled TODO with TODO-TITLE as DONE if not already done today.
ENTRY: Journal entry linked under the heading."
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

(defconst org-gnosis-db-version 1)

(defconst org-gnosis-db--table-schemata
  '((nodes
     ([(id :not-null :primary-key)
       (file :not-null)
       (title text :not-null)
       (level text :not-null)
       tags]))
    (tags
     ([(tag text :primary-key)]
      (:unique [tag])))
    (journal
     ([(id :not-null :primary-key)
       (file :not-null)
       (title text :not-null)
       (level text :not-null)
       tags]))
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
    (emacsql-with-transaction org-gnosis-db
      ;; Maybe use sql for version upgrades that change schemata?
      (dolist (table (mapcar #'car org-gnosis-db--table-schemata))
	(org-gnosis--drop-table table)))))

(defun org-gnosis-db-sync--journal ()
  "Sync journal entries in databse."
  (cl-loop for file in (cl-remove-if-not
			(lambda (file)
			  (and
			   (string-match-p "^[0-9]"
					   (file-name-nondirectory file))
			   (not (file-directory-p file))))
			(directory-files org-gnosis-journal-dir t nil t))
	   do (org-gnosis-update-file file)))

;;;###autoload
(defun org-gnosis-db-sync ()
  "Sync `org-gnosis-db'."
  (interactive)
  (org-gnosis-db-init)
  (let ((files (cl-remove-if-not
		(lambda (file)
		  (and (string-match-p "^[0-9]"
				       (file-name-nondirectory file))
		       (not (file-directory-p file))))
		(directory-files org-gnosis-dir t nil t))))
    (cl-loop for file in files
	     do (org-gnosis-update-file file)))
  (org-gnosis-db-sync--journal))

(defun org-gnosis-db-init ()
  "Initialize database.

If database tables exist, delete them & recreate the db."
  (org-gnosis-db-delete-tables)
  (when (length< (emacsql org-gnosis-db
			  [:select name :from sqlite-master :where (= type 'table)])
		 3)
    (emacsql-with-transaction org-gnosis-db
      (pcase-dolist (`(,table ,schema) org-gnosis-db--table-schemata)
	(emacsql org-gnosis-db [:create-table $i1 $S2] table schema))
      (emacsql org-gnosis-db [:pragma (= user-version org-gnosis-db-version)]))))

(defun org-gnosis-db-init-if-needed ()
  "Init database if it has not been initizalized."
  (when (length< (emacsql org-gnosis-db
			  [:select name :from sqlite-master :where (= type 'table)])
		 4)
    (message "Creating org-gnosis database...")
    (org-gnosis-db-init)))

(provide 'org-gnosis)
;;; org-gnosis.el ends here
