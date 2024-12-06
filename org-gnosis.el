;;; org-gnosis.el --- Org Note Management System  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis
;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2") (emacsql "4.0.3") (compat "29.1.4.2"))

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

;; Under development

;;; Code:

(require 'cl-lib)
(require 'emacsql-sqlite)
(require 'org-element)

(defgroup org-gnosis nil
  "Note Taking System."
  :group 'external)

(defcustom org-gnosis-dir "~/Notes"
  "Directory with gnosis notes."
  :type 'directory
  :group 'org-gnosis)

(defcustom org-gnosis-journal-template
  "* Daily Notes\n\n* Goals\n+ []"
  "Template for journaling."
  :type 'string
  :group 'org-gnosis)

(defcustom org-gnosis-journal-dir (expand-file-name "journal" org-gnosis-dir)
  "Gnosis journal directory."
  :type 'directory
  :group 'org-gnosis)

(defcustom org-gnosis-show-tags nil
  "Display tags with `org-gnosis-find'."
  :type 'boolean
  :group 'org-gnosis)

(defcustom org-gnosis-completing-read-func
  (cond ((or (bound-and-true-p ivy-mode)
	     (bound-and-true-p helm-mode)
	     (bound-and-true-p vertico-mode)
	     (bound-and-true-p fido-mode))
	 #'completing-read)
	(t #'ido-completing-read))
  "Function to use for `completing-read'."
  :type 'function
  :group 'gnosis)

(defface org-gnosis-face-tags
  '((t :inherit font-lock-type-face))
  "Face for displaying gnosis with `org-gnosis-find'."
  :group 'org-gnosis)

(defvar org-gnosis-db (emacsql-sqlite-open (locate-user-emacs-file "org-gnosis.db")))

(cl-defun org-gnosis-select (value table &optional (restrictions '1=1) (flatten nil))
  "Select VALUE from TABLE, optionally with RESTRICTIONS.

Optional argument FLATTEN, when non-nil, flattens the result."
  (let ((output (emacsql org-gnosis-db `[:select ,value :from ,table :where ,restrictions])))
    (if flatten
	(apply #'append output)
      output)))

(cl-defun org-gnosis--insert-into (table values)
  "Insert VALUES to TABLE."
  (emacsql org-gnosis-db `[:insert :into ,table :values ,values]))

(defun org-gnosis--delete (table value)
  "From TABLE use where to delete VALUE."
  (emacsql org-gnosis-db `[:delete :from ,table :where ,value]))

(cl-defun org-gnosis--drop-table (table)
  "Drop TABLE from `gnosis-db'."
  (emacsql org-gnosis-db `[:drop-table ,table]))

(defun org-gnosis-get-current-node-title ()
  "Return the title of the current node."
  (when (derived-mode-p 'org-mode)
    (let* ((parsed-data (org-element-parse-buffer))
           (title (org-element-map parsed-data 'keyword
                    (lambda (kw)
                      (when (string-equal (org-element-property :key kw) "TITLE")
                        (org-element-property :value kw)))
                    nil t)))
      title)))

(defun org-gnosis-get-filetags (&optional parsed-data)
  "Return the filetags of the buffer's PARSED-DATA as a comma-separated string."
  (let* ((parsed-data (or parsed-data (org-element-parse-buffer)))
         (filetags (org-element-map parsed-data 'keyword
                     (lambda (kw)
                       (when (string-equal (org-element-property :key kw) "FILETAGS")
                         (org-element-property :value kw)))
                     nil t)))
    (and filetags (remove "" (split-string filetags ":")))))

(defun org-gnosis-get-links (contents)
  "Recursively collect all node id links from CONTENTS."
  (org-element-map contents 'link
    (lambda (link)
      (when (string-equal "id" (org-element-property :type link))
        (org-element-property :path link)))))

(defun org-gnosis-process-node (node)
  "Process a single headline NODE and return information as a list."
  (let ((title (org-element-property :raw-value node))
        (tags (org-element-property :tags node))
        (id (org-element-property :ID node))
	(links (org-gnosis-get-links (org-element-contents node)))
        (children (org-element-contents node)))
    (when title
      (list title tags id
            (org-gnosis-process-children children (1+ (org-element-property :level node)))
	    links))))

(defun org-gnosis-process-children (nodes level)
  "Recursively process NODES at a given LEVEL."
  (let (result)
    (while nodes
      (let ((current-node (car nodes))
            (current-level (org-element-property :level (car nodes))))
        (if (and current-level (= current-level level))
            (progn
              (push (org-gnosis-process-node current-node) result)
              (setq nodes (cdr nodes)))
          (setq nodes (cdr nodes)))))
    (nreverse result)))

(defun org-gnosis-get-data--nodes (&optional parsed-data)
  "Return a hierarchical list of nodes with titles, tags, and IDs from PARSED-DATA."
  (let ((parsed-data (or parsed-data (org-element-parse-buffer))))
    (org-gnosis-process-children (org-element-map parsed-data 'headline #'identity) 1)))

(defun org-gnosis-get-data--topic (&optional parsed-data)
  "Retrieve the title and ID from the current org buffer or given PARSED-DATA."
  (let* ((parsed-data (or parsed-data (org-element-parse-buffer)))
         (title (org-element-map parsed-data 'keyword
                  (lambda (kw)
                    (when (string= (org-element-property :key kw) "TITLE")
                      (org-element-property :value kw)))
                  nil t))
         (id (org-element-map parsed-data 'property-drawer
                (lambda (drawer)
                  (org-element-map (org-element-contents drawer) 'node-property
                    (lambda (prop)
                      (when (string= (org-element-property :key prop) "ID")
                        (org-element-property :value prop)))
                    nil t))
                nil t))
	 (tags (org-gnosis-get-filetags)))
    (list title tags id)))

(defun org-gnosis-get--data (file)
  "Return data for FILE.

FILE: File path"
  (let* ((parsed-data (org-element-parse-buffer))
	 (topic (org-gnosis-get-data--topic parsed-data))
	 (nodes (org-gnosis-get-data--nodes parsed-data))
	 (filename (file-name-nondirectory file))
	 (links (org-gnosis-get-links parsed-data)))
    `(:file ,filename :topic ,topic :nodes ,nodes :links ,links)))

(defun org-gnosis-adjust-title (input &optional node-id)
  "Adjust the INPUT string to replace id link structures with plain text.

Adjust title INPUT for NODE-ID.  If node-id contains an id link, it's
inserted as link for NODE-ID in the database."
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

(defun org-gnosis-get-file-info (filename)
  "Something FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (let* ((data (org-gnosis-get--data filename)))
      data)))

(defun org-gnosis-parse-nodes (nodes top-node-id inherited-tags)
  "Parse a list of nodes, inheriting tags and associating the top node ID.
NODES: list of nodes to parse.
TOP-NODE-ID: the ID of the top node to associate with each node.
INHERITED-TAGS: tags from the top node to inherit."
  (cl-loop for (name tags id sub-nodes links) in nodes
           ;; Only include nodes with non-nil id
           when id
           append (list (list :node name
                              :tags (append tags inherited-tags)
                              :id id
                              :top-node top-node-id
			      :links links))
           ;; Recursively parse sub-nodes, inheriting current node's tags
           append (org-gnosis-parse-nodes sub-nodes (when id id) (append tags inherited-tags))))

(defun org-gnosis-parse-data-recursive (data &optional initial-tags top-node-id)
  "Recursively parse the entire data structure, extracting nodes and details.
DATA: List of top-level nodes to start parsing.
INITIAL-TAGS: Initial set of tags to inherit."
  (cl-loop for (node tags id sub-nodes) in data
           ;; Directly parse sub-nodes, using top-level nodes only if they have valid id
           append (when id (list (list :node node
                                       :tags (append tags initial-tags)
                                       :id id
                                       :top-node top-node-id)))
           append (org-gnosis-parse-nodes sub-nodes id (append tags initial-tags))))

(defun org-gnosis-update-file (&optional file)
  "Update contents of FILE in databse.

Removes all contents of FILE in database, adding them anew."
  (let* ((file (or file (file-name-nondirectory (buffer-file-name))))
	 (journal-p (file-in-directory-p file org-gnosis-journal-dir))
	 (nodes (if journal-p
		    (org-gnosis-select 'id 'journal `(= file ,file) t)
		  (org-gnosis-select 'id 'nodes `(= file ,file) t))))
    (emacsql-with-transaction org-gnosis-db
      ;; Delete all nodes of file in db
      (cl-loop for node in nodes
	       do (if journal-p
		      (org-gnosis--delete 'journal `(= id ,node))
		    (org-gnosis--delete 'nodes `(= id ,node))))
      (if journal-p
	  (org-gnosis-journal--update-file file)
	(org-gnosis--update-file file)))))

(defun org-gnosis--is-journal-entry-p (file)
  "Check if FILE is a journal entry."
  (let ((file-dir (file-name-directory (expand-file-name file)))
        (expanded-dir (file-name-as-directory (expand-file-name org-gnosis-journal-dir))))
    (string-equal file-dir expanded-dir)))

(defun org-gnosis--update-file (file)
  "Add contents of FILE to database."
  (let* ((data (org-gnosis-get-file-info file))
	 (file (plist-get data :file))
	 (topic (org-gnosis-adjust-title (nth 0 (plist-get data :topic))))
	 (tags (nth 1 (plist-get data :topic)))
	 (hash (nth 2 (plist-get data :topic))) ;; topic id
	 (links  (plist-get data :links))) ;; main topic links
    ;; Add gnosis topic
    (emacsql-with-transaction org-gnosis-db
      (org-gnosis--insert-into 'nodes `([,hash ,file ,topic ,tags]))
      (cl-loop for link in links
	       do (org-gnosis--insert-into 'links `([,hash ,link]))))
    ;; Add nodes of topic
    (cl-loop for node in (org-gnosis-parse-data-recursive
			  (plist-get data :nodes)
			  tags ;; initial topic tags for top node
			  hash ;; node topic hash
			  )
	     do
	     (let ((title (org-gnosis-adjust-title (plist-get node :node)))
		   (tags (plist-get node :tags))
		   (id (plist-get node :id)))
	       (emacsql-with-transaction org-gnosis-db
		 (org-gnosis--insert-into 'nodes `([,id ,file ,title ,tags]))
		 ;; (org-gnosis--insert-into 'links `([,id ]))
		 )))))

(defun org-gnosis-journal--update-file (file)
  "Update database for journal FILE."
  (let* ((data (org-gnosis-get-file-info file))
	 (file (plist-get data :file))
	 (tags (nth 1 (plist-get data :topic)))
	 (date (org-gnosis-adjust-title (nth 0 (plist-get data :topic))))
	 (hash (nth 2 (plist-get data :topic)))
	 (links (plist-get data :links)))
    ;; Add journal
    (emacsql-with-transaction org-gnosis-db
      (org-gnosis--insert-into 'journal `([,hash ,file ,date ,tags])))))

(defun org-gnosis-db-sync--journal ()
  "Sync journal entries in databse."
  (cl-loop for file in (cl-remove-if-not (lambda (file)
					   (and (string-match-p "^[0-9]"
								(file-name-nondirectory file))
						(not (file-directory-p file))))
					 (directory-files org-gnosis-journal-dir t nil t))
	   do (org-gnosis-journal--update-file file)))

;;;###autoload
(defun org-gnosis-db-sync (&optional arg)
  "Sync `org-gnosis-db'.

If called with ARG do not initialize the database."
  (interactive "P")
  (unless arg
    (org-gnosis-db-init))
  (let ((files (cl-remove-if-not (lambda (file)
				   (and (string-match-p "^[0-9]" (file-name-nondirectory file))
					(not (file-directory-p file))))
				 (directory-files org-gnosis-dir t nil t))))
    (cl-loop for file in files
	     do (org-gnosis--update-file file)))
  (org-gnosis-db-sync--journal))

(defun org-gnosis-find--tag-with-tag-prop (lst)
  "Combine each sublist of strings in LST into a single string."
  (mapcar (lambda (item)
          (let* ((title (car item))
                 (tags (cadr item))
                 (propertized-tags
		  (when tags
                    (concat (propertize "#" 'face 'org-gnosis-face-tags)
                            (propertize (mapconcat 'identity tags "#")
					'face 'org-gnosis-face-tags)))))
            (if propertized-tags
                (format "%s  %s" title propertized-tags)
              title)))
        lst))

(defun org-gnosis--create-file (title &optional file extras)
  "Create node & FILE for TITLE."
  (let* ((file-name (replace-regexp-in-string "#" "" ;; hashtags are used for tags
					      (replace-regexp-in-string " " "-" title)))
	 (file (or file (expand-file-name
			 (format "%s--%s.org" (format-time-string "%Y%m%d%H%M%S") file-name)
			 org-gnosis-dir))))
    (find-file file)
    (unless (file-exists-p file)
      (insert (format "#+title: %s\n#+filetags:\n" title))
      (org-id-get-create)
      (and extras (insert extras))
      (org-mode)
      (org-gnosis-mode)
      file-name)))

(defun org-gnosis-find--with-tags (&optional prompt entries)
  "Select gnosis node with tags from ENTRIES.

PROMPT: Prompt message."
  (replace-regexp-in-string "  #[^[:space:]]+" ""
   (completing-read (or prompt "Select gnosis node: ")
		    (org-gnosis-find--tag-with-tag-prop
		     (or entries (org-gnosis-select '[title tags] 'nodes))))))

(defun org-gnosis--find (prompt entries-with-tags entries)
  "PROMPT user to select from ENTRIES.

If `org-gnosis-show-tags' is non-nil, ENTRIES-WITH-TAGS will be used
instead."
  (let* ((entry (if org-gnosis-show-tags
                    (org-gnosis-find--with-tags prompt entries-with-tags)
                  (completing-read prompt entries))))
    entry))

;;;###autoload
(defun org-gnosis-find (&optional title file id directory)
  "Select gnosis node."
  (interactive)
  (let* ((title (or title (if org-gnosis-show-tags
			      (org-gnosis-find--with-tags)
			    (completing-read "Select gnosis node: "
					     (org-gnosis-select 'title 'nodes)))))
	 (file (or file (caar (org-gnosis-select 'file 'nodes `(= title ,title)))))
	 (id (or id (caar (or id (org-gnosis-select 'id 'nodes `(= title ,title))))))
	 (directory (or directory org-gnosis-dir)))
    (cond ((null file)
	   (org-gnosis--create-file title))
	  ((file-exists-p (expand-file-name file directory))
	   (find-file
	    (expand-file-name file directory))
	   (ignore-errors (org-id-goto id))
	   (org-gnosis-mode 1)))))

;;;###autoload
(defun org-gnosis-insert ()
  "Insert gnosis node."
  (interactive)
  (let* ((node (org-gnosis--find "Select gnosis node: "
				 (org-gnosis-select '[title tags] 'nodes '1=1)
				 (org-gnosis-select 'title 'nodes '1=1)))
	 (id (concat "id:" (car (org-gnosis-select 'id 'nodes `(= ,node title) '1=1)))))
    (org-insert-link nil id node)))

;;;###autoload
(defun org-gnosis-journal-find (&optional date)
  "Find journal entry for DATE."
  (interactive)
  (let* ((prompt "Select journal entry")
	 (date (or date (org-gnosis--find
			 prompt
			 (org-gnosis-select '[date tags] 'journal)
			 (org-gnosis-select 'date 'journal))))
	 (id (car (org-gnosis-select 'id 'journal `(= date ,date) t)))
	 (file (car (org-gnosis-select 'file 'journal `(= date ,date) t))))
    (org-gnosis-find date file id org-gnosis-journal-dir)))

;;;###autoload
(defun org-gnosis-journal-insert ()
  "Insert journal entry."
  (interactive)
  (let* ((node (org-gnosis--find "Select journal entry: "
		  (org-gnosis-select '[date tags] 'journal '1=1)
		  (org-gnosis-select 'date 'journal '1=1)))
	 (node-id (concat "id:" (car (org-gnosis-select 'id 'journal `(= ,node date) '1=1)))))
    (org-insert-link nil node-id node)))

(defun org-gnosis-journal (&optional template)
  "Start journaling for current date."
  (interactive)
  (let* ((date (format-time-string "%Y-%m-%d"))
	 (file (format "%s.org" date)))
    (org-gnosis--create-file date (expand-file-name file org-gnosis-journal-dir)
			     (or template org-gnosis-journal-template))))

(define-minor-mode org-gnosis-mode
  "Org gnosis mode."
  :lighter " org-gnosis"
  :keymap nil
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
       (title text)
       tags]))
    (tags
     ([(tag-name text :primary-key)]))
    (journal
     ([(id :not-null :primary-key)
       (file :not-null)
       (date text)
       tags]))
    ;; (node-tags
    ;;  ([(node-id :not-null)
    ;;    (tags :not-null)
    ;;    (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)
    ;;    (:foreign-key [tags] :references tags [tag-name] :on-delete :cascade)]))
    (links
     ([(source text)
       (dest text)]
      (:foreign-key [source] :references nodes [id] :on-delete :cascade)
      (:unique [source dest])))))

(defun org-gnosis-db-delete-tables ()
  "Drop all tables."
  (ignore-errors
    (emacsql-with-transaction org-gnosis-db
    (org-gnosis--drop-table 'nodes)
    (org-gnosis--drop-table 'tags)
    (org-gnosis--drop-table 'journal)
    (org-gnosis--drop-table 'links))))

(defun org-gnosis-db-init ()
  "Initialize database DB with the correct schema and user version."
  (setf org-gnosis-db (emacsql-sqlite-open (locate-user-emacs-file "org-gnosis.db")))
  (org-gnosis-db-delete-tables)
  (when (length< (emacsql org-gnosis-db
			    [:select name :from sqlite-master :where (= type table)])
		 3)
    (org-gnosis-db-delete-tables)
    (emacsql-with-transaction org-gnosis-db
      (pcase-dolist (`(,table ,schema) org-gnosis-db--table-schemata)
	(emacsql org-gnosis-db [:create-table $i1 $S2] table schema))
      (emacsql org-gnosis-db [:pragma (= user-version org-gnosis-db-version)]))))

(provide 'org-gnosis)
;;; org-gnosis.el ends here
