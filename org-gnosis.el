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

(defcustom org-gnosis-journal-templates
  '(("default" "* Daily Notes\n\n* Goals\n+ []")
    ("Empty" ""))
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

(defun org-gnosis-parse-headline (headline inherited-tags topic-id)
  "Parse a single headline and return a plist with its info."
  (let* ((title (org-element-property :raw-value headline))
         (id (org-element-property :ID headline))
         (level (org-element-property :level headline))
         (tags (or (org-element-property :tags headline) inherited-tags))
         (all-tags (delete-dups (append inherited-tags tags)))
         (links (org-gnosis-collect-id-links headline))
         (master (if (= level 1) topic-id
                   (org-element-property :ID (org-element-property :parent headline)))))
    (and id
         (list :title title :id id :links links :tags all-tags :master master))))

(defun org-gnosis-collect-id-links (element)
  "Collect all ID links within ELEMENT that start with id:."
  (org-element-map element 'link
    (lambda (link)
      (let ((raw-link (org-element-property :raw-link link)))
        (when (string-prefix-p "id:" raw-link)
          (substring raw-link 3))))
    nil nil t))

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

;; This one is used mostly for topic
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
         (topic-id (nth 2 topic-info))
         (topic-links (org-gnosis-collect-id-links parsed-data)))
    (when topic-id
      (list :title topic-title :id topic-id :links topic-links :tags topic-tags :master 0))))

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
    (when topic
      (plist-put topic :links (org-gnosis-collect-id-links parsed-data)))
    (nreverse (cons topic headlines))))

(defun org-gnosis-get-file-info (filename)
  "Get data for FILENAME.

Returns file data with FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (let* ((data (org-gnosis-buffer-data)))
      (append data (list (file-name-nondirectory filename))))))

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
	 (filename (file-name-nondirectory file)))
    ;; Add gnosis topic
    (emacsql-with-transaction org-gnosis-db
      (cl-loop for item in (butlast data)
	       do (let ((title (org-gnosis-adjust-title
				(plist-get item :title)))
			(id (plist-get item :id))
			(links (plist-get item :links))
			;; (master (plist-get item :master))
			(tags (plist-get item :tags)))
		    (org-gnosis--insert-into 'nodes `([,id ,filename ,title ,tags]))
	            (cl-loop for link in links
			     do (org-gnosis--insert-into 'links `([,id ,link])))
		    (cl-loop for tag in tags
			     do
			     (org-gnosis--insert-into 'tags `([,tag]))
			     (org-gnosis--insert-into 'node-tag `([,id ,tag]))))))))


(defun org-gnosis-journal--update-file (file)
  "Update database for journal FILE."
  (let* ((data (org-gnosis-get-file-info file))
	 (file (file-name-nondirectory file)))
    (emacsql-with-transaction org-gnosis-db
      (cl-loop for item in (butlast data)
	       do (let ((title (plist-get item :title))
			(id (plist-get item :id))
			(links (plist-get item :links))
			;; (master (plist-get item :master))
			(tags (plist-get item :tags)))
		    (org-gnosis--insert-into 'journal `([,id ,file ,title ,tags]))
	            (cl-loop for link in links
			     do (org-gnosis--insert-into 'links `([,id ,link])))
		    (cl-loop for tag in tags
			     do
			     (org-gnosis--insert-into 'tags `([,tag]))
			     (org-gnosis--insert-into 'node-tag `([,id ,tag]))))))))

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
      (insert (format "#+title: %s\n#+filetags: \n" title))
      (org-id-get-create)
      (and extras (insert extras))
      (org-mode)
      (org-gnosis-mode)
      file-name)))

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

(defun org-gnosis-journal-select-template (&optional templates)
  "Selecte journal template from TEMPLATES."
  (let* ((templates (or templates org-gnosis-journal-templates))
	 (selected (funcall org-gnosis-completing-read-func "Select template:"
			    (mapcar #'car templates)))
	 (template (cdr (assoc selected templates))))
    (apply #'append template)))

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
(defun org-gnosis-insert-tag ()
  "Insert filetag."
  (interactive)
  (let* ((filetags (org-gnosis-select 'tag 'tags '1=1 t))
         (tag (funcall org-gnosis-completing-read-func "Select tag: " filetags)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+FILETAGS:" nil t)
          (progn
            (end-of-line)
            (insert (if (looking-back ":" nil) "" ":") tag ":"))
        (progn
          (insert "#+FILETAGS: :" tag ":")
          (newline))))))

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
			     (or template (org-gnosis-journal-select-template)))))

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
     ([(tag text :primary-key)]
      (:unique [tag])))
    (journal
     ([(id :not-null :primary-key)
       (file :not-null)
       (date text)
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
      (org-gnosis--drop-table 'nodes)
      (org-gnosis--drop-table 'tags)
      (org-gnosis--drop-table 'journal)
      (org-gnosis--drop-table 'links)
      (org-gnosis--drop-table 'node-tag))))

(defun org-gnosis-db-sync--journal ()
  "Sync journal entries in databse."
  (cl-loop for file in (cl-remove-if-not (lambda (file)
					   (and (string-match-p "^[0-9]"
								(file-name-nondirectory file))
						(not (file-directory-p file))))
					 (directory-files org-gnosis-journal-dir t nil t))
	   do (org-gnosis-journal--update-file file)))

;;;###autoload
(defun org-gnosis-db-sync ()
  "Sync `org-gnosis-db'.

If called with ARG do not initialize the database."
  (interactive)
  (org-gnosis-db-init)
  (let ((files (cl-remove-if-not (lambda (file)
				   (and (string-match-p "^[0-9]" (file-name-nondirectory file))
					(not (file-directory-p file))))
				 (directory-files org-gnosis-dir t nil t))))
    (cl-loop for file in files
	     do (org-gnosis--update-file file)))
  (org-gnosis-db-sync--journal))

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
