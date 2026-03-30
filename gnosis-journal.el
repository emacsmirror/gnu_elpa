;;; gnosis-journal.el --- Journal module for gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions

;;; Commentary:

;; Journal entries, TODO integration, and checked item tracking.
;; Uses gnosis-sqlite for DB and gnosis-org for parsing.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'gnosis-org)

;; Forward declarations
(declare-function gnosis-select "gnosis")
(declare-function gnosis--ensure-db "gnosis")
(declare-function gnosis-sqlite-with-transaction "gnosis-sqlite")
(declare-function gnosis-nodes-select "gnosis-nodes")
(declare-function gnosis-nodes--find "gnosis-nodes")
(declare-function gnosis-nodes--create-file "gnosis-nodes")
(declare-function gnosis-nodes-select-template "gnosis-nodes")
(declare-function gnosis-nodes-find "gnosis-nodes")
(declare-function gnosis-nodes-mode "gnosis-nodes")
(declare-function gnosis-nodes-update-file "gnosis-nodes")
(declare-function gnosis-nodes--file-changed-p "gnosis-nodes")

(defgroup gnosis-journal nil
  "Gnosis journal."
  :group 'gnosis)

(defcustom gnosis-journal-dir
  (expand-file-name "journal" (bound-and-true-p gnosis-nodes-dir))
  "Gnosis journal directory."
  :type 'directory)

(defcustom gnosis-journal-file nil
  "When non-nil, use this file for journal entries as level 1 headings.
If nil, journal entries are created as separate files in
`gnosis-journal-dir'."
  :type '(choice (const :tag "Use separate files" nil)
                 (file :tag "Single journal file")))

(defcustom gnosis-journal-as-gpg nil
  "When non-nil, create journal entries with a .gpg suffix."
  :type 'boolean)

(defcustom gnosis-journal-templates
  (list (cons "Default"
              (lambda () (concat "{*} Daily Notes\n\n{*} Goals\n" (gnosis-journal-todos))))
        (cons "Empty" (lambda () "")))
  "Templates for journaling.
Template functions return strings.  Use \"{*}\" as a heading
placeholder; it will be expanded to org heading stars relative to
the insertion context.  \"{**}\" adds one extra level, \"{***}\"
adds two, etc."
  :type '(alist :key-type (string :tag "Name")
                :value-type (function :tag "Template Function")))

(defcustom gnosis-journal-todo-files org-agenda-files
  "TODO files used for the journal entries."
  :type '(repeat string))

(defcustom gnosis-journal-todo-keywords org-todo-keywords
  "TODO Keywords used for parsing `gnosis-journal-todo-files'.
All items after the vertical bar \"|\" will be ignored, for
compatability with `org-todo-keywords'."
  :type '(repeat string))

(defcustom gnosis-journal-bullet-point-char "+"
  "String to indicate a bullet point."
  :type 'string)

;;; Internal helpers

(defun gnosis-journal--dir ()
  "Return journal directory, ensuring it exists."
  (let ((dir (or gnosis-journal-dir
		 (expand-file-name "journal"
				   (bound-and-true-p gnosis-nodes-dir)))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun gnosis-journal--create-file ()
  "Create `gnosis-journal-file' when non-nil and file does not exist."
  (when (and gnosis-journal-file
	     (not (file-exists-p gnosis-journal-file)))
    (with-current-buffer (find-file-noselect gnosis-journal-file)
      (insert (format "#+title: %s Journal\n#+filetags: \n" (or user-full-name "")))
      (gnosis-nodes-mode)
      (save-buffer)
      (message "Created journal file."))))

(defun gnosis-journal--add-entry (title)
  "Add entry for TITLE to `gnosis-journal-file'."
  (when gnosis-journal-file
    (gnosis-journal--create-file)
    (find-file gnosis-journal-file)
    (goto-char (point-max))
    (insert (format "* %s\n" title))
    (org-id-get-create)
    (insert (gnosis-org-expand-headings
	     (gnosis-nodes-select-template gnosis-journal-templates)))))

;;; TODOs

(defun gnosis-journal-get--todos (file)
  "Get TODO items for FILE."
  (let ((todos))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (headline)
          (when (member (org-element-property :todo-keyword headline)
			(cl-loop for keyword in gnosis-journal-todo-keywords
				 until (and (stringp keyword) (string= keyword "|"))
				 collect keyword))
            (let* ((title (org-element-property :raw-value headline))
                   (timestamp (org-element-property
			       :raw-value (org-element-property :scheduled headline))))
              (push `(,title ,timestamp ,file) todos))))))
    (nreverse todos)))

(defun gnosis-journal-get-todos (&optional files)
  "Get TODO items for FILES."
  (let ((files (or files gnosis-journal-todo-files))
	todos)
    (cl-loop for file in files
	     do (push (gnosis-journal-get--todos file) todos))
    (nreverse (apply #'append todos))))

(defun gnosis-journal-todos ()
  "Output todos as checkboxes in a string for current date."
  (let ((todos (gnosis-journal-get-todos))
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
			       (format "%s [ ] %s\n" gnosis-journal-bullet-point-char
				       todo-title))))))
    (or todos-string "")))

(defun gnosis-journal-get-checked-items (element)
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

(defun gnosis-journal-find-file-with-heading (title files)
  "Find first org file in FILES containing heading TITLE."
  (catch 'found
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (goto-char (point-min))
        (when (org-find-exact-headline-in-buffer title)
          (throw 'found file))))))

(defun gnosis-journal-mark-todo-as-done (todo-title)
  "Mark scheduled TODO with TODO-TITLE as DONE if not already done today."
  (let* ((file (gnosis-journal-find-file-with-heading todo-title gnosis-journal-todo-files))
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
                             (not (org-entry-get (org-element-property :begin headline)
					       "LAST_DONE_DATE")))
                    (org-with-point-at (org-element-property :begin headline)
                      (org-todo 'done)
                      (org-entry-put nil "LAST_DONE_DATE" today))
                    (setq found t))))))
          (save-buffer))))))

(defun gnosis-journal--update-todos (file)
  "Update TODO items from journal FILE."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (buf (get-file-buffer file))
         (parsed-buffer (with-temp-buffer
                          (if buf
                              (insert-buffer-substring buf)
                            (insert-file-contents file))
                          (unless (derived-mode-p 'org-mode)
                            (org-mode))
                          (org-element-parse-buffer)))
         (done-todos (if (and gnosis-journal-file
                              (string= (file-name-nondirectory file)
                                       (file-name-nondirectory gnosis-journal-file)))
                         (let ((today-heading
                                (org-element-map parsed-buffer 'headline
                                  (lambda (headline)
                                    (when (string= (org-element-property :raw-value headline) today)
                                      headline))
                                  nil t)))
                           (if today-heading
                               (gnosis-journal-get-checked-items today-heading)
                             nil))
                       (gnosis-journal-get-checked-items parsed-buffer))))
    (cl-loop for done-todo in done-todos
	     do (gnosis-journal-mark-todo-as-done done-todo))))

;;; Interactive commands

;;;###autoload
(defun gnosis-journal-find (&optional title)
  "Find journal entry for TITLE."
  (interactive)
  (let* ((title (or title (gnosis-nodes--find
			   "Select journal entry: "
			   (gnosis-nodes-select '[title tags] 'journal)
			   (gnosis-nodes-select 'title 'journal))))
	 (id (car (gnosis-nodes-select 'id 'journal `(= title ,title) t)))
	 (file (car (gnosis-nodes-select 'file 'journal `(= title ,title) t))))
    (cond
     ((and id file)
      (gnosis-nodes-find
       title file id (gnosis-journal--dir) gnosis-journal-templates))
     ((and gnosis-journal-file
	   (string= title (format-time-string "%Y-%m-%d")))
      (gnosis-journal--add-entry title))
     (t
      (gnosis-nodes--create-file
       title (gnosis-journal--dir)
       (gnosis-nodes-select-template gnosis-journal-templates))))))

;;;###autoload
(defun gnosis-journal-insert (arg)
  "Insert journal entry.
If called with prefix ARG, use custom link description."
  (interactive "P")
  (gnosis-nodes-insert arg t))

;;;###autoload
(defun gnosis-journal ()
  "Journal for current date."
  (interactive)
  (let* ((date (format-time-string "%Y-%m-%d")))
    (gnosis-journal-find date)))

;;; Sync

(defun gnosis-journal-db-sync (&optional force)
  "Sync journal entries in database.
When FORCE, update all files.  Otherwise, only update changed files."
  (let* ((journal-dir (gnosis-journal--dir))
	 (journal-files (cl-remove-if-not
                         (lambda (file)
                           (and (string-match-p "\\.org\\(?:\\.gpg\\)?$" file)
                                (not (file-directory-p file))))
                         (directory-files journal-dir t nil t)))
         (all-files (if (and gnosis-journal-file
                             (file-exists-p gnosis-journal-file))
                        (cons gnosis-journal-file journal-files)
                      journal-files))
         (files (if force
                    all-files
                  (cl-remove-if-not
                   (lambda (file) (gnosis-nodes--file-changed-p file 'journal))
                   all-files))))
    (when (> (length files) 0)
      (let ((progress (make-progress-reporter
                       (format "Processing %d/%d journal files..." (length files) (length all-files))
                       0 (length files))))
        (cl-loop for file in files
                 for i from 0
                 do (progn
                      (gnosis-nodes-update-file file)
                      (progress-reporter-update progress i)))
        (progress-reporter-done progress)))))

(provide 'gnosis-journal)
;;; gnosis-journal.el ends here
