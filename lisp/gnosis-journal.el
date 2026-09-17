;;; gnosis-journal.el --- Journal module for gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions

;;; Commentary:

;; Dated Org journal entries, task references, and optional study views.
;; Org files own journal text and IDs; saving a journal never completes
;; external tasks.  `gnosis-study' owns study history views.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'string-edit)
(require 'gnosis-org)
(require 'gnosis-db)
(require 'gnosis-sqlite)
(require 'gnosis-nodes)
(declare-function gnosis-study-day "gnosis-study")

(defgroup gnosis-journal nil
  "Gnosis journal."
  :group 'gnosis)

(defcustom gnosis-journal-dir
  (expand-file-name "journal" (bound-and-true-p gnosis-nodes-dir))
  "Gnosis journal directory."
  :type 'directory)

(defcustom gnosis-journal-file t
  "Single journal file, or nil for one file per entry.

The value t selects `journal.org' under `gnosis-journal-dir', or
`journal.org.gpg' when `gnosis-journal-as-gpg' is non-nil.  Nil
keeps separate files in `gnosis-journal-dir'.  A string is an
explicit file: relative values resolve against
`gnosis-journal-dir', absolute paths are used as-is, and the
suffix is authoritative.  Existing files are never renamed or
converted."
  :type '(choice (const :tag "Single file (journal.org or journal.org.gpg)" t)
                 (const :tag "Use separate files" nil)
                 (file :tag "Explicit journal file")))

(defcustom gnosis-journal-as-gpg nil
  "When non-nil, create journal files with a .gpg suffix.
The automatic single-file setting (`gnosis-journal-file' = t)
then uses journal.org.gpg under `gnosis-journal-dir'.  An explicit
`gnosis-journal-file' string keeps its suffix unchanged."
  :type 'boolean)

(defcustom gnosis-journal-templates
  (list (cons "Default"
              (lambda ()
                (concat "{*} Daily Notes\n:PROPERTIES:\n"
                        ":GNOSIS_JOURNAL_ROLES: thoughts\n:END:\n\n"
                        "{*} Goals\n:PROPERTIES:\n"
                        ":GNOSIS_JOURNAL_ROLES: todos\n:END:\n"
                        (gnosis-journal-todos))))
        (cons "Empty" (lambda () ""))
        (cons "Study" (lambda ()
                        "{*} Study\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: thoughts\n:END:\n\n"))
        (cons "Goals" (lambda ()
                        (concat "{*} Goals\n:PROPERTIES:\n"
                                ":GNOSIS_JOURNAL_ROLES: todos\n:END:\n"
                                (gnosis-journal-todos))))
        (cons "Day reflection"
              (lambda ()
                "{*} Day reflection\n:PROPERTIES:\n:GNOSIS_JOURNAL_ROLES: thoughts\n:END:\n\n")))
  "Templates for journaling.
Template functions take no arguments and return strings.  During
journal creation or insertion, `gnosis-journal-template-date' holds
the selected ISO date; `gnosis-journal-todos' uses that date.
Use {*} as a heading
placeholder; it will be expanded to org heading stars relative to
the insertion context.  {**} adds one extra level, {***} adds two,
etc.  A heading's non-inherited GNOSIS_JOURNAL_ROLES property may be
thoughts, todos, or thoughts todos.  Each role has at most one destination
per date.  Consolidate additive GNOSIS_JOURNAL_ROLES+ declarations into
one property; additive syntax is not supported.  Capture appends to its
own body before children; absent roles
use the date body with a notice.  Heading titles have no routing meaning.
Existing dates retain their own declarations when templates change."
  :type '(alist :key-type (string :tag "Name")
                :value-type (function :tag "Template Function")))

(defcustom gnosis-journal-new-entry-template nil
  "Template name to insert automatically when creating a new dated entry.
Nil means free-form writing, without generated body or template prompt.
A string names an entry in `gnosis-journal-templates'.  Existing dates
are never rewritten.  Non-dated entries retain template selection.
Use `gnosis-journal-insert-template' to add a template on demand."
  :type '(choice (const :tag "Free-form" nil) (string :tag "Template name"))
  :group 'gnosis-journal)

(defvar gnosis-journal-template-date nil
  "ISO date being expanded by a journal template, or nil outside expansion.
Template functions still take no arguments.  They may read this variable
or pass an explicit date to `gnosis-journal-todos'.")

(defcustom gnosis-journal-todo-files org-agenda-files
  "TODO files used for journal task collection."
  :type '(repeat string))

(defcustom gnosis-journal-todo-keywords '("TODO")
  "TODO keywords used for parsing `gnosis-journal-todo-files'."
  :type '(repeat string))

(defcustom gnosis-journal-bullet-point-char "+"
  "String to indicate a bullet point."
  :type 'string)

(defvar-keymap gnosis-journal-prefix-map
  :doc "Prefix map for Gnosis journal commands."
  "j" #'gnosis-journal
  "d" #'gnosis-journal-date
  "f" #'gnosis-journal-find
  "n" #'gnosis-journal-next
  "p" #'gnosis-journal-previous
  "c" #'gnosis-journal-capture
  "a" #'gnosis-journal-add-todo
  "i" #'gnosis-journal-insert-template
  "t" #'gnosis-journal-insert-task
  "k" #'gnosis-journal-complete-task
  "s" #'gnosis-journal-study
  "l" #'gnosis-journal-insert)

;;; Path resolution

(defun gnosis-journal--directory ()
  "Return the journal directory path without creating it."
  (or gnosis-journal-dir
      (expand-file-name "journal" (bound-and-true-p gnosis-nodes-dir))))

(defun gnosis-journal--dir ()
  "Return journal directory, ensuring it exists."
  (let ((dir (gnosis-journal--directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun gnosis-journal--configured-file ()
  "Return the absolute single journal file, or nil for separate-file mode.
The value t selects journal.org or journal.org.gpg from
`gnosis-journal-as-gpg'.  A string is used as given.  Do not
create the file or signal on a directory target."
  (cond
   ((eq gnosis-journal-file t)
    (expand-file-name (if gnosis-journal-as-gpg "journal.org.gpg" "journal.org")
                      (gnosis-journal--directory)))
   ((null gnosis-journal-file) nil)
   ((and (stringp gnosis-journal-file)
         (not (string-empty-p gnosis-journal-file)))
    (expand-file-name gnosis-journal-file
                      (gnosis-journal--directory)))
   (t
    (user-error "Invalid gnosis-journal-file: %S" gnosis-journal-file))))

(defun gnosis-journal--file ()
  "Return the resolved single journal file, or nil for separate-file mode.
Signal `user-error' if the configured path is a directory or not a
usable file path.  Do not create the file."
  (when-let* ((file (gnosis-journal--configured-file)))
    (when (or (and (stringp gnosis-journal-file)
                   (directory-name-p gnosis-journal-file))
              (and (file-exists-p file) (not (file-regular-p file))))
      (user-error "Journal file must be a regular file: %s" file))
    file))

(defun gnosis-journal--file-p (file)
  "Return non-nil if FILE is the configured single journal file."
  (when-let* ((single (gnosis-journal--configured-file)))
    (and file
         (not (file-directory-p single))
         (equal (expand-file-name file) (expand-file-name single)))))

;;; Dates and entries

(defun gnosis-journal--iso-date-p (string)
  "Return non-nil if STRING is a real YYYY-MM-DD calendar date."
  (when (and (stringp string)
             (string-match
              "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
              string))
    (let* ((year (string-to-number (match-string 1 string)))
           (month (string-to-number (match-string 2 string)))
           (day (string-to-number (match-string 3 string)))
           (decoded (decode-time (encode-time 0 0 0 day month year))))
      (and (= day (nth 3 decoded))
           (= month (nth 4 decoded))
           (= year (nth 5 decoded))))))

(defun gnosis-journal--title-date (title)
  "Return TITLE's ISO date, including root-id hierarchical titles."
  (save-match-data
    (cond ((gnosis-journal--iso-date-p title) title)
          ((and (stringp title)
                (string-match
                 ":\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\'"
                 title))
           (let ((date (match-string 1 title)))
             (and (gnosis-journal--iso-date-p date) date))))))

(defun gnosis-journal--iso-to-int (date)
  "Return DATE as a YYYYMMDD integer."
  (unless (gnosis-journal--iso-date-p date)
    (user-error "Not a calendar date: %s" date))
  (string-to-number (replace-regexp-in-string "-" "" date)))

(defun gnosis-journal--parse-file (file function)
  "Call FUNCTION in FILE's live buffer or a temp copy of its disk text.
Use a live buffer visiting the same physical file, even through an alias.
Signal ordinary read, permission and decryption errors."
  (if-let* ((buf (find-buffer-visiting file)))
      (with-current-buffer buf
        (save-excursion
          (save-restriction
            (widen)
            (funcall function))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (funcall function))))

(defun gnosis-journal--level-one-headings (file)
  "Return level-1 heading entries in the current buffer for FILE."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline)
      (when (= (org-element-property :level headline) 1)
        (let ((title (org-element-property :raw-value headline)))
          (list (gnosis-journal--title-date title)
                title file (org-element-property :ID headline)))))
    nil nil 'headline))

(defun gnosis-journal--file-entries (file)
  "Return journal entries from FILE's live buffer or disk text.
Keep a file-level node when it has an ID or a calendar-date TITLE,
alongside ID-bearing or dated child entries.  Without a file-level ID,
non-dated files stay heading-first.  Ordinary sections must not hide a
separate daily file's root node."
  (gnosis-journal--parse-file
   file
   (lambda ()
     (let* ((headings (gnosis-journal--level-one-headings file))
            (topic (and (cadar (org-collect-keywords '("TITLE")))
                        (gnosis-org-get-data--topic)))
            (root (and topic
                       (list (gnosis-journal--title-date (car topic))
                             (car topic) file (nth 2 topic)))))
       (if (and root (or (nth 3 root) (car root)))
           (cons root (cl-remove-if-not
                       (lambda (entry) (or (car entry) (nth 3 entry)))
                       headings))
         (or headings (and root (list root))))))))

(defun gnosis-journal--indexed-entries ()
  "Return journal entries from SQLite without reading Org files."
  (cl-loop for (id title file) in (gnosis-nodes-select '[id title file] 'journal)
           for path = (expand-file-name file (gnosis-journal--directory))
           when (file-exists-p path)
           collect (list (gnosis-journal--title-date title)
                         title path id)))

(defun gnosis-journal--physical-equal (a b)
  "Return non-nil if A and B name the same physical file."
  (or (equal (expand-file-name a) (expand-file-name b))
      (and (file-exists-p a) (file-exists-p b) (file-equal-p a b))))

(defun gnosis-journal--source-files ()
  "Return unique existing journal Org files, including retained separate files.
Enumerate filenames only; callers choose which contents to inspect."
  (let* ((single (gnosis-journal--file))
         (dir (gnosis-journal--dir))
         (files (cl-remove-if-not
                 #'gnosis-nodes--org-file-p
                 (directory-files dir t nil t))))
    (cl-delete-duplicates
     (append (and single (file-regular-p single)
                  (list (expand-file-name single)))
             files)
     :test #'gnosis-journal--physical-equal)))

(defun gnosis-journal--inspected-files ()
  "Return configured and live journal files to inspect, configured first.
Keep each physical file once, independently of whether it has entries."
  (let ((single (gnosis-journal--file))
        (dir (gnosis-journal--directory)))
    (cl-delete-duplicates
     (append (when (and single (or (get-file-buffer single)
                                   (file-regular-p single)))
               (list single))
             (cl-loop for buffer in (buffer-list)
                      for file = (buffer-file-name buffer)
                      when (and file (file-directory-p dir)
                                (file-in-directory-p file dir)
                                (with-current-buffer buffer
                                  (derived-mode-p 'org-mode)))
                      collect file))
     :test #'gnosis-journal--physical-equal :from-end t)))

(defun gnosis-journal--entries ()
  "Return journal entries, with inspected source superseding the index.
Do not read archived files."
  (let* ((inspected (gnosis-journal--inspected-files))
         (source (mapcan #'gnosis-journal--file-entries inspected))
         (index (cl-remove-if
                 (lambda (entry)
                   (let ((file (expand-file-name (nth 2 entry))))
                     (cl-some (lambda (known)
                                (gnosis-journal--physical-equal file known))
                              inspected)))
                 (gnosis-journal--indexed-entries))))
    (append source index)))

(defun gnosis-journal--title-files (title)
  "Return retained filenames that can represent TITLE.
Match native names, including timestamp prefixes and encrypted suffixes,
without reading unrelated archives.  Callers still validate Org contents."
  (let ((pattern (concat "\\(?:\\`\\|--\\)"
                         (regexp-quote (gnosis-org--create-name title))
                         "\\(?:\\.gpg\\)?\\'"))
        (single (gnosis-journal--file)))
    (cl-remove-if-not
     (lambda (file)
       (and (not (and single
                      (gnosis-journal--physical-equal file single)))
            (string-match-p pattern (file-name-nondirectory file))))
     (gnosis-journal--source-files))))

(defun gnosis-journal--lookup-title (title)
  "Return (DATE TITLE FILE ID) entries named TITLE.
Inspect matching native filenames before creating a new entry, even
before the first archive sync.  Do not read unrelated archived files."
  (let* ((files (gnosis-journal--title-files title))
         (source (mapcan #'gnosis-journal--file-entries files))
         (entries
          (append source
                  (cl-remove-if
                   (lambda (entry)
                     (cl-some (lambda (file)
                                (gnosis-journal--physical-equal
                                 (nth 2 entry) file))
                              files))
                   (gnosis-journal--entries)))))
    (cl-remove-if-not
     (lambda (entry)
       (or (equal (nth 1 entry) title) (equal (car entry) title)))
     entries)))

(defun gnosis-journal--read-entry ()
  "Read a source-qualified entry or a new title without reading archives.
Tags decorate candidates, never identify targets.  Refuse ambiguous titles."
  (let* ((entries (gnosis-journal--entries))
         (tags (and gnosis-nodes-show-tags
                    (gnosis-nodes-select '[id tags] 'journal)))
         (labels (gnosis-nodes-find--tag-with-tag-prop
                  (mapcar (lambda (entry)
                            (list (nth 1 entry)
                                  (cadr (assoc (nth 3 entry) tags))))
                          entries)))
         (candidates (cl-mapcar #'cons labels entries))
         (choice (funcall gnosis-nodes-completing-read-func
                          "Select journal entry: " labels))
         (entry (cdr (assoc choice candidates))))
    (when (and entry
               (or (> (cl-count choice labels :test #'equal) 1)
                   (> (cl-count-if
                       (lambda (other)
                         (or (equal (nth 1 entry) (nth 1 other))
                             (equal (nth 1 entry) (car other))))
                       entries)
                      1)))
      (user-error "Ambiguous journal title: %s" (nth 1 entry)))
    (or entry choice)))

(defun gnosis-journal--unique-entry (title)
  "Return the unique (DATE TITLE FILE ID) entry named TITLE, or nil.
Signal `user-error' if more than one entry uses TITLE."
  (let ((found (gnosis-journal--lookup-title title)))
    (cond ((null found) nil)
          ((cdr found)
           (user-error "Ambiguous journal title: %s" title))
          (t (car found)))))

(defun gnosis-journal--goto-heading (title)
  "Move to the first level-1 heading named TITLE.
Return non-nil if found.  Search the widened buffer."
  (save-restriction
    (widen)
    (let ((found
           (org-element-map (org-element-parse-buffer) 'headline
             (lambda (headline)
               (when (and (= (org-element-property :level headline) 1)
                          (equal (org-element-property :raw-value headline)
                                 title))
                 headline))
             nil t 'headline)))
      (when found
        (goto-char (org-element-property :begin found))
        t))))

(defun gnosis-journal--heading-matches-p (title date)
  "Return non-nil if the node at point matches TITLE or DATE."
  (let* ((here (if (org-before-first-heading-p)
                   (cadar (org-collect-keywords '("TITLE")))
                 (org-get-heading t t t t)))
         (here-date (gnosis-journal--title-date here)))
    (or (equal here title)
        (and here-date
             (or (equal here-date date)
                 (equal here-date (gnosis-journal--title-date title)))))))

(defun gnosis-journal--goto-entry (entry)
  "Visit ENTRY, a (DATE TITLE FILE ID) list.
Validate the selected file and ID against the widened source.
Signal if the indexed location is stale.  Do not fall back from a
missing ID to a different heading with the same title.  When ENTRY
has no ID, its dated file TITLE may identify the root."
  (pcase entry
    (`(,date ,title ,file ,id)
     (unless (or (get-file-buffer file) (file-regular-p file))
       (user-error "Journal file %s does not exist.  \
Try `gnosis-nodes-db-force-sync' to resolve this" file))
     (find-file file)
     (widen)
     (cond
        (id
         (let ((positions (gnosis-journal--id-positions id)))
           (cond ((cdr positions)
                  (user-error "Duplicate journal ID %s" id))
                 ((null positions)
                  (user-error "Journal entry %s is missing from %s.  \
Try `gnosis-nodes-db-force-sync' to resolve this" title file))
                 (t
                  (goto-char (car positions))
                  (unless (or (gnosis-journal--heading-matches-p title date)
                              (seq-some
                               (lambda (node)
                                 (and (equal (plist-get node :id) id)
                                      (equal (plist-get node :title) title)))
                               (gnosis-org-buffer-data)))
                    (user-error "Journal entry %s is missing from %s.  \
Try `gnosis-nodes-db-force-sync' to resolve this" title file))))))
        (t
         (let ((count 0))
           (org-element-map (org-element-parse-buffer) 'headline
             (lambda (headline)
               (when (and (= (org-element-property :level headline) 1)
                          (equal (org-element-property :raw-value headline)
                                 title))
                 (cl-incf count)))
             nil nil 'headline)
           (cond ((> count 1)
                  (user-error "Ambiguous journal title: %s" title))
                 ((= count 1)
                  (gnosis-journal--goto-heading title))
                 (t
                  (goto-char (point-min))
                  (unless (and (org-before-first-heading-p)
                               (gnosis-journal--iso-date-p
                                (cadar (org-collect-keywords '("TITLE"))))
                               (gnosis-journal--heading-matches-p title date))
                    (user-error "Journal entry %s is missing from %s.  \
Try `gnosis-nodes-db-force-sync' to resolve this" title file)))))))
     (gnosis-nodes-mode 1))))

(defun gnosis-journal--date-at-point ()
  "Return the ISO date of the enclosing journal entry, or nil."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (or (when (and (org-current-level) (org-back-to-heading t))
              (while (and (> (org-current-level) 1)
                          (org-up-heading-safe)))
              (let ((title (org-get-heading t t t t)))
                (and (gnosis-journal--iso-date-p title) title)))
            (let ((title (cadar (org-collect-keywords '("TITLE")))))
              (and (gnosis-journal--iso-date-p title) title)))))))

;;; Creation

(defun gnosis-journal--destination ()
  "Return (FILE DB DIR) for the current journal destination.
FILE is nil in separate-file mode.  DIR is not created."
  (list (gnosis-journal--file) gnosis-db (gnosis-journal--directory)))

(defun gnosis-journal--assert-destination (file db dir)
  "Signal if the journal destination is no longer FILE, DB and DIR."
  (unless (and (equal (gnosis-journal--file) file)
               (eq gnosis-db db)
               (equal (expand-file-name (gnosis-journal--directory))
                      (expand-file-name dir)))
    (user-error "Journal destination changed during template selection")))

(defun gnosis-journal--buffer-state (file)
  "Return FILE's visiting buffer identity and edit state, or nil."
  (when-let* ((buffer (and file (get-file-buffer file))))
    (with-current-buffer buffer
      (list buffer buffer-file-name major-mode (buffer-chars-modified-tick)))))

(defun gnosis-journal--assert-buffer-state (file state)
  "Signal if FILE's visiting buffer no longer matches STATE."
  (unless (equal state (gnosis-journal--buffer-state file))
    (user-error "Journal buffer changed during input")))

(defun gnosis-journal--template-text (name date)
  "Evaluate template NAME for DATE without changing the caller's point.
Signal on unknown names or non-string results."
  (let ((function (cdr (assoc name gnosis-journal-templates)))
        (gnosis-journal-template-date date))
    (unless (and (stringp name) (functionp function))
      (user-error "Unknown journal template: %s" name))
    (let ((text (save-excursion (funcall function))))
      (unless (stringp text)
        (user-error "Journal template must return a string"))
      text)))

(defun gnosis-journal--read-template (file db dir &optional title)
  "Prepare a journal template without writing.
FILE, DB and DIR are the captured destination.  Dated TITLE uses
`gnosis-journal-new-entry-template'; other titles prompt as before."
  (let* ((state (gnosis-journal--buffer-state file))
         (date (gnosis-journal--title-date title))
         (template
          (if date
              (when gnosis-journal-new-entry-template
                (gnosis-journal--template-text
                 gnosis-journal-new-entry-template date))
            (save-excursion
              (gnosis-nodes-select-template gnosis-journal-templates)))))
    (gnosis-journal--assert-destination file db dir)
    (gnosis-journal--assert-buffer-state file state)
    template))

(defun gnosis-journal--prepared-body (template heading-p)
  "Return expanded TEMPLATE text, or nil if empty.
HEADING-P non-nil expands relative to a level-1 heading.  Signal
if TEMPLATE is not a string."
  (cond ((null template) nil)
        ((not (stringp template))
         (user-error "Journal template must return a string"))
        ((string-empty-p template) nil)
        (t
         (let ((body (gnosis-org-expand-headings template (if heading-p 2 1))))
           (gnosis-journal--validate-body body heading-p)
           body))))

(defun gnosis-journal--validate-body (body heading-p &optional existing)
  "Validate BODY's roles combined with EXISTING dated text, without edits.
HEADING-P means a dated heading rather than a file-level entry."
  (with-temp-buffer
    (org-mode)
    (insert (or existing (if heading-p "* Date\n" "#+title: Date\n")))
    (unless (bolp) (insert "\n"))
    (insert body)
    (gnosis-journal--role-position 'thoughts heading-p)))

(defun gnosis-journal--ensure-buffer (file)
  "Visit FILE, inserting journal metadata when the buffer is new and empty.
Do not write FILE to disk; native save handles encryption."
  (make-directory (file-name-directory file) t)
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (unless (or (file-exists-p file) (> (buffer-size) 0))
        (insert (format "#+title: %s Journal\n#+filetags: \n"
                        (or user-full-name ""))))
      buffer)))

(cl-defun gnosis-journal--create-heading (title &optional (prepared nil prepared-p))
  "Append a level-1 heading TITLE in the single journal file.
Widen before appending so a narrowed subtree is not the insertion
point, and leave the buffer widened on the new heading.
Use PREPARED template text when PREPARED-P is non-nil."
  (pcase-let* ((`(,file ,db ,dir) (gnosis-journal--destination))
               (template (if prepared-p prepared
                           (gnosis-journal--read-template file db dir title)))
               (body (gnosis-journal--prepared-body template t)))
    (unless file
      (user-error "No single journal file is configured"))
    (gnosis-journal--assert-destination file db dir)
    (gnosis-journal--ensure-buffer file)
    (find-file file)
    (widen)
    (if (gnosis-journal--goto-heading title)
        (gnosis-nodes-mode 1)
      (atomic-change-group
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (format "* %s\n" title))
        (org-id-get-create)
        (when body (insert body)))
      (gnosis-nodes-mode 1))))

(cl-defun gnosis-journal--create-separate (title &optional (prepared nil prepared-p))
  "Create a separate journal file for TITLE after template selection.
Use PREPARED template text when PREPARED-P is non-nil."
  (pcase-let* ((`(,file ,db ,dir) (gnosis-journal--destination))
               (template (if prepared-p prepared
                           (gnosis-journal--read-template file db dir title)))
               (body (gnosis-journal--prepared-body template nil)))
    (gnosis-journal--assert-destination file db dir)
    (gnosis-nodes--create-file title (gnosis-journal--dir) body)))

(defun gnosis-journal--visit-or-create (target)
  "Visit TARGET, a selected entry or title, creating a new title if needed."
  (if-let* ((entry (if (stringp target)
                      (gnosis-journal--unique-entry target)
                    target)))
      (gnosis-journal--goto-entry entry)
    (if (gnosis-journal--file)
        (gnosis-journal--create-heading target)
      (gnosis-journal--create-separate target))))

;;; Task collection

(defun gnosis-journal--todo-files (&optional files)
  "Return regular Org files from FILES, expanding directories.
FILES defaults to `gnosis-journal-todo-files'."
  (cl-delete-duplicates
   (mapcar #'expand-file-name
           (cl-loop for file in (or files gnosis-journal-todo-files)
                    append (cond
                            ((null file) nil)
                            ((file-directory-p file)
                             (cl-remove-if-not
                              #'gnosis-nodes--org-file-p
                              (directory-files file t nil t)))
                            ((file-regular-p file) (list file)))))
   :test #'gnosis-journal--physical-equal))

(defun gnosis-journal--collect-todos (file live)
  "Return TODO records from the current Org buffer for FILE.
LIVE non-nil records marker and tick instead of a content digest.
Each item is (TITLE TIMESTAMP FILE ID BEGIN MARKER TICK LINE DIGEST)."
  (let ((digest (unless live (secure-hash 'sha1 (current-buffer))))
        (tick (and live (buffer-chars-modified-tick)))
        todos)
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (when (member (org-element-property :todo-keyword headline)
                      gnosis-journal-todo-keywords)
          (let* ((title (org-element-property :raw-value headline))
                 (timestamp (org-element-property
                             :raw-value
                             (org-element-property :scheduled headline)))
                 (id (org-element-property :ID headline))
                 (begin (org-element-property :begin headline))
                 (line (save-excursion
                         (goto-char begin)
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))))
            (push (list title timestamp file id begin
                        (and live (copy-marker begin t))
                        tick line digest)
                  todos)))))
    (nreverse todos)))

(defun gnosis-journal-get--todos (file)
  "Return TODO items for FILE.
Each item is (TITLE TIMESTAMP FILE ID BEGIN MARKER TICK LINE DIGEST).
MARKER and TICK are set only for a live Org buffer.  DIGEST is a
content hash for disk-only or non-Org live buffers.  Prefer a live
visiting buffer over disk.  Do not change the source major mode,
point, or narrowing."
  (if-let* ((buf (get-file-buffer file)))
      (with-current-buffer buf
        (save-excursion
          (save-restriction
            (widen)
            (if (derived-mode-p 'org-mode)
                (gnosis-journal--collect-todos file t)
              (let ((text (buffer-substring-no-properties (point-min)
                                                          (point-max))))
                (with-temp-buffer
                  (insert text)
                  (org-mode)
                  (gnosis-journal--collect-todos file nil)))))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (gnosis-journal--collect-todos file nil))))

(defun gnosis-journal-get-todos (&optional files)
  "Get TODO items for FILES."
  (let ((files (gnosis-journal--todo-files files))
        todos)
    (cl-loop for file in files
             do (push (gnosis-journal-get--todos file) todos))
    (nreverse (apply #'append todos))))

(defun gnosis-journal-todos (&optional date)
  "Return unscheduled and DATE-scheduled tasks as checkboxes.
DATE is an ISO calendar date, defaulting to `gnosis-journal-template-date'
or today outside template expansion.
Headings with source IDs are written as ID links; headings without
IDs remain plain text and cannot complete external tasks."
  (let ((todos (gnosis-journal-get-todos))
        (current-date (or date gnosis-journal-template-date
                          (format-time-string "%Y-%m-%d")))
        todos-string)
    (unless (gnosis-journal--iso-date-p current-date)
      (user-error "Not a calendar date: %s" current-date))
    (cl-loop for todo in todos
             do
             (pcase todo
               (`(,todo-title ,todo-timestamp ,_file ,id . ,_)
                (when (or (null todo-timestamp)
                          (string-match-p (regexp-quote current-date)
                                          todo-timestamp))
                  (setq todos-string
                        (concat todos-string
                                (format "%s [ ] %s\n"
                                        gnosis-journal-bullet-point-char
                                        (if id
                                            (org-link-make-string
                                             (concat "id:" id) todo-title)
                                          todo-title))))))))
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
  "Find first org file in FILES containing heading TITLE.
This helper no longer drives task completion."
  (declare (obsolete nil "0.11.0"))
  (catch 'found
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (goto-char (point-min))
        (when (org-find-exact-headline-in-buffer title)
          (throw 'found file))))))

(defun gnosis-journal-mark-todo-as-done (_todo-title)
  "Obsolete title-based task completion.
Saving a journal no longer completes external tasks by title.
Use `gnosis-journal-complete-task' on a source ID link."
  (declare (obsolete gnosis-journal-complete-task "0.11.0"))
  (user-error "Journal no longer completes tasks by title; \
use `gnosis-journal-complete-task' on an ID link"))

(defun gnosis-journal--update-todos (&rest _)
  "Obsolete save-hook task completion.
INDEX-ONLY callers of `gnosis-nodes-update-file' remain valid;
this function no longer mutates task files."
  (declare (obsolete gnosis-journal-complete-task "0.11.0"))
  nil)

;;; Interactive commands

;;;###autoload
(defun gnosis-journal-find (&optional title)
  "Find journal entry for TITLE."
  (interactive)
  (gnosis-journal--visit-or-create (or title (gnosis-journal--read-entry))))

;;;###autoload
(defun gnosis-journal-insert (arg)
  "Insert an ID link to a journal entry.
If called with prefix ARG, use a custom link description.
Create the entry if needed.  Do not save the journal."
  (interactive "P")
  (gnosis-journal--insert-link arg))

(defun gnosis-journal--insert-link (arg)
  "Insert an ID link to a journal entry, creating it if needed.
ARG non-nil prompts for a custom description.  Do not save an
already dirty journal buffer.  Validate the selected target in
its source, then restore origin point and restriction before inserting."
  (let* ((node (gnosis-journal--read-entry))
         (title (if (stringp node) node (nth 1 node)))
         (desc (cond ((use-region-p)
                      (buffer-substring-no-properties
                       (region-beginning) (region-end)))
                     (arg (read-string "Description: "))
                     (t title)))
         (origin (current-buffer))
         id)
    (save-window-excursion
      (save-excursion
        (save-restriction
          (gnosis-journal--visit-or-create node)
          (setq id (or (org-id-get) (org-id-get-create))))))
    (unless id
      (user-error "Journal entry %s has no ID" node))
    (with-current-buffer origin
      (org-insert-link nil (format "id:%s" id) desc))))

;;;###autoload
(defun gnosis-journal ()
  "Journal for current date."
  (interactive)
  (gnosis-journal-find (format-time-string "%Y-%m-%d")))

;;;###autoload
(defun gnosis-journal-date (&optional date)
  "Open the journal entry for DATE.
DATE is an ISO 8601 calendar date.  Prompt with `org-read-date'
when called interactively."
  (interactive (list (org-read-date nil nil nil "Journal date: ")))
  (unless (gnosis-journal--iso-date-p date)
    (user-error "Not a calendar date: %s" date))
  (gnosis-journal-find date))

(defun gnosis-journal--dated-titles ()
  "Return sorted unique ISO dates that already have journal entries.
Use inspected source plus remaining index rows, without reading archives."
  (sort (delete-dups
         (delq nil (mapcar #'car (gnosis-journal--entries))))
        #'string<))

(defun gnosis-journal--visit-existing-date (date)
  "Visit an existing dated entry DATE without creating one."
  (let ((entry (gnosis-journal--unique-entry date)))
    (unless entry
      (user-error "No journal entry for %s" date))
    (gnosis-journal--goto-entry entry)))

;;;###autoload
(defun gnosis-journal-previous ()
  "Visit the previous existing dated journal entry.
Skip missing dates.  Do not create an entry."
  (interactive)
  (let* ((date (or (gnosis-journal--date-at-point)
                   (format-time-string "%Y-%m-%d")))
         (previous (cl-loop for candidate in (nreverse (gnosis-journal--dated-titles))
                            when (string< candidate date)
                            return candidate)))
    (unless previous
      (user-error "No earlier journal entry"))
    (gnosis-journal--visit-existing-date previous)))

;;;###autoload
(defun gnosis-journal-next ()
  "Visit the next existing dated journal entry.
Skip missing dates.  Do not create an entry."
  (interactive)
  (let* ((date (or (gnosis-journal--date-at-point)
                   (format-time-string "%Y-%m-%d")))
         (next (cl-loop for candidate in (gnosis-journal--dated-titles)
                        when (string> candidate date)
                        return candidate)))
    (unless next
      (user-error "No later journal entry"))
    (gnosis-journal--visit-existing-date next)))

;;;###autoload
(defun gnosis-journal-insert-template (&optional name)
  "Append template NAME to the dated entry at point, without saving.
Prompt for a name from `gnosis-journal-templates' when NAME is nil.
Expand headings relative to the entry, not the section at point.
Leave point on a blank line after nonempty templates, within the date.
Preserve existing text and IDs.  Quit or invalid input inserts nothing."
  (interactive)
  (let* ((date (or (gnosis-journal--date-at-point)
                   (user-error "No dated journal entry at point")))
         (file buffer-file-name)
         (entry (gnosis-journal--unique-entry date))
         (destination (gnosis-journal--destination))
         (state (gnosis-journal--buffer-state file))
         (config (gnosis-journal--capture-config)))
    (unless (and file entry (equal file (nth 2 entry)))
      (user-error "Not in the selected journal entry"))
    (let ((name (or name (funcall gnosis-nodes-completing-read-func
                                 "Insert journal template: "
                                 (mapcar #'car gnosis-journal-templates)))))
      (unless (equal config (gnosis-journal--capture-config))
        (user-error "Journal configuration changed during input"))
      (apply #'gnosis-journal--assert-destination destination)
      (gnosis-journal--assert-buffer-state file state)
      (let ((text (with-current-buffer (car state)
                    (gnosis-journal--template-text name date))))
        (apply #'gnosis-journal--assert-destination destination)
        (gnosis-journal--assert-buffer-state file state)
        (gnosis-journal--goto-entry entry)
        (let* ((heading-p (org-current-level))
               (body (gnosis-journal--prepared-body text heading-p)))
          (when body
            (gnosis-journal--validate-body
             body heading-p
             (save-restriction
               (when heading-p (org-narrow-to-subtree))
               (buffer-substring-no-properties (point-min) (point-max)))))
          (unless (equal config (gnosis-journal--capture-config))
            (user-error "Journal configuration changed during input"))
          (apply #'gnosis-journal--assert-destination destination)
          (gnosis-journal--assert-buffer-state file state)
          (when body
            (atomic-change-group
              (if (org-current-level)
                  (org-end-of-subtree t t)
                (goto-char (point-max)))
              (unless (bolp) (insert "\n"))
              (insert body)
              (unless (bolp) (insert "\n"))
              ;; Keep a writable line before the next date, not on metadata.
              (unless (eobp)
                (backward-char 1)
                (unless (bolp) (insert "\n"))))))))))

(defun gnosis-journal--role-position (role &optional heading-p)
  "Return (POSITION . FALLBACK) for ROLE in this complete dated restriction.
HEADING-P means the restriction must be exactly one heading subtree;
otherwise it is a dated file.  ROLE is `thoughts' or `todos'.  Validate
all non-inherited GNOSIS_JOURNAL_ROLES declarations, including duplicate
keys and destinations.  Append to the heading's own body before children.
No matching role means the date body and non-nil FALLBACK.
Use native Org boundaries: valid blocks and drawers are opaque; unescaped
star lines are headings, not literal examples.  Do not lint free prose."
  (unless (memq role '(thoughts todos))
    (user-error "Unknown journal role: %s" role))
  (save-excursion
    (save-match-data
      (let* ((tree (org-element-parse-buffer))
             (headlines (org-element-map tree 'headline #'identity))
             (root (and heading-p (car headlines)))
             destinations)
        (when (and heading-p
                   (or (null root)
                       (/= (org-element-property :begin root) (point-min))
                       (/= (org-element-property :end root) (point-max))))
          (user-error "Narrow to exactly one dated journal subtree"))
        (org-element-map tree 'paragraph
          (lambda (paragraph)
            (when (eq (org-element-type (org-element-property :parent paragraph))
                      'section)
              (goto-char (org-element-property :begin paragraph))
              (when (let ((case-fold-search t))
                      (re-search-forward "^[ \t]*:GNOSIS_JOURNAL_ROLES[+:]"
                                         (org-element-property :end paragraph) t))
                (user-error "Put GNOSIS_JOURNAL_ROLES in the heading's property drawer")))))
        (org-element-map tree 'node-property
          (lambda (property)
            (when (equal (upcase (org-element-property :key property))
                         "GNOSIS_JOURNAL_ROLES+")
              (user-error "Consolidate additive declarations into one GNOSIS_JOURNAL_ROLES property"))))
        (dolist (headline headlines)
          (let* ((section (car (org-element-contents headline)))
                 (properties
                  (and (eq (org-element-type section) 'section)
                       (org-element-map section 'node-property
                         (lambda (property)
                           (when (equal (upcase (org-element-property :key property))
                                        "GNOSIS_JOURNAL_ROLES")
                             property)))))
                 (value (and properties
                             (org-element-property :value (car properties))))
                 (roles (and value (split-string value nil t)))
                 (title (org-element-property :raw-value headline)))
            (when (cdr properties)
              (user-error "Duplicate GNOSIS_JOURNAL_ROLES keys on %s; keep one" title))
            (when (and properties
                       (or (null roles)
                           (/= (length roles) (length (delete-dups (copy-sequence roles))))
                           (seq-some (lambda (item)
                                       (not (member item '("thoughts" "todos"))))
                                     roles)))
              (user-error "Invalid GNOSIS_JOURNAL_ROLES on %s; use thoughts, todos or thoughts todos" title))
            (dolist (item roles)
              (when (assoc item destinations)
                (user-error "Multiple %s destinations; keep one GNOSIS_JOURNAL_ROLES declaration for this role" item))
              (push (cons item headline) destinations))))
        (let* ((match (cdr (assoc (symbol-name role) destinations)))
               (target (or match root))
               (begin (and target (org-element-property :begin target)))
               (next (seq-find
                      (lambda (headline)
                        (or (null begin)
                            (> (org-element-property :begin headline) begin)))
                      headlines)))
          (cons (if next (org-element-property :begin next) (point-max))
                (not match)))))))

(defun gnosis-journal--writing-position (role)
  "Move from the dated root to ROLE's own-body insertion position.
Announce a missing role's date-body fallback.  Never search sibling dates."
  (let ((target
         (save-restriction
           (let ((heading-p (org-current-level)))
             (when heading-p (org-narrow-to-subtree))
             (gnosis-journal--role-position role heading-p)))))
    (goto-char (car target))
    (when (cdr target)
      (message "No %s journal role; using the date body" role))))

(defun gnosis-journal--capture-config ()
  "Return the configuration that owns an in-progress journal capture.
Keep template function identity, not a copy of closure-local state."
  (list (mapcar (lambda (value)
                  (if (stringp value) (copy-sequence value) value))
                (append (gnosis-journal--destination)
                        (list gnosis-journal-as-gpg gnosis-nodes-timestring
                              gnosis-nodes-create-as-gpg
                              gnosis-journal-bullet-point-char
                              gnosis-journal-new-entry-template)))
        (mapcar (lambda (item)
                  (list (copy-sequence (car item)) (cdr item)
                        (and (symbolp (cdr item))
                             (fboundp (cdr item))
                             (symbol-function (cdr item)))))
                gnosis-journal-templates)))

(defun gnosis-journal--capture-context ()
  "Capture date, entry, file, buffer and configuration before any input.
Visit an existing target without changing its contents."
  (let* ((date (or (and buffer-file-name
                       (gnosis-nodes--journal-file-p buffer-file-name)
                       (gnosis-journal--date-at-point))
                  (format-time-string "%Y-%m-%d")))
         (entry (gnosis-journal--unique-entry date))
         (file (or (nth 2 entry) (gnosis-journal--file))))
    (when (and file (file-exists-p file) (not (get-file-buffer file)))
      (find-file-noselect file))
    (list :date date :entry entry :file file
          :state (gnosis-journal--buffer-state file)
          :config (gnosis-journal--capture-config))))

(defun gnosis-journal--assert-capture (context)
  "Refuse to retarget CONTEXT after recursive input or source effects."
  (unless (equal (plist-get context :entry)
                 (gnosis-journal--unique-entry (plist-get context :date)))
    (user-error "Journal date changed during input"))
  (unless (equal (plist-get context :config) (gnosis-journal--capture-config))
    (user-error "Journal configuration changed during input"))
  (gnosis-journal--assert-buffer-state
   (plist-get context :file) (plist-get context :state)))

(defun gnosis-journal--capture-template (context role)
  "Preflight CONTEXT's ROLE and return a prepared new-date template, or nil.
Perform all template prompts and routing checks before source ID effects."
  (gnosis-journal--assert-capture context)
  (let ((template
         (if-let* ((entry (plist-get context :entry)))
             (save-window-excursion
               (with-current-buffer (car (plist-get context :state))
                 (save-excursion
                   (save-restriction
                     (gnosis-journal--goto-entry entry)
                     (let ((heading-p (org-current-level)))
                       (when heading-p (org-narrow-to-subtree))
                       (gnosis-journal--role-position role heading-p))
                     nil))))
           (let ((text (apply #'gnosis-journal--read-template
                              (append (gnosis-journal--destination)
                                      (list (plist-get context :date))))))
             (gnosis-journal--prepared-body text (gnosis-journal--file))
             text))))
    (gnosis-journal--assert-capture context)
    template))

(defun gnosis-journal--create-capture (context role text template)
  "Create CONTEXT's missing date with ROLE's TEXT and prepared TEMPLATE.
Settle native buffer initialization before generating dated content.
Keep initialization edits and confirmed source IDs outside the atomic
insertion.  Native ID callback text edits share its rollback; file and
mode changes are not undone."
  (let* ((date (plist-get context :date))
         (single (gnosis-journal--file))
         (body (gnosis-journal--prepared-body template single))
         (file (or single
                   (expand-file-name
                    (gnosis-org--create-name
                     date nil gnosis-journal-as-gpg
                     gnosis-nodes-create-as-gpg gnosis-nodes-timestring)
                    (gnosis-journal--dir))))
         (before (gnosis-journal--buffer-state file)))
    (gnosis-journal--assert-capture context)
    (make-directory (file-name-directory file) t)
    (switch-to-buffer (find-file-noselect file))
    (widen)
    (unless gnosis-nodes-mode (gnosis-nodes-mode 1))
    (unless (and (derived-mode-p 'org-mode)
                 (equal buffer-file-name file))
      (user-error "Journal buffer changed during initialization"))
    (unless (equal (plist-get context :config) (gnosis-journal--capture-config))
      (user-error "Journal configuration changed during input"))
    (when before (gnosis-journal--assert-buffer-state file before))
    (when (gnosis-journal--unique-entry date)
      (user-error "Journal date changed during input"))
    (unless (or single (and (not (file-exists-p file)) (= (buffer-size) 0)))
      (user-error "Journal file is not empty; preserve its contents before retrying"))
    (let ((owner (current-buffer))
          (mode major-mode))
      (atomic-change-group
        (when (and single (= (buffer-size) 0))
          (insert (format "#+title: %s Journal\n#+filetags: \n"
                          (or user-full-name ""))))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (let ((begin (point))
              (org-id-track-globally nil))
          (insert (if single (format "* %s\n" date)
                    (format "#+title: %s\n#+filetags: \n" date)))
          (goto-char begin)
          (org-id-get-create)
          (unless (and (eq (current-buffer) owner)
                       (eq (get-file-buffer file) owner)
                       (equal buffer-file-name file)
                       (eq major-mode mode))
            (user-error "Journal buffer changed during ID creation"))
          (goto-char (point-max))
          (when body (insert body))
          (goto-char begin)
          (gnosis-journal--writing-position role)
          (unless (equal (plist-get context :config) (gnosis-journal--capture-config))
            (user-error "Journal configuration changed during input"))
          (unless (bolp) (insert "\n"))
          (insert text "\n"))))))

(cl-defun gnosis-journal--insert-capture
    (context role text &optional (prepared nil prepared-p))
  "Insert TEXT under ROLE for the validated CONTEXT, without saving.
Evaluate an automatic template only once, before creating a missing date.
Use PREPARED text when PREPARED-P is non-nil."
  (gnosis-journal--assert-capture context)
  (let* ((entry (plist-get context :entry))
         (template (if prepared-p prepared
                     (gnosis-journal--capture-template context role))))
    (gnosis-journal--assert-capture context)
    (if (not entry)
        (gnosis-journal--create-capture context role text template)
      (gnosis-journal--goto-entry entry)
      (gnosis-journal--assert-capture context)
      (let ((file buffer-file-name)
            (state (gnosis-journal--buffer-state buffer-file-name)))
        (gnosis-journal--writing-position role)
        (gnosis-journal--assert-capture context)
        (gnosis-journal--assert-buffer-state file state)
        (atomic-change-group
          (unless (bolp) (insert "\n"))
          (insert text "\n"))))))

(defun gnosis-journal--capture-text (role note)
  "Read NOTE if nil and append it under ROLE with native draft recovery."
  (let* ((context (gnosis-journal--capture-context))
         (note (or note (read-string-from-buffer
                        (if (eq role 'thoughts) "Thought " "Todo ") ""))))
    (when (string-empty-p (string-trim note))
      (user-error "Journal note is empty"))
    (condition-case err
        (gnosis-journal--insert-capture
         context role
         (concat (if (eq role 'thoughts)
                     (format-time-string "- %H:%M ")
                   (concat gnosis-journal-bullet-point-char " [ ] "))
                 note))
      ((error quit)
       (let ((buffer (generate-new-buffer "*Gnosis journal recovery*")))
         (with-current-buffer buffer
           (insert note)
           (setq-local header-line-format
                       "Journal capture refused; copy this draft to retry"))
         (display-buffer buffer)
         (message "Journal draft retained in %s" (buffer-name buffer)))
       (signal (car err) (cdr err))))))

;;;###autoload
(defun gnosis-journal-capture (&optional note)
  "Append timestamped NOTE to this journal date, or today's outside journals.
Read NOTE with native buffer input when nil.  A heading's non-inherited
GNOSIS_JOURNAL_ROLES property selects thoughts; absent roles use the date
body, with a notice.  Refuse ambiguous roles without edits.  Retain an
accepted draft in a recovery buffer if later validation fails.  Do not save."
  (interactive)
  (gnosis-journal--capture-text 'thoughts note))

;;;###autoload
(defun gnosis-journal-add-todo (&optional note)
  "Append local checkbox NOTE to this journal date, or today's.
Read NOTE with native buffer input when nil.  Use the non-inherited todos
GNOSIS_JOURNAL_ROLES destination, or the date body with a notice.
Retain accepted drafts on refusal.  Do not create external tasks or save."
  (interactive)
  (gnosis-journal--capture-text 'todos note))

;;;###autoload
(defun gnosis-journal-insert-task ()
  "Insert a linked source-task checkbox under this date's todos role.
Use today outside a journal.  If the source has no ID, confirm creating
one at its captured position, including in this journal buffer.  Display
source ID edits without saving them; a later refusal leaves those explicit
edits unsaved.  Do not complete tasks or save the journal."
  (interactive)
  (let* ((context (gnosis-journal--capture-context))
         (todos (gnosis-journal-get-todos))
         (source-modes
          (mapcar (lambda (todo)
                    (when-let* ((buffer (get-file-buffer (nth 2 todo))))
                      (cons buffer (buffer-local-value 'major-mode buffer))))
                  todos))
         (candidates
          (cl-loop for todo in todos
                   collect
                   (pcase todo
                     (`(,title ,_ts ,file ,id ,begin . ,_)
                      (cons (format "%s (%s:%s)%s"
                                    title
                                    file
                                    begin
                                    (if id "" " [no ID]"))
                            todo)))))
         (choice (funcall gnosis-nodes-completing-read-func
                          "Insert task: " (mapcar #'car candidates)))
         (todo (cdr (assoc choice candidates))))
    (unless todo
      (user-error "No task selected"))
    (pcase todo
      (`(,title ,_ts ,file ,id ,begin ,marker ,tick ,line ,digest)
       (gnosis-journal--assert-capture context)
       (unless (or id (y-or-n-p (format "Create an Org ID on %s? " title)))
         (user-error "Canceled"))
       (let ((template (gnosis-journal--capture-template context 'todos)))
         (unless id
           (let* ((state (plist-get context :state))
                  (same (and state marker (eq (car state) (marker-buffer marker))))
                  (position (and same (marker-position marker)))
                  (before (and same
                               (with-current-buffer (car state)
                                 (save-restriction
                                   (widen)
                                   (buffer-substring-no-properties
                                    (point-min) (point-max)))))))
             (setq id (gnosis-journal--create-task-id
                       title file begin marker tick line digest
                       (cdr (assq (get-file-buffer file) source-modes))))
             (when same
               (gnosis-journal--accept-task-id context before position id))))
         (gnosis-journal--insert-capture
          context 'todos
          (format "%s [ ] %s" gnosis-journal-bullet-point-char
                  (org-link-make-string (concat "id:" id) title))
          template))))))

(defun gnosis-journal--accept-task-id (context before position id)
  "Accept only the authorized ID delta at POSITION in CONTEXT's owner.
Compare native property insertion into BEFORE with the entire resulting
buffer.  Do not bless unrelated edits, buffer replacement or mode changes."
  (let* ((file (plist-get context :file))
         (old (plist-get context :state))
         (new (gnosis-journal--buffer-state file))
         (expected (with-temp-buffer
                     (org-mode)
                     (insert before)
                     (goto-char position)
                     (org-entry-put nil "ID" id)
                     (buffer-string))))
    (unless (and (equal (seq-take old 3) (seq-take new 3))
                 (with-current-buffer (car old)
                   (save-restriction
                     (widen)
                     (equal expected (buffer-substring-no-properties
                                      (point-min) (point-max))))))
      (user-error "Journal changed beyond the confirmed source ID; insertion refused"))
    (setf (plist-get context :state) new)
    (let* ((entry (plist-get context :entry))
           (current (and entry (gnosis-journal--unique-entry (car entry)))))
      (when (and entry (null (nth 3 entry))
                 (equal (seq-take entry 3) (seq-take current 3))
                 (equal (nth 3 current) id))
        (setf (plist-get context :entry) current)))))

(defun gnosis-journal--create-task-id (title file begin marker tick line digest
                                          &optional mode)
  "Create an Org ID on the captured source heading and return it.
TITLE, FILE, BEGIN, MARKER, TICK, LINE and DIGEST identify the
captured source.  Require Org mode and the captured MODE when non-nil.
Fail atomically if the source buffer or snapshot changed."
  (when (and marker (not (marker-buffer marker)))
    (user-error "Task heading buffer is gone"))
  (with-current-buffer (if marker (marker-buffer marker)
                         (find-file-noselect file))
    (unless (and (derived-mode-p 'org-mode)
                 (or (null mode) (eq major-mode mode)))
      (user-error "Task source mode changed; return to Org mode and retry"))
    (unless (equal (expand-file-name (or buffer-file-name ""))
                   (expand-file-name file))
      (user-error "Task buffer is no longer %s" file))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (or marker begin))
        (unless (if marker
                    (and tick (= (buffer-chars-modified-tick) tick))
                  (and digest
                       (equal (secure-hash 'sha1 (current-buffer)) digest)
                       (equal (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position))
                              line)))
          (user-error "Task heading changed: %s" title))
        (let ((id (atomic-change-group (org-id-get-create))))
          (display-buffer (current-buffer))
          id)))))

(defun gnosis-journal--id-link-at-point ()
  "Return the Org ID of the link at point, or nil."
  (let ((link (org-element-lineage (org-element-context) '(link) t)))
    (when (and link (equal (org-element-property :type link) "id"))
      (org-element-property :path link))))

(defun gnosis-journal--id-links-on-line ()
  "Return ID link paths on the current line."
  (save-excursion
    (let ((end (line-end-position))
          ids)
      (beginning-of-line)
      (while (re-search-forward org-link-bracket-re end t)
        (save-excursion
          (goto-char (match-beginning 0))
          (let ((link (org-element-lineage (org-element-context) '(link) t)))
            (when (and link (equal (org-element-property :type link) "id"))
              (push (org-element-property :path link) ids)))))
      (nreverse ids))))

(defun gnosis-journal--task-id-at-point ()
  "Return the selected source Org ID, or nil.
If point is on an ID link, use that ID.  If point is on another
link, return nil.  Outside a link, use the ID on this line when
it is unique."
  (let ((link (org-element-lineage (org-element-context) '(link) t)))
    (cond (link
           (and (equal (org-element-property :type link) "id")
                (org-element-property :path link)))
          (t
           (let ((ids (delete-dups (gnosis-journal--id-links-on-line))))
             (and (= (length ids) 1) (car ids)))))))

(defun gnosis-journal--id-positions (id)
  "Return beginning positions of ID in the current buffer."
  (let (positions)
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (when (equal (org-element-property :ID headline) id)
          (push (org-element-property :begin headline) positions))))
    (save-excursion
      (goto-char (point-min))
      (when (and (org-before-first-heading-p)
                 (equal (org-entry-get nil "ID") id))
        (push (point-min) positions)))
    (nreverse (delete-dups positions))))

(defun gnosis-journal--goto-id (id)
  "Move to ID in the current buffer.  Signal if duplicated.
Return non-nil if found."
  (let ((positions (gnosis-journal--id-positions id)))
    (cond ((null positions) nil)
          ((cdr positions)
           (user-error "Duplicate source ID %s" id))
          (t (goto-char (car positions)) t))))

(defun gnosis-journal--locate-task-id (id)
  "Return the configured task file containing ID, or nil.
Signal if ID appears more than once, including within one file."
  (let (found)
    (dolist (file (gnosis-journal--todo-files))
      (gnosis-journal--parse-file
       file
       (lambda ()
         (let ((positions (gnosis-journal--id-positions id)))
           (cond ((cdr positions)
                  (user-error "Duplicate source ID %s" id))
                 (positions
                  (when found
                    (user-error "Duplicate source ID %s" id))
                  (setq found file)))))))
    found))

(defun gnosis-journal--live-journal-buffers ()
  "Return live buffers visiting journal files, without opening files."
  (let ((single (ignore-errors (gnosis-journal--configured-file)))
        (dir (gnosis-journal--directory))
        buffers)
    (dolist (buf (buffer-list))
      (when-let* ((file (buffer-file-name buf)))
        (when (or (and single (gnosis-journal--physical-equal file single))
                  (and dir (file-directory-p dir)
                       (file-in-directory-p file dir)))
          (cl-pushnew buf buffers :test #'eq))))
    (nreverse buffers)))

(defun gnosis-journal--live-id-location (id)
  "Return (BUFFER POSITION) for ID in a live journal buffer, or nil.
Do not visit or decrypt unopened files."
  (catch 'found
    (dolist (buf (gnosis-journal--live-journal-buffers))
      (with-current-buffer buf
        (when (derived-mode-p 'org-mode)
          (save-excursion
            (save-restriction
              (widen)
              (let ((positions (gnosis-journal--id-positions id)))
                (cond ((cdr positions)
                       (user-error "Duplicate journal ID %s" id))
                      (positions
                       (throw 'found (list buf (car positions)))))))))))
    nil))

(defun gnosis-journal--goto-live-id (id)
  "Visit ID in a live journal buffer.  Return non-nil if found.
Widen and keep that view so a narrowed target remains current."
  (when-let* ((loc (gnosis-journal--live-id-location id)))
    (pop-to-buffer (car loc))
    (widen)
    (goto-char (cadr loc))
    (gnosis-nodes-mode 1)
    t))

(defun gnosis-journal--open-live-id ()
  "Follow a journal ID at point from a live journal buffer.
Return non-nil when this function handled the link.  Do not open
unvisited journal files."
  (when-let* ((id (gnosis-journal--id-link-at-point)))
    (gnosis-journal--goto-live-id id)))

(defun gnosis-journal--open-task-id ()
  "Follow an ID link through configured task files.
Return nil so Org can handle journal and node IDs.  Recheck that
the current buffer is still a journal file."
  (when (and buffer-file-name
             (gnosis-nodes--journal-file-p buffer-file-name))
    (when-let* ((id (gnosis-journal--task-id-at-point))
                (file (gnosis-journal--locate-task-id id)))
      (find-file file)
      (widen)
      (gnosis-journal--goto-id id))))

(defun gnosis-journal--setup-buffer ()
  "Follow configured task IDs from journal buffers."
  (when (and buffer-file-name
             (gnosis-nodes--journal-file-p buffer-file-name))
    (add-hook 'org-open-at-point-functions
              #'gnosis-journal--open-task-id nil t)))

(add-hook 'org-mode-hook #'gnosis-journal--setup-buffer)
(add-hook 'org-open-at-point-functions #'gnosis-journal--open-live-id)

;;;###autoload
(defun gnosis-journal-complete-task ()
  "Complete the source task linked at point after confirmation.
Act on the exact current source ID with `org-todo'.  Do not save
the source buffer, so pre-existing unsaved edits stay unsaved.
The source buffer is displayed so its dirty state is visible.
Plain checkboxes without ID links are journal prose."
  (interactive)
  (let* ((id (or (gnosis-journal--task-id-at-point)
                 (user-error "No source ID link at point")))
         (file (or (gnosis-journal--locate-task-id id)
                   (user-error
                    "Source ID %s is not in `gnosis-journal-todo-files'"
                    id)))
         (buffer (or (get-file-buffer file)
                     (find-file-noselect file)))
         title mode state)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (unless (gnosis-journal--goto-id id)
          (user-error "Source ID %s is missing" id))
        (setq title (org-get-heading t t t t)
              mode major-mode
              state (org-get-todo-state))
        (unless (member state gnosis-journal-todo-keywords)
          (user-error "Source ID %s is not an active task" id))))
    (unless (y-or-n-p (format "Mark task %s as done? " title))
      (user-error "Canceled"))
    (unless (and (buffer-live-p buffer)
                 (equal (buffer-file-name buffer)
                        (expand-file-name file)))
      (user-error "Source task buffer changed"))
    (with-current-buffer buffer
      (unless (eq major-mode mode)
        (user-error "Source task buffer changed"))
      (save-restriction
        (widen)
        (unless (gnosis-journal--goto-id id)
          (user-error "Source ID %s is missing" id))
        (unless (equal (org-get-todo-state) state)
          (user-error "Source task changed"))
        (org-todo 'done))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun gnosis-journal-study ()
  "Open the read-only study view for this journal entry's date."
  (interactive)
  (let ((date (or (gnosis-journal--date-at-point)
                  (user-error "No dated journal entry at point"))))
    (require 'gnosis-study)
    (unless (fboundp 'gnosis-study-day)
      (user-error "Study day view is not available"))
    (gnosis-study-day (gnosis-journal--iso-to-int date))))

;;; Sync

(defun gnosis-journal-db-sync (&optional force)
  "Sync journal entries in database.
Normally index changed files from disk, leaving visiting buffers untouched.
When FORCE, rebuild all files using visiting contents when available.
Only rebuild indexes; do not complete journal tasks."
  (gnosis-journal--file)
  (let* ((all-files (gnosis-journal--source-files))
         (files (if force
                    all-files
                  (cl-remove-if-not
                   (lambda (file)
                     (gnosis-nodes--file-changed-p file 'journal))
                   all-files))))
    (when (> (length files) 0)
      (let ((progress (make-progress-reporter
                       (format "Processing %d/%d journal files..."
                               (length files) (length all-files))
                       0 (length files))))
        (cl-loop for file in files
                 for i from 0
                 do (progn
                      ;; Preserve live-buffer recovery on explicit rebuilds.
                      (if force
                          (gnosis-nodes-update-file file t)
                        (gnosis-nodes--update-file file t))
                      (progress-reporter-update progress i)))
        (progress-reporter-done progress)))))

(provide 'gnosis-journal)
;;; gnosis-journal.el ends here
