;;; gnosis-export-import.el --- Export/import for gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis

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

;; Export and import operations for gnosis themata.
;;
;; This module handles:
;; - Exporting themata to SQLite database files (`gnosis-export-db')
;; - Importing themata from SQLite databases with diff review (`gnosis-import-db')
;; - Editing support: parsing org buffers and saving themata (`gnosis-save')

;;; Code:

(require 'gnosis)
(require 'gnosis-algorithm)
(require 'transient)
(require 'org)
(require 'org-element)

(declare-function gnosis--ensure-db "gnosis")
(declare-function gnosis--insert-into "gnosis")
(declare-function gnosis--thema-expected-links "gnosis")
(declare-function gnosis-select "gnosis")
(declare-function gnosis-filter-by-tags "gnosis")
(declare-function gnosis-tags-filter-prompt "gnosis")
(declare-function gnosis-extract-id-links "gnosis")
(declare-function gnosis-edit-quit "gnosis")
(declare-function gnosis--date-to-int "gnosis")
(declare-function gnosis-sqlite-execute "gnosis-sqlite")
(declare-function gnosis-sqlite-select "gnosis-sqlite")
(declare-function gnosis-sqlite-execute-batch "gnosis-sqlite")
(declare-function gnosis-sqlite--decode "gnosis-sqlite")
(declare-function gnosis-algorithm-date "gnosis-algorithm")
(declare-function gnosis--git-cmd "gnosis")
(declare-function gnosis-vc-push "gnosis")

(defvar gnosis-db)
(defvar gnosis-dir)
(defvar gnosis-testing)
(defvar gnosis-vc-auto-push)
(defvar gnosis-export-separator)
(defvar gnosis-thema-types)
(defvar gnosis-save-hook)
(defvar gnosis--id-cache)
(defvar gnosis-edit-mode)

;;; ---- Edit mode support ----

(defun gnosis-export--insert-read-only (string)
  "Insert STRING as read-only."
  (let ((start (point)))
    (insert string)
    (add-text-properties start (point) '(read-only t))
    (let ((inhibit-read-only t))
      (insert " "))))

(cl-defun gnosis-export--insert-thema (id type &optional keimenon hypothesis
					  answer parathema tags example)
  "Insert thema for thema ID.

TYPE: Thema type, refer to `gnosis-thema-types'
KEIMENON: Text user is first presented with.
HYPOTHESIS: Hypothesis for what the ANSWER is
ANSWER: The revelation after KEIMENON
PARATHEMA: The text where THEMA is derived from.
TAGS: List of THEMA tags
EXAMPLE: Boolean value, if non-nil do not add properties for thema."
  (let ((components `(("** Keimenon" . ,keimenon)
                      ("** Hypothesis" . ,hypothesis)
                      ("** Answer" . ,answer)
                      ("** Parathema" . ,parathema))))
    (goto-char (point-max))
    (insert "\n* Thema")
    (when tags
      (insert " :" (mapconcat #'identity tags ":") ":"))
    (insert "\n")
    (unless example
      (let ((start (point)))
        (insert ":PROPERTIES:\n:GNOSIS_ID: " id "\n")
        (add-text-properties start (point)
			     '(read-only t rear-nonsticky (read-only))))
      (insert ":GNOSIS_TYPE: " type "\n")
      (let ((start (point)))
        (insert ":END:\n")
        (add-text-properties start (point)
			     '(read-only t rear-nonsticky (read-only)))))
    (dolist (comp components)
      (goto-char (point-max))
      (gnosis-export--insert-read-only (car comp))
      (insert "\n" (or (cdr comp) "") "\n\n"))))

(defun gnosis-export-parse-themata (&optional separator)
  "Extract content for each level-2 heading for thema headings with a GNOSIS_ID.

Split content of Hypothesis and Answer headings using SEPARATOR."
  (let ((sep (or separator gnosis-export-separator))
        results)
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (let* ((level (org-element-property :level headline))
               (gnosis-id (org-element-property :GNOSIS_ID headline))
               (gnosis-type (org-element-property :GNOSIS_TYPE headline))
               (tags (org-element-property :tags headline)))
          (when (and (= level 1) gnosis-id gnosis-type)
            (let ((line (line-number-at-pos
                         (org-element-property :begin headline)))
                  entry)
              (push gnosis-id entry)
              (push gnosis-type entry)
              (dolist (child (org-element-contents headline))
                (when (eq 'headline (org-element-type child))
                  (let* ((child-title (org-element-property :raw-value child))
                         (child-text (substring-no-properties
                                      (string-trim
                                       (org-element-interpret-data
					(org-element-contents child)))))
                         (processed-text
                          (cond
                           ((and (member child-title '("Hypothesis" "Answer"))
                                 (not (string-empty-p child-text)))
                            (mapcar (lambda (s)
                                      (string-trim
                                       (string-remove-prefix "-"
							     (string-remove-prefix sep s))))
                                    (split-string child-text sep t "[ \t\n]+")))
                           ((string-empty-p child-text) nil)
                           (t child-text))))
                    (push processed-text entry))))
              (push tags entry)
              (push line entry)
              (push (nreverse entry) results)))))
      nil nil)
    results))

(defun gnosis-export--insert-themata (ids &optional new-p)
  "Export themata for IDS.

If NEW-P replace the ids of themata with NEW, used for new themata to
generate new thema id."
  (cl-assert (listp ids) nil "IDS value must be a list.")
  (let ((id-values (mapcar (lambda (id)
                             (if (listp id) (car id) id))
                           ids)))
    (dolist (id id-values)
      (let ((thema-data (append (gnosis-select '[type keimenon hypothesis answer]
                                               'themata `(= id ,id) t)
				(gnosis-select 'parathema 'extras `(= id ,id) t)))
            (tags (gnosis-select 'tag 'thema-tag `(= thema-id ,id) t)))
        (gnosis-export--insert-thema
         (if new-p "NEW" (number-to-string id))
         (nth 0 thema-data)
         (nth 1 thema-data)
         (concat (string-remove-prefix "\n" gnosis-export-separator)
                 (mapconcat 'identity (nth 2 thema-data) gnosis-export-separator))
         (concat (string-remove-prefix "\n" gnosis-export-separator)
                 (mapconcat 'identity (nth 3 thema-data) gnosis-export-separator))
         (nth 4 thema-data)
         tags)))))

(defun gnosis-save-thema (thema)
  "Save THEMA.
Returns nil on success, or an error message string on failure."
  (let* ((id (nth 0 thema))
	 (type (nth 1 thema))
	 (keimenon (nth 2 thema))
	 (hypothesis (nth 3 thema))
	 (answer (nth 4 thema))
	 (parathema (or (nth 5 thema) ""))
	 (tags (nth 6 thema))
	 (line (nth 7 thema))
	 (links (append (gnosis-extract-id-links parathema)
			(gnosis-extract-id-links keimenon)))
	 (thema-func (cdr (assoc (downcase type)
				  (mapcar (lambda (pair) (cons (downcase (car pair))
							  (cdr pair)))
					  gnosis-thema-types)))))
    (cl-assert (and type (stringp type) thema-func) nil
	       "GNOSIS_TYPE must be one of: %s (got %S)"
	       (mapconcat #'car gnosis-thema-types ", ") type)
    (condition-case err
        (progn
          (funcall thema-func id type keimenon hypothesis
	           answer parathema tags 0 links)
          nil)
      (error (format "Line %s (id:%s): %s" (or line "?") id
                     (error-message-string err))))))

(defun gnosis-save ()
  "Save themata in current buffer."
  (interactive nil gnosis-edit-mode)
  (let* ((gc-cons-threshold most-positive-fixnum)
         (themata (gnosis-export-parse-themata))
	 (gnosis--id-cache (let ((ht (make-hash-table :test 'equal)))
			     (dolist (id (gnosis-select 'id 'themata nil t) ht)
			       (puthash id t ht))))
	 (errors nil)
	 (edited-id (string-to-number (caar themata))))
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (cl-loop for thema in themata
	       for err = (gnosis-save-thema thema)
	       when err do (push err errors)))
    (if errors
        (user-error "Failed to import %d thema(ta):\n%s"
                    (length errors) (mapconcat #'identity (nreverse errors) "\n"))
      (gnosis-edit-quit)
      (run-hook-with-args 'gnosis-save-hook edited-id))))

;;; ---- SQLite export ----

(defun gnosis-import--sanitize-path (path)
  "Escape single quotes in PATH for use in SQL ATTACH statement."
  (replace-regexp-in-string "'" "''" (expand-file-name path)))

(defconst gnosis-export--themata-schema
  "CREATE TABLE export_db.themata (
  id INTEGER PRIMARY KEY,
  type TEXT NOT NULL,
  keimenon TEXT NOT NULL,
  hypothesis TEXT NOT NULL,
  answer TEXT NOT NULL)"
  "SQL schema for the themata table in export databases.")

(defconst gnosis-export--thema-tag-schema
  "CREATE TABLE export_db.thema_tag (
  thema_id INTEGER NOT NULL,
  tag TEXT NOT NULL,
  FOREIGN KEY (thema_id) REFERENCES themata(id) ON DELETE CASCADE,
  UNIQUE (thema_id, tag))"
  "SQL schema for the thema_tag table in export databases.")

(defconst gnosis-export--extras-schema
  "CREATE TABLE export_db.extras (
  id INTEGER PRIMARY KEY,
  parathema TEXT,
  review_image TEXT,
  FOREIGN KEY (id) REFERENCES themata(id) ON DELETE CASCADE)"
  "SQL schema for the extras table in export databases.")

(defconst gnosis-export--meta-schema
  "CREATE TABLE export_db.gnosis_meta (
  key TEXT PRIMARY KEY,
  value TEXT)"
  "SQL schema for the gnosis_meta table in export databases.")

;;;###autoload
(defun gnosis-export-db (file &optional include-tags exclude-tags include-suspended)
  "Export filtered themata to FILE as a SQLite database.

When called interactively, prompts for tag filters.
INCLUDE-TAGS and EXCLUDE-TAGS filter themata.
When INCLUDE-SUSPENDED, also export suspended themata."
  (interactive
   (let ((filter (gnosis-tags-filter-prompt)))
     (list (read-file-name "Export database: " nil nil nil "gnosis-export.gnosis")
           (car filter) (cdr filter)
           (y-or-n-p "Include suspended themata? "))))
  (let* ((db (gnosis--ensure-db))
	 (file (expand-file-name file))
	 (sanitized (gnosis-import--sanitize-path file))
	 ;; Compute IDs to export
	 (suspended-ids
	  (unless include-suspended
	    (mapcar #'car
		    (gnosis-sqlite-select db
					  "SELECT id FROM review_log WHERE suspend = 1"))))
	 (excluded-ht (when suspended-ids
			(let ((ht (make-hash-table :test 'equal
						   :size (length suspended-ids))))
			  (dolist (id suspended-ids ht)
			    (puthash id t ht)))))
	 (ids (cond
	       ((and (or include-tags exclude-tags) excluded-ht)
		(cl-remove-if (lambda (id) (gethash id excluded-ht))
			      (gnosis-filter-by-tags include-tags exclude-tags)))
	       ((or include-tags exclude-tags)
		(gnosis-filter-by-tags include-tags exclude-tags))
	       (excluded-ht
		(cl-remove-if (lambda (id) (gethash id excluded-ht))
			      (mapcar #'car
				      (gnosis-sqlite-select db "SELECT id FROM themata"))))
	       (t nil)))
	 (count (if ids (length ids)
		  (caar (gnosis-sqlite-select db "SELECT COUNT(*) FROM themata")))))
    (when (called-interactively-p 'any)
      (unless (y-or-n-p (format "Export %d themata to %s? " count file))
	(user-error "Export cancelled")))
    (when (file-exists-p file)
      (delete-file file))
    (gnosis-sqlite-execute db (format "ATTACH DATABASE '%s' AS export_db" sanitized))
    (unwind-protect
	(progn
	  (gnosis-sqlite-execute db gnosis-export--themata-schema)
	  (gnosis-sqlite-execute db gnosis-export--thema-tag-schema)
	  (gnosis-sqlite-execute db gnosis-export--extras-schema)
	  (gnosis-sqlite-execute db gnosis-export--meta-schema)
	  (if ids
	      (progn
		(gnosis-sqlite-execute-batch db
					     "INSERT INTO export_db.themata SELECT id, type, keimenon, hypothesis, answer FROM themata WHERE id IN (%s)"
					     ids)
		(gnosis-sqlite-execute-batch db
					     "INSERT INTO export_db.extras SELECT id, parathema, review_image FROM extras WHERE id IN (%s)"
					     ids)
		(gnosis-sqlite-execute-batch db
					     "INSERT INTO export_db.thema_tag SELECT thema_id, tag FROM thema_tag WHERE thema_id IN (%s)"
					     ids))
	    (gnosis-sqlite-execute db
				   "INSERT INTO export_db.themata SELECT id, type, keimenon, hypothesis, answer FROM themata")
	    (gnosis-sqlite-execute db
				   "INSERT INTO export_db.extras SELECT id, parathema, review_image FROM extras")
	    (gnosis-sqlite-execute db
				   "INSERT INTO export_db.thema_tag SELECT thema_id, tag FROM thema_tag"))
	  ;; Metadata uses raw sqlite-execute to store plain text
	  ;; (gnosis-sqlite-execute would emacsql-encode the values)
	  (let ((now (format-time-string "%Y-%m-%dT%H:%M:%S")))
	    (sqlite-execute db
			    "INSERT INTO export_db.gnosis_meta (key, value) VALUES (?, ?)"
			    (list "exported_at" now))
	    (sqlite-execute db
			    "INSERT INTO export_db.gnosis_meta (key, value) VALUES (?, ?)"
			    (list "thema_count" (number-to-string count)))))
      (gnosis-sqlite-execute db "DETACH DATABASE export_db"))
    (message "Exported %d themata to %s" count file)))

;;; ---- SQLite import ----

(defun gnosis-import--commit (new-count changed-count filename)
  "Commit database after importing NEW-COUNT new and CHANGED-COUNT changed from FILENAME."
  (let ((default-directory gnosis-dir))
    (unless gnosis-testing
      (when (file-exists-p (expand-file-name ".git" gnosis-dir))
	(call-process (executable-find "git") nil nil nil "add" "gnosis.db")
	(gnosis--git-cmd
	 (list "commit" "-m"
	       (format "Import: %d new, %d updated from %s"
		       new-count changed-count filename)))))
    (when (and gnosis-vc-auto-push (not gnosis-testing))
      (gnosis-vc-push))))

(defface gnosis-import-new-face
  '((t :inherit success))
  "Face for NEW entries in the import diff buffer."
  :group 'gnosis)

(defface gnosis-import-changed-face
  '((t :inherit warning))
  "Face for CHANGED entries in the import diff buffer."
  :group 'gnosis)

(defvar-local gnosis-import--file nil
  "Path to the import database file.")

(defvar-local gnosis-import--new-ids nil
  "List of new thema IDs from the import.")

(defvar-local gnosis-import--changed-ids nil
  "List of changed thema IDs from the import.")

(defun gnosis-import--changed-fields (row)
  "Return comma-separated string of changed field names from ROW.
ROW has flags at positions 3-7: type, keimenon, hypothesis, answer, parathema."
  (let ((names '((3 . "type") (4 . "keimenon") (5 . "hypothesis")
		 (6 . "answer") (7 . "parathema") (8 . "tags"))))
    (mapconcat #'cdr
	       (cl-remove-if-not (lambda (pair) (= 1 (or (nth (car pair) row) 0)))
				 names)
	       ", ")))

(defun gnosis-import--diff (file)
  "Compute diff between import FILE and main gnosis database.
Returns (NEW-ROWS CHANGED-ROWS).
NEW-ROWS: each is (ID TYPE KEIMENON).
CHANGED-ROWS: each is (ID TYPE KEIMENON CHANGED-FIELDS-STRING)."
  (let* ((db (gnosis--ensure-db))
	 (sanitized (gnosis-import--sanitize-path file))
	 new-rows changed-rows)
    (gnosis-sqlite-execute db (format "ATTACH DATABASE '%s' AS import_db" sanitized))
    (unwind-protect
	(progn
	  ;; New themata: in import but not in main
	  (setq new-rows
		(mapcar (lambda (row)
			  (list (car row)
				(gnosis-sqlite--decode (nth 1 row))
				(gnosis-sqlite--decode (nth 2 row))))
			(sqlite-select db
				       "SELECT i.id, i.type, i.keimenon FROM import_db.themata i \
WHERE i.id NOT IN (SELECT id FROM themata)")))
	  ;; Changed themata: single query checking all dimensions
	  ;; Flags: 3=type 4=keimenon 5=hypothesis 6=answer 7=parathema 8=tags
	  (let ((raw-changed
		 (sqlite-select db
				"SELECT i.id, i.type, i.keimenon, \
CASE WHEN i.type != t.type THEN 1 ELSE 0 END, \
CASE WHEN i.keimenon != t.keimenon THEN 1 ELSE 0 END, \
CASE WHEN i.hypothesis != t.hypothesis THEN 1 ELSE 0 END, \
CASE WHEN i.answer != t.answer THEN 1 ELSE 0 END, \
CASE WHEN COALESCE(ie.parathema, '') != COALESCE(e.parathema, '') THEN 1 ELSE 0 END, \
CASE WHEN COALESCE((SELECT GROUP_CONCAT(tag) FROM \
  (SELECT tag FROM import_db.thema_tag WHERE thema_id = i.id ORDER BY tag)), '') \
!= COALESCE((SELECT GROUP_CONCAT(tag) FROM \
  (SELECT tag FROM thema_tag WHERE thema_id = i.id ORDER BY tag)), '') THEN 1 ELSE 0 END \
FROM import_db.themata i \
INNER JOIN themata t ON i.id = t.id \
LEFT JOIN import_db.extras ie ON i.id = ie.id \
LEFT JOIN extras e ON i.id = e.id \
WHERE i.type != t.type OR i.keimenon != t.keimenon \
OR i.hypothesis != t.hypothesis OR i.answer != t.answer \
OR COALESCE(ie.parathema, '') != COALESCE(e.parathema, '') \
OR COALESCE((SELECT GROUP_CONCAT(tag) FROM \
  (SELECT tag FROM import_db.thema_tag WHERE thema_id = i.id ORDER BY tag)), '') \
!= COALESCE((SELECT GROUP_CONCAT(tag) FROM \
  (SELECT tag FROM thema_tag WHERE thema_id = i.id ORDER BY tag)), '')")))
	    (setq changed-rows
		  (mapcar (lambda (row)
			    (list (car row)
				  (gnosis-sqlite--decode (nth 1 row))
				  (gnosis-sqlite--decode (nth 2 row))
				  (gnosis-import--changed-fields row)))
			  raw-changed))))
      (gnosis-sqlite-execute db "DETACH DATABASE import_db"))
    (list new-rows changed-rows)))

(defun gnosis-import--render-diff (new-rows changed-rows)
  "Render diff entries in current tabulated-list buffer.
NEW-ROWS: (ID TYPE KEIMENON).  CHANGED-ROWS: (ID TYPE KEIMENON FIELDS)."
  (setq tabulated-list-entries
	(append
	 (mapcar (lambda (row)
		   (let ((id (car row))
			 (type (nth 1 row))
			 (keimenon (truncate-string-to-width
				    (or (nth 2 row) "") 40 nil nil t)))
		     (list id (vector
			       (propertize "NEW" 'face 'gnosis-import-new-face)
			       (number-to-string id)
			       (or type "")
			       keimenon
			       ""))))
		 new-rows)
	 (mapcar (lambda (row)
		   (let ((id (car row))
			 (type (nth 1 row))
			 (keimenon (truncate-string-to-width
				    (or (nth 2 row) "") 40 nil nil t))
			 (fields (or (nth 3 row) "")))
		     (list id (vector
			       (propertize "CHANGED" 'face 'gnosis-import-changed-face)
			       (number-to-string id)
			       (or type "")
			       keimenon
			       fields))))
		 changed-rows)))
  (tabulated-list-print t))

(defun gnosis-import--apply-changes (file new-ids changed-ids)
  "Apply import from FILE: insert NEW-IDS, update CHANGED-IDS."
  (let* ((db (gnosis--ensure-db))
	 (sanitized (gnosis-import--sanitize-path file))
	 (today (gnosis--date-to-int (gnosis-algorithm-date))))
    (gnosis-sqlite-execute db (format "ATTACH DATABASE '%s' AS import_db" sanitized))
    (unwind-protect
	(gnosis-sqlite-with-transaction db
	  ;; Insert new themata
	  (when new-ids
	    (gnosis-sqlite-execute-batch db
					 "INSERT INTO themata (id, type, keimenon, hypothesis, answer) \
SELECT id, type, keimenon, hypothesis, answer FROM import_db.themata WHERE id IN (%s)"
					 new-ids)
	    (gnosis-sqlite-execute-batch db
					 "INSERT OR IGNORE INTO extras (id, parathema, review_image) \
SELECT id, parathema, review_image FROM import_db.extras WHERE id IN (%s)"
					 new-ids)
	    (gnosis-sqlite-execute-batch db
					 "INSERT OR IGNORE INTO thema_tag (thema_id, tag) \
SELECT thema_id, tag FROM import_db.thema_tag WHERE thema_id IN (%s)"
					 new-ids)
	    ;; Initialize review state for new themata
	    (let ((gnosis-val (prin1-to-string gnosis-algorithm-gnosis-value))
		  (amnesia-val (prin1-to-string gnosis-algorithm-amnesia-value)))
	      (gnosis-sqlite-execute-batch db
					   (format "INSERT OR IGNORE INTO review (id, gnosis, amnesia) \
SELECT id, '%s', '%s' FROM import_db.themata WHERE id IN (%%s)"
						   gnosis-val amnesia-val)
					   new-ids))
	    (gnosis-sqlite-execute-batch db
					 (format "INSERT OR IGNORE INTO review_log \
(id, last_rev, next_rev, c_success, t_success, c_fails, t_fails, suspend, n) \
SELECT id, 0, %d, 0, 0, 0, 0, 0, 0 FROM import_db.themata WHERE id IN (%%s)"
						 today)
					 new-ids)
	    ;; Sync thema-links for new themata
	    (dolist (id new-ids)
	      (let* ((keimenon (caar (gnosis-select 'keimenon 'themata `(= id ,id))))
		     (parathema (caar (gnosis-select 'parathema 'extras `(= id ,id))))
		     (links (gnosis--thema-expected-links
			     (or keimenon "") (or parathema ""))))
		(dolist (link links)
		  (gnosis--insert-into 'thema-links `([,id ,link]))))))
	  ;; Update changed themata
	  (when changed-ids
	    ;; Update themata content
	    (gnosis-sqlite-execute-batch db
					 "UPDATE themata SET \
type = (SELECT type FROM import_db.themata WHERE import_db.themata.id = themata.id), \
keimenon = (SELECT keimenon FROM import_db.themata WHERE import_db.themata.id = themata.id), \
hypothesis = (SELECT hypothesis FROM import_db.themata WHERE import_db.themata.id = themata.id), \
answer = (SELECT answer FROM import_db.themata WHERE import_db.themata.id = themata.id) \
WHERE id IN (%s)"
					 changed-ids)
	    ;; Update extras
	    (gnosis-sqlite-execute-batch db
					 "UPDATE extras SET \
parathema = (SELECT parathema FROM import_db.extras WHERE import_db.extras.id = extras.id) \
WHERE id IN (%s)"
					 changed-ids)
	    ;; Replace tags: delete old, insert new
	    (gnosis-sqlite-execute-batch db
					 "DELETE FROM thema_tag WHERE thema_id IN (%s)"
					 changed-ids)
	    (gnosis-sqlite-execute-batch db
					 "INSERT OR IGNORE INTO thema_tag (thema_id, tag) \
SELECT thema_id, tag FROM import_db.thema_tag WHERE thema_id IN (%s)"
					 changed-ids)
	    ;; Re-sync thema-links from updated keimenon/parathema
	    (gnosis-sqlite-execute-batch db
					 "DELETE FROM thema_links WHERE source IN (%s)"
					 changed-ids)
	    (dolist (id changed-ids)
	      (let* ((keimenon (caar (gnosis-select 'keimenon 'themata `(= id ,id))))
		     (parathema (caar (gnosis-select 'parathema 'extras `(= id ,id))))
		     (links (gnosis--thema-expected-links
			     (or keimenon "") (or parathema ""))))
		(dolist (link links)
		  (gnosis--insert-into 'thema-links `([,id ,link])))))))
      (gnosis-sqlite-execute db "DETACH DATABASE import_db"))))

(defun gnosis-import-apply ()
  "Apply all changes from the import diff."
  (interactive nil gnosis-import-diff-mode)
  (let ((new-ids gnosis-import--new-ids)
	(changed-ids gnosis-import--changed-ids)
	(file gnosis-import--file))
    (unless (or new-ids changed-ids)
      (user-error "No changes to apply"))
    (unless (y-or-n-p (format "Apply %d new and %d changed themata? "
			      (length new-ids) (length changed-ids)))
      (user-error "Import cancelled"))
    (gnosis-import--apply-changes file new-ids changed-ids)
    (gnosis-import--commit (length new-ids) (length changed-ids)
			   (file-name-nondirectory file))
    (message "Applied: %d new, %d updated" (length new-ids) (length changed-ids))
    (quit-window t)))

(defun gnosis-import--insert-field (label old new)
  "Insert field LABEL comparing OLD and NEW values.
When OLD is nil, just display NEW (for new themata)."
  (insert (propertize (format "%s:\n" label) 'face 'bold))
  (let ((old-s (format "%s" (or old "")))
	(new-s (format "%s" (or new ""))))
    (cond
     ((null old)
      (insert "  " new-s "\n\n"))
     ((equal old-s new-s)
      (insert "  " old-s "\n\n"))
     (t
      (insert (propertize (concat "  - " old-s "\n")
			  'face 'gnosis-import-changed-face)
	      (propertize (concat "  + " new-s "\n\n")
			  'face 'gnosis-import-new-face))))))

(defun gnosis-import--fetch-detail (db id)
  "Fetch current and import data for thema ID from DB.
Assumes import_db is already attached.  Returns a plist."
  (list
   :import-row (car (gnosis-sqlite-select db
					  "SELECT type, keimenon, hypothesis, answer \
FROM import_db.themata WHERE id = ?" (list id)))
   :import-extra (caar (gnosis-sqlite-select db
					     "SELECT parathema FROM import_db.extras WHERE id = ?"
					     (list id)))
   :import-tags (sort (mapcar #'car (gnosis-sqlite-select db
							  "SELECT tag FROM import_db.thema_tag \
WHERE thema_id = ?" (list id))) #'string<)
   :current-row (car (gnosis-sqlite-select db
					   "SELECT type, keimenon, hypothesis, answer \
FROM themata WHERE id = ?" (list id)))
   :current-extra (caar (gnosis-sqlite-select db
					      "SELECT parathema FROM extras WHERE id = ?"
					      (list id)))
   :current-tags (sort (mapcar #'car (gnosis-sqlite-select db
							   "SELECT tag FROM thema_tag \
WHERE thema_id = ?" (list id))) #'string<)))

(defun gnosis-import--render-detail (id status data)
  "Render detail buffer for thema ID with STATUS using DATA plist."
  (let* ((current-row (or (plist-get data :current-row) '(nil nil nil nil)))
	 (import-row (plist-get data :import-row))
	 (fields '("Type" "Keimenon" "Hypothesis" "Answer"))
	 (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (format "Thema %d  [%s]\n\n" id status) 'face 'bold))
    (cl-loop for field in fields
	     for old in current-row
	     for new in import-row
	     do (gnosis-import--insert-field field old new))
    (gnosis-import--insert-field "Parathema"
				 (plist-get data :current-extra) (plist-get data :import-extra))
    (gnosis-import--insert-field "Tags"
				 (when (plist-get data :current-tags)
				   (mapconcat #'identity (plist-get data :current-tags) ", "))
				 (mapconcat #'identity (plist-get data :import-tags) ", "))
    (goto-char (point-min))
    (special-mode)))

(defun gnosis-import-view-detail ()
  "Show detailed diff for the thema at point."
  (interactive nil gnosis-import-diff-mode)
  (let* ((id (tabulated-list-get-id))
	 (status (substring-no-properties (aref (tabulated-list-get-entry) 0)))
	 (db (gnosis--ensure-db))
	 (sanitized (gnosis-import--sanitize-path gnosis-import--file))
	 (data nil))
    (gnosis-sqlite-execute db (format "ATTACH DATABASE '%s' AS import_db" sanitized))
    (unwind-protect
	(setq data (gnosis-import--fetch-detail db id))
      (gnosis-sqlite-execute db "DETACH DATABASE import_db"))
    (let ((buf (get-buffer-create "*Gnosis Import Detail*")))
      (with-current-buffer buf
	(gnosis-import--render-detail id status data))
      (display-buffer buf))))

(defvar gnosis-import-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gnosis-import-view-detail)
    (define-key map "a" #'gnosis-import-apply)
    (define-key map "q" #'quit-window)
    (define-key map "?" #'gnosis-import-diff-menu)
    (define-key map "h" #'gnosis-import-diff-menu)
    map)
  "Keymap for `gnosis-import-diff-mode'.")

(define-derived-mode gnosis-import-diff-mode tabulated-list-mode "Gnosis Import"
  "Major mode for reviewing gnosis import diffs.

\\{gnosis-import-diff-mode-map}"
  :interactive nil
  (setq tabulated-list-format [("Status" 8 t)
			       ("ID" 12 t)
			       ("Type" 10 t)
			       ("Keimenon" 40 t)
			       ("Changes" 30 t)])
  (tabulated-list-init-header))

(defun gnosis-import--menu-description ()
  "Return description string for import transient menu."
  (let ((new-count (length gnosis-import--new-ids))
	(changed-count (length gnosis-import--changed-ids)))
    (concat "Import: "
	    (propertize (format "%d new" new-count) 'face 'gnosis-import-new-face)
	    ", "
	    (propertize (format "%d changed" changed-count) 'face 'gnosis-import-changed-face))))

(transient-define-prefix gnosis-import-diff-menu ()
  "Transient menu for import diff buffer."
  [:description gnosis-import--menu-description
		("RET" "View detail" gnosis-import-view-detail)
		("a" "Apply changes" gnosis-import-apply)
		("q" "Quit" quit-window)])

;;;###autoload
(defun gnosis-import-db (file)
  "Import themata from SQLite database FILE.
Shows a diff buffer for review before applying."
  (interactive (list (read-file-name "Import database: ")))
  (let ((file (expand-file-name file)))
    (unless (file-exists-p file)
      (user-error "File does not exist: %s" file))
    (let* ((diff (gnosis-import--diff file))
	   (new-rows (car diff))
	   (changed-rows (cadr diff)))
      (if (and (null new-rows) (null changed-rows))
	  (message "No new or changed themata in %s" file)
	(let ((buf (get-buffer-create "*Gnosis Import*")))
	  (with-current-buffer buf
	    (gnosis-import-diff-mode)
	    (setq gnosis-import--file file)
	    (setq gnosis-import--new-ids (mapcar #'car new-rows))
	    (setq gnosis-import--changed-ids (mapcar #'car changed-rows))
	    (gnosis-import--render-diff new-rows changed-rows))
	  (pop-to-buffer buf))))))

(provide 'gnosis-export-import)
;;; gnosis-export-import.el ends here
