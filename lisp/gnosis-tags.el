;;; gnosis-tags.el --- Tag management for gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions

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

;; Tag queries, filtering, renaming, and mutations for gnosis themata.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'gnosis-db)
(require 'gnosis-sqlite)

(defvar gnosis-previous-thema-tags '()
  "Tags input from previously added thema.")

(defun gnosis-get-tags--unique ()
  "Return a list of unique strings for tags in `gnosis-db'."
  (mapcar #'car
	  (gnosis-sqlite-select (gnosis--ensure-db)
				"SELECT DISTINCT tag FROM thema_tag")))

(defun gnosis-get-tags-for-ids (ids)
  "Return unique tags for thema IDS."
  (when ids
    (delete-dups
     (mapcar #'car
             (gnosis-sqlite-select-batch
              (gnosis--ensure-db)
              "SELECT DISTINCT tag FROM thema_tag WHERE thema_id IN (%s)"
              ids)))))

(defun gnosis-collect-tag-thema-ids (tags &optional ids)
  "Collect thema IDS for TAGS."
  (cl-assert (listp tags))
  (if (null tags) ids
    (append ids (cl-loop for tag in tags append (gnosis-get-tag-themata tag)))))

(defun gnosis-get-tag-themata (tag)
  "Return thema ids for TAG."
  (gnosis-select 'thema-id 'thema-tag `(= tag ,tag) t))

(defun gnosis-filter-by-tags (include-tags exclude-tags)
  "Return thema IDs matching INCLUDE-TAGS but not EXCLUDE-TAGS.
When INCLUDE-TAGS is nil, start from all thema IDs."
  (let ((ids (if include-tags
		 (cl-remove-duplicates (gnosis-collect-tag-thema-ids include-tags))
	       (gnosis-select 'id 'themata nil t))))
    (if exclude-tags
	(let ((excluded (cl-remove-duplicates
			 (gnosis-collect-tag-thema-ids exclude-tags))))
	  (cl-set-difference ids excluded))
      ids)))

(defun gnosis-tags--parse-filter (input)
  "Parse INPUT list of \"+tag\" / \"-tag\" strings.
Return (INCLUDE . EXCLUDE) cons of plain tag lists."
  (let (include exclude)
    (dolist (entry input)
      (cond ((string-prefix-p "+" entry)
	     (push (substring entry 1) include))
	    ((string-prefix-p "-" entry)
	     (push (substring entry 1) exclude))))
    (cons (nreverse include) (nreverse exclude))))

(defun gnosis-tags-filter-prompt (&optional tags)
  "Prompt for tag filters using +include / -exclude notation.
TAGS is an optional list of tag strings; defaults to all unique tags.
Return (INCLUDE . EXCLUDE) cons of plain tag lists."
  (interactive)
  (let* ((tags (or tags (gnosis-get-tags--unique)))
	 (candidates (cl-loop for tag in tags
			      nconc (list (concat "+" tag)
					  (concat "-" tag))))
	 (input (completing-read-multiple
		 "Filter tags (+include -exclude): "
		 candidates nil nil)))
    (gnosis-tags--parse-filter input)))

(defun gnosis-tags--check-org (tags)
  "Refuse TAGS that native Org headline syntax cannot preserve exactly.
This is an authoring boundary, not a restriction on stored tag identities."
  (dolist (tag tags)
    (unless (and (stringp tag)
                 (string-match-p (concat "\\`" org-tag-re "\\'") tag))
      (user-error
       "Org cannot preserve tag %S; explicitly rename or remove it with the tag commands first"
       tag))))

(defun gnosis-tags-prompt ()
  "Tag prompt for adding themata."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "This function is meant to be used in an org-mode buffer"))
  (save-excursion
    (let ((input (delete-dups
		  (completing-read-multiple
		   "Tags (separated by ,): " (gnosis-get-tags--unique)))))
      (org-back-to-heading t)
      (while (org-up-heading-safe))
      (when input
        (gnosis-tags--check-org input)
	(setf gnosis-previous-thema-tags input)
        (org-set-tags (append input (org-get-tags)))))))

(defun gnosis-tag-rename (tag &optional new-tag)
  "Rename TAG to NEW-TAG, merging if NEW-TAG already exists.

Replace dashes (-) to underscores (_) for NEW-TAG, as org currently
does not accept heading tags with dashes.
When a thema already has NEW-TAG, the duplicate OLD row is removed."
  (let ((new-tag (or new-tag
		     (replace-regexp-in-string
		      "-" "_" (read-string "New tag name: "))))
	(db (gnosis--ensure-db)))
    (when (string-empty-p new-tag)
      (user-error "Tag name cannot be empty"))
    (when (string= tag new-tag)
      (user-error "New tag name is the same as the old one"))
    (gnosis-sqlite-with-transaction db
      ;; Remove rows where the thema already has new-tag
      ;; (avoid UNIQUE conflict)
      (gnosis-sqlite-execute db
			     "DELETE FROM thema_tag WHERE tag = ? AND thema_id IN
         (SELECT thema_id FROM thema_tag WHERE tag = ?)"
			     (list tag new-tag))
      ;; Rename remaining rows
      (gnosis-sqlite-execute db
			     "UPDATE thema_tag SET tag = ? WHERE tag = ?" (list new-tag tag)))
    (message "Renamed tag '%s' to '%s'" tag new-tag)))

(defun gnosis--tag-rename-batch (pairs)
  "Rename tags simultaneously per PAIRS, an alist of (OLD . NEW).
An empty NEW deletes the original tag.  Deduplicate only the final
set, including collisions with unchanged tags.  Batch mapping rows
to respect SQLite parameter limits; replace affected rows atomically."
  (when pairs
    (let* ((db (gnosis--ensure-db))
           (batch-size (/ (gnosis-sqlite--max-variable-number db) 2)))
      (gnosis-sqlite-with-transaction db
        (gnosis-sqlite-execute
         db "CREATE TEMP TABLE _tag_map (old_tag TEXT PRIMARY KEY, new_tag TEXT)")
        (dolist (chunk (seq-partition pairs batch-size))
          (gnosis-sqlite-execute
           db (concat "INSERT INTO _tag_map VALUES "
                      (mapconcat (lambda (_) "(?, ?)") chunk ", "))
           (cl-loop for (old . new) in chunk append (list old new))))
        ;; Snapshot the mapped set before any source tag is removed.
        (gnosis-sqlite-execute
         db "CREATE TEMP TABLE _tag_final AS
             SELECT DISTINCT tt.thema_id, m.new_tag AS tag
             FROM thema_tag tt JOIN _tag_map m ON tt.tag = m.old_tag
             WHERE m.new_tag != ?" (list ""))
        (gnosis-sqlite-execute
         db "DELETE FROM thema_tag WHERE tag IN (SELECT old_tag FROM _tag_map)")
        (gnosis-sqlite-execute
         db "INSERT OR IGNORE INTO thema_tag (thema_id, tag)
             SELECT thema_id, tag FROM _tag_final")
        (gnosis-sqlite-execute db "DROP TABLE _tag_final")
        (gnosis-sqlite-execute db "DROP TABLE _tag_map")))))

(defun gnosis-modify-thema-tags (ids add-tags remove-tags)
  "Modify tags for thema IDS.  Add ADD-TAGS and remove REMOVE-TAGS.
Batched to stay within SQL variable limits."
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-with-transaction db
      (dolist (tag remove-tags)
        (gnosis-sqlite-execute-batch db
				     "DELETE FROM thema_tag WHERE tag = ? AND thema_id IN (%s)"
				     ids
				     (list tag)))
      (dolist (tag add-tags)
        (let* ((max-vars (gnosis-sqlite--max-variable-number db))
               (batch-size (/ max-vars 2))
               (encoded-tag (gnosis-sqlite--serialize tag)))
          (dolist (chunk (seq-partition ids batch-size))
            (let* ((placeholders (mapconcat (lambda (_) "(?, ?)") chunk ", "))
                   (params (cl-loop for id in chunk
                                    append (list id encoded-tag))))
              (sqlite-execute db
			      (format "INSERT OR IGNORE INTO thema_tag (thema_id, tag) VALUES %s"
				      placeholders)
			      params))))))))

(provide 'gnosis-tags)
;;; gnosis-tags.el ends here
