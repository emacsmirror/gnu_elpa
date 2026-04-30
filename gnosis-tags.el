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
    (mapcar #'car
	    (gnosis-sqlite-select-batch (gnosis--ensure-db)
					"SELECT DISTINCT tag FROM thema_tag WHERE thema_id IN (%s)"
					ids))))

(defun gnosis-collect-tag-thema-ids (tags &optional ids)
  "Collect thema IDS for TAGS."
  (cl-assert (listp tags))
  (if (null tags) ids
    (gnosis-collect-tag-thema-ids (cdr tags)
                                  (append ids (gnosis-get-tag-themata (car tags))))))

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

(defun gnosis-tags-prompt ()
  "Tag prompt for adding themata."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "This function is meant to be used in an org-mode buffer"))
  (save-excursion
    (let ((input (delete-dups
		  (completing-read-multiple
		   "Tags (separated by ,): " (gnosis-get-tags--unique))))
	  (current-tags (org-get-tags)))
      (outline-up-heading 99)
      (when input
	(setf gnosis-previous-thema-tags input)
        (org-set-tags (append input current-tags))))))

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
      ;; Remove rows where the thema already has new-tag (avoid UNIQUE conflict)
      (gnosis-sqlite-execute db
			     "DELETE FROM thema_tag WHERE tag = ? AND thema_id IN
         (SELECT thema_id FROM thema_tag WHERE tag = ?)"
			     (list tag new-tag))
      ;; Rename remaining rows
      (gnosis-sqlite-execute db
			     "UPDATE thema_tag SET tag = ? WHERE tag = ?" (list new-tag tag)))
    (message "Renamed tag '%s' to '%s'" tag new-tag)))

(defun gnosis--tag-rename-batch (pairs)
  "Rename tags per PAIRS, an alist of (OLD . NEW).
Pairs where NEW is empty delete those tag rows instead of renaming.
Uses a temp mapping table so the entire rename is O(1) SQL statements."
  (let* ((delete-tags (mapcar #'car (cl-remove-if-not
				     (lambda (p) (string-empty-p (cdr p)))
				     pairs)))
	 (rename-pairs (cl-remove-if (lambda (p) (string-empty-p (cdr p)))
				     pairs))
	 (db (gnosis--ensure-db))
	 (max-vars (gnosis-sqlite--max-variable-number db)))
    (gnosis-sqlite-with-transaction db
      ;; Delete tags that rename to empty string
      (when delete-tags
	(gnosis-sqlite-execute-batch db
				     "DELETE FROM thema_tag WHERE tag IN (%s)" delete-tags))
      (when rename-pairs
	(gnosis-sqlite-execute db
			       "CREATE TEMP TABLE _tag_map (old_tag TEXT, new_tag TEXT)")
	;; Bulk-insert pairs, chunked to stay within variable limits
	(let ((offset 0)
	      (total (length rename-pairs)))
	  (while (< offset total)
	    (let* ((batch-size (/ max-vars 2))
		   (end (min (+ offset batch-size) total))
		   (chunk (cl-subseq rename-pairs offset end))
		   (placeholders (mapconcat (lambda (_) "(?, ?)") chunk ", "))
		   (params (cl-loop for (old . new) in chunk
				    append (list old new))))
	      (gnosis-sqlite-execute db
				     (format "INSERT INTO _tag_map VALUES %s" placeholders)
				     params)
	      (setq offset end))))
	;; 1) Delete rows where thema already has the target tag
	(gnosis-sqlite-execute db
			       "DELETE FROM thema_tag WHERE rowid IN (
           SELECT tt.rowid FROM thema_tag tt
           JOIN _tag_map m ON tt.tag = m.old_tag
           WHERE EXISTS (SELECT 1 FROM thema_tag t2
                         WHERE t2.thema_id = tt.thema_id
                           AND t2.tag = m.new_tag))")
	;; 2) Delete intra-rename duplicates: multiple old tags -> same new tag
	;;    for the same thema.  Keep the row with the smallest rowid.
	(gnosis-sqlite-execute db
			       "DELETE FROM thema_tag WHERE rowid IN (
           SELECT tt.rowid FROM thema_tag tt
           JOIN _tag_map m ON tt.tag = m.old_tag
           WHERE EXISTS (SELECT 1 FROM thema_tag tt2
                         JOIN _tag_map m2 ON tt2.tag = m2.old_tag
                         WHERE tt2.thema_id = tt.thema_id
                           AND m2.new_tag = m.new_tag
                           AND tt2.rowid < tt.rowid))")
	;; 3) Rename remaining rows
	(gnosis-sqlite-execute db
			       "UPDATE thema_tag SET tag = (
           SELECT m.new_tag FROM _tag_map m WHERE m.old_tag = thema_tag.tag)
         WHERE tag IN (SELECT old_tag FROM _tag_map)")
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
               (encoded-tag (prin1-to-string tag))
               (offset 0)
               (total (length ids)))
          (while (< offset total)
            (let* ((end (min (+ offset batch-size) total))
                   (chunk (cl-subseq ids offset end))
                   (placeholders (mapconcat (lambda (_) "(?, ?)") chunk ", "))
                   (params (cl-loop for id in chunk
                                    append (list id encoded-tag))))
              (sqlite-execute db
			      (format "INSERT OR IGNORE INTO thema_tag (thema_id, tag) VALUES %s"
				      placeholders)
			      params)
              (setq offset end))))))))

(provide 'gnosis-tags)
;;; gnosis-tags.el ends here
