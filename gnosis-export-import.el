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
;; - Exporting themata to org-mode files (`gnosis-export-themata-to-file')
;; - Importing themata from org-mode files (`gnosis-import-file')
;; - Parsing exported org buffers (`gnosis-export-parse-themata')
;; - Saving edited themata (`gnosis-save')

;;; Code:

(require 'gnosis)
(require 'gnosis-algorithm)
(require 'org)
(require 'org-element)

(defun gnosis-export--insert-read-only (string)
  "Insert STRING as read-only."
  (let ((start (point)))
    (insert string)
    ;; Set the just inserted string as read-only
    (add-text-properties start (point) '(read-only t))
    ;; Since the space is inserted outside of the read-only region, it's editable
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
        (insert ":PROPERTIES:\n:GNOSIS_ID: " id "\n:GNOSIS_TYPE: " type "\n:END:\n")
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
  ;; Extract just the ID values if they're in a list structure
  (let ((id-values (mapcar (lambda (id)
                             (if (listp id) (car id) id))
                           ids)))
    ;; Process each thema
    (dolist (id id-values)
      (let ((thema-data (append (gnosis-select '[type keimenon hypothesis answer tags]
                                              'themata `(= id ,id) t)
                               (gnosis-select 'parathema 'extras `(= id ,id) t))))
        (gnosis-export--insert-thema
         (if new-p "NEW" (number-to-string id))
         (nth 0 thema-data)
         (nth 1 thema-data)
         (concat (string-remove-prefix "\n" gnosis-export-separator)
                 (mapconcat 'identity (nth 2 thema-data) gnosis-export-separator))
         (concat (string-remove-prefix "\n" gnosis-export-separator)
                 (mapconcat 'identity (nth 3 thema-data) gnosis-export-separator))
         (nth 5 thema-data)
         (nth 4 thema-data))))))

;;; Export helpers

(defun gnosis-export--fetch-themata-data (thema-ids include-suspended)
  "Fetch and prepare export data for THEMA-IDS.
When INCLUDE-SUSPENDED is nil, filter out suspended themata.
Returns (ALL-THEMATA . EXTRAS-HT)."
  (let* ((all-themata (if thema-ids
			  (let* ((placeholders (mapconcat (lambda (_) "?") thema-ids ", "))
				 (sql (format "SELECT id, type, keimenon, hypothesis, answer, tags FROM themata WHERE id IN (%s)"
					      placeholders)))
			    (gnosis-sqlite-select (gnosis--ensure-db) sql thema-ids))
			(gnosis-sqlite-select (gnosis--ensure-db)
			  "SELECT id, type, keimenon, hypothesis, answer, tags FROM themata")))
         (all-ids (mapcar #'car all-themata))
         (suspended-ids (when (and all-ids (not include-suspended))
			  (let* ((placeholders (mapconcat (lambda (_) "?") all-ids ", "))
				 (sql (format "SELECT id FROM review_log WHERE id IN (%s) AND suspend = 1"
					      placeholders)))
			    (mapcar #'car (gnosis-sqlite-select (gnosis--ensure-db) sql all-ids)))))
         (all-themata (if suspended-ids
                          (cl-remove-if (lambda (row)
                                          (member (car row) suspended-ids))
                                        all-themata)
                        all-themata))
         (all-ids (mapcar #'car all-themata))
         (all-extras (when all-ids
		       (let* ((placeholders (mapconcat (lambda (_) "?") all-ids ", "))
			      (sql (format "SELECT id, parathema FROM extras WHERE id IN (%s)"
					   placeholders)))
			 (gnosis-sqlite-select (gnosis--ensure-db) sql all-ids))))
         (extras-ht (let ((ht (make-hash-table :test 'equal :size (length all-ids))))
                      (dolist (row all-extras ht)
                        (puthash (car row) (cadr row) ht)))))
    (cons all-themata extras-ht)))

(defun gnosis-export--insert-row (row extras-ht new-p)
  "Insert a single thema ROW into the current buffer.
EXTRAS-HT maps thema IDs to parathema.  When NEW-P, use \"NEW\" as ID."
  (let* ((id (nth 0 row))
         (sep-prefix (string-remove-prefix "\n" gnosis-export-separator)))
    (gnosis-export--insert-thema
     (if new-p "NEW" (number-to-string id))
     (nth 1 row) (nth 2 row)
     (concat sep-prefix (mapconcat #'identity (nth 3 row) gnosis-export-separator))
     (concat sep-prefix (mapconcat #'identity (nth 4 row) gnosis-export-separator))
     (gethash id extras-ht "")
     (nth 5 row))))

(defun gnosis-export--prepare-buffer (name filename)
  "Prepare export buffer for NAME, resolving FILENAME.
Returns (BUFFER . FILENAME)."
  (let ((filename (if (file-directory-p filename)
                      (expand-file-name name filename)
                    filename)))
    (unless (string-match-p "\\.org$" filename)
      (setq filename (concat (or filename name) ".org")))
    (let ((buffer (get-buffer-create (format "EXPORT: %s" name))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (buffer-disable-undo)
          (org-mode)
          (erase-buffer)))
      (cons buffer filename))))

;;; Export commands

(defvar gnosis-nodes-dir)

(defun gnosis-export--collect-linked-node-files (thema-ids)
  "Return list of (ID FILE) pairs for nodes linked from THEMA-IDS."
  (when thema-ids
    (let* ((placeholders (mapconcat (lambda (_) "?") thema-ids ", "))
	   (sql (format "SELECT DISTINCT n.id, n.file FROM nodes n \
INNER JOIN thema_links tl ON n.id = tl.dest \
WHERE tl.source IN (%s)" placeholders)))
      (gnosis-sqlite-select (gnosis--ensure-db) sql thema-ids))))

(defun gnosis-export--copy-node-files (node-rows directory)
  "Copy node files from NODE-ROWS to DIRECTORY/nodes/.
NODE-ROWS is a list of (ID FILE) pairs.  Returns count of copied files."
  (let ((nodes-dir (expand-file-name "nodes" directory))
	(count 0))
    (make-directory nodes-dir t)
    (dolist (row node-rows)
      (let* ((file (cadr row))
	     (src (expand-file-name file gnosis-nodes-dir))
	     (dest (expand-file-name file nodes-dir)))
	(when (file-exists-p src)
	  (copy-file src dest t)
	  (cl-incf count))))
    count))

;;;###autoload
(defun gnosis-export-themata (directory &optional new-p include-suspended
					include-tags exclude-tags)
  "Export filtered themata to DIRECTORY with linked node files.

When called interactively, prompts for tag filters using +/-
notation.  INCLUDE-TAGS and EXCLUDE-TAGS can be passed directly
for programmatic use.
When NEW-P, replace thema IDs with NEW for fresh import.
When INCLUDE-SUSPENDED, also export suspended themata."
  (interactive
   (let ((filter (gnosis-tags-filter-prompt)))
     (list (read-directory-name "Export to directory: ")
	   (not (y-or-n-p "Export with current thema ids? "))
	   (y-or-n-p "Include suspended themata? ")
	   (car filter) (cdr filter))))
  (let* ((gc-cons-threshold most-positive-fixnum)
	 (thema-ids (gnosis-filter-by-tags include-tags exclude-tags))
	 (data (gnosis-export--fetch-themata-data thema-ids include-suspended))
	 (all-themata (car data))
	 (extras-ht (cdr data))
	 (tags-str (concat
		    (when include-tags
		      (mapconcat (lambda (tag) (concat "+" tag)) include-tags " "))
		    (when (and include-tags exclude-tags) " ")
		    (when exclude-tags
		      (mapconcat (lambda (tag) (concat "-" tag)) exclude-tags " ")))))
    (make-directory directory t)
    (let ((filename (expand-file-name "themata.org" directory))
	  (inhibit-read-only t))
      (with-temp-file filename
	(insert (format "#+TAGS: %s\n" tags-str))
	(insert (format "#+THEMATA: %d\n\n" (length all-themata)))
	(dolist (row all-themata)
	  (gnosis-export--insert-row row extras-ht new-p))))
    ;; Copy linked node files
    (let* ((exported-ids (mapcar #'car all-themata))
	   (node-rows (gnosis-export--collect-linked-node-files exported-ids))
	   (node-count (if node-rows
			   (gnosis-export--copy-node-files node-rows directory)
			 0)))
      (message "Exported %d themata and %d nodes to %s"
	       (length all-themata) node-count directory))))

;;;###autoload
(defun gnosis-export-themata-to-file (&optional filename new-p include-suspended)
  "Export all non-suspended themata to FILENAME.

When NEW-P, replace thema IDs with NEW for fresh import.
When INCLUDE-SUSPENDED, also export suspended themata."
  (interactive (list (read-file-name "Export to file: ")
		     (not (y-or-n-p "Export with current thema ids? "))
		     (y-or-n-p "Include suspended themata? ")))
  (let* ((gc-cons-threshold most-positive-fixnum)
         (prepared (gnosis-export--prepare-buffer "gnosis-export" filename))
         (buffer (car prepared))
         (filename (cdr prepared))
         (data (gnosis-export--fetch-themata-data nil include-suspended))
         (all-themata (car data))
         (extras-ht (cdr data)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (insert (format "#+THEMATA: %d\n\n" (length all-themata)))
        (dolist (row all-themata)
          (gnosis-export--insert-row row extras-ht new-p))
        (when filename
          (write-file filename)
          (message "Exported %d themata to %s" (length all-themata) filename))))))

;;; Save/import

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

;;;###autoload
(defun gnosis-import-file (file)
  "Import gnosis themata from FILE."
  (interactive "fFile: ")
  (let* ((gc-cons-threshold most-positive-fixnum)
	 (gnosis--id-cache (let ((ht (make-hash-table :test 'equal)))
			     (dolist (id (gnosis-select 'id 'themata nil t) ht)
			       (puthash id t ht))))
	 (errors nil)
	 (themata (with-temp-buffer
		    (insert-file-contents file)
		    (org-mode)
		    (gnosis-export-parse-themata))))
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (cl-loop for thema in themata
	       for err = (gnosis-save-thema thema)
	       when err do (push err errors)))
    (if errors
        (user-error "Failed to import %d thema(ta):\n%s"
                    (length errors) (mapconcat #'identity (nreverse errors) "\n"))
      (message "Imported %d themata from %s" (length themata) file))))

(defun gnosis--import-split-chunks (text chunk-size)
  "Split org TEXT into chunks of CHUNK-SIZE themata.

Return a list of strings, each containing up to CHUNK-SIZE
`* Thema' headings."
  (let ((headings '())
        (start 0))
    ;; Find all `* Thema' positions
    (while (string-match "^\\* Thema" text start)
      (push (match-beginning 0) headings)
      (setf start (1+ (match-beginning 0))))
    (setq headings (nreverse headings))
    (let ((chunks '())
          (total (length headings)))
      (cl-loop for i from 0 below total by chunk-size
               for beg = (nth i headings)
               for end-idx = (min (+ i chunk-size) total)
               for end = (if (< end-idx total)
                             (nth end-idx headings)
                           (length text))
               do (push (substring text beg end) chunks))
      (nreverse chunks))))

(defun gnosis--import-chunk (chunk id-cache)
  "Import a single CHUNK of org text.

ID-CACHE is the shared `gnosis--id-cache' hash table.
Returns a list of error strings (nil on full success)."
  (let ((gc-cons-threshold most-positive-fixnum)
        (gnosis--id-cache id-cache)
        (errors nil))
    (with-temp-buffer
      (insert chunk)
      (org-mode)
      (let ((themata (gnosis-export-parse-themata)))
        (gnosis-sqlite-with-transaction (gnosis--ensure-db)
          (cl-loop for thema in themata
                   for err = (gnosis-save-thema thema)
                   when err do (push err errors)))))
    (nreverse errors)))

;;;###autoload
(defun gnosis-import-file-async (file &optional chunk-size)
  "Import gnosis themata from FILE asynchronously in chunks.

CHUNK-SIZE controls how many themata to process per batch
\(default 500).  Uses `run-with-timer' between chunks so Emacs
stays responsive.  Progress is reported in the echo area."
  (interactive "fFile: ")
  (let* ((chunk-size (or chunk-size 500))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
         (id-cache (let ((ht (make-hash-table :test 'equal)))
                     (dolist (id (gnosis-select 'id 'themata nil t) ht)
                       (puthash id t ht))))
         (chunks (gnosis--import-split-chunks text chunk-size))
         (total-chunks (length chunks))
         ;; Count total themata from the text
         (total-themata (with-temp-buffer
                          (insert text)
                          (count-matches "^\\* Thema" (point-min) (point-max))))
         (imported 0)
         (all-errors '()))
    (message "Importing %d themata in %d chunks..." total-themata total-chunks)
    (cl-labels
        ((process-next (remaining chunk-n)
           (if (null remaining)
               ;; Done
               (if all-errors
                   (message "Import complete: %d themata, %d errors"
                            imported (length all-errors))
                 (message "Import complete: %d themata" imported))
             (let* ((chunk (car remaining))
                    (errors (gnosis--import-chunk chunk id-cache))
                    ;; Count headings in this chunk
                    (n (with-temp-buffer
                         (insert chunk)
                         (count-matches "^\\* Thema" (point-min) (point-max)))))
               (setq imported (+ imported n))
               (when errors
                 (setq all-errors (append all-errors errors)))
               (message "Importing... %d/%d themata (chunk %d/%d)"
                        imported total-themata chunk-n total-chunks)
               (run-with-timer 0.01 nil
                               #'process-next (cdr remaining) (1+ chunk-n))))))
      (process-next chunks 1))))

(provide 'gnosis-export-import)
;;; gnosis-export-import.el ends here
