;;; gnosis-export-import.el --- Export/import  -*- lexical-binding: t; -*-

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
;; - Importing themata from SQLite databases with diff
;;   review (`gnosis-import-db')
;; - Editing support: parsing org buffers and saving themata (`gnosis-save')

;;; Code:

(require 'gnosis)
(require 'gnosis-db)
(require 'gnosis-answer)
(require 'gnosis-scheduler)
(require 'gnosis-tags)
(require 'gnosis-vc)
(require 'gnosis-links)
(require 'gnosis-logical-day)
(require 'gnosis-sqlite)
(require 'keymap-popup)
(require 'org)
(require 'org-element)

(declare-function gnosis-edit-quit "gnosis")

;;; Edit mode support

(defun gnosis-export--insert-read-only (string)
  "Insert STRING as read-only."
  (let ((start (point)))
    (insert string)
    (add-text-properties start (point) '(read-only t))
    (let ((inhibit-read-only t))
      (insert " "))))

(cl-defun gnosis-export--insert-thema (id type
					  &optional keimenon
					  hypothesis answer
					  parathema tags
					  example accepted-aliases)
  "Insert thema for thema ID.

TYPE: Thema type, refer to `gnosis-thema-types'
KEIMENON: Text user is first presented with.
HYPOTHESIS: Hypothesis for what the ANSWER is
ANSWER: The revelation after KEIMENON
PARATHEMA: The text where THEMA is derived from.
TAGS: List of THEMA tags
EXAMPLE: Boolean value, if non-nil do not add properties for thema.
ACCEPTED-ALIASES: Independent list of authored accepted spellings."
  (when (and (equal (downcase type) "image-occlusion")
             hypothesis answer (not (string-match-p "\n- " hypothesis)))
    (pcase-let ((`(,fields ,text)
                 (gnosis-image-occlusion-fields (list hypothesis) (list answer))))
      (setq hypothesis (mapconcat #'identity fields "\n- ") answer (car text))))
  (let ((components `(("** Keimenon" . ,keimenon)
                      (,(cond ((member (downcase type) '("model" "model-name")) "** Resource and starting view")
                              ((member (downcase type) '("image-region" "image-occlusion"))
                               "** Image resource")
                              (t "** Hypothesis")) . ,hypothesis)
                      ("** Answer" . ,answer)
                      ("** Parathema" . ,parathema)
                      ,@(when accepted-aliases
                          (list (cons "** Accepted aliases"
                                      (gnosis-answer-aliases-to-section accepted-aliases)))))))
    (goto-char (point-max))
    (insert "\n* Thema")
    (when tags
      (insert " :" (mapconcat #'identity tags ":") ":"))
    (insert "\n")
    (unless example
      (let ((start (point)))
        (insert ":PROPERTIES:\n:GNOSIS_ID: " id "\n")
        (add-text-properties
         start (point)
         '(read-only t rear-nonsticky (read-only))))
      (insert ":GNOSIS_TYPE: " type "\n")
      (let ((start (point)))
        (insert ":END:\n")
        (add-text-properties
         start (point)
         '(read-only t rear-nonsticky (read-only)))))
    (dolist (comp components)
      (goto-char (point-max))
      (gnosis-export--insert-read-only (car comp))
      (insert "\n" (or (cdr comp) "") "\n\n"))))

(defun gnosis-export--parse-field (title text separator)
  "Parse field TITLE from TEXT using list SEPARATOR."
  (cond
   ((equal title "Accepted aliases")
    (gnosis-answer-aliases-from-section text))
   ((string-empty-p text) nil)
   ((member title '("Hypothesis" "Resource and starting view" "Image resource" "Answer"))
    (if (equal text "-") '("")
      (mapcar #'string-trim
              (split-string (string-remove-prefix "- " text)
                            separator t "[ \t\n]+"))))
   (t text)))

(defun gnosis-export-parse-themata (&optional separator)
  "Extract level-1 themata by field heading, using list SEPARATOR.
Return (ID TYPE KEIMENON HYPOTHESIS ANSWER PARATHEMA TAGS LINE ALIASES).
Missing aliases mean nil.  Reject duplicate and unknown field headings."
  (let ((sep (or separator gnosis-export-separator)) results)
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (let ((id (org-element-property :GNOSIS_ID headline))
              (type (org-element-property :GNOSIS_TYPE headline)))
          (when (and (= 1 (org-element-property :level headline)) id type)
            (let (fields)
              (dolist (child (org-element-contents headline))
                (when (eq 'headline (org-element-type child))
                  (let* ((title (org-element-property :raw-value child))
                         (slot (cdr (assoc title
                                           '(("Keimenon" . 2) ("Hypothesis" . 3)
                                             ("Resource and starting view" . 3)
                                             ("Image resource" . 3) ("Answer" . 4)
                                             ("Parathema" . 5) ("Accepted aliases" . 8)))))
                         (raw (if (equal title "Accepted aliases")
                                  ;; Org interpretation rewrites checkboxes and
                                  ;; spacing; aliases are literal authored text.
                                  (save-excursion
                                    (goto-char (org-element-property :begin child))
                                    (forward-line 1)
                                    (buffer-substring-no-properties
                                     (point) (org-element-property :end child)))
                                (substring-no-properties
                                 (org-element-interpret-data (org-element-contents child)))))
                         (text (if (equal title "Accepted aliases")
                                   (string-trim raw "[\n\r]+" "[\n\r]+")
                                 (string-trim raw))))
                    (unless slot (user-error "Unknown thema field: %s" title))
                    (when (assq slot fields) (user-error "Duplicate thema field: %s" title))
                    (push (cons slot (gnosis-export--parse-field title text sep)) fields))))
              (push (append
                     (list id type (alist-get 2 fields) (alist-get 3 fields)
                           (alist-get 4 fields) (alist-get 5 fields)
                           (org-element-property :tags headline)
                           (line-number-at-pos (org-element-property :begin headline)))
                     (when (assq 8 fields) (list (alist-get 8 fields))))
                    results)))))
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
      (let* ((thema-data
              (append (gnosis-select
                      '[type keimenon hypothesis answer]
                      'themata `(= id ,id) t)
                     (gnosis-select 'parathema 'extras
                                    `(= id ,id) t)))
             ;; Resolve legacy fields before native list markers are added.
             (fields (if (equal (downcase (car thema-data)) "image-occlusion")
                         (gnosis-image-occlusion-fields (nth 2 thema-data) (nth 3 thema-data))
                       (list (nth 2 thema-data) (nth 3 thema-data))))
             (tags (gnosis-select 'tag 'thema-tag
                                  `(= thema-id ,id) t)))
        (gnosis-export--insert-thema
         (if new-p "NEW" (number-to-string id))
         (nth 0 thema-data)
         (nth 1 thema-data)
         (concat (string-remove-prefix
                  "\n" gnosis-export-separator)
                 (mapconcat #'identity
                            (car fields)
                            gnosis-export-separator))
         (concat (string-remove-prefix
                  "\n" gnosis-export-separator)
                 (mapconcat #'identity
                            (cadr fields)
                            gnosis-export-separator))
         (nth 4 thema-data)
         tags nil (gnosis-get 'accepted-aliases 'themata `(= id ,id)))))))

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
         (links (append
                 (gnosis-extract-id-links parathema)
                 (gnosis-extract-id-links keimenon)))
         (thema-func
          (cdr (assoc
                (downcase type)
                (mapcar (lambda (pair)
                          (cons (downcase (car pair))
                                (cdr pair)))
                        gnosis-thema-types)))))
    (cl-assert (and type (stringp type) thema-func) nil
               "GNOSIS_TYPE must be one of: %s (got %S)"
               (mapconcat #'car gnosis-thema-types ", ")
               type)
    (condition-case err
        (progn
          (let* ((aliases (nth 8 thema))
                 (arguments (list id type keimenon hypothesis answer parathema
                                  tags 0 links))
                 (maximum (cdr (func-arity thema-func))))
            ;; Old third-party handlers can still save alias-free drafts.
            ;; Never discard authored aliases for a handler that cannot save them.
            (apply thema-func
                   (if (and (null aliases) (eql maximum 9)) arguments
                     (append arguments (list aliases)))))
          nil)
      (error
       (format "Line %s (id:%s): %s"
               (or line "?") id
               (error-message-string err))))))

;;;###autoload
(defun gnosis-save ()
  "Save themata in the current native draft.
Refuse changed database ownership or original content without discarding
the draft.  Copy its text before cancelling and reopening to reconcile."
  (interactive nil gnosis-edit-mode)
  (gnosis--draft-check-owner)
  (let* ((gc-cons-threshold most-positive-fixnum)
         (themata (gnosis-export-parse-themata))
         (gnosis--id-cache
          (let ((ht (make-hash-table :test 'equal)))
            (dolist (id (gnosis-select 'id 'themata nil t) ht)
              (puthash id t ht))))
         (errors nil)
         (receipt gnosis--draft-save-receipt)
         (saved-content nil)
         (edited-id (string-to-number (caar themata))))
    (catch 'gnosis-save-failed
      (gnosis-sqlite-with-transaction gnosis--draft-db
        (gnosis--draft-validate themata)
        (cl-loop for thema in themata
                 for err = (gnosis-save-thema thema)
                 when err do (push err errors))
        (when errors
          (throw 'gnosis-save-failed nil))
        (when (and receipt gnosis--draft-original)
          (setq saved-content
                (list gnosis--draft-db (car gnosis--draft-original)
                      (seq-take (gnosis--draft-content
                                 gnosis--draft-db (car gnosis--draft-original)) 2))))))
    (if errors
        (user-error
         "Failed to import %d thema(ta):\n%s"
         (length errors)
         (mapconcat #'identity (nreverse errors) "\n"))
      (when receipt (setcar receipt saved-content))
      (gnosis-edit-quit)
      (run-hook-with-args 'gnosis-save-hook edited-id))))

;;; SQLite export

(defconst gnosis-export--themata-schema
  "CREATE TABLE export_db.themata (
  id INTEGER PRIMARY KEY,
  type TEXT NOT NULL,
  keimenon TEXT NOT NULL,
  hypothesis TEXT NOT NULL,
  answer TEXT NOT NULL,
  accepted_aliases TEXT)"
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

(defconst gnosis-export-format-version 3
  "Current SQLite content export format version.")

(defun gnosis-export--image-ids (db schema)
  "Return IDs with managed images in content DB SCHEMA, including malformed refs."
  (unless (member schema '("main" "import_db")) (error "Invalid content schema"))
  (cl-loop for row in (gnosis-sqlite-select
                      db (format "SELECT t.id, t.type, t.keimenon, t.hypothesis,
                                         t.answer, e.parathema, e.review_image, %s
                                  FROM %s.themata t LEFT JOIN %s.extras e ON t.id = e.id"
                                 (if (seq-some
                                      (lambda (column) (equal (nth 1 column) "accepted_aliases"))
                                      (sqlite-select db (format "PRAGMA %s.table_info(themata)" schema)))
                                     "t.accepted_aliases" "NULL")
                                 schema schema))
           when (or (member (downcase (nth 1 row)) '("image-region" "image-occlusion"))
                    (gnosis-image-content-p (cddr row)))
           collect (car row)))

(defun gnosis-import--format-version-in-db (db schema)
  "Return supported content format version from DB SCHEMA."
  (unless (member schema '("main" "import_db"))
    (error "Invalid Gnosis content schema"))
  (when (gnosis-sqlite-select
         db (format "SELECT id FROM %s.themata WHERE lower(type) IN (?, ?)" schema)
         '("model" "model-name"))
    (user-error "Model resource content import is unsupported; assets are not bundled"))
  (when (gnosis-export--image-ids db schema)
    (user-error "Managed image content import is unsupported; assets are not bundled"))
  (let ((objects
         (sqlite-select
          db (format "SELECT type, name FROM %s.sqlite_master
                       WHERE lower(name) = 'gnosis_meta'" schema))))
    (if (null objects) 1
      (unless (and (= (length objects) 1)
                   (equal "table" (caar objects)))
        (error "Invalid Gnosis metadata object"))
      (let ((columns
             (mapcar
              (lambda (row) (list (nth 1 row) (upcase (nth 2 row)) (nth 5 row)))
              (sqlite-select db (format "PRAGMA %s.table_info(gnosis_meta)"
                                        schema))))
            (versions
             (sqlite-select
              db (format "SELECT key, value FROM %s.gnosis_meta
                           WHERE lower(key) = 'format_version'" schema))))
        (unless (equal columns '(("key" "TEXT" 1) ("value" "TEXT" 0)))
          (error "Invalid Gnosis metadata schema"))
        (cond
         ((null versions) 1)
         ((and (= (length versions) 1)
               (equal "format_version" (caar versions)))
          (let ((raw (cadar versions)))
            (unless (and (stringp raw)
                         (string-match-p "\\`[0-9]+\\'" raw))
              (error "Invalid Gnosis content format version"))
            (let ((version (string-to-number raw)))
              (unless (memq version (list 1 2 gnosis-export-format-version))
                (error "Unsupported Gnosis content format version: %s"
                       version))
              version)))
         (t (error "Invalid Gnosis format-version metadata")))))))

(defun gnosis-import--format-version (file)
  "Return supported content format version of SQLite FILE."
  (let ((db (sqlite-open file)))
    (unwind-protect
        (gnosis-import--format-version-in-db db "main")
      (sqlite-close db))))

(defun gnosis-import--file-sha256 (file)
  "Return SHA-256 identity of FILE bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defconst gnosis-export--sqlite-companions '("-wal" "-shm" "-journal")
  "Companion suffixes which must not be replaced independently of SQLite.")

(defun gnosis-export--check-destination (db file)
  "Reject FILE if it overlaps DB files or has SQLite companions.
Protect active database and companion aliases, even before creation.
Never remove, recover or checkpoint another owner's companion files."
  (when (file-remote-p file)
    (user-error "Export requires a local destination"))
  (let ((target (file-truename file)))
    (dolist (row (sqlite-select db "PRAGMA database_list"))
      (let ((active (nth 2 row)))
        (when (and active (not (string-empty-p active)))
          (dolist (base (list active (file-truename active)))
            (dolist (suffix (cons "" gnosis-export--sqlite-companions))
              (let ((protected (concat base suffix)))
                (when (or (equal target (file-truename protected))
                          (and (file-exists-p file) (file-exists-p protected)
                               (file-equal-p file protected)))
                  (user-error "Cannot export over an active Gnosis database or companion"))))))))
    (dolist (base (list file target))
      (dolist (suffix gnosis-export--sqlite-companions)
        (let ((companion (concat base suffix)))
          (when (or (file-exists-p companion) (file-symlink-p companion))
            (user-error "Cannot replace an export with SQLite companions: %s"
                        companion)))))))

(defun gnosis-export--validate (file count)
  "Validate completed export FILE containing COUNT themata."
  (let ((db (gnosis-sqlite-open file)))
    (unwind-protect
        (unless (and (= (gnosis-import--format-version-in-db db "main")
                        gnosis-export-format-version)
                     (equal '(("ok")) (sqlite-select db "PRAGMA integrity_check"))
                     (null (sqlite-select db "PRAGMA foreign_key_check"))
                     (= count (caar (sqlite-select db "SELECT COUNT(*) FROM themata")))
                     (equal (number-to-string count)
                            (caar (sqlite-select db "SELECT value FROM gnosis_meta
                                                    WHERE key = 'thema_count'")))
                     (equal '(("extras") ("gnosis_meta") ("thema_tag") ("themata"))
                            (sqlite-select db "SELECT name FROM sqlite_master
                                               WHERE type = 'table' ORDER BY name")))
          (error "Invalid completed Gnosis export"))
      (sqlite-close db))))

(defun gnosis-export--write-content (db ids selection-p count)
  "Write content for IDS to the attached export database on DB.
SELECTION-P distinguishes an empty selection from all themata.
Record COUNT as the expected number of exported themata."
  (gnosis-sqlite-with-transaction db
    (gnosis-import--content-rows
     db "main" (if selection-p ids
                 (mapcar #'car (sqlite-select db "SELECT id FROM themata"))))
    (dolist (schema (list gnosis-export--themata-schema
                         gnosis-export--thema-tag-schema
                         gnosis-export--extras-schema
                         gnosis-export--meta-schema))
      (gnosis-sqlite-execute db schema))
    (unless (and selection-p (null ids))
      (pcase-dolist (`(,table ,columns ,key)
                    '(("themata" "id, type, keimenon, hypothesis, answer, accepted_aliases" "id")
                      ("extras" "id, parathema, review_image" "id")
                      ("thema_tag" "thema_id, tag" "thema_id")))
        (let ((sql (format "INSERT INTO export_db.%s SELECT %s FROM main.%s"
                           table columns table)))
          (if selection-p
              (gnosis-sqlite-execute-batch
               db (concat sql " WHERE " key " IN (%s)") ids)
            (gnosis-sqlite-execute db sql)))))
    ;; Metadata is plain text, not EmacSQL-encoded content.
    (dolist (row `(("format_version" ,(number-to-string gnosis-export-format-version))
                   ("exported_at" ,(format-time-string "%Y-%m-%dT%H:%M:%S"))
                   ("thema_count" ,(number-to-string count))))
      (sqlite-execute db "INSERT INTO export_db.gnosis_meta (key, value)
                          VALUES (?, ?)" row))))

(defun gnosis-export--replace-file (db file ids selection-p count)
  "Atomically replace FILE with exported content from DB.
IDS, SELECTION-P and COUNT describe the reviewed selection.  Build and
validate a private sibling first; preserve FILE on error or quit."
  (gnosis-export--check-destination db file)
  (let ((scratch (make-temp-file
                  (expand-file-name ".gnosis-export-" (file-name-directory file))
                  nil ".db"))
        attached)
    (unwind-protect
        (progn
          (unwind-protect
              (progn
                (let ((inhibit-quit t))
                  (sqlite-execute db "ATTACH DATABASE ? AS export_db" (list scratch))
                  (setq attached t))
                (gnosis-export--write-content db ids selection-p count))
            (when attached
              (let ((inhibit-quit t))
                (sqlite-execute db "DETACH DATABASE export_db"))))
          (gnosis-export--validate scratch count)
          (let ((inhibit-quit t))
            (gnosis-export--check-destination db file)
            (rename-file scratch file t)))
      (when (file-exists-p scratch)
        (delete-file scratch)))))

;;;###autoload
(defun gnosis-export-db (file &optional include-tags
                              exclude-tags include-suspended)
  "Export filtered themata to FILE as a SQLite database.

When called interactively, prompt for tag filters.
INCLUDE-TAGS and EXCLUDE-TAGS filter themata.
When INCLUDE-SUSPENDED, also export suspended themata.
Reject active database and companion aliases, and destinations with SQLite
companions.  Replace an existing FILE only after completing and validating
the export; errors preserve the previous file."
  (interactive
   (let ((filter (gnosis-tags-filter-prompt)))
     (list (read-file-name "Export database: "
                           nil nil nil "gnosis-export.gnosis")
           (car filter) (cdr filter)
           (y-or-n-p "Include suspended themata? "))))
  (let* ((db (gnosis--ensure-db))
         (file (expand-file-name file))
         (suspended-ids
          (unless include-suspended
            (mapcar #'car
                    (gnosis-sqlite-select
                     db "SELECT thema_id FROM scheduler_state WHERE suspended = 1"))))
         (selection-p (or include-tags exclude-tags suspended-ids))
         (ids (when selection-p
                (seq-difference (gnosis-filter-by-tags include-tags exclude-tags)
                                suspended-ids)))
         (count (if selection-p (length ids)
                  (caar (sqlite-select db "SELECT COUNT(*) FROM themata")))))
    (when (seq-some (lambda (row)
                      (or (not selection-p) (member (car row) ids)))
                    (gnosis-sqlite-select db "SELECT id FROM themata WHERE lower(type) IN (?, ?)"
                                          '("model" "model-name")))
      (user-error "Model resource content export is unsupported; back up DB and assets together"))
    (when (seq-some (lambda (id) (or (not selection-p) (member id ids)))
                    (gnosis-export--image-ids db "main"))
      (user-error "Managed image content export is unsupported; back up DB and assets together"))
    (gnosis-export--check-destination db file)
    (when (called-interactively-p 'any)
      (unless (y-or-n-p (format "Export %d themata to %s? " count file))
        (user-error "Export cancelled")))
    (gnosis-export--replace-file db file ids selection-p count)
    (message "Exported %d themata to %s" count file)))

;;; SQLite import

(defun gnosis-import--commit (new-count changed-count filename)
  "Commit database after importing NEW-COUNT new entries.
CHANGED-COUNT is the updated count from FILENAME."
  (gnosis-vc--auto-commit
   (format "Import: %d new, %d updated from %s"
           new-count changed-count filename)
   t))

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

(defvar-local gnosis-import--source-id nil
  "SHA-256 identity of the reviewed import database bytes.")

(defvar-local gnosis-import--new-ids nil
  "List of new thema IDs from the import.")

(defvar-local gnosis-import--changed-ids nil
  "List of changed thema IDs from the import.")

(defvar-local gnosis-import--destination nil
  "Database handle and content rows shown in the reviewed import diff.")

(defvar-local gnosis-import--details nil
  "Reviewed (ID CURRENT INCOMING) entries for detail rendering.
CURRENT and INCOMING are normalized content rows, never reread for display.")

(defun gnosis-import--normalize-rows (rows tags)
  "Return content ROWS with sorted TAGS inserted before their aliases.
TAGS contains (THEMA-ID TAG) rows.  Preserve absent and empty values."
  (let ((by-id (make-hash-table :test #'eql)))
    (pcase-dolist (`(,id ,tag) tags)
      (puthash id (cons tag (gethash id by-id)) by-id))
    (mapcar (lambda (row)
              (append (seq-take row 7)
                      (list (sort (copy-sequence (gethash (car row) by-id)) #'string<)
                            (nth 7 row))))
            rows)))

(defun gnosis-import--alias-column (db schema)
  "Return alias SQL expression for content DB SCHEMA.
Formats 1 and 2 have no aliases; format 3 must carry the column."
  (unless (member schema '("main" "import_db"))
    (error "Invalid Gnosis content schema"))
  (if (or (equal schema "main")
          (= 3 (gnosis-import--format-version-in-db db schema)))
      "accepted_aliases"
    "NULL"))

(defun gnosis-import--validate-alias-row (row)
  "Validate independent aliases in normalized content ROW and return ROW."
  (let ((aliases (gnosis-answer-validate-aliases (nth 8 row))))
    (when aliases
      (unless (and (member (downcase (nth 1 row))
                           '("basic" "image-occlusion" "model-name"))
                   (proper-list-p (nth 4 row))
                   (= 1 (length (nth 4 row)))
                   (stringp (car (nth 4 row)))
                   (not (string-empty-p (string-trim (car (nth 4 row))))))
        (user-error "Aliases require one canonical typed answer"))))
  row)

(defun gnosis-import--content-rows (db schema ids)
  "Read content rows for IDS in DB SCHEMA.
Each row contains ID, TYPE, KEIMENON, HYPOTHESIS, ANSWER, PARATHEMA,
REVIEW-IMAGE, TAGS, and ALIASES, in that order.
TAGS is sorted; SQL NULL and empty strings remain distinct."
  (unless (member schema '("main" "import_db"))
    (error "Invalid Gnosis content schema"))
  (mapcar #'gnosis-import--validate-alias-row
   (gnosis-import--normalize-rows
   (gnosis-sqlite-select-batch
    db (format "SELECT t.id, t.type, t.keimenon, t.hypothesis, t.answer,
                       e.parathema, e.review_image, %s
                  FROM %s.themata t LEFT JOIN %s.extras e ON e.id = t.id
                 WHERE t.id IN (%%s) ORDER BY t.id"
               (gnosis-import--alias-column db schema) schema schema)
    ids)
   (gnosis-sqlite-select-batch
    db (format "SELECT thema_id, tag FROM %s.thema_tag
                 WHERE thema_id IN (%%s)" schema)
    ids))))

(defconst gnosis-import--content-fields
  '("type" "keimenon" "hypothesis" "answer" "parathema" "review_image" "tags" "accepted_aliases")
  "Field names in a normalized portable content row, after its ID.")

(defun gnosis-import--change-plan (incoming current)
  "Return new and changed preview rows from INCOMING and CURRENT content.
Content rows follow `gnosis-import--content-rows'."
  (let ((new (cl-remove-if (lambda (row) (assoc (car row) current)) incoming))
        (changed
         (cl-loop for row in incoming
                  for old = (assoc (car row) current)
                  when (and old (not (equal row old)))
                  collect
                  (append (seq-take row 3)
                          (list
                           (cl-loop for name in gnosis-import--content-fields
                                    for before in (cdr old)
                                    for after in (cdr row)
                                    unless (equal before after)
                                    collect (list name before after)))))))
    (list (mapcar (lambda (row) (seq-take row 3)) new) changed)))

(defun gnosis-import--destination-state (db ids)
  "Return DB identity and relevant content state for IDS."
  (let ((rows (gnosis-import--content-rows db "main" ids)))
    (list db (mapcar (lambda (id) (cons id (assoc id rows))) ids))))

(defun gnosis-import--diff-snapshot (file)
  "Compute an import preview from immutable snapshot FILE.
Return (NEW-ROWS CHANGED-ROWS DESTINATION DETAILS), where new rows are
\(ID TYPE KEIMENON), changed rows append a list of (FIELD OLD NEW) entries,
DESTINATION pins the database and only the content to be overwritten,
and DETAILS retains (ID CURRENT INCOMING) entries for those same IDs."
  (gnosis-import--format-version file)
  (let ((db (gnosis--ensure-db)))
    (sqlite-execute db "ATTACH DATABASE ? AS import_db" (list file))
    (unwind-protect
        (gnosis-sqlite-with-transaction db
          (gnosis-import--format-version-in-db db "import_db")
          (let* ((ids (mapcar #'car (sqlite-select
                                    db "SELECT id FROM import_db.themata ORDER BY id")))
                 (incoming (gnosis-import--content-rows db "import_db" ids))
                 (current (gnosis-import--content-rows db "main" ids))
                 (plan (gnosis-import--change-plan incoming current))
                 (reviewed-ids (mapcar #'car (append (car plan) (cadr plan)))))
            (append plan
                    (list (gnosis-import--destination-state db reviewed-ids)
                          (mapcar (lambda (id)
                                    (list id (assoc id current) (assoc id incoming)))
                                  reviewed-ids)))))
      (sqlite-execute db "DETACH DATABASE import_db"))))

(defun gnosis-import--diff (file)
  "Compute reviewed import diff and source identity from FILE."
  (let ((snapshot (make-temp-file "gnosis-import-diff-" nil ".db")))
    (unwind-protect
        (progn
          (copy-file file snapshot t)
          (let ((source-id (gnosis-import--file-sha256 snapshot))
                (diff (gnosis-import--diff-snapshot snapshot)))
            (list (car diff) (cadr diff) source-id (nth 2 diff) (nth 3 diff))))
      (when (file-exists-p snapshot)
        (delete-file snapshot)))))

(defun gnosis-import--render-diff (new-rows changed-rows)
  "Render diff entries in current tabulated-list buffer.
NEW-ROWS: (ID TYPE KEIMENON).
CHANGED-ROWS: (ID TYPE KEIMENON CHANGES), with (FIELD OLD NEW) changes."
  (setq tabulated-list-entries
        (append
         (mapcar (lambda (row)
                   (let ((id (car row))
                         (type (nth 1 row))
                         (keimenon
                          (truncate-string-to-width
                           (or (nth 2 row) "")
                           40 nil nil t)))
                     (list id (vector
                               (propertize
                                "NEW" 'face
                                'gnosis-import-new-face)
                               (number-to-string id)
                               (or type "")
                               keimenon
                               ""))))
                 new-rows)
         (mapcar (lambda (row)
                   (let ((id (car row))
                         (type (nth 1 row))
                         (keimenon
                          (truncate-string-to-width
                           (or (nth 2 row) "")
                           40 nil nil t))
                         (fields (mapconcat #'car (nth 3 row) ", ")))
                     (list id (vector
                               (propertize
                                "CHANGED" 'face
                                'gnosis-import-changed-face)
                               (number-to-string id)
                               (or type "")
                               keimenon
                               fields))))
                 changed-rows)))
  (tabulated-list-print t))

(defun gnosis-import--write-changes (db new-ids changed-ids today)
  "Write NEW-IDS and CHANGED-IDS from the attached import database on DB.
Initialize new study state for TODAY.  The caller owns the transaction."
  (gnosis-sqlite-execute-batch
   db (format "INSERT INTO themata (id, type, keimenon, hypothesis, answer, accepted_aliases)
       SELECT id, type, keimenon, hypothesis, answer, %s FROM import_db.themata
        WHERE id IN (%%s)" (gnosis-import--alias-column db "import_db"))
   new-ids)
  (gnosis-sqlite-execute-batch
   db (format "UPDATE themata SET
       (type, keimenon, hypothesis, answer, accepted_aliases) =
         (SELECT type, keimenon, hypothesis, answer, %s FROM import_db.themata i
           WHERE i.id = themata.id)
       WHERE id IN (%%s)" (gnosis-import--alias-column db "import_db"))
   changed-ids)
  (let ((ids (append new-ids changed-ids)))
    ;; Replace optional rows as well as values; never touch study history.
    (gnosis-sqlite-execute-batch
     db "DELETE FROM extras WHERE id IN (%s)" changed-ids)
    (gnosis-sqlite-execute-batch
     db "INSERT INTO extras (id, parathema, review_image)
         SELECT id, parathema, review_image FROM import_db.extras
          WHERE id IN (%s)"
     ids)
    (gnosis-sqlite-execute-batch
     db "DELETE FROM thema_tag WHERE thema_id IN (%s)" changed-ids)
    (gnosis-sqlite-execute-batch
     db "INSERT OR IGNORE INTO thema_tag (thema_id, tag)
         SELECT thema_id, tag FROM import_db.thema_tag WHERE thema_id IN (%s)"
     ids)
    (when new-ids
      (gnosis-scheduler-initialize-themata
       (mapcar (lambda (id) (list id today 0)) new-ids) db))
    (gnosis-sqlite-execute-batch
     db "DELETE FROM thema_links WHERE source IN (%s)" changed-ids)
    (dolist (row (gnosis-import--content-rows db "main" ids))
      (dolist (link (gnosis--thema-expected-links (or (nth 2 row) "")
                                                 (or (nth 5 row) "")))
        (gnosis-sqlite-execute
         db "INSERT INTO thema_links (source, dest) VALUES (?, ?)"
         (list (car row) link))))))

(defun gnosis-import--apply-snapshot (file new-ids changed-ids &optional destination)
  "Apply NEW-IDS and CHANGED-IDS from immutable snapshot FILE.
When DESTINATION is non-nil, revalidate the reviewed destination state
inside the write transaction before applying any change."
  (let ((db (gnosis--ensure-db))
        (today (gnosis--date-to-int (gnosis-date)))
        attached)
    (unwind-protect
        (progn
          (let ((inhibit-quit t))
            (sqlite-execute db "ATTACH DATABASE ? AS import_db" (list file))
            (setq attached t))
          (gnosis-import--format-version-in-db db "import_db")
          (gnosis-sqlite-with-transaction db
            (when (and destination
                       (not (equal destination
                                   (gnosis-import--destination-state
                                    db (append new-ids changed-ids)))))
              (user-error "Gnosis import destination changed; review the import again"))
            (gnosis-import--content-rows db "import_db" (append new-ids changed-ids))
            (gnosis-import--write-changes db new-ids changed-ids today)))
      (when attached
        (let ((inhibit-quit t))
          (sqlite-execute db "DETACH DATABASE import_db"))))))

(defun gnosis-import--apply-changes
    (file new-ids changed-ids source-id &optional destination)
  "Import from reviewed FILE pinned by SOURCE-ID.
Insert NEW-IDS and update CHANGED-IDS.  When DESTINATION is non-nil,
require the reviewed database and content rows to be unchanged."
  (unless (and (stringp source-id)
               (string-match-p "\\`[0-9a-f]\\{64\\}\\'" source-id))
    (error "Invalid Gnosis import source identity"))
  (let ((snapshot (make-temp-file "gnosis-import-source-" nil ".db")))
    (unwind-protect
        (progn
          (copy-file file snapshot t)
          (unless (equal source-id (gnosis-import--file-sha256 snapshot))
            (error "Gnosis import source changed after review"))
          (gnosis-import--format-version snapshot)
          (gnosis-import--apply-snapshot snapshot new-ids changed-ids destination))
      (when (file-exists-p snapshot)
        (delete-file snapshot)))))

(defun gnosis-import-apply ()
  "Apply the complete import diff."
  (interactive nil gnosis-import-diff-mode)
  (let ((new-ids gnosis-import--new-ids)
        (changed-ids gnosis-import--changed-ids)
        (file gnosis-import--file)
        (source-id gnosis-import--source-id)
        (destination gnosis-import--destination))
    (unless (or new-ids changed-ids)
      (user-error "No changes to apply"))
    (unless (y-or-n-p
             (format "Apply %d new and %d changed themata? "
                     (length new-ids)
                     (length changed-ids)))
      (user-error "Import cancelled"))
    (unless destination
      (user-error "Missing import destination; review the import again"))
    (gnosis-import--apply-changes
     file new-ids changed-ids source-id destination)
    (gnosis-import--commit
     (length new-ids) (length changed-ids)
     (file-name-nondirectory file))
    (message "Applied: %d new, %d updated"
             (length new-ids) (length changed-ids))
    (quit-window t)))

(defun gnosis-import--field-text (value)
  "Return display text for VALUE without conflating nil and empty strings."
  (if (equal value "") "\"\"" (format "%s" value)))

(defun gnosis-import--insert-field (label old new &optional new-p)
  "Insert field LABEL comparing OLD and NEW values.
When NEW-P is non-nil, display only NEW for a newly imported thema."
  (insert (propertize (format "%s:\n" label) 'face 'bold))
  (let ((old-s (gnosis-import--field-text old))
        (new-s (gnosis-import--field-text new)))
    (cond
     (new-p (insert "  " new-s "\n\n"))
     ((equal old new) (insert "  " old-s "\n\n"))
     (t
      (insert (propertize (concat "  - " old-s "\n")
                          'face 'gnosis-import-changed-face)
              (propertize (concat "  + " new-s "\n\n")
                          'face 'gnosis-import-new-face))))))

(defun gnosis-import--render-detail (id status data)
  "Render detail buffer for thema ID with STATUS using content DATA.
DATA contains current and imported rows from `gnosis-import--content-rows'."
  (let ((current (car data))
        (incoming (cadr data))
        (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (format "Thema %d  [%s]\n\n" id status) 'face 'bold))
    (cl-loop for name in gnosis-import--content-fields
             for index from 1
             do (gnosis-import--insert-field
                 (capitalize (string-replace "_" " " name))
                 (nth index current) (nth index incoming) (null current)))
    (goto-char (point-min))
    (special-mode)))

(defun gnosis-import-view-detail ()
  "Show the retained, reviewed diff for the thema at point."
  (interactive nil gnosis-import-diff-mode)
  (let* ((id (tabulated-list-get-id))
         (data (cdr (assoc id gnosis-import--details))))
    (unless data
      (user-error "No reviewed thema at point"))
    (let ((buf (get-buffer-create
                "*Gnosis Import Detail*")))
      (with-current-buffer buf
        (gnosis-import--render-detail
         id (if (car data) "CHANGED" "NEW") data))
      (display-buffer buf))))

(keymap-popup-define gnosis-import-diff-mode-map
  "Import Review"
  :description "Import Review"
  :group (lambda ()
           (gnosis-import--menu-description))
  "RET" ("View detail" gnosis-import-view-detail)
  "a" ("Apply changes" gnosis-import-apply)
  "q" ("Quit" quit-window))

(define-derived-mode gnosis-import-diff-mode
  tabulated-list-mode "Gnosis Import"
  "Major mode for reviewing gnosis import diffs.

\\{gnosis-import-diff-mode-map}"
  :interactive nil
  (setq tabulated-list-format
        [("Status" 8 t)
         ("ID" 12 t)
         ("Type" 10 t)
         ("Keimenon" 40 t)
         ("Changes" 30 t)])
  (tabulated-list-init-header))

(defun gnosis-import--menu-description ()
  "Return description string for import menu."
  (let ((new-count (length gnosis-import--new-ids))
        (changed-count
         (length gnosis-import--changed-ids)))
    (concat
     "Import: "
     (propertize (format "%d new" new-count)
                 'face 'gnosis-import-new-face)
     ", "
     (propertize (format "%d changed" changed-count)
                 'face
                 'gnosis-import-changed-face))))


;;;###autoload
(defun gnosis-import-db (file)
  "Import themata from SQLite database FILE.
Shows a diff buffer for review before applying."
  (interactive
   (list (read-file-name "Import database: ")))
  (let ((file (expand-file-name file)))
    (unless (file-exists-p file)
      (user-error "File does not exist: %s" file))
    (let* ((diff (gnosis-import--diff file))
           (new-rows (car diff))
           (changed-rows (cadr diff))
           (source-id (nth 2 diff)))
      (if (and (null new-rows) (null changed-rows))
          (message "No new or changed themata in %s"
                   file)
        (let ((buf (get-buffer-create
                    "*Gnosis Import*")))
          (with-current-buffer buf
            (gnosis-import-diff-mode)
            (setq gnosis-import--file file)
            (setq gnosis-import--source-id source-id)
            (setq gnosis-import--destination (nth 3 diff))
            (setq gnosis-import--details (nth 4 diff))
            (setq gnosis-import--new-ids
                  (mapcar #'car new-rows))
            (setq gnosis-import--changed-ids
                  (mapcar #'car changed-rows))
            (gnosis-import--render-diff
             new-rows changed-rows))
          (pop-to-buffer buf))))))

(provide 'gnosis-export-import)
;;; gnosis-export-import.el ends here
