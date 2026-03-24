;;; gnosis-anki.el --- Anki import for gnosis  -*- lexical-binding: t; -*-

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

;; Import Anki collections (.anki2, .anki21, .apkg) into gnosis.
;;
;; Supports basic and cloze note types.  Converts HTML markup to
;; org-mode equivalents (bold, italic, underline, sub/superscript,
;; links).  Splits Anki hierarchical tags into individual gnosis tags.
;; Skips image occlusion notes.

;;; Code:

(require 'gnosis)
(require 'gnosis-algorithm)
(require 'seq)

(defun gnosis-anki--html-to-org (str)
  "Convert HTML markup in STR to org-mode equivalents.
Converts bold, italic, underline, sub/superscript and links.
Strips remaining HTML tags.  Collapses excessive blank lines.
Short-circuits when STR contains no HTML markup or entities."
  (if (not (or (string-match-p "[<&]" str)
               (string-match-p "\n\\{3,\\}" str)))
      (string-trim str)
    (let ((s str))
      (when (string-match-p "<" s)
        (setq s (replace-regexp-in-string "<b>\\(.*?\\)</b>" "*\\1*" s))
        (setq s (replace-regexp-in-string "<i>\\(.*?\\)</i>" "/\\1/" s))
        (setq s (replace-regexp-in-string "<u>\\(.*?\\)</u>" "_\\1_" s))
        (setq s (replace-regexp-in-string "<sub>\\(.*?\\)</sub>" "_{\\1}" s))
        (setq s (replace-regexp-in-string "<sup>\\(.*?\\)</sup>" "^{\\1}" s))
        (setq s (replace-regexp-in-string
                 "<a [^>]*href=\"\\([^\"]+\\)\"[^>]*>\\(.*?\\)</a>" "[[\\1][\\2]]" s))
        (setq s (replace-regexp-in-string "<br\\s*/?>\\s*" "\n" s))
        (setq s (replace-regexp-in-string "<div>\\(.*?\\)</div>" "\\1\n" s))
        (setq s (replace-regexp-in-string "<[^>]+>" "" s)))
      (when (string-match-p "&" s)
        (setq s (replace-regexp-in-string "&nbsp;" " " s))
        (setq s (replace-regexp-in-string "&amp;" "&" s))
        (setq s (replace-regexp-in-string "&lt;" "<" s))
        (setq s (replace-regexp-in-string "&gt;" ">" s))
        (setq s (replace-regexp-in-string "&quot;" "\"" s)))
      (setq s (replace-regexp-in-string "\n\\{3,\\}" "\n\n" s))
      (string-trim s))))

(defun gnosis-anki--strip-emphasis (str)
  "Strip org emphasis markers from STR.
Removes *bold*, /italic/, _underline_ wrappers, keeping content."
  (let ((s str))
    (setq s (replace-regexp-in-string "\\*\\([^*]+\\)\\*" "\\1" s))
    (setq s (replace-regexp-in-string "/\\([^/]+\\)/" "\\1" s))
    (setq s (replace-regexp-in-string "_\\([^_]+\\)_" "\\1" s))
    s))

(defun gnosis-anki--sanitize-segment (seg)
  "Sanitize a single tag SEG for org-mode.
Replaces dashes with underscores, removes non-alphanumeric characters
\(keeping underscore and @), collapses repeated underscores, and trims
leading/trailing underscores.  Returns nil for empty results."
  (let* ((s (subst-char-in-string ?- ?_ seg))
         (s (replace-regexp-in-string "[^[:alnum:]_@]" "" s))
         (s (replace-regexp-in-string "__+" "_" s))
         (s (string-trim s "_")))
    (unless (string-empty-p s) s)))

(defun gnosis-anki--parse-tags (tag-string &optional seg-cache seen)
  "Parse Anki TAG-STRING into a flat list of unique tags.
Splits on :: and whitespace, sanitizes each segment via a
segment-level cache SEG-CACHE.  SEEN is a reusable hash table
for per-call dedup (caller should `clrhash' it between calls)."
  (let ((seg-cache (or seg-cache (make-hash-table :test 'equal)))
        (seen (or seen (make-hash-table :test 'equal)))
        (tags nil))
    (dolist (seg (split-string tag-string "[ \t]+\\|::" t))
      (let ((tag (or (gethash seg seg-cache)
                     (puthash seg (or (gnosis-anki--sanitize-segment seg) "")
                              seg-cache))))
        (unless (or (string-empty-p tag) (gethash tag seen))
          (puthash tag t seen)
          (push tag tags))))
    (nreverse tags)))

(defun gnosis-anki--extract-db (file)
  "Return path to SQLite database from Anki FILE.
If FILE is an .apkg (zip), extract the collection database to a
temp directory via 7z and return its path.  Modern .apkg files
contain a zstd-compressed collection.anki21b which is decompressed
via zstd or 7z.  If FILE is .anki2/.anki21, return it directly."
  (if (string-match-p "\\.apkg\\'" file)
      (let* ((tmpdir (make-temp-file "gnosis-anki-" t))
             (abs-file (expand-file-name file))
             (db-name nil)
             (7z (or (executable-find "7z")
                     (executable-find "7za")))
             (zstd (executable-find "zstd")))
        (unless 7z
          (delete-directory tmpdir t)
          (user-error "7z not found; install p7zip to import .apkg files"))
        ;; Try collection.anki21b (modern, zstd-compressed), then legacy formats
        (dolist (name '("collection.anki21b" "collection.anki21" "collection.anki2"))
          (when (and (null db-name)
                     (zerop (call-process 7z nil nil nil
                                          "e" abs-file
                                          (concat "-o" tmpdir)
                                          name "-y")))
            (let ((extracted (expand-file-name name tmpdir)))
              (when (file-exists-p extracted)
                (if (string-suffix-p ".anki21b" name)
                    ;; Decompress zstd -> SQLite
                    (let ((decompressed (expand-file-name "collection.anki21" tmpdir)))
                      (if (and zstd
                               (zerop (call-process zstd nil nil nil
                                                    "-d" extracted
                                                    "-o" decompressed)))
                          (progn
                            (delete-file extracted)
                            (setq db-name decompressed))
                        ;; Fallback: 7z can also decompress zstd
                        (let ((7z-out (expand-file-name "collection" tmpdir)))
                          (when (zerop (call-process 7z nil nil nil
                                                     "e" extracted
                                                     (concat "-o" tmpdir) "-y"))
                            (when (file-exists-p 7z-out)
                              (delete-file extracted)
                              (setq db-name 7z-out))))))
                  (setq db-name extracted))))))
        (unless db-name
          (delete-directory tmpdir t)
          (user-error "No collection database found in %s" file))
        db-name)
    file))

(defun gnosis-anki--parse-notes (anki-db model-info)
  "Parse all notes from ANKI-DB using MODEL-INFO.
Returns (SKIPPED . PREPARED) where PREPARED is a flat list of plists.
Each plist has keys :type :keimenon :hypothesis :answer
:parathema :tags.  Tag parsing is cached per unique tag string.
MODEL-INFO maps mid strings to (mtype . field-names)."
  (let ((notes (sqlite-select anki-db "SELECT id, mid, flds, tags FROM notes"))
        (seg-cache (make-hash-table :test 'equal :size 50000))
        (seen (make-hash-table :test 'equal :size 200))
        (result nil)
        (skipped 0))
    (dolist (note notes)
      (let* ((mid (number-to-string (nth 1 note)))
             (flds (nth 2 note))
             (tag-str (nth 3 note))
             (info (gethash mid model-info))
             (mtype (car info))
             (tmpl-count (cadr info)))
        (cond
         ;; Skip image occlusion (pre-marked in model-info)
         ((eq mtype 'skip)
          (cl-incf skipped))
         ;; Cloze note
         ((and mtype (= mtype 1))
          (let* ((fields (split-string flds "\x1f"))
                 (text (gnosis-anki--html-to-org (or (nth 0 fields) "")))
                 (extra (gnosis-anki--html-to-org (or (nth 1 fields) "")))
                 (_ (clrhash seen))
                 (tags (gnosis-anki--parse-tags tag-str seg-cache seen))
                 (contents (gnosis-cloze-extract-contents text))
                 (clozes (gnosis-cloze-extract-answers contents))
                 (hints (gnosis-cloze-extract-hints contents))
                 (keimenon (gnosis-cloze-remove-tags text)))
            (if clozes
                (cl-loop for cloze in clozes
                         for hint in hints
                         do (push (list :type "cloze" :keimenon keimenon
                                        :hypothesis hint
                                        :answer (mapcar #'gnosis-anki--strip-emphasis cloze)
                                        :parathema extra :tags tags)
                                  result))
              (cl-incf skipped))))
         ;; Basic note
         ((and mtype (= mtype 0))
          (let* ((fields (split-string flds "\x1f"))
                 (front (gnosis-anki--html-to-org (or (nth 0 fields) "")))
                 (back (gnosis-anki--html-to-org (or (nth 1 fields) "")))
                 (_ (clrhash seen))
                 (tags (gnosis-anki--parse-tags tag-str seg-cache seen)))
            (if (and (not (string-empty-p front))
                     (not (string-empty-p back)))
                (progn
                  (push (list :type "basic" :keimenon front
                              :hypothesis '("")
                              :answer (list (gnosis-anki--strip-emphasis back))
                              :parathema "" :tags tags)
                        result)
                  ;; Reversed card for 2-template basic notes (e.g. "Basic (and reversed card)")
                  (when (and tmpl-count (= tmpl-count 2))
                    (push (list :type "basic" :keimenon back
                                :hypothesis '("")
                                :answer (list (gnosis-anki--strip-emphasis front))
                                :parathema "" :tags tags)
                          result)))
              (cl-incf skipped))))
         (t (cl-incf skipped)))))
    (cons skipped (nreverse result))))

(defun gnosis-anki--bulk-insert-chunk (db items ids gnosis-val amnesia-val today)
  "Bulk-insert ITEMS into DB with pre-assigned IDS.
ITEMS is a list of plists from `gnosis-anki--parse-notes'.
IDS is a list of integer IDs, one per item.
GNOSIS-VAL, AMNESIA-VAL, TODAY are pre-computed constants."
  (let ((themata-params nil)
        (review-params nil)
        (review-log-params nil)
        (extras-params nil)
        (tag-params nil))
    ;; Build param lists
    (cl-loop for item in items
             for id in ids
             do (let ((type (plist-get item :type))
                      (keimenon (plist-get item :keimenon))
                      (hypothesis (plist-get item :hypothesis))
                      (answer (plist-get item :answer))
                      (parathema (plist-get item :parathema))
                      (tags (plist-get item :tags)))
                  ;; themata: id, type, keimenon, hypothesis, answer
                  ;; Strings must be prin1-encoded (emacsql compat)
                  (setq themata-params
                        (nconc themata-params
                               (list id (prin1-to-string type)
                                     (prin1-to-string keimenon)
                                     (prin1-to-string hypothesis)
                                     (prin1-to-string answer))))
                  ;; review: id, gnosis-val, amnesia-val
                  (setq review-params
                        (nconc review-params
                               (list id gnosis-val amnesia-val)))
                  ;; review_log: id, date, date, 0, 0, 0, 0, suspend, 0
                  (setq review-log-params
                        (nconc review-log-params
                               (list id today today 0 0 0 0 0 0)))
                  ;; extras: id, parathema, review-image
                  (setq extras-params
                        (nconc extras-params
                               (list id (prin1-to-string parathema)
                                     (prin1-to-string ""))))
                  ;; thema_tag: id, tag (variable per item)
                  (dolist (tag tags)
                    (setq tag-params
                          (nconc tag-params
                                 (list id (prin1-to-string tag)))))))
    (gnosis-sqlite-with-transaction db
        ;; INSERT INTO themata (5 of 6 cols, tags has DEFAULT)
        (sqlite-execute db
          (concat "INSERT INTO themata (id, type, keimenon, hypothesis, answer) VALUES "
                  (mapconcat (lambda (_) "(?,?,?,?,?)") items ", "))
          themata-params)
        ;; INSERT INTO review (3 cols)
        (sqlite-execute db
          (concat "INSERT INTO review VALUES "
                  (mapconcat (lambda (_) "(?,?,?)") items ", "))
          review-params)
        ;; INSERT INTO review_log (9 cols)
        (sqlite-execute db
          (concat "INSERT INTO review_log VALUES "
                  (mapconcat (lambda (_) "(?,?,?,?,?,?,?,?,?)") items ", "))
          review-log-params)
        ;; INSERT INTO extras (3 cols)
        (sqlite-execute db
          (concat "INSERT INTO extras VALUES "
                  (mapconcat (lambda (_) "(?,?,?)") items ", "))
          extras-params)
        ;; INSERT INTO thema_tag in batches (2 cols, variable count)
        (let* ((tag-count (/ (length tag-params) 2)))
          (when (> tag-count 0)
            (let ((batch-size 400)
                  (offset 0))
              (while (< offset tag-count)
                (let* ((end (min (+ offset batch-size) tag-count))
                       (batch-n (- end offset))
                       (param-start (* offset 2))
                       (param-end (* end 2))
                       (batch-params (cl-subseq tag-params param-start param-end)))
                  (sqlite-execute db
                    (concat "INSERT INTO thema_tag VALUES "
                            (mapconcat (lambda (_) "(?,?)")
                                       (number-sequence 1 batch-n) ", "))
                    batch-params)
                  (setq offset end)))))))))

(defun gnosis-anki--build-model-info-from-col (anki-db model-info)
  "Build MODEL-INFO from ANKI-DB using col.models JSON.
Deck exports (.apkg) store note types as JSON in the col table's
models column.  Each key is a model id mapping to an object with
type (0=basic, 1=cloze), flds (field list), and tmpls (templates).
Values are (mtype template-count . field-names)."
  (require 'json)
  (let* ((raw (caar (sqlite-select anki-db "SELECT models FROM col")))
         (models (json-parse-string raw :object-type 'alist)))
    (dolist (entry models)
      (let* ((model (cdr entry))
             (mid (number-to-string (alist-get 'id model)))
             (mtype (alist-get 'type model))
             (flds-arr (alist-get 'flds model))
             (tmpls-arr (alist-get 'tmpls model))
             (tmpl-count (length (append tmpls-arr nil)))
             (fields (mapcar (lambda (f) (alist-get 'name f))
                             (append flds-arr nil)))
             (mtype-resolved
              (cond
               ((cl-find-if (lambda (f)
                              (string-match-p "Image\\|SVG\\|Mask" f))
                            fields)
                'skip)
               ((= mtype 1) 1)
               (t 0))))
        (puthash mid (cons mtype-resolved (cons tmpl-count fields)) model-info)))))

(defun gnosis-anki--build-model-info-from-tables (anki-db model-info)
  "Build MODEL-INFO from ANKI-DB using notetypes/fields/templates tables.
Collection databases (.anki2) store note types in separate tables.
Values are (mtype template-count . field-names)."
  (let ((notetypes (sqlite-select anki-db "SELECT id, name FROM notetypes"))
        (cloze-ids (mapcar #'car
                    (sqlite-select anki-db
                      "SELECT DISTINCT ntid FROM templates WHERE name COLLATE NOCASE = 'Cloze'"))))
    (dolist (nt notetypes)
      (let* ((ntid (car nt))
             (mid (number-to-string ntid))
             (fields (mapcar #'cadr
                      (sqlite-select anki-db
                        "SELECT ord, name FROM fields WHERE ntid = ? ORDER BY ord"
                        (list ntid))))
             (tmpl-count (caar (sqlite-select anki-db
                                 "SELECT count(*) FROM templates WHERE ntid = ?"
                                 (list ntid))))
             (mtype (cond
                     ((cl-find-if (lambda (f)
                                    (string-match-p "Image\\|SVG\\|Mask" f))
                                  fields)
                      'skip)
                     ((member ntid cloze-ids) 1)
                     (t 0))))
        (puthash mid (cons mtype (cons tmpl-count fields)) model-info)))))

(defun gnosis-anki--build-model-info (anki-db)
  "Build model-info hash table from ANKI-DB.
Detects whether this is a collection (.anki2, has notetypes table)
or a deck export (.apkg, uses col.models JSON) and dispatches accordingly."
  (let ((model-info (make-hash-table :test 'equal))
        (has-notetypes (sqlite-select anki-db
                         "SELECT 1 FROM sqlite_master WHERE type='table' AND name='notetypes'")))
    (if has-notetypes
        (gnosis-anki--build-model-info-from-tables anki-db model-info)
      (gnosis-anki--build-model-info-from-col anki-db model-info))
    model-info))

(defun gnosis-anki--cleanup-temp (db-file)
  "Delete DB-FILE and its parent temp directory."
  (let ((dir (file-name-directory db-file)))
    (delete-directory dir t)))

(defun gnosis-anki--import-db (db-file &optional tmp-p)
  "Import notes from Anki database at DB-FILE asynchronously.
Parses all notes, then bulk-inserts in chunks of 200 using timers
so Emacs stays responsive.  When TMP-P is non-nil, clean up
DB-FILE and its temp directory after import."
  (let* ((anki-db (sqlite-open db-file))
         (model-info (gnosis-anki--build-model-info anki-db))
         (db (gnosis--ensure-db))
         (id-cache (make-hash-table :test 'equal))
         (chunk-size 200))
    ;; Parse all notes (synchronous, pure)
    (message "Parsing Anki notes...")
    (let* ((parse-result (gnosis-anki--parse-notes anki-db model-info))
           (skipped (car parse-result))
           (prepared (cdr parse-result))
           (total (length prepared)))
      (sqlite-close anki-db)
      (if (zerop total)
          (progn
            (when tmp-p (gnosis-anki--cleanup-temp db-file))
            (message "Anki import: 0 imported, %d skipped" skipped))
        ;; Populate id cache
        (dolist (id (gnosis-select 'id 'themata nil t))
          (puthash id t id-cache))
        ;; Pre-generate all IDs
        (message "Generating %d IDs..." total)
        (let* ((gnosis--id-cache id-cache)
               (all-ids (gnosis-generate-ids total))
               ;; Pre-compute constants
               (gnosis-val (prin1-to-string gnosis-algorithm-gnosis-value))
               (amnesia-val gnosis-algorithm-amnesia-value)
               (today (gnosis--today-int))
               ;; Split into chunks
               (item-chunks (seq-partition prepared chunk-size))
               (id-chunks (seq-partition all-ids chunk-size))
               (imported 0))
          ;; Async chunked insertion
          (cl-labels
              ((process-next (item-rest id-rest chunk-n)
                 (if (null item-rest)
                     (progn
                       (when tmp-p (gnosis-anki--cleanup-temp db-file))
                       (message "Anki import complete: %d imported, %d skipped"
                                imported skipped))
                   (let ((chunk-items (car item-rest))
                         (chunk-ids (car id-rest)))
                     (gnosis-anki--bulk-insert-chunk
                      db chunk-items chunk-ids gnosis-val amnesia-val today)
                     (setq imported (+ imported (length chunk-items)))
                     (message "Importing... %d/%d (%d%%)"
                              imported total
                              (/ (* 100 imported) total))
                     (run-with-timer 0.1 nil
                                     #'process-next
                                     (cdr item-rest) (cdr id-rest)
                                     (1+ chunk-n))))))
            (process-next item-chunks id-chunks 1)))))))


(defun gnosis-import-anki-collection (file)
  "Import Anki collection database FILE (.anki2) into gnosis."
  (interactive "fAnki collection (.anki2): ")
  (unless (file-exists-p file)
    (user-error "File not found: %s" file))
  (gnosis-anki--import-db file))

(defun gnosis-import-anki-deck (file)
  "Import Anki deck FILE (.apkg) into gnosis."
  (interactive "fAnki deck (.apkg): ")
  (unless (file-exists-p file)
    (user-error "File not found: %s" file))
  (let ((db-file (gnosis-anki--extract-db file)))
    (gnosis-anki--import-db db-file (not (string= db-file file)))))

(defun gnosis-import-anki (file)
  "Import Anki FILE into gnosis.
Detects format by extension: .apkg as deck, .anki2/.anki21 as collection."
  (interactive "fAnki file (.apkg, .anki2): ")
  (unless (file-exists-p file)
    (user-error "File not found: %s" file))
  (cond
   ((string-match-p "\\.apkg\\'" file)
    (gnosis-import-anki-deck file))
   ((string-match-p "\\.anki2[1]?\\'" file)
    (gnosis-import-anki-collection file))
   (t (user-error "Unsupported file type: %s" file))))

(provide 'gnosis-anki)
;;; gnosis-anki.el ends here
