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

;; Import Anki deck packages (.apkg) into gnosis.
;;
;; Supports basic and cloze note types.  Converts HTML markup to
;; org-mode equivalents (bold, italic, underline, sub/superscript,
;; links).  Splits Anki hierarchical tags into individual gnosis tags.
;; Skips image occlusion notes.

;;; Code:

(require 'gnosis)
(require 'gnosis-algorithm)
(require 'seq)

(defconst gnosis-anki--chunk-size 200
  "Number of themata per async insert chunk.")

(defconst gnosis-anki--tag-batch-size 400
  "Number of tag rows per INSERT batch within a chunk.")

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
      ;; Strip Anki media references
      (setq s (replace-regexp-in-string "\\[sound:[^]]*\\]" "" s))
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
NFC-normalizes, replaces dashes with underscores, removes
non-alphanumeric characters (keeping underscore and @), collapses
repeated underscores, and trims leading/trailing underscores.
Returns nil for empty results."
  (require 'ucs-normalize)
  (let* ((s (ucs-normalize-NFC-string seg))
         (s (subst-char-in-string ?- ?_ s))
         (s (replace-regexp-in-string "[^[:alnum:]_@]" "" s))
         (s (replace-regexp-in-string "__+" "_" s))
         (s (string-trim s "_")))
    (unless (string-empty-p s) s)))

(defconst gnosis-anki--system-tags '("marked" "leech")
  "Anki system tags to exclude from import.")

(defun gnosis-anki--parse-tags (tag-string &optional seg-cache seen)
  "Parse Anki TAG-STRING into a flat list of unique tags.
Splits on :: and whitespace, sanitizes each segment via a
segment-level cache SEG-CACHE.  SEEN is a reusable hash table
for per-call dedup (caller should `clrhash' it between calls).
Strips Anki system tags (marked, leech)."
  (let ((seg-cache (or seg-cache (make-hash-table :test 'equal)))
        (seen (or seen (make-hash-table :test 'equal)))
        (tags nil))
    (dolist (seg (split-string tag-string "[ \t]+\\|::" t))
      (let ((tag (or (gethash seg seg-cache)
                     (puthash seg (or (gnosis-anki--sanitize-segment seg) "")
                              seg-cache))))
        (unless (or (string-empty-p tag)
                    (gethash tag seen)
                    (member (downcase tag) gnosis-anki--system-tags))
          (puthash tag t seen)
          (push tag tags))))
    (nreverse tags)))

(defun gnosis-anki--decode-varint (bytes pos)
  "Decode a protobuf varint from BYTES starting at POS.
Returns (VALUE . NEW-POS)."
  (let ((result 0) (shift 0) (b 0))
    (while (progn
             (setq b (aref bytes pos))
             (setq result (logior result (ash (logand b #x7f) shift)))
             (setq pos (1+ pos))
             (setq shift (+ shift 7))
             (/= (logand b #x80) 0)))
    (cons result pos)))

(defun gnosis-anki--decode-template-config (config)
  "Extract qfmt and afmt from protobuf CONFIG blob.
Returns (QFMT . AFMT) or nil if parsing fails.
Protobuf layout: field 1 (tag 0x0a) = qfmt, field 2 (tag 0x12) = afmt."
  (when (and config (> (length config) 2))
    (condition-case nil
        (let ((bytes (if (stringp config)
                         (encode-coding-string config 'raw-text)
                       config))
              qfmt afmt (pos 0))
          (while (< pos (length bytes))
            (let* ((tag (aref bytes pos))
                   (field-num (ash tag -3))
                   (wire-type (logand tag #x07)))
              (setq pos (1+ pos))
              (cond
               ;; Length-delimited fields (wire type 2)
               ((= wire-type 2)
                (let* ((vr (gnosis-anki--decode-varint bytes pos))
                       (len (car vr))
                       (start (cdr vr))
                       (val (decode-coding-string
                             (substring bytes start (+ start len))
                             'utf-8)))
                  (cond ((= field-num 1) (setq qfmt val))
                        ((= field-num 2) (setq afmt val)))
                  (setq pos (+ start len))))
               ;; Varint fields (wire type 0) - skip
               ((= wire-type 0)
                (let ((vr (gnosis-anki--decode-varint bytes pos)))
                  (setq pos (cdr vr))))
               ;; Unknown wire type - bail
               (t (setq pos (length bytes))))))
          (when (and qfmt afmt)
            (cons qfmt afmt)))
      (error nil))))

(defun gnosis-anki--template-fields (template)
  "Extract field names referenced in TEMPLATE string.
Parses {{FieldName}} and {{type:FieldName}} placeholders.
Returns a list of field name strings in order of first appearance."
  (let ((pos 0) fields)
    (while (string-match "{{\\(?:type:\\|cloze:\\)?\\([^}#/!]+\\)}}" template pos)
      (let ((field (match-string 1 template)))
        (unless (or (member field fields)
                    (string= field "FrontSide")
                    (string= field "Deck")
                    (string= field "Tags"))
          (push field fields)))
      (setq pos (match-end 0)))
    (nreverse fields)))

(defun gnosis-anki--front-back-from-templates (tmpls-arr fields)
  "Determine front and back field names from TMPLS-ARR.
FIELDS is the ordered list of all field names in the note type.
Returns (FRONT-FIELDS . BACK-FIELDS) where each is a list of field
names.  Uses the first template's qfmt/afmt.  Falls back to first
field as front, rest as back if templates are unavailable."
  (if (and tmpls-arr (> (length tmpls-arr) 0))
      (let* ((tmpl (if (vectorp tmpls-arr)
                       (aref tmpls-arr 0)
                     (car tmpls-arr)))
             (qfmt (or (alist-get 'qfmt tmpl) ""))
             (afmt (or (alist-get 'afmt tmpl) ""))
             (q-fields (gnosis-anki--template-fields qfmt))
             (a-fields (cl-remove-if (lambda (f) (member f q-fields))
                                     (gnosis-anki--template-fields afmt))))
        (if (and q-fields a-fields)
            (cons q-fields a-fields)
          ;; Templates don't reference known fields, fall back
          (cons (list (car fields)) (cdr fields))))
    (cons (list (car fields)) (cdr fields))))

(defun gnosis-anki--media-value-p (value)
  "Return non-nil if VALUE contains only media references, not text."
  (and (not (string-empty-p value))
       (string-match-p "\\`\\s-*\\(\\[sound:[^]]*\\]\\|<img [^>]*>\\)\\s-*\\'" value)))

(defun gnosis-anki--resolve-field-values (field-names-to-get all-field-names raw-fields)
  "Get field values for FIELD-NAMES-TO-GET from RAW-FIELDS.
ALL-FIELD-NAMES is the ordered list of all field names.
Skips fields whose values are pure media (sound/image).
Returns a list of org-mode text strings."
  (let (texts)
    (dolist (target field-names-to-get)
      (let ((idx (cl-position target all-field-names :test #'string=)))
        (when idx
          (let* ((val (or (nth idx raw-fields) ""))
                 (converted (gnosis-anki--html-to-org val)))
            (unless (or (string-empty-p converted)
                        (gnosis-anki--media-value-p val))
              (push converted texts))))))
    (nreverse texts)))

(defun gnosis-anki--image-occlusion-p (name fields)
  "Return non-nil if note type NAME with FIELDS is an image occlusion.
Checks for \"Image Occlusion\" or \"IO\" prefix in the note type name,
or for IO-specific fields (InSVG, OutSVG, Original Mask, I0)."
  (or (string-match-p "\\(?:Image Occlusion\\|\\`IO\\)" name)
      (cl-find-if (lambda (f)
                    (string-match-p "\\`\\(InSVG\\|OutSVG\\|Original Mask\\|Mask\\|I0\\)\\'" f))
                  fields)))

(defun gnosis-anki--extract-db (file)
  "Extract SQLite database from Anki .apkg FILE.
Extracts the collection database to a temp directory via 7z.
Modern .apkg files contain a zstd-compressed collection.anki21b
which is decompressed via zstd or 7z."
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
                (setq db-name
                      (gnosis-anki--decompress-zstd extracted tmpdir zstd 7z))
              (setq db-name extracted))))))
    (unless db-name
      (delete-directory tmpdir t)
      (user-error "No collection database found in %s" file))
    db-name))

(defun gnosis-anki--decompress-zstd (extracted tmpdir zstd 7z)
  "Decompress zstd-compressed EXTRACTED file in TMPDIR.
Try ZSTD first, fall back to 7Z.  Returns path to decompressed DB."
  (let ((decompressed (expand-file-name "collection.anki21" tmpdir)))
    (if (and zstd
             (zerop (call-process zstd nil nil nil
                                  "-d" extracted "-o" decompressed)))
        (progn
          (delete-file extracted)
          decompressed)
      ;; Fallback: 7z can also decompress zstd
      (let ((7z-out (expand-file-name "collection" tmpdir)))
        (when (zerop (call-process 7z nil nil nil
                                   "e" extracted
                                   (concat "-o" tmpdir) "-y"))
          (when (file-exists-p 7z-out)
            (delete-file extracted)
            7z-out))))))

(defun gnosis-anki--parse-cloze-note (flds tag-str seg-cache seen)
  "Parse a cloze note from FLDS string with TAG-STR.
SEG-CACHE and SEEN are shared tag-parsing caches.
Returns a list of plists (one per cloze deletion), or nil if skipped."
  (let* ((fields (split-string flds "\x1f"))
         (text (gnosis-anki--html-to-org (or (nth 0 fields) "")))
         (extra (gnosis-anki--html-to-org (or (nth 1 fields) "")))
         (_ (clrhash seen))
         (tags (gnosis-anki--parse-tags tag-str seg-cache seen))
         (contents (gnosis-cloze-extract-contents text))
         (clozes (gnosis-cloze-extract-answers contents))
         (hints (gnosis-cloze-extract-hints contents))
         (keimenon (gnosis-cloze-remove-tags text)))
    (when clozes
      (cl-loop for cloze in clozes
               for hint in hints
               collect (list :type "cloze" :keimenon keimenon
                             :hypothesis hint
                             :answer (mapcar #'gnosis-anki--strip-emphasis cloze)
                             :parathema extra :tags tags)))))

(defun gnosis-anki--parse-basic-note (flds front-field-names back-field-names
                                            all-field-names tag-str tmpl-count
                                            seg-cache seen)
  "Parse a basic note from FLDS string.
FRONT-FIELD-NAMES, BACK-FIELD-NAMES, ALL-FIELD-NAMES control
field extraction.  TAG-STR is the raw tag string.  TMPL-COUNT
triggers reversed cards when 2.  SEG-CACHE and SEEN are shared
tag-parsing caches.
Returns a list of plists (1 or 2 items), or nil if skipped."
  (let* ((raw-fields (split-string flds "\x1f"))
         (front-texts (gnosis-anki--resolve-field-values
                       front-field-names all-field-names raw-fields))
         (back-texts (gnosis-anki--resolve-field-values
                      back-field-names all-field-names raw-fields))
         (front (mapconcat #'identity front-texts "\n"))
         (back (car back-texts))
         (extra (mapconcat #'identity (cdr back-texts) "\n"))
         (_ (clrhash seen))
         (tags (gnosis-anki--parse-tags tag-str seg-cache seen)))
    (when (and (not (string-empty-p front))
               back (not (string-empty-p back)))
      (let ((items (list (list :type "basic" :keimenon front
                               :hypothesis '("")
                               :answer (list (gnosis-anki--strip-emphasis back))
                               :parathema extra :tags tags))))
        (when (and tmpl-count (= tmpl-count 2))
          (push (list :type "basic" :keimenon back
                      :hypothesis '("")
                      :answer (list (gnosis-anki--strip-emphasis front))
                      :parathema extra :tags tags)
                items))
        items))))

(defun gnosis-anki--parse-notes (anki-db model-info)
  "Parse all notes from ANKI-DB using MODEL-INFO.
Returns (SKIPPED . PREPARED) where PREPARED is a flat list of plists.
Each plist has keys :type :keimenon :hypothesis :answer
:parathema :tags :guid.  Tag parsing is cached per unique tag string.
MODEL-INFO maps mid strings to
  (mtype tmpl-count front-fields back-fields . all-fields)."
  (let ((notes (sqlite-select anki-db
                 "SELECT id, mid, flds, tags, guid FROM notes"))
        (seg-cache (make-hash-table :test 'equal :size 50000))
        (seen (make-hash-table :test 'equal :size 200))
        (result nil)
        (skipped 0))
    (dolist (note notes)
      (let* ((mid (number-to-string (nth 1 note)))
             (flds (nth 2 note))
             (tag-str (nth 3 note))
             (guid (nth 4 note))
             (info (gethash mid model-info))
             (mtype (nth 0 info))
             (items
              (cond
               ((eq mtype 'skip) nil)
               ((and mtype (= mtype 1))
                (gnosis-anki--parse-cloze-note flds tag-str seg-cache seen))
               ((and mtype (= mtype 0))
                (gnosis-anki--parse-basic-note
                 flds (nth 2 info) (nth 3 info) (nthcdr 4 info)
                 tag-str (nth 1 info) seg-cache seen)))))
        (if items
            (dolist (item items)
              (push (plist-put item :guid guid) result))
          (cl-incf skipped))))
    (cons skipped (nreverse result))))

(defun gnosis-anki--insert-tags (db tag-params)
  "Bulk-insert TAG-PARAMS (flat list of id,tag pairs) into thema_tag.
Batches inserts in groups of `gnosis-anki--tag-batch-size'."
  (let ((tag-count (/ (length tag-params) 2)))
    (when (> tag-count 0)
      (let ((offset 0))
        (while (< offset tag-count)
          (let* ((end (min (+ offset gnosis-anki--tag-batch-size) tag-count))
                 (batch-n (- end offset))
                 (batch-params (cl-subseq tag-params (* offset 2) (* end 2))))
            (sqlite-execute db
              (concat "INSERT INTO thema_tag VALUES "
                      (mapconcat (lambda (_) "(?,?)")
                                 (number-sequence 1 batch-n) ", "))
              batch-params)
            (setq offset end)))))))

(defun gnosis-anki--bulk-insert-chunk (db items ids gnosis-val amnesia-val today
                                         &optional extra-tag suspend)
  "Bulk-insert ITEMS into DB with pre-assigned IDS.
ITEMS is a list of plists from `gnosis-anki--parse-notes'.
IDS is a list of integer IDs, one per item.
GNOSIS-VAL, AMNESIA-VAL, TODAY are pre-computed constants.
EXTRA-TAG, when non-nil, is appended to each item's tags.
SUSPEND, when non-nil, imports themata as suspended."
  (let ((themata-params nil)
        (review-params nil)
        (review-log-params nil)
        (extras-params nil)
        (tag-params nil)
        (suspend-val (if suspend 1 0)))
    ;; Build param lists
    (cl-loop for item in items
             for id in ids
             do (let ((type (plist-get item :type))
                      (keimenon (plist-get item :keimenon))
                      (hypothesis (plist-get item :hypothesis))
                      (answer (plist-get item :answer))
                      (parathema (plist-get item :parathema))
                      (guid (plist-get item :guid))
                      (tags (let ((tl (plist-get item :tags)))
                              (if (and extra-tag (not (member extra-tag tl)))
                                  (append tl (list extra-tag))
                                tl))))
                  ;; themata: id, type, keimenon, hypothesis, answer, source_guid
                  ;; Strings must be prin1-encoded (emacsql compat)
                  (setq themata-params
                        (nconc themata-params
                               (list id (prin1-to-string type)
                                     (prin1-to-string keimenon)
                                     (prin1-to-string hypothesis)
                                     (prin1-to-string answer)
                                     guid)))
                  ;; review: id, gnosis-val, amnesia-val
                  (setq review-params
                        (nconc review-params
                               (list id gnosis-val amnesia-val)))
                  ;; review_log: id, date, date, 0, 0, 0, 0, suspend, 0
                  (setq review-log-params
                        (nconc review-log-params
                               (list id today today 0 0 0 0 suspend-val 0)))
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
        (sqlite-execute db
          (concat "INSERT INTO themata (id, type, keimenon, hypothesis, answer, source_guid) VALUES "
                  (mapconcat (lambda (_) "(?,?,?,?,?,?)") items ", "))
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
        (gnosis-anki--insert-tags db tag-params))))

(defun gnosis-anki--build-model-info-from-col (anki-db model-info)
  "Build MODEL-INFO from ANKI-DB using col.models JSON.
Deck exports (.apkg) store note types as JSON in the col table's
models column.  Each key is a model id mapping to an object with
type (0=basic, 1=cloze), flds (field list), and tmpls (templates).
Values are (mtype tmpl-count front-fields back-fields . all-fields)."
  (require 'json)
  (let* ((raw (caar (sqlite-select anki-db "SELECT models FROM col")))
         (models (json-parse-string raw :object-type 'alist)))
    (dolist (entry models)
      (let* ((model (cdr entry))
             (mid (number-to-string (alist-get 'id model)))
             (name (or (alist-get 'name model) ""))
             (mtype (alist-get 'type model))
             (flds-arr (alist-get 'flds model))
             (tmpls-arr (alist-get 'tmpls model))
             (tmpl-count (length (append tmpls-arr nil)))
             (fields (mapcar (lambda (f) (alist-get 'name f))
                             (append flds-arr nil)))
             (fb (gnosis-anki--front-back-from-templates tmpls-arr fields))
             (mtype-resolved
              (cond
               ((gnosis-anki--image-occlusion-p name fields) 'skip)
               ((= mtype 1) 1)
               (t 0))))
        (puthash mid (cl-list* mtype-resolved tmpl-count
                            (car fb) (cdr fb) fields)
                 model-info)))))

(defun gnosis-anki--build-model-info-from-tables (anki-db model-info)
  "Build MODEL-INFO from ANKI-DB using notetypes/fields/templates tables.
Modern .apkg files store note types in separate tables with protobuf
config blobs.  Template qfmt/afmt are decoded from the config blob.
Uses 5 total queries instead of 3N+2.
Values are (mtype tmpl-count front-fields back-fields . all-fields)."
  (let ((notetypes (sqlite-select anki-db "SELECT id, name FROM notetypes"))
        (cloze-ids (mapcar #'car
                    (sqlite-select anki-db
                      "SELECT DISTINCT ntid FROM templates WHERE name COLLATE NOCASE = 'Cloze'")))
        (all-fields (sqlite-select anki-db
                      "SELECT ntid, ord, name FROM fields ORDER BY ntid, ord"))
        (all-configs (sqlite-select anki-db
                       "SELECT ntid, config FROM templates ORDER BY ntid, ord"))
        (all-counts (sqlite-select anki-db
                      "SELECT ntid, count(*) FROM templates GROUP BY ntid"))
        (fields-ht (make-hash-table :test 'equal))
        (config-ht (make-hash-table :test 'equal))
        (count-ht (make-hash-table :test 'equal)))
    ;; Group fields by ntid
    (dolist (row all-fields)
      (let ((ntid (car row))
            (name (nth 2 row)))
        (push name (gethash ntid fields-ht))))
    ;; Store first config blob per ntid
    (dolist (row all-configs)
      (let ((ntid (car row)))
        (unless (gethash ntid config-ht)
          (puthash ntid (cadr row) config-ht))))
    ;; Store template counts
    (dolist (row all-counts)
      (puthash (car row) (cadr row) count-ht))
    ;; Build model-info from pre-fetched data
    (dolist (nt notetypes)
      (let* ((ntid (car nt))
             (name (or (cadr nt) ""))
             (mid (number-to-string ntid))
             (fields (nreverse (gethash ntid fields-ht)))
             (config-blob (gethash ntid config-ht))
             (tmpl-count (or (gethash ntid count-ht) 0))
             (decoded (gnosis-anki--decode-template-config config-blob))
             (fb (if decoded
                     (let* ((q-fields (gnosis-anki--template-fields (car decoded)))
                            (a-fields (cl-remove-if (lambda (f) (member f q-fields))
                                                    (gnosis-anki--template-fields (cdr decoded)))))
                       (if (and q-fields a-fields)
                           (cons q-fields a-fields)
                         (cons (list (car fields)) (cdr fields))))
                   (cons (list (car fields)) (cdr fields))))
             (mtype (cond
                     ((gnosis-anki--image-occlusion-p name fields) 'skip)
                     ((member ntid cloze-ids) 1)
                     (t 0))))
        (puthash mid (cl-list* mtype tmpl-count
                            (car fb) (cdr fb) fields)
                 model-info)))))

(defun gnosis-anki--build-model-info (anki-db)
  "Build model-info hash table from ANKI-DB.
Detects whether this .apkg uses the new format (notetypes/fields/
templates tables with protobuf config) or old format (col.models
JSON) and dispatches accordingly."
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

(defun gnosis-anki--parse-anki-db (db-file)
  "Open and parse all notes from Anki database at DB-FILE.
Returns (SKIPPED . PREPARED) where PREPARED is a list of plists."
  (message "Parsing Anki notes...")
  (let ((anki-db (sqlite-open db-file)))
    (unwind-protect
        (let ((model-info (gnosis-anki--build-model-info anki-db)))
          (gnosis-anki--parse-notes anki-db model-info))
      (sqlite-close anki-db))))

(defun gnosis-anki--build-id-cache ()
  "Build hash table of existing thema IDs for collision avoidance."
  (let ((cache (make-hash-table :test 'equal)))
    (dolist (id (gnosis-select 'id 'themata nil t))
      (puthash id t cache))
    cache))

(defun gnosis-anki--commit-import (count source-file)
  "Commit after importing COUNT themata from SOURCE-FILE."
  (unless gnosis-testing
    (gnosis--ensure-git-repo)
    (gnosis--git-chain
     `(("add" "gnosis.db")
       ("commit" "-m"
        ,(format "Anki import: %d themata from %s"
                 count (file-name-nondirectory source-file))))
     (lambda ()
       (when gnosis-vc-auto-push (gnosis-vc-push))))))

(defun gnosis-anki--chunk-insert (db item-chunks id-chunks total skipped
                                     gnosis-val amnesia-val today cleanup-fn
                                     source-file
                                     &optional extra-tag suspend)
  "Insert ITEM-CHUNKS with ID-CHUNKS into DB asynchronously.
TOTAL and SKIPPED are counts for progress messages.  GNOSIS-VAL,
AMNESIA-VAL, TODAY are pre-computed constants.  CLEANUP-FN is
called with no args after the last chunk.  SOURCE-FILE is the
original file path for the git commit message.  EXTRA-TAG and
SUSPEND are passed through to `gnosis-anki--bulk-insert-chunk'."
  (let ((imported 0))
    (cl-labels
        ((process-next (item-rest id-rest)
           (if (null item-rest)
               (progn
                 (sqlite-execute db "ANALYZE")
                 (gnosis-anki--commit-import imported source-file)
                 (when cleanup-fn (funcall cleanup-fn))
                 (message "Anki import complete: %d imported, %d skipped"
                          imported skipped))
             (condition-case err
                 (progn
                   (gnosis-anki--bulk-insert-chunk
                    db (car item-rest) (car id-rest)
                    gnosis-val amnesia-val today extra-tag suspend)
                   (setq imported (+ imported (length (car item-rest))))
                   (message "Importing... %d/%d (%d%%)"
                            imported total (/ (* 100 imported) total))
                   (run-with-timer 0.1 nil #'process-next
                                   (cdr item-rest) (cdr id-rest)))
               (error
                (message "Anki import error at chunk %d: %S" imported err)
                (when cleanup-fn (funcall cleanup-fn)))))))
      (process-next item-chunks id-chunks))))

(defun gnosis-anki--build-guid-cache ()
  "Build hash table of existing source GUIDs for duplicate detection.
Uses raw `sqlite-select' because source_guid is stored without
`prin1-to-string' encoding."
  (let ((cache (make-hash-table :test 'equal))
        (rows (sqlite-select (gnosis--ensure-db)
                "SELECT source_guid FROM themata WHERE source_guid IS NOT NULL")))
    (dolist (row rows)
      (puthash (car row) t cache))
    cache))

(defun gnosis-anki--import-db (db-file &optional tmp-p extra-tag suspend source-file)
  "Import notes from Anki database at DB-FILE asynchronously.
Parses all notes, then bulk-inserts in chunks using timers so
Emacs stays responsive.  When TMP-P is non-nil, clean up DB-FILE
and its temp directory after import.  EXTRA-TAG is appended to
each thema's tags.  SUSPEND imports as suspended.  SOURCE-FILE
is the original .apkg path for the git commit message."
  (let* ((parse-result (gnosis-anki--parse-anki-db db-file))
         (skipped (car parse-result))
         (prepared (cdr parse-result))
         ;; Filter out duplicates by GUID
         (guid-cache (gnosis-anki--build-guid-cache))
         (deduped (cl-remove-if
                   (lambda (item)
                     (let ((guid (plist-get item :guid)))
                       (and guid (gethash guid guid-cache))))
                   prepared))
         (dup-count (- (length prepared) (length deduped)))
         (skipped (+ skipped dup-count))
         (prepared deduped)
         (total (length prepared))
         (cleanup-fn (when tmp-p
                       (lambda () (gnosis-anki--cleanup-temp db-file)))))
    (when (> dup-count 0)
      (message "Skipping %d duplicate notes (already imported)" dup-count))
    (if (zerop total)
        (progn
          (when cleanup-fn (funcall cleanup-fn))
          (message "Anki import: 0 imported, %d skipped" skipped))
      (let* ((gnosis--id-cache (gnosis-anki--build-id-cache))
             (all-ids (progn (message "Generating %d IDs..." total)
                             (gnosis-generate-ids total))))
        (gnosis-anki--chunk-insert
         (gnosis--ensure-db)
         (seq-partition prepared gnosis-anki--chunk-size)
         (seq-partition all-ids gnosis-anki--chunk-size)
         total skipped
         (prin1-to-string gnosis-algorithm-gnosis-value)
         gnosis-algorithm-amnesia-value
         (gnosis--today-int)
         cleanup-fn
         (or source-file db-file)
         extra-tag suspend)))))

(defun gnosis-import-anki (file)
  "Import Anki deck package FILE (.apkg) into gnosis."
  (interactive "fAnki deck (.apkg): ")
  (unless (file-exists-p file)
    (user-error "File not found: %s" file))
  (unless (string-match-p "\\.apkg\\'" file)
    (user-error "Unsupported file type: %s (only .apkg supported)" file))
  (let ((extra-tag (let ((tag (read-string "Tag for imported themata (empty to skip): ")))
                     (if (string-empty-p tag) nil tag)))
        (suspend (y-or-n-p "Import as suspended?"))
        (db-file (gnosis-anki--extract-db file)))
    (gnosis-anki--import-db db-file t extra-tag suspend file)))

(provide 'gnosis-anki)
;;; gnosis-anki.el ends here
