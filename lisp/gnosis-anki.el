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

;; Import Anki deck packages (.apkg) or collections (.anki2/.anki21).
;;
;; Supports basic and cloze note types.  Converts HTML markup to
;; org-mode equivalents (bold, italic, underline, sub/superscript,
;; links).  Splits Anki hierarchical tags into individual gnosis tags.
;; Skips image occlusion notes.

;;; Code:

(require 'gnosis)
(require 'gnosis-db)
(require 'gnosis-scheduler)
(require 'gnosis-cloze)
(require 'gnosis-vc)
(require 'seq)
(require 'ucs-normalize)

(defconst gnosis-anki--chunk-size 200
  "Number of themata per async insert chunk.")

(defconst gnosis-anki--tag-batch-size 400
  "Number of tag rows per INSERT batch within a chunk.")

(defun gnosis-anki--html-to-org (str &optional no-emphasis)
  "Convert HTML markup in STR to Org mode equivalents.
Converts bold, italic, underline, sub/superscript and links.
With NO-EMPHASIS, omit bold, italic and underline wrappers for typed
answers, preserving literal punctuation from STR.
Strips remaining HTML tags.  Collapses excessive blank lines.
Short-circuits when STR contains no HTML markup or entities."
  (if (not (or (string-match-p "[<&]" str)
               (string-match-p "\n\\{3,\\}" str)))
      (string-trim str)
    (string-trim
     (seq-reduce
      (lambda (text rule)
        (replace-regexp-in-string (car rule) (cdr rule) text))
      `(("<b>\\(.*?\\)</b>" . ,(if no-emphasis "\\1" "*\\1*"))
        ("<i>\\(.*?\\)</i>" . ,(if no-emphasis "\\1" "/\\1/"))
        ("<u>\\(.*?\\)</u>" . ,(if no-emphasis "\\1" "_\\1_"))
        ("<sub>\\(.*?\\)</sub>" . "_{\\1}")
        ("<sup>\\(.*?\\)</sup>" . "^{\\1}")
        ("<a [^>]*href=\"\\([^\"]+\\)\"[^>]*>\\(.*?\\)</a>" . "[[\\1][\\2]]")
        ("<br\\(?:[[:space:]][^>]*\\)?/?>" . "\n")
        ("</?\\(?:div\\|p\\|li\\|tr\\|h[1-6]\\)\\(?:[[:space:]][^>]*\\)?>" . "\n")
        ("<[^>]+>" . "")
        ("&nbsp;" . " ")
        ("&amp;" . "&")
        ("&lt;" . "<")
        ("&gt;" . ">")
        ("&quot;" . "\"")
        ("\\[sound:[^]]*\\]" . "")
        ("\n\\{3,\\}" . "\n\n"))
      str))))

(defun gnosis-anki--sanitize-segment (seg)
  "Sanitize a single tag SEG for Org mode.
NFC-normalizes, replaces dashes with underscores, removes
non-alphanumeric characters (keeping underscore and @), collapses
repeated underscores, and trims leading/trailing underscores.
Returns nil for empty results."
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
    (while (string-match
            "{{\\(?:type:\\|cloze:\\)?\\([^}#/!]+\\)}}"
            template pos)
      (let ((field (match-string 1 template)))
        (unless (or (member field fields)
                    (string= field "FrontSide")
                    (string= field "Deck")
                    (string= field "Tags"))
          (push field fields)))
      (setq pos (match-end 0)))
    (nreverse fields)))

(defun gnosis-anki--front-back-fields (qfmt afmt fields &optional cloze-p)
  "Return front/back field names for decoded QFMT, AFMT and ordered FIELDS.
Remove front references from the back.  When either side has no fields,
fall back to the first field as front and the remaining fields as back.
With CLOZE-P, retain explicit front fields even without back-only fields."
  (let* ((front (gnosis-anki--template-fields (or qfmt "")))
         (back (cl-remove-if (lambda (field) (member field front))
                             (gnosis-anki--template-fields (or afmt "")))))
    (if (and front (or back cloze-p)) (cons front back)
      (cons (list (car fields)) (cdr fields)))))

(defun gnosis-anki--media-value-p (value)
  "Return non-nil when VALUE consists only of media references, not text."
  (and (not (string-empty-p value))
       (string-match-p
        "\\`\\s-*\\(\\[sound:[^]]*\\]\\|<img [^>]*>\\)\\s-*\\'"
        value)))

(defun gnosis-anki--resolve-field-values
    (field-names-to-get all-field-names raw-fields &optional no-emphasis)
  "Get field values for FIELD-NAMES-TO-GET from RAW-FIELDS.
ALL-FIELD-NAMES is the ordered list of all field names.
Skip absent, blank source fields and pure media (sound/image).
Return a list of Org mode text strings, retaining empty conversions so
later fields cannot replace a selected answer.  Pass NO-EMPHASIS to
`gnosis-anki--html-to-org' for typed answers without changing which
source fields supply the text."
  (let (texts)
    (dolist (target field-names-to-get)
      (let ((idx (cl-position target all-field-names :test #'string=)))
        (when idx
          (let ((val (or (nth idx raw-fields) "")))
            (unless (or (string-empty-p (string-trim val))
                        (gnosis-anki--media-value-p val))
              (push (gnosis-anki--html-to-org val no-emphasis)
                    texts))))))
    (nreverse texts)))

(defun gnosis-anki--image-occlusion-p (name fields)
  "Return non-nil if note type NAME with FIELDS is an image occlusion.
Checks for \"Image Occlusion\" or \"IO\" prefix in the note type name,
or for IO-specific fields (InSVG, OutSVG, Original Mask, I0)."
  (or (string-match-p "\\(?:Image Occlusion\\|\\`IO\\)" name)
      (cl-find-if
       (lambda (f)
         (string-match-p
          "\\`\\(InSVG\\|OutSVG\\|Original Mask\\|Mask\\|I0\\)\\'"
          f))
       fields)))

(defun gnosis-anki--extract-db (file)
  "Extract SQLite database from Anki .apkg FILE.
Extracts the collection database to a temp directory via 7z.
Modern .apkg files contain a zstd-compressed collection.anki21b
which is decompressed via zstd or 7z.
Return the database filename; the caller owns its temporary directory.
Remove that directory on error or quit before returning successfully."
  (let ((tmpdir (make-temp-file "gnosis-anki-" t))
        transferred)
    (unwind-protect
        (let* ((abs-file (expand-file-name file))
               (db-name nil)
               (7z (or (executable-find "7z")
                       (executable-find "7za")))
               (zstd (executable-find "zstd")))
          (unless 7z
            (user-error "7z not found; install p7zip to import .apkg files"))
          ;; Try the modern, zstd-compressed database, then legacy formats.
          (dolist (name '("collection.anki21b"
                          "collection.anki21"
                          "collection.anki2"))
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
            (user-error "No collection database found in %s" file))
          (prog1 db-name
            (setq transferred t)))
      (unless transferred
        (delete-directory tmpdir t)))))

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

(defun gnosis-anki--usable-answer-p (answer)
  "Return t for a converted ANSWER with non-whitespace text."
  (and (stringp answer) (not (string-empty-p (string-trim answer)))))

(defun gnosis-anki--parse-cloze-note (flds tag-str seg-cache seen &optional model-info)
  "Parse a cloze note from FLDS string with TAG-STR.
SEG-CACHE and SEEN are shared tag-parsing caches.
MODEL-INFO is this note type's entry from `gnosis-anki--build-model-info';
when omitted, assume the standard Text, Extra layout.
Return one plist per usable cloze group and nil per rejected group.
Return nil if there are no groups.  Keep rejected slots for skip accounting;
never remove individual answers and shift the remaining hints."
  (let* ((info (or model-info '(1 1 ("Text") ("Extra") "Text" "Extra")))
         (fields (split-string flds "\x1f"))
         (names (nthcdr 4 info))
         ;; Extract all source members before HTML conversion and validation.
         (text (mapconcat
                (lambda (name)
                  (let ((index (cl-position name names :test #'equal)))
                    (if index (or (nth index fields) "") "")))
                (nth 2 info) "\n"))
         (extra (mapconcat #'identity
                           (gnosis-anki--resolve-field-values (nth 3 info) names fields)
                           "\n"))
         (_ (clrhash seen))
         (tags (gnosis-anki--parse-tags tag-str seg-cache seen))
         (contents (gnosis-cloze-extract-contents text))
         (clozes (gnosis-cloze-extract-answers contents))
         (hints (gnosis-cloze-extract-hints contents))
         (keimenon (gnosis-anki--html-to-org (gnosis-cloze-remove-tags text))))
    (when clozes
      (cl-loop for cloze in clozes
               for hint in hints
               for answers = (mapcar
                              (lambda (answer)
                                (gnosis-anki--html-to-org answer t))
                              cloze)
               collect (when (and answers
                                  (seq-every-p #'gnosis-anki--usable-answer-p answers))
                         (list :type "cloze"
                               :keimenon keimenon
                               :hypothesis (mapcar
                                            (lambda (text)
                                              (and text (gnosis-anki--html-to-org text)))
                                            hint)
                               :answer answers
                               :parathema extra
                               :tags tags))))))

(defun gnosis-anki--parse-basic-note (flds front-field-names back-field-names
                                           all-field-names tag-str tmpl-count
                                           seg-cache seen)
  "Parse a basic note from FLDS string.
FRONT-FIELD-NAMES, BACK-FIELD-NAMES, ALL-FIELD-NAMES control
field extraction.  TAG-STR is the raw tag string.  TMPL-COUNT
triggers reversed cards when 2.  SEG-CACHE and SEEN are shared
tag-parsing caches.
Return a list of plists (1 or 2 items), or nil if the note is skipped.
Retain a nil slot for an unusable reverse answer, for skip accounting."
  (let* ((raw-fields (split-string flds "\x1f"))
         (front-texts (gnosis-anki--resolve-field-values
                       front-field-names all-field-names raw-fields))
         (back-texts (gnosis-anki--resolve-field-values
                      back-field-names all-field-names raw-fields))
         (front (mapconcat #'identity front-texts "\n"))
         (back (car back-texts))
         (answer (car (gnosis-anki--resolve-field-values
                       back-field-names all-field-names raw-fields t)))
         (extra (mapconcat #'identity (cdr back-texts) "\n"))
         (_ (clrhash seen))
         (tags (gnosis-anki--parse-tags tag-str seg-cache seen)))
    (when (and (not (string-empty-p front))
               (gnosis-anki--usable-answer-p answer))
      (let ((items (list (list :type "basic"
                               :keimenon front
                               :hypothesis '("")
                               :answer (list answer)
                               :parathema extra
                               :tags tags))))
        (when (and tmpl-count (= tmpl-count 2))
          (let ((reverse-answer
                 (mapconcat
                  #'identity
                  (gnosis-anki--resolve-field-values
                   front-field-names all-field-names raw-fields t)
                  "\n")))
            (push (when (gnosis-anki--usable-answer-p reverse-answer)
                    (list :type "basic"
                          :keimenon back
                          :hypothesis '("")
                          :answer (list reverse-answer)
                          :parathema extra
                          :tags tags))
                  items)))
        items))))

(defun gnosis-anki--parse-notes (anki-db model-info)
  "Parse all notes from ANKI-DB using MODEL-INFO.
Returns (SKIPPED . PREPARED) where PREPARED is a flat list of plists.
SKIPPED counts rejected generated items, or one for a note with no items.
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
                (gnosis-anki--parse-cloze-note flds tag-str seg-cache seen info))
               ((and mtype (= mtype 0))
                (gnosis-anki--parse-basic-note
                 flds (nth 2 info) (nth 3 info) (nthcdr 4 info)
                 tag-str (nth 1 info) seg-cache seen)))))
        (if items
            (dolist (item items)
              (if item
                  (push (plist-put item :guid guid) result)
                (cl-incf skipped)))
          (cl-incf skipped))))
    (cons skipped (nreverse result))))

(defun gnosis-anki--insert-tags (db tag-params)
  "Bulk-insert TAG-PARAMS into thema_tag using DB.
TAG-PARAMS is a flat list of id,tag pairs.
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

(defun gnosis-anki--bulk-insert-chunk
    (db items ids today
	&optional extra-tag suspend)
  "Bulk-insert ITEMS into DB with pre-assigned IDS.
ITEMS is a list of plists from `gnosis-anki--parse-notes'.
IDS is a list of integer IDs, one per item.
TODAY is the captured logical review day.
EXTRA-TAG, when non-nil, is appended to each item's tags.
SUSPEND, when non-nil, imports themata as suspended."
  (let ((themata-params nil)
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
                  ;; themata: id, type, keimenon, hypothesis,
                  ;; answer, source_guid
                  ;; Lisp fields retain text nil; source_guid stays raw.
                  (setq themata-params
                        (nconc themata-params
                               (list id (gnosis-sqlite--serialize type)
                                     (gnosis-sqlite--serialize keimenon)
                                     (gnosis-sqlite--serialize hypothesis)
                                     (gnosis-sqlite--serialize answer)
                                     guid)))
                  ;; extras: id, parathema, review-image
                  (setq extras-params
                        (nconc extras-params
                               (list id (gnosis-sqlite--serialize parathema)
                                     (gnosis-sqlite--serialize ""))))
                  ;; thema_tag: id, tag (variable per item)
                  (dolist (tag tags)
                    (setq tag-params
                          (nconc tag-params
                                 (list id (gnosis-sqlite--serialize tag)))))))
    (gnosis-sqlite-with-transaction db
      (sqlite-execute db
		      (concat "INSERT INTO themata (id, type, keimenon, hypothesis, answer, source_guid) VALUES "
			      (mapconcat (lambda (_) "(?,?,?,?,?,?)") items ", "))
		      themata-params)
      (gnosis-scheduler-initialize-themata
       (mapcar (lambda (id) (list id today suspend-val)) ids) db)
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
             (tmpl (and (> tmpl-count 0) (elt tmpls-arr 0)))
             (fb (gnosis-anki--front-back-fields
                  (alist-get 'qfmt tmpl) (alist-get 'afmt tmpl) fields (eql mtype 1)))
             (mtype-resolved
              (cond
               ((gnosis-anki--image-occlusion-p name fields) 'skip)
               ((= mtype 1) 1)
               (t 0))))
        (puthash mid (cl-list* mtype-resolved tmpl-count
                               (car fb) (cdr fb) fields)
                 model-info)))))

(defun gnosis-anki--notetype-kind (config)
  "Return the note kind encoded in protobuf CONFIG.
Read Notetype.Config.kind (field 1): 0 means basic and 1 means cloze.
An omitted kind defaults to basic.  Skip unrelated protobuf fields;
reject malformed input and unsupported kinds instead of guessing."
  (let ((bytes (encode-coding-string config 'raw-text))
        (pos 0)
        (kind 0))
    (while (< pos (length bytes))
      (let* ((tag (gnosis-anki--decode-varint bytes pos))
             (field (ash (car tag) -3))
             (wire (logand (car tag) 7)))
        (setq pos (cdr tag))
        (when (or (zerop field) (and (= field 1) (/= wire 0)))
          (user-error "Invalid Anki note type field"))
        (pcase wire
          (0 (let ((value (gnosis-anki--decode-varint bytes pos)))
               (when (= field 1) (setq kind (car value)))
               (setq pos (cdr value))))
          (1 (setq pos (+ pos 8)))
          (2 (let ((size (gnosis-anki--decode-varint bytes pos)))
               (setq pos (+ (cdr size) (car size)))))
          (5 (setq pos (+ pos 4)))
          (_ (user-error "Unsupported Anki note type wire format: %s" wire)))
        (when (> pos (length bytes))
          (user-error "Truncated Anki note type configuration"))))
    (unless (memq kind '(0 1))
      (user-error "Unsupported Anki note type kind: %s" kind))
    kind))

(defun gnosis-anki--build-model-info-from-tables (anki-db model-info)
  "Build MODEL-INFO from ANKI-DB using notetypes/fields/templates tables.
Modern .apkg files store note types in separate tables with protobuf
config blobs.  Template qfmt/afmt are decoded from the config blob.
Uses 4 total queries instead of 3N+2.  Read the semantic note kind from
notetypes.config, never from a template's display name.
Values are (mtype tmpl-count front-fields back-fields . all-fields)."
  (let ((notetypes (sqlite-select anki-db "SELECT id, name, config FROM notetypes"))
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
             (mtype (if (gnosis-anki--image-occlusion-p name fields)
                        'skip
                      (gnosis-anki--notetype-kind (nth 2 nt))))
             (fb (gnosis-anki--front-back-fields
                  (car decoded) (cdr decoded) fields (eql mtype 1))))
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
  (gnosis-vc--auto-commit
   (format "Anki import: %d themata from %s"
           count (file-name-nondirectory source-file))))

(defun gnosis-anki--import-chunks (items size)
  "Partition ITEMS into chunks of about SIZE without splitting source notes.
Group by :guid in first-appearance order.  Items without GUIDs are
independent.  A note larger than SIZE occupies one chunk, whose SQL
writes must be bounded separately inside a single transaction."
  (let* ((by-guid (make-hash-table :test #'equal))
         (keys (cl-loop for item in items for index from 0
                        for key = (or (plist-get item :guid) index)
                        unless (gethash key by-guid) collect key
                        do (push item (gethash key by-guid))))
         (groups (mapcar (lambda (key) (nreverse (gethash key by-guid))) keys)))
    (let ((count 0) chunk chunks)
      (dolist (group groups)
        (when (and chunk (> (+ count (length group)) size))
          (push (apply #'append (nreverse chunk)) chunks)
          (setq count 0 chunk nil))
        (push group chunk)
        (setq count (+ count (length group))))
      (when chunk (push (apply #'append (nreverse chunk)) chunks))
      (nreverse chunks))))

(defun gnosis-anki--insert-pending-chunk (db items ids today extra-tag suspend)
  "Insert complete source notes from ITEMS into DB unless already present.
Recheck GUIDs under the write transaction before inserting any siblings.
ITEMS and corresponding IDS form one logical chunk; bound SQL statements
separately.  Pass TODAY, EXTRA-TAG and SUSPEND to the bulk writer.
Return the number of themata committed."
  (gnosis-sqlite-with-transaction db
    (let ((present (make-hash-table :test #'equal))
          (guids (delete-dups (delq nil (mapcar (lambda (item)
                                                (plist-get item :guid)) items)))))
      ;; source_guid is raw text, not a serialized Lisp string.
      (dolist (batch (seq-partition guids gnosis-anki--chunk-size))
        (dolist (row (sqlite-select
                     db (concat "SELECT DISTINCT source_guid FROM themata
                                 WHERE source_guid IN ("
                                (mapconcat (lambda (_) "?") batch ",") ")")
                     batch))
          (puthash (car row) t present)))
      (let ((pending (cl-loop for item in items for id in ids
                              unless (gethash (plist-get item :guid) present)
                              collect (cons item id))))
        (dolist (batch (seq-partition pending gnosis-anki--chunk-size))
          (gnosis-anki--bulk-insert-chunk
           db (mapcar #'car batch) (mapcar #'cdr batch) today extra-tag suspend))
        (length pending)))))

(defun gnosis-anki--chunk-insert (db item-chunks id-chunks total skipped
                                   today source-file &optional extra-tag suspend)
  "Insert ITEM-CHUNKS with ID-CHUNKS into DB asynchronously.
Each chunk commits atomically; SQL writes stay bounded even for a
large source note.  TOTAL and SKIPPED are progress counts.  TODAY is
the captured logical review day.  SOURCE-FILE identifies the import commit.
Pass EXTRA-TAG and SUSPEND to `gnosis-anki--bulk-insert-chunk'.
Capture the repository beside DB before yielding, independently of
`gnosis-dir'.  Skip automatic Git for a database not named gnosis.db."
  (let* ((imported 0)
         ;; Metadata is optional for Git; report a closed DB through the writer.
         (file (condition-case nil
                   (nth 2 (seq-find
                           (lambda (row) (equal (nth 1 row) "main"))
                           (sqlite-select db "PRAGMA database_list")))
                 (error nil)))
         (directory (and (stringp file) (file-name-absolute-p file)
                         (equal (file-name-nondirectory file) "gnosis.db")
                         (file-name-directory file))))
    (cl-labels
        ((process-next (item-rest id-rest)
           (condition-case err
               (if (null item-rest)
                   (progn
                     (sqlite-execute db "ANALYZE")
                     (if directory
                         (let ((gnosis-dir directory))
                           (gnosis-anki--commit-import imported source-file))
                       (message "Anki import: Git skipped; database is not gnosis.db"))
                     (message "Anki import complete: %d imported, %d skipped"
                              imported skipped))
                 (let* ((count (gnosis-anki--insert-pending-chunk
                                db (car item-rest) (car id-rest)
                                today extra-tag suspend))
                        (duplicates (- (length (car item-rest)) count)))
                   (setq imported (+ imported count)
                         skipped (+ skipped duplicates)
                         total (- total duplicates)))
                 (message "Importing... %d/%d (%d%%)"
                          imported total
                          (if (zerop total) 100 (/ (* 100 imported) total)))
                 (run-with-timer 0.1 nil #'process-next
                                 (cdr item-rest) (cdr id-rest)))
             (error
              (message "Anki import error after %d themata: %S" imported err)))))
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

(defun gnosis-anki--import-db
    (db-file &optional tmp-p extra-tag suspend source-file validate)
  "Import notes from Anki database at DB-FILE asynchronously.
Parses all notes, then bulk-inserts in chunks using timers so
Emacs stays responsive.  When TMP-P is non-nil, clean up DB-FILE
and its temp directory after parsing, including on failure.
EXTRA-TAG is appended to each thema's tags.  SUSPEND imports as
suspended.  SOURCE-FILE is the original import path for the Git commit message.
Call optional VALIDATE after parsing and before starting writes."
  (let* ((parse-result (unwind-protect
                           (gnosis-anki--parse-anki-db db-file)
                         ;; Prepared notes own all data needed by the timers.
                         (when tmp-p (gnosis-anki--cleanup-temp db-file))))
         (skipped (car parse-result))
         (prepared (cdr parse-result))
         ;; Filter out duplicates by GUID
         (guid-cache (progn
                       (when validate (funcall validate))
                       (gnosis-anki--build-guid-cache)))
         (deduped (cl-remove-if
                   (lambda (item)
                     (let ((guid (plist-get item :guid)))
                       (and guid (gethash guid guid-cache))))
                   prepared))
         (dup-count (- (length prepared) (length deduped)))
         (skipped (+ skipped dup-count))
         (prepared deduped)
         (total (length prepared)))
    (when (> dup-count 0)
      (message "Skipping %d duplicate notes (already imported)" dup-count))
    (if (zerop total)
        (message "Anki import: 0 imported, %d skipped" skipped)
      (let* ((gnosis--id-cache (gnosis-anki--build-id-cache))
             (chunks (gnosis-anki--import-chunks prepared gnosis-anki--chunk-size))
             (id-chunks (mapcar (lambda (chunk)
                                  (gnosis-generate-ids (length chunk)))
                                chunks)))
        (when validate (funcall validate))
        (gnosis-anki--chunk-insert
         (gnosis--ensure-db) chunks id-chunks total skipped
         (gnosis--today-int)
         (or source-file db-file)
         extra-tag suspend)))))

;;;###autoload
(defun gnosis-import-anki (file)
  "Import Anki FILE (.apkg, .anki2 or .anki21) into gnosis.
Direct collection files must be quiescent standalone SQLite databases.
Prompt for FILE when nil.  Refuse destination changes during input or
preparation; cancelled input does not open an uninitialized destination."
  (interactive (list nil))
  (let* ((db gnosis-db)
         (directory (expand-file-name gnosis-dir))
         (validate
          (lambda ()
            (unless (and (eq db gnosis-db)
                         (or db (equal directory (expand-file-name gnosis-dir))))
              (user-error "Gnosis database changed; start the import again"))))
         (file (or file (read-file-name "Anki file (.apkg, .anki2, .anki21): "
                                        nil nil t))))
    (funcall validate)
    (unless (file-exists-p file)
      (user-error "File not found: %s" file))
    (unless (member (file-name-extension file) '("apkg" "anki2" "anki21"))
      (user-error "Unsupported file type: %s (use .apkg, .anki2 or .anki21)" file))
    (let* ((tag (prog1 (read-string "Tag for imported themata (empty to skip): ")
                  (funcall validate)))
           (suspend (prog1 (y-or-n-p "Import as suspended?")
                      (funcall validate)))
           (archive-p (equal (file-name-extension file) "apkg")))
      ;; Acquire a lazy destination only after all input has been accepted.
      (setq db (gnosis--ensure-db))
      (gnosis-anki--import-db (if archive-p (gnosis-anki--extract-db file) file)
                             archive-p (unless (string-empty-p tag) tag)
                             suspend file validate))))

(provide 'gnosis-anki)
;;; gnosis-anki.el ends here
