;;; gnosis-test-anki.el --- Anki import tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for Anki import pipeline: parse-notes, bulk-insert-chunk,
;; and full import.  Creates Anki databases matching the real schema
;; (notetypes, fields, templates, notes with all columns) and
;; populates them with random data.

;;; Code:
(require 'ert)
(require 'gnosis)
(require 'gnosis-anki)

(load (expand-file-name "gnosis-test-helpers.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; ---- Schema creation (mirrors real Anki .anki2) ----

(defun gnosis-test-anki--create-schema (db)
  "Create Anki tables in DB matching the real .anki2 schema.
Uses NOCASE instead of Anki's custom unicase collation."
  (sqlite-execute db "
    CREATE TABLE notetypes (
      id integer NOT NULL PRIMARY KEY,
      name text NOT NULL COLLATE NOCASE,
      mtime_secs integer NOT NULL,
      usn integer NOT NULL,
      config blob NOT NULL)")
  (sqlite-execute db "
    CREATE TABLE fields (
      ntid integer NOT NULL,
      ord integer NOT NULL,
      name text NOT NULL COLLATE NOCASE,
      config blob NOT NULL,
      PRIMARY KEY (ntid, ord)) WITHOUT ROWID")
  (sqlite-execute db "
    CREATE TABLE templates (
      ntid integer NOT NULL,
      ord integer NOT NULL,
      name text NOT NULL COLLATE NOCASE,
      mtime_secs integer NOT NULL,
      usn integer NOT NULL,
      config blob NOT NULL,
      PRIMARY KEY (ntid, ord)) WITHOUT ROWID")
  (sqlite-execute db "
    CREATE TABLE notes (
      id integer PRIMARY KEY,
      guid text NOT NULL,
      mid integer NOT NULL,
      mod integer NOT NULL,
      usn integer NOT NULL,
      tags text NOT NULL,
      flds text NOT NULL,
      sfld integer NOT NULL,
      csum integer NOT NULL,
      flags integer NOT NULL,
      data text NOT NULL)"))

(defun gnosis-test-anki--insert-notetype (db id name type)
  "Insert notetype ID with NAME into DB.  TYPE is basic or cloze."
  (sqlite-execute db
    "INSERT INTO notetypes VALUES (?,?,0,0,'')" (list id name))
  (if (eq type 'cloze)
      (progn
        (sqlite-execute db
          "INSERT INTO fields VALUES (?,0,'Text','')
           " (list id))
        (sqlite-execute db
          "INSERT INTO fields VALUES (?,1,'Extra','')
           " (list id))
        (sqlite-execute db
          "INSERT INTO templates VALUES (?,0,'Cloze',0,0,'')" (list id)))
    ;; basic
    (sqlite-execute db
      "INSERT INTO fields VALUES (?,0,'Front','')" (list id))
    (sqlite-execute db
      "INSERT INTO fields VALUES (?,1,'Back','')" (list id))
    (sqlite-execute db
      "INSERT INTO templates VALUES (?,0,'Card 1',0,0,'')" (list id))))

(defun gnosis-test-anki--insert-image-notetype (db id name)
  "Insert image-occlusion notetype ID with NAME into DB."
  (sqlite-execute db
    "INSERT INTO notetypes VALUES (?,?,0,0,'')" (list id name))
  (sqlite-execute db
    "INSERT INTO fields VALUES (?,0,'Image','')" (list id))
  (sqlite-execute db
    "INSERT INTO fields VALUES (?,1,'Mask SVG','')" (list id))
  (sqlite-execute db
    "INSERT INTO templates VALUES (?,0,'Card 1',0,0,'')" (list id)))

(defun gnosis-test-anki--insert-note (db id mid flds tags)
  "Insert a note into DB with ID, MID, FLDS, TAGS."
  (sqlite-execute db
    "INSERT INTO notes VALUES (?,?,?,0,0,?,?,0,0,0,'')"
    (list id (format "guid%d" id) mid tags flds)))

;;; ---- Random data generation ----

(defvar gnosis-test-anki--words
  '("Emacs" "Lisp" "buffer" "window" "frame" "mode" "hook"
    "function" "variable" "macro" "defun" "lambda" "cons"
    "car" "cdr" "list" "vector" "string" "symbol" "integer"
    "float" "hash" "table" "alist" "plist" "sequence" "atom"
    "nil" "point" "mark" "region" "overlay" "text" "font"
    "face" "keymap" "binding" "command" "prefix" "minor"
    "major" "global" "local" "dynamic" "lexical" "scope")
  "Word pool for random note generation.")

(defvar gnosis-test-anki--tag-pool
  '("Science" "Math" "History" "Biology::Genetics"
    "CS::Algorithms" "CS::Data_Structures" "Languages::Lisp"
    "Languages::Python" "#tagged" "multi-word-tag"
    "Geography" "Physics::Quantum" "Art::Music")
  "Tag pool for random note generation.")

(defun gnosis-test-anki--random-sentence (n)
  "Generate a random sentence of N words."
  (let ((words gnosis-test-anki--words)
        (len (length gnosis-test-anki--words)))
    (mapconcat (lambda (_) (nth (random len) words))
               (number-sequence 1 n) " ")))

(defun gnosis-test-anki--random-tags (max-tags)
  "Return a tag string with 1 to MAX-TAGS random tags."
  (let* ((pool gnosis-test-anki--tag-pool)
         (len (length pool))
         (n (1+ (random max-tags)))
         (tags nil))
    (dotimes (_ n)
      (push (nth (random len) pool) tags))
    (concat " " (mapconcat #'identity (delete-dups tags) " ") " ")))

(defun gnosis-test-anki--random-html-sentence (n)
  "Generate a sentence of N words with random HTML markup."
  (let ((words gnosis-test-anki--words)
        (len (length gnosis-test-anki--words)))
    (mapconcat
     (lambda (_)
       (let ((w (nth (random len) words))
             (r (random 10)))
         (cond ((= r 0) (format "<b>%s</b>" w))
               ((= r 1) (format "<i>%s</i>" w))
               ((= r 2) (format "<u>%s</u>" w))
               (t w))))
     (number-sequence 1 n) " ")))

(defun gnosis-test-anki--populate-collection (path n-basic n-cloze)
  "Create an Anki collection DB at PATH with random data.
N-BASIC basic notes and N-CLOZE cloze notes, plus 1 image occlusion."
  (let ((db (sqlite-open path))
        (note-id 1))
    (gnosis-test-anki--create-schema db)
    ;; Notetypes: 100=Basic, 200=Cloze, 300=Image Occlusion
    (gnosis-test-anki--insert-notetype db 100 "Basic" 'basic)
    (gnosis-test-anki--insert-notetype db 200 "Cloze" 'cloze)
    (gnosis-test-anki--insert-image-notetype db 300 "Image Occlusion")
    ;; Basic notes
    (dotimes (_ n-basic)
      (let ((front (gnosis-test-anki--random-html-sentence (+ 3 (random 8))))
            (back (gnosis-test-anki--random-html-sentence (+ 2 (random 10))))
            (tags (gnosis-test-anki--random-tags 4)))
        (gnosis-test-anki--insert-note db note-id 100
                                       (concat front "\x1f" back) tags)
        (setq note-id (1+ note-id))))
    ;; Cloze notes
    (dotimes (_ n-cloze)
      (let* ((w1 (nth (random (length gnosis-test-anki--words))
                       gnosis-test-anki--words))
             (w2 (nth (random (length gnosis-test-anki--words))
                       gnosis-test-anki--words))
             (text (format "%s is related to {{c1::%s}}"
                           (gnosis-test-anki--random-sentence 3) w1))
             ;; Some notes get a second cloze deletion
             (text (if (zerop (random 3))
                       (concat text (format " and {{c2::%s}}" w2))
                     text))
             (extra (gnosis-test-anki--random-sentence (+ 1 (random 5))))
             (tags (gnosis-test-anki--random-tags 3)))
        (gnosis-test-anki--insert-note db note-id 200
                                       (concat text "\x1f" extra) tags)
        (setq note-id (1+ note-id))))
    ;; One image occlusion note (should be skipped)
    (gnosis-test-anki--insert-note db note-id 300
                                   (concat "image_data" "\x1f" "svg_mask")
                                   " img ")
    (sqlite-close db)
    path))

(defun gnosis-test-anki--populate-deck (path n-basic n-cloze)
  "Create an .apkg-like Anki DB at PATH.
Same as collection but simulates a deck export (identical schema)."
  (gnosis-test-anki--populate-collection path n-basic n-cloze))

;;; ---- Group 1: parse-notes with real schema ----

(ert-deftest gnosis-test-anki-parse-collection ()
  "Parse notes from a collection-style Anki DB with random data."
  (let* ((tmp (make-temp-file "anki-coll-" nil ".db"))
         (anki-db nil)
         (model-info (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (gnosis-test-anki--populate-collection tmp 10 5)
          (setq anki-db (sqlite-open tmp))
          ;; Build model-info as import-db does
          (let ((notetypes (sqlite-select anki-db
                            "SELECT id, name FROM notetypes"))
                (cloze-ids (mapcar #'car
                            (sqlite-select anki-db
                              "SELECT DISTINCT ntid FROM templates
                               WHERE name COLLATE NOCASE = 'Cloze'"))))
            (dolist (nt notetypes)
              (let* ((ntid (car nt))
                     (mid (number-to-string ntid))
                     (fields (mapcar #'cadr
                              (sqlite-select anki-db
                                "SELECT ord, name FROM fields
                                 WHERE ntid = ? ORDER BY ord"
                                (list ntid))))
                     (mtype (cond
                             ((cl-find-if (lambda (f)
                                            (string-match-p
                                             "Image\\|SVG\\|Mask" f))
                                          fields)
                              'skip)
                             ((member ntid cloze-ids) 1)
                             (t 0))))
                (puthash mid (cons mtype (cons 1 fields)) model-info))))
          (let* ((result (gnosis-anki--parse-notes anki-db model-info))
                 (skipped (car result))
                 (prepared (cdr result)))
            ;; 1 image occlusion skipped
            (should (>= skipped 1))
            ;; At least 10 basic + some clozes (each cloze note -> 1-2 items)
            (should (>= (length prepared) 15))
            ;; All items have required keys
            (dolist (item prepared)
              (should (plist-get item :type))
              (should (plist-get item :keimenon))
              (should (plist-get item :tags)))))
      (when anki-db (sqlite-close anki-db))
      (delete-file tmp))))

(ert-deftest gnosis-test-anki-parse-basic-content ()
  "Verify basic note parsing produces correct fields."
  (let* ((tmp (make-temp-file "anki-basic-" nil ".db"))
         (anki-db nil)
         (model-info (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Single known basic note
          (let ((db (sqlite-open tmp)))
            (gnosis-test-anki--create-schema db)
            (gnosis-test-anki--insert-notetype db 100 "Basic" 'basic)
            (gnosis-test-anki--insert-note db 1 100
              (concat "<b>Bold</b> question" "\x1f" "<i>Italic</i> answer")
              " emacs lisp ")
            (sqlite-close db))
          (setq anki-db (sqlite-open tmp))
          (puthash "100" '(0 1 "Front" "Back") model-info)
          (let* ((result (gnosis-anki--parse-notes anki-db model-info))
                 (prepared (cdr result))
                 (item (car prepared)))
            (should (= 1 (length prepared)))
            (should (string= "basic" (plist-get item :type)))
            (should (string= "*Bold* question" (plist-get item :keimenon)))
            (should (equal '("Italic answer") (plist-get item :answer)))
            (should (member "emacs" (plist-get item :tags)))
            (should (member "lisp" (plist-get item :tags)))))
      (when anki-db (sqlite-close anki-db))
      (delete-file tmp))))

(ert-deftest gnosis-test-anki-parse-cloze-content ()
  "Verify cloze note parsing extracts answers and hints."
  (let* ((tmp (make-temp-file "anki-cloze-" nil ".db"))
         (anki-db nil)
         (model-info (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (let ((db (sqlite-open tmp)))
            (gnosis-test-anki--create-schema db)
            (gnosis-test-anki--insert-notetype db 200 "Cloze" 'cloze)
            ;; Multi-cloze note
            (gnosis-test-anki--insert-note db 1 200
              (concat "{{c1::Emacs}} is a {{c2::text editor}}"
                      "\x1f" "Extra info")
              " cloze ")
            (sqlite-close db))
          (setq anki-db (sqlite-open tmp))
          (puthash "200" '(1 1 "Text" "Extra") model-info)
          (let* ((result (gnosis-anki--parse-notes anki-db model-info))
                 (prepared (cdr result)))
            ;; 2 cloze items from one note
            (should (= 2 (length prepared)))
            (dolist (item prepared)
              (should (string= "cloze" (plist-get item :type)))
              (should (string= "Emacs is a text editor"
                                (plist-get item :keimenon)))
              (should (string= "Extra info" (plist-get item :parathema))))))
      (when anki-db (sqlite-close anki-db))
      (delete-file tmp))))

(ert-deftest gnosis-test-anki-parse-skips ()
  "Image occlusion, empty answers, and no-cloze notes are skipped."
  (let* ((tmp (make-temp-file "anki-skip-" nil ".db"))
         (anki-db nil)
         (model-info (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (let ((db (sqlite-open tmp)))
            (gnosis-test-anki--create-schema db)
            (gnosis-test-anki--insert-notetype db 100 "Basic" 'basic)
            (gnosis-test-anki--insert-notetype db 200 "Cloze" 'cloze)
            (gnosis-test-anki--insert-image-notetype db 300 "Image Occlusion")
            ;; Empty back
            (gnosis-test-anki--insert-note db 1 100
              (concat "Q" "\x1f" "") " t ")
            ;; No cloze markers
            (gnosis-test-anki--insert-note db 2 200
              (concat "no cloze" "\x1f" "extra") " t ")
            ;; Image occlusion
            (gnosis-test-anki--insert-note db 3 300
              (concat "img" "\x1f" "mask") " t ")
            (sqlite-close db))
          (setq anki-db (sqlite-open tmp))
          (puthash "100" '(0 1 "Front" "Back") model-info)
          (puthash "200" '(1 1 "Text" "Extra") model-info)
          (puthash "300" '(skip 1 "Image" "Mask SVG") model-info)
          (let* ((result (gnosis-anki--parse-notes anki-db model-info))
                 (skipped (car result))
                 (prepared (cdr result)))
            (should (= 3 skipped))
            (should (= 0 (length prepared)))))
      (when anki-db (sqlite-close anki-db))
      (delete-file tmp))))

(ert-deftest gnosis-test-anki-parse-tags-cached ()
  "Tag parsing is cached per unique tag string."
  (let* ((tmp (make-temp-file "anki-cache-" nil ".db"))
         (anki-db nil)
         (model-info (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (let ((db (sqlite-open tmp)))
            (gnosis-test-anki--create-schema db)
            (gnosis-test-anki--insert-notetype db 100 "Basic" 'basic)
            (gnosis-test-anki--insert-note db 1 100
              (concat "Q1" "\x1f" "A1") " shared_tag ")
            (gnosis-test-anki--insert-note db 2 100
              (concat "Q2" "\x1f" "A2") " shared_tag ")
            (sqlite-close db))
          (setq anki-db (sqlite-open tmp))
          (puthash "100" '(0 1 "Front" "Back") model-info)
          (let* ((result (gnosis-anki--parse-notes anki-db model-info))
                 (prepared (cdr result)))
            (should (= 2 (length prepared)))
            ;; Tags should be equal (segment cache produces same values)
            (should (equal (plist-get (nth 0 prepared) :tags)
                           (plist-get (nth 1 prepared) :tags)))))
      (when anki-db (sqlite-close anki-db))
      (delete-file tmp))))

;;; ---- Group 2: bulk-insert-chunk ----

(ert-deftest gnosis-test-anki-bulk-insert-basic ()
  "Bulk insert basic themata and verify all DB tables."
  (gnosis-test-with-db
    (let* ((items (list (list :type "basic"
                              :keimenon "What is Emacs?"
                              :hypothesis '("")
                              :answer '("A text editor")
                              :parathema "See manual"
                              :tags '("emacs" "editor"))
                        (list :type "basic"
                              :keimenon "What is Lisp?"
                              :hypothesis '("")
                              :answer '("A programming language")
                              :parathema ""
                              :tags '("lisp"))))
           (ids '(1001 1002))
           (gnosis-val (prin1-to-string gnosis-algorithm-gnosis-value))
           (amnesia-val gnosis-algorithm-amnesia-value)
           (today (prin1-to-string (gnosis-algorithm-date))))
      (gnosis-anki--bulk-insert-chunk gnosis-db items ids
                                      gnosis-val amnesia-val today)
      ;; Verify themata
      (should (= 2 (length (gnosis-select 'id 'themata nil t))))
      (should (string= "basic" (gnosis-get 'type 'themata '(= id 1001))))
      (should (string= "What is Emacs?"
                        (gnosis-get 'keimenon 'themata '(= id 1001))))
      ;; Verify review
      (should (= 2 (length (gnosis-select 'id 'review nil t))))
      ;; Verify review_log
      (should (= 2 (length (gnosis-select 'id 'review-log nil t))))
      ;; Verify extras
      (should (string= "See manual"
                        (gnosis-get 'parathema 'extras '(= id 1001))))
      ;; Verify tags
      (let ((tags (gnosis-select 'tag 'thema-tag '(= thema-id 1001) t)))
        (should (= 2 (length tags)))
        (should (member "emacs" tags))
        (should (member "editor" tags)))
      (let ((tags (gnosis-select 'tag 'thema-tag '(= thema-id 1002) t)))
        (should (= 1 (length tags)))
        (should (member "lisp" tags))))))

(ert-deftest gnosis-test-anki-bulk-insert-cloze ()
  "Bulk insert cloze themata and verify encoding."
  (gnosis-test-with-db
    (let* ((items (list (list :type "cloze"
                              :keimenon "Emacs is a text editor"
                              :hypothesis '("editor hint")
                              :answer '("text editor")
                              :parathema "extra info"
                              :tags '("cloze"))))
           (ids '(2001))
           (gnosis-val (prin1-to-string gnosis-algorithm-gnosis-value))
           (amnesia-val gnosis-algorithm-amnesia-value)
           (today (prin1-to-string (gnosis-algorithm-date))))
      (gnosis-anki--bulk-insert-chunk gnosis-db items ids
                                      gnosis-val amnesia-val today)
      (should (= 1 (length (gnosis-select 'id 'themata nil t))))
      (should (string= "cloze" (gnosis-get 'type 'themata '(= id 2001))))
      (should (string= "Emacs is a text editor"
                        (gnosis-get 'keimenon 'themata '(= id 2001))))
      (should (equal '("text editor")
                     (gnosis-get 'answer 'themata '(= id 2001))))
      (should (string= "extra info"
                        (gnosis-get 'parathema 'extras '(= id 2001)))))))

(ert-deftest gnosis-test-anki-bulk-insert-many-tags ()
  "Bulk insert with many tags triggers tag batching."
  (gnosis-test-with-db
    (let* ((many-tags (cl-loop for i from 1 to 20
                               collect (format "tag%d" i)))
           (items (list (list :type "basic"
                              :keimenon "Q"
                              :hypothesis '("")
                              :answer '("A")
                              :parathema ""
                              :tags many-tags)))
           (ids '(3001))
           (gnosis-val (prin1-to-string gnosis-algorithm-gnosis-value))
           (amnesia-val gnosis-algorithm-amnesia-value)
           (today (prin1-to-string (gnosis-algorithm-date))))
      (gnosis-anki--bulk-insert-chunk gnosis-db items ids
                                      gnosis-val amnesia-val today)
      (let ((tags (gnosis-select 'tag 'thema-tag '(= thema-id 3001) t)))
        (should (= 20 (length tags)))))))

;;; ---- Group 3: full import (collection-style DB) ----

(ert-deftest gnosis-test-anki-import-collection ()
  "Import a collection-style DB with random basic and cloze notes."
  (gnosis-test-with-db
    (let ((anki-file (make-temp-file "anki-coll-" nil ".db")))
      (unwind-protect
          (progn
            (gnosis-test-anki--populate-collection anki-file 10 5)
            (gnosis-anki--import-db anki-file)
            ;; Wait for async timer to complete
            (sleep-for 1)
            ;; At least 15 themata (10 basic + 5+ cloze items)
            (let ((count (length (gnosis-select 'id 'themata nil t))))
              (should (>= count 15)))
            ;; Every thema has a review entry
            (should (= (length (gnosis-select 'id 'themata nil t))
                       (length (gnosis-select 'id 'review nil t))))
            ;; Every thema has a review_log entry
            (should (= (length (gnosis-select 'id 'themata nil t))
                       (length (gnosis-select 'id 'review-log nil t))))
            ;; Every thema has an extras entry
            (should (= (length (gnosis-select 'id 'themata nil t))
                       (length (gnosis-select 'id 'extras nil t))))
            ;; Tags were inserted
            (should (> (length (gnosis-select 'tag 'thema-tag nil t)) 0)))
        (when (file-exists-p anki-file)
          (delete-file anki-file))))))

(ert-deftest gnosis-test-anki-import-deck ()
  "Import a deck-style DB (same schema, different data mix)."
  (gnosis-test-with-db
    (let ((anki-file (make-temp-file "anki-deck-" nil ".db")))
      (unwind-protect
          (progn
            (gnosis-test-anki--populate-deck anki-file 5 10)
            (gnosis-anki--import-db anki-file)
            (sleep-for 1)
            ;; At least 15 themata (5 basic + 10+ cloze items)
            (let ((count (length (gnosis-select 'id 'themata nil t))))
              (should (>= count 15)))
            ;; All tables consistent
            (should (= (length (gnosis-select 'id 'themata nil t))
                       (length (gnosis-select 'id 'review nil t)))))
        (when (file-exists-p anki-file)
          (delete-file anki-file))))))

(ert-deftest gnosis-test-anki-import-empty ()
  "Import from Anki DB with no notes."
  (gnosis-test-with-db
    (let ((anki-file (make-temp-file "anki-empty-" nil ".db")))
      (unwind-protect
          (progn
            (let ((db (sqlite-open anki-file)))
              (gnosis-test-anki--create-schema db)
              (gnosis-test-anki--insert-notetype db 100 "Basic" 'basic)
              (sqlite-close db))
            (gnosis-anki--import-db anki-file)
            (should (= 0 (length (gnosis-select 'id 'themata nil t)))))
        (when (file-exists-p anki-file)
          (delete-file anki-file))))))

;;; ---- Group 4: generate-ids ----

(ert-deftest gnosis-test-anki-generate-ids-unique ()
  "gnosis-generate-ids returns N unique IDs."
  (gnosis-test-with-db
    (let* ((gnosis--id-cache (make-hash-table :test 'equal))
           (ids (gnosis-generate-ids 100)))
      (should (= 100 (length ids)))
      (should (= 100 (length (delete-dups (copy-sequence ids))))))))

(ert-deftest gnosis-test-anki-generate-ids-no-collisions ()
  "gnosis-generate-ids avoids existing IDs in cache."
  (gnosis-test-with-db
    (let* ((gnosis--id-cache (make-hash-table :test 'equal)))
      ;; Pre-fill cache with some IDs
      (dotimes (i 100)
        (puthash (+ 1000000000 i) t gnosis--id-cache))
      (let ((ids (gnosis-generate-ids 50)))
        (should (= 50 (length ids)))
        (dolist (id ids)
          (should-not (and (>= id 1000000000)
                           (< id 1000000100))))))))

;;; ---- Group 5: html-to-org ----

(ert-deftest gnosis-test-anki-html-bold ()
  "Convert bold HTML to org."
  (should (string= "*text*" (gnosis-anki--html-to-org "<b>text</b>"))))

(ert-deftest gnosis-test-anki-html-italic ()
  "Convert italic HTML to org."
  (should (string= "/text/" (gnosis-anki--html-to-org "<i>text</i>"))))

(ert-deftest gnosis-test-anki-html-link ()
  "Convert HTML link to org link."
  (should (string= "[[http://example.com][click]]"
                    (gnosis-anki--html-to-org
                     "<a href=\"http://example.com\">click</a>"))))

(ert-deftest gnosis-test-anki-html-strip-tags ()
  "Strip unknown HTML tags."
  (should (string= "plain text"
                    (gnosis-anki--html-to-org "<span>plain text</span>"))))

;;; ---- Group 6: parse-tags ----

(ert-deftest gnosis-test-anki-parse-tags-hierarchical ()
  "Split hierarchical tags on :: and flatten."
  (should (equal '("Science" "Biology" "Genetics")
                 (gnosis-anki--parse-tags "Science::Biology::Genetics"))))

(ert-deftest gnosis-test-anki-parse-tags-dashes ()
  "Replace dashes with underscores."
  (should (equal '("my_tag")
                 (gnosis-anki--parse-tags "my-tag"))))

(ert-deftest gnosis-test-anki-parse-tags-hash-prefix ()
  "Strip # prefixes."
  (should (equal '("tagged")
                 (gnosis-anki--parse-tags "#tagged"))))

(ert-deftest gnosis-test-anki-parse-tags-strip-symbols ()
  "Remove characters invalid in org tags."
  (should (equal '("AnKing_Step_1_2")
                 (gnosis-anki--parse-tags "AnKing_Step_1_&_2")))
  (should (equal '("ObGyn")
                 (gnosis-anki--parse-tags "Ob/Gyn")))
  (should (equal '("gram")
                 (gnosis-anki--parse-tags "gram+")))
  (should (equal '("OLD_VERSION")
                 (gnosis-anki--parse-tags "[OLD_VERSION]"))))

(ert-deftest gnosis-test-anki-parse-tags-unicode ()
  "Keep Unicode letters in tags."
  (should (equal '("ΑΓΙΑ_ΓΡΑΦΗ")
                 (gnosis-anki--parse-tags "ΑΓΙΑ_ΓΡΑΦΗ"))))

(ert-deftest gnosis-test-anki-parse-tags-dedup ()
  "Deduplicate tags."
  (should (equal '("a" "b")
                 (gnosis-anki--parse-tags "a b a"))))

(provide 'gnosis-test-anki)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-anki.el ends here
