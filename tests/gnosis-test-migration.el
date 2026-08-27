;;; gnosis-test-migration.el --- Migration tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for the complete migration chain v1 through v9.
;; One chain test exercises the full sequence; edge-case tests isolate
;; specific migration steps.

;;; Code:
(require 'ert)
(require 'gnosis)

(let ((lisp-dir (expand-file-name "../lisp"
                  (file-name-directory (or load-file-name default-directory)))))
  (add-to-list 'load-path lisp-dir))

(load (expand-file-name "gnosis-test-helpers.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; ---- Group 1: Test infrastructure ----

(defmacro gnosis-test-with-v1-db (&rest body)
  "Run BODY with a temp SQLite database using the v1 schema.
v1 (0.1.x): notes, decks, review (ef/ff/interval), review_log, extras
\(extra_notes/images/extra_image).  No activity_log."
  (declare (indent 0) (debug t))
  `(let* ((gnosis-test--db-file (make-temp-file "gnosis-test-mig-v1-" nil ".db"))
          (gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (user-emacs-directory (file-name-directory gnosis-test--db-file)))
     (unwind-protect
         (progn
           ;; Disable FK enforcement so DROP COLUMN / DROP TABLE
           ;; during migrations do not cascade-delete rows.
           (gnosis-sqlite-execute gnosis-db "PRAGMA foreign_keys = OFF")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE decks (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                name TEXT NOT NULL,
                UNIQUE (name))")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE notes (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                type TEXT NOT NULL,
                main TEXT NOT NULL,
                options TEXT NOT NULL,
                answer TEXT NOT NULL,
                tags TEXT DEFAULT 'untagged',
                deck_id INTEGER NOT NULL,
                FOREIGN KEY (deck_id) REFERENCES decks (id) ON DELETE CASCADE)")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE review (
                id INTEGER PRIMARY KEY NOT NULL,
                ef INTEGER NOT NULL,
                ff INTEGER NOT NULL,
                interval INTEGER NOT NULL,
                FOREIGN KEY (id) REFERENCES notes (id) ON DELETE CASCADE)")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE review_log (
                id INTEGER PRIMARY KEY NOT NULL,
                last_rev INTEGER NOT NULL,
                next_rev INTEGER NOT NULL,
                c_success INTEGER NOT NULL,
                t_success INTEGER NOT NULL,
                c_fails INTEGER NOT NULL,
                t_fails INTEGER NOT NULL,
                suspend INTEGER NOT NULL,
                n INTEGER NOT NULL,
                FOREIGN KEY (id) REFERENCES notes (id) ON DELETE CASCADE)")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE extras (
                id INTEGER PRIMARY KEY NOT NULL,
                extra_notes TEXT,
                images TEXT,
                extra_image TEXT,
                FOREIGN KEY (id) REFERENCES notes (id) ON DELETE CASCADE)")
           (gnosis-sqlite-execute gnosis-db "PRAGMA user_version = 1")
           ,@body)
       (gnosis-sqlite-close gnosis-db)
       (delete-file gnosis-test--db-file))))

(defmacro gnosis-test-with-v3-db (&rest body)
  "Run BODY with a temp SQLite database using the v3 schema.
v3: decks (no algorithm columns), notes with old column names,
review with gnosis/amnesia (no interval), activity_log."
  (declare (indent 0) (debug t))
  `(let* ((gnosis-test--db-file (make-temp-file "gnosis-test-mig-v3-" nil ".db"))
          (gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (user-emacs-directory (file-name-directory gnosis-test--db-file)))
     (unwind-protect
         (progn
           (gnosis-sqlite-execute gnosis-db "PRAGMA foreign_keys = OFF")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE decks (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                name TEXT NOT NULL,
                UNIQUE (name))")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE notes (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                type TEXT NOT NULL,
                main TEXT NOT NULL,
                options TEXT NOT NULL,
                answer TEXT NOT NULL,
                tags TEXT DEFAULT 'untagged',
                deck_id INTEGER NOT NULL,
                FOREIGN KEY (deck_id) REFERENCES decks (id) ON DELETE CASCADE)")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE review (
                id INTEGER PRIMARY KEY NOT NULL,
                gnosis INTEGER NOT NULL,
                amnesia INTEGER NOT NULL,
                FOREIGN KEY (id) REFERENCES notes (id) ON DELETE CASCADE)")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE review_log (
                id INTEGER PRIMARY KEY NOT NULL,
                last_rev INTEGER NOT NULL,
                next_rev INTEGER NOT NULL,
                c_success INTEGER NOT NULL,
                t_success INTEGER NOT NULL,
                c_fails INTEGER NOT NULL,
                t_fails INTEGER NOT NULL,
                suspend INTEGER NOT NULL,
                n INTEGER NOT NULL,
                FOREIGN KEY (id) REFERENCES notes (id) ON DELETE CASCADE)")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE extras (
                id INTEGER PRIMARY KEY NOT NULL,
                extra_notes TEXT,
                images TEXT,
                extra_image TEXT,
                FOREIGN KEY (id) REFERENCES notes (id) ON DELETE CASCADE)")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE activity_log (
                date TEXT NOT NULL,
                reviewed_total INTEGER NOT NULL,
                reviewed_new INTEGER NOT NULL)")
           (gnosis-sqlite-execute gnosis-db "PRAGMA user_version = 3")
           ,@body)
       (gnosis-sqlite-close gnosis-db)
       (delete-file gnosis-test--db-file))))

(defmacro gnosis-test-with-v4-db (&rest body)
  "Run BODY with a temp SQLite database using the v4 schema.
v4: themata (renamed from notes), keimenon/hypothesis columns,
tags and links tables, extras with parathema/review_image."
  (declare (indent 0) (debug t))
  `(let* ((gnosis-test--db-file (make-temp-file "gnosis-test-mig-v4-" nil ".db"))
          (gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (user-emacs-directory (file-name-directory gnosis-test--db-file)))
     (unwind-protect
         (progn
           (gnosis-sqlite-execute gnosis-db "PRAGMA foreign_keys = OFF")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE decks (
                id INTEGER PRIMARY KEY,
                name TEXT NOT NULL,
                UNIQUE (name))")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE themata (
                id INTEGER PRIMARY KEY,
                type TEXT NOT NULL,
                keimenon TEXT NOT NULL,
                hypothesis TEXT NOT NULL,
                answer TEXT NOT NULL,
                tags TEXT DEFAULT 'untagged',
                deck_id INTEGER NOT NULL,
                FOREIGN KEY (deck_id) REFERENCES decks (id) ON DELETE CASCADE)")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE review (
                id INTEGER PRIMARY KEY NOT NULL,
                gnosis INTEGER NOT NULL,
                amnesia INTEGER NOT NULL,
                FOREIGN KEY (id) REFERENCES themata (id) ON DELETE CASCADE)")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE review_log (
                id INTEGER PRIMARY KEY NOT NULL,
                last_rev INTEGER NOT NULL,
                next_rev INTEGER NOT NULL,
                c_success INTEGER NOT NULL,
                t_success INTEGER NOT NULL,
                c_fails INTEGER NOT NULL,
                t_fails INTEGER NOT NULL,
                suspend INTEGER NOT NULL,
                n INTEGER NOT NULL,
                FOREIGN KEY (id) REFERENCES themata (id) ON DELETE CASCADE)")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE extras (
                id INTEGER PRIMARY KEY NOT NULL,
                parathema TEXT,
                review_image TEXT,
                FOREIGN KEY (id) REFERENCES themata (id) ON DELETE CASCADE)")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE activity_log (
                date TEXT NOT NULL,
                reviewed_total INTEGER NOT NULL,
                reviewed_new INTEGER NOT NULL)")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE tags (
                tag TEXT PRIMARY KEY,
                UNIQUE (tag))")
           (gnosis-sqlite-execute gnosis-db
             "CREATE TABLE links (
                source INTEGER,
                dest TEXT,
                FOREIGN KEY (source) REFERENCES themata (id) ON DELETE CASCADE,
                UNIQUE (source, dest))")
           (gnosis-sqlite-execute gnosis-db "PRAGMA user_version = 4")
           ,@body)
       (gnosis-sqlite-close gnosis-db)
       (delete-file gnosis-test--db-file))))

(defun gnosis-test--create-legacy-scheduler-tables ()
  "Create the scheduler tables present before the v9 cutover."
  (gnosis-sqlite-execute
   gnosis-db
   "CREATE TABLE review (
      id INTEGER PRIMARY KEY NOT NULL,
      gnosis INTEGER NOT NULL,
      amnesia INTEGER NOT NULL,
      FOREIGN KEY (id) REFERENCES themata (id) ON DELETE CASCADE)")
  (gnosis-sqlite-execute
   gnosis-db
   "CREATE TABLE review_log (
      id INTEGER PRIMARY KEY NOT NULL,
      last_rev INTEGER NOT NULL, next_rev INTEGER NOT NULL,
      c_success INTEGER NOT NULL, t_success INTEGER NOT NULL,
      c_fails INTEGER NOT NULL, t_fails INTEGER NOT NULL,
      suspend INTEGER NOT NULL, n INTEGER NOT NULL,
      FOREIGN KEY (id) REFERENCES themata (id) ON DELETE CASCADE)")
  (gnosis-sqlite-execute
   gnosis-db
   "CREATE TABLE activity_log (
      date INTEGER NOT NULL,
      reviewed_total INTEGER NOT NULL,
      reviewed_new INTEGER NOT NULL)"))

(defmacro gnosis-test-with-v8-db (&rest body)
  "Run BODY with a disposable exact v8 database."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-db
     (dolist (table '(review-events scheduler-state scheduler-baseline
                      scheduler-config review-activity-baseline))
       (gnosis-sqlite-execute
        gnosis-db (format "DROP TABLE %s" (gnosis-sqlite--ident table))))
     (gnosis-test--create-legacy-scheduler-tables)
     (gnosis--db-set-version 8)
     ,@body))

(defun gnosis-test--populate-v8-scheduler-data ()
  "Insert representative content, schedules, and activity into a v8 DB."
  (dolist (row '((1 "Q1" ("A1")) (2 "Q2" ("A2"))))
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO themata
        (id, type, keimenon, hypothesis, answer, source_guid)
      VALUES (?, ?, ?, ?, ?, ?)"
     (list (nth 0 row) "basic" (nth 1 row) '("") (nth 2 row) nil))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO review (id, gnosis, amnesia) VALUES (?, ?, ?)"
     (list (nth 0 row) 1 1)))
  (dolist (row '((1 20260820 20260825 3 5 0 2 0 7)
                 (2 20260821 20260901 0 0 0 0 1 0)))
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO review_log
        (id, last_rev, next_rev, c_success, t_success,
         c_fails, t_fails, suspend, n)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
     row))
  (dolist (row '((20260820 5 2) (20260820 3 1) (20260821 4 0)))
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO activity_log (date, reviewed_total, reviewed_new)
      VALUES (?, ?, ?)"
     row)))

;;; Populate functions

(defun gnosis-test--populate-v1-data ()
  "Insert v1-era data: old column names, integer MCQ answer, y-or-n, dash tags."
  ;; Decks
  (gnosis-sqlite-execute gnosis-db
    "INSERT INTO decks (id, name) VALUES (?, ?)" '(1 "Pharmacology"))
  (gnosis-sqlite-execute gnosis-db
    "INSERT INTO decks (id, name) VALUES (?, ?)" '(2 "Languages"))
  ;; Notes (main, options - v1 column names)
  (let ((notes
         '((1 "basic" "MOA of aspirin?"
              ("") ("COX inhibition") ("pharmacology" "my-nsaids") 1)
           (2 "mcq" "Which cephalosporin gen for meningitis?"
              ("1st" "2nd" "3rd" "4th") 3 ("pharma-cology") 1)
           (3 "y-or-n" "Is Emacs the best editor?"
              ("Yes" "No") 121 ("my-tag") 2)
           (4 "cloze" "repetitio est mater *memoriae*"
              ("") ("memoriae") ("latin" "phrases") 2))))
    (dolist (row notes)
      (gnosis-sqlite-execute gnosis-db
        "INSERT INTO notes (id, type, main, options, answer, tags, deck_id)
         VALUES (?, ?, ?, ?, ?, ?, ?)"
        row)))
  ;; Review (ef, ff, interval - v1 column names)
  (let ((reviews '((1 (1 1 1.3) 0.5 5)
                   (2 (1 1 1.3) 0.5 3)
                   (3 (2 1 1.5) 0.5 7)
                   (4 (1 1 1.3) 0.5 2))))
    (dolist (row reviews)
      (gnosis-sqlite-execute gnosis-db
        "INSERT INTO review (id, ef, ff, interval) VALUES (?, ?, ?, ?)"
        row)))
  ;; Review log
  (let ((logs '((1 20240101 20240110 3 5 0 1 0 6)
                (2 20240102 20240115 2 3 1 2 0 5)
                (3 20240103 20240120 4 7 0 0 0 7)
                (4 20240104 20240108 1 2 2 3 0 5))))
    (dolist (row logs)
      (gnosis-sqlite-execute gnosis-db
        "INSERT INTO review_log (id, last_rev, next_rev, c_success, t_success,
                                 c_fails, t_fails, suspend, n)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        row)))
  ;; Extras (extra_notes, images, extra_image - v1 column names)
  (let ((extras
         '((1 "Aspirin is an NSAID." "/img/aspirin.png" "/img/aspirin-extra.png")
           (2 "Third-gen cross the BBB." nil nil)
           (3 "A question about editors." nil nil)
           (4 "A Latin proverb." nil nil))))
    (dolist (row extras)
      (gnosis-sqlite-execute gnosis-db
        "INSERT INTO extras (id, extra_notes, images, extra_image) VALUES (?, ?, ?, ?)"
        row))))

;;; ---- Group 2: Full chain test ----

(ert-deftest gnosis-test-migrate-v1-to-v9-chain ()
  "Sequential migration chain from v1 through v9 on one DB."
  (gnosis-test-with-v1-db
    (gnosis-test--populate-v1-data)

    ;; -- v1 -> v2 --
    (gnosis-db--migrate-v2)
    (should (= 2 (gnosis--db-version)))
    ;; Deck algorithm columns added (NULL values)
    (let ((row (car (gnosis-sqlite-select gnosis-db
                      "SELECT failure_factor, ef_increase, ef_decrease,
                              ef_threshold, initial_interval
                       FROM decks WHERE id = 1"))))
      (should (= 5 (length row))))
    ;; Notes table still exists with all 4 rows
    (should (= 4 (length (gnosis-sqlite-select gnosis-db "SELECT id FROM notes"))))
    ;; Review data preserved (still ef/ff/interval at v2)
    (let ((row (car (gnosis-sqlite-select gnosis-db
                      "SELECT ef, ff, interval FROM review WHERE id = 1"))))
      (should (= 3 (length row))))
    ;; Extras preserved
    (should (equal "Aspirin is an NSAID."
                   (caar (gnosis-sqlite-select gnosis-db
                           "SELECT extra_notes FROM extras WHERE id = 1"))))
    ;; Activity log created (empty)
    (should (= 0 (length (gnosis-sqlite-select gnosis-db "SELECT * FROM activity_log"))))

    ;; -- v2 -> v3 --
    (gnosis-db--migrate-v3)
    (should (= 3 (gnosis--db-version)))
    ;; Deck algorithm columns dropped
    (should-error (gnosis-sqlite-select gnosis-db "SELECT failure_factor FROM decks"))
    ;; Review: ef->gnosis, ff->amnesia, interval dropped
    (let ((row (car (gnosis-sqlite-select gnosis-db
                      "SELECT gnosis, amnesia FROM review WHERE id = 1"))))
      (should (= 2 (length row))))
    (should-error (gnosis-sqlite-select gnosis-db "SELECT ef FROM review"))
    (should-error (gnosis-sqlite-select gnosis-db "SELECT interval FROM review"))
    ;; Notes still exist
    (should (= 4 (length (gnosis-sqlite-select gnosis-db "SELECT id FROM notes"))))

    ;; -- v3 -> v4 --
    (gnosis-db--migrate-v4)
    (should (= 4 (gnosis--db-version)))
    ;; notes renamed to themata
    (should (= 4 (length (gnosis-sqlite-select gnosis-db "SELECT id FROM themata"))))
    (should-error (gnosis-sqlite-select gnosis-db "SELECT * FROM notes"))
    ;; Columns renamed: main->keimenon, options->hypothesis
    (should (equal "MOA of aspirin?"
                   (caar (gnosis-sqlite-select gnosis-db
                           "SELECT keimenon FROM themata WHERE id = 1"))))
    (should-error (gnosis-sqlite-select gnosis-db "SELECT main FROM themata"))
    ;; Extras: extra_notes->parathema, images->review_image, extra_image dropped
    (let ((row (car (gnosis-sqlite-select gnosis-db
                      "SELECT parathema, review_image FROM extras WHERE id = 1"))))
      (should (equal "Aspirin is an NSAID." (nth 0 row)))
      (should (equal "/img/aspirin.png" (nth 1 row))))
    (should-error (gnosis-sqlite-select gnosis-db "SELECT extra_image FROM extras"))
    ;; Tags and links tables created
    (should (>= (length (gnosis-sqlite-select gnosis-db "SELECT tag FROM tags")) 1))
    (should (= 0 (length (gnosis-sqlite-select gnosis-db "SELECT * FROM links"))))
    ;; MCQ fix: integer answer 3 -> text "3rd"
    (let* ((row (car (gnosis-sqlite-select gnosis-db
                       "SELECT answer FROM themata WHERE id = 2")))
           (ans (car row)))
      (should (listp ans))
      (should (equal "3rd" (car ans))))
    ;; y-or-n -> MCQ: type=mcq, hypothesis=("Yes" "No"), answer=("Yes")
    (let ((row (car (gnosis-sqlite-select gnosis-db
                      "SELECT type, hypothesis, answer FROM themata WHERE id = 3"))))
      (should (equal "mcq" (nth 0 row)))
      (should (equal '("Yes" "No") (nth 1 row)))
      (should (equal '("Yes") (nth 2 row))))
    ;; Dash tags -> underscore
    (let* ((tags (caar (gnosis-sqlite-select gnosis-db
                         "SELECT tags FROM themata WHERE id = 1"))))
      (should (member "my_nsaids" tags))
      (should-not (member "my-nsaids" tags)))
    ;; Review and review_log preserved
    (should (= 4 (length (gnosis-sqlite-select gnosis-db "SELECT id FROM review"))))
    (should (= 4 (length (gnosis-sqlite-select gnosis-db "SELECT id FROM review_log"))))

    ;; -- v4 -> v5 --
    (gnosis-db--migrate-v5)
    (should (= 5 (gnosis--db-version)))
    ;; Table is already themata (v4 renamed it), v5 is idempotent
    (should (= 4 (length (gnosis-sqlite-select gnosis-db "SELECT id FROM themata"))))

    ;; -- v5 -> v6 --
    (gnosis-db--migrate-v6)
    (should (= 6 (gnosis--db-version)))
    ;; Deck names became tags (in junction table)
    (let ((tags (mapcar #'car (gnosis-sqlite-select gnosis-db
                                "SELECT tag FROM thema_tag WHERE thema_id = 1"))))
      (should (member "Pharmacology" tags)))
    ;; Decks table dropped
    (should-error (gnosis-sqlite-select gnosis-db "SELECT * FROM decks"))
    ;; links -> thema_links
    (should-error (gnosis-sqlite-select gnosis-db "SELECT * FROM links"))
    ;; thema_tag junction table populated
    (should (> (caar (gnosis-sqlite-select gnosis-db "SELECT COUNT(*) FROM thema_tag")) 0))
    ;; Node tables created (empty)
    (dolist (table '("nodes" "journal" "node_tag" "node_links"))
      (should (= 0 (length (gnosis-sqlite-select gnosis-db
                              (format "SELECT * FROM %s" table))))))
    ;; Review data survived entire chain
    (should (= 4 (length (gnosis-sqlite-select gnosis-db "SELECT id FROM review"))))
    ;; Activity log preserved
    (should (= 0 (length (gnosis-sqlite-select gnosis-db "SELECT * FROM activity_log"))))

    ;; -- v6 -> v7 --
    ;; Insert activity_log with list dates (as emacsql would have stored them)
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO activity_log (date, reviewed_total, reviewed_new) VALUES (?, ?, ?)"
      (list '(2026 3 20) 10 2))
    ;; Also update review_log dates to list format to test conversion
    (gnosis-sqlite-execute gnosis-db
      "UPDATE review_log SET last_rev = ?, next_rev = ? WHERE id = 1"
      (list '(2026 3 15) '(2026 3 25)))
    (gnosis-db--migrate-v7)
    (should (= 7 (gnosis--db-version)))
    ;; review_log dates are now YYYYMMDD integers
    (let ((row (car (gnosis-sqlite-select gnosis-db
                      "SELECT last_rev, next_rev FROM review_log WHERE id = 1"))))
      (should (= 20260315 (nth 0 row)))
      (should (= 20260325 (nth 1 row))))
    ;; activity_log dates are now YYYYMMDD integers
    (let ((row (car (gnosis-sqlite-select gnosis-db
                      "SELECT date FROM activity_log"))))
      (should (= 20260320 (car row))))

    ;; -- v7 -> v8 --
    (gnosis-db--migrate-v8)
    (should (= 8 (gnosis--db-version)))
    ;; source_guid column exists (NULL for existing themata)
    (let ((row (car (gnosis-sqlite-select gnosis-db
                      "SELECT source_guid FROM themata WHERE id = 1"))))
      (should (null (car row))))
    ;; -- v8 -> v9 --
    (gnosis-db--migrate-v9)
    (should (= 9 (gnosis--db-version)))
    (should (= 4 (caar (gnosis-sqlite-select
                        gnosis-db "SELECT COUNT(*) FROM scheduler_state"))))
    (should (equal '((20260320 10 2))
                   (gnosis-sqlite-select
                    gnosis-db "SELECT * FROM review_activity_baseline")))
    (dolist (legacy '(review review-log activity-log))
      (should-not (gnosis-table-exists-p legacy)))))

;;; ---- Group 3: Edge-case tests ----

(ert-deftest gnosis-test-fresh-v9-omits-legacy-storage ()
  "Create a fresh current database without retired scheduler tables."
  (gnosis-test-with-db
    (dolist (legacy '(review review-log activity-log))
      (should-not (gnosis-table-exists-p legacy)))))

(ert-deftest gnosis-test-migrate-mcq-answer-index-first ()
  "MCQ answer index 1 resolves to the first option."
  (gnosis-test-with-v3-db
    ;; One deck, one MCQ note with answer=1
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO decks (id, name) VALUES (?, ?)" '(1 "Test"))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO notes (id, type, main, options, answer, tags, deck_id)
       VALUES (?, ?, ?, ?, ?, ?, ?)"
      '(1 "mcq" "Pick first?" ("Alpha" "Beta") 1 ("test") 1))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review (id, gnosis, amnesia) VALUES (?, ?, ?)"
      '(1 (1 1 1.3) 0.5))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review_log (id, last_rev, next_rev, c_success, t_success,
                               c_fails, t_fails, suspend, n)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      '(1 20240101 20240110 1 1 0 0 0 1))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO extras (id, extra_notes, images, extra_image)
       VALUES (?, ?, ?, ?)"
      '(1 nil nil nil))
    (gnosis-db--migrate-v4)
    (let* ((row (car (gnosis-sqlite-select gnosis-db
                       "SELECT answer FROM themata WHERE id = 1")))
           (ans (car row)))
      (should (listp ans))
      (should (equal "Alpha" (car ans))))))

(ert-deftest gnosis-test-migrate-mcq-answer-index-last ()
  "MCQ answer index at end resolves to the last option."
  (gnosis-test-with-v3-db
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO decks (id, name) VALUES (?, ?)" '(1 "Test"))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO notes (id, type, main, options, answer, tags, deck_id)
       VALUES (?, ?, ?, ?, ?, ?, ?)"
      '(1 "mcq" "Pick last?" ("A" "B" "C" "D") 4 ("test") 1))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review (id, gnosis, amnesia) VALUES (?, ?, ?)"
      '(1 (1 1 1.3) 0.5))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review_log (id, last_rev, next_rev, c_success, t_success,
                               c_fails, t_fails, suspend, n)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      '(1 20240101 20240110 1 1 0 0 0 1))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO extras (id, extra_notes, images, extra_image)
       VALUES (?, ?, ?, ?)"
      '(1 nil nil nil))
    (gnosis-db--migrate-v4)
    (let* ((row (car (gnosis-sqlite-select gnosis-db
                       "SELECT answer FROM themata WHERE id = 1")))
           (ans (car row)))
      (should (listp ans))
      (should (equal "D" (car ans))))))

(ert-deftest gnosis-test-migrate-y-or-n-answer-no ()
  "y-or-n with answer 110 (?n) migrates to MCQ answer No."
  (gnosis-test-with-v3-db
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO decks (id, name) VALUES (?, ?)" '(1 "Test"))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO notes (id, type, main, options, answer, tags, deck_id)
       VALUES (?, ?, ?, ?, ?, ?, ?)"
      '(1 "y-or-n" "Is the sky green?" ("Yes" "No") 110 ("test") 1))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review (id, gnosis, amnesia) VALUES (?, ?, ?)"
      '(1 (1 1 1.3) 0.5))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review_log (id, last_rev, next_rev, c_success, t_success,
                               c_fails, t_fails, suspend, n)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      '(1 20240101 20240110 1 1 0 0 0 1))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO extras (id, extra_notes, images, extra_image)
       VALUES (?, ?, ?, ?)"
      '(1 nil nil nil))
    (gnosis-db--migrate-v4)
    (let ((row (car (gnosis-sqlite-select gnosis-db
                      "SELECT type, hypothesis, answer FROM themata WHERE id = 1"))))
      (should (equal "mcq" (nth 0 row)))
      (should (equal '("Yes" "No") (nth 1 row)))
      (should (equal '("No") (nth 2 row))))))

(ert-deftest gnosis-test-migrate-tag-multiple-dashes ()
  "Tag with multiple dashes has all dashes replaced by underscores."
  (gnosis-test-with-v3-db
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO decks (id, name) VALUES (?, ?)" '(1 "Test"))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO notes (id, type, main, options, answer, tags, deck_id)
       VALUES (?, ?, ?, ?, ?, ?, ?)"
      '(1 "basic" "Question?" ("") ("Answer") ("multi-dash-tag") 1))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review (id, gnosis, amnesia) VALUES (?, ?, ?)"
      '(1 (1 1 1.3) 0.5))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review_log (id, last_rev, next_rev, c_success, t_success,
                               c_fails, t_fails, suspend, n)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      '(1 20240101 20240110 1 1 0 0 0 1))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO extras (id, extra_notes, images, extra_image)
       VALUES (?, ?, ?, ?)"
      '(1 nil nil nil))
    (gnosis-db--migrate-v4)
    (let* ((tags (caar (gnosis-sqlite-select gnosis-db
                         "SELECT tags FROM themata WHERE id = 1"))))
      (should (member "multi_dash_tag" tags))
      (should-not (member "multi-dash-tag" tags)))))

(ert-deftest gnosis-test-migrate-empty-db-chain ()
  "Empty DB schema migrates v1 through v9 without error."
  (gnosis-test-with-v1-db
    (gnosis-db--migrate-v2)
    (gnosis-db--migrate-v3)
    (gnosis-db--migrate-v4)
    (gnosis-db--migrate-v5)
    (gnosis-db--migrate-v6)
    (gnosis-db--migrate-v7)
    (gnosis-db--migrate-v8)
    (gnosis-db--migrate-v9)
    (should (= 9 (gnosis--db-version)))
    (dolist (legacy '(review review-log activity-log))
      (should-not (gnosis-table-exists-p legacy)))))

(ert-deftest gnosis-test-migrate-v4-populated-to-v6 ()
  "PRAGMA 4 DB with data: v5 is no-op, v6 migrates decks/tags/nodes."
  (gnosis-test-with-v4-db
    ;; Two decks
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO decks (id, name) VALUES (?, ?)" '(1 "Greek"))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO decks (id, name) VALUES (?, ?)" '(2 "Latin"))
    ;; Two themata across decks, with tags
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO themata (id, type, keimenon, hypothesis, answer, tags, deck_id)
       VALUES (?, ?, ?, ?, ?, ?, ?)"
      '(1 "basic" "What is logos?" ("") ("reason") ("philosophy") 1))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO themata (id, type, keimenon, hypothesis, answer, tags, deck_id)
       VALUES (?, ?, ?, ?, ?, ?, ?)"
      '(2 "cloze" "repetitio est mater *memoriae*" ("") ("memoriae") ("proverbs") 2))
    ;; Tags table
    (gnosis-sqlite-execute gnosis-db
      "INSERT OR IGNORE INTO tags (tag) VALUES (?)" '("philosophy"))
    (gnosis-sqlite-execute gnosis-db
      "INSERT OR IGNORE INTO tags (tag) VALUES (?)" '("proverbs"))
    ;; Links table (one thema linked to a node)
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO links (source, dest) VALUES (?, ?)" '(1 "node-uuid-1"))
    ;; Review data
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review (id, gnosis, amnesia) VALUES (?, ?, ?)"
      '(1 (2 1 1.5) 0.5))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review (id, gnosis, amnesia) VALUES (?, ?, ?)"
      '(2 (1 1 1.3) 0.5))
    ;; Review log
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review_log (id, last_rev, next_rev, c_success, t_success,
                               c_fails, t_fails, suspend, n)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      '(1 20250101 20250115 5 10 0 1 0 11))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review_log (id, last_rev, next_rev, c_success, t_success,
                               c_fails, t_fails, suspend, n)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      '(2 20250102 20250110 2 3 1 2 0 5))

    ;; -- v5: no-op (table is already themata) --
    (gnosis-db--migrate-v5)
    (should (= 5 (gnosis--db-version)))
    (should (= 2 (length (gnosis-sqlite-select gnosis-db "SELECT id FROM themata"))))

    ;; -- v6: deck->tag, junction table, node tables --
    (gnosis-db--migrate-v6)
    (should (= 6 (gnosis--db-version)))
    ;; Deck names merged into tags (in junction table)
    (let ((tags (mapcar #'car (gnosis-sqlite-select gnosis-db
                                "SELECT tag FROM thema_tag WHERE thema_id = 1"))))
      (should (member "Greek" tags))
      (should (member "philosophy" tags)))
    (let ((tags (mapcar #'car (gnosis-sqlite-select gnosis-db
                                "SELECT tag FROM thema_tag WHERE thema_id = 2"))))
      (should (member "Latin" tags))
      (should (member "proverbs" tags)))
    ;; Decks table gone
    (should-error (gnosis-sqlite-select gnosis-db "SELECT * FROM decks"))
    ;; links -> thema_links, old table gone
    (should (= 1 (length (gnosis-sqlite-select gnosis-db "SELECT * FROM thema_links"))))
    (should-error (gnosis-sqlite-select gnosis-db "SELECT * FROM links"))
    ;; thema_tag junction populated (philosophy, Greek, proverbs, Latin)
    (should (= 4 (caar (gnosis-sqlite-select gnosis-db "SELECT COUNT(*) FROM thema_tag"))))
    ;; Node tables created (empty)
    (dolist (table '("nodes" "journal" "node_tag" "node_links"))
      (should (= 0 (length (gnosis-sqlite-select gnosis-db
                              (format "SELECT * FROM %s" table))))))
    ;; Review data survived
    (should (= 2 (length (gnosis-sqlite-select gnosis-db "SELECT id FROM review"))))
    (should (= 2 (length (gnosis-sqlite-select gnosis-db "SELECT id FROM review_log"))))))

(ert-deftest gnosis-test-migrate-deck-with-no-themata ()
  "Deck with no referencing themata is dropped cleanly by v6 migration."
  (gnosis-test-with-v4-db
    ;; Insert a deck but no themata referencing it
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO decks (id, name) VALUES (?, ?)" '(1 "OrphanDeck"))
    (gnosis-db--migrate-v5)
    (gnosis-db--migrate-v6)
    (should (= 6 (gnosis--db-version)))
    ;; Deck table is gone
    (should-error (gnosis-sqlite-select gnosis-db "SELECT * FROM decks"))))

(ert-deftest gnosis-test-migrate-commits-after-migrations ()
  "gnosis--db-run-migrations calls gnosis--commit-migration with correct versions."
  (gnosis-test-with-v4-db
    (let (commit-args)
      (cl-letf (((symbol-function 'gnosis--commit-migration)
		 (lambda (from to) (setq commit-args (list from to)))))
	(gnosis--db-run-migrations 4))
      ;; Should have been called with from=4, to=9 (last migration run)
      (should (equal '(4 9) commit-args))
      (should (= 9 (gnosis--db-version))))))

(ert-deftest gnosis-test-migrate-no-commit-when-up-to-date ()
  "gnosis--db-run-migrations does not commit when no migrations are needed."
  (gnosis-test-with-v4-db
    ;; Manually set version to current
    (gnosis--db-set-version gnosis-db-version)
    (let (commit-called)
      (cl-letf (((symbol-function 'gnosis--commit-migration)
		 (lambda (_from _to) (setq commit-called t))))
	(gnosis--db-run-migrations gnosis-db-version))
      (should-not commit-called))))

;;; ---- Group 4: v7 date migration tests ----

(ert-deftest gnosis-test-migrate-v7-list-dates-to-integers ()
  "v7 migration converts Lisp list dates to YYYYMMDD integers."
  (gnosis-test-with-db
    ;; Simulate pre-v7 schedule storage with list dates in DB.
    (gnosis--db-set-version 6)
    (gnosis-test--create-legacy-scheduler-tables)
    ;; Insert test data with list dates (as emacsql stores them)
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO themata (id, type, keimenon, hypothesis, answer)
       VALUES (?, ?, ?, ?, ?)"
      '(1 "basic" "Test Q" ("") ("A")))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review (id, gnosis, amnesia) VALUES (?, ?, ?)"
      '(1 (1 1 1.3) 0.5))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review_log (id, last_rev, next_rev, c_success, t_success,
                               c_fails, t_fails, suspend, n)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      (list 1 '(2026 3 20) '(2026 3 25) 3 5 0 1 0 6))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO activity_log (date, reviewed_total, reviewed_new) VALUES (?, ?, ?)"
      (list '(2026 3 20) 15 3))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO activity_log (date, reviewed_total, reviewed_new) VALUES (?, ?, ?)"
      (list '(2026 3 21) 8 1))
    ;; Run v7 migration
    (gnosis-db--migrate-v7)
    (should (= 7 (gnosis--db-version)))
    ;; Verify review_log dates
    (let ((row (car (gnosis-sqlite-select gnosis-db
                      "SELECT last_rev, next_rev FROM review_log WHERE id = 1"))))
      (should (= 20260320 (nth 0 row)))
      (should (= 20260325 (nth 1 row))))
    ;; Verify activity_log dates
    (let ((dates (mapcar #'car (gnosis-sqlite-select gnosis-db
                                 "SELECT date FROM activity_log ORDER BY date"))))
      (should (= 20260320 (car dates)))
      (should (= 20260321 (cadr dates))))))

(ert-deftest gnosis-test-migrate-v7-idempotent ()
  "v7 migration is idempotent: already-integer dates are unchanged."
  (gnosis-test-with-db
    (gnosis--db-set-version 6)
    (gnosis-test--create-legacy-scheduler-tables)
    ;; Insert data with integer dates (already migrated format)
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO themata (id, type, keimenon, hypothesis, answer)
       VALUES (?, ?, ?, ?, ?)"
      '(1 "basic" "Test Q" ("") ("A")))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review (id, gnosis, amnesia) VALUES (?, ?, ?)"
      '(1 (1 1 1.3) 0.5))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO review_log (id, last_rev, next_rev, c_success, t_success,
                               c_fails, t_fails, suspend, n)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      '(1 20260320 20260325 3 5 0 1 0 6))
    (gnosis-sqlite-execute gnosis-db
      "INSERT INTO activity_log (date, reviewed_total, reviewed_new) VALUES (?, ?, ?)"
      '(20260320 15 3))
    ;; Run v7 migration
    (gnosis-db--migrate-v7)
    (should (= 7 (gnosis--db-version)))
    ;; Values unchanged
    (let ((row (car (gnosis-sqlite-select gnosis-db
                      "SELECT last_rev, next_rev FROM review_log WHERE id = 1"))))
      (should (= 20260320 (nth 0 row)))
      (should (= 20260325 (nth 1 row))))
    (let ((date (caar (gnosis-sqlite-select gnosis-db "SELECT date FROM activity_log"))))
      (should (= 20260320 date)))))

;;; ---- Group 5: v9 FSRS bootstrap ----

(ert-deftest gnosis-test-migrate-v8-to-v9-preserves-known-facts ()
  "Bootstrap scheduler storage without inventing FSRS memory or events."
  (gnosis-test-with-v8-db
    (gnosis-test--populate-v8-scheduler-data)
    (gnosis-sqlite-execute gnosis-db "PRAGMA foreign_keys = ON")
    (let ((content (gnosis-sqlite-select
                    gnosis-db "SELECT * FROM themata ORDER BY id")))
      (gnosis-db--migrate-v9)
      (should (= 9 (gnosis--db-version)))
      (should (equal content
                     (gnosis-sqlite-select
                      gnosis-db "SELECT * FROM themata ORDER BY id"))))
    (should
     (equal '((1 20260825 7 2) (2 20260901 0 0))
            (gnosis-sqlite-select
             gnosis-db "SELECT * FROM scheduler_baseline ORDER BY thema_id")))
    (should
     (equal '((1 1 nil nil nil nil 20260825 7 2 0)
              (2 1 nil nil nil nil 20260901 0 0 1))
            (gnosis-sqlite-select
             gnosis-db "SELECT * FROM scheduler_state ORDER BY thema_id")))
    (should (= 0 (caar (gnosis-sqlite-select
                        gnosis-db "SELECT COUNT(*) FROM review_events"))))
    (should
     (equal '((20260820 8 3) (20260821 4 0))
            (gnosis-sqlite-select
             gnosis-db "SELECT * FROM review_activity_baseline ORDER BY date")))
    (should
     (equal '((20260820 8 3) (20260821 4 0))
            (gnosis-db-review-activity gnosis-db)))
    (dolist (legacy '(review review-log activity-log))
      (should-not (gnosis-table-exists-p legacy)))))

(ert-deftest gnosis-test-migrate-v8-to-v9-acceptance-journey ()
  "Migrate, review, replay, reopen, and hard-delete one overdue thema."
  (gnosis-test-with-v8-db
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO themata VALUES (?, ?, ?, ?, ?, ?)"
     '(1 "basic" "Question" ("") ("Answer") nil))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO review VALUES (?, ?, ?)" '(1 1 1))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO review_log VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
     '(1 20260810 20260820 1 1 0 1 0 2))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO activity_log VALUES (?, ?, ?)" '(20260829 3 1))
    (gnosis-db--migrate-v9)
    (should (equal '(1 20260820 2 1)
                   (car (gnosis-sqlite-select
                         gnosis-db "SELECT * FROM scheduler_baseline"))))
    (should (equal '(1 1 nil nil nil nil 20260820 2 1 0)
                   (car (gnosis-sqlite-select
                         gnosis-db "SELECT * FROM scheduler_state"))))
    (dolist (legacy '(review review-log activity-log))
      (should-not (gnosis-table-exists-p legacy)))
    (cl-letf (((symbol-function 'gnosis--today-int) (lambda () 20260830)))
      (should (equal '(1) (gnosis-review-get-due-themata)))
      (should-not (gnosis-review-is-thema-new-p 1))
      (should-not (gnosis-suspended-p 1)))
    (let* ((event-id (make-string 64 ?a))
           (first (gnosis-scheduler-accept-review
                   event-id 1 'success 1000000 20260830))
           (state-after
            (car (gnosis-sqlite-select gnosis-db
                                       "SELECT * FROM scheduler_state")))
           (retry (gnosis-scheduler-accept-review
                   event-id 1 'success 1000000 20260830))
           (activity '((20260829 3 1) (20260830 1 0))))
      (should (plist-get first :inserted-p))
      (should-not (plist-get retry :inserted-p))
      (should (= 1 (caar (gnosis-sqlite-select
                          gnosis-db "SELECT COUNT(*) FROM review_events"))))
      (should (equal state-after
                     (car (gnosis-sqlite-select
                           gnosis-db "SELECT * FROM scheduler_state"))))
      (should (equal activity (gnosis-review-activity)))
      (gnosis-sqlite-execute
       gnosis-db "DELETE FROM scheduler_state WHERE thema_id = ?" '(1))
      (gnosis-scheduler-rebuild-state 1 0 gnosis-db)
      (should (equal state-after
                     (car (gnosis-sqlite-select
                           gnosis-db "SELECT * FROM scheduler_state"))))
      (gnosis-sqlite-close gnosis-db)
      (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
      (gnosis-db-init)
      (should (equal state-after
                     (car (gnosis-sqlite-select
                           gnosis-db "SELECT * FROM scheduler_state"))))
      (should (equal activity (gnosis-review-activity)))
      (cl-letf (((symbol-function 'gnosis--today-int) (lambda () 20260830)))
        (should-not (gnosis-review-get-due-themata))
        (should-not (gnosis-review-is-thema-new-p 1))
        (should-not (gnosis-suspended-p 1)))
      (gnosis-delete-themata '(1))
      (dolist (table '(themata scheduler-baseline scheduler-state review-events))
        (should (= 0 (caar (gnosis-sqlite-select
                            gnosis-db
                            (format "SELECT COUNT(*) FROM %s"
                                    (gnosis-sqlite--ident table))))))))))

(ert-deftest gnosis-test-migrate-v8-to-v9-rolls-back-completely ()
  "Roll back scheduler DDL, data, and version when bootstrap fails."
  (gnosis-test-with-v8-db
    (gnosis-test--populate-v8-scheduler-data)
    (gnosis-sqlite-execute gnosis-db "PRAGMA foreign_keys = ON")
    (let ((content (gnosis-sqlite-select gnosis-db "SELECT * FROM themata"))
          (schedule (gnosis-sqlite-select gnosis-db "SELECT * FROM review_log"))
          (activity (gnosis-sqlite-select gnosis-db "SELECT * FROM activity_log"))
          failure-reached)
      (cl-letf (((symbol-function 'gnosis-db--create-scheduler-guards)
                 (lambda (_db)
                   (setq failure-reached t)
                   (error "controlled migration failure"))))
        (should-error (gnosis-db--migrate-v9)))
      (should failure-reached)
      (should (= 8 (gnosis--db-version)))
      (dolist (table '(scheduler-config scheduler-baseline scheduler-state
                       review-events review-activity-baseline))
        (should-not (gnosis-table-exists-p table)))
      (should (equal content
                     (gnosis-sqlite-select gnosis-db "SELECT * FROM themata")))
      (should (equal schedule
                     (gnosis-sqlite-select gnosis-db "SELECT * FROM review_log")))
      (should (equal activity
                     (gnosis-sqlite-select gnosis-db "SELECT * FROM activity_log"))))))

(ert-deftest gnosis-test-migrate-v8-to-v9-rejects-missing-schedule ()
  "Reject incomplete legacy schedule authority without partial migration."
  (gnosis-test-with-v8-db
    (gnosis-test--populate-v8-scheduler-data)
    (gnosis-sqlite-execute gnosis-db "DELETE FROM review_log WHERE id = ?" '(2))
    (should-error (gnosis-db--migrate-v9))
    (should (= 8 (gnosis--db-version)))
    (dolist (table '(scheduler-config scheduler-baseline scheduler-state
                     review-events review-activity-baseline))
      (should-not (gnosis-table-exists-p table)))
    (should (= 2 (caar (gnosis-sqlite-select
                        gnosis-db "SELECT COUNT(*) FROM themata"))))))

(provide 'gnosis-test-migration)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-migration.el ends here
