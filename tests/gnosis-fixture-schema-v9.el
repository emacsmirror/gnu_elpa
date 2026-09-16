;;; gnosis-fixture-schema-v9.el --- Released 0.11.0 fixture -*- lexical-binding: t; -*-
;; Literal SQL and initial rows captured from 190a68ecaa0fc3f30255985764317c73e558c575.
(require 'gnosis-db)

(defconst gnosis-fixture-v9-ddl
  '("CREATE TABLE extras (id INTEGER PRIMARY KEY NOT NULL, parathema STRING, review_image STRING, FOREIGN KEY (id) REFERENCES themata (id) ON DELETE CASCADE)"
    "CREATE TABLE journal (id TEXT NOT NULL PRIMARY KEY, file TEXT NOT NULL, title TEXT NOT NULL, level TEXT NOT NULL, tags TEXT, mtime TEXT, hash TEXT)"
    "CREATE TABLE node_links (source TEXT, dest TEXT, FOREIGN KEY (source) REFERENCES nodes (id) ON DELETE CASCADE, UNIQUE (source, dest))"
    "CREATE TABLE node_tag (node_id TEXT NOT NULL, tag TEXT NOT NULL, FOREIGN KEY (node_id) REFERENCES nodes (id) ON DELETE CASCADE, UNIQUE (node_id, tag))"
    "CREATE TABLE nodes (id TEXT NOT NULL PRIMARY KEY, file TEXT NOT NULL, title TEXT NOT NULL, level TEXT NOT NULL, tags TEXT, mtime TEXT, hash TEXT)"
    "CREATE TABLE practice_events (event_id TEXT PRIMARY KEY NOT NULL, thema_id INTEGER NOT NULL, session_id TEXT NOT NULL, attempt INTEGER NOT NULL, reviewed_at_us INTEGER NOT NULL, rating INTEGER NOT NULL, FOREIGN KEY (thema_id) REFERENCES themata (id) ON DELETE CASCADE, UNIQUE (session_id, attempt), CHECK (attempt > 0), CHECK (reviewed_at_us > 0), CHECK (rating IN (1, 3)))"
    "CREATE TABLE practice_voids (correction_id TEXT PRIMARY KEY NOT NULL, event_id TEXT NOT NULL, UNIQUE (event_id), FOREIGN KEY (event_id) REFERENCES practice_events (event_id) ON DELETE CASCADE)"
    "CREATE TABLE review_activity_baseline (date INTEGER PRIMARY KEY NOT NULL, reviewed_total INTEGER NOT NULL, reviewed_new INTEGER NOT NULL, CHECK (reviewed_total >= 0), CHECK (reviewed_new BETWEEN 0 AND reviewed_total))"
    "CREATE TABLE review_events (event_id TEXT PRIMARY KEY NOT NULL, thema_id INTEGER NOT NULL, config_id INTEGER NOT NULL, reviewed_at_us INTEGER NOT NULL, review_day INTEGER NOT NULL, rating INTEGER NOT NULL, elapsed_days INTEGER NOT NULL, prior_stability REAL, prior_difficulty REAL, stability REAL NOT NULL, difficulty REAL NOT NULL, raw_interval_days REAL NOT NULL, calendar_interval_days INTEGER NOT NULL, due_day INTEGER NOT NULL, reps_before INTEGER NOT NULL, reps_after INTEGER NOT NULL, lapses_before INTEGER NOT NULL, lapses_after INTEGER NOT NULL, new_p INTEGER NOT NULL, FOREIGN KEY (thema_id) REFERENCES scheduler_baseline (thema_id) ON DELETE CASCADE, FOREIGN KEY (config_id) REFERENCES scheduler_config (id), CHECK (rating IN (1, 3)), CHECK (elapsed_days >= 0), CHECK ((prior_stability IS NULL) = (prior_difficulty IS NULL)), CHECK (stability > 0), CHECK (difficulty BETWEEN 1 AND 10), CHECK (raw_interval_days >= 0), CHECK (calendar_interval_days >= 1), CHECK (reps_before >= 0 AND reps_after = reps_before + 1), CHECK (lapses_before >= 0), CHECK (lapses_after = lapses_before + CASE rating WHEN 1 THEN 1 ELSE 0 END), CHECK (new_p IN (0, 1)), CHECK (new_p = CASE reps_before WHEN 0 THEN 1 ELSE 0 END))"
    "CREATE TABLE review_voids (correction_id TEXT PRIMARY KEY NOT NULL, event_id TEXT NOT NULL, UNIQUE (event_id), FOREIGN KEY (event_id) REFERENCES review_events (event_id) ON DELETE CASCADE)"
    "CREATE TABLE scheduler_active (id INTEGER PRIMARY KEY NOT NULL, config_id INTEGER NOT NULL, CHECK (id = 1), FOREIGN KEY (config_id) REFERENCES scheduler_config (id))"
    "CREATE TABLE scheduler_baseline (thema_id INTEGER PRIMARY KEY NOT NULL, due_day INTEGER NOT NULL, reps INTEGER NOT NULL, lapses INTEGER NOT NULL, FOREIGN KEY (thema_id) REFERENCES themata (id) ON DELETE CASCADE)"
    "CREATE TABLE scheduler_config (id INTEGER PRIMARY KEY NOT NULL, algorithm TEXT NOT NULL, model TEXT NOT NULL, implementation TEXT NOT NULL, desired_retention REAL NOT NULL, parameters TEXT NOT NULL)"
    "CREATE TABLE scheduler_state (thema_id INTEGER PRIMARY KEY NOT NULL, config_id INTEGER NOT NULL, stability REAL, difficulty REAL, last_reviewed_at_us INTEGER, last_review_day INTEGER, due_day INTEGER NOT NULL, reps INTEGER NOT NULL, lapses INTEGER NOT NULL, suspended INTEGER NOT NULL, FOREIGN KEY (thema_id) REFERENCES scheduler_baseline (thema_id) ON DELETE CASCADE, FOREIGN KEY (config_id) REFERENCES scheduler_config (id))"
    "CREATE TABLE study_history (session_id TEXT PRIMARY KEY NOT NULL, data TEXT NOT NULL)"
    "CREATE TABLE study_session (id INTEGER PRIMARY KEY NOT NULL, data TEXT NOT NULL, CHECK (id = 1))"
    "CREATE TABLE thema_links (source INTEGER, dest TEXT, FOREIGN KEY (source) REFERENCES themata (id) ON DELETE CASCADE, UNIQUE (source, dest))"
    "CREATE TABLE thema_tag (thema_id INTEGER NOT NULL, tag TEXT NOT NULL, FOREIGN KEY (thema_id) REFERENCES themata (id) ON DELETE CASCADE, UNIQUE (thema_id, tag))"
    "CREATE TABLE themata (id INTEGER PRIMARY KEY, type TEXT NOT NULL, keimenon TEXT NOT NULL, hypothesis TEXT NOT NULL, answer TEXT NOT NULL, source_guid TEXT, accepted_aliases TEXT)"
    "CREATE INDEX idx_journal_file
                   ON journal(file)"
    "CREATE INDEX idx_node_links_dest
                   ON node_links(dest)"
    "CREATE INDEX idx_node_links_source
                   ON node_links(source)"
    "CREATE INDEX idx_nodes_file
                   ON nodes(file)"
    "CREATE INDEX idx_review_events_day
        ON review_events(review_day)"
    "CREATE INDEX idx_review_events_replay
        ON review_events(thema_id, reviewed_at_us, event_id)"
    "CREATE INDEX idx_scheduler_state_due
        ON scheduler_state(suspended, due_day, reps)"
    "CREATE INDEX idx_thema_links_dest
                   ON thema_links(dest)"
    "CREATE INDEX idx_thema_links_source
                   ON thema_links(source)"
    "CREATE INDEX idx_thema_tag_tag
                   ON thema_tag(tag)"
    "CREATE INDEX idx_thema_tag_thema_id
                   ON thema_tag(thema_id)"
    "CREATE INDEX idx_themata_source_guid ON themata(source_guid)"
    "CREATE TRIGGER practice_events_no_direct_delete BEFORE DELETE ON practice_events
               WHEN EXISTS (SELECT 1 FROM themata WHERE id = OLD.thema_id)
               BEGIN SELECT RAISE(ABORT, 'hard deletion required'); END"
    "CREATE TRIGGER practice_events_no_replace BEFORE INSERT ON practice_events
               WHEN EXISTS (SELECT 1 FROM practice_events WHERE event_id = NEW.event_id OR (session_id = NEW.session_id AND attempt = NEW.attempt))
               BEGIN SELECT RAISE(ABORT, 'study identity exists'); END"
    "CREATE TRIGGER practice_events_no_update BEFORE UPDATE ON practice_events
               BEGIN SELECT RAISE(ABORT, 'immutable study evidence'); END"
    "CREATE TRIGGER practice_voids_no_direct_delete BEFORE DELETE ON practice_voids
               WHEN EXISTS (SELECT 1 FROM practice_events WHERE event_id = OLD.event_id)
               BEGIN SELECT RAISE(ABORT, 'hard deletion required'); END"
    "CREATE TRIGGER practice_voids_no_replace BEFORE INSERT ON practice_voids
               WHEN EXISTS (SELECT 1 FROM practice_voids WHERE correction_id = NEW.correction_id OR event_id = NEW.event_id)
               BEGIN SELECT RAISE(ABORT, 'study identity exists'); END"
    "CREATE TRIGGER practice_voids_no_update BEFORE UPDATE ON practice_voids
               BEGIN SELECT RAISE(ABORT, 'immutable study evidence'); END"
    "CREATE TRIGGER review_activity_baseline_no_delete
        BEFORE DELETE ON review_activity_baseline
        BEGIN
          SELECT RAISE(ABORT, 'review activity baseline is immutable');
        END"
    "CREATE TRIGGER review_activity_baseline_no_replace
        BEFORE INSERT ON review_activity_baseline
        WHEN EXISTS
          (SELECT 1 FROM review_activity_baseline WHERE date = NEW.date)
        BEGIN
          SELECT RAISE(ABORT, 'review activity baseline already exists');
        END"
    "CREATE TRIGGER review_activity_baseline_no_update
        BEFORE UPDATE ON review_activity_baseline
        BEGIN
          SELECT RAISE(ABORT, 'review activity baseline is immutable');
        END"
    "CREATE TRIGGER review_events_no_direct_delete
        BEFORE DELETE ON review_events
        WHEN EXISTS
          (SELECT 1 FROM scheduler_baseline
             WHERE thema_id = OLD.thema_id)
        BEGIN
          SELECT RAISE(ABORT, 'review events require hard thema deletion');
        END"
    "CREATE TRIGGER review_events_no_replace
        BEFORE INSERT ON review_events
        WHEN EXISTS
          (SELECT 1 FROM review_events WHERE event_id = NEW.event_id)
        BEGIN
          SELECT RAISE(ABORT, 'review event already exists');
        END"
    "CREATE TRIGGER review_events_no_update
        BEFORE UPDATE ON review_events
        BEGIN
          SELECT RAISE(ABORT, 'review events are immutable');
        END"
    "CREATE TRIGGER review_voids_no_direct_delete BEFORE DELETE ON review_voids
               WHEN EXISTS (SELECT 1 FROM review_events WHERE event_id = OLD.event_id)
               BEGIN SELECT RAISE(ABORT, 'hard deletion required'); END"
    "CREATE TRIGGER review_voids_no_replace BEFORE INSERT ON review_voids
               WHEN EXISTS (SELECT 1 FROM review_voids WHERE correction_id = NEW.correction_id OR event_id = NEW.event_id)
               BEGIN SELECT RAISE(ABORT, 'study identity exists'); END"
    "CREATE TRIGGER review_voids_no_update BEFORE UPDATE ON review_voids
               BEGIN SELECT RAISE(ABORT, 'immutable study evidence'); END"
    "CREATE TRIGGER scheduler_baseline_no_direct_delete
        BEFORE DELETE ON scheduler_baseline
        WHEN EXISTS (SELECT 1 FROM themata WHERE id = OLD.thema_id)
        BEGIN
          SELECT RAISE(ABORT, 'scheduler baseline requires hard thema deletion');
        END"
    "CREATE TRIGGER scheduler_baseline_no_replace
        BEFORE INSERT ON scheduler_baseline
        WHEN EXISTS
          (SELECT 1 FROM scheduler_baseline
             WHERE thema_id = NEW.thema_id)
        BEGIN
          SELECT RAISE(ABORT, 'scheduler baseline already exists');
        END"
    "CREATE TRIGGER scheduler_baseline_no_update
        BEFORE UPDATE ON scheduler_baseline
        BEGIN
          SELECT RAISE(ABORT, 'scheduler baseline is immutable');
        END"
    "CREATE TRIGGER scheduler_config_no_delete
        BEFORE DELETE ON scheduler_config
        BEGIN
          SELECT RAISE(ABORT, 'scheduler config is immutable');
        END"
    "CREATE TRIGGER scheduler_config_no_replace
        BEFORE INSERT ON scheduler_config
        WHEN EXISTS (SELECT 1 FROM scheduler_config WHERE id = NEW.id)
        BEGIN
          SELECT RAISE(ABORT, 'scheduler config already exists');
        END"
    "CREATE TRIGGER scheduler_config_no_update
        BEFORE UPDATE ON scheduler_config
        BEGIN
          SELECT RAISE(ABORT, 'scheduler config is immutable');
        END")
  "Released schema 9 declarations, independent of candidate constants.")

(defconst gnosis-fixture-v9-config
  '((1 "\"fsrs\"" "\"gnosis-fsrs6-v1\"" "\"fsrs-rs-6.6.1\"" 0.9 "[0.212 1.2931 2.3065 8.2956 6.4133 0.8334 3.0194 0.001 1.8722 0.1666 0.796 1.4835 0.0614 0.2629 1.6483 0.6014 1.8729 0.5425 0.0912 0.0658 0.1542]"))
  "Literal released initial scheduler configuration.")

(defun gnosis-fixture-create-v9 ()
  "Create released schema 9 on the empty fixture connection."
  (dolist (sql gnosis-fixture-v9-ddl) (sqlite-execute gnosis-db sql))
  (dolist (row gnosis-fixture-v9-config)
    (sqlite-execute gnosis-db "INSERT INTO scheduler_config VALUES (?, ?, ?, ?, ?, ?)" row))
  (sqlite-execute gnosis-db "INSERT INTO scheduler_active VALUES (1, 1)")
  (sqlite-execute gnosis-db "PRAGMA user_version = 9"))

(defun gnosis-fixture-add-basic (id question)
  "Insert basic thema ID and QUESTION using the released common columns."
  (gnosis-sqlite-execute
   gnosis-db
   "INSERT INTO themata (id, type, keimenon, hypothesis, answer) VALUES (?, ?, ?, ?, ?)"
   (list id "basic" question '("Hint") '("Answer")))
  (gnosis-scheduler-initialize-thema id (gnosis--today-int) 0)
  (gnosis--insert-into 'extras `([,id "Context" nil]))
  (gnosis--insert-into 'thema-tag `([,id "test"])))

(provide 'gnosis-fixture-schema-v9)
