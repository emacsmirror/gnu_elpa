;;; gnosis-test-backup.el --- Data snapshot recovery tests -*- lexical-binding: t; -*-
;;; Commentary:
;; Real SQLite and disposable managed bytes only.
;;; Code:
(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-backup)
(require 'gnosis-model)
(require 'gnosis-review)

(defun gnosis-test-backup--bytes (file bytes)
  "Write literal BYTES into FILE, creating its disposable parents."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'no-conversion))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert bytes))))

(defun gnosis-test-backup--asset ()
  "Create an unreferenced binary asset and empty directory; return its file."
  (let ((file (expand-file-name "assets/orphan/payload.bin" gnosis-dir)))
    (gnosis-test-backup--bytes file (unibyte-string 0 255 13 10 128))
    (make-directory (expand-file-name "assets/empty" gnosis-dir))
    file))

(defun gnosis-test-backup--no-stage ()
  "Assert no unpublished sibling stage remains in the disposable data root."
  (should-not (directory-files gnosis-dir nil "^\\.gnosis-backup-")))

(defun gnosis-test-backup--rewrite-manifest (snapshot change)
  "Apply CHANGE to SNAPSHOT's JSON value without blessing payload bytes."
  (let* ((file (expand-file-name "manifest.json" snapshot))
         (value (with-temp-buffer (insert-file-contents file) (json-parse-buffer))))
    (with-temp-file file (insert (json-serialize (funcall change value))))))

(ert-deftest gnosis-backup-roundtrip-empty-assets ()
  (gnosis-test-with-db
    (let ((snapshot (expand-file-name "snapshot" gnosis-dir))
          (restore (expand-file-name "restore" gnosis-dir))
          (owner gnosis-db))
      (gnosis-test--add-basic-thema "Question" "Answer")
      (should (equal snapshot (gnosis-backup-data snapshot)))
      (should (gnosis-backup-verify snapshot))
      (should (equal restore (gnosis-backup-restore snapshot restore)))
      (should (gnosis-backup-verify restore))
      (should (eq owner gnosis-db))
      (let ((copy (sqlite-open (expand-file-name "database.sqlite" restore))))
        (unwind-protect
            (should (equal (sqlite-select owner "SELECT * FROM themata")
                           (sqlite-select copy "SELECT * FROM themata")))
          (sqlite-close copy))))))

(ert-deftest gnosis-backup-connected-path-and-all-managed-bytes ()
  (gnosis-test-with-db
    (let* ((file (gnosis-test-backup--asset))
           (hash (gnosis-assets-hash file))
           (snapshot (expand-file-name "snapshot" gnosis-dir))
           (restore (expand-file-name "restore" gnosis-dir))
           (root (gnosis-assets-root))
           (before (gnosis-backup--inventory root)))
      (let ((gnosis-dir "/not-the-connected-database"))
        (gnosis-backup-data snapshot))
      (let ((gnosis-db nil))
        (should (gnosis-backup-verify snapshot))
        (gnosis-backup-restore snapshot restore)
        (should-not gnosis-db))
      (should (equal before (gnosis-backup--inventory (expand-file-name "assets" restore))))
      (should (equal before (gnosis-backup--inventory root)))
      (should (equal hash (gnosis-assets-hash file)))
      (should-not (file-exists-p (expand-file-name "gnosis.db" restore))))))

(ert-deftest gnosis-backup-never-connects-or-migrates ()
  (let ((directory (make-temp-file "gnosis-backup-disconnected-" t)) (gnosis-db nil))
    (unwind-protect
        (let ((gnosis-dir directory))
          (should-error (gnosis-backup-data (expand-file-name "snapshot" directory)))
          (should-not (directory-files directory nil directory-files-no-dot-files-regexp)))
      (delete-directory directory t)))
  (let ((gnosis-db (sqlite-open)))
    (unwind-protect (should-error (gnosis-backup-data "/unused-snapshot"))
      (sqlite-close gnosis-db))))

(ert-deftest gnosis-backup-rejects-destination-aliases-before-staging ()
  (gnosis-test-with-db
    (gnosis-test-backup--asset)
    (let ((alias (expand-file-name "alias" gnosis-dir))
          (file (expand-file-name "existing" gnosis-dir)))
      (make-symbolic-link gnosis-dir alias)
      (gnosis-test-backup--bytes file "Preserve")
      (dolist (destination (list gnosis-dir file gnosis-test--db-file
                                 (expand-file-name "assets/new" gnosis-dir)
                                 (expand-file-name "assets" gnosis-dir)
                                 (expand-file-name "alias/new" gnosis-dir)
                                 (expand-file-name "absent/new" gnosis-dir)))
        (cl-letf (((symbol-function 'make-temp-file)
                   (lambda (&rest _) (ert-fail "Staging before preflight refusal"))))
          (should-error (gnosis-backup-data destination) :type 'user-error)))
      (should (equal "Preserve" (with-temp-buffer (insert-file-contents file) (buffer-string))))
      (should (file-symlink-p alias))
      (gnosis-test-backup--no-stage))))

(ert-deftest gnosis-backup-rejects-absent-asset-root-overlap ()
  (gnosis-test-with-db
    (let ((root (expand-file-name "assets" gnosis-dir)))
      (should-error (gnosis-backup-data root))
      (should-not (file-exists-p root))
      (gnosis-test-backup--no-stage))))

(ert-deftest gnosis-backup-rejects-source-links-and-unsafe-names ()
  (dolist (kind '(root revision file unsafe fifo))
    (gnosis-test-with-db
      (let* ((file (gnosis-test-backup--asset))
             (snapshot (expand-file-name "snapshot" gnosis-dir))
             (outside (expand-file-name "outside" gnosis-dir)))
        (make-directory outside)
        (pcase kind
          ('root (delete-directory (gnosis-assets-root) t)
                 (make-symbolic-link outside (gnosis-assets-root)))
          ('revision (make-symbolic-link outside (expand-file-name "assets/link" gnosis-dir)))
          ('file (delete-file file) (make-symbolic-link outside file))
          ('unsafe (gnosis-test-backup--bytes (expand-file-name "assets/bad name" gnosis-dir) "bad"))
          ('fifo (should (= 0 (call-process "mkfifo" nil nil nil
                                            (expand-file-name "assets/pipe" gnosis-dir))))))
        (should-error (gnosis-backup-data snapshot))
        (should-not (file-exists-p snapshot))
        (should (file-directory-p outside))
        (gnosis-test-backup--no-stage)))))

(ert-deftest gnosis-backup-detects-source-drift-during-copy ()
  (dolist (kind '(modify delete add directory symlink))
    (gnosis-test-with-db
      (let* ((file (gnosis-test-backup--asset))
             (snapshot (expand-file-name "snapshot" gnosis-dir))
             (copy (symbol-function 'copy-file)))
        (cl-letf (((symbol-function 'copy-file)
                   (lambda (from to &rest args)
                     (prog1 (apply copy from to args)
                       (when (equal from file)
                         (pcase kind
                           ('modify (gnosis-test-backup--bytes file "changed"))
                           ('delete (delete-file file))
                           ('add (gnosis-test-backup--bytes
                                  (expand-file-name "assets/late" gnosis-dir) "new"))
                           ('directory (make-directory (expand-file-name "assets/late" gnosis-dir)))
                           ('symlink (make-symbolic-link file (expand-file-name "assets/late" gnosis-dir)))))))))
          (should-error (gnosis-backup-data snapshot)))
        (should-not (file-exists-p snapshot))
        (gnosis-test-backup--no-stage)))))

(ert-deftest gnosis-backup-detects-source-drift-after-stage-validation ()
  (gnosis-test-with-db
    (gnosis-test-backup--asset)
    (let ((check (symbol-function 'gnosis-backup--check-database))
          (snapshot (expand-file-name "snapshot" gnosis-dir)))
      (cl-letf (((symbol-function 'gnosis-backup--check-database)
                 (lambda (file)
                   (funcall check file)
                   (gnosis-test-backup--bytes (expand-file-name "assets/late" gnosis-dir) "late"))))
        (should-error (gnosis-backup-data snapshot)))
      (should-not (file-exists-p snapshot))
      (gnosis-test-backup--no-stage))))

(ert-deftest gnosis-backup-copy-rename-quit-cleanup-and-retry ()
  (dolist (operation '(copy-file rename-file))
    (dolist (condition '(error quit))
      (gnosis-test-with-db
        (gnosis-test-backup--asset)
        (let* ((snapshot (expand-file-name "snapshot" gnosis-dir))
               (root (gnosis-assets-root))
               (before (gnosis-backup--inventory root))
               (dbhash (gnosis-assets-hash gnosis-test--db-file)))
          (cl-letf (((symbol-function operation)
                     (lambda (&rest _) (signal condition '("Injected interruption")))))
            (should (eq condition (condition-case err (gnosis-backup-data snapshot)
                                    ((error quit) (car err))))))
          (should-not (file-exists-p snapshot))
          (should (equal before (gnosis-backup--inventory root)))
          (should (equal dbhash (gnosis-assets-hash gnosis-test--db-file)))
          (should (equal '((1)) (sqlite-select gnosis-db "SELECT 1")))
          (gnosis-test-backup--no-stage)
          (should (gnosis-backup-data (expand-file-name "retry" gnosis-dir))))))))

(ert-deftest gnosis-backup-pins-connection-during-callbacks ()
  (gnosis-test-with-db
    (let* ((asset (gnosis-test-backup--asset))
           (original gnosis-db)
           (other (sqlite-open (expand-file-name "other.db" gnosis-dir)))
           (copy (symbol-function 'copy-file))
           (snapshot (expand-file-name "snapshot" gnosis-dir)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'copy-file)
                       (lambda (from to &rest args)
                         (prog1 (apply copy from to args)
                           (when (equal from asset) (setq gnosis-db other))))))
              (should-error (gnosis-backup-data snapshot)))
            (should (eq gnosis-db other))
            (should-not (file-exists-p snapshot))
            (should (equal '((1)) (sqlite-select original "SELECT 1")))
            (gnosis-test-backup--no-stage))
        (setq gnosis-db original)
        (sqlite-close other)))))

(ert-deftest gnosis-backup-refuses-publication-destination-created-mid-copy ()
  (gnosis-test-with-db
    (let* ((asset (gnosis-test-backup--asset))
           (snapshot (expand-file-name "snapshot" gnosis-dir))
           (copy (symbol-function 'copy-file)))
      (cl-letf (((symbol-function 'copy-file)
                 (lambda (from to &rest args)
                   (prog1 (apply copy from to args)
                     (when (equal from asset)
                       (gnosis-test-backup--bytes (expand-file-name "keep" snapshot) "keep"))))))
        (should-error (gnosis-backup-data snapshot)))
      (should (equal '("keep") (directory-files snapshot nil directory-files-no-dot-files-regexp)))
      (gnosis-test-backup--no-stage))))

(ert-deftest gnosis-backup-corrupt-missing-extra-and-wrong-type-payloads ()
  (dolist (kind '(modify truncate missing extra extra-directory db db-missing db-extra link directory))
    (gnosis-test-with-db
      (gnosis-test-backup--asset)
      (let* ((snapshot (gnosis-backup-data (expand-file-name "snapshot" gnosis-dir)))
             (file (expand-file-name "assets/orphan/payload.bin" snapshot))
             (restore (expand-file-name "restore" gnosis-dir)))
        (pcase kind
          ('modify (gnosis-test-backup--bytes file (unibyte-string 0 255 13 10 129)))
          ('truncate (gnosis-test-backup--bytes file ""))
          ('missing (delete-file file))
          ('extra (gnosis-test-backup--bytes (expand-file-name "assets/new.bin" snapshot) "new"))
          ('extra-directory (make-directory (expand-file-name "assets/new" snapshot)))
          ('db (gnosis-test-backup--bytes (expand-file-name "database.sqlite" snapshot) "bad"))
          ('db-missing (delete-file (expand-file-name "database.sqlite" snapshot)))
          ('db-extra (gnosis-test-backup--bytes (expand-file-name "database.sqlite-wal" snapshot) "bad"))
          ('link (delete-file file) (make-symbolic-link gnosis-test--db-file file))
          ('directory (delete-file file) (make-directory file)))
        (should-error (gnosis-backup-verify snapshot))
        (should-error (gnosis-backup-restore snapshot restore))
        (should-not (file-exists-p restore))
        (gnosis-test-backup--no-stage)))))

(ert-deftest gnosis-backup-rejects-manifest-duplicates-paths-and-versions ()
  (dolist (kind '(duplicate escape absolute collision version format extra-field))
    (gnosis-test-with-db
      (gnosis-test-backup--asset)
      (let ((snapshot (gnosis-backup-data (expand-file-name "snapshot" gnosis-dir))))
        (gnosis-test-backup--rewrite-manifest
         snapshot
         (lambda (data)
           (pcase kind
             ('duplicate (aset data 2 (vconcat (aref data 2) (list (aref (aref data 2) 0)))))
             ('escape (aset (aref (aref data 2) 0) 0 "../escape"))
             ('absolute (aset (aref (aref data 2) 0) 0 "/escape"))
             ('collision (aset (aref (aref data 2) 1) 0 "assets"))
             ('version (aset data 1 2))
             ('format (aset data 0 "unknown"))
             ('extra-field (setq data (vconcat data '("extra")))))
           data))
        (should-error (gnosis-backup-verify snapshot))))))

(ert-deftest gnosis-backup-manifest-and-inventory-bounds ()
  (gnosis-test-with-db
    (gnosis-test-backup--asset)
    (let ((snapshot (expand-file-name "snapshot" gnosis-dir)))
      (let ((gnosis-backup--entry-limit 1)) (should-error (gnosis-backup-data snapshot)))
      (let ((gnosis-backup--manifest-limit 1)) (should-error (gnosis-backup-data snapshot)))
      (gnosis-test-backup--no-stage)
      (gnosis-backup-data snapshot)
      (let ((gnosis-backup--manifest-limit 1)) (should-error (gnosis-backup-verify snapshot))))))

(ert-deftest gnosis-backup-physical-integrity-is-not-just-a-byte-hash ()
  (dolist (bytes (list "" "Not a database" (concat "SQLite format 3\0" (make-string 200 ?x))))
    (gnosis-test-with-db
      (let ((snapshot (gnosis-backup-data (expand-file-name "snapshot" gnosis-dir))))
        (gnosis-test-backup--bytes (expand-file-name "database.sqlite" snapshot) bytes)
        (delete-file (expand-file-name "manifest.json" snapshot))
        (gnosis-backup--write-manifest snapshot)
        (should-error (gnosis-backup-verify snapshot))))))

(ert-deftest gnosis-backup-restore-overlap-refusal-before-staging ()
  (gnosis-test-with-db
    (let ((snapshot (gnosis-backup-data (expand-file-name "snapshot" gnosis-dir))))
      (dolist (destination (list snapshot (expand-file-name "child" snapshot)
                                 (expand-file-name "assets/new" snapshot)
                                 (expand-file-name "assets" gnosis-dir) gnosis-dir))
        (cl-letf (((symbol-function 'make-temp-file)
                   (lambda (&rest _) (ert-fail "Staged overlapping restore"))))
          (should-error (gnosis-backup-restore snapshot destination) :type 'user-error)))
      (should (gnosis-backup-verify snapshot))
      (gnosis-test-backup--no-stage))))

(ert-deftest gnosis-backup-restore-source-drift-copy-failure-and-retry ()
  (dolist (kind '(add modify copy-error copy-quit rename-error rename-quit))
    (gnosis-test-with-db
      (gnosis-test-backup--asset)
      (let* ((snapshot (gnosis-backup-data (expand-file-name "snapshot" gnosis-dir)))
             (restore (expand-file-name "restore" gnosis-dir))
             (file (expand-file-name "assets/orphan/payload.bin" snapshot))
             (copy (symbol-function 'copy-file))
             (rename (symbol-function 'rename-file)))
        (cl-letf (((symbol-function 'copy-file)
                   (lambda (from to &rest args)
                     (prog1 (apply copy from to args)
                       (when (equal from file)
                         (pcase kind
                           ('add (gnosis-test-backup--bytes (expand-file-name "assets/late" snapshot) "late"))
                           ('modify (gnosis-test-backup--bytes file "changed"))
                           ('copy-error (error "Injected copy failure"))
                           ('copy-quit (signal 'quit nil)))))))
                  ((symbol-function 'rename-file)
                   (lambda (&rest args)
                     (pcase kind
                       ('rename-error (error "Injected rename failure"))
                       ('rename-quit (signal 'quit nil))
                       (_ (apply rename args))))))
          (should (memq (condition-case err (gnosis-backup-restore snapshot restore)
                          ((error quit) (car err))) '(error user-error quit))))
        (should-not (file-exists-p restore))
        (gnosis-test-backup--no-stage)
        (let ((fresh (gnosis-backup-data (expand-file-name "fresh" gnosis-dir))))
          (should (gnosis-backup-restore fresh (expand-file-name "retry" gnosis-dir))))))))

(ert-deftest gnosis-backup-restored-model-and-durable-evidence ()
  (gnosis-test-with-db
    (save-window-excursion
      (let* ((scene (expand-file-name "source/scene.json" gnosis-dir))
             (gnosis-center-content nil)
             (gnosis-review-buffer-name "*Gnosis Backup Test*")
             (buffers (buffer-list)))
        (unwind-protect
            (progn
              (gnosis-test-backup--bytes (expand-file-name "source/triangle.obj" gnosis-dir)
                                         "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n")
              (gnosis-test-backup--bytes scene "{\"objects\":[{\"id\":\"triangle\",\"label\":\"Triangle\",\"path\":\"triangle.obj\"},{\"id\":\"other\",\"label\":\"Other\",\"path\":\"triangle.obj\"}],\"initial_view\":[0,-90,1],\"license\":\"CC0\",\"source\":\"Original fixture\"}")
              (let* ((reference (gnosis-model-import scene))
                     (model-id (gnosis-generate-id))
                     (id (gnosis-test--add-basic-thema "Question" "Answer"))
                     (owner gnosis-db))
                (gnosis-add-thema-fields "model" "Select triangle"
                                        (list reference "0" "-90" "1") '("triangle")
                                        "Original geometry" '("test") 0 nil nil model-id)
                (gnosis-scheduler-accept-review (make-string 64 ?a) id 'success 1000000 (gnosis--today-int))
                (with-current-buffer (gnosis-review--setup-buffer (list id) 'practice)
                  (setf (gnosis-review-state-persistent-p gnosis-review--state) t)
                  (gnosis-review--save-session gnosis-review--state)
                  (gnosis-review-result id t (gnosis-review-algorithm id t)))
                (should (gnosis-select '* 'review-events))
                (should (gnosis-select '* 'practice-events))
                (let* ((snapshot (gnosis-backup-data (expand-file-name "snapshot" gnosis-dir)))
                       (restore (gnosis-backup-restore snapshot (expand-file-name "restore" gnosis-dir)))
                       (copy (sqlite-open (expand-file-name "database.sqlite" restore))))
                  (unwind-protect
                      (progn
                        (dolist (row (sqlite-select owner "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"))
                          (let ((sql (format "SELECT * FROM \"%s\" ORDER BY 1" (car row))))
                            (should (equal (sqlite-select owner sql) (sqlite-select copy sql)))))
                        (let ((gnosis-db copy) (gnosis-dir "/wrong-config"))
                          (should (equal (expand-file-name "assets" restore) (gnosis-assets-root)))
                          (should (gnosis-model-resolve
                                   (gnosis-get 'hypothesis 'themata `(= id ,model-id))
                                   (gnosis-get 'answer 'themata `(= id ,model-id)))))
                        (should (eq owner gnosis-db)))
                    (sqlite-close copy)))))
          (dolist (buffer (seq-difference (buffer-list) buffers))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest gnosis-backup-wal-state-and-offline-verification-preservation ()
  (gnosis-test-with-db
    (sqlite-execute gnosis-db "PRAGMA journal_mode = WAL")
    (sqlite-execute gnosis-db "PRAGMA wal_autocheckpoint = 0")
    (gnosis-test--add-basic-thema "Committed WAL question" "Answer")
    (let* ((wal (concat gnosis-test--db-file "-wal"))
           (hash (gnosis-assets-hash wal))
           (snapshot (gnosis-backup-data (expand-file-name "snapshot" gnosis-dir)))
           (before (gnosis-backup--inventory snapshot)))
      (should (equal hash (gnosis-assets-hash wal)))
      (let ((gnosis-db nil)) (should (gnosis-backup-verify snapshot)))
      (should (equal before (gnosis-backup--inventory snapshot)))
      (let ((copy (sqlite-open (expand-file-name "database.sqlite" snapshot))))
        (unwind-protect
            (should (equal (sqlite-select gnosis-db "SELECT * FROM themata")
                           (sqlite-select copy "SELECT * FROM themata")))
          (sqlite-close copy))))))

(ert-deftest gnosis-backup-foreign-key-violations-are-not-blessed-by-hashes ()
  (gnosis-test-with-db
    (sqlite-execute gnosis-db "PRAGMA foreign_keys = OFF")
    (sqlite-execute gnosis-db "CREATE TABLE backup_parent (id PRIMARY KEY)")
    (sqlite-execute gnosis-db "CREATE TABLE backup_child (id REFERENCES backup_parent(id))")
    (sqlite-execute gnosis-db "INSERT INTO backup_child VALUES (1)")
    (let ((snapshot (expand-file-name "snapshot" gnosis-dir)))
      (should-error (gnosis-backup-data snapshot))
      (should-not (file-exists-p snapshot))
      (should (equal '((1)) (sqlite-select gnosis-db "SELECT * FROM backup_child")))
      (gnosis-test-backup--no-stage))))

(ert-deftest gnosis-backup-manifest-rejects-trailing-data-and-malformed-json ()
  (dolist (suffix '(" {}" " garbage" " []"))
    (gnosis-test-with-db
      (let* ((snapshot (gnosis-backup-data (expand-file-name "snapshot" gnosis-dir)))
             (file (expand-file-name "manifest.json" snapshot)))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-max))
          (insert suffix)
          (write-region (point-min) (point-max) file nil 'silent))
        (should-error (gnosis-backup-verify snapshot)))))
  (gnosis-test-with-db
    (let ((snapshot (gnosis-backup-data (expand-file-name "snapshot" gnosis-dir))))
      (gnosis-test-backup--bytes (expand-file-name "manifest.json" snapshot) "[")
      (should-error (gnosis-backup-verify snapshot)))))

(ert-deftest gnosis-backup-public-interactive-dispatch-and-cancel ()
  (gnosis-test-with-db
    (let ((snapshot (expand-file-name "snapshot" gnosis-dir))
          (restore (expand-file-name "restore" gnosis-dir)))
      (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) snapshot)))
        (should (equal snapshot (call-interactively #'gnosis-backup-data)))
        (should (call-interactively #'gnosis-backup-verify)))
      (let ((answers (list snapshot restore)))
        (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) (pop answers))))
          (should (equal restore (call-interactively #'gnosis-backup-restore))))))
    (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) (signal 'quit nil))))
      (should (eq 'quit (condition-case err (call-interactively #'gnosis-backup-data)
                         (quit (car err))))))
    (gnosis-test-backup--no-stage)))

(ert-deftest gnosis-backup-legacy-external-paths-remain-external ()
  (gnosis-test-with-db
    (let* ((external (expand-file-name "legacy.png" gnosis-dir))
           (text (format "Question [[file:%s]]" external))
           (id (gnosis-test--add-basic-thema text "Answer")))
      (gnosis-test-backup--bytes external "External legacy bytes")
      (gnosis-update 'extras `(= review-image ,external) `(= id ,id))
      (let* ((snapshot (gnosis-backup-data (expand-file-name "snapshot" gnosis-dir)))
             (restore (gnosis-backup-restore snapshot (expand-file-name "restore" gnosis-dir)))
             (copy (sqlite-open (expand-file-name "database.sqlite" restore))))
        (unwind-protect
            (let ((gnosis-db copy))
              (should (equal text (gnosis-get 'keimenon 'themata `(= id ,id))))
              (should (equal external (gnosis-get 'review-image 'extras `(= id ,id))))
              (should-not (directory-files (expand-file-name "assets" restore) nil
                                           directory-files-no-dot-files-regexp))
              (should-not (file-exists-p (expand-file-name "legacy.png" restore))))
          (sqlite-close copy))))))

(ert-deftest gnosis-backup-refuses-open-transaction-without-publication ()
  (gnosis-test-with-db
    (let ((snapshot (expand-file-name "snapshot" gnosis-dir)))
      (sqlite-execute gnosis-db "BEGIN")
      (unwind-protect
          (progn
            (should-error (gnosis-backup-data snapshot))
            (should-not (file-exists-p snapshot))
            (gnosis-test-backup--no-stage)
            (should (equal '((1)) (sqlite-select gnosis-db "SELECT 1"))))
        (sqlite-execute gnosis-db "ROLLBACK")))))

(ert-deftest gnosis-backup-rejects-earlier-copied-file-corruption ()
  (gnosis-test-with-db
    (gnosis-test-backup--asset)
    (gnosis-test-backup--bytes (expand-file-name "assets/alpha.bin" gnosis-dir) "First")
    (let ((snapshot (expand-file-name "snapshot" gnosis-dir))
          (copy (symbol-function 'copy-file)) first-copy)
      (cl-letf (((symbol-function 'copy-file)
                 (lambda (from to &rest args)
                   (prog1 (apply copy from to args)
                     (cond
                      ((string-suffix-p "/alpha.bin" from) (setq first-copy to))
                      ((and first-copy (string-suffix-p "/payload.bin" from))
                       (gnosis-test-backup--bytes first-copy "Corrupt after its hash check")))))))
        (should-error (gnosis-backup-data snapshot)))
      (should-not (file-exists-p snapshot))
      (gnosis-test-backup--no-stage))))

(ert-deftest gnosis-backup-pins-vacuum-output-before-copy-callbacks ()
  (gnosis-test-with-db
    (let ((asset (gnosis-test-backup--asset))
          (snapshot (expand-file-name "snapshot" gnosis-dir))
          (copy (symbol-function 'copy-file)))
      (cl-letf (((symbol-function 'copy-file)
                 (lambda (from to &rest args)
                   (prog1 (apply copy from to args)
                     (when (equal from asset)
                       (let ((db (sqlite-open
                                  (expand-file-name "database.sqlite"
                                                    (locate-dominating-file to "database.sqlite")))))
                         (unwind-protect
                             (sqlite-execute db "CREATE TABLE injected (id)")
                           (sqlite-close db))))))))
        (should-error (gnosis-backup-data snapshot)))
      (should-not (file-exists-p snapshot))
      (gnosis-test-backup--no-stage))))

(provide 'gnosis-test-backup)
;;; gnosis-test-backup.el ends here
