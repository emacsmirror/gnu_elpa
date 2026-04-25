;;; forgejo-test-db.el --- Tests for forgejo-db  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for SQLite cache: schema creation, issue upsert/query,
;; timeline storage, label/milestone operations, sync state, filters.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "../lisp/forgejo.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/forgejo-db.el"
       (file-name-directory (or load-file-name buffer-file-name))))

(defvar forgejo-test-db--temp-dir nil)

(defun forgejo-test-db--setup ()
  "Create a temp directory and initialize the database."
  (setq forgejo-test-db--temp-dir (make-temp-file "forgejo-test" t))
  (setq forgejo-db-dir forgejo-test-db--temp-dir)
  (setq forgejo-db nil)
  (forgejo-db--ensure))

(defun forgejo-test-db--teardown ()
  "Close database and remove temp directory."
  (forgejo-db--close)
  (when (and forgejo-test-db--temp-dir
             (file-directory-p forgejo-test-db--temp-dir))
    (delete-directory forgejo-test-db--temp-dir t))
  (setq forgejo-test-db--temp-dir nil))

(defun forgejo-test-db--sample-issue (&optional overrides)
  "Return a fresh sample issue alist, optionally merged with OVERRIDES."
  (let ((base (list (cons 'id 100)
                    (cons 'number 42)
                    (cons 'title "Fix login bug")
                    (cons 'state "open")
                    (cons 'body "The login page crashes.")
                    (cons 'user (list (cons 'login "alice")))
                    (cons 'labels (list (list (cons 'id 1)
                                              (cons 'name "bug")
                                              (cons 'color "d73a4a"))))
                    (cons 'milestone (list (cons 'title "v1.0")))
                    (cons 'assignees (list (list (cons 'login "bob"))))
                    (cons 'comments 3)
                    (cons 'created_at "2026-01-01T00:00:00Z")
                    (cons 'updated_at "2026-04-15T12:00:00Z")
                    (cons 'closed_at nil)
                    (cons 'pull_request nil))))
    (dolist (pair overrides base)
      (setf (alist-get (car pair) base) (cdr pair)))))

;;; ---- Group 1: Schema ----

(ert-deftest forgejo-test-db-schema-creation ()
  "Database schema creates without error."
  (forgejo-test-db--setup)
  (unwind-protect
      (should (sqlitep forgejo-db))
    (forgejo-test-db--teardown)))

(ert-deftest forgejo-test-db-reopen ()
  "Re-opening the database reuses the existing file."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((path (expand-file-name "forgejo.db" forgejo-db-dir)))
        (should (file-exists-p path))
        (forgejo-db--close)
        (forgejo-db--ensure)
        (should (sqlitep forgejo-db)))
    (forgejo-test-db--teardown)))

;;; ---- Group 2: Issues ----

(ert-deftest forgejo-test-db-save-and-get-issue ()
  "Save an issue and retrieve it."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((issue (forgejo-test-db--sample-issue)))
        (forgejo-db-save-issues "codeberg.org" "owner" "repo" (list issue))
        (let* ((rows (forgejo-db-get-issues "codeberg.org" "owner" "repo"))
               (alist (forgejo-db--row-to-issue-alist (car rows))))
          (should (= (length rows) 1))
          (should (= (alist-get 'number alist) 42))))
    (forgejo-test-db--teardown)))

(ert-deftest forgejo-test-db-upsert-issue ()
  "Upserting an issue with same key replaces it."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((issue1 (forgejo-test-db--sample-issue))
            (issue2 (forgejo-test-db--sample-issue
                     '((title . "Fixed login bug") (state . "closed")))))
        (forgejo-db-save-issues "codeberg.org" "owner" "repo" (list issue1))
        (forgejo-db-save-issues "codeberg.org" "owner" "repo" (list issue2))
        (let* ((rows (forgejo-db-get-issues "codeberg.org" "owner" "repo"))
               (alist (forgejo-db--row-to-issue-alist (car rows))))
          (should (= (length rows) 1))
          (should (string= (alist-get 'title alist) "Fixed login bug"))))
    (forgejo-test-db--teardown)))

(ert-deftest forgejo-test-db-filter-by-state ()
  "Filter issues by state."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((open-issue (forgejo-test-db--sample-issue))
            (closed-issue (forgejo-test-db--sample-issue
                           '((id . 101) (number . 43)
                             (state . "closed")
                             (title . "Old bug")))))
        (forgejo-db-save-issues "codeberg.org" "owner" "repo"
                                (list open-issue closed-issue))
        (let ((open (forgejo-db-get-issues "codeberg.org" "owner" "repo"
                                           '(:state "open")))
              (closed (forgejo-db-get-issues "codeberg.org" "owner" "repo"
                                             '(:state "closed"))))
          (should (= (length open) 1))
          (should (= (length closed) 1))))
    (forgejo-test-db--teardown)))

(ert-deftest forgejo-test-db-filter-by-query ()
  "Filter issues by title search."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((issue1 (forgejo-test-db--sample-issue))
            (issue2 (forgejo-test-db--sample-issue
                     '((id . 101) (number . 43)
                       (title . "Add dark theme")))))
        (forgejo-db-save-issues "codeberg.org" "owner" "repo"
                                (list issue1 issue2))
        (let ((results (forgejo-db-get-issues "codeberg.org" "owner" "repo"
                                              '(:query "login"))))
          (should (= (length results) 1))))
    (forgejo-test-db--teardown)))

(ert-deftest forgejo-test-db-filter-pulls ()
  "Filter pull requests vs issues."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((issue (forgejo-test-db--sample-issue))
            (pr (forgejo-test-db--sample-issue
                 '((id . 101) (number . 43)
                   (title . "Feature PR")
                   (pull_request . ((merged . :false)))))))
        (forgejo-db-save-issues "codeberg.org" "owner" "repo"
                                (list issue pr))
        (let ((pulls (forgejo-db-get-issues "codeberg.org" "owner" "repo"
                                            '(:is-pull t)))
              (issues (forgejo-db-get-issues "codeberg.org" "owner" "repo"
                                             '(:no-pulls t))))
          (should (= (length pulls) 1))
          (should (= (length issues) 1))))
    (forgejo-test-db--teardown)))

;;; ---- Group 3: Timeline ----

(ert-deftest forgejo-test-db-save-and-get-timeline ()
  "Save timeline events and retrieve them."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((events `(((id . 1) (type . "comment")
                       (body . "Looks good") (user . ((login . "alice")))
                       (created_at . "2026-01-02T00:00:00Z"))
                      ((id . 2) (type . "close")
                       (body) (user . ((login . "bob")))
                       (created_at . "2026-01-03T00:00:00Z")))))
        (forgejo-db-save-timeline "codeberg.org" "owner" "repo" 42 events)
        (let* ((rows (forgejo-db-get-timeline "codeberg.org" "owner" "repo" 42))
               (alist (forgejo-db--row-to-timeline-alist (car rows))))
          (should (= (length rows) 2))
          ;; Ordered by created_at ASC
          (should (string= (alist-get 'type alist) "comment"))))
    (forgejo-test-db--teardown)))

;;; ---- Group 4: Labels ----

(ert-deftest forgejo-test-db-save-and-get-labels ()
  "Save labels and retrieve them."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((labels `(((id . 1) (name . "bug") (color . "d73a4a")
                       (description . "Something broken"))
                      ((id . 2) (name . "enhancement") (color . "a2eeef")
                       (description . "New feature")))))
        (forgejo-db-save-labels "codeberg.org" "owner" "repo" labels)
        (let ((rows (forgejo-db-get-labels "codeberg.org" "owner" "repo")))
          (should (= (length rows) 2))
          ;; Ordered by name
          (should (string= (nth 4 (car rows)) "bug"))))
    (forgejo-test-db--teardown)))

;;; ---- Group 5: Milestones ----

(ert-deftest forgejo-test-db-save-and-get-milestones ()
  "Save milestones and retrieve them."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((milestones `(((id . 1) (title . "v1.0") (state . "open")
                           (open_issues . 5) (closed_issues . 3))
                          ((id . 2) (title . "v2.0") (state . "open")
                           (open_issues . 10) (closed_issues . 0)))))
        (forgejo-db-save-milestones "codeberg.org" "owner" "repo" milestones)
        (let ((rows (forgejo-db-get-milestones "codeberg.org" "owner" "repo")))
          (should (= (length rows) 2))))
    (forgejo-test-db--teardown)))

;;; ---- Group 6: Sync state ----

(ert-deftest forgejo-test-db-sync-state ()
  "Set and get sync timestamps."
  (forgejo-test-db--setup)
  (unwind-protect
      (progn
        (should (null (forgejo-db-get-sync-time
                       "codeberg.org" "owner" "repo" "issues")))
        (forgejo-db-set-sync-time
         "codeberg.org" "owner" "repo" "issues" "2026-04-15T12:00:00Z")
        (should (string= (forgejo-db-get-sync-time
                           "codeberg.org" "owner" "repo" "issues")
                          "2026-04-15T12:00:00Z")))
    (forgejo-test-db--teardown)))

(ert-deftest forgejo-test-db-sync-state-update ()
  "Updating sync time replaces the old value."
  (forgejo-test-db--setup)
  (unwind-protect
      (progn
        (forgejo-db-set-sync-time
         "codeberg.org" "owner" "repo" "issues" "2026-01-01T00:00:00Z")
        (forgejo-db-set-sync-time
         "codeberg.org" "owner" "repo" "issues" "2026-04-20T00:00:00Z")
        (should (string= (forgejo-db-get-sync-time
                           "codeberg.org" "owner" "repo" "issues")
                          "2026-04-20T00:00:00Z")))
    (forgejo-test-db--teardown)))

;;; ---- Group 7: JSON helpers ----

(ert-deftest forgejo-test-db-json-roundtrip ()
  "JSON encode/decode roundtrips correctly."
  (let* ((data '(((name . "bug") (color . "red"))
                 ((name . "feature") (color . "blue"))))
         (encoded (forgejo-db--encode-json data))
         (decoded (forgejo-db--decode-json encoded)))
    (should (= (length decoded) 2))
    (should (string= (alist-get 'name (car decoded)) "bug"))))

(ert-deftest forgejo-test-db-json-nil ()
  "Nil encodes to \"null\" and decodes back to nil."
  (should (string= (forgejo-db--encode-json nil) "null"))
  (should (null (forgejo-db--decode-json "null")))
  (should (null (forgejo-db--decode-json nil))))

;;; ---- Group 8: :null handling (real API data shape) ----

(ert-deftest forgejo-test-db-nullable ()
  "Convert :null and :false to nil."
  (should (null (forgejo-db--nullable :null)))
  (should (null (forgejo-db--nullable :false)))
  (should (string= (forgejo-db--nullable "open") "open"))
  (should (= (forgejo-db--nullable 42) 42))
  (should (null (forgejo-db--nullable nil))))

(ert-deftest forgejo-test-db-save-issue-with-nulls ()
  "Save an issue where API returns :null for optional fields."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((issue (list (cons 'id 200)
                         (cons 'number 10)
                         (cons 'title "Test with nulls")
                         (cons 'state "open")
                         (cons 'body "body text")
                         (cons 'user (list (cons 'login "alice")))
                         (cons 'labels :null)
                         (cons 'milestone :null)
                         (cons 'assignees :null)
                         (cons 'comments 0)
                         (cons 'created_at "2026-01-01T00:00:00Z")
                         (cons 'updated_at "2026-04-15T12:00:00Z")
                         (cons 'closed_at :null)
                         (cons 'pull_request :null))))
        (forgejo-db-save-issues "codeberg.org" "owner" "repo" (list issue))
        (let* ((rows (forgejo-db-get-issues "codeberg.org" "owner" "repo"))
               (alist (forgejo-db--row-to-issue-alist (car rows))))
          (should (= (length rows) 1))
          (should (string= (alist-get 'title alist) "Test with nulls"))
          (should (null (alist-get 'milestone alist)))))
    (forgejo-test-db--teardown)))

(ert-deftest forgejo-test-db-save-pr-with-nulls ()
  "Save a PR where assignees/milestone are :null but labels exist."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((pr (list (cons 'id 300)
                      (cons 'number 20)
                      (cons 'title "Feature PR")
                      (cons 'state "open")
                      (cons 'body :null)
                      (cons 'user (list (cons 'login "bob")))
                      (cons 'labels (list (list (cons 'id 1)
                                                (cons 'name "bug")
                                                (cons 'color "ff0000"))))
                      (cons 'milestone :null)
                      (cons 'assignees :null)
                      (cons 'comments 5)
                      (cons 'created_at "2026-02-01T00:00:00Z")
                      (cons 'updated_at "2026-04-10T00:00:00Z")
                      (cons 'closed_at :null)
                      (cons 'pull_request (list (cons 'merged :false))))))
        (forgejo-db-save-issues "codeberg.org" "owner" "repo" (list pr))
        (let ((rows (forgejo-db-get-issues "codeberg.org" "owner" "repo"
                                           '(:is-pull t))))
          (should (= (length rows) 1))))
    (forgejo-test-db--teardown)))

(ert-deftest forgejo-test-db-save-timeline-with-nulls ()
  "Save timeline events with :null fields."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((events (list (list (cons 'id 1)
                                (cons 'type "comment")
                                (cons 'body "text")
                                (cons 'user (list (cons 'login "alice")))
                                (cons 'created_at "2026-01-02T00:00:00Z"))
                          (list (cons 'id 2)
                                (cons 'type "label")
                                (cons 'body :null)
                                (cons 'user :null)
                                (cons 'created_at "2026-01-03T00:00:00Z")))))
        (forgejo-db-save-timeline "codeberg.org" "owner" "repo" 42 events)
        (let ((rows (forgejo-db-get-timeline "codeberg.org" "owner" "repo" 42)))
          (should (= (length rows) 2))))
    (forgejo-test-db--teardown)))

;;; ---- Group 9: Row-to-alist conversion ----

(ert-deftest forgejo-test-db-row-to-issue-alist ()
  "Convert a DB row back to an API-shaped alist."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((issue (list (cons 'id 100)
                         (cons 'number 42)
                         (cons 'title "Test issue")
                         (cons 'state "open")
                         (cons 'body "Body text")
                         (cons 'user (list (cons 'login "alice")))
                         (cons 'labels (list (list (cons 'id 1)
                                                   (cons 'name "bug")
                                                   (cons 'color "ff0000"))))
                         (cons 'milestone (list (cons 'title "v1.0")))
                         (cons 'assignees (list (list (cons 'login "bob"))))
                         (cons 'comments 3)
                         (cons 'created_at "2026-01-01T00:00:00Z")
                         (cons 'updated_at "2026-04-15T12:00:00Z")
                         (cons 'closed_at nil)
                         (cons 'pull_request nil))))
        (forgejo-db-save-issues "codeberg.org" "owner" "repo" (list issue))
        (let* ((rows (forgejo-db-get-issues "codeberg.org" "owner" "repo"))
               (alist (forgejo-db--row-to-issue-alist (car rows))))
          (should (= (alist-get 'number alist) 42))
          (should (string= (alist-get 'title alist) "Test issue"))
          (should (string= (alist-get 'state alist) "open"))
          (should (string= (alist-get 'login (alist-get 'user alist)) "alice"))
          (should (= (alist-get 'comments alist) 3))
          (should (string= (alist-get 'updated_at alist) "2026-04-15T12:00:00Z"))))
    (forgejo-test-db--teardown)))

(ert-deftest forgejo-test-db-row-to-issue-alist-with-nulls ()
  "Row-to-alist handles nil/missing fields gracefully."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((issue (list (cons 'id 200)
                         (cons 'number 10)
                         (cons 'title "Null test")
                         (cons 'state "open")
                         (cons 'body :null)
                         (cons 'user (list (cons 'login "bob")))
                         (cons 'labels :null)
                         (cons 'milestone :null)
                         (cons 'assignees :null)
                         (cons 'comments 0)
                         (cons 'created_at "2026-01-01T00:00:00Z")
                         (cons 'updated_at "2026-04-15T12:00:00Z")
                         (cons 'closed_at :null)
                         (cons 'pull_request :null))))
        (forgejo-db-save-issues "codeberg.org" "owner" "repo" (list issue))
        (let* ((rows (forgejo-db-get-issues "codeberg.org" "owner" "repo"))
               (alist (forgejo-db--row-to-issue-alist (car rows))))
          (should (= (alist-get 'number alist) 10))
          (should (null (alist-get 'milestone alist)))
          (should (null (alist-get 'pull_request alist)))))
    (forgejo-test-db--teardown)))

;;; ---- Group 10: Host lookup ----

(ert-deftest forgejo-test-db-get-hosts-for-repo ()
  "Return distinct hosts that have cached data for a repo."
  (forgejo-test-db--setup)
  (unwind-protect
      (let ((issue-a (list (cons 'id 1) (cons 'number 1)
                           (cons 'title "A") (cons 'state "open")
                           (cons 'user (list (cons 'login "alice")))
                           (cons 'updated_at "2026-01-01T00:00:00Z")))
            (issue-b (list (cons 'id 2) (cons 'number 2)
                           (cons 'title "B") (cons 'state "open")
                           (cons 'user (list (cons 'login "bob")))
                           (cons 'updated_at "2026-01-02T00:00:00Z"))))
        (forgejo-db-save-issues "codeberg.org" "owner" "repo" (list issue-a))
        (forgejo-db-save-issues "git.myorg.com" "owner" "repo" (list issue-b))
        (let ((hosts (forgejo-db-get-hosts-for-repo "owner" "repo")))
          (should (= (length hosts) 2))
          (should (member "codeberg.org" hosts))
          (should (member "git.myorg.com" hosts)))
        ;; Different repo returns only its host
        (forgejo-db-save-issues "codeberg.org" "other" "project"
                                (list (list (cons 'id 3) (cons 'number 1)
                                            (cons 'title "C") (cons 'state "open")
                                            (cons 'user (list (cons 'login "x")))
                                            (cons 'updated_at "2026-01-03T00:00:00Z"))))
        (let ((hosts (forgejo-db-get-hosts-for-repo "other" "project")))
          (should (= (length hosts) 1))
          (should (string= (car hosts) "codeberg.org"))))
    (forgejo-test-db--teardown)))

(provide 'forgejo-test-db)
;;; forgejo-test-db.el ends here
