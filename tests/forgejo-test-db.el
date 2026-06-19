;;; forgejo-test-db.el --- Tests for forgejo-db  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for SQLite cache: schema creation, issue upsert/query,
;; timeline storage, label/milestone operations, sync state, filters.

;;; Code:

(require 'forgejo-test-helper)
(require 'forgejo)
(require 'forgejo-db)

;;; Group 1: Schema

(ert-deftest forgejo-test-db-schema-creation ()
  "Database schema creates without error."
  (forgejo-test-with-temp-db
   (should (sqlitep forgejo-db))))

(ert-deftest forgejo-test-db-reopen ()
  "Re-opening the database reuses the existing file."
  (forgejo-test-with-temp-db
   (let ((path (expand-file-name "forgejo.db" forgejo-db-dir)))
     (should (file-exists-p path))
     (forgejo-db--close)
     (forgejo-db--ensure)
     (should (sqlitep forgejo-db)))))

;;; Group 2: Issues

(ert-deftest forgejo-test-db-save-and-get-issue ()
  "Save an issue and retrieve it."
  (forgejo-test-with-temp-db
   (forgejo-db-save-issues "codeberg.org" "owner" "repo"
                           (list (forgejo-test-issue)))
   (let* ((rows (forgejo-db-get-issues "codeberg.org" "owner" "repo"))
          (alist (forgejo-db--row-to-issue-alist (car rows))))
     (should (= (length rows) 1))
     (should (= (alist-get 'number alist) 42)))))

(ert-deftest forgejo-test-db-upsert-issue ()
  "Upserting an issue with same key replaces it."
  (forgejo-test-with-temp-db
   (forgejo-db-save-issues "codeberg.org" "owner" "repo"
                           (list (forgejo-test-issue)))
   (forgejo-db-save-issues
    "codeberg.org" "owner" "repo"
    (list (forgejo-test-issue '((title . "Fixed login bug")
                                (state . "closed")))))
   (let* ((rows (forgejo-db-get-issues "codeberg.org" "owner" "repo"))
          (alist (forgejo-db--row-to-issue-alist (car rows))))
     (should (= (length rows) 1))
     (should (string= (alist-get 'title alist) "Fixed login bug")))))

(ert-deftest forgejo-test-db-filter-by-state ()
  "Filter issues by state."
  (forgejo-test-with-temp-db
   (forgejo-db-save-issues
    "codeberg.org" "owner" "repo"
    (list (forgejo-test-issue)
          (forgejo-test-issue '((id . 101) (number . 43)
                                (state . "closed")
                                (title . "Old bug")))))
   (let ((open (forgejo-db-get-issues "codeberg.org" "owner" "repo"
                                      '(:state "open")))
         (closed (forgejo-db-get-issues "codeberg.org" "owner" "repo"
                                        '(:state "closed"))))
     (should (= (length open) 1))
     (should (= (length closed) 1)))))

(ert-deftest forgejo-test-db-filter-by-query ()
  "Filter issues by title search."
  (forgejo-test-with-temp-db
   (forgejo-db-save-issues
    "codeberg.org" "owner" "repo"
    (list (forgejo-test-issue)
          (forgejo-test-issue '((id . 101) (number . 43)
                                (title . "Add dark theme")))))
   (let ((results (forgejo-db-get-issues "codeberg.org" "owner" "repo"
                                         '(:query "login"))))
     (should (= (length results) 1)))))

(ert-deftest forgejo-test-db-filter-pulls ()
  "Filter pull requests vs issues."
  (forgejo-test-with-temp-db
   (forgejo-db-save-issues "codeberg.org" "owner" "repo"
                           (list (forgejo-test-issue)
                                 (forgejo-test-pr)))
   (let ((pulls (forgejo-db-get-issues "codeberg.org" "owner" "repo"
                                       '(:is-pull t)))
         (issues (forgejo-db-get-issues "codeberg.org" "owner" "repo"
                                        '(:no-pulls t))))
     (should (= (length pulls) 1))
     (should (= (length issues) 1)))))

;;; Group 3: Timeline

(ert-deftest forgejo-test-db-save-and-get-timeline ()
  "Save timeline events and retrieve them."
  (forgejo-test-with-temp-db
   (forgejo-db-save-timeline
    "codeberg.org" "owner" "repo" 42
    (list (forgejo-test-comment 1 '((body . "Looks good")))
          (forgejo-test-comment 2 '((type . "close") (body)))))
   (let* ((rows (forgejo-db-get-timeline "codeberg.org" "owner" "repo" 42))
          (alist (forgejo-db--row-to-timeline-alist (car rows))))
     (should (= (length rows) 2))
     (should (string= (alist-get 'type alist) "comment")))))

;;; Group 4: Labels

(ert-deftest forgejo-test-db-save-and-get-labels ()
  "Save labels and retrieve them."
  (forgejo-test-with-temp-db
   (forgejo-db-save-labels
    "codeberg.org" "owner" "repo"
    (list (forgejo-test-label)
          (forgejo-test-label '((id . 2) (name . "enhancement")
                                (color . "a2eeef")
                                (description . "New feature")))))
   (let ((rows (forgejo-db-get-labels "codeberg.org" "owner" "repo")))
     (should (= (length rows) 2))
     (should (string= (nth 4 (car rows)) "bug")))))

;;; Group 5: Milestones

(ert-deftest forgejo-test-db-save-and-get-milestones ()
  "Save milestones and retrieve them."
  (forgejo-test-with-temp-db
   (forgejo-db-save-milestones
    "codeberg.org" "owner" "repo"
    '(((id . 1) (title . "v1.0") (state . "open")
       (open_issues . 5) (closed_issues . 3))
      ((id . 2) (title . "v2.0") (state . "open")
       (open_issues . 10) (closed_issues . 0))))
   (let ((rows (forgejo-db-get-milestones "codeberg.org" "owner" "repo")))
     (should (= (length rows) 2)))))

;;; Group 6: Sync state

(ert-deftest forgejo-test-db-sync-state ()
  "Set and get sync timestamps."
  (forgejo-test-with-temp-db
   (should (null (forgejo-db-get-sync-time
                  "codeberg.org" "owner" "repo" "issues")))
   (forgejo-db-set-sync-time
    "codeberg.org" "owner" "repo" "issues" "2026-04-15T12:00:00Z")
   (should (string= (forgejo-db-get-sync-time
                     "codeberg.org" "owner" "repo" "issues")
                    "2026-04-15T12:00:00Z"))))

(ert-deftest forgejo-test-db-sync-state-update ()
  "Updating sync time replaces the old value."
  (forgejo-test-with-temp-db
   (forgejo-db-set-sync-time
    "codeberg.org" "owner" "repo" "issues" "2026-01-01T00:00:00Z")
   (forgejo-db-set-sync-time
    "codeberg.org" "owner" "repo" "issues" "2026-04-20T00:00:00Z")
   (should (string= (forgejo-db-get-sync-time
                     "codeberg.org" "owner" "repo" "issues")
                    "2026-04-20T00:00:00Z"))))

;;; Group 7: JSON helpers

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

;;; Group 8: :null handling

(ert-deftest forgejo-test-db-nullable ()
  "Convert :null and :false to nil."
  (should (null (forgejo-db--nullable :null)))
  (should (null (forgejo-db--nullable :false)))
  (should (string= (forgejo-db--nullable "open") "open"))
  (should (= (forgejo-db--nullable 42) 42))
  (should (null (forgejo-db--nullable nil))))

(ert-deftest forgejo-test-db-save-issue-with-nulls ()
  "Save an issue where API returns :null for optional fields."
  (forgejo-test-with-temp-db
   (forgejo-db-save-issues
    "codeberg.org" "owner" "repo"
    (list (forgejo-test-issue '((id . 200)
                                (number . 10)
                                (title . "Test with nulls")
                                (body . "body text")
                                (labels . :null)
                                (milestone . :null)
                                (assignees . :null)
                                (comments . 0)
                                (closed_at . :null)
                                (pull_request . :null)))))
   (let* ((rows (forgejo-db-get-issues "codeberg.org" "owner" "repo"))
          (alist (forgejo-db--row-to-issue-alist (car rows))))
     (should (= (length rows) 1))
     (should (string= (alist-get 'title alist) "Test with nulls"))
     (should (null (alist-get 'milestone alist))))))

(ert-deftest forgejo-test-db-save-pr-with-nulls ()
  "Save a PR where assignees/milestone are :null but labels exist."
  (forgejo-test-with-temp-db
   (forgejo-db-save-issues
    "codeberg.org" "owner" "repo"
    (list (forgejo-test-pr '((id . 300)
                             (number . 20)
                             (body . :null)
                             (user . ((login . "bob")))
                             (labels . (((id . 1)
                                         (name . "bug")
                                         (color . "ff0000"))))
                             (milestone . :null)
                             (assignees . :null)
                             (comments . 5)
                             (closed_at . :null)))))
   (let ((rows (forgejo-db-get-issues "codeberg.org" "owner" "repo"
                                      '(:is-pull t))))
     (should (= (length rows) 1)))))

(ert-deftest forgejo-test-db-save-timeline-with-nulls ()
  "Save timeline events with :null fields."
  (forgejo-test-with-temp-db
   (forgejo-db-save-timeline
    "codeberg.org" "owner" "repo" 42
    (list (forgejo-test-comment 1 '((body . "text")
                                    (user . ((login . "alice")))
                                    (created_at . "2026-01-02T00:00:00Z")))
          (forgejo-test-comment 2 '((type . "label")
                                    (body . :null)
                                    (user . :null)
                                    (created_at . "2026-01-03T00:00:00Z")))))
   (let ((rows (forgejo-db-get-timeline "codeberg.org" "owner" "repo" 42)))
     (should (= (length rows) 2)))))

;;; Group 9: Row-to-alist conversion

(ert-deftest forgejo-test-db-row-to-issue-alist ()
  "Convert a DB row back to an API-shaped alist."
  (forgejo-test-with-temp-db
   (forgejo-db-save-issues "codeberg.org" "owner" "repo"
                           (list (forgejo-test-issue
                                  '((title . "Test issue")
                                    (body . "Body text")
                                    (labels . (((id . 1)
                                                (name . "bug")
                                                (color . "ff0000"))))))))
   (let* ((rows (forgejo-db-get-issues "codeberg.org" "owner" "repo"))
          (alist (forgejo-db--row-to-issue-alist (car rows))))
     (should (= (alist-get 'number alist) 42))
     (should (string= (alist-get 'title alist) "Test issue"))
     (should (string= (alist-get 'state alist) "open"))
     (should (string= (alist-get 'login (alist-get 'user alist)) "alice"))
     (should (= (alist-get 'comments alist) 3))
     (should (string= (alist-get 'updated_at alist)
                      "2026-04-15T12:00:00Z")))))

(ert-deftest forgejo-test-db-row-to-issue-alist-with-nulls ()
  "Row-to-alist handles nil/missing fields gracefully."
  (forgejo-test-with-temp-db
   (forgejo-db-save-issues
    "codeberg.org" "owner" "repo"
    (list (forgejo-test-issue '((id . 200)
                                (number . 10)
                                (title . "Null test")
                                (body . :null)
                                (user . ((login . "bob")))
                                (labels . :null)
                                (milestone . :null)
                                (assignees . :null)
                                (comments . 0)
                                (closed_at . :null)
                                (pull_request . :null)))))
   (let* ((rows (forgejo-db-get-issues "codeberg.org" "owner" "repo"))
          (alist (forgejo-db--row-to-issue-alist (car rows))))
     (should (= (alist-get 'number alist) 10))
     (should (null (alist-get 'milestone alist)))
     (should (null (alist-get 'pull_request alist))))))

;;; Group 10: Host lookup

(ert-deftest forgejo-test-db-get-hosts-for-repo ()
  "Return distinct hosts that have cached data for a repo."
  (forgejo-test-with-temp-db
   (forgejo-db-save-issues
    "codeberg.org" "owner" "repo"
    (list (forgejo-test-issue '((id . 1) (number . 1) (title . "A")))))
   (forgejo-db-save-issues
    "git.myorg.com" "owner" "repo"
    (list (forgejo-test-issue '((id . 2) (number . 2)
                                (title . "B")
                                (user . ((login . "bob")))
                                (updated_at . "2026-01-02T00:00:00Z")))))
   (let ((hosts (forgejo-db-get-hosts-for-repo "owner" "repo")))
     (should (= (length hosts) 2))
     (should (member "codeberg.org" hosts))
     (should (member "git.myorg.com" hosts)))
   (forgejo-db-save-issues
    "codeberg.org" "other" "project"
    (list (forgejo-test-issue '((id . 3) (number . 1)
                                (title . "C")
                                (user . ((login . "x")))
                                (updated_at . "2026-01-03T00:00:00Z")))))
   (let ((hosts (forgejo-db-get-hosts-for-repo "other" "project")))
     (should (= (length hosts) 1))
     (should (string= (car hosts) "codeberg.org")))))

;;; Group 11: Reactions

(ert-deftest forgejo-test-db-save-and-get-reactions ()
  "Save reactions and retrieve them grouped by comment-id and content."
  (forgejo-test-with-temp-db
   (forgejo-db-save-reactions
    "codeberg.org" "owner" "repo" 42 0
    (list (forgejo-test-reaction "+1" "alice")
          (forgejo-test-reaction "+1" "bob"
                                 '((created_at . "2026-01-01T01:00:00Z")))
          (forgejo-test-reaction "heart" "alice"
                                 '((created_at . "2026-01-01T02:00:00Z")))))
   (forgejo-db-save-reactions
    "codeberg.org" "owner" "repo" 42 100
    (list (forgejo-test-reaction "eyes" "charlie"
                                 '((created_at . "2026-01-02T00:00:00Z")))))
   (let ((all (forgejo-db-get-reactions "codeberg.org" "owner" "repo" 42)))
     (should (= (length all) 2))
     (let ((issue-r (cdr (assoc 0 all)))
           (comment-r (cdr (assoc 100 all))))
       (should (equal (cdr (assoc "+1" issue-r #'string=))
                      '("alice" "bob")))
       (should (equal (cdr (assoc "heart" issue-r #'string=))
                      '("alice")))
       (should (equal (cdr (assoc "eyes" comment-r #'string=))
                      '("charlie")))))))

(ert-deftest forgejo-test-db-save-reactions-replaces ()
  "Saving reactions replaces previous ones for the same comment-id."
  (forgejo-test-with-temp-db
   (forgejo-db-save-reactions
    "codeberg.org" "owner" "repo" 42 0
    (list (forgejo-test-reaction "+1" "alice")))
   (forgejo-db-save-reactions
    "codeberg.org" "owner" "repo" 42 0
    (list (forgejo-test-reaction "heart" "bob"
                                 '((created_at . "2026-01-02T00:00:00Z")))))
   (let* ((all (forgejo-db-get-reactions "codeberg.org" "owner" "repo" 42))
          (issue-r (cdr (assoc 0 all))))
     (should-not (assoc "+1" issue-r #'string=))
     (should (equal (cdr (assoc "heart" issue-r #'string=))
                    '("bob"))))))

(ert-deftest forgejo-test-db-save-reactions-empty ()
  "Saving empty reactions clears existing ones."
  (forgejo-test-with-temp-db
   (forgejo-db-save-reactions
    "codeberg.org" "owner" "repo" 42 0
    (list (forgejo-test-reaction "+1" "alice")))
   (forgejo-db-save-reactions "codeberg.org" "owner" "repo" 42 0 nil)
   (let ((all (forgejo-db-get-reactions "codeberg.org" "owner" "repo" 42)))
     (should (null all)))))

;;; Group 12: PR targets

(ert-deftest forgejo-test-db-pr-target ()
  "Remember and retrieve a PR target for a branch."
  (forgejo-test-with-temp-db
   (should (null (forgejo-db-get-pr-target
                  "codeberg.org" "owner" "repo" "topic")))
   (forgejo-db-set-pr-target
    "codeberg.org" "owner" "repo" "topic" "origin/master")
   (should (string= (forgejo-db-get-pr-target
                     "codeberg.org" "owner" "repo" "topic")
                    "origin/master"))))

(ert-deftest forgejo-test-db-pr-target-update ()
  "Updating a PR target replaces the old value."
  (forgejo-test-with-temp-db
   (forgejo-db-set-pr-target
    "codeberg.org" "owner" "repo" "topic" "origin/master")
   (forgejo-db-set-pr-target
    "codeberg.org" "owner" "repo" "topic" "origin/devel")
   (should (string= (forgejo-db-get-pr-target
                     "codeberg.org" "owner" "repo" "topic")
                    "origin/devel"))))

(provide 'forgejo-test-db)
;;; forgejo-test-db.el ends here
