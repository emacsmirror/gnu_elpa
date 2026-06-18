;;; forgejo-test-helper.el --- Shared Forgejo test helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared ERT helpers and fixtures for Forgejo tests.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cl-lib)

(defvar forgejo-markdown-mode)

(setq forgejo-markdown-mode 'text-mode)

(defmacro forgejo-test-with-temp-db (&rest body)
  "Run BODY with a fresh Forgejo database in a temporary directory."
  (declare (indent 0) (debug t))
  (let ((dir (make-symbol "forgejo-test-db-dir")))
    `(ert-with-temp-directory ,dir
       (let ((forgejo-db-dir ,dir)
             (forgejo-db nil))
         (unwind-protect
             (progn
               (forgejo-db--ensure)
               ,@body)
           (forgejo-db--close))))))

(defun forgejo-test-alist-merge (alist overrides)
  "Return ALIST with OVERRIDES merged by key."
  (let ((copy (copy-tree alist)))
    (dolist (pair overrides copy)
      (setf (alist-get (car pair) copy) (cdr pair)))))

(defun forgejo-test-label (&optional overrides)
  "Return a label alist merged with OVERRIDES."
  (forgejo-test-alist-merge
   '((id . 1)
     (name . "bug")
     (color . "d73a4a")
     (description . "Something broken"))
   overrides))

(defun forgejo-test-issue (&optional overrides)
  "Return an issue alist merged with OVERRIDES."
  (forgejo-test-alist-merge
   `((id . 100)
     (number . 42)
     (title . "Fix login bug")
     (state . "open")
     (body . "The login page crashes.")
     (user . ((login . "alice")))
     (labels . (,(forgejo-test-label)))
     (milestone . ((title . "v1.0")))
     (assignees . (((login . "bob"))))
     (comments . 3)
     (created_at . "2026-01-01T00:00:00Z")
     (updated_at . "2026-04-15T12:00:00Z")
     (closed_at)
     (pull_request))
   overrides))

(defun forgejo-test-pr (&optional overrides)
  "Return a pull request issue alist merged with OVERRIDES."
  (forgejo-test-issue
   (append '((id . 101)
             (number . 43)
             (title . "Feature PR")
             (pull_request . ((merged . :false))))
           overrides)))

(defun forgejo-test-detail-issue (&optional overrides)
  "Return a compact issue alist merged with OVERRIDES."
  (forgejo-test-issue
   (append '((number . 1)
             (title . "Test issue")
             (body . "")
             (updated_at . "2026-01-01T00:00:00Z"))
           overrides)))

(defun forgejo-test-detail-pr (&optional overrides)
  "Return a compact pull request alist merged with OVERRIDES."
  (forgejo-test-pr
   (append '((number . 1)
             (title . "Test PR")
             (body . "")
             (pull_request . t)
             (updated_at . "2026-01-01T00:00:00Z"))
           overrides)))

(defun forgejo-test-comment (id &optional overrides)
  "Return a timeline comment alist for ID merged with OVERRIDES."
  (forgejo-test-alist-merge
   `((id . ,id)
     (type . "comment")
     (body . ,(format "Comment %d" id))
     (user . ((login . "commenter")))
     (created_at . "2026-01-01T00:00:00Z")
     (updated_at . "2026-01-01T00:00:00Z"))
   overrides))

(defun forgejo-test-timeline (&rest ids)
  "Return comment timeline events for IDS."
  (mapcar #'forgejo-test-comment ids))

(defun forgejo-test-reaction (content user &optional overrides)
  "Return a reaction alist for CONTENT by USER merged with OVERRIDES."
  (forgejo-test-alist-merge
   `((content . ,content)
     (user . ((login . ,user)))
     (created_at . "2026-01-01T00:00:00Z"))
   overrides))

(provide 'forgejo-test-helper)
;;; forgejo-test-helper.el ends here
