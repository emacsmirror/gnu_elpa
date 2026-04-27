;;; forgejo-review.el --- Review data operations for Forgejo  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions

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

;; Review operations: sync review comments from the API, query them
;; from the DB, submit reviews, render thread buffers, and handle
;; diff commenting.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'url-parse)
(require 'keymap-popup)
(require 'forgejo)
(require 'forgejo-api)
(require 'forgejo-db)
(require 'forgejo-buffer)
(require 'forgejo-utils)

(defvar forgejo-repo--host)
(defvar forgejo-repo--owner)
(defvar forgejo-repo--name)
(defvar forgejo-view--data)
(defvar forgejo-diff--owner)
(defvar forgejo-diff--repo)
(defvar forgejo-diff--pr-number)

;;; Pure helpers

(defun forgejo-review--ids-from-timeline (timeline-alists)
  "Return deduplicated list of review IDs from TIMELINE-ALISTS."
  (delete-dups
   (delq nil (mapcar (lambda (a) (alist-get 'review_id a))
                     (cl-remove-if-not
                      (lambda (a) (string= (alist-get 'type a) "review"))
                      timeline-alists)))))

(defun forgejo-review--comments-to-events (comments review-id)
  "Convert review COMMENTS from API to timeline event alists."
  (mapcar (lambda (c)
            `((id . ,(alist-get 'id c))
              (type . "review_comment")
              (body . ,(alist-get 'body c))
              (user . ,(alist-get 'user c))
              (created_at . ,(alist-get 'created_at c))
              (updated_at . ,(alist-get 'updated_at c))
              (path . ,(alist-get 'path c))
              (diff_hunk . ,(alist-get 'diff_hunk c))
              (position . ,(alist-get 'position c))
              (original_position . ,(alist-get 'original_position c))
              (resolver . ,(alist-get 'resolver c))
              (review_id . ,review-id)))
          comments))

(defun forgejo-review--summary (review-id timeline)
  "Build a list of thread summaries for REVIEW-ID from TIMELINE.
Each thread is grouped by (path, position, original_position) matching
Forgejo's internal thread grouping.  Returns a list of plists
\(:count N :path PATH :position POS :original-position OPOS
  :diff-hunk HUNK :resolved BOOL),
or nil if no comments."
  (let ((comments (cl-remove-if-not
                   (lambda (e)
                     (and (string= (alist-get 'type e) "review_comment")
                          (eql (alist-get 'review_id e) review-id)))
                   timeline))
        (threads (make-hash-table :test 'equal)))
    (when comments
      (dolist (c comments)
        (let ((key (list (alist-get 'path c)
                         (alist-get 'position c)
                         (alist-get 'original_position c))))
          (puthash key (cons c (gethash key threads)) threads)))
      (let (result)
        (maphash
         (lambda (_key thread-comments)
           (let* ((first (car (last thread-comments)))
                  (resolver (alist-get 'resolver first)))
             (push (list :count (length thread-comments)
                         :path (alist-get 'path first)
                         :position (alist-get 'position first)
                         :original-position (alist-get 'original_position first)
                         :diff-hunk (alist-get 'diff_hunk first)
                         :resolved (and (listp resolver)
                                        (alist-get 'login resolver)))
                   result)))
         threads)
        result))))

;;; DB queries

(defun forgejo-review--comments-for-id (host owner repo number review-id
					     &optional path position original-position)
  "Return review_comment alists for REVIEW-ID from the DB.
When PATH is non-nil, filter to that specific thread using
POSITION and ORIGINAL-POSITION for matching."
  (let ((rows (forgejo-db--select
               (format "SELECT %s FROM timeline_events
                        WHERE host = ? AND owner = ? AND repo = ?
                        AND issue_number = ? AND type = 'review_comment'
                        ORDER BY created_at"
                       forgejo-db--timeline-columns)
               (list host owner repo number))))
    (cl-remove-if-not
     (lambda (a)
       (and (eql (alist-get 'review_id a) review-id)
            (or (null path)
                (and (equal (alist-get 'path a) path)
                     (eql (alist-get 'position a) position)
                     (eql (alist-get 'original_position a) original-position)))))
     (mapcar #'forgejo-db--row-to-timeline-alist rows))))

;;; API operations

(defun forgejo-review--fetch-comments (host-url host owner repo number
						review-id &optional callback)
  "Fetch comments for REVIEW-ID from HOST-URL and save to DB.
HOST is the hostname.  Calls CALLBACK when done."
  (forgejo-api-get
   host-url
   (format "repos/%s/%s/pulls/%d/reviews/%d/comments"
           owner repo number review-id)
   nil
   (lambda (comments _headers)
     (forgejo-db-save-timeline host owner repo number
                               (forgejo-review--comments-to-events
                                comments review-id))
     (when callback (funcall callback)))))

(defun forgejo-review-sync-comments (host-url host owner repo number
					      timeline-alists callback)
  "Fetch review comments for all reviews in TIMELINE-ALISTS.
HOST-URL is the instance.  HOST is the hostname.
Calls CALLBACK when all are done."
  (let* ((ids (forgejo-review--ids-from-timeline timeline-alists))
         (remaining (length ids)))
    (if (zerop remaining)
        (when callback (funcall callback))
      (dolist (id ids)
        (forgejo-review--fetch-comments
         host-url host owner repo number id
         (lambda ()
           (setq remaining (1- remaining))
           (when (zerop remaining)
             (when callback (funcall callback)))))))))

(defun forgejo-review--submit (host-url owner repo number callback)
  "Submit a review on PR NUMBER in OWNER/REPO on HOST-URL.
Prompts for the review type and an optional body.
CALLBACK is called on success."
  (let* ((type (completing-read "Review type: "
                                '("approve" "comment")
                                nil t))
         (event (pcase type
                  ("approve" "APPROVED")
                  ("comment" "COMMENT")))
         (body (forgejo-utils-read-body "Review body")))
    (forgejo-api-post
     host-url
     (format "repos/%s/%s/pulls/%d/reviews" owner repo number)
     nil
     `((event . ,event)
       ,@(when (and body (not (string-empty-p (string-trim body))))
           `((body . ,body))))
     (lambda (_data _headers)
       (message "Review submitted: %s on %s/%s#%d" type owner repo number)
       (when callback (funcall callback))))))

(defun forgejo-review--reply (host-url owner repo number review-id
				       path position original-position callback)
  "Reply to review REVIEW-ID on PR NUMBER in OWNER/REPO on HOST-URL.
PATH, POSITION, and ORIGINAL-POSITION from the original comment
for thread grouping.  Prompts for the reply body.
CALLBACK is called on success."
  (let ((body (forgejo-utils-read-body "Reply"))
        (host (url-host (url-generic-parse-url host-url))))
    (when (and body (not (string-empty-p (string-trim body))))
      (forgejo-api-post
       host-url
       (format "repos/%s/%s/pulls/%d/reviews/%d/comments"
               owner repo number review-id)
       nil
       `((body . ,(string-trim body))
         (path . ,(or path ""))
         ,@(if (and position (> position 0))
               `((new_position . ,position))
             `((old_position . ,(or original-position 0)))))
       (lambda (data _headers)
         (forgejo-db-save-timeline
          host owner repo number
          (forgejo-review--comments-to-events (list data) review-id))
         (message "Reply posted on %s/%s#%d review %d"
                  owner repo number review-id)
         (when callback (funcall callback)))))))

;;; Diff review

(defun forgejo-review--diff-new-line-number ()
  "Return the new-file line number for the diff line at point.
Parses the @@ hunk header and counts context/added lines."
  (save-excursion
    (let ((target (line-beginning-position)))
      (diff-beginning-of-hunk t)
      (when (looking-at "@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@")
        (let ((new-start (string-to-number (match-string 1)))
              (new-count 0))
          (forward-line 1)
          (while (< (point) target)
            (unless (eq (char-after) ?-)
              (setq new-count (1+ new-count)))
            (forward-line 1))
          (+ new-start new-count))))))

(defun forgejo-review-diff-comment ()
  "Post a review comment on the line at point in the diff.
Prompts for review type: comment or request_changes."
  (interactive)
  (unless forgejo-diff--pr-number
    (user-error "No PR associated with this diff"))
  (let* ((file-raw (car (diff-hunk-file-names)))
         (file (when file-raw
                 (replace-regexp-in-string
                  "\\`[ab]/" ""
                  (substring-no-properties file-raw))))
         (line (forgejo-review--diff-new-line-number)))
    (unless file
      (user-error "Cannot determine file at point"))
    (let* ((type (completing-read "Review type: "
                                  '("comment" "request_changes") nil t))
           (event (pcase type
                    ("comment" "COMMENT")
                    ("request_changes" "REQUEST_CHANGES")))
           (body (forgejo-utils-read-body "Review comment")))
      (when (and body (not (string-empty-p (string-trim body))))
        (forgejo-api-post
         forgejo-repo--host
         (format "repos/%s/%s/pulls/%d/reviews"
                 forgejo-diff--owner forgejo-diff--repo
                 forgejo-diff--pr-number)
         nil
         `((body . ,(if (string= event "REQUEST_CHANGES") body ""))
           (event . ,event)
           (comments . [((path . ,file)
                         (new_position . ,line)
                         (body . ,body))]))
         (lambda (_data _headers)
           (message "Review %s posted on %s:%d" type file line)))))))

;;; Review thread buffer

(defvar-local forgejo-review--thread-id nil
  "Review ID for the current review thread buffer.")
(defvar-local forgejo-review--thread-number nil
  "Issue/PR number for the current review thread buffer.")
(defvar-local forgejo-review--thread-path nil
  "File path for the current review thread.")
(defvar-local forgejo-review--thread-position nil
  "New-side diff position for the current review thread.")
(defvar-local forgejo-review--thread-original-position nil
  "Old-side diff position for the current review thread.")
(defvar-local forgejo-review--thread-diff-hunk nil
  "Diff hunk text for the current review thread.")

(keymap-popup-define forgejo-review-thread-map
  "Forgejo review thread."
  :parent special-mode-map
  "q" ("Quit" quit-window)
  "c" ("Reply" forgejo-review-thread-reply)
  "g" ("Refresh" forgejo-review-thread-refresh))

(defun forgejo-review--render-thread (buf-name comments)
  "Render review thread COMMENTS into BUF-NAME."
  (with-current-buffer (get-buffer-create buf-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when-let* ((first (car comments))
                  (path (alist-get 'path first))
                  (hunk (alist-get 'diff_hunk first)))
        (insert (propertize (or path "unknown file") 'face 'diff-file-header))
        (let ((resolver (alist-get 'resolver first)))
          (when (and (listp resolver) (alist-get 'login resolver))
            (insert "  " (propertize
                          (format "(resolved by %s)" (alist-get 'login resolver))
                          'face 'success))))
        (insert "\n")
        (forgejo-buffer--insert-diff-hunk hunk))
      (let* ((raw-bodies (mapcar (lambda (c)
                                   (forgejo-buffer--clean-body
                                    (alist-get 'body c)))
                                 comments))
             (fontified (forgejo-buffer--fontify-bodies
                         (cl-remove-if #'null raw-bodies)))
             (font-idx 0))
        (dolist (c comments)
          (let ((author (or (alist-get 'login (alist-get 'user c)) "unknown"))
                (has-body (nth (cl-position c comments) raw-bodies))
                (created (alist-get 'created_at c)))
            (insert "\n"
                    (propertize author 'face 'forgejo-comment-author-face)
                    (propertize (concat " commented "
                                        (forgejo-buffer--relative-time created))
                                'face 'shadow)
                    "\n")
            (when has-body
              (forgejo-buffer--insert-body (nth font-idx fontified))
              (setq font-idx (1+ font-idx)))
            (forgejo-buffer--insert-separator)))))
    (use-local-map forgejo-review-thread-map)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (current-buffer)))

(defun forgejo-review-thread-refresh ()
  "Refresh the current review thread buffer."
  (interactive)
  (when-let* ((review-id forgejo-review--thread-id)
              (number forgejo-review--thread-number)
              (host (url-host (url-generic-parse-url forgejo-repo--host)))
              (owner forgejo-repo--owner)
              (repo forgejo-repo--name)
              (path forgejo-review--thread-path)
              (position forgejo-review--thread-position)
              (opos forgejo-review--thread-original-position)
              (buf (buffer-name)))
    (forgejo-review--fetch-comments
     forgejo-repo--host host owner repo number review-id
     (lambda ()
       (let ((comments (forgejo-review--comments-for-id
                        host owner repo number review-id
                        path position opos)))
         (forgejo-review--render-thread buf comments))))))

(defun forgejo-review-thread-reply ()
  "Reply to the current review thread."
  (interactive)
  (when-let* ((review-id forgejo-review--thread-id)
              (number forgejo-review--thread-number)
              (owner forgejo-repo--owner)
              (repo forgejo-repo--name)
              (host (url-host (url-generic-parse-url forgejo-repo--host)))
              (path forgejo-review--thread-path)
              (position forgejo-review--thread-position)
              (opos forgejo-review--thread-original-position)
              (buf (current-buffer)))
    (forgejo-review--reply
     forgejo-repo--host owner repo number review-id path position opos
     (lambda ()
       (let ((comments (forgejo-review--comments-for-id
                        host owner repo number review-id
                        path position opos)))
         (forgejo-review--render-thread
          (buffer-name buf) comments))))))

(defun forgejo-review-open-thread ()
  "Open the review thread for the review link at point."
  (interactive)
  (when-let* ((review-id (get-text-property (point) 'forgejo-review-id))
              (path (get-text-property (point) 'forgejo-review-path))
              (host-url forgejo-repo--host)
              (host (url-host (url-generic-parse-url host-url)))
              (owner forgejo-repo--owner)
              (repo forgejo-repo--name)
              (number (alist-get 'number
                                 (bound-and-true-p forgejo-view--data))))
    (let* ((position (get-text-property (point) 'forgejo-review-position))
           (opos (get-text-property (point) 'forgejo-review-opos))
           (comments (forgejo-review--comments-for-id
                      host owner repo number review-id path position opos))
           (buf-name (format "*forgejo-review: %s/%s#%d r%d %s*"
                             owner repo number review-id
                             (file-name-nondirectory (or path "?")))))
      (if comments
          (let ((buf (forgejo-review--render-thread buf-name comments)))
            (with-current-buffer buf
              (setq forgejo-repo--host host-url
                    forgejo-repo--owner owner
                    forgejo-repo--name repo
                    forgejo-review--thread-id review-id
                    forgejo-review--thread-number number
                    forgejo-review--thread-path
                    (alist-get 'path (car comments))
                    forgejo-review--thread-position
                    (alist-get 'position (car comments))
                    forgejo-review--thread-original-position
                    (alist-get 'original_position (car comments))
                    forgejo-review--thread-diff-hunk
                    (alist-get 'diff_hunk (car comments))))
            (pop-to-buffer buf))
        (message "No review comments found for review %d" review-id)))))

;;; Interactive commands

(defun forgejo-review-submit ()
  "Submit a review on the current pull request."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data)))
    (forgejo-review--submit
     forgejo-repo--host forgejo-repo--owner forgejo-repo--name number
     (forgejo--post-action-callback))))

(provide 'forgejo-review)
;;; forgejo-review.el ends here
