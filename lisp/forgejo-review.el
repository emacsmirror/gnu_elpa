;;; forgejo-review.el --- Review data operations for Forgejo  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

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

;; Review data operations: sync review comments from the API, query
;; them from the DB, submit reviews.  Display logic lives in
;; forgejo-buffer.el.

;;; Code:

(require 'cl-lib)
(require 'url-parse)
(require 'forgejo)
(require 'forgejo-api)
(require 'forgejo-db)
(require 'forgejo-utils)

(defvar forgejo-repo--host)
(defvar forgejo-repo--owner)
(defvar forgejo-repo--name)
(defvar forgejo-view--data)

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
