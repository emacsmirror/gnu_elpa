;;; forgejo-buffer.el --- Shared display and rendering utilities  -*- lexical-binding: t; -*-

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

;; Shared display logic for issues and pull requests: faces, formatting
;; helpers, EWOC pretty-printers, and shr-based HTML rendering.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'shr)
(require 'dom)
(require 'diff-mode)
(require 'forgejo)
(require 'forgejo-db)

(defvar forgejo-repo--owner)
(defvar forgejo-repo--name)
(defvar forgejo-pull--data)
(defvar forgejo-issue--data)

;;; Faces

(defface forgejo-number-face
  '((t :inherit shadow))
  "Face for issue/PR numbers."
  :group 'forgejo)

(defface forgejo-open-face
  '((t :inherit success))
  "Face for open issue/PR state."
  :group 'forgejo)

(defface forgejo-closed-face
  '((t :inherit shadow))
  "Face for closed issue/PR state."
  :group 'forgejo)

(defface forgejo-label-face
  '((t :weight bold))
  "Base face for labels."
  :group 'forgejo)

(defface forgejo-comment-author-face
  '((t :inherit font-lock-constant-face))
  "Face for comment author names."
  :group 'forgejo)

(defface forgejo-event-face
  '((t :inherit shadow :slant italic))
  "Face for timeline events (close, label, assign, etc.)."
  :group 'forgejo)

(defface forgejo-separator-face
  '((default :inherit org-hide)
    (((background light)) :strike-through "gray70")
    (t :strike-through "gray30"))
  "Face for separator lines between comments."
  :group 'forgejo)

(defface forgejo-blockquote-face
  '((t :inherit font-lock-doc-face :slant italic))
  "Face for quoted text (blockquotes) in comments."
  :group 'forgejo)

;;; Formatting helpers

(defun forgejo-buffer--format-state (state)
  "Propertize STATE string with appropriate face."
  (propertize state 'face
              (if (string= state "open")
                  'forgejo-open-face
                'forgejo-closed-face)))

(defun forgejo-buffer--readable-color (hex)
  "Adjust HEX color for readability against the current background.
Lightens dark colors on dark themes, darkens light colors on light themes."
  (let* ((hex (replace-regexp-in-string "^#" "" hex))
         (r (/ (string-to-number (substring hex 0 2) 16) 255.0))
         (g (/ (string-to-number (substring hex 2 4) 16) 255.0))
         (b (/ (string-to-number (substring hex 4 6) 16) 255.0))
         (lum (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)))
         (dark-bg (eq (frame-parameter nil 'background-mode) 'dark)))
    (if dark-bg
        (if (< lum 0.4)
            (format "#%02x%02x%02x"
                    (min 255 (round (* (+ r 0.4) 255)))
                    (min 255 (round (* (+ g 0.4) 255)))
                    (min 255 (round (* (+ b 0.4) 255))))
          (concat "#" hex))
      (if (> lum 0.7)
          (format "#%02x%02x%02x"
                  (round (* r 0.6 255))
                  (round (* g 0.6 255))
                  (round (* b 0.6 255)))
        (concat "#" hex)))))

(defun forgejo-buffer--format-labels (labels)
  "Format LABELS alist into a propertized string."
  (if (and labels (listp labels))
      (mapconcat
       (lambda (label)
         (let ((name (alist-get 'name label))
               (color (alist-get 'color label)))
           (if color
               (propertize name 'face
                           (list :foreground
                                 (forgejo-buffer--readable-color color)
                                 :weight 'bold))
             (propertize name 'face 'forgejo-label-face))))
       labels ", ")
    ""))

(defun forgejo-buffer--relative-time (time-string)
  "Convert TIME-STRING to a relative time description."
  (if (or (null time-string) (string-empty-p time-string))
      ""
    (let* ((time (date-to-time time-string))
           (diff (float-time (time-subtract nil time))))
      (cond
       ((< diff 60) "just now")
       ((< diff 3600) (format "%dm ago" (floor (/ diff 60))))
       ((< diff 86400) (format "%dh ago" (floor (/ diff 3600))))
       ((< diff 604800) (format "%dd ago" (floor (/ diff 86400))))
       ((< diff 2592000) (format "%dw ago" (floor (/ diff 604800))))
       (t (format-time-string "%Y-%m-%d" time))))))

(defun forgejo-buffer--login (user-alist)
  "Extract login from USER-ALIST, handling :null safely."
  (when (listp user-alist)
    (alist-get 'login user-alist)))

;;; Body rendering

(defun forgejo-buffer--clean-body (body)
  "Strip carriage returns from BODY text.
Returns nil if BODY is :null, nil, or empty."
  (when (and body (stringp body) (not (string-empty-p body)))
    (replace-regexp-in-string "\r" "" body)))

(defun forgejo-buffer--shr-blockquote (dom)
  "Render a blockquote DOM node with `forgejo-blockquote-face'."
  (let ((start (point))
        (shr-indentation (+ shr-indentation 2)))
    (shr-ensure-newline)
    (shr-generic dom)
    (shr-ensure-newline)
    (put-text-property start (point) 'face 'forgejo-blockquote-face)))

(defun forgejo-buffer--insert-html (html)
  "Insert rendered HTML into the current buffer using shr.
Applies `forgejo-blockquote-face' to quoted text.
Falls back to plain text insertion if HTML parsing fails."
  (when (and html (stringp html) (not (string-empty-p html)))
    (condition-case nil
        (let ((dom (with-temp-buffer
                     (insert html)
                     (libxml-parse-html-region (point-min) (point-max))))
              (shr-use-fonts nil)
              (shr-width (min (- (window-width) 4) 80))
              (start (point)))
          (let ((shr-external-rendering-functions
                 (cons '(blockquote . forgejo-buffer--shr-blockquote)
                       shr-external-rendering-functions)))
            (shr-insert-document dom)))
      (error (insert html "\n")))))

(defun forgejo-buffer--body-or-text (alist)
  "Return body_html from ALIST if available, else cleaned body text."
  (or (alist-get 'body_html alist)
      (forgejo-buffer--clean-body (alist-get 'body alist))))

;;; Separator

(defun forgejo-buffer--insert-separator ()
  "Insert a full-width strike-through separator line."
  (insert "\n"
          (propertize " " 'display '(space :width text)
                      'face 'forgejo-separator-face)
          "\n"))

;;; Edited indicator

(defvar forgejo-buffer-edited-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'forgejo-buffer--show-edit-history)
    (define-key map [mouse-1] #'forgejo-buffer--show-edit-history)
    map)
  "Keymap for the (edited) indicator.")

(defun forgejo-buffer--edited-p (created updated)
  "Return non-nil if CREATED and UPDATED timestamps differ."
  (and created updated
       (not (string= created updated))))

(defun forgejo-buffer--insert-edited (created updated)
  "Insert an (edited) indicator if CREATED differs from UPDATED."
  (when (forgejo-buffer--edited-p created updated)
    (insert " "
            (propertize "(edited)"
                        'face 'shadow
                        'mouse-face 'highlight
                        'keymap forgejo-buffer-edited-map
                        'forgejo-edit-created created
                        'forgejo-edit-updated updated
                        'help-echo "RET: view edit history"))))

(defun forgejo-buffer--show-edit-history ()
  "Show edit timestamps for the item at point."
  (interactive)
  (let ((created (get-text-property (point) 'forgejo-edit-created))
        (updated (get-text-property (point) 'forgejo-edit-updated)))
    (when (and created updated)
      (with-current-buffer (get-buffer-create "*forgejo-edit-history*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "Edit History\n\n" 'face 'bold)
                  (propertize "Created: " 'face 'shadow) created "\n"
                  (propertize "Updated: " 'face 'shadow) updated "\n"))
        (special-mode)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

;;; EWOC pretty-printers

(defun forgejo-buffer--pp (node-data)
  "Pretty-print NODE-DATA for the detail EWOC.
NODE-DATA is a plist with :type and type-specific keys."
  (let ((type (plist-get node-data :type)))
    (pcase type
      ('header (forgejo-buffer--pp-header node-data))
      ('comment (forgejo-buffer--pp-comment node-data))
      ('review-link (forgejo-buffer--pp-review-link node-data))
      ('event (forgejo-buffer--pp-event node-data)))))

(defun forgejo-buffer--pp-header (data)
  "Render the issue/PR header from DATA plist."
  (let ((title (plist-get data :title))
        (state (plist-get data :state))
        (author (plist-get data :author))
        (number (plist-get data :number))
        (body-html (plist-get data :body-html))
        (labels (plist-get data :labels))
        (milestone (plist-get data :milestone))
        (assignees (plist-get data :assignees))
        (created (plist-get data :created-at))
        (updated (plist-get data :updated-at))
        (comments-count (plist-get data :comments-count)))
    (insert (propertize (format "#%d " number) 'face 'bold)
            (propertize title 'face 'bold)
            "\n")
    (insert (forgejo-buffer--format-state state)
            "  "
            (propertize author 'face 'forgejo-comment-author-face)
            " opened " (forgejo-buffer--relative-time created))
    (forgejo-buffer--insert-edited created updated)
    (insert (format "  [%d comments]" (or comments-count 0))
            "\n")
    (when (and labels (listp labels))
      (insert "Labels: " (forgejo-buffer--format-labels labels) "\n"))
    (when milestone
      (insert "Milestone: " milestone "\n"))
    (when (and assignees (listp assignees))
      (insert "Assignees: "
              (mapconcat (lambda (a) (alist-get 'login a)) assignees ", ")
              "\n"))
    (forgejo-buffer--insert-separator)
    ;; Body displayed like a comment from the author
    (when body-html
      (insert (propertize author 'face 'forgejo-comment-author-face)
              "\n\n")
      (forgejo-buffer--insert-html body-html)
      (forgejo-buffer--insert-separator))))

(defun forgejo-buffer--pp-comment (data)
  "Render a comment from DATA plist."
  (let ((author (plist-get data :author))
        (body-html (plist-get data :body-html))
        (created (plist-get data :created-at))
        (updated (plist-get data :updated-at)))
    (insert (propertize author 'face 'forgejo-comment-author-face)
            (propertize (concat " commented " (forgejo-buffer--relative-time created))
                        'face 'shadow))
    (forgejo-buffer--insert-edited created updated)
    (insert "\n\n")
    (forgejo-buffer--insert-html body-html)
    (forgejo-buffer--insert-separator)))

(defvar forgejo-buffer-ref-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'forgejo-buffer-follow-ref)
    (define-key map [mouse-1] #'forgejo-buffer-follow-ref)
    map)
  "Keymap for reference links in event lines.")

(defun forgejo-buffer-follow-ref ()
  "Follow the reference link at point."
  (interactive)
  (when-let* ((number (get-text-property (point) 'forgejo-ref-number)))
    (declare-function forgejo-issue-view "forgejo-issue.el"
                      (owner repo number))
    (forgejo-issue-view forgejo-repo--owner forgejo-repo--name number)))

(defvar forgejo-buffer-commit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'forgejo-buffer-view-commit-diff)
    (define-key map [mouse-1] #'forgejo-buffer-view-commit-diff)
    map)
  "Keymap for commit links in event lines.")

;;; Diff buffer

(defvar-local forgejo-diff--owner nil "Owner of the repo for this diff.")
(defvar-local forgejo-diff--repo nil "Repo name for this diff.")
(defvar-local forgejo-diff--pr-number nil "PR number for this diff.")

(defvar forgejo-buffer-diff-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "c") #'forgejo-buffer-diff-comment)
    map)
  "Keymap for forgejo diff buffers.")

(defun forgejo-buffer-view-commit-diff ()
  "View the diff for the commit at point."
  (interactive)
  (when-let* ((sha (get-text-property (point) 'forgejo-commit-sha)))
    (declare-function forgejo-token "forgejo.el" ())
    (defvar forgejo-repo--host)
    (forgejo-with-host forgejo-repo--host
      (let* ((owner forgejo-repo--owner)
             (repo forgejo-repo--name)
             (pr-number (when-let* ((data (or (bound-and-true-p forgejo-pull--data)
                                              (bound-and-true-p forgejo-issue--data))))
                          (alist-get 'number data)))
             (url (format "%s/api/v1/repos/%s/%s/git/commits/%s.diff"
                          forgejo-host owner repo sha))
             (url-request-method "GET")
             (url-request-extra-headers
              `(("Authorization" . ,(encode-coding-string
                                     (concat "token " (forgejo-token)) 'ascii)))))
      (url-retrieve
       url
       (lambda (_status)
         (goto-char (point-min))
         (re-search-forward "\r?\n\r?\n" nil t)
         (let ((diff-text (buffer-substring-no-properties (point) (point-max)))
               (buf-name (format "*forgejo-diff: %s*" (substring sha 0 8))))
           (kill-buffer (current-buffer))
           (with-current-buffer (get-buffer-create buf-name)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert diff-text))
             (diff-mode)
             (use-local-map forgejo-buffer-diff-map)
             (setq buffer-read-only t
                   forgejo-diff--owner owner
                   forgejo-diff--repo repo
                   forgejo-diff--pr-number pr-number)
             (goto-char (point-min))
             (switch-to-buffer (current-buffer)))))
       nil t)))))

;; TODO: Implement pending review flow -- accumulate line comments
;; during diff review, then submit them all together with a verdict.
;; Forgejo supports this via PENDING reviews: create with event
;; "PENDING", add comments via POST /reviews/{id}/comments, then
;; submit with POST /reviews/{id}/submit.  Currently each comment
;; creates a separate review.  Needs testing with a second account
;; (can't review own PRs).

(defun forgejo-buffer--diff-new-line-number ()
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

(defun forgejo-buffer-diff-comment ()
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
         (line (forgejo-buffer--diff-new-line-number)))
    (unless file
      (user-error "Cannot determine file at point"))
    (let* ((type (completing-read "Review type: "
                                  '("comment" "request_changes") nil t))
           (event (pcase type
                    ("comment" "COMMENT")
                    ("request_changes" "REQUEST_CHANGES")))
           (body (read-string-from-buffer "Review comment" "")))
      (when (and body (not (string-empty-p (string-trim body))))
        (declare-function forgejo-api-post "forgejo-api.el"
                          (endpoint &optional params json-body callback))
        (forgejo-api-post
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

(defun forgejo-buffer--pp-event (data)
  "Render a timeline event from DATA plist."
  (let ((event-type (plist-get data :event-type))
        (actor (plist-get data :actor))
        (created (plist-get data :created-at))
        (detail (plist-get data :detail))
        (deadline (plist-get data :deadline))
        (label-color (plist-get data :label-color))
        (ref-number (plist-get data :ref-number))
        (commits (plist-get data :commits)))
    ;; Actor in author face, verb in event face
    (insert (propertize actor 'face 'forgejo-comment-author-face)
            " "
            (propertize (concat event-type " ")
                        'face (pcase event-type
                                ("close" 'forgejo-closed-face)
                                ("reopen" 'forgejo-open-face)
                                ("merged" 'success)
                                (_ 'forgejo-event-face))))
    ;; Detail: deadline, label with color, ref with link, or plain
    (cond
     (deadline
      (insert (propertize deadline 'face '(bold underline))))
     ((and detail ref-number)
      (insert (propertize detail
                          'face '(forgejo-event-face :underline t)
                          'mouse-face 'highlight
                          'forgejo-ref-number ref-number
                          'keymap forgejo-buffer-ref-map
                          'help-echo "RET: view this issue/PR")))
     ((and detail label-color)
      (insert (propertize detail
                          'face (list :foreground
                                      (forgejo-buffer--readable-color
                                       label-color)
                                      :weight 'bold))))
     (detail
      (insert (propertize detail 'face 'forgejo-event-face))))
    ;; Timestamp in shadow
    (insert (propertize (format " %s\n"
                                (forgejo-buffer--relative-time created))
                        'face 'shadow))
    ;; Commit links
    (when commits
      (dolist (sha commits)
        (insert "  "
                (propertize (substring sha 0 (min 8 (length sha)))
                            'face '(forgejo-event-face :underline t)
                            'mouse-face 'highlight
                            'forgejo-commit-sha sha
                            'keymap forgejo-buffer-commit-map
                            'help-echo "RET: view diff")
                "\n")))
    (forgejo-buffer--insert-separator)))

;;; EWOC node builder

(declare-function forgejo-review--summary "forgejo-review.el"
                  (review-id timeline))

(defun forgejo-buffer--build-nodes (issue-data timeline)
  "Build EWOC node plists from ISSUE-DATA and TIMELINE.
Both should be alists with `body_html' pre-populated from the DB."
  (let ((nodes nil))
    ;; Header node
    (let-alist issue-data
      (push (list :type 'header
                  :number .number
                  :title .title
                  :state .state
                  :author (or (forgejo-buffer--login .user) "unknown")
                  :body .body
                  :body-html (forgejo-buffer--body-or-text issue-data)
                  :labels (when (listp .labels) .labels)
                  :milestone (when (listp .milestone)
                               (alist-get 'title .milestone))
                  :assignees (when (listp .assignees) .assignees)
                  :created-at .created_at
                  :updated-at .updated_at
                  :comments-count .comments)
            nodes))
    ;; Timeline nodes
    (dolist (event timeline)
      (let-alist event
        (let ((actor (or (forgejo-buffer--login .user) "system")))
          (pcase .type
            ("comment"
             (push (list :type 'comment
                         :id .id
                         :author actor
                         :body (alist-get 'body event)
                         :body-html (forgejo-buffer--body-or-text event)
                         :created-at .created_at
                         :updated-at .updated_at)
                   nodes))
            ((or "close" "reopen")
             (push (list :type 'event
                         :event-type .type
                         :actor actor
                         :created-at .created_at
                         :detail nil)
                   nodes))
            ("label"
             (let ((removed (or (string-empty-p (or (alist-get 'body event) ""))
                                (equal (alist-get 'body event) "0"))))
               (push (list :type 'event
                           :event-type (if removed "removed label" "added label")
                           :actor actor
                           :created-at .created_at
                           :detail (when (listp .label)
                                     (alist-get 'name .label))
                           :label-color (when (listp .label)
                                          (alist-get 'color .label)))
                     nodes)))
            ("assignees"
             (push (list :type 'event
                         :event-type (if (eq .removed_assignee t)
                                         "unassigned" "assigned")
                         :actor actor
                         :created-at .created_at
                         :detail (forgejo-buffer--login .assignee))
                   nodes))
            ("pull_push"
             (let* ((body-str (alist-get 'body event))
                    (push-data (when (and body-str (stringp body-str))
                                 (condition-case nil
                                     (json-parse-string body-str
                                                        :object-type 'alist
                                                        :array-type 'list)
                                   (error nil))))
                    (commit-ids (when push-data
                                  (alist-get 'commit_ids push-data)))
                    (force-p (when push-data
                               (alist-get 'is_force_push push-data))))
               (push (list :type 'event
                           :event-type (if (eq force-p t) "force pushed"
                                         "pushed")
                           :actor actor
                           :created-at .created_at
                           :commits commit-ids
                           :detail (format "%d commit%s"
                                           (length commit-ids)
                                           (if (= (length commit-ids) 1)
                                               "" "s")))
                     nodes)))
            ("change_issue_ref"
             (push (list :type 'event
                         :event-type "changed ref"
                         :actor actor
                         :created-at .created_at
                         :detail (format "%s -> %s"
                                         (if (string-empty-p (or .old_ref ""))
                                             "(none)" .old_ref)
                                         (or .new_ref "?")))
                   nodes))
            ("change_title"
             (push (list :type 'event
                         :event-type "renamed"
                         :actor actor
                         :created-at .created_at
                         :detail (format "\"%s\" -> \"%s\""
                                         (or .old_title "?")
                                         (or .new_title "?")))
                   nodes))
            ((or "pull_ref" "issue_ref" "commit_ref" "comment_ref")
             (let* ((ref-issue .ref_issue)
                    (ref-title (when (listp ref-issue)
                                 (alist-get 'title ref-issue)))
                    (ref-number (when (listp ref-issue)
                                  (alist-get 'number ref-issue))))
               (push (list :type 'event
                           :event-type "referenced"
                           :actor actor
                           :created-at .created_at
                           :ref-number ref-number
                           :detail (if ref-number
                                       (format "#%d %s" ref-number
                                               (or ref-title ""))
                                     nil))
                     nodes)))
            ((or "added_deadline" "modified_deadline")
             (push (list :type 'event
                         :event-type (if (string= .type "added_deadline")
                                         "set deadline" "changed deadline")
                         :actor actor
                         :created-at .created_at
                         :deadline (alist-get 'body event))
                   nodes))
            ("removed_deadline"
             (push (list :type 'event
                         :event-type "removed deadline"
                         :actor actor
                         :created-at .created_at
                         :detail nil)
                   nodes))
            ("review_request"
             (let ((target (when (listp .assignee)
                             (alist-get 'login .assignee))))
               (push (list :type 'event
                           :event-type "requested review from"
                           :actor actor
                           :created-at .created_at
                           :detail target)
                     nodes)))
            ("milestone"
             (let* ((ms (when (listp .milestone)
                          (alist-get 'title .milestone)))
                    (removed (string-empty-p (or (alist-get 'body event) ""))))
               (push (list :type 'event
                           :event-type (if removed "removed milestone"
                                         "set milestone")
                           :actor actor
                           :created-at .created_at
                           :detail ms)
                     nodes)))
            ("merge_pull"
             (push (list :type 'event
                         :event-type "merged"
                         :actor actor
                         :created-at .created_at
                         :detail nil)
                   nodes))
            ("review"
             (let* ((body (alist-get 'body event))
                    (threads (forgejo-review--summary
                              .review_id timeline)))
               (cond
                (threads
                 ;; Review with inline comments: one node with all thread links
                 (push (list :type 'review-link
                             :review-id .review_id
                             :actor actor
                             :created-at .created_at
                             :threads threads
                             :body-html (when (and body (not (string-empty-p body)))
                                          (forgejo-buffer--body-or-text event)))
                       nodes))
                ;; Review with body but no inline comments
                ((and body (not (string-empty-p body)))
                 (push (list :type 'comment
                             :id .id
                             :author actor
                             :body body
                             :body-html (forgejo-buffer--body-or-text event)
                             :created-at .created_at)
                       nodes))
                ;; Empty review, no comments: one-liner
                (t (push (list :type 'event
                               :event-type "reviewed"
                               :actor actor
                               :created-at .created_at
                               :detail nil)
                         nodes)))))
            ;; review_comment: rendered in dedicated thread buffer, skip here
            ("review_comment" nil)
            ((or "add_dependency" "remove_dependency")
             (let* ((dep (when (listp .dependent_issue) .dependent_issue))
                    (dep-number (alist-get 'number dep))
                    (dep-title (alist-get 'title dep)))
               (push (list :type 'event
                           :event-type (if (string= .type "add_dependency")
                                           "added dependency" "removed dependency")
                           :actor actor
                           :created-at .created_at
                           :ref-number dep-number
                           :detail (when dep-number
                                     (format "#%d %s" dep-number
                                             (or dep-title ""))))
                     nodes)))
            ("delete_branch"
             (push (list :type 'event
                         :event-type "deleted branch"
                         :actor actor
                         :created-at .created_at
                         :detail nil)
                   nodes))
            (_
             (when .type
               (push (list :type 'event
                           :event-type .type
                           :actor actor
                           :created-at .created_at
                           :detail nil)
                     nodes)))))))
    (nreverse nodes)))

;;; Utilities for detail views

(defun forgejo-buffer--node-at-point (ewoc)
  "Return the data plist of the EWOC node at point, or nil."
  (when-let* ((node (ewoc-locate ewoc)))
    (ewoc-data node)))

;;; Header line

(defun forgejo-buffer--header-line (data)
  "Build a `header-line-format' string from issue/PR DATA alist."
  (let-alist data
    (let ((labels (when (listp .labels)
                    (forgejo-buffer--format-labels .labels)))
          (assignees (when (listp .assignees)
                       (mapconcat (lambda (a) (alist-get 'login a))
                                  .assignees ", "))))
      (concat (propertize (format " #%d" .number) 'face 'forgejo-number-face)
              " "
              (propertize (or .state "") 'face
                          (if (string= .state "open")
                              'forgejo-open-face 'forgejo-closed-face))
              "  "
              (propertize (or .title "") 'face 'bold)
              (when (and labels (not (string-empty-p labels)))
                (concat "  " labels))
              (when (and assignees (not (string-empty-p assignees)))
                (concat "  " (propertize assignees 'face 'shadow)))))))

;;; Re-render helper

(defun forgejo-buffer--re-render (buf-name host owner repo number
                                  render-fn &optional restore-line)
  "Re-render detail buffer BUF-NAME from fresh DB data.
RENDER-FN is called with (BUF-NAME OWNER REPO ISSUE-ALIST TIMELINE-ALISTS)."
  (when (buffer-live-p (get-buffer buf-name))
    (let* ((issue (forgejo-db-get-issue host owner repo number))
           (tl-rows (forgejo-db-get-timeline host owner repo number))
           (tl-alists (mapcar #'forgejo-db--row-to-timeline-alist tl-rows)))
      (funcall render-fn buf-name owner repo issue tl-alists)
      (when restore-line
        (with-current-buffer buf-name
          (goto-char (point-min))
          (forward-line (1- restore-line)))))))

;;; Diff hunk rendering

(defun forgejo-buffer--insert-diff-hunk (hunk-text)
  "Insert HUNK-TEXT with diff faces applied per line."
  (when (and hunk-text (stringp hunk-text) (not (string-empty-p hunk-text)))
    (let ((start (point)))
      (insert hunk-text)
      (unless (eq (char-before) ?\n) (insert "\n"))
      (save-excursion
        (goto-char start)
        (while (< (point) (point-max))
          (let ((face (cond
                       ((looking-at-p "^\\+") 'diff-added)
                       ((looking-at-p "^-")   'diff-removed)
                       ((looking-at-p "^@@")  'diff-hunk-header))))
            (when face
              (put-text-property (line-beginning-position)
                                 (line-end-position) 'face face)))
          (forward-line 1))))))

;;; Review link rendering

(defvar forgejo-buffer-review-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'forgejo-buffer--open-review-thread)
    (define-key map [mouse-1] #'forgejo-buffer--open-review-thread)
    map)
  "Keymap for review links in the timeline.")

(defun forgejo-buffer--pp-review-link (data)
  "Render a review with thread links from DATA plist."
  (let ((actor (plist-get data :actor))
        (created (plist-get data :created-at))
        (threads (plist-get data :threads))
        (review-id (plist-get data :review-id))
        (body-html (plist-get data :body-html)))
    (insert (propertize actor 'face 'forgejo-comment-author-face)
            (propertize (concat " reviewed " (forgejo-buffer--relative-time created))
                        'face 'shadow)
            "\n")
    (dolist (thread threads)
      (let ((count (plist-get thread :count))
            (path (plist-get thread :path))
            (resolved (plist-get thread :resolved)))
        (insert "  "
                (propertize (format "[%d comment%s on %s]"
                                    (or count 0)
                                    (if (= (or count 0) 1) "" "s")
                                    (or (file-name-nondirectory (or path "")) "?"))
                            'face '(link :underline t)
                            'mouse-face 'highlight
                            'forgejo-review-id review-id
                            'forgejo-review-path path
                            'forgejo-review-position (plist-get thread :position)
                            'forgejo-review-opos (plist-get thread :original-position)
                            'keymap forgejo-buffer-review-map
                            'help-echo "RET: view review thread")
                " "
                (if resolved
                    (propertize "(resolved)" 'face 'success)
                  (propertize "(unresolved)" 'face 'warning))
                "\n")))
    (when body-html
      (forgejo-buffer--insert-html body-html))
    (forgejo-buffer--insert-separator)))

;;; Review thread buffer

(declare-function forgejo-review--comments-for-id "forgejo-review.el"
                  (host owner repo number review-id
                   &optional path position original-position))
(declare-function forgejo-review--reply "forgejo-review.el"
                  (owner repo number review-id
                   path position original-position callback))

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

(defvar forgejo-review-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "c") #'forgejo-buffer--review-thread-reply)
    (define-key map (kbd "g") #'forgejo-buffer--review-thread-refresh)
    map)
  "Keymap for review thread buffers.")

(defun forgejo-buffer--render-review-thread (buf-name comments)
  "Render review thread COMMENTS into BUF-NAME."
  (with-current-buffer (get-buffer-create buf-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when-let* ((first (car comments))
                  (path (alist-get 'path first))
                  (hunk (alist-get 'diff_hunk first)))
        ;; Header
        (insert (propertize (or path "unknown file") 'face 'diff-file-header))
        (let ((resolver (alist-get 'resolver first)))
          (when (and (listp resolver) (alist-get 'login resolver))
            (insert "  " (propertize
                          (format "(resolved by %s)" (alist-get 'login resolver))
                          'face 'success))))
        (insert "\n")
        ;; Diff hunk
        (forgejo-buffer--insert-diff-hunk hunk))
      ;; Comments
      (dolist (c comments)
        (let ((author (or (alist-get 'login (alist-get 'user c)) "unknown"))
              (body (or (alist-get 'body_html c) (alist-get 'body c)))
              (created (alist-get 'created_at c)))
          (insert "\n"
                  (propertize author 'face 'forgejo-comment-author-face)
                  (propertize (concat " commented "
                                      (forgejo-buffer--relative-time created))
                              'face 'shadow)
                  "\n")
          (forgejo-buffer--insert-html body)
          (forgejo-buffer--insert-separator))))
    (use-local-map forgejo-review-thread-mode-map)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (current-buffer)))

(defun forgejo-buffer--review-thread-refresh ()
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
     host owner repo number review-id
     (lambda ()
       (let ((comments (forgejo-review--comments-for-id
                        host owner repo number review-id
                        path position opos)))
         (forgejo-buffer--render-review-thread buf comments))))))

(declare-function forgejo-review--fetch-comments "forgejo-review.el"
                  (host owner repo number review-id &optional callback))

(defun forgejo-buffer--review-thread-reply ()
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
    (forgejo-with-host forgejo-repo--host
      (forgejo-review--reply
       owner repo number review-id path position opos
       (lambda ()
         (let ((comments (forgejo-review--comments-for-id
                          host owner repo number review-id
                          path position opos)))
           (forgejo-buffer--render-review-thread
            (buffer-name buf) comments)))))))

(defun forgejo-buffer--open-review-thread ()
  "Open the review thread for the review link at point."
  (interactive)
  (when-let* ((review-id (get-text-property (point) 'forgejo-review-id))
              (path (get-text-property (point) 'forgejo-review-path))
              (host (url-host (url-generic-parse-url
                               (or (bound-and-true-p forgejo-repo--host)
                                   forgejo-host))))
              (owner (bound-and-true-p forgejo-repo--owner))
              (repo (bound-and-true-p forgejo-repo--name))
              (number (alist-get 'number
                                 (or (bound-and-true-p forgejo-pull--data)
                                     (bound-and-true-p forgejo-issue--data)))))
    (let* ((position (get-text-property (point) 'forgejo-review-position))
           (opos (get-text-property (point) 'forgejo-review-opos))
           (comments (forgejo-review--comments-for-id
                      host owner repo number review-id path position opos))
           (buf-name (format "*forgejo-review: %s/%s#%d r%d %s*"
                             owner repo number review-id
                             (file-name-nondirectory (or path "?")))))
      (if comments
          (let ((buf (forgejo-buffer--render-review-thread buf-name comments)))
            (with-current-buffer buf
              (setq forgejo-repo--host (or (bound-and-true-p forgejo-repo--host)
                                           forgejo-host)
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

(provide 'forgejo-buffer)
;;; forgejo-buffer.el ends here
