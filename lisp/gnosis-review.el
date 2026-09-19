;;; gnosis-review.el --- Review system for gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis

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

;; Review system for gnosis spaced repetition.
;;
;; This module handles:
;; - Scheduling: due/overdue thema detection (`gnosis-review-is-due-p',
;;   `gnosis-review-get-due-themata', `gnosis-review-get-overdue-themata')
;; - Review display: keimenon, images, clozes, answers, hints, parathema
;; - Type-specific review logic: MCQ, basic, cloze, MC-cloze
;; - Review session management and actions (next, override, suspend,
;;   edit, quit, view-link)
;; - Scheduler bridge: previewing and accepting FSRS review results
;; - Monkeytype integration for typing practice
;; - Link view mode for viewing Gnosis nodes during review

;;; Code:

(require 'button)
(require 'gnosis)
(require 'gnosis-db)
(require 'gnosis-scheduler)
(require 'gnosis-study)
(require 'gnosis-cloze)
(require 'gnosis-vc)
(require 'gnosis-monkeytype)
(require 'gnosis-utils)
(require 'gnosis-nodes)
(require 'gnosis-links)
(require 'keymap-popup)

(autoload 'gnosis-review-agent-eval "gnosis-agent-eval")

;;; Review vars

(defvar gnosis-review-types '("Due themata"
			      "Due themata of specified tag(s)"
			      "Overdue themata"
			      "Due themata (Without Overdue)"
			      "All themata of tag(s)"))

(defvar gnosis-review-buffer-name "*gnosis*"
  "Review buffer name.")

(defvar gnosis-review--display-buffer nil
  "Captured destination for native review rendering and navigation.
Bind around an encounter or action so renaming cannot redirect its output.
Standalone display callers still use `gnosis-review-buffer-name'.")

(defcustom gnosis-review-basic-input 'typed
  "Input style for basic questions.
Typed answers use string comparison with a correctable verdict.
Self-grade asks for recall, reveals the answer/checklist and parathema,
then asks for binary success.  Neither revealing nor editing accepts a grade."
  :type '(choice (const typed) (const self-grade))
  :group 'gnosis)

(defvar gnosis-practice-completed-hook nil
  "Hook run with one event after a native practice batch completes.
Each function receives a fresh API v1 plist: :api-version 1, :mode
\"practice\", :session-id, :database (main SQLite filename), and :connection
for the originating open connection.  No answers are included.  Bind
`gnosis-db' to :connection when querying origin results with
`gnosis-agent-results'; do not assume the current database is the origin.

Delivery is synchronous, after evidence/checkpoint commit, input teardown
and summary presentation.  Errors and quits in one subscriber are reported
without preventing others.  Subscribers should be quick and read-only;
there is no sandbox or durable delivery queue.  A crash can lose delivery.
Reopening a summary does not notify.  Undo and subsequent completion may
notify again: deduplicate by (:database :session-id), or query results again
when tracking corrections.  Empty, cancelled and unfinished batches do not
notify.  No optional agent integration is required.")

;;; Review state

(cl-defstruct (gnosis-review-state (:constructor gnosis-review-state-create))
  "State for a review session.
DATABASE is the owning open connection, never part of persisted data."
  (reviewed 0 :type integer)
  (total 0 :type integer)
  (remaining nil :type list)
  (requeued nil :type list)
  (mode 'due)
  session-id
  (initial 0)
  outcomes
  skipped
  event-id
  undo
  last-event
  last-correction
  persistent-p
  database
  basic-input
  policy
  selected
  selection
  cancelled-p
  launch-token)

(defvar gnosis-review--running nil
  "Identity of the study session currently presenting native input, or nil.")

(defvar-local gnosis-review--state nil
  "Buffer-local review state for the current session.")

(defvar-local gnosis-review--retired nil
  "Non-nil after this review buffer lost its native lifetime.")

(defvar-local gnosis-review--setup-owner nil
  "Identity of the current buffer setup, cleared on retirement.
Setup retains this occurrence across hooks, even if a mode change clears
the buffer-local variables or reenters setup in the same buffer.")

(defun gnosis-review--watch-buffer (&optional owner)
  "Reject a retired review buffer and watch its native lifetime.
If OWNER is non-nil, require that exact setup occurrence and buffer."
  (when (or gnosis-review--retired buffer-file-name
            (and owner
                 (not (and (eq owner gnosis-review--setup-owner)
                           (eq (car owner) (current-buffer))))))
    (user-error "Review buffer was repurposed; resume in a new buffer"))
  (dolist (hook '(after-set-visited-file-name-hook change-major-mode-hook
                 kill-buffer-hook))
    (add-hook hook #'gnosis-review--retire-buffer nil t)))

(defvar-local gnosis-review--summary-target nil
  "Database connection and frozen checkpoint displayed by this summary.
The cons (DATABASE . DATA) owns only this connection and checkpoint, not
another database with matching IDs or a newer checkpoint of the same batch.")

(defvar gnosis-review--monkeytype-text nil
  "Text to monkeytype on failed review, or nil.
Set by type-specific review functions, consumed by
`gnosis-review-process-thema'.")

(defun gnosis-review--header-line ()
  "Return centered header string derived from `gnosis-review--state'."
  (when gnosis-review--state
    (let ((reviewed (gnosis-review-state-reviewed gnosis-review--state))
	  (total (gnosis-review-state-total gnosis-review--state)))
      (gnosis-center-string
       (format "%s %s %s"
	       (propertize (number-to-string reviewed)
			   'face 'font-lock-type-face)
	       (propertize "/" 'face 'font-lock-comment-face)
	       (propertize (number-to-string total)
			   'face 'gnosis-face-false))))))

(defun gnosis-review--setup-buffer (themata &optional mode)
  "Create or reset the review buffer for THEMATA.
Sets `gnosis-mode', initializes state struct, and installs `:eval' header.
Returns the buffer.  MODE defaults to due; practice never reschedules."
  (let ((buf (get-buffer-create gnosis-review-buffer-name)))
    (with-current-buffer buf
      (when (or gnosis-review--retired buffer-file-name
                (and (not (eq major-mode 'gnosis-mode)) (> (buffer-size) 0)))
        (user-error "Review buffer contains unrelated content; rename it first"))
      (let ((initialize (not (eq major-mode 'gnosis-mode)))
            (owner (list buf)))
        (if initialize
            (let ((gnosis--mode-setup-function
                   (lambda ()
                     ;; Claim after the parent resets locals, but before the
                     ;; mode body calls minor modes and their native hooks.
                     (setq gnosis-review--setup-owner owner)
                     (gnosis-review--watch-buffer owner)
                     (lambda () (gnosis-review--watch-buffer owner)))))
              (delay-mode-hooks (gnosis-mode)))
          (setq gnosis-review--setup-owner owner)
          (gnosis-review--watch-buffer owner))
        (when initialize (run-mode-hooks))
        (gnosis-review--watch-buffer owner)
        (gnosis-review--lookahead-cancel)
        (gnosis-review--watch-buffer owner))
      (setq gnosis-review--state
	    (gnosis-review-state-create
	     :mode (or mode 'due)
             :session-id (gnosis-scheduler-event-id)
             :event-id (gnosis-scheduler-event-id)
             :database (gnosis--ensure-db)
             :basic-input gnosis-review-basic-input
             :selected (copy-sequence themata)
             :initial (length themata)
             :reviewed 0
	     :total (length themata)
	     :remaining (copy-sequence themata)))
      (add-hook 'window-configuration-change-hook #'gnosis-image-refresh nil t)
      (setq header-line-format '(:eval (gnosis-review--header-line))))
    buf))

;;; Display functions

(defvar gnosis-review--display-validate nil
  "Validator captured before native encounter callbacks.
Bind with `gnosis-review--display-buffer' through synchronous rendering.
Deferred callbacks must retain their own encounter context.")

(defun gnosis-review--display-validator ()
  "Return the encounter validator, or capture this standalone display lifetime."
  (or gnosis-review--display-validate
      (let ((buffer (current-buffer))
            (mode major-mode)
            (owner (or gnosis-review--setup-owner
                       (setq gnosis-review--setup-owner (list (current-buffer))))))
        (gnosis-review--watch-buffer owner)
        (lambda ()
          (unless (buffer-live-p buffer) (user-error "Display buffer was killed"))
          (with-current-buffer buffer
            (gnosis-review--watch-buffer owner)
            (unless (eq mode major-mode) (user-error "Display mode changed")))))))

(defvar-local gnosis-review--layout nil
  "Last text tick, centering preference and displaying window sizes.")

(defvar-local gnosis-review--layout-overlays nil
  "Window-specific line prefixes owned by this review buffer.")

(defun gnosis-review--clear-layout ()
  "Remove review line prefixes without changing buffer text."
  (mapc #'delete-overlay gnosis-review--layout-overlays)
  (setq gnosis-review--layout-overlays nil
        gnosis-review--layout nil))

(defun gnosis-review--refresh-layout (&rest _ignored)
  "Update review line prefixes for each displaying window.
Short lines are centered using window-specific pixel measurements; long
lines wrap natively when the window is narrower than their initial filling.
Text, point and window starts are never rewritten.  Ignore repeated calls
with unchanged text and widths."
  (let* ((windows (get-buffer-window-list (current-buffer) nil t))
         (layout (list (buffer-modified-tick) gnosis-center-content
                       (mapcar (lambda (window)
                                 (cons window (window-body-width window t)))
                               windows))))
    (unless (equal layout gnosis-review--layout)
      (gnosis-review--clear-layout)
      (when gnosis-center-content
        (save-excursion
          (dolist (window windows)
            (goto-char (point-min))
            (while (< (point) (point-max))
              (let ((start (point))
                    (end (line-end-position))
                    (width (window-body-width window t)))
                ;; Images and separators have their own display geometry.
                (unless (or (= start end)
                            (text-property-not-all start end 'display nil))
                  (let* ((truncate-lines t)
                         (pixels (car (window-text-pixel-size
                                       window start end (1+ width))))
                         (padding (max 0 (/ (- width pixels) 2))))
                    (when (> padding 0)
                      (let ((overlay (make-overlay start end nil nil t)))
                        (overlay-put overlay 'window window)
                        (overlay-put overlay 'line-prefix
                                     `(space :align-to (,padding)))
                        (push overlay gnosis-review--layout-overlays))))))
              (forward-line 1)))))
      (setq gnosis-review--layout layout))))

(defun gnosis-review--enable-layout ()
  "Enable native, non-destructive wrapping in this review buffer."
  (setq-local word-wrap t)
  (setq-local truncate-lines nil)
  (setq-local truncate-partial-width-windows nil)
  (add-hook 'window-configuration-change-hook
            #'gnosis-review--refresh-layout nil t)
  (add-hook 'after-change-functions #'gnosis-review--refresh-layout nil t)
  (add-hook 'change-major-mode-hook #'gnosis-review--clear-layout nil t))

(defun gnosis-review--unstyle-inline-newlines (str)
  "Return STR without link or cloze answer faces on newline characters.
Preserve other faces, link destinations and all non-newline properties.
Fontification, multiline hints and filling can carry inline faces onto
newlines, where redisplay extends them into otherwise empty display space."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (let* ((start (1- (point)))
             (faces (ensure-list (get-text-property start 'face)))
             (remaining (seq-remove
                         (lambda (face)
                           (memq face '(org-link gnosis-face-cloze
                                        gnosis-face-correct gnosis-face-false
                                        gnosis-face-unanswered)))
                         faces)))
        (unless (equal faces remaining)
          (put-text-property start (point) 'face remaining))
        (when (and (memq 'org-link faces)
                   (eq (get-text-property start 'mouse-face) 'highlight))
          (remove-text-properties start (point) '(mouse-face nil)))))
    (buffer-string)))

(defun gnosis-review--format-string (str &optional literal)
  "Format STR with stable filling and no window-dependent padding.
When centering is enabled, fill prose once to `fill-column'.  Preserve
explicit line breaks and display-bearing lines, including image properties.
Keep inline link and cloze faces off newlines, including filled breaks.
Narrow windows wrap the resulting text natively without rewriting it.
When LITERAL is non-nil, skip link and image interpretation of STR."
  (let ((text (if literal str
                (let ((gnosis-center-content nil)) (gnosis-format-string str))))
        (column fill-column))
    (gnosis-review--unstyle-inline-newlines
     (if (not gnosis-center-content)
         text
       (mapconcat
        (lambda (line)
          (if (text-property-not-all 0 (length line) 'display nil line)
              line
            (with-temp-buffer
              (setq fill-column column)
              (insert (string-trim line))
              (fill-region (point-min) (point-max))
              (buffer-string))))
        (split-string text "\n") "\n")))))

(defun gnosis-display-keimenon (str)
  "Display STR as keimenon."
  (with-current-buffer (or gnosis-review--display-buffer gnosis-review-buffer-name)
    (let* ((validate (gnosis-review--display-validator))
           (text (gnosis-review--format-string str)))
      (funcall validate)
      (gnosis-review--enable-layout)
      (erase-buffer)
      (funcall validate)
      (insert "\n" text)
      (funcall validate)
      (gnosis-insert-separator)
      (funcall validate)
      (when (and gnosis-review--running gnosis-review--state
                 (not (member (gnosis-get 'type 'themata
                                          `(= id ,(car (gnosis-review-state-remaining gnosis-review--state))))
                              '("model" "model-name"))))
        (gnosis-review--lookahead-start))
      (funcall validate))))

(defun gnosis-display-image (keimenon &optional validate)
  "Display image link from KEIMENON in new window.
Call VALIDATE in the captured destination after file and window callbacks,
when non-nil, before returning to or continuing work in that destination."
  (let ((buffer (or gnosis-review--display-buffer
                    (get-buffer gnosis-review-buffer-name)
                    gnosis-review-buffer-name))
        (image-path (and (string-match "\\[file:\\(.*?\\)\\]" keimenon)
			 (match-string 1 keimenon))))
    (when image-path
      (setq validate (or validate
                         (with-current-buffer buffer (gnosis-review--display-validator))))
      (find-file-other-window image-path)
      (when validate (with-current-buffer buffer (funcall validate)))
      (switch-to-buffer-other-window buffer)
      (when validate (with-current-buffer buffer (funcall validate))))))

(defun gnosis-display-cloze-string (str clozes hints correct false &optional remaining)
  "Display STR with CLOZES and HINTS; return the actually shown hints.
Apply highlighting for CORRECT and FALSE answer strings.  With REMAINING,
a vector of original blank indices (including the empty vector), CLOZES
and HINTS are the complete original lists, CORRECT is unused and FALSE
requests failed feedback instead of masking remaining blanks."
  (let* ((gnosis-review--display-buffer
          (or gnosis-review--display-buffer (get-buffer gnosis-review-buffer-name)))
         (gnosis-review--display-validate
          (with-current-buffer gnosis-review--display-buffer (gnosis-review--display-validator)))
         (rendered
          (if remaining
              (gnosis-cloze--render (gnosis-org-format-string str) clozes
                                    (append remaining nil) hints false)
            (let* ((hinted (gnosis-cloze-add-hints (gnosis-cloze-create str clozes) hints nil t))
                   (corrected (gnosis-cloze-highlight (car hinted) correct 'gnosis-face-correct)))
              (cons (gnosis-cloze-mark-false corrected false) (cdr hinted))))))
    (funcall gnosis-review--display-validate)
    (gnosis-display-keimenon (car rendered))
    (cdr rendered)))

(defun gnosis-display-basic-answer (answer success user-input)
  "Display ANSWER and, unless SUCCESS, the literal USER-INPUT."
  (with-current-buffer (or gnosis-review--display-buffer gnosis-review-buffer-name)
    (let* ((validate (gnosis-review--display-validator))
           (text (gnosis-review--format-string
                  (concat (propertize "Answer:" 'face 'gnosis-face-directions)
                          " " (propertize (gnosis-image-format-string answer)
                                          'face 'gnosis-face-correct)) t))
           (wrong (unless success
                    (gnosis-review--format-string
                     (concat (propertize "Your answer:" 'face 'gnosis-face-directions)
                             " " (propertize user-input 'face 'gnosis-face-false)) t))))
      (funcall validate)
      (goto-char (point-max))
      (insert "\n\n" text (if wrong (concat "\n" wrong) ""))
      (funcall validate))))

(defun gnosis-display-hint (hint)
  "Display HINT."
  (unless (or (null hint) (string-empty-p hint))
    (let* ((validate (gnosis-review--display-validator))
           (text (gnosis-review--format-string
                  (propertize hint 'face 'gnosis-face-hint))))
      (funcall validate)
      (goto-char (point-max))
      (insert "\n" text)
      (funcall validate)
      (gnosis-insert-separator)
      (funcall validate))))

(defun gnosis-display-cloze-user-answer (user-input &optional false)
  "Display literal USER-INPUT, using the incorrect face when FALSE is non-nil."
  (let* ((validate (gnosis-review--display-validator))
         (text (gnosis-review--format-string
                (concat (propertize "Your answer:" 'face 'gnosis-face-directions)
                        " " (propertize user-input 'face
                                        (if false 'gnosis-face-false 'gnosis-face-correct))) t)))
    (funcall validate)
    (goto-char (point-max))
    (insert "\n\n" text "\n")
    (funcall validate)))

(defun gnosis-display-correct-answer-mcq (answer user-choice)
  "Display correct ANSWER and USER-CHOICE for an MCQ thema."
  (let* ((validate (gnosis-review--display-validator))
         (text (gnosis-review--format-string
                (format "%s %s\n%s %s"
                        (propertize "Correct Answer:" 'face 'gnosis-face-directions)
                        (propertize answer 'face 'gnosis-face-correct)
                        (propertize "Your answer:" 'face 'gnosis-face-directions)
                        (propertize user-choice 'face (if (string= answer user-choice)
                                                        'gnosis-face-correct
                                                      'gnosis-face-false))))))
    (funcall validate)
    (goto-char (point-max))
    (insert "\n\n" text "\n")
    (funcall validate)
    (gnosis-insert-separator)
    (funcall validate)))

(defun gnosis-display-parathema (parathema)
  "Display PARATHEMA only if its destination survives formatting callbacks."
  (when (and parathema (not (string-empty-p parathema)))
    (let* ((validate (gnosis-review--display-validator))
           (text (gnosis-review--format-string (gnosis-org-format-string parathema))))
      (funcall validate)
      (goto-char (point-max))
      (insert "\n" text "\n")
      (funcall validate))))

(defvar-local gnosis-review--status nil
  "Overlay delimiting this setup's scheduling status, never authored prose.")

(defun gnosis-display-next-review (interval success)
  "Display INTERVAL as next review date.
SUCCESS controls the face used when overriding a previous display."
  (with-current-buffer (or gnosis-review--display-buffer gnosis-review-buffer-name)
    (let* ((validate (gnosis-review--display-validator))
           (message (when interval
                      (concat (propertize "Next review:" 'face 'gnosis-face-directions)
                              " " (propertize
                                   (replace-regexp-in-string
                                    "[]()[:space:]]"
                                    (lambda (match) (if (string= match " ") "/" ""))
                                    (format "%s" interval) t t)
                                   'face 'gnosis-face-next-review))))
           (text (and message (gnosis-review--format-string message))))
      (funcall validate)
      (let ((replace (and (overlayp gnosis-review--status)
                          (eq (overlay-buffer gnosis-review--status) (current-buffer))
                          (eq (overlay-get gnosis-review--status 'owner)
                              gnosis-review--setup-owner))))
        (if replace
            (progn
              (goto-char (overlay-start gnosis-review--status))
              (delete-region (point) (overlay-end gnosis-review--status)))
          (goto-char (point-max))
          (insert "\n\n"))
        (funcall validate)
        (let ((start (point)))
          (insert (cond ((null interval) (propertize "Practice: schedule unchanged" 'face 'shadow))
                        (replace (propertize message 'face (if success 'gnosis-face-correct 'gnosis-face-false)))
                        (t text)))
          (funcall validate)
          (setq gnosis-review--status (make-overlay start (point)))
          (overlay-put gnosis-review--status 'evaporate t)
          (overlay-put gnosis-review--status 'owner gnosis-review--setup-owner)))
      (funcall validate))))

;;; Link view mode

(defun gnosis-get-linked-nodes (id)
  "Return the title of linked node(s) for thema ID."
  (let ((links (gnosis-select 'dest 'thema-links `(= source ,id) t)))
    (when links
      (mapcar #'car
	      (gnosis-sqlite-select-batch (gnosis--ensure-db)
					  "SELECT title FROM nodes WHERE id IN (%s)"
					  links)))))

(declare-function gnosis-lecture-sources "gnosis-lecture" (text))
(declare-function gnosis-lecture-open "gnosis-lecture" (path &optional argument validate))

(defun gnosis-view-linked-node (id &optional validate)
  "Visit linked nodes or external lecture sources for thema ID.
When non-nil, call VALIDATE before and after navigation callbacks."
  (when validate (funcall validate))
  (let* ((ids (gnosis-select 'dest 'thema-links `(= source ,id) t))
         (candidates (append (and ids (gnosis-study-topic-candidates ids))
                             (progn
                               (require 'gnosis-lecture)
                               (gnosis-lecture-sources
                                (gnosis-get 'parathema 'extras `(= id ,id)))))))
    (unless candidates (user-error "No indexed source for this thema"))
    (let ((node (cdr (assoc (completing-read "Source: " candidates nil t)
                            candidates))))
      (when validate (funcall validate))
      (window-configuration-to-register :gnosis-link-view)
      (when validate (funcall validate))
      (if (and (consp node) (eq (car node) 'lecture))
          (gnosis-lecture-open (cdr node) nil validate)
        (gnosis-nodes-goto-id node))
      (when validate (funcall validate))
      (gnosis-link-view-mode)
      (when validate (funcall validate)))))

(defun gnosis-link-view--exit ()
  "Exit link view mode."
  (interactive nil gnosis-link-view-mode)
  (gnosis-link-view-mode -1)
  (jump-to-register :gnosis-link-view)
  (exit-recursive-edit))

(defvar-keymap gnosis-link-view-mode-map
  :doc "Keymap for `gnosis-link-view-mode'."
  "C-c C-c" #'gnosis-link-view--exit)

(define-minor-mode gnosis-link-view-mode "Gnosis Link View."
  :interactive nil
  :lighter " Gnosis Link View"
  :keymap gnosis-link-view-mode-map
  (if gnosis-link-view-mode
      (setq-local header-line-format
		  (substitute-command-keys
		   " Return to review with: \\[gnosis-link-view--exit]"))
    (setq-local header-line-format nil)))

;;; Due/scheduling

(defun gnosis-review-is-due-p (thema-id)
  "Check if thema with value of THEMA-ID for id is due for review.

Check suspension and whether it is due today."
  (and (not (gnosis-suspended-p thema-id))
       (gnosis-review-is-due-today-p thema-id)))

(defun gnosis-review-is-due-today-p (id)
  "Return t if thema with ID is due today.

This function ignores if thema is suspended.  Refer to
`gnosis-review-is-due-p' if you need to check for suspended value as
well."
  (let ((due-day (gnosis-get 'due-day 'scheduler-state
                             `(= thema-id ,id))))
    (<= due-day (gnosis--today-int))))

(defun gnosis-review-get--due-themata ()
  "Return due thema IDs & due dates."
  (let* ((db (gnosis--ensure-db))
         (today (gnosis--today-int))
	 (old-themata
          (gnosis-sqlite-select
           db "SELECT thema_id, due_day FROM scheduler_state
                WHERE reps > 0 AND suspended = 0 AND due_day <= ?
                ORDER BY due_day, thema_id" (list today)))
	 (new-themata
          (gnosis-sqlite-select
           db "SELECT thema_id, due_day FROM scheduler_state
                WHERE reps = 0 AND suspended = 0 AND due_day <= ?
                ORDER BY due_day, thema_id" (list today))))
    (let ((limited-new (if gnosis-new-themata-limit
			   (cl-subseq new-themata 0 (min gnosis-new-themata-limit
							 (length new-themata)))
			 new-themata)))
      (if gnosis-review-new-first
	  (append limited-new old-themata)
	(append old-themata limited-new)))))

(defun gnosis-review-count-due ()
  "Return the number of due themata, respecting the new-thema limit.
Count in SQLite rather than materializing the review queue for a badge."
  (pcase-let ((`((,old ,new))
               (gnosis-sqlite-select
                (gnosis--ensure-db)
                "SELECT COUNT(CASE WHEN reps > 0 THEN 1 END),
                        COUNT(CASE WHEN reps = 0 THEN 1 END)
                   FROM scheduler_state
                  WHERE suspended = 0 AND due_day <= ?"
                (list (gnosis--today-int)))))
    ;; Match `cl-subseq' in the queue selector, including negative end indices.
    (let* ((end (min new (or gnosis-new-themata-limit new)))
           (limited-new (if (< end 0) (+ new end) end)))
      (when (< limited-new 0)
        (error "End index out of bounds: %s" gnosis-new-themata-limit))
      (+ old limited-new))))

(defun gnosis-review-get-due-themata ()
  "Return all due thema IDs."
  (mapcar #'car (gnosis-review-get--due-themata)))

(defun gnosis-review-get-overdue-themata ()
  "Return IDs of overdue themata (reviewed at least once, due before today)."
  (let ((today (gnosis--today-int)))
    (mapcar #'car
            (gnosis-sqlite-select
             (gnosis--ensure-db)
             "SELECT thema_id FROM scheduler_state
               WHERE reps > 0 AND suspended = 0 AND due_day < ?
               ORDER BY due_day, thema_id" (list today)))))

(defun gnosis-review-count-overdue ()
  "Return count of overdue themata."
  (let ((today (gnosis--today-int)))
    (or (caar (gnosis-sqlite-select (gnosis--ensure-db)
				    "SELECT COUNT(*) FROM scheduler_state
                                      WHERE reps > 0 AND suspended = 0
                                        AND due_day < ?"
				    (list today)))
	0)))

;;; Scheduler bridge

(defun gnosis-review--pending-result
    (id success &optional event-id reviewed-at-us review-day)
  "Return pending binary review for ID and SUCCESS without mutation.
EVENT-ID, REVIEWED-AT-US, and REVIEW-DAY may pin deterministic facts.
Derive an omitted REVIEW-DAY from REVIEWED-AT-US, capturing the current
instant once when REVIEWED-AT-US is also omitted."
  (let* ((event-id (or event-id
                       (and gnosis-review--state (gnosis-review-state-event-id gnosis-review--state))
                       (gnosis-scheduler-event-id)))
         (reviewed-at-us
          (or reviewed-at-us (car (time-convert nil 1000000))))
         (review-day
          (or review-day
              (gnosis--date-to-int
               (gnosis-date nil (cons reviewed-at-us 1000000)))))
         (outcome (if success 'success 'failure)))
    (list :event-id event-id :thema-id id :outcome outcome
          :reviewed-at-us reviewed-at-us :review-day review-day
          :preview (gnosis-scheduler-preview-review
                    event-id id outcome reviewed-at-us review-day))))

(defun gnosis-review-algorithm (id success)
  "Return pending FSRS review for thema ID and binary SUCCESS."
  (if (and gnosis-review--state
           (eq (gnosis-review-state-mode gnosis-review--state) 'practice))
      (list :mode 'practice :event-id (or (gnosis-review-state-event-id gnosis-review--state)
                                           (gnosis-scheduler-event-id))
            :thema-id id :outcome (if success 'success 'failure)
            :reviewed-at-us (car (time-convert nil 1000000))
            :session-id (gnosis-review-state-session-id gnosis-review--state)
            :attempt (1+ (or (caar (gnosis-sqlite-select
                                   (gnosis--ensure-db)
                                   "SELECT MAX(attempt) FROM practice_events WHERE session_id = ?"
                                   (list (gnosis-review-state-session-id gnosis-review--state)))) 0)))
    (gnosis-review--pending-result id success)))

(defun gnosis-review--override-result (result success)
  "Return RESULT preview recomputed for binary SUCCESS."
  (let ((pending (copy-sequence result)))
    ;; Merge only recomputed facts; the constructor retains nil/default semantics.
    (cl-loop for (key value) on
             (if (eq (plist-get result :mode) 'practice)
                 (list :outcome (if success 'success 'failure))
               (gnosis-review--pending-result
                (plist-get result :thema-id) success
                (plist-get result :event-id) (plist-get result :reviewed-at-us)
                (plist-get result :review-day)))
             by #'cddr do (setq pending (plist-put pending key value)))
    pending))

(defun gnosis-review--result-date (result)
  "Return next review date from pending RESULT."
  (unless (eq (plist-get result :mode) 'practice)
    (gnosis--int-to-date
     (plist-get (plist-get result :preview) :due-day))))

(defun gnosis-review--check-result-content (id result)
  "Validate pending RESULT's content and encounter owners for thema ID.
A media result retains (DATABASE THEMA BUFFER STATE SNAPSHOT), followed
by verified fields for models.  Its owner must still match; only the last
committed persistent attempt may retry after session advancement."
  (when-let* ((owner (plist-get result :content)))
    (gnosis-review--content-check id owner result))
  (dolist (key '(:model :image))
    (when-let* ((model (plist-get result key)))
      (let ((state (nth 3 model))
            (snapshot (nth 4 model)))
        (unless (and (eq (nth 2 model) (current-buffer))
                     (eq state gnosis-review--state)
                     (eq (plist-get snapshot :mode) (gnosis-review-state-mode state))
                     (or (equal snapshot (gnosis-review--state-data state))
                         (and (gnosis-review-state-persistent-p state)
                              (equal (plist-get snapshot :session-id)
                                     (gnosis-review-state-session-id state))
                              (equal (plist-get result :event-id)
                                     (gnosis-review-state-last-event state)))))
          (user-error "Media answer belongs to an outdated encounter")))
      (unless (and (eq (car model) (gnosis--ensure-db))
                   (or (gnosis-review--edited-content-p id result)
                       (equal (cadr model)
                              (if (eq key :image) (gnosis-review--image-thema id)
                                (gnosis-review--answer-thema id)))))
        (user-error "Media answer belongs to an outdated thema or database"))
      (let ((row (car (cadr model))))
        (if (eq key :image)
            (gnosis-review--image-validate row)
          (gnosis--validate-accepted-aliases (nth 0 row) (nth 3 row) (nth 4 row))
          (if (nth 5 model)
              (gnosis-model-check-fields (nth 5 model))
            (gnosis-model-fields (nth 0 row) (nth 2 row) (nth 3 row))))))))

(defun gnosis-review--write-result (id success result)
  "Accept pending RESULT for thema ID and binary SUCCESS.
Validate its encountered content, then commit the retained scheduler facts."
  (gnosis-review--check-result-content id result)
  (let ((outcome (if success 'success 'failure)))
    (unless (and (= id (plist-get result :thema-id))
                 (eq outcome (plist-get result :outcome)))
      (error "Review result does not match final outcome"))
    (if (eq (plist-get result :mode) 'practice)
        (gnosis-study-accept-practice result)
      (gnosis-scheduler-accept-review
       (plist-get result :event-id) id outcome
       (plist-get result :reviewed-at-us)
       (plist-get result :review-day)
       (plist-get (plist-get result :preview) :config-id)
       (plist-get result :preview)))))

(defun gnosis-review--state-data (state)
  "Encode STATE as plain versioned session data, without buffer state."
  (list :version 1 :session-id (gnosis-review-state-session-id state)
        :mode (gnosis-review-state-mode state) :initial (gnosis-review-state-initial state)
        :total (gnosis-review-state-total state) :reviewed (gnosis-review-state-reviewed state)
        :remaining (gnosis-review-state-remaining state) :requeued (gnosis-review-state-requeued state)
        :outcomes (gnosis-review-state-outcomes state) :skipped (gnosis-review-state-skipped state)
        :event-id (gnosis-review-state-event-id state) :undo (gnosis-review-state-undo state)
        :last-event (gnosis-review-state-last-event state)
        :last-correction (gnosis-review-state-last-correction state)
        :basic-input (gnosis-review-state-basic-input state)
        :policy (gnosis-review-state-policy state)
        :selected (gnosis-review-state-selected state)
        :selection (gnosis-review-state-selection state)
        :cancelled-p (gnosis-review-state-cancelled-p state)
        :launch-token (gnosis-review-state-launch-token state)))

(defun gnosis-review--check-frozen-policy (policy)
  "Validate complete frozen POLICY without filling omitted fields.
Nil retains the legacy one-retry session policy."
  (when policy
    (unless (= (length policy) (length (gnosis-review-practice-policy policy)))
      (error "Incomplete frozen practice policy"))))

(defun gnosis-review--read-session ()
  "Return the retained session state, or nil."
  (when-let* ((data (gnosis-get 'data 'study-session '(= id 1))))
    (unless (equal 1 (plist-get data :version)) (error "Unsupported study session format"))
    (gnosis-review--check-frozen-policy (plist-get data :policy))
    (apply #'gnosis-review-state-create :persistent-p t
           :database (gnosis--ensure-db) (cddr data))))

(defun gnosis-review--session-target ()
  "Return the current database and normalized checkpoint, ignoring buffers."
  (cons (gnosis--ensure-db)
        (when-let* ((state (gnosis-review--read-session)))
          (gnosis-review--state-data state))))

(defun gnosis-review--save-session (state)
  "Persist STATE on the caller's transaction."
  (gnosis-sqlite-execute (gnosis--ensure-db)
                         "INSERT OR REPLACE INTO study_session VALUES (1, ?)"
                         (list (gnosis-review--state-data state)))
  (gnosis-review--save-history state))

(defun gnosis-review--save-history (state)
  "Retain STATE by identity on the caller's transaction."
  (gnosis-sqlite-execute
   (gnosis--ensure-db)
   "INSERT INTO study_history VALUES (?, ?)
    ON CONFLICT(session_id) DO UPDATE SET data = excluded.data"
   (list (gnosis-review-state-session-id state)
         (gnosis-review--state-data state))))

(defun gnosis-review-practice-policy (&optional policy)
  "Validate POLICY overrides and return a fresh frozen practice policy.
Require positive integer targets and a boolean consecutive flag.
An explicit :max-attempts-per-thema nil means unlimited; omission keeps the
finite default.  Reject unknown and duplicate keys and malformed plists."
  (let ((defaults '(:successes-required 1 :successes-after-failure 2
                   :consecutive t :max-attempts-per-thema 5))
        seen)
    (unless (and (proper-list-p policy) (zerop (% (length policy) 2)))
      (user-error "Practice policy must be a keyword plist"))
    (cl-loop for (key value) on policy by #'cddr do
             (unless (and (plist-member defaults key) (not (memq key seen))
                          (cond ((eq key :consecutive) (memq value '(nil t)))
                                ((and (eq key :max-attempts-per-thema) (null value)) t)
                                (t (and (integerp value) (> value 0)))))
               (user-error "Invalid practice policy field: %S" key))
             (push key seen))
    (cl-loop for (key value) on defaults by #'cddr append
             (list key (if (plist-member policy key) (plist-get policy key) value)))))

(defun gnosis-review-policy-progress (policy outcomes)
  "Return practice progress for POLICY and newest-first boolean OUTCOMES.
Count effective accepted grades, not voided grades.  Consecutive successes
reset on failure; otherwise count all successes.  Any failure selects the
failure target.  A reached target takes precedence over the attempt cap."
  (let* ((attempts (length outcomes))
         (failed (memq nil outcomes))
         (target (plist-get policy (if failed :successes-after-failure
                                    :successes-required)))
         (successes (if (plist-get policy :consecutive)
                        (length (seq-take-while #'identity outcomes))
                      (seq-count #'identity outcomes))))
    (list :attempts attempts :successes successes :target target
          :reason (cond ((>= successes target) "target-reached")
                        ((and (plist-get policy :max-attempts-per-thema)
                              (>= attempts (plist-get policy :max-attempts-per-thema)))
                         "attempt-limit")
                        (t "unfinished")))))

(defun gnosis-review--replace-session (state target)
  "Install nonempty STATE in place of the exact database/checkpoint TARGET.
End unfinished progress early, preserving accepted evidence and historical
remaining membership.  The existing cancelled flag distinguishes this from
completion.  Invalidate deferred launches without invoking adapter code."
  (when gnosis-review--running (user-error "Finish the active review first"))
  (unless (gnosis-review-state-remaining state) (error "Cannot replace with an empty batch"))
  (gnosis-review--check-database state)
  (gnosis-sqlite-with-transaction (car target)
    (gnosis-review--check-action-target target)
    (when-let* ((old (gnosis-review--read-session))
                ((gnosis-review-state-remaining old)))
      (setf (gnosis-review-state-cancelled-p old) t
            (gnosis-review-state-launch-token old) nil)
      (gnosis-review--save-history old))
    (gnosis-review--save-session state)))

(defun gnosis-review--reserve-practice (ids policy selection &optional target)
  "Reserve IDS for native practice with frozen POLICY and SELECTION metadata.
Return durable state without displaying a buffer or asking for an answer.
Replace optional database/checkpoint TARGET, defaulting to the current batch.
An empty selection retains only its report, leaving the current batch intact."
  (when gnosis-review--running (user-error "Finish the active review first"))
  (let* ((target (or target (gnosis-review--session-target)))
         (policy (gnosis-review-practice-policy policy))
         (state (gnosis-review-state-create
                 :mode 'practice :persistent-p t :database (car target)
                 :session-id (gnosis-scheduler-event-id)
                 :event-id (gnosis-scheduler-event-id)
                 :basic-input gnosis-review-basic-input
                 :policy policy :selection selection
                 :selected (copy-sequence ids) :remaining (copy-sequence ids)
                 :initial (length ids) :total (length ids))))
    (if ids
        (gnosis-review--replace-session state target)
      (gnosis-sqlite-with-transaction (car target)
        (gnosis-review--check-action-target target)
        (gnosis-review--save-history state)))
    state))

(defun gnosis-review--advance (state id success eligible next-event &optional skipped)
  "Return a fresh STATE advanced after ID and SUCCESS, or SKIPPED presentation.
ELIGIBLE says whether ID can be retried.  NEXT-EVENT is the next attempt ID.
Read no database or clock and leave STATE and its prior snapshots unchanged."
  (let* ((state (copy-gnosis-review-state state))
         (rest (cdr (gnosis-review-state-remaining state)))
         (retry (and (not skipped)
                    (if (gnosis-review-state-policy state)
                        (equal "unfinished"
                               (plist-get
                                (gnosis-review-policy-progress
                                 (gnosis-review-state-policy state)
                                 (cons success
                                       (mapcar #'cdr
                                               (seq-filter
                                                (lambda (row) (equal id (car row)))
                                                (gnosis-review-state-outcomes state)))))
                                :reason))
                      (and (not success)
                           (not (member id (gnosis-review-state-requeued state)))))))
         (tail (and retry eligible)))
    (setf (gnosis-review-state-remaining state) (if tail (append rest (list id)) rest)
          (gnosis-review-state-event-id state) next-event)
    ;; Retain why a required continuation was dropped, independently of its
    ;; accepted grade and of later eligibility changes.
    (when (or skipped (and retry (not tail)))
      (push id (gnosis-review-state-skipped state)))
    (unless skipped
      (push (cons id success) (gnosis-review-state-outcomes state))
      (cl-incf (gnosis-review-state-reviewed state)))
    (when tail
      (push id (gnosis-review-state-requeued state))
      (cl-incf (gnosis-review-state-total state)))
    state))

(defun gnosis-review--copy-state (state source)
  "Copy SOURCE fields onto the existing UI STATE object and return STATE."
  (cl-loop for i from 1 below (length state) do (aset state i (aref source i)))
  state)

(defun gnosis-review--check-database (state)
  "Reject persistent STATE if its owning database is no longer current."
  (when (and (gnosis-review-state-persistent-p state)
             (not (eq (gnosis-review-state-database state) (gnosis--ensure-db))))
    (user-error "Study database changed; resume in its original database")))

(defun gnosis-review--skip (state id)
  "Skip unavailable ID in STATE without overwriting newer durable progress."
  (gnosis-review--check-database state)
  (if (not (gnosis-review-state-persistent-p state))
      (gnosis-review--copy-state
       state (gnosis-review--advance state id nil nil (gnosis-scheduler-event-id) t))
    (let ((stored
           (gnosis-sqlite-with-transaction (gnosis--ensure-db)
             (let ((current (gnosis-review--read-session)))
               (unless (and current
                            (equal id (car (gnosis-review-state-remaining current)))
                            (equal (gnosis-review--state-data current)
                                   (gnosis-review--state-data state)))
                 (user-error "Study attempt is stale; resume the retained batch"))
               (when (gnosis-study-eligible-p id)
                 (user-error "Thema is available again; resume the batch"))
               (let ((next (gnosis-review--advance
                            current id nil nil (gnosis-scheduler-event-id) t)))
                 (gnosis-review--save-session next)
                 next)))))
      (gnosis-review--copy-state state stored))))

(defun gnosis-review-result (id success result)
  "Atomically accept RESULT for ID/SUCCESS and settle durable session progress."
  (let* ((state gnosis-review--state)
         (persistent (and state (gnosis-review-state-persistent-p state)))
         (db (gnosis--ensure-db))
         (accepted
          (gnosis-sqlite-with-transaction db
            (let ((stored (and persistent (gnosis-review--read-session))))
              (when persistent
                (gnosis-review--check-database state)
                (unless (and stored
                             (equal (gnosis-review-state-session-id stored)
                                    (gnosis-review-state-session-id state))
                             (or (equal (plist-get result :event-id)
                                        (gnosis-review-state-last-event stored))
                                 (and (equal id (car (gnosis-review-state-remaining stored)))
                                      (equal (plist-get result :event-id)
                                             (gnosis-review-state-event-id stored)))))
                  (error "Study attempt is stale")))
              (let ((accepted (gnosis-review--write-result id success result)))
                (when (and persistent
                           (not (equal (plist-get result :event-id)
                                       (gnosis-review-state-last-event stored))))
                  ;; Keep one checkpoint, never a chain of old checkpoints.
                  (let* ((before (plist-put (gnosis-review--state-data stored) :undo nil))
                         (next (gnosis-review--advance
                                stored id success (gnosis-study-eligible-p id)
                                (gnosis-scheduler-event-id))))
                    (setf (gnosis-review-state-undo next)
                          (list :event-id (plist-get result :event-id) :thema-id id
                                :correction-id (gnosis-scheduler-event-id) :before before)
                          (gnosis-review-state-last-event next) (plist-get result :event-id))
                    (gnosis-review--save-session next)))
                accepted)))))
    ;; Database authority has committed before UI projection.  A retry after
    ;; a UI failure resolves the retained event and settled checkpoint.
    (when persistent
      (gnosis-review--copy-state state (gnosis-review--read-session)))
    (when gnosis-due-themata-total
      (setq gnosis-due-themata-total (length (gnosis-review-get-due-themata))))
    accepted))

;;; Model encounters

(defvar-local gnosis-review--lookahead nil
  "One owned next-card preparation, never a renderer or review result.
The private plist retains buffer, state/checkpoint, database, thema, child,
poll timer and either prepared fields or a transferred foreground context.")

(defun gnosis-review--lookahead-cancel ()
  "Retire the review buffer's speculative preparation and pending delivery."
  (when-let* ((slot gnosis-review--lookahead))
    (setq gnosis-review--lookahead nil)
    (when (timerp (plist-get slot :timer))
      (cancel-timer (plist-get slot :timer)))
    (gnosis-model-cancel-preparation (plist-get slot :job))
    (setf (plist-get slot :fields) nil)))

(defun gnosis-review--lookahead-valid-p (slot &optional checkpoint)
  "Return non-nil if SLOT still owns its exact next card and session.
CHECKPOINT supplies the pre-acceptance state during an authorized advancement."
  (let ((state gnosis-review--state)
        (id (plist-get slot :id)))
    (and (not gnosis-review--retired) (not buffer-file-name)
         (eq slot gnosis-review--lookahead)
         (eq (plist-get slot :buffer) (current-buffer))
         (eq (plist-get slot :database) gnosis-db)
         (eq (plist-get slot :state) state)
         (equal gnosis-review--running (gnosis-review-state-session-id state))
         (equal (plist-get slot :checkpoint)
                (or checkpoint (gnosis-review--state-data state)))
         (or checkpoint
             (equal id (if (plist-get slot :advanced)
                           (car (gnosis-review-state-remaining state))
                         (cadr (gnosis-review-state-remaining state)))))
         (gnosis-study-eligible-p id)
         (equal (plist-get slot :thema) (gnosis-review--content-thema id))
         (or (not (gnosis-review-state-persistent-p state))
             (equal (gnosis-review--state-data state)
                    (when-let* ((stored (gnosis-review--read-session)))
                      (gnosis-review--state-data stored)))))))

(defun gnosis-review--lookahead-poll (slot)
  "Retire stale SLOT without parsing assets."
  (when (buffer-live-p (plist-get slot :buffer))
    (with-current-buffer (plist-get slot :buffer)
      (when (and (eq slot gnosis-review--lookahead)
                 (not (condition-case nil (gnosis-review--lookahead-valid-p slot)
                        (error nil))))
        (gnosis-review--lookahead-cancel)))))

(defun gnosis-review--lookahead-delivered (slot fields failure)
  "Retain SLOT's FIELDS without displaying them, or discard FAILURE.
After transfer, deliver through the ordinary foreground ownership checks."
  (if-let* ((context (plist-get slot :foreground)))
      (gnosis-review--model-prepared context fields failure)
    (when (buffer-live-p (plist-get slot :buffer))
      (with-current-buffer (plist-get slot :buffer)
        (gnosis-review--lookahead-poll slot)
        (when (eq slot gnosis-review--lookahead)
          (if failure (gnosis-review--lookahead-cancel)
            (setf (plist-get slot :fields) fields)))))))

(defun gnosis-review--lookahead-start ()
  "Prepare only the next queued model after the current question is displayed.
Do not grade, display future content, or start a renderer.  Speculation errors
are silent; the actual encounter retains its normal responsive loading path."
  (when (and gnosis-review--running gnosis-review--state
             (not gnosis-review--lookahead))
    (condition-case nil
        (when-let* ((_ (progn (gnosis-review--watch-buffer) t))
                    (id (cadr (gnosis-review-state-remaining gnosis-review--state)))
                    (eligible (gnosis-study-eligible-p id))
                    (thema (gnosis-review--content-thema id))
                    (row (car thema))
                    (model (member (car row) '("model" "model-name"))))
          (let ((slot (list :buffer (current-buffer) :state gnosis-review--state
                            :database (gnosis--ensure-db) :id id :thema (copy-tree thema)
                            :checkpoint (copy-tree (gnosis-review--state-data gnosis-review--state))
                            :advanced nil :job nil :timer nil :fields nil :foreground nil)))
            (setq gnosis-review--lookahead slot)
            (setf (plist-get slot :timer)
                  (run-at-time 0.5 0.5 #'gnosis-review--lookahead-poll slot)
                  (plist-get slot :job)
                  (gnosis-model-prepare
                   (car row) (nth 2 row) (nth 3 row)
                   (lambda (fields failure)
                     (gnosis-review--lookahead-delivered slot fields failure))))
            (unless (or (eq slot gnosis-review--lookahead)
                        (plist-get slot :foreground))
              (gnosis-model-cancel-preparation (plist-get slot :job)))))
      (error (gnosis-review--lookahead-cancel)))))

(defun gnosis-review--lookahead-advance (checkpoint)
  "Transfer CHECKPOINT's speculation across ordinary queue advancement.
Only the captured tail, optionally followed by the current card's retry, is
eligible; arbitrary queue replacement must load afresh."
  (when-let* ((slot gnosis-review--lookahead))
    (let* ((old (plist-get checkpoint :remaining))
           (remaining (gnosis-review-state-remaining gnosis-review--state)))
      (if (and (condition-case nil (gnosis-review--lookahead-valid-p slot checkpoint)
                 (error nil))
               (equal (plist-get slot :id) (car remaining))
               (or (equal remaining (cdr old))
                   (equal remaining (append (cdr old) (list (car old))))))
          (setf (plist-get slot :advanced) t
                (plist-get slot :checkpoint)
                (copy-tree (gnosis-review--state-data gnosis-review--state)))
        (gnosis-review--lookahead-cancel)))))

(defun gnosis-review--lookahead-take (context)
  "Move a matching pending or ready preparation into foreground CONTEXT.
Return non-nil on transfer.  The existing foreground checks validate literal
asset bytes before attachment; pending delivery follows the same path."
  (when-let* ((slot gnosis-review--lookahead))
    (gnosis-review--lookahead-poll slot)
    (if (and (eq slot gnosis-review--lookahead)
             (plist-get slot :advanced)
             (equal (plist-get context :id) (plist-get slot :id)))
        (progn
          (setq gnosis-review--lookahead nil)
          (cancel-timer (plist-get slot :timer))
          (setf (plist-get slot :foreground) context
                (plist-get context :preparation) (plist-get slot :job))
          (when-let* ((fields (plist-get slot :fields)))
            (setf (plist-get slot :fields) nil)
            (gnosis-review--model-prepared context fields nil))
          t)
      (gnosis-review--lookahead-cancel)
      nil)))

(defvar-local gnosis-review--model-context nil
  "Owned model encounter context, present only during native input.")

(defun gnosis-review--retire-buffer ()
  "Retire encounter owners before cancelling their outstanding work.
File association is permanent retirement, even if the file is later detached.
Do not unwind native hooks; the old input can still be cancelled normally."
  (let ((context gnosis-review--model-context))
    (setq gnosis-review--retired t gnosis-review--state nil
          gnosis-review--setup-owner nil)
    (when context (setf (plist-get context :cancelled) t))
    (gnosis-review--lookahead-cancel)
    (when context (gnosis-review--model-retire context))))
(declare-function canvas-3d-detach "canvas-3d")
(defvar canvas-3d-mode-map)
(defvar canvas-3d-selection-hook)
(defvar canvas-3d-selected-id)
(defvar canvas-3d--process)
(defvar canvas-3d--image)
(defvar canvas-3d--frame)
(defvar canvas-3d--busy)
(defvar canvas-3d--dirty)
(defvar canvas-3d--yaw)
(defvar canvas-3d--pitch)
(defvar canvas-3d--zoom)
(defvar canvas-3d--status)

(defun gnosis-review--model-header ()
  "Return neutral selection and renderer status, never anatomical labels."
  (let* ((context gnosis-review--model-context)
         (attached (plist-get context :attachment))
         (failure (plist-get context :error))
         (live (and attached (process-live-p canvas-3d--process)))
         (name (eq (plist-get (plist-get context :fields) :response) 'name))
         (status (cond (failure "Unavailable")
                       ((not attached) "Loading…")
                       ((not live) "Unavailable")
                       (t canvas-3d--status)))
         (face (cond ((or failure (and attached (not live))) 'error)
                     ((equal status "Ready") 'success)
                     (t 'warning)))
         (action (cond (failure "Details")
                       (live (if name "Answer" "Submit")))))
    (concat
     " "
     (mapconcat
      #'identity
      (delq nil
            (list (concat (propertize "Model" 'face 'font-lock-type-face)
                          "  "
                          (propertize status 'face face 'help-echo
                                      (or failure (and attached canvas-3d--status))))
                  (when (and live (not failure) (not name))
                    (if (plist-get context :selection)
                        (propertize "Selected" 'face 'match)
                      (propertize "Select" 'face 'warning)))
                  (when action
                    (concat (propertize "RET" 'face 'help-key-binding)
                            " " action))
                  (concat (propertize "q" 'face 'help-key-binding) " Cancel")
                  (concat (propertize "?" 'face 'help-key-binding) " Help")))
      "    "))))

(defun gnosis-review--model-detach (context)
  "Detach CONTEXT's exact attachment, even after its renderer stopped."
  (when (timerp (plist-get context :display-timer))
    (cancel-timer (plist-get context :display-timer)))
  (when (and (plist-get context :attachment)
             (fboundp 'canvas-3d-detach)
             (eq (plist-get context :attachment) canvas-3d--image)
             (or (null canvas-3d--process)
                 (eq (plist-get context :process) canvas-3d--process)))
    (canvas-3d-detach)))

(defun gnosis-review-model-cancel ()
  "Cancel model input without accepting an answer."
  (interactive)
  (let ((context gnosis-review--model-context))
    ;; Cancelling lookahead may reenter foreground delivery.
    (when context (setf (plist-get context :cancelled) t))
    (gnosis-review--lookahead-cancel)
    (when context
      (gnosis-review--model-retire context)
      (when (and (eq gnosis-review--model-context context)
                 (= (recursion-depth) (1+ (plist-get context :depth))))
        (abort-recursive-edit)))))

(defun gnosis-review--model-retire (context)
  "Retire CONTEXT before invoking preparation cancellation callbacks."
  (setf (plist-get context :cancelled) t)
  (gnosis-model-cancel-preparation (plist-get context :preparation))
  (when (buffer-live-p (plist-get context :buffer))
    (with-current-buffer (plist-get context :buffer)
      (when (eq gnosis-review--model-context context)
        (gnosis-review--model-detach context)))))

(defun gnosis-review--model-selection (selection)
  "Retain explicit SELECTION for this encounter without grading."
  (when gnosis-review--model-context
    (setf (plist-get gnosis-review--model-context :selection) nil
          (plist-get gnosis-review--model-context :view) nil)
    ;; Picking requests a highlight frame.  DIRTY means it coalesced behind
    ;; an already pending view, so its old displayed camera is not current.
    (when (and (not canvas-3d--dirty)
               (eq (plist-get gnosis-review--model-context :process) canvas-3d--process)
               (eq (plist-get selection :owner) canvas-3d--process)
               (equal (plist-get selection :frame) (plist-get canvas-3d--frame :seq)))
      (setf (plist-get gnosis-review--model-context :selection)
            (copy-tree selection)
            (plist-get gnosis-review--model-context :view)
            (list canvas-3d--yaw canvas-3d--pitch canvas-3d--zoom)))))

(defun gnosis-review--model-check (context &optional preparing)
  "Validate CONTEXT against its original encounter and current resource.
PREPARING checks only ownership, before verified fields have arrived."
  (let ((owner (plist-get context :buffer))
        (state (plist-get context :state))
        (id (plist-get context :id)))
    (unless (and (not gnosis-review--retired) (not buffer-file-name)
                 (not (plist-get context :cancelled))
                 (not (plist-get context :result))
                 (buffer-live-p owner)
                 (eq owner (current-buffer))
                 (eq (plist-get context :database) (gnosis--ensure-db))
                 (with-current-buffer owner (eq state gnosis-review--state))
                 (equal (plist-get context :state-data) (gnosis-review--state-data state))
                 (equal id (car (gnosis-review-state-remaining state)))
                 (equal (plist-get context :thema)
                        (gnosis-review--answer-thema id))
                 (or (not (gnosis-review-state-persistent-p state))
                     (when-let* ((stored (gnosis-review--read-session)))
                       (equal (plist-get context :state-data)
                              (gnosis-review--state-data stored)))))
      (user-error "Model encounter is outdated; resume the original session"))
    (when-let* ((content (plist-get context :content)))
      (gnosis-review--content-check id content))
    (let ((row (car (plist-get context :thema))))
      (gnosis--validate-accepted-aliases (nth 0 row) (nth 3 row) (nth 4 row))
      (unless preparing
        (unless (plist-get context :fields)
          (user-error "%s" (or (plist-get context :error) "Wait for model preparation")))
        (gnosis-model-check-fields (plist-get context :fields))))))

(defun gnosis-review--model-ready-p (context)
  "Return whether CONTEXT's renderer has a current complete owned frame."
  (and (eq (plist-get context :process) canvas-3d--process)
       (process-live-p canvas-3d--process)
       (eq (plist-get canvas-3d--frame :owner) canvas-3d--process)
       (not canvas-3d--busy) (not canvas-3d--dirty)))

(defun gnosis-review-model-submit ()
  "Submit a picked target, or enter a Name answer, in the current encounter.
Exploratory clicks never submit.  Wait for a stable view before submitting."
  (interactive)
  (let ((context gnosis-review--model-context))
    (unless (and context
                 (= (recursion-depth) (1+ (plist-get context :depth))))
      (user-error "No active model input"))
    (let ((fields (gnosis-review--model-check context)) success)
      (unless (gnosis-review--model-ready-p context)
        (user-error "Wait for the current model view before submitting"))
      (if (eq (plist-get fields :response) 'name)
          (let ((input
                 (condition-case nil
                     (gnosis--read-string-with-input-method "Name: " (plist-get fields :answer))
                   (quit
                    (when (eq gnosis-review--model-context context)
                      (gnosis-review-model-cancel))
                    (signal 'quit nil)))))
            (gnosis-review--model-check context)
            (unless (gnosis-review--model-ready-p context)
              (user-error "Model renderer changed while entering the name"))
            (setf (plist-get context :input) input)
            (setq success (gnosis-answer-match-p
                           (plist-get fields :answer) input (plist-get context :aliases)
                           (plist-get context :tolerance))))
        (unless (and (plist-get context :selection)
                     (equal (plist-get context :view)
                            (list canvas-3d--yaw canvas-3d--pitch canvas-3d--zoom)))
          (user-error "Click a surface in the current view before submitting"))
        (let ((selection (gnosis-model-selection fields)))
          (unless (and selection
                       (cl-every (lambda (key)
                                   (equal (plist-get selection key)
                                          (plist-get (plist-get context :selection) key)))
                                 '(:mesh :face :point)))
            (user-error "Select a surface in the current ready view before submitting"))
          (setf (plist-get context :selection) selection)
          (setq success (equal (plist-get selection :id) (plist-get fields :target)))))
      (let ((result (with-current-buffer (plist-get context :buffer)
                      (gnosis-review-algorithm (plist-get context :id) success))))
      (setq result (plist-put result :model
                              (list (plist-get context :database)
                                    (copy-tree (plist-get context :thema))
                                    (plist-get context :buffer)
                                    (plist-get context :state)
                                    (copy-tree (plist-get context :state-data))
                                    (plist-get context :fields))))
      (setf (plist-get context :result) (cons success result))
        (exit-recursive-edit)))))

(defun gnosis-review--model-input-map (parent)
  "Return a fresh model input map inheriting PARENT, including loading input."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map parent)
    (define-key map (kbd "RET") #'gnosis-review-model-submit)
    (define-key map (kbd "SPC") #'ignore)
    (define-key map (kbd "q") #'gnosis-review-model-cancel)
    (define-key map (kbd "C-g") #'gnosis-review-model-cancel)
    map))

(defun gnosis-review--model-prepared (context fields failure)
  "Install prepared FIELDS or FAILURE only in CONTEXT's still-owned encounter.
Defer attachment while the encounter is hidden; never select another buffer."
  (when (timerp (plist-get context :display-timer))
    (cancel-timer (plist-get context :display-timer))
    (setf (plist-get context :display-timer) nil))
  (when (buffer-live-p (plist-get context :buffer))
    (with-current-buffer (plist-get context :buffer)
      (when (and (eq gnosis-review--model-context context)
                 (not (plist-get context :cancelled))
                 (not (plist-get context :fields))
                 (not (plist-get context :error)))
        (condition-case err
            (progn
              (gnosis-review--model-check context t)
              (when failure (user-error "%s" failure))
              (if-let* ((window (get-buffer-window (current-buffer) t)))
                  (with-selected-window window
                    (gnosis-review--model-check context t)
                    (gnosis-model-check-fields fields)
                    (save-excursion
                      (goto-char (point-max))
                      (gnosis-model-open
                       (plist-get fields :scene) (plist-get fields :view)
                       (min (gnosis-model--canvas-size)
                            (max 128 (- (window-body-height nil t)
                                        (* (+ 5 (count-lines (point-min) (point-max)))
                                           (frame-char-height))))) t
                       (and (eq (plist-get fields :response) 'name) (plist-get fields :target))
                       (plist-get fields :verified)))
                    (gnosis-review--model-check context t)
                    (setf (plist-get context :fields) fields
                          (plist-get context :process) canvas-3d--process
                          (plist-get context :attachment) canvas-3d--image)
                    (use-local-map
                     (gnosis-review--model-input-map
                      (make-composed-keymap (copy-keymap canvas-3d-mode-map)
                                            (plist-get context :map))))
                    (gnosis-review--lookahead-start))
                (setf (plist-get context :display-timer)
                      (run-at-time 0.2 nil #'gnosis-review--model-prepared
                                   context fields nil))))
          (error (setf (plist-get context :error) (error-message-string err)))
          (quit (gnosis-review-model-cancel)))
        (force-mode-line-update)))))

(defun gnosis-review-model (id)
  "Present model ID through native Find or typed Name in a review encounter.
Return the ordinary pending review result; existing review actions accept it.
Preparation runs in a child process while native input remains available;
Use `gnosis-review-model-cancel' to cancel preparation or input.
Missing assets and cancellation never produce a grade."
  (unless gnosis-review--state
    (user-error "Start a review session before answering a model"))
  (let* ((owner (current-buffer))
         (gnosis-review--display-buffer owner)
         (state gnosis-review--state)
         (content-owner (gnosis-review--content-owner id))
         (gnosis-review--display-validate
          (lambda () (gnosis-review--content-check id content-owner)))
         (thema (gnosis-review--answer-thema id))
         (row (car thema))
         (map (current-local-map))
         (header header-line-format)
         (context (list :buffer owner :state state :id id :database (gnosis--ensure-db)
                        :state-data (copy-tree (gnosis-review--state-data state))
                        :thema (copy-tree thema) :content content-owner :fields nil :map map
                        :aliases (copy-tree (nth 4 row)) :error nil :preparation nil
                        :display-timer nil
                        :tolerance gnosis-string-difference :input nil
                        :depth (recursion-depth) :result nil :cancelled nil
                        :process nil :attachment nil :selection nil :view nil)))
    (gnosis-review--model-check context t)
    (gnosis-display-keimenon (gnosis-org-format-string (nth 1 row)))
    (unwind-protect
        (progn
          (goto-char (point-max))
          (insert "\n")
          (funcall gnosis-review--display-validate)
          (setq-local gnosis-review--model-context context)
          (use-local-map (gnosis-review--model-input-map map))
          (setq-local header-line-format '(:eval (gnosis-review--model-header)))
          (add-hook 'canvas-3d-selection-hook #'gnosis-review--model-selection nil t)
          (goto-char (point-min))
          (unless (gnosis-review--lookahead-take context)
            (gnosis-review--model-check context t)
            (setf (plist-get context :preparation)
                  (gnosis-model-prepare
                   (car row) (nth 2 row) (nth 3 row)
                   (lambda (fields failure) (gnosis-review--model-prepared context fields failure)))))
          (gnosis-review--model-check context t)
          (recursive-edit)
          (when (plist-get context :error) (user-error "%s" (plist-get context :error)))
          (unless (plist-get context :result) (user-error "Model input cancelled"))
          (let* ((fields (plist-get context :fields))
                 (scene (plist-get (plist-get fields :verified) :manifest))
                 (target (gnosis-model-target scene (plist-get fields :target))))
            (with-current-buffer owner
              (gnosis-review--content-check id content-owner)
              (setcdr
               (plist-get context :result)
               (gnosis-review--encounter
                (plist-put (cdr (plist-get context :result)) :content content-owner)
                (car (cadr content-owner))
                (if (eq (plist-get fields :response) 'name)
                    (list :kind "text" :text (plist-get context :input))
                  (let ((selection (plist-get context :selection)))
                    (list :kind "surface" :selected-target (plist-get selection :id)
                          :mesh (plist-get selection :mesh) :face (plist-get selection :face)
                          :point (vconcat (plist-get selection :point))
                          :view (vconcat (plist-get context :view)))))
                nil (when (eq (plist-get fields :response) 'name)
                      (plist-get context :tolerance))))
              (gnosis-display-basic-answer
               (or (plist-get fields :answer) (alist-get 'label target))
               (car (plist-get context :result))
               (if (eq (plist-get fields :response) 'name)
                   (plist-get context :input)
                 (if-let* ((selected (plist-get (plist-get context :selection) :id)))
                     (alist-get 'label (gnosis-model-target scene selected))
                   "Unmarked surface")))
              (gnosis-display-parathema (nth 5 (car (cadr content-owner))))
              (gnosis-display-next-review
               (gnosis-review--result-date (cdr (plist-get context :result)))
               (car (plist-get context :result)))))
          (plist-get context :result))
      (gnosis-review--model-retire context)
      (when (timerp (plist-get context :display-timer))
        (cancel-timer (plist-get context :display-timer)))
      (when (buffer-live-p owner)
        (with-current-buffer owner
          ;; Retired input must not restore its map or header over a successor.
          (when (eq gnosis-review--model-context context)
            (setq gnosis-review--model-context nil)
            (when (condition-case nil
                      (progn (gnosis-review--content-check id content-owner) t)
                    (user-error nil))
              (setq header-line-format header)
              (use-local-map map))
            (remove-hook 'canvas-3d-selection-hook #'gnosis-review--model-selection t)))))))

(defun gnosis-review-model-name (id)
  "Review ID by inspecting its highlighted target and typing its name."
  (unless (equal "model-name" (gnosis-get 'type 'themata `(= id ,id)))
    (user-error "Thema is not a model-name question"))
  (gnosis-review-model id))


;;; Image encounters

(defun gnosis-review--image-thema (id)
  "Return ID's type, text, hypothesis, answer, explanation, image and aliases."
  (mapcar (lambda (row)
            (append (butlast row)
                    (car (gnosis-select '[parathema review-image] 'extras `(= id ,id)))
                    (last row)))
          (gnosis-select '[type keimenon hypothesis answer accepted-aliases]
                         'themata `(= id ,id))))

(defun gnosis-review--image-validate (row)
  "Validate image content and aliases from a retained seven-field ROW."
  (apply #'gnosis-image-validate-fields (butlast row))
  (gnosis--validate-accepted-aliases (nth 0 row) (nth 3 row) (nth 6 row)))

(defun gnosis-review--image-owner (id)
  "Validate image content for ID and retain its exact encounter owner, or nil."
  (gnosis-review--watch-buffer)
  (let* ((thema (gnosis-review--image-thema id)) (row (car thema))
         (references (gnosis-image-references (butlast row))))
    (when (or references (member (downcase (car row)) '("image-region" "image-occlusion")))
      (unless gnosis-review--state (user-error "Start a review session first"))
      (gnosis-review--image-validate row)
      ;; Decode hidden answer/explanation images too, without displaying them.
      ;; A broken payload is unavailable, not a failed recall.
      (mapc (lambda (reference) (gnosis-image--decode (gnosis-image-resolve reference))) references)
      (list (gnosis--ensure-db) (copy-tree thema) (current-buffer)
            gnosis-review--state (copy-tree (gnosis-review--state-data gnosis-review--state))))))

(defun gnosis-review--image-check (id owner)
  "Validate image encounter OWNER for ID before producing a pending result."
  (let ((buffer (nth 2 owner)) (state (nth 3 owner)) (snapshot (nth 4 owner)))
    (unless (and (eq (car owner) (gnosis--ensure-db)) (buffer-live-p buffer)
                 (with-current-buffer buffer (eq gnosis-review--state state))
                 (equal snapshot (gnosis-review--state-data state))
                 (equal id (car (gnosis-review-state-remaining state)))
                 (equal (cadr owner) (gnosis-review--image-thema id))
                 (or (not (gnosis-review-state-persistent-p state))
                     (when-let* ((stored (gnosis-review--read-session)))
                       (equal snapshot (gnosis-review--state-data stored)))))
      (user-error "Image encounter is outdated; resume the original session"))
    (gnosis-review--image-validate (car (cadr owner)))))

(defun gnosis-review--image (id)
  "Present region thema ID with explicit click and submit."
  (let* ((gnosis-review--display-buffer (current-buffer))
         (content-owner (gnosis-review--content-owner id))
         (gnosis-review--display-validate
          (lambda ()
            (with-current-buffer (nth 2 content-owner)
              (gnosis-review--content-check id content-owner))))
         (owner (gnosis-review--image-owner id))
         (row (car (cadr owner)))
         (reference (car (nth 2 row))) (target (car (nth 3 row)))
         (scene (gnosis-image-resolve reference target))
         (label (alist-get 'label (seq-find (lambda (r) (equal target (alist-get 'id r)))
                                          (alist-get 'regions scene))))
         (prompt (gnosis-org-format-string (nth 1 row)))
         (validate (lambda ()
                     (with-current-buffer (nth 2 content-owner)
                       (gnosis-review--content-check id content-owner)
                       (gnosis-review--image-check id owner))))
         (input (progn
                  (gnosis-display-keimenon prompt)
                  (gnosis-image-input (cons (cons 'prompt prompt) scene)
                                      'region target validate)))
         (success (equal target (cadr input))))
    (funcall gnosis-review--display-validate)
    (gnosis-review--image-check id owner)
    (let ((result (gnosis-review--encounter
                   (plist-put (plist-put (gnosis-review-algorithm id success)
                                        :content content-owner) :image owner)
                   (append (seq-take row 4) (list (nth 6 row) (nth 4 row) (nth 5 row)))
                   (list :kind "region" :selected-target (cadr input)) nil nil)))
      (gnosis-display-basic-answer
       label success
       (or (alist-get 'label (seq-find (lambda (r) (equal (cadr input) (alist-get 'id r)))
                                      (alist-get 'regions scene))) ""))
      (gnosis-display-parathema (nth 4 row))
      (gnosis-display-next-review (gnosis-review--result-date result) success)
      (cons success result))))

(defun gnosis-review-image-region (id)
  "Review image-region thema ID with neutral selection and explicit submit."
  (gnosis-review--image id))

(defun gnosis-review-image-occlusion (id)
  "Review occlusion ID inline with a typed answer, then reveal and give feedback."
  (let* ((gnosis-review--display-buffer (current-buffer))
         (content-owner (gnosis-review--content-owner id))
         (gnosis-review--display-validate
          (lambda ()
            (with-current-buffer (nth 2 content-owner)
              (gnosis-review--content-check id content-owner))))
         (owner (gnosis-review--image-owner id))
         (row (car (cadr owner)))
         (fields (gnosis-image-occlusion-fields (nth 2 row) (nth 3 row)))
         (target (cadar fields)) (answer (caadr fields))
         (policy (gnosis-image-occlusion-policy (car fields)))
         (aliases (nth 6 row)) (tolerance gnosis-string-difference)
         (scene (gnosis-image-resolve (caar fields) target)))
    (gnosis-display-keimenon
     (concat (gnosis-org-format-string (nth 1 row)) "\n\n"
             (gnosis-image-mask scene target nil nil policy)))
    (let ((input (gnosis--read-string-with-input-method "Answer: " answer)))
      (funcall gnosis-review--display-validate)
      (gnosis-review--image-check id owner)
      (let* ((success (gnosis-answer-match-p answer input aliases tolerance))
             (result (gnosis-review--encounter
                      (plist-put (plist-put (gnosis-review-algorithm id success)
                                            :content content-owner) :image owner)
                      (list (nth 0 row) (nth 1 row) (car fields) (list answer)
                            aliases (nth 4 row) (nth 5 row))
                      (list :kind "text" :text input) nil tolerance)))
        (gnosis-display-keimenon
         (concat (gnosis-org-format-string (nth 1 row)) "\n\n"
                 (gnosis-image-mask scene target t nil policy)))
        (gnosis-display-basic-answer answer success input)
        (gnosis-display-parathema (nth 4 row))
        (gnosis-display-next-review (gnosis-review--result-date result) success)
        (cons success result)))))

;;; Type-specific review

(defun gnosis-review--display-question (id owner keimenon)
  "Display KEIMENON for ID only while its captured OWNER remains current."
  (gnosis-review--content-check id owner)
  (gnosis-display-image keimenon (lambda () (gnosis-review--content-check id owner)))
  ;; Org mode hooks run during formatting; check before replacing the view.
  (let ((text (gnosis-org-format-string keimenon)))
    (gnosis-review--content-check id owner)
    (gnosis-display-keimenon text))
  ;; Rendering may start preparation; refuse before hints or answer input.
  (gnosis-review--content-check id owner))

(defun gnosis-review-mcq (id)
  "Review MCQ thema with ID."
  (let* ((gnosis-review--display-buffer (current-buffer))
         (owner (gnosis-review--content-owner id))
         (gnosis-review--display-validate
          (lambda () (gnosis-review--content-check id owner)))
         (data (car (cadr owner)))
         (keimenon (nth 1 data))
         (answer (car (nth 3 data)))
         (parathema (nth 5 data)))
    (gnosis-review--display-question id owner keimenon)
    (let* ((user-choice (gnosis-mcq-answer id))
           (_ (gnosis-review--content-check id owner))
	   (success (string= answer user-choice))
           (result (plist-put (gnosis-review-algorithm id success) :content owner)))
      (setq result (gnosis-review--encounter
                    result data (list :kind "choice" :selected user-choice
                                      :choices (vconcat (nth 2 data))) nil nil))
      (unless success (setq gnosis-review--monkeytype-text answer))
      (gnosis-display-correct-answer-mcq answer user-choice)
      (gnosis-display-parathema parathema)
      (gnosis-display-next-review (gnosis-review--result-date result) success)
      (cons success result))))

(defun gnosis-review--answer-thema (id)
  "Read ID's typed-response content, including its accepted aliases."
  (gnosis-select '[type keimenon hypothesis answer accepted-aliases]
                 'themata `(= id ,id)))

(defun gnosis-review--encounter (result row response hints tolerance)
  "Return RESULT with captured practice evidence from content ROW.
RESPONSE is type-specific plain data, or nil for unobservable input.
HINTS records actually displayed hints, not merely available content.
TOLERANCE is the captured typed-match rule, or nil for exact selection.
ROW uses `gnosis-review--content-thema' order.  This is an accepted-encounter
snapshot, never a content archive or retrospective regrading rule."
  (if (not (eq (plist-get result :mode) 'practice)) result
    (plist-put
     result :encounter
     (list :version 1 :kind (nth 0 row) :prompt (nth 1 row)
           :hypothesis (vconcat (nth 2 row)) :expected-answers (vconcat (nth 3 row))
           :accepted-aliases (vconcat (nth 4 row)) :parathema (nth 5 row)
           :review-image (nth 6 row) :response response
           :hints-available (vconcat (when (member (nth 0 row) '("basic" "cloze" "agent-eval")) (nth 2 row)))
           :hints-shown (vconcat hints) :coaching nil
           :match-rule (cond ((equal (nth 0 row) "agent-eval")
                              (list :kind "agent-eval" :rubric (nth 7 row)))
                             (tolerance (list :kind "text" :tolerance tolerance))
                             ((equal (plist-get response :kind) "self-grade")
                              (list :kind "self-grade"))
                             (t (list :kind "exact")))
           :original-outcome (symbol-name (plist-get result :outcome))))))

(defun gnosis-review--content-owner (id)
  "Capture ID's content and optional encounter owner before input."
  (gnosis-review--session-check nil (current-buffer))
  (gnosis-review--watch-buffer)
  (unless gnosis-review--setup-owner
    (setq gnosis-review--setup-owner (list (current-buffer))))
  (list (gnosis--ensure-db) (copy-tree (gnosis-review--content-thema id))
        (current-buffer) gnosis-review--state
        (and gnosis-review--state
             (copy-tree (gnosis-review--state-data gnosis-review--state)))
        gnosis-review--setup-owner))

(defun gnosis-review--content-thema (id)
  "Read ID's response rules and presentation, excluding scheduling and tags."
  (mapcar (lambda (row)
            (append row (or (car (gnosis-select '[parathema review-image]
                                                'extras `(= id ,id)))
                            '(nil nil))
                    (list (gnosis-get 'rubric 'themata `(= id ,id)))))
          (gnosis-review--answer-thema id)))

(define-error 'gnosis-review-content-changed "Review content changed" 'user-error)

(defun gnosis-review--content-check (id owner &optional result)
  "Reject changed content or encounter for ID and OWNER.
RESULT permits an identical retry of the last committed persistent attempt."
  (let ((state (nth 3 owner)) (snapshot (nth 4 owner))
        (row (car (nth 1 owner))))
    (unless (and (not gnosis-review--retired) (not buffer-file-name)
                 (eq (car owner) (gnosis--ensure-db))
                 (buffer-live-p (nth 2 owner)) (eq (current-buffer) (nth 2 owner))
                 (eq gnosis-review--state state)
                 (eq gnosis-review--setup-owner (nth 5 owner))
                 (or (null state)
                     (equal snapshot (gnosis-review--state-data state))
                     (and result (gnosis-review-state-persistent-p state)
                          (eq (plist-get snapshot :mode) (gnosis-review-state-mode state))
                          (equal (plist-get snapshot :session-id)
                                 (gnosis-review-state-session-id state))
                          (equal (plist-get result :event-id)
                                 (gnosis-review-state-last-event state))))
                 (or (null state) (not (gnosis-review-state-persistent-p state))
                     (when-let* ((stored (gnosis-review--read-session)))
                       (equal (gnosis-review--state-data state)
                              (gnosis-review--state-data stored))))
                 (or (gnosis-review--edited-content-p id result)
                     (equal (nth 1 owner) (gnosis-review--content-thema id))))
      (signal 'gnosis-review-content-changed
              '("The content or encounter changed; resume the batch to answer again")))
    (gnosis--validate-accepted-aliases (nth 0 row) (nth 3 row) (nth 4 row))
    (gnosis--validate-agent-eval-fields
     (nth 0 row) (nth 1 row) (nth 2 row) (nth 3 row) (nth 7 row))))

(defun gnosis-review-basic (id)
  "Review basic type thema for ID."
  (let* ((gnosis-review--display-buffer (current-buffer))
         (owner (gnosis-review--content-owner id))
         (gnosis-review--display-validate
          (lambda () (gnosis-review--content-check id owner)))
         (data (car (cadr owner)))
	 (keimenon (nth 1 data))
	 (hypothesis (car (nth 2 data)))
	 (answer (car (nth 3 data)))
         (aliases (nth 4 data))
         (tolerance gnosis-string-difference)
	 (parathema (gnosis-get 'parathema 'extras
				`(= id ,id))))
    (gnosis-review--display-question id owner keimenon)
    (gnosis-display-hint hypothesis)
    (let* ((self-grade (or (eq gnosis-review-basic-input 'self-grade)
                            (gnosis-image-content-p answer)))
           (user-input
            (unless self-grade
              (gnosis--read-string-with-input-method "Answer: " answer)))
           (success
            (if self-grade
                (progn
                  (read-char-choice "Recall first; press SPC to reveal: " '(?\s))
                  (gnosis-review--content-check id owner)
                  (gnosis-display-basic-answer answer t "")
                  (gnosis-display-parathema parathema)
                  (= (read-char-choice "Recalled the checklist?  y yes, n no: " '(?y ?n)) ?y))
              (gnosis-answer-match-p answer user-input aliases tolerance)))
           (_ (gnosis-review--content-check id owner))
           (result (gnosis-review-algorithm id success)))
      (setq result
            (gnosis-review--encounter
             (plist-put result :content owner) data
             (if self-grade (list :kind "self-grade" :recalled (if success t :false))
               (list :kind "text" :text user-input))
             (when (and hypothesis (not (string-empty-p hypothesis))) (list hypothesis))
             (unless self-grade tolerance)))
      (unless (or success self-grade) (setq gnosis-review--monkeytype-text answer))
      (unless self-grade
        (gnosis-display-basic-answer answer success user-input)
        (gnosis-display-parathema parathema))
      (gnosis-display-next-review (gnosis-review--result-date result) success)
      (cons success result))))

(defun gnosis-review-cloze--input (clozes &optional user-input)
  "Prompt for USER-INPUT during cloze review.

CLOZES is a list of possible correct answers.

Returns a cons; ='(position . user-input) if correct,
='(nil . user-input) if incorrect."
  (let* ((user-input (or user-input
                         (gnosis--read-string-with-input-method
                          "Answer: " (car clozes))))
         (position (cl-position user-input clozes
				:test #'gnosis-compare-strings)))
    (cons position user-input)))

(defun gnosis-review-cloze (id)
  "Review cloze type thema for ID."
  (let* ((gnosis-review--display-buffer (current-buffer))
         (owner (gnosis-review--content-owner id))
         (gnosis-review--display-validate
          (lambda () (gnosis-review--content-check id owner)))
         (data (car (cadr owner)))
	 (keimenon (nth 1 data))
	 (all-clozes (nth 3 data))
	 (all-hints (nth 2 data))
         (indices (number-sequence 0 (1- (length all-clozes))))
         (responses nil)
         (shown-hints nil)
         (tolerance gnosis-string-difference)
         (parathema (nth 5 data))
         (success t))
    (setq shown-hints (gnosis-display-cloze-string
                       keimenon all-clozes all-hints nil nil (vconcat indices)))
    (while (and indices success)
      (let* ((remaining (mapcar (lambda (i) (nth i all-clozes)) indices))
             (input (let ((gnosis-string-difference tolerance))
                      (gnosis-review-cloze--input remaining)))
             (position (car input)))
        (gnosis-review--content-check id owner)
        (push (list :text (cdr input) :remaining-blank-indices (vconcat indices)
                    :matched-blank-index (and position (nth position indices))) responses)
        (if position
            (progn
              (setq indices (remq (nth position indices) indices))
              (setq shown-hints
                    (append shown-hints
                            (gnosis-display-cloze-string
                             keimenon all-clozes all-hints nil nil (vconcat indices)))))
          (gnosis-display-cloze-string keimenon all-clozes all-hints nil t (vconcat indices))
          (gnosis-display-cloze-user-answer (cdr input) t)
          (setq success nil gnosis-review--monkeytype-text (car remaining)))))
    (gnosis-review--content-check id owner)
    (let ((result (gnosis-review--encounter
                   (plist-put (gnosis-review-algorithm id success) :content owner) data
                   (list :kind "blanks" :inputs (vconcat (reverse responses)))
                   (delete-dups shown-hints) tolerance)))
      (gnosis-display-parathema parathema)
      (gnosis-display-next-review (gnosis-review--result-date result) success)
      (cons success result))))

(defun gnosis-review-mc-cloze (id)
  "Review mc-cloze type thema for ID."
  (let* ((gnosis-review--display-buffer (current-buffer))
         (owner (gnosis-review--content-owner id))
         (gnosis-review--display-validate
          (lambda () (gnosis-review--content-check id owner)))
         (data (car (cadr owner)))
	 (keimenon (nth 1 data))
	 (cloze (nth 3 data))
	 (options (nth 2 data))
	 (parathema (nth 5 data))
	 (user-input)
	 (success))
    (gnosis-display-cloze-string keimenon cloze nil nil nil)
    (setq user-input (gnosis-completing-read "Select answer: "
					     (copy-sequence options)))
    (gnosis-review--content-check id owner)
    (if (string= user-input (car cloze))
	(progn
	  (gnosis-display-cloze-string keimenon nil nil cloze nil)
	  (setq success t))
      (gnosis-display-cloze-string keimenon nil nil nil cloze)
      (gnosis-display-correct-answer-mcq (car cloze) user-input)
      (setq gnosis-review--monkeytype-text (car cloze)))
    (gnosis-review--content-check id owner)
    (let ((result (gnosis-review--encounter
                   (plist-put (gnosis-review-algorithm id success) :content owner) data
                   (list :kind "choice" :selected user-input :choices (vconcat options)) nil nil)))
      (gnosis-display-parathema parathema)
      (gnosis-display-next-review (gnosis-review--result-date result) success)
      (cons success result))))

(defun gnosis-review-is-thema-new-p (id)
  "Return t if thema with ID is new."
  (zerop (gnosis-get 'reps 'scheduler-state `(= thema-id ,id))))

;;; Session management

(defvar gnosis-review--session-validate nil
  "Validator retaining the running batch's original buffer and setup.
Never recapture this authority after navigation or between presentations.")

(defun gnosis-review--session-validator (state)
  "Capture STATE's buffer lifetime and database before session callbacks.
The returned function also accepts an optional frozen checkpoint.  Detached
nonpersistent queue computations do not acquire a native buffer lifetime."
  (if (not (or (gnosis-review-state-persistent-p state)
               (eq state gnosis-review--state)))
      (lambda (&optional _checkpoint _destination) nil)
    (let ((buffer (current-buffer))
          (setup (or gnosis-review--setup-owner
                     (setq gnosis-review--setup-owner (list (current-buffer)))))
          (database (gnosis--ensure-db)))
      (lambda (&optional checkpoint destination)
        (unless (buffer-live-p buffer) (user-error "Review buffer was killed"))
        (with-current-buffer buffer
          (gnosis-review--watch-buffer setup)
          (unless (and (eq state gnosis-review--state)
                       (or (null destination) (eq buffer destination))
                       (eq database (gnosis--ensure-db))
                       (or (null checkpoint)
                           (equal checkpoint (gnosis-review--state-data state))))
            (user-error "Review session changed; resume the batch again"))
          (when (gnosis-review-state-persistent-p state)
            (gnosis-review--check-database state)
            (gnosis-review--check-action-target
             (cons database (gnosis-review--state-data state)))))))))

(defun gnosis-review--session-check (&optional checkpoint destination)
  "Validate the batch owner, optional CHECKPOINT and input DESTINATION."
  (when gnosis-review--session-validate
    (funcall gnosis-review--session-validate checkpoint destination)))

(defun gnosis-review--display-thema (id)
  "Display thema with ID and call the appropriate review func.
Returns (TYPE (SUCCESS . ALGORITHM-RESULT))."
  (let* ((type (gnosis-get 'type 'themata `(= id ,id)))
         (func-name (intern (format "gnosis-review-%s"
				    (downcase type)))))
    (if (fboundp func-name)
        (let ((owner (gnosis-review--content-owner id)))
          (window-configuration-to-register :gnosis-pre-image)
          (gnosis-review--content-check id owner)
          (let* ((image-owner (gnosis-review--image-owner id))
                 (_ (gnosis-review--content-check id owner))
                 (answer (funcall func-name id)))
            (gnosis-review--content-check id owner)
            (when image-owner
              (gnosis-review--image-check id image-owner)
              (setcdr answer (plist-put (cdr answer) :image image-owner)))
            (list type answer)))
      (error "Malformed thema type: '%s'" type))))

(defun gnosis-review--failed-disposition-p (disposition)
  "Return t if accepted DISPOSITION records an Again rating."
  (let ((rating (and (listp disposition)
                     (plist-get disposition :rating))))
    (cond ((eq disposition :deleted) nil)
          ((equal rating 1) t)
          ((equal rating 3) nil)
          (t (error "Review action did not settle a binary outcome")))))

(defun gnosis-review-process-thema (thema state)
  "Process review for THEMA and update STATE.

Displays the thema, processes the review result, advances the bounded
remaining queue, and forces header redisplay.  Return STATE.

This is a helper function for `gnosis-review-session'."
  (let ((gnosis-review--session-validate
         (or gnosis-review--session-validate (gnosis-review--session-validator state)))
        (remaining (gnosis-review-state-remaining state))
        (checkpoint (copy-tree (gnosis-review--state-data state))))
    (gnosis-review--session-check checkpoint (current-buffer))
    (unless (equal thema (car remaining))
      (error "Review queue is out of order"))
    (pcase-let* ((gnosis-review--monkeytype-text nil)
                 (`(,thema-type ,review-cons)
                  (gnosis-review--display-thema thema))
                 (success (car review-cons))
                 (result (cdr review-cons)))
      (when (and (not success)
                 gnosis-review--monkeytype-text
                 gnosis-monkeytype-enable
                 (member thema-type gnosis-monkeytype-themata))
        (let ((text gnosis-review--monkeytype-text))
          (gnosis-monkeytype text)
          (when-let* ((encounter (plist-get result :encounter)))
            (setf (plist-get encounter :coaching)
                  (vector (list :kind "copy-practice" :text text))))))
      (let* ((disposition (gnosis-review-actions success thema result))
             (failed-p (gnosis-review--failed-disposition-p disposition))
             (requeued (gnosis-review-state-requeued state))
             (accepted (copy-tree (gnosis-review--state-data state))))
        (gnosis-review--session-check accepted)
        ;; Use jump-to-register after first review.
        (when (get-register :gnosis-pre-image)
          (jump-to-register :gnosis-pre-image))
        (gnosis-review--session-check accepted)
        (if (gnosis-review-state-persistent-p state)
            (when (eq disposition :deleted)
              (gnosis-review--skip state thema))
          (let* ((rest (cdr remaining))
               (requeue-p (and failed-p
                               (not (gnosis-suspended-p thema))
                               (not (member thema requeued)))))
          (unless (eq disposition :deleted)
            (push (cons thema (not failed-p)) (gnosis-review-state-outcomes state))
            (cl-incf (gnosis-review-state-reviewed state)))
          (when (eq disposition :deleted)
            (push thema (gnosis-review-state-skipped state)))
          (when requeue-p
            (cl-incf (gnosis-review-state-total state)))
          (setf (gnosis-review-state-remaining state)
                (if requeue-p (append rest (list thema)) rest)
                (gnosis-review-state-requeued state)
                (if requeue-p (cons thema requeued) requeued)))))
    (let ((advanced (copy-tree (gnosis-review--state-data state))))
      (gnosis-review--lookahead-advance checkpoint)
      (gnosis-review--session-check advanced))
    (force-mode-line-update)
    state)))


(defun gnosis-review-session (state)
  "Review the bounded remaining queue in review STATE.
Return STATE after completion."
  (if (null (gnosis-review-state-remaining state))
      (progn (message "No themata for review.") state)
    (let ((buffer (current-buffer))
          (gnosis-review--session-validate
           (or gnosis-review--session-validate (gnosis-review--session-validator state))))
      (while (gnosis-review-state-remaining state)
        (gnosis-review--session-check)
        (let ((id (car (gnosis-review-state-remaining state)))
              (checkpoint (copy-tree (gnosis-review--state-data state))))
          (if (gnosis-study-eligible-p id)
              (progn
                (gnosis-review--session-check checkpoint)
                (pop-to-buffer-same-window buffer)
                (gnosis-review--session-check checkpoint)
                ;; Do not restore source windows or the preceding answer image.
                (delete-other-windows)
                (gnosis-review--session-check checkpoint)
                (gnosis-review-process-thema id state))
            (gnosis-review--session-check checkpoint)
            (gnosis-review--skip state id))
          (gnosis-review--session-check))))
    state))

(defun gnosis-review-practice-projection (state events)
  "Project retained effective practice EVENTS onto a copy of STATE.
EVENTS are ordered rows from `gnosis-study-practice-events'.  Deleted selected
items remain excluded membership, never accepted outcome evidence."
  (gnosis-review--check-database state)
  (let ((projection (copy-gnosis-review-state state)))
    (setf (gnosis-review-state-outcomes projection)
          (reverse (mapcar (lambda (row) (cons (nth 1 row) (= 3 (nth 5 row))))
                           (seq-remove (lambda (row) (nth 6 row)) events)))
          (gnosis-review-state-skipped projection)
          (delete-dups
           (append (gnosis-review-state-skipped state)
                   (seq-remove (lambda (id) (gnosis-get 'id 'themata `(= id ,id)))
                               (gnosis-review-state-selected state)))))
    projection))

(defun gnosis-review-summary (state)
  "Return truthful unique and attempt counts from accepted outcomes in STATE."
  (let* ((rows (reverse (gnosis-review-state-outcomes state)))
         (ids (delete-dups (mapcar #'car rows)))
         (first (mapcar (lambda (id) (assoc id rows)) ids))
         (last (mapcar (lambda (id) (assoc id (gnosis-review-state-outcomes state))) ids))
         (retry (cl-set-difference rows first :test #'eq)))
    (list :selected (gnosis-review-state-initial state)
          :unique (length ids) :attempts (length rows)
          :first-success (seq-count #'cdr first)
          :first-failure (seq-count (lambda (row) (not (cdr row))) first)
          :retry-success (seq-count #'cdr retry)
          :retry-failure (seq-count (lambda (row) (not (cdr row))) retry)
          :needs-work (seq-count (lambda (row) (not (cdr row))) last)
          :excluded (length (delete-dups (copy-sequence
                                          (gnosis-review-state-skipped state))))
          :unattempted (- (gnosis-review-state-initial state) (length ids)))))

(defun gnosis-review-policy-summary (state)
  "Return frozen-policy target and cap counts for practice STATE."
  (let ((reasons
         (mapcar
          (lambda (id)
            (if (or (member id (gnosis-review-state-skipped state))
                    (not (gnosis-get 'id 'themata `(= id ,id))))
                "excluded"
              (plist-get
               (gnosis-review-policy-progress
                (gnosis-review-state-policy state)
                (mapcar #'cdr (seq-filter (lambda (row) (equal id (car row)))
                                         (gnosis-review-state-outcomes state))))
               :reason)))
          (gnosis-review-state-selected state))))
    (list :target-reached (seq-count (lambda (reason) (equal reason "target-reached")) reasons)
          :attempt-limit (seq-count (lambda (reason) (equal reason "attempt-limit")) reasons)
          :unfinished (seq-count (lambda (reason) (equal reason "unfinished")) reasons)
          :excluded (seq-count (lambda (reason) (equal reason "excluded")) reasons))))

(defun gnosis-review--show-summary (state)
  "Display accepted recall evidence from STATE, not a mastery estimate.
Read evidence in STATE's owning database without changing the current database
or granting summary actions authority over a different current connection."
  (let* ((database (or (gnosis-review-state-database state) (gnosis--ensure-db)))
         (projection
          (let ((gnosis-db database))
            (if (and (eq (gnosis-review-state-mode state) 'practice)
                     (gnosis-review-state-persistent-p state))
                (gnosis-review-practice-projection
                 state (gnosis-study-practice-events (gnosis-review-state-session-id state)))
              state)))
         (summary (gnosis-review-summary projection))
         (backlog (let ((gnosis-db database)
                        (gnosis-new-themata-limit nil))
                    (length (gnosis-review-get-due-themata))))
         (progress (when (gnosis-review-state-policy state)
                     (let ((gnosis-db database))
                       (gnosis-review-policy-summary projection))))
         (target (cons (gnosis-review-state-database state)
                       (copy-tree (gnosis-review--state-data state))))
         (buf (generate-new-buffer "*Gnosis Study Summary*")))
    (with-current-buffer buf
      (insert (propertize (if (eq (gnosis-review-state-mode state) 'practice)
                             "Practice — schedule unchanged\n" "Review — FSRS accepted\n")
                         'face 'bold)
              (format "Selected: %d   Unique attempted: %d   Accepted attempts: %d\n"
                      (plist-get summary :selected) (plist-get summary :unique)
                      (plist-get summary :attempts))
              (format "First attempt: %d success, %d failure\nRetry: %d success, %d failure\n"
                      (plist-get summary :first-success) (plist-get summary :first-failure)
                      (plist-get summary :retry-success) (plist-get summary :retry-failure))
              (propertize (format "Last-attempt failures (not manual flags): %d\n" (plist-get summary :needs-work))
                          'face 'warning)
              (format "Unattempted: %d\nSkipped items (including retries): %d\n"
                      (plist-get summary :unattempted) (plist-get summary :excluded))
              (format "Remaining due backlog: %d\n" backlog)
              "Session recall is not topic mastery.  Practice is excluded from FSRS replay.\n")
      (when-let* ((policy (gnosis-review-state-policy state)))
        (insert (format "\nFrozen practice target: %d successes; after failure: %d; consecutive: %s; cap: %s effective attempts per thema\n"
                        (plist-get policy :successes-required)
                        (plist-get policy :successes-after-failure)
                        (if (plist-get policy :consecutive) "yes" "no")
                        (or (plist-get policy :max-attempts-per-thema) "unlimited"))
                (format "Targets reached: %d   Attempt limit (target unmet): %d   Unfinished: %d   Excluded: %d\n"
                        (plist-get progress :target-reached) (plist-get progress :attempt-limit)
                        (plist-get progress :unfinished) (plist-get progress :excluded))))
      (when (and (eq 'practice (gnosis-review-state-mode state))
                 (gnosis-review-state-selected state)
                 (null (gnosis-review-state-remaining state))
                 (not (gnosis-review-state-cancelled-p state)))
        (insert "\nScheduled review uses current questions from this selection, due only.\n"
                "New cards are not capped; newly accepted answers affect scheduling.\n"
                "Practice evidence is never converted or replayed as FSRS grades.\n")
        (insert-text-button "Scheduled review"
                            'action (lambda (_button)
                                      (gnosis-review--check-summary-owner (cons buf target))
                                      (with-current-buffer buf
                                        (gnosis-review-summary-scheduled)))
                            'follow-link t)
        (insert "\n"))
      ;; Establish authority before hooks can retire this buffer.
      (delay-mode-hooks
        (gnosis-review-summary-mode)
        (setq gnosis-review--summary-target target))
      (run-mode-hooks))
    (pop-to-buffer buf)))

(keymap-popup-define gnosis-review-summary-mode-map
  "Study summary"
  :parent special-mode-map
  :group "Batch"
  "r" ("Resume" gnosis-review-resume)
  "c" ("Continue with another batch" gnosis-review-continue)
  "d" ("Discard progress" gnosis-review-discard)
  "u" ("Undo last accepted grade" gnosis-review-undo)
  :group "Practice"
  "s" ("Scheduled review" gnosis-review-summary-scheduled
       :if #'gnosis-review--completed-practice-summary-p)
  :group "Repair"
  "w" ("Repair questions" gnosis-study-repair)
  "t" ("Study topic" gnosis-study-topic)
  :group "Navigate"
  "q" ("Quit" quit-window))

(defun gnosis-review--retire-summary ()
  "Retire the summary's authority when its buffer is repurposed."
  (setq gnosis-review--summary-target nil))

(define-derived-mode gnosis-review-summary-mode special-mode "Study Summary"
  "Inspect truthful study outcomes; use h for continuation and repair.
Resume, discard and undo act only on the displayed checkpoint in its original
open database.  Reopen a summary after the checkpoint or connection changes.
Visiting a file permanently retires this summary's actions."
  (add-hook 'after-set-visited-file-name-hook #'gnosis-review--retire-summary nil t)
  (add-hook 'change-major-mode-hook #'gnosis-review--retire-summary nil t))

(defun gnosis-review--summary-owner ()
  "Return the current summary buffer and target, or nil outside summaries."
  (when (derived-mode-p 'gnosis-review-summary-mode)
    (cons (current-buffer) gnosis-review--summary-target)))

(defun gnosis-review--check-summary-owner (owner)
  "Reject a retired summary OWNER; nil imposes no buffer ownership."
  (when owner
    (unless (and (buffer-live-p (car owner))
                 (with-current-buffer (car owner)
                   (and (derived-mode-p 'gnosis-review-summary-mode)
                        (not buffer-file-name)
                        gnosis-review--summary-target
                        (eq gnosis-review--summary-target (cdr owner)))))
      (user-error "Study summary buffer changed; reopen the summary first"))))

(defun gnosis-review--check-action-target (target)
  "Reject TARGET unless its database and frozen checkpoint are still current.
Check at the action boundary, including after prompts or buffer setup hooks."
  (unless (and (eq (car target) (gnosis--ensure-db))
               (equal (cdr target)
                      (when-let* ((state (gnosis-review--read-session)))
                        (gnosis-review--state-data state))))
    (user-error "Study summary or session changed; inspect the current batch first")))

(defun gnosis-review--action-target (owner)
  "Return summary OWNER's database/checkpoint, or the current batch for nil."
  (gnosis-review--check-summary-owner owner)
  (let ((target (if owner (cdr owner) (gnosis-review--session-target))))
    (gnosis-review--check-action-target target)
    target))

(defun gnosis-review--completed-practice-summary-p ()
  "Return non-nil if this summary displays a completed nonempty practice batch."
  (let ((data (cdr gnosis-review--summary-target)))
    (and (eq 'practice (plist-get data :mode))
         (plist-get data :selected)
         (null (plist-get data :remaining))
         (not (plist-get data :cancelled-p)))))

(defun gnosis-review--scheduled-selection (ids)
  "Classify original IDS against current eligibility and native due dates.
Return (ID . REASON) pairs, with reason due, deleted, suspended, ineligible
or not-due.  Like explicit topic review, do not apply the global new limit."
  (mapcar (lambda (id)
            (cons id (cond ((not (gnosis-get 'id 'themata `(= id ,id))) 'deleted)
                           ((equal 1 (gnosis-get 'suspended 'scheduler-state
                                                `(= thema-id ,id))) 'suspended)
                           ((not (gnosis-study-eligible-p id)) 'ineligible)
                           ((gnosis-review-is-due-today-p id) 'due)
                           (t 'not-due))))
          ids))

(defun gnosis-review-summary-scheduled ()
  "Start a separate scheduled review from this completed practice summary.
Resolve original selected IDs against current content and due dates.  Like
explicit topic review, include new cards without the global new-card cap.
Show exclusions and confirm before replacing the checkpoint.  Only newly
accepted answers affect scheduling; never convert practice evidence."
  (interactive nil gnosis-review-summary-mode)
  (when gnosis-review--running (user-error "Finish the active review first"))
  (let* ((owner (or (gnosis-review--summary-owner)
                    (user-error "Open a completed practice summary first")))
         (target (gnosis-review--action-target owner)))
    (unless (gnosis-review--completed-practice-summary-p)
      (user-error "This is not a completed practice batch"))
    (let* ((ids (plist-get (cdr target) :selected))
           (selection (gnosis-review--scheduled-selection ids))
           (counts (mapcar (lambda (reason)
                             (seq-count (lambda (row) (eq reason (cdr row))) selection))
                           '(due deleted suspended ineligible not-due)))
           (description (apply #'format
                               "Due: %d; excluded: %d deleted, %d suspended, %d ineligible, %d not yet due"
                               counts))
           (validate (lambda ()
                       (gnosis-review--check-summary-owner owner)
                       (gnosis-review--check-action-target target)
                       (unless (equal selection (gnosis-review--scheduled-selection ids))
                         (user-error "Eligibility changed; request scheduled review again")))))
      (if (zerop (car counts))
          (message "No scheduled review to start.  %s" description)
        (when (y-or-n-p (concat description
                               ".  New cards are not capped.  New answers affect scheduling.  Start scheduled review? "))
          (funcall validate)
          (gnosis-review-loop
           (mapcar #'car (seq-filter (lambda (row) (eq 'due (cdr row))) selection))
           'due target validate))))))

(defun gnosis-review-loop (collector &optional mode target validate)
  "Review one finite batch from COLLECTOR in MODE, defaulting to due.
COLLECTOR is a list of IDs or a function called exactly once.  Deduplicate
and freeze membership, then recheck deletion and suspension before each
presentation.  Practice records separate encounters and never reschedules.
Return the session state, also on ordinary quit, or nil for empty selection.
Keyboard quit preserves accepted grades and restores windows.  Cancelling
an answer writes no grade.  A new nonempty selection ends an unfinished
batch early without changing its accepted evidence or schedules.  Empty,
failed or cancelled selection keeps the old batch and its summary.
Optional TARGET is the database/checkpoint captured before earlier prompts;
otherwise capture it before calling COLLECTOR.
Optional VALIDATE is a caller check, run before selection, buffer setup,
and batch replacement; it must signal if the caller no longer owns the action."
  (when gnosis-review--running (user-error "Finish the active review first"))
  (unless (memq mode '(nil due practice)) (error "Unknown study mode"))
  (when validate (funcall validate))
  (let* ((target (or target (gnosis-review--session-target)))
         (themata (seq-filter #'gnosis-study-eligible-p
                              (delete-dups (copy-sequence
                                            (if (functionp collector)
                                                (funcall collector) collector))))))
    (if (null themata)
        (progn (message "No eligible themata selected") nil)
      (gnosis-review--check-action-target target)
      (when validate (funcall validate))
      (let* ((buf (gnosis-review--setup-buffer themata mode))
             (state (buffer-local-value 'gnosis-review--state buf)))
        (when validate (funcall validate))
        (setf (gnosis-review-state-persistent-p state) t)
        (gnosis-review--replace-session state target)
        (gnosis-review--run-state buf state)))))

(defun gnosis-review--completion-event (state)
  "Return a notification for completed practice STATE, or nil.
Call only after checking the committed checkpoint and its original owner."
  (when (and (gnosis-review-state-persistent-p state)
             (eq 'practice (gnosis-review-state-mode state))
             (> (gnosis-review-state-initial state) 0)
             (> (gnosis-review-state-reviewed state) 0)
             (not (gnosis-review-state-cancelled-p state))
             (null (gnosis-review-state-remaining state)))
    (list :api-version 1 :mode "practice"
          :session-id (copy-sequence (gnosis-review-state-session-id state))
          :database (nth 2 (assoc 0 (sqlite-select
                                    (gnosis-review-state-database state)
                                    "PRAGMA database_list")))
          :connection (gnosis-review-state-database state))))

(defun gnosis-review--notify-completion (event)
  "Deliver completed EVENT without giving subscribers a core continuation."
  (run-hook-wrapped
   'gnosis-practice-completed-hook
   (lambda (function)
     (condition-case err
         (save-current-buffer
           (save-match-data
             ;; Isolate the flat payload, including mutable strings, from both
             ;; retained state and other subscribers.  The connection is shared.
             (funcall function (mapcar (lambda (value)
                                        (if (stringp value) (copy-sequence value) value))
                                      event))))
       ((error quit) (message "Gnosis completion subscriber failed: %s"
                              (error-message-string err))))
     nil)))

(defun gnosis-review--run-state (buf state)
  "Present STATE in BUF with frozen input policy and restored windows."
  (let (completion)
    (unwind-protect
        (let ((gnosis-review-basic-input (gnosis-review-state-basic-input state))
              (reviewed (gnosis-review-state-reviewed state))
              (checkpoint (copy-tree (gnosis-review--state-data state)))
              (gnosis-review--session-validate
               (with-current-buffer buf (gnosis-review--session-validator state)))
              (gnosis-review--running (gnosis-review-state-session-id state)))
          (gnosis-review--session-check checkpoint)
          (unwind-protect
              (save-window-excursion
                (pop-to-buffer-same-window buf)
                (gnosis-review--session-check checkpoint)
                (delete-other-windows)
                (gnosis-review--session-check checkpoint)
                (catch 'review-loop (gnosis-review-session state))
                (gnosis-review--session-check)
                (let ((finished (copy-tree (gnosis-review--state-data state))))
                  (when (> (gnosis-review-state-reviewed state) reviewed)
                    (gnosis-review-commit (- (gnosis-review-state-reviewed state) reviewed)))
                  (gnosis-review--session-check finished)))
            ;; Window restoration and cancellation may run native callbacks too.
            ;; Never cancel a successor's preparation or steal its view for a summary.
            (when (condition-case nil
                      (progn (gnosis-review--session-check) t)
                    (user-error nil))
              (let ((finished (copy-tree (gnosis-review--state-data state))))
                (with-current-buffer buf (gnosis-review--lookahead-cancel))
                (gnosis-review--session-check finished)
                (when (plist-get checkpoint :remaining)
                  (setq completion (gnosis-review--completion-event state)))
                (gnosis-review--show-summary state)))))
      ;; The running/input bindings have unwound and the summary is already
      ;; presented.  Reentrant subscribers get no subsequent core UI or writes.
      (when completion (gnosis-review--notify-completion completion))))
  state)

;;;###autoload
(defun gnosis-review-resume ()
  "Resume the unfinished frozen batch, discarding any unaccepted reveal.
From a summary, require its original database and unchanged checkpoint."
  (interactive)
  (let ((owner (gnosis-review--summary-owner)))
    (gnosis-review--resume
     (gnosis-review--action-target owner)
     (lambda () (gnosis-review--check-summary-owner owner)))))

(defun gnosis-review--resume (target &optional validate)
  "Resume exact database/checkpoint TARGET, independently of the current buffer.
Optional VALIDATE checks the caller before setup and checkpoint mutation."
  (when gnosis-review--running (user-error "Finish the active review first"))
  (when validate (funcall validate))
  (gnosis-review--check-action-target target)
  (let ((state (or (gnosis-review--read-session) (user-error "No study session"))))
    (unless (gnosis-review-state-remaining state) (user-error "Batch is complete"))
    ;; Setup can run hooks while the deferred launch token is still current.
    (let ((buf (let ((gnosis-review--running (gnosis-review-state-session-id state)))
                 (gnosis-review--setup-buffer nil))))
      (when validate (funcall validate))
      (gnosis-review--check-action-target target)
      ;; Setup hooks must not retire the caller before this checkpoint write.
      ;; Invalidate any deferred adapter launch before entering native input.
      (when (gnosis-review-state-launch-token state)
        (gnosis-sqlite-with-transaction (gnosis--ensure-db)
          (when validate (funcall validate))
          (gnosis-review--check-action-target target)
          (setf (gnosis-review-state-launch-token state) nil)
          (gnosis-review--save-session state)))
      (with-current-buffer buf (setq gnosis-review--state state))
      (gnosis-review--run-state buf state))))

;;;###autoload
(defun gnosis-review-discard ()
  "Discard the retained batch and undo slot, never accepted evidence.
From a summary, require its original database and unchanged checkpoint."
  (interactive)
  (when gnosis-review--running (user-error "Finish the active review first"))
  (let* ((owner (gnosis-review--summary-owner))
         (target (gnosis-review--action-target owner)))
    (when (y-or-n-p "Discard batch progress (keep accepted grades)? ")
      (gnosis-sqlite-with-transaction (car target)
        (gnosis-review--check-summary-owner owner)
        (gnosis-review--check-action-target target)
        (when-let* ((state (gnosis-review--read-session)))
          (setf (gnosis-review-state-cancelled-p state) t
                (gnosis-review-state-launch-token state) nil)
          (gnosis-review--save-history state))
        (gnosis--delete 'study-session)))))

;;;###autoload
(defun gnosis-review-continue ()
  "Select another batch, ending unfinished progress only after selection.
Keep accepted evidence and schedules.  Cancelling the menu changes nothing."
  (interactive)
  (when gnosis-review--running (user-error "Finish the active review first"))
  (gnosis-review))

;;;###autoload
(defun gnosis-review-undo (&optional event-id correction-id)
  "Undo the last accepted session grade, retaining append-only evidence.
Optional EVENT-ID and CORRECTION-ID pin an idempotent retry.  Reject stale
or superseded targets.  Re-answer with a fresh attempt identity.
From a summary, require its original database and unchanged checkpoint."
  (interactive)
  (when gnosis-review--running (user-error "Quit the active review before undo"))
  (let* ((owner (gnosis-review--summary-owner))
         (target (gnosis-review--action-target owner))
         (restored
          (gnosis-sqlite-with-transaction (car target)
            (gnosis-review--check-summary-owner owner)
            (gnosis-review--check-action-target target)
            (let* ((state (or (gnosis-review--read-session) (user-error "No retained session")))
             (slot (gnosis-review-state-undo state))
             (event (or event-id (plist-get slot :event-id)))
             (correction (or correction-id (plist-get slot :correction-id))))
        (unless (and event correction) (user-error "No grade available to undo"))
        (if (equal (cons event correction) (gnosis-review-state-last-correction state))
            state
          (unless (and (equal event (plist-get slot :event-id))
                       (equal correction (plist-get slot :correction-id)))
            (user-error "Undo target is stale"))
          (if (eq (gnosis-review-state-mode state) 'practice)
              (gnosis-study-void-practice correction event)
            (gnosis-scheduler-void-review correction event))
          (let ((restored (apply #'gnosis-review-state-create :persistent-p t
                                 :database (car target)
                                 (cddr (plist-get slot :before)))))
            (setf (gnosis-review-state-event-id restored) (gnosis-scheduler-event-id)
                  (gnosis-review-state-last-correction restored) (cons event correction))
            (gnosis-review--save-session restored)
            (message "Grade voided; resume to answer again")
            restored))))))
    ;; Refresh only after durable correction and checkpoint commit.  Preserve
    ;; the caller's current buffer so programmatic assignment targets stay put.
    (save-current-buffer (gnosis-review--show-summary restored))
    restored))

(defun gnosis-review-commit (thema-num)
  "Report THEMA-NUM accepted attempts and optionally commit the database.
When Git is available, initialize `gnosis-dir' as a repository if needed
and commit gnosis.db.  Git failures do not invalidate accepted reviews."
  (message "Review session finished.  %d review attempts accepted." thema-num)
  (gnosis-vc--auto-commit (format "Total themata reviewed: %d" thema-num)))

;;; Review actions

(defun gnosis-review--edited-content-p (id result)
  "Validate RESULT's acknowledged native edit of ID, returning non-nil.
Only the exact saved response and extras may replace the content guard.
The encountered question, answer rules, resources and owner remain intact."
  (when-let* ((saved (plist-get result :edited-content)))
    (unless (and (eq (car saved) (gnosis--ensure-db))
                 (equal id (nth 1 saved))
                 (equal (nth 2 saved)
                        (seq-take (gnosis--draft-content (car saved) id) 2)))
      (signal 'gnosis-review-content-changed
              '("The saved content changed; resume the batch to answer again")))
    t))

(defun gnosis-review-action--edit (success thema result)
  "Edit THEMA's future presentations, preserving pending SUCCESS and RESULT.
Return (SUCCESS . RESULT) after native save or cancel.  A save
acknowledges only this edit's content, never another encounter or write."
  (gnosis-review--check-result-content thema result)
  (let ((origin (current-buffer))
        (receipt (list nil))
        (gnosis-review-editing-p t))
    (gnosis-edit-thema thema)
    (setq gnosis--draft-save-receipt receipt)
    (with-current-buffer origin
      (gnosis-review--check-result-content thema result))
    (recursive-edit)
    (unless (buffer-live-p origin)
      (user-error "Review buffer no longer exists"))
    (with-current-buffer origin
      (let ((result (if (car receipt)
                        (plist-put (copy-sequence result) :edited-content (car receipt))
                      result)))
        (gnosis-review--check-result-content thema result)
        (cons success result)))))

(defun gnosis-review-action--quit (success thema result)
  "Quit review session.

Update result for THEMA review with SUCCESS.
RESULT is the algorithm result for the DB update.

This function should be used with `gnosis-review-actions', to finish
the review session."
  (let ((accepted (gnosis-review--accept thema success result)))
    (when (and gnosis-review--state
               (not (gnosis-review-state-persistent-p gnosis-review--state)))
      (push (cons thema (not (gnosis-review--failed-disposition-p accepted)))
            (gnosis-review-state-outcomes gnosis-review--state))
      (cl-incf (gnosis-review-state-reviewed gnosis-review--state))))
  ;; Break the review loop of `gnosis-review-loop'
  (throw 'review-loop t))

(defun gnosis-review-action--suspend (success thema result)
  "Suspend/unsuspend THEMA, returning unchanged (SUCCESS . RESULT)."
  (gnosis-toggle-suspend-themata
   (list thema) nil nil
   (lambda () (gnosis-review--check-result-content thema result)))
  (cons success result))

(defun gnosis-review-action--override (success thema result &optional alternate)
  "Override pending RESULT for THEMA by flipping binary SUCCESS.

Return the new (SUCCESS . RESULT) to the action reader.
Use cached ALTERNATE when supplied, matching the preview shown in the popup."
  (gnosis-review--check-result-content thema result)
  (let* ((success (not success))
         (new-result (or alternate (gnosis-review--override-result result success))))
    (gnosis-display-next-review
     (gnosis-review--result-date new-result) success)
    (cons success new-result)))

(defun gnosis-review-action--view-link (success thema result)
  "View linked node(s) for THEMA.
Return unchanged (SUCCESS . RESULT) after source navigation."
  (let* ((origin (current-buffer))
         (validate (lambda ()
                     (unless (buffer-live-p origin)
                       (user-error "Review buffer no longer exists"))
                     (with-current-buffer origin
                       (gnosis-review--check-result-content thema result)))))
    (funcall validate)
    (if (or (gnosis-get-linked-nodes thema)
            (progn (require 'gnosis-lecture)
                   (gnosis-lecture-sources (gnosis-get 'parathema 'extras `(= id ,thema)))))
        (condition-case err
            (progn (gnosis-view-linked-node thema validate)
                   (funcall validate)
                   (recursive-edit))
          (gnosis-lecture-error
           (funcall validate)
           (pop-to-buffer origin)
           (message "%s" (error-message-string err))))
      (message "No linked nodes for thema: %d" thema)
      (sleep-for 0.5))
    (funcall validate)
    (cons success result)))

(defun gnosis-review--accept (id success result)
  "Accept ID and SUCCESS using RESULT, preserving its identity on retry."
  (catch 'accepted
    (while t
      (condition-case err
          (throw 'accepted (gnosis-review-result id success result))
        (gnosis-review-content-changed (signal (car err) (cdr err)))
        (error
         (unless (y-or-n-p (format "%s; retry this grade? " (error-message-string err)))
           (signal (car err) (cdr err))))))))

(defvar-local gnosis-review--feedback nil
  "Owned feedback reader context, or nil outside post-answer input.")

(defun gnosis-review--feedback-check ()
  "Return the current feedback context after validating its input owner."
  (unless (and gnosis-review--feedback
               (eq (plist-get gnosis-review--feedback :buffer) (current-buffer))
               (= (plist-get gnosis-review--feedback :depth) (recursion-depth)))
    (user-error "No active feedback input in this buffer"))
  (gnosis-review--check-result-content
   (plist-get gnosis-review--feedback :id)
   (plist-get gnosis-review--feedback :result))
  gnosis-review--feedback)

(defun gnosis-review--feedback-select (choice)
  "Select CHOICE and exit only the owned feedback reader."
  (let ((context (gnosis-review--feedback-check)))
    (setf (plist-get context :choice) choice)
    (exit-recursive-edit)))

(defun gnosis-review-feedback-next ()
  "Accept the pending answer and continue reviewing."
  (interactive nil gnosis-review-feedback-mode)
  (gnosis-review--feedback-select ?n))

(defun gnosis-review-feedback-override ()
  "Override the pending answer without accepting it."
  (interactive nil gnosis-review-feedback-mode)
  (gnosis-review--feedback-select ?o))

(defun gnosis-review-feedback-quit ()
  "Accept the pending answer and quit reviewing."
  (interactive nil gnosis-review-feedback-mode)
  (gnosis-review--feedback-select ?q))

(defun gnosis-review-feedback-edit ()
  "Edit the thema without changing its pending answer."
  (interactive nil gnosis-review-feedback-mode)
  (gnosis-review--feedback-select ?e))

(defun gnosis-review-feedback-source ()
  "Visit the pending thema's source."
  (interactive nil gnosis-review-feedback-mode)
  (gnosis-review--feedback-select ?v))

(defun gnosis-review-feedback-flag ()
  "Flag the pending thema as needing work."
  (interactive nil gnosis-review-feedback-mode)
  (gnosis-review--feedback-select ?f))

(defun gnosis-review-feedback-suspend ()
  "Toggle suspension of the pending thema."
  (interactive nil gnosis-review-feedback-mode)
  (gnosis-review--feedback-select ?s))

(defun gnosis-review-feedback-delete ()
  "Request confirmation to delete the pending thema."
  (interactive nil gnosis-review-feedback-mode)
  (gnosis-review--feedback-select ?d))

(defun gnosis-review-feedback-cancel ()
  "Abort feedback input without accepting the pending answer."
  (interactive nil gnosis-review-feedback-mode)
  ;; Cancellation belongs to the input depth, not to still-valid content.
  ;; A retired encounter must remain cancellable without accepting it.
  (unless (and gnosis-review--feedback
               (= (plist-get gnosis-review--feedback :depth) (recursion-depth)))
    (user-error "No active feedback input in this buffer"))
  (abort-recursive-edit))

(defun gnosis-review--feedback-date (result)
  "Return a faced scheduling label for cached pending RESULT."
  (propertize
   (if (eq (plist-get result :mode) 'practice)
       "schedule unchanged"
     (pcase (gnosis-review--result-date result)
       (`(,year ,month ,day) (format "%04d-%02d-%02d" year month day))))
   'face 'keymap-popup-value))

(defun gnosis-review--feedback-next-label ()
  "Describe acceptance using the cached pending schedule."
  (if gnosis-review--feedback
      (concat "Next · " (gnosis-review--feedback-date
                         (plist-get gnosis-review--feedback :result)))
    "Next"))

(defun gnosis-review--feedback-override-label ()
  "Describe the cached current outcome and schedule without database reads."
  (if gnosis-review--feedback
      (concat "Override · "
              (propertize
               (if (plist-get gnosis-review--feedback :success) "Correct" "Incorrect")
               'face (if (plist-get gnosis-review--feedback :success) 'success 'error))
              " · " (gnosis-review--feedback-date
                       (plist-get gnosis-review--feedback :result)))
    "Override result"))

(defvar-keymap gnosis-review-feedback-mode-map
  :doc "Keymap for post-answer review input."
  "n" #'gnosis-review-feedback-next
  "o" #'gnosis-review-feedback-override
  "q" #'gnosis-review-feedback-quit
  "e" #'gnosis-review-feedback-edit
  "v" #'gnosis-review-feedback-source
  "f" #'gnosis-review-feedback-flag
  "s" #'gnosis-review-feedback-suspend
  "d" #'gnosis-review-feedback-delete
  "?" #'gnosis-review-feedback-menu
  "C-g" #'gnosis-review-feedback-cancel)

(keymap-popup-annotate gnosis-review-feedback-mode-map
  :exit-key "C-g"
  :persistent nil
  :description "Review answer"
  :group "Review"
  gnosis-review-feedback-next #'gnosis-review--feedback-next-label
  gnosis-review-feedback-override #'gnosis-review--feedback-override-label
  gnosis-review-feedback-quit "Accept & quit"
  :row
  :group "Content"
  gnosis-review-feedback-edit "Edit"
  gnosis-review-feedback-source "View source"
  :group "Manage"
  gnosis-review-feedback-flag "Flag needs_work"
  gnosis-review-feedback-suspend "Suspend / unsuspend"
  gnosis-review-feedback-delete "Delete")

(defun gnosis-review--feedback-show ()
  "Show feedback help and retain its exact disposable popup buffer."
  (let* ((reader gnosis-review--feedback)
         (origin (current-buffer))
         (backend (funcall keymap-popup-backend))
         (show (plist-get backend :show))
         (claimed nil)
         (keymap-popup-backend
          (lambda ()
            (plist-put (copy-sequence backend) :show
                       (lambda (popup)
                         ;; Capture before display callbacks can replace the
                         ;; source or open an unrelated successor popup.
                         (unless claimed
                           (setq claimed t)
                           (setf (plist-get reader :popup) popup))
                         (funcall show popup))))))
    (keymap-popup gnosis-review-feedback-mode-map)
    (unless (and (buffer-live-p origin)
                 (eq reader (buffer-local-value 'gnosis-review--feedback origin)))
      (user-error "Feedback input was replaced"))
    (with-current-buffer origin
      (gnosis-review--check-result-content
       (plist-get reader :id) (plist-get reader :result)))))

(defun gnosis-review-feedback-menu ()
  "Show the pending answer's feedback actions."
  (interactive nil gnosis-review-feedback-mode)
  (gnosis-review--feedback-check)
  (gnosis-review--feedback-show))

(define-minor-mode gnosis-review-feedback-mode
  "Expose post-answer actions while preserving ordinary review navigation.
The popup opens automatically; \\<gnosis-review-feedback-mode-map>\\[gnosis-review-feedback-menu] reopens it.
Dismiss the popup with `C-g' without accepting an answer.  Outside the
popup, \\[gnosis-review-feedback-cancel] aborts the pending input."
  :interactive nil
  :lighter nil
  :keymap gnosis-review-feedback-mode-map)

(defun gnosis-review--read-action (context)
  "Read one feedback action for owned CONTEXT through the native command loop.
CONTEXT contains :id, :success and :result; cache its :alternate preview
before showing the popup.  Return an action character without accepting it."
  (let* ((buffer (current-buffer))
         (reader (append (list :buffer buffer :depth (1+ (recursion-depth))
                               :choice nil :popup nil)
                         context)))
    (setf (plist-get context :alternate)
          (gnosis-review--override-result (plist-get context :result)
                                          (not (plist-get context :success))))
    (unwind-protect
        (progn
          (pop-to-buffer buffer)
          (unless (eq (current-buffer) buffer)
            (user-error "Feedback destination changed"))
          (gnosis-review--check-result-content
           (plist-get context :id) (plist-get context :result))
          (setq gnosis-review--feedback reader)
          (gnosis-review-feedback-mode 1)
          (unless (and (eq (current-buffer) buffer)
                       (eq gnosis-review--feedback reader))
            (user-error "Feedback input was replaced"))
          (gnosis-review--check-result-content
           (plist-get context :id) (plist-get context :result))
          (gnosis-review--feedback-show)
          (unless (and (eq (current-buffer) buffer)
                       (eq gnosis-review--feedback reader))
            (user-error "Feedback input was replaced"))
          (gnosis-review--check-result-content
           (plist-get context :id) (plist-get context :result))
          (recursive-edit)
          (or (plist-get reader :choice) (signal 'quit nil)))
      (when (and (plist-get reader :popup)
                 (eq (plist-get reader :popup) (keymap-popup--popup-buffer)))
        (keymap-popup-dismiss))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (eq gnosis-review--feedback reader)
            (gnosis-review-feedback-mode -1)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (when (eq gnosis-review--feedback reader)
                  (setq gnosis-review--feedback nil))))))))))

(defun gnosis-review-actions (success id result)
  "Specify action during review of thema.

SUCCESS: Review result.
ID: Thema ID.
RESULT: Return value of `gnosis-review-algorithm'.

Return :deleted only after confirmed deletion completes.  Declining
deletion returns to the action prompt with the same pending result.

Customize `gnosis-review-feedback-mode-map' to change feedback bindings."
  (gnosis-review--check-result-content id result)
  (let* ((gnosis-review--display-buffer (current-buffer))
         (gnosis-review--display-validate
          (lambda () (gnosis-review--check-result-content id result))))
    (catch 'done
      (while t
        ;; A callback may select another buffer; never adopt it as the owner.
        (unless (buffer-live-p gnosis-review--display-buffer)
          (user-error "Review buffer no longer exists"))
        (with-current-buffer gnosis-review--display-buffer
          (gnosis-review--check-result-content id result)
          (let* ((context (list :id id :success success :result result :alternate nil))
                 (choice (gnosis-review--read-action context)))
            (gnosis-review--check-result-content id result)
            (let ((next
                   (pcase choice
                     (?n (throw 'done (gnosis-review--accept id success result)))
                     (?o (gnosis-review-action--override
                          success id result (plist-get context :alternate)))
                     (?s (gnosis-review-action--suspend success id result))
                     (?d (when (gnosis-delete-thema
                                id nil (lambda () (gnosis-review--check-result-content id result)))
                           (throw 'done :deleted)))
                     (?f (gnosis-review--check-result-content id result)
                         (gnosis-study-flag id) nil)
                     (?e (gnosis-review-action--edit success id result))
                     (?v (gnosis-review-action--view-link success id result))
                     (?q (gnosis-review-action--quit success id result)))))
              (when next (setq success (car next) result (cdr next))))))))))

;;; Monkeytype integration

(defun gnosis-monkeytype-session (themata &rest _)
  "Start monkeytype session for THEMATA ids."
  (cl-assert (listp themata) nil "Themata must be a list of ids")
  (catch 'monkeytype-loop
    (cl-loop for thema in themata
	     do (gnosis-monkeytype-thema thema))))

;;;###autoload
(defun gnosis-monkeytype-start ()
  "Select themata and start typing practice without accepting study grades.
Leave the retained review batch, study evidence and schedules unchanged."
  (interactive)
  (gnosis-monkeytype-session
   (gnosis-review--selection-ids (gnosis-review--read-selection))))

(defun gnosis-monkeytype-thema (thema)
  "Process monkeytyping for THEMA id.

This is used to type the keimenon of thema, with the
answers highlighted."
  (apply #'gnosis-monkeytype
         (gnosis-monkeytype--thema-content
          (gnosis-select '[keimenon type answer] 'themata `(= id ,thema) t))))

;;; Entry points

(defun gnosis-review--read-selection (&optional kind)
  "Read filters and return a (KIND . TAGS) selection without starting study.
KIND is due, due-tags, overdue, without-overdue or tags.  Prompt when nil."
  (let ((kind (or kind
                  (pcase-exhaustive
                      (gnosis-completing-read "Select themata: " gnosis-review-types t)
                    ("Due themata" 'due)
                    ("Due themata of specified tag(s)" 'due-tags)
                    ("Overdue themata" 'overdue)
                    ("Due themata (Without Overdue)" 'without-overdue)
                    ("All themata of tag(s)" 'tags)))))
    (cons kind
          (pcase kind
            ('due-tags (gnosis-tags-filter-prompt
                        (gnosis-get-tags-for-ids (gnosis-review-get-due-themata))))
            ('tags (gnosis-tags-filter-prompt))))))

(defun gnosis-review--selection-ids (selection)
  "Return thema IDs for SELECTION without presenting or grading them.
SELECTION is a (KIND . TAGS) pair from `gnosis-review--read-selection'."
  (pcase-exhaustive (car selection)
    ((or 'due 'due-tags) (gnosis-collect-thema-ids :due t :tags (cdr selection)))
    ('overdue (gnosis-review-get-overdue-themata))
    ('without-overdue (cl-set-difference (gnosis-review-get-due-themata)
                                       (gnosis-review-get-overdue-themata)))
    ('tags (gnosis-collect-thema-ids :tags (cdr selection)))))

(defun gnosis-review-due ()
  "Start a scheduled review of due themata."
  (interactive)
  (gnosis-review-loop
   (lambda () (gnosis-review--selection-ids '(due)))))

(defun gnosis-review-due-tags ()
  "Start a scheduled review of due themata with selected tags."
  (interactive)
  (gnosis-review-loop
   (lambda ()
     (gnosis-review--selection-ids
      (gnosis-review--read-selection 'due-tags)))))

(defun gnosis-review-overdue ()
  "Start a scheduled review of overdue themata."
  (interactive)
  (gnosis-review-loop
   (lambda () (gnosis-review--selection-ids '(overdue)))))

(defun gnosis-review-without-overdue ()
  "Start a scheduled review of due themata excluding overdue themata."
  (interactive)
  (gnosis-review-loop
   (lambda () (gnosis-review--selection-ids '(without-overdue)))))

(defun gnosis-review-tags ()
  "Start a scheduled review of all themata with selected tags.
Include themata not yet due; accepted answers update their schedules."
  (interactive)
  (gnosis-review-loop
   (lambda ()
     (gnosis-review--selection-ids
      (gnosis-review--read-selection 'tags)))))

(keymap-popup-define gnosis-review-map
  "Review"
  :description "Review"
  :group "Review"
  "d" ("Due themata" gnosis-review-due)
  "t" ("Due themata of tag(s)" gnosis-review-due-tags)
  "o" ("Overdue themata" gnosis-review-overdue)
  "w" ("Due without overdue" gnosis-review-without-overdue)
  "T" ("All themata of tag(s)" gnosis-review-tags)
  :group "Topic"
  "n" ("Review due topic" gnosis-review-due-topic)
  "p" ("Practise topic (no rescheduling)" gnosis-practice-topic)
  "a" ("Review ahead topic (FSRS)" gnosis-review-topic)
  :group "Repair"
  "r" ("Repair themata" gnosis-study-repair)
  :group "Batch"
  "R" ("Resume unfinished" gnosis-review-resume)
  "D" ("Discard unfinished" gnosis-review-discard)
  "u" ("Undo last grade" gnosis-review-undo)
  :group "Retention"
  "s" ("Set desired retention" gnosis-scheduler-set-retention)
  "H" ("History evidence" gnosis-study-history-audit))

;;;###autoload
(defun gnosis-review ()
  "Start gnosis review session."
  (interactive)
  (keymap-popup gnosis-review-map))

(defun gnosis-review--select-topic ()
  "Prompt for topic and return its id."
  (let ((candidates (gnosis-study-topic-candidates)))
    (cdr (assoc (gnosis-completing-read "Select topic: " candidates t) candidates))))

;;;###autoload
(defun gnosis-review-topic (&optional node-id fwd-depth back-depth target validate)
  "Review ahead: reschedule all eligible themata linked to topic NODE-ID.
FWD-DEPTH and BACK-DEPTH control forward/backlink traversal depth.
With prefix arg, prompt for depths.  Optional TARGET is the database/checkpoint
captured by a caller before its own selection prompts.
Optional VALIDATE checks caller ownership before selection and replacement."
  (interactive
   (let ((target (gnosis-review--session-target)))
     (list nil
           (when current-prefix-arg (read-number "Forward link depth: " 1))
           (when current-prefix-arg (read-number "Backlink depth: " 0))
           target)))
  (when gnosis-review--running (user-error "Finish the active review first"))
  (when validate (funcall validate))
  (let* ((target (or target (gnosis-review--session-target)))
         (fwd-depth (or fwd-depth 0))
         (back-depth (or back-depth 0))
         (node-id (or node-id (gnosis-review--select-topic)))
         (_ (when validate (funcall validate)))
	 (node-title (car (gnosis-select 'title 'nodes
					 `(= id ,node-id) t)))
	 (node-ids (if (or (> fwd-depth 0) (> back-depth 0))
		       (gnosis-collect-nodes-at-depth
			node-id fwd-depth back-depth)
		     (list node-id)))
	 (gnosis-questions (gnosis-study-topic-ids node-ids)))
    (if (null gnosis-questions)
	(message "No thema found for %s (id:%s)" node-title node-id)
      (when (y-or-n-p
	     (format "Review %s thema(s) for '%s'%s?"
		     (length gnosis-questions) node-title
		     (if (> (length node-ids) 1)
			 (format " (%d nodes, fwd:%d back:%d)"
				 (length node-ids) fwd-depth back-depth)
		       "")))
	(gnosis-review-loop gnosis-questions nil target validate)))))

(provide 'gnosis-review)
;;; gnosis-review.el ends here
