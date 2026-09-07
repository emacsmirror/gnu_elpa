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

(require 'gnosis)
(require 'gnosis-db)
(require 'gnosis-scheduler)
(require 'gnosis-study)
(require 'gnosis-cloze)
(require 'gnosis-vc)
(require 'gnosis-monkeytype)
(require 'gnosis-utils)
(require 'gnosis-nodes)
(require 'keymap-popup)

;;; Review vars

(defvar gnosis-review-types '("Due themata"
			      "Due themata of specified tag(s)"
			      "Overdue themata"
			      "Due themata (Without Overdue)"
			      "All themata of tag(s)"))

(defvar gnosis-review-buffer-name "*gnosis*"
  "Review buffer name.")

(defcustom gnosis-review-basic-input 'typed
  "Input style for basic questions.
Typed answers use string comparison with a correctable verdict.
Self-grade asks for recall, reveals the answer/checklist and parathema,
then asks for binary success.  Neither revealing nor editing accepts a grade."
  :type '(choice (const typed) (const self-grade))
  :group 'gnosis)

;;; Review state

(cl-defstruct (gnosis-review-state (:constructor gnosis-review-state-create))
  "State for a review session."
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
      (when (and (not (eq major-mode 'gnosis-mode))
                 (or buffer-file-name (> (buffer-size) 0)))
        (user-error "Review buffer contains unrelated content; rename it first"))
      (unless (eq major-mode 'gnosis-mode)
	(gnosis-mode))
      (setq gnosis-review--state
	    (gnosis-review-state-create
	     :mode (or mode 'due)
             :session-id (gnosis-scheduler-event-id)
             :event-id (gnosis-scheduler-event-id)
             :basic-input gnosis-review-basic-input
             :selected (copy-sequence themata)
             :initial (length themata)
             :reviewed 0
	     :total (length themata)
	     :remaining (copy-sequence themata)))
      (setq header-line-format '(:eval (gnosis-review--header-line))))
    buf))

;;; Display functions

(defun gnosis-display-keimenon (str)
  "Display STR as keimenon."
  (with-current-buffer gnosis-review-buffer-name
    (erase-buffer)
    (insert "\n" (gnosis-format-string str))
    (gnosis-insert-separator)
    (gnosis-apply-center-buffer-overlay)))

(defun gnosis-display-image (keimenon)
  "Display image link from KEIMENON in new window."
  (let ((image-path (and (string-match "\\[file:\\(.*?\\)\\]" keimenon)
			 (match-string 1 keimenon))))
    (when image-path
      (find-file-other-window image-path)
      (switch-to-buffer-other-window gnosis-review-buffer-name))))

(defun gnosis-display-cloze-string (str clozes hints correct false)
  "Display STR with CLOZES and HINTS.

Applies highlighting for CORRECT & FALSE."
  (let* ((cloze-str (gnosis-cloze-create str clozes))
	 (str-with-hints (gnosis-cloze-add-hints cloze-str hints))
	 (str-with-c-answers
	  (gnosis-utils-highlight-words str-with-hints correct 'gnosis-face-correct))
	 (final (gnosis-cloze-mark-false str-with-c-answers false)))
    (gnosis-display-keimenon final)))

(defun gnosis-display-basic-answer (answer success user-input)
  "Display ANSWER.

When SUCCESS nil, display USER-INPUT as well"
  (with-current-buffer gnosis-review-buffer-name
    (goto-char (point-max))
    (insert "\n\n"
	    (propertize "Answer:" 'face 'gnosis-face-directions)
	    " "
	    (propertize answer 'face 'gnosis-face-correct))
    (when gnosis-center-content
      (gnosis-center-current-line))
    ;; Insert user wrong answer
    (when (not success)
      (insert "\n"
	      (propertize "Your answer:" 'face 'gnosis-face-directions)
	      " "
	      (propertize user-input 'face 'gnosis-face-false))
      (when gnosis-center-content
	(gnosis-center-current-line)))))

(defun gnosis-display-hint (hint)
  "Display HINT."
  (let ((hint (or hint "")))
    (unless (string-empty-p hint)
      (goto-char (point-max))
      (and (not (string-empty-p hint))
	   (insert "\n"
		   (gnosis-format-string
		    (propertize hint 'face 'gnosis-face-hint))))
      (gnosis-insert-separator))))

(defun gnosis-display-cloze-user-answer (user-input &optional false)
  "Display USER-INPUT answer for cloze thema upon failed review.

If FALSE t, use gnosis-face-false face"
  (goto-char (point-max))
  (insert "\n\n"
	  (propertize "Your answer:" 'face 'gnosis-face-directions)
	  " "
	  (propertize user-input 'face
		      (if false 'gnosis-face-false 'gnosis-face-correct)))
  (when gnosis-center-content
    (gnosis-center-current-line))
  (newline))

(defun gnosis-display-correct-answer-mcq (answer user-choice)
  "Display correct ANSWER & USER-CHOICE for MCQ thema."
  (goto-char (point-max))
  (insert (gnosis-format-string
	   (format "%s %s\n%s %s"
		   (propertize "Correct Answer:" 'face 'gnosis-face-directions)
		   (propertize answer 'face 'gnosis-face-correct)
		   (propertize "Your answer:" 'face 'gnosis-face-directions)
		   (propertize user-choice 'face (if (string= answer user-choice)
						     'gnosis-face-correct
						   'gnosis-face-false))))
	  "\n")
  (gnosis-insert-separator))

(defun gnosis-display-parathema (parathema)
  "Display PARATHEMA."
  (when (and parathema (not (string-empty-p parathema)))
    (goto-char (point-max))
    (insert "\n"
	    (gnosis-format-string
	     (gnosis-org-format-string parathema))
	    "\n")))

(defun gnosis-display-next-review (interval success)
  "Display INTERVAL as next review date.
SUCCESS controls the face used when overriding a previous display."
  (with-current-buffer gnosis-review-buffer-name
    (if (null interval)
        (progn
          (goto-char (point-max))
          (unless (save-excursion (search-backward "Practice: schedule unchanged" nil t))
            (insert (propertize "\n\nPractice: schedule unchanged" 'face 'shadow))))
      (let ((next-review-msg (format "\n\n%s %s"
				   (propertize "Next review:" 'face 'gnosis-face-directions)
				   (propertize
				    (replace-regexp-in-string
				     "[]()[:space:]]"
				     (lambda (match)
				       (if (string= match " ") "/" ""))
				     (format "%s" interval) t t)
				    'face 'gnosis-face-next-review))))
      (if (search-backward "Next review" nil t)
	  ;; Delete previous result, and override with new -- this
	  ;; occurs only when used for overriding review result.
          (progn (delete-region (point) (progn (end-of-line) (point)))
		 (insert (propertize (replace-regexp-in-string "\n" "" next-review-msg)
				     'face (if success 'gnosis-face-correct
					     'gnosis-face-false))))
	;; Default behaviour
	(goto-char (point-max))
	(insert (gnosis-format-string next-review-msg)))))))

;;; Link view mode

(defun gnosis-get-linked-nodes (id)
  "Return the title of linked node(s) for thema ID."
  (let ((links (gnosis-select 'dest 'thema-links `(= source ,id) t)))
    (when links
      (mapcar #'car
	      (gnosis-sqlite-select-batch (gnosis--ensure-db)
					  "SELECT title FROM nodes WHERE id IN (%s)"
					  links)))))

(defun gnosis-view-linked-node (id)
  "Visit linked node(s) for thema ID."
  (let* ((ids (gnosis-select 'dest 'thema-links `(= source ,id) t))
         (candidates (and ids (gnosis-study-topic-candidates ids))))
    (unless candidates (user-error "No indexed source for this thema"))
    (let ((node (cdr (assoc (completing-read "Source: " candidates nil t)
                            candidates))))
      (window-configuration-to-register :gnosis-link-view)
      (gnosis-nodes-goto-id node)
      (gnosis-link-view-mode))))

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

Check if it's suspended, and if it's due today."
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
EVENT-ID, REVIEWED-AT-US, and REVIEW-DAY may pin deterministic facts."
  (let* ((event-id (or event-id
                       (and gnosis-review--state (gnosis-review-state-event-id gnosis-review--state))
                       (gnosis-scheduler-event-id)))
         (reviewed-at-us
          (or reviewed-at-us (car (time-convert nil 1000000))))
         (review-day (or review-day (gnosis--today-int)))
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
  (if (eq (plist-get result :mode) 'practice)
      (plist-put (copy-sequence result) :outcome (if success 'success 'failure))
    (gnosis-review--pending-result
     (plist-get result :thema-id) success
     (plist-get result :event-id) (plist-get result :reviewed-at-us)
     (plist-get result :review-day))))

(defun gnosis-review--result-date (result)
  "Return next review date from pending RESULT."
  (unless (eq (plist-get result :mode) 'practice)
    (gnosis--int-to-date
     (plist-get (plist-get result :preview) :due-day))))

(defun gnosis-review--write-result (id success result)
  "Accept pending RESULT for thema ID and binary SUCCESS."
  (let ((outcome (if success 'success 'failure)))
    (unless (and (= id (plist-get result :thema-id))
                 (eq outcome (plist-get result :outcome)))
      (error "Review result does not match final outcome"))
    (let ((accepted
           (if (eq (plist-get result :mode) 'practice)
               (gnosis-study-accept-practice result)
             (gnosis-scheduler-accept-review
              (plist-get result :event-id) id outcome
              (plist-get result :reviewed-at-us)
              (plist-get result :review-day)
              (plist-get (plist-get result :preview) :config-id)
              (plist-get result :preview)))))
      accepted)))

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

(defun gnosis-review--read-session ()
  "Return the retained session state, or nil."
  (when-let* ((data (gnosis-get 'data 'study-session '(= id 1))))
    (unless (equal 1 (plist-get data :version)) (error "Unsupported study session format"))
    (apply #'gnosis-review-state-create :persistent-p t (cddr data))))

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
Require positive integer targets and a cap, and a boolean
consecutive flag.  Reject unknown and duplicate keys and malformed plists."
  (let ((defaults '(:successes-required 1 :successes-after-failure 2
                   :consecutive t :max-attempts-per-thema 5))
        seen)
    (unless (and (proper-list-p policy) (zerop (% (length policy) 2)))
      (user-error "Practice policy must be a keyword plist"))
    (cl-loop for (key value) on policy by #'cddr do
             (unless (and (plist-member defaults key) (not (memq key seen))
                          (if (eq key :consecutive) (memq value '(nil t))
                            (and (integerp value) (> value 0))))
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
                        ((>= attempts (plist-get policy :max-attempts-per-thema))
                         "attempt-limit")
                        (t "unfinished")))))

(defun gnosis-review--reserve-practice (ids policy selection)
  "Reserve IDS for native practice with frozen POLICY and SELECTION metadata.
Return durable state without displaying a buffer or asking for an answer."
  (when gnosis-review--running (user-error "Finish the active review first"))
  (let ((policy (gnosis-review-practice-policy policy)))
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (when-let* ((old (gnosis-review--read-session))
                  (_ (gnosis-review-state-remaining old)))
        (user-error "Resume or discard the unfinished study session first"))
      (let ((state (gnosis-review-state-create
                    :mode 'practice :persistent-p t
                    :session-id (gnosis-scheduler-event-id)
                    :event-id (gnosis-scheduler-event-id)
                    :basic-input gnosis-review-basic-input
                    :policy policy :selection selection
                    :selected (copy-sequence ids) :remaining (copy-sequence ids)
                    :initial (length ids) :total (length ids))))
        (gnosis-review--save-session state)
        state))))

(defun gnosis-review--advance (state id success &optional skipped)
  "Advance STATE after ID and SUCCESS, or a SKIPPED presentation."
  (let* ((rest (cdr (gnosis-review-state-remaining state)))
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
         (tail (and retry (gnosis-study-eligible-p id))))
    (setf (gnosis-review-state-remaining state) (if tail (append rest (list id)) rest)
          (gnosis-review-state-event-id state) (gnosis-scheduler-event-id))
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

(defun gnosis-review--skip (state id)
  "Skip unavailable ID in STATE without overwriting newer durable progress."
  (if (not (gnosis-review-state-persistent-p state))
      (gnosis-review--advance state id nil t)
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
               (gnosis-review--advance current id nil t)
               (gnosis-review--save-session current)
               current))))
      (cl-loop for i from 1 below (length state) do (aset state i (aref stored i))))))

(defun gnosis-review-result (id success result)
  "Atomically accept RESULT for ID/SUCCESS and settle durable session progress."
  (let* ((state gnosis-review--state)
         (persistent (and state (gnosis-review-state-persistent-p state)))
         (db (gnosis--ensure-db))
         (accepted
          (gnosis-sqlite-with-transaction db
            (let ((stored (and persistent (gnosis-review--read-session))))
              (when persistent
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
                  (let ((before (gnosis-review--state-data stored)))
                    ;; Keep one checkpoint, never a chain of old checkpoints.
                    (setq before (plist-put before :undo nil))
                    (gnosis-review--advance stored id success)
                    (setf (gnosis-review-state-undo stored)
                          (list :event-id (plist-get result :event-id) :thema-id id
                                :correction-id (gnosis-scheduler-event-id) :before before)
                          (gnosis-review-state-last-event stored) (plist-get result :event-id))
                    (gnosis-review--save-session stored)))
                accepted)))))
    ;; Database authority has committed before UI projection.  A retry after
    ;; a UI failure resolves the retained event and settled checkpoint.
    (when persistent
      (let ((stored (gnosis-review--read-session)))
        (cl-loop for i from 1 below (length state) do (aset state i (aref stored i)))))
    (when gnosis-due-themata-total
      (setq gnosis-due-themata-total (length (gnosis-review-get-due-themata))))
    accepted))

;;; Type-specific review

(defun gnosis-review-mcq (id)
  "Review MCQ thema with ID."
  (let* ((data (car (gnosis-select '[keimenon answer] 'themata `(= id ,id))))
	 (keimenon (nth 0 data))
	 (answer (car (nth 1 data)))
	 (parathema (gnosis-get 'parathema 'extras `(= id ,id))))
    (gnosis-display-image keimenon)
    (gnosis-display-keimenon (gnosis-org-format-string keimenon))
    (let* ((user-choice (gnosis-mcq-answer id))
	   (success (string= answer user-choice))
	   (result (gnosis-review-algorithm id success)))
      (unless success (setq gnosis-review--monkeytype-text answer))
      (gnosis-display-correct-answer-mcq answer user-choice)
      (gnosis-display-parathema parathema)
      (gnosis-display-next-review (gnosis-review--result-date result) success)
      (cons success result))))

(defun gnosis-review-basic (id)
  "Review basic type thema for ID."
  (let* ((data (car (gnosis-select
		     '[keimenon hypothesis answer]
		     'themata `(= id ,id))))
	 (keimenon (nth 0 data))
	 (hypothesis (car (nth 1 data)))
	 (answer (car (nth 2 data)))
	 (parathema (gnosis-get 'parathema 'extras
				`(= id ,id))))
    (gnosis-display-image keimenon)
    (gnosis-display-keimenon (gnosis-org-format-string keimenon))
    (gnosis-display-hint hypothesis)
    (let* ((self-grade (eq gnosis-review-basic-input 'self-grade))
           (user-input
            (unless self-grade
              (gnosis--read-string-with-input-method "Answer: " answer)))
           (success
            (if self-grade
                (progn
                  (read-char-choice "Recall first; press SPC to reveal: " '(?\s))
                  (gnosis-display-basic-answer answer t "")
                  (gnosis-display-parathema parathema)
                  (= (read-char-choice "Recalled the checklist?  y yes, n no: " '(?y ?n)) ?y))
              (gnosis-compare-strings answer user-input)))
           (result (gnosis-review-algorithm id success)))
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

(defun gnosis-review-cloze--update-state
    (position unrevealed-clozes unrevealed-hints
	      all-clozes revealed-clozes)
  "Return updated cloze state after correct match at POSITION.
UNREVEALED-CLOZES, UNREVEALED-HINTS: remaining items.
ALL-CLOZES: original list for sort ordering.
REVEALED-CLOZES: previously matched items.
Returns (NEW-UNREVEALED NEW-HINTS NEW-REVEALED)."
  (let* ((matched (nth position unrevealed-clozes))
	 (new-revealed
	  (cl-sort (cons matched revealed-clozes)
		   #'< :key (lambda (c)
			      (cl-position c all-clozes))))
	 (new-unrevealed
	  (append (cl-subseq unrevealed-clozes 0 position)
		  (cl-subseq unrevealed-clozes (1+ position))))
	 (new-hints
	  (if (< position (length unrevealed-hints))
	      (append (cl-subseq unrevealed-hints 0 position)
		      (cl-subseq unrevealed-hints (1+ position)))
	    unrevealed-hints)))
    (list new-unrevealed new-hints new-revealed)))

(defun gnosis-review-cloze (id)
  "Review cloze type thema for ID."
  (let* ((data (car (gnosis-select
		     '[keimenon answer hypothesis]
		     'themata `(= id ,id))))
	 (keimenon (nth 0 data))
	 (all-clozes (nth 1 data))
	 (all-hints (nth 2 data))
	 (revealed-clozes '())
	 (unrevealed-clozes all-clozes)
	 (unrevealed-hints all-hints)
	 (parathema (gnosis-get 'parathema 'extras
				`(= id ,id)))
	 (success t))
    (gnosis-display-cloze-string
     keimenon unrevealed-clozes unrevealed-hints nil nil)
    (catch 'done
      (while unrevealed-clozes
	(let* ((input (gnosis-review-cloze--input
		       unrevealed-clozes))
	       (position (car input)))
	  (if position
	      (pcase-let ((`(,new-unrev ,new-hints ,new-rev)
			   (gnosis-review-cloze--update-state
			    position unrevealed-clozes
			    unrevealed-hints all-clozes
			    revealed-clozes)))
		(setq unrevealed-clozes new-unrev
		      unrevealed-hints new-hints
		      revealed-clozes new-rev)
		(gnosis-display-cloze-string
		 keimenon unrevealed-clozes
		 unrevealed-hints revealed-clozes nil))
	    (gnosis-display-cloze-string
	     keimenon nil nil
	     revealed-clozes unrevealed-clozes)
	    (gnosis-display-cloze-user-answer (cdr input))
	    (setq success nil
		  gnosis-review--monkeytype-text
		  (car unrevealed-clozes))
	    (throw 'done nil)))))
    (let ((result (gnosis-review-algorithm id success)))
      (gnosis-display-parathema parathema)
      (gnosis-display-next-review (gnosis-review--result-date result) success)
      (cons success result))))

(defun gnosis-review-mc-cloze (id)
  "Review mc-cloze type thema for ID."
  (let* ((data (car (gnosis-select
		     '[keimenon answer hypothesis]
		     'themata `(= id ,id))))
	 (keimenon (nth 0 data))
	 (cloze (nth 1 data))
	 (options (nth 2 data))
	 (parathema (gnosis-get 'parathema 'extras
				`(= id ,id)))
	 (user-input)
	 (success))
    (gnosis-display-cloze-string keimenon cloze nil nil nil)
    (setq user-input (gnosis-completing-read "Select answer: "
					     (gnosis-shuffle options)))
    (if (string= user-input (car cloze))
	(progn
	  (gnosis-display-cloze-string keimenon nil nil cloze nil)
	  (setq success t))
      (gnosis-display-cloze-string keimenon nil nil nil cloze)
      (gnosis-display-correct-answer-mcq (car cloze) user-input)
      (setq gnosis-review--monkeytype-text (car cloze)))
    (let ((result (gnosis-review-algorithm id success)))
      (gnosis-display-parathema parathema)
      (gnosis-display-next-review (gnosis-review--result-date result) success)
      (cons success result))))

(defun gnosis-review-is-thema-new-p (id)
  "Return t if thema with ID is new."
  (zerop (gnosis-get 'reps 'scheduler-state `(= thema-id ,id))))

;;; Session management

(defun gnosis-review--display-thema (id)
  "Display thema with ID and call the appropriate review func.
Returns (TYPE (SUCCESS . ALGORITHM-RESULT))."
  (let* ((type (gnosis-get 'type 'themata `(= id ,id)))
         (func-name (intern (format "gnosis-review-%s"
				    (downcase type)))))
    (if (fboundp func-name)
        (progn
	  (window-configuration-to-register :gnosis-pre-image)
          (list type (funcall func-name id)))
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
  (let ((remaining (gnosis-review-state-remaining state)))
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
        (gnosis-monkeytype gnosis-review--monkeytype-text))
      (let* ((disposition (gnosis-review-actions success thema result))
             (failed-p (gnosis-review--failed-disposition-p disposition))
             (requeued (gnosis-review-state-requeued state)))
        ;; Use jump-to-register after first review.
        (when (get-register :gnosis-pre-image)
          (jump-to-register :gnosis-pre-image))
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
    (force-mode-line-update)
    state)))


(defun gnosis-review-session (state)
  "Review the bounded remaining queue in review STATE.
Return STATE after completion."
  (if (null (gnosis-review-state-remaining state))
      (progn (message "No themata for review.") state)
    (while (gnosis-review-state-remaining state)
      (let ((id (car (gnosis-review-state-remaining state))))
        (if (gnosis-study-eligible-p id)
            (progn
              (pop-to-buffer-same-window gnosis-review-buffer-name)
              ;; Do not restore source windows or the preceding answer image.
              (delete-other-windows)
              (gnosis-review-process-thema id state))
          (gnosis-review--skip state id))))
    state))

(defun gnosis-review-summary (state)
  "Return truthful unique and attempt counts from accepted outcomes in STATE."
  (let* ((rows (reverse (gnosis-review-state-outcomes state)))
         (ids (delete-dups (mapcar #'car rows)))
         (first (mapcar (lambda (id) (assoc id rows)) ids))
         (last (mapcar (lambda (id) (assoc id (reverse rows))) ids))
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
  "Display accepted recall evidence from STATE, not a mastery estimate."
  (let* ((summary (gnosis-review-summary state))
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
              (format "Remaining due backlog: %d\n"
                      (let ((gnosis-new-themata-limit nil))
                        (length (gnosis-review-get-due-themata))))
              "Session recall is not topic mastery.  Practice is excluded from FSRS replay.\n")
      (when-let* ((policy (gnosis-review-state-policy state)))
        (let ((progress (gnosis-review-policy-summary state)))
          (insert (format "\nFrozen practice target: %d successes; after failure: %d; consecutive: %s; cap: %d effective attempts per thema\n"
                          (plist-get policy :successes-required)
                          (plist-get policy :successes-after-failure)
                          (if (plist-get policy :consecutive) "yes" "no")
                          (plist-get policy :max-attempts-per-thema))
                  (format "Targets reached: %d   Attempt limit (target unmet): %d   Unfinished: %d   Excluded: %d\n"
                          (plist-get progress :target-reached) (plist-get progress :attempt-limit)
                          (plist-get progress :unfinished) (plist-get progress :excluded)))))
      (gnosis-review-summary-mode))
    (pop-to-buffer buf)))

(keymap-popup-define gnosis-review-summary-mode-map
  "Study summary"
  :parent special-mode-map
  :group "Batch"
  "r" ("Resume" gnosis-review-resume)
  "c" ("Continue with another batch" gnosis-review-continue)
  "d" ("Discard progress" gnosis-review-discard)
  "u" ("Undo last accepted grade" gnosis-review-undo)
  :group "Repair"
  "w" ("Repair questions" gnosis-study-repair)
  "t" ("Study topic" gnosis-study-topic)
  :group "Navigate"
  "q" ("Quit" quit-window))

(define-derived-mode gnosis-review-summary-mode special-mode "Study Summary"
  "Inspect truthful study outcomes; use h for continuation and repair.")

(defun gnosis-review-loop (collector &optional mode)
  "Review one finite batch from COLLECTOR in MODE, defaulting to due.
COLLECTOR is a list of IDs or a function called exactly once.  Deduplicate
and freeze membership, then recheck deletion and suspension before each
presentation.  Practice records separate encounters and never reschedules.
Return the session state, also on ordinary quit.  Keyboard quit preserves
accepted grades and restores windows.  Cancelling an answer writes no grade."
  (when gnosis-review--running (user-error "Finish the active review first"))
  (unless (memq mode '(nil due practice)) (error "Unknown study mode"))
  (when-let* ((old (gnosis-review--read-session))
              (_ (gnosis-review-state-remaining old)))
    (user-error "Resume or discard the unfinished study session first"))
  (let* ((previous (gnosis-get 'data 'study-session '(= id 1)))
         (themata (seq-filter #'gnosis-study-eligible-p
                              (delete-dups (copy-sequence
                                            (if (functionp collector)
                                                (funcall collector) collector)))))
         (buf (gnosis-review--setup-buffer themata mode))
         (state (buffer-local-value 'gnosis-review--state buf)))
    (when themata
      (setf (gnosis-review-state-persistent-p state) t)
      (gnosis-sqlite-with-transaction (gnosis--ensure-db)
        (let ((current (gnosis-get 'data 'study-session '(= id 1))))
          (unless (and (equal previous current) (not (plist-get current :remaining)))
            (user-error "Study session changed; resume or discard it first"))
          (gnosis-review--save-session state))))
    (gnosis-review--run-state buf state)))

(defun gnosis-review--run-state (buf state)
  "Present STATE in BUF with frozen input policy and restored windows."
  (let ((gnosis-review-basic-input (gnosis-review-state-basic-input state))
        (reviewed (gnosis-review-state-reviewed state))
        (gnosis-review--running (gnosis-review-state-session-id state)))
    (unwind-protect
        (save-window-excursion
          (pop-to-buffer-same-window buf)
          (delete-other-windows)
          (catch 'review-loop (gnosis-review-session state))
          (when (> (gnosis-review-state-reviewed state) reviewed)
            (gnosis-review-commit (- (gnosis-review-state-reviewed state) reviewed))))
      (gnosis-review--show-summary state)))
  state)

;;;###autoload
(defun gnosis-review-resume ()
  "Resume the unfinished frozen batch, discarding any unaccepted reveal."
  (interactive)
  (when gnosis-review--running (user-error "Finish the active review first"))
  (let ((state (or (gnosis-review--read-session) (user-error "No study session"))))
    (unless (gnosis-review-state-remaining state) (user-error "Batch is complete"))
    ;; Invalidate any deferred adapter launch before entering native input.
    (when (gnosis-review-state-launch-token state)
      (gnosis-sqlite-with-transaction (gnosis--ensure-db)
        (unless (equal (gnosis-review--state-data state)
                       (gnosis-review--state-data (gnosis-review--read-session)))
          (user-error "Session changed before resume"))
        (setf (gnosis-review-state-launch-token state) nil)
        (gnosis-review--save-session state)))
    (let ((buf (gnosis-review--setup-buffer nil)))
      (with-current-buffer buf (setq gnosis-review--state state))
      (gnosis-review--run-state buf state))))

;;;###autoload
(defun gnosis-review-discard ()
  "Discard the retained batch and undo slot, never accepted evidence."
  (interactive)
  (when gnosis-review--running (user-error "Finish the active review first"))
  (let ((previous (gnosis-get 'data 'study-session '(= id 1))))
    (when (y-or-n-p "Discard batch progress (keep accepted grades)? ")
      (gnosis-sqlite-with-transaction (gnosis--ensure-db)
        (unless (equal previous (gnosis-get 'data 'study-session '(= id 1)))
          (user-error "Study session changed; inspect it before discarding"))
        (when-let* ((state (gnosis-review--read-session)))
          (setf (gnosis-review-state-cancelled-p state) t
                (gnosis-review-state-launch-token state) nil)
          (gnosis-review--save-history state))
        (gnosis--delete 'study-session)))))

;;;###autoload
(defun gnosis-review-continue ()
  "Deliberately select another batch after finishing the current one."
  (interactive)
  (when-let* ((state (gnosis-review--read-session))
              (_ (gnosis-review-state-remaining state)))
    (user-error "Finish, resume or discard the current batch first"))
  (gnosis-review))

;;;###autoload
(defun gnosis-review-undo (&optional event-id correction-id)
  "Undo the last accepted session grade, retaining append-only evidence.
Optional EVENT-ID and CORRECTION-ID pin an idempotent retry.  Reject stale
or superseded targets.  Re-answer with a fresh attempt identity."
  (interactive)
  (when gnosis-review--running (user-error "Quit the active review before undo"))
  (let* ((db (gnosis--ensure-db))
         (restored
          (gnosis-sqlite-with-transaction db
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
  "Commit review session on git repository.

This function initializes the `gnosis-dir' as a Git repository if it is not
already one.  It then adds the gnosis.db file to the repository and commits
the changes with a message containing the reviewed number THEMA-NUM."
  (if gnosis-testing
      (message "Review session finished.  %d review attempts accepted." thema-num)
    (gnosis--ensure-git-repo)
    (gnosis--git-chain
     `(("add" "gnosis.db")
       ("commit" "-m"
        ,(format "Total themata reviewed: %d" thema-num)))
     (lambda ()
       (when gnosis-vc-auto-push (gnosis-vc-push))
       (message "Review session finished.  %d review attempts accepted."
		thema-num)))))

;;; Review actions

(defun gnosis-review-action--edit (success thema result)
  "Edit THEMA during review.

Save current contents of *gnosis-edit* buffer, if any, and start
editing THEMA with its new contents.
RESULT is the algorithm result to thread through.

After done editing, call `gnosis-review-actions' with SUCCESS THEMA."
  (gnosis-edit-thema thema)
  (setf gnosis-review-editing-p t)
  (recursive-edit)
  (gnosis-review-actions success thema result))

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
  "Suspend/Unsuspend THEMA.
RESULT is the algorithm result to thread through.

This function should be used with `gnosis-review-actions', which
should be recursively called using SUCCESS and THEMA."
  (gnosis-toggle-suspend-themata (list thema))
  (gnosis-review-actions success thema result))

(defun gnosis-review-action--override (success thema result)
  "Override pending RESULT for THEMA by flipping binary SUCCESS.

This function should be used with `gnosis-review-actions', which will
be called with new SUCCESS value plus THEMA."
  (setf success (not success))
  (let ((new-result (gnosis-review--override-result result success)))
    (gnosis-display-next-review
     (gnosis-review--result-date new-result) success)
    (gnosis-review-actions success thema new-result)))

(defun gnosis-review-action--view-link (success thema result)
  "View linked node(s) for THEMA.
SUCCESS is the review result.
RESULT is the algorithm result to thread through."
  (if (gnosis-get-linked-nodes thema)
      (progn (gnosis-view-linked-node thema)
	     (recursive-edit))
    (message (format "No linked nodes for thema: %d" thema))
    (sleep-for 0.5))
  (gnosis-review-actions success thema result))

(defun gnosis-review--accept (id success result)
  "Accept ID and SUCCESS using RESULT, preserving its identity on retry."
  (catch 'accepted
    (while t
      (condition-case err
          (throw 'accepted (gnosis-review-result id success result))
        (error
         (unless (y-or-n-p (format "%s; retry this grade? " (error-message-string err)))
           (signal (car err) (cdr err))))))))

(defun gnosis-review-actions (success id result)
  "Specify action during review of thema.

SUCCESS: Review result.
ID: Thema ID.
RESULT: Return value of `gnosis-review-algorithm'.

To customize the keybindings, adjust `gnosis-review-keybindings'."
  (let* ((prompt
	  (concat "Action: %sext, %sverride result, "
		  "%suspend, %selete, %sdit thema, "
		  "%siew link, %suit (accept), f flag needs_work: "))
	 (choice (read-char-choice
		  (apply #'format prompt
			 (mapcar
			  (lambda (str) (propertize str 'face 'match))
			  '("n" "o" "s" "d" "e" "v" "q")))
		  '(?n ?o ?s ?d ?e ?v ?q ?f))))
    (pcase choice
      (?n (gnosis-review--accept id success result))
      (?o (gnosis-review-action--override success id result))
      (?s (gnosis-review-action--suspend success id result))
      (?d (gnosis-delete-thema id) :deleted)
      (?f (gnosis-study-flag id)
          (gnosis-review-actions success id result))
      (?e (gnosis-review-action--edit success id result))
      (?v (gnosis-review-action--view-link success id result))
      (?q (gnosis-review-action--quit success id result)))))

;;; Monkeytype integration

(defun gnosis-monkeytype-session (themata &rest _)
  "Start monkeytype session for THEMATA ids."
  (cl-assert (listp themata) nil "Themata must be a list of ids")
  (catch 'monkeytype-loop
    (cl-loop for thema in themata
	     do (gnosis-monkeytype-thema thema))))

;;;###autoload
(defun gnosis-monkeytype-start ()
  "Start a Gnosis Monkeytype session."
  (interactive)
  (gnosis-review #'gnosis-monkeytype-session))

(defun gnosis-monkeytype-thema (thema)
  "Process monkeytyping for THEMA id.

This is used to type the keimenon of thema, with the
answers highlighted."
  (let* ((thema-context
	  (gnosis-select '[keimenon type answer]
			 'themata `(= id ,thema) t))
	 (keimenon (replace-regexp-in-string
		    "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" "\\2" ;; remove links
		    (nth 0 thema-context)))
	 (type (nth 1 thema-context))
	 (answer (cl-loop for answer in (nth 2 thema-context)
			  collect (gnosis-utils-trim-quotes answer))))
    (cond ((string= type "basic")
	   (gnosis-monkeytype (concat keimenon "\n" (car answer)) answer))
	  (t (gnosis-monkeytype keimenon answer)))))

;;; Entry points

(keymap-popup-define gnosis-review-map
  "Review"
  :description "Review"
  :group "Review"
  "d" ("Due themata" (lambda () (interactive)
		       (gnosis-review-loop
			(lambda () (gnosis-collect-thema-ids :due t)))))
  "t" ("Due themata of tag(s)" (lambda () (interactive)
				 (let* ((due-tags (gnosis-get-tags-for-ids
						   (gnosis-review-get-due-themata)))
					(tags (gnosis-tags-filter-prompt due-tags)))
				   (gnosis-review-loop
				    (lambda () (gnosis-collect-thema-ids :due t :tags tags))))))
  "o" ("Overdue themata" (lambda () (interactive)
			   (gnosis-review-loop (gnosis-review-get-overdue-themata))))
  "w" ("Due without overdue" (lambda () (interactive)
			       (gnosis-review-loop
				(cl-set-difference
				 (mapcar #'car (gnosis-review-get--due-themata))
				 (gnosis-review-get-overdue-themata)))))
  "T" ("All themata of tag(s)" (lambda () (interactive)
				 (gnosis-review-loop
				  (gnosis-collect-thema-ids :tags (gnosis-tags-filter-prompt)))))
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

(defun gnosis-collect-nodes-at-depth (node-id &optional fwd-depth back-depth)
  "Collect node IDs reachable from NODE-ID within depth limits.
FWD-DEPTH is max hops for forward links (default 0).
BACK-DEPTH is max hops for backlinks (default 0).
Returns a deduplicated list including NODE-ID itself."
  (let* ((fwd-depth (or fwd-depth 0))
	(back-depth (or back-depth 0))
	(max-depth (max fwd-depth back-depth))
	(visited (make-hash-table :test 'equal))
	(queue (list node-id)))
    (puthash node-id t visited)
    (dotimes (level max-depth)
      (when queue
	(let* ((qvec (vconcat queue))
	       (neighbors (append
			   (when (< level fwd-depth)
			     (gnosis-select 'dest 'node-links
					    `(in source ,qvec) t))
			   (when (< level back-depth)
			     (gnosis-select 'source 'node-links
					    `(in dest ,qvec) t))))
	       (next-queue nil))
	  (dolist (neighbor neighbors)
	    (unless (gethash neighbor visited)
	      (puthash neighbor t visited)
	      (push neighbor next-queue)))
	  (setq queue next-queue))))
    (hash-table-keys visited)))

;;;###autoload
(defun gnosis-review-topic (&optional node-id fwd-depth back-depth)
  "Review ahead: reschedule all eligible themata linked to topic NODE-ID.
FWD-DEPTH and BACK-DEPTH control forward/backlink traversal depth.
With prefix arg, prompt for depths."
  (interactive
   (list nil
	 (when current-prefix-arg (read-number "Forward link depth: " 1))
	 (when current-prefix-arg (read-number "Backlink depth: " 0))))
  (let* ((node-id (or node-id (gnosis-review--select-topic)))
	 (fwd-depth (or fwd-depth 0))
	 (back-depth (or back-depth 0))
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
	(gnosis-review-loop gnosis-questions)))))

(provide 'gnosis-review)
;;; gnosis-review.el ends here
