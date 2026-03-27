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
;; - Algorithm bridge: computing next intervals and gnosis scores
;; - Monkeytype integration for typing practice
;; - Link view mode for viewing org-gnosis nodes during review

;;; Code:

(require 'gnosis)
(require 'gnosis-algorithm)
(require 'gnosis-monkeytype)
(require 'gnosis-utils)
(require 'gnosis-nodes)
(require 'transient)

;;; Review vars

(defvar gnosis-review-types '("Due themata"
			      "Due themata of specified tag(s)"
			      "Overdue themata"
			      "Due themata (Without Overdue)"
			      "All themata of tag(s)"))

(defvar gnosis-review-buffer-name "*gnosis*"
  "Review buffer name.")

;;; Review state

(cl-defstruct (gnosis-review-state (:constructor gnosis-review-state-create))
  "State for a review session."
  (reviewed 0 :type integer)
  (total 0 :type integer)
  (remaining nil :type list))

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

(defun gnosis-review--setup-buffer (themata)
  "Create or reset the review buffer for THEMATA.
Sets `gnosis-mode', initializes state struct, and installs `:eval' header.
Returns the buffer."
  (let ((buf (get-buffer-create gnosis-review-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'gnosis-mode)
	(gnosis-mode))
      (setq gnosis-review--state
	    (gnosis-review-state-create
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
	   (insert "\n" (gnosis-format-string (propertize hint 'face 'gnosis-face-hint))))
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
    (insert "\n" (gnosis-format-string (gnosis-org-format-string parathema)) "\n")))

(defun gnosis-display-next-review (interval success)
  "Display INTERVAL as next review date.
SUCCESS controls the face used when overriding a previous display."
  (with-current-buffer gnosis-review-buffer-name
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
	(insert (gnosis-format-string next-review-msg))))))

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
  (let* ((node (gnosis-completing-read "Select node: " (gnosis-get-linked-nodes id) t)))
    (window-configuration-to-register :gnosis-link-view)
    (gnosis-nodes-find node)
    (gnosis-link-view-mode)))

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
  (let ((next-rev (gnosis-get 'next-rev 'review-log `(= id ,id))))
    (<= next-rev (gnosis--today-int))))

(defun gnosis-review-get--due-themata ()
  "Return due thema IDs & due dates."
  (let* ((today (gnosis--today-int))
	 (old-themata (gnosis-select '[id next-rev] 'review-log
				     `(and (> n 0) (= suspend 0)
					   (<= next-rev ,today))))
	 (new-themata (gnosis-select '[id next-rev] 'review-log
				     `(and (= n 0) (= suspend 0)
					   (<= next-rev ,today)))))
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
    (gnosis-select 'id 'review-log
		   `(and (> n 0) (= suspend 0) (< next-rev ,today))
		   t)))

(defun gnosis-review-count-overdue ()
  "Return count of overdue themata."
  (let ((today (gnosis--today-int)))
    (or (caar (gnosis-sqlite-select (gnosis--ensure-db)
	        "SELECT COUNT(*) FROM review_log WHERE n > 0 AND suspend = 0 AND next_rev < ?"
		(list today)))
	0)))

;;; Algorithm bridge

(defun gnosis-review-algorithm (id success &optional tags)
  "Return next review date, gnosis score, and log data for thema ID.

SUCCESS is a boolean value, t for success, nil for failure.
TAGS, when non-nil, are passed to custom value lookups so they
skip the per-thema tag query.

Returns (NEXT-REV GNOSIS-SCORE LOG-ALIST) where LOG-ALIST has
keys n, c-success, c-fails, t-success, t-fails for
`gnosis-review--update'."
  (let* (;; Fetch all review-log fields in one query (includes n, t-fails)
	 (log-data (car (gnosis-select '[t-success c-success c-fails
					last-rev next-rev n t-fails]
				       'review-log `(= id ,id))))
	 (t-success (nth 0 log-data))
	 (c-success (nth 1 log-data))
	 (c-fails (nth 2 log-data))
	 (last-interval (gnosis-algorithm-date-diff (gnosis--int-to-date (nth 3 log-data))))
	 (existing-next-rev (gnosis--int-to-date (nth 4 log-data)))
	 (n (nth 5 log-data))
	 (t-fails (nth 6 log-data))
	 (gnosis (gnosis-get 'gnosis 'review `(= id ,id)))
	 ;; Pass tags to skip per-thema tag query
	 (amnesia (gnosis-get-thema-amnesia nil tags))
	 (lethe (gnosis-get-thema-lethe nil tags))
	 (computed-next-rev (gnosis-algorithm-next-interval
			     :last-interval last-interval
			     :gnosis-synolon (nth 2 gnosis)
			     :success success
			     :successful-reviews t-success
			     :c-fails c-fails
			     :lethe lethe
			     :amnesia amnesia
			     :proto (gnosis-get-thema-proto nil tags)))
	 ;; On success, keep the later of computed vs existing to prevent
	 ;; early reviews from deflating intervals.
	 (next-rev (if (and success
			    (gnosis-algorithm--date-later-p existing-next-rev computed-next-rev))
		       existing-next-rev
		     computed-next-rev)))
    (list
     next-rev
     (gnosis-algorithm-next-gnosis
      :gnosis gnosis
      :success success
      :epignosis (gnosis-get-thema-epignosis nil tags)
      :agnoia (gnosis-get-thema-agnoia nil tags)
      :anagnosis (gnosis-get-thema-anagnosis nil tags)
      :c-successes (if success (1+ c-success) 0)
      :c-failures (if success 0 (1+ c-fails))
      :lethe lethe)
     `((n . ,n) (c-success . ,c-success) (c-fails . ,c-fails)
       (t-success . ,t-success) (t-fails . ,t-fails)))))

(defun gnosis-review--update (id success result)
  "Update review-log for thema ID.

SUCCESS is a boolean value, t for success, nil for failure.
RESULT is the return value of `gnosis-review-algorithm'."
  (let* ((next-rev (nth 0 result))
	 (gnosis-score (nth 1 result))
	 (log-alist (nth 2 result))
	 (n (alist-get 'n log-alist))
	 (c-success (alist-get 'c-success log-alist))
	 (c-fails (alist-get 'c-fails log-alist))
	 (t-success (alist-get 't-success log-alist))
	 (t-fails (alist-get 't-fails log-alist)))
    (gnosis-review-increment-activity-log (not (> n 0)))
    ;; Single review-log UPDATE
    (gnosis-sqlite-execute (gnosis--ensure-db)
	     "UPDATE review_log SET last_rev = ?, next_rev = ?, n = ?, c_success = ?, c_fails = ?, t_success = ?, t_fails = ? WHERE id = ?"
	     (list (gnosis--today-int) (gnosis--date-to-int next-rev) (1+ n)
		   (if success (1+ c-success) 0)
		   (if success 0 (1+ c-fails))
		   (if success (1+ t-success) t-success)
		   (if success t-fails (1+ t-fails))
		   id))
    ;; Single review UPDATE
    (gnosis-update 'review `(= gnosis ',gnosis-score) `(= id ,id))))

(defun gnosis-review-result (id success result)
  "Update review thema ID results for SUCCESS.
RESULT is the return value of `gnosis-review-algorithm'."
  (gnosis-review--update id success result)
  (when (and gnosis-due-themata-total (> gnosis-due-themata-total 0))
    (cl-decf gnosis-due-themata-total)))

;;; Type-specific review

(defun gnosis-review-mcq (id tags)
  "Review MCQ thema with ID.
TAGS are pre-fetched for custom value lookup."
  (let* ((data (car (gnosis-select '[keimenon answer] 'themata `(= id ,id))))
	 (keimenon (nth 0 data))
	 (answer (car (nth 1 data)))
	 (parathema (gnosis-get 'parathema 'extras `(= id ,id))))
    (gnosis-display-image keimenon)
    (gnosis-display-keimenon (gnosis-org-format-string keimenon))
    (let* ((user-choice (gnosis-mcq-answer id))
	   (success (string= answer user-choice))
	   (result (gnosis-review-algorithm id success tags)))
      (unless success (setq gnosis-review--monkeytype-text answer))
      (gnosis-display-correct-answer-mcq answer user-choice)
      (gnosis-display-parathema parathema)
      (gnosis-display-next-review (nth 0 result) success)
      (cons success result))))

(defun gnosis-review-basic (id tags)
  "Review basic type thema for ID.
TAGS are pre-fetched for custom value lookup."
  (let* ((data (car (gnosis-select '[keimenon hypothesis answer] 'themata `(= id ,id))))
	 (keimenon (nth 0 data))
	 (hypothesis (car (nth 1 data)))
	 (answer (car (nth 2 data)))
	 (parathema (gnosis-get 'parathema 'extras `(= id ,id))))
    (gnosis-display-image keimenon)
    (gnosis-display-keimenon (gnosis-org-format-string keimenon))
    (gnosis-display-hint hypothesis)
    (let* ((user-input (gnosis--read-string-with-input-method "Answer: " answer))
	   (success (gnosis-compare-strings answer user-input))
	   (result (gnosis-review-algorithm id success tags)))
      (unless success (setq gnosis-review--monkeytype-text answer))
      (gnosis-display-basic-answer answer success user-input)
      (gnosis-display-parathema parathema)
      (gnosis-display-next-review (nth 0 result) success)
      (cons success result))))

(defun gnosis-review-cloze--input (clozes &optional user-input)
  "Prompt for USER-INPUT during cloze review.

CLOZES is a list of possible correct answers.

Returns a cons; ='(position . user-input) if correct,
='(nil . user-input) if incorrect."
  (let* ((user-input (or user-input
                         (gnosis--read-string-with-input-method
                          "Answer: " (car clozes))))
         (position (cl-position user-input clozes :test #'gnosis-compare-strings)))
    (cons position user-input)))

(defun gnosis-review-cloze (id tags)
  "Review cloze type thema for ID.
TAGS are pre-fetched for custom value lookup."
  (let* ((data (car (gnosis-select '[keimenon answer hypothesis] 'themata `(= id ,id))))
	 (keimenon (nth 0 data))
         (all-clozes (nth 1 data))
         (all-hints (nth 2 data))
         (revealed-clozes '()) ;; List of revealed clozes
         (unrevealed-clozes all-clozes)
         (unrevealed-hints all-hints)
         (parathema (gnosis-get 'parathema 'extras `(= id ,id)))
         (success t))
    ;; Initially display the sentence with no reveals
    (gnosis-display-cloze-string keimenon unrevealed-clozes unrevealed-hints nil nil)
    (catch 'done
      (while unrevealed-clozes
        (let* ((input (gnosis-review-cloze--input unrevealed-clozes))
               (position (car input))
               (matched-cloze (when position (nth position unrevealed-clozes)))
               (matched-hint (when (and position (< position (length unrevealed-hints)))
                               (nth position unrevealed-hints))))
          (if matched-cloze
              ;; Correct answer - move cloze from unrevealed to revealed
              (progn
                ;; Add to revealed clozes list, preserving original order
                (setq revealed-clozes
                      (cl-sort (cons matched-cloze revealed-clozes)
                               #'< :key (lambda (cloze)
                                          (cl-position cloze all-clozes))))
                ;; Remove from unrevealed lists by position
                (setq unrevealed-clozes (append (cl-subseq unrevealed-clozes 0 position)
                                               (cl-subseq unrevealed-clozes (1+ position))))
                (when (and matched-hint (< position (length unrevealed-hints)))
		  (setq unrevealed-hints (append (cl-subseq unrevealed-hints 0 position)
                                                (cl-subseq unrevealed-hints (1+ position)))))
                ;; Display with updated revealed/unrevealed lists
                (gnosis-display-cloze-string keimenon unrevealed-clozes unrevealed-hints
                                           revealed-clozes nil))
            ;; Incorrect answer
            (gnosis-display-cloze-string keimenon nil nil
                                       revealed-clozes unrevealed-clozes)
            (gnosis-display-cloze-user-answer (cdr input))
            (setq success nil)
            (setq gnosis-review--monkeytype-text (car unrevealed-clozes))
            (throw 'done nil)))))
    (let ((result (gnosis-review-algorithm id success tags)))
      (gnosis-display-parathema parathema)
      (gnosis-display-next-review (nth 0 result) success)
      (cons success result))))

(defun gnosis-review-mc-cloze (id tags)
  "Review mc-cloze type thema for ID.
TAGS are pre-fetched for custom value lookup."
  (let* ((data (car (gnosis-select '[keimenon answer hypothesis] 'themata `(= id ,id))))
	 (keimenon (nth 0 data))
	 (cloze (nth 1 data))
	 (options (nth 2 data))
	 (parathema (gnosis-get 'parathema 'extras `(= id ,id)))
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
    (let ((result (gnosis-review-algorithm id success tags)))
      (gnosis-display-parathema parathema)
      (gnosis-display-next-review (nth 0 result) success)
      (cons success result))))

(defun gnosis-review-is-thema-new-p (id)
  "Return t if thema with ID is new."
  (let ((reviews (car (gnosis-select 'n 'review-log `(= id ,id) t))))
    (not (> reviews 0))))

;;; Activity log

(defun gnosis-review-increment-activity-log (new? &optional date)
  "Increment activity log for DATE by one.

If NEW? is non-nil, increment new themata log by 1."
  (let* ((current-total-value (gnosis-get-date-total-themata))
	 (inc-total (cl-incf current-total-value))
	 (current-new-value (gnosis-get-date-new-themata))
	 (inc-new (cl-incf current-new-value))
	 (date (or date (gnosis--today-int))))
    (gnosis-update 'activity-log `(= reviewed-total ,inc-total) `(= date ,date))
    (and new? (gnosis-update 'activity-log `(= reviewed-new ,inc-new) `(= date ,date)))))

(defun gnosis-history-clear ()
  "Delete all activity log entries."
  (interactive)
  (when (y-or-n-p "Delete all activity log?")
    (gnosis-sqlite-execute (gnosis--ensure-db) "DELETE FROM activity_log")))

;;; Session management

(defun gnosis-review--display-thema (id)
  "Display thema with ID and call the appropriate review func.
Fetches tags once and passes them to the type-specific function.
Returns (SUCCESS . ALGORITHM-RESULT)."
  (let* ((type (gnosis-get 'type 'themata `(= id ,id)))
         (tags (gnosis-select 'tag 'thema-tag `(= thema-id ,id) t))
         (func-name (intern (format "gnosis-review-%s" (downcase type)))))
    (if (fboundp func-name)
        (progn
	  (window-configuration-to-register :gnosis-pre-image)
          (funcall func-name id tags))
      (error "Malformed thema type: '%s'" type))))

(defun gnosis-review-process-thema (thema state)
  "Process review for THEMA and update STATE.

Displays the thema, processes the review result, increments the
reviewed count, and pops from remaining.  Forces header redisplay.
Returns STATE.

This is a helper function for `gnosis-review-session'."
  (let* ((gnosis-review--monkeytype-text nil)
	 (review-cons (gnosis-review--display-thema thema))
	 (success (car review-cons))
	 (result (cdr review-cons)))
    (when (and (not success)
	       gnosis-review--monkeytype-text
	       gnosis-monkeytype-enable
	       (member (gnosis-get 'type 'themata `(= id ,thema))
		       gnosis-monkeytype-themata))
      (gnosis-monkeytype gnosis-review--monkeytype-text))
    (gnosis-review-actions success thema result)
    ;; Use jump-to-register after first review.
    (when (get-register :gnosis-pre-image)
      (jump-to-register :gnosis-pre-image))
    (cl-incf (gnosis-review-state-reviewed state))
    (setf (gnosis-review-state-remaining state)
	  (remove thema (gnosis-review-state-remaining state)))
    (force-mode-line-update)
    state))


(defun gnosis-review-session (themata state)
  "Review THEMATA in a single pass using STATE.
THEMATA: list of thema IDs.
STATE: a `gnosis-review-state' struct.
Returns STATE."
  (if (null themata)
      (progn (message "No themata for review.") state)
    (cl-loop for thema in themata
	     do (gnosis-review-process-thema thema state))
    state))

(defun gnosis-review-loop (collector)
  "Run review sessions using COLLECTOR, then commit and show dashboard.
COLLECTOR is either:
- a function returning thema IDs (called repeatedly until it returns nil)
- a list of thema IDs (reviewed once)

Sets up session state, then loops: collect IDs, review them, repeat.
The loop is wrapped in a `review-loop' catch so that
`gnosis-review-action--quit' can break out at any point."
  (setq gnosis-due-themata-total (length (gnosis-review-get-due-themata)))
  (set-register :gnosis-pre-image nil)
  (let* ((fn (if (functionp collector)
		 collector
	       (let ((ids collector))
		 (lambda () (prog1 ids (setq ids nil))))))
	 (themata (funcall fn))
	 (buf (gnosis-review--setup-buffer themata))
	 (state (buffer-local-value 'gnosis-review--state buf)))
    (pop-to-buffer-same-window buf)
    (catch 'review-loop
      (while themata
	(gnosis-review-session themata state)
	(setq themata (funcall fn))
	(when themata
	  (message "New %d remaining themata" (length themata))
	  (setf (gnosis-review-state-remaining state)
		(copy-sequence themata))
	  (cl-incf (gnosis-review-state-total state) (length themata))
	  (force-mode-line-update))))
    (gnosis-dashboard)
    (gnosis-review-commit (gnosis-review-state-reviewed state))))

(defun gnosis-review-commit (thema-num)
  "Commit review session on git repository.

This function initializes the `gnosis-dir' as a Git repository if it is not
already one.  It then adds the gnosis.db file to the repository and commits
the changes with a message containing the reviewed number THEMA-NUM."
  (let ((default-directory gnosis-dir))
    (unless (executable-find "git")
      (error "Git not found, please install git"))
    (unless (file-exists-p (expand-file-name ".git" gnosis-dir))
      (vc-git-create-repo))
    (unless gnosis-testing
      (call-process (executable-find "git") nil nil nil "add" "gnosis.db")
      (gnosis--git-cmd
       (list "commit" "-m"
             (format "Total themata reviewed: %d" thema-num))))
    (sit-for 0.1)
    (when (and gnosis-vc-auto-push (not gnosis-testing))
      (gnosis-vc-push))
    (message "Review session finished.  %d themata reviewed." thema-num)))

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
  (gnosis-review-result thema success result)
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
  "Override current review result for SUCCESS.
RESULT is the current algorithm result; will be recomputed with
the flipped SUCCESS value.

This function should be used with `gnosis-review-actions', which will
be called with new SUCCESS value plus THEMA."
  (setf success (not success))
  (let* ((tags (gnosis-select 'tag 'thema-tag `(= thema-id ,thema) t))
	 (new-result (gnosis-review-algorithm thema success tags)))
    (gnosis-display-next-review (nth 0 new-result) success)
    (gnosis-review-actions success thema new-result)))

(defun gnosis-review-action--view-link (success thema result)
  "View linked node(s) for THEMA.
RESULT is the algorithm result to thread through."
  (if (gnosis-get-linked-nodes thema)
    (progn (gnosis-view-linked-node thema)
	   (recursive-edit))
    (message (format "No linked nodes for thema: %d" thema))
    (sleep-for 0.5))
  (gnosis-review-actions success thema result))

(defun gnosis-review-actions (success id result)
  "Specify action during review of thema.

SUCCESS: Review result.
ID: Thema ID.
RESULT: Return value of `gnosis-review-algorithm'.

To customize the keybindings, adjust `gnosis-review-keybindings'."
  (let* ((prompt
	  "Action: %sext, %sverride result, %suspend, %selete, %sdit thema, %siew link, %suit: ")
	 (choice (read-char-choice
		  (apply #'format prompt
			 (mapcar
			  (lambda (str) (propertize str 'face 'match))
			  '("n" "o" "s" "d" "e" "v" "q")))
		  '(?n ?o ?s ?d ?e ?v ?q))))
    (pcase choice
      (?n (gnosis-review-result id success result))
      (?o (gnosis-review-action--override success id result))
      (?s (gnosis-review-action--suspend success id result))
      (?d (gnosis-delete-thema id))
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

(defun gnosis-monkeytype-start ()
  "Gnosis Monkeytype Session"
  (interactive)
  (gnosis-review #'gnosis-monkeytype-session))

(defun gnosis-monkeytype-thema (thema)
  "Process monkeytyping for THEMA id.

This is used to type the keimenon of thema, with the answers highlighted."
  (let* ((thema-context (gnosis-select '[keimenon type answer] 'themata `(= id ,thema) t))
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

(transient-define-prefix gnosis-review ()
  "Start gnosis review session."
  [["Review"
    ("d" "Due themata" (lambda () (interactive)
			 (gnosis-review-loop
			  (lambda () (gnosis-collect-thema-ids :due t)))))
    ("t" "Due themata of tag(s)" (lambda () (interactive)
				   (let* ((due-tags (gnosis-get-tags-for-ids
						     (gnosis-review-get-due-themata)))
					  (tags (gnosis-tags-filter-prompt due-tags)))
				     (gnosis-review-loop
				      (lambda () (gnosis-collect-thema-ids :due t :tags tags))))))
    ("o" "Overdue themata" (lambda () (interactive)
			     (gnosis-review-loop (gnosis-review-get-overdue-themata))))
    ("w" "Due without overdue" (lambda () (interactive)
				 (gnosis-review-loop
				  (cl-set-difference
				   (mapcar #'car (gnosis-review-get--due-themata))
				   (gnosis-review-get-overdue-themata)))))
    ("T" "All themata of tag(s)" (lambda () (interactive)
				   (gnosis-review-loop
				    (gnosis-collect-thema-ids :tags (gnosis-tags-filter-prompt)))))
    ("n" "Review node" gnosis-review-topic)
    ("q" "Quit" transient-quit-one)]])

(defun gnosis-review--select-topic ()
  "Prompt for topic and return its id."
  (let* ((topic-title (gnosis-completing-read "Select topic: "
					      (gnosis-select 'title 'nodes nil t)))
	 (topic-id (caar (gnosis-select 'id 'nodes `(= title ,topic-title)))))
    topic-id))

(defun gnosis-collect-nodes-at-depth (node-id &optional fwd-depth back-depth)
  "Collect node IDs reachable from NODE-ID within depth limits.
FWD-DEPTH is max hops for forward links (default 0).
BACK-DEPTH is max hops for backlinks (default 0).
Returns a deduplicated list including NODE-ID itself."
  (let ((fwd-depth (or fwd-depth 0))
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
  "Review themata linked to topic NODE-ID.
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
	 (gnosis-questions (gnosis-select 'source 'thema-links
					  `(in dest ,(vconcat node-ids)) t)))
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
