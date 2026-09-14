;;; gnosis-agent.el --- Asynchronous native practice handoff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
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

;; Optional, agent-independent handoff to native human answering.  No grades
;; or learner transcript are synthesized here.  Practice and corrections use
;; the same durable core as interactive review.  API v1 returns keyword plists,
;; strings, numbers, vectors, t and :false; nil denotes JSON null.  Serialize
;; with `json-serialize' using :false-object :false and :null-object nil.

;;; Code:

(require 'gnosis-review)

(defvar gnosis-agent--launches nil
  "Pending launch records, owned by exact database, session and token.")

(defun gnosis-agent--session (session-id)
  "Read the exact agent practice SESSION-ID, never the latest batch."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (user-error "Session ID must be a nonempty string"))
  (let ((data (gnosis-get 'data 'study-history `(= session-id ,session-id))))
    (unless (and (equal 1 (plist-get data :version))
                 (eq 'practice (plist-get data :mode))
                 (plist-get data :policy))
      (user-error "Unknown agent practice session: %s" session-id))
    (gnosis-review--check-frozen-policy (plist-get data :policy))
    (apply #'gnosis-review-state-create :persistent-p t
           :database (gnosis--ensure-db) (cddr data))))

(defun gnosis-agent--current (session-id)
  "Return current agent SESSION-ID, refusing historical or active input."
  (when gnosis-review--running (user-error "Quit native review before changing the batch"))
  (let ((state (gnosis-agent--session session-id))
        (current (gnosis-review--read-session)))
    (unless (and current
                 (equal session-id (gnosis-review-state-session-id current))
                 (not (gnosis-review-state-cancelled-p state)))
      (user-error "Session is no longer the current batch"))
    state))

(defun gnosis-agent--release (record)
  "Cancel only the timer owned by launch RECORD and unpublish it."
  (setq gnosis-agent--launches (delq record gnosis-agent--launches))
  (when (timerp (plist-get record :timer))
    (cancel-timer (plist-get record :timer))))

(defun gnosis-agent--launch (record)
  "Present native input only while RECORD owns the exact pending batch.
A changed database, directory, frame, minibuffer or active review leaves the
batch unfinished for explicit resume.  Contain quit and errors at the timer
boundary; neither accepts a pending grade."
  (when (memq record gnosis-agent--launches)
    (gnosis-agent--release record)
    (condition-case err
        (when (and (eq gnosis-db (plist-get record :db))
                   (sqlitep gnosis-db)
                   (equal (expand-file-name gnosis-dir) (plist-get record :dir))
                   (frame-live-p (plist-get record :frame))
                   (not (active-minibuffer-window))
                   (not gnosis-review--running))
          (let ((state (gnosis-review--read-session)))
            (when (and state
                       (equal (plist-get record :session-id)
                              (gnosis-review-state-session-id state))
                       (equal (plist-get record :token)
                              (gnosis-review-state-launch-token state))
                       (gnosis-review-state-remaining state))
              (with-selected-frame (plist-get record :frame)
                (let ((gnosis-review-buffer-name (plist-get record :buffer-name)))
                  (gnosis-review--resume
                   (cons (plist-get record :db) (gnosis-review--state-data state))))))))
      (quit (message "Gnosis practice interrupted; resume the same session"))
      (error (message "Gnosis practice stopped: %s" (error-message-string err))))))

(defun gnosis-agent--schedule (state)
  "Schedule exact STATE for native input after returning to the event loop."
  (when (gnosis-review-state-remaining state)
    (let* ((token (gnosis-scheduler-event-id))
           (record (list :db (gnosis--ensure-db)
                         :dir (expand-file-name gnosis-dir)
                         :frame (selected-frame) :buffer-name gnosis-review-buffer-name
                         :session-id (gnosis-review-state-session-id state)
                         :token token :timer nil)))
      (gnosis-sqlite-with-transaction (gnosis--ensure-db)
        (unless (equal (gnosis-review--state-data state)
                       (gnosis-review--state-data (gnosis-agent--current
                                                   (gnosis-review-state-session-id state))))
          (user-error "Session changed before launch"))
        (setf (gnosis-review-state-launch-token state) token)
        (gnosis-review--save-session state))
      (dolist (old (copy-sequence gnosis-agent--launches))
        (when (eq (plist-get old :db) gnosis-db) (gnosis-agent--release old)))
      (push record gnosis-agent--launches)
      (condition-case err
          (setf (plist-get record :timer) (run-with-timer 0 nil #'gnosis-agent--launch record))
        (error
         (gnosis-agent--release record)
         (message "Practice reserved but not launched: %s" (error-message-string err)))))))

(cl-defun gnosis-agent-start-practice (&key thema-ids topic-ids limit policy)
          "Reserve native practice and return API v1 status immediately.
Supply exactly one nonempty list: integer THEMA-IDS or string TOPIC-IDS.
LIMIT is a required positive unique-item limit.  Select direct
topic links only, sorted by thema ID; exclude missing/suspended themata,
deduplicate before limiting and report shortfall.  Freeze validated POLICY
as described by `gnosis-review-practice-policy'.  A nonempty selection ends
unfinished progress early, retaining accepted evidence and schedules.  An empty
selection returns a completed shortfall report without replacing the batch.
Refuse active native input.  The human answers later; this records no grade."
          (unless (and (integerp limit) (> limit 0)
                       (if thema-ids (null topic-ids) topic-ids)
                       (proper-list-p (or thema-ids topic-ids))
                       (seq-every-p (if thema-ids #'integerp
                                      (lambda (id) (and (stringp id) (not (string-empty-p id)))))
                                    (or thema-ids topic-ids)))
            (user-error "Provide thema IDs or topic IDs and a positive unique limit"))
          (let* ((target (gnosis-review--session-target))
                 (policy (gnosis-review-practice-policy policy))
                 (topics (delete-dups (copy-sequence topic-ids)))
                 (unknown (seq-remove (lambda (id) (gnosis-get 'id 'nodes `(= id ,id))) topics)))
            (when unknown (user-error "Unknown topics: %S" unknown))
            (let* ((candidates (sort (delete-dups
                                      (copy-sequence
                                       (or thema-ids
                                           (gnosis-select 'source 'thema-links
                                                          `(in dest ,(vconcat topics)) t)))) #'<))
                   (eligible (seq-filter #'gnosis-study-eligible-p candidates))
                   (ids (seq-take eligible limit))
                   (selection (list :limit limit :candidates (length candidates)
                                    :eligible (length eligible) :selected (length ids)
                                    :shortfall (max 0 (- limit (length ids)))
                                    :omitted-by-limit (max 0 (- (length eligible) limit))
                                    :excluded-ids (vconcat (seq-difference candidates eligible))
                                    :topic-ids (vconcat topics)))
                   (state (gnosis-review--reserve-practice ids policy selection target)))
              (gnosis-agent--schedule state)
              (gnosis-agent-status (gnosis-review-state-session-id state)))))

(defun gnosis-agent-current-practice ()
  "Return status for the current practice reservation, or nil.
Read only the connected database's exact active checkpoint, including a
zero-attempt reservation.  Do not start, resume, cancel or select a batch.
A completed checkpoint remains discoverable until replaced or discarded;
scheduled sessions and cancelled reservations return nil."
  (gnosis-sqlite-with-transaction (gnosis--ensure-db)
    (when-let* ((state (gnosis-review--read-session))
                ((eq (gnosis-review-state-mode state) 'practice))
                ((gnosis-review-state-policy state))
                ((not (gnosis-review-state-cancelled-p state))))
      (gnosis-agent-status (gnosis-review-state-session-id state)))))

(defun gnosis-agent-status (session-id)
  "Return API v1 status and progress for exact SESSION-ID.
Statuses are pending, running, unfinished, completed and cancelled strings.
Pending is process-local: after restart a reserved batch is unfinished.
Cancelled includes batches ended early by a replacement, never completion.
Their remaining IDs record abandoned membership, not resumable active work.
Return :api-version 1, :session-id, :database (connected main filename),
:mode, :status, :schedule-updated :false,
:selected-ids and :remaining-ids vectors, frozen :policy, :selection counts,
:summary effective first/retry grade counts, and :targets reason counts.
Selection records limit, candidates, eligible, selected, shortfall,
omitted-by-limit, excluded-ids and topic-ids.  Targets count target-reached,
attempt-limit, unfinished and excluded items.  Summary needs-work counts
last effective failures, not unmet repetition targets; consult :targets.
Use `json-serialize' with :false-object :false and :null-object nil."
  (gnosis-sqlite-with-transaction (gnosis--ensure-db)
    (gnosis-agent--status (gnosis-agent--session session-id)
                          (gnosis-study-practice-events session-id))))

(defun gnosis-agent--status (state events)
  "Render API status from exact STATE and ordered retained EVENTS."
  (let* ((session-id (gnosis-review-state-session-id state))
           (token (gnosis-review-state-launch-token state))
           (pending (seq-some (lambda (record)
                                (and (eq gnosis-db (plist-get record :db))
                                     (equal token (plist-get record :token))))
                              gnosis-agent--launches))
           (policy (copy-sequence (gnosis-review-state-policy state)))
           (projection (gnosis-review-practice-projection state events)))
      (unless (plist-get policy :consecutive) (setq policy (plist-put policy :consecutive :false)))
      (list :api-version 1 :session-id session-id :mode "practice"
            :database (nth 2 (assoc 0 (sqlite-select (gnosis--ensure-db) "PRAGMA database_list")))
            :status (cond ((gnosis-review-state-cancelled-p state) "cancelled")
                          ((equal gnosis-review--running session-id) "running")
                          ((null (gnosis-review-state-remaining state)) "completed")
                          (pending "pending")
                          (t "unfinished"))
            :schedule-updated :false
            :selected-ids (vconcat (gnosis-review-state-selected state))
            :policy policy :selection (gnosis-review-state-selection state)
            :summary (gnosis-review-summary projection)
            :targets (gnosis-review-policy-summary projection)
            :remaining-ids (vconcat (gnosis-review-state-remaining state)))))

(defun gnosis-agent--item (state id events)
  "Return result for thema ID in STATE using retained EVENTS."
  (let* ((rows (seq-filter (lambda (row) (equal id (nth 1 row))) events))
         (effective (seq-remove (lambda (row) (nth 6 row)) rows))
         (ordered (mapcar (lambda (row) (= 3 (nth 5 row))) effective))
         (first-correct (seq-position ordered t))
         (outcomes (reverse ordered))
         (progress (gnosis-review-policy-progress (gnosis-review-state-policy state) outcomes))
         (deleted (not (gnosis-get 'id 'themata `(= id ,id))))
         (excluded (or deleted (member id (gnosis-review-state-skipped state)))))
    (list :thema-id id
          :reason (if excluded "excluded" (plist-get progress :reason))
          :deleted (if deleted t :false)
          :attempts (length effective) :total-attempts (length rows)
          :first-outcome (when effective (if (car ordered) "success" "failure"))
          :attempts-to-first-correct (and first-correct (1+ first-correct))
          :retries-to-first-correct first-correct
          :successes (plist-get progress :successes)
          :target (plist-get progress :target)
          :events
          (vconcat (mapcar
                    (lambda (row)
                      (list :event-id (car row) :attempt (nth 3 row)
                            :reviewed-at-us (nth 4 row)
                            :outcome (if (= 3 (nth 5 row)) "success" "failure")
                            :effective (if (nth 6 row) :false t)
                            :correction-id (nth 6 row)
                            :encounter (nth 7 row))) rows)))))

(defun gnosis-agent-results (session-id)
  "Return API v1 status, per-item outcomes and corrections for SESSION-ID.
The additional :items vector contains thema-id, reason, deleted, attempts,
successes, target and events.  Reasons are target-reached, attempt-limit,
unfinished or excluded strings.  Each event has event-id, attempt (the
session-wide ordinal), reviewed-at-us, accepted outcome (success/failure),
effective (t or :false) and correction-id (string or nil).  Event vectors
retain accepted grades and void identifiers.  Optional encounter data retains
versioned presented content, type-specific response, captured matching rules,
shown hints and the original pre-override outcome.  Nil means unavailable;
legacy events are never supplemented with current question content.
Items also include total-attempts (including voids), first-outcome and nullable
attempts-to-first-correct/retries-to-first-correct from effective evidence.
Effective grades alone determine first/retry counts and policy progress.
Hard thema deletion removes owned events; deleted membership remains visible.
Completion and immediate repetition are not mastery or calibrated retention."
  (gnosis-sqlite-with-transaction (gnosis--ensure-db)
    (let* ((state (gnosis-agent--session session-id))
           (events (gnosis-study-practice-events session-id t)))
      (append (gnosis-agent--status state events)
              (list :items (vconcat (mapcar (lambda (id) (gnosis-agent--item state id events))
                                            (gnosis-review-state-selected state))))))))

(defun gnosis-agent-resume (session-id)
  "Schedule exact current unfinished SESSION-ID and return API v1 status."
  (let ((state (gnosis-agent--current session-id)))
    (unless (gnosis-review-state-remaining state) (user-error "Batch is complete"))
    (gnosis-agent--schedule state)
    (gnosis-agent-status session-id)))

(defun gnosis-agent-cancel (session-id)
  "Cancel pending or interrupted SESSION-ID, retaining all accepted evidence.
Refuse active native input: the human must quit that input first.  Repeated
cancellation is harmless; completed and historical sessions are not changed."
  (let ((state (gnosis-agent--session session-id)))
    (unless (or (gnosis-review-state-cancelled-p state)
                (null (gnosis-review-state-remaining state)))
      (gnosis-sqlite-with-transaction (gnosis--ensure-db)
        (setq state (gnosis-agent--current session-id))
        (setf (gnosis-review-state-cancelled-p state) t
              (gnosis-review-state-launch-token state) nil)
        (gnosis-review--save-history state)
        (gnosis--delete 'study-session))
      (dolist (record (copy-sequence gnosis-agent--launches))
        (when (and (eq gnosis-db (plist-get record :db))
                   (equal session-id (plist-get record :session-id)))
          (gnosis-agent--release record))))
    (gnosis-agent-status session-id)))

(provide 'gnosis-agent)
;;; gnosis-agent.el ends here
