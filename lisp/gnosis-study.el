;;; gnosis-study.el --- Topic study and practice evidence -*- lexical-binding: t; -*-

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

;; Topics select existing themata, not new deck ownership.  Practice writes
;; separate exposure evidence, never scheduler history.  Full database backups
;; retain it; portable content exports deliberately contain no study history.
;; Hard deletion of a thema deletes its practice evidence, as for reviews.

;;; Code:

(require 'gnosis)
(require 'gnosis-nodes)
(require 'gnosis-links)
(require 'gnosis-tags)
(require 'keymap-popup)
(require 'tabulated-list)

(declare-function gnosis-review-loop "gnosis-review" (collector &optional mode target validate))
(declare-function gnosis-review--session-target "gnosis-review" ())
(declare-function gnosis-review-resume "gnosis-review" ())
(declare-function gnosis-review-undo "gnosis-review" (&optional event-id correction-id))
(declare-function gnosis-dashboard-history "gnosis-dashboard" (&optional history))

(defvar gnosis-review--running)

(defun gnosis-study-topic-candidates (&optional ids)
  "Return title-based completion candidates, optionally for IDS.
Distinguish duplicate titles by source file, then by an occurrence number.
Keep Org IDs in the values, not the labels."
  (let* ((rows (gnosis-select '[id title file] 'nodes))
         (counts (make-hash-table :test #'equal))
         (used (make-hash-table :test #'equal)))
    (dolist (row rows)
      (puthash (cadr row) (1+ (gethash (cadr row) counts 0)) counts)
      (puthash (cadr row) t used))
    (let ((candidates
           (mapcar
            (lambda (row)
              (let* ((title (cadr row))
                     (label
                      (if (= 1 (gethash title counts)) title
                        (let* ((base (format "%s — %s" title (nth 2 row)))
                               (label (cl-loop for n from 1
                                               for candidate = (if (= n 1) base
                                                                 (format "%s <%d>" base n))
                                               unless (gethash candidate used)
                                               return candidate)))
                          (puthash label t used)
                          label))))
                (cons label (car row))))
            rows)))
      (if ids (seq-filter (lambda (candidate) (member (cdr candidate) ids)) candidates)
        candidates))))

(defun gnosis-study-read-topics ()
  "Read one or more topic IDs, distinguishing duplicate titles."
  (let* ((candidates (gnosis-study-topic-candidates))
         (choices (completing-read-multiple "Topics: " candidates nil t)))
    (delete-dups (mapcar (lambda (label) (cdr (assoc label candidates))) choices))))

(defun gnosis-study-eligible-p (id)
  "Return non-nil if ID still exists and is not suspended."
  (and (gnosis-get 'id 'themata `(= id ,id))
       (equal 0 (gnosis-get 'suspended 'scheduler-state `(= thema-id ,id)))))

(defun gnosis-study-topic-ids (nodes &optional due fwd back)
  "Return unique eligible themata linked to NODES.
When DUE is non-nil, exclude not-due items.  FWD and BACK are explicit
bounded graph depths, both defaulting to zero.  Ignore the daily new limit."
  (unless (and (natnump (or fwd 0)) (natnump (or back 0)))
    (user-error "Graph depths must be nonnegative integers"))
  (let* ((nodes (delete-dups
                 (apply #'append
                        (mapcar (lambda (id)
                                  (gnosis-collect-nodes-at-depth id fwd back))
                                nodes))))
         (ids (when nodes
                (delete-dups
                 (gnosis-select 'source 'thema-links
                                `(in dest ,(vconcat nodes)) t)))))
    (seq-filter
     (lambda (id)
       (and (gnosis-study-eligible-p id)
            (or (not due)
                (<= (gnosis-get 'due-day 'scheduler-state `(= thema-id ,id))
                    (gnosis--today-int)))))
     ids)))

(defun gnosis-study-composition (ids)
  "Return counts for unique IDS as a plist.
Due and new counts include only active items; new can also be due."
  (let* ((ids (delete-dups (copy-sequence ids)))
         (rows (when ids (gnosis-select '[reps due-day suspended thema-id]
                                         'scheduler-state
                                         `(in thema-id ,(vconcat ids)))))
         (active (seq-filter (lambda (row) (zerop (nth 2 row))) rows))
         (due (seq-count
               (lambda (row)
                 (<= (cadr row) (gnosis--today-int)))
               active)))
    (list :total (length rows) :eligible (length active)
          :suspended (- (length rows) (length active))
          :new (seq-count (lambda (row) (zerop (car row))) active)
          :due due :not-due (- (length active) due))))

(defun gnosis-study--start (nodes mode &optional fwd back target validate)
  "Preview and start a finite batch for NODES in MODE using FWD/BACK depths.
Read topics when NODES is nil.  Optional TARGET is the database/checkpoint
captured before earlier prompts, otherwise capture it before collecting.
Optional VALIDATE checks the caller before selection and batch replacement."
  (require 'gnosis-review)
  (when gnosis-review--running (user-error "Finish the active review first"))
  (when validate (funcall validate))
  (let* ((target (or target (gnosis-review--session-target)))
         (nodes (or nodes (gnosis-study-read-topics)))
         (_ (when validate (funcall validate)))
         (ids (gnosis-study-topic-ids nodes (eq mode 'due) fwd back))
         (counts (gnosis-study-composition ids))
         (candidates (gnosis-study-topic-candidates nodes))
         (scope (mapconcat (lambda (id)
                             (or (car (rassoc id candidates)) "Unknown topic"))
                           nodes ", ")))
    (if (null ids) (message "No eligible themata for %s" scope)
      (when (y-or-n-p
             (format "%s: %s — %d %s (%d new, %d not due).%s Retry missed answers once.  Start? "
                     (if (eq mode 'practice) "Practise without rescheduling" "Review due (reschedules)")
                     scope (length ids) (if (= (length ids) 1) "thema" "themata")
                     (plist-get counts :new)
                     (plist-get counts :not-due)
                     (if (or (> (or fwd 0) 0) (> (or back 0) 0))
                         (format " Link depth: %d forward, %d backward." (or fwd 0) (or back 0))
                       "")))
        (gnosis-review-loop (gnosis-shuffle ids) mode target validate)))))

;;;###autoload
(defun gnosis-practice-topic (&optional nodes fwd back target validate)
  "Practise NODES without rescheduling, including new and not-due themata.
NODES is a list of Org IDs.  Interactively select topics; with a prefix,
prompt for bounded FWD and BACK graph depths.  Retry each failure at most once.
Optional TARGET is the database/checkpoint captured before earlier prompts.
Optional VALIDATE checks caller ownership before selection and replacement."
  (interactive
   (progn
     (require 'gnosis-review)
     (let ((target (gnosis-review--session-target)))
       (list nil
             (when current-prefix-arg (read-number "Forward depth: " 0))
             (when current-prefix-arg (read-number "Backlink depth: " 0))
             target))))
  (gnosis-study--start nodes 'practice fwd back target validate))

;;;###autoload
(defun gnosis-review-due-topic (&optional nodes fwd back target validate)
  "Review due themata of NODES with normal FSRS acceptance.
NODES is a list of Org IDs.  FWD and BACK optionally expand graph selection.
This explicit topic batch is not capped by the daily new-item limit.
Optional TARGET is the database/checkpoint captured before earlier prompts.
Optional VALIDATE checks caller ownership before selection and replacement."
  (interactive
   (progn
     (require 'gnosis-review)
     (let ((target (gnosis-review--session-target)))
       (list nil
             (when current-prefix-arg (read-number "Forward depth: " 0))
             (when current-prefix-arg (read-number "Backlink depth: " 0))
             target))))
  (gnosis-study--start nodes 'due fwd back target validate))

;;;###autoload
(defun gnosis-study-subtree (&optional due)
  "Practise topics structurally inside the current Org subtree.
With prefix DUE, review only due themata with normal rescheduling.
Include the enclosing source ID and descendant IDs, without graph traversal."
  (interactive "P")
  (unless (derived-mode-p 'org-mode) (user-error "Visit an Org source first"))
  (let ((owner (gnosis-org-get-id))
        (ids (save-excursion
               (save-restriction
                 (unless (org-before-first-heading-p) (org-narrow-to-subtree))
                 (org-map-entries (lambda () (org-entry-get nil "ID")))))))
    (unless owner (user-error "Add an Org ID to the source first"))
    (gnosis-study--start (delete-dups (delq nil (cons owner ids)))
                         (if due 'due 'practice))))

(defun gnosis-study--check-encounter (data)
  "Validate optional versioned practice encounter DATA and return it.
Nil means historical or unobservable evidence, never reconstructed content."
  (when data
    (unless (and (proper-list-p data) (zerop (% (length data) 2))
                 (equal (plist-get data :version) 1)
                 (stringp (plist-get data :kind))
                 (stringp (plist-get data :prompt))
                 (seq-every-p (lambda (key) (vectorp (plist-get data key)))
                              '(:hypothesis :expected-answers :accepted-aliases
                                :hints-available :hints-shown))
                 (member (plist-get data :original-outcome) '("success" "failure"))
                 (plist-member data :response)
                 (plist-member data :coaching)
                 (plist-member data :parathema))
      (error "Unsupported practice encounter format"))
    ;; Reject nonportable objects rather than retain unreadable encounter data.
    (ignore (json-serialize data :false-object :false :null-object nil)))
  data)

(defun gnosis-study-accept-practice (result)
  "Accept pending practice RESULT idempotently, without scheduler writes.
Reject reuse of an encounter identity with different facts.  Retain evidence
until hard thema deletion.  Content exports exclude all study evidence."
  (let* ((db (gnosis--ensure-db))
         (encounter (gnosis-study--check-encounter (plist-get result :encounter)))
         (row (list (plist-get result :event-id) (plist-get result :thema-id)
                    (plist-get result :session-id) (plist-get result :attempt)
                    (plist-get result :reviewed-at-us)
                    (pcase-exhaustive (plist-get result :outcome)
                      ('success 3) ('failure 1)))))
    (unless (and (eq (plist-get result :mode) 'practice)
                 (stringp (nth 0 row)) (not (string-empty-p (nth 0 row)))
                 (integerp (nth 1 row))
                 (stringp (nth 2 row)) (not (string-empty-p (nth 2 row)))
                 (integerp (nth 3 row)) (> (nth 3 row) 0)
                 (integerp (nth 4 row)) (> (nth 4 row) 0))
      (error "Invalid practice encounter"))
    (gnosis-sqlite-with-transaction db
      (let ((existing (car (gnosis-sqlite-select
                            db "SELECT * FROM practice_events WHERE event_id = ?"
                            (list (car row))))))
        (when (gnosis-get 'event-id 'practice-voids `(= event-id ,(car row)))
          (error "Practice encounter was voided"))
        (if existing
            (unless (and (equal existing row)
                         (equal encounter (gnosis-get 'data 'practice-encounters
                                                       `(= event-id ,(car row)))))
              (error "Practice identity conflict"))
          (unless (gnosis-get 'id 'themata `(= id ,(nth 1 row)))
            (user-error "Thema was deleted before acceptance"))
          (gnosis-sqlite-execute db "INSERT INTO practice_events VALUES (?, ?, ?, ?, ?, ?)" row)
          (when encounter
            (gnosis-sqlite-execute db "INSERT INTO practice_encounters VALUES (?, ?)"
                                   (list (car row) encounter))))))
    (list :event-id (car row) :rating (nth 5 row))))

(defun gnosis-study--practice-day-events (date)
  "Return effective practice (REVIEWED-AT-US THEMA-ID) rows for DATE.
DATE is a YYYYMMDD integer.  Exclude voided events.  Read neighboring
calendar days, then use the encounter wall-clock rule.  Keep ambiguous
cutoffs, including a repeated midnight, strictly inside the coarse range
rather than on either boundary."
  (let* ((calendar (gnosis--int-to-date date))
         (bounds (mapcar
                  (lambda (offset)
                    (car (time-convert
                          (encode-time 0 0 0 (+ (nth 2 calendar) offset)
                                       (nth 1 calendar) (car calendar))
                          1000000)))
                  '(-1 2))))
    (cl-loop for (time id) in
             (gnosis-sqlite-select
              (gnosis--ensure-db)
              "SELECT reviewed_at_us, thema_id FROM practice_events
                WHERE reviewed_at_us >= ? AND reviewed_at_us < ?
                  AND event_id NOT IN (SELECT event_id FROM practice_voids)"
              bounds)
             when (equal calendar (gnosis-date nil (cons time 1000000)))
             collect (list time id))))

(defun gnosis-study-activity (&optional date)
  "Return accepted study attempt counts for logical DATE as a plist.
DATE defaults to today, as a YYYYMMDD integer.  Return :total, :scheduled,
:practice and :new (new scheduled reviews only).  Count effective accepted
attempts, including retries, not distinct themata.  Exclude voided events.
Scheduled dates remain as recorded, including legacy daily aggregates.
Practice timestamps use the current local timezone and `gnosis-day-start-hour';
historical practice did not retain its original day-boundary settings."
  (let* ((today (gnosis-date))
         (date (or date (gnosis--date-to-int today)))
         (scheduled (gnosis-review-activity date))
         (practice (length (gnosis-study--practice-day-events date))))
    (list :total (+ (nth 1 scheduled) practice)
          :scheduled (nth 1 scheduled) :practice practice :new (nth 2 scheduled))))

(defun gnosis-study-practice-history ()
  "Return practice session plists from retained event evidence.
Each session has :session-id, :first-us and :last-us (recorded timestamps),
:attempts, :unique, :successes (effective evidence), :voids and :status.
Include sessions with only voided evidence, but not empty reservations.
Session snapshots supply status only, never grades or fabricated timestamps.
A session whose snapshot is absent has status Recorded.  Completed refers
to queue completion, not mastery.  Hard thema deletion removes its events."
  (mapcar
   (lambda (row)
     (let ((data (nth 7 row)))
       (list :session-id (nth 0 row) :first-us (nth 1 row) :last-us (nth 2 row)
             :attempts (nth 3 row) :unique (nth 4 row) :successes (nth 5 row)
             :voids (nth 6 row)
             :status (cond ((null data) "Recorded")
                           ((plist-get data :cancelled-p) "Ended early")
                           ((plist-get data :remaining) "Unfinished")
                           (t "Completed")))))
   (gnosis-sqlite-select
    (gnosis--ensure-db)
    "SELECT e.session_id, MIN(e.reviewed_at_us), MAX(e.reviewed_at_us),
            SUM(v.event_id IS NULL),
            COUNT(DISTINCT CASE WHEN v.event_id IS NULL THEN e.thema_id END),
            SUM(v.event_id IS NULL AND e.rating = 3),
            COUNT(v.event_id), h.data
       FROM practice_events e
       LEFT JOIN practice_voids v ON v.event_id = e.event_id
       LEFT JOIN study_history h ON h.session_id = e.session_id
      GROUP BY e.session_id ORDER BY MIN(e.reviewed_at_us), e.session_id")))

(defun gnosis-study-practice-events (session-id &optional encounters)
  "Return recorded practice events and corrections for exact SESSION-ID.
Rows contain event ID, thema ID, session ID, attempt ordinal, timestamp in
microseconds, rating, and correction ID (nil for effective evidence).
With ENCOUNTERS, append captured answer/context data, or nil when unavailable.
Order by the session-wide attempt ordinal; never infer grades from a queue."
  (let ((rows (gnosis-sqlite-select
               (gnosis--ensure-db)
               (concat "SELECT e.*, v.correction_id"
                       (when encounters ", c.data")
                       " FROM practice_events e
                         LEFT JOIN practice_voids v ON v.event_id = e.event_id "
                       (when encounters "LEFT JOIN practice_encounters c ON c.event_id = e.event_id ")
                       "WHERE e.session_id = ? ORDER BY e.attempt")
               (list session-id))))
    (when encounters
      (mapc (lambda (row) (gnosis-study--check-encounter (nth 7 row))) rows))
    rows))

;;;###autoload
(defun gnosis-backup-db (file)
  "Write a consistent full database backup to new FILE without upgrading.
Read the existing gnosis.db independently, even before first Gnosis use;
never initialize or migrate it.  Retain its schema version, schedules,
review and practice evidence, and indexes.  This is not a content export.
Restore with every database owner disconnected, using the matching source
version.  Back up Org source files and media separately."
  (interactive "FNew full database backup: ")
  (let ((file (expand-file-name file))
        (source (expand-file-name "gnosis.db" gnosis-dir)))
    (when (or (file-exists-p file) (file-symlink-p file))
      (user-error "Backup target already exists"))
    (unless (file-regular-p source) (user-error "No existing Gnosis database"))
    (let ((db (sqlite-open source)))
      (unwind-protect
          (sqlite-execute db "VACUUM INTO ?" (list file))
        (sqlite-close db)))
    (message "Full database backup: %s" file)))

(defun gnosis-study-flag (id &optional clear)
  "Flag thema ID with the ordinary needs_work tag, or CLEAR that flag."
  (gnosis-modify-thema-tags (list id) (unless clear '("needs_work"))
                            (when clear '("needs_work"))))

(defun gnosis-study-delayed-evidence ()
  "Return (ID FAILURES SAMPLE) rows for delayed scheduled first attempts.
Only events with elapsed_days > 0 count.  Immediate retries, new encounters
and all practice are excluded.  No automatic suspension threshold is applied."
  (gnosis-sqlite-select
   (gnosis--ensure-db)
   "SELECT thema_id, SUM(CASE rating WHEN 1 THEN 1 ELSE 0 END), COUNT(*)
      FROM review_events WHERE elapsed_days > 0
       AND event_id NOT IN (SELECT event_id FROM review_voids)
     GROUP BY thema_id ORDER BY thema_id"))

(defvar-local gnosis-study--topic nil "Org ID of the displayed study topic.")
(defvar-local gnosis-study--repair-p nil "Non-nil for the repair collection.")
(defvar-local gnosis-study--owner nil
  "Buffer and database pair identifying this rendering of the collection.")

(defun gnosis-study--check-owner (&optional owner)
  "Return current collection OWNER, or signal if its rendering is stale.
An explicit OWNER may be checked after source navigation or a prompt."
  (let* ((owner (or owner gnosis-study--owner))
         (buffer (car owner))
         (database (cdr owner)))
    (unless (and (buffer-live-p buffer)
                 (eq database gnosis-db)
                 database
                 (with-current-buffer buffer
                   (and (derived-mode-p 'gnosis-study-mode)
                        (not buffer-file-name)
                        (eq owner gnosis-study--owner))))
      (user-error "Study view is stale; refresh or reopen the collection"))
    owner))

(defun gnosis-study-refresh ()
  "Refresh the collection from the current database, preserving row identity.
This explicitly adopts the current database for subsequent row commands."
  (interactive)
  (unless (and (derived-mode-p 'gnosis-study-mode)
               (not buffer-file-name)
               (or gnosis-study--topic gnosis-study--repair-p))
    (user-error "Open a study collection first"))
  (setq gnosis-study--owner nil)
  (let* ((database (gnosis--ensure-db))
         (evidence (gnosis-study-delayed-evidence))
         (ids (if gnosis-study--repair-p
                  (delete-dups
                   (append (gnosis-get-tag-themata "needs_work")
                           (mapcar #'car (seq-filter (lambda (row) (> (cadr row) 1)) evidence))))
                (gnosis-select 'source 'thema-links `(= dest ,gnosis-study--topic) t)))
         (counts (gnosis-study-composition ids)))
    (setq tabulated-list-entries
          (mapcar
           (lambda (id)
             (let ((row (assoc id evidence)))
               (list id (vector
                         (gnosis-get 'keimenon 'themata `(= id ,id))
                         (if (member "needs_work" (gnosis-get-tags-for-ids (list id)))
                             (propertize "needs_work" 'face 'warning) "")
                         (cond ((not (gnosis-study-eligible-p id)) "Suspended")
                               ((zerop (gnosis-get 'reps 'scheduler-state `(= thema-id ,id))) "New")
                               ((<= (gnosis-get 'due-day 'scheduler-state `(= thema-id ,id))
                                    (gnosis--today-int)) "Due")
                               (t "Not due"))
                         (format "%d / %d" (or (cadr row) 0) (or (nth 2 row) 0))))))
           ids)
          header-line-format
          (format "%s | %d linked, %d due, %d new, %d suspended | ? help%s"
                  (if gnosis-study--repair-p "Repair: flags or repeated delayed failures"
                    (format "%s [id:%s]" (gnosis-get 'title 'nodes `(= id ,gnosis-study--topic))
                            gnosis-study--topic))
                  (plist-get counts :total) (plist-get counts :due)
                  (plist-get counts :new) (plist-get counts :suspended)
                  (if (null ids) " | No linked questions: authoring gap candidate" "")))
    (tabulated-list-print t)
    (setq gnosis-study--owner (cons (current-buffer) database))))

(defun gnosis-study-edit ()
  "Edit the selected thema, visiting its indexed source when available.
Do not record recall or replace an unfinished edit."
  (interactive)
  (let ((owner (gnosis-study--check-owner))
        (id (or (tabulated-list-get-id) (user-error "No thema at point"))))
    (when (and (get-buffer "*Gnosis Edit*")
               (buffer-modified-p (get-buffer "*Gnosis Edit*")))
      (user-error "Finish the existing Gnosis edit first"))
    (gnosis-study-source t)
    (gnosis-study--check-owner owner)
    (gnosis-edit-thema id)))

(defun gnosis-study-source (&optional if-available)
  "Visit the topic source or an identity-selected source of the current thema.
With IF-AVAILABLE non-nil, do nothing when no indexed source exists."
  (interactive)
  (let* ((owner (gnosis-study--check-owner))
         (id (tabulated-list-get-id))
         (nodes (if gnosis-study--topic (list gnosis-study--topic)
                  (and id (gnosis-select 'dest 'thema-links `(= source ,id) t))))
         (candidates (and nodes (gnosis-study-topic-candidates nodes))))
    (if candidates
        (let ((node (if (= (length candidates) 1) (cdar candidates)
                      (cdr (assoc (completing-read "Source: " candidates nil t)
                                  candidates)))))
          (gnosis-study--check-owner owner)
          (gnosis-nodes-goto-id node))
      (unless if-available (user-error "No indexed source for this thema")))))

(defun gnosis-study-create ()
  "Compose a basic thema from the selected topic source."
  (interactive)
  (unless gnosis-study--topic (user-error "Open a topic first"))
  (let ((owner (gnosis-study--check-owner)))
    (gnosis-study-source)
    (gnosis-study--check-owner owner)
    (gnosis-add-thema-from-node)))

(defun gnosis-study-due ()
  "Review due themata of the displayed topic."
  (interactive)
  (unless gnosis-study--topic (user-error "Open a topic first"))
  (let ((owner (gnosis-study--check-owner)))
    (gnosis-review-due-topic
     (list gnosis-study--topic) nil nil nil
     (lambda () (gnosis-study--check-owner owner)))))

(defun gnosis-study-practice ()
  "Practise the displayed topic without rescheduling."
  (interactive)
  (unless gnosis-study--topic (user-error "Open a topic first"))
  (let ((owner (gnosis-study--check-owner)))
    (gnosis-practice-topic
     (list gnosis-study--topic) nil nil nil
     (lambda () (gnosis-study--check-owner owner)))))

(defun gnosis-study-toggle-flag ()
  "Toggle needs_work for the thema at point."
  (interactive)
  (gnosis-study--check-owner)
  (let ((id (or (tabulated-list-get-id) (user-error "No thema at point"))))
    (gnosis-study-flag id (member "needs_work" (gnosis-get-tags-for-ids (list id))))
    (gnosis-study-refresh)))

(defun gnosis-study-suspend ()
  "Explicitly toggle suspension of the thema at point."
  (interactive)
  (let* ((owner (gnosis-study--check-owner))
         (id (or (tabulated-list-get-id) (user-error "No thema at point")))
         (value (if (gnosis-suspended-p id) 0 1)))
    (when (y-or-n-p (if (= value 1) "Suspend thema? " "Unsuspend thema? "))
      (gnosis-study--check-owner owner)
      (gnosis-toggle-suspend-themata (list id) value t)
      (with-current-buffer (car owner)
        (gnosis-study-refresh)))))

(keymap-popup-define gnosis-study-mode-map
  "Topic study"
  :parent tabulated-list-mode-map
  :group "Study"
  "d" ("Review due" gnosis-study-due :if (lambda () gnosis-study--topic))
  "p" ("Practise (no rescheduling)" gnosis-study-practice
       :if (lambda () gnosis-study--topic))
  :group "Author"
  "c" ("Create thema" gnosis-study-create :if (lambda () gnosis-study--topic))
  "RET" ("Inspect/edit thema" gnosis-study-edit)
  "v" ("Visit source" gnosis-study-source)
  :group "Repair"
  "f" ("Toggle needs_work" gnosis-study-toggle-flag)
  "s" ("Toggle suspension" gnosis-study-suspend)
  :group "Session"
  "R" ("Resume batch" gnosis-review-resume)
  "u" ("Undo last grade" gnosis-review-undo)
  :group "Retention"
  "D" ("Desired retention" gnosis-scheduler-set-retention)
  "H" ("History evidence" gnosis-study-history-audit)
  :group "Navigate"
  "o" ("Open topic" gnosis-study-topic)
  "g" ("Refresh" gnosis-study-refresh)
  "q" ("Quit" quit-window)
  "?" ("Help" gnosis-study-help))

(defun gnosis-study-help ()
  "Show topic study commands."
  (interactive)
  (keymap-popup gnosis-study-mode-map))

(defun gnosis-study--retire-collection ()
  "Retire collection identity when its buffer is repurposed."
  (setq gnosis-study--owner nil
        gnosis-study--topic nil
        gnosis-study--repair-p nil)
  ;; File association clears this handler without changing the major mode.
  ;; Keep reverting subject to the retired collection check until mode change.
  (setq-local revert-buffer-function #'tabulated-list-revert))

(define-derived-mode gnosis-study-mode tabulated-list-mode "Gnosis Study"
  "Study existing topic themata and repair weak questions."
  (setq tabulated-list-format [("Question" 48 t) ("Flag" 12 t)
                               ("Status" 12 t) ("Delayed failures / sample" 26 t)]
        tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook #'gnosis-study-refresh nil t)
  (add-hook 'after-set-visited-file-name-hook
            #'gnosis-study--retire-collection nil t)
  (add-hook 'change-major-mode-hook #'gnosis-study--retire-collection nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun gnosis-study-topic (&optional node)
  "Open a native study view for topic NODE, an Org ID."
  (interactive)
  (let* ((candidates (gnosis-study-topic-candidates))
         (node (or node (cdr (assoc (completing-read "Topic: " candidates nil t) candidates)))))
    (unless (gnosis-get 'id 'nodes `(= id ,node)) (user-error "Unknown topic"))
    (pop-to-buffer (generate-new-buffer "*Gnosis Topic*"))
    (gnosis-study-mode)
    (setq gnosis-study--topic node)
    (gnosis-study-refresh)))

;;;###autoload
(defun gnosis-study-repair ()
  "Show manual flags and repeated delayed failures, not cram mistakes."
  (interactive)
  (pop-to-buffer (generate-new-buffer "*Gnosis Repair*"))
  (gnosis-study-mode)
  (setq gnosis-study--repair-p t)
  (gnosis-study-refresh))

(defun gnosis-study-void-practice (correction-id event-id)
  "Void latest practice EVENT-ID using idempotent CORRECTION-ID.
Retain exposure evidence and never call scheduler acceptance or rebuild."
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-with-transaction db
      (let* ((retained (gnosis-get 'event-id 'practice-voids `(= correction-id ,correction-id)))
             (event (car (gnosis-select '* 'practice-events `(= event-id ,event-id)))))
        (cond
         (retained (unless (equal retained event-id) (error "Correction identity conflict")))
         ((null event) (user-error "Practice encounter no longer exists"))
         (t
          (unless (equal event-id
                         (caar (gnosis-sqlite-select
                                db "SELECT event_id FROM practice_events WHERE thema_id = ?
                                     AND event_id NOT IN (SELECT event_id FROM practice_voids)
                                     ORDER BY reviewed_at_us DESC, event_id DESC LIMIT 1"
                                (list (nth 1 event)))))
            (user-error "Only latest effective practice can be undone"))
          (gnosis-sqlite-execute db "INSERT INTO practice_voids VALUES (?, ?)"
                                 (list correction-id event-id))))
        event-id))))

;;;###autoload
(defun gnosis-study-history-audit ()
  "Show the amount and limitations of retained learning evidence.
Do not fit parameters or claim personalized calibration from these counts."
  (interactive)
  (let* ((db (gnosis--ensure-db))
         (queries '("SELECT COALESCE(SUM(reps), 0) FROM scheduler_baseline"
                    "SELECT COUNT(*) FROM review_events WHERE event_id NOT IN (SELECT event_id FROM review_voids)"
                    "SELECT COUNT(*) FROM review_events WHERE elapsed_days > 0 AND event_id NOT IN (SELECT event_id FROM review_voids)"
                    "SELECT COUNT(*) FROM practice_events"
                    "SELECT COUNT(*) FROM review_voids"
                    "SELECT COUNT(*) FROM practice_voids"
                    "SELECT COUNT(*) FROM scheduler_config"))
         (counts (mapcar (lambda (sql) (caar (gnosis-sqlite-select db sql))) queries)))
    (with-current-buffer (generate-new-buffer "*Gnosis History Evidence*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (apply #'format
                       (concat "Aggregate-only baseline reviews: %d\nEffective scheduled events: %d\n"
                               "Delayed scheduled events: %d\nPractice exposures (including voided grades): %d\n"
                               "Voided scheduled grades: %d\nVoided practice grades: %d\nConfig versions: %d\n") counts)
                "\nThese counts do not establish learning effectiveness.\n"
                "Baseline aggregates lack event-level times/outcomes.  Content edits are not versioned.\n"
                "Practice exposures are retained but excluded from FSRS replay and fitting.\n"
                "Before fitting, assess complete chronological histories, model versions,\n"
                "same-day policy, changed questions and prior practice; use held-out future\n"
                "events for calibration/log loss and workload at matched retention.\n"
                "Keep candidate predictions in shadow; no fitted parameters are promoted here.\n"
                "There is no automatic sibling burying: source links do not identify variants.\n")
        (special-mode))
      (pop-to-buffer (current-buffer)))
    counts))

(defvar gnosis-study-day-buffer-name "*Gnosis Study Day*"
  "Name of the read-only study-day view.")

(defvar-local gnosis-study--day nil
  "YYYYMMDD integer displayed by this study-day view.")

(defvar-local gnosis-study--day-owner nil
  "Buffer and database pair identifying this study-day rendering.")

(defun gnosis-study--calendar-day (day)
  "Return DAY when it is a real YYYYMMDD calendar date."
  (unless (and (integerp day) (> day 0))
    (user-error "Study date must be a YYYYMMDD integer"))
  (let* ((date (gnosis--int-to-date day))
         (time (encode-time 0 0 12 (nth 2 date) (nth 1 date) (nth 0 date)))
         (decoded (decode-time time))
         (roundtrip (gnosis--date-to-int
                     (list (decoded-time-year decoded)
                           (decoded-time-month decoded)
                           (decoded-time-day decoded)))))
    (unless (= day roundtrip)
      (user-error "Study date is not a real calendar day"))
    day))

(defun gnosis-study--read-day ()
  "Read a calendar date as a YYYYMMDD integer using `org-read-date'."
  (let* ((raw (org-read-date nil nil nil "Study day"))
         (decoded (parse-time-string raw))
         (year (decoded-time-year decoded))
         (month (decoded-time-month decoded))
         (day (decoded-time-day decoded)))
    (gnosis-study--calendar-day
     (if (and year month day)
         (gnosis--date-to-int (list year month day))
       0))))

(defun gnosis-study--format-day (date)
  "Format YYYYMMDD integer DATE as a calendar date string."
  (apply #'format "%04d-%02d-%02d" (gnosis--int-to-date date)))

(defun gnosis-study--day-scheduled-events (date)
  "Return the number of effective scheduled events recorded on DATE."
  (caar (gnosis-sqlite-select
         (gnosis--ensure-db)
         "SELECT COUNT(*) FROM review_events
           WHERE review_day = ?
             AND event_id NOT IN (SELECT event_id FROM review_voids)"
         (list date))))

(defun gnosis-study--day-source-nodes (date)
  "Return unique source completion pairs for event-level activity on DATE.
Pairs are (LABEL . ID) from `gnosis-study-topic-candidates'.  Ignore
legacy scheduled aggregates, which have no event-level themata."
  (let* ((practice (mapcar #'cadr (gnosis-study--practice-day-events date)))
         (scheduled
          (mapcar #'car
                  (gnosis-sqlite-select
                   (gnosis--ensure-db)
                   "SELECT DISTINCT thema_id FROM review_events
                     WHERE review_day = ?
                       AND event_id NOT IN (SELECT event_id FROM review_voids)"
                   (list date))))
         (ids (delete-dups (delq nil (append practice scheduled))))
         (nodes (when ids
                  (delete-dups
                   (gnosis-select 'dest 'thema-links
                                  `(in source ,(vconcat ids)) t)))))
    (and nodes (gnosis-study-topic-candidates nodes))))

(defun gnosis-study--day-payload (date)
  "Return display data for logical DATE as a plist."
  (list :date date
        :activity (gnosis-study-activity date)
        :scheduled-events (gnosis-study--day-scheduled-events date)
        :sources (gnosis-study--day-source-nodes date)))

(defun gnosis-study-day--buffer ()
  "Return the owned study-day buffer.
Refuse to reuse unrelated or file-visiting buffers with the same name."
  (let ((buffer (get-buffer gnosis-study-day-buffer-name)))
    (when (and buffer
               (not (with-current-buffer buffer
                      (and (not buffer-file-name)
                           (derived-mode-p 'gnosis-study-day-mode)))))
      (user-error "Buffer %s is not a Gnosis study-day view; rename it first"
                  gnosis-study-day-buffer-name))
    (or buffer (generate-new-buffer gnosis-study-day-buffer-name))))

(defun gnosis-study-day--check-owner (&optional owner)
  "Return current study-day OWNER, or signal if its rendering is stale.
An explicit OWNER may be checked after a prompt or source navigation."
  (let* ((owner (or owner gnosis-study--day-owner))
         (buffer (car owner))
         (database (cdr owner)))
    (unless (and (buffer-live-p buffer)
                 (eq database gnosis-db)
                 database
                 (with-current-buffer buffer
                   (and (derived-mode-p 'gnosis-study-day-mode)
                        (not buffer-file-name)
                        (eq owner gnosis-study--day-owner))))
      (user-error "Study-day view is stale; refresh or reopen it"))
    owner))

(defun gnosis-study--insert-day (payload owner)
  "Insert study-day PAYLOAD owned by OWNER into the current buffer."
  (let* ((date (plist-get payload :date))
         (activity (plist-get payload :activity))
         (sources (plist-get payload :sources))
         (scheduled-events (or (plist-get payload :scheduled-events) 0))
         (inhibit-read-only t)
         (point (point)))
    (erase-buffer)
    (insert (propertize "Study day " 'face 'shadow)
            (propertize (gnosis-study--format-day date) 'face 'org-date)
            "\n\nAccepted attempts: "
            (propertize (number-to-string (plist-get activity :total))
                        'face 'success)
            "\nScheduled: "
            (propertize (number-to-string (plist-get activity :scheduled))
                        'face 'font-lock-type-face)
            " (New: "
            (propertize (number-to-string (plist-get activity :new))
                        'face 'font-lock-keyword-face)
            ")\nPractice: "
            (propertize (number-to-string (plist-get activity :practice))
                        'face 'font-lock-type-face)
            "\n\n"
            (propertize
             (format (concat "Practice uses the current timezone and day-start hour (%d).\n"
                             "Scheduled days remain as recorded, including legacy daily aggregates.\n"
                             "Counts are accepted attempts, including retries, not distinct themata.\n")
                     gnosis-day-start-hour)
             'face 'shadow)
            "\n")
    (when (> (plist-get activity :scheduled) scheduled-events)
      (insert (propertize
               "Scheduled totals include legacy aggregates without event-level sources.\n\n"
               'face 'warning)))
    (insert (propertize "Sources\n" 'face 'bold))
    (if sources
        (dolist (pair sources)
          (insert "  ")
          (insert-text-button
           (car pair)
           'face 'link
           'follow-link t
           'action (lambda (button)
                     (gnosis-study-day-visit-source
                      (button-get button 'gnosis-node-id)
                      (button-get button 'gnosis-day-owner)
                      (button-get button 'gnosis-day-date)))
           'gnosis-node-id (cdr pair)
           'gnosis-day-owner owner
           'gnosis-day-date date
           'help-echo (format "Visit source %s" (cdr pair)))
          (insert "\n"))
      (insert (propertize
               "  No indexed source nodes for this day's event-level activity.\n"
               'face 'shadow)))
    (insert "\n"
            (propertize "H opens History for session-level evidence.\n"
                        'face 'shadow))
    (setq header-line-format
          (format " Study day %s | %d attempts"
                  (gnosis-study--format-day date)
                  (plist-get activity :total)))
    (goto-char (min point (point-max)))
    (set-buffer-modified-p nil)))

(defun gnosis-study-day-refresh ()
  "Refresh this study-day view from the current database.
Keep the displayed date and adopt the current connection."
  (interactive)
  (unless (and (derived-mode-p 'gnosis-study-day-mode)
               (not buffer-file-name))
    (user-error "Open a study-day view first"))
  (let* ((date (or gnosis-study--day
                   (user-error "Study-day view is stale; reopen it")))
         (database (gnosis--ensure-db))
         (payload (gnosis-study--day-payload date))
         (owner (cons (current-buffer) database)))
    (setq gnosis-study--day date
          gnosis-study--day-owner owner)
    (gnosis-study--insert-day payload owner)))

(defun gnosis-study-day-visit-source (&optional node owner date)
  "Visit source NODE from this study-day view.
NODE is an Org ID.  OWNER and DATE must be this rendering's identity."
  (interactive)
  (let* ((current (gnosis-study-day--check-owner))
         (node (or node (get-text-property (point) 'gnosis-node-id)
                   (user-error "No source at point")))
         (owner (or owner (get-text-property (point) 'gnosis-day-owner)))
         (date (or date (get-text-property (point) 'gnosis-day-date))))
    (unless (and (eq owner current)
                 (integerp date)
                 (= date gnosis-study--day))
      (user-error "Study-day view is stale; refresh or reopen it"))
    (gnosis-nodes-goto-id node)))

(defun gnosis-study-day-history ()
  "Open recorded study history from this view."
  (interactive)
  (gnosis-study-day--check-owner)
  (require 'gnosis-dashboard)
  (gnosis-dashboard-history))

(defun gnosis-study-day--detach ()
  "Retire study-day ownership when the buffer is repurposed."
  (setq gnosis-study--day-owner nil
        gnosis-study--day nil))

(defvar-keymap gnosis-study-day-mode-map
  :doc "Keymap for the read-only study-day view."
  :parent special-mode-map
  "g" #'gnosis-study-day-refresh
  "H" #'gnosis-study-day-history
  "RET" #'gnosis-study-day-visit-source)

(define-derived-mode gnosis-study-day-mode special-mode "Study Day"
  "Read-only accepted study activity for one logical day.
\\<gnosis-study-day-mode-map>\\[gnosis-study-day-refresh] refreshes;
\\[gnosis-study-day-history] opens History;
\\[quit-window] closes the view."
  :interactive nil
  (setq-local revert-buffer-function
              (lambda (&rest _) (gnosis-study-day-refresh)))
  (add-hook 'change-major-mode-hook #'gnosis-study-day--detach nil t)
  (add-hook 'after-set-visited-file-name-hook #'gnosis-study-day--detach nil t))

;;;###autoload
(defun gnosis-study-day (&optional date)
  "Show a read-only view of accepted study activity for DATE.
DATE is a YYYYMMDD integer and defaults to the current logical day.
Interactively, prompt with `org-read-date' when DATE is omitted.
Validate that DATE is a real calendar day.  Do not write journal prose
or study evidence."
  (interactive (list (gnosis-study--read-day)))
  (let* ((date (gnosis-study--calendar-day (or date (gnosis--today-int))))
         (database (gnosis--ensure-db))
         (payload (gnosis-study--day-payload date))
         (buffer (gnosis-study-day--buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'gnosis-study-day-mode)
        (gnosis-study-day-mode))
      (let ((owner (cons (current-buffer) database)))
        (setq gnosis-study--day date
              gnosis-study--day-owner owner)
        (gnosis-study--insert-day payload owner)))
    (pop-to-buffer buffer)))

(provide 'gnosis-study)
;;; gnosis-study.el ends here
