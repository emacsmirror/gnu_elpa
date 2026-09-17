;;; gnosis-test-study-day.el --- Read-only study-day view tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gnosis-study)
(require 'gnosis-test-helpers)

(defmacro gnosis-test-study-day (&rest body)
  "Run BODY with disposable data and study-day buffers."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-db
     (let ((buffers (buffer-list))
           (gnosis-study-day-buffer-name " *Gnosis Study Day Test*"))
       (save-window-excursion
         (unwind-protect (progn ,@body)
           (dolist (buffer (seq-difference (buffer-list) buffers))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))))))

(defun gnosis-test-study-day-snapshot ()
  "Return retained study and source rows."
  (mapcar (lambda (table) (gnosis-select '* table))
          '(scheduler-config scheduler-active scheduler-baseline scheduler-state
            review-events review-voids review-activity-baseline
            practice-events practice-voids study-history themata extras
            thema-links nodes)))

(defun gnosis-test-study-day-event (id session attempt time &optional failure)
  "Accept practice ID in SESSION at ATTEMPT and TIME."
  (gnosis-study-accept-practice
   (list :mode 'practice :thema-id id :session-id session
         :attempt attempt :event-id (gnosis-scheduler-event-id)
         :reviewed-at-us time :outcome (if failure 'failure 'success))))

(defun gnosis-test-study-day-node (id &optional title)
  "Insert node ID with TITLE into the disposable index."
  (gnosis--insert-into 'nodes
                       `([,id "fixture.org" ,(or title "Topic") "1" nil nil nil])))

(defun gnosis-test-study-day-text ()
  "Return the current study-day buffer text without properties."
  (with-current-buffer gnosis-study-day-buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest gnosis-study-day-read-only-preserves-evidence ()
  "Public view is a read-only special-mode projection of accepted attempts."
  (gnosis-test-study-day
    (let* ((id (gnosis-test--add-basic-thema "Q" "A"))
           (today (gnosis--today-int))
           (now (car (time-convert nil 1000000))))
      (gnosis-scheduler-accept-review
       (gnosis-scheduler-event-id) id 'success now today)
      (gnosis-test-study-day-event id "retries" 1 now t)
      (gnosis-test-study-day-event id "retries" 2 (1+ now))
      (let ((before (gnosis-test-study-day-snapshot)))
        (gnosis-study-day today)
        (with-current-buffer gnosis-study-day-buffer-name
          (should (derived-mode-p 'gnosis-study-day-mode))
          (should (derived-mode-p 'special-mode))
          (should buffer-read-only)
          (should-error (insert "x"))
          (should (eq (key-binding (kbd "q")) #'quit-window))
          (should (eq (key-binding (kbd "g")) #'gnosis-study-day-refresh))
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-search "Accepted attempts: 3" text))
            (should (string-search "Scheduled: 1 (New: 1)" text))
            (should (string-search "Practice: 2" text))
            (should (string-search "day-start hour" text))
            (should (string-search "legacy daily aggregates" text))))
        (should (equal before (gnosis-test-study-day-snapshot)))))))

(ert-deftest gnosis-study-day-legacy-aggregate-without-fabricated-sources ()
  "Show legacy scheduled totals without inventing event-level source links."
  (gnosis-test-study-day
    (let ((today (gnosis--today-int)))
      (gnosis-sqlite-execute
       gnosis-db "INSERT INTO review_activity_baseline VALUES (?, 10, 2)"
       (list today))
      (gnosis-study-day today)
      (let ((text (gnosis-test-study-day-text)))
        (should (string-search "Accepted attempts: 10" text))
        (should (string-search "Scheduled: 10 (New: 2)" text))
        (should (string-search "Practice: 0" text))
        (should (string-search "legacy aggregates without event-level sources" text))
        (should (string-search "No indexed source nodes" text)))
      (with-current-buffer gnosis-study-day-buffer-name
        (goto-char (point-min))
        (should-not (next-button (point)))))))

(ert-deftest gnosis-study-day-practice-cutoff-and-voids ()
  "Project practice with the encounter cutoff and exclude voided attempts."
  (gnosis-test-study-day
    (let ((old-zone (getenv "TZ"))
          (gnosis-day-start-hour 3)
          (id (gnosis-test--add-basic-thema "Q" "A")))
      (unwind-protect
          (progn
            (set-time-zone-rule "Europe/Athens")
            (cl-loop for stamp in '("2026-10-24T23:59:59Z" "2026-10-25T00:00:00Z"
                                    "2026-10-25T00:30:00Z" "2026-10-25T01:30:00Z")
                     for attempt from 1 do
                     (gnosis-test-study-day-event
                      id "calendar" attempt
                      (car (time-convert (date-to-time stamp) 1000000))))
            (gnosis-study-day 20261025)
            (should (string-search "Practice: 3" (gnosis-test-study-day-text)))
            (should (string-search "Accepted attempts: 3" (gnosis-test-study-day-text)))
            (let ((event (car (gnosis-select 'event-id 'practice-events
                                             `(= attempt 4) t))))
              (gnosis-study-void-practice "correction" event))
            (with-current-buffer gnosis-study-day-buffer-name
              (call-interactively (key-binding (kbd "g"))))
            (should (string-search "Practice: 2" (gnosis-test-study-day-text)))
            (should (equal '(:total 2 :scheduled 0 :practice 2 :new 0)
                           (gnosis-study-activity 20261025))))
        (set-time-zone-rule old-zone)))))

(ert-deftest gnosis-study-day-empty-day ()
  "Render an empty logical day without inventing activity or sources."
  (gnosis-test-study-day
    (gnosis-study-day 19990101)
    (let ((text (gnosis-test-study-day-text)))
      (should (string-search "1999-01-01" text))
      (should (string-search "Accepted attempts: 0" text))
      (should (string-search "Scheduled: 0 (New: 0)" text))
      (should (string-search "Practice: 0" text))
      (should (string-search "No indexed source nodes" text)))))

(ert-deftest gnosis-study-day-invalid-date-preserves-journal ()
  "Reject unreal dates before touching journal contents or the named view."
  (gnosis-test-study-day
    (with-temp-buffer
      (insert "* 2026-02-31\nJournal draft")
      (let ((journal (current-buffer))
            (before (buffer-string)))
        (dolist (date '(20260231 20261301 0 -1 "2026-10-25"))
          (should-error (gnosis-study-day date) :type 'user-error)
          (should (eq journal (current-buffer)))
          (should (equal before (buffer-string)))
          (should-not (get-buffer gnosis-study-day-buffer-name)))
        (cl-letf (((symbol-function 'org-read-date)
                   (lambda (&rest _) "2026-02-31")))
          (should-error (call-interactively #'gnosis-study-day)
                        :type 'user-error))
        (should (eq journal (current-buffer)))
        (should (equal before (buffer-string)))
        (should-not (get-buffer gnosis-study-day-buffer-name))))))

(ert-deftest gnosis-study-day-buffer-ownership-and-refresh ()
  "Refuse unrelated same-name buffers; refresh adopts the current database."
  (gnosis-test-study-day
    (let* ((id (gnosis-test--add-basic-thema "Q" "A"))
           (today (gnosis--today-int))
           (now (car (time-convert nil 1000000)))
           (original-db gnosis-db)
           (original-dir gnosis-dir))
      (gnosis-test-study-day-event id "owned" 1 now)
      (let ((foreign (generate-new-buffer " *gnosis-day-draft*")))
        (unwind-protect
            (progn
              (with-current-buffer foreign
                (rename-buffer gnosis-study-day-buffer-name)
                (insert "Unrelated journal"))
              (let ((before (with-current-buffer foreign
                              (list (buffer-string) major-mode
                                    (buffer-modified-p) buffer-file-name))))
                (should-error (gnosis-study-day today) :type 'user-error)
                (should (equal before
                               (with-current-buffer foreign
                                 (list (buffer-string) major-mode
                                       (buffer-modified-p) buffer-file-name))))))
          (when (buffer-live-p foreign)
            (with-current-buffer foreign (set-buffer-modified-p nil))
            (kill-buffer foreign))))
      (gnosis-study-day today)
      (let ((view (get-buffer gnosis-study-day-buffer-name)))
        (should (string-search "Practice: 1" (gnosis-test-study-day-text)))
        (gnosis-study-day today)
        (should (eq view (get-buffer gnosis-study-day-buffer-name)))
        (with-current-buffer view
          (let ((before (buffer-string))
                (owner gnosis-study--day-owner)
                (date gnosis-study--day))
            (cl-letf (((symbol-function 'gnosis-review-activity)
                       (lambda (&rest _) (error "query failed"))))
              (should-error (call-interactively (key-binding (kbd "g"))))
              (should-error (gnosis-study-day today)))
            (should (equal before (buffer-string)))
            (should (eq owner gnosis-study--day-owner))
            (should (eq date gnosis-study--day))
            (should buffer-read-only)))
        (gnosis-test-with-db
          (with-current-buffer view
            (call-interactively (key-binding (kbd "g")))
            (should (eq gnosis-study--day today))
            (should (eq (cdr gnosis-study--day-owner) gnosis-db))
            (should (string-search "Practice: 0"
                                   (buffer-substring-no-properties
                                    (point-min) (point-max))))))
        (setq gnosis-db original-db gnosis-dir original-dir)
        (with-current-buffer view
          (setq buffer-file-name (expand-file-name "journal.org" gnosis-dir))
          (let ((before (buffer-substring-no-properties (point-min) (point-max))))
            (should-error (call-interactively (key-binding (kbd "g")))
                          :type 'user-error)
            (should-error (gnosis-study-day today) :type 'user-error)
            (should (equal before (buffer-substring-no-properties
                                   (point-min) (point-max))))))))))

(ert-deftest gnosis-study-day-source-buttons-are-owner-qualified ()
  "Retained source buttons cannot follow a later date or database."
  (gnosis-test-study-day
    (let* ((id (gnosis-test--add-basic-thema "Q" "A"))
           (today (gnosis--today-int))
           (now (car (time-convert nil 1000000)))
           (visited nil))
      (gnosis-test-study-day-node "topic-id" "Cardiology")
      (gnosis--insert-into 'thema-links `([,id "topic-id"]))
      (gnosis-test-study-day-event id "linked" 1 now)
      (gnosis-study-day today)
      (with-current-buffer gnosis-study-day-buffer-name
        (should (string-search "Cardiology" (buffer-string)))
        (goto-char (point-min))
        (let* ((button (next-button (point)))
               (action (button-get button 'action))
               (old-owner (button-get button 'gnosis-day-owner)))
          (should button)
          (should (equal "topic-id" (button-get button 'gnosis-node-id)))
          (should (eq old-owner gnosis-study--day-owner))
          (should (= today (button-get button 'gnosis-day-date)))
          (cl-letf (((symbol-function 'gnosis-nodes-goto-id)
                     (lambda (node) (setq visited node))))
            (funcall action button)
            (should (equal "topic-id" visited))
            (setq visited nil)
            (gnosis-study-day 19990101)
            (should (eq gnosis-study--day 19990101))
            (should-not (eq old-owner gnosis-study--day-owner))
            (should-error (funcall action button) :type 'user-error)
            (should-not visited)
            (gnosis-study-day today)
            (gnosis-test-with-db
              (with-current-buffer gnosis-study-day-buffer-name
                (call-interactively (key-binding (kbd "g")))
                (should-error (funcall action button) :type 'user-error)
                (should-not visited)))
            (text-mode)
            (should-not gnosis-study--day-owner)
            (should-error (gnosis-study-day-visit-source
                           "topic-id" old-owner today)
                          :type 'user-error)
            (should-not visited)))))))

(ert-deftest gnosis-study-day-interactive-org-read-date ()
  "Prompt with org-read-date when DATE is omitted interactively."
  (gnosis-test-study-day
    (cl-letf (((symbol-function 'org-read-date)
               (lambda (&rest _) "2026-10-25")))
      (call-interactively #'gnosis-study-day))
    (with-current-buffer gnosis-study-day-buffer-name
      (should (eq gnosis-study--day 20261025))
      (should (string-search "2026-10-25" (buffer-string))))))

(defun gnosis-test-study-day-seed ()
  "Accept scheduled and practice attempts; return their logical day."
  (let ((id (gnosis-test--add-basic-thema "Ownership Q" "A"))
        (today (gnosis--today-int))
        (now (car (time-convert nil 1000000))))
    (gnosis-scheduler-accept-review
     (gnosis-scheduler-event-id) id 'success now today)
    (gnosis-test-study-day-event id "ownership" 1 now)
    (should (gnosis-select '* 'review-events))
    (should (gnosis-select '* 'practice-events))
    today))

(defun gnosis-test-study-day-buffer-state (buffer)
  "Return BUFFER's draft and view state, including text properties."
  (with-current-buffer buffer
    (list (buffer-string) (buffer-modified-p) buffer-file-name major-mode
          buffer-read-only header-line-format gnosis-study--day
          gnosis-study--day-owner)))

(ert-deftest gnosis-study-day-retired-name-reuse-preserves-draft ()
  "Native file association retires the view even after detachment and rename."
  (dolist (detach '(nil t))
    (gnosis-test-study-day
      (let* ((today (gnosis-test-study-day-seed))
             (evidence (gnosis-test-study-day-snapshot))
             (file (expand-file-name "successor.org" gnosis-dir)))
        (with-temp-file file (insert "Saved successor\n"))
        (gnosis-study-day today)
        (let ((view (current-buffer)))
          (set-visited-file-name file t)
          (when detach (set-visited-file-name nil t))
          (rename-buffer gnosis-study-day-buffer-name)
          (read-only-mode -1)
          (erase-buffer)
          (insert "Unsaved successor λ\n")
          (should (derived-mode-p 'gnosis-study-day-mode))
          (should-not gnosis-study--day-owner)
          (let ((before (gnosis-test-study-day-buffer-state view)))
            (should-error (call-interactively (key-binding (kbd "g")))
                          :type 'user-error)
            (should-error (gnosis-study-day today) :type 'user-error)
            (should (equal before (gnosis-test-study-day-buffer-state view))))
          ;; Giving the successor a distinct name allows a genuinely new view.
          (rename-buffer " *Retired study-day draft*")
          (let ((before (gnosis-test-study-day-buffer-state view)))
            (gnosis-study-day today)
            (should-not (eq view (current-buffer)))
            (should (string-search "Accepted attempts: 2" (buffer-string)))
            (should (equal before (gnosis-test-study-day-buffer-state view))))
          (should (equal "Saved successor\n"
                         (with-temp-buffer
                           (insert-file-contents file) (buffer-string))))
          (should (equal evidence (gnosis-test-study-day-snapshot))))))))

(ert-deftest gnosis-study-day-setup-hooks-preserve-successor ()
  "Native mode/file round trips cannot revive an interrupted setup."
  (dolist (transition '(file file-roundtrip mode mode-roundtrip setup))
    (gnosis-test-study-day
      (let* ((today (gnosis-test-study-day-seed))
             (evidence (gnosis-test-study-day-snapshot))
             (file (expand-file-name "hook-successor.org" gnosis-dir))
             view before
             (gnosis-study-day-mode-hook
              (list
               (lambda ()
                 (setq view (current-buffer))
                 (pcase transition
                   ((or 'file 'file-roundtrip)
                    (set-visited-file-name file t)
                    (when (eq transition 'file-roundtrip)
                      (set-visited-file-name nil t)))
                   ((or 'mode 'mode-roundtrip)
                    (text-mode)
                    (when (eq transition 'mode-roundtrip)
                      (let ((gnosis-study-day-mode-hook nil))
                        (gnosis-study-day-mode))))
                   ('setup (gnosis-study-day 19990101)))
                 (unless (eq transition 'setup)
                   (read-only-mode -1)
                   (erase-buffer)
                   (insert "Hook successor draft λ\n"))
                 (setq before (gnosis-test-study-day-buffer-state view))))))
        (with-temp-file file (insert "Saved hook successor\n"))
        (should-error (gnosis-study-day today) :type 'user-error)
        (should (buffer-live-p view))
        (should (equal before (gnosis-test-study-day-buffer-state view)))
        (should (equal "Saved hook successor\n"
                       (with-temp-buffer
                         (insert-file-contents file) (buffer-string))))
        (should (equal evidence (gnosis-test-study-day-snapshot)))))))

(ert-deftest gnosis-study-day-owned-reopen-date-rename-and-refresh ()
  "Keep benign hooks, date changes and explicit database adoption usable."
  (gnosis-test-study-day
    (let* ((today (gnosis-test-study-day-seed))
           (evidence (gnosis-test-study-day-snapshot))
           (calls 0)
           (gnosis-study-day-mode-hook
            (list (lambda () (cl-incf calls) (setq-local truncate-lines t)))))
      (gnosis-study-day today)
      (let ((view (current-buffer)))
        (should (= calls 1))
        (should truncate-lines)
        (should buffer-read-only)
        (should-not (buffer-modified-p))
        (gnosis-study-day 19990101)
        (should (eq view (current-buffer)))
        (should (string-search "Accepted attempts: 0" (buffer-string)))
        (gnosis-study-day today)
        (should (eq view (current-buffer)))
        (should (= calls 1))
        (rename-buffer " *Renamed owned day*")
        (let ((replacement (generate-new-buffer gnosis-study-day-buffer-name)))
          (with-current-buffer replacement (insert "Unrelated named draft"))
          (let ((before (gnosis-test-study-day-buffer-state replacement)))
            (call-interactively (key-binding (kbd "g")))
            (should (string-search "Accepted attempts: 2" (buffer-string)))
            (gnosis-test-with-db
              (gnosis-test-study-day-seed)
              ;; Distinguish the two nonempty databases' projected counts.
              (gnosis-sqlite-execute
               gnosis-db "INSERT INTO review_activity_baseline VALUES (?, 4, 0)"
               (list today))
              (let ((foreign-evidence (gnosis-test-study-day-snapshot)))
                (should-error (gnosis-study-day--check-owner) :type 'user-error)
                (call-interactively (key-binding (kbd "g")))
                (should (eq gnosis-db (cdr gnosis-study--day-owner)))
                (should (string-search "Accepted attempts: 6" (buffer-string)))
                (should (equal foreign-evidence (gnosis-test-study-day-snapshot)))))
            (should (equal before
                           (gnosis-test-study-day-buffer-state replacement))))
          (kill-buffer replacement))
        (rename-buffer gnosis-study-day-buffer-name)
        (gnosis-study-day today)
        (should (eq view (current-buffer)))
        (should (eq gnosis-db (cdr gnosis-study--day-owner)))
        (should (string-search "Accepted attempts: 2" (buffer-string)))
        (should (equal evidence (gnosis-test-study-day-snapshot)))))))

(ert-deftest gnosis-study-day-mode-hook-buffer-switch ()
  "A hook selecting another buffer cannot redirect the original render."
  (gnosis-test-study-day
    (let* ((today (gnosis-test-study-day-seed))
           (evidence (gnosis-test-study-day-snapshot))
           (draft (generate-new-buffer " *Study-day hook destination*"))
           view
           (gnosis-study-day-mode-hook
            (list (lambda ()
                    (setq view (current-buffer))
                    (switch-to-buffer draft)))))
      (with-current-buffer draft (insert "Unrelated hook destination λ\n"))
      (let ((before (gnosis-test-study-day-buffer-state draft)))
        (gnosis-study-day today)
        (should (eq view (current-buffer)))
        (should (string-search "Accepted attempts: 2" (buffer-string)))
        (should (equal before (gnosis-test-study-day-buffer-state draft)))
        (should (equal evidence (gnosis-test-study-day-snapshot)))))))

(ert-deftest gnosis-study-day-single-read-exact-projection ()
  "Reuse practice rows without changing counts, sources, cutoffs or voids."
  (gnosis-test-study-day
    (let ((old-zone (getenv "TZ"))
          (gnosis-day-start-hour 3))
      (unwind-protect
          (progn
            (set-time-zone-rule "Europe/Athens")
            (dolist (id '(101 102 103 104))
              (gnosis-test--add-basic-thema "Q" "A" nil nil id)
              (gnosis-test-study-day-node (number-to-string id)
                                          (format "Topic %d" id))
              (gnosis--insert-into 'thema-links `([,id ,(number-to-string id)])))
            ;; A shared source must appear once despite retries and both modes.
            (gnosis--insert-into 'thema-links '([102 "101"]))
            (gnosis-sqlite-execute
             gnosis-db "INSERT INTO review_activity_baseline VALUES (20261025, 10, 2)")
            (gnosis-scheduler-accept-review
             (gnosis-scheduler-event-id) 101 'success 1000000 20261025)
            (let ((event (gnosis-scheduler-event-id)))
              (gnosis-scheduler-accept-review event 104 'success 2000000 20261025)
              (gnosis-scheduler-void-review (gnosis-scheduler-event-id) event))
            ;; Before cutoff, both repeated 03:30s, then an event to void.
            (cl-loop for (id stamp) in
                     '((103 "2026-10-24T23:59:59Z")
                       (102 "2026-10-25T00:30:00Z")
                       (102 "2026-10-25T01:30:00Z")
                       (104 "2026-10-25T02:00:00Z"))
                     for attempt from 1 do
                     (gnosis-test-study-day-event
                      id "calendar-projection" attempt
                      (car (time-convert (date-to-time stamp) 1000000))))
            (gnosis-study-void-practice
             "practice-correction" (gnosis-get 'event-id 'practice-events '(= attempt 4)))
            (let ((evidence (gnosis-test-study-day-snapshot))
                  (select (symbol-function 'sqlite-select))
                  (insert-day (symbol-function 'gnosis-study--insert-day))
                  (reads 0) observed)
              (cl-letf (((symbol-function 'sqlite-select)
                         (lambda (db sql &rest args)
                           (when (string-search "FROM practice_events" sql)
                             (cl-incf reads))
                           (apply select db sql args)))
                        ((symbol-function 'gnosis-study--insert-day)
                         (lambda (payload owner)
                           (setq observed payload)
                           (funcall insert-day payload owner))))
                (gnosis-study-day 20261025)
                (should (equal '(:date 20261025
                                       :activity (:total 13 :scheduled 11 :practice 2 :new 3)
                                       :scheduled-events 1
                                       :sources (("Topic 101" . "101") ("Topic 102" . "102")))
                               observed))
                (message "Study day public open practice reads: %d" reads)
                (should (= 1 reads))
                (setq reads 0)
                (call-interactively (key-binding (kbd "g")))
                (should (= 1 reads))
                (setq reads 0)
                (should (equal (plist-get observed :activity) (gnosis-study-activity 20261025)))
                (should (= 1 reads))
                ;; Empty rows are supplied, not interpreted as 'fetch again'.
                (setq reads 0)
                (gnosis-study-day 19990101)
                (should (= 1 reads))
                (should (equal '(:date 19990101 :activity (:total 0 :scheduled 0 :practice 0 :new 0)
                                       :scheduled-events 0 :sources nil)
                               observed)))
              (should (equal evidence (gnosis-test-study-day-snapshot)))))
        (set-time-zone-rule old-zone)))))

(provide 'gnosis-test-study-day)
;;; gnosis-test-study-day.el ends here
