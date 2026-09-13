;;; gnosis-test-study-activity.el --- Accepted study activity tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gnosis-study)
(require 'gnosis-dashboard)
(require 'gnosis-agent)
(require 'gnosis-test-helpers)

(defmacro gnosis-test-activity (&rest body)
  "Run BODY with disposable data, buffers and inert agent launch timers."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-db
     (let ((buffers (buffer-list))
           (gnosis-agent--launches nil)
           (gnosis-review--running nil)
           (gnosis-review-buffer-name "*Gnosis Activity Test*")
           (gnosis-review-basic-input 'typed)
           (gnosis-monkeytype-enable nil)
           (gnosis-center-content nil)
           (register-alist nil))
       (save-window-excursion
         (unwind-protect
             (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _) nil)))
               ,@body)
           (mapc #'gnosis-agent--release (copy-sequence gnosis-agent--launches))
           (dolist (buffer (seq-difference (buffer-list) buffers))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))))))

(defun gnosis-test-activity-snapshot ()
  "Return all scheduler authorities and scheduled evidence."
  (mapcar (lambda (table) (gnosis-select '* table))
          '(scheduler-config scheduler-active scheduler-baseline scheduler-state
            review-events review-voids review-activity-baseline)))

(defun gnosis-test-activity-grade (success)
  "Accept SUCCESS for the current queued thema, returning its result."
  (let* ((gnosis-review--state (gnosis-review--read-session))
         (id (car (gnosis-review-state-remaining gnosis-review--state)))
         (result (gnosis-review-algorithm id success)))
    (gnosis-review-result id success result)
    result))

(defun gnosis-test-activity-event (id session attempt time &optional failure)
  "Accept practice ID in SESSION at ATTEMPT and TIME, optionally as FAILURE."
  (let ((result (list :mode 'practice :thema-id id :session-id session
                      :attempt attempt :event-id (gnosis-scheduler-event-id)
                      :reviewed-at-us time :outcome (if failure 'failure 'success))))
    (gnosis-study-accept-practice result)
    result))

(ert-deftest gnosis-study-activity-dashboard-includes-native-practice ()
  "Count native accepted attempts without pretending retries are unique cards."
  (gnosis-test-activity
    (let* ((id (gnosis-test--add-basic-thema "Q" "A"))
           (answers '("wrong" "A")))
      (gnosis-scheduler-accept-review
       (gnosis-scheduler-event-id) id 'success
       (car (time-convert nil 1000000)) (gnosis--today-int))
      (let ((before (gnosis-test-activity-snapshot)))
        (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                   (lambda (&rest _) (pop answers)))
                  ((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
          (gnosis-review-loop (list id) 'practice))
        (with-temp-buffer
          (funcall gnosis-dashboard-module-today-stats)
          (should (string-search "Studied today: 3 attempts" (buffer-string)))
          (should (string-search "Scheduled: 1 (New: 1)" (buffer-string)))
          (should (string-search "Practice: 2" (buffer-string))))
        (should (equal before (gnosis-test-activity-snapshot)))))))

(ert-deftest gnosis-study-activity-agent-only-day-history-keys-and-evidence ()
  "Expose agent-launched native practice even without scheduled reviews."
  (gnosis-test-activity
    (let* ((id (gnosis-test--add-basic-thema "Q" "A"))
           (before (gnosis-test-activity-snapshot))
           (status (gnosis-agent-start-practice :thema-ids (list id) :limit 1))
           (session (plist-get status :session-id))
           (answers '("wrong" "A" "A")))
      (should-not (gnosis-study-practice-history))
      (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                 (lambda (&rest _) (pop answers)))
                ((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
        (gnosis-agent--launch (car gnosis-agent--launches)))
      (should-not answers)
      (should (equal '(:total 3 :scheduled 0 :practice 3 :new 0)
                     (gnosis-study-activity)))
      (with-temp-buffer
        (funcall gnosis-dashboard-module-today-stats)
        (should (string-search "Studied today: 3 attempts" (buffer-string)))
        (should (string-search "Scheduled: 0 (New: 0) · Practice: 3" (buffer-string))))
      (with-temp-buffer
        (gnosis-dashboard-mode)
        (call-interactively (key-binding (kbd "H"))))
      (with-current-buffer "*Gnosis History*"
        (should (derived-mode-p 'gnosis-dashboard-history-mode))
        (should (= 1 (length tabulated-list-entries)))
        (should (equal (cons 'practice session) (tabulated-list-get-id)))
        (should (string-search "Practice session" (buffer-string)))
        (should (string-search "Completed · 1 distinct · 0 voided" (buffer-string)))
        (call-interactively (key-binding (kbd "RET"))))
      (with-current-buffer "*Gnosis Practice Evidence*"
        (should (derived-mode-p 'help-mode))
        (should buffer-read-only)
        (should (string-search session (buffer-string)))
        (dolist (row (gnosis-study-practice-events session))
          (should (string-search (car row) (buffer-string))))
        (should (string-search "Failure" (buffer-string)))
        (should (string-search "Success" (buffer-string))))
      (should (equal before (gnosis-test-activity-snapshot))))))

(ert-deftest gnosis-study-activity-resume-replacement-undo-and-refresh ()
  "Retain exact sessions through retries, replacement, undo and database reopen."
  (gnosis-test-activity
    (let* ((id (gnosis-test--add-basic-thema "Q" "A"))
           (first (plist-get (gnosis-agent-start-practice :thema-ids (list id) :limit 1)
                             :session-id))
           (result (gnosis-test-activity-grade nil)))
      ;; Pending results and a repeated delivery do not manufacture attempts.
      (gnosis-study-accept-practice result)
      (gnosis-agent-resume first)
      (should (= 1 (plist-get (gnosis-study-activity) :total)))
      (should (equal "Unfinished" (plist-get (car (gnosis-study-practice-history)) :status)))
      (let ((second (plist-get (gnosis-agent-start-practice :thema-ids (list id) :limit 1)
                               :session-id)))
        (should (equal "Ended early" (plist-get (car (gnosis-study-practice-history)) :status)))
        (should (= 1 (length (gnosis-study-practice-history))))
        (gnosis-test-activity-grade t)
        (gnosis-dashboard-history)
        (gnosis-dashboard--goto-id (cons 'practice second))
        (should (equal "1" (aref (tabulated-list-get-entry) 1)))
        (gnosis-review-undo)
        (with-current-buffer "*Gnosis History*"
          (call-interactively (key-binding (kbd "g")))
          (should (equal (cons 'practice second) (tabulated-list-get-id)))
          (should (equal "0" (aref (tabulated-list-get-entry) 1)))
          (should (string-search "Unfinished · 0 distinct · 1 voided" (buffer-string)))
          (call-interactively (key-binding (kbd "RET"))))
        (with-current-buffer "*Gnosis Practice Evidence*"
          (should (string-search "(voided)" (buffer-string)))
          (should (string-search "Correction:" (buffer-string))))
        (should (= 1 (plist-get (gnosis-study-activity) :total)))
        (gnosis-test-activity-grade t)
        (should (= 2 (plist-get (gnosis-study-activity) :total)))
        (let ((history (gnosis-study-practice-history)))
          (gnosis-sqlite-close gnosis-db)
          (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (gnosis-db-init)
          (should (equal history (gnosis-study-practice-history)))
          (with-current-buffer "*Gnosis History*"
            (should-error (call-interactively (key-binding (kbd "RET"))) :type 'user-error)
            (call-interactively (key-binding (kbd "g")))
            (should (equal "1" (aref (tabulated-list-get-entry) 1)))
            (should (string-search "Completed · 1 distinct · 1 voided" (buffer-string)))))
        (should (= 2 (length (gnosis-study-practice-history))))))))

(ert-deftest gnosis-study-activity-empty-pending-and-failed-acceptance ()
  "Do not count selection, cancelled input, provisional results or rollback."
  (gnosis-test-activity
    (let* ((id (gnosis-test--add-basic-thema "Q" "A"))
           (session (plist-get (gnosis-agent-start-practice :thema-ids (list id) :limit 1)
                               :session-id))
           (before (gnosis-test-activity-snapshot)))
      (gnosis-agent-cancel session)
      (gnosis-agent-start-practice :thema-ids '(9999999) :limit 1)
      (should-not (gnosis-study-practice-history))
      (dolist (condition '(error quit))
        (condition-case caught
            (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                       (lambda (&rest _) (signal condition '("Input unavailable")))))
              (gnosis-review-loop (list id) 'practice))
          ((error quit) (should (eq (car caught) condition)))))
      (let* ((gnosis-review--state (gnosis-review--read-session))
             (pending (gnosis-review-algorithm id nil)))
        (should pending)
        (should (= 0 (plist-get (gnosis-study-activity) :total)))
        (cl-letf (((symbol-function 'gnosis-review--save-history)
                   (lambda (_) (error "History write failed"))))
          (should-error (gnosis-review-result id nil pending))))
      (should-not (gnosis-study-practice-history))
      (should-not (gnosis-select '* 'practice-events))
      (should (= 0 (plist-get (gnosis-study-activity) :total)))
      (should (equal before (gnosis-test-activity-snapshot))))))

(ert-deftest gnosis-study-activity-retained-custom-evidence-and-voids ()
  "Project historical/custom evidence without snapshots, backfill or double count."
  (gnosis-test-activity
    (let* ((id (gnosis-test--add-basic-thema "Q" "A"))
           (now (car (time-convert nil 1000000)))
           (first (gnosis-test-activity-event id "retained-custom" 1 now t))
           (second (gnosis-test-activity-event id "retained-custom" 2 (1+ now))))
      (gnosis-study-accept-practice first)
      (should (= 2 (plist-get (gnosis-study-activity) :practice)))
      (should-error (gnosis-study-accept-practice
                     (plist-put (copy-sequence first) :outcome 'success)))
      (gnosis-study-void-practice "correction" (plist-get second :event-id))
      (should (= 1 (plist-get (gnosis-study-activity) :practice)))
      (let* ((before (mapcar (lambda (table) (gnosis-select '* table))
                             '(practice-events practice-voids study-history)))
             (row (car (gnosis-study-practice-history))))
        (should (equal "Recorded" (plist-get row :status)))
        (should (= 1 (plist-get row :attempts)))
        (should (= 1 (plist-get row :unique)))
        (should (= 1 (plist-get row :voids)))
        (gnosis-dashboard-history)
        (should (equal (cons 'practice "retained-custom") (tabulated-list-get-id)))
        (should (equal before (mapcar (lambda (table) (gnosis-select '* table))
                                     '(practice-events practice-voids study-history))))))))

(ert-deftest gnosis-study-activity-logical-days-timezones-and-dst ()
  "Use encounter day rules across cutoff, DST gaps/folds and timezone changes."
  (gnosis-test-activity
    (let ((old-zone (getenv "TZ"))
          (gnosis-day-start-hour 3)
          (id (gnosis-test--add-basic-thema "Q" "A")))
      (unwind-protect
          (progn
            (set-time-zone-rule "Europe/Athens")
            ;; Explicit UTC instants cover both occurrences of 03:30 on fall-back.
            (cl-loop for stamp in '("2026-10-24T23:59:59Z" "2026-10-25T00:00:00Z"
                                    "2026-10-25T00:30:00Z" "2026-10-25T01:30:00Z"
                                    "2026-10-26T00:59:59Z" "2026-10-26T01:00:00Z"
                                    "2026-03-29T00:59:59Z" "2026-03-29T01:00:00Z")
                     for attempt from 1 do
                     (gnosis-test-activity-event
                      id "calendar" attempt
                      (car (time-convert (date-to-time stamp) 1000000))))
            (should (= 1 (plist-get (gnosis-study-activity 20261024) :practice)))
            (should (= 4 (plist-get (gnosis-study-activity 20261025) :practice)))
            (should (= 1 (plist-get (gnosis-study-activity 20261026) :practice)))
            (should (= 1 (plist-get (gnosis-study-activity 20260328) :practice)))
            (should (= 1 (plist-get (gnosis-study-activity 20260329) :practice)))
            ;; Stored scheduled days must not be reinterpreted like practice.
            (gnosis-scheduler-accept-review
             (gnosis-scheduler-event-id) id 'success 1000000 20261025)
            (set-time-zone-rule "UTC0")
            (should (= 2 (plist-get (gnosis-study-activity 20261025) :practice)))
            (should (= 1 (plist-get (gnosis-study-activity 20261025) :scheduled)))
            (let ((gnosis-day-start-hour 0))
              (should (= 3 (plist-get (gnosis-study-activity 20261025) :practice)))))
        (set-time-zone-rule old-zone)))))

(ert-deftest gnosis-study-activity-scheduled-baseline-undo-and-deletion ()
  "Keep scheduled aggregate authority and remove only deleted practice evidence."
  (gnosis-test-activity
    (let* ((a (gnosis-test--add-basic-thema "A" "a"))
           (b (gnosis-test--add-basic-thema "B" "b"))
           (today (gnosis--today-int))
           (now (car (time-convert nil 1000000)))
           (event (gnosis-scheduler-event-id)))
      (gnosis-sqlite-execute gnosis-db "INSERT INTO review_activity_baseline VALUES (?, 10, 2)"
                             (list today))
      (gnosis-scheduler-accept-review event a 'failure now today)
      (gnosis-test-activity-event a "custom-a" 1 now t)
      (gnosis-test-activity-event b "custom-b" 1 now)
      (should (equal '(:total 13 :scheduled 11 :practice 2 :new 3)
                     (gnosis-study-activity)))
      (gnosis-scheduler-void-review (gnosis-scheduler-event-id) event)
      (should (equal '(:total 12 :scheduled 10 :practice 2 :new 2)
                     (gnosis-study-activity)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (gnosis-delete-themata (list a)))
      (should (equal '(:total 11 :scheduled 10 :practice 1 :new 2)
                     (gnosis-study-activity)))
      (gnosis-dashboard-history)
      (should (= 2 (length tabulated-list-entries)))
      (should (assoc today tabulated-list-entries))
      (should (assoc '(practice . "custom-b") tabulated-list-entries))
      (should-not (assoc '(practice . "custom-a") tabulated-list-entries)))))

(ert-deftest gnosis-study-activity-history-refuses-file-association ()
  "Do not erase a history buffer repurposed to visit a user file."
  (gnosis-test-activity
    (let ((id (gnosis-test--add-basic-thema "Q" "A")))
      (gnosis-test-activity-event id "custom" 1 (car (time-convert nil 1000000)))
      (gnosis-dashboard-history)
      (let ((before (buffer-string)))
        (setq buffer-file-name (expand-file-name "unrelated" gnosis-dir))
        (should-error (call-interactively (key-binding (kbd "g"))) :type 'user-error)
        (should-error (call-interactively (key-binding (kbd "RET"))) :type 'user-error)
        (should-error (gnosis-dashboard-history) :type 'user-error)
        (should (equal before (buffer-string)))))))

(ert-deftest gnosis-study-activity-upgrades-owned-history-buffer ()
  "Reopen an owned pre-upgrade history buffer using the typed history mode."
  (gnosis-test-activity
    (with-current-buffer (get-buffer-create "*Gnosis History*")
      (tabulated-list-mode)
      (setq gnosis-dashboard--buffer-owner 'history)
      (let ((inhibit-read-only t)) (insert "Old daily history rendering")))
    (gnosis-dashboard-history)
    (should (derived-mode-p 'gnosis-dashboard-history-mode))
    (should-not (string-search "Old daily history rendering" (buffer-string)))
    (should (eq (key-binding (kbd "RET")) #'gnosis-dashboard-history-details))))

(ert-deftest gnosis-study-activity-repeated-midnight ()
  "Count both occurrences of midnight with a midnight logical-day cutoff."
  (gnosis-test-activity
    (let ((old-zone (getenv "TZ"))
          (gnosis-day-start-hour 0)
          (id (gnosis-test--add-basic-thema "Q" "A")))
      (unwind-protect
          (progn
            (set-time-zone-rule "America/Havana")
            (cl-loop for stamp in '("2026-11-01T04:30:00Z" "2026-11-01T05:30:00Z")
                     for attempt from 1 do
                     (gnosis-test-activity-event
                      id "midnight" attempt
                      (car (time-convert (date-to-time stamp) 1000000))))
            ;; Prime the libc time conversion with standard time, not a
            ;; coincidental DST choice for the ambiguous midnight boundary.
            (encode-time 0 0 12 1 11 2026)
            (should (= 2 (plist-get (gnosis-study-activity 20261101) :practice))))
        (set-time-zone-rule old-zone)))))

(provide 'gnosis-test-study-activity)
;;; gnosis-test-study-activity.el ends here
