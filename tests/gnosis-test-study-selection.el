;;; gnosis-test-study-selection.el --- Topic selection costs -*- lexical-binding: t; -*-

(require 'gnosis-test-study)
(require 'gnosis-test-agent)

(ert-deftest gnosis-study-selection-raw-native-and-agent-contracts ()
  "Keep raw diagnostics separate from native graph and due selection."
  (gnosis-test-agent
    (dolist (node '("one" "two" "back" "empty"))
      (gnosis-test-study-node node node))
    (gnosis--insert-into 'node-links
                         '(["one" "two"] ["two" "one"] ["back" "one"]))
    (dolist (id '(101 102 103 104 105))
      (gnosis-test--add-basic-thema (format "Q%d" id) "A" nil nil id
                                   (if (= id 103) 1 0)))
    (gnosis-update 'scheduler-state '(= due-day 20261026) '(= thema-id 102))
    (gnosis-update 'scheduler-state '(= due-day 20261025)
                   '(in thema-id [101 104 105]))
    (gnosis--insert-into 'thema-links
                         '([104 "one"] [102 "two"] [103 "one"]
                           [101 "one"] [101 "two"] [105 "back"]))
    (let* ((topics (list "two" "one" "one"))
           (before (copy-sequence topics))
           (gnosis-new-themata-limit 0)
           (evidence (gnosis-test-study-snapshot)))
      (cl-letf (((symbol-function 'gnosis--today-int) (lambda () 20261025)))
        (should (equal '(104 101 102) (gnosis-study-topic-ids topics)))
        (should (equal '(104 101) (gnosis-study-topic-ids topics t)))
        (should (equal '(104 101 102) (gnosis-study-topic-ids '("one") nil 8 0)))
        (should (equal '(105 104 101 102) (gnosis-study-topic-ids '("one") nil 8 8)))
        (should-not (gnosis-study-topic-ids nil))
        (should-not (gnosis-study-topic-ids '("empty") t))
        ;; Native launch shuffles; the adapter must never inherit that order.
        (let (shuffled launched)
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'gnosis-shuffle)
                     (lambda (ids) (setq shuffled ids) (reverse ids)))
                    ((symbol-function 'gnosis-review-loop)
                     (lambda (ids &rest _) (setq launched ids))))
            (gnosis-review-due-topic topics))
          (should (equal '(104 101) shuffled))
          (should (equal '(101 104) launched))))
      (gnosis-study-topic "one")
      (should (equal '(104 103 101) (mapcar #'car tabulated-list-entries)))
      (should (equal "Suspended" (aref (cadr (assoc 103 tabulated-list-entries)) 2)))
      (call-interactively (key-binding (kbd "g")))
      (should (equal '(104 103 101) (mapcar #'car tabulated-list-entries)))
      (let* ((status (gnosis-agent-start-practice :topic-ids '("one" "one") :limit 1))
             (selection (plist-get status :selection)))
        (should (equal [101] (plist-get status :selected-ids)))
        (should (equal '(:limit 1 :candidates 3 :eligible 2 :selected 1
                               :shortfall 0 :omitted-by-limit 1
                               :excluded-ids [103] :topic-ids ["one"])
                       selection))
        (should (stringp (json-serialize status :false-object :false :null-object nil)))
        (gnosis-agent-cancel (plist-get status :session-id)))
      (let* ((ids (list 104 999 103 101 101))
             (original (copy-sequence ids))
             (status (gnosis-agent-start-practice :thema-ids ids :limit 5)))
        (should (equal [101 104] (plist-get status :selected-ids)))
        (should (equal '(:limit 5 :candidates 4 :eligible 2 :selected 2
                               :shortfall 3 :omitted-by-limit 0
                               :excluded-ids [103 999] :topic-ids [])
                       (plist-get status :selection)))
        (should (equal original ids))
        (gnosis-agent-cancel (plist-get status :session-id)))
      (should (equal before topics))
      (should (equal evidence (gnosis-test-study-snapshot))))))

(ert-deftest gnosis-study-selection-missing-rows ()
  "Exclude absent themata and scheduler rows even in a damaged link index."
  (gnosis-test-study
    (gnosis-test--add-basic-thema "Q" "A" nil nil 101)
    (gnosis-test--add-basic-thema "No state" "A" nil nil 102)
    (gnosis--insert-into 'thema-links '([101 "one"] [102 "one"]))
    (gnosis--delete 'scheduler-state '(= thema-id 102))
    ;; Deliberately damaged disposable index, not a supported database writer.
    (sqlite-execute gnosis-db "PRAGMA foreign_keys = OFF")
    (unwind-protect
        (sqlite-execute gnosis-db "INSERT INTO thema_links VALUES (999, 'one')")
      (sqlite-execute gnosis-db "PRAGMA foreign_keys = ON"))
    (dolist (due '(nil t))
      (should (equal '(101) (gnosis-study-topic-ids '("one" "one") due))))))

(ert-deftest gnosis-study-selection-bounded-query-counts ()
  "Count real SQLite reads at scale and with a small parameter budget."
  (gnosis-test-with-db
    (let ((previous 0) measurements)
      (dolist (size '(32 256 1024))
        (gnosis-sqlite-with-transaction gnosis-db
          (cl-loop for id from (1+ previous) to size do
                   (gnosis-test--add-basic-thema "Q" "A" nil nil id)
                   (gnosis--insert-into 'thema-links `([,id "one"]))))
        (setq previous size)
        (dolist (limit '(32766 17))
          (dolist (due '(nil t))
            (let ((gnosis-sqlite--max-vars limit)
                  (select (symbol-function 'sqlite-select))
                  (calls 0) (max-params 0))
              (cl-letf (((symbol-function 'sqlite-select)
                         (lambda (db sql &optional params &rest args)
                           (cl-incf calls)
                           (setq max-params (max max-params (length params)))
                           (apply select db sql params args))))
                (should (equal (number-sequence 1 size)
                               (gnosis-study-topic-ids '("one") due))))
              (push (list size limit due calls max-params) measurements)))))
      (setq measurements (nreverse measurements))
      (message "Study selection (size limit due reads max-params): %S" measurements)
      (dolist (row measurements)
        (pcase-let ((`(,size ,limit ,_ ,calls ,max-params) row))
          (should (= calls (1+ (ceiling size limit))))
          (should (<= max-params limit)))))))

(ert-deftest gnosis-study-selection-logical-cutoff ()
  "Use the logical due day rather than calendar midnight, with no new cap."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Q" "A" nil nil 101)
    (gnosis--insert-into 'thema-links '([101 "one"]))
    (gnosis-update 'scheduler-state '(= due-day 20261026) '(= thema-id 101))
    (let ((zone (getenv "TZ"))
          (gnosis-new-themata-limit 0)
          (gnosis-day-start-hour 3))
      (unwind-protect
          (progn
            (set-time-zone-rule "UTC0")
            (cl-letf (((symbol-function 'current-time)
                       (lambda () (date-to-time "2026-10-26T02:59:59Z"))))
              (should (equal '(101) (gnosis-study-topic-ids '("one"))))
              (should-not (gnosis-study-topic-ids '("one") t)))
            (cl-letf (((symbol-function 'current-time)
                       (lambda () (date-to-time "2026-10-26T03:00:00Z"))))
              (should (equal '(101) (gnosis-study-topic-ids '("one") t)))))
        (set-time-zone-rule zone)))))

(ert-deftest gnosis-study-selection-projection-preserves-inputs ()
  "Filter original ID order independently of query order without mutation."
  (let* ((ids (list 101 999 102 103 101))
         (rows (list (list 103 1 20261025) (list 102 0 20261026)
                     (list 101 0 20261025)))
         (before (copy-tree (list ids rows))))
    (should (equal '(101 102 101) (gnosis-study--eligible-ids ids rows nil)))
    (should (equal '(101 101) (gnosis-study--eligible-ids ids rows 20261025)))
    (should (equal before (list ids rows)))))

(provide 'gnosis-test-study-selection)
;;; gnosis-test-study-selection.el ends here
