;;; gnosis-test-dashboard-hardening.el --- Dashboard ownership tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Exercise refresh and progressive rendering with disposable SQLite data.

;;; Code:

(require 'ert)
(require 'gnosis-anki)
(require 'gnosis-dashboard)
(require 'gnosis-study)
(require 'gnosis-test-helpers)

(defmacro gnosis-test-dashboard--with-view (&rest body)
  "Run BODY in an isolated dashboard, retaining callbacks for explicit delivery."
  (declare (indent 0) (debug t))
  `(let* ((buffer (generate-new-buffer " *gnosis-dashboard-hardening*"))
          (gnosis-dashboard-buffer-name (buffer-name buffer))
          (gnosis-dashboard-render-chunk-size 2)
          (run-with-timer (symbol-function 'run-with-timer))
          callbacks)
     (unwind-protect
         (save-window-excursion
           (cl-letf (((symbol-function 'run-with-timer)
                      (lambda (delay repeat function &rest args)
                        (if (eq function #'gnosis-dashboard--append-chunk)
                            (progn
                              (setq callbacks
                                    (append callbacks (list (cons function args))))
                              nil)
                          (apply run-with-timer delay repeat function args)))))
             (cl-labels ((drain ()
                           (while callbacks
                             (let ((callback (pop callbacks)))
                               (apply (car callback) (cdr callback))))))
               ,@body)))
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun gnosis-test-dashboard--visible-ids ()
  "Return displayed row IDs in order without moving point."
  (save-excursion
    (goto-char (point-min))
    (cl-loop until (eobp)
             collect (tabulated-list-get-id)
             do (forward-line 1))))

(defun gnosis-test-dashboard--visible-entries ()
  "Return displayed IDs and column vectors without moving point."
  (save-excursion
    (goto-char (point-min))
    (cl-loop until (eobp)
             collect (list (tabulated-list-get-id) (tabulated-list-get-entry))
             do (forward-line 1))))

(ert-deftest gnosis-dashboard-hardening-refresh-external-writes ()
  "Refresh reads suspension and flags written outside the dashboard."
  (gnosis-test-with-db
    (let ((id (gnosis-test--add-basic-thema "Question" "Answer")))
      (gnosis-test-dashboard--with-view
        (gnosis-dashboard-output-themata (list id))
        (should (equal "No" (aref (tabulated-list-get-entry) 5)))
        (with-temp-buffer
          (gnosis-toggle-suspend-themata (list id) 1 t)
          (gnosis-study-flag id))
        (call-interactively (local-key-binding (kbd "g")))
        (should (equal "Yes" (aref (tabulated-list-get-entry) 5)))
        (should (string-search "needs_work" (aref (tabulated-list-get-entry) 3)))
        (with-temp-buffer
          (gnosis-toggle-suspend-themata (list id) 0 t)
          (gnosis-study-flag id t))
        (call-interactively (local-key-binding (kbd "g")))
        (should (equal "No" (aref (tabulated-list-get-entry) 5)))
        (should-not (string-search "needs_work" (aref (tabulated-list-get-entry) 3)))))))

(ert-deftest gnosis-dashboard-hardening-refresh-external-deletion ()
  "Refresh removes externally deleted IDs from both projection and collection."
  (gnosis-test-with-db
    (let ((id (gnosis-test--add-basic-thema "Question" "Answer")))
      (gnosis-test-dashboard--with-view
        (gnosis-dashboard-output-themata (list id))
        (with-temp-buffer (gnosis-delete-themata (list id)))
        (call-interactively (local-key-binding (kbd "g")))
        (drain)
        (should-not (gnosis-test-dashboard--visible-ids))
        (should-not tabulated-list-entries)
        (should-not gnosis-dashboard-themata-current-ids)))))

(ert-deftest gnosis-dashboard-hardening-append-preserves-snapshots ()
  "Progressive append preserves input, pending rows, and prior collection values."
  (gnosis-test-with-db
    (let* ((ids (cl-loop for n from 11 to 15
                         collect (gnosis-test--add-basic-thema
                                  (number-to-string n) "Answer" nil nil n)))
           (input (gnosis-dashboard--output-themata ids))
           (input-snapshot (copy-tree input t)))
      (gnosis-test-dashboard--with-view
        (with-current-buffer buffer
          (gnosis-dashboard-themata-mode)
          (gnosis-dashboard--set-column-format)
          (setq gnosis-dashboard--database gnosis-db)
          (gnosis-dashboard--progressive-render
           input gnosis-dashboard--load-generation)
          (should (equal input-snapshot input))
          (should-not tabulated-list-sort-key)
          (should (equal '(11 12) (mapcar #'car tabulated-list-entries)))
          (let* ((prefix tabulated-list-entries)
                 (prefix-snapshot (copy-tree prefix t))
                 (pending gnosis-dashboard--pending-entries)
                 (pending-snapshot (copy-tree pending t))
                 (callback (pop callbacks)))
            (should callback)
            ;; Deliver only after the initiating render has returned.
            (apply (car callback) (cdr callback))
            (should (equal '(11 12 13 14) (mapcar #'car tabulated-list-entries)))
            (should (equal prefix-snapshot prefix))
            (should (equal pending-snapshot pending))
            (should (equal input-snapshot input))
            (let* ((next-prefix tabulated-list-entries)
                   (next-snapshot (copy-tree next-prefix t)))
              (should callbacks)
              (drain)
              (should (equal ids (mapcar #'car tabulated-list-entries)))
              (should (equal tabulated-list-entries
                             (gnosis-test-dashboard--visible-entries)))
              (should-not gnosis-dashboard--pending-entries)
              (should-not gnosis-dashboard--timer)
              (should (equal prefix-snapshot prefix))
              (should (equal next-snapshot next-prefix))
              (should (equal pending-snapshot pending))
              (should (equal input-snapshot input)))))))))

(ert-deftest gnosis-dashboard-hardening-delete-during-append ()
  "Deleting rendered, pending, or all rows cannot detach the collection tail."
  (gnosis-test-with-db
    (dolist (positions '((1) (2) (0 1) (0 1 2)))
      (let ((ids (cl-loop for n below 3
                          collect (gnosis-test--add-basic-thema
                                   (format "Question %d" n) "Answer"))))
        (gnosis-test-dashboard--with-view
          (gnosis-dashboard-output-themata ids)
          (should callbacks)
          (setq gnosis-dashboard--selected-ids
                (mapcar (lambda (n) (nth n ids)) positions))
          (let ((remaining (seq-difference ids gnosis-dashboard--selected-ids)))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (call-interactively (local-key-binding (kbd "d"))))
            ;; Deliver canceled work too, as if already queued by the event loop.
            (drain)
            (should (equal remaining (gnosis-test-dashboard--visible-ids)))
            (should (equal remaining (mapcar #'car tabulated-list-entries)))
            (should (equal remaining gnosis-dashboard-themata-current-ids))))))))

(ert-deftest gnosis-dashboard-hardening-update-pending-row ()
  "An update before append must not later paint the old row snapshot."
  (gnosis-test-with-db
    (let ((ids (cl-loop for n below 3
                        collect (gnosis-test--add-basic-thema
                                 (format "Question %d" n) "Answer"))))
      (gnosis-test-dashboard--with-view
        (gnosis-dashboard-output-themata ids)
        (gnosis-update 'themata '(= keimenon "Updated") `(= id ,(nth 2 ids)))
        (gnosis-dashboard--update-entries (list (nth 2 ids)))
        (drain)
        (gnosis-dashboard--goto-id (nth 2 ids))
        (should (equal "Updated" (aref (tabulated-list-get-entry) 0)))
        (should (equal ids (gnosis-test-dashboard--visible-ids)))
        (should (equal ids (mapcar #'car tabulated-list-entries)))))))

(ert-deftest gnosis-dashboard-hardening-independent-render-owners ()
  "A second dashboard must not replace or cancel the first buffer's state."
  (gnosis-test-with-db
    (let ((ids (cl-loop for n below 3
                        collect (gnosis-test--add-basic-thema
                                 (format "Question %d" n) "Answer"))))
      (gnosis-test-dashboard--with-view
        (gnosis-dashboard-output-themata ids)
        (setq gnosis-dashboard--selected-ids (list (car ids)))
        (let ((other (generate-new-buffer " *gnosis-dashboard-other*")))
          (unwind-protect
              (let ((gnosis-dashboard-buffer-name (buffer-name other)))
                (gnosis-dashboard-output-themata (list (nth 2 ids)))
                (drain)
                (with-current-buffer buffer
                  (should (equal ids gnosis-dashboard-themata-current-ids))
                  (should (equal ids (gnosis-test-dashboard--visible-ids)))
                  (should (equal (list (car ids)) gnosis-dashboard--selected-ids)))
                (with-current-buffer other
                  (should (equal (list (nth 2 ids))
                                 (gnosis-test-dashboard--visible-ids)))))
            (kill-buffer other)))))))

(ert-deftest gnosis-dashboard-hardening-retired-mode-callback ()
  "A callback cannot regain authority when the same major mode is reentered."
  (gnosis-test-with-db
    (let ((ids (cl-loop for n below 3
                        collect (gnosis-test--add-basic-thema
                                 (format "Question %d" n) "Answer"))))
      (gnosis-test-dashboard--with-view
        (gnosis-dashboard-output-themata ids)
        (fundamental-mode)
        (gnosis-dashboard-themata-mode)
        (setq tabulated-list-format [("Question" 12 t)])
        (let ((inhibit-read-only t)) (erase-buffer) (insert "Successor\n"))
        (drain)
        (should (equal "Successor\n" (buffer-string)))
        (should-not tabulated-list-entries)))))

(ert-deftest gnosis-dashboard-hardening-restore-yields-to-point-motion ()
  "Deferred selection restoration must not override subsequent user movement."
  (gnosis-test-with-db
    (let ((ids (cl-loop for n below 3
                        collect (gnosis-test--add-basic-thema
                                 (format "Question %d" n) "Answer"))))
      (gnosis-test-dashboard--with-view
        (gnosis-dashboard-output-themata ids (nth 2 ids))
        (forward-line 1)
        (move-to-column 6)
        (drain)
        (should (equal (nth 1 ids) (tabulated-list-get-id)))
        (should (= 6 (current-column)))))))

(ert-deftest gnosis-dashboard-hardening-visit-selected-node-id ()
  "Visit follows the selected ID even when two native Org nodes share a title."
  (gnosis-test-with-db
    (let ((gnosis-nodes-dir gnosis-dir)
          (org-modules nil)
          (org-id-track-globally nil)
          (org-id-locations nil)
          node-buffers)
      (unwind-protect
          (progn
            (dolist (id '("node-a" "node-b"))
              (let ((file (concat id ".org")))
                (with-temp-file (expand-file-name file gnosis-dir)
                  (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: Shared\n" id)))
                (gnosis--insert-into 'nodes `([,id ,file "Shared" 0 nil "0" "hash"]))))
            (gnosis-test-dashboard--with-view
              (gnosis-dashboard-output-nodes '("node-a" "node-b"))
              (gnosis-dashboard--goto-id "node-b")
              (call-interactively (local-key-binding (kbd "RET")))
              (push (current-buffer) node-buffers)
              (should (equal (expand-file-name "node-b.org" gnosis-dir)
                             buffer-file-name))
              (should (equal "node-b" (org-id-get)))))
        (dolist (buffer node-buffers)
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest gnosis-dashboard-hardening-average-label ()
  "Describe the existing active-day denominator instead of a calendar-day mean."
  (gnosis-test-with-db
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO review_activity_baseline VALUES (?, 10, 0)"
     (list (gnosis--today-int)))
    (with-temp-buffer
      (funcall gnosis-dashboard-module-average-rev)
      (should (string-search "Reviews per active day: 10.00" (buffer-string))))))

(ert-deftest gnosis-dashboard-hardening-native-sort-during-append ()
  "Native sorting during a progressive load leaves a complete ordered list."
  (gnosis-test-with-db
    (let ((ids (mapcar (lambda (question)
                        (gnosis-test--add-basic-thema question "Answer"))
                      '("Charlie" "Alice" "Bob"))))
      (gnosis-test-dashboard--with-view
        (gnosis-dashboard-output-themata ids)
        (tabulated-list-sort 0)
        (drain)
        (should (equal (list (nth 1 ids) (nth 2 ids) (nth 0 ids))
                       (gnosis-test-dashboard--visible-ids)))
        (should (equal (gnosis-test-dashboard--visible-ids)
                       (mapcar #'car tabulated-list-entries)))))))

(ert-deftest gnosis-dashboard-hardening-sorted-bulk-suspend ()
  "The native suspension command preserves the active sort and selected row."
  (gnosis-test-with-db
    (dolist (id '(100 101 102 103))
      (gnosis-test--add-basic-thema
       (format "Question %d" id) "Answer" nil nil id (if (< id 102) 0 1)))
    (gnosis-test-dashboard--with-view
      (let ((gnosis-dashboard-render-chunk-size 4))
        (gnosis-dashboard-output-themata '(100 101 102 103)))
      (tabulated-list-sort 5)
      (dolist (id '(100 102))
        (gnosis-dashboard--goto-id id)
        (call-interactively (local-key-binding (kbd "m"))))
      (gnosis-dashboard--goto-id 100)
      (move-to-column 6)
      (let* ((old-entries tabulated-list-entries)
             (snapshot (copy-tree old-entries t)))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (call-interactively (local-key-binding (kbd "s"))))
        (should (equal '("Suspend") tabulated-list-sort-key))
        (should (equal '(101 100 102 103) (gnosis-test-dashboard--visible-ids)))
        (should (equal '(101 100 102 103) (mapcar #'car tabulated-list-entries)))
        (should (equal '("No" "Yes" "Yes" "Yes")
                       (mapcar (lambda (entry) (aref (cadr entry) 5))
                               tabulated-list-entries)))
        (should (equal tabulated-list-entries
                       (gnosis-test-dashboard--visible-entries)))
        (should (equal 100 (tabulated-list-get-id)))
        (should (= 6 (current-column)))
        (should-not gnosis-dashboard--selected-ids)
        (should (equal snapshot old-entries))))))

(ert-deftest gnosis-dashboard-hardening-pending-suspend-preserves-snapshots ()
  "Sorted settlement preserves retained prefix and pending row snapshots."
  (gnosis-test-with-db
    (dolist (id '(1 2 3 4))
      (gnosis-test--add-basic-thema (number-to-string id) "Answer" nil nil id))
    (gnosis-test-dashboard--with-view
      (gnosis-dashboard-output-themata '(4 3 2 1))
      (tabulated-list-sort 0)
      (should callbacks)
      (should (equal '(3 4) (mapcar #'car tabulated-list-entries)))
      (should (equal '(2 1) (mapcar #'car gnosis-dashboard--pending-entries)))
      (let* ((entries tabulated-list-entries)
             (snapshot (copy-tree entries t))
             (pending gnosis-dashboard--pending-entries)
             (pending-snapshot (copy-tree pending t)))
        (gnosis-dashboard--goto-id 3)
        (move-to-column 2)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (call-interactively (local-key-binding (kbd "s"))))
        (should (equal '("Keimenon") tabulated-list-sort-key))
        (should (equal '(1 2 3 4) (mapcar #'car tabulated-list-entries)))
        (should (equal tabulated-list-entries
                       (gnosis-test-dashboard--visible-entries)))
        (should (equal 3 (tabulated-list-get-id)))
        (should (= 2 (current-column)))
        (should (equal "Yes" (aref (tabulated-list-get-entry) 5)))
        (should (equal snapshot entries))
        (should (equal pending-snapshot pending))
        (should-not gnosis-dashboard--pending-entries)
        (let ((settled (copy-tree tabulated-list-entries t))
              (text (buffer-string)))
          ;; Deliver canceled callbacks after the synchronous mutation returns.
          (drain)
          (should (equal snapshot entries))
          (should (equal pending-snapshot pending))
          (should (equal settled tabulated-list-entries))
          (should (equal text (buffer-string)))
          (should (equal '("Keimenon") tabulated-list-sort-key))
          (should (equal 3 (tabulated-list-get-id)))
          (should (= 2 (current-column))))))))

(ert-deftest gnosis-dashboard-hardening-sorted-delete-during-append ()
  "Deleting through the native binding settles pending rows in sort order."
  (gnosis-test-with-db
    (let ((ids (mapcar (lambda (question)
                        (gnosis-test--add-basic-thema question "Answer"))
                      '("Charlie" "Alice" "Bob" "Aaron"))))
      (gnosis-test-dashboard--with-view
        (gnosis-dashboard-output-themata ids)
        (tabulated-list-sort 0)
        (should callbacks)
        (gnosis-dashboard--goto-id (nth 1 ids))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (call-interactively (local-key-binding (kbd "d"))))
        (let* ((expected (list (nth 3 ids) (nth 2 ids) (nth 0 ids)))
               (entries tabulated-list-entries)
               (snapshot (copy-tree entries t))
               (text (buffer-string))
               (selected-id (tabulated-list-get-id)))
          (should (equal '("Keimenon") tabulated-list-sort-key))
          (should (equal expected (gnosis-test-dashboard--visible-ids)))
          (should (equal expected (mapcar #'car tabulated-list-entries)))
          (should (equal tabulated-list-entries
                         (gnosis-test-dashboard--visible-entries)))
          ;; Alice's next row in the settled ascending list is Bob.
          (should (equal (nth 2 ids) selected-id))
          (should-not gnosis-dashboard--pending-entries)
          (should-not gnosis-dashboard--timer)
          (drain)
          (should (equal '("Keimenon") tabulated-list-sort-key))
          (should (equal text (buffer-string)))
          (should (equal selected-id (tabulated-list-get-id)))
          (should (equal expected (gnosis-test-dashboard--visible-ids)))
          (should (equal snapshot tabulated-list-entries))
          (should (equal snapshot entries)))))))

(ert-deftest gnosis-dashboard-hardening-sorted-native-edit-save ()
  "Native edit/save re-sorts changed text and rejects the canceled append."
  (require 'gnosis-export-import)
  (gnosis-test-with-db
    (let ((ids (mapcar (lambda (question)
                        (gnosis-test--add-basic-thema question "Answer"))
                      '("Charlie" "Alice" "Bob" "Aaron")))
          (register-alist nil)
          (org-id-track-globally nil))
      (unwind-protect
          (gnosis-test-dashboard--with-view
            (gnosis-dashboard-output-themata ids)
            (tabulated-list-sort 0)
            (tabulated-list-sort 0)
            (gnosis-dashboard--goto-id (car ids))
            (should callbacks)
            (call-interactively (local-key-binding (kbd "RET")))
            (should (derived-mode-p 'gnosis-edit-mode))
            (goto-char (point-min))
            (should (search-forward "Charlie" nil t))
            (replace-match "Aardvark" t t)
            (call-interactively (local-key-binding (kbd "C-c C-c")))
            (should (eq buffer (current-buffer)))
            (should (equal "Aardvark" (gnosis-get 'keimenon 'themata
                                                   `(= id ,(car ids)))))
            (should (equal '("Keimenon" . t) tabulated-list-sort-key))
            (should (equal (list (nth 2 ids) (nth 1 ids) (nth 3 ids) (car ids))
                           (gnosis-test-dashboard--visible-ids)))
            (should (equal tabulated-list-entries
                           (gnosis-test-dashboard--visible-entries)))
            (should (equal (car ids) (tabulated-list-get-id)))
            (let* ((entries tabulated-list-entries)
                   (snapshot (copy-tree entries t))
                   (text (buffer-string))
                   (callback (car callbacks)))
              (drain)
              (should (equal text (buffer-string)))
              (should (equal snapshot tabulated-list-entries))
              (should (equal (car ids) (tabulated-list-get-id)))
              ;; A retired projection must also survive late delivery after refresh.
              (gnosis-dashboard-output-themata (list (nth 2 ids)))
              (let ((successor (buffer-string)))
                (apply (car callback) (cdr callback))
                (should (equal successor (buffer-string)))
                (should (equal (list (nth 2 ids))
                               (mapcar #'car tabulated-list-entries)))
                (should (equal snapshot entries)))))
        (when-let* ((edit (get-buffer "*Gnosis Edit*")))
          (kill-buffer edit))))))

(ert-deftest gnosis-dashboard-hardening-callback-rejects-replaced-database ()
  "A deferred row from an old database cannot render under its replacement."
  (gnosis-test-with-db
    (let ((ids (cl-loop for n below 3
                        collect (gnosis-test--add-basic-thema
                                 (format "Question %d" n) "Answer"))))
      (gnosis-test-dashboard--with-view
        (gnosis-dashboard-output-themata ids)
        (let ((before (buffer-string)))
          (gnosis-test-with-db (drain))
          (should (equal before (buffer-string))))))))

(ert-deftest gnosis-dashboard-hardening-kill-retires-real-timer ()
  "Killing a progressive view cancels its real timer and rejects late delivery."
  (gnosis-test-with-db
    (let* ((buffer (generate-new-buffer " *gnosis-dashboard-timer*"))
           (gnosis-dashboard-buffer-name (buffer-name buffer))
           (gnosis-dashboard-render-chunk-size 1)
           (gnosis-dashboard-timer-delay 60)
           (ids (list (gnosis-test--add-basic-thema "One" "Answer")
                      (gnosis-test--add-basic-thema "Two" "Answer")))
           timer callback args)
      (unwind-protect
          (save-window-excursion
            (gnosis-dashboard-output-themata ids)
            (setq timer (seq-find
                         (lambda (timer)
                           (eq (timer--function timer)
                               #'gnosis-dashboard--append-chunk))
                         timer-list)
                  callback (timer--function timer)
                  args (timer--args timer))
            (should (timerp timer))
            (kill-buffer buffer)
            (should-not (memq timer timer-list))
            (with-temp-buffer
              (rename-buffer gnosis-dashboard-buffer-name)
              (insert "Unrelated replacement")
              (apply callback args)
              (should (equal "Unrelated replacement" (buffer-string)))))
        (when (timerp timer) (cancel-timer timer))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest gnosis-dashboard-hardening-failed-refresh-preserves-owner ()
  "A failed row query leaves the previous collection and its pending work intact."
  (gnosis-test-with-db
    (let ((ids (cl-loop for n below 3
                        collect (gnosis-test--add-basic-thema
                                 (format "Question %d" n) "Answer"))))
      (gnosis-test-dashboard--with-view
        (gnosis-dashboard-output-themata ids)
        (let ((before (buffer-string))
              (entries tabulated-list-entries))
          (cl-letf (((symbol-function 'gnosis-sqlite-select-batch)
                     (lambda (&rest _) (error "Injected read failure"))))
            (should-error (gnosis-dashboard-output-themata ids)))
          (should (equal before (buffer-string)))
          (should (equal entries tabulated-list-entries))
          (drain)
          (should (equal ids (gnosis-test-dashboard--visible-ids))))))))

(ert-deftest gnosis-dashboard-hardening-multiline-import-delete ()
  "Deleting an imported multiline cloze leaves no actionable continuation row."
  (gnosis-test-with-db
    (let* ((items (gnosis-anki--parse-cloze-note
                   (concat "{{c1::alpha<br>beta}} and {{c2::gamma}}" "\x1f")
                   " test " (make-hash-table :test 'equal)
                   (make-hash-table :test 'equal)))
           (buffer (generate-new-buffer " *gnosis-dashboard-multiline*"))
           (gnosis-dashboard-buffer-name (buffer-name buffer))
           (gnosis-dashboard-render-chunk-size 2))
      (unwind-protect
          (save-window-excursion
            (should (= 2 (length items)))
            (gnosis-anki--bulk-insert-chunk
             gnosis-db items '(801 802) (gnosis--today-int))
            (gnosis-dashboard-output-themata '(801 802))
            (should (equal '("alpha\nbeta")
                           (gnosis-get 'answer 'themata '(= id 801))))
            (let ((before (gnosis-test-dashboard--visible-ids)))
              (goto-char (point-min))
              (should (equal 801 (tabulated-list-get-id)))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                (call-interactively (local-key-binding (kbd "d"))))
              (should (equal '(802) (gnosis-select 'id 'themata nil t)))
              (should (equal '(802) (mapcar #'car tabulated-list-entries)))
              (should (equal '(802) gnosis-dashboard-themata-current-ids))
              (should (equal '(802) (gnosis-test-dashboard--visible-ids)))
              (should (equal '(801 802) before))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest gnosis-dashboard-hardening-multiline-refresh-suspend ()
  "Refresh and row replacement flatten display, not stored answers, hints or tags."
  (gnosis-test-with-db
    (let* ((items (gnosis-anki--parse-cloze-note
                   (concat "{{c1::alpha<br>beta::first<br>hint}} and {{c2::gamma}}"
                           "\x1f")
                   " test " (make-hash-table :test 'equal)
                   (make-hash-table :test 'equal)))
           (buffer (generate-new-buffer " *gnosis-dashboard-multiline*"))
           (gnosis-dashboard-buffer-name (buffer-name buffer))
           (gnosis-dashboard-render-chunk-size 2))
      (unwind-protect
          (save-window-excursion
            (gnosis-anki--bulk-insert-chunk
             gnosis-db items '(801 802) (gnosis--today-int) "two\nlines")
            (gnosis-dashboard-output-themata '(801 802))
            (dolist (step '(("g" . 0) ("s" . 1) ("s" . 0)))
              (goto-char (point-min))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                (call-interactively (local-key-binding (kbd (car step)))))
              (should (equal '(801 802) (gnosis-test-dashboard--visible-ids)))
              (should (equal tabulated-list-entries
                             (gnosis-test-dashboard--visible-entries)))
              (should (= (cdr step) (gnosis-get 'suspended 'scheduler-state
                                               '(= thema-id 801))))
              (should (equal (if (= (cdr step) 1) "Yes" "No")
                             (aref (tabulated-list-get-entry) 5)))
              (should (equal "alpha beta and gamma"
                             (aref (tabulated-list-get-entry) 0)))
              (should (equal "first hint" (aref (tabulated-list-get-entry) 1)))
              (should (equal "alpha beta" (aref (tabulated-list-get-entry) 2)))
              (should (member "two lines"
                              (split-string (aref (tabulated-list-get-entry) 3) ",")))
              (should (equal '("alpha\nbeta")
                             (gnosis-get 'answer 'themata '(= id 801))))
              (should (equal '("first\nhint")
                             (gnosis-get 'hypothesis 'themata '(= id 801))))
              (should (equal '("test" "two\nlines")
                             (sort (gnosis-select 'tag 'thema-tag
                                                  '(= thema-id 801) t)
                                   #'string<)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'gnosis-test-dashboard-hardening)
;;; gnosis-test-dashboard-hardening.el ends here
