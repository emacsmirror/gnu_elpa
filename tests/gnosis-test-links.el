;;; gnosis-test-links.el --- Link mutation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Verify link maintenance behavior against isolated SQLite databases.

;;; Code:

(require 'gnosis-test-helpers)
(require 'gnosis-links)
(require 'gnosis-dashboard)

(ert-deftest gnosis-test-links-cleanup-rolls-back-all-deletes ()
  "Cleanup rolls back earlier deletes when a later delete fails."
  (gnosis-test-with-db
    (let* ((source (gnosis-test--add-basic-thema "No links" "Answer"))
           (orphan "missing-node")
           (node-link '("node-source" "missing-dest")))
      (gnosis--insert-into
       'nodes '(["node-source" "node.org" "Node" 0 nil "0" "hash"]))
      (gnosis--insert-into 'thema-links `([,source ,orphan]))
      (gnosis--insert-into 'node-links `([,@node-link]))
      (gnosis-sqlite-execute
       gnosis-db
       (concat "CREATE TRIGGER fail_node_link_delete "
               "BEFORE DELETE ON node_links BEGIN "
               "SELECT RAISE(ABORT, 'forced delete failure'); END"))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (should-error (gnosis-links-cleanup)))
      (should (equal (gnosis-select '[source dest] 'thema-links)
                     `((,source ,orphan))))
      (should (equal (gnosis-select '[source dest] 'node-links)
                     (list node-link))))))

(ert-deftest gnosis-test-links-sync-rolls-back-deletes-on-insert-failure ()
  "Sync restores deleted links when inserting a missing link fails."
  (gnosis-test-with-db
    (let* ((expected "expected-node")
           (orphan "missing-node")
           (source
            (gnosis-test--add-basic-thema
             (format "See [[id:%s][Node]]" expected) "Answer")))
      (gnosis--insert-into
       'nodes `([,expected "node.org" "Node" 0 nil "0" "hash"]))
      (gnosis--insert-into 'thema-links `([,source ,orphan]))
      (gnosis-sqlite-execute
       gnosis-db
       (concat "CREATE TRIGGER fail_thema_link_insert "
               "BEFORE INSERT ON thema_links BEGIN "
               "SELECT RAISE(ABORT, 'forced insert failure'); END"))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (should-error (gnosis-links-sync)))
      (should (equal (gnosis-select '[source dest] 'thema-links)
                     `((,source ,orphan))))
      (should-not
       (gnosis-select '[source dest] 'thema-links
                      `(and (= source ,source) (= dest ,expected)))))))

(ert-deftest gnosis-test-bulk-link-updates-thema-link-index ()
  "Bulk-link updates both thema text and its node-link index."
  (gnosis-test-with-db
    (let ((id (gnosis-test--add-basic-thema "Emacs question" "Answer")))
      (gnosis--insert-into
       'nodes '(["node-1" "node.org" "Emacs" 0 nil "0" "hash"]))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (gnosis-bulk-link-themata (list id) "Emacs" "node-1"))
      (should (equal (gnosis-get 'keimenon 'themata `(= id ,id))
                     "[[id:node-1][Emacs]] question"))
      (should (equal (gnosis-select '[source dest] 'thema-links)
                     `((,id "node-1")))))))

(ert-deftest gnosis-test-stale-links-bounds-source-lookups ()
  "Finding linked source text must not rescan the full thema collection."
  (let* ((themata (cl-loop for id from 1 to 200
                           collect (list id "[[id:valid]]")))
         (links (cl-loop for id from 181 to 200
                        append (list (list id "valid")
                                     (list id "stale"))))
         (snapshot (read (prin1-to-string (list themata links))))
         (find (symbol-function 'cl-find))
         (visits 0))
    (cl-letf (((symbol-function 'gnosis-select)
               (lambda (_columns table &rest _)
                 (pcase table
                   ('themata themata)
                   ('thema-links links))))
              ((symbol-function 'cl-find)
               (lambda (item sequence &rest args)
                 (if (not (eq sequence themata))
                     (apply find item sequence args)
                   (let ((key (or (plist-get args :key) #'identity)))
                     (apply find item sequence
                            (plist-put
                             (copy-sequence args) :key
                             (lambda (row)
                               (cl-incf visits)
                               (funcall key row)))))))))
      (should (equal (gnosis--stale-links)
                     (cl-loop for id from 181 to 200
                              collect (list id "stale"))))
      (should (equal (list themata links) snapshot))
      (should (<= visits (* 2 (length themata)))))))

(ert-deftest gnosis-test-stale-links-extracts-each-source-once ()
  "Multiple links share text extraction, including empty source text."
  (let* ((themata '((1 "[[id:question][Question]]") (2 nil)
                   (3 "Unlinked text")))
         (extras '((1 "[[id:context]] [[id:question]]") (2 nil)))
         (links '((1 "stale-z") (2 "empty-a") (1 "question")
                  (1 "stale-a") (2 "empty-b") (1 "context")
                  (99 "absent-a") (99 "absent-b")))
         (snapshot (read (prin1-to-string (list themata extras links))))
         (extract (symbol-function 'gnosis-extract-id-links))
         (calls 0))
    (cl-letf (((symbol-function 'gnosis-select)
               (lambda (_columns table &rest _)
                 (pcase table
                   ('themata themata)
                   ('extras extras)
                   ('thema-links links))))
              ((symbol-function 'gnosis-extract-id-links)
               (lambda (&rest args)
                 (cl-incf calls)
                 (apply extract args))))
      (should (equal (gnosis--stale-links)
                     '((1 "stale-z") (2 "empty-a") (1 "stale-a")
                       (2 "empty-b") (99 "absent-a") (99 "absent-b"))))
      (should (equal (list themata extras links) snapshot))
      (should (= calls 6)))))

(ert-deftest gnosis-test-links-report-and-dashboard-agree ()
  "The public report and automatic dashboard count retain link issues."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema
                "[[id:question]]" "Answer" nil "[[id:context]]"))
           (stale (list (list id "stale-a") (list id "stale-z")))
           (before nil)
           (report nil))
      (dolist (dest '("question" "context" "stale-a" "stale-z"))
        (gnosis--insert-into
         'nodes `([,dest "node.org" ,dest 0 nil "0" "hash"])))
      (dolist (dest '("question" "stale-a" "stale-z"))
        (gnosis--insert-into 'thema-links `([,id ,dest])))
      (setq before (gnosis-select '[source dest] 'thema-links))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'gnosis--links-report-generate)
                 (lambda (&rest sections) (setq report sections))))
        (gnosis-links-check))
      (should (equal report (list nil stale (list (list id "context"))
                                 nil nil)))
      (with-temp-buffer
        (gnosis-dashboard-mode)
        (setq gnosis-dashboard--database gnosis-db)
        (let (callbacks)
          (cl-letf (((symbol-function 'run-with-timer)
                     (lambda (_delay _repeat fn &rest args)
                       (push (cons fn args) callbacks) nil)))
            (gnosis-dashboard--compute-link-issues)
            (while callbacks
              (let ((call (pop callbacks))) (apply (car call) (cdr call))))))
        (should (= gnosis-dashboard--link-issues 3)))
      (should (equal (gnosis-select '[source dest] 'thema-links) before)))))

(ert-deftest gnosis-test-links-dashboard-null-and-literal-nil-counts ()
  "Keep SQL NULL distinct from literal IDs in reports and the badge."
  ;; TEXT, indexed (SOURCE DEST) rows, and the five public summary counts.
  (dolist (case '(("[[id:nil]]" ((1 nil)) (1 1 1 0 0))
                  ("[[id:nil]]" nil (0 0 1 0 0))
                  ("[[id:nil]]" ((1 "nil")) (1 0 0 0 0))
                  ("[[id:nil]]" ((1 nil) (1 "nil")) (2 1 0 0 0))
                  ("[[id:nil]]" ((1 nil) (1 nil) (1 "nil")) (2 2 0 0 0))
                  ("No links" ((1 nil) (1 "nil")) (2 2 0 0 0))
                  ("[[id:other]]" ((1 nil)) (1 1 1 0 0))
                  ("[[id:nil]]" ((2 nil)) (1 1 1 0 0))))
    (ert-info ((format "Text/index/summary: %S" case))
      (gnosis-test-with-db
        (gnosis-test--add-basic-thema (car case) "Answer" nil nil 1)
        (gnosis-test--add-basic-thema "No links" "Answer" nil nil 2)
        (dolist (row (cadr case))
          (gnosis--insert-into 'thema-links (list (vconcat row))))
        (let ((before (gnosis-select '[source dest] 'thema-links))
              (format-count (symbol-function 'gnosis--links-check-format-count))
              counts)
          ;; The public summary and incremental badge share exact identities.
          (cl-letf (((symbol-function 'gnosis--links-check-format-count)
                     (lambda (count)
                       (push count counts) (funcall format-count count)))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
            (gnosis-links-check))
          (should (equal (nreverse counts) (nth 2 case)))
          (with-temp-buffer
            (gnosis-dashboard-mode)
            (setq gnosis-dashboard--database gnosis-db)
            (let (callbacks)
              (cl-letf (((symbol-function 'run-with-timer)
                         (lambda (_delay _repeat fn &rest args)
                           (push (cons fn args) callbacks) nil)))
                (gnosis-dashboard--compute-link-issues)
                (should-not gnosis-dashboard--link-issues)
                (should callbacks)
                (let ((steps 0))
                  (while callbacks
                    (let ((call (pop callbacks)))
                      (apply (car call) (cdr call)))
                    (should (< (cl-incf steps) 10))))))
            (should (= gnosis-dashboard--link-issues
                       (apply #'+ (nth 2 case)))))
          (should (equal (gnosis-select '[source dest] 'thema-links) before)))))))

(ert-deftest gnosis-test-link-destinations-linear-and-ordered ()
  "Destination deduplication and subtraction preserve last-occurrence order."
  (let* ((dests '("z" "a" "z" "present" "b" "a"))
         (snapshot (read (prin1-to-string dests)))
         (difference (symbol-function 'cl-set-difference))
         (duplicates (symbol-function 'cl-remove-duplicates))
         (quadratic 0))
    (cl-letf (((symbol-function 'gnosis-select)
               (lambda (_columns table &rest _)
                 (pcase table ('thema-links dests) ('nodes '("present")))))
              ((symbol-function 'cl-set-difference)
               (lambda (&rest args)
                 (cl-incf quadratic) (apply difference args)))
              ((symbol-function 'cl-remove-duplicates)
               (lambda (&rest args)
                 (cl-incf quadratic) (apply duplicates args))))
      (should (equal (gnosis--all-link-dests) '("z" "present" "b" "a")))
      (should (equal (gnosis--orphaned-link-dests) '("z" "b" "a")))
      (should (equal dests snapshot))
      (should (zerop quadratic)))))

(ert-deftest gnosis-test-dashboard-link-audit-yields-before-publishing ()
  "A fresh automatic count yields without publishing a partial result."
  (gnosis-test-with-db
    (dotimes (i 600)
      (gnosis-test--add-basic-thema "[[id:missing]]" "Answer" nil nil (1+ i)))
    (with-temp-buffer
      (gnosis-dashboard-mode)
      (setq gnosis-dashboard--database gnosis-db)
      (let ((callbacks nil)
            (extract (symbol-function 'gnosis-extract-id-links))
            (select (symbol-function 'gnosis-sqlite-select))
            (extracts 0))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat fn &rest args)
                     (push (cons fn args) callbacks) nil))
                  ((symbol-function 'gnosis-sqlite-select)
                   (lambda (&rest args)
                     (let ((rows (apply select args)))
                       (should (<= (length rows) 256))
                       rows)))
                  ((symbol-function 'gnosis-extract-id-links)
                   (lambda (&rest args)
                     (cl-incf extracts) (apply extract args))))
          (gnosis-dashboard--compute-link-issues)
          (should-not gnosis-dashboard--link-issues)
          (should callbacks)
          (let ((steps 0))
            (while callbacks
              (let ((call (pop callbacks)) (before extracts))
                (apply (car call) (cdr call))
                (should (<= (- extracts before) 512)))
              (cl-incf steps)
              (should (< steps 30))))
          (should (= gnosis-dashboard--link-issues 600)))))))

(ert-deftest gnosis-test-dashboard-link-audit-restarts-on-content-mutation ()
  "A content write between slices invalidates the entire partial count."
  (gnosis-test-with-db
    (dotimes (i 600)
      (gnosis-test--add-basic-thema "[[id:missing]]" "Answer" nil nil (1+ i)))
    (with-temp-buffer
      (gnosis-dashboard-mode)
      (setq gnosis-dashboard--database gnosis-db)
      (let (callbacks)
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat fn &rest args)
                     (push (cons fn args) callbacks) nil)))
          (gnosis-dashboard--compute-link-issues)
          (should callbacks)
          ;; Advance until some, but not all, text has contributed.
          (let ((extract (symbol-function 'gnosis-extract-id-links))
                (extracts 0))
            (cl-letf (((symbol-function 'gnosis-extract-id-links)
                       (lambda (&rest args)
                         (cl-incf extracts) (apply extract args))))
              (while (and callbacks (zerop extracts))
                (let ((call (pop callbacks))) (apply (car call) (cdr call))))))
          (should-not gnosis-dashboard--link-issues)
          (gnosis-update 'themata '(= keimenon "No links") '(= id 1))
          (let ((steps 0))
            (while callbacks
              (let ((call (pop callbacks))) (apply (car call) (cdr call)))
              (cl-incf steps)
              (should (< steps 30))))
          (should (= gnosis-dashboard--link-issues 599)))))))

(ert-deftest gnosis-test-dashboard-link-audit-rejects-retired-owners ()
  "Queued pages cannot publish or schedule after their view owner retires."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "[[id:missing]]" "Answer")
    (dolist (retire '(cancel mode refresh database kill))
      (let ((buffer (generate-new-buffer " *link audit owner*")) callbacks)
        (unwind-protect
            (with-current-buffer buffer
              (gnosis-dashboard-mode)
              (setq gnosis-dashboard--database gnosis-db)
              (cl-letf (((symbol-function 'run-with-timer)
                         (lambda (_delay _repeat fn &rest args)
                           (push (cons fn args) callbacks) nil)))
                (gnosis-dashboard--compute-link-issues)
                (let ((old (pop callbacks)))
                  (should old)
                  (pcase retire
                    ('cancel (gnosis-dashboard--cancel-load))
                    ('mode (fundamental-mode))
                    ('refresh (gnosis-dashboard--compute-link-issues))
                    ('database (setq gnosis-dashboard--database 'successor))
                    ('kill (kill-buffer buffer)))
                  (let ((successor callbacks))
                    (with-temp-buffer (apply (car old) (cdr old)))
                    (should (eq callbacks successor)))
                  (when (buffer-live-p buffer)
                    (should-not gnosis-dashboard--link-issues)))
                ;; Refresh owns an independent complete result.
                (when (eq retire 'refresh)
                  (while callbacks
                    (let ((call (pop callbacks))) (apply (car call) (cdr call))))
                  (should (= gnosis-dashboard--link-issues 1)))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest gnosis-test-dashboard-link-audit-restarts-on-external-write ()
  "A second SQLite connection invalidates text already read by an audit."
  (gnosis-test-with-db
    (dotimes (i 300)
      (gnosis-test--add-basic-thema "[[id:missing]]" "Answer" nil nil (1+ i)))
    (let ((other (gnosis-sqlite-open
                  (caddr (car (sqlite-select gnosis-db "PRAGMA database_list"))))))
      (unwind-protect
          (with-temp-buffer
            (gnosis-dashboard-mode)
            (setq gnosis-dashboard--database gnosis-db)
            (let (callbacks)
              (cl-letf (((symbol-function 'run-with-timer)
                         (lambda (_delay _repeat fn &rest args)
                           (push (cons fn args) callbacks) nil)))
                (gnosis-dashboard--compute-link-issues)
                ;; Advance through the node and link tables and first text page.
                (dotimes (_ 4)
                  (let ((call (pop callbacks))) (apply (car call) (cdr call))))
                (should-not gnosis-dashboard--link-issues)
                (gnosis-sqlite-execute other
                                       "UPDATE themata SET keimenon = ? WHERE id = 1"
                                       '("No links"))
                (while callbacks
                  (let ((call (pop callbacks))) (apply (car call) (cdr call))))
                (should (= gnosis-dashboard--link-issues 299)))))
        (gnosis-sqlite-close other)))))

(ert-deftest gnosis-test-link-audit-count-matches-all-report-sections ()
  "Paged counts include orphan destinations, both node ends and text issues."
  (gnosis-test-with-db
    (let ((id (gnosis-test--add-basic-thema
               "[[id:valid]] [[id:missing]] [[id:valid]]" "Answer"
               nil "[[id:journal]] [[id:missing]]")))
      (gnosis--insert-into 'nodes '(["valid" "file" "Valid" 0 nil nil nil]))
      (gnosis--insert-into 'journal '(["journal" "file" "Journal" 0 nil nil nil]))
      (gnosis--insert-into 'thema-links
                          `([,id "valid"] [,id "orphan"] [,id "journal"]
                            [nil "orphan"] [nil nil] [nil nil]))
      (gnosis--insert-into 'node-links
                          '(["valid" "gone"] ["valid" "journal"] [nil "gone"]))
      (let* ((before (gnosis-select '* 'thema-links))
             (expected (+ (length (gnosis--orphaned-link-dests))
                          (length (gnosis--stale-links))
                          (length (gnosis--missing-links))
                          (length (gnosis--node-links-missing-dest))
                          (length (gnosis--node-links-missing-source))))
             (audit (gnosis--link-audit-new)))
        (while (not (gnosis--link-audit-page gnosis-db audit)))
        (should (= (plist-get audit :count) expected))
        (should (equal (gnosis-select '* 'thema-links) before))))))

(ert-deftest gnosis-test-dashboard-link-audit-checks-revision-after-page ()
  "A write while reading the final page cannot publish a superseded count."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "[[id:missing]]" "Answer" nil nil 1)
    (with-temp-buffer
      (gnosis-dashboard-mode)
      (setq gnosis-dashboard--database gnosis-db)
      (let ((page (symbol-function 'gnosis--link-audit-page))
            callbacks changed)
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat fn &rest args)
                     (push (cons fn args) callbacks) nil))
                  ((symbol-function 'gnosis--link-audit-page)
                   (lambda (&rest args)
                     (let ((done (apply page args)))
                       (when (and done (not changed))
                         (setq changed t)
                         (gnosis-update 'themata '(= keimenon "No links") '(= id 1)))
                       done))))
          (gnosis-dashboard--compute-link-issues)
          (while callbacks
            (let ((call (pop callbacks))) (apply (car call) (cdr call)))
            (should-not (eql gnosis-dashboard--link-issues 1)))
          (should changed)
          (should (= gnosis-dashboard--link-issues 0)))))))

(provide 'gnosis-test-links)
;;; gnosis-test-links.el ends here
