;;; gnosis-test-printer-callers.el --- Persistent printer boundaries -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Raw bulk parameters and nested Lisp strings retain their existing wire
;; representation regardless of display settings.  Event hashes retain every
;; entropy input, including the nested timestamp.

;;; Code:

(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-anki)
(require 'gnosis-nodes)
(require 'gnosis-tags)
(require 'gnosis-scheduler)

(ert-deftest gnosis-test-printer-anki-wire-roundtrip ()
  "Reopen bulk imports with complete fields and unchanged SQL representations."
  (dolist (settings '(nil ((print-length . 2)) ((print-level . 0))
                         ((print-escape-newlines . t)
                          (print-escape-multibyte . t))))
    (gnosis-test-with-db
      (cl-progv (mapcar #'car settings) (mapcar #'cdr settings)
        (gnosis-anki--bulk-insert-chunk
         gnosis-db
         '((:type "cloze" :keimenon "Q\nλ" :hypothesis ("a" "b" "c")
            :answer ("A" "B" "C") :parathema "P\nλ" :guid "raw\"guid"
            :tags ("tag\nλ"))
           (:type "cloze" :keimenon "Q2" :hypothesis nil
            :answer ("A2") :parathema nil :guid nil)
           (:type "basic" :keimenon "Q3" :hypothesis ""
            :answer ("nil") :parathema "" :guid ""))
         '(101 102 103) 20260907 nil t))
      (gnosis-sqlite-close gnosis-db)
      (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
      ;; Literal legacy wire values distinguish text nil, empty text and NULL.
      ;; IDs remain SQLite integers; source_guid is deliberately not Lisp text.
      (should
       (equal (sqlite-select gnosis-db
                             "SELECT * FROM themata ORDER BY id")
              '((101 "\"cloze\"" "\"Q\nλ\"" "(\"a\" \"b\" \"c\")"
                     "(\"A\" \"B\" \"C\")" "raw\"guid" nil)
                (102 "\"cloze\"" "\"Q2\"" "nil" "(\"A2\")" nil nil)
                (103 "\"basic\"" "\"Q3\"" "\"\"" "(\"nil\")" "" nil))))
      (should (equal (sqlite-select gnosis-db "SELECT * FROM extras ORDER BY id")
                     '((101 "\"P\nλ\"" "\"\"")
                       (102 "nil" "\"\"") (103 "\"\"" "\"\""))))
      (should (equal (sqlite-select gnosis-db "SELECT * FROM thema_tag")
                     '((101 "\"tag\nλ\""))))
      (should (equal (gnosis-select '[id hypothesis answer] 'themata)
                     '((101 ("a" "b" "c") ("A" "B" "C"))
                       (102 nil ("A2")) (103 "" ("nil")))))
      (should (equal (sqlite-select
                      gnosis-db
                      "SELECT thema_id, due_day, suspended FROM scheduler_state
                       ORDER BY thema_id")
                     '((101 20260907 1) (102 20260907 1) (103 20260907 1))))
      (let ((guids (gnosis-anki--build-guid-cache)))
        (should (gethash "raw\"guid" guids))
        (should (gethash "" guids))
        (should (= 2 (hash-table-count guids)))))))

(ert-deftest gnosis-test-printer-anki-fields-have-local-numbering ()
  "Read each imported field without another field's printer reference table."
  (gnosis-test-with-db
    (let* ((answers (list "A" "B" "C"))
           (item (list :type "cloze" :keimenon "Q" :hypothesis answers
                       :answer answers :parathema ""))
           (print-circle t)
           (print-continuous-numbering t)
           (print-number-table nil))
      ;; Make the shared list known to the ambient printer before insertion.
      (prin1-to-string (list answers answers))
      (gnosis-anki--bulk-insert-chunk
       gnosis-db (list item item) '(201 202) 20260907)
      (gnosis-sqlite-close gnosis-db)
      (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
      (dolist (row (sqlite-select gnosis-db
                                 "SELECT hypothesis, answer FROM themata"))
        (dolist (field row)
          (should (equal answers (read field))))))))

(ert-deftest gnosis-test-printer-node-tags-keep-nested-wire ()
  "The node index retains a serialized tag string, including text nil."
  (dolist (settings '(nil ((print-length . 2)) ((print-level . 0))
                         ((print-circle . t)
                          (print-continuous-numbering . t)
                          (print-number-table))))
    (gnosis-test-with-db
      (let ((tags (list "one" "two" "three")))
        (cl-progv (mapcar #'car settings) (mapcar #'cdr settings)
          (prin1-to-string (list tags tags))
          (gnosis-nodes--insert-file-data
           'nodes "fixture.org" "1"
           (list (list :id "tagged" :title "Tagged" :level "0" :tags tags)
                 (list :id "untagged" :title "Untagged" :level "0" :tags nil)
                 nil "fixture-hash")))
        (gnosis-sqlite-close gnosis-db)
        (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
        (should
         (equal (sqlite-select gnosis-db "SELECT tags FROM nodes ORDER BY id")
                '(("\"(\\\"one\\\" \\\"two\\\" \\\"three\\\")\"")
                  ("\"nil\""))))
        (should (equal (gnosis-get 'tags 'nodes '(= id "untagged")) "nil"))
        (should (equal (read (gnosis-get 'tags 'nodes '(= id "tagged"))) tags))
        (should (equal (sort (gnosis-nodes--all-tags) #'string<)
                       '("one" "three" "two")))))))

(ert-deftest gnosis-test-printer-bulk-tags-match-single-inserts ()
  "Batched tag writes use the same wire values as ordinary inserts."
  (gnosis-test-with-db
    (dolist (id '(301 302 303 304))
      (gnosis-test--add-basic-thema "Q" "A" nil nil id))
    (let ((gnosis-sqlite--max-vars 4)
          (print-escape-newlines t)
          (print-escape-multibyte t))
      (gnosis-modify-thema-tags '(301 302 303) '("tag\nλ") '("test"))
      (gnosis--delete 'thema-tag '(= thema-id 304))
      (gnosis--insert-into 'thema-tag '([304 "tag\nλ"])))
    (gnosis-sqlite-close gnosis-db)
    (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
    (should (equal (sqlite-select gnosis-db
                                 "SELECT * FROM thema_tag ORDER BY thema_id")
                   '((301 "\"tag\nλ\"") (302 "\"tag\nλ\"")
                     (303 "\"tag\nλ\"") (304 "\"tag\nλ\""))))
    (should (equal (gnosis-select '[thema-id tag] 'thema-tag)
                   '((301 "tag\nλ") (302 "tag\nλ") (303 "tag\nλ") (304 "tag\nλ"))))
    ;; Read-side matching/removal must agree with both insertion paths.
    (gnosis-modify-thema-tags '(301 302 303 304) nil '("tag\nλ"))
    (should-not (gnosis-select 'tag 'thema-tag))))

(ert-deftest gnosis-test-printer-event-id-preserves-preimage ()
  "Every entropy field reaches SHA256 unchanged under hostile printers."
  (let ((hash (symbol-function 'secure-hash)))
    (dolist (settings '(nil ((print-length . 0)) ((print-level . 1))
                           ((print-integers-as-characters . t))))
      ;; Change each input independently, including a nested timestamp field.
      (dolist (case '((((1 2 3 4) 101 202 303) . "((1 2 3 4) 101 202 303)")
                      (((1 2 3 5) 101 202 303) . "((1 2 3 5) 101 202 303)")
                      (((1 2 3 4) 102 202 303) . "((1 2 3 4) 102 202 303)")
                      (((1 2 3 4) 101 203 303) . "((1 2 3 4) 101 203 303)")
                      (((1 2 3 4) 101 202 304) . "((1 2 3 4) 101 202 304)")))
        (let ((input (car case)) preimage actual)
          (cl-letf (((symbol-function 'current-time) (lambda () (nth 0 input)))
                    ((symbol-function 'emacs-pid) (lambda () (nth 1 input)))
                    ((symbol-function 'random) (lambda (&optional _) (nth 2 input)))
                    ((symbol-function 'user-uid) (lambda () (nth 3 input)))
                    ((symbol-function 'secure-hash)
                     (lambda (algorithm object &rest args)
                       (setq preimage object)
                       (apply hash algorithm object args))))
            (cl-progv (mapcar #'car settings) (mapcar #'cdr settings)
              (setq actual (gnosis-scheduler-event-id))))
          (should (equal preimage (cdr case)))
          (should (equal actual (funcall hash 'sha256 (cdr case)))))))))

(provide 'gnosis-test-printer-callers)
;;; gnosis-test-printer-callers.el ends here
