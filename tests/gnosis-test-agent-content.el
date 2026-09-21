;;; gnosis-test-agent-content.el --- Structured content regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'gnosis-agent-content)
(require 'gnosis-test-helpers)

(defun gnosis-test-content-roundtrip (value)
  "Round trip VALUE through the documented JSON boundary."
  (json-parse-string (decode-coding-string
                      (json-serialize value :false-object :false :null-object nil) 'utf-8)
                     :object-type 'plist :array-type 'array :false-object :false :null-object nil))

(defun gnosis-test-content-change (id &rest fields)
  "Build a current change for decimal ID and FIELDS."
  (append (list :id id :revision
                (plist-get (aref (plist-get (gnosis-agent-content-fetch (vector id)) :items) 0)
                           :revision)) fields))

(defun gnosis-test-content-plan (changes)
  "Preview CHANGES on the current connection."
  (gnosis-agent-content-preview (plist-get (gnosis-agent-content-fetch []) :owner) changes))

(defun gnosis-test-content-source (id &optional journal)
  "Create saved fixture source ID, optionally in JOURNAL index."
  (let* ((file (expand-file-name (concat id ".org") gnosis-dir))
         (text (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: Source\nSaved teaching\n" id)))
    (with-temp-file file (insert text))
    (gnosis--insert-into (if journal 'journal 'nodes)
                        `([,id ,file "Source" "1" nil nil ,(secure-hash 'sha1 text)]))
    file))

(defun gnosis-test-content-evidence ()
  "Read all scheduler, practice and session authority rows."
  (mapcar (lambda (table) (gnosis-select '* table))
          '(scheduler-config scheduler-active scheduler-baseline scheduler-state
            review-events review-voids review-activity-baseline
            practice-events practice-encounters practice-voids study-session study-history)))

(ert-deftest gnosis-content-fetch-cloze-aliases-and-lossless-json ()
  (gnosis-test-with-db
    (gnosis-add-thema-fields "cloze" "See 12.2 and 2 vessels; vein and vein."
                             '("number" "first") '("2" "vein") "[[id:a]] [[id:missing]]"
                             '("ανατομία") 0 '("a" "b") nil 9007199254740993)
    (gnosis-add-thema-fields "basic" "Greek\nquestion" nil '("α") "" nil 0 nil nil 102 '("άλφα"))
    (let* ((result (gnosis-agent-content-fetch ["9007199254740993" "102" "999" "102"]))
           (json (gnosis-test-content-roundtrip result))
           (cloze (aref (plist-get json :items) 0)))
      (should (equal result json))
      (should (equal (plist-get cloze :id) "9007199254740993"))
      (should (equal (plist-get cloze :recall-question) "See 12.2 and (number) vessels; (first) and vein."))
      (should (equal (plist-get cloze :answers) ["2" "vein"]))
      (should (equal (plist-get cloze :source-ids) ["a" "b"]))
      (should (equal (plist-get cloze :authored-source-ids) ["a" "missing"]))
      (should (equal (plist-get (aref (plist-get json :items) 1) :accepted-aliases) ["άλφα"]))
      (should (equal (plist-get (aref (plist-get json :items) 2) :status) "missing"))
      (should (equal (aref (plist-get json :items) 1) (aref (plist-get json :items) 3))))
    (should (equal [] (plist-get (gnosis-agent-content-fetch []) :items)))
    (dolist (ids '([1] ["01"] ["1.0"] ["1e3"] ["-0"] ["-01"] ["+1"] ["-9223372036854775809"] ["9223372036854775808"] ("1")))
      (should-error (gnosis-agent-content-fetch ids) :type 'user-error))))

(ert-deftest gnosis-content-pagination-complete-and-filtered ()
  (gnosis-test-with-db
    (dotimes (n 260)
      (gnosis-add-thema-fields "basic" (format "Item %d" n) nil '("answer") ""
                               '("topic") 0 '("source") nil (+ 100 n)))
    (let (cursor collected)
      (cl-loop for page = (gnosis-test-content-roundtrip
                          (gnosis-agent-content-search :limit 17 :cursor cursor))
               do (setq collected (append collected (mapcar (lambda (r) (plist-get r :id)) (plist-get page :items)))
                        cursor (plist-get page :next))
               while cursor
               finally (should (eq t (plist-get page :complete))))
      (should (equal collected (mapcar #'number-to-string (number-sequence 100 359)))))
    (let (cursor found)
      (cl-loop for page = (gnosis-agent-content-search :text "Item 259" :tag "topic" :source-id "source" :limit 1 :cursor cursor)
               do (setq cursor (plist-get page :next)
                        found (append found (append (plist-get page :items) nil)))
               while cursor)
      (should (equal '("359") (mapcar (lambda (r) (plist-get r :id)) found))))
    (should (equal [] (plist-get (gnosis-agent-content-search :text "not present") :items)))
    (let ((cursor (plist-get (gnosis-agent-content-search :limit 1) :next)))
      (should-error (gnosis-agent-content-search :text "different" :cursor cursor) :type 'user-error)
      (gnosis-modify-thema-tags '(100) '("new") nil)
      (should-error (gnosis-agent-content-search :cursor cursor) :type 'user-error))))

(ert-deftest gnosis-content-vocabulary-empty-and-pages ()
  (gnosis-test-with-db
    (should (equal [] (plist-get (gnosis-agent-content-vocabulary "sources") :items)))
    (gnosis-test--add-basic-thema "A" "a" '("α" "b" "c") nil 101)
    (gnosis--insert-into 'nodes '(["node-a" "a.org" "Alpha" "1" nil nil nil]
                                ["node-b" "b.org" "Beta" "1" nil nil nil]))
    (dolist (kind '("tags" "sources"))
      (let (cursor items)
        (cl-loop for page = (gnosis-test-content-roundtrip (gnosis-agent-content-vocabulary kind :limit 1 :cursor cursor))
                 do (setq items (append items (append (plist-get page :items) nil)) cursor (plist-get page :next))
                 while cursor)
        (should (= (length items) (if (equal kind "tags") 3 2)))))))

(ert-deftest gnosis-content-preview-apply-preserves-retained-evidence ()
  (gnosis-test-with-db
    (gnosis-add-thema-fields "basic" "The vessel supplies α." '("hint") '("a") "[[id:old][Old]]"
                             '("old" "keep") 0 '("old") "image.png" 101 '("alias"))
    (gnosis-test--add-basic-thema "B" "b" nil nil 102)
    (gnosis-test-content-source "node")
    (gnosis-scheduler-accept-review (gnosis-scheduler-event-id) 101 'success 1780000000000000 (gnosis--today-int))
    (let ((gnosis-agent--launches nil))
      (unwind-protect
          (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _) nil)))
            (gnosis-agent-start-practice :thema-ids '(101 102) :limit 2)
            (let ((gnosis-review--state (gnosis-review--read-session)))
              (gnosis-review-result 101 t (gnosis-review-algorithm 101 t)))
            (let* ((evidence (gnosis-test-content-evidence))
                   (before (gnosis-agent-content-fetch ["101" "102"]))
                   (changes (vector (gnosis-test-content-change "101" :add-tags ["new"] :remove-tags ["old"]
                                                               :add-sources [(:id "node" :label "Lecture vessel")])))
                   (plan (gnosis-test-content-plan changes)))
              (should (equal before (gnosis-agent-content-fetch ["101" "102"])))
              (should (equal evidence (gnosis-test-content-evidence)))
              (let* ((receipt (gnosis-agent-content-apply (gnosis-test-content-roundtrip plan)))
                     (record (aref (plist-get receipt :items) 0)))
                (should (equal receipt (gnosis-agent-content-fetch ["101"])))
                (should (equal (plist-get record :tags) ["keep" "new"]))
                (should (equal (plist-get record :source-ids) ["node" "old"]))
                (should (equal (plist-get record :question) "The vessel supplies α."))
                (dolist (field '(:answers :hypothesis :accepted-aliases :review-image :rubric))
                  (should (equal (plist-get record field) (plist-get (aref (plist-get before :items) 0) field)))))
              (should (equal evidence (gnosis-test-content-evidence)))
              (should (equal (aref (plist-get before :items) 1)
                             (aref (plist-get (gnosis-agent-content-fetch ["102"]) :items) 0)))
              (should-error (gnosis-agent-content-apply plan) :type 'user-error)))
        (mapc #'gnosis-agent--release (copy-sequence gnosis-agent--launches))))))

(ert-deftest gnosis-content-stale-foreign-active-and-draft-refusal ()
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "A" "a" nil nil 101)
    (let* ((change (gnosis-test-content-change "101" :add-tags ["new"]))
           (plan (gnosis-test-content-plan (vector change)))
           (before (gnosis-agent-content-fetch ["101"])))
      (let ((gnosis-review--running "encounter"))
        (should (eq t (plist-get (gnosis-agent-content-encounter) :busy)))
        (should-error (gnosis-agent-content-apply plan) :type 'user-error))
      (with-temp-buffer
        (setq-local gnosis--draft-db gnosis-db gnosis--draft-original (cons 101 'snapshot))
        (insert "unsaved learner draft")
        (should-error (gnosis-agent-content-apply plan) :type 'user-error)
        (should (equal "unsaved learner draft" (buffer-string))))
      (let ((gnosis-db (gnosis-sqlite-open gnosis-test--db-file)))
        (unwind-protect (should-error (gnosis-agent-content-apply plan) :type 'user-error)
          (gnosis-sqlite-close gnosis-db)))
      (should (equal before (gnosis-agent-content-fetch ["101"])))
      (gnosis-update 'extras '(= parathema "Changed teaching") '(= id 101))
      (should-error (gnosis-agent-content-apply plan) :type 'user-error)
      (should (equal '((101 "test")) (gnosis-select '* 'thema-tag))))))

(ert-deftest gnosis-content-batch-validation-and-rollback ()
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "A" "a" nil nil 101)
    (gnosis-test--add-basic-thema "B" "b" nil nil 102)
    (let* ((change (gnosis-test-content-change "101" :add-tags ["new"]))
           (other (gnosis-test-content-change "102" :add-tags ["new"]))
           (plan (gnosis-test-content-plan (vector change other)))
           (before (gnosis-agent-content-fetch ["101" "102"]))
           (writer (symbol-function 'gnosis-modify-thema-tags)))
      (should-error (gnosis-test-content-plan (vector change change)) :type 'user-error)
      (dolist (bad '((:unknown 1) (:add-tags [4]) (:add-tags [""]) (:add-tags ["x"] :remove-tags ["x"])
                     (:link-text "A") (:link-text "absent" :link-source-id "missing")))
        (should-error (gnosis-test-content-plan (vector (append (seq-take change 4) bad))) :type 'user-error))
      (cl-letf (((symbol-function 'gnosis-modify-thema-tags)
                 (lambda (ids add remove)
                   (if (= (car ids) 102) (error "Injected second-write failure")
                     (funcall writer ids add remove)))))
        (should-error (gnosis-agent-content-apply plan)))
      (should (equal before (gnosis-agent-content-fetch ["101" "102"])))
      (let ((tampered (copy-tree plan t)))
        (setf (plist-get (aref (plist-get tampered :items) 0) :question) "Tampered")
        (should-error (gnosis-agent-content-apply tampered) :type 'user-error))
      (should (equal [] (plist-get (gnosis-agent-content-apply (gnosis-test-content-plan [])) :items))))))

(ert-deftest gnosis-content-source-disk-hash-id-and-draft-boundaries ()
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (make-temp-file "gnosis-content-nodes" t))
           (file (expand-file-name "note.org" gnosis-nodes-dir))
           (text ":PROPERTIES:\n:ID: root\n:END:\n#+title: Root\nRoot prose\n* First\n:PROPERTIES:\n:ID: first\n:END:\nGreek α\n* Second\n:PROPERTIES:\n:ID: second\n:END:\nHidden sibling\n"))
      (unwind-protect
          (progn
            (with-temp-file file (insert text))
            (gnosis-nodes--update-file file)
            (let ((root (gnosis-agent-content-source "root"))
                  (first (gnosis-agent-content-source "first")))
              (should (equal "found" (plist-get root :status)))
              (should (equal text (plist-get root :text)))
              (should (equal "found" (plist-get first :status)))
              (should (string-search "Greek α" (plist-get first :text)))
              (should-not (string-search "Hidden sibling" (plist-get first :text))))
            (with-temp-buffer
              (setq buffer-file-name file)
              (insert "Private unsaved draft")
              (should (equal "dirty" (plist-get (gnosis-agent-content-source "root") :status)))
              (should (equal "Private unsaved draft" (buffer-string))))
            (with-temp-file file (insert text "Changed disk"))
            (should (equal "stale" (plist-get (gnosis-agent-content-source "root") :status)))
            (delete-file file)
            (should (equal "unavailable" (plist-get (gnosis-agent-content-source "root") :status)))
            (should (equal "missing" (plist-get (gnosis-agent-content-source "unknown") :status))))
        (delete-directory gnosis-nodes-dir t)))))

(ert-deftest gnosis-content-native-encounter-refuses-before-and-after-answer ()
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "A" "a" nil nil 101)
    (gnosis-test-content-source "lecture")
    (gnosis-agent-content-apply
     (gnosis-test-content-plan
      (vector (gnosis-test-content-change "101" :add-sources [(:id "lecture" :label "Lecture citation")]))))
    (let* ((gnosis-agent--launches nil)
           (gnosis-review-buffer-name "*Gnosis Content Native*")
           (gnosis-review-basic-input 'typed)
           (gnosis-center-content nil)
           (gnosis-monkeytype-enable nil)
           (plan (gnosis-test-content-plan (vector (gnosis-test-content-change "101" :add-tags ["new"])))))
      (unwind-protect
          (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _) nil))
                    ((symbol-function 'gnosis--read-string-with-input-method)
                     (lambda (&rest _)
                       (with-current-buffer gnosis-review-buffer-name
                         (should-not (string-search "Lecture citation" (buffer-string))))
                       (let* ((status (gnosis-agent-content-encounter))
                              (item (aref (plist-get status :items) 0)))
                         (should (eq t (plist-get status :busy)))
                         (should (equal "101" (plist-get item :id)))
                         (should (equal "answering" (plist-get item :phase))))
                       (should-error (gnosis-agent-content-apply plan) :type 'user-error)
                       "a"))
                    ((symbol-function 'gnosis-review--read-action)
                     (lambda (&rest _)
                       (with-current-buffer gnosis-review-buffer-name
                         (should (string-search "Lecture citation" (buffer-string))))
                       (should (eq t (plist-get (gnosis-agent-content-encounter) :busy)))
                       (should-error (gnosis-agent-content-apply plan) :type 'user-error)
                       (signal 'quit nil))))
            (gnosis-agent-start-practice :thema-ids '(101) :limit 1)
            (gnosis-agent--launch (car gnosis-agent--launches))
            (should-not (gnosis-select '* 'practice-events))
            (should (equal '((101 "test")) (gnosis-select '* 'thema-tag))))
        (mapc #'gnosis-agent--release (copy-sequence gnosis-agent--launches))
        (when (get-buffer gnosis-review-buffer-name) (kill-buffer gnosis-review-buffer-name))))))

(ert-deftest gnosis-content-large-id-apply-and-source-owner-change ()
  (gnosis-test-with-db
    (gnosis-add-thema-fields "basic" "A vessel" nil '("answer") "" nil 0 nil nil 9007199254740993)
    (gnosis-test-content-source "node")
    (let* ((change (gnosis-test-content-change "9007199254740993" :add-tags ["new"]
                                              :add-sources [(:id "node" :label "Lecture vessel")]))
           (plan (gnosis-test-content-plan (vector change))))
      (gnosis-update 'nodes '(= title "Replacement") '(= id "node"))
      (should-error (gnosis-agent-content-apply plan) :type 'user-error)
      (should-not (gnosis-select '* 'thema-tag))
      (let* ((fresh (gnosis-test-content-plan (vector change)))
             (receipt (gnosis-test-content-roundtrip
                       (gnosis-agent-content-apply (gnosis-test-content-roundtrip fresh)))))
        (should (equal "9007199254740993" (plist-get (aref (plist-get receipt :items) 0) :id)))
        (should (equal '("new") (gnosis-select 'tag 'thema-tag '(= thema-id 9007199254740993) t)))))))

(ert-deftest gnosis-content-external-write-invalidates-cursor ()
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "A" "a" nil nil 101)
    (gnosis-test--add-basic-thema "B" "b" nil nil 102)
    (let ((cursor (plist-get (gnosis-agent-content-search :limit 1) :next)))
      (let ((gnosis-db (gnosis-sqlite-open gnosis-test--db-file)))
        (unwind-protect (gnosis-modify-thema-tags '(102) '("external") nil)
          (gnosis-sqlite-close gnosis-db)))
      (should-error (gnosis-agent-content-search :cursor cursor) :type 'user-error))))

(ert-deftest gnosis-content-empty-collections-and-ambiguous-source ()
  (gnosis-test-with-db
    (should (equal [] (plist-get (gnosis-agent-content-search) :items)))
    (should (eq t (plist-get (gnosis-agent-content-search) :complete)))
    (should-not (plist-get (gnosis-agent-content-search) :next))
    (gnosis--insert-into 'nodes '(["same" "a.org" "Node" "1" nil nil nil]))
    (gnosis--insert-into 'journal '(["same" "j.org" "Journal" "1" nil nil nil]))
    (gnosis--insert-into 'nodes '(["z" "z.org" "Later" "1" nil nil nil]))
    (let* ((page (gnosis-agent-content-vocabulary "sources" :limit 1))
           (item (aref (plist-get page :items) 0))
           (next (gnosis-agent-content-vocabulary "sources" :limit 1 :cursor (plist-get page :next))))
      (should (equal "ambiguous" (plist-get item :kind)))
      (should (equal "z" (plist-get (aref (plist-get next :items) 0) :id)))
      (should (eq t (plist-get next :complete)))
      (should (equal "ambiguous" (plist-get (gnosis-agent-content-source "same") :status))))))

(ert-deftest gnosis-content-signed-native-ids ()
  (gnosis-test-with-db
    (dolist (id '(-9223372036854775808 -1 0 9223372036854775807))
      (gnosis-add-thema-fields "basic" "Q" nil '("A") "" nil 0 nil nil id))
    (should (equal '("-9223372036854775808" "-1" "0" "9223372036854775807")
                   (mapcar (lambda (r) (plist-get r :id))
                           (plist-get (gnosis-agent-content-search) :items))))
    (should (equal 4 (length (plist-get (gnosis-agent-content-fetch
                                       ["-9223372036854775808" "-1" "0" "9223372036854775807"])
                                      :items))))))

(ert-deftest gnosis-content-sources-preserve-cloze-and-correct-associations ()
  (gnosis-test-with-db
    (gnosis-test-content-source "artery")
    (gnosis-test-content-source "journal" t)
    (dolist (type '("cloze" "mc-cloze"))
      (gnosis-add-thema-fields type "The artery supplies artery and vein." nil '("artery" "vein")
                               "Prose [[id:old][Old teaching]] and [[id:keep][Keep]]." nil 0 '("old" "keep") nil 101)
      (gnosis-scheduler-accept-review (gnosis-scheduler-event-id) 101 'success 1780000000000000 (gnosis--today-int))
      (let* ((before (aref (plist-get (gnosis-agent-content-fetch ["101"]) :items) 0))
             (evidence (gnosis-test-content-evidence)))
        ;; Both original reviewer counterexamples now refuse, with no writes.
        (dolist (phrase '("arter" "artery"))
          (should-error (gnosis-test-content-plan
                         (vector (gnosis-test-content-change "101" :link-text phrase :link-source-id "artery")))
                        :type 'user-error))
        (let* ((plan (gnosis-test-content-plan
                      (vector (gnosis-test-content-change
                               "101" :remove-sources ["old"]
                               :add-sources [(:id "artery" :label "Arterial lecture")
                                             (:id "journal" :label "Journal source")]))))
               (after (aref (plist-get (gnosis-agent-content-apply (gnosis-test-content-roundtrip plan)) :items) 0)))
          (should (equal (plist-get before :question) (plist-get after :question)))
          (should (equal (plist-get before :recall-question) (plist-get after :recall-question)))
          (should (equal (plist-get after :source-ids) ["artery" "journal" "keep"]))
          (should (equal (plist-get after :parathema)
                         "Prose Old teaching and [[id:keep][Keep]].\n[[id:artery][Arterial lecture]]\n[[id:journal][Journal source]]"))
          (should (equal (sort (gnosis--thema-expected-links (plist-get after :question) (plist-get after :parathema)) #'string<)
                         (append (plist-get after :source-ids) nil)))
          (gnosis-agent-content-apply
           (gnosis-test-content-plan (vector (gnosis-test-content-change "101" :remove-sources ["artery"])) ))
          (should (equal '("journal" "keep") (sort (gnosis-select 'dest 'thema-links '(= source 101) t) #'string<)))
          (should (equal evidence (gnosis-test-content-evidence)))))
      (gnosis-delete-thema 101 t))))

(ert-deftest gnosis-content-source-stale-dirty-active-and-question-refusal ()
  (gnosis-test-with-db
    (let ((file (gnosis-test-content-source "source")))
      (gnosis-test--add-basic-thema "Question" "Answer" nil nil 101)
      (let* ((change (gnosis-test-content-change "101" :add-sources [(:id "source" :label "Reference")]))
             (plan (gnosis-test-content-plan (vector change)))
             (before (gnosis-agent-content-fetch ["101"])))
        (let ((gnosis-review--running "active")) (should-error (gnosis-agent-content-apply plan) :type 'user-error))
        (with-temp-buffer
          (setq-local gnosis--draft-db gnosis-db gnosis--draft-original '(101 . original))
          (should-error (gnosis-agent-content-apply plan) :type 'user-error))
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert "Unsaved source")
          (should-error (gnosis-agent-content-apply plan) :type 'user-error))
        (with-temp-file file (insert "Changed saved source"))
        (should-error (gnosis-agent-content-apply plan) :type 'user-error)
        (should (equal before (gnosis-agent-content-fetch ["101"])))))
    (gnosis-update 'themata '(= keimenon "[[id:source][Question]]") '(= id 101))
    (should-error (gnosis-test-content-plan (vector (gnosis-test-content-change "101" :remove-sources ["source"]))) :type 'user-error)))

(ert-deftest gnosis-content-search-empty-pages-batch-reads ()
  (gnosis-test-with-db
    (gnosis-sqlite-with-transaction gnosis-db
      (dotimes (n 260)
        (gnosis-add-thema-fields "basic" "Question" nil '("Answer")
                                 (if (= n 259) "Rare α" "ordinary") nil 0 nil nil (1+ n))))
    (let ((reader (symbol-function 'sqlite-select)) (reads 0) cursor ids (pages 0))
      (cl-letf (((symbol-function 'sqlite-select)
                 (lambda (&rest args) (setq reads (1+ reads)) (apply reader args))))
        (cl-loop for page = (gnosis-test-content-roundtrip
                            (gnosis-agent-content-search :text "Rare α" :cursor cursor))
                 do (setq pages (1+ pages) cursor (plist-get page :next)
                          ids (append ids (mapcar (lambda (r) (plist-get r :id)) (plist-get page :items))))
                 when (< pages 3) do (should (equal [] (plist-get page :items)))
                 while cursor))
      (should (= pages 3))
      (should (equal '("260") ids))
      ;; Query count scales with pages plus returned records, not candidates.
      (should (< reads 40)))
    (let ((page (gnosis-agent-content-search :text "absent")))
      (should (eq :false (plist-get page :complete)))
      (should (equal [] (plist-get page :items)))
      (should (equal "128" (plist-get (plist-get page :next) :after))))))

(ert-deftest gnosis-content-source-bounds-and-native-rollback ()
  (gnosis-test-with-db
    (gnosis-test-content-source "source")
    (gnosis-test--add-basic-thema "First" "Answer" nil "Prose" 101)
    (gnosis-test--add-basic-thema "Second" "Answer" nil "Other" 102)
    (let* ((before (gnosis-agent-content-fetch ["101" "102"]))
           (writer (symbol-function 'gnosis-update-thema))
           (plan (gnosis-test-content-plan
                  (vector (gnosis-test-content-change "101" :add-sources [(:id "source" :label "Reference")])
                          (gnosis-test-content-change "102" :add-sources [(:id "source" :label "Reference")])))))
      (dolist (sources (list [(:id "source" :label "Bad]label")]
                            [(:id "missing" :label "Missing")]
                            (make-vector 101 '(:id "source" :label "Too many"))))
        (should-error (gnosis-test-content-plan (vector (gnosis-test-content-change "101" :add-sources sources))) :type 'user-error))
      (let ((gnosis-db (gnosis-sqlite-open gnosis-test--db-file)))
        (unwind-protect (should-error (gnosis-agent-content-apply plan) :type 'user-error)
          (gnosis-sqlite-close gnosis-db)))
      (cl-letf (((symbol-function 'gnosis-update-thema)
                 (lambda (id &rest args)
                   (if (= id 102) (error "Injected content writer failure")
                     (apply writer id args)))))
        (should-error (gnosis-agent-content-apply plan)))
      (should (equal before (gnosis-agent-content-fetch ["101" "102"]))))))

(ert-deftest gnosis-content-removal-refuses-literal-org-without-writes ()
  (dolist (literal '("#+begin_example\n[[id:old][Literal α]]\n#+end_example"
                     "#+begin_src text\n[[id:old][Literal α]]\n#+end_src"
                     "~[[id:old][Literal α]]~"
                     "=[[id:old][Literal α]]="))
    (gnosis-test-with-db
      (let ((text (concat "Teaching [[id:old][Old source]].\n" literal
                          "\nKeep [[id:keep][Other source]].")))
        (with-temp-buffer
          (insert text)
          (delay-mode-hooks (org-mode))
          (should (equal '("old" "keep")
                         (org-element-map (org-element-parse-buffer) 'link
                           (lambda (link) (org-element-property :path link))))))
        (gnosis-add-thema-fields "basic" "Q" nil '("A") text nil 0 '("old" "keep") nil 101)
        (gnosis-test--add-basic-thema "Other" "Answer" nil nil 102)
        (gnosis-scheduler-accept-review (gnosis-scheduler-event-id) 101 'success
                                        1780000000000000 (gnosis--today-int))
        (let* ((before (gnosis-agent-content-fetch ["101" "102"]))
               (evidence (gnosis-test-content-evidence))
               (epoch (gnosis-agent-content--epoch))
               (changes (vector (gnosis-test-content-change "102" :add-tags ["new"])
                                (gnosis-test-content-change "101" :remove-sources ["old"])))
               ;; Apply must also revalidate supplied plans before any writer.
               (plan (gnosis-test-content-plan [])))
          (setf (plist-get plan :changes) changes)
          (dolist (operation (list (lambda () (gnosis-test-content-plan changes))
                                  (lambda () (gnosis-agent-content-apply plan))))
            (should (string-match-p
                     "Cannot safely remove literal"
                     (error-message-string (should-error (funcall operation) :type 'user-error))))
            (should (equal before (gnosis-agent-content-fetch ["101" "102"])))
            (should (equal evidence (gnosis-test-content-evidence)))
            (should (equal epoch (gnosis-agent-content--epoch)))))))))

(ert-deftest gnosis-content-removal-preserves-unrelated-literals-and-index ()
  (gnosis-test-with-db
    (let* ((literal "#+begin_example\n[[id:keep][Literal α]]\n#+end_example\n")
           (text (concat literal "[[id:old][Old α]]  and [[id:old]]; [[id:keep][Keep]].")))
      (gnosis-add-thema-fields "basic" "Q" nil '("A") text nil 0 '("old" "keep" "index-only") nil 101)
      (let* ((before (gnosis-agent-content-fetch ["101"]))
             (plan (gnosis-test-content-plan
                    (vector (gnosis-test-content-change "101" :remove-sources ["old"]))))
             (expected (concat literal "Old α  and ; [[id:keep][Keep]].")))
        (should (equal before (gnosis-agent-content-fetch ["101"])))
        (should (equal expected (plist-get (aref (plist-get plan :items) 0) :parathema)))
        (let ((result (aref (plist-get (gnosis-agent-content-apply
                                       (gnosis-test-content-roundtrip plan)) :items) 0)))
          (should (equal expected (plist-get result :parathema)))
          (should (equal ["index-only" "keep"] (plist-get result :source-ids)))
          (should (equal ["keep"] (plist-get result :authored-source-ids))))))))

(provide 'gnosis-test-agent-content)
;;; gnosis-test-agent-content.el ends here
