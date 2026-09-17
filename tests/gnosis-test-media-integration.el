;;; gnosis-test-media-integration.el --- Media storage and acceptance -*- lexical-binding: t; -*-

(require 'ert)
(require 'gnosis-test-db-safety)
(require 'gnosis-test-helpers)
(require 'gnosis-review)

(defun gnosis-test-media--old-database (&optional archive)
  "Create released schema 8, optionally retaining ARCHIVE metadata."
  (gnosis-test-safety-v8)
  (let ((gnosis-db (gnosis-sqlite-open (expand-file-name "gnosis.db" gnosis-dir))))
    (unwind-protect
        (progn
          (when archive
            (sqlite-execute gnosis-db "ALTER TABLE themata ADD COLUMN archived_at_us INTEGER")
            (sqlite-execute gnosis-db "UPDATE themata SET archived_at_us = 42 WHERE id = 1"))
          (sqlite-execute gnosis-db
            "CREATE TABLE retained_labels (id TEXT PRIMARY KEY, value TEXT) WITHOUT ROWID")
          (sqlite-execute gnosis-db "INSERT INTO retained_labels VALUES ('a', 'kept')")
          (sqlite-execute gnosis-db
            "CREATE TRIGGER retained_labels_no_update BEFORE UPDATE ON retained_labels
             BEGIN SELECT RAISE(ABORT, 'retained label'); END"))
      (sqlite-close gnosis-db))))

(defun gnosis-test-media--rows (db)
  "Return named column projections and rows of all tables in DB."
  (mapcar
   (lambda (row)
     (let* ((table (car row))
            (columns (mapcar #'cadr (sqlite-select db (format "PRAGMA table_info(%s)" table))))
            (projection (mapconcat #'identity columns ", ")))
       (list table projection (sqlite-select db (format "SELECT %s FROM %s ORDER BY 1" projection table)))))
   (sqlite-select db "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")))

(ert-deftest gnosis-media-schema-fresh-alias-column-and-old-writer ()
  (gnosis-test-safety
    (gnosis--ensure-db)
    (should (= 11 (gnosis--db-version)))
    (should (equal '("accepted_aliases" "TEXT" 0 nil 0)
                   (cdr (assoc 6 (sqlite-select gnosis-db "PRAGMA table_info(themata)")))))
    (gnosis--insert-into 'themata '([900 "basic" "Q" ("") ("A") "source"]))
    (should-not (gnosis-get 'accepted-aliases 'themata '(= id 900)))
    (should (equal "source" (gnosis-get 'source-guid 'themata '(= id 900))))))

(ert-deftest gnosis-media-schema-upgrade-preserves-all-retained-columns ()
  (progn
    (dolist (archive '(nil t))
      (gnosis-test-safety
        (gnosis-test-media--old-database archive)
        (let* ((file (expand-file-name "gnosis.db" gnosis-dir))
               (raw (sqlite-open file))
               (trigger (sqlite-select raw
                         "SELECT sql FROM sqlite_master WHERE name = 'retained_labels_no_update'"))
               (before (unwind-protect (gnosis-test-media--rows raw) (sqlite-close raw))))
          (gnosis--ensure-db)
          (should (= 11 (gnosis--db-version)))
          (dolist (entry (seq-remove
                          (lambda (row) (member (car row)
                            '("review" "review_log" "activity_log"))) before))
            (should (equal (nth 2 entry)
                           (sqlite-select gnosis-db
                             (format "SELECT %s FROM %s ORDER BY 1" (nth 1 entry) (car entry))))))
          (should (equal trigger (sqlite-select gnosis-db
                                  "SELECT sql FROM sqlite_master WHERE name = 'retained_labels_no_update'")))
          (should-error (sqlite-execute gnosis-db "UPDATE retained_labels SET value = 'lost'"))
          (should-not (gnosis-get 'accepted-aliases 'themata '(= id 1)))
          (when archive (should (= 42 (gnosis-get 'archived-at-us 'themata '(= id 1)))))
          (sqlite-close gnosis-db)
          (setq gnosis-db nil)
          (should (gnosis--ensure-db)))))))

(ert-deftest gnosis-media-schema-v8-upgrade-failure-is-atomic ()
  (dolist (fault '(error quit))
    (gnosis-test-safety
      (gnosis-test-media--old-database t)
      (let* ((file (expand-file-name "gnosis.db" gnosis-dir))
             (before (gnosis-test-safety-snapshot file))
             (setter (symbol-function 'gnosis--db-set-version)))
        (cl-letf (((symbol-function 'gnosis--db-set-version)
                   (lambda (version)
                     (if (= version 9) (signal fault '("after ALTER"))
                       (funcall setter version)))))
          (should (condition-case nil (progn (gnosis--ensure-db) nil)
                    ((error quit) t))))
        (should-not gnosis-db)
        (should (equal before (gnosis-test-safety-snapshot file)))
        (should (gnosis--ensure-db))
        (should (= 11 (gnosis--db-version)))))))

(ert-deftest gnosis-media-schema-rejects-alias-column-drift-before-writes ()
  (gnosis-test-safety
    (gnosis--ensure-db)
    (sqlite-execute gnosis-db "ALTER TABLE themata DROP COLUMN accepted_aliases")
    (sqlite-execute gnosis-db "ALTER TABLE themata ADD COLUMN accepted_aliases TEXT NOT NULL DEFAULT 'bad'")
    (sqlite-close gnosis-db)
    (setq gnosis-db nil)
    (let* ((file (expand-file-name "gnosis.db" gnosis-dir))
           (before (gnosis-test-safety-snapshot file)))
      (dotimes (_ 2)
        (should-error (gnosis--ensure-db))
        (should-not gnosis-db)
        (should (equal before (gnosis-test-safety-snapshot file)))))))

(ert-deftest gnosis-media-alias-creation-update-and-explicit-clear ()
  (gnosis-test-safety
    (gnosis--ensure-db)
    (let* ((aliases '("ATFL" "Anterior talofibular lig."))
           (before (copy-tree aliases)))
      (gnosis-add-thema-fields "basic" "Name" '("") '("Anterior talofibular ligament")
                              "Context" '("anatomy") 0 nil nil 900 aliases)
      (should (equal before aliases))
      (should (equal before (gnosis-get 'accepted-aliases 'themata '(= id 900))))
      (gnosis-update-thema 900 "Name revised" '("") '("Anterior talofibular ligament")
                           "Context" '("anatomy") nil "basic")
      (should (equal before (gnosis-get 'accepted-aliases 'themata '(= id 900))))
      (gnosis-update-thema 900 "Name revised" '("") '("Anterior talofibular ligament")
                           "Context" '("anatomy") nil "basic" nil)
      (should-not (gnosis-get 'accepted-aliases 'themata '(= id 900))))))

(ert-deftest gnosis-media-alias-invalid-type-and-values-do-not-mutate ()
  (gnosis-test-safety
    (gnosis--ensure-db)
    (gnosis-add-thema-fields "basic" "Name" '("") '("A") "" nil 0 nil nil 900 '("Alias"))
    (let ((before (gnosis-test-media--rows gnosis-db)))
      (dolist (bad '("Alias" (nil) ("") ("   ") ("first\nsecond") ("a" . "b")))
        (should-error (gnosis-update-thema 900 "Q" '("") '("A") "" nil nil "basic" bad)))
      (should-error (gnosis-update-thema 900 "Q" '("A" "B") '("A") "" nil nil "mcq"))
      (should-error (gnosis-add-thema-fields "mcq" "Q" '("A" "B") '("A")
                                            "" nil 0 nil nil 901 '("Alias")))
      (should (equal before (gnosis-test-media--rows gnosis-db))))))

(ert-deftest gnosis-media-alias-dispatch-omission-and-full-replacement ()
  (gnosis-test-safety
    (gnosis--ensure-db)
    (gnosis-add-thema--basic "NEW" "basic" "Q" '("") '("Answer") "" nil 0 nil '("Alias"))
    (let ((id (car (gnosis-select 'id 'themata nil t))))
      (gnosis-add-thema--basic (number-to-string id) "basic" "Q2" '("") '("Answer") "" nil 0 nil)
      (should (equal '("Alias") (gnosis-get 'accepted-aliases 'themata `(= id ,id))))
      (gnosis-add-thema--basic (number-to-string id) "basic" "Q3" '("") '("Answer") "" nil 0 nil nil)
      (should-not (gnosis-get 'accepted-aliases 'themata `(= id ,id))))))

(ert-deftest gnosis-media-alias-double-is-forward-only ()
  (gnosis-test-safety
    (gnosis--ensure-db)
    (gnosis-add-thema--double "NEW" "double" "Question" '("") '("Answer") "" nil 0 nil '("Alias"))
    (should (equal '("Alias") (gnosis-get 'accepted-aliases 'themata '(= keimenon "Question"))))
    (should-not (gnosis-get 'accepted-aliases 'themata '(= keimenon "Answer")))))

(ert-deftest gnosis-media-model-name-public-creation-dispatch ()
  (let (calls)
    (cl-letf (((symbol-function 'gnosis-add-model-thema)
               (lambda (&optional type) (push type calls))))
      (gnosis-add-model-name-thema)
      (gnosis-add-thema "model-name"))
    (should (equal '("model-name" "model-name") calls))))

(defmacro gnosis-test-media--presentation (&rest body)
  "Run BODY without terminal display effects; keep real review/data operations."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'gnosis-display-image) #'ignore)
             ((symbol-function 'gnosis-display-keimenon) #'ignore)
             ((symbol-function 'gnosis-display-hint) #'ignore)
             ((symbol-function 'gnosis-display-basic-answer) #'ignore)
             ((symbol-function 'gnosis-display-parathema) #'ignore)
             ((symbol-function 'gnosis-display-next-review) #'ignore))
     ,@body))

(ert-deftest gnosis-media-alias-review-uses-normal-binary-acceptance ()
  (dolist (mode '(scheduled practice))
    (gnosis-test-safety
      (gnosis--ensure-db)
      (gnosis-add-thema-fields "basic" "Name" '("") '("Anterior talofibular ligament")
                              "" nil 0 nil nil 900 '("ATFL"))
      (with-temp-buffer
        (gnosis-mode)
        (setq gnosis-review--state
              (if (eq mode 'practice)
                  (gnosis-review--reserve-practice '(900) (gnosis-review-practice-policy) nil)
                (gnosis-review-state-create :mode mode :database gnosis-db
                  :initial '(900) :remaining '(900) :total 1 :reviewed 0)))
        (let ((gnosis-string-difference 0) (gnosis-review-basic-input 'typed)
              (before (sqlite-select gnosis-db "SELECT * FROM scheduler_state ORDER BY thema_id")))
          (gnosis-test-media--presentation
            (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                       (lambda (_prompt canonical)
                         (should (equal canonical "Anterior talofibular ligament")) "ATFL")))
              (let ((answer (gnosis-review-basic 900)))
                (should (car answer))
                (gnosis-review--write-result 900 (car answer) (cdr answer)))))
          (if (eq mode 'practice)
              (progn
                (should (equal before (sqlite-select gnosis-db "SELECT * FROM scheduler_state ORDER BY thema_id")))
                (should (= 1 (caar (sqlite-select gnosis-db "SELECT count(*) FROM practice_events"))))
                (should (= 0 (caar (sqlite-select gnosis-db "SELECT count(*) FROM review_events")))))
            (should (= 1 (caar (sqlite-select gnosis-db "SELECT count(*) FROM review_events"))))))))))

(ert-deftest gnosis-media-alias-rule-drift-during-input-and-before-acceptance ()
  (dolist (stage '(input pending))
    (gnosis-test-safety
      (gnosis--ensure-db)
      (gnosis-add-thema-fields "basic" "Name" '("") '("Anterior talofibular ligament")
                              "" nil 0 nil nil 900 '("ATFL"))
      (with-temp-buffer
        (gnosis-mode)
        (setq gnosis-review--state (gnosis-review-state-create
                                    :mode 'scheduled :database gnosis-db
                                    :initial '(900) :remaining '(900) :total 1 :reviewed 0))
        (let ((gnosis-string-difference 0) (gnosis-review-basic-input 'typed))
          (gnosis-test-media--presentation
            (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                       (lambda (&rest _)
                         (when (eq stage 'input)
                           (gnosis-update 'themata '(= accepted-aliases ("Other")) '(= id 900)))
                         "ATFL")))
              (if (eq stage 'input)
                  (should-error (gnosis-review-basic 900))
                (let* ((answer (gnosis-review-basic 900))
                       (override (gnosis-review--override-result (cdr answer) nil)))
                  (gnosis-update 'themata '(= accepted-aliases ("Other")) '(= id 900))
                  (should-error (gnosis-review--write-result 900 nil override))))))
          (should (= 0 (caar (sqlite-select gnosis-db "SELECT count(*) FROM review_events")))))))))

(ert-deftest gnosis-media-image-policy-alias-and-binary-acceptance ()
  (require 'gnosis-image-test-support)
  (dolist (mode '(scheduled practice))
    (dolist (policy '("hide-target" "hide-all"))
      (gnosis-test-safety
        (gnosis--ensure-db)
        (let ((reference (gnosis-image-import
                          (gnosis-test-image--file) gnosis-test-image-targets--regions)))
          (gnosis-add-thema-fields "image-occlusion" "Name the hidden artery"
                                  (list reference "artery" policy) '("Anterior artery")
                                  "Explanation" nil 0 nil nil 900 '("AA")))
        (with-temp-buffer
          (gnosis-mode)
          (setq-local gnosis-review-buffer-name (buffer-name))
          (setq gnosis-review--state
                (if (eq mode 'practice)
                    (gnosis-review--reserve-practice '(900) (gnosis-review-practice-policy) nil)
                  (gnosis-review-state-create :mode mode :database gnosis-db
                    :initial '(900) :remaining '(900) :total 1 :reviewed 0)))
          (let ((gnosis-string-difference 0) rendered
                (before (sqlite-select gnosis-db "SELECT * FROM scheduler_state ORDER BY thema_id")))
            (cl-letf (((symbol-function 'gnosis-image--decode) #'ignore)
                      ((symbol-function 'image-type-available-p) (lambda (_) t))
                      ((symbol-function 'svg-image)
                       (lambda (svg &rest _) (setq rendered svg) '(image :type svg)))
                      ((symbol-function 'gnosis--read-string-with-input-method)
                       (lambda (_prompt canonical)
                         (should (equal canonical "Anterior artery"))
                         (should (= (if (equal policy "hide-all") 3 2)
                                    (length (seq-filter
                                             (lambda (rect) (equal (dom-attr rect 'fill) "#202020"))
                                             (dom-by-tag rendered 'rect)))))
                         "AA")))
              (let ((answer (gnosis-review-image-occlusion 900)))
                (should (car answer))
                (should-not (dom-by-tag rendered 'text))
                (should-not (dom-by-tag rendered 'rect))
                (gnosis-review--write-result 900 (car answer) (cdr answer))))
            (if (eq mode 'practice)
                (progn
                  (should (equal before (sqlite-select gnosis-db "SELECT * FROM scheduler_state ORDER BY thema_id")))
                  (should (= 1 (caar (sqlite-select gnosis-db "SELECT count(*) FROM practice_events")))))
              (should (= 1 (caar (sqlite-select gnosis-db "SELECT count(*) FROM review_events")))))))))))

(ert-deftest gnosis-media-model-find-and-name-target-acceptance ()
  (require 'gnosis-model-test-support)
  (dolist (mode '(scheduled practice))
    (dolist (case '(("model" "whole" 0 (8 4 0) nil t)
                    ("model" "tip" 0 (8 4 0) nil t)
                    ("model" "tip" 1 (2 8 0) nil nil)
                    ("model" "patch" 1 (2 8 0) nil t)
                    ("model" "patch" 0 (8 4 0) nil nil)
                    ("model-name" "whole" nil nil "Alias" t)
                    ("model-name" "tip" nil nil "Alias" t)
                    ("model-name" "patch" nil nil "Wrong" nil)))
      (gnosis-test-safety
        (gnosis--ensure-db)
        (pcase-let* ((`(,type ,target ,face ,point ,input ,success) case)
                     (name (equal type "model-name"))
                     (resource (gnosis-model-import (gnosis-test-model-targets--fixture))))
          (gnosis-add-thema-fields type "Inspect the target"
                                  (if name (list resource target "0" "0" "1")
                                    (list resource "0" "0" "1"))
                                  (if name '("Authored name") (list target))
                                  "Explanation" nil 0 nil nil 900 (and name '("Alias")))
          (save-window-excursion
            (with-temp-buffer
              (gnosis-mode)
              (setq-local gnosis-review-buffer-name (buffer-name))
              (setq gnosis-review--state
                    (if (eq mode 'practice)
                        (gnosis-review--reserve-practice '(900) (gnosis-review-practice-policy) nil)
                      (gnosis-review-state-create :mode mode :database gnosis-db
                        :initial '(900) :remaining '(900) :total 1 :reviewed 0)))
              (let ((gnosis-string-difference 0) (depth 0) shown
                    (before (sqlite-select gnosis-db "SELECT * FROM scheduler_state ORDER BY thema_id")))
                (cl-letf (((symbol-function 'gnosis-model-open)
                           (lambda (path view &optional size inline question-target _verified)
                             (setq shown question-target)
                             (gnosis-test-model--canvas path view size inline)))
                          ((symbol-function 'gnosis-model--canvas-size) (lambda () 400))
                          ((symbol-function 'recursion-depth) (lambda () depth))
                          ((symbol-function 'exit-recursive-edit) #'ignore)
                          ((symbol-function 'gnosis--read-string-with-input-method)
                           (lambda (_prompt canonical)
                             (should (equal canonical "Authored name")) input))
                          ((symbol-function 'recursive-edit)
                           (lambda ()
                             (setq depth 1)
                             (gnosis-test-model--wait-for-preparation)
                             (if name
                                 (progn
                                   (should (equal shown target))
                                   (should-not (string-match-p "Authored name" (gnosis-review--model-header))))
                               (should-not shown)
                               (setq-local canvas-3d-selected-id "mesh")
                               (setq-local canvas-3d--selection
                                           (list :mesh "mesh" :id "mesh" :face face :point point
                                                 :frame 1 :owner canvas-3d--process))
                               (gnosis-review--model-selection canvas-3d--selection))
                             (gnosis-review-model-submit))))
                  (let* ((display (gnosis-review--display-thema 900))
                         (answer (cadr display)))
                    (should (eq success (car answer)))
                    (gnosis-review--write-result 900 success (cdr answer))))
                (if (eq mode 'practice)
                    (progn
                      (should (equal before (sqlite-select gnosis-db "SELECT * FROM scheduler_state ORDER BY thema_id")))
                      (should (= 1 (caar (sqlite-select gnosis-db "SELECT count(*) FROM practice_events")))))
                  (should (= 1 (caar (sqlite-select gnosis-db "SELECT count(*) FROM review_events")))))))))))))

(defun gnosis-test-media--durable-state (mode)
  "Create a durable encounter for thema 900 in MODE."
  (if (eq mode 'practice)
      (gnosis-review--reserve-practice '(900) (gnosis-review-practice-policy) nil)
    (let ((state (gnosis-review-state-create
                  :mode 'scheduled :database gnosis-db :persistent-p t
                  :session-id "media-owned-session" :event-id (gnosis-scheduler-event-id)
                  :initial '(900) :remaining '(900) :total 1 :reviewed 0)))
      (gnosis-review--save-session state)
      state)))

(ert-deftest gnosis-media-name-recursive-owner-faults ()
  (require 'gnosis-model-test-support)
  (dolist (mode '(scheduled practice))
    (dolist (fault '(aliases frame retired-session mode quit renderer-death successor))
      (gnosis-test-safety
        (gnosis--ensure-db)
        (let ((resource (gnosis-model-import (gnosis-test-model-targets--fixture))))
          (gnosis-add-thema-fields "model-name" "Q" (list resource "tip" "0" "0" "1")
                                  '("Canonical") "" nil 0 nil nil 900 '("Alias")))
        (save-window-excursion
          (with-temp-buffer
            (gnosis-mode)
            (setq-local gnosis-review-buffer-name (buffer-name))
            (setq-local gnosis-review--state (gnosis-test-media--durable-state mode))
            (let ((depth 0) process successor canonical-seen fault-reached)
              (unwind-protect
                  (progn
                    ;; Only renderer/input boundaries are doubled.  Submit,
                    ;; owner validation, cancellation and retirement stay real.
                    (cl-letf (((symbol-function 'gnosis-model-open)
                               (lambda (&rest args)
                                 (prog1 (apply #'gnosis-test-model--canvas args)
                                   (setq process canvas-3d--process))))
                              ((symbol-function 'gnosis-model--canvas-size) (lambda () 400))
                              ((symbol-function 'recursion-depth) (lambda () depth))
                              ((symbol-function 'exit-recursive-edit) #'ignore)
                              ((symbol-function 'abort-recursive-edit) (lambda () (signal 'quit nil)))
                              ((symbol-function 'gnosis--read-string-with-input-method)
                               (lambda (_prompt canonical)
                                 (setq canonical-seen canonical fault-reached t)
                                 (pcase fault
                                   ('aliases (gnosis-update 'themata '(= accepted-aliases ("Other")) '(= id 900)))
                                   ('frame (setq canvas-3d--frame nil))
                                   ('retired-session (sqlite-execute gnosis-db "DELETE FROM study_session"))
                                   ('mode (fundamental-mode))
                                   ('quit (signal 'quit nil))
                                   ('renderer-death (delete-process process))
                                   ('successor
                                    (delete-process process)
                                    (setq successor (make-pipe-process :name "gnosis-name-successor" :noquery t)
                                          canvas-3d--process successor)))
                                 "Alias"))
                              ((symbol-function 'recursive-edit)
                               (lambda () (setq depth 1)
                                  (gnosis-test-model--wait-for-preparation)
                                  (gnosis-review-model-submit))))
                      (should (condition-case nil (progn (gnosis-review-model-name 900) nil)
                                ((error quit) t))))
                    ;; Assert outside the expected-error boundary: an early
                    ;; setup error must not masquerade as a guarded input fault.
                    (should fault-reached)
                    (should (equal "Canonical" canonical-seen))
                    (should (processp process))
                    (should-not (process-live-p process))
                    (should-not gnosis-review--model-context)
                    (when successor
                      (should (process-live-p successor))
                      (should (eq canvas-3d--process successor)))
                    (should (= 0 (caar (sqlite-select gnosis-db "SELECT count(*) FROM review_events"))))
                    (should (= 0 (caar (sqlite-select gnosis-db "SELECT count(*) FROM practice_events")))))
                (when (process-live-p successor) (delete-process successor))
                (when (process-live-p process) (delete-process process))))))))))

(ert-deftest gnosis-media-name-pending-rule-drift ()
  (require 'gnosis-model-test-support)
  (dolist (mode '(scheduled practice))
    (gnosis-test-safety
      (gnosis--ensure-db)
      (let ((resource (gnosis-model-import (gnosis-test-model-targets--fixture))))
        (gnosis-add-thema-fields "model-name" "Q" (list resource "patch" "0" "0" "1")
                                '("Canonical") "" nil 0 nil nil 900 '("Alias")))
      (save-window-excursion
        (with-temp-buffer
          (gnosis-mode)
          (setq-local gnosis-review-buffer-name (buffer-name))
          (setq-local gnosis-review--state (gnosis-test-media--durable-state mode))
          (let ((depth 0) process)
            (cl-letf (((symbol-function 'gnosis-model-open)
                       (lambda (&rest args)
                         (prog1 (apply #'gnosis-test-model--canvas args)
                           (setq process canvas-3d--process))))
                      ((symbol-function 'gnosis-model--canvas-size) (lambda () 400))
                      ((symbol-function 'recursion-depth) (lambda () depth))
                      ((symbol-function 'exit-recursive-edit) #'ignore)
                      ((symbol-function 'gnosis--read-string-with-input-method) (lambda (&rest _) "Alias"))
                      ((symbol-function 'recursive-edit) (lambda () (setq depth 1)
                                  (gnosis-test-model--wait-for-preparation)
                                  (gnosis-review-model-submit))))
              (let* ((answer (cadr (gnosis-review--display-thema 900)))
                     (result (gnosis-review--override-result (cdr answer) nil)))
                (should (car answer))
                (should-not (process-live-p process))
                (should-not gnosis-review--model-context)
                (sqlite-execute gnosis-db "UPDATE themata SET accepted_aliases = NULL WHERE id = 900")
                (let ((before (gnosis-test-media--rows gnosis-db)))
                  (should-error (gnosis-review-result 900 nil result) :type 'user-error)
                  (should (equal before (gnosis-test-media--rows gnosis-db))))
                (should (= 0 (caar (sqlite-select gnosis-db "SELECT count(*) FROM review_events"))))
                (should (= 0 (caar (sqlite-select gnosis-db "SELECT count(*) FROM practice_events"))))))))))))

(provide 'gnosis-test-media-integration)
;;; gnosis-test-media-integration.el ends here
