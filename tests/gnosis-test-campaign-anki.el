;;; gnosis-test-campaign-anki.el --- Import text fidelity -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Public imports preserve visible HTML text and render every cloze blank.

;;; Code:

(require 'ert)
(require 'gnosis-test-anki-models)
(require 'gnosis-review)

(defun gnosis-test-campaign-anki--retain ()
  "Create unrelated content with nonempty scheduled and practice evidence."
  (let ((id (gnosis-test--add-basic-thema "Retained *question*" "&lt;")))
    (gnosis-scheduler-accept-review (make-string 64 ?a) id 'success
                                    1000000 (gnosis--today-int))
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO practice_events VALUES ('retained-practice', ?, 'retained-session', 1, 100, 3)"
     (list id))
    id))

(defun gnosis-test-campaign-anki--facts (id)
  "Return retained content and study facts for ID."
  (append
   (mapcar (lambda (table)
             (sqlite-select gnosis-db
                            (format "SELECT * FROM %s WHERE id=?" table)
                            (list id)))
           '(themata extras))
   (mapcar (lambda (table)
             (sqlite-select gnosis-db
                            (format "SELECT * FROM %s WHERE thema_id=? ORDER BY 1" table)
                            (list id)))
           '(thema_tag scheduler_state scheduler_baseline))
   (mapcar (lambda (table)
             (sqlite-select gnosis-db (format "SELECT * FROM %s ORDER BY 1" table)))
           '(review_events practice_events practice_encounters study_session study_history))))

(defun gnosis-test-campaign-anki--row ()
  "Return the imported question, answers, hints and explanation."
  (car (gnosis-sqlite-select
        gnosis-db
        "SELECT t.keimenon,t.answer,t.hypothesis,e.parathema
         FROM themata t JOIN extras e USING(id) WHERE source_guid='guid7'")))

(defun gnosis-test-campaign-anki--render (row)
  "Assert every answer in ROW is masked with its intended hint."
  (let ((gnosis-center-content nil)
        (gnosis-latex-preview nil))
    (with-temp-buffer
      (let ((gnosis-review--display-buffer (current-buffer)))
        (should
         (equal (gnosis-display-cloze-string
                 (nth 0 row) (nth 1 row) (nth 2 row) nil nil
                 (vconcat (number-sequence 0 (1- (length (nth 1 row))))))
                (nth 2 row)))
        (dolist (answer (nth 1 row))
          (should-not (string-search answer (buffer-string))))))))

(ert-deftest gnosis-test-campaign-anki-partial-emphasis ()
  "Mask partial emphasis without changing outside or whole-answer formatting."
  (dolist (schema '(legacy modern))
    (dolist (case '(("heart muscle" "heart muscle" "heart muscle")
                    ("<b>heart muscle</b>" "*heart muscle*" "heart muscle")
                    ("<b>heart</b> muscle" "heart muscle" "heart muscle")
                    ("heart <i>muscle</i>" "heart muscle" "heart muscle")
                    ("<u>heart</u> muscle" "heart muscle" "heart muscle")
                    ("<b><i>heart</i></b> muscle" "heart muscle" "heart muscle")
                    ("<b>*heart*</b> /muscle/" "*heart* /muscle/" "*heart* /muscle/")))
      (ert-info ((format "%S %S" schema case))
        (gnosis-test-with-db
          (let* ((id (gnosis-test-campaign-anki--retain))
                 (before (gnosis-test-campaign-anki--facts id))
                 (source (expand-file-name "cloze.anki2" gnosis-dir)))
            (gnosis-test-anki-models--source
             source schema '("Text" "Extra")
             (list (concat "<b>The</b> {{c1::" (car case)
                           "::tissue}} and {{c1::<i>smooth</i> muscle::second}} contract")
                   "<i>Explanation</i>")
             1 '(("{{cloze:Text}}" . "{{cloze:Text}}{{Extra}}")))
            (let ((hash (gnosis-test-anki-models--hash source)))
              (dotimes (_ 2)
                (gnosis-test-anki-models--public source "" nil)
                (let ((row (gnosis-test-campaign-anki--row)))
                  (should (equal row
                                 (list (concat "*The* " (nth 1 case)
                                               " and smooth muscle contract")
                                       (list (nth 2 case) "smooth muscle")
                                       '("tissue" "second") "/Explanation/")))
                  (gnosis-test-campaign-anki--render row))
                (should (= 2 (length (gnosis-select 'id 'themata))))
                (should (equal before (gnosis-test-campaign-anki--facts id)))
                (should (equal hash (gnosis-test-anki-models--hash source)))))))))))

(ert-deftest gnosis-test-campaign-anki-single-entity-decoding ()
  "Decode basic and cloze fields once in both schemas, surviving reopen."
  ;; These expected strings are literal HTML semantics, not converter output.
  (dolist (schema '(legacy modern))
    (dolist (kind '(basic cloze))
      (dolist (case '(("&lt;" "<") ("&amp;lt;" "&lt;")
                      ("&gt;" ">") ("&amp;gt;" "&gt;")
                      ("&quot;" "\"") ("&amp;quot;" "&quot;")
                      ("&amp;" "&") ("&amp;amp;" "&amp;")
                      ("&nbsp;" " ") ("&amp;nbsp;" "&nbsp;")))
        (ert-info ((format "%S %S %S" schema kind case))
          (gnosis-test-with-db
            (let* ((id (gnosis-test-campaign-anki--retain))
                   (before (gnosis-test-campaign-anki--facts id))
                   (source (expand-file-name "entities.anki2" gnosis-dir))
                   (raw (concat "value " (car case) " end"))
                   (expected (concat "value " (cadr case) " end")))
              (if (eq kind 'basic)
                  (gnosis-test-anki-models--source
                   source schema '("Front" "Back" "Extra")
                   (list raw raw raw) 0 '(("{{Front}}" . "{{Back}}{{Extra}}")))
                (gnosis-test-anki-models--source
                 source schema '("Text" "Extra")
                 (list (concat "{{c1::" raw "::literal}}") raw)
                 1 '(("{{cloze:Text}}" . "{{cloze:Text}}{{Extra}}"))))
              (let ((hash (gnosis-test-anki-models--hash source)))
                (gnosis-test-anki-models--public source "" nil)
                (gnosis-sqlite-close gnosis-db)
                (setq gnosis-db (gnosis-db--open gnosis-dir))
                (let ((row (gnosis-test-campaign-anki--row)))
                  (should (equal row (list expected (list expected)
                                          (if (eq kind 'basic) '("") '("literal"))
                                          expected)))
                  (should (gnosis-answer-match-p (car (nth 1 row)) expected nil 0))
                  (when (eq kind 'cloze) (gnosis-test-campaign-anki--render row)))
                (should (equal hash (gnosis-test-anki-models--hash source)))
                (should (equal before (gnosis-test-campaign-anki--facts id)))))))))))

(ert-deftest gnosis-test-campaign-anki-failure-retry ()
  "Roll back partially written cloze siblings on error or quit, then retry."
  (dolist (schema '(legacy modern))
    (dolist (fault '(error quit))
      (gnosis-test-with-db
        (let* ((id (gnosis-test-campaign-anki--retain))
               (before (gnosis-test-campaign-anki--facts id))
               (source (expand-file-name "retry.anki2" gnosis-dir))
               (gnosis-anki--chunk-size 1)
               (writer (symbol-function 'gnosis-anki--bulk-insert-chunk))
               (writes 0))
          (gnosis-test-anki-models--source
           source schema '("Text" "Extra")
           '("{{c1::<b>heart</b> muscle::tissue}} {{c2::value &amp;lt; end::literal}}" "Extra")
           1 '(("{{cloze:Text}}" . "{{cloze:Text}}{{Extra}}")))
          (let ((hash (gnosis-test-anki-models--hash source)))
            (cl-letf (((symbol-function 'gnosis-anki--bulk-insert-chunk)
                       (lambda (&rest args)
                         (apply writer args)
                         (when (= (cl-incf writes) 2)
                           (signal fault '("Controlled sibling failure"))))))
              (condition-case err
                  (gnosis-test-anki-models--public source "" nil)
                (quit (should (equal err '(quit "Controlled sibling failure"))))))
            (should (= writes 2))
            (should-not (gnosis-test-campaign-anki--row))
            (should (equal before (gnosis-test-campaign-anki--facts id)))
            (dotimes (_ 2)
              (gnosis-test-anki-models--public source "" nil)
              (should (= 3 (length (gnosis-select 'id 'themata))))
              (dolist (row (gnosis-sqlite-select
                           gnosis-db "SELECT keimenon,answer,hypothesis FROM themata
                                      WHERE source_guid='guid7'"))
                (gnosis-test-campaign-anki--render row))
              (should (equal before (gnosis-test-campaign-anki--facts id)))
              (should (equal hash (gnosis-test-anki-models--hash source))))))))))

(ert-deftest gnosis-test-campaign-anki-unrenderable-group ()
  "Skip an entire unrenderable group, retaining its independent sibling."
  (dolist (schema '(legacy modern))
    (gnosis-test-with-db
      (let* ((id (gnosis-test-campaign-anki--retain))
             (before (gnosis-test-campaign-anki--facts id))
             (source (expand-file-name "unrenderable.anki2" gnosis-dir)))
        ;; The unmatched HTML opener joins surrounding text into a tag after
        ;; deletion assembly.  Never persist a blank absent from the question.
        (gnosis-test-anki-models--source
         source schema '("Text" "Extra")
         '("{{c1::<b>heart</b> muscle::tissue}} {{c1::x<::bad}}lost> {{c2::safe::good}}"
           "Extra")
         1 '(("{{cloze:Text}}" . "{{cloze:Text}}{{Extra}}")))
        (let ((hash (gnosis-test-anki-models--hash source)))
          (dotimes (_ 2)
            (gnosis-test-anki-models--public source "" nil)
            (should (= 2 (length (gnosis-select 'id 'themata))))
            (let ((row (gnosis-test-campaign-anki--row)))
              (should (equal row '("heart muscle x safe" ("safe") ("good") "Extra")))
              (gnosis-test-campaign-anki--render row))
            (should (equal before (gnosis-test-campaign-anki--facts id)))
            (should (equal hash (gnosis-test-anki-models--hash source)))))))))

(provide 'gnosis-test-campaign-anki)
;;; gnosis-test-campaign-anki.el ends here
