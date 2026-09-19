;;; gnosis-test-narrowed-tags.el --- Restricted draft tagging -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Tag the owning thema, not a visible field or nested body heading.

;;; Code:

(require 'ert)
(require 'gnosis-test-draft-ownership)

(defun gnosis-test-narrowed-tags--position (where)
  "Position the draft at WHERE, narrowing for field and body cases."
  (goto-char (point-min))
  (search-forward "* Thema :original:")
  (pcase where
    ('headline (beginning-of-line))
    ('field (search-forward "** Keimenon") (beginning-of-line))
    (_ (search-forward "*** Details") (beginning-of-line)))
  (when (memq where '(field body)) (org-narrow-to-subtree)))

(defun gnosis-test-narrowed-tags--prompt (input)
  "Invoke the native tag binding with INPUT or a simulated quit."
  (cl-letf (((symbol-function 'completing-read-multiple)
             (lambda (&rest _)
               (if (eq input 'quit) (signal 'quit nil) (copy-sequence input)))))
    (call-interactively (key-binding (kbd "C-c C-q")))))

(ert-deftest gnosis-test-narrowed-tags-owning-thema ()
  "Restricted tagging preserves question, siblings, position and study rows."
  (dolist (where '(headline unrestricted field body))
    (gnosis-test-with-db
      (gnosis-test--add-basic-thema
       "Question\n*** Details\nBody" "Answer" '("original") nil 111)
      (gnosis-scheduler-accept-review
       (gnosis-scheduler-event-id) 111 'success 1000000 (gnosis--today-int))
      (gnosis--insert-into 'practice-events '(["tag-practice" 111 "session" 1 1 3]))
      (let ((evidence (seq-remove
                       (lambda (row) (member (car row) '("themata" "extras" "thema_tag")))
                       (gnosis-test-draft--rows))))
        (gnosis-test-draft--with-editor
          (gnosis-edit-thema 111)
          (goto-char (point-min))
          (gnosis-export--insert-thema "NEW" "basic" "Before" nil "B" nil '("before"))
          (goto-char (point-max))
          (gnosis-export--insert-thema "NEW" "basic" "After" nil "A" nil '("after"))
          (gnosis-test-narrowed-tags--position where)
          (let ((position (point-marker))
                (start (copy-marker (point-min)))
                (end (copy-marker (point-max) t))
                (visible (and (buffer-narrowed-p) (buffer-string))))
            (gnosis-test-narrowed-tags--prompt '("added" "Ελληνικά" "added"))
            (should (= (point) position))
            (should (= (point-min) start))
            (should (= (point-max) end))
            (when visible (should (equal visible (buffer-string)))))
          (call-interactively (key-binding (kbd "C-c C-c")))
          (should (equal (sort (gnosis-get-tags-for-ids '(111)) #'string<)
                         '("added" "original" "Ελληνικά")))
          (should (equal (gnosis-get 'keimenon 'themata '(= id 111))
                         "Question\n*** Details\nBody"))
          (dolist (neighbor '("Before" "After"))
            (let ((id (gnosis-get 'id 'themata `(= keimenon ,neighbor))))
              (should id)
              (should (equal (gnosis-get-tags-for-ids (list id))
                             (list (downcase neighbor))))))
          ;; Newly authored siblings get scheduler rows; retained evidence must
          ;; remain byte-for-byte equal for the existing reviewed thema.
          (dolist (row evidence)
            (let ((after (cdr (assoc (car row) (gnosis-test-draft--rows)))))
              (if (member (car row) '("scheduler_state" "scheduler_baseline"))
                  (should (seq-every-p (lambda (old) (member old after)) (cdr row)))
                (should (equal (cdr row) after)))))
          (gnosis-edit-thema 111)
          (should (equal (nth 2 (car (gnosis-export-parse-themata)))
                         "Question\n*** Details\nBody"))
          (call-interactively (key-binding (kbd "C-c C-k"))))))))

(ert-deftest gnosis-test-narrowed-tags-refusal-preserves-draft ()
  "Invalid, empty and cancelled tag input preserves the entire draft."
  (dolist (where '(headline unrestricted field body))
    (dolist (input '(nil quit ("needs-work")))
      (gnosis-test-with-db
        (gnosis-test-draft--with-editor
          (gnosis-add-thema "basic" "Question\n*** Details\nBody" nil "Answer" nil '("original"))
          (let ((text (buffer-string))
                (rows (gnosis-test-draft--rows))
                (gnosis-previous-thema-tags '("previous")))
            (gnosis-test-narrowed-tags--position where)
            (let ((position (point)) (start (point-min)) (end (point-max)))
              (pcase input
                ('quit (should (eq 'quit (condition-case nil
                                            (gnosis-test-narrowed-tags--prompt input)
                                          (quit 'quit)))))
                ('nil (gnosis-test-narrowed-tags--prompt input))
                (_ (should-error (gnosis-test-narrowed-tags--prompt input) :type 'user-error)))
              (should (= (point) position))
              (should (= (point-min) start))
              (should (= (point-max) end)))
            (should (equal gnosis-previous-thema-tags '("previous")))
            (should (equal rows (gnosis-test-draft--rows)))
            (save-restriction
              (widen)
              (should (equal text (buffer-string))))))))))

(provide 'gnosis-test-narrowed-tags)
;;; gnosis-test-narrowed-tags.el ends here
