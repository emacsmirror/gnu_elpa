;;; gnosis-test-link-quality.el --- Link and tag preservation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Public text mutations and bounded SQLite operations on disposable data.

;;; Code:

(require 'ert)
(require 'gnosis-test-draft-ownership)
(require 'gnosis-review)
(require 'gnosis-links)

(defun gnosis-test-link-quality--reopen ()
  "Reopen the current fixture's physical database."
  (gnosis-sqlite-close gnosis-db)
  (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file)))

(defun gnosis-test-link-quality--key (key command)
  "Dispatch KEY, requiring its native binding to be COMMAND."
  (should (eq (key-binding (kbd key)) command))
  (call-interactively command))

(ert-deftest gnosis-test-link-quality-literal-bulk-reopen ()
  "Link literal backslashes at every outside position without other writes."
  (dolist (token '("\\1" "\\alpha" "\\&" "tail\\" "dRuG"))
    (gnosis-test-with-db
      (let* ((link "[[id:old][Keep]]")
             (text (mapconcat #'identity (list token link token link token) " "))
             (replacement (format "[[id:target][%s]]" token))
             (expected (mapconcat #'identity
                                  (list replacement link replacement link replacement) " ")))
        (gnosis-add-thema-fields "basic" text nil '("A") "Context" '("tag") 0 '("old") nil 1)
        (gnosis-add-thema-fields "basic" "Neighbor" nil '("N") "Unchanged" '("other") 0 nil nil 2)
        (let ((before (gnosis-test-draft--rows)))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (should (equal (gnosis-bulk-link-themata '(1 2) (downcase token) "target") '(1))))
          (gnosis-test-link-quality--reopen)
          (should (equal (gnosis-get 'keimenon 'themata '(= id 1)) expected))
          (should (equal (gnosis-select 'dest 'thema-links '(= source 1) t)
                         '("old" "target")))
          ;; Restore only the intended effects, then compare every stored row.
          (gnosis-update 'themata `(= keimenon ,text) '(= id 1))
          (gnosis-sqlite-execute gnosis-db
                                "DELETE FROM thema_links WHERE source = ? AND dest = ?"
                                '(1 "target"))
          (should (equal before (gnosis-test-draft--rows))))))))

(ert-deftest gnosis-test-link-quality-native-spans ()
  "Native bracket spans and properties survive helpers and public writes."
  (dolist (link (list (org-link-make-string "id:old" "Drug [alpha] δ")
                      (org-link-make-string "id:old" "Drug\nβ")
                      (org-link-make-string "id:Drug")
                      (org-link-make-string "id:old" "Drug \\alpha")
                      (org-link-make-string "id:old" "Drug")))
    (should (string-match org-link-bracket-re link))
    (should (= (match-end 0) (length link)))
    (let* ((protected (propertize link 'face 'bold 'help-echo "retained"))
           (outside (propertize "dRuG" 'face 'italic))
           (text (concat protected " " outside))
           (copy (copy-sequence text))
           (expected (concat protected " [[id:target][" outside "]]")))
      (should-not (gnosis-utils-string-outside-links-p protected "Drug"))
      (should (gnosis-utils-string-outside-links-p text "Drug"))
      (should (equal-including-properties
               (gnosis-utils-replace-string-with-link protected "Drug" "target")
               (cons nil protected)))
      (should (equal-including-properties
               (gnosis-utils-replace-string-with-link text "Drug" "target")
               (cons t expected)))
      (should (equal-including-properties text copy))
      (gnosis-test-with-db
        (gnosis-add-thema-fields "basic" text nil '("A") "P" '("tag") 0 '("old") nil 1)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (gnosis-bulk-link-themata '(1) "Drug" "target"))
        (gnosis-test-link-quality--reopen)
        (should (equal-including-properties
                 (gnosis-get 'keimenon 'themata '(= id 1)) expected)))))
  ;; This remains a text helper, not an Org document reserializer: code
  ;; examples retain the same outside-text policy as ordinary prose.
  (should (equal (cdr (gnosis-utils-replace-string-with-link
                      "#+begin_src text\nDrug\n#+end_src" "Drug" "target"))
                 "#+begin_src text\n[[id:target][Drug]]\n#+end_src")))

(ert-deftest gnosis-test-link-quality-bulk-refusal-and-rollback ()
  "Decline, quit and failure after the first real write leave no effects."
  (dolist (fault '(decline prompt-quit error quit))
    (gnosis-test-with-db
      (dolist (id '(1 2))
        (gnosis-add-thema-fields "basic" "Drug" nil '("A") "P" '("tag") 0 nil nil id))
      (let ((before (gnosis-test-draft--rows))
            (update (symbol-function 'gnosis-update))
            (writes 0) caught)
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (pcase fault ('decline nil) ('prompt-quit (signal 'quit nil)) (_ t))))
                  ((symbol-function 'gnosis-update)
                   (lambda (&rest args)
                     (prog1 (apply update args)
                       (cl-incf writes)
                       (when (= writes 1)
                         (pcase fault
                           ('error (error "Injected link write failure"))
                           ('quit (signal 'quit nil))))))))
          (condition-case err (gnosis-bulk-link-themata '(1 2) "Drug" "target")
            ((error quit) (setq caught (car err)))))
        (should (eq caught (pcase fault ('decline nil) ('prompt-quit 'quit) (_ fault))))
        (should (= writes (if (memq fault '(error quit)) 1 0)))
        (gnosis-test-link-quality--reopen)
        (should (equal before (gnosis-test-draft--rows)))))))

(ert-deftest gnosis-test-link-quality-tag-owner-save ()
  "The tag binding resolves either sibling from owner, field or body."
  (dolist (id '(1 2))
    (dolist (position '(owner field body))
      (gnosis-test-with-db
        (gnosis-test-draft--with-editor
          (gnosis-add-thema-fields "basic" "Neighbor" nil '("A") "P" '("old") 0 nil nil 99)
          (let ((scheduler (sqlite-select gnosis-db "SELECT * FROM scheduler_state WHERE thema_id = 99")))
            (gnosis-add-thema "basic" "Question 1" nil "A" "P" '("old"))
            (goto-char (point-max))
            (gnosis-export--insert-thema "NEW" "basic" "Question 2" nil "A" "P" '("old"))
            (goto-char (point-min))
            (search-forward (format "Question %s" id))
            (org-back-to-heading t)
            (while (org-up-heading-safe))
            (pcase position
              ('field (search-forward "** Keimenon"))
              ('body (search-forward (format "Question %s" id))))
            (let ((origin (copy-marker (point))))
              (cl-letf (((symbol-function 'completing-read-multiple)
                         (lambda (&rest _) '("new_α" "new_α"))))
                (gnosis-test-link-quality--key "C-c C-q" 'gnosis-tags-prompt))
              (should (= (point) origin)))
            (gnosis-test-link-quality--key "C-c C-c" 'gnosis-save)
            (gnosis-test-link-quality--reopen)
            (let ((selected (gnosis-get 'id 'themata `(= keimenon ,(format "Question %s" id))))
                  (neighbor (gnosis-get 'id 'themata `(= keimenon ,(format "Question %s" (- 3 id))))))
              (should (equal (sort (gnosis-get-tags-for-ids (list selected)) #'string<)
                             '("new_α" "old")))
              (should (equal (gnosis-get-tags-for-ids (list neighbor)) '("old")))
              (should (equal scheduler (sqlite-select gnosis-db "SELECT * FROM scheduler_state WHERE thema_id = 99")))
              (gnosis-edit-thema selected))
            (should (equal (sort (nth 6 (car (gnosis-export-parse-themata))) #'string<)
                           '("new_α" "old")))))))))

(ert-deftest gnosis-test-link-quality-tag-prompt-refusal ()
  "Empty, invalid, error and quit input preserve the full draft and point."
  (dolist (input '(nil ("bad-tag") error quit))
    (gnosis-test-with-db
      (gnosis-test-draft--with-editor
        (gnosis-add-thema "basic" "Question" nil "A" "P" '("old"))
        (goto-char (point-min))
        (search-forward "* Thema")
        (put-text-property (point) (1+ (point)) 'help-echo "exact")
        (let ((text (buffer-string)) (origin (point))
              (rows (gnosis-test-draft--rows))
              (gnosis-previous-thema-tags '("previous")) caught)
          (cl-letf (((symbol-function 'completing-read-multiple)
                     (lambda (&rest _)
                       (if (memq input '(error quit))
                           (signal input '("Injected prompt failure")) input))))
            (condition-case err
                (gnosis-test-link-quality--key "C-c C-q" 'gnosis-tags-prompt)
              ((error quit) (setq caught (car err)))))
          (should (eq caught (cond ((null input) nil) ((listp input) 'user-error) (t input))))
          (should (equal-including-properties text (buffer-string)))
          (should (= origin (point)))
          (should (equal gnosis-previous-thema-tags '("previous")))
          (gnosis-test-link-quality--key "C-c C-k" 'gnosis-edit-quit)
          (gnosis-test-link-quality--reopen)
          (should (equal rows (gnosis-test-draft--rows))))))))

(ert-deftest gnosis-test-link-quality-tag-batches-unique ()
  "All decoded tag batches contribute one first-seen exact identity."
  (gnosis-test-with-db
    (dolist (id '(1 2 3))
      (gnosis-add-thema-fields "basic" "Q" nil '("A") "P" '("shared_α") 0 nil nil id))
    (gnosis-add-thema-fields "basic" "Q" nil '("A") "P" '("other") 0 nil nil 4)
    (let ((gnosis-sqlite--max-vars 2))
      (dotimes (_ 2)
        (dolist (ids '(nil [] (1 2) [1 2] (1 2 3 4 1) [1 2 3 4 1]))
          (let ((before (copy-sequence ids)))
            (should (equal (gnosis-get-tags-for-ids ids)
                           (cond ((zerop (length ids)) nil)
                                 ((= (length ids) 2) '("shared_α"))
                                 (t '("shared_α" "other")))))
            (should (equal ids before))))
        (gnosis-test-link-quality--reopen))
      (let ((gnosis-new-themata-limit nil) candidates checked
            (check (symbol-function 'gnosis-tags--check-org)))
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (_prompt values &rest _) (setq candidates values) nil)))
          (gnosis-review--read-selection 'due-tags))
        (should (equal (sort candidates #'string<)
                       '("+other" "+shared_α" "-other" "-shared_α")))
        (with-temp-buffer
          (org-mode)
          (cl-letf (((symbol-function 'gnosis-tags--check-org)
                     (lambda (tags) (push (copy-sequence tags) checked) (funcall check tags))))
            (gnosis-export--insert-themata '(1 2 3 4)))
          (should (equal (car (last checked)) '("shared_α" "other"))))))))

(ert-deftest gnosis-test-link-quality-collector-characterization ()
  "Collection preserves query order, duplicates, initial identity and errors."
  (gnosis-test-with-db
    (gnosis-add-thema-fields "basic" "One" nil '("A") "P" '("a" "α") 0 nil nil 1)
    (gnosis-add-thema-fields "basic" "Two" nil '("A") "P" '("b" "a") 0 nil nil 2)
    (gnosis-add-thema-fields "basic" "Three" nil '("A") "P" '("b") 0 nil nil 3)
    (dolist (case '((nil nil) (("a") (1 2)) (("b" "a") (2 3 1 2))
                    (("a" "a" "b") (1 2 1 2 2 3)) (("α" "missing") (1))))
      (dolist (initial '(nil (99) (99 99 98)))
        (let* ((tags (copy-tree (car case))) (before (copy-tree tags))
               (ids (copy-tree initial))
               (actual (gnosis-collect-tag-thema-ids tags ids)))
          (should (equal actual (append initial (cadr case))))
          (should (equal tags before))
          (should (equal ids initial))
          (unless tags (should (eq actual ids))))))
    (should (equal (sort (gnosis-filter-by-tags '("a" "b") '("α")) #'<) '(2 3)))
    (should (equal (gnosis-filter-by-tags nil '("a")) '(3)))
    (let ((query (symbol-function 'gnosis-get-tag-themata)) calls)
      (cl-letf (((symbol-function 'gnosis-get-tag-themata)
                 (lambda (tag)
                   (push tag calls)
                   (if (equal tag "missing") (error "Injected query failure")
                     (funcall query tag)))))
        (should-error (gnosis-collect-tag-thema-ids '("a" "missing" "b"))))
      (should (equal (reverse calls) '("a" "missing"))))))

(provide 'gnosis-test-link-quality)
;;; gnosis-test-link-quality.el ends here
