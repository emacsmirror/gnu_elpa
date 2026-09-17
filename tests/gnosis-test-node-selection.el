;;; gnosis-test-node-selection.el --- Exact node selection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Use indexed Org files, not fabricated rows, to check selection and visits.

;;; Code:

(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-nodes)
(require 'gnosis-journal)

(defmacro gnosis-test-node-selection--with-vault (&rest body)
  "Run BODY with disposable indexed files and visiting buffers."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-db
     (let ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-dir))
           (gnosis-journal-file nil)
           (org-id-track-globally nil)
           (org-id-locations nil)
           (make-backup-files nil)
           (create-lockfiles nil))
       (make-directory gnosis-nodes-dir t)
       (make-directory gnosis-journal-dir t)
       (unwind-protect
           (save-window-excursion ,@body)
         (dolist (buffer (buffer-list))
           (when (and (buffer-file-name buffer)
                      (file-in-directory-p (buffer-file-name buffer) gnosis-dir))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))))))

(defun gnosis-test-node-selection--file (name id title &optional tags body journal)
  "Write and index NAME with ID, TITLE, TAGS and BODY.
When JOURNAL is non-nil, use the journal directory and index."
  (let ((file (expand-file-name name (if journal gnosis-journal-dir
                                     gnosis-nodes-dir))))
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: %s\n#+filetags: %s\n\n%s"
                      id title (or tags "") (or body ""))))
    (gnosis-nodes-update-file file)
    file))

(ert-deftest gnosis-test-node-selection-tag-filter-retains-id ()
  "The only tagged namesake is selected, not an unfiltered title match."
  (gnosis-test-node-selection--with-vault
    (gnosis-test-node-selection--file "first.org" "first" "Shared" ":one:")
    (let ((file (gnosis-test-node-selection--file
                 "second.org" "second" "Shared" ":two:")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Shared")))
        (gnosis-nodes-find-by-tag "two"))
      (should (equal buffer-file-name file))
      (should (equal (org-id-get) "second")))))

(ert-deftest gnosis-test-node-selection-literal-tag-decoration ()
  "Literal hash decorations, brackets, colons and Unicode survive find."
  (gnosis-test-node-selection--with-vault
    (let* ((title "Org: [α]  #literal title")
           (file (gnosis-test-node-selection--file "literal.org" "literal" title ":tag:"))
           (gnosis-nodes-show-tags t)
           (gnosis-nodes-templates '(("Empty" . (lambda () ""))))
           (gnosis-nodes-completing-read-func
            (lambda (_prompt values) (car (all-completions "" values)))))
      (call-interactively #'gnosis-nodes-find)
      (should (equal buffer-file-name file))
      (should (equal (org-id-get) "literal"))
      (should-not (buffer-modified-p))
      (should (= 1 (length (directory-files gnosis-nodes-dir nil "\\.org$")))))))

(ert-deftest gnosis-test-node-selection-title-helper-preserves-decoration ()
  "The legacy tagged-title helper maps selected labels, never strips text."
  (let* ((title "Org  #literal title")
         (gnosis-nodes-completing-read-func
          (lambda (_prompt values) (car (all-completions "" values)))))
    (should (equal title (gnosis-nodes-find--with-tags
                         nil `((,title "(\"tag\")")))))))

(ert-deftest gnosis-test-node-selection-duplicate-find-and-insert ()
  "Plain/tagged pickers retain namesake IDs for root and heading targets."
  (dolist (show-tags '(nil t))
    (gnosis-test-node-selection--with-vault
      (gnosis-test-node-selection--file
       "first.org" "first" "Shared" ":same:"
       "* Child\n:PROPERTIES:\n:ID: first-child\n:END:\n")
      (let* ((file (gnosis-test-node-selection--file
                    "second.org" "second" "Shared" ":same:"
                    "* Child\n:PROPERTIES:\n:ID: second-child\n:END:\n"))
             (gnosis-nodes-show-tags show-tags))
        (dolist (target '("second" "second-child"))
          (let* ((title (if (equal target "second") "Shared" "Shared:Child"))
                 (gnosis-nodes-completing-read-func
                  (lambda (_prompt values)
                    (or (seq-find (lambda (label)
                                    (and (string-prefix-p title label)
                                         (string-match-p "second.org" label)))
                                  (all-completions "" values))
                        ;; A baseline title-only picker cannot identify either file.
                        title))))
            (call-interactively #'gnosis-nodes-find)
            (should (equal buffer-file-name file))
            (should (equal (org-id-get) target))
            (with-temp-buffer
              (org-mode)
              (gnosis-nodes-insert nil)
              (goto-char (point-min))
              (should (equal (org-element-property :path (org-element-context)) target))
              (should (equal (buffer-string) (format "[[id:%s][%s]]" target title))))))))))

(ert-deftest gnosis-test-node-selection-backlinks-retain-id ()
  "Backlinks do not escape their filtered set through a title lookup."
  (gnosis-test-node-selection--with-vault
    (gnosis-test-node-selection--file "first.org" "first" "Shared")
    (let ((file (gnosis-test-node-selection--file
                 "second.org" "second" "Shared" nil "[[id:target][Target]]\n"))
          (destination (gnosis-test-node-selection--file "target.org" "target" "Target")))
      (find-file destination)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Shared")))
        (call-interactively #'gnosis-nodes-visit-backlinks))
      (should (equal buffer-file-name file))
      (should (equal (org-id-get) "second")))))

(ert-deftest gnosis-test-node-selection-new-title-still-creates ()
  "An unmatched title creates a draft, with its exact literal title."
  (gnosis-test-node-selection--with-vault
    (let* ((title "New: [α]  #literal")
           (gnosis-nodes-timestring nil)
           (gnosis-nodes-templates '(("Empty" . (lambda () ""))))
           (gnosis-nodes-completing-read-func (lambda (&rest _) title)))
      (call-interactively #'gnosis-nodes-find)
      (should (buffer-modified-p))
      (should (equal (cadr (assoc "TITLE" (org-collect-keywords '("TITLE")))) title))
      (should (org-id-get))
      (should-not (file-exists-p buffer-file-name)))))

(ert-deftest gnosis-test-node-selection-explicit-id-does-not-create ()
  "An explicit existing ID never turns a stale title into a new draft."
  (gnosis-test-node-selection--with-vault
    (let ((file (gnosis-test-node-selection--file "known.org" "known" "New title"))
          (gnosis-nodes-templates '(("Empty" . (lambda () "")))))
      (gnosis-nodes-find "Old title" nil "known")
      (should (equal buffer-file-name file))
      (should (equal (org-id-get) "known"))
      (should-not (buffer-modified-p)))))

(ert-deftest gnosis-test-node-selection-colliding-labels-stay-distinct ()
  "Literal file/occurrence-looking labels do not steal duplicate candidates."
  (dolist (show-tags '(nil t))
    (gnosis-test-node-selection--with-vault
      (gnosis-test-node-selection--file
       "same.org" "root" "Shared" nil
       (concat "* Child\n:PROPERTIES:\n:ID: first\n:END:\n"
               "* Child\n:PROPERTIES:\n:ID: second\n:END:\n"))
      (gnosis-test-node-selection--file
       "literal.org" "literal" "Shared:Child — same.org")
      (gnosis-test-node-selection--file
       "numbered.org" "numbered" "Shared:Child — same.org <2>")
      (let* ((gnosis-nodes-show-tags show-tags)
             (rows (gnosis-nodes-select '[id title file tags] 'nodes))
             (candidates (gnosis-nodes--completion-candidates rows show-tags))
             (labels (mapcar #'car candidates)))
        (should (= (length labels) (length (delete-dups (copy-sequence labels)))))
        (dolist (id '("first" "second" "literal" "numbered"))
          (let* ((label (car (rassoc id candidates)))
                 (gnosis-nodes-completing-read-func
                  (lambda (_prompt collection)
                    (should (test-completion label collection))
                    (substring-no-properties label))))
            (call-interactively #'gnosis-nodes-find)
            (should (equal (org-id-get) id))
            (should-not (buffer-modified-p))))))))

(ert-deftest gnosis-test-node-selection-insertion-descriptions-and-origin ()
  "Node/journal insertion keeps exact labels and custom/region descriptions."
  (dolist (journal '(nil t))
    (gnosis-test-node-selection--with-vault
      (let* ((title "Literal: [α]  #tag")
             (file (gnosis-test-node-selection--file
                    "source.org" "source" title ":tag:"
                    "* Origin\nInsert here.\n* Hidden\nKeep.\n" journal))
             (gnosis-nodes-show-tags t)
             (gnosis-nodes-completing-read-func
              (lambda (_prompt collection) (car (all-completions "" collection)))))
        (find-file file)
        (search-forward "* Origin") (org-narrow-to-subtree)
        (search-forward "Insert here.")
        (let ((start (point)) (min (point-min)))
          (gnosis-nodes-insert nil journal)
          (should (= min (point-min)))
          (should (buffer-narrowed-p))
          (should (equal (buffer-substring start (point))
                         (format "[[id:source][%s]]" title)))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Custom α")))
            (gnosis-nodes-insert t journal))
          (should (string-match-p "\\[\\[id:source\\]\\[Custom α\\]\\]" (buffer-string)))
          (let ((transient-mark-mode t))
            (insert "Region description")
            (push-mark (- (point) (length "Region description")) t t)
            (gnosis-nodes-insert nil journal))
          (should (string-match-p "\\[\\[id:source\\]\\[Region description\\]\\]" (buffer-string))))))))

(ert-deftest gnosis-test-node-selection-empty-filter-does-not-create ()
  "An empty tag filter cannot select a global namesake or create a draft."
  (gnosis-test-node-selection--with-vault
    (gnosis-test-node-selection--file "known.org" "known" "Known")
    (with-temp-buffer
      (org-mode)
      (insert "Origin draft")
      (let ((origin (current-buffer))
            (gnosis-nodes-completing-read-func (lambda (&rest _) "Known")))
        (should-error (gnosis-nodes-find-by-tag "missing") :type 'user-error)
        (should (eq (current-buffer) origin))
        (should (equal (buffer-string) "Origin draft"))))))

(ert-deftest gnosis-test-node-selection-custom-reader-accepts-literal-title ()
  "A two-argument reader can still return an undecorated existing title."
  (gnosis-test-node-selection--with-vault
    (let* ((file (gnosis-test-node-selection--file "known.org" "known" "Known" ":tag:"))
           (gnosis-nodes-show-tags t)
           (gnosis-nodes-templates '(("Empty" . (lambda () ""))))
           (gnosis-nodes-completing-read-func (lambda (_prompt _collection) "Known")))
      (call-interactively #'gnosis-nodes-find)
      (should (equal buffer-file-name file))
      (should (equal (org-id-get) "known"))
      (should-not (buffer-modified-p))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Known")))
        (gnosis-nodes-find-by-tag "tag"))
      (should (equal (org-id-get) "known"))
      (with-temp-buffer
        (org-mode)
        (gnosis-nodes-insert nil)
        (should (equal (buffer-string) "[[id:known][Known]]"))))))

(defun gnosis-test-node-selection--created-link (same)
  "Check insertion origin after creation or visiting the SAME unindexed file."
  (gnosis-test-node-selection--with-vault
    (let* ((file (gnosis-test-node-selection--file
                  (if same "Created.org" "origin.org") "root"
                  (if same "Created" "Origin") nil
                  "* Child\n:PROPERTIES:\n:ID: child\n:END:\nInsert here.\n* Other\nKeep.\n"))
           (gnosis-nodes-timestring nil)
           (gnosis-nodes-completing-read-func (lambda (&rest _) "Created")))
      (when same (gnosis--delete 'nodes))
      (find-file file)
      (search-forward "* Child") (org-narrow-to-subtree)
      (search-forward "Insert here.")
      (let ((origin (current-buffer)) (min (point-min)) (start (point)))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (gnosis-nodes-insert nil))
        (should (eq origin (current-buffer)))
        (should (= min (point-min)))
        (should (buffer-narrowed-p))
        (goto-char start)
        (let ((id (org-element-property :path (org-element-context))))
          (should (stringp id))
          (should-not (member id '("nil" "child")))
          (if same
              (progn
                (should (equal id "root"))
                ;; An unchanged file is not silently saved or reindexed.
                (should-not (gnosis-nodes-select 'file 'nodes `(= id ,id))))
            (should (gnosis-nodes-select 'file 'nodes `(= id ,id)))))
        (should (file-exists-p (expand-file-name "Created.org" gnosis-nodes-dir)))))))

(ert-deftest gnosis-test-node-selection-created-link-preserves-origin ()
  "New-file insertion retains the source point and restriction."
  (gnosis-test-node-selection--created-link nil))

(ert-deftest gnosis-test-node-selection-same-file-creation-retains-root-id ()
  "An unindexed same-file visit links the root, not nil or a child ID."
  (gnosis-test-node-selection--created-link t))

(provide 'gnosis-test-node-selection)
;;; gnosis-test-node-selection.el ends here
