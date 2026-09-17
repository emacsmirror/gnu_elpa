;;; gnosis-test-journal-selection.el --- Journal source identities -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Public completion, source coverage and native task-link regressions.

;;; Code:

(require 'ert)
(require 'gnosis-journal)
(require 'gnosis-test-helpers)

(defmacro gnosis-test-journal-selection--with-files (&rest body)
  "Run BODY with disposable journal sources and a native index."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-db
     (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
            (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
            (gnosis-journal-file t)
            (gnosis-journal-as-gpg nil)
            (gnosis-journal-new-entry-template nil)
            (gnosis-journal-todo-files nil)
            (org-id-track-globally nil)
            (org-id-locations nil)
            (create-lockfiles nil)
            (make-backup-files nil))
       (make-directory gnosis-journal-dir t)
       (unwind-protect
           (save-window-excursion ,@body)
         (dolist (buffer (buffer-list))
           (when-let* ((file (buffer-file-name buffer)))
             (when (file-in-directory-p file gnosis-dir)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))))))

(defun gnosis-test-journal-selection--file (name title id)
  "Write and index journal NAME with heading TITLE and ID."
  (let ((file (expand-file-name name gnosis-journal-dir)))
    (with-temp-file file
      (insert (format "#+title: Journal\n#+filetags: :journal_tag:\n* %s\n:PROPERTIES:\n:ID: %s\n:END:\nBody\n"
                      title id)))
    (gnosis-nodes-update-file file)
    file))

(defun gnosis-test-journal-selection--read (file)
  "Return FILE's disk text."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun gnosis-test-journal-selection--links ()
  "Return parsed ID links with their full descriptions and trailing text."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (equal (org-element-property :type link) "id")
        (list (org-element-property :path link)
              (buffer-substring-no-properties
               (org-element-property :contents-begin link)
               (org-element-property :contents-end link))
              (save-excursion
                (goto-char (org-element-property :end link))
                (buffer-substring-no-properties (point) (line-end-position))))))))

(ert-deftest gnosis-test-journal-selection-existing-identities ()
  "Both public pickers retain IDs, live titles and literal punctuation."
  (dolist (tagged '(t nil))
    (dolist (command '(gnosis-journal-find gnosis-journal-insert))
      (dolist (kind '(configured retained renamed literal retained-literal))
        (gnosis-test-journal-selection--with-files
          (let* ((gnosis-nodes-show-tags tagged)
                 (literal (memq kind '(literal retained-literal)))
                 (retained (memq kind '(retained retained-literal)))
                 (title (if literal "Topic:2001-02-03  #literal" "2001-02-03"))
                 (file (gnosis-test-journal-selection--file
                        (if retained "archive.org" "journal.org") title "day"))
                 (gnosis-nodes-completing-read-func
                  (lambda (_ collection &rest _)
                    (or (cl-find-if (lambda (s) (string-match-p (regexp-quote title) s)) collection)
                        (ert-fail (format "Missing %s in %S" title collection))))))
            (with-current-buffer (find-file-noselect file)
              (goto-char (point-min))
              (insert ":PROPERTIES:\n:ID: root\n:END:\n")
              (save-buffer)
              (gnosis-nodes-update-file file)
              (when (eq kind 'renamed)
                (re-search-forward "2001-02-03")
                (replace-match "2001-02-05")
                (setq title "2001-02-05")))
            (when retained (kill-buffer (get-file-buffer file)))
            (with-temp-buffer
              (org-mode)
              (call-interactively command)
              (if (eq command 'gnosis-journal-find)
                  (should (equal (org-id-get) "day"))
                (should (equal (caar (gnosis-test-journal-selection--links)) "day"))
                (when (eq kind 'literal)
                  (should (equal (cadar (gnosis-test-journal-selection--links)) title)))))
            (with-current-buffer (get-file-buffer file)
              (should (= (count-matches "^\\* " (point-min) (point-max)) 1))
              (save-buffer)
              (kill-buffer))
            (with-current-buffer (find-file-noselect file)
              (should (= (count-matches "^\\* " (point-min) (point-max)) 1))
              (should (org-find-entry-with-id "day")))))))))

(ert-deftest gnosis-test-journal-selection-empty-completion ()
  "Empty authoritative sources never fall back to stale index titles."
  (dolist (layout '(t nil))
    (dolist (tagged '(nil t))
      (dolist (command '(gnosis-journal-find gnosis-journal-insert))
        (gnosis-test-journal-selection--with-files
          (let* ((gnosis-journal-file layout)
                 (gnosis-nodes-show-tags tagged)
                 (file (gnosis-test-journal-selection--file "journal.org" "2001-02-03" "day"))
                 (gnosis-nodes-completing-read-func
                  (lambda (_ collection &rest _) (throw 'choices collection))))
            (with-current-buffer (find-file-noselect file) (erase-buffer))
            (should-not (catch 'choices (call-interactively command)))))))))

(ert-deftest gnosis-test-journal-selection-source-coverage ()
  "Parse each source once, preserving empty drafts and index-only archives."
  (dolist (layout '(t nil))
    (gnosis-test-journal-selection--with-files
      (let* ((gnosis-journal-file layout)
             (current (gnosis-test-journal-selection--file "journal.org" "2001-02-03" "current"))
             (empty (gnosis-test-journal-selection--file "retained.org" "2001-02-02" "stale"))
             (archive (gnosis-test-journal-selection--file "archive.org" "2001-02-01" "archive"))
             (disk (gnosis-test-journal-selection--read empty))
             (index (gnosis-nodes-select '* 'journal))
             (parse (symbol-function 'gnosis-journal--file-entries))
             (read (symbol-function 'insert-file-contents))
             calls)
        (when (get-file-buffer archive) (kill-buffer (get-file-buffer archive)))
        (find-file current)
        (goto-char (point-max))
        (insert "Unsaved tail\n")
        (narrow-to-region (1- (point-max)) (point-max))
        (with-current-buffer (find-file-noselect empty) (erase-buffer))
        (cl-letf (((symbol-function 'gnosis-journal--file-entries)
                   (lambda (file) (push file calls) (funcall parse file)))
                  ((symbol-function 'insert-file-contents)
                   (lambda (file &rest args)
                     (should-not (equal file archive))
                     (apply read file args))))
          (should (equal (gnosis-journal--dated-titles) '("2001-02-01" "2001-02-03"))))
        (should (= (cl-count current calls :test #'equal) 1))
        (should (= (cl-count empty calls :test #'equal) 1))
        (should (= (length calls) 2))
        (should (buffer-narrowed-p))
        (should (buffer-modified-p))
        (gnosis-journal-find "2001-02-03")
        (call-interactively 'gnosis-journal-previous)
        (should (equal (org-id-get) "archive"))
        (call-interactively 'gnosis-journal-next)
        (should (equal (org-id-get) "current"))
        (with-current-buffer (get-file-buffer empty)
          (should (= (buffer-size) 0))
          (should (buffer-modified-p)))
        (should (equal disk (gnosis-test-journal-selection--read empty)))
        (should (equal index (gnosis-nodes-select '* 'journal)))))))

(ert-deftest gnosis-test-journal-selection-task-link-descriptions ()
  "Template and explicit task insertion serialize complete native ID links."
  (dolist (route '(template insert-task))
    (dolist (title '("Read paper then compare" "Review [draft] then compare"
                     "Read [[https://example.com][paper]] then compare"
                     "Read [[paper]] then compare" "Ελληνικά ]] ουρά 🧠]"))
      (gnosis-test-journal-selection--with-files
        (let* ((tasks (expand-file-name "tasks.org" gnosis-nodes-dir))
               (text (format "* TODO %s\n:PROPERTIES:\n:ID: task\n:END:\n" title))
               (gnosis-journal-todo-files (list tasks))
               (gnosis-journal-new-entry-template (and (eq route 'template) "Goals"))
               (gnosis-nodes-completing-read-func (lambda (_ collection &rest _) (car collection))))
          (with-temp-file tasks (insert text))
          (gnosis-journal-date "2001-02-03")
          (when (eq route 'insert-task) (call-interactively 'gnosis-journal-insert-task))
          (let ((links (gnosis-test-journal-selection--links)))
            (should (= (length links) 1))
            (should (equal (caar links) "task"))
            (should (equal (replace-regexp-in-string "​" "" (cadar links)) title))
            (should (string-empty-p (string-trim (nth 2 (car links))))))
          (should (equal (gnosis-journal--date-at-point) "2001-02-03"))
          (let ((file buffer-file-name) (body (buffer-string)))
            (save-buffer)
            (kill-buffer)
            (find-file file)
            (should (equal body (buffer-string))))
          (should (equal text (gnosis-test-journal-selection--read tasks))))))))

(ert-deftest gnosis-test-journal-selection-ambiguity-and-retired-id ()
  "Reject duplicate titles and a selected ID removed during completion."
  (dolist (tagged '(nil t))
    (dolist (command '(gnosis-journal-find gnosis-journal-insert))
      (gnosis-test-journal-selection--with-files
        (let* ((gnosis-nodes-show-tags tagged)
               (file (gnosis-test-journal-selection--file "journal.org" "Topic" "old"))
               (buffer (find-file-noselect file))
               (gnosis-nodes-completing-read-func
                (lambda (_ choices &rest _) (car choices))))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "* Topic\n:PROPERTIES:\n:ID: duplicate\n:END:\n"))
          (should-error (call-interactively command) :type 'user-error)
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "* Topic" nil nil 2)
            (delete-region (line-beginning-position) (point-max)))
          (let ((gnosis-nodes-completing-read-func
                 (lambda (_ choices &rest _)
                   (with-current-buffer buffer
                     (goto-char (point-min))
                     (search-forward ":ID: old")
                     (replace-match ":ID: replacement"))
                   (car choices))))
            (should-error (call-interactively command) :type 'user-error))
          (with-current-buffer buffer
            (should (= (count-matches "^\\* " (point-min) (point-max)) 1))))))))

(ert-deftest gnosis-test-journal-selection-acquisition-controls ()
  "Configured aliases stay first; empty/nonempty reads and errors stay exact."
  (gnosis-test-journal-selection--with-files
    (let* ((file (gnosis-test-journal-selection--file "journal.org" "2001-02-03" "day"))
           (other (gnosis-test-journal-selection--file "other.org" "2001-02-04" "other"))
           (alias (expand-file-name "alias.org" gnosis-journal-dir))
           (parse (symbol-function 'gnosis-journal--file-entries)))
      (make-symbolic-link file alias)
      (find-file file)
      (find-file other)
      (let ((gnosis-journal-file alias) calls)
        (cl-letf (((symbol-function 'gnosis-journal--file-entries)
                   (lambda (path) (push path calls) (funcall parse path))))
          (should (equal (mapcar #'car (gnosis-journal--entries))
                         '("2001-02-03" "2001-02-04"))))
        (should (equal (nreverse calls) (list alias other))))
      (dolist (empty '(nil t))
        (with-current-buffer (get-file-buffer file)
          (when empty (erase-buffer)))
        (with-current-buffer (get-file-buffer other)
          (when empty (erase-buffer)))
        (let (calls)
          (cl-letf (((symbol-function 'gnosis-journal--file-entries)
                     (lambda (path) (push path calls) (funcall parse path))))
            (should (= (length (gnosis-journal--entries)) (if empty 0 2))))
          (should (equal (nreverse calls) (list file other)))))
      (with-current-buffer (get-file-buffer file)
        (set-buffer-modified-p nil)
        (kill-buffer))
      (let ((read (symbol-function 'insert-file-contents)))
        (cl-letf (((symbol-function 'insert-file-contents)
                   (lambda (path &rest args)
                     (if (equal path file)
                         (signal 'file-error '("Read/decrypt failed"))
                       (apply read path args)))))
          (should-error (call-interactively 'gnosis-journal-find) :type 'file-error))))))

(ert-deftest gnosis-test-journal-selection-alias-live-completion ()
  "Configured aliases retain empty or renamed canonical live sources."
  (dolist (empty '(t nil))
    (dolist (tagged '(nil t))
      (dolist (command '(gnosis-journal-find gnosis-journal-insert))
        (gnosis-test-journal-selection--with-files
          (let* ((file (gnosis-test-journal-selection--file "journal.org" "2001-02-03" "day"))
                 (alias (expand-file-name "alias.org" gnosis-journal-dir))
                 (gnosis-journal-file alias)
                 (gnosis-nodes-show-tags tagged)
                 (disk (gnosis-test-journal-selection--read file))
                 (index (gnosis-nodes-select '* 'journal))
                 (source (find-file-noselect file))
                 (parse (symbol-function 'gnosis-journal--file-entries))
                 (read (symbol-function 'insert-file-contents))
                 calls state text)
            (make-symbolic-link file alias)
            (with-current-buffer source
              (rename-buffer (generate-new-buffer-name "Canonical journal"))
              (if empty (erase-buffer)
                (goto-char (point-min))
                (search-forward "2001-02-03")
                (replace-match "2001-02-05"))
              (setq text (buffer-string))
              (unless empty
                (narrow-to-region (1- (point-max)) (point-max))
                (goto-char (point-min)))
              (setq state (list major-mode (point) (point-min) (point-max)
                                (buffer-modified-p) (buffer-chars-modified-tick))))
            (let ((gnosis-nodes-completing-read-func
                   (lambda (_ choices &rest _)
                     ;; Acquisition must restore the original restriction/point
                     ;; before the selected command intentionally navigates.
                     (with-current-buffer source
                       (should (equal state
                                      (list major-mode (point) (point-min) (point-max)
                                            (buffer-modified-p) (buffer-chars-modified-tick)))))
                     (if empty
                         (progn (should-not choices) (throw 'empty t))
                       (should (= (length choices) 1))
                       (should (string-prefix-p "2001-02-05" (car choices)))
                       (car choices)))))
              (cl-letf (((symbol-function 'gnosis-journal--file-entries)
                         (lambda (path) (push path calls) (funcall parse path)))
                        ((symbol-function 'insert-file-contents)
                         (lambda (path &rest args)
                           (should-not (gnosis-journal--physical-equal path file))
                           (apply read path args))))
                (with-temp-buffer
                  (org-mode)
                  (if empty
                      (should (catch 'empty (call-interactively command) nil))
                    (call-interactively command)
                    (if (eq command 'gnosis-journal-find)
                        (progn (should (eq (current-buffer) source))
                               (should (equal (org-id-get) "day")))
                      (should (equal (caar (gnosis-test-journal-selection--links)) "day")))))))
            (should (equal calls (list alias)))
            (with-current-buffer source
              (save-restriction
                (widen)
                (should (equal text (buffer-string))))
              (should (buffer-modified-p))
              (should (= (nth 5 state) (buffer-chars-modified-tick))))
            (should (equal disk (gnosis-test-journal-selection--read file)))
            (should (equal index (gnosis-nodes-select '* 'journal)))))))))

(ert-deftest gnosis-test-journal-selection-alias-live-navigation ()
  "Previous/next skip stale alias dates but visit unsaved renamed dates."
  (dolist (empty '(t nil))
    (gnosis-test-journal-selection--with-files
      (let* ((file (gnosis-test-journal-selection--file "journal.org" "2001-02-02" "day"))
             (alias (expand-file-name "alias.org" gnosis-journal-dir))
             (gnosis-journal-file alias)
             (early (gnosis-test-journal-selection--file "early.org" "2001-02-01" "early"))
             (late (gnosis-test-journal-selection--file "late.org" "2001-02-03" "late"))
             (disk (gnosis-test-journal-selection--read file))
             (index (gnosis-nodes-select '* 'journal))
             (source (find-file-noselect file))
             (read (symbol-function 'insert-file-contents)))
        (make-symbolic-link file alias)
        (with-current-buffer source
          (if empty (erase-buffer)
            (goto-char (point-min)) (search-forward "2001-02-02")
            (replace-match "2001-02-04")
            (narrow-to-region (1- (point-max)) (point-max))))
        (gnosis-journal-find "2001-02-01")
        (cl-letf (((symbol-function 'insert-file-contents)
                   (lambda (path &rest args)
                     (should-not (gnosis-journal--physical-equal path file))
                     (apply read path args))))
          (call-interactively 'gnosis-journal-next)
          (should (equal buffer-file-name late))
          (should (equal (org-id-get) "late"))
          (if empty
              (should-error (call-interactively 'gnosis-journal-next) :type 'user-error)
            (call-interactively 'gnosis-journal-next)
            (should (eq (current-buffer) source))
            (should (equal (org-id-get) "day"))
            (should (equal (gnosis-journal--date-at-point) "2001-02-04"))
            (call-interactively 'gnosis-journal-previous)
            (should (equal (org-id-get) "late")))
          (call-interactively 'gnosis-journal-previous)
          (should (equal buffer-file-name early))
          (should (equal (org-id-get) "early")))
        (with-current-buffer source
          (should (buffer-modified-p))
          (when empty (should (= (buffer-size) 0))))
        (should (equal disk (gnosis-test-journal-selection--read file)))
        (should (equal index (gnosis-nodes-select '* 'journal)))))))

(ert-deftest gnosis-test-journal-selection-whole-non-org-todos ()
  "Public templates use all live TODOs without changing mode or restriction."
  (dolist (mode '(fundamental-mode org-mode))
    (gnosis-test-journal-selection--with-files
      (let* ((tasks (expand-file-name "tasks.org" gnosis-nodes-dir))
             (gnosis-journal-todo-files (list tasks))
             (gnosis-journal-new-entry-template "Goals")
             (disk "* TODO Disk only\n")
             source state)
        (with-temp-file tasks (insert disk))
        (setq source (find-file-noselect tasks))
        (with-current-buffer source
          (funcall mode)
          (erase-buffer)
          (dolist (name '("Before" "Inside" "After"))
            (insert (format "* TODO %s\n:PROPERTIES:\n:ID: %s\n:END:\n" name name)))
          (goto-char (point-min))
          (search-forward "* TODO Inside")
          (beginning-of-line)
          (let ((start (point)))
            (search-forward "* TODO After")
            (beginning-of-line)
            (narrow-to-region start (point))
            (goto-char start))
          (setq state (list major-mode (point) (point-min) (point-max)
                            (buffer-modified-p) (buffer-chars-modified-tick))))
        (gnosis-journal-date "2001-02-03")
        (should (equal (sort (mapcar #'car (gnosis-test-journal-selection--links))
                             #'string<)
                       '("After" "Before" "Inside")))
        (should-not (string-match-p "Disk only" (buffer-string)))
        (with-current-buffer source
          (should (equal state (list major-mode (point) (point-min) (point-max)
                                     (buffer-modified-p) (buffer-chars-modified-tick)))))
        (should (equal disk (gnosis-test-journal-selection--read tasks)))))))

(provide 'gnosis-test-journal-selection)
;;; gnosis-test-journal-selection.el ends here
