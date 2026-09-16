;;; gnosis-test-import-owner.el --- Import view ownership -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Exercise public import commands with real disposable SQLite destinations.

;;; Code:

(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-export-import)

(defmacro gnosis-test-import-owner--with-preview (&rest body)
  "Run BODY with an owned preview and disposable source and destination."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-db
     (save-window-excursion
       (let* ((buffers (buffer-list))
              (id (gnosis-test--add-basic-thema "Incoming α" "Answer" nil nil 101))
              (file (expand-file-name "incoming.gnosis" gnosis-dir))
              preview)
         (unwind-protect
             (progn
               (gnosis-export-db file)
               (gnosis-sqlite-execute gnosis-db "UPDATE themata SET keimenon = ?"
                                      '("Local β"))
               (gnosis-import-db file)
               (setq preview (current-buffer))
               (ignore id preview)
               ,@body)
           (dolist (buffer (seq-difference (buffer-list) buffers))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))))))

(defun gnosis-test-import-owner--facts ()
  "Return every application table's rows in the current database."
  (mapcar (lambda (row)
            (cons (car row) (sqlite-select gnosis-db
                                          (format "SELECT * FROM %s" (car row)))))
          (sqlite-select gnosis-db
                         "SELECT name FROM sqlite_master WHERE type = 'table' ORDER BY name")))

(defun gnosis-test-import-owner--sentinel (&optional detach)
  "Repurpose the current view as a file with unsaved text.
When DETACH is non-nil, detach afterward without changing major mode."
  (set-visited-file-name (expand-file-name "successor.txt" gnosis-dir) t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Successor UNSAVED γ"))
  (when detach (set-visited-file-name nil t)))

(ert-deftest gnosis-import-owner-preserves-named-foreign-buffers ()
  "Names alone never grant authority to erase a draft or visiting buffer."
  (dolist (name '("*Gnosis Import*" "*Gnosis Import Detail*"))
    (dolist (visiting '(nil t))
      (gnosis-test-with-db
        (save-window-excursion
          (let* ((buffers (buffer-list))
                 (foreign (generate-new-buffer name))
                 (file (expand-file-name "source.gnosis" gnosis-dir)))
            (unwind-protect
                (progn
                  (gnosis-test--add-basic-thema "Incoming" "A")
                  (gnosis-export-db file)
                  (gnosis-sqlite-execute gnosis-db "UPDATE themata SET keimenon = ?" '("Local"))
                  (with-current-buffer foreign
                    (when visiting
                      (set-visited-file-name (expand-file-name "draft.txt" gnosis-dir) t)
                      (rename-buffer name))
                    (insert "UNSAVED SENTINEL δ"))
                  (let ((facts (gnosis-test-import-owner--facts))
                        (hash (gnosis-import--file-sha256 file))
                        (filename (buffer-local-value 'buffer-file-name foreign)))
                    (gnosis-import-db file)
                    (goto-char (point-min))
                    (call-interactively (key-binding (kbd "RET")))
                    (with-current-buffer foreign
                      (should (equal (buffer-string) "UNSAVED SENTINEL δ"))
                      (should (buffer-modified-p))
                      (should (equal filename buffer-file-name)))
                    (should (equal facts (gnosis-test-import-owner--facts)))
                    (should (equal hash (gnosis-import--file-sha256 file)))))
              (dolist (buffer (seq-difference (buffer-list) buffers))
                (with-current-buffer buffer (set-buffer-modified-p nil))
                (kill-buffer buffer)))))))))

(ert-deftest gnosis-import-owner-independent-previews-and-details ()
  "Two previews retain independent content, details and normal refresh."
  (gnosis-test-import-owner--with-preview
    (let ((first-text (buffer-string))
          (first-source gnosis-import--source-id)
          (other (expand-file-name "other.gnosis" gnosis-dir)))
      (gnosis-export-db other)
      (gnosis-sqlite-execute gnosis-db "UPDATE themata SET keimenon = ?" '("Local C"))
      (gnosis-import-db other)
      (let ((second (current-buffer)))
        (should-not (eq preview second))
        (with-current-buffer preview
          (should (equal first-text (buffer-string)))
          (should (equal first-source gnosis-import--source-id)))
        (goto-char (point-min))
        (call-interactively (key-binding (kbd "RET")))
        (with-current-buffer preview
          (goto-char (point-min))
          (call-interactively (key-binding (kbd "RET"))))))))

(ert-deftest gnosis-import-owner-file-association-retires-commands ()
  "Association and detachment cannot revive apply, detail or retained refresh."
  (dolist (detach '(nil t))
    (gnosis-test-import-owner--with-preview
      (let ((facts (gnosis-test-import-owner--facts))
            (hash (gnosis-import--file-sha256 file))
            (refresh revert-buffer-function))
        (gnosis-test-import-owner--sentinel detach)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (should-error (call-interactively (key-binding (kbd "a"))) :type 'user-error))
        (should-error (call-interactively (key-binding (kbd "RET"))) :type 'user-error)
        (dotimes (_ 2)
          (should-error (funcall refresh nil t) :type 'user-error))
        (should (equal "Successor UNSAVED γ" (buffer-string)))
        (should (buffer-modified-p))
        (should (equal facts (gnosis-test-import-owner--facts)))
        (should (equal hash (gnosis-import--file-sha256 file)))))))

(ert-deftest gnosis-import-owner-confirmation-replacement ()
  "Confirmation cannot authorize a retired preview or kill its successor."
  (dolist (replacement '(associate detach mode kill refresh))
    (gnosis-test-import-owner--with-preview
      (let ((facts (gnosis-test-import-owner--facts))
            (hash (gnosis-import--file-sha256 file))
            successor reached)
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (setq reached t)
                     (pcase replacement
                       ('associate (gnosis-test-import-owner--sentinel))
                       ('detach (gnosis-test-import-owner--sentinel t))
                       ('mode (gnosis-import-diff-mode))
                       ('kill (kill-buffer preview))
                       ('refresh (funcall revert-buffer-function nil t)))
                     (unless (eq replacement 'refresh)
                       (gnosis-import-db file)
                       (setq successor (current-buffer)))
                     t)))
          (if (eq replacement 'refresh)
              (call-interactively (key-binding (kbd "a")))
            (should-error (call-interactively (key-binding (kbd "a"))) :type 'user-error)))
        (should reached)
        (if (eq replacement 'refresh)
            (should (equal "Incoming α" (gnosis-get 'keimenon 'themata `(= id ,id))))
          (should (equal facts (gnosis-test-import-owner--facts)))
          (should (buffer-live-p successor))
          (with-current-buffer successor
            (should (derived-mode-p 'gnosis-import-diff-mode)))
          (when (memq replacement '(associate detach))
            (with-current-buffer preview
              (should (equal "Successor UNSAVED γ" (buffer-string)))
              (should (buffer-modified-p)))))
        (should (equal hash (gnosis-import--file-sha256 file)))))))

(ert-deftest gnosis-import-owner-detail-reassociation ()
  "Repeated details preserve a repurposed detail, even after detachment."
  (dolist (detach '(nil t))
    (gnosis-test-import-owner--with-preview
      (goto-char (point-min))
      (let (detail replacement)
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (buffer &rest _) (setq detail buffer))))
          (call-interactively (key-binding (kbd "RET"))))
        (with-current-buffer detail
          (gnosis-test-import-owner--sentinel detach)
          (rename-buffer "*Gnosis Import Detail*")
          ;; A saved successor is still foreign after detachment.
          (set-buffer-modified-p nil))
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (buffer &rest _) (setq replacement buffer))))
          (call-interactively (key-binding (kbd "RET"))))
        (should-not (eq detail replacement))
        (with-current-buffer detail
          (should (equal "Successor UNSAVED γ" (buffer-string)))
          (should-not (buffer-modified-p)))
        (with-current-buffer replacement
          (should (string-search "Incoming α" (buffer-string))))))))

(ert-deftest gnosis-import-owner-decline-and-quit-preserve-facts ()
  "Decline and quit leave reviewed contents and all destination rows intact."
  (dolist (answer '(decline quit))
    (gnosis-test-import-owner--with-preview
      (let ((facts (gnosis-test-import-owner--facts))
            (text (buffer-string))
            (hash (gnosis-import--file-sha256 file))
            cancelled)
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _) (when (eq answer 'quit) (signal 'quit nil)) nil)))
          (condition-case nil
              (call-interactively (key-binding (kbd "a")))
            ((user-error quit) (setq cancelled t))))
        (should cancelled)
        (should (equal text (buffer-string)))
        (should (equal facts (gnosis-test-import-owner--facts)))
        (should (equal hash (gnosis-import--file-sha256 file)))))))

(ert-deftest gnosis-import-owner-detail-preserves-modified-namesake ()
  "Native RET preserves unrelated modified text under the detail name."
  (gnosis-test-import-owner--with-preview
    (let ((foreign (generate-new-buffer "*Gnosis Import Detail*")))
      (with-current-buffer foreign (insert "Unrelated detail draft"))
      (goto-char (point-min))
      (call-interactively (key-binding (kbd "RET")))
      (with-current-buffer foreign
        (should (equal "Unrelated detail draft" (buffer-string)))
        (should (buffer-modified-p))))))

(ert-deftest gnosis-import-owner-detail-reuse-and-preview-refresh ()
  "Same-owner refresh and repeated RET retain one readable detail buffer."
  (gnosis-test-import-owner--with-preview
    (let (first second)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (buffer &rest _) (setq first buffer))))
        (call-interactively (key-binding (kbd "RET"))))
      (call-interactively (key-binding (kbd "g")))
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (buffer &rest _) (setq second buffer))))
        (call-interactively (key-binding (kbd "RET"))))
      (should (eq first second))
      (with-current-buffer first
        (should buffer-read-only)
        (should-not (buffer-modified-p))
        (should (string-search "Incoming α" (buffer-string)))))))

(ert-deftest gnosis-import-owner-postcommit-cleanup-keeps-successor ()
  "Successful apply cannot kill a successor selected by post-commit work."
  (dolist (repurpose '(nil t))
    (gnosis-test-import-owner--with-preview
      (let (successor)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'gnosis-import--commit)
                   (lambda (&rest _)
                     (if repurpose
                         (progn
                           (gnosis-test-import-owner--sentinel t)
                           (setq successor preview))
                       (setq successor (generate-new-buffer "successor"))
                       (pop-to-buffer successor)
                       (insert "Successor UNSAVED γ")))))
          (call-interactively (key-binding (kbd "a"))))
        (should (equal "Incoming α" (gnosis-get 'keimenon 'themata `(= id ,id))))
        (should (buffer-live-p successor))
        (with-current-buffer successor
          (should (equal "Successor UNSAVED γ" (buffer-string)))
          (should (buffer-modified-p)))))))

(ert-deftest gnosis-import-owner-transaction-drift-rolls-back ()
  "Ownership lost after real writes rolls back before transaction commit."
  (gnosis-test-import-owner--with-preview
    (let ((facts (gnosis-test-import-owner--facts))
          (writer (symbol-function 'gnosis-import--write-changes))
          reached)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'gnosis-import--write-changes)
                 (lambda (&rest args)
                   (apply writer args)
                   (setq reached t)
                   (gnosis-test-import-owner--sentinel t))))
        (should-error (call-interactively (key-binding (kbd "a"))) :type 'user-error))
      (should reached)
      (should (equal facts (gnosis-test-import-owner--facts)))
      (should (equal "Successor UNSAVED γ" (buffer-string)))
      (should (buffer-modified-p)))))

(ert-deftest gnosis-import-owner-confirmation-database-replacement ()
  "A second database and preview cannot acquire an old confirmation."
  (gnosis-test-import-owner--with-preview
    (let* ((original gnosis-db)
           (before (gnosis-test-import-owner--facts))
           (hash (gnosis-import--file-sha256 file))
           (second (gnosis-sqlite-open (expand-file-name "second.db" gnosis-dir)))
           (other (expand-file-name "other.gnosis" gnosis-dir))
           other-before other-hash successor)
      (unwind-protect
          (progn
            (let ((gnosis-db second))
              (gnosis-db-init)
              (gnosis-test--add-basic-thema "Other incoming" "B" nil nil id)
              (gnosis-export-db other)
              (gnosis-sqlite-execute gnosis-db "UPDATE themata SET keimenon = ?"
                                     '("Other local"))
              (setq other-before (gnosis-test-import-owner--facts)
                    other-hash (gnosis-import--file-sha256 other)))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _)
                         (setq gnosis-db second)
                         (gnosis-import-db other)
                         (setq successor (current-buffer))
                         t)))
              (should-error (call-interactively (key-binding (kbd "a"))) :type 'user-error))
            (should (equal other-before (gnosis-test-import-owner--facts)))
            (should (equal other-hash (gnosis-import--file-sha256 other)))
            (should (buffer-live-p successor))
            (with-current-buffer successor
              (should (equal other gnosis-import--file))
              (should (string-search "Other incoming" (buffer-string))))
            (setq gnosis-db original)
            (should (equal before (gnosis-test-import-owner--facts)))
            (should (equal hash (gnosis-import--file-sha256 file))))
        (setq gnosis-db original)
        (sqlite-close second)))))

(ert-deftest gnosis-import-owner-save-detach-does-not-revive-preview ()
  "Saving and detaching a repurposed preview cannot restore its old commands."
  (gnosis-test-import-owner--with-preview
    (let ((facts (gnosis-test-import-owner--facts))
          (refresh revert-buffer-function)
          successor)
      (gnosis-test-import-owner--sentinel)
      (setq successor buffer-file-name)
      (save-buffer)
      (should-not (buffer-modified-p))
      (set-visited-file-name nil t)
      (should-not (buffer-modified-p))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (should-error (call-interactively (key-binding (kbd "a"))) :type 'user-error))
      (should-error (call-interactively (key-binding (kbd "RET"))) :type 'user-error)
      (should-error (funcall refresh nil t) :type 'user-error)
      (should (equal "Successor UNSAVED γ" (buffer-string)))
      (should-not (buffer-modified-p))
      (kill-buffer preview)
      (with-current-buffer (find-file-noselect successor)
        (should (equal "Successor UNSAVED γ" (buffer-string))))
      (should (equal facts (gnosis-test-import-owner--facts))))))

(ert-deftest gnosis-import-owner-refresh-hook-keeps-ownership ()
  "Run native refresh hooks, but never render over a successor they install."
  (gnosis-test-import-owner--with-preview
    (let ((text (buffer-string))
          (facts (gnosis-test-import-owner--facts))
          reached)
      (let ((tabulated-list-revert-hook (list (lambda () (setq reached t)))))
        (call-interactively (key-binding (kbd "g"))))
      (should reached)
      (should (equal text (buffer-string)))
      (let ((tabulated-list-revert-hook
             (list (lambda () (gnosis-test-import-owner--sentinel t)))))
        (should-error (call-interactively (key-binding (kbd "g"))) :type 'user-error))
      (should (equal "Successor UNSAVED γ" (buffer-string)))
      (should (buffer-modified-p))
      (should (equal facts (gnosis-test-import-owner--facts))))))

(provide 'gnosis-test-import-owner)
;;; gnosis-test-import-owner.el ends here
