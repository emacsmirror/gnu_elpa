;;; gnosis-test-buffer-ownership.el --- Named view ownership -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:
;; Named views must not replace unrelated unsaved drafts.

;;; Code:

(require 'ert)
(require 'gnosis-dashboard)
(require 'gnosis-monkeytype)
(require 'gnosis-test-helpers)

(defun gnosis-test-buffer--snapshot (buffer)
  "Return the text, mode, modification and file identity of BUFFER."
  (with-current-buffer buffer
    (list (buffer-string) major-mode (buffer-modified-p)
          buffer-file-name buffer-file-truename)))

(defun gnosis-test-buffer--collision (entry)
  "Check that ENTRY preserves renamed drafts and visited files."
  (gnosis-test-with-db
    (dolist (filep '(nil t))
      (progn
        (let* ((file (expand-file-name "draft.txt" gnosis-dir))
               (buffer (if filep (find-file-noselect file)
                         (generate-new-buffer " *gnosis-draft*")))
               (gnosis-dashboard-buffer-name " *gnosis-collision*")
               (gnosis-monkeytype-buffer-name gnosis-dashboard-buffer-name)
               (gnosis-dashboard-timer-delay 60)
               (gnosis-script-input-method-alist nil))
          (unwind-protect
              (save-window-excursion
                (with-current-buffer buffer
                  (rename-buffer (if (eq entry 'gnosis-dashboard-history)
                                     "*Gnosis History*"
                                   gnosis-dashboard-buffer-name))
                  (insert "Unrelated unsaved draft"))
                (let ((before (gnosis-test-buffer--snapshot buffer)))
                  (cl-letf (((symbol-function 'keymap-popup) #'ignore)
                            ((symbol-function 'recursive-edit) #'ignore))
                    (should-error
                     (if (eq entry 'gnosis-monkeytype)
                         (gnosis-monkeytype "Typing target")
                       (if (eq entry 'gnosis-dashboard-output-themata)
                           (funcall entry nil)
                         (call-interactively entry)))
                     :type 'user-error))
                  (should (equal before (gnosis-test-buffer--snapshot buffer)))))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest gnosis-buffer-ownership-dashboard-collision ()
  (dolist (entry '(gnosis-dashboard gnosis-dashboard-output-themata
                  gnosis-dashboard-output-tags gnosis-dashboard-output-nodes))
    (gnosis-test-buffer--collision entry)))

(ert-deftest gnosis-buffer-ownership-history-collision ()
  (gnosis-test-buffer--collision #'gnosis-dashboard-history))

(ert-deftest gnosis-buffer-ownership-monkeytype-collision ()
  (gnosis-test-buffer--collision #'gnosis-monkeytype))

(ert-deftest gnosis-buffer-ownership-dashboard-reuse-and-repurpose ()
  "Owned dashboard views reuse their buffer; repurposed views are refused."
  (gnosis-test-with-db
    (let ((gnosis-dashboard-buffer-name " *gnosis-owned-dashboard*")
          (gnosis-dashboard-timer-delay 60)
          buffer)
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'keymap-popup) #'ignore))
              (call-interactively #'gnosis-dashboard)
              (setq buffer (get-buffer gnosis-dashboard-buffer-name))
              (switch-to-buffer buffer)
              (gnosis-dashboard-output-themata nil)
              (should (eq buffer (current-buffer)))
              (gnosis-dashboard-output-tags nil)
              (should (eq buffer (current-buffer)))
              (call-interactively #'gnosis-dashboard-output-nodes)
              (should (eq buffer (current-buffer)))
              (call-interactively #'gnosis-dashboard)
              (should (eq buffer (current-buffer)))
              (text-mode)
              (setq buffer-read-only nil)
              (erase-buffer)
              (insert "Successor draft")
              (let ((before (gnosis-test-buffer--snapshot buffer)))
                (should-error (gnosis-dashboard) :type 'user-error)
                (should (equal before (gnosis-test-buffer--snapshot buffer))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest gnosis-buffer-ownership-history-reuse-and-repurpose ()
  "History reuses its own view, not an unrelated tabulated list."
  (gnosis-test-with-db
    (let (buffer)
      (unwind-protect
          (save-window-excursion
            (gnosis-dashboard-history '((20260101 2 1)))
            (setq buffer (current-buffer))
            (gnosis-dashboard-history '((20260102 3 1)))
            (should (eq buffer (current-buffer)))
            (should (string-match-p "2026/01/02" (buffer-string)))
            (tabulated-list-mode)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "Unrelated table draft"))
            (let ((before (gnosis-test-buffer--snapshot buffer)))
              (should-error (call-interactively #'gnosis-dashboard-history)
                            :type 'user-error)
              (should (equal before (gnosis-test-buffer--snapshot buffer)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest gnosis-buffer-ownership-monkeytype-reuse-rename-and-cancel ()
  "Typing and native cancel act on the exact owned buffer after a rename."
  (let ((gnosis-monkeytype-buffer-name " *gnosis-owned-typing*")
        (gnosis-script-input-method-alist nil)
        buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'recursive-edit) #'ignore))
            (gnosis-monkeytype "old"))
          (setq buffer (get-buffer gnosis-monkeytype-buffer-name))
          (cl-letf (((symbol-function 'recursive-edit)
                     (lambda ()
                       (should (eq buffer (current-buffer)))
                       (rename-buffer " *gnosis-renamed-typing*")
                       (let ((this-command 'self-insert-command)
                             (last-command-event ?a))
                         (call-interactively #'self-insert-command))
                       (should (equal (buffer-substring-no-properties
                                       (point-min) (point-max)) "ab"))
                       (should (= (point) 2))
                       (call-interactively (key-binding (kbd "C-c C-k"))))))
            (catch 'monkeytype-loop (gnosis-monkeytype "ab")))
          (should-not (buffer-live-p buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest gnosis-buffer-ownership-monkeytype-refuses-repurpose ()
  "A previous typing buffer no longer owns text after changing major mode."
  (let ((gnosis-monkeytype-buffer-name " *gnosis-owned-typing*")
        (gnosis-script-input-method-alist nil)
        buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'recursive-edit) #'ignore))
            (gnosis-monkeytype "old"))
          (setq buffer (get-buffer gnosis-monkeytype-buffer-name))
          (with-current-buffer buffer
            (text-mode)
            (erase-buffer)
            (insert "Successor draft")
            (let ((before (gnosis-test-buffer--snapshot buffer)))
              (cl-letf (((symbol-function 'recursive-edit) #'ignore))
                (should-error (gnosis-monkeytype "new") :type 'user-error))
              (should (equal before (gnosis-test-buffer--snapshot buffer)))
              (should-error (call-interactively #'gnosis-monkeytype-exit)
                            :type 'user-error)
              (should (equal before (gnosis-test-buffer--snapshot buffer))))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest gnosis-buffer-ownership-refuses-owned-views-visiting-files ()
  "Keeping the view mode does not authorize replacing a newly visited file."
  (gnosis-test-with-db
    (dolist (entry '(gnosis-dashboard gnosis-dashboard-history gnosis-monkeytype))
      (let ((gnosis-dashboard-buffer-name " *gnosis-file-dashboard*")
            (gnosis-monkeytype-buffer-name " *gnosis-file-typing*")
            (gnosis-dashboard-timer-delay 60)
            (gnosis-script-input-method-alist nil)
            buffer)
        (unwind-protect
            (save-window-excursion
              (cl-letf (((symbol-function 'keymap-popup) #'ignore)
                        ((symbol-function 'recursive-edit) #'ignore))
                (if (eq entry 'gnosis-monkeytype)
                    (gnosis-monkeytype "abc")
                  (call-interactively entry))
                (setq buffer (get-buffer
                              (pcase entry
                                ('gnosis-dashboard gnosis-dashboard-buffer-name)
                                ('gnosis-monkeytype gnosis-monkeytype-buffer-name)
                                (_ "*Gnosis History*"))))
                (with-current-buffer buffer
                  ;; A file association need not retire the major mode.
                  (setq buffer-file-name
                        (expand-file-name "successor.txt" gnosis-dir)
                        buffer-file-truename buffer-file-name)
                  (rename-buffer
                   (pcase entry
                     ('gnosis-dashboard gnosis-dashboard-buffer-name)
                     ('gnosis-monkeytype gnosis-monkeytype-buffer-name)
                     (_ "*Gnosis History*")))
                  (let ((inhibit-read-only t)
                        (inhibit-modification-hooks t))
                    (erase-buffer)
                    (insert "Unsaved file successor"))
                  (let ((before (gnosis-test-buffer--snapshot buffer)))
                    (should-error
                     (if (eq entry 'gnosis-monkeytype)
                         (gnosis-monkeytype "replacement")
                       (call-interactively entry))
                     :type 'user-error)
                    (when (eq entry 'gnosis-monkeytype)
                      (should-error
                       (call-interactively (key-binding (kbd "C-c C-k")))
                       :type 'user-error))
                    (should (equal before (gnosis-test-buffer--snapshot buffer)))))))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(provide 'gnosis-test-buffer-ownership)
;;; gnosis-test-buffer-ownership.el ends here
