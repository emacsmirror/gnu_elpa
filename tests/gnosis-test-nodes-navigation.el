;;; gnosis-test-nodes-navigation.el --- Navigation ownership tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Exercise destination buffers without touching user notes or Org ID state.

;;; Code:

(require 'ert)
(require 'gnosis-nodes)

(defvar gnosis-journal-dir)

(defmacro gnosis-test-navigation-with-destination (&rest body)
  "Run BODY with a disposable Org destination and source buffer."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "gnosis-navigation-" t))
          (gnosis-nodes-dir (expand-file-name "nodes" dir))
          (gnosis-journal-dir (expand-file-name "journal" dir))
          (file (expand-file-name "destination.org" dir))
          (org-id-track-globally nil)
          ;; Explicit navigation must enable owned destinations even when the
          ;; automatic file hook has not enabled them already.
          (org-mode-hook nil)
          (destination nil))
     (unwind-protect
         (save-window-excursion
           (with-temp-file file
             (insert "* Destination\n:PROPERTIES:\n:ID: destination-id\n:END:\n"))
           (setq destination (find-file-noselect file))
           (with-temp-buffer
             (org-mode)
             (insert (format "[[file:%s]]" file))
             (goto-char (point-min))
             ,@body))
       (when (buffer-live-p destination)
         (kill-buffer destination))
       (delete-directory dir t))))

(ert-deftest gnosis-test-navigation-native-preserves-destination-ownership ()
  "Native links preserve both disabled and already enabled destination state."
  (dolist (enabled '(nil t))
    (dolist (id '(nil "unknown-id"))
      (gnosis-test-navigation-with-destination
        (with-current-buffer destination
          (when enabled (gnosis-nodes-mode 1)))
        (let ((hooks (buffer-local-value 'after-save-hook destination)))
          (cl-letf (((symbol-function 'gnosis-nodes-select)
                     (lambda (&rest _) nil)))
            (gnosis-nodes-goto-id id))
          (should (eq (current-buffer) destination))
          (with-current-buffer destination
            (should (eq gnosis-nodes-mode enabled))
            (should (equal after-save-hook hooks))
            (should (eq (not (null (memq #'gnosis-nodes-update-file
                                        after-save-hook)))
                        enabled)))
          (should-not org-id-track-globally))))))

(ert-deftest gnosis-test-navigation-owned-destination-enables-mode ()
  "Resolved node and journal IDs enable the actual destination save hook."
  (dolist (table '(nodes journal))
    (gnosis-test-navigation-with-destination
      (should-not (buffer-local-value 'gnosis-nodes-mode destination))
      ;; Isolate lookup routing; file visiting, ID positioning, mode and hooks
      ;; remain real.  The absolute fixture path works with either owned root.
      (cl-letf (((symbol-function 'gnosis-nodes-select)
                 (lambda (column candidate-table &rest _)
                   (when (and (eq column 'file) (eq candidate-table table))
                     (list file)))))
        (gnosis-nodes-goto-id "destination-id"))
      (should (eq (current-buffer) destination))
      (should (equal (org-id-get) "destination-id"))
      (should gnosis-nodes-mode)
      (should (local-variable-p 'after-save-hook))
      (should (memq #'gnosis-nodes-update-file after-save-hook))
      (should-not org-id-track-globally))))

(ert-deftest gnosis-test-navigation-failed-fallback-does-not-own-destination ()
  "A fallback error after switching buffers must not attach a save hook."
  (gnosis-test-navigation-with-destination
    (let ((hooks (buffer-local-value 'after-save-hook destination)))
      (cl-letf (((symbol-function 'org-open-at-point)
                 (lambda (&rest _)
                   (switch-to-buffer destination)
                   (user-error "Destination search failed"))))
        (should-error (gnosis-nodes-goto-id) :type 'user-error))
      (should (eq (current-buffer) destination))
      (should-not gnosis-nodes-mode)
      (should (equal after-save-hook hooks))
      (should-not (memq #'gnosis-nodes-update-file after-save-hook))
      (should-not org-id-track-globally))))

(provide 'gnosis-test-nodes-navigation)
;;; gnosis-test-nodes-navigation.el ends here
