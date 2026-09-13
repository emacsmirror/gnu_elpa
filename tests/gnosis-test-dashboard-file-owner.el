;;; gnosis-test-dashboard-file-owner.el --- Retained dashboard file owners -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:
;; File association does not change major mode.  Retained commands and timers
;; must nevertheless leave successor drafts and their data alone.

;;; Code:

(require 'gnosis-test-interaction-integration)
(require 'gnosis-review)

(defun gnosis-test-dashboard-file--draft ()
  "Turn this view into an unsaved file draft, retaining its major mode."
  (set-visited-file-name (expand-file-name "successor.txt" gnosis-dir) t)
  (read-only-mode -1)
  (erase-buffer)
  (insert "Successor unsaved draft λ")
  (let ((overlay (make-overlay (point-min) (point-max))))
    (overlay-put overlay 'gnosis-mark t)
    (overlay-put overlay 'face 'highlight))
  (setq header-line-format "Successor header"
        gnosis-dashboard--selected-ids '(successor)))

(defun gnosis-test-dashboard-file--snapshot ()
  "Return draft text, file, marks, modified flag and view state."
  (list (buffer-string) buffer-file-name (buffer-modified-p) (point)
        major-mode header-line-format
        (copy-sequence gnosis-dashboard--selected-ids)
        (mapcar (lambda (overlay)
                  (list overlay (overlay-start overlay) (overlay-end overlay)
                        (overlay-properties overlay)))
                (overlays-in (point-min) (point-max)))))

(ert-deftest gnosis-dashboard-file-confirmation-refuses-write ()
  "Final row confirmation cannot mutate a newly file-visiting projection."
  (dolist (key '("s" "d" "b"))
    (gnosis-test-with-db
      (dolist (id '(1 2 3 4))
        (gnosis-test--add-basic-thema (number-to-string id) "Answer" nil nil id))
      (gnosis--insert-into 'nodes '(["node-1" "disposable.org" "Target" "0" nil nil nil]))
      (gnosis-test-dashboard--with-view
        (gnosis-dashboard-output-themata '(1 2 3 4))
        (call-interactively (local-key-binding (kbd "m")))
        (tabulated-list-sort 0)
        (let ((before (gnosis-test-dashboard-owner--snapshot gnosis-db))
              (prompts 0) draft)
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "1"))
                    ((symbol-function 'gnosis-completing-read) (lambda (&rest _) "Target"))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _)
                       (cl-incf prompts)
                       (gnosis-test-dashboard-file--draft)
                       (setq draft (gnosis-test-dashboard-file--snapshot))
                       t)))
            (should-error (call-interactively (local-key-binding (kbd key)))
                          :type 'user-error))
          (should (= prompts 1))
          (drain)
          (should (equal draft (gnosis-test-dashboard-file--snapshot)))
          (should (equal before (gnosis-test-dashboard-owner--snapshot gnosis-db)))
          (set-buffer-modified-p nil))))))

(ert-deftest gnosis-dashboard-file-queued-render-preserves-draft ()
  "Sorted and unsorted retained append callbacks cannot touch file drafts."
  (dolist (sorted '(nil t))
    (dolist (modified '(nil t))
      (gnosis-test-with-db
        (dolist (id '(1 2 3 4))
          (gnosis-test--add-basic-thema (number-to-string id) "Answer" nil nil id))
        (gnosis-test-dashboard--with-view
          (gnosis-dashboard-output-themata '(1 2 3 4))
          (when sorted (tabulated-list-sort 0))
          (let ((callback (car callbacks))
                (owner (gnosis-dashboard--command-owner))
                (before (gnosis-test-dashboard-owner--snapshot gnosis-db)))
            (should callback)
            ;; Also prove the boundary itself, independently of hook cleanup.
            (let ((after-set-visited-file-name-hook nil))
              (gnosis-test-dashboard-file--draft))
            (set-buffer-modified-p modified)
            (let ((draft (gnosis-test-dashboard-file--snapshot)))
              (dotimes (_ 2) (apply (car callback) (cdr callback)))
              (should (equal draft (gnosis-test-dashboard-file--snapshot)))
              (should-error (gnosis-dashboard--command-owner owner) :type 'user-error)
              ;; Direct save-hook and settlement entry points are stale too.
              (gnosis-dashboard-update-entry 1)
              (gnosis-dashboard--finish-render t)
              (gnosis-dashboard--progressive-render
               (gnosis-dashboard--output-themata '(1 2 3 4))
               gnosis-dashboard--load-generation)
              (should-error (gnosis-dashboard--update-entries '(1)) :type 'user-error)
              (should-error (gnosis-dashboard--remove-entries '(1)) :type 'user-error)
              (should (equal draft (gnosis-test-dashboard-file--snapshot)))
              (should (equal before (gnosis-test-dashboard-owner--snapshot gnosis-db))))
          (set-buffer-modified-p nil)))))))

(ert-deftest gnosis-dashboard-file-deferred-statistics-and-audit ()
  "Statistics, idle launch, and audit delivery cannot claim a file draft."
  (dolist (phase '(stats idle audit))
    (gnosis-test-with-db
      (gnosis-test--add-basic-thema "Question" "Answer" nil nil 1)
      (let ((buffer (generate-new-buffer " *gnosis-file-stats*"))
            callbacks idle-callbacks)
        (unwind-protect
            (with-current-buffer buffer
              (gnosis-dashboard-mode)
              (let ((inhibit-read-only t)) (insert "Statistics placeholder"))
              (cl-letf (((symbol-function 'run-with-timer)
                         (lambda (_delay _repeat function &rest args)
                           (push (cons function args) callbacks) nil))
                        ((symbol-function 'run-with-idle-timer)
                         (lambda (_delay _repeat function &rest args)
                           (push (cons function args) idle-callbacks) nil)))
                (let* ((stats (list #'gnosis-dashboard--load-stats buffer
                                    (copy-marker (point-min))
                                    gnosis-dashboard--load-generation))
                       (before (gnosis-test-dashboard-owner--snapshot gnosis-db))
                       (callback
                        (pcase phase
                          ('stats stats)
                          ('idle (apply (car stats) (cdr stats)) (car idle-callbacks))
                          ('audit (gnosis-dashboard--compute-link-issues) (car callbacks)))))
                  (should callback)
                  (let ((after-set-visited-file-name-hook nil))
                    (gnosis-test-dashboard-file--draft))
                  (let ((draft (gnosis-test-dashboard-file--snapshot))
                        (timers (list callbacks idle-callbacks))
                        (audit gnosis-dashboard--link-audit)
                        (issues gnosis-dashboard--link-issues))
                    (dotimes (_ 2) (apply (car callback) (cdr callback)))
                    (gnosis-dashboard--compute-link-issues)
                    (should (equal draft (gnosis-test-dashboard-file--snapshot)))
                    (should (equal timers (list callbacks idle-callbacks)))
                    (should (eq audit gnosis-dashboard--link-audit))
                    (should (equal issues gnosis-dashboard--link-issues))
                    (should (equal before (gnosis-test-dashboard-owner--snapshot gnosis-db)))))))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(ert-deftest gnosis-dashboard-file-detachment-does-not-revive-owner ()
  "Removing a visited filename does not resurrect retired commands or work."
  (gnosis-test-with-db
    (dolist (id '(1 2 3 4))
      (gnosis-test--add-basic-thema (number-to-string id) "Answer" nil nil id))
    (gnosis-test-dashboard--with-view
      (gnosis-dashboard-output-themata '(1 2 3 4))
      (let ((owner (gnosis-dashboard--command-owner))
            (callback (car callbacks)))
        (gnosis-test-dashboard-file--draft)
        (should-not gnosis-dashboard--pending-entries)
        (should-not gnosis-dashboard--timer)
        (set-visited-file-name nil t)
        (let ((draft (gnosis-test-dashboard-file--snapshot)))
          (should-error (gnosis-dashboard--command-owner owner) :type 'user-error)
          (dotimes (_ 2) (apply (car callback) (cdr callback)))
          (should (equal draft (gnosis-test-dashboard-file--snapshot))))
        (set-buffer-modified-p nil)))))

(provide 'gnosis-test-dashboard-file-owner)
;;; gnosis-test-dashboard-file-owner.el ends here
