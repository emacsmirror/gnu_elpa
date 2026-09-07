;;; gnosis-test-storage-hardening.el --- Storage UI ownership -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Mode-line ownership must not depend on database state or evaluation of
;; unrelated mode-line forms.

;;; Code:

(require 'ert)
(require 'gnosis)
(require 'gnosis-test-helpers)

(ert-deftest gnosis-modeline-disable-removes-empty-owned-entry ()
  "Disable the entry even when it has never displayed a due count."
  (let ((global-mode-string nil)
        (gnosis-modeline-mode nil)
        (gnosis-due-themata-total nil))
    (cl-letf (((symbol-function 'gnosis-review-get-due-themata) (lambda () nil)))
      (gnosis-modeline-mode 1)
      (should (= 1 (length global-mode-string)))
      (gnosis-modeline-mode -1)
      (setq gnosis-due-themata-total 3)
      (should-not global-mode-string)
      (gnosis-modeline-mode 1)
      (gnosis-modeline-mode 1)
      (should (= 1 (length global-mode-string)))
      (gnosis-modeline-mode -1)
      (should-not global-mode-string))))

(ert-deftest gnosis-modeline-disable-does-not-query-or-evaluate-others ()
  "Teardown removes only Gnosis's entry without querying storage."
  (let* ((other '(:eval (error "Unrelated mode-line form evaluated")))
         (global-mode-string (list "other" other))
         (gnosis-modeline-mode nil)
         (gnosis-due-themata-total nil))
    (cl-letf (((symbol-function 'gnosis-review-get-due-themata) (lambda () '(1))))
      (gnosis-modeline-mode 1))
    (cl-letf (((symbol-function 'gnosis-review-get-due-themata)
               (lambda () (error "Storage queried during disable"))))
      (gnosis-modeline-mode -1))
    (should (equal global-mode-string (list "other" other)))))

(ert-deftest gnosis-modeline-reentrant-query-disable ()
  "A due query that disables the mode must not reinstall its entry."
  (let ((global-mode-string nil)
        (gnosis-modeline-mode nil)
        (gnosis-due-themata-total nil))
    (cl-letf (((symbol-function 'gnosis-review-get-due-themata)
               (lambda () (gnosis-modeline-mode -1) '(1))))
      (gnosis-modeline-mode 1))
    (should-not gnosis-modeline-mode)
    (should-not global-mode-string)
    (setq gnosis-due-themata-total 3)
    (should-not global-mode-string)))

(ert-deftest gnosis-modeline-reentrant-watcher-disable ()
  "A native due-count watcher can disable the mode during activation."
  (gnosis-test-with-db
    (let ((global-mode-string nil)
          (gnosis-modeline-mode nil)
          (gnosis-due-themata-total nil)
          disabled)
      (let ((watcher
             (lambda (_symbol _value operation _where)
               (when (and (eq operation 'set) gnosis-modeline-mode
                          (not disabled))
                 (setq disabled t)
                 (gnosis-modeline-mode -1)))))
        (unwind-protect
            (progn
              (add-variable-watcher 'gnosis-due-themata-total watcher)
              (gnosis-modeline-mode 1)
              (should disabled)
              (should-not gnosis-modeline-mode)
              (should-not global-mode-string))
          (remove-variable-watcher 'gnosis-due-themata-total watcher)))
      (gnosis-test--add-basic-thema "Question" "Answer")
      (setq gnosis-due-themata-total (length (gnosis-review-get-due-themata)))
      (should (> gnosis-due-themata-total 0))
      (should-not gnosis-modeline-mode)
      (should-not global-mode-string))))

(ert-deftest gnosis-modeline-reentrant-publication-disable ()
  "A publication watcher can disable the mode without leaving its entry."
  (let* ((other '(:eval (error "Unrelated mode-line form evaluated")))
         (global-mode-string (list other))
         (gnosis-modeline-mode nil)
         (gnosis-due-themata-total nil)
         (queries 0)
         disabled)
    (let ((watcher
           (lambda (_symbol value operation _where)
             (when (and (eq operation 'set)
                        (member gnosis--modeline-entry value)
                        (not disabled))
               (setq disabled t)
               (gnosis-modeline-mode -1)))))
      (unwind-protect
          (progn
            (add-variable-watcher 'global-mode-string watcher)
            (cl-letf (((symbol-function 'gnosis-review-get-due-themata)
                       (lambda ()
                         (cl-incf queries)
                         (when disabled
                           (error "Storage queried after disable"))
                         '(1))))
              (gnosis-modeline-mode 1))
            (should disabled)
            (should-not gnosis-modeline-mode)
            (should (= 1 queries))
            (should (= 1 gnosis-due-themata-total))
            (should (equal (list other) global-mode-string)))
        (remove-variable-watcher 'global-mode-string watcher)))))

(provide 'gnosis-test-storage-hardening)
;;; gnosis-test-storage-hardening.el ends here
