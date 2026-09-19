;;; gnosis-test-anki-owner.el --- Import destination tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Exercise public import prompts against real source and destination databases.

;;; Code:

(require 'ert)
(require 'gnosis-test-anki)

(defmacro gnosis-test-anki-owner--with-databases (&rest body)
  "Run BODY with two disposable destinations and a one-note Anki source."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "gnosis-anki-owner-" t))
          (gnosis-testing t)
          (gnosis-vc-auto-push nil)
          (gnosis-dir (expand-file-name "a" root))
          (a (gnosis-db--open gnosis-dir))
          (b (gnosis-db--open (expand-file-name "b" root)))
          (gnosis-db a)
          (source (expand-file-name "collection.anki2" root))
          (source-db (sqlite-open source)))
     (unwind-protect
         (progn
           (gnosis-test-anki--create-schema source-db)
           (gnosis-test-anki--insert-notetype source-db 1 "Basic" 'basic)
           (gnosis-test-anki--insert-note
            source-db 1 1 (concat "Question" (string 31) "Answer") " test ")
           (sqlite-close source-db)
           ,@body)
       (ignore-errors (sqlite-close source-db))
       (sqlite-close a)
       (sqlite-close b)
       (delete-directory root t))))

(ert-deftest gnosis-test-anki-owner-prompt-replacement ()
  "Replacement during any public prompt refuses without importing."
  (dolist (phase '(filename tag suspend))
    (gnosis-test-anki-owner--with-databases
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _)
                   (when (eq phase 'filename) (setq gnosis-db b)) source))
		((symbol-function 'read-string)
                 (lambda (&rest _)
                   (when (eq phase 'tag) (setq gnosis-db b)) ""))
		((symbol-function 'y-or-n-p)
                 (lambda (&rest _)
                   (when (eq phase 'suspend) (setq gnosis-db b)) nil))
		((symbol-function 'run-with-timer) (lambda (&rest _) nil)))
	(should-error (call-interactively #'gnosis-import-anki) :type 'user-error))
      (should (equal (sqlite-select a "SELECT count(*) FROM themata") '((0))))
      (should (equal (sqlite-select b "SELECT count(*) FROM themata") '((0)))))))

(ert-deftest gnosis-test-anki-owner-prompt-quit-stays-lazy ()
  "Cancelled public input does not initialize a destination."
  (dolist (phase '(filename tag suspend))
    (gnosis-test-anki-owner--with-databases
      (let ((gnosis-db nil)
            (gnosis-dir (expand-file-name "unopened" root)))
	(cl-letf (((symbol-function 'read-file-name)
                   (lambda (&rest _)
                     (when (eq phase 'filename) (signal 'quit nil)) source))
                  ((symbol-function 'read-string)
                   (lambda (&rest _)
                     (when (eq phase 'tag) (signal 'quit nil)) ""))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) (signal 'quit nil))))
          (should (eq (condition-case nil
                          (call-interactively #'gnosis-import-anki)
			(quit 'cancelled))
                      'cancelled)))
	(should-not gnosis-db)
	(should-not (file-exists-p gnosis-dir)))
      (should (equal (sqlite-select a "SELECT count(*) FROM themata") '((0))))
      (should (equal (sqlite-select b "SELECT count(*) FROM themata") '((0)))))))

(ert-deftest gnosis-test-anki-owner-lazy-directory-replacement ()
  "An unopened destination cannot change directory during input."
  (gnosis-test-anki-owner--with-databases
    (let* ((gnosis-db nil)
           (original (expand-file-name "unopened-a" root))
           (replacement (expand-file-name "unopened-b" root))
           (gnosis-dir original))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (setq gnosis-dir replacement) ""))
		((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
		((symbol-function 'run-with-timer) (lambda (&rest _) nil)))
	(should-error (gnosis-import-anki source) :type 'user-error))
      (should-not (file-exists-p original))
      (should-not (file-exists-p replacement)))))

(ert-deftest gnosis-test-anki-owner-preparation-replacement ()
  "Parsing and ID preparation cannot redirect an accepted import."
  (dolist (phase '(gnosis-anki--parse-anki-db gnosis-generate-ids))
    (gnosis-test-anki-owner--with-databases
      (let ((prepare (symbol-function phase)))
	(cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                  ((symbol-function phase)
                   (lambda (&rest args)
                     (prog1 (apply prepare args) (setq gnosis-db b))))
                  ((symbol-function 'run-with-timer) (lambda (&rest _) nil)))
          (should-error (gnosis-import-anki source) :type 'user-error)))
      (should (equal (sqlite-select a "SELECT count(*) FROM themata") '((0))))
      (should (equal (sqlite-select b "SELECT count(*) FROM themata") '((0)))))))

(defun gnosis-test-anki-owner--unchanged-import (kind)
  "Import KIND and check real content, tags and suspension."
  (gnosis-test-anki-owner--with-databases
    (let* ((default-directory (file-name-as-directory root))
           (file (if (eq kind 'archive)
                     (let ((archive (expand-file-name "source.apkg" root)))
                       (should (zerop (call-process
                                       (or (executable-find "7z")
                                           (executable-find "7za"))
                                       nil nil nil "a" "-tzip" archive
                                       "collection.anki2")))
                       archive)
                   source))
           (gnosis-db (unless (eq kind 'lazy) a))
           (gnosis-dir (if (eq kind 'lazy)
                           (expand-file-name "lazy" root) gnosis-dir)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "imported"))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (_seconds _repeat function &rest args)
                         (apply function args))))
              (if (eq kind 'programmatic)
                  (gnosis-anki--import-db file nil "imported" t)
		(gnosis-import-anki file)))
            (should (equal (gnosis-select 'keimenon 'themata nil t) '("Question")))
            (should (equal (gnosis-select 'answer 'themata nil t) '(("Answer"))))
            (should (member "imported" (gnosis-select 'tag 'thema-tag nil t)))
            (should (equal (sqlite-select gnosis-db "SELECT suspended FROM scheduler_state")
                           '((1))))
            (should (equal (sqlite-select b "SELECT count(*) FROM themata") '((0)))))
	(when (eq kind 'lazy) (sqlite-close gnosis-db))))))

(ert-deftest gnosis-test-anki-owner-unchanged-imports ()
  "Direct, lazy and programmatic imports preserve content and options."
  (dolist (kind '(direct lazy programmatic))
    (gnosis-test-anki-owner--unchanged-import kind)))

(ert-deftest gnosis-test-anki-owner-unchanged-archive ()
  "An unchanged destination accepts an actual archive import."
  (skip-unless (or (executable-find "7z") (executable-find "7za")))
  (gnosis-test-anki-owner--unchanged-import 'archive))

(ert-deftest gnosis-test-anki-owner-delayed-chunks ()
  "Once import starts, deferred chunks keep their captured destination."
  (gnosis-test-anki-owner--with-databases
    (let ((source-db (sqlite-open source))
          (gnosis-anki--chunk-size 1)
          pending)
      (unwind-protect
          (gnosis-test-anki--insert-note
           source-db 2 1 (concat "Second" (string 31) "Answer") " test ")
	(sqlite-close source-db))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
		((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
		((symbol-function 'run-with-timer)
                 (lambda (_seconds _repeat function &rest args)
                   (push (cons function args) pending))))
	(gnosis-import-anki source)
	(should (equal (sqlite-select a "SELECT count(*) FROM themata") '((1))))
	(setq gnosis-db b)
	(while pending
          (let ((callback (pop pending)))
            (apply (car callback) (cdr callback)))))
      (should (equal (sqlite-select a "SELECT count(*) FROM themata") '((2))))
      (should (equal (sqlite-select b "SELECT count(*) FROM themata") '((0)))))))

(provide 'gnosis-test-anki-owner)
;;; gnosis-test-anki-owner.el ends here
