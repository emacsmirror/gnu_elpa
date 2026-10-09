;; -*- lexical-binding: t; -*-
;; Copyright (C) 2024, 2025, 2026  Free Software Foundation, Inc.
;;
;; Author: Michelangelo Rodriguez <michelangelo.rodriguez@gmail.com>
;; License: GPLv3+

;;; Commentary:

;; Policy: every test in this file runs inside
;; `greader-dict-test-with-sandbox', directly or through
;; `with-greader-dict-test-buffer'.  The sandbox redirects every path
;; greader-dict can write to (dictionary files, merge records,
;; temporary files) into a throw-away directory, and rebinds the
;; global state the tests change, so running the suite inside a live
;; Emacs session, or in batch with the real HOME, never touches the
;; user's real dictionaries.  As a tripwire, any `write-region' to a
;; file outside the sandbox signals an error instead of writing.
;;
;; Even tests that write nothing today are wrapped: a function that is
;; pure now can start writing later (this is how
;; `greader-dict-add' started saving to disk on every addition), and
;; the sandbox costs nothing.  A test that needs real dictionary data
;; must copy it into the sandbox, never use it in place.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'greader-dict)

;;; Helpers

(defvar greader-dict-test--sandbox nil
  "Directory of the sandbox currently active, or nil.")

(defun greader-dict-test--tripwire (_start _end filename &rest _)
  "Signal an error if FILENAME is outside the active sandbox.
Installed as `:before' advice on `write-region' by
`greader-dict-test-with-sandbox'."
  (when (and greader-dict-test--sandbox
	     (stringp filename)
	     (not (file-in-directory-p (expand-file-name filename)
				       greader-dict-test--sandbox)))
    (error "greader-dict test tried to write outside the sandbox: %s"
	   filename)))

(defmacro greader-dict-test-with-sandbox (&rest body)
  "Run BODY with all greader-dict file I/O confined to a temporary directory.
The directory is deleted afterwards, and the global variables the
tests modify are restored."
  (declare (indent 0) (debug (body)))
  (let ((dir (make-symbol "dir")))
    `(let* ((,dir (file-name-as-directory
		   (make-temp-file "greader-dict-test-" t)))
	    (greader-dict-test--sandbox ,dir)
	    (user-emacs-directory ,dir)
	    (temporary-file-directory ,dir)
	    (greader--current-buffer nil)
	    (greader-dict--current-reading-buffer
	     greader-dict--current-reading-buffer)
	    (greader-dict-merge-dictionaries-alist nil)
	    (buffer-list-update-hook nil)
	    (greader-after-get-sentence-functions nil)
	    (greader-after-change-language-hook nil))
       (unwind-protect
	   ;; `greader-dict-directory' is automatically buffer-local, so
	   ;; a plain `let' could bind only the current buffer's value;
	   ;; bind the default value seen by every new buffer instead.
	   (cl-letf (((default-value 'greader-dict-directory)
		      (concat ,dir ".greader-dict/"
			      (greader-get-language) "/")))
	     (advice-add 'write-region :before
			 #'greader-dict-test--tripwire)
	     (unwind-protect
		 (progn ,@body)
	       (advice-remove 'write-region
			      #'greader-dict-test--tripwire)))
	 (delete-directory ,dir t)))))

(defmacro with-greader-dict-test-buffer (&rest body)
  "Run BODY in a sandboxed buffer with `greader-dict-mode' initialized.
See `greader-dict-test-with-sandbox'."
  (declare (indent defun) (debug (body)))
  `(greader-dict-test-with-sandbox
     (with-temp-buffer
       (greader-dict-mode 1)
       ,@body)))

;;; The sandbox itself

(ert-deftest greader-dict-test-sandbox-redirects-dictionary-file ()
  "Inside the sandbox the dictionary file lives in the sandbox."
  (with-greader-dict-test-buffer
    (should (file-in-directory-p (greader-dict--get-file-name)
				 greader-dict-test--sandbox))))

(ert-deftest greader-dict-test-sandbox-blocks-outside-writes ()
  "The tripwire refuses writes outside the sandbox, and only those."
  (let ((outside (expand-file-name
		  (make-temp-name "greader-dict-tripwire-")
		  temporary-file-directory)))
    (unwind-protect
	(greader-dict-test-with-sandbox
	  (should-error (write-region "x" nil outside nil 'silent))
	  (should-not (file-exists-p outside))
	  (let ((inside (expand-file-name "ok" greader-dict-test--sandbox)))
	    (write-region "x" nil inside nil 'silent)
	    (should (file-exists-p inside))))
      (when (file-exists-p outside)
	(delete-file outside)))
    ;; Once the sandbox is gone the advice must be gone too.
    (should-not (advice-member-p #'greader-dict-test--tripwire
				 'write-region))))

;;; greader-dict--merge / greader-dict--merged-p

(ert-deftest greader-dict--merge-sets-property ()
  "greader-dict--merge returns a key with the merged text property set."
  (greader-dict-test-with-sandbox
    (let ((key (greader-dict--merge "foo")))
      (should (greader-dict--merged-p key)))))

(ert-deftest greader-dict--merge-idempotent ()
  "greader-dict--merge on an already-merged key returns the same key."
  (greader-dict-test-with-sandbox
    (let* ((key1 (greader-dict--merge "foo"))
	   (key2 (greader-dict--merge key1)))
      (should (greader-dict--merged-p key2))
      (should (equal (substring-no-properties key1)
		     (substring-no-properties key2))))))

(ert-deftest greader-dict--merged-p-nil-for-plain-string ()
  "greader-dict--merged-p returns nil for a plain string."
  (greader-dict-test-with-sandbox
    (should-not (greader-dict--merged-p "foo"))))

;;; greader-dict-add with merge=t

(ert-deftest greader-dict-add-merge-marks-entry ()
  "greader-dict-add with MERGE non-nil marks the hash key as merged."
  (with-greader-dict-test-buffer
    (greader-dict-add "testword" "sostituzione" t)
    (let ((found nil))
      (maphash (lambda (k _v)
		 (when (equal (substring-no-properties k) "testword")
		   (setq found k)))
	       greader-dictionary)
      (should found)
      (should (greader-dict--merged-p found)))))

(ert-deftest greader-dict-add-no-merge-does-not-mark ()
  "greader-dict-add without MERGE does not mark the key as merged."
  (with-greader-dict-test-buffer
    (unwind-protect
	(progn
	  (greader-dict-add "testword2" "sostituzione")
	  (let ((found nil))
	    (maphash (lambda (k _v)
		       (when (equal (substring-no-properties k) "testword2")
			 (setq found k)))
		     greader-dictionary)
	    (should found)
	    (should-not (greader-dict--merged-p found))))
      (greader-dict-remove "testword2"))))

;;; greader-dict-write-file skips merged entries

(ert-deftest greader-dict-write-file-skips-merged ()
  "Merged entries are not written to the dictionary file.
Uses `cl-letf' to intercept `write-region' rather than touching the
filesystem, avoiding the `greader-dict-directory' path-rewriting done
by `greader-dict--get-file-name'."
  (greader-dict-test-with-sandbox
    (let (written-content)
      (cl-letf (((symbol-function 'write-region)
		 (lambda (start end _filename &rest _)
		   (setq written-content (buffer-substring start end)))))
	(with-temp-buffer
	  (setq greader-dict--current-reading-buffer (current-buffer))
	  (setq greader-dictionary (make-hash-table :test 'ignore-case))
	  (greader-dict-add "parola" "sostituzione")   ; normal entry
	  (greader-dict-add "fusa" "merged-value" t)   ; merged entry
	  (let ((greader-dict--saved-flag nil))
	    (greader-dict-write-file))))
      (should written-content)
      (should (string-match-p "parola" written-content))
      (should-not (string-match-p "fusa" written-content)))))

;;; greader-dict-merge-dictionary loads entries as merged and updates alist

(ert-deftest greader-dict-merge-dictionary-loads-and-marks ()
  "greader-dict-merge-dictionary loads entries from file marked as merged."
  (greader-dict-test-with-sandbox
    (let ((aux-file (make-temp-file "greader-dict-aux" nil ".dict")))
      (write-region "\"parola\"=sostituzione\n" nil aux-file)
      (with-temp-buffer
	(setq greader-dict--current-reading-buffer (current-buffer))
	(setq greader-dictionary (make-hash-table :test 'ignore-case))
	(let* ((main-file (greader-dict--get-file-name))
	       (greader-dict-merge-save nil))
	  (greader-dict-merge-dictionary aux-file)
	  ;; Entry must be present and marked merged.
	  (let ((found nil))
	    (maphash (lambda (k _v)
		       (when (equal (substring-no-properties k) "parola")
			 (setq found k)))
		     greader-dictionary)
	    (should found)
	    (should (greader-dict--merged-p found)))
	  ;; Alist must record the merge.
	  (let ((entry (assoc main-file greader-dict-merge-dictionaries-alist)))
	    (should entry)
	    (should (member aux-file (cdr entry)))))))))

(ert-deftest greader-dict-test-word-not-replaced-inside-other-words ()
  "A word entry replaces only whole words, wherever they appear.
Each case is an (INPUT . EXPECTED) pair for the entry re -> ré.  The
comparison is done on (INPUT . RESULT) pairs, so a failure report
shows which input went wrong.  Single-word inputs are left out on
purpose: `greader-dict-check-and-replace' appends a newline to them."
  (with-greader-dict-test-buffer
    (greader-dict-add "re" "ré")
    (dolist (case '(;; The whole word comes first.
		    ("re fare" . "ré fare")
		    ;; The longer word comes first, followed by a separator:
		    ;; the original bug turned "fare" into "faré".
		    ("fare re." . "fare ré.")
		    ("fare re re" . "fare ré ré")
		    ("il re, il re" . "il ré, il ré")
		    ;; The whole word is the last thing in the text, right
		    ;; after a rejected match: the separator between them
		    ;; must not be consumed by the rejected match.
		    ("fare re" . "fare ré")
		    ("fare, fare re" . "fare, fare ré")
		    ("rere re" . "rere ré")
		    ("re-fare re" . "ré-fare ré")
		    ;; Case is preserved.
		    ("Re e re." . "Ré e ré.")
		    ;; Nothing to replace at all.
		    ("fare dare" . "fare dare")))
      (should (equal case
		     (cons (car case)
			   (greader-dict-check-and-replace (car case))))))))

(provide 'greader-dict-tests)
;;; greader-dict-tests.el ends here

