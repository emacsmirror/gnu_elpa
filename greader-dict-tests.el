;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'greader-dict)

;;; Helpers

(defmacro with-greader-dict-test-buffer (&rest body)
  "Run BODY in a buffer with `greader-dict-mode' initialized."
  (declare (indent defun))
  `(with-temp-buffer
     (greader-dict-mode 1)
     ,@body))

;;; greader-dict--merge / greader-dict--merged-p

(ert-deftest greader-dict--merge-sets-property ()
  "greader-dict--merge returns a key with the merged text property set."
  (let ((key (greader-dict--merge "foo")))
    (should (greader-dict--merged-p key))))

(ert-deftest greader-dict--merge-idempotent ()
  "greader-dict--merge on an already-merged key returns the same key."
  (let* ((key1 (greader-dict--merge "foo"))
	 (key2 (greader-dict--merge key1)))
    (should (greader-dict--merged-p key2))
    (should (equal (substring-no-properties key1)
		   (substring-no-properties key2)))))

(ert-deftest greader-dict--merged-p-nil-for-plain-string ()
  "greader-dict--merged-p returns nil for a plain string."
  (should-not (greader-dict--merged-p "foo")))

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
    (greader-dict-add "testword2" "sostituzione")
    (let ((found nil))
      (maphash (lambda (k _v)
		 (when (equal (substring-no-properties k) "testword2")
		   (setq found k)))
	       greader-dictionary)
      (should found)
      (should-not (greader-dict--merged-p found)))))

;;; greader-dict-write-file skips merged entries

(ert-deftest greader-dict-write-file-skips-merged ()
  "Merged entries are not written to the dictionary file.
Uses `cl-letf' to intercept `write-region' rather than touching the
filesystem, avoiding the `greader-dict-directory' path-rewriting done
by `greader-dict--get-file-name'."
  (require 'cl-lib)
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
    (should-not (string-match-p "fusa" written-content))))

;;; greader-dict-merge-dictionary loads entries as merged and updates alist

(ert-deftest greader-dict-merge-dictionary-loads-and-marks ()
  "greader-dict-merge-dictionary loads entries from file marked as merged."
  (let ((aux-file (make-temp-file "greader-dict-aux" nil ".dict")))
    (unwind-protect
	(progn
	  (write-region "\"parola\"=sostituzione\n" nil aux-file)
	  (with-temp-buffer
	    (setq greader-dict--current-reading-buffer (current-buffer))
	    (setq greader-dictionary (make-hash-table :test 'ignore-case))
	    (setq greader-dict-merge-dictionaries-alist nil)
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
		(should (member aux-file (cdr entry)))))))
      (when (file-exists-p aux-file)
	(delete-file aux-file)))))

(provide 'greader-dict-tests)
