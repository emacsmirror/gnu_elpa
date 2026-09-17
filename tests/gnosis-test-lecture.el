;;; gnosis-test-lecture.el --- Lecture attachments and portability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Native draft and immutable media exchange regressions.  Real PDF rendering
;; and graphical review are separately exercised by the disposable native probe.

;;; Code:

(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-lecture)
(require 'gnosis-export-import)
(require 'gnosis-review)

(defun gnosis-test-lecture--png ()
  "Return a tiny original RGB PNG in the disposable test directory."
  (let ((file (expand-file-name "Διάλεξη [image] #51.png" gnosis-dir))
        (coding-system-for-write 'no-conversion))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert (base64-decode-string
               "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGCAIAAABxZ0isAAAAEUlEQVR4nGP4UKGBFTEMpAQAIGBLAbAg65EAAAAASUVORK5CYII=")))
    file))

(defmacro gnosis-test-lecture--draft (&rest body)
  "Run BODY in an owned native draft."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (gnosis-edit-mode)
     (setq gnosis--draft-db gnosis-db)
     (gnosis-export--insert-thema "NEW" "basic" "Question" "Hint" "Answer" "Explanation")
     ,@body))

(ert-deftest gnosis-lecture-image-native-save-reopen ()
  (gnosis-test-with-db
   (let ((file (gnosis-test-lecture--png)))
     (gnosis-test-lecture--draft
      (should (eq (key-binding (kbd "C-c C-l")) #'gnosis-lecture-attach))
      (gnosis-lecture-attach file)
      (let* ((thema (car (gnosis-export-parse-themata)))
             (text (nth 5 thema))
             (reference (car (gnosis-image-references text)))
             (source (cdar (gnosis-lecture-sources text))))
        (should (equal (nth 2 thema) "Question"))
        (should (equal (gnosis-lecture--target (cdr source)) (list file nil)))
        (should (equal (alist-get 'source (gnosis-image-resolve reference))
                       (gnosis-lecture--citation file nil)))
        (gnosis-save-thema thema)
        (let ((id (car (gnosis-select 'id 'themata nil t))))
          (should (equal text (gnosis-get 'parathema 'extras `(= id ,id))))
          (let ((inhibit-read-only t)) (erase-buffer))
          (gnosis-export--insert-themata (list id))
          (should (equal text (nth 5 (car (gnosis-export-parse-themata)))))))))))

(ert-deftest gnosis-lecture-narrowed-draft-and-alias-section ()
  (gnosis-test-with-db
   (let ((file (gnosis-test-lecture--png)))
     (gnosis-test-lecture--draft
      (goto-char (point-max))
      (insert "** Accepted aliases\n- Other\n")
      (narrow-to-region (point-min) (1+ (point-min)))
      (gnosis-lecture-attach file)
      (widen)
      (let ((thema (car (gnosis-export-parse-themata))))
        (should (gnosis-image-references (nth 5 thema)))
        (should (equal (nth 8 thema) '("Other"))))))))

(ert-deftest gnosis-lecture-source-unicode-and-physical-page ()
  (let* ((file "/tmp/Διάλεξη [physical pages] #51.pdf")
         (citation (gnosis-lecture--citation file 2))
         (path (cddar (gnosis-lecture-sources citation))))
    (should (equal (gnosis-lecture--target path) (list file 2)))
    (dolist (bad '("0:/ssh:host:/tmp/file.pdf" "2:relative.pdf" "bad"))
      (should-error (gnosis-lecture--target bad)))))

(ert-deftest gnosis-lecture-image-copy-drift-preserves-draft ()
  (gnosis-test-with-db
   (let ((file (gnosis-test-lecture--png)) (copy (symbol-function 'copy-file)))
     (gnosis-test-lecture--draft
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'copy-file)
                   (lambda (from to &rest args)
                     (apply copy from to args)
                     (with-temp-file to (insert "replacement")))))
          (should-error (gnosis-lecture-attach file)))
        (should (equal before (buffer-string)))
        (should-not gnosis-lecture--job)
        (should-not (file-exists-p (gnosis-assets-root))))))))

(ert-deftest gnosis-lecture-prompts-cannot-retarget-draft ()
  (gnosis-test-with-db
   (let ((file (gnosis-test-lecture--png)))
     (gnosis-test-lecture--draft
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _) (goto-char (point-max)) (insert "new text") file)))
        (should-error (call-interactively #'gnosis-lecture-attach)))
      (should-not (gnosis-image-references (buffer-string)))
      (should-not gnosis-lecture--job)))))

(ert-deftest gnosis-lecture-late-publication-refuses-owner-changes ()
  (gnosis-test-with-db
   (let ((file (gnosis-test-lecture--png)))
     (dolist (change '(edit mode file cancel database))
       (gnosis-test-lecture--draft
        (let* ((job (list :buffer (current-buffer) :database gnosis-db
                          :tick (buffer-chars-modified-tick) :position (point-max)
                          :file file :digest (gnosis-lecture--digest file)))
               (other nil))
          (setq gnosis-lecture--job job)
          (unwind-protect
              (progn
                (pcase change
                  ('edit (insert "successor"))
                  ('mode (text-mode))
                  ('file (setq buffer-file-name (expand-file-name "other.org" gnosis-dir)))
                  ('cancel (gnosis-lecture-cancel))
                  ('database (setq other (gnosis-sqlite-open (expand-file-name "other.db" gnosis-dir)))))
                (let ((before (buffer-string)) (gnosis-db (or other gnosis-db)))
                  (should-error (gnosis-lecture--publish job file))
                  (should (equal before (buffer-string)))))
            (when other (sqlite-close other))
            (gnosis-lecture--cleanup job))))))))

(ert-deftest gnosis-lecture-unavailable-input-preserves-draft ()
  (gnosis-test-with-db
   (let ((file (expand-file-name "bad.pdf" gnosis-dir)))
     (with-temp-file file (insert "%PDF-invalid"))
     (gnosis-test-lecture--draft
      (let ((before (buffer-string)))
        (dolist (page '(0 -1 1.5))
          (should-error (gnosis-lecture-attach file page)))
        (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
          (should-error (gnosis-lecture-attach file 1)))
        (should-error (gnosis-lecture-attach "/ssh:host:/lecture.pdf" 1))
        (should-error (gnosis-lecture-attach (concat file "-missing") 1))
        (should (equal before (buffer-string)))
        (should-not gnosis-lecture--job))))))

(ert-deftest gnosis-lecture-original-shorter-refuses-before-navigation ()
  (let ((opened nil))
    (cl-letf (((symbol-function 'gnosis-lecture--file) #'identity)
              ((symbol-function 'executable-find) (lambda (_) "/usr/bin/pdfinfo"))
              ((symbol-function 'process-file) (lambda (&rest _) (insert "Pages: 1\n") 0))
              ((symbol-function 'find-file) (lambda (&rest _) (setq opened t))))
      (should-error (gnosis-lecture-open "2:%2Ftmp%2Fshort.pdf") :type 'gnosis-lecture-error)
      (should-not opened))))

(ert-deftest gnosis-lecture-exchange-bundles-only-referenced-raster ()
  (gnosis-test-with-db
   (let* ((file (gnosis-test-lecture--png))
          (copy (expand-file-name "image.png" gnosis-dir))
          (export (expand-file-name "portable.gnosis" gnosis-dir)))
     (copy-file file copy)
     (let* ((reference (gnosis-image-import copy nil (gnosis-lecture--citation file 2)))
            (text (format "[[gnosis-image:%s]]\n%s" reference (gnosis-lecture--citation file 2)))
            (id (gnosis-test--add-basic-thema "Question" "Answer" nil text)))
       (gnosis-export-db export nil nil t)
       (should (= 5 (gnosis-import--format-version export)))
       (delete-file file)
       (gnosis-test-with-db
        (let ((diff (gnosis-import--diff export)))
          (should-not (file-exists-p (gnosis-assets-root)))
          (gnosis-import--apply-changes export (list id) nil (nth 2 diff) (nth 3 diff))
          (should (equal text (gnosis-get 'parathema 'extras `(= id ,id))))
          (should (gnosis-image-resolve reference))
          (should-error (gnosis-lecture-open (cddar (gnosis-lecture-sources text)))
                        :type 'gnosis-lecture-error)
          (should-not (gnosis-select '* 'review-events))
          (should-not (gnosis-select '* 'practice-encounters))))))))

(ert-deftest gnosis-lecture-exchange-corruption-and-paths-refused ()
  (gnosis-test-with-db
   (let* ((file (gnosis-test-lecture--png))
          (copy (expand-file-name "image.png" gnosis-dir))
          (export (expand-file-name "portable.gnosis" gnosis-dir)))
     (copy-file file copy)
     (let* ((reference (gnosis-image-import copy))
            (text (format "[[gnosis-image:%s]]" reference)))
       (gnosis-test--add-basic-thema "Question" "Answer" nil text)
       (gnosis-export-db export nil nil t)
       (dolist (sql '("UPDATE gnosis_media SET raster = 'broken'"
                      "UPDATE gnosis_media SET reference = '../image.json'"
                      "DELETE FROM gnosis_media"))
         (let ((bad (make-temp-file "gnosis-bad-bundle-" nil ".db")))
           (unwind-protect
               (progn
                 (copy-file export bad t)
                 (let ((db (sqlite-open bad)))
                   (unwind-protect (sqlite-execute db sql) (sqlite-close db)))
                 (gnosis-test-with-db
                  (should-error (gnosis-import--diff bad))
                  (should-not (gnosis-select '* 'themata))
                  (should-not (file-exists-p (gnosis-assets-root)))))
             (delete-file bad))))))))

(ert-deftest gnosis-lecture-async-success-stale-cancel-and-failure ()
  (gnosis-test-with-db
   (let* ((png (gnosis-test-lecture--png))
          (pdf (expand-file-name "lecture.pdf" gnosis-dir))
          (make (symbol-function 'make-process)))
     (with-temp-file pdf (insert "%PDF process-lifecycle fixture"))
     (dolist (case '(success edit cancel mode failure))
       (gnosis-test-lecture--draft
        (let ((original (buffer-string)) (sentinel nil) (job nil))
          (cl-letf (((symbol-function 'executable-find) (lambda (_) shell-file-name))
                    ((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq sentinel (plist-get args :sentinel))
                       (copy-file png (concat (car (last (plist-get args :command))) ".png"))
                       (funcall make :name "gnosis-lecture-test" :noquery t
                                :command (list shell-file-name shell-command-switch
                                               (if (eq case 'failure) "exit 1" "exit 0"))
                                :sentinel sentinel))))
            (setq job (gnosis-lecture-attach pdf 2)))
          (pcase case
            ('edit (goto-char (point-max)) (insert "successor"))
            ('cancel (call-interactively #'gnosis-lecture-cancel))
            ('mode (text-mode)))
          (let ((deadline (+ (float-time) 5)))
            (while (and (process-live-p (plist-get job :process)) (< (float-time) deadline))
              (accept-process-output (plist-get job :process) 0.02)))
          ;; Also deliver a queued stale callback after explicit retirement.
          (funcall sentinel (plist-get job :process) "finished\n")
          (should-not gnosis-lecture--job)
          (should-not (file-exists-p (plist-get job :directory)))
          (if (eq case 'success)
              (should (gnosis-image-references (nth 5 (car (gnosis-export-parse-themata)))))
            (should-not (gnosis-image-references (buffer-string)))
            (unless (eq case 'edit) (should (equal original (buffer-string)))))))))))

(provide 'gnosis-test-lecture)
;;; gnosis-test-lecture.el ends here
