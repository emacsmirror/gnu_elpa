;;; gnosis-lecture.el --- Lecture illustrations in explanations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Attach a local raster or asynchronously rendered physical PDF page to a
;; native draft's Parathema.  Only the extracted raster is managed; citations
;; retain the external original, which is never bundled or fetched.

;;; Code:

(require 'gnosis-image)
(require 'ol)
(require 'org-element)
(require 'url-util)

(declare-function gnosis--draft-check-owner "gnosis")
(declare-function gnosis-export-parse-themata "gnosis-export-import" (&optional separator))
(declare-function pdf-view-goto-page "pdf-view" (page &optional window))
(declare-function doc-view-goto-page "doc-view" (page))

(define-error 'gnosis-lecture-error "Lecture media unavailable" 'user-error)

(defvar-local gnosis-lecture--job nil
  "Current lecture attachment job, or nil.")

(defun gnosis-lecture--file (file)
  "Return readable local regular FILE, refusing remote sources."
  (when (file-remote-p file) (user-error "Lecture media must be local"))
  (let ((file (expand-file-name file)))
    (unless (and (file-regular-p file) (file-readable-p file))
      (user-error "Lecture source unavailable: %s" file))
    file))

(defun gnosis-lecture--digest (file)
  "Return literal SHA256 of local FILE, bounded to 100 MiB."
  (gnosis-lecture--file file)
  (when (> (file-attribute-size (file-attributes file)) (* 100 1024 1024))
    (user-error "Lecture source exceeds 100 MiB"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun gnosis-lecture--citation (file page)
  "Return an actionable local FILE citation, optionally to physical PAGE."
  (org-link-make-string
   (format "gnosis-lecture:%d:%s" (or page 0) (url-hexify-string file))
   (format "%s%s" (file-name-nondirectory file)
           (if page (format ", physical page %d" page) ""))))

(defun gnosis-lecture--target (path)
  "Decode a lecture link PATH as (FILE PAGE), with nil for raster pages."
  (unless (string-match "\\`\\([0-9]+\\):\\(.+\\)\\'" path)
    (user-error "Malformed lecture source link"))
  (let ((page (string-to-number (match-string 1 path)))
        (file (decode-coding-string (url-unhex-string (match-string 2 path)) 'utf-8)))
    (unless (and (file-name-absolute-p file) (not (file-remote-p file)))
      (user-error "Lecture source must be an absolute local file"))
    (list file (and (> page 0) page))))

(defun gnosis-lecture--check-page (file page)
  "Require physical PAGE to exist in local PDF FILE using Poppler."
  (unless (executable-find "pdfinfo")
    (user-error "Install Poppler (pdfinfo) to verify the original PDF page"))
  (with-temp-buffer
    (let ((process-environment (cons "LC_ALL=C" process-environment)))
      (unless (and (= 0 (process-file "pdfinfo" nil t nil file))
                   (progn (goto-char (point-min))
                          (re-search-forward "^Pages:[ \t]+\\([0-9]+\\)" nil t))
                   (<= page (string-to-number (match-string 1))))
        (user-error "Original PDF page unavailable; verify the physical page and source")))))

(defun gnosis-lecture-open (path &optional _argument validate)
  "Open original lecture link PATH at its physical page.
Missing external originals are media errors; managed illustrations remain
usable.  Native DocView or an already configured pdf-tools handles PDFs.
Call optional VALIDATE through navigation before changing the page."
  (pcase-let ((`(,file ,page) (gnosis-lecture--target path)))
    (condition-case err
        (progn
          (gnosis-lecture--file file)
          (when page (gnosis-lecture--check-page file page)))
      (error (signal 'gnosis-lecture-error (list (error-message-string err)))))
    (when validate (funcall validate))
    (let ((enable-local-variables nil) (enable-local-eval nil))
      (find-file file))
    (when validate (funcall validate))
    (unless (and buffer-file-name (file-equal-p file buffer-file-name))
      (signal 'gnosis-lecture-error '("Source buffer changed during navigation")))
    (when page
      (cond ((derived-mode-p 'pdf-view-mode) (pdf-view-goto-page page))
            ((derived-mode-p 'doc-view-mode) (doc-view-goto-page page))
            (t (signal 'gnosis-lecture-error '("PDF source requires DocView or pdf-tools")))))
    (when validate (funcall validate))))

(org-link-set-parameters "gnosis-lecture" :follow #'gnosis-lecture-open)

(defun gnosis-lecture-sources (text)
  "Return source completion candidates from authored TEXT."
  (with-temp-buffer
    (insert (or text ""))
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (equal (org-element-property :type link) "gnosis-lecture")
          (let* ((path (org-element-property :path link))
                 (target (gnosis-lecture--target path)))
            (cons (format "Lecture: %s%s" (car target)
                          (if (cadr target) (format " (page %d)" (cadr target)) ""))
                  (cons 'lecture path))))))))

(defun gnosis-lecture--check (job)
  "Require JOB to retain its original draft and database."
  (let ((buffer (plist-get job :buffer)))
    (unless (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and (eq gnosis-lecture--job job)
                        (derived-mode-p 'gnosis-edit-mode)
                        (not buffer-file-name)
                        (= (buffer-chars-modified-tick) (plist-get job :tick)))))
      (user-error "Lecture draft changed; attach again"))
    (gnosis-assets-root (plist-get job :database))
    (with-current-buffer buffer (gnosis--draft-check-owner))))

(defun gnosis-lecture--cleanup (job)
  "Retire JOB before cancelling its process and deleting private files."
  (let ((buffer (plist-get job :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq gnosis-lecture--job job)
          (setq gnosis-lecture--job nil)
          (remove-hook 'kill-buffer-hook #'gnosis-lecture-cancel t)
          (remove-hook 'change-major-mode-hook #'gnosis-lecture-cancel t)
          (remove-hook 'after-set-visited-file-name-hook #'gnosis-lecture-cancel t)))))
  (when-let* ((timer (plist-get job :timer))) (cancel-timer timer))
  (when-let* ((process (plist-get job :process)))
    (set-process-sentinel process #'ignore)
    (when (process-live-p process) (delete-process process)))
  (when-let* ((output (plist-get job :output)))
    (when (buffer-live-p output) (kill-buffer output)))
  (when-let* ((directory (plist-get job :directory)))
    (when (file-directory-p directory) (delete-directory directory t))))

;;;###autoload
(defun gnosis-lecture-cancel ()
  "Cancel this draft's pending lecture attachment without changing its text."
  (interactive nil gnosis-edit-mode)
  (when gnosis-lecture--job
    (gnosis-lecture--cleanup gnosis-lecture--job)
    (message "Lecture attachment cancelled")))

(defun gnosis-lecture--publish (job image)
  "Publish IMAGE and append it to JOB's original Parathema."
  (gnosis-lecture--check job)
  (unless (equal (plist-get job :digest)
                 (gnosis-lecture--digest (plist-get job :file)))
    (user-error "Lecture source changed; attach again"))
  (let* ((citation (gnosis-lecture--citation (plist-get job :file) (plist-get job :page)))
         (reference (gnosis-image-import image nil citation)))
    (gnosis-lecture--check job)
    (with-current-buffer (plist-get job :buffer)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (plist-get job :position))
          (atomic-change-group
            (insert (format "\n[[gnosis-image:%s]]\n%s\n" reference citation))))))
    reference))

(defun gnosis-lecture--finished (job process)
  "Settle JOB after PROCESS exits, without selecting any window."
  (when (memq (process-status process) '(exit signal))
    (unwind-protect
        (condition-case err
            (progn
              (gnosis-lecture--check job)
              (unless (= (process-exit-status process) 0)
                (user-error "PDF page unavailable; check the physical page and PDF (pdftoppm failed)"))
              (gnosis-lecture--publish job
                                       (expand-file-name "page.png" (plist-get job :directory)))
              (message "Lecture page attached to Parathema"))
          ((error quit) (message "Lecture media: %s" (error-message-string err))))
      (gnosis-lecture--cleanup job))))

;;;###autoload
(defun gnosis-lecture-attach (&optional file page)
  "Attach local image FILE or physical PDF PAGE to this draft's Parathema.
Prompt for FILE and a 1-based physical PAGE when interactive.  PNG and JPEG
are copied to managed storage.  PDFs require Poppler's pdftoppm; rendering
runs asynchronously, with a 60-second deadline.  Editing the draft while it
runs refuses late insertion.  Cancel with `gnosis-lecture-cancel'.
Only the illustration is bundled by content export, never the original PDF."
  (interactive nil gnosis-edit-mode)
  (unless (derived-mode-p 'gnosis-edit-mode) (user-error "Open a Gnosis draft first"))
  (gnosis--draft-check-owner)
  (when gnosis-lecture--job (user-error "Lecture attachment pending; cancel it first"))
  (let* ((buffer (current-buffer))
         (database (gnosis--ensure-db))
         (tick (buffer-chars-modified-tick))
         (position
          (save-restriction
            (widen)
            (let* ((document (org-element-parse-buffer))
                   (sections (org-element-map document 'headline
                               (lambda (h)
                                 (when (and (= (org-element-property :level h) 2)
                                            (equal (org-element-property :raw-value h) "Parathema"))
                                   h)))))
              (unless (and (= (length (gnosis-export-parse-themata)) 1)
                           (= (length sections) 1))
                (user-error "Attach in a single thema draft with a Parathema section"))
              (org-element-property :end (car sections)))))
         (job (list :buffer buffer :database database :tick tick :position position
                    :process nil :timer nil :output nil :directory nil
                    :file nil :page nil :digest nil)))
    (setq gnosis-lecture--job job)
    (add-hook 'kill-buffer-hook #'gnosis-lecture-cancel nil t)
    (add-hook 'change-major-mode-hook #'gnosis-lecture-cancel nil t)
    (add-hook 'after-set-visited-file-name-hook #'gnosis-lecture-cancel nil t)
    (condition-case err
        (let* ((file (gnosis-lecture--file (or file (read-file-name "Lecture image or PDF: " nil nil t))))
               (pdf (equal (downcase (or (file-name-extension file) "")) "pdf"))
               (page (and pdf (or page (read-number "Physical PDF page (1-based): " 1)))))
          (gnosis-lecture--check job)
          (when (and pdf (not (and (integerp page) (> page 0))))
            (user-error "Physical PDF page must be a positive integer"))
          (when (and pdf (not (executable-find "pdftoppm")))
            (user-error "Install Poppler (pdftoppm) to attach PDF pages"))
          (setf (plist-get job :file) file
                (plist-get job :page) page
                (plist-get job :digest) (gnosis-lecture--digest file)
                (plist-get job :directory) (make-temp-file "gnosis-lecture-" t))
          (if pdf
              (let* ((directory (plist-get job :directory))
                     (output (generate-new-buffer " *Gnosis PDF extraction*")))
                ;; Render a private snapshot; verify the source again at publication.
                (copy-file file (expand-file-name "source.pdf" directory))
                (unless (equal (plist-get job :digest)
                               (gnosis-lecture--digest (expand-file-name "source.pdf" directory)))
                  (user-error "Lecture source changed while copying; attach again"))
                (setf (plist-get job :output) output
                      (plist-get job :process)
                      (make-process
                       :name "gnosis-lecture" :buffer output :noquery t
                       :command (list (executable-find "pdftoppm") "-f" (number-to-string page)
                                      "-l" (number-to-string page) "-singlefile" "-scale-to" "1800"
                                      "-png" (expand-file-name "source.pdf" directory)
                                      (expand-file-name "page" directory))
                       :sentinel (lambda (process _event) (gnosis-lecture--finished job process)))
                      (plist-get job :timer)
                      (run-at-time 60 nil
                                   (lambda ()
                                     (gnosis-lecture--cleanup job)
                                     (message "Lecture media: PDF extraction timed out; try another page"))))
                (message "Rendering lecture page; continue editing to discard, or cancel attachment"))
            (let ((image (expand-file-name "image" (plist-get job :directory))))
              (copy-file file image)
              (unless (equal (plist-get job :digest) (gnosis-lecture--digest image))
                (user-error "Lecture image changed while copying; attach again"))
              (gnosis-lecture--publish job image)
              (gnosis-lecture--cleanup job)
              (message "Lecture image attached to Parathema")))
          job)
      ((error quit)
       (gnosis-lecture--cleanup job)
       (signal (car err) (cdr err))))))

(provide 'gnosis-lecture)
;;; gnosis-lecture.el ends here
