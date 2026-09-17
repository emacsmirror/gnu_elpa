;;; gnosis-image-test-support.el --- Shared image fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Shared fixtures and assertions; loading this library registers no tests.

;;; Code:
(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-review)
(require 'gnosis-export-import)

(defun gnosis-test-image--file (&optional destination)
  "Write an original valid 8 by 6 RGB PNG and return its path.
Write to DESTINATION when non-nil, otherwise below `gnosis-dir'."
  (let ((file (or destination (expand-file-name "original.png" gnosis-dir)))
        (coding-system-for-write 'no-conversion))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert (base64-decode-string
               "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGCAIAAABxZ0isAAAAEUlEQVR4nGP4UKGBFTEMpAQAIGBLAbAg65EAAAAASUVORK5CYII=")))
    file))

(defconst gnosis-test-image--regions
  '(((id . "left") (label . "Left region") (rect . (0.0 0.0 0.4 0.8)))
    ((id . "right") (label . "Right region") (rect . (0.5 0.1 0.4 0.8)))))

(defun gnosis-test-image--add (&optional type regions policy)
  "Return an image thema ID using TYPE, REGIONS and occlusion POLICY.
Omitted REGIONS use legacy rectangles; omitted POLICY keeps legacy fields."
  (let* ((regions (or regions gnosis-test-image--regions))
         (reference (gnosis-image-import (gnosis-test-image--file) regions))
         (target (alist-get 'id (car regions)))
         (id (gnosis-generate-id)))
    (gnosis-add-thema-fields
     (or type "image-region") "Select left"
     (if policy (list reference target policy) (list reference))
     (list (if policy (alist-get 'label (car regions)) target))
     "Explanation" '("image_test") 0 nil nil id)
    id))

(defun gnosis-test-image-targets--text (node)
  "Return direct text children of SVG text NODE."
  (mapconcat #'identity (seq-filter #'stringp (dom-children node)) ""))

(defconst gnosis-test-image-targets--regions
  '(((id . "artery") (label . "Artery")
     (rects . ((0.1 0.1 0.2 0.1) (0.6 0.6 0.2 0.1))))
    ((id . "vein") (label . "Vein") (rects . ((0.1 0.3 0.2 0.1))))))

(defun gnosis-test-image--author-roundtrip (regions policy)
  "Author REGIONS with POLICY, edit the answer, save and reopen twice."
  (gnosis-test-with-db
   (save-window-excursion
     (let ((gnosis-save-hook nil) (file (gnosis-test-image--file))
           (target (alist-get 'id (car regions)))
           (label (alist-get 'label (car regions))))
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) file))
                       ((symbol-function 'read-string) (lambda (&rest _) ""))
                       ((symbol-function 'gnosis-image-edit-regions)
                        (lambda (_) (copy-tree regions)))
                       ((symbol-function 'completing-read) (lambda (prompt choices &rest _)
                          (if (equal prompt "Label visibility: ") policy
                            (caar choices)))))
               (gnosis-add-image-thema "image-occlusion"))
             (insert "Name the masked structure")
             (let* ((row (car (gnosis-export-parse-themata)))
                    (reference (car (nth 3 row))))
               (should (equal (nth 3 row) (list reference target policy)))
               (should (equal (nth 4 row) (list label)))
               (should (equal regions
                              (alist-get 'regions (gnosis-image-resolve reference))))
               (goto-char (point-min))
               (search-forward "** Answer")
               (search-forward label)
               (replace-match "Editable anatomy" t t)
               (call-interactively (key-binding (kbd "C-c C-c")))
               (let* ((id (car (gnosis-select 'id 'themata nil t)))
                      (before (gnosis-select '* 'themata)))
                 (should (equal '("Editable anatomy") (gnosis-get 'answer 'themata `(= id ,id))))
                 (gnosis-sqlite-close gnosis-db)
                 (setq gnosis-db (gnosis-db--open gnosis-dir))
                 (gnosis-edit-thema id)
                 (should (equal (nth 3 (car (gnosis-export-parse-themata))) (list reference target policy)))
                 (call-interactively (key-binding (kbd "C-c C-c")))
                 (should (equal before (gnosis-select '* 'themata))))))
         (dolist (name '("*Gnosis NEW*" "*Gnosis Edit*"))
           (when (get-buffer name) (kill-buffer name))))))))

(defun gnosis-test-image--legacy-roundtrip (regions)
  "Export and edit legacy occlusion fields backed by REGIONS."
  (gnosis-test-with-db
   (save-window-excursion
     (let* ((target (alist-get 'id (car regions)))
            (label (alist-get 'label (car regions)))
            (id (gnosis-test-image--add "image-occlusion" regions))
            (reference (car (gnosis-get 'hypothesis 'themata `(= id ,id))))
            (scene (gnosis-image-resolve reference target))
            (gnosis-save-hook nil))
       (unwind-protect
           (progn
             (should (equal regions (alist-get 'regions scene)))
             ;; Org export must decode to canonical fields without writing the row.
             (with-temp-buffer
               (org-mode)
               (gnosis-export--insert-themata (list id))
               (let ((row (car (gnosis-export-parse-themata))))
                 (should (equal (nth 3 row) (list reference target "hide-target")))
                 (should (equal (nth 4 row) (list label)))
                 (should (equal (nth 6 row) '("image_test")))))
             (should (equal (gnosis-get 'hypothesis 'themata `(= id ,id)) (list reference)))
             (gnosis-edit-thema id)
             (let ((row (car (gnosis-export-parse-themata))))
               (should (equal (nth 3 row) (list reference target "hide-target")))
               (should (equal (nth 4 row) (list label)))
               (should (equal (nth 6 row) '("image_test"))))
             (goto-char (point-min))
             (search-forward "** Answer")
             (search-forward label)
             (replace-match "Edited human answer" t t)
             (call-interactively (key-binding (kbd "C-c C-c")))
             (gnosis-sqlite-close gnosis-db)
             (setq gnosis-db (gnosis-db--open gnosis-dir))
             (should (equal (gnosis-get 'hypothesis 'themata `(= id ,id)) (list reference target "hide-target")))
             (should (equal (gnosis-get 'answer 'themata `(= id ,id)) '("Edited human answer")))
             (should (equal (gnosis-get-tags-for-ids (list id)) '("image_test")))
             (gnosis-edit-thema id)
             (let ((row (car (gnosis-export-parse-themata))))
               (should (equal (nth 3 row) (list reference target "hide-target")))
               (should (equal (nth 4 row) '("Edited human answer")))
               (should (equal (nth 6 row) '("image_test")))))
         (when (get-buffer "*Gnosis Edit*") (kill-buffer "*Gnosis Edit*")))
       ;; Resolving checks the immutable byte revision as well as manifest data.
       (should (equal scene (gnosis-image-resolve reference target)))))))

(defun gnosis-test-image--feedback-resize (regions policy mask-widths)
  "Review REGIONS under POLICY and check opaque MASK-WIDTHS, reveal and resize."
  ;; Exercise real review, mask, SVG construction and refresh; only native
  ;; decoding/image creation and minibuffer input are replaced in batch.
  (dolist (correct '(nil t))
    (gnosis-test-with-db
     (save-window-excursion
       (let* ((label (alist-get 'label (car regions)))
              (cues (make-list (length (gnosis-image--region-rectangles (car regions))) "?"))
              (id (gnosis-test-image--add "image-occlusion" regions policy))
              (reference (car (gnosis-get 'hypothesis 'themata `(= id ,id))))
              (scene (gnosis-image-resolve reference))
              (snapshot (copy-tree scene))
              (buffer (gnosis-review--setup-buffer (list id)))
              (width 40) rendered source)
         (unwind-protect
             (progn
               (should (equal regions (alist-get 'regions scene)))
               (should (equal (gnosis-get 'hypothesis 'themata `(= id ,id))
                              (if policy (list reference (alist-get 'id (car regions)) policy)
                                (list reference))))
               (switch-to-buffer buffer)
               (cl-letf (((symbol-function 'gnosis-image--decode) #'ignore)
                         ((symbol-function 'image-type-available-p) (lambda (_) t))
                         ((symbol-function 'window-body-width) (lambda (&rest _) width))
                         ((symbol-function 'window-body-height) (lambda (&rest _) 1000))
                         ((symbol-function 'svg-image)
                          (lambda (svg &rest _)
                            (setq rendered svg)
                            '(image :type svg)))
                         ((symbol-function 'gnosis--read-string-with-input-method)
                          (lambda (&rest _)
                            (setq source (copy-tree (dom-by-tag rendered 'image)))
                            (should (= (length source) 1))
                            (should (equal cues (mapcar #'gnosis-test-image-targets--text
                                                      (dom-by-tag rendered 'text))))
                            (let* ((rects (dom-by-tag rendered 'rect))
                                   (masks (seq-remove
                                           (lambda (rect) (equal (dom-attr rect 'fill) "none"))
                                           rects)))
                              (should (= (length rects) (+ (length mask-widths) (length cues))))
                              (should (equal mask-widths (mapcar (lambda (mask) (dom-attr mask 'width)) masks)))
                              (dolist (mask masks)
                                (should (equal (dom-attr mask 'fill) "#202020"))
                                (should (= (dom-attr mask 'fill-opacity) 1))))
                            (if correct label "Other answer"))))
                 (should (eq correct (car (gnosis-review-image-occlusion id))))
                 (should (equal source (dom-by-tag rendered 'image)))
                 (should-not (dom-by-tag rendered 'text))
                 (should-not (dom-by-tag rendered 'rect))
                 (should (string-match-p (regexp-quote (concat "Answer: " label)) (buffer-string)))
                 (unless correct
                   (should (string-match-p "Your answer: Other answer" (buffer-string))))
                 (goto-char (point-max))
                 (set-buffer-modified-p nil)
                 (let ((text (buffer-substring-no-properties (point-min) (point-max)))
                       (position (point)))
                   (setq width 36)
                   (run-hooks 'window-configuration-change-hook)
                   (should (= (dom-attr rendered 'width) 4))
                   (should (= (length (dom-by-tag rendered 'image)) 1))
                   (should (equal (dom-attr (car source) 'xlink:href)
                                  (dom-attr (car (dom-by-tag rendered 'image)) 'xlink:href)))
                   (should-not (dom-by-tag rendered 'text))
                   (should-not (dom-by-tag rendered 'rect))
                   (should (equal text (buffer-substring-no-properties (point-min) (point-max))))
                   (should (= position (point)))
                   (should-not (buffer-modified-p))))
               (should (equal snapshot (gnosis-image-resolve reference)))
               (should-not (gnosis-select '* 'review-events)))
           (kill-buffer buffer)))))))

(defun gnosis-test-image--delete-cancel (regions)
  "Delete REGIONS through native keys, cancel, accept and verify old revisions."
  (gnosis-test-with-db
   (let* ((target (alist-get 'id (car regions)))
          (file (gnosis-test-image--file))
          (reference (gnosis-image-import file regions))
          (scene (gnosis-image-resolve reference)))
     (save-window-excursion
       (with-temp-buffer
         (switch-to-buffer (current-buffer))
         (gnosis-image-mode)
         (setq gnosis-image--regions (copy-tree regions)
               gnosis-image--purpose 'edit gnosis-image--selection target
               gnosis-image--depth 0)
         (let ((cancelled 0) (accepted 0) (confirmations 0))
           (cl-letf (((symbol-function 'gnosis-image--render) #'ignore)
                     ((symbol-function 'y-or-n-p)
                      (lambda (prompt)
                        (cl-incf confirmations)
                        (should (equal prompt "Remove this target from the new image revision? "))
                        t))
                     ((symbol-function 'recursion-depth) (lambda () 1))
                     ((symbol-function 'abort-recursive-edit) (lambda () (cl-incf cancelled)))
                     ((symbol-function 'exit-recursive-edit) (lambda () (cl-incf accepted))))
             (when (assq 'rects (car regions))
               (execute-kbd-macro "d")
               (should (equal target (alist-get 'id (car gnosis-image--regions))))
               (should (= 1 (length (gnosis-image--region-rectangles (car gnosis-image--regions)))))
               (should (= confirmations 0)))
             (execute-kbd-macro "d")
             (should (= confirmations 1))
             (should (equal (mapcar (lambda (r) (alist-get 'id r)) (cdr regions))
                            (mapcar (lambda (r) (alist-get 'id r)) gnosis-image--regions)))
             (execute-kbd-macro "q")
             (should (= cancelled 1))
             (should-not gnosis-image--accepted)
             (execute-kbd-macro (kbd "RET"))
             (should (= accepted 1))
             (should gnosis-image--accepted)
             (setq gnosis-image--depth nil)))))
     (should (equal regions (alist-get 'regions (gnosis-image-resolve reference))))
     (let ((revised (gnosis-image-import file (cdr regions))))
       (should-not (equal reference revised))
       (should (equal regions (alist-get 'regions scene)))
       (should (gnosis-image-resolve reference target))
       (should-error (gnosis-image-resolve revised target))))))

(provide 'gnosis-image-test-support)
;;; gnosis-image-test-support.el ends here
