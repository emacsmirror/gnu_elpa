;;; gnosis-test-image-targets.el --- Grouped image targets -*- lexical-binding: t; -*-
;;; Commentary:
;; Domain tests; graphical acceptance is a separate gate.
;;; Code:
(require 'ert)
(require 'gnosis-image)
(require 'dom)
(require 'gnosis-test-image)

(defun gnosis-test-image-targets--text (node)
  "Return direct text children of SVG text NODE."
  (mapconcat #'identity (seq-filter #'stringp (dom-children node)) ""))

(defconst gnosis-test-image-targets--regions
  '(((id . "artery") (label . "Artery")
     (rects . ((0.1 0.1 0.2 0.1) (0.6 0.6 0.2 0.1))))
    ((id . "vein") (label . "Vein") (rects . ((0.1 0.3 0.2 0.1))))))

(ert-deftest gnosis-image-targets-plural-validation-and-hit ()
  (let ((before (copy-tree gnosis-test-image-targets--regions)))
    (should (equal before (gnosis-image--regions before)))
    (should (equal "artery" (gnosis-image--hit before '(0.7 . 0.65))))
    (should (equal before gnosis-test-image-targets--regions))))

(ert-deftest gnosis-image-targets-policy ()
  (should (equal "hide-target" (gnosis-image-occlusion-policy '("resource"))))
  (should (equal "hide-all" (gnosis-image-occlusion-policy '("resource" "id" "hide-all"))))
  (dolist (fields '(nil ("r" "id" nil) ("r" "id" "") ("r" "id" "hide-all" "extra")))
    (should-error (gnosis-image-occlusion-policy fields))))

(defmacro gnosis-test-image-targets--scene (&rest body)
  "Evaluate BODY with a disposable real raster SCENE."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "gnosis-image-targets-" t))
          (file (expand-file-name "atlas.png" directory))
          (coding-system-for-write 'no-conversion))
     (unwind-protect
         (progn
           (with-temp-file file
             (set-buffer-multibyte nil)
             (insert (base64-decode-string
                      "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGCAIAAABxZ0isAAAAEUlEQVR4nGP4UKGBFTEMpAQAIGBLAbAg65EAAAAASUVORK5CYII=")))
           (let ((scene `((path . ,file) (width . 8) (height . 6)
                          (regions . ,(copy-tree gnosis-test-image-targets--regions)))))
             (ignore scene)
             ,@body))
       (delete-directory directory t))))

(ert-deftest gnosis-image-targets-manifest-versions-and-bounds ()
  (gnosis-test-image-targets--scene
    (let* ((legacy '(((id . "old") (label . "Old") (rect . (0 0 1 1)))))
           (base `((file . "atlas.png") (width . 8) (height . 6) (source . "") (attribution . ""))))
      (dolist (version '(1 2))
        (let ((regions (if (= version 1) legacy gnosis-test-image-targets--regions)))
          (should (gnosis-image--manifest (append `((version . ,version) (regions . ,regions)) base) directory))
          (should-error (gnosis-image--manifest
                         (append `((version . ,(- 3 version)) (regions . ,regions)) base) directory))))
      (should (equal legacy (gnosis-image--regions legacy)))
      (should (gnosis-image--manifest (append '((version . 2) (regions)) base) directory))
      (dolist (r '(((id . "x") (label . "X") (rects))
                   ((id . "x") (label . "X") (rect . (0 0 1 1)) (rects . ((0 0 1 1))))
                   ((id . "x") (label . "X") (rects . ((0 0 1 1))) (rects . ((0 0 1 1))))))
        (should-error (gnosis-image--regions (list r))))
      (dolist (rect '((0 0 0 1) (0 0 1.0e+INF 1) (0 0 0.0e+NaN 1) (0.8 0 0.3 1) (0 0 1)))
        (should-error (gnosis-image--regions `(((id . "x") (label . "X") (rects . (,rect)))))))
      (should-error (gnosis-image--regions
                     `(((id . "x") (label . "X") (rects . ,(make-list 256 '(0 0 1 1))))))))))

(ert-deftest gnosis-image-targets-mask-layers-and-clean-reveal ()
  (gnosis-test-image-targets--scene
    (dolist (policy '("hide-target" "hide-all"))
      (let* ((before (copy-tree scene))
             (svg (gnosis-image--svg scene (alist-get 'regions scene) 800 600
                                     'occlusion "artery" "vein" nil policy))
             (rects (dom-by-tag svg 'rect))
             (masks (seq-remove (lambda (r) (equal (dom-attr r 'fill) "none")) rects)))
        (should (= (length masks) (if (equal policy "hide-all") 3 2)))
        (should (seq-every-p (lambda (r) (= (dom-attr r 'fill-opacity) 1)) masks))
        (should (equal '("?" "?") (mapcar #'gnosis-test-image-targets--text (dom-by-tag svg 'text))))
        (should (equal "?" (gnosis-test-image-targets--text (car (last (dom-children svg))))))
        (let ((reveal (gnosis-image--svg scene (alist-get 'regions scene) 800 600
                                         'occlusion "artery" "vein" t policy)))
          (dolist (tag '(rect text path line polygon)) (should-not (dom-by-tag reveal tag)))
          (should (equal (dom-by-tag svg 'image) (dom-by-tag reveal 'image))))
        (should (equal before scene))))))

(ert-deftest gnosis-image-targets-refresh-old-new-properties ()
  (gnosis-test-image-targets--scene
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (cl-letf (((symbol-function 'gnosis-image--decode) #'ignore)
                  ((symbol-function 'image-type-available-p) (lambda (_) t))
                  ((symbol-function 'svg-image) (lambda (svg &rest _) (list 'image :data svg))))
          (insert (propertize " " 'gnosis-image-mask (list scene "artery" nil)))
          (insert (gnosis-image-mask scene "artery" nil nil "hide-all"))
          (insert (gnosis-image-mask scene "artery" t nil "hide-all"))
          (goto-char (point-max)) (set-buffer-modified-p nil)
          (let ((point (point)) (text (buffer-substring-no-properties (point-min) (point-max))))
            (gnosis-image-refresh)
            (cl-loop for pos from 1 to 3 for count in '(4 5 0) do
                     (let ((svg (plist-get (cdr (get-text-property pos 'display)) :data)))
                       (should (= count (length (dom-by-tag svg 'rect))))))
            (should (= point (point))) (should-not (buffer-modified-p))
            (should (equal text (buffer-substring-no-properties (point-min) (point-max))))))))))

(defmacro gnosis-test-image-targets--editor (&rest body)
  "Run BODY in an owned editor, replacing only raster display."
  (declare (indent 0) (debug t))
  `(gnosis-test-image-targets--scene
     (save-window-excursion
       (with-temp-buffer
         (switch-to-buffer (current-buffer))
         (gnosis-image-mode)
         (setq gnosis-image--scene scene
               gnosis-image--regions (copy-tree gnosis-test-image-targets--regions)
               gnosis-image--purpose 'edit gnosis-image--selection "artery")
         (cl-letf (((symbol-function 'svg-image)
                    (lambda (&rest _) '(image :type svg :data "<svg/>"))))
           ,@body)))))

(ert-deftest gnosis-image-targets-native-edit-and-stable-identity ()
  (gnosis-test-image-targets--editor
    (execute-kbd-macro "n")
    (should (= 1 gnosis-image--rectangle))
    (should (equal "artery" gnosis-image--selection))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "2: Vein")))
      (execute-kbd-macro "r"))
    (should (equal "vein" gnosis-image--selection))
    (should (= 1 gnosis-image--rectangle))
    (should (= 1 (length (gnosis-image--region-rectangles (car gnosis-image--regions)))))
    (should (= 2 (length (gnosis-image--region-rectangles (cadr gnosis-image--regions)))))
    (execute-kbd-macro "d")
    (should (= 1 (length (gnosis-image--region-rectangles (cadr gnosis-image--regions)))))
    (let ((before (copy-tree gnosis-image--regions)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))) (execute-kbd-macro "D"))
      (should (equal before gnosis-image--regions)))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Renamed"))) (execute-kbd-macro "l"))
    (should (equal "vein" (alist-get 'id (cadr gnosis-image--regions))))
    (should (equal "Renamed" (alist-get 'label (cadr gnosis-image--regions))))))

(ert-deftest gnosis-image-targets-drag-add-no-id-recycling ()
  (gnosis-test-image-targets--editor
    (cl-letf (((symbol-function 'gnosis-image--position) #'identity)
              ((symbol-function 'event-start) (lambda (_) '(0.4 . 0.4)))
              ((symbol-function 'event-end) (lambda (_) '(0.5 . 0.5)))
              ((symbol-function 'read-string) (lambda (&rest _) "New")))
      (funcall (keymap-lookup gnosis-image-mode-map "S-<drag-mouse-1>") 'drag)
      (should (equal "artery" gnosis-image--selection))
      (should (= 3 (length (gnosis-image--region-rectangles (car gnosis-image--regions)))))
      (funcall (keymap-lookup gnosis-image-mode-map "<drag-mouse-1>") 'drag)
      (should (equal "region-1" gnosis-image--selection))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))) (execute-kbd-macro "D"))
      (funcall (keymap-lookup gnosis-image-mode-map "<drag-mouse-1>") 'drag)
      (should (equal "region-2" gnosis-image--selection)))))

(ert-deftest gnosis-image-targets-prompt-cancel-and-owner-drift ()
  (dolist (command '(gnosis-image-rename-target gnosis-image-reassign-rectangle gnosis-image-delete-target))
    (dolist (fault '(quit mode selection mutation))
      (gnosis-test-image-targets--editor
        (let* ((before (copy-tree gnosis-image--regions))
               (prompt (lambda (&rest _)
                         (pcase fault
                           ('quit (signal 'quit nil))
                           ('mode (fundamental-mode))
                           ('selection (setq gnosis-image--selection "vein"))
                           ('mutation (setf (alist-get 'label (car gnosis-image--regions)) "External")))
                         (if (eq command 'gnosis-image-reassign-rectangle) "2: Vein" "New"))))
          (cl-letf (((symbol-function 'read-string) prompt)
                    ((symbol-function 'completing-read) prompt)
                    ((symbol-function 'y-or-n-p) prompt))
            (should (condition-case nil (progn (call-interactively command) nil) ((error quit) t))))
          (unless (memq fault '(mode mutation)) (should (equal before gnosis-image--regions))))))))

(ert-deftest gnosis-image-targets-save-alias-supplied-semantics ()
  (let (calls)
    (cl-letf (((symbol-function 'gnosis-add-thema--assert-common) #'ignore)
              ((symbol-function 'gnosis-add-thema--dispatch) (lambda (&rest args) (push args calls))))
      ;; A non-media type lets the real field preflight run without filesystem seams.
      (gnosis-image--save "id" "basic" "q" nil '("a") "p" nil 0 nil)
      (gnosis-image--save "id" "basic" "q" nil '("a") "p" nil 0 nil nil)
      (gnosis-image--save "id" "basic" "q" nil '("a") "p" nil 0 nil '("alias")))
    (should (equal '(10 10 9) (mapcar #'length calls)))
    (should (equal '("alias") (nth 9 (car calls))))
    (should-not (nth 9 (cadr calls)))))

(ert-deftest gnosis-image-targets-import-immutable-roundtrip ()
  (gnosis-test-image-targets--scene
    (let* ((gnosis-db (gnosis-db--open directory))
           (gnosis-dir directory)
           (legacy '(((id . "old") (label . "Old") (rect . (0 0 1 1))))))
      (unwind-protect
          (let* ((old (gnosis-image-import file legacy))
                 (before (gnosis-image-resolve old))
                 (mixed (append legacy gnosis-test-image-targets--regions))
                 (copy (copy-tree mixed))
                 (new (gnosis-image-import file mixed))
                 (scene (gnosis-image-resolve new "artery")))
            (should (equal old (gnosis-image-import file legacy)))
            (should (equal before (gnosis-image-resolve old)))
            (should (= 1 (alist-get 'version before)))
            (should (equal legacy (alist-get 'regions before)))
            (should (= 2 (alist-get 'version scene)))
            (should (seq-every-p (lambda (r) (and (assq 'rects r) (not (assq 'rect r))))
                                 (alist-get 'regions scene)))
            (should (equal new (gnosis-image-import file mixed)))
            (should (equal copy mixed))
            (should-error (gnosis-image-resolve old "artery"))
            (should (equal (gnosis-image-occlusion-fields (list new "artery" "hide-all") '("Edited"))
                           (list (list new "artery" "hide-all") '("Edited")))))
        (gnosis-sqlite-close gnosis-db)))))

(ert-deftest gnosis-image-targets-attachment-frozen-codec-boundary ()
  ;; The new codec argument is parent-owned.  Check the exact call payload
  ;; without pretending the old codec implements aliases.
  (gnosis-test-image-targets--scene
    (let* ((gnosis-db (gnosis-db--open directory)) (gnosis-dir directory)
           (reference (gnosis-image-import file gnosis-test-image-targets--regions)))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (setq major-mode 'gnosis-edit-mode)
            (insert "Authored draft")
            (let* ((row (list "id" "image-occlusion" "Question"
                              (list reference "artery" "hide-all") '("Authored answer")
                              "Explanation" '("tag") 1 '("Arteria" "A.")))
                   payload)
              (cl-letf (((symbol-function 'gnosis-export-parse-themata) (lambda () (list row)))
                        ((symbol-function 'gnosis-image--read-resource) (lambda (&rest _) reference))
                        ((symbol-function 'completing-read)
                         (lambda (prompt _collection &rest args)
                           (if (string-prefix-p "Expected" prompt) "2: Vein"
                             (should (equal "hide-all" (nth 4 args))) "hide-all")))
                        ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                        ((symbol-function 'gnosis-export--insert-thema)
                         (lambda (&rest args) (setq payload args) (insert "Reinserted"))))
                (gnosis-image-attach))
              (should (equal '("Arteria" "A.") (nth 8 payload)))
              (should (equal "Authored answer" (nth 4 payload)))
              (should (equal (concat reference "\n- vein\n- hide-all") (nth 3 payload)))
              (should (equal "Reinserted" (buffer-string)))))
        (gnosis-sqlite-close gnosis-db)))))

(ert-deftest gnosis-image-targets-reassign-new-last-and-no-op ()
  (gnosis-test-image-targets--editor
    (setq gnosis-image--selection "vein")
    (let ((before (copy-tree gnosis-image--regions)))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "2: Vein")))
        (execute-kbd-macro "r"))
      (should (equal before gnosis-image--regions))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "New target"))
                ((symbol-function 'read-string) (lambda (&rest _) "New"))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (execute-kbd-macro "r"))
      (should (equal before gnosis-image--regions))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "New target"))
                ((symbol-function 'read-string) (lambda (&rest _) "New"))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (execute-kbd-macro "r"))
      (should (equal '("artery" "region-1") (mapcar (lambda (r) (alist-get 'id r)) gnosis-image--regions)))
      (should (equal (car before) (car gnosis-image--regions)))
      (should (equal (gnosis-image--region-rectangles (cadr before))
                     (gnosis-image--region-rectangles (cadr gnosis-image--regions)))))))

(ert-deftest gnosis-image-targets-overlap-and-region-neutrality ()
  (gnosis-test-image-targets--scene
    (let* ((regions (append gnosis-test-image-targets--regions
                            '(((id . "overlap") (label . "Secret") (rect . (0 0 1 1))))))
           (svg (gnosis-image--svg scene regions 800 600 'region "artery" "vein" nil)))
      (should (equal '("artery" . 1) (gnosis-image--hit-rectangle regions '(0.7 . 0.65))))
      (should (equal "overlap" (gnosis-image--hit regions '(0.95 . 0.95))))
      (should-not (dom-by-tag svg 'text))
      (should (= 1 (length (dom-by-tag svg 'rect)))))))

(ert-deftest gnosis-image-targets-serialized-budget-before-publish ()
  (gnosis-test-image-targets--scene
    (let ((regions (cl-loop for n below 255
                            collect `((id . ,(format "r%d" n))
                                      (label . ,(make-string 200 #x1f600))
                                      (rects . ((0 0 1 1)))))))
      (cl-letf (((symbol-function 'gnosis-assets-import)
                 (lambda (&rest _) (ert-fail "Published oversized manifest"))))
        (should-error (gnosis-image-import file regions) :type 'user-error)))))

;; Exercise the established public journeys with the new explicit policy,
;; neutral cue and last-rectangle confirmation contracts.
(ert-deftest gnosis-image-targets-occlusion-feedback-restores-source-through-resize ()
  ;; Exercise real review, mask, SVG construction and refresh; only native
  ;; decoding/image creation and minibuffer input are replaced in batch.
  (dolist (correct '(nil t))
    (gnosis-test-with-db
     (save-window-excursion
       (let* ((id (gnosis-test-image--add "image-occlusion"))
              (reference (car (gnosis-get 'hypothesis 'themata `(= id ,id))))
              (scene (gnosis-image-resolve reference))
              (snapshot (copy-tree scene))
              (buffer (gnosis-review--setup-buffer (list id)))
              (width 40) rendered source)
         (unwind-protect
             (progn
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
                            (should (equal '("?") (mapcar #'gnosis-test-image-targets--text (dom-by-tag rendered 'text))))
                            (should (= (length (dom-by-tag rendered 'rect)) 2))
                            (let ((mask (car (dom-by-tag rendered 'rect))))
                              (should (equal (dom-attr mask 'fill) "#202020"))
                              (should (= (dom-attr mask 'fill-opacity) 1))
                              (should (= (dom-attr mask 'width) 3.2)))
                            (if correct "Left region" "Other answer"))))
                 (should (eq correct (car (gnosis-review-image-occlusion id))))
                 (should (equal source (dom-by-tag rendered 'image)))
                 (should-not (dom-by-tag rendered 'text))
                 (should-not (dom-by-tag rendered 'rect))
                 (should (string-match-p "Answer: Left region" (buffer-string)))
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

(ert-deftest gnosis-image-targets-occlusion-native-author-save-reopen ()
  (gnosis-test-with-db
   (save-window-excursion
     (let ((gnosis-save-hook nil) (file (gnosis-test-image--file)))
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) file))
                       ((symbol-function 'read-string) (lambda (&rest _) ""))
                       ((symbol-function 'gnosis-image-edit-regions)
                        (lambda (_) (copy-tree gnosis-test-image--regions)))
                       ((symbol-function 'completing-read) (lambda (_ choices &rest _) (caar choices))))
               (gnosis-add-image-thema "image-occlusion"))
             (insert "Name the masked structure")
             (let* ((row (car (gnosis-export-parse-themata)))
                    (reference (car (nth 3 row))))
               (should (equal (nth 3 row) (list reference "left" "hide-target")))
               (should (equal (nth 4 row) '("Left region")))
               (goto-char (point-min))
               (search-forward "Left region")
               (replace-match "Editable anatomy" t t)
               (call-interactively (key-binding (kbd "C-c C-c")))
               (let* ((id (car (gnosis-select 'id 'themata nil t)))
                      (before (gnosis-select '* 'themata)))
                 (should (equal '("Editable anatomy") (gnosis-get 'answer 'themata `(= id ,id))))
                 (gnosis-sqlite-close gnosis-db)
                 (setq gnosis-db (gnosis-db--open gnosis-dir))
                 (gnosis-edit-thema id)
                 (should (equal (nth 3 (car (gnosis-export-parse-themata))) (list reference "left" "hide-target")))
                 (call-interactively (key-binding (kbd "C-c C-c")))
                 (should (equal before (gnosis-select '* 'themata))))))
         (dolist (name '("*Gnosis NEW*" "*Gnosis Edit*"))
           (when (get-buffer name) (kill-buffer name))))))))

(ert-deftest gnosis-image-targets-occlusion-legacy-edit-save-and-org-export ()
  (gnosis-test-with-db
   (save-window-excursion
     (let* ((id (gnosis-test-image--add "image-occlusion"))
            (reference (car (gnosis-get 'hypothesis 'themata `(= id ,id))))
            (scene (gnosis-image-resolve reference "left"))
            (gnosis-save-hook nil))
       (unwind-protect
           (progn
             ;; Org export must decode to canonical fields without writing the row.
             (with-temp-buffer
               (org-mode)
               (gnosis-export--insert-themata (list id))
               (let ((row (car (gnosis-export-parse-themata))))
                 (should (equal (nth 3 row) (list reference "left" "hide-target")))
                 (should (equal (nth 4 row) '("Left region")))
                 (should (equal (nth 6 row) '("image_test")))))
             (should (equal (gnosis-get 'hypothesis 'themata `(= id ,id)) (list reference)))
             (gnosis-edit-thema id)
             (let ((row (car (gnosis-export-parse-themata))))
               (should (equal (nth 3 row) (list reference "left" "hide-target")))
               (should (equal (nth 4 row) '("Left region")))
               (should (equal (nth 6 row) '("image_test"))))
             (goto-char (point-min))
             (search-forward "Left region")
             (replace-match "Edited human answer" t t)
             (call-interactively (key-binding (kbd "C-c C-c")))
             (gnosis-sqlite-close gnosis-db)
             (setq gnosis-db (gnosis-db--open gnosis-dir))
             (should (equal (gnosis-get 'hypothesis 'themata `(= id ,id)) (list reference "left" "hide-target")))
             (should (equal (gnosis-get 'answer 'themata `(= id ,id)) '("Edited human answer")))
             (should (equal (gnosis-get-tags-for-ids (list id)) '("image_test")))
             (gnosis-edit-thema id)
             (let ((row (car (gnosis-export-parse-themata))))
               (should (equal (nth 3 row) (list reference "left" "hide-target")))
               (should (equal (nth 4 row) '("Edited human answer")))
               (should (equal (nth 6 row) '("image_test")))))
         (when (get-buffer "*Gnosis Edit*") (kill-buffer "*Gnosis Edit*")))
       ;; Resolving checks the immutable byte revision as well as manifest data.
       (should (equal scene (gnosis-image-resolve reference "left")))))))

(ert-deftest gnosis-image-targets-native-keymap-edit-delete-and-cancel ()
  (gnosis-test-with-db
   (let* ((file (gnosis-test-image--file))
          (reference (gnosis-image-import file gnosis-test-image--regions))
          (scene (gnosis-image-resolve reference)))
     (save-window-excursion
       (with-temp-buffer
         (switch-to-buffer (current-buffer))
         (gnosis-image-mode)
         (setq gnosis-image--regions (copy-tree gnosis-test-image--regions)
               gnosis-image--purpose 'edit gnosis-image--selection "left"
               gnosis-image--depth 0)
         (let ((cancelled 0) (accepted 0))
           (cl-letf (((symbol-function 'gnosis-image--render) #'ignore)
                     ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                     ((symbol-function 'recursion-depth) (lambda () 1))
                     ((symbol-function 'abort-recursive-edit) (lambda () (cl-incf cancelled)))
                     ((symbol-function 'exit-recursive-edit) (lambda () (cl-incf accepted))))
             (execute-kbd-macro "d")
             (should (equal '("right") (mapcar (lambda (r) (alist-get 'id r)) gnosis-image--regions)))
             (execute-kbd-macro "q")
             (should (= cancelled 1))
             (should-not gnosis-image--accepted)
             (execute-kbd-macro (kbd "RET"))
             (should (= accepted 1))
             (should gnosis-image--accepted)
             (setq gnosis-image--depth nil)))))
     (should (equal gnosis-test-image--regions (alist-get 'regions (gnosis-image-resolve reference))))
     (let ((revised (gnosis-image-import file (cdr gnosis-test-image--regions))))
       (should-not (equal reference revised))
       (should (equal gnosis-test-image--regions (alist-get 'regions scene)))
       (should (gnosis-image-resolve reference "left"))
       (should-error (gnosis-image-resolve revised "left"))))))

(ert-deftest gnosis-image-targets-reattach-real-codec-aliases-policy ()
  (gnosis-test-with-db
   (let* ((reference (gnosis-image-import (gnosis-test-image--file)
                                         gnosis-test-image-targets--regions)))
     (with-temp-buffer
       (org-mode)
       (gnosis-export--insert-thema "NEW" "image-occlusion" "Question"
                                    (concat reference "\n- artery\n- hide-all")
                                    "Authored answer" "Explanation" '("tag") nil '("Arteria"))
       (setq major-mode 'gnosis-edit-mode)
       (goto-char (point-min))
       (cl-letf (((symbol-function 'gnosis-image--read-resource) (lambda (&rest _) reference))
                 ((symbol-function 'completing-read)
                  (lambda (prompt &rest _)
                    (if (string-prefix-p "Expected" prompt) "2: Vein" "hide-all")))
                 ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
         (gnosis-image-attach))
       (let ((row (car (gnosis-export-parse-themata))))
         (should (equal (nth 3 row) (list reference "vein" "hide-all")))
         (should (equal (nth 4 row) '("Authored answer")))
         (should (equal (nth 8 row) '("Arteria"))))))))

(provide 'gnosis-test-image-targets)
;;; gnosis-test-image-targets.el ends here
