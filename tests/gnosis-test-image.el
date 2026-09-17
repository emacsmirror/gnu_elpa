;;; gnosis-test-image.el --- Managed image journeys -*- lexical-binding: t; -*-
;;; Commentary:
;; Original RGB PNG fixture with real compressed pixels and checksums.
;;; Code:
(require 'ert)
(require 'gnosis-image-test-support)
(require 'gnosis-review)
(require 'gnosis-export-import)

(ert-deftest gnosis-image-missing-inline-blocks-insertion ()
  (gnosis-test-with-db
   (let ((link (concat "[[gnosis-image:" (make-string 64 ?a) "/image.json]]")))
     (dolist (fields (list (list link nil '("a") "")
                           (list "q" (list link) '("a") "")
                           (list "q" nil (list link) "")
                           (list "q" nil '("a") link)))
       (should-error (apply #'gnosis-add-thema-fields
                            (append (list "basic") fields (list nil 0 nil)))))
     (should-not (gnosis-select 'id 'themata)))))

(ert-deftest gnosis-image-real-header-and-immutable-import ()
  (gnosis-test-with-db
   (let* ((file (gnosis-test-image--file))
          (reference (gnosis-image-import file gnosis-test-image--regions "Original" "Author"))
          (json-encoding-separator ";") (json-encoding-pretty-print t)
          (print-length 1) (print-level 1))
     (should (equal '(8 6 png) (gnosis-image--dimensions file)))
     (should (equal reference (gnosis-image-import file gnosis-test-image--regions "Original" "Author")))
     (delete-file file)
     (let ((scene (gnosis-image-resolve reference "left")))
       (should (equal "Original" (alist-get 'source scene)))
       (should (equal "Author" (alist-get 'attribution scene)))
       (should (file-exists-p (alist-get 'path scene)))))))

(ert-deftest gnosis-image-header-rejects-truncated-and-huge ()
  (gnosis-test-with-db
   (let ((file (gnosis-test-image--file)))
     (with-temp-file file (insert "not pixels"))
     (should-error (gnosis-image--dimensions file))
     (gnosis-test-image--file)
     (let ((coding-system-for-write 'no-conversion))
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert-file-contents-literally file)
         (goto-char 17) (delete-char 4) (insert (unibyte-string 127 255 255 255))
         (write-region (point-min) (point-max) file nil 'silent)))
     (should-error (gnosis-image--dimensions file)))))

(ert-deftest gnosis-image-headless-decoding-fails-honestly ()
  (skip-unless noninteractive)
  (gnosis-test-with-db
   (let ((scene (gnosis-image-resolve (gnosis-image-import (gnosis-test-image--file)))))
     (should (equal 8 (alist-get 'width scene)))
     (should-error (gnosis-image--decode scene) :type 'user-error))))

(ert-deftest gnosis-image-region-invariants-and-input-preservation ()
  (let ((original (copy-tree gnosis-test-image--regions)))
    (should (equal original (gnosis-image--regions gnosis-test-image--regions)))
    (dolist (rect '((0 0 0 1) (-1 0 1 1) (0.5 0 0.8 1) (0 0 1 1.1) (0 0 1)
                    (0 0 0.0e+NaN 1) (0 0 1.0e+INF 1)))
      (should-error (gnosis-image--regions `(((id . "r") (label . "Region") (rect . ,rect))))))
    (should-error (gnosis-image--regions (append gnosis-test-image--regions gnosis-test-image--regions)))))

(ert-deftest gnosis-image-reference-syntax-and-legacy ()
  (let ((reference (concat (make-string 64 ?a) "/image.json")))
    (should (equal (list reference reference)
                   (gnosis-image-references
                    (format "[[gnosis-image:%s]] text [[gnosis-image:%s][Caption]]" reference reference))))
    (should-not (gnosis-image-references "[[file:/tmp/legacy.png]]"))
    (dolist (text '("gnosis-image:broken" "[[gnosis-image:broken]" "[[gnosis-image:bad\n]]"))
      (should-error (gnosis-image-references text)))))

(ert-deftest gnosis-image-corrupt-resource-and-target-refuse-update ()
  (gnosis-test-with-db
   (let* ((id (gnosis-test-image--add))
          (reference (car (gnosis-get 'hypothesis 'themata `(= id ,id))))
          (scene (gnosis-image-resolve reference))
          (before (gnosis-select '* 'themata)))
     (should-error (gnosis-image-resolve reference "unknown"))
     (should-error (gnosis-image-resolve "../image.json"))
     (with-temp-file (alist-get 'path scene) (insert "broken"))
     (should-error (gnosis-update-thema id "new" (list reference) '("left") "" nil nil))
     (should (equal before (gnosis-select '* 'themata))))))

(ert-deftest gnosis-image-import-cancel-and-retry ()
  (gnosis-test-with-db
   (let ((file (gnosis-test-image--file)))
     (dolist (condition '(error quit))
       (cl-letf (((symbol-function 'copy-file) (lambda (&rest _) (signal condition '("Interrupted")))))
         (should (eq condition (condition-case err (gnosis-image-import file)
                                 ((error quit) (car err)))))))
     (should-not (directory-files (gnosis-assets-root) nil "^\\.import-"))
     (should (gnosis-image-resolve (gnosis-image-import file))))))

(ert-deftest gnosis-image-svg-neutral-and-opaque-reveal ()
  (gnosis-test-with-db
   (let* ((scene (gnosis-image-resolve (gnosis-image-import (gnosis-test-image--file)
                                                            gnosis-test-image--regions)))
          (neutral (gnosis-image--svg scene gnosis-test-image--regions 500 375 'region "left" "right" nil))
          (hidden (gnosis-image--svg scene gnosis-test-image--regions 750 562 'occlusion "left" nil nil))
          (revealed (gnosis-image--svg scene gnosis-test-image--regions 750 562 'occlusion "left" "right" t)))
     (should-not (dom-by-tag neutral 'text))
     (should (equal (mapcar #'dom-text (dom-by-tag hidden 'text)) '("?")))
     (let ((mask (car (dom-by-tag hidden 'rect))))
       (should (equal (dom-attr mask 'fill) "#202020"))
       (should (= (dom-attr mask 'fill-opacity) 1))
       (should (= (dom-attr mask 'width) 300.0)))
     (should-not (dom-by-tag revealed 'text))
     (should-not (dom-by-tag revealed 'rect))
     (should (= (length (dom-by-tag neutral 'rect)) 1))
     (should (equal (dom-attr (car (dom-by-tag neutral 'rect)) 'stroke)
                    (gnosis-image--color 'gnosis-image-selected 'foreground)))
     (let ((editor (gnosis-image--svg scene gnosis-test-image--regions 500 375
                                    'edit nil "right" nil)))
       (should (= (length (dom-by-tag editor 'rect)) 2))
       (should (equal (mapcar #'dom-text (dom-by-tag editor 'text))
                      '("Left region" "Right region"))))
     (should (equal (gnosis-image--hit gnosis-test-image--regions '(0.2 . 0.2)) "left"))
     (should-not (gnosis-image--hit gnosis-test-image--regions '(0.45 . 0.9))))))

(ert-deftest gnosis-image-selection-does-not-submit-and-occlusion-needs-reveal ()
  (with-temp-buffer
    (gnosis-image-mode)
    (setq gnosis-image--regions (copy-tree gnosis-test-image--regions)
          gnosis-image--purpose 'region gnosis-image--depth 0)
    (let ((exits 0) (checks 0))
      (setq gnosis-image--check (lambda () (cl-incf checks)))
      (cl-letf (((symbol-function 'recursion-depth) (lambda () 1))
                ((symbol-function 'exit-recursive-edit) (lambda () (cl-incf exits)))
                ((symbol-function 'gnosis-image--render) #'ignore)
                ((symbol-function 'gnosis-image--position) (lambda (_) '(0.7 . 0.3))))
        (should-error (gnosis-image-submit))
        (gnosis-image-select '(mouse-1 nil))
        (should (= exits 0))
        (should (equal gnosis-image--selection "right"))
        (gnosis-image-submit)
        (should (= exits 1))
        (setq gnosis-image--purpose 'occlusion gnosis-image--accepted nil)
        (gnosis-image-submit)
        (should gnosis-image--revealed)
        (should-not gnosis-image--accepted)
        (should (= exits 1))
        (gnosis-image-submit)
        (should (= exits 2))
        (should (= checks 4))))))

(ert-deftest gnosis-image-author-save-reopen-and-edit ()
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
               (gnosis-add-image-thema "image-region"))
             (insert "Identify the left region")
             (should (eq (key-binding (kbd "C-c C-a")) #'gnosis-image-attach))
             (call-interactively (key-binding (kbd "C-c C-c")))
             (let* ((id (car (gnosis-select 'id 'themata nil t)))
                    (before (gnosis-select '* 'themata)))
               (gnosis-sqlite-close gnosis-db)
               (setq gnosis-db (gnosis-db--open gnosis-dir))
               (gnosis-edit-thema id)
               (should (string-match-p "Image resource" (buffer-string)))
               (call-interactively (key-binding (kbd "C-c C-c")))
               (should (equal before (gnosis-select '* 'themata)))))
         (dolist (name '("*Gnosis NEW*" "*Gnosis Edit*"))
           (when (get-buffer name) (kill-buffer name))))))))

(ert-deftest gnosis-image-inline-attachment-preserves-draft-on-cancel ()
  (gnosis-test-with-db
   (save-window-excursion
     (unwind-protect
         (progn
           (gnosis-add-thema "basic" "Question" nil "Answer")
           (let ((before (buffer-string)))
             (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) (signal 'quit nil))))
               (should (eq 'quit (condition-case err (gnosis-image-attach) (quit (car err))))))
             (should (equal before (buffer-string)))))
       (when (get-buffer "*Gnosis NEW*") (kill-buffer "*Gnosis NEW*"))))))

(ert-deftest gnosis-image-scheduled-pending-override-atomic-retry ()
  (gnosis-test-with-db
   (let* ((id (gnosis-test-image--add)) (buffer (gnosis-review--setup-buffer (list id))))
     (unwind-protect
         (with-current-buffer buffer
           (let* ((owner (gnosis-review--image-owner id))
                  (result (plist-put (gnosis-review-algorithm id nil) :image owner))
                  (override (gnosis-review--override-result result t))
                  (before (gnosis-select '* 'scheduler-state)))
             (should (equal (plist-get result :event-id) (plist-get override :event-id)))
             (should (eq owner (plist-get override :image)))
             (let ((execute (symbol-function 'gnosis-sqlite-execute)))
               (cl-letf (((symbol-function 'gnosis-sqlite-execute)
                          (lambda (db sql &optional values)
                            (when (string-match-p "INSERT INTO review_events" sql)
                              (signal 'quit nil))
                            (funcall execute db sql values))))
                 (should (eq 'quit (condition-case err (gnosis-review--write-result id t override)
                                     (quit (car err)))))))
             (should (equal before (gnosis-select '* 'scheduler-state)))
             (should-not (gnosis-select '* 'review-events))
             (gnosis-review--write-result id t override)
             (gnosis-review--write-result id t override)
             (should (= 1 (length (gnosis-select '* 'review-events))))))
       (kill-buffer buffer)))))

(ert-deftest gnosis-image-pending-drift-before-and-after-override ()
  (dolist (override-p '(nil t))
    (dolist (mutation '(state thema extras resource connection))
      (gnosis-test-with-db
       (let* ((id (gnosis-test-image--add)) (buffer (gnosis-review--setup-buffer (list id)))
              (database gnosis-db))
         (unwind-protect
             (with-current-buffer buffer
               (let* ((owner (gnosis-review--image-owner id))
                      (result (plist-put (gnosis-review-algorithm id nil) :image owner))
                      (result (if override-p (gnosis-review--override-result result t) result)))
                 (pcase mutation
                   ('state (setf (gnosis-review-state-event-id gnosis-review--state) "different"))
                   ('thema (gnosis-update 'themata '(= keimenon "Changed") `(= id ,id)))
                   ('extras (gnosis-update 'extras '(= parathema "Changed") `(= id ,id)))
                   ('resource (let ((scene (gnosis-image-resolve (car (gnosis-get 'hypothesis 'themata `(= id ,id))))))
                                (with-temp-file (alist-get 'path scene) (insert "corrupt"))))
                   ('connection (setq gnosis-db (gnosis-db--open gnosis-dir))))
                 (should-error (gnosis-review--image-check id owner))
                 (should-error (gnosis-review--write-result id override-p result))
                 (should-not (gnosis-select '* 'review-events))))
           (unless (eq gnosis-db database) (gnosis-sqlite-close database))
           (kill-buffer buffer)))))))

(ert-deftest gnosis-image-normal-dispatch-explicit-region-and-occlusion ()
  (dolist (type '("image-region" "image-occlusion"))
    (gnosis-test-with-db
     (let* ((id (gnosis-test-image--add type)) (buffer (gnosis-review--setup-buffer (list id))))
       (unwind-protect
           (with-current-buffer buffer
             (cl-letf (((symbol-function 'gnosis-image-input)
                        (lambda (_scene purpose _target check)
                          (should (eq purpose (if (equal type "image-region") 'region 'occlusion)))
                          (funcall check)
                          (list gnosis-test-image--regions "left")))
                       ((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "No self-report")))
                       ((symbol-function 'gnosis-image--decode) (lambda (_) '(image :type png)))
                       ((symbol-function 'svg-image) (lambda (&rest _) '(image :type svg)))
                       ((symbol-function 'image-type-available-p) (lambda (_) t))
                       ((symbol-function 'gnosis--read-string-with-input-method)
                        (lambda (_ answer) (should (equal answer "Left region")) "Left region")))
               (pcase-let ((`(,actual (,success . ,result)) (gnosis-review--display-thema id)))
                 (should (equal type actual)) (should success)
                 (should (plist-get result :image))
                 (should-not (gnosis-select '* 'review-events))
                 (gnosis-review--write-result id success result)
                 (should (= 1 (length (gnosis-select '* 'review-events)))))))
         (kill-buffer buffer))))))

(ert-deftest gnosis-image-cancelled-input-and-submit-drift-never-grade ()
  (dolist (condition '(quit drift))
    (gnosis-test-with-db
     (let* ((id (gnosis-test-image--add)) (buffer (gnosis-review--setup-buffer (list id))))
       (unwind-protect
           (with-current-buffer buffer
             (cl-letf (((symbol-function 'gnosis-image-input)
                        (lambda (_scene _purpose _target check)
                          (if (eq condition 'quit) (signal 'quit nil)
                            (gnosis-update 'extras '(= parathema "Changed") `(= id ,id))
                            (funcall check)))))
               (should (condition-case nil (progn (gnosis-review--display-thema id) nil)
                         ((error quit) t))))
             (should-not (gnosis-select '* 'review-events)))
         (kill-buffer buffer))))))

(ert-deftest gnosis-image-content-exchange-refuses-every-field ()
  (gnosis-test-with-db
   (let* ((id (gnosis-test-image--add)) (destination (expand-file-name "export.db" gnosis-dir)))
     (should-error (gnosis-export-db destination nil nil t))
     (should-not (file-exists-p destination))
     (gnosis-update 'themata '(= type "basic") `(= id ,id))
     (gnosis-sqlite-execute gnosis-db "UPDATE themata SET hypothesis = ? WHERE id = ?" (list '("hint") id))
     (let ((link "[[gnosis-image:broken]]"))
       (dolist (field '(keimenon hypothesis answer))
         (let ((before (gnosis-get field 'themata `(= id ,id))))
           (gnosis-sqlite-execute gnosis-db (format "UPDATE themata SET %s = ? WHERE id = ?" field)
                                  (list (if (eq field 'keimenon) link (list link)) id))
           (should-error (gnosis-export-db destination nil nil t))
           (should-error (gnosis-import--format-version-in-db gnosis-db "main"))
           (gnosis-sqlite-execute gnosis-db (format "UPDATE themata SET %s = ? WHERE id = ?" field) (list before id))))
       (dolist (field '(parathema review-image))
         (gnosis-update 'extras `(= ,field ,link) `(= id ,id))
         (should-error (gnosis-export-db destination nil nil t))
         (should-error (gnosis-import--format-version-in-db gnosis-db "main"))
         (gnosis-update 'extras `(= ,field "") `(= id ,id)))))))

(ert-deftest gnosis-image-standard-raster-headers ()
  ;; Real compressed PNG pixels for every standard color/depth combination;
  ;; the grayscale JPEG was encoded from an original 8x6 gradient.
  ;; This tests header acceptance, NOT native pixel decoding in batch Emacs.
  (gnosis-test-with-db
   (dolist (fixture
            '(
              ("png-0-1" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGAQAAAADWfuJWAAAAC0lEQVR4nGNgQAAAAAwAAXxMRMIAAAAASUVORK5CYII=")
              ("png-0-2" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGAgAAAACR3piGAAAAC0lEQVR4nGNgQAcAABIAAXfx+gAAAAAASUVORK5CYII=")
              ("png-0-4" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGBAAAAAAenm0mAAAAC0lEQVR4nGNgwAcAAB4AAfb96ZYAAAAASUVORK5CYII=")
              ("png-0-8" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGCAAAAADbboAnAAAADElEQVR4nGNgIAcAAAA2AAG2dLktAAAAAElFTkSuQmCC")
              ("png-0-16" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGEAAAAACL/lxkAAAADElEQVR4nGNgoAcAAABmAAGJ8xJHAAAAAElFTkSuQmCC")
              ("png-2-8" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGCAIAAABxZ0isAAAADElEQVR4nGNgGIwAAACWAAGzNRKNAAAAAElFTkSuQmCC")
              ("png-2-16" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGEAIAAAAh95TvAAAADUlEQVR4nGNgGAWEAQABJgAByIJELwAAAABJRU5ErkJggg==")
              ("png-3-1" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGAQMAAADEy024AAAABlBMVEUAAAD///+l2Z/dAAAAC0lEQVR4nGNgQAAAAAwAAXxMRMIAAAAASUVORK5CYII=")
              ("png-3-2" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGAgMAAACDazdoAAAABlBMVEUAAAD///+l2Z/dAAAAC0lEQVR4nGNgQAcAABIAAXfx+gAAAAAASUVORK5CYII=")
              ("png-3-4" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGBAMAAAAMK8LIAAAABlBMVEUAAAD///+l2Z/dAAAAC0lEQVR4nGNgwAcAAB4AAfb96ZYAAAAASUVORK5CYII=")
              ("png-3-8" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGCAMAAADJ2y/JAAAABlBMVEUAAAD///+l2Z/dAAAADElEQVR4nGNgIAcAAAA2AAG2dLktAAAAAElFTkSuQmCC")
              ("png-4-8" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGCAQAAABUDBdwAAAADElEQVR4nGNgoAcAAABmAAGJ8xJHAAAAAElFTkSuQmCC")
              ("png-4-16" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGEAQAAAAEnMszAAAADElEQVR4nGNgGA4AAADGAAHAAMfmAAAAAElFTkSuQmCC")
              ("png-6-8" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGCAYAAAD+Bd/7AAAADElEQVR4nGNgGA4AAADGAAHAAMfmAAAAAElFTkSuQmCC")
              ("png-6-16" png "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGEAYAAACulQO4AAAADUlEQVR4nGNgGAUDDwABhgABZkGsQwAAAABJRU5ErkJggg==")
              ("gray-jpeg" jpeg "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAMCAgICAgMCAgIDAwMDBAYEBAQEBAgGBgUGCQgKCgkICQkKDA8MCgsOCwkJDRENDg8QEBEQCgwSExIQEw8QEBD/wAALCAAGAAgBAREA/8QAFAABAAAAAAAAAAAAAAAAAAAAB//EABsQAAAHAQAAAAAAAAAAAAAAAAABBQcZU5PS/9oACAEBAAA/ACmWx0K1HUuh/9k=")
              ))
     (let ((file (expand-file-name (car fixture) gnosis-dir))
           (coding-system-for-write 'no-conversion))
       (with-temp-file file
         (set-buffer-multibyte nil)
         (insert (base64-decode-string (nth 2 fixture))))
       (should (equal (list 8 6 (nth 1 fixture)) (gnosis-image--dimensions file)))
       (should (gnosis-image-resolve (gnosis-image-import file)))))))

(ert-deftest gnosis-image-inline-fields-command-codec-roundtrip ()
  (gnosis-test-with-db
   (save-window-excursion
     (let ((reference (gnosis-image-import (gnosis-test-image--file)))
           (gnosis-save-hook nil))
       (unwind-protect
           (progn
             (gnosis-add-thema "basic" "Question" "Hint" "Answer" "Explanation")
             (dolist (heading '("Keimenon" "Hypothesis" "Answer" "Parathema"))
               (goto-char (point-min))
               (search-forward (downcase (concat "** " heading)))
               (forward-line)
               (end-of-line)
               (insert " ")
               (cl-letf (((symbol-function 'gnosis-image--read-resource)
                          (lambda (&rest _) reference)))
                 (execute-kbd-macro (kbd "C-c C-a"))))
             (let ((fields (gnosis-export-parse-themata)))
               (should (= 4 (length (gnosis-image-references fields))))
               (execute-kbd-macro (kbd "C-c C-c"))
               (let ((id (car (gnosis-select 'id 'themata nil t))))
                 (gnosis-edit-thema id)
                 (let ((saved (gnosis-export-parse-themata)))
                   (should (equal (cddar fields) (cddar saved)))
                   (should (= 4 (length (gnosis-image-references saved))))))))
         (dolist (name '("*Gnosis NEW*" "*Gnosis Edit*"))
           (when (get-buffer name) (kill-buffer name))))))))

(ert-deftest gnosis-image-native-keymap-edit-delete-and-cancel ()
  (gnosis-test-image--delete-cancel gnosis-test-image--regions))

(ert-deftest gnosis-image-format-values-and-owned-refresh ()
  (gnosis-test-with-db
   (save-window-excursion
     (let* ((reference (gnosis-image-import (gnosis-test-image--file)))
            (text (propertize (format "Q [[gnosis-image:%s][Caption]] Z" reference)
                              'read-only t 'face 'bold))
            (before (copy-sequence text))
            (image '(image :type png :file "fixture"))
            (buffer (gnosis-review--setup-buffer nil))
            (width 500))
       (unwind-protect
           (cl-letf (((symbol-function 'gnosis-image--decode) (lambda (_) image))
                     ((symbol-function 'window-body-width) (lambda (&rest _) width)))
             (with-temp-buffer
               (insert "Unrelated draft")
               (let ((snapshot (buffer-string)))
                 (gnosis-image-format-string text)
                 (should (equal snapshot (buffer-string))))
               (should-not (memq #'gnosis-image-refresh window-configuration-change-hook)))
             (switch-to-buffer buffer)
             (insert (gnosis-image-format-string text))
             (goto-char (point-max))
             (set-buffer-modified-p nil)
             (let ((content (buffer-substring-no-properties (point-min) (point-max)))
                   (position (point)))
               (setq width 750)
               (run-hooks 'window-configuration-change-hook)
               (should (memq #'gnosis-image-refresh window-configuration-change-hook))
               (should (= 718 (plist-get (cdr (get-text-property 3 'display)) :max-width)))
               (should (equal content (buffer-substring-no-properties (point-min) (point-max))))
               (should (= position (point)))
               (should-not (buffer-modified-p)))
             (should (equal-including-properties text before))
             (should (equal image '(image :type png :file "fixture"))))
         (kill-buffer buffer))))))

(ert-deftest gnosis-image-all-content-preflight-before-input ()
  (gnosis-test-with-db
   (let* ((reference (gnosis-image-import (gnosis-test-image--file)))
          (link (format "[[gnosis-image:%s]]" reference))
          (id (gnosis-generate-id)))
     (gnosis-add-thema-fields "basic" "Q" '("H") '("A") "P" nil 0 nil nil id)
     (let ((buffer (gnosis-review--setup-buffer (list id))))
       (unwind-protect
           (with-current-buffer buffer
             (dolist (field '(keimenon hypothesis answer parathema review-image))
               (let* ((table (if (memq field '(parathema review-image)) 'extras 'themata))
                      (old (gnosis-get field table `(= id ,id)))
                      (value (if (memq field '(hypothesis answer)) (list link) link)))
                 (gnosis-update table `(= ,field ,value) `(= id ,id))
                 (cl-letf (((symbol-function 'gnosis-image--decode)
                            (lambda (_) (user-error "Corrupt native pixels")))
                           ((symbol-function 'gnosis-review-basic)
                            (lambda (_) (ert-fail "Input ran before image preflight"))))
                   (should-error (gnosis-review--display-thema id) :type 'user-error))
                 (should-not (gnosis-select '* 'review-events))
                 (gnosis-update table `(= ,field ,old) `(= id ,id)))))
         (kill-buffer buffer))))))

(ert-deftest gnosis-image-practice-acceptance-retry-and-scheduled-undo ()
  (dolist (mode '(practice due))
    (gnosis-test-with-db
     (let* ((id (gnosis-test-image--add))
            (buffer (gnosis-review--setup-buffer (list id) mode))
            (before (gnosis-select '* 'scheduler-state)))
       (unwind-protect
           (with-current-buffer buffer
             (setf (gnosis-review-state-persistent-p gnosis-review--state) t
                   (gnosis-review-state-policy gnosis-review--state) (gnosis-review-practice-policy))
             (gnosis-review--save-session gnosis-review--state)
             (let* ((owner (gnosis-review--image-owner id))
                    (result (plist-put (gnosis-review-algorithm id t) :image owner)))
               (gnosis-review-result id t result)
               (gnosis-review-result id t result)
               (should (= 1 (gnosis-review-state-reviewed gnosis-review--state)))
               (if (eq mode 'practice)
                   (progn
                     (should (equal before (gnosis-select '* 'scheduler-state)))
                     (should-not (gnosis-select '* 'review-events))
                     (should (= 1 (length (gnosis-select '* 'practice-events)))))
                 (should (= 1 (length (gnosis-select '* 'review-events))))
                 (let ((slot (gnosis-review-state-undo gnosis-review--state)))
                   (setq gnosis-review--state
                         (gnosis-review-undo (plist-get slot :event-id) (plist-get slot :correction-id)))
                   (should (equal (list id) (gnosis-review-state-remaining gnosis-review--state)))
                   (should-error (gnosis-review--write-result id t result))))))
         (kill-buffer buffer))))))

(ert-deftest gnosis-image-and-model-guards-coexist-through-overrides ()
  (dolist (corrupt '(image model))
    (gnosis-test-with-db
     (let ((scene-file (expand-file-name "scene.json" gnosis-dir))
           (obj-file (expand-file-name "shape.obj" gnosis-dir)))
       (with-temp-file obj-file (insert "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n"))
       (with-temp-file scene-file
         (insert "{\"objects\":[{\"id\":\"shape\",\"label\":\"Shape\",\"path\":\"shape.obj\"}],\"initial_view\":[0,-90,1],\"license\":\"Original\",\"source\":\"ERT\"}"))
       (let* ((reference (gnosis-model-import scene-file))
              (image (gnosis-image-import (gnosis-test-image--file)))
              (id (gnosis-generate-id)))
         (gnosis-add-thema-fields "model" "Select shape" (list reference "0" "-90" "1")
                                  '("shape") (format "[[gnosis-image:%s]]" image) nil 0 nil nil id)
         (let ((buffer (gnosis-review--setup-buffer (list id))))
           (unwind-protect
               (with-current-buffer buffer
                 (let* ((model (list gnosis-db
                                     (gnosis-select '[type keimenon hypothesis answer] 'themata `(= id ,id))
                                     buffer gnosis-review--state
                                     (copy-tree (gnosis-review--state-data gnosis-review--state))))
                        (owner (cl-letf (((symbol-function 'gnosis-image--decode) #'ignore))
                                 (gnosis-review--image-owner id)))
                        (result (append (gnosis-review-algorithm id nil) (list :model model :image owner)))
                        (override (gnosis-review--override-result result t)))
                   (should (eq model (plist-get override :model)))
                   (should (eq owner (plist-get override :image)))
                   (let ((file (if (eq corrupt 'image)
                                   (alist-get 'path (gnosis-image-resolve image))
                                 (expand-file-name "shape.obj"
                                                   (expand-file-name (car (split-string reference "/"))
                                                                     (gnosis-assets-root))))))
                     (with-temp-file file (insert "corrupt")))
                   (should-error (gnosis-review--write-result id t override))
                   (should-not (gnosis-select '* 'review-events))))
             (kill-buffer buffer))))))))

(ert-deftest gnosis-image-model-attachment-key-is-buffer-owned ()
  (gnosis-test-with-db
   (save-window-excursion
     (unwind-protect
         (progn
           (gnosis-add-thema "model" "Q" "resource" "shape")
           (should (eq (key-binding (kbd "C-c C-a")) #'gnosis-model-attach))
           (kill-buffer (current-buffer))
           (gnosis-add-thema "basic" "Q" nil "A")
           (should (eq (key-binding (kbd "C-c C-a")) #'gnosis-image-attach)))
       (when (get-buffer "*Gnosis NEW*") (kill-buffer "*Gnosis NEW*"))))))

(ert-deftest gnosis-image-reattach-cancel-and-revision-preserve-draft ()
  (gnosis-test-with-db
   (save-window-excursion
     (let* ((file (gnosis-test-image--file))
            (old (gnosis-image-import file gnosis-test-image--regions))
            (revised (gnosis-image-import file (cdr gnosis-test-image--regions))))
       (unwind-protect
           (progn
             (gnosis-add-thema "image-region" "My question" old "left" "My explanation" '("my_tag"))
             (let ((before (buffer-string)))
               (cl-letf (((symbol-function 'gnosis-image--read-fields)
                          (lambda (&rest _) (signal 'quit nil))))
                 (should (eq 'quit (condition-case err (gnosis-image-attach) (quit (car err))))))
               (should (equal-including-properties before (buffer-string))))
             (cl-letf (((symbol-function 'gnosis-image--read-fields)
                        (lambda (reference &optional _occlusion)
                                                  (should (equal reference old))
                          (list (list revised) '("right")))))
               (execute-kbd-macro (kbd "C-c C-a")))
             (let ((row (car (gnosis-export-parse-themata))))
               (should (equal "My question" (nth 2 row)))
               (should (equal (list revised) (nth 3 row)))
               (should (equal '("right") (nth 4 row)))
               (should (equal "My explanation" (nth 5 row)))
               (should (equal '("my_tag") (nth 6 row))))
             (should (gnosis-image-resolve old "left"))
             (let ((owner (current-buffer)))
               (cl-letf (((symbol-function 'gnosis-image--read-fields)
                          (lambda (&rest _)
                            (with-current-buffer owner (goto-char (point-max)) (insert "User typing"))
                            (list (list old) '("left")))))
                 (should-error (gnosis-image-attach)))
               (should (string-suffix-p "User typing" (buffer-string)))
               (should (equal (list revised) (nth 3 (car (gnosis-export-parse-themata)))))))
         (when (get-buffer "*Gnosis NEW*") (kill-buffer "*Gnosis NEW*")))))))

(ert-deftest gnosis-image-display-callers-preserve-boundary-images ()
  (gnosis-test-with-db
   (let* ((reference (gnosis-image-import (gnosis-test-image--file)))
          (link (format "[[gnosis-image:%s]]" reference))
          (buffer (gnosis-review--setup-buffer nil)))
     (unwind-protect
         (with-current-buffer buffer
           (dolist (center '(nil t))
             (setq-local gnosis-center-content center)
             (dolist (text (list link (concat link " Text") (concat "Text " link)
                                (concat link " " link) (concat link "\n" link)
                                (concat link " [[https://example.org][Caption]] " link)
                                (concat "Text " (make-string 100 ?x) " " link)))
               (dolist (display '(gnosis-display-keimenon gnosis-display-hint
                                 gnosis-display-parathema gnosis-display-basic-answer))
                 (erase-buffer)
                 (let ((original (propertize text 'help-echo "Retained text"))
                       (expected (length (gnosis-image-references text))))
                   (cl-letf (((symbol-function 'gnosis-image--decode)
                              (lambda (_) '(image :type png :file "fixture"))))
                     (if (eq display 'gnosis-display-basic-answer)
                         (funcall display original t "")
                       (funcall display original)))
                   (should (equal text (substring-no-properties original)))
                   (let ((positions
                          (cl-loop for pos from (point-min) below (point-max)
                                   when (get-text-property pos 'gnosis-image-reference)
                                   collect pos)))
                     (should (= expected (length positions)))
                     (dolist (pos positions)
                       (should (equal reference (get-text-property pos 'gnosis-image-reference)))
                       (should (eq 'image (car (get-text-property pos 'display))))))
                   (when (string-match-p "Text" text)
                     (goto-char (point-min))
                     (search-forward "Text")
                     (should (equal "Retained text" (get-text-property (1- (point)) 'help-echo)))))))))
       (kill-buffer buffer)))))

(ert-deftest gnosis-image-region-edit-resource-drift-preserves-draft ()
  (dolist (mutation '(raster manifest))
    (gnosis-test-with-db
     (save-window-excursion
       (let* ((file (gnosis-test-image--file))
              (reference (gnosis-image-import file gnosis-test-image--regions)))
         (unwind-protect
             (progn
               (gnosis-add-thema "image-region" "Question" reference "left" "Explanation")
               (let ((before (buffer-string))
                     (inventory (directory-files (gnosis-assets-root) nil "^[^.]"))
                     (rows (gnosis-select '* 'themata)))
                 (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                           ((symbol-function 'completing-read)
                            (lambda (_ choices &rest _) (caar choices)))
                           ((symbol-function 'read-string) (lambda (&rest _) ""))
                           ((symbol-function 'gnosis-image-edit-regions)
                            (lambda (scene)
                              (if (eq mutation 'raster)
                                  (let ((coding-system-for-write 'no-conversion))
                                    (write-region
                                     (base64-decode-string
                                      "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAGCAIAAABxZ0isAAAADElEQVR4nGNgGIwAAACWAAGzNRKNAAAAAElFTkSuQmCC")
                                     nil (alist-get 'path scene) nil 'silent))
                                ;; Valid JSON, semantically identical, different bytes.
                                (write-region " " nil
                                              (expand-file-name "image.json" (alist-get 'directory scene))
                                              t 'silent))
                              (cdr (copy-tree gnosis-test-image--regions)))))
                   (should-error (call-interactively (key-binding (kbd "C-c C-a")))
                                 :type 'user-error))
                 (should (equal-including-properties before (buffer-string)))
                 (should (equal rows (gnosis-select '* 'themata)))
                 (should (equal inventory (directory-files (gnosis-assets-root) nil "^[^.]")))
                 (should (equal (list reference) (nth 3 (car (gnosis-export-parse-themata)))))))
           (when (get-buffer "*Gnosis NEW*") (kill-buffer "*Gnosis NEW*"))))))))

(ert-deftest gnosis-image-mixed-case-normal-due-and-practice ()
  (dolist (type '("Image-region" "Image-occlusion"))
    (dolist (mode '(due practice))
      (gnosis-test-with-db
       (let* ((id (gnosis-test-image--add (downcase type)))
              (reference (gnosis-get 'hypothesis 'themata `(= id ,id))))
         (gnosis-update-thema id "Question" reference '("left") "Explanation" nil nil type)
         (should (equal type (gnosis-get 'type 'themata `(= id ,id))))
         (let ((buffer (gnosis-review--setup-buffer (list id) mode))
               (before (gnosis-select '* 'scheduler-state)))
           (unwind-protect
               (with-current-buffer buffer
                 (setf (gnosis-review-state-persistent-p gnosis-review--state) t
                       (gnosis-review-state-policy gnosis-review--state) (gnosis-review-practice-policy))
                 (gnosis-review--save-session gnosis-review--state)
                 (cl-letf (((symbol-function 'gnosis-image-input)
                            (lambda (scene purpose target check)
                              (should (eq purpose (if (equal type "Image-region") 'region 'occlusion)))
                              (should (equal target "left"))
                              (should (alist-get 'path scene))
                              (funcall check)
                              (should-not (gnosis-select '* 'review-events))
                              (should-not (gnosis-select '* 'practice-events))
                              (list gnosis-test-image--regions "left")))
                           ((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "No self-report")))
                       ((symbol-function 'gnosis-image--decode) (lambda (_) '(image :type png)))
                       ((symbol-function 'svg-image) (lambda (&rest _) '(image :type svg)))
                       ((symbol-function 'image-type-available-p) (lambda (_) t))
                       ((symbol-function 'gnosis--read-string-with-input-method)
                        (lambda (_ answer) (should (equal answer "Left region")) "Left region")))
                   (pcase-let ((`(,actual (,success . ,result)) (gnosis-review--display-thema id)))
                     (should (equal type actual))
                     (should success)
                     (should (plist-get result :image))
                     (gnosis-review--image-check id (plist-get result :image))
                     (should-not (gnosis-select '* 'review-events))
                     (should-not (gnosis-select '* 'practice-events))
                     (gnosis-review-result id success result)
                     (gnosis-review-result id success result)
                     (should (= 1 (gnosis-review-state-reviewed gnosis-review--state)))
                     (if (eq mode 'practice)
                         (progn
                           (should (equal before (gnosis-select '* 'scheduler-state)))
                           (should-not (gnosis-select '* 'review-events))
                           (should (= 1 (length (gnosis-select '* 'practice-events)))))
                       (should (= 1 (length (gnosis-select '* 'review-events))))))))
             (kill-buffer buffer))))))))

(ert-deftest gnosis-image-wrong-selection-feedback-shows-label-not-id ()
  (gnosis-test-with-db
   (let* ((id (gnosis-test-image--add))
          (buffer (gnosis-review--setup-buffer (list id))))
     (unwind-protect
         (with-current-buffer buffer
           (cl-letf (((symbol-function 'gnosis-image-input)
                      (lambda (_scene _purpose _target check)
                        (funcall check)
                        (should-not (string-match-p "Right region" (buffer-string)))
                        (should-not (gnosis-select '* 'review-events))
                        (list gnosis-test-image--regions "right"))))
             (pcase-let ((`(,_ (,success . ,result)) (gnosis-review--display-thema id)))
               (should-not success)
               (should (string-match-p "Answer: Left region" (buffer-string)))
               (should (string-match-p "Your answer: Right region" (buffer-string)))
               (should-not (string-match-p "Your answer: right[ \t]*$" (buffer-string)))
               (should (equal '("left") (gnosis-get 'answer 'themata `(= id ,id))))
               (should-not (gnosis-select '* 'review-events))
               (gnosis-review-result id success result)
               (should (= 1 (length (gnosis-select '* 'review-events)))))))
       (kill-buffer buffer)))))


(ert-deftest gnosis-image-occlusion-typed-inline-fields-and-legacy ()
  (dolist (canonical '(nil t))
    (dolist (correct '(nil t))
      (gnosis-test-with-db
       (let* ((id (gnosis-test-image--add "image-occlusion"))
              (reference (car (gnosis-get 'hypothesis 'themata `(= id ,id))))
              (answer (if canonical "Editable anatomy" "Left region"))
              (text (if correct (upcase answer) "unrelated"))
              (buffer (gnosis-review--setup-buffer (list id))))
         (when canonical
           (gnosis-update-thema id "Question" (list reference "left")
                                (list answer) "Explanation" nil nil "image-occlusion"))
         (unwind-protect
             (with-current-buffer buffer
               (cl-letf (((symbol-function 'gnosis-image--decode) (lambda (_) '(image :type png)))
                         ((symbol-function 'svg-image) (lambda (&rest _) '(image :type svg)))
                         ((symbol-function 'image-type-available-p) (lambda (_) t))
                         ((symbol-function 'gnosis-image-input) (lambda (&rest _) (ert-fail "Separate viewer")))
                         ((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "Self-report")))
                         ((symbol-function 'gnosis--read-string-with-input-method)
                          (lambda (_ expected)
                            (should (equal expected answer))
                            (should (eq (current-buffer) buffer))
                            (let ((pos (text-property-not-all (point-min) (point-max) 'gnosis-image-mask nil)))
                              (should pos)
                              (should-not (nth 2 (get-text-property pos 'gnosis-image-mask))))
                            text)))
                 (let ((pending (gnosis-review-image-occlusion id)))
                   (should (eq (car pending) correct))
                   (should (plist-get (cdr pending) :image))
                   (let ((pos (text-property-not-all (point-min) (point-max) 'gnosis-image-mask nil)))
                     (should (nth 2 (get-text-property pos 'gnosis-image-mask))))
                   (should-not (gnosis-select '* 'review-events)))))
           (kill-buffer buffer)))))))

(ert-deftest gnosis-image-occlusion-feedback-restores-source-through-resize ()
  (gnosis-test-image--feedback-resize gnosis-test-image--regions nil '(3.2)))

(ert-deftest gnosis-image-occlusion-distinct-text-fields ()
  (gnosis-test-with-db
   (let ((reference (gnosis-image-import (gnosis-test-image--file) gnosis-test-image--regions)))
     (gnosis-image-validate-fields "image-occlusion" "q" (list reference "left") '("Anatomy answer") "")
     (should-error (gnosis-image-validate-fields "image-occlusion" "q" (list reference "missing") '("Anatomy answer") "")))))


(ert-deftest gnosis-image-occlusion-native-author-save-reopen ()
  (gnosis-test-image--author-roundtrip gnosis-test-image--regions "hide-target"))

(ert-deftest gnosis-image-occlusion-typed-cancel-and-owner-guards ()
  (dolist (fault '(quit state mode thema resource database))
    (gnosis-test-with-db
     (let* ((id (gnosis-test-image--add "image-occlusion"))
            (buffer (gnosis-review--setup-buffer (list id)))
            (database gnosis-db))
       (unwind-protect
           (with-current-buffer buffer
             (cl-letf (((symbol-function 'gnosis-image--decode) (lambda (_) '(image :type png)))
                       ((symbol-function 'svg-image) (lambda (&rest _) '(image :type svg)))
                       ((symbol-function 'image-type-available-p) (lambda (_) t))
                       ((symbol-function 'gnosis--read-string-with-input-method)
                        (lambda (&rest _)
                          (pcase fault
                            ('quit (signal 'quit nil))
                            ('state (setf (gnosis-review-state-event-id gnosis-review--state) "changed"))
                            ('mode (fundamental-mode))
                            ('thema (gnosis-update 'extras '(= parathema "changed") `(= id ,id)))
                            ('resource
                             (with-temp-file (alist-get 'path (gnosis-image-resolve
                                                              (car (gnosis-get 'hypothesis 'themata `(= id ,id)))))
                               (insert "corrupt")))
                            ('database (setq gnosis-db (gnosis-db--open gnosis-dir))))
                          "Left region")))
               (should (condition-case nil (progn (gnosis-review-image-occlusion id) nil)
                         ((error quit) t))))
             (should-not (gnosis-select '* 'review-events)))
         (unless (eq database gnosis-db) (gnosis-sqlite-close database))
         (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest gnosis-image-org-export-refuses-unrepresentable-tags ()
  "Keep retained tag identities and media intact when Org cannot encode them."
  (gnosis-test-with-db
   (let* ((id (gnosis-test-image--add "image-occlusion"))
          (reference (car (gnosis-get 'hypothesis 'themata `(= id ,id))))
          (scene (gnosis-image-resolve reference "left"))
          (before (gnosis-select '* 'themata)))
     ;; Domain tags remain unrestricted; only native Org rendering refuses.
     (gnosis-modify-thema-tags (list id) '("image-test") '("image_test"))
     (with-temp-buffer
       (org-mode)
       (insert "Existing export text\n")
       (let ((text (buffer-string)))
         (should-error (gnosis-export--insert-themata (list id))
                       :type 'user-error)
         (should (equal text (buffer-string)))))
     (gnosis-sqlite-close gnosis-db)
     (setq gnosis-db (gnosis-db--open gnosis-dir))
     (should (equal (gnosis-get-tags-for-ids (list id)) '("image-test")))
     (should (equal before (gnosis-select '* 'themata)))
     (should (equal scene (gnosis-image-resolve reference "left"))))))

(ert-deftest gnosis-image-occlusion-legacy-edit-save-and-org-export ()
  (gnosis-test-image--legacy-roundtrip gnosis-test-image--regions))

(ert-deftest gnosis-image-resolver-paths-are-authoritative ()
  (gnosis-test-with-db
    (dolist (version '(1 2))
      (let* ((file (gnosis-test-image--file))
             (regions (if (= version 1) gnosis-test-image--regions
                        '(((id . "left") (label . "Left region")
                           (rects . ((0.0 0.0 0.4 0.8)))))))
             (ordinary (gnosis-image-import file regions "Source" "Author"))
             (ordinary-scene (gnosis-image-resolve ordinary))
             (manifest (with-temp-buffer
                         (insert-file-contents
                          (expand-file-name "image.json" (alist-get 'directory ordinary-scene)))
                         (json-parse-buffer :object-type 'alist)))
             (crafted (append `((path . ,file) (directory . "/not-managed")) manifest))
             (revision (gnosis-assets-import gnosis-dir '("original.png")
                                             (list (cons "image.json" (json-encode crafted)))))
             (reference (concat revision "/image.json"))
             (scene (gnosis-image-resolve reference "left"))
             (directory (expand-file-name revision (gnosis-assets-root))))
        (should (= version (alist-get 'version scene)))
        (should (equal directory (alist-get 'directory scene)))
        (should (equal (expand-file-name "original.png" directory) (alist-get 'path scene)))
        ;; Deleting the untrusted path cannot affect native rendering's input.
        (delete-file file)
        (should (equal scene (gnosis-image-resolve reference)))
        (should (dom-by-tag (gnosis-image--svg scene regions 8 6 'edit nil nil nil) 'image))
        (should (equal ordinary
                       (gnosis-image-import (alist-get 'path ordinary-scene)
                                            regions "Source" "Author")))))))

(ert-deftest gnosis-image-reassociation-retires-resize-and-commands ()
  (gnosis-test-with-db
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (gnosis-image-mode)
        (setq gnosis-image--scene (gnosis-image-resolve
                                   (gnosis-image-import (gnosis-test-image--file)))
              gnosis-image--regions (copy-tree gnosis-test-image--regions)
              gnosis-image--purpose 'edit gnosis-image--selection "left")
        (let ((queued #'gnosis-image--render)
              (successor "Unrelated unsaved successor text"))
          (set-visited-file-name (expand-file-name "successor.txt" gnosis-dir) t)
          (should (eq major-mode 'gnosis-image-mode))
          (let ((inhibit-read-only t)) (erase-buffer) (insert successor))
          (cl-letf (((symbol-function 'svg-image) (lambda (&rest _) '(image :type svg)))
                    ((symbol-function 'insert-image) (lambda (&rest _) (insert "[image]"))))
            (dolist (detach '(nil t))
              (when detach (set-visited-file-name nil t))
              (dotimes (_ 2)
                (run-hooks 'window-configuration-change-hook)
                (funcall queued))
              (should (equal successor (buffer-string)))
              (should (buffer-modified-p))
              (dolist (command '(gnosis-image-next-rectangle gnosis-image-submit))
                (should-error (call-interactively command) :type 'user-error))
              (should-error (gnosis-image-select '(mouse-1 nil)) :type 'user-error)
              (gnosis-image-cancel)
              (should (equal successor (buffer-string)))))
          (set-buffer-modified-p nil))))))

(ert-deftest gnosis-image-reassociation-during-prompt-refuses-edit ()
  (gnosis-test-with-db
    (with-temp-buffer
      (gnosis-image-mode)
      (setq gnosis-image--regions (copy-tree gnosis-test-image--regions)
            gnosis-image--purpose 'edit gnosis-image--selection "left")
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _)
                   (set-visited-file-name (expand-file-name "successor.txt" gnosis-dir) t)
                   (set-visited-file-name nil t)
                   (let ((inhibit-read-only t)) (insert "Successor"))
                   "Changed")))
        (should-error (gnosis-image-rename-target) :type 'user-error))
      (should (equal gnosis-test-image--regions gnosis-image--regions))
      (should (equal "Successor" (buffer-string))))))

(ert-deftest gnosis-image-reassociation-input-cleanup-preserves-successor ()
  (dolist (detach '(nil t))
    (dolist (finish '(return quit))
      (gnosis-test-with-db
        (let ((scene (gnosis-image-resolve (gnosis-image-import (gnosis-test-image--file))))
              successor)
          (unwind-protect
              (cl-letf (((symbol-function 'gnosis-image--decode) #'ignore)
                        ((symbol-function 'image-type-available-p) (lambda (_) t))
                        ((symbol-function 'gnosis-image--render) #'ignore)
                        ((symbol-function 'recursive-edit)
                         (lambda ()
                           (setq successor (current-buffer))
                           (set-visited-file-name (expand-file-name "successor.txt" gnosis-dir) t)
                           (when detach (set-visited-file-name nil t))
                           (let ((inhibit-read-only t)) (erase-buffer) (insert "Keep my edits"))
                           (if (eq finish 'quit) (signal 'quit nil)
                             (setq gnosis-image--accepted t)))))
                (should (condition-case nil (progn (gnosis-image-input scene 'edit) nil)
                          ((error quit) t)))
                (should (buffer-live-p successor))
                (with-current-buffer successor
                  (should (equal "Keep my edits" (buffer-string)))
                  (should (buffer-modified-p))
                  ;; Retained q cannot abort a later unrelated input at the
                  ;; same depth after the original input has unwound.
                  (cl-letf (((symbol-function 'recursion-depth) (lambda () 1))
                            ((symbol-function 'abort-recursive-edit)
                             (lambda () (ert-fail "Aborted successor input"))))
                    (call-interactively (key-binding (kbd "q"))))))
            (when (buffer-live-p successor)
              (with-current-buffer successor (setq gnosis-image--depth nil) (set-buffer-modified-p nil))
              (kill-buffer successor))))))))

(ert-deftest gnosis-image-reassociation-nested-input-remains-cancellable ()
  (gnosis-test-with-db
    (with-temp-buffer
      (gnosis-image-mode)
      (setq gnosis-image--purpose 'edit gnosis-image--depth 0)
      (let ((aborts 0))
        ;; Association inside nested input must not abort that inner reader.
        (cl-letf (((symbol-function 'recursion-depth) (lambda () 2))
                  ((symbol-function 'abort-recursive-edit) (lambda () (cl-incf aborts))))
          (set-visited-file-name (expand-file-name "successor.txt" gnosis-dir) t)
          (should (= aborts 0)))
        ;; Once that reader returns, q can still leave the original input;
        ;; the input's cleanup, not this command, owns buffer destruction.
        (cl-letf (((symbol-function 'recursion-depth) (lambda () 1))
                  ((symbol-function 'abort-recursive-edit) (lambda () (cl-incf aborts))))
          (call-interactively (key-binding (kbd "q")))
          (should (= aborts 1)))))))

(ert-deftest gnosis-image-creation-unwind ()
  "Early error and quit destroy the untouched viewer and restore windows."
  (dolist (boundary '(pop windows))
    (dolist (failure '(error quit))
      (save-window-excursion
        (let ((origin (current-buffer))
              (configuration (current-window-configuration))
              viewer caught rendered entered)
          (unwind-protect
              (let ((buffer-list-update-hook
                     (list (lambda ()
                             (when (and (not viewer)
                                        (string-prefix-p "*Gnosis Image*" (buffer-name)))
                               (setq viewer (current-buffer))
                               (when (eq boundary 'pop) (signal failure '("Early exit")))))))
                    (checks 0))
                (cl-letf (((symbol-function 'gnosis-image--decode) #'ignore)
                          ((symbol-function 'image-type-available-p) (lambda (_) t))
                          ((symbol-function 'gnosis-image--render) (lambda () (setq rendered t)))
                          ((symbol-function 'recursive-edit) (lambda () (setq entered t))))
                  (condition-case err
                      (gnosis-image-input
                       nil 'edit nil
                       (lambda ()
                         (when (and (eq boundary 'windows) (= (cl-incf checks) 3))
                           (signal failure '("Early exit")))))
                    ((error quit) (setq caught err))))
                (should (equal caught (list failure "Early exit")))
                (should viewer)
                (should-not (buffer-live-p viewer))
                (should-not rendered)
                (should-not entered)
                (should (eq origin (current-buffer)))
                (should (compare-window-configurations configuration (current-window-configuration))))
            (when (buffer-live-p viewer) (kill-buffer viewer))))))))

(ert-deftest gnosis-image-creation-preserves-successor ()
  "Never initialize or destroy a repurposed buffer, even if it is empty again."
  (dolist (mutation '(text text-roundtrip mode mode-roundtrip file detach))
    (save-window-excursion
      (let (viewer rendered entered contents mode file tick map)
        (unwind-protect
            (let ((buffer-list-update-hook
                   (list (lambda ()
                           (when (and (not viewer)
                                      (string-prefix-p "*Gnosis Image*" (buffer-name)))
                             (setq viewer (current-buffer))
                             (pcase mutation
                               ((or 'text 'text-roundtrip)
                                (insert "Successor text")
                                (when (eq mutation 'text-roundtrip) (erase-buffer)))
                               ((or 'mode 'mode-roundtrip)
                                (text-mode)
                                (when (eq mutation 'mode-roundtrip) (fundamental-mode)))
                               ((or 'file 'detach)
                                (set-visited-file-name
                                 (make-temp-name (expand-file-name "gnosis-image-successor-" temporary-file-directory)) t)
                                (when (eq mutation 'detach) (set-visited-file-name nil t))))
                             (setq contents (buffer-string) mode major-mode file buffer-file-name
                                   tick (buffer-modified-tick) map (current-local-map)))))))
              (cl-letf (((symbol-function 'gnosis-image--decode) #'ignore)
                        ((symbol-function 'image-type-available-p) (lambda (_) t))
                        ((symbol-function 'gnosis-image--render) (lambda () (setq rendered t)))
                        ((symbol-function 'recursive-edit) (lambda () (setq entered t))))
                (should-error (gnosis-image-input nil 'edit) :type 'user-error))
              (should-not rendered)
              (should-not entered)
              (should (buffer-live-p viewer))
              (with-current-buffer viewer
                (should (equal contents (buffer-string)))
                (should (eq mode major-mode))
                (should (equal file buffer-file-name))
                (should (= tick (buffer-modified-tick)))
                (should (eq map (current-local-map)))))
          (when (buffer-live-p viewer)
            (with-current-buffer viewer (set-buffer-modified-p nil))
            (kill-buffer viewer)))))))

(provide 'gnosis-test-image)
;;; gnosis-test-image.el ends here
