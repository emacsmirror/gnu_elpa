;;; gnosis-test-model-targets.el --- Surface targets -*- lexical-binding: t; -*-
;;; Commentary:
;; Deterministic geometry, revision and ownership contracts.
;;; Code:
(require 'ert)
(require 'gnosis-model)
(require 'gnosis-model-test-support)
(require 'gnosis-export-import)

;; Declare optional renderer state for interpreted tests without loading it.
(defvar canvas-3d--process nil)
(defvar canvas-3d--selection nil)
(defvar canvas-3d--frame nil)
(defvar canvas-3d--busy nil)
(defvar canvas-3d--dirty nil)
(defvar-local canvas-3d--question-target nil)
(defvar canvas-3d-selected-id nil)
(defvar canvas-3d--yaw 0)
(defvar canvas-3d--pitch 0)
(defvar canvas-3d--zoom 1)

(ert-deftest gnosis-model-targets-topology-and-domain-policy ()
  (gnosis-test-with-db
   (let* ((file (gnosis-test-model-targets--fixture))
          (scene (gnosis-model--scene file))
          (geometry (gnosis-model--validate-targets scene (file-name-directory file)))
          (hit '(:mesh "mesh" :face 0 :point (8 4 0))))
     (should (equal (aref (cdar geometry) 1) '((0 0 0) (10 10 0) (0 10 0))))
     (should (equal (gnosis-model--candidate scene geometry "whole" hit) "whole"))
     (should (equal (gnosis-model--candidate scene geometry "tip" hit) "tip"))
     (should-not (gnosis-model--candidate scene geometry "patch" hit))
     (should (equal (gnosis-model--candidate scene geometry "patch" '(:mesh "mesh" :face 1)) "patch"))
     (should-not (gnosis-model--candidate scene geometry "tip" '(:mesh "mesh" :face 0 :point (9.01 4 0))))
     (should-not (gnosis-model--candidate scene geometry "tip" '(:mesh "other" :face 0 :point (8 4 0)))))))

(ert-deftest gnosis-model-targets-fields-pinned-and-name ()
  (gnosis-test-with-db
   (let* ((resource (gnosis-model-import (gnosis-test-model-targets--fixture)))
          (find (gnosis-model-fields "model" (list resource "0" "0" "1") '("tip")))
          (name (gnosis-model-fields "model-name" (list resource "tip" "0" "0" "1") '("Authored text"))))
     (should (eq (plist-get find :response) 'find))
     (should (eq (plist-get name :response) 'name))
     (should (equal (plist-get name :answer) "Authored text"))
     (should (equal (plist-get find :scene) (plist-get name :scene)))
     (should-error (gnosis-model-fields "model-name" (list resource "unknown" "0" "0" "1") '("Text")))
     (with-temp-file (expand-file-name "surface.obj" (file-name-directory (plist-get name :scene)))
       (insert "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n"))
     (should-error (gnosis-model-fields "model-name" (list resource "tip" "0" "0" "1") '("Text"))))))

(ert-deftest gnosis-model-targets-invalid-manifests-refused ()
  (gnosis-test-with-db
   (let* ((file (gnosis-test-model-targets--fixture))
          (original (gnosis-model--scene file))
          (dir (file-name-directory file)))
     (dolist (change '((version . 3) (targets . nil)))
       (let ((scene (copy-tree original)))
         (setf (alist-get (car change) scene) (cdr change))
         (should-error (gnosis-model--validate-targets scene dir))))
     (dolist (change '((face . 2) (barycentric . (0 0 0)) (tolerance . 0) (kind . "unknown") (mesh . "absent")))
       (let ((scene (copy-tree original)))
         (setf (alist-get (car change) (nth 1 (alist-get 'targets scene))) (cdr change))
         (should-error (gnosis-model--validate-targets scene dir)))))))

(ert-deftest gnosis-model-targets-publish-preserves-old-geometry-and-ids ()
  (gnosis-test-with-db
   (let* ((old (gnosis-model-import (gnosis-test-model-targets--fixture)))
          (file (expand-file-name old (gnosis-assets-root)))
          (scene (gnosis-model--scene file))
          (dir (file-name-directory file))
          (before (gnosis-assets-revision dir '("surface.obj"))))
     (setf (alist-get 'label (nth 1 (alist-get 'targets scene))) "Edited tip")
     (let* ((new (gnosis-model--publish-scene scene old))
            (newfile (expand-file-name new (gnosis-assets-root))))
       (should-not (equal old new))
       (should (equal before (gnosis-assets-revision (file-name-directory newfile) '("surface.obj"))))
       (should (equal "Tip" (alist-get 'label (gnosis-model-target (gnosis-model--scene file) "tip"))))
       (should (equal "Edited tip" (alist-get 'label (gnosis-model-target (gnosis-model--scene newfile) "tip"))))))))

(ert-deftest gnosis-model-targets-author-id-and-barycentric ()
  (let ((context (list :used-ids '("landmark-1" "landmark-3"))))
    (should (equal "landmark-2" (gnosis-model--author-id context)))
    (should (equal "landmark-4" (gnosis-model--author-id context))))
  (let ((bary (gnosis-model--barycentric '(8 4 0) '((0 0 0) (10 0 0) (10 10 0)))))
    (should (< (abs (- (nth 0 bary) 0.2)) 0.000001))
    (should (< (abs (- (nth 1 bary) 0.4)) 0.000001))))

(ert-deftest gnosis-model-targets-stale-selection-refused ()
  (let ((canvas-3d--process nil) (canvas-3d--selection nil) (canvas-3d--frame nil))
    (should-error (gnosis-model-selection '(:target "tip")))))

(ert-deftest gnosis-model-targets-native-edit-cancel-and-retained-ids ()
  (with-temp-buffer
    (setq-local canvas-3d--question-target nil)
    (gnosis-test-with-db
     (let* ((file (gnosis-test-model-targets--fixture))
            (scene (gnosis-model--scene file))
            (original (copy-tree scene))
            (context (list :scene scene :directory (file-name-directory file) :target "tip"))
            (gnosis-model--author-context context)
            (canvas-3d--question-target nil)
            (canvas-3d-selected-id nil))
       (cl-letf (((symbol-function 'gnosis-model--author-check) #'ignore)
                 ((symbol-function 'canvas-3d--request) #'ignore)
                 ((symbol-function 'read-string) (lambda (&rest _) "Changed"))
                 ((symbol-function 'read-number) (lambda (&rest _) 2)))
         (gnosis-model-author-edit)
         (should (equal scene original))
         (should (equal "tip" (plist-get context :target)))
         (should (= 2 (alist-get 'tolerance (gnosis-model-target (plist-get context :scene) "tip"))))
         (let ((before (copy-tree (plist-get context :scene))))
           (cl-letf (((symbol-function 'read-number) (lambda (&rest _) (signal 'quit nil))))
             (should (eq 'cancelled (condition-case nil (gnosis-model-author-edit) (quit 'cancelled)))))
           (should (equal before (plist-get context :scene))))
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (gnosis-model-author-remove))
         (should-error (gnosis-model-target (plist-get context :scene) "tip"))
         (should (equal original (gnosis-model--scene file))))))))

(ert-deftest gnosis-model-targets-native-region-and-point-move ()
  (with-temp-buffer
    (setq-local canvas-3d--question-target nil)
    (gnosis-test-with-db
     (let* ((file (gnosis-test-model-targets--fixture))
            (scene (gnosis-model--scene file))
            (context (list :scene scene :directory (file-name-directory file) :target "patch"
                           :geometry (gnosis-model--validate-targets scene (file-name-directory file))))
            (gnosis-model--author-context context)
            (canvas-3d--question-target nil)
            (canvas-3d-selected-id nil))
       (cl-letf (((symbol-function 'gnosis-model--author-hit)
                  (lambda () '(:mesh "mesh" :face 0 :point (8 4 0))))
                 ((symbol-function 'canvas-3d--request) #'ignore))
         (gnosis-model-author-region-toggle)
         (should (equal '(1 0) (alist-get 'faces (gnosis-model-target (plist-get context :scene) "patch"))))
         (gnosis-model-author-region-toggle)
         (should (equal '(1) (alist-get 'faces (gnosis-model-target (plist-get context :scene) "patch"))))
         (setf (plist-get context :target) "tip")
         (gnosis-model-author-move)
         (should (equal "tip" (plist-get context :target)))
         (should (= 1 (alist-get 'tolerance (gnosis-model-target (plist-get context :scene) "tip")))))))))

(ert-deftest gnosis-model-targets-save-alias-omission-is-distinct ()
  (let (calls)
    (cl-letf (((symbol-function 'gnosis-add-thema--assert-common) #'ignore)
              ((symbol-function 'gnosis-model-fields) #'ignore)
              ((symbol-function 'gnosis-add-thema--dispatch) (lambda (&rest args) (push args calls))))
      (gnosis-model--save "id" "model-name" "Q" '("r") '("A") "" nil 0 nil)
      (gnosis-model--save "id" "model-name" "Q" '("r") '("A") "" nil 0 nil nil)
      (gnosis-model--save "id" "model-name" "Q" '("r") '("A") "" nil 0 nil '("Alias"))
      (should (equal (car (last (nth 0 calls))) '("Alias")))
      (should (= (length (nth 1 calls)) 10))
      (should (= (length (nth 2 calls)) 9)))))

(ert-deftest gnosis-model-targets-obj-invalid-topology ()
  (let ((file (make-temp-file "gnosis-invalid-" nil ".obj")))
    (unwind-protect
        (dolist (text '("v 0 0 0\nv 1 0 0\nv 2 0 0\nf 1 2 3\n"
                        "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 4\n"
                        "v NaN 0 0\nf 1 1 1\n"))
          (with-temp-file file (insert text))
          (should-error (gnosis-model--geometry file)))
      (delete-file file))))

(ert-deftest gnosis-model-targets-nearest-point-before-id ()
  (gnosis-test-with-db
   (let* ((file (gnosis-test-model-targets--fixture))
          (scene (gnosis-model--scene file))
          (tip (copy-tree (gnosis-model-target scene "tip")))
          (near (copy-tree tip)))
     (setf (alist-get 'id tip) "a-far"
           (alist-get 'tolerance tip) 10
           (alist-get 'id near) "z-near"
           (alist-get 'barycentric near) '(0.1 0.5 0.4)
           (alist-get 'tolerance near) 10
           (alist-get 'targets scene) (list tip near))
     (let ((geometry (gnosis-model--validate-targets scene (file-name-directory file))))
       (should (equal "z-near" (gnosis-model--candidate
                                scene geometry "a-far" '(:mesh "mesh" :face 0 :point (9 4 0)))))
       (should (equal "a-far" (gnosis-model--candidate
                               scene geometry "z-near" '(:mesh "mesh" :face 0 :point (8.5 4 0)))))))))

(ert-deftest gnosis-model-targets-selection-retains-wrong-surface ()
  (gnosis-test-with-db
   (let* ((resource (gnosis-model-import (gnosis-test-model-targets--fixture)))
          (fields (gnosis-model-fields "model" (list resource "0" "0" "1") '("tip")))
          (process (make-pipe-process :name "gnosis-selection-test" :noquery t)))
     (unwind-protect
         (with-temp-buffer
           (let* ((canvas-3d--process process)
                  (canvas-3d--frame (list :owner process :seq 7))
                  (canvas-3d--busy nil) (canvas-3d--dirty nil)
                  (canvas-3d--selection (list :owner process :frame 7 :id "mesh"
                                              :mesh "mesh" :face 0 :point '(1 1 0)))
                  (before (copy-tree canvas-3d--selection))
                  (hit (gnosis-model-selection fields)))
             (should hit)
             (should-not (plist-get hit :id))
             (dolist (key '(:mesh :face :point :frame :owner))
               (should (equal (plist-get hit key) (plist-get before key))))
             (should (equal before canvas-3d--selection))
             (setq canvas-3d--selection (list :owner process :frame 7))
             (should-not (gnosis-model-selection fields))
             (setq canvas-3d--selection before canvas-3d--busy t)
             (should-error (gnosis-model-selection fields))))
       (delete-process process)))))

(ert-deftest gnosis-model-targets-import-v2-starts-with-target-id ()
  (gnosis-test-with-db
   (let* ((resource (gnosis-model-import (gnosis-test-model-targets--fixture)))
          (current-prefix-arg '(4)))
     (cl-letf (((symbol-function 'gnosis-model--read-source) (lambda (_) resource))
               ((symbol-function 'gnosis-model--read-numeric) #'list))
       (should (equal '("whole") (cadr (gnosis-model--read-fields))))))))

(ert-deftest gnosis-model-targets-object-click-accepts-distinct-target-id ()
  (gnosis-test-with-db
   (let* ((resource (gnosis-model-import (gnosis-test-model-targets--fixture)))
          (file (expand-file-name resource (gnosis-assets-root)))
          (scene (gnosis-model--scene file))
          (process (make-pipe-process :name "gnosis-author-test" :noquery t)))
     (unwind-protect
         (with-temp-buffer
           (let* ((context (list :depth 0 :target "whole" :scene scene
                                 :objects (gnosis-model--targets scene)
                                 :process process :reference resource :view '(0 0 1) :result nil))
                  (gnosis-model--author-context context)
                  (canvas-3d--process process)
                  (canvas-3d--frame (list :owner process :seq 1))
                  (canvas-3d--busy nil) (canvas-3d--dirty nil)
                  (canvas-3d-selected-id "mesh")
                  (canvas-3d--question-target nil)
                  (canvas-3d--yaw 0) (canvas-3d--pitch 0) (canvas-3d--zoom 1))
             (cl-letf (((symbol-function 'gnosis-model--author-check) #'ignore)
                       ((symbol-function 'recursion-depth) (lambda () 1))
                       ((symbol-function 'exit-recursive-edit) #'ignore))
               (gnosis-model-author-accept)
               (should (equal '("whole") (cadr (plist-get context :result)))))))
       (delete-process process)))))

(ert-deftest gnosis-model-targets-name-public-draft-save-attach ()
  (gnosis-test-with-db
   (save-window-excursion
     (let* ((resource (gnosis-model-import (gnosis-test-model-targets--fixture)))
            (fields (list (list resource "0" "0" "1") '("tip")))
            (gnosis-save-hook nil))
       (unwind-protect
           (cl-letf (((symbol-function 'gnosis-model--read-fields) (lambda (&rest _) fields)))
             (gnosis-add-model-thema "model-name")
             (insert "Name this landmark")
             (goto-char (point-max))
             (insert "\n** Accepted aliases\n- Apex\n")
             (gnosis-model-attach)
             (let ((entry (car (gnosis-export-parse-themata))))
               (should (equal "model-name" (downcase (nth 1 entry))))
               (should (equal '("Tip") (nth 4 entry)))
               (should (equal '("Apex") (nth 8 entry))))
             (call-interactively (key-binding (kbd "C-c C-c")))
             (let ((id (car (gnosis-select 'id 'themata nil t))))
               (should (equal '("Tip") (gnosis-get 'answer 'themata `(= id ,id))))
               (should (equal '("Apex") (gnosis-get 'accepted-aliases 'themata `(= id ,id))))
               (should (equal (cons resource '("tip" "0" "0" "1"))
                              (gnosis-get 'hypothesis 'themata `(= id ,id))))))
         (when (get-buffer "*Gnosis NEW*") (kill-buffer "*Gnosis NEW*")))))))

(defun gnosis-test-model-targets--refuse-drift (context fault)
  "Refuse FAULT during CONTEXT acceptance, preserving the draft for retry."
  (let* ((directory (plist-get context :directory))
         (file (expand-file-name (if (eq fault 'manifest) "scene.json" "surface.obj")
                                 directory))
         (original (with-temp-buffer
                     (insert-file-contents-literally file) (buffer-string)))
         (replacement (if (eq fault 'manifest) (concat original "\n")
                        "v 100 0 0\nv 110 0 0\nv 110 10 0\nv 100 10 0\nf -4 -3 -2 -1\n"))
         (before (copy-tree context))
         (owner (plist-get context :buffer))
         (draft (with-current-buffer owner (buffer-string)))
         (entries (directory-files (gnosis-assets-root) nil nil t))
         (copy (symbol-function 'copy-file))
         stage injected)
    (unwind-protect
        (progn
          (when (memq fault '(geometry manifest))
            (with-temp-file file (insert replacement)))
          (cl-letf (((symbol-function 'copy-file)
                     (lambda (source destination &rest args)
                       (if (and (not injected) (equal source file)
                                (memq fault '(copy after restore quit)))
                           (progn
                             (setq injected t stage (file-name-directory destination))
                             ;; Valid replacement topology, not a parser failure.
                             (when (memq fault '(copy restore))
                               (with-temp-file source (insert replacement)))
                             (apply copy source destination args)
                             (when (eq fault 'after)
                               (with-temp-file source (insert replacement)))
                             (when (eq fault 'restore)
                               (with-temp-file source (insert original)))
                             (when (eq fault 'quit) (signal 'quit nil)))
                         (apply copy source destination args)))))
            (if (eq fault 'quit)
                (should (eq 'cancelled
                            (condition-case nil
                                (call-interactively #'gnosis-model-author-accept)
                              (quit 'cancelled))))
              (should-error (call-interactively #'gnosis-model-author-accept)
                            :type 'user-error)))
          (should (equal before context))
          (should (equal draft (with-current-buffer owner (buffer-string))))
          (should (equal (sort entries #'string<)
                         (sort (directory-files (gnosis-assets-root) nil nil t) #'string<)))
          (when (memq fault '(copy after restore quit))
            (should injected)
            (should-not (file-exists-p stage))))
      (with-temp-file file (insert original)))))

(defun gnosis-test-model-targets--author-roundtrip (command type &optional changed-key fault)
  "Exercise COMMAND through acceptance, native TYPE saving and reopening.
When CHANGED-KEY is non-nil, start with an explicit nil change flag.
If FAULT is non-nil, refuse resource drift first and retry after restoration."

  (gnosis-test-with-db
   (save-window-excursion
    (let* ((resource (gnosis-model-import (gnosis-test-model-targets--fixture)))
           (file (expand-file-name resource (gnosis-assets-root)))
           (scene (gnosis-model--scene file))
           (directory (file-name-directory file))
           (revision (gnosis-assets-revision directory '("scene.json" "surface.obj")))
           (process (make-pipe-process :name "gnosis-author-roundtrip" :noquery t))
           ;; Like the visual reader, retain the same context outside the viewer.
           ;; In particular, the initial context has no :changed property.
           (context (list :buffer (current-buffer) :mode major-mode
                          :tick (buffer-chars-modified-tick) :database gnosis-db
                          :depth 0 :scene scene :directory directory
                          :geometry (gnosis-model--validate-targets scene directory)
                          :serial 0 :used-ids '("whole" "tip" "patch")
                          :objects (gnosis-model--targets scene) :target "tip"
                          :process process :reference resource :view '(0 0 1)
                          :result nil :cancelled nil))
           (gnosis-save-hook nil)
           expected)
      (when changed-key (setq context (plist-put context :changed nil)))
      (unwind-protect
          (progn
            (with-temp-buffer
              (let ((gnosis-model--author-context context)
                    (canvas-3d--process process)
                    (canvas-3d--frame (list :owner process :seq 1))
                    (canvas-3d--selection (list :owner process :frame 1 :mesh "mesh"
                                               :face 0 :point '(9 4 0)))
                    (canvas-3d--busy nil) (canvas-3d--dirty nil)
                    (canvas-3d-selected-id nil)
                    (canvas-3d--yaw 0) (canvas-3d--pitch 0) (canvas-3d--zoom 1))
                (setq-local canvas-3d--question-target (gnosis-model-target scene "tip"))
                (cl-letf (((symbol-function 'canvas-3d--request) #'ignore)
                          ((symbol-function 'read-string) (lambda (&rest _) "New label"))
                          ((symbol-function 'read-number) (lambda (&rest _) 2))
                          ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                          ((symbol-function 'recursion-depth) (lambda () 1))
                          ((symbol-function 'exit-recursive-edit) #'ignore))
                  (when (eq command 'gnosis-model-author-region-toggle)
                    (setf (plist-get context :target) "patch"))
                  (when command (call-interactively command))
                  (when (eq command 'gnosis-model-author-remove)
                    (cl-letf (((symbol-function 'completing-read)
                               (lambda (&rest _) "Whole (whole)")))
                      (call-interactively #'gnosis-model-author-target)))
                  (setq expected (copy-tree (plist-get context :scene)))
                  (when fault (gnosis-test-model-targets--refuse-drift context fault))
                  (call-interactively #'gnosis-model-author-accept))))
            (let* ((fields (plist-get context :result))
                   (new (caar fields))
                   (published (gnosis-model--scene
                               (expand-file-name new (gnosis-assets-root)))))
              (should (equal (not (equal new resource)) (and command t)))
              (should (equal (gnosis-model--targets expected)
                             (gnosis-model--targets published)))
              (should (equal revision (gnosis-assets-revision
                                       directory '("scene.json" "surface.obj"))))
              (cl-letf (((symbol-function 'gnosis-model--read-fields)
                         (lambda (&rest _) fields)))
                (gnosis-add-model-thema type))
              (insert "Identify the target")
              (call-interactively (key-binding (kbd "C-c C-c")))
              (let ((id (car (gnosis-select 'id 'themata nil t))))
                (gnosis-sqlite-close gnosis-db)
                (setq gnosis-db (gnosis-db--open gnosis-dir))
                (gnosis-edit-thema id)
                (let* ((entry (car (gnosis-export-parse-themata)))
                       (reopened (gnosis-model-fields type (nth 3 entry) (nth 4 entry))))
                  (should (equal new (plist-get reopened :resource)))
                  (should (equal (caadr fields) (plist-get reopened :target)))
                  (should (equal (gnosis-model--targets expected)
                                 (gnosis-model--targets
                                  (gnosis-model--scene (plist-get reopened :scene)))))))))
        (delete-process process)
        (dolist (name '("*Gnosis NEW*" "*Gnosis Edit*"))
          (when (get-buffer name) (kill-buffer name))))))))

(ert-deftest gnosis-model-targets-author-point-accept-save-reopen ()
  (dolist (type '("model" "model-name"))
    (gnosis-test-model-targets--author-roundtrip #'gnosis-model-author-point type)))

(ert-deftest gnosis-model-targets-author-region-accept-save-reopen ()
  (dolist (type '("model" "model-name"))
    (gnosis-test-model-targets--author-roundtrip #'gnosis-model-author-region type)))

(ert-deftest gnosis-model-targets-author-edit-accept-save-reopen ()
  (gnosis-test-model-targets--author-roundtrip #'gnosis-model-author-edit "model-name"))

(ert-deftest gnosis-model-targets-author-move-accept-save-reopen ()
  (gnosis-test-model-targets--author-roundtrip #'gnosis-model-author-move "model"))

(ert-deftest gnosis-model-targets-author-toggle-accept-save-reopen ()
  (gnosis-test-model-targets--author-roundtrip #'gnosis-model-author-region-toggle "model"))

(ert-deftest gnosis-model-targets-author-remove-accept-save-reopen ()
  (gnosis-test-model-targets--author-roundtrip #'gnosis-model-author-remove "model"))

(ert-deftest gnosis-model-targets-author-unchanged-accept-save-reopen ()
  (gnosis-test-model-targets--author-roundtrip nil "model"))

(ert-deftest gnosis-model-targets-author-nil-flag-accept-save-reopen ()
  (dolist (command '(gnosis-model-author-point gnosis-model-author-region
                     gnosis-model-author-edit gnosis-model-author-move
                     gnosis-model-author-region-toggle gnosis-model-author-remove))
    (gnosis-test-model-targets--author-roundtrip command "model" t)))

(ert-deftest gnosis-model-targets-author-drift-label ()
  (dolist (fault '(geometry manifest))
    (gnosis-test-model-targets--author-roundtrip
     #'gnosis-model-author-edit "model-name" nil fault)))

(ert-deftest gnosis-model-targets-author-drift-point ()
  (gnosis-test-model-targets--author-roundtrip
   #'gnosis-model-author-point "model" nil 'geometry))

(ert-deftest gnosis-model-targets-author-drift-region ()
  (gnosis-test-model-targets--author-roundtrip
   #'gnosis-model-author-region "model" nil 'geometry))

(ert-deftest gnosis-model-targets-author-drift-unchanged ()
  (gnosis-test-model-targets--author-roundtrip nil "model" nil 'geometry))

(ert-deftest gnosis-model-targets-author-drift-during-copy ()
  (gnosis-test-model-targets--author-roundtrip
   #'gnosis-model-author-edit "model" nil 'copy))

(ert-deftest gnosis-model-targets-author-drift-after-copy ()
  (gnosis-test-model-targets--author-roundtrip
   #'gnosis-model-author-edit "model" nil 'after))

(ert-deftest gnosis-model-targets-author-drift-copied-bytes ()
  ;; Restoring the source before copy-file returns defeats source-only checks.
  (gnosis-test-model-targets--author-roundtrip
   #'gnosis-model-author-edit "model" nil 'restore))

(ert-deftest gnosis-model-targets-author-drift-copy-quit ()
  (gnosis-test-model-targets--author-roundtrip
   #'gnosis-model-author-edit "model" nil 'quit))

(provide 'gnosis-test-model-targets)
;;; gnosis-test-model-targets.el ends here
