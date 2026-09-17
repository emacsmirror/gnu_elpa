;;; gnosis-model-test-support.el --- Shared model fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Shared fixtures and assertions; loading this library registers no tests.

;;; Code:
(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-model)
(require 'gnosis-review)
(require 'gnosis-export-import)

;; The optional renderer is loaded only when the canvas fixture is used.
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

(defun gnosis-test-model--scene ()
  "Create a tiny licensed source scene in the current disposable directory."
  (let ((dir (expand-file-name "source" gnosis-dir)))
    (make-directory dir t)
    (with-temp-file (expand-file-name "triangle.obj" dir)
      (insert "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n"))
    (with-temp-file (expand-file-name "scene.json" dir)
      (insert "{\"objects\":[{\"id\":\"triangle\",\"label\":\"Triangle\",\"path\":\"triangle.obj\"},{\"id\":\"other\",\"label\":\"Other triangle\",\"path\":\"triangle.obj\"}],\"initial_view\":[0,-90,1],\"license\":\"CC0; original test geometry\",\"source\":\"Gnosis ERT fixture\"}"))
    (expand-file-name "scene.json" dir)))

(defun gnosis-test-model--add ()
  "Create and return a model thema ID in the disposable environment."
  (let* ((reference (gnosis-model-import (gnosis-test-model--scene)))
         (id (gnosis-generate-id)))
    (gnosis-add-thema-fields "model" "Select triangle" (list reference "0" "-90" "1")
                            '("triangle") "Original geometry" '("test") 0 nil nil id)
    id))

(defun gnosis-test-model--canvas (_path _view &optional _size inline _question-target _verified)
  "Create a deterministic stand-in for the optional canvas boundary.
Use the current buffer for INLINE, otherwise a separate special buffer."
  (let ((load-path (cons (gnosis-model--renderer-directory) load-path)))
    (require 'canvas-3d))
  (let ((buffer (if inline (current-buffer) (generate-new-buffer " *Gnosis test canvas*"))))
    (with-current-buffer buffer
      (unless inline (special-mode))
      (setq-local canvas-3d--process (make-pipe-process :name "gnosis-model-test" :noquery t))
      (setq-local canvas-3d--image (list 'image :type 'canvas))
      (setq-local canvas-3d--frame (list :seq 1 :owner canvas-3d--process))
      (setq-local canvas-3d--status "Ready")
      (setq-local canvas-3d--busy nil)
      (setq-local canvas-3d--dirty nil)
      (setq-local canvas-3d--yaw 0)
      (setq-local canvas-3d--pitch -90)
      (setq-local canvas-3d--zoom 1)
      (setq-local canvas-3d-selection-hook nil)
      (add-hook 'kill-buffer-hook
                (lambda () (when (process-live-p canvas-3d--process)
                             (delete-process canvas-3d--process))) nil t))
    (pop-to-buffer buffer)
    buffer))

(defun gnosis-test-model--review-pick (mesh &optional owner)
  "Deliver a renderer surface hit for MESH belonging to OWNER."
  (setq-local canvas-3d--selection
              (list :id mesh :mesh mesh :face 0 :point '(0 0 0)
                    :frame 1 :owner (or owner canvas-3d--process)))
  (gnosis-review--model-selection canvas-3d--selection))

(defun gnosis-test-model--wait-for-preparation ()
  "Wait for the real owned child in a mocked batch recursive input loop."
  ;; Batch callers create an encounter without the public window display.
  ;; Supply that display too: hidden encounters now defer attachment.
  (set-window-buffer (selected-window) (current-buffer))
  (let ((deadline (+ (float-time) 15)))
    (while (and gnosis-review--model-context
                (not (plist-get gnosis-review--model-context :fields))
                (not (plist-get gnosis-review--model-context :error))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (unless (plist-get gnosis-review--model-context :fields)
      (error "%s" (or (plist-get gnosis-review--model-context :error)
                      "Model preparation timed out")))))

(defmacro gnosis-test-model--encounter (&rest input)
  "Run INPUT at the real model encounter's recursive input boundary."
  (declare (indent 0) (debug t))
  `(let ((depth 0))
     (cl-letf (((symbol-function 'gnosis-model-open) #'gnosis-test-model--canvas)
               ((symbol-function 'gnosis-model--canvas-size) (lambda () 400))
               ((symbol-function 'recursion-depth) (lambda () depth))
               ((symbol-function 'exit-recursive-edit) #'ignore)
               ((symbol-function 'recursive-edit)
                (lambda ()
                  (setq depth 1)
                  (gnosis-test-model--wait-for-preparation)
                  (progn
                    (setq-local canvas-3d-selected-id "triangle")
                    (gnosis-test-model--review-pick "triangle" canvas-3d--process)
                    ,@input))))
       (gnosis-review--display-thema model))))

(defun gnosis-test-model-targets--fixture ()
  "Create original quad geometry and return a versioned scene filename."
  (let ((dir (expand-file-name "surface-source" gnosis-dir)))
    (make-directory dir t)
    (with-temp-file (expand-file-name "surface.obj" dir)
      (insert "v 0 0 0\nv 10 0 0\nv 10 10 0\nv 0 10 0\nf -4 -3 -2 -1\n"))
    (with-temp-file (expand-file-name "scene.json" dir)
      (insert "{\"version\":2,\"objects\":[{\"id\":\"mesh\",\"label\":\"Surface\",\"path\":\"surface.obj\"}],\"targets\":[{\"id\":\"whole\",\"label\":\"Whole\",\"mesh\":\"mesh\",\"kind\":\"object\"},{\"id\":\"tip\",\"label\":\"Tip\",\"mesh\":\"mesh\",\"kind\":\"point\",\"face\":0,\"barycentric\":[0.2,0.4,0.4],\"tolerance\":1},{\"id\":\"patch\",\"label\":\"Patch\",\"mesh\":\"mesh\",\"kind\":\"region\",\"faces\":[1]}],\"initial_view\":[0,0,1],\"license\":\"CC0\",\"source\":\"Original test fixture\"}"))
    (expand-file-name "scene.json" dir)))

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

(provide 'gnosis-model-test-support)
;;; gnosis-model-test-support.el ends here
