;;; gnosis-model-test-support.el --- Shared model fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Shared fixtures and assertions; loading this library registers no tests.

;;; Code:
(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-model)
(require 'gnosis-review)

;; The optional renderer is loaded only when the canvas fixture is used.
(defvar canvas-3d--process)
(defvar canvas-3d--selection)

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

(provide 'gnosis-model-test-support)
;;; gnosis-model-test-support.el ends here
