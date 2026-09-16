;;; canvas-3d-model-tests.el --- Model picking precision tests -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Free Software Foundation, Inc.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Exercise real EGL, compact wire decoding and Gnosis candidate resolution.
;; Add the core Lisp directory and its dependencies to load-path for this suite.
;; Only native image display is stubbed; no learner database is opened.

;;; Code:
(require 'ert)
(require 'gnosis-model)
(require 'canvas-3d-geometry-tests)

(ert-deftest canvas-3d-model-translated-highlight-is-pickable ()
  (dolist (offset '(0.0 999999.01))
    (let* ((directory (make-temp-file "canvas-model-precision-" t))
           (path (expand-file-name "scene.json" directory))
           (target '((id . "point") (label . "Point") (kind . "point")
                     (mesh . "model") (face . 0) (barycentric . (0.25 0.25 0.5))
                     (tolerance . 0.003))))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "mesh.obj" directory)
              (insert (format "v %.17g %.17g 0\nv %.17g %.17g 0\nv %.17g %.17g 0\nf 1 2 3\n"
                              (- offset 0.01) (- offset 0.01)
                              (+ offset 0.01) (- offset 0.01)
                              offset (+ offset 0.01))))
            (with-temp-file path
              (insert (json-encode
                       `((version . 2) (initial_view . [0 0 1])
                         (objects . [((id . "model") (label . "Model") (path . "mesh.obj"))])
                         (targets . [,target])))))
            (let* ((scene (gnosis-model--scene path "GPL-3.0-or-later" "Synthetic fixture"))
                   (geometry (gnosis-model--validate-targets scene directory)))
              (with-temp-buffer
                (unwind-protect
                    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                              ((symbol-function 'image-type-available-p) (lambda (_) t))
                              ((symbol-function 'canvas-refresh) #'ignore))
                      (canvas-3d-attach path nil '(0 0 1) 128)
                      (canvas-3d-geometry-test--settle)
                      (let ((base (canvas-3d-geometry-test--color)))
                        (setq canvas-3d--question-target (gnosis-model-target scene "point"))
                        (canvas-3d--request)
                        (canvas-3d-geometry-test--settle)
                        (let ((marked (canvas-3d-geometry-test--color)))
                          (dolist (sample '((64 64 "point") (80 80 nil)))
                            (let* ((x (nth 0 sample)) (y (nth 1 sample))
                                   (expected (nth 2 sample)) (pixel (* 4 (+ x (* y 128)))))
                              (should (eq (not (equal (substring base pixel (+ pixel 4))
                                                     (substring marked pixel (+ pixel 4))))
                                          (and expected t)))
                              (canvas-3d-pick x y)
                              (canvas-3d-geometry-test--settle)
                              (should (equal (plist-get canvas-3d--selection :mesh) "model"))
                              (should (= (plist-get canvas-3d--selection :face) 0))
                              (should (equal (gnosis-model--candidate
                                              scene geometry "point" canvas-3d--selection)
                                             expected)))))))
                  (canvas-3d-detach)))))
        (delete-directory directory t)))))

(provide 'canvas-3d-model-tests)
;;; canvas-3d-model-tests.el ends here
