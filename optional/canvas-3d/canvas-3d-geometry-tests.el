;;; canvas-3d-geometry-tests.el --- Geometry transport tests -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Thanos Apollo
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Real EGL plus small hostile packet and owner fixtures; no native GUI.

;;; Code:
(require 'ert)
(require 'canvas-3d)

(defun canvas-3d-geometry-test--packet (seq)
  "Return a one-pixel C3D2 packet with sequence SEQ and face 70000."
  (concat "C3D2" (unibyte-string 0 0 0 seq)
          (unibyte-string 1 2 3 255 1 0 1 17 113
                          63 128 0 0 192 32 0 0 0 0 0 1)))

(ert-deftest canvas-3d-geometry-splits-and-float-domain ()
  (let ((packet (canvas-3d-geometry-test--packet 1)))
    (dotimes (split (1+ (length packet)))
      (with-temp-buffer
        (canvas-3d-mode)
        (setq canvas-3d--protocol 2 canvas-3d--size 1
              canvas-3d--seq 1 canvas-3d--busy t
              canvas-3d--objects '(((id . "mesh"))))
        (setq canvas-3d--process
              (make-pipe-process :name "geometry-split" :buffer (current-buffer) :noquery t))
        (cl-letf (((symbol-function 'canvas-3d--paint) #'ignore)
                  ((symbol-function 'canvas-3d--request) #'ignore))
          (canvas-3d--receive canvas-3d--process (substring packet 0 split))
          (canvas-3d--receive canvas-3d--process (substring packet split))
          (should (equal (canvas-3d-pick 0 0) "mesh"))
          (should (= (plist-get canvas-3d--selection :face) 70000))
          (should (equal (plist-get canvas-3d--selection :point)
                         (list 1.0 -2.5 (expt 2.0 -149))))
          (should (eq (plist-get canvas-3d--selection :owner) canvas-3d--process))
          (should (= (plist-get canvas-3d--selection :frame) 1))))))
  (dolist (bits '((127 128 0 0) (255 128 0 0) (127 192 0 0)))
    (should-error (canvas-3d--float32 (apply #'unibyte-string bits) 0))))

(ert-deftest canvas-3d-geometry-redraw-same-mesh-race-and-owner ()
  (with-temp-buffer
    (canvas-3d-mode)
    (setq canvas-3d--protocol 2 canvas-3d--size 1
          canvas-3d--objects '(((id . "mesh"))))
    (setq canvas-3d--process
          (make-pipe-process :name "geometry-owner" :buffer (current-buffer) :noquery t))
    (cl-letf (((symbol-function 'canvas-3d--paint) #'ignore))
      (canvas-3d--request)
      (canvas-3d--receive canvas-3d--process (canvas-3d-geometry-test--packet 1))
      (canvas-3d-pick 0 0)
      (let ((selected (copy-tree canvas-3d--selection))
            (old-frame canvas-3d--frame))
        (canvas-3d--receive canvas-3d--process (canvas-3d-geometry-test--packet 2))
        (should (= (plist-get canvas-3d--selection :frame) 2))
        (should (= (plist-get selected :frame) 1))
        (should (equal (plist-get selected :point) (plist-get canvas-3d--selection :point)))
        (should-error (canvas-3d-pick 0 0 old-frame) :type 'user-error))
      (canvas-3d-right)
      ;; Another hit on the same mesh changes geometry, not just the mesh ID.
      (setq canvas-3d--selection (plist-put (copy-tree canvas-3d--selection) :face 3))
      (canvas-3d--request)
      (let ((old-frame canvas-3d--frame))
        (canvas-3d--receive canvas-3d--process (canvas-3d-geometry-test--packet 3))
        (should (eq canvas-3d--frame old-frame)))
      (canvas-3d--receive canvas-3d--process (canvas-3d-geometry-test--packet 4))
      (should (= (plist-get canvas-3d--selection :frame) 4))
      (should (= (plist-get canvas-3d--selection :face) 3))
      ;; Foreign selection ownership may never acquire this process's frame.
      (setq canvas-3d--selection (plist-put (copy-tree canvas-3d--selection) :owner 'retired))
      (canvas-3d-right)
      (canvas-3d--receive canvas-3d--process (canvas-3d-geometry-test--packet 5))
      (should (= (plist-get canvas-3d--selection :frame) 4))
      (canvas-3d-cancel)
      (should-not canvas-3d--selection))))

(defun canvas-3d-geometry-test--settle ()
  "Wait boundedly for this viewer's real renderer to settle."
  (let ((deadline (+ (float-time) 15)))
    (while (and canvas-3d--busy (< (float-time) deadline))
      (accept-process-output canvas-3d--process 0.05)))
  (should-not canvas-3d--busy)
  (should (equal canvas-3d--status "Ready")))

(ert-deftest canvas-3d-geometry-real-locked-question-after-open ()
  ;; Opening, EGL, requests and picking are real; only native display is stubbed.
  (dolist (target '(((mesh . "model") (kind . "point") (face . 0)
                    (barycentric . (0.3 0.3 0.4)) (tolerance . 0.5))
                   ((mesh . "model") (kind . "region") (faces . (0 1)))))
    (with-temp-buffer
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'image-type-available-p) (lambda (_) t))
                ((symbol-function 'canvas-refresh) #'ignore))
        (canvas-3d-attach (expand-file-name "fixtures/pyramid.obj" canvas-3d--directory)
                          "SECRET LABEL" '(0 0 1) 128)
        (should (= canvas-3d--protocol 2))
        ;; Domain sets its target after open, while frame one may be in flight.
        (setq canvas-3d--question-target (copy-tree target))
        (canvas-3d--request)
        (canvas-3d-geometry-test--settle)
        (should (= (plist-get canvas-3d--frame :seq) 2))
        (let* ((original (plist-get canvas-3d--frame :color))
               (pixel (seq-position (plist-get canvas-3d--frame :ids) 1))
               (seq canvas-3d--seq)
               events)
          (add-hook 'canvas-3d-selection-hook (lambda (hit) (push hit events)) nil t)
          (canvas-3d-pick (% pixel 128) (/ pixel 128))
          (should (equal canvas-3d-selected-id "model"))
          (should (equal (plist-get canvas-3d--selection :mesh) "model"))
          (should (integerp (plist-get canvas-3d--selection :face)))
          (should (= (length (plist-get canvas-3d--selection :point)) 3))
          (should (= (length events) 1))
          (should (= canvas-3d--seq (1+ seq)))
          (canvas-3d-geometry-test--settle)
          (should (equal original (plist-get canvas-3d--frame :color)))
          (should (equal target canvas-3d--question-target))
          (canvas-3d-reveal)
          (should-not (string-match-p "SECRET" (canvas-3d--header)))
          (canvas-3d-right)
          (canvas-3d-geometry-test--settle)
          (should-not (equal original (plist-get canvas-3d--frame :color)))
          (canvas-3d-zoom-in)
          (canvas-3d-geometry-test--settle)
          (canvas-3d-reset)
          (canvas-3d-geometry-test--settle)
          (should (equal original (plist-get canvas-3d--frame :color)))
          (should (equal target canvas-3d--question-target)))
        (canvas-3d-detach)
        (should-not canvas-3d--question-target)))))

(ert-deftest canvas-3d-geometry-request-has-no-labels ()
  (with-temp-buffer
    (canvas-3d-mode)
    (setq canvas-3d--process
          (make-pipe-process :name "geometry-wire" :buffer (current-buffer) :noquery t))
    (let (payload)
      (cl-letf (((symbol-function 'process-send-string)
                 (lambda (_process text) (setq payload text))))
        (canvas-3d--request)
        (should (eq :null (alist-get 'highlight (json-parse-string payload :object-type 'alist))))
        (setq canvas-3d--question-target
              '((id . "SECRET-ID") (label . "SECRET-LABEL")
                (kind . "region") (mesh . "mesh") (faces . (70000))))
        (canvas-3d--request)
        (should canvas-3d--dirty)
        (setq canvas-3d--busy nil)
        (when (timerp canvas-3d--timer) (cancel-timer canvas-3d--timer))
        (canvas-3d--request)
        (should-not (string-match-p "SECRET" payload))
        (let ((geometry (alist-get 'highlight (json-parse-string payload :object-type 'alist))))
          (should (equal (alist-get 'faces geometry) [70000]))
          (should (equal (alist-get 'mesh geometry) "mesh")))))))

(provide 'canvas-3d-geometry-tests)
;;; canvas-3d-geometry-tests.el ends here
