;;; canvas-3d-pick-tests.el --- Compact picking tests -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Thanos Apollo
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Exercise compact packets and deferred, frame-qualified selection delivery.

;;; Code:
(require 'ert)
(require 'canvas-3d)

(ert-deftest canvas-3d-compact-frame-and-deferred-pick ()
  (with-temp-buffer
    (let* ((owner (current-buffer))
           (process (make-pipe-process :name "pick-test" :buffer owner :noquery t))
           callbacks sent events)
      (unwind-protect
          (cl-letf (((symbol-function 'canvas-3d--paint) #'ignore)
                    ((symbol-function 'process-send-string)
                     (lambda (_process text) (push text sent)))
                    ((symbol-function 'run-at-time)
                     (lambda (_time _repeat callback &rest args)
                       (push (lambda () (apply callback args)) callbacks))))
            (setq canvas-3d--process process canvas-3d--protocol 3
                  canvas-3d--size 1 canvas-3d--seq 1 canvas-3d--busy t
                  canvas-3d--objects '(((id . "mesh") (label . "Secret")))
                  canvas-3d-selection-hook (list (lambda (hit) (push hit events))))
            (let ((packet (concat "C3D3" (unibyte-string 0 0 0 1 4 3 2 255))))
              (dotimes (i (length packet))
                (canvas-3d--receive process (substring packet i (1+ i)))))
            (should (equal (plist-get canvas-3d--frame :color) (unibyte-string 4 3 2 255)))
            (should-not (plist-get canvas-3d--frame :ids))
            (canvas-3d-pick 0 0)
            (should (eq canvas-3d--busy 'pick))
            (should (string-match-p "\"frame\":1" (car sent)))
            (let ((packet (concat "C3P3" (unibyte-string
                                         0 0 0 2 0 0 0 1 0 0 0 1 0 0 0 3
                                         63 128 0 0 64 0 0 0 64 64 0 0))))
              (canvas-3d--receive process (substring packet 0 7))
              (canvas-3d--receive process (substring packet 7)))
            (should-not canvas-3d--selection)
            (should-not events)
            ;; An already queued timeout must not retire a completed reply.
            (funcall (cadr callbacks))
            (should (eq canvas-3d--busy 'pick-ready))
            (funcall (car callbacks))
            (should (equal (plist-get canvas-3d--selection :point) '(1.0 2.0 3.0)))
            (should (= (plist-get canvas-3d--selection :face) 2))
            (should (equal events (list canvas-3d--selection)))
            (should (eq canvas-3d--busy t)))
        (canvas-3d--stop)))))
(ert-deftest canvas-3d-compact-pick-overlap-and-retirement ()
  (dolist (action '(rotate replace cancel stale-frame retired-process timeout))
    (with-temp-buffer
      (let* ((process (make-pipe-process :name "pick-owner" :buffer (current-buffer) :noquery t))
             (frame (list :owner process :seq 1 :color (unibyte-string 0 0 0 255)))
             (packet (concat "C3P3" (unibyte-string
                                    0 0 0 2 0 0 0 1 0 0 0 1 0 0 0 3
                                    63 128 0 0 64 0 0 0 64 64 0 0)))
             callbacks hooks sent)
        (unwind-protect
            (cl-letf (((symbol-function 'process-send-string)
                       (lambda (_process text) (push text sent)))
                      ((symbol-function 'run-at-time)
                       (lambda (_time _repeat callback &rest args)
                         (push (lambda () (apply callback args)) callbacks))))
              (setq canvas-3d--process process canvas-3d--protocol 3
                    canvas-3d--size 1 canvas-3d--seq 1 canvas-3d--frame frame
                    canvas-3d--objects '(((id . "mesh") (label . "Secret")))
                    canvas-3d-selection-hook (list (lambda (hit) (push hit hooks))))
              (canvas-3d-pick 0 0)
              (should-error (canvas-3d-pick 0 0) :type 'user-error)
              (if (eq action 'timeout)
                  (funcall (car callbacks))
                (canvas-3d--receive process packet)
                (pcase action
                  ('rotate (canvas-3d-right) (canvas-3d-right)
                           (should canvas-3d--dirty))
                  ('replace (setq canvas-3d--frame (copy-tree frame)))
                  ('stale-frame (setf (plist-get frame :seq) 99))
                  ('cancel (canvas-3d-cancel))
                  ('retired-process
                   (canvas-3d-cancel)
                   (setq canvas-3d--process
                         (make-pipe-process :name "pick-successor" :buffer (current-buffer) :noquery t)
                         canvas-3d--seq 2 canvas-3d--busy 'pick-ready)))
                ;; Simulate an already queued callback even after cancellation.
                (funcall (car callbacks)))
              (should-not hooks)
              (should-not canvas-3d--selection)
              (when (eq action 'rotate)
                (should (= canvas-3d--seq 3))
                (should (string-match-p "\"yaw\":20" (car sent)))))
          (canvas-3d--stop)
          (when (process-live-p process) (delete-process process)))))))

(ert-deftest canvas-3d-compact-pick-all-splits-and-invalid-packets ()
  (let ((packet (concat "C3P3" (unibyte-string
                              0 0 0 2 0 0 0 1 0 0 0 1 0 0 0 3
                              63 128 0 0 64 0 0 0 64 64 0 0))))
    (dotimes (split 33)
      (with-temp-buffer
        (let ((process (make-pipe-process :name "pick-split" :buffer (current-buffer) :noquery t)))
          (unwind-protect
              (cl-letf (((symbol-function 'run-at-time) #'ignore))
                (setq canvas-3d--process process canvas-3d--protocol 3
                      canvas-3d--busy 'pick canvas-3d--seq 2
                      canvas-3d--objects '(((id . "mesh"))))
                (canvas-3d--receive process (substring packet 0 split))
                (when (< split 32)
                  (canvas-3d--receive process (substring packet split)))
                (should (eq canvas-3d--busy 'pick-ready))
                (should-not canvas-3d--selection)
                (canvas-3d--receive process packet)
                (should-not canvas-3d--process))
            (canvas-3d--stop)))))))

(ert-deftest canvas-3d-compact-pick-malformed-replies ()
  (let ((valid (concat "C3P3" (unibyte-string
                             0 0 0 2 0 0 0 1 0 0 0 1 0 0 0 3
                             63 128 0 0 64 0 0 0 64 64 0 0))))
    ;; Wrong magic/sequence/index, missing face, nonfinite coordinates, excess.
    (dolist (change '((0 . 88) (7 . 3) (15 . 2) (19 . 0)
                      (20 . 127) (32 . 0)))
      (with-temp-buffer
        (let ((packet (copy-sequence valid))
              (process (make-pipe-process :name "pick-invalid"
                                          :buffer (current-buffer) :noquery t)))
          (unwind-protect
              (progn
                (if (= (car change) 32)
                    (setq packet (concat packet (unibyte-string 0)))
                  (aset packet (car change) (cdr change)))
                (setq canvas-3d--process process canvas-3d--protocol 3
                      canvas-3d--busy 'pick canvas-3d--seq 2
                      canvas-3d--objects '(((id . "mesh"))))
                (canvas-3d--receive process packet)
                (should-not canvas-3d--process)
                (should-not canvas-3d--selection))
            (canvas-3d--stop)))))))

(provide 'canvas-3d-pick-tests)
;;; canvas-3d-pick-tests.el ends here
