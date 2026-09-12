;;; canvas-3d-pick-tests.el --- Compact picking tests -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Free Software Foundation, Inc.
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

(defun canvas-3d-test--publish (seq &optional bytes)
  "Publish a test frame SEQ with optional raw BYTES."
  (let ((coding-system-for-write 'binary)
        (file (expand-file-name (format "pending-%d.bgra" seq) canvas-3d--frame-directory)))
    (write-region (or bytes (make-string (* 4 canvas-3d--size canvas-3d--size) 42))
                  nil file nil 'silent)
    (set-file-modes file #o600))
  (concat "C3D4" (unibyte-string 0 0 0 seq) canvas-3d--file-identity))

(ert-deftest canvas-3d-file-full-resolution-coalescing-and-retirement ()
  (dolist (retire '(cancel detach timeout death mode kill))
    (let ((buffer (generate-new-buffer " *file test*")) directory process image sent callbacks)
      (unwind-protect
          (with-current-buffer buffer
            (canvas-3d-mode)
            (setq canvas-3d--protocol 4 canvas-3d--size 128
                  canvas-3d--frame-directory (make-temp-file "canvas-file-test-" t)
                  directory canvas-3d--frame-directory
                  canvas-3d--file-identity "0123456789abcdef"
                  canvas-3d--process (make-process :name "file-test" :buffer buffer :noquery t
                                                      :command '("sleep" "60") :sentinel #'ignore)
                  process canvas-3d--process
                  canvas-3d--image (list 'image :type 'canvas :data "initial")
                  image canvas-3d--image)
            (cl-letf (((symbol-function 'canvas-refresh)
                       (lambda (spec &rest _)
                         (should-not (plist-member (cdr spec) :data))
                         (should (file-readable-p (plist-get (cdr spec) :file)))))
                      ((symbol-function 'process-send-string)
                       (lambda (_ text) (push (json-parse-string text :object-type 'alist) sent)))
                      ((symbol-function 'run-at-time)
                       (lambda (_time _repeat callback &rest args)
                         (push (lambda () (apply callback args)) callbacks))))
              (canvas-3d-right)
              (dotimes (_ 8) (canvas-3d-right))
              (should (= (length sent) 1))
              (should (= (alist-get 'size (car sent)) 128))
              (let ((packet (canvas-3d-test--publish 1)))
                (dotimes (i 24) (canvas-3d--receive process (substring packet i (1+ i)))))
              (should (= (length sent) 2))
              (should (= (alist-get 'yaw (car sent)) 90))
              (let ((old canvas-3d--current-file))
                (canvas-3d--receive process (canvas-3d-test--publish 2))
                (should-not (file-exists-p old)))
              (should (equal (directory-files directory nil "^[^.]") '("frame-2.bgra")))
              (should (= (plist-get (cdr image) :data-width) 128))
              (should (= (plist-get (cdr image) :scale) 1))
              (should-not canvas-3d--busy)
              (canvas-3d-right)
              ;; A partially written successor must not destroy the last image.
              (canvas-3d-test--publish 3 "partial")
              (pcase retire
                ('cancel (canvas-3d-cancel))
                ('detach (canvas-3d-detach))
                ('timeout (funcall (car callbacks)))
                ('death (delete-process process) (canvas-3d--sentinel process "finished"))
                ('mode (fundamental-mode))
                ('kill (kill-buffer buffer)))
              (should-not (file-exists-p directory))
              (should-not (process-live-p process))
              (should-not (plist-member (cdr image) :file))
              (should (equal (plist-get (cdr image) :data) (make-string (* 4 128 128) 42)))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (canvas-3d--receive process (concat "C3D4" (make-string 20 0)))
                (should-not canvas-3d--frame)
                (should-not canvas-3d--frame-directory)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (when (process-live-p process) (delete-process process))
        (when (and directory (file-exists-p directory)) (delete-directory directory t))))))

(ert-deftest canvas-3d-file-rejects-invalid-publication-before-paint ()
  (dolist (failure '(missing short oversized symlink permissions identity sequence cap paint))
    (with-temp-buffer
      (let ((process (make-pipe-process :name "bad-file" :buffer (current-buffer) :noquery t))
            painted)
        (unwind-protect
            (progn
              (setq canvas-3d--protocol 4 canvas-3d--size 1 canvas-3d--seq 1
                    canvas-3d--busy t canvas-3d--process process
                    canvas-3d--frame-directory (make-temp-file "bad-canvas-file-" t)
                    canvas-3d--file-identity "0123456789abcdef"
                    canvas-3d--image (list 'image :type 'canvas :data "keep"))
              (let* ((directory canvas-3d--frame-directory)
                     (file (expand-file-name "pending-1.bgra" directory))
                     (packet (canvas-3d-test--publish 1)))
                (pcase failure
                  ('missing (delete-file file))
                  ('short (canvas-3d-test--publish 1 "x"))
                  ('oversized (canvas-3d-test--publish 1 "xxxxx"))
                  ('symlink (delete-file file) (make-symbolic-link "/dev/zero" file))
                  ('permissions (set-file-modes file #o644))
                  ('identity (aset packet 8 ?f))
                  ('sequence (aset packet 7 2))
                  ('cap (setq packet (make-string 49 0))))
                (cl-letf (((symbol-function 'canvas-refresh)
                           (lambda (&rest _) (setq painted t) (error "Native refresh failed"))))
                  (canvas-3d--receive process packet))
                (should (eq painted (eq failure 'paint)))
                (should-not canvas-3d--frame)
                (should-not canvas-3d--process)
                (should-not (file-exists-p directory))
                (should (equal (plist-get (cdr canvas-3d--image) :data) "keep"))))
          (canvas-3d--stop))))))

(ert-deftest canvas-3d-file-invalid-retirement-is-bounded-and-final ()
  (dolist (failure '(missing short oversized symlink))
    (with-temp-buffer
      (let* ((directory (make-temp-file "canvas-retire-test-" t))
             (file (expand-file-name "frame-1.bgra" directory))
             (reads 0))
        (unwind-protect
            (progn
              (setq canvas-3d--size 1 canvas-3d--frame-directory directory
                    canvas-3d--current-file file
                    canvas-3d--image (list 'image :type 'canvas :data-width 1
                                           :data-height 1 :file file))
              (pcase failure
                ('short (write-region "x" nil file nil 'silent))
                ('oversized (write-region "xxxxx" nil file nil 'silent))
                ('symlink (make-symbolic-link "/dev/zero" file)))
              (when (memq failure '(short oversized)) (set-file-modes file #o600))
              (cl-letf (((symbol-function 'insert-file-contents-literally)
                         (lambda (&rest _) (setq reads (1+ reads))
                           (error "Invalid backing file must not be read"))))
                (canvas-3d-cancel)
                (canvas-3d-cancel))
              (should (= reads 0))
              (should-not (file-exists-p directory))
              (should-not canvas-3d--frame-directory)
              (should-not canvas-3d--current-file)
              (should-not (plist-member (cdr canvas-3d--image) :file))
              (should (= (length (plist-get (cdr canvas-3d--image) :data)) 4)))
          (when (file-exists-p directory) (delete-directory directory t)))))))

(ert-deftest canvas-3d-file-start-failure-and-reopen ()
  (with-temp-buffer
    (let ((path (expand-file-name "fixtures/pyramid.obj" canvas-3d--directory))
          failed-directory retired-image retired-directory)
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'image-type-available-p) (lambda (_) t))
                ((symbol-function 'canvas-refresh) #'ignore))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest _)
                     (setq failed-directory canvas-3d--frame-directory)
                     (error "Startup failure"))))
          (should-error (canvas-3d-attach path nil nil 128)))
        (should failed-directory)
        (should-not (file-exists-p failed-directory))
        (should-not canvas-3d--frame-directory)
        (dotimes (_ 2)
          (unwind-protect
              (progn
                (canvas-3d-attach path nil nil 128)
                (should-not (equal retired-directory canvas-3d--frame-directory))
                (let ((deadline (+ (float-time) 15)))
                  (while (and canvas-3d--busy (< (float-time) deadline))
                    (accept-process-output canvas-3d--process 0.05)))
                (should (equal canvas-3d--status "Ready"))
                (should (file-exists-p canvas-3d--current-file))
                (when retired-image
                  (should-not (plist-member (cdr retired-image) :file))
                  (should (= (length (plist-get (cdr retired-image) :data)) (* 4 128 128)))))
            (setq retired-image canvas-3d--image
                  retired-directory canvas-3d--frame-directory)
            (let ((read (symbol-function 'insert-file-contents-literally))
                  (reads 0))
              (cl-letf (((symbol-function 'insert-file-contents-literally)
                         (lambda (file &optional visit begin end replace)
                           (setq reads (1+ reads))
                           (should (= begin 0))
                           (should (= end (* 4 128 128)))
                           (funcall read file visit begin end replace))))
                (canvas-3d-detach)
                (canvas-3d-detach))
              (should (= reads 1)))
            (should-not (file-exists-p retired-directory))))))))

(provide 'canvas-3d-pick-tests)
;;; canvas-3d-pick-tests.el ends here
