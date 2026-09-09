;;; canvas-3d-tests.el --- Canvas renderer tests -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Thanos Apollo
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Exercise generic transport, lifecycle and real EGL with synthetic geometry.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'canvas-3d)

(ert-deftest canvas-3d-controls-and-label ()
  (with-temp-buffer
    (canvas-3d-mode)
    (setq canvas-3d--label "Pyramid")
    (cl-letf (((symbol-function 'canvas-3d--request) #'ignore))
      (canvas-3d-right) (canvas-3d-up) (canvas-3d-zoom-in)
      (should (= canvas-3d--yaw 10))
      (should (= canvas-3d--pitch 350))
      (should (> canvas-3d--zoom 1))
      (canvas-3d-zoom 100) (should (= canvas-3d--zoom 4))
      (canvas-3d-zoom 0.001) (should (= canvas-3d--zoom 0.25))
      (canvas-3d-reset)
      (should (equal (list canvas-3d--yaw canvas-3d--pitch canvas-3d--zoom) '(0 0 1.0)))
      (should-not (string-match-p "Pyramid" (canvas-3d--header)))
      (canvas-3d-reveal)
      (should (string-match-p "Pyramid" (canvas-3d--header)))
      (setq canvas-3d--initial-view '(0 -90 1.0))
      (canvas-3d-right) (canvas-3d-reset)
      (should (= canvas-3d--pitch -90))
      (should (eq (keymap-lookup canvas-3d-mode-map "q") #'canvas-3d-quit)))))

(ert-deftest canvas-3d-real-process-frame-coalescing-and-cleanup ()
  ;; Batch Emacs cannot draw; stub only the final native display operation.
  ;; Everything through EGL, pipes, binary filter, state, and ownership is real.
  (let ((owner (generate-new-buffer " *canvas test*")) (refreshes 0) process image)
    (unwind-protect
        (cl-letf (((symbol-function 'canvas-refresh)
                   (lambda (spec reload)
                     (should (eq spec image))
                     (should reload)
                     (cl-incf refreshes))))
          (with-current-buffer owner
            (canvas-3d-mode)
            (setq canvas-3d--objects '(((id . "model") (label . "Model")))
                  canvas-3d--image
                  (list 'image :type 'canvas :id (make-symbol "test")
                        :data-width 512 :data-height 512)
                  image canvas-3d--image
                  canvas-3d--bytes nil canvas-3d--byte-count 0
                  canvas-3d--stderr (generate-new-buffer " *canvas test log*"))
            (setq process
                  (make-process :name "canvas-test" :buffer owner :coding 'binary
                                :connection-type 'pipe :noquery t
                                :command (list (canvas-3d--python)
                                               (expand-file-name "render.py" canvas-3d--directory)
                                               (expand-file-name "fixtures/pyramid.obj" canvas-3d--directory))
                                :stderr canvas-3d--stderr
                                :filter #'canvas-3d--receive :sentinel #'canvas-3d--sentinel)
                  canvas-3d--process process)
            (canvas-3d--request)
            (canvas-3d-right) (canvas-3d-right)
            (should canvas-3d--dirty)
            (let ((deadline (+ (float-time) 15)))
              (while (and canvas-3d--busy (< (float-time) deadline))
                (accept-process-output process 0.05)))
            (should (= refreshes 2))
            (should (equal canvas-3d--status "Ready"))
            (should-not canvas-3d--timer)
            (should (= (length (plist-get (cdr image) :data)) (* 512 512 4)))
            (should-not (multibyte-string-p (plist-get (cdr image) :data)))
            (let* ((frame canvas-3d--frame)
                   (pixel (seq-position (plist-get frame :ids) 1))
                   (color (plist-get frame :color)))
              (should pixel)
              (should (equal (canvas-3d-pick (% pixel 512) (/ pixel 512)) "model"))
              (let ((deadline (+ (float-time) 15)))
                (while (and canvas-3d--busy (< (float-time) deadline))
                  (accept-process-output process 0.05)))
              (should (equal canvas-3d--status "Ready"))
              (should (equal (plist-get frame :ids) (plist-get canvas-3d--frame :ids)))
              (should-not (equal color (plist-get canvas-3d--frame :color))))
            (canvas-3d-cancel)
            (should-not (process-live-p process))
            (should-not canvas-3d--process)
            ;; A queued filter from the retired process cannot replace its frame.
            (let ((before (plist-get (cdr image) :data)))
              (canvas-3d--receive process (unibyte-string 1 2 3))
              (should (eq before (plist-get (cdr image) :data))))))
      (when (buffer-live-p owner) (kill-buffer owner))
      (when (process-live-p process) (delete-process process)))))

(ert-deftest canvas-3d-mode-change-cleans-process ()
  (with-temp-buffer
    (canvas-3d-mode)
    (let ((process (make-pipe-process :name "canvas-pipe" :buffer (current-buffer) :noquery t)))
      (setq canvas-3d--process process
            canvas-3d--timer (run-at-time 100 nil #'ignore))
      (fundamental-mode)
      (should-not (process-live-p process)))))

(defun canvas-3d-test--packet (seq ids)
  "Return tiny protocol frame SEQ with four IDS pixels."
  (concat "C3D1" (unibyte-string 0 0 0 seq) (make-string 16 42) ids))

(ert-deftest canvas-3d-fragment-multiframe-pick-and-owner ()
  (with-temp-buffer
    (canvas-3d-mode)
    (setq canvas-3d--size 2 canvas-3d--seq 1 canvas-3d--busy t
          canvas-3d--objects '(((id . "one")) ((id . "two")))
          canvas-3d--bytes nil canvas-3d--byte-count 0
          canvas-3d--image (list 'image :data nil))
    (let* ((p (make-pipe-process :name "canvas-fragments" :buffer (current-buffer) :noquery t))
           (a (canvas-3d-test--packet 1 (unibyte-string 1 2 0 1)))
           (b (canvas-3d-test--packet 2 (unibyte-string 2 1 0 2)))
           events)
      (setq canvas-3d--process p)
      (add-hook 'canvas-3d-selection-hook (lambda (value) (push value events)) nil t)
      (cl-letf (((symbol-function 'canvas-refresh) #'ignore))
        (canvas-3d--receive p (substring a 0 3))
        (should-not canvas-3d--frame)
        (canvas-3d--receive p (substring a 3))
        (let ((displayed canvas-3d--frame))
          ;; Pick while a camera frame is outstanding: IDs are still frame 1.
          (setq canvas-3d--busy t canvas-3d--seq 2)
          (should (equal (canvas-3d-pick 0 0) "one"))
          (should (= (plist-get (car events) :frame) 1))
          (should canvas-3d--dirty)
          ;; Old unhighlighted in-flight frame is discarded; selection survives.
          (canvas-3d--receive p b)
          (should (eq displayed canvas-3d--frame))
          (should (equal canvas-3d-selected-id "one"))
          (should (= canvas-3d--seq 3))
          (should-not (canvas-3d-pick 0 1))
          (should-not (plist-get (car events) :id))
          (canvas-3d-cancel)
          (should-error (canvas-3d-pick 0 0 displayed) :type 'user-error)
          (canvas-3d--receive p a)
          (should-not canvas-3d--frame)
          (should-not canvas-3d-selected-id))))))

(ert-deftest canvas-3d-coalesced-packets-and-cap ()
  (with-temp-buffer
    (canvas-3d-mode)
    (setq canvas-3d--size 2 canvas-3d--seq 1 canvas-3d--busy t
          canvas-3d--dirty t canvas-3d--objects '(((id . "one")))
          canvas-3d--bytes nil canvas-3d--byte-count 0 canvas-3d--image (list 'image :data nil))
    (let* ((p (make-pipe-process :name "canvas-multiple" :buffer (current-buffer) :noquery t))
           (a (canvas-3d-test--packet 1 (unibyte-string 1 0 0 1)))
           (b (canvas-3d-test--packet 2 (unibyte-string 0 1 1 0))))
      (setq canvas-3d--process p)
      (cl-letf (((symbol-function 'canvas-refresh) #'ignore))
        (canvas-3d--receive p (concat a (substring b 0 6)))
        (should (= (plist-get canvas-3d--frame :seq) 1))
        (canvas-3d--receive p (substring b 6))
        (should (= (plist-get canvas-3d--frame :seq) 2))
        (should-not canvas-3d--busy)
        (canvas-3d--receive p (make-string 100 0))
        (should-not canvas-3d--process)
        (should (string-match-p "cap" canvas-3d--status))))))

(ert-deftest canvas-3d-image-pixel-origin-and-scaling ()
  (with-temp-buffer
    (canvas-3d-mode)
    (setq canvas-3d--size 768 canvas-3d--image '(image :type canvas))
    ;; Native posn accessors: image glyph at (100,200) in window,
    ;; object-relative click (128,64), displayed at 384 square.
    (let ((pos (list (selected-window) 1 '(228 . 264) 0 nil 1 '(0 . 0)
                     canvas-3d--image '(128 . 64) '(384 . 384))))
      (should (equal (canvas-3d--pixel pos) '(256 . 128)))
      (setcar (nthcdr 8 pos) '(384 . 64))
      (should-not (canvas-3d--pixel pos)))
    (should (eq (keymap-lookup canvas-3d-mode-map "<down-mouse-1>") #'canvas-3d-mouse))
    (should (eq (keymap-lookup canvas-3d-mode-map "<wheel-up>") #'canvas-3d-wheel))))

(ert-deftest canvas-3d-chunks-retained-once-and-cancelled ()
  (with-temp-buffer
    (canvas-3d-mode)
    (setq canvas-3d--size 2 canvas-3d--seq 1 canvas-3d--busy t
          canvas-3d--objects '(((id . "one"))))
    (let* ((p (make-pipe-process :name "canvas-chunks" :buffer (current-buffer) :noquery t))
           (packet (canvas-3d-test--packet 1 (unibyte-string 1 0 1 0)))
           (a (substring packet 0 3)) (b (substring packet 3 9)))
      (setq canvas-3d--process p)
      (canvas-3d--receive p a)
      (canvas-3d--receive p (unibyte-string))
      (canvas-3d--receive p b)
      ;; Identity, not timing: incomplete reads must not copy earlier payloads.
      (should (eq (car canvas-3d--bytes) b))
      (should (eq (cadr canvas-3d--bytes) a))
      (should (= canvas-3d--byte-count 9))
      (should-not canvas-3d--frame)
      (canvas-3d-cancel)
      (should-not canvas-3d--bytes)
      (should (zerop canvas-3d--byte-count))
      (canvas-3d--receive p (substring packet 9))
      (should-not canvas-3d--bytes)
      (should-not canvas-3d--frame))))

(ert-deftest canvas-3d-every-packet-split-and-adjacent-frames ()
  (let* ((a (canvas-3d-test--packet 1 (unibyte-string 1 0 1 0)))
         (b (canvas-3d-test--packet 2 (unibyte-string 0 1 0 1)))
         (stream (concat a b)))
    (dotimes (split (1+ (length stream)))
      (with-temp-buffer
        (canvas-3d-mode)
        (setq canvas-3d--size 2 canvas-3d--seq 1 canvas-3d--busy t
              canvas-3d--dirty t canvas-3d--objects '(((id . "one"))))
        (let ((p (make-pipe-process :name "canvas-splits" :buffer (current-buffer) :noquery t))
              frames)
          (setq canvas-3d--process p)
          (cl-letf (((symbol-function 'canvas-3d--paint)
                     (lambda (frame) (push frame frames))))
            (canvas-3d--receive p (substring stream 0 split))
            (canvas-3d--receive p (substring stream split)))
          (should (equal (mapcar (lambda (f) (plist-get f :seq)) frames) '(2 1)))
          (should (equal (plist-get canvas-3d--frame :ids) (unibyte-string 0 1 0 1)))
          (should (equal (plist-get canvas-3d--frame :color) (make-string 16 42)))
          (should (eq (plist-get canvas-3d--frame :owner) p))
          (should-not canvas-3d--bytes)
          (should (zerop canvas-3d--byte-count))
          (should-not canvas-3d--busy))))))

(ert-deftest canvas-3d-fragmented-invalid-frames-retire-owner ()
  (dolist (packet (list (canvas-3d-test--packet 2 (unibyte-string 1 0 1 0))
                        (concat "BAD!" (substring (canvas-3d-test--packet 1 (unibyte-string 1 0 1 0)) 4))
                        (canvas-3d-test--packet 1 (unibyte-string 1 0 2 0))))
    (with-temp-buffer
      (canvas-3d-mode)
      (setq canvas-3d--size 2 canvas-3d--seq 1 canvas-3d--busy t
            canvas-3d--objects '(((id . "one"))))
      (let ((p (make-pipe-process :name "canvas-invalid" :buffer (current-buffer) :noquery t)))
        (setq canvas-3d--process p)
        (cl-letf (((symbol-function 'canvas-3d--paint)
                   (lambda (_) (ert-fail "Invalid frame was displayed"))))
          (dotimes (i (length packet))
            (canvas-3d--receive p (substring packet i (1+ i)))))
	(should-not canvas-3d--process)
	(should-not canvas-3d--bytes)
	(should (zerop canvas-3d--byte-count))
	(should-not canvas-3d--frame)))))

(ert-deftest canvas-3d-index-byte-domain-exhaustive ()
  (with-temp-buffer
    (dolist (case-fold-search '(nil t))
      (cl-loop for count from 1 to 255 do
               (dotimes (byte 256)
                 (should (eq (not (null (canvas-3d--invalid-indices-p
                                        (unibyte-string byte) count)))
                             (> byte count))))))))

(ert-deftest canvas-3d-valid-index-charset-metacharacter ()
  (with-temp-buffer
    (canvas-3d-mode)
    (setq canvas-3d--size 2 canvas-3d--seq 1 canvas-3d--busy t
          canvas-3d--objects (make-list 94 '((id . "object"))))
    (let ((p (make-pipe-process :name "canvas-index93" :buffer (current-buffer) :noquery t)))
      (setq canvas-3d--process p)
      (cl-letf (((symbol-function 'canvas-3d--paint) #'ignore))
        (canvas-3d--receive p (canvas-3d-test--packet 1 (unibyte-string 0 93 94 0))))
      (should (eq canvas-3d--process p))
      (should (equal (plist-get canvas-3d--frame :ids) (unibyte-string 0 93 94 0)))
      (should (equal canvas-3d--status "Ready")))))

(ert-deftest canvas-3d-opening-keeps-point-on-image ()
  (let (viewer)
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'canvas-refresh) #'ignore)
                  ((symbol-function 'image-type-available-p) (lambda (_) t))
                  ((symbol-function 'file-executable-p) (lambda (_) t))
                  ((symbol-function 'canvas-3d--request) #'ignore)
                  ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer))
                  ((symbol-function 'make-process)
                   (lambda (&rest args)
                     (make-pipe-process :name "canvas-opening" :noquery t
                                        :buffer (plist-get args :buffer)))))
          (setq viewer (canvas-3d-open
                        (expand-file-name "fixtures/pyramid.obj" canvas-3d--directory)))
          (with-current-buffer viewer
            (should (= (point) (point-min)))
            (should (get-text-property (point) 'display))))
      (when (buffer-live-p viewer) (kill-buffer viewer)))))

(ert-deftest canvas-3d-python-configuration ()
  (let ((canvas-3d-python-command "prepared-python"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (command)
                 (should (equal command "prepared-python"))
                 "/prepared/bin/python")))
      (should (equal (canvas-3d--python) "/prepared/bin/python"))))
  (let ((canvas-3d-python-command nil)
        (canvas-3d--directory "/packaged/"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (command)
                 (should (equal command "/packaged/.venv/bin/python"))
                 nil)))
      (should-error (canvas-3d--python) :type 'user-error)))
  (let ((canvas-3d-python-command "/ssh:example:python"))
    (should-error (canvas-3d--python) :type 'user-error)))

(ert-deftest canvas-3d-size-bounds ()
  (dolist (size '(127 769 128.0 "512"))
    (should-error (canvas-3d-open "unavailable.obj" nil nil size)
                  :type 'user-error))
  (dolist (size '(128 320 768))
    (let (viewer)
      (unwind-protect
          (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                    ((symbol-function 'canvas-refresh) #'ignore)
                    ((symbol-function 'image-type-available-p) (lambda (_) t))
                    ((symbol-function 'canvas-3d--python) (lambda () "python"))
                    ((symbol-function 'canvas-3d--request) #'ignore)
                    ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer))
                    ((symbol-function 'make-process)
                     (lambda (&rest args)
                       (should (equal (car (last (plist-get args :command)))
                                      (number-to-string size)))
                       (make-pipe-process :name "canvas-size" :noquery t
                                          :buffer (plist-get args :buffer)))))
            (setq viewer (canvas-3d-open
                          (expand-file-name "fixtures/pyramid.obj" canvas-3d--directory)
                          nil nil size))
            (with-current-buffer viewer
              (should (= canvas-3d--size size))
              (should (= (length (plist-get (cdr canvas-3d--image) :data))
                         (* 4 size size)))))
        (when (buffer-live-p viewer) (kill-buffer viewer))))))

(ert-deftest canvas-3d-startup-error-visible ()
  (let (viewer)
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'canvas-refresh) #'ignore)
                  ((symbol-function 'image-type-available-p) (lambda (_) t))
                  ((symbol-function 'canvas-3d--python) (lambda () "python"))
                  ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer))
                  ((symbol-function 'make-process)
                   (lambda (&rest _) (error "Synthetic startup failure"))))
          (setq viewer (canvas-3d-open
                        (expand-file-name "fixtures/pyramid.obj" canvas-3d--directory)))
          (with-current-buffer viewer
            (should (string-match-p "Synthetic startup failure" (canvas-3d--header)))
            (should-not canvas-3d--process)
            (should-not canvas-3d--timer)
            (should-error (canvas-3d-pick 0 0) :type 'user-error)))
      (when (buffer-live-p viewer) (kill-buffer viewer)))))

(ert-deftest canvas-3d-asynchronous-startup-failure-visible ()
  (let ((scene (make-temp-file "canvas-broken-" nil ".json")) viewer)
    (unwind-protect
        (progn
          (with-temp-file scene
            (insert (json-encode
                     '((objects . [((id . "missing") (label . "Missing")
                                     (path . "absent-geometry.obj"))])))))
          (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                    ((symbol-function 'canvas-refresh) #'ignore)
                    ((symbol-function 'image-type-available-p) (lambda (_) t))
                    ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer)))
            (setq viewer (canvas-3d-open scene nil nil 128)))
          (with-current-buffer viewer
            (let ((deadline (+ (float-time) 15)))
              (while (and canvas-3d--process (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should-not canvas-3d--process)
            (should-not canvas-3d--timer)
            (should-not canvas-3d--busy)
            (should (string-match-p "Renderer exited" (canvas-3d--header)))
            (should (buffer-live-p canvas-3d--stderr))
            (with-current-buffer canvas-3d--stderr
              (should (string-match-p "absent-geometry.obj" (buffer-string))))
            (should-error (canvas-3d-pick 0 0) :type 'user-error)))
      (when (buffer-live-p viewer) (kill-buffer viewer))
      (delete-file scene))))

(provide 'canvas-3d-tests)
;;; canvas-3d-tests.el ends here
