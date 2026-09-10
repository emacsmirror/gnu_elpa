;;; canvas-3d.el --- Native canvas viewer for local OBJ scenes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "32.0.50"))
;; Keywords: multimedia
;; URL: https://thanosapollo.org/projects/gnosis/
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of Gnosis and is distributed under the GNU General
;; Public License, version 3 or later.  See LICENSE for details.

;;; Commentary:
;; Run `canvas-3d-open' in a graphical Emacs with canvas image support.
;; A single persistent subprocess owns OpenGL and the mesh.  Only one frame
;; is in flight; repeated input coalesces to the latest requested view.
;;; Code:
(require 'json)
(require 'seq)
(require 'image)
(require 'cl-lib)
(require 'regexp-opt)

(defconst canvas-3d--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the packaged renderer.")

(defgroup canvas-3d nil
  "Native canvas rendering of local OBJ scenes."
  :group 'multimedia)

(defcustom canvas-3d-python-command nil
  "Python executable used to run the packaged render.py.
Nil uses .venv/bin/python beside this library, created explicitly with
uv sync --locked.  A string names an executable on variable `exec-path' or an
absolute executable filename for a separately prepared environment.
Opening a viewer never installs dependencies."
  :type '(choice (const :tag "Packaged virtual environment" nil) string)
  :group 'canvas-3d)

(defun canvas-3d--python ()
  "Return the configured renderer Python executable, or signal an error."
  (let* ((command (or canvas-3d-python-command
                      (expand-file-name ".venv/bin/python" canvas-3d--directory)))
         (python (and (stringp command) (not (file-remote-p command))
                      (executable-find command))))
    (unless python
      (user-error "Renderer Python unavailable; run uv sync --locked in %s or set canvas-3d-python-command"
                  canvas-3d--directory))
    python))
(defvar-local canvas-3d--process nil)
(defvar-local canvas-3d--timer nil)
(defvar-local canvas-3d--image nil)
(defvar-local canvas-3d--bytes nil
  "Chunks of the incomplete packet, in reverse arrival order.")
(defvar-local canvas-3d--byte-count 0
  "Number of bytes retained in the incomplete packet.")
(defvar-local canvas-3d--busy nil)
(defvar-local canvas-3d--dirty nil)
(defvar-local canvas-3d--yaw 0)
(defvar-local canvas-3d--pitch 0)
(defvar-local canvas-3d--zoom 1.0)
(defvar-local canvas-3d--initial-view '(0 0 1.0))
(defvar-local canvas-3d--label nil)
(defvar-local canvas-3d--revealed nil)
(defvar-local canvas-3d--status "Starting")
(defvar-local canvas-3d--stderr nil)
(defvar-local canvas-3d--size 512 "Canvas side length in pixels.")
(defvar-local canvas-3d--objects nil)
(defvar-local canvas-3d--invalid-index-regexp nil
  "Cached (OBJECT-COUNT . REGEXP) matching invalid object-index bytes.")
(defvar-local canvas-3d--seq 0)
(defvar-local canvas-3d--protocol 1
  "Wire version; real viewers use 2, legacy packet fixtures default to 1.")
(defvar-local canvas-3d--selection nil
  "Owned geometry plist with :mesh :face :point :id :frame and :owner.")
(defvar-local canvas-3d--question-target nil
  "Locked question geometry alist, independent of exploratory selection.")
(defvar-local canvas-3d--requested-target nil)
(defvar-local canvas-3d--requested-geometry nil)
(defvar-local canvas-3d--requested-selection nil)
(defvar-local canvas-3d--frame nil)
(defvar-local canvas-3d-selected-id nil
  "Stable ID selected in this viewer, or nil for background.")
(defvar-local canvas-3d-selection-hook nil
  "Functions called with the geometry plist in `canvas-3d--selection'.
Keys are :mesh :face :point :id :frame and :owner.
Run synchronously from the picking command, never from the process filter.
ID is nil for background.  Consumers must not infer a scheduler action.")

(defun canvas-3d--header ()
  "Return the current viewer heading."
  (format " %s | %s | zoom %.2f | ? help"
          (if (and canvas-3d--revealed (not canvas-3d--question-target))
              canvas-3d--label "Model (label hidden)")
          canvas-3d--status canvas-3d--zoom))

(defun canvas-3d--stop (&optional status)
  "Retire the renderer and any pending frame, displaying STATUS."
  (when (timerp canvas-3d--timer) (cancel-timer canvas-3d--timer))
  (setq canvas-3d--timer nil canvas-3d--busy nil canvas-3d--dirty nil
        canvas-3d--bytes nil canvas-3d--byte-count 0 canvas-3d--frame nil
        canvas-3d-selected-id nil canvas-3d--selection nil)
  (let ((process canvas-3d--process))
    (setq canvas-3d--process nil)
    (when (process-live-p process) (delete-process process)))
  (when status (setq canvas-3d--status status))
  (force-mode-line-update))

(defun canvas-3d--cleanup ()
  "Release resources owned by this viewer."
  (canvas-3d--stop)
  (when (buffer-live-p canvas-3d--stderr) (kill-buffer canvas-3d--stderr)))

(defun canvas-3d-cancel ()
  "Stop rendering while preserving the last frame."
  (interactive)
  (canvas-3d--stop "Stopped; open another model with M-x canvas-3d-open"))

(defun canvas-3d-quit ()
  "Close this viewer and release its renderer."
  (interactive)
  (quit-window t))

(defun canvas-3d--request ()
  "Request the latest view, coalescing input while a frame is pending."
  (unless (process-live-p canvas-3d--process)
    (user-error "Renderer stopped; reopen with M-x canvas-3d-open"))
  (if canvas-3d--busy
      (setq canvas-3d--dirty t)
    (setq canvas-3d--busy t canvas-3d--dirty nil canvas-3d--status "Rendering"
          canvas-3d--requested-selection canvas-3d-selected-id
          canvas-3d--requested-geometry (copy-tree canvas-3d--selection)
          canvas-3d--requested-target (copy-tree canvas-3d--question-target))
    (let ((owner (current-buffer)) (process canvas-3d--process))
      (setq canvas-3d--timer
            (run-at-time 20 nil
                         (lambda ()
                           (when (buffer-live-p owner)
                             (with-current-buffer owner
                               (when (eq process canvas-3d--process)
                                 (canvas-3d--stop "Renderer timed out"))))))))
    (process-send-string
     canvas-3d--process
     (concat (json-encode `((seq . ,(cl-incf canvas-3d--seq))
                            (selected . ,(if-let* ((index (seq-position canvas-3d--objects canvas-3d-selected-id
									(lambda (obj id) (equal (alist-get 'id obj) id)))))
                                             (1+ index) 0))
                            (highlight . ,(or (seq-filter
                                               (lambda (pair)
                                                 (memq (car pair) '(mesh kind face barycentric tolerance faces)))
                                               canvas-3d--question-target)
                                              json-null))
                            (yaw . ,canvas-3d--yaw)
                            (pitch . ,canvas-3d--pitch)
                            (zoom . ,canvas-3d--zoom))) "\n")))
  (force-mode-line-update))

(defun canvas-3d--paint (frame)
  "Display FRAME without changing its paired object-index plane."
  (setf (plist-get (cdr canvas-3d--image) :data) (plist-get frame :color))
  (canvas-refresh canvas-3d--image t))

(defun canvas-3d--invalid-indices-p (ids count)
  "Return non-nil if IDS contains a byte above object COUNT."
  (unless (eql count (car canvas-3d--invalid-index-regexp))
    (setq canvas-3d--invalid-index-regexp
          (cons count (string-to-unibyte
                       (regexp-opt-charset
                        (mapcar #'unibyte-char-to-multibyte
                                (number-sequence (1+ count) 255)))))))
  (let ((case-fold-search nil))
    (string-match-p (cdr canvas-3d--invalid-index-regexp) ids)))

(defun canvas-3d--receive (process chunk)
  "Consume framed binary CHUNK from the exact current renderer PROCESS."
  (when-let* ((owner (process-buffer process)) ((buffer-live-p owner)))
    (with-current-buffer owner
      (when (eq process canvas-3d--process)
        (condition-case err
            (let* ((area (* canvas-3d--size canvas-3d--size))
                   (expected (+ 8 (* (if (= canvas-3d--protocol 2) 21 5) area))))
              (when (> (+ canvas-3d--byte-count (length chunk)) (* 2 expected))
                (error "Renderer buffer cap exceeded"))
              ;; Retain chunks without copying the growing packet on every read.
              ;; Split only at packet boundaries; assemble each complete packet once.
              (while (> (length chunk) 0)
                (let ((needed (- expected canvas-3d--byte-count)))
                  (if (<= (length chunk) needed)
                      (progn
                        (push chunk canvas-3d--bytes)
                        (cl-incf canvas-3d--byte-count (length chunk))
                        (setq chunk (unibyte-string)))
                    (push (substring chunk 0 needed) canvas-3d--bytes)
                    (setq canvas-3d--byte-count expected
                          chunk (substring chunk needed))))
                (when (= canvas-3d--byte-count expected)
                  (let* ((packet (mapconcat #'identity
                                            (nreverse canvas-3d--bytes)
                                            (unibyte-string)))
			 (seq (cl-loop for i from 4 below 8
                                       for n = (aref packet i) then (+ (* n 256) (aref packet i))
                                       finally return n)))
                    (unless (and canvas-3d--busy (= seq canvas-3d--seq)
				 (equal (substring packet 0 4)
                                        (if (= canvas-3d--protocol 2) "C3D2" "C3D1")))
                      (error "Unsolicited or invalid renderer frame"))
                    (let ((frame (list :owner process :seq seq
                                       :color (substring packet 8 (+ 8 (* 4 area)))
                                       :ids (substring packet (+ 8 (* 4 area)) (+ 8 (* 5 area)))
                                       :faces (and (= canvas-3d--protocol 2)
                                                   (substring packet (+ 8 (* 5 area)) (+ 8 (* 9 area))))
                                       :points (and (= canvas-3d--protocol 2)
                                                    (substring packet (+ 8 (* 9 area)))))))
                      (when (canvas-3d--invalid-indices-p
                             (plist-get frame :ids) (length canvas-3d--objects))
			(error "Invalid object index"))
                      ;; No event-loop yield between display and publication of its ID plane.
                      (when (and (equal canvas-3d--requested-selection canvas-3d-selected-id)
                                 (equal canvas-3d--requested-geometry canvas-3d--selection)
                                 (equal canvas-3d--requested-target canvas-3d--question-target))
                        (canvas-3d--paint frame)
                        (setq canvas-3d--frame frame)
                        ;; Carry original geometry, never reinterpret an old pixel.
                        (when (and canvas-3d--selection
                                   (eq (plist-get canvas-3d--selection :owner) process)
                                   (equal (plist-get canvas-3d--selection :id) canvas-3d-selected-id)
                                   (equal (plist-get canvas-3d--selection :mesh) canvas-3d-selected-id))
                          (setq canvas-3d--selection
                                (plist-put (copy-sequence canvas-3d--selection) :frame seq)))))
                    (when (timerp canvas-3d--timer) (cancel-timer canvas-3d--timer))
                    (setq canvas-3d--timer nil
                          canvas-3d--bytes nil canvas-3d--byte-count 0
                          canvas-3d--busy nil canvas-3d--status "Ready")
                    (when canvas-3d--dirty (canvas-3d--request)))))
              (force-mode-line-update))
          (error (canvas-3d--stop (error-message-string err))))))))

(defun canvas-3d--uint32 (bytes offset)
  "Decode a big-endian uint32 from BYTES at OFFSET."
  (cl-loop for i from offset below (+ offset 4)
           for value = (aref bytes i) then (+ (* value 256) (aref bytes i))
           finally return value))

(defun canvas-3d--float32 (bytes offset)
  "Decode a finite big-endian IEEE float32 from BYTES at OFFSET."
  (let* ((bits (canvas-3d--uint32 bytes offset))
         (exponent (logand (ash bits -23) 255))
         (fraction (logand bits #x7fffff)))
    (when (= exponent 255) (error "Non-finite renderer coordinate"))
    (* (if (zerop (logand bits #x80000000)) 1.0 -1.0)
       (if (zerop exponent)
           (* fraction (expt 2.0 -149))
         (* (+ 1.0 (/ fraction 8388608.0)) (expt 2.0 (- exponent 127)))))))

(defun canvas-3d-pick (x y &optional frame)
  "Select geometry at canvas pixel X, Y, returning its mesh ID or nil.
Use displayed FRAME when supplied; reject retired or replaced frames.
A click during rendering uses the displayed frame, not the pending camera.
A locked question target is never changed by picking.
Run `canvas-3d-selection-hook' synchronously with a plain selection plist."
  (let ((frame (or frame canvas-3d--frame)))
    (unless (and frame (eq frame canvas-3d--frame)
                 (eq (plist-get frame :owner) canvas-3d--process)
                 (process-live-p canvas-3d--process))
      (user-error "No live displayed frame"))
    (unless (and (integerp x) (integerp y) (<= 0 x) (< x canvas-3d--size)
                 (<= 0 y) (< y canvas-3d--size))
      (user-error "Pixel outside canvas"))
    (let* ((pixel (+ x (* y canvas-3d--size)))
           (index (aref (plist-get frame :ids) pixel))
           (id (and (> index 0) (alist-get 'id (nth (1- index) canvas-3d--objects))))
           (face (and id (plist-get frame :faces)
                      (1- (canvas-3d--uint32 (plist-get frame :faces) (* 4 pixel)))))
           (point (and id (plist-get frame :points)
                       (cl-loop for axis below 3 collect
                                (canvas-3d--float32 (plist-get frame :points)
                                                    (+ (* 12 pixel) (* 4 axis)))))))
      (when (and face (< face 0)) (user-error "Missing renderer face"))
      (setq canvas-3d-selected-id id
            canvas-3d--selection
            (list :mesh id :face face :point point :id id
                  :frame (plist-get frame :seq) :owner canvas-3d--process))
      ;; A question highlight is independent of this authoring/inspection hit.
      (canvas-3d--request)
      (run-hook-with-args 'canvas-3d-selection-hook (copy-tree canvas-3d--selection))
      id)))

(defun canvas-3d--pixel (position)
  "Return canvas pixel coordinates for image POSITION, or nil outside it.
Use glyph-relative coordinates and displayed dimensions, not global mouse state."
  (when (and (eq (posn-window position) (selected-window))
             (eq (posn-image position) canvas-3d--image))
    (let ((xy (posn-object-x-y position))
          (size (posn-object-width-height position)))
      (when (and xy size (> (car size) 0) (> (cdr size) 0)
                 (<= 0 (car xy)) (< (car xy) (car size))
                 (<= 0 (cdr xy)) (< (cdr xy) (cdr size)))
        (cons (floor (* (car xy) canvas-3d--size) (car size))
              (floor (* (cdr xy) canvas-3d--size) (cdr size)))))))

(defun canvas-3d-mouse (event)
  "Rotate by dragging EVENT, or pick on a stationary button release.
Quit exits tracking; other keyboard events return to the command loop."
  (interactive "e")
  (let* ((position (event-start event)) (window (posn-window position)))
    (when (window-live-p window)
      (select-window window)
      (when-let* ((pixel (canvas-3d--pixel position)))
        (let ((owner (current-buffer)) (process canvas-3d--process)
              (frame canvas-3d--frame) (last (posn-x-y position))
              (origin (posn-x-y position)) (dragged nil) (done nil))
          (condition-case nil
              (track-mouse
		(while (not done)
		  (let* ((next (read-event))
			 (pos (and (mouse-event-p next) (event-end next)))
			 (xy (and pos (posn-x-y pos))))
                    (cond
                     ((not (and (buffer-live-p owner)
				(eq process canvas-3d--process))) (setq done t))
                     ((and (eq (car-safe next) 'mouse-movement)
			   (eq (posn-window pos) window))
                      (when (or dragged (> (+ (abs (- (car xy) (car origin)))
                                              (abs (- (cdr xy) (cdr origin)))) 3))
			(setq dragged t)
			(canvas-3d-rotate (* 0.5 (- (car xy) (car last)))
					  (* 0.5 (- (cdr xy) (cdr last))))
			(setq last xy)))
                     ((memq (car-safe next) '(mouse-1 drag-mouse-1))
                      (setq done t)
                      (when (and (not dragged) (eq (posn-window pos) window)
				 (canvas-3d--pixel pos))
			(canvas-3d-pick (car pixel) (cdr pixel) frame)))
                     ((eq (car-safe next) 'mouse-movement) nil)
                     (t (setq unread-command-events (cons next unread-command-events)
                              done t))))))
            (quit (canvas-3d-cancel) (signal 'quit nil))))))))

(defun canvas-3d-wheel (event)
  "Zoom the canvas under wheel EVENT."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (window-live-p window)
      (with-selected-window window
        (when (and canvas-3d--image
                   (canvas-3d--pixel (event-start event)))
          (canvas-3d-zoom (if (memq (car event) '(wheel-up mouse-4)) 1.15 (/ 1.0 1.15))))))))

(defun canvas-3d--sentinel (process _event)
  "Settle an exited renderer PROCESS without touching successor sessions."
  (when-let* ((owner (process-buffer process)) ((buffer-live-p owner)))
    (with-current-buffer owner
      (when (and (eq process canvas-3d--process)
                 (memq (process-status process) '(exit signal failed)))
        (canvas-3d--stop
         (format "Renderer exited (%s); e shows log" (process-exit-status process)))))))

(defun canvas-3d-rotate (yaw pitch)
  "Rotate the model by YAW and PITCH degrees."
  (setq canvas-3d--yaw (mod (+ canvas-3d--yaw yaw) 360)
        canvas-3d--pitch (mod (+ canvas-3d--pitch pitch) 360))
  (canvas-3d--request))
(defun canvas-3d-left () "Rotate left." (interactive) (canvas-3d-rotate -10 0))
(defun canvas-3d-right () "Rotate right." (interactive) (canvas-3d-rotate 10 0))
(defun canvas-3d-up () "Rotate up." (interactive) (canvas-3d-rotate 0 -10))
(defun canvas-3d-down () "Rotate down." (interactive) (canvas-3d-rotate 0 10))
(defun canvas-3d-zoom (factor)
  "Multiply the model zoom by FACTOR, clamped to 0.25 through 4."
  (setq canvas-3d--zoom (max 0.25 (min 4.0 (* canvas-3d--zoom factor))))
  (canvas-3d--request))
(defun canvas-3d-zoom-in () "Zoom in." (interactive) (canvas-3d-zoom 1.15))
(defun canvas-3d-zoom-out () "Zoom out." (interactive) (canvas-3d-zoom (/ 1.0 1.15)))
(defun canvas-3d-reset ()
  "Restore the initial orientation and zoom."
  (interactive)
  (setq canvas-3d--yaw (nth 0 canvas-3d--initial-view)
        canvas-3d--pitch (nth 1 canvas-3d--initial-view)
        canvas-3d--zoom (nth 2 canvas-3d--initial-view))
  (canvas-3d--request))
(defun canvas-3d-reveal ()
  "Toggle the model label, without changing its orientation."
  (interactive)
  (setq canvas-3d--revealed (not canvas-3d--revealed))
  (force-mode-line-update))
(defun canvas-3d-log ()
  "Display the renderer diagnostic log."
  (interactive)
  (when (buffer-live-p canvas-3d--stderr) (display-buffer canvas-3d--stderr)))

(defvar-keymap canvas-3d-mode-map
  :parent special-mode-map
  "<down-mouse-1>" #'canvas-3d-mouse
  "<wheel-up>" #'canvas-3d-wheel "<wheel-down>" #'canvas-3d-wheel
  "<mouse-4>" #'canvas-3d-wheel "<mouse-5>" #'canvas-3d-wheel
  "<left>" #'canvas-3d-left "<right>" #'canvas-3d-right
  "<up>" #'canvas-3d-up "<down>" #'canvas-3d-down
  "b" #'canvas-3d-left "f" #'canvas-3d-right
  "p" #'canvas-3d-up "n" #'canvas-3d-down
  "+" #'canvas-3d-zoom-in "=" #'canvas-3d-zoom-in "-" #'canvas-3d-zoom-out
  "r" #'canvas-3d-reset "SPC" #'canvas-3d-reveal
  "e" #'canvas-3d-log "?" #'describe-mode
  "C-g" #'canvas-3d-cancel "q" #'canvas-3d-quit)

(define-derived-mode canvas-3d-mode special-mode "Canvas-3D"
  "View an OBJ through an Emacs canvas, with asynchronous OpenGL rendering.
Drag with the left button to rotate; click to pick; wheel or +/- to zoom.
Arrow keys or n/p/f/b rotate; r resets; SPC reveals the label.
\\<canvas-3d-mode-map>\\[canvas-3d-cancel] stops rendering.
Use \\[canvas-3d-quit] to close the viewer and \\[canvas-3d-log] to show errors."
  (setq-local cursor-type nil)
  (setq-local truncate-lines t)
  (setq-local header-line-format '(:eval (canvas-3d--header)))
  (add-hook 'kill-buffer-hook #'canvas-3d--cleanup nil t)
  (add-hook 'change-major-mode-hook #'canvas-3d--cleanup nil t))

;;;###autoload
(defun canvas-3d-open (path &optional label initial-view size buffer)
  "Open local OBJ or scene JSON PATH in a fresh canvas viewer.
Use optional LABEL as the hidden title.
INITIAL-VIEW is a list (YAW PITCH ZOOM), defaulting to (0 0 1.0).
Reset returns to that view.  The label starts hidden.
SIZE is an integer from 128 to 768 pixels, defaulting to 512.
Scene JSON supplies objects and initial_view.
When BUFFER is non-nil, attach at its point without changing mode or layout.
Only one attachment may own a buffer; use `canvas-3d-detach' when finished."
  (interactive "fOBJ or scene JSON: ")
  (unless (and (integerp (or size 512)) (<= 128 (or size 512) 768))
    (user-error "Size must be an integer from 128 to 768"))
  (when (or (file-remote-p path) (not (file-readable-p path)))
    (user-error "Select a readable local scene or OBJ"))
  (let ((objects
         (if (equal (downcase (or (file-name-extension path) "")) "json")
             (progn
               (when (> (file-attribute-size (file-attributes path)) 65536)
                 (user-error "Scene JSON exceeds 64 KiB"))
               (let ((scene (json-parse-string
                             (with-temp-buffer (insert-file-contents path) (buffer-string))
                             :object-type 'alist :array-type 'list)))
                 (setq initial-view (or initial-view (alist-get 'initial_view scene)))
                 (alist-get 'objects scene)))
           (list (list (cons 'id "model") (cons 'label (or label (file-name-base path))))))))
    (unless (and (proper-list-p objects) (<= 1 (length objects) 255)
                 (seq-every-p (lambda (obj) (and (stringp (alist-get 'id obj))
                                                 (> (length (alist-get 'id obj)) 0)
                                                 (stringp (alist-get 'label obj)))) objects)
                 (= (length objects) (length (delete-dups (mapcar (lambda (o) (alist-get 'id o)) objects)))))
      (user-error "Scene requires 1..255 objects with unique string IDs and labels"))
    (setq initial-view (or initial-view '(0 0 1.0)))
    (unless (and (proper-list-p initial-view) (= (length initial-view) 3)
		 (seq-every-p (lambda (value)
				(and (numberp value) (<= (abs value) 1000000)))
                              initial-view)
		 (<= 0.25 (nth 2 initial-view) 4))
      (user-error "Initial view must contain finite yaw, pitch, and zoom 0.25..4"))
    (unless (and (display-graphic-p) (fboundp 'canvas-refresh)
		 (image-type-available-p 'canvas))
      (user-error "A graphical Emacs with canvas images is required"))
    (when (or (file-remote-p path) (not (file-readable-p path)))
      (user-error "Select a readable local OBJ file"))
    (let ((python (canvas-3d--python)))
      (let* ((embedded buffer)
             (buffer (or buffer (generate-new-buffer "*Canvas 3D*"))))
	(with-current-buffer buffer
          (when canvas-3d--image
            (user-error "A canvas already owns this buffer; detach it first"))
          (unless embedded (canvas-3d-mode))
          (add-hook 'kill-buffer-hook #'canvas-3d--cleanup nil t)
          (add-hook 'change-major-mode-hook #'canvas-3d--cleanup nil t)
          (setq canvas-3d--objects objects
                canvas-3d--protocol 2 canvas-3d--selection nil
                canvas-3d--question-target nil canvas-3d-selected-id nil
		canvas-3d--size (or size 512)
		canvas-3d--initial-view (copy-sequence initial-view)
		canvas-3d--yaw (nth 0 initial-view)
		canvas-3d--pitch (nth 1 initial-view)
		canvas-3d--zoom (float (nth 2 initial-view))
		canvas-3d--label (or label (file-name-base path))
		canvas-3d--bytes nil canvas-3d--byte-count 0
		canvas-3d--stderr (generate-new-buffer " *Canvas 3D log*"))
          (setq canvas-3d--image
		(list 'image :type 'canvas :scale 1.0 :id (make-symbol "canvas-3d")
                      :data-width canvas-3d--size :data-height canvas-3d--size
                      :data (make-string (* 4 canvas-3d--size canvas-3d--size) 0)))
          (let ((inhibit-read-only t))
            (insert (propertize " " 'display canvas-3d--image) "\n")
            (unless embedded (goto-char (point-min))))
          (condition-case err
              (progn
		(setq canvas-3d--process
                      (make-process :name "canvas-3d" :buffer buffer
                                    :command (list python (expand-file-name "render.py" canvas-3d--directory)
                                                   (expand-file-name path) "--size"
                                                   (number-to-string canvas-3d--size))
                                    :connection-type 'pipe :coding 'binary :noquery t
                                    :stderr canvas-3d--stderr
                                    :filter #'canvas-3d--receive :sentinel #'canvas-3d--sentinel))
		(canvas-3d--request))
            (error
             (if embedded
                 (progn (canvas-3d-detach) (signal (car err) (cdr err)))
               (canvas-3d--stop (error-message-string err))))
            (quit
             (canvas-3d-detach)
             (unless embedded (kill-buffer buffer))
             (signal (car err) (cdr err)))))
	(unless embedded (pop-to-buffer buffer))
	buffer))))

(defun canvas-3d-attach (path &optional label initial-view size)
  "Attach PATH with LABEL, INITIAL-VIEW and SIZE at point in this buffer.
Preserve its text, major mode, keymap and windows.  The caller owns input keys.
Call `canvas-3d-detach' on completion; the last display remains as feedback."
  (canvas-3d-open path label initial-view size (current-buffer)))

(defun canvas-3d-detach ()
  "Release this buffer's renderer without killing the buffer or its last image."
  (canvas-3d--cleanup)
  (setq canvas-3d--image nil canvas-3d--stderr nil
        canvas-3d--revealed nil canvas-3d--seq 0
        canvas-3d--question-target nil canvas-3d--requested-target nil
        canvas-3d--requested-geometry nil)
  (remove-hook 'kill-buffer-hook #'canvas-3d--cleanup t)
  (remove-hook 'change-major-mode-hook #'canvas-3d--cleanup t))

(provide 'canvas-3d)
;;; canvas-3d.el ends here
