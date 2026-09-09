;;; gnosis-model.el --- Managed model resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Model themata store (RESOURCE YAW PITCH ZOOM) strings in hypothesis and
;; one stable target ID in answer.  Resources are immutable scene directories,
;; addressed by a digest of the manifest and every geometry file.  No renderer
;; is needed to load Gnosis, import resources, or edit model themata.

;;; Code:

(require 'gnosis-assets)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'org)

(declare-function gnosis-add-thema "gnosis")
(declare-function gnosis-add-thema--assert-common "gnosis")
(declare-function gnosis-add-thema--dispatch "gnosis")
(declare-function gnosis-export--insert-thema "gnosis-export-import")
(declare-function gnosis-export-parse-themata "gnosis-export-import")
(declare-function canvas-3d-open "canvas-3d")
(declare-function canvas-3d-attach "canvas-3d")
(declare-function canvas-3d--python "canvas-3d")
(declare-function canvas-3d--request "canvas-3d")
(defvar canvas-3d--directory)
(defvar canvas-3d--process)
(defvar canvas-3d--frame)
(defvar canvas-3d--busy)
(defvar canvas-3d--dirty)
(defvar canvas-3d--yaw)
(defvar canvas-3d--pitch)
(defvar canvas-3d--zoom)
(defvar canvas-3d--status)
(defvar canvas-3d-selected-id)
(defvar canvas-3d-selection-hook)
(defvar gnosis-export-separator)

(defconst gnosis-model--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of the installed Gnosis model library.")

(defvar-local gnosis-model--author-context nil
  "Ownership and tentative fields for the active visual authoring session.")

(defcustom gnosis-model-renderer-directory nil
  "Optional directory containing canvas-3d.el, render.py and .venv.
The external canvas-3d package must implement scene opening and the
selection hook protocol.  Nil discovers it on `load-path' or in the
installed checkout's optional/canvas-3d directory."
  :type '(choice (const nil) directory)
  :group 'gnosis)

(defalias 'gnosis-model--root #'gnosis-assets-root)

(defun gnosis-model--view (view)
  "Validate and return camera VIEW as three finite numbers."
  (unless (and (proper-list-p view) (= (length view) 3)
               (seq-every-p (lambda (n) (and (numberp n) (<= (abs n) 1000000))) view)
               (<= 0.25 (nth 2 view) 4))
    (user-error "Camera requires finite yaw, pitch and zoom 0.25..4"))
  view)

(defun gnosis-model--scene (file &optional license source)
  "Read and validate scene FILE, optionally supplying LICENSE and SOURCE.
Return the manifest alist; it owns stable targets and provenance."
  (when (or (file-remote-p file) (file-symlink-p file)
            (not (file-regular-p file))
            (> (file-attribute-size (file-attributes file)) 65536))
    (user-error "Model scene must be a local JSON file below 64 KiB"))
  (let* ((scene (json-parse-string
                 (with-temp-buffer (insert-file-contents file) (buffer-string))
                 :object-type 'alist :array-type 'list))
         (objects (alist-get 'objects scene))
         (directory (file-name-directory file)))
    (when license (setf (alist-get 'license scene) license))
    (when source (setf (alist-get 'source scene) source))
    (unless (and (proper-list-p objects) (<= 1 (length objects) 255)
                 (seq-every-p
                  (lambda (object)
                    (and (stringp (alist-get 'id object))
                         (string-match-p "\\`[[:alnum:]_-]+\\'" (alist-get 'id object))
                         (stringp (alist-get 'label object))
                         (not (string-empty-p (alist-get 'label object)))
                         (equal (file-name-extension (alist-get 'path object)) "obj")
                         (gnosis-assets-file directory (alist-get 'path object))))
                  objects)
                 (= (length objects)
                    (length (delete-dups (mapcar (lambda (o) (alist-get 'id o)) objects)))))
      (user-error "Scene requires 1..255 unique stable targets and OBJ files"))
    (dolist (field '(license source))
      (unless (and (stringp (alist-get field scene))
                   (not (string-empty-p (string-trim (alist-get field scene)))))
        (user-error "Scene requires %s metadata" field)))
    (gnosis-model--view (alist-get 'initial_view scene))
    scene))

(defun gnosis-model--revision (directory)
  "Return validated scene digest for DIRECTORY."
  (let ((scene (gnosis-model--scene (gnosis-assets-file directory "scene.json"))))
    (gnosis-assets-revision
     directory (cons "scene.json" (mapcar (lambda (o) (alist-get 'path o))
                                         (alist-get 'objects scene))))))

(defun gnosis-model-import (file &optional license source)
  "Import local scene FILE into managed assets and return its reference.
Optional LICENSE and SOURCE supply missing provenance.  Stage all files,
validate their bytes, then publish by rename.  Retry shares identical resources.
Failure or quit removes only the unpublished staging directory."
  (let* ((database (gnosis--ensure-db))
         (file (gnosis-assets-file (file-name-directory (expand-file-name file))
                                   (file-name-nondirectory file)))
         (scene (gnosis-model--scene file license source))
         (json-encoding-pretty-print nil)
         (json-encoding-separator ",")
         (text (json-encode scene)))
    (when (> (string-bytes (encode-coding-string text 'utf-8-unix)) 65536)
      (user-error "Model scene must be below 64 KiB"))
    (gnosis-assets-root database)
    (concat (gnosis-assets-import
             (file-name-directory file)
             (mapcar (lambda (o) (alist-get 'path o)) (alist-get 'objects scene))
             (list (cons "scene.json" text)))
            "/scene.json")))

(defun gnosis-model-resolve (hypothesis answer)
  "Validate model HYPOTHESIS and ANSWER; return scene path and camera.
HYPOTHESIS is (RESOURCE YAW PITCH ZOOM), all strings.  ANSWER contains one
stable target ID.  Refuse missing or changed resources, never score them."
  (unless (and (proper-list-p hypothesis) (= (length hypothesis) 4)
               (seq-every-p #'stringp hypothesis)
               (string-match-p "\\`[0-9a-f]\\{64\\}/scene\\.json\\'" (car hypothesis))
               (proper-list-p answer) (= (length answer) 1) (stringp (car answer)))
    (user-error "Invalid model resource reference or target"))
  (let* ((root (gnosis-assets-root))
         (revision (car (split-string (car hypothesis) "/")))
         (directory (expand-file-name revision root))
         (file (expand-file-name "scene.json" directory))
         (view (mapcar
                (lambda (text)
                  (unless (string-match-p
                           "\\`[-+]?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][-+]?[0-9]+\\)?\\'" text)
                    (user-error "Invalid model camera number"))
                  (string-to-number text)) (cdr hypothesis))))
    (when (or (file-symlink-p root) (file-symlink-p directory)
              (not (equal revision (gnosis-model--revision directory))))
      (user-error "Model unavailable: revision changed"))
    (unless (seq-find (lambda (o) (equal (car answer) (alist-get 'id o)))
                      (alist-get 'objects (gnosis-model--scene file)))
      (user-error "Model target is absent from its pinned scene"))
    (list file (gnosis-model--view view))))

(defun gnosis-model--save (id type keimenon hypothesis answer parathema tags suspend links)
  "Save model ID of TYPE with validated content fields.
Validate KEIMENON, HYPOTHESIS, ANSWER, PARATHEMA, TAGS, SUSPEND and LINKS."
  (gnosis-add-thema--assert-common keimenon tags suspend links)
  (gnosis-model-resolve hypothesis answer)
  (gnosis-add-thema--dispatch id type keimenon hypothesis answer parathema tags suspend links))

(defun gnosis-model--object-file (file)
  "Return readable local OBJ FILE, rejecting symlinks and directories."
  (when (or (file-remote-p file) (file-symlink-p file)
            (not (file-regular-p file)) (not (file-readable-p file))
            (not (equal (downcase (or (file-name-extension file) "")) "obj")))
    (user-error "Choose a readable local OBJ file, not a symlink"))
  (file-truename file))

(defun gnosis-model-import-objects (objects license source)
  "Import local OBJECTS with explicit LICENSE and SOURCE; return a reference.
OBJECTS is an alist of (FILE . LABEL).  Files must share scene coordinates.
Generate stable IDs from canonical source paths, independent of geometry bytes
and input order.  Copy to safe distinct basenames; retain no source paths in
those names.  Geometry changes create new immutable revisions, not new IDs."
  (unless (and (proper-list-p objects) (<= 1 (length objects) 255))
    (user-error "Choose 1..255 objects"))
  (dolist (value (list license source))
    (unless (and (stringp value) (not (string-empty-p (string-trim value))))
      (user-error "Supply the actual license/attribution and source")))
  (let* ((database (gnosis--ensure-db))
         (entries (sort (mapcar (lambda (entry)
                                 (unless (and (consp entry) (stringp (cdr entry))
                                              (not (string-empty-p (string-trim (cdr entry)))))
                                   (user-error "Every object requires a label"))
                                 (cons (gnosis-model--object-file (car entry)) (cdr entry)))
                               objects)
                        (lambda (a b) (string< (car a) (car b)))))
         (paths (mapcar #'car entries))
         (stage (make-temp-file "gnosis-model-scene-" t)))
    (unwind-protect
        (progn
          (unless (= (length paths) (length (delete-dups (copy-sequence paths))))
            (user-error "Choose each OBJ file only once"))
          (let* ((manifest-objects
                  (mapcar
                   (lambda (entry)
                     (let* ((id (concat "object-" (secure-hash
                                                  'sha256 (encode-coding-string
                                                           (car entry) 'utf-8-unix))))
                            (name (concat id ".obj")))
                       (copy-file (car entry) (expand-file-name name stage))
                       `((id . ,id) (label . ,(cdr entry)) (path . ,name))))
                   entries))
                 (scene `((objects . ,(vconcat manifest-objects))
                          (initial_view . [0 0 1]) (license . ,license) (source . ,source)))
                 (file (expand-file-name "scene.json" stage))
                 (json-encoding-pretty-print nil)
                 (json-encoding-separator ",")
                 (coding-system-for-write 'utf-8-unix))
            (with-temp-file file (insert (json-encode scene)))
            (gnosis-assets-root database)
            (gnosis-model-import file)))
      (delete-directory stage t))))

(defun gnosis-model--author-check (context)
  "Reject changed authoring owner or database in CONTEXT."
  (let ((owner (plist-get context :buffer)))
    (unless (and (buffer-live-p owner)
                 (eq (plist-get context :database) (gnosis--ensure-db))
                 (with-current-buffer owner
                   (and (eq major-mode (plist-get context :mode))
                        (= (buffer-chars-modified-tick) (plist-get context :tick)))))
      (user-error "Model authoring owner or database changed"))))

(defun gnosis-model--read-source (context)
  "Read and import OBJ files or an advanced scene manifest for CONTEXT."
  (let* ((file (expand-file-name
                (read-file-name "Model OBJ (or scene JSON): " nil nil t)))
         (manifest-p (equal (downcase (or (file-name-extension file) "")) "json"))
         (objects
          (unless manifest-p
            (let ((files (list (gnosis-model--object-file file))))
              (while (and (< (length files) 255) (y-or-n-p "Add another OBJ to this scene? "))
                (setq files (append files
                                    (list (gnosis-model--object-file
                                           (read-file-name "Next OBJ: " nil nil t))))))
              (mapcar (lambda (path)
                        (cons path (read-string (format "Label for %s: " path)
                                                (file-name-base path)))) files))))
         (license (read-string (if manifest-p
                                   "License/attribution (blank uses manifest): "
                                 "License/attribution (include required notices): ")))
         (source (read-string (if manifest-p "Source (blank uses manifest): "
                                "Source/provenance: "))))
    (gnosis-model--author-check context)
    (if manifest-p
        (gnosis-model-import file (unless (string-empty-p license) license)
                             (unless (string-empty-p source) source))
      (gnosis-model-import-objects objects license source))))

(defun gnosis-model--read-numeric (hypothesis answer)
  "Read advanced target and camera fields from HYPOTHESIS and ANSWER."
  (let* ((resolved (gnosis-model-resolve hypothesis answer))
         (objects (alist-get 'objects (gnosis-model--scene (car resolved))))
         (choices (mapcar (lambda (o)
                            (cons (format "%s (%s)" (alist-get 'label o) (alist-get 'id o))
                                  (alist-get 'id o))) objects))
         (target (cdr (assoc (completing-read "Expected target: " choices nil t) choices)))
         (view (gnosis-model--view
                (cl-mapcar (lambda (label value) (read-number label value))
                           '("Starting yaw: " "Starting pitch: " "Starting zoom: ")
                           (cadr resolved)))))
    (list (cons (car hypothesis) (mapcar #'number-to-string view)) (list target))))

(defun gnosis-model--read-fields (&optional existing)
  "Read visual model authoring fields, optionally reframing EXISTING fields.
Return (HYPOTHESIS ANSWER).  A prefix argument selects advanced numeric
input instead of the canvas.  Import immutable assets before visual input."
  (let* ((context (list :buffer (current-buffer) :mode major-mode
                        :tick (buffer-chars-modified-tick) :database (gnosis--ensure-db)))
         (advanced current-prefix-arg)
         (keep (and existing (car (car existing))
                    (y-or-n-p "Reframe the current scene (no imports)? ")))
         (fields
          (if keep existing
            (let* ((reference (gnosis-model--read-source context))
                   (scene (gnosis-model--scene
                           (expand-file-name reference (gnosis-assets-root)))))
              (list (cons reference (mapcar #'number-to-string (alist-get 'initial_view scene)))
                    (list (alist-get 'id (car (alist-get 'objects scene)))))))))
    (gnosis-model--author-check context)
    (apply #'gnosis-model-resolve fields)
    (let ((result (if advanced (apply #'gnosis-model--read-numeric fields)
                    (gnosis-model--read-visual (car fields) (cadr fields)
                                              (append context (list :initial (and keep fields)))))))
      (gnosis-model--author-check context)
      (apply #'gnosis-model-resolve result)
      result)))

(defun gnosis-model--author-header ()
  "Return selected object label and live renderer status for authoring."
  (let* ((context gnosis-model--author-context)
         (id (plist-get context :target))
         (object (seq-find (lambda (o) (equal id (alist-get 'id o)))
                           (plist-get context :objects))))
    (format " %s | %s | RET accept, q cancel, ? help"
            (if object (alist-get 'label object) "Click target; drag to frame")
            (if (process-live-p canvas-3d--process) canvas-3d--status
              (concat "Unavailable: " canvas-3d--status)))))

(defun gnosis-model--author-selection (selection)
  "Retain canvas SELECTION as a tentative authoring target, never accept it."
  (when (and gnosis-model--author-context
             (eq (plist-get selection :owner) canvas-3d--process)
             (equal (plist-get selection :frame) (plist-get canvas-3d--frame :seq)))
    (setf (plist-get gnosis-model--author-context :target) (plist-get selection :id))
    (force-mode-line-update)))

(defun gnosis-model-author-cancel ()
  "Cancel visual authoring without changing the original draft."
  (interactive)
  (when gnosis-model--author-context
    (setf (plist-get gnosis-model--author-context :cancelled) t)
    (when (= (recursion-depth) (1+ (plist-get gnosis-model--author-context :depth)))
      (abort-recursive-edit))))

(defun gnosis-model-author-accept ()
  "Accept the selected target and current ready camera into authoring fields."
  (interactive)
  (let* ((context gnosis-model--author-context)
         (target (plist-get context :target)))
    (unless (and context (not (plist-get context :result))
                 (not (plist-get context :cancelled))
                 (= (recursion-depth) (1+ (plist-get context :depth))))
      (user-error "No active visual model authoring"))
    (gnosis-model--author-check context)
    (unless (and target (equal target canvas-3d-selected-id)
                 (process-live-p canvas-3d--process)
                 (eq (plist-get context :process) canvas-3d--process)
                 (eq (plist-get canvas-3d--frame :owner) canvas-3d--process)
                 (not canvas-3d--busy) (not canvas-3d--dirty))
      (user-error "Select a target and wait for the view to be ready"))
    (let* ((view (gnosis-model--view
                  (append (cl-mapcar (lambda (angle initial)
                                       (+ angle (* 360 (round (/ (- initial angle) 360.0)))))
                                     (list canvas-3d--yaw canvas-3d--pitch)
                                     (seq-take (plist-get context :view) 2))
                          (list canvas-3d--zoom))))
           (fields (list (cons (plist-get context :reference) (mapcar #'number-to-string view))
                         (list target))))
      (apply #'gnosis-model-resolve fields)
      (setf (plist-get context :result) fields)
      (exit-recursive-edit))))

(defun gnosis-model--canvas-size ()
  "Return a canvas size fitting the selected window, reserving header space."
  (let ((size (min 768 (window-body-width nil t)
                   (- (window-body-height nil t) (* 2 (frame-char-height))))))
    (when (< size 128) (user-error "Enlarge the window to display a model canvas"))
    size))

(defun gnosis-model--read-visual (hypothesis answer owner)
  "Read target and camera visually from HYPOTHESIS, ANSWER and OWNER context."
  (let* ((resolved (gnosis-model-resolve hypothesis answer))
         (initial (plist-get owner :initial))
         (context (append owner
                          (list :reference (car hypothesis) :view (cadr resolved) :depth (recursion-depth)
                                :objects (alist-get 'objects (gnosis-model--scene (car resolved)))
                                :target (and initial (car answer)) :process nil
                                :result nil :cancelled nil)))
         viewer)
    (save-window-excursion
      (unwind-protect
          (progn
            (let ((display-buffer-overriding-action '(display-buffer-same-window)))
              (setq viewer (gnosis-model-open (car resolved) (cadr resolved)
                                               (gnosis-model--canvas-size))))
            (with-current-buffer viewer
              (setq-local gnosis-model--author-context context)
              (setf (plist-get context :process) canvas-3d--process)
              (use-local-map (copy-keymap (current-local-map)))
              (local-set-key (kbd "RET") #'gnosis-model-author-accept)
              (local-set-key (kbd "q") #'gnosis-model-author-cancel)
              (local-set-key (kbd "C-g") #'gnosis-model-author-cancel)
              (setq-local header-line-format '(:eval (gnosis-model--author-header)))
              (add-hook 'canvas-3d-selection-hook #'gnosis-model--author-selection nil t)
              (add-hook 'kill-buffer-hook #'gnosis-model-author-cancel nil t)
              (add-hook 'change-major-mode-hook #'gnosis-model-author-cancel nil t)
              (when initial
                (setq canvas-3d-selected-id (car answer))
                (canvas-3d--request))
              (goto-char (point-min)))
            (recursive-edit)
            (unless (and (not (plist-get context :cancelled)) (plist-get context :result))
              (user-error "Model authoring cancelled"))
            (gnosis-model--author-check context)
            (plist-get context :result))
        (when (buffer-live-p viewer)
          (with-current-buffer viewer (setq gnosis-model--author-context nil))
          (kill-buffer viewer))))))

;;;###autoload
(defun gnosis-add-model-thema ()
  "Import objects and visually choose a target and view for a new thema.
With a prefix argument, use advanced numeric input instead of the canvas."
  (interactive)
  (when (get-buffer "*Gnosis NEW*") (user-error "Finish the existing draft first"))
  (pcase-let ((`(,hypothesis ,answer) (gnosis-model--read-fields)))
    (gnosis-add-thema "model" nil
                      (mapconcat #'identity hypothesis gnosis-export-separator)
                      (car answer))))

(defun gnosis-model-attach ()
  "Attach a scene to the single model thema in the current authoring buffer.
Preserve all draft text on prompt cancellation or an outdated draft.
Offer to reframe the current scene without importing it again.
With a prefix argument, use advanced numeric input instead of the canvas."
  (interactive nil gnosis-edit-mode)
  (let* ((owner (current-buffer))
         (mode major-mode)
         (tick (buffer-chars-modified-tick))
         (themata (gnosis-export-parse-themata))
         (thema (car themata)))
    (unless (and (= (length themata) 1) (equal (downcase (nth 1 thema)) "model"))
      (user-error "Attach a scene in a single model thema draft"))
    (pcase-let ((`(,hypothesis ,answer)
                 (gnosis-model--read-fields
                  (when (= (length (nth 3 thema)) 4)
                    (list (nth 3 thema) (nth 4 thema))))))
      (unless (and (buffer-live-p owner)
                   (with-current-buffer owner
                     (and (eq mode major-mode)
                          (= tick (buffer-chars-modified-tick)))))
        (user-error "Model draft changed during attachment"))
      (with-current-buffer owner
        (atomic-change-group
          (let ((inhibit-read-only t))
            (erase-buffer)
            (gnosis-export--insert-thema
             (nth 0 thema) "model" (nth 2 thema)
             (mapconcat #'identity hypothesis gnosis-export-separator)
             (car answer) (nth 5 thema) (nth 6 thema))
            (goto-char (point-min))))))))

(defun gnosis-model--renderer-directory ()
  "Find the optional backend without loading it or installing dependencies."
  (or (and gnosis-model-renderer-directory
           (expand-file-name gnosis-model-renderer-directory))
      (when-let* ((library (locate-library "canvas-3d")))
        (file-name-directory library))
      (let ((bundled (expand-file-name "../optional/canvas-3d" gnosis-model--directory)))
        (when (file-readable-p (expand-file-name "canvas-3d.el" bundled)) bundled))))

(defun gnosis-model-open (path view &optional size inline)
  "Open validated scene PATH at VIEW using the optional canvas backend.
SIZE defaults to 512 pixels; callers with an owned layout may pass its actual
available size.  INLINE attaches at point, preserving the current buffer.
Never install dependencies or use the network on opening."
  (let* ((directory (gnosis-model--renderer-directory))
         (load-path (if directory (cons directory load-path) load-path)))
    (when (and directory (file-remote-p directory))
      (user-error "The model renderer must be installed locally"))
    (unless (require 'canvas-3d nil t)
      (user-error "Model unavailable: install optional/canvas-3d or set gnosis-model-renderer-directory"))
    (let ((canvas-3d--directory (or directory canvas-3d--directory)))
      (unless (and (display-graphic-p) (fboundp 'canvas-refresh)
                   (image-type-available-p 'canvas))
        (user-error "3D requires graphical GNU Emacs built with native canvas image support; ordinary Gnosis does not"))
      (condition-case nil (canvas-3d--python)
        (user-error
         (user-error "Model renderer dependencies missing: run uv sync --locked --project %s (OpenGL/EGL required)"
                     (shell-quote-argument canvas-3d--directory))))
      (if inline
          (progn
            (unless (fboundp 'canvas-3d-attach)
              (user-error "Update the optional canvas backend for inline review"))
            (canvas-3d-attach path "Gnosis model" view (or size 512)))
        (canvas-3d-open path "Gnosis model" view (or size 512))))))

(provide 'gnosis-model)
;;; gnosis-model.el ends here
