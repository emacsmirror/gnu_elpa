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

(require 'gnosis-db)
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
(defvar canvas-3d--directory)
(defvar gnosis-export-separator)

(defcustom gnosis-model-renderer-directory nil
  "Optional directory containing canvas-3d.el, render.py and .venv.
The external canvas-3d package must implement scene opening and the
selection hook protocol.  Nil uses the package found on `load-path'."
  :type '(choice (const nil) directory)
  :group 'gnosis)

(defun gnosis-model--root ()
  "Return the asset root beside the currently open data database."
  (expand-file-name
   "assets" (file-name-directory
             (nth 2 (seq-find (lambda (row) (equal (nth 1 row) "main"))
                              (sqlite-select (gnosis--ensure-db)
                                             "PRAGMA database_list"))))))

(defun gnosis-model--file (directory name)
  "Return regular local file NAME confined to DIRECTORY.
Reject symlinks, absolute paths and parent traversal."
  (unless (and (stringp name)
               (string-match-p "\\`[[:alnum:]_-][[:alnum:]_.-]*\\'" name)
               (not (member name '("." ".."))))
    (user-error "Model files must have simple relative names"))
  (let ((file (expand-file-name name directory)))
    (unless (and (not (file-remote-p file)) (not (file-symlink-p file))
                 (file-regular-p file) (file-readable-p file))
      (user-error "Model file unavailable: %s" name))
    file))

(defun gnosis-model--hash (file)
  "Return the SHA256 of the literal bytes in FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

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
                         (gnosis-model--file directory (alist-get 'path object))))
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
  "Return validated resource digest for DIRECTORY."
  (let* ((file (gnosis-model--file directory "scene.json"))
         (scene (gnosis-model--scene file))
         (names (sort (delete-dups
                       (cons "scene.json"
                             (mapcar (lambda (o) (alist-get 'path o))
                                     (alist-get 'objects scene)))) #'string<)))
    (secure-hash
     'sha256 (gnosis-sqlite--serialize
              (mapcar (lambda (name)
                        (list name (gnosis-model--hash
                                    (gnosis-model--file directory name)))) names)))))

(defun gnosis-model-import (file &optional license source)
  "Import local scene FILE into managed assets and return its reference.
Optional LICENSE and SOURCE supply missing provenance.  Stage all files,
validate their bytes, then publish by rename.  Retry shares identical resources.
Failure or quit removes only the unpublished staging directory."
  (let* ((file (expand-file-name file))
         (scene (gnosis-model--scene file license source))
         (root (gnosis-model--root))
         (directory (file-name-directory file)))
    (when (file-symlink-p root) (user-error "Asset root must not be a symlink"))
    (make-directory root t)
    (let ((stage (make-temp-file (expand-file-name ".import-" root) t)))
      (unwind-protect
          (progn
            (dolist (name (delete-dups
                          (mapcar (lambda (o) (alist-get 'path o))
                                  (alist-get 'objects scene))))
              (let* ((source-file (gnosis-model--file directory name))
                     (hash (gnosis-model--hash source-file))
                     (dest (expand-file-name name stage)))
                (copy-file source-file dest)
                (unless (equal hash (gnosis-model--hash dest))
                  (user-error "Model changed during import"))))
            (let ((json-encoding-pretty-print nil)
                  (json-encoding-separator ","))
              (with-temp-file (expand-file-name "scene.json" stage)
                (insert (json-encode scene))))
            (let* ((revision (gnosis-model--revision stage))
                   (destination (expand-file-name revision root)))
              (if (file-exists-p destination)
                  (unless (and (not (file-symlink-p destination))
                               (equal revision (gnosis-model--revision destination)))
                    (user-error "Existing model revision is corrupt"))
                (rename-file stage destination))
              (concat revision "/scene.json")))
        (when (file-directory-p stage) (delete-directory stage t))))))

(defun gnosis-model-resolve (hypothesis answer)
  "Validate model HYPOTHESIS and ANSWER; return scene path and camera.
HYPOTHESIS is (RESOURCE YAW PITCH ZOOM), all strings.  ANSWER contains one
stable target ID.  Refuse missing or changed resources, never score them."
  (unless (and (proper-list-p hypothesis) (= (length hypothesis) 4)
               (seq-every-p #'stringp hypothesis)
               (string-match-p "\\`[0-9a-f]\\{64\\}/scene\\.json\\'" (car hypothesis))
               (proper-list-p answer) (= (length answer) 1) (stringp (car answer)))
    (user-error "Invalid model resource reference or target"))
  (let* ((root (gnosis-model--root))
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

(defun gnosis-model--read-fields ()
  "Read a local scene, target and starting camera; return authoring fields."
  (let* ((db (gnosis--ensure-db))
         (file (read-file-name "Model scene JSON: " nil nil t))
         (license (read-string "License and attribution (blank uses manifest): "))
         (source (read-string "Source/provenance (blank uses manifest): "))
         (scene (gnosis-model--scene (expand-file-name file)
                                     (unless (string-empty-p license) license)
                                     (unless (string-empty-p source) source)))
         (reference (progn
                      (unless (eq db (gnosis--ensure-db))
                        (user-error "Authoring database changed"))
                      (gnosis-model-import file (alist-get 'license scene)
                                           (alist-get 'source scene))))
         (scene (gnosis-model--scene (expand-file-name reference (gnosis-model--root))))
         (choices (mapcar (lambda (o)
                           (cons (format "%s (%s)" (alist-get 'label o) (alist-get 'id o))
                                 (alist-get 'id o))) (alist-get 'objects scene)))
         (target (cdr (assoc (completing-read "Expected target: " choices nil t) choices)))
         (view (gnosis-model--view
                (cl-mapcar (lambda (label value) (read-number label value))
                           '("Starting yaw: " "Starting pitch: " "Starting zoom: ")
                           (alist-get 'initial_view scene)))))
    (unless (eq db (gnosis--ensure-db)) (user-error "Authoring database changed"))
    (let ((hypothesis (cons reference (mapcar #'number-to-string view))))
      (gnosis-model-resolve hypothesis (list target))
      (list hypothesis (list target)))))

;;;###autoload
(defun gnosis-add-model-thema ()
  "Import a scene, choose target and camera, then open a normal thema draft."
  (interactive)
  (when (get-buffer "*Gnosis NEW*") (user-error "Finish the existing draft first"))
  (pcase-let ((`(,hypothesis ,answer) (gnosis-model--read-fields)))
    (gnosis-add-thema "model" nil
                      (mapconcat #'identity hypothesis gnosis-export-separator)
                      (car answer))))

(defun gnosis-model-attach ()
  "Attach a scene to the single model thema in the current authoring buffer.
Preserve all draft text on prompt cancellation or an outdated draft."
  (interactive nil gnosis-edit-mode)
  (let* ((owner (current-buffer))
         (mode major-mode)
         (tick (buffer-chars-modified-tick))
         (themata (gnosis-export-parse-themata))
         (thema (car themata)))
    (unless (and (= (length themata) 1) (equal (downcase (nth 1 thema)) "model"))
      (user-error "Attach a scene in a single model thema draft"))
    (pcase-let ((`(,hypothesis ,answer) (gnosis-model--read-fields)))
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

(defun gnosis-model-open (path view)
  "Open validated scene PATH at VIEW using the optional canvas backend."
  (let ((load-path (if gnosis-model-renderer-directory
                       (cons gnosis-model-renderer-directory load-path) load-path)))
    (unless (require 'canvas-3d nil t)
      (user-error "Model unavailable: install the optional canvas-3d backend")))
  (let ((canvas-3d--directory (or gnosis-model-renderer-directory canvas-3d--directory)))
    (canvas-3d-open path "Gnosis model" view 512)))

(provide 'gnosis-model)
;;; gnosis-model.el ends here
