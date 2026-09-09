;;; gnosis-image.el --- Managed raster images and regions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Immutable PNG/JPEG resources, portable Org links, and a native SVG region
;; editor.  Header validation is independent of graphical Emacs; displaying
;; additionally requires a successful native raster decode.  No external tools.

;;; Code:

(require 'gnosis-assets)
(require 'json)
(require 'org)
(require 'svg)
(require 'image)
(declare-function image-size "image.c" (spec &optional pixels frame))
(require 'seq)
(require 'subr-x)

(declare-function gnosis-add-thema "gnosis")
(declare-function gnosis-model-attach "gnosis-model")
(declare-function gnosis-add-thema--assert-common "gnosis")
(declare-function gnosis-add-thema--dispatch "gnosis")
(declare-function gnosis-export-parse-themata "gnosis-export-import")
(declare-function gnosis-export--insert-thema "gnosis-export-import")

(defconst gnosis-image--link-regexp
  "\\[\\[gnosis-image:\\([^]\n]+\\)\\]\\(?:\\[[^]\n]*\\]\\)?\\]"
  "Portable managed image Org link syntax.")

(defun gnosis-image-references (value)
  "Return managed references in content VALUE, recursively scanning lists.
Reject malformed managed links rather than silently displaying missing media."
  (cond
   ((stringp value)
    (let ((start 0) references)
      (while (string-match "gnosis-image:" value start)
        (let ((begin (- (match-beginning 0) 2)))
          (unless (and (>= begin 0)
                       (string-match gnosis-image--link-regexp value begin)
                       (= (match-beginning 0) begin))
            (user-error "Malformed managed image link"))
          (push (match-string 1 value) references)
          (setq start (match-end 0))))
      (nreverse references)))
   ((consp value) (mapcan #'gnosis-image-references value))))

(defun gnosis-image-content-p (value)
  "Return t if VALUE has managed image syntax, including malformed links."
  (if (stringp value) (string-match-p "gnosis-image:" value)
    (and (consp value) (seq-some #'gnosis-image-content-p value))))

(defun gnosis-image--uint (bytes offset count)
  "Read unsigned big-endian integer from BYTES at OFFSET using COUNT bytes."
  (when (> (+ offset count) (length bytes)) (user-error "Truncated image header"))
  (cl-loop for i from offset below (+ offset count)
           for value = (aref bytes i) then (+ (* value 256) (aref bytes i))
           finally return value))

(defun gnosis-image--dimensions (file)
  "Return (WIDTH HEIGHT TYPE) from bounded local PNG or JPEG FILE headers.
This checks header structure, not compressed pixel validity.  Native display
must decode the payload separately.  Limit pixels to 40 million and 16384/axis."
  (let* ((file (gnosis-assets-file (file-name-directory file) (file-name-nondirectory file)))
         (size (file-attribute-size (file-attributes file)))
         (bytes (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (when (> size (* 32 1024 1024)) (user-error "Image exceeds 32 MiB"))
                  (insert-file-contents-literally file)
                  (buffer-string)))
         (dimensions
          (cond
           ((and (>= size 33) (equal (substring bytes 0 8) "\211PNG\r\n\032\n")
                 (= (gnosis-image--uint bytes 8 4) 13)
                 (equal (substring bytes 12 16) "IHDR")
                 (memq (aref bytes 24)
                       (alist-get (aref bytes 25)
                                  '((0 1 2 4 8 16) (2 8 16) (3 1 2 4 8)
                                    (4 8 16) (6 8 16))))
                 (= (aref bytes 26) 0) (= (aref bytes 27) 0)
                 (memq (aref bytes 28) '(0 1)))
            (list (gnosis-image--uint bytes 16 4) (gnosis-image--uint bytes 20 4) 'png))
           ((and (> size 4) (= (aref bytes 0) 255) (= (aref bytes 1) 216))
            (let ((offset 2) found)
              (while (and (< offset size) (not found))
                (unless (= (aref bytes offset) 255) (user-error "Invalid JPEG marker"))
                (while (and (< offset size) (= (aref bytes offset) 255))
                  (setq offset (1+ offset)))
                (when (>= offset size) (user-error "Truncated JPEG marker"))
                (let* ((marker (aref bytes offset))
                       (length (gnosis-image--uint bytes (1+ offset) 2)))
                  (unless (and (>= length 2) (<= (+ offset 1 length) size)
                               (not (memq marker '(217 218))))
                    (user-error "JPEG lacks a supported frame header"))
                  (when (memq marker '(192 194))
                    (unless (and (>= length 11) (= (aref bytes (+ offset 3)) 8)
                                 (memq (aref bytes (+ offset 8)) '(1 3))
                                 (= length (+ 8 (* 3 (aref bytes (+ offset 8))))))
                      (user-error "Use an 8-bit grayscale or RGB JPEG"))
                    (setq found (list (gnosis-image--uint bytes (+ offset 6) 2)
                                      (gnosis-image--uint bytes (+ offset 4) 2) 'jpeg)))
                  (setq offset (+ offset 1 length))))
              found)))))
    (unless (and dimensions (<= 1 (car dimensions) 16384)
                 (<= 1 (cadr dimensions) 16384)
                 (<= (* (car dimensions) (cadr dimensions)) 40000000))
      (user-error "Use a bounded PNG or grayscale/RGB JPEG image"))
    dimensions))

(defun gnosis-image--regions (regions)
  "Validate and return REGIONS with stable IDs, labels and normalized rectangles."
  (unless (and (proper-list-p regions) (<= (length regions) 255)
               (seq-every-p
                (lambda (region)
                  (let ((id (alist-get 'id region)) (label (alist-get 'label region))
                        (rect (alist-get 'rect region)))
                    (and (stringp id) (string-match-p "\\`[a-zA-Z0-9_-]+\\'" id)
                         (<= (length id) 80) (stringp label)
                         (<= 1 (length (string-trim label)) 200)
                         (proper-list-p rect) (= (length rect) 4)
                         (seq-every-p (lambda (n) (and (numberp n) (<= 0 n 1))) rect)
                         (> (nth 2 rect) 0) (> (nth 3 rect) 0)
                         (<= (+ (nth 0 rect) (nth 2 rect)) 1)
                         (<= (+ (nth 1 rect) (nth 3 rect)) 1)))) regions)
               (= (length regions)
                  (length (delete-dups (mapcar (lambda (r) (alist-get 'id r)) regions)))))
    (user-error "Image requires unique labelled regions within the image"))
  regions)

(defun gnosis-image--manifest (manifest directory)
  "Validate MANIFEST and its raster in DIRECTORY; return MANIFEST."
  (let* ((file (gnosis-assets-file directory (alist-get 'file manifest)))
         (dimensions (gnosis-image--dimensions file)))
    (unless (and (equal (alist-get 'version manifest) 1)
                 (equal (alist-get 'width manifest) (car dimensions))
                 (equal (alist-get 'height manifest) (cadr dimensions))
                 (seq-every-p (lambda (key)
                                (let ((text (alist-get key manifest)))
                                  (and (stringp text) (<= (length text) 4000))))
                              '(source attribution)))
      (user-error "Invalid image manifest version, dimensions or metadata"))
    (gnosis-image--regions (alist-get 'regions manifest))
    manifest))

(defun gnosis-image-resolve (reference &optional target)
  "Return validated manifest for managed REFERENCE, optionally requiring TARGET.
Include private `directory' and `path' entries for the verified local payload."
  (unless (and (stringp reference)
               (string-match-p "\\`[0-9a-f]\\{64\\}/image\\.json\\'" reference))
    (user-error "Invalid managed image reference"))
  (let* ((root (gnosis-assets-root))
         (revision (substring reference 0 64))
         (directory (expand-file-name revision root))
         (file (gnosis-assets-file directory "image.json")))
    (when (> (file-attribute-size (file-attributes file)) 131072)
      (user-error "Image manifest exceeds 128 KiB"))
    (let ((manifest (json-parse-string
                     (with-temp-buffer (insert-file-contents file) (buffer-string))
                     :object-type 'alist :array-type 'list :null-object nil)))
      (gnosis-image--manifest manifest directory)
      (gnosis-assets-validate root revision (list "image.json" (alist-get 'file manifest)))
      (when (and target (not (seq-find (lambda (r) (equal target (alist-get 'id r)))
                                       (alist-get 'regions manifest))))
        (user-error "Image target is absent from its pinned resource"))
      (append manifest (list (cons 'directory directory)
                             (cons 'path (gnosis-assets-file directory (alist-get 'file manifest))))))))

(defun gnosis-image-import (file &optional regions source attribution)
  "Import PNG/JPEG FILE with REGIONS, SOURCE and ATTRIBUTION; return reference.
Metadata is optional and never guessed.  Identical imports share a revision."
  (let* ((file (expand-file-name file))
         (dimensions (gnosis-image--dimensions file))
         (manifest `((version . 1) (file . ,(file-name-nondirectory file))
                     (width . ,(car dimensions)) (height . ,(cadr dimensions))
                     (source . ,(or source "")) (attribution . ,(or attribution ""))
                     (regions . ,(vconcat (gnosis-image--regions regions)))))
         (json-encoding-pretty-print nil) (json-encoding-separator ",")
         (text (json-encode manifest)))
    (gnosis-image--manifest (cons (cons 'regions regions) (assq-delete-all 'regions (copy-tree manifest)))
                            (file-name-directory file))
    (when (> (string-bytes text) 131072) (user-error "Image manifest exceeds 128 KiB"))
    (concat (gnosis-assets-import (file-name-directory file) (list (file-name-nondirectory file))
                                  (list (cons "image.json" text))) "/image.json")))

(defun gnosis-image-validate-fields (type keimenon hypothesis answer parathema
                                          &optional review-image)
  "Validate TYPE, KEIMENON, HYPOTHESIS, ANSWER, PARATHEMA and REVIEW-IMAGE media."
  (mapc #'gnosis-image-resolve
        (gnosis-image-references (list keimenon hypothesis answer parathema review-image)))
  (when (member (downcase type) '("image-region" "image-occlusion"))
    (unless (and (proper-list-p hypothesis) (= (length hypothesis) 1)
                 (proper-list-p answer) (= (length answer) 1) (stringp (car answer)))
      (user-error "Image thema needs one resource and one target"))
    (gnosis-image-resolve (car hypothesis) (car answer))))

(defun gnosis-image--save (id type keimenon hypothesis answer parathema tags suspend links)
  "Save image ID of TYPE with validated content fields.
Validate KEIMENON, HYPOTHESIS, ANSWER, PARATHEMA, TAGS, SUSPEND and LINKS."
  (gnosis-add-thema--assert-common keimenon tags suspend links)
  (gnosis-image-validate-fields type keimenon hypothesis answer parathema)
  (gnosis-add-thema--dispatch id type keimenon hypothesis answer parathema tags suspend links))

(defun gnosis-image--decode (scene)
  "Decode SCENE raster natively, rejecting unsupported or corrupt pixels."
  (let* ((path (alist-get 'path scene))
         (type (nth 2 (gnosis-image--dimensions path)))
         (image (and (display-images-p) (image-type-available-p type)
                     (create-image path type nil)))
         (size (and image (image-size image t))))
    (unless (and size (equal (car size) (alist-get 'width scene))
                 (equal (cdr size) (alist-get 'height scene)))
      (user-error "Image unavailable: native PNG/JPEG decoding requires a graphical frame"))
    image))

(defun gnosis-image-format-string (text &optional window)
  "Return TEXT with managed image displays fitted to WINDOW, default selected."
  (let ((start 0) (result ""))
    (gnosis-image-references text)
    (while (string-match gnosis-image--link-regexp text start)
      (let* ((begin (match-beginning 0)) (end (match-end 0))
             (reference (match-string 1 text))
             (scene (gnosis-image-resolve reference))
             (image (gnosis-image--decode scene))
             (width (max 1 (- (window-body-width window t) 32))))
        (setq image (copy-tree image))
        (setcdr image (plist-put (cdr image) :max-width width))
        (setcdr image (plist-put (cdr image) :max-height
                                 (max 1 (- (window-body-height window t) 80))))
        (setq result (concat result (substring text start begin)
                             (propertize " " 'display image 'gnosis-image-reference reference))
              start end)))
    (concat result (substring text start))))

(defun gnosis-image-refresh ()
  "Resize inline managed image displays without changing text or point."
  (when-let* ((window (get-buffer-window (current-buffer))))
    (save-excursion
      (let ((position (point-min)))
        (while (< position (point-max))
          (let* ((end (next-single-property-change position 'gnosis-image-reference nil (point-max)))
                 (reference (get-text-property position 'gnosis-image-reference)))
            (when reference
              (let ((display (condition-case nil
                                 (get-text-property
                                  0 'display (gnosis-image-format-string
                                              (format "[[gnosis-image:%s]]" reference) window))
                               (error "[Image unavailable]"))))
                (with-silent-modifications
                  (put-text-property position end 'display display))))
            (setq position end)))))))

;;; Native owned viewer/editor

(defvar-local gnosis-image--scene nil "Immutable scene displayed in this buffer.")
(defvar-local gnosis-image--regions nil "Privately owned editable region list.")
(defvar-local gnosis-image--selection nil "Selected stable region ID, or nil.")
(defvar-local gnosis-image--purpose nil "One of edit, region or occlusion.")
(defvar-local gnosis-image--target nil "Occluded stable target ID.")
(defvar-local gnosis-image--revealed nil "Non-nil after explicit reveal.")
(defvar-local gnosis-image--size nil "Displayed width and height in pixels.")
(defvar-local gnosis-image--depth nil "Recursive input depth owned by this viewer.")
(defvar-local gnosis-image--accepted nil "Non-nil when explicit input finished.")
(defvar-local gnosis-image--check nil "Encounter validation function, or nil.")

(defun gnosis-image--svg (scene regions width height purpose target selection revealed)
  "Build SCENE and REGIONS SVG at WIDTH and HEIGHT.
PURPOSE, TARGET, SELECTION and REVEALED control neutral or hidden overlays."
  (let* ((svg (svg-create width height))
         (path (alist-get 'path scene))
         (type (nth 2 (gnosis-image--dimensions path)))
         (data (with-temp-buffer (set-buffer-multibyte nil)
                                 (insert-file-contents-literally path) (buffer-string))))
    (svg-embed svg data (if (eq type 'png) "image/png" "image/jpeg") t
               :x 0 :y 0 :width width :height height)
    (dolist (region regions)
      (pcase-let* ((`(,x ,y ,w ,h) (alist-get 'rect region))
                   (id (alist-get 'id region))
                   (hidden (and (eq purpose 'occlusion) (equal id target) (not revealed)))
                   (visible-label (or (eq purpose 'edit) revealed)))
        (when (or hidden visible-label (equal id selection))
          (svg-rectangle svg (* x width) (* y height) (* w width) (* h height)
                         :fill (if hidden "#202020" "none") :fill-opacity 1
                         :stroke (if (equal id selection) "#4080ff" "#808080") :stroke-width 2)
          (when visible-label
            (svg-text svg (alist-get 'label region) :x (+ 3 (* x width))
                      :y (+ 16 (* y height)) :font-size 14 :fill "#ffffff"
                      :stroke "#000000" :stroke-width 0.3)))))
    svg))

(defun gnosis-image--render (&rest _)
  "Render this owned viewer at its current window size."
  (when-let* ((window (get-buffer-window (current-buffer))) (scene gnosis-image--scene))
    (let* ((scale (min 1.0 (/ (float (max 1 (- (window-body-width window t) 16)))
                              (alist-get 'width scene))
                       (/ (float (max 1 (- (window-body-height window t) 120)))
                          (alist-get 'height scene))))
           (width (max 1 (floor (* scale (alist-get 'width scene)))))
           (height (max 1 (floor (* scale (alist-get 'height scene)))))
           (image (svg-image (gnosis-image--svg scene gnosis-image--regions width height
                                                gnosis-image--purpose gnosis-image--target
                                                gnosis-image--selection gnosis-image--revealed)))
           (inhibit-read-only t))
      (setq gnosis-image--size (cons width height))
      (erase-buffer)
      (when-let* ((prompt (alist-get 'prompt scene))) (insert prompt "\n\n"))
      (insert-image image)
      (goto-char (point-min))
      (setq header-line-format
            (pcase gnosis-image--purpose
              ('edit " Drag: draw and label | click: select | d: delete | RET: accept | q: cancel")
              ('region " Click to select (neutral) | RET: submit | q: cancel")
              (_ (if gnosis-image--revealed " Answer revealed | RET: continue | q: cancel"
                   " Recall hidden region | RET: reveal | q: cancel")))))))

(defun gnosis-image--position (position)
  "Return normalized coordinates for image event POSITION, or nil."
  (let ((xy (posn-object-x-y position)) (object (posn-object position)))
    (when (and (eq (posn-window position) (get-buffer-window (current-buffer)))
               (eq (car-safe object) 'image) xy gnosis-image--size
               (<= 0 (car xy) (car gnosis-image--size))
               (<= 0 (cdr xy) (cdr gnosis-image--size)))
      (cons (/ (float (car xy)) (car gnosis-image--size))
            (/ (float (cdr xy)) (cdr gnosis-image--size))))))

(defun gnosis-image--hit (regions xy)
  "Return first stable ID in REGIONS containing normalized XY."
  (when xy
    (alist-get 'id
               (seq-find (lambda (r)
                           (pcase-let ((`(,x ,y ,w ,h) (alist-get 'rect r)))
                             (and (<= x (car xy) (+ x w)) (<= y (cdr xy) (+ y h)))))
                         regions))))

(defun gnosis-image-select (event)
  "Select region at mouse EVENT without grading or revealing labels."
  (interactive "e")
  (setq gnosis-image--selection
        (gnosis-image--hit gnosis-image--regions (gnosis-image--position (event-start event))))
  (gnosis-image--render))

(defun gnosis-image-draw (event)
  "Draw and label one rectangle from drag EVENT in the region editor."
  (interactive "e")
  (unless (eq gnosis-image--purpose 'edit) (user-error "Not editing regions"))
  (let* ((start (gnosis-image--position (event-start event)))
         (end (gnosis-image--position (event-end event)))
         (owner (current-buffer)) (regions gnosis-image--regions))
    (unless (and start end (> (abs (- (car start) (car end))) 0.002)
                 (> (abs (- (cdr start) (cdr end))) 0.002))
      (user-error "Drag a rectangle inside the image"))
    (let* ((label (read-string "Region label: "))
           (id (cl-loop for n from 1 for candidate = (format "region-%d" n)
                        unless (seq-find (lambda (r) (equal candidate (alist-get 'id r))) regions)
                        return candidate))
           (region `((id . ,id) (label . ,label)
                     (rect . (,(min (car start) (car end)) ,(min (cdr start) (cdr end))
                              ,(abs (- (car start) (car end))) ,(abs (- (cdr start) (cdr end))))))))
      (unless (and (buffer-live-p owner) (eq (current-buffer) owner)
                   (eq regions gnosis-image--regions) (eq gnosis-image--purpose 'edit))
        (user-error "Image editor changed while labelling"))
      (setq gnosis-image--regions (gnosis-image--regions (append regions (list region)))
            gnosis-image--selection id)
      (gnosis-image--render))))

(defun gnosis-image-delete-region ()
  "Delete the selected rectangle from this edit, not from published resources."
  (interactive)
  (unless (and (eq gnosis-image--purpose 'edit) gnosis-image--selection)
    (user-error "Select a region in the editor first"))
  (setq gnosis-image--regions
        (seq-remove (lambda (r) (equal gnosis-image--selection (alist-get 'id r))) gnosis-image--regions)
        gnosis-image--selection nil)
  (gnosis-image--render))

(defun gnosis-image-submit ()
  "Accept edited regions, submit a selection, or explicitly reveal occlusion."
  (interactive)
  (unless (and gnosis-image--depth (= (recursion-depth) (1+ gnosis-image--depth)))
    (user-error "No active image input"))
  (when gnosis-image--check (funcall gnosis-image--check))
  (when (and (eq gnosis-image--purpose 'region) (not gnosis-image--selection))
    (user-error "Click a region before submitting"))
  (if (and (eq gnosis-image--purpose 'occlusion) (not gnosis-image--revealed))
      (progn (setq gnosis-image--revealed t) (gnosis-image--render))
    (setq gnosis-image--accepted t)
    (exit-recursive-edit)))

(defun gnosis-image-cancel ()
  "Cancel owned image input without returning an answer."
  (interactive)
  (when (and gnosis-image--depth (= (recursion-depth) (1+ gnosis-image--depth)))
    (abort-recursive-edit)))

(defvar-keymap gnosis-image-mode-map
  :parent special-mode-map
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'gnosis-image-select
  "<drag-mouse-1>" #'gnosis-image-draw
  "d" #'gnosis-image-delete-region
  "RET" #'gnosis-image-submit
  "q" #'gnosis-image-cancel
  "C-g" #'gnosis-image-cancel)

(define-derived-mode gnosis-image-mode special-mode "Gnosis Image"
  "Inspect or edit an owned native image; header line shows available actions."
  (setq-local cursor-type nil)
  (add-hook 'window-configuration-change-hook #'gnosis-image--render nil t)
  (add-hook 'kill-buffer-hook #'gnosis-image-cancel nil t)
  (add-hook 'change-major-mode-hook #'gnosis-image-cancel nil t))

(defun gnosis-image-input (scene purpose &optional target check)
  "Present SCENE for PURPOSE and return (REGIONS SELECTION).
TARGET identifies occlusion.  CHECK revalidates encounter ownership on submit.
Restore the original layout and destroy only the owned viewer on every exit."
  (gnosis-image--decode scene)
  (unless (image-type-available-p 'svg) (user-error "Native SVG support is required"))
  (let ((buffer (generate-new-buffer "*Gnosis Image*")))
    (save-window-excursion
      (unwind-protect
          (progn
            (pop-to-buffer-same-window buffer)
            (delete-other-windows)
            (gnosis-image-mode)
            (setq gnosis-image--scene (copy-tree scene)
                  gnosis-image--regions (copy-tree (alist-get 'regions scene))
                  gnosis-image--purpose purpose gnosis-image--target target
                  gnosis-image--depth (recursion-depth) gnosis-image--check check)
            (gnosis-image--render)
            (recursive-edit)
            (unless (and (buffer-live-p buffer)
                         (with-current-buffer buffer gnosis-image--accepted))
              (user-error "Image input cancelled"))
            (with-current-buffer buffer
              (list (copy-tree gnosis-image--regions) gnosis-image--selection)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (setq gnosis-image--depth nil))
          (kill-buffer buffer))))))

(defun gnosis-image-edit-regions (scene)
  "Edit SCENE rectangles graphically and return accepted plain region values."
  (car (gnosis-image-input scene 'edit)))

(defun gnosis-image--read-resource (&optional reference regions-p)
  "Read or reuse REFERENCE, optionally editing regions when REGIONS-P.
Return immutable managed reference.  Prompts never assign guessed provenance."
  (let* ((database (gnosis--ensure-db))
         (old (and reference (gnosis-image-resolve reference)))
         (reuse (and old (y-or-n-p "Reuse this image? ")))
         (file (if reuse (alist-get 'path old) (read-file-name "PNG or JPEG: " nil nil t)))
         (source (read-string "Source (optional): " (and reuse (alist-get 'source old))))
         (attribution (read-string "Attribution/license (optional): "
                                   (and reuse (alist-get 'attribution old))))
         (pinned (progn
                   (gnosis-assets-root database)
                   (gnosis-image-import file (and reuse (alist-get 'regions old)) source attribution)))
         (scene (gnosis-image-resolve pinned))
         (regions (if regions-p (gnosis-image-edit-regions scene) (alist-get 'regions scene))))
    (gnosis-assets-root database)
    ;; Recursive editing can outlive the pixels and manifest it displayed.
    (gnosis-image-resolve pinned)
    (gnosis-image-import (alist-get 'path scene) regions source attribution)))

(defun gnosis-image--read-fields (&optional reference)
  "Read graphical image fields, optionally reusing REFERENCE."
  (let* ((database (gnosis--ensure-db))
         (reference (gnosis-image--read-resource reference t))
         (regions (alist-get 'regions (gnosis-image-resolve reference)))
         (choices (mapcar (lambda (r) (cons (format "%s (%s)" (alist-get 'label r) (alist-get 'id r))
                                            (alist-get 'id r))) regions)))
    (unless choices (user-error "Draw at least one labelled region"))
    (let ((target (cdr (assoc (completing-read "Expected region: " choices nil t) choices))))
      (unless target (user-error "Choose an expected region"))
      (gnosis-assets-root database)
      (gnosis-image-resolve reference target)
      (list (list reference) (list target)))))

;;;###autoload
(defun gnosis-add-image-thema (&optional type)
  "Draw regions and open a normal image thema draft of TYPE.
TYPE is image-region or image-occlusion; interactively choose between them."
  (interactive)
  (let ((type (or type (completing-read "Image type: " '("image-region" "image-occlusion") nil t))))
    (unless (member type '("image-region" "image-occlusion")) (user-error "Invalid image type"))
    (when (get-buffer "*Gnosis NEW*") (user-error "Finish the existing draft first"))
    (pcase-let ((`(,hypothesis ,answer) (gnosis-image--read-fields)))
      (gnosis-add-thema type nil (car hypothesis) (car answer)))))

(defun gnosis-image-attach ()
  "Attach a managed image at point, or edit a single media thema resource.
Delegate model drafts to their own attachment command.  Preserve draft text
and database ownership through all prompts and cancellation."
  (interactive nil gnosis-edit-mode)
  (let* ((owner (current-buffer)) (tick (buffer-chars-modified-tick))
         (position (point)) (database (gnosis--ensure-db))
         (themata (gnosis-export-parse-themata)) (thema (car themata))
         (type (downcase (or (nth 1 thema) "")))
         (image-p (member type '("image-region" "image-occlusion"))))
    (unless (= (length themata) 1) (user-error "Attach in a single thema draft"))
    (if (equal type "model")
        (gnosis-model-attach)
      (unless image-p
        (unless (member (save-excursion (org-back-to-heading t) (org-get-heading t t t t))
                        '("Keimenon" "Hypothesis" "Answer" "Parathema"))
          (user-error "Attach inside a question, hint, answer or explanation")))
      (let ((value (if image-p (gnosis-image--read-fields (car (nth 3 thema)))
                     (gnosis-image--read-resource))))
        (gnosis-assets-root database)
        (unless (and (buffer-live-p owner)
                     (with-current-buffer owner
                       (and (derived-mode-p 'gnosis-edit-mode) (= tick (buffer-chars-modified-tick)))))
          (user-error "Image draft changed during attachment"))
        (with-current-buffer owner
          (atomic-change-group
            (if image-p
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (gnosis-export--insert-thema (nth 0 thema) type (nth 2 thema)
                                               (caar value) (caadr value) (nth 5 thema) (nth 6 thema))
                  (goto-char (point-min)))
              (goto-char position)
              (insert (format "[[gnosis-image:%s]]" value)))))))))

(provide 'gnosis-image)
;;; gnosis-image.el ends here
