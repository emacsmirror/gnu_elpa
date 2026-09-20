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

(defun gnosis-image--source-bytes (file)
  "Return bounded bytes of local regular raster FILE, allowing any basename."
  (let ((file (gnosis-assets--local-path file)))
    (gnosis-assets--local-path (file-name-directory file))
    (unless (and (file-regular-p file) (file-readable-p file))
      (user-error "Image file unavailable: %s" file))
    (when (> (file-attribute-size (file-attributes file)) (* 32 1024 1024))
      (user-error "Image exceeds 32 MiB"))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (when (> (buffer-size) (* 32 1024 1024)) (user-error "Image exceeds 32 MiB"))
      (buffer-string))))

(defun gnosis-image--dimensions (file)
  "Return (WIDTH HEIGHT TYPE) from bounded local PNG or JPEG FILE headers.
This checks header structure, not compressed pixel validity.  Native display
must decode the payload separately.  Limit pixels to 40 million and 16384/axis."
  (let* ((bytes (gnosis-image--source-bytes file))
         (size (length bytes))
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

(defun gnosis-image--region-rectangles (region)
  "Return rectangles belonging to legacy or plural REGION, without mutation."
  (if (assq 'rects region) (alist-get 'rects region)
    (list (alist-get 'rect region))))

(defun gnosis-image--rectangle-p (rect)
  "Return non-nil if RECT is a bounded normalized rectangle."
  (and (proper-list-p rect) (= (length rect) 4)
       (seq-every-p (lambda (n) (and (numberp n) (<= 0 n 1))) rect)
       (> (nth 2 rect) 0) (> (nth 3 rect) 0)
       (<= (+ (nth 0 rect) (nth 2 rect)) 1)
       (<= (+ (nth 1 rect) (nth 3 rect)) 1)))

(defun gnosis-image--regions (regions &optional version)
  "Validate and return REGIONS, optionally requiring manifest VERSION geometry."
  (unless
      (and (proper-list-p regions) (<= (length regions) 255)
           (seq-every-p
            (lambda (region)
              (and (proper-list-p region) (seq-every-p #'consp region)
                   (let ((id (alist-get 'id region)) (label (alist-get 'label region))
                         (keys (mapcar #'car region)))
                     (and (= (length keys) (length (delete-dups (copy-sequence keys))))
                          (stringp id) (string-match-p "\\`[a-zA-Z0-9_-]+\\'" id)
                          (<= (length id) 80) (stringp label)
                          (<= 1 (length (string-trim label)) 200)
                          (not (eq (not (memq 'rect keys)) (not (memq 'rects keys))))
                          (pcase version (1 (memq 'rect keys)) (2 (memq 'rects keys)) (_ t))
                          (let ((rects (gnosis-image--region-rectangles region)))
                            (and (proper-list-p rects) rects
                                 (seq-every-p #'gnosis-image--rectangle-p rects))))))) regions)
           (<= (apply #'+ (mapcar (lambda (r) (length (gnosis-image--region-rectangles r))) regions)) 255)
           (= (length regions)
              (length (delete-dups (mapcar (lambda (r) (alist-get 'id r)) regions)))))
    (user-error "Image requires unique labelled targets and at most 255 rectangles"))
  regions)

(defun gnosis-image-target (scene target)
  "Return TARGET alist in validated SCENE, or signal an absent target."
  (or (seq-find (lambda (r) (equal target (alist-get 'id r)))
                (alist-get 'regions scene))
      (user-error "Image target is absent from its pinned resource")))

(defun gnosis-image--manifest (manifest directory)
  "Validate MANIFEST and its raster in DIRECTORY; return MANIFEST."
  (let* ((file (gnosis-assets-file directory (alist-get 'file manifest)))
         (dimensions (gnosis-image--dimensions file)))
    (unless (and (memq (alist-get 'version manifest) '(1 2))
                 (equal (alist-get 'width manifest) (car dimensions))
                 (equal (alist-get 'height manifest) (cadr dimensions))
                 (seq-every-p (lambda (key)
                                (let ((text (alist-get key manifest)))
                                  (and (stringp text) (<= (length text) 4000))))
                              '(source attribution)))
      (user-error "Invalid image manifest version, dimensions or metadata"))
    (gnosis-image--regions (alist-get 'regions manifest) (alist-get 'version manifest))
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
      (append (list (cons 'directory directory)
                    (cons 'path (gnosis-assets-file directory (alist-get 'file manifest))))
              (seq-remove (lambda (entry) (memq (car entry) '(directory path))) manifest)))))

(defun gnosis-image-import (file &optional regions source attribution)
  "Import PNG/JPEG FILE with REGIONS, SOURCE and ATTRIBUTION; return reference.
Metadata is optional and never guessed.  Identical imports share a revision.
External filenames need not satisfy the managed asset basename grammar."
  (let* ((file (expand-file-name file))
         (name (condition-case nil (gnosis-assets--name (file-name-nondirectory file))
                 (user-error nil))))
    (if (and name (not (equal name "image.json")))
        (gnosis-image--import file regions source attribution)
      (let* ((database (gnosis--ensure-db))
             (dimensions (gnosis-image--dimensions file))
             (hash (secure-hash 'sha256 (gnosis-image--source-bytes file)))
             (stage (make-temp-file "gnosis-image-source-" t))
             (copy (expand-file-name (if (eq (nth 2 dimensions) 'png) "image.png" "image.jpg") stage)))
        (unwind-protect
            (progn
              (copy-file file copy)
              (unless (and (equal hash (gnosis-assets-hash copy))
                           (equal hash (secure-hash 'sha256 (gnosis-image--source-bytes file))))
                (user-error "Image source changed during copying"))
              (gnosis-assets-root database)
              (gnosis-image--import copy regions source attribution))
          (delete-directory stage t))))))

(defun gnosis-image--import (file regions source attribution)
  "Publish confined raster FILE with REGIONS, SOURCE and ATTRIBUTION."
  (let* ((file (expand-file-name file))
         (dimensions (gnosis-image--dimensions file))
         (plural (seq-some (lambda (r) (assq 'rects r)) (gnosis-image--regions regions)))
         (regions (if plural
                      (mapcar (lambda (r)
                                `((id . ,(alist-get 'id r)) (label . ,(alist-get 'label r))
                                  (rects . ,(gnosis-image--region-rectangles r)))) regions)
                    regions))
         (manifest `((version . ,(if plural 2 1)) (file . ,(file-name-nondirectory file))
                     (width . ,(car dimensions)) (height . ,(cadr dimensions))
                     (source . ,(or source "")) (attribution . ,(or attribution ""))
                     (regions . ,(vconcat
                                  (if plural
                                      (mapcar (lambda (r)
                                                `((id . ,(alist-get 'id r))
                                                  (label . ,(alist-get 'label r))
                                                  (rects . ,(vconcat (mapcar #'vconcat
                                                                             (alist-get 'rects r))))))
                                              regions)
                                    regions)))))
         (json-encoding-pretty-print nil) (json-encoding-separator ",")
         (text (json-encode manifest)))
    (gnosis-image--manifest (cons (cons 'regions regions) (assq-delete-all 'regions (copy-tree manifest)))
                            (file-name-directory file))
    (when (> (string-bytes text) 131072) (user-error "Image manifest exceeds 128 KiB"))
    (concat (gnosis-assets-import (file-name-directory file) (list (file-name-nondirectory file))
                                  (list (cons "image.json" text))) "/image.json")))

(defun gnosis-image-occlusion-policy (hypothesis)
  "Return effective visibility policy for occlusion HYPOTHESIS.
Only one/two-field legacy forms default to hide-target."
  (unless (and (proper-list-p hypothesis) (memq (length hypothesis) '(1 2 3))
               (seq-every-p #'stringp hypothesis)
               (or (< (length hypothesis) 3)
                   (member (nth 2 hypothesis) '("hide-target" "hide-all"))))
    (user-error "Invalid image occlusion visibility policy"))
  (if (= (length hypothesis) 3) (nth 2 hypothesis) "hide-target"))

(defun gnosis-image-occlusion-fields (hypothesis answer)
  "Return canonical occlusion (HYPOTHESIS ANSWER) fields.
HYPOTHESIS holds resource and stable target; ANSWER holds editable text.
Canonical fields need no media access, so unavailable resources remain editable.
Only historical resource-only hypotheses derive text from the pinned label.
Use `gnosis-image-validate-fields' to validate resources before saving."
  (unless (and (proper-list-p hypothesis) (memq (length hypothesis) '(1 2 3))
               (seq-every-p #'stringp hypothesis)
               (proper-list-p answer) (= (length answer) 1)
               (stringp (car answer)) (not (string-empty-p (string-trim (car answer)))))
    (user-error "Occlusion needs resource, target and a nonempty text answer"))
  (let* ((policy (gnosis-image-occlusion-policy hypothesis))
         (target (if (cdr hypothesis) (cadr hypothesis) (car answer)))
         (text (if (cdr hypothesis) (car answer)
                 (condition-case nil
                     (alist-get 'label (gnosis-image-target
                                        (gnosis-image-resolve (car hypothesis) target) target))
                   (error (user-error "Restore the legacy image to recover its answer before replacement"))))))
    (list (list (car hypothesis) target policy) (list text))))

(defun gnosis-image-validate-fields (type keimenon hypothesis answer parathema
                                          &optional review-image)
  "Validate TYPE, KEIMENON, HYPOTHESIS, ANSWER, PARATHEMA and REVIEW-IMAGE media."
  (mapc #'gnosis-image-resolve
        (gnosis-image-references (list keimenon hypothesis answer parathema review-image)))
  (when (equal (downcase type) "image-occlusion")
    (let ((fields (gnosis-image-occlusion-fields hypothesis answer)))
      (gnosis-image-resolve (caar fields) (cadar fields))))
  (when (equal (downcase type) "image-region")
    (unless (and (proper-list-p hypothesis) (= (length hypothesis) 1)
                 (proper-list-p answer) (= (length answer) 1) (stringp (car answer)))
      (user-error "Image thema needs one resource and one target"))
    (gnosis-image-resolve (car hypothesis) (car answer))))

(cl-defun gnosis-image--save (id type keimenon hypothesis answer parathema tags suspend links
                                  &optional (accepted-aliases nil aliases-p))
  "Save image ID of TYPE with validated content fields.
Validate KEIMENON, HYPOTHESIS, ANSWER, PARATHEMA, TAGS, SUSPEND and LINKS.
Forward ACCEPTED-ALIASES only when supplied, preserving omitted updates."
  (gnosis-add-thema--assert-common keimenon tags suspend links)
  (gnosis-image-validate-fields type keimenon hypothesis answer parathema)
  (apply #'gnosis-add-thema--dispatch id type keimenon hypothesis answer parathema tags suspend links
         (when aliases-p (list accepted-aliases))))

(defun gnosis-image--decode (scene)
  "Decode SCENE raster natively, rejecting unsupported or corrupt pixels."
  (let* ((path (alist-get 'path scene))
         (type (nth 2 (gnosis-image--dimensions path)))
         (image (and (display-images-p) (image-type-available-p type)
                     (create-image path type nil :scale 1.0)))
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
                             (propertize " " 'display image
                                          'gnosis-display-layout 'independent
                                          'gnosis-image-reference reference))
              start end)))
    (concat result (substring text start))))

(defun gnosis-image-mask (scene target revealed &optional window policy)
  "Return SCENE hiding TARGET unless REVEALED, fitting WINDOW using POLICY."
  (setq policy (gnosis-image-occlusion-policy (list "" target (or policy "hide-target"))))
  (gnosis-image-target scene target)
  (gnosis-image--decode scene)
  (unless (image-type-available-p 'svg) (user-error "Native SVG support is required"))
  (let* ((scale (min 1.0 (/ (float (max 1 (- (window-body-width window t) 32)))
                            (alist-get 'width scene))
                     (/ (float (max 1 (- (window-body-height window t) 160)))
                        (alist-get 'height scene))))
         (width (max 1 (floor (* scale (alist-get 'width scene)))))
         (height (max 1 (floor (* scale (alist-get 'height scene))))))
    (propertize " " 'gnosis-image-mask (list scene target revealed policy)
                'display (svg-image (gnosis-image--svg
                                     scene (alist-get 'regions scene) width height
                                     'occlusion target nil revealed policy) :scale 1.0))))

(defun gnosis-image-refresh ()
  "Resize inline managed image displays without changing text or point."
  (when-let* ((window (get-buffer-window (current-buffer))))
    (save-excursion
      (let ((position (point-min)))
        (while (< position (point-max))
          (let* ((end (next-single-property-change position 'gnosis-image-mask nil (point-max)))
                 (mask (get-text-property position 'gnosis-image-mask)))
            (when mask
              (with-silent-modifications
                (put-text-property position end 'display
                                   (condition-case nil
                                       (get-text-property 0 'display
                                                          (gnosis-image-mask (nth 0 mask) (nth 1 mask)
                                                                             (nth 2 mask) window (nth 3 mask)))
                                     (error "[Image unavailable]")))))
            (setq position end))))
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
(defvar-local gnosis-image--selection nil "Selected stable target ID, or nil.")
(defvar-local gnosis-image--rectangle 0 "Selected rectangle index in editor.")
(defvar-local gnosis-image--reserved nil "Target IDs reserved for this edit session.")
(defvar-local gnosis-image--policy "hide-target" "Owned occlusion visibility policy.")
(defvar-local gnosis-image--purpose nil "One of edit, region or occlusion.")
(defvar-local gnosis-image--target nil "Occluded stable target ID.")
(defvar-local gnosis-image--revealed nil "Non-nil after explicit reveal.")
(defvar-local gnosis-image--size nil "Displayed width and height in pixels.")
(defvar-local gnosis-image--depth nil "Recursive input depth owned by this viewer.")
(defvar-local gnosis-image--accepted nil "Non-nil when explicit input finished.")
(defvar-local gnosis-image--check nil "Encounter validation function, or nil.")
(defvar-local gnosis-image--owner nil
  "Viewer token: (RENDER-OWNED . INPUT-LIVE), retired independently.")

(defun gnosis-image--owned-p ()
  "Return non-nil while this buffer still belongs to its native viewer."
  (and (car gnosis-image--owner) (not buffer-file-name)
       (derived-mode-p 'gnosis-image-mode)))

(defun gnosis-image--retire ()
  "Retire this viewer permanently on native file association."
  (let ((depth (and (cdr gnosis-image--owner) gnosis-image--depth)))
    (when gnosis-image--owner (setcar gnosis-image--owner nil))
    (setq gnosis-image--accepted nil)
    ;; Retire before unwinding: cleanup must not kill the successor buffer.
    (when (and depth (= (recursion-depth) (1+ depth)))
      (abort-recursive-edit))))

(defface gnosis-image-mask '((t (:background "#202020" :foreground "#ffffff")))
  "Opaque answer coverage and neutral cue text." :group 'gnosis)
(defface gnosis-image-selected '((t (:inherit match)))
  "Selected target outline." :group 'gnosis)
(defface gnosis-image-outline '((t (:inherit shadow)))
  "Unselected annotation outline." :group 'gnosis)

(defun gnosis-image--color (face attribute)
  "Return usable SVG color from FACE ATTRIBUTE."
  (let ((color (face-attribute face (intern (concat ":" (symbol-name attribute))) nil t)))
    (if (and (stringp color) (not (string-prefix-p "unspecified" color))) color
      (if (eq attribute 'background) "#202020" "#ffffff"))))

(defun gnosis-image--svg (scene regions width height purpose target selection revealed &optional policy)
  "Build SCENE and REGIONS SVG at WIDTH and HEIGHT.
PURPOSE is edit, region or occlusion.  Edit shows labels and rectangles;
region outlines SELECTION.  Occlusion masks TARGET until REVEALED, then
shows only the source image, without labels or selection outlines.
POLICY defaults to hide-target, or hide-all masks every annotated target."
  (let* ((svg (svg-create width height))
         (path (alist-get 'path scene))
         (type (nth 2 (gnosis-image--dimensions path)))
         (data (with-temp-buffer (set-buffer-multibyte nil)
                                 (insert-file-contents-literally path) (buffer-string))))
    (svg-embed svg data (if (eq type 'png) "image/png" "image/jpeg") t
               :x 0 :y 0 :width width :height height)
    (unless (and (eq purpose 'occlusion) revealed)
      (dolist (region regions)
        (cl-loop for rect in (gnosis-image--region-rectangles region)
                 for index from 0 do
                 (pcase-let* ((`(,x ,y ,w ,h) rect)
                              (id (alist-get 'id region))
                              (hidden (and (eq purpose 'occlusion)
                                           (or (equal policy "hide-all") (equal id target))))
                              (selected (equal id (if (consp selection) (car selection) selection))))
                   (when (or hidden (eq purpose 'edit) (and (eq purpose 'region) selected))
                     (svg-rectangle svg (* x width) (* y height) (* w width) (* h height)
                                    :fill (if hidden (gnosis-image--color 'gnosis-image-mask 'background) "none")
                                    :fill-opacity 1
                                    :stroke (gnosis-image--color
                                             (if selected 'gnosis-image-selected 'gnosis-image-outline) 'foreground)
                                    :stroke-width (if (equal selection (cons id index)) 4 2))
                   (when (eq purpose 'edit)
                     (svg-text svg (alist-get 'label region) :x (+ 3 (* x width))
                               :y (+ 16 (* y height)) :font-size 14
                               :stroke (gnosis-image--color 'gnosis-image-mask 'background)
                               :stroke-width 0.3
                               :fill (gnosis-image--color 'gnosis-image-mask 'foreground)))))))
      ;; Paint cues after every opaque rectangle, including overlapping siblings.
      (when (eq purpose 'occlusion)
        (dolist (rect (gnosis-image--region-rectangles
                      (gnosis-image-target `((regions . ,regions)) target)))
          (pcase-let ((`(,x ,y ,w ,h) rect))
            (svg-rectangle svg (* x width) (* y height) (* w width) (* h height)
                           :fill "none" :stroke (gnosis-image--color 'gnosis-image-selected 'foreground)
                           :stroke-width 3 :stroke-dasharray "5 2")
            (svg-text svg "?" :x (* (+ x (/ w 2)) width)
                      :y (* (+ y (/ h 2)) height) :text-anchor "middle"
                      :dominant-baseline "central" :font-size 16 :font-weight "bold"
                      :fill (gnosis-image--color 'gnosis-image-mask 'foreground)
                      :stroke (gnosis-image--color 'gnosis-image-mask 'background)
                      :stroke-width 0.5)))))
    svg))

(defun gnosis-image--render (&rest _)
  "Render this owned viewer at its current window size."
  (when-let* (((gnosis-image--owned-p))
              (window (get-buffer-window (current-buffer))) (scene gnosis-image--scene))
    (let* ((scale (min 1.0 (/ (float (max 1 (- (window-body-width window t) 16)))
                              (alist-get 'width scene))
                       (/ (float (max 1 (- (window-body-height window t) 120)))
                          (alist-get 'height scene))))
           (width (max 1 (floor (* scale (alist-get 'width scene)))))
           (height (max 1 (floor (* scale (alist-get 'height scene)))))
           (image (svg-image (gnosis-image--svg scene gnosis-image--regions width height
                                                gnosis-image--purpose gnosis-image--target
                                                (if (eq gnosis-image--purpose 'edit)
                                                    (cons gnosis-image--selection gnosis-image--rectangle)
                                                  gnosis-image--selection)
                                                gnosis-image--revealed gnosis-image--policy)))
           (inhibit-read-only t))
      (setq gnosis-image--size (cons width height))
      (erase-buffer)
      (when-let* ((prompt (alist-get 'prompt scene))) (insert prompt "\n\n"))
      (when (and (eq gnosis-image--purpose 'edit) gnosis-image--selection)
        (let* ((target (gnosis-image-target `((regions . ,gnosis-image--regions)) gnosis-image--selection))
               (count (length (gnosis-image--region-rectangles target))))
          (insert (propertize (alist-get 'label target) 'face 'gnosis-image-selected)
                  (format " — rectangle %d/%d\n\n" (1+ gnosis-image--rectangle) count))))
      (insert-image image)
      (goto-char (point-min))
      (setq header-line-format
            (pcase gnosis-image--purpose
              ('edit " Drag: new | S-drag: add | n/p: rectangle | d/D: remove/target | r: reassign | l: rename | RET: accept | q: cancel")
              ('region " Click to select (neutral) | RET: submit | q: cancel")
              (_ (if gnosis-image--revealed " Answer revealed | RET: continue | q: cancel"
                   " Name the label hidden by the ? masks | RET: reveal | q: cancel")))))))

(defun gnosis-image--position (position)
  "Return normalized coordinates for image event POSITION, or nil."
  (let ((xy (posn-object-x-y position)) (object (posn-object position)))
    (when (and (eq (posn-window position) (get-buffer-window (current-buffer)))
               (eq (car-safe object) 'image) xy gnosis-image--size
               (<= 0 (car xy) (car gnosis-image--size))
               (<= 0 (cdr xy) (cdr gnosis-image--size)))
      (cons (/ (float (car xy)) (car gnosis-image--size))
            (/ (float (cdr xy)) (cdr gnosis-image--size))))))

(defun gnosis-image--hit-rectangle (regions xy)
  "Return first (TARGET . INDEX) in REGIONS containing normalized XY."
  (when xy
    (cl-loop for r in regions thereis
             (cl-loop for rect in (gnosis-image--region-rectangles r)
                      for index from 0
                      when (pcase-let ((`(,x ,y ,w ,h) rect))
                             (and (<= x (car xy) (+ x w)) (<= y (cdr xy) (+ y h))))
                      return (cons (alist-get 'id r) index)))))

(defun gnosis-image--hit (regions xy)
  "Return first stable ID in REGIONS containing normalized XY."
  (car (gnosis-image--hit-rectangle regions xy)))

(defun gnosis-image-select (event)
  "Select target at mouse EVENT without grading or revealing labels."
  (interactive "e")
  (unless (gnosis-image--owned-p) (user-error "Image viewer is no longer active"))
  (let ((hit (gnosis-image--hit-rectangle gnosis-image--regions
                                          (gnosis-image--position (event-start event)))))
    (setq gnosis-image--selection (car hit) gnosis-image--rectangle (or (cdr hit) 0)))
  (gnosis-image--render))

(defun gnosis-image--editor-snapshot (&optional selected)
  "Capture editor ownership, requiring a selection when SELECTED."
  (unless (and (gnosis-image--owned-p) (eq gnosis-image--purpose 'edit))
    (user-error "Not editing image targets"))
  (when selected
    (unless (and gnosis-image--selection
                 (nth gnosis-image--rectangle
                      (gnosis-image--region-rectangles
                       (gnosis-image-target `((regions . ,gnosis-image--regions)) gnosis-image--selection))))
      (user-error "Select a rectangle first")))
  (list (current-buffer) gnosis-image--regions (copy-tree gnosis-image--regions)
        gnosis-image--selection gnosis-image--rectangle (copy-sequence gnosis-image--reserved)
        gnosis-image--scene (copy-tree gnosis-image--scene) gnosis-image--owner))

(defun gnosis-image--editor-check (snapshot)
  "Reject changed editor ownership or contents since SNAPSHOT."
  (unless (and (buffer-live-p (car snapshot)) (eq (car snapshot) (current-buffer))
               (gnosis-image--owned-p) (eq (nth 8 snapshot) gnosis-image--owner)
               (eq gnosis-image--purpose 'edit)
               (eq (nth 1 snapshot) gnosis-image--regions)
               (equal (nth 2 snapshot) gnosis-image--regions)
               (equal (nth 3 snapshot) gnosis-image--selection)
               (equal (nth 4 snapshot) gnosis-image--rectangle)
               (equal (nth 5 snapshot) gnosis-image--reserved)
               (eq (nth 6 snapshot) gnosis-image--scene)
               (equal (nth 7 snapshot) gnosis-image--scene))
    (user-error "Image editor changed during input")))

(defun gnosis-image--with-rectangles (region rectangles)
  "Return REGION with plural RECTANGLES, preserving target identity."
  (append (seq-remove (lambda (entry) (memq (car entry) '(rect rects))) region)
          (list (cons 'rects rectangles))))

(defun gnosis-image--replace-rectangles (regions id rectangles)
  "Return REGIONS with ID geometry replaced by RECTANGLES, or removed if empty."
  (cl-loop for r in regions
           if (not (equal id (alist-get 'id r))) collect r
           else when rectangles collect (gnosis-image--with-rectangles r rectangles)))

(defun gnosis-image--editor-apply (snapshot regions id index)
  "Apply validated REGIONS and selection ID/INDEX to owned SNAPSHOT."
  (gnosis-image--editor-check snapshot)
  (gnosis-image--regions regions)
  (setq gnosis-image--reserved
        (delete-dups (append gnosis-image--reserved
                             (mapcar (lambda (r) (alist-get 'id r)) gnosis-image--regions)
                             (mapcar (lambda (r) (alist-get 'id r)) regions)))
        gnosis-image--regions regions gnosis-image--selection id
        gnosis-image--rectangle index)
  (gnosis-image--render))

(defun gnosis-image--fresh-id ()
  "Return an ID never used in this editor session."
  (cl-loop for n from 1 for id = (format "region-%d" n)
           unless (or (member id gnosis-image--reserved)
                      (seq-some (lambda (r) (equal id (alist-get 'id r))) gnosis-image--regions))
           return id))

(defun gnosis-image-draw (event &optional add)
  "Draw a new labelled target from drag EVENT, or append to selection with ADD."
  (interactive "e")
  (let* ((snapshot (gnosis-image--editor-snapshot add))
         (start (gnosis-image--position (event-start event)))
         (end (gnosis-image--position (event-end event))))
    (unless (and start end (> (abs (- (car start) (car end))) 0.002)
                 (> (abs (- (cdr start) (cdr end))) 0.002))
      (user-error "Drag a rectangle inside the image"))
    (let* ((rect (list (min (car start) (car end)) (min (cdr start) (cdr end))
                       (abs (- (car start) (car end))) (abs (- (cdr start) (cdr end)))))
           (regions gnosis-image--regions)
           (id (if add gnosis-image--selection (gnosis-image--fresh-id)))
           (target (and add (gnosis-image-target `((regions . ,regions)) id)))
           (rects (and add (gnosis-image--region-rectangles target)))
           (label (unless add (prog1 (read-string "Target label: ")
                               (gnosis-image--editor-check snapshot)))))
      (gnosis-image--editor-apply
       snapshot
       (if add (gnosis-image--replace-rectangles regions id (append rects (list rect)))
         (append regions (list `((id . ,id) (label . ,label) (rects . (,rect))))))
       id (if add (length rects) 0)))))

(defun gnosis-image-add-rectangle (event)
  "Append drag EVENT as another rectangle of the selected target."
  (interactive "e")
  (gnosis-image-draw event t))

(defun gnosis-image-next-rectangle (&optional backwards)
  "Select the next rectangle, or previous when BACKWARDS, including overlaps."
  (interactive)
  (gnosis-image--editor-snapshot)
  (let* ((choices (cl-loop for r in gnosis-image--regions append
                           (cl-loop for i below (length (gnosis-image--region-rectangles r))
                                    collect (cons (alist-get 'id r) i))))
         (position (cl-position (cons gnosis-image--selection gnosis-image--rectangle) choices :test #'equal))
         (next (and choices (nth (mod (+ (or position (if backwards 0 -1))
                                         (if backwards -1 1)) (length choices)) choices))))
    (setq gnosis-image--selection (car next) gnosis-image--rectangle (or (cdr next) 0))
    (gnosis-image--render)))

(defun gnosis-image-previous-rectangle ()
  "Select the previous rectangle, including obscured overlaps."
  (interactive)
  (gnosis-image-next-rectangle t))

(defun gnosis-image-delete-region (&optional whole)
  "Remove selected rectangle, or the WHOLE target, from this new revision."
  (interactive)
  (let* ((snapshot (gnosis-image--editor-snapshot t))
         (id gnosis-image--selection)
         (rects (gnosis-image--region-rectangles
                 (gnosis-image-target `((regions . ,gnosis-image--regions)) id))))
    (when (or (and (not whole) (> (length rects) 1))
              (prog1 (y-or-n-p "Remove this target from the new image revision? ")
                (gnosis-image--editor-check snapshot)))
      (let ((remaining (unless whole
                         (cl-loop for r in rects for i from 0
                                  unless (= i gnosis-image--rectangle) collect r))))
        (gnosis-image--editor-apply snapshot
                                    (gnosis-image--replace-rectangles gnosis-image--regions id remaining)
                                    (and remaining id) 0)))))

(defun gnosis-image-delete-target ()
  "Remove the entire selected target after confirmation."
  (interactive)
  (gnosis-image-delete-region t))

(defun gnosis-image--target-choices (regions)
  "Return collision-free labelled completion choices for REGIONS."
  (cl-loop for r in regions for n from 1
           collect (cons (format "%d: %s" n (alist-get 'label r)) (alist-get 'id r))))

(defun gnosis-image-reassign-rectangle ()
  "Move selected rectangle to an existing or explicitly new target."
  (interactive)
  (let* ((snapshot (gnosis-image--editor-snapshot t))
         (regions gnosis-image--regions) (source gnosis-image--selection)
         (rects (gnosis-image--region-rectangles (gnosis-image-target `((regions . ,regions)) source)))
         (choices (cons '("New target" . new) (gnosis-image--target-choices regions)))
         (choice (cdr (assoc (completing-read "Move rectangle to: " choices nil t) choices))))
    (gnosis-image--editor-check snapshot)
    (unless choice (user-error "Choose a target"))
    (unless (equal choice source)
      (let* ((id (if (eq choice 'new) (gnosis-image--fresh-id) choice))
             (label (when (eq choice 'new)
                      (prog1 (read-string "New target label: ") (gnosis-image--editor-check snapshot))))
             (destination (unless label (gnosis-image-target `((regions . ,regions)) id)))
             (existing (and destination (gnosis-image--region-rectangles destination))))
        (when (or (> (length rects) 1)
                  (prog1 (y-or-n-p "Moving the last rectangle removes its old target; continue? ")
                    (gnosis-image--editor-check snapshot)))
          (let* ((moved (nth gnosis-image--rectangle rects))
                 (remaining (cl-loop for r in rects for i from 0
                                     unless (= i gnosis-image--rectangle) collect r))
                 (without (gnosis-image--replace-rectangles regions source remaining))
                 (result (if destination
                             (gnosis-image--replace-rectangles without id (append existing (list moved)))
                           (append without (list `((id . ,id) (label . ,label) (rects . (,moved))))))))
            (gnosis-image--editor-apply snapshot result id (length existing))))))))

(defun gnosis-image-rename-target ()
  "Rename selected target without changing its ID or merging equal labels."
  (interactive)
  (let* ((snapshot (gnosis-image--editor-snapshot t))
         (id gnosis-image--selection)
         (target (gnosis-image-target `((regions . ,gnosis-image--regions)) id))
         (label (read-string "Target label: " (alist-get 'label target))))
    (gnosis-image--editor-apply
     snapshot (mapcar (lambda (r) (if (equal id (alist-get 'id r))
                                     (cons (cons 'label label) (assq-delete-all 'label (copy-tree r))) r))
                      gnosis-image--regions)
     id gnosis-image--rectangle)))

(defun gnosis-image-submit ()
  "Accept edited regions, submit a selection, or explicitly reveal occlusion."
  (interactive)
  (unless (and (gnosis-image--owned-p) gnosis-image--depth
               (= (recursion-depth) (1+ gnosis-image--depth)))
    (user-error "No active image input"))
  (let ((buffer (current-buffer)) (owner gnosis-image--owner))
    (when gnosis-image--check (funcall gnosis-image--check))
    (unless (and (eq buffer (current-buffer)) (eq owner gnosis-image--owner)
                 (gnosis-image--owned-p))
      (user-error "Image viewer changed during input")))
  (when (and (eq gnosis-image--purpose 'region) (not gnosis-image--selection))
    (user-error "Click a region before submitting"))
  (if (and (eq gnosis-image--purpose 'occlusion) (not gnosis-image--revealed))
      (progn (setq gnosis-image--revealed t) (gnosis-image--render))
    (setq gnosis-image--accepted t)
    (exit-recursive-edit)))

(defun gnosis-image-cancel ()
  "Cancel owned image input without returning an answer."
  (interactive)
  ;; A retired viewer can still be inside its original recursive input after
  ;; nested input returns.  Only that input's unwind retires cancellation.
  (when (and (cdr gnosis-image--owner) gnosis-image--depth
             (= (recursion-depth) (1+ gnosis-image--depth)))
    (abort-recursive-edit)))

(defvar-keymap gnosis-image-mode-map
  :parent special-mode-map
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'gnosis-image-select
  "<drag-mouse-1>" #'gnosis-image-draw
  "S-<down-mouse-1>" #'ignore
  "S-<drag-mouse-1>" #'gnosis-image-add-rectangle
  "n" #'gnosis-image-next-rectangle
  "p" #'gnosis-image-previous-rectangle
  "r" #'gnosis-image-reassign-rectangle
  "l" #'gnosis-image-rename-target
  "D" #'gnosis-image-delete-target
  "d" #'gnosis-image-delete-region
  "RET" #'gnosis-image-submit
  "q" #'gnosis-image-cancel
  "C-g" #'gnosis-image-cancel)

(define-derived-mode gnosis-image-mode special-mode "Gnosis Image"
  "Inspect or edit an owned native image; header line shows available actions."
  (setq-local cursor-type nil)
  (setq gnosis-image--owner (cons t t))
  (add-hook 'window-configuration-change-hook #'gnosis-image--render nil t)
  (add-hook 'after-set-visited-file-name-hook #'gnosis-image--retire nil t)
  (add-hook 'kill-buffer-hook #'gnosis-image-cancel nil t)
  (add-hook 'change-major-mode-hook #'gnosis-image-cancel nil t))

(defun gnosis-image-input (scene purpose &optional target check policy)
  "Present SCENE for PURPOSE and return (REGIONS SELECTION).
TARGET identifies occlusion.  CHECK revalidates ownership.
POLICY controls masks.
Restore the original layout and destroy only the owned viewer on every exit."
  (gnosis-image--decode scene)
  (when check (funcall check))
  (unless (image-type-available-p 'svg) (user-error "Native SVG support is required"))
  (let* ((buffer (generate-new-buffer "*Gnosis Image*"))
         (tick (with-current-buffer buffer (buffer-modified-tick)))
         (created t)
         (retire (lambda () (setq created nil)))
         (created-p
          (lambda ()
            (and created (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and (eq major-mode 'fundamental-mode) (not buffer-file-name)
                        (= tick (buffer-modified-tick)))))))
         owner)
    (save-window-excursion
      (unwind-protect
          (progn
            ;; Navigation can run callbacks before the viewer mode owns BUFFER.
            ;; Retire even across a mode round trip or file detachment.
            (with-current-buffer buffer
              (add-hook 'change-major-mode-hook retire nil t)
              (add-hook 'after-set-visited-file-name-hook retire nil t))
            (pop-to-buffer-same-window buffer)
            (when check (funcall check))
            (unless (and (eq (current-buffer) buffer) (funcall created-p))
              (user-error "Image viewer changed during setup"))
            (delete-other-windows)
            (when check (funcall check))
            (unless (and (eq (current-buffer) buffer) (funcall created-p))
              (user-error "Image viewer changed during setup"))
            (gnosis-image-mode)
            (setq owner gnosis-image--owner)
            (when check (funcall check))
            (setq gnosis-image--scene (copy-tree scene)
                  gnosis-image--regions (copy-tree (alist-get 'regions scene))
                  gnosis-image--purpose purpose gnosis-image--target target
                  gnosis-image--reserved (mapcar (lambda (r) (alist-get 'id r)) gnosis-image--regions)
                  gnosis-image--policy (or policy "hide-target")
                  gnosis-image--depth (recursion-depth) gnosis-image--check check)
            (gnosis-image--render)
            (when check (funcall check))
            (recursive-edit)
            (when check (funcall check))
            (unless (and (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (and (eq owner gnosis-image--owner) (gnosis-image--owned-p)
                                gnosis-image--accepted)))
              (user-error "Image input cancelled"))
            (with-current-buffer buffer
              (list (copy-tree gnosis-image--regions) gnosis-image--selection)))
        (when owner (setcdr owner nil))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (remove-hook 'change-major-mode-hook retire t)
            (remove-hook 'after-set-visited-file-name-hook retire t)))
        (when (and (buffer-live-p buffer)
                   (or (funcall created-p)
                       (with-current-buffer buffer
                         (and owner (eq owner gnosis-image--owner)
                              (gnosis-image--owned-p)))))
          (with-current-buffer buffer (setq gnosis-image--depth nil))
          (kill-buffer buffer))))))

(defun gnosis-image-edit-regions (scene)
  "Edit SCENE rectangles graphically and return accepted plain region values."
  (car (gnosis-image-input scene 'edit)))

(defun gnosis-image--publish (reference regions source attribution)
  "Publish REGIONS, SOURCE and ATTRIBUTION over the exact original REFERENCE.
Verify the private original copy before replacing its manifest."
  (let* ((database (gnosis--ensure-db))
         (root (gnosis-assets-root database))
         (scene (gnosis-image-resolve reference))
         (revision (substring reference 0 64))
         (names (list "image.json" (alist-get 'file scene)))
         (stage (make-temp-file "gnosis-image-edit-" t)))
    (unwind-protect
        (progn
          (dolist (name names)
            (copy-file (gnosis-assets-file (alist-get 'directory scene) name)
                       (expand-file-name name stage)))
          (unless (equal revision (gnosis-assets-revision stage names))
            (user-error "Image resource changed during copying"))
          (gnosis-assets-validate root revision names)
          (gnosis-assets-root database)
          (gnosis-image-import (expand-file-name (alist-get 'file scene) stage)
                               regions source attribution))
      (delete-directory stage t))))

(defun gnosis-image--read-resource (&optional reference regions-p)
  "Read or reuse REFERENCE, optionally editing regions when REGIONS-P.
Return immutable managed reference.  Prompts never assign guessed provenance."
  (let* ((database (gnosis--ensure-db))
         (reuse (and reference (y-or-n-p "Reuse this image? ")))
         (old (and reuse (gnosis-image-resolve reference)))
         (file (unless reuse (read-file-name "PNG or JPEG: " nil nil t)))
         (source (read-string "Source (optional): " (and reuse (alist-get 'source old))))
         (attribution (read-string "Attribution/license (optional): "
                                   (and reuse (alist-get 'attribution old))))
         (pinned (progn
                   (gnosis-assets-root database)
                   (if reuse
                       (gnosis-image--publish reference (alist-get 'regions old) source attribution)
                     (gnosis-image-import file nil source attribution))))
         (scene (gnosis-image-resolve pinned))
         (regions (when regions-p (gnosis-image-edit-regions scene))))
    (gnosis-assets-root database)
    ;; Recursive editing can outlive the pixels and manifest it displayed.
    (gnosis-image-resolve pinned)
    (if regions-p (gnosis-image--publish pinned regions source attribution) pinned)))

(defun gnosis-image--read-fields (&optional reference occlusion policy)
  "Read graphical fields, reusing REFERENCE, with text for OCCLUSION and POLICY."
  (let* ((database (gnosis--ensure-db))
         (reference (gnosis-image--read-resource reference t))
         (regions (alist-get 'regions (gnosis-image-resolve reference)))
         (choices (gnosis-image--target-choices regions)))
    (unless choices (user-error "Draw at least one labelled region"))
    (let ((target (cdr (assoc (completing-read "Expected region: " choices nil t) choices))))
      (unless target (user-error "Choose an expected region"))
      (gnosis-assets-root database)
      (gnosis-image-resolve reference target)
      (if occlusion
          (let ((policy (completing-read "Label visibility: "
                                          '(("hide-target") ("hide-all")) nil t nil nil
                                          (or policy "hide-target"))))
            (gnosis-assets-root database)
            (gnosis-image-occlusion-fields
             (list reference target policy)
             (list (alist-get 'label (gnosis-image-target (gnosis-image-resolve reference) target)))))
        (list (list reference) (list target))))))

;;;###autoload
(defun gnosis-add-image-thema (&optional type)
  "Draw regions and open a normal image thema draft of TYPE.
TYPE is image-region or image-occlusion; interactively choose between them."
  (interactive)
  (let ((type (or type (completing-read "Image type: " '("image-region" "image-occlusion") nil t))))
    (unless (member type '("image-region" "image-occlusion")) (user-error "Invalid image type"))
    (when (get-buffer "*Gnosis NEW*") (user-error "Finish the existing draft first"))
    (pcase-let ((`(,hypothesis ,answer) (gnosis-image--read-fields nil (equal type "image-occlusion"))))
      (gnosis-add-thema type nil (mapconcat #'identity hypothesis "\n- ") (car answer)))))

(defun gnosis-image-attach ()
  "Attach a managed image at point, or edit a single media thema resource.
Delegate model drafts to their own attachment command.  Preserve draft text
and database ownership through all prompts and cancellation."
  (interactive nil gnosis-edit-mode)
  (let* ((owner (current-buffer)) (tick (buffer-chars-modified-tick))
         (position (point)) (database (gnosis--ensure-db))
         (themata (save-restriction (widen) (gnosis-export-parse-themata)))
         (thema (car themata))
         (type (downcase (or (nth 1 thema) "")))
         (image-p (member type '("image-region" "image-occlusion"))))
    (unless (= (length themata) 1) (user-error "Attach in a single thema draft"))
    (if (equal type "model")
        (gnosis-model-attach)
      (unless image-p
        (unless (member (save-excursion (org-back-to-heading t) (org-get-heading t t t t))
                        '("Keimenon" "Hypothesis" "Answer" "Parathema"))
          (user-error "Attach inside a question, hint, answer or explanation")))
      (let* ((occlusion (equal type "image-occlusion"))
             (old-fields (and occlusion (gnosis-image-occlusion-fields (nth 3 thema) (nth 4 thema))))
             (value (if image-p
                        (if occlusion
                            (gnosis-image--read-fields (car (nth 3 thema)) t (nth 2 (car old-fields)))
                          (gnosis-image--read-fields (car (nth 3 thema))))
                      (gnosis-image--read-resource))))
        (when (and occlusion (not (equal (cadar old-fields) (cadar value))))
          (unless (y-or-n-p "Keep the authored answer and aliases for the changed target? ")
            (user-error "Image attachment cancelled")))
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
                                               (mapconcat #'identity (car value) "\n- ")
                                               (if occlusion (caadr old-fields) (caadr value))
                                               (nth 5 thema) (nth 6 thema) nil (nth 8 thema))
                  (goto-char (point-min)))
              (goto-char position)
              (insert (format "[[gnosis-image:%s]]" value)))))))))

(provide 'gnosis-image)
;;; gnosis-image.el ends here
