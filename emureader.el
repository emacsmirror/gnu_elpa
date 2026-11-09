;;; emureader.el --- General-purpose document reader -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Divya Ranjan Pattanaik <divya@dialetheia.xyz>
;;         Tushar <tusharhero@sdf.org>
;; Keywords: lisp, files, tools
;; Version: 0.4.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://codeberg.org/MonadicSheep/emureader

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a general purpose document reader within
;; Emacs by leveraging the use of dynamic modules.  It primarily
;; relies on MuPDF to do the rendering, and Emacs Lisp’s native image
;; displaying capabilities to view the rendered images and manipulate them.

;; To use,
;; - open a file using `find-file'
;; - Switch pages using `C-n', `C-p'
;; - Zoom using `+' and `-'.

;;; Code:

(require 'image)

(defconst emureader-directory
  (file-name-directory (or
			(macroexp-file-name)
			load-file-name
			default-directory))
  "The directory from where emureader was first loaded.")

(defun emureader--load-module ()
  "Load the `emureader-dyn' dynamic module from `emureader-directory'."
  (let ((load-path (cons emureader-directory load-path)))
    (require 'emureader-dyn)))

(defun emureader--module-built-p ()
  "Return non-nil if the dynamic module exists in `emureader-directory'."
  (file-exists-p
   (file-name-concat emureader-directory
		     (concat "emureader-dyn" module-file-suffix))))

(defun emureader--post-build (_buffer status)
  "Load the module if the build finished successfully, per STATUS."
  (if (string-prefix-p "finished" status)
      (progn (emureader--load-module)
	     (mapc (lambda (buff)
		     (with-current-buffer buff
		       (emureader-mode)))
		   (match-buffers '(major-mode . emureader-mode))))
    (message "emureader: module build failed, see *emureader-module-compilation*")))

;;;###autoload
(defun emureader-build ()
  "Build emureader's dynamic module, then load it."
  (interactive)
  (let ((default-directory emureader-directory)
	(compilation-buffer-name-function
	 (lambda (_) "*emureader-module-compilation*")))
    (compile (concat (or (executable-find "gmake") "make")
		     " module"))
    (with-current-buffer "*emureader-module-compilation*"
      ;; Buffer-local hook
      (add-hook 'compilation-finish-functions #'emureader--post-build nil t))
    ;; Let it compile.
    (sit-for 1)))

(defun emureader-ensure-module ()
  "Ensure emureader's dynamic module is loaded."
  (unless (featurep 'emureader-dyn)
    (if (emureader--module-built-p)
	(emureader--load-module)
      (when (yes-or-no-p "Build emureader's dynamic module? ")
	(emureader-build)))))

(defvar emureader--recent-pagenumber-fallback)
(defvar emureader-current-doc-state-ptr)
(defvar emureader--recent-pagenumber-fallback)
(defvar emureader-current-doc-render-status)

;;; Forward declarations for module functions
(declare-function emureader-dyn--load-doc "ext:emureader-dyn.so" (file &optional page))
(declare-function emureader-dyn--redisplay-doc "ext:emureader-dyn.so")
(declare-function emureader-dyn--close-doc "ext:emureader-dyn.so")
(declare-function emureader-dyn--next-page "ext:emureader-dyn.so")
(declare-function emureader-dyn--prev-page "ext:emureader-dyn.so")
(declare-function emureader-dyn--first-page "ext:emureader-dyn.so")
(declare-function emureader-dyn--last-page "ext:emureader-dyn.so")
(declare-function emureader-dyn--goto-page "ext:emureader-dyn.so" (page))
(declare-function emureader-dyn--scale-page "ext:emureader-dyn.so" (factor))
(declare-function emureader-dyn--doc-invert "ext:emureader-dyn.so" (invert))
(declare-function emureader-dyn--rotate-page "ext:emureader-dyn.so" (degree))
(declare-function emureader-dyn--window-create "ext:emureader-dyn.so" (window))
(declare-function emureader-dyn--window-close "ext:emureader-dyn.so" (window))

(defgroup emureader nil
  "Group for EMuReader’s customizations."
  :prefix "emureader-"
  :group 'custom)

(defcustom emureader-enlarge-factor 1.25
  "The fractional amount by which the page would be enlarged."
  :type 'number)

(defcustom emureader-shrink-factor 0.75
  "The fractional amount by which the page would be shrinked."
  :type 'number)

(defcustom emureader-default-fit #'emureader-fit-to-height
  "The default fitting for documents."
  :type '(radio (function-item emureader-fit-to-height)
		(function-item emureader-fit-to-width)))

(defvar-local emureader--recent-scale-fallback 1.0
  "Previous scaling factor applied to the document.")

(defvar-local emureader--reloading nil
  "Boolean flag to represent reloading status of a document.")

;; Setting of `auto-mode-list' fails if not autoloaded.
;;;###autoload
(defconst emureader-supported-formats (list "pdf" "epub" "mobi"
					    "fb2" "xps" "cbz"
					    "docx""pptx" "xlsx"
					    "odt" "ods" "odp" "odg")
  "File formats supported by the document emureader.")

(defun emureader-current-doc-overlay (&optional window)
  "Overlay parameter for WINDOW.

WINDOW must be a valid window and defaults to the selected one."
  (window-parameter (or window (selected-window)) 'overlay))

(defun emureader--window-owned-p (window)
  "Non-nil if WINDOW's emureader overlay belongs to WINDOW's current buffer."
  (let ((ov (window-parameter window 'overlay)))
    (and (overlayp ov)
	 (eq (overlay-buffer ov)
	     (window-buffer window)))))

(defun emureader--current-doc-window ()
  "Return a window showing the current buffer's document, or nil.
Prefer the selected window."
  (seq-find #'emureader--window-owned-p (get-buffer-window-list nil nil t)))

(defun emureader-current-doc-pagenumber (&optional window)
  "Page number of WINDOW.

WINDOW defaults to a window showing the current buffer.  Without
one fall back to the buffer-local page number."
  (let ((window (or window (emureader--current-doc-window))))
    (or (and window (emureader--window-owned-p window)
	     (window-parameter window 'page))
	emureader--recent-pagenumber-fallback)))

(defun emureader-current-doc-scale-value (&optional window)
  "Scaling factor of WINDOW.

WINDOW defaults to a window showing the current buffer.  Without
one fall back to the buffer-local scale value."
  (let ((window (or window (emureader--current-doc-window))))
    (or (and window (emureader--window-owned-p window)
	     (window-parameter window 'scale))
	emureader--recent-scale-fallback)))

;; We queue some commands because the user is expected to use the
;; commands repeatedly, such as by simply spamming a key. If we don't
;; queue the commands, Emacs may start skipping intermediate commands.

(defvar emureader--command-queue nil
  "Queue of emureader commands to be executed sequentially.")

(defun emureader--process-command-queue ()
  "Process the next command in the queue."
  (when-let* ((cmd (pop emureader--command-queue))
	      ;; Selected through trial and error.
	      (delay 0.0001))
    (funcall cmd)
    (redisplay)
    (run-with-idle-timer delay nil #'emureader--process-command-queue)))

(defun emureader--enqueue-command (cmd args)
  "Add CMD with ARGS to the queue and start processing if needed."
  (push (apply #'apply-partially cmd args) emureader--command-queue)
  (unless (or (active-minibuffer-window)
              (memq #'emureader--process-command-queue post-command-hook))
    (add-hook 'post-command-hook #'emureader--process-command-queue nil t)))

(defmacro emureader--define-queue-command (name arglist docstring interactive &rest body)
  "Define NAME as a emureader command.
Also define a necessary non-queue function.

Much like `defun', except that ARGLIST, DOCSTRING, and INTERACTIVE are
required.

The emureader--non-queue-NAME function is simply defined as a function with
body BODY.

The emureader-NAME command is simply a wrapper around
`emureader--enqueue-command' with emureader--non-queue-NAME as the argument."
  (declare (indent defun)
	   (doc-string 3))
  (let* ((name (symbol-name name))
	 (non-queue-function-name (concat "emureader--non-queue-" name))
	 (queue-function-name (concat "emureader-" name)))
    `(progn

       (defun ,(intern non-queue-function-name) ,arglist
	 ,(format "%s

This is the actual function.
See `%s',
for interactive version."
		  docstring queue-function-name)
	 ,(push 'progn body))

       (defun ,(intern queue-function-name) ,arglist
	 ,(format "%s

This is the queuing function.
See `%s',
for actual definition."
		  docstring non-queue-function-name)
	 ,interactive
	 (emureader--enqueue-command #',(intern non-queue-function-name)
				     ,(remq '&optional (push 'list arglist)))))))

(defun emureader--read-doc-name ()
  "Read a document file name."
  (let* ((regexp (regexp-opt emureader-supported-formats t))
	 (prompt "Open document: ")
	 (file (read-file-name
		prompt nil nil t nil
		(lambda (f)
		  (or (file-directory-p f)
		      (string-match-p regexp f))))))
    (if (file-directory-p file)
	(emureader--read-doc-name) file)))

;;;###autoload
(defun emureader-open-doc (document)
  "Open DOCUMENT for viewing.

The only formats that can be opened `emureader-supported-formats', any
other file format will simply not show up as a candidate."
  (interactive (list (expand-file-name (emureader--read-doc-name))))
  (find-file document))

(define-queue-command next-page ()
  "Go to the next page of the document."
  (interactive)
  (when (emureader-dyn--next-page)
    ;; Must return `t' when the command succeeds.
    (force-mode-line-update) t))

(define-queue-command previous-page ()
  "Go to the previous page of the document."
  (interactive)
  (when (emureader-dyn--prev-page)
    ;; Must return `t' when the command succeeds.
    (force-mode-line-update) t))

(defun emureader-first-page ()
  "Go to the first page of the document."
  (interactive)
  (emureader-dyn--first-page)
  (emureader--center-page)
  (force-mode-line-update))

(defun emureader-last-page ()
  "Go to the last page of the document."
  (interactive)
  (emureader-dyn--last-page)
  (emureader--center-page)
  (force-mode-line-update))

(defun emureader-goto-page (n)
  "Go to page number N in the current document."
  (interactive "nGoto page: ")
  (emureader-dyn--goto-page (- n 1)) ; MuPDF does 0-indexing
  (emureader--center-page))

(defun emureader--get-current-doc-image-size (&optional window)
  "Get the dimensions of the page in WINDOW.

WINDOW must be a valid window and defaults to the selected one."
  (let* ((cdr-image (cdr (overlay-get (emureader-current-doc-overlay window) 'display)))
	 (width (plist-get cdr-image :width))
	 (height (plist-get cdr-image :height)))
    (cons width height)))

(defun emureader-doc-scale-page (factor)
  "Scales the page by a given FACTOR.

It also updates `emureader--recent-scale-fallback' and `scale' property of
selected window to reflect the new, possibly clamped scale."
  (let ((inhibit-quit t))
    (when-let* ((factor-clamped (emureader-dyn--scale-page factor)))
      (setq emureader--recent-scale-fallback factor-clamped)
      (set-window-parameter nil 'scale factor-clamped))))

(defun emureader-enlarge-size (&optional scaling-factor)
  "Enlarge the size of the current page by the `emureader-enlarge-factor'.

Optionally scale it by the SCALING-FACTOR.  Scales on selected window."
  (interactive (list (float
		      (* (emureader-current-doc-scale-value) emureader-enlarge-factor))))
  (emureader-doc-scale-page scaling-factor)
  (emureader--center-page))

(defun emureader-shrink-size (&optional scaling-factor)
  "Shrink the size of the current page by the `emureader-shrink-factor'.

Optionally scale it by the SCALING-FACTOR.  Scales on selected window."
  (interactive (list (float
		      (* (emureader-current-doc-scale-value) emureader-shrink-factor))))
  (emureader-doc-scale-page scaling-factor)
  (emureader--center-page))

(defun emureader-reset-size ()
  "Reset the size of the current page to 1.0."
  (interactive)
  (emureader-doc-scale-page 1.0)
  (emureader--center-page))

(defun emureader-fit-to-height ()
  "Scale the current page in the selected window to fit its height."
  (interactive)
  (let* ((image-height (cdr (emureader--get-current-doc-image-size)))
	 (pixel-window-height (window-body-height nil t))
	 (unscaled-height (/ image-height (emureader-current-doc-scale-value)))
	 (scaling-factor (/ pixel-window-height unscaled-height)))
    (emureader-doc-scale-page scaling-factor)
    (emureader--center-page)
    (emureader--set-window-vscroll nil 0)))

(defun emureader-fit-to-width ()
  "Scale the current page in the selected window to fit its width."
  (interactive)
  (let* ((image-width (car (emureader--get-current-doc-image-size)))
	 (pixel-window-width (window-body-width nil t))
	 (unscaled-width (/ image-width (emureader-current-doc-scale-value)))
	 (scaling-factor (/ pixel-window-width unscaled-width)))
    (emureader-doc-scale-page scaling-factor)
    (emureader--center-page)))

(defun emureader--get-pixel-per-col (&optional window)
  "Get the no of pixels per column for WINDOW.

WINDOW must be a valid window and defaults to the selected one."
  (/ (window-body-width window t) (window-body-width window)))

;; We need to do this because scrolling is possible in one direction
;; (downwards) indefinitely.

(defun emureader--set-window-vscroll (window vscroll &optional pixels-p)
  "Set amount by which WINDOW should be scrolled vertically to VSCROLL.

If setting VSCROLL makes the document page's top disappear, it sets the
maximum vertical scroll possible without doing that.

If PIXELS-P is non-nil, VSCROLL is considered to be in pixels.
Also see `set-window-vscroll'."
  (let* ((image-height (cdr (emureader--get-current-doc-image-size window)))
	 (pixel-window-height (window-body-height window t))
	 (window-height (window-body-height window))
	 (pixel-per-col (/ pixel-window-height window-height))
	 (pixel-vscroll (* pixel-per-col vscroll))
	 (max-pixel-vscroll (- image-height pixel-window-height))
	 (corrected-pixel-vscroll (min pixel-vscroll max-pixel-vscroll))
	 (corrected-vscroll (/ corrected-pixel-vscroll pixel-per-col)))
    (set-window-vscroll window
			(if pixels-p corrected-pixel-vscroll corrected-vscroll)
			pixels-p)))

;; Most of the scrolling functions here exist because of our handling
;; of centering in `emureader--center-page'.

(defun emureader--get-prefix-width (&optional window)
  "Get the line prefix width set by `emureader--center-page'.

WINDOW must be a valid window and defaults to the selected one."
  (car
   (plist-get
    (cdr (overlay-get (emureader-current-doc-overlay window) 'line-prefix))
    :width)))

(defun emureader--right-most-window-hscroll (&optional window)
  "Get the maximum horizontal scroll value for WINDOW.

This position is at the rightmost point.
WINDOW must be a valid window and defaults to the selected one."
  (let* ((image-width (car (emureader--get-current-doc-image-size window)))
	 (line-prefix-width (emureader--get-prefix-width window))
	 (pixel-window-width (window-body-width window t))
	 (max-ncol (round (/ (max line-prefix-width
				  (- image-width pixel-window-width))
			     (emureader--get-pixel-per-col window)))))
    max-ncol))

(defun emureader--set-window-hscroll (window ncol &optional unconstrained)
  "Set number of columns WINDOW is scrolled from left margin to NCOL.

If setting NCOL makes the document page disappear, it sets the maximum
horizontal scroll possible without doing that.  If UNCONSTRAINED is
non-nil, it allows setting NCOL even if it makes the page disappear.

See also `set-window-hscroll'."
  (let* ((line-prefix-width (emureader--get-prefix-width window))
	 (pixel-per-col (emureader--get-pixel-per-col window))
	 (calibrated-ncol (round (- (/ line-prefix-width pixel-per-col) ncol)))
	 (max-ncol (emureader--right-most-window-hscroll window))
	 (ncol (if unconstrained
		   calibrated-ncol
		 (min calibrated-ncol max-ncol))))
    (set-window-hscroll window ncol)
    (emureader--window-hscroll window)))

(defun emureader--window-hscroll (&optional window)
  "Return the number of columns by which WINDOW is scrolled from left margin.

WINDOW must be a live window and defaults to the selected one.

This correctly handles the prefix width set by emureader documents and does
not return the actual horizontal scroll value; for that, see
`window-hscroll'."
  (let* ((line-prefix-width (emureader--get-prefix-width window))
	 (pixel-per-col (emureader--get-pixel-per-col window))
	 (hscroll (round (- (/ line-prefix-width pixel-per-col) (window-hscroll window)))))
    hscroll))

;; We need this hack involving line-prefix because Emacs' scrolling is
;; idiosyncratic, and doesn't allow arbitrary scrolling in every
;; direction.
(defun emureader--center-page (&optional window)
  "Center the document with respect to WINDOW.

WINDOW must be a valid window and defaults to the selected one."
  (when-let* ((overlay (emureader-current-doc-overlay window))
              (_ (overlayp overlay))
	      (window-width (window-body-width window t))
              (doc-image-width (car (emureader--get-current-doc-image-size window)))
	      (max-left-offset (max 0 (- window-width doc-image-width)))
	      (overlay-offset `(space :width (,max-left-offset)))
	      (pixel-per-col (emureader--get-pixel-per-col window))
	      (doc-left-offset (- window-width doc-image-width))
              (doc-center-offset (/ doc-left-offset 2))
              (scroll-offset (round (/ doc-center-offset pixel-per-col))))
    ;; Add prefix so that the page is at the leftmost point of the window.
    (overlay-put overlay 'line-prefix overlay-offset)
    ;; scroll window back to the center of the doc
    (emureader--set-window-hscroll window scroll-offset t)))

(defun emureader-scroll-up (&optional amount)
  "Scroll up the current page by AMOUNT.

AMOUNT defaults to 1."
  (interactive "p")
  (or amount (setq amount 1))
  (let* ((prev-scroll (window-vscroll))
	 (vscroll (- prev-scroll amount)))
    (- prev-scroll (emureader--set-window-vscroll nil vscroll))))

(defun emureader-scroll-down (&optional amount)
  "Scroll down the current page by AMOUNT.

AMOUNT defaults to 1."
  (interactive "p")
  (or amount (setq amount 1))
  (let* ((prev-scroll (window-vscroll))
	 (vscroll (+ prev-scroll amount)))
    (- (emureader--set-window-vscroll nil vscroll) prev-scroll)))

(defun emureader-scroll-up-screen (&optional amount)
  "Scroll up the current page by a screen.

Unless AMOUNT is specified in which case we scroll by that value."
  (interactive "P")
  (let ((amount (or amount (- (window-body-height)
			      next-screen-context-lines))))
    (when (= 0 (emureader-scroll-up amount))
      (message "Beginning of page"))))

(defun emureader-scroll-down-screen (&optional amount)
  "Scroll down the current page by a screen.

Unless AMOUNT is specified in which case we scroll by that value."
  (interactive "P")
  (let ((amount (or amount (- (window-body-height)
			      next-screen-context-lines))))
    (when (= 0 (emureader-scroll-down amount))
      (message "End of page"))))

(defun emureader-scroll-left (&optional amount)
  "Scroll to the left of the current page by AMOUNT.

AMOUNT defaults to 1.

Only scrolls when the document page width is larger then the window width."
  (interactive "p")
  (or amount (setq amount 1))
  (when-let* ((_ (< (window-body-width nil t) (car (emureader--get-current-doc-image-size))))
	      (prev-scroll (emureader--window-hscroll))
	      (hscroll (+ prev-scroll amount)))
    (- (emureader--set-window-hscroll nil hscroll) prev-scroll)))

(defun emureader-scroll-right (&optional amount)
  "Scroll to the right of the current page by AMOUNT (or 1).

Only scrolls when the document page width is larger then the window width."
  (interactive "p")
  (or amount (setq amount 1))
  (when-let* ((_ (< (window-body-width nil t) (car (emureader--get-current-doc-image-size))))
	      (prev-scroll (emureader--window-hscroll))
	      (hscroll (- prev-scroll amount)))
    (- prev-scroll (emureader--set-window-hscroll nil hscroll))))

(defun emureader-scroll-left-most ()
  "Scroll to the left most point of the current page.

Only scrolls when the document page width is larger then the window width."
  (interactive)
  (when (< (window-body-width nil t) (car (emureader--get-current-doc-image-size)))
    (emureader--set-window-hscroll nil 0)))

(defun emureader-scroll-right-most ()
  "Scroll to the right most point of the current page.

Only scrolls when the document page width is larger then the window width."
  (interactive)
  (when (< (window-body-width nil t) (car (emureader--get-current-doc-image-size)))
    ;; We use `set-window-hscroll' here because we need to go the right
    ;; most point directly, bypassing `emureader--set-window-hscroll' checks.
    (set-window-hscroll nil (emureader--right-most-window-hscroll))))

(define-queue-command scroll-up-or-prev-page (&optional amount)
  "Scroll up the current page by AMOUNT, otherwise switch to the previous page.

AMOUNT defaults to 1."
  (interactive "p")
  (or amount (setq amount 1))
  (when-let* ((_ (and (= 0 (emureader-scroll-up amount))
		      (emureader--non-queue-previous-page))) ; if succeeds
	      (image-height (cdr (emureader--get-current-doc-image-size)))
	      (pixel-window-height (window-body-height nil t))
	      (bottom-most-scroll-pixel
	       (- image-height pixel-window-height)))
    (emureader--set-window-vscroll nil bottom-most-scroll-pixel t)))

(define-queue-command scroll-down-or-next-page (&optional amount)
  "Scroll down the current page by AMOUNT, otherwise switch to the next page.

AMOUNT defaults to 1."
  (interactive "p")
  (or amount (setq amount 1))
  (when (and (= 0 (emureader-scroll-down amount))
	     (emureader--non-queue-next-page)) ; if succeeds
    (emureader--set-window-vscroll nil 0)))

(define-queue-command scroll-up-screen-or-prev-page ()
  "Scroll up the current page by screen, otherwise switch to the previous page."
  (interactive)
  (let ((scroll (- (window-body-height)
		   next-screen-context-lines)))
    (emureader--non-queue-scroll-up-or-prev-page scroll)))

(define-queue-command scroll-down-screen-or-next-page ()
  "Scroll down the current page by screen, otherwise switch to the next page."
  (interactive)
  (let ((scroll (- (window-body-height)
		   next-screen-context-lines)))
    (emureader--non-queue-scroll-down-or-next-page scroll)))

(define-queue-command mwheel-scroll-up (event)
  "Scroll up or switch to the previous page, but also handle mouse EVENT.

See also `emureader-non-queue-scroll-up-or-prev-page'."
  (interactive "e")
  (let* ((event-type (car event))
	 (amount (pcase event-type
		   ('wheel-up 1)
		   ('double-wheel-up 2)
		   ('triple-wheel-up 3)))
	 (scrolled-window (car (cadr event))))
    (with-selected-window scrolled-window
      (emureader--non-queue-scroll-up-or-prev-page amount))))

(define-queue-command mwheel-scroll-down (event)
  "Scroll down or switch to the next page, but also handle mouse EVENT.

See also `emureader--non-queue-scroll-down-or-next-page'."
  (interactive "e")
  (let* ((event-type (car event))
	 (amount (pcase event-type
		   ('wheel-down 1)
		   ('double-wheel-down 2)
		   ('triple-wheel-down 3)))
	 (scrolled-window (car (cadr event))))
    (with-selected-window scrolled-window
      (emureader--non-queue-scroll-down-or-next-page amount))))

(defun emureader-mwheel-scroll-left (event)
  "Scroll to the left, but also handle mouse EVENT.

See also `emureader-scroll-left'."
  (interactive "e")
  (let* ((event-type (car event))
	 (amount (pcase event-type
		   ('S-wheel-up 1)
		   ('S-double-wheel-up 2)
		   ('S-triple-wheel-up 3)))
	 (scrolled-window (car (cadr event))))
    (with-selected-window scrolled-window
      (emureader-scroll-left amount))))

(defun emureader-mwheel-scroll-right (event)
  "Scroll to the right, but also handle mouse EVENT.

See also `emureader-scroll-right'."
  (interactive "e")
  (let* ((event-type (car event))
	 (amount (pcase event-type
		   ('S-wheel-down 1)
		   ('S-double-wheel-down 2)
		   ('S-triple-wheel-down 3)))
	 (scrolled-window (car (cadr event))))
    (with-selected-window scrolled-window
      (emureader-scroll-right amount))))

(defun emureader-mwheel-enlarge-size (event)
  "Enlarge the current page, but also handle mouse EVENT.

See also `emureader-enlarge-size'."
  (interactive "e")
  (let* ((event-type (car event))
	 (scaling-factor (pcase event-type
			   ('C-wheel-up emureader-enlarge-factor)
			   ('C-double-wheel-up (+ emureader-enlarge-factor 0.1))
			   ('C-triple-wheel-up (+ emureader-enlarge-factor 0.2))))
	 (scrolled-window (car (cadr event))))
    (with-selected-window scrolled-window
      (emureader-enlarge-size
       (* scaling-factor (emureader-current-doc-scale-value))))))

(defun emureader-mouse-select-window (event)
  "Select the window.

EVENT must be a click event."
  (interactive "e")
  (when-let* ((end-window (posn-window (event-end event)))
	      (_ (window-live-p end-window)))
    (select-window end-window)))

(defun emureader-mwheel-shrink-size (event)
  "Shrink the current page, but also handle mouse EVENT.

See also `emureader-shrink-size'."
  (interactive "e")
  (let* ((event-type (car event))
	 (scaling-factor (pcase event-type
			   ('C-wheel-down emureader-shrink-factor)
			   ('C-double-wheel-down (- emureader-shrink-factor 0.1))
			   ('C-triple-wheel-down (- emureader-shrink-factor 0.2))))
	 (scrolled-window (car (cadr event))))
    (with-selected-window scrolled-window
      (emureader-shrink-size
       (* scaling-factor (emureader-current-doc-scale-value))))))

(defun emureader-rotate-clockwise ()
  "Rotate the current window's view by 90 degrees, clockwise."
  (interactive)
  (let ((inhibit-quit t))
    (emureader-dyn--rotate-page 90)
    (emureader--center-page)))

(defun emureader-rotate-counter-clockwise ()
  "Rotate the current window's view by 90 degrees, counter-clockwise."
  (interactive)
  (let ((inhibit-quit t))
    (emureader-dyn--rotate-page -90)
    (emureader--center-page)))

(defun emureader--clear-doc-memory ()
  "Clear document memory if we are in `emureader-mode'.

Intended for use in `kill-buffer-hook'."
  (when (derived-mode-p 'emureader-mode)
    (emureader-dyn--close-doc)))

(defun emureader-close-doc ()
  "Close the current document, also prompt the user for confirmation."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to close the current document?")
    (kill-buffer (current-buffer))))

(defun emureader--restore-display-on-quit (page scale)
  "Reload the document with PAGE at SCALE and rebuild window overlays on quit.

Quit is deferred inside so the recovery cannot itself be interrupted."
  (with-local-quit
    (condition-case nil
        (progn
          (emureader--render-buffer (1- page))
          (emureader-doc-scale-page scale)
          (emureader--center-page)
          (emureader--manage-window-overlays))
      (error nil))))

(define-queue-command refresh-doc-buffer (&optional ignore-auto noconfirm preserve-modes)
  "Refresh the buffer document with file on disk.

It reloads the entire document while preserving the previous state.
The arguments are ignored and are simply to satisfy the template for
`revert-buffer-function'.

This function is replaced as `revert-buffer-function' for
`emureader-mode' buffers."
  (interactive)
  ;; Can't just prefix with _ because they get used when constructing
  ;; the function definitions.
  (ignore ignore-auto noconfirm preserve-modes)
  ;; A buffer that no window shows is skipped.
  (when-let* ((_ (and buffer-file-name (not emureader--reloading)))
	      (win (get-buffer-window nil t)))
    (setq emureader--reloading t)
    (unwind-protect
	(with-selected-window win
	  (let ((page (or (emureader-current-doc-pagenumber) 1))
		(scale (emureader-current-doc-scale-value)))
	    (message "Reloading your document...Press C-g to stop.")
	    (condition-case err
		(progn
		  (emureader--render-buffer (1- page))
		  (emureader-dyn--doc-invert (if (bound-and-true-p emureader-dark-mode) 1 0))
		  (emureader-doc-scale-page scale)
		  (emureader--center-page)
		  (emureader--manage-window-overlays))
	      ((quit error)
	       (message "Reloading interrupted; restoring display...")
	       (emureader--restore-display-on-quit page scale)
	       (signal (car err) (cdr err))))))
      (setq-local emureader--reloading nil))))

(defun emureader--render-buffer (&optional page)
  "Render the document file current buffer is associated with.
Load the PAGE if provided."
  (interactive)
  (if-let* ((file (buffer-file-name (current-buffer))))
      (when-let* ((_ (file-exists-p file))
		  (page (if (numberp page) (max 0 page) 0)))
	(emureader-dyn--load-doc file page)
	;; emureader-dyn--load-doc resets the window state, which also
	;; needs to be reflected in the frontend.
	(set-window-parameter nil 'scale 1.0))
    (message "No file associated with buffer.")))

;; We have an overlay per window, since we (naturally) need to be able
;; show different pages on different windows.
;;
;; The window knows (as a window parameter):
;; * Which overlay it belongs to.
;; * Which page number it's currently displaying.
;; * What the current scale is.
;;
;; The overlay knows (as an overlay property):
;; * Which window it belongs to.
;; * user pointer to the underlying EmacsWinState from the native
;; module.
;;
;; Window and overlay need to know each other for efficient lookup.

(defun emureader--manage-window-overlays ()
  "Manage the creation and deletion of window overlays as needed.

It is hooked to `window-configuration-change-hook' to keep detecting."
  (let ((overlays (car (overlay-lists)))
	(windows (get-buffer-window-list (current-buffer) nil t)))
    (mapc #'emureader--window-close-function overlays)
    (mapc #'emureader--window-create-function windows)))

(defun emureader--window-create-function (window)
  "Create window overlay for WINDOW."
  ;; Only buffer-local state is used: the window's own  parameters
  ;; may be left over from another buffer.
  (with-selected-window window
    (unless (emureader--window-owned-p window)
      (when-let* ((old (window-parameter window 'overlay))
		  (_ (overlayp old)))
	(emureader-dyn--window-close old))
      (set-window-parameter window 'overlay nil)
      (emureader-dyn--window-create window)
      ;; Scale first so goto-page renders and centers once at the final scale.
      (emureader-doc-scale-page emureader--recent-scale-fallback)
      (emureader-goto-page (or emureader--recent-pagenumber-fallback 1)))))

(defun emureader--window-close-function (overlay)
  "Properly close the window belonging to OVERLAY."
  (let ((window (overlay-get overlay 'window)))
    (unless (window-valid-p window)
      (emureader-dyn--window-close overlay))))

(defun emureader--handle-window-buffer-change (window)
  "Handle overlay state when WINDOW's buffer changes.
Cleans up stale overlays if the window switched to a different buffer,
and ensures a valid overlay exists if the new buffer is a emureader buffer."
  (when-let* ((_ (windowp window))
	      (overlay (window-parameter window 'overlay))
	      (_ (overlayp overlay)))
    (unless (eq (overlay-buffer overlay) (window-buffer window))
      ;; Overlay belongs to a different buffer. Clean it up.
      (emureader-dyn--window-close overlay)
      (set-window-parameter window 'overlay nil))
    ;; If the new buffer is a emureader buffer and lacks an overlay, create one.
    (when (with-current-buffer (window-buffer window)
            (derived-mode-p 'emureader-mode))
      (unless (window-parameter window 'overlay)
        (emureader--window-create-function window)))))


(declare-function emureader-outline-show "emureader-outline")
;; We explicitly bind default keys because relying on the rebinding
;; mechanism can lead to them not being bound at all if users have
;; overridden the defaults with other commands.
(defvar-keymap emureader-mode-map
  :doc "Keymap for `emureader-mode'."
  "n"       #'emureader-next-page
  "p"       #'emureader-previous-page

  "C-p"     #'emureader-scroll-up-or-prev-page
  "C-n"     #'emureader-scroll-down-or-next-page
  "<remap> <previous-line>" #'emureader-scroll-up-or-prev-page
  "<remap> <next-line>" #'emureader-scroll-down-or-next-page
  "<remap> <next>" #'emureader-scroll-down-or-next-page
  "<remap> <prior>" #'emureader-scroll-up-or-prev-page

  "<wheel-up>" #'emureader-mwheel-scroll-up
  "<wheel-down>" #'emureader-mwheel-scroll-down

  "S-<wheel-up>" #'emureader-mwheel-scroll-left
  "S-<wheel-down>" #'emureader-mwheel-scroll-right

  "C-v"      #'emureader-scroll-down-screen
  "M-v"      #'emureader-scroll-up-screen
  "<remap> <scroll-down-command>" #'emureader-scroll-up-screen
  "<remap> <scroll-up-command>" #'emureader-scroll-down-screen

  "SPC"     #'emureader-scroll-down-screen-or-next-page
  "DEL"     #'emureader-scroll-up-screen-or-prev-page
  "S-SPC"   #'emureader-scroll-up-screen-or-prev-page

  "C-f"     #'emureader-scroll-right
  "C-b"     #'emureader-scroll-left
  "<remap> <forward-char>" #'emureader-scroll-right
  "<remap> <backward-char>" #'emureader-scroll-left

  "C-e"     #'emureader-scroll-right-most
  "C-a"     #'emureader-scroll-left-most
  "<remap> <move-end-of-line>" #'emureader-scroll-right-most
  "<remap> <move-beginning-of-line>" #'emureader-scroll-left-most

  "M-<" #'emureader-first-page
  "M->" #'emureader-last-page
  "<remap> <beginning-of-buffer>" #'emureader-first-page
  "<remap> <end-of-buffer>" #'emureader-last-page

  "g" #'emureader-refresh-doc-buffer

  "M-g g"   #'emureader-goto-page
  "M-g M-g" #'emureader-goto-page
  "<remap> <goto-line>"   #'emureader-goto-page

  "="       #'emureader-enlarge-size
  "+"       #'emureader-enlarge-size
  "C-<wheel-up>" #'emureader-mwheel-enlarge-size

  "-"       #'emureader-shrink-size
  "C-<wheel-down>" #'emureader-mwheel-shrink-size

  "0"       #'emureader-reset-size

  "H"       #'emureader-fit-to-height
  "W"       #'emureader-fit-to-width

  "r"       #'emureader-rotate-clockwise
  "R"       #'emureader-rotate-counter-clockwise

  "o"       #'emureader-outline-show

  "Q"       #'emureader-close-doc


  ;; Disable a few mouse commands to make sure the user is not able to
  ;; accidentally move the point to a weird place, which causes the
  ;; cursor (which we use as a page border) to be drawn after the
  ;; page.

  "<remap> <mouse-set-point>"   #'emureader-mouse-select-window
  "<remap> <mouse-set-region>"  #'emureader-mouse-select-window
  "<remap> <mouse-set-mark>"    #'emureader-mouse-select-window
  "<remap> <mouse-set-secondary>"  #'emureader-mouse-select-window
  "<remap> <mouse-drag-region>"    #'emureader-mouse-select-window
  "<remap> <mouse-drag-secondary>" #'emureader-mouse-select-window
  "<remap> <mouse-save-then-kill>" #'emureader-mouse-select-window
  "<remap> <mouse-secondary-save-then-kill>" #'emureader-mouse-select-window)

;;;###autoload
(define-derived-mode emureader-mode special-mode "EμReader"
  "Major mode for viewing documents in The EMuReader.

Keybindings:
\\{emureader-mode-map}"
  :group 'emureader
  :interactive nil

  (emureader-ensure-module)

  (unless (and buffer-file-name
	       (file-exists-p buffer-file-name)
	       (file-regular-p buffer-file-name)
	       (file-readable-p buffer-file-name))
    (message "EMuReader did not open the document: %s is not a valid file"
             (or buffer-file-name "<no file>"))
    (kill-buffer (current-buffer)))

  (setq-local buffer-read-only t
	      global-linum-mode nil
	      cursor-type 'hollow
	      display-line-numbers-mode nil
	      ;; messes up centering(line-prefix) otherwise, the
	      ;; default value is 8, so we keep it at that.
	      left-fringe-width 8)
  (set-buffer-modified-p nil)
  (blink-cursor-mode 0)
  (auto-revert-mode 1)
  (setq-local revert-buffer-function #'emureader--non-queue-refresh-doc-buffer)

  ;; Disable `pixel-scroll-precision-mode' locally because it doesn't
  ;; work nicely with `emureader-mode'.
  (when (bound-and-true-p pixel-scroll-precision-mode)
    (setq-local pixel-scroll-precision-mode nil))

  (declare-function emureader-bookmark-make-record "emureader-bookmark")
  (setq-local bookmark-make-record-function
	      #'emureader-bookmark-make-record)

  (unless (bound-and-true-p
	   emureader-current-doc-render-status)
    (emureader--render-buffer))

  (declare-function emureader--outline-imenu-goto "emureader-outline")
  (declare-function emureader--outline-imenu-create-index "emureader-outline")
  (setq-local imenu-create-index-function #'emureader--outline-imenu-create-index
	      imenu-default-goto-function #'emureader--outline-imenu-goto)

  (run-hooks 'emureader-mode-hook)

  (funcall emureader-default-fit)
  (add-hook 'window-size-change-functions #'emureader--center-page nil t)

  ;;`bookmark--jump-via' messes up with centering, so we need to center again.
  (add-hook 'bookmark-after-jump-hook #'emureader--center-page nil t)
  ;; Depth 90 so that it runs after `save-place-to-alist' so scroll/page
  ;; are saved before the document state is cleared.
  (add-hook 'kill-buffer-hook #'emureader--clear-doc-memory 90 t)
  (add-hook 'window-configuration-change-hook #'emureader--manage-window-overlays nil t)
  (add-hook 'window-buffer-change-functions #'emureader--handle-window-buffer-change nil t))

(defun emureader-mode-line ()
  "Set custom mode-line interface when reading documents."
  (setq-local mode-line-position
	      '(" P" (:eval (number-to-string (emureader-current-doc-pagenumber)))
                "/" (:eval (number-to-string emureader-current-doc-pagecount)))))

(add-hook 'emureader-mode-hook #'emureader-mode-line)
(add-hook 'emureader-mode-hook #'imenu-add-menubar-index)

(defun emureader--redisplay-all-windows ()
  "Redisplay the current document in all windows showing this buffer."
  (let ((inhibit-quit t))
    (dolist (win (get-buffer-window-list nil nil t))
      (with-selected-window win
	(emureader-dyn--redisplay-doc)))))

(define-minor-mode emureader-dark-mode
  "Toggle dark-mode for current emureader document."
  :lighter " Dark"
  (when (derived-mode-p 'emureader-mode)
    (emureader-dyn--doc-invert (if emureader-dark-mode 1 0))
    (emureader--redisplay-all-windows)))

(define-minor-mode emureader-presentation-mode
  "Toggle presentation view for current emureader document."
  :lighter ""
  (when (derived-mode-p 'emureader-mode)
    (cond (emureader-presentation-mode
	   (setq-local mode-line-format nil)
	   (emureader-fit-to-height))
	  ((kill-local-variable 'mode-line-format)))))

;;;###autoload
(dolist (pattern emureader-supported-formats)
  (add-to-list 'auto-mode-alist (cons (concat "\\." pattern "\\'") 'emureader-mode)))

(provide 'emureader)
;;; emureader.el ends here.

;; Local Variables:
;; read-symbol-shorthands: (("define-queue-command" . "emureader--define-queue-command"))
;; End:
