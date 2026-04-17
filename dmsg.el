;;; dmsg.el --- Timestamped debug messages with backtrace support -*- lexical-binding: t -*-

;; Copyright (C) 2026
;; Author: Al Haji-Ali <abdo.haji.ali@gmail.com>
;; URL: https://github.com/haji-ali/dmsg.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: maint, tools
;;
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
;;
;; `dmsg' writes structured entries to a dedicated buffer and provides
;; `dmsg-mode' to interact with the buffer.
;;
;; Buffer format:
;;
;;   [LVL] [YYYY-MM-DD HH:MM:SS.mmm] first line of message
;;    continuation line               (exactly one leading space per \n)
;;   (fn-name args)                backtrace frame
;;   (fn-name ...)                unevaluated frame
;;
;;
;; Default keys (dmsg-mode):
;;   <tab>   Toggle compact fn <- fn <- fn chain for entry at point
;;   b       Open detailed backtrace for entry at point in a side window
;;   c       Clear entries without modifying buffer (toggle)
;;   e       Erase buffer (destructive)
;;   f       Filter entries by regexp
;;   F       Clear regexp filter
;;   s       Snapshot visible entries to a .log file
;;   l1-l4   Set minimum display level (l1=debug l2=info l3=warn l4=error)
;;
;; Usage:
;;   (require 'dmsg)
;;   (dmsg "value is %S" x)
;;   (dmsg 'warn "something odd: %=S" x)

;;; Code:

(require 'cl-lib)

(defconst dmsg--levels
  '((debug "DBG" dmsg-level-debug-face)
    (info  "INF" dmsg-level-info-face)
    (warn  "WRN" dmsg-level-warn-face)
    (error "ERR" dmsg-level-error-face))
  "Level definitions in increasing severity order.
Each element: (SYMBOL LABEL FACE-SYMBOL).")

(defconst dmsg--level-order
  (mapcar #'car dmsg--levels)
  "Level symbols ordered least-to-most severe, derived from `dmsg--levels'.")

(defconst dmsg--header-re
  (concat "^\\* \\("
          (mapconcat #'cadr dmsg--levels "\\|")
          "\\) \\[\\([^]]+\\)\\]")
  "Regexp matching an entry header at column 0.
Group 1: label (e.g. \"DBG\").  Group 2: timestamp string.
After `(match-end 0)' is an optional space then the first message line.")

(defgroup dmsg nil
  "Timestamped debug messages with collapsible backtraces."
  :group 'development
  :prefix "dmsg-")

(defcustom dmsg-buffer-name "*DEBUG*Messages*"
  "Name of the buffer where debug messages are collected."
  :type 'string :group 'dmsg)

(defcustom dmsg-backtrace-buffer-name "*DEBUG*Backtrace*"
  "Name of the buffer used to display detailed backtraces."
  :type 'string :group 'dmsg)

(defcustom dmsg-min-level 'debug
  "Minimum severity level to display.
Entries below this level are hidden by an overlay — never deleted.
Changing this via `customize' or `dmsg-set-min-level' refreshes the buffer."
  :type `(choice ,@(mapcar (lambda (e)
                             `(const :tag ,(capitalize (symbol-name (car e)))
                                     ,(car e)))
                           dmsg--levels))
  :set (lambda (sym val)
         (set-default sym val)
         (when-let* ((buf (and (boundp 'dmsg-buffer-name)
                               (get-buffer dmsg-buffer-name))))
           (with-current-buffer buf
             (when (derived-mode-p 'dmsg-mode)
               (dmsg--refresh-visibility)))))
  :group 'dmsg)

(defcustom dmsg-show-caller t
  "If non-nil, append a clickable caller tag to each entry header."
  :type 'boolean :group 'dmsg)

(defcustom dmsg-max-entries nil
  "When non-nil, hide the oldest entries that exceed this count.
Buffer text is never deleted by this limit.
Changing this via `customize' refreshes the buffer."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :set (lambda (sym val)
         (set-default sym val)
         (when-let* ((buf (and (boundp 'dmsg-buffer-name)
                               (get-buffer dmsg-buffer-name))))
           (with-current-buffer buf
             (when (derived-mode-p 'dmsg-mode)
               (dmsg--refresh-visibility)))))
  :group 'dmsg)

(defcustom dmsg-message-continuation-indent "  "
  "String to indent message continuation lines."
  :type 'string :group 'dmsg)

(defcustom dmsg-compact-skip-functions
  '("edebug.*" debug-after apply funcall (pred special-form-p))
  "Functions to omit from the compact backtrace chain.
Each element: a symbol (eq match), a regexp string (name match),
or a list (pred FN) where FN is called with the symbol."
  :type '(repeat (choice symbol regexp)) :group 'dmsg)

(defcustom dmsg-detailed-arg-max-length 100
  "Maximum displayed characters per argument in the detailed backtrace."
  :type 'integer :group 'dmsg)

(defface dmsg-timestamp-face
  '((t :foreground "gray50" :weight light))
  "Timestamp."
  :group 'dmsg)
(defface dmsg-caller-face
  '((t :foreground "medium sea green" :underline t))
  "Caller tag."
  :group 'dmsg)
(defface dmsg-compact-bt-face
  '((t :foreground "gray55" :slant italic)) "Compact chain." :group 'dmsg)
(defface dmsg-level-debug-face
  '((t :foreground "gray60"))              "DEBUG." :group 'dmsg)
(defface dmsg-level-info-face
  '((t :foreground "deep sky blue"))       "INFO."  :group 'dmsg)
(defface dmsg-level-warn-face
  '((t :foreground "orange" :weight bold)) "WARN."  :group 'dmsg)
(defface dmsg-level-error-face
  '((t :foreground "tomato" :weight bold)) "ERROR." :group 'dmsg)

;;;; Level data accessors
(defsubst dmsg--level-label (lvl)
  "Access LVL's label from `dmsg--levels'."
  (cadr  (assq lvl dmsg--levels)))
(defsubst dmsg--level-face  (lvl)
  "Access LVL's face from `dmsg--levels'."
  (caddr (assq lvl dmsg--levels)))
(defsubst dmsg--label->level (lbl)
  "Find level symbol given label string LBL."
  (car (cl-find lbl dmsg--levels :key #'cadr :test #'equal)))

;;;; Buffer-local state

(defvar-local dmsg--entry-count 0
  "Total entry count including hidden entries.
Incremented by `dmsg-write', reset by `dmsg--scan-buffer'.")

(defvar-local dmsg--visible-count 0
  "Count of currently visible (non-hidden) entries.
Maintained incrementally by `dmsg--on-new-entry' and
recomputed from scratch by `dmsg--refresh-visibility'.")

(defvar-local dmsg--filter-regexp nil
  "Active regexp filter string, or nil.  Stored separately for header display.")

(defvar-local dmsg--hide-predicates nil
  "Alist of (KEY . PRED).
PRED is called with the buffer position of an entry header; non-nil means
hide that entry.  Managed via `dmsg--set-predicate'.
Built-in keys used internally: `regexp', `clear'.")

(defvar dmsg-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [tab] #'dmsg-toggle-compact)
    (define-key m "b" #'dmsg-show-backtrace)
    (define-key m "c" #'dmsg-clear)
    (define-key m "e" #'dmsg-erase)
    (define-key m "f" #'dmsg-filter)
    (define-key m "F" #'dmsg-filter-clear)
    (define-key m "s" #'dmsg-snapshot)
    ;; Level shortcuts: l1=debug l2=info l3=warn l4=error
    (cl-loop for lvl in dmsg--level-order
             for key from ?1
             do (let ((l lvl))
                  (define-key m (concat "l" (char-to-string key))
                    (lambda () (interactive) (dmsg-set-min-level l)))))
    m)
  "Keymap for `dmsg-mode'.")

(defun dmsg-jump-to-def (&optional e)
  "Jump to the definition of the dmsg function label at point or event E."
  (interactive "e")
  (when-let* ((fn (get-text-property
                   (if e
                       (posn-point (event-start e))
                     (point))
                   'dmsg-fn)))
    (condition-case err (find-function fn)
      (error (message "%s" (error-message-string err))))))

(defvar dmsg--fn-keymap
  (let ((km (make-sparse-keymap)))
    (define-key km [mouse-1] #'dmsg-jump-to-def)
    (define-key km (kbd "RET") #'dmsg-jump-to-def)
    km)
  "Keymap for dmsg function-jump labels.
Reads the target symbol from the `dmsg-fn' text property at point.")

(define-derived-mode dmsg-mode special-mode "DMsg"
  "Major mode for `dmsg' output.  Buffer text is the sole persistent state.
Save the buffer, reopen it and re-enable this mode to restore interactivity.

\\{dmsg-mode-map}"
  (setq-local truncate-lines t)
  ;; dmsg--teardown runs on any mode change, including switch to
  ;; fundamental-mode.
  (add-hook 'change-major-mode-hook #'dmsg--teardown nil t)
  (dmsg--scan-buffer)
  (dmsg--refresh-visibility))

(defun dmsg--teardown ()
  "Remove all dmsg overlays and text properties.
Runs on `change-major-mode-hook', covering the switch to `fundamental-mode'."
  (remove-overlays (point-min) (point-max) 'dmsg-ov t)
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max)
                            '(dmsg-entry nil invisible nil
                                         dmsg-level nil face nil)))
  (setq dmsg--entry-count    0
        dmsg--visible-count  0
        dmsg--filter-regexp  nil
        dmsg--hide-predicates nil)
  (kill-local-variable 'header-line-format))

(defun dmsg--make-ov (start end &rest props)
  "Create an overlay START–END tagged `dmsg-ov t', with additional PROPS.
The tag is required for `remove-overlays' calls in teardown and scan."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'dmsg-ov t)
    (cl-loop for (k v) on props by #'cddr do (overlay-put ov k v))
    ov))

;;;; Entry navigation

(defun dmsg--entry-end (entry-pos)
  "Return position just past all lines belonging to the entry at ENTRY-POS."
  (save-excursion
    (goto-char entry-pos)
    (forward-line 1)
    (while (and (not (eobp)) (not (looking-at dmsg--header-re)))
      (forward-line 1))
    (point)))

(defun dmsg--bt-start (entry-pos)
  "Return start of the `backtrace-frame' block for the entry at ENTRY-POS.
Skips the header line and any message-continuation lines (leading space)."
  (save-excursion
    (goto-char entry-pos)
    (forward-line 1)
    (while (and (not (eobp))
                (not (looking-at dmsg--header-re))
                (eq (char-after) ?\s))
      (forward-line 1))
    (point)))

(defun dmsg--entry-at-point ()
  "Return buffer position of the entry header at or enclosing point, or nil."
  (save-excursion
    (beginning-of-line)
    (catch 'found
      (while t
        (cond
         ((looking-at dmsg--header-re) (throw 'found (point)))
         ((bobp)                        (throw 'found nil))
         (t                             (forward-line -1)))))))

(defun dmsg--entry-message (entry-pos)
  "Return the full message string for the entry at ENTRY-POS.
Newlines are restored: each continuation line contributes one joined part."
  (save-excursion
    (goto-char entry-pos)
    (let (parts)
      ;; First line: text following the header (after "[LVL] [TS]" + optional space)
      (when (looking-at dmsg--header-re)
        (let ((from (match-end 0)))
          (when (and (< from (line-end-position))
                     (eq (char-after from) ?\s))
            (cl-incf from))
          (push (buffer-substring-no-properties from (line-end-position)) parts)))
      ;; Continuation lines: exactly one leading space
      (forward-line 1)
      (while (and (not (eobp))
                  (not (looking-at dmsg--header-re))
                  (eq (char-after) ?\s))
        (push (buffer-substring-no-properties (1+ (point)) (line-end-position)) parts)
        (forward-line 1))
      (mapconcat #'identity (nreverse parts) "\n"))))

(defun dmsg--scan-buffer ()
  "Scan all entries; set text properties and create display overlays.
Idempotent.  Does not modify buffer text."
  (dmsg--teardown)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (looking-at dmsg--header-re)
        (cl-incf dmsg--entry-count)
        (dmsg--apply-entry-display (point)))
      (forward-line 1))))

(defun dmsg--apply-entry-display (entry-pos)
  "Apply faces, hide BT lines, and add caller tag for the entry at ENTRY-POS.
Called from `dmsg--scan-buffer' and from `dmsg-write' after writing.
Never inserts, deletes, or modifies buffer text."
  (save-excursion
    (goto-char entry-pos)
    (when (looking-at dmsg--header-re)
      (let* ((level   (dmsg--label->level (match-string 1)))
             (level-start   (match-beginning 1))
             (ts-b    (match-beginning 2))
             (ts-e    (match-end 2))
             (hdr-end (line-end-position)))
        (with-silent-modifications
          ;; Hide anything before level
          (put-text-property entry-pos level-start 'invisible t)
          (put-text-property entry-pos (1+ hdr-end) 'dmsg-entry t)
          (put-text-property entry-pos (1+ hdr-end) 'dmsg-level level)
          (put-text-property entry-pos (1- ts-b)    'face (dmsg--level-face level))
          (put-text-property (1- ts-b)  (1+ ts-e)  'face 'dmsg-timestamp-face)
          ;; Message continuation lines: replace leading space with indent string.
          (forward-line 1)
          (while (and (not (eobp))
                      (not (looking-at dmsg--header-re))
                      (eq (char-after) ?\s))
            (put-text-property (point) (1+ (point))
                               'display dmsg-message-continuation-indent)
            (forward-line 1)))
        ;; Backtrace block: walk to the next header (or eob).
        ;; bt-end is simply (point) after the walk — do NOT use
        ;; (line-end-position) here, which would refer to the next entry.
        (let* ((bt-start (point))
               (bt-end   (progn
                           (while (and (not (eobp))
                                       (not (looking-at dmsg--header-re)))
                             (forward-line 1))
                           (point)))
               (chain    (dmsg--compact-chain bt-start bt-end)))
          (dmsg--make-ov bt-start bt-end
                         'dmsg-bt   (dmsg--compact-chain-string chain)
                         'invisible t
                         'category  'dmsg-bt)
          ;; Caller tag: zero-width overlay appending a button after the
          ;; timestamp.  Uses the shared keymap via `dmsg-fn' text property.
          (when-let* ((caller (and dmsg-show-caller (car chain))))
            (dmsg--make-ov (1+ ts-e) (+ ts-e 2)
                           'display
                           (concat " "
                                   (dmsg--buttonify-fn
                                    caller
                                    (format "[%s]" (symbol-name caller)))
                                   " ")
                           'category 'dmsg-caller)))))))

(defun dmsg--compact-chain (bt-start bt-end)
  "Return a list of non-skipped function symbols from BT-START to BT-END.
Frames are stored innermost-first; the returned list is also innermost-first,
so `(car chain)' is the direct caller of `dmsg'."
  (let (fns)
    (save-excursion
      (goto-char bt-start)
      (while (< (point) bt-end)
        (let* ((line (buffer-substring-no-properties
                      (point)
                      (line-end-position)))
               (fn (when (string-match "^(\\([^ )]+\\)" line)
                     (intern-soft (match-string 1 line)))))
          (when (and fn (not
                         (cl-some (lambda (pat)
                                    (cond
                                     ((eq (car-safe pat) 'pred)
                                      (funcall (cadr pat) fn))
                                     ((stringp pat)
                                      (string-match-p pat (symbol-name fn)))
                                     (t
                                      (eq fn pat))))
                                  dmsg-compact-skip-functions)))
            (push fn fns)))
        (forward-line 1)))
    (reverse fns)))

(defun dmsg--compact-chain-string (chain)
  "Return a display string for CHAIN (innermost-first list of symbols).
Renders as `outer ← … ← inner'.  Each name carries `dmsg-fn' and
`dmsg--fn-keymap' so the shared keymap can jump to its definition.
Long chains are wrapped at `fill-column' when set."
  (if (null chain)
      (propertize "  (no frames)\n" 'face 'dmsg-compact-bt-face)
    (let* ((sep (propertize " ← " 'face 'dmsg-compact-bt-face))
           (indent "    ")             ; continuation indent after wrap
           (col    2)                  ; current column (starts after "  ")
           (parts  (list "  ")))
      (cl-loop for (fn . rest) on (reverse chain) do
               (let* ((name (symbol-name fn))
                      (item (dmsg--buttonify-fn fn name)))
                 ;; Wrap before this item if it would exceed the column limit.
                 (when (and fill-column rest  ; never wrap after last
                            (> (+ col (length sep) (length name))
                               fill-column))
                   (push (concat "\n" indent) parts)
                   (setq col (length indent)))
                 (push item parts)
                 (cl-incf col (length name))
                 (when rest
                   (push sep parts)
                   (cl-incf col (length " ← ")))))
      (concat (apply #'concat parts) "\n"))))

;;;; Predicate-based filter system
(defun dmsg--set-predicate (key pred-or-nil)
  "Add or replace predicate KEY in `dmsg--hide-predicates', then refresh.
If PRED-OR-NIL is nil, remove KEY instead.  Triggers `dmsg--refresh-visibility'."
  (if pred-or-nil
      (setf (alist-get key dmsg--hide-predicates) pred-or-nil)
    (setq dmsg--hide-predicates (assq-delete-all key dmsg--hide-predicates)))
  (dmsg--refresh-visibility))

(defun dmsg--entry-hidden-p (pos)
  "Return non-nil if the entry at POS already has a hide overlay."
  (cl-some (lambda (ov) (eq (overlay-get ov 'category) 'dmsg-hide))
           (overlays-at pos)))

(defun dmsg--should-hide-p (pos)
  "Return non-nil if the entry at POS should be hidden.
Checks min-level and all active predicates; does NOT check max-entries."
  (let ((level (get-text-property pos 'dmsg-level)))
    (or (not
         (>= (cl-position level             dmsg--level-order)
             (cl-position dmsg-min-level  dmsg--level-order)))
        (cl-some (lambda (kp) (funcall (cdr kp) pos))
                 dmsg--hide-predicates))))

(defun dmsg--refresh-visibility ()
  "Recompute all hide overlays from scratch.
Called when predicates or settings change — not on individual new entries.
Uses `remove-overlays' to delete existing hide overlays (tagged
`category dmsg-hide'), then rebuilds them by scanning the buffer."
  (remove-overlays (point-min) (point-max) 'category 'dmsg-hide)
  (setq dmsg--visible-count 0)
  (let ((n 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'dmsg-entry)
          (cl-incf n)
          (let* ((pos    (point))
                 (hidden (or (and dmsg-max-entries
                                  (<= n (- dmsg--entry-count dmsg-max-entries)))
                             (dmsg--should-hide-p pos))))
            (if hidden
                (dmsg--make-ov pos (dmsg--entry-end pos)
                               'invisible t 'category 'dmsg-hide)
              (cl-incf dmsg--visible-count))))
        (forward-line 1))))
  (dmsg--update-header))

(defun dmsg--on-new-entry (entry-pos)
  "Apply visibility to the newly appended entry at ENTRY-POS.
Only this entry is examined — existing entries are unaffected.
Also hides the oldest newly-excess entry when `dmsg-max-entries' is active."
  ;; Step 1: hide the new entry if it fails level or predicate checks.
  (if (dmsg--should-hide-p entry-pos)
      (dmsg--make-ov entry-pos (dmsg--entry-end entry-pos)
                     'invisible t 'category 'dmsg-hide)
    (cl-incf dmsg--visible-count))
  ;; Step 2: if max-entries is exceeded, hide the oldest non-hidden excess entry.
  ;; The entry that just became excess is at 1-based index
  ;; (entry-count - max-entries) from the start of the buffer.
  (when (and dmsg-max-entries (> dmsg--entry-count dmsg-max-entries))
    (dmsg--hide-nth-entry (- dmsg--entry-count dmsg-max-entries)))
  (dmsg--update-header))

(defun dmsg--hide-nth-entry (n)
  "Hide the Nth entry (1-based, oldest first) if it is not already hidden.
Decrements `dmsg--visible-count' when the entry was previously visible."
  (catch 'done
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (not (eobp))
          (when (get-text-property (point) 'dmsg-entry)
            (cl-incf count)
            (when (= count n)
              (let ((pos (point)))
                (unless (dmsg--entry-hidden-p pos)
                  (dmsg--make-ov pos (dmsg--entry-end pos)
                                 'invisible t 'category 'dmsg-hide)
                  (cl-decf dmsg--visible-count))
                (throw 'done nil))))
          (forward-line 1))))))

(defun dmsg--update-header ()
  "Set `header-line-format' from buffer-local counts and active conditions."
  (setq header-line-format
        (concat
         (propertize " DMsg " 'face '(:weight bold))
         (propertize (format "%d/%d" dmsg--visible-count dmsg--entry-count)
                     'face 'dmsg-timestamp-face)
         (when dmsg--filter-regexp
           (propertize (format "  filter: %s" dmsg--filter-regexp)
                       'face '(:foreground "orange")))
         (when (assq 'clear dmsg--hide-predicates)
           (propertize "  [cleared — press c to restore]"
                       'face '(:foreground "gray50")))
         (unless (eq dmsg-min-level 'debug)
           (propertize (format "  min-level: %s" (dmsg--level-label dmsg-min-level))
                       'face '(:foreground "gray50"))))))

;;;###autoload
(defun dmsg-toggle-compact ()
  "Toggle the compact fn←fn←fn chain for the entry at point.
The chain is derived on-the-fly from the hidden backtrace lines.
Press <tab> again to hide it."
  (interactive)
  (let ((entry-pos (dmsg--entry-at-point)))
    (unless entry-pos (user-error "No dmsg entry at point"))
    (when-let* ((entry-end (dmsg--entry-end entry-pos))
                (bt-ov  (cl-find-if (lambda (ov) (eq
                                                  (overlay-get ov 'category)
                                                  'dmsg-bt))
                                    (overlays-in entry-pos entry-end))))
      (overlay-put bt-ov
                   'display
                   (and (not (overlay-get bt-ov 'display))
                        (overlay-get bt-ov 'dmsg-bt))))))

;;;###autoload
(defun dmsg-show-backtrace ()
  "Open the detailed backtrace for the entry at point in a side window."
  (interactive)
  (let ((entry-pos (dmsg--entry-at-point)))
    (unless entry-pos (user-error "No dmsg entry at point"))
    (let* ((hdr      (save-excursion
                       (goto-char entry-pos)
                       (buffer-substring-no-properties (point) (line-end-position))))
           (level     (and (string-match dmsg--header-re hdr)
                           (dmsg--label->level (match-string 1 hdr))))
           (timestamp (and (string-match dmsg--header-re hdr)
                           (match-string 2 hdr)))
           (msg-text  (dmsg--entry-message entry-pos))
           (bt-s      (dmsg--bt-start entry-pos))
           (bt-e      (dmsg--entry-end entry-pos))
           (bt-text (string-trim (buffer-substring-no-properties bt-s bt-e)))
           frames)
      (with-current-buffer (get-buffer-create dmsg-backtrace-buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "Debug Backtrace\n" 'face '(:weight bold :height 1.2)))
          (insert (propertize (format "Timestamp : %s\n" timestamp)
                              'face 'dmsg-timestamp-face))
          (insert (propertize (format "Level     : %s\n" (dmsg--level-label level))
                              'face (dmsg--level-face level)))
          (insert (format "Message   : %s\n" msg-text))
          (insert (make-string fill-column ?─) "\n\n")
          (if (string-empty-p bt-text)
              (insert "(no frames captured)\n")
            (let ((bt-start (point)))
              (insert bt-text "\n")
              ;; Make every function name in the BT block a clickable button.
              (save-excursion
                (goto-char bt-start)
                (while (not (eobp))
                  (when (looking-at "(\\([^ )]+\\)")
                    (let ((start (match-beginning 1))
                          (end (match-end 1)))
                      (dmsg--buttonify-fn
                       (intern-soft
                        (buffer-substring-no-properties start end))
                       nil start end)))
                  (forward-line 1)))))
          (goto-char (point-min)))
        (view-mode 1))
      (pop-to-buffer dmsg-backtrace-buffer-name
                     '((display-buffer-below-selected
                        display-buffer-at-bottom)
                       (window-height . 0.35))))))

(defun dmsg--buttonify-fn (fn object &optional start end)
  "Make the text from START to END in OBJECT a function-jump button.
FN is the symbol to jump to.  OBJECT is either a string (when building
a display string) or nil (when annotating an existing buffer region via
START/END buffer positions).  Returns OBJECT."
  (when fn
    (add-text-properties
     (or start 0) (or end (length object))
     (list 'face       'dmsg-caller-face
           'mouse-face 'highlight
           'help-echo
           (let ((overriding-local-map dmsg--fn-keymap))
             (substitute-command-keys
              (format "\\[dmsg-jump-to-def]: jump to `%s'" fn)))
           'dmsg-fn    fn
           'keymap     dmsg--fn-keymap)
     object)
    object))

(defmacro dmsg--imessage (&rest args)
  "Call `message' with ARGS only when the enclosing function was called interactively."
  `(when (called-interactively-p 'any)
     (message ,@args)))

;;;###autoload
(defun dmsg-clear ()
  "Toggle hiding all entries without modifying buffer text.
Call again to restore.  Use `dmsg-erase' to truly delete content."
  (interactive)
  (if (assq 'clear dmsg--hide-predicates)
      (progn (dmsg--set-predicate 'clear nil)
             (dmsg--imessage "dmsg: entries restored"))
    (dmsg--set-predicate 'clear (let ((cur-max (point-max)))
                                  (lambda (pos) (< pos cur-max))))
    (dmsg--imessage "dmsg: all entries hidden — press c to restore, e to erase")))

;;;###autoload
(defun dmsg-erase ()
  "Erase all buffer content.  Destructive — use `dmsg-clear' to hide only."
  (interactive)
  (when (or (not (called-interactively-p 'any))
            (y-or-n-p "Erase all dmsg entries? "))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (dmsg--teardown)
    (dmsg--update-header)
    (dmsg--imessage "dmsg: buffer erased")))

;;;###autoload
(defun dmsg-filter (regexp)
  "Show only entries whose message matches REGEXP.  Empty input clears filter."
  (interactive
   (list (let ((input (read-regexp
                       (format "Filter%s: "
                               (if dmsg--filter-regexp
                                   (format " (current: %s)"
                                           dmsg--filter-regexp)
                                 "")))))
           (if (string-empty-p input) nil input))))
  (setq dmsg--filter-regexp regexp)
  (dmsg--set-predicate
   'regexp
   (when regexp
     (let ((re regexp))
       (lambda (pos) (not (string-match-p re (dmsg--entry-message pos)))))))
  (if regexp
      (dmsg--imessage "dmsg filter: %s" regexp)
    (dmsg--imessage "dmsg filter cleared")))

;;;###autoload
(defun dmsg-filter-clear ()
  "Clear the active regexp filter."
  (interactive)
  (dmsg-filter nil))

;;;###autoload
(defun dmsg-set-min-level (level)
  "Set `dmsg-min-level' to LEVEL and refresh visibility.
Interactively, prompts for the level with completion.
Can also be invoked via `l1'–`l4' in `dmsg-mode'."
  (interactive
   (list (intern (completing-read
                  "Min level: "
                  (mapcar (lambda (e) (symbol-name (car e))) dmsg--levels)
                  nil t nil nil (symbol-name dmsg-min-level)))))
  (setq dmsg-min-level level)
  (dmsg--refresh-visibility)
  (dmsg--imessage "dmsg: min-level → %s" (dmsg--level-label level)))

;;;###autoload
(defun dmsg-snapshot (file)
  "Write currently visible entries to FILE."
  (interactive
   (list (let* ((default-name (format-time-string "dmsg-%Y%m%d-%H%M%S.log"))
                (path (read-file-name
                       (format "Snapshot to [default: %s]: " default-name)
                       default-directory
                       (expand-file-name default-name default-directory)
                       nil)))
           (if (file-directory-p path)
               (expand-file-name default-name path)
             path))))
  (unless (derived-mode-p 'dmsg-mode)
    (user-error "Not in a dmsg buffer"))
  (let* ((count 0)
         chunks)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'dmsg-entry)
          (let ((pos (point)))
            (unless (dmsg--entry-hidden-p pos)
              (cl-incf count)
              (push (buffer-substring-no-properties pos (dmsg--entry-end pos))
                    chunks))))
        (forward-line 1)))
    (with-temp-buffer
      (dolist (chunk (nreverse chunks))
        (insert chunk))
      (write-region (point-min) (point-max) file nil 'silent))
    (dmsg--imessage "dmsg: %d visible entr%s → %s"
                    count (if (= count 1) "y" "ies") file)))

;;;; Backtrace capture
(defun dmsg--format-arg (arg)
  "Format ARG, truncating to `dmsg-detailed-arg-max-length' characters."
  (let ((s (format "%S" arg)))
    (if (> (length s) dmsg-detailed-arg-max-length)
        (concat (substring s 0 (1- dmsg-detailed-arg-max-length)) "…")
      s)))

(defun dmsg--capture-bt ()
  "Return the current backtrace as a list of strings, innermost first.
All dmsg internal frames are excluded: the accumulator is reset to nil
each time a dmsg-prefixed function is encountered, so only frames that
are strictly outer to all dmsg machinery are returned."
  (let ((lines '()))
    (mapbacktrace
     (lambda (evald func args _flags)
       (if (and (symbolp func)
                (string-match "dmsg.*" (symbol-name func)))
           ;; Ignore all functions in dmsg, or those called by them
           (setq lines nil)
         (push (if evald
                   (format "(%s%s)" func
                           (if args
                               (concat " " (mapconcat #'dmsg--format-arg args " "))
                             ""))
                 (format "(%s ...)" func))
               lines))))
    ;; innermost-first
    (nreverse lines)))

;;;; Main entry point and message formatting

;;;###autoload
(defun dmsg-write (level str &optional no-bt)
  "Insert a timestamped debug entry into `dmsg-buffer-name'.
LEVEL is the log level symbol (debug/info/warn/error).
STR is the fully formatted message string.
When NO-BT is non-nil, no backtrace frames are written.

This is the low-level entry point.  Prefer the `dmsg' macro, which
automatically captures the call site, formats the message with `%='
label support, and provides a convenient syntax for specifying the level."
  (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S.%3N"))
         (msg-lines (split-string str "\n")))
    (with-current-buffer (get-buffer-create dmsg-buffer-name)
      (unless (derived-mode-p 'dmsg-mode) (dmsg-mode))
      (let ((inhibit-read-only t)
            (entry-start       (point-max)))
        (goto-char entry-start)
        ;; Entry header
        ;; "[LVL] [TIMESTAMP] " + first message line
        (insert (format "* %s [%s] %s\n"
                        (dmsg--level-label level)
                        timestamp
                        (car msg-lines)))
        ;; Continuation lines with leading space
        (dolist (line (cdr msg-lines))
          (insert " " line "\n"))
        ;; Backtrace frame lines
        (if-let* ((frames (and (not no-bt) (dmsg--capture-bt))))
            (dolist (frame frames)
              (insert frame "\n"))
          (insert "\n"))
        ;; Apply display to this entry
        (cl-incf dmsg--entry-count)
        (dmsg--apply-entry-display entry-start)
        ;; Update visibility for this entry only
        (dmsg--on-new-entry entry-start))
      (goto-char (point-max))
      (when-let* ((win (get-buffer-window (current-buffer))))
        (set-window-point win (point-max))))))

(defun dmsg--format (fmt args args-labels)
  "Format FMT like `format', with support for the `%=' labeled specifier.

`%=SPEC' formats its argument as \"label=value\" where \"value\" is taken
from ARGS and \"label\" taken from ARGS-LABELS.
`%N$=SPEC' does the same with a positional argument reference.
All other specifiers behave exactly as in `format'."
  (let ((i 0) start (len (length fmt)) (seq-idx 1) parts)
    (while (< i len)
      (let ((c (aref fmt i)))
        (if (/= c ?%)
            (progn
              (unless start (setq start i))
              (cl-incf i))
          (when start
            (push (substring fmt start i) parts)
            (setq start nil))
          (cl-incf i)
          (when (>= i len)
            (error "Trailing `%%%%' in format string: %S" fmt))
          (if (= (aref fmt i) ?%)
              ;; %% → %%%%
              (progn (push "%%%%" parts) (cl-incf i))
            ;; Real specifier.  Scan optional leading digits.
            (let ((d0 i))
              (while (and (< i len) (<= ?0 (aref fmt i) ?9)) (cl-incf i))
              (let* (;; Positional if digits are followed by $ (and non-empty)
                     (pos-p   (and (> i d0) (< i len) (= (aref fmt i) ?$)))
                     (_       (when pos-p
                                (cl-incf i)))
                     ;; Labelled if next char is =
                     (labeled (and (< i len) (= (aref fmt i) ?=)))
                     (_       (when labeled (cl-incf i))))
                (if labeled
                    (if pos-p
                        (progn
                          (let ((pos (substring fmt d0 (1- i))))
                            (push (concat "%" pos "s=%%" pos) parts)))
                      (push (concat "%" (number-to-string seq-idx) "$s=%%")
                            parts)
                      (cl-incf seq-idx))
                  (push (concat "%%" (substring fmt d0 i)) parts)
                  (unless pos-p
                    (cl-incf seq-idx)))))))))
    (when start
      (push (substring fmt start len) parts))

    (apply #'format
           (apply #'format ;; First pass for labels
                  (apply #'concat (nreverse parts)) ;; Transformed fmt
                  args-labels)
           args)))

(defun dmsg--dispatch (args args-label)
  "Dispatcher called by the `dmsg' macro.
ARGS is the evaluated argument list.  ARGS-LABEL is a parallel list of
argument labels.  First argument could be a symbol which is used as
level. The argument after that is the string format."
  (let* ((level (if (memq (car args) dmsg--level-order)
                    (progn
                      (pop args-label)       ; discard label for level symbol
                      (pop args))
                  'debug))
         (fmt (pop args)))
    (pop args-label)                         ; discard label for fmt string
    (dmsg-write level (dmsg--format fmt args args-label))))

(defmacro dmsg (&rest args)
  "Insert a timestamped, levelled debug entry into `dmsg-buffer-name'.

Syntax:
\(dmsg LEVEL FMT [ARGS…])   — explicit level symbol first
\(dmsg FMT [ARGS…])         — defaults to `debug' level

In FMT, `%=X' (where X is a conversion character) formats the corresponding
argument as \"label=value\", where the label is the unevaluated argument name."
  `(dmsg--dispatch (list ,@args)
                   (list ,@(mapcar (apply-partially #'format "%S") args))))

;;;; dmsg Injection
(defvar dmsg-on-message nil
  "When non-nil, intercept `message' calls and copy matching ones to dmsg.
The value must be a regexp string.  Any call to `message' whose formatted
output matches it is also logged via `dmsg' at debug level.")

(define-advice message
    (:after (fmt &rest args) dmsg-on-message)
  "Advice to reproduce messages on dmsg.
`dmsg' is called whenever (format FMT ARGS) matches `dmsg-on-message'."
  (when (and dmsg-on-message fmt)
    (let ((pat dmsg-on-message)
          ;; Prevent infinite loops
          dmsg-on-message)
      (let ((msg (apply 'format fmt args)))
        (when (and msg (string-match-p pat msg))
          (dmsg-write 'debug msg))))))

(defun dmsg-log-debugger (symb type &optional sig-args)
  "Debugger function that logs errors via `dmsg' before re-signaling.
SYMB is the name of the function being debugged (a string).
TYPE is the debug event symbol (typically `error').
SIG-ARGS is the error condition cons cell (ERROR-SYMBOL . DATA)."
  (dmsg-write 'error (format "%s: %s" symb
                             (error-message-string sig-args)))
  (when (eq type 'error)
    (signal (car sig-args) (cdr sig-args))))

(defun dmsg--function-advice (oldfn &rest args)
  "Logs any error signalled by (apply OLDFN ARGS) via `dmsg'.

Install on a function with:
  (advice-add \\='SYMBOL :around #\\='dmsg--function-advice)
or interactively with `dmsg-on-error'."
  (let ((debug-on-error t)
        (debugger
         (apply-partially #'dmsg-log-debugger (symbol-name oldfn))))
    ;; This is needed for functions in `post-command-hook'
    ;; See https://lists.gnu.org/archive/html/emacs-devel/2010-07/msg01410.html
    (condition-case err
        (apply oldfn args)
      ((debug error)
       (signal
        (car err)
        (cdr err))))))

(defun dmsg-on-error (symbol &optional action)
  "Add, remove, or toggle error-logging advice on SYMBOL.
ACTION controls what happens:
  t        — add the advice unconditionally
  nil      — remove the advice unconditionally
  `toggle' — flip the current state (default when called interactively)

When the advice is active, any error signalled by SYMBOL is logged via
`dmsg' at ERROR level and then re-signalled normally, preserving existing
error handling behaviour.

Returns t if the advice is now active, nil if it was removed."
  (interactive
   (list (intern (completing-read "Callable: " obarray #'fboundp t))
         'toggle))
  (let* ((active (advice-member-p 'dmsg--function-advice symbol))
         (add    (pcase action
                   ('toggle (not active))
                   (`t      t)
                   (_       nil))))
    (if add
        (progn
          (advice-add symbol :around #'dmsg--function-advice)
          (dmsg--imessage "dmsg: error advice added to `%s'" symbol)
          t)
      (advice-remove symbol #'dmsg--function-advice)
      (dmsg--imessage "dmsg: error advice removed from `%s'" symbol)
      nil)))

(provide 'dmsg)
;;; dmsg.el ends here
