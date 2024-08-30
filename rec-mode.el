;;; rec-mode.el --- Major mode for viewing/editing recfiles for GNU Recutils  -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2022  Free Software Foundation, Inc.

;; Author: Jose E. Marchesi <jemarch@gnu.org>
;; Maintainer: Antoine Kalmbach <ane@iki.fi>
;; URL: https://www.gnu.org/software/recutils/
;; Package-Requires: ((emacs "25"))
;; Version: 1.9.4

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Rec Mode is a mode for browsing and editing recfiles, which are text files
;; containing data structured in fields and records.  It is part of the GNU
;; recutils suite, for more information visit http://www.gnu.org/software/recutils.
;;
;; Recfiles are text-based databases which are easy to read and write manually
;; using a text editor.  At the same time they feature enough structure so they can
;; be read, edited and processed automatically by programs.
;;
;; Rec Mode offers convenience commands for navigation, edition, record statistics,
;; syntax highlighting, and more.  Rec mode comes with a manual, see Info node
;; `(rec-mode)Introduction'.

;;; Code:
;; To see the structure of this file activate the outline minor mode
;; and execute M-xhide-body

(require 'compile)
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'cl-seq))
(require 'calendar)
(require 'hl-line)
(require 'tabulated-list)
(eval-when-compile (require 'subr-x))
(require 'seq)
(require 'eieio)
(require 'xref)

;;;; Customization

(defgroup rec-mode nil
  "Rec Mode, a major mode for GNU Recutils recfiles."
  :group 'applications
  :link '(url-link "http://www.gnu.org/software/recutils"))

(defcustom rec-open-mode 'navigation
  "Default mode to use when switching a buffer to `rec-mode'.
Valid values are `edit' and `navigation'.  The default is `navigation'"
  :type 'symbol
  :safe (lambda (x) (member x '(edit navigation))))
;;;###autoload (put 'rec-open-mode 'safe-local-variable (lambda (x) (member x '(edit navigation))))

(defcustom rec-popup-calendar t
  "Whether to use a popup calendar to select dates when editing field values.
The default is t."
  :type 'boolean)

(defcustom rec-move-to-next-line-after-edit t
  "Whether to move forward to the next field or line after editing a record field."
  :type 'boolean)

(defcustom rec-mode-hook nil
  "Hook run when entering rec mode."
  :type 'hook)

(defcustom rec-summary-window-height 10
  "Height of the summary window."
  :type 'integer)

(defcustom rec-summary-deletes-other-windows nil
  "Whether entering `rec-summary-mode' should kill other windows."
  :type 'boolean)

;;;; Faces and variables

(defvar rec-max-lines-in-fields 15
  "Truncate displaying lines exceeding this limit.

Values of fields having more than the specified lines will be
hidden by default in navigation mode.")
(put 'rec-max-lines-in-fields 'safe-local-variable 'numberp)

(defvar rec-recsel "recsel"
  "Name of the `recsel' utility from the GNU recutils.")

(defvar rec-recinf "recinf"
  "Name of the `recinf' utility from the GNU recutils.")

(defvar rec-recfix "recfix"
  "Name of the `recfix' utility from the GNU recutils.")

(defface rec-field-name-face '((t :inherit font-lock-variable-name-face))
  "Face for field names in record entries.")

(defface rec-keyword-face '((t :inherit font-lock-keyword-face))
  "Face for keywords in the record descriptor.")

(defface rec-continuation-line-face '((t :weight bold))
  "Face for line continuations (+).")

;;;; Utilities

(defun rec-mode--syntax-highlight (str)
  "Highlight STR using rec-mode's font-locking."
  (with-temp-buffer
    (insert str)
    (delay-mode-hooks
      (rec-mode)
      (if (fboundp 'font-lock-ensure)
          (font-lock-ensure)
        (with-no-warnings
          (font-lock-fontify-buffer)))
      (buffer-string))))

;;;; Variables and constants that the user does not want to touch (really!)

(defconst rec-mode-version "1.5"
  "Version of rec-mode.el.")

(defconst rec-keyword-prefix "%"
  "Prefix used to distinguish special fields.")

(defconst rec-keyword-rec (concat rec-keyword-prefix "rec")
  ;; Remember to update `rec-font-lock-keywords' if you change this
  ;; value!!
  "Rec keyword.")

(defconst rec-keyword-key (concat rec-keyword-prefix "key")
  "Key keyword.")

(defconst rec-keyword-mandatory (concat rec-keyword-prefix "mandatory")
  "Mandatory keyword.")

(defconst rec-keyword-summary (concat rec-keyword-prefix "summary")
  "Summary keyword.")

(defconst rec-time-stamp-format "%Y-%m-%d %a %H:%M"
  "Format for `format-time-string' which is used for time stamps.")

(defvar rec-comment-re "^#.*"
  "Regexp denoting a comment line.")

(defvar rec-comment-field-re "^\\(#.*\n\\)*\\([a-zA-Z0-1_%-]+:\\)+"
  "Regexp denoting the beginning of a record.")

(defvar rec-field-name-re
  "^[a-zA-Z%][a-zA-Z0-9_]*:"
  "Regexp matching a field name.")

(defvar rec-field-value-re
  "\\(?:\n\\+ ?\\|\\\\\n\\|\\\\.\\|[^\n\\]\\)*"
  "Regexp matching a field value.")

(defvar rec-type-re
  (concat "^" rec-keyword-rec ":\s+" "\\([[:word:]_]+\\)"))

(defvar rec-field-re
  (concat rec-field-name-re
          rec-field-value-re
          "\n")
  "Regexp matching a field.")

(defvar rec-record-re
  (concat rec-field-re "\\(" rec-field-re "\\|" rec-comment-re "\\)*")
  "Regexp matching a record.")

(defvar rec-constants
  '("yes" "no" "true" "false" "MIN" "MAX")
  "Symbols that are constants, like boolean values or MIN/MAX.")

(defvar rec-field-types
  '("line" "real" "int" "regexp" "enum" "date" "field" "uuid" "email" "bool" "size"
    "rec" "range" ))

(defvar rec-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)   ; Comment start
    (modify-syntax-entry ?\n ">" st)  ; Comment end
    (modify-syntax-entry ?\" "w" st)  ;FIXME: really?  Why?
    (modify-syntax-entry ?\' "w" st)  ;FIXME: really?  Why?
    st)
  "Syntax table used in `rec-mode'.")

(defconst rec-syntax-propertize-function
  (syntax-propertize-rules
   ;; In rec, `#' only starts a comment when at BOL.
   (".\\(#\\)" (1 "."))))

(defvar rec-font-lock-keywords
  `((,(concat "^" rec-keyword-prefix "[a-zA-Z0-9_]+:") . 'rec-keyword-face)
    (,rec-type-re . (1 font-lock-function-name-face))
    (,(regexp-opt rec-constants 'words) . 'font-lock-constant-face)
    (,(regexp-opt rec-field-types 'words) . 'font-lock-type-face)
    (,rec-field-name-re . 'rec-field-name-face)
    ("^\\+" . 'rec-continuation-line-face))
  "Font lock keywords used in `rec-mode'.")

(defvar rec-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'rec-cmd-goto-next-rec)
    (define-key map (kbd "C-c C-p") 'rec-cmd-goto-previous-rec)
    (define-key map (kbd "C-c C-e") 'rec-cmd-edit-field)
    (define-key map (kbd "C-c C-d") 'rec-cmd-show-descriptor)
    (define-key map (kbd "C-c C-t") 'rec-cmd-show-type)
    (define-key map (kbd "C-c C-#") 'rec-cmd-count)
    (define-key map (kbd "C-c C-%") 'rec-cmd-statistic)
    (define-key map (kbd "C-c C-f m") 'rec-cmd-trim-field-value)
    (define-key map (kbd "C-c C-k") 'rec-cmd-compile)
    (define-key map (kbd "C-c C-/ s") 'rec-cmd-navigate-current-type-by-sex)
    (define-key map (kbd "C-c C-/ C-k") 'rec-cmd-exit-selection)
    (define-key map (kbd "C-c M-s C-s") 'rec-cmd-new-buffer-from-sex)
    (define-key map (kbd "C-c M-s C-q") 'rec-cmd-navigate-by-sex)
    (define-key map (kbd "C-c C-x C-s") 'rec-cmd-xref-sex)
    (define-key map (kbd "C-c C-x C-q") 'rec-cmd-xref-fast-string)
    (define-key map (kbd "C-c C-s q") 'rec-cmd-jump-to-fast)
    (define-key map (kbd "C-c C-s s") 'rec-cmd-jump-to-sex)
    (define-key map (kbd "C-c M-h") 'rec-cmd-show-summary)
    (define-key map (kbd "C-c C-i") 'rec-cmd-show-info)
    (define-key map (kbd "C-c C-f C-w") 'rec-cmd-kill-field)
    (define-key map (kbd "C-c C-f M-w") 'rec-cmd-copy-field)
    (define-key map (kbd "C-c C-r C-w") 'rec-cmd-kill-record)
    (define-key map (kbd "C-c C-r M-w") 'rec-cmd-copy-record)
    (define-key map (kbd "C-c C-SPC") 'rec-cmd-toggle-field-visibility)
    (define-key map [tab] 'rec-cmd-goto-next-field)
    (define-key map (kbd "TAB") 'rec-cmd-goto-next-field)
    (define-key map (kbd "C-c C-b") 'rec-cmd-jump-back)
    (define-key map (kbd "C-c C-c") 'rec-finish-editing)
    (define-key map (kbd "C-c C-t") 'rec-find-type)
    ;; Set a dummy keymap as the parent so we shadow
    ;; `rec-mode-map' instead of inheriting it.
    (set-keymap-parent map (make-sparse-keymap))
    map)
  "Keymap for `rec-edit-mode'.")

(defvar rec-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'rec-cmd-goto-next-rec)
    (define-key map (kbd "p") 'rec-cmd-goto-previous-rec)
    (define-key map (kbd "e") 'rec-cmd-edit-field)
    (define-key map (kbd "R") 'rec-edit-record)
    (define-key map (kbd "T") 'rec-edit-type)
    (define-key map (kbd "B") 'rec-edit-buffer)
    (define-key map (kbd "A") 'rec-cmd-append-field)
    (define-key map (kbd "I") 'rec-cmd-show-info)
    (define-key map (kbd "d") 'rec-cmd-show-descriptor)
    (define-key map (kbd "t") 'rec-cmd-show-type)
    (define-key map (kbd "m") 'rec-cmd-trim-field-value)
    (define-key map (kbd "c") 'rec-cmd-compile)
    (define-key map (kbd "/ s") 'rec-cmd-navigate-current-type-by-sex)
    (define-key map (kbd "/ q") 'rec-cmd-navigate-current-type-by-fast-string)
    (define-key map (kbd "/ /") 'rec-cmd-exit-selection)
    (define-key map (kbd "X s") 'rec-cmd-xref-sex)
    (define-key map (kbd "X q") 'rec-cmd-xref-fast-string)
    (define-key map (kbd "f C-w") 'rec-cmd-kill-field)
    (define-key map (kbd "f M-w") 'rec-cmd-copy-field)
    (define-key map (kbd "r C-w") 'rec-cmd-kill-record)
    (define-key map (kbd "r M-w") 'rec-cmd-copy-record)
    (define-key map (kbd "s q") 'rec-cmd-jump-to-fast)
    (define-key map (kbd "s s") 'rec-cmd-jump-to-sex)
    (define-key map (kbd "S s") 'rec-cmd-new-buffer-from-sex)
    (define-key map (kbd "S q") 'rec-cmd-new-buffer-from-fast-string)
    (define-key map (kbd "h") 'rec-cmd-show-summary)
    (define-key map (kbd "C-c C-t") 'rec-find-type)
    (define-key map [remap undo] 'rec-cmd-undo)
    (define-key map (kbd "#") 'rec-cmd-count)
    (define-key map (kbd "%") 'rec-cmd-statistic)
    (define-key map [tab] 'rec-cmd-goto-next-field)
    (define-key map (kbd "TAB") 'rec-cmd-goto-next-field)
    (define-key map (kbd "SPC") 'rec-cmd-toggle-field-visibility)
    (define-key map (kbd "b") 'rec-cmd-jump-back)
    (define-key map [remap xref-go-back] 'rec-cmd-xref-go-back)
    (define-key map [remap xref-go-forward] 'rec-cmd-xref-go-forward)
    map)
  "Keymap for `rec-mode'.")

;;;; Parsing functions (rec-parse-*)
;;
;; Those functions read the contents of the buffer (starting at the
;; current position of the pointer) and try to parse field, comment
;; and records structures.

(defun rec-parse-comment ()
  "Parse and return a comment starting at point.

Return a list whose first element is the symbol \\='comment and the
second element is the string with the contents of the comment,
including the leading #:

   (comment POSITION \"# foo\")

If the point is not at the beginning of a comment then return nil"
  (when (and (equal (current-column) 0)
             (looking-at rec-comment-re))
    (let ((comment (make-rec-comment :position (point)
                                     :value (buffer-substring-no-properties (match-beginning 0)
                                                                            (match-end 0)))))
      (goto-char (match-end 0))
      ;; Skip a newline if needed
      (when (eolp) (forward-line 1))
      comment)))

(defun rec-parse-field-name ()
  "Parse and return a field name starting at point.
If the point is not at the beginning of a field name return nil"
  (when (and (equal (current-column) 0)
             (looking-at rec-field-name-re))
    (goto-char (match-end 0))
    (buffer-substring-no-properties (match-beginning 0)
                                    (- (match-end 0) 1))))

(defun rec-parse-field-value ()
  "Return the field value under the pointer.

Return a string containing the value of the field.

If the pointer is not at the beginning of a field value, return
nil"
  (when (looking-at rec-field-value-re)
    (goto-char (match-end 0))
    (let ((val (buffer-substring-no-properties (match-beginning 0)
                                               (match-end 0))))
      ;; Replace escaped newlines
      (setq val (replace-regexp-in-string "\\\\\n" "" val))
      ;; Replace continuation lines
      (setq val (replace-regexp-in-string "\n\\+ ?" "\n" val))
      ;; Remove the initial blank
      (with-temp-buffer
        (insert val)
        (goto-char (point-min))
        (if (equal (char-after (point)) ?\s)
            (progn
              (delete-char 1)))
        (setq val (buffer-substring-no-properties (point-min)
                                                  (point-max))))
      val)))

(defun rec-parse-field ()
  "Return a field struct describing the field starting from the pointer.

If the pointer is not at the beginning of a field descriptor then
return nil."
  (let ((there (point))
        field-name field-value)
    (when (and (setq field-name (rec-parse-field-name))
               (setq field-value (rec-parse-field-value)))
      ;; Skip a newline if needed
      (when (looking-at "\n") (goto-char (match-end 0)))
      (make-rec-field :position there
                      :name field-name
                      :value field-value))))

(defun rec-parse-record ()
  "Return a structure describing the record starting from the pointer.

Returns either an object `rec-record' or `rec-record-descriptor' depending
whether the current record is a plain record or a record
descriptor.

If the pointer is not at the beginning of a record, then return
nil."
  (let ((there (point))
        (fields ()) field-or-comment)
    (while (setq field-or-comment (or (rec-parse-field)
                                      (rec-parse-comment)))
      (push field-or-comment fields))

    (let ((record (rec-make-record there (reverse fields))))
      (or (rec-narrow-record record) record))))


;;;; Operations on record structures
;;
;; Those functions retrieve or set properties of field structures.

(cl-defstruct (rec-record
               (:constructor rec-make-record (position fields)))
  "A record."
  position fields)

(cl-defstruct (rec-record-descriptor (:include rec-record))
  "A record descriptor."
  type types key auto doc)

(cl-defstruct rec-record-element
  "A record element, either a comment or a field."
  position value)

(cl-defstruct (rec-comment (:include rec-record-element))
  "A record comment.")

(cl-defstruct (rec-field (:include rec-record-element))
  name)

(defun rec-map-fields (fun record)
  "Map function FUN over the fields in RECORD."
  (cl-loop for field in (rec-record-fields record)
           when (rec-field-p field)
           collect (funcall fun field)))

(cl-defmethod rec-record-assoc (name (record rec-record))
  "Get a list with the values of the fields in RECORD named NAME.

NAME shall be a field name.  If no such field exists in RECORD
then nil is returned."
  (cl-loop for field in (rec-record-fields record)
           when (and (rec-field-p field)
                     (equal name (rec-field-name field)))
           collect (rec-field-value field)))

(cl-defgeneric rec-record-names (record)
  "Get a list of the field names in the RECORD.")

(cl-defmethod rec-record-names ((record rec-record))
  "Get a list of the field names in the RECORD."
  (cl-loop for field in (rec-record-fields record)
           when (rec-field-p field)
           collect (rec-field-name field)))

(defun rec-record-values (record fields)
  "Given a list of field names in FIELDS, return a list of the values of RECORD."
  (when fields
    (append (rec-record-assoc (car fields) record)
            (rec-record-values record (cdr fields)))))

;;;; Writer functions (rec-insert)
;;
;; Those functions dump the written representation of the parser
;; structures (field, comment, record, etc) into the current buffer
;; starting at the current position.

(cl-defgeneric rec-insert (element)
  "Insert the written form of ELEMENT into the current buffer.")

(cl-defmethod rec-insert ((comment rec-comment))
  "Insert the written form of COMMENT in the current buffer."
  (insert (rec-record-element-value comment) "\n"))

(defun rec-insert-field-name (field-name)
  "Insert the written form of FIELD-NAME in the current buffer."
  (when (stringp field-name)
    (insert (concat field-name ":"))
    t))

(defun rec-insert-field-value (field-value)
  "Insert the written form of FIELD-VALUE in the current buffer."
  (when (stringp field-value)
    (let ((val field-value))
      ;; FIXME: Maximum line size
      (insert (replace-regexp-in-string "\n" "\n+ " val)))
    (insert "\n")))

(cl-defmethod rec-insert ((field rec-field))
  "Insert the written form of FIELD in the current buffer."
  (when (rec-insert-field-name (rec-field-name field))
    (insert " ")
    (rec-insert-field-value (rec-field-value field))))

(cl-defmethod rec-insert ((record rec-record))
  "Insert the written form of RECORD in the current buffer."
  (mapc #'rec-insert (rec-record-fields record)))

;;;; Operations on field structures
;;
;; Those functions retrieve or set properties of field structures.

(defun rec-field-trim-value (field)
  "Return FIELD with its value trimmed."
  (when (rec-field-p field)
    (let ((value (rec-field-value field))
          c)
      (with-temp-buffer
        (insert value)
        (goto-char (point-min))
        (when (looking-at "[ \t\n][ \t\n]+")
          (delete-region (match-beginning 0)
                         (match-end 0)))
        (goto-char (point-max))
        (setq c (char-before))
        (while (and c (member c '(?\n ?\t ?\s)))
          (backward-char)
          (setq c (char-before)))
        (delete-region (point) (point-max))
        (setq value (buffer-substring-no-properties (point-min)
                                                    (point-max))))
      (setf (rec-field-value field) value)
      field)))

;;;; Get entities under pointer
;;
;; Those functions retrieve structures of the entities under pointer
;; like comments, fields and records.  If the especified entity is not
;; under the pointer then nil is returned.

(defun rec-beginning-of-field-pos ()
  "Return the position of the beginning of the current field.

Return nil if the pointer is not on a field."
  (save-excursion
    (beginning-of-line)
    (while (and (not (= (line-beginning-position) (point-min)))
                (or (looking-at "\\+")
                    (and (not (bobp))
                         (save-excursion
                           (backward-char 2)
                           (looking-at "\\\\\n")))))
      (forward-line -1))
    (if (looking-at rec-field-name-re)
        (point)
      nil)))

(defun rec-end-of-field-pos ()
  "Return the position of the end of the current field.

Return nil if the pointer is not on a field."
  (let ((begin-pos (rec-beginning-of-field-pos)))
    (when begin-pos
      (save-excursion
        (goto-char begin-pos)
        ;; The following hack is due to the fact that
        ;; the regular expressions search engine is
        ;; hanging on rec-field-re
        (when (looking-at rec-field-name-re)
          (goto-char (match-end 0)))
        (when (looking-at rec-field-value-re)
          (match-end 0))))))

(defun rec-beginning-of-comment-pos ()
  "Return the position of the beginning of the current comment,or nil if the pointer is not on a comment."
  (save-excursion
    (beginning-of-line)
    (when (looking-at rec-comment-re)
      (point))))

(defun rec-end-of-comment-pos ()
  "Return the position of the end of the current comment, or nil if the pointer is not on a comment."
  (let ((begin-pos (rec-beginning-of-comment-pos)))
    (when begin-pos
      (save-excursion
        (goto-char begin-pos)
        (when (looking-at rec-comment-re)
          (match-end 0))))))

(defun rec-beginning-of-record-pos ()
  "Return the position of the beginning of the current record, or nil if the pointer is not on a record."
  (let (field-pos)
    (save-excursion
      (cl-block found
        (while (not (equal (point) (point-min)))
          (save-excursion
            (backward-char)
            (setq field-pos (or (rec-beginning-of-field-pos)
                                (rec-beginning-of-comment-pos))))
          (if field-pos
              (goto-char field-pos)
            (if (or (rec-beginning-of-field-pos)
                    (rec-beginning-of-comment-pos))
                (cl-return-from found)
              (backward-char)))))
      (when (looking-at rec-comment-field-re)
        (point)))))

(defun rec-end-of-record-pos ()
  "Return the position of the end of the current record, or nil if the pointer is not on a record."
  (let ((begin-pos (rec-beginning-of-record-pos)))
    (when begin-pos
      (save-excursion
        (goto-char begin-pos)
        (while (or (looking-at rec-field-name-re)
                   (looking-at rec-comment-re))
          (goto-char (match-end 0))
          (when (or (looking-at rec-field-value-re)
                    (looking-at rec-comment-re))
            (goto-char (+ (match-end 0) 1))))
        (point)))))

(defun rec-current-field ()
  "Return a structure with the contents of the current field.
The current field is the field where the pointer is."
  (save-excursion
    (let ((begin-pos (rec-beginning-of-field-pos)))
      (when begin-pos
        (goto-char begin-pos)
        (rec-parse-field)))))

(defun rec-current-field-type ()
  "Return the field type of the field under point, if any."
  (let ((current-field (rec-current-field)))
    (when current-field
      (rec-field-type (rec-field-name current-field)))))

(defun rec-current-record ()
  "Return a structure with the contents of the current record.
The current record is the record where the pointer is"
  (save-excursion
    (let ((begin-pos (rec-beginning-of-record-pos)))
      (when begin-pos
        (goto-char begin-pos)
        (rec-parse-record)))))

;;;; Visibility
;;
;; These functions manage the visibility in the rec buffer.

(defun rec-narrow-to-record ()
  "Narrow to the current record, if any."
  (let ((begin-pos (rec-beginning-of-record-pos))
        (end-pos (rec-end-of-record-pos)))
    (if (and begin-pos end-pos)
        (narrow-to-region begin-pos end-pos))))

(defun rec-narrow-to-type (type)
  "Narrow to the specified TYPE, if any."
  (let ((begin-pos (or (rec-type-pos type) (point-min)))
        (end-pos (or (rec-type-pos (rec-type-next type)) (point-max))))
    (narrow-to-region begin-pos end-pos)))

;;;; Record collection management
;;
;; These functions perform the management of the collection of records
;; in the buffer.


(defvar rec-buffer-descriptors nil
  "List of `rec--descriptor's.")
(make-variable-buffer-local 'rec-buffer-descriptors)

(defun rec-buffer-valid-p ()
  "Determine if the current buffer has valid rec data."
  (equal (call-process-region (point-min) (point-max)
                              rec-recinf
                              nil ; delete
                              nil ; discard output
                              nil ; display
                              )
         0))

(defun rec-update-buffer-descriptors-and-check (&optional dont-go-fundamental)
  "Update buffer descriptors and check if there's a parse error.

Switch to fundamental mode if there is a parse error.  If
DONT-GO-FUNDAMENTAL is non-nil, don't switch to fundamental."
  (if (rec-buffer-valid-p)
      (progn
        (rec-update-buffer-descriptors)
        t)
    (unless dont-go-fundamental
      (fundamental-mode))
    (let* ((cur-buf (current-buffer))
           (errmsg (with-temp-buffer
                     (let ((err-buf (current-buffer)))
                       (with-current-buffer cur-buf
                         (call-process-region (point-min) (point-max)
                                              rec-recfix
                                              nil ; delete
                                              (list err-buf t)
                                              nil ; display
                                              "--check")))
                     (goto-char (point-min))
                     (when (looking-at "stdin: ")
                       (delete-region (point-min) (match-end 0)))
                     (goto-char (point-max))
                     (when (equal (char-after) ?\n)
                       (delete-char 1))
                     (buffer-substring (point-min) (- (point-max) 1)))))
      (message (concat (buffer-name) ": " errmsg))
      nil)))


(defun rec-narrow-record (record)
  "Try making a record descriptor out of RECORD.

If the record is a descriptor, it will be an instance of
`rec-record-descriptor', otherwise nil.  This judgment is based
on the existence of the existence of the \"%rec\" field.  If a record
has this field, it is a descriptor."
  (when-let ((type (car-safe (rec-record-assoc "%rec" record))))
    (make-rec-record-descriptor :position (rec-record-position record)
                                :fields (rec-record-fields record)
                                :type type
                                :types (rec-record-assoc "%type" record)
                                :key (car-safe (rec-record-assoc "%key" record))
                                :auto (car-safe (rec-record-assoc "%auto" record))
                                :doc (car-safe (rec-record-assoc "%doc" record)))))

(defun rec--parse-sexp-records (records)
  "Parse a recinf sexp record in RECORDS."
  (cl-loop for (nil pos fields) in records
           for parsed-fields = (cl-loop for (nil pos name value) in fields
                                        collect (make-rec-field :position pos
                                                                :name name
                                                                :value value))
           for record = (rec-make-record pos parsed-fields)
           collect (or (rec-narrow-record record) record)))

(defun rec-update-buffer-descriptors ()
  "Get a list of the record descriptors in the current buffer.

If the contents of the current buffer are not valid rec data then
this function returns nil."
  (setq rec-buffer-descriptors
	(let* ((buffer (generate-new-buffer "Rec Inf "))
	       (status
	        ;; Call 'recinf' to get the list of record descriptors in
	        ;; sexp format.
	        (call-process-region (point-min) (point-max)
                                     rec-recinf
                                     nil ; delete
                                     buffer
                                     nil ; display
                                     "-S" "-d")))
          (if (equal status 0)
              (progn (with-current-buffer buffer
                       (goto-char (point-min))
                       (insert "(")
                       (goto-char (point-max))
                       (insert ")")
                       (unwind-protect
                           (let* ((result (read (point-min-marker)))
                                  (first (car-safe result)))
                             (if (and first (seq-every-p #'listp result))
                                 (seq-filter #'rec-record-descriptor-p
                                             (rec--parse-sexp-records result))))
                         (kill-buffer buffer))))
            (kill-buffer buffer)
            nil))))

(defun rec-buffer-types ()
  "Return a list with the names of the record types in the existing buffer."
  ;; If a descriptor has more than a %rec field, then the first one is
  ;; used.  The rest are ignored.
  (mapcar (lambda (descriptor)
            (rec-record-descriptor-type descriptor))
          rec-buffer-descriptors))

(defun rec-type-p (type)
  "Determine if there are records of type TYPE in the current file."
  (member type (rec-buffer-types)))

(defun rec-goto-type (type)
  "Goto the beginning of the descriptor with type TYPE.

If there are records of type TYPE in the record set then goto the
first record.  Otherwise goto to the record descriptor.

If the type do not exist in the current buffer then
this function returns nil."
  (if (or (not type)
          (equal type ""))
      ;; If there is a regular record in the
      ;; beginning of the file, go there.
      (if (save-excursion
            (goto-char (point-min))
            (unless (looking-at rec-comment-field-re)
              (rec-goto-next-rec))
            (rec-regular-p))
          (progn
            (goto-char (point-min))
            (unless (looking-at rec-comment-field-re)
              (rec-goto-next-rec))
            t)
        nil)
    (let (found
          (descriptors rec-buffer-descriptors))
      (mapc
       (lambda (elem)
         (when (equal type (rec-record-descriptor-type elem))
           (setq found t)
           (goto-char (rec-record-descriptor-position elem))))
       descriptors)
      found)))

(defun rec-type-pos (type)
  "Return the position where the records of type TYPE start in the current file.

If no records of type TYPE are defined in the
current file then return nil."
  (when (rec-type-p type)
    (save-excursion
      (rec-goto-type type)
      (point))))

(defun rec-type-next (type)
  "Return the name of the type following TYPE in the file, if any.

If the specified type is the last appearing in the file,
or the specified type does not exist, then return nil."
  (let ((types (member type (rec-buffer-types))))
    (nth 1 types)))

(defun rec-type-previous (type)
  "Return the name of the type preceding TYPE in the file, if any.

If the specified type is the first appearing in the file,
or the specified type does not exist, then return nil."
  (let ((types (member type (reverse (rec-buffer-types)))))
    (nth 1 types)))

(defun rec-goto-next-field ()
  "Move the pointer to the beginning of the next field in the file."
  (let ((pos (save-excursion
               (rec-end-of-field)
               (when (re-search-forward rec-field-name-re nil t)
                 (match-beginning 0)))))
    (when pos
      (goto-char pos)
      t)))

(defun rec-goto-next-rec ()
  "Move the pointer to the beginning of the next record in the file."
  (let ((pos (save-excursion
               (rec-end-of-record)
               (when (re-search-forward rec-comment-field-re nil t)
                 (match-beginning 0)))))
    (when pos
      (push-mark)
      (goto-char pos)
      t)))

(defun rec-goto-previous-rec ()
  "Move the pointer to the end of the previous record in the file."
  (let ((pos (save-excursion
               (rec-beginning-of-record)
               (if (not (= (point) (point-min)))
                   (backward-char))
               (when (and (re-search-backward rec-record-re nil t)
                          (rec-beginning-of-record))
                 (point)))))
    (when pos
      (push-mark)
      (goto-char pos)
      t)))

(defun rec-type-first-rec-pos (type)
  "Return the position of the first record of the specified TYPE.

If TYPE is nil then return the position of the first regular record in the file.
If there are no regular records in the file, return nil."
  (save-excursion
    (when (or (not type) (rec-type-p type))
      (if type
          (rec-goto-type type)
        (goto-char (point-min)))
      ;; Find the next regular record
      (when (and (rec-goto-next-rec)
                 (rec-regular-p))
        (point)))))



(defun rec-goto-type-first-rec (type)
  "Goto to the first record of type TYPE present in the file.
If TYPE is nil then goto to the first Unknown record on the file.

If the record is found, return its position.
If no such record exist then don't move and return nil."
  (let ((pos (rec-type-first-rec-pos type)))
    (when pos
      (goto-char pos))))


(defun rec-count (&optional type sex)
  "Return number of record in the file.

When optional argument TYPE is a string, return records of that
type.

Optional rgument SEX specifies the selection expression to apply
on top of the results."
  (let (num
        (rec-file-name (if buffer-file-name
                           buffer-file-name
                         "")))
    (with-temp-buffer
      (if (stringp type)
          (if (stringp sex)
              (call-process rec-recsel
                            nil ; infile
                            t   ; output to current buffer.
                            nil ; display
                            "-t" type "-e" sex "-c" rec-file-name)
            (call-process rec-recsel
                          nil ; infile
                          t   ; output to current buffer.
                          nil ; display
                          "-t" type "-c" rec-file-name))
        (if (stringp sex)
            (call-process rec-recsel
                          nil ; infile
                          t   ; output to current buffer.
                          nil ; display
                          "-e" sex "-c" rec-file-name)
          (call-process rec-recsel
                        nil ; infile
                        t   ; output to current buffer.
                        nil ; display
                        "-c" rec-file-name)))
      (setq num (buffer-substring-no-properties (point-min) (point-max))))
    (string-to-number num)))

(defun rec-regular-p ()
  "Return t if the record under point is a regular record.
Return nil otherwise."
  (let ((rec (rec-current-record)))
    (when rec
      (= (length (rec-record-assoc rec-keyword-rec rec))
         0))))

(defun rec-record-type ()
  "Return the type of the record under point.

If the record is of no known type, return nil."
  (when-let ((descriptor (rec-current-record-descriptor)))
    (rec-record-descriptor-type descriptor)))

(defun rec-current-record-descriptor ()
  "Return the record descriptor of the record under point.

Return \"\" if no proper record descriptor is found in the file.
Return nil if the point is not on a record."
  (when (rec-current-record)
    (cl-loop with descriptors = rec-buffer-descriptors
             with next-descriptors = (append (cdr descriptors) '(nil))
             with count = (length descriptors)
             with point = (point)

             for index from 0
             for curr in descriptors and
             next in next-descriptors

             if (and (>= point (rec-record-descriptor-position curr))
                     (or (= index (- count 1))
                         (< point (rec-record-descriptor-position next))))

             return curr)))

(defun rec-summary-fields ()
  "Return a list with the names of the summary fields in the current record set."
  (let ((descriptor (rec-current-record-descriptor)))
    (when descriptor
      (let ((fields-str (rec-record-assoc rec-keyword-summary descriptor)))
        (when fields-str
          (split-string (car fields-str) "[ ,]"))))))

(defun rec-mandatory-fields ()
  "Return a list with the names of the mandatory fields in the current record set."
  (let ((descriptor (rec-current-record-descriptor)))
    (when descriptor
      (let ((fields-str (rec-record-assoc rec-keyword-mandatory descriptor)))
        (when fields-str
          (split-string (car fields-str)))))))

(defun rec-key ()
  "Return the name of the field declared as the key of the current record set.

Returns nil if no key is declared."
  (when-let ((descr (rec-current-record-descriptor)))
    (rec-record-descriptor-key descr)))

;;;; Navigation

(defun rec-show-type (type &optional show-descriptor)
  "Show the records of the given TYPE.

If TYPE is nil then the records of the default type are shown.

Optional argument SHOW-DESCRIPTOR when non-nil shows the
descriptor record.  If nil, the descriptor is skipped."
  (widen)
  (unless (rec-goto-type type)
    (message "No records of the requested type were found."))
  ;; Show the first data record of this type, if it exists.
  (if (and (not show-descriptor)
           (save-excursion
             (let ((record-type (rec-record-type)))
               (and (rec-goto-next-rec)
                    record-type
                    (equal (rec-record-type) record-type)))))
      (rec-goto-next-rec))
  (rec-show-record))

(defun rec-show-record ()
  "Show the record under the point."
  (setq buffer-read-only t)
  (rec-narrow-to-record)
  (rec-set-head-line nil)
  (rec-update-mode-line)
  ;; Hide the contents of big fields.
  (rec-hide-record-fields)
  ;; Change the appearance of continuation line markers to look like
  ;; indentation.
  (rec-hide-continuation-line-markers))

(defvar rec-continuation-line-markers-width 4
  "Number of characters used to represent continuation line markers.")

(defvar rec-continuation-line-markers-overlays nil
  "Continuation line markers overlays.")

(defun rec-hide-continuation-line-markers ()
  "Make continuation line markers look like indentation."
  (let ((record (rec-current-record)))
    (when record
      (mapc
       (lambda (field)
         (when (rec-field-p field)
           (let* ((pos (rec-field-position field))
                  (value-begin (+ pos (length (rec-field-name field)) 1))
                  (value-end (+ value-begin
                                (length (with-temp-buffer
                                          (rec-insert-field-value (rec-field-value field))
                                          (buffer-substring (point-min) (point-max)))))))
             (save-excursion
               (goto-char value-begin)
               (while (re-search-forward "^\\+ ?" (+ value-end 1) t)
                 (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                   (overlay-put ov 'display '(space . (:width rec-continuation-line-markers-width)))
                   (push ov rec-continuation-line-markers-overlays)))))))
       (rec-record-fields record)))))

(defun rec-remove-continuation-line-marker-overlays ()
  "Delete all the continuation line markers overlays."
  (mapc #'delete-overlay rec-continuation-line-markers-overlays)
  (setq rec-continuation-line-markers-overlays nil))

;;;; Field folding

(defvar rec-hide-field-overlays nil
  "Overlays hiding fields.")

(defun rec-hide-record-fields ()
  "Hide the contents of fields that are too long.

If the value value exceeds `rec-max-lines-in-fields' lines, TAB
can then be used to toggle the visibility."
  (let ((record (rec-current-record)))
    (when (rec-record-p record)
      (mapcar
       (lambda (field)
         (when (rec-field-p field)
           (let ((lines-in-value (with-temp-buffer
                                   (insert (rec-field-value field))
                                   (count-lines (point-min) (point-max)))))
             (when (> lines-in-value rec-max-lines-in-fields)
               (save-excursion
                 (goto-char (rec-field-position field))
                 (rec-fold-field))
               t))))
       (rec-record-fields record)))))

(defun rec-field-folded-p ()
  "Return whether the current field is folded."
  (let ((field (rec-current-field)))
    (when (rec-field-p field)
      (let ((value-start (+ (point) (length (rec-field-name field)) 1)))
        (memq t (mapcar (lambda (overlay)
                          (eq (overlay-get overlay 'invisible)
                              'rec-hide-field))
                        (overlays-at value-start)))))))

(defun rec-fold-field ()
  "Fold the current field."
  (let ((field (rec-current-field)))
    (when (rec-field-p field)
      (save-excursion
        (goto-char (rec-field-position field))
        (when (looking-at rec-field-re)
          (let ((value-start (+ (point) (length (rec-field-name field)) 1))
                (value-end (- (match-end 0) 1))
                ov)
            (setq ov (make-overlay value-start value-end))
            (overlay-put ov 'invisible 'rec-hide-field)
            (push ov rec-hide-field-overlays)))))))

(defun rec-unfold-field ()
  "Unfold the current field."
  (let ((field (rec-current-field)))
    (when (rec-field-p field)
      (let ((value-start (+ (point) (length (rec-field-name field)) 1)))
        (mapcar (lambda (overlay)
                  (when (eq (overlay-get overlay 'invisible) 'rec-hide-field)
                    (delete-overlay overlay)))
                (overlays-at value-start))))))

(defun rec-unfold-all-fields ()
  "Unfold all folded fields in the buffer."
  (mapc #'delete-overlay rec-hide-field-overlays)
  (setq rec-hide-field-overlays nil))

(defun rec-unfold-record-fields ()
  "Unfold any folded field in the current record."
  (let ((record (rec-current-record)))
    (mapcar
     (lambda (field)
       (when (rec-field-p field)
         (save-excursion
           (goto-char (rec-field-position field))
           (rec-unfold-field))))
     (rec-record-fields record))))

(defun rec-toggle-field-visibility ()
  "Toggle the visibility of the current field."
  (let ((field (rec-current-field)))
    (when (rec-field-p field)
      (save-excursion
        (goto-char (rec-field-position field))
        (when (looking-at rec-field-re)
          (let* ((value-start (+ (point) (length (rec-field-name field)) 1))
                 (value-end (- (match-end 0) 1))
                 ov)
            (if (memq t (mapcar (lambda (overlay)
                                  (eq (overlay-get overlay 'invisible)
                                      'rec-hide-field))
                                (overlays-at value-start)))
                (mapcar #'delete-overlay
                        (overlays-at value-start))
              (setq ov (make-overlay value-start value-end))
              (overlay-put ov 'invisible 'rec-hide-field))))))))

;;;; Field types
;;
;; This section contains functions and variable implementing field
;; types as described in the recutils manual.
;;
;; Each type is stored in a structure like:
;;
;;    (type KIND EXPR DATA)
;;
;; Where EXPR is the type descriptor used to create the type, and NAME
;; is the name of the type.
;;
;; KIND is the class of the type, and is one of:
;;
;;    int, bool, range, real, size, line, regexp, date,
;;    enum, field, email, uuid, rec
;;
;; DESCR is the data describing the type, and its value depends on the
;; kind:
;;
;;    - For sized strings, it is the maximum size of the string.
;;
;;    - For ranges, it is a list (MIN MAX) defining the range
;;      [MIN,MAX].  Open ranges can be specified by using nil.  For
;;      example: (0,nil).
;;
;;    - For regexps, it is a string containing the regexp.
;;
;;    - For record types, it is a string containing the type of
;;      the referred records.
;;
;;    - For any other type, it is nil.

(cl-defstruct (rec-type
               (:predicate nil)         ;Don't override the `rec-type-p' above!
               (:constructor nil)
               (:constructor rec-type--create (kind text data))
               (:type list))
  (-head 'type)
  kind text data)

(defconst rec-types
  '("int" "bool" "range" "real" "size" "line" "regexp" "date" "enum" "field" "email" "uuid" "rec")
  "Built-in types of recutils.")

(defun rec-type-kind-p (kind)
  "Determine whether the given symbol or string is a type KIND."
  (cond
   ((symbolp kind)
    (member (symbol-name kind) rec-types))
   ((stringp kind)
    (member kind rec-types))
   (t
    nil)))

(defun rec-parse-type (str)
  "Parse STR into a new type structure and return it.

STR must contain a type description as defined in the recutils
manual."
  ;; FIXME: Does not support type aliases.  Most likely this function will be
  ;; built into recinf so that we do not do parsing in Elisp land.
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (when (looking-at "[a-z]+")
      (let ((kind (match-string 0)))
        (goto-char (match-end 0))
        (when (rec-type-kind-p kind)
          (cond
           ((member kind '("int" "bool" "real" "line" "date" "field" "email" "uuid"))
            (when (looking-at "[ \n\t]*$")
              (rec-type--create (intern kind) str nil)))
           ((equal kind "size")
            (when (looking-at "[ \n\t]*\\([0-9]+\\)[ \n\t]*$")
              (rec-type--create (intern kind) str (string-to-number (match-string 1)))))
           ((equal kind "range")
            (when (or
                   (looking-at "[ \n\t]*\\(-?[0-9]+\\)[ \n\t]*$")
                   (looking-at "[ \n\t]*\\(-?[0-9]+\\)[ \n\t]+\\([0-9]+\\)[ \n\t]*$"))
              (let ((min (string-to-number (match-string 1)))
                    (max (when (stringp (match-string 2))
                           (string-to-number (match-string 2)))))
                (rec-type--create (intern kind) str (list min max)))))
           ((equal kind "enum")
            (let ((str-copy str)
                  (here (point)))
              ;; Remove comments from the enum description.
              (delete-region (point-min) (point-max))
              (insert (replace-regexp-in-string "([^)]*)" "" str-copy))
              (goto-char here)
              (when (looking-at "\\([ \n\t]*[a-zA-Z0-9][a-zA-Z0-9_-]*\\)+[ \n\t]*$")
                (let (names)
                  ;; Scan the enum literals.
                  (while (looking-at "[ \n\t]+\\([a-zA-Z0-9][a-zA-Z0-9_-]*\\)")
                    (setq names (cons (match-string 1) names))
                    (goto-char (match-end 0)))
                  (rec-type--create (intern kind) str-copy (reverse names))))))
           ((equal kind "rec")
            (when (looking-at "[ \n\t]*\\([a-zA-Z%][a-zA-Z0-9_]*\\)[ \n\t]*$") ; Field name without a colon.
              (let ((referred-record (match-string 1)))
                (rec-type--create (intern kind) str referred-record))))
           ((equal kind "regexp")
            (when (looking-at "[ \n\t]*\\(.*?\\)[ \n\t]*$")
              (let ((expr (match-string 1)))
                (when (and (>= (length expr) 2)
                           (equal (elt expr 0) (elt expr (- (length expr) 1))))
                  (rec-type--create (intern kind) str (substring expr 1 -1))))))
           (t
            nil)))))))

(defun rec-field-type (field-name)

  "Return the type of FIELD-NAME in determined in the current record set.

If the field has no type, i.e. it is an unrestricted field which
can contain any text, then nil is returned."
  (let (res-type)
    (when-let ((descriptor (rec-current-record-descriptor))
               (types      (rec-record-assoc "%type" descriptor)))
      ;; Note that invalid %type entries are simply ignored.
      (mapc
       (lambda (type-descr)
         (with-temp-buffer
           (insert type-descr)
           (goto-char (point-min))
           (when (looking-at "[ \n\t]*\\([a-zA-Z%][a-zA-Z0-9_-]*\\(,[a-zA-Z%][a-zA-Z0-9_-]*\\)?\\)[ \n\t]*")
             (let (;; (names (match-string 1))
                   (begin-description (match-end 0)))
               (goto-char (match-beginning 1))
               (while (looking-at "\\([a-zA-Z%][a-zA-Z0-9_]*\\),?")
                 (if (equal (match-string 1) field-name)
                     (progn
                       (goto-char begin-description)
                       (setq res-type (rec-parse-type (buffer-substring (point) (point-max)))))
                   (goto-char (match-end 0))))))))
       types))
    res-type))

;;;; Mode line and Head line

(defun rec-update-mode-line ()
  "Update the modeline in rec buffers.

Argument STR is the string to be displayed.

If `rec-selection-mode' is active, that string is displayed instead."
  (let* ((message (cond ((derived-mode-p 'rec-edit-mode)
                         (if rec-selection-mode
                             (format "Edit %s / %s" rec-edit-mode-type selection-format)
                           (format "Edit %s" (symbol-name rec-edit-mode-type))))

                        (rec-selection-mode
                         (rec-selection-stringify rec-selection-current-selection))

                        (t (rec-record-type)))))
    (when message
      (setq mode-line-buffer-identification
            (list 20 "%b "
                  (propertize message 'face 'bold))))))

(defun rec-set-head-line (str)
  "Set the headline in rec buffers to STR."
  (setq header-line-format str))

;;;; Fast selection

(defun rec-fast-selection (names prompt)
  "Fast group tag selection with single keys.

NAMES is an association list of the form:

    ((\"NAME1\" char1) ...)

Each character should identify only one name.

PROMPT is the prompt string to use."
  ;; Adapted from `org-fast-tag-selection' in org.el by Carsten Dominic
  ;; Thanks Carsten! :D
  (let* ((maxlen (apply #'max (mapcar (lambda (name)
                                        (string-width (car name)))
                                      names)))
         ;; (buf (current-buffer))
         (fwidth (+ maxlen 3 1 3))
         (ncol (/ (- (window-width) 4) fwidth))
         name count result char key-list)
    (save-window-excursion
      (set-buffer (get-buffer-create " *Rec Fast Selection*"))
      (delete-other-windows)
      ;;      (split-window-vertically)
      ;; FIXME: This `switch-to-buffer-other-window' can pop up a new frame,
      ;; which `save-window-excursion' won't undo at the end!
      (switch-to-buffer-other-window (get-buffer-create " *Rec Fast Selection*"))
      (erase-buffer)
      (insert prompt ":")
      (insert "\n\n")
      (setq count 0)
      (while (setq name (pop names))
        (setq key-list (cons (cadr name) key-list))
        (insert "[" (cadr name) "] "
                (car name)
                (make-string (- fwidth 4 (length (car name))) ?\ ))
        (when (= (setq count (+ count 1)) ncol)
          (insert "\n")
          (setq count 0)))
      (goto-char (point-min))
      (if (fboundp 'fit-window-to-buffer)
          (fit-window-to-buffer))
      (catch 'exit
        (while t
          (message "[a-z0-9...]: Select entry   [RET]: Exit")
          (setq char (let ((inhibit-quit t)) (read-char-exclusive)))
          (cond
           ((= char ?\r)
            (setq result nil)
            (throw 'exit t))
           ((member char key-list)
            (setq result char)
            (throw 'exit t)))))
      result)))

;;;; Summary mode

(defvar rec-marker)
(make-variable-buffer-local 'rec-marker)

(defvar rec-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'rec-summary-cmd-jump-to-record)
    (define-key map (kbd "RET") 'rec-summary-cmd-jump-to-record)
    (define-key map (kbd "n") 'rec-summary-cmd-next)
    (define-key map (kbd "p") 'rec-summary-cmd-prev)
    (define-key map (kbd "q") 'rec-summary-quit)
    map)
  "Local keymap for `rec-summary-mode' buffers.")

(defun rec-summary-quit ()
  "Quit the summary."
  (interactive)
  (let ((record-buffer rec-summary-rec-buffer))
    (kill-buffer (current-buffer))
    (if (not (eq (selected-window) (next-window nil 'no-minibuf)))
        (delete-window)
      (switch-to-buffer record-buffer))))

(defvar rec-summary-buffer nil
  "The `rec-summary-mode' buffer associated with the rec file.

This variable is not buffer local, as there can be only one
summary buffer open for the current buffer.")

(defvar-local rec-summary-rec-buffer nil
  "The `rec-mode' buffer paired with this summary buffer.")

(define-derived-mode rec-summary-mode tabulated-list-mode "Rec Summary"
  "Major mode for summarizing the contents of a recfile.
\\<rec-summary-mode-map>
\\{rec-summary-mode-map}"
  (setq tabulated-list-format nil)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (add-hook 'post-command-hook #'rec-summary-sync-rec-buffer nil t))

(defun rec-summary-populate (headers entries)
  "Populate a `rec-mode' summary buffer with the data in ENTRIES.

The data has the same structure than `tabulated-list-entries'.
Argument HEADERS specifies the headers to display."
  (setq tabulated-list-format headers)
  (setq tabulated-list-padding 2)
  ;;  (setq tabulated-list-sort-key (cons (car (elt headers 0)) nil))
  (tabulated-list-init-header)
  (setq tabulated-list-entries entries)
  (tabulated-list-print nil))

(defvar rec-summary-inhibit-sync nil
  "Whether to prevent syncing the rec buffer.")

(defun rec-summary-sync-rec-buffer ()
  "Make sure the rec buffer of the summary shows the currently selected record."
  (when  (and (not rec-summary-inhibit-sync)
              rec-summary-rec-buffer
              (tabulated-list-get-id))
    (if (eobp)
        (forward-line -1))
    (unless (eobp)
      (save-excursion
        (let ((there (marker-position (tabulated-list-get-id)))
              (old (selected-window))
              (window (get-buffer-window rec-summary-rec-buffer t)))
          (if window
              (unwind-protect
                  (progn
                    (select-window window)
                    (rec-goto-position there t))
                (select-window old))
            (with-current-buffer rec-summary-rec-buffer
              (rec-goto-position there t))))))))

(defun rec-summary-cmd-next ()
  "Move to the next line in the summary."
  (interactive)
  (forward-line 1)
  (rec-summary-update-overlay)
  (display-buffer rec-summary-rec-buffer))

(defun rec-summary-cmd-prev ()
  "Move to the next line in the summary."
  (interactive)
  (forward-line -1)
  (rec-summary-update-overlay)
  (display-buffer rec-summary-rec-buffer))

(defun rec-summary-cmd-jump-to-record ()
  "Jump to the selected record in the `rec-mode' buffer."
  (interactive)
  ;;  (if (buffer-live-p rec-summary-rec-buffer)
  (let ((rec-marker (tabulated-list-get-id)))
    (with-current-buffer (marker-buffer rec-marker)
      (widen)
      (goto-char (marker-position rec-marker))
      (rec-show-record))))
;;    (message "The rec buffer paired with this summary is not alive.")))

;;;; Database access functions
;;
;; The following functions map the high-level API rec_db_* provided by
;; librec, using the command line utilities instead.  Note that we are
;; using the support for keyword arguments provided by the cl package
;; in order to ease the usage of the functions.
;;
;; Note that the functions are not checking for the integrity of the
;; arguments: it is the invoked recutil which is in charge of that.

(cl-defun rec-query (&rest args
                           &key (type nil) (join nil) (index nil) (sex nil)
                           (fast-string nil) (random nil) (fex nil) (password nil)
                           (group-by nil) (sort-by nil) (icase nil) (uniq nil) (no-sexps nil)
                           (descriptor nil) (values nil))
  "Perform a query in the current buffer using recsel.

ARGS contains the arguments to pass to the program.

Optional argument TYPE speficies the type of records to select.

Optional argument JOIN the field to perform a join operation with.

Optional argument INDEX chooses record at specified index.

Optional argument SEX is a selection expression to use.

Optional argument FAST-STRING is a fast search on the string.

Optional argument RANDOM selects a given number of random records.

Optional argument FEX prints the specified fields (field expression).

Optional argument PASSWORD specifies the password to use.

Optional argument GROUP-BY groups results by given fields.

Optional argument SORT-BY sorts the output by given fields.

Optional argument ICASE makes the selection case-insensitive.

Optional argument UNIQ when non-nil, returns only unique results.

Optional argument NO-SEXPS when non-nil, returns the results in rec format.

Optional argument DESCRIPTOR when non-nil, includes the record descriptor.

Optional argument VALUES when non-nil, returns only the values of the fields.
Requires NO-SEXPS with non-nil value to work properly."
  (let ((buffer (generate-new-buffer "Rec Sel "))
        args status)
    (save-restriction
      (widen)
      (unwind-protect
          (progn
            ;; Prepare the arguments to recsel based on the arguments
            ;; passed to this function.

            (when (stringp type)
              (setq args (cons "-t" (cons type args))))
            (when (stringp join)
              (setq args (cons "-j" (cons join args))))
            (when (integerp index)
              (setq args (cons "-n" (cons (number-to-string index) args))))
            (when (stringp sex)
              (setq args (cons "-e" (cons sex args))))
            (when (stringp fast-string)
              (setq args (cons "-q" (cons fast-string args))))
            (when (integerp random)
              (setq args (cons "-m" (cons (number-to-string random) args))))
            (when (stringp fex)
              (setq args (cons "-p" (cons fex args))))
            (when (stringp values)
              (setq args (cons "-P" (cons values args))))
            (when (stringp password)
              (setq args (cons "-s" (cons password args))))
            (when (stringp group-by)
              (setq args (cons "-G" (cons group-by args))))
            (when (stringp sort-by)
              (setq args (cons "-S" (cons sort-by args))))
            (when icase
              (setq args (cons "-i" args)))
            (when uniq
              (setq args (cons "-U" args)))
            (when (and (not group-by) (not sort-by) (not no-sexps))
              (setq args (cons "--print-sexps" args)))
            (when descriptor
              (setq args (cons "-d" args)))
            ;; Call 'recsel' to perform the query.
            (setq status (apply #'call-process-region
                                (point-min) (point-max)
                                rec-recsel
                                nil ; delete
                                buffer
                                nil ; display
                                args))
            (if (/= status 0)
                (error "Recsel returned error: %d" status))
            (with-current-buffer buffer
              (if (not no-sexps)
                  (progn
                    (goto-char (point-min))
                    (insert "(")
                    (goto-char (point-max))
                    (insert ")")
                    (read (point-min-marker)))
                (buffer-substring-no-properties (point-min) (point-max)))))
        (kill-buffer buffer)))))

;;;; Cross referencing

(defun rec-mode--xref-after-jump-hook ()
  "After jumping via Xref, narrow to the current record if necssary."
  (unless (derived-mode-p 'rec-edit-mode)
    (rec-show-record)))

(defun rec-mode--xref-widen-before-return ()
  "Widen the buffer before returning from xref."
  (unless (derived-mode-p 'rec-edit-mode)
    (rec-show-record)))

(defun rec-cmd-xref-go-back ()
  "Go back in the XREF history.

See `xref-go-back'."
  (interactive)
  (widen)
  (xref-go-back)
  (unless (derived-mode-p 'rec-edit-mode)
    (rec-show-record)))

(defun rec-cmd-xref-go-forward ()
  "Go back in the XREF history.

See `xref-go-forward'."
  (interactive)
  (widen)
  (xref-go-forward)
  (unless (derived-mode-p 'rec-edit-mode)
    (rec-show-record)))


(defun rec-mode--xref-backend ()
  "Return the XREF backend for `rec-mode'."
  'rec)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql rec)))
  "Return a cross referencable identifier for the current record field at point."
  (if-let ((desc (rec-current-record-descriptor))
           (type (rec-record-descriptor-type desc)))
      (if-let ((key (rec-record-descriptor-key desc))
               (value (car-safe (rec-record-assoc key (rec-current-record)))))
          (format "%s '%s'" type value)
        type)
    (when-let ((field (rec-current-field)))
      (rec-field-name field))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql rec)))
  (if-let* ((descriptor (rec-current-record-descriptor))
            (key (rec-record-descriptor-key descriptor)))
      (list key)
    (user-error "Current record type has no %key and cannot be a foreign key")))

(cl-defmethod xref-backend-references ((_backend (eql rec)) _identifier)
  "Find references to the current field with value IDENTIFIER in the recfile."
  (when-let* ((descriptor (rec-current-record-descriptor))
              (key (rec-record-descriptor-key descriptor))
              (type (rec-record-descriptor-type descriptor))
              (value (car-safe (rec-record-assoc key (rec-current-record))))

              ;; Find all records that have "%type: Xx rec FOO", meaning
              ;; a field "Xx: ABC" refers to records of type FOO.
              (descriptors rec-buffer-descriptors)
              (references
               (seq-remove
                #'null
                (seq-map
                 (lambda (descr)
                   (let ((types (rec-record-descriptor-types descr)))
                     (seq-remove
                      #'null
                      (seq-map (lambda (typ)
                                 (let ((elts (split-string typ " ")))
                                   (and (eq 3 (length elts))
                                        (string= "rec" (nth 1 elts))
                                        (string= type (nth 2 elts))
                                        (list (rec-record-descriptor-type descr) (cl-first elts)))))
                               types))))
                 descriptors)))

              ;; Find those that refer to *this* FOO.
              (matching-references (seq-map
                                    (lambda (reference)
                                      (cl-destructuring-bind (ftype field) (car reference)
                                        (cons
                                         ftype
                                         (rec--parse-sexp-records
                                          (rec-query :sex (format "%s = '%s'" field value)
                                                     :descriptor nil
                                                     :fex field
                                                     :type ftype)))))
                                    references)))
    (seq-mapcat
     (lambda (matching-reference)
       (cl-destructuring-bind (source-type . records) matching-reference
         (seq-map (lambda (record)
                    (rec-record-to-xref record source-type (current-buffer) (cons 'sex "bogus")))
                  records)))
     matching-references)))

(cl-defmethod xref-backend-definitions ((_backend (eql rec)) _value)
  "Find the definition of record referenced by the field, if available.

If the VALUE is a foreign key to another record, jump to it.  If not,
does nothing. The referent record type must have %key for that to work."
  (when-let* ((type (rec-current-field-type))
              (source (rec-field-value (rec-current-field))))
    (if (eq 'rec (nth 1 type))
        (let* ((reference (nth 3 type))
               (results (rec--parse-sexp-records
                         (rec-query :descriptor t
                                    :type reference)))
               (descriptor (seq-find #'rec-record-descriptor-p results)))
          (when descriptor
            (if-let* ((key (rec-record-descriptor-key descriptor))
                      (sex (format "%s = '%s'" key source))
                      (target (car-safe
                               (rec--parse-sexp-records
                                (rec-query :descriptor nil
                                           :type reference
                                           :sex sex
                                           :fex key))))
                      (field (seq-find (lambda (field)
                                         (string= key (rec-field-name field)))
                                       (rec-record-fields target))))
                (list
                 (xref-make
                  (rec--xref-summary-for-record target reference (cons 'sex sex))
                  (rec-xref-make-location (current-buffer) (rec-field-position field))))
              (user-error "Impossible reference: target record type '%s' has no '%%key' defined" reference))))
      (user-error "Field '%s' does not refer to anything" (rec-field-name (rec-current-field))))))

;;;; Selection of records
;;
;; The following functions implement selection of records, which
;; maintains a subset of the records in the current buffer.

(defvar rec-current-selection nil
  "List of records corresponding to the last executed selection.")
(make-variable-buffer-local 'rec-current-selection)

(defun rec-navigate-selection ()
  "Goto the first record of the current selection, if any."
  (if (not rec-current-selection)
      (message "No current selection")
    (widen)
    (let* ((first-record (car rec-current-selection))
           (pos (rec-record-position first-record)))
      (goto-char pos)
      (rec-show-record))))

(defun rec-cmd-select ()
  "Perform a selection on the current buffer using some criteria.

The result of the selection is stored in `rec-current-selection'."
  (interactive)
  (setq rec-current-selection (rec-query)))

(defun rec-cmd-jump-to-fast (prefix fast-string)
  "Jump to the first record matching FAST-STRING.

Argument PREFIX when non-nil means to use a case-insensitive search."
  (interactive "P\nsFast string query: ")
  (when (not (equal fast-string ""))
    (setq rec-current-selection
          (rec--parse-sexp-records
           (rec-query :fast-string fast-string
                      :icase prefix
                      :type (rec-record-type))))
    (rec-navigate-selection)))

(defun rec-cmd-jump-to-sex (prefix sex)
  "Jump to the first record matching SEX.

A PREFIX argument means to use a case-insensitive search.
Argument SEX is the selection expression to use."
  (interactive "P\nsSelection expression: ")
  (when (not (equal sex ""))
    (setq rec-current-selection
          (rec--parse-sexp-records
           (rec-query :sex sex
                      :icase prefix
                      :type (rec-record-type))))
    (rec-navigate-selection)))

;;;;; Selecting into a new buffer

(defun rec--new-buffer-for-selection (selection expr)
  "Generate a new buffer for SELECTION and switch to it."
  (if selection
      (let* ((name (generate-new-buffer-name
                    (format "*Selection of %s*" (buffer-name))))
             (origin (buffer-name))
             (buf (get-buffer-create name)))
        (with-current-buffer buf
          (insert
           (format "# Generated from %s using expression %s" origin expr))
          (newline)
          (insert selection)
          (goto-char (point-min))
          (rec-edit-mode)
          (run-hooks 'hack-local-variables-hook))
        (rec-update-buffer-descriptors)
        (switch-to-buffer buf))
    (user-error "No results.?")))

(defun rec-cmd-new-buffer-from-sex (sex)
  "Query the current buffer using SEX and insert the result into a new buffer."
  (interactive
   (list (read-string "Selection expression: "
                      nil
                      'rec-selection-sex-history)))
  (when sex
    (rec--new-buffer-for-selection
     (rec-query :type (rec-record-type)
                :sex sex
                :descriptor t
                :no-sexps t)
     sex)))


(defun rec-cmd-new-buffer-from-fast-string (fast-string)
  "Query the current buffer using FAST-STRING.

Inserts the result into a new buffer."
  (interactive
   (list (read-string "Fast string search: "
                      nil
                      'rec-selection-fast-history)))
  (when fast-string
    (rec--new-buffer-for-selection
     (rec-query :type (rec-record-type)
                :fast-string fast-string
                :descriptor t
                :no-sexps t)
     fast-string)))


;;;;; Record selection mode

(defvar-local rec-selection-mode nil)

;;;;;; Classes that represent the current selection

(cl-defgeneric rec-selection-query (selection &optional fex)
  "Query records on the current buffer using SELECTION.
Optionally select only the fields in FEX.")

(cl-defgeneric rec-selection-stringify (selection)
  "Return a string representation of SELECTION.")

(cl-defgeneric rec-selection-expr (selection)
  "Return the actual expression used in the selection of SELECTION.")

(cl-defstruct rec-selection
  "A query to restrict candidates for the current buffer."
  type icase)

(cl-defstruct (rec-selection-sex (:include rec-selection))
  "A selection based on selection expressions."
  sex)

(cl-defstruct (rec-selection-fast (:include rec-selection))
  "A fast string search selection."
  fast)

(cl-defmethod rec-selection-expr ((selection rec-selection-fast))
  (rec-selection-fast-fast selection))

(cl-defmethod rec-selection-stringify ((selection rec-selection-fast))
  (format "%s[%s]" (rec-selection-type selection) (rec-selection-fast-fast selection)))

(cl-defmethod rec-selection-query ((selection rec-selection-fast) &optional fex)
  (rec-query :type (rec-selection-type selection)
             :fex fex
             :icase (rec-selection-icase selection)
             :fast-string (rec-selection-fast-fast selection)))

(cl-defmethod rec-selection-expr ((selection rec-selection-sex))
  (rec-selection-sex-sex selection))

(cl-defmethod rec-selection-stringify ((selection rec-selection-sex))
  (format "%s / %s" (rec-selection-type selection) (rec-selection-sex-sex selection)))

(cl-defmethod rec-selection-query ((selection rec-selection-sex) &optional fex)
  "Query records using a selection expression."
  (rec-query :type (rec-selection-type selection)
             :fex fex
             :icase (rec-selection-icase selection)
             :sex (rec-selection-sex-sex selection)))

;;;;;; Variables for containing the selectionk

(defvar-local rec-selection-current-selection nil
  "The currently active selection expression.")

(defvar-local rec-selection-current-record-type nil
  "The record type being navigated in selection mode.")

(defvar-local rec-selection-previous-mode-line nil
  "The mode line text before the selection.")

(defvar rec-selection-sex-history nil
  "The history of record selection history.")

;;;;;; Functions for entering selection mode

(defun rec-begin-selection (selection)
  "Enter `rec-selection-mode' via SELECTION."
  (when (or (not rec-selection-mode)
            (y-or-n-p "You are currently navigating a selection.  Do you want to quit navigation and start with another selection? "))
    (let ((results (rec--parse-sexp-records
                    (rec-selection-query selection))))
      (if results
          (progn
            (setq rec-current-selection results)
            (when rec-selection-mode
              (rec-cmd-exit-selection))

            (setq rec-selection-previous-mode-line mode-line-buffer-identification)
            (setq rec-selection-current-selection selection)
            (rec-selection-mode)
            (save-window-excursion
              (when (buffer-live-p rec-summary-buffer)
                (rec-cmd-show-summary)))
            (rec-cmd-goto-next-selection-record 0))
        (user-error "The search returned no results")))))

(defun rec-cmd-navigate-current-type-by-sex (prefix sex)
  "Query records with SEX and restrict navigation to those records.
Prefix argument PREFIX if non-nil means ignore case.

See `rec-selection-mode'."
  (interactive
   (let ((prev (if rec-selection-current-selection
                   (rec-selection-expr rec-selection-current-selection))))
     (list current-prefix-arg
           (read-string
            (format "Navigate %s records by selection expression%s: "
                    (rec-record-type)
                    (if prev
                        (format " (default %s)" prev)
                      ""))
            nil 'rec-selection-sex-history prev))))
  (when (not (equal sex ""))
    (rec-begin-selection
     (make-rec-selection-sex :sex sex
                             :icase prefix
                             :type (rec-record-type)))))

(defvar rec-selection-fast-history nil
  "The history of record selection history (fast search).")

(defun rec-cmd-navigate-current-type-by-fast-string (prefix fast-string)
  "Query records with fast-string  and restrict navigation to those records.

Prefix argument PREFIX if non-nil means ignore case.

See `rec-selection-mode'."
  (interactive
   (let ((prev (if rec-selection-current-selection
                   (rec-selection-expr rec-selection-current-selection))))
     (list current-prefix-arg
           (read-string
            (format "Navigate %s records by fast string search%s: "
                    (rec-record-type)
                    (if prev
                        (format " (default %s)" prev)
                      ""))
            nil 'rec-selection-fast-history prev))))
  (when (not (equal fast-string ""))
    (rec-begin-selection
     (make-rec-selection-fast :fast fast-string
                              :type (rec-record-type)
                              :icase prefix))))

(defun rec-cmd-exit-selection ()
  "Exit `rec-selection-mode'."
  (interactive)
  (when rec-selection-mode
    (rec-selection-mode -1)
    (save-window-excursion
      (when (buffer-live-p rec-summary-buffer)
        (rec-cmd-show-summary)))
    (put 'rec-summary-buffer 'selection nil)
    (setq mode-line-buffer-identification rec-selection-previous-mode-line)))

(defvar rec-show-hook nil
  "List of functions to call when a record is shown.")

(defun rec-cmd-goto-next-selection-record (n)
  "Goto the next record in the current selection.

Prefix arguments N moves next by N records."
  (interactive "P")
  (if rec-current-selection
      (let* ((record (rec-current-record))
             (pos (rec-record-position record))
             (where-am-i
              (cl-position-if
               (lambda (rec)
                 (= pos (byte-to-position (rec-record-position rec))))
               rec-current-selection))
             (next (if (numberp where-am-i)
                       (nth (+ where-am-i (or n 1)) rec-current-selection)
                     (car rec-current-selection))))
        (if (and next (or (/= pos (rec-record-position next)) (zerop n)))
            (rec-goto-record next)
          (user-error
           (if rec-selection-current-selection
               (format "No more records of type %s in selection %s"
                       (rec-record-type rec-selection-current-selection)
                       (rec-selection-expr rec-selection-current-selection))
             (format "No more records in selection %s" (rec-selection-expr rec-selection-current-selection))))))
    (user-error "No active selection")))

(defun rec-cmd-goto-prev-selection-record (n)
  "Goto the previous record in the current selection.

Prefix arguments N moves next by N records."
  (interactive "P")
  (rec-cmd-goto-next-selection-record (- (or n 1))))



;;;;; Selection cross reference

(cl-defgeneric rec--xref-summary-for-record (record type kind)
  "Return a formated summary line for RECORD of type TYPE using KIND."
  (let* ((pos (byte-to-position (rec-record-position record)))
         (line-number (number-to-string
                       (save-restriction
                         (widen)
                         (line-number-at-pos pos t))))
         (heading (concat (propertize type 'face 'font-lock-type-face)
                          " at line "
                          line-number)))

    (add-face-text-property 0 (length heading) 'bold nil heading)
    (format "%s\n%s"
            heading
            (rec--xref-truncate-fields record kind))))

(defun rec-record-to-xref (record type buffer kind)
  "Make an xref object out of a record structure.

If TYPE is nil, the summary line will show just 'Record'.  BUFFER is the buffer
from which to display results.  The KIND determines"
  (xref-make
   (rec--xref-summary-for-record record type kind)
   (rec-xref-make-location buffer (or (byte-to-position (rec-record-position record)) 0))))


(cl-defgeneric rec--xref-truncate-fields (record kind)
  "Truncate fields of RECORD of search KIND.")

(cl-defgeneric rec--xref-truncate-fields (record (_kind (head sex)))
  "Truncate fields from a selection expression SEX of RECORD.

Takes up to the first three elements of a record and displays them, padded
with four spaces."
  (let* ((rec-fields (rec-record-fields record))
         (fields (mapconcat
                  (lambda (field)
                    (concat
                     "    "
                     (with-temp-buffer
                       (rec-insert field)
                       (string-trim-right
                        (rec-mode--syntax-highlight (buffer-string))))))
                  (cl-subseq rec-fields 0 (min (length rec-fields) 3))
                  "\n")))
    (if (< 3 (length rec-fields))
        (concat fields "\n    ...")
      fields)))

(cl-defgeneric rec--xref-truncate-fields (record (kind (head fast)))
  "Truncate fields for KIND fast string searches in RECORD."
  (let* ((fields (rec-record-fields record))
         (matching (seq-filter
                    (lambda (field)
                      (cl-search (cdr kind) (rec-field-value field)))
                    fields)))
    (mapconcat
     (lambda (field)
       (concat "    "
               (let* ((full (rec-mode--syntax-highlight
                             (with-temp-buffer
                               (rec-insert field)
                               (string-trim-right
                                (buffer-string))))))
                 (when (string-match (regexp-quote
                                      (cdr kind))
                                     full)
                   (put-text-property (match-beginning 0)
                                      (match-end 0)
                                      'face
                                      'highlight
                                      full))
                 full)))
     matching
     "\n")))

(defun rec-xref-make-location (buffer position)
  "Make an XREF object out of BUFFER and POSITION.

Aims to be backwards compatible with Emacs versions
28 and below."
  (if (fboundp 'xref-make-buffer-location)
      (xref-make-buffer-location buffer position)
    (xref-buffer-location buffer :position position)))

(defun rec--xref-query (query kind)
  "Make a XREF results list using QUERY identified by KIND."
  (let* ((results (rec--parse-sexp-records query))
         (descriptor (cl-find-if #'rec-record-descriptor-p results))
         (type (if descriptor
                   (rec-record-descriptor-type descriptor)
                 "Record"))
         (data (if descriptor
                   (cdr results)
                 results)))
    (when data
      (xref--show-xrefs
       (mapcar
        (lambda (record)
          (xref-make
           (rec--xref-summary-for-record record type kind)
           (rec-xref-make-location (current-buffer)
                                   (byte-to-position (rec-record-position record)))))
        data)
       nil))))

(defun rec-cmd-xref-sex (sex)
  "Cross reference records for SEX.

Creates an XREF buffer with entries of the current record type
in the current buffer matching the selection expression."
  (interactive
   (list (read-string "Selection expression: "
                      nil 'rec-selection-sex-history)))
  (rec--xref-query
   (rec-query :sex sex
              :descriptor t
              :icase t
              :type (rec-record-type))
   (cons 'sex sex)))


(defun rec-cmd-xref-fast-string (fast-string)
  "Cross reference records for FAST-STRING.

Creates an XREF buffer with entries of the current record type
in the current buffer matching the fast string search."
  (interactive
   (list (read-string "Fast string search: "
                      nil 'rec-selection-sex-history)))
  (rec--xref-query
   (rec-query :fast-string fast-string
              :descriptor t
              :icase t
              :type (rec-record-type))
   (cons 'fast fast-string)))


;;;; Commands
;;
;; The following functions implement interactive commands available in
;; the several modes defined in this file.
(defvar rec-field-name)
(make-variable-buffer-local 'rec-field-name)
(defvar rec-buffer)
(make-variable-buffer-local 'rec-buffer)
(defvar rec-jump-back nil)
(make-variable-buffer-local 'rec-jump-back)
(defvar rec-update-p nil)
(make-variable-buffer-local 'rec-update-p)
(defvar rec-preserve-last-newline nil)
(make-variable-buffer-local 'rec-preserve-last-newline)

(defvar rec-prev-buffer nil
  "The previous buffer we were in before jumping into `rec-edit-field-mode'.")
(make-variable-buffer-local 'rec-prev-bufffer)

(defvar rec-pointer nil
  "The previous position in `rec-prev-buffer' we were at.

The position is recorded before jumping into `rec-edit-field-mode'.")
(make-variable-buffer-local 'rec-point)

(defvar rec-prev-window-configuration nil
  "The window configuration before jumping into `rec-edit-field-mode'.")
(make-variable-buffer-local 'rec-prev-window-configuration)

(defconst rec-cmd-edit-field-message
  "Edit the value of the field and use \\[rec-finish-editing-field] to exit"
  "Message to display when entering `rec-edit-field-mode'.")

(defun rec-cmd-edit-field (n)
  "Edit the contents of the field under point in a separate buffer.

The input method used for getting the field value depends on its
type, unless a prefix argument N is used.  Then the more general
method, i.e. asking for the new value in an unrestricted buffer,
will be used for fields of any type."
  (interactive "P")
  (let* ((field (rec-current-field))
         (field-value (rec-field-value field))
         (field-name (rec-field-name field))
         (field-type (rec-field-type field-name))
         (field-type-kind (when field-type (rec-type-kind field-type)))
         (pointer (rec-beginning-of-field-pos))
         (prev-buffer (current-buffer)))
    (if field-value
        (cond
         ((and (member field-type-kind '(enum bool))
               (null n))
          (let* ((data (rec-type-data field-type))
                 (fast-selection-data
                  (cond
                   ((equal field-type-kind 'enum)
                    (let (used-letters)
                      (mapcar
                       (lambda (elem)
                         ;; Assign a letter to this enumerated entry.
                         ;; The letters are chosen using the following
                         ;; criteria: if the first letter of the entry
                         ;; is free then use it.  Otherwise, if the
                         ;; second letter of the entry is free then
                         ;; use it.  Otherwise use the first available
                         ;; symbol in the alphabet. Note that ELEM
                         ;; cannot be the empty string.
                         (let ((letter (if (not (member (elt elem 0) used-letters))
                                           (elt elem 0)
                                         (if (and (> (length elem) 1)
                                                  (not (member (elt elem 1) used-letters)))
                                             (elt elem 1)
                                           (let* ((c ?a) (i 0))
                                             (while (member c used-letters)
                                               (setq c (+ ?a i))
                                               (setq i (+ i 1)))
                                             c)))))
                           (setq used-letters (cons letter used-letters))
                           (list elem letter)))
                       data)))
                   ((equal field-type-kind 'bool)
                    '(("yes" ?y) ("no" ?n) ("1" ?o) ("0" ?z) ("true" ?t) ("false" ?f)))
                   (t
                    (error "Invalid kind of type"))))
                 (letter (rec-fast-selection fast-selection-data "New value")))
            (when letter
              (let ((inhibit-read-only t)
                    new-value)
                (mapc
                 (lambda (elem)
                   (when (equal letter (cadr elem))
                     (setq new-value (car elem))))
                 fast-selection-data)
                (rec-delete-field)
                (save-excursion
                  (rec-insert
                   (make-rec-field :position 0
                                   :name field-name
                                   :value new-value)))
                (rec-finish-editing-move)))))
         ((and (equal field-type-kind 'date) rec-popup-calendar
               (null n))
          (setq rec-field-name field-name)
          (setq rec-prev-buffer prev-buffer)
          (setq rec-pointer pointer)
          (calendar)
          (let ((old-map (current-local-map)) ;Isn't this calendar-mode-map?
                (map (make-sparse-keymap)))
            (set-keymap-parent map calendar-mode-map)
            (define-key map "q"
                        (lambda () (interactive)
                          (use-local-map old-map)
                          (calendar-exit)))
            (define-key map "t"
                        (lambda () (interactive)
                          (use-local-map old-map)
                          (calendar-exit)
                          (set-buffer rec-prev-buffer)
                          (let ((inhibit-read-only t))
                            (rec-delete-field)
                            (save-excursion
                              (rec-insert
                               (make-rec-field :position 0
                                               :name rec-field-name
                                               :value (format-time-string rec-time-stamp-format))))
                            (rec-finish-editing-move))))
            (define-key map (kbd "RET")
                        (lambda () (interactive)
                          (let* ((date (calendar-cursor-to-date))
                                 (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
                            (use-local-map old-map)
                            (calendar-exit)
                            (set-buffer rec-prev-buffer)
                            (let ((inhibit-read-only t))
                              (rec-delete-field)
                              (save-excursion
                                (rec-insert
                                 (make-rec-field :position 0
                                                 :name rec-field-name
                                                 :value (format-time-string "%Y-%m-%d" time))))
                              (rec-finish-editing-move)))))
            (use-local-map map)
            (message "[RET]: Select date [t]: Time-stamp     [q]: Exit")))
         (t
          (let ((edit-buf (get-buffer-create "Rec Edit")))
            (set-buffer edit-buf)
            (delete-region (point-min) (point-max))
            (rec-edit-field-mode)
            (setq rec-field-name field-name)
            (setq rec-marker (make-marker))
            (set-marker rec-marker pointer prev-buffer)
            (setq rec-prev-buffer prev-buffer)
            (setq rec-pointer pointer)
            (setq rec-prev-window-configuration (current-window-configuration))
            (insert field-value)
            (switch-to-buffer-other-window edit-buf)
            (goto-char (point-min))
            (message (substitute-command-keys rec-cmd-edit-field-message)))))
      (message "Not in a field"))))

(defun rec-finish-editing-move ()
  "Move point to the next field or line after editing.

If there is no next field, move forward one line, unless
`rec-editing' is nil, we are on a single record, so we're not
going to move past the last line.

If the movement would jump to a next record, this moves forward one line.

If `rec-move-to-next-line-after-edit' is nil, do nothing."
  (when rec-move-to-next-line-after-edit
    (if (let ((this-end (rec-end-of-record-pos)))
          (save-excursion
            (rec-goto-next-field)
            (< (line-end-position) this-end)))
        (rec-goto-next-field)
      (when (derived-mode-p 'rec-edit-mode)
        (forward-line 1)))))

(defun rec-finish-editing-field (&optional stay)
  "Stop editing the value of a field, and move on the next field.

If no field is next, move forward one line.

If `rec-move-to-next-line-after-edit' is non-nil, moves to the
next field or line.

Prefix argument STAY means stay on the field we just edited."
  (interactive "P")
  (let ((marker rec-marker)
        (prev-pointer rec-pointer)
        (edit-buffer (current-buffer))
        (name rec-field-name)
        (value (buffer-substring-no-properties (point-min) (point-max))))
    (set-window-configuration rec-prev-window-configuration)
    (switch-to-buffer rec-prev-buffer)
    (let ((inhibit-read-only t))
      (kill-buffer edit-buffer)
      (goto-char marker)
      (rec-delete-field)
      (rec-insert (make-rec-field :position 0
                                  :name name
                                  :value value))
      (goto-char prev-pointer)
      (unless (derived-mode-p 'rec-edit-mode)
        (rec-hide-continuation-line-markers))
      (unless stay
        (rec-finish-editing-move)))))

(defun rec-beginning-of-field ()
  "Goto to the beginning of the current field."
  (interactive)
  (let ((pos (rec-beginning-of-field-pos)))
    (when pos
      (goto-char pos))))

(defun rec-end-of-field ()
  "Goto to the end of the current field."
  (interactive)
  (let ((pos (rec-end-of-field-pos)))
    (when pos
      (goto-char pos))))

(defun rec-beginning-of-record ()
  "Goto to the beginning of the current record."
  (interactive)
  (let ((pos (rec-beginning-of-record-pos)))
    (when pos
      (goto-char pos))))

(defun rec-end-of-record ()
  "Goto to the end of the current record."
  (interactive)
  (let ((pos (rec-end-of-record-pos)))
    (when pos
      (goto-char pos))))

(defun rec-delete-field ()
  "Delete the current field."
  (interactive)
  (let ((begin-pos (rec-beginning-of-field-pos))
        (end-pos (rec-end-of-field-pos)))
    (when (and begin-pos end-pos)
      (delete-region begin-pos end-pos)
      (when (equal (char-after) ?\n)
        (delete-char 1)))))

(defun rec-copy-record ()
  "Copy the current record."
  (interactive))

(defun rec-find-type ()
  "Goto the beginning of the descriptor with a given type."
  (interactive)
  (let ((type (completing-read "Record type: "
                               (save-restriction
                                 (widen)
                                 (rec-buffer-types)))))
    (if (equal type "") (setq type nil))
    (rec-show-type type)))

(defun rec-cmd-goto-next-field ()
  "Move to the next field.

Interactive version of `rec-goto-next-field'."
  (interactive)
  (if (save-excursion
        (not (rec-goto-next-field)))
      (if (derived-mode-p 'rec-edit-mode)
          (progn
            (goto-char (point-min))
            (unless (looking-at rec-field-name-re)
              (rec-goto-next-field)))
        (rec-beginning-of-record))
    (rec-goto-next-field)))

(defun rec-cmd-goto-next-rec (&optional n)
  "Move to the next record of the same type.

Interactive version of `rec-goto-next-rec'.
Optional argument N specifies number of records to skip."
  (interactive "P")
  (when (null n) (setq n 1))
  (widen)
  (let ((record-type (rec-record-type)))
    (dotimes (_ n)
      (if (save-excursion
	    (and (rec-goto-next-rec)
		 (equal (rec-record-type) record-type)
		 (not (rec-record-descriptor-p (rec-current-record)))))
	  (progn
	    (rec-unfold-all-fields)
	    (rec-remove-continuation-line-marker-overlays)
	    (rec-goto-next-rec))
	(if (not (rec-record-type))
	    (message "No more records")
	  (message "%s" (concat "No more records of type "
				(rec-record-type)))))))
  (unless (derived-mode-p 'rec-edit-mode)
    (rec-show-record)
    (rec-summary-move-to-record (rec-current-record))))

(defun rec-cmd-goto-previous-rec (&optional n)
  "Move to the previous record of the same type.

Interactive version of `rec-goto-previous-rec'.
Optional argument N specifies number of records to skip."

  (interactive "P")
  (when (null n) (setq n 1))
  (widen)
  (let ((record-type (rec-record-type)))
    (dotimes (_ n)
      (if (save-excursion
	    (and (rec-goto-previous-rec)
		 (equal (rec-record-type) record-type)
		 (not (rec-record-descriptor-p (rec-current-record)))))
	  (progn
	    (rec-unfold-all-fields)
	    (rec-remove-continuation-line-marker-overlays)
	    (rec-goto-previous-rec))
	(if (not (rec-record-type))
	    (message "No more records")
	  (message "%s" (concat "No more records of type "
				(rec-record-type)))))))
  (unless (derived-mode-p 'rec-edit-mode)
    (rec-show-record))
  (rec-summary-move-to-record (rec-current-record)))

(defun rec-cmd-undo ()
  "Undo a change in the buffer when in navigation mode."
  (interactive)
  (let ((inhibit-read-only t))
    (undo)))

(defun rec-cmd-jump-back ()
  "Undo the previous jump."
  (interactive)
  (if rec-jump-back
      (progn
        (widen)
        (goto-char (marker-position rec-jump-back))
        (unless (derived-mode-p 'rec-edit-mode)
          (rec-show-record))
        (setq rec-jump-back nil))
    (message "No previous position to jump")))

(defvar-local rec-edit-mode-type nil
  "The kind of thing we are navigating.

One of `buffer', `record' or `type'.")

(defun rec-edit-record ()
  "Go to the record edition mode."
  (interactive)
  (major-mode-suspend)
  (save-restriction
    (rec-edit-mode))
  (rec-set-head-line (substitute-command-keys "Editing record - use \\[rec-finish-editing] to return to navigation mode"))
  (setq rec-edit-mode-type 'record)
  (rec-update-mode-line)
  (setq rec-update-p nil)
  (setq rec-preserve-last-newline t))

(defun rec-edit-type ()
  "Go to the type edition mode."
  (interactive)
  (major-mode-suspend)
  (rec-edit-mode)
  (widen)
  (rec-narrow-to-type (rec-record-type))
  (setq rec-update-p t)
  (rec-set-head-line (concat "Editing type "
                             "'" (rec-record-type) "'"
                             (substitute-command-keys " - use \\[rec-finish-editing] to return to navigation mode")))
  (setq rec-edit-mode-type 'type)
  (rec-update-mode-line))

(defun rec-edit-buffer ()
  "Go to the buffer edition mode."
  (interactive)
  (major-mode-suspend)
  (rec-edit-mode)
  (widen)
  (setq rec-update-p t)
  (rec-set-head-line (substitute-command-keys "Editing buffer - use \\[rec-finish-editing] to return to navigation mode"))
  (setq rec-edit-mode-type 'buffer)
  (rec-update-mode-line)
  (recenter-top-bottom))

(defun rec-finish-editing ()
  "Go back from the record, type or buffer edition mode."
  (interactive)
  (when (or (not rec-update-p)
            (and rec-update-p
                 (save-restriction (widen) (rec-update-buffer-descriptors-and-check t))))
    (save-excursion
      (save-restriction
        (major-mode-restore)))
    (or (rec-current-record)
        (rec-goto-next-rec)
        (rec-goto-previous-rec))
    (when rec-preserve-last-newline
      (save-excursion
        (goto-char (point-max))
        (unless (equal (char-before) ?\n)
          (insert ?\n))))
    (setq rec-update-p nil)
    (rec-show-record)
    (rec-set-head-line nil)
    (rec-update-mode-line)
    (message "End of edition")))

(defun rec-cmd-show-descriptor ()
  "Show the descriptor record of the current record.

This jump sets jump-back."
  (interactive)
  (let ((type (rec-record-type)))
    (when type
      (setq rec-jump-back (point-marker))
      (if (derived-mode-p 'rec-edit-mode)
          (rec-goto-type type)
        (rec-show-type type t)))))

(defun rec-cmd-show-type ()
  "Show the descriptor corresponding to the field under point, in the modeline."
  (interactive)
  (let ((type (rec-current-field-type)))
    (if type
        (display-message-or-buffer (rec-type-text type))
      (message "Unrestricted text"))))

(defun rec-cmd-count (n)
  "Show number of records of the current type in the minibuffer.

Prefix argument N prompts for the selection expression to use, otherwise
the value of the current field is used to specify the selection expression."
  (interactive "P")
  (let* ((default-sex (let ((current-field (rec-current-field)))
                        (when (and current-field
                                   (not (string-match "\n" (rec-field-value current-field)))
                                   (< (length (rec-field-value current-field)) 20))
                          (concat (rec-field-name current-field) " = '" (rec-field-value current-field) "'"))))
         (sex (and (not (null n)) (read-from-minibuffer (concat "Selection expression"
                                                                (if default-sex
                                                                    (concat " (default " default-sex ")")
                                                                  "")
                                                                ": ")))))
    (when (equal sex "")
      (setq sex default-sex))
    (message "Counting records...")
    (let ((type (rec-record-type)))
      (message "%s" (concat (number-to-string (rec-count type sex))
                            (if (or (not type)
                                    (equal type ""))
                                " records"
                              (concat " records of type " type))
                            (when (and sex (not (equal sex "")))
                              (concat " with sex " sex)))))))

(defun rec-cmd-statistic ()
  "Display statistics of the current field value.

Displays a statistic on the occurrence of the value contained in
the field under point in the minibuffer, if any.

This command is especially useful with enumerated types."
  (interactive)
  (let* ((field (rec-current-field))
         (field-name (rec-field-name field))
         (type (rec-field-type field-name))
         (type-kind (when type (rec-type-kind type))))
    (cond ((equal type-kind 'enum)
           (let* ((keys (rec-type-data type))
                  (total (rec-count (rec-record-type)))
                  (percentages (mapcar (lambda (key)
                                         (let ((key-count (rec-count (rec-record-type)
                                                                     (concat field-name " = '" key "'"))))
                                           (list key key-count (/ (* key-count 100) total))))
                                       keys))
                  str)
             (mapc (lambda (occurrence)
                     (setq str (concat str
                                       (number-to-string (nth 1 occurrence))
                                       " "
                                       (nth 0 occurrence)
                                       " ("
                                       (number-to-string (nth 2 occurrence))
                                       "%) ")))
                   percentages)
             (message "%s" str))))))

(defun rec-cmd-append-field ()
  "Goto the end of the record and switch to edit record mode."
  (interactive)
  (unless (derived-mode-p 'rec-edit-mode)
    (rec-edit-record)
    (goto-char (point-max))
    (insert "\n")
    (backward-char)))

(defun rec-cmd-trim-field-value ()
  "Trim the value of the field under point, if any."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
          (field (rec-current-field)))
      (setq field (rec-field-trim-value field))
      (rec-delete-field)
      (rec-insert field))))

(defun rec-cmd-compile ()
  "Compile the current file with recfix."
  (interactive)
  (let ((cur-buf (current-buffer))
        (cmd (concat rec-recfix " "))
        (tmpfile (make-temp-file "rec-mode-")))
    (if buffer-file-name
        (setq cmd (concat cmd (shell-quote-argument buffer-file-name)))
      (with-temp-file tmpfile
        (insert-buffer-substring cur-buf))
      (setq cmd (concat cmd (shell-quote-argument tmpfile))))
    (compilation-start cmd)))

(defun rec-cmd-show-info ()
  "Show information about the recfile in the modeline."
  (interactive)
  (let ((cur-buf (current-buffer))
        (filename (if buffer-file-name
                      buffer-file-name
                    (make-temp-file "rec-mode-")))
        (msg ""))
    (if (not buffer-file-name)
        (with-temp-file filename
          (insert-buffer-substring cur-buf)))
    (with-temp-buffer
      (call-process rec-recinf
                    nil ; infile
                    t   ; output to current buffer
                    nil ; display
                    filename)
      (setq msg (buffer-substring-no-properties (point-min)
                                                (point-max))))
    ;; Delete temporary file.
    (if (not buffer-file-name)
        (delete-file filename))
    ;; Show the message.
    (setq msg (replace-regexp-in-string "\n$" "" msg))
    (setq msg (replace-regexp-in-string "\n" ", " msg))
    (message "%s" msg)))

(defun rec-cmd-toggle-field-visibility ()
  "Toggle the visibility of the field under point."
  (interactive)
  (when (rec-field-p (rec-current-field))
    (if (rec-field-folded-p)
        (rec-unfold-field)
      (rec-fold-field))))

(defun rec-cmd-kill-field ()
  "Kill the current field."
  (interactive)
  (let ((begin-pos (rec-beginning-of-field-pos))
        (end-pos (rec-end-of-field-pos)))
    (if (and begin-pos end-pos)
        (kill-region begin-pos end-pos)
      (message "Not in a field"))))

(defun rec-cmd-copy-field ()
  "Copy the current field."
  (interactive)
  (let ((begin-pos (rec-beginning-of-field-pos))
        (end-pos (rec-end-of-field-pos)))
    (if (and begin-pos end-pos)
        (progn
          (copy-region-as-kill begin-pos end-pos)
          (message "Field copied to kill ring"))
      (message "Not in a field"))))

(defun rec-cmd-kill-record ()
  "Kill the current record."
  (interactive)
  (let ((begin-pos (rec-beginning-of-record-pos))
        (end-pos (rec-end-of-record-pos)))
    (if (and begin-pos end-pos)
        (progn
          (when (looking-back "^[ \t]*" (line-beginning-position))
            ;; Delete the newline before the record as well, but do
            ;; not include it in the kill ring.
            (delete-region (match-beginning 0) (+ (match-end 0) 1)))
          (kill-region begin-pos end-pos))
      (message "Not in a record"))))

(defun rec-cmd-copy-record ()
  "Copy the current record."
  (interactive)
  (let ((begin-pos (rec-beginning-of-record-pos))
        (end-pos (rec-end-of-record-pos)))
    (if (and begin-pos end-pos)
        (progn
          (copy-region-as-kill begin-pos end-pos)
          (message "record copied to kill ring"))
      (message "Not in a record"))))

;;;; Summary

(defvar-local rec-summary-overlay nil
  "The overlay on a highlighted record summary line.")

(defun rec-summary-update-overlay ()
  (unless rec-summary-overlay
    (setq rec-summary-overlay (make-overlay (point) (point))))
  (move-overlay rec-summary-overlay
                (line-beginning-position)
                (line-end-position))
  (overlay-put rec-summary-overlay 'face 'highlight))

(defmacro rec-select-summary (&rest body)
  "Execute BODY in the rec summary, if it exists."
  (declare (indent 0) (debug (&rest form)))
  `(if (and rec-summary-buffer (get-buffer-window rec-summary-buffer t))
       (let ((selected (selected-window)))
         (save-excursion
           (unwind-protect
               (progn
                 (pop-to-buffer rec-summary-buffer)
                 ,@body)
             (select-window selected))))
     (with-current-buffer rec-summary-buffer
       ,@body)))

(defun rec-summary-move-to-record (record)
  "Move the cursor in the summary buffer to the position of RECORD."
  (when (buffer-live-p rec-summary-buffer)
    (let ((target (rec-record-position record))
          (rec-summary-inhibit-sync t)
          where)
      (with-current-buffer rec-summary-buffer
        (goto-char (point-min))
        (cl-loop for current = (marker-position (tabulated-list-get-id))
                 until (eq target current)
                 do (forward-line 1)
                 finally (setq where (point))))
      (when (integer-or-marker-p where)
        (rec-select-summary
          (goto-char where)
          (rec-summary-update-overlay))))))

(defvar-local rec-summary-selection nil
  "The selection that was used to display the summary.")

(defun rec-cmd-show-summary (&optional use-selection)
  "Show a window with a summary of the contents of the current record set.

The fields used to build the summary are determined in the
following way: if there is a %summary field in the record
descriptor of the current record set then it must contain a comma
separated list of fields.  Otherwise the %key is used.  Otherwise
the user is prompted.

When USE-SELECTION is non-nil, present the summary buffer for the currently
active selection in `rec-selection-current-selection'."
  (interactive)
  (let* ((summary-buffer-name (concat (buffer-name (current-buffer)) " Summary"))
         (current-record (save-excursion
                           (rec-beginning-of-record)
                           (rec-current-record)))
         (buffer (or rec-summary-buffer (get-buffer summary-buffer-name))))
    (when (and (buffer-live-p buffer)
               ;; Kill the buffer if we have a selection but
               ;; the buffer is not on a selection and vice versa.
               (or (and (get 'rec-summary-buffer 'selection)
                        (not rec-selection-mode))
                   (and (not (get 'rec-summary-buffer 'selection))
                        rec-selection-mode)))
      (kill-buffer buffer))
    (if (buffer-live-p buffer)
        (switch-to-buffer rec-summary-buffer)
      (let ((summary-fields (rec-summary-fields)))
        (unless summary-fields
          (setq summary-fields (list (rec-key)))
          (unless (car summary-fields)
            (setq summary-fields (split-string (read-from-minibuffer "Fields to use in the summary: ") "[ ,]"))))
        (if (car summary-fields)
            (let* ((fex (string-join summary-fields ","))
                   (query (if (and rec-selection-mode rec-selection-current-selection)
                              (rec-selection-query rec-selection-current-selection fex)
                            (rec-query :type (rec-record-type)
                                       :fex fex)))
                   (summary-list
                    (mapcar (lambda (rec)
                              (let* ((entry-marker (make-marker)))
                                (set-marker entry-marker
                                            (byte-to-position (rec-record-position rec)))
                                (list entry-marker
                                      (vconcat
                                       (cl-loop for field in summary-fields
                                                for value = (string-join (rec-record-assoc field rec) ",")
                                                collect (or value ""))))))
                            (rec--parse-sexp-records query))))
              ;; Create the summary window if it does not exist and populate
              ;; it.
              (let ((rec-buf (current-buffer))
                    (rec-summary-inhibit-sync t)
                    (selection rec-selection-current-selection)
                    (buf (get-buffer-create summary-buffer-name)))
                (setq rec-summary-buffer buf)
                (when rec-selection-mode
                  (put 'rec-summary-buffer 'selection (rec-selection-stringify rec-selection-current-selection)))
                (when rec-summary-deletes-other-windows
                  (delete-other-windows))
                ;; If there's just one window, split the buffer.
                (let ((split-width-threshold nil)) ;; Forbid horiz split.
                  (if (and (one-window-p)
                           pop-up-windows
                           (not pop-up-frames))
                      (progn

                        (split-window (selected-window) rec-summary-window-height)
                        (select-window (next-window (frame-first-window)))
                        (pop-to-buffer buf)
                        (if (not (eq buf (window-buffer (frame-first-window))))
                                 (delete-other-windows)))
                    (pop-to-buffer buf))
                  (set-buffer rec-buf)
                  (rec-select-summary nil)
                  (set-buffer buf))
                (let ((inhibit-read-only t)
                      (rec-summary-buffer buf))
                  (delete-region (point-min) (point-max))
                  (rec-summary-mode)
                  (setq rec-summary-selection selection)
                  (setq rec-summary-rec-buffer rec-buf)
                  (rec-summary-populate (vconcat (mapcar (lambda (field) (list field 15 nil)) summary-fields)) summary-list)
                  (rec-summary-move-to-record current-record))))
          (message "No fields to build the summary."))))))

;;;; Record movement

(defun rec-goto-position (pos &optional skip-summary)
  "Move to POS, and highlight the record there.

Optional argument SKIP-SUMMARY means do not sync buffer
positions, this is useful when the callee of this function is the
summary buffer."
  (if (derived-mode-p 'rec-edit-mode)
      (progn
        (goto-char pos)
        (pulse-momentary-highlight-region (rec-beginning-of-record-pos)
                                          (rec-end-of-record-pos)
                                          'next-error)
        (rec-update-mode-line))
    (progn
      (widen)
      (goto-char pos)
      (rec-show-record)))
  (unless skip-summary
    (rec-summary-move-to-record (rec-current-record))))

(defun rec-goto-record (record)
  "Go to the position of RECORD.

The record is assumed to have its position in bytes, not
characters."
  (rec-goto-position (rec-record-position record)))


;;;; Interacting with other modes

(defun rec-log-current-defun ()
  "Return the value of the key in the current record, if any.

If no key is defined then return the value of the first field in
the record.  In case the pointer is not in a record then this
function returns nil."
  (let ((record (rec-current-record))
        (key (rec-key)))
    (when record
      (if key
          (let ((values (rec-record-assoc key record)))
            (if values
                (car values)
              (rec-field-value (car (rec-record-fields record)))))
        (rec-field-value (car (rec-record-fields record)))))))


;;;; Flymake support

;; Tell the byte compiler this function is available.  This is new in Emacs 26,
;; so as long as we support Emacs 25 this will remain.  Note that the hook
;; `flymake-diagnostic-functions' is not in Flymake of Emacs 25, so this
;; function never gets called on Emacs 25.  So Flymake is not going to work on
;; Emacs <26, but the alternative would be to explicitly Package-Require a new,
;; backported version of Flymake from ELPA, but we would like to limit these to
;; a minimum.
(declare-function flymake-make-diagnostic "flymake")

(defvar-local rec-mode--recfix-process nil
  "Buffer-local process for recfix'ing the buffer.")

(defun rec-mode-flymake-recfix-diagnostics (source-buffer output-buffer)
  "Read diagnostics for SOURCE-BUFFER using the results in OUTPUT-BUFFER."
  (with-current-buffer source-buffer
    (save-restriction
      (set-buffer source-buffer)
      (widen)
      (with-current-buffer output-buffer
        (goto-char (point-min))
        (cl-loop
         while (search-forward-regexp
                "^.*?:\s*\\([0-9]+\\): .*?: \\(.*\\)$"
                nil
                t)
         for (beg . end) = (flymake-diag-region
                            source-buffer
                            (string-to-number (match-string 1))
                            nil)
         for msg = (match-string 2)
         collect (flymake-make-diagnostic source-buffer beg end :error msg))))))

;;;###autoload
(defun rec-mode-flymake-recfix (report-fn &rest _args)
  "A Flymake backend for recfile compilation.

Defers to `recfix' for checking the buffer, calling REPORT-FN
to report the errors."
  (when (executable-find rec-recfix)
    (when (process-live-p rec-mode--recfix-process)
      (kill-process rec-mode--recfix-process))
    (let ((source-buffer (current-buffer))
          (output-buffer (generate-new-buffer " *flymake-recfix*")))
      (setq
       rec-mode--recfix-process
       (make-process
        :name "rec-mode-flymake-recfix"
        :stderr output-buffer
        :buffer "*stdout of flymake-recfix*"
        :command (list rec-recfix)
        :connection-type 'pipe
        :sentinel
        (lambda (proc _event)
          (when (eq (process-status proc) 'exit)
            (unwind-protect
                (let ((diagnostics (rec-mode-flymake-recfix-diagnostics source-buffer output-buffer)))
                  (if (or diagnostics (zerop (process-exit-status proc)))
                      (funcall report-fn diagnostics)
                    (funcall report-fn
                             :panic
                             :explanation
                             (format "recfix process %s died" proc))))
              (kill-buffer output-buffer))))
        :noquery t))
      (save-restriction
        (widen)
        (process-send-string rec-mode--recfix-process
                             (buffer-substring-no-properties (point-min) (point-max)))
        (process-send-eof rec-mode--recfix-process)))))

;;;###autoload
(defun rec-mode-eldoc-function (&rest _ignore)
  "ElDoc documentation function for `rec-mode'."
  (when (rec-current-field)
    (let* ((name (rec-field-name (rec-current-field)))
           (field-type (rec-current-field-type))
           (type (rec-type-kind field-type))
           (data (rec-type-data field-type)))
      (when (and (not (null name))
                 (not (null type)))
        (rec-mode--syntax-highlight
         (format "%s: %s %s"
                 name
                 (symbol-name type)
                 (if (listp data)
                     (mapconcat (lambda (datum)
                                  (if (stringp datum)
                                      datum
                                    (prin1-to-string datum))) data " ")
                   data)))))))

;;;; Imenu support

(defun rec-mode-imenu-index-name-function ()
  "Get an imenu index entry for the record at point."
  (let ((type (rec-record-type))
        (current (rec-current-record)))
    (if type
        (cond ((rec-record-descriptor-p current)
              (propertize (format "%%%s" type) 'face 'font-lock-keyword-face))
              ((not (null (rec-key)))
               (let ((key-value (car-safe (rec-record-assoc
                                           (rec-key)
                                           current))))
                 (format "%s %s" type (or key-value ""))))
              (t type))
      "Record")))

(defun rec-mode-imenu-goto-function (name position &rest rest)
  "Move to the chosen record.

Calls function `imenu-default-goto-function' (which see), with
NAME, POSITION and REST, but unlike that function, re-narrows
onto the chosen record."
  (apply 'imenu-default-goto-function name position rest)
  (unless (derived-mode-p 'rec-edit-mode)
    (rec-show-record))
  (rec-summary-move-to-record (rec-current-record)))

;;;; Definition of modes

(defvar font-lock-defaults)
(defvar add-log-current-defun-section)


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rec\\'" . rec-mode))

(easy-menu-define rec-mode-menu (list rec-mode-map rec-edit-mode-map)
  "Menu for rec-mode."
  '("Rec"
    ["Jump back"               rec-cmd-jump-back rec-jump-back]
    ["Next record"             rec-cmd-goto-next-rec
     :help "Go to the next record of the same type."]
    ["Previous record"         rec-cmd-goto-previous-rec
     :help "Go to the previous record of the same type."]
    ["Next field"              rec-cmd-goto-next-field t]
    ["Go to record descriptor" rec-cmd-show-descriptor t]
    "---"
    ["Append field"       rec-cmd-append-field t]
    ["Toggle field visibility" rec-cmd-toggle-field-visibility t]
    ["Trim field value"   rec-cmd-trim-field-value t]

    "---"
    ("Restrict navigation"
     ["Matching selection expression..." rec-cmd-navigate-current-type-by-sex
      :help "Restrict movement to records of the current type matching a selection expression."]
     ["Matching fast string search..." rec-cmd-navigate-current-type-by-fast-string
      :help "Restrict movement to records of the current type matching an fast string search."]
     "---"
     ["Exit selection navigation" rec-cmd-exit-selection rec-selection-mode])
    ("Select records"
     ["New buffer from selection expression..." rec-cmd-new-buffer-from-sex
      :help "Run a selection expression on the file and copy the results to a new buffer."]
     ["New buffer from fast string search..." rec-cmd-new-buffer-from-fast-string
      :help "Run a fast string search on the file and copy the results to a new buffer."])
    ("Cross reference"

     ["For selection expression..." rec-cmd-xref-sex
      :help "Run a selection expression on the buffer and make an XREF list out of it."]
     ["For fast string search..." rec-cmd-xref-fast-string
      :help "Run a fast string search and copy the matching lines into a new buffer."])

    "---"
    ["Edit field"         rec-cmd-edit-field t]
    ["Edit record"        rec-edit-record  (not (derived-mode-p 'rec-edit-mode))]
    ["Edit type"          rec-edit-type  (not (derived-mode-p 'rec-edit-mode))]
    ["Edit buffer"        rec-edit-buffer (not (derived-mode-p 'rec-edit-mode))]
    "---"
    ["Show info about file" rec-cmd-show-info t]
    ["Show field type"    rec-cmd-show-type t]
    ["Show field value statistics" rec-cmd-statistic t]
    "---"
    ["Count records of same type" rec-cmd-count t]
    "---"
    ["Show summary"       rec-cmd-show-summary t]
    ["Compile (recfix)"   rec-cmd-compile t]
    ["Find type..."       rec-find-type t]
    ["---"                nil                :visible (derived-mode-p 'rec-edit-mode)]
    ["Finish editing"     rec-finish-editing :visible (derived-mode-p 'rec-edit-mode)]))

;;;###autoload
(define-derived-mode rec-mode nil "Rec"
  "A major mode for editing rec files.
\\{rec-mode-map}"
  :syntax-table rec-mode-syntax-table
  (widen)
  ;; Local variables
  (setq-local add-log-current-defun-function #'rec-log-current-defun)
  (setq-local font-lock-defaults '(rec-font-lock-keywords))
  (setq-local syntax-propertize-function rec-syntax-propertize-function)
  (setq-local beginning-of-defun-function #'rec-beginning-of-record)
  (setq-local end-of-defun-function #'rec-end-of-record)
  (add-to-invisibility-spec '(rec-hide-field . "..."))

  (setq-local xref-prompt-for-identifier nil)
  (add-hook 'xref-after-jump-hook #'rec-mode--xref-after-jump-hook nil t)
  (add-hook 'xref-after-return-hook #'rec-mode--xref-widen-before-return nil t)
  (add-hook 'xref-backend-functions #'rec-mode--xref-backend nil t)

  ;; Run some code later (i.e. after running the mode hook and setting the
  ;; file-local variables).
  (add-hook 'hack-local-variables-hook #'rec--after-major-mode nil t)
  (setq-local eldoc-documentation-function #'rec-mode-eldoc-function)
  (setq-local imenu-prev-index-position-function #'rec-goto-previous-rec)
  (setq-local imenu-extract-index-name-function #'rec-mode-imenu-index-name-function)
  (setq-local imenu-default-goto-function #'rec-mode-imenu-goto-function)


  (add-hook 'flymake-diagnostic-functions #'rec-mode-flymake-recfix nil t))

(defun rec--after-major-mode ()
  "Goto the first record of the first type (including the Unknown).
If there is a problem (i.e.  syntax error) then go to fundamental
mode and show the output of recfix in a separated buffer."
  (when (rec-update-buffer-descriptors-and-check)
    ;; If the configured open-mode is navigation, set up the buffer
    ;; accordingly.  But don't go into navigation mode if the file is
    ;; empty.
    (if (and (equal rec-open-mode 'navigation)
             (> (buffer-size (current-buffer)) 0))
        (progn
          (setq buffer-read-only t)
          (rec-show-type (car (rec-buffer-types))))
      ;; Edit mode
      (rec-edit-mode)
      (setq rec-edit-mode-type 'buffer)
      (rec-update-mode-line))))

(define-derived-mode rec-edit-mode rec-mode "Rec Edit"
  "A major mode for editing recfiles in Edit Mode.

See Info node `(rec-mode)Editing Mode'.
\\{rec-edit-mode-map}"
  (rec-unfold-all-fields)
  (rec-remove-continuation-line-marker-overlays)
  (setq buffer-read-only nil)
  (remove-hook 'hack-local-variables-hook #'rec--after-major-mode t)
  (add-hook 'hack-local-variables-hook #'rec-update-buffer-descriptors-and-check nil t))

(defvar rec-edit-field-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'rec-finish-editing-field)
    map)
  "Keymap for `rec-edit-field-mode'.")

(define-derived-mode rec-edit-field-mode nil "Rec Edit"
  "A major mode for editing rec field values.")

(defvar rec-selection-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap rec-cmd-goto-next-rec] #'rec-cmd-goto-next-selection-record)
    (define-key map [remap rec-cmd-goto-previous-rec] #'rec-cmd-goto-prev-selection-record)
    map))

;;;###autoload
(define-minor-mode rec-selection-mode
  "A minor mode for navigating a selection of the current buffer.

When a selection is entered via `\\[rec-cmd-filter-sex]', the
minor mode is entered.  This minor mode alters the behaviour of
the standard bindings of `rec-cmd-goto-next-rec' and
`rec-cmd-goto-previous-rec'.  In the minor mode, only the records
matching the currently active selection are available for
navigation.  The minor mode can be exited using
`rec-selection-exit', bound to `\\[rec-cmd-exit-selection]'.

\\{rec-selection-mode-map}."
  :lighter " Selection"
  :variable rec-selection-mode
  :keymap rec-selection-mode-map)

;; Local variables:
;; outline-regexp: ";;;;"
;; End:

(provide 'rec-mode)
;;; rec-mode.el ends here
