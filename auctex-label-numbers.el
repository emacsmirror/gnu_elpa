;;; auctex-label-numbers.el --- Numbering for LaTeX previews and folds  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.2
;; URL: https://github.com/ultronozm/auctex-label-numbers.el
;; Package-Requires: ((emacs "27.1") (auctex "14.0.5"))
;; Keywords: tex

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

;; This package provides a function,
;; `auctex-label-numbers-label-to-number', that retrieves label
;; numbers for LaTeX documents from the compiled aux file.  That
;; function is used to implement a global minor mode,
;; `auctex-label-numbers-mode', that augments many features of AUCTeX
;; (previews, folding, ...).  See README.org for details.

;;; Code:

(require 'tex)
(require 'latex)
(require 'tex-fold)
(require 'preview)
(require 'reftex)

(defgroup auctex-label-numbers nil
  "Numbering for LaTeX previews and folds."
  :group 'AUCTeX)

(defvar auctex-label-numbers-cache (make-hash-table :test 'equal)
  "Cache of label numbers from aux files.
The keys are aux file names.  The values are hash tables, mapping label
strings to label number strings.")

(defconst auctex-label-numbers--newlabel-regexp
  "\\\\newlabel{\\([^}]+\\)}{{\\([^}]+\\)}"
  "Regexp for \\newlabel commands, capturing label and number.")

(defun auctex-label-numbers-update-cache (aux-file)
  "Update the cache for AUX-FILE.
Return the updated cache, or nil if the aux file does not exist."
  (when (file-exists-p aux-file)
    (with-temp-buffer
      (insert-file-contents aux-file)
      (let ((cache (make-hash-table :test 'equal)))
        (goto-char (point-min))
        (while (re-search-forward
                auctex-label-numbers--newlabel-regexp nil t)
          (let ((label (match-string 1))
                (number (match-string 2)))
            (puthash label number cache)))
        (puthash 'timestamp (current-time) cache)
        (puthash aux-file cache auctex-label-numbers-cache)
        cache))))

(defcustom auctex-label-numbers-label-to-number-function nil
  "Function to retrieve label numbers.
If non-nil, `auctex-label-numbers-label-to-number' delegates to this
function.  The function should take a label string as its argument and
return the corresponding label number as a string, or nil if that number
cannot be retrieved."
  :type '(choice (const :tag "Default" nil)
                 (function :tag "Custom function")))

;; FIXME: we don't properly handle the optional arguments to
;; \externaldocument, i.e., prefixes for labels.

(defun auctex-label-numbers-label-to-number-helper (label aux-file)
  "Get the number of LABEL from the AUX-FILE.
Check the cache first, and update it if AUX-FILE has changed.  Return
the label number as a string, or nil if the label cannot be found."
  (let ((cache (gethash aux-file auctex-label-numbers-cache)))
    (if (or (not cache)
            (time-less-p (gethash 'timestamp cache)
                         (nth 5 (file-attributes aux-file))))
        (setq cache (auctex-label-numbers-update-cache aux-file)))
    (when cache
      (gethash label cache))))

(defcustom auctex-label-numbers-search-external-documents t
  "Whether to search external documents for label numbers.
If non-nil, `auctex-label-numbers-label-to-number' will search external
documents for label numbers if the label cannot be found in the current
document.

The search is performed by looking for \\externaldocument{FILENAME}
commands in the current document, and then looking for the label in
FILENAME.tex.  This operation opens external TeX documents in unselected
buffers, so that it can use AUCTeX's function `TeX-master-output-file'
to find the corresponding aux files."
  :type '(choice (const :tag "Search external documents" t)
                 (const :tag "Don't search external documents" nil)))

(defconst auctex-label-numbers--external-document-regexp
  "\\\\external\\(?:cite\\)?document\\(?:\\[\\([^]]+\\)\\]\\)?{\\([^}]+\\)}"
  "Regexp for \\externaldocument commands.
Optional prefix is (match-string 1), filename is (match-string 2).")

(defun auctex-label-numbers-label-to-number (label)
  "Get number of LABEL for current tex buffer.
If the buffer does not point to a file, or if the corresponding aux file
does not exist, or if the label cannot be found, then return nil.
Otherwise, return the label number as a string.  If the label is found
in an external document, prefix the string with \"X\"."
  (if auctex-label-numbers-label-to-number-function
      (funcall auctex-label-numbers-label-to-number-function label)
    (or
     ;; Check main aux file
     (when-let* ((aux-file (TeX-master-output-file "aux")))
       (auctex-label-numbers-label-to-number-helper label aux-file))
     ;; Search external documents
     (and
      auctex-label-numbers-search-external-documents
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let (found)
            (while (and (null found)
                        (re-search-forward auctex-label-numbers--external-document-regexp
                                           nil t))
              (let* ((prefix (match-string 1))
                     (tex-filename (concat (match-string 2) ".tex"))
                     (tex-buffer (find-file-noselect tex-filename))
                     (aux-filename
                      (with-current-buffer tex-buffer
                        (hack-local-variables)
                        (TeX-master-output-file "aux"))))
                (when aux-filename
                  (let ((full-label (concat (or prefix "") label)))
                    (setq found (auctex-label-numbers-label-to-number-helper full-label aux-filename))))))
            (when found
              (concat "X" found)))))))))

(defun auctex-label-numbers-preview-preprocessor (str)
  "Preprocess STR for preview by adding tags to labels.
Uses `auctex-label-numbers-label-to-number-function' to retrieve label
numbers."
  (let ((buf (current-buffer))
        (label-re
         (concat "\\(?:" (mapconcat #'identity reftex-label-regexps "\\|") "\\)")))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward label-re nil t)
        (let ((label (match-string 1)))
          (if-let ((number
                    (with-current-buffer buf
                      (auctex-label-numbers-label-to-number label))))
              (when (let ((comment-start-skip
                           (concat
                            "\\(\\(^\\|[^\\\n]\\)\\("
                            (regexp-quote TeX-esc)
                            (regexp-quote TeX-esc)
                            "\\)*\\)\\(%+ *\\)")))
                      (texmathp))
                (insert (format "\\tag{%s}" number)))
            (insert "\\nonumber"))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun auctex-label-numbers-ref-helper (label default)
  "Helper function for `auctex-label-numbers-ref-display'.
Returns a fold display string for LABEL (retrieved via
`auctex-label-numbers-label-to-number-function'), or DEFAULT if the
label number cannot be retrieved."
  (format "[%s]" (or (auctex-label-numbers-label-to-number label) default)))

(defun auctex-label-numbers-ref-display (label &rest _args)
  "Fold display for a \\ref{LABEL} macro."
  (auctex-label-numbers-ref-helper label "r"))

(defun auctex-label-numbers-eqref-display (label &rest _args)
  "Fold display for a \\eqref{LABEL} macro."
  (auctex-label-numbers-ref-helper label "e"))

(defun auctex-label-numbers-label-display (label &rest _args)
  "Fold display for a \\label{LABEL} macro."
  (auctex-label-numbers-ref-helper label "l"))

(defvar auctex-label-numbers--saved-spec-list nil
  "Saved values from `TeX-fold-macro-spec-list'.")

(defcustom auctex-label-numbers-macro-list '("ref" "eqref" "label")
  "List of macros to fold with theorem or equation numbers.
Each element describes a LaTeX macro that takes a label as its argument.
There should be a corresponding function
`auctex-label-numbers-MACRO-display' that returns a fold display string
for that macro."
  :type '(repeat (string :tag "Macro name")))

(defun auctex-label-numbers-label-annotation-advice (orig-fun label)
  "Return context for LABEL, augmented by the corresponding label number.
Call ORIG-FUN with LABEL, and append the label number in parentheses if
it can be retrieved."
  (concat
   (funcall orig-fun label)
   (when-let ((number (auctex-label-numbers-label-to-number label)))
     (format " (%s)" number))))

(defun auctex-label-numbers--reftex-goto-label-advice (orig-fun &rest args)
  "Advice for `reftex-goto-label'.
Call ORIG-FUN with ARGS, and add the label number to the annotation."
  (let* ((buf (current-buffer))
         (fn (lambda (label)
               (if-let ((str (with-current-buffer buf
                               (funcall #'auctex-label-numbers-label-to-number label))))
                   (format " [%s]" str)
                 "")))
         (completion-extra-properties
          `(:annotation-function ,fn)))
    (apply orig-fun args)))

;;;###autoload
(define-minor-mode auctex-label-numbers-mode
  "Toggle `auctex-label-numbers' mode."
  :global t
  :lighter nil
  (cond
   (auctex-label-numbers-mode
    (add-to-list 'preview-preprocess-functions #'auctex-label-numbers-preview-preprocessor)
    (advice-add 'LaTeX-completion-label-annotation-function
                :around #'auctex-label-numbers-label-annotation-advice)
    (advice-add 'reftex-goto-label :around #'auctex-label-numbers--reftex-goto-label-advice)
    (require 'tex-fold)
    (dolist (macro auctex-label-numbers-macro-list)
      (let ((func (intern (format "auctex-label-numbers-%s-display" macro))))
        (dolist (spec TeX-fold-macro-spec-list)
          (when (and (member macro (cadr spec))
                     (not (eq (car spec) func)))
            (push (cons macro (car spec)) auctex-label-numbers--saved-spec-list)
            (setcdr spec (list
                          (seq-remove (lambda (x) (equal x macro)) (cadr spec))))))
        (add-to-list 'TeX-fold-macro-spec-list (list func (list macro)))))
    (when TeX-fold-mode
      (TeX-fold-mode 1)))
   (t
    (setq preview-preprocess-functions
          (delete #'auctex-label-numbers-preview-preprocessor preview-preprocess-functions))
    (advice-remove 'LaTeX-completion-label-annotation-function
                   #'auctex-label-numbers-label-annotation-advice)
    (advice-remove 'reftex-goto-label #'auctex-label-numbers--reftex-goto-label-advice)
    (dolist (macro auctex-label-numbers-macro-list)
      (let ((func (intern (format "auctex-label-numbers-%s-display" macro))))
        (setq TeX-fold-macro-spec-list
              (seq-remove (lambda (elem) (eq (car elem) func))
                          TeX-fold-macro-spec-list)))
      (when-let ((saved (assoc macro auctex-label-numbers--saved-spec-list)))
        (dolist (spec TeX-fold-macro-spec-list)
          (when (eq (car spec) (cdr saved))
            (push macro (cadr spec))))))
    (setq auctex-label-numbers--saved-spec-list nil)
    (when TeX-fold-mode
      (TeX-fold-mode 1)))))

(provide 'auctex-label-numbers)
;;; auctex-label-numbers.el ends here
