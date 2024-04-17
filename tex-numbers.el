;;; tex-numbers.el --- numbering for LaTeX previews and folds  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/tex-numbers.el
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

;; This package augments the preview and folding features of AUCTeX:
;;
;; - Previews of labeled equations are numbered as in the compiled
;;   document.
;;
;; - The the macros \\ref, \\eqref, and \\label are folded with the
;;   corresponding numbers.
;;
;; - completion-at-point annotations for the contents of \\ref and
;;   \\eqref include equation numbers.
;;
;; TEMPORARY NOTE: this package currently only works with the master
;; branch of AUCTeX.  Its release is intended to be synchronized with
;; the next release of AUCTeX.
;;
;; Activate via M-x tex-numbers-mode, or by adding to your init file:
;;
;; (use-package tex-numbers
;;   :hook
;;   (LaTeX-mode . tex-numbers-mode))
;;
;; The package provides an interface for retrieving label numbers in
;; LaTeX documents.  This interface is used to implement a global
;; minor mode, `tex-numbers-mode', which enables the noted features.
;;
;; The label numbers are retrieved from the aux file of the compiled
;; document.  To update them, one should compile the document,
;; regenerate the previews and refresh the folds.
;;
;; By customizing the variable `tex-numbers-label-to-number-function', one
;; can specify a different way to retrieve label numbers, e.g., by
;; querying an LSP server.

;;; Code:

(require 'tex)
(require 'latex)
(require 'tex-fold)
(require 'preview)
(require 'reftex)

(defgroup tex-numbers nil
  "Numbering for LaTeX previews and folds."
  :group 'AUCTeX)

(defvar tex-numbers-cache (make-hash-table :test 'equal)
  "Cache of label numbers from aux files.
The keys are aux file names.  The values are hash tables, mapping label
strings to label number strings.")

(defun tex-numbers-update-cache (aux-file)
  "Update the cache for AUX-FILE.
Return the updated cache, or nil if the aux file does not exist."
  (when (file-exists-p aux-file)
    (with-temp-buffer
      (insert-file-contents aux-file)
      (let ((cache (make-hash-table :test 'equal))
            (pattern "\\newlabel{\\([^}]+\\)}{{\\([^}]+\\)}"))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (let ((label (match-string 1))
                  (number (match-string 2)))
              (puthash label number cache))))
        (puthash 'timestamp (current-time) cache)
        (puthash aux-file cache tex-numbers-cache)
        cache))))

(defcustom tex-numbers-label-to-number-function nil
  "Function to retrieve label numbers.
If non-nil, `tex-numbers-label-to-number' delegates to this function.
The function should take a label string as its argument and return the
corresponding label number as a string, or nil if that number cannot be
retrieved."
  :type '(choice (const :tag "Default" nil) function))

(defconst tex-numbers--external-document-regexp
  "\\\\external\\(?:cite\\)?document\\(?:\\[[^]]+\\]\\)\\{0,2\\}{\\([^}]+\\)}")

(defun tex-numbers-label-to-number-helper (label aux-file)
  "Get the number of LABEL from the AUX-FILE.
Check the cache first, and update it if AUX-FILE has changed.  Return
the label number as a string, or nil if the label cannot be found."
  (let ((cache (gethash aux-file tex-numbers-cache)))
    (if (or (not cache)
            (time-less-p (gethash 'timestamp cache)
                         (nth 5 (file-attributes aux-file))))
        (setq cache (tex-numbers-update-cache aux-file)))
    (when cache
      (gethash label cache))))

(defun tex-numbers-label-to-number (label)
  "Get number of LABEL for current tex buffer.
If the buffer does not point to a file, or if the corresponding
aux file does not exist, or if the label cannot be found, then
return nil.  Otherwise, return the label number as a string.  If
the label is found in an external document, prefix the string
with \"X\"."
  (if tex-numbers-label-to-number-function
      (funcall tex-numbers-label-to-number-function label)
    (or
     (when-let* ((aux-file (TeX-master-file "aux")))
       (tex-numbers-label-to-number-helper label aux-file))
     ;; If we can't retrieve the label from the main file, then we look
     ;; at any external documents.
     (save-excursion
       (save-restriction
         (widen)
         (goto-char (point-min))
         (let (found)
           (while (and (null found)
                       (re-search-forward tex-numbers--external-document-regexp
                                          nil t))
             (let* ((filename (concat (match-string 1) ".aux")))
               (setq found (tex-numbers-label-to-number-helper label filename))))
           (when found
             (concat "X" found))))))))

(defun tex-numbers-preview-preprocessor (str)
  "Preprocess STR for preview by adding tags to labels.
Uses `tex-numbers-label-to-number-function' to retrieve label numbers."
  (let ((buf (current-buffer))
        (label-re
         (concat "\\(?:" (mapconcat #'identity reftex-label-regexps "\\|") "\\)")))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward label-re nil t)
        (let ((label (match-string 1)))
          (when-let ((number
                      (with-current-buffer buf
                        (tex-numbers-label-to-number label))))
            (when (let ((comment-start-skip
                         (concat
                          "\\(\\(^\\|[^\\\n]\\)\\("
                          (regexp-quote TeX-esc)
                          (regexp-quote TeX-esc)
                          "\\)*\\)\\(%+ *\\)")))
                    (texmathp))
              (insert (format "\\tag{%s}" number))))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun tex-numbers-ref-helper (label default)
  "Helper function for `tex-numbers-ref-display'.
Returns a fold display string for LABEL (retrieved via
`tex-numbers-label-to-number-function'), or DEFAULT if the label number cannot
be retrieved."
  (format "[%s]" (or (tex-numbers-label-to-number label) default)))

(defun tex-numbers-ref-display (label &rest _args)
  "Fold display for a \\ref{LABEL} macro."
  (tex-numbers-ref-helper label "r"))

(defun tex-numbers-eqref-display (label &rest _args)
  "Fold display for a \\eqref{LABEL} macro."
  (tex-numbers-ref-helper label "e"))

(defun tex-numbers-label-display (label &rest _args)
  "Fold display for a \\label{LABEL} macro."
  (tex-numbers-ref-helper label "l"))

(defvar tex-numbers--saved-spec-list nil
  "Saved values from `TeX-fold-macro-spec-list'.")

(defcustom tex-numbers-macro-list '("ref" "eqref" "label")
  "List of macros to fold with theorem or equation numbers.
Each element describes a LaTeX macro that takes a label as its argument.
There should be a corresponding function `tex-numbers-MACRO-display'
that returns a fold display string for that macro."
  :type '(repeat string))

(defun tex-numbers-label-annotation-advice (orig-fun label)
  "Return context for LABEL, augmented by the corresponding label number."
  (concat
   (funcall orig-fun label)
   (when-let ((number (tex-numbers-label-to-number label)))
     (format " (%s)" number))))

;;;###autoload
(define-minor-mode tex-numbers-mode
  "Toggle `tex-numbers' mode."
  :global t
  :lighter nil
  (cond
   (tex-numbers-mode
    (setq preview-preprocess-function #'tex-numbers-preview-preprocessor)
    (advice-add 'LaTeX-completion-label-annotation-function
                :around #'tex-numbers-label-annotation-advice)
    (require 'tex-fold)
    (dolist (macro tex-numbers-macro-list)
      (let ((func (intern (format "tex-numbers-%s-display" macro))))
        (dolist (spec TeX-fold-macro-spec-list)
          (when (and (member macro (cadr spec))
                     (not (eq (car spec) func)))
            (push (cons macro (car spec)) tex-numbers--saved-spec-list)
            (setcdr spec (list (cl-remove macro (cadr spec) :test 'equal)))))
        (add-to-list 'TeX-fold-macro-spec-list (list func (list macro)))))
    (when TeX-fold-mode
      (TeX-fold-mode 1)))
   (t
    (setq preview-preprocess-function nil)
    (advice-remove 'LaTeX-completion-label-annotation-function
                   #'tex-numbers-label-annotation-advice)
    (dolist (macro tex-numbers-macro-list)
      (let ((func (intern (format "tex-numbers-%s-display" macro))))
        (setq TeX-fold-macro-spec-list
              (cl-remove-if (lambda (elem) (eq (car elem) func))
                            TeX-fold-macro-spec-list)))
      (when-let ((saved (assoc macro tex-numbers--saved-spec-list)))
        (dolist (spec TeX-fold-macro-spec-list)
          (when (eq (car spec) (cdr saved))
            (push macro (cadr spec))))))
    (setq tex-numbers--saved-spec-list nil)
    (when TeX-fold-mode
      (TeX-fold-mode 1)))))

(provide 'tex-numbers)
;;; tex-numbers.el ends here
