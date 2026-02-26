;;; consult-hoogle.el --- Hoogle frontend using consult -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

;; Author: rahguzar <rahguzar@mailbox.org>
;; Maintainer: rahguzar <rahguzar@mailbox.org>
;; Created: April 10, 2022
;; License: GPL-3.0-or-later
;; Version: 0.6.0
;; Keywords: docs languages
;; Homepage: https://codeberg.org/rahguzar/consult-hoogle
;; Package-Requires: ((emacs "27.1") (consult "2.0"))

;; This file is part of GNU Emacs.

;;; Commentary:

;; Search the local hoogle database from Emacs using the nicities provided by
;; consult.

;;; Code:

;;;; Packages
(require 'consult)
(require 'hoogle-base)

(declare-function hoogle-buffer "hoogle-buffer")

;;;; Variables
(defgroup consult-hoogle nil
  "A frontend for hoogle."
  :group 'consult)

(define-obsolete-variable-alias 'consult-hoogle-args 'hoogle-base-args "0.2.0")
(define-obsolete-variable-alias 'consult-hoogle-project-args 'hoogle-base-project-args "0.2.0")

(defcustom consult-hoogle-show-module-and-package t
  "Whether to show the package and module in the candidate line."
  :type 'boolean
  :group 'consult-hoogle)

(defvar consult-hoogle--history nil
  "Variable to store history for hoogle searches.")

(defvar consult-hoogle-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map hoogle-base-map)
    (define-key map (kbd "M-<up>") #'consult-hoogle-scroll-docs-down)
    (define-key map (kbd "M-<down>") #'consult-hoogle-scroll-docs-up)
    (define-key map (kbd "C-e") #'consult-hoogle-export-to-buffer)
    map))

;;;; Constructing the string to display
(defun consult-hoogle--fontify (text)
  "Fontify TEXT, returning the fontified text.
This is adapted from `haskell-fontify-as-mode' but for better performance
we use the same buffer throughout."
  (with-current-buffer " *Hoogle Fontification*"
    (erase-buffer)
    (insert text)
    (font-lock-ensure)
    (buffer-substring (point-min) (point-max))))

(defun consult-hoogle--format-value (item in module from package)
  "Construct the disaply string from ITEM IN MODULE FROM and PACKAGE."
  (if (not consult-hoogle-show-module-and-package)
      item
    (concat item from module in package)))

(defun consult-hoogle--format-result (json)
  "Parse the JSON resturned by hoogle to construct a result."
  (when-let ((parsed (ignore-errors (json-parse-string json :object-type 'alist))))
    (let* ((in (propertize " in " 'face 'font-lock-comment-face))
           (from (propertize " from " 'face 'font-lock-comment-face))
           (module (cl-callf propertize
                       (alist-get 'name (alist-get 'module parsed) "")
                     'face 'font-lock-keyword-face))
           (package (cl-callf propertize
                        (alist-get 'name (alist-get 'package parsed) "")
                      'face 'font-lock-preprocessor-face)))
      (propertize
       (pcase (alist-get 'type parsed)
         (""  (consult-hoogle--format-value
               (cl-callf consult-hoogle--fontify (alist-get 'item parsed))
               from module in package))
         ("module" (concat "Module "
                           (cl-callf hoogle-base--name (alist-get 'item parsed)
                             'font-lock-keyword-face)
                           in package))
         ("package" (concat "Package "
                            (cl-callf hoogle-base--name (alist-get 'item parsed)
                              'font-lock-preprocessor-face))))
       'consult--candidate parsed))))

(defun consult-hoogle--format (lines)
  "Format candidates using LINES."
  (let (candidates)
    (dolist (line lines)
      (push (consult-hoogle--format-result line) candidates))
    (nreverse (delq nil candidates))))

;;;; Constructing the details buffer for the selected result
(defun consult-hoogle--show-details (action cand)
  "Show the details for the current CAND and handle ACTION."
  (let ((buf (get-buffer-create " *Hoogle Documentation*" t))
        (inhibit-read-only t)
        (inhibit-message t))
    (with-current-buffer buf
      (erase-buffer)
      (pcase action
        ('preview (when cand
                    (hoogle-base--details cand)
                    (goto-char (point-min))))
        ('setup (visual-line-mode)
                (read-only-mode)
                (display-buffer buf `( display-buffer-in-side-window
                                       (window-height . 16)
                                       (side . bottom)
                                       (slot . -1))))
        ('return (when-let ((win (get-buffer-window buf)))
                   (delete-window win))
                 (kill-buffer buf))))))

;;;; Refining searches
(defun consult-hoogle--async-input ()
  "Return the async part of the input."
  (let* ((style (alist-get consult-async-split-style consult-async-split-styles-alist))
         (input (substring-no-properties (minibuffer-contents)))
         (initial (when (plist-get style :initial)
                    (string-match (rx bos (group (opt punct))) input)
                    (match-string 1 input)))
         (separator (or (plist-get style :separator) initial))
         (async-rx (rx-to-string `(: ,(or initial "") (group (0+ (not ,separator)))))))
    (goto-char (minibuffer-prompt-end))
    (when (looking-at async-rx)
      (substring-no-properties (match-string 1)))))

(defun consult-hoogle--modify-async-input (fun)
  "Change async part of input to (funcall FUN async-input)."
  (save-excursion
    (when-let ((input (consult-hoogle--async-input)))
      (replace-match (string-trim (funcall fun input)) nil t nil 1))))

;;;; Consult integration
(defun consult-hoogle--candidate ()
  "Get the current candidate."
  (when-let ((candidate (run-hook-with-args-until-success
                         'consult--completion-candidate-hook)))
    (get-text-property 0 'consult--candidate candidate)))

(defun consult-hoogle--source (&optional command)
  "Return an async source to search with hoogle using COMMAND."
  (let* ((command (or command hoogle-base-args))
         (exe (or (executable-find (car command))
                  (error "Executable %s not found on path" (car command))))
         (command `(,exe . ,(cdr command))))
    (consult--process-collection
        (lambda (input)
          (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
            (unless (string-blank-p arg)
              (cons (append command opts (list arg))
                    (cdr (consult--default-regexp-compiler input 'basic t))))))
      :transform (consult--async-transform #'consult-hoogle--format)
      :highlight t)))

(defun consult-hoogle--search (&optional command state action)
  "Search local hoogle database using COMMAND and take ACTION with the selection.
STATE is the optional state function passed to the `consult--read'."
  (let ((consult-async-min-input 0)
        (hoogle-base-find-candidate #'consult-hoogle--candidate)
        (hoogle-base-modify-query-function #'consult-hoogle--modify-async-input)
        (source (consult-hoogle--source command))
        (fun (or action (lambda (alist) (hoogle-base--browse-url 'item alist)))))
    (with-current-buffer (get-buffer-create " *Hoogle Fontification*" t)
      (setq-local delay-mode-hooks t)
      (hoogle-base--haskell-mode))
    (unwind-protect
        (funcall fun (consult--read
                      source
                      :prompt "Hoogle: "
                      :require-match t
                      :lookup #'consult--lookup-candidate
                      :state state
                      :sort nil
                      :keymap consult-hoogle-map
                      :add-history (or (and (fboundp 'haskell-ident-at-point)
                                            (haskell-ident-at-point))
                                       (thing-at-point 'symbol))
                      :category 'consult-hoogle
                      :history '(:input consult-hoogle--history)))
      (when-let ((buf (get-buffer " *Hoogle Fontification*")))
        (kill-buffer buf)))))

;;;; Interactive Commands
;;;###autoload
(defun consult-hoogle (arg &optional command)
  "Search the local hoogle database with COMMAND (by default `hoogle-base-args').
By default this shows the documentation for the current candidate in a side
window.  This can be disabled by a prefix ARG."
  (interactive (list current-prefix-arg))
  (if arg (consult-hoogle--search command)
    (consult-hoogle--search command #'consult-hoogle--show-details)))

;;;###autoload
(defun consult-hoogle-project (arg)
  "Search the local hoogle database for current project.
By default uses cabal-hoogle and the database should have been generated
by running `cabal-hoogle generate'.  `hoogle-base-project-args' can be
customized to configure an alternate command.
By default this shows the documentation for the current candidate in a side
window.  This can be disabled by a prefix ARG."
  (interactive (list current-prefix-arg))
  (let ((default-directory (funcall hoogle-base-project-root-function)))
    (consult-hoogle arg hoogle-base-project-args)))

(defun consult-hoogle-scroll-docs-down (&optional arg)
  "Scroll the window with documentation ARG lines down."
  (interactive)
  (with-selected-window (get-buffer-window " *Hoogle Documentation*")
    (scroll-down arg)))

(defun consult-hoogle-scroll-docs-up (&optional arg)
  "Scroll the window with documentation ARG lines down."
  (interactive)
  (with-selected-window (get-buffer-window " *Hoogle Documentation*")
    (scroll-up arg)))

(defun consult-hoogle-export-to-buffer ()
  "Open a buffer containing results for the async part of current search."
  (interactive)
  (let ((input (consult-hoogle--async-input))
        (args hoogle-base-args)
        (dir default-directory))
    (run-at-time
     0 nil
     (lambda ()
       (let ((hoogle-base-args args)
             (default-directory dir))
         (hoogle-buffer input (get-buffer-create "*hoogle-search*")))))
    (abort-recursive-edit)))

(provide 'consult-hoogle)
;;; consult-hoogle.el ends here
