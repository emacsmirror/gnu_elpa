;;; hoogle-base.el --- Basic interaction with hoogle -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: rahguzar <rahguzar@mailbox.org>
;; Maintainer: rahguzar <rahguzar@mailbox.org>
;; Created: April 10, 2022
;; License: GPL-3.0-or-later
;; Version: 0.6.0
;; Keywords: docs languages
;; Homepage: https://codeberg.org/rahguzar/consult-hoogle
;; Package-Requires: ((emacs "28.1"))

;; This file is part of GNU Emacs.

;;; Commentary:

;; This files contains common infrastructure used by both `hoogle-buffer'
;; and `hoogle-base'.

;;; Code:
;;;; Packages
(require 'shr)

;;;; Customization Options
(defgroup hoogle-base nil
  "A frontend for hoogle."
  :group 'haskell)

(defcustom hoogle-base-args
  '("hoogle" . ("search" "--jsonl" "-q" "--count=250"))
  "The hoogle invocation used to get results.
It is should be a cons (COMMAND . ARGS).  COMMAND should be valid executable.
It is called arguments ARGS with the search query appended.  It should produce
search results in JSON lines format."
  :type '(cons (string :tag "Hoogle command")
               (repeat :tag "Args for hoogle" string)))

(defcustom hoogle-base-project-args
  '("cabal-hoogle" . ("run" "--" "search" "--jsonl" "-q" "--count=250"))
  "The cabal-hoogle invocation used to get results for current project.
It should be cons (COMMAND . ARGS). See `hoogle-base-args' for details.  By
default it uses `cabal-hoogle' https://github.com/kokobd/cabal-hoogle ."
  :type '(cons (string :tag "Project specific hoogle command")
               (repeat :tag "Args for hoogle" string)))

(defcustom hoogle-base-project-root-function #'hoogle-base-locate-cabal-file
  "Function to find the `default-directory' for project specific searches.
It is called without any arguments by `consult-hoogle-project' and
`hoogle-buffer-project'."
  :type '(choice (const :tag "Directory with cabal file"
                        hoogle-base-locate-cabal-file)
                 (other :tag "Other function" function)))

;;;; Variables
(defvar hoogle-base-find-candidate nil
  "Function of no arguments to find current candidate.")

(defvar hoogle-base-modify-query-function nil
  "Function to modify current query.
It is called with one argument which a function to modify the cuurent query.
It should run the search with the new query.")

(defvar hoogle-base-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-i") #'hoogle-base-browse-item)
    (define-key map (kbd "M-j") #'hoogle-base-browse-package)
    (define-key map (kbd "M-m") #'hoogle-base-browse-module)
    (define-key map (kbd "TAB p") #'hoogle-base-restrict-to-package)
    (define-key map (kbd "TAB m") #'hoogle-base-restrict-to-module)
    (define-key map (kbd "TAB b") #'hoogle-base-restrict-to-module-level-beg)
    (define-key map (kbd "TAB e") #'hoogle-base-restrict-to-module-level-end)
    (define-key map (kbd "TAB c") #'hoogle-base-clear-restrictions)
    map))

;;;; Utility Functions
(defun hoogle-base--haskell-mode ()
  "Run the major-mode suitable for haskell."
  (if-let ((mode (assoc-default ".hs" auto-mode-alist #'string-match-p)))
      (funcall mode)
    (display-warning
     'hoogle-base
     "No major-mode for haskell is configured. There will be no syntax hightlighting."
     :warning)))

(defun hoogle-base--candidate ()
  "Get the current candidate."
  (if (functionp hoogle-base-find-candidate)
      (or (funcall hoogle-base-find-candidate)
          (error "No hoogle candidate found"))
    (error "The variable %S is not set properly" 'hoogle-base-find-candidate)))

(defun hoogle-base--name (item &optional face)
  "Return name of ITEM with FACE."
  (propertize (or (cadr (split-string item nil t " +")) item) 'face face))

(defun hoogle-base--doc-line (label elem item)
  "Construct a line for doc buffer from LABEL ELEM and ITEM."
  (concat (propertize label 'face 'bold)
          (if (and elem (not (equal elem "")))
              elem
            (hoogle-base--name item))
          "\n"))

(defun hoogle-base--details (alist)
  "Construct the details from ALIST."
  (let-alist alist
    (let* ((package-line (hoogle-base--doc-line
                          "Package: " .package.name .item))
           (module-line (unless (equal "package" .type)
                          (hoogle-base--doc-line
                           "Module: " .module.name .item)))
           (item-line (when (equal .type "")
                        (concat (propertize .item 'hoogle-code t) "\n"))))
      (insert (concat item-line module-line package-line) "\n"))
    (let ((beg (point)))
      (insert .docs)
      (shr-render-region beg (point)))))

(defun hoogle-base-locate-cabal-file ()
  "Return the parent directory of `default-directory' containing a cabal file."
  (if-let* ((dir (locate-dominating-file
                  default-directory
                  (lambda (dir) (directory-files dir nil (rx ".cabal" eos))))))
      dir
    (user-error "Cabal file not found in any parent of %s" default-directory)))

;;;; Following the urls from hoogle results.
(defun hoogle-base--browse-url (type &optional alist)
  "Open the url of TYPE from ALIST."
  (let-alist alist
    (if-let ((type-url (pcase .type
                         ("" type)
                         ("module" (if (eq type 'module) 'item type))
                         ("package" (if (eq type 'package) 'item type))))
             (url (if (eq 'item type-url)
                      .url
                    (alist-get 'url (alist-get type-url alist)))))
        (progn (if (and (or (eq type 'package)
                            (equal .type "package"))
                        (url-file-host-is-local-p
                         (url-host (url-generic-parse-url url))))
                   (browse-url (concat url "index.html"))
                 (browse-url url))
               (when (minibufferp nil t)
                (abort-recursive-edit)))
      (message "No suitable url for current candidate."))))

;;;; Refining Searches
(defun hoogle-base--add-to-input (&rest addition)
  "Add ADDITION to the async part of the input."
  (funcall hoogle-base-modify-query-function
           (lambda (match) (apply #'concat match " " addition))))

(defun hoogle-base--get (key &optional alist)
  "Return the value for KEY from the ALIST."
  (let ((alist (or alist (hoogle-base--candidate))))
    (let-alist alist
      (pcase .type
        ("" (alist-get 'name (alist-get key alist)))
        ("module" (hoogle-base--name (if (eq key 'module) .item .package.name)))
        ("package" (hoogle-base--name .item))))))


;;;; Interactive Commands
(defun hoogle-base-browse-item ()
  "Browse the url for current item."
  (interactive)
  (hoogle-base--browse-url 'item (hoogle-base--candidate)))

(defun hoogle-base-browse-module ()
  "Browse the url for the module the current item belongs to."
  (interactive)
  (hoogle-base--browse-url 'module (hoogle-base--candidate)))

(defun hoogle-base-browse-package ()
  "Browse the url for the package the current item belongs to."
  (interactive)
  (hoogle-base--browse-url 'package (hoogle-base--candidate)))

(defun hoogle-base-restrict-to-package (package &optional arg)
  "Restrict the search to PACKAGE.
With prefix ARG exluce package from search."
  (interactive (list (hoogle-base--get 'package) current-prefix-arg))
  (when package
    (hoogle-base--add-to-input (if arg "-" "+") (downcase package))))

(defun hoogle-base-restrict-to-module (module &optional arg)
  "Restrict the search to MODULE.
With prefix ARG exluce module from search."
  (interactive (list (hoogle-base--get 'module) current-prefix-arg))
  (when module (hoogle-base--add-to-input (if arg "-" "+") module)))

(defun hoogle-base-restrict-to-module-level-beg (module level)
  "Restrict to a part of MODULE heirarchy.
If called with numeric prefix LEVEL only use first ARG levels of module."
  (interactive (list (hoogle-base--get 'module)
                     (prefix-numeric-value current-prefix-arg)))
  (when module
    (hoogle-base--add-to-input
     (if (> level 0) "+" "-")
     (progn
       (string-match (rx-to-string
                      `(: bos (= ,(abs level) (: (1+ (not ".")) (?? ".")))))
                     module)
       (match-string 0 module)))))

(defun hoogle-base-restrict-to-module-level-end (module level)
  "Restrict to a part of MODULE heirarchy.
If called with numeric prefix LEVEL only use last ARG levels of module."
  (interactive (list (hoogle-base--get 'module)
                     (prefix-numeric-value current-prefix-arg)))
  (when module
    (hoogle-base--add-to-input
     (if (> level 0) "+" "-")
     (progn
       (string-match (rx-to-string
                      `(: (= ,(abs level) (: (1+ (not ".")) (?? "."))) eos))
                     module)
       (match-string 0 module)))))

(defun hoogle-base-clear-restrictions (arg)
  "Clear all restrictions and exclusions on the search.
With positive prefix ARG only clear restrictions. With negative prefix
only clear exclusions."
  (interactive (list (when current-prefix-arg
                       (prefix-numeric-value current-prefix-arg))))
  (let ((restriction-rx (rx-to-string `(: ,(if (not arg)
                                               '(or "+" "-")
                                             (if (> arg 0) "+" "-"))
                                        (0+ (not space))))))
    (funcall hoogle-base-modify-query-function
             (lambda (match) (replace-regexp-in-string restriction-rx "" match)))))

(provide 'hoogle-base)
;;; hoogle-base.el ends here
