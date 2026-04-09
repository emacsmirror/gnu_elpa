;;; trust-manager.el --- Convenient trust management  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Maintainer: Eshel Yaron <me@eshelyaron.com>
;; Keywords: security trust
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

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

;; This library provides the `trust-manager-mode' minor mode, which
;; helps you manage trusted directories with minimal configuration.
;; It is intended to streamline the management of `trusted-content',
;; added in Emacs 30 as a new security measure.
;;
;; Just enable `trust-manager-mode' in your init file and you should
;; be good to go.  The mode focuses on per-project trust designation.
;; It asks you whether you trust a project the first time you visit a
;; file in that project, and remembers your choices across sessions.
;; Your trusted/untrusted projects are stored in the user option
;; `trust-manager-trust-alist'.  If you change your mind about some
;; project, just customize this user option; you can do so directly
;; or via the utility command `trust-manager-customize'.  You may also
;; customize `trust-manager-trust-alist' to designate some directories
;; as trusted or untrusted before actually visiting files in them.

;;; Code:

(defgroup trust-manager nil
  "Trust management."
  :group 'files)

(defun trust-manager--set-file-trust (file trust)
  "If TRUST is non-nil, trust FILE; otherwise untrust it."
  (let* ((file (abbreviate-file-name
                (expand-file-name (if (file-directory-p file)
                                      (file-name-as-directory file)
                                    file))))
         (curr (delete file (default-value 'trusted-content))))
    (setq-default trusted-content (if trust (cons file curr) curr))))

(defcustom trust-manager-trust-alist nil
  "Alist mapping file/directory names to boolean trust values.

When `trust-manager-mode' is enabled, it marks files and directories
that appear as keys in this alist as trusted or untrusted according to
their associated values.
This also happens when you customize this user option."
  :type '(alist :key-type (file :tag "File or Directory")
                :value-type (boolean :tag "Is Trusted"))
  :risky t
  :set (lambda (symbol value)
         (pcase-dolist (`(,dir . ,trust) value)
           (trust-manager--set-file-trust dir trust))
         (set-default-toplevel-value symbol (sort value))))

;;;###autoload
(defun trust-manager-customize ()
  "Customize trusted files and directories."
  (interactive)
  (customize-option 'trust-manager-trust-alist))

(declare-function project-root "project" (project))

(defun trust-manager--check-file ()
  "Check if the current project should be marked as trusted."
  (when-let* ((pc (project-current)))
    (let ((pr (project-root pc)))
      (unless (assoc pr trust-manager-trust-alist)
        (let ((trust (yes-or-no-p
                      (substitute-quotes
                       (format "Trust project directory `%s'?" pr)))))
          (trust-manager--set-file-trust pr trust)
          (customize-save-variable
           'trust-manager-trust-alist
           `((,pr . ,trust) . ,trust-manager-trust-alist))
          (message "Marked project directory `%s' as %strusted"
                   pr (if trust "" "un")))))))

;;;###autoload
(define-minor-mode trust-manager-mode
  "Toggle per-project trust management with Trust Manager minor mode.

When you enable Trust Manager mode, it marks some standard files as
trusted by adding them to `trusted-content', and sets up a hook that asks
you whether you trust a project the first time you visit a file in that
project.  Your answers are persisted in the `trust-manager-trust-alist'
user option, which you may also customize directly.

The standard files that this mode adds to `trusted-content' are your
`user-init-file', `early-init-file', `custom-file' and all directories
in your `load-path'.

If you later disable this mode, it removes the hook that asks you about
project trust, but it does not mark any file or directory as untrusted."
  ;; TODO: Add interactive mode line trust indicator.
  :group 'files
  :global t
  (if trust-manager-mode
      (progn
        (dolist (fn `(,user-init-file
                      ,early-init-file
                      ,custom-file
                      . ,load-path))
          (and fn (file-name-absolute-p fn)
               (trust-manager--set-file-trust fn t)))
        (pcase-dolist (`(,dir . ,trust) trust-manager-trust-alist)
          (trust-manager--set-file-trust dir trust))
        (add-hook 'find-file-hook #'trust-manager--check-file))
    (remove-hook 'find-file-hook #'trust-manager--check-file)))

(provide 'trust-manager)
;;; trust-manager.el ends here
