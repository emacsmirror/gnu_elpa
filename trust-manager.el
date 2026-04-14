;;; trust-manager.el --- Convenient trust management  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Eshel Yaron

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Maintainer: Eshel Yaron <me@eshelyaron.com>
;; Keywords: security trust files
;; URL: https://git.sr.ht/~eshel/trust-manager
;; Package-Version: 0.2.0
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
;;
;; Another utility command is `trust-manager-set-project-trust', which
;; lets you mark any project as trusted or untrusted, not necessarily
;; the current project.  The command `trust-manager-set-file-trust' is
;; similar, except that it supports arbitrary files/directories,
;; rather than just projects.
;;
;; By default, `trust-manager-mode' also adds a mode line indicator in
;; untrusted buffers where risky features may have been disabled.
;; The default indicator is a `?' shown in red.  You can click on the
;; indicator to mark the buffer as trusted (it runs the command
;; `trust-manager-trust-this-buffer', which you can run directly too).
;; You can also customize or disable this indicator via the
;; `trust-manager-untrusted-indicator' user option, and the face with
;; the same name.
;;
;; Since only some features require trust, not every untrusted buffer
;; needs your attention, only those in which the lack of trust matters.
;; The user option `trust-manager-trust-indicator-buffer-condition'
;; controls in which untrusted buffers the indicator is shown.
;; By default it specifies only Emacs Lisp buffers, because several
;; Emacs Lisp editing features, including on-the-fly diagnostics,
;; require trust.
;;
;; When `trust-manager-mode' marks a previously untrusted buffer as
;; trusted, e.g. when you click on the untrusted buffer mode line
;; indicator, it runs the hook `trust-manager-now-trusted-hook'.
;; By default, `trust-manager-mode' uses this hook to re-enable the
;; Emacs Lisp Flymake backend for on-the-fly diagnostics.

;;; Code:

(defgroup trust-manager nil
  "Trust management."
  :group 'files)

(defvar-local trust-manager--cached-trusted-content 'unset)

(defun trust-manager--cached-trusted-content-p ()
  "Like `trusted-content-p', but caches result in a buffer local variable.
Do not use this function, especially not for security-relevant trust checks.
It's only meant for UI use where stale values are acceptable."
  (if (eq trust-manager--cached-trusted-content 'unset)
      (setq trust-manager--cached-trusted-content (trusted-content-p))
    trust-manager--cached-trusted-content))

(defcustom trust-manager-now-trusted-hook nil
  "Hook `trust-manager-mode' runs when marking an untrusted buffer as trusted.
The newly trusted buffer is current when functions on this hook run."
  :type 'hook)

(defun trust-manager--set-file-trust (file trust)
  "If TRUST is non-nil, trust FILE; otherwise untrust it."
  (let* ((exp (expand-file-name (if (file-directory-p file)
                                    (file-name-as-directory file)
                                  file)))
         (file (abbreviate-file-name exp))
         (curr (delete file (default-value 'trusted-content))))
    (setq-default trusted-content (if trust (cons file curr) curr))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when-let* ((fbuf buffer-file-name)
                    (fbuf (expand-file-name fbuf)))
          (when (or (and (string-suffix-p "/" exp) (string-prefix-p exp fbuf))
                    (equal fbuf exp))
            (let ((old trust-manager--cached-trusted-content))
              (setq trust-manager--cached-trusted-content trust)
              (and trust (not old)
                   (run-hooks 'trust-manager-now-trusted-hook)))))))))

(defun trust-manager--set-files-trust (alist)
  "Apply trust values from ALIST."
  (let ((last-trusted-dir nil))
    (pcase-dolist (`(,file . ,trust)
                   ;; Handle files/subdirectories after a directory
                   ;; that contains them.
                   (sort (mapcar (pcase-lambda (`(,file . ,trust))
                                   (cons (expand-file-name file) trust))
                                 alist)))
      (unless (and last-trusted-dir
                   ;; Skip FILE if it's a descendant of a trusted dir,
                   ;; and thus trusted too.
                   (string-prefix-p last-trusted-dir (expand-file-name file)))
        (and (file-directory-p file) trust
             (setq last-trusted-dir
                   (expand-file-name (file-name-as-directory file))))
        (trust-manager--set-file-trust file trust)))))

(defcustom trust-manager-trust-alist nil
  "Alist mapping file/directory names to boolean trust values.

When `trust-manager-mode' is enabled, it marks files and directories
that appear as keys in this alist as trusted or untrusted according to
their associated values.
This also happens when you customize this user option.

If an entry in this alist says that some directory is trusted, then any
other entry that specifies a file or subdirectory of the trusted
directory is ignored, since it is already implicitly trusted as well."
  :type '(alist :key-type (file :tag "File or Directory")
                :value-type (boolean :tag "Is Trusted"))
  :risky t
  :package-version '(trust-manager . "0.1.0")
  :set (lambda (symbol value)
         (trust-manager--set-files-trust value)
         (set-default-toplevel-value symbol (sort value))))

;;;###autoload
(defun trust-manager-customize ()
  "Customize trusted files and directories."
  (interactive)
  (customize-option 'trust-manager-trust-alist))

;;;###autoload
(defun trust-manager-set-file-trust (file &optional trust)
  "If TRUST is non-nil, trust FILE; otherwise untrust it.

Interactively, prompt for FILE, and set TRUST to non-nil.
With a prefix argument, set TRUST to nil instead."
  (interactive
   (let ((trust (not current-prefix-arg))
         (dir (if default-directory (abbreviate-file-name default-directory) "~/")))
     (list (read-file-name
            (format "%srust file/directory: " (if trust "T" "Unt")) dir dir)
           trust)))
  (let ((abbr (abbreviate-file-name
               (expand-file-name (if (file-directory-p file)
                                     (file-name-as-directory file)
                                   file)))))
    (setf (alist-get abbr trust-manager-trust-alist nil nil #'equal) trust)
    (customize-save-variable 'trust-manager-trust-alist trust-manager-trust-alist)
    (message "Marked `%s' as %strusted" abbr (if trust "" "un"))))

;;;###autoload
(defun trust-manager-set-project-trust (project &optional trust)
  "If TRUST is non-nil, trust PROJECT; otherwise untrust it.

Interactively, prompt for PROJECT, and set TRUST to non-nil.
With a prefix argument, set TRUST to nil instead."
  (interactive
   (let ((trust (not current-prefix-arg))
         (def (when-let* ((pr (project-current))) (project-root pr))))
     (list (completing-read
            (format-prompt "%srust project" def (if trust "T" "Unt"))
            (project-known-project-roots)
            (lambda (root)
              (if-let* ((ent (assoc root trust-manager-trust-alist)))
                  (xor trust (cdr ent))
                t)))
           trust)))
  (trust-manager-set-file-trust project trust))

(defun trust-manager--already-trusted-p (dir)
  "Return non-nil if DIR is trusted according to `trusted-content'."
  (let ((tc (default-value 'trusted-content)))
    (or (eq tc :all)
        (and (listp tc)
             (catch 'ball
               (dolist (par tc)
                 (when (file-in-directory-p dir par)
                   (throw 'ball t))))))))

(defun trust-manager--should-trust-p (pr)
  "Ask and return non-nil if project directory PR should be trusted."
  (yes-or-no-p
   (substitute-quotes
    (format "Trust project directory `%s'?" pr))))

(declare-function project-root "project" (project))

(defun trust-manager--check-file ()
  "Check if the current project should be marked as trusted."
  (when-let* ((pc (project-current)))
    (let ((pr (project-root pc)))
      (unless (or
               ;; Already asked about this project.
               (assoc pr trust-manager-trust-alist)
               ;; Already trusted in `trusted-content'.
               (trust-manager--already-trusted-p pr))
        (let ((trust (trust-manager--should-trust-p pr)))
          (trust-manager--set-file-trust pr trust)
          (customize-save-variable
           'trust-manager-trust-alist
           `((,pr . ,trust) . ,trust-manager-trust-alist))
          (message "Marked project directory `%s' as %strusted"
                   pr (if trust "" "un")))))))

(defun trust-manager--set-buffer-trust (&optional buffer trust)
  "If TRUST is non-nil, trust BUFFER; otherwise untrust it."
  (with-current-buffer (or buffer (current-buffer))
    (let ((old (trust-manager--cached-trusted-content-p)))
      (setq-local trusted-content (when trust :all))
      (setq trust-manager--cached-trusted-content trust)
      (and trust (not old) (run-hooks 'trust-manager-now-trusted-hook)))))

(defun trust-manager-trust-this-buffer (&optional event)
  "Mark buffer at EVENT as trusted, defaulting to current buffer."
  (declare
   (completion
    ;; Exclude this command from M-x completions in trusted buffers.
    (lambda (_ buf &rest _)
      (not (eq t (buffer-local-value
                  'trust-manager--cached-trusted-content buf))))))
  (interactive (list last-nonmenu-event))
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (trust-manager--set-buffer-trust nil t)
    (message "Buffer `%s' is now trusted" (current-buffer))
    (force-mode-line-update)))

(defvar-keymap trust-manager--on-trust-indicator-mouse-map
  "<mode-line> <follow-link>" 'mouse-face
  "<mode-line> <mouse-2>"    #'trust-manager-trust-this-buffer)

(defcustom trust-manager-untrusted-indicator "?"
  "String to show in the mode line in untrusted buffers.
This can also be nil, in which case `trust-manager-mode' does not
display any indicator for untrusted buffers."
  :type '(choice string (const :tag "No indicator" nil))
  :risky t
  :package-version '(trust-manager . "0.2.0"))

(defcustom trust-manager-trust-indicator-buffer-condition
  '(derived-mode emacs-lisp-mode)
  "Condition determining in which buffers to show trust indicator.
See `buffer-match-p' for a description of the possible condition values."
  :type 'buffer-predicate
  :risky t
  :package-version '(trust-manager . "0.2.0"))

(defface trust-manager-untrusted-indicator '((t :inherit error))
  "Face for the indicator `trust-manager-mode' shows in untrusted buffers."
  :package-version '(trust-manager . "0.2.0"))

(defvar trust-manager--trust-indicator
  '(:eval
    (when (buffer-match-p
           trust-manager-trust-indicator-buffer-condition
           (current-buffer))
      (if (trust-manager--cached-trusted-content-p) ""
        (concat
         (propertize
          trust-manager-untrusted-indicator
          'face 'trust-manager-untrusted-indicator
          'help-echo "mouse-2: Trust this buffer\nBuffer is UNTRUSTED (some features may be disabled)"
          'mouse-face 'mode-line-highlight
          'follow-link t
          'keymap trust-manager--on-trust-indicator-mouse-map)
         " ")))))

(put 'trust-manager--trust-indicator 'risky-local-variable t)

(defun trust-manager--ensure-extensible-global-mode-string ()
  "Ensure `global-mode-string' can be extended."
  (unless global-mode-string (setq global-mode-string '("")))
  (when (or (atom global-mode-string)
            (when-let* ((sym (car global-mode-string))) (symbolp sym)))
    (setq global-mode-string `("" ,global-mode-string))))

(defun trust-manager--enable-elisp-flymake-backend ()
  "Re-enable `elisp-flymake-byte-compile', if it is disabled."
  (and (bound-and-true-p flymake-mode)
       (fboundp 'flymake-disabled-backends) ; Silence byte-compiler.
       (memq 'elisp-flymake-byte-compile (flymake-disabled-backends))
       (fboundp 'flymake--run-backend)      ; Likewise.
       (flymake--run-backend 'elisp-flymake-byte-compile)))

(defun trust-manager--set-up-for-elisp ()
  "Set up re-enabling `elisp-flymake-byte-compile' when the buffer is trusted."
  (add-hook
   'trust-manager-now-trusted-hook
   #'trust-manager--enable-elisp-flymake-backend nil t))

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
  :group 'files
  :global t
  :package-version '(trust-manager . "0.1.0")
  (when trust-manager-untrusted-indicator
    (trust-manager--ensure-extensible-global-mode-string))
  (if trust-manager-mode
      (progn
        (dolist (fn `(,user-init-file
                      ,early-init-file
                      ,custom-file
                      . ,load-path))
          (and fn (file-name-absolute-p fn)
               (trust-manager--set-file-trust fn t)))
        (trust-manager--set-files-trust trust-manager-trust-alist)
        (add-hook 'find-file-hook #'trust-manager--check-file)
        (add-hook 'emacs-lisp-mode-hook #'trust-manager--set-up-for-elisp)
        (or (null trust-manager-untrusted-indicator)
            (memq 'trust-manager--trust-indicator global-mode-string)
            (setq global-mode-string
                  (append global-mode-string '(trust-manager--trust-indicator)))))
    (remove-hook 'find-file-hook #'trust-manager--check-file)
    (remove-hook 'emacs-lisp-mode-hook #'trust-manager--set-up-for-elisp)
    (setq global-mode-string
          (delq 'trust-manager--trust-indicator global-mode-string))))

(provide 'trust-manager)
;;; trust-manager.el ends here
