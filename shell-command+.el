;;; shell-command+.el --- An extended shell-command -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024  Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <~pkal/public-inbox@lists.sr.ht>
;; Version: 2.4.2
;; Keywords: unix, processes, convenience
;; Package-Requires: ((emacs "24.3"))
;; URL: https://git.sr.ht/~pkal/shell-command-plus

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `shell-command+' is a `shell-command' substitute, that extends the
;; regular Emacs command with several features.  After installed,
;; configure the package as follows:
;;
;;	(global-set-key (kbd "M-!") #'shell-command+)
;;
;; A few examples of what `shell-command+' can do:
;;
;; * Count all lines in a buffer, and display the result in the
;;   minibuffer:
;;
;;   > wc -l
;;
;; * Replace the current region (or buffer in no region is selected)
;;   with a directory listing of the parent directory.
;;
;;   .. < ls -l
;;
;; * Delete all instances of the charachters a, b, c, ..., z, in the
;;   selected region (or buffer, if no region was selected).
;;
;;   | tr -d a-z
;;
;; * Open a man-page using Emacs default man page viewer.
;;   `shell-command+' can be extended to use custom Elisp handlers via
;;   as specified in `shell-command+-substitute-alist'.
;;
;;   man fprintf
;;
;; See `shell-command+'s docstring for more details on how it's input
;; is interpreted.  See `shell-command+-features' if you want to
;; disable or add new features.
;;
;; `shell-command+' was originally based on the command `bang' by Leah
;; Neukirchen (https://leahneukirchen.org/dotfiles/.emacs).

;;; News:

;;;; Version 2.4.0 (???)

;; - Allow adding or removing features using
;;   `shell-command+-features'.
;; - Add `shell-command+-default-region' user option.
;; - Remove `shell-command+-use-eshell'.
;; - Deprecate `shell-command+-enable-file-substitution'.
;; - Minor bug fixes and stability improvements.
;;
;; Thanks to Visuwesh for many productive discussions on the mailing
;; list:
;;
;; - https://lists.sr.ht/~pkal/public-inbox/%3C87czduxwt4.fsf%40gmail.com%3E
;; - https://lists.sr.ht/~pkal/public-inbox/%3C878roixwds.fsf%40gmail.com%3E
;; - https://lists.sr.ht/~pkal/public-inbox/%3C87edxmakqq.fsf%40gmail.com%3E

;;; Code:

(eval-when-compile (require 'rx))
(eval-when-compile (require 'pcase))
(require 'thingatpt)

(defgroup shell-command+ nil
  "An extended `shell-command'."
  :group 'external
  :prefix "shell-command+-")

(defcustom shell-command+-prompt "Shell command: "
  "Prompt to use when invoking `shell-command+'."
  :type 'string)

(defcustom shell-command+-default-region 'buffer
  "Default thing to apply a command onto.
A symbol such as `line', `page', `defun', ... as defined by
`bounds-of-thing-at-point' will restrict the region to whatever
is specified."
  :type '(symbol :tag "Thing"))


;;; Modular feature support

;;;###autoload
(defcustom shell-command+-features
  (list #'shell-command+-expand-%
        #'shell-command+-command-substitution
        #'shell-command+-redirect-output
        #'shell-command+-implicit-cd)
  "List of features to use by `shell-command+'.
Each element of the list is a symbol designating a function to
call in order.  Each is passed the parsed shell command (see
`shell-command+-parse'), and two functions implementing the
\"main functionality\" and a context.  The former is invoked with
three arguments, the final command string, and two points
designating the beginning and ending of the implicit region.  The
context function is invoked with the previous function passed as
a function object and the same argument as the function,
totalling to four arguments."
  :type '(repeat function))


;;;; Input-output redirection

(defcustom shell-command+-flip-redirection nil
  "Flip the meaning of < and > at the beginning of a command."
  :type 'boolean)

(defun shell-command+-redirect-output (parse form context)
  "Replace form with a command that redirects input and output.
For PARSE, FORM and CONTEXT see `shell-command+-features'."
  (pcase-let ((`(,_ ,mode ,_ ,_) parse))
    (list parse
          (cond ((if shell-command+-flip-redirection
                     (eq mode 'output) (eq mode 'input))
                 (lambda (input beg end)
                   (delete-region beg end)
                   (shell-command input t shell-command-default-error-buffer)
                   (exchange-point-and-mark)))
                ((if shell-command+-flip-redirection
                     (eq mode 'input) (eq mode 'output))
                 (lambda (input beg end)
                   (shell-command-on-region
                    beg end input nil nil
                    shell-command-default-error-buffer t)))
                ((eq mode 'pipe)
                 (lambda (input beg end)
                   (shell-command-on-region
                    beg end input t t
                    shell-command-default-error-buffer t)
                   (exchange-point-and-mark)))
                (t form))
          context)))

(put #'shell-command+-redirect-output
     'shell-command+-docstring
     "When COMMAND starts with...
  <  the output of COMMAND replaces the current selection
  >  COMMAND is run with the current selection as input
  |  the current selection is filtered through COMMAND
  !  COMMAND is simply executed (same as without any prefix)")


;;;; % (file name) expansion

(defun shell-command+-expand-% (parse form context)
  "Replace occurrences of \"%\" in the command.
For PARSE, FORM and CONTEXT see `shell-command+-features'."
  (when buffer-file-name
    (setf (nth 3 parse)
          (replace-regexp-in-string
           (rx (* ?\\ ?\\) (or ?\\ (group "%")))
           (or (file-remote-p buffer-file-name 'localname)
               buffer-file-name)
           (nth 3 parse))))
  (list parse form context))

(put #'shell-command+-expand-%
     'shell-command+-docstring
     "Inside COMMAND, % is replaced with the current file name.  To
insert a literal % quote it using a backslash.")


;;;; Implicit cd

(defun shell-command+-expand-path (path)
  "Expand any PATH into absolute path with additional tricks.

Furthermore, replace each sequence with three or more `.'s with a
proper upwards directory pointers.  This means that '....' becomes
'../../../..', and so on."
  (expand-file-name
   (replace-regexp-in-string
    (rx (>= 2 "."))
    (lambda (sub)
      (mapconcat #'identity (make-list (1- (length sub)) "..") "/"))
    path)))

(defun shell-command+-implicit-cd (parse form context)
  "Modify the `default-directory' in CONTEXT.
For PARSE, FORM and CONTEXT see `shell-command+-features'."
  (pcase-let* ((`(,dir ,_ ,_ ,_) parse))
    (list parse form
          (if dir
              (lambda (fn input beg end)
                (let ((default-directory (shell-command+-expand-path dir)))
                  (funcall fn input beg end)))
            context))))

(put #'shell-command+-implicit-cd
     'shell-command+-docstring
     "If COMMAND is prefixed with an absolute or relative path, the
created process will the executed in the specified path.

This path can also consist pseudo-directories consisting of more
than one \".\".  E.g. if you want to execute a command four
directories above the current `default-directory', you can either
prefix the command with \"../../../../\" or \"....\".")


;;;; Command substitution

(defun shell-command+-cmd-grep (command)
  "Convert COMMAND into a `grep' call."
  (grep-compute-defaults)
  (pcase-let ((`(,cmd . ,args) (shell-command+-tokenize command t)))
    (grep (concat
           (replace-regexp-in-string
            (concat "\\`" grep-program) cmd grep-command)
           (mapconcat #'shell-quote-argument args " ")))))

(defun shell-command+-cmd-find (command)
  "Convert COMMAND into a `find-dired' call."
  (pcase-let ((`(,_ ,dir . ,args) (shell-command+-tokenize command)))
    (find-dired dir (mapconcat #'shell-quote-argument args " "))))

(defun shell-command+-cmd-locate (command)
  "Convert COMMAND into a `locate' call."
  (pcase-let ((`(,_ ,search) (shell-command+-tokenize command)))
    (locate search)))

(defun shell-command+-cmd-man (command)
  "Convert COMMAND into a `man' call."
  (pcase-let ((`(,_ . ,args) (shell-command+-tokenize command)))
    (man (mapconcat #'shell-quote-argument args " "))))

(declare-function Info-menu "info" (menu-item &optional fork))
(defun shell-command+-cmd-info (command)
  "Convert COMMAND into a `info' call."
  (require 'info)
  (pcase-let ((`(,_ . ,args) (shell-command+-tokenize command)))
    (Info-directory)
    (dolist (menu args)
      (Info-menu menu))))

(declare-function diff-no-select "diff" (old new &optional switches no-async buf))
(defun shell-command+-cmd-diff (command)
  "Convert COMMAND into `diff' call."
  (require 'diff)
  (pcase-let ((`(,_ . ,args) (shell-command+-tokenize command t)))
    (let (files flags)
      (dolist (arg args)
        (if (string-match-p (rx bos "-") arg)
            (push arg flags)
          (push arg files)))
      (unless (= (length files) 2)
        (user-error "Usage: diff [file1] [file2]"))
      (pop-to-buffer (diff-no-select (car files)
                                     (cadr files)
                                     flags)))))

(defvar shell-command+--command-regexp)
(defun shell-command+-cmd-sudo (command)
  "Use TRAMP's \"sudo\" method to execute COMMAND."
  (let ((default-directory (concat "/sudo::" default-directory)))
    (unless (string-match shell-command+--command-regexp command)
      (error "Couldn't parse command"))
    (shell-command+ (replace-match "" nil nil command 4))))

(defun shell-command+-cmd-cd (command)
  "Convert COMMAND into a `cd' call."
  (pcase-let ((`(,_ ,directory) (shell-command+-tokenize command)))
    (cd directory)))

(defcustom shell-command+-clear-function (apply-partially #'quit-window t)
  "Function to invoke without any arguments when handling \"clear\"."
  :type 'function)

(defun shell-command+-cmd-clear (&rest _command)
  "Empty the contents of the the *Shell Output* buffer."
  (with-current-buffer (or (bound-and-true-p shell-command-buffer-name)
                           "*Shell Command Output*")
    (erase-buffer)
    (funcall shell-command+-clear-function)))

(defcustom shell-command+-substitute-alist
  '(("grep" . shell-command+-cmd-grep)
    ("fgrep" . shell-command+-cmd-grep)
    ("agrep" . shell-command+-cmd-grep)
    ("egrep" . shell-command+-cmd-grep)
    ("rgrep" . shell-command+-cmd-grep)
    ("find" . shell-command+-cmd-find)
    ("locate" . shell-command+-cmd-locate)
    ("man" . shell-command+-cmd-man)
    ("info" . shell-command+-cmd-info)
    ("diff" . shell-command+-cmd-diff)
    ("make" . compile)
    ("sudo" . shell-command+-cmd-sudo)
    ("cd" . shell-command+-cmd-cd)
    ("clear" . shell-command+-cmd-clear))
  "Association of command substitutes in Elisp.
Each entry has the form (COMMAND . FUNC), where FUNC is passed
the command string.  To disable all command substitutions, set
this option to nil."
  :type '(alist :key-type (string :tag "Command Name")
                :value-type (function :tag "Substitute"))
  :set-after '(shell-command+-use-eshell))

(defun shell-command+-command-substitution (parse form context)
  "Check if FORM can be replaced by some other function call.
This is done by querying `shell-command+-substitute-alist'.  FORM
PARSE, FORM and CONTEXT see `shell-command+-features'."
  (pcase-let ((`(,_ ,mode ,name ,_) parse))
    (list parse
          (let ((fn (assoc name shell-command+-substitute-alist)))
            ;; FIXME: It might be that `name' is modified in such a
            ;; way that this check fails and.  Currently no function
            ;; in `shell-command+-features' does this.
            (if (and fn (not (eq mode 'literal)))
                (lambda (command _beg _end)
                  (funcall (cdr fn) command))
              form))
          context)))

(put #'shell-command+-command-substitution
     'shell-command+-docstring
     "If the first word in COMMAND, matches an entry in the alist
`shell-command+-substitute-alist', the respective function is
used to execute the command instead of passing it to a shell
process.  This behaviour can be inhibited by prefixing COMMAND
with !.")


;;; Command tokenization

(defconst shell-command+-token-regexp
  (rx (* space)
      (or (: ?\"
             (group-n 1 (* (or (: ?\\ anychar) (not (any ?\\ ?\")))))
             ?\")
          (: ?\'
             (group-n 1 (* (or (: ?\\ anychar) (not (any ?\\ ?\')))))
             ?\')
          (group (+ (not (any space ?\\ ?\" ?\')))
                 (* ?\\ anychar (* (not (any space ?\\ ?\" ?\'))))))
      (* space))
  "Regular expression for tokenizing shell commands.")

(defun shell-command+-tokenize (command &optional expand)
  "Return list of tokens of COMMAND.
If EXPAND is non-nil, expand wildcards."
  (let ((pos 0) tokens)
    (while (string-match shell-command+-token-regexp command pos)
      (push (let ((tok (match-string 2 command)))
              (if (and expand tok)
                  (or (file-expand-wildcards tok) (list tok))
                (list (replace-regexp-in-string
                       (rx (* ?\\ ?\\) (group ?\\ (group anychar)))
                       "\\2"
                       (or (match-string 2 command)
                           (match-string 1 command))
                       nil nil 1))))
            tokens)
      (when (= pos (match-end 0))
        (error "Zero-width token parsed"))
      (setq pos (match-end 0)))
    (unless (= pos (length command))
      (error "Tokenization error at %S in string %S (parsed until %d, instead of %d)"
             (substring command pos) command pos (length command)))
    (apply #'append (nreverse tokens))))


;;; Command parsing

(defconst shell-command+--command-regexp
  (rx bos
      ;; Ignore all preceding whitespace
      (* space)
      ;; Check for working directory string
      (? (group (or (: ?. (not (any "/"))) ?/ ?~)
                (* (not space)))
         (+ space))
      ;; Check for redirection indicator
      (? (group (or ?< ?> ?| ?!)))
      ;; Allow whitespace after indicator
      (* space)
      ;; Actual command
      (group
       ;; Skip environmental variables
       (* (: (+ alnum) "=" (or (: ?\" (* (or (: ?\\ anychar) (not (any ?\\ ?\")))) ?\")
                               (: ?\'(* (or (: ?\\ anychar) (not (any ?\\ ?\')))) ?\')
                               (+ (not space))))
          (+ space))
       ;; Command name
       (group (+ (not space)))
       ;; Parse arguments
       (*? space)
       (group (*? anything)))
      ;; Ignore all trailing whitespace
      (* space)
      eos)
  "Regular expression to parse `shell-command+' input.")

(defun shell-command+-parse (command)
  "Return parsed representation of COMMAND.
The resulting list has the form (DIRECTORY INDIRECTION EXECUTABLE
COMMAND), where DIRECTORY is the directory the command should be
executed in, if non-nil, indirection is one of `input', `output',
`pipe', `literal' or nil depending on the indirection-prefix,
executable is the name of the executable, and command is the
entire command."
  (save-match-data
    (unless (string-match shell-command+--command-regexp command)
      (error "Invalid command"))
    (let ((dir (match-string-no-properties 1 command))
          (ind (cond ((string= (match-string-no-properties 2 command) "<")
                      'input)
                     ((string= (match-string-no-properties 2 command) ">")
                      'output)
                     ((string= (match-string-no-properties 2 command) "|")
                      'pipe)
                     ((or (string= (match-string-no-properties 2 command) "!")
                          ;; Check if the output of the command is being
                          ;; piped into some other command. In that case,
                          ;; interpret the command literally.
                          (let ((args (match-string-no-properties 5 command)))
                            (save-match-data
                              (member "|" (shell-command+-tokenize args)))))
                      'literal)))
          (cmd (match-string-no-properties 4 command))
          (all (match-string-no-properties 3 command)))
      (if (or (null dir) (file-directory-p (shell-command+-expand-path dir)))
          ;; FIXME: Avoid hard-coding the `shell-command+-expand-path'
          ;; check into the parsing function.
          (list dir ind cmd all)
        (list nil ind dir (format "%s %s" dir all))))))


;;; Main entry point

;;;###autoload
(defun shell-command+--make-docstring ()
  "Return a docstring for `shell-command+'."
  (with-temp-buffer
    (insert (documentation (symbol-function 'shell-command+) 'raw))
    (dolist (feature shell-command+-features)
      (if (fboundp 'make-separator-line)
          (insert "\n\n" (make-separator-line) "\n")
        (newline 2))
      (insert
       (let ((doc (get feature 'shell-command+-docstring)))
         (or doc (documentation feature)
             (format "`%S' is not explicitly documented." feature)))))
    (buffer-string)))

;;;###autoload
(put #'shell-command+ 'function-documentation
     '(shell-command+--make-docstring))

;;;###autoload
(defun shell-command+ (command &optional beg end)
  "An extended alternative to `shell-command'.

COMMAND may be parsed and modified based on the comments of
`shell-command+-features'.  If the command modifies the current
buffer contents, it will do so between BEG and END.  If BEG or
END are not passed, the beginning or end of the buffer will
respectively be assumed as a fallback.

The current configuration adds the following functionality, that
can be combined but will be processed in the following order:"
  (interactive (let ((bounds (bounds-of-thing-at-point
                              (or shell-command+-default-region
                                  ;; We default to buffer for
                                  ;; compatibility reasons, back when
                                  ;; `shell-command+-default-region'
                                  ;; interpreted nil as the default
                                  ;; option to use the entire buffer.
                                  'buffer))))
                 (list (read-shell-command
                        (if (bound-and-true-p shell-command-prompt-show-cwd)
                            (format shell-command+-prompt
                                    (abbreviate-file-name default-directory))
                          shell-command+-prompt))
                       (cond ((use-region-p) (region-beginning))
                             (bounds (car bounds)))
                       (cond ((use-region-p) (region-end))
                             (bounds (cdr bounds))))))
  ;; Make sure in case there is a previous output buffer, that it has
  ;; the same `default-directory' as the `default-directory' caller.
  (let ((shell-command-buffer (get-buffer (or (bound-and-true-p shell-command-buffer-name)
                                              "*Shell Command Output*")))
        (def-dir default-directory))
    (when shell-command-buffer
      (with-current-buffer shell-command-buffer
        (cd def-dir))))
  (let ((shell-command+-features shell-command+-features) ;copy binding
        (form (lambda (input _beg _end)
                (shell-command
                 input
                 (and current-prefix-arg t)
                 shell-command-default-error-buffer)))
        (context #'funcall)
        (parse (shell-command+-parse command)))
    (while shell-command+-features
      (let ((step (funcall (pop shell-command+-features)
                           parse form context)))
        (setq parse (nth 0 step)
              form (nth 1 step)
              context (nth 2 step))))
    (funcall context form (nth 3 parse)
             (or beg (point-min))
             (or end (point-max)))))

(provide 'shell-command+)

;;; shell-command+.el ends here
