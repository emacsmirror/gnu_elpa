;;; transient-cycles.el --- Define command variants with transient cycling  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026  Free Software Foundation, Inc.

;; Author: Sean Whitton <spwhitton@spwhitton.name>
;; Maintainer: Sean Whitton <spwhitton@spwhitton.name>
;; Package-Requires: ((emacs "29.1"))
;; Version: 2.1
;; URL: https://git.spwhitton.name/dotfiles/tree/.emacs.d/user-lisp/transient-cycles.el
;; Keywords: buffer, window, processes, minor-mode, convenience

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides four global minor modes:
;;
;; - `transient-cycles-buffer-siblings-mode'
;;   Enhances buffer switching commands by adding transient cycling.
;;   After typing 'C-x b', 'C-x 4 C-o', 'C-h i' and others, you can use
;;   <left>/<right> to switch between other closely related buffers.
;;   For example, after using 'C-x b' to switch to a buffer which has a
;;   (possibly indirect) clone, <right> will switch to the clone, and a
;;   subsequent <left> will take you back.
;;
;; - `transient-cycles-window-buffers-mode'
;;   Enhances 'C-x <left>' and 'C-x <right>' by adding transient cycling.
;;   After typing one of these commands, you can use <left>/<right> to move
;;   further forwards or backwards in a list of the buffer's previous,
;;   current and next buffers.  But this list is virtual: after exiting
;;   transient cycling, it is as though you used an exact numeric prefix
;;   argument to 'C-x <left>' or 'C-x <right>' to go to the final destination
;;   buffer in just one command, without visiting the others.
;;
;; - `transient-cycles-tab-bar-mode'
;;   Enhances 'C-x t o' and 'C-x t O' by adding transient cycling.
;;   After typing one of these commands, you can use <left>/<right> to move
;;   further in the list of tabs.  But after exiting transient cycling, it is
;;   as though you did not visit the intervening tabs and went straight to
;;   your destination tab.  In particular, 'M-x tab-recent' toggles between
;;   the initial and final tabs.
;;
;; - `transient-cycles-shells-mode'
;;   Enhances 'C-x p s' (or C-x p e) with transient cycling, and completely
;;   replaces 'M-!', 'M-&', and Dired's '!' and '&' commands.  These new
;;   commands all switch to shell buffers instead of doing minibuffer
;;   prompting, automatically starting fresh shell buffers when others are
;;   busy running commands.  In addition, after switching to a shell, you can
;;   use <left>/<right> to quickly switch to other shell buffers.
;;   There is support for both `shell-mode' inferior shells and Eshell.
;;
;; Further discussion:
;;
;; Many commands can be conceptualised as selecting an item from an ordered
;; list or ring.  Sometimes after running such a command, you find that the
;; item selected is not the one you would have preferred, but the preferred
;; item is nearby in the list.  If the command has been augmented with
;; transient cycling, then it finishes by setting a transient map with keys to
;; move backwards and forwards in the list of items, so you can select a
;; nearby item instead of the one the command selected.  From the point of
;; view of commands subsequent to the deactivation of the transient map, it is
;; as though the first command actually selected the nearby item, not the one
;; it really selected.
;;
;; For example, suppose that you often use `eshell' with a prefix argument to
;; create multiple Eshell buffers, *eshell*, *eshell*<2>, *eshell*<3> and so
;; on.  When you use `switch-to-buffer' to switch to one of these buffers, you
;; will typically end up in the most recently used Eshell.  But sometimes what
;; you wanted was an older Eshell -- but which one?  It would be convenient to
;; quickly cycle through the other Eshell buffers when you discover that
;; `switch-to-buffer' took you to the wrong one.  That way, you don't need to
;; remember which numbered Eshell you were using to do what.
;;
;; In this example, we can think of `switch-to-buffer' as selecting from the
;; list of buffers in `eshell-mode'.  If we augment the command with transient
;; cycling, then you can use the next and previous keys in the transient map
;; to cycle through the `eshell-mode' buffers to get to the right one.
;; Afterwards, it is as though `switch-to-buffer' had taken you directly
;; there: the buffer list is undisturbed except for the target Eshell buffer
;; having been moved to the top, and only the target Eshell buffer is pushed
;; to the window's previous buffers (see `window-prev-buffers').
;;
;; This library provides macros to define variants of commands which have
;; transient cycling, and also some minor modes which replace some standard
;; Emacs commands with transient cycling variants the author has found useful.
;; `transient-cycles-buffer-siblings-mode' implements a slightly more complex
;; version of the transient cycling described in the above example.
;;
;; Definitions of command variants in this file only hide the fact that
;; transient cycling went on -- in the above example, how the buffer list is
;; undisturbed and how only the final buffer is pushed to the window's
;; previous buffers -- to the extent that doing so does not require saving a
;; lot of information when commencing transient cycling.

;;; News:

;; Ver 2.1 2026/06/25 Sean Whitton
;;     In `transient-cycles-shells-jump', match `shell-mode' or `eshell-mode'
;;     exactly instead of using `derived-mode-p'.
;;
;; Ver 2.0 2025/06/25 Sean Whitton
;;     New minor mode, `transient-cycles-shells-mode'.
;;     New macro `transient-cycles-define-buffer-switch'.
;;     New command to restart transient cycling (not bound by default):
;;     `transient-cycles-cmd-transient-cycles-siblings-from-here'.
;;
;; Ver 1.1 2025/02/25 Sean Whitton
;;     Replace uses of `when-let' with `when-let*'.
;;
;; Ver 1.0 2022/04/10 Sean Whitton
;;     Initial release.
;;     Thanks to Protesilaos Stavrou for testing and docs feedback.

;;; Code:

(require 'ring)
(require 'subr-x)
(require 'cl-lib)

(defgroup transient-cycles nil
  "Defaults and options when defining variants of commands with
transient cycling."
  :group 'convenience)

(defcustom transient-cycles-show-cycling-keys t
  "Whether to show the cycling keys in the echo area when
commencing transient cycling."
  :type 'boolean
  :group 'transient-cycles)

(defcustom transient-cycles-default-cycle-backwards-key [left]
  "Default key for cycling backwards in the transient maps set by
commands to which transient cycling has been added."
  :type 'key-sequence
  :group 'transient-cycles)

(defcustom transient-cycles-default-cycle-forwards-key [right]
  "Default key for cycling forwards in the transient maps set by
commands to which transient cycling has been added."
  :type 'key-sequence
  :group 'transient-cycles)

(cl-defmacro transient-cycles-define-commands
    (bindings commands cycler-generator
     &key on-exit
       (cycle-forwards-key transient-cycles-default-cycle-forwards-key)
       (cycle-backwards-key transient-cycles-default-cycle-backwards-key)
       (keymap '(current-global-map)))
  "Define command variants closing over BINDINGS as specified by
COMMANDS with transient cycling as supplied by CYCLER-GENERATOR.

BINDINGS are established by means of `let*' at the beginning of
each command variant.  Thus for each command variant,
CYCLER-GENERATOR and ON-EXIT all close over each of BINDINGS.
The storage is intended to last for the duration of transient
cycling, and may be used for cycling state or to save values from
before cycling began for restoration during ON-EXIT.

Each of COMMANDS defines a command variant, and should be of one
of the following forms:

1. (ORIGINAL ARGS [INTERACTIVE] &body BODY)

2. ORIGINAL alone, which means
   (ORIGINAL (&rest args) (apply ORIGINAL args)).

ORIGINAL can have one of the following forms:

a. a plain symbol

b. a pair (KEY . ORIGINAL) where ORIGINAL is a symbol

c. a vector [remap ORIGINAL] where ORIGINAL is a symbol, which
   means ([remap ORIGINAL] . ORIGINAL).

In each combination, ORIGINAL names the command for which a
transient cycling variant should be defined; ARGS, INTERACTIVE
and BODY are as in `lambda'; and KEY, if present, is a key
sequence to which the command should be bound in KEYMAP.  If
INTERACTIVE is absent then the newly defined command receives
ORIGINAL's interactive form.

CYCLER-GENERATOR defines a function which will be called with the
return value of each command variant, and must return a function
of one argument, which is known as the cycler.  After the call to
the command variant, a transient map is established in which
CYCLE-FORWARDS-KEY invokes the cycler with the numeric value of
the prefix argument and CYCLE-BACKWARDS-KEY invokes the cycler
with the numeric value of the prefix argument multiplied by -1.

CYCLE-FORWARDS-KEY and CYCLE-BACKWARDS-KEY are evaluated at the
time the transient map is established, so it is possible to
compute cycling keys from the binding used to invoke the command.
For example, for CYCLE-FORWARDS-KEY, you might have

    (cond ((memq last-command-event \\='(up down)) [down])
	  ((memq last-command-event \\='(left right)) [right])
	  (t transient-cycles-default-cycle-forwards-key))

ON-EXIT, if present, is wrapped in a lambda expression with no
arguments, i.e. (lambda () ON-EXIT), and passed as the third
argument to `set-transient-map'."
  (macroexp-progn
   (cl-loop with on-exit = (and on-exit `(lambda () ,on-exit))
	 and arg = (gensym) and cycler = (gensym) and tmap = (gensym)
	 and kforwards = (gensym) and kbackwards = (gensym)
	 for command in commands
	 for (original args . body)
	 = (if (proper-list-p command)
	       command
	     (let ((f (cl-etypecase command
			(symbol command)
			(cons (cdr command))
			(vector (aref command 1)))))
	       `(,command (&rest args) (apply #',f args))))
	 for (key . original*) = (cond ((symbolp original)
					(cons nil original))
				       ((and (vectorp original)
					     (eq 'remap (aref original 0)))
					(cons original (aref original 1)))
				       (t original))
	 for name
	 = (intern (format "transient-cycles-cmd-%s" (symbol-name original*)))
	 for original*-name = (symbol-name original*)
	 for doc
	 = (if (stringp (car body))
	       (pop body)
	     (format "Like `%s',%sbut augmented with transient cycling."
		     original*-name
		     (if (length> original*-name
				  (- emacs-lisp-docstring-fill-column 45))
			 "\n"
		       "\s")))
	 collect
	 `(defun ,name ()
	    ,doc
	    (interactive)
	    (let* (,@bindings
		   (,arg (call-interactively
			  (lambda ,args
			    ,@(if (and (listp (car body))
				       (eq 'interactive (caar body)))
				  body
				(cons (interactive-form original*) body))))))
	      (when-let* ((,cycler (funcall ,cycler-generator ,arg))
			  (,tmap (make-sparse-keymap))
			  (,kforwards ,cycle-forwards-key)
			  (,kbackwards ,cycle-backwards-key))
		;; It might be additionally useful to bind something in the
		;; transient map to kill the current buffer and cycle once.
		;;
		;; For the forward direction we could elide this lambda and
		;; just bind the key to the cycler.  But this way means we are
		;; consistent in always supplying an integer.
		(define-key ,tmap ,kforwards (lambda (,arg)
					       (interactive "p")
					       (funcall ,cycler ,arg)))
		(define-key ,tmap ,kbackwards (lambda (,arg)
						(interactive "p")
						(funcall ,cycler (- ,arg))))
		(when transient-cycles-show-cycling-keys
		  (message "Cycle forwards with %s, backwards with %s"
			   (key-description ,kforwards)
			   (key-description ,kbackwards)))
		(set-transient-map ,tmap t ,on-exit))))
	 when key collect `(define-key ,keymap ,key #',name))))
(put 'transient-cycles-define-commands 'common-lisp-indent-function
     '(4 (&whole 2 &rest (&whole 1 4 &body)) &body))

(defvar-local transient-cycles--last-buffers-ring nil
  "Ring of buffers used in last transient cycling that included this buffer.")
(defvar-local transient-cycles--last-buffers-pos nil
  "Position of this buffer in `transient-cycles--last-buffers-ring'.")

(cl-defmacro transient-cycles-buffer-ring-cycler
    (&key (start 0)
       (ring '(transient-cycles-buffer-siblings-ring ret-val))
       (action '(switch-to-buffer buffer t t)))
  "Yield a lambda expression to cycle RING from START using ACTION.
This macro is intended for use as the CYCLER-GENERATOR argument
to `transient-cycles-define-keys'.

RING is a form which evaluates to a ring of buffers.  It should
be written in terms of `ret-val', which at time of evaluation
will hold the return value of calling the command variant as
described in the docstring of `transient-cycles-define-keys'.
ACTION is a form in terms of `buffer', which should cycle to
`buffer' in the relevant sense."
  (cl-with-gensyms (count buffers buffers-pos)
    `(lambda (ret-val)
       (when-let* ((,buffers ,ring)
		   (,buffers-pos ,start))
	 ;; Set these in the index zero buffer (this will often be the current
	 ;; buffer, but not always) and in buffers we actually cycle through.
	 ;; The idea is that buffers the user doesn't see don't get new values
	 ;; for these vars in order to preserve any they might already have.
	 (with-current-buffer (ring-ref ,buffers ,buffers-pos)
	   (setq-local transient-cycles--last-buffers-ring ,buffers
		       transient-cycles--last-buffers-pos ,buffers-pos))
	 (lambda (,count)
	   (interactive "p")
	   (cl-incf ,buffers-pos ,count)
	   (let ((buffer (ring-ref ,buffers ,buffers-pos)))
	     (with-current-buffer buffer
	       (setq-local transient-cycles--last-buffers-ring ,buffers
			   transient-cycles--last-buffers-pos ,buffers-pos))
	     ,action))))))

(defmacro transient-cycles-define-buffer-switch
    (commands &rest keyword-arguments)
  "`transient-cycles-define-commands' but with an implicit CYCLER-GENERATOR.
The return value of each command variant defined by COMMANDS determines a
ring of buffers.  The command variant may either return a ring of buffers
directly, or return a buffer or a window.  In the latter two cases, the
ring of buffers is the buffer siblings of the return value, in the sense of
`transient-cycles-buffer-siblings-ring'.
The cycler generator implicitly provided by this macro returns a cycler which
cycles through the ring of buffers, displaying each one in the selected window
(or if the command variant returned a window, in that window)."
  (declare (indent 0))
  (cl-with-gensyms (window prev-buffers)
    `(transient-cycles-define-commands (,window ,prev-buffers)
       ,(cl-loop for command in commands
		 for (original lambda . body)
		   = (if (proper-list-p command) command
		       `(,command (&rest args)
			  ,(interactive-form (cdr command))
			  (apply #',(cdr command) args)))
		 collect `(,original ,lambda
			   ,@(and (stringp (car body))
				  (list (pop body)))
			   ,@(and (listp (car body))
				  (eq (caar body) 'interactive)
				  (list (pop body)))
			   (let ((ret-val ,(macroexp-progn body)))
			     (when (windowp ret-val)
			       (setq ,window ret-val))
			     (setq ,prev-buffers
				   (window-prev-buffers ,window))
			     ret-val)))
       (transient-cycles-buffer-ring-cycler
	:ring (cl-etypecase ret-val
		(buffer (transient-cycles-buffer-siblings-ring ret-val))
		(window (transient-cycles-buffer-siblings-ring
			 (window-buffer ret-val)))
		(ring ret-val)
		(null nil))
	:action (if (windowp ret-val)
		    (with-selected-window ret-val
		      (let ((display-buffer-overriding-action
			     '((display-buffer-same-window)
			       (inhibit-same-window . nil))))
			(display-buffer buffer)))
		  (switch-to-buffer buffer t t)))
       :on-exit (if ,window
		    (progn (set-window-next-buffers ,window nil)
			   (set-window-prev-buffers ,window ,prev-buffers))
		  (switch-to-buffer (current-buffer) nil t)
		  (set-window-next-buffers nil nil)
		  (set-window-prev-buffers nil ,prev-buffers))
       . ,keyword-arguments)))
(put 'transient-cycles-define-buffer-switch 'common-lisp-indent-function
     '((&whole 2 &rest (&whole 1 4 &body)) &body))

(defcustom transient-cycles-buffer-siblings-major-modes
  '(("\\`*unsent mail" . message-mode))
  "Alist mapping regexps to major modes.
Buffers whose names match a regexp are considered to have the
associated major mode for the purpose of determining whether they
should be associated with families of clones as generated by
`transient-cycles-buffer-siblings-ring', which see."
  :type '(alist :key-type regexp :value-type symbol)
  :group 'transient-cycles)

(defun transient-cycles-buffer-siblings-ring (buffer
					      &optional old-ring old-pos)
  "Return ring of BUFFER clones and buffers sharing the clones' major mode.
BUFFER itself is the first element of the ring, followed by the
clones of BUFFER, and then buffers merely sharing the major mode
of the family of clones.

Clonehood is determined by similarity of buffer names.  Clones
produced by `clone-buffer' and `clone-indirect-buffer' will be
counted as siblings, but so will the two Eshell buffers produced
if you type \\[project-eshell] then \\[universal-argument] \\[project-eshell],
as the same naming scheme is used.  This is desirable for
`transient-cycles-buffer-siblings-mode', which see.

The singular major mode of the family of clones is determined
using heuristics, as it is expected that clones of a buffer may
have different major modes: visiting one file with more than one
major mode is one of the primary uses of indirect clones.

Optional arguments OLD-RING and OLD-POS, if non-nil, are a ring of buffers
and an index into that ring, respectively.  The ring should normally
include BUFFER.  OLD-RING is divided into two lists of buffers (skipping
killed buffers): elements with indexes greater than or equal to OLD-POS,
and elements with indexes strictly less than OLD-POS.  The first list
becomes the first elements of the ring this function returns, instead of
BUFFER, and the second list becomes the last elements of the ring."
  (let* ((clones-hash (make-hash-table))
	 (root-name (buffer-name buffer))
	 (root-name (if (string-match "\\`\\(.+\\)<[0-9]+>\\'" root-name)
			(match-string 1 root-name)
		      root-name))
	 (clones-regexp
	  (concat "\\`" (regexp-quote root-name) "\\(<[0-9]+>\\)?\\'"))
	 (clones-pred
	  (lambda (b) (string-match clones-regexp (buffer-name b))))
	 (buffers (cl-remove-if-not clones-pred (buffer-list)))
	 (mode (or (cdr (assoc root-name
			       transient-cycles-buffer-siblings-major-modes
			       #'string-match))
		   ;; If only one buffer or root clone is visiting a file, use
		   ;; major mode of that one buffer or root clone.  The only
		   ;; case we want here is the root of a family of indirect
		   ;; clones.  Thus, don't consider arbitrary clones visiting
		   ;; files, as this may be because the user cloned, edited
		   ;; down, changed major mode and then wrote to a file.
		   (and (= 1 (length buffers))
			(with-current-buffer (car buffers) major-mode))
		   (let ((root-clone
			  (cl-find root-name buffers
				   :key #'buffer-name :test #'string=)))
		     (and root-clone (with-current-buffer root-clone
				       (and (buffer-file-name) major-mode))))
		   ;; See if the name of one of the clones is a substring of
		   ;; its major mode, and if so, use that mode.
		   ;; E.g. *eww* -> `eww-mode'.  Cases this heuristic will get
		   ;; wrong should have entries in
		   ;; `transient-cycles-buffer-sublings-major-modes'.
		   (cl-loop
		    with root-root-name = (regexp-quote
					   (string-trim root-name "\\*" "\\*"))
		    with case-fold-search = t
		    for buffer in buffers
		    for mode = (symbol-name
				(with-current-buffer buffer major-mode))
		    when (string-match root-root-name mode) return mode)
		   ;; Fallback.
		   (with-current-buffer buffer major-mode)))
	 (old-elts (and old-ring
			(cl-delete-if-not #'buffer-live-p
					  (ring-elements old-ring))))
	 (head+tail-len (if old-ring (length old-elts) 1))
	 (old-pos (or old-pos 0))
	 (head (if old-ring
		   (nthcdr old-pos old-elts)
		 (list buffer)))
	 (tail (ntake old-pos old-elts)))
    (dolist (buffer buffers) (puthash buffer t clones-hash))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (and (eq mode major-mode) (not (gethash buffer clones-hash)))
	  (push buffer buffers))))
    (setq buffers (cl-nset-difference buffers head))
    (setq buffers (cl-nset-difference buffers tail))
    (let ((ring (make-ring (+ (length buffers) head+tail-len))))
      (dolist (buffer (nreverse (nconc head buffers tail)))
	(ring-insert ring buffer))
      ring)))


;;;; Minor modes

(defvar transient-cycles-buffer-siblings-mode-map (make-sparse-keymap)
  "Keymap for `transient-cycles-buffer-siblings-mode'.")

(defcustom transient-cycles-buffer-siblings-cycle-backwards-key [left]
  "Key to cycle backwards in the transient maps set by commands
defined by `transient-cycles-buffer-siblings-mode'."
  :type 'key-sequence
  :group 'transient-cycles)

(defcustom transient-cycles-buffer-siblings-cycle-forwards-key [right]
  "Key to cycle forwards in the transient maps set by commands
defined by `transient-cycles-buffer-siblings-mode'."
  :type 'key-sequence
  :group 'transient-cycles)

;;;###autoload
(define-minor-mode transient-cycles-buffer-siblings-mode
  "Enhance buffer switching commands by adding transient cycling.

Augments a number of standard buffer switching commands.  After
typing \\[switch-to-buffer], \\[display-buffer], \\[info] and
some others, you can use the keys
`transient-cycles-buffer-siblings-cycle-backwards-key' and
`transient-cycles-buffer-siblings-cycle-forwards-key' to select a
different, relevantly similar buffer to select or display
instead.  See `transient-cycles-buffer-siblings-ring' for details
of the notion of similarity employed.

See also `transient-cycles-cmd-transient-cycles-siblings-from-here'.

The purpose of this mode is to make it easier to handle large
numbers of similarly-named buffers without having to take the
time to manually rename them.  For example, suppose while reading
Info you type \\<Info-mode-map>\\[clone-buffer] several times in
order to view several pieces of information at once.  Later you
need to refer back to one of those buffers, but \\[info] will
always take you to `*info*', and if you use \\[switch-to-buffer]
it might take you several tries to select the buffer you wanted.
Thanks to this minor mode, after using either of those commands
to switch to any `Info-mode' buffer you can quickly cycle through
to the intended target."
  :lighter nil :keymap transient-cycles-buffer-siblings-mode-map :global t
  :group 'transient-cycles)

;; It would be possible to rewrite the following two forms with
;; `transient-cycles-define-buffer-switch'.  Leaving them like this serves as
;; a nice usage example for `transient-cycles-define-commands', though.

(transient-cycles-define-commands (prev-buffers)
  (([remap switch-to-buffer] (buffer &optional _norecord force-same-window)
     (prog1 (switch-to-buffer buffer t force-same-window)
       (setq prev-buffers (window-prev-buffers))))

   ([remap switch-to-buffer-other-window] (buffer-or-name &rest _ignore)
     (prog1 (switch-to-buffer-other-window buffer-or-name t)
       (setq prev-buffers (window-prev-buffers))))

   ([remap switch-to-buffer-other-tab] (buffer-or-name)
     (prog1 (window-buffer (switch-to-buffer-other-tab buffer-or-name))
       (setq prev-buffers (window-prev-buffers)))))

  (transient-cycles-buffer-ring-cycler)
  :on-exit (progn (switch-to-buffer (current-buffer) nil t)
		  (set-window-next-buffers nil nil)
		  (set-window-prev-buffers nil prev-buffers))
  :keymap transient-cycles-buffer-siblings-mode-map
  :cycle-forwards-key transient-cycles-buffer-siblings-cycle-forwards-key
  :cycle-backwards-key transient-cycles-buffer-siblings-cycle-backwards-key)

;; Here we don't try to restore the fundamental or frame buffer lists, but it
;; would be possible to do so.  See (info "(elisp) Buffer List").
(transient-cycles-define-commands (window prev-buffers)
  (([remap display-buffer] (buffer-or-name &optional action frame)
     (prog1 (setq window (display-buffer buffer-or-name action frame))
       (setq prev-buffers (window-prev-buffers window))))

   ([remap info] (&optional file-or-node buffer)
    (prog2 (info file-or-node buffer)
	(setq window (get-buffer-window buffer))
      (setq prev-buffers (and window (window-prev-buffers window))))))

  (transient-cycles-buffer-ring-cycler
   :ring (transient-cycles-buffer-siblings-ring (window-buffer ret-val))
   :action
   (with-selected-window ret-val
     (let ((display-buffer-overriding-action
	    '((display-buffer-same-window) (inhibit-same-window . nil))))
       (display-buffer buffer))))
  :on-exit (progn (set-window-next-buffers window nil)
		  (set-window-prev-buffers window prev-buffers))
  :keymap transient-cycles-buffer-siblings-mode-map
  :cycle-forwards-key transient-cycles-buffer-siblings-cycle-forwards-key
  :cycle-backwards-key transient-cycles-buffer-siblings-cycle-backwards-key)

(transient-cycles-define-commands (prev-buffers)
  ((transient-cycles-siblings-from-here ()
     "Start or restart transient cycling among the current buffer's siblings.

This is like \\<transient-cycles-buffer-siblings-mode-map>\\[transient-cycles-cmd-switch-to-buffer] under `transient-cycles-buffer-siblings-mode' except
that cycling always begins with the current buffer.  In addition, if there
was a previous instance of transient cycling among buffers that cycled
through or ended in this buffer, then that transient cycling is restarted.
This works for all commands whose transient cycling was implemented with
`transient-cycles-buffer-ring-cycler'; this includes all commands from
`transient-cycles-buffer-siblings-mode' and `transient-cycles-shells-mode'.

This command is not bound by any of the minor modes included with the
Transient Cycles package.  Therefore, to use this command, you'll need to
first bind it to two key sequences, ending in each of
`transient-cycles-buffer-siblings-cycle-forwards-key' and
`transient-cycles-buffer-siblings-cycle-backwards-key'.
For example, with the default cycling keys, you could use

    (global-set-key [?\\C-c left]
		    #\\='transient-cycles-cmd-transient-cycles-siblings-from-here)
    (global-set-key [?\\C-c right]
		    #\\='transient-cycles-cmd-transient-cycles-siblings-from-here)

You can usefully prefix this command with \\[other-window-prefix], \\[other-frame-prefix] etc. to
(re)start cycling elsewhere."
     (interactive)
     (unless
	 (member (vector last-command-event)
		 (list transient-cycles-buffer-siblings-cycle-forwards-key
		       transient-cycles-buffer-siblings-cycle-backwards-key))
       (error "This command's binding must end in existing cycling key"))
     (prog1
	 (let ((display-buffer-overriding-action
		(or display-buffer-overriding-action
		    '(display-buffer-same-window
		      (inhibit-same-window . nil)))))
	   ;; NORECORD nil because we *do* want the current buffer pushed to
	   ;; the window's previous buffers.
	   (pop-to-buffer-same-window (current-buffer)))
       (push last-command-event unread-command-events)
       (setq prev-buffers (window-prev-buffers)))))
  (transient-cycles-buffer-ring-cycler
   ;; The sense in which this command *re*starts transient cycling is how
   ;; passing these arguments to `transient-cycles-buffer-siblings-ring' makes
   ;; it return a ring of buffer siblings that's similar to the last one.
   ;; The only differences should be that any new buffers have been inserted
   ;; in the middle of the ring (i.e. far away from the current buffer, in
   ;; either direction), and killed buffers have been taken out.
   :ring (transient-cycles-buffer-siblings-ring
	  ret-val
	  (buffer-local-value 'transient-cycles--last-buffers-ring ret-val)
	  (buffer-local-value 'transient-cycles--last-buffers-pos ret-val)))
  :on-exit (progn (switch-to-buffer (current-buffer) nil t)
		  (set-window-next-buffers nil nil)
		  (set-window-prev-buffers nil prev-buffers))
  :cycle-forwards-key transient-cycles-buffer-siblings-cycle-forwards-key
  :cycle-backwards-key transient-cycles-buffer-siblings-cycle-backwards-key)

(defvar transient-cycles-window-buffers-mode-map (make-sparse-keymap)
  "Keymap for `transient-cycles-window-buffers-mode'.")

(defcustom transient-cycles-window-buffers-cycle-backwards-key [left]
  "Key to cycle backwards in the transient maps set by commands
defined by `transient-cycles-window-buffers-mode'."
  :type 'key-sequence
  :group 'transient-cycles)

(defcustom transient-cycles-window-buffers-cycle-forwards-key [right]
  "Key to cycle forwards in the transient maps set by commands
defined by `transient-cycles-window-buffers-mode'."
  :type 'key-sequence
  :group 'transient-cycles)

;;;###autoload
(define-minor-mode transient-cycles-window-buffers-mode
  "Enhance window buffer switching commands by adding transient cycling.

Augments \\[previous-buffer] and \\[next-buffer].  After typing
those commands, you can use
`transient-cycles-window-buffers-cycle-backwards-key' and
`transient-cycles-window-buffers-cycle-forwards-key' to move
forwards and backwards in a virtual list of the window's
previous, current and next buffers.  When transient cycling
completes, your starting point will be stored, such that
\\[transient-cycles-window-buffers-back-and-forth] can quickly
take you back there."
  :lighter nil :keymap transient-cycles-window-buffers-mode-map :global t
  :group 'transient-cycles
  (if transient-cycles-window-buffers-mode
      ;; Clear window parameter just when the list of next buffers is cleared.
      (advice-add 'set-window-buffer :after
		  #'transient-cycles--reset-window-recent-buffer)
    (advice-remove 'set-window-buffer
		   #'transient-cycles--reset-window-recent-buffer)))

(cl-symbol-macrolet
    ((param (window-parameter nil 'transient-cycles--window-recent-buffer)))
  (transient-cycles-define-commands (recent-buffer last-recent-buffer)
    (([remap previous-buffer] (count)
       (interactive "p")
       (setq recent-buffer (and (window-next-buffers) count)
	     last-recent-buffer param)
       (previous-buffer count))

     ([remap next-buffer] (count)
       (interactive "p")
       ;; We consider only the window's next buffers, not the frame's next
       ;; buffers as `next-buffer' does.  This is because otherwise our
       ;; `recent-buffer' and window parameter become invalid.
       ;;
       ;; `previous-buffer' and `next-buffer' use `switch-to-prev-buffer' and
       ;; `switch-to-next-buffer' as subroutines, so buffers previously shown
       ;; in the selected window come up first
       (if (window-next-buffers)
	   (progn (setq recent-buffer (* -1 count)
			last-recent-buffer param)
		  (next-buffer count))
	 (user-error "No next buffer"))))

    (lambda (_ignore)
      (lambda (count)
	(if (cl-plusp count)
	    (if (window-next-buffers)
		(progn (when recent-buffer (cl-decf recent-buffer count))
		       (next-buffer count))
	      (message "No next buffer"))
	  (setq count (* -1 count))
	  (when recent-buffer (cl-incf recent-buffer count))
	  (previous-buffer count))))
    ;; If `recent-buffer' is zero then we are back where we started.
    ;; in.  In that case, restore the old parameter value so that
    ;; `transient-cycles-window-buffers-back-and-forth' does something useful.
    :on-exit (setq param (if (and recent-buffer (zerop recent-buffer))
			     last-recent-buffer
			   recent-buffer))
    :keymap transient-cycles-window-buffers-mode-map
    :cycle-forwards-key transient-cycles-window-buffers-cycle-forwards-key
    :cycle-backwards-key transient-cycles-window-buffers-cycle-backwards-key))

(defun transient-cycles--reset-window-recent-buffer (&rest _ignore)
  (set-window-parameter nil 'transient-cycles--window-recent-buffer nil))

(defun transient-cycles-window-buffers-back-and-forth ()
  "Switch to the buffer most recently accessed using the bindings
established by `transient-cycles-window-buffers-mode', on the
condition that no other commands have set this window's buffer
since then.  Otherwise, call `previous-buffer'."
  (interactive)
  (cl-symbol-macrolet
      ((param (window-parameter nil 'transient-cycles--window-recent-buffer)))
    (cond (param
	   (let ((new (and (window-next-buffers) (* -1 param))))
	     (cond ((cl-plusp param) (next-buffer param))
		   ((cl-minusp param) (previous-buffer (* -1 param))))
	     (setq param new)))
	  ((window-next-buffers)
	   (let ((count (length (window-next-buffers))))
	     (next-buffer count)
	     (setq param (* -1 count))))
	  (t (previous-buffer)))))

(defvar transient-cycles-tab-bar-mode-map (make-sparse-keymap)
  "Keymap for `transient-cycles-tab-bar-mode'.")

;;;###autoload
(define-minor-mode transient-cycles-tab-bar-mode
  "Enhance tab switching commands by adding transient cycling.

Augments \\[tab-previous], \\[tab-next] and
\\[tab-bar-switch-to-recent-tab].  After running those commands,
you can use `transient-cycles-tab-bar-cycle-backwards-key' and
`transient-cycles-tab-bar-cycle-forwards-key' to move forwards
and backwards in the list of tabs.  When transient cycling
completes, tab access times will be as though you had moved
directly from the first tab to the final tab.  That means that
\\[tab-bar-switch-to-recent-tab] may be used to switch back and
forth between the first tab and the final tab."
  :lighter nil :keymap transient-cycles-tab-bar-mode-map :global t
  :group 'transient-cycles)

(defcustom transient-cycles-tab-bar-cycle-backwards-key [left]
  "Key to cycle backwards in the transient maps set by commands
defined by `transient-cycles-tab-bar-mode'."
  :type 'key-sequence
  :group 'transient-cycles)

(defcustom transient-cycles-tab-bar-cycle-forwards-key [right]
  "Key to cycle forwards in the transient maps set by commands
defined by `transient-cycles-tab-bar-mode'."
  :type 'key-sequence
  :group 'transient-cycles)

(transient-cycles-define-commands (recent-tab-old-time)
  (([remap tab-previous] (count)
     (setq recent-tab-old-time (transient-cycles--nth-tab-time (* -1 count)))
     (tab-previous count))

   ;; `tab-bar-switch-to-recent-tab' does not have a binding by default but
   ;; establish a remapping so that the user can easily access the transient
   ;; cycling variant simply by adding a binding for the original command.
   ([remap tab-bar-switch-to-recent-tab] (count)
     (setq recent-tab-old-time
	   (cl-loop for tab in (funcall tab-bar-tabs-function)
		 unless (eq 'current-tab (car tab))
		 minimize (cdr (assq 'time tab))))
     (tab-bar-switch-to-recent-tab count))

   ([remap tab-next] (count)
     (setq recent-tab-old-time (transient-cycles--nth-tab-time count))
     (tab-next count)))

  (lambda (_ignore)
    (lambda (count)
      ;; We are moving away from the current tab, so restore its time as if
      ;; we had never selected it, and store the time of the tab we're
      ;; moving to in case we need to do this again.
      (let ((next-tab-old-time (transient-cycles--nth-tab-time count)))
	(tab-bar-switch-to-next-tab count)
	(cl-loop with max
	      for tab in (funcall tab-bar-tabs-function)
	      for tab-time = (assq 'time tab)
	      when (and (not (eq 'current-tab (car tab)))
			(or (not max) (> (cdr tab-time) (cdr max))))
	      do (setq max tab-time)
	      finally (setcdr max recent-tab-old-time))
	(setq recent-tab-old-time next-tab-old-time))))
  :keymap transient-cycles-tab-bar-mode-map
  :cycle-forwards-key transient-cycles-tab-bar-cycle-forwards-key
  :cycle-backwards-key transient-cycles-tab-bar-cycle-backwards-key)

(defun transient-cycles--nth-tab-time (n)
  (let* ((tabs (funcall tab-bar-tabs-function))
	 (current-index (cl-position 'current-tab tabs :key #'car))
	 (new-index (mod (+ n current-index) (length tabs))))
    (alist-get 'time (nth new-index tabs))))


;;;; Shells

(defcustom transient-cycles-shell-command 'shell
  "Command to start your preferred transient cycling shell."
  :type '(choice (const :tag "`shell-mode'" shell)
		 (const :tag "Eshell" eshell))
  :group 'transient-cycles)

(defvar eshell-last-output-end)
(declare-function eshell-quote-argument "esh-arg")
(declare-function eshell-send-input "esh-mode")
(declare-function comint-send-input "comint")

;; We could have an optional argument to kill any input and reinsert it after
;; running the command, and even restore point within that input.
;; Might be useful in `transient-cycles-shells-jump' & interactively.
(defun transient-cycles-shells-insert-and-send (&rest args)
  (let ((args (if (cdr args)
		  (cl-ecase transient-cycles-shell-command
		    (eshell (string-join (mapcar #'eshell-quote-argument args)
					 " "))
		    (shell (combine-and-quote-strings args)))
		(car args))))
    (cl-ecase transient-cycles-shell-command
      (eshell (delete-region eshell-last-output-end (point-max))
	      (when (> eshell-last-output-end (point))
		(goto-char eshell-last-output-end))
	      (insert-and-inherit args)
	      (eshell-send-input))
      (shell (if-let* ((process (get-buffer-process (current-buffer))))
		 (progn (goto-char (process-mark process))
			(delete-region (point) (point-max))
			(insert args)
			(comint-send-input))
	       (user-error "Current buffer has no process"))))))

(defvar comint-prompt-regexp)
(defvar eshell-buffer-name)
(declare-function dired-current-directory "dired")
(declare-function project-root "project")

(defun transient-cycles-shells-jump (&optional chdir busy-okay)
  "Pop to a recently-used shell that isn't busy, or start a fresh one.
Return a ring for transient cycling among other shells, in the order of most
recent use.  A shell is busy if there's a command running, or it's narrowed
(in the latter case for Eshell, this was probably done with C-u C-c C-r).
When BUSY-OKAY is `interactive', a shell is additionally considered busy
when there is a partially-entered command.

Non-nil CHDIR requests a shell that's related to `default-directory'.
Specifically, if CHDIR is non-nil, pop to a shell in `default-directory',
pop to a shell under the current project root and change its directory to
`default-directory', or start a fresh shell in `default-directory'.
If CHDIR is `project', use the current project root as `default-directory'.
In `dired-mode', unless CHDIR is `strict', use the result of calling
`dired-current-directory' as `default-directory'.

Non-nil BUSY-OKAY requests ignoring whether shells are busy.  This makes
it easy to return to shells with long-running commands.
If BUSY-OKAY is `interactive', as it is interactively, ignore whether shells
are busy unless there is a prefix argument, and unconditionally start a fresh
shell if the prefix argument is 16 or greater (e.g. with C-u C-u).
If BUSY-OKAY is `fresh', unconditionally start a fresh shell, whether or not
a shell that isn't busy already exists.
Any other non-nil value means to ignore whether shells are busy.

If BUSY-OKAY is `interactive', `this-command' is equal to `last-command',
and there is no prefix argument, set the prefix argument to the numeric
value of the last prefix argument multiplied by 4, and also bind
`display-buffer-overriding-action' to use the selected window.
Thus, M-& M-& is equivalent to M-& C-u M-&, and M-& M-& M-& is equivalent to
M-& C-u M-& C-u C-u M-&.  This streamlines the case where this command takes
you to a buffer that's busy but you need one that isn't, but note that with
the current implementation transient cycling is restarted, so the busy buffer
will become the most recently selected buffer.

Some ideas behind these behaviours are as follows.

- Just like Lisp REPLs, we do not normally need a lot of different shells;
  it is fine for shell history associated with different tasks to become
  mixed together.  But we do require an easy way to start new shells when
  other shells are already busy running commands.

- Rename *shell* to *shell*<N>, but don't ever rename *shell*<N> back to
  *shell*, because that is a conventional workflow -- stock Emacs's M-&,
  C-h i, M-x ielm, M-x compile etc. always take you to the unnumbered buffer,
  possibly renaming the numbered one out of the way.

  We do nevertheless reuse shells, not for the sake of creating fewer, but
  just so that this command can be used to get back to the most recent few
  shells you were working in, to see output.

- We'll sometimes use C-x 4 1 in front of this command, and if we're
  already in a shell, we might use C-x 4 4 C-x <left>/<right> to cycle to
  another shell in another window, or a sequence like M-& C-u M-&, which
  doesn't bind `display-buffer-overriding-action'.

- It's not especially convenient to distinguish between `project-shell'
  and `shell' shells.  We just want a way to quickly obtain a shell in
  the project root, and bind that to C-x p s.

- Except when `this-command' is equal to `last-command', don't do anything
  special when the current buffer is the one we'd pop to, as previous
  versions of this command did.  That sort of context-dependent behavioural
  variation reduces the speed with which one can use the command because
  you have to think more about what it will do."
  (interactive '(nil interactive))
  (let* ((default-directory (or (and (not (eq chdir 'strict))
				     (derived-mode-p 'dired-mode)
				     (dired-current-directory))
				default-directory))
	 (current-project (and (not (file-remote-p default-directory))
			       (project-current)))
	 (proj-root (and current-project (project-root current-project)))
	 (target-directory (expand-file-name (or (and (eq chdir 'project)
						      proj-root)
						 default-directory)))
	 (again (and (not current-prefix-arg)
		     (eq busy-okay 'interactive)
		     (eq this-command last-command)))
	 (display-buffer-overriding-action
	  (if again '(display-buffer-same-window (inhibit-same-window . nil))
	    display-buffer-overriding-action))
	 (orig-busy-okay busy-okay)
	 (mode (cl-ecase transient-cycles-shell-command
		 (eshell 'eshell-mode)
		 (shell 'shell-mode)))
	 target-directory-shells other-shells
	 most-recent-shell same-project-shell target-directory-shell)
    ;; It's important that `transient-cycles-cmd-transient-cycles-shells-jump'
    ;; never sees this prefix argument because it has its own meaning for C-u.
    ;; This means that C-u M-! and M-! M-! are different, which is desirable.
    ;;
    ;; We could multiply by 16 if `last-prefix-arg' is nil and the current
    ;; buffer is a shell that's not busy.  The idea would be that when M-&
    ;; takes us to a non-busy buffer, a second M-& would only take us to the
    ;; same buffer, so skip over that step and do C-u C-u M-&.
    ;; However, this simpler design has the advantage that if I know I want a
    ;; non-busy shell I can just hit M-& M-& without looking and I know I'll
    ;; get the most recent non-busy shell in the right directory.
    (when again
      (setq current-prefix-arg (* 4 (prefix-numeric-value last-prefix-arg))))
    (when (eq orig-busy-okay 'interactive)
      (setq busy-okay (cond ((>= (prefix-numeric-value current-prefix-arg) 16)
			     'fresh)
			    ((not current-prefix-arg)
			     t))))
    (cl-flet
	((busy-p (buffer)
	   (cl-ecase transient-cycles-shell-command
	     (eshell
	      (or (get-buffer-process buffer)
		  (with-current-buffer buffer
		    (or (buffer-narrowed-p)
			(and (eq orig-busy-okay 'interactive)
			     (> (point-max) eshell-last-output-end))))))
	     (shell
	      (with-current-buffer buffer
		(save-excursion
		  (or (buffer-narrowed-p)
		      (not (get-buffer-process buffer))
		      (let ((pmark (process-mark
				    (get-buffer-process buffer))))
			(goto-char pmark)
			(forward-line 0)
			;; We can't rely on fields because of the case
			;; where a running process has output (part of) a
			;; line without (yet) a trailing newline.
			;; There's no way to distinguish that from a shell
			;; prompt without doing a regexp match.
			(not
			 (and (re-search-forward comint-prompt-regexp pmark t)
			      (or (not (eq orig-busy-okay 'interactive))
				  (eobp)))))))))))
	 (fresh-shell ()
	   (when-let* ((buffer (get-buffer
				(cl-ecase transient-cycles-shell-command
				  (eshell (require 'eshell)
					  eshell-buffer-name)
				  (shell "*shell*")))))
	     (with-current-buffer buffer (rename-uniquely)))
	   (let ((default-directory (if chdir
					target-directory
				      (expand-file-name "~/"))))
	     (funcall transient-cycles-shell-command))))
      (dolist (buffer (buffer-list))
	(with-current-buffer buffer
	  ;; We previously used `derived-mode-p' here.
	  ;; Accepting only MODE has the advantage of excluding buffers not
	  ;; created by us which happen to use derivatives of MODE but which
	  ;; can't immediately be used as general purpose interactive shells.
	  ;; If this proves too simple we could have a defcustom to switch
	  ;; back to using `derived-mode-p' or to supply a custom predicate.
	  (when (eq mode major-mode)
	    (let ((in-target-p (and chdir (equal default-directory
						 target-directory))))
	      (push buffer
		    (if in-target-p target-directory-shells other-shells))
	      (cond ((and (not chdir)
			  (not most-recent-shell)
			  (or busy-okay (not (busy-p buffer))))
		     (setq most-recent-shell buffer))
		    ((and in-target-p
			  (not target-directory-shell)
			  (or busy-okay (not (busy-p buffer))))
		     (setq target-directory-shell buffer))
		    ((and chdir proj-root
			  (not same-project-shell)
			  ;; We'll change its directory so it mustn't be busy.
			  (not (busy-p buffer))
			  (file-in-directory-p default-directory proj-root))
		     (setq same-project-shell buffer)))))))
      (cond ((eq busy-okay 'fresh)
	     (fresh-shell))
	    ((and chdir target-directory-shell)
	     (pop-to-buffer target-directory-shell))
	    ((and chdir same-project-shell)
	     (pop-to-buffer same-project-shell)
	     (transient-cycles-shells-insert-and-send "cd" target-directory))
	    (most-recent-shell		; CHDIR nil
	     (pop-to-buffer most-recent-shell))
	    (t
	     (fresh-shell))))
    ;; In an interactive call where we specifically requested a shell that's
    ;; not busy, ensure it's ready for us to enter a command.
    ;; Otherwise, it's useful to be able to jump back to exactly where we were
    ;; in a shell, and when called from Lisp, let the caller decide what to
    ;; do about where we are in the buffer, and about any partially-entered
    ;; command (e.g. see `transient-cycles-shells-dired-copy-filename').
    (when (and (not busy-okay) (eq orig-busy-okay 'interactive))
      (goto-char (point-max)))
    (let* ((all (delq (current-buffer)
		      (nconc other-shells target-directory-shells)))
	   (ring (make-ring (1+ (length all)))))
      (dolist (buffer all)
	(ring-insert ring buffer))
      (ring-insert ring (current-buffer))
      ring)))
(put 'transient-cycles-shells-jump 'project-aware t)

(declare-function dired-get-marked-files "dired")
(declare-function dired-get-subdir "dired")

(defun transient-cycles-shells-dired-copy-filename (&optional arg)
  "Like `dired-copy-filename-as-kill' but copy file names to a shell buffer.
This is instead of prompting for the command in the minibuffer.
See `transient-cycles-shells-mode'."
  (interactive "P")
  (let* ((subdir (dired-get-subdir))
	 (files
          ;; We treat as primary the meanings of the prefix argument to
	  ;; `dired-copy-filename-as-kill', then try to call
	  ;; `transient-cycles-shells-jump' in a way that corresponds.
	  ;; Thus, there isn't a way to express a prefix argument to M-&,
	  ;; but can use, e.g., C-u C-u M-& C-x o &.
	  ;; (It wouldn't make sense to pass a prefix argument to M-!.)
	  ;;
	  ;; Invoking with \\`!', and no prefix argument, is a shortcut for
	  ;; copying absolute paths, and behaving more like M-! than M-&.
	  (cond (subdir
		 (transient-cycles-shells-jump)
		 (ensure-list subdir))
		((if arg
		     (eql 0 arg)
		   (char-equal last-command-event ?!))
		 (prog1 (dired-get-marked-files)
		   (transient-cycles-shells-jump)))
		((eql 1 arg)
		 ;; Don't call `project-current' in order to ensure we behave
		 ;; just the same as `transient-cycles-shells-jump' when there
		 ;; is no current project, without repeating its logic here.
		 (cl-loop with files = (dired-get-marked-files)
			  initially (transient-cycles-shells-jump 'project)
			  for file in files
			  collect (file-relative-name file
						      default-directory)))
                ((consp arg)
                 (prog1 (dired-get-marked-files t)
		   (transient-cycles-shells-jump 'strict)))
                (t
		 (prog1
		     (dired-get-marked-files 'no-dir
					     (and arg
						  (prefix-numeric-value arg)))
		   (transient-cycles-shells-jump t)))))
	 (string (mapconcat (lambda (file)
			      (if (string-match-p "[ \"']" file)
				  (format "%S" file)
				file))
			    files
			    " ")))
    (unless (string-empty-p string)
      (let* ((pmark (cl-ecase transient-cycles-shell-command
		      (eshell eshell-last-output-end)
		      (shell
		       (if-let* ((proc (get-buffer-process (current-buffer))))
			   (process-mark proc)
			 (user-error "Current buffer has no process")))))
	     (empty-p (= pmark (point-max))))
	;; If we're somewhere else in the buffer, jump to the end.
	;; This means that if you want to insert the filenames into an old
	;; command you're editing, you have to C-c RET first.
	(when (> pmark (point))
	  (goto-char (point-max)))
	(save-restriction
	  (when (= pmark (point))
	    (narrow-to-region (point) (point-max)))
	  (just-one-space))
	(insert string)
	(just-one-space)
	(when empty-p
	  (goto-char pmark)
	  ;; There is now also `shell-command-guess'.
	  ;; (when-let* ((default (dired-guess-default files)))
	  ;;   (if (listp default)
	  ;; 	(let ((completion-at-point-functions
	  ;; 	       (list (lambda () (list (point) (point) default)))))
	  ;; 	  (completion-at-point))
	  ;;     (insert default)))
	  )))))

(defvar transient-cycles-shells-mode-map (make-sparse-keymap)
  "Keymap for `transient-cycles-shells-mode'.")

(defvar dired-mode-map)
(declare-function dired-do-shell-command "dired-aux")
(declare-function dired-do-async-shell-command "dired-aux")

;;;###autoload
(define-minor-mode transient-cycles-shells-mode
  "Replace system shell commands with transient cycling of shell buffers.

Augments \\[project-shell] (or \\[project-eshell]) and completely replaces \\`M-!', \\`M-&',
and Dired's \\`!' and \\`&' commands (but not \\`X').  These commands now all
switch to shell buffers instead of doing minibuffer prompting.

\\`M-!' always switches back to the most recently used shell.
\\`M-&' switches to a shell in the current `default-directory'.
\\[project-shell]/\\[project-eshell] are like \\`M-&' but using the project root.

For \\[project-shell]/\\[project-eshell], \\`M-!' and \\`M-&', type the command twice in a row
to skip over shell buffers already occupied by running commands.
Type the command a third time for a newly created shell buffer.

In addition, after running those commands, you can use
`transient-cycles-shells-cycle-backwards-key' and
`transient-cycles-shells-cycle-forwards-key' to select a different shell
buffer instead.

See also `transient-cycles-cmd-transient-cycles-siblings-from-here'."
  :lighter nil :keymap transient-cycles-shells-mode-map :global t
  :group 'transient-cycles
  ;; We need to bind into `project-prefix-map', rather than adding a remap
  ;; to our own minor mode map, so that we have the command under C-x 4 p,
  ;; C-x 5 p and C-x t p too.
  (require 'project)
  (cl-ecase transient-cycles-shell-command
    (eshell
     (define-key project-prefix-map "e"
		 (if transient-cycles-shells-mode
		     #'transient-cycles-cmd-transient-cycles-shells-project
		   #'project-eshell)))
    (shell
     (define-key project-prefix-map "s"
		 (if transient-cycles-shells-mode
		     #'transient-cycles-cmd-transient-cycles-shells-project
		   #'project-shell))))

  ;; Note that \\`X' remains bound to `dired-do-shell-command', and adding a
  ;; \\`&' to the end of the input gets you `dired-do-async-shell-command'.
  (require 'dired)
  (define-key dired-mode-map "!"
	      (if transient-cycles-shells-mode
		  #'transient-cycles-shells-dired-copy-filename
		#'dired-do-shell-command))
  (define-key dired-mode-map "&"
	      (if transient-cycles-shells-mode
		  #'transient-cycles-shells-dired-copy-filename
		#'dired-do-async-shell-command)))

(defcustom transient-cycles-shells-cycle-backwards-key [left]
  "Key to cycle backwards in the transient maps set by commands
defined by `transient-cycles-shells-mode'."
  :type 'key-sequence
  :group 'transient-cycles)

(defcustom transient-cycles-shells-cycle-forwards-key [right]
  "Key to cycle forwards in the transient maps set by commands
defined by `transient-cycles-shells-mode'."
  :type 'key-sequence
  :group 'transient-cycles)

(transient-cycles-define-buffer-switch
  ((("\M-!" . transient-cycles-shells-jump) (arg)
     (interactive "p")
     (cl-ecase transient-cycles-shell-command
       (eshell
	(let ((>>> (and (> arg 1) (format " >>>#<buffer %s>" (buffer-name)))))
	  (prog1 (transient-cycles-shells-jump (> arg 4)
					       (and (= arg 1) 'interactive))
	    (when >>>
	      (let ((there (save-excursion
			     (goto-char (point-max))
			     (skip-syntax-backward "\\s-")
			     (- (point) (length >>>)))))
		(unless (and (>= there 0)
			     (equal >>> (buffer-substring there (point-max))))
		  (save-excursion
		    (goto-char (point-max))
		    (insert >>>)
		    (backward-char (length >>>))
		    (when (> (point) eshell-last-output-end)
		      (just-one-space)))))))))
       (shell
	(if (> arg 1)
	    (progn (call-interactively #'shell-command)
		   nil)			; disable transient cycling
	  (transient-cycles-shells-jump nil 'interactive)))))
   (("\M-&" . transient-cycles-shells-jump-from-here) ()
     (interactive)
     (transient-cycles-shells-jump t 'interactive)))
  :keymap transient-cycles-shells-mode-map
  :cycle-forwards-key transient-cycles-shells-cycle-forwards-key
  :cycle-backwards-key transient-cycles-shells-cycle-backwards-key)

(transient-cycles-define-buffer-switch
  ((transient-cycles-shells-project ()
     (interactive)
     (prog1 (transient-cycles-shells-jump 'project 'interactive)
       ;; Make it possible to use M-& to repeat C-x p s / C-x p e.
       (let ((map (make-sparse-keymap)))
	 (define-key map "\M-&"
		     #'transient-cycles-cmd-transient-cycles-shells-project)
	 (set-transient-map map)))))
  :cycle-forwards-key transient-cycles-shells-cycle-forwards-key
  :cycle-backwards-key transient-cycles-shells-cycle-backwards-key)
(put 'transient-cycles-cmd-transient-cycles-shells-project 'project-aware t)

(provide 'transient-cycles)

;;; transient-cycles.el ends here
