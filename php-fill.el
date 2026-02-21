;;; php-fill.el --- Additional fill commands for PHP code editing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc

;; Author: Ariel Del Valle Lozano <arielmazatlan@gmail.com>
;; Maintainer: Ariel Del Valle Lozano <arielmazatlan@gmail.com>
;; URL: https://github.com/arielenter/php-fill.el
;; Version: 1.1.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: php, languages, tools, convenience

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Additional fill commands for PHP code editing.

;; - Fills string literals by breaking them into smaller ones.
;; - Conditionally use ‘NOSQUEEZE’ on c and c++ style comments.
;; - Use “<return>” to break or add a new line to string literals and
;;   doc blocks.
;; - Use “<backspace>” or “<delete>” at the beginning and at the end to
;;   join them.
;; - Minor mode Php-Refill.

;; Minimum suggested inclusion code with ‘php-mode’:

;; (use-package php-fill
;;   :requires 'php-mode
;;   :hook
;;   (php-mode . php-fill-set-local-variables)
;;   (php-mode . php-fill-refill-mode)
;;   (php-mode . display-fill-column-indicator-mode)
;;   :bind
;;   (:map php-mode-map
;;   	("M-q" . php-fill-paragraph)
;;   	("<return>" . php-fill-newline)
;;   	("C-<return>" . newline)
;;   	("<backspace>" . php-fill-backward-delete)
;;   	("<delete>" . php-fill-delete-forward)))

;;; News:
;; Version 1.1.1

;; - Major refractory.
;; - SQUEEZE typo fixed.
;; - Additional information about what ‘squeeze’ means in the context of
;;   fill prefixed functions.

;; Version 1.1.0

;; - Customizable variable `php-fill-refill-black-list' added, so that
;;   if a command that conflicts with `php-fill-refill-mode' is found,
;;   that command can be added by the user.

;;; Code:

(require 'cc-engine)
(require 'cc-cmds)

(defgroup php-fill nil
  "Customization options for package php-fill."
  :group 'php
  :group 'languages
  :group 'tools
  :group 'convenience)

(defcustom php-fill-nosqueeze-c-comments t
  "\\[php-fill-c-fill-paragraph] decides whether or not to use\n\
\\[php-fill-c-fill-paragraph-nosqueeze] on c style comments depending on
the value of this variable.

A t default value was chosen so that Phpdoc blocks (which are c style
comments) using spaces to alight columns (like when using the '@param'
tag), would not loose said alignment if \\[php-fill-paragraph] or
\\[php-fill-c-fill-paragraph] are used on them.  If this way of
formatting doc blocks values isn't used, setting a nil value might be
preferred.

In the context of the fill prefixed functions, a ‘squeeze’ refers to the
action of replacing instances where multiple space between words exist
with a single one.

See command \\[php-fill-c-fill-paragraph-nosqueeze] for additional
information."
  :type 'boolean
  :group 'php-fill)

(defcustom php-fill-nosqueeze-c++-comments nil
  "\\[php-fill-c-fill-paragraph] decides whether or not to use\n\
\\[php-fill-c-fill-paragraph-nosqueeze] on c++ style comments depending
on the value of this variables."
  :type 'boolean
  :group 'php-fill)

(defcustom php-fill-refill-black-list
  '(comment-or-uncomment-region comment-region uncomment-region)
  "List of commands whose changes should not trigger a refill.

An example are while commenting and un-commenting code, since filling
said code while is commented will make it loose its format, which is
more likely unwanted.

Feel free to add any other command that you find problematic to trigger
a refill."
  :type '(repeat symbol)
  :group 'php-fill)

(defcustom php-fill-sentence-end-double-space nil
  "Determines what local value `sentence-end-double-space' will \n\
received from command \\[php-fill-set-local-variables] on the current
buffer."
  :type 'boolean
  :group 'php-fill)

(defcustom php-fill-fill-column 80
  "Determines what local value `fill-column' will received from command\n\
\\[php-fill-set-local-variables] on the current buffer.

It's important to say that this customizable variable doesn't replace
`fill-column'.  `fill-column' it's still used as the value look up to
fill any content.  The purpose of this variable is to ease the process
of setting up a default `fill-column' for PHP files, through the use of
the command `php-fill-set-local-variables' and a hook to a major mode
like the ones provided by packages ‘php-mode’ or ‘phps-mode’."
  :type 'integer
  :group 'php-fill)

(defun php-fill-c-fill-paragraph (&optional arg)
  "Use two variables to determine to use \\[c-fill-paragraph] or\n\
\\[php-fill-c-fill-paragraph-nosqueeze] on current comment.

This two are `php-fill-nosqueeze-c-comments' for the c style commends
and `php-fill-nosqueeze-c++-comments' for the c++ style ones.

As an example, if pointer is located within a C++ styled comment and
`php-fill-nosqueeze-c++-comments' is not nil, then command
\\[php-fill-c-fill-paragraph-nosqueeze] will be used to fill it.
Otherwise if `php-fill-nosqueeze-c++-comments' were to be nil, then
`c-fill-paragraph' would be used on its place.

In the context of the fill prefixed functions, a ‘squeeze’ refers to the
action of replacing instances where multiple space between words exist
with a single one.

ARG attribute is pass down to `c-fill-paragraph' if it ends up being
called.

WARNING: If optional prefix ARG is not nil, then `c-fill-paragraph' will
be used regardless of variables `php-fill-nosqueeze-c-comments' and
`php-fill-nosqueeze-c++-comments''s values, since a non nil ARG value
tells `c-fill-paragraph' to use full justification on the comments
being filled, which ends up making NOSQUEEZE irrelevant."
  (interactive "*P")
  (let ((literal-type (car (php-fill-get-literal))))
    (if (and (not arg)
	     (or (and (equal literal-type 'c)
		      php-fill-nosqueeze-c-comments)
		 (and (equal literal-type 'c++)
		      php-fill-nosqueeze-c++-comments)))
	(php-fill-c-fill-paragraph-nosqueeze)
      (c-fill-paragraph arg))))

(defun php-fill-paragraph (&optional only-one-line-up)
  "Like \\[c-fill-paragraph] but string literals are handled in a\n\
different way (tailor made for PHP) and a small customizable option was
added for comments.

String literals are handled using command \\[php-fill-string-literal].

In the case of comments, two global variables decide whether or not the
argument NOSQUEEZE of function `fill-delete-newlines' will be used either
on C or C++ style comments or both.  See command
\\[php-fill-c-fill-paragraph] for additional information.

In the context of the fill prefixed functions, a ‘squeeze’ refers to the
action of replacing instances where multiple space between words exist
with a single one.

Optional argument ONLY-ONE-LINE-UP will be passed down to
`php-fill-string-literal'."
  (interactive)
  (let* ((php-fill-literal (php-fill-get-literal))
	 (literal-limits (car php-fill-literal)))
    (if (or (not literal-limits) (equal literal-limits 'string))
	(php-fill-string-literal only-one-line-up)
      (call-interactively 'php-fill-c-fill-paragraph))))

(defun php-fill-get-literal ()
  "Return both the literal type and the positions of its delimiters as a\n\
con.

This code is taken from `c-mask-paragraph' with little changes.

Since this values may be requested more than once on the same context by
different functions, running the complete process again may be
unnecessary.  With this in mind, a method to avoid this was created that
consist on declaring variable `php-fill-literal' non globally using
`let' and assigning to it the returned value of a called of this
function.  This way, if this function is run again in the same
context (within the aforementioned `let'), it'll simply return the
already established value of the variable `php-fill-literal'."
  (if (boundp 'php-fill-literal)
      php-fill-literal
    (c-save-buffer-state ()
      (save-restriction
	;; Widen to catch limits correctly.
	(widen)
	(let ((literal-limits (c-literal-limits nil nil)))
	  (cons (c-literal-type literal-limits) literal-limits))))))

(defun php-fill-string-literal (&optional only-one-line-up)
  "Fill PHP's quoted string literals by breaking them into smaller ones.

Works similarly to how functions like `fill-paragraph' and
`c-fill-paragraph' break lines of text after they have passed the
`current-fill-column', but while `c-fill-paragraph' breaks the lines of
text within the confinements of the string literal's content, keeping
it's delimiters and simplify adding newlines to it, this function
instead breaks long quoted string literals (either single or double
quoted) into smaller ones, one per line, if said string literal is
located at the end of a line.  This is done using word wrap rules,
meaning that string literals are not broken in the middle of words, but
right after the closet space preceding a word.  Each subsequent line
will start with a dot and a space, representing the concatenation of the
smaller string literals.  Indentation is done using the command
\\[indent-according-to-mode], for this reason, it's highly recommended
to use some PHP dedicated major mode like the ones offer by packages
‘php-mode’ and ‘phps-mode’.

It isn't necessary to have the pointer inside the string literal located
at the end of the line, neither at any of its subsequent parts, to have
said string structure filled.  As long as the pointer is located in a
line ending on a string literal, that string literal would become the
subject of the fill process.  A line is considered to end on a string
literal only if both string delimiting quotes are located on the same
line, meaning string literals containing one or more literal line breaks
are not supported.  Escape sequence line breaks ‘\\n’ are fine.  It
might also be pertinent to mention that neither ‘heredoc’ nor ‘nowdoc’
string literal PHP's syntax are supported, only quoted (either single or
double) strings are.

The objective is that none of the content of the lines should over pass
the `current-fill-column' columns, but it's worth pointing out that the
length of a line doesn't depend exclusively of the string literal's
content.  It is however, the only part that would be looked to be broken
apart.  The rest of the line content like the indentation, any content
before the string literal, the string literal's delimiting quotes and
possible statement ending semicolon or an array value separation coma
ending the line, are part of what establishes the length of the line
holding the string literal, but they will not be touch in any way.  So
even if the string literal content doesn't cross over the
`current-fill-column', if at least its ending quote, a a semicolon, or a
coma ending the line do, the string literal will be looked to be broken
just the same.  Another interesting scenario is what happens if the rest
of the line content before the string literal pushes the beginning of
this last mentioned, too close or even passing `current-fill-column',
since as aforementioned before, words aren't cut in the middle.  In an
scenario as described before, this command will look to break the string
content at the closest suitable breakable place from the beginning of
said string.

Another caveat is that (commonly) ‘fill’ prefixed functions trim white
spaces on both left and right ends, but that won't be done to the string
literal content.  Also multiple spaces between words are normally
‘squeezed’ into just one, but this won't be done either.  In fact, in
order for a dedicated string literal concatenating line to be considered
part of the previous one for the fill process, the previous string
content must end on a space.

Concatenated string literals are considered part of the same structure
only if they used the same quote type (double or single quotes), and
only if a concatenated string literal covers the entire line, meaning,
if a line contains more than one string literal being concatenated, they
won't be considered as part of the same string in the fill process.

If two string literal shared a line, for instance on a concatenation or
when array values are being defined, only last string literal will be
filled.

Optional argument ONLY-ONE-LINE-UP value will be passed down to command
\\[php-fill-stitch-string-parts-together] as is."
  (interactive)
  (when (php-fill-supported-string-literal-in-line)
    (php-fill-stitch-string-parts-together only-one-line-up)
    (php-fill-break-long-string-literal-apart)))

(rx-let ((anything-before-string-literal (: (*? nonl) (or (not ?\\) bol)))
	 ;; Matches \\, \" and \\\" but not \\" nor \\\\"
	 (scaped-sequence (: ?\\ nonl))
	 (scaped-sqnc-or-not-a-quote
	  (quote) (or scaped-sequence (not (or quote "\n" ?\\))))
	 (scaped-sqnc-or-not-a-quote-nor-a-space
	  (quote) (or scaped-sequence (not (or quote "\n" ?\\ " "))))
	 (string-literal-with-any-or-no-content
	  (quote) (: quote (* (scaped-sqnc-or-not-a-quote quote)) quote))
	 (quote-version (quote)
			(string-literal-with-any-or-no-content quote))
	 (either-version (or (quote-version ?') (quote-version ?\")))
	 (may-or-maynot-end-a-statement-or-an-array-value
	  (: (? (in ",;")) eol))
	 (breakable-string-literal
	  (quote) (: (group-n 101 quote)
		     (* (scaped-sqnc-or-not-a-quote quote)) " "
		     (+ (scaped-sqnc-or-not-a-quote-nor-a-space quote))
		     (* " ") quote))
	 (concatenation (: (* (in " \t")) ". "))
	 (two-lines-concatenation
	  (quote) (: quote (*? (scaped-sqnc-or-not-a-quote quote)) " "
		     (group-n 100 quote "\n" concatenation quote)
		     (* (scaped-sqnc-or-not-a-quote quote)) quote)))
  
  (defconst php-fill-line-ends-with-a-string-literal-regexp
    (rx anything-before-string-literal either-version
	may-or-maynot-end-a-statement-or-an-array-value)
    "Looking at a line ending on a string literal with any or no content.

When looking from the beginning of a line, a match of this regular
expression will indicate that the current line ends in a quoted string
literal with any or no content, regardless of how the rest of the line
starts with or if the string literal occupies the entire line. After
said string literal, there may or may not be either a semicolon or a
coma, representing an statement end or part of an array values
assignment, but nothing more, only the end of the line.")

  (rx-let ((quote-version (quote) (breakable-string-literal quote)))
    (defconst php-fill-line-ends-with-breakable-str-regexp
      (rx anything-before-string-literal either-version
	  may-or-maynot-end-a-statement-or-an-array-value)
      "Looking at a line ending on a breakable string literal.

When looking from the beginning of a line, a match of this regular
expression will indicate that the current line ends with a breakable
quoted string literal, regardless of how the line starts with or if the
string literal occupies the entire line. After said string literal,
there may or may not be either a semicolon or a coma, representing an
statement end or part of an array values assignment, but nothing more,
only the end of the line.

In the context of this package, a string literal is considered breakable
if its content posses at least one word succeeding a space, which is the
criteria used by the function `php-fill-break-long-string-literal-apart'
to established where a string literal can be broken when the content of
the line holding it over pass the `current-fill-column' position.
Considering this, a string literal can't be broken by
`php-fill-break-long-string-literal-apart' if there isn't a word that
succeeds a space."))

  (rx-let ((quote-version (quote) (two-lines-concatenation quote)))
    (defconst php-fill-two-lines-concatenation-regexp
      (rx anything-before-string-literal either-version
	  may-or-maynot-end-a-statement-or-an-array-value)
      "Looking at two string literals being concatenated on two separated\n\
lines.

When looking from the beginning of a line, a match of this regular
expression will indicate that the current line ends with a quoted string
literal whose contend ends with a space, regardless of what the line
starts with or if the string literal occupies the entire line.  Said
string literal may or may not have any additional content.  What follows
is a new line, and said line is dedicated to a string concatenation of a
secondary string literal that uses the exact same quote type than the
first one.  The contend of this second line is a possible indentation,
followed by a dot, one space, and a string literal that covers the rest
of the line.  A possible semicolon or a coma may follow, but nothing
else except for the end of the line.  The second string literal content
does not matter, it can even be empty.

This regexp establishes the requirements that function
`php-fill-stitch-two-lines-concatenation' uses to stitch two string
literals together into one."))

  (defconst php-fill-string-concatenation-dedicated-line-regexp
    (rx concatenation either-version
	may-or-maynot-end-a-statement-or-an-array-value)
    "Looking at a line dedicated to a string concatenation.

When looking from the beginning of a line, a match of this regular
expression will indicate that the current line is dedicated to a string
concatenation, meaning that the line starts with a possible indentation,
followed by a dot, one space, and a quoted string literal that covers
the rest of the line.  A possible semicolon or a coma may follow, but
nothing else except for the end of the line.  The string literal content
does not matter, it can even be empty.

A string concatenation dedicated line, tells function
`php-fill-supported-string-literal-in-line' that the line might be part
of a supported `php-fill-stitch-two-lines-concatenation' string literal
concatenation, but further analysis will be needed to be sure."))

(defun php-fill-supported-string-literal-in-line ()
  "Return t if current line ends on a supported fill-able string literal\n\
candidate for command \\[php-fill-string-literal].

The reason they are yet only candidates is because some other factors
are needed to be checked later on, to be sure if they can, or more
precisely, if they need to be filled.  Some of this other factors
include, but are not limited to the lines length and possible
concatenation with other supported string literals."
  (save-restriction
    (widen)
    (let* ((literal (php-fill-get-literal)) (literal-type (car literal))
	   (literal-limits (cdr literal)))
      (and (not (memq literal-type '(c c++ pound)))
	   (or (not literal-type)
	       ;; At this point, ‘literal-type’ value would have to be
	       ;; ‘string’, in which case we should check whether or not
	       ;; current line is not inside a multi line string, since
	       ;; there is no reason to act on a code that's inside a
	       ;; string literal.  We can discard this by checking if
	       ;; the string literal, point is currently within, does
	       ;; not extend more than one line, meaning both delimiters
	       ;; should be located in the same line.
	       (php-fill-lit-limits-are-in-the-same-line literal-limits))
	   (save-excursion
	     (beginning-of-line)
	     (and (looking-at
		   php-fill-line-ends-with-a-string-literal-regexp)
		  (or (looking-at
		       php-fill-line-ends-with-breakable-str-regexp)
		      (looking-at
		       php-fill-two-lines-concatenation-regexp)
		      (looking-at
		       php-fill-string-concatenation-dedicated-line-regexp))))))))

(defun php-fill-lit-limits-are-in-the-same-line (literal-limits)
  "Check if both LITERAL-LIMITS are located in the same line."
  (= (line-number-at-pos (car literal-limits))
     (line-number-at-pos (cdr literal-limits))))

(defun php-fill-stitch-string-parts-together (&optional only-one-line-up)
  "Stitch together multiple string literals being concatenated if they\n\
meet an specified criteria.

Same way \\[c-fill-paragraph] uses `fill-delete-newlines' to unfill c
and c++ style comments so that their content can then be readjusted or
more precisely refilled, when substantial changes have been made, this
function is used to stitch together string literals concatenations that
follow the format expressed in the regular expression constant
`php-fill-two-lines-concatenation-regexp', so that they can be broken
again by command \\[php-fill-break-long-string-literal-apart] as part of
the whole fill process of the \\[php-fill-string-literal] command.

To this end, this function looks up and down from the initial point to
check what lines have the correct concatenation format to stitch string
literals together.  It's possible that there might be no string literal
concatenations up or down from point to stitch together with.

Some times, it is not needed to stitch together the entire structure of
supported string literal concatenations, if you know exactly where a
change is being made.  This is the case of function
`php-fill-refill-paragraph' that is used by minor mode
`php-fill-refill-mode', whenever a change is done.  For this cases, if a
non nil value is given to ‘ONLY-ONE-LINE-UP’ argument, this function
will only look to stitch one line up from the initial point, since it's
the only string literal up from point that may be affected from a change
done at the line where point is located.

As an interactive function, this function can be used on it's own.  In
combination with \\[php-fill-break-long-string-literal-apart] it will
pretty much end up doing what \\[php-fill-string-literal] does in a
single swift call.

One idea this command might be useful on its own will be (if
`php-fill-refill-mode' is not wanted) to use this command first, edit a
string literal as a whole (possibly using `visual-line-mode' on), and
then use \\[php-fill-break-long-string-literal-apart]."
  (interactive)
  (while (php-fill-stitch-two-lines-concatenation))
  (catch 'exit-while
    (while (php-fill-stitch-two-lines-concatenation 0)
      (when only-one-line-up (throw 'exit-while t)))))

(defun php-fill-stitch-two-lines-concatenation (&optional n pos)
  "Stitch together two string literals being concatenated if they meet\n\
an specified criteria.

This function does the heavy lifting for
\\[php-fill-stitch-string-parts-together], \\[php-fill-backward-delete]
and \\[php-fill-delete-forward] commands, when it comes to stitching
supported string literal concatenations together into one.

First it moves pointer to either the beginning of the current line or
the previous one depending on the N argument value.  Then it checks if
the two lines in front meet the requirements set by the regular
expression `php-fill-two-lines-concatenation-regexp' to get them stitch
together.

Depending on whether or not POS argument value is nil or the value of
the point position is given to it, argument POS's value will be taken
into account to stitch the two concatenated string literals together or
not."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line n)
      (if (not (and (looking-at php-fill-two-lines-concatenation-regexp)
		    (or (not pos)
			(and (= 0 n) (= (match-end 100) pos))
			(and (or (= 1 n) (not n))
			     (or (= (match-beginning 100) pos)
				 (= (- (match-beginning 100) 1) pos))))))
	  nil
	(replace-match "" nil nil nil 100) t))))

(defun php-fill-break-long-string-literal-apart ()
  "Breaks long string literals into smaller ones if they are located at\n\
the end of a line which content crosses the `current-fill-column'.

Checks if current line ends with a breakable quoted string literal and
if the line content over passes `current-fill-column', it breaks the
aforementioned string literal into smaller ones, one per line.  Each
line containing the smaller string literals will start with the PHP's
syntax to concatenate it with the previous string, meaning a dot follow
by a space, and it's content will be correctly indented according to the
major mode, for which we recommend using one for PHP like the ones
offer by packages like ‘php-mode’ and ‘phps-mode’.

This is only the last of the two steps done by
\\[php-fill-string-literal] to full refilled string literals, being the
first one \\[php-fill-stitch-string-parts-together].

All other descriptions from this package mentioned that the string
literals are broken at the end of a space that precedes a word, but this
description might not be clear enough.  In the context of this package,
a word means any amount of characters between alphanumeric, symbols, or
anything else not being a space nor the end delimiter quote of the
string literal being worked on.  It might be worth to mention that
escaped quotes are accounted for so they won't be mistaken with the
ending quote.

As an interactive function, this function can be used on it's own.  In
combination with \\[php-fill-stitch-string-parts-together] it will
pretty much end up doing what \\[php-fill-string-literal] does in a
single swift call.

One idea this command might be useful on its own will be (if
`php-fill-refill-mode' is not wanted) to use
\\[php-fill-stitch-string-parts-together] command first, edit a string
literal as a whole (possibly using `visual-line-mode' on), and then use
this command at the end.

Function `current-fill-column' was chosen to be used for no other reason
that it is the function used by command \\[fill-paragraph]."
  (interactive)
  (save-restriction
    (widen)
    (indent-according-to-mode)
    (let (quote breakable-point beg-column break-column)
      (save-excursion
	(while (and (progn (end-of-line) (> (current-column)
					    (current-fill-column)))
		    (progn (beginning-of-line)
			   (looking-at
			    php-fill-line-ends-with-breakable-str-regexp)))
	  (goto-char (match-beginning 101))
	  (setq beg-column (current-column))
	  ;; break-column has to be at least 2 characters less from
	  ;; current-fill-column to fit a space and the ending quote.
	  (setq break-column (if (> beg-column (- (current-fill-column) 2))
				 beg-column (- (current-fill-column) 2)))
          (move-to-column break-column)
	  (when (not quote) (setq quote (match-string 101))
		;; The breaking point that'll be looked for, is a space
		;; preceding any other character not being a space nor
		;; the string literal ending quote.
		(setq breakable-point (concat " [^ " quote "]")))
	  (while (and (> (current-column) beg-column)
		      (not (looking-at breakable-point)))
	    (backward-char))
	  (when (not (looking-at breakable-point))
	    (move-to-column break-column)
	    (forward-char)
	    (while (not (looking-at breakable-point))
	      (forward-char)))
	  ;; By now, break point would have to have been found (for
	  ;; example ' w') now move forward and break it.
	  (forward-char)
	  (insert quote "\n. " quote)
	  (indent-according-to-mode))))))

(defun php-fill-c-fill-paragraph-nosqueeze ()
  "Force function `fill-delete-newlines' to be called with a non nil\n\
NOSQUEEZE argument once \\[c-fill-paragraph] is called within this
function.

\\[c-fill-paragraph] uses `fill-delete-newlines' to unfill c and c++
style comments so that their content can then be re-adjusted (refilled)
when a pertinent amount of changes have been made.  While this is done,
normally if multiple spaces exist between words, they are ‘squeezed’, or
more precisely, replaced with a single one.  This might not always be
desirable.  For instance, while writing a Phpdoc block (which are c
style comments) with '@param' tags, we might want to alight their values
creating plain text columns using spaces.  In this scenario, ‘squeeze’
would destroy said alignment if command \\[c-fill-paragraph] were to be
used on its own while point is located in said doc block comment."
  (interactive)
  (unwind-protect
      (progn
	(advice-add 'fill-delete-newlines :filter-args
		    #'php-fill-delete-newlines-filter-args-advice)
	(c-fill-paragraph))
    (advice-remove
     'fill-delete-newlines
     #'php-fill-delete-newlines-filter-args-advice)))

(defun php-fill-delete-newlines-filter-args-advice (arg)
  "Argument-filter advice that replace NOSQUEEZE with a non nil value.

ARG contains all the arguments caught before function
`fill-delete-newlines' is executed.  Then NOSQUEEZE is exclusively edited
to a t value, the arguments are repackaged into a list, and finally
returned back to `fill-delete-newlines' called."
  (pcase-let ((`(,from ,to ,justify ,nosqueeze ,squeeze-after) arg))
    (setq nosqueeze t)
    (list from to justify nosqueeze squeeze-after)))

(define-minor-mode php-fill-refill-mode
  "Like \\[refill-mode] but handles C, C++ style comments, and quoted \
PHP's string literals.

For this purpose, function `php-fill-paragraph' is used."
  :init-value nil
  :lighter " Php-Refill"
  :interactive 'php-mode 'phps-mode
  :group 'php
  (if php-fill-refill-mode
      (progn
	(add-hook 'after-change-functions
		  #'php-fill-refill-after-change-function nil t)
	(add-hook 'post-command-hook
		  #'php-fill-refill-post-command-function nil t))
    (remove-hook 'after-change-functions
		 #'php-fill-refill-after-change-function t)
    (remove-hook 'post-command-hook
		 #'php-fill-refill-post-command-function t)))

(defvar-local php-fill-refill-promise-to-doit nil
  "This variable is used by `php-fill-refill-mode' to avoid doing the\n\
refill process more times that needed.

Check function `php-fill-refill-after-change-function' for additional
information.")

(defun php-fill-refill-after-change-function (_beg _end _len)
  "After-change function used by minor mode `php-fill-refill-mode'.

Notice that the refill process is not called here yet, only a promise to
do so later on by the function `php-fill-refill-post-command-function'
is set.  This allows not having to run the refill process multiple times
if the current command being run makes multiple changes.

Notice as well that changes done by an undo progress should not trigger
refill, otherwise undo won't be able to reverse back what the refill
process did right before (since it would be redone right back), or other
undesirable effects might arise.

A customizable list of commands (`php-fill-refill-black-list') exist to
establish what command's changes should not trigger a refill.

Function \\[php-fill-newline] is a special case, since we only want to
suppress the refill process if it ends up breaking a string literal in
two.  The reason is that, if the refill process were not suppressed and
if a split were to be made right after a space, the user could get
confused since it would appear as if nothing happened, since right after
the string literals is broken, it will immediately be reattached back
together by the refill process.  For this reason, \\[php-fill-newline]
would set variable `php-fill-refill-promise-to-doit' back to nil only in
the case that it ends up breaking a string literal.  This way we do not
need to suppress it on its totality, which is what would happen if we
were to added it to the `php-fill-refill-black-list' command list."
  (unless (or undo-in-progress
	      (memq this-command php-fill-refill-black-list))
    (setq php-fill-refill-promise-to-doit t)))

(defun php-fill-refill-post-command-function ()
  "Post-command function used by minor mode `php-fill-refill-mode' to do\n\
the refill process conditionally.

Uses variable `php-fill-refill-promise-to-doit' to check if a change was made
by the last called command, and if so, call the refill function then.

Notice that the `php-fill-refill-promise-to-doit' must be return back to
nil to conserved the mechanism that prevents the refill process from
being run unless a change has been made by the last command.

Check function `php-fill-refill-after-change-function' for additional
information."
  (when php-fill-refill-promise-to-doit ; there was a change
    (php-fill-refill-paragraph)
    (setq php-fill-refill-promise-to-doit nil)))

(defun php-fill-refill-paragraph ()
  "Used by `php-fill-refill-mode' to refill current comment or string\n\
literal.

Notices that the fill function won't be run if point is located at the
end of the line and the previous character is a space.  Otherwise, while
composing a comment, a refill will delete the white space that might
have just been added to it, which will just prevent adding additional
content to the comment being work on.

Notice also that command \\[php-fill-paragraph]'s argument
ONLY-ONE-LINE-UP is send with a value of t.  This will end up telling
function `php-fill-stitch-string-parts-together' that is not necessary
to stitch back all possible supported concatenation string literals
present above point's current line, only one line up is enough.  The
reason for this, is that any change made in the current line, can only
possibly affect the right before string literal fill.  See command
\\[php-fill-stitch-string-parts-together] for additional information.
This assumes you've been using `php-fill-refill-mode' from the start
while the current string structure was being created, otherwise if it
already existed, you might want to use \\[php-fill-paragraph] on its own
to get the entire structure readjust and refilled before continuing
editing it."
  (when (save-excursion (backward-char 1) (not (looking-at " $")))
    (php-fill-paragraph t)))

(defun php-fill-newline ()
  "Add a newline in a particular way, if point is located within doc\n\
block or a string literal.

If point is located inside a quoted (double or single quoted) string
literal, said string literal will be broken into two from the point this
command is called from.  The second string literal will be placed in the
next line, and it will share the same quote type used by the original.
Also, the PHP's code corresponding to the concatenation of the two,
meaning a dot and a space, will be added at the beginning of the new
line.  And finally the new line will be indented using the method
provided by the major mode.

Something to note is that string literals broken this way might get
stitch back together if command \\[php-fill-paragraph] is used on them
or if a change is made on them in minor mode `php-fill-refill-mode'.
This will happen when a string literal is broken after a space, since
it's the criteria used by the aforementioned functions to stitch string
literals together that are being concatenated.  If this is undesired,
you can make sure to split a string literal content at a place other
than a space.

Lastly, if point is located on a Phpdoc block, a new line follow by and
asterisk will be inserted and the line will be indented.  Depending if
the insertion was done in the middle of a paragraph and not at the end
of one, another line will be added, adding also and asterisk and
indenting the line, making it so that the original paragraph is split
into two paragraphs separated by and empty line."
  (interactive)
  (save-restriction
    (widen)
    (let* ((literal (php-fill-get-literal)) (literal-type (car literal))
	   (literal-limits (cdr literal)))
      (cond
       ((and (equal literal-type 'string)
	     (php-fill-lit-limits-are-in-the-same-line literal-limits)
	     (save-excursion (goto-char (car literal-limits))
			     (looking-at "['\"]")))
	(let ((quote (match-string 0)))
	  (insert quote "\n. " quote)
	  (indent-according-to-mode))
	; Avoid refill
	(setq php-fill-refill-promise-to-doit nil))
       ((and (equal literal-type 'c)
	     (save-excursion (beginning-of-line)
			     (looking-at "[ \t]*")))
	(insert "\n*")
	(indent-according-to-mode)
	(when (not (looking-at " *$"))
	  (insert "\n*")
	  (indent-according-to-mode)))
       (t (newline))))))

(defun php-fill-backward-delete ()
  "Depending on point's position, stitch the content of two strings\n\
literals or two prefixed comment lines together.

Depending if point is located at the beginning of a second quoted string
literal of two being concatenated, or at the beginning of a second line
of a c or c++ style prefixed comment that succeeds another, a backward
delete will be done in a particular way.

In the case of a supported two string literals being concatenated (see
`php-fill-stitch-two-lines-concatenation'), both string literals will be
stitch together before command \\[c-electric-backspace] is executed.

In the case of point being at the beginning of a prefixed comment
content, said prefix, meaning '#' and '//' for c++ style comments or '*'
for Phpdoc block's c style comments, including their preceding spaces
and line indentation, will be deleted first before
`c-electric-backspace' is called, ending up stitching previous comment
line with current line, word with word.  For this to happen, previous
line must also be of the same kind of comment and prefix.  See
`php-fill-stitch-together-two-comment-lines' for additional information."
  (interactive)
  (when (not (region-active-p))
    (let ((literal-type (car (php-fill-get-literal))))
      (cond
       ((memq literal-type '(c c++))
	(php-fill-stitch-together-two-comment-lines 0))
       ((php-fill-supported-string-literal-in-line)
	(php-fill-stitch-two-lines-concatenation 0 (point))))))
  (call-interactively 'c-electric-backspace))

(rx-let ((comment-start (st) (: (* (in " \t")) st))
	 (comment-version
	  (st) (: (comment-start st) (* nonl) "\n"
		  (group-n 1 (comment-start st) (* " ")) (not " ")))
	 (all-versions (or (comment-version "*") (comment-version "#")
			   (comment-version "//"))))
  
  (defun php-fill-stitch-together-two-comment-lines (&optional n)
    "Depending on point's position, stitch two prefixed comment lines\n\
together.

This function verifies if pointer is located at the beginning or at the
end (depending of the value of argument N) of two consecutive prefixed c
or c++ commends of the same kind and with the same prefix ('#', '//' or
'*').  If that's the case, stitch them together before either a backward
or forward delete is performed by either \\[php-fill-backward-delete] or
\\[php-fill-delete-forward] respectively.

The objective is to seemingly thread the content of comments as if they
were a continuous line, without indention, prefix and subsequent spaces,
working well when used in conjunction with `php-fill-refill-mode' minor
mode or even without it."
    (let ((pos (point)))
      (save-excursion
	(when (and (progn (beginning-of-line n)
			  (looking-at (rx all-versions)))
		   (or (and n (= 0 n) (= (match-end 1) pos))
		       (and (or (not n) (= 1 n))
			    (= (- (match-beginning 1) 1) pos))))
	  (replace-match "" nil nil nil 1))))))

(defun php-fill-delete-forward ()
  "Depending on point's position, stitch the content of two strings\n\
literals or two prefixed comment lines together.

Depending if point is located at the end of the content of the first
quoted string literal of two being concatenated, or at the end of the
content of the first line of a c or c++ styled and prefixed comment that
precedes another one, a backward delete will be done in a particular
way.

In the case of a supported two string literals being concatenated (see
`php-fill-stitch-two-lines-concatenation'), both string literals will be
stitch together before command \\[delete-forward-char] is called.

In the case of point being at the end of a prefixed comment that
precedes another one, the beginning of said second commented line which
includes its prefix, meaning '#' and '//' for c++ style comments or '*'
for Phpdoc block's c style comments, their succeeding spaces and the
line indentation will be deleted first before command
\\[delete-forward-char] is called interactively, ending up stitching
current comment line with next one, word with word.  For this to happen,
previous line must also be of the same kind of comment and have the same
prefix.  See `php-fill-stitch-together-two-comment-lines' for additional
information."
  (interactive)
  (when (not (region-active-p))
    (let ((literal-type (car (php-fill-get-literal))))
      (cond
       ((memq literal-type '(c c++))
	(php-fill-stitch-together-two-comment-lines))
       ((php-fill-supported-string-literal-in-line)
	(php-fill-stitch-two-lines-concatenation 1 (point))))))
  (call-interactively 'delete-forward-char))

(defun php-fill-set-local-variables ()
  "Set current buffer's values of variables `sentence-end-double-space'\n\
and `fill-column'.

To this end, the values of `php-fill-sentence-end-double-space' and
`php-fill-fill-column' are used."
  (interactive)
  (setq-local sentence-end-double-space php-fill-sentence-end-double-space)
  (setq-local fill-column php-fill-fill-column))

(provide 'php-fill)
;;; php-fill.el ends here
