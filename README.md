Easy configuring of site-local Elisp files
==========================================

Find here the source for site-lisp.el, a script that make the loading
of other scripts inside of [Emacs] easier.  All you need to do is
place a file or directory within a well-known location and
site-lisp.el should take care of the rest.

[Emacs]:
	https://www.gnu.org/software/emacs/

Installation
------------

Site-lisp.el is avaliable from [GNU ELPA]. It can be installed by
invoking

	M-x package-install RET site-lisp RET

[GNU ELPA]:
	http://elpa.gnu.org/packages/site-lisp.html

Usage
-----

All you should need is to require `site-lisp` or call
`site-lisp-initialise` at some point in your initialisation file:

	(site-lisp-initialise)

or for example after Emacs has initialised:

	(add-hook 'after-init-hook #'site-lisp-initialise)

Contribute
----------

As site-lisp.el is distribed as part of [GNU ELPA], and therefore
requires a [copyright assignment] to the [FSF], for all non-trivial
code contributions.

[copyright assignment]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html
[FSF]:
	https://www.fsf.org/

Source code
-----------

Site-lisp.el is developed on [Codeberg].

[SourceHut]:
	https://codeberg.org/pkal/site-lisp.el/

Bugs and Patches
----------------

Bugs, patches, comments or questions can be submitted via [Codeberg's
issue system] or by sending [me] an email.

[Codeberg's issue system]:
	https://codeberg.org/pkal/site-lisp.el/issues
[me]:
	https://sdf.org/~pkal#contact

Distribution
------------

Site-lisp.el and all other source files in this directory are distributed
under the [GNU Public License], Version 3 (like Emacs itself).

[GNU Public License]:
	https://www.gnu.org/licenses/gpl-3.0.en.html
