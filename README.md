Meta-Xref backend
=================

Find here the source for xref-union.el, an meta-[Xref] backed for
combining the results of multiple other backends.

[Xref]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html

Installation
------------

Xref-union.el is avaliable from [GNU ELPA]. It can be installed by
invoking

	M-x package-install RET xref-union RET

[GNU ELPA]:
	http://elpa.gnu.org/packages/xref-union.html

Usage
-----

The easiest way to use the package is to enable the `xref-union-mode`
minor mode, manually or by adding a hook to whatever mode you expect
using the package with

	(add-hook 'foo-bar-mode-hook 'xref-union-mode)

This will merge all the other backends into one.

You can also manually choose what backends to combine, by enumerating
them in an object like:

	'(union some-randomxref-backend etags--xref-backend)

and adding that to `xref-backend-functions`.

Contribute
----------

As xref-union.el is distribed as part of [GNU ELPA], and therefore
requires a [copyright assignment] to the [FSF], for all non-trivial
code contributions.

[copyright assignment]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html
[FSF]:
	https://www.fsf.org/

Source code
-----------

Xref-union.el is developed on [Codeberg].

[Codeberg]:
	https://codeberg.org/pkal/xref-union.el

Bugs and Patches
----------------

Bugs, patches, comments or questions can be submitted via [Codeberg's
issue system] or by sending [me] an email.

[Codeberg's issue system]:
	https://codeberg.org/pkal/xref-union.el/issues
[me]:
	https://sdf.org/~pkal#contact

Distribution
------------

Xref-union.el and all other source files in this directory are
distributed under the [GNU Public License], Version 3 (like Emacs
itself).

[GNU Public License]:
	https://www.gnu.org/licenses/gpl-3.0.en.html
