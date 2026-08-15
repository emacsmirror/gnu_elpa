A Flymake Backend of Harper
===========================

This package makes it possible to use [Harper] with Emacs built-in
[Flymake], the syntax checker UI that this package uses to present
local grammar analysis.  This package is a fork of [flymake-proselint]
by Manuel Uberti.

[Harper]:
	https://writewithharper.com/
[Flymake]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Flymake.html
[flymake-proselint]:
	https://elpa.gnu.org/packages/flymake-proselint.html

Installation
------------

`flymake-harper.el` is available from [GNU ELPA]. It can be installed
by invoking

	M-x package-install RET flymake-harper RET

[GNU ELPA]:
	http://elpa.gnu.org/packages/flymake-harper.html

Usage
-----

You can add `flymake-harper-setup` to a hook of your choice
(e.g. `text-mode-hook`) or by manually adding `flymake-harper-backend'
to `flymake-diagnostic-functions'.

All the analysis by Harper will then be presented using Flymake's UI.

Using the user option `flymake-harper-disable' you can suppress
certain kinds of Harper warnings.

Contribute
----------

As `flymake-harper.el` is distributed as part of [GNU ELPA], and
therefore requires a [copyright assignment] to the [FSF], for all
nontrivial code contributions.

[copyright assignment]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html
[FSF]:
	https://www.fsf.org/

Source code
-----------

`flymake-harper` is developed on [Codeberg].

[Codeberg]:
	https://codeberg.org/pkal/flymake-harper.el

Bugs and Patches
----------------

Bugs, patches, comments, or questions can be submitted via [Codeberg's
issue system] or my sending [me] an email.

[Codeberg's issue system]:
	https://codeberg.org/pkal/flymake-harper.el/issues
[me]:
	https://sdf.org/~pkal/#contact

Distribution
------------

`flymake-harper.el` and all other source files in this directory are
distributed under the [GNU Public License], Version 3 (like Emacs
itself).

[GNU Public License]:
	https://www.gnu.org/licenses/gpl-3.0.en.html
