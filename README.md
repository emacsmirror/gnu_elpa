An Emacs client for https://keys.openpgp.org/
=============================================

Find here the source for openpgp.el, an Emacs client for the
[keys.openpgp.org] key server.

[keys.openpgp.org]:
	https://keys.openpgp.org/

Installation
------------

Openpgp.el is avaliable from [GNU ELPA]. It can be installed by
invoking

	M-x package-install RET openpgp RET

[GNU ELPA]:
	http://elpa.gnu.org/packages/openpgp.html

Usage
-----

The package provides three commands for fetching a key (by
fingerprint, key ID and email) and two commands for uploading a key
(from a file or from your GPG keychain).  Use <kbd>C-h a openpgp-
RET</kbd> to generate an overview.

Contribute
----------

As openpgp.el is distribed as part of [GNU ELPA], and therefore
requires a [copyright assignment] to the [FSF], for all non-trivial
code contributions.

[copyright assignment]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html
[FSF]:
	https://www.fsf.org/

Source code
-----------

Openpgp.el is developed on [SourceHut].

[SourceHut]:
	https://git.sr.ht/~pkal/openpgp/

Bugs and Patches
----------------

Bugs, patches, comments or questions can be submitted to my [public
inbox].

[public inbox]:
	https://lists.sr.ht/~pkal/public-inbox

Distribution
------------

Openpgp.el and all other source files in this directory are distributed
under the [GNU Public License], Version 3 (like Emacs itself).

[GNU Public License]:
	https://www.gnu.org/licenses/gpl-3.0.en.html
