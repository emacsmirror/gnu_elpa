Autocrypt for Emacs
===================

[Autocrypt] is cryptography protocol, for distributing and
automatically encrypting emails.  This package generically implements
the protocol, for various Emacs MUAs (Mail User Agent).

Currently, it supports:

- Rmail, as a viewer
- Gnus, as a viewer
- mu4e, as a viewer
- message, as a composer

As of writing, this package doesn't fully implement the autocrypt
protocol.  It is currently still missing:

- Composing the setup message
- Parsing the setup message
- Key-Gossip Parsing (although the logic has been implemented)

I will attempt to on these issues in time, but any contributions, both
in terms of extending the support of the protocol, but also MUA
integration is welcomed.

[Autocrypt]:
	https://autocrypt.org/

How to use
----------

This package is distributed on [GNU ELPA].

One might configure `autocrypt.el` to read headers using Rmail and
inject headers using `message-mode` as follows:

~~~elisp
(add-hook 'rmail-mode-hook #'autocrypt-mode)
(add-hook 'message-mode-hook #'autocrypt-mode)
~~~

Autocrypt recommends using a new or separate key pair for signing and
encrypting.  If you wish to do so, call the `autocrypt-create-account`
command.  In case you want to manually configure your setup, customise
the `autocrypt-accounts` option.  Note that configuring a key is
necessary for `autocrypt.el` to function properly.

[GNU ELPA]:
	https://elpa.gnu.org/packages/autocrypt.html

Extending `autocrypt.el`
------------------------

Autocrypt.el uses a custom extension mechanism, comparable to
vc-mode.  Each time a "generic" function is invoked, either
`autocrypt-backend-function` is used to return the right function or
`autocrypt-backends` is used to find a function.

Support for additional MUAs can be added to this package, but should
preferably be part of the MUAs.  An external MUA should either set
autocrypt-backend-function in every buffer it handles, or modify
`autocrypt-backends` to define a backend.

A backend is designated by a symbol.  This is used together with a
generic command to check for a function.  For example, given the
backend `gnus` and the command `get-header`, autocrypt would check if
any of the following functions are defined:

- `autocrypt-gnus--get-header`
- `gnus-autocrypt--get-header`
- `gnus--autocrypt-get-header`

and call the first one it finds.  All a backend has to do is to define
these functions and ensure that they are visible (e.g. by autoloading
when necessary).

Source code
-----------

Do-at-point.el is developed on [Codeberg].

[Codeberg]:
	https://codeberg.org/pkal/do-at-point.el

Bugs and Patches
----------------

Bugs, patches, comments or questions can be submitted via [Codeberg's
issue system] or by sending [me] an email.

[public inbox]:
	https://lists.sr.ht/~pkal/public-inbox
[me]:
	https://amodernist.com/#email

Distribution
---------

Autocrypt.el and all other source files in this directory are
distributed under the [GNU Public License], Version 3 (like Emacs
itself).

[GNU Public License]:
	https://www.gnu.org/licenses/gpl-3.0.en.html
