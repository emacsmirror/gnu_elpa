`shell-command` With a Few Extra Features
=========================================

Find here the source for shell-command+.el, that defines an extended
version of Emacs' `shell-command` (bound to <kbd>M-!</kbd> by
default). See [(emacs) Single Shell] if you are
unfamiliar with the command.

`shell-command+` has been based on a function named `bang` by [Leah
Neukirchen].

[(emacs) Single Shell]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Single-Shell.html
[Leah Neukirchen]:
	http://leahneukirchen.org/dotfiles/.emacs

Installation
------------

`shell-command+` is available from [GNU ELPA]. It can be installed by
invoking

	M-x package-install RET shell-command+ RET

[GNU ELPA]:
	http://elpa.gnu.org/packages/shell-command+.html

Usage
-----

Bind the command `shell-command+` to any key, for example
<kbd>M-!</kbd>:

~~~elisp
(global-set-key (kbd "M-!") #'shell-command+)
~~~

No further changes are necessary.  It is recommended to consult the
`shell-command+` documentation string (`C-h f shell-command+`) and the
`shell-command+` customisation group (`M-x customize-group
shell-command+`).

Note that [Dired] rebinds <kbd>M-!</kbd>, so it might be necessary to
also bind `shell-command+` in `dired-mode-map`.  I do this using [setup]:

~~~elisp
(setup (:package shell-command+)
  (:option (remove shell-command+-features) #'shell-command+-implicit-cd
           shell-command+-prompt "$ ")
  (:bind-into dired "M-!" shell-command+)
  (:global "M-!" shell-command+))
~~~

[Dired]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
[setup]:
	http://elpa.gnu.org/packages/setup.html

Contribute
----------

As `shell-command+.el` is distribed as part of [GNU ELPA], and
therefore requires a [copyright assignment] to the [FSF], for all
non-trivial code contributions.

[copyright assignment]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html
[FSF]:
	https://www.fsf.org/

Source code
-----------

`shell-command+` is developed on [Codeberg].

[Codeberg]:
	https://codeberg.org/pkal/shell-command-plus.el

Bugs and Patches
----------------

Bugs or comments can be submitted via [Codeberg's issue system] or by
sending [me] an email.

When contributing, make sure to provide test and use the existing
tests defined in shell-command+-tests.el.  These can be easily
executed using the bundled Makefile:

	make test

[Codeberg's issue system]:
	https://codeberg.org/pkal/shell-command-plus.el/issues
[me]:
	https://amodernist.com/#email

Distribution
------------

shell-command+.el and all other source files in this directory are
distributed under the [GNU Public License], Version 3 (like Emacs
itself).

[GNU Public License]:
	https://www.gnu.org/licenses/gpl-3.0.en.html
