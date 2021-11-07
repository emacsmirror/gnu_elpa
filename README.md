# a68-mode -- Algol68 major mode

This is an improved and modernised version of algol68 mode by Jose
E. Marchesi.  It fully supports automatic indentation and font locking
(i.e. syntax highlighting -- comments included).


### Manual installation

Just put `a68-mode.el` somewhere in your `load-path` and require it.
Or visit the file with Emacs and `M-x package-install-file`.


### Customization

The following variables are available for customization (see `M-x
customize-group a68 RET`)

 * `a68-indent-level` (default 3): indentation offset
 * `a68-comment-style` (default `"#"`): the default comment style used
   by e.g. `comment-dwim`.
