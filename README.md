# a68-mode -- Algol68 major mode

This is an improved and modernised version of the Algol68 mode by Jose
E. Marchesi.  It fully supports automatic indentation and font locking
(i.e. syntax highlighting) including the three comment styles.

a68-mode supports only the UPPER stropping style and not the QUOTE or
POINT style.  Patch for those are welcome.  At least in theory, it
could be possible with a clever usage of text properties and/or
overlays to implement the **strict** language and use a bold typeface
for the "reserved words".


### Manual installation

Just put `a68-mode.el` somewhere in your `load-path` and require it.
Or visit the file with Emacs and `M-x package-install-file RET`.


### Customization

The following variables are available for customization:

 * `a68-indent-level` (default 3): indentation offset
 * `a68-comment-style` (default `"#"`): the default comment style used
   by e.g. `comment-dwim`.

see `M-x customize-group a68 RET` for more info.


### Known issues

It doesn't handle well shebangs: `#!` is taken as the start of the
comment up to the next `#`.
