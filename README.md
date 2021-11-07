# a68-mode -- Algol68 major mode

This is an improved and modernised version of the Algol68 mode by Jose
E. Marchesi.  It fully supports automatic indentation and font locking
(i.e. syntax highlighting -- comments included).

a68-mode only supports the UPPER stropping style and not the QUOTE or
POINT style.  Patch for those are welcome.  At least in theory, it
could be possible with a clever usage of text properties and/or
overlays to implement the **strict** language and use a bold typeface
for the "reserved words".


### Manual installation

Just put `a68-mode.el` somewhere in your `load-path` and require it.
Or visit the file with Emacs and `M-x package-install-file`.


### Customization

The following variables are available for customization (see `M-x
customize-group a68 RET`)

 * `a68-indent-level` (default 3): indentation offset
 * `a68-comment-style` (default `"#"`): the default comment style used
   by e.g. `comment-dwim`.
