# trust-manager.el

This Emacs package streamlines per-project trust management.

Install it however you install packages, enable `trust-manager-mode`,
and you're good to go!

## What trust does this package manage?

Emacs 30 introduced a notion of trusted and untrusted buffers
following [my report](https://eshelyaron.com/posts/2024-11-27-emacs-aritrary-code-execution-and-how-to-avoid-it.html)
of an arbitrary code execution vulnerability known as CVE-2024-53920.
A buffer is trusted if the function `trusted-content-p` returns
non-nil ("true") in that buffer, otherwise it is untrusted.
Some features, which would be dangerous in untrusted buffers, are only
enabled in trusted buffers.

## Why do I need a trust manager?

By default, all Emacs buffers are untrusted, and you need configure
which buffers to trust via the user option `trusted-content`.  That's
a safe default, but it's not the most convenient.  Hence this package.

`trust-manager` sets up sane and safe defaults, where common files
that you already trust are marked as trusted so you get all the
features when you visit them.  This includes your init file and all of
the ELisp source files that come with Emacs and packages you install.

In addition, whenever you visit a file in a new project for the first
time, `trust-manager-mode` asks you whether or not you trust the
project, so you don't need to worry about configuring which projects
you trust ahead of time.

These conveniences let you benefit from all the features you want
without compromising on safety.  No need to open up overly broad trust
just to get things working.

