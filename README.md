# Hugoista - Manage Hugo Posts like a Barista (in Emacs)

![screenshot](/screenshot.png "screenshot")

Hugoista is an [Emacs](https://www.gnu.org/software/emacs/) package
which provides an overview of the blog posts in a
[Hugo](https://gohugo.io) site, grouped by their publication status.
Multiple Hugoista buffers can be open at the same time, each for a
different Hugo site.

To open the list of posts for a site, do `M-: (hugoista
"~/path/to/site")`. When you have a single site only, set the variable
`hugoista-site-dir` to the site directory (where `hugo.toml` is), and
simply do `M-x hugoista`.

With the cursor on the line for a post, hitting the `RETURN` key opens
the corresponding file for editing. Hitting the `+` or `N` key creates
a new post file, prompting for the file name. All standard
`tabulated-list-mode` key bindings are available, too (for example `g`
to refresh the list, `S` to sort by the current column, etc.).

Too see Hugoista's customisation options, do `M-x customize-group RET
hugoista RET`.

Under the hood, Hugoista calls Hugo to list the posts, and create new
content files. You are thus free to use page bundles, and all other
features supported by Hugo, as well as all content file, and front
matter formats supported by Hugo.

## Installation

Hugoista is available from [GNU ELPA](https://elpa.gnu.org/packages/).
Thus, installing it is as easy as doing interactively:

```
M-x package-install RET hugoista RET
M-: (require 'hugoista) RET
```

or with `use-package` and setting a default site directory at the same
time:

``` emacs-lisp
(use-package hugoista
  :ensure t
  :custom
  (hugoista-site-dir "~/path/to/default/site"))
```

Hope you'll find this useful, and happy posting!
