# denote-search: A simple search utility for Denote

This package provides a search utility for Denote, the simple-to-use,
focused-in-scope, and effective note-taking tool for Emacs.

The command `denote-search` is the main point of entry.  It accepts a
query, which should be a regular expression, and then searches the
contents of all the notes stored in `denote-directory` for it.  The
results are put in a buffer which allows folding and further
filtering; all standard commands offered by Xref are available as
well.

This package has the same code principles as Denote: to be
simple-to-use, focused-in-scope, and effective.  We build upon Xref to
be good Emacs citizens, and don't use any dependencies other than
Denote and built-in libraries.

See the `README.org` file for a comprehensive manual.
