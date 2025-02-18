# jj (Jujutsu) integration with Emacs vc.el and project.el

Work-in-progress support for `vc.el` and `project.el` implementations
for the [Jujutsu](https://github.com/jj-vcs/jj) version control
system.

## Contributing

We welcome bug reports and pull requests!  Since we plan to contribute
the code upstream, either to GNU elpa or to core Emacs, non-trivial
code contributions should have the standard FSF copyright assignment
in place.

## Jujutsu configuration hints

Emacs has built-in support for git-style diff and conflict markers, so
you might want to set the following options in your configuration,
e.g., via `jj config edit --user` or `jj config edit --repo`:

```toml
[ui]
diff.format = "git"
conflict-marker-style = "git"
```
