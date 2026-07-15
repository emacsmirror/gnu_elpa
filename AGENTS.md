# AGENTS.md

Public guidance for contributors and coding agents working on `keymap-popup`.

## Project

`keymap-popup` adds popup help to ordinary Emacs keymaps. The
`keymap-popup-define` macro creates a real `defvar-keymap`,
`keymap-popup-annotate` describes an existing keymap, and `keymap-popup`
displays either form interactively. The package requires Emacs 29.1+ and has no
external runtime dependencies.

## Design and architecture

- Keep one definition for direct dispatch and popup help. The resulting value
  must remain a normal keymap compatible with `where-is`, `describe-bindings`,
  `keymap-set`, parent maps, and standard keymap composition.
- Parsers validate and normalize declarations into plain plist/alist data. The
  macros build keymap bindings and attach descriptions as metadata on the keymap
  value through `keymap-popup--attach-meta`.
- `keymap-popup--render` is the computation boundary: descriptions and current
  state in, propertized text out. Buffer and window effects belong elsewhere.
- The wrapper map parents the source keymap and overrides only behavior needed
  while the popup is active. Unhandled keys continue through the source map.
- Popup lifecycle uses `set-transient-map` and one buffer-local session. Nested
  menus stack navigation state; dismissal must unwind every active map and hook.
- `:if` controls the underlying binding through a `menu-item` filter.
  `:inapt-if` is popup-only: it changes presentation and refuses the key while
  leaving direct dispatch outside the popup unchanged.

## Constraints and conventions

- Use lexical binding. Public names use `keymap-popup-`; internal helpers use
  `keymap-popup--`.
- Prefer small pure transformations, explicit arguments, and plain alists or
  plists. Keep buffer insertion, display, hooks, and command execution at clear
  boundaries.
- Preserve the additive design: the keymap must work without opening a popup.
  Do not introduce a parallel command registry or hidden ownership of user state.
- Keep predicate evaluation in its intentional layers: dispatch reads the live
  keymap; rendering reads normalized descriptions. Do not merge these paths just
  to remove apparent duplication.
- Complex infix systems and stateful sub-prefix languages are outside this
  package's scope.
- Add focused ERT coverage for parser validation, macro expansion, metadata,
  predicates, wrapper dispatch, nested menus, persistence, and cleanup.

## Verification

The Makefile enters the Nix development environment when available.

```sh
make test      # Run the ERT suite
make lint      # Run checkdoc and package-lint
make compile   # Byte-compile keymap-popup.el
make doc       # Build the Info manual
make dev       # Compile, lint, and test
```

Run `make dev` before submission. Run `make doc` when changing user-facing
syntax or documentation.

## Contributions

Send patches to <patches@thanosapollo.org> with a subject like
`[PATCH emacs-keymap-popup] Short description`.

Send bugs and feature requests to <bugs@thanosapollo.org> with a subject like
`[BUG emacs-keymap-popup] Short description`.
