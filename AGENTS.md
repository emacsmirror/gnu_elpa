# AGENTS.md

Public guidance for contributors and coding agents on `keymap-popup`.

## Project

`keymap-popup` adds popup help to ordinary Emacs keymaps. `keymap-popup-define`
builds a real `defvar-keymap`; `keymap-popup-annotate` describes an existing
map; `keymap-popup` shows either interactively. Emacs 29.1+. No runtime deps.

## Architecture

- One definition serves direct dispatch and popup help. Value stays a normal
  keymap: `where-is`, `describe-bindings`, `keymap-set`, parents, composition.
- Parsers normalize declarations to plain plist/alist data. Macros bind keys and
  attach descriptions via `keymap-popup--attach-meta`.
- `keymap-popup--render`: descriptions + state in, propertized text out. Buffer
  and window effects live elsewhere.
- Wrapper map parents the source map and overrides only active-popup behavior.
  Unhandled keys fall through.
- Lifecycle: `set-transient-map` plus one buffer-local session. Nested menus
  stack navigation; dismissal unwinds every active map and hook.
- `:if` filters the live binding through `menu-item`. `:inapt-if` is popup-only:
  presentation and refuse-in-popup; direct dispatch outside popup unchanged.

## Conventions

- Lexical binding. Public `keymap-popup-`; internal `keymap-popup--`.
- Small pure transforms, explicit args, plain alists/plists. Insert, display,
  hooks, and command run at clear boundaries.
- Additive: keymap works without opening a popup. No parallel command registry
  or hidden ownership of user state.
- Keep predicate layers separate: dispatch reads the live keymap; render reads
  normalized descriptions. Do not merge paths to erase apparent duplication.
- Complex infix systems and stateful sub-prefix languages are out of scope.
- Focused ERT for parser validation, macro expansion, metadata, predicates,
  wrapper dispatch, nested menus, persistence, and cleanup.

## Verification

Makefile enters Nix develop when available.

```sh
make test      # ERT
make lint      # checkdoc + package-lint
make compile   # byte-compile keymap-popup.el
make doc       # Info manual
make dev       # compile + lint + test
```

Run `make dev` after non-trivial edits. Run `make doc` when user-facing syntax
or docs change.
