# Urgrep News

## v0.5.2 (2025-06-06)

### Bug fixes
- Disable `ansi-color-compilation-filter` in Urgrep buffers

---

## v0.5.1 (2024-08-22)

### Bug fixes
- Fix an edge case when filtering ANSI escapes

---

## v0.5.0 (2024-05-11)

### New features
- Add ability to immediately adjust search options in the results buffer

### Breaking changes
- In the Urgrep minibuffer, toggling search of hidden files now uses `M-s H`

---

## v0.4.1 (2024-03-10)

### Bug fixes
- Fix highlighting of matches when using Ugrep

---

## v0.4.0 (2024-01-22)

### New features
- Add support for `outline-minor-mode`

### Bug fixes
- Fix invocation of `ripgrep` and `ugrep` on Windows

---

## v0.3.0 (2023-12-17)

### New features
- Add optional `urgrep-xref` feature, which lets Xref use Urgrep to create the
  command for searching in files/directories

### Bug fixes
- Respect buffer-local values of `urgrep-preferred-tools`

### Breaking changes
- `:files` keyword argument in `urgrep-command` and friends is now
  `:file-wildcard`
- `:directory` keyword argument in `urgrep-commnd` and friends is now `:root`,
  and can accept file and/or directory names

---

## v0.2.0 (2023-08-31)

### New features
- Add support for toggling whether to search in hidden files (`M-s h` in the
  search prompt, or `urgrep-search-hidden-files` globally)
- Add `:directory` key to `urgrep-command`, allowing you to specify zero or more
  directories to search in
- `urgrep` builtin for Eshell now supports specifying search directories as
  arguments
- Allow setting the search tool to use on the fly when reading the query
  (`M-s t` in the search prompt)

### Breaking changes
- `urgrep-run-command` now takes `:tool` as an optional key to match `urgrep`

---

## v0.1.1 (2023-06-07)

### Bug fixes
- Fix Eshell integration

---

## v0.1.0 (2023-05-13)

Initial release
