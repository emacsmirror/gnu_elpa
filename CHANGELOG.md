# Version 0.2.1 (2026-04-11)

## Features

- Added project-local virtual environment binary detection for Python/REPLs. The
  new `termint-find-binary` function searches upward for executables in `.venv`
  directories, with platform-aware paths for Unix and Windows. This is supported
  by two helper functions: `termint-ipython-cmd-function` and
  `termint-python-cmd-function`.

- Expanded configuration options by defining more settings via `defcustom`,
  improving customization support.

# Version 0.2 (2025-11-30)

# Features

Three configuration variables have been introduced to enable the creation of
customized commands or keymaps. These allow all schemas to inherit the same
extensions, eliminating the need for repetition per REPL:

- `termint-region-dispatchers`: Register additional region selectors to
  automatically generate corresponding send/source commands across all schemas.
- `termint-schema-custom-commands`: Define universal custom commands that apply
  to every schema.
- `termint-mode-map-additional-keys`: Specify global keybindings (mapping keys
  to command suffixes) for inclusion in all generated schema keymaps, ensuring
  common shortcuts remain synchronized.

# Version 0.1 (2025-08-12)

- Initial release.
