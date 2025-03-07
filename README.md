# flymake-clippy

`flymake-clippy` is an Emacs package that integrates [Clippy](https://doc.rust-lang.org/clippy/), a linter for the Rust programming language, with the Flymake diagnostics system. It provides real-time linting feedback directly in your Emacs buffers using cargo clippy.

## Features

- Runs `cargo clippy` automatically for Rust code.
- Integrates with Emacs' Flymake for live linting feedback.
- Displays Clippy diagnostics in the Emacs buffer.
- Supports error, warning, and help messages with suggestions for fixing issues.

## Dependencies

- Emacs 27 or later.
- Rust with `cargo` and `clippy` installed.

## Installation

### Manual Installation

1. Clone or download the repository to a local directory:

    ```bash
    git clone https://github.com/SiberzK/flymake-clippy.git
    ```

2. Add the following to your Emacs configuration:

    ```elisp
    (use-package flymake-clippy
      :load-path "~/path/to/flymake-clippy"
      :hook ((rust-mode . flymake-clippy-setup))
      :custom (flymake-clippy-cargo-path "cargo") ; Optional: Specify the path to the `cargo' executable.
      :config (add-hook 'rust-mode-hook #'flymake-mode))
    ```

## License

This package is free software and is distributed under the terms of the GNU General Public License v3.0.
