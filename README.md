Flymake backend for Clippy, displaying lint diagnostics as overlays in the buffer.

Based on an [example](https://www.gnu.org/software/emacs/manual/html_mono/flymake.html#An-annotated-example-backend) provided in the official Flymake manual.

Usage:

```elisp
(use-package clippy-flymake
  :load-path "~/workspace/clippy-flymake"
  :hook ((rust-mode . clippy-flymake-setup))
  :config
  (add-hook 'rust-mode-hook #'flymake-mode))
```
