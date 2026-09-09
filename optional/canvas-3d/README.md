# Optional Canvas 3D renderer

This subtree is a self-contained, optional Gnosis backend. It displays local
OBJ meshes and multi-object JSON scenes in a native GNU Emacs canvas image.
It does not grade answers, schedule reviews, install packages on opening, or
contain anatomical assets. Gnosis itself does not require these dependencies.

## Install explicitly

Requirements: GNU Emacs with `canvas-refresh` and the `canvas` image type
(currently Emacs 32 development builds), a graphical Emacs session, Python
3.12–3.14, and a working EGL/OpenGL 3.3 driver. This backend uses Linux EGL;
other platforms are not verified. Python dependencies cannot supply the
system graphics driver or add canvas support to an older Emacs.

Install [uv](https://docs.astral.sh/uv/getting-started/installation/) separately.
From a Gnosis checkout:

```sh
cd optional/canvas-3d
uv sync --locked
./preflight.py
```

The committed `.python-version` selects Python 3.12. `uv sync` creates a local
`.venv` and can download that Python if needed. It installs only the pinned
ModernGL, glcontext, NumPy and trimesh dependencies from `uv.lock`; no system
Python installation is modified. Copy this **whole directory**, including
fixtures and the lockfile, when distributing it independently. Run the same
install command in the new location; do not copy a virtual environment.

`preflight.py` uses only the standard library in its launcher. It checks the
specified Emacs binary, Python imports, and actual EGL color/ID frame output
independently, reports each failure, and exits nonzero if any check fails:

```sh
./preflight.py --emacs /path/to/emacs --python /path/to/prepared/bin/python
```

The batch Emacs check proves compiled canvas capability, **not** that a
particular graphical frame can display the image. Opening a viewer checks the
actual session. The EGL check renders original synthetic fixtures twice and
verifies packet identities, picking IDs and selection-only highlight changes.

## Load and use

Set `gnosis-model-renderer-directory` to this directory in your Gnosis
configuration. Alternatively, add this directory to `load-path`; Gnosis finds
`canvas-3d` lazily when a model is opened. For standalone use:

```elisp
(add-to-list 'load-path "/path/to/gnosis/optional/canvas-3d")
(require 'canvas-3d)
(canvas-3d-open "/path/to/gnosis/optional/canvas-3d/fixtures/scene.json"
                "Two triangles" '(0 0 1.0) 320)
```

`canvas-3d-python-command` defaults to nil: use `.venv/bin/python` beside
`canvas-3d.el`. Set it to an absolute executable filename or an executable
name on `exec-path` to use another **already prepared** Python environment.
It is one executable, not a shell command or argument list. The renderer
script remains `render.py` beside the library. No dependency installation or
network request occurs when a viewer opens.

The optional size argument is a fixed square side from 128 to 768 pixels;
the default is 512. Choose a size that fits the intended window before opening.
The viewer does not automatically resize or replace its renderer on window
changes. Larger sizes cost more pipe transfer and Emacs allocation.

- Drag with mouse button 1 or use arrows / `h j k l` to rotate.
- Click to select a visible object; background clears selection.
- Wheel or `+` / `-` zooms; `r` restores the initial camera.
- `SPC` toggles the initially hidden model label; `?` shows mode help.
- `e` opens the renderer error log. Startup/import/EGL failures remain visible
  in the header. Run preflight to distinguish missing capabilities.
- `C-g` stops the process and clears live picking while retaining the last
  displayed pixels; `q` closes the viewer. Reopen to restart a stopped viewer.

## Scene and selection contract

A scene is JSON with 1–255 objects, unique nonempty string IDs, string labels,
and OBJ paths relative to the scene file. For example:

```json
{"objects": [{"id": "left", "label": "Left triangle", "path": "left.obj"}],
 "initial_view": [0, 0, 1.0]}
```

Use trusted local geometry. Scene JSON is bounded to 64 KiB, each OBJ to
100 MB, and the scene to two million triangles. These are input limits, not a
sandbox or a peak-memory guarantee. Meshes are jointly normalized so their
relative coordinates survive. Materials/textures are not loaded. A bare OBJ
has stable selection ID `model`. Asset licenses remain the asset author's
responsibility; the fixture geometry here is original GPL-3.0-or-later data.
Gnosis managed-resource imports impose their own stricter provenance/path rules.

`canvas-3d-open (PATH &optional LABEL INITIAL-VIEW SIZE)` returns a fresh
viewer buffer. `canvas-3d-selected-id` and `canvas-3d-selection-hook` are local
to it. The hook runs synchronously from picking with a plist:
`(:id ID :frame SEQUENCE :owner PROCESS)`. Background ID is nil. Consumers
own interpretation, explicit submission and grading; selecting is not grading.
`canvas-3d-pick` accepts integer canvas X/Y and an optional retained frame.

The unchanged pipe protocol is newline-delimited JSON requests with `seq`,
`yaw`, `pitch`, `zoom`, and `selected` (one-based object index, zero for none).
Responses are `C3D1`, a big-endian uint32 sequence, then tightly packed
opaque BGRA color bytes and one object-ID byte per pixel, both top-down.
Color and depth-tested IDs come from the same draw and publish together.
The receiver validates sequence, ownership and the raw ID byte alphabet;
it bounds fragment backlog and discards superseded highlights. One request
is in flight, later camera input coalesces, and timeout/cancel/buffer retirement
releases the renderer. Picking uses the displayed frame, not a pending camera.

## Verify locally

After `uv sync --locked`, from this directory:

```sh
./preflight.py
.venv/bin/python -m unittest -v test_render
emacs -Q --batch -L . --eval "(require 'canvas-3d-tests)" -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile canvas-3d.el canvas-3d-tests.el
emacs -Q --batch --eval \
  '(progn (require (quote checkdoc)) (checkdoc-file "canvas-3d.el"))'
```

Use fresh bytecode after editing. ERT includes a real EGL subprocess, frame
coalescing, picking and highlight test; only native image refresh is stubbed
in batch. These tests do not establish native mouse/window behavior or the
Gnosis author/save/reopen/review journey, which require separate graphical
integration acceptance with disposable data.

Source and original fixtures: GNU GPL version 3 or later; see `LICENSE`.
