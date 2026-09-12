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
This directory is not shipped by GNU ELPA. Obtain a Gnosis source checkout
matching the installed package version (check `M-x describe-package RET
gnosis RET`), and retain the entire `optional/canvas-3d/` subtree. Do not
add the checkout's `lisp/` to `load-path`: keep using the ELPA core package.
From that checkout:

```sh
cd optional/canvas-3d
uv sync --locked
./preflight.py
```

The committed `.python-version` selects Python 3.12. `uv sync` creates a local
`.venv` and can download that Python if needed. It installs only the pinned
ModernGL, glcontext and NumPy dependencies from `uv.lock`; no system
Python installation is modified. Copy this **whole directory**, including
fixtures and the lockfile, when distributing it independently. Run the same
install command in the new location; do not copy a virtual environment.

`preflight.py` uses only the standard library in its launcher. It checks the
specified Emacs binary, Python imports, and actual EGL color frames and compact picking output
independently, reports each failure, and exits nonzero if any check fails:

```sh
./preflight.py --emacs /path/to/emacs --python /path/to/prepared/bin/python
```

The batch Emacs check proves compiled canvas capability, **not** that a
particular graphical frame can display the image. Opening a viewer checks the
actual session. The EGL check renders original synthetic fixtures twice and
verifies packet identities, background/stale picks and a changed highlight.

## Load and use

Set the absolute backend directory in your Emacs configuration:

```elisp
(with-eval-after-load 'gnosis
  (setq gnosis-model-renderer-directory
        "/path/to/gnosis/optional/canvas-3d/"))
```

Gnosis first uses this setting, then looks for `canvas-3d` on `load-path`,
then checks for a sibling backend in a source checkout. An ELPA-only
installation has no sibling backend. Loading core Gnosis does not load the
renderer; opening a model checks the actual graphical/canvas and prepared
Python capabilities and reports a setup error if they are missing.
For standalone use:

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
The displayed dimensions remain fixed across window changes. Rotation and zoom
always use that full resolution; there is no moving-image blur or delayed
refinement. Choose a smaller fixed size when needed on a lower-powered host.
Geometry is never simplified. Picks use the exact displayed frame; a pending
newer view still prevents picking.

Real viewers transfer BGRA through native canvas `:file`, not bulk Lisp strings.
Each renderer has a caller-created private temporary directory. Complete files
are atomically published under sequence-qualified names before a 24-byte pipe
notification. The receiver checks process, sequence, directory identity, file
ownership/type/mode and exact size before native loading. No renderer-supplied
path is accepted. Only the current backing file and one pending publication
(with a bounded writing file) are retained. Cancel, process death, timeout or
detach copies the last image into memory once and removes the directory; old
feedback remains redisplayable, without retaining publication files. If the last
backing file is missing or invalid, cleanup reports the loss and installs blank
in-memory pixels instead of reading unbounded data or retaining a dead filename.
Killing or changing the owning buffer mode also releases these resources.

- Drag with mouse button 1 or use arrows / `n p f b` to rotate.
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
to it. The hook runs after asynchronous picking delivery with a plist:
`(:mesh ID :face TRIANGLE :point (X Y Z) :id ID :frame SEQUENCE :owner PROCESS)`.
`canvas-3d--selection` retains this geometry snapshot. Face indices are zero-based
file-order OBJ fan triangles, not object-index bytes; points use original mesh
coordinates (float32 wire precision). Background ID and geometry are nil. Consumers
own interpretation, explicit submission and grading; selecting is not grading.
`canvas-3d-pick` accepts integer canvas X/Y and an optional retained frame.

The pipe protocol is newline-delimited JSON requests with `seq`, `yaw`, `pitch`,
`zoom`, `selected` (one-based object index, zero for none), and `highlight`
(null or target geometry). Requests are bounded to 128 KiB. An optional request
`size` must equal the configured fixed size. Native file mode is selected at
startup with `--frame-directory` and a 16-digit lowercase hex `--frame-identity`.
Its view notification is `C3D4`, a big-endian uint32 sequence and the 16 ASCII
identity bytes. The corresponding `pending-SEQUENCE.bgra` contains exactly
4 × area opaque, top-down BGRA bytes. Emacs gives displayed files a separate
sequence-qualified name and deletes the previous one only after refresh.
Standalone script users omitting the file flags receive `C3D3`, a big-endian
uint32 sequence and tightly packed BGRA color (8 + 4 × area bytes).
The renderer retains the matching depth-tested object, face and
original-coordinate GPU attachments; only the clicked pixel is read back
on a pick, and no geometry planes travel with the image.

A pick request has `op: "pick"`, its own `seq`, the displayed `frame` sequence,
and integer raw-frame pixel `x` and `y`. Its 32-byte response contains:

- `C3P3`, request sequence, requested frame sequence (12 bytes);
- big-endian uint32 object index and triangle index **plus one** (8 bytes);
- original XYZ, three finite big-endian IEEE float32 values (12 bytes).

Zero object/face/coordinates mean background. Object index `0xffffffff` means
the renderer no longer retains the requested frame, not background. Faces above
65535 retain their full identity. Python `frame_pair` still returns color/mesh
planes locally. Existing Elisp protocol 1/2 fixture paths remain for transport
regressions; real open/attach always sets 4. There is no wire negotiation or
fallback to old renderer installations.

OBJ `v` and `f` are read in file order. Each polygon becomes `(v0, vi, vi+1)`;
positive position indices and negative indices relative to preceding vertices
are supported. Texture/normal suffixes, groups and materials never change
triangle numbering. Invalid indices, nonfinite vertices and degenerate
triangles fail before rendering. No geometry merging or trimesh parser is used.

A domain may set buffer-local `canvas-3d--question-target` **after** open/attach,
then call `canvas-3d--request`. This alist has `mesh` (object ID) and `kind`
(`"object"`, `"point"`, `"region"`). Points add zero-based `face`, three
`barycentric` weights and positive original-coordinate `tolerance`; regions
add nonempty `faces`. Rendering colors only that mesh/surface geometry and
never draws a label. Occluded target surfaces stay occluded. The question
highlight survives rotation, zoom and reset. Clicks still update the independent
geometry selection and run selection hooks for native authoring/inspection,
but never retarget this highlight. Even label reveal stays hidden.
Grading and scene target validation belong to Gnosis, not this backend.

The receiver validates sequences, ownership, object indices and finite surface
coordinates; it bounds fragment backlog and discards superseded highlights.
Views and picks share one serialized request stream, later camera input coalesces,
and timeout/cancel/buffer retirement releases the renderer. Picking is refused
while a view is pending and rejects a supplied frame once replaced. A camera
change supersedes a pending pick. Selection hooks run asynchronously after hit
validation, outside the process filter; `canvas-3d-pick` returns nil immediately.
Camera redraw carries retained
geometry to the new frame only if the request's selection snapshot still
matches and its process owns the selection; it never reinterprets old pixels.

## Verify locally

After `uv sync --locked`, from this directory:

```sh
./preflight.py
.venv/bin/python -m unittest -v test_protocol test_render test_geometry
emacs -Q --batch -L . --eval "(progn (require 'canvas-3d-tests) (require 'canvas-3d-geometry-tests) (require 'canvas-3d-pick-tests))" -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile canvas-3d.el canvas-3d-tests.el canvas-3d-geometry-tests.el canvas-3d-pick-tests.el
emacs -Q --batch --eval \
  '(progn (require (quote checkdoc)) (checkdoc-file "canvas-3d.el"))'
```

Use fresh bytecode after editing. ERT includes a real EGL subprocess, frame
coalescing, picking and highlight test; only native image refresh is stubbed
in batch. These tests do not establish native mouse/window behavior or the
Gnosis author/save/reopen/review journey, which require separate graphical
integration acceptance with disposable data.

Source and original fixtures: GNU GPL version 3 or later; see `LICENSE`.
