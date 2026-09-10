#!/usr/bin/env python3
# Copyright (C) 2026 Thanos Apollo
# SPDX-License-Identifier: GPL-3.0-or-later
"""Check Emacs canvas, Python imports, and real EGL rendering separately."""
import argparse
import hashlib
import json
from pathlib import Path
import struct
import subprocess
import sys

ROOT = Path(__file__).resolve().parent


def run(command: list[str], **kwargs) -> subprocess.CompletedProcess:
    """Run a bounded diagnostic, retaining both output streams."""
    return subprocess.run(command, capture_output=True, timeout=30, **kwargs)


def emacs_check(emacs: str) -> None:
    """Check compiled canvas support without opening a graphical session."""
    expression = """(progn (require 'image)
      (princ (format "Emacs %s; canvas-refresh=%S; canvas type=%S\\n"
                     emacs-version (fboundp 'canvas-refresh)
                     (memq 'canvas image-types)))
      (kill-emacs (if (and (fboundp 'canvas-refresh)
                           (memq 'canvas image-types)) 0 1)))"""
    result = run([emacs, "-Q", "--batch", "--eval", expression], text=True)
    print(result.stdout.strip())
    if result.returncode:
        raise RuntimeError(result.stderr.strip() or "GNU Emacs with canvas support is required")
    print("Graphical display availability is not established by this batch check.")


def imports_check(python: str) -> None:
    """Check renderer imports without creating an OpenGL context."""
    result = run([python, "-c",
                  "import moderngl, glcontext, numpy; "
                  "print('moderngl=' + moderngl.__version__, "
                  "'numpy=' + numpy.__version__)"], text=True)
    if result.returncode:
        raise RuntimeError(result.stderr.strip())
    print(result.stdout.strip())


def egl_check(python: str) -> None:
    """Render color-only frames and verify compact background/stale replies."""
    size = 128
    request = (b'{"seq":1}\n{"seq":2,"selected":1}\n'
               b'{"op":"pick","seq":3,"frame":2,"x":0,"y":0}\n'
               b'{"op":"pick","seq":4,"frame":1,"x":0,"y":0}\n')
    result = run([python, str(ROOT / "render.py"),
                  str(ROOT / "fixtures/scene.json"), "--size", str(size)], input=request)
    if result.returncode:
        raise RuntimeError(result.stderr.decode(errors="replace").strip())
    area = size * size
    length = 8 + 4 * area
    if len(result.stdout) != 2 * length + 64:
        raise RuntimeError("Wrong packet length")
    frames = [result.stdout[i * length:(i + 1) * length] for i in range(2)]
    for seq, packet in enumerate(frames, 1):
        if packet[:8] != b"C3D3" + struct.pack(">I", seq):
            raise RuntimeError("Wrong packet identity")
    color, marked = [packet[8:] for packet in frames]
    if color == marked:
        raise RuntimeError("Selected highlight failed")
    background = struct.unpack(">4sIIIIfff", result.stdout[2 * length:2 * length + 32])
    stale = struct.unpack(">4sIIIIfff", result.stdout[2 * length + 32:])
    if background != (b"C3P3", 3, 2, 0, 0, 0, 0, 0):
        raise RuntimeError("Wrong compact background pick")
    if stale != (b"C3P3", 4, 1, 0xffffffff, 0, 0, 0, 0):
        raise RuntimeError("Stale frame was not refused")
    print(result.stderr.decode(errors="replace").strip())
    print(json.dumps({"frames": 2, "picks": 2, "bytes": len(result.stdout),
                      "color_sha256": hashlib.sha256(color).hexdigest(),
                      "highlight_sha256": hashlib.sha256(marked).hexdigest()}))


def main() -> int:
    """Report independent capabilities and fail if any requested check fails."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--emacs", default="emacs", help="GNU Emacs executable")
    parser.add_argument("--python", default=str(ROOT / ".venv/bin/python"),
                        help="Prepared renderer Python executable")
    args = parser.parse_args()
    failed = False
    for name, check, executable in (("EMACS CANVAS", emacs_check, args.emacs),
                                     ("PYTHON IMPORTS", imports_check, args.python),
                                     ("EGL COLOR + PICKING", egl_check, args.python)):
        print(f"== {name} ==", flush=True)
        try:
            check(executable)
        except (OSError, subprocess.TimeoutExpired, RuntimeError) as error:
            print(f"FAIL: {error}", flush=True)
            failed = True
        else:
            print("PASS", flush=True)
    return int(failed)


if __name__ == "__main__":
    sys.exit(main())
