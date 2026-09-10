"""Small wire-contract tests; no OpenGL context or third-party imports."""
# Copyright (C) 2026 Thanos Apollo
# SPDX-License-Identifier: GPL-3.0-or-later
import importlib.util
from pathlib import Path
import struct
import sys
import unittest
from unittest.mock import Mock, patch, call

spec = importlib.util.spec_from_file_location("protocol_renderer", Path(__file__).with_name("render.py"))
assert spec is not None and spec.loader is not None
render = importlib.util.module_from_spec(spec)
with patch.dict(sys.modules, {"moderngl": Mock(), "numpy": Mock()}):
    spec.loader.exec_module(render)


class ProtocolTests(unittest.TestCase):
    def setUp(self):
        self.renderer = render.Renderer.__new__(render.Renderer)
        self.renderer.size = self.renderer.rendered_size = 256
        self.renderer.latest = None
        self.renderer._draw = Mock()
        self.renderer._color_bytes = Mock(return_value=b"color")
        self.renderer.framebuffer = Mock()

    def test_color_only_and_exact_surface_coordinates(self):
        r = self.renderer
        packet = r.request(seq=10, size=256, yaw=40, pitch=20)
        self.assertEqual(packet, b"C3D3" + struct.pack(">I", 10) + b"color")
        r._draw.assert_called_once_with(256, yaw=40, pitch=20)
        r.framebuffer.read.assert_not_called()
        r.rendered_size = 128
        r.framebuffer.read.side_effect = [b"\x02", struct.pack("=I", 70001), struct.pack("=fff", -1, -2, -3)]
        hit = r.request(op="pick", seq=11, frame=10, x=5, y=7)
        self.assertEqual(struct.unpack(">4sIIIIfff", hit),
                         (b"C3P3", 11, 10, 2, 70001, -1, -2, -3))
        self.assertEqual(len(hit), 32)
        self.assertEqual(r._draw.call_count, 1)
        self.assertEqual(r.framebuffer.read.call_args_list, [
            call(viewport=(5, 120, 1, 1), components=n, attachment=a, alignment=1, dtype=d)
            for n, a, d in ((1, 1, "f1"), (1, 2, "u4"), (3, 3, "f4"))])

    def test_stale_and_background_are_distinct(self):
        r = self.renderer
        r.packet(10)
        r.framebuffer.read.return_value = b"\0"
        self.assertEqual(struct.unpack(">4sIIIIfff", r.pick(11, 10, 1, 0))[3:], (0, 0, 0, 0, 0))
        self.assertEqual(r.framebuffer.read.call_count, 1)
        r.packet(12, yaw=90)
        r.framebuffer.read.reset_mock()
        self.assertEqual(struct.unpack(">4sIIIIfff", r.pick(13, 10, 0, 1))[2:],
                         (10, 0xffffffff, 0, 0, 0, 0))
        r.framebuffer.read.assert_not_called()

    def test_invalid_requests(self):
        r = self.renderer
        for args in ((1, 1, -1, 0), (1, 1, 256, 0), (1, 1, True, 0),
                     (0, 1, 0, 0), (1, False, 0, 0)):
            with self.assertRaises(ValueError):
                r.pick(*args)
        for size in (True, 127, 257, 128.0, "128"):
            with self.assertRaises(ValueError):
                r.packet(1, size=size)
        r._draw.assert_not_called()
        with self.assertRaises(ValueError):
            r.request(1, op="grade", success=True)


class FilePublicationTests(unittest.TestCase):
    def test_atomic_bounded_and_retired_directory(self):
        import tempfile
        import os
        with tempfile.TemporaryDirectory() as directory:
            output = render.FrameOutput(directory, "0123456789abcdef")
            try:
                packet = b"C3D3" + struct.pack(">I", 1) + b"abcd"
                self.assertEqual(output.publish(packet), b"C3D4" + packet[4:8] + b"0123456789abcdef")
                pending = Path(directory, "pending-1.bgra")
                self.assertEqual(pending.read_bytes(), b"abcd")
                self.assertEqual(pending.stat().st_nlink, 1)
                self.assertEqual(pending.stat().st_mode & 0o777, 0o600)
                self.assertEqual(list(Path(directory).iterdir()), [pending])
                with self.assertRaises(ValueError):
                    output.publish(packet)
                pending.unlink()
                # A publication fault cannot announce partial pixels or leak its temp.
                with patch.object(render.os, "link", side_effect=OSError("publication failed")):
                    with self.assertRaises(OSError):
                        output.publish(packet)
                self.assertEqual(list(Path(directory).iterdir()), [])
                os.rmdir(directory)
                os.mkdir(directory, 0o700)
                # A replacement path is not the original directory capability.
                with self.assertRaises(OSError):
                    output.publish(packet)
                self.assertEqual(list(Path(directory).iterdir()), [])
            finally:
                output.close()

    def test_private_directory_and_no_symlink_overwrite(self):
        import tempfile
        import os
        with tempfile.TemporaryDirectory() as directory:
            for identity in (None, "x", "A" * 16, "../" + "a" * 13):
                with self.assertRaises(ValueError):
                    render.FrameOutput(directory, identity)
            os.chmod(directory, 0o755)
            with self.assertRaises(ValueError):
                render.FrameOutput(directory, "0123456789abcdef")
            os.chmod(directory, 0o700)
            target = Path(directory, "keep")
            target.write_bytes(b"unchanged")
            Path(directory, "writing.bgra").symlink_to(target)
            output = render.FrameOutput(directory, "0123456789abcdef")
            try:
                with self.assertRaises(FileExistsError):
                    output.publish(b"C3D3" + struct.pack(">I", 1) + b"abcd")
                self.assertEqual(target.read_bytes(), b"unchanged")
            finally:
                output.close()


if __name__ == "__main__":
    unittest.main()
