"""Small wire-contract tests; no OpenGL context or third-party imports."""
# Copyright (C) 2026 Thanos Apollo
# SPDX-License-Identifier: GPL-3.0-or-later
import importlib.util
from pathlib import Path
import struct
import sys
import unittest
from unittest.mock import Mock, patch

# Import only the protocol methods without installing or initializing EGL.
spec = importlib.util.spec_from_file_location("protocol_renderer", Path(__file__).with_name("render.py"))
assert spec is not None and spec.loader is not None
render = importlib.util.module_from_spec(spec)
with patch.dict(sys.modules, {"moderngl": Mock(), "numpy": Mock()}):
    spec.loader.exec_module(render)


class ProtocolTests(unittest.TestCase):
    def setUp(self):
        self.renderer = render.Renderer.__new__(render.Renderer)
        self.renderer.size = 2
        self.geometry = (bytes(range(16)), bytes([1, 0, 2, 1]),
                         struct.pack(">IIII", 3, 0, 7, 1),
                         struct.pack(">12f", 1, 2, 3, 0, 0, 0, -1, -2, -3, 4, 5, 6))
        self.renderer.frame_geometry = Mock(return_value=self.geometry)

    def test_color_only_and_exact_surface_coordinates(self):
        r = self.renderer
        packet = r.request(seq=10, yaw=40, pitch=20)
        self.assertEqual(packet, b"C3D3" + struct.pack(">I", 10) + self.geometry[0])
        r.frame_geometry.assert_called_once_with(yaw=40, pitch=20)
        hit = r.request(op="pick", seq=11, frame=10, x=0, y=1)
        self.assertEqual(struct.unpack(">4sIIIIfff", hit),
                         (b"C3P3", 11, 10, 2, 7, -1, -2, -3))
        self.assertEqual(len(hit), 32)
        self.assertEqual(r.frame_geometry.call_count, 1)

    def test_stale_and_background_are_distinct(self):
        r = self.renderer
        r.packet(10)
        background = struct.unpack(">4sIIIIfff", r.pick(11, 10, 1, 0))
        self.assertEqual(background[3:], (0, 0, 0, 0, 0))
        r.frame_geometry.return_value = (self.geometry[0], bytes(4), bytes(16), bytes(48))
        r.packet(12, yaw=90)
        stale = struct.unpack(">4sIIIIfff", r.pick(13, 10, 0, 1))
        self.assertEqual(stale[2:], (10, 0xffffffff, 0, 0, 0, 0))
        self.assertEqual(struct.unpack(">4sIIIIfff", r.pick(14, 12, 0, 1))[3], 0)

    def test_invalid_requests(self):
        r = self.renderer
        for args in ((1, 1, -1, 0), (1, 1, 2, 0), (1, 1, True, 0),
                     (0, 1, 0, 0), (1, False, 0, 0)):
            with self.assertRaises(ValueError):
                r.pick(*args)
        with self.assertRaises(ValueError):
            r.request(1, op="grade", success=True)


if __name__ == "__main__":
    unittest.main()
