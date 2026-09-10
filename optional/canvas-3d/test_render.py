"""Real EGL framebuffer and subprocess protocol checks."""
# Copyright (C) 2026 Thanos Apollo
# SPDX-License-Identifier: GPL-3.0-or-later
import hashlib
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import struct
import unittest
import numpy as np
from render import Renderer

MODEL = Path(__file__).parent / "fixtures/pyramid.obj"

class RenderTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.renderer = Renderer(MODEL, 128)

    @classmethod
    def tearDownClass(cls):
        cls.renderer.close()

    def test_frame_rotation_zoom_and_reset(self):
        r = self.renderer
        original = r.frame()
        self.assertEqual(len(original), 128 * 128 * 4)
        pixels = np.frombuffer(original, dtype=np.uint8).reshape(128, 128, 4)
        self.assertTrue((pixels[:, :, 3] == 255).all())
        self.assertGreater(len(np.unique(pixels.reshape(-1, 4), axis=0)), 3)
        self.assertNotEqual(original, r.frame(yaw=40, pitch=20))
        self.assertNotEqual(original, r.frame(zoom=1.5))
        self.assertEqual(original, r.frame())
        print("synthetic pyramid frame SHA256:", hashlib.sha256(original).hexdigest())

    def test_bounds(self):
        for args in ({"zoom": 0}, {"zoom": 5}, {"yaw": float("nan")}):
            with self.assertRaises(ValueError):
                self.renderer.frame(**args)

    def test_stream_two_frames_and_eof(self):
        result = subprocess.run([sys.executable, "render.py", str(MODEL), "--size", "64"],
                                input=b'{"seq":1}\n{"seq":2,"yaw":90}\n', capture_output=True, timeout=20)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(len(result.stdout), 2 * (8 + 64 * 64 * 4))
        a, b = np.frombuffer(result.stdout, dtype=np.uint8).reshape(2, -1)
        self.assertFalse(np.array_equal(a, b))

    def test_bad_request_fails_cleanly(self):
        result = subprocess.run([sys.executable, "render.py", str(MODEL), "--size", "64"],
                                input=b'{"seq":1,"zoom": 0}\n', capture_output=True, timeout=20)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, b"")
        self.assertIn(b"zoom", result.stderr)

class SceneTests(unittest.TestCase):
    def test_scene_joint_coordinates_and_highlight(self):
        r = Renderer(MODEL.parent / "scene.json", 128)
        try:
            self.assertEqual([o["id"] for o in r.objects], ["left", "right"])
            color, ids = r.frame_pair(pitch=0)
            self.assertEqual(set(ids), {0, 1, 2})
            marked, same_ids = r.frame_pair(pitch=0, selected=1)
            self.assertEqual(ids, same_ids)
            self.assertNotEqual(color, marked)
            a = np.frombuffer(color, dtype=np.uint8).reshape(-1, 4)
            b = np.frombuffer(marked, dtype=np.uint8).reshape(-1, 4)
            self.assertTrue(np.array_equal(a[np.frombuffer(ids, dtype=np.uint8) != 1],
                                           b[np.frombuffer(ids, dtype=np.uint8) != 1]))
        finally:
            r.close()

    def test_depth_overlap_background_and_joint_normalization(self):
        with tempfile.TemporaryDirectory() as d:
            root = Path(d)
            # Identical xy triangles, front z=1; deliberately draw rear second.
            for name, z in (("front", 1), ("rear", -1)):
                (root / (name + ".obj")).write_text(
                    f"v -1 -1 {z}\nv 1 -1 {z}\nv 0 1 {z}\nf 1 2 3\n")
            (root / "scene.json").write_text(json.dumps({"objects": [
                {"id": name, "label": name, "path": name + ".obj"}
                for name in ("front", "rear")]}))
            r = Renderer(root / "scene.json", 64)
            try:
                _, ids = r.frame_pair()
                self.assertEqual(ids[32 * 64 + 32], 1)
                self.assertEqual(ids[0], 0)
                _, reverse = r.frame_pair(yaw=180)
                self.assertEqual(reverse[32 * 64 + 32], 2)
                np.testing.assert_equal(r.center, [0, 0, 0])
                self.assertAlmostEqual(r.radius, 3 ** 0.5)
            finally:
                r.close()

    def test_protocol_caps_and_sequence(self):
        for request in (b"x" * 4097, b'{"seq":0}\n', b'{"seq":1}'):
            result = subprocess.run([sys.executable, "render.py", str(MODEL), "--size", "32"],
                                    input=request, capture_output=True, timeout=20)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(result.stdout, b"")

if __name__ == "__main__":
    unittest.main()
