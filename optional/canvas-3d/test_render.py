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
from unittest.mock import patch
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

class PublicationTests(unittest.TestCase):
    def test_native_file_script_pixels_identity_pick_and_failures(self):
        identity = "0123456789abcdef"
        command = [sys.executable, "render.py", str(MODEL), "--size", "128"]
        request = b'{"seq":1,"yaw":31,"pitch":-17,"zoom":1.2}\n'
        pick = b'{"seq":2,"op":"pick","frame":1,"x":64,"y":64}\n'
        reference = subprocess.run(command, input=request + pick,
                                   capture_output=True, timeout=20, check=True).stdout
        for failure in (None, "unconsumed", "directory", "identity", "path"):
            with tempfile.TemporaryDirectory() as directory:
                args = command + ["--frame-directory", directory, "--frame-identity", identity]
                if failure == "directory":
                    Path(directory).chmod(0o755)
                if failure == "identity":
                    args[-1] = "../bad"
                requests = request + pick
                if failure == "unconsumed":
                    requests += b'{"seq":3}\n'
                if failure == "path":
                    requests = b'{"seq":1,"path":"/tmp/unowned.bgra"}\n'
                result = subprocess.run(args, input=requests, capture_output=True, timeout=20)
                files = list(Path(directory).iterdir())
                if failure in ("directory", "identity", "path"):
                    self.assertNotEqual(result.returncode, 0)
                    self.assertEqual(result.stdout, b"")
                    self.assertEqual(files, [])
                else:
                    self.assertEqual(result.returncode == 0, failure is None, result.stderr)
                    self.assertEqual(result.stdout[:24], b"C3D4" + struct.pack(">I", 1) + identity.encode())
                    self.assertEqual(result.stdout[24:], reference[-32:])
                    self.assertEqual([p.name for p in files], ["pending-1.bgra"])
                    self.assertEqual(files[0].read_bytes(), reference[8:-32])
                    self.assertEqual(files[0].stat().st_size, 4 * 128 * 128)

    def test_full_resolution_geometry_and_frame_ownership(self):
        view = dict(yaw=31, pitch=-17, zoom=1.2)
        for size in (128, 256, 768):
            r = Renderer(MODEL, size)
            try:
                color, ids, faces, points = r.frame_geometry(**view)
                framebuffer = r.framebuffer
                full = r.packet(1)
                for seq in range(2, 5):
                    packet = r.packet(seq, size=size, **view)
                    self.assertEqual(len(packet), 8 + 4 * size * size)
                    self.assertEqual(packet[:8], b"C3D3" + struct.pack(">I", seq))
                    self.assertIs(r.framebuffer, framebuffer)
                    self.assertEqual(packet[8:], color)
                    for x, y in ((0, 0), (size-1, size-1), (size//2, size//2),
                                 (size//2, size//3), (size//3, size//2)):
                        hit = struct.unpack(">4sIIIIfff", r.pick(100, seq, x, y))
                        pixel = y * size + x
                        self.assertEqual(hit[3], ids[pixel])
                        self.assertEqual(hit[4], struct.unpack_from(">I", faces, pixel*4)[0])
                        self.assertEqual(hit[5:], struct.unpack_from(">fff", points, pixel*12))
                    self.assertEqual(struct.unpack(">4sIIIIfff", r.pick(100, seq-1, 0, 0))[3], 0xffffffff)
                    with self.assertRaises(ValueError):
                        r.pick(100, seq, size, 0)
                self.assertEqual(r.packet(20)[8:], full[8:])
                r.frame_geometry(yaw=90)
                self.assertEqual(struct.unpack(">4sIIIIfff", r.pick(21, 20, 64, 64))[3], 0xffffffff)
            finally:
                r.close()

    def test_packet_reads_only_color_and_failed_draw_retires_pick(self):
        r = Renderer(MODEL, 128)
        try:
            with (patch.object(r.identity, "read", side_effect=AssertionError("full ID read")),
                  patch.object(r.faces, "read", side_effect=AssertionError("full face read")),
                  patch.object(r.points, "read", side_effect=AssertionError("full point read")),
                  patch.object(r.framebuffer, "read", wraps=r.framebuffer.read) as read):
                r.packet(1)
                read.assert_not_called()
                self.assertEqual(struct.unpack(">4sIIIIfff", r.pick(2, 1, 64, 64))[3], 1)
                self.assertEqual(read.call_count, 3)
                for args in read.call_args_list:
                    self.assertEqual(args.kwargs["viewport"], (64, 63, 1, 1))
                with patch.object(r, "_color_bytes", side_effect=RuntimeError("readback")):
                    with self.assertRaises(RuntimeError):
                        r.packet(3, yaw=90)
                read.reset_mock()
                self.assertEqual(struct.unpack(">4sIIIIfff", r.pick(4, 1, 64, 64))[3], 0xffffffff)
                self.assertEqual(struct.unpack(">4sIIIIfff", r.pick(5, 3, 64, 64))[3], 0xffffffff)
                read.assert_not_called()
        finally:
            r.close()

    def test_stream_full_resolution_and_picks(self):
        requests = (b'{"seq":1,"size":256}\n'
                    b'{"seq":2,"op":"pick","frame":1,"x":128,"y":128}\n'
                    b'{"seq":3}\n'
                    b'{"seq":4,"op":"pick","frame":1,"x":128,"y":128}\n')
        result = subprocess.run([sys.executable, "render.py", str(MODEL), "--size", "256"],
                                input=requests, capture_output=True, timeout=20)
        self.assertEqual(result.returncode, 0, result.stderr)
        first = second = 8 + 4*256*256
        self.assertEqual(len(result.stdout), first + second + 64)
        self.assertEqual(struct.unpack(">4sIIIIfff", result.stdout[first:first+32])[:4],
                         (b"C3P3", 2, 1, 1))
        self.assertEqual(result.stdout[first+32:first+40], b"C3D3" + struct.pack(">I", 3))
        self.assertEqual(struct.unpack(">4sIIIIfff", result.stdout[-32:])[:4],
                         (b"C3P3", 4, 1, 0xffffffff))


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
