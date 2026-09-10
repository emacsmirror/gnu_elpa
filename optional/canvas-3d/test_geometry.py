"""Deterministic topology, real depth/coordinate attachments and locked highlights."""
# Copyright (C) 2026 Thanos Apollo
# SPDX-License-Identifier: GPL-3.0-or-later
import json
from pathlib import Path
import struct
import tempfile
import unittest

import numpy as np
from render import Renderer, obj_geometry


class GeometryTests(unittest.TestCase):
    def test_obj_fan_groups_negative_and_invalid_geometry(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "mesh.obj"
            path.write_text("v 10 20 30\nv 12 20 30\nv 12 22 30\nv 10 22 30\n"
                            "g one\nf 1/4/2 2/3/1 3/2/4 4/1/3 # quad\n"
                            "usemtl ignored\ng two\nf -4 -2 -1\n")
            vertices, faces = obj_geometry(path)
            np.testing.assert_array_equal(faces, [[0, 1, 2], [0, 2, 3], [0, 2, 3]])
            np.testing.assert_array_equal(vertices[0], [10, 20, 30])
            for suffix in ("f 0 2 3", "f 1 2 5", "f -5 -2 -1", "f 1 1 2",
                           "f 1 2", "v nan 0 0", "v inf 0 0"):
                path.write_text("v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n" + suffix + "\n")
                with self.subTest(suffix=suffix), self.assertRaises(ValueError):
                    obj_geometry(path)

    @staticmethod
    def decode(frame, size):
        color, ids, faces, points = frame
        return (np.frombuffer(color, "u1").reshape(size, size, 4),
                np.frombuffer(ids, "u1").reshape(size, size),
                np.frombuffer(faces, ">u4").reshape(size, size),
                np.frombuffer(points, ">f4").reshape(size, size, 3))

    def test_high_face_depth_original_coordinates_and_packet(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "mesh.obj"
            # Face 70000 wins over 70000 earlier triangles of the SAME object.
            path.write_text("v 9 19 29\nv 11 19 29\nv 10 21 29\n"
                            "v 9 19 31\nv 11 19 31\nv 10 21 31\n"
                            + "f 1 2 3\n" * 70000 + "f 4 5 6\n")
            renderer = Renderer(path, 64)
            try:
                packet = renderer.packet(0x12345678)
                self.assertEqual(packet[:8], b"C3D3" + struct.pack(">I", 0x12345678))
                self.assertEqual(len(packet), 8 + 4 * 64 * 64)
                for seq, (yaw, pitch, zoom) in enumerate(((0, 0, 1), (25, 15, 1.4), (180, 0, 1)), 1):
                    renderer.packet(seq, yaw=yaw, pitch=pitch, zoom=zoom)
                    _, raw_ids, raw_faces, raw_points = renderer.latest
                    _, ids, faces, points = self.decode((bytes(4 * 64 * 64), raw_ids, raw_faces, raw_points), 64)
                    picked = struct.unpack(">4sIIIIfff", renderer.pick(seq + 100, seq, 32, 32))
                    self.assertEqual(picked[:5], (b"C3P3", seq + 100, seq, int(ids[32, 32]), int(faces[32, 32])))
                    np.testing.assert_allclose(picked[5:], points[32, 32])
                    stale = struct.unpack(">4sIIIIfff", renderer.pick(seq + 101, seq + 10, 32, 32))
                    self.assertEqual(stale[3], 0xffffffff)
                    hit = ids != 0
                    self.assertTrue(hit.any())
                    self.assertTrue((faces[~hit] == 0).all())
                    self.assertTrue((points[~hit] == 0).all())
                    # Original xyz, transformed independently, must land at its pixel center.
                    y, p = np.radians([yaw, pitch])
                    ry = np.array([[np.cos(y), 0, np.sin(y)], [0, 1, 0], [-np.sin(y), 0, np.cos(y)]])
                    rx = np.array([[1, 0, 0], [0, np.cos(p), -np.sin(p)], [0, np.sin(p), np.cos(p)]])
                    projected = ((points[hit] - renderer.center) / renderer.radius) @ (ry @ rx).T
                    rows, cols = np.nonzero(hit)
                    np.testing.assert_allclose(projected[:, 0] * zoom * .8, (cols + .5) / 32 - 1, atol=1e-5)
                    np.testing.assert_allclose(projected[:, 1] * zoom * .8, 1 - (rows + .5) / 32, atol=1e-5)
                    if yaw == 0:
                        self.assertEqual(int(faces[32, 32]), 70001)
                        self.assertAlmostEqual(float(points[32, 32, 2]), 31)
                    elif yaw == 180:
                        self.assertEqual(int(faces[32, 32]), 1)
                        self.assertAlmostEqual(float(points[32, 32, 2]), 29)
                # Region uses the high face index directly; no byte overloading.
                base = renderer.frame_geometry()
                marked = renderer.frame_geometry(highlight={"kind": "region", "mesh": "model", "faces": [70000]})
                self.assertNotEqual(base[0], marked[0])
                self.assertEqual(base[1:], marked[1:])
            finally:
                renderer.close()

    def test_point_highlight_scale_invariance_and_original_picking(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "mesh.obj"
            # Tiny geometry is accepted by the managed domain; the large scale
            # also exercises the standalone renderer's float32 distance overflow.
            for scale in (1.0, 1e-20, 1e20):
                for offset in ((0, 0, 0), (4, -3, 2)):
                    with self.subTest(scale=scale, offset=offset):
                        vertices = (np.array([[-1, -1, 0], [1, -1, 0], [0, 1, 0]])
                                    + offset) * scale
                        path.write_text("".join(f"v {x} {y} {z}\n" for x, y, z in vertices)
                                        + "f 1 2 3\n")
                        renderer = Renderer(path, 64)
                        try:
                            target = {"kind": "point", "mesh": "model", "face": 0,
                                      "barycentric": [.25, .25, .5], "tolerance": .3 * scale}
                            center = np.asarray(target["barycentric"]) @ vertices
                            for yaw, pitch, zoom in ((0, 0, 1), (35, 20, 1.4)):
                                base = renderer.frame_geometry(yaw, pitch, zoom)
                                marked = renderer.frame_geometry(yaw, pitch, zoom, highlight=target)
                                self.assertEqual(base[1:], marked[1:])
                                color, ids, faces, points = self.decode(base, 64)
                                highlighted, _, _, _ = self.decode(marked, 64)
                                hit = ids != 0
                                self.assertTrue(hit.any())
                                self.assertTrue(np.isfinite(points).all())
                                self.assertTrue((faces[hit] == 1).all())
                                np.testing.assert_allclose(points[hit, 2].astype("f8") / scale,
                                                           offset[2], atol=1e-6)
                                # Compute reference distance in float64 ORIGINAL units.
                                expected = hit & (np.linalg.norm(points.astype("f8") - center,
                                                                axis=2) <= target["tolerance"])
                                self.assertTrue(expected.any())
                                self.assertTrue((hit & ~expected).any())
                                changed = (color != highlighted).any(axis=2)
                                np.testing.assert_array_equal(changed, expected)
                        finally:
                            renderer.close()

    def test_geometry_only_highlights_camera_reset_and_occlusion(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for name, z in (("front", 1), ("rear", -1)):
                (root / f"{name}.obj").write_text(
                    f"v -1 -1 {z}\nv 1 -1 {z}\nv 1 1 {z}\nv -1 1 {z}\nf 1 2 3 4\n")
            scene = root / "scene.json"
            scene.write_text(json.dumps({"version": 2, "objects": [
                {"id": name, "label": "SECRET", "path": f"{name}.obj"} for name in ("front", "rear")]}))
            renderer = Renderer(scene, 128)
            try:
                targets = [{"kind": "object", "mesh": "front"},
                           {"kind": "region", "mesh": "front", "faces": [1]},
                           {"kind": "point", "mesh": "front", "face": 0,
                            "barycentric": [.25, .25, .5], "tolerance": .3}]
                for target in targets:
                    initial = renderer.frame_geometry(highlight=target)
                    for yaw, pitch, zoom in ((0, 0, 1), (35, 20, 1.4), (180, 0, 1), (0, 0, 1)):
                        base = renderer.frame_geometry(yaw, pitch, zoom)
                        marked = renderer.frame_geometry(yaw, pitch, zoom, selected=2, highlight=target)
                        a, ids, faces, points = self.decode(base, 128)
                        b, _, _, _ = self.decode(marked, 128)
                        self.assertEqual(base[1:], marked[1:])
                        changed = (a != b).any(axis=2)
                        expected = ids == 1
                        if target["kind"] == "region":
                            expected &= faces == 2
                        elif target["kind"] == "point":
                            expected &= np.linalg.norm(points - [.5, 0, 1], axis=2) <= .3
                        np.testing.assert_array_equal(changed, expected)
                        if yaw == 0 and zoom == 1:
                            self.assertEqual(initial, marked)
                    self.assertEqual(target.get("label"), None)
                for target in ({"mesh": "no", "kind": "object"},
                               {"mesh": "front", "kind": "region", "faces": [65536]},
                               {"mesh": "front", "kind": "region", "faces": [0, 0]},
                               {"mesh": "front", "kind": "point", "face": 0,
                                "barycentric": [1, 1, 1], "tolerance": .1}):
                    with self.assertRaises(ValueError):
                        renderer.frame_geometry(highlight=target)
            finally:
                renderer.close()


if __name__ == "__main__":
    unittest.main()
