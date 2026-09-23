"""Uniform tiny scales preserve real EGL pixels and original-coordinate picks."""
# Copyright (C) 2026 Thanos Apollo
# SPDX-License-Identifier: GPL-3.0-or-later
import json
from pathlib import Path
import struct
import tempfile
import unittest

import numpy as np
from render import Renderer, obj_geometry


class NumericTests(unittest.TestCase):
    def test_tiny_scene_pixels_highlights_and_compact_picks(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            reference = {}
            for scale, tolerance in ((1., .3), (1e-40, 3e-41),
                                     (1e-90, 3e-91), (1e-200, 3e-201)):
                with self.subTest(scale=scale):
                    vertices = np.array([[-1., -1., 0.], [1., -1., 0.], [0., 1., 0.]]) * scale
                    obj = root / "mesh.obj"
                    obj.write_text("".join(f"v {x} {y} {z}\n" for x, y, z in vertices) + "f 1 2 3\n")
                    target = dict(id="point", label="Point", kind="point", mesh="mesh", face=0,
                                  barycentric=[.25, .25, .5], tolerance=tolerance)
                    scene = root / "scene.json"
                    scene.write_text(json.dumps(dict(version=2, initial_view=[0, 0, 1],
                        objects=[dict(id="mesh", label="Mesh", path="mesh.obj")], targets=[target],
                        license="GPL-3.0-or-later", source="synthetic")))
                    original = obj.read_bytes(), scene.read_bytes()
                    renderer = Renderer(scene, 64)
                    try:
                        np.testing.assert_array_equal(renderer.meshes[0][0], vertices)
                        np.testing.assert_array_equal(renderer.meshes[0][1], [[0, 1, 2]])
                        self.assertTrue(np.isfinite(renderer.radius))
                        self.assertGreater(renderer.radius, 0)
                        for view in ((0, 0, 1), (35, 20, 1.4)):
                            yaw, pitch, zoom = view
                            base = renderer.packet(1, yaw=yaw, pitch=pitch, zoom=zoom)
                            marked = renderer.packet(2, yaw=yaw, pitch=pitch, zoom=zoom, highlight=target)
                            self.assertEqual(base[:8], b"C3D3" + struct.pack(">I", 1))
                            self.assertEqual(len(base), 8 + 64 * 64 * 4)
                            changed = (np.frombuffer(base[8:], "u1").reshape(64, 64, 4)
                                       != np.frombuffer(marked[8:], "u1").reshape(64, 64, 4)).any(axis=2)
                            points, identities = [], []
                            for row in range(64):
                                for col in range(64):
                                    pick = struct.unpack(">4sIIIIddd", renderer.pick(3, 2, col, row))
                                    self.assertEqual(pick[:3], (b"C3P4", 3, 2))
                                    self.assertIn(pick[3], (0, 1))
                                    self.assertEqual(pick[4], pick[3])
                                    point = np.asarray(pick[5:]) / scale
                                    self.assertTrue(np.isfinite(point).all())
                                    identities.append(pick[3:5])
                                    points.append(point)
                                    # Unit-space distance is independent of both the
                                    # renderer's radius and underflowing squared norms.
                                    expected = bool(pick[3]) and np.hypot.reduce(point) <= .3
                                    self.assertEqual(bool(changed[row, col]), expected)
                                    if not pick[3]:
                                        self.assertEqual(pick[5:], (0, 0, 0))
                            center = struct.unpack(">4sIIIIddd", renderer.pick(4, 2, 32, 32))
                            self.assertEqual(center[3:5], (1, 1))
                            self.assertTrue(changed[32, 32])
                            self.assertTrue(changed.any())
                            self.assertGreater(sum(item[0] for item in identities), int(changed.sum()))
                            if scale == 1:
                                reference[view] = (base, marked, identities, points)
                            else:
                                a, b, ids, xyz = reference[view]
                                self.assertEqual(base, a)
                                self.assertEqual(marked, b)
                                self.assertEqual(identities, ids)
                                np.testing.assert_allclose(points, xyz, rtol=0, atol=2e-7)
                    finally:
                        renderer.close()
                    self.assertEqual((obj.read_bytes(), scene.read_bytes()), original)

    def test_collinear_and_repeated_vertices_remain_invalid(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "mesh.obj"
            for scale in (1., 1e-40, 1e-90, 1e-200):
                for face in ("1 2 3", "2 3 1", "3 1 2", "1 1 2"):
                    with self.subTest(scale=scale, face=face):
                        path.write_text(f"v 0 0 0\nv {scale} {scale} {scale}\n"
                                        f"v {2 * scale} {2 * scale} {2 * scale}\nf {face}\n")
                        with self.assertRaisesRegex(ValueError, "Degenerate triangles"):
                            obj_geometry(path)
            # An arbitrary scale divisor can round this exactly collinear
            # integer geometry into a spurious nonzero cross product.
            path.write_text("v 0 0 0\nv 5617 1732 9116\nv 28085 8660 45580\nf 1 2 3\n")
            with self.assertRaisesRegex(ValueError, "Degenerate triangles"):
                obj_geometry(path)

    def test_thin_nonzero_triangles_are_not_rejected_by_an_epsilon(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "mesh.obj"
            for x, y in ((1., 1e-200), (1e-200, 1e-209)):
                for face in ("1 2 3", "2 3 1", "3 1 2"):
                    with self.subTest(x=x, y=y, face=face):
                        path.write_text(f"v 0 0 0\nv {x} 0 0\nv {x} {y} 0\nf {face}\n")
                        vertices, faces = obj_geometry(path)
                        np.testing.assert_array_equal(vertices, [[0, 0, 0], [x, 0, 0], [x, y, 0]])
                        np.testing.assert_array_equal(faces, [[int(n) - 1 for n in face.split()]])


if __name__ == "__main__":
    unittest.main()
