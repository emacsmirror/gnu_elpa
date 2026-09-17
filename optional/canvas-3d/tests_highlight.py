"""Real EGL region-mask work, stale storage and exact draw/pick invariants."""
# Copyright (C) 2026 Thanos Apollo
# SPDX-License-Identifier: GPL-3.0-or-later
import contextlib
import json
from pathlib import Path
import struct
import tempfile
import unittest
from unittest.mock import patch

import numpy as np
from render import Renderer


def region(mesh, face):
    return {"kind": "region", "mesh": mesh, "faces": [face]}


class HighlightTests(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        root = Path(directory.name)
        for name, left, right in (("left", -2, -.25), ("right", .25, 2)):
            (root / f"{name}.obj").write_text(
                f"v {left} -1 0\nv {right} -1 0\nv {right} 1 0\nv {left} 1 0\nf 1 2 3\nf 1 3 4\n")
        scene = root / "scene.json"
        scene.write_text(json.dumps({"version": 2, "objects": [
            {"id": name, "label": name, "path": f"{name}.obj"} for name in ("left", "right")]}))
        self.renderer = Renderer(scene, 32)
        self.addCleanup(self.renderer.close)

    @staticmethod
    def cases():
        return [
            ({}, None),
            ({"highlight": region("left", 0)}, None),
            ({"highlight": region("right", 1)}, None),
            ({}, None),
            ({"selected": 2}, None),
            ({"highlight": {"kind": "object", "mesh": "left"}}, None),
            ({"highlight": {"kind": "point", "mesh": "right", "face": 0,
                            "barycentric": [.25, .25, .5], "tolerance": .35}}, None),
            ({"highlight": region("left", 1)}, None),
            ({"highlight": region("left", 0)}, None),
            ({"highlight": region("right", 0)}, None),
            ({"highlight": region("right", 1), "yaw": 31, "pitch": -17, "zoom": 1.2}, None),
            ({"highlight": region("missing", 0)}, "Invalid highlight mesh"),
            ({"highlight": {"kind": "missing", "mesh": "left"}}, "Invalid highlight kind"),
            ({"highlight": {"kind": "region", "mesh": "left", "faces": []}}, "Invalid region highlight"),
            ({"highlight": {"kind": "region", "mesh": "left", "faces": [0, 0]}}, "Invalid region highlight"),
            ({"highlight": region("left", 2)}, "Invalid region highlight"),
            ({"highlight": region("left", True)}, "Invalid region highlight"),
            ({"highlight": {"kind": "point", "mesh": "right", "face": 0,
                            "barycentric": [1, 1, 1], "tolerance": .35}}, "Invalid point highlight"),
            ({"selected": 3}, "Invalid selected index"),
            ({"highlight": region("left", 0), "yaw": float("nan")}, "Finite angles and zoom 0.25..4 required"),
            ({"highlight": region("left", 1)}, None),
            ({}, None),
            ({"highlight": region("right", 1)}, None),
        ]

    def test_only_selected_region_uploads_complete_mask(self):
        renderer = self.renderer
        with contextlib.ExitStack() as stack:
            spies = [stack.enter_context(patch.object(buffer, "write", wraps=buffer.write))
                     for buffer in renderer.mark_buffers]
            for seq, (view, error) in enumerate(self.cases(), 1):
                with self.subTest(seq=seq, view=view):
                    before = [buffer.read() for buffer in renderer.mark_buffers]
                    for spy in spies:
                        spy.reset_mock()
                    if error:
                        with self.assertRaisesRegex(ValueError, error):
                            renderer.packet(seq, **view)
                    else:
                        renderer.packet(seq, **view)
                    target = view.get("highlight", {})
                    # Camera validation follows valid highlight setup, including its upload.
                    upload = target.get("kind") == "region" and (error is None or "yaw" in view)
                    selected = ("left", "right").index(target["mesh"]) if upload else None
                    for index, (buffer, spy) in enumerate(zip(renderer.mark_buffers, spies)):
                        if index == selected:
                            expected = np.zeros(2, dtype="f4")
                            expected[target["faces"]] = 1
                            complete = np.repeat(expected, 3).tobytes()
                            spy.assert_called_once_with(complete)
                            self.assertEqual(buffer.read(), complete)
                        else:
                            spy.assert_not_called()
                            self.assertEqual(buffer.read(), before[index])
                    if seq == 4:
                        # Both nonempty masks really survive a no-highlight frame.
                        self.assertTrue(all(np.frombuffer(buffer.read(), "f4").any()
                                            for buffer in renderer.mark_buffers))

    def capture(self, seq, view):
        renderer = self.renderer
        color = renderer.packet(seq, **view)[8:]
        attachments = tuple(texture.read(alignment=1) for texture in
                            (renderer.identity, renderer.faces, renderer.points))
        picks = []
        for y in range(renderer.size):
            for x in range(renderer.size):
                pick = renderer.pick(1000, seq, x, y)
                self.assertEqual(pick[:12], struct.pack(">4sII", b"C3P4", 1000, seq))
                picks.append(pick[12:])
        return color, attachments, b"".join(picks)

    def test_stale_masks_preserve_exact_pixels_and_all_picks(self):
        renderer = self.renderer
        references = {}
        # Store independent camera references before any region writes.  Only one
        # EGL context is live; no cross-context current-state assumptions are made.
        for camera in ({}, {"yaw": 31, "pitch": -17, "zoom": 1.2}):
            base, attachments, picks = self.capture(100, camera)
            fields = np.frombuffer(picks, dtype=np.dtype([
                ("mesh", ">u4"), ("face", ">u4"), ("point", ">f8", (3,))]))
            marked = np.frombuffer(base, "u1").reshape(-1, 4).copy()
            for index, mesh in enumerate(("left", "right"), 1):
                color = renderer.packet(101, **camera, highlight={"kind": "object", "mesh": mesh})[8:]
                mask = fields["mesh"] == index
                marked[mask] = np.frombuffer(color, "u1").reshape(-1, 4)[mask]
                for face in (1, 2):
                    self.assertTrue(((fields["mesh"] == index) & (fields["face"] == face)).any())
            references[bool(camera)] = base, marked, attachments, picks, fields
        previous = 101
        for seq, (view, error) in enumerate(self.cases(), 1):
            with self.subTest(seq=seq, view=view):
                if error:
                    with self.assertRaisesRegex(ValueError, error):
                        renderer.packet(seq, **view)
                    self.assertIsNone(renderer.latest)
                    self.assertEqual(struct.unpack(">4sIIIIddd", renderer.pick(1000, previous, 0, 0)),
                                     (b"C3P4", 1000, previous, 0xffffffff, 0, 0, 0, 0))
                    continue
                color, attachments, picks = self.capture(seq, view)
                base, marked, expected_attachments, expected_picks, fields = references["yaw" in view]
                self.assertEqual(attachments, expected_attachments)
                self.assertEqual(picks, expected_picks)
                target = view.get("highlight")
                mask = fields["mesh"] == view.get("selected", -1)
                if target:
                    mesh = ("left", "right").index(target["mesh"])
                    mask = fields["mesh"] == mesh + 1
                    if target["kind"] == "region":
                        mask &= np.isin(fields["face"], np.asarray(target["faces"]) + 1)
                    elif target["kind"] == "point":
                        vertices, faces = renderer.meshes[mesh]
                        point = np.asarray(target["barycentric"]) @ vertices[faces[target["face"]]]
                        mask &= np.linalg.norm(fields["point"] - point, axis=1) <= target["tolerance"]
                expected = np.frombuffer(base, "u1").reshape(-1, 4).copy()
                expected[mask] = marked[mask]
                # Exact pixel membership kills both no-upload and accumulating-mask
                # regressions, especially the same-mesh face 0 -> face 1 change.
                self.assertEqual(color, expected.tobytes())
                if target or view.get("selected"):
                    self.assertTrue(mask.any())
                    self.assertNotEqual(color, base)
                self.assertEqual(renderer.latest, seq)
                self.assertEqual(struct.unpack(">4sIIIIddd", renderer.pick(1000, seq + 100, 0, 0)),
                                 (b"C3P4", 1000, seq + 100, 0xffffffff, 0, 0, 0, 0))
                previous = seq


if __name__ == "__main__":
    unittest.main()
