#!/usr/bin/env python3
# Copyright (C) 2026 Thanos Apollo
# SPDX-License-Identifier: GPL-3.0-or-later
"""Local deterministic OBJ scenes -> EGL -> C3D3 color frames and frame-pinned compact picks."""
import argparse
import os
import re
import stat
import json
import math
from pathlib import Path
import struct
import sys

os.environ.setdefault("LP_NUM_THREADS", "2")
import moderngl
import numpy as np


def scene_objects(path):
    """Read a bounded scene manifest, or wrap one OBJ as a single object."""
    path = Path(path)
    if path.suffix.lower() == ".obj":
        return [{"id": "model", "label": path.stem, "path": path}]
    if path.suffix.lower() != ".json" or path.stat().st_size > 65536:
        raise ValueError("Expected OBJ or scene JSON <=64 KiB")
    scene = json.loads(path.read_text())
    if "version" in scene and (type(scene["version"]) is not int or scene["version"] != 2):
        raise ValueError("Unknown scene version")
    objects = scene["objects"]
    if not isinstance(objects, list) or not 1 <= len(objects) <= 255:
        raise ValueError("Scene requires 1..255 objects")
    ids = set()
    result = []
    for obj in objects:
        if not isinstance(obj["id"], str) or not obj["id"] or obj["id"] in ids:
            raise ValueError("Nonempty unique string IDs required")
        if not isinstance(obj["label"], str):
            raise ValueError("String labels required")
        ids.add(obj["id"])
        result.append(dict(obj, path=path.parent / obj["path"]))
    return result


def obj_geometry(path):
    """Return original vertices and file-order fan triangles, ignoring vt/vn.

    Negative indices are relative to vertices defined at that face.  Do not
    merge, reorder, or split geometry according to OBJ groups or materials.
    """
    path = Path(path)
    if path.suffix.lower() != ".obj" or path.stat().st_size > 100_000_000:
        raise ValueError("Expected a local OBJ of at most 100 MB")
    vertices, faces = [], []
    with path.open(encoding="utf-8") as source:
        for line in source:
            words = line.split("#", 1)[0].split()
            if not words:
                continue
            if words[0] == "v":
                if len(words) < 4:
                    raise ValueError("OBJ vertex needs three coordinates")
                vertex = [float(v) for v in words[1:4]]
                if not all(math.isfinite(v) for v in vertex):
                    raise ValueError("Non-finite geometry")
                vertices.append(vertex)
            elif words[0] == "f":
                if len(words) < 4:
                    raise ValueError("OBJ face needs at least three vertices")
                polygon = []
                for word in words[1:]:
                    index = int(word.split("/", 1)[0])
                    resolved = index - 1 if index > 0 else len(vertices) + index
                    if index == 0 or not 0 <= resolved < len(vertices):
                        raise ValueError("Invalid OBJ position index")
                    polygon.append(resolved)
                faces.extend((polygon[0], polygon[i], polygon[i + 1])
                             for i in range(1, len(polygon) - 1))
                if len(faces) > 2_000_000:
                    raise ValueError("At most two million scene triangles supported")
    if not faces:
        raise ValueError("OBJ contains no triangles")
    vertices, faces = np.asarray(vertices, dtype="f8"), np.asarray(faces, dtype="u4")
    triangles = vertices[faces]
    normals = np.cross(triangles[:, 1] - triangles[:, 0], triangles[:, 2] - triangles[:, 0])
    if not np.isfinite(normals).all() or np.any(np.linalg.norm(normals, axis=1) == 0):
        raise ValueError("Degenerate triangles")
    if not np.isfinite(vertices.astype("f4")).all():
        raise ValueError("Geometry exceeds float32 transport range")
    return vertices, faces


class Renderer:
    def __init__(self, path, size=512):
        if not 32 <= size <= 1024:
            raise ValueError("Size must be 32..1024")
        self.objects = scene_objects(path)
        self.meshes = [obj_geometry(obj["path"]) for obj in self.objects]
        if sum(len(faces) for _, faces in self.meshes) > 2_000_000:
            raise ValueError("At most two million scene triangles supported")
        points = np.concatenate([vertices for vertices, _ in self.meshes])
        self.center = (points.max(axis=0) + points.min(axis=0)) / 2
        self.radius = np.linalg.norm(points - self.center, axis=1).max()
        if not math.isfinite(self.radius) or self.radius <= 0:
            raise ValueError("Degenerate geometry")
        self.size = size
        self.ctx = moderngl.create_context(standalone=True, backend="egl", require=330)
        self.program = self.ctx.program(vertex_shader='''#version 330
            uniform mat4 rotation;
            uniform float zoom;
            in vec3 position;
            in vec3 normal;
            in float marked;
            out vec3 n;
            out vec3 normalized_point;
            flat out float region_mark;
            void main() {
                vec4 p = rotation * vec4(position, 1.0);
                gl_Position = vec4(p.xy * zoom * 0.8, -p.z * 0.25, 1.0);
                n = mat3(rotation) * normal;
                normalized_point = position;
                region_mark = marked;
            }
        ''', fragment_shader='''#version 330
            uniform float object_index;
            uniform float selected;
            uniform int highlight_kind;
            uniform bool highlight_mesh;
            uniform vec3 highlight_point;
            uniform float highlight_radius;
            in vec3 n;
            in vec3 normalized_point;
            flat in float region_mark;
            layout(location=0) out vec4 color;
            layout(location=1) out float identity;
            layout(location=2) out uint face;
            layout(location=3) out vec3 surface_point;
            void main() {
                float light = 0.28 + 0.72 * abs(dot(normalize(n), normalize(vec3(-0.4,0.6,1.0))));
                bool marked = highlight_kind == 0 ? object_index == selected :
                    highlight_mesh && (highlight_kind == 1 ||
                    (highlight_kind == 2 && distance(normalized_point, highlight_point) <= highlight_radius) ||
                    (highlight_kind == 3 && region_mark > 0.5));
                color = vec4((marked ? vec3(1.0, 0.82, 0.2) : vec3(0.92, 0.77, 0.59)) * light, 1.0);
                identity = object_index / 255.0;
                face = uint(gl_PrimitiveID) + 1u;
                surface_point = normalized_point;
            }
        ''')
        self.vaos, self.mark_buffers = [], []
        for vertices, faces in self.meshes:
            original = vertices[faces]
            points = (original - self.center) / self.radius
            normals = np.cross(points[:, 1] - points[:, 0], points[:, 2] - points[:, 0])
            normals /= np.linalg.norm(normals, axis=1)[:, None]
            data = np.concatenate((points, np.repeat(normals[:, None, :], 3, axis=1)), axis=2)
            vbo = self.ctx.buffer(data.astype("f4").tobytes())
            marks = self.ctx.buffer(np.zeros(len(faces) * 3, dtype="f4").tobytes())
            self.mark_buffers.append(marks)
            self.vaos.append(self.ctx.vertex_array(self.program,
                             [(vbo, "3f 3f", "position", "normal"),
                              (marks, "1f", "marked")]))
        self.color = self.ctx.texture((size, size), 4)
        self.identity = self.ctx.texture((size, size), 1)
        self.faces = self.ctx.texture((size, size), 1, dtype="u4")
        self.points = self.ctx.texture((size, size), 3, dtype="f4")
        self.framebuffer = self.ctx.framebuffer([self.color, self.identity, self.faces, self.points],
                                               self.ctx.depth_renderbuffer((size, size)))
        self.latest = None
        self.rendered_size = size
        self.ctx.enable(moderngl.DEPTH_TEST)
        print(json.dumps({"renderer": self.ctx.info["GL_RENDERER"],
                          "triangles": sum(len(f) for _, f in self.meshes), "size": size}),
              file=sys.stderr, flush=True)

    def _highlight(self, highlight):
        """Validate geometry and prepare draw state; labels never enter shaders."""
        kind, mesh, point, radius, region = 0, -1, (0, 0, 0), 0, []
        if highlight is not None:
            ids = [obj["id"] for obj in self.objects]
            if not isinstance(highlight, dict) or highlight.get("mesh") not in ids:
                raise ValueError("Invalid highlight mesh")
            mesh = ids.index(highlight["mesh"])
            vertices, faces = self.meshes[mesh]
            kind = {"object": 1, "point": 2, "region": 3}.get(highlight.get("kind"))
            if kind is None:
                raise ValueError("Invalid highlight kind")
            def valid_face(face):
                return type(face) is int and 0 <= face < len(faces)
            if kind == 2:
                face = highlight.get("face")
                bary = highlight.get("barycentric")
                radius = highlight.get("tolerance")
                if (not valid_face(face) or not isinstance(bary, list) or len(bary) != 3
                        or not all(type(v) in (int, float) and math.isfinite(v) and v >= 0 for v in bary)
                        or abs(sum(bary) - 1) > 1e-6
                        or type(radius) not in (int, float) or not math.isfinite(radius) or radius <= 0):
                    raise ValueError("Invalid point highlight")
                point = np.asarray(bary) @ vertices[faces[face]]
                # Match the unrotated normalized positions used for drawing.
                # Original float32 coordinates lose small translated geometry;
                # both highlighting and picking use the normalized surface.
                point = (point - self.center) / self.radius
                radius /= self.radius
            elif kind == 3:
                region = highlight.get("faces")
                if (not isinstance(region, list) or not region or not all(map(valid_face, region))
                        or len(set(region)) != len(region)):
                    raise ValueError("Invalid region highlight")
        self.program["highlight_kind"].value = kind
        self.program["highlight_point"].value = tuple(point)
        self.program["highlight_radius"].value = radius
        if kind == 3:
            marks = np.zeros(len(faces), dtype="f4")
            marks[region] = 1
            self.mark_buffers[mesh].write(np.repeat(marks, 3).tobytes())
        return mesh

    def _draw(self, size, yaw=0, pitch=0, zoom=1, selected=0, highlight=None):
        """Draw attachments, invalidating any previously published frame."""
        self.latest = None
        if type(selected) is not int or not 0 <= selected <= len(self.objects):
            raise ValueError("Invalid selected index")
        self.program["selected"].value = selected
        highlighted_mesh = self._highlight(highlight)
        values = tuple(float(v) for v in (yaw, pitch, zoom))
        if not all(math.isfinite(v) and abs(v) <= 1000000 for v in values) or not 0.25 <= values[2] <= 4:
            raise ValueError("Finite angles and zoom 0.25..4 required")
        y, p = np.radians(values[:2])
        cy, sy, cp, sp = np.cos(y), np.sin(y), np.cos(p), np.sin(p)
        ry = np.array([[cy, 0, sy, 0], [0, 1, 0, 0], [-sy, 0, cy, 0], [0, 0, 0, 1]])
        rx = np.array([[1, 0, 0, 0], [0, cp, -sp, 0], [0, sp, cp, 0], [0, 0, 0, 1]])
        self.program["rotation"].write(np.asarray(ry @ rx, dtype="f4").T.tobytes())
        self.program["zoom"].value = values[2]
        self.framebuffer.use()
        self.framebuffer.clear(0, 0, 0, 1, depth=1)
        for index, vao in enumerate(self.vaos, 1):
            self.program["object_index"].value = index
            self.program["highlight_mesh"].value = index - 1 == highlighted_mesh
            vao.render(moderngl.TRIANGLES)

    def _color_bytes(self):
        """Pack top-down BGRA without NumPy's advanced-indexing copy."""
        size = self.rendered_size
        rgba = np.frombuffer(self.color.read(alignment=1), dtype=np.uint8).reshape(size, size, 4)[::-1]
        bgra = np.empty((size, size, 4), dtype=np.uint8)
        bgra[:, :, 0] = rgba[:, :, 2]
        bgra[:, :, 1] = rgba[:, :, 1]
        bgra[:, :, 2] = rgba[:, :, 0]
        bgra[:, :, 3] = rgba[:, :, 3]
        return bgra.tobytes()

    def frame_geometry(self, yaw=0, pitch=0, zoom=1, selected=0, highlight=None):
        """Return full-size BGRA, mesh bytes, BE uint32 faces+1 and BE xyz floats.

        This legacy readback API draws anew and invalidates packet picking.
        """
        self._draw(self.size, yaw, pitch, zoom, selected, highlight)
        color = self._color_bytes()
        ids = np.frombuffer(self.identity.read(alignment=1), dtype=np.uint8).reshape(self.size, self.size)[::-1]
        faces = np.frombuffer(self.faces.read(alignment=1), dtype="u4").reshape(self.size, self.size)[::-1].copy()
        points = np.frombuffer(self.points.read(alignment=1), dtype="f4").reshape(self.size, self.size, 3)[::-1].copy()
        # Preserve the legacy float32 original-coordinate API.  Compact picks
        # instead retain float64 reconstruction through their wire boundary.
        points = points.astype("f8") * self.radius + self.center
        # Explicitly canonicalize background for all attachment clear formats.
        faces[ids == 0], points[ids == 0] = 0, 0
        return (color, ids.tobytes(),
                faces.astype(">u4").tobytes(), points.astype(">f4").tobytes())

    def frame_pair(self, yaw=0, pitch=0, zoom=1, selected=0):
        """Retain the legacy color/mesh tuple API, not the old wire protocol."""
        return self.frame_geometry(yaw, pitch, zoom, selected)[:2]

    def frame(self, yaw=0, pitch=0, zoom=1):
        return self.frame_pair(yaw, pitch, zoom)[0]

    def packet(self, seq, size=None, **view):
        """Return full-resolution C3D3 color, retaining GPU pick attachments.

        Optional SIZE must match the configured size.
        Only the latest successful packet is pickable until another draw.
        """
        if type(seq) is not int or not 1 <= seq <= 0xffffffff:
            raise ValueError("Sequence must be uint32 > 0")
        if size is not None and (type(size) is not int or size != self.size):
            raise ValueError("Requested size must match configured size")
        self._draw(self.size if size is None else size, **view)
        packet = b"C3D3" + struct.pack(">I", seq) + self._color_bytes()
        self.latest = seq
        return packet

    def pick(self, seq, frame, x, y):
        """Return a 44-byte C3P4 hit from exactly FRAME, never a newer camera.

        XYZ are original coordinates encoded as big-endian IEEE float64.
        Reconstruct on the CPU: float32 original coordinates collapse small
        translated geometry even when its normalized drawing is accurate.
        """
        if (type(seq) is not int or not 1 <= seq <= 0xffffffff
                or type(frame) is not int or not 1 <= frame <= 0xffffffff
                or type(x) is not int or type(y) is not int
                or not 0 <= x < self.rendered_size or not 0 <= y < self.rendered_size):
            raise ValueError("Invalid pick request")
        index, face, point = 0xffffffff, 0, (0, 0, 0)
        if self.latest == frame:
            viewport = (x, self.rendered_size - 1 - y, 1, 1)
            index = self.framebuffer.read(viewport=viewport, components=1,
                                          attachment=1, alignment=1, dtype="f1")[0]
            if index:
                face = struct.unpack("=I", self.framebuffer.read(
                    viewport=viewport, components=1, attachment=2, alignment=1, dtype="u4"))[0]
                point = struct.unpack("=fff", self.framebuffer.read(
                    viewport=viewport, components=3, attachment=3, alignment=1, dtype="f4"))
                point = tuple(value * self.radius + center
                              for value, center in zip(point, self.center))
        return struct.pack(">4sIIIIddd", b"C3P4", seq, frame, index, face, *point)

    def request(self, seq, op="view", **request):
        """Dispatch bounded view/pick requests; neither operation grades."""
        if op == "view":
            return self.packet(seq, **request)
        if op == "pick":
            return self.pick(seq, **request)
        raise ValueError("Unknown renderer operation")

    def close(self):
        self.ctx.release()


class FrameOutput:
    """Publish bounded BGRA files in a private directory owned by the caller.

    Hold an open directory descriptor: retirement cannot recreate a directory
    or redirect later publication to a replacement pathname.  The caller moves
    the sequence-qualified pending file away before the next view.  Never accept paths in
    requests or return them in responses.
    """

    def __init__(self, directory, identity):
        if not re.fullmatch(r"[0-9a-f]{16}", identity or ""):
            raise ValueError("File identity must be 16 lowercase hex digits")
        self.identity = identity.encode("ascii")
        self.pending = None
        self.fd = os.open(directory, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
        info = os.fstat(self.fd)
        if info.st_uid != os.getuid() or stat.S_IMODE(info.st_mode) != 0o700:
            self.close()
            raise ValueError("Frame directory must be private and owned by this user")

    def publish(self, packet):
        if packet[:4] != b"C3D3" or not 8 < len(packet) <= 8 + 4 * 768 * 768:
            raise ValueError("Invalid color packet")
        # Exclusive temporary creation rejects unexpected files/symlinks.  An
        # unacknowledged pending frame must not be silently overwritten either.
        if self.pending:
            try:
                os.stat(self.pending, dir_fd=self.fd, follow_symlinks=False)
            except FileNotFoundError:
                pass
            else:
                raise ValueError("Previous frame has not been consumed")
        seq = struct.unpack(">I", packet[4:8])[0]
        pending = f"pending-{seq}.bgra"
        fd = os.open("writing.bgra", os.O_WRONLY | os.O_CREAT | os.O_EXCL
                     | os.O_NOFOLLOW, 0o600, dir_fd=self.fd)
        try:
            with os.fdopen(fd, "wb") as stream:
                stream.write(memoryview(packet)[8:])
            # The new sequence pathname must not already name a publication.
            os.link("writing.bgra", pending, src_dir_fd=self.fd,
                    dst_dir_fd=self.fd, follow_symlinks=False)
            self.pending = pending
        finally:
            try:
                os.unlink("writing.bgra", dir_fd=self.fd)
            except FileNotFoundError:
                pass
        return b"C3D4" + packet[4:8] + self.identity

    def close(self):
        os.close(self.fd)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("model")
    parser.add_argument("--size", type=int, default=512)
    parser.add_argument("--output", help="Write one raw BGRA frame instead of serving stdin")
    parser.add_argument("--yaw", type=float, default=0)
    parser.add_argument("--pitch", type=float, default=0)
    parser.add_argument("--zoom", type=float, default=1)
    parser.add_argument("--frame-directory", help="Existing private directory for native BGRA")
    parser.add_argument("--frame-identity", help="Caller-owned file transport identity")
    args = parser.parse_args()
    if bool(args.frame_directory) != bool(args.frame_identity) or (args.output and args.frame_directory):
        parser.error("File transport needs both directory and identity, without --output")
    output = FrameOutput(args.frame_directory, args.frame_identity) if args.frame_directory else None
    renderer = None
    try:
        renderer = Renderer(args.model, args.size)
        if args.output:
            Path(args.output).write_bytes(renderer.frame(args.yaw, args.pitch, args.zoom))
        else:
            # Allow a complete bounded scene target plus JSON encoding whitespace.
            limit = 131072
            while line := sys.stdin.buffer.readline(limit + 1):
                if len(line) > limit or not line.endswith(b"\n"):
                    raise ValueError("Invalid or oversized request")
                packet = renderer.request(**json.loads(line))
                if output and packet[:4] == b"C3D3":
                    packet = output.publish(packet)
                sys.stdout.buffer.write(packet)
                sys.stdout.buffer.flush()
    finally:
        if renderer:
            renderer.close()
        if output:
            output.close()


if __name__ == "__main__":
    main()
