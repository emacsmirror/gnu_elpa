#!/usr/bin/env python3
# Copyright (C) 2026 Thanos Apollo
# SPDX-License-Identifier: GPL-3.0-or-later
"""Local scene -> EGL -> framed BGRA and depth-tested object-index pixels."""
import argparse
import json
import math
import os
from pathlib import Path
import struct
import sys

os.environ.setdefault("LP_NUM_THREADS", "2")
import moderngl
import numpy as np
import trimesh


def scene_objects(path):
    """Read a bounded scene manifest, or wrap one OBJ as a single object."""
    path = Path(path)
    if path.suffix.lower() == ".obj":
        return [{"id": "model", "label": path.stem, "path": path}]
    if path.suffix.lower() != ".json" or path.stat().st_size > 65536:
        raise ValueError("Expected OBJ or scene JSON <=64 KiB")
    objects = json.loads(path.read_text())["objects"]
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


class Renderer:
    def __init__(self, path, size=512):
        if not 32 <= size <= 1024:
            raise ValueError("Size must be 32..1024")
        self.objects = scene_objects(path)
        meshes = []
        for obj in self.objects:
            path = obj["path"]
            if path.suffix.lower() != ".obj" or path.stat().st_size > 100_000_000:
                raise ValueError("Expected a local OBJ of at most 100 MB")
            with path.open("r", encoding="utf-8") as source:
                mesh = trimesh.load(source, file_type="obj", force="mesh",
                                    process=False, skip_materials=True)
            if not isinstance(mesh, trimesh.Trimesh) or not len(mesh.faces):
                raise ValueError("OBJ contains no triangles")
            if not np.isfinite(mesh.vertices).all():
                raise ValueError("Non-finite geometry")
            meshes.append(mesh)
        if sum(len(m.faces) for m in meshes) > 2_000_000:
            raise ValueError("At most two million scene triangles supported")
        # One transform for the entire assembly preserves original relative coordinates.
        points = np.concatenate([m.vertices for m in meshes])
        self.center = (points.max(axis=0) + points.min(axis=0)) / 2
        self.radius = np.linalg.norm(points - self.center, axis=1).max()
        if self.radius <= 0:
            raise ValueError("Degenerate geometry")
        self.size = size
        self.ctx = moderngl.create_context(standalone=True, backend="egl", require=330)
        self.program = self.ctx.program(vertex_shader='''#version 330
            uniform mat4 rotation;
            uniform float zoom;
            in vec3 position;
            in vec3 normal;
            out vec3 n;
            void main() {
                vec4 p = rotation * vec4(position, 1.0);
                gl_Position = vec4(p.xy * zoom * 0.8, -p.z * 0.25, 1.0);
                n = mat3(rotation) * normal;
            }
        ''', fragment_shader='''#version 330
            uniform float object_index;
            uniform float selected;
            in vec3 n;
            layout(location=0) out vec4 color;
            layout(location=1) out float identity;
            void main() {
                float light = 0.28 + 0.72 * abs(dot(normalize(n), normalize(vec3(-0.4,0.6,1.0))));
                color = vec4((object_index == selected ? vec3(1.0, 0.82, 0.2) : vec3(0.92, 0.77, 0.59)) * light, 1.0);
                identity = object_index / 255.0;
            }
        ''')
        self.vaos = []
        for mesh in meshes:
            points = ((mesh.vertices - self.center) / self.radius).astype("f4")
            normals = np.zeros_like(points)
            contributions = mesh.face_normals[:, None, :] * mesh.face_angles[:, :, None]
            np.add.at(normals, mesh.faces.ravel(), contributions.reshape(-1, 3))
            vertices = np.column_stack((points, trimesh.util.unitize(normals))).astype("f4")
            vbo = self.ctx.buffer(vertices.tobytes())
            ibo = self.ctx.buffer(np.asarray(mesh.faces, dtype="u4").tobytes())
            self.vaos.append(self.ctx.vertex_array(self.program,
                             [(vbo, "3f 3f", "position", "normal")], ibo))
        self.color = self.ctx.texture((size, size), 4)
        self.identity = self.ctx.texture((size, size), 1)
        self.framebuffer = self.ctx.framebuffer([self.color, self.identity],
                                               self.ctx.depth_renderbuffer((size, size)))
        self.ctx.enable(moderngl.DEPTH_TEST)
        print(json.dumps({"renderer": self.ctx.info["GL_RENDERER"],
                          "triangles": sum(len(m.faces) for m in meshes), "size": size}),
              file=sys.stderr, flush=True)

    def frame_pair(self, yaw=0, pitch=0, zoom=1, selected=0):
        if type(selected) is not int or not 0 <= selected <= len(self.objects):
            raise ValueError("Invalid selected index")
        self.program["selected"].value = selected
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
            vao.render(moderngl.TRIANGLES)
        rgba = np.frombuffer(self.color.read(alignment=1), dtype=np.uint8)
        ids = np.frombuffer(self.identity.read(alignment=1), dtype=np.uint8)
        return (rgba.reshape(self.size, self.size, 4)[::-1, :, [2, 1, 0, 3]].tobytes(),
                ids.reshape(self.size, self.size)[::-1].tobytes())

    def frame(self, yaw=0, pitch=0, zoom=1):
        return self.frame_pair(yaw, pitch, zoom)[0]

    def packet(self, seq, **view):
        if type(seq) is not int or not 1 <= seq <= 0xffffffff:
            raise ValueError("Sequence must be uint32 > 0")
        color, ids = self.frame_pair(**view)
        return b"C3D1" + struct.pack(">I", seq) + color + ids

    def close(self):
        self.ctx.release()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("model")
    parser.add_argument("--size", type=int, default=512)
    parser.add_argument("--output", help="Write one raw BGRA frame instead of serving stdin")
    parser.add_argument("--yaw", type=float, default=0)
    parser.add_argument("--pitch", type=float, default=0)
    parser.add_argument("--zoom", type=float, default=1)
    args = parser.parse_args()
    renderer = Renderer(args.model, args.size)
    try:
        if args.output:
            Path(args.output).write_bytes(renderer.frame(args.yaw, args.pitch, args.zoom))
        else:
            while line := sys.stdin.buffer.readline(4097):
                if len(line) > 4096 or not line.endswith(b"\n"):
                    raise ValueError("Invalid or oversized request")
                sys.stdout.buffer.write(renderer.packet(**json.loads(line)))
                sys.stdout.buffer.flush()
    finally:
        renderer.close()


if __name__ == "__main__":
    main()
