"""Regression tests for matrix failure, provenance, and isolation boundaries."""

from contextlib import redirect_stdout
import io
import json
import os
from pathlib import Path
import runpy
import subprocess
import tempfile
import unittest
from unittest.mock import patch


RUNNER = runpy.run_path(str(Path(__file__).with_name("test-matrix")))
SOURCE = Path(__file__).resolve().parent.parent


class MatrixTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="keymap-popup-matrix-selftest-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)

    def test_header_minimum_is_exact(self):
        self.assertEqual(RUNNER["minimum_version"](SOURCE), "29.1")

    def test_snapshot_omits_private_and_generated_files(self):
        source = self.root / "input"
        (source / "admin").mkdir(parents=True)
        (source / "admin/source-manifest.json").write_text('["source.el"]')
        (source / "source.el").write_text("source")
        for name in ("private.el", "local.mk", "source.elc", "module.so"):
            (source / name).write_text("must not copy")
        destination = self.root / "output"
        RUNNER["snapshot"](source, destination)
        self.assertEqual([p.name for p in destination.iterdir()], ["source.el"])
        (source / "source.el").unlink()
        (source / "source.el").symlink_to(source / "private.el")
        with self.assertRaisesRegex(ValueError, "symlinked"):
            RUNNER["snapshot"](source, self.root / "rejected")

    def run_matrix(self, missing_fork=False, fail_minimum=False, missing_receipt=False):
        calls = []
        resolved = []

        def resolve(command):
            resolved.append(command)
            if command == "user-fork" and missing_fork:
                raise ValueError("missing user-fork")
            return "/resolved/" + command

        def run(command, **_):
            name = command[command.index("--lane") + 1]
            calls.append(name)
            self.assertEqual(resolved[0], "user-fork")
            self.assertEqual(command[2].split("#")[0], str(SOURCE))
            if name == "fork":
                self.assertEqual(command[-2:], ["--emacs", "/resolved/user-fork"])
            if not missing_receipt:
                lane = Path(command[command.index("--root") + 1])
                lane.mkdir()
                (lane / "passed").write_text("29.1\n")
            return subprocess.CompletedProcess(command, 1 if fail_minimum and name == "minimum" else 0)

        with patch.dict(os.environ, {"THANOS_EMACS": "user-fork"}, clear=True), \
             patch.dict(RUNNER["matrix"].__globals__, {"executable": resolve}), \
             patch("tempfile.mkdtemp", return_value=str(self.root)), \
             patch("subprocess.check_output", return_value=json.dumps({"path": str(SOURCE)})), \
             patch("subprocess.run", side_effect=run), redirect_stdout(io.StringIO()):
            if missing_fork or fail_minimum or missing_receipt:
                with self.assertRaisesRegex(RuntimeError, "Required matrix lanes failed"):
                    RUNNER["matrix"](SOURCE)
            else:
                RUNNER["matrix"](SOURCE)
        return calls

    def test_success_resolves_fork_before_nix(self):
        self.assertEqual(self.run_matrix(), ["minimum", "default", "fork"])

    def test_failure_still_attempts_every_lane(self):
        self.assertEqual(self.run_matrix(fail_minimum=True), ["minimum", "default", "fork"])

    def test_missing_fork_does_not_suppress_nix_lanes(self):
        self.assertEqual(self.run_matrix(missing_fork=True), ["minimum", "default"])

    def test_zero_exit_without_receipt_is_failure(self):
        self.assertEqual(self.run_matrix(missing_receipt=True), ["minimum", "default", "fork"])

    def test_lane_isolation_and_focused_options(self):
        dependencies = self.root / "deps"
        dependencies.mkdir()
        (dependencies / "package-lint.el").write_text("source")
        observed = []

        def run(command, cwd, env, **_):
            observed.append((cwd, env))
            self.assertIn("TESTS=tests/keymap-popup-declarations-tests.el", command)
            self.assertNotIn("MAKEFLAGS", env)
            self.assertTrue(env["EMACSLOADPATH"].endswith(os.pathsep))
            if "compile" in command:
                self.assertFalse((cwd / "keymap-popup.elc").exists())
            (cwd / "keymap-popup.elc").write_text("private bytecode")
            Path(env["KEYMAP_POPUP_MATRIX_RECEIPT"]).write_text(json.dumps(
                {"total": 1, "completed": 1, "expected": 1, "unexpected": 0, "skipped": 0}))
            return subprocess.CompletedProcess(command, 0)

        with patch.dict(os.environ, {"KEYMAP_POPUP_MATRIX_DEPS": str(dependencies),
                                     "MATRIX_TESTS": "tests/keymap-popup-declarations-tests.el",
                                     "MAKEFLAGS": "bad", "EMACSLOADPATH": "bad"}), \
             patch("subprocess.check_output", return_value="29.1"), \
             patch("subprocess.run", side_effect=run), redirect_stdout(io.StringIO()):
            for name in ("minimum", "default", "fork"):
                RUNNER["lane"](SOURCE, self.root / name, name, "/fake/emacs", "29.1")
        for key in ("HOME", "TMPDIR", "XDG_CACHE_HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME", "XDG_STATE_HOME"):
            self.assertEqual(len({env[key] for _, env in observed}), 3)
        self.assertEqual(len({cwd for cwd, _ in observed}), 3)

    def test_invalid_or_incomplete_receipts(self):
        for stats in ({}, {"total": 0, "completed": 0, "expected": 0, "unexpected": 0, "skipped": 0},
                      {"total": 2, "completed": 1, "expected": 1, "unexpected": 0, "skipped": 0},
                      {"total": 1, "completed": 1, "expected": 0, "unexpected": 1, "skipped": 0}):
            with self.subTest(stats=stats), self.assertRaises(ValueError):
                RUNNER["validate_stats"](stats)

    def test_wrong_patch_release_is_not_minimum(self):
        dependencies = self.root / "deps"
        dependencies.mkdir()
        with patch.dict(os.environ, {"KEYMAP_POPUP_MATRIX_DEPS": str(dependencies)}), \
             patch("subprocess.check_output", return_value="29.4"), \
             patch("subprocess.run") as run, redirect_stdout(io.StringIO()):
            with self.assertRaisesRegex(ValueError, "expected 29.1, got 29.4"):
                RUNNER["lane"](SOURCE, self.root / "minimum", "minimum", "/fake/emacs", "29.1")
            run.assert_not_called()


if __name__ == "__main__":
    unittest.main()
