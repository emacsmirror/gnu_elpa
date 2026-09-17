"""Current compact-pick diagnostics and independent capability reporting."""
# Copyright (C) 2026 Thanos Apollo
# SPDX-License-Identifier: GPL-3.0-or-later
import contextlib
import io
import struct
import subprocess
import unittest
from unittest.mock import patch

import preflight


class PreflightTests(unittest.TestCase):
    def setUp(self):
        self.frame_length = 8 + 4 * 128 * 128
        self.frames = (b"C3D3" + struct.pack(">I", 1) + bytes(4 * 128 * 128)
                       + b"C3D3" + struct.pack(">I", 2) + b"\xff" * (4 * 128 * 128))
        self.background = (b"C3P4", 3, 2, 0, 0, 0, 0, 0)
        self.stale = (b"C3P4", 4, 1, 0xffffffff, 0, 0, 0, 0)
        self.response = self.frames + self.picks(self.background, self.stale)

    @staticmethod
    def picks(*values):
        return b"".join(struct.pack(">4sIIIIddd", *value) for value in values)

    def check_response(self, response):
        result = subprocess.CompletedProcess([], 0, response, b"test renderer\n")
        with (patch.object(preflight, "run", return_value=result),
              contextlib.redirect_stdout(io.StringIO())):
            preflight.egl_check("prepared-python")

    def test_current_float64_response(self):
        self.check_response(self.response)

    def test_corrupt_responses_and_recovery(self):
        cases = {
            "truncated": self.response[:-1],
            "extra": self.response + b"\0",
            "empty": b"",
            "old_layout": self.frames + b"".join(
                struct.pack(">4sIIIIfff", b"C3P3", *value[1:])
                for value in (self.background, self.stale)),
            "unchanged_highlight": self.frames[:self.frame_length + 8]
                + self.frames[8:self.frame_length] + self.response[len(self.frames):],
        }
        for offset in (0, self.frame_length):
            for field, replacement in ((0, b"BAD!"), (4, struct.pack(">I", 99))):
                start = offset + field
                cases[f"frame_{offset}_field_{field}"] = (
                    self.response[:start] + replacement + self.response[start + 4:])
        for packet, original in enumerate((self.background, self.stale)):
            for field, bad_values in ((0, (b"C3P3", b"BAD!")), (1, (99,)),
                                      (2, (99,)), (3, (1,)), (4, (1,)),
                                      (5, (1., float("nan"), float("inf"))),
                                      (6, (1.,)), (7, (1.,))):
                for bad in bad_values:
                    values: list[list[bytes | int | float]] = [list(self.background), list(self.stale)]
                    changed: list[bytes | int | float] = list(original)
                    changed[field] = bad
                    values[packet] = changed
                    cases[f"pick_{packet}_field_{field}_{bad}"] = self.frames + self.picks(*values)
        # A stale frame reported as ordinary background must not be accepted.
        cases["stale_accepted"] = self.frames + self.picks(
            self.background, (b"C3P4", 4, 1, 0, 0, 0, 0, 0))
        for name, response in cases.items():
            with self.subTest(case=name):
                with self.assertRaises(RuntimeError):
                    self.check_response(response)
                self.check_response(self.response)

    def test_capabilities_report_separately_after_failures(self):
        checks = ("emacs_check", "imports_check", "egl_check")
        for failed in (None, *checks):
            with self.subTest(failed=failed), contextlib.ExitStack() as stack:
                mocks = [stack.enter_context(patch.object(preflight, name)) for name in checks]
                if failed:
                    mocks[checks.index(failed)].side_effect = subprocess.TimeoutExpired("probe", 30)
                stack.enter_context(patch("sys.argv", ["preflight", "--emacs", "canvas-emacs",
                                                       "--python", "prepared-python"]))
                output = stack.enter_context(contextlib.redirect_stdout(io.StringIO()))
                self.assertEqual(preflight.main(), int(failed is not None))
                for mock, executable in zip(mocks, ("canvas-emacs", "prepared-python", "prepared-python")):
                    mock.assert_called_once_with(executable)
                self.assertEqual(output.getvalue().count("PASS"), 3 if failed is None else 2)
                self.assertEqual(output.getvalue().count("FAIL:"), int(failed is not None))

    def test_subprocess_has_timeout_and_both_streams(self):
        with patch.object(subprocess, "run") as run:
            preflight.run(["probe"], input=b"request")
        run.assert_called_once_with(["probe"], capture_output=True, timeout=30, input=b"request")


if __name__ == "__main__":
    unittest.main()
