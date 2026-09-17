;;; gnosis-tooling-canvas.el --- Opt-in canvas suites -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:
;; Load only from the optional Make gate, never ordinary core test discovery.

;;; Code:

(require 'canvas-3d)
(setq canvas-3d-python-command (getenv "GNOSIS_CANVAS_PYTHON"))
(require 'canvas-3d-tests)
(require 'canvas-3d-geometry-tests)
(require 'canvas-3d-pick-tests)
(require 'canvas-3d-model-tests)

(provide 'gnosis-tooling-canvas)
;;; gnosis-tooling-canvas.el ends here
