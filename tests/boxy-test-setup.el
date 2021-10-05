;;; boxy-test-setup.el --- Set up for boxy tests -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Tyler Grinn <tylergrinn@gmail.com>

;;; Code:

(require 'boxy "boxy.el")

(setq boxy-default-margin-x 0)
(setq boxy-default-margin-y 1)
(setq boxy-default-padding-x 0)
(setq boxy-default-padding-y 0)

(provide 'boxy-test-setup)

;;; boxy-test-setup.el ends here
