;;; boxy-test-smoke.el -- Smoke tests for boxy -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Taylor Grinn <grinntaylor@gmail.com>

;;; Code:

;;;; Requirements

(require 'boxy-test-setup)

;;;; Tests

(ert-deftest boxy-smoke-in ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (in (boxy-box :name "in" :rel "in")))
    (boxy-add-next thing world)
    (boxy-add-next in thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭─────╮
│thing│
│╭──╮ │
││in│ │
│╰──╯ │
╰─────╯
")))))

(ert-deftest boxy-smoke-on ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (on (boxy-box :name "on" :rel "on")))
    (boxy-add-next thing world)
    (boxy-add-next on thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭─────╮
│thing│
│╭──╮ │
││on│ │
│╰──╯ │
╰─────╯
")))))

(ert-deftest boxy-smoke-behind ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (behind (boxy-box :name "behind" :rel "behind")))
    (boxy-add-next thing world)
    (boxy-add-next behind thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭────────╮
│thing   │
│╭╌╌╌╌╌╌╮│
│╎behind╎│
│╰╌╌╌╌╌╌╯│
╰────────╯
")))))

(ert-deftest boxy-smoke-in-front ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (in-front (boxy-box :name "in front" :rel "in front of")))
    (boxy-add-next thing world)
    (boxy-add-next in-front thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭──────────╮
│thing     │
│╭────────╮│
││in front││
╰┴────────┴╯
")))))

(ert-deftest boxy-smoke-on-top ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (on-top (boxy-box :name "on top" :rel "on top of")))
    (boxy-add-next thing world)
    (boxy-add-next on-top thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
 ╭──────╮
 │on top│
╭┴──────┴╮
│thing   │
╰────────╯
")))))

(ert-deftest boxy-smoke-above ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (above (boxy-box :name "above" :rel "above")))
    (boxy-add-next thing world)
    (boxy-add-next above thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭─────╮
│above│
╰─────╯
╭─────╮
│thing│
╰─────╯
")))))

(ert-deftest boxy-smoke-below ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (below (boxy-box :name "below" :rel "below" :margin-y 0)))
    (boxy-add-next thing world)
    (boxy-add-next below thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭─────╮
│thing│
╰─────╯
╭─────╮
│below│
╰─────╯
")))))

(ert-deftest boxy-smoke-right ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (right (boxy-box :name "right" :rel "to the right of")))
    (boxy-add-next thing world)
    (boxy-add-next right thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭─────╮╭─────╮
│thing││right│
╰─────╯╰─────╯
")))))

(ert-deftest boxy-smoke-left ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (left (boxy-box :name "left" :rel "to the left of")))
    (boxy-add-next thing world)
    (boxy-add-next left thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭────╮╭─────╮
│left││thing│
╰────╯╰─────╯
")))))
