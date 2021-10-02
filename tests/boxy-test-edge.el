;;; boxy-test-edge-cases.el --- Edge cases for boxy -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Tyler Grinn <tylergrinn@gmail.com>

;;; Code:

;;;; Requirements

(require 'boxy-test-setup)

;;;; Tests

(ert-deftest boxy-edge-above-on-top ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (on-top (boxy-box :name "on top" :rel "on top of"))
         (above (boxy-box :name "above" :rel "above")))
    (boxy-add-next thing world)
    (boxy-add-next on-top thing)
    (boxy-add-next above on-top)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭─────╮
│above│
╰─────╯
 ╭──────╮
 │on top│
╭┴──────┴╮
│thing   │
╰────────╯
")))))

(ert-deftest boxy-edge-above-on-top-on-top ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (on-top (boxy-box :name "on top" :rel "on top of"))
         (on-top-on-top (boxy-box :name "on top on top" :rel "on top of"))
         (above (boxy-box :name "above" :rel "above")))
    (boxy-add-next thing world)
    (boxy-add-next on-top thing)
    (boxy-add-next on-top-on-top on-top)
    (boxy-add-next above on-top-on-top)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
                       "
╭─────╮
│above│
╰─────╯
  ╭─────────────╮
  │on top on top│
 ╭┴─────────────┴╮
 │on top         │
╭┴───────────────┴╮
│thing            │
╰─────────────────╯
")))))

(ert-deftest boxy-edge-below-on-top ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (on-top (boxy-box :name "on top" :rel "on top of"))
         (below (boxy-box :name "below" :rel "below")))
    (boxy-add-next thing world)
    (boxy-add-next on-top thing)
    (boxy-add-next below on-top)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
                       "
 ╭──────╮
 │on top│
╭┴──────┴╮
│thing   │
│╭─────╮ │
││below│ │
│╰─────╯ │
╰────────╯
")))))

(ert-deftest boxy-edge-below-on-top-on-top ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (on-top (boxy-box :name "on top" :rel "on top of"))
         (on-top-on-top (boxy-box :name "on top on top" :rel "on top of"))
         (below (boxy-box :name "below" :rel "below")))
    (boxy-add-next thing world)
    (boxy-add-next on-top thing)
    (boxy-add-next on-top-on-top on-top)
    (boxy-add-next below on-top-on-top)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
                       "
  ╭─────────────╮
  │on top on top│
 ╭┴─────────────┴╮
 │on top         │
 │╭─────╮        │
 ││below│        │
 │╰─────╯        │
╭┴───────────────┴╮
│thing            │
╰─────────────────╯
")))))

(ert-deftest boxy-edge-below-in-front ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (in-front (boxy-box :name "in front" :rel "in front of"))
         (below (boxy-box :name "below" :rel "below")))
    (boxy-add-next thing world)
    (boxy-add-next in-front thing)
    (boxy-add-next below in-front)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
                       "
╭──────────╮
│thing     │
│╭────────╮│
││in front││
╰┴────────┴╯

╭─────╮
│below│
╰─────╯
")))))

(ert-deftest boxy-edge-below-in-front-in-front ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (in-front (boxy-box :name "in front" :rel "in front of"))
         (in-front-in-front (boxy-box :name "in front in front" :rel "in front of"))
         (below (boxy-box :name "below" :rel "below")))
    (boxy-add-next thing world)
    (boxy-add-next in-front thing)
    (boxy-add-next in-front-in-front in-front)
    (boxy-add-next below in-front-in-front)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
                       "
╭─────────────────────╮
│thing                │
│╭───────────────────╮│
││in front           ││
││╭─────────────────╮││
│││in front in front│││
╰┴┴─────────────────┴┴╯

╭─────╮
│below│
╰─────╯
")))))

(ert-deftest boxy-edge-above-in-front ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (in-front (boxy-box :name "in front" :rel "in front of"))
         (above (boxy-box :name "above" :rel "above")))
    (boxy-add-next thing world)
    (boxy-add-next in-front thing)
    (boxy-add-next above in-front)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
                       "
╭──────────╮
│thing     │
│╭─────╮   │
││above│   │
│╰─────╯   │
│╭────────╮│
││in front││
╰┴────────┴╯
")))))

(ert-deftest boxy-edge-above-in-front-in-front ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (in-front (boxy-box :name "in front" :rel "in front of"))
         (in-front-in-front (boxy-box :name "in front in front" :rel "in front of"))
         (above (boxy-box :name "above" :rel "above")))
    (boxy-add-next thing world)
    (boxy-add-next in-front thing)
    (boxy-add-next in-front-in-front in-front)
    (boxy-add-next above in-front-in-front)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
                       "
╭─────────────────────╮
│thing                │
│╭───────────────────╮│
││in front           ││
││╭─────╮            ││
│││above│            ││
││╰─────╯            ││
││╭─────────────────╮││
│││in front in front│││
╰┴┴─────────────────┴┴╯
")))))

(ert-deftest boxy-edge-merge-above-on-top ()
  (let ((world-1 (boxy-box))
        (thing-1 (boxy-box :name "thing" :margin-y 0))
        (on-top-1 (boxy-box :name "on top" :rel "on top of"))
        (world-2 (boxy-box))
        (thing-2 (boxy-box :name "thing" :margin-y 0))
        (on-top-2 (boxy-box :name "on top" :rel "on top of"))
        (above (boxy-box :name "above" :rel "above")))
    (boxy-add-next thing-1 world-1)
    (boxy-add-next on-top-1 thing-1)
    (boxy-add-next thing-2 world-2)
    (boxy-add-next on-top-2 thing-2)
    (boxy-add-next above on-top-2)
    (boxy-pp (boxy-merge (list world-1 world-2)))
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
                       "
╭─────╮
│above│
╰─────╯
 ╭──────╮
 │on top│
╭┴──────┴╮
│thing   │
╰────────╯
")))))
