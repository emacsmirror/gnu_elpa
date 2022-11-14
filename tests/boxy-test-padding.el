;;; boxy-test-padding.el -- Padding tests for boxy -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Taylor Grinn <grinntaylor@gmail.com>

;;; Code:

;;;; Requirements

(require 'boxy-test-setup)

;;;; Tests

(ert-deftest boxy-padding-0-above ()
  (let* ((world (boxy-box :padding-x 0 :padding-y 0))
         (thing (boxy-box :name "thing"))
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

(ert-deftest boxy-padding-3-2-above ()
  (let* ((world (boxy-box :padding-x 3 :padding-y 2))
         (thing (boxy-box :name "thing"))
         (above (boxy-box :name "above" :rel "above")))
    (boxy-add-next thing world)
    (boxy-add-next above thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭───────────╮
│           │
│           │
│   above   │
│           │
│           │
╰───────────╯

╭───────────╮
│           │
│           │
│   thing   │
│           │
│           │
╰───────────╯
")))))

(ert-deftest boxy-padding-0-below ()
  (let* ((world (boxy-box :padding-x 0 :padding-y 0))
         (thing (boxy-box :name "thing"))
         (below (boxy-box :name "below" :rel "below")))
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

(ert-deftest boxy-padding-3-2-below ()
  (let* ((world (boxy-box :padding-x 3 :padding-y 2))
         (thing (boxy-box :name "thing"))
         (below (boxy-box :name "below" :rel "below")))
    (boxy-add-next thing world)
    (boxy-add-next below thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭───────────╮
│           │
│           │
│   thing   │
│           │
│           │
╰───────────╯

╭───────────╮
│           │
│           │
│   below   │
│           │
│           │
╰───────────╯
")))))

(ert-deftest boxy-padding-0-left ()
  (let* ((world (boxy-box :padding-x 0 :padding-y 0))
         (thing (boxy-box :name "thing"))
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

(ert-deftest boxy-padding-3-2-left ()
  (let* ((world (boxy-box :padding-x 3 :padding-y 2))
         (thing (boxy-box :name "thing"))
         (left (boxy-box :name "left" :rel "to the left of")))
    (boxy-add-next thing world)
    (boxy-add-next left thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭──────────╮╭───────────╮
│          ││           │
│          ││           │
│   left   ││   thing   │
│          ││           │
│          ││           │
╰──────────╯╰───────────╯
")))))

(ert-deftest boxy-padding-0-right ()
  (let* ((world (boxy-box :padding-x 0 :padding-y 0))
         (thing (boxy-box :name "thing"))
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

(ert-deftest boxy-padding-3-2-right ()
  (let* ((world (boxy-box :padding-x 3 :padding-y 2))
         (thing (boxy-box :name "thing"))
         (right (boxy-box :name "right" :rel "to the right of")))
    (boxy-add-next thing world)
    (boxy-add-next right thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭───────────╮╭───────────╮
│           ││           │
│           ││           │
│   thing   ││   right   │
│           ││           │
│           ││           │
╰───────────╯╰───────────╯
")))))

(ert-deftest boxy-padding-0-in ()
  (let* ((world (boxy-box :padding-x 0 :padding-y 0))
         (thing (boxy-box :name "thing"))
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

(ert-deftest boxy-padding-3-2-in ()
  (let* ((world (boxy-box :padding-x 3 :padding-y 2))
         (thing (boxy-box :name "thing"))
         (in (boxy-box :name "in" :rel "in")))
    (boxy-add-next thing world)
    (boxy-add-next in thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭────────────────╮
│                │
│                │
│   thing        │
│                │
│                │
│   ╭────────╮   │
│   │        │   │
│   │        │   │
│   │   in   │   │
│   │        │   │
│   │        │   │
│   ╰────────╯   │
╰────────────────╯
")))))

(ert-deftest boxy-padding-0-behind ()
  (let* ((world (boxy-box :padding-x 0 :padding-y 0))
         (thing (boxy-box :name "thing"))
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

(ert-deftest boxy-padding-3-2-behind ()
  (let* ((world (boxy-box :padding-x 3 :padding-y 2))
         (thing (boxy-box :name "thing"))
         (behind (boxy-box :name "behind" :rel "behind")))
    (boxy-add-next thing world)
    (boxy-add-next behind thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭────────────────────╮
│                    │
│                    │
│   thing            │
│                    │
│                    │
│   ╭╌╌╌╌╌╌╌╌╌╌╌╌╮   │
│   ╎            ╎   │
│   ╎            ╎   │
│   ╎   behind   ╎   │
│   ╎            ╎   │
│   ╎            ╎   │
│   ╰╌╌╌╌╌╌╌╌╌╌╌╌╯   │
╰────────────────────╯
")))))

(ert-deftest boxy-padding-0-in-front ()
  (let* ((world (boxy-box :padding-x 0 :padding-y 0))
         (thing (boxy-box :name "thing"))
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

(ert-deftest boxy-padding-3-2-in-front ()
  (let* ((world (boxy-box :padding-x 3 :padding-y 2))
         (thing (boxy-box :name "thing"))
         (in-front (boxy-box :name "in front" :rel "in front of")))
    (boxy-add-next thing world)
    (boxy-add-next in-front thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭──────────────────────╮
│                      │
│                      │
│   thing              │
│                      │
│                      │
│   ╭──────────────╮   │
│   │              │   │
│   │              │   │
│   │   in front   │   │
│   │              │   │
│   │              │   │
╰───┴──────────────┴───╯
")))))


(ert-deftest boxy-padding-0-on-top ()
  (let* ((world (boxy-box :padding-x 0 :padding-y 0))
         (thing (boxy-box :name "thing"))
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

(ert-deftest boxy-padding-3-2-on-top ()
  (let* ((world (boxy-box :padding-x 3 :padding-y 2))
         (thing (boxy-box :name "thing"))
         (on-top (boxy-box :name "on top" :rel "on top of")))
    (boxy-add-next thing world)
    (boxy-add-next on-top thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
    ╭────────────╮
    │            │
    │            │
    │   on top   │
    │            │
    │            │
╭───┴────────────┴───╮
│                    │
│                    │
│   thing            │
│                    │
│                    │
╰────────────────────╯
")))))






