;;; boxy-test-margin.el -- Margin tests for boxy -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Tyler Grinn <tylergrinn@gmail.com>

;;; Code:

;;;; Requirements

(require 'boxy-test-setup)

;;;; Tests

(ert-deftest boxy-margin-0-above ()
  (let* ((world (boxy-box :margin-x 0 :margin-y 0))
         (thing (boxy-box :name "thing"))
         (above (boxy-box :name "above" :rel "above")))
    (boxy-add-next thing world)
    (boxy-add-next above thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "╭─────╮
│above│
╰─────╯
╭─────╮
│thing│
╰─────╯
")))))

(ert-deftest boxy-margin-3-2-above ()
  (let* ((world (boxy-box :margin-x 3 :margin-y 2))
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

(ert-deftest boxy-margin-0-below ()
  (let* ((world (boxy-box :margin-x 0 :margin-y 0))
         (thing (boxy-box :name "thing"))
         (below (boxy-box :name "below" :rel "below")))
    (boxy-add-next thing world)
    (boxy-add-next below thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "╭─────╮
│thing│
╰─────╯
╭─────╮
│below│
╰─────╯
")))))

(ert-deftest boxy-margin-3-2-below ()
  (let* ((world (boxy-box :margin-x 3 :margin-y 2))
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

(ert-deftest boxy-margin-0-left ()
  (let* ((world (boxy-box :margin-x 0 :margin-y 0))
         (thing (boxy-box :name "thing"))
         (left (boxy-box :name "left" :rel "to the left of")))
    (boxy-add-next thing world)
    (boxy-add-next left thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "╭────╮╭─────╮
│left││thing│
╰────╯╰─────╯
")))))

(ert-deftest boxy-margin-3-2-left ()
  (let* ((world (boxy-box :margin-x 3 :margin-y 2))
         (thing (boxy-box :name "thing"))
         (left (boxy-box :name "left" :rel "to the left of")))
    (boxy-add-next thing world)
    (boxy-add-next left thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "

   ╭────╮   ╭─────╮
   │left│   │thing│
   ╰────╯   ╰─────╯
")))))

(ert-deftest boxy-margin-0-right ()
  (let* ((world (boxy-box :margin-x 0 :margin-y 0))
         (thing (boxy-box :name "thing"))
         (right (boxy-box :name "right" :rel "to the right of")))
    (boxy-add-next thing world)
    (boxy-add-next right thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "╭─────╮╭─────╮
│thing││right│
╰─────╯╰─────╯
")))))

(ert-deftest boxy-margin-3-2-right ()
  (let* ((world (boxy-box :margin-x 3 :margin-y 2))
         (thing (boxy-box :name "thing"))
         (right (boxy-box :name "right" :rel "to the right of")))
    (boxy-add-next thing world)
    (boxy-add-next right thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "

   ╭─────╮   ╭─────╮
   │thing│   │right│
   ╰─────╯   ╰─────╯
")))))




