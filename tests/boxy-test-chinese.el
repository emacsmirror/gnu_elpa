;;; boxy-test-chinese.el --- Chinese test cases for boxy -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Taylor Grinn <grinntaylor@gmail.com>

;;; Code:

;;;; Requirements

(require 'boxy-test-setup)

;;;; Tests

(ert-deftest boxy-test-chinese-hello ()
  (let* ((world (boxy-box))
         (thing (boxy-box :name "thing" :margin-y 0))
         (hello (boxy-box :name "你好" :rel "in")))
    (boxy-add-next thing world)
    (boxy-add-next hello thing)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭──────╮
│thing │
│╭────╮│
││你好││
│╰────╯│
╰──────╯
")))))

(ert-deftest boxy-test-chinese-greeting ()
  (let* ((world (boxy-box))
         (greeting (boxy-box :name "我叫泰勒" :margin-y 0))
         (hello (boxy-box :name "你好" :rel "in")))
    (boxy-add-next greeting world)
    (boxy-add-next hello greeting)
    (boxy-pp world)
    (with-current-buffer (get-buffer "*Boxy*")
      (should (string= (buffer-string)
           "
╭────────╮
│我叫泰勒│
│╭────╮  │
││你好│  │
│╰────╯  │
╰────────╯
")))))


