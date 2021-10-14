;;; boxy-test-chinese.Eli --- Visual tests for boxy -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Tyler Grinn <tylergrinn@gmail.com>

;;; Commentary:
;;
;; These tests must be evaluated manually
;;

;;; Code:

;;;; Tests

;; Check that the tooltip for 你好 shows correctly
(let* ((world (boxy-box))
       (greeting (boxy-box :name "我叫泰勒" :margin-y 0))
       (hello (boxy-box :name "你好" :rel "in" :tooltip "你好\n我叫泰勒")))
  (boxy-add-next greeting world)
  (boxy-add-next hello greeting)
  (boxy-pp world))

;; Check that the tooltip for 你好 shows correctly
(let* ((world (boxy-box))
       (greeting (boxy-box :name "我叫泰勒" :margin-y 0))
       (hello (boxy-box :name "你好" :rel "in" :tooltip "你好\n我叫泰勒"))
       (right (boxy-box :name "右边 " :rel "to the right of")))
  (boxy-add-next greeting world)
  (boxy-add-next hello greeting)
  (boxy-add-next right greeting)
  (boxy-pp world))
