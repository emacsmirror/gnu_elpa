;;; denote-sequence-test.el --- Unit tests for denote-sequence.el -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Free Software Foundation, Inc.

;; Author: Protesilaos <info@protesilaos.com>
;; Maintainer: Protesilaos <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for denote-sequence.el.  Note that we are using Shorthands in
;; this file, so the "dst-" prefix really is "denote-sequence-test-".
;; Evaluate the following to learn more:
;;
;;    (info "(elisp) Shorthands")

;;; Code:

(require 'ert)
(require 'denote-sequence)

(defun dst-relative-p (sequence type &rest files)
  "Return non-nil if FILES are relatives of SEQUENCE given TYPE."
  (when-let* ((relatives (denote-sequence-get-relative sequence type files))
              (found (seq-filter
                      (lambda (file)
                        (member file relatives))
                      files)))
    (>= (length files) (length found))))

(ert-deftest dst-denote-sequence-numeric-p ()
  "Test that `denote-sequence-numeric-p' does what it is supposed to."
  (should-not (denote-sequence-numeric-p "a"))
  (should-not (denote-sequence-numeric-p "1a"))
  (should-not (denote-sequence-numeric-p "1=a"))
  (should-not (denote-sequence-numeric-p "hello"))
  (should (string= (denote-sequence-numeric-p "1") "1"))
  (should (string= (denote-sequence-numeric-p "1=1") "1=1")))

(ert-deftest dst-denote-sequence-alphanumeric-p ()
  "Test that `denote-sequence-alphanumeric-p' does what it is supposed to."
  (should-not (denote-sequence-alphanumeric-p "1=1"))
  (should-not (denote-sequence-alphanumeric-p "1=a"))
  (should-not (denote-sequence-alphanumeric-p "hello"))
  (should (string= (denote-sequence-alphanumeric-p "1") "1"))
  (should (string= (denote-sequence-alphanumeric-p "1a") "1a")))

(ert-deftest dst-denote-sequence--alphanumeric-delimited-check-alternation ()
  "Test that `denote-sequence--alphanumeric-delimited-check-alternation' does the right thing.
This helper function only checks that we alternate between numbers and
letters.  It is not responsible to validate the levels of depth."
  (should-not (denote-sequence--alphanumeric-delimited-check-alternation '("1" "=" "a" "=" "a")))
  (should (denote-sequence--alphanumeric-delimited-check-alternation '("1" "=" "a" "=" "1"))))

(ert-deftest dst-denote-sequence--alphanumeric-delimited-check-depths ()
  "Test that `denote-sequence--alphanumeric-delimited-check-depths' does the right thing.
This helper function is not responsible for checking whether the
sequence alternates between numbers and letters.  It only checks the
levels of depth between delimiters."
  (should-not (denote-sequence--alphanumeric-delimited-check-depths '("1" "=" "a" "=" "1")))
  (should-not (denote-sequence--alphanumeric-delimited-check-depths '("1" "=" "a" "=" "1" "a")))
  (should-not (denote-sequence--alphanumeric-delimited-check-depths '("1" "a" "=" "1" "a" "=" "1" "a")))
  (should (denote-sequence--alphanumeric-delimited-check-depths '("1")))
  (should (denote-sequence--alphanumeric-delimited-check-depths '("1" "=" "1")))
  (should (denote-sequence--alphanumeric-delimited-check-depths '("1" "=" "1" "a")))
  (should (denote-sequence--alphanumeric-delimited-check-depths '("1" "=" "1" "a" "1")))
  (should (denote-sequence--alphanumeric-delimited-check-depths '("1" "=" "1" "a" "1" "=" "1")))
  (should (denote-sequence--alphanumeric-delimited-check-depths '("1" "=" "1" "a" "1" "=" "1" "a")))
  (should (denote-sequence--alphanumeric-delimited-check-depths '("1" "=" "1" "a" "1" "=" "1" "a" "1"))))

(ert-deftest dst-denote-sequence-alphanumeric-delimited-p ()
  "Test that `denote-sequence-alphanumeric-delimited-p' does what it is supposed to."
  (should-not (denote-sequence-alphanumeric-delimited-p "1a"))
  (should-not (denote-sequence-alphanumeric-delimited-p "1=1"))
  (should-not (denote-sequence-alphanumeric-delimited-p "1=a=a"))
  (should-not (denote-sequence-alphanumeric-delimited-p "1=a=1")) ; check FIXME in the source
  (should-not (denote-sequence-alphanumeric-delimited-p "hello"))
  (should (string= (denote-sequence-alphanumeric-delimited-p "1") "1"))
  (should (string= (denote-sequence-alphanumeric-delimited-p "1=a") "1=a"))
  (should (string= (denote-sequence-alphanumeric-delimited-p "1=a1b") "1=a1b"))
  (should (string= (denote-sequence-alphanumeric-delimited-p "1=a1b=2a1") "1=a1b=2a1"))
  (should (string= (denote-sequence-alphanumeric-delimited-p "1=zza1zb=2za1") "1=zza1zb=2za1")))

(ert-deftest dst-denote-sequence-and-scheme-p ()
  "Test that `denote-sequence-and-scheme-p' covers all cases."
  (should-error (denote-sequence-and-scheme-p "a"))
  (should (equal (denote-sequence-and-scheme-p "1=1") (cons "1=1" 'numeric)))
  (should (equal (denote-sequence-and-scheme-p "1a") (cons "1a" 'alphanumeric)))
  (should (equal (denote-sequence-and-scheme-p "1=a") (cons "1=a" 'alphanumeric-delimited)))
  (should
   (let ((denote-sequence-scheme 'numeric))
     (equal (denote-sequence-and-scheme-p "1") (cons "1" 'numeric))))
  (should
   (let ((denote-sequence-scheme 'alphanumeric))
     (equal (denote-sequence-and-scheme-p "1") (cons "1" 'alphanumeric))))
  (should
   (let ((denote-sequence-scheme 'alphanumeric-delimited))
     (equal (denote-sequence-and-scheme-p "1") (cons "1" 'alphanumeric-delimited)))))

(ert-deftest dst-denote-sequence-join ()
  "Test that `denote-sequence-join' works as intended.
The `denote-sequence-join' is not responsible for checking if the
STRINGS passed to it conform with the given SCHEME."
  (should (string= (denote-sequence-join '("1" "1" "1" "1") 'numeric) "1=1=1=1"))
  (should (string= (denote-sequence-join '("1" "a" "1" "a") 'alphanumeric) "1a1a"))
  (should (string= (denote-sequence-join '("1" "a") 'alphanumeric-delimited) "1=a"))
  (should (string= (denote-sequence-join '("1" "a" "1") 'alphanumeric-delimited) "1=a1"))
  (should (string= (denote-sequence-join '("1" "a" "1" "a") 'alphanumeric-delimited) "1=a1a"))
  (should (string= (denote-sequence-join '("1" "a" "1" "a" "1" "a" "1" "a" "1" "a") 'alphanumeric-delimited) "1=a1a=1a1=a1a")))

(ert-deftest dst-denote-sequence--number-to-alpha-complete ()
  "Test that `denote-sequence--number-to-alpha-complete' does the right thing."
  (should (string= (denote-sequence--number-to-alpha-complete "1=1=1=1=1=1=1" 'alphanumeric) "1a1a1a1"))
  (should (string= (denote-sequence--number-to-alpha-complete "1=1=1=1=1=1=1" 'alphanumeric-delimited) "1=a1a=1a1"))
  (should-error (denote-sequence--number-to-alpha-complete "1=1=1=1=1=1=1" 'numeric))
  (should-error (denote-sequence--number-to-alpha-complete "1=1=1=1=1=1=1" 'numericdkjdldk)))

(ert-deftest dst-denote-sequence--get-new-exhaustive ()
  "Test if we get the correct parent, child, sibling, or relatives of a sequence.
Use the function `denote-sequence-get-new' for child and sibling with
the numeric and alphanumeric `denote-sequence-scheme', as well as the
function `denote-sequence-get-relative'."
  (let* ((denote-sequence-scheme 'numeric)
         (denote-directory (expand-file-name "denote-sequence-test" temporary-file-directory))
         (files
          (mapcar
           (lambda (file)
             (let ((path (expand-file-name file (denote-directory))))
               (if (file-exists-p path)
                   path
                 (with-current-buffer (find-file-noselect path)
                   (save-buffer)
                   (kill-buffer (current-buffer)))
                 path)))
           '("20241230T075023==1--test__testing.txt"
             "20241230T075023==1=1--test__testing.txt"
             "20241230T075023==1=1=1--test__testing.txt"
             "20241230T075023==1=1=2--test__testing.txt"
             "20241230T075023==1=2--test__testing.txt"
             "20241230T075023==1=2=1--test__testing.txt"
             "20241230T075023==1=2=1=1--test__testing.txt"
             "20241230T075023==2--test__testing.txt"
             "20241230T075023==10--test__testing.txt"
             "20241230T075023==10=1--test__testing.txt"
             "20241230T075023==10=1=1--test__testing.txt"
             "20241230T075023==10=2--test__testing.txt"
             "20241230T075023==10=10--test__testing.txt"
             "20241230T075023==10=10=1--test__testing.txt")))
         (sequences (denote-sequence-get-all-sequences files)))
    (should (string= (denote-sequence-get-new 'parent) "11"))

    (should (string= (denote-sequence-get-new 'child "1" sequences) "1=3"))
    (should (string= (denote-sequence-get-new 'child "1=1" sequences) "1=1=3"))
    (should (string= (denote-sequence-get-new 'child "1=1=2" sequences) "1=1=2=1"))
    (should (string= (denote-sequence-get-new 'child "1=2" sequences) "1=2=2"))
    (should (string= (denote-sequence-get-new 'child "1=2=1" sequences) "1=2=1=2"))
    (should (string= (denote-sequence-get-new 'child "2" sequences) "2=1"))
    (should-error (denote-sequence-get-new 'child "11" sequences))

    (should (string= (denote-sequence-get-new 'sibling "1" sequences) "11"))
    (should (string= (denote-sequence-get-new 'sibling "1=1" sequences) "1=3"))
    (should (string= (denote-sequence-get-new 'sibling "1=1=1" sequences) "1=1=3"))
    (should (string= (denote-sequence-get-new 'sibling "1=1=2" sequences) "1=1=3"))
    (should (string= (denote-sequence-get-new 'sibling "1=2" sequences) "1=3"))
    (should (string= (denote-sequence-get-new 'sibling "1=2=1" sequences) "1=2=2"))
    (should (string= (denote-sequence-get-new 'sibling "2" sequences) "11"))
    (should-error (denote-sequence-get-new 'sibling "12" sequences))

    (should (string= (denote-sequence-get-relative "1=2=1=1" 'parent files)
                     (expand-file-name "20241230T075023==1=2=1--test__testing.txt" denote-directory)))
    (should (string= (denote-sequence-get-relative "10=1=1" 'parent files)
                     (expand-file-name "20241230T075023==10=1--test__testing.txt" denote-directory)))
    (should (string= (denote-sequence-get-relative "10=10=1" 'parent files)
                     (expand-file-name "20241230T075023==10=10--test__testing.txt" denote-directory)))
    (should (dst-relative-p "1=2=1=1" 'all-parents
                            "20241230T075023==1--test__testing.txt"
                            "20241230T075023==1=2--test__testing.txt"
                            "20241230T075023==1=2=1--test__testing.txt"))
    (should (dst-relative-p "10=1=1" 'all-parents
                            "20241230T075023==10--test__testing.txt"
                            "20241230T075023==10=1--test__testing.txt"))
    (should (dst-relative-p "10=10=1" 'all-parents
                            "20241230T075023==10--test__testing.txt"
                            "20241230T075023==10=10--test__testing.txt"))
    (should (dst-relative-p "1=1" 'siblings
                            "20241230T075023==1=1--test__testing.txt"
                            "20241230T075023==1=2--test__testing.txt"))
    (should (dst-relative-p "10=1" 'siblings
                            "20241230T075023==10=1--test__testing.txt"
                            "20241230T075023==10=10--test__testing.txt"
                            "20241230T075023==10=2--test__testing.txt"))
    (should (dst-relative-p "1" 'children
                            "20241230T075023==1=1--test__testing.txt"
                            "20241230T075023==1=2--test__testing.txt"))
    (should (dst-relative-p "10" 'children
                            "20241230T075023==10=1--test__testing.txt"
                            "20241230T075023==10=2--test__testing.txt"
                            "20241230T075023==10=10--test__testing.txt"))
    (should (dst-relative-p "1=1" 'all-children
                            "20241230T075023==1=1=1--test__testing.txt"
                            "20241230T075023==1=1=2--test__testing.txt")))

  (let* ((denote-sequence-scheme 'alphanumeric)
         (denote-directory (expand-file-name "denote-sequence-test" temporary-file-directory))
         (files
          (mapcar
           (lambda (file)
             (let ((path (expand-file-name file (denote-directory))))
               (if (file-exists-p path)
                   path
                 (with-current-buffer (find-file-noselect path)
                   (save-buffer)
                   (kill-buffer (current-buffer)))
                 path)))
           '("20241230T075023==1--test__testing.txt"
             "20241230T075023==1a--test__testing.txt"
             "20241230T075023==1a1--test__testing.txt"
             "20241230T075023==1a2--test__testing.txt"
             "20241230T075023==1b--test__testing.txt"
             "20241230T075023==1b1--test__testing.txt"
             "20241230T075023==1b1a--test__testing.txt"
             "20241230T075023==2--test__testing.txt"
             "20241230T075023==10--test__testing.txt"
             "20241230T075023==10a--test__testing.txt"
             "20241230T075023==10b--test__testing.txt")))
         (sequences (denote-sequence-get-all-sequences files)))
    (should (string= (denote-sequence-get-new 'parent) "11"))

    (should (string= (denote-sequence-get-new 'child "1" sequences) "1c"))
    (should (string= (denote-sequence-get-new 'child "1a" sequences) "1a3"))
    (should (string= (denote-sequence-get-new 'child "1a2" sequences) "1a2a"))
    (should (string= (denote-sequence-get-new 'child "1b" sequences) "1b2"))
    (should (string= (denote-sequence-get-new 'child "1b1" sequences) "1b1b"))
    (should (string= (denote-sequence-get-new 'child "2" sequences) "2a"))
    (should-error (denote-sequence-get-new 'child "11" sequences))

    (should (string= (denote-sequence-get-new 'sibling "1" sequences) "11"))
    (should (string= (denote-sequence-get-new 'sibling "1a" sequences) "1c"))
    (should (string= (denote-sequence-get-new 'sibling "1a1" sequences) "1a3"))
    (should (string= (denote-sequence-get-new 'sibling "1a2" sequences) "1a3"))
    (should (string= (denote-sequence-get-new 'sibling "1b" sequences) "1c"))
    (should (string= (denote-sequence-get-new 'sibling "1b1" sequences) "1b2"))
    (should (string= (denote-sequence-get-new 'sibling "2" sequences) "11"))
    (should-error (denote-sequence-get-new 'sibling "12" sequences))

    (should (string= (denote-sequence-get-relative "1b1a" 'parent files)
                     (expand-file-name "20241230T075023==1b1--test__testing.txt" denote-directory)))
    (should (string= (denote-sequence-get-relative "10a" 'parent files)
                     (expand-file-name "20241230T075023==10--test__testing.txt" denote-directory)))
    (should (dst-relative-p "1b1a" 'all-parents
                            "20241230T075023==1--test__testing.txt"
                            "20241230T075023==1b--test__testing.txt"
                            "20241230T075023==1b1--test__testing.txt"))
    (should (dst-relative-p "1a" 'siblings
                            "20241230T075023==1a--test__testing.txt"
                            "20241230T075023==1b--test__testing.txt"))
    (should (dst-relative-p "10a" 'siblings
                            "20241230T075023==10a--test__testing.txt"
                            "20241230T075023==10b--test__testing.txt"))
    (should (dst-relative-p "1" 'children
                            "20241230T075023==1a--test__testing.txt"
                            "20241230T075023==1b--test__testing.txt"))
    (should (dst-relative-p "10" 'children
                            "20241230T075023==10a--test__testing.txt"
                            "20241230T075023==10b--test__testing.txt"))
    (should (dst-relative-p "1a" 'all-children
                            "20241230T075023==1a1--test__testing.txt"
                            "20241230T075023==1a2--test__testing.txt")))

  (let* ((denote-sequence-scheme 'alphanumeric-delimited)
         (denote-directory (expand-file-name "denote-sequence-test" temporary-file-directory))
         (files
          (mapcar
           (lambda (file)
             (let ((path (expand-file-name file (denote-directory))))
               (if (file-exists-p path)
                   path
                 (with-current-buffer (find-file-noselect path)
                   (save-buffer)
                   (kill-buffer (current-buffer)))
                 path)))
           '("20241230T075023==1--test__testing.txt"
             "20241230T075023==1=a--test__testing.txt"
             "20241230T075023==1=a1--test__testing.txt"
             "20241230T075023==1=a2--test__testing.txt"
             "20241230T075023==1=b--test__testing.txt"
             "20241230T075023==1=b1--test__testing.txt"
             "20241230T075023==1=b1a--test__testing.txt"
             "20241230T075023==2--test__testing.txt"
             "20241230T075023==10--test__testing.txt"
             "20241230T075023==10=a--test__testing.txt"
             "20241230T075023==10=b--test__testing.txt")))
         (sequences (denote-sequence-get-all-sequences files)))
    (should (string= (denote-sequence-get-new 'parent) "11"))

    (should (string= (denote-sequence-get-new 'child "1" sequences) "1=c"))
    (should (string= (denote-sequence-get-new 'child "1=a" sequences) "1=a3"))
    (should (string= (denote-sequence-get-new 'child "1=a2" sequences) "1=a2a"))
    (should (string= (denote-sequence-get-new 'child "1=b" sequences) "1=b2"))
    (should (string= (denote-sequence-get-new 'child "1=b1" sequences) "1=b1b"))
    (should (string= (denote-sequence-get-new 'child "2" sequences) "2=a"))
    (should-error (denote-sequence-get-new 'child "11" sequences))

    (should (string= (denote-sequence-get-new 'sibling "1" sequences) "11"))
    (should (string= (denote-sequence-get-new 'sibling "1=a" sequences) "1=c"))
    (should (string= (denote-sequence-get-new 'sibling "1=a1" sequences) "1=a3"))
    (should (string= (denote-sequence-get-new 'sibling "1=a2" sequences) "1=a3"))
    (should (string= (denote-sequence-get-new 'sibling "1=b" sequences) "1=c"))
    (should (string= (denote-sequence-get-new 'sibling "1=b1" sequences) "1=b2"))
    (should (string= (denote-sequence-get-new 'sibling "2" sequences) "11"))
    (should-error (denote-sequence-get-new 'sibling "12" sequences))

    (should (string= (denote-sequence-get-relative "1=b1a" 'parent files)
                     (expand-file-name "20241230T075023==1=b1--test__testing.txt" denote-directory)))
    (should (string= (denote-sequence-get-relative "10=a" 'parent files)
                     (expand-file-name "20241230T075023==10--test__testing.txt" denote-directory)))
    (should (dst-relative-p "1=b1a" 'all-parents
                            "20241230T075023==1--test__testing.txt"
                            "20241230T075023==1=b--test__testing.txt"
                            "20241230T075023==1=b1--test__testing.txt"))
    (should (dst-relative-p "1=a" 'siblings
                            "20241230T075023==1=a--test__testing.txt"
                            "20241230T075023==1=b--test__testing.txt"))
    (should (dst-relative-p "10=a" 'siblings
                            "20241230T075023==10=a--test__testing.txt"
                            "20241230T075023==10=b--test__testing.txt"))
    (should (dst-relative-p "1" 'children
                            "20241230T075023==1=a--test__testing.txt"
                            "20241230T075023==1=b--test__testing.txt"))
    (should (dst-relative-p "10" 'children
                            "20241230T075023==10=a--test__testing.txt"
                            "20241230T075023==10=b--test__testing.txt"))
    (should (dst-relative-p "1=a" 'all-children
                            "20241230T075023==1=a1--test__testing.txt"
                            "20241230T075023==1=a2--test__testing.txt"))))

(ert-deftest dst-denote-sequence-split ()
  "Test that `denote-sequence-split' splits a sequence correctly."
  (should (equal (denote-sequence-split "1") '("1")))
  (should (equal (denote-sequence-split "1=1=2") '("1" "1" "2")))
  (should (equal (denote-sequence-split "1za5zx") '("1" "za" "5" "zx")))
  (should (equal (denote-sequence-split "1=za5zx") '("1" "za" "5" "zx")))
  (should (equal (denote-sequence-split "1=a2b") '("1" "a" "2" "b"))))
  (should (equal (denote-sequence-split "1=a2b=1c3") '("1" "a" "2" "b" "1" "c" "3")))

(ert-deftest dst-denote-sequence-make-conversion ()
  "Test that `denote-sequence-make-conversion' converts from alpha to numeric and vice versa."
  (should (string= (denote-sequence-make-conversion "3" 'alphanumeric :string-is-partial-sequence) "c"))
  (should (string= (denote-sequence-make-conversion "18" 'alphanumeric :string-is-partial-sequence) "r"))
  (should (string= (denote-sequence-make-conversion "26" 'alphanumeric :string-is-partial-sequence) "z"))
  (should (string= (denote-sequence-make-conversion "27" 'alphanumeric :string-is-partial-sequence) "za"))
  (should (string= (denote-sequence-make-conversion "130" 'alphanumeric :string-is-partial-sequence) "zzzzz"))
  (should (string= (denote-sequence-make-conversion "131" 'alphanumeric :string-is-partial-sequence) "zzzzza"))
  (should (string= (denote-sequence-make-conversion "c" 'numeric :string-is-partial-sequence) "3"))
  (should (string= (denote-sequence-make-conversion "r" 'numeric :string-is-partial-sequence) "18"))
  (should (string= (denote-sequence-make-conversion "z" 'numeric :string-is-partial-sequence) "26"))
  (should (string= (denote-sequence-make-conversion "za" 'numeric :string-is-partial-sequence) "27"))
  (should (string= (denote-sequence-make-conversion "zzzzz" 'numeric :string-is-partial-sequence) "130"))
  (should (string= (denote-sequence-make-conversion "zzzzza" 'numeric :string-is-partial-sequence) "131"))
  (should (string= (denote-sequence-make-conversion "1=1=2" 'alphanumeric) "1a2"))
  (should (string= (denote-sequence-make-conversion "1a2" 'numeric) "1=1=2"))
  (should (string= (denote-sequence-make-conversion "1=27=2=55" 'alphanumeric) "1za2zzc"))
  (should (string= (denote-sequence-make-conversion "1=27=2=55" 'alphanumeric-delimited) "1=za2zzc"))
  (should (string= (denote-sequence-make-conversion "1za2zzc" 'numeric) "1=27=2=55"))
  (should (string= (denote-sequence-make-conversion "1=1=2=2=4=1" 'alphanumeric) "1a2b4a"))
  (should (string= (denote-sequence-make-conversion "1=1=2=2=4=1" 'alphanumeric-delimited) "1=a2b=4a"))
  (should (string= (denote-sequence-make-conversion "1a2b4a" 'numeric) "1=1=2=2=4=1")))

(ert-deftest dst-denote-sequence-increment-partial ()
  "Test that `denote-sequence-increment-partial' does the right thing."
  (should (string= (denote-sequence-increment-partial "1") "2"))
  (should (string= (denote-sequence-increment-partial "a") "b"))
  (should (string= (denote-sequence-increment-partial "z") "za"))
  (should (string= (denote-sequence-increment-partial "zz") "zza"))
  (should (string= (denote-sequence-increment-partial "bbcz") "bbcza"))
  (should-error (denote-sequence-increment-partial "1=1"))
  (should-error (denote-sequence-increment-partial "1a")))

(ert-deftest dst-denote-sequence-decrement-partial ()
  "Test that `denote-sequence-decrement-partial' does the right thing."
  (should (null (denote-sequence-decrement-partial "1")))
  (should (null (denote-sequence-decrement-partial "a")))
  (should (string= (denote-sequence-decrement-partial "2") "1"))
  (should (string= (denote-sequence-decrement-partial "b") "a"))
  (should (string= (denote-sequence-decrement-partial "za") "z"))
  (should (string= (denote-sequence-decrement-partial "zza") "zz"))
  (should (string= (denote-sequence-decrement-partial "bbcza") "bbcz"))
  (should-error (denote-sequence-decrement-partial "1a")))

(ert-deftest dst-denote-sequence--infer-sibling ()
  "Test that `denote-sequence--infer-sibling' returns the correct result."
  (should (string= (denote-sequence--infer-sibling "1" 'next) "2"))
  (should (string= (denote-sequence--infer-sibling "1a" 'next) "1b"))
  (should (string= (denote-sequence--infer-sibling "1z" 'next) "1za"))
  (should (string= (denote-sequence--infer-sibling "1=1" 'next) "1=2"))
  (should (string= (denote-sequence--infer-sibling "2" 'previous) "1"))
  (should (string= (denote-sequence--infer-sibling "1b" 'previous) "1a"))
  (should (string= (denote-sequence--infer-sibling "1za" 'previous) "1z"))
  (should (string= (denote-sequence--infer-sibling "1=2" 'previous) "1=1"))
  (should (null (denote-sequence--infer-sibling "1" 'previous)))
  (should (null (denote-sequence--infer-sibling "1=1" 'previous)))
  (should (null (denote-sequence--infer-sibling "1a" 'previous))))

(ert-deftest dst-denote-sequence--keep-siblings ()
  "Test that `denote-sequence--keep-siblings' behaves as expected."
  (should (equal
           (denote-sequence--keep-siblings :greater "1b" '("1f" "1b" "1a" "1d" "1e" "1c"))
           '("1c" "1d" "1e" "1f")))
  (should (equal
           (denote-sequence--keep-siblings :lesser "1d" '("1f" "1b" "1a" "1d" "1e" "1c"))
           '("1a" "1b" "1c")))
  (should (null (denote-sequence--keep-siblings :greater "1f" '("1f" "1b" "1a" "1d" "1e" "1c"))))
  (should (null (denote-sequence--keep-siblings :lesser "1a" '("1f" "1b" "1a" "1d" "1e" "1c")))))

(defun dst-denote-sequence--keep-sibling-files-helper (lesser-or-greater sequence)
  "Use LESSER-OR-GREATER with SEQUENCE to do `denote-sequence--keep-sibling-files'."
  (let ((files '("00000000T000000==1b--title__keywords.org"
                 "00000000T000000==1w--title__keywords.org"
                 "00000000T000000==1d--title__keywords.org"
                 "00000000T000000==1a--title__keywords.org"
                 "00000000T000000==1c--title__keywords.org")))
    (denote-sequence--keep-sibling-files lesser-or-greater sequence files)))

(ert-deftest dst-denote-sequence--keep-sibling-files ()
  "Test that `denote-sequence--keep-sibling-files' behaves as expected."
  (should (equal
           (dst-denote-sequence--keep-sibling-files-helper :greater "1b")
           '("00000000T000000==1b--title__keywords.org"
             "00000000T000000==1c--title__keywords.org"
             "00000000T000000==1d--title__keywords.org"
             "00000000T000000==1w--title__keywords.org")))
  (should (equal
           (dst-denote-sequence--keep-sibling-files-helper :lesser "1b")
           '("00000000T000000==1a--title__keywords.org")))
  (should (null (dst-denote-sequence--keep-sibling-files-helper :greater "1z")))
  (should (null (dst-denote-sequence--keep-sibling-files-helper :lesser "1a"))))

(ert-deftest dst-denote-sequence-dired--get-files ()
  "Test that `denote-sequence-dired--get-files' returns files in the correct format."
  (let ((denote-sequence-scheme 'numeric)
        (denote-directory (expand-file-name "denote-sequence-test" temporary-file-directory))
        (names '("20241230T075023==1--test__testing.txt"
                 "20241230T075023==1=1--test__testing.txt"
                 "20241230T075023==1=1=1--test__testing.txt"
                 "20241230T075023==1=1=2--test__testing.txt"
                 "20241230T075023==1=2--test__testing.txt"
                 "20241230T075023==1=2=1--test__testing.txt"
                 "20241230T075023==1=2=1=1--test__testing.txt"
                 "20241230T075023==2--test__testing.txt"
                 "20241230T075023==10--test__testing.txt"
                 "20241230T075023==10=1--test__testing.txt"
                 "20241230T075023==10=1=1--test__testing.txt"
                 "20241230T075023==10=2--test__testing.txt"
                 "20241230T075023==10=10--test__testing.txt"
                 "20241230T075023==10=10=1--test__testing.txt")))
    (dolist (name names)
      (let ((path (expand-file-name name (car (denote-directories)))))
        (if (file-exists-p path)
            path
          (with-current-buffer (find-file-noselect path)
            (save-buffer)
            (kill-buffer (current-buffer)))
          path)))
    (should-not (denote-sequence-dired--get-files "3" nil))
    (should-not (denote-sequence-dired--get-files "3" 1))
    (should (equal (denote-sequence-dired--get-files "1" 1) (list "20241230T075023==1--test__testing.txt")))
    (should (equal (denote-sequence-dired--get-files "1" 2) (list "20241230T075023==1--test__testing.txt" "20241230T075023==1=1--test__testing.txt" "20241230T075023==1=2--test__testing.txt")))
    (should
     (equal
      (denote-sequence-dired--get-files "1" nil)
      (list "20241230T075023==1--test__testing.txt" "20241230T075023==1=1--test__testing.txt" "20241230T075023==1=1=1--test__testing.txt"
            "20241230T075023==1=1=2--test__testing.txt" "20241230T075023==1=2--test__testing.txt" "20241230T075023==1=2=1--test__testing.txt"
            "20241230T075023==1=2=1=1--test__testing.txt")))
    (should
     (equal
      (denote-sequence-dired--get-files nil 2)
      (list "20241230T075023==1--test__testing.txt" "20241230T075023==1=1--test__testing.txt" "20241230T075023==1=2--test__testing.txt"
            "20241230T075023==2--test__testing.txt" "20241230T075023==10--test__testing.txt" "20241230T075023==10=1--test__testing.txt"
            "20241230T075023==10=2--test__testing.txt" "20241230T075023==10=10--test__testing.txt")))
    (should (equal (denote-sequence-dired--get-files nil nil) names))))

(provide 'denote-sequence-test)
;;; denote-sequence-test.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("dst" . "denote-sequence-test-"))
;; End:
