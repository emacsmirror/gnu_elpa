;;; auctex-label-numbers-tests.el --- tests for auctex-label-numbers.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for auctex-label-numbers.el.

;;; Code:

(require 'ert)
(require 'auctex-label-numbers)

(ert-deftest
    test-auctex-label-numbers--external-document-regexp ()
  "Test the regexp for \\externaldocument commands."

  ;; Test with no optional argument
  (should (string-match auctex-label-numbers--external-document-regexp
                        "\\externaldocument{somefile}"))
  (should (equal (match-string 1 "\\externaldocument{somefile}") nil))
  (should (equal (match-string 2 "\\externaldocument{somefile}") "somefile"))

  ;; Test with optional argument
  (should (string-match auctex-label-numbers--external-document-regexp
                        "\\externaldocument[PRE]{otherfile}"))
  (should (equal (match-string 1 "\\externaldocument[PRE]{otherfile}") "PRE"))
  (should (equal (match-string 2 "\\externaldocument[PRE]{otherfile}") "otherfile"))

  ;; Test with \externalcitedocument
  (should (string-match auctex-label-numbers--external-document-regexp
                        "\\externalcitedocument{citefile}"))
  (should (equal (match-string 1 "\\externalcitedocument{citefile}") nil))
  (should (equal (match-string 2 "\\externalcitedocument{citefile}") "citefile"))

  ;; Test with \externalcitedocument and optional argument
  (should (string-match auctex-label-numbers--external-document-regexp
                        "\\externalcitedocument[CITE]{citefile2}"))
  (should (equal (match-string 1 "\\externalcitedocument[CITE]{citefile2}") "CITE"))
  (should (equal (match-string 2 "\\externalcitedocument[CITE]{citefile2}") "citefile2"))

  ;; Test with more complex filenames and prefixes
  (should (string-match auctex-label-numbers--external-document-regexp
                        "\\externaldocument[ch1:]{chapter1-file}"))
  (should (equal (match-string 1 "\\externaldocument[ch1:]{chapter1-file}") "ch1:"))
  (should (equal (match-string 2 "\\externaldocument[ch1:]{chapter1-file}") "chapter1-file"))

  ;; Test that it doesn't match invalid syntax
  (should-not (string-match auctex-label-numbers--external-document-regexp
                            "\\externaldocument{missingbracket"))
  (should-not (string-match auctex-label-numbers--external-document-regexp
                            "\\externaldocument[]{empty}")) ; Empty optional argument
  (should-not (string-match auctex-label-numbers--external-document-regexp
                            "\\externaldocument[prefix]nobraces")))

(ert-deftest test-auctex-label-numbers-fold-spec-signatures ()
  "Preserve AUCTeX signatures while replacing folding displays."
  (let ((auctex-label-numbers-macro-list '("ref" "eqref" "label"))
        (auctex-label-numbers--saved-spec-list nil)
        (TeX-fold-macro-spec-list
         '((("[l]" . TeX-fold-stop-after-first-mandatory) ("label"))
           (("[r]" . 1) ("ref" "eqref"))))
        (preview-preprocess-functions nil)
        (TeX-fold-mode nil))
    (unwind-protect
        (progn
          (auctex-label-numbers-mode 1)
          (should
           (member '((auctex-label-numbers-ref-display . 1) ("ref"))
                   TeX-fold-macro-spec-list))
          (should
           (member '((auctex-label-numbers-eqref-display . 1) ("eqref"))
                   TeX-fold-macro-spec-list))
          (should
           (member
            '((auctex-label-numbers-label-display
               . TeX-fold-stop-after-first-mandatory)
              ("label"))
            TeX-fold-macro-spec-list)))
      (auctex-label-numbers-mode -1))
    (should-not
     (seq-some
      (lambda (spec)
        (memq (if (consp (car spec)) (caar spec) (car spec))
              '(auctex-label-numbers-ref-display
                auctex-label-numbers-eqref-display
                auctex-label-numbers-label-display)))
      TeX-fold-macro-spec-list))))

(ert-deftest test-auctex-label-numbers--external-documents ()
  "Test collection of \\externaldocument declarations."
  (with-temp-buffer
    (insert "\\externaldocument[A-]{alpha}\n"
            "\\externaldocument{beta}\n"
            "\\externalcitedocument[C-]{gamma}\n")
    (should (equal (auctex-label-numbers--external-documents)
                   '((1 "A-" "alpha")
                     (2 nil "beta")
                     (3 "C-" "gamma"))))))

(provide 'auctex-label-numbers-tests)
;;; auctex-label-numbers-tests.el ends here
