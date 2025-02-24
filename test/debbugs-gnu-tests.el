;;; debbugs-gnu-tests.el --- tests for debbugs-gnu.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Morgan Smith <Morgan.J.Smith@outlook.com>
;; Package: debbugs

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Please ensure tests don't actually make network calls.

;;; Code:

(require 'ert)
(require 'debbugs-gnu)
(require 'debbugs-test-helpers)

;;; Tests:

(ert-deftest--debbugs debbugs-test-gnu-search ()
  "Test `debbugs-gnu-search'."
  (cl-letf (((symbol-function #'debbugs-gnu)
             #'list))
    (should
     (equal '(nil ("guix" "guix-patches") nil)
            (debbugs-gnu-search "frogs" '((pending . "pending")) nil '("guix" "guix-patches") nil)))
    (should (equal debbugs-gnu-current-query '((phrase . "frogs"))))
    (should (equal debbugs-gnu-current-filter '((pending . "pending"))))))

(provide 'debbugs-gnu-tests)

;;; debbugs-gnu-tests.el ends here
