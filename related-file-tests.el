;;; related-file-tests.el --- Tests for the related-file package  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Stefan Monnier

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
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

;; 

;;; Code:

(require 'related-file)

(ert-deftest related-file--parse-input ()
  (should (equal (related-file--parse-input "a2/b" -1)
                 '(nil (("a" "2") ("b")) nil)))
  (should (equal (related-file--parse-input "~/a2/b" -1)
                 '("~/" (("a" "2") ("b")) nil)))
  (should (equal (related-file--parse-input "../a2//b" -1)
                 '("../" (("a" "2")) (("b")))))
  (should (equal (related-file--parse-input "//a2/b" -1)
                 '("/" nil (("a" "2") ("b"))))))

(provide 'related-file-tests)
;;; related-file-tests.el ends here
