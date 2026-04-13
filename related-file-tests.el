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

(ert-deftest related-file-parse-input ()
  (should (equal (related-file--parse-input "ac2/b" 1)
                 '(nil (("a" point "c" "2") ("b")) nil)))
  (should (equal (related-file--parse-input "~/ac2/b" 3)
                 '("~/" (("a" point "c" "2") ("b")) nil)))
  (should (equal (related-file--parse-input "../ac2//b" 4)
                 '("../" (("a" point "c" "2")) (("b")))))
  (should (equal (related-file--parse-input ".//ac2/b" 4)
                 '("./" nil (("a" point "c" "2") ("b")))))
  (should (equal (related-file--parse-input "..//ac2/b" 5)
                 '("../" nil (("a" point "c" "2") ("b")))))
  (should (equal (related-file--parse-input "//ac2/b" 3)
                 '(nil nil (("a" point "c" "2") ("b")))))
  (should (equal (related-file--parse-input "///ac2/b" 4)
                 '("/" nil (("a" point "c" "2") ("b")))))

  (should (equal (nth 1 (related-file--parse-input "he*lp" 1))
                 '(("h" point "e" * "lp"))))
  (should (equal (nth 1 (related-file--parse-input "foo.c" 1))
                 '(("f" point "oo" "." "c"))))
  (should (equal (nth 1 (related-file--parse-input "he*lp/**/ba" 1))
                 '(("h" point "e" * "lp") ** ("ba"))))
  )

(defvar related-file-tests--known-file
  (or (macroexp-file-name) buffer-file-name))

(ert-deftest related-file-related-file ()
  (let* ((kdir2 (file-name-directory related-file-tests--known-file))
         (file (file-name-nondirectory related-file-tests--known-file))
         (kdir1 (file-name-directory (directory-file-name kdir2)))
         (dir2 (file-name-nondirectory (directory-file-name kdir2)))
         (kdir0 (file-name-directory (directory-file-name kdir1)))
         (dir1 (file-name-nondirectory (directory-file-name kdir1))))
    ;; (should (equal related-file-tests--known-file
    ;;                (file-name-concat kdir0 dir1 dir2 file)))
    (should (equal related-file-tests--known-file
                   (concat kdir0 dir1 "/" dir2 "/" file)))

    (let ((res (related-file dir1 (concat kdir0 "unlikely" "/" dir2 "/" file))))
      (should (equal (car res) kdir0))
      (should (member (concat dir1 "/" dir2 "/" file)
                      (mapcar (lambda (x) (related-file--to-string nil x))
                              (cdr res)))))

    (let ((res (related-file dir2
                             (concat kdir0 dir1 "/" "never-seen" "/" file))))
      (should (equal (car res) (concat kdir0 dir1 "/")))
      (should (member (concat dir2 "/" file)
                      (mapcar (lambda (x) (related-file--to-string nil x))
                              (cdr res)))))
    (let ((res (related-file
                (concat dir1 "/" file)
                (concat kdir0 "never-seen" "/" dir2 "/" "unlikely"))))
      (should (equal (car res) kdir0))
      (should (member (concat dir1 "/" dir2 "/" file)
                      (mapcar (lambda (x) (related-file--to-string nil x))
                              (cdr res)))))

    ))

(provide 'related-file-tests)
;;; related-file-tests.el ends here
