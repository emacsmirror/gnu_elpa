;;; vc-jj-tests.el --- tests for vc-jj.el            -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Rudolf Schlatte

;; Author: Rudolf Schlatte <rudi@constantly.at>

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

(require 'ert-x)
(require 'vc)
(require 'vc-dir)
(require 'vc-jj)

(defmacro vc-jj-test--with-repo (name &rest body)
  "Initialize a repository in a temporary directory and evaluate BODY.

The current directory will be set to the top of that repository; NAME
will be bound to that directory's file name.  Once BODY exits, the
directory will be deleted.

Some environment variables that control jj's behavior will be set
for the duration of BODY."
  (declare (indent 1))
  `(ert-with-temp-directory ,name
     (let ((default-directory ,name)
           ;; Note: if we need reproducible repository state, use
           ;; JJ_RANDOMNESS_SEED=12345 when calling `jj git init', and
           ;; increase its value by 1 or call `jj config set --repo
           ;; debug.randomness-seed 12346' etc. between each jj call
           ;; afterwards -- see
           ;; https://github.com/jj-vcs/jj/blob/d79c7a0dd5b8f9d3d6f9436452dcf0e1600b0b14/cli/tests/common/test_environment.rs#L115
           ;; for other relevant environment variables.
           (process-environment (append '("JJ_EMAIL=john@example.com"
                                          "JJ_USER=john")
                                        process-environment)))
       (vc-create-repo 'jj)
       ,@body)))

(ert-deftest vc-jj-test-add-file ()
  (vc-jj-test--with-repo repo
    (write-region "New file" nil "README")
    (should (vc-jj--file-tracked "README"))
    (should (vc-jj--file-added "README"))
    (should (not (vc-jj--file-modified "README")))
    (should (not (vc-jj--file-conflicted "README")))
    (should (eq (vc-state "README" 'jj) 'added))))

(ert-deftest vc-jj-test-added-tracked ()
  (vc-jj-test--with-repo repo
    (write-region "In first commit" nil "first-file")
    (vc-jj-checkin '("first-file") "First commit")
    (write-region "In second commit" nil "second-file")
    (should (eq (vc-jj-state "second-file") 'added))
    (should (eq (vc-jj-state "first-file") 'up-to-date))))

(ert-deftest vc-jj-test-conflict ()
  (vc-jj-test--with-repo repo
    (let (branch-1 branch-2 branch-merged)
      ;; the root change id is always zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
      (shell-command "jj new zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
      (setq branch-1 (vc-jj-working-revision "."))
      (write-region "Unconflicted" nil "unconflicted.txt")
      (write-region "Branch 1" nil "conflicted.txt")
      (make-directory "subdir")
      (write-region "Branch 1" nil "subdir/conflicted.txt")
      (shell-command "jj new zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
      (setq branch-2 (vc-jj-working-revision "."))
      (write-region "Unconflicted" nil "unconflicted.txt")
      (write-region "Branch 2" nil "conflicted.txt")
      (make-directory "subdir")
      (write-region "Branch 2" nil "subdir/conflicted.txt")
      (shell-command (concat "jj new " branch-1 " " branch-2))
      (should (eq (vc-jj-state "unconflicted.txt") 'up-to-date))
      (should (eq (vc-jj-state "conflicted.txt") 'conflict))
      (should (eq (vc-jj-state "subdir/conflicted.txt") 'conflict)))))

(provide 'vc-jj-tests)
;;; vc-jj-tests.el ends here
