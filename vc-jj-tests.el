;;; vc-jj-tests.el --- tests for vc-jj.el            -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

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
(require 'iso8601)
(require 'cl-lib)
(require 'thingatpt)

(defun vc-jj-test-environment (seq)
  "Create a list suitable for prepending to `process-environment'.
The purpose is to make tests reproducible by fixing timestamps,
change ids, author information etc.  SEQ is an integer that
modifies the JJ_RANDOMNESS_SEED, JJ_TIMESTAMP and JJ_OP_TIMESTAMP
environment variables.  Increasing values for SEQ will result in
increasing timestamps.

Note that it not necessary to use this function, except when
stably increasing timestamps and stable change ids across test
runs are necessary."
  ;; For other potentially relevant variables, see
  ;; https://github.com/jj-vcs/jj/blob/d79c7a0dd5b8f9d3d6f9436452dcf0e1600b0b14/cli/tests/common/test_environment.rs#L115
  (let* ((startdate (iso8601-parse "2001-02-03T04:05:06+07:00"))
         (timezone (cl-ninth startdate))
         (offset (time-add (encode-time startdate) seq))
         (timestring (format-time-string "%FT%T%:z" offset timezone)))
    (list "JJ_EMAIL=john@example.com"
          "JJ_USER=john"
          (format "JJ_RANDOMNESS_SEED=%i" (+ 12345 seq))
          (format "JJ_TIMESTAMP=%s" timestring)
          (format "JJ_OP_TIMESTAMP=%s" timestring))))

(defmacro vc-jj-test-with-repo (name &rest body)
  "Initialize a repository in a temporary directory and evaluate BODY.
The current directory will be set to the top of that repository;
NAME will be bound to that directory's file name.  Once BODY
exits, the directory will be deleted.

jj commands are executed with a fixed username and email; augment
`process-environment' with `vc-jj-test-environment' if control
over timestamps and random number seed (and thereby change ids)
is needed."
  (declare (indent 1) (debug (symbolp body)))
  `(ert-with-temp-directory ,name
     (let ((default-directory ,name)
           (process-environment
            (append (list "JJ_EMAIL=john@example.com" "JJ_USER=john")
                    process-environment)))
       (let ((process-environment
              (append (vc-jj-test-environment 0) process-environment)))
         (vc-create-repo 'jj))
       ;; On macOS, the generated filename "/var/folders/..." was in
       ;; reality "/private/var/folders/...", which got unfolded by
       ;; `vc-jj-root' within some tests -- do this here already
       (let ((,name (vc-jj-root ,name))
             (default-directory ,name))
         ,@body))))

(ert-deftest vc-jj-test-add-file ()
  "Test the \"added\" vc state."
  (vc-jj-test-with-repo repo
    (write-region "New file" nil "README")
    (should (eq (vc-jj-state "README") 'added))
    (should (equal (vc-jj-dir-status-files repo nil (lambda (x y) x))
                   '(("README" added))))))

(ert-deftest vc-jj-test-added-tracked ()
  "Test the \"up-to-date\" vc state."
  (vc-jj-test-with-repo repo
    (write-region "In first commit" nil "first-file")
    (vc-jj-checkin '("first-file") "First commit")
    (write-region "In second commit" nil "second-file")
    (should (eq (vc-jj-state "second-file") 'added))
    (should (eq (vc-jj-state "first-file") 'up-to-date))
    (should (equal (vc-jj-dir-status-files repo nil (lambda (x y) x))
                   '(("second-file" added) ("first-file" up-to-date))))))

(ert-deftest vc-jj-delete-file ()
  "Test \"removed\" vc state and `vc-jj-delete-file'."
  (vc-jj-test-with-repo repo
    (write-region "First file" nil "first-file")
    (should (eq (vc-jj-state "first-file") 'added))
    (vc-jj-checkin '("first-file") "Commit")
    (vc-jj-delete-file "first-file")
    (should (eq (vc-jj-state "first-file") 'removed))
    (write-region "Second file" nil "second-file")
    (should (eq (vc-jj-state "second-file") 'added))
    (should (equal (vc-jj-dir-status-files repo nil (lambda (x y) x))
                   '(("second-file" added) ("first-file" removed))))))

(ert-deftest vc-jj-test-conflict ()
  "Test the \"conflict\" vc state."
  (vc-jj-test-with-repo repo
    (let (branch-1 branch-2 branch-merged)
      ;; the root change id is always zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
      (shell-command "jj new zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
      (write-region "Unconflicted" nil "unconflicted.txt")
      (write-region "Branch 1" nil "conflicted.txt")
      (make-directory "subdir")
      (write-region "Branch 1" nil "subdir/conflicted.txt")
      (setq branch-1 (vc-jj-working-revision "unconflicted.txt"))
      (shell-command "jj new zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
      (write-region "Unconflicted" nil "unconflicted.txt")
      (write-region "Branch 2" nil "conflicted.txt")
      (make-directory "subdir")
      (write-region "Branch 2" nil "subdir/conflicted.txt")
      (setq branch-2 (vc-jj-working-revision "unconflicted.txt"))
      (shell-command (concat "jj new " branch-1 " " branch-2))
      (should (eq (vc-jj-state "unconflicted.txt") 'up-to-date))
      (should (eq (vc-jj-state "conflicted.txt") 'conflict))
      (should (eq (vc-jj-state "subdir/conflicted.txt") 'conflict)))))

(ert-deftest vc-jj-test-annotate ()
  "Test `vc-annotate'."
  (vc-jj-test-with-repo repo
    (let ( change-1 change-2
           readme-buffer annotation-buffer)
      ;; Create two changes, make sure that the change ids in the
      ;; annotation buffer match.  This test is supposed to detect
      ;; changes in the output format of `jj annotate'.
      (unwind-protect
          (progn
            (write-region "Line 1\n" nil "README")
            (setq change-1 (vc-jj-working-revision "README"))
            (shell-command "jj commit -m 'First change'")
            (write-region "Line 2\n" nil "README" t)
            (shell-command "jj describe -m 'Second change'")
            (setq change-2 (vc-jj-working-revision "README"))
            (find-file "README")
            (setq readme-buffer (current-buffer))
            (vc-annotate "README" change-2)
            (let ((annotation-process (get-buffer-process (current-buffer))))
              (while (process-live-p annotation-process)
                (accept-process-output annotation-process)))
            (setq annotation-buffer (current-buffer))
            (goto-char (point-min))
            (should (string-prefix-p (thing-at-point 'word) change-1))
            (forward-line)
            (should (string-prefix-p (thing-at-point 'word) change-2)))
        (when (buffer-live-p readme-buffer) (kill-buffer readme-buffer))
        (when (buffer-live-p annotation-buffer) (kill-buffer annotation-buffer))))))

(ert-deftest vc-jj-ignore ()
  "Test \"ignored\" vc state."
  (vc-jj-test-with-repo repo
    (let (gitignore-buffer)
      (unwind-protect
          (progn
            ;; Create then register two files, one in the root and
            ;; another in a subdirectory
            (setq gitignore-buffer (find-file ".gitignore"))
            (write-region "xxx" nil "root-ignored.txt")
            (make-directory "subdir")
            (write-region "xxx" nil "subdir/subdir-ignored.txt")
            (should (eq (vc-jj-state "root-ignored.txt") 'added))
            (should (eq (vc-jj-state "subdir/subdir-ignored.txt") 'added))
            ;; Ignore both files
            (vc-jj-ignore "root-ignored.txt")
            (vc-jj-ignore "subdir/subdir-ignored.txt")
            (should (eq (vc-jj-state "root-ignored.txt") 'ignored))
            (should (eq (vc-jj-state "subdir/subdir-ignored.txt") 'ignored))
            ;; Un-ignore both files
            (vc-jj-ignore "root-ignored.txt" nil t)
            (vc-jj-ignore "subdir/subdir-ignored.txt" nil t)
            (should (eq (vc-jj-state "root-ignored.txt") 'added))
            (should (eq (vc-jj-state "subdir/subdir-ignored.txt") 'added)))
        (when (buffer-live-p gitignore-buffer)
          (kill-buffer gitignore-buffer))))))

(ert-deftest vc-jj-list-files ()
  "Test `vc-jj-dir-status-files' with a variety of vc states."
  (vc-jj-test-with-repo repo
    (let (branch-1 branch-2 branch-merged)
      ;; the root change id is always zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
      (shell-command "jj new zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
      (write-region "Unconflicted" nil "unconflicted.txt")
      (write-region "Branch 1" nil "conflicted.txt")
      (make-directory "subdir")
      (write-region "Branch 1" nil "subdir/conflicted.txt")
      (setq branch-1 (vc-jj-working-revision "unconflicted.txt"))
      (shell-command "jj new zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
      (write-region "Unconflicted" nil "unconflicted.txt")
      (write-region "Branch 2" nil "conflicted.txt")
      (make-directory "subdir")
      (write-region "Branch 2" nil "subdir/conflicted.txt")
      (setq branch-2 (vc-jj-working-revision "unconflicted.txt"))
      (shell-command (concat "jj new " branch-1 " " branch-2))
      (write-region "Added" nil "added.txt")
      (should (equal (vc-jj-dir-status-files repo nil (lambda (x y) x))
                     '(("conflicted.txt" conflict)
                       ("subdir/conflicted.txt" conflict)
                       ("added.txt" added)
                       ("unconflicted.txt" up-to-date))))
      (should (equal (vc-jj-dir-status-files (expand-file-name "subdir/" repo) nil (lambda (x y) x))
                     '(("conflicted.txt" conflict)))))))

(ert-deftest vc-jj-checkin-directory ()
  "Test checking in an entire directory of files.
This tests when a subdirectory path is passed to `vc-jj-checkin', rather
than a path to a regular file."
  ;; https://codeberg.org/emacs-jj-vc/vc-jj.el/issues/62
  (vc-jj-test-with-repo repo
    (make-directory "subdir")
    (write-region "foo" nil "subdir/file1.txt")
    (write-region "bar" nil "subdir/file2.txt")
    (make-directory "subdir/deeper_subdir")
    (write-region "baz" nil "subdir/deeper_subdir/file3.txt")
    (should (eq (vc-jj-state "subdir/file1.txt") 'added))
    (should (eq (vc-jj-state "subdir/file2.txt") 'added))
    (should (eq (vc-jj-state "subdir/deeper_subdir/file3.txt") 'added))
    (vc-jj-checkin (list "subdir/") "Sample commit message")
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo nil (lambda (x y) x))
             '(("subdir/file1.txt" up-to-date)
               ("subdir/file2.txt" up-to-date)
               ("subdir/deeper_subdir/file3.txt" up-to-date))))))

(ert-deftest vc-jj-funky-filename ()
  "Test compatibility with unusual characters in file names.
We test the presence of apostrophes, double quotes, and the equal sign
in file names, which are allowed in Linux.  See
https://codeberg.org/emacs-jj-vc/vc-jj.el/issues/38."
  (vc-jj-test-with-repo repo
    (write-region "Hello" nil "TEST=TEST.txt")
    (write-region "Hello" nil "with'apostrophe.txt")
    (write-region "Hello" nil "with\"quotation.txt")
    (should (eq (vc-jj-state "TEST=TEST.txt") 'added))
    (should (eq (vc-jj-state "with'apostrophe.txt") 'added))
    (should (eq (vc-jj-state "with\"quotation.txt") 'added))
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo nil (lambda (x y) x))
             '(("TEST=TEST.txt" added)
               ("with'apostrophe.txt" added)
               ("with\"quotation.txt" added))))))

(ert-deftest vc-jj-very-large-file ()
  "Test very large files.
Jujutsu usually prints to stderr when there is too large of a file
registered.  See https://codeberg.org/emacs-jj-vc/vc-jj.el/issues/52."
  (vc-jj-test-with-repo repo
    (shell-command "jj config set --repo snapshot.max-new-file-size 12")
    (write-region "1234567890" nil "numbers.txt")
    (write-region "abcdefghijklmnopqrstuvwxyz" nil "alphabet.txt")
    (should (eq (vc-jj-state "numbers.txt") 'added))
    (should (eq (vc-jj-state "alphabet.txt") 'ignored))))

(ert-deftest vc-jj-tolerate-repo-corruption ()
  "Test functionality after removing .git in a colocated repository.
See https://codeberg.org/emacs-jj-vc/vc-jj.el/issues/63."
  (let ((current-prefix-arg 4))         ; create git co-located repo
    (vc-jj-test-with-repo repo
      (write-region "Hello!" nil "README")
      (shell-command "rm -r .git")
      (should (eq (vc-jj-dir-status-files repo nil (lambda (x y) x))
                  nil)))))

(provide 'vc-jj-tests)
;;; vc-jj-tests.el ends here
