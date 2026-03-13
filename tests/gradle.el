;;; tests/gradle.el --- javaimp Gradle tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert)
(require 'ert-x)
(require 'javaimp-gradle)
(require 'javaimp-tests)

;; Note that you have to set up `javaimp-gradle-program' to point to
;; Gradle installation, which is rather untypical for Gradle projects
;; (most often, a "Gradle wrapper" is used, resulting in project-local
;; copy of Gradle).

(defun javaimp-test-gradle--get-tree (project-dir)
  (javaimp-tree-map-nodes
   (lambda (mod)
     (cons t
           (cons (javaimp-print-id (javaimp-module-id mod))
                 (file-relative-name
                  (javaimp-module-file mod)
                  ;; Gradle seems to report filenames with symlinks
                  ;; resolved (so we get /private/var/ instead of
                  ;; /var/ on macOS), so do it too
                  (file-truename project-dir)))))
   #'always
   (javaimp-gradle-visit (concat project-dir "build.gradle"))))


(ert-deftest javaimp-gradle-visit-multi ()
  :tags '(:expensive)
  (javaimp-call-with-data
   (ert-resource-file "gradle-multi.tar.gz")
   (lambda (tmpdir)
     (should
      (equal
       (javaimp-test-gradle--get-tree
        (concat tmpdir (file-name-as-directory "multi")))
       '((("<root>:org.example:1.0" . "build.gradle")
          (("child:org.example:1.0" . "child/build.gradle")
           (("child.grandchild:org.example:1.0" . "child/grandchild/build.gradle")))
          ;; directory layout different from project layout
          (("non-direct-child:org.example:1.0" . "foo/non-direct-child/build.gradle"))
          )))))))
