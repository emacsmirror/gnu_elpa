;;; javaimp-gradle.el --- javaimp gradle support  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

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

(require 'javaimp-util)

(eval-when-compile (require 'subr-x))

(defcustom javaimp-gradle-program "gradle"
  "Path to the `gradle' program.  If the visited project has local
gradlew (Gradle wrapper), it is used in preference."
  :type 'string
  :group 'javaimp)


(defun javaimp-gradle-visit (file)
  "Calls gradle on FILE to get various project information.

Passes specially crafted init file as -I argument to gradle and
invokes task contained in it.  This task outputs all needed
information."
  (when javaimp-verbose
    (message "Visiting Gradle build file %s..." file))
  (let* ((alists (javaimp-gradle--call file #'javaimp-gradle--handler))
         (modules (mapcar (lambda (alist)
                            (javaimp-gradle--module-from-alist alist file))
                          alists)))
    ;; first module is always root
    (when javaimp-verbose
      (message "Building tree for root: %s"
               (javaimp-print-id (javaimp-module-id (car modules)))))
    (list
     (javaimp-tree-build (car modules) modules
	                 ;; more or less reliable way to find children
	                 ;; is to look for modules with "this" as the
	                 ;; parent
                         (lambda (el tested)
                           (equal (javaimp-module-parent-id tested)
                                  (javaimp-module-id el)))))))

(defun javaimp-gradle--handler ()
  "Parse current buffer into list of project descriptors, each of
which is an alist of attributes (NAME . VALUE).  Each attribute
occupies one line, and is of the form \"NAME=VALUE\".  See file
gradleTaskBody.inc.kts for the script which generates such
output.  Attribute \"id\" signifies the start of another
descriptor."
  (goto-char (point-min))
  (let (alist res sym val)
    (while (re-search-forward "^\\([[:alnum:]-]+\\)=\\(.*\\)$" nil t)
      (setq sym (intern (match-string 1))
            val (match-string 2))
      (if (string-blank-p val)
          (setq val nil))
      (when (and (eq sym 'id) alist)    ;start of next module
        (push alist res)
        (setq alist nil))
      (push (cons sym val) alist))
    (when alist                         ;last module
      (push alist res))
    (nreverse res)))

(defun javaimp-gradle--module-from-alist (alist file-orig)
  "Make `javaimp-module' structure out of attribute alist ALIST."
  (make-javaimp-module
   :id (javaimp-gradle--id-from-semi-separated
        (cdr (assq 'id alist)))
   :parent-id (javaimp-gradle--id-from-semi-separated
               (cdr (assq 'parent-id alist)))
   :file (cdr (assq 'file alist))
   :file-orig file-orig
   :artifact (when-let ((final-name (javaimp-cygpath-convert-file-name
                                     (cdr (assq 'final-name alist)))))
               ;; only jar/war supported
               (and (member (file-name-extension final-name) '("jar" "war"))
                    final-name))
   :source-dirs (mapcar #'file-name-as-directory
                        (javaimp-split-native-path
                         (cdr (assq 'source-dirs alist))))
   :build-dir (file-name-as-directory
               (javaimp-cygpath-convert-file-name
                (cdr (assq 'build-dir alist))))
   :dep-jars (javaimp-split-native-path (cdr (assq 'dep-jars alist)))
   :dep-jars-with-source t
   :load-ts (current-time)
   :dep-jars-fetcher #'javaimp-gradle--fetch-dep-jars
   :raw nil))

(defun javaimp-gradle--id-from-semi-separated (str)
  (when str
    (let ((parts (split-string str ";" t))
          artifact)
      (unless (= (length parts) 3)
        (error "Invalid project id: %s" str))
      (setq artifact (nth 1 parts))
      (if (equal artifact ":")
          (setq artifact "<root>")
        ;; convert "[:]foo:bar:baz" into "foo.bar.baz"
        (setq artifact (replace-regexp-in-string
                        ":" "." (string-remove-prefix ":" artifact))))
      (make-javaimp-id :group (nth 0 parts)
                       :artifact artifact
                       :version (nth 2 parts)))))

(defun javaimp-gradle--fetch-dep-jars (module ids)
  (javaimp-gradle--call
   ;; Always invoke on orig file (which is root build script) because
   ;; module's own file may not exist, even if reported by Gradle as
   ;; project.buildFile.  Furthermore, we use that file's directory to
   ;; determine for local build tool wrappers.
   (javaimp-module-file-orig module)
   (lambda ()
     (re-search-forward "^dep-jars=\\(.*\\)$")
     (javaimp-split-native-path (match-string 1)))
   (let ((mod-path (mapconcat #'javaimp-id-artifact (cdr ids) ":")))
     (unless (string-empty-p mod-path)
       (format ":%s:" mod-path)))))

(defun javaimp-gradle--call (file handler &optional mod-path)
  (let* (;; There is (was) "-b" switch for specifying build file,
         ;; however it became deprecated in Gradle 7, so we try to run
         ;; in build file directory.
         (default-directory (file-name-directory file))
         ;; Prefer local gradle wrapper
         (local-gradlew (if (eq system-type '(windows-nt))
                            "gradlew.bat"
                          "gradlew"))
         (program (if (file-exists-p local-gradlew)
                      (concat default-directory local-gradlew)
                    javaimp-gradle-program))
         (task (concat mod-path "javaimpTask")))
    (message "Calling Gradle task %s on %s ..." task file)
    (javaimp-call-java-program
     program
     handler
     "-q"
     ;; It's easier for us to track jars instead of classes for
     ;; java-library projects.  See
     ;; https://docs.gradle.org/current/userguide/java_library_plugin.html#sec:java_library_classes_usage
     "-Dorg.gradle.java.compile-classpath-packaging=true"
     "--no-configuration-cache"
     "-I" (javaimp-cygpath-convert-file-name
           (expand-file-name "javaimp-init-script.gradle"
                             (file-name-concat javaimp-basedir "support")))
     task)))

(provide 'javaimp-gradle)
