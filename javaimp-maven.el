;;; javaimp-maven.el --- javaimp maven support  -*- lexical-binding: t; -*-

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


;;; Code:

(require 'javaimp-util)

(defcustom javaimp-mvn-program "mvn"
  "Path to the `mvn' program.  If the visited project has local
mvnw (Maven wrapper), it is used in preference."
  :type 'string
  :group 'javaimp)


(defun javaimp-maven-visit (file)
  "Calls \"mvn help:effective-pom\" on FILE,
reads project structure from the output and records which files
belong to which modules and other module information.  Returns
resulting module trees."
  (when javaimp-verbose
    (message "Visiting Maven POM file %s..." file))
  (let* ((xml-tree (javaimp-maven--call
                    file
                    #'javaimp-maven--effective-pom-handler
                    "help:effective-pom"))
	 (projects (or (if (assq 'project xml-tree)
                           (list (assq 'project xml-tree))
                         (javaimp-xml-children (assq 'projects xml-tree) 'project))
                       (user-error "No projects found")))
	 (modules (mapcar (lambda (proj-elt)
                            (javaimp-maven--module-from-xml proj-elt file))
                          projects)))
    ;; Set :file slots in a separate step because Maven doesn't give
    ;; pom.xml file location for each subproject.
    (javaimp-maven--fill-modules-files file modules)
    (when-let* ((without-files (seq-filter (lambda (m)
                                             (null (javaimp-module-file m)))
                                           modules)))
      (error "Cannot find file for module(s): %s"
             (mapconcat (lambda (m)
                          (javaimp-print-id (javaimp-module-id m)))
                        without-files
                        ", ")))
    (dolist (m modules)
      (setf (javaimp-module-raw m) nil)) ;no longer need it
    ;; Build project trees
    (let ((roots
           (cons
            (car modules)               ;first module is always root
            ;; In some rare cases, a project inside a tree has parent
            ;; outside the tree (examples: "log4j-bom" in
            ;; log4j-2.11.2, "external" in jaxb-ri-2.3.3).  Such
            ;; projects didn't get into the tree, and we need to add
            ;; them a separate roots.
            (seq-filter (lambda (m)
                          (not (seq-find (lambda (aux)
                                           (equal (javaimp-module-id aux)
                                                  (javaimp-module-parent-id m)))
                                         modules)))
		        (cdr modules)))))
      (mapcar (lambda (root)
                (when javaimp-verbose
                  (message "Building tree for root: %s"
                           (javaimp-print-id (javaimp-module-id root))))
                (javaimp-tree-build
                 root modules
	         ;; more or less reliable way to find children is to
	         ;; look for modules with "this" as the parent
                 (lambda (el tested)
                   (equal (javaimp-module-parent-id tested)
                          (javaimp-module-id el)))))
              roots))))

(defun javaimp-maven--effective-pom-handler ()
  (let ((start
	 (save-excursion
	   (progn
	     (goto-char (point-min))
	     (re-search-forward "<\\?xml\\|<projects?")
	     (match-beginning 0))))
	(end
	 (save-excursion
	   (progn
	     (goto-char (point-min))
	     (re-search-forward "<\\(projects?\\)")
	     ;; find corresponding close tag
	     (search-forward (concat "</" (match-string 1) ">"))
	     (match-end 0)))))
    (xml-parse-region start end)))

(defun javaimp-maven--module-from-xml (elt file-orig)
  (let* ((build-elt (javaimp-xml-child 'build elt))
         (build-dir
          (file-name-as-directory
	   (javaimp-cygpath-convert-file-name
	    (javaimp-xml-first-child (javaimp-xml-child 'directory build-elt))))))
    (make-javaimp-module
     :id (javaimp-maven--id-from-xml elt)
     :parent-id (javaimp-maven--id-from-xml (javaimp-xml-child 'parent elt))
     ;; <project> element does not contain pom file path, so we set this slot
     ;; later, see javaimp-maven--fill-modules-files
     :file nil
     :file-orig file-orig
     :artifact (let ((packaging (or (javaimp-xml-first-child
		                     (javaimp-xml-child 'packaging elt))
                                    "jar")))
                 ;; only jar/war supported
                 (when (member packaging '("jar" "war"))
                   (file-name-concat
                    build-dir
                    (concat (javaimp-xml-first-child
                             (javaimp-xml-child 'finalName build-elt))
                            "." packaging))))
     :source-dirs (list (file-name-as-directory
		         (javaimp-cygpath-convert-file-name
		          (javaimp-xml-first-child
		           (javaimp-xml-child 'sourceDirectory build-elt))))
                        (file-name-as-directory
		         (javaimp-cygpath-convert-file-name
			  (javaimp-xml-first-child
			   (javaimp-xml-child 'testSourceDirectory build-elt)))))
     :build-dir build-dir
     ;; Because dependency list is retrieved by a separate command, we
     ;; initialize it lazily on demand
     :dep-jars t
     :dep-jars-with-source t
     :load-ts (current-time)
     :dep-jars-fetcher #'javaimp-maven--fetch-dep-jars
     :raw elt)))

(defun javaimp-maven--id-from-xml (elt)
  (make-javaimp-id
   :group (javaimp-xml-first-child (javaimp-xml-child 'groupId elt))
   :artifact (javaimp-xml-first-child (javaimp-xml-child 'artifactId elt))
   :version (javaimp-xml-first-child (javaimp-xml-child 'version elt))))

(defun javaimp-maven--fill-modules-files (file modules)
  "Reads <module> ids from FILE, looks up corresponding modules in
MODULES, sets their :file slot, then recurses for each of them.
A submodule file path is constructed by appending relative path
taken from <module> to FILE's directory.  This seems to be the
most reliable way, because relationships between modules in Maven
are somewhat arbitrary."
  (let* ((xml-tree (with-temp-buffer
		     (insert-file-contents file)
		     (xml-parse-region (point-min) (point-max))))
	 (project-elt (assq 'project xml-tree))
	 (this-id (javaimp-maven--id-from-xml project-elt))
         (module (seq-find
                  (lambda (m)
                    (let ((mid (javaimp-module-id m)))
                      (and (equal (javaimp-id-artifact this-id)
				  (javaimp-id-artifact mid))
	                   ;; Group and version may be inherited and
	                   ;; thus not presented in file
                           (or (null (javaimp-id-group this-id))
                               (equal (javaimp-id-group this-id)
				      (javaimp-id-group mid)))
                           (or (null (javaimp-id-version this-id))
                               (equal (javaimp-id-version this-id)
				      (javaimp-id-version mid))))))
                  modules)))
    (unless module
      (error "Cannot find module corresponding to file \"%s\" (id %s)"
             file (javaimp-print-id this-id)))
    (and (null (javaimp-id-group this-id))
         (null (javaimp-id-version this-id))
         (message "File \"%s\" contains incomplete id, used lax matching" file))

    (setf (javaimp-module-file module) file)

    ;; Take <module> subelements from information we've got from
    ;; help:effective-pom because Maven can extend the list in the
    ;; file with elements from profiles
    (let* ((submodules-elt
            (javaimp-xml-child 'modules (javaimp-module-raw module)))
           (submodules (javaimp-xml-children submodules-elt 'module))
           (rel-paths (mapcar #'javaimp-xml-first-child submodules)))
      ;; recurse into each submodule
      (dolist (rel-path rel-paths)
	(javaimp-maven--fill-modules-files
         (concat (file-name-directory file)
                 (mapconcat #'file-name-as-directory
                            (split-string rel-path "[/\\\\]+" t) nil)
		 "pom.xml")
	 modules)))))

(defun javaimp-maven--fetch-dep-jars (module _ids)
  (javaimp-maven--call
   ;; always invoke for this module's pom.xml
   (javaimp-module-file module)
   (lambda ()
     (search-forward "Dependencies classpath:")
     (forward-line 1)
     (javaimp-split-native-path (thing-at-point 'line)))
   "dependency:build-classpath"
   ;; Invoke in original file's directory because there may be local
   ;; build tool wrapper.
   (file-name-directory (javaimp-module-file-orig module))))

(defun javaimp-maven--call (file handler task &optional dir)
  (let* ((default-directory (or dir (file-name-directory file)))
         ;; Prefer local mvn wrapper
         (local-mvnw (if (memq system-type '(windows-nt))
                         "mvnw.cmd"
                       "mvnw"))
         (program (if (file-exists-p local-mvnw)
                      (concat default-directory local-mvnw)
                    javaimp-mvn-program)))
    (message "Calling Maven task %s on %s ..." task file)
    (javaimp-call-java-program
     program
     handler
     "-f" (javaimp-cygpath-convert-file-name file)
     task)))

(provide 'javaimp-maven)
