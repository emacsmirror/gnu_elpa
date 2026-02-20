;;; javaimp.el --- Add and reorder Java import statements in Maven/Gradle projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2022  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>
;; Version: 0.9.1
;; Keywords: java, maven, gradle, programming

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

;; Allows to manage Java import statements in Maven / Gradle projects,
;; plus some editing and navigation support.  This package does not
;; add all needed imports automatically!  It only helps you to quickly
;; add imports when stepping through compilation errors.
;;
;; In addition, this package provides `javaimp-minor-mode' which
;; enables decent Imenu support (with nesting and abstract methods in
;; interfaces and abstract classes), xref support (finding definition
;; is implemented; finding references is left at default) and some
;; navigation functions, like beginning-of-defun.
;;
;;   Quick start
;;
;; - Put something like this in your .emacs:
;;
;; (require 'javaimp)
;; (add-to-list 'javaimp-import-group-alist
;;   '("\\`my.company\\." . 80))
;; (keymap-global-set "C-c J v" #'javaimp-visit-project)
;; (add-hook 'java-mode-hook #'javaimp-minor-mode)
;;
;; - Call `javaimp-visit-project', giving it the top-level build file
;; of your project.  If called within a project, supplies useful
;; default candidates in minibuffer input (topmost build file in the
;; current directory hierarchy, then nested ones).  If you don't visit
;; a project, then Javaimp will try to determine current source root
;; directory from the 'package' directive in the current file, and
;; will still offer some functions.
;;
;; - Then in a Java buffer visiting a file under that project or one
;; of its submodules call `javaimp-organize-imports' or
;; `javaimp-add-import'.
;;
;;
;;   Visiting projects & managing imports
;;
;; Lists of classes in archive and source files, and Maven / Gradle
;; project structures are cached, so usually only the first command
;; should take a considerable amount of time to complete.  Project
;; structure is re-read if a module's build file or any of its
;; parents' build files (within visited tree) was modified since last
;; check.
;;
;; `javaimp-flush-cache': command to clear jar / source cache.
;;
;; `javaimp-forget-visited-projects': command to forget all visited
;; projects.
;;
;; Project structure and dependency information is retrieved from the
;; corresponding build tool, see functions `javaimp-maven-visit' and
;; `javaimp-gradle-visit' for details (and generally, all handlers
;; mentioned in `javaimp-handler-regexp-alist').  The output from the
;; build tool can be inspected in the buffer named by
;; `javaimp-output-buf-name' variable.  If there exists Maven / Gradle
;; wrapper in the project directory, as it is popular these days, it
;; will be used in preference to `javaimp-mvn-program' /
;; `javaimp-gradle-program'.
;;
;; `javaimp-java-home': defcustom giving location of JDK to use.
;; Classes from JDK are included into import completion candidates.
;; Also, when invoking a Java program, JAVA_HOME environment variable
;; is added to the subprocess environment.  The variable is
;; initialized from JAVA_HOME environment variable, so typically you
;; won't need to customize it.
;;
;; If you get jar reading errors with Gradle despite following
;; recommendation which is shown (text from
;; `javaimp--jar-error-header' followed by offending jars), then it
;; might be the case that Gradle reordered build in such a way that
;; those jars are really not built yet.  In this case, just build them
;; manually, like: './gradlew :project1:build :project2:build'.
;;
;; `javaimp-add-import': command to add an import statement in the
;; current file.  See its docstring for how completion candidates are
;; collected.  When it is called for the first time in a given project
;; / module, it parses dependency archives, as well as JDK ones, and
;; it may take quite a while.
;;
;; `javaimp-organize-imports': command to organize imports in the
;; current buffer, sorting and deleting duplicates.
;;
;; If you don't visit a project, then Javaimp tries to determine
;; current source root directory (see
;; `javaimp--get-current-source-dir'), dependency information of
;; course will not be available, and you'll get completions only from
;; your current sources (and from JDK).
;;
;;
;;   Source parsing
;;
;; Parsing is implemented in javaimp-parse.el using `syntax-ppss',
;; generally is simple (we do not try to parse the source completely -
;; just the interesting pieces), but can be time-consuming for large
;; projects (to be improved).  Currently, on the author's machine,
;; source for java.util.Collections from JDK 11 (~ 5600 lines and >
;; 1000 "scopes") parses in ~1.5 seconds, which is not that bad...
;;
;; `javaimp-show-scopes': command to list all parsed "scopes" (blocks
;; of code in braces) in the current buffer, with support for
;; `next-error'.
;;
;; Parsing is also used for Imenu support, for xref support and for
;; navigation commands, these are installed by `javaimp-minor-mode'.
;;
;; `javaimp-imenu-use-sub-alists': if non-nil then Imenu items are
;; presented in a nested fashion, instead of a flat list (default is
;; flat list).


;;; Code:

(require 'javaimp-util)
(require 'javaimp-maven)
(require 'javaimp-gradle)
(require 'javaimp-parse)

(require 'imenu)
(require 'xref)

(eval-when-compile (require 'subr-x))

;; User options

(defgroup javaimp ()
  "Add and reorder Java import statements in Maven/Gradle
projects"
  :group 'c)

(defcustom javaimp-import-group-alist
  '(("\\`java\\." . 10)
    ("\\`javax\\." . 15))
  "Specifies how to group classes and how to order resulting
groups in the imports list.

Each element should be of the form (CLASSNAME-REGEXP . ORDER)
where CLASSNAME-REGEXP is a regexp matching the fully qualified
class name.  Lowest-order groups are placed earlier.

The order of classes which were not matched is defined by
`javaimp-import-default-order'."
  :type '(alist :key-type string :value-type integer))

(defcustom javaimp-import-default-order 50
  "Defines the order of classes which were not matched by
`javaimp-import-group-alist'"
  :type 'integer)

(defcustom javaimp-additional-source-dirs nil
  "List of directories where additional (e.g. generated)
source files reside.

Each directory in the list should be a name relative to project
build directory.

With Maven, typically you would check the documentation for a
particular plugin, look for default value there, and add it to
this list.
E.g. \"${project.build.directory}/generated-sources/<plugin_name>\"
becomes \"generated-sources/<plugin_name>\" (note the absence of
the leading slash)."
  :type '(repeat (string :tag "Relative directory")))

(defcustom javaimp-imenu-use-sub-alists nil
  "If non-nil, make sub-alist for each containing scope (e.g. a
class).  In this case, scopes nested inside methods (local
classes, anonymous classes and their methods) are not shown,
because otherwise it won't be possible to go to the method
itself.  Non-method scopes with no children are also omitted."
  :type 'boolean)

(defcustom javaimp-jar-program "jar"
  "Path to the `jar' program used to read contents of jar files."
  :type 'string)

(defcustom javaimp-jmod-program "jmod"
  "Path to the `jmod' program used to read contents of jmod files."
  :type 'string)



;; Structs

(cl-defstruct javaimp-cached-file
  file read-ts value)



;; Variables

(defvar javaimp-handler-regexp-alist
  `(("\\`build.gradle" . ,#'javaimp-gradle-visit)
    ("\\`pom.xml\\'" . ,#'javaimp-maven-visit))
  "Alist of file name patterns vs corresponding handler function.
A handler function takes one argument, a FILE.")

(defvar javaimp-project-forest nil
  "Visited projects")

;; These variables are all alists of (FILE . CACHED-FILE), where FILE
;; is expanded file name and CACHED-FILE is `javaimp-cached-file'
;; struct.  They're suitable for use with
;; `javaimp--collect-from-file-cached'.
(defvar javaimp--jar-idents-cache nil)
(defvar javaimp--module-idents-cache nil)
(defvar javaimp--source-idents-cache nil)

(defconst javaimp--jar-error-header
  "There were errors when reading some of the dependency files,
they are listed below.

Note that if you're using java-library plugin in Gradle for any
modules inside the project tree, then Gradle may avoid creating
jars for them.  You need to put this into
$HOME/.gradle/gradle.properties to force that:

systemProp.org.gradle.java.compile-classpath-packaging=true

For more info, see
https://docs.gradle.org/current/userguide/java_library_plugin.html\
#sec:java_library_classes_usage
")



;; Subroutines

(defsubst javaimp--get-file-ts (file)
  (file-attribute-modification-time (file-attributes file)))


(defun javaimp--collect-from-file (file cache-sym fun)
  "Return what FUN returns when invoked on FILE, with cache.  FILE
may be just a filename, or a cons cell where car is filename.
Use CACHE-SYM as a cache, it should be an alist with elements of
the form (FILENAME . CACHED-FILE).  If not found in cache, or the
cache is outdated, then values are read using FUN, which should
be a function of one argument, a FILE.  If that function throws
an error, the cache for FILENAME is cleared.  FUN may also be
nil, in which case the symbol t is returned for a cache miss, and
cache not updated."
  (let ((filename (if (consp file) (car file) file)))
    (condition-case err
        (let ((cached-file
               (alist-get filename (symbol-value cache-sym) nil nil #'string=)))
          (when (or (not cached-file)
                    ;; If the file doesn't exist this will be current
                    ;; time, and thus condition always true
                    (> (float-time (javaimp--get-file-ts filename))
	               (float-time (javaimp-cached-file-read-ts cached-file))))
            (setq cached-file (if fun
                                  (make-javaimp-cached-file
		                   :file filename
		                   :read-ts (javaimp--get-file-ts filename)
		                   :value (funcall fun file))
                                t)))
          (if (eq cached-file t)
              t
            (setf (alist-get filename (symbol-value cache-sym) nil 'remove #'string=)
                  cached-file)
            (javaimp-cached-file-value cached-file)))
      (t
       ;; Clear on any signal
       (setf (alist-get filename (symbol-value cache-sym) nil 'remove #'string=) nil)
       (signal (car err) (cdr err))))))

(defun javaimp--collect-from-files (fun files cache-sym what-desc
                                        &optional no-progress-report)
  "Collect values for FILES in a flat list.  Each element in FILES
should be a file name, or a cons where car is a file name.  FUN
and CACHE-SYM are passed to `javaimp--collect-from-file', which
see.  WHAT-DESC is included in the messages.  NO-PROGRESS-REPORT,
when non-nil, prevents progress reporter creation."
  (let (tmp unread res errors)
    ;; Collect from cache hits
    (dolist (file files)
      (setq tmp (javaimp--collect-from-file file cache-sym nil))
      (if (eq tmp t)
          (push file unread)
        (setq res (nconc res (copy-sequence tmp)))))
    ;; Now read all cache misses
    (when unread
      (let ((reporter (unless no-progress-report
                        (make-progress-reporter
                         (format "Reading %d %s files (%d taken from cache) ..."
                                 (length unread) what-desc
                                 (- (length files) (length unread)))
                         0 (length unread))))
            (i 0)
            filename)
        (dolist (file unread)
          (setq filename (if (consp file) (car file) file)
                tmp (condition-case err
                        (javaimp--collect-from-file file cache-sym fun)
                      (error
                       (push (concat filename ": " (error-message-string err))
                             errors)
                       nil)))
          (when tmp
            (setq res (nconc res (copy-sequence tmp))))
          (setq i (1+ i))
          (when reporter
            (progress-reporter-update reporter i filename)))
        (when reporter
          (progress-reporter-done reporter))))
    (when errors
      (with-output-to-temp-buffer "*Javaimp errors*"
        (princ javaimp--jar-error-header)
        (terpri)
        (dolist (err (nreverse errors))
          (princ err)
          (terpri))))
    res))

(defun javaimp--collect-from-source-dir (fun dir cache-sym what-desc)
  "For each Java source file in DIR, invoke FUN and collect results
in a flat list.  FUN is given two arguments: a buffer BUF, and
file name FILE, which is non-nil only if BUF is a temporary
buffer.  It should return a list of some values.

Files which are not visited in some buffer in the current session
are processed with `javaimp--collect-from-files', with CACHE-SYM
and WHAT-DESC given as corresponding arguments.  In this case we
visit the file in a temp buffer, and so FILE given to FUN will be
non-nil.

Unparsed or partially parsed buffers (as determined by
`javaimp-parse-fully-parsed-p') are processed in
`dolist-with-progress-reporter', without cache.

Finally, already parsed buffers are processed in
`with-delayed-message', without cache."
  (when (file-accessible-directory-p dir)
    (let ((sources (seq-filter (lambda (f)
                                 (not (file-symlink-p f)))
                               (directory-files-recursively dir "\\.java\\'")))
          files parsed-bufs unparsed-bufs)
      (dolist (s sources)
        (if-let* ((buf (get-file-buffer s)))
            (if (with-current-buffer buf
                  (javaimp-parse-fully-parsed-p))
                (push buf parsed-bufs)
              (push buf unparsed-bufs))
          (push s files)))
      (nconc
       ;; Read files
       (javaimp--collect-from-files
        (lambda (file)
          (with-temp-buffer
            (insert-file-contents file)
            (java-mode)
            (javaimp-minor-mode)
            (funcall fun (current-buffer) file)))
        files cache-sym
        (concat what-desc
                (when javaimp-verbose
                  (format " (dir %s)" dir))))
       ;; Parse unparsed buffers
       (when unparsed-bufs
         (let (tmp)
           (dolist-with-progress-reporter (buf unparsed-bufs tmp)
               (format "Parsed %d yet unparsed buffers..." (length unparsed-bufs))
             (setq tmp (nconc tmp (funcall fun buf nil))))))
       ;; Read parsed buffers - usually will be quick
       (when parsed-bufs
         (with-delayed-message
               (1 (format "Reading %d parsed buffers..." (length parsed-bufs)))
           (mapcan
            (lambda (buf)
              (funcall fun buf nil))
            parsed-bufs)))))))

(defun javaimp--get-current-source-dir ()
  "Try to determine current root source directory from \"package\"
directive in the current buffer.  If there's no such directive,
then just return `default-directory'."
  (if-let* ((package (save-excursion
                       (save-restriction
                         (widen)
                         (javaimp-parse-get-package)))))
      (string-remove-suffix
       (file-name-as-directory
        (apply #'file-name-concat (split-string package "\\." t)))
       default-directory)
    default-directory))



;; Subroutines for identifiers

(defun javaimp--read-dir-source-idents (scope-pred dir what-desc)
  (javaimp--collect-from-source-dir
   (apply-partially #'javaimp--collect-idents scope-pred)
   dir 'javaimp--source-idents-cache what-desc))

(defun javaimp--collect-idents (scope-pred buf file)
  "Return all identifiers satisfying SCOPE-PRED in buffer BUF,
which is temporary if FILE is non-nil."
  (with-current-buffer buf
    (save-excursion
      (save-restriction
        (widen)
        (let* ((package (javaimp-parse-get-package))
               (scopes (javaimp-parse-get-all-scopes nil nil scope-pred)))
          (mapcar (lambda (s)
                    (goto-char (javaimp-scope-open-brace s))
                    (propertize (javaimp-scope-name s)
                                'file (or file buffer-file-name)
                                'pos (point)
                                'line (line-number-at-pos)
                                'column (current-column)
                                'package package
                                'parents (javaimp-scope-concat-parents s)))
                  scopes))))))

(defun javaimp--ident-to-fqcn (ident)
  "Convert identifier IDENT to fully-qualified class name."
  (mapconcat #'identity
             (seq-filter (lambda (s) (not (string-empty-p s)))
                         (list
                          (get-text-property 0 'package ident)
                          (get-text-property 0 'parents ident)
                          ident))
             "."))


(defun javaimp--read-jar-classes (file)
  "Read FILE which should be a .jar or a .jmod and return classes
contained in it as a list."
  (let ((ext (downcase (file-name-extension file))))
    (unless (member ext '("jar" "jmod"))
      (error "Unexpected file name: %s" file))
    (let ((javaimp-output-buf-name nil))
      (javaimp-call-java-program
       (symbol-value (intern (format "javaimp-%s-program" ext)))
       #'javaimp--read-jar-classes-handler
       (if (equal ext "jar") "tf" "list")
       ;; On cygwin, "jar/jmod" is a native windows program, so file
       ;; path needs to be converted appropriately.
       (javaimp-cygpath-convert-file-name file 'windows)))))

(defun javaimp--read-jar-classes-handler ()
  "Used by `javaimp--read-jar-classes' to handle jar program
output."
  (let (result curr)
    (while (re-search-forward
            (rx (and bol
                     (? "classes/")     ; prefix output by jmod
                     (group (+ (any alnum "_/$")))
                     ".class"
                     eol))
            nil t)
      (setq curr (match-string 1))
      (unless (or (string-suffix-p "module-info" curr)
                  (string-suffix-p "package-info" curr)
                  ;; like Provider$1.class
                  (string-match-p "\\$[[:digit:]]" curr))
        (push
         (string-replace "/" "."
                         (string-replace "$" "." curr))
         result)))
    result))



;; Subroutines for working with modules

(defun javaimp--detect-module ()
  (let* ((file (expand-file-name
                (or buffer-file-name
		    (error "Buffer is not visiting a file!"))))
         (node (javaimp-tree-find-node
	        (lambda (m)
                  (seq-some (lambda (dir)
                              (string-prefix-p dir file))
                            (javaimp-module-source-dirs m)))
                javaimp-project-forest)))
    (when node
      (javaimp--update-module-maybe node)
      (javaimp-node-contents node))))

(defun javaimp--update-module-maybe (node)
  (let ((module (javaimp-node-contents node))
	need-update ids)
    ;; Check if deps are initialized
    (when (eq (javaimp-module-dep-jars module) t)
      (message "Will load dependencies for %s" (javaimp-module-id module))
      (setq need-update t))
    ;; Check if this or any parent build file has changed since we
    ;; loaded the module
    (let ((tmp node))
      (while tmp
	(let ((cur (javaimp-node-contents tmp)))
	  (when (and (not need-update)
                     (> (max (if (file-exists-p (javaimp-module-file cur))
                                 (float-time
                                  (javaimp--get-file-ts (javaimp-module-file cur)))
                               -1)
                             (if (file-exists-p (javaimp-module-file-orig cur))
                                 (float-time
                                  (javaimp--get-file-ts (javaimp-module-file-orig cur)))
                               -1))
		        (float-time (javaimp-module-load-ts module))))
	    (message "Will reload dependencies for %s because build file changed"
                     (javaimp-module-id cur))
	    (setq need-update t))
          (push (javaimp-module-id cur) ids))
	(setq tmp (javaimp-node-parent tmp))))
    (when need-update
      (setf (javaimp-module-dep-jars module)
            (funcall (javaimp-module-dep-jars-fetcher module) module ids))
      (setf (javaimp-module-load-ts module)
            (current-time)))
    (when (or need-update
              (eq (javaimp-module-dep-jars-with-source module) t))
      ;; Find out which of dep jars are also available as sources in
      ;; the current project
      (let* ((all (mapcar (lambda (m)
                            (cons (javaimp-module-artifact m) (javaimp-module-id m)))
                          (javaimp-collect-modules
                           (lambda (m)
                             (not (string-empty-p (javaimp-module-artifact m)))))))
             ;; FIXME we need elements from all as the result, is it
             ;; reliable to just put all first?
             (matches (seq-intersection
                       all
                       (javaimp-module-dep-jars module)
                       (lambda (el1 el2)
                         (string= (if (consp el1) (car el1) el1)
                                  (if (consp el2) (car el2) el2))))))
        (setf (javaimp-module-dep-jars-with-source module) matches)))))

(defun javaimp--collect-module-dep-jars-classes (module)
  "Return list of classes from MODULE's jar dependencies.  We
build the list each time because jars may change."
  (let ((dep-jars-no-source
         (seq-difference
          (javaimp-module-dep-jars module)
          (javaimp-module-dep-jars-with-source module)
          (lambda (el1 el2)
            (string= (if (consp el1) (car el1) el1)
                     (if (consp el2) (car el2) el2))))))
    (javaimp--collect-from-files
     #'javaimp--read-jar-classes
     dep-jars-no-source
     'javaimp--jar-idents-cache
     (concat (javaimp-print-id (javaimp-module-id module)) " dep jars"))))

(defun javaimp--collect-module-dep-jars-with-source-idents (scope-pred module)
  "Return list of identifiers satisfying SCOPE-PRED from MODULE's
dependencies for which we know where the source is.  The list is
cached by _artifact file_, so cache is refreshed only when
artifact is rebuilt."
  (javaimp--collect-from-files
   (lambda (artifact-and-id)
     (let* ((mod-id (cdr artifact-and-id))
            (mod (javaimp-find-module
                  (lambda (m)
                    (equal (javaimp-module-id m) mod-id)))))
       (if mod
           (javaimp--read-module-source-idents scope-pred mod)
         (error "Could not find module %s!  Please re-visit its \
top-level project." (javaimp-print-id mod-id)))))
   (javaimp-module-dep-jars-with-source module)
   'javaimp--module-idents-cache
   (concat (javaimp-print-id (javaimp-module-id module)) " dep sources")
   ;; We have "inner" loop inside the function passed to
   ;; javaimp--collect-from-files, so don't report progress on "outer"
   ;; loop
   t))

(defun javaimp--read-module-source-idents (scope-pred module)
  (let ((source-dirs
         (append
          (javaimp-module-source-dirs module)
          (mapcar (lambda (dir)
                    (file-name-as-directory
                     (file-name-concat (javaimp-module-build-dir module) dir)))
                  javaimp-additional-source-dirs))))
    (seq-mapcat (lambda (dir)
                  (javaimp--read-dir-source-idents
                   scope-pred dir
                   (concat (javaimp-print-id (javaimp-module-id module))
                           " source")))
                source-dirs)))



;; Some API functions.  They do not expose tree structure, return only
;; modules.

(defun javaimp-find-module (predicate)
  "Return first module in `javaimp-project-forest' for which
PREDICATE returns non-nil."
  (javaimp-tree-find-node predicate javaimp-project-forest t))

(defun javaimp-collect-modules (predicate)
  "Return all modules in `javaimp-project-forest' for which
PREDICATE returns non-nil."
  (javaimp-tree-collect-nodes predicate javaimp-project-forest))

(defun javaimp-map-modules (function)
  (javaimp-tree-map-nodes function #'always javaimp-project-forest))


;;; Adding imports

;;;###autoload
(defun javaimp-add-import (classname)
  "Import CLASSNAME in the current buffer and call `javaimp-organize-imports'.
Interactively, provide completion alternatives relevant for this
file, additionally filtering them by matching simple class
name (without package) against `symbol-at-point' (with prefix arg
- don't filter).

The set of relevant classes is collected from the following:

- If `javaimp-java-home' is set then add JDK classes, see
`javaimp--get-jdk-classes'.

- If current module can be determined, then add all classes from
its jar dependencies, as well as its source dependencies.

- Add classes in current module (if any) or source tree (see
`javaimp--get-current-source-dir')."
  (interactive
   (let* ((module (javaimp--detect-module))
          (scope-pred (javaimp-scope-defun-p))
          (classes
           (nconc
            ;; JDK
            (when javaimp-java-home
              (javaimp--get-jdk-classes javaimp-java-home))
            (when module
              (nconc
               ;; Jar dependencies
               (javaimp--collect-module-dep-jars-classes module)
               ;; Source dependencies
               (mapcar #'javaimp--ident-to-fqcn
                       (javaimp--collect-module-dep-jars-with-source-idents
                        scope-pred module))))
            ;; Current module or source tree
            (mapcar #'javaimp--ident-to-fqcn
                    (if module
                        (javaimp--read-module-source-idents scope-pred module)
                      (javaimp--read-dir-source-idents
                       scope-pred (javaimp--get-current-source-dir)
                       "current source")))))
          (completion-regexp-list
           (and (not current-prefix-arg)
                (symbol-at-point)
                (list (rx (and symbol-start
                               (literal (symbol-name (symbol-at-point)))
                               eol))))))
     (list (completing-read "Import: " classes nil t nil nil
                            (symbol-name (symbol-at-point))))))
  (javaimp-organize-imports (list (cons classname 'normal))))

(defun javaimp--get-jdk-classes (java-home)
  "If \"jmods\" subdirectory exists in JAVA-HOME (Java 9+), read
all .jmod files in it.  Else, if \"jre/lib\" subdirectory exists
in JAVA-HOME (earlier Java versions), read all .jar files in it."
  (let ((dir (file-name-concat java-home "jmods")))
    (if (file-directory-p dir)
        (javaimp--collect-from-files
         #'javaimp--read-jar-classes (directory-files dir t "\\.jmod\\'")
         'javaimp--jar-idents-cache "jdk .jmod")
      (setq dir (file-name-concat java-home "jre" "lib"))
      (if (file-directory-p dir)
          (javaimp--collect-from-files
           #'javaimp--read-jar-classes (directory-files dir t "\\.jar\\'")
           'javaimp--jar-idents-cache "jdk .jar")
        (user-error "Could not load JDK classes")))))



;; Organizing imports

;;;###autoload
(defun javaimp-organize-imports (&optional add-alist)
  "Group import statements according to the value of
`javaimp-import-group-alist' (which see) and print resulting
groups putting one blank line between groups.

If buffer already contains some import statements, put imports at
that same place.  Else, if there's a package directive, put
imports below it, separated by one line.  Else, just put them at
bob.

Classes within a single group are sorted lexicographically.
Imports not matched by any regexp in `javaimp-import-group-alist'
are assigned a default order defined by
`javaimp-import-default-order'.  Duplicate imports are elided.

Additionally, merge imports from ADD-ALIST, an alist of the same
form as CLASS-ALIST in return value of
`javaimp-parse-get-imports'."
  (interactive)
  (barf-if-buffer-read-only)
  (save-excursion
    (save-restriction
      (widen)
      (let ((parsed (javaimp-parse-get-imports)))
        (when (or (cdr parsed) add-alist)
          (javaimp-parse-without-hook
            (javaimp--position-for-insert-imports (car parsed))
            (let ((with-order
		   (mapcar
		    (lambda (import)
		      (let ((order
                             (or (assoc-default (car import)
                                                javaimp-import-group-alist
					        'string-match)
			         javaimp-import-default-order)))
		        (cons import order)))
                    (delete-dups (append (cdr parsed) add-alist))))
                  by-type)
	      (setq with-order
		    (sort with-order
			  (lambda (first second)
			    ;; sort by order then name
			    (if (/= (cdr first) (cdr second))
                                (< (cdr first) (cdr second))
			      (string< (caar first) (caar second))))))
              (setq by-type (seq-group-by #'cdar with-order))
              (javaimp--insert-import-group
               (cdr (assq 'normal by-type)) "import %s;\n")
              (javaimp--insert-import-group
               (cdr (assq 'static by-type)) "import static %s;\n"))
            ;; Make sure there's only one blank line after
            (forward-line -2)
            (delete-blank-lines)
            (end-of-line)
            (insert ?\n)))))))

(defun javaimp--position-for-insert-imports (old-region)
  (if old-region
      (progn
        (delete-region (car old-region) (cdr old-region))
        (goto-char (car old-region)))
    (if (javaimp-parse-get-package)
        (insert "\n\n")
      ;; As a last resort, go to bob and skip comments
      (goto-char (point-min))
      (forward-comment (buffer-size))
      (skip-chars-backward " \t\n")
      (unless (bobp)
        (insert "\n\n")))))

(defun javaimp--insert-import-group (imports fmt)
  (let (prev-order)
    (dolist (import imports)
      ;; If adjacent imports have different order value, insert a
      ;; newline between them
      (and prev-order
	   (/= (cdr import) prev-order)
	   (insert ?\n))
      (insert (format fmt (caar import)))
      (setq prev-order (cdr import)))
    (when imports
      (insert ?\n))))


;; Imenu support

(defsubst javaimp-imenu--make-entry (scope)
  (list (javaimp-scope-name scope)
        (if imenu-use-markers
            (copy-marker (javaimp-scope-open-brace scope))
          (javaimp-scope-open-brace scope))
        #'javaimp-imenu--function
        scope))


;;;###autoload
(defun javaimp-imenu-create-index ()
  "Function to use as `imenu-create-index-function'.
How to show the index is determined by
`javaimp-imenu-use-sub-alists', which see."
  (if javaimp-imenu-use-sub-alists
      (javaimp-imenu--create-index-nested)
    (javaimp-imenu--create-index-flat)))

(defun javaimp-imenu--create-index-nested ()
  "Build nested index for `javaimp-imenu-create-index'.  Scopes
nested in methods are not included, see
`javaimp-imenu-use-sub-alists' for explanation."
  (let ((forest (javaimp-imenu--get-forest
                 (javaimp-scope-defun-p 'method))))
    (javaimp-tree-map-nodes
     (lambda (scope)
       (if (eq (javaimp-scope-type scope) 'method)
           ;; Leaf entry for method
           (cons nil (javaimp-imenu--make-entry scope))
         ;; Sub-alist for container defuns - classes etc.
         (cons t (javaimp-scope-name scope))))
     (lambda (res)
       (or (functionp (nth 2 res))      ; leaf imenu entry
           (cdr res)))                  ; non-empty sub-alist
     forest)))

(defun javaimp-imenu--create-index-flat ()
  "Build flat index for `javaimp-imenu-create-index'."
  (let* ((forest (javaimp-imenu--get-forest
                  (javaimp-scope-defun-p t)))
         (entries
          (mapcar #'javaimp-imenu--make-entry
                  (seq-sort-by #'javaimp-scope-start #'<
                               (javaimp-tree-collect-nodes #'always forest))))
         alist)
    (mapc (lambda (entry)
            (setf (alist-get (car entry) alist 0 nil #'equal)
                  (1+ (alist-get (car entry) alist 0 nil #'equal))))
          entries)
    ;; Append parents to equal names to disambiguate them
    (mapc (lambda (entry)
            (when (> (alist-get (car entry) alist 0 nil #'equal) 1)
              (setcar entry
                      (format "%s [%s]"
                              (car entry)
                              (javaimp-scope-concat-parents
                               (nth 3 entry))))))
          entries)))

(defun javaimp-imenu--get-forest (scope-pred)
  "Build forest for imenu from scopes matching SCOPE-PRED."
  (let* ((scopes (javaimp-parse-get-all-scopes nil nil scope-pred))
         ;; Note that as these are abstract methods, open-brace is nil
         ;; for them.  Represent them as scopes for convenience.
         (abstract-methods
          (mapcan
           #'javaimp-parse-get-abstract-methods
           (seq-filter
            (lambda (s)
              (or (eq (javaimp-scope-type s) 'interface)
                  (plist-get (javaimp-scope-attrs s) 'abstract)))
            scopes))))
    (mapcar
     (lambda (top-defun)
       (when javaimp-verbose
         (message "Building tree for top-level %s %s"
                  (javaimp-scope-type top-defun)
                  (javaimp-scope-name top-defun)))
       (javaimp-tree-build top-defun
                           (append scopes abstract-methods)
                           (lambda (el tested)
                             (eq el (javaimp-scope-parent tested)))
                           nil
                           (lambda (s1 s2)
                             (< (javaimp-scope-start s1)
                                (javaimp-scope-start s2)))))
     (seq-filter (lambda (s)
                   (not (javaimp-scope-parent s)))
                 scopes))))

(defun javaimp-imenu--function (_index-name index-position scope)
  (if-let* ((decl-beg (javaimp--beg-of-defun-decl index-position)))
      (goto-char decl-beg)
    (goto-char (javaimp-scope-start scope))
    (back-to-indentation)))



;; Xref support

(defun javaimp-xref--backend () 'javaimp)

(defun javaimp-xref--ident-completion-table ()
  ;; FIXME: Include local classes and abstract methods
  (let ((scope-pred (javaimp-scope-defun-p 'method))
        (module (javaimp--detect-module)))
    (if module
        (nconc
         (javaimp--collect-module-dep-jars-with-source-idents scope-pred module)
         (javaimp--read-module-source-idents scope-pred module))
      (javaimp--read-dir-source-idents
       scope-pred (javaimp--get-current-source-dir) "current source"))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql 'javaimp)))
  (javaimp-xref--ident-completion-table))

(defun javaimp-xref--ident-definition (ident)
  (let* ((file (get-text-property 0 'file ident))
         (buf (get-file-buffer file))
         (loc (if buf
                  (xref-make-buffer-location
                   buf (get-text-property 0 'pos ident))
                (xref-make-file-location
                 file
                 (get-text-property 0 'line ident)
                 (get-text-property 0 'column ident)))))
    (xref-make ident loc)))

(cl-defmethod xref-backend-definitions ((_backend (eql 'javaimp)) identifier)
  (let* ((comp-table (javaimp-xref--ident-completion-table))
         (identifiers (all-completions identifier comp-table)))
    (mapcar #'javaimp-xref--ident-definition identifiers)))

(cl-defmethod xref-backend-apropos ((_backend (eql 'javaimp)) pattern)
  (let* ((comp-table (javaimp-xref--ident-completion-table))
         (identifiers (seq-filter
                       (apply-partially #'string-match
                                        (xref-apropos-regexp pattern))
                       comp-table)))
    (mapcar #'javaimp-xref--ident-definition
            (sort identifiers #'string-lessp))))



;; Show scopes

(defvar javaimp-show-scopes-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-m" #'javaimp-show-scopes-goto-scope)
    (define-key map [mouse-2] #'javaimp-show-scopes-goto-scope)
    (define-key map "g" #'revert-buffer)
    (define-key map "n" #'next-error-no-select)
    (define-key map "p" #'previous-error-no-select)
    (define-key map "l" #'recenter-current-error)
    map)
  "Javaimp Show Scopes keymap.")

(defun javaimp-show-scopes-goto-scope (event &optional to-start)
  "Go to the opening brace (`javaimp-scope-open-brace') of the scope.
Target scope is determined by location of mouse EVENT, if it's
non-nil.  Else, take the scope at current line.  When TO-START is
non-nil, go to scope start (`javaimp-scope-start') instead of the
opening brace."
  (interactive (list last-nonmenu-event current-prefix-arg))
  (let* ((buf (current-buffer))
         (scopes-buf (if event
                         (window-buffer (posn-window (event-end event)))
                       (current-buffer)))
         (scopes-pos (if event
                         (posn-point (event-end event))
                       (point)))
         (markers (or (with-current-buffer scopes-buf
                        (get-text-property scopes-pos 'javaimp-show-scopes-markers))
                      (user-error "No scope on this line")))
         (marker (if to-start
                     (cadr markers)
                   (car markers))))
    (unless (buffer-live-p (marker-buffer marker))
      (user-error "Buffer for this scope was killed"))
    (pop-to-buffer (marker-buffer marker))
    (goto-char marker)
    (next-error-found buf (current-buffer))))

(put 'javaimp-show-scopes-mode 'mode-class 'special)
(define-derived-mode javaimp-show-scopes-mode special-mode "Javaimp Show Scopes"
  (setq-local revert-buffer-function #'javaimp-show-scopes-revert)
  (setq next-error-function #'javaimp-show-scopes-next-error))

(defun javaimp-show-scopes ()
  "Show scopes in *javaimp-scopes* buffer."
  (interactive)
  (display-buffer
   (javaimp-show-scopes-1 (current-buffer))))

(defun javaimp-show-scopes-revert (_ignore1 _ignore2)
  "Function to be used as `revert-buffer-function' in
`javaimp-show-scopes-mode' buffers."
  (let ((source-buf
         (get-file-buffer
          (get-text-property (point-min) 'javaimp-show-scopes-file))))
    (if source-buf
        (javaimp-show-scopes-1 source-buf)
      (user-error "Source buffer has been killed"))))

(defun javaimp-show-scopes-1 (source-buf)
  "Subroutine of `javaimp-show-scopes', outputs scopes from
SOURCE-BUF in *javaimp-scopes* buffer.  Returns resulting
buffer."
  (let ((scopes
         (with-current-buffer source-buf
           (save-excursion
             (save-restriction
               (widen)
               (javaimp-parse-get-all-scopes
                nil nil (javaimp-scope-defun-p t))))))
        (default-dir
         (with-current-buffer source-buf
           default-directory))
        (buf (get-buffer-create "*javaimp-scopes*")))
    (with-current-buffer buf
      (setq default-directory default-dir)
      (javaimp-show-scopes-mode)
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (erase-buffer)
        (insert (propertize (format "%s" (buffer-file-name source-buf))
                            'javaimp-show-scopes-file (buffer-file-name source-buf))
                "\n\n")
        (dolist (scope scopes)
          (let ((depth 0)
                (tmp scope)
                (markers
                 (list (set-marker
                        (make-marker) (javaimp-scope-open-brace scope) source-buf)
                       (set-marker
                        (make-marker) (javaimp-scope-start scope) source-buf))))
            (while (setq tmp (javaimp-scope-parent tmp))
              (setq depth (1+ depth)))
            (insert (propertize
                     (format "%5d %2d %s: %s%s"
                             (with-current-buffer source-buf
                               (line-number-at-pos (javaimp-scope-start scope)))
                             depth
                             (substring (symbol-name (javaimp-scope-type scope))
                                        0 2)
                             (make-string (* 2 depth) ? )
                             (javaimp-scope-name scope))
                     'mouse-face 'highlight
                     'help-echo "mouse-2: go to this scope"
                     'follow-link t
                     'javaimp-show-scopes-markers markers)
                    ?\n)))
        (insert (format "\nTotal: %d scopes\n" (length scopes)))
        (goto-char (point-min))
        (setq next-error-last-buffer buf)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)))
    buf))

(defun javaimp-show-scopes-next-error (&optional argp reset)
  "Compatibility function for \\[next-error] invocations after
`javaimp-show-scopes'."
  (interactive "p")
  (or argp (setq argp 1))
  (when reset
    (if-let* ((pos (next-single-property-change
                    (point-min) 'javaimp-show-scopes-markers)))
        (progn
          (goto-char pos)
          (forward-line -1))
      (user-error "No scopes")))
  (forward-line argp)
  (unless (get-text-property (point) 'javaimp-show-scopes-markers)
    (user-error "No more scopes"))
  ;; In case the buffer is visible in a nonselected window.
  (when-let* ((win (get-buffer-window (current-buffer) t)))
    (set-window-point win (point)))
  (javaimp-show-scopes-goto-scope nil))


;; Navigation

(defun javaimp-beginning-of-defun (arg)
  "Function to be used as `beginning-of-defun-function'."
  (if (zerop arg)
      t
    (let* ((ctx (javaimp--get-sibling-context))
           (parent-start (nth 0 ctx))
           (offset-from-prev (if (> arg 0)
                                 (1- arg) ;prev counts for 1
                               arg))
           (target-idx (- (nth 1 ctx) offset-from-prev))
           (siblings (nthcdr 2 ctx)))
      (cond ((and (>= target-idx 0)
                  (< target-idx (length siblings)))
             ;; Move to target sibling
             (let* ((scope (nth target-idx siblings))
                    (pos (javaimp-scope-open-brace scope)))
               (goto-char (or (javaimp--beg-of-defun-decl pos parent-start)
                              pos))))
            (siblings
             ;; Move to start of first/last sibling
             (let* ((scope (car (if (< target-idx 0)
                                    siblings
                                  (last siblings))))
                    (pos (javaimp-scope-open-brace scope)))
               (goto-char (or (javaimp--beg-of-defun-decl pos) pos))))
            (parent-start
             (goto-char parent-start)
             (let ((parent-end (ignore-errors
                                 (scan-lists parent-start 1 0))))
               (if (and parent-end
                        (= (line-number-at-pos parent-start)
                           (line-number-at-pos parent-end)))
                   ;; open / close braces are on the same line
                   (forward-char)
                 (forward-line))))
            (t
             ;; There're no siblings and no parent
             (goto-char (if (< target-idx 0)
                            (point-min) (point-max))))))))

(defun javaimp--beg-of-defun-decl (pos &optional bound)
  "Assuming POS is somewhere inside the defun declaration, return
the beginning of that declaration.  Don't go farther backwards
than BOUND.  POS should not be in arglist or similar list."
  (save-excursion
    (save-restriction
      (widen)
      (javaimp-parse-get-defun-decl-start pos bound))))

(defun javaimp-end-of-defun ()
  "Function to be used as `end-of-defun-function'."
  ;; This function is called after javaimp-beginning-of-defun, which
  ;; in the normal course will position the point before the
  ;; open-brace, so we can inspect property.
  (when-let* ((brace-pos
               (next-single-property-change (point) 'javaimp-parse-scope))
              ((get-text-property brace-pos 'javaimp-parse-scope))
              ;; When there're no siblings, javaimp-beginning-of-defun
              ;; moves to the parent start.  In this case we should
              ;; stay inside the parent.
              ((eql (nth 1 (syntax-ppss))
                    (save-excursion
                      (nth 1 (syntax-ppss brace-pos))))))
    (ignore-errors
      (goto-char
       (scan-lists brace-pos 1 0)))))

(defun javaimp--get-sibling-context ()
  "Return list of the form (PARENT-START PREV-INDEX .
 SIBLINGS), where SIBLINGS is a list of all sibling defun scopes.
PREV-INDEX is the index of the \"previous\" (relative to point)
scope in this list, or -1.  PARENT-START is the position of
beginning (as in `javaimp-scope-open-brace') of parent defun, if
any.

Both when we're inside a method and between methods, the parent
is the method's enclosing class.  When we're inside the method,
PREV-INDEX gives the index of the method itself."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((pos (point))
             (defun-pred (javaimp-scope-defun-p t))
             (enc (javaimp-parse-get-enclosing-scope defun-pred))
             (parent
              (if (and enc (eq (javaimp-scope-type enc) 'method))
                  ;; We're inside a method, and need to look at
                  ;; sibling defuns within same parent (it's ok for
                  ;; parent to be nil)
                  (javaimp-scope-parent enc)
                ;; We're either inside a type (but not within its
                ;; nested defuns), or just at top-level.  Look at
                ;; defuns whose parent is enc.
                enc))
             (parent-beg (and parent (javaimp-scope-open-brace parent)))
             (sibling-pred (javaimp-scope-same-parent-p parent))
             (siblings
              (javaimp-parse-get-all-scopes
               ;; beg/end are not strictly needed, pred is enough, but
               ;; provide them for effectiveness
               parent-beg (when parent-beg
                            (ignore-errors
                              (scan-lists parent-beg 1 0)))
               (lambda (s)
                 (and (funcall defun-pred s)
                      (funcall sibling-pred s)))))
             (prev (javaimp--get-prev-scope pos enc parent-beg siblings)))
        (nconc
         (list
          (and parent (javaimp-scope-open-brace parent))
          (or (and prev
                   (seq-position siblings prev
                                 (lambda (s1 s2)
                                   (= (javaimp-scope-open-brace s1)
                                      (javaimp-scope-open-brace s2)))))
              -1))
         siblings)))))

(defun javaimp--get-prev-scope (pos enc parent-beg siblings)
  "Subroutine of `javaimp--get-sibling-context'."
  ;; Note: when looking for prev/next sibling, it might be tempting to
  ;; directly look at prev/next property change, but this would be
  ;; correct only by accident - there might be any scopes in different
  ;; nests in between.
  (if (and enc (eq (javaimp-scope-type enc) 'method))
      enc
    (if-let* ((next (seq-find
                     (lambda (s)
                       (>= (javaimp-scope-open-brace s) pos))
                     siblings))
              (next-beg-decl
               (javaimp--beg-of-defun-decl
                (javaimp-scope-open-brace next) parent-beg))
              (beg-decl
               (let ((tmp pos))
                 ;; pos may be inside arg list or some other nested
                 ;; construct, move out
                 (while (and tmp
                             ;; Note that case when both are nil is
                             ;; also correct: there's no parent and
                             ;; we're outside of any scope.
                             (not (eql parent-beg (nth 1 (syntax-ppss tmp)))))
                   (setq tmp (nth 1 (syntax-ppss tmp))))
                 (when tmp
                   (javaimp--beg-of-defun-decl tmp parent-beg))))
              ((= next-beg-decl beg-decl)))
        ;; If we're inside next's declaration - behave as if we were
        ;; inside its body, so it becomes our prev
        next
      ;; Just find previous defun
      (seq-find (lambda (s)
                  (< (javaimp-scope-open-brace s) pos))
                (reverse siblings)))))

(defun javaimp-jump-to-enclosing-scope ()
  "Jump to enclosing scope at point."
  (interactive)
  (if-let* ((scope (save-excursion
                     (save-restriction
                       (widen)
                       (javaimp-parse-get-enclosing-scope #'always t)))))
      (progn
        (goto-char (or (and (javaimp-scope-type scope)
                            (not (memq (javaimp-scope-type scope)
                                       '(array-init simple-statement statement)))
                            (javaimp--beg-of-defun-decl
                             (javaimp-scope-open-brace scope)))
                       (javaimp-scope-start scope)))
        (message "%s %s at position %d"
                 (javaimp-scope-type scope) (javaimp-scope-name scope)
                 (javaimp-scope-start scope)))
    (user-error "There is no enclosing scope at point")))


(defun javaimp-add-log-current-defun ()
  "Function to be used as `add-log-current-defun-function'."
  (save-excursion
    (save-restriction
      (widen)
      (let ((s (javaimp-parse-get-enclosing-scope
                (javaimp-scope-defun-p 'method)))
            names)
        (while s
          (push (javaimp-scope-name s) names)
          (setq s (javaimp-scope-parent s)))
        ;; Omit top-level class name if there're other components,
        ;; but only if it matches file name (it usually will).
        (when (and (> (length names) 1)
                   buffer-file-name
                   (equal (car names) (file-name-sans-extension
                                       (file-name-nondirectory buffer-file-name))))
          (setq names (cdr names)))
        (when names
          (string-join names "."))))))


;; Main

(defvar-keymap javaimp-basic-map
  "e" #'javaimp-jump-to-enclosing-scope
  "i" #'javaimp-add-import
  "o" #'javaimp-organize-imports
  "s" #'javaimp-show-scopes)

(defvar-keymap javaimp-minor-mode-map
  "C-c j" javaimp-basic-map
  ;; Override functions from java-mode
  "C-M-a" #'beginning-of-defun
  "C-M-e" #'end-of-defun
  "C-M-h" #'mark-defun)


;;;###autoload
(define-minor-mode javaimp-minor-mode
  "Javaimp minor mode.
When enabled, provides Imenu support, Xref support and navigation
functions using Javaimp facilities.

Set `narrow-to-defun-include-comments' to non-nil if you want
defun javadoc to be included in the narrowed region when using
\\[narrow-to-defun].

\\{javaimp-minor-mode-map}"
  :lighter " JavaImp"
  :interactive (java-mode)
  (if javaimp-minor-mode
      (progn
        (add-function :override (local 'imenu-create-index-function)
                      #'javaimp-imenu-create-index)
        (add-function :override (local 'beginning-of-defun-function)
                      #'javaimp-beginning-of-defun)
        (add-function :override (local 'end-of-defun-function)
                      #'javaimp-end-of-defun)
        (add-function :override (local 'add-log-current-defun-function)
                      #'javaimp-add-log-current-defun)
        (add-hook 'after-change-functions #'javaimp-parse--update-dirty-pos nil t)
        (add-hook 'xref-backend-functions #'javaimp-xref--backend nil t)
        (setq-local parse-sexp-ignore-comments t)
        (setq-local multibyte-syntax-as-symbol t)
        (setq-local end-of-defun-moves-to-eol nil)
        ;; Discard parse state, if any
        (setq javaimp-parse--dirty-pos nil)
        (setq syntax-ppss-table java-mode-syntax-table)
        ;; There're spaces within generic types, just show them
        (setq-local imenu-space-replacement nil))
    (remove-function (local 'imenu-create-index-function)
                     #'javaimp-imenu-create-index)
    (remove-function (local 'beginning-of-defun-function)
                     #'javaimp-beginning-of-defun)
    (remove-function (local 'end-of-defun-function)
                     #'javaimp-end-of-defun)
    (remove-function (local 'add-log-current-defun-function)
                     #'javaimp-add-log-current-defun)
    (remove-hook 'after-change-functions #'javaimp-parse--update-dirty-pos t)
    (remove-hook 'xref-backend-functions #'javaimp-xref--backend t)
    (kill-local-variable 'parse-sexp-ignore-comments)
    (kill-local-variable 'multibyte-syntax-as-symbol)
    (kill-local-variable 'end-of-defun-moves-to-eol)
    (kill-local-variable 'imenu-space-replacement)))


;;;###autoload
(defun javaimp-visit-project (file)
  "Loads a project and its submodules from FILE.
FILE should have a handler as per `javaimp-handler-regexp-alist'.
Interactively, finds suitable files in this directory and parent
directories, and offers them as completion alternatives for FILE,
topmost first.

After being processed by this command, the module tree becomes
known to javaimp and `javaimp-add-import' may be called inside
any module's source file."
  (interactive
   (let ((file-regexp (mapconcat #'car javaimp-handler-regexp-alist "\\|"))
         (cur-dir (expand-file-name default-directory))
         files parent)
     (while (setq files (append (directory-files cur-dir t file-regexp) files)
                  ;; Prevent infloop on root
                  parent (file-name-directory (directory-file-name cur-dir))
                  cur-dir (unless (string= parent cur-dir) parent)))
     (list (read-file-name "Visit project from file: " nil files t))))
  (setq file (expand-file-name file))
  (let ((handler (or (assoc-default (file-name-nondirectory file)
                                    javaimp-handler-regexp-alist
                                    #'string-match)
                     (user-error "No handler for file: %s" file))))
    ;; Forget previous tree(s) loaded from this build file, if any.
    ;; Additional project trees (see below) have the same file-orig,
    ;; so there may be several here.
    (when-let* ((existing-list
                 (seq-filter (lambda (node)
                               (equal (javaimp-module-file-orig
                                       (javaimp-node-contents node))
	                              file))
                             javaimp-project-forest)))
      (if (y-or-n-p "Forget already loaded project(s)?")
          (setq javaimp-project-forest
                (seq-remove (lambda (node)
                              (memq node existing-list))
                            javaimp-project-forest))
        (user-error "Aborted")))
    (let ((trees (funcall handler file)))
      (push (car trees) javaimp-project-forest)
      (dolist (node (cdr trees))
        (when (y-or-n-p
               (format "Include additional project tree rooted at %S?"
                       (javaimp-module-id (javaimp-node-contents node))))
          (push node javaimp-project-forest)))
      (message "Loaded project from %s" file))))


(defun javaimp-forget-visited-projects ()
  "Forget all visited projects."
  (interactive)
  (setq javaimp-project-forest nil))

(defun javaimp-flush-cache ()
  "Flush all caches."
  (interactive)
  (setq javaimp--jar-idents-cache nil
        javaimp--module-idents-cache nil
        javaimp--source-idents-cache nil))

(provide 'javaimp)

;;; javaimp.el ends here
