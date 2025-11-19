;;; zuul.el --- Interface to Zuul -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; Maintainer: zuul.el Development <~niklaseklund/zuul.el@lists.sr.ht>
;; URL: https://sr.ht/~niklaseklund/zuul.el
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1") (project "0.8.1"))
;; Keywords: convenience tools

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Browse Zuul build logs from Emacs.  The `zuul' package provides
;; functionality to communicate with Zuul through its REST API [1].

;; The `zuul-log-mode' makes it smooth to browse errors in the logs
;; and offers a custom filename parser to make sure Emacs finds local
;; files even though the absolute paths differs from the remote build
;; machine.

;; [1] https://zuul-ci.org/docs/zuul/latest/rest-api.html

;;; Code:

;;;; Requirements

(require 'ansi-color)
(require 'comint)
(require 'compile)
(require 'project)
(eval-when-compile (require 'rx))
(require 'subr-x)
(require 'text-property-search)
(require 'url)

(declare-function json-parse-buffer "json.c" (&rest args))

;;;; Variables

(defcustom zuul-base-url ""
  "Base URL to Zuul."
  :group 'zuul
  :type 'string)

(defcustom zuul-tenant ""
  "Zuul tenant."
  :group 'zuul
  :type 'string)

(defcustom zuul-tenant-configs nil
  "A list of tenant configurations.

Each configuration is a property list with the following properties:
NAME: Name of the tenant
PROJECT-ROOTS: An alist of (name . root)"
  :group 'zuul
  :type '(repeat (plist :options ((:name string)
                                   (:project-roots
                                     (alist :key-type string
                                       :value-type string))))))

(defcustom zuul-build-annotation
  '((:name build :function zuul--build-name-str)
     (:name status :function zuul--build-status-str)
     (:name pipeline :function zuul--build-pipeline-str :face zuul-pipeline-face)
     (:name duration :function zuul--build-duration-str :align right :face zuul-duration-face)
     (:name start-time :function zuul--build-start-time-str :face zuul-time-face))
  "A list of annotations to display for a build.

Each entry in the list is a property list with the following properties:
- :name
- :function
- :align
- :face"
  :group 'zuul
  :type '(repeat (plist :options ((:name symbol)
                                   (:function symbol)
                                   (:align symbol)
                                   (:face symbol)))))

(defcustom zuul-buildset-annotation
  '((:name patchset :function zuul--buildset-patchset-str)
     (:name status :function zuul--buildset-status-str)
     (:name duration :function zuul--buildset-duration-str :face zuul-duration-face)
     (:name summary :function zuul--buildset-summary-str :face zuul-buildset-summary-face)
     (:name start-time :function zuul--buildset-start-time-str :face zuul-time-face))
  "A list of annotations to display for a buildset.

Each entry in the list is a property list with the following properties:
- :name
- :function
- :align
- :face"
  :group 'zuul
  :type '(repeat (plist :options ((:name symbol)
                                   (:function symbol)
                                   (:align symbol)
                                   (:face symbol)))))

(defcustom zuul-build-command-annotation
  '((:name command :function zuul--data-host-cmd-str :width 50)
     (:name task-name :function zuul--data-task-name-str :face zuul-unknown-face)
     (:name host-result :function zuul--data-host-result-str)
     (:name task-duration :function zuul--data-task-duration-str :align right :face zuul-duration-face)
     (:name phase :function zuul--data-playbook-phase-str :face zuul-playbook-phase-face)
     (:name task-role :function zuul--data-task-role-str :face zuul-task-role-face)
     (:name host :function zuul--data-host-name-str :face zuul-host-face))
  "A list of annotations to display for a build command.

Each entry in the list is a property list with the following properties:
- :name
- :function
- :align
- :face"
  :group 'zuul
  :type '(repeat (plist :options ((:name symbol)
                                   (:function symbol)
                                   (:align symbol)
                                   (:face symbol)))))

(defcustom zuul-build-imenu-annotation
  '((:name task-name :function zuul--data-task-name-str)
     (:name host-result :function zuul--data-host-result-str)
     (:name task-duration :function zuul--data-task-duration-str :align right :face zuul-duration-face)
     (:name phase :function zuul--data-playbook-phase-str :face zuul-playbook-phase-face)
     (:name task-role :function zuul--data-task-role-str :face zuul-task-role-face)
     (:name host :function zuul--data-host-name-str :face zuul-host-face))
  "A list of annotations to display for `imenu'.

Each entry in the list is a property list with the following properties:
- :name
- :function
- :align
- :face"
  :group 'zuul
  :type '(repeat (plist :options ((:name symbol)
                                   (:function symbol)
                                   (:align symbol)
                                   (:face symbol)))))

(defcustom zuul-build-display-buffer-action '(display-buffer-same-window
                                               (inhibit-same-window . nil))
  "The configuration for `display-buffer' when opening a build."
  :group 'zuul
  :type 'sexp)

(defcustom zuul-add-builds-to-buildset t
  "If set to t builds will be added to buildsets."
  :group 'zuul
  :type 'boolean)

(defcustom zuul-sort-priority-functions
  `((lambda (it)
      (if-let ((start-time (zuul--start-time it)))
        (float-time (date-to-time start-time))
        0.0))
     (lambda (it)
       (pcase (zuul--status it)
         ("SUCCESS" 0)
         ("QUEUED" 1)
         ("FAILURE" 3)
         (_ 2)))
     (lambda (it)
       (pcase (let-alist (zuul-data it) .pipeline)
         ("check" 1)
         ("gate" 2)
         (_ 3))))
  "A list of functions to use to sort builds."
  :group 'zuul
  :type '(repeat sexp))

(defcustom zuul-status-face-function #'zuul--get-status-face
  "Function to map a status to face."
  :group 'zuul
  :type 'sexp)

;;;;; Private

(defconst zuul--response-buffer " *zuul-response*"
  "Name of the buffer which holds the response from Zuul.")

(defvar zuul--build-playbook-id nil "Identifier for playbook.")
(defvar zuul--build-play-id nil "Identifier for play.")
(defvar zuul--build-task-id nil "Identifier for task.")
(defvar zuul--build-data nil "Data of build.")
(defvar zuul--builds nil "List of builds.")
(defvar zuul--build nil "A build.")

(defvar-local zuul--current-build nil
  "A buffer local variable of the current build.")
(defvar-local zuul--current-builds nil
  "A buffer local variable of the current builds.")
(defvar-local zuul--project-files nil
  "A buffer local variable with all files in the current project.")

;;;; Faces

(defgroup zuul-faces nil
  "Faces used by `zuul'."
  :group 'zuul
  :group 'faces)

(defface zuul-unknown-face
  '((t :inherit font-lock-comment-face))
  "Face used to highlight duration in `zuul'.")

(defface zuul-duration-face
  '((t :inherit zuul-unknown-face))
  "Face used to highlight duration in `zuul'.")

(defface zuul-pipeline-face
  '((t :inherit font-lock-doc-face))
  "Face used to highlight pipeline in `zuul'.")

(defface zuul-buildset-summary-face
  '((t :inherit zuul-unknown-face))
  "Face used to highlight buildset summary in `zuul'.")

(defface zuul-time-face
  '((t :inherit zuul-unknown-face))
  "Face used to highlight time in `zuul'.")

(defface zuul-error-face
  '((t :inherit error))
  "Face used to highlight error in `zuul'.")

(defface zuul-success-face
  '((t :inherit success))
  "Face used to highlight success in `zuul'.")

(defface zuul-active-face
  '((t :inherit font-lock-string-face))
  "Face used to highlight active in `zuul'.")

(defface zuul-host-face
  '((t :inherit font-lock-string-face))
  "Face used to highlight host in `zuul'.")

(defface zuul-playbook-phase-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used to highlight playbook phase in `zuul'.")

(defface zuul-task-role-face
  '((t :inherit font-lock-doc-face))
  "Face used to highlight task role in `zuul'.")

(defface zuul-build-command-face
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight build command in `zuul'.")

(defface zuul-command-prompt-face
  '((t :inherit comint-highlight-prompt))
  "Face used to highlight command prompt in `zuul'.")

(defface zuul-prompt-input-face
  '((t :inherit comint-highlight-input))
  "Face used to highlight prompt input in `zuul'.")

;;;; Data structures

(cl-defstruct (zuul-buildset
                (:constructor zuul--buildset-create)
                (:conc-name zuul--buildset-))
  (data nil)
  (builds nil))

(cl-defstruct (zuul-build
                (:constructor zuul--build-create)
                (:conc-name zuul--build-))
  (data nil))

;;;; Functions

(cl-defgeneric zuul-build (entity)
  "Return build for ENTITY.")

(cl-defmethod zuul-build ((entities list))
  "Select and return a build from ENTITIES."
  (cond ((zuul-buildset-p (seq-first entities))
          (zuul-build (zuul--builds entities)))
    ((zuul-build-p (seq-first entities))
      (zuul--select-build entities))
    (t nil)))

(cl-defmethod zuul-build ((buildset zuul-buildset))
  "Select and return a build from BUILDSET."
  (zuul-build (zuul--builds buildset)))

(cl-defmethod zuul-build ((build zuul-build))
  "Return BUILD."
  build)

(cl-defgeneric zuul-data (entity)
  "Return data of ENTITY.")

(cl-defmethod zuul-data ((buildset zuul-buildset))
  "Return BUILDSET's data."
  (zuul--buildset-data buildset))

(cl-defmethod zuul-data ((build zuul-build))
  "Return BUILD's data."
  (zuul--build-data build))

(defun zuul-open-build-log (query)
  "Open a build log based on QUERY.

Query which can either be a list or a function returning a list.
The content of the list should be the query function to use as well as
the parameter to pass to it.  Examples of query functions are:
- `zuul-get-buildsets'
- `zuul-get-builds'"
  (when-let ((builds-or-buildsets
               (apply (if (functionp query)
                        (funcall query)
                        query))))
    (zuul--open-build-log builds-or-buildsets)))

(defun zuul--open-build-log (builds-or-buildsets)
  "Open a build log from an item in list BUILDS-OR-BUILDSETS."
  (when (or (and (listp builds-or-buildsets)
              (or (zuul-buildset-p (seq-first builds-or-buildsets))
                (zuul-build-p (seq-first builds-or-buildsets))))
          (or (zuul-buildset-p builds-or-buildsets)
            (zuul-build-p builds-or-buildsets)))
    (when-let* ((zuul--builds (or zuul--builds t))
                 (zuul--build (zuul-build builds-or-buildsets)))
      (let-alist (zuul-data zuul--build)
        (let* ((buffer (format "[%s/%s]-%s" .ref.change .ref.patchset .job_name))
                (project-root (zuul--project-root .project))
                (build-output
                  (zuul--get-build-output .uuid
                    :json t
                    :parser #'zuul--build-json-parser)))
          (if (not build-output)
            (message "Build has no output")
            (with-current-buffer (get-buffer-create buffer)
              (setq-local default-directory project-root)
              (erase-buffer)
              (insert build-output)
              (zuul-log-mode)
              (setq zuul--current-build zuul--build)
              (setq zuul--current-builds zuul--builds)
              (goto-char (point-min))
              (compilation-minor-mode t)
              (compilation--ensure-parse (point-max))
              (goto-char (point-max))
              (select-window
                (display-buffer buffer zuul-build-display-buffer-action)))))))))

(cl-defun zuul-get-builds (&key
                            change
                            project
                            patchset
                            branch
                            ref
                            (limit "10000"))
  "Return a list of `zuul-build' objects.

Optionally provide CHANGE, PROJECT, PATCHSET, BRANCH, REF and LIMIT."
  (let* ((params `(("limit" ,limit)
                    ,(and change `("change" ,change))
                    ,(and patchset `("patchset" ,patchset))
                    ,(and project `("project" ,project))
                    ,(and branch `("branch" ,branch))
                    ,(and ref `("ref" ,ref))))
          (response
            (zuul--rest-request
              (concat "/api/tenant/" zuul-tenant "/builds")
              :params (seq-remove #'null params))))
    (seq-map (lambda (it) (zuul--build-create :data it)) response)))

(cl-defun zuul-get-buildsets (&key
                               change
                               project
                               patchset
                               result
                               (limit "200"))
  "Return a list of `zuul-buildset' objects.

Optionally provide parameters CHANGE, PROJECT, PATCHSET, RESULT and LIMIT."
  (let* ((params `(("limit" ,limit)
                    ,(and change `("change" ,change))
                    ,(and patchset `("patchset" ,patchset))
                    ,(and result `("result" ,result))
                    ,(and project `("project" ,project))))
          (response
            (zuul--rest-request
              (concat "/api/tenant/" zuul-tenant "/buildsets")
              :params (seq-remove #'null params)
              :parser #'zuul--request-json-parser))
          (buildsets (seq-map (lambda (it) (zuul--buildset-create :data it)) response)))
    (if (and zuul-add-builds-to-buildset
          (not (seq-empty-p buildsets)))
      (zuul--add-builds-to-buildsets buildsets)
      buildsets)))

;;;; Commands

(defun zuul-switch-build ()
  "Switch to another build."
  (interactive)
  (zuul--open-build-log zuul--current-builds))

(defun zuul-switch-buildset ()
  "Switch to a build from a specific buildset."
  (interactive)
  (let ((zuul--builds zuul--current-builds))
    (zuul--open-build-log
      (zuul--buildsets zuul--current-build))))

(defun zuul-open-build-in-browser ()
  "Open build in browser."
  (interactive)
  (let-alist (zuul-data zuul--current-build)
    (browse-url
      (concat zuul-base-url
        "/t/" zuul-tenant
        "/build/" .uuid "/console"))))

(defun zuul-run-build-command ()
  "Run build command from build log."
  (interactive)
  (when-let ((command (zuul--build-log-command)))
    (compile command)))

(defun zuul-next-build ()
  "Switch to next build."
  (interactive)
  (let* ((zuul--build zuul--current-build)
          (zuul--builds zuul--current-builds)
          (builds-with-index (zuul--current-builds-with-index))
          (build-index (zuul--current-build-index builds-with-index))
          (next-index
            (mod
              (1+ build-index)
              (length builds-with-index)))
          (next-build
            (cdr (assoc next-index builds-with-index))))
    (zuul--open-build-log next-build)))

(defun zuul-previous-build ()
  "Switch to previous build."
  (interactive)
  (let* ((zuul--build zuul--current-build)
          (zuul--builds zuul--current-builds)
          (builds-with-index (zuul--current-builds-with-index))
          (build-index (zuul--current-build-index builds-with-index))
          (previous-index
            (mod
              (1- build-index)
              (length builds-with-index)))
          (previous-build
            (cdr (assoc previous-index builds-with-index))))
    (zuul--open-build-log previous-build)))

(defun zuul-quit-build ()
  "Kill buffers associated with build."
  (interactive)
  (let ((zuul--build zuul--current-build)
         (zuul-log-buffers
           (seq-filter (lambda (it)
                         (eq 'zuul-log-mode
                           (with-current-buffer it
                             major-mode)))
             (buffer-list))))
    (thread-last zuul-log-buffers
      (seq-filter (lambda (it)
                    (with-current-buffer it
                      (=
                        (let-alist (zuul-data zuul--current-build)
                          .change)
                        (let-alist (zuul-data zuul--build)
                          .change)))))
      (seq-do #'kill-buffer))))

(defun zuul-previous-command ()
  "Navigate to previous command."
  (interactive)
  (let ((re-prompt (rx "zuul@" (regexp ".*") "$ ")))
    (beginning-of-line)
    (when (re-search-backward re-prompt nil t)
      (goto-char (match-end 0)))))

(defun zuul-next-command ()
  "Navigate to next command."
  (interactive)
  (let ((re-prompt (rx "zuul@" (regexp ".*") "$ ")))
    (end-of-line)
    (when (re-search-forward re-prompt nil t)
      (goto-char (match-end 0)))))

;;;; Support functions

(defun zuul--tenant-config ()
  "Return TENANT configuration."
  (with-connection-local-variables
    (seq-find
      (lambda (it)
        (string= (plist-get it :name) zuul-tenant))
      zuul-tenant-configs)))

(defun zuul--project-root (project)
  "Return the path to the root of PROJECT."
  (if-let ((tenant-config (zuul--tenant-config))
            (project-root (cdr
                            (assoc project
                              (plist-get tenant-config :project-roots)))))
    (concat (file-remote-p default-directory) project-root)
    (message "Project root for %s wasn't found, falling back to `default-directory'" project)
    default-directory))

(cl-defun zuul--get-build-output (build-uuid &key
                                   json
                                   (parser #'buffer-string)
                                   (buffer zuul--response-buffer))
  "Return the build output for BUILD-UUID.

Optionally provide extra parameters JSON, PARSER or BUFFER."
  (let* ((build
           (zuul--rest-request
             (concat "/api/tenant/" zuul-tenant "/builds")
             :params `(("uuid" ,build-uuid)))))
    (if (seq-empty-p build)
      (message "Build with uuid=%s can't be found" build-uuid)
      (when-let ((log-url (alist-get 'log_url (seq-elt build 0))))
        (zuul--request
          (concat log-url "job-output" (if json ".json" ".txt"))
          :buffer buffer
          :parser parser)))))

(defun zuul--add-builds-to-buildsets (buildsets)
  "Add builds to BUILDSETS."
  (when-let* ((builds
                (let-alist (zuul-data (seq-first buildsets))
                  (zuul-get-builds :change .change
                    :project .project)))
               (patchset-builds
                 (seq-group-by (lambda (it)
                                 (let-alist (zuul-data it) .patchset))
                   builds)))
    (seq-map (lambda (it)
               (let-alist (zuul-data it)
                 (when-let ((builds (alist-get .patchset patchset-builds nil nil #'string=)))
                   (setf (zuul--buildset-builds it) builds)))
               it)
      buildsets)))

(cl-defun zuul--completing-read (candidates &key
                                  category
                                  prompt)
  "Select a candidate from CANDIDATES.

Optionally provide CATEGORY and PROMPT."
  (unless (seq-empty-p candidates)
    (when-let* ((metadata `(metadata
                             (category . ,category)
                             (cycle-sort-function . identity)
                             (display-sort-function . identity)))
                 (collection (lambda (string predicate action)
                               (if (eq action 'metadata)
                                 metadata
                                 (complete-with-action action candidates string predicate))))
                 (selected (completing-read prompt collection nil t)))
      (cdr (assoc selected candidates)))))

(defun zuul--get-status-face (status)
  "Return face to use for STATUS."
  (pcase status
    ("SUCCESS" 'zuul-success-face)
    ("FAILURE" 'zuul-error-face)
    ("QUEUED" 'zuul-unknown-face)
    (_ 'zuul-active-face)))

(defun zuul--sort-builds (builds)
  "Sort BUILDS."
  (seq-do (lambda (priority-fun)
            (setq builds (seq-sort-by priority-fun #'> builds)))
    zuul-sort-priority-functions)
  builds)

(defun zuul--locate-file (filename)
  "Locate FILENAME.

Build outputs can contain absolute file paths from a remote
machine.  This function tries to locate find the best matching project file
relative to the projects root."
  (or
    (thread-last zuul--project-files
      (seq-filter (lambda (it) (string-suffix-p it filename)))
      (seq-sort-by #'length #'>)
      (seq-first))
    filename))

(defun zuul--select-build (builds)
  "Select and return a build selected from BUILDS."
  (unless (seq-empty-p builds)
    (setq zuul--builds
      (zuul--sort-builds builds))
    (zuul--completing-read
      (zuul--candidate-annotations
        zuul--builds
        zuul-build-annotation)
      :category 'zuul-build
      :prompt "Select build: ")))

(defun zuul--select-buildset (buildsets)
  "Select and return a buildset from BUILDSETS."
  (unless (seq-empty-p buildsets)
    (zuul--completing-read
      (zuul--candidate-annotations
        buildsets zuul-buildset-annotation)
      :category 'zuul-buildset
      :prompt "Select buildset: ")))

(defun zuul--current-builds-with-index ()
  "Return current builds with index."
  (seq-map-indexed (lambda (it index) `(,index . ,it)) zuul--current-builds))

(defun zuul--current-build-index (builds-with-index)
  "Return index of current build in BUILDS-WITH-INDEX."
  (thread-last builds-with-index
    (seq-find (lambda (it) (zuul--build-equal-p (cdr it) zuul--current-build)) builds-with-index)
    (car)))

;;;;; Accessors

(cl-defgeneric zuul--buildsets (entity)
  "Return buildsets for ENTITY.")

(cl-defmethod zuul--buildsets ((build zuul-build))
  "Return buildsets which BUILD relates to."
  (let-alist (zuul-data build)
    (zuul-get-buildsets :change .change
      :project .project)))

(cl-defmethod zuul--buildsets ((buildset zuul-buildset))
  "Return all other buildsets which relates to BUILDSET."
  (let-alist (zuul-data buildset)
    (zuul-get-buildsets :change .change
      :project .project)))

(cl-defgeneric zuul--builds (entity)
  "Return builds for ENTITY.")

(cl-defmethod zuul--builds ((build zuul-build))
  "Return all builds from the same buildset as BUILD."
  (let-alist (zuul-data build)
    (zuul-get-builds :change .change
      :project .project
      :patchset .patchset)))

(cl-defmethod zuul--builds ((buildset zuul-buildset))
  "Return all builds from BUILDSET."
  (if-let ((builds (zuul--buildset-builds buildset)))
    builds
    (let-alist (zuul-data buildset)
      (zuul-get-builds :change .change
        :project .project
        :patchset .patchset))))

(cl-defmethod zuul--builds ((buildsets list))
  "Select a buildset from BUILDSETS and return all its builds."
  (zuul--builds
    (zuul--select-buildset buildsets)))

(cl-defgeneric zuul--status (entity)
  "Return the status of ENTITY.")

(cl-defmethod zuul--status ((buildset zuul-buildset))
  "Return the status of BUILDSET."
  (let-alist (zuul-data buildset)
    (if .result
      .result
      (if .first_build_start_time
        "RUNNING"
        "QUEUED"))))

(cl-defmethod zuul--status ((build zuul-build))
  "Return the status of BUILD."
  (let-alist (zuul-data build)
    (if .result
      .result
      (if .start_time
        "RUNNING"
        "QUEUED"))))

(cl-defgeneric zuul--start-time (entity)
  "Return the start time of ENTITY.")

(cl-defmethod zuul--start-time ((buildset zuul-buildset))
  "Return the start time of BUILDSET."
  (let-alist (zuul-data buildset)
    .first_build_start_time))

(cl-defmethod zuul--start-time ((build zuul-build))
  "Return the start time of BUILD."
  (let-alist (zuul-data build)
    .start_time))

(defun zuul--build-equal-p (build1 build2)
  "Return t if BUILD1 and BUILD2 are equal."
  (string= (let-alist (zuul-data build1) .uuid)
    (let-alist (zuul-data build2) .uuid)))

;;;;; Request

(cl-defun zuul--request (url &key
                          (parser #'zuul--request-json-parser)
                          (method "GET")
                          (buffer zuul--response-buffer)
                          (headers '(("Content-Type" . "application/json"))))
  "Perform a request to URL.

Optionally provide extra parameters PARSER, METHOD, BUFFER or HEADERS."
  (with-current-buffer (get-buffer-create buffer)
    (erase-buffer)
    (let ((url-request-method method)
           (url-request-extra-headers headers))
      (url-insert-file-contents url))
    (funcall parser)))

(cl-defun zuul--rest-request (endpoint &key
                               params
                               (parser #'zuul--request-json-parser)
                               (method "GET")
                               (buffer zuul--response-buffer)
                               (headers '(("Content-Type" . "application/json"))))
  "Perform a REST API request to ENDPOINT.

Optionally provide extra parameters PARAMS, PARSER, METHOD, BUFFER or HEADERS."
  (let ((url (concat zuul-base-url endpoint
               (when params
                 (format "?%s" (url-build-query-string params))))))
    (zuul--request
      url
      :parser parser
      :method method
      :buffer buffer
      :headers headers)))

;;;;; Parsers

(defun zuul--request-json-parser ()
  "Parse a request response with `json-parse-buffer'."
  (goto-char (point-min))

  ;; Avoid json-parse-error \u0000 is not allowed without JSON_ALLOW_NUL
  (let ((re (rx (group (regexp "\\\\u0000")) (group (zero-or-one "\"")))))
    (save-excursion
      (while (search-forward-regexp re nil t)
        (replace-match (concat "" (match-string 2))))))

  (json-parse-buffer :array-type 'array
    :object-type 'alist
    :null-object nil
    :false-object nil))

(defun zuul--request-debug-parser ()
  "Parser that show a pretty-printed result of the request."
  (json-pretty-print-buffer)
  (goto-char (point-min))
  (pop-to-buffer (current-buffer)))

(defun zuul--build-json-parser ()
  "Parse the output of a json build."
  (let ((build (zuul--request-json-parser)))
    (string-join
      (flatten-list
        (seq-map #'zuul--build-playbook-output build))
      "\n")))

(defun zuul--build-playbook-output (playbook)
  "Return the output from the PLAYBOOK."
  (let ((zuul--build-data))
    (let-alist playbook
      (let* ((zuul--build-playbook-id .playbook)
              (zuul--build-data `(,@zuul--build-data :playbook ,playbook)))
        (thread-last .plays
          (seq-map #'zuul--build-play-output)
          (seq-remove #'null))))))

(defun zuul--build-play-output (play)
  "Return the output from the PLAY."
  (let-alist play
    (let* ((zuul--build-play-id .play.id)
            (zuul--build-data `(,@zuul--build-data :play ,play)))
      (thread-last .tasks
        (seq-map #'zuul--build-task-output)
        (seq-remove #'null)))))

(defun zuul--build-task-output (task)
  "Return the output from the TASK."
  (let-alist task
    (let* ((zuul--build-task-id .task.id)
            (zuul--build-data `(,@zuul--build-data :task ,task)))
      (thread-last .hosts
        (seq-map #'zuul--build-host-output)
        (seq-remove #'null)))))

(defun zuul--build-host-output (host)
  "Return the command and its output from the HOST."
  (pcase-let* ((`(,hostname . ,data) host)
                (cmd (let-alist data .cmd))
                (output (let-alist data .stdout))
                (host-id (let-alist data .zuul_log_id))
                (zuul--build-data `(,@zuul--build-data :host ,host)))
    (when-let ((cmd-str
                 (when cmd
                   (format "zuul@%s$ %s"
                     hostname
                     (if (stringp cmd)
                       cmd
                       (string-join cmd " "))))))
      (zuul--propertize-face cmd-str 'bold-italic)
      (if (or (null output) (string-empty-p output))
        (setq output (concat cmd-str "\n"))
        (setq output (concat cmd-str "\n" output "\n")))
      (put-text-property 0 (length output) 'zuul-playbook zuul--build-playbook-id output)
      (put-text-property 0 (length output) 'zuul-play zuul--build-play-id output)
      (put-text-property 0 (length output) 'zuul-task zuul--build-task-id output)
      (put-text-property 0 (length output) 'zuul-host host-id output)
      (put-text-property 0 (length output) 'zuul-data zuul--build-data output)
      output)))

(defun zuul--build-log-command ()
  "Select a command from the build log."
  (let ((host-data)
         (prop))
    (save-excursion
      (goto-char (point-min))
      (while (setq prop (text-property-search-forward 'zuul-host))
        (let* ((text-properties (text-properties-at (prop-match-beginning prop)))
                (data (plist-get text-properties 'zuul-data)))
          (push data host-data))))
    (when-let ((selected
                 (zuul--completing-read
                   (zuul--candidate-annotations
                     host-data
                     zuul-build-command-annotation)
                   :category 'zuul-commands
                   :prompt "Select command: ")))
      (zuul--data-host-cmd-str selected))))

;;;;; Annotation functions

(defun zuul--candidate-annotations (candidates annotation-config)
  "Return annotated CANDIDATES according to ANNOTATION-CONFIG."
  (let* ((annotations
           (seq-map (lambda (candidate)
                      (cl-loop for config in annotation-config
                        collect `(,(plist-get config :name) .
                                   ,(funcall (plist-get config :function) candidate))))
             candidates))
          (annotation-widths
            (cl-loop for config in annotation-config
              collect
              `(,(plist-get config :name) .
                 ,(thread-last annotations
                    (seq-map (lambda (it) (length (alist-get (plist-get config :name) it))))
                    (funcall (lambda (it)
                               (if-let ((max-width (plist-get config :width)))
                                 (min (seq-max it) max-width)
                                 (seq-max it)))))))))
    (cl-mapcar (lambda (candidate annotation)
                 `(,(cl-loop for config in annotation-config
                      concat
                      (let* ((padding 3)
                              (str (alist-get (plist-get config :name) annotation))
                              (width (alist-get (plist-get config :name) annotation-widths))
                              (new-str
                                (if-let* ((align (plist-get config :align))
                                           (align-right (eq 'right align)))
                                  (concat (make-string (- width (length str)) ?\s)
                                    str (make-string padding ?\s))
                                  (concat
                                    (truncate-string-to-width str width 0 ?\s)
                                    (make-string padding ?\s)))))
                        (if-let ((face (plist-get config :face)))
                          (zuul--propertize-face new-str face)
                          new-str)))
                    . ,candidate))
      candidates annotations)))

(defun zuul--project-files ()
  "Return a list of project files, relative to project root."
  (let* ((project (project-current nil))
          (root (expand-file-name (project-root project)))
          (files (project-files project)))
    (seq-map (lambda (it) (string-remove-prefix root it)) files)))

(defun zuul--eldoc-function (_callback)
  "A member of `eldoc-documentation-functions', for signatures."
  (when-let* ((text-properties (text-properties-at (point)))
               (data (plist-get text-properties 'zuul-data)))
    (string-join
      `(,(format "%s playbook" (zuul--data-playbook-phase-str data))
         ,(zuul--data-playbook-name-str data)
         ,(format "Play: %s" (zuul--data-play-name-str data))
         ,(concat "Task: ["
            (when-let ((role-str (zuul--data-task-role-str data)))
              (unless (string-empty-p role-str)
                (concat role-str ": ")))
            (zuul--data-task-name-str data)
            "] "
            (string-trim (zuul--data-task-duration-str data)))
         ,(format "Host: %s" (zuul--data-host-name-str data)))
      " ")))

(defun zuul--imenu-index ()
  "Create an `imenu' index for the build log."
  (let ((property)
         (index)
         (annotations))
    (save-excursion
      (goto-char (point-min))
      (while (setq property (text-property-search-forward 'zuul-task))
        (let* ((text-properties (text-properties-at (prop-match-beginning property)))
                (data (plist-get text-properties 'zuul-data)))
          (push `(,data . ,(prop-match-beginning property))
            index))))
    (setq annotations
      (seq-map #'car
        (zuul--candidate-annotations
          (seq-map #'car index)
          zuul-build-imenu-annotation)))
    (cl-mapcar (lambda (annotation index-item)
                 (setf (car index-item) annotation))
      annotations index)
    index))

;;;;; String representations

(defun zuul--buildset-summary-str (buildset)
  "Return a summary of BUILDSET."
  (if-let* ((summary
              (thread-last (zuul--builds buildset)
                (seq-group-by (lambda (it) (zuul--status it)))
                (seq-map (lambda (it)
                           (pcase-let ((`(,type . ,builds) it))
                             (format "%s(%s)" type (length builds))))))))
    (string-join summary " ")
    ""))

(defun zuul--buildset-duration-str (buildset)
  "Return duration of BUILDSET."
  (let-alist (zuul-data buildset)
    (if-let* ((start-time .first_build_start_time)
               (end-time .last_build_end_time)
               (duration
                 (float-time
                   (time-subtract
                     (date-to-time end-time)
                     (date-to-time start-time)))))
      (zuul--duration-str duration)
      "")))

(defun zuul--buildset-patchset-str (buildset)
  "Return patchset for BUILDSET."
  (let-alist (zuul-data buildset) .patchset))

(defun zuul--buildset-start-time-str (buildset)
  "Return start time for BUILDSET."
  (or (zuul--start-time buildset) ""))

(defun zuul--buildset-status-str (buildset)
  "Return status of BUILDSET."
  (let ((status (zuul--status buildset)))
    (zuul--propertize-face
      status
      (funcall zuul-status-face-function status))))

(defun zuul--build-name-str (build)
  "Return the name of the BUILD."
  (let-alist (zuul-data build) .job_name))

(defun zuul--build-pipeline-str (build)
  "Return BUILD's pipeline."
  (let-alist (zuul-data build)
    .pipeline))

(defun zuul--build-duration-str (build)
  "Return the duration of BUILD."
  (let-alist (zuul-data build)
    (if-let ((duration .duration))
      (zuul--duration-str duration)
      "")))

(defun zuul--build-start-time-str (build)
  "Return start time for BUILD."
  (if-let* ((start-time (zuul--start-time build)))
    (zuul--propertize-face start-time 'zuul-time-face)
    ""))

(defun zuul--build-status-str (build)
  "Return status for BUILD."
  (let ((status (zuul--status build)))
    (zuul--propertize-face
      status
      (funcall zuul-status-face-function status))))

(defun zuul--duration-str (duration)
  "Return a string representation of DURATION."
  (let* ((time (format-seconds "%h:%m:%s" duration))
          (re (rx  (group (one-or-more digit)) ":"
                (group (one-or-more digit)) ":"
                (group (one-or-more digit)))))
    (string-match re time)
    (cond ((not (= 0 (string-to-number (match-string 1 time))))
            (format-seconds "%2hh %2mm %2ss" duration))
      ((not (= 0 (string-to-number (match-string 2 time))))
        (format-seconds "%2mm %2ss" duration))
      (t (format-seconds "%2ss" duration)))))

(defun zuul--data-playbook-name-str (data)
  "Return name of playbook in DATA."
  (let-alist (plist-get data :playbook)
    .playbook))

(defun zuul--data-playbook-phase-str (data)
  "Return the phase of playbook in DATA."
  (let-alist (plist-get data :playbook)
    (concat (upcase (substring .phase 0 1))
      (substring .phase 1))))

(defun zuul--data-play-name-str (data)
  "Return the name of play in DATA."
  (let-alist (plist-get data :play)
    .play.name))

(defun zuul--data-task-name-str (data)
  "Return the name of task in DATA."
  (let-alist (plist-get data :task)
    .task.name))

(defun zuul--data-task-role-str (data)
  "Return the role of task in DATA."
  (let-alist (plist-get data :task)
    (or .role.name "")))

(defun zuul--data-task-duration-str (data)
  "Return the duration of task in DATA."
  (let-alist (plist-get data :task)
    (let ((duration
            (float-time
              (time-subtract
                (date-to-time .task.duration.end)
                (date-to-time .task.duration.start)))))
      (zuul--duration-str duration))))

(defun zuul--data-host-name-str (data)
  "Return the name of the host in DATA."
  (pcase-let* ((`(,hostname . ,_data) (plist-get data :host)))
    (symbol-name hostname)))

(defun zuul--data-host-cmd-str (data)
  "Return the command of the host in DATA."
  (pcase-let* ((`(,_hostname . ,data) (plist-get data :host))
                (cmd-str (let-alist data .cmd)))
    (if (stringp cmd-str)
      cmd-str
      (string-join cmd-str " "))))

(defun zuul--data-host-result-str (data)
  "Return the result of the host in DATA."
  (pcase-let* ((`(,_hostname . ,data) (plist-get data :host))
                (result
                  (let-alist data
                    (if .failed
                      "FAILURE"
                      "SUCCESS"))))
    (zuul--propertize-face
      result
      (funcall zuul-status-face-function result))))

(defun zuul--propertize-face (str value)
  "Put face VALUE on STR."
  (put-text-property 0 (length str) 'face value str)
  str)

(defun zuul--build-mode-line ()
  "Return main modeline string."
  (let-alist (zuul-data zuul--current-build)
    (format "[%s,%s] %s"
      .change
      .patchset
      (zuul--build-name-str zuul--current-build))))

(defun zuul--build-mode-line-status ()
  "Return the status of the modeline."
  (let-alist (zuul-data zuul--current-build)
    (zuul--build-status-str zuul--current-build)))

(defun zuul--build-mode-line-id ()
  "Return the id of the modeline."
  (format "(%s/%s)"
    (1+ (zuul--current-build-index
          (zuul--current-builds-with-index)))
    (length (zuul--current-builds-with-index))))

;;;;; Other

(defun zuul--highlight-cmd ()
  "Highlight commands in build log."
  (let ((property))
    (save-excursion
      (goto-char (point-min))
      (while (setq property (text-property-search-forward 'zuul-task))
        (save-excursion
          (goto-char (prop-match-beginning property))
          (search-forward "$")
          (let* ((ov-prompt (make-overlay (prop-match-beginning property) (point)))
                  (ov-input (make-overlay (point) (line-end-position))))
            (overlay-put ov-prompt 'face 'zuul-command-prompt-face)
            (overlay-put ov-input 'face 'zuul-prompt-input-face)))))))

;;;; Major mode

(defvar zuul-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") #'zuul-switch-build)
    (define-key map (kbd "C-c C-.") #'eldoc-mode)
    (define-key map (kbd "C-c ]") #'zuul-next-build)
    (define-key map (kbd "C-c C-o") #'zuul-open-build-in-browser)
    (define-key map (kbd "C-c [") #'zuul-previous-build)
    (define-key map (kbd "C-c C-p") #'zuul-previous-command)
    (define-key map (kbd "C-c C-n") #'zuul-next-command)
    (define-key map (kbd "C-c C-q") #'zuul-quit-build)
    (define-key map (kbd "C-c C-s") #'zuul-switch-buildset)
    (define-key map (kbd "C-c C-r") #'zuul-run-build-command)
    (define-key map (kbd "g") #'zuul-run-build-command)
    map))

(define-derived-mode zuul-log-mode fundamental-mode "Zuul Log"
  "Mode for `zuul' build log."
  (ansi-color-apply-on-region (point-min) (point-max))
  (comint-carriage-motion (point-min) (point-max))
  (setq zuul--project-files (zuul--project-files))
  (setq-local compilation-parse-errors-filename-function #'zuul--locate-file)
  (setq-local imenu-create-index-function #'zuul--imenu-index)
  (add-hook 'eldoc-documentation-functions #'zuul--eldoc-function nil t)
  (zuul--highlight-cmd))

(provide 'zuul)

;;; zuul.el ends here
