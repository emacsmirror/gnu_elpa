;;; repology-utils.el --- Utilities for Repology   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>

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

;; This library provides various user-facing tools.  They include accessors and
;; predicates for package, project and problem objects, in addition to more
;; specialized functions useful in configuration variables.

;; The library also provides the low-level `repology-request', used to
;; perform a raw HTTP request on a given URL.

;;; Code:

(require 'seq)
(require 'url)


;;; Constants
(defconst repology-package-all-fields
  '(repo subrepo name srcname binname visiblename version origversion status
         summary categories licenses maintainers www downloads)
  "List of known package fields.")

(defconst repology-package-all-status
  '("newest" "devel" "unique" "outdated" "legacy" "rolling" "noscheme"
    "incorrect" "untrusted" "ignored")
  "List of known status values.")

(defconst repology-request-delay 1.0
  "Number of seconds between two requests to Repology API.
The value is defined according to its terms of use.  See URL
`https://repology.org/api'.")


;;; Packages
(defun repology-package-p (object)
  "Return t if OBJECT is a package."
  (and (consp object)
       ;; Mandatory fields.
       (stringp (alist-get 'repo object))
       (stringp (or (alist-get 'name object)
                    (alist-get 'srcname object)
                    (alist-get 'binname object)))
       (stringp (alist-get 'version object))))

(defun repology-package-field (package field)
  "Return PACKAGE's FIELD.

FIELD is a symbol among:

`repo'
    name of repository for this package

`subrepo'
    name of subrepository (if applicable; for example, main or contrib or
    non-free for Debian)

`name', `srcname', `binname'
    package name(s) as used in repository - generic one and/or source package
    name and/or binary package name, whichever is applicable

`visiblename'
    package name as shown to the user by Repology

`version'
    package version (sanitized, as shown by Repology)

`origversion'
    package version as in repository

`status'
    package status, one of \"newest\", \"devel\", \"unique\", \"outdated\", \
\"legacy\",
    \"rolling\", \"noscheme\", \"incorrect\", \"untrusted\", \"ignored\"

`summary'
    one-line description of the package

`categories'
    list of package categories

`licenses'
    list of package licenses

`maintainers'
    list of package maintainers

`www'
    list of package webpages

`downloads'
    list of package downloads

Mandatory fields are `repo', `visiblename', and `version'; all other fields
are optional."
  (unless (memq field repology-package-all-fields)
    (user-error "Unknown field: %S" field))
  (alist-get field package))

(defun repology-package-repository-full-name (package)
  "Return PACKAGE repository's full name.
Return PACKAGE's repository internal name if the full name is unknown."
  (let ((repo (repology-package-field package 'repo)))
    ;; Since `repology-list-repositories' may fail, e.g., due to
    ;; connectivity issues, ensure something is returned anyway, in
    ;; this case, the repository internal name.
    (or (ignore-errors (repology-repository-full-name repo))
        repo)))

(defun repology--package-status-face (package)
  "Return face associated to status from PACKAGE."
  (let ((status (repology-package-field package 'status)))
    (if (member status repology-package-all-status)
        (intern (format "repology-%s-status" status))
      ;; If package status list is not up-to-date, fall back to
      ;; `default' face.
      (warn "Repology: Unknown package status: %S; Using `default' face" status)
      'default)))

(defun repology-package-colorized-status (package)
  "Return colorized status string for PACKAGE.
The version string is emphasized according to PACKAGE's status.
Return \"-\" if PACKAGE has no status field."
  (let ((status (repology-package-field package 'status)))
    (if (stringp status)
        (propertize status 'face (repology--package-status-face package))
      "-")))

(defun repology-package-colorized-version (package)
  "Return colorized version string for PACKAGE.
The version string is emphasized according to PACKAGE's status.
Return \"-\" if PACKAGE has no version field."
  (let ((version (repology-package-field package 'version)))
    (if (stringp version)
        (propertize version 'face (repology--package-status-face package))
      "-")))


;;; Projects
(defun repology-project-p (object)
  "Return t if OBJECT is a project."
  (pcase object
    (`(,(pred symbolp) . ,(and (pred listp) packages))
     (seq-every-p #'repology-package-p packages))
    (_ nil)))

(defun repology-project-name (project)
  "Return PROJECT's name, as a string."
  (symbol-name (car project)))

(defun repology-project-packages (project)
  "Return list of packages associated to PROJECT."
  (cdr project))

(defun repology-project-create (name packages)
  "Create a project object out of a NAME and a list of PACKAGES.
NAME is a string or a symbol.  PACKAGES is a list of package objects."
  (let* ((name-symbol
          (pcase name
            ((pred symbolp) name)
            ((pred stringp) (intern name))
            (_ (user-error "Invalid project name: %S" name))))
         (project (cons name-symbol packages)))
    (unless (repology-project-p project)
      (user-error "Invalid packages value: %S" packages))
    project))

(defun repology-project-newest-version (project)
  "Return newest version string for packages in PROJECT, or nil."
  (let ((newest
         (seq-find (lambda (package)
                     (equal "newest" (repology-package-field package 'status)))
                   (repology-project-packages project))))
    (and newest (repology-package-field newest 'version))))

(defun repology-project-outdated-versions (project)
  "Return a list of outdated versions for packages in PROJECT, or nil.
Versions are sorted in descending order."
  (let ((outdated
         (seq-filter
          (lambda (package)
            (equal "outdated"
                   (repology-package-field package 'status)))
          (repology-project-packages project))))
    (sort (mapcar (lambda (p) (repology-package-field p 'version))
                  outdated)
          ;; Return versions in decreasing order.
          (lambda (s1 s2) (repology-version-< s2 s1)))))

(defun repology--masked-package-p (package masks)
  "Return non-nil if PACKAGE is masked.
PACKAGE is a Repology package. MASKS is a list of
masks, as defined in `repology-outdated-project-definition'."
  (seq-some
   (pcase-lambda (`(,_ ,version ,repository-re))
     (and (or (not version)
              (progn
                (unless (string-match
                         (rx string-start
                             (or "<=" "<" "=" ">" ">=")
                             (zero-or-more space))
                         version)
                  (user-error "Invalid version comparison string: %S"
                              version))
                (let ((prefix (match-string 0 version))
                      (base (substring version (match-end 0)))
                      (package-version
                       (repology-package-field package 'version)))
                  (pcase prefix
                    ("=" (and
                          (not (repology-version-< base package-version))
                          (not (repology-version-< package-version base))))
                    ("<=" (not (repology-version-< base package-version)))
                    (">=" (not (repology-version-< package-version base)))
                    ("<" (repology-version-< package-version base))
                    (">" (repology-version-< base package-version))))))
          (or (not repository-re)
              (string-match-p repository-re
                              (repology-package-field package 'repo)))))
   masks))


;;; Problems
(defun repology-problem-field (problem field)
  "Return PROBLEM's FIELD.

FIELD is a symbol.  Repology API does not define an exhaustive list of
allowed symbols.  However, it currently supports, among others, the
following ones:

`repo'
    repository name

`visiblename'
    package name as in Repology

`effname'
    repology project name

`maintainer'
    package maintainer associated with the problem; may be null; note that
    if there are multiple package maintainers, problem is duplicated for
    each one

`type'
    textual description of the problem"
  (alist-get field problem))


;;; Repositories
(defconst repology-statistics-url "https://repology.org/repositories/statistics"
  "URL for \"Statistics\" page in Repology website.
It is used as a source for all known repositories.")

(defvar repology--repositories nil
  "List of repositories known to Repology.
The list is populated by `repology-list-repositories'.  Call that function
instead of using this variable.")

(defun repology-list-repositories (&optional full-name)
  "Return repositories known to Repology.

Return a list of strings.  When option argument FULL-NAME is non-nil, list
the repositories with their full name instead of their internal name."
  (unless repology--repositories
    (with-temp-message "Repology: Fetching list of repositories..."
      (let ((request (repology-request repology-statistics-url)))
        (pcase (plist-get request :reason)
          ("OK"
           (let ((body (plist-get request :body))
                 (repositories nil)
                 (start 0)
                 (internal-re (rx "id=\"" (group (+? nonl)) "\""))
                 (full-re (rx "href=\"/repository/"
                              (+? anychar)
                              "\">"
                              (group (+? anychar))
                              "<")))
             (while (string-match internal-re body start)
               (setq start (match-end 0))
               (let* ((repo (match-string 1 body))
                      (true-name
                       (and (string-match full-re body start)
                            (match-string 1 body))))
                 (push (cons repo true-name) repositories)))
             (setq repology--repositories (nreverse repositories))))
          (status
           (error "Cannot retrieve information: %S" status))))))
  (mapcar (if full-name #'cdr #'car) repology--repositories))

(defun repology-refresh-repositories ()
  "Refresh list of repositories known to Repology."
  (setq repology--repositories nil)
  (repology-list-repositories))

(defun repology-repository-name (full-name)
  "Return name of repository named after string FULL-NAME.
Raise an error if FULL-NAME is unknown to Repology."
  (unless (member full-name (repology-list-repositories t))
    (user-error "Unknown repository: %S" full-name))
  (pcase (rassoc full-name repology--repositories)
    (`(,(and (pred stringp) name) . ,_) name)
    (_ (error "Corrupted repository list!"))))

(defun repology-repository-full-name (repository)
  "Return user-facing name for string REPOSITORY.
Raise an error if REPOSITORY is unknown to Repology."
  (unless (member repository (repology-list-repositories))
    (user-error "Unknown repository: %S" repository))
  (or (alist-get repository repology--repositories nil nil #'equal)
      (error "Corrupted repository list!")))


;;; Requests
(defvar repology--last-request-time nil
  "Time for last request to the Repology API, or nil.
It is updated each time `repology-request' is called.")

(defun repology-request (url &optional extra-headers)
  "Perform a raw HTTP request on URL.

EXTRA-HEADERS is an assoc list of headers/contents to send with
the request.

Return a property list with `:code', `:reason', `:header' and
`:body' keywords. The value for `:reason' is either \"OK\", or
a string explaining the issue."
  (let* ((url-request-method "GET")
         (url-request-extra-headers extra-headers))
    ;; Make sure to wait at least for `repology-request-delay' seconds
    ;; between queries.
    (when (and repology--last-request-time
               (< (float-time (time-subtract (current-time)
                                             repology--last-request-time))
                  repology-request-delay))
      (sleep-for repology-request-delay))
    (setq repology--last-request-time (current-time))
    (pcase (condition-case err
               (url-retrieve-synchronously url t)
             (error (error-message-string err)))
      ('nil (list :reason "No data associated to request"))
      ((and (pred stringp) reason) (list :reason reason))
      ((and (pred bufferp) process-buffer)
       (unwind-protect
           (with-current-buffer process-buffer
             (goto-char (point-min))
             (let* ((status-line-regexp
                     (rx bol
                         (one-or-more (not (any " "))) " "
                         (group (in "1-5") (= 2 digit)) " "
                         (group (one-or-more (in "A-Z" "a-z" " ")))
                         eol))
                    (status
                     (and (looking-at status-line-regexp)
                          (list :code (string-to-number (match-string 1))
                                :reason (match-string 2))))
                    (header nil)
                    (body nil))
               (forward-line)
               (while (looking-at (rx line-start (group (+? nonl)) ": "))
                 (push (match-string 1) header)
                 (push (buffer-substring (match-end 0) (line-end-position))
                       header)
                 (forward-line))
               (forward-line)
               (unless (eobp)
                 (setq body (buffer-substring (point) (point-max))))
               (append status (list :header (nreverse header) :body body))))
         (kill-buffer process-buffer)))
      (_ (error "This should not happen")))))


;;; Version Comparison

;; This part implements version comparison as done by Repology's
;; libversion.  See
;; <https://github.com/repology/libversion/blob/master/doc/ALGORITHM.md>.
(defconst repology-version-zero-component '(1 . 0)
  "Version component representing 0 or any missing component.")

(defconst repology-version-pre-keywords '("alpha" "beta" "rc" "pre")
  "List of pre-release keywords in version strings.")

(defconst repology-version-post-keywords '("patch" "post" "pl" "errata")
  "List of post-release keywords in version strings.")

(defun repology--string-to-version (s)
  "Return version associated to string S.
Version is a list of components (RANK . VALUE) suitable for comparison, with
the function `repology-version-<'."
  (let ((result nil)
        (regexp (rx (one-or-more (any digit alpha))))
        (start 0))
    ;; Extract alphanumeric tokens. Then split all numeric and all
    ;; alphabetic components apart.
    (while (string-match regexp s start)
      (setq start (match-end 0))
      (let ((token (match-string 0 s))
            (special-flag nil)
            (i 0))
        (while (string-match (rx (or (group (one-or-more digit))
                                     (one-or-more alpha)))
                             token
                             i)
          ;; Attach ranks to components.  NUMERIC-FLAG is used to catch
          ;; trailing alphabetic components, which get a special rank.
          ;; However, if there is no numeric component, no alphabetic
          ;; component ever gets this rank, hence the initial value.
          (push (cond
                 ;; Numeric component: distinguish between 0 (rank 1)
                 ;; and other value (rank 3).
                 ((match-beginning 1)
                  (let ((component (string-to-number (match-string 1 token))))
                    (when (= i 0) (setq special-flag t))
                    (if (= 0 component)
                        repology-version-zero-component
                      (cons 3 component))))
                 ;; Special case: alphabetic component which follows
                 ;; numeric component, and is not followed by another
                 ;; numeric component.  If it is not a pre- or post-
                 ;; keyword, give it rank 4.
                 ((and special-flag
                       (= (match-end 0) (length token)))
                  (let ((component (downcase (match-string 0 token))))
                    (cons (cond
                           ((member component repology-version-pre-keywords) 0)
                           ((member component repology-version-post-keywords) 2)
                           (t 4))
                          component)))
                 ;; Now the special case is out of the way, rank is
                 ;; either 2 (the component is a post-keyword), or 0.
                 (t
                  (let ((component (downcase (match-string 0 token))))
                    (cons (if (member component repology-version-post-keywords)
                              2
                            0)
                          component))))
                result)
          (setq i (match-end 0)))))
    (nreverse result)))

(defun repology-version-< (v1 v2)
  "Return t if version V1 is lower (older) than version V2.
V1 and V2 are strings."
  (let ((v1 (repology--string-to-version v1))
        (v2 (repology--string-to-version v2)))
    (catch :less?
      (while (or v1 v2)
        (pcase-let ((`(,r1 . ,v1)
                     (or (pop v1) repology-version-zero-component))
                    (`(,r2 . ,v2)
                     (or (pop v2) repology-version-zero-component)))
          (cond
           ;; First compare ranks, then values.
           ((/= r1 r2) (throw :less? (< r1 r2)))
           ;; Components are equal.  Try next component.
           ((equal v1 v2) nil)
           ;; Numeric components are compared... numerically.
           ((= r1 3) (throw :less? (< v1 v2)))
           ;; Alphabetic components are compared by case insensitively
           ;; comparing their first letters.
           (t (throw :less?
                     (string-lessp (substring v1 0 1) (substring v2 0 1)))))))
      ;; Strings V1 and V2 represent equal versions.
      nil)))


;;; Sort Functions
(defun repology--display-sort-generic (e1 e2 predicate)
  "Compare entries E1 and E2 according to PREDICATE.
E1 and E2 are elements of `tabulated-list-entries'.  PREDICATE is called on the
values from E1 and E2 at the column being sorted."
  (let ((column
         ;; Find number of column being sorted.
         (seq-position tabulated-list-format
                       (car tabulated-list-sort-key)
                       (pcase-lambda (`(,n . ,_) s) (equal n s)))))
    ;; Call PREDICATE on values instead of entries.
    (let ((val1 (elt (cadr e1) column))
          (val2 (elt (cadr e2) column)))
      (funcall predicate val1 val2))))

(defun repology-display-sort-numbers (e1 e2)
  "Return t if E1 is numerically less than E2.
E1 and E2 are elements of `tabulated-list-entries'."
  (repology--display-sort-generic
   e1 e2
   (lambda (s1 s2) (< (string-to-number s1) (string-to-number s2)))))

(defun repology-display-sort-texts (e1 e2)
  "Return t if E1 is before E2, in collation order.
E1 and E2 are elements of `tabulated-list-entries'.  Case is ignored."
  (repology--display-sort-generic
   e1 e2
   (lambda (s1 s2) (string-collate-lessp s1 s2 nil t))))

(defun repology-display-sort-versions (e1 e2)
  "Return t if E1 is older than E2.
E1 and E2 are elements of `tabulated-list-entries'."
  (repology--display-sort-generic e1 e2 #'repology-version-<))


(provide 'repology-utils)
;;; repology-utils.el ends here
