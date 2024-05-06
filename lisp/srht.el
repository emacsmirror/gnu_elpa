;;; srht.el --- Sourcehut               -*- lexical-binding: t; -*-

;; Copyright © 2022-2023  Free Software Foundation, Inc.

;; Author: Aleksandr Vityazev <avityazev@posteo.org>
;; Maintainer: Aleksandr Vityazev <avityazev@posteo.org>
;; Keywords: comm vc
;; Package-Version: 0.4
;; Homepage: https://sr.ht/~akagi/srht.el/
;; Keywords: comm
;; Package-Requires: ((emacs "27.1") (plz "0.7") (transient "0.4.3"))

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; https://man.sr.ht/api-conventions.md
;;

;;; Code:
(require 'cl-lib)
(require 'plz)
(require 'rx)
(require 'auth-source)

(defgroup srht nil
  "Customize options."
  :prefix "srht"
  :group 'comm)

(defcustom srht-instances '("sr.ht")
  "Sourcehut instance domain names."
  :type '(list (repeat :tag "Instance"
		       :inline t
		       (string :format "%v")))
  :group 'srht)

(defcustom srht-token nil
  "Personal access token for Sourcehut instance.
It is necessary to use Oauth personal token not Oauth2."
  :type 'string
  :group 'srht)

(defcustom srht-username ""
  "Sourcehut username.  May contain ~ or not, your choice."
  :type 'string
  :group 'srht)

(defcustom srht-browse-url-after-kill nil
  "When t open the url copied to `kill-ring' url `browse-url'."
  :type 'boolean
  :group 'srht)

(defun srht-token (host)
  "Lookup variable `srht-token' if needed and return it.
If variable `srht-token' is nil or cannot be looked up or is empty, signal
an error.  HOST - DNS name of the service."
  (if srht-token
      srht-token
    (let ((token (when-let ((f (plist-get
                                (car
                                 (if srht-username
                                     (auth-source-search :host host
                                                         :user srht-username)
                                   (auth-source-search :host host)))
                                :secret)))
                   (funcall f))))
      (unless token
        (error "No srht-token"))
      (when (string-empty-p token)
        (error "srht-token must not be empty"))
      token)))

(defmacro srht-with-no-message (&rest body)
  "Evaluate BODY with messages are not displayed."
  `(let ((inhibit-message t)
         (message-log-max nil))
     ,@body))

(cl-defun srht--build-uri-string (scheme &key host path query)
  "Construct a URI string.
SCHEME should be a symbol.  HOST should be strings or nil
PATH should be strings or nil.  QUERY should be strings or nil."
  (concat
   (if scheme (concat (symbol-name scheme) ":") "")
   (if host
       (concat "//"
               (if (string-match-p ":" host)
                   (format "[%s]" host)
                 host))
     "")
   (pcase path
     ((or (pred null) (pred string-empty-p)) "")
     ((rx bol "/" (zero-or-more alnum)) path)
     (_ (error "Expected absolute path starting with \"/\" or empty string: %s" path)))
   (if query (concat "?" query) "")))

(defun srht--make-uri (instance service path query)
  "Construct a URI for making a request to Sourcehut INSTANCE.
SERVICE is name of the service, PATH is the path for the URI, and
QUERY is the query for the URI."
  (cl-assert (and (not (string-empty-p instance)) instance)
             nil "Require instance")
  (let ((host (format "%s.%s" service instance)))
    (srht--build-uri-string
     'https :host host :path path :query query)))

(defun srht--else (plz-error)
  "An optional callback function.
Called when the request fails with one argument, a ‘plz-error’ struct PLZ-ERROR."
  (pcase-let* (((cl-struct plz-error response) plz-error)
               ((cl-struct plz-response status) response))
    (pcase status
      (201 (message "Created. Successful with status %s." status))
      (204 (message "No Content. Successful with status %s" status))
      (_ (error "Unkown error with status %s: %S" status plz-error)))))

(defun srht--as ()
  "Parse and return the JSON object following point.
A function, which is called in the response buffer with it
narrowed to the response body."
  (let ((json-object-type 'plist)
        (json-key-type 'keyword)
        (json-array-type 'list))
    (json-read)))

;; TODO add body-type to use with `multipart/from-data'
(cl-defun srht--api-request (method &key instance service path query
                                    body (else #'srht--else)
                                    form (then 'sync) (as #'srht--as)
                                    &allow-other-keys)
  "Request METHOD from SERVICE.
Return the curl process object or, for a synchronous request, the
selected result.

INSTANCE is the instance name of the Sourcehut instance.

HEADERS may be an alist of extra headers to send with the
request.

PATH is the path for the URI and QUERY is the query for the URI.

If FORM is nil, the content type used will be `application/json'.

BODY is the body sent to the URI.

AS selects the kind of result to pass to the callback function
THEN (see `plz').
THEN is a callback function, which is called in the response data.
ELSE is an optional callback function called when the request
fails with one argument, a `plz-error' struct."
  (let ((uri (srht--make-uri instance service path query))
        (content-type (or form "application/json")))
    (plz method uri
      :headers `(,(cons "Content-Type" content-type)
                 ,(cons "Authorization"
                        (concat "token " (srht-with-no-message
                                          (srht-token "sr.ht")))))
      :body body
      :then then
      :else else
      :as as)))


(cl-defun srht--gql-api-request (&key instance service query form token-host
                                      (else #'srht--else)
                                      (then 'sync)
                                      (as #'srht--as)
                                      &allow-other-keys)
  (let ((uri (srht--make-uri instance service "/query" nil))
        (content-type (or form "application/json")))
    (plz 'post uri
      :headers `(,(cons "Content-Type" content-type)
                 ,(cons "Authorization"
                        (format "Bearer %s" (srht-with-no-message
                                             (srht-token token-host)))))
      :body query
      :then then
      :else else
      :as as)))

(defun srht-generic-crud (instance service path &optional query body form)
  "Return a list of arguments to pass to `srht--make-crud-request'.
INSTANCE is the instance name of the Sourcehut instance.
SERVICE is the service to used, and PATH is the path for the URI.
BODY is optional, if it is an empty list, the resulting list will not
contain the body at all.  FORM is optional.  QUERY is the query for the
URI."
  (let ((crud `(:instance ,instance :service ,service
                :path ,path :query , query :form ,form)))
    (if body
        (append crud `(:body ,(if form body (json-encode body))))
      crud)))

(defun srht--make-crud-request (method args)
  "Make API request with METHOD and ARGS."
  (apply #'srht--api-request method (append (car args) (cdr args))))

(defun srht-create (&rest args)
  "Create an API request with ARGS using the POST method."
  (srht--make-crud-request 'post args))

(defun srht-retrive (&rest args)
  "Create an API request with ARGS using the GET method."
  (srht--make-crud-request 'get args))

(defun srht-update (&rest args)
  "Create an API request with ARGS using the PUT method."
  (srht--make-crud-request 'put args))

(defun srht-delete (&rest args)
  "Create an API request with ARGS using the DELETE method."
  (srht--make-crud-request 'delete args))

(defun srht-read-with-annotaion (prompt collection annot-function category)
  "Read a string in the minibuffer, with completion.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
COLLECTION can be a list of strings, an alist or a hash table.
ANNOT-FUNCTION value should be a function for “annotating” completions.
The function should take one argument, STRING, which is a possible completion.
CATEGORY value should be a symbol describing what kind of text the
completion function is trying to complete."
  (declare (indent 1))
  (let ((table
         (lambda (string pred action)
           (if (eq action 'metadata)
               `(metadata
                 (category . ,category)
                 (annotation-function . ,annot-function)
                 (cycle-sort-function . identity)
                 (display-sort-function . identity))
             (complete-with-action action collection string pred)))))
    (completing-read prompt table nil t)))

(defun srht-copy-url (url)
  "Make URL the latest kill in the kill ring."
  (kill-new url)
  (message "Copied \"%s\" into clipboard" url))

(defun srht-browse-url (url)
  "Browse URL."
  (when srht-browse-url-after-kill
    (browse-url url)))

(defmacro srht-with-json-read-from-string (string pattern &rest body)
  "Read the JSON object contained in STRING.
Bind it with the ‘pcase’ PATTERN and do BODY."
  (declare (indent 1))
  `(pcase-let* ((json-object-type 'plist)
                (json-key-type 'keyword)
                (json-array-type 'list)
                (,pattern (json-read-from-string ,string)))
     ,@body))

(defun srht-read-instance (prompt)
  "Read instance name of the Sourcehut instance from `srht-instances' collection.
If the collection contains only one name, return it without completion.
PROMPT is a string to prompt with; normally it ends in a colon and a space."
  (if (eq (length srht-instances) 1)
      (car srht-instances)
    (completing-read prompt srht-instances nil t)))

(defun srht-read-visibility (prompt &optional initial-input)
  "Select a visibility through `completing-read'.
PROMPT, INITIAL-INPUT see `completing-read' doc."
  (completing-read prompt '("private" "public" "unlisted") nil t initial-input))

(defun srht-plist-get (plist &rest kw-list)
  "Extract a value for last keyword in KW-LIST from a property list.
PLIST nested property list."
  (declare (indent 1))
  (seq-reduce (lambda (acc kw)
                (setq acc (plist-get acc kw)))
              kw-list plist))

(defun srht-results-get (instance plist)
  "Extract the value for the :results property.
For the existing PLIST for the INSTANCE instance name."
  (declare (indent 1))
  (srht-plist-get plist (intern instance) :results))

(defmacro srht-put (plist instance val)
  "Change value in PLIST of INSTANCE to VAL if is not nil."
  (declare (indent 1))
  `(when ,val (setq ,plist (plist-put ,plist (intern ,instance) ,val))))

(defun srht-annotation (str visibility created)
  "Return an annotation for STR using VISIBILITY and CREATED."
  (declare (indent 1))
  (let* ((ws-char (string-to-char " "))
         (len (- 40 (length (substring-no-properties str))))
         (blank (make-string (if (> len 1) len 1) ws-char)))
    (concat blank visibility (make-string (- 12 (length visibility)) ws-char)
            created)))

(declare-function iso8601-parse "iso8601" (string &optional form))

(defun srht--format-date (str)
  "PARSE an ISO 8601 STR.
Return string in format DAY.MONTH.YEAR."
  (pcase-let (((seq _sec _min _hour day month year)
               (iso8601-parse str)))
    (format "%02d.%02d.%02d" day month year)))

(defalias 'srht--make-vtable
  (if (and (require 'vtable nil t)
           (fboundp 'make-vtable))
      #'make-vtable
    (lambda (&rest _args)
      (error "Require vtable"))))

(defalias 'srht--vtable-colum
  (if (and (require 'vtable nil t)
           (fboundp 'vtable-column))
      #'vtable-column
    (lambda (&rest _args)
      (error "Require vtable"))))

(defalias 'srht--define-keymap
  (if (and (require 'keymap nil t)
           (fboundp 'define-keymap))
      #'define-keymap
    (lambda (&rest _args)
      (error "Require define-keymap"))))

(cl-defun srht--vtable (&key buffer
                             columns
                             objects
                             getter
                             separator-width
                             actions)
  "Create and inster a vtable to BUFFER.
After that display BUFFER in the window.
COLUMNS OBJECTS GETTER SEPARATOR-WIDTH ACTIONS (see `make-vtable')."
  (let ((buff (get-buffer-create buffer)))
    (with-current-buffer buff
      (let ((inhibit-read-only t))
        (srht--make-vtable
         :columns columns
         :objects objects
         :getter getter
         :separator-width separator-width
         :keymap (srht--define-keymap
                   "q" #'kill-current-buffer
                   "n" #'next-line
                   "p" #'previous-line)
         :actions actions))
      (read-only-mode)
      (hl-line-mode))
    (switch-to-buffer buffer)))

(defun srht--view (instance repositories &optional actions)
  "Display a list of Sourcehut INSTANCE REPOSITORIES.
ACTIONS are simple commands that will be called with the
object under point."
  (declare (indent 2))
  (srht--vtable
   :buffer "*Sourcehut repositories*"
   :columns '("Name"
              (:name "Visibility"
               :formatter (lambda (val) (when val (downcase val))))
              (:name "Created"
               :formatter srht--format-date
               :width 10)
              (:name "Updated"
               :formatter srht--format-date
               :width 10))
   :objects (plist-get repositories (intern instance))
   :getter (lambda (object column vtable)
             (pcase (srht--vtable-colum vtable column)
               ("Name" (plist-get object :name))
               ("Visibility" (plist-get object :visibility))
               ("Created" (plist-get object :created))
               ("Updated" (plist-get object :updated))))
   :separator-width 5
   :actions actions))

(defun srht--view-log (log &optional actions)
  "Display repository LOG.
ACTIONS are simple commands that will be called with the
object under point."
  (srht--vtable
   :buffer "*Sourcehut log*"
   :columns '("ShortId"
              (:name "Message"
               :width 40)
              (:name "Author name")
              (:name "Author email"))
   :objects log
   :getter (lambda (object column vtable)
             (pcase (srht--vtable-colum vtable column)
               ("ShortId" (plist-get object :shortId))
               ("Message" (replace-regexp-in-string
                           "\n" "" (plist-get object :message)))
               ("Author name" (srht-plist-get object :author :name))
               ("Author email" (srht-plist-get object :author :email))))
   :separator-width 1
   :actions actions))

(provide 'srht)
;;; srht.el ends here
