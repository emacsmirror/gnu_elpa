;;; openpgp.el --- Client for keys.openpgp.org       -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2023, 2024  Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <~pkal/public-inbox@lists.sr.ht>
;; URL: https://git.sr.ht/~pkal/openpgp/
;; Version: 1.0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience

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

;; A basic client for querying, verifying and uploading keys to the
;; keys.openpgp.org keyserver.
;;
;; After loading the file, use C-h a openpgp- RET to generate a list
;; of entry points.

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)
(require 'epg)

(defgroup openpgp '()
  "Client for keys.openpgpg.org."
  :prefix "openpgp-"
  :group 'epg)

(defcustom openpgp-keyserver "keys.openpgp.org"
  "Domain of keyserver to use.

NOTE: currently the default value is the only working keyserver,
as federation hasn't been implemented yet."
  :type 'string)

;;; Fetching Keys

(defsubst openpgp--api-url (endpoint &optional arg)
  "Construct VKS request querying ENDPOINT.

The optional argument ARG will be concatenated to the end of the
URL, if non-nil."
  (format "https://%s/vks/v1/%s%s"
	  openpgp-keyserver endpoint
	  (if arg (concat "/" arg) "")))

(defun openpgp--process-key (status)
  "Callback function for `url-retrieve' for receiving a key.
STATUS is a plist described in the docstring for `url-retrieve'."
  (when (plist-get status :error)
    (error "Request failed: %s"
	   (caddr (assq (caddr (plist-get status :error))
			url-http-codes))))
  (forward-paragraph)
  (forward-line)
  (epa-import-armor-in-region (point) (point-max)))

;;;###autoload
(defun openpgp-fetch-key-by-fingerprint (fingerprint)
  "Query key via FINGERPRINT and add to keychain."
  (interactive "sFingerprint: ")
  (let ((fingerprint (string-remove-prefix "0X" (upcase fingerprint))))
    (url-retrieve (openpgp--api-url "by-fingerprint" fingerprint)
		  #'openpgp--process-key)))

;;;###autoload
(defun openpgp-fetch-key-by-keyid (keyid)
  "Query key via KEYID and add to keychain."
  (interactive "sKey ID: ")
  (let ((keyid (string-remove-prefix "0X" (upcase keyid))))
    (url-retrieve (openpgp--api-url "by-keyid" keyid)
		  #'openpgp--process-key)))

(declare-function message--name-table "message" (orig-string))

;;;###autoload
(defun openpgp-fetch-key-by-email (email)
  "Query key via EMAIL and add to keychain."
  (interactive (progn
                 (require 'message)
                 (cdr (mail-extract-address-components
                       (completing-read "Email: " (message--name-table ""))))))
  (url-retrieve (openpgp--api-url "by-email" (url-hexify-string email))
		#'openpgp--process-key))

;;; Uploading Keys

(defun openpgp--key-ids ()
  "Generate a list of KEY ids."
  (let* ((keys (epg-list-keys (epg-make-context)))
	 (uids (mapcan #'epg-key-user-id-list keys))
	 (ids (mapcar #'epg-user-id-string uids))
	 (addr (mapcar #'mail-extract-address-components ids)))
    (mapcan (lambda (addr) (and (car addr) (cdr addr))) addr)))

(defun openpgp--verify-callback (status email)
  "Callback function for `url-retrieve' for verifying a key.
STATUS is a plist described in the docstring for `url-retrieve'.
EMAIL is the email address being verified."
  (when (plist-get status :error)
    (error "Request failed: %s"
	   (caddr (assq (caddr (plist-get status :error))
			url-http-codes))))
  (forward-paragraph)
  (let* ((json-object-type 'hash-table)
	 (data (json-read)))
    (when (gethash "error" data)
      (error "Error in response: %s" (gethash "error" data)))
    (let ((resp (gethash email (gethash "status" data))))
      (cond ((null resp)
	     (message "Verification request might have failed. Are you using the correct address?"))
	    ((string= resp "unpublished")
	     (message "Verification request might have failed. Are you using the correct key?"))
	    ((string= resp "published")
	     (message "Verification request succeeded, but key has already been published."))
	    ((string= resp "revoked")
	     (message "Verification request succeeded, but key has been revoked."))
	    ((string= resp " pending")
	     (message "Verification request succeeded, a email should arrive soon."))))))

(defun openpgp-request-verify (email token)
  "Request verification email for address EMAIL.

TOKEN should be supplied by a previous \"upload-key\" request."
  (let ((url-request-method "POST")
	(url-request-extra-headers '(("Content-Type" . "application/json")))
	(url-request-data (json-encode `(("token" . ,token)
					 ("addresses" . (,email))))))
    (url-retrieve (openpgp--api-url "request-verify")
		  #'openpgp--verify-callback
		  (list email))))

(defun openpgp--upload-callback (status email)
  "Callback function for `url-retrieve' for uploading a key.
STATUS is a plist described in the docstring for `url-retrieve'.
EMAIL is the email address being uploaded."
  (when (plist-get status :error)
    (error "Request failed: %s"
	   (caddr (assq (caddr (plist-get status :error))
			url-http-codes))))
  (forward-paragraph)
  (let* ((json-object-type 'hash-table)
	 (data (json-read)))
    (when (gethash "error" data)
      (error "Error in response: %s" (gethash "error" data)))
    (let ((resp (gethash email (gethash "status" data))))
      (when (cond ((null resp)
		   (yes-or-no-p "Your address hasn't been recognised by the server, are you sure you want to proceed?"))
		  ((string= resp "published")
		   (yes-or-no-p "Key has already been published, are you sure you want to proceed?"))
		  ((string= resp "revoked")
		   (yes-or-no-p "Key has been revoked, are you sure you want to proceed?"))
		  ((string= resp " pending")
		   (yes-or-no-p "Key is already pending, are you sure you want to proceed?"))
		  (t t))
	(openpgp-request-verify email (cdr (assq 'token data)))))))

(defun openpgp-upload-key-string (email key)
  "Upload KEY for address EMAIL to keyserver.

The KEY should be a string, containing a ASCII armoured public
key."
  (let ((url-request-method "POST")
	(url-request-extra-headers '(("Content-Type" . "application/json")))
	(url-request-data (json-encode `(("keytext" . ,key)))))
    (url-retrieve (openpgp--api-url "upload")
		  #'openpgp--upload-callback
		  (list email))))

;;;###autoload
(defun openpgp-upload-key-file (email key-file)
  "Upload key from KEY-FILE for address EMAIL."
  (interactive (list (completing-read "Email: " (openpgp--key-ids)
				      nil nil nil nil user-mail-address)
		     (read-file-name "Key file: ")))
  (with-temp-buffer
    (insert-file-contents key-file)
    (openpgp-upload-key-string email (buffer-string))))

;;;###autoload
(defun openpgp-upload-key (email)
  "Upload public key for address EMAIL using gpg."
  (interactive (list (completing-read "Email: " (openpgp--key-ids)
				      nil nil nil nil user-mail-address)))
  (let* ((addr (shell-quote-argument email))
	 (cmd (format "gpg --armor --export %s" addr))
	 (out (shell-command-to-string cmd)))
    (openpgp-upload-key-string email out)))

(provide 'openpgp)
;;; openpgp.el ends here
