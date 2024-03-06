;;; rt-liberation-rest.el --- Interface to the RT REST API  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015  Free Software Foundation, Inc.
;;
;; Authors: Yoni Rabkin <yrk@gnu.org>
;;
;; This file is a part of rt-liberation.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.


;;; History:
;;
;; Started in May of 2014 in order to remove rt-liberation's
;; dependency on a local copy of the RT CLI.

;;; Code:
(require 'url)
(require 'url-util)
(require 'auth-source)


;;; ------------------------------------------------------------------
;;; variables and constants
;;; ------------------------------------------------------------------
(defvar rt-liber-rest-debug-buffer-name "*rt-liber-rest debug log*"
  "Buffer name of debug capture.")

(defvar rt-liber-rest-debug-p nil
  "If non-nil, record traffic in a debug buffer.")

(defvar rt-liber-rest-scheme "https"
  "Scheme used for transport. Is one of http or https.")

(defvar rt-liber-rest-url ""
  "URL of RT installation.")

(defvar rt-liber-rest-username nil
  "Username of RT account.")

(defvar rt-liber-rest-password nil
  "Password of RT account.")

(defvar rt-liber-rest-verbose-p t
  "If non-nil, be verbose about what's happening.")

(defvar rt-liber-ticket-old-threshold 30
  "Age in days before a ticket is considered old.")

(defvar rt-liber-field-dictionary
  '((owner   . "Owner")
    (queue   . "Queue")
    (status  . "Status")
    (subject  . "Subject")
    (priority  . "Priority"))
  "Mapping between field symbols and RT field strings.
The field symbols provide the programmer with a consistent way of
referring to RT fields.")

(defvar rt-liber-debug-log-enable nil
  "If t then enable logging of communication to a buffer.
Careful! This might create a sizable buffer.")

(defvar rt-liber-debug-log-buffer-name "*rt-liber debug log*"
  "Name of debug log buffer.")


;;; ------------------------------------------------------------------
;;; functions
;;; ------------------------------------------------------------------
(defun rt-liber-rest-write-debug (str)
  "Write to debug buffer."
  (when (not (stringp str))
    (error "argument not string"))
  (when rt-liber-rest-debug-p
    (with-current-buffer
	(get-buffer-create rt-liber-rest-debug-buffer-name)
      (goto-char (point-max))
      (insert str))))

(defun rt-liber-rest-auth ()
  "Try to get the REST credentials."
  (if (and (stringp rt-liber-rest-username)
	   (stringp rt-liber-rest-password)
	   (< 0 (length rt-liber-rest-username))
	   (< 0 (length rt-liber-rest-password)))
      t
    (message "rt-liber: no REST credentials set, so attempting auth-source")
    (let ((auth-source-found-p
	   (auth-source-search :host "rt-liberation" :require '(:user :secret) :create nil)))
      (when (not auth-source-found-p)
	(error "no auth-source found for login"))
      (setq rt-liber-rest-password (funcall (plist-get (nth 0 auth-source-found-p) :secret))
	    rt-liber-rest-username (plist-get (nth 0 auth-source-found-p) :user)))))

(defun rt-liber-rest-search-string (scheme url username password query)
  "Return the search query string."
  (let ((user (url-encode-url username))
	(pass (url-encode-url password)))
    (concat scheme
	    "://"
	    url
	    "/REST/1.0/search/ticket" "?"
	    "user=" user "&"
	    "pass=" pass "&"
	    "query=" (url-encode-url query) "&"
	    "format=i" "&"
	    "orderby=+Created")))

(defun rt-liber-rest-show-string (scheme url ticket-id-list username password _query)
  "Return the ticket show string."
  (let ((user (url-encode-url username))
	(pass (url-encode-url password)))
    (concat scheme
	    "://"
	    url
	    "/REST/1.0/ticket/" ticket-id-list
	    "/show" "?"
	    "user=" user "&"
	    "pass=" pass "&")))

(defun rt-liber-rest-history-string (scheme url ticket-id username password)
  "Return the ticket show string."
  (let ((user (url-encode-url username))
	(pass (url-encode-url password)))
    (concat scheme
	    "://"
	    url
	    "/REST/1.0/ticket/" ticket-id
	    "/history" "?"
	    "format=l" "&"
	    "user=" user "&"
	    "pass=" pass)))

(defun rt-liber-rest-command-edit-string (scheme url ticket-id username password)
  "Return the ticket edit string."
  (let ((user (url-encode-url username))
	(pass (url-encode-url password)))
    (concat scheme
	    "://"
	    url
	    "/REST/1.0/ticket/" ticket-id
	    "/edit" "?"
	    "user=" user "&"
	    "pass=" pass)))

(defun rt-liber-rest-call (url)
  "Perform a REST call with URL."
  (let ((url-request-method "POST"))
    (let ((response
	   (url-retrieve-synchronously url))
	  str)
      (setq str
	    (decode-coding-string
	     (with-current-buffer response
	       (buffer-substring-no-properties (point-min)
					       (point-max)))
	     'utf-8))
      (message "done")
      (rt-liber-rest-write-debug
       (format "outgoing rest call -->\n%s\n<-- incoming\n%s\n" url str))
      str)))

(defun rt-liber-rest-query-runner (op query-string)
  "Run OP on QUERY-STRING."
  (when (or (not (stringp op))
	    (not (stringp query-string)))
    (error "bad arguments"))
  (rt-liber-rest-auth)
  (cond ((string= op "ls")
	 (rt-liber-rest-call
	  (rt-liber-rest-search-string rt-liber-rest-scheme
				       rt-liber-rest-url
				       rt-liber-rest-username
				       rt-liber-rest-password
				       query-string)))
	((string= op "show")
	 (rt-liber-rest-call
	  (rt-liber-rest-show-string rt-liber-rest-scheme
				     rt-liber-rest-url
				     query-string
				     rt-liber-rest-username
				     rt-liber-rest-password
				     query-string)))
	((string= op "history")
	 (rt-liber-rest-call
	  (rt-liber-rest-history-string rt-liber-rest-scheme
					rt-liber-rest-url
					query-string
					rt-liber-rest-username
					rt-liber-rest-password)))
	(t (error "unknown op [%s]" op))))

(defun rt-liber-rest-parse-http-header ()
  "Parse the HTTP header from the server."
  (let ((http-ok-regexp "^HTTP.*200 OK$")
	(rt-ok-regexp   "^rt/.*200 ok$"))
    (condition-case nil
	(progn
	  (re-search-forward http-ok-regexp (point-max))
	  (re-search-forward rt-ok-regexp (point-max)))
      (error "bad HTTP response from server")))) ;FIXME: Unused string!

(defun rt-liber-rest-show-process (response)
  "Process and return the show query response."
  (when (not (stringp response))
    (error "argument not a string"))
  (with-temp-buffer
    (save-excursion
      (insert response))
    (rt-liber-rest-parse-http-header)
    (buffer-substring (point) (point-max))))

(defun rt-liber-rest-show-query-runner (idsublist)
  "Iterate over IDSUBLIST and return the collected result."
  (when (not (listp idsublist))
    (error "argument not list"))
  (with-temp-buffer
    (let ((ticket-ids (reverse (copy-tree idsublist)))
	  (c 1)
	  (l (length idsublist)))
      (while ticket-ids

	(when rt-liber-rest-verbose-p
	  (message "retrieving ticket %d/%d" c l)
	  (setq c (1+ c)))

	(insert
	 (rt-liber-rest-show-process
	  (rt-liber-rest-query-runner "show" (caar ticket-ids))))
	(setq ticket-ids (cdr ticket-ids))
	(when ticket-ids
	  (insert "\n--\n")))
      (when rt-liber-rest-verbose-p
	(message "done retrieving %d tickets" l)))
    (buffer-substring (point-min) (point-max))))

(defun rt-liber-rest-handle-response (buffer)
  "Handle the response provided in BUFFER."
  (with-current-buffer buffer
    (rt-liber-rest-write-debug (buffer-string))))

(defun rt-liber-rest-edit-runner (ticket-id field value)
  "Run edit comment to set FIELD to VALUE."
  (message "started edit command at %s..." (current-time-string))
  (message "ticket #%s, %s <- %s" ticket-id field value)
  (rt-liber-rest-auth)
  (let ((request-data
	 (format "content=%s: %s"
		 (url-hexify-string field)
		 (url-hexify-string value))))
    (rt-liber-rest-write-debug (concat request-data "\n"))
    (let ((url-request-method "POST")
	  (url-request-data request-data)
	  response-buffer)
      (setq response-buffer
	    (url-retrieve-synchronously
	     (rt-liber-rest-command-edit-string
	      rt-liber-rest-scheme
	      rt-liber-rest-url
	      ticket-id
	      rt-liber-rest-username
	      rt-liber-rest-password)))
      (rt-liber-rest-handle-response response-buffer)))
  (message "edit command ended at %s" (current-time-string)))


;;; --------------------------------------------------------
;;; Debug log
;;; --------------------------------------------------------
(defun rt-liber-debug-log-write (str)
  "Write STR to debug log."
  (when (not (stringp str))
    (error "must be a string"))
  (with-current-buffer (get-buffer-create
			rt-liber-debug-log-buffer-name)
    (goto-char (point-max))
    (insert str)))


;;; --------------------------------------------------------
;;; Parse Answer
;;; --------------------------------------------------------
(defun rt-liber-parse-answer (answer-string parser-f)
  "Operate on ANSWER-STRING with PARSER-F."
  (with-temp-buffer
    (insert answer-string)
    (goto-char (point-min))
    (when rt-liber-debug-log-enable
      (rt-liber-debug-log-write (buffer-substring (point-min)
						  (point-max))))
    (funcall parser-f)))


;;; --------------------------------------------------------
;;; Ticket list retriever
;;; --------------------------------------------------------
(put 'rt-liber-no-result-from-query-error
     'error-conditions
     '(error rt-liber-errors rt-liber-no-result-from-query-error))

(put 'rt-liber-no-result-from-query-error
     'error-message
     "No results from query")

(defun rt-liber-ticket-base-retriever-parser-f ()
  "Parser function for ticket list."
  (let (ticketbase-list ticketbase (continue t))
    (while (save-excursion
	     (re-search-forward "^id:" (point-max) t))
      (while (and continue
		  (re-search-forward
		   "^\\(\\([.{} #[:alpha:]]+\\): \\(.*\\)\\)$\\|^--$"
		   (point-max) t))
	(if (string= (match-string-no-properties 0) "--")
	    (setq continue nil)
	  (push (cons (match-string-no-properties 2)
		      (match-string-no-properties 3))
		ticketbase)))
      (push (copy-sequence ticketbase) ticketbase-list)
      (setq ticketbase nil
	    continue t))
    ticketbase-list))

(defun rt-liber-rest-ticketsql-runner-parser-f ()
  "Parser function for a textual list of tickets."
  (let (idsub-list)
    (rt-liber-rest-parse-http-header)
    (while (re-search-forward "ticket/\\([0-9].+\\)" (point-max) t)
      (push (list (match-string-no-properties 1)
		  ".")
	    idsub-list))
    idsub-list))

(defun rt-liber-rest-run-ls-query (query)
  "Run an \"ls\" type query against the server with QUERY."
  (rt-liber-parse-answer
   (rt-liber-rest-query-runner "ls" query)
   'rt-liber-rest-ticketsql-runner-parser-f))

(defun rt-liber-rest-run-show-base-query (idsublist)
  "Run \"show\" type query against the server with IDSUBLIST."
  (rt-liber-parse-answer
   (rt-liber-rest-show-query-runner idsublist)
   #'rt-liber-ticket-base-retriever-parser-f))

(defun rt-liber-rest-run-ticket-history-base-query (ticket-id)
  "Run history query against server for TICKET-ID."
  (rt-liber-parse-answer
   (rt-liber-rest-query-runner "history" ticket-id)
   #'(lambda ()
       (rt-liber-rest-parse-http-header)
       (buffer-substring (point) (point-max)))))

(defun rt-liber-rest-command-set (id field status)
  "Set ticket ID status to be STATUS."
  (rt-liber-parse-answer
   (rt-liber-rest-edit-runner id field status)
   'rt-liber-command-runner-parser-f))


;;; --------------------------------------------------------
;;; Ticket utilities
;;; --------------------------------------------------------
(defun rt-liber-ticket-days-old (ticket-alist)
  "Return the age of the ticket in positive days."
  (days-between (format-time-string "%Y-%m-%dT%T%z" (current-time))
		(cdr (assoc "Created" ticket-alist))))

(defun rt-liber-ticket-old-p (ticket-alist)
  (<= rt-liber-ticket-old-threshold
      (rt-liber-ticket-days-old ticket-alist)))

(defun rt-liber-ticket-id-only (ticket-alist)
  "Return numerical portion of ticket number from TICKET-ALIST."
  (if ticket-alist
      (substring (cdr (assoc "id" ticket-alist)) 7)
    nil))

(defun rt-liber-ticket-priority-only (ticket-alist)
  "Return an integer value priority or NIL."
  (if ticket-alist
      (let ((p-str (cdr (assoc "Priority" ticket-alist))))
	(if p-str
	    (string-to-number p-str)
	  nil))
    nil))

(defun rt-liber-ticket-owner-only (ticket-alist)
  "Return the string value of the ticket owner."
  (when (not ticket-alist)
    (error "null ticket-alist"))
  (cdr (assoc (rt-liber-get-field-string 'owner)
	      ticket-alist)))

(defun rt-liber-get-field-string (field-symbol)
  (when (not field-symbol)
    (error "null field symbol"))
  (cdr (assoc field-symbol rt-liber-field-dictionary)))


(provide 'rt-liberation-rest)

;;; rt-liberation-rest.el ends here.
