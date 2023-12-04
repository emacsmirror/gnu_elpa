;;; rt-liberation.el --- Emacs interface to RT  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2022 Free Software Foundation, Inc.

;; Author: Yoni Rabkin <yrk@gnu.org>
;; Authors: Aaron S. Hawley <aaron.s.hawley@gmail.com>, John Sullivan <johnsu01@wjsullivan.net>
;; Maintainer: Yoni Rabkin <yrk@gnu.org>
;; Version: 6
;; Keywords: rt, tickets
;; Package-Type: multi
;; url: http://www.nongnu.org/rtliber/

;; This file is a part of rt-liberation.

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

;;; Installation and Use:
;;
;; Detailed instructions for installation and use can be found in the
;; rt-liberation manual, in the doc/ directory of the distribution.

;;; History:
;;
;; Started near the end of 2008.


;;; Code:
(require 'browse-url)
(require 'time-date)
(require 'cl-lib)

(require 'rt-liberation-rest)
(require 'rt-liberation-compiler)


;; wrapper functions around specific functions provided by a backend
(declare-function rt-liber-get-ancillary-text "rt-liberation-storage.el")
(declare-function rt-liber-ticket-marked-p "rt-liberation-multi.el")
(declare-function rt-liber-set-ancillary-text "rt-liberation-storage.el")


(defgroup rt-liber nil
  "*rt-liberation, the Emacs interface to RT"
  :prefix "rt-liber-"
  :group 'rt-liber)

(defcustom rt-liber-directory "~/.emacs.d/rt-liber"
  "*Directory to store persistent information."
  :type 'string
  :group 'rt-liber)


(defface rt-liber-ticket-face
  '((((class color) (background dark))
     (:foreground "DarkSeaGreen"))
    (((class color) (background light))
     (:foreground "Blue"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "Blue")))
  "Face for tickets in browser buffer.")

(defface rt-liber-priority-ticket-face
  '((((class color) (background dark))
     (:foreground "Orange"))
    (((class color) (background light))
     (:foreground "Orange"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "Black")))
  "Face for high priority tickets in browser buffer.")


(defvar rt-liber-viewer-section-header-regexp
  "^# [0-9]+/[0-9]+ (id/[0-9]+/total)")

(defvar rt-liber-viewer-section-field-regexp
  "^\\(.+\\): \\(.+\\)$")

(defvar rt-liber-resolved-string "Resolved"
  "String representation of \"resolved\" query tag.")

(defvar rt-liber-base-url ""
  "Base url for ticket display.")

(defvar rt-liber-content-regexp "^Content:.*$"
  "Regular expression for section headers.")

(defvar rt-liber-correspondence-regexp
  "^Type: \\(EmailRecord\\|CommentEmailRecord\\|Correspond\\)"
  "Regular expression for correspondence sections.")

(defvar rt-liber-username nil
  "Username for assigning ownership on the RT server.")

(defvar rt-liber-browser-buffer-name "*ticket-browser*"
  "Name of ticket browser buffer.")

(defvar rt-liber-browser-buffer nil
  "Ticket browser buffer.")

(defvar rt-liber-browser-default-sorting-function
  'rt-liber-sort-by-time-created
  "Default sorting function.")

(defvar rt-liber-browser-default-filter-function
  'rt-liber-default-filter-f
  "Default filtering function.
This is a function which accepts the ticket alist as a single
argument and returns nil if the ticket needs to be filtered out,
dropped or ignored (however you wish to put it.), otherwise the
function returns a truth value.")

(defvar rt-liber-custom-ticket-redraw-function
  'rt-liber-ticketlist-browser-redraw-f
  "Default ticket redraw function.")

(defvar rt-liber-jump-to-latest nil
  "jump to the latest correspondence when viewing a ticket.")

(defvar rt-liber-anc-p nil
  "Display ancillary data for tickets.")

(defvar rt-liber-ticket-list nil
  "Ticket-list structure (becomes ticket-browser buffer local).")

(defvar rt-liber-query nil
  "Query structure (becomes ticket-browser buffer local).")

(defvar rt-liber-browser-time-format-string "%b %d %Y %H:%M"
  "String passed to `format-time-string' in the ticket browser.")

(defvar rt-liber-browser-priority-cutoff 0
  "Tickets with a priority higher than this are high priority.")

(defvar rt-liber-browser-do-refresh t
  "When t, run `rt-liber-browser-refresh' otherwise disable it.")

(defvar rt-liber-command-dictionary
  '((comment . "comment")
    (edit    . "edit"))
  "Mapping between command symbols and command strings.

The command symbols provide the programmer with a consistent way
of referring to certain commands. The command strings are the
specific strings which would produce the desired effect in the
server.")

(defvar rt-liber-status-dictionary
  '((deleted  . "deleted")
    (resolved . "resolved")
    (open     . "open")
    (new      . "new"))
  "Mapping between status symbols and status strings.
The status symbols provide the programmer with a consistent way
of referring to certain statuses. The status strings are the
server specific strings.")

(defvar rt-liber-ticket-local nil
  "Buffer local storage for a ticket.
This variable is made buffer local for the ticket history")

(defvar rt-liber-assoc-browser nil
  "Browser associated with a ticket history.
This variable is made buffer local for the ticket history")

(defvar rt-liber-display-ticket-at-point-f 'rt-liber-viewer-display-ticket-at-point
  "Function for displaying ticket at point in the browser.")

(defvar rt-liber-viewer-section-regexp "^Ticket [0-9]+ by "
  "Regular expression to match a section in the viewer.")

(defvar rt-liber-viewer-communicate-regexp (regexp-opt '("[Create]" "[Correspond]" "[Comment]"))
  "Regular expression to match ticket communication.")

(defvar rt-liber-viewer-recenter 4
  "Argument passed to `recenter' in the viewer.")


;;; --------------------------------------------------------
;;; Ticket browser
;;; --------------------------------------------------------
;; accept a ticket-alist object and return an alist mapping ticket
;; properties to format characters for use in `rt-liber-format'.
(defun rt-liber-format-function (ticket-alist)
  "Return a pairing of TICKET-ALIST values to %-sequences."
  (let* ((id         (rt-liber-ticket-id-only ticket-alist))
	 (subject    (cdr (assoc "Subject" ticket-alist)))
	 (status     (cdr (assoc "Status" ticket-alist)))
	 (created    (format-time-string
		      rt-liber-browser-time-format-string
		      (date-to-time
		       (cdr (assoc "Created" ticket-alist)))))
	 (resolved   (cdr (assoc "Resolved" ticket-alist)))
	 (requestors (cdr (assoc "Requestors" ticket-alist)))
	 (creator    (cdr (assoc "Creator" ticket-alist)))
	 (owner      (rt-liber-ticket-owner-only ticket-alist))
	 (queue      (cdr (assoc "Queue" ticket-alist)))
	 (anc        (if rt-liber-anc-p
			 (rt-liber-get-ancillary-text
			  (rt-liber-ticket-id-only ticket-alist))
		       ""))
	 (priority   (cdr (assoc "Priority" ticket-alist))))
    (list (cons ?i (or id "N/A"))
	  (cons ?s (or subject "N/A"))
	  (cons ?c (or created "N/A"))
	  (cons ?S (or status "N/A"))
	  (cons ?r (or resolved "N/A"))
	  (cons ?R (or requestors "N/A"))
	  (cons ?C (or creator "N/A"))
	  (cons ?o (or owner "N/A"))
	  (cons ?q (or queue "N/A"))
	  (cons ?A (or anc ""))
	  (cons ?p (or priority "N/A")))))

(defun rt-liber-browser-assoc (char alist)
  "Process the %-sequence association."
  (let ((v (cdr (assoc char alist))))
    (cond ((eq char ?%) "%") ;; escape sequence for %
	  (t (or v "")))))

(defun rt-liber-high-priority-p (ticket-alist)
  "Return t if TICKET-ALIST is high priority.

The ticket's priority is compared to the variable
  `rt-liber-browser-priority-cutoff'."
  (let ((p (rt-liber-ticket-priority-only ticket-alist)))
    (if p
	(< rt-liber-browser-priority-cutoff p)
      nil)))

(defun rt-liber-format (format ticket-alist)
  "Substitute %-sequences in FORMAT."
  (let ((alist (rt-liber-format-function ticket-alist)))
    (replace-regexp-in-string
     "%."
     (lambda (str)
       (rt-liber-browser-assoc (aref str 1) alist))
     format t t)))

(defun rt-liber-ticketlist-browser-redraw-f (ticket)
  "Display TICKET."
  (insert (rt-liber-format "[%c %i %S]" ticket))
  (add-text-properties (line-beginning-position)
		       (line-end-position)
		       '(face rt-liber-ticket-face))
  (when (rt-liber-high-priority-p ticket)
    (let ((p (point)))
      (insert (format " HIGH PRIORITY (%d)"
		      (rt-liber-ticket-priority-only ticket)))
      (add-text-properties p
			   (line-end-position)
			   '(face rt-liber-priority-ticket-face))))

  (newline)
  (insert (rt-liber-format "  [%o] %R: %s" ticket))
  (let ((p (point)))
    (insert (rt-liber-format "    %A" ticket))
    (add-text-properties p (point)
			 '(face font-lock-comment-face)))
  (newline))

(defun rt-liber-ticketlist-browser-redraw (ticketlist &optional query)
  "Display TICKETLIST. Optionally display QUERY as well."
  (erase-buffer)
  (when query
    (insert (format "Query: %s" query))
    (newline)
    (insert (format "%d tickets" (length ticketlist)))
    (newline))
  (when ticketlist
    (let ((filtered-count 0))
      (newline 2)
      (dolist (ticket
	       (funcall rt-liber-browser-default-sorting-function
			ticketlist))
	;; skip filtered tickets, but count how many have been skipped
	(if (funcall rt-liber-browser-default-filter-function ticket)
	    (progn
	      ;; assumes that rt-liber-ticketlist-browser-redraw-f leaves
	      ;; point at the end of the ticket drawn
	      (let ((start (point)))
		(funcall rt-liber-custom-ticket-redraw-function ticket)
		(add-text-properties start
				     (point)
				     (list 'rt-ticket ticket))
		(when (and (featurep 'rt-liberation-multi)
			   (rt-liber-ticket-marked-p ticket))
		  (add-text-properties start
				       (point)
				       '(face rt-liber-marked-ticket-face))))
	      (newline))
	  (setq filtered-count (1+ filtered-count))))
      (when (< 0 filtered-count)
	(insert (format "%d tickets not shown (filtered)" filtered-count))))))

(defun rt-liber-browser-refresh (&optional _ignore-auto noconfirm)
  (interactive)
  (if rt-liber-query
      (when (or rt-liber-browser-do-refresh
		noconfirm)
	;; explicitly pass nil NEW to `rt-liber-browse-query'
	(rt-liber-browse-query rt-liber-query nil))
    (error "no buffer-local query")))

(defun rt-liber-browser-refresh-and-return ()
  (interactive)
  (let ((id (rt-liber-browser-ticket-id-at-point)))
    (rt-liber-browser-refresh)
    (rt-liber-browser-move-point-to-ticket id)))

;; This is just a special case of
;; `rt-liber-browser-ticket-<PROPERTY>-at-point'
(defun rt-liber-browser-ticket-id-at-point (&optional point)
  "Return the ticket id for the ticket at buffer position.

If POINT is nil then called on (point)."
  (when (not point)
    (setq point (point)))
  (let ((value (rt-liber-ticket-id-only
		(get-text-property point 'rt-ticket))))
    (if value
	value
      (error "no such ticket property at point"))))

(defun rt-liber-ticket-taken-p (ticket-alist)
  "Return t if TICKET-ALIST is owned by Nobody."
  (when (not ticket-alist)
    (error "null argument"))
  (let ((owner (rt-liber-ticket-owner-only ticket-alist)))
    (if (string= owner "Nobody")
	nil
      t)))

(defun rt-liber-next-ticket-in-browser ()
  "Move point to the next ticket."
  (interactive)
  (let ((next (next-single-property-change (point) 'rt-ticket)))
    (when next (goto-char next))))

(defun rt-liber-previous-ticket-in-browser ()
  "Move point to the previous ticket."
  (interactive)
  (let ((prev (previous-single-property-change (point) 'rt-ticket)))
    (when prev (goto-char prev))))

(defun rt-liber-ticket-at-point ()
  "Display the contents of the ticket at point."
  (interactive)
  (funcall rt-liber-display-ticket-at-point-f))

(defun rt-liber-browser-search (id)
  "Return point where ticket with ID is displayed or nil."
  (let ((p nil))
    (save-excursion
      (goto-char (point-min))
      (let ((point-id (rt-liber-ticket-id-only
		       (get-text-property (point) 'rt-ticket))))
	(if
	    ;; (predicate) looks for the exceptional situation
	    (and point-id (string= id point-id))
	    ;; (consequent) we're done
	    (setq p (point))
	  ;; (alternative) continue looking
	  (while (and (not p)
		      (rt-liber-next-ticket-in-browser))
	    (let ((point-id (rt-liber-ticket-id-only
			     (get-text-property (point) 'rt-ticket))))
	      (when (string= id point-id)
		(setq p (point))))))))
    p))

(defun rt-liber-browser-move-point-to-ticket (id)
  "Move point to the beginning of ticket with ID."
  (let ((p (rt-liber-browser-search id)))
    (if p
	(progn
	  (goto-char p)
	  (recenter-top-bottom))
      (error "ticket #%s not found" id))))


;;; --------------------------------------------------------
;;; Ticket browser sorting
;;; --------------------------------------------------------
(defun rt-liber-lex-lessthan-p (a b field)
  "Return t if A is lexicographically less than B in FIELD."
  (let ((field-a (cdr (assoc field a)))
	(field-b (cdr (assoc field b))))
    (if (and field-a field-b)
	(string-lessp field-a field-b)
      (error "\"%s\" is not a valid ticket field" field))))

(defun rt-liber-time-lessthan-p (a b field)
  "Return t if A is chronologically less than B in FIELD."
  (let ((field-a (cdr (assoc field a)))
	(field-b (cdr (assoc field b))))
    (if (and field-a field-b)
	(time-less-p (date-to-time field-a)
		     (date-to-time field-b))
      (error "\"%s\" is not a valid ticket field" field))))

(defun rt-liber-sort-ticket-list (ticket-list sort-f)
  "Return a copy of TICKET-LIST sorted by SORT-F."
  (let ((seq (copy-sequence ticket-list)))
    (sort seq sort-f)))

(defun rt-liber-sort-by-owner (ticket-list)
  "Sort TICKET-LIST lexicographically by owner."
  (rt-liber-sort-ticket-list
   ticket-list
   #'(lambda (a b)
       (rt-liber-lex-lessthan-p
	a b (rt-liber-get-field-string 'owner)))))

(defun rt-liber-sort-by-time-created (ticket-list)
  "Sort TICKET-LIST in reverse chronological order."
  (reverse
   (rt-liber-sort-ticket-list
    ticket-list
    #'(lambda (a b)
	(rt-liber-time-lessthan-p a b "Created")))))


;;; --------------------------------------------------------
;;; Ticket browser filtering
;;; --------------------------------------------------------
;; See the fine manual for example code.

(defun rt-liber-default-filter-f (_ticket)
  "The default filtering function for the ticket browser

This function is really a placeholder for user custom functions,
and as such always return t."
  t)


;;; --------------------------------------------------------
;;; Entry points
;;; --------------------------------------------------------
(defun rt-liber-browse-query (query &optional new)
  "Run QUERY against the server and launch the browser.

NEW if non-nil create additional browser buffer. If NEW is a
string then that will be the name of the new buffer."
  (interactive "Mquery: ")
  (condition-case nil
      (rt-liber-browser-startup
       (rt-liber-rest-run-show-base-query
	(rt-liber-rest-run-ls-query query))
       query new)
    (rt-liber-no-result-from-query-error
     (rt-liber-browser-with-message "no results from query"
				    query new))))

(defun rt-liber-print-query (query &optional ticket-redraw-f)
  "Run QUERY against the server and return a string.

The optional function TICKET-REDRAW-F will be bound to
`rt-liber-custom-ticket-redraw-function' for the duration of the
query output. Note that unlike the browser output, the string
returned as no associated text properties."
  (let ((rt-liber-custom-ticket-redraw-function
	 (or ticket-redraw-f
	     rt-liber-custom-ticket-redraw-function))
	(out ""))
    (condition-case nil
	(with-temp-buffer
	  (rt-liber-ticketlist-browser-redraw
	   (rt-liber-rest-run-show-base-query
	    (rt-liber-rest-run-ls-query query))
	   query)
	  (setq out (buffer-substring-no-properties 1 (- (point-max) 1))))
      (rt-liber-no-result-from-query-error
       (rt-liber-browser-with-message "no results from query"
				      query)))
    out))


;;; --------------------------------------------------------
;;; Major mode definitions
;;; --------------------------------------------------------
(defun rt-liber-browser-mode-quit ()
  "Bury the ticket browser."
  (interactive)
  (bury-buffer))

(defconst rt-liber-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'rt-liber-browser-mode-quit)
    (define-key map (kbd "n") 'rt-liber-next-ticket-in-browser)
    (define-key map (kbd "p") 'rt-liber-previous-ticket-in-browser)
    (define-key map (kbd "RET") 'rt-liber-ticket-at-point)
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "G") 'rt-liber-browser-refresh-and-return)
    (define-key map (kbd "a") 'rt-liber-browser-assign)
    (define-key map (kbd "r") 'rt-liber-browser-resolve)
    (define-key map (kbd "o") 'rt-liber-browser-open)
    (define-key map (kbd "N") 'rt-liber-browser-new)
    (define-key map (kbd "t") 'rt-liber-browser-take-ticket-at-point)
    (define-key map (kbd "A") 'rt-liber-browser-ancillary-text)
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "DEL") 'scroll-down)
    (define-key map (kbd "M") 'rt-liber-mark-ticket-at-point)
    (define-key map (kbd "P") 'rt-liber-browser-prioritize)
    map)
  "Key map for ticket browser.")

(define-derived-mode rt-liber-browser-mode nil
  "RT Liberation Browser"
  "Major Mode for browsing RT tickets.
\\{rt-liber-browser-mode-map}"
  (set (make-local-variable 'revert-buffer-function)
       #'rt-liber-browser-refresh)
  (set (make-local-variable 'buffer-stale-function)
       (lambda (&optional _noconfirm) 'slow))
  (run-hooks 'rt-liber-browser-hook))

(defun rt-liber-setup-browser-name (new)
  (setq rt-liber-browser-buffer
	(get-buffer-create
	 (if new
	     (generate-new-buffer-name
	      (if (stringp new)
		  new
		rt-liber-browser-buffer-name))
	   (if (and (boundp 'rt-liber-query)
		    rt-liber-query)
	       (buffer-name)
	     rt-liber-browser-buffer-name)))))

(defun rt-liber-browser-with-message (message &optional query new)
  "Start the RT ticket browser and display MESSAGE."
  (interactive)
  (rt-liber-setup-browser-name new)
  ;; setup stage (invisible to user)
  (with-current-buffer rt-liber-browser-buffer
    (let ((inhibit-read-only t))
      (rt-liber-browser-mode)
      (goto-char (point-min))
      (rt-liber-ticketlist-browser-redraw nil query)
      (newline 2)
      (insert message)
      (set (make-local-variable 'rt-liber-query) query)))
  ;; display stage (user can see updates)
  (switch-to-buffer rt-liber-browser-buffer)
  (setq buffer-read-only t))

(defun rt-liber-browser-startup (ticket-list &optional query new)
  "Start the RT ticket browser."
  (interactive)
  (rt-liber-setup-browser-name new)
  ;; setup stage (invisible to user)
  (with-current-buffer rt-liber-browser-buffer
    (let ((inhibit-read-only t))
      (rt-liber-ticketlist-browser-redraw ticket-list query)
      (goto-char (point-min))
      (rt-liber-next-ticket-in-browser)
      (rt-liber-browser-mode)
      ;; store the ticket-list and the query which produced the buffer
      ;; as buffer local variables
      (set (make-local-variable 'rt-liber-ticket-list) ticket-list)
      (set (make-local-variable 'rt-liber-query) query)))
  ;; display stage (user can see updates)
  (switch-to-buffer rt-liber-browser-buffer)
  (setq buffer-read-only t))

(defun rt-liber-browser-ancillary-text ()
  "Wrapper function around storage backend."
  (interactive)
  (when (not (featurep 'rt-liberation-storage))
    (error "rt-liberation-storage isn't loaded"))
  (let ((initial-contents ""))
    (rt-liber-set-ancillary-text
     (read-from-minibuffer "Text: " initial-contents))))


;;; --------------------------------------------------------
;;; Command module
;;; --------------------------------------------------------
(defun rt-liber-command-get-dictionary-value (sym dic)
  "Utility function for retrieving alist values."
  (let ((value (cdr (assoc sym dic))))
    (if value
	value
      (error "%s not a key in dictionary %s" sym dic))))

(defun rt-liber-command-get-command-string (command-symbol)
  "Return value associated with key COMMAND-SYMBOL."
  (rt-liber-command-get-dictionary-value
   command-symbol
   rt-liber-command-dictionary))

(defun rt-liber-command-get-status-string (status-symbol)
  "Return value associated with key STATUS-SYMBOL."
  (rt-liber-command-get-dictionary-value
   status-symbol
   rt-liber-status-dictionary))

(defun rt-liber-command-runner-parser-f ()
  "Display command return status from the server to the user."
  (message (buffer-string)))

(defun rt-liber-command-set-status-deleted (id)
  "Set the status of ticket ID to `deleted'."
  (rt-liber-rest-command-set
   id
   (rt-liber-get-field-string 'status)
   (rt-liber-command-get-status-string 'deleted)))

(defun rt-liber-command-set-status-new (id)
  "Set the status of ticket ID to `new'."
  (rt-liber-rest-command-set
   id
   (rt-liber-get-field-string 'status)
   (rt-liber-command-get-status-string 'new)))

(defun rt-liber-command-set-status-resolved (id)
  "Set the status of ticket ID to `resolved'."
  (rt-liber-rest-command-set
   id
   (rt-liber-get-field-string 'status)
   (rt-liber-command-get-status-string 'resolved)))

(defun rt-liber-command-set-status-open (id)
  "Set the status of ticket ID to `open'."
  (rt-liber-rest-command-set
   id
   (rt-liber-get-field-string 'status)
   (rt-liber-command-get-status-string 'open)))

(defun rt-liber-command-set-owner (id new-owner)
  "Set the owner of ticket in TICKET-ALIST to NEW-OWNER."
  (rt-liber-rest-command-set
   id
   (rt-liber-get-field-string 'owner)
   new-owner))

(defun rt-liber-browser-prioritize (n)
  "Assigng current ticket priority N."
  (interactive "nPriority (number): ")
  (rt-liber-rest-command-set
   (rt-liber-browser-ticket-id-at-point)
   (rt-liber-get-field-string 'priority)
   ;; Work around the strangeness of RT. RT doesn't accept "0" as
   ;; string to set priority to 0, but does accept "00".
   (if (< 0 n)
       (format "%s" n)
     "00"))
  (rt-liber-browser-refresh-and-return))

(defun rt-liber-browser-assign (name)
  "Assign current ticket to a user NAME."
  (interactive "sAssign to: ")
  (let ((taken-p (rt-liber-ticket-taken-p
		  (get-text-property (point) 'rt-ticket))))
    (when (or (not taken-p)
	      (and taken-p
		   (y-or-n-p "Ticket already assigned! Are you sure?")))
      (rt-liber-command-set-owner
       (rt-liber-browser-ticket-id-at-point)
       name)
      (rt-liber-browser-refresh-and-return))))

(defun rt-liber-browser-resolve ()
  "Resolve the current ticket."
  (interactive)
  (rt-liber-command-set-status-resolved
   (rt-liber-browser-ticket-id-at-point))
  (rt-liber-browser-refresh-and-return))

(defun rt-liber-browser-open ()
  "Open the current ticket."
  (interactive)
  (rt-liber-command-set-status-open
   (rt-liber-browser-ticket-id-at-point))
  (rt-liber-browser-refresh-and-return))

(defun rt-liber-browser-new ()
  "Change the current ticket's status to `new'."
  (interactive)
  (rt-liber-command-set-status-new
   (rt-liber-browser-ticket-id-at-point))
  (rt-liber-browser-refresh-and-return))

(defun rt-liber-browser-take-ticket-at-point ()
  "Assign the ticket under point to `rt-liber-username'."
  (interactive)
  (when (not rt-liber-username)
    (error "`rt-liber-username' is nil"))
  (rt-liber-browser-assign rt-liber-username))


;;; ------------------------------------------------------------------
;;; viewer mode functions
;;; ------------------------------------------------------------------
(defun rt-liber-viewer-visit-in-browser ()
  "Visit this ticket in the RT Web interface."
  (interactive)
  (let ((id (rt-liber-ticket-id-only rt-liber-ticket-local)))
    (if id
	(browse-url
	 (concat rt-liber-base-url "Ticket/Display.html?id=" id))
      (error "no ticket currently in view"))))

(defun rt-liber-viewer-show-ticket-browser ()
  "Return to the ticket browser buffer."
  (interactive)
  (let ((id (rt-liber-ticket-id-only rt-liber-ticket-local)))
    (if id
	(let ((target-buffer
	       (if rt-liber-assoc-browser
		   (buffer-name rt-liber-assoc-browser)
		 (buffer-name rt-liber-browser-buffer-name))))
	  (if target-buffer
	      (switch-to-buffer target-buffer)
	    (error "associated ticket browser buffer no longer exists"))
	  (rt-liber-browser-move-point-to-ticket id))
      (error "no ticket currently in view"))))


;;; ------------------------------------------------------------------
;;; viewer
;;; ------------------------------------------------------------------
(defface rt-liber-ticket-emph-face
  '((((class color) (background dark))
     (:foreground "gray53"))
    (((class color) (background light))
     (:foreground "gray65"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "Blue")))
  "Face for important text.")

(defconst rt-liber-viewer-font-lock-keywords
  `(("^$" 0 'rt-liber-ticket-subdued-face))
  "Expressions to font-lock for RT ticket viewer.")

(defun rt-liber-viewer-vernacular-plural (time)
  "Add an ess as needed."
  (if (= time 1)
      ""
    "s"))

(defun rt-liber-viewer-vernacular-date (date)
  "Return a vernacular time delta."
  (let* ((now (format-time-string "%Y-%m-%dT%T%z" (current-time)))
	 (days-ago (days-between now date)))
    (cond ((= 0 days-ago)
	   "today")
	  ((< 0 days-ago 7)
	   (format "%s day%s ago" days-ago
		   (rt-liber-viewer-vernacular-plural days-ago)))
	  ((<= 7 days-ago 30)
	   (let ((weeks (floor (/ days-ago 7.0))))
	     (format "%s week%s ago"
		     weeks
		     (rt-liber-viewer-vernacular-plural weeks))))
	  ((<= 30 days-ago 365)
	   (let ((months (floor (/ days-ago 30.0))))
	     (format "%s month%s ago"
		     months
		     (rt-liber-viewer-vernacular-plural months))))
	  (t (let ((years (floor (/ days-ago 365.0))))
	       (format "%s year%s ago"
		       years
		       (rt-liber-viewer-vernacular-plural years)))))))

(defun rt-liber-viewer-mode-quit ()
  "Bury the ticket viewer."
  (interactive)
  (bury-buffer))

(defun rt-liber-viewer-reduce (section-list f acc)
  "A Not Invented Here tail-recursive reduce function."
  (cond ((null (cdr section-list)) acc)
	(t (rt-liber-viewer-reduce (cdr section-list)
				   f
				   (append acc (list
						(funcall f
							 (car section-list)
							 (cadr section-list))))))))

;; According to:
;; "https://rt-wiki.bestpractical.com/wiki/REST#Ticket_History_Entry"
;; id: <history-id>
;; Ticket: <ticket-id>
;; TimeTaken: <...>
;; Type: <...>
;; Field: <...>
;; OldValue: <...>
;; NewValue: <...>
;; Data: <...>
;; Description: <...>

;; Content: <lin1-0>
;;          <line-1>
;;          ...
;;          <line-n>

;; Creator: <...>
;; Created: <...>
;; Attachments: <...>
(defun rt-liber-viewer-parse-section (start end)
  (goto-char start)
  (when (not (re-search-forward
	      rt-liber-viewer-section-header-regexp
	      end t))
    (error "invalid section"))
  (forward-line 2)
  (let (section-field-alist
	(rt-field-list
	 '(id Ticket TimeTaken Type Field
	      OldValue NewValue Data Description
	      Creator Created)))
    ;; definitely error out if any of this doesn't work
    (setq section-field-alist
	  (mapcar
	   (lambda (field-symbol)
	     (re-search-forward (format "^%s:" (symbol-name field-symbol)) end nil)
	     (cons field-symbol (buffer-substring (1+ (point)) (line-end-position))))
	   rt-field-list))
    ;; content
    (goto-char start)
    (let ((content-start (re-search-forward "^Content: " end nil))
	  (content-end (progn
			 (re-search-forward "^Creator: " end nil)
			 (line-beginning-position))))
      (append section-field-alist
	      `(,(cons 'Content
		       (buffer-substring content-start
					 content-end)))))))

;; According to:
;; "https://rt-wiki.bestpractical.com/wiki/REST#Ticket_History" is of
;; the form: "# <n>/<n> (id/<history-id>/total)"
(defun rt-liber-viewer-parse-history (ticket-history)
  "Parse the string TICKET-HISTORY."
  (when (not (stringp ticket-history))
    (error "invalid ticket-history"))
  (with-temp-buffer
    (insert ticket-history)
    (goto-char (point-min))
    ;; find history detail sections and procude a list of section
    ;; (start . end) pairs
    (let (section-point-list
	  section-list)
      (while (re-search-forward rt-liber-viewer-section-header-regexp (point-max) t)
	(setq section-point-list (append section-point-list
					 (list (line-beginning-position)))))
      (when (not section-point-list)
	(error "no history detail sections found"))
      (setq section-point-list (append section-point-list
				       (list (point-max)))
	    section-point-list (rt-liber-viewer-reduce section-point-list #'cons nil))
      ;; collect the sections
      (setq section-list
	    (mapcar
	     (lambda (section-points)
	       (rt-liber-viewer-parse-section
		(car section-points)
		(cdr section-points)))
	     section-point-list))
      section-list)))

(defun rt-liber-viewer-get-section-data ()
  "Return the current section data."
  (let ((section (get-text-property (point) 'rt-liberation-section-data)))
    (when (not section)
      (save-excursion
	(rt-liber-viewer-previous-section-in)
	(setq section (get-text-property (point) 'rt-liberation-section-data))))
    section))

(defun rt-liber-viewer-format-content (content)
  "Wrangle RT's content format."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (if (re-search-forward "^This transaction appears to have no content" (point-max) t)
	"" ; make no content mean... no content
      ;; trim leading blank lines
      (save-excursion
	(goto-char (point-min))
	(re-search-forward "[[:graph:]]" (point-max) t)
	(forward-line -1)
	(flush-lines "^[[:space:]]+$" (point-min) (point)))
      ;; trim trailing blank lines
      (save-excursion
	(goto-char (point-max))
	(re-search-backward "[[:graph:]]" (point-min) t)
	(forward-line 2)
	(flush-lines "^[[:space:]]+$" (point-max) (point)))
      ;; Convert the 9 leading whitespaces from RT's comment lines.
      (goto-char (point-min))
      (insert "    ")
      (while (re-search-forward "^         " (point-max) t)
	(replace-match "    "))
      ;; fill
      ;; (let ((paragraph-separate "    >[[:space:]]+$"))
      ;; 	(fill-region (point-min)
      ;; 		     (point-max)))
      ;; finally
      (buffer-substring (point-min)
			(point-max)))))

(defun rt-liber-viewer-clean-content (section)
  "Format section content for email."
  (with-temp-buffer
    (insert (rt-liber-viewer-format-content
	     (alist-get 'Content section)))
    (goto-char (point-min))
    (while (re-search-forward "^    " (point-max) t)
      (replace-match ""))
    ;; fill
    ;; (let ((paragraph-separate ">[[:space:]]+$"))
    ;;   (fill-region (point-min)
    ;; 		   (point-max)))
    ;; finally
    (buffer-substring (point-min)
		      (point-max))))

(defun rt-liber-viewer-display-section (section)
  (let ((start     (point))
	(ticket-id (alist-get 'Ticket section))
	(creator   (alist-get 'Creator section))
	(date      (alist-get 'Created section))
	(type	   (alist-get 'Type section))
	(desc      (alist-get 'Description section))
	(oldvalue  (alist-get 'OldValue section))
	(newvalue  (alist-get 'NewValue section))
	(s-content (rt-liber-viewer-format-content
		    (alist-get 'Content section))))
    (when (not (or (string= type "CommentEmailRecord")
		   (string= type "EmailRecord")))
      (insert
       (format "Ticket %s by %s, %s (%s) [%s]\n"
	       ticket-id
	       creator
	       (rt-liber-viewer-vernacular-date date)
	       date
	       (format "%s%s%s"
		       type
		       (if (< 0 (length oldvalue))
			   (concat " " oldvalue)
			 "")
		       (if (< 0 (length newvalue))
			   (concat "->" newvalue)
			 ""))))
      (add-text-properties start
			   (point)
                           `(font-lock-face rt-liber-ticket-emph-face))
      (add-text-properties start
			   (point)
                           `(rt-liberation-viewer-header t))
      (add-text-properties start
			   (point)
			   `(rt-liberation-section-data ,section))
      (cond ((or (string= type "Status")
		 (string= type "SetWatcher")
		 (string= type "Set")
		 (string= type "AddLink"))
	     (insert
	      (format "\n    %s\n\n" desc)))
	    ;; catch-all
	    (t
	     (insert
	      (format "\n%s\n" s-content)))))))

(defun rt-liber-viewer-display-history (contents)
  (let ((section-list (rt-liber-viewer-parse-history contents)))
    (mapc
     (lambda (section)
       (rt-liber-viewer-display-section section))
     section-list)))

(defun rt-liber-viewer-display-ticket-at-point ()
  "Display the contents of the ticket at point."
  (interactive)
  (let ((ticket-alist (get-text-property (point) 'rt-ticket)))
    (rt-liber-viewer-display-ticket-history ticket-alist (current-buffer))))

(defun rt-liber-viewer-display-ticket-history (ticket-alist &optional assoc-browser)
  "Display history for ticket.
TICKET-ALIST alist of ticket data.
ASSOC-BROWSER if non-nil should be a ticket browser."
  (let* ((ticket-id (rt-liber-ticket-id-only ticket-alist))
	 (contents (rt-liber-rest-run-ticket-history-base-query ticket-id))
	 (new-ticket-buffer (get-buffer-create
			     (concat "*RT (Viewer) Ticket #" ticket-id "*"))))
    (with-current-buffer new-ticket-buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(rt-liber-viewer-display-history contents)
	(goto-char (point-min))
	(rt-liber-viewer-mode)
	(set
	 (make-local-variable 'rt-liber-ticket-local)
	 ticket-alist)
	(when assoc-browser
	  (set
	   (make-local-variable 'rt-liber-assoc-browser)
	   assoc-browser))
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)))
    (switch-to-buffer new-ticket-buffer)))

(defun rt-liber-viewer-refresh-ticket-history (&optional _ignore-auto _noconfirm)
  (interactive)
  (if rt-liber-ticket-local
      (rt-liber-viewer-display-ticket-history rt-liber-ticket-local
					       rt-liber-assoc-browser)
    (error "not viewing a ticket")))

(defun rt-liber-viewer-move-point-to-section (history-id)
  "Move point to the beginning of section with HISTORY-ID."
  (let ((current-history-id (alist-get 'id (rt-liber-viewer-get-section-data)))
	(previous-history-id nil))
    (while (not (or (string-equal history-id current-history-id)
		    (eq current-history-id previous-history-id)))
      (setq previous-history-id current-history-id)
      (rt-liber-viewer-next-section-in)
      (setq current-history-id (alist-get 'id (rt-liber-viewer-get-section-data))))
    (when (not (string-equal history-id current-history-id))
      (error "Cannot find section."))))

(defun rt-liber-viewer-next-section-in ()
  (interactive)
  (when (looking-at rt-liber-viewer-section-regexp)
    (goto-char (1+ (point))))
  (let ((next (re-search-forward rt-liber-viewer-section-regexp
				 (point-max)
				 t)))
    (cond ((not next)
	   (message "no next section"))
	  (t
	   (recenter rt-liber-viewer-recenter)))
    (goto-char (line-beginning-position))))

(defun rt-liber-viewer-last-communicate-in ()
  (interactive)
  (revert-buffer)
  (goto-char (point-max))
  (let ((last (re-search-backward
	       rt-liber-viewer-communicate-regexp
	       (point-min)
	       t)))
    (if (not last)
	(error "no communcations found")
      (recenter rt-liber-viewer-recenter)
      (goto-char (line-beginning-position)))))

(defun rt-liber-viewer-previous-section-in ()
  (interactive)
  (when (looking-at rt-liber-viewer-section-regexp)
    (goto-char (1- (point))))
  (let ((prev (re-search-backward
	       rt-liber-viewer-section-regexp
	       (point-min)
	       t)))
    (cond ((not prev)
	   (message "no previous section"))
	  (t
	   (recenter rt-liber-viewer-recenter)))
    (goto-char (line-beginning-position))))


(defconst rt-liber-viewer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'rt-liber-viewer-mode-quit)
    (define-key map (kbd "N") 'rt-liber-viewer-last-communicate-in)
    (define-key map (kbd "n") 'rt-liber-viewer-next-section-in)
    (define-key map (kbd "p") 'rt-liber-viewer-previous-section-in)
    (define-key map (kbd "V") 'rt-liber-viewer-visit-in-browser)
    (define-key map (kbd "M") 'rt-liber-gnus-viewer-answer)
    (define-key map (kbd "C") 'rt-liber-gnus-viewer-comment)
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "DEL") 'scroll-down)
    (define-key map (kbd "h") 'rt-liber-viewer-show-ticket-browser)
    map)
  "Key map for ticket viewer.")

(define-derived-mode rt-liber-viewer-mode nil
  "RT Liberation Viewer"
  "Major Mode for viewing RT tickets.
\\{rt-liber-viewer-mode-map}"
  (set
   (make-local-variable 'font-lock-defaults)
   '((rt-liber-viewer-font-lock-keywords) t))
  (set (make-local-variable 'revert-buffer-function)
       #'rt-liber-viewer-refresh-ticket-history)
  (set (make-local-variable 'buffer-stale-function)
       (lambda (&optional _noconfirm) 'slow))
  (run-hooks 'rt-liber-viewer-hook))

(defun rt-liber-display-ticket (ticket-number)
  "Display ticket with TICKET-NUMBER."
  (interactive "nticket number: ")
  (let ((ticket-id (number-to-string ticket-number)))
    (rt-liber-browse-query
     (rt-liber-compile-query
      (id ticket-id))
     (concat "#" ticket-id))))

;; for use in macro `rt-liber-compile-query'
(eval-and-compile
  (defun rt-liber-reduce-op (op seq)
    "Simple reduction function for ticket IDs."
    (concat "Id = "
	    (format "'%s'" (car seq))
	    (rt-liber-reduce-op-int op (cdr seq) "")))

  (defun rt-liber-reduce-op-int (op seq acc)
    "Simple reduction function for ticket IDs (internal)."
    (cond ((not seq) acc)
	  (t (rt-liber-reduce-op-int
	      op
	      (cdr seq)
	      (concat acc " " op " Id = " (format "'%s'" (car seq))))))))

(defun rt-liber-display-ticket-list (que ticket-id-list &optional buffer-name)
  "Display from QUEUE the tickets TICKET-ID-LIST.

BUFFER-NAME: optionally name the resultant browser buffer."
  (rt-liber-browse-query
   (rt-liber-compile-query
    (and (queue que)
	 ((or (rt-liber-reduce-op "OR" ticket-id-list)))))
   buffer-name))


(provide 'rt-liberation)

;;; rt-liberation.el ends here.
