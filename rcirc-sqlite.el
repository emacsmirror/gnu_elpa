;;; rcirc-sqlite.el --- rcirc logging in SQLite      -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Matto Fransen <matto@matto.nl>
;; Maintainer: Matto Fransen <matto@matto.nl>
;; Url: https://codeberg.org/mattof/rcirc-sqlite
;; Version: 0.1.3
;; Keywords: comm
;; Package-Requires: ((emacs "30.0"))

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

;; Stores the logging from rcirc into a SQLite database.
;; rcirc-sqlite overrides the rcirc-log-write function.
;;
;; rcirc is a default, simple IRC client in Emacs.
;; rcirc can be enabled to log the IRC chats, it logs to files.
;; This minor mode, when activated, diverts the rcirc
;; logs to a SQLite database.
;;
;;;; Usage:
;;
;; To toggle the mode:
;;   M-x rcirc-sqlite-log-mode
;; To start rcirc-sqlite automatically in `rcirc', add the following
;; to your init file:
;;   (add-hook 'rcirc-mode-hook #'rcirc-sqlite-log-mode)
;;
;;;; Customization:
;;
;; To customize various options, including the file to hold the
;; SQLite database:
;;   M-x customize-group rcirc-sqlite RET

;;; Code:

(defvar rcirc-log-alist)
(defvar rcirc-log-time-format)

(defgroup rcirc-sqlite nil
  "Rcirc logging in SQLite."
  :prefix "rcirc-sqlite-"
  :link '(custom-manual "(rcirc-sqlite)Top")
  :group 'rcirc)

(defcustom rcirc-sqlite-database
  (locate-user-emacs-file "rcirc-log.db")
  "Path to the SQLite database used for logging messages."
  :type 'file)
(defcustom rcirc-sqlite-time-format "%Y-%m-%d %H:%M"
  "Describes how timestamps are displayed in the log buffer."
  :type 'string)

(defcustom rcirc-sqlite-rows "200"
  "Default maximum number of lines displayed in view log."
  :type 'string)

(defcustom rcirc-sqlite-channel-column-width 30
  "Default width of the column displaying the channelname."
  :type 'natnum)

(defvar rcirc-sqlite--conn nil
  "Variable to store the current database connection.
If non-nil, it will designate a object satisfying `sqlitep',
otherwise no connection has been opened.")

(defvar rcirc-sqlite-drill-down-method nil
  "Variable to store how to drill down from the stats.")

(defun rcirc-sqlite--conn ()
  "Return an open database connection, or open one up."
  (or rcirc-sqlite--conn
      (setq rcirc-sqlite--conn
            (let ((conn (sqlite-open rcirc-sqlite-database)))
              (and (sqlitep conn) conn)))
      (error "Failed to open a database connection")))

(defun rcirc-sqlite-create-db (db)
  "Create the table in SQLite database DB, if needed."
  (sqlite-execute db
                  "CREATE VIRTUAL TABLE IF NOT EXISTS 'rcirclogs' USING
fts5(channel, time, nick, message)"))

(defun rcirc-sqlite-set-log-time-format (fn &rest args)
  "Advise to create parsable time format.
This is advised to run around rcirc-log (FN) in rcirc.  After setting
the time format, FN (rcirc-log) is called with ARGS as it's arguments.
This changes the `rcirc-log-time-format' into the Unix timestamp format.
?\x0 is used as delimiter after the timestamp."
  (let ((rcirc-log-time-format "%s\x0"))
    (apply fn args)))

(defun rcirc-sqlite-store-log ()
  "Save the loglines into SQLite.
Fetches the loglines cached in `rcirc-log-alist'.
This function overrides `rcirc-log-write' from rcirc.el.
?\x0 is used as delimiter after the timestamp."
  (let ((db (rcirc-sqlite--conn)))
    (rcirc-sqlite-create-db db)
    (dolist (cell rcirc-log-alist)
      (dolist (logline (split-string (cdr cell) "\n"))
	(unless (string-empty-p logline)
	  (save-match-data
	    (and (string-match "\\`\\([0-9]+?\\)?\x0<\\([^@]+?\\)> \\([^@]+\\)\\'"
			       logline)
		 (sqlite-execute db
				 "INSERT INTO rcirclogs (channel, time, nick, message)
VALUES (?,?,?,?)"
				 (list (car cell)
				       (match-string 1 logline)
				       (match-string 2 logline)
				       (match-string 3 logline))))))))
    (setq rcirc-log-alist nil)))

(defun rcirc-sqlite-db-query-channels ()
  "List the channels from the SQLite database."
  (let ((db (rcirc-sqlite--conn)))
    (sqlite-select db "SELECT DISTINCT channel FROM rcirclogs")))

(defun rcirc-sqlite-db-query-nicks ()
  "List the nicks from the SQLite database."
  (let ((db (rcirc-sqlite--conn)))
    (sqlite-select db "SELECT DISTINCT nick FROM rcirclogs")))

(defun rcirc-sqlite-db-query-months ()
  "List the year/months from the SQLite database."
  (let ((db (rcirc-sqlite--conn)))
    (sqlite-select db "SELECT DISTINCT strftime('%Y-%m', time, 'unixepoch')
FROM rcirclogs")))

(defun rcirc-sqlite-db-query-stats (arg-list)
  "List the number of rows per channel.
ARG-LIST is a list with the requested nick and/or channel.
`rcirc-sqlite-drill-down-method' defines the next step (drilldown)."
  (let ((db (rcirc-sqlite--conn))
	(column "channel")
	(dimension "messages")
	(from "")
	(dbdata ()))
    (setq rcirc-sqlite-drill-down-method arg-list)
    (cond ((string= (car arg-list) "Nicks per channel")
	   (setq dimension "nicks")
	   (push "channel" rcirc-sqlite-drill-down-method)
	   (setq from "(SELECT channel, nick FROM rcirclogs GROUP BY channel,nick)"))
	  ((string= (car arg-list) "Channels per nick")
	   (setq column "nick")
	   (setq dimension "channels")
	   (push "nick" rcirc-sqlite-drill-down-method)
	   (setq from "(SELECT nick, channel FROM rcirclogs GROUP BY nick, channel)"))
	  ((string= (car arg-list) "Channel")
	   (setq column "nick")
	   (setq dimension "messages")
	   (setq from "rcirclogs where channel = ?")
	   (setq dbdata (cdr arg-list)))
	  ((string= (car arg-list) "All nicks")
	   (push "channel" rcirc-sqlite-drill-down-method)
	   (setq from "rcirclogs"))
	  (t
	   (setq from "rcirclogs where nick = ?")
	   (push "channel" rcirc-sqlite-drill-down-method)
	   (push (car arg-list) dbdata)
	   (setq dimension "messages")))
    (let ((dbquery (format "SELECT %s, COUNT() || ' %s' FROM %s GROUP BY %s ORDER BY %s"
			   column dimension from column column)))
      (sqlite-select db dbquery dbdata))))

(defun rcirc-sqlite-db-query-log (arg-list)
  "Fetch the last N rows of the logs from a specific channel.
N is defined in `rcirc-sqlite-rows' and is default 200.
The user can opt for no limit, or a different limit and offset.
ARG-LIST is a list build from channel, period (year-month), unlimited,
offset and limit."
  (let ((db (rcirc-sqlite--conn))
	(dbquery "SELECT * FROM rcirclogs")
	(dbdata ()))
    (pcase-let ((`(,channel ,when ,unlimited ,offset ,limit) arg-list))
      (unless (string= channel "All channels")
	(setq dbquery (concat dbquery " WHERE channel=?"))
	(push channel dbdata))
      (unless (string= when "Anytime")
	(if (string= channel "All channels")
	    (setq dbquery (concat dbquery " WHERE "))
	  (setq dbquery (concat dbquery " AND ")))
	(setq dbquery (concat dbquery "strftime('%Y-%m', time, 'unixepoch')=?"))
	(push when dbdata))
      (unless unlimited
	(if limit
	    (progn
	      (setq dbquery (concat dbquery " ORDER BY time ASC LIMIT ?,?"))
	      (push offset dbdata)
	      (push limit dbdata))
	  (setq dbquery (concat "SELECT * FROM (" dbquery
				(format " ORDER BY time DESC LIMIT %s) ORDER BY time ASC"
					rcirc-sqlite-rows)))))
      (sqlite-execute db dbquery (reverse dbdata)))))

(defun rcirc-sqlite-db-search-log (arg-list)
  "Perform a full text search.
ARG-LIST describes the search argument and possibly a specific
channel, month and/or nick to narrow the search to."
  (let ((db (rcirc-sqlite--conn))
	(dbquery "SELECT * FROM rcirclogs WHERE rcirclogs=?"))
    (pcase-let ((`(,query ,channel ,when ,nick) arg-list))
      (let ((dbdata (list query)))
	(unless (string= channel "All channels")
	  (setq dbquery (concat dbquery " AND channel=?"))
	  (push channel dbdata))
	(unless (string= when "Anytime")
	  (setq dbquery (concat dbquery " AND strftime('%Y-%m', time, 'unixepoch')=?"))
	  (push when dbdata))
	(unless (string= nick "All nicks")
	  (setq dbquery (concat dbquery " AND nick=?"))
	  (push nick dbdata))
	(setq dbquery (concat dbquery " ORDER BY rank"))
	(sqlite-execute db dbquery
			(reverse dbdata))))))

(defun rcirc-sqlite-db-drilldown (arg-list)
  "Drill down to messages per nick or channel.
ARG-LIST defines which records to select."
  (let ((db (rcirc-sqlite--conn))
	(dbquery "SELECT * FROM rcirclogs WHERE ")
	(dbdata ()))
    (pcase-let ((`(,what ,where ,nick) arg-list))
      (cond
       ((string= nick "Channels per nick")
	(setq dbquery (concat dbquery "nick=?"))
	(push what dbdata))
       ((string= nick "All nicks")
	(setq dbquery (concat dbquery "channel=?"))
	(push what dbdata))
       (t
	(setq dbquery (concat dbquery (format "%s=? and nick=?" where)))
	(push nick dbdata)
	(push what dbdata)))
      (sqlite-execute db dbquery dbdata))))
    
(defun rcirc-sqlite-convert-tabulation-list (list-to-convert)
  "Convert LIST-TO-CONVERT to format for `tabulated-list-mode'.
Build a vector from the data in LIST-TO-CONVERT and format the
timestamp."
  (unless (listp list-to-convert)
    (error "No data available"))
      (mapcar (lambda (cell)
		(list  nil (vector (nth 0 cell)
				   (format-time-string rcirc-sqlite-time-format
						       (string-to-number (nth 1 cell)))
				   (nth 2 cell)
				   (nth 3 cell))))
              list-to-convert))

(defun rcirc-sqlite-convert-two-column-tabulation-list (list-to-convert)
  "Convert LIST-TO-CONVERT to format for `tabulated-list-mode'.
Build a vector from the data, converting numbers to text, because
`tabulated-list-mode' can't handle numbers."
  (unless (listp list-to-convert)
    (error "No data available"))
  (if (numberp (cadr (car list-to-convert)))
      (mapcar (lambda (cell)
		(list (car cell) (vector (car cell)
					 (number-to-string (cadr cell)))))
              list-to-convert)
    (mapcar (lambda (cell)
              (list (car cell) (vector (car cell) (cadr cell))))
            list-to-convert)))

(define-derived-mode rcirc-sqlite-list-mode tabulated-list-mode
  "rcirc-sqlite-list-mode"
  "Major mode Rcirc-SQLite, to display tabulated db queries."
  (setq tabulated-list-format
	(vector (list "Channel" rcirc-sqlite-channel-column-width t)
		(list "Time"
		      (length (format (format-time-string rcirc-sqlite-time-format
							  (current-time)))) t)
		(list "Nick" 16 t)  ;; max length on Libera Chat,
		;; see also https://modern.ircdocs.horse/#userlen-parameter
		(list "Message" 0 t)))
  (tabulated-list-init-header))

(defvar-keymap rcirc-sqlite-two-column-mode-map
  "RET" #'rcirc-sqlite-view-drill-down
  "<down-mouse-1>" #'rcirc-sqlite-view-drill-down)

(define-derived-mode rcirc-sqlite-two-column-mode tabulated-list-mode
  "rcirc-sqlite-two-column-mode"
  "Major mode Rcirc-SQLite, to display two-column tabulated db queries."
  (setq tabulated-list-format
	(vector (list "Channel/Nick" rcirc-sqlite-channel-column-width t)
		(list "Data" 0 t)))
  (tabulated-list-init-header))

(defun rcirc-sqlite-display-tabulation-list (identstr with-function
						      &optional arg-list)
  "Display data in tabulated format in a new buffer.
Retreive data using WITH-FUNCTION, optionally with additional arguments
in ARG-LIST, IDENTSTR explains the current query through the mode-line."
  (with-current-buffer (get-buffer-create "*rcirc log*")
    (rcirc-sqlite-list-mode)
    (setq tabulated-list-entries
          (rcirc-sqlite-convert-tabulation-list (funcall with-function arg-list)))
    (tabulated-list-print t)
    (display-buffer (current-buffer))
    (setq mode-line-buffer-identification identstr)
    (force-mode-line-update)))

(defun rcirc-sqlite-display-two-column-tabulation-list
    (identstr with-function &optional arg-list)
  "Display data in tabulated format in a new buffer.
Retreive data using WITH-FUNCTION, optionally with additional
arguments in ARG-LIST.  IDENTSTR explains which stat is shown."
  (with-current-buffer (get-buffer-create "*rcirc log*")
    (rcirc-sqlite-two-column-mode)
    (setq tabulated-list-entries
          (rcirc-sqlite-convert-two-column-tabulation-list
	   (funcall with-function arg-list)))
    (tabulated-list-print t)
    (display-buffer (current-buffer))
    (setq mode-line-buffer-identification identstr)
    (force-mode-line-update)))

(defun rcirc-sqlite-view-drill-down-final ()
  "Last step in drill-down, query individual messages."
    (rcirc-sqlite-display-tabulation-list
     (format "Drill-down (%s %s)"
	     (nth 0 rcirc-sqlite-drill-down-method)
	     (nth 2 rcirc-sqlite-drill-down-method))
     #'rcirc-sqlite-db-drilldown rcirc-sqlite-drill-down-method))

(defun rcirc-sqlite-view-drill-down ()
  "Show messages per nick or channel.
Called from `rcirc-sqlite-two-column-mode'."
(interactive nil rcirc-sqlite-two-column-mode)
(cond
 ((string= (nth 1 rcirc-sqlite-drill-down-method) "Nicks per channel")
  (let ((arg-list (list "Channel" (tabulated-list-get-id))))
    (rcirc-sqlite-display-two-column-tabulation-list
     (format "Stats (%s)" (tabulated-list-get-id))
     #'rcirc-sqlite-db-query-stats arg-list)))
 ((string= (nth 0 rcirc-sqlite-drill-down-method) "Channel")
  (setq rcirc-sqlite-drill-down-method
	(list (nth 1 rcirc-sqlite-drill-down-method)
	      "channel"
	      (tabulated-list-get-id)))
  (rcirc-sqlite-view-drill-down-final))
 (t
  (push (tabulated-list-get-id) rcirc-sqlite-drill-down-method)
  (rcirc-sqlite-view-drill-down-final))))

(defun rcirc-sqlite-select-channel ()
  "Provide completion to select a channel."
  (completing-read
   "Select a channel (use completion): "
   (append (list "All channels") (rcirc-sqlite-db-query-channels))))

(defun rcirc-sqlite-select-nick (wild-card-values)
  "Provide completion to select a nick.
Extend the list of nicks with WILD-CARD-VALUES to offer the
user more choices."
  (completing-read
   "Select a nick (use completion): "
   (append wild-card-values (rcirc-sqlite-db-query-nicks))))

(defun rcirc-sqlite-select-month (wild-card-values)
  "Provide completion to select a year and month.
Extend the list of months with WILD-CARD-VALUES to offer the
user more choices."
  (completing-read
   "Select a month (use completion): "
   (append wild-card-values (rcirc-sqlite-db-query-months))))

(defun rcirc-sqlite-view-log (channel when &optional unlimited offset limit)
  "View the logs of a specific CHANNEL.
WHEN is either `Anytime' or a specific month.
Shows the result in a new buffer.
When called without OFFSET and LIMIT, show the last 200 rows.
When called with non-nil UNLIMITED, show all the rows.
Otherwise offset and limit are used; in that case  both offset
and limit have to be provided."
  (interactive (list (rcirc-sqlite-select-channel)
		     (rcirc-sqlite-select-month (list "Anytime"))))
  (let ((searcharg-list (list channel when unlimited offset limit)))
    (rcirc-sqlite-display-tabulation-list
     (format "View log (%s %s)" channel when)
     #'rcirc-sqlite-db-query-log searcharg-list)))

(defun rcirc-sqlite-text-search (query channel when nick)
  "Perform full text search for QUERY.
WHEN is either `Anytime' or a specific month, to narrow the search.
Optional narrow search in a specific CHANNEL and/or with a specific NICK.
The results are displayed a new buffer."
  (interactive (list (read-string "Search for: ")
                     (rcirc-sqlite-select-channel)
		     (rcirc-sqlite-select-month (list "Anytime"))
		     (rcirc-sqlite-select-nick (list "All nicks"))))
  (let ((searcharg-list (list query channel when nick)))
    (rcirc-sqlite-display-tabulation-list
     (format "Search %s (%s %s %s)" query channel when nick)
     #'rcirc-sqlite-db-search-log searcharg-list)))

(defun rcirc-sqlite-stats (nick)
  "Display overview of the number of rows per channel.
Optionally narrow to a specific NICK.
The results are displayed a new buffer."
  (interactive (list (rcirc-sqlite-select-nick (list "All nicks"
						     "Nicks per channel"
						     "Channels per nick"))))
  (let ((searcharg-list (list nick)))
    (rcirc-sqlite-display-two-column-tabulation-list
     (format "Stats (%s)" nick)
     #'rcirc-sqlite-db-query-stats searcharg-list)))

;;;###autoload
(define-minor-mode rcirc-sqlite-log-mode
  "Log messages to a SQLite database."
  :global t
  (unless (sqlite-available-p)
    (error "SQLite support not available"))
  (if rcirc-sqlite-log-mode
      (progn
	(advice-add 'rcirc-log :around #'rcirc-sqlite-set-log-time-format)
	(advice-add 'rcirc-log-write :override #'rcirc-sqlite-store-log))
    (progn
      (advice-remove 'rcirc-log-write #'rcirc-sqlite-store-log)
      (advice-remove 'rcirc-log #'rcirc-sqlite-set-log-time-format))))
  
(provide 'rcirc-sqlite)

;;; rcirc-sqlite.el ends here
