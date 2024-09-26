;;; rcirc-sqlite.el --- rcirc logging in SQLite      -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Matto Fransen <matto@matto.nl>
;; Maintainer: Matto Fransen <matto@matto.nl>
;; Url: https://codeberg.org/mattof/rcirc-sqlite
;; Version: 1.0.3
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
;; Full instructions can be found in the rcirc-sqlite info manual.
;; Evaluate:
;;
;;   (info "(rcirc-sqlite) Top")
;;
;; or visit `https://elpa.gnu.org/packages/doc/rcirc-sqlite.html'
;; for the online manual.

;;;; Customization:
;;
;; To customize various options, including the file to hold the
;; SQLite database:
;;   M-x customize-group rcirc-sqlite RET

;;; News:

;; Version 1.0.3 - 2024-09-26

;; * New command: rcirc-sqlite-stats-per-month
;;   Display overview of the number of rows per month.
;;   Drill down to number of rows per day, and then
;;   drill down to messages of that day.

;; Version 1.0.2 - 2024-06-06

;; * Toggle the display of the server in the channel name
;;   Use the keys `(' and `)' to suppress or activate the display
;;   of the server in the channel name.

;; Version 1.0.1 - 2024-04-22

;; * New custom option:  rcirc-sqlite-register
;;   Register to store messages (default: register `r').
;;
;; * Quickly change the view of the logs
;;   When exploring your logs, change the view with just one key:
;;   - Show all the logs of a channel for a single day or two days.
;;   - Show all the logs of a channel from a specific nick for a single day.
;;
;; * Collect individual messages with just one key:
;;   - Select and copy a message nicely formatted to the `kill-ring'.
;;   - Collect one or more messages in a register.

;; Version 1.0.0 - 2024-04-16

;; * Narrow queries to time range
;;   Use completion to narrow queries to last 90 days, 60 days,
;;   30 days, 7 days or a manually selected time range.
;;
;; * Show logs from a specific nick
;;   New command: M-x rcirc-sqlite-logs-from-nick
;;   Show the logs from a specific nick

;;; Code:

(require 'org) ;; needed for org-read-date

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

(defcustom rcirc-sqlite-register ?r
  "Register to store messages."
  :type 'character)

(defvar rcirc-sqlite--conn nil
  "Variable to store the current database connection.
If non-nil, it will designate a object satisfying `sqlitep',
otherwise no connection has been opened.")

(defvar rcirc-sqlite-drill-down-method nil
  "Variable to store how to drill down from the stats.")

(defvar rcirc-sqlite-shorten-channel nil
  "Variable to store how channel name should be printed.")

(defvar rcirc-sqlite-dbquery ""
  "Variable to store the latest query.")

(defvar rcirc-sqlite-dbdata ""
  "Variable to store the latest query data.")

(defvar rcirc-sqlite-db-statdata ""
  "Variable to store the latest stats-query data.")

(defconst rcirc-sqlite-all-channels "All channels")
(defconst rcirc-sqlite-all-nicks "All nicks")
(defconst rcirc-sqlite-nicks-per-channel "Nicks per channel")
(defconst rcirc-sqlite-channels-per-nick "Channels per nick")
(defconst rcirc-sqlite-anytime "Anytime")
(defconst rcirc-sqlite-last-90-days "Last 90 days")
(defconst rcirc-sqlite-last-60-days "Last 60 days")
(defconst rcirc-sqlite-last-30-days "Last 30 days")
(defconst rcirc-sqlite-last-07-days "Last 7 days")
(defconst rcirc-sqlite-manually-select "Manually select range")

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
This is advised to run around `rcirc-log' (FN) in rcirc.  After setting
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
	    (and (string-match
                  "\\`\\([0-9]+?\\)?\x0<\\([^@]+?\\)> \\([^@]+\\)\\'"
		  logline)
		 (sqlite-execute db
				 "INSERT INTO rcirclogs
   (channel, time, nick, message) VALUES (?,?,?,?)"
				 (list (car cell)
				       (match-string 1 logline)
				       (match-string 2 logline)
				       (match-string 3 logline))))))))
    (setq rcirc-log-alist nil)))

(defun rcirc-sqlite-db-row-data (rowid)
  "Select the key data from the database.
ROWID defines the record.  Use SQLite `start of day' to prevent time
zone errors during time conversions and calculations."
  (let ((db (rcirc-sqlite--conn)))
    (sqlite-select db
                   "SELECT channel, time, nick, message,
unixepoch(datetime(time, 'unixepoch','start of day'))
FROM rcirclogs WHERE rowid=?"
		   (list rowid))))

(defun rcirc-sqlite-db-query-channels ()
  "List the channels from the SQLite database."
  (let ((db (rcirc-sqlite--conn)))
    (sqlite-select db "SELECT DISTINCT channel FROM rcirclogs")))

(defun rcirc-sqlite-db-query-nicks ()
  "List the nicks from the SQLite database."
  (let ((db (rcirc-sqlite--conn)))
    (sqlite-select db "SELECT DISTINCT nick FROM rcirclogs")))

(defun rcirc-sqlite-db-query-monthly-count (&optional arg)
  "List the count per month from the SQLite database.
ARG is a month, in the format YYYY-MM."
  (let ((db (rcirc-sqlite--conn))
        (dbquery
         "SELECT (strftime( '%Y-%m', time, 'unixepoch')) AS month, count(*)
FROM rcirclogs GROUP BY month ORDER BY month"))
    (when arg
      (setq dbquery
            (concat
             "SELECT (strftime( '%Y-%m-%d', time, 'unixepoch')) AS day, count(*)
FROM rcirclogs WHERE (strftime( '%Y-%m', time, 'unixepoch')) = "
             (format "'%s'" arg)
             " GROUP BY day ORDER BY day")))
    (sqlite-select db dbquery)))

(defun rcirc-sqlite-db-query-nick (arg-list)
  "Fetch the logs from a specific nick.
ARG-LIST is a list build from the nick and a time range."
  (let ((db (rcirc-sqlite--conn))
	(dbquery "SELECT rowid, * FROM rcirclogs")
	(dbdata ()))
    (pcase-let ((`(,nick ,when) arg-list))
      (unless (string= nick "All nicks")
	(setq dbquery (concat dbquery " WHERE nick=?"))
	(push nick dbdata))
      (unless (= (car when) 0)
	(if (string= nick "All nicks")
	    (setq dbquery (concat dbquery " WHERE "))
	  (setq dbquery (concat dbquery " AND ")))
	(setq dbquery (concat dbquery
			      (rcirc-sqlite-create-period-selectstring when)))
	(push (car when) dbdata)
	(when (> (cdr when) 0)
	  (push (cdr when) dbdata)))
      (setq rcirc-sqlite-dbquery dbquery
	     rcirc-sqlite-dbdata (reverse dbdata))
      (sqlite-execute db dbquery (reverse dbdata)))))

(defun rcirc-sqlite-db-query-log (arg-list)
  "Fetch the last N rows of the logs from a specific channel.
N is defined in `rcirc-sqlite-rows' and is default 200.
The user can opt for no limit, or a different limit and offset.
ARG-LIST is a list build from channel, time range, unlimited,
offset and limit."
  (let ((db (rcirc-sqlite--conn))
	(dbquery "SELECT rowid, channel, time, nick, message FROM rcirclogs")
	(dbdata ()))
    (pcase-let ((`(,channel ,when ,unlimited ,offset ,limit) arg-list))
      (unless (string= channel rcirc-sqlite-all-channels)
	(setq dbquery (concat dbquery " WHERE channel=?"))
	(push channel dbdata))
      (unless (= (car when) 0)
	(if (string= channel rcirc-sqlite-all-channels)
	    (setq dbquery (concat dbquery " WHERE "))
	  (setq dbquery (concat dbquery " AND ")))
	(setq dbquery (concat dbquery
			      (rcirc-sqlite-create-period-selectstring when)))
	(push (car when) dbdata)
	(when (> (cdr when) 0)
	  (push (cdr when) dbdata)))
      (unless unlimited
	(if limit
	    (progn
	      (setq dbquery (concat dbquery " ORDER BY time ASC LIMIT ?,?"))
	      (push offset dbdata)
	      (push limit dbdata))
	  (setq dbquery (concat "SELECT * FROM (" dbquery
				(format
                                 " ORDER BY time DESC LIMIT %s) ORDER BY time ASC"
				 rcirc-sqlite-rows)))))
      (setq rcirc-sqlite-dbquery dbquery
	    rcirc-sqlite-dbdata (reverse dbdata))
      (sqlite-execute db dbquery (reverse dbdata)))))

(defun rcirc-sqlite-db-search-log (arg-list)
  "Perform a full text search.
ARG-LIST describes the search argument and possibly a specific
channel, time range, and/or nick to narrow the search to."
  (let ((db (rcirc-sqlite--conn))
	(dbquery "")
        (dbdata nil))
    (pcase-let ((`(,query ,channel ,when ,nick) arg-list))
      (when query
        (setq dbquery "rcirclogs=?")
        (push query dbdata))
      (unless (string= channel rcirc-sqlite-all-channels)
        (when (not (string-empty-p dbquery))
          (setq dbquery (concat dbquery " AND ")))
	(setq dbquery (concat dbquery "channel=?"))
	(push channel dbdata))
      (unless (= (car when) 0)
        (when (not (string-empty-p dbquery))
          (setq dbquery (concat dbquery " AND ")))
	(setq dbquery (concat dbquery
			      (rcirc-sqlite-create-period-selectstring
                               when)))
	(push (car when) dbdata)
	(when (> (cdr when) 0)
	  (push (cdr when) dbdata)))
      (unless (string= nick rcirc-sqlite-all-nicks)
        (when (not (string-empty-p dbquery))
          (setq dbquery (concat dbquery " AND ")))
        (setq dbquery (concat dbquery "nick=?"))
	(push nick dbdata))
      (setq dbquery
            (concat
             "SELECT rowid, channel, time, nick, message FROM rcirclogs WHERE "
             dbquery))
      (setq dbquery (concat dbquery " ORDER BY rank, time"))
      (setq rcirc-sqlite-dbquery dbquery
	    rcirc-sqlite-dbdata (reverse dbdata))
      (sqlite-execute db dbquery
		      (reverse dbdata)))))

(defun rcirc-sqlite-db-refresh ()
  "Repeat latest query to refresh the tabulated list."
  (let ((db (rcirc-sqlite--conn)))
    (sqlite-execute db rcirc-sqlite-dbquery rcirc-sqlite-dbdata)))

(defun rcirc-sqlite-db-drilldown (arg-list)
  "Drill down to messages per nick, channel, or day.
ARG-LIST defines which records to select."
  (let ((db (rcirc-sqlite--conn))
	(dbquery "SELECT rowid, channel, time, nick, message FROM rcirclogs WHERE ")
	(dbdata ()))
    (pcase-let ((`(,what ,where ,nick) arg-list))
      (cond
       ((string= nick rcirc-sqlite-channels-per-nick)
	(setq dbquery (concat dbquery "nick=?"))
	(push what dbdata))
       ((string= nick rcirc-sqlite-all-nicks)
	(setq dbquery (concat dbquery "channel=?"))
	(push what dbdata))
       ((string= nick "stats for day")
        (setq dbquery (concat dbquery "(strftime( '%Y-%m-%d', time, 'unixepoch'))=?"))
        (push what dbdata))
       (t
	(setq dbquery (concat dbquery (format "%s=? and nick=?" where)))
	(push nick dbdata)
	(push what dbdata)))
      (setq rcirc-sqlite-dbquery dbquery
	     rcirc-sqlite-dbdata dbdata)
      (sqlite-execute db dbquery dbdata))))

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
    (cond ((string= (car arg-list) rcirc-sqlite-nicks-per-channel)
	   (setq dimension "nicks")
	   (push "channel" rcirc-sqlite-drill-down-method)
	   (setq from
                 "(SELECT channel, nick FROM rcirclogs GROUP BY channel,nick)"))
	  ((string= (car arg-list) rcirc-sqlite-channels-per-nick)
	   (setq column "nick")
	   (setq dimension "channels")
	   (push "nick" rcirc-sqlite-drill-down-method)
	   (setq from
                 "(SELECT nick, channel FROM rcirclogs GROUP BY nick, channel)"))
	  ((string= (car arg-list) "Channel")
	   (setq column "nick")
	   (setq dimension "messages")
	   (setq from "rcirclogs where channel = ?")
	   (setq dbdata (cdr arg-list)))
	  ((string= (car arg-list) rcirc-sqlite-all-nicks)
	   (push "channel" rcirc-sqlite-drill-down-method)
	   (setq from "rcirclogs"))
	  (t
	   (setq from "rcirclogs where nick = ?")
	   (push "channel" rcirc-sqlite-drill-down-method)
	   (push (car arg-list) dbdata)
	   (setq dimension "messages")))
    (setq rcirc-sqlite-db-statdata (list column dimension from dbdata))
    (let ((dbquery (format
                    "SELECT %s, COUNT() || ' %s' FROM %s GROUP BY %s ORDER BY %s"
			   column dimension from column column)))
      (sqlite-select db dbquery dbdata))))

(defun rcirc-sqlite-db-refresh-stats ()
  "Fetch the stats again, to redisplay."
  (let ((db (rcirc-sqlite--conn)))
	(pcase-let ((`(,column ,dimension ,from ,dbdata) rcirc-sqlite-db-statdata))
	(let ((dbquery (format
			"SELECT %s, COUNT() || ' %s' FROM %s GROUP BY %s ORDER BY %s"
			column dimension from column column)))
	  (sqlite-select db dbquery dbdata)))))

(defun rcirc-sqlite-convert-tabulation-list (list-to-convert)
  "Convert LIST-TO-CONVERT to format for `tabulated-list-mode'.
Build a vector from the data in LIST-TO-CONVERT and format the
timestamp."
  (unless (listp list-to-convert)
    (error "No data available"))
  (mapcar (lambda (cell)
	    (list (nth 0 cell) (vector
				(if rcirc-sqlite-shorten-channel
				    (car (split-string (nth 1 cell) "@"))
				  (nth 1 cell))
				(format-time-string
				 rcirc-sqlite-time-format
				 (string-to-number (nth 2 cell)))
				(nth 3 cell)
				(nth 4 cell))))
          list-to-convert))

(defun rcirc-sqlite-convert-two-column-tabulation-list (list-to-convert)
  "Convert LIST-TO-CONVERT to format for `tabulated-list-mode'.
Build a vector from the data, converting numbers to text, because
`tabulated-list-mode' can't handle numbers."
  (unless (listp list-to-convert)
    (error "No data available"))
  (if (numberp (cadr (car list-to-convert)))
      (mapcar (lambda (cell)
		(list (car cell) (vector
				  (if rcirc-sqlite-shorten-channel
				      (car (split-string (car cell) "@"))
				    (car cell))
				  (number-to-string (cadr cell)))))
              list-to-convert)
    (mapcar (lambda (cell)
              (list (car cell) (vector
				(if rcirc-sqlite-shorten-channel
				    (car (split-string (car cell) "@"))
				  (car cell))
				(cadr cell))))
            list-to-convert)))

(defun rcirc-sqlite-update-buffer-with-short-channelnames ()
  "Update the tabulated list with short channel names.
This function should be called from the tabulated list."
  (interactive nil rcirc-sqlite-list-mode)
  (setq rcirc-sqlite-shorten-channel t)
  (rcirc-sqlite-display-tabulation-update))

(defun rcirc-sqlite-update-buffer-with-long-channelnames ()
    "Update the tabulated list with long channel names.
This function should be called from the tabulated list."
  (interactive nil rcirc-sqlite-list-mode)
  (setq rcirc-sqlite-shorten-channel nil)
  (rcirc-sqlite-display-tabulation-update))

(defun rcirc-sqlite-update-stats-buffer-with-short-channelnames ()
    "Update the tabulated list with short channel names.
This function should be called from the two column tabulated list."
 (interactive nil rcirc-sqlite--two-column-mode)
  (setq rcirc-sqlite-shorten-channel t)
  (rcirc-sqlite-display-stats-tabulation-update))

(defun rcirc-sqlite-update-stats-buffer-with-long-channelnames ()
    "Update the tabulated list with short channel names.
This function should be called from the two column tabulated list."
 (interactive nil rcirc-sqlite--two-column-mode)
  (setq rcirc-sqlite-shorten-channel nil)
  (rcirc-sqlite-display-stats-tabulation-update))

(defvar-keymap rcirc-sqlite-two-column-mode-map
  :parent tabulated-list-mode-map
  "RET" #'rcirc-sqlite-view-drill-down
  "<down-mouse-1>" #'rcirc-sqlite-view-drill-down
  (kbd "(") #'rcirc-sqlite-update-stats-buffer-with-short-channelnames
  (kbd ")") #'rcirc-sqlite-update-stats-buffer-with-long-channelnames)

(defvar-keymap rcirc-sqlite-list-mode-map
  :parent tabulated-list-mode-map
  "RET" #'rcirc-sqlite-single-day
  "<down-mouse-1>" #'rcirc-sqlite-single-day
  (kbd "<") #'rcirc-sqlite-single-nick
  (kbd ">") #'rcirc-sqlite-next-day
  (kbd "^") #'rcirc-sqlite-previous-day
  (kbd "a") #'rcirc-sqlite-all-next-days
  (kbd "c") #'rcirc-sqlite-kill-insert
  (kbd "r") #'rcirc-sqlite-register-append
  (kbd "R") #'rcirc-sqlite-register-insert
  (kbd "(") #'rcirc-sqlite-update-buffer-with-short-channelnames
  (kbd ")") #'rcirc-sqlite-update-buffer-with-long-channelnames)

(define-derived-mode rcirc-sqlite-two-column-mode tabulated-list-mode
  "rcirc-sqlite-two-column-mode"
  "Major mode Rcirc-SQLite, to display two-column tabulated db queries."
  (setq tabulated-list-format
	(vector (list "Channel/Nick" rcirc-sqlite-channel-column-width t)
		(list "Data" 0 t)))
  (tabulated-list-init-header))

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

(defun rcirc-sqlite-display-tabulation-list
    (identstr with-function &optional arg-list)
  "Display data in tabulated format in a new buffer.
Retreive data using WITH-FUNCTION, optionally with additional arguments
in ARG-LIST, IDENTSTR explains the current query through the mode-line."
  (with-current-buffer (get-buffer-create "*rcirc log*")
    (rcirc-sqlite-list-mode)
    (setq tabulated-list-entries
          (rcirc-sqlite-convert-tabulation-list
	   (funcall with-function arg-list)))
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

(defun rcirc-sqlite-display-tabulation-update ()
  "Update the data in the tabulated list."
  (with-current-buffer (get-buffer-create "*rcirc log*")
    (setq tabulated-list-entries
          (rcirc-sqlite-convert-tabulation-list
	   (funcall #'rcirc-sqlite-db-refresh)))
    (tabulated-list-print t)))

(defun rcirc-sqlite-display-stats-tabulation-update ()
  "Update the data in the tabulated stats list."
  (with-current-buffer (get-buffer-create "*rcirc log*")
    (setq tabulated-list-entries
          (rcirc-sqlite-convert-two-column-tabulation-list
	   (funcall #'rcirc-sqlite-db-refresh-stats)))
    (tabulated-list-print t)))

(defun rcirc-sqlite-view-drill-down-final ()
  "Last step in drill-down, query individual messages."
    (rcirc-sqlite-display-tabulation-list
     (format "Drill-down (%s %s)"
	     (nth 0 rcirc-sqlite-drill-down-method)
	     (nth 2 rcirc-sqlite-drill-down-method))
     #'rcirc-sqlite-db-drilldown rcirc-sqlite-drill-down-method))

(defun rcirc-sqlite-create-period-selectstring (when)
  "Create a where string for the query.
WHEN is a cons of start-time and end-time."
  (let ((subquery ""))
   (unless (= (car when) 0)
     (setq subquery (concat subquery "time+0 > ?"))
     (when (> (cdr when) 0)
       (setq subquery (concat subquery " AND time+0 < ?"))))
   subquery))

(defun rcirc-sqlite-format-period-string (when)
  "Create a human readable string from a time range.
WHEN is a cons of starttime and endtime."
  (let ((range-string rcirc-sqlite-anytime))
    (unless (= (car when) 0)
      (setq range-string (format-time-string "%F %R - " (car when)))
      (if (= (cdr when) 0)
	  (setq range-string (concat range-string "now"))
	(setq range-string
	      (concat range-string
		      (format-time-string "%F %R" (cdr when))))))
    range-string))

(defun rcirc-sqlite-view-drill-down ()
  "Show messages per nick or channel.
Called from `rcirc-sqlite-two-column-mode'."
  (interactive nil rcirc-sqlite-two-column-mode)
  (cond
   ((string= (nth 1 rcirc-sqlite-drill-down-method)
	     rcirc-sqlite-nicks-per-channel)
    (let ((arg-list (list "Channel" (tabulated-list-get-id))))
      (rcirc-sqlite-display-two-column-tabulation-list
       (format "Stats (%s)" (tabulated-list-get-id))
       #'rcirc-sqlite-db-query-stats arg-list)))
   ((string= (nth 0 rcirc-sqlite-drill-down-method) "stats per day")
    (setq rcirc-sqlite-drill-down-method
          (list (tabulated-list-get-id)
                nil
           "stats for day"))
    (rcirc-sqlite-view-drill-down-final))
   ((string= (nth 0 rcirc-sqlite-drill-down-method) "stats per month")
    (setq rcirc-sqlite-drill-down-method (list "stats per day" ""))
    (rcirc-sqlite-display-two-column-tabulation-list
     (format "Stats per month")
     #'rcirc-sqlite-db-query-monthly-count (tabulated-list-get-id)))
   ((string= (nth 0 rcirc-sqlite-drill-down-method) "Channel")
    (setq rcirc-sqlite-drill-down-method
	  (list (nth 1 rcirc-sqlite-drill-down-method)
		"channel"
		(tabulated-list-get-id)))
    (rcirc-sqlite-view-drill-down-final))
   (t
    (push (tabulated-list-get-id) rcirc-sqlite-drill-down-method)
    (rcirc-sqlite-view-drill-down-final))))

(defun rcirc-sqlite-query-single-day
    (with-nick extra-days all-next-days)
  "Select logs from a single day.
When WITH-NICK is not nil, narrow the selection to a nick.
EXTRA-DAYS extends to previous or next days.
When ALL-NEXT-DAYS is true, select everything after the start-time."
  (let ((rowid (tabulated-list-get-id)))
    (when (not rowid)
      (error "No row selected"))
    (let ((rowdata
	   (car (rcirc-sqlite-db-row-data rowid)))
          (nick rcirc-sqlite-all-nicks)
          (timerange nil))
      (cond (all-next-days
             (setq timerange (cons (nth 4 rowdata) 0)))
	    ((< extra-days 0)
	     (setq timerange (cons (+ (* extra-days 86400)
                                      (nth 4 rowdata))
				   (+ (nth 4 rowdata) 86400))))
	    (t
             (setq timerange (cons (nth 4 rowdata)
				   (+ (* extra-days 86400)
                                      (nth 4 rowdata))))))
      (let ((identstring (nth 0 rowdata)))
        (when with-nick
          (setq nick (nth 2 rowdata))
          (setq identstring (concat identstring " <" nick ">")))
        (rcirc-sqlite-display-tabulation-list
         (format "%s %s" identstring
                 (rcirc-sqlite-format-period-string timerange))
         #'rcirc-sqlite-db-search-log (list nil
                                            (nth 0 rowdata)
                                            timerange
                                            nick))))))

(defun rcirc-sqlite-single-day ()
  "Drill down from the main tabulated view.
Select all the logs from the day and channel of the selected line.
Called from `rcirc-sqlite-list-mode'."
  (interactive nil rcirc-sqlite-list-mode)
  (rcirc-sqlite-query-single-day nil 1 nil))

(defun rcirc-sqlite-single-nick ()
  "Drill down from the main tabulated view.
Select all the logs from the day and channel of the selected line
and narrow to the nick from the selected line.
Called from `rcirc-sqlite-list-mode'."
  (interactive nil rcirc-sqlite-list-mode)
  (rcirc-sqlite-query-single-day t 1 nil))

(defun rcirc-sqlite-next-day ()
  "Drill down from the main tabulated view.
Select all the logs from the day of the selected line
as well as from the day after that.
Called from `rcirc-sqlite-list-mode'."
  (interactive nil rcirc-sqlite-list-mode)
  (rcirc-sqlite-query-single-day nil 2 nil))

(defun rcirc-sqlite-previous-day ()
  "Drill down from the main tabulated view.
Select all the logs from the day of the selected line
as well as from the day before that.
Called from `rcirc-sqlite-list-mode'."
  (interactive nil rcirc-sqlite-list-mode)
  (rcirc-sqlite-query-single-day nil -1 nil))

(defun rcirc-sqlite-all-next-days ()
    "Drill down from the main tabulated view.
Select all the logs from the day of the selected line
as well as all following days.
Called from `rcirc-sqlite-list-mode'."
  (interactive nil rcirc-sqlite-list-mode)
  (rcirc-sqlite-query-single-day nil nil t))

(defun rcirc-sqlite-format-message ()
  "Fetch the selected message and nicely format it."
  (let ((rowid (tabulated-list-get-id)))
    (when (not rowid)
      (error "No row selected"))
    (pcase-let ((`(,channel ,time ,nick ,message)
		 (car (rcirc-sqlite-db-row-data rowid))))
      (let ((day (format-time-string "%F" (string-to-number time))))
        (format "** %s <%s>\n<%s> wrote on %s in %s\n\n%s\n"
		day nick nick day channel message)))))

(defun rcirc-sqlite-kill-insert ()
  "Add the selected message nicely formatted to the `kill-ring'."
  (interactive nil rcirc-sqlite-list-mode)
  (kill-new (rcirc-sqlite-format-message))
  (message "Added message to kill-ring"))

(defun rcirc-sqlite-register-add (method)
  "Insert a chatmessage into the register `rcirc-sqlite-register'.
METHOD is eiter insert or append."
  (if (string= method "insert")
      (set-register rcirc-sqlite-register (rcirc-sqlite-format-message))
    (set-register rcirc-sqlite-register
		  (format "%s\n%s"
			  (get-register rcirc-sqlite-register)
			  (rcirc-sqlite-format-message))))
  (message "Added message to register %c" rcirc-sqlite-register))

(defun rcirc-sqlite-register-insert ()
  "Insert (overwrite) the selected chatmessage into a register.
Default the register `r' is used, this can be changed by the user.
Called from `rcirc-sqlite-list-mode'."
  (interactive nil rcirc-sqlite-list-mode)
  (rcirc-sqlite-register-add "insert"))

(defun rcirc-sqlite-register-append ()
   "Append the selected chatmessage to a register.
Default register `r' is used, this can be changed by the user.
Called from `rcirc-sqlite-list-mode'."
  (interactive nil rcirc-sqlite-list-mode)
    (rcirc-sqlite-register-add "append"))

(defun rcirc-sqlite-select-channel ()
  "Provide completion to select a channel."
  (let ((default (or (bound-and-true-p rcirc-target)
		     rcirc-sqlite-all-channels)))
    (completing-read
     (format-prompt "Select a channel" default)
     (cons rcirc-sqlite-all-channels (rcirc-sqlite-db-query-channels))
     nil nil nil nil default)))

(defun rcirc-sqlite-select-nick (wild-card-value)
  "Provide completion to select a nick.
Extend the list of nicks with WILD-CARD-VALUE to offer the user more
choices.  This will also be used as the default choice."
  (completing-read
   (format-prompt "Select a nick" wild-card-value)
   (append wild-card-value (rcirc-sqlite-db-query-nicks))
   nil nil nil nil wild-card-value))

(defun rcirc-sqlite-select-time-range ()
  "Select the start and end in epochseconds of a time range.
When end-time is before start-time, exchange them."
  (let ((chperiod (completing-read
		   (format-prompt "Select the search period (use completion): "
				  rcirc-sqlite-anytime)
		   (list rcirc-sqlite-anytime rcirc-sqlite-manually-select
		     rcirc-sqlite-last-90-days rcirc-sqlite-last-60-days
		     rcirc-sqlite-last-30-days rcirc-sqlite-last-07-days)
		   nil nil nil nil rcirc-sqlite-anytime))
	(end-time 0)
	(start-time (time-convert (current-time) 'integer)))
    (cond
     ((string= chperiod rcirc-sqlite-anytime) (setq start-time 0))
     ((string= chperiod rcirc-sqlite-last-90-days)
      (setq start-time (- start-time (* 90 86400))))
     ((string= chperiod rcirc-sqlite-last-60-days)
      (setq start-time (- start-time (* 60 86400))))
     ((string= chperiod rcirc-sqlite-last-30-days)
      (setq start-time (- start-time (* 30 86400))))
     ((string= chperiod rcirc-sqlite-last-07-days)
      (setq start-time (- start-time (* 7 86400))))
     ((string= chperiod rcirc-sqlite-manually-select)
      (setq start-time
	    (1- (time-convert
		(org-read-date t t nil
			       "Range starts" nil nil) 'integer))
	    end-time
	    (1+ (time-convert
		(org-read-date t t nil
			       "Range ends" nil nil) 'integer)))
      (when (< end-time start-time)
	(let ((tmp-time end-time))
	  (setq end-time start-time)
	  (setq start-time tmp-time)))))
    (cons start-time end-time)))

(defun rcirc-sqlite-logs-from-nick (nick when)
  "View the logs from a specific NICK.
WHEN is a cons of starttime and endtime.
The results are displayed a new buffer."
  (interactive (list
		(rcirc-sqlite-select-nick nil)
		(rcirc-sqlite-select-time-range)))
  (when (string-empty-p nick)
    (error "No nick selected"))
  (let ((searcharg (list nil rcirc-sqlite-all-channels when nick)))
    (rcirc-sqlite-display-tabulation-list
     (format "<%s> %s" nick (rcirc-sqlite-format-period-string when))
     #'rcirc-sqlite-db-search-log searcharg)))

(defun rcirc-sqlite-view-log (channel when &optional unlimited offset limit)
  "View the logs of a specific CHANNEL.
WHEN is a cons of start time and end time, each possible zero.
Shows the result in a new buffer.
When called without OFFSET and LIMIT, show the last 200 rows.
When called with non-nil UNLIMITED, show all the rows.
Otherwise offset and limit are used; in that case  both offset
and limit have to be provided."
  (interactive (list (rcirc-sqlite-select-channel)
		     (rcirc-sqlite-select-time-range)))
  (let ((searcharg-list (list channel when unlimited offset limit)))
    (rcirc-sqlite-display-tabulation-list
     (format "View log (%s %s)" channel
	     (rcirc-sqlite-format-period-string when))
     #'rcirc-sqlite-db-query-log searcharg-list)))

(defun rcirc-sqlite-text-search (query channel when nick)
  "Perform full text search for QUERY.
WHEN is a cons of start time and end time, each possible zero.
Optional narrow search in a specific CHANNEL and/or with a specific NICK.
The results are displayed a new buffer."
  (interactive (list (read-string "Search for: ")
                     (rcirc-sqlite-select-channel)
		     (rcirc-sqlite-select-time-range)
		     (rcirc-sqlite-select-nick (list rcirc-sqlite-all-nicks))))
  (let ((searcharg-list (list query channel when nick nil)))
    (rcirc-sqlite-display-tabulation-list
     (format "Search %s (%s %s <%s>)" query channel
	     (rcirc-sqlite-format-period-string when)
	     nick)
     #'rcirc-sqlite-db-search-log searcharg-list)))

(defun rcirc-sqlite-stats (nick)
  "Display overview of the number of rows per channel.
Optionally narrow to a specific NICK.
The results are displayed a new buffer."
  (interactive (list (rcirc-sqlite-select-nick
                      (list rcirc-sqlite-all-nicks
			    rcirc-sqlite-nicks-per-channel
			    rcirc-sqlite-channels-per-nick))))
  (let ((searcharg-list (list nick)))
    (rcirc-sqlite-display-two-column-tabulation-list
     (format "Stats (<%s>)" nick)
     #'rcirc-sqlite-db-query-stats searcharg-list)))

(defun rcirc-sqlite-stats-per-month ()
  "Display overview of the number of rows per month.
The results are displayed a new buffer."
  (interactive)
  (setq rcirc-sqlite-drill-down-method (list "stats per month" ""))
  (rcirc-sqlite-display-two-column-tabulation-list
   (format "Stats per month")
   #'rcirc-sqlite-db-query-monthly-count nil))

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
