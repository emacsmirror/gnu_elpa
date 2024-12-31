;;; greader-speechd.el --- speech-dispatcher back-end for greader  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2025  Free Software Foundation, Inc.

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
;; 

;;; Code:
(require 'ring)
;;; customization variables
(defgroup greader-speechd
  nil
  "Speech-dispatcher back-end for greader."
  :group 'greader)

(defcustom greader-speechd-handle-server nil
  "automatically handle of server.
If this variable is set, greader will check if there is a running
instance of `speech-dispatcher', kill it and relaunch with the correct
parameters to make it work properly with greader.
Alternatively, you can start speech-dispatcher as daemon, and with
timer disabled as in:
`$ speech-dispatcher -d -t0'.
Use this variable with caution, especially if you are using other
assistive technologies that rely on speech-dispatcher."
  :type 'boolean)

(defvar greader-speechd-server nil
  "contains the instance of speech-dispatcher server, or nil if
`greader-speechd-handle-server' is enabled.")

(defun greader-speechd-start-server ()
  "Start speech-dispatcher.
If there are other instances of speech-dispatcher, this function will
kill them first. This function does nothing when
`greader-speechd-handle-server' is disabled."
  (when
      (and greader-speechd-handle-server (not greader-speechd-server))
    (call-process "killall" nil 0 nil "speech-dispatcher")
    ;; Now we start `speech-dispatcher' with -s, that means we want a
    ;; single application instance, and with `-t0', which stands for
    ;; timer disabled. In this way speech-dispatcher will keep running
    ;; even if there no are currently messages to speak.
    (setq greader-speechd-server
	  (make-process  :name "greader-speechd" :command
			 '("speech-dispatcher" "-s" "-t0")
			 :sentinel (lambda (_p _status) (setq
							 greader-speechd-server
							 nil))))))

(defcustom greader-speechd-executable "spd-say"
  "Executable file name."
  :tag "speech-dispatcher client executable file name"
  :type 'string)

(defcustom greader-speechd-language "en"
  "Language of speech-dispatcher client to speak in."
  :tag "speech-dispatcher language"
  :type 'string)

(defcustom greader-speechd-rate 10
  "Rate of speech.
Can be a value between -100 and 100."
  :tag "speech-dispatcher rate"
  :type 'integer)

(defcustom greader-speechd-punctuation "none"
  "Punctuation level of speech-dispatcher client to speak.
It must be one of the following:
none, some, or all."
  :tag "speech-dispatcher punctuation level"
  :type 'string)

;;; code
(defun greader-speechd--find-executable ()
  "Try to find speech-dispatcher client.
using `greader-speechd-executable' as basename."
  (locate-file greader-speechd-executable exec-path))

(defun greader-speechd-set-language
    (&optional lang)
  "Set language LANG for speech-dispatcher client.
if LANG is omitted, it looks in variable `greader-speechd-language' and
retrieves the appropriate string used by spd-say or another client
compatible."
  (if (not lang)
      (concat "-l" greader-speechd-language)
    (progn
      (setq-local greader-speechd-language lang)
      (concat "-l" lang))))

(defun greader-speechd-set-rate
    (&optional rate)
  "Return parameter suitable for spd-say to set speech rate.
for further documentation, see the `greader-speechd-rate' variable."
  (if (not rate)
      (concat "-r " (number-to-string greader-speechd-rate))
    (progn
      (setq-local greader-speechd-rate rate)
      (concat "-r " (number-to-string rate)))))

(defun greader-speechd-set-punctuation (&optional punct)
  "Return a suitable parameter to pass to spd-say for setting punctuation level.
PUNCT must be a numeric value, 0 for no punctuation, 1 for some and 2
or >2 for all punctuation."
  (setq-local greader-speechd-punctuation
	      (pcase punct
		('t "all")
		('0 "none")
		('1 "some")
		((and (pred numberp) (pred (<= 2))) "all")
		(_ (error "Unknown punctuation: %S" punct))))
  (concat "-m" greader-speechd-punctuation))

(defun greader-speechd-stop ()
  "Stops speech-dispatcher client."
  (start-process "speechd-client" nil greader-speechd-executable "-S")
  (sleep-for 0.100))

(defvar greader-speechd--punctuation-ring (make-ring 3))
(ring-insert greader-speechd--punctuation-ring 0)
(ring-insert greader-speechd--punctuation-ring 1)
(ring-insert greader-speechd--punctuation-ring 2)

(defvar greader-speechd--ring-item
  (cond
   ((equal greader-speechd-punctuation "none") 0)
   ((equal greader-speechd-punctuation "some") 1)
   ((= greader-speechd-punctuation "all") 2)))

(defun greader-speechd--ring-next ()
  "Return the next element in the `greader-speechd--punctuation-ring'
based on `greader-speechd--ring-item'"
  (setq greader-speechd--ring-item (ring-next
				    greader-speechd--punctuation-ring
					      greader-speechd--ring-item)))

;;;###autoload
(defun greader-speechd (command &optional arg)
  "greader speech-dispatcher back-end."
  (greader-speechd-start-server)
  (pcase command
    ('executable
     greader-speechd-executable)
    ('lang
     (if (not arg)
	 (greader-speechd-set-language)
       (greader-speechd-set-language arg)))
    ('rate
     (cond((not arg)
	   (greader-speechd-set-rate))
	  ((numberp arg)
	   (greader-speechd-set-rate arg))
	  ((equal arg 'value)
	   greader-speechd-rate)))
    ('punctuation
     (cond
      ((equal arg 'toggle)
       (prog1
	   (greader-speechd-set-punctuation
	    (greader-speechd--ring-next))
	 (message
	  (concat "punctuation set to " greader-speechd-punctuation))))
      ((not arg)
       (cond
	((equal greader-speechd--ring-item 0) "-mnone")
	((equal greader-speechd--ring-item 1) "-msome")
	((>= greader-speechd--ring-item 2) "-mall")))))
    ('stop
     (greader-speechd-stop))
    ('extra
     "-w")
    ('get-rate
     greader-speechd-rate)
    (_
     'not-implemented)))
(put 'greader-speechd 'greader-backend-name "greader-speechd")
(provide 'greader-speechd)
;;; greader-speechd.el ends here
