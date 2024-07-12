;;; greader-audiobook.el --- Converts buffers into audio. -*- lexical-binding: t; -*-
;; Copyright (C) 2017-2024  Free Software Foundation, Inc.
;; Filename: greader-audiobook.el
;; Description: converts the current buffer into an audiobook using espeak.
;; Author: Michelangelo Rodriguez <michelangelo.rodriguez@gmail.com>
;; Maintainer: Michelangelo Rodriguez
;; <michelangelo.rodriguez@gmail.com>

;; Created: Dom Mar 31 00:32:55 2024 (+0100)
;; URL: https://gitlab.com/michelangelo-rodriguez/greader
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; This module defines just one command:
;; `greader-audiobook-buffer'.  All the rest of the functionality is
;; controlled by customizing the module through customizing the group
;; `greader-audiobook' so:
;; 'M-x customize-group <RET> greader-audiobook <RET>.
;; Please see the documentation of each single customization item, and
;; the documentation of `greader-audiobook-buffer'.
;;
;; The parameters of espeak are the same used normally with
;; `greader-read', so you just have to configure greader normally in
;; terms of the back-end.
;; If you want the maximum speed of the conversion, disable
;; `greader-audiobook-transcode-wave-files'.
;; In this way you will have a directory with only the wave files
;; produced by espeak.
;; in order for greader-audiobook to transcode your files you must
;; have ffmpeg utility installed on your system.
;; the default format in which transcode the files produced by espeak
;; is "mp3", but honestly it is not the better choice, only the most
;; popular.
;; If you want better quality of output, you can set
;; `greader-audiobook-transcode-format to "m4a" or "flac"; The
;; conversion will take more time, but as sayd first, the quality is
;; definitely better.
;;
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(require 'subr-x)
;; variable definitions
(require 'greader-dict)
(declare-function greader-dehyphenate nil)
(declare-function greader-get-rate nil)
(declare-function greader-get-language nil)

(defgroup greader-audiobook
  nil
  "Greader audiobook configuration."
  :group 'greader)

(defcustom greader-audiobook-compress t
  "When enabled, compress the directory created using zip."
  :type '(boolean))


(defcustom greader-audiobook-cancel-intermediate-wave-files nil
  "Whether cancel or not intermediate wave files.
This variable is used when variable `greader-audiobook-compress' is enabled."
  :type 'boolean)

(defcustom greader-audiobook-base-directory (concat
					     user-emacs-directory
					     "audiobooks/")
  "Base directory in which store converted audiobooks."
  :type 'string)

(defcustom greader-audiobook-block-size "15"
  "Specify the size of each block produced when converting the document.
If you specify a string, it should contain a number to specify the
size in minutes based on `greader-get-rate', so the calculus is
approximate.
If you specify a non-negative number, it will be treated as a size in
characters.
In any case the size or the time are approximate, because the
block will end at an end of sentence.
If the value is 0 or \"0\", an unique file will be generated.
If current `major-mode' is in the variable `greader-audiobook-modes',
this variable will be ignored to honor the mode specified in
`greader-audiobook-modes'."
  :type '(choice (natnum :tag "size in characters") (string :tag "size
  in minutes")))

(defcustom greader-audiobook-modes '((ereader-mode . "\\W*"))
  "Different treatment of block based on the current major mode.
Instead of numerical block size, use a string or function to determine the end
of each block.
If you specify a function, that function has to return a cons in which
car represents the start of the block, and cdr represents the end,
or nil if there are no more blocks to convert."
  :type '(alist :key-type (symbol :tag "mode") :value-type (choice
  (string  function))))

(defcustom greader-audiobook-transcode-wave-files nil
  "If enabled,  transcode original wave files using `ffmpeg'."
  :type '(boolean))

(defcustom greader-audiobook-transcode-format "mp3"
  "Specify the format in which transcode original wave files.
You should specify the format without the initial dot, so for example
if you want to transcode original files in flac format, you should
set this variable to \"flac\" \(not \".flac\"\)."
  :type '(string :tag "format (without extension)"))

(defcustom greader-audiobook-ffmpeg-extra-global-args nil
  "List of strings containing extra output arguments to pass to ffmpeg."

  :type '(repeat (string :tag "argument")))

(defcustom greader-audiobook-ffmpeg-extra-output-args nil
  "Extra output arguments to pass to ffmpeg."
  :type '(repeat (string :tag "argument")))
(defcustom greader-audiobook-zip-args nil
  "Arguments to pass to the zip utility."
  :type '(repeat (string :tag "argument")))

(defcustom greader-audiobook-compress-remove-original nil
  "When enabled, remove the original directory of the book converted.
In this way, you will have only the zipped file containing the book."
  :type 'boolean)

(defcustom greader-audiobook-buffer-quietly nil
  "Convert buffer without messages.
Only the final report will be printed."
  :type '(boolean))

(defcustom greader-audiobook-include-track-name-in-audio nil
  "If t, audio track names will be included at start of each file."
  :type '(boolean))
(defcustom greader-audiobook-pause-at-end-of-track t
  "Enable to add a pause at end of each block."
  :type '(boolean))

(defcustom greader-audiobook-pause-string "\n.\n.\n.\n.\n.\n"
  "The string that will be used to generate the pause at end of sentence."
  :type '(string))
(defcustom greader-audiobook-create-m4b nil
  "If enabled, an m4b file will be created.
Enabling it implies disabling of the variable `greader-audiobook-compress'."
  :type '(boolean))
;; functions

(defun greader-audiobook--percentage ()
  "Return the percentage read of the buffer."
  (let ((unit (/ (point-max) 100)))
    (/ (point) unit)))
(defun greader-audiobook--get-block ()
  "Get a block of text in current buffer.
This function uses `greader-audiobook-block-size' to determine the
position of the end of the block.
If the current major mode is in `greader-audiobook-modes', the
associated string has priority over `greader-audiobook-block-size.
Return a cons with start and end of the block or nil if at end of the buffer."

  (save-excursion
    (let ((start (point))
	  (end (point-max)))
      (if (assq major-mode greader-audiobook-modes)
	  (progn
	    (when (looking-at "\\W")
	      (setq start (re-search-forward "\\W*" nil 1)))
	    (re-search-forward
	     (cdr (assq major-mode greader-audiobook-modes))
	     nil t 1)
	    (setq end (point)))
	(pcase greader-audiobook-block-size
	  ((pred numberp)
	   (when (> greader-audiobook-block-size 0)
	     (goto-char (+ (point) greader-audiobook-block-size))
	     (when (thing-at-point 'sentence)
	       (forward-sentence)
	       (setq end (point)))
	     (when (looking-at "\\W")
	       (re-search-forward "\\W*" nil 1))
	     (setq end (point))))
	  ((pred stringp)
	   (cond
	    ((> (string-to-number greader-audiobook-block-size) 0)
	     (forward-word (* (string-to-number
			       greader-audiobook-block-size)
			      (greader-get-rate)))
	     (when (thing-at-point 'sentence)
	       (forward-sentence))
	     (when (looking-at "\\W")
	       (re-search-forward "\\W*" nil 1))
	     (setq end (point)))))
	  (_
	   (error "Cannot determine the block size"))))
      (if (> end start)
	  (cons start end)
	nil))))

(defun greader-audiobook-convert-block (filename)
  "Convert a block of text in the current buffer, saving it in FILENAME.
If variable `greader-dict-mode' or
variable `greader-dict-toggle-filters' are enabled,
substitutions will be performed on the block.
After conversion, point will be moved to the end of the block.
Return the generated file name, or nil if at end of the buffer."

  (let*
      ((command "espeak-ng")
       (rate (concat "-s" (number-to-string (greader-get-rate))))
       (language (concat "-v" (greader-get-language)))
       (wave-file (concat "-w" filename))
       (output nil)
       (block (greader-audiobook--get-block))
       (text (when block (buffer-substring (car block) (cdr block)))))
    (if block
	(progn
	  (when greader-audiobook-include-track-name-in-audio
	    (setq text (concat (file-name-sans-extension filename)
			       ".\n" text)))
	  (setq text (greader-dehyphenate text))
	  (when (or greader-dict-mode greader-dict-toggle-filters)
	    (setq text (greader-dict-check-and-replace text)))
	  (when greader-audiobook-pause-at-end-of-track
	    (setq text (concat text greader-audiobook-pause-string)))
	  (setq output (call-process command nil "*espeak-output*" nil
				     rate language
				     wave-file text))
	  (if (= output 0)
	      (goto-char (cdr block))
	    (error "Espeak has generated an error.  Please see
      *espeak-output* for more information"))
	  filename)
      nil)))

(defun greader-audiobook--count-blocks ()
  "Return the number of total blocks that constitutes a buffer."
  (save-excursion
    (let ((blocks 0)
	  (block (greader-audiobook--get-block)))
      (while block
	(setq blocks (+ blocks 1))
	(goto-char (cdr block))
	(setq block (greader-audiobook--get-block)))
      blocks)))


(defun greader-audiobook-transcode-file (filename)
  "Transcode FILENAME using ffmpeg.
You have certain control of how this happens by configuring
`greader-audiobook-ffmpeg-extra-global-args', and
`greader-audiobook-ffmpeg-extra-output-args'."

  (let
      ((ffmpeg-args (append greader-audiobook-ffmpeg-extra-global-args
			    (list "-i" filename)
			    greader-audiobook-ffmpeg-extra-output-args
			    (list (concat
				   (file-name-sans-extension filename)
				   "."
				   greader-audiobook-transcode-format))))
       (result nil))
    (setq result (apply #'call-process "ffmpeg" nil "*ffmpeg-output*"
			nil ffmpeg-args))
    (unless (eq result 0)
      (error "Error while transcoding, see buffer `*ffmpeg-output*'"))))

(defun greader-audiobook--calculate-file-name (counter total-blocks)
  "Calculate a file name based on the length of TOTAL-BLOCKS.
COUNTER represents the current file name."

  (let* ((counter-string (number-to-string counter))
	 (total-blocks-string (number-to-string total-blocks))
	 (filename nil)
	 (counter-chars 0))
    (while (< counter-chars (- (length
				total-blocks-string)
			       (length counter-string)))
      (setq filename (concat filename "0"))
      (setq counter-chars (+ counter-chars 1)))
    (setq filename (concat filename counter-string ".wav"))))

(defun greader-audiobook-compress (book-directory)
  "Compress given BOOK-DIRECTORY."
  (let ((zip-args (append (list "-rj")greader-audiobook-zip-args (list
								  (concat
								   (string-remove-suffix
								    "/"
								    book-directory)
								   ".zip"))
			  (list book-directory)))
	(result nil))
    (setq result (apply #'call-process "zip" nil "*audiobook-zip*" nil
			zip-args))
    (unless (eq result 0)
      (error "Error while compressing, see buffer *audiobook-zip* for
more information"))))
(defun greader-audiobook--get-file-list ()
  "Return the list of media files in current dir.
Media files means that those files must have the same extension as
`greader-audiobook-transcode-format'."

  (let* ((regexp greader-audiobook-transcode-format)
	 (file-list (directory-files default-directory nil regexp)))
    file-list))

(defun greader-audiobook--make-index ()
  "Write a file with all the tracks pertaining to this book."
  (let* ((output-file-name "filelist.txt")
	 (file-list (greader-audiobook--get-file-list)))
    (with-temp-buffer
      (dolist (file file-list)
	(insert "file " "\'" file "\'" "\n"))
      (write-region (point-min) (point-max) output-file-name))))

(defun greader-audiobook--get-file-duration (file-name)
  "Return the duration of FILE-NAME in milliseconds."
  (with-temp-buffer
    (let ((command "ffprobe")
          (args (list "-i" file-name
                      "-show_entries" "format=duration"
                      "-v" "quiet"
                      "-of" "csv=p=0"))
          (duration 0)
          (process nil))
      (setq process (apply #'call-process command nil t nil args))
      (if (= process 0)
          (progn
            (goto-char (point-min))
            (unless (re-search-forward "[0-9]+\\.?[0-9]*" nil t)
              (error "Cannot determine the duration of this file"))
            (setq duration (match-string 0)))
        (error
	 "There was an error while determining the duration of %s"
	 file-name))
      (truncate (* (string-to-number duration) 1000)))))

(defun greader-audiobook--make-chapters-file ()
  "Create a file \=chapters.txt\= with our chapters.
This file should be compatible with ffmpeg."

  (let* ((output-file-name "chapters.txt")
	 (file-list (greader-audiobook--get-file-list))
	 (title (file-name-sans-extension (buffer-name)))
	 (start 0)
	 (end 0))
    (with-temp-buffer
      (insert
       ";FFMETADATA1\n"
       "artist=\n"
       "title=" title "\n")
      (dolist (file file-list)
	(setq end
	      (+ (greader-audiobook--get-file-duration file) start))
	(insert
	 "\[CHAPTER\]\n"
	 "TIMEBASE=1/1000\n"
	 "START=" (number-to-string start) "\n"
	 "END=" (number-to-string end) "\n"
	 "title="
	 (file-name-nondirectory (file-name-sans-extension file))
	 "\n\n")
	(setq start end))
      (write-region (point-min) (point-max) output-file-name))))

(defun greader-audiobook-make-m4b ()
  "Make an m4b file from the current folder."
  (let* ((command "ffmpeg")
         (output-file (concat
                       (expand-file-name
			greader-audiobook-base-directory)
                       (file-name-sans-extension (buffer-name)) ".m4b"))
         (args (list "-f" "concat" "-safe" "0" "-i" "filelist.txt"
                     "-i" "chapters.txt" "-map_metadata" "1"
                     "-f" "mp4" output-file))
         (output-buffer "*ffmpeg-m4b-output*")
         (process nil)
         (files-to-delete '("filelist.txt" "chapters.txt")))
    (greader-audiobook--make-index)
    (greader-audiobook--make-chapters-file)
    (setq process
	  (apply #'call-process command nil output-buffer nil args))
    (unless (eq process 0)
      (error
       "Error while creating m4b file.  Please see %s for more information"
       output-buffer))
    (mapc #'delete-file files-to-delete))
  t)

;;;###autoload
(defun greader-audiobook-buffer (&optional start-position)
  "Convert current buffer to an audiobook starting at START-POSITION.
With prefix, the conversion will start from the beginning of the
buffer, otherwise it will start from point to the end.
If region is active, only the region will be converted.
This function will create a directory under
`greader-audiobook-base-directory with the same name as the
buffer without the extension, if any."

  (interactive "P")
  (when (directory-files greader-audiobook-base-directory nil
			 (file-name-sans-extension (buffer-name)))
    (let ((response (yes-or-no-p "This audiobook already
  exists.  Overwrite it?")))
      (if response
	  (progn
	    (unless greader-audiobook-buffer-quietly
	      (message "removing old audiobook..."))
	    (delete-directory (concat greader-audiobook-base-directory
				      (file-name-sans-extension (buffer-name)))
			      t t))
	(user-error "Audiobook creation aborted by user"))))
  (unless greader-audiobook-buffer-quietly
    (message "Preparing for conversion (this could take some time...)"))
  (let ((end-position (point-max)))
    (cond
     ((not start-position)
      (setq start-position (point)))
     ((listp start-position)
      (setq start-position (point-min)))
     ((region-active-p)
      (setq start-position (region-beginning))
      (setq end-position (region-end))))
    (save-excursion
      (save-restriction
	(narrow-to-region start-position end-position)
	(goto-char start-position)
	(unless (file-exists-p greader-audiobook-base-directory)
	  (make-directory greader-audiobook-base-directory))
	(let* ((book-directory (concat (file-name-sans-extension
					(buffer-name))
				       "/"))
	       (default-directory (concat
				   greader-audiobook-base-directory
				   book-directory))
	       (output-file-name nil)
	       (output-file-counter 1)
	       (total-blocks (greader-audiobook--count-blocks)))
	  (unless (file-exists-p default-directory)
	    (make-directory default-directory))
	  (unless greader-audiobook-buffer-quietly
	    (message "Starting conversion of %s ."
		     book-directory))
	  (while (greader-audiobook--get-block)
	    (setq output-file-name
		  (greader-audiobook--calculate-file-name
		   output-file-counter total-blocks))
	    (unless greader-audiobook-buffer-quietly
	      (message "converting block %d of %d \(%s\)"
		       output-file-counter
		       total-blocks (concat (number-to-string
					     (greader-audiobook--percentage))
					    "\%")))
	    (setq output-file-name
		  (greader-audiobook-convert-block output-file-name))
	    (if output-file-name
		(progn
		  (when greader-audiobook-transcode-wave-files
		    (unless greader-audiobook-buffer-quietly
		      (message "Transcoding block to %s..."
			       greader-audiobook-transcode-format))
		    (greader-audiobook-transcode-file
		     output-file-name)
		    (when
			greader-audiobook-cancel-intermediate-wave-files
		      (delete-file output-file-name)))
		  (setq output-file-counter (+ output-file-counter 1)))
	      (error "An error has occurred while converting")))
	  (when greader-audiobook-create-m4b
	    (unless greader-audiobook-buffer-quietly
	      (message "Building m4b..."))
	    (greader-audiobook-make-m4b)
	    (setq book-directory (concat (string-remove-suffix "/"
							       book-directory)
					 ".m4b")))
	  (when
	      (and greader-audiobook-compress
		   (not greader-audiobook-create-m4b))
	    (setq default-directory greader-audiobook-base-directory)
	    (unless greader-audiobook-buffer-quietly
	      (message "compressing %s..." book-directory))
	    (greader-audiobook-compress book-directory)
	    (when greader-audiobook-compress-remove-original
	      (delete-directory book-directory t t)
	      (setq book-directory (concat (string-remove-suffix "/"
								 book-directory)
					   ".zip"))))
	  (message "conversion terminated and saved in %s"
		   (concat greader-audiobook-base-directory
			   book-directory)))))))

(defvar greader-audiobook-transcode-history nil)

(defun greader-audiobook-retranscode (audiobook-directory new-format)
  "Transcode an AUDIOBOOK-DIRECTORY into NEW-FORMAT.
If `greader-audiobook-cancel-intermediate-wave-files is enabled, then
original files will be deleted."

  (interactive
   (let ((book-directory (read-directory-name "Audiobook to
re-transcode (directory): " greader-audiobook-base-directory nil t))
	 (new-format (read-string "New format: " nil
				 'greader-audiobook-transcode-history)))
     (list book-directory new-format)))
  (let* ((default-directory audiobook-directory)
	 (greader-audiobook-transcode-format new-format)
	 (file-list (directory-files default-directory nil
				    "^[[:digit:]]")))
    (dolist (file file-list)
      (unless greader-audiobook-buffer-quietly
	(message "Re-transcoding file %s..." file))
      (greader-audiobook-transcode-file file)
      (when greader-audiobook-cancel-intermediate-wave-files
	(delete-file file)))
    (message "re-transcoding finished.")))

(provide 'greader-audiobook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; greader-audiobook.el ends here
