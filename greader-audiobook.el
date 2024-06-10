;;; greader-audiobook.el --- Converts buffers into audio. -*- lexical-binding: t; -*-
;; 
;; Filename: greader-audiobook.el
;; Description: converts the current buffer into an audiobook using espeak.
;; Author: Michelangelo Rodriguez <michelangelo.rodriguez@gmail.com>
;; Maintainer: Michelangelo Rodriguez
;; <michelangelo.rodriguez@gmail.com>

;; Created: Dom Mar 31 00:32:55 2024 (+0100)
;; Package-Requires: ()
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

(defcustom greader-audiobook-modes '((ereader-mode . ""))
  "Different treatment of block based on the current major mode.
Instead of numerical block size, use a string to determine the end of
each block."
  :type '(alist :key-type (symbol :tag "mode") :value-type (string)))

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

;; functions
(defun greader-audiobook--get-block ()
  "Get a block of text in current buffer.
This function uses `greader-audiobook-block-size' to determine the
position of the end of the block.
If the current major mode is in `greader-audiobook-modes', the
associated string has priority over `greader-audiobook-block-size.
Return a cons with start and end of the block or nil if at end of the buffer."

  (save-excursion
    (let ((start (point))
	  (end (point-max))
	  (words (count-words (point) (point-max))))
      (if (assq major-mode greader-audiobook-modes)
	  (progn
	    (search-forward
	     (cdr (assq major-mode greader-audiobook-modes))
	     nil t 1)
	    (setq end (point)))
	(pcase greader-audiobook-block-size
	  ((pred numberp)
	   (when
	       (< (+ (point) greader-audiobook-block-size) (point-max))
	     (cond
	      ((> greader-audiobook-block-size 0)
	       (goto-char (+ (point) greader-audiobook-block-size))
	       (when (thing-at-point 'sentence)
		 (forward-sentence))
	       (setq end (point))))))
	  ((pred stringp)
	   (cond
	    ((> (string-to-number greader-audiobook-block-size) 0)
	     (if (< (*
		     (string-to-number greader-audiobook-block-size)
		     (greader-get-rate))
		    words)
		 (progn
		   (forward-word (* (string-to-number
				     greader-audiobook-block-size)
				    (greader-get-rate)))
		   (when (thing-at-point 'sentence)
		     (forward-sentence))
		   (setq end (point)))
	       (setq end (point-max))))))
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
	  (setq text (greader-dehyphenate text))
	  (when (or greader-dict-mode greader-dict-toggle-filters)
	    (setq text (greader-dict-check-and-replace text)))
	  (setq output (call-process command nil nil nil rate language
				     wave-file text))
	  (when (= output 0)
	    (goto-char (cdr block)))
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
    (setq result (apply 'call-process "ffmpeg" nil "*ffmpeg-output*"
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
    (setq result (apply 'call-process "zip" nil "*audiobook-zip*" nil
			zip-args))
    (unless (eq result 0)
      (error "Error while compressing, see buffer *audiobook-zip* for
more information"))))

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
	      (message "converting block %d of %d"
							     output-file-counter
							     total-blocks))
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
	  (when greader-audiobook-compress
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

(provide 'greader-audiobook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; greader-audiobook.el ends here
