;;; isbn.el --- ISBN library for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Maximilian Cook

;; Author: Maximilian Cook <maximiliancook@fclr.net>
;; Created: 05 Mar 2026
;; Version: 0.1.0
;; Keywords: data
;; URL: https://git.fclr.net/maximilian/isbn.el
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Manage ISBNs via elisp.  Currently it is possible to validate,
;; convert and hyphenate ISBNs.

;;;; API

;; In addition to programmatic invocation, each API function can be
;; called interactively (except `isbn-10-p' and `isbn-13-p' since it
;; is obvious to the eye whether an ISBN is an ISBN-10 or ISBN-13).

;; N.B. All below functions expect ISBNs to be strings.

;;;;; Validation

;; - (isbn-valid-p ISBN): Return t if ISBN is a valid ISBN-10 or
;; ISBN-13.
;; - (isbn-10-p ISBN): Return t if ISBN is a valid ISBN-10.
;; - (isbn-13-p ISBN): Return t if ISBN is a valid ISBN-13.

;;;;; Conversion

;; - (isbn-convert ISBN &optional FORMAT): If ISBN is an ISBN-10,
;; convert to ISBN-13; if ISBN-13, ISBN-10.  If FORMAT (integer: 10 or
;; 13) is non-nil, convert ISBN to ISBN-FORMAT.

;;;;; Hyphenation (requires libxml and an internet connection)

;; Hyphenation is made possible by the International ISBN Agency,
;; which provides a complete set of ISBN hyphenation ranges in XML
;; format [1].  Since hyphenation ranges are only updated yearly,
;; ranges are cached to `isbn-hyphenation-ranges-cache-file'.  To
;; cache this file, users must run `isbn-hyphenation-ranges-update' at
;; least once before calling any the of functions below.  Users may
;; wish to add `isbn-hyphenation-ranges-maybe-update' to their
;; `init.el' to check for updates automatically (every 365 days by
;; default).

;; [1] <https://www.isbn-international.org/range_file_generation>

;; - (isbn-hyphenate ISBN): Hyphenate ISBN, returning it as an
;; ISBN-13.  Hyphenating ISBN-10s is currently not supported.

;;; Code:

(require 'dom)
(require 'seq)
(eval-and-compile (require 'subr-x))

(defgroup isbn nil
  "ISBN library for Emacs."
  :prefix "isbn-"
  :group 'data)

;;;; Core

(defun isbn--get-interactively ()
  "Retrieve the ISBN interactively."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (read-string "ISBN: ")))

(defun isbn--list->string (isbn)
  "Convert ISBN (list) to a string."
  (mapconcat
   (lambda (digit)
     (if (eq digit 10) "X"
       (int-to-string digit)))
   isbn))

(defun isbn--string->list (isbn)
  "Convert ISBN (string) to a list of numbers."
  (mapcar
   (lambda (digit)
     (if (eq digit 88) 10
       (string-to-number (char-to-string digit))))
   (upcase (string-replace "-" "" isbn))))

;;;; Validation

;;;###autoload
(defun isbn-valid-p (isbn &optional interactive)
  "Return t if ISBN is valid, otherwise nil.

Optional argument INTERACTIVE, when non-nil, displays a message
concerning ISBN's validation."
  (interactive (list (isbn--get-interactively) t))
  (if (string-match-p (rx (not (any digit ?X ?x ?-))) isbn)
      (when interactive (user-error "ISBN has invalid characters"))
    (setq isbn (isbn--string->list isbn))
    (let ((res (pcase (length isbn)
                 (10 (thread-first
                       (apply #'+
                              (seq-map-indexed
                               (lambda (digit index) (* digit (1+ index)))
                               isbn))
                       (mod 11) (eq 0)))
                 (13 (thread-first
                       (apply #'+
                              (seq-map-indexed
                               (lambda (digit index) (* digit (if (eq (logand index 1) 0) 1 3)))
                               (butlast isbn)))
                       (+ (car (last isbn)))
                       (mod 10) (eq 0))))))
      (when interactive
        (message (if res "ISBN is valid" "ISBN is not valid")))
      res)))

;;;###autoload
(defun isbn-10-p (isbn)
  "Return t if ISBN is matches the ISBN-10 format."
  (and
   (isbn-valid-p isbn)
   (eq (length (isbn--string->list isbn)) 10)))

;;;###autoload
(defun isbn-13-p (isbn)
  "Return t if ISBN is matches the ISBN-13 format."
  (and
   (isbn-valid-p isbn)
   (eq (length (isbn--string->list isbn)) 13)))

;;;; Conversion

;;;###autoload
(defun isbn-convert (isbn &optional format interactive)
  "Convert ISBN between 10- and 13-digit formats.

Optional argument FORMAT (integer: 10, 13) forces conversion to a
specified format.

Optional argument INTERACTIVE, when non-nil, adds the converted ISBN to
the kill ring or, when there is an active region, replaces the active
region with the converted ISBN."
  (interactive (list (isbn--get-interactively) nil t))
  (unless (isbn-valid-p isbn)
    (user-error "ISBN is invalid"))
  (setq isbn (string-replace "-" "" isbn))
  (let ((res (pcase isbn
               ((pred isbn-10-p)
                (if (eq format 10) isbn
                  (let* ((isbn (concat "978" (substring isbn 0 -1)))
                         (checkint (seq-map-indexed
                                    (lambda (digit index) (* digit (if (eq (logand index 1) 0) 1 3)))
                                    (isbn--string->list isbn)))
                         (check (- 10 (mod (apply #'+ checkint) 10))))
                    (format "%s%i" isbn check))))
               ((pred isbn-13-p)
                (if (eq format 13) isbn
                  (unless (string-prefix-p "978" isbn)
                    (user-error "ISBN does not start with `978'"))
                  (let* ((isbn (string-remove-prefix "978" isbn))
                         (checkint (seq-map-indexed
                                    (lambda (digit index) (* digit (1+ index)))
                                    (isbn--string->list (substring isbn 0 -1))))
                         (check (mod (apply #'+ checkint) 11)))
                    (concat (substring isbn 0 -1)
                            (if (eq check 10) "X" (int-to-string check))))))
               (_ (user-error "ISBN is not 10 or 13 digits")))))
    (when interactive
      (if (region-active-p)
          (replace-region-contents (region-beginning) (region-end) res)
        (kill-new res)
        (message "Converted ISBN added to kill ring")))
    res))

;;;; Hyphenation

(defcustom isbn-hyphenation-ranges-cache-file
  (file-name-concat user-emacs-directory "isbn-hyphenation-ranges.eld")
  "File containing a cache of ISBN hyphenation ranges."
  :type 'file
  :group 'isbn
  :version "30.2"
  :package-version '((isbn . "0.1.0")))

;; Cache the ranges so that nested operations (hyphenation ->
;; validation) do not request the ranges needlessly; they do not
;; update that often.
(defvar isbn-hyphenation-ranges-cache nil
  "Cache of all valid ISBN ranges from the International ISBN Agency.")

(defcustom isbn-hyphenation-ranges-update-interval 365
  "Interval in days (integer) which must elapse before updating ranges.
Set to nil to prevent automatic updates.

ISBN ranges from the International ISBN Agency are updated yearly."
  :type '(choice (integer :tag "Days")
                 (const :tag "Do not update" nil))
  :group 'isbn
  :version "30.2"
  :package-version '((isbn . "0.1.0")))

(defun isbn-hyphenation-ranges-update (&optional nofile)
  "Update cache of ISBN hyphenation ranges.

Optional argument NOFILE, if non-nil, prevents writing to
`isbn-hyphenation-ranges-cache-file'."
  (interactive)
  (let ((prgrp (progress-reporter-make
                "Fetching hyphenation ranges from `www.isbn-international.org'...")))
    (condition-case-unless-debug nil
        (with-current-buffer
            (url-retrieve-synchronously
             "https://www.isbn-international.org/export_rangemessage.xml"
             :silent :inhibit-cookies 5)
          (let* ((ranges (libxml-parse-xml-region (point))))
            (unless ranges (signal 'error))
            (unless nofile
              (write-region
               (concat ";; TIME: " (prin1-to-string (current-time)) "\n")
               :ignored isbn-hyphenation-ranges-cache-file)
              (write-region
               (prin1-to-string ranges) :ignored
               isbn-hyphenation-ranges-cache-file :append))
            (setq isbn-hyphenation-ranges-cache ranges)
            (progress-reporter-done prgrp))
          (lambda (_status &rest _cbargs)))
      (error (error "Failed to retrieve hyphenation data")))))

(defun isbn-hyphenation-ranges-maybe-update ()
  "Update ranges cache if enough time has elapsed.
The amount of time before updating is determined by
`isbn-hyphenation-ranges-update-interval'."
  (require 'time-date)
  (when (and isbn-hyphenation-ranges-update-interval
             (file-exists-p isbn-hyphenation-ranges-cache-file))
    (let ((time (with-temp-buffer
                  (insert-file-contents isbn-hyphenation-ranges-cache-file nil 0 64)
                  (goto-char (point-min)) (search-forward "TIME:" (line-end-position))
                  (read (string-trim (buffer-substring (point) (line-end-position)))))))
      (when (> (time-to-number-of-days (time-since time)) isbn-hyphenation-ranges-update-interval)
        (isbn-hyphenation-ranges-update)))))

;;;###autoload
(defun isbn-hyphenate (isbn &optional interactive)
  "Hyphenate ISBN as an ISBN-13.

Optional argument INTERACTIVE, when non-nil, adds the hyphenated ISBN to
the kill ring or, when there is an active region, replaces the region
with the hyphenated ISBN."
  (interactive (list (isbn--get-interactively) t))

  (unless (libxml-available-p)
    (user-error "Hyphenation requires libxml2 support"))

  (unless isbn-hyphenation-ranges-cache
    (let ((prgrp (progress-reporter-make "Loading hyphenation ranges from cache...")))
      (with-temp-buffer
        (condition-case-unless-debug nil
            (insert-file-contents isbn-hyphenation-ranges-cache-file)
          (error (error "Hyphenation ranges are not cached.  \
Please run `isbn-hyphenation-ranges-update'")))
        (setq isbn-hyphenation-ranges-cache (read (current-buffer))))
      (progress-reporter-done prgrp)))

  (setq isbn (isbn-convert isbn 13))

  ;; The data from the International ISBN Agency comes in two sets:
  ;; the EAN prefix (978 or 979) and the registration groups.  The
  ;; former determines how long the group identifier is, and the
  ;; latter determines how long the publisher code is.  The rest of
  ;; the ISBN can be determined automatically.
  (let* (hyphenated
         (ean (substring isbn 0 3))
         (remaining
          (lambda ()
            (let ((len (length (apply #'concat hyphenated))))
              (substring isbn len (+ len 7)))))
         (length-from-range
          (lambda (dom number)
            (setq number (string-to-number number))
            (thread-last
              (dom-child-by-tag rule 'Length)
              dom-text string-to-number
              (throw 'len)
              (when (and (>= number (car range)) (<= number (cadr range))))
              (let ((range (mapcar #'string-to-number
                                   (string-split (dom-text (dom-child-by-tag rule 'Range)) "-")))))
              (dolist (rule (dom-by-tag dom 'Rule)))
              (catch 'len)))))

    ;; 1. EAN prefix
    (push ean hyphenated)

    ;; 2. Group identifier
    (condition-case-unless-debug nil
        (let* ((rem (funcall remaining))
               ;; The DOM holds several `Prefix' nodes, but only two
               ;; reside under an `EAN.UCC' node.  One of these
               ;; *should* contain the EAN prefix as its only text.
               (len (thread-last
                      (throw 'len (funcall length-from-range n rem))
                      (when (string= (dom-text (dom-child-by-tag n 'Prefix)) ean))
                      (dolist (n (dom-by-tag isbn-hyphenation-ranges-cache 'EAN.UCC)))
                      (catch 'len))))
          (unless (eq len 0)
            (push (substring rem 0 len) hyphenated)))
      (error (error "Could not determine group identifier")))

    ;; 3. Publisher code
    (condition-case-unless-debug nil
        (let* ((prefix (string-join (reverse hyphenated) "-"))
               (rem (funcall remaining))
               (len (thread-last
                      (funcall length-from-range n rem)
                      (throw 'len)
                      (when (string= (dom-text (dom-child-by-tag n 'Prefix)) prefix))
                      (dolist (n (dom-by-tag isbn-hyphenation-ranges-cache 'Group)))
                      (catch 'len))))
          (push (substring rem 0 len) hyphenated))
      (error (error "Could not determine publisher code")))

    ;; 4. Title identifier
    (push (substring isbn (length (string-join hyphenated)) -1) hyphenated)

    ;; 5. Check digit
    (push (substring isbn -1) hyphenated)

    (setq isbn (string-join (reverse hyphenated) "-")))
  (when interactive
    (if (region-active-p)
        (replace-region-contents (region-beginning) (region-end) isbn)
      (kill-new isbn)
      (message "Hyphenated ISBN added to kill ring")))
  isbn)

(provide 'isbn)

;;; isbn.el ends here
