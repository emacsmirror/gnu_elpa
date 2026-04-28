;;; mstest-sections.el --- MATLAB Shell sections tests -*- lexical-binding: t; -*-

;; Author: John Ciolfi <john.ciolfi.32@gmail.com>

;; Copyright (C) 2025 Free Software Foundation, Inc.
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;    Test matlab-sections.el using files in ./mstest-sections-files/

;;; Code:

(require 'matlab-ts-mode)
(require 'matlab-sections)

(declare-function mstest-get-command-output "mstest.el")

(defun mstest-sections-header ()
  "Exercise the regexp that identifies the \"%% section\" header."

  (message "TEST: running (mstest-sections-header)")

  (let ((header-comments
         '("%% description of header" ;; typical section header
           "%%"   ;; section header without a description
           "%%  " ;; section header without a description and ending in spaces
           " \t %% description with starting spaces and tabs"
           "%% description can end with spaces and tabs  \t  "
           "%% description ending in 1%" ;; ending in <CHAR>% is a section header
           ))
        (non-header-comments
         '("% not a section header"
           "% this is %% not a section header"
           "%%% using three or more percents is not a section header"
           "%%%%%%%%%%%%%%%%%%%" ;; comment block
           "%% comment block %%" ;; ending in % is a comment block
           "%%%%%%%%%%%%%%%%%%%" ;; comment block
           )))
    (dolist (header-comment header-comments)
      (when (not (string-match matlab-sections-section-break-regexp header-comment))
        (user-error "Failed to match \"%s\" as a section header comment" header-comment)))

    (dolist (non-header-comment non-header-comments)
      (when (string-match matlab-sections-section-break-regexp non-header-comment)
        (user-error "Matched \"%s\" as a section header comment when it should have failed to match"
                    non-header-comment))))

  (message "PASSED: (mstest-single-sections-header)"))

(defun mstest-sections--run-section (test-point-desc expected-matlab-out)
  "Run `matlab-sections-run-section' for TEST-POINT-DESC on current buffer.
Validate we get EXPECTED-MATLAB-OUT string in the *MATLAB* buffer.
Also validate we see both the current buffer and *MATLAB* buffer."

  (let* ((m-file-name (buffer-name))
         (got-matlab-out (mstest-get-command-output 'matlab-sections-run-section))
         (got-buffers (mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list)))
         (expected-buffers `(,m-file-name "*MATLAB*")))

    (when (not (string= got-matlab-out expected-matlab-out))
      (user-error "Unexpected result for %s. Got '%s' expected '%s'" test-point-desc
                  got-matlab-out expected-matlab-out))

    (when (not (equal got-buffers expected-buffers))
      (user-error "Uexpected result for %s. Got buffers '%s' expected '%s'" test-point-desc
                  got-buffers expected-buffers))

    (message "PASS: %s" test-point-desc)))

(defun mstest-sections ()
  "Test \"%% code section\" support."

  (message "TEST: running (mstest-sections)")

  (save-excursion
    (let ((sections-buf (find-file "mstest-sections-files/sections.m")))

      ;; We run in batch, so need to explicitly enable sections
      (matlab-sections-mode-enable)
      (when (not matlab-sections-minor-mode)
        (user-error "Failed to start matlab-sections-minor-mode"))

      (let ((test-point-desc "matlab-sections test case point-min backup"))
        ;;  at point-min, we shouldn't get a lisp error if we try to backup
        (goto-char (point-min))
        (matlab-sections-backward-section)
        (when (not (= (point) (point-min)))
          (user-error "Unexpected result for %s" test-point-desc))
        (message "PASS: %s" test-point-desc))

      (let ((test-point-desc "matlab-sections test case point-min forward"))
        (matlab-sections-forward-section)
        (when (not (looking-at "^sectionOneA = 1"))
          (user-error "Unexpected result for %s, point=%d" test-point-desc (point)))
        (message "PASS: %s" test-point-desc))

      (let ((test-point-desc "matlab-sections test case end-of-section"))
        (matlab-sections-end-of-section)
        (forward-line -1)
        (when (not (looking-at "^sectionOneB = 1"))
          (user-error "Unexpected result for %s" test-point-desc))
        (message "PASS: %s" test-point-desc))

      (let ((test-point-desc "matlab-sections test case beginning-of-section"))
        (matlab-sections-beginning-of-section)
        (when (not (looking-at "^sectionOneA = 1"))
          (user-error "Unexpected result for %s" test-point-desc))
        (message "PASS: %s" test-point-desc))

      (let ((test-point-desc "matlab-sections test case mark section one"))
        (save-excursion
          (matlab-sections-mark-section)
          (let ((mark-start (mark))
                (mark-end (point)))
            (when (or (not (= mark-start 36))
                      (not (= mark-end 85)))
              (user-error "Unexpected result for %s" test-point-desc))
            (deactivate-mark)
            (message "PASS: %s" test-point-desc))))

      (let ((test-point-desc "matlab-sections test case move section down"))
        (matlab-sections-beginning-of-section)
        (matlab-sections-move-section-down)
        (when (not (string= (buffer-substring (point-min) (point-max))
                            "\
sectionZeroA = 1
sectionZeroB = 1

%% section two

sectionTwoA = 1
sectionTwoB = 1
%% section one

sectionOneA = 1
sectionOneB = 1

"))
          (user-error "Unexpected result for %s" test-point-desc))
        (set-buffer-modified-p nil)
        (message "PASS: %s" test-point-desc))

      (let ((test-point-desc "matlab-sections test case move section up"))
        (matlab-sections-move-section-up)
        (when (not (string= (buffer-substring (point-min) (point-max))
                            "\
sectionZeroA = 1
sectionZeroB = 1

%% section one

sectionOneA = 1
sectionOneB = 1

%% section two

sectionTwoA = 1
sectionTwoB = 1
"))
          (user-error "Unexpected result for %s" test-point-desc))
        (set-buffer-modified-p nil)
        (message "PASS: %s" test-point-desc))

      (let ((test-point-desc "matlab-sections test case forward to section two"))
        (matlab-sections-forward-section)
        (when (not (looking-at "^sectionTwoA = 1"))
          (user-error "Unexpected result for %s" test-point-desc))
        (message "PASS: %s" test-point-desc))

      (let ((test-point-desc "matlab-sections test case backup to section one"))
        (save-excursion ;; save-excursion, so we remain at section two when test case is done
          (matlab-sections-backward-section)
          (when (not (looking-at "^sectionOneA = 1"))
            (user-error "Unexpected result for %s" test-point-desc))
          (message "PASS: %s" test-point-desc)))

      (let* ((test-point-desc "matlab-sections test case run section two"))
        (mstest-sections--run-section test-point-desc
                                      "
sectionTwoA =

     1


sectionTwoB =

     1

emacsrunregion: finished running sections.m lines 9 to 13"))
             

      (let ((test-point-desc "matlab-sections test case run prior sections, zero and one")
            (got-matlab-out (mstest-get-command-output 'matlab-sections-run-prior-sections))
            (expected-matlab-out "
sectionZeroA =

     1


sectionZeroB =

     1


sectionOneA =

     1


sectionOneB =

     1

emacsrunregion: finished running sections.m lines 1 to 10"))
        (when (not (string= got-matlab-out expected-matlab-out))
          (user-error "Unexpected result for %s. Got '%s' expected '%s'" test-point-desc
                      got-matlab-out expected-matlab-out))

        (message "PASS: %s" test-point-desc))

      (kill-buffer sections-buf)))

  (message "PASSED: (mstest-sections)"))

(defun mstest-sections-single ()
  "Test \"%% code section\" support on a script with one section."
  (if (and (>= emacs-major-version 30)
           (fboundp 'treesit-ready-p)
           (treesit-ready-p 'matlab t))
      (save-excursion
        (message "TEST: running (mstest-sections-single)")
        (let ((sections-single-buf (find-file "mstest-sections-files/sections_single.m"))
              (mfile-type (matlab-ts-mode--mfile-type)))

          (when (not (eq mfile-type 'script))
            (user-error "%s is mfile-type is not 'script" (current-buffer)))

          (font-lock-mode 1)
          (font-lock-ensure (point-min) (point-max))
          (font-lock-fontify-region (point-min) (point-max))

          ;; When noninteractive or running via M-: (mstest-sections),
          ;; force `matlab-sections-minor-mode'
          (matlab-sections-minor-mode 1)
          (font-lock-flush (point-min) (point-max))

          (goto-char (point-min))
          (re-search-forward "^%% one section")
          (beginning-of-line)
          (let ((test-point-desc "matlab-sections-single test case heading face")
                (got (face-at-point))
                (expected 'matlab-sections-highlight-face))
            (when (not (eq got expected))
              (user-error "Unexpected result for %s. Got '%s' expected '%s'"
                          test-point-desc got expected))
            (message "PASS: %s" test-point-desc))

          (kill-buffer sections-single-buf))
        (message "PASSED: (mstest-sections-single)"))
    (message "mstest-sections.el:1: warning: matlab tree-sitter shared object is not installed, \
unable to run mstest-sections-single")))

(provide 'mstest-sections)
;;; mstest-sections.el ends here

;; LocalWords:  gmail defun buf dolist
