;;; t-utils.el --- Test utilities -*- lexical-binding: t -*-
;;
;; Copyright 2025 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; Commentary:
;;
;; Test utilities used by test-*.el files.
;;
;; Most of the test utilities provided by t-utils are "looping tests".  For example, let's suppose
;; we wish to test the font-lock of LANGUAGE-ts-mode.el for *.lang files.  We create these files:
;;
;;    ./LANGUAGE-ts-mode.el
;;    ./tests/t-utils.el
;;    ./tests/test-LANGUAGE-ts-mode-font-lock.el
;;    ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test1.lang
;;    ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test1_expected.txt
;;    ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test2.lang
;;    ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test2_expected.txt
;;    <snip>
;;
;; ./tests/test-LANGUAGE-ts-mode-font-lock.el is shown below.  You write this and it loops
;; over all ./tests/test-LANGUAGE-ts-mode-font-lock-files/*.lang files.
;;
;; To add tests, create files of form
;;   ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test1.lang
;; and then
;;
;;   M-x ert
;;   Run tests: test-LANGUAGE-ts-mode-font-lock
;;
;; This will create ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test1_expected.txt~ and
;; after examining it, rename it to
;; ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test1_expected.txt.
;;
;; When you run ert interactively, you'll be presented with an ert result buffer.  You can
;; type "m" on the colored dots in the ert result buffer to see the messages for that ert test
;; and the messages contain the sub-tests from the test loop for that ert test.  This will bring
;; up an *ERT Messages* buffer.  In this buffer, type
;;   M-x compilation-minor-mode
;; to view the and navigate errors.  The default error viewing in the ert result buffer is a bit
;; dense due to the looping nature of the t-utils tests.
;;
;; To run your tests in a build system, use
;;
;;   Emacs --batch -Q -l t-utils -eval t-utils-run
;;
;; ----------------------------------------------
;; | ./tests/test-LANGUAGE-ts-mode-font-lock.el |
;; ----------------------------------------------
;; (require 't-utils)
;; (require 'language-ts-mode)
;;
;; (defvar test-LANGUAGE-ts-mode-font-lock--file nil)
;;
;; (defun test-LANGUAGE-ts-mode-font-lock--file (lang-file)
;;   "Test an individual LANG-FILE.
;; This is provided for debugging.
;;   M-: (test-LANGUAGE-ts-mode-font-lock--file "test-LANGUAGE-ts-mode-font-lock-files/LANG-FILE")"
;;   (let ((test-LANGUAGE-ts-mode-font-lock--file lang-file))
;;     (ert-run-tests-interactively "test-LANGUAGE-ts-mode-font-lock")))
;;
;; (ert-deftest test-LANGUAGE-ts-mode-font-lock ()
;;   "Test font-lock using ./test-LANGUAGE-ts-mode-font-lock-files/NAME.lang.
;; Compare font of ./test-LANGUAGE-ts-mode-font-lock-files/NAME.lang against
;; ./test-LANGUAGE-ts-mode-font-lock-files/NAME_expected.txt, where
;; NAME_expected.txt is of same length as NAME.lang where each source
;; character in NAME.lang is replaced with a character code representing the
;; font-lock face used for said source character.  The mapping is defined
;; by the code-to-face alist setup by this function.  This loops
;; on all ./test-LANGUAGE-ts-mode-font-lock-files/NAME.lang files.
;;
;; To add a test, create
;;   ./test-LANGUAGE-ts-mode-font-lock-files/NAME.lang
;; and run this function.  The baseline is saved for you as
;;   ./test-LANGUAGE-ts-mode-font-lock-files/NAME_expected.txt~
;; after validating it, rename it to
;;   ./test-LANGUAGE-ts-mode-font-lock-files/NAME_expected.txt"
;;
;;   (let* ((test-name "test-LANGUAGE-ts-mode-font-lock")
;;          (LANGUAGE-ts-mode-font-lock-level 4)
;;          (lang-files (t-utils-get-files
;;                    test-name
;;                    (rx ".lang" eos)
;;                    nil
;;                    test-LANGUAGE-ts-mode-font-lock--file))
;;          (code-to-face '(
;;                          ("b" . font-lock-bracket-face)
;;                          ("B" . font-lock-builtin-face)
;;                          ("c" . font-lock-comment-face)
;;                          ;; <add-more-as-needed>
;;                          )))
;;     (t-utils-error-if-no-treesit-for 'LANGUAGE test-name)
;;     (t-utils-test-font-lock test-name lang-files code-to-face)))
;;
;; ------------------
;; | Skipping tests |
;; ------------------
;; Since the ert package doesn't provide a looping facility and t-utils.el tests are mostly
;; looping, to skip tests we cannot use the ert facilities to skip a test because that would
;; disable testing when we want to run the test but skip a few of the input files.  Therefore,
;; to skip tests you should create *.skip.txt files.  For example, consider the font-lock looping
;; test above where it has three ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test*.lang
;; input files.  Suppose there's an issue with the 2nd input file.  We create a
;;   ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test2.skip.txt
;; containing the reason for skipping the test, e.g.
;; "see issue 123, when resolved delete this *.skip.txt file".
;;
;;    ./LANGUAGE-ts-mode.el
;;    ./tests/t-utils.el
;;    ./tests/test-LANGUAGE-ts-mode-font-lock.el
;;    ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test1.lang
;;    ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test1_expected.txt
;;    ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test2.lang
;;    ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test2.skip.txt
;;    ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test2_expected.txt
;;    ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test3.lang
;;    ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test3_expected.txt
;;
;; When we
;;   M-x ert
;;   Run tests: test-LANGUAGE-ts-mode-font-lock
;;
;; and examine the *Messages* buffer for the test, we'll a message that
;; ./tests/test-LANGUAGE-ts-mode-font-lock-files/font_lock_test2.lang was skipped.
;;
;; ---------------
;; | Sweep tests |
;; ---------------
;; Another type of test is a sweep test that takes a directory tree and runs actions on every
;; file matching a pattern.  The actions look for errors signal, etc.  See
;; `t-utils-sweep-test-indent' for an example.
;;
;; ----------
;; | Issues |
;; ----------
;; t-utils.el uses the ert package.  Many of t-utils functions operate on a set of input files and
;; compare them against baselines.  For example, `t-utils-test-font-lock' loops over a set of files,
;; NAME.LANG, and compares them against NAME_expected.txt.  The ert package does not provide a
;; looping facility.  Therefore, t-utils internally performs the looping.  This makes reporting a
;; little off.  One test is really a number of tests defined by the test input files.  To debug a
;; specific input file, the caller of the t-utils needs to setup for debugging.  See
;; `t-utils-test-font-lock' above for this setup.
;;


;;; Code:

(require 'cl-macs)
(require 'cl-seq)
(require 'diff)
(require 'ert)
(require 'outline)
(require 'treesit)

;; Add abs-path of ".." to load-path so we can require packages from above us.
(let* ((lf (or load-file-name (buffer-file-name (current-buffer))))
       (d1 (file-name-directory lf))
       (parent-dir (expand-file-name (file-name-directory (directory-file-name d1)))))
  (add-to-list 'load-path parent-dir t))

(defun t-utils--trim ()
  "Trim trailing whitespace and lines with utf-8-unix encoding."
  (setq buffer-file-coding-system 'utf-8-unix)
  (let ((delete-trailing-lines t))
    (delete-trailing-whitespace (point-min) (point-max))))

(defun t-utils--skip-message (test-file skip-file &optional test-description)
  "Display message, skipping TEST-FILE because of SKIP-FILE.
Optional TEST-DESCRIPTION defaults to \"this test input\"."
  (let ((skip-file-contents (with-temp-buffer
                              (insert-file-contents-literally skip-file)
                              (string-trim-right
                               (replace-regexp-in-string "^" "    "
                                                         (buffer-string))))))
    (message "%s:1: warning: skipping %s because %s exists\n%s"
             test-file (or test-description "this test input") skip-file skip-file-contents)))


(defun t-utils-get-files (test-name base-regexp &optional skip-regexp file-to-use)
  "Return list of test input files, /abs/path/to/TEST-NAME-files/FILE.LANG.
The basename of each returned file matches BASE-REGEXP and not optional
SKIP-REGEXP.  Optional FILE-TO-USE narrow the list of full paths to that
file and the result is a list of one file.

For each:   /abs/path/to/TEST-NAME-files/FILE.LANG
if exists:  /abs/path/to/TEST-NAME-files/FILE.skip.txt
then this test input file is skipped.

TEST-NAME is used to locate the TEST-NAME-files directory.

For example,
  (t-utils-get-files \"test-LANGUAGE-ts-mode-indent\"
                     \"\\\\.lang\\\\\\='\" \"_expected\\\\.lang\\\\\\='\" nil)
will return a list of files matching glob
   /abs/path/to/test-LANGUAGE-ts-mode-indent-files/*.lang
skipping all *_expected.lang files."

  (let* ((test-file (or (symbol-file (intern test-name))
                        (error "Failed to locate test file for test-name, %s" test-name)))
         (files-dir (replace-regexp-in-string "\\.el\\'" "-files" test-file))
         (files (cl-delete-if (lambda (file)
                                (and skip-regexp
                                     (string-match skip-regexp file)))
                              (directory-files files-dir t base-regexp))))
    (when file-to-use
      ;; Narrow result to (list (file-truename (file-to-use)))
      (let ((true-file-to-use (file-truename file-to-use)))
        (when (not (member true-file-to-use files))
          (if (file-exists-p true-file-to-use)
              (error (concat "File %s, resolved to %s, is not a valid selection.\n"
                             "It should be one of %S" file-to-use true-file-to-use files))
            (error "File %s does not exist" file-to-use)))
        (setq files (list true-file-to-use))))

    ;; File each NAME.LANG in files, remove it when a corresponding FILE.skip.txt exists.
    (let ((files-not-skipped '()))
      (dolist (file files)
        (let ((skip-file (replace-regexp-in-string "\\.[^.]\\'" ".skip.txt" file)))
          (if (file-exists-p skip-file)
              (t-utils--skip-message file skip-file)
            ;; Not skipped. Note we ignore hidden link files, e.g. .#foo.lang
            (when (not (string-match-p "\\`\\.#" file))
              (push file files-not-skipped)))))

      ;; Sorted result
      (sort files-not-skipped))))

(defun t-utils-error-if-no-treesit-for (language test-name)
  "Error if tree-sitter for LANGUAGE is not available.
If not available an error containing TEST-NAME is generated."
  (when (not (treesit-ready-p language t))
    (error "Test %s requires treesit for language %s and it's not available" test-name language)))

(defun t-utils-run (&optional match)
  "Run test files in current directory matching regexp, MATCH.
If optional MATCH is non-nil, only run test file names whose
non-directory part matches the regexp, MATCH.  For example,
\"^test-foo.*\\\\.el\\\\\\='\" would run tell t-utils-run to run
\"test-foo*.el\" files.  The default MATCH is
\"^test-.*\\\\.el\\\\\\='\".

It is assumed each matched test contains `ert-deftest' that has the same
name as the test file.  These test names are then run using `ert'."

  (interactive)

  (when (not match)
    (setq match "^test-.*\\.el\\'"))

  (let ((tests '()))

    (dolist (test-file (directory-files "." t match))
      (when (not (load-file test-file))
        (error "Failed to load %s" test-file))
      (push (replace-regexp-in-string "\\.el" "" (file-name-nondirectory test-file)) tests))

    (let ((test-selector (rx-to-string `(seq bos (or ,@tests) eos) t)))
      (if noninteractive
          (ert-run-tests-batch-and-exit test-selector)
        (ert-run-tests-interactively test-selector)))))

(defvar-local t-utils--buf-file nil
  "Name of file associated with t-utils temporary buffers.")

(defun t-utils--took (start-time)
  "Return \"- took N seconds\".
N is `current-time' minus START-TIME."
  (format "- took %.2f seconds" (float-time (time-subtract (current-time) start-time))))

(defun t-utils--log (log-file string &optional create)
  "Append STRING to LOG-FILE.
If CREATE is t, create LOG-FILE instead of appending"
  (let ((coding-system-for-write 'no-conversion))
    (write-region string nil log-file (not create))))

(defun t-utils--log-create (test-name log-file)
  "Create LOG-FILE with \"START: TEST-NAME\" content.
Returns LOG-FILE truename"

  (setq log-file (file-truename (or log-file (concat test-name ".log"))))
  (t-utils--log log-file (format "START: %s\n" test-name) t)
  (message "Log: %s" log-file)
  log-file)

(defun t-utils--baseline-check (test-name start-time
                                          lang-file got got-file expected expected-file
                                          &optional checker-fun)
  "Validate GOT string matches EXPECTED for LANG-FILE of TEST-NAME.
GOT-FILE is equal to EXPECTED-FILE with a tilde prefix.  GOT-FILE will
be created if (string= GOT EXPECTED) is nil.  EXPECTED-FILE is the
baseline file for EXPECTED.  START-TIME is when we started the test and
is used in displaying the test took time.

Optional CHECKER-FUN if specified is called with LANG-FILE GOT GOT-FILE
EXPECTED EXPECTED-FILE.  CHECKER-FUN should cons
  (error-messages . do-baseline-check)
where error-messages is a list of error messages or nil indicating
success.  Boolean, do-baseline-check, indicates whether or not to do the
baseline check of GOT vs EXPECTED.

Returns nil on success, otherwise an error message list of strings if
baseline check fails."

  (let (error-msg
        (do-baseline-check t))

    (when checker-fun
      (let ((result (funcall checker-fun lang-file got got-file expected expected-file)))
        (when result
          (when (not (consp result))
            (error "Baseline checker-fun for %s on %s didn't return a cons" test-name lang-file))
          (when (car result)
            (when (not (listp (car result)))
              (error "Baseline checker-fun for %s on %s didn't return a valid list for (car result)"
                     test-name lang-file))
            (setq error-msg (car result)))
          (setq do-baseline-check (cdr result)))))

    (when (not (string= got expected))
      (let ((coding-system-for-write 'raw-text-unix))
        (write-region got nil got-file))

      (when do-baseline-check
        (let (baseline-errors)
          (if (not expected)
              (setq baseline-errors (list
                                     (format "Test: %s" lang-file)
                                     (format "Got: %s" got-file)
                                     (format "NoBaseline: if \"Got\" is good, rename it to %s"
                                             expected-file)))
            (setq baseline-errors (list
                                   (format "Test: %s" lang-file)
                                   (format "Got: %s" got-file )
                                   (format "Expected: %s" expected-file))))
          (when baseline-errors
            (if error-msg
                (setq error-msg (append error-msg baseline-errors))
              (setq error-msg baseline-errors))))))

    ;; When run non-interactively, having errors show up like compiler messages aids in finding
    ;; them. For example, run the test and pipe to a log file, then view the log file in
    ;; `compilation-minor-mode'.
    (when error-msg
      (message "%s:1: error: test failed" lang-file)
      (dolist (msg error-msg)
        (message "-> %s" msg)))

    ;; Report the status of the test.
    (message "%s: %s %s %s"
             (if error-msg "FAIL" "PASS")
             test-name lang-file (t-utils--took start-time))
    error-msg))

(defun t-utils--display-result (test-name directory result &optional result-file)
  "Display a test RESULT.
Save result to RESULT-FILE, defaulting to TEST-NAME.result.txt.  If
noninteractive display message \"See: RESULT-FILE\".  If interactive
show *TEST-NAME* result buffer with `default-directory' set to DIRECTORY
containing RESULT."

  (setq result (concat
                "# -*- mode: fundamental; eval: (compilation-minor-mode 1) -*-\n\n"
                result))
  (if noninteractive
      (let ((coding-system-for-write 'no-conversion))
        (setq result-file (file-truename (or result-file (concat test-name ".result.txt"))))
        (write-region result nil result-file)
        (message "See: %s" result-file))
    (let ((result-buf (get-buffer-create (concat "*" test-name "*"))))
      (with-current-buffer result-buf
        (read-only-mode -1)
        (buffer-disable-undo)
        (setq-local default-directory (file-truename directory))
        (erase-buffer)
        (insert result)
        (goto-char (point-min))
        (fundamental-mode)
        (compilation-minor-mode 1) ;; this lets us navigate to errors (would be nice to disable "g")
        (set-buffer-modified-p nil)
        (read-only-mode 1))
      (display-buffer result-buf))))

(defun t-utils--insert-file-for-test (file
                                      &optional file-major-mode setup-callback skip-corrupt-check)
  "Insert FILE into current temporary buffer for testing.
If optional FILE-MAJOR-MODE function is provided, run that, otherwise
we examine the first line of the file for the major mode:

  -*- MODE-NAME -*-
  -*- mode: MODE-NAME -*-

and run that.

If optional SETUP-CALLBACK is specified, it is invoked after setting
the major mode in the temporary buffer.

If optional SKIP-CORRUPT-CHECK is non-nil, the check for corrupted content is
skipped."

  (insert-file-contents-literally file)

  ;; We're testing a programming language which is using utf-8-unix encoding
  (set-buffer-file-coding-system 'utf-8-unix)

  ;; Check for corrupted characters (these can crash Emacs via the language server parser)
  (when (not skip-corrupt-check)
    (goto-char (point-min))
    (when (re-search-forward "[^[:print:][:space:]]" nil t)
      (error "%s appears corrupt, non-printable utf8 character at point %d: %c"
             file (point) (char-before))))

  ;; CRLF -> LF for consistency between Unix and Windows
  (goto-char (point-min))
  (while (re-search-forward "\r" nil t)
    (replace-match ""))
  (goto-char (point-min))

  ;; Set mode
  (if file-major-mode
      (funcall file-major-mode)
    ;; else get major mode from the first line of the file.
    (when (and (not (looking-at "^.* -\\*-[ \t]+\\([-a-z0-9]+\\)[ \t]+-\\*-"))
               (not (looking-at "^.* -\\*-.*mode:[ \t]+\\([-a-z0-9]+\\).*-\\*-")))
      (error "First line of %s must contain -*- MODE-NAME -*- (or -*- mode: MODE-NAME -*-)" file))
    (let* ((mode (match-string 1))
           (mode-cmd (intern (concat mode "-mode"))))
      (funcall mode-cmd)))

  (when setup-callback
    (funcall setup-callback))

  ;; Incase the mode moves the point, reset to point-min.
  (goto-char (point-min))

  ;; Stash away the real buffer file for later use (and return it).
  (setq-local t-utils--buf-file file))

(defun t-utils--get-buf-file ()
  "Return the file corresponding to the buffer under test."
  (cond ((and (local-variable-if-set-p 't-utils--buf-file)
              t-utils--buf-file)
         t-utils--buf-file)
        ((buffer-file-name)
         (buffer-file-name))
        (t
         (error "This must be invoked from within a file buffer"))))

(defun t-utils--diff-strings-impl (start-contents end-contents)
  "Implementation for `t-utils-diff-string'.
Returns diff of START-CONTENTS and END-CONTENTS."
  (with-temp-buffer
    (let* ((tmp-name-prefix (condition-case nil
                                (t-utils--get-buf-file)
                              ( error "t-utils--tmp-file")))
           (start-tmp-file (make-temp-file (concat tmp-name-prefix ".start.") nil ".txt"
                                           start-contents))
           (end-tmp-file (make-temp-file (concat tmp-name-prefix ".end.") nil ".txt"
                                         end-contents))
           (diff-buf (current-buffer)))

      (diff-no-select start-tmp-file end-tmp-file "-u" t diff-buf)

      (read-only-mode -1)

      ;; Delete the "diff -u start-file end-file" command
      (goto-char (point-min))
      (re-search-forward "^--- ")
      (beginning-of-line)
      (delete-region (point-min) (point))

      ;; Remove temp file names and time stamps to make output stable and easier to read
      (re-search-forward "^--- .+$")
      (replace-match "--- start_contents")
      (re-search-forward "^\\+\\+\\+ .+$")
      (replace-match "+++ end_contents")

      ;; Remove the diff finished buffer info. At end of buffer there's a blank line then
      ;; "Diff finished. TIME"
      (goto-char (point-max))
      (forward-line -2)
      (delete-region (point) (point-max))

      (delete-file start-tmp-file)
      (delete-file end-tmp-file)

      (let ((diff-result (buffer-substring-no-properties (point-min) (point-max))))
        (kill-buffer diff-buf)
        diff-result))))

(defvar t-utils--diff-checked nil)

(defun t-utils--diff-check ()
  "Validate diff is setup correctly."

  (let* ((s1 (concat "L1\n" "L2\n" "L3\n" "L4\n" "L5\n" "L6\n" "L7\n" "L8\n" "L9\n" "L10\n"))
         (s2 (replace-regexp-in-string "L5" "L5-MODIFIED" s1))
         (got (t-utils--diff-strings-impl s1 s2))
         (expected (concat "--- start_contents\n"
                           "+++ end_contents\n"
                           "@@ -2,7 +2,7 @@\n"
                           " L2\n L3\n L4\n-L5\n+L5-MODIFIED\n L6\n L7\n L8\n")))
    (when (not (string= got expected))
      (error "Running diff produced unexpected results.
Verify that diff is setup correctly, check `diff-command', etc.
You can run `t-utils--diff-check' to debug"))))

(defun t-utils--diff-strings (start-contents end-contents)
  "Return diff of START-CONTENTS and END-CONTENTS."

  ;; Do a one time diff on sample start/end contents vs expected result
  (when (not t-utils--diff-checked)
    (t-utils--diff-check)
    (setq t-utils--diff-checked t))

  (t-utils--diff-strings-impl start-contents end-contents))

(defun t-utils--get-point-for-display (point)
  "Return \"<point> L<num> C<num>\" for POINT."
  (format "%d L%d C%d"
          point
          (line-number-at-pos point)
          (save-excursion (goto-char point)
                          (current-column))))

(defun t-utils-xr-print-code (start-point end-point)
  "For use in `t-utils-xr' to print a code block.
The buffer text from START-POINT to END-POINT is captured and placed in
in a code block using the `major-mode' of the buffer:
  #+begin_src MAJOR-MODE
     <result>
  #+end_src"
  (print (buffer-substring-no-properties start-point end-point)))

(defvar t-utils--xr-impl-result-active)
(defvar t-utils--xr-impl-result)

(defun  t-utils--use-xr-impl-result ()
  "Send result to `t-utils--xr-impl-result'?"
  (and (boundp 't-utils--xr-impl-result-active)
       t-utils--xr-impl-result-active))

(defun t-utils--xr-impl (commands)
  "Implementation for `t-utils-xr' that processes COMMANDS."
  (when (or (= (point) 1)
            (not (save-excursion (goto-char (1- (point))) (looking-at ")"))))
    (error "Expected point to be after a closing parenthesis, \")\""))

  (let* ((line-move-visual nil) ;; C-n, next-line: moves by true lines and not the width
         (buf-file (t-utils--get-buf-file))
         (start-line (line-number-at-pos))
         (xr-end-point (point))
         (xr-start-point
          (save-excursion
            (backward-list)
            (when (not (looking-at "(t-utils-xr"))
              (error "`backward-list from point, %d, didn't not jump to (t-utils-xr" xr-end-point))
            (point)))
         (xr-cmd (buffer-substring-no-properties xr-start-point xr-end-point))
         (result (format "\n* Executing commands from %s:%d:%d:\n\n  %s\n"
                         (file-name-nondirectory buf-file)
                         (line-number-at-pos xr-start-point)
                         (save-excursion (goto-char xr-start-point)
                                         (current-column))
                         xr-cmd))
         (cmd-num 0))

    ;; Enable "C-SPC" in `t-utils-xr' commands.  Under regular running, we are being invoked from
    ;; `t-utils-xr-test' and current buffer is a temporary buffer.  In batch mode,
    ;; `transient-mark-mode' is not active, thus activate it.
    (transient-mark-mode 1)

    (dolist (command commands)
      (setq cmd-num (1+ cmd-num))
      (let ((standard-output (generate-new-buffer " *temp t-utils-xr-capture*" t))
            (buf-major-mode major-mode))
        (unwind-protect
            (let* ((start-pt (point))
                   (start-pt-str (t-utils--get-point-for-display start-pt))
                   (start-contents (buffer-substring-no-properties (point-min) (point-max)))
                   (key-command (when (eq (type-of command) 'string)
                                  ;; Keybinding, e.g. (t-utils-xr "C-M-a")
                                  (let ((cmd (key-binding (kbd command))))
                                    (when (not cmd)
                                      (user-error "%s:%d: Command, %s, is not a known keybinding"
                                                  buf-file start-line command))
                                    cmd)))
                   print-code-active)
              (setq result (concat result "\n"
                                   (format "- Invoking      : %S%s\n"
                                           command (if key-command
                                                       (concat " = " (symbol-name key-command))
                                                     ""))
                                   (format "  Start point   : %4d\n" start-pt)))

              (if key-command
                  ;; a keybinding: (t-util-xr "C-M-a")
                  (call-interactively key-command)
                ;; a command: (t-utils-xr (beginning-of-defun))
                (setq print-code-active (string-match-p "\\`(t-utils-xr-print-code"
                                                        (format "%S" command)))
                (eval command))

              (let ((end-pt (point))
                    (end-contents (buffer-substring-no-properties (point-min) (point-max)))
                    (debug-msg (format "%d: %S, start point %s" cmd-num command start-pt-str)))

                ;; Record point movement by adding what happened to result
                (if (equal start-pt end-pt)
                    (setq result (concat result "  No point movement\n")
                          debug-msg (concat debug-msg ", no point movement"))
                  (let* ((current-line (buffer-substring-no-properties (line-beginning-position)
                                                                       (line-end-position)))
                         (position (format "%d:%d: " (line-number-at-pos) (current-column)))
                         (carrot (concat (make-string (+ (length position) (current-column)) ?\s)
                                         "^")))
                    (setq result (concat result (format "  Moved to point: %4d\n  : %s%s\n  : %s\n"
                                                        end-pt position current-line carrot))
                          debug-msg (concat debug-msg
                                            (format ", moved point to %s"
                                                    (t-utils--get-point-for-display (point)))))))

                ;; Grab standard-output from `prin1' or `print'
                (with-current-buffer standard-output
                  (let ((contents (string-trim (buffer-substring (point-min) (point-max)))))
                    (when (not (string= contents ""))
                      (setq contents (replace-regexp-in-string "\\`\"" "" contents))
                      (setq contents (replace-regexp-in-string "\"\\'" "" contents))
                      (setq result (concat result
                                           "  standard-output:\n"
                                           (if print-code-active
                                               (concat "  #+begin_src "
                                                       (replace-regexp-in-string
                                                        "-mode\\'" ""
                                                        (symbol-name buf-major-mode))
                                                       "\n")
                                             "  #+begin_example\n")
                                           contents
                                           (if (string-match-p "\n\\'" contents) "" "\n")
                                           (if print-code-active
                                               "  #+end_src\n"
                                             "  #+end_example\n"))))))

                ;; Record buffer modifications by adding what happened to result
                (if (equal start-contents end-contents)
                    (setq result (concat result "  No buffer modifications\n")
                          debug-msg (concat debug-msg ", no buffer modifications"))
                  (setq result (concat result
                                       "  Buffer modified:\n"
                                       "  #+begin_src diff\n"
                                       (t-utils--diff-strings start-contents end-contents)
                                       "  #+end_src diff\n")
                        debug-msg (concat debug-msg ", buffer modified")))

                (when (not (t-utils--use-xr-impl-result))
                  ;; Display debugging info for interactive evaluation of (t-utils-xr COMMANDS)
                  (read-string (concat debug-msg "\n" "Enter to continue:")))))
          ;; unwind-protect unwind forms
          (and (buffer-name standard-output)
               (kill-buffer standard-output)))))

    (if (t-utils--use-xr-impl-result)
        (progn
          (setq t-utils--xr-impl-result result)
          nil)
      result)))

(cl-defmacro t-utils-xr (&rest commands)
  "Execute and record results of each command in list of COMMANDS.
This returns a string recording point movement and buffer modification
differences for each command.  See `t-utils-test-xr' for details."
  (t-utils--xr-impl commands))

(defun t-utils--eval-sexp-print-advice (_old-function &rest _)
  "Advice around `elisp--eval-last-sexp-print-value' to ignore the print.")

(defun t-utils-test-xr (test-name lang-files)
  "Execute and record (t-utils-xr COMMANDS) from LANG-FILES list.
For each NAME.LANG in LANG-FILES, run each (t-utils-xr COMMANDS) and
compare results against NAME_expected.org.  TEST-NAME is used in
messages.

The commands that you can place within (t-utils-xr COMMANDS) are
 1. Lisp expressions.  For example,
      (t-utils-xr (beginning-of-defun))
 2. Keybindings.  For example,
      (t-utils-xr \"C-M-a\")
 3. `standard-output' is captured.
    * `t-utils-xr-print-code' can be use to print part of the buffer
      into a code block:
        (t-utils-xr .... (t-utils-xr-print-code start-point end-point))
    * (print OBJECT) or (print OBJECT) can also be used.  These
      write to `standard-output' and that is captured into an example
      block.  For example,
        (t-utils-xr (prin1 (a-buffer-query-function-special-to-your-mode)))

Multiple expressions or keybindings can be specified.

Consider ./test-defun-movement/my_test.c:

  1 | #include <stdlib.h>
  2 |
  3 | int fcn1(void) {
  4 |   // (t-utils-xr \"C-M-e\" \"C-M-e\")
  5 |   return 1;
  6 | }
  7 |
  8 | int main(void) {
  9 |   return fcn1();
  10|   // (t-utils-xr (beginning-of-defun) (beginning-of-defun))
  11| }

You can interactively evaluate each (t-utils-xr COMMANDS) by placing the
`point' on the closing parenthesis and typing \\[eval-last-sexp].  For
example, with the point after the closing parenthesis on line 4 and
running \\[eval-last-sexp], we'll see in the *Messages* buffer:

    * Executing commands from my_test.c:4:

      // (t-utils-xr \"C-M-e\" \"C-M-e\")

    - Invoking      : \"C-M-e\" = c-end-of-defun
      Start point   :   72
      Moved to point:   87
      : 7:0:
      :      ^
      No buffer modifications

    - Invoking      : \"C-M-e\" = c-end-of-defun
      Start point   :   87
      Moved to point:  158
      : 12:0:
      :       ^
      No buffer modifications

Running

  M-: (t-utils-test-xr \"test-defun-movement\"
                       \\='(\"test-defun-movement/my_test.c\"))

will run the two (t-utils-xr COMMANDS) statements from line 4 and 10 of
my_test.c.  The result is compared against
test-defun-movement/my_test_expected.org.  If my_test_expected.org does
not exist or result doesn't match the existing my_test_expected.org,
my_test_expected.org~ is generated and if it looks correct, you should
rename it to my_test_expected.org.

TODO should example test setup, see t-utils-test-font-lock."

  (let ((error-msgs '()))
    (dolist (lang-file lang-files)
      (with-temp-buffer

        (t-utils--insert-file-for-test lang-file)

        (let* ((start-time (current-time))
               (expected-file (replace-regexp-in-string "\\.[^.]+\\'" "_expected.org"
                                                        lang-file))
               (expected (when (file-exists-p expected-file)
                           (with-temp-buffer
                             (insert-file-contents-literally expected-file)
                             (buffer-string))))
               (got "#+startup: showall\n")
               (got-file (concat expected-file "~")))

          (message "START: %s %s" test-name lang-file)

          (condition-case err
              (while (re-search-forward "(t-utils-xr" nil t)
                (re-search-backward "(")
                (forward-list)
                (let* ((xr-end-point (point)))
                  (setq t-utils--xr-impl-result-active t)
                  (unwind-protect
                      (progn
                        ;; `eval-last-sexp' on (t-utils-xr COMMANDS) calls
                        ;; `elisp--eval-last-sexp-print-value' which will (prin1 value output) where
                        ;; output is t which means send to echo area.  We don't want to print value
                        ;; which is nil in our case, so we override
                        ;; elisp--eval-last-sexp-print-value locally during this eval.
                        (advice-add #'elisp--eval-last-sexp-print-value :override
                                    #'t-utils--eval-sexp-print-advice)

                        (eval-last-sexp nil)
                        (setq got (concat got t-utils--xr-impl-result)
                              t-utils--xr-impl-result-active nil
                              t-utils--xr-impl-result nil))
                    (progn
                      (setq t-utils--xr-impl-result-active nil
                            t-utils--xr-impl-result nil)
                      (advice-remove #'elisp--eval-last-sexp-print-value
                                     #'t-utils--eval-sexp-print-advice)))

                  ;; look for next (t-utils-xr COMMANDS)
                  (goto-char xr-end-point)))
            (error
             (error "Failure in %s at point %d: %s" lang-file (point) (error-message-string err))
             ))

          (kill-buffer)

          (when (string= got "")
            (error "No (t-utils-xr COMMANDS) found in %s" lang-file))

          (let ((error-msg (t-utils--baseline-check test-name start-time
                                                    lang-file got got-file expected expected-file)))
            (when error-msg
              (push error-msg error-msgs))))))

    ;; Validate t-utils-test-xr result
    (setq error-msgs (reverse error-msgs))
    (should (equal error-msgs '()))))

(defun t-utils--test-font-lock-checker (lang-file
                                        got got-file expected expected-file code-to-face)
  "Get error that includes the position of the first font face difference.
See `t-utils-test-font-lock' for
LANG-FILE GOT GOT-FILE EXPECTED EXPECTED-FILE CODE-TO-FACE."

  (when (and (not (string= got expected))
             (= (length got) (length expected)))
    (let* ((diff-idx (abs (compare-strings got nil nil
                                           expected nil nil)))
           (got-code (substring got (1- diff-idx) diff-idx))
           (got-face (cdr (assoc got-code code-to-face)))
           (expected-code (substring expected (1- diff-idx) diff-idx))
           (expected-face (cdr (assoc expected-code code-to-face))))
      (cons
       (list (format "Test: %s" lang-file)
             (format "Got: %s" got-file)
             (format "Expected: %s" expected-file)
             (format "Difference-at-point: %d, \
got code-to-face (\"%s\" . %S), expected code-to-face (\"%s\" . %S)"
                     diff-idx
                     got-code got-face
                     expected-code expected-face))
       nil ;; nil ==> do not do standard baseline comparison
       ))))

(defun t-utils-test-font-lock (test-name lang-files code-to-face &optional setup-callback)
  "Test font-lock using on each lang-file in LANG-FILES list.
Foreach file NAME.LANG in LANG-FILES compare the file against
NAME_expected.txt, where NAME the file name minus the lang-file
extension, EXT.  NAME_expected.txt is of same length as the file and has
a character for each face setup by font-lock.  CODE_TO_FACE is an alist
where each element is (CHAR . FACE).  TEST-NAME is used when displaying
messages.

If NAME_expected.txt does not exist or doesn't match the results we
got, a NAME_expected.txt~ will be generated.  After reviewing
NAME_expected.txt~, you should rename it to NAME_expected.txt or fix
your code and rerun the test.

If optional SETUP-CALLBACK is specified it is called after setting the
major mode on the temporary buffer for lang-file.

For example, suppose our LANG-FILE contains
    int foo(void) {
        return 1;
    }
our NAME_expected.txt will contain:
    kkk fffDkkkkD b
        kkkkkk nD
    D
where int and void are keywords, etc. and CODE-TO-FACE contains:
  \\='((\"b\" . font-lock-bracket-face)
    (\"d\" . default)
    (\"D\" . font-lock-delimiter-face)
    (\"f\" . font-lock-function-name-face)
    (\"k\" . font-lock-keyword-face)
    (\"n\" . font-lock-constant-face))

Example test setup:

  ./LANGUAGE-ts-mode.el
  ./tests/test-LANGUAGE-ts-mode-font-lock.el
  ./tests/test-LANGUAGE-ts-mode-font-lock-files/NAME1.LANG
  ./tests/test-LANGUAGE-ts-mode-font-lock-files/NAME1_expected.txt
  ./tests/test-LANGUAGE-ts-mode-font-lock-files/NAME2.LANG
  ./tests/test-LANGUAGE-ts-mode-font-lock-files/NAME2_expected.txt
  ....

Where ./tests/test-LANGUAGE-ts-mode-font-lock.el contains:

  (defvar test-LANGUAGE-ts-mode-font-lock--file nil)

  (defun test-LANGUAGE-ts-mode-font-lock--file (lang-file)
    \"Test font-lock on LANG-FILE.\"
    (let ((test-LANGUAGE-ts-mode-font-lock--file lang-file))
      (ert-run-tests-interactively \"test-LANGUAGE-ts-mode-font-lock\")))

  (ert-deftest test-LANGUAGE-ts-mode-font-lock ()
    (let* ((test-name \"test-LANGUAGE-ts-mode-font-lock\")
           (LANGUAGE-ts-mode-font-lock-level 4)
           (lang-files (t-utils-get-files
                     test-name
                     (rx \".lang\" eos)
                     nil
                     test-LANGUAGE-ts-mode-font-lock--file))
           (code-to-face \\='(
                           (\"b\" . font-lock-bracket-face)
                           (\"B\" . font-lock-builtin-face)
                           (\"c\" . font-lock-comment-face)
                           ;; <add-more-as-needed>
                           )))
      (t-utils-error-if-no-treesit-for \\='LANGUAGE test-name)
      (t-utils-test-font-lock test-name lang-files code-to-face)))

To loop over all NAME*.LANG font-lock test files, interactively

  \\[ert] RET test-LANGUAGE-ts-mode-font-lock RET

In the `ert' result buffer, you can type \"m\" at the point of the
test (where the color marker is) to see messages that were displayed by
your test.

To debug a specific font-lock test file

 M-: (test-LANGUAGE-ts-mode-font-lock--file
      \"test-LANGUAGE-ts-mode-font-lock-files/NAME.LANG\")"

  (let ((face-to-code (mapcar (lambda (pair)
                                (cons (cdr pair) (car pair)))
                              code-to-face))
        (error-msgs '()))
    (dolist (lang-file lang-files)
      (with-temp-buffer
        (t-utils--insert-file-for-test lang-file nil setup-callback)
        (let ((start-time (current-time)))
          (message "START: %s %s" test-name lang-file)

          ;; Force font lock to throw catchable errors.
          (font-lock-mode 1)
          (font-lock-flush (point-min) (point-max))
          (font-lock-ensure (point-min) (point-max))

          (goto-char (point-min))
          (let* ((expected-file (replace-regexp-in-string "\\.[^.]+\\'" "_expected.txt"
                                                          lang-file))
                 (expected (when (file-exists-p expected-file)
                             (with-temp-buffer
                               (insert-file-contents-literally expected-file)
                               (buffer-string))))
                 (got "")
                 (got-file (concat expected-file "~")))

            (while (not (eobp))
              (let* ((face (if (face-at-point) (face-at-point) 'default))
                     (code (if (looking-at "\\([ \t\n]\\)")
                               (match-string 1)
                             (cdr (assoc face face-to-code)))))
                (when (not code)
                  (error "Face, %S, is not in code-to-face alist" face))
                (setq got (concat got code))
                (forward-char)
                (when (looking-at "\n")
                  (setq got (concat got "\n"))
                  (forward-char))))

            (kill-buffer)

            (let ((error-msg
                   (t-utils--baseline-check
                    test-name start-time
                    lang-file got got-file expected expected-file
                    (lambda (lang-file got got-file expected expected-file)
                      (t-utils--test-font-lock-checker lang-file got got-file
                                                       expected expected-file
                                                       code-to-face)))))
              (when error-msg
                (push error-msg error-msgs)))))))

    ;; Validate t-utils-test-font-lock result
    (setq error-msgs (reverse error-msgs))
    (should (equal error-msgs '()))))

(defun t-utils--test-indent-unindented (lang-file lang-file-mode
                                                  expected expected-file
                                                  &optional line-manipulator)
  "Indent the unindented contents of LANG-FILE.
In a temporary buffer
  - Insert all non-empty non-blank lines unindented
  - TAB on each line
  - RET to add blank lines
Validate result matches EXPECTED from EXPECTED-FILE.

LANG-FILE-MODE is the mode to use for LANG-FILE.  See
See `t-utils-test-indent' for LINE-MANIPULATOR."

  (let* ((lang-file-contents (with-temp-buffer
                               (insert-file-contents-literally lang-file)
                               (buffer-substring (point-min) (point-max))))
         (lines (split-string (string-trim lang-file-contents) "\n")))
    (with-temp-buffer
      (erase-buffer)
      (set-buffer-file-coding-system 'utf-8-unix)
      (funcall lang-file-mode)

      ;; Insert the non-empty lines into typing-lang-file-name buffer
      (dolist (line lines)
        (setq line (string-trim line))
        (when (not (string= line ""))
          (insert line "\n")))

      (goto-char (point-min))

      (when line-manipulator
        (save-excursion
          (funcall line-manipulator)))

      ;; Now indent each line and insert the empty ("") lines into typing-lang-file-buffer
      ;; as we indent. This exercises the RET and TAB behaviors which cause different
      ;; tree-sitter nodes to be provided to the indent engine rules.
      (while (not (eobp))

        (call-interactively #'indent-for-tab-command) ;; TAB on code just added

        ;; While next line in our original contents is a newline insert "\n"
        (while (let ((next-line (nth (line-number-at-pos (point)) lines)))
                 (and next-line (string-match-p "^[ \t\r]*$" next-line)))
          (goto-char (line-end-position))
          ;; RET to add blank line
          (call-interactively #'newline)
          ;; TAB on the same blank line can result in different tree-sitter nodes than
          ;; the RET, so exercise that.
          (call-interactively #'indent-for-tab-command))
        (forward-line))

      (t-utils--trim)

      (let ((typing-got (buffer-substring (point-min) (point-max)))
            error-msg)
        (set-buffer-modified-p nil)
        (kill-buffer)
        (when (not (string= typing-got expected))
          (let ((coding-system-for-write 'raw-text-unix)
                (typing-got-file (replace-regexp-in-string "\\.\\([^.]+\\)\\'"
                                                           "_indent_unindented.\\1~"
                                                           lang-file)))
            (write-region typing-got nil typing-got-file)
            (setq error-msg
                  (list
                   (format "Indenting-unindented-contents-of: %s" lang-file)
                   (format "Got: %s" typing-got-file)
                   (format "Expected: %s" expected-file)))))
        ;; result is nil or an error message list of strings
        error-msg))))

(defun t-utils--test-indent-typing-line-by-line (lang-file lang-file-mode
                                                      expected expected-file)
  "Indent LANG-FILE by typing it line-by-line.
Validate result matches EXPECTED from EXPECTED-FILE.

LANG-FILE-MODE is the mode to use for LANG-FILE.  See
See `t-utils-test-indent' for LINE-MANIPULATOR."

  (let* ((contents (with-temp-buffer
                     (insert-file-contents-literally lang-file)
                     (buffer-substring (point-min) (point-max))))
         (lines (split-string (string-trim contents) "\n")))
    (with-temp-buffer
      (erase-buffer)
      (set-buffer-file-coding-system 'utf-8-unix)
      (funcall lang-file-mode)

      ;; Insert lines one a time and indent via newline (C-m)

      (dolist (line lines)
        (setq line (string-trim line))
        (when (not (string= line ""))
          (insert line))
        (call-interactively #'newline))

      (let ((typing-got (buffer-substring (point-min) (point-max)))
            error-msg)
        (set-buffer-modified-p nil)
        (kill-buffer)
        (when (not (string= typing-got expected))
          (let ((coding-system-for-write 'raw-text-unix)
                (typing-got-file (replace-regexp-in-string "\\.\\([^.]+\\)\\'"
                                                           "_typing_line_by_line.\\1~"
                                                           lang-file)))
            (write-region typing-got nil typing-got-file)
            (setq error-msg
                  (list
                   (format "Typing-line-by-line: %s" lang-file)
                   (format "Got: %s" typing-got-file)
                   (format "Expected: %s" expected-file)))))
        ;; result is nil or an error message list of strings
        error-msg))))

(defun t-utils-test-indent (test-name lang-files
                                      &optional indent-checker line-manipulator error-nodes-regexp)
  "Test indent on each file in LANG-FILES list.
Compare indent of each NAME.LANG in LANG-FILES against NAME_expected.LANG.
TEST-NAME is used in messages.

If NAME_expected.LANG does not exist or the indent of NAME.LANG doesn't
match NAME_expected.txt, NAME_expected.LANG~ will be created.  You are
then instructed to validate the indent and rename NAME_expected.LANG~
to NAME_expected.LANG.

To add a test for TEST-NAME.el which calls this function, in the
corresponding TEST-NAME-files/ directory, create
TEST-NAME-files/NAME.LANG, then run the test.  Follow the messages to
accept the generated baseline after validating it.

Two methods are used to indent each file in LANG-FILES,

 1. (indent-region (point-min) (point-man))

    If optional INDENT-CHECKER function is provided, that is called with
    the temporary buffer in context after the `indent-region'.

 2. Indent the unindented contents of lang-file when there are no
    error nodes in lang-file.  In a temporary buffer
      - Insert all non-empty non-blank lines unindented
      - TAB on each line
      - RET to add blank lines
    In tree-sitter modes, TAB and RET need to be handled and this
    verifies they are handled.  Error nodes are identified by using
    ERROR-NODES-REGEXP which defaults to (rx bos \"ERROR\" eos).

    If the test fails, a file named NAME_indent_unindented.LANG~ is
    created.

    The typing buffer is initialized with the string-trim'd version of
    the non-empty lines of LANG-FILE.  If optional LINE-MANIPULATOR
    function is specified, it is called with the typing buffer as the
    current buffer.  LINE-MANIPULATOR should only adjust whitespace in
    the lines.  It should not add newlines to the buffer.
    LINE-MANIPULATOR is called from within a `save-excursion', so your
    function doesn't need to do that.

    In the typing buffer, each line is indented via
    `indent-for-tab-command' and blank lines are inserted by calling
    `newline'.`

 3. Indent the contents of lang-file line-by-line when there are no
    error nodes in lang-file.  In a temporary buffer
      - Insert the string-trim'd first line of lang-file
      - RET to indent via `newline'
      - Repeat for each remaining line of lang-file.
    Validate result matches the EXPECTED.

    If the test fails, a file named NAME_typing_line_by_line.LANG~ is
    created.

    It can be difficult to make the typing part of the test for
    lang-file pass due to ERROR nodes in the tree-sitter parse tree.
    Therefore, if you want to disable this part of the test for lang-file
    with basename NAME.LANG create basename NAME.typing.skip.txt.
    For example, if lang-file has name
      .../tests/test-LANGUAGE-ts-mode-indent-files/indent_fcn.lang
    create the following to skip it and put a comment in the .typing.skip.txt
    file as to why it's skipped
      .../tests/test-LANGUAGE-ts-mode-indent-files/indent_fcn.skip.typing.txt

    If it is not possible to indent the file line-by-line, you can disable
    the line-by-line indent without generating a warning by adding in a
    comment to lang-file:
      t-utils-test-indent: no-line-by-line-indent - <REASON>

Example test setup:

  ./LANGUAGE-ts-mode.el
  ./tests/test-LANGUAGE-ts-mode-indent.el
  ./tests/test-LANGUAGE-ts-mode-indent-files/NAME1.LANG
  ./tests/test-LANGUAGE-ts-mode-indent-files/NAME1_expected.LANG
  ./tests/test-LANGUAGE-ts-mode-indent-files/NAME2.LANG
  ./tests/test-LANGUAGE-ts-mode-indent-files/NAME2_expected.LANG

Where ./tests/test-LANGUAGE-ts-mode-indent.el contains:

  (defvar test-LANGUAGE-ts-mode-indent--file nil)

  (defun test-LANGUAGE-ts-mode-indent--file (lang-file)
    \"Test indent on LANG-FILE.\"
    (let ((test-LANGUAGE-ts-mode-indent--file lang-file))
      (ert-run-tests-interactively \"test-LANGUAGE-ts-mode-indent\")))

  (ert-deftest test-LANGUAGE-ts-mode-indent ()
    (let* ((test-name \"test-LANGUAGE-ts-mode-indent\")
           (lang-files (t-utils-get-files
                        test-name
                        (rx \".lang\" eos)
                        (rx \"_expected.lang\" eos) ;; skip our *_expected.lang baselines
                        test-LANGUAGE-ts-mode-indent--file)))
      (t-utils-error-if-no-treesit-for \\='LANGUAGE test-name)
      (t-utils-test-indent test-name lang-files)))

To loop over all NAME*.LANG indent test files, interactively

  \\[ert] RET test-LANGUAGE-ts-mode-indent RET

In the `ert' result buffer, you can type \"m\" at the point of the
test (where the color marker is) to see messages that were displayed by
your test.

To debug a specific indent test file

 M-: (test-LANGUAGE-ts-mode-indent--file
      \"test-LANGUAGE-ts-mode-indent-files/NAME.LANG\")"


  (when (not error-nodes-regexp)
    (setq error-nodes-regexp (rx bos "ERROR" eos)))

  (let ((error-msgs '()))
    (dolist (lang-file lang-files)
      (let* ((expected-file (replace-regexp-in-string "\\.\\([^.]+\\)\\'" "_expected.\\1"
                                                      lang-file))
             (expected (when (file-exists-p expected-file)
                         (with-temp-buffer
                           (insert-file-contents-literally expected-file)
                           (buffer-string))))
             do-line-by-line-indent
             lang-file-major-mode
             error-node)

        ;; Indent lang-file
        (with-temp-buffer
          (let ((start-time (current-time)))
            (t-utils--insert-file-for-test lang-file)
            (setq error-node (treesit-search-subtree
                              (treesit-buffer-root-node) error-nodes-regexp nil t))

            (message "START: %s <indent-region> %s" test-name lang-file)
            (setq lang-file-major-mode major-mode)

            (indent-region (point-min) (point-max))

            (when indent-checker
              (funcall indent-checker))

            (t-utils--trim)
            (let ((got (buffer-substring (point-min) (point-max)))
                  (got-file (concat expected-file "~")))

              (setq do-line-by-line-indent
                    (not (string-match-p "t-utils-test-indent:[ \t]*no-line-by-line-indent" got)))

              (set-buffer-modified-p nil)
              (kill-buffer)

              (let ((indent-error-msg (t-utils--baseline-check
                                       (concat test-name " <indent-region>") start-time
                                       lang-file got got-file expected expected-file)))
                (when indent-error-msg
                  (push indent-error-msg error-msgs))))))

        ;; Now, simulate typing lang-file and indent it (exercise TAB and RET)
        (when (not error-node)
          (message "START: %s <indent-using-unindented-contents> %s" test-name lang-file)
          (let ((start-time (current-time))
                (unindented-error-msg (t-utils--test-indent-unindented
                                   lang-file lang-file-major-mode
                                   expected expected-file
                                   line-manipulator)))
            (message "%s: %s <indent-using-unindented-contents> %s %s" test-name lang-file
                     (if unindented-error-msg "FAIL" "PASS")
                     (t-utils--took start-time))
            (when unindented-error-msg
              (push unindented-error-msg error-msgs)))

          ;; Indent line-by-line if
          ;;  1)  lang-file does not contain:
          ;;          t-utils-test-indent: no-line-by-line-indent - <reason>
          ;;      No message produced about skipping it.
          ;; and
          ;;  2) LANG-FILE-NO-EXT.skip.typing.txt does not exist
          ;;     where LANG-FILE-NO-EXT is the lang-file without the extension.
          (when do-line-by-line-indent
            (let ((skip-typing-file (replace-regexp-in-string "\\.[^.]\\'" ".skip.typing.txt"
                                                              lang-file)))
              (if (file-exists-p skip-typing-file)
                  (t-utils--skip-message lang-file skip-typing-file
                                         "typing line-by-line this test input")
                (message "START: %s <indent-via-typing-line-by-line> %s" test-name lang-file)
                (let ((start-time (current-time))
                      (typing-error-msg (t-utils--test-indent-typing-line-by-line
                                         lang-file lang-file-major-mode
                                         expected expected-file)))
                  (message "%s: %s <indent-via-typing-line-by-line> %s %s" test-name lang-file
                           (if typing-error-msg "FAIL" "PASS")
                           (t-utils--took start-time))
                  (when typing-error-msg
                    (push typing-error-msg error-msgs))))))
          )
        ))
    ;; Validate t-utils-test-indent result
    (setq error-msgs (reverse error-msgs))
    (should (equal error-msgs '()))))

(defun t-utils--check-parse (lang-file error-nodes-regexp syntax-checker-fun check-valid-parse)
  "Return (parse-error . invalid-successful-parse) pair of strings.
This looks for ERROR-NODES-REGEXP in the tree-sitter parse tree.
See `t-utils-sweep-test-indent' for a description of
LANG-FILE ERROR-NODES-REGEXP SYNTAX-CHECKER-FUN CHECK-VALID-PARSE."

  (when (and check-valid-parse
             (not syntax-checker-fun))
    (error "SYNTAX-CHECKER-FUN must be provided when CHECK-VALID-PARSE is t"))

  (let* ((root (treesit-buffer-root-node))
         (error-node (treesit-search-subtree root error-nodes-regexp nil t))
         (check-pair (when (and syntax-checker-fun
                                (or error-node
                                    check-valid-parse))
                       (funcall syntax-checker-fun lang-file)))
         (check-valid (when check-pair (car check-pair)))
         (check-result (if check-pair
                           (concat
                            (string-trim (replace-regexp-in-string "^" "    " (cdr check-pair)))
                            "\n")
                         ""))
         (parse-error "")
         (invalid-successful-parse ""))

    (if error-node
        (when (or (eq check-pair nil) ;; no syntax checker
                  check-valid)        ;; or we passed syntax checker
          (setq parse-error
                (format "%s:%d:%d: error: parse tree contains error node %S\n%s"
                        lang-file
                        (line-number-at-pos (treesit-node-start error-node))
                        (save-excursion (goto-char (treesit-node-start error-node))
                                        (current-column))
                        error-node
                        check-result)))
      ;; else if we have a valid parse, very syntax-checker-fun also says its valid
      (when (and check-valid-parse (not check-valid)) ;; syntax checker reports an error
        (setq invalid-successful-parse
              (format "%s:1: error: tree-sitter parsed without error, yet file has \
errors according to the syntax-checker-fun\n%s" lang-file check-result))))

    (cons parse-error invalid-successful-parse)))

(defun t-utils-sweep-test-indent (test-name directory lang-file-regexp major-mode-fun
                                            &optional syntax-checker-fun check-valid-parse
                                            error-nodes-regexp
                                            log-file
                                            result-file)
  "Sweep test indent on files under DIRECTORY recursively.
File base names matching LANG-FILE-REGEXP are tested.
TEST-NAME is used in messages.

Each matching file is read into a temporary buffer and then
MAJOR-MODE-FUN is called.

ERROR-NODES-REGEXP, defaulting to (rx box \"ERROR\" eos), is provided to
`treesit-search-subtree' to look for syntax errors in the parse tree.
SYNTAX-CHECKER-FUN is a function that should take one argument, the
current file being sweep tested, and should return cons pair
  (VALID . CHECK-RESULT)
VALID is t there are no syntax errors, otherwise nil.  String
CHECK-RESULT is the what the checker produced.  Optional
CHECK-VALID-PARSE if t, will call SYNTAX-CHECKER-FUN on all files being
processed to verify that the a successful tree-sitter parse also has no
errors according to SYNTAX-CHECKER-FUN.

Progress messages are logged to LOG-FILE which defaults to
TEST_NAME.log.  Result is written to RESULT-FILE which defaults
to TEST_NAME.result.txt.

If the tree-sitter parse tree contains a node matching ERROR-NODES-REGEXP,
SYNTAX-CHECKER-FUN is called and if the file does not have syntax error,
it is reported because the tree-sitter parser says it has errors and
the SYNTAX-CHECKER-FUN says it does not.

Next, the buffer is indented using `indent-region' and if this fails it
is reported.  In addition, the slowest indents are reported.

Callers of this function should activate any assertions prior to calling
this function.  For example, the last rule of the tree-sitter mode may
be an assert rule and this should be activated:

  (defvar LANGUAGE-ts-mode--indent-assert nil
    \"Tests set this to t to identify when indent rules are missing.\")

  (defvar LANGUAGE-ts-mode--indent-assert-rule
    \\='((lambda (node parent bol)
        (when LANGUAGE-ts-mode--indent-assert
          (error \"Assert no rule for: N:%S P:%S BOL:%S GP:%S NPS:%S BUF:%S\"
                 node parent bol
                 (treesit-node-parent parent)
                 (treesit-node-prev-sibling node)
                 (buffer-name))))
      nil
      0))

  (defvar LANGUAGE-ts-mode--indent-rules
    `((LANGUAGE
       ;; <snip>
       ,LANGUAGE-ts-mode--indent-assert-rule
      )))

  (define-derived-mode LANGUAGE-ts-mode prog-mode \"LANGUAGE:ts\"
     ;; <snip>
     (setq-local treesit-simple-indent-rules LANGUAGE-ts-mode--indent-rules)
     ;; <snip>
     )

Example usage:

  (defun sweep-LANGUAGE-ts-mode-indent (&optional directory)
    (let ((LANGUAGE-ts-mode--indent-assert t))
      (t-utils-sweep-test-indent \"sweep-LANGUAGE-ts-mode-indent\"
                                 (or directory default-directory)
                                 (rx \".lang\" eos)
                                 #\\='LANGUAGE-ts-mode)))

The result is:

    Files-with-parse-error-nodes[-but-pass-syntax-checker-fun]:
      <files with error nodes>
    Files-that-parsed-successfully-but-failed-syntax-checker-fun:
      <files that tree-sitter parsed successfully but fail syntax-checker-fun>
    Indent-errors:
      <files that generated an indent error>
    Slowest-indents:
      <files where indent was slowest>

When run in an interactive Emacs session, e.g.
   M-: (sweep-LANGUAGE-ts-mode-indent)
the result is shown in \"*TEST-NAME*\" buffer, otherwise it
is displayed on stdout.

After running this, you examine the results to see if there are issues.
For example, the files with parse error nodes may be identifying issues
with your LANGUAGE tree-sitter where it is failing to parse or the files
may have syntax errors in them and the tree-sitter parse tree with error
nodes is correct.  Any files that generated errors during
`indent-region' are likely bugs that should be addressed because
this will only call `indent-region' on files when the tree-sitter parse
tree has no error nodes.  You should also look at the files where
`indent-region' was slow.  Very slow indents could be bugs in the
LANGUAGE tree-sitter that need addressing or some other issue."

  (when (not error-nodes-regexp)
    (setq error-nodes-regexp (rx bos "ERROR" eos)))

  (let ((all-lang-files (directory-files-recursively directory lang-file-regexp))
        (start-time (current-time))
        (parse-errors "")
        (invalid-successful-parse "")
        (indent-errors "")
        (took-ht (make-hash-table :test 'equal)))

    (setq log-file (t-utils--log-create test-name log-file))

    (t-utils--log log-file (format "Found %d files to indent %s\n"
                                   (length all-lang-files) (t-utils--took start-time)))

    (dolist (lang-file all-lang-files)
      (with-temp-buffer

        (let (ok)
          (t-utils--log log-file (format "Reading: %s\n" lang-file))

          (condition-case err
              (progn
                (t-utils--insert-file-for-test lang-file major-mode-fun)
                (setq ok t))
            (error
             (t-utils--log log-file (format "Skipping %s, %s\n"
                                            lang-file (error-message-string err)))))
          (when ok
            ;; Check for bad tree-sitter parse
            (let ((pair (t-utils--check-parse lang-file error-nodes-regexp
                                              syntax-checker-fun check-valid-parse)))
              (setq parse-errors (concat parse-errors (car pair))
                    invalid-successful-parse (concat invalid-successful-parse (car pair))))

            ;; Check indent
            (condition-case err
                (let ((indent-start (current-time)))
                  (indent-region (point-min) (point-max))
                  (puthash lang-file (float-time (time-subtract (current-time) indent-start))
                           took-ht))
              (error (setq indent-errors
                           (concat indent-errors
                                   (format "%s:1: error: failed to indent, %s\n"
                                           lang-file (error-message-string err))))))))))

    (let* ((slow-files (let ((files '()))
                         (maphash (lambda (file _took-time)
                                    (push file files))
                                  took-ht)
                         (sort files
                               :lessp (lambda (f1 f2)
                                        (> (gethash f1 took-ht)
                                           (gethash f2 took-ht)))
                               :in-place t)
                         (let* ((max-to-show 100)
                                (n-entries-to-rm (- (length files) max-to-show)))
                           (when (> n-entries-to-rm 0)
                             (nbutlast files n-entries-to-rm)))
                         (mapconcat #'identity
                                    (mapcar (lambda (file)
                                              (format "%s:1: note: indent took %.3f seconds\n"
                                                      file (gethash file took-ht)))
                                            files))))
           (result (concat (format "Files-with-parse-error-nodes%s:\n"
                                   (if syntax-checker-fun
                                       "-but-pass-syntax-checker-fun"
                                     ""))
                           parse-errors
                           "\n"
                           (when check-valid-parse
                             (concat
                              "Files-that-parsed-successfully-but-failed-syntax-checker-fun:\n"
                              invalid-successful-parse
                              "\n"))
                           "Indent-errors:\n"
                           indent-errors
                           "\n"
                           "Slowest-indents:\n"
                           slow-files)))

      (t-utils--display-result test-name directory result result-file))

    (message "FINISHED: %s %s" test-name (t-utils--took start-time))))

(defun t-utils-test-syntax-table (test-name lang-files)
  "Test syntax-table on each file in LANG-FILES list.
Compare syntax-table of each NAME.LANG in LANG-FILES against NAME_expected.txt.
TEST-NAME is used in messages.

If NAME_expected.txt does not exist or the result of NAME.LANG doesn't
match NAME_expected.txt, NAME_expected.txt~ will be created.  You are
then instructed to validate the result and rename NAME_expected.txt~
to NAME_expected.txt.

To add a test for TEST-NAME.el which calls this function, in the
corresponding TEST-NAME-files/ directory, create
TEST-NAME-files/NAME.LANG, then run the test.  Follow the messages to
accept the generated baseline after validating it.

TODO should example test setup, see t-utils-test-font-lock."

  (let ((error-msgs '()))
    (dolist (lang-file lang-files)
      (with-temp-buffer

        (let ((start-time (current-time)))
          (message "START: %s %s" test-name lang-file)

          (t-utils--insert-file-for-test lang-file)

          (let* ((got "")
                 (expected-file (replace-regexp-in-string "\\.[^.]+\\'" "_expected.txt" lang-file))
                 (got-file (concat expected-file "~"))
                 (expected (when (file-exists-p expected-file)
                             (with-temp-buffer
                               (insert-file-contents-literally expected-file)
                               (buffer-string)))))
            (forward-line) ;; skip the mode line specification
            (while (not (eobp))
              (when (looking-at "^")
                (setq got (concat got (format
                                       "Line:%d: %s\n"
                                       (line-number-at-pos)
                                       (buffer-substring-no-properties (point)
                                                                       (line-end-position))))))

              (let ((char (buffer-substring-no-properties (point) (1+ (point)))))
                (when (string= char "\n")
                  (setq char "\\n"))
                (setq got (concat got (format "  %2s: %S\n" char (syntax-ppss (point))))))

              (forward-char))

            (kill-buffer)

            (let ((error-msg (t-utils--baseline-check
                              test-name start-time
                              lang-file got got-file expected expected-file)))
              (when error-msg
                (push error-msg error-msgs)))))))
    ;; Validate t-utils-test-syntax-table result
    (setq error-msgs (reverse error-msgs))
    (should (equal error-msgs '()))))

(defun t-utils-test-treesit-defun-name (test-name lang-files)
  "Test `treesit-defun-name-function' setup.
Compare the result of `treesit-defun-name-function' against each
tree-sitter node in each NAME.LANG of LANG-FILES against
NAME_expected.txt.  TEST-NAME is used in messages.

If NAME_expected.txt does not exist or the result doesn't match
NAME_expected.txt, NAME_expected.txt~ will be created.  You are then
instructed to validate the result and rename NAME_expected.txt~
to NAME_expected.txt.

To add a test for TEST-NAME.el which calls this function, in the
corresponding TEST-NAME-files/ directory, create
TEST-NAME-files/NAME.LANG, then run the test.  Follow the messages to
accept the generated baseline after validating it.

TODO should example test setup, see t-utils-test-font-lock."

  (let ((error-msgs '()))
    (dolist (lang-file lang-files)
      (with-temp-buffer

        (let ((start-time (current-time)))
          (message "START: %s %s" test-name lang-file)

          (t-utils--insert-file-for-test lang-file)

          (let* ((root (treesit-buffer-root-node))
                 (expected-file (replace-regexp-in-string "\\.[^.]+\\'" "_expected.txt" lang-file))
                 (expected (when (file-exists-p expected-file)
                             (with-temp-buffer
                               (insert-file-contents-literally expected-file)
                               (buffer-string))))
                 (got "")
                 (got-file (concat expected-file "~")))

            (treesit-search-subtree
             root
             (lambda (node)
               (let ((defun-name (funcall treesit-defun-name-function node))
                     (node-type (replace-regexp-in-string "\n" "\\n" (treesit-node-type node)))
                     (node-start (treesit-node-start node))
                     (node-end (treesit-node-end node)))
                 (setq got (concat
                            got
                            (format "Node %25s at %4d to %4d: defun-name = %s\n"
                                    node-type node-start node-end
                                    (if defun-name defun-name "nil")))))
               nil))

            (kill-buffer)

            (let ((error-msg (t-utils--baseline-check
                              test-name start-time
                              lang-file got got-file expected expected-file)))
              (when error-msg
                (push error-msg error-msgs)))))))

    ;; Validate t-utils-test-treesit-defun-name result
    (setq error-msgs (reverse error-msgs))
    (should (equal error-msgs '()))))

(defun t-utils-test-imenu (test-name lang-files)
  "Test imenu support.
Compare the result of `imenu-create-index-function' on each NAME.LANG in
LANG-FILES against NAME_expected.txt.  TEST-NAME is used in messages.

If NAME_expected.txt does not exist or the result doesn't match
NAME_expected.txt, NAME_expected.txt~ will be created.  You are then
instructed to validate the result and rename NAME_expected.txt~
to NAME_expected.txt.

To add a test for TEST-NAME.el which calls this function, in the
corresponding TEST-NAME-files/ directory, create
TEST-NAME-files/NAME.LANG, then run the test.  Follow the messages to
accept the generated baseline after validating it.

TODO should example test setup, see t-utils-test-font-lock."

  (let ((error-msgs '()))
    (dolist (lang-file lang-files)
      (with-temp-buffer

        (let ((start-time (current-time)))
          (message "START: %s %s" test-name lang-file)

          (t-utils--insert-file-for-test lang-file)

          (let* ((index (funcall imenu-create-index-function))
                 (expected-file (replace-regexp-in-string "\\.[^.]+\\'" "_expected.txt" lang-file))
                 (expected (when (file-exists-p expected-file)
                             (with-temp-buffer
                               (insert-file-contents-literally expected-file)
                               (buffer-string))))
                 (got (concat (string-join
                               (mapcar (lambda (el) (substring-no-properties (car el))) index)
                               "\n")
                              "\n"))
                 (got-file (concat expected-file "~")))

            (kill-buffer)
            (let ((error-msg (t-utils--baseline-check
                              test-name start-time
                              lang-file got got-file expected expected-file)))
              (when error-msg
                (push error-msg error-msgs)))))))
    ;; Validate t-utils-test-imenu result
    (setq error-msgs (reverse error-msgs))
    (should (equal error-msgs '()))))

(defun t-utils-test-outline-search-function (test-name lang-files)
  "Test setup for `outline-minor-mode'.
Compare the result of `outline-search-function' on each NAME.LANG in
LANG-FILES against NAME_expected.txt.  TEST-NAME is used in messages.

If NAME_expected.txt does not exist or the result doesn't match
NAME_expected.txt, NAME_expected.txt~ will be created.  You are then
instructed to validate the result and rename NAME_expected.txt~
to NAME_expected.txt.

To add a test for TEST-NAME.el which calls this function, in the
corresponding TEST-NAME-files/ directory, create
TEST-NAME-files/NAME.LANG, then run the test.  Follow the messages to
accept the generated baseline after validating it.

TODO should example test setup, see t-utils-test-font-lock."

  (let ((error-msgs '()))
    (dolist (lang-file lang-files)
      (with-temp-buffer

        (let ((start-time (current-time))
              (lang-file-base (file-name-nondirectory lang-file)))

          (message "START: %s %s" test-name lang-file)

          (t-utils--insert-file-for-test lang-file)

          (let* ((expected-file (replace-regexp-in-string "\\.[^.]+\\'" "_expected.txt" lang-file))
                 (expected (when (file-exists-p expected-file)
                             (with-temp-buffer
                               (insert-file-contents-literally expected-file)
                               (buffer-string))))
                 (got "Section heading lines\n\n")
                 (got-file (concat expected-file "~")))

            (while (not (eobp))
              (let ((next-heading (funcall outline-search-function)))
                (if next-heading
                    (let ((heading-info (format "%s:%d: %s\n"
                                                lang-file-base
                                                (line-number-at-pos)
                                                (buffer-substring-no-properties
                                                 (line-beginning-position) (line-end-position)))))
                      (setq got (concat got heading-info))
                      (forward-line))
                  (goto-char (point-max)))))

            (kill-buffer)
            (let ((error-msg (t-utils--baseline-check
                              test-name start-time
                              lang-file got got-file expected expected-file)))
              (when error-msg
                (push error-msg error-msgs)))))))
    ;; Validate t-utils-test-outline-search-function result
    (setq error-msgs (reverse error-msgs))
    (should (equal error-msgs '()))))

(defun t-utils-test-file-encoding (test-name lang-files file-major-mode)
  "Test to check that the major-mode handles bad file encodings.
Corrupted content in NAME.LANG of LANG-FILES list can crash Emacs when a
tree-sitter language shared library runs on the corrupted content.  This
loads the file, activate the major-mode, FILE-MAJOR-MODE, function and
captures success if the file major-mode succeeds indicating file is not
corrupted or the error message.  This result is captured in
NAME_expected.txt.

TEST-NAME is used in messages.

If NAME_expected.txt does not exist or the result doesn't match
NAME_expected.txt, NAME_expected.txt~ will be created.  You are then
instructed to validate the result and rename NAME_expected.txt~ to
NAME_expected.txt.

To add a test for TEST-NAME.el which calls this function, in the
corresponding TEST-NAME-files/ directory, create
TEST-NAME-files/NAME.LANG with either corrupted or non-corrupted
content, then run the test.  Follow the messages to accept the generated
baseline after validating it.

Example test setup:

  ./LANGUAGE-ts-mode.el
  ./tests/test-LANGUAGE-ts-mode-file-encoding.el
  ./tests/test-LANGUAGE-ts-mode-file-encoding-files/NAME1.LANG
  ./tests/test-LANGUAGE-ts-mode-file-encoding-files/NAME1_expected.txt
  ./tests/test-LANGUAGE-ts-mode-file-encoding-files/NAME2.LANG
  ./tests/test-LANGUAGE-ts-mode-file-encoding-files/NAME2_expected.txt

Where ./tests/test-LANGUAGE-ts-mode-file-encoding.el contains:

  (defvar test-LANGUAGE-ts-mode-file-encoding--file nil)

  (defun test-LANGUAGE-ts-mode-file-encoding--file (lang-file)
    \"Test file-encoding on LANG-FILE.\"
    (let ((test-LANGUAGE-ts-mode-file-encoding--file lang-file))
      (ert-run-tests-interactively \"test-LANGUAGE-ts-mode-file-encoding\")))

  (ert-deftest test-LANGUAGE-ts-mode-file-encoding ()
    (let* ((test-name \"test-LANGUAGE-ts-mode-file-encoding\")
           (lang-files (t-utils-get-files
                     test-name
                     (rx \".lang\" eos)
                     nil
                     test-LANGUAGE-ts-mode-file-encoding--file)))
      (t-utils-error-if-no-treesit-for \\='LANGUAGE test-name)
      (t-utils-test-file-encoding test-name lang-files \\='#LANGUAGE-ts-mode)))

To loop over all NAME*.LANG file-encoding test files, interactively

  \\[ert] RET test-LANGUAGE-ts-mode-file-encoding RET

In the `ert' result buffer, you can type \"m\" at the point of the
test (where the color marker is) to see messages that were displayed by
your test.

To debug a specific file-encoding test file

 M-: (test-LANGUAGE-ts-mode-file-encoding--file
      \"test-LANGUAGE-ts-mode-file-encoding-files/NAME.LANG\")"

  (let ((error-msgs '()))
    (dolist (lang-file lang-files)
      (with-temp-buffer

        (let ((start-time (current-time)))

          (message "START: %s %s" test-name lang-file)

          (let* ((expected-file (replace-regexp-in-string "\\.[^.]+\\'" "_expected.txt" lang-file))
                 (expected (when (file-exists-p expected-file)
                             (with-temp-buffer
                               (insert-file-contents-literally expected-file)
                               (buffer-string))))
                 (got "Major mode activated successfully.")
                 (got-file (concat expected-file "~")))

            ;; Load lang-file in temp buffer and activate file-major-mode
            (condition-case err
                (t-utils--insert-file-for-test lang-file file-major-mode nil 'skip-corrupt-check)
              (error
               (setq got (concat "Major mode errored with message\n" (error-message-string err)))))

            (setq got (concat got "\n\n" "Entered major-mode: " (symbol-name major-mode) "\n"))

            (kill-buffer)

            (let ((error-msg (t-utils--baseline-check
                              test-name start-time
                              lang-file got got-file expected expected-file)))
              (when error-msg
                (push error-msg error-msgs)))))))

    ;; Validate t-utils-test-file-encoding result
    (setq error-msgs (reverse error-msgs))
    (should (equal error-msgs '()))))

(defun t-utils--bad-parse-msg (lang-file parse-issue error-info)
  "Return an bad parse error message for LANG-FILE containing ERROR-INFO.
PARSE-ISSUE is a string for the message.
ERROR-INFO is \"at line NUM:COL<optional-text\""

  (cond
   ((string-match "at line \\([0-9]+\\):\\([0-9]+\\)" error-info)
    (format  "%s:%s:%s: error: %s %s\n"
             lang-file (match-string 1 error-info) (match-string 2 error-info)
             parse-issue error-info))
   (t
    (error "%s bad error-info, %s" lang-file error-info))))

(defun t-utils--err-loc (error-node)
  "Get \"type at line N1:C1 to N2:C2\" string for ERROR-NODE."

  (let* ((start-point (treesit-node-start error-node))
         (start-line (line-number-at-pos start-point))
         (start-col (save-excursion ;; error messages are one based columns
                      (goto-char start-point)
                      (1+ (current-column))))
         (end-point (treesit-node-end error-node))
         (end-line (line-number-at-pos end-point))
         (end-col (save-excursion
                    (goto-char end-point)
                    (1+ (current-column)))))
    (format "%s node at line %d:%d to %d:%d (point %d to %d)"
            (treesit-node-type error-node)
            start-line start-col
            end-line end-col
            start-point
            end-point)))

(defun t-utils-sweep-test-ts-grammar (test-name
                                      directory
                                      lang-file-regexp
                                      major-mode-fun
                                      syntax-checker-fun
                                      &optional error-nodes-regexp
                                      log-file
                                      result-file)
  "Sweep test a tree-sitter grammar shared library looking for parse issues.

File base names matching LANG-FILE-REGEXP under DIRECTORY
recursively are examined.  TEST-NAME is used in messages.

Each matching file is read into a temporary buffer and then
MAJOR-MODE-FUN is called.  This should be a mode that activates
a tree-sitter grammar, i.e. calls (treesit-parser-create \\='LANGUAGE).

ERROR-NODES-REGEXP, defaulting to (rx bol \"ERROR\" eos), is provided to
`treesit-search-subtree' to look for syntax errors in the parse tree.

SYNTAX-CHECKER-FUN is a function that takes a list of files and should
return a hash table with files as the keys and the value of each key is
either
  (cons \"no-syntax-errors\" nil)
  (cons \"has-syntax-errors\" \"at line N1:COL1 to N2:COL2\")

Progress messages are logged to LOG-FILE which defaults to
TEST_NAME.log.  Result is written to RESULT-FILE which defaults
to TEST_NAME.result.txt.

The result is:

    Files-with-parse-error-nodes-but-pass-syntax-checker-fun:
      <files with tree-sitter error nodes>

    Files-that-parsed-successfully-but-failed-syntax-checker-fun:
      <files without tree-sitter error nodes>

    Total-consistently-parsed-files: M of N

When run in an interactive Emacs session, e.g.
    M-: (sweep-LANGUAGE-ts-mode-grammar)
the result is shown in \"*TEST-NAME*\" buffer,
otherwise the result is displayed on stdout."

  (when (not error-nodes-regexp)
    (setq error-nodes-regexp (rx bos "ERROR" eos)))

  (setq log-file (t-utils--log-create test-name log-file))

  (let ((start-time (current-time))
        (all-lang-files (sort (mapcar #'file-truename ;; Expand "~" for the syntax-checker-fun
                                      (directory-files-recursively directory lang-file-regexp))))
        (lang-files-to-check '())
        (ts-parse-result-ht (make-hash-table :test 'equal)))

    (when (= (length all-lang-files) 0)
      (user-error "No files found in directory %s recursively matching regexp \"%s\""
                  directory lang-file-regexp))
    (t-utils--log log-file (format "Found %d files to check %s\n"
                                   (length all-lang-files) (t-utils--took start-time)))

    (dolist (lang-file all-lang-files)
      (with-temp-buffer
        (let (ok)
          (t-utils--log log-file (format "Reading: %s\n" lang-file))
          (condition-case err
              (progn
                (t-utils--insert-file-for-test lang-file major-mode-fun)
                (setq ok t))
            (error
             (t-utils--log log-file (format "Skipping %s, %s\n"
                                            lang-file (error-message-string err)))))
          (when ok
            (push lang-file lang-files-to-check)
            (let* ((root (treesit-buffer-root-node))
                   (error-node (treesit-search-subtree root error-nodes-regexp nil t))
                   (syntax-status-pair (if error-node
                                           (cons "has-syntax-errors" (t-utils--err-loc error-node))
                                         (cons "no-syntax-errors" nil))))
              (puthash lang-file syntax-status-pair ts-parse-result-ht)
              (t-utils--log log-file (format "ts-parse: %s > %S\n"
                                             lang-file syntax-status-pair)))))))

    (when (= (length lang-files-to-check) 0)
      (user-error "No files to check (all skipped)\n"))
    (setq lang-files-to-check (sort lang-files-to-check))
    (t-utils--log log-file (format "Checking %d files\n" (length lang-files-to-check)))

    (t-utils--log log-file (format "Calling %S\n" syntax-checker-fun))
    (let ((syntax-check-result-ht (funcall syntax-checker-fun lang-files-to-check))
          (files-with-bad-ts-error-parse "")
          (files-with-bad-ts-success-parse "")
          (n-consistent-files 0))

      (t-utils--log log-file (format "Examining %S result\n" syntax-checker-fun))

      (dolist (lang-file lang-files-to-check)
        (let ((ts-parse-file-result-pair (gethash lang-file ts-parse-result-ht))
              (syntax-check-file-result-pair
               (let ((pair (gethash lang-file syntax-check-result-ht)))
                 (when (not (or (equal (car pair) "has-syntax-errors")
                                (equal (car pair) "no-syntax-errors")))
                   (user-error "Bad hash %S, %s -> %S" syntax-check-result-ht lang-file pair))
                 pair)))
          (if (string= (car ts-parse-file-result-pair) (car syntax-check-file-result-pair))
              (setq n-consistent-files (1+ n-consistent-files))
            (pcase (car ts-parse-file-result-pair)
              ("has-syntax-errors" ;; ts says syntax errors, syntax-check says no errors
               (setq files-with-bad-ts-error-parse
                     (concat files-with-bad-ts-error-parse
                             (t-utils--bad-parse-msg lang-file
                                                     "bad tree-sitter parse"
                                                     (cdr ts-parse-file-result-pair)))))
              ("no-syntax-errors";; ts says no syntax errors, syntax-check says have errors
               (setq files-with-bad-ts-success-parse
                     (concat files-with-bad-ts-success-parse
                             (t-utils--bad-parse-msg lang-file
                                                     "tree-sitter did not detect error"
                                                     (cdr syntax-check-file-result-pair)))))
              (_ (cl-assert nil))))))

      (let ((result
             (concat
              "Files-with-parse-error-nodes-but-pass-syntax-checker-fun:\n"
              files-with-bad-ts-error-parse
              "\n"
              "Files-that-parsed-successfully-but-failed-syntax-checker-fun:\n"
              files-with-bad-ts-success-parse
              "\n"
              "Total-consistently-parsed-files: " (format "%d of %d\n" n-consistent-files
                                                          (length lang-files-to-check)))))

        (t-utils--display-result test-name directory result result-file)))

    (t-utils--log log-file (format "FINISHED: %s %s\n" test-name (t-utils--took start-time)))))

(defun t-utils--syntax-tree-draw-node (node)
  "Draw the syntax tree of NODE in the current buffer.

When this function is called, point should be at the position where the
node should start.  When this function returns, it leaves point at the
end of the last line of NODE.

Similar `treesit--explorer-draw-node' but designed for test baselines."

  (let* ((type (treesit-node-type node))
         (field-name (treesit-node-field-name node))
         (children (treesit-node-children node))
         (named (treesit-node-check node 'named))
         ;; Column number of the start of the field-name, aka start of
         ;; the whole node.
         (before-field-column (current-column))
         ;; Column number after the field-name.
         after-field-column
         ;; Are all children suitable for inline?
         (all-children-inline
          (eq 0 (apply #'+ (mapcar #'treesit-node-child-count children))))
         ;; If the child is the first child, we can inline, if the
         ;; previous child is suitable for inline, this child can
         ;; inline, if the previous child is not suitable for inline,
         ;; this child cannot inline.
         (can-inline t))

    ;; Draw field name.  If all children are suitable for inline, we
    ;; draw everything in one line, other wise draw field name and the
    ;; rest of the node in two lines.
    (when field-name
      (insert (concat field-name ": "))
      (when (and children (not all-children-inline))
        (insert "\n")
        (indent-to-column (1+ before-field-column))))
    (setq after-field-column (current-column))

    ;; Draw the decorated type.
    (if named
        (progn
          (insert "(")
          (insert type)
          (when (not (treesit-node-child node 0)) ;; leaf node?
            (let ((node-text (substring-no-properties (treesit-node-text node))))
              (dolist (pair (list '("\t" . "\\\\t") '("\r" . "\\\\r") '("\n" . "\\\\n")))
                (setq node-text (replace-regexp-in-string (car pair) (cdr pair) node-text)))
              (when (> (length node-text) 50)
                (setq node-text (concat (substring node-text 0 50) "...")))
              (insert (format "[%d,%d]{%s}"
                              (treesit-node-start node)
                              (treesit-node-end node)
                              node-text)))))
      (pcase type
        ("\n" (insert "\\n"))
        ("\t" (insert "\\t"))
        (" "  (insert "SPC"))
        (_    (insert type))))

    ;; Draw children.
    (dolist (child children)
      ;; If a child doesn't have children, it is suitable for inline.
      (let ((draw-inline (eq 0 (treesit-node-child-count child)))
            (children-indent (1+ after-field-column)))
        (if (and draw-inline can-inline)
            ;; Draw children on the same line.
            (progn
              (insert " ")
              (t-utils--syntax-tree-draw-node child))
          ;; Draw children on the new line.
          (insert "\n")
          (indent-to-column children-indent)
          (t-utils--syntax-tree-draw-node child))
        (setq can-inline draw-inline)))

    ;; Done drawing children, draw the ending paren.
    (when named (insert ")"))))

(defun t-utils--get-syntax-tree ()
  "Return the syntax tree for the current buffer."
  (let ((root (or (treesit-buffer-root-node)
                  (error "No tree-sitter root node"))))
    (with-temp-buffer
      (insert "# tree-sitter parse tree annotated with [NODE-START,NODE-END]{NODE-TEXT}\n")
      (t-utils--syntax-tree-draw-node root)
      (buffer-string))))

(defun t-utils--test-parser-error-node-checker (lang-file _got _got-file _expected _expected-file)
  "Check ERROR node status for `t-utils-test-parser'.

If LANG-FILE contains \"error\" in it's name, then the parse tree must
have an ERROR node.  If LANG-FILE does not contain \"error\" in it's
name, then the parse tree must not have an ERROR node."
  (let* ((tree-error-node (treesit-search-subtree (treesit-buffer-root-node)
                                                  (rx (seq bos "ERROR" eos))))
         (tree-has-error (when tree-error-node t))
         (lang-file-name (file-name-nondirectory lang-file))
         (name-has-error (when (string-match-p (rx (seq bow "error" eow)) lang-file-name) t)))
    (cond
     ((equal tree-has-error (not name-has-error))
      (cons
       (list (concat "Parse tree for " lang-file
                     "contains an ERROR node and file name does not contain the word \"error\" "
                     "indicating that the parse tree should not have an ERROR node"))
       nil ;; nil ==> do not do standard baseline check
       ))

     ((equal (not tree-has-error) name-has-error)
      (cons
       (list (concat "Parse tree for " lang-file
                     "does not contain an ERROR node and file name contains word \"error\" "
                     "indicating the parse tree should have an ERROR node"))
       nil ;; nil ==> do not do standard baseline check
       )))))

(defun t-utils-test-parser (test-name lang-files)
  "Validate the tree-sitter parse tree against a baseline.
Each NAME.LANG of LANG-FILES list parse tree is captured and
an annotated version of it is compared against baseline, NAME_expected.txt.
TEST-NAME is used in messages.

If NAME.LANG contains the word \"error\" (error*.LANG,
_error.LANG or _error_), then the parse tree must contain an \"ERROR\"
node, otherwise it must not contain an \"ERROR\" node.

If NAME_expected.txt does not exist or the result doesn't match
NAME_expected.txt, NAME_expected.txt~ will be created.  You are then
instructed to validate the result and rename NAME_expected.txt~ to
NAME_expected.txt.

To add a test for TEST-NAME.el which calls this function, in the
corresponding TEST-NAME-files/ directory, create
TEST-NAME-files/NAME.LANG with either corrupted or non-corrupted
content, then run the test.  Follow the messages to accept the generated
baseline after validating it.

Example test setup:

  ./LANGUAGE-ts-mode.el
  ./tests/test-LANGUAGE-ts-mode-parser.el
  ./tests/test-LANGUAGE-ts-mode-parser-files/NAME1.LANG
  ./tests/test-LANGUAGE-ts-mode-parser-files/NAME1_expected.txt
  ./tests/test-LANGUAGE-ts-mode-parser-files/NAME2.LANG
  ./tests/test-LANGUAGE-ts-mode-parser-files/NAME2_expected.txt

Where ./tests/test-LANGUAGE-ts-mode-parser.el contains:

  (defvar test-LANGUAGE-ts-mode-parser--file nil)

  (defun test-LANGUAGE-ts-mode-parser--file (lang-file)
    (let ((test-LANGUAGE-ts-mode-parser--file lang-file))
      (ert-run-tests-interactively \"test-LANGUAGE-ts-mode-parser\")))

  (ert-deftest test-LANGUAGE-ts-mode-parser ()
    (let* ((test-name \"test-LANGUAGE-ts-mode-parser\")
           (lang-files (t-utils-get-files
                        test-name
                        (rx \".lang\" eos)
                        nil
                        test-LANGUAGE-ts-mode-parser--file)))
      (t-utils-error-if-no-treesit-for \\='LANGUAGE test-name)
      (t-utils-test-parser test-name lang-files \\='#LANGUAGE-ts-mode)))

To loop over all NAME*.LANG test files, interactively

  \\[ert] RET test-LANGUAGE-ts-mode-parser RET

In the `ert' result buffer, you can type \"m\" at the point of the
test (where the color marker is) to see messages that were displayed by
your test.

To debug a specific -parser test file

 M-: (test-LANGUAGE-ts-mode-parser--file
      \"test-LANGUAGE-ts-mode-parser-files/NAME.LANG\")"

  (let ((error-msgs '()))
    (dolist (lang-file lang-files)
      (with-temp-buffer

        (let ((start-time (current-time)))

          (message "START: %s %s" test-name lang-file)

          (let* ((expected-file (replace-regexp-in-string "\\.[^.]+\\'" "_expected.txt" lang-file))
                 (expected (when (file-exists-p expected-file)
                             (with-temp-buffer
                               (insert-file-contents-literally expected-file)
                               (buffer-string))))
                 (got-file (concat expected-file "~"))
                 got)

            (t-utils--insert-file-for-test lang-file)

            (setq got (t-utils--get-syntax-tree))

            (let ((error-msg (t-utils--baseline-check
                              test-name start-time
                              lang-file got got-file expected expected-file
                              #'t-utils--test-parser-error-node-checker)))

              (kill-buffer)

              (when error-msg
                (push error-msg error-msgs)))))))

    ;; Validate t-utils-test-file-encoding result
    (setq error-msgs (reverse error-msgs))
    (should (equal error-msgs '()))))

(provide 't-utils)
;;; t-utils.el ends here

;; LocalWords:  lang defun alist eos treesit lf setq truename dolist nondirectory bos buf funcall
;; LocalWords:  consp listp cdr CRLF impl tmp xr boundp SPC kbd prin progn defmacro sexp stdlib
;; LocalWords:  showall repeat:nil kkk fff Dkkkk kkkkkk mapcar eobp trim'd bol NPS prev puthash
;; LocalWords:  maphash lessp gethash nbutlast mapconcat ppss imenu pcase eow
