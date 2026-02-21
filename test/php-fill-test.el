;;; php-fill-test.el --- Tests for php-fill


;;; Code Preparation Before Tests:

(defvar php-fill-project-root
  (if (file-exists-p "erts/")
      (expand-file-name "../") ;; ERT's native cmds are being used
    default-directory) ;; eask is being used
  "Establishes what's the project's root dierecory.

It's later used as the base for the location of the ERTS files.
Depending how ERT is run, either by using ‘eask’ or its native
way (either the interactive or batch mode), `defult-directory' may show
the root directory or the same directory where the tests resign (which
in the context of this project) is the ‘test’ directory.  To find which
is the case, this code checks the existence of the ‘erts’ directory that
only exist on the ‘test’ directory.")

;; This is only needed by ERT's native commands but not by eask
(when (not (boundp 'php-fill))
  (add-to-list 'load-path php-fill-project-root))

;; The following code is only needed by ERT's native batch mode,
;; otherwise an evaluation has to be run first, but this code will avoid
;; needed to do so.  It's important to mention though, that
;; independently of whether the interactive or the batch mode is used,
;; the package ‘php-mode’ must have already been installed.  In the case
;; of eask, it's needed to be installed by using the 'eask install-deps
;; --dev' command first.  Though this function might help eask, it's not
;; the eask intended way.
(when (not (boundp 'php-mode))
  (let (php-mode-location)
    (when (setq php-mode-location
		(directory-files "~/.emacs.d/elpa/" t "php-mode-.*"))
      (add-to-list 'load-path (car php-mode-location)))))

(require 'php-fill)
(require 'php-mode)
(require 'ert-x)

(defvar php-fill-erts-dir (concat php-fill-project-root "test/erts/")
  "Path of the directory that holds the ERTS files.")

(defun php-fill-erts-path (erts-file)
  "Concatenates `php-fill-erts-dir' with ERTS-FILE."
  (concat php-fill-erts-dir erts-file))

;; Tests Start Here:

(ert-deftest php-fill-break-long-string-literl-apart ()
  (ert-test-erts-file (php-fill-erts-path
		       "break-long-string-literl-apart.erts")))

(ert-deftest php-fill-stitch-string-parts-together ()
  (ert-test-erts-file (php-fill-erts-path
		       "stitch-string-parts-together.erts")))

(ert-deftest php-fill-string-literal ()
  (ert-test-erts-file (php-fill-erts-path "fill-string-literal.erts")))

(ert-deftest php-fill-c-fill-paragraph-nosqueeze ()
  (ert-test-erts-file (php-fill-erts-path
		       "c-fill-paragraph-nosqueeze.erts")))

(ert-deftest php-fill-c-fill-paragraph ()
  (ert-test-erts-file (php-fill-erts-path "c-fill-paragraph.erts")))

(ert-deftest php-fill-paragraph ()
  (ert-test-erts-file (php-fill-erts-path "fill-paragraph.erts")))

(ert-deftest php-fill-newline ()
  (ert-test-erts-file (php-fill-erts-path "newline.erts")))

(ert-deftest php-fill-backward-delete ()
  (ert-test-erts-file (php-fill-erts-path "backward-delete.erts")))

(ert-deftest php-fill-delete-forward ()
  (ert-test-erts-file (php-fill-erts-path "delete-forward.erts")))

(ert-deftest php-fill-refill-mode ()
  (ert-test-erts-file (php-fill-erts-path "refill-mode.erts")))

(ert-deftest php-fill-set-local-variables ()
  (ert-test-erts-file (php-fill-erts-path "set-local-variables.erts")))

(ert-deftest php-fill-refill-mode-not-while-undo ()
  "Test that a refill is not performed after \\[undo].

This test could't be run on an erts file because they have their undo
ring is disabled."
  (ert-with-test-buffer ()
    (php-mode)
    (setq-local fill-column 38)
    (php-fill-refill-mode)
    (let ((content "$stringVar = 'Lorem ipsum dolor sit amet, \
consectetur adipiscing elit. Fusce vel luctus tellus, non varius ante.';"))
      (insert content)
      (undo-boundary)
      (goto-char 17)
      (ert-simulate-command '(self-insert-command 1 ?n))
      (undo-boundary)
      (ert-simulate-command '(undo))
      (should (equal (buffer-string) content)))))

;;; php-fill-test.el ends here
