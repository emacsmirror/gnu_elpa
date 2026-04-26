;;; guix.scm --- Build keymap-popup from the current working tree.
;;
;; Usage:
;;
;;   One-shot install into the user profile:
;;       guix package -f guix.scm
;;
;;   Development shell with all dependencies:
;;       guix shell -D -f guix.scm
;;
;;   Run tests inside Guix:
;;       guix shell -D -f guix.scm -- make test

(use-modules (gnu packages emacs)
             (guix build-system emacs)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix gexp)
             (ice-9 popen)
             (ice-9 rdelim))

(define %source-dir (dirname (current-filename)))

(define (git-output . args)
  "Run `git -C %source-dir ARGS...' and return trimmed stdout, or #f
if the command fails."
  (let* ((port (apply open-pipe* OPEN_READ "git" "-C" %source-dir args))
         (line (read-line port)))
    (close-pipe port)
    (if (eof-object? line) #f line)))

(define %version
  (or (git-output "describe" "--tags" "--always" "--dirty")
      (and=> (git-output "rev-parse" "--short" "HEAD")
             (lambda (hash) (string-append "0.1.0-" hash)))
      "0.1.0-git"))

(define (source-file? file stat)
  "Include everything except VCS metadata and build artifacts."
  (let ((name (basename file)))
    (not (or (string-prefix? "." name)
             (string-contains file "/refs/")
             (string-suffix? ".elc" file)
             (string-suffix? "~" file)))))

(define-public emacs-keymap-popup-git
  (package
    (name "emacs-keymap-popup-git")
    (version %version)
    (source (local-file %source-dir
                        "keymap-popup-checkout"
                        #:recursive? #t
                        #:select? source-file?))
    (build-system emacs-build-system)
    (home-page "https://codeberg.org/thanosapollo/emacs-forgejo")
    (synopsis "Described keymaps with popup help")
    (description
     "A single macro that produces both a real Emacs keymap (for direct
key dispatch) and stored descriptions (for a popup help window).
One definition, two uses.  No EIEIO, no OOP, no infix arguments.")
    (license license:gpl3+)))

emacs-keymap-popup-git
