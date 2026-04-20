;;; guix.scm --- Build emacs-gnosis from the current working tree.
;;
;; Usage:
;;
;;   One-shot install into the user profile:
;;       guix package -f guix.scm
;;
;;   Development shell with all dependencies:
;;       guix shell -D -f guix.scm
;;
;; This file mirrors the upstream Guix recipe for `emacs-gnosis' as
;; closely as possible so it doubles as a local test harness for the
;; recipe before it is merged.  The only intentional differences are:
;;
;;   - `source' is a `local-file' of the current checkout rather than
;;     a `git-fetch' of a pinned commit, so the build always reflects
;;     whatever is on disk.
;;   - `version' is derived from `git describe' at evaluation time.

(use-modules (gnu packages)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages texinfo)
             (guix build-system emacs)
             (guix gexp)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix utils)
             (ice-9 popen)
             (ice-9 rdelim))

(define %source-dir (dirname (current-filename)))

(define (git-output . args)
  "Run `git -C %source-dir ARGS...' and return its trimmed stdout, or
#f if the command fails or produces no output."
  (let* ((port (apply open-pipe* OPEN_READ "git" "-C" %source-dir args))
         (line (read-line port)))
    (close-pipe port)
    (if (eof-object? line) #f line)))

(define %version
  (or (git-output "describe" "--tags" "--always" "--dirty")
      (and=> (git-output "rev-parse" "--short" "HEAD")
             (lambda (hash) (string-append "0.10.3-" hash)))
      "0.10.3-git"))

(define (gnosis-file? file stat)
  "Include every file in the checkout except VCS metadata and build
artifacts."
  (let ((name (basename file)))
    (not (or (string-contains file "/.git/")
             (string=? name ".git")
             (string-suffix? ".elc" file)
             (string-suffix? "~" file)
             (string-suffix? ".tar" file)
             (string-suffix? ".tar.gz" file)
             (string-suffix? ".apkg" file)
             (string=? name "gnosis.db")
             (string=? name ":memory:")
             (string-contains file "/.worktrees/")
             (string-contains file "/org-fc/")
             (string-contains file "/org-roam/")
             (string-contains file "/temp/")))))

(define-public emacs-gnosis-git
  (package
    (name "emacs-gnosis-git")
    (version %version)
    (source (local-file %source-dir
                        "gnosis-checkout"
                        #:recursive? #t
                        #:select? gnosis-file?))
    (build-system emacs-build-system)
    (arguments
     (list
      #:test-command
      #~(list "make" "test" "GUIX_SHELL=")
      #:emacs emacs-no-x
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME"
                      (getenv "TMPDIR"))
              (mkdir-p (string-append
                        (getenv "HOME")
                        "/.emacs.d"))))
          (add-before 'install 'make-info
            (lambda _
              (invoke "make" "doc" "GUIX_SHELL="))))))
    (native-inputs (list texinfo))
    (propagated-inputs (list emacs-transient))
    (home-page "https://thanosapollo.org/projects/gnosis/")
    (synopsis "Personal knowledge system for GNU Emacs")
    (description
     "Gnosis is a personal knowledge system for GNU Emacs that
integrates note-taking with spaced repetition.  It combines
Zettelkasten-style linked notes with self-testing review,
all stored in a single SQLite database.  This package definition
builds straight from the current git checkout, so the installed
version always matches the working tree.")
    (license license:gpl3+)))

emacs-gnosis-git
