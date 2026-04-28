;;; guix.scm --- Build emacs-forgejo from the current working tree.
;;
;; Usage:
;;
;;   One-shot install into the user profile:
;;       guix package -f guix.scm
;;
;;   Development shell with all dependencies:
;;       guix shell -D -f guix.scm
;;
;; This file mirrors a future upstream Guix recipe for `emacs-forgejo'
;; as closely as possible so it doubles as a local test harness.  The
;; only intentional differences are:
;;
;;   - `source' is a `local-file' of the current checkout rather than
;;     a `git-fetch' of a pinned commit, so the build always reflects
;;     whatever is on disk.
;;   - `version' is derived from `git describe' at evaluation time.

(use-modules (gnu packages)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (guix build-system emacs)
             (guix download)
             (guix gexp)
             (guix git-download)
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
             (lambda (hash) (string-append "0.1.0-" hash)))
      "0.1.0-git"))

(define (forgejo-file? file stat)
  "Include every file in the checkout except VCS metadata and build
artifacts."
  (let ((name (basename file)))
    (not (or (string-prefix? "." name)
             (string-contains file "/refs/")
             (string-suffix? ".elc" file)
             (string-suffix? "~" file)
             (string-suffix? ".tar" file)
             (string-suffix? ".tar.gz" file)
             (string-suffix? ".db" file)))))

(define-public emacs-keymap-popup
  (let ((commit "fec80af2cdf9e3a25bb5033b32bf873584778f05")
        (revision "0"))
    (package
     (name "emacs-keymap-popup")
     (version (git-version "0.2.0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/thanosapollo/emacs-keymap-popup")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0x9vq4hnp7famcfv72qq6f28faang58gvr8ah223iqsvphrc5bz6"))))
     (build-system emacs-build-system)
     (arguments (list #:tests? #f))
     (home-page "https://codeberg.org/thanosapollo/emacs-keymap-popup")
     (synopsis "Described keymaps with popup help")
     (description
      "Produces a real Emacs keymap with embedded descriptions for a popup
help window.  One definition, two uses.")
     (license license:gpl3+))))

(define-public emacs-forgejo-git
  (package
   (name "emacs-forgejo-git")
   (version %version)
   (source (local-file %source-dir
                       "forgejo-checkout"
                       #:recursive? #t
                       #:select? forgejo-file?))
   (build-system emacs-build-system)
   (arguments
    (list
     #:lisp-directory "lisp"
     #:test-command #~(list "make" "-C" ".." "test")
     #:emacs emacs-next-pgtk
     #:phases
     #~(modify-phases %standard-phases
		      (add-before 'check 'set-home
				  (lambda _
				    (setenv "HOME"
					    (getenv "TMPDIR"))
				    (mkdir-p (string-append
					      (getenv "HOME")
					      "/.emacs.d")))))))
   (propagated-inputs (list emacs-markdown-mode emacs-keymap-popup))
   (home-page "https://thanosapollo.org/projects/forgejo/")
   (synopsis "Emacs front-end for Forgejo instances")
   (description
    "An Emacs front-end for Forgejo instances (Codeberg, self-hosted).
Browse, filter, and view issues and pull requests.  Caches API
responses in a local SQLite database for fast offline re-display.
This package definition builds straight from the current git
checkout, so the installed version always matches the working tree.")
   (license license:gpl3+)))

emacs-forgejo-git
