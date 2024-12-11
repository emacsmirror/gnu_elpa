(use-modules (gnu packages base)
             (gnu packages emacs-xyz)
             (gnu packages version-control)
             (guix build-system emacs)
             (guix download)
             (guix git-download)
             (guix gexp)
             ((guix licenses) #:prefix license:)
             (guix packages))

(define repository-root-directory (dirname (current-filename)))

(define-public emacs-disproject
  (package
    (name "emacs-disproject")
    (version "0.0.0-dev")
    (source
     (local-file repository-root-directory
                 #:recursive? #t
                 #:select? (git-predicate repository-root-directory)))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-transient))
    (home-page "https://github.com/aurtzy/disproject")
    (synopsis "Transient interface for managing and interacting with projects")
    (description
     "Disproject is a package for Emacs that provides integration with
project.el via extendable Transient menus.")
    (license license:gpl3+)))

emacs-disproject
