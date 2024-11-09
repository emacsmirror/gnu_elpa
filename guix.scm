(use-modules (gnu packages emacs-xyz)
             (gnu packages version-control)
             (guix build-system emacs)
             (guix download)
             (guix gexp)
             ((guix licenses) #:prefix license:)
             (guix packages))

(define-public emacs-disproject
  (package
    (name "emacs-disproject")
    (version "0.0.0-dev")
    (source
     (local-file (canonicalize-path ".")
                 #:recursive? #t
                 #:select? (lambda (file stat)
                             (not (string-contains file "/.git/")))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-transient git))
    (home-page "https://github.com/aurtzy/disproject")
    (synopsis "Transient interface for managing and interacting with projects")
    (description
     "Disproject is a package for Emacs that provides integration with
project.el via extendable Transient menus.")
    (license license:gpl3+)))

emacs-disproject
