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

;; Disproject depends on emacs-transient>=0.8.0 which is not yet available
;; upstream - older versions may not work.  See manifest.scm for newer
;; emacs-transient package that can be used (needs to replace older
;; emacs-transient in guix environment completely to be used).
(define-public emacs-disproject
  (package
    (name "emacs-disproject")
    (version "0.0.0-dev")
    (source
     (local-file repository-root-directory
                 #:recursive? #t
                 #:select? (git-predicate repository-root-directory)))
    (build-system emacs-build-system)
    ;; Some project.el functions depend on external programs.
    (propagated-inputs (list emacs-transient findutils git grep))
    (home-page "https://github.com/aurtzy/disproject")
    (synopsis "Transient interface for managing and interacting with projects")
    (description
     "Disproject is a package for Emacs that provides integration with
project.el via extendable Transient menus.")
    (license license:gpl3+)))

emacs-disproject
