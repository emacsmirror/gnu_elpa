(use-modules (gnu packages base)
             (gnu packages emacs-xyz)
             (gnu packages texinfo)
             (gnu packages version-control)
             (guix build-system emacs)
             (guix download)
             (guix git-download)
             (guix gexp)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix transformations))

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
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-before 'install 'build-info-manual
                     (lambda _
                       (invoke "make" "info"))))))
    (native-inputs (list gnu-make texinfo))
    (propagated-inputs
     (let ((transform (options->transformation
                       '((with-commit . "emacs-transient=v0.9.2")))))
       (map transform (list emacs-transient))))
    (home-page "https://github.com/aurtzy/disproject")
    (synopsis "Transient interface for managing and interacting with projects")
    (description
     "Disproject is a package for GNU Emacs that implements Transient menus for
dispatching project-related commands on top of the @code{project.el} library.  It
aims to provide a more featureful version of the @code{project-switch-project}
command, which it is inspired by.  Those who are familiar with Projectile may also
find similarities to @code{projectile-commander}.")
    (license license:gpl3+)))

emacs-disproject
