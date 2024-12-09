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

(define-public emacs-transient/newer
  (package/inherit emacs-transient
    (name "emacs-transient")
    (version "0.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/magit/transient")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1f8w6b242ar0538pimnrf7a5j277q80z7n379hyihdblvjk90xi2"))))))

;; Disproject depends on emacs-transient>=0.8.0 which is not yet available
;; upstream.  emacs-transient/newer may be used, but needs to replace
;; emacs-transient in all other emacs packages for the environment as well to
;; reliably work.
(define-public emacs-disproject
  (package
    (name "emacs-disproject")
    (version "0.0.0-dev")
    (source
     (local-file repository-root-directory
                 #:recursive? #t
                 #:select? (git-predicate repository-root-directory)))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-transient/newer))
    (home-page "https://github.com/aurtzy/disproject")
    (synopsis "Transient interface for managing and interacting with projects")
    (description
     "Disproject is a package for Emacs that provides integration with
project.el via extendable Transient menus.")
    (license license:gpl3+)))

emacs-disproject
