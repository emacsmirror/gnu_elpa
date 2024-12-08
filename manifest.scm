(use-modules (gnu)
             (gnu packages emacs-xyz)
             (guix)
             (guix git-download))

;; Not upstreamed yet
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

(packages->manifest
 (append (specifications->packages (list "findutils" "git" "grep"))
         (list emacs-transient/newer)))
