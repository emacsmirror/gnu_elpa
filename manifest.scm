(use-modules (gnu)
             (gnu packages emacs-xyz)
             (guix)
             (guix git-download))

;; Not upstreamed yet
(define-public emacs-transient/newer
  (package/inherit emacs-transient
    (name "emacs-transient")
    (version "0.7.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/magit/transient")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k30zhj2acs9spri6pqcyc0v0wcd9smb3xgl1vm0i6485d9lvr2p"))))))

(packages->manifest
 (append (specifications->packages (list "findutils" "git" "grep"))
         (list emacs-transient/newer)))
