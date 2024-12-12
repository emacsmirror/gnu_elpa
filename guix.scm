;;; greader.
;;; TODO: Copyright ©
;;;
;;; This file is part of greader.

(define-module (guix))

(import
 (ice-9 popen)
 (ice-9 textual-ports)

 (gnu packages speech)

 (guix build-system emacs)
 (guix gexp)
 (guix git-download)
 (prefix (guix licenses) license:)
 (guix packages)
 (guix utils)
 )

(define vcs-file?
  ;; Return true if the given file is under version control.
  (or (git-predicate (current-source-directory))
      (const #t)))

(define (sh-runner code)
  (with-input-from-port (open-pipe code OPEN_READ)
    (lambda ()
      (get-string-all (current-input-port)))))

(define emacs-greader-mode
  (let ((version "0.11.18")
        (revision "1"))
    (package
      (name "emacs-greader-mode")
      (version (git-version version revision (sh-runner "git rev-parse HEAD")))
      (home-page "https://gitlab.com/michelangelo-rodriguez/greader")
      (source
       (local-file "." "emacs-greader-mode-checkout"
                   #:recursive? #t
                   #:select? vcs-file?))
      (build-system emacs-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'add-requires
              (lambda _
                (substitute* "greader-dict.el"
                  ((";;; Code:")
                   ";;; Code:\n(require 'greader)\n")))))))
      (inputs (list espeak-ng))
      (synopsis
       "Gnamù Reader - greader-mode, send buffer contents to a speech engine")
      (description
       "Greader is a module that allows you to send any emacs buffer to a TTS.
A text-to-speech like engine @code{espeak-ng} or @code{speech-dispatcher} are
already supported, plus limited bakend support native to macOS.  The
mode supports timer reading, automatic scrolling of buffers in modes
like @code{info-mode}, repeating reading of regions or the whole buffer,
includes a feature to facilitate the compilation of espeak-ng
pronunciations, and other features.")
      (license license:gpl3+))))

emacs-greader-mode
