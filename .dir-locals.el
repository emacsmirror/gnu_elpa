;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((fill-column . 70)
         (sentence-end-double-space . t)
         (indent-tabs-mode . nil)))
 (emacs-lisp-mode . ((bug-reference-url-format . (lambda ()
                                                   (format "https://codeberg.org/emacs-jj-vc/vc-jj.el/%s/%s"
                                                           (if (member (match-string 2) '("pr" "PR" "pull" "Pull" "PULL"))
                                                               "pulls"
                                                             "issues")
                                                           (match-string 3))))
                     ;; Matches bug references of the form
                     ;; KEYWORD#NUMBER, where KEYWORD is one of: bug,
                     ;; Bug, BUG, issue, Issue, ISSUE, pull, Pull,
                     ;; PULL, pr, PR.
                     (bug-reference-bug-regexp . "\\(\\(\\(?:B\\(?:UG\\|ug\\)\\|I\\(?:SSUE\\|ssue\\)\\|P\\(?:R\\|ULL\\|ull\\)\\|bug\\|issue\\|p\\(?:r\\|ull\\)\\)\\)#\\([[:digit:]]+\\)\\)")
                     (eval . (bug-reference-prog-mode 1)))))
