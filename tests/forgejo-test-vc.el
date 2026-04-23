;;; forgejo-test-vc.el --- Tests for forgejo-vc  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for AGit-Flow helpers: refspec building, description
;; encoding, and push option construction.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "../lisp/forgejo.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/forgejo-utils.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/forgejo-vc.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; ---- Group 1: Refspec building ----

(ert-deftest forgejo-test-vc-refspec ()
  "Build a correct AGit-Flow refspec."
  (should (string= (forgejo-vc--refspec "HEAD" "main" "fix-login")
                   "HEAD:refs/for/main/fix-login")))

(ert-deftest forgejo-test-vc-refspec-feature-branch ()
  "Refspec works with feature branch topics."
  (should (string= (forgejo-vc--refspec "HEAD" "develop" "feature/auth")
                   "HEAD:refs/for/develop/feature/auth")))

;;; ---- Group 2: Description encoding ----

(ert-deftest forgejo-test-vc-encode-description ()
  "Base64-encode description with {base64} prefix."
  (let ((result (forgejo-vc--encode-description "Hello World")))
    (should (string-prefix-p "{base64}" result))
    (should (string= (decode-coding-string
                       (base64-decode-string
                        (substring result (length "{base64}")))
                       'utf-8)
                      "Hello World"))))

(ert-deftest forgejo-test-vc-encode-description-unicode ()
  "UTF-8 text roundtrips through encoding."
  (let* ((text "Unicode: \u03b1\u03b2\u03b3")
         (result (forgejo-vc--encode-description text))
         (decoded (decode-coding-string
                   (base64-decode-string
                    (substring result (length "{base64}")))
                   'utf-8)))
    (should (string= decoded text))))

;;; ---- Group 3: Push options ----

(ert-deftest forgejo-test-vc-push-options ()
  "Push options include title and encoded description."
  (let ((opts (forgejo-vc--push-options "My PR" "Description text")))
    (should (= (length opts) 4))
    (should (string= (nth 0 opts) "-o"))
    (should (string= (nth 1 opts) "title=My PR"))
    (should (string= (nth 2 opts) "-o"))
    (should (string-prefix-p "description={base64}" (nth 3 opts)))))

;;; ---- Group 4: Remote detection ----

(ert-deftest forgejo-test-vc-repo-from-remote-https ()
  "Parse HTTPS remote into (HOST OWNER REPO)."
  (cl-letf (((symbol-function 'process-file)
             (lambda (_prog _infile _dest _display &rest _args)
               (insert "https://codeberg.org/thanos/forgejo.git")
               0)))
    (should (equal (forgejo-vc--repo-from-remote)
                   '("https://codeberg.org" "thanos" "forgejo")))))

(ert-deftest forgejo-test-vc-repo-from-remote-ssh ()
  "Parse SSH remote into (HOST OWNER REPO)."
  (cl-letf (((symbol-function 'process-file)
             (lambda (_prog _infile _dest _display &rest _args)
               (insert "ssh://git@codeberg.org/thanos/forgejo.git")
               0)))
    (should (equal (forgejo-vc--repo-from-remote)
                   '("https://codeberg.org" "thanos" "forgejo")))))

(ert-deftest forgejo-test-vc-repo-from-remote-scp ()
  "Parse SCP-style remote into (HOST OWNER REPO)."
  (cl-letf (((symbol-function 'process-file)
             (lambda (_prog _infile _dest _display &rest _args)
               (insert "git@codeberg.org:thanos/forgejo")
               0)))
    (should (equal (forgejo-vc--repo-from-remote)
                   '("https://codeberg.org" "thanos" "forgejo")))))

(ert-deftest forgejo-test-vc-repo-from-remote-selfhosted ()
  "Parse HTTPS remote from a self-hosted instance."
  (cl-letf (((symbol-function 'process-file)
             (lambda (_prog _infile _dest _display &rest _args)
               (insert "https://git.example.com/org/project.git")
               0)))
    (should (equal (forgejo-vc--repo-from-remote)
                   '("https://git.example.com" "org" "project")))))

(provide 'forgejo-test-vc)
;;; forgejo-test-vc.el ends here
