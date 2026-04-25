;;; forgejo-test-host.el --- Tests for host resolution and token lookup  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for forgejo-hosts configuration, host validation, token
;; resolution, and host-from-hosts-list selection.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "../lisp/forgejo.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; ---- Group 1: Host validation ----

(ert-deftest forgejo-test-host-validate-known ()
  "Known host passes validation."
  (let ((forgejo-hosts '(("https://codeberg.org")
                         ("https://git.myorg.com"))))
    (should-not (forgejo--validate-host "https://codeberg.org"))
    (should-not (forgejo--validate-host "https://git.myorg.com"))))

(ert-deftest forgejo-test-host-validate-unknown ()
  "Unknown host signals error."
  (let ((forgejo-hosts '(("https://codeberg.org"))))
    (should-error (forgejo--validate-host "https://unknown.example.com")
                  :type 'user-error)))

;;; ---- Group 2: Token resolution ----

(ert-deftest forgejo-test-host-token-inline ()
  "Inline token from forgejo-hosts is returned."
  (let ((forgejo-hosts '(("https://codeberg.org" "tok_inline"))))
    (should (string= (forgejo--hosts-token "https://codeberg.org")
                     "tok_inline"))))

(ert-deftest forgejo-test-host-token-no-inline ()
  "Host without inline token returns nil."
  (let ((forgejo-hosts '(("https://codeberg.org"))))
    (should-not (forgejo--hosts-token "https://codeberg.org"))))

(ert-deftest forgejo-test-host-token-unknown ()
  "Unknown host returns nil for inline token."
  (let ((forgejo-hosts '(("https://codeberg.org" "tok_abc"))))
    (should-not (forgejo--hosts-token "https://unknown.example.com"))))

(ert-deftest forgejo-test-host-token-full-resolution ()
  "Token resolution: inline -> auth-source -> forgejo-token -> error."
  (let ((forgejo-hosts '(("https://codeberg.org" "tok_inline")))
        (forgejo-token-use-auth-source nil)
        (forgejo-token "tok_fallback"))
    ;; Inline token wins
    (should (string= (forgejo-token "https://codeberg.org") "tok_inline"))))

(ert-deftest forgejo-test-host-token-fallback-variable ()
  "Falls back to forgejo-token variable when no inline or auth-source."
  (let ((forgejo-hosts '(("https://codeberg.org")))
        (forgejo-token-use-auth-source nil)
        (forgejo-token "tok_fallback"))
    (should (string= (forgejo-token "https://codeberg.org") "tok_fallback"))))

(ert-deftest forgejo-test-host-token-error-when-none ()
  "Signals error when no token found anywhere."
  (let ((forgejo-hosts '(("https://codeberg.org")))
        (forgejo-token-use-auth-source nil)
        (forgejo-token nil))
    (should-error (forgejo-token "https://codeberg.org")
                  :type 'user-error)))

(ert-deftest forgejo-test-host-token-validates-first ()
  "Token lookup validates host before searching."
  (let ((forgejo-hosts '(("https://codeberg.org")))
        (forgejo-token "tok_exists"))
    (should-error (forgejo-token "https://evil.example.com")
                  :type 'user-error)))

;;; ---- Group 3: Host selection ----

(ert-deftest forgejo-test-host-from-list-single ()
  "Single configured host is returned without prompting."
  (let ((forgejo-hosts '(("https://codeberg.org"))))
    (should (string= (forgejo--host-from-hosts-list)
                     "https://codeberg.org"))))

(ert-deftest forgejo-test-host-from-list-empty ()
  "Empty forgejo-hosts signals error."
  (let ((forgejo-hosts nil))
    (should-error (forgejo--host-from-hosts-list)
                  :type 'user-error)))

;;; ---- Group 4: Host URL lookup ----

(ert-deftest forgejo-test-host-url-for-hostname ()
  "Look up full URL from hostname."
  (let ((forgejo-hosts '(("https://codeberg.org")
                         ("https://git.myorg.com" "tok"))))
    (should (string= (forgejo--host-url-for-hostname "codeberg.org")
                     "https://codeberg.org"))
    (should (string= (forgejo--host-url-for-hostname "git.myorg.com")
                     "https://git.myorg.com"))))

(ert-deftest forgejo-test-host-url-for-hostname-fallback ()
  "Unknown hostname falls back to https://HOSTNAME."
  (let ((forgejo-hosts '(("https://codeberg.org"))))
    (should (string= (forgejo--host-url-for-hostname "unknown.example.com")
                     "https://unknown.example.com"))))

(provide 'forgejo-test-host)
;;; forgejo-test-host.el ends here
