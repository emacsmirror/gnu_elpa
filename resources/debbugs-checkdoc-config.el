;;; debbugs-checkdoc-config.el --- Configuration for running checkdoc on debbugs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Morgan Smith <Morgan.J.Smith@outlook.com>
;; Package: debbugs

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar checkdoc-package-keywords-flag)
(defvar checkdoc-spellcheck-documentation-flag)
(defvar checkdoc-ispell-lisp-words)

(with-eval-after-load "checkdoc"
  (setq checkdoc-package-keywords-flag t)
  (setq checkdoc-spellcheck-documentation-flag t)
  (setq checkdoc-ispell-lisp-words
        '("ChangeLog" "ChangeLogs" "UTF" "alist" "args"
          "armstrong" "backend" "bcc" "bugreport" "cdate" "cedet"
          "coreutils" "cygwin" "debbugs" "debian" "el" "emacs"
          "etags" "freemail" "fsf" "guix" "gw" "henoch" "hu"
          "hyperestraier" "keymap" "magit" "magnus" "maint"
          "maintainer" "maintainer's" "mbox" "mboxes" "minibuffer"
          "moreinfo" "multibyte" "notabug" "paren" "persistency"
          "regexp" "rescan" "rgm" "rmail" "severities" "sexp"
          "solaris" "src" "sublist" "submitter" "submitter's"
          "subproduct" "subqueries" "subquery" "teardown"
          "unarchived" "unibyte" "unreproducible" "url" "util"
          "wishlist" "wontfix" "wsdl" "www" "xsd" "zltuz")))

(provide 'debbugs-checkdoc-config)

;;; debbugs-checkdoc-config.el ends here
