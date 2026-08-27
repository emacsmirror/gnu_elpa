;;; gnosis-test-algorithm.el --- Gnosis Algorithm tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/gnosis
;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Testing module for gnosis algorithm functions.

;; Before making any push on master we should be passing the following
;; tests.

;;; Code:
(require 'ert)
(require 'gnosis)

(let ((lisp-dir (expand-file-name "../lisp"
                  (file-name-directory (or load-file-name default-directory)))))
  (add-to-list 'load-path lisp-dir))

(provide 'gnosis-test-algorithm)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-algorithm.el ends here
