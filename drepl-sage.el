;;; drepl-sage.el --- dREPL for SageMath  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: languages, processes
;; URL: https://github.com/astoff/drepl

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

;; This file defines a shell for Sage (https://www.sagemath.org/).

;;; Code:

(require 'comint-mime)
(require 'drepl-ipython)
(require 'mathjax)

;;; Customization options

(defgroup drepl-sage nil
  "Sage shell implemented via dREPL."
  :group 'drepl
  :group 'python
  :link '(url-link "https://github.com/astoff/drepl"))

(defcustom drepl-sage-python-interpreter '("sage-python")
  "Python interpreter used to run Sage.
This should be a list consisting of the executable name optionally
followed by command line arguments."
  :type '(choice
          (const :tag "Use system installation" ("sage-python"))
          (const :tag "Run via Podman"
                 ("podman" "run" "--rm" "-i" "--entrypoint" "sage/src/bin/sage"
                  "docker.io/sagemath/sagemath" "--python"))
          (const :tag "Run via Docker"
                 ("docker" "run" "--rm" "-i" "--entrypoint" "sage/src/bin/sage"
                  "sagemath/sagemath" "--python"))
          (repeat :tag "Custom command" string)))

(defcustom drepl-sage-config nil
  "Customization options for the Sage shell.
See `drepl-ipython-config' to learn how to adjust this variable."
  :type 'plist)

(defvar drepl-sage--start-file
  (expand-file-name "drepl-sage.py" (file-name-directory (macroexp-file-name)))
  "File name of the startup script.")

;;;###autoload (autoload 'drepl-sage "drepl-sage" nil t)
(drepl--define drepl-sage :display-name "Sage")

(cl-defmethod drepl--command ((_ drepl-sage))
  `(,@drepl-sage-python-interpreter "-c" "\
from sys import stdin; \
exec(stdin.read(int(stdin.readline()))); \
SageRepl.run()"))

(defun drepl-sage--render-html (header data)
  (let ((start (point)))
    (let ((shr-fill-text nil))
      (comint-mime-render-html header data))
    (mathjax-typeset-region start (point))))

(cl-defmethod drepl--init ((repl drepl-sage))
  (cl-call-next-method repl)
  (drepl--adapt-comint-to-mode ".py")
  (push '("5151" . comint-mime-osc-handler) ansi-osc-handlers)
  (make-local-variable 'comint-mime-renderer-alist)
  (push '("\\`text/html\\>" . drepl-sage--render-html)
        comint-mime-renderer-alist)
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-file-contents drepl-ipython--start-file)
      (goto-char (point-max))
      (insert-file-contents drepl-sage--start-file)
      (process-send-string buffer (format "%s\n" (buffer-size)))
      (process-send-region buffer (point-min) (point-max))
      (process-send-string buffer (json-serialize drepl-sage-config))
      (process-send-string buffer "\n"))))

(provide 'drepl-sage)

;;; drepl-sage.el ends here
