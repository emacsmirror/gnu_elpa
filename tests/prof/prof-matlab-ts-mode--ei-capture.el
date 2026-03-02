;;; prof-matlab-ts-mode--ei-capture.el --- -*- lexical-binding: t -*-

;; Version: 8.0.0
;; URL: https://github.com/mathworks/Emacs-MATLAB-Mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: John Ciolfi <john.ciolfi.32@gmail.com>
;; Keywords: MATLAB

;; Copyright (C) 2026 Free Software Foundation, Inc.
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;    Manually profile tree-sitter node captures.


;;; Code:

(require 'profiler)
(require 'matlab-ts-mode)

(defun matlab-ts-mode--ei-elapsed-time (start-time)
  "Return elapsed time string, now - START-TIME."
  (concat "Elapsed time: "
          (string-trim (format "%10.1f" (float-time (time-subtract (current-time) start-time))))
          " seconds."))

(defvar matlab-ts-mode--ei-all-nodes-query (when (treesit-available-p)
                                             (treesit-query-compile 'matlab '(_ @n))))

(defun matlab-ts-mode--ei-line-nodes-in-region (beg end)
  "Get line nodes in region BEG to END.
Line nodes are typically leaf nodes except for nodes like strings which
are not leaf nodes.  Returns list with elements (NODE
. MODIFIED-NODE-TYPE) where MODIFIED-NODE-TYPE is the NODE type
adjusted.  For example, when NODE parent is a string and NODE is the
string start characters, we return the parent string node.  Another
example: when NODE is a \"+\" and parent is a unary_operator, we return
MODIFIED-NODE-TYPE to be unary-op even though the node type is \"+\"."
  (matlab-ts-mode--ei-fast-back-to-indentation)
  (let ((region-nodes (treesit-query-capture (treesit-buffer-root-node 'matlab)
                                             matlab-ts-mode--ei-all-nodes-query beg end t))
        dimensions-node ;; we use this to track the "number" and ":" with in properties dimension
        line-nodes)

    (dolist (node region-nodes)
      (let ((node-type (treesit-node-type node)))

        (cond

         ;; Case: \n - ignore these, we don't pad them or anything
         ((string= node-type "\n")
          (setq node nil))

         ;; Case: track when in a property dimensions node
         ((string= node-type "dimensions")
          (setq node nil
                dimensions-node node))

         ;; Case: property dimensions
         ;;   foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
         ((and dimensions-node (or (string= node-type "number") (string= node-type ":")))
          (setq node-type "prop-dim"))

         ;; Case: lambda:     @(x) ((ischar(x) || isstring(x)))
         ;;                      ^
         ;;       properties: foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
         ;;                             ^
         ((string= node-type ")")
          (setq dimensions-node nil) ;; close of dimension-node or other close paren
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (if (string= parent-type "lambda")
                (setq node-type "lambda-)")
              (if (string= parent-type "dimensions")
                  (setq node-type "dim-)")))))

         ;; Case: parts of a string to ignore ("), ('), "string_content"
         ((or (string= node-type "\"") (string= node-type "'") (string= node-type "string_content"))
          (setq node nil))

         ;; Case: string ("double-quote-string" or 'single-quote-string')
         ((string= node-type "string")
          nil)

         ;; Case: prop-id, prop-class-id, enum-id
         ((string= node-type "identifier")
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (cond ((string= parent-type "property") ;; propertyWithOutDot?
                   (if (equal (treesit-node-child parent 0) node)
                       (setq node-type "prop-id")
                     (setq node-type "prop-class-id")))
                  ((string= parent-type "property_name") ;; property.nameWithDot?
                   (if (equal (treesit-node-child (treesit-node-parent parent) 0) parent)
                       (setq node-type "prop-id")
                     (setq node-type "prop-class-id")))
                  ((string= parent-type "enum")
                   (setq node-type "enum-id")))))

         ;; Case: unary operator sign, + or -, e.g. [0 -e] or g = - e
         ((or (string= node-type "+") (string= node-type "-"))
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (when(and (string= parent-type "unary_operator")
                      (equal (treesit-node-child parent 0) node))
              (setq node-type "unary-op"))))

         ;; Case: super-class constructor call
         ;;  obj@myUtils.super;
         ((string= node-type "@")
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (when (string= parent-type "function_call")
              (setq node-type "@-fcn-call"))))

         ;; Case: events, enumeration, methods
         ((string-match-p (rx bos (or "events" "enumeration" "methods" "arguments") eos) node-type)
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (when (string= parent-type "identifier")
              ;; TopTester: electric_indent_inspect_keyword_commands.m
              ;; TopTester: electric_indent_inspect_keyword_commands2.m
              (setq node-type (concat node-type "-fcn")))))

         ;; Case: arguments fcn keyword: arguments (1, :) {mustBeNumeric}
         ;;                                        ^
         ((string= node-type "(")
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (when (string= parent-type "dimensions")
              (setq node-type "dim-("))))

         ((not (= (treesit-node-child-count node) 0)) ;; non-leaf node?
          (setq node nil)))

        (when node
          (push `(,node ,node-type) line-nodes))))

    (reverse line-nodes)))

(defun prof-matlab-ts-mode--ei-line-nodes-region (arg)
  "Profile `matlab-ts-mode--ei-line-nodes-in-region'.
With prefix ARG, report elapsed time without profiling."
  (interactive "P")
  (when (not (eq major-mode 'matlab-ts-mode))
    (user-error "Buffer %s major-mode is not matlab-ts-mode" (buffer-name)))
  (goto-char (point-min))
  (let ((start-time (current-time))
        line-nodes)
    (when (not arg)
      (profiler-start 'cpu))
    (unwind-protect
        (setq line-nodes (matlab-ts-mode--ei-line-nodes-in-region (point-min) (point-max)))
      (when (not arg)
        (profiler-stop)
        (profiler-report)))
    (message "Found %d line nodes. %s" (length line-nodes)
             (matlab-ts-mode--ei-elapsed-time start-time))))

(defun matlab-ts-mode--ei-nodes-in-line ()
  "Get leave nodes in current line."
  (matlab-ts-mode--ei-fast-back-to-indentation)
  (let ((line-nodes (treesit-query-capture (treesit-buffer-root-node)
                                           matlab-ts-mode--ei-all-nodes-query (point) (pos-eol) t))
        line-leaf-nodes)
    (dolist (node line-nodes)
      (when (= (treesit-node-child-count node) 0)
        (push node line-leaf-nodes)))
    (reverse line-leaf-nodes)))

(defun prof-matlab-ts-mode--ei-nodes-in-line (arg)
  "Profile `matlab-ts-mode--ei-nodes-in-line'.
This profiles the current `matlab-ts-mode' buffer.
With prefix ARG, report elapsed time without profiling."
  (interactive "P")
  (when (not (eq major-mode 'matlab-ts-mode))
    (user-error "Buffer %s major-mode is not matlab-ts-mode" (buffer-name)))
  (goto-char (point-min))
  (let ((start-time (current-time))
        (count 0))
    (when (not arg)
      (profiler-start 'cpu))
    (unwind-protect
        (while (not (eobp))
          (let ((line-leaf-nodes (matlab-ts-mode--ei-nodes-in-line)))
            (setq count (+ count (length line-leaf-nodes))))
          (forward-line))
      (when (not arg)
        (profiler-stop)
        (profiler-report)))
    (message "Found %d leaf nodes. %s" count
             (matlab-ts-mode--ei-elapsed-time start-time))))

(provide 'prof-matlab-ts-mode--ei-capture)
;;; prof-matlab-ts-mode--ei-capture.el ends here

;; LocalWords:  SPDX gmail defun treesit dolist setq isstring bos eos eol eobp
