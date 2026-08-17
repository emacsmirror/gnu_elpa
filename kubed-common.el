;;; kubed-common.el --- Common definitons for Kubed   -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Free Software Foundation, Inc.

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Keywords: tools

;;; Commentary:

;; This library provides definitions that both `kubed' and `kubed-tramp'
;; require.

;;; Code:

(require 'files-x)

(defvar kubed-tramp-method "kubedv2"    ;Versioned, for compatibility.
  ;; (find-file "/kubedv2:CONTEXT%NAMESPACE%POD%CONTAINER:/some/file")
  "Name of the Kubed Tramp method.")

(defcustom kubed-kubectl-program "kubectl"
  "Name of `kubectl' executable to use for interacting with Kubernetes."
  :type 'string
  :group 'kubed)

(defun kubed-kubectl-program ()
  "Return the connection-local value of variable `kubed-kubectl-program'."
  ;; This is the expansion of the following form, which we spell out
  ;; because `connection-local-value' is not available in Emacs 29:
  ;; (connection-local-value kubed-kubectl-program 'kubed)
  (let ((criteria (connection-local-criteria-for-default-directory 'kubed))
        connection-local-variables-alist file-local-variables-alist)
    (if (not criteria) kubed-kubectl-program
      (hack-connection-local-variables criteria)
      (if-let* ((result (assq 'kubed-kubectl-program
                              connection-local-variables-alist)))
          (cdr result)
        kubed-kubectl-program))))

(provide 'kubed-common)
;;; kubed-common.el ends here
