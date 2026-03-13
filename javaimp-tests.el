;;; javaimp-tests.el --- javaimp test util  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert-x)

(defun javaimp-call-with-data (filename handler)
  "Untar FILENAME into temporary directory and call HANDLER,
supplying that directory name as the only arg."
  (ert-with-temp-directory tmpdir
    :prefix "javaimp-test"
    (let ((rc (call-process
               "tar" nil nil nil
               "-x"
               "-f" filename
               "-C" tmpdir)))
      (if (= rc 0)
          (funcall handler tmpdir)
        (error "Cannot untar test data file %s: %d" filename rc)))))

(defmacro javaimp-with-temp-buffer (file &rest body)
  "Execute BODY in temporary buffer with `javaimp-minor-mode'
turned on.  If FILE is non-nil then its contents are inserted."
  (declare (debug t) (indent 1))
  `(with-temp-buffer
     (when ,file
       (insert-file-contents (ert-resource-file ,file)))
     (java-mode)
     (javaimp-minor-mode)
     ,@body))

(provide 'javaimp-tests)
