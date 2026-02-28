;;; futur-client.el --- A client to Futur's ELisp server  -*- lexical-binding: t -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

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

;;; Code:

;; (require 'trace)
;; (trace-function 'futur--elisp-process-filter)
;; (trace-function 'futur--elisp-process-answer)

(require 'futur)

(defconst futur--elisp-impossible-string "\n# \"# "
  "String that will necessarily cause `read' to signal an error.
This has to be the same used by `futur-server'.")

(defvar futur--elisp-servers nil)

(defun futur--elisp-process-filter (proc string)
  (cl-assert (memq proc futur--elisp-servers))
  (let ((pending (process-get proc 'futur--pending))
        (case-fold-search nil))
    (named-let loop ((string string))
      ;; (trace-values 'looping string)
      (pcase-exhaustive (process-get proc 'futur--state)
        (:booting
         (let ((string (if pending (concat pending string) string)))
           (if (not (string-match " \\(fes:[0-9a-f]+\\) " string))
               (process-put proc 'futur--pending string)
             (let ((before (string-trim
                            (substring string 0 (match-beginning 0)))))
               (unless (equal "" before)
                 (message "Skipping output from futur-server: %S" before)))
             (process-put proc 'futur--sid (match-string 0 string))
             (process-put proc 'futur--sid-sym (intern (match-string 1 string)))
             (process-put proc 'futur--state :sexp)
             (process-put proc 'futur--pending nil)
             (process-put proc 'futur--pendings nil)
             (when (< (match-end 0) (length string))
               (loop (substring string (match-end 0)))))))
        (:sexp
         (when pending
           (cl-assert (< (length pending)
                         (length futur--elisp-impossible-string)))
           (setq string (concat pending string))
           (process-put proc 'futur--pending nil))
         (if (not (string-match "\n" string))
             (push string (process-get proc 'futur--pendings))
           (unless (eq 0 (match-beginning 0))
             (push (substring string 0 (match-beginning 0))
                   (process-get proc 'futur--pendings))
             (setq string (substring string (match-beginning 0))))
           ;; (trace-values ':sexp string)
           (cond
            ((string-prefix-p futur--elisp-impossible-string string)
             (let* ((pendings (process-get proc 'futur--pendings))
                    (sexp-string (mapconcat #'identity (nreverse pendings) "")))
               (process-put proc 'futur--pendings nil)
               (process-put proc 'futur--state :next)
               (futur--funcall #'futur--elisp-process-answer proc sexp-string)
               (when (< (length futur--elisp-impossible-string) (length string))
                 (loop (substring string
                                  (length futur--elisp-impossible-string))))))
            ((< (length string) (length futur--elisp-impossible-string))
             (process-put proc 'futur--pending string))
            ((string-match "\n" string 1)
             (push (substring string 0 (match-beginning 0))
                   (process-get proc 'futur--pendings))
             (loop (substring string (match-beginning 0))))
            (t (push string (process-get proc 'futur--pendings))))))
        (:next
         (let ((sid (process-get proc 'futur--sid)))
           (when pending
             (cl-assert (< (length pending) (length sid)))
             (setq string (concat pending string))
             (process-put proc 'futur--pending nil))
           (cond
            ((string-match sid string)
             (let ((before (string-trim (substring string 0 (match-beginning 0))))
                   (after (substring string (match-end 0))))
               (unless (equal "" before)
                 (message "Skipping output from futur-server: %S" before))
               (process-put proc 'futur--state :sexp)
               (loop after)))
            (t
             (string-match "[:0-9a-fs]*\\'" string ;; This regexp Can't fail.
                           (max 0 (- (length string) (length sid))))
             (let ((before (string-trim
                            (substring string 0 (match-beginning 0)))))
               (unless (equal "" before)
                 (message "Skipping output from futur-server: %S" before))
               (process-put proc 'futur--pending
                            (substring string (match-beginning 0))))))))))))

(defun futur--elisp-process-filter-stderr (proc string)
  (let ((pending (process-get proc 'futur--pending)))
    (process-put proc 'futur--pending
                 (if (not (string-match "\n" string))
                     (if pending (concat pending string) string)
                   (let ((head (substring string 0 (match-beginning 0)))
                         (tail (substring string (match-end 0))))
                     (message "futur-server: %S"
                              (if pending (concat pending head) head))
                     tail)))))

(defun futur--elisp-process-sentinel (proc status)
  (if (futur--process-completed-p proc)
      (setq futur--elisp-servers (delq proc futur--elisp-servers))
    (message "futur--elisp-process-sentinel before end: %S" status)))

(defun futur--elisp-launch ()
  (let* ((buffer (get-buffer-create " *futur-server*"))
         (stderr (make-pipe-process
                  :name "futur-server-stderr"
                  :noquery t
                  :coding 'emacs-internal
                  :buffer buffer
                  :filter #'futur--elisp-process-filter-stderr
                  :sentinel #'ignore))
         (proc (make-process
                :name "futur-server"
                :noquery t
                :buffer buffer
                :connection-type 'pipe
                :coding 'emacs-internal
                :stderr stderr
                :filter #'futur--elisp-process-filter
                :sentinel #'futur--elisp-process-sentinel
                :command
                `(,(expand-file-name invocation-name invocation-directory)
                  "-Q" "--batch"
                  "-l" ,(locate-library "futur-server")
                  "-f" "futur-elisp-server"))))
    (process-put proc 'futur--state :booting)
    (push proc futur--elisp-servers)
    proc))

(defun futur--elisp-process-answer (proc sexp-string)
  (pcase-let* ((`(,sexp . ,end)
                (condition-case err
                         (read-from-string sexp-string)
                       (error `((:unreadable-answer . ,err)
                                . ,(length sexp-string)))))
               (sexp (if (string-match "[^ \n\t]" sexp-string end)
                         `(:trailing-garbage ,sexp ,(substring sexp-string end))
                       sexp))
               (futur (process-get proc 'futur--destination)))
    (if (null futur)
        ;; FIXME: Maybe it's just that we haven't finished processing
        ;; the previous answer and thus haven't yet installed the next
        ;; `futur--destination'.
        (message "Unsolicited futur-server answer: %S" sexp)
      (process-put proc 'futur--destination nil)
      (futur-deliver-value futur sexp))))

(defun futur--elisp-get-process ()
  (or (seq-find (lambda (proc) (process-get proc 'futur--ready))
                futur--elisp-servers)
      (futur-let*
          ((proc (futur--elisp-launch))
           (answer
            <- (futur-new (lambda (futur)
                            (process-put proc 'futur--destination futur)
                            ;; FIXME: Wait more efficiently and abort
                            ;; more cleanly.
                            ;; `(futur-server . ,proc)
                            nil))))
        (if (eq answer :ready)
            (progn
              (process-put proc 'futur--ready t)
              proc)
          (error "unexpected boot message from futur-server: %S" answer)))))

;; (cl-defmethod futur-blocker-abort ((_ (head futur-server)) _)
;;   ;; Don't kill the server, since we may want to reuse it for other
;;   ;; requests.
;;   nil)
;; (cl-defmethod futur-blocker-wait ((blocker (head futur-server)))
;;   (while ?? (accept-process-output proc ...)))


(provide 'futur-client)
;;; futur-client.el ends here
