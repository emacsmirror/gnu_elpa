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
;; (trace-function 'process-send-region)
;; (trace-function 'process-send-string)
;; (trace-function 'futur--funcall)
;; (trace-function 'futur--elisp-get-process)
;; (trace-function 'futur--elisp-funcall)

(require 'futur)

(defconst futur--elisp-impossible-string "\n# \"# "
  "String that will necessarily cause `read' to signal an error.
This has to be the same used by `futur-server'.")

(defvar futur--elisp-servers nil)

(defun futur--elisp-process-filter (proc string)
  (cl-assert (memq proc futur--elisp-servers))
  (let ((pending (process-get proc 'futur--pending))
        (case-fold-search nil))
    (process-put proc 'futur--pending nil)
    (named-let loop ((string (if pending (concat pending string) string)))
      ;; (trace-values :looping (process-get proc 'futur--state) string)
      (pcase-exhaustive (process-get proc 'futur--state)
        (:booting
         (if (not (string-match " \\(fes:[0-9a-f]+\\) " string))
             (process-put proc 'futur--pending string)
           (let ((before (string-trim
                          (substring string 0 (match-beginning 0)))))
             (unless (equal "" before)
               (message "Skipping output from futur-server: %S" before)))
           (process-put proc 'futur--sid (match-string 0 string))
           (process-put proc 'futur--sid-sym (intern (match-string 1 string)))
           (process-put proc 'futur--state :sexp)
           (process-put proc 'futur--pendings nil)
           (when (< (match-end 0) (length string))
             (loop (substring string (match-end 0))))))
        (:sexp
         (when pending
           (cl-assert (< (length pending)
                         (length futur--elisp-impossible-string))))
         (if (not (string-match "\n" string))
             (push string (process-get proc 'futur--pendings))
           (unless (eq 0 (match-beginning 0))
             (push (substring string 0 (match-beginning 0))
                   (process-get proc 'futur--pendings))
             (setq string (substring string (match-beginning 0))))
           ;; (trace-values :sexp string)
           (cond
            ((string-prefix-p futur--elisp-impossible-string string)
             (let* ((pendings (process-get proc 'futur--pendings))
                    (sexp-string (mapconcat #'identity (nreverse pendings) "")))
               (process-put proc 'futur--pendings nil)
               (process-put proc 'futur--state :next)
               (futur--funcall #'futur--elisp-process-answer proc sexp-string)
               (when (< (length futur--elisp-impossible-string) (length string))
                 ;; (trace-values :loop1)
                 (loop (substring string
                                  (length futur--elisp-impossible-string))))))
            ((< (length string) (length futur--elisp-impossible-string))
             (process-put proc 'futur--pending string))
            ((string-match "\n" string 1)
             (push (substring string 0 (match-beginning 0))
                   (process-get proc 'futur--pendings))
             ;; (trace-values :loop2)
             (loop (substring string (match-beginning 0))))
            (t (push string (process-get proc 'futur--pendings))))))
        (:next
         (let ((sid (process-get proc 'futur--sid)))
           (when pending
             (cl-assert (< (length pending) (length sid))))
           (cond
            ((string-match sid string)
             (let ((after (substring string (match-end 0)))
                   (before (string-trim
                            (substring string 0 (match-beginning 0)))))
               (unless (equal "" before)
                 (message "Skipping output from futur-server: %S" before))
               (process-put proc 'futur--state :sexp)
               ;; (trace-values :loop3 before sid after)
               (loop after)))
            (t
             (string-match "[:0-9a-fs]*\\'" string ;; This regexp Can't fail.
                           (max 0 (- (length string) (length sid))))
             (let ((after (substring string (match-beginning 0)))
                   (before (string-trim
                            (substring string 0 (match-beginning 0)))))
               (unless (equal "" before)
                 (message "Skipping output from futur-server: %S" before))
               (process-put proc 'futur--pending after))))))))))

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
    (process-put proc 'futur--rid 0)
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
        ;; Hopefully, some destination will show up later to consume it.
        (process-put proc 'futur--answers
                     (nconc (process-get proc 'futur--answers) (list sexp)))
      (process-put proc 'futur--destination nil)
      (futur-deliver-value futur sexp))))

(defun futur--elisp-set-destination (proc futur)
  (cl-assert (null (process-get proc 'futur--destination)))
  (let ((answers (process-get proc 'futur--answers)))
    (if answers
        (let ((answer (car answers)))
          (process-put proc 'futur--answers (cdr answers))
          (futur-deliver-value futur answer))
      (process-put proc 'futur--destination futur))))

(defun futur--elisp-answer-futur (proc)
  (futur-new (lambda (futur)
               (futur--elisp-set-destination proc futur)
               ;; FIXME: Wait more efficiently and abort
               ;; more cleanly.
               ;; `(futur-server . ,proc)
               nil)))

(defun futur--elisp-get-process ()
  (let ((ready (seq-find (lambda (proc) (process-get proc 'futur--ready))
                         futur--elisp-servers)))
    (if ready (futur-done ready)
      (futur-let*
          ((proc (futur--elisp-launch))
           (answer <- (futur--elisp-answer-futur proc)))
        (if (eq answer :ready)
            (progn
              (process-put proc 'futur--ready t)
              proc)
          (error "unexpected boot message from futur-server: %S" answer))))))

;; (cl-defmethod futur-blocker-abort ((_ (head futur-server)) _)
;;   ;; Don't kill the server, since we may want to reuse it for other
;;   ;; requests.
;;   nil)
;; (cl-defmethod futur-blocker-wait ((blocker (head futur-server)))
;;   (while ?? (accept-process-output proc ...)))

(defun futur--elisp-funcall (func &rest args)
  (futur-let*
      ((proc <- (futur--elisp-get-process))
       (rid (cl-incf (process-get proc 'futur--rid)))
       (_ (with-temp-buffer
            ;; (trace-values :funcall rid func args)
            (process-put proc 'futur--ready nil)
            (let ((print-length nil)
                  (print-level nil)
                  (coding-system-for-write 'emacs-internal)
                  (print-circle t)
                  (print-gensym t)
                  ;; The server reads with `read-from-minibuffer' which
                  ;; works only on single-lines, so it's super-important
                  ;; we don't include any LF by accident.
                  (print-escape-newlines t)
                  ;; SWP aren't currently printed in a `read'able way, so we may
                  ;; as well print them bare.
                  (print-symbols-bare t))
              (prin1 `(,(process-get proc 'futur--sid-sym) ,rid
                       ,func ,@args)
                     (current-buffer))
              (insert "\n")
              (process-send-string proc (buffer-string))
              ;; (process-send-region proc (point-min) (point-max))
              )))
       (read-answer <- (futur--elisp-answer-futur proc)))
    ;; (trace-values :read-answer read-answer)
    (pcase-exhaustive read-answer
      (`(:read-success ,(pred (equal rid)))
       (futur-let* ((call-answer  <- (futur--elisp-answer-futur proc)))
         (pcase-exhaustive call-answer
           (`(:funcall-success ,(pred (equal rid)) . ,val)
            (process-put proc 'futur--ready t)
            val)
           (`(:funcall-error ,(pred (equal rid)) . ,err)
            (process-put proc 'futur--ready t)
            (futur--resignal err))))))))

(provide 'futur-client)
;;; futur-client.el ends here
