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

(defvar futur--elisp-servers nil
  "Alist mapping server kinds to lists of processes.
A server kind is a symbol.")

(defvar futur--elisp-servers-delay (* 60 10)
  "Number of seconds after which an inactive server is killed.")

(defvar futur--elisp-servers-timer nil)

(defun futur--elisp-process-filter (proc string)
  (cl-assert (process-get proc 'futur--kind))
  (cl-assert (memq proc (assq (process-get proc 'futur--kind)
                              futur--elisp-servers)))
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
               (process-put proc 'futur--pending after)))))))))
  nil)

(defun futur--elisp-process-filter-stderr (proc string)
  (let ((pending (process-get proc 'futur--pending)))
    (while (string-match "\n" string)
      (let* ((head (substring string 0 (match-beginning 0)))
             (tail (substring string (match-end 0)))
             (line (if pending (concat pending head) head))
             (parent (process-get proc 'futur--parent))
             (kind (if parent (process-get parent 'futur--kind) 'orphan)))
        (unless (equal line "")
          (message "%s: %S" (or kind 'futur-unknown-kind) line))
        (setq pending nil)
        (setq string tail)))
    (process-put proc 'futur--pending
                 (if pending (concat pending string) string))))

(defun futur--elisp-process-sentinel (proc status)
  (let* ((proclist (assq (process-get proc 'futur--kind)
                         futur--elisp-servers)))
    (cl-assert (memq proc (cdr proclist)))
    (if (not (futur--process-completed-p proc))
        (message "futur--elisp-process-sentinel before end: %S" status)
      (cl-callf (lambda (ps) (delq proc ps)) (cdr proclist))
      (let ((futur (process-get proc 'futur--destination)))
        (when futur
          (process-put proc 'futur--destination nil)
          (futur-deliver-failure futur (list 'error "Futur-server died")))))
    nil))

(defun futur--elisp-launch (kind &optional prefix)
  (let* ((buffer (get-buffer-create (format" *%s*" kind)))
         (stderr (make-pipe-process
                  :name (format "%s-stderr" kind)
                  :noquery t
                  :coding 'emacs-internal
                  :buffer buffer
                  :filter #'futur--elisp-process-filter-stderr
                  :sentinel #'ignore))
         (proc (make-process
                :name (symbol-name kind)
                :noquery t
                :buffer buffer
                :connection-type 'pipe
                :coding 'emacs-internal
                :stderr stderr
                :filter #'futur--elisp-process-filter
                :sentinel #'futur--elisp-process-sentinel
                :command
                `(,@prefix
                  ,(expand-file-name invocation-name invocation-directory)
                  "-Q" "--batch"
                  "-l" ,(locate-library "futur-server")
                  "-f" "futur-server"))))
    (process-put stderr 'futur--parent proc)
    (process-put proc 'futur--kind kind)
    (process-put proc 'futur--state :booting)
    (process-put proc 'futur--rid 0)
    (process-put proc 'futur--last-time (float-time))
    (push proc (alist-get kind futur--elisp-servers))
    (unless futur--elisp-servers-timer
      (setq futur--elisp-servers-timer
            (run-with-timer futur--elisp-servers-delay
                            futur--elisp-servers-delay
                            #'futur--elisp-reap-idle-servers)))
    proc))

(defun futur--elisp-reap-idle-servers ()
  (let ((time (float-time))
        (left nil))
    (pcase-dolist (`(,_kind . ,procs) futur--elisp-servers)
      (dolist (proc procs)
        (if (> (- time (process-get proc 'futur--last-time))
               futur--elisp-servers-delay)
            ;; No activity in during more than `futur--elisp-servers-delay'.
            ;; FIXME: Maybe we should use different delays for the case
            ;; where the server is really idle, or the case where we're
            ;; waiting for an answer?
            (delete-process proc)
          (setq left t))))
    (unless left
      (cancel-timer futur--elisp-servers-timer)
      (setq futur--elisp-servers-timer nil))))

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
      (futur-deliver-value futur sexp))
    nil))

(defun futur--elisp-set-destination (proc futur)
  (cl-assert (null (process-get proc 'futur--destination)) nil
             "Pre-existing destination %S vs new %S"
             (process-get proc 'futur--destination) futur)
  (let ((answers (process-get proc 'futur--answers)))
    (if answers
        (let ((answer (car answers)))
          (process-put proc 'futur--answers (cdr answers))
          (futur-deliver-value futur answer)
          nil)
      (process-put proc 'futur--destination futur)
      `(futur-server . ,proc))))

(defun futur--elisp-answer-futur (proc)
  (futur-new (lambda (futur)
               (futur--elisp-set-destination proc futur))))

(defun futur--elisp-get-process (kind launcher)
  (let ((ready (seq-find (lambda (proc) (process-get proc 'futur--ready))
                         (alist-get kind futur--elisp-servers))))
    (if ready (futur-done ready)
      (futur-let*
          ((proc (funcall launcher kind))
           (answer <- (futur--elisp-answer-futur proc)))
        (if (eq answer :ready)
            (progn
              (process-put proc 'futur--ready t)
              proc)
          (error "unexpected boot message from futur-server: %S" answer))))))

(cl-defmethod futur-blocker-abort ((blocker (head futur-server)) _)
  ;; Don't kill the server, since we may want to reuse it for other
  ;; requests.
  (let ((proc (cdr blocker)))
    ;; FIXME: This USR1 hack doesn't work in Windows and Android.
    (signal-process proc 'USR1)))

(cl-defmethod futur-blocker-wait ((_blocker (head futur-server)))
  ;; FIXME: (while ?? (accept-process-output proc ...))!
  nil)

(defun futur--elisp-funcall-1 (futur-proc func args)
  (futur-let*
      ((proc <- futur-proc)
       (rid
        ;; FIXME: In Emacs<31, cl-incf on process-get doesn't return
        ;; the expected value.
        ;; (cl-incf (process-get proc 'futur--rid))
        (progn
          (cl-incf (process-get proc 'futur--rid))
          (process-get proc 'futur--rid)))
       (_ (with-temp-buffer
            ;; (trace-values :funcall rid func args)
            (process-put proc 'futur--ready nil)
            (process-put proc 'futur--last-time (float-time))
            (let ((print-length nil)
                  (print-level nil)
                  (coding-system-for-write 'emacs-internal)
                  (print-circle t)
                  (print-gensym t)
                  ;; The server reads with `read-from-minibuffer' which
                  ;; works only on single-lines, so it's super-important
                  ;; we don't include any LF by accident.
                  (print-escape-newlines t)
                  ;; Not only LF but also CR terminates the single line :-(
                  (print-escape-control-characters t)
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
    (pcase read-answer
      (`(:read-success ,(pred (equal rid)))
       (futur-let* ((call-answer  <- (futur--elisp-answer-futur proc)))
         (pcase-exhaustive call-answer
           (`(:funcall-success ,(pred (equal rid)) . ,val)
            (process-put proc 'futur--ready t)
            (process-put proc 'futur--last-time (float-time))
            val)
           (`(:funcall-error ,(pred (equal rid)) . ,err)
            (process-put proc 'futur--ready t)
            (process-put proc 'futur--last-time (float-time))
            (futur--resignal err)))))
      (`(:read-success . ,_)
       ;; (futur--funcall #'futur--client-resync proc)
       (error "Out-of-order reply: %S" read-answer))
      (_
       ;; (futur--funcall #'futur--client-resync proc)
       (error "futur-server error: %S" read-answer)))))

(defun futur--elisp-funcall (func &rest args)
  (futur--elisp-funcall-1
   (futur--elisp-get-process 'futur-server #'futur--elisp-launch)
   func args))

;;;; Running in a sandbox

;; Inspired by the code in `elpa-admin.el'.

(defconst futur--sandbox-bwrap-args
  '("--unshare-all"
    "--dev" "/dev"
    "--proc" "/proc"
    "--tmpfs" "/tmp"))

(defvar futur--sandbox-ro-dirs
  '("/lib" "/lib64" "/bin" "/usr" "/etc/alternatives" "/etc/emacs" "/gnu" "~/"))

(defvar futur-sandbox-temp-dir (make-temp-file "futur-sandbox" 'dir)
  "Directory to pass temporary files to the sandbox.
Contrary to /tmp, this directory is readable by the sandboxed processes.")

(defun futur--sandbox-launch (kind)
  ;; Don't inherit MAKEFLAGS from any surrounding make process,
  ;; nor TMP/TMPDIR since the container uses its own tmp dir.
  (let ((process-environment `("MAKEFLAGS" "TMP" "TMPDIR"
                               ,@process-environment)))
    (futur--elisp-launch
     kind `("bwrap"
            ,@futur--sandbox-bwrap-args
            ,@(mapcan (lambda (dir)
                        (when (file-exists-p dir)
                          (let ((dir (expand-file-name dir)))
                            `("--ro-bind" ,dir ,dir))))
                      (append futur--sandbox-ro-dirs
                              (list futur-sandbox-temp-dir)))))))

(defun futur--sandbox-funcall (func &rest args)
  (futur--elisp-funcall-1
   (futur--elisp-get-process 'futur-sandbox #'futur--sandbox-launch)
   func args))

(defun futur--elisp-kill-subprocesses ()
  ;; FIXME: Add to `futur-server' a timer to auto-exit after a while.
  (pcase-dolist (`(_kind . ,procs) futur--elisp-servers)
    (mapc #'delete-process procs))
  (delete-directory futur-sandbox-temp-dir 'recursive))

(add-hook 'kill-emacs-hook #'futur--elisp-kill-subprocesses)

(provide 'futur-client)
;;; futur-client.el ends here
