;;; gnosis-test-model-preparation-process.el --- Child failure and retry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Exercise the real preparation pipe, sentinel, parser and deferred delivery.
;; Only the first child's expression is fault-injected; retry uses the normal
;; child and tiny managed geometry.  The downstream canvas and recursive input
;; use the existing batch fixture, not a renderer or an interactive UI proof.

;;; Code:

(require 'gnosis-model-test-support)

(defvar canvas-3d--process)

(defun gnosis-test-preparation--wait (predicate)
  "Pump process output until PREDICATE succeeds, with a bounded deadline."
  (let ((deadline (+ (float-time) 15)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(defun gnosis-test-preparation--snapshot ()
  "Return queue, durable checkpoint and study evidence for this fixture."
  (copy-tree
   (list (gnosis-review--state-data gnosis-review--state)
         (gnosis-review--state-data (gnosis-review--read-session))
         (gnosis-select '* 'scheduler-state)
         (gnosis-select '* 'review-events)
         (gnosis-select '* 'practice-events))))

(defun gnosis-test-preparation--retired (job)
  "Assert that JOB owns no live child, buffers or scheduled delivery."
  (should job)
  (should (plist-get job :cancelled))
  (should-not (process-live-p (plist-get job :process)))
  (should-not (buffer-live-p (plist-get job :output)))
  (should-not (buffer-live-p (plist-get job :errors)))
  (should-not (memq (plist-get job :timer) timer-list)))

(defun gnosis-test-preparation--expression (fault expression)
  "Return child EXPRESSION with the requested FAULT, or unchanged."
  (pcase fault
    ;; Emit valid fields first: a nonzero status must dominate valid output.
    ('nonzero `(progn ,expression (kill-emacs 23)))
    ('signal `(progn ,expression (signal-process (emacs-pid) 'SIGKILL)))
    ('malformed '(princ "("))
    ('trailing '(princ "(:fields nil) trailing"))
    ('absent '(kill-emacs 0))
    ('empty '(princ "nil"))
    (_ expression)))

(defun gnosis-test-preparation--retry (fault mode)
  "Fail or cancel one preparation using FAULT, then retry in MODE.
The queued case holds a real successful delivery timer until cancellation;
all other cases let the native sentinel and timer deliver the child failure."
  (gnosis-test-with-db
    (save-window-excursion
      (let* ((model (gnosis-test-model--add))
             (next (gnosis-test--add-basic-thema "Following card" "Untouched"))
             (gnosis-review-buffer-name " *Gnosis preparation process test*")
             (buffer (gnosis-review--setup-buffer (list model next) mode))
             (make-child (symbol-function 'make-process))
             (prepare (symbol-function 'gnosis-model-prepare))
             (schedule (symbol-function 'run-at-time))
             (depth 0) (starts 0) (opens 0)
             jobs deliveries context pending)
        (unwind-protect
            (with-current-buffer buffer
              (setf (gnosis-review-state-persistent-p gnosis-review--state) t)
              (gnosis-review--save-session gnosis-review--state)
              (set-window-buffer (selected-window) buffer)
              (let ((gnosis-review--running
                     (gnosis-review-state-session-id gnosis-review--state))
                    (before (gnosis-test-preparation--snapshot))
                    (scheduler (gnosis-select '* 'scheduler-state))
                    (map (current-local-map))
                    (header header-line-format))
                (cl-letf
                    (((symbol-function 'make-process)
                      (lambda (&rest args)
                        (when (equal (plist-get args :name) "gnosis-model-prepare")
                          (cl-incf starts)
                          (when (= starts 1)
                            (let* ((command (plist-get args :command))
                                   (expression (read (car (last command)))))
                              (setq args
                                    (plist-put
                                     args :command
                                     (append (butlast command)
                                             (list
                                              (gnosis-sqlite--serialize
                                               (gnosis-test-preparation--expression
                                                fault expression)))))))))
                        (apply make-child args)))
                     ((symbol-function 'gnosis-model-prepare)
                      (lambda (type hypothesis answer callback)
                        (let ((job (funcall prepare type hypothesis answer
                                            (lambda (fields failure)
                                              ;; Record outside callback assertions:
                                              ;; owner delivery contains errors.
                                              (push (list fields failure) deliveries)
                                              (funcall callback fields failure)))))
                          (push job jobs)
                          job)))
                     ((symbol-function 'run-at-time)
                      (lambda (time repeat function &rest args)
                        ;; Hold only the first transport delivery, not its
                        ;; sentinel, pipe output or any foreground retry.
                        (apply schedule
                               (if (and (eq fault 'queued) (= starts 1)
                                        (eq function #'gnosis-model--deliver-preparation))
                                   3600 time)
                               repeat function args)))
                     ((symbol-function 'gnosis-model-open)
                      (lambda (&rest args)
                        (cl-incf opens)
                        (apply #'gnosis-test-model--canvas args)))
                     ((symbol-function 'gnosis-model--canvas-size) (lambda () 400))
                     ((symbol-function 'recursion-depth) (lambda () depth))
                     ((symbol-function 'exit-recursive-edit) #'ignore)
                     ((symbol-function 'abort-recursive-edit)
                      (lambda () (signal 'quit nil)))
                     ((symbol-function 'gnosis-review-actions)
                      (lambda (success id result)
                        (setq pending (cons success result))
                        (gnosis-review--accept id success result)))
                     ((symbol-function 'recursive-edit)
                      (lambda ()
                        (setq depth 1 context gnosis-review--model-context)
                        (let* ((job (plist-get context :preparation))
                               (process (plist-get job :process)))
                          (should (integerp (process-id process)))
                          (if (= starts 1)
                              (progn
                                (gnosis-test-preparation--wait
                                 (lambda ()
                                   (if (eq fault 'queued)
                                       (timerp (plist-get job :timer))
                                     (or (plist-get context :error)
                                         (plist-get context :fields)))))
                                (should (timerp (plist-get job :timer)))
                                (should (eq (process-status process)
                                            (if (eq fault 'signal) 'signal 'exit)))
                                (if (eq fault 'queued)
                                    (progn
                                      (should (= 0 (process-exit-status process)))
                                      (with-current-buffer (plist-get job :output)
                                        (should (plist-get (read (buffer-string)) :fields)))
                                      (should-not deliveries))
                                  (should (= 1 (length deliveries)))
                                  (should-not (caar deliveries))
                                  (should (equal (cadar deliveries) (plist-get context :error)))
                                  (should
                                   (string-match-p
                                    (pcase fault
                                      ((or 'nonzero 'signal) "Model preparation process failed")
                                      ('trailing "Invalid model preparation response")
                                      ('empty "Empty model preparation response")
                                      (_ "End of file"))
                                    (plist-get context :error)))
                                  (when (eq fault 'nonzero)
                                    (should (= 23 (process-exit-status process))))
                                  (gnosis-test-preparation--retired job))
                                (should (= 0 opens))
                                (should-not (plist-get context :fields))
                                (should-not (plist-get context :result))
                                (should-error (call-interactively (key-binding (kbd "RET")))
                                              :type 'user-error)
                                (should (equal before (gnosis-test-preparation--snapshot)))
                                (call-interactively (key-binding (kbd "q"))))
                            (let* ((old (cadr jobs))
                                   (timer (plist-get old :timer)))
                              (should-not (eq old job))
                              ;; Force even an already queued old delivery to
                              ;; run while the fresh encounter owns the buffer.
                              (apply (timer--function timer) (timer--args timer))
                              (should (eq context gnosis-review--model-context))
                              (should-not (plist-get context :fields))
                              (should-not (plist-get context :error))
                              (gnosis-test-model--wait-for-preparation)
                              (should (plist-get context :fields))
                              (should-not (plist-get context :error))
                              (should (= 1 opens))
                              (should (equal before (gnosis-test-preparation--snapshot)))
                              (gnosis-test-model--review-pick "triangle" canvas-3d--process)
                              (call-interactively (key-binding (kbd "RET")))
                              (should (car (plist-get context :result)))
                              (should-error (gnosis-review-model-submit) :type 'user-error)))))))
                  (should (condition-case nil
                              (progn (gnosis-review-process-thema model gnosis-review--state) nil)
                            (quit t)))
                  (should-not gnosis-review--model-context)
                  (should-not pending)
                  (should (eq map (current-local-map)))
                  (should (equal header header-line-format))
                  (should (equal before (gnosis-test-preparation--snapshot)))
                  (gnosis-test-preparation--retired (car jobs))
                  (setq depth 0)
                  (gnosis-review-process-thema model gnosis-review--state)
                  (should (= 2 starts))
                  (should (= (length deliveries) (if (eq fault 'queued) 1 2)))
                  (should (caar deliveries))
                  (should-not (cadar deliveries))
                  (should pending)
                  (should (car pending))
                  (should (equal (list next) (gnosis-review-state-remaining gnosis-review--state)))
                  (should (= 1 (gnosis-review-state-reviewed gnosis-review--state)))
                  (should (= 1 (length (gnosis-select '* (if (eq mode 'practice)
                                                           'practice-events 'review-events)))))
                  (should-not (gnosis-select '* (if (eq mode 'practice)
                                                   'review-events 'practice-events)))
                  (if (eq mode 'practice)
                      (should (equal scheduler (gnosis-select '* 'scheduler-state)))
                    (should-not (equal scheduler (gnosis-select '* 'scheduler-state))))
                  (let ((accepted (gnosis-test-preparation--snapshot))
                        (timer (plist-get (cadr jobs) :timer)))
                    (gnosis-review-result model (car pending) (cdr pending))
                    (apply (timer--function timer) (timer--args timer))
                    (should (equal accepted (gnosis-test-preparation--snapshot)))
                    (should (= 1 opens))
                    (should (= (length deliveries) (if (eq fault 'queued) 1 2))))
                  (should-not gnosis-review--model-context)
                  (mapc #'gnosis-test-preparation--retired jobs))))
          (mapc #'gnosis-model-cancel-preparation jobs)
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest gnosis-model-preparation-process-failure-retry ()
  (dolist (mode '(practice due))
    (dolist (fault '(nonzero malformed trailing absent empty))
      (ert-info ((format "Mode %s; child fault %s" mode fault))
        (gnosis-test-preparation--retry fault mode)))))

(ert-deftest gnosis-model-preparation-process-signal-retry ()
  (skip-unless (memq system-type '(gnu/linux darwin berkeley-unix)))
  (dolist (mode '(practice due))
    (ert-info ((format "Mode %s; child killed by signal" mode))
      (gnosis-test-preparation--retry 'signal mode))))

(ert-deftest gnosis-model-preparation-process-cancel-queued-success-retry ()
  (dolist (mode '(practice due))
    (ert-info ((format "Mode %s; successful child delivery after cancel" mode))
      (gnosis-test-preparation--retry 'queued mode))))

(provide 'gnosis-test-model-preparation-process)
;;; gnosis-test-model-preparation-process.el ends here
