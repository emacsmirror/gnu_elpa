;;; futur.el --- Future/promise-based async library  -*- lexical-binding: t -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 1.3
;; Keywords: concurrency, async, promises, futures

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

;; A library to try and make async programming a bit easier.
;; This is inspired from Javscript's async/await, Haskell's monads,
;; and ConcurrentML's events.

;; You can create trivial futures with `futur-done'.
;; You can create a "process future" with `futur-process-call'.
;; And the main way to use futures is to compose them with `futur-let*',
;; which can be used as follows:

;;     (futur-let*
;;         ((buf (current-buffer))
;;          (exitcode1 <- (futur-process-call CMD1 nil buf nil ARG1 ARG2))
;;          (out (with-current-buffer buf
;;                 (buffer-string)))  ;; Get the process's output.
;;          (exitcode2 <- (futur-process-call CMD2 nil buf nil ARG3 ARG4)))
;;       (with-current-buffer buf
;;         (concat out (buffer-string))))

;; This example builds a future which runs two commands in sequence.
;; For those rare cases where you really do need to block everything
;; else and wait for a future to complete, you can
;; use`futur-blocking-wait-to-get-result'.

;;;; Low level API

;; - (futur-done VAL): Create a trivial future returning VAL.
;; - (futur-failed ERR): Create a trivial failed future.
;; - (futur-new FUN): Create a non-trivial future.
;;   FUN is called with one argument (the new `futur' object) and should
;;   return the "blocker" that `futur' is waiting for (used mostly
;;   when aborting a future).
;; - (futur-abort FUTUR REASON): Aborts execution of FUTUR.
;; - (futur-deliver-value FUTUR VAL): Mark FUTUR as having completed
;;   successfully with VAL, and runs the clients waiting for that event.
;; - (futur-deliver-failure FUTUR ERROR): Mark FUTUR as having failed
;;   with ERROR, and runs the clients waiting for that event.
;; - (futur-blocking-wait-to-get-result FUTUR): Busy-wait for FUTUR to complete
;;   and return its value.  Better use `futur-bind' or `futur-let*' instead.
;;   BEWARE: Please don't use it unless you really absolutely have to.

;;;; Composing futures

;; - (futur-bind FUTUR FUN &optional ERROR-FUN): Builds a new future which
;;   waits for FUTUR to completes and then calls FUN (or ERROR-FUN) with the
;;   resulting value (or its error).  (ERROR-)FUN should itself return
;;   a future, tho if it doesn't it's automatically turned into a trivial one.
;; - (futur-let* BINDINGS [:error-fun ERROR-FUN] BODY): Macro built on top
;;   of `futur-bind' which runs BINDINGS in sequence and then runs BODY.
;;   Each BINDING can be either a simple (PAT EXP) that is executed
;;   as in a `pcase-let*' or a (PAT <- FUTUR) in which case the rest is
;;   delayed until FUTUR completes.
;; - (futur-list &rest FUTURS): Run FUTURS concurrently and return the
;;   resulting list of values.
;; - (futur-race &rest FUTURS): Run FUTURS concurrently, return the
;;   first result, and discard the rest.

;;;; Predefined future constructors

;; - (futur-funcall FUNC &rest ARGS)
;;   Like `funcall', but runs the code asynchronously.
;; - (futur-timeout TIME)
;; - (futur-sit-for TIME)
;; - (futur-process-call PROG &optional INFILE DESTINATION _DISPLAY &rest ARGS)
;;   Like `call-process' but asynchronous, thus allows parallelism between
;;   Emacs and the subprocess.
;; - (futur-with-temp-buffer &rest BODY)
;; - (futur-unwind-protect FORM &rest FORMS)
;; - (futur-concurrency-bound FUNC &rest ARGS)
;;   Like `futur-funcall' but throttles execution to avoid running too
;;   many tasks concurrently.

;;;; Experimental

;; - (futur-hacks-mode &optional ARG)
;;   Minor mode making various Emacs features use futures.
;; - (futur--elisp-funcall FUNC &rest ARGS)
;;   Like `futur-funcall' but runs the code in parallel in a subprocess.
;; - (futur--sandbox-funcall FUNC &rest ARGS)
;;   Like `futur--elisp-funcall' but runs the code in a sandbox so it
;;   can be used with untrusted code.

;;;; Related packages

;; - [deferred](https://melpa.org/#/deferred): Provides similar functionality.
;;   Maybe the only reason `futur.el' exists is because `deferred' is different
;;   from what I expected (NIH syndrome?).
;; - [promise](https://melpa.org/#/promise) is a very similar library,
;;   which tries to stay as close as possible to JavaScript's promises,
;;   leading to a very non-idiomatic implementation in `promise-core.el'.
;;   TODO: We only provide the core functionality of `promise', currently
;;   and it would make sense to add most of the rest, or even provide
;;   a bridge between the two.
;; - [pfuture](https://melpa.org/#/pfuture): Sounds similar, but is more
;;   of a wrapper around `make-process'.  Compared to this package,
;;   `pfuture' does not try very hard to help compose async computations
;;   and to propagate errors.
;; - [async](http://elpa.gnu.org/packages/async.html): A package that focuses
;;   on executing ELisp code concurrently by launching additional Emacs
;;   (batch) sessions.
;;   TODO: It would make a lot of sense to allow use of `async'
;;   via `futur' objects.
;; - [async-await](https://melpa.org/#/async-await): This provides
;;   JavaScript-style async/await operators on top of the `promise' package.
;;   This fundamentally require a kind of CPS conversion of the code, for
;;   which they use `generator.el'.
;;   TODO: It would be possible to make `async-await' work on top of `futur',
;;   but to the extent that `generator.el' is not able to perform CPS
;;   correctly in all cases (because it's hard/impossible in general),
;;   I'm not sure it's a good idea to encourage this coding style.
;;   Maybe instead we should develop some way to detect&flag most of the
;;   pitfalls of the current style (such as using `progn' instead of
;;   `future-let*' to sequence execution when one part is a future).
;; - [aio](https://melpa.org/#/aio): Also provides await/async style
;;   coding (also using `generator.el' under the hood) but using its
;;   own "promise" objects, which are much simpler than those of `promise.el'.
;; - [async1](https://melpa.org/#/async1): A more limited/ad-hoc solution to
;;   the problem that async/await try to solve that hence avoids the need
;;   to perform CPS.  Not sure if it's significantly better than `futur-let*'.
;; - [asyncloop](https://melpa.org/#/asyncloop): Focuses on just
;;   running a sequence of function calls with regular "stops" in-between
;;   to let other operations happen "concurrently".
;; - [async-job-queue](https://melpa.org/#/async-job-queue):
;; - [pdd](https://melpa.org/#/pdd): HTTP library that uses its own
;;   implementation of promises.
;; - el-job: Library to run ELisp jobs in parallel in Emacs subprocesses.
;;   `futur-client/server.el' took some inspiration from that package.

;;;; BUGS

;; - There might still be cases where we run code sometimes in the
;;   "current" dynamic context and sometimes in the background thread.
;; - Sometimes the `futur--background' thread gets blocked on some
;;   operation (e.g. entering the debugger), which blocks all further
;;   execution of async tasks.
;; - When launching elisp/sandbox servers (or during `futur-reset-context'),
;;   the client receives and displays all the `message's from the subprocess,
;;   which can be annoying more than helpful.
;; - When using `futur-hacks-mode' I sometimes see void-variable errors
;;   about `cl-struct-eieio--class-tags' which sound like problems in
;;   `futur--obarray-snapshot/revert'.  I have not investigated them yet,
;;   but maybe this idea of obarray snapshots can't work reliably.

;;; News:

;; Version 1.3:

;; - Syntax of `:error-fun' changed in `futur-let*'.
;; - Remove `idle' argument from `futur-timeout'.
;; - Adjusted `futur-hacks-mode' to changes in Emacs `master'.

;; Version 1.2:

;; - `futur-abort' takes a second argument (the reason for the abortion).
;; - New function `futur-funcall'.
;; - `futur-bind' and `futur-blocking-wait-to-get-result' can now select
;;   which errors they catch.
;; - New function `futur-p'.
;; - Preliminary support to run ELisp code in subproceses&sandboxes.
;; - Experimental `futur-hacks-mode' using the preliminary sandbox code.
;; - New var `futur-use-threads' to be able to force the use of timers.

;; Version 1.1:

;; - New functions: `futur-race', `futur-sit-for', `futur-url-retrieve'.
;; - New function `futur-concurrency-bound' when you need to limit concurrency.
;; - Rename `futur-error' to `futur-failed'.
;; - Rename `futur-register-callback' to `futur--register-callback'.
;; - Rename `futur-ize' to `futur--ize'.
;; - Fix compatibility with Emacs<31.
;; - Minor bug fixes.

;; Version 1.0:

;; - After years of sitting in the dark, it's finally getting dusted up for
;;   a release.

;;; Code:

;; TODO:
;; - Handle exceptions.

(require 'cl-lib)

(cl-defstruct (futur--queue
               (:conc-name futur--queue-)
               (:constructor nil)
               (:constructor futur--queue ()
                "Return a new empty `futur--queue'."))
  head revtail)

(defun futur--queue-enqueue (queue val)
  "Add VAL to the end of the QUEUE."
  (push val (futur--queue-revtail queue)))

(defun futur--queue-empty-p (queue)
  "Return non-nil if and only if the QUEUE is empty."
  (and (null (futur--queue-head queue))
       (let ((tail (futur--queue-revtail queue)))
         (if tail
             (progn
               (setf (futur--queue-head queue) (nreverse tail))
               (setf (futur--queue-revtail queue) nil)
               nil)
           t))))

(defun futur--queue-dequeue (queue)
  "Remove the first element from the QUEUE and return it.
It's an error to use it without checking first with `futur--queue-empty-p'
that it is not empty."
  (let ((h (futur--queue-head queue)))
    (cl-assert h)
    (setf (futur--queue-head queue) (cdr h))
    (car h)))

(defun futur--queue-requeue (queue val)
  "Push VAL back to the head of the QUEUE."
  (push val (futur--queue-head queue)))

(defvar futur--pending (futur--queue)
  "Pending operations.")

(defconst futur--pending-mutex (make-mutex "futur-pending"))
(defconst futur--pending-condvar
  (make-condition-variable futur--pending-mutex))

(eval-and-compile
  (unless (fboundp 'with-suppressed-warnings) ;Emacs-27
    (defmacro with-suppressed-warnings (_warnings &rest body)
      (with-no-warnings ,@body))))

(defun futur--background ()
  (let ((inhibit-quit t)
        ;; Entering the debugger blocks all subsequent pending tasks,
        ;; plus it bumps into problems like bug#80537.
        ;; (debug-on-error nil)
        )
    (while t
      (let ((pending
             (with-mutex futur--pending-mutex
               (while (futur--queue-empty-p futur--pending)
                 (condition-wait futur--pending-condvar))
               (futur--queue-dequeue futur--pending))))
        (with-demoted-errors "future--background: %S"
          (apply pending))))))

(defun futur--make-thread (f name)
  (condition-case nil
      (with-suppressed-warnings ((callargs make-thread))
        (make-thread f name 'silently))
    (wrong-number-of-arguments ;; Emacs<31
     (with-current-buffer (get-buffer-create " *futur--background*")
       (make-thread f name)))))

(defvar futur-use-threads (fboundp 'make-thread) ;New in Emacs-26
  "If non-nil, futur will use timers for background tasks instead.
Threads have the advantage that we can make sure background tasks are
run in a clean dynamic environment, whereas timers are run in the dynamic
environment of the current waiting code, which can include all kinds of
undesirable let-bindings.
But Emacs's threads still suffer from several implementation warts
and limitations.")

(defconst futur--background
  (when futur-use-threads
    (futur--make-thread #'futur--background "futur--background")))

(defun futur--funcall (&rest args)
  "Call ARGS like `funcall' but outside of the current dynamic scope.
The code is conceptually run in another thread and while we try to run as
soon as possible, and fairly, we do not guarantee the specific
time or order of execution."
  (if (not futur--background)
      (apply #'run-with-timer 0 nil args)
    (with-mutex futur--pending-mutex
      (futur--queue-enqueue futur--pending args)
      ;; FIXME: Maybe we should have a combo
      ;; `mutex-unlock+condition-notify', i.e. a variant of
      ;; `condition-notify' which doesn't regrab the lock, since it's
      ;; common to do `condition-notify' at the end of a critical section.
      (condition-notify futur--pending-condvar))))

(defvar futur--idle-loop-bug80286
  ;; "Idle loop" thread to try and make sure we run timers, filters, etc...
  ;; Seems to give me assertion errors:
  ;;
  ;;     process.c:5174: Emacs fatal error: assertion failed: XTHREAD (ps->thread) == current_thread
  ;;     [Switching to Thread 0x7fffe186d6c0 (LWP 3046715)]
  ;;     
  ;;     Thread 8 "futur-idle-loop" hit Breakpoint 1, terminate_due_to_signal (
  ;;         sig=sig@entry=6, backtrace_limit=backtrace_limit@entry=2147483647)
  ;;         at emacs.c:445
  ;;
  ;;(when futur-use-threads
  ;;  (futur--make-thread
  ;;   (lambda ()
  ;;     (while t (accept-process-output nil (* 60 60 24))))
  ;;   "futur-idle-loop"))
  nil)

;;;; The `futur' data structure

(cl-defstruct (futur
               (:conc-name futur--)
               (:noinline t)
               (:predicate futur--p)
               (:constructor nil)
               (:constructor futur--done (value &aux (clients 't))
                "Return a new `futur' that just returns VALUE.")
               (:constructor futur--failed (value &aux (clients 'error))
                "Return a new `futur' that signals error VALUE")
               (:constructor futur--waiting (&optional blocker clients
                                             &aux (value blocker))
                "Return a new `futur' that's waiting for BLOCKER."))
  "A promise/future.
A futur has 3 possible states:
- (futur-done VAL): in that state, `clients' is `t', and `value' holds VAL.
- (futur-failed ERR): in that state, `clients' is `error', and `value' holds ERR.
- (futur-waiting BLOCKER CLIENTS): in that state, `clients' is a list
  of \"callbacks\" of the form (FUTUR . FUN) meaning that FUTUR is waiting
  to receive the ERR and VAL via FUN, and `value' holds
  the BLOCKER that will deliver the value (can be another future,
  a process, a thread, a list (of futures), or possibly other objects
  with `futur-blocker-wait/abort' methods)."
  (clients nil)
  (value nil))

(pcase-defmacro futur--done (result)
  `(and (pred futur--p)
        (app futur--clients 't)
        (app futur--value ,result)))

(pcase-defmacro futur--failed (error-object)
  `(and (pred futur--p)
        (app futur--clients 'error)
        (app futur--value ,error-object)))

(pcase-defmacro futur--waiting (&optional blocker clients)
  `(and (pred futur--p)
        (app futur--clients (and (pred listp) ,(or clients '_)))
        (app futur--value ,(or blocker '_))))

(defun futur--waiting-p (futur)
  (pcase futur ((or (futur--waiting)
                    ;; Tell Pcase to presume FUTUR *is* a futur.
                    (and (pred (not futur--p)) pcase--dontcare))
                t)))

(define-inline futur--blocker (futur)
  "Pseudo-slot for a waiting FUTUR."
  (inline-letevals (futur)
    (inline-quote (progn (cl-assert (futur--waiting-p ,futur))
                         (futur--value ,futur)))))

(defun futur--deliver (futur err val)
  (pcase-exhaustive futur
    ((futur--waiting _ clients)
     (setf (futur--clients futur) (if err 'error t))
     (setf (futur--value futur) (or err val))
     (if (null clients) ;; FIXME: They may also all be unwinds!
         (when err
           ;; Don't let errors disappear silently.
           (message "Error in futur: %s" (error-message-string err)))
       ;; CLIENTS is usually in reverse order since we always `push' to them.
       (dolist (client (nreverse clients))
         ;; Don't run the clients directly from here, so we don't nest,
         ;; and also because we may be in an "interrupt" context where
         ;; operations like blocking could be dangerous.
         (futur--funcall (cdr client) err val))))
    ((futur--failed `(futur-aborted . ,_))
     nil)     ;; Just ignore the late delivery.
    ((pred futur--p)
     (error "Delivering a second time: %S %S %S" futur err val))))

(defun futur-p (object)
  "Return non-nil if OBJECT is a `futur'."
  (futur--p object))

(defun futur-deliver-value (futur val)
  "Announce completion of FUTUR with result VAL."
  (futur--deliver futur nil val))

(defun futur-deliver-failure (futur error)
  "Announce that computation of FUTUR encountered an ERROR."
  (futur--deliver futur error nil))

(defun futur-done (val)
  "Build a trivial `futur' which returns VAL."
  (futur--done val))

(defun futur-failed (error-object)
  "Build a trivial `futur' which just signals ERROR-OBJECT."
  (futur--failed error-object))

(defun futur-new (builder)
  "Build a future.
BUILDER is a function that will be called with one argument
\(the new `futur' object, not yet fully initialized) and it should
return the \"blocker\", i.e. the object on which the future is waiting.
The code creating this future needs to call `futur-deliver-value'
of `futur-deliver-failure' on its argument when the future has completed.
The blocker can be any object for which there are `futur-blocker-wait'
and `futur-blocker-abort' methods.  `nil' is a valid blocker."
  (let* ((f (futur--waiting))
         (x (funcall builder f)))
    (when x
      (setf (futur--blocker f) x))
    f))

(defun futur-abort (futur reason)
  "Interrupt execution of FUTUR, marking it as having failed.
The error is `futur-aborted'.  Does nothing if FUTUR was already complete."
  (pcase futur
    ((futur--waiting blocker)
     (let ((error (list 'futur-aborted reason)))
       (futur-blocker-abort blocker error futur)
       (futur-deliver-failure futur error)))
    (_ nil))) ;; No point in throwing away the result we already got.

;;;; Composing futures

(defun futur--register-callback (fun futur cfut &optional now-ok)
  ;; FIXME: Add info about the downstream future for abort's purpose.
  "Call FUN when FUTUR completes.
CFUT if non-nil is another future which is waiting for FUTUR to complete
and will receive its result via FUN.
Calls FUN with two arguments (ERR VAL), where only one of the two is non-nil,
and throws away the return value.  If FUTUR fails ERR is the error object,
otherwise ERR is nil and VAL is the result value.
When FUN is called, FUTUR is already marked as completed.
If FUTUR already completed, FUN is called immediately.
If NOW-OK is non-nil, it means that we can call FUN in the current
dynamic context, otherwise, always go through `futur--funcall'."
  ;; FIXME: Maybe we should pass CFUT to FUN?
  (pcase futur
    ((futur--waiting _ clients)
     (setf (futur--clients futur) (cons (cons cfut fun) clients)))
    ((futur--failed err) (funcall (if now-ok #'funcall #'futur--funcall)
                                  fun err nil))
    ((futur--done val) (funcall (if now-ok #'funcall #'futur--funcall)
                                fun nil val)))
  nil)

(defun futur--ize (val)
  "Make sure VAL is a `futur'.  If not, make it a trivial one that returns VAL."
  (if (futur--p val) val (futur--done val)))

(defun futur--handle-error (error-fun err runner default)
 "Select the appropriate element of ERROR-FUN for ERR.
Passes it to RUNNER (with ERR as second argument)
if found, and if no handler applies, calls DEFAULT with ERR
as argument."
  (if (functionp error-fun)
      (funcall runner error-fun err)
    (while (and error-fun
                (not (futur--error-member-p
                      err (caar error-fun))))
      (setq error-fun (cdr error-fun)))
    (if error-fun
        (funcall runner (cdar error-fun) err)
      (funcall default err))))

(defun futur-bind (futur fun &optional error-fun)
  "Build a new future by composition.
That future calls FUN with the return value of FUTUR and returns
the same value as the future returned by FUN.
By default any error in FUTUR is propagated to the returned future.
But ERROR-FUN can be used to handle errors:
- It can be a function, in which case Emacs will call it instead of
  FUN when FUTUR fails.  It receives a single argument (the error object).
- It can be a list of (CONDITION-NAME . ERR-FUN), in which case it
  behaves a bit like `condition-case' and calls the ERR-FUN of the first
  CONDITION-NAME that matches the error, passing it the error.
ERROR-FUN and FUN can also return non-future values,
FUTUR can also be a non-`futur' object, in which case it's passed
as-is to FUN.
Both FUN and ERROR-FUN are called in an empty dynamic context,
and not necessarily in the current-buffer either."
  (let ((new (futur--waiting futur)))
    (if (not (futur--p futur))
        (futur--funcall #'futur--run-continuation new fun (list futur))
      (futur--register-callback
       (lambda (err val)
         (cond
          ((null err) (futur--run-continuation new fun (list val)))
          (t
           (futur--handle-error
            error-fun err
            (lambda (error-fun err)
              (futur--run-continuation new error-fun (list err)))
            (lambda (err) (futur-deliver-failure new err))))))
       futur new))
    new))

(defun futur-funcall (func &rest args)
 "Schedule to call FUNC with ARGS, and return a future to hold the result.
The call takes place in an empty dynamic context."
  (futur-bind nil (lambda (_) (apply func args))))

(defun futur--run-continuation (futur fun args)
  ;; `futur' May have aborted between the time we scheduled
  ;; the call to `futur--run-continuation' and now.
  (when (futur--waiting-p futur)
    ;; The thing FUTUR was waiting for is completed, maybe we'll soon be waiting
    ;; for another future, but for now, there's no blocker object,
    ;; we're just waiting for some piece of ELisp (namely FUN) to terminate.
    (setf (futur--blocker futur) nil)
    (condition-case err
        (let ((res (apply fun args)))
          (if (not (futur--p res))
              (futur-deliver-value futur res)
            (when (futur--waiting-p futur) ;Check it wasn't aborted.
              (setf (futur--blocker futur) res)
              (futur--register-callback
               (lambda (err val)
                 (if err (futur-deliver-failure futur err)
                   (futur-deliver-value futur val)))
               res futur 'now-ok))))
      (t (futur-deliver-failure futur err)))))

(defun futur--resignal (error-object)
  ;; Undocumented feature of `signal', this re-signals an error using the exact
  ;; same error object:
  ;; (should (eq e1 (condition-case e2 (signal e1 nil) (error e2))))
  (signal error-object nil))

(defun futur--error-member-p (error condition)
  (or (eq condition t)
      (when (symbolp (car-safe error))
        (memq condition (get (car error) 'error-conditions)))))

(defun futur-blocking-wait-to-get-result (futur &optional error-fun)
  "Wait for FUTUR to deliver and then return its value.
Ideally, this should never be used, hence the long name to discourage
abuse.  Instead, you should use `futur-bind' or `futur-let*' to execute
what you need when FUTUR completes.
If FUTUR fails, calls ERROR-FUN with the error object and returns
its result, or (re)signals the error.  Accepts the same ERROR-FUN
as `futur-bind'."
  ;; Waiting for a task to finish has always been a PITA in ELisp,
  ;; because `sit-for/accept-process-output/sleep-for' have proved brittle
  ;; with lots of weird corner cases.  `futur-blocker-wait' does its best,
  ;; thanks to years of bug fixing, but it's still messy and brittle.
  ;; See the VCS history of `url-retrieve-synchronously' for another example.
  ;; The use of `condition-notify' should side-step this problem, except
  ;; that bug#80286 means that `condition-wait' can lock up your
  ;; Emacs session hard.
  ;; FIXME: Even `futur--idle-loop-bug80286' doesn't seem sufficient.
  (when inhibit-quit
    (error "Blocking/waiting within an asynchronous context is not supported"))
  (if (not (futur--p futur))
      futur
    (when (futur--waiting-p futur)
      (if t ;; (null futur--idle-loop-bug80286)
          ;; Create a "dummy" new future to record our interest in
          ;; the result of `futur' in case `futur' has other clients,
          ;; so an abort doesn't incorrectly abort `futur'.
          (let ((new (futur-bind futur #'identity)))
            (condition-case err
                (futur-blocker-wait futur)
              ;; FIXME: Use `handler-bind' instead of `futur--resignal'.
              (quit (futur-abort new err) (futur--resignal err))))
        (let* ((mutex (make-mutex "futur-wait"))
               (condvar (make-condition-variable mutex))
               (deliver (lambda (_) (with-mutex mutex (condition-notify condvar))))
               (new (futur-bind futur deliver deliver)))
          (condition-case err
              (with-mutex mutex (condition-wait condvar))
            ;; FIXME: Use `handler-bind' instead of `futur--resignal'.
            (quit (futur-abort new err) (futur--resignal err))))))
    (pcase-exhaustive futur
      ((futur--failed err)
       (futur--handle-error error-fun err #'funcall #'futur--resignal))
      ((futur--done val) val))))

(defmacro futur-let* (bindings &rest body)
  "Sequence asynchronous operations via futures.
BINDINGS can contain the usual (VAR EXP) bindings of `let*' but also
\(VAR <- EXP) bindings where EXP should return a future, in which case
the rest of the code is executed only once the future terminates,
binding the result in VAR.  BODY is executed at the very end and should
return a future.
BODY can start with `:error-fun ERROR-FUN' in which case errors in
the futures in BINDINGS will cause execution of ERROR-FUN instead of BODY.
ERROR-FUN can be:
- A lambda-expression taking an error descriptor as argument,
- A list of (CONDITION (VAR) BODY...) where BODY
  will be executed with VAR bound to the error descriptor if CONDITION
  is one of the error's condition names."
  ;; FIXME: ERROR-FUN shouldn't be an expression.
  (declare (indent 1) (debug ((&rest (sexp . [&or ("<-" form) (form)])) body)))
  (cl-assert lexical-binding)
  (let* ((error-fun (when (eq :error-fun (car body))
                      (prog1 (cadr body)
                        (setq body (cddr body)))))
         (error-fun-form
          (pcase-exhaustive error-fun
            ('nil nil)
            ((or `(lambda . ,_) (pred symbolp)) error-fun)
            ((pred consp)
             `(list
               ,@(mapcar (lambda (x) `(cons ',(car x) (lambda . ,(cdr x))))
                         error-fun))))))
    (named-let loop ((bindings bindings))
      (pcase-exhaustive bindings
        ('() (macroexp-progn body))
        (`((,var ,exp) . ,bindings)
         ;; FIXME: Catch errors in EXP, to run `error-fun'?
         `(pcase-let ((,var ,exp)) ,(loop bindings)))
        (`((,var <- ,exp) . ,bindings)
         ;; FIXME: Catch errors in EXP, to run `error-fun'?
         `(futur-bind ,exp
                      (pcase-lambda (,var) ,(loop bindings))
                      ,error-fun-form))))))

(defun futur--multi-clients-p (clients)
  (let ((count 0))
    (while (and clients (< count 2))
      (let ((client (pop clients)))
        (if (eq :unwind (car client)) nil
         (cl-incf count))))
    (>= count 2)))

(defun futur--unwind-protect (futur fun)
  "Make sure FUN is called, with no arguments, once FUTUR completes.
Calls it both when FUTUR succeeds and when it fails.
Unlike what happens with `unwind-protect', there is no guarantee of
exactly when FUN is called, other than not before FUTUR completes.
The return value of FUN is ignored."
  ;; FIXME: Not sure if this implementation is making enough efforts to make
  ;; sure not to forget to run FUN.  Maybe we should register FUTUR+FUN
  ;; on some global list somewhere that we can occasionally scan, in case
  ;; something happened that prevented running FUN?
  (let ((futur (futur--ize futur)))
    ;; Use `:unwind' to let `futur--multi-clients-p' know not to count
    ;; this function as a "real" client.
    (futur--register-callback (lambda (_ _) (funcall fun)) futur :unwind)
    futur))

(defmacro futur-unwind-protect (form &rest forms)
  "Run FORM, and when that completes, run FORMS.
FORM is supposed to return a `futur'.
When that future completes, run FORMS.
Returns a future that returns the same value as FORM.
Execution of FORMS is guarantee to occur after completion of FORM,
but it is not guaranteed to occur before completion of the returned future."
  (declare (indent 1) (debug t))
  `(futur--unwind-protect (futur-funcall (lambda () ,form)) (lambda () ,@forms)))

;;;; Futur blockers
;; Futur blockers are the objects over which a futur can be waiting, like
;; a process, a timer, another futur, ...
;; These need to implement `futur-blocker-wait' and `futur-blocker-abort'.

(cl-defgeneric futur-blocker-wait (_object)
  "Wait for OBJECT to complete.
OBJECT is an object some FUTUR is waiting.
Return non-nil if we successfully waited until the completion of BLOCKER."
  nil)

(cl-defmethod futur-blocker-wait ((futur futur))
  (if (not (futur--waiting-p futur))
      nil ;; FUTUR already completed.
    (let ((i 0))
      (while
          (pcase futur
            ((futur--waiting blocker)
             (if (futur-blocker-wait blocker)
                 (setq i 0)
               (let ((delay (* 0.01 (expt 1.1 i))))
                 (if (> delay 1.0)
                     (sit-for 0) ;; Just redisplay every 1s, just in case.
                   (setq i (1+ i)))
                 (accept-process-output nil delay)))
             ;; Always retry since even if `futur-blocker-wait' succeeded,
             ;; the futur might not have completed yet (and it may have
             ;; a new blocker).
             t)))
      t)))

(define-error 'futur-aborted "Future aborted: ")

(cl-defgeneric futur-blocker-abort (blocker error futur)
  "Abort processing of BLOCKER because FUTUR failed with ERROR.")

(cl-defmethod futur-blocker-abort ((blocker futur) error futur)
  (pcase blocker
    ((futur--waiting pblocker clients)
     (let ((c (assq futur clients)))
       (cl-assert c)
       (if (futur--multi-clients-p clients)
           ;; If there are more than 1 clients, presumably someone else is
           ;; still interested in FUTURs result, so we shouldn't abort it.
           (progn
             ;; But we can unregister ourselves from its callbacks.
             (setf (futur--clients blocker) (delq c clients))
             nil)
         ;; If CLIENTS has only one "real" element, it's presumably the future
         ;; we're in the process of aborting (call it CHILD), so there's
         ;; no harm in aborting FUTUR.  We should not just `futur-abort'
         ;; FUTUR because we shouldn't run CHILD's client, but we should
         ;; still run the other (auxiliary/cleanup) functions.
         (futur-blocker-abort pblocker error blocker)
         (setf (futur--clients blocker) 'error)
         (setf (futur--value blocker) error)
         (dolist (client clients)
           (when (eq (car client) :unwind)
             (futur--funcall (cdr client) error nil))))))))

(cl-defmethod futur-blocker-abort ((_ (eql nil)) _error _futur)
  ;; No blocker to abort.
  nil)

;;;; Postpone

(defvar futur--timeout-use-process nil
  "Use processes instead of timers in `futur-timeout'.
Used for testing/debugging purposes.")

(defun futur-timeout (time)
  "Return nil in TIME seconds."
  (if futur--timeout-use-process
      (futur-bind (futur-process-call "sleep" nil nil nil (format "%.3f" time))
                  #'ignore)
    (futur-new
     (lambda (futur)
       (cons 'timer ;; FIXME: Make timers proper structs instead!
             (run-with-timer time nil
                             (lambda (futur) (futur-deliver-value futur nil))
                             futur))))))

(defun futur-timeout-idle (time)
  (futur-new
   (lambda (futur)
     (let ((delivery (lambda (futur) (futur-deliver-value futur nil))))
       (if (null (current-idle-time))
           (cons 'timer (run-with-idle-timer time nil delivery futur))
         ;; FIXME: Ugly hack implementation, because we want to start
         ;; counting  from now for the current idle interval, whereas
         ;; `run-with-idle-timer' would start counting from the
         ;; beginning of the idle interval.
         (let* ((timer (run-with-timer time nil delivery futur))
                (blocker (cons 'timer timer)))
           (letrec ((switch
                     (lambda (&rest _)
                       (advice-remove 'internal-timer-start-idle switch)
                       (when (futur--waiting-p futur)
                         ;; This part is not super-elegant but it's tolerable.
                         (cancel-timer timer)
                         (setf (cdr blocker)
                               (run-with-idle-timer time nil
                                                    delivery futur))))))
             ;; FIXME: Eww!
             (advice-add 'internal-timer-start-idle :before switch))
           blocker))))))

(defun futur-sit-for (time)
  "Return a `futur' that completes after TIME or upon user-input.
Similar to `sit-for' but non-blocking.
Returns non-nil if it waited the full TIME."
  (futur-new
   (lambda (futur)
     ;; FIXME: This implementation relies on `pre-command-hook' to detect
     ;; user input, which can be "slow" (e.g. when the user types `C-x C-s'
     ;; `pre-command-hook' is run only after the `C-s').
     (letrec ((timer (run-with-timer
                      time nil
                      (lambda (futur)
                        (remove-hook 'pre-command-hook abort)
                        (futur-deliver-value futur t))
                      futur))
              (abort (lambda ()
                       (remove-hook 'pre-command-hook abort)
                       (cancel-timer timer)
                       (futur-deliver-value futur nil))))
       (add-hook 'pre-command-hook abort)
       (cons 'timer timer))))) ;; FIXME: Make timers proper structs instead!

(cl-defmethod futur-blocker-wait ((timer (head timer)))
  (setq timer (cdr timer))
  ;; No support for repeated timers (yet?).
  (cl-assert (not (timer--repeat-delay timer)))
  (if (timer--triggered timer)
      nil ;; Already completed.
    (while (not (timer--triggered timer))
      (let* ((idle (timer--idle-delay timer))
             (time (timer--time timer))
             (delay (time-to-seconds
                     (if idle time (time-subtract time (current-time))))))
        (accept-process-output nil (min 1.0 (max 0.01 delay)))
        (if (> delay 1) (sit-for 0)))) ;; Redisplay every 1s, just in case.
    t))

(cl-defmethod futur-blocker-abort ((timer (head timer)) _error _futur)
  (setq timer (cdr timer))
  ;; Older versions of Emacs signal errors if we try to cancel a timer
  ;; that's already run (or been canceled).
  (unless (timer--triggered timer) (cancel-timer timer)))

;;;; Bounding concurrent resource usage

;; This is a crude support to limit the amount of concurrency.
;;
;; We could limit it automatically within `futur-process-call'
;; and/or `url-retrieve', but in practice it can be preferable
;; to apply the limit "higher up" in the call-tree.
;; E.g. for `smerge-refine' rather than prepare right away the inputs
;; to all the `diff' processes and then bound the number of concurrent
;; processes, it can be preferable to apply the "delay until CPU is
;; available" not just to the actual process but also to the preparation
;; of the input files (e.g. so we don't uselessly prepare all those
;; input files if we end up aborting the futures before the `diff's
;; get to run).
;;
;; There's a lot more we could do, but I don't think we have enough
;; experience to be sure what we'll need, really.
;; Possible future needs:
;;
;; - Support for things like `nice' when launching processes.
;; - Distinguish different resources.  E.g. rather than just N concurrent
;;   futures, each future could specify what it consumes (as in CPU-vs-network
;;   when comparing `url-retrieve' and `call-process').
;; - Distinguish quantitative needs: the background native compiler
;;   might prefer to bound its concurrency to 50% of the available CPUs,
;;   while `diff-refine' might prefer to use more CPUs to reduce latency.

(defvar futur-concurrency-bound
  (if (fboundp 'num-processors) ;; Emacs-28
      ;; Apparently there's a max of 32 subprocesses on the w32 build,
      ;; so make sure we don't eat them all up.
      (if (eq system-type 'windows-nt)
          (max 16 (num-processors))
        (num-processors))
    2)
  "Maximum number of concurrent subprocesses.")

(defvar futur--concurrency-bound-active nil
  "List of active process-futures.")

(defvar futur--concurrency-bound-waiting (futur--queue)
  "Queue of futures waiting to start
Each element is of the form (FUTURE FUN . ARGS).")

(defun futur-concurrency-bound (func &rest args)
  "Call FUNC with ARGS while limiting the amount of concurrency.
FUNC should return a `futur'.  Returns a `futur' with the same value.
The amount of concurrently active futures is determined by the variable
`futur-concurrency-bound' and considers only those futures constructed
via the function `futur-concurrency-bound'.
FUNC is called in an empty dynamic context."
  (if (< (length futur--concurrency-bound-active) futur-concurrency-bound)
      (futur--concurrency-bound-start #'futur-funcall (cons func args))
    (let ((new (futur--waiting 'waiting)))
      (futur--queue-enqueue futur--concurrency-bound-waiting
                            `(,new ,func . ,args))
      new)))

(defun futur--concurrency-bound-start (func args)
  (let ((new (apply func args)))
    (push new futur--concurrency-bound-active)
    (futur-unwind-protect new
      (futur--concurrency-bound-next new))))

(defun futur--concurrency-bound-next (done)
  (setq futur--concurrency-bound-active
        (delq done futur--concurrency-bound-active))
  (cl-block nil
    (while (not (futur--queue-empty-p futur--concurrency-bound-waiting))
      (pcase-let ((`(,fut . ,call)
                   (futur--queue-dequeue futur--concurrency-bound-waiting)))
        (pcase fut
          ((futur--waiting)
           (let ((new (futur--concurrency-bound-start (car call) (cdr call))))
             (futur--register-callback
              (lambda (err val) (futur--deliver fut err val)) new fut 'now-ok)
             (cl-return))))))))

(cl-defmethod futur-blocker-abort ((_ (eql 'waiting)) _error _futur)
  nil)

;;;; Processes

(defun futur--process-completed-p (proc)
  (memq (process-status proc) '(exit signal closed failed)))

(defun futur--process-sentinel (proc futur)
  (when (futur--process-completed-p proc)
    (futur-deliver-value futur (process-exit-status proc))))

(defun futur--process-make (&rest args)
  "Create a process and return a future that delivers its exit code.
The ARGS are like those of `make-process' except that they can't include
`:sentinel' because that is used internally."
  (futur-new
   (lambda (f)
     (let ((p (apply #'make-process
                     :sentinel
                     (lambda (proc _state)
                       (futur--process-sentinel proc f))
                     args)))
       (when (fboundp #'set-process-thread)
         (set-process-thread p nil))
       p))))

(defun futur-process-call--filter (proc string)
  (let* ((file (process-get proc 'futur-destination)))
    (write-region string nil file 'append 'silent)))

(defun futur-send-file (proc infile)
  ;; FIXME: Make it more concurrent!
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally infile)
    (futur-process-send proc (buffer-string))))

(defun futur-process-call (program &optional infile destination _display
                                   &rest args)
  "Like `call-process' but runs concurrently as a `futur'.
The DISPLAY argument is ignored: redisplay always happens."
  (when (eq t (car-safe (cdr-safe destination)))
    (setq destination (car destination)))
  (pcase destination
    (0 (error "A value of 0 is not supported for DESTINATION"))
    ('t (setq destination (current-buffer)))
    ((pred stringp) (setq destination (get-buffer-create destination)))
    (`(:file ,(and file (pred stringp)))
     (setq destination (expand-file-name file)))
    (`(,_ . ,_) (error "Separate handling of stderr is not supported yet")))
  (let* ((futur (futur--process-make
                 :name program
                 :command (cons program args)
                 :coding (if (stringp destination) '(binary . nil))
                 :connection-type 'pipe
                 :buffer (if (bufferp destination) destination)
                 :filter (if (bufferp destination) nil
                           #'futur-process-call--filter)))
         (proc (pcase-exhaustive futur ((futur--waiting blocker) blocker))))
    (when (stringp destination)
      (write-region "" nil destination nil 'silent))
    (pcase-exhaustive infile
      ;; FIXME: Not sure how this can happen, but I got backtraces like:
      ;;
      ;;     Debugger entered--Lisp error: (error "Process diff not running: exited abnormally with code 1\n")
      ;;       process-send-eof(#<process diff>)
      ;;       futur-process-call("diff" nil t nil "-ad" "/tmp/diff1UHFpgS" "/tmp/diff2k4UPTf")
      ;;
      ;; So let's double-check that `proc' is still alive before
      ;; sending EOF, tho I'm not sure it's sufficient.
      ('nil (when (process-live-p proc)
              (process-send-eof proc)))
      ((pred stringp) (futur-send-file proc infile)))
    (process-put proc 'futur-destination destination)
    futur))

(defun futur-process-exit-status (proc)
  "Create a future that returns the exit code of the process PROC."
  (if (memq (process-status proc) '(exit signal closed failed))
      (futur-done (process-exit-status proc))
    (futur-new
     (lambda (f)
       ;; FIXME: If the process's sentinel signals an error, it won't run us :-(
       (add-function :after (process-sentinel proc)
                     (lambda (proc _state)
                       (futur--process-sentinel proc f)))
       proc))))

(cl-defmethod futur-blocker-wait ((proc process))
  (if (futur--process-completed-p proc)
      nil
    (while (and
            (not (futur--process-completed-p proc))
            (accept-process-output proc 1.0))
      (sit-for 0)) ;; Redisplay every 1s, just in case.
    t))

(cl-defmethod futur-blocker-abort ((proc process) _error _futur)
  (delete-process proc))

(defun futur-process-send (proc string)
  ;; FIXME: This is quite inefficient.  Our C code should instead provide
  ;; a non-blocking `(process-send-string PROC STRING CALLBACK)'.
  (futur-new
   (lambda (f) (futur--make-thread
           (lambda () (futur-deliver-value f (process-send-string proc string)))
           "futur-process-send"))))

(cl-defmethod futur-blocker-wait ((th thread))
  (if (not (thread-live-p th))
      nil
    (thread-join th)
    t))

(cl-defmethod futur-blocker-abort ((th thread) error _futur)
  ;; FIXME: This doesn't guarantee that the thread is aborted.
  ;; FIXME: Let's hope that the undocumented feature of `signal' applies
  ;; also to `thread-signal'.
  (when (thread-live-p th) (thread-signal th error nil)))

;;;; Multi futures: Futures that are waiting for several other futures.

(defun futur-list (&rest futurs)
  "Build a futur that returns the list of values of FUTURS.
If one of FUTURS fails, fails the whole future and aborts those FUTURS
that have not yet completed."
  (if (null futurs)
      (futur-done nil)
    (let* ((new (futur--waiting futurs))
           (count (length futurs))
           (failed nil)
           (args (make-list count :futur--waiting-for-result))
           (i 0))
     (dolist (futur futurs)
       (futur--register-callback
        (let ((cell (nthcdr i args)))
          (lambda (err val)
            (cl-assert (eq :futur--waiting-for-result (car cell)))
            (cond
             (failed nil)
             (err
              (setq failed t)
              (futur-deliver-failure new err)
              ;; Abort the remaining ones.
              (let ((abort-error (list 'futur-aborted `(futur-list . ,err))))
                (futur-blocker-abort futurs abort-error new)))
             (t
              (setf (car cell) val)
              (setq count (1- count))
              (when (zerop count)
                (pcase new
                  ;; We don't unbind ourselves from some FUTURs
                  ;; when aborting, so ignore their delivery here.
                  ((futur--failed `(futur-aborted . ,_)) nil)
                  (_ (futur-deliver-value new args))))))))
        futur new 'now-ok)
       (setq i (1+ i)))
      new)))

(defun futur-race (&rest futurs)
  "Build a `futur' that returns the value of the first of FUTURS that completes.
If the first future among FUTURS completes with a failure, then the new
future also completes with that same failure."
  (let* ((new (futur--waiting futurs)))
    (dolist (futur futurs)
      (futur--register-callback
       (lambda (err val)
         (pcase new
           ((futur--waiting)
            (futur--deliver new err val)
            ;; Abort the remaining ones.
            (let ((abort-error (list 'futur-aborted 'futur-race)))
              (futur-blocker-abort futurs abort-error new)))))
       futur new 'now-ok))
    new))

(cl-defmethod futur-blocker-wait ((_blockers cons))
  ;; FIXME: The loop below can misbehave when there's an early-exit
  ;; because of an error: we may remain waiting for the first blocker
  ;; while the second blocker has already signaled an error causing the
  ;; whole future to be aborted.  So we just "punt" and fallback on the
  ;; "generic" (and thus less efficient) wait loop
  ;;(let ((waited nil))
  ;;  (dolist (blocker blockers)
  ;;    (when (futur-blocker-wait blocker) (setq waited t)))
  ;;  waited)
  nil)

(cl-defmethod futur-blocker-abort ((futurs cons) error cfut)
  ;; Propagate the abort to the futurs we're still waiting for.
  (dolist (futur futurs)
    (futur-blocker-abort futur error cfut)))

;; FIXME: Make futures that returns synchronously if the result
;; is obtained before a specific timeout, or continue
;; asynchronously otherwise?
;; Or futures which emit a message (like "still waiting for ...") after
;; a certain timeout if the main future hasn't completed yet?

;;;; URL futures

(defun futur-url-retrieve (url &optional silent inhibit-cookies)
  "Retrieve URL asynchronously.
Thin wrapper around `url-retrieve'.
URL is either a string or a parsed URL.  If it is a string
containing characters that are not valid in a URI, those
characters are percent-encoded; see `url-encode-url'.

The future returns nil if the process has already completed (i.e. URL
was a mailto URL or similar).  Otherwise, it returns (BUFFER . STATUS),
where BUFFER contains the object, and any MIME headers associated with
it, and STATUS is a plist representing what happened during the request,
with most recent events first, or an empty list if no events have
occurred.  Each pair is one of:

\(:redirect REDIRECTED-TO) - the request was redirected to this URL.

\(:error (error type . DATA)) - an error occurred.  TYPE is a
symbol that says something about where the error occurred, and
DATA is a list (possibly nil) that describes the error further.

The variables `url-request-data', `url-request-method' and
`url-request-extra-headers' can be dynamically bound around the
request; dynamic binding of other variables doesn't necessarily
take effect.

If SILENT, then don't message progress reports and the like.
If INHIBIT-COOKIES, cookies will neither be stored nor sent to
the server.
If URL is a multibyte string, it will be encoded as utf-8 and
URL-encoded before it's used."
  (futur-new
   (lambda (f)
     (let ((res (url-retrieve url
                              (lambda (status)
                                (futur-deliver-value
                                 f (cons (current-buffer) status)))
                              nil silent inhibit-cookies)))
       (if (null res)
           (futur-deliver-value f res)
         (cons 'url-retrieve res))))))

;; (cl-defmethod futur-blocker-wait ((blocker (head url-retrieve))) nil)

(cl-defmethod futur-blocker-abort ((blocker (head url-retrieve)) _error _futur)
  ;; AFAIK the URL library doesn't provide support for aborting
  ;; a request, so this is a best-effort attempt.
  (when (buffer-live-p (cdr blocker))
    (with-current-buffer (cdr blocker)
      (let ((proc (get-buffer-process (current-buffer))))
        (when proc (delete-process proc))))))

;;;; Other helpers

(defmacro futur-with-temp-buffer (&rest body)
  (declare (indent 0) (debug t))
  `(futur--with-temp-buffer (lambda () ,@body)))

(defun futur--with-temp-buffer (body-fun)
  (let ((buf (generate-new-buffer " *temp*" t)))
    (futur--unwind-protect
     (with-current-buffer buf (funcall body-fun))
     (lambda () (and (buffer-live-p buf)
                (kill-buffer buf))))))

(provide 'futur)
;;; futur.el ends here
