;;; with-command-redo.el --- Repeat commands with automatic undo -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-with-command-redo
;; Version: 0.2
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; A generic mechanism for running a command repeatedly where each
;; successive call undoes the previous result before applying the next.
;; Consumers build cycling, completion, or other repeat-with-redo
;; behaviors on top.

;;; Code:


;; ---------------------------------------------------------------------------
;; Internal Variables


;; The global value must remain nil, use as a buffer-local variable only.
;;
;; Keys are:
;; - `buffer-undo-list': The `buffer-undo-list' before execution.
;; - `buffer-undo-list-post': The `buffer-undo-list' after execution.
;; - `pending-undo-list': The `pending-undo-list' before execution.
;; - `point': The point before execution.
;; - `chain-id': Symbol identifying the chain.
;; - `exec-count': Number of times the callback has been called (0-indexed).
;; - `fn-cache': Optional cache for the callback to use.
;; - `is-first-post-command': Flag to allow the chain's own command.
;; - `on-other-command': Handler for non-modifying external commands.
(defvar with-command-redo--alist nil
  "Internal state for the active command-redo chain.")


;; ---------------------------------------------------------------------------
;; Parameter Parsing

(defun with-command-redo--parse-params (params)
  "Parse keyword PARAMS, return an alist of parsed values."
  (declare (important-return-value t))
  (let ((kw-id nil)
        (kw-on-other-command nil)
        (keyw nil)
        (val nil)
        (params-rest params))

    (unless (listp params-rest)
      (error "with-command-redo: PARAMS must be a list"))

    (while (keywordp (setq keyw (car params-rest)))
      (setq params-rest (cdr params-rest))
      (setq val (pop params-rest))
      (pcase keyw
        (:id (setq kw-id val))
        (:on-other-command (setq kw-on-other-command val))
        (_ (error "With-command-redo: unknown keyword %S" keyw))))

    (when params-rest
      (error "With-command-redo: unexpected trailing arguments in PARAMS: %S" params-rest))

    (unless kw-id
      (error "With-command-redo: :id is required"))

    (list (cons 'id kw-id) (cons 'on-other-command kw-on-other-command))))


;; ---------------------------------------------------------------------------
;; Internal Utilities

(defun with-command-redo--buffer-undo-skip-barrier (list)
  "Skip leading nil boundaries in LIST.

Argument LIST is a `buffer-undo-list' compatible list."
  (declare (important-return-value t))
  (while (and list (null (car list)))
    (setq list (cdr list)))
  list)

(defun with-command-redo--undo-next (list)
  "Get the next undo step in LIST.

Argument LIST is a `buffer-undo-list' compatible list."
  (declare (important-return-value t))
  (while (car list)
    (setq list (cdr list)))
  (with-command-redo--buffer-undo-skip-barrier list))


;; ---------------------------------------------------------------------------
;; Hook Management

(defun with-command-redo--hooks-add ()
  "Add the post-command hook for the command-redo chain (buffer-local)."
  (declare (important-return-value nil))
  (add-hook 'post-command-hook #'with-command-redo--post-command-hook 0 t))

(defun with-command-redo--hooks-remove ()
  "Remove the post-command hook for the command-redo chain (buffer-local)."
  (declare (important-return-value nil))
  (remove-hook 'post-command-hook #'with-command-redo--post-command-hook t))

(defun with-command-redo--chain-clear ()
  "Tear down the active chain: remove the hook and drop the buffer-local state."
  (declare (important-return-value nil))
  (with-command-redo--hooks-remove)
  (kill-local-variable 'with-command-redo--alist))


;; ---------------------------------------------------------------------------
;; Hooks

(defun with-command-redo--post-command-hook ()
  "Post-command hook for the command-redo chain.
On the chain's own command: clear `is-first-post-command' flag.
On an external command that modified the buffer: break unconditionally.
On a non-modifying external command: call `on-other-command',
break the chain if it returns non-nil."
  (declare (important-return-value nil))
  (cond
   ;; The chain's own command just ran: clear the flag, keep hooks alive.
   ((alist-get 'is-first-post-command with-command-redo--alist)
    (setf (alist-get 'is-first-post-command with-command-redo--alist) nil))

   ;; External buffer modification: always break the chain.
   ((not
     (eq
      (with-command-redo--buffer-undo-skip-barrier buffer-undo-list)
      (alist-get 'buffer-undo-list-post with-command-redo--alist)))
    (with-command-redo--chain-clear))

   ;; A non-modifying external command ran while chain is active.
   (t
    (let ((on-other-command (alist-get 'on-other-command with-command-redo--alist)))
      (when (or (null on-other-command)
                (funcall on-other-command (alist-get 'fn-cache with-command-redo--alist)))
        (with-command-redo--chain-clear))))))


;; ---------------------------------------------------------------------------
;; Public API

;;;###autoload
(defmacro with-command-redo (params props-var &rest body)
  "Execute BODY with undo-and-redo chain management.
Expands to a call to `with-command-redo-fn', see that function for details.

PARAMS is an expression that evaluates to a plist (:id, :on-other-command).
PROPS-VAR is a symbol naming the plist bound in BODY.

Return value: the last form in BODY."
  (declare (indent 2))
  (unless (symbolp props-var)
    (error "with-command-redo: PROPS-VAR must be a symbol"))
  `(with-command-redo-fn ,params (lambda (,props-var) ,@body)))

;;;###autoload
(defun with-command-redo-fn (params fn)
  "Function variant of `with-command-redo'.

PARAMS is a plist of keyword arguments:

  :id SYMBOL
    Identifies the chain.  Required.

  :on-other-command FUNCTION
    Called from `post-command-hook' when a non-modifying external
    command runs while the chain is active.  Receives the cache as
    its sole argument.  Side effects (messaging, cache mutation)
    are permitted.  Return non-nil to break the chain, nil to keep
    it alive.

FN is called with a single plist argument containing:

  :count  The execution count (0 on the first call).
  :cache  nil on the first call.  Use `plist-put' to pass
          a value to the next call.
  :result Non-nil when the operation produced a result worth
          keeping (defaults to t).  Use `plist-put' to set nil
          when the operation has no result.

Note: only use `plist-put' to update existing keys.
Adding new keys requires `setq' on the return value which
won't be visible to the caller.

Return value: forwarded from FN."
  (declare (important-return-value t))

  (let* ((parsed (with-command-redo--parse-params params))
         (chain-id (alist-get 'id parsed))
         (on-other-command (alist-get 'on-other-command parsed)))

    ;; Undo must be enabled for the chain to work.
    (when (eq buffer-undo-list t)
      (user-error "With-command-redo: undo disabled for this buffer"))

    (let ((buffer-undo-list-init buffer-undo-list)
          (pending-undo-list-init pending-undo-list)
          (exec-count 0)
          (fn-cache nil)

          ;; Initial values if not overwritten by the values in the alist.
          (point-init (point)))

      ;; Roll-back and continue the chain.
      (let ((alist with-command-redo--alist))

        ;; Always clear immediately; reusing after an error could cause
        ;; unpredictable behavior.
        (kill-local-variable 'with-command-redo--alist)

        ;; Break the chain if the chain-id changed.
        (when (and alist (null (eq chain-id (alist-get 'chain-id alist))))
          (setq alist nil))

        (when alist

          ;; Update vars from previous state.
          (setq point-init (alist-get 'point alist))
          (setq buffer-undo-list-init (alist-get 'buffer-undo-list alist))
          (setq pending-undo-list-init (alist-get 'pending-undo-list alist))
          (setq fn-cache (alist-get 'fn-cache alist))
          (setq exec-count (1+ (alist-get 'exec-count alist)))

          ;; Undo with strict checks so we know _exactly_ what's going on
          ;; and don't allow some unknown state to be entered.
          (let ((undo-data (cdr buffer-undo-list)) ; Skip the 'nil' car of the list.
                (undo-data-init (cdr buffer-undo-list-init)))

            ;; It's possible the last action did not add an undo step.
            ;; This can happen when the callback produces no change.
            (unless (eq undo-data-init undo-data)
              ;; Ensure we have exactly one undo step added,
              ;; so calling undo returns to a known state.
              (unless (eq undo-data-init (with-command-redo--undo-next undo-data))
                (user-error "With-command-redo: unexpected undo-state before undo, abort!"))

              ;; Roll back the edit.
              (let ((this-command nil)
                    (undo-in-region nil)
                    (inhibit-message t)
                    (message-log-max nil))

                (undo-only)

                ;; Ensure a single undo step was rolled back.
                (setq undo-data (cdr buffer-undo-list))
                (unless (eq
                         undo-data-init
                         (with-command-redo--undo-next (with-command-redo--undo-next undo-data)))
                  (user-error "With-command-redo: unexpected undo-state after undo, abort!")))))

          ;; Roll back the buffer state.
          (setq buffer-undo-list buffer-undo-list-init)
          (setq pending-undo-list pending-undo-list-init)

          (goto-char point-init)))

      ;; Invoke the body with undo amalgamation.
      (let* ((props (list :count exec-count :cache fn-cache :result t))
             (body-result
              (with-undo-amalgamate
                (funcall fn props))))

        (cond
         ((null (plist-get props :result))
          ;; Failure, nothing happened.
          body-result)

         (t
          ;; Save state for the next chain call.
          (make-local-variable 'with-command-redo--alist)
          (setq with-command-redo--alist
                (list
                 (cons 'buffer-undo-list buffer-undo-list-init)
                 (cons
                  'buffer-undo-list-post
                  (with-command-redo--buffer-undo-skip-barrier buffer-undo-list))
                 (cons 'pending-undo-list pending-undo-list-init)
                 (cons 'point point-init)
                 (cons 'chain-id chain-id)
                 (cons 'exec-count exec-count)
                 (cons 'fn-cache (plist-get props :cache))
                 (cons 'is-first-post-command t)
                 (cons 'on-other-command on-other-command)))

          (with-command-redo--hooks-add)

          ;; Return the body's result.
          body-result))))))

;;;###autoload
(defun with-command-redo-active-p (id)
  "Return non-nil if a chain with ID is active in the current buffer."
  (declare (important-return-value t))
  (and (local-variable-p 'with-command-redo--alist)
       (eq id (alist-get 'chain-id with-command-redo--alist))))

;;;###autoload
(defun with-command-redo-break (id)
  "End the active command-redo chain with ID in the current buffer, if any.
A subsequent `with-command-redo-fn' then starts a fresh chain instead of
continuing (and rolling back) this one - useful when a command that begins a
chain may run while its own previous chain is still active (back-to-back)."
  (declare (important-return-value nil))
  (when (with-command-redo-active-p id)
    (with-command-redo--chain-clear)))

(provide 'with-command-redo)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; with-command-redo.el ends here
