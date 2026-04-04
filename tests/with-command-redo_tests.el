;;; with-command-redo_tests.el --- With command redo test -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2026  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-with-command-redo
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This is a test for `with-command-redo'.
;;

;;; Usage

;;
;; To test this file run:
;;
;;     `python tests/with-command-redo_tests.py'
;;

;;; Code:

(require 'ert)

;; ---------------------------------------------------------------------------
;; Setup Environment

;; Loaded by the Python test runner which pre-loads `with-command-redo'.

;; ---------------------------------------------------------------------------
;; Test Utilities

(defmacro simulate-input (&rest keys)
  "Helper macro to simulate input using KEYS."
  (declare (indent 0))
  `(let ((keys-list (list ,@keys)))
     (dolist (keys keys-list)
       (let ((minibuffer-message-timeout 0))
         (execute-kbd-macro keys)))))

(defun buffer-reset-text (initial-buffer-text)
  "Use INITIAL-BUFFER-TEXT to initialize the buffer with text."
  (buffer-disable-undo)
  (erase-buffer)
  (save-excursion (insert initial-buffer-text))
  (buffer-enable-undo))

(defmacro with-command-redo-test--run (initial-buffer-text command &rest body)
  "Run BODY in a buffer with INITIAL-BUFFER-TEXT, binding M-p to COMMAND.
Messages are suppressed."
  (declare (indent 2))
  `(let ((buf (generate-new-buffer "with-command-redo-test")))
     (switch-to-buffer buf)
     (buffer-reset-text ,initial-buffer-text)
     (let ((inhibit-message t))
       (local-set-key (kbd "M-p") ,command)
       ,@body)
     (kill-buffer buf)))

;; ---------------------------------------------------------------------------
;; Test Commands

(defun with-command-redo-test--cycle-insert ()
  "Insert cycling text: alpha, beta, gamma on successive calls."
  (interactive)
  (with-command-redo (list :id 'test-cycle) props
    (let ((count (plist-get props :count))
          (items '("alpha" "beta" "gamma")))
      (insert (nth (mod count (length items)) items)))))

(defun with-command-redo-test--cycle-insert-with-cache ()
  "Insert cycling text, using cache to track items."
  (interactive)
  (with-command-redo (list :id 'test-cache) props
    (let ((count (plist-get props :count))
          (cache (plist-get props :cache)))
      (unless cache
        (setq cache '("one" "two" "three"))
        (plist-put props :cache cache))
      (insert (nth (mod count (length cache)) cache)))))

(defun with-command-redo-test--insert-with-result ()
  "Insert text only on even calls, odd calls produce no result."
  (interactive)
  (with-command-redo (list :id 'test-result) props
    (let ((count (plist-get props :count)))
      (when (= (mod count 2) 0)
        (insert "even")
        (plist-put props :result t))
      (plist-put props :result (= (mod count 2) 0)))))

(defun with-command-redo-test--cycle-insert-other ()
  "Insert cycling text with a different chain id."
  (interactive)
  (with-command-redo (list :id 'test-cycle-other) props
    (let ((count (plist-get props :count))
          (items '("one" "two" "three")))
      (insert (nth (mod count (length items)) items)))))

(defun with-command-redo-test--cycle-insert-at-point ()
  "Insert cycling text at point, for testing point restoration."
  (interactive)
  (with-command-redo (list :id 'test-point) props
    (let ((count (plist-get props :count))
          (items '("[first]" "[second]" "[third]")))
      (insert (nth (mod count (length items)) items)))))

(defun with-command-redo-test--cycle-with-break-p ()
  "Insert cycling text, allowing `beginning-of-buffer' without breaking."
  (interactive)
  (with-command-redo
      (list :id 'test-break-p
            :on-other-command (lambda (_cache) (not (eq this-command 'beginning-of-buffer))))
      props
    (let ((count (plist-get props :count))
          (items '("alpha" "beta" "gamma")))
      (insert (nth (mod count (length items)) items)))))

;; ---------------------------------------------------------------------------
;; Tests: Single Call

(ert-deftest single-call ()
  "A single call should insert text."
  (with-command-redo-test--run "" #'with-command-redo-test--cycle-insert
    (simulate-input
      (kbd "M-p"))
    (should (equal "alpha" (buffer-substring-no-properties (point-min) (point-max))))))

;; ---------------------------------------------------------------------------
;; Tests: Chain Cycling

(ert-deftest chain-two-calls ()
  "Second call should undo the first and insert the next item."
  (with-command-redo-test--run "" #'with-command-redo-test--cycle-insert
    (simulate-input
      (kbd "M-p M-p"))
    (should (equal "beta" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest chain-three-calls ()
  "Three calls should cycle to the third item."
  (with-command-redo-test--run "" #'with-command-redo-test--cycle-insert
    (simulate-input
      (kbd "M-p M-p M-p"))
    (should (equal "gamma" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest chain-wraps-around ()
  "Four calls should wrap back to the first item."
  (with-command-redo-test--run "" #'with-command-redo-test--cycle-insert
    (simulate-input
      (kbd "M-p M-p M-p M-p"))
    (should (equal "alpha" (buffer-substring-no-properties (point-min) (point-max))))))

;; ---------------------------------------------------------------------------
;; Tests: Existing Buffer Content

(ert-deftest preserves-existing-text ()
  "Chain should not disturb text that existed before the chain."
  (with-command-redo-test--run "prefix " #'with-command-redo-test--cycle-insert
    (goto-char (point-max))
    (simulate-input
      (kbd "M-p M-p"))
    (should (equal "prefix beta" (buffer-substring-no-properties (point-min) (point-max))))))

;; ---------------------------------------------------------------------------
;; Tests: Cache

(ert-deftest cache-persists ()
  "Cache set in the first call should be available in subsequent calls."
  (with-command-redo-test--run "" #'with-command-redo-test--cycle-insert-with-cache
    (simulate-input
      (kbd "M-p M-p"))
    (should (equal "two" (buffer-substring-no-properties (point-min) (point-max))))))

;; ---------------------------------------------------------------------------
;; Tests: Result Flag

(ert-deftest result-nil-no-chain ()
  "When result is nil the chain should not be established."
  (with-command-redo-test--run "" #'with-command-redo-test--insert-with-result
    ;; First call (count=0, even) inserts "even", chain saved.
    ;; Second call (count=1, odd) undoes "even", result is nil, chain dropped.
    ;; Third call (count=0, fresh chain) inserts "even".
    (simulate-input
      (kbd "M-p M-p M-p"))
    (should (equal "even" (buffer-substring-no-properties (point-min) (point-max))))))

;; ---------------------------------------------------------------------------
;; Tests: Different ID Breaks Chain

(ert-deftest different-id-breaks-chain ()
  "Switching to a command with a different :id should break the chain."
  (with-command-redo-test--run "" #'with-command-redo-test--cycle-insert
    (local-set-key (kbd "M-n") #'with-command-redo-test--cycle-insert-other)
    ;; M-p inserts "alpha" (chain A), M-p cycles to "beta" (chain A),
    ;; M-n has a different id so starts fresh, inserts "one" without undoing "beta".
    (simulate-input
      (kbd "M-p M-p M-n"))
    (should (equal "betaone" (buffer-substring-no-properties (point-min) (point-max))))))

;; ---------------------------------------------------------------------------
;; Tests: External Modification Breaks Chain

(ert-deftest external-edit-breaks-chain ()
  "An external buffer modification should break the chain."
  (with-command-redo-test--run "" #'with-command-redo-test--cycle-insert
    ;; Bind M-n to a plain insert (external modification).
    (local-set-key
     (kbd "M-n")
     (lambda ()
       (interactive)
       (insert "EXT")))
    ;; M-p inserts "alpha", M-n inserts "EXT" (breaks chain via before-change-hook),
    ;; M-p starts fresh chain, inserts "alpha" again.
    (simulate-input
      (kbd "M-p M-n M-p"))
    (should (equal "alphaEXTalpha" (buffer-substring-no-properties (point-min) (point-max))))))

;; ---------------------------------------------------------------------------
;; Tests: On Other Command Handler

(ert-deftest on-other-command-keeps-chain ()
  "A command allowed by :on-other-command should not break the chain."
  (with-command-redo-test--run "" #'with-command-redo-test--cycle-with-break-p
    ;; Bind M-n to `beginning-of-buffer' which the handler allows.
    (local-set-key (kbd "M-n") #'beginning-of-buffer)
    ;; M-p inserts "alpha", M-n runs beginning-of-buffer (allowed, chain continues),
    ;; M-p cycles to "beta".
    (simulate-input
      (kbd "M-p M-n M-p"))
    (should (equal "beta" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest on-other-command-breaks-chain ()
  "A command not allowed by :on-other-command should break the chain."
  (with-command-redo-test--run "" #'with-command-redo-test--cycle-with-break-p
    ;; Bind M-n to `end-of-buffer' which the handler does not allow.
    (local-set-key (kbd "M-n") #'end-of-buffer)
    ;; M-p inserts "alpha", M-n runs end-of-buffer (not allowed, chain breaks),
    ;; M-p starts fresh, inserts "alpha" again (not "beta").
    (simulate-input
      (kbd "M-p M-n M-p"))
    (should (equal "alphaalpha" (buffer-substring-no-properties (point-min) (point-max))))))

;; ---------------------------------------------------------------------------
;; Tests: Point Restoration

(ert-deftest point-restored-between-calls ()
  "Point should be restored to its pre-chain position before each redo."
  (with-command-redo-test--run "abcd" #'with-command-redo-test--cycle-insert-at-point
    ;; Place point between "ab" and "cd".
    (goto-char 3)
    ;; M-p inserts "[first]" at point 3 -> "ab[first]cd".
    ;; M-p undoes, restores point to 3, inserts "[second]" -> "ab[second]cd".
    (simulate-input
      (kbd "M-p M-p"))
    (should (equal "ab[second]cd" (buffer-substring-no-properties (point-min) (point-max))))))

;; ---------------------------------------------------------------------------
;; Tests: Undo After Chain

(ert-deftest undo-after-chain ()
  "After the chain ends, undo should cleanly restore the original buffer."
  (with-command-redo-test--run "original" #'with-command-redo-test--cycle-insert
    (goto-char (point-max))
    ;; M-p inserts "alpha", M-p cycles to "beta".
    (simulate-input
      (kbd "M-p M-p"))
    (should (equal "originalbeta" (buffer-substring-no-properties (point-min) (point-max))))
    ;; Now break the chain with a non-chain command, then undo.
    ;; beginning-of-buffer breaks the chain, then undo should remove "beta".
    (local-set-key (kbd "M-n") #'beginning-of-buffer)
    (local-set-key (kbd "M-u") #'undo)
    (simulate-input
      (kbd "M-n M-u"))
    (should (equal "original" (buffer-substring-no-properties (point-min) (point-max))))))

;; ---------------------------------------------------------------------------
;; Tests: Undo Disabled

(ert-deftest undo-disabled-signals-error ()
  "Calling the macro in a buffer with undo disabled should signal a user-error."
  (with-command-redo-test--run "" #'with-command-redo-test--cycle-insert
    (buffer-disable-undo)
    (should-error
     (with-command-redo-test--cycle-insert)
     :type 'user-error)))

;; ---------------------------------------------------------------------------
;; Test Commands: Function Variant

(defun with-command-redo-test--fn-cycle-insert ()
  "Insert cycling text using `with-command-redo-fn'."
  (interactive)
  (with-command-redo-fn
   (list :id 'test-fn-cycle)
   (lambda (props)
     (let ((count (plist-get props :count))
           (items '("alpha" "beta" "gamma")))
       (insert (nth (mod count (length items)) items))))))

;; ---------------------------------------------------------------------------
;; Tests: Function Variant

(ert-deftest fn-single-call ()
  "A single call via `with-command-redo-fn' should insert text."
  (with-command-redo-test--run "" #'with-command-redo-test--fn-cycle-insert
    (simulate-input
      (kbd "M-p"))
    (should (equal "alpha" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest fn-chain-two-calls ()
  "Second call via `with-command-redo-fn' should undo the first and insert the next."
  (with-command-redo-test--run "" #'with-command-redo-test--fn-cycle-insert
    (simulate-input
      (kbd "M-p M-p"))
    (should (equal "beta" (buffer-substring-no-properties (point-min) (point-max))))))

(provide 'with-command-redo_tests)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; with-command-redo_tests.el ends here
