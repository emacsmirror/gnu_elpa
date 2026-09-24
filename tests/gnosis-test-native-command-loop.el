;;; gnosis-test-native-command-loop.el --- Disposable native journeys -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Run real terminal Emacs children, not batch-mode recursive-edit substitutes.
;; The ordinary per-file Make runner discovers this suite.  No display server,
;; Python, learner daemon, or external input automation is needed.

;;; Code:

(require 'ert)
(require 'gnosis-test-helpers)

(defun gnosis-test-native--run (journey)
  "Run JOURNEY in a disposable terminal Emacs with a bounded watchdog."
  (let* ((root (make-temp-file "gnosis-native-" t))
         (log (expand-file-name "receipt" root))
         (output (generate-new-buffer " *gnosis-native-terminal*"))
         (process-environment (copy-sequence process-environment))
         (deadline (+ (float-time) 45))
         (cursor 0)
         process receipt)
    (unwind-protect
        (progn
          (dolist (variable '("HOME" "XDG_CACHE_HOME" "XDG_CONFIG_HOME"
                              "XDG_DATA_HOME" "XDG_STATE_HOME" "TMPDIR"))
            (let ((directory (expand-file-name variable root)))
              (make-directory directory)
              (setenv variable directory)))
          (setenv "TERM" "xterm")
          (setenv "GNOSIS_NATIVE_ROOT" root)
          (setenv "GNOSIS_NATIVE_JOURNEY" (symbol-name journey))
          (setq process
                (make-process
                 :name "gnosis-native" :buffer output :noquery t
                 :connection-type 'pty
                 :command
                 (list (expand-file-name invocation-name invocation-directory)
                       "-Q" "-nw" "--eval"
                       (prin1-to-string
                        `(condition-case error-data
                             (progn
                               (setq load-path ',(mapcar #'expand-file-name load-path)
                                     load-prefer-newer t
                                     gnosis-dir (file-name-as-directory
                                                 (getenv "GNOSIS_NATIVE_ROOT"))
                                     gnosis-testing t gnosis-vc-auto-push nil)
                               (require 'gnosis-native-command-loop)
                               (add-hook 'emacs-startup-hook #'gnosis-native--run))
                           (error (message "Native setup failed: %S" error-data)
                                  (kill-emacs 2)))))))
          (while (and (process-live-p process) (< (float-time) deadline))
            (accept-process-output process 0.05)
            (when (file-exists-p log)
              (with-temp-buffer
                (insert-file-contents log)
                (goto-char (1+ cursor))
                (while (search-forward "\n" nil t)
                  (let ((line (buffer-substring-no-properties
                               (1+ cursor) (1- (point)))))
                    (setq cursor (1- (point)))
                    (when (string-prefix-p "INPUT " line)
                      (process-send-string process (read (substring line 6)))))))))
          (setq receipt (when (file-exists-p log)
                          (with-temp-buffer
                            (insert-file-contents log) (buffer-string))))
          (ert-info ((format "Native %S receipt:\n%s\nTerminal:\n%s"
                             journey receipt
                             (with-current-buffer output (buffer-string))))
            (should-not (process-live-p process))
            (should (eq (process-status process) 'exit))
            (should (= (process-exit-status process) 0))
            (should (string-match-p "^PASS native journey depth=0$" (or receipt ""))))
          (message "%s" receipt))
      (when (and process (process-live-p process)) (delete-process process))
      (kill-buffer output)
      (delete-directory root t))))

(ert-deftest gnosis-test-native-monkeytype ()
  "Standalone typing completion and cancellation unwind real recursive input."
  (gnosis-test-native--run 'monkeytype))

(ert-deftest gnosis-test-native-review-edit ()
  "Due/practice native edit, save and cancel retain pending acceptance."
  (gnosis-test-native--run 'review))

(ert-deftest gnosis-test-native-feedback ()
  "Native feedback supports browsing, popup control and owned acceptance."
  (gnosis-test-native--run 'feedback))

(ert-deftest gnosis-test-native-journal ()
  "Repeated multiline capture and local TODOs retain dates across reopening."
  (gnosis-test-native--run 'journal))

(ert-deftest gnosis-test-native-sources ()
  "Source open, choice, cancel and return preserve both modes' pending input."
  (gnosis-test-native--run 'sources))

(ert-deftest gnosis-test-native-recovery ()
  "Revisited reviews keep safe help/actions during editing and source input."
  (gnosis-test-native--run 'recovery))

(provide 'gnosis-test-native-command-loop)
;;; gnosis-test-native-command-loop.el ends here
