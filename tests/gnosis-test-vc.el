;;; gnosis-test-vc.el --- Git chain ownership tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise deferred chains with real Git processes and isolated local data.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gnosis-vc)

(defmacro gnosis-test-vc--with-directories (&rest body)
  "Run BODY with private directories and observed real Git processes."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "gnosis-vc-" t))
          (a (file-name-as-directory (expand-file-name "a" root)))
          (b (file-name-as-directory (expand-file-name "b" root)))
          (gnosis-dir a)
          (default-directory a)
          (process-connection-type nil)
          (process-environment
           (append (list "GIT_CONFIG_NOSYSTEM=1"
                         (concat "GIT_CONFIG_GLOBAL=" null-device))
                   (cl-remove-if
                    (lambda (entry) (string-prefix-p "GIT_" entry))
                    process-environment)))
          (start (symbol-function 'start-process))
          processes directories)
     (unwind-protect
         (progn
           (skip-unless (executable-find "git"))
           (dolist (directory (list a b))
             (make-directory directory)
             (dolist (file '("one" "two"))
               (with-temp-file (expand-file-name file directory)
                 (insert "identical\n"))))
           (cl-letf (((symbol-function 'start-process)
                      (lambda (&rest args)
                        (push default-directory directories)
                        (let ((process (apply start args)))
                          (push process processes)
                          process))))
             ,@body))
       (dolist (process processes)
         (set-process-sentinel process #'ignore)
         (when (process-live-p process) (delete-process process)))
       (delete-directory root t))))

(defun gnosis-test-vc--wait (predicate)
  "Dispatch process events until PREDICATE succeeds, or fail after five seconds."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(ert-deftest gnosis-test-vc-chain-retains-directory ()
  "Keep every command and the final callback in the admitting directory."
  (gnosis-test-vc--with-directories
    (let (finished owner)
      (gnosis--git-chain
       '(("--version") ("--no-pager" "diff" "--no-index" "one" "two")
         ("--version"))
       (lambda () (setq owner (list gnosis-dir default-directory) finished t)))
      (setq gnosis-dir b default-directory b)
      (gnosis-test-vc--wait (lambda () finished))
      (should (equal (reverse directories) (list a a a)))
      (should (equal owner (list a a)))
      (should (equal gnosis-dir b))
      (should (equal default-directory b)))))

(ert-deftest gnosis-test-vc-chain-pins-relative-directory ()
  "Resolve relative configuration before the initiating buffer changes."
  (gnosis-test-vc--with-directories
    (let ((gnosis-dir "./") finished owner)
      (gnosis--git-chain
       '(("--version") ("--no-pager" "diff" "--no-index" "one" "two"))
       (lambda () (setq owner (list gnosis-dir default-directory) finished t)))
      (setq default-directory b)
      (gnosis-test-vc--wait (lambda () finished))
      (should (equal (reverse directories) (list a a)))
      (should (equal owner (list a a)))
      (should (equal gnosis-dir "./"))
      (should (equal default-directory b)))))

(ert-deftest gnosis-test-vc-chain-final-push-retains-directory ()
  "The public push callback must use the chain owner, not current configuration."
  (gnosis-test-vc--with-directories
    ;; Empty local repositories have no remotes: real push must fail locally.
    (dolist (directory (list a b))
      (let ((default-directory directory))
        (should (zerop (call-process "git" nil nil nil "-c" "init.templateDir="
                                     "init" "--quiet")))))
    (gnosis--git-chain '(("--version") ("--version")) #'gnosis-vc-push)
    (setq gnosis-dir b default-directory b)
    (gnosis-test-vc--wait
     (lambda () (and (= (length processes) 3)
                     (eq (process-status (car processes)) 'exit))))
    (should (equal (cdr (process-command (car processes))) '("push")))
    (should-not (zerop (process-exit-status (car processes))))
    (should (equal (reverse directories) (list a a a)))
    (should (equal gnosis-dir b))
    (should (equal default-directory b))))

(ert-deftest gnosis-test-vc-chain-nonzero-and-signal-stop ()
  "Failed and killed processes must never start successors or finish."
  (dolist (failure '(nonzero signal))
    (gnosis-test-vc--with-directories
      (let (finished)
        (with-temp-file (expand-file-name "two" a) (insert "different\n"))
        (let* ((process
                (gnosis--git-chain
                 (list (if (eq failure 'nonzero)
                           '("--no-pager" "diff" "--no-index" "one" "two")
                         '("hash-object" "--stdin"))
                       '("--version"))
                 (lambda () (setq finished t))))
               (sentinel (process-sentinel process))
               settled)
          (set-process-sentinel
           process (lambda (proc event)
                     (funcall sentinel proc event)
                     (when (memq (process-status proc) '(exit signal))
                       (setq settled t))))
          (setq gnosis-dir b default-directory b)
          (when (eq failure 'signal) (delete-process process))
          (gnosis-test-vc--wait (lambda () settled))
          (should-not finished)
          (should (equal directories (list a)))
          (should (equal gnosis-dir b)))))))

(ert-deftest gnosis-test-vc-chain-faults-restore-directory ()
  "Errors and quits in continuation startup or completion restore the caller."
  (dolist (stage '(start finish))
    (dolist (fault '(error quit))
      (gnosis-test-vc--with-directories
        (let* ((process
                (gnosis--git-chain
                 (if (eq stage 'start) '(("--version") ("--version"))
                   '(("--version")))
                 (lambda () (signal fault '("Completion fault")))))
               (sentinel (process-sentinel process))
               attempted-owner caught)
          ;; Dispatch explicitly so ERT can inspect errors normally demoted by
          ;; Emacs's process event loop.  The predecessor process is still real.
          (set-process-sentinel process #'ignore)
          (gnosis-test-vc--wait (lambda () (eq (process-status process) 'exit)))
          (setq gnosis-dir b default-directory b)
          (cl-letf (((symbol-function 'start-process)
                     (lambda (&rest _)
                       (setq attempted-owner (list gnosis-dir default-directory))
                       (signal fault '("Startup fault")))))
            (condition-case err
                (funcall sentinel process "finished\n")
              ((error quit) (setq caught (car err)))))
          (should (eq caught fault))
          (when (eq stage 'start)
            (should (equal attempted-owner (list a a))))
          (should (equal gnosis-dir b))
          (should (equal default-directory b))
          (should-not (process-live-p process)))))))

(ert-deftest gnosis-test-vc-chain-initial-start-fault-restores-directory ()
  "Failure to create the first process leaves no work or directory binding."
  (dolist (fault '(error quit))
    (gnosis-test-vc--with-directories
      (let ((gnosis-dir "./") finished caught)
        (cl-letf (((symbol-function 'start-process)
                   (lambda (&rest _) (signal fault '("Startup fault")))))
          (condition-case err
              (gnosis--git-chain '(("--version"))
                                 (lambda () (setq finished t)))
            ((error quit) (setq caught (car err)))))
        (should (eq caught fault))
        (should-not finished)
        (should-not processes)
        (should (equal gnosis-dir "./"))
        (should (equal default-directory a))))))

(ert-deftest gnosis-test-vc-empty-chain-binds-completion-directory ()
  "Even immediate completion receives an absolute owner without leaking it."
  (gnosis-test-vc--with-directories
    (let ((gnosis-dir "./") owner)
      (gnosis--git-chain
       nil (lambda () (setq owner (list gnosis-dir default-directory))))
      (should (equal owner (list a a)))
      (should-not processes)
      (should (equal gnosis-dir "./")))))

(provide 'gnosis-test-vc)
;;; gnosis-test-vc.el ends here
