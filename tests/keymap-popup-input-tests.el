;;; keymap-popup-input-tests.el --- Native popup input tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'keymap-popup)

(defmacro keymap-popup-input--with-source (&rest body)
  "Run BODY with isolated command-loop state and a visible source buffer."
  (declare (indent 0) (debug t))
  `(let ((overriding-terminal-local-map nil)
         (pre-command-hook nil) (post-command-hook nil)
         (minibuffer-setup-hook nil) (minibuffer-exit-hook nil)
         (prefix-arg nil) (current-prefix-arg nil)
         (keymap-popup-persistent nil)
         (keymap-popup--buffer-name " *keymap-popup-input*")
         (source (generate-new-buffer " *keymap-popup-input-source*")))
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer source)
           ;; Let a disposable terminal finish its initial command setup.
           (execute-kbd-macro (kbd "C-e"))
           ,@body)
       (keymap-popup-dismiss)
       (kill-buffer source))))

(defun keymap-popup-input--clean-p ()
  "Assert that native popup maps, buffers and hooks have been removed."
  (should-not (get-buffer keymap-popup--buffer-name))
  (should-not overriding-terminal-local-map)
  (should-not pre-command-hook)
  (should-not post-command-hook)
  (should-not minibuffer-setup-hook)
  (should-not minibuffer-exit-hook))

(defun keymap-popup-input--settled-p ()
  "Assert that the visible popup no longer advertises a pending prefix."
  (let ((buf (get-buffer keymap-popup--buffer-name)))
    (should (buffer-live-p buf))
    (should-not (keymap-popup--session-get buf :prefix-mode))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "(Prefix)")
      (should (eq (get-text-property (1- (point)) 'face) 'shadow)))))

(defun keymap-popup-input--suffix-map (command persistent &rest properties)
  "Return an action map for COMMAND, PERSISTENT and entry PROPERTIES."
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" command)
    (keymap-popup-attach
     map (list "a" (append (list "Action" command :c-u "Prefix") properties))
     :persistent persistent)
    map))

(ert-deftest keymap-popup-input-native-prefix-control ()
  (keymap-popup-input--with-source
    (let ((observed nil) (map (make-sparse-keymap)))
      (keymap-set map "a" (lambda (arg) (interactive "P") (push arg observed)))
      (use-local-map map)
      (execute-kbd-macro (kbd "C-u 3 a C-u a C-u - 3 a C-u C-u a a"))
      (should (equal (reverse observed) '(3 (4) -3 (16) nil))))))

(defun keymap-popup-input--repeat-prefix (persistent)
  "Check repeated accepted prefixes with PERSISTENT or stay-open dispatch."
  (keymap-popup-input--with-source
    (let* ((observed nil)
           (command (lambda (arg) (interactive "P") (push arg observed))))
      (keymap-popup (apply #'keymap-popup-input--suffix-map command persistent
                          (unless persistent '(:stay-open t))))
      (execute-kbd-macro (kbd "C-u 3 a"))
      (keymap-popup-input--settled-p)
      (execute-kbd-macro (kbd "C-u a C-u - 3 a C-u C-u a a"))
      (should (equal (reverse observed) '(3 (4) -3 (16) nil)))
      (keymap-popup-input--settled-p)
      (execute-kbd-macro (kbd "q"))
      (keymap-popup-input--clean-p))))

(ert-deftest keymap-popup-input-stay-open-consumes-prefix ()
  (keymap-popup-input--repeat-prefix nil))

(ert-deftest keymap-popup-input-persistent-consumes-prefix ()
  (keymap-popup-input--repeat-prefix t))

(ert-deftest keymap-popup-input-inapt-retains-negative-prefix ()
  (dolist (persistent '(nil t))
    (keymap-popup-input--with-source
      (let* ((observed nil)
             (command (lambda (arg) (interactive "P") (push arg observed)))
             (map (keymap-popup-input--suffix-map command persistent :stay-open t)))
        (keymap-set map "b" command)
        (keymap-popup-attach
         map (list "a" (list "Action" command :stay-open t
                             :inapt-if (lambda () t))
                   "b" (list "Accept" command :stay-open t :c-u "Prefix"))
         :persistent persistent)
        (keymap-popup map)
        ;; Keep the refused and accepted keys in one native macro: starting
        ;; another execute-kbd-macro resets the pending native prefix.
        (execute-kbd-macro (kbd "C-u - 3 a b"))
        (should (equal observed '(-3)))
        (keymap-popup-input--settled-p)
        (execute-kbd-macro (kbd "q"))
        (keymap-popup-input--clean-p)))))

(defun keymap-popup-input--failing-prefix (persistent condition)
  "Check prefix settlement when PERSISTENT dispatch signals CONDITION."
  (keymap-popup-input--with-source
    (let* ((observed nil) (fail t)
           (command (lambda (arg)
                      (interactive "P")
                      (push arg observed)
                      (when fail (signal condition '("Input test"))))))
      (keymap-popup (apply #'keymap-popup-input--suffix-map command persistent
                          (unless persistent '(:stay-open t))))
      (condition-case nil
          (execute-kbd-macro (kbd "C-u 3 a"))
        ((error quit) nil))
      ;; A command error need not run post-command-hook.
      (should (equal observed '(3)))
      (keymap-popup-input--settled-p)
      (setq fail nil)
      (execute-kbd-macro (kbd "C-u a"))
      (should (equal observed '((4) 3)))
      (execute-kbd-macro (kbd "q"))
      (keymap-popup-input--clean-p))))

(ert-deftest keymap-popup-input-stay-open-error-consumes-prefix ()
  (keymap-popup-input--failing-prefix nil 'error))

(ert-deftest keymap-popup-input-stay-open-quit-consumes-prefix ()
  (keymap-popup-input--failing-prefix nil 'quit))

(ert-deftest keymap-popup-input-persistent-error-consumes-prefix ()
  (keymap-popup-input--failing-prefix t 'error))

(ert-deftest keymap-popup-input-persistent-quit-consumes-prefix ()
  (keymap-popup-input--failing-prefix t 'quit))

(defun keymap-popup-input--submenu-fallthrough (second-key)
  "Exercise a parent-only submenu SECOND-KEY through native map teardown."
  (keymap-popup-input--with-source
    (let* ((child (keymap-popup-input--suffix-map #'ignore nil))
           (other (keymap-popup-input--suffix-map #'ignore nil))
           (root (make-sparse-keymap)))
      (keymap-set root "s" (lambda () (interactive) (keymap-popup child)))
      (keymap-set root "d" (lambda () (interactive) (keymap-popup other)))
      (keymap-popup-attach root
                          (list "s" (list "Child" :keymap child)
                                "d" (list "Other" :keymap other)))
      (keymap-popup root)
      (execute-kbd-macro (kbd "s"))
      (let ((old (get-buffer keymap-popup--buffer-name)))
        (execute-kbd-macro (kbd second-key))
        (let ((buf (get-buffer keymap-popup--buffer-name)))
          (should (buffer-live-p buf))
          (should-not (eq old buf))
          (should (eq (keymap-popup--active-get buf :keymap)
                      (if (equal second-key "s") child other)))
          (should-not (keymap-popup--session-get buf :stack))))
      (execute-kbd-macro (kbd "q"))
      (keymap-popup-input--clean-p))))

(ert-deftest keymap-popup-input-repeated-parent-submenu ()
  (keymap-popup-input--submenu-fallthrough "s"))

(ert-deftest keymap-popup-input-different-parent-submenu ()
  (keymap-popup-input--submenu-fallthrough "d"))

(ert-deftest keymap-popup-input-child-binding-wins ()
  (keymap-popup-input--with-source
    (let ((child (make-sparse-keymap)) (root (make-sparse-keymap))
          (called nil))
      (keymap-set child "s" (lambda () (interactive) (setq called t)))
      (keymap-set root "s" (lambda () (interactive) (keymap-popup child)))
      (keymap-popup-attach
       child (list "s" (list "Child action" (keymap-lookup child "s"))))
      (keymap-popup-attach root (list "s" (list "Child" :keymap child)))
      (keymap-popup root)
      (execute-kbd-macro (kbd "s s"))
      (should called)
      (keymap-popup-input--clean-p))))

(ert-deftest keymap-popup-input-preserves-native-command-state ()
  (dolist (persistent '(nil t))
    (keymap-popup-input--with-source
      (let* ((observed nil) (identity nil)
             (command (lambda (arg)
                        (interactive "P")
                        (setq identity this-command)
                        (push arg observed)
                        (setq prefix-arg 7)))
             (next (lambda (arg) (interactive "P") (push arg observed)))
             (map (keymap-popup-input--suffix-map command persistent :stay-open t)))
        (keymap-set map "b" next)
        (keymap-popup-attach
         map (list "a" (list "Action" command :stay-open t :c-u "Prefix")
                   "b" (list "Next" next :stay-open t))
         :persistent persistent)
        (keymap-popup map)
        (execute-kbd-macro (kbd "C-u a b"))
        (should (equal observed '(7 (4))))
        (should (eq identity command))
        (should (eq last-command next))
        (execute-kbd-macro (kbd "q"))
        (keymap-popup-input--clean-p)))))

(ert-deftest keymap-popup-input-expired-submenu-uses-live-binding ()
  (keymap-popup-input--with-source
    (let ((root (make-sparse-keymap))
          (child (keymap-popup-input--suffix-map #'ignore nil))
          (called nil))
      (keymap-set root "s" (lambda () (interactive) (keymap-popup child)))
      (keymap-popup-attach root (list "s" (list "Child" :keymap child)))
      (keymap-popup root)
      (execute-kbd-macro (kbd "s"))
      (keymap-set root "s" (lambda () (interactive) (setq called t)))
      (execute-kbd-macro (kbd "s"))
      (should called)
      (keymap-popup-input--clean-p))))

(provide 'keymap-popup-input-tests)
;;; keymap-popup-input-tests.el ends here
