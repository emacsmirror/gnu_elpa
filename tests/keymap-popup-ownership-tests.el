;;; keymap-popup-ownership-tests.el --- Resource tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Public launch and retirement with real windows and failing callbacks.

;;; Code:
(require 'ert)
(require 'keymap-popup)

(defun keymap-popup-ownership--map ()
  "Return a small described keymap."
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" #'ignore)
    (keymap-popup-attach map '("a" ("Action" ignore)))
    map))

(defmacro keymap-popup-ownership--isolated (&rest body)
  "Run BODY with disposable popup and command-loop state."
  (declare (indent 0) (debug t))
  (let ((buffers (make-symbol "buffers"))
        (prepare (make-symbol "prepare")))
    `(let ((keymap-popup--buffer-name " *keymap-popup-ownership*")
           (keymap-popup--buffer nil)
           (overriding-terminal-local-map nil)
           (pre-command-hook nil) (post-command-hook nil)
           ;; Globalized modes can schedule unrelated command hooks on Emacs 29.
           (change-major-mode-hook nil) (after-change-major-mode-hook nil)
           (minibuffer-setup-hook nil) (minibuffer-exit-hook nil)
           (,buffers nil)
           (,prepare (symbol-function 'keymap-popup--prepare-buffer)))
       (save-window-excursion
         (unwind-protect
             (cl-letf (((symbol-function 'keymap-popup--prepare-buffer)
                        (lambda ()
                          (let ((buf (funcall ,prepare)))
                            (push buf ,buffers)
                            buf))))
               ,@body)
           (condition-case nil (keymap-popup-dismiss) ((error quit) nil))
           ;; File association renames buffers: retain fixture objects, not names.
           (dolist (buf (cons (get-buffer keymap-popup--buffer-name) ,buffers))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq buffer-file-name nil buffer-auto-save-file-name nil)
                 (set-buffer-modified-p nil)
                 (let ((kill-buffer-query-functions nil))
                   (kill-buffer buf))))))))))

(defun keymap-popup-ownership--failure (thunk)
  "Return the error or quit signaled by THUNK."
  (condition-case err (progn (funcall thunk) nil)
    ((error quit) err)))

(defun keymap-popup-ownership--clean-p ()
  "Assert that no package input resources remain."
  (should-not overriding-terminal-local-map)
  (should-not (default-value 'pre-command-hook))
  (should-not (default-value 'post-command-hook))
  (should-not minibuffer-setup-hook)
  (should-not minibuffer-exit-hook))

(ert-deftest keymap-popup-ownership-foreign-collisions ()
  (dolist (kind '(ordinary file veto))
    (keymap-popup-ownership--isolated
      (let* ((foreign (get-buffer-create keymap-popup--buffer-name))
             (map (make-sparse-keymap))
             (keymap-popup-backend
              (lambda () (list :show #'ignore :fit #'ignore :hide #'ignore))))
        (with-current-buffer foreign
          (insert "Unsaved foreign text")
          (use-local-map map)
          (when (eq kind 'file)
            (setq buffer-file-name "/nonexistent/keymap-popup-foreign"))
          (when (eq kind 'veto)
            (setq-local kill-buffer-query-functions (list (lambda () nil)))))
        (keymap-popup (keymap-popup-ownership--map))
        (keymap-popup-dismiss)
        (should (buffer-live-p foreign))
        (with-current-buffer foreign
          (should (equal (buffer-string) "Unsaved foreign text"))
          (should (eq (current-local-map) map))
          (should (buffer-modified-p)))
        (keymap-popup-ownership--clean-p)))))

(ert-deftest keymap-popup-ownership-renamed-owner ()
  (keymap-popup-ownership--isolated
    (keymap-popup (keymap-popup-ownership--map))
    (let ((owned (get-buffer keymap-popup--buffer-name))
          (foreign nil))
      (unwind-protect
          (progn
            (with-current-buffer owned (rename-buffer " *renamed-popup*" t))
            (setq foreign (get-buffer-create keymap-popup--buffer-name))
            (with-current-buffer foreign (insert "Replacement draft"))
            (cl-letf (((symbol-function 'universal-argument--mode) #'ignore))
              (keymap-popup--prefix-argument))
            (should (keymap-popup--session-get owned :prefix-mode))
            (keymap-popup--suspend)
            (should-not overriding-terminal-local-map)
            (keymap-popup--resume)
            (should overriding-terminal-local-map)
            (keymap-popup-dismiss)
            (should-not (buffer-live-p owned))
            (should (equal (with-current-buffer foreign (buffer-string))
                           "Replacement draft")))
        (setq prefix-arg nil)
        (when (buffer-live-p owned) (kill-buffer owned))))))

(ert-deftest keymap-popup-ownership-duplicate-window ()
  (keymap-popup-ownership--isolated
    (delete-other-windows)
    (let* ((ordinary (selected-window))
           (other (split-window-right))
           (fits nil)
           (native-fit (symbol-function 'fit-window-to-buffer)))
      (keymap-popup (keymap-popup-ownership--map))
      (let* ((buf (get-buffer keymap-popup--buffer-name))
             (popup (get-buffer-window buf)))
        (switch-to-buffer buf)
        (cl-letf (((symbol-function 'fit-window-to-buffer)
                   (lambda (window &rest args)
                     (push window fits)
                     (apply native-fit window args))))
          (keymap-popup--refresh buf))
        (should (equal fits (list popup)))
        (keymap-popup-dismiss)
        (should (window-live-p ordinary))
        (should (window-live-p other))
        (should-not (window-live-p popup))))))

(ert-deftest keymap-popup-ownership-reused-window ()
  (keymap-popup-ownership--isolated
    (delete-other-windows)
    (let ((ordinary (selected-window))
          (source (window-buffer))
          (keymap-popup-display-action '(display-buffer-same-window)))
      (keymap-popup (keymap-popup-ownership--map))
      (keymap-popup-dismiss)
      (should (window-live-p ordinary))
      (should (eq (window-buffer ordinary) source)))))

(ert-deftest keymap-popup-ownership-stale-window ()
  (keymap-popup-ownership--isolated
    (keymap-popup (keymap-popup-ownership--map))
    (let* ((buf (get-buffer keymap-popup--buffer-name))
           (owned (get-buffer-window buf))
           (other (generate-new-buffer " *replacement-window*")))
      (unwind-protect
          (progn
            (set-window-dedicated-p owned nil)
            (set-window-buffer owned other)
            (keymap-popup-dismiss)
            (should (window-live-p owned))
            (should (eq (window-buffer owned) other)))
        (kill-buffer other)))))

(ert-deftest keymap-popup-ownership-acquisition-failures ()
  (dolist (stage '(render show fit activate))
    (dolist (condition '(error quit))
      (keymap-popup-ownership--isolated
        (let* ((backend (keymap-popup-backend-side-window))
               (fit-count 0) (hides 0) (buf nil)
               (native-activate (symbol-function 'keymap-popup--activate-transient-map))
               (keymap-popup-backend
                (lambda ()
                  (list :show (lambda (b)
                                (setq buf b)
                                (funcall (plist-get backend :show) b)
                                (when (eq stage 'show)
                                  (signal condition '("primary"))))
                        :fit (lambda (b)
                               (setq buf b fit-count (1+ fit-count))
                               (funcall (plist-get backend :fit) b)
                               (when (or (eq stage 'render)
                                         (and (eq stage 'fit) (= fit-count 2)))
                                 (signal condition '("primary"))))
                        :hide (lambda (b)
                                (setq hides (1+ hides))
                                (funcall (plist-get backend :hide) b))))))
          (cl-letf (((symbol-function 'keymap-popup--activate-transient-map)
                     (lambda (b)
                       (funcall native-activate b)
                       (when (eq stage 'activate)
                         (signal condition '("primary"))))))
            (should (equal (keymap-popup-ownership--failure
                            (lambda () (keymap-popup (keymap-popup-ownership--map))))
                           (list condition "primary"))))
          (should (= hides 1))
          (should-not (buffer-live-p buf))
          (keymap-popup-ownership--clean-p))))))

(ert-deftest keymap-popup-ownership-hide-failures ()
  (dolist (condition '(error quit))
    (dolist (launch-fails '(nil t))
      (keymap-popup-ownership--isolated
        (let* ((backend (keymap-popup-backend-side-window))
               (buf nil)
               (keymap-popup-persistent t)
               (keymap-popup-backend
                (lambda ()
                  (list :show (lambda (b)
                                (setq buf b)
                                (funcall (plist-get backend :show) b)
                                (when launch-fails (error "primary")))
                        :fit (plist-get backend :fit)
                        :hide (lambda (_b) (signal condition '("secondary")))))))
          (if launch-fails
              (should (equal (keymap-popup-ownership--failure
                              (lambda () (keymap-popup (keymap-popup-ownership--map))))
                             '(error "primary")))
            (keymap-popup (keymap-popup-ownership--map))
            (should (equal (keymap-popup-ownership--failure #'keymap-popup-dismiss)
                           (list condition "secondary"))))
          (should-not (buffer-live-p buf))
          (keymap-popup-ownership--clean-p))))))

(defun keymap-popup-ownership--repurpose (buf kind)
  "Replace BUF with a foreign draft through native ownership change KIND."
  (with-current-buffer buf
    (if (eq kind 'mode)
        (text-mode)
      (set-visited-file-name "/nonexistent/keymap-popup-owned" t))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Foreign draft"))))

(defun keymap-popup-ownership--foreign-p (buf kind)
  "Assert that BUF retains the foreign draft and ownership change KIND."
  (should (buffer-live-p buf))
  (with-current-buffer buf
    (should (equal (buffer-string) "Foreign draft"))
    (should (buffer-modified-p))
    (if (eq kind 'mode)
        (should (eq major-mode 'text-mode))
      (should (equal buffer-file-name "/nonexistent/keymap-popup-owned")))
    (should-not keymap-popup--session)
    (dolist (hook '(kill-buffer-hook change-major-mode-hook
                   after-set-visited-file-name-hook))
      (should-not (memq #'keymap-popup--kill-buffer-cleanup
                        (symbol-value hook)))))
  (should-not keymap-popup--buffer)
  (keymap-popup-ownership--clean-p))

(ert-deftest keymap-popup-ownership-hide-repurpose ()
  (dolist (kind '(mode file))
    (dolist (condition '(nil error quit))
      (dolist (launch-failure '(nil error quit))
        (keymap-popup-ownership--isolated
          (let* ((buf nil)
                 (hides 0)
                 (keymap-popup-persistent t)
                 (keymap-popup-backend
                  (lambda ()
                    (list :show (lambda (b)
                                  (setq buf b)
                                  (when launch-failure
                                    (signal launch-failure '("primary"))))
                          :fit #'ignore
                          :hide (lambda (b)
                                  (setq hides (1+ hides))
                                  (keymap-popup-ownership--repurpose b kind)
                                  (when condition
                                    (signal condition '("hide failed"))))))))
            (if launch-failure
                (should (equal (keymap-popup-ownership--failure
                                (lambda () (keymap-popup (keymap-popup-ownership--map))))
                               (list launch-failure "primary")))
              (keymap-popup (keymap-popup-ownership--map))
              (keymap-popup--push-submenu buf (keymap-popup-ownership--map))
              (should (equal (keymap-popup-ownership--failure #'keymap-popup-dismiss)
                             (and condition (list condition "hide failed")))))
            (should (= hides 1))
            (keymap-popup-ownership--foreign-p buf kind)))))))

(ert-deftest keymap-popup-ownership-submenu-repurpose ()
  (dolist (kind '(mode file))
    (dolist (condition '(error quit))
      (keymap-popup-ownership--isolated
        (let* ((fail nil)
               (root (keymap-popup-ownership--map))
               (child (keymap-popup-ownership--map))
               (keymap-popup-persistent t)
               (keymap-popup-backend
                (lambda ()
                  (list :show #'ignore :hide #'ignore
                        :fit (lambda (buf)
                               (when fail
                                 (keymap-popup-ownership--repurpose buf kind)
                                 (signal condition '("fit failed"))))))))
          (keymap-set root "s" child)
          (keymap-popup-attach root (list "s" (list "Submenu" :keymap child)))
          (keymap-popup root)
          (let ((buf (keymap-popup--popup-buffer)))
            (setq fail t)
            (should (equal (keymap-popup-ownership--failure
                            (lambda () (call-interactively (key-binding (kbd "s")))))
                           (list condition "fit failed")))
            (keymap-popup-ownership--foreign-p buf kind)))))))

(ert-deftest keymap-popup-ownership-submenu-rollback ()
  (dolist (condition '(error quit))
    (keymap-popup-ownership--isolated
      (let* ((fail nil)
             (keymap-popup-backend
              (lambda ()
                (list :show #'ignore :hide #'ignore
                      :fit (lambda (_buf)
                             (when fail (signal condition '("fit failed"))))))))
        (keymap-popup (keymap-popup-ownership--map))
        (let* ((buf (keymap-popup--popup-buffer))
               (session (keymap-popup--session-state buf))
               (map overriding-terminal-local-map)
               (hooks (copy-sequence pre-command-hook))
               (content (with-current-buffer buf (buffer-string)))
               (position (with-current-buffer buf (goto-char 3))))
          (setq fail t)
          (should (equal (keymap-popup-ownership--failure
                          (lambda () (keymap-popup--push-submenu
                                      buf (keymap-popup-ownership--map))))
                         (list condition "fit failed")))
          (should (eq (keymap-popup--session-state buf) session))
          (should (eq (keymap-popup--popup-buffer) buf))
          (should (eq overriding-terminal-local-map map))
          (should (equal pre-command-hook hooks))
          (with-current-buffer buf
            (should (equal-including-properties (buffer-string) content))
            (should (= (point) position)))
          (keymap-popup-dismiss)
          (should-not (buffer-live-p buf))
          (keymap-popup-ownership--clean-p))))))

(ert-deftest keymap-popup-ownership-render-failure ()
  (dolist (condition '(error quit))
    (keymap-popup-ownership--isolated
      (let* ((map (keymap-popup-ownership--map))
             (hidden nil)
             (keymap-popup-backend
              (lambda () (list :show #'ignore :fit #'ignore
                               :hide (lambda (buf) (setq hidden buf))))))
        (keymap-popup-attach
         map (list "a" (list (lambda () (signal condition '("description")))
                             'ignore)))
        (should (equal (keymap-popup-ownership--failure (lambda () (keymap-popup map)))
                       (list condition "description")))
        (should hidden)
        (should-not (buffer-live-p hidden))
        (keymap-popup-ownership--clean-p)))))

(ert-deftest keymap-popup-ownership-prepare-failure ()
  (keymap-popup-ownership--isolated
    (let ((keymap-popup-buffer-parameters '((t . nil))))
      (should-error (keymap-popup (keymap-popup-ownership--map)))
      (should-not (get-buffer keymap-popup--buffer-name))
      (keymap-popup-ownership--clean-p))))

(ert-deftest keymap-popup-ownership-direct-kill-hide-failure ()
  (dolist (condition '(error quit))
    (keymap-popup-ownership--isolated
      (let ((keymap-popup-persistent t)
            (keymap-popup-backend
             (lambda () (list :show #'ignore :fit #'ignore
                              :hide (lambda (_buf) (signal condition '("hide")))))))
        (keymap-popup (keymap-popup-ownership--map))
        (let ((buf (get-buffer keymap-popup--buffer-name)))
          (kill-buffer buf)
          (should-not (buffer-live-p buf))
          (keymap-popup-ownership--clean-p))))))

(ert-deftest keymap-popup-ownership-repurpose-buffer ()
  (dolist (kind '(mode file))
    (keymap-popup-ownership--isolated
      (let ((keymap-popup-persistent t))
        (keymap-popup (keymap-popup-ownership--map))
        (let ((buf (get-buffer keymap-popup--buffer-name)))
          (with-current-buffer buf
            (if (eq kind 'mode)
                (text-mode)
              (set-visited-file-name "/nonexistent/keymap-popup-owned" t))
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "Repurposed text")))
          (keymap-popup-dismiss)
          (should (buffer-live-p buf))
          (should (equal (with-current-buffer buf (buffer-string)) "Repurposed text"))
          (should-not (keymap-popup--session-state buf))
          (keymap-popup-ownership--clean-p))))))

(ert-deftest keymap-popup-ownership-repurposed-during-show ()
  (keymap-popup-ownership--isolated
    (let* ((backend (keymap-popup-backend-side-window))
           (buf nil)
           (keymap-popup-backend
            (lambda ()
              (list :fit (plist-get backend :fit)
                    :hide (plist-get backend :hide)
                    :show (lambda (b)
                            (setq buf b)
                            (funcall (plist-get backend :show) b)
                            (with-current-buffer b
                              (text-mode)
                              (let ((inhibit-read-only t))
                                (erase-buffer)
                                (insert "New owner"))))))))
      (should-error (keymap-popup (keymap-popup-ownership--map)))
      (should (buffer-live-p buf))
      (should (equal (with-current-buffer buf (buffer-string)) "New owner"))
      (should-not (keymap-popup--session-state buf))
      (keymap-popup-ownership--clean-p))))

(ert-deftest keymap-popup-ownership-replacement ()
  (keymap-popup-ownership--isolated
    (keymap-popup (keymap-popup-ownership--map))
    (let ((first (get-buffer keymap-popup--buffer-name)))
      (with-current-buffer first (rename-buffer " *renamed-old-popup*" t))
      (keymap-popup (keymap-popup-ownership--map))
      (should-not (buffer-live-p first))
      (should (get-buffer keymap-popup--buffer-name))
      (keymap-popup-dismiss)
      (keymap-popup-ownership--clean-p))))

(ert-deftest keymap-popup-ownership-deleted-window ()
  (keymap-popup-ownership--isolated
    (keymap-popup (keymap-popup-ownership--map))
    (let* ((buf (get-buffer keymap-popup--buffer-name))
           (owned (get-buffer-window buf))
           (ordinary (selected-window)))
      (delete-window owned)
      (switch-to-buffer buf)
      (keymap-popup--refresh buf)
      (keymap-popup-dismiss)
      (should (window-live-p ordinary)))))

(ert-deftest keymap-popup-ownership-graphical-cross-frame ()
  (skip-unless (display-graphic-p))
  (keymap-popup-ownership--isolated
    (let ((other (make-frame '((visibility . nil) (no-accept-focus . t)
                               (no-focus-on-map . t)))))
      (unwind-protect
          (dolist (backend '(keymap-popup-backend-side-window
                             keymap-popup-backend-child-frame))
            (let ((keymap-popup-backend backend)
                  (origin (selected-frame)))
              (keymap-popup (keymap-popup-ownership--map))
              (let* ((buf (get-buffer keymap-popup--buffer-name))
                     (window (keymap-popup--session-get buf :window))
                     (child (keymap-popup--session-get buf :frame)))
                (with-selected-frame other
                  (let ((ordinary (selected-window)))
                    (switch-to-buffer buf)
                    (keymap-popup--refresh buf)
                    (keymap-popup-dismiss)
                    (should (window-live-p ordinary))))
                (should (frame-live-p origin))
                (should (frame-live-p other))
                (should-not (window-live-p window))
                (when child (should-not (frame-live-p child))))))
        (when (frame-live-p other) (delete-frame other t))))))

(ert-deftest keymap-popup-ownership-graphical-child-failures ()
  (skip-unless (display-graphic-p))
  (dolist (operation '(set-window-buffer fit-frame-to-buffer make-frame-visible))
    (dolist (condition '(error quit))
      (keymap-popup-ownership--isolated
        (let ((frames (frame-list))
              (native (symbol-function operation))
              (keymap-popup-backend #'keymap-popup-backend-child-frame))
          (cl-letf (((symbol-function operation)
                     (lambda (&rest args)
                       (if (and (cdr (frame-list))
                                (not (equal frames (frame-list))))
                           (signal condition '("child creation"))
                         (apply native args)))))
            (should (equal (keymap-popup-ownership--failure
                            (lambda () (keymap-popup (keymap-popup-ownership--map))))
                           (list condition "child creation"))))
          (should (equal (frame-list) frames))
          (should-not (get-buffer keymap-popup--buffer-name))
          (keymap-popup-ownership--clean-p))))))

(provide 'keymap-popup-ownership-tests)
;;; keymap-popup-ownership-tests.el ends here
