;;; keymap-popup-semantics-tests.el --- Native semantics -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Compare popup actions with the native command loop, not nested invocation.

;;; Code:

(require 'ert)
(require 'repeat)
(require 'keymap-popup)
(require 'keymap-popup-input-tests)
(require 'keymap-popup-mouse-tests)

(defvar keymap-popup-semantics--count 0)
(defvar keymap-popup-semantics--map)

(defun keymap-popup-semantics--action ()
  "Count one command invocation."
  (interactive)
  (cl-incf keymap-popup-semantics--count))

(ert-deftest keymap-popup-semantics-disabled ()
  (dolist (popup '(nil t))
    (keymap-popup-input--with-source
      (let* ((keymap-popup-semantics--count 0)
             (refused 0)
             (disabled-command-function (lambda (&rest _) (cl-incf refused)))
             (map (keymap-popup-input--suffix-map
                   #'keymap-popup-semantics--action nil :stay-open t)))
        (put 'keymap-popup-semantics--action 'disabled t)
        (unwind-protect
            (progn
              (use-local-map map)
              (when popup (keymap-popup map))
              (execute-kbd-macro (kbd "a"))
              (should (= refused 1))
              (should (= keymap-popup-semantics--count 0)))
          (put 'keymap-popup-semantics--action 'disabled nil))))))

(ert-deftest keymap-popup-semantics-repeat-exclusion ()
  (dolist (popup '(nil t))
    (keymap-popup-input--with-source
      (let* ((keymap-popup-semantics--count 0)
             (repeat-too-dangerous '(keymap-popup-semantics--action))
             (map (keymap-popup-input--suffix-map
                   #'keymap-popup-semantics--action nil :stay-open t)))
        (use-local-map map)
        (when popup (keymap-popup map))
        (should-error (execute-kbd-macro (kbd "a C-x z")) :type 'error)
        (should (= keymap-popup-semantics--count 1))))))

(ert-deftest keymap-popup-semantics-hooks ()
  (keymap-popup-input--with-source
    (let* ((observed nil)
           (map (keymap-popup-input--suffix-map
                 #'keymap-popup-semantics--action nil :stay-open t)))
      (keymap-popup map)
      (add-hook 'pre-command-hook
                (lambda () (push (list this-command real-this-command) observed)))
      (execute-kbd-macro (kbd "a"))
      (should (equal observed
                     '((keymap-popup-semantics--action
                        keymap-popup-semantics--action)))))))

(ert-deftest keymap-popup-semantics-named-macro ()
  (keymap-popup-input--with-source
    (fset 'keymap-popup-semantics--macro (kbd "x"))
    (unwind-protect
        (let ((map (keymap-popup-input--suffix-map
                    'keymap-popup-semantics--macro nil :stay-open t)))
          (keymap-popup map)
          (execute-kbd-macro (kbd "a"))
          (should (equal (buffer-string) "x")))
      (fmakunbound 'keymap-popup-semantics--macro))))

(ert-deftest keymap-popup-semantics-meta-collision ()
  (dolist (prefix '(27 24))
    (let ((meta-prefix-char prefix))
      (dolist (declaration '(keymap-popup-define keymap-popup-annotate))
        (let ((keymap-popup-semantics--map (make-sparse-keymap)))
          (keymap-set keymap-popup-semantics--map "M-h" #'forward-char)
          (keymap-popup-attach keymap-popup-semantics--map
                               '("M-h" ("Original" forward-char)))
          (let ((before (copy-keymap keymap-popup-semantics--map)))
            (should-error
             (eval `(,declaration keymap-popup-semantics--map
                      :popup-key "M-h" "a" ("Action" ignore)) t))
            (should (equal keymap-popup-semantics--map before))))))))

(ert-deftest keymap-popup-semantics-native-help ()
  (let ((keymap-popup-semantics--map (make-sparse-keymap)))
    (keymap-set keymap-popup-semantics--map "a" #'forward-char)
    (keymap-popup-attach keymap-popup-semantics--map
                         '("a" ("Action" forward-char)) :description "Title")
    (let ((text (substitute-command-keys "\\{keymap-popup-semantics--map}")))
      (should (string-match-p "forward-char" text))
      (should-not (string-match-p "<keymap-popup>\\|Keyboard Macro\\|:entries" text)))
    (should (equal (where-is-internal #'forward-char
                                    keymap-popup-semantics--map t) [97]))))

(ert-deftest keymap-popup-semantics-control-labels ()
  (keymap-popup-input--with-source
    (let ((map (make-sparse-keymap)))
      (keymap-set map "q" #'keymap-popup-semantics--action)
      (keymap-set map "C-u" #'keymap-popup-semantics--action)
      (keymap-set map "a" #'ignore)
      (keymap-popup-attach map '("q" ("Misleading quit" keymap-popup-semantics--action)
                                 "C-u" ("Misleading prefix" keymap-popup-semantics--action)
                                 "a" ("Action" ignore)))
      (keymap-popup map)
      (with-current-buffer (keymap-popup--popup-buffer)
        (should-not (string-match-p "Misleading" (buffer-string))))
      (let ((keymap-popup-semantics--count 0))
        (execute-kbd-macro (kbd "C-u q"))
        (should (= keymap-popup-semantics--count 0))
        (should-not (keymap-popup--popup-buffer))))))

(defun keymap-popup-semantics--control-hint (key label)
  "Assert that KEY and LABEL form an inert control hint in the popup."
  (with-current-buffer (keymap-popup--popup-buffer)
    (goto-char (point-min))
    (should (search-forward (concat key "  " label) nil t))
    (let ((end (point))
          (start (- (point) (length key) 2 (length label))))
      (dolist (property '(keymap-popup--entry mouse-face
                         help-echo keymap local-map))
        (should-not (text-property-not-all start end property nil))))))

(ert-deftest keymap-popup-semantics-exit-control-journey ()
  (dolist (persistent '(nil t))
    (dolist (custom '(nil t))
      (keymap-popup-input--with-source
        (let* ((keymap-popup-persistent persistent)
               (backend (keymap-popup-backend-side-window))
               (keymap-popup-backend
                (if custom
                    (lambda ()
                      (list :show (lambda (buf)
                                    (with-current-buffer buf
                                      (setq-local mode-line-format nil))
                                    (funcall (plist-get backend :show) buf))
                            :fit (plist-get backend :fit)
                            :hide (plist-get backend :hide)))
                  keymap-popup-backend))
               (child (make-sparse-keymap))
               (root (make-sparse-keymap)))
          (insert "Draft")
          (buffer-enable-undo)
          (insert " retained")
          (undo-boundary)
          (keymap-set child "<escape>" #'keymap-popup-dismiss)
          (keymap-set child "a" #'ignore)
          (keymap-popup-attach
           child '("<escape>" ("Dismiss menu" keymap-popup-dismiss)
                   "a" ("Child action" ignore :stay-open t))
           :exit-key "<escape>")
          (keymap-popup-attach root (list "c" (list "Child" :keymap child))
                               :exit-key "q")
          (let ((before (list (current-buffer) (selected-window) (point)
                              (buffer-string) (copy-tree buffer-undo-list)))
                (root-before (copy-keymap root))
                (child-before (copy-keymap child)))
            (keymap-popup root)
            (keymap-popup-semantics--control-hint "q" "Dismiss menu")
            (when custom
              (should-not (buffer-local-value
                           'mode-line-format (keymap-popup--popup-buffer))))
            (execute-kbd-macro
             (keymap-popup-mouse-test--click
              (keymap-popup-mouse-test--position "Dismiss menu")))
            (should (keymap-popup--popup-buffer))
            (execute-kbd-macro (kbd "c"))
            (keymap-popup-semantics--control-hint "<escape>" "Back")
            (with-current-buffer (keymap-popup--popup-buffer)
              (should-not (string-match-p "Dismiss menu" (buffer-string))))
            (execute-kbd-macro (kbd "a"))
            (keymap-popup-semantics--control-hint "<escape>" "Back")
            (execute-kbd-macro
             (keymap-popup-mouse-test--click
              (keymap-popup-mouse-test--position "Back")))
            (keymap-popup-semantics--control-hint "<escape>" "Back")
            (execute-kbd-macro (kbd "<escape>"))
            (keymap-popup-semantics--control-hint "q" "Dismiss menu")
            (execute-kbd-macro (kbd "q"))
            (should-not (keymap-popup--popup-buffer))
            (should (equal root root-before))
            (should (equal child child-before))
            (should (equal before
                           (list (current-buffer) (selected-window) (point)
                                 (buffer-string) buffer-undo-list)))))))))

(ert-deftest keymap-popup-semantics-exit-control-width ()
  (keymap-popup-input--with-source
    (keymap-popup (keymap-popup-input--suffix-map #'ignore nil :stay-open t))
    (let* ((buf (keymap-popup--popup-buffer))
           (columns (keymap-popup--rows-to-columns
                     (keymap-popup--active-descriptions buf))))
      (dolist (width '(12 24 80))
        (cl-letf (((symbol-function 'window-body-width)
                   (lambda (&rest _) (1+ width))))
          (keymap-popup--write-rendered buf columns))
        (with-current-buffer buf
          (let ((content (buffer-string))
                (actions (keymap-popup--render-columns columns width)))
            ;; Navigation must not widen or rearrange existing action columns.
            (should (equal actions (substring content 0 (length actions))))
            (dolist (line (split-string content "\n"))
              (should (<= (string-width line) width)))))))
    (execute-kbd-macro (kbd "q"))
    (should-not (keymap-popup--popup-buffer))))

(ert-deftest keymap-popup-semantics-exit-control-prefix-conflict ()
  (dolist (exit '("C-u" "C-u x"))
    (keymap-popup-input--with-source
      (let ((map (make-sparse-keymap)) (observed nil))
        (keymap-set map "a" (lambda (arg) (interactive "P") (setq observed arg)))
        (keymap-popup-attach map (list "a" (list "Action" (keymap-lookup map "a")
                                               :stay-open t))
                             :exit-key exit)
        (keymap-popup map)
        (with-current-buffer (keymap-popup--popup-buffer)
          (should-not (string-match-p "Dismiss menu" (buffer-string))))
        ;; The native prefix override wins over these configured exits.
        (execute-kbd-macro (kbd "C-u a"))
        (should (equal observed '(4)))
        (should (keymap-popup--popup-buffer))))))

(ert-deftest keymap-popup-semantics-repeat-positive ()
  (keymap-popup-input--with-source
    (let ((keymap-popup-semantics--count 0))
      (keymap-popup (keymap-popup-input--suffix-map
                     #'keymap-popup-semantics--action nil :stay-open t))
      (execute-kbd-macro (kbd "a C-x z"))
      (should (= keymap-popup-semantics--count 2)))))

(ert-deftest keymap-popup-semantics-remap-prefix-hooks ()
  (keymap-popup-input--with-source
    (let* ((observed nil)
           (hook-command nil)
           (target (lambda (arg)
                     (interactive "P")
                     (setq observed (list arg this-command real-this-command))))
           (map (keymap-popup-input--suffix-map #'forward-char nil :stay-open t)))
      (keymap-set map "<remap> <forward-char>" target)
      (keymap-popup map)
      (add-hook 'pre-command-hook
                (lambda ()
                  (when (equal (this-command-keys-vector) [97])
                    (setq hook-command this-command))))
      (execute-kbd-macro (kbd "C-u - 3 a"))
      (should (eq hook-command target))
      (should (equal observed (list -3 target target)))
      (should (keymap-popup--popup-buffer)))))

(ert-deftest keymap-popup-semantics-copy-composition ()
  (keymap-popup-input--with-source
    (let* ((calls 0)
           (command (lambda () (interactive) (cl-incf calls)))
           (original (keymap-popup-input--suffix-map command nil :stay-open t))
           (copy (copy-keymap original))
           (child (make-sparse-keymap))
           (composed (make-composed-keymap (list child copy))))
      (set-keymap-parent child copy)
      (should (eq (keymap-lookup copy "a") command))
      (should (eq (plist-get
                   (keymap-popup--find-entry-by-key
                    (keymap-popup--collect-descriptions copy) "a") :command)
                  command))
      (keymap-popup-attach original '("a" ("Reattached" ignore)))
      (keymap-popup composed)
      (with-current-buffer (keymap-popup--popup-buffer)
        (should (string-match-p "Action" (buffer-string)))
        (should-not (string-match-p "Reattached" (buffer-string))))
      (execute-kbd-macro (kbd "a q"))
      (should (= calls 1))
      (keymap-popup-remove-entry copy "a")
      (should-not (keymap-lookup copy "a"))
      (should (eq (keymap-lookup original "a") command)))))

(ert-deftest keymap-popup-semantics-separated-exit-action ()
  (keymap-popup-input--with-source
    (let ((map (make-sparse-keymap))
          (keymap-popup-semantics--count 0))
      (keymap-set map "q" #'keymap-popup-semantics--action)
      (keymap-popup-attach map '("q" ("Real quit" keymap-popup-semantics--action))
                           :exit-key "C-g")
      (keymap-popup map)
      (with-current-buffer (keymap-popup--popup-buffer)
        (should (string-match-p "Real quit" (buffer-string))))
      (execute-kbd-macro (kbd "q"))
      (should (= keymap-popup-semantics--count 1))
      (should-not (keymap-popup--popup-buffer)))))

(ert-deftest keymap-popup-semantics-control-prefix-and-alias ()
  (dolist (exit '("q" "ESC q"))
    (keymap-popup-input--with-source
      (let ((map (make-sparse-keymap)))
        (keymap-set map (concat exit " a") #'forward-char)
        (keymap-set map "C-u a" #'backward-char)
        (keymap-set map "x" #'ignore)
        (keymap-popup-attach
         map (list (concat exit " a") '("Shadowed exit prefix" forward-char)
                   "C-u a" '("Shadowed universal prefix" backward-char)
                   "x" '("Visible" ignore))
         :exit-key exit)
        (keymap-popup map)
        (with-current-buffer (keymap-popup--popup-buffer)
          (should-not (string-match-p "Shadowed" (buffer-string)))
          (should (string-match-p "Visible" (buffer-string))))
        (execute-kbd-macro (key-parse exit))
        (should-not (keymap-popup--popup-buffer))))))

(defvar keymap-popup-semantics--seen nil)
(defvar keymap-popup-semantics--switch nil)
(keymap-popup-define keymap-popup-semantics--switch-map
  "a" ("Action" :switch keymap-popup-semantics--switch))

(defun keymap-popup-semantics--record (arg)
  "Record ARG and native command identities."
  (interactive "P")
  (push (list arg this-command real-this-command this-original-command)
        keymap-popup-semantics--seen))

(defun keymap-popup-semantics--second ()
  "Reject an incorrectly chained remapping."
  (interactive)
  (ert-fail "Native remapping ran twice"))

(ert-deftest keymap-popup-semantics-remap-once ()
  (dolist (mode '(direct close persistent stay inapt switch))
    (dolist (mouse '(nil t))
      (unless (and mouse (eq mode 'direct))
        (dolist (chain '(nil t))
          (keymap-popup-input--with-source
            (let* ((keymap-popup-semantics--seen nil)
                   (hooks nil)
                   (map (if (eq mode 'switch)
                            (copy-keymap keymap-popup-semantics--switch-map)
                          (keymap-popup-input--suffix-map
                           #'forward-char (eq mode 'persistent)
                           :stay-open (eq mode 'stay)
                           :inapt-if (and (eq mode 'inapt) (lambda () nil)))))
                   (original (keymap-lookup map "a" nil t)))
              (keymap-set map (format "<remap> <%s>" original)
                          #'keymap-popup-semantics--record)
              (when chain
                (keymap-set map "<remap> <keymap-popup-semantics--record>"
                            #'keymap-popup-semantics--second))
              (use-local-map map)
              (unless (eq mode 'direct) (keymap-popup map))
              (add-hook 'pre-command-hook
                        (lambda ()
                          (when (equal (this-command-keys-vector) [97])
                            (push (list this-command real-this-command
                                        this-original-command) hooks))))
              (execute-kbd-macro
               (vconcat (kbd "C-u - 3")
                        (if mouse
                            (keymap-popup-mouse-test--click
                             (keymap-popup-mouse-test--position "Action"))
                          (kbd "a"))))
              (should (equal keymap-popup-semantics--seen
                             (list (list -3 'keymap-popup-semantics--record
                                         'keymap-popup-semantics--record original))))
              (should (equal hooks
                             (list (list 'keymap-popup-semantics--record
                                         'keymap-popup-semantics--record original)))))))))))

(ert-deftest keymap-popup-semantics-unannotated-remap-once ()
  (keymap-popup-input--with-source
    (let* ((keymap-popup-semantics--seen nil)
           (map (keymap-popup-input--suffix-map #'ignore t :stay-open t)))
      (keymap-set map "<remap> <forward-char>" #'keymap-popup-semantics--record)
      (keymap-set map "<remap> <keymap-popup-semantics--record>"
                  #'keymap-popup-semantics--second)
      (keymap-popup map)
      ;; The installed override must yield to an undescribed live replacement.
      (keymap-set map "a" #'forward-char)
      (execute-kbd-macro (kbd "C-u - 3 a"))
      (should (equal keymap-popup-semantics--seen
                     '((-3 keymap-popup-semantics--record
                           keymap-popup-semantics--record forward-char)))))))

(ert-deftest keymap-popup-semantics-retained-handler-remaps-once ()
  (keymap-popup-input--with-source
    (let* ((keymap-popup-semantics--seen nil)
           (map (keymap-popup-input--suffix-map #'forward-char nil :stay-open t)))
      (keymap-set map "<remap> <forward-char>" #'keymap-popup-semantics--record)
      (keymap-set map "<remap> <keymap-popup-semantics--record>"
                  #'keymap-popup-semantics--second)
      (use-local-map map)
      (keymap-popup map)
      (let* ((wrapper (keymap-popup--active-get (keymap-popup--popup-buffer) :wrapper-map))
             (handler (nth 2 (keymap-popup--raw-local-event-binding wrapper ?a)))
             (current-prefix-arg -3))
        (keymap-popup-dismiss)
        (call-interactively handler)
        (should (equal keymap-popup-semantics--seen
                       '((-3 keymap-popup-semantics--record
                             keymap-popup-semantics--record forward-char))))))))

(defun keymap-popup-semantics--options (map)
  "Return effective popup options of MAP."
  (mapcar (lambda (property) (keymap-popup--meta map property))
          '(description exit-key persistent)))

(defun keymap-popup-semantics--parent ()
  "Return a parent with nondefault options and a real q action."
  (let ((map (make-sparse-keymap)))
    (keymap-set map "q" #'keymap-popup-semantics--action)
    (keymap-set map "a" #'ignore)
    (keymap-popup-attach
     map '("q" ("Queue" keymap-popup-semantics--action) "a" ("Other" ignore))
     :description "Parent" :exit-key "C-g" :persistent t)))

(ert-deftest keymap-popup-semantics-inherited-options ()
  (dolist (mutation '(add remove))
    (keymap-popup-input--with-source
      (let* ((keymap-popup-semantics--count 0)
             (parent (keymap-popup-semantics--parent))
             (before (copy-keymap parent))
             (child (make-sparse-keymap)))
        (set-keymap-parent child parent)
        (if (eq mutation 'add)
            (keymap-popup-add-entry child "b" "Added" #'forward-char)
          (keymap-popup-remove-entry child "a"))
        (should (equal (keymap-popup-semantics--options child) '("Parent" "C-g" yes)))
        (should (equal parent before))
        (keymap-popup child)
        (with-current-buffer (keymap-popup--popup-buffer)
          (should (string-match-p "Queue" (buffer-string))))
        (execute-kbd-macro (kbd "q"))
        (should (= keymap-popup-semantics--count 1))
        (should (keymap-popup--popup-buffer))
        (condition-case nil (execute-kbd-macro (kbd "C-g")) (quit nil))
        (keymap-popup-input--clean-p)))))

(ert-deftest keymap-popup-semantics-composed-options-and-nil ()
  (let* ((parent (keymap-popup-semantics--parent))
         (first (make-sparse-keymap))
         (composed (make-composed-keymap (list first parent))))
    (keymap-set first "b" #'forward-char)
    (keymap-popup-attach first '("b" ("First" forward-char)))
    (should (equal (keymap-popup-semantics--options composed) '("Parent" "C-g" yes)))
    (keymap-popup-attach first '("b" ("First" forward-char))
                         :description "Local" :exit-key "x" :persistent nil)
    (let ((copy (copy-keymap composed)))
      (keymap-popup-add-entry first "c" "Added" #'backward-char)
      (keymap-popup-remove-entry first "b")
      (should (equal (keymap-popup-semantics--options composed) '("Local" "x" no)))
      (should (equal (keymap-popup-semantics--options parent) '("Parent" "C-g" yes)))
      (should-not (keymap-lookup copy "c"))
      (should (eq (keymap-lookup copy "b") #'forward-char))
      (keymap-popup-input--with-source
        (let ((keymap-popup-persistent t))
          (keymap-popup composed)
          (should-not (keymap-popup--session-get (keymap-popup--popup-buffer) :persistent))
          (execute-kbd-macro (kbd "q"))
          (keymap-popup-input--clean-p))))))

(ert-deftest keymap-popup-semantics-predicate-failure-recovery ()
  (dolist (condition '(error quit))
    (dolist (exit '("q" "C-g"))
      (keymap-popup-input--with-source
        (let* ((fail nil)
               (keymap-popup-semantics--count 0)
               (map (keymap-popup-input--suffix-map
                     #'keymap-popup-semantics--action nil :stay-open t
                     :inapt-if (lambda () (when fail (signal condition '("Predicate")))))))
          (keymap-popup map)
          (setq fail t)
          (let ((failure
                 (let ((debug-on-error nil) (debug-on-quit nil))
                   (condition-case err (execute-kbd-macro (kbd "a"))
                     ((error quit) err)))))
            (should (equal failure (list condition "Predicate"))))
          (should (= keymap-popup-semantics--count 0))
          (setq fail nil)
          (execute-kbd-macro (kbd "a"))
          (should (= keymap-popup-semantics--count 1))
          (condition-case nil (execute-kbd-macro (kbd exit)) (quit nil))
          (keymap-popup-input--clean-p))))))

(ert-deftest keymap-popup-semantics-predicate-failure-preserves-successor ()
  (dolist (condition '(error quit))
    (keymap-popup-input--with-source
      (let* ((fail nil)
             (keymap-popup-semantics--count 0)
             (successor (keymap-popup-input--suffix-map
                         #'keymap-popup-semantics--action nil :stay-open t))
             (map (keymap-popup-input--suffix-map
                   #'keymap-popup-semantics--action nil :stay-open t
                   :inapt-if (lambda ()
                               (when fail
                                 (setq fail nil)
                                 (keymap-popup successor)
                                 (signal condition '("Reentrant predicate"))))))
             (hook (lambda ()
                     (when (equal (this-command-keys-vector) [97])
                       (setq fail t)))))
        (keymap-popup map)
        ;; Change policy only after lookup, inside the native keep predicate.
        (add-hook 'pre-command-hook hook -95)
        (let ((failure
               (let ((debug-on-error nil) (debug-on-quit nil))
                 (condition-case err (execute-kbd-macro (kbd "a"))
                   ((error quit) err)))))
          (should (equal failure (list condition "Reentrant predicate"))))
        (remove-hook 'pre-command-hook hook)
        (should (= keymap-popup-semantics--count 0))
        (should (eq (keymap-popup--active-get (keymap-popup--popup-buffer) :keymap) successor))
        (execute-kbd-macro (kbd "a q"))
        (should (= keymap-popup-semantics--count 1))
        (keymap-popup-input--clean-p)))))

(ert-deftest keymap-popup-semantics-early-inapt-transition ()
  (dolist (group '(nil t))
    (dolist (persistent '(nil t))
      (dolist (exit '("q" "C-g"))
        (keymap-popup-input--with-source
          (let* ((inapt nil) (seen nil) (observed nil) (attempts 0)
                 (action (lambda (arg) (interactive "P") (push arg seen)))
                 (predicate (lambda () inapt))
                 (map (make-sparse-keymap))
                 (hook (lambda ()
                         (when (equal (this-command-keys-vector) [97])
                           (setq inapt (= (cl-incf attempts) 1)))))
                 (after (lambda ()
                          (when (equal (this-command-keys-vector) [97])
                            (push (list seen (keymap-popup--session-get
                                              (keymap-popup--popup-buffer) :prefix-mode))
                                  observed)))))
            (keymap-set map "a" action)
            (keymap-popup-attach
             map (if group
                     (list :group (list "Guarded" :inapt-if predicate)
                           "a" (list "Action" action :stay-open (not persistent)))
                   (list "a" (list "Action" action :stay-open (not persistent)
                                   :inapt-if predicate)))
             :persistent persistent)
            (keymap-popup map)
            (add-hook 'pre-command-hook hook -95)
            (add-hook 'post-command-hook after)
            ;; A second execute-kbd-macro would reset the native prefix.
            (execute-kbd-macro (kbd "C-u - 3 a a"))
            (remove-hook 'pre-command-hook hook)
            (remove-hook 'post-command-hook after)
            (should (equal (reverse observed) '((nil t) ((-3) nil))))
            (should (equal seen '(-3)))
            (condition-case nil (execute-kbd-macro (kbd exit)) (quit nil))
            (keymap-popup-input--clean-p)))))))

(ert-deftest keymap-popup-semantics-lookup-failure-captured ()
  (dolist (condition '(error quit))
    (dolist (one-shot '(nil t))
      (dolist (successor-p '(nil t))
        (dolist (exit '("q" "C-g"))
          (keymap-popup-input--with-source
            (let* ((armed nil) (calls 0) (effects 0) (next-effects 0)
                   (successor (keymap-popup-input--suffix-map
                               (lambda () (interactive) (cl-incf next-effects))
                               nil :stay-open t))
                   (map (keymap-popup-input--suffix-map
                         (lambda () (interactive) (cl-incf effects)) nil :stay-open t
                         :inapt-if (lambda ()
                                     (when armed
                                       (cl-incf calls)
                                       (when one-shot (setq armed nil))
                                       (when successor-p (keymap-popup successor))
                                       (signal condition '("Lookup failure" 17)))))))
              (keymap-popup map)
              (setq armed t)
              (let ((failure
                     (let ((debug-on-error nil) (debug-on-quit nil))
                       (condition-case err (execute-kbd-macro (kbd "a"))
                         ((error quit) err)))))
                (should (equal failure (list condition "Lookup failure" 17))))
              (should (= effects 0))
              (should (= calls 1))
              (should (eq (keymap-popup--active-get (keymap-popup--popup-buffer) :keymap)
                          (if successor-p successor map)))
              (setq armed nil)
              (execute-kbd-macro (kbd "a"))
              (should (= effects (if successor-p 0 1)))
              (should (= next-effects (if successor-p 1 0)))
              (condition-case nil (execute-kbd-macro (kbd exit)) (quit nil))
              (keymap-popup-input--clean-p))))))))

(ert-deftest keymap-popup-semantics-composed-public-mutation ()
  (dolist (full '(nil t))
    (dolist (nested '(nil t))
      (dolist (symbolic '(nil t))
        (dolist (mutation '(add remove attach))
          (dolist (global '(nil t))
            (keymap-popup-input--with-source
              (let* ((keymap-popup-persistent global)
                     (keymap-popup-semantics--count 0)
                     (first (if full (make-keymap) (make-sparse-keymap)))
                     (later (keymap-popup-semantics--parent))
                     (parent (keymap-popup-semantics--parent))
                     (inner (make-composed-keymap (list first later) parent))
                     (composed (if nested (make-composed-keymap (list inner later) parent)
                                 inner)))
                (keymap-set first "q" #'keymap-popup-semantics--action)
                (keymap-set first "a" #'ignore)
                (keymap-popup-attach
                 first '("q" ("Queue" keymap-popup-semantics--action) "a" ("Other" ignore))
                 :description "First" :exit-key "C-g" :persistent nil)
                (let* ((before (copy-keymap composed))
                       (copy (copy-keymap composed))
                       (destination (if nested (cadr (cadr copy)) (cadr copy)))
                       (name (make-symbol "keymap-popup-test-composed"))
                       (map (if symbolic (progn (fset name copy) name) copy)))
                  (pcase mutation
                    ('add (keymap-popup-add-entry map "b" "Added" #'ignore))
                    ('remove (keymap-popup-remove-entry map "a"))
                    ('attach (keymap-popup-attach
                              map '("q" ("Queue" keymap-popup-semantics--action))
                              :description "Attached" :exit-key "C-g" :persistent nil)))
                  (should (equal composed before))
                  (should (eq destination (if nested (cadr (cadr copy)) (cadr copy))))
                  (let ((options (list (if (eq mutation 'attach) "Attached" "First") "C-g" 'no)))
                    (should (equal (keymap-popup-semantics--options map) options))
                    (should (equal (keymap-popup-semantics--options destination) options)))
                  (should (eq (keymap-lookup map "q") #'keymap-popup-semantics--action))
                  (keymap-popup map)
                  (with-current-buffer (keymap-popup--popup-buffer)
                    (should (string-match-p "Queue" (buffer-string))))
                  (execute-kbd-macro (kbd "q"))
                  (should (= keymap-popup-semantics--count 1))
                  (keymap-popup-input--clean-p))))))))))

(ert-deftest keymap-popup-semantics-composed-attachment-destination ()
  (let* ((first (make-keymap))
         (later (keymap-popup-semantics--parent))
         (parent (keymap-popup-semantics--parent))
         (map (make-composed-keymap
               (list (make-composed-keymap (list first later)) later) parent))
         (later-before (copy-keymap later))
         (parent-before (copy-keymap parent)))
    (keymap-set first "a" #'ignore)
    (keymap-popup-attach map '("a" ("Action" ignore))
                         :description "Title" :exit-key "C-g" :persistent nil)
    (should (equal (keymap-popup-semantics--options first) '("Title" "C-g" no)))
    (should (keymap-popup--meta first 'descriptions))
    (should (equal later later-before))
    (should (equal parent parent-before))
    (keymap-popup-attach map '("a" ("Reattached" ignore)))
    (should-not (keymap-popup--meta first 'persistent))
    (should-not (keymap-popup--meta first 'description))
    (should-not (keymap-popup--meta first 'exit-key))
    (should (keymap-popup--meta first 'descriptions))
    (should (equal (keymap-popup-semantics--options map) '("Parent" "C-g" yes)))))

(ert-deftest keymap-popup-semantics-early-refusal-not-repeat-target ()
  (keymap-popup-input--with-source
    (let* ((inapt nil) (effects 0)
           (map (keymap-popup-input--suffix-map
                 (lambda () (interactive) (cl-incf effects)) nil :stay-open t
                 :inapt-if (lambda () inapt)))
           (hook (lambda ()
                   (when (equal (this-command-keys-vector) [97])
                     (setq inapt t)))))
      (keymap-popup map)
      (add-hook 'pre-command-hook hook -95)
      ;; C-e retires repeat's own transient map before checking popup cleanup.
      (execute-kbd-macro (kbd "a C-x z C-e"))
      (remove-hook 'pre-command-hook hook)
      (should (= effects 0))
      (keymap-popup-input--clean-p))))

(ert-deftest keymap-popup-semantics-lookup-failure-before-later-policy ()
  (keymap-popup-input--with-source
    (let* ((armed nil) (inapt nil) (effects 0)
           (map (keymap-popup-input--suffix-map
                 (lambda () (interactive) (cl-incf effects)) nil :stay-open t
                 :inapt-if (lambda ()
                             (if armed
                                 (progn (setq armed nil)
                                        (signal 'error '("First failure")))
                               inapt))))
           (hook (lambda () (setq inapt t))))
      (keymap-popup map)
      (setq armed t)
      (add-hook 'pre-command-hook hook -95)
      (should (equal (let ((debug-on-error nil))
                       (condition-case err (execute-kbd-macro (kbd "a"))
                         (error err)))
                     '(error "First failure")))
      (remove-hook 'pre-command-hook hook)
      (should (= effects 0))
      (setq inapt nil)
      (execute-kbd-macro (kbd "a q"))
      (should (= effects 1))
      (keymap-popup-input--clean-p))))

(defvar keymap-popup-semantics--mouse-boundary 'lookup
  "Semantic boundary of the instrumented mouse replay.")

(defvar keymap-popup-semantics--mouse-guards 0
  "Number of replay guards observed by the current fixture.")

(defun keymap-popup-semantics--with-mouse-boundaries (function)
  "Call FUNCTION with scoped observations of real mouse replay boundaries.
Wrap the actual registered guard, qualification and native lookup functions;
do not infer their dynamic extent from compiler-dependent stack frames."
  (let* ((keymap-popup-semantics--mouse-boundary 'lookup)
         (guards nil)
         (guard-advice
          (lambda (original &rest args)
            (let ((keymap-popup-semantics--mouse-boundary 'replay))
              (apply original args))))
         (replay-advice
          (lambda (original &rest args)
            (let ((before (copy-sequence pre-command-hook)))
              (prog1 (apply original args)
                (dolist (guard (seq-difference pre-command-hook before #'eq))
                  (cl-incf keymap-popup-semantics--mouse-guards)
                  (push guard guards)
                  (advice-add guard :around guard-advice))))))
         (current-advice
          (lambda (original &rest args)
            (let ((keymap-popup-semantics--mouse-boundary
                   (if (eq keymap-popup-semantics--mouse-boundary 'replay)
                       'policy
                     keymap-popup-semantics--mouse-boundary)))
              (apply original args))))
         (binding-advice
          (lambda (original &rest args)
            (let ((keymap-popup-semantics--mouse-boundary
                   (pcase keymap-popup-semantics--mouse-boundary
                     ('policy 'binding)
                     ('replay 'final)
                     (other other))))
              (apply original args)))))
    (unwind-protect
        (progn
          (advice-add 'keymap-popup--mouse-replay :around replay-advice)
          (advice-add 'keymap-popup--mouse-current-p :around current-advice)
          (advice-add 'key-binding :around binding-advice)
          (funcall function))
      (advice-remove 'keymap-popup--mouse-replay replay-advice)
      (advice-remove 'keymap-popup--mouse-current-p current-advice)
      (advice-remove 'key-binding binding-advice)
      (dolist (guard guards)
        (advice-remove guard guard-advice)))))

(ert-deftest keymap-popup-semantics-mouse-failure-boundaries ()
  ;; Native event fixtures, not physical mouse input.  Arm after release so
  ;; each boundary belongs to the replay rather than initial click validation.
  (dolist (boundary '(lookup policy binding final))
    (dolist (property '(:if :inapt-if))
      (dolist (group '(nil t))
        (dolist (condition '(error quit))
          (dolist (sustained '(nil t))
            (dolist (ownership '(unchanged successor retired))
              (ert-info ((format "%S" (list boundary property group condition
                                            sustained ownership)))
                (keymap-popup-input--with-source
                  (let* ((armed nil) (calls 0) (effects 0) (next-effects 0)
                         (lifetime nil)
                         (reached (make-hash-table :test #'eq))
                         (keymap-popup-semantics--mouse-guards 0)
                         (action (lambda () (interactive) (cl-incf effects)))
                         (label (lambda () (format "Action count=%d" effects)))
                         (next-action (lambda () (interactive) (cl-incf next-effects)))
                         (next (make-sparse-keymap))
                         (map (make-sparse-keymap))
                         (predicate
                          (lambda ()
                            (when armed
                              (cl-incf (gethash keymap-popup-semantics--mouse-boundary
                                               reached 0)))
                            (when (and armed
                                       (eq keymap-popup-semantics--mouse-boundary boundary))
                              (cl-incf calls)
                              (unless sustained (setq armed nil))
                              (pcase ownership
                                ('successor (keymap-popup next))
                                ('retired (fundamental-mode)))
                              (signal condition (list "First mouse failure" calls)))
                            (eq property :if)))
                         (arm (lambda ()
                                (when (and (eq (car-safe last-input-event) 'mouse-1)
                                           unread-command-events)
                                  (setq armed t)))))
                    (keymap-set next "a" next-action)
                    (keymap-popup-attach
                     next (list "a" (list (lambda () (format "Next count=%d" next-effects))
                                          next-action :stay-open t)))
                    (keymap-set map "a" action)
                    (keymap-popup-attach
                     map (if group
                             (list :group (list "Group" property predicate)
                                   "a" (list label action :stay-open t))
                           (list "a" (list label action :stay-open t property predicate))))
                    (keymap-popup map)
                    (setq lifetime (keymap-popup--session-get
                                    (keymap-popup--popup-buffer) :source-live))
                    (add-hook 'post-command-hook arm)
                    (let ((failure
                           (let ((debug-on-error nil) (debug-on-quit nil))
                             (condition-case err
                                 (keymap-popup-semantics--with-mouse-boundaries
                                  (lambda ()
                                    (execute-kbd-macro
                                     (keymap-popup-mouse-test--click
                                      (keymap-popup-mouse-test--position "Action count=0")))))
                               ((error quit) err)))))
                      (setq armed nil)
                      (remove-hook 'post-command-hook arm)
                      (should (equal failure (list condition "First mouse failure" 1))))
                    (should (= keymap-popup-semantics--mouse-guards 1))
                    (should (= (gethash boundary reached 0) 1))
                    (should (= calls 1))
                    (should (= effects 0))
                    (should (= next-effects 0))
                    (should (eq (not (null (car lifetime)))
                                (not (eq ownership 'retired))))
                    (if (eq ownership 'retired)
                        (keymap-popup next)
                      (should (eq (keymap-popup--active-get
                                   (keymap-popup--popup-buffer) :keymap)
                                  (if (eq ownership 'successor) next map))))
                    (let* ((buf (keymap-popup--popup-buffer))
                           (refresh (keymap-popup--session-get buf :persistent-hook)))
                      (execute-kbd-macro (kbd "a"))
                      (should (memq refresh post-command-hook))
                      (with-current-buffer buf
                        (should (string-match-p "count=1" (buffer-string))))
                      (should (= (+ effects next-effects) 1))
                      (execute-kbd-macro (kbd "q"))
                      (keymap-popup-input--clean-p))))))))))))

(defun keymap-popup-semantics--refresh-failure-case
    (condition persistent mouse sustained kind &optional ownership)
  "Check first refresh CONDITION and recovery for one retained action.
PERSISTENT, MOUSE and SUSTAINED select dispatch and fault duration.
KIND selects the callback; OWNERSHIP selects its lifetime change."
  (ert-info ((format "%S" (list condition persistent mouse sustained kind ownership)))
    (keymap-popup-input--with-source
      (let* ((effects 0) (next-effects 0) (armed nil) (arm-action t) (calls 0)
             (next (keymap-popup-input--suffix-map
                    (lambda () (interactive) (cl-incf next-effects)) nil :stay-open t))
             (fault (lambda ()
                      (when armed
                        (cl-incf calls)
                        (unless sustained (setq armed nil))
                        (pcase ownership
                          ('dismiss (keymap-popup-dismiss))
                          ('successor (keymap-popup next))
                          ('retired (fundamental-mode)))
                        (signal condition (list "First refresh failure" calls)))))
             (action (lambda () (interactive)
                       (cl-incf effects)
                       (when arm-action (setq armed t))))
             (label (lambda ()
                      (when (eq kind 'description) (funcall fault))
                      (format "Live count=%d" effects)))
             (backend keymap-popup-backend)
             (keymap-popup-backend
              (lambda ()
                (let* ((value (funcall backend)) (fit (plist-get value :fit)))
                  (plist-put value :fit
                             (lambda (buf)
                               (when (eq kind 'backend) (funcall fault))
                               (when fit (funcall fit buf)))))))
             (map (make-sparse-keymap)))
        (keymap-set map "a" action)
        (keymap-popup-attach
         map (list "a" (list label action :stay-open t
                             :inapt-if (lambda ()
                                         (when (eq kind 'inapt) (funcall fault)))))
         :persistent persistent)
        (keymap-popup map)
        (let* ((buf (keymap-popup--popup-buffer))
               (refresh (keymap-popup--session-get buf :persistent-hook))
               (original-message (symbol-function 'message))
               (resolve (symbol-function 'keymap-popup--active-descriptions))
               messages)
          ;; Observe the native hook's diagnostic without allowing ERT failures
          ;; inside the production handler to be mistaken for the fixture fault.
          (cl-letf (((symbol-function 'keymap-popup--active-descriptions)
                     (lambda (popup)
                       (when (eq kind 'qualification) (funcall fault))
                       (funcall resolve popup)))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (when format-string
                         (push (apply #'format format-string args) messages))
                       (apply original-message format-string args))))
            (let ((debug-on-error nil) (debug-on-quit nil))
              (execute-kbd-macro
               (if mouse (keymap-popup-mouse-test--click
                          (keymap-popup-mouse-test--position "Live count=0"))
                 (kbd "a")))))
          (setq armed nil arm-action nil)
          (should (= calls 1))
          (should (= effects 1))
          (should (= next-effects 0))
          (should (= 1 (cl-count
                        (format "Popup refresh failed: %S"
                                (list condition "First refresh failure" 1))
                        messages :test #'equal)))
          (if (memq ownership '(dismiss successor))
              (should-not (memq refresh post-command-hook))
            (should (memq refresh post-command-hook)))
          (pcase ownership
            ((or 'dismiss 'retired) (keymap-popup next))
            ('successor
             (should (eq (keymap-popup--active-get
                          (keymap-popup--popup-buffer) :keymap) next))))
          (if ownership
              (progn
                (should-not (memq refresh post-command-hook))
                (execute-kbd-macro (kbd "a"))
                (should (= next-effects 1))
                (should (= effects 1)))
            (execute-kbd-macro (kbd "a"))
            (should (= effects 2))
            (should (memq refresh post-command-hook))
            (with-current-buffer buf
              (should (string-match-p "Live count=2" (buffer-string)))))
          (execute-kbd-macro (kbd "q"))
          (keymap-popup-input--clean-p))))))

(ert-deftest keymap-popup-semantics-first-refresh-failure-recovery ()
  ;; Arm only in the accepted action, never in prior lookup/replay/keep work.
  (dolist (condition '(error quit))
    (dolist (persistent '(nil t))
      (dolist (mouse '(nil t))
        (dolist (sustained '(nil t))
          (dolist (kind '(description inapt))
            (keymap-popup-semantics--refresh-failure-case
             condition persistent mouse sustained kind)))))))

(ert-deftest keymap-popup-semantics-first-refresh-failure-boundaries ()
  ;; Inject a resolver failure in nonpersistent qualification, before refresh.
  ;; Native menu filters can swallow their own errors; this tests an escaping
  ;; resolver fault.  Backend faults occur after rendering.  Neither failure
  ;; may resurrect retired hooks.
  (dolist (condition '(error quit))
    (dolist (kind '(qualification backend))
      (dolist (ownership '(nil dismiss successor retired))
        (keymap-popup-semantics--refresh-failure-case
         condition nil nil nil kind ownership)))))

(ert-deftest keymap-popup-semantics-symbol-component-write-owner ()
  ;; Use native define-key with the exact symbolic metadata event as oracle.
  ;; Its destination cell is then populated through public APIs; no duplicate
  ;; implementation of store_in_keymap participates in the expectation.
  (dolist (shape '(plain before between after symbols nested local))
    (dolist (full '(nil t))
      (dolist (symbol-root '(nil t))
        (dolist (copy-p '(nil t))
          (dolist (operation '(add remove attach))
            (ert-info ((format "%S" (list shape full symbol-root copy-p operation)))
              (keymap-popup-input--with-source
                (let* ((keymap-popup-persistent t)
                       (effects 0)
                       (action (lambda () (interactive) (cl-incf effects)))
                       (first (if full (make-keymap) (make-sparse-keymap)))
                       (later (make-sparse-keymap))
                       (parent (make-sparse-keymap))
                       (symbol-map (make-sparse-keymap))
                       (name (make-symbol "embedded"))
                       (root-name (make-symbol "root"))
                       (alias (make-symbol "alias"))
                       (raw (progn
                              (fset name symbol-map)
                              (pcase shape
                                ('plain first)
                                ('before (make-composed-keymap (list name first later)))
                                ('between (make-composed-keymap (list first name later)))
                                ('after (make-composed-keymap (list first later name)))
                                ('symbols (make-composed-keymap (list name name)))
                                ('nested (make-composed-keymap
                                          (list name (make-composed-keymap
                                                      (list name first name later)) later)))
                                ('local (list 'keymap (cons 'keymap-popup nil) name first)))))
                       (map (if copy-p (copy-keymap raw) raw)))
                  (keymap-set parent "p" #'ignore)
                  (set-keymap-parent raw parent)
                  (when copy-p (set-keymap-parent map parent))
                  (when symbol-root
                    (fset root-name map)
                    (fset alias root-name)
                    (setq map alias))
                  (let ((original (copy-keymap raw))
                        (symbol-before (copy-keymap symbol-map))
                        (parent-before (copy-keymap parent))
                        (marker (list 'menu-item "" nil)))
                    (define-key map [keymap-popup] marker)
                    ;; Locate the *actual* cons selected by native define-key.
                    ;; New Emacs copies menu-item lists, so compare their value.
                    (let* ((cell
                            (cl-labels ((locate (value)
                                          (when (consp value)
                                            (if (equal (cdr value) marker) value
                                              (or (locate (car value))
                                                  (locate (cdr value)))))))
                              (locate (if symbol-root (indirect-function map) map))))
                           (options '(description "Destination" exit-key "C-g" persistent no)))
                      (should cell)
                      (keymap-set map "q" action)
                      (keymap-set map "a" #'ignore)
                      (keymap-popup-attach
                       map (list "q" (list "Queue" action) "a" '("Other" ignore))
                       :description "Destination" :exit-key "C-g" :persistent nil)
                      (pcase operation
                        ('add (keymap-popup-add-entry map "b" "Added" #'ignore))
                        ('remove (keymap-popup-remove-entry map "a"))
                        ('attach
                         (keymap-popup-attach map (list "q" (list "Attached queue" action))
                                              :description "Attached" :exit-key "C-g"
                                              :persistent nil)
                         (setq options '(description "Attached" exit-key "C-g" persistent no))))
                      (dolist (prop '(description exit-key persistent))
                        (should (equal (plist-get (nthcdr 3 (cdr cell)) prop)
                                       (plist-get options prop))))
                      (should (plist-get (nthcdr 3 (cdr cell)) 'descriptions))
                      (should (equal symbol-map symbol-before))
                      (should (equal parent parent-before))
                      (when copy-p (should (equal (copy-keymap raw) original)))
                      (keymap-popup map)
                      (with-current-buffer (keymap-popup--popup-buffer)
                        (should (string-match-p (if (eq operation 'attach)
                                                    "Attached queue" "Queue")
                                                (buffer-string))))
                      (execute-kbd-macro (kbd "q"))
                      (should (= effects 1))
                      (keymap-popup-input--clean-p))))))))))))

(provide 'keymap-popup-semantics-tests)
;;; keymap-popup-semantics-tests.el ends here
