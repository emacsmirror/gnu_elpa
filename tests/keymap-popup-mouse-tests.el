;;; keymap-popup-mouse-tests.el --- Mouse activation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'keymap-popup)
(require 'keymap-popup-input-tests)

(defun keymap-popup-mouse-test--position (text)
  "Return a mouse position on TEXT in the current popup."
  (let* ((buf (keymap-popup--popup-buffer))
         (window (get-buffer-window buf)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (search-forward text)
        (list window (- (point) (length text)) '(2 . 1) 0)))))

(defun keymap-popup-mouse-test--click (position)
  "Return a complete single left click at POSITION."
  (vector (list 'down-mouse-1 position) (list 'mouse-1 position)))

(ert-deftest keymap-popup-mouse-test-native-action-and-prefix ()
  (dolist (stay '(nil t persistent))
    (keymap-popup-input--with-source
      (insert "Draft")
      (let* ((observed nil)
             (command (lambda (arg)
                        (interactive "P")
                        (push (list (current-buffer) (point) arg
                                    this-command real-this-command
                                    last-command-event (this-command-keys-vector))
                              observed)))
             (map (keymap-popup-input--suffix-map
                   command (eq stay 'persistent) :stay-open (eq stay t))))
        (keymap-popup map)
        (let ((position (keymap-popup-mouse-test--position "Action"))
              (expected-real (key-binding (kbd "a"))))
          (execute-kbd-macro
           (vconcat (kbd "C-u - 3") (keymap-popup-mouse-test--click position)))
          (should (= (length observed) 1))
          (should (equal (car observed)
                         (list source 6 -3 command expected-real ?a [97]))))
        (should (eq last-command command))
        (if stay
            (progn
              (should (keymap-popup--popup-buffer))
              (should-not (keymap-popup--session-get
                           (keymap-popup--popup-buffer) :prefix-mode))
              (execute-kbd-macro (kbd "q")))
          (should-not (keymap-popup--popup-buffer)))
        (keymap-popup-input--clean-p)))))

(ert-deftest keymap-popup-mouse-test-multiple-events-and-remapping ()
  (keymap-popup-input--with-source
    (let ((map (make-sparse-keymap)) (observed nil))
      (keymap-set map "C-c a" #'forward-char)
      (keymap-set map "<remap> <forward-char>"
                  (lambda () (interactive)
                    (setq observed (list last-command-event
                                         (this-command-keys-vector)))))
      (keymap-popup-attach map '("C-c a" ("Move" forward-char)))
      (keymap-popup map)
      (execute-kbd-macro
       (keymap-popup-mouse-test--click (keymap-popup-mouse-test--position "Move")))
      (should (equal observed (list ?a (key-parse "C-c a"))))
      (keymap-popup-input--clean-p))))

(ert-deftest keymap-popup-mouse-test-refuses-stale-and-inapt ()
  (dolist (change '(hidden inapt rebound removed))
    (keymap-popup-input--with-source
      (let* ((hidden nil) (inapt nil) (called nil)
             (command (lambda () (interactive) (setq called t)))
             (map (keymap-popup-input--suffix-map
                   command nil :stay-open t :if (lambda () (not hidden))
                   :inapt-if (lambda () inapt))))
        (keymap-popup map)
        (let ((position (keymap-popup-mouse-test--position "Action")))
          (pcase change
            ('hidden (setq hidden t))
            ('inapt (setq inapt t))
            ('rebound (keymap-set map "a" (lambda () (interactive) (setq called 'bad))))
            ('removed (keymap-unset map "a")))
          (execute-kbd-macro (keymap-popup-mouse-test--click position)))
        (should-not called)
        (should (keymap-popup--popup-buffer))
        (should (eq (current-buffer) source))
        (execute-kbd-macro (kbd "q"))
        (keymap-popup-input--clean-p)))))

(ert-deftest keymap-popup-mouse-test-background-double-and-drag ()
  (keymap-popup-input--with-source
    (let* ((called nil)
           (command (lambda (arg) (interactive "P") (push arg called)))
           (map (keymap-popup-input--suffix-map command nil :stay-open t)))
      (keymap-popup map)
      (let* ((position (keymap-popup-mouse-test--position "Action"))
             (background (list (car position) 1 '(0 . 0) 0)))
        (execute-kbd-macro
         (vconcat (kbd "C-u 3")
                  (keymap-popup-mouse-test--click background)
                  (vector (list 'down-mouse-1 background)
                          (list 'mouse-1 position)
                          (list 'drag-mouse-1 position background)
                          (list 'double-down-mouse-1 position)
                          (list 'double-mouse-1 position)
                          (list 'triple-down-mouse-1 position)
                          (list 'triple-mouse-1 position))
                  (keymap-popup-mouse-test--click position))))
      (should (equal called '(3)))
      (execute-kbd-macro (kbd "q"))
      (keymap-popup-input--clean-p))))

(ert-deftest keymap-popup-mouse-test-replay-revalidates-owner ()
  (dolist (change '(rebind retire replace source-window source-death))
    (keymap-popup-input--with-source
      (let* ((calls nil)
             (map (keymap-popup-input--suffix-map
                   (lambda () (interactive) (push 'old calls)) nil :stay-open t))
             (other (generate-new-buffer " *mouse-other*"))
             (other-map (keymap-popup-input--suffix-map
                         (lambda () (interactive) (push 'other calls)) nil)))
        (unwind-protect
            (progn
              (use-local-map other-map)
              (keymap-popup map)
              (let ((position (keymap-popup-mouse-test--position "Action"))
                    (post-command-hook
                     (list
                      (lambda ()
                        (when (eq (car-safe last-input-event) 'mouse-1)
                          (pcase change
                            ('rebind (keymap-set map "a"
                                               (lambda () (interactive)
                                                 (push 'new calls))))
                            ('retire (keymap-popup-dismiss))
                            ('replace (keymap-popup other-map))
                            ('source-window (switch-to-buffer other)
                                            (use-local-map other-map))
                            ('source-death (kill-buffer source)
                                           (switch-to-buffer other)
                                           (use-local-map other-map))))))))
                (execute-kbd-macro (keymap-popup-mouse-test--click position))
                (should-not calls)
                (should-not unread-command-events)
                (should-not (seq-some
                             (lambda (hook)
                               (and (symbolp hook)
                                    (equal (symbol-name hook)
                                           "keymap-popup--mouse-replay")))
                             pre-command-hook))))
          (kill-buffer other))))))

(ert-deftest keymap-popup-mouse-test-replay-discards-partial-sequence ()
  (keymap-popup-input--with-source
    (let* ((map (make-sparse-keymap)) (calls nil)
           (command (lambda () (interactive) (push t calls))))
      (keymap-set map "C-c x" command)
      (keymap-popup-attach map (list "C-c x" (list "Action" command)))
      (keymap-popup map)
      (let ((position (keymap-popup-mouse-test--position "Action"))
            (post-command-hook
             (list (lambda ()
                     (when (eq (car-safe last-input-event) 'mouse-1)
                       (keymap-set map "C-c" command))))))
        (execute-kbd-macro (keymap-popup-mouse-test--click position))
        (should-not calls)
        (should (equal (buffer-string) ""))
        (should-not unread-command-events)))))

(ert-deftest keymap-popup-mouse-test-property-boundaries ()
  (let* ((entry '(:key "a" :description "Action" :type suffix))
         (group (list :name "Heading" :entries (list entry)))
         (rendered (keymap-popup--render (list (list group)) nil 12)))
    (should-not (get-text-property 0 'keymap-popup--entry rendered))
    (should (equal (get-text-property (string-match "a  " rendered)
                                      'keymap-popup--entry rendered) entry))
    (should-not (get-text-property (+ 1 (string-match "a  " rendered))
                                  'keymap-popup--entry rendered))
    (should (get-text-property (string-match "Act" rendered)
                              'mouse-face rendered))))

(defvar keymap-popup-mouse-test--switch nil)
(keymap-popup-define keymap-popup-mouse-test--switch-map
  "s" ("Toggle" :switch keymap-popup-mouse-test--switch))

(ert-deftest keymap-popup-mouse-test-switch-and-runtime-submenu ()
  (keymap-popup-input--with-source
    (setq-local keymap-popup-mouse-test--switch nil)
    (let ((root (make-sparse-keymap))
          (child keymap-popup-mouse-test--switch-map))
      (keymap-set root "s" child)
      (keymap-popup-attach root (list "s" (list "Child" :keymap child)))
      (keymap-popup root)
      (let ((buf (keymap-popup--popup-buffer)))
        (execute-kbd-macro
         (keymap-popup-mouse-test--click
          (keymap-popup-mouse-test--position "Child")))
        (should (eq (keymap-popup--active-get buf :keymap) child))
        (should (keymap-popup--session-get buf :stack))
        (execute-kbd-macro
         (keymap-popup-mouse-test--click
          (keymap-popup-mouse-test--position "Toggle")))
        (should keymap-popup-mouse-test--switch)
        (should (eq (keymap-popup--popup-buffer) buf))
        (execute-kbd-macro (kbd "q"))
        (should (eq (keymap-popup--active-get buf :keymap) root))
        (execute-kbd-macro (kbd "q"))
        (keymap-popup-input--clean-p)))))

(ert-deftest keymap-popup-mouse-test-native-key-spellings ()
  (dolist (key '("TAB" "RET" "M-a" "M-ESC f" "ESC ESC f" "<f5>"))
    (ert-info ((format "key=%s" key))
      (keymap-popup-input--with-source
        (let* ((called nil)
               (command (lambda () (interactive) (setq called t)))
               (map (make-sparse-keymap)))
          (keymap-set map key command)
          (keymap-popup-attach map (list key (list "Action" command)))
          (keymap-popup map)
          (execute-kbd-macro
           (keymap-popup-mouse-test--click
            (keymap-popup-mouse-test--position "Action")))
          (should called)
          (keymap-popup-input--clean-p))))))

(ert-deftest keymap-popup-mouse-test-refuses-foreign-source-window ()
  (keymap-popup-input--with-source
    (let* ((called nil)
           (command (lambda () (interactive) (setq called t)))
           (map (keymap-popup-input--suffix-map command nil))
           (foreign (generate-new-buffer " *mouse-foreign*")))
      (unwind-protect
          (progn
            (keymap-popup map)
            (let ((position (keymap-popup-mouse-test--position "Action")))
              (set-window-buffer (selected-window) foreign)
              (set-buffer foreign)
              (execute-kbd-macro (keymap-popup-mouse-test--click position)))
            (should-not called)
            (should (eq (current-buffer) foreign))
            (should (equal (buffer-string) "")))
        (keymap-popup-dismiss)
        (kill-buffer foreign)))))

(ert-deftest keymap-popup-mouse-test-preserves-command-history ()
  (keymap-popup-input--with-source
    (let* ((observed nil)
           (command (lambda () (interactive) (push last-command observed)))
           (map (keymap-popup-input--suffix-map command nil :stay-open t)))
      (keymap-popup map)
      (let ((click (keymap-popup-mouse-test--click
                    (keymap-popup-mouse-test--position "Action"))))
        (execute-kbd-macro (vconcat (kbd "a") click click)))
      (should (eq (car observed) command))
      (should (eq (cadr observed) command))
      (should (eq last-command command))
      (execute-kbd-macro (kbd "q"))
      (keymap-popup-input--clean-p))))

(ert-deftest keymap-popup-mouse-test-selected-foreign-window ()
  (keymap-popup-input--with-source
    (insert "Source")
    (goto-char 3)
    (let* ((window (selected-window))
           (foreign (generate-new-buffer " *mouse-other-window*"))
           (other (split-window-right))
           (observed nil)
           (command (lambda () (interactive)
                      (setq observed (list (current-buffer) (point)))
                      (insert "!")))
           (map (keymap-popup-input--suffix-map command t)))
      (unwind-protect
          (progn
            (set-window-buffer other foreign)
            (keymap-popup map)
            (let* ((position (keymap-popup-mouse-test--position "Action"))
                   (background (list (car position) 1 '(0 . 0) 0)))
              (select-window other)
              (execute-kbd-macro (keymap-popup-mouse-test--click background))
              (should (eq (selected-window) other))
              (execute-kbd-macro (keymap-popup-mouse-test--click position)))
            (should (equal observed (list source 3)))
            (should (eq (selected-window) window))
            (should (equal (buffer-string) "So!urce"))
            (should (equal (with-current-buffer foreign (buffer-string)) ""))
            (execute-kbd-macro (kbd "q"))
            (keymap-popup-input--clean-p))
        (keymap-popup-dismiss)
        (kill-buffer foreign)))))

(ert-deftest keymap-popup-mouse-test-requires-matching-press ()
  (dolist (change '(orphan replacement submenu target))
    (keymap-popup-input--with-source
      (let* ((called nil)
             (command (lambda () (interactive) (setq called t) (insert "!")))
             (map (keymap-popup-input--suffix-map command nil :stay-open t))
             (other (generate-new-buffer " *mouse-replacement*")))
        (unwind-protect
            (progn
              (keymap-set map "b" command)
              (keymap-popup-attach
               map (list "a" (list "Action" command :stay-open t)
                         "b" (list "Other" command :stay-open t)))
              (keymap-popup map)
              (unless (eq change 'orphan)
                (execute-kbd-macro
                 (vector (list 'down-mouse-1
                               (keymap-popup-mouse-test--position "Action")))))
              (pcase change
                ('replacement (switch-to-buffer other) (keymap-popup map))
                ('submenu (keymap-popup--push-submenu
                           (keymap-popup--popup-buffer) map)))
              (execute-kbd-macro
               (vector (list 'mouse-1
                             (keymap-popup-mouse-test--position
                              (if (eq change 'target) "Other" "Action")))))
              (should-not called)
              (should (equal (with-current-buffer source (buffer-string)) ""))
              (should (equal (with-current-buffer other (buffer-string)) ""))
              ;; Refusing the stale gesture must not disable the new owner.
              (execute-kbd-macro
               (keymap-popup-mouse-test--click
                (keymap-popup-mouse-test--position "Action")))
              (should called))
          (keymap-popup-dismiss)
          (kill-buffer other))))))

(ert-deftest keymap-popup-mouse-test-predicate-errors-fail-closed ()
  (dolist (property '(:if :inapt-if))
    (dolist (stage '(release replay))
      (keymap-popup-input--with-source
        (let* ((explode nil) (called nil)
               (window (selected-window))
               (command (lambda () (interactive) (setq called t) (insert "!")))
               (predicate (lambda ()
                            (when explode (error "Mouse predicate failure"))
                            (eq property :if)))
               (map (keymap-popup-input--suffix-map
                     command nil property predicate)))
          (keymap-popup map)
          (let ((position (keymap-popup-mouse-test--position "Action")))
            (execute-kbd-macro (vector (list 'down-mouse-1 position)))
            (setq explode (eq stage 'release))
            (let ((post-command-hook
                   (list (lambda ()
                           (when (and (eq (car-safe last-input-event) 'mouse-1)
                                      unread-command-events)
                             (setq explode t))))))
              (condition-case nil
                  (execute-kbd-macro (vector (list 'mouse-1 position)))
                (error nil))))
          (should-not called)
          (should-not unread-command-events)
          (should (eq (selected-window) window))
          (should (eq (current-buffer) source))
          (should (equal (buffer-string) "")))))))

(ert-deftest keymap-popup-mouse-test-target-pre-command-hook ()
  (keymap-popup-input--with-source
    (keymap-popup (keymap-popup-input--suffix-map #'ignore nil :stay-open t))
    (let* ((seen nil)
           (pre-command-hook
            (append pre-command-hook
                    (list (lambda ()
                            (when (equal (this-command-keys-vector) [97])
                              (push 'target seen)))))))
      (execute-kbd-macro (kbd "a"))
      (should (equal seen '(target)))
      (setq seen nil)
      (execute-kbd-macro
       (keymap-popup-mouse-test--click
        (keymap-popup-mouse-test--position "Action")))
      (should (equal seen '(target))))))

(ert-deftest keymap-popup-mouse-test-prefix-history ()
  (keymap-popup-input--with-source
    (let* ((observed nil)
           (command (lambda () (interactive)
                      (push (list current-prefix-arg last-prefix-arg) observed))))
      (keymap-popup (keymap-popup-input--suffix-map command nil :stay-open t))
      (execute-kbd-macro (kbd "C-u 2 a C-u 3 a"))
      (let ((keyboard (car observed)))
        (execute-kbd-macro (kbd "C-u 2 a"))
        (execute-kbd-macro
         (vconcat (kbd "C-u 3")
                  (keymap-popup-mouse-test--click
                   (keymap-popup-mouse-test--position "Action"))))
        (should (equal (car observed) keyboard))))))

(ert-deftest keymap-popup-mouse-test-action-window-selection-persists ()
  (keymap-popup-input--with-source
    (let* ((other (split-window-right))
           (command (lambda () (interactive) (select-window other))))
      (keymap-popup (keymap-popup-input--suffix-map command nil))
      (execute-kbd-macro
       (keymap-popup-mouse-test--click
        (keymap-popup-mouse-test--position "Action")))
      (should (eq (selected-window) other))
      (keymap-popup-input--clean-p))))

(defun keymap-popup-mouse-test--repurpose (change)
  "Repurpose the current source according to CHANGE without writing a file."
  (pcase change
    ('mode (text-mode))
    ('reinitialize (fundamental-mode))
    ('mode-round-trip (text-mode) (fundamental-mode))
    ((or 'file 'file-round-trip)
     ;; Preserve the mode to exercise the independent document boundary.
     (let ((change-major-mode-with-file-name nil)
           (auto-save-default nil)
           (create-lockfiles nil))
       (set-visited-file-name
        (make-temp-name (expand-file-name "keymap-popup-source-"
                                         temporary-file-directory)) t)
       (when (eq change 'file-round-trip)
         (set-visited-file-name nil t))
       (set-buffer-modified-p nil)))))

(ert-deftest keymap-popup-mouse-test-source-repurposing ()
  (dolist (stay '(nil t))
    (dolist (stage '(release replay))
      (dolist (change '(mode reinitialize mode-round-trip file file-round-trip))
        (ert-info ((format "stay=%S stage=%S change=%S" stay stage change))
          (keymap-popup-input--with-source
            (insert "Draft")
            (let* ((observed nil)
                   (command (lambda () (interactive)
                              (push (list (current-buffer) last-command) observed)
                              (insert "!")))
                   (map (keymap-popup-input--suffix-map command nil :stay-open stay)))
              (keymap-popup map)
              (let* ((buf (keymap-popup--popup-buffer))
                     (position (keymap-popup-mouse-test--position "Action"))
                     (lifetime (keymap-popup--session-get buf :source-live)))
                (execute-kbd-macro (vector (list 'down-mouse-1 position)))
                (if (eq stage 'release)
                    (keymap-popup-mouse-test--repurpose change)
                  (add-hook 'post-command-hook
                            (lambda ()
                              (when (eq (car-safe last-input-event) 'mouse-1)
                                (keymap-popup-mouse-test--repurpose change)))))
                (execute-kbd-macro (vector (list 'mouse-1 position)))
                (setq post-command-hook nil)
                (should-not (car lifetime))
                (should-not observed)
                (should-not unread-command-events)
                (should (equal (buffer-string) "Draft"))
                ;; Restoring the mode/file or starting a new gesture does
                ;; not revive the retired source lifetime.
                (execute-kbd-macro (keymap-popup-mouse-test--click position))
                (should-not observed)
                (should (equal (buffer-string) "Draft"))
                ;; A newly opened session owns the repurposed source.
                (keymap-popup map)
                (should (car (keymap-popup--session-get
                              (keymap-popup--popup-buffer) :source-live)))
                (execute-kbd-macro
                 (keymap-popup-mouse-test--click
                  (keymap-popup-mouse-test--position "Action")))
                (should (= (length observed) 1))
                (should (eq (caar observed) source))
                (should (eq last-command command))
                (should (equal (buffer-string) "Draft!"))
                (should (eq (not (null (keymap-popup--popup-buffer))) stay))
                (keymap-popup-dismiss)
                (set-buffer-modified-p nil)
                (keymap-popup-input--clean-p)))))))))

(ert-deftest keymap-popup-mouse-test-source-lifetime-positive-and-cleanup ()
  (dolist (stay '(nil t))
    (keymap-popup-input--with-source
      (let* ((observed nil)
             (unrelated #'ignore)
             (command (lambda (arg) (interactive "P")
                        (push (list (current-buffer) arg this-command) observed))))
        (add-hook 'change-major-mode-hook unrelated nil t)
        (add-hook 'after-set-visited-file-name-hook unrelated nil t)
        (keymap-popup (keymap-popup-input--suffix-map command nil :stay-open stay))
        (let* ((buf (keymap-popup--popup-buffer))
               (retire (keymap-popup--session-get buf :source-retire)))
          (rename-buffer (generate-new-buffer-name " *renamed-mouse-source*"))
          (execute-kbd-macro
           (vconcat (kbd "C-u 3")
                    (keymap-popup-mouse-test--click
                     (keymap-popup-mouse-test--position "Action"))))
          (should (equal observed (list (list source 3 command))))
          (should (eq last-command command))
          (keymap-popup-dismiss)
          (should-not (memq retire change-major-mode-hook))
          (should-not (memq retire after-set-visited-file-name-hook))
          (should (memq unrelated change-major-mode-hook))
          (should (memq unrelated after-set-visited-file-name-hook))
          (keymap-popup-input--clean-p))))))

(ert-deftest keymap-popup-mouse-test-native-later-hook-context ()
  ;; A hook intentionally changing context after native lookup governs both
  ;; input methods.  This is not stale input before the replay guard.
  (dolist (stay '(nil t))
    (let (results)
      (dolist (method '(keyboard mouse))
        (keymap-popup-input--with-source
          (let* ((foreign (generate-new-buffer " *owner-native-hook-target*"))
                 (observed nil)
                 (command (lambda () (interactive)
                            (setq observed
                                  (list (eq (current-buffer) foreign)
                                        (eq this-command real-this-command)
                                        (this-command-keys-vector)
                                        current-prefix-arg))
                            (insert "HOOK-TARGET")))
                 (hook (lambda ()
                         (when (equal (this-command-keys-vector) [97])
                           (keymap-popup-dismiss)
                           (switch-to-buffer foreign)))))
            (unwind-protect
                (progn
                  (keymap-popup (keymap-popup-input--suffix-map command nil :stay-open stay))
                  (add-hook 'pre-command-hook hook 10)
                  (execute-kbd-macro
                   (if (eq method 'keyboard) [97]
                     (keymap-popup-mouse-test--click
                      (keymap-popup-mouse-test--position "Action"))))
                  (should observed)
                  (should (equal (with-current-buffer source (buffer-string)) ""))
                  (should (equal (with-current-buffer foreign (buffer-string)) "HOOK-TARGET"))
                  (should (eq last-command command))
                  (push observed results))
              (remove-hook 'pre-command-hook hook)
              (kill-buffer foreign)))))
      (should (equal (car results) (cadr results)))
      ;; Persistent suffix wrappers change this-command; native closing
      ;; suffixes retain the command.  Both input methods must agree.
      (should (equal (car results) (list t (not stay) [97] nil))))))

(ert-deftest keymap-popup-mouse-test-navigation-roundtrip ()
  (dolist (persistent '(nil t))
    (keymap-popup-input--with-source
      (let* ((calls 0)
             (command (lambda () (interactive) (cl-incf calls) (insert "!")))
             (root (make-sparse-keymap))
             (child (keymap-popup-input--suffix-map #'ignore nil :stay-open t)))
        (keymap-set root "a" command)
        (keymap-set root "s" child)
        (keymap-popup-attach root (list "a" (list "Action" command :stay-open t)
                                      "s" (list "Child" :keymap child))
                             :persistent persistent)
        (keymap-popup root)
        (let* ((buf (keymap-popup--popup-buffer))
               (position (keymap-popup-mouse-test--position "Action"))
               (wrapper (keymap-popup--active-get buf :wrapper-map)))
          (execute-kbd-macro (keymap-popup-mouse-test--click position))
          (should (= calls 1))
          (execute-kbd-macro
           (vconcat (vector (list 'down-mouse-1 position)) (kbd "s q")))
          (should (eq (keymap-popup--active-get buf :keymap) root))
          (should (eq (keymap-popup--active-get buf :wrapper-map) wrapper))
          (should-not (keymap-popup--session-get buf :stack))
          (execute-kbd-macro (vector (list 'mouse-1 position)))
          (should (= calls 1))
          (should (equal (buffer-string) "!"))
          (execute-kbd-macro (keymap-popup-mouse-test--click position))
          (should (= calls 2))
          (should (equal (buffer-string) "!!")))))))

(ert-deftest keymap-popup-mouse-test-navigation-retires-queued-replay ()
  (dolist (persistent '(nil t))
    (keymap-popup-input--with-source
      (let* ((calls 0)
             (command (lambda () (interactive) (cl-incf calls) (insert "!")))
             (root (keymap-popup-input--suffix-map command persistent :stay-open t))
             (child (keymap-popup-input--suffix-map #'ignore nil :stay-open t)))
        (keymap-popup root)
        (let* ((buf (keymap-popup--popup-buffer))
               (wrapper (keymap-popup--active-get buf :wrapper-map))
               (hook (lambda ()
                       (when (eq (car-safe last-input-event) 'mouse-1)
                         ;; A callback navigates while the clicked key is queued.
                         ;; Preserve that queue; invoke the native exit closure
                         ;; with the child's back key rather than reading input.
                         (keymap-popup--push-submenu buf child)
                         (cl-letf (((symbol-function 'this-command-keys-vector)
                                    (lambda () [113])))
                           (funcall (keymap-popup--active-get buf :exit-function)))))))
          (add-hook 'post-command-hook hook)
          (unwind-protect
              (execute-kbd-macro
               (keymap-popup-mouse-test--click
                (keymap-popup-mouse-test--position "Action")))
            (remove-hook 'post-command-hook hook))
          (should (eq (keymap-popup--active-get buf :wrapper-map) wrapper))
          (should-not (keymap-popup--session-get buf :stack))
          (should (= calls 0))
          (should (equal (buffer-string) ""))
          (should-not unread-command-events)
          (execute-kbd-macro
           (keymap-popup-mouse-test--click
            (keymap-popup-mouse-test--position "Action")))
          (should (= calls 1)))))))

(ert-deftest keymap-popup-mouse-test-filter-retires-source ()
  (dolist (stage '(current final))
    (dolist (change '(nil mode file))
      (keymap-popup-input--with-source
        (insert "Draft")
        (let* ((armed nil) (reached nil) (called nil)
               (command (lambda () (interactive) (setq called t) (insert "!")))
               (map (keymap-popup-input--suffix-map command nil)))
          ;; Use a real menu filter.  Identify the qualification boundary
          ;; from its native stack, not a version-specific lookup count.
          (keymap-set
           map "a"
           (list 'menu-item "Action" command :filter
                 (lambda (binding)
                   (when (and armed (not reached))
                     (let* ((frames (mapcar
                                     (lambda (frame)
                                       (when (symbolp (cadr frame))
                                         (symbol-name (cadr frame))))
                                     (backtrace-frames)))
                            (inside (member "keymap-popup--mouse-current-p" frames)))
                       (when (and (member "keymap-popup--mouse-replay" frames)
                                  (if (eq stage 'current) inside (not inside)))
                         (setq reached t)
                         (when change
                           (keymap-popup-mouse-test--repurpose change)))))
                   binding)))
          (keymap-popup map)
          (let* ((buf (keymap-popup--popup-buffer))
                 (lifetime (keymap-popup--session-get buf :source-live))
                 (post-command-hook
                  (list (lambda ()
                          (when (eq (car-safe last-input-event) 'mouse-1)
                            (setq armed t))))))
            (execute-kbd-macro
             (keymap-popup-mouse-test--click
              (keymap-popup-mouse-test--position "Action")))
            (should reached)
            (should (eq called (null change)))
            (should (eq (not (null (car lifetime))) (null change)))
            (should (equal (buffer-string) (if change "Draft" "Draft!")))
            (should-not unread-command-events)
            (set-buffer-modified-p nil)))))))

(provide 'keymap-popup-mouse-tests)
;;; keymap-popup-mouse-tests.el ends here
