;;; keymap-popup-declarations-tests.el --- Declaration regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'keymap-popup)

(defvar keymap-popup-declarations-test--map)

(defmacro keymap-popup-declarations-test--with-popup (&rest body)
  "Run BODY with isolated popup state and a disposable owner buffer."
  (declare (indent 0) (debug t))
  `(let ((overriding-terminal-local-map nil)
         (pre-command-hook nil)
         (post-command-hook nil)
         (minibuffer-setup-hook nil)
         (minibuffer-exit-hook nil)
         (keymap-popup-backend
          (lambda () (list :show #'ignore :fit #'ignore :hide #'ignore))))
     (unwind-protect
         (with-temp-buffer ,@body)
       (keymap-popup-dismiss))))

(defun keymap-popup-declarations-test--text (map)
  "Return the public popup text for MAP, then dismiss it."
  (unwind-protect
      (progn
        (keymap-popup map)
        (with-current-buffer keymap-popup--buffer-name (buffer-string)))
    (keymap-popup-dismiss)))

(ert-deftest keymap-popup-declarations-test-add-group-predicate ()
  "Adds, replacements, moves and fallback all use the destination predicate."
  (keymap-popup-declarations-test--with-popup
    (let* ((enabled nil)
           (other-enabled t)
           (map (make-sparse-keymap)))
      (keymap-popup-attach
       map (list :group (list "First" :if (lambda () enabled))
                 "a" '("Original" ignore)
                 :group (list "Second" :if (lambda () other-enabled))
                 "s" '("Second" ignore)))
      (use-local-map map)
      (dolist (group '("First" nil "Missing"))
        (keymap-popup-add-entry map "x" "Added" #'forward-char group)
        (should-not (keymap-lookup map "x"))
        (should-not (eq (key-binding (kbd "x")) #'forward-char))
        (setq enabled t)
        (insert "ab")
        (goto-char (point-min))
        (call-interactively (key-binding (kbd "x")))
        (should (= (point) 2))
        (should (string-match-p "Added"
                                (keymap-popup-declarations-test--text map)))
        (setq enabled nil))
      (keymap-popup-add-entry map "x" "Moved" #'backward-char "Second")
      (should (eq (key-binding (kbd "x")) #'backward-char))
      (setq other-enabled nil enabled t)
      (should-not (keymap-lookup map "x"))
      (should-not (eq (key-binding (kbd "x")) #'backward-char))
      (keymap-popup-add-entry map "a" "Replaced" #'forward-char "First")
      (should (eq (key-binding (kbd "a")) #'forward-char))
      (setq enabled nil)
      (should-not (keymap-lookup map "a"))
      (should-not (eq (key-binding (kbd "a")) #'forward-char))
      (let ((text (keymap-popup-declarations-test--text map)))
        (should-not (string-match-p "Replaced\\|Moved" text))))))

(ert-deftest keymap-popup-declarations-test-add-inapt-is-popup-only ()
  "Group inapt state must not disable ordinary direct dispatch."
  (keymap-popup-declarations-test--with-popup
    (let ((map (make-sparse-keymap)))
      (keymap-popup-attach
       map (list :group (list "Disabled" :inapt-if #'always)
                 "a" '("Original" ignore)))
      (keymap-popup-add-entry map "x" "Added" #'forward-char)
      (use-local-map map)
      (should (eq (key-binding (kbd "x")) #'forward-char))
      (insert "ab")
      (goto-char (point-min))
      (call-interactively (key-binding (kbd "x")))
      (should (= (point) 2))
      (keymap-popup map)
      (call-interactively (key-binding (kbd "x")))
      (should (= (point) 2)))))

(defun keymap-popup-declarations-test--aliases ()
  "Return native alias pairs in both orders for `meta-prefix-char'."
  (let ((prefix (single-key-description meta-prefix-char)))
    (cl-loop for pair in
             (append '(("TAB" "C-i") ("RET" "C-m"))
                     (list (list "M-f" (concat prefix " f"))
                           (list "C-c M-f M-b"
                                 (concat "C-c " prefix " f " prefix " b"))
                           (list (concat (single-key-description
                                          (logior meta-prefix-char #x8000000)) " f")
                                 (concat prefix " " prefix " f"))))
             append (list pair (reverse pair)))))

(ert-deftest keymap-popup-declarations-test-remove-key-aliases ()
  "Removing either spelling clears the metadata and preserves distinct keys."
  (keymap-popup-declarations-test--with-popup
    (dolist (meta-prefix-char '(27 24))
      (dolist (pair (keymap-popup-declarations-test--aliases))
        (let ((map (make-sparse-keymap)))
          (keymap-set map (car pair) #'forward-char)
          (should (eq (keymap-lookup map (cadr pair)) #'forward-char))
          (keymap-set map "z" #'backward-char)
          (keymap-popup-attach
           map (list (car pair) '("Removed" forward-char)
                     "z" '("Preserved" backward-char)))
          (keymap-popup-remove-entry map (cadr pair))
          (should-not (keymap-lookup map (car pair)))
          (should-not (keymap-lookup map (cadr pair)))
          (should (= (length (keymap-popup--flatten-with-groups
                             (keymap-popup--meta map 'descriptions))) 1))
          (let ((text (keymap-popup-declarations-test--text map)))
            (should-not (string-match-p "Removed" text))
            (should (string-match-p "Preserved" text)))
          (keymap-popup-remove-entry map "z")
          (should-not (keymap-popup--meta map 'descriptions))
          (should-error (keymap-popup map) :type 'user-error))))))

(ert-deftest keymap-popup-declarations-test-replace-key-aliases ()
  "Alias replacement keeps only the new label and its display spelling."
  (keymap-popup-declarations-test--with-popup
    (dolist (meta-prefix-char '(27 24))
      (dolist (pair (keymap-popup-declarations-test--aliases))
        (dolist (command '(forward-char backward-char))
          (let ((map (make-sparse-keymap)))
            (keymap-set map (car pair) #'forward-char)
            (should (eq (keymap-lookup map (cadr pair)) #'forward-char))
            (keymap-popup-attach map (list (car pair) '("Old" forward-char)))
            (keymap-popup-add-entry map (cadr pair) "New" command)
            (should (eq (keymap-lookup map (car pair)) command))
            (let ((entries (keymap-popup--flatten-with-groups
                            (keymap-popup--meta map 'descriptions)))
                  (text (keymap-popup-declarations-test--text map)))
              (should (= (length entries) 1))
              (should (equal (plist-get (car entries) :key) (cadr pair)))
              (should-not (string-match-p "Old" text))
              (should (string-match-p "New" text)))
            (keymap-popup-remove-entry map (car pair))
            (should-not (keymap-popup--meta map 'descriptions))
            (should-error (keymap-popup map) :type 'user-error)))))))

(ert-deftest keymap-popup-declarations-test-distinct-native-keys ()
  "Do not coalesce distinct keys, even when they run the same command."
  (keymap-popup-declarations-test--with-popup
    (dolist (case '((27 "TAB" "<tab>") (27 "RET" "<return>")
                    (27 "M-<left>" "ESC <left>") (24 "M-f" "ESC f")))
      (pcase-let ((`(,meta-prefix-char ,first ,second) case))
        (let ((map (make-sparse-keymap)))
          (keymap-set map first #'forward-char)
          (keymap-popup-attach map (list first '("First" forward-char)))
          (keymap-popup-add-entry map second "Second" #'forward-char)
          (should (= (length (keymap-popup--flatten-with-groups
                             (keymap-popup--meta map 'descriptions))) 2))
          (keymap-popup-remove-entry map first)
          (should-not (keymap-lookup map first))
          (should (eq (keymap-lookup map second) #'forward-char))
          (let ((text (keymap-popup-declarations-test--text map)))
            (should-not (string-match-p "First" text))
            (should (string-match-p "Second" text))))))))

(ert-deftest keymap-popup-declarations-test-reload-key-aliases ()
  "Reevaluating an alias spelling retains the declared anonymous command."
  (keymap-popup-declarations-test--with-popup
    (dolist (meta-prefix-char '(27 24))
      (dolist (pair (keymap-popup-declarations-test--aliases))
        (let ((keymap-popup-declarations-test--map nil))
          (makunbound 'keymap-popup-declarations-test--map)
          (eval `(keymap-popup-define keymap-popup-declarations-test--map
                   ,(car pair) ("Original" (lambda () (interactive) (insert "first"))))
                t)
          (let* ((map keymap-popup-declarations-test--map)
                 (original (keymap-lookup map (car pair))))
            (should (eq original (keymap-lookup map (cadr pair))))
            (eval `(keymap-popup-define keymap-popup-declarations-test--map
                     ,(cadr pair) ("Reloaded" (lambda () (interactive) (insert "second"))))
                  t)
            (should (eq map keymap-popup-declarations-test--map))
            (should (eq original (keymap-lookup map (cadr pair))))
            (let ((text (keymap-popup-declarations-test--text map)))
              (should (string-match-p "Reloaded" text))
              (should-not (string-match-p "Original" text)))
            (erase-buffer)
            (keymap-popup map)
            (call-interactively (key-binding (kbd (cadr pair))))
            (should (equal (buffer-string) "first"))
            (keymap-popup-dismiss)))))))

(defun keymap-popup-declarations-test--reload (compile)
  "Exercise repeated anonymous declarations using COMPILE on their factory."
  (keymap-popup-declarations-test--with-popup
    (let* ((keymap-popup-declarations-test--map nil)
           (factory
            (funcall compile
                     (eval
                      '(lambda (value label)
                         (keymap-popup-define keymap-popup-declarations-test--map
                           "a" (label (lambda () (interactive) (insert value)))
                           "f" ("Filtered" (lambda () (interactive) (insert value))
                                :if (lambda () t))))
                      t))))
      (makunbound 'keymap-popup-declarations-test--map)
      (funcall factory "first" "Original")
      (let* ((map keymap-popup-declarations-test--map)
             (original (keymap-lookup map "a"))
             (filtered (keymap-lookup map "f")))
        (keymap-set map "u" #'ignore)
        (funcall factory "second" "Reloaded")
        (should (eq map keymap-popup-declarations-test--map))
        (should (eq original (keymap-lookup map "a")))
        (should (eq filtered (keymap-lookup map "f")))
        (should (eq (keymap-lookup map "u") #'ignore))
        (let ((text (keymap-popup-declarations-test--text map)))
          (should (string-match-p "Reloaded" text))
          (should (string-match-p "Filtered" text)))
        (keymap-popup map)
        (call-interactively (key-binding (kbd "a")))
        (should (equal (buffer-string) "first"))
        (keymap-popup-dismiss)
        ;; Do not steal a user replacement's identity, even if it is equal.
        (let ((replacement
               (eval '(lambda () (interactive) (insert value))
                     '((value . "first")))))
          (should-not (eq original replacement))
          (keymap-set map "a" replacement)
          (funcall factory "third" "Reloaded")
          (should (eq (keymap-lookup map "a") replacement))
          (should-not (string-match-p
                       "Reloaded" (keymap-popup-declarations-test--text map)))
          (keymap-set map "z" original)
          (funcall factory "fourth" "Reloaded")
          (should (string-match-p
                   "Reloaded" (keymap-popup-declarations-test--text map)))
          (keymap-popup map)
          (call-interactively (key-binding (kbd "z")))
          (should (equal (buffer-string) "firstfirst")))))))

(ert-deftest keymap-popup-declarations-test-reload-source ()
  (keymap-popup-declarations-test--reload #'identity))

(ert-deftest keymap-popup-declarations-test-reload-byte-compiled ()
  (keymap-popup-declarations-test--reload #'byte-compile))

(ert-deftest keymap-popup-declarations-test-reload-one-action ()
  "A one-action declaration can still open its popup after a reload."
  (keymap-popup-declarations-test--with-popup
    (let ((keymap-popup-declarations-test--map nil))
      (makunbound 'keymap-popup-declarations-test--map)
      (dotimes (_ 2)
        (eval '(keymap-popup-define keymap-popup-declarations-test--map
                 "a" ("Only action" (lambda () (interactive) (insert "x"))))
              t))
      (keymap-popup keymap-popup-declarations-test--map)
      (should (string-match-p "Only action"
                              (with-current-buffer keymap-popup--buffer-name
                                (buffer-string))))
      (call-interactively (key-binding (kbd "a")))
      (should (equal (buffer-string) "x")))))

(ert-deftest keymap-popup-declarations-test-reload-hidden-initializer ()
  "Reuse hidden command identity without reevaluating its initializer."
  (keymap-popup-declarations-test--with-popup
    (let* ((keymap-popup-declarations-test--map nil)
           (state (list 0 nil))
           (factory
            (eval '(lambda ()
                     (keymap-popup-define keymap-popup-declarations-test--map
                       "a" ("Hidden"
                            (progn
                              (cl-incf (car state))
                              (lambda () (interactive) (insert "x")))
                            :if (lambda () (cadr state)))))
                  `((state . ,state)))))
      (makunbound 'keymap-popup-declarations-test--map)
      (funcall factory)
      (funcall factory)
      (should (= (car state) 1))
      (should-not (keymap-lookup keymap-popup-declarations-test--map "a"))
      (setcar (cdr state) t)
      (should (string-match-p
               "Hidden" (keymap-popup-declarations-test--text
                         keymap-popup-declarations-test--map)))
      (makunbound 'keymap-popup-declarations-test--map)
      (funcall factory)
      (should (= (car state) 2)))))

(ert-deftest keymap-popup-declarations-test-reload-distinct-equal-command ()
  "Reload must not transfer descriptions to an equal user replacement."
  (keymap-popup-declarations-test--with-popup
    (let ((keymap-popup-declarations-test--map nil)
          (form '(keymap-popup-define keymap-popup-declarations-test--map
                   "a" ("Original" (lambda () (interactive) (insert "x")))
                   "n" ("Named" ignore))))
      (makunbound 'keymap-popup-declarations-test--map)
      (eval form t)
      (let* ((map keymap-popup-declarations-test--map)
             (original (keymap-lookup map "a"))
             (replacement (eval '(lambda () (interactive) (insert "x")) t)))
        (should (equal original replacement))
        (should-not (eq original replacement))
        (keymap-set map "a" replacement)
        (eval form t)
        (should (eq replacement (keymap-lookup map "a")))
        (should-not (string-match-p
                     "Original" (keymap-popup-declarations-test--text map)))))))

(ert-deftest keymap-popup-declarations-test-reload-native-compiled ()
  "Reload a native-compiled consumer with a captured anonymous command."
  (skip-unless (and (fboundp 'native-comp-available-p)
                    (native-comp-available-p)))
  (require 'comp)
  (let* ((directory (make-temp-file "keymap-popup-native-" t))
         (source (expand-file-name "consumer.el" directory))
         (native-comp-eln-load-path (cons directory native-comp-eln-load-path))
         (keymap-popup-declarations-test--map nil))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert
             ";;; consumer.el -*- lexical-binding: t; -*-\n"
             "(require 'keymap-popup)\n"
             "(defun keymap-popup-declarations-test--native-factory (value)\n"
             "  (keymap-popup-define keymap-popup-declarations-test--map\n"
             "    \"a\" (\"Native\" (lambda () (interactive) (insert value)))))\n"))
          ;; File loading here tests a real compiled consumer, not test discovery.
          (load (native-compile source) nil t)
          (keymap-popup-declarations-test--with-popup
            (makunbound 'keymap-popup-declarations-test--map)
            (let ((factory (symbol-function
                            'keymap-popup-declarations-test--native-factory)))
              (should (subrp factory))
              (funcall factory "first")
              (let ((map keymap-popup-declarations-test--map))
                (funcall factory "second")
                (should (eq map keymap-popup-declarations-test--map))
                (should (string-match-p
                         "Native" (keymap-popup-declarations-test--text map)))
                (keymap-popup map)
                (call-interactively (key-binding (kbd "a")))
                (should (equal (buffer-string) "first"))))))
      (fmakunbound 'keymap-popup-declarations-test--native-factory)
      (delete-directory directory t))))

(provide 'keymap-popup-declarations-tests)
;;; keymap-popup-declarations-tests.el ends here
