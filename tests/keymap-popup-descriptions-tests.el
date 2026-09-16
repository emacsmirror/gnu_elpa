;;; keymap-popup-descriptions-tests.el --- Description regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Native composition, inherited rebinding and display-column alignment.

;;; Code:

(require 'ert)
(require 'keymap-popup)

(defun keymap-popup-descriptions-test--map (name key command label &rest props)
  "Return a described map with NAME, KEY, COMMAND, LABEL and PROPS."
  (let ((map (make-sparse-keymap)))
    (keymap-set map key command)
    (keymap-popup-attach map
                        (list :group name key (append (list label command) props)))
    map))

(defun keymap-popup-descriptions-test--entries (map)
  "Return resolved entries of MAP in display order."
  (cl-loop for row in (keymap-popup--resolve-descriptions
                      (keymap-popup--collect-descriptions map) map)
           append (cl-loop for group in row append (plist-get group :entries))))

(defun keymap-popup-descriptions-test--popup (map check)
  "Open MAP in a disposable popup, then call CHECK with its buffer.
Only the display backend is replaced; session and wrapper setup are real."
  (let ((keymap-popup--buffer-name " *keymap-popup-description-test*")
        (keymap-popup-backend
         (lambda () (list :show #'ignore :fit #'ignore :hide #'ignore))))
    (unwind-protect
        (progn
          (keymap-popup map)
          (funcall check (get-buffer keymap-popup--buffer-name)))
      (keymap-popup-dismiss))))

(ert-deftest keymap-popup-descriptions-test-composed-public ()
  "Later components supply both visible labels and popup-only restrictions."
  (let* ((a (keymap-popup-descriptions-test--map "First" "a" 'forward-char "Forward"))
         (b (keymap-popup-descriptions-test--map "Second" "b" 'backward-char "Backward"
                                                :inapt-if (lambda () t)))
         (map (make-composed-keymap (list a b))))
    (should (eq (keymap-lookup map "b") 'backward-char))
    (keymap-popup-descriptions-test--popup
     map (lambda (buf)
           (with-current-buffer buf
             (should (string-match-p "Forward" (buffer-string)))
             (should (string-match-p "Backward" (buffer-string))))
           (let ((wrapper (keymap-popup--active-get buf :wrapper-map)))
             (with-temp-buffer
               (insert "ab")
               (goto-char 2)
               (call-interactively (keymap-lookup wrapper "b"))
               (should (= (point) 2))))))))

(ert-deftest keymap-popup-descriptions-test-nested-composition ()
  "Nested components, shared parents and collisions follow native lookup."
  (let* ((parent (keymap-popup-descriptions-test--map "Parent" "p" 'beginning-of-line "Parent"))
         (a (keymap-popup-descriptions-test--map "First" "a" 'forward-char "Winner"))
         (b (keymap-popup-descriptions-test--map "Second" "a" 'backward-char "Loser"))
         (c (keymap-popup-descriptions-test--map "Third" "c" 'end-of-line "Third")))
    (set-keymap-parent a parent)
    (set-keymap-parent c parent)
    (let* ((map (make-composed-keymap (list a (make-composed-keymap (list b c))) parent))
           (snapshot (copy-tree map))
           (entries (keymap-popup-descriptions-test--entries map)))
      (should (eq (keymap-lookup map "a") 'forward-char))
      (should (equal (mapcar (lambda (entry) (plist-get entry :description)) entries)
                     '("Winner" "Parent" "Third")))
      (should (equal map snapshot)))))

(ert-deftest keymap-popup-descriptions-test-rebound-parent-public ()
  "A parent's moved action survives a child at the old declaration key."
  (let* ((parent (keymap-popup-descriptions-test--map "Parent" "x" 'forward-char "Parent action"))
         (child (keymap-popup-descriptions-test--map "Child" "x" 'backward-char "Child action")))
    (keymap-unset parent "x" t)
    (keymap-set parent "y" #'forward-char)
    (set-keymap-parent child parent)
    (should (eq (keymap-lookup child "x") 'backward-char))
    (should (eq (keymap-lookup child "y") 'forward-char))
    (keymap-popup-descriptions-test--popup
     child (lambda (buf)
             (with-current-buffer buf
               (should (string-match-p "x  Child action" (buffer-string)))
               (should (string-match-p "y  Parent action" (buffer-string))))))))

(defun keymap-popup-descriptions-test--aliases ()
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

(ert-deftest keymap-popup-descriptions-test-native-alias-deduplication ()
  "Equivalent spellings share one effective key and keep first precedence."
  (dolist (meta-prefix-char '(27 24))
    (dolist (pair (keymap-popup-descriptions-test--aliases))
      (let* ((a (keymap-popup-descriptions-test--map "First" (car pair) 'forward-char "Winner"))
             (b (keymap-popup-descriptions-test--map "Second" (cadr pair) 'forward-char "Loser"))
             (map (make-composed-keymap (list a b)))
             (entries (keymap-popup-descriptions-test--entries map)))
        (should (eq (keymap-lookup a (cadr pair)) #'forward-char))
        (should (= (length entries) 1))
        (should (equal (plist-get (car entries) :description) "Winner"))))))

(ert-deftest keymap-popup-descriptions-test-native-alias-popup-restriction ()
  "Composed and inherited aliases cannot bypass the winning popup policy."
  (dolist (meta-prefix-char '(27 24))
    (dolist (pair (keymap-popup-descriptions-test--aliases))
      (dolist (inherited '(nil t))
        (with-temp-buffer
          (insert "ab")
          (goto-char (point-min))
          (let* ((a (keymap-popup-descriptions-test--map
                     "First" (car pair) 'forward-char "Winner" :inapt-if #'always))
                 (b (keymap-popup-descriptions-test--map
                     "Second" (cadr pair) 'forward-char "Loser" :stay-open t))
                 (map (if inherited
                          (progn (set-keymap-parent a b) a)
                        (make-composed-keymap (list a b)))))
            (should (eq (keymap-lookup a (cadr pair)) #'forward-char))
            (keymap-popup-descriptions-test--popup
             map (lambda (buf)
                   (let ((wrapper (keymap-popup--active-get buf :wrapper-map))
                         (rows (keymap-popup--active-get buf :descriptions)))
                     ;; Exercise the actual wrapper before policy assertions:
                     ;; duplicate alias wrappers used to run the losing action.
                     (dolist (key pair)
                       (call-interactively (keymap-lookup wrapper key))
                       (should (= (point) 1))
                       (should (keymap-popup--inapt-key-p buf key))
                       (should-not (keymap-popup--keep-popup-p rows key)))
                     (should (eq (keymap-popup--find-entry-by-key rows (car pair))
                                 (keymap-popup--find-entry-by-key rows (cadr pair)))))
                   (with-current-buffer buf
                     (should (string-match-p "Winner" (buffer-string)))
                     (should-not (string-match-p "Loser" (buffer-string))))))
            ;; The winning restriction remains popup-only.
            (call-interactively (keymap-lookup map (car pair)))
            (should (= (point) 2))))))))

(ert-deftest keymap-popup-descriptions-test-filtered-component-policy ()
  "Native component fallback selects labels and policy before deduplication."
  (dolist (pair '(("f" "f") ("M-f" "ESC f") ("ESC f" "M-f")))
    (dolist (enabled '(nil t))
      (with-temp-buffer
        (insert "ab")
        (goto-char (point-min))
        (let* ((a (keymap-popup-descriptions-test--map
                   "First" (car pair) 'forward-char "First action"
                   :if (lambda () enabled) :stay-open t))
               (b (keymap-popup-descriptions-test--map
                   "Second" (cadr pair) 'forward-char "Second action"
                   :inapt-if #'always))
               (map (make-composed-keymap (list a b))))
          (keymap-set a (car pair)
                      `(menu-item "" forward-char
                                  :filter ,(lambda (command)
                                             (and enabled command))))
          (should (eq (keymap-lookup map (car pair)) #'forward-char))
          (keymap-popup-descriptions-test--popup
           map (lambda (buf)
                 (let ((wrapper (keymap-popup--active-get buf :wrapper-map))
                       (rows (keymap-popup--active-get buf :descriptions)))
                   (dolist (key pair)
                     (goto-char (point-min))
                     (call-interactively (keymap-lookup wrapper key))
                     (should (= (point) (if enabled 2 1)))
                     (should (eq (not (null (keymap-popup--keep-popup-p rows key)))
                                 enabled))
                     (should (eq (not (null (keymap-popup--inapt-key-p buf key)))
                                 (not enabled)))))
                 (with-current-buffer buf
                   (should (string-match-p
                            (if enabled "First action" "Second action")
                            (buffer-string)))
                   (should-not (string-match-p
                                (if enabled "Second action" "First action")
                                (buffer-string))))))
          ;; The selected component's restriction is popup-only.
          (goto-char (point-min))
          (call-interactively (keymap-lookup map (cadr pair)))
          (should (= (point) 2)))))))

(ert-deftest keymap-popup-descriptions-test-filtered-child-masks-parent ()
  "A filtered inherited binding masks its parent rather than falling back."
  (dolist (pair '(("f" "f") ("M-f" "ESC f") ("ESC f" "M-f")))
    (let* ((a (keymap-popup-descriptions-test--map
               "Child" (car pair) 'forward-char "Hidden child" :if #'ignore))
           (b (keymap-popup-descriptions-test--map
               "Parent" (cadr pair) 'forward-char "Masked parent" :inapt-if #'always)))
      (keymap-set a (car pair) '(menu-item "" forward-char :filter ignore))
      (set-keymap-parent a b)
      (should-not (keymap-lookup a (car pair)))
      (keymap-popup-descriptions-test--popup
       a (lambda (buf)
           (let* ((rows (keymap-popup--active-get buf :descriptions))
                  (entry (keymap-popup--find-entry-by-key rows (car pair))))
             (should (equal (plist-get entry :description) "Hidden child")))
           (with-current-buffer buf
             (should-not (string-match-p "Masked parent" (buffer-string)))
             (should-not (string-match-p "Hidden child" (buffer-string)))))))))

(ert-deftest keymap-popup-descriptions-test-filtered-component-parent-paths ()
  "Inherited metadata must reach the binding through an effective component."
  (dolist (shared '(nil t))
    (with-temp-buffer
      (insert "ab")
      (goto-char (point-min))
      (let* ((parent (keymap-popup-descriptions-test--map
                      "Parent" "M-f" 'forward-char "Parent action" :stay-open t))
             (a (make-sparse-keymap))
             (b (if shared (make-sparse-keymap)
                  (keymap-popup-descriptions-test--map
                   "Second" "ESC f" 'forward-char "Second action" :inapt-if #'always))))
        (keymap-set a "M-f" '(menu-item "" forward-char :filter ignore))
        (set-keymap-parent a parent)
        (when shared (set-keymap-parent b parent))
        (let ((map (make-composed-keymap (list a b))))
          (should-not (keymap-lookup a "M-f"))
          (should (eq (keymap-lookup map "M-f") #'forward-char))
          (keymap-popup-descriptions-test--popup
           map (lambda (buf)
                 (call-interactively
                  (keymap-lookup (keymap-popup--active-get buf :wrapper-map) "M-f"))
                 (should (= (point) (if shared 2 1)))
                 (with-current-buffer buf
                   (should (string-match-p
                            (if shared "Parent action" "Second action")
                            (buffer-string)))
                   (should-not (string-match-p
                                (if shared "Second action" "Parent action")
                                (buffer-string)))))))))))

(ert-deftest keymap-popup-descriptions-test-presentation-is-not-dispatch ()
  "A presentation predicate cannot select another component's metadata."
  (with-temp-buffer
    (insert "ab")
    (goto-char (point-min))
    (let* ((a (keymap-popup-descriptions-test--map
               "First" "f" 'forward-char "Hidden winner" :if #'ignore))
           (b (keymap-popup-descriptions-test--map
               "Second" "f" 'forward-char "Visible loser" :inapt-if #'always))
           (map (make-composed-keymap (list a b))))
      (should (eq (keymap-lookup a "f") #'forward-char))
      (keymap-popup-descriptions-test--popup
       map (lambda (buf)
             (let* ((rows (keymap-popup--active-get buf :descriptions))
                    (entry (keymap-popup--find-entry-by-key rows "f")))
               (should (equal (plist-get entry :description) "Hidden winner")))
             (call-interactively
              (keymap-lookup (keymap-popup--active-get buf :wrapper-map) "f"))
             (should (= (point) 2))
             (with-current-buffer buf
               (should-not (string-match-p "Visible loser" (buffer-string)))
               (should-not (string-match-p "Hidden winner" (buffer-string)))))))))

(ert-deftest keymap-popup-descriptions-test-distinct-native-keys ()
  "Keep genuinely distinct keys despite identical command identities."
  (dolist (case '((27 "TAB" "<tab>") (27 "RET" "<return>")
                  (27 "M-<left>" "ESC <left>") (24 "M-f" "ESC f")))
    (pcase-let* ((`(,meta-prefix-char ,first ,second) case)
                 (a (keymap-popup-descriptions-test--map
                     "First" first 'forward-char "First"))
                 (b (keymap-popup-descriptions-test--map
                     "Second" second 'forward-char "Second"))
                 (map (make-composed-keymap (list a b)))
                 (entries (keymap-popup-descriptions-test--entries map)))
      (should-not (eq (keymap-lookup a second) #'forward-char))
      (should (equal (mapcar (lambda (entry) (plist-get entry :description)) entries)
                     '("First" "Second"))))))

(ert-deftest keymap-popup-descriptions-test-unannotated-child ()
  "Inherited metadata is collected only once through an unannotated map."
  (let* ((parent (keymap-popup-descriptions-test--map "Parent" "a" 'forward-char "Forward"))
         (child (make-sparse-keymap)))
    (set-keymap-parent child parent)
    (should (= (length (keymap-popup--collect-descriptions child)) 1))))

(ert-deftest keymap-popup-descriptions-test-unicode-exact-fit ()
  "Display-width padding keeps every neighboring label at an exact fit."
  (dolist (label '("界" "é" "ASCII"))
    (let* ((text (propertize label 'face 'success 'help-echo "Original"))
           (rows `(((:name ,label :entries ((:key "a" :description ,text)))
                    (:name "Next" :entries ((:key "b" :description "OK"))))))
           (columns (keymap-popup--rows-to-columns rows))
           (width (+ (apply #'+ (keymap-popup--global-col-widths columns))
                     (string-width keymap-popup--column-separator)))
           (output (keymap-popup--render rows nil width))
           (lines (split-string output "\n" t))
           (position (string-match (regexp-quote label) (cadr lines))))
      (should (string-match-p "b  OK" (cadr lines)))
      (should (= (string-width
                  (substring (car lines) 0 (string-match "Next" (car lines))))
                 (+ (car (keymap-popup--global-col-widths columns))
                    (string-width keymap-popup--column-separator))))
      (should (= (- (string-width
                     (substring (cadr lines) 0 (string-match "b  OK" (cadr lines))))
                    2)
                 (+ (car (keymap-popup--global-col-widths columns))
                    (string-width keymap-popup--column-separator))))
      (should (seq-every-p (lambda (line) (<= (string-width line) width)) lines))
      (should (equal (get-text-property position 'help-echo (cadr lines)) "Original"))
      (should (eq (get-text-property position 'face (cadr lines)) 'success)))))

(ert-deftest keymap-popup-descriptions-test-unicode-key-alignment ()
  "Wide and combining keys align by display columns, not character count."
  (let* ((group '(:entries ((:key "界" :description "Wide")
                           (:key "é" :description "Combining")
                           (:key "a" :description "ASCII"))))
         (lines (keymap-popup--render-group-lines group)))
    (should (equal
             (cl-mapcar (lambda (line label)
                          (string-width (substring line 0 (string-match label line))))
                        lines '("Wide" "Combining" "ASCII"))
             '(6 6 6)))))

(ert-deftest keymap-popup-descriptions-test-deferred-annotation-keys ()
  "Collect nil keys unchanged, then resolve every described command."
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" #'forward-char)
    (keymap-set map "b" #'backward-char)
    (keymap-popup-attach map '(forward-char "Forward" backward-char "Backward"))
    (let* ((collected (keymap-popup--collect-descriptions map))
           (entries (plist-get (caar collected) :entries)))
      (should (= (length entries) 2))
      (should (seq-every-p (lambda (entry) (null (plist-get entry :key))) entries))
      (should (equal (mapcar (lambda (entry) (plist-get entry :key))
                            (keymap-popup-descriptions-test--entries map))
                     '("a" "b")))
      (should (seq-every-p (lambda (entry) (null (plist-get entry :key))) entries)))))

(ert-deftest keymap-popup-descriptions-test-hidden-filter-fallback ()
  "Hidden inherited bindings retain stored keys for later rendering."
  (let* ((enabled nil)
         (predicate (lambda () enabled))
         (parent (keymap-popup-descriptions-test--map
                  "Parent" "p" 'forward-char "Parent action" :if predicate))
         (child (keymap-popup-descriptions-test--map "Child" "c" 'backward-char "Child action")))
    (keymap-set parent "p" `(menu-item "" forward-char
                                      :filter ,(lambda (command)
                                                 (and enabled command))))
    (set-keymap-parent child parent)
    (let* ((rows (keymap-popup--resolve-descriptions
                  (keymap-popup--collect-descriptions child) child))
           (entry (keymap-popup--find-entry-by-key rows "p")))
      (should entry)
      (should-not (keymap-lookup child "p"))
      (should-not (string-match-p "Parent action" (keymap-popup--render rows)))
      (setq enabled t)
      (should (eq (keymap-lookup child "p") 'forward-char))
      (should (string-match-p "Parent action" (keymap-popup--render rows))))))

(ert-deftest keymap-popup-descriptions-test-effective-child-shadowing ()
  "Resolved live collisions keep the child group and drop the parent group."
  (let* ((parent (keymap-popup-descriptions-test--map "Parent" "a" 'forward-char "Parent action"))
         (child (keymap-popup-descriptions-test--map "Child" "a" 'backward-char "Child action")))
    (set-keymap-parent child parent)
    (let ((rows (keymap-popup--resolve-descriptions
                 (keymap-popup--collect-descriptions child) child)))
      (should (equal (mapcar (lambda (row) (plist-get (car row) :name)) rows)
                     '("Child")))
      (should (equal (plist-get (car (plist-get (caar rows) :entries)) :description)
                     "Child action")))))

(ert-deftest keymap-popup-descriptions-test-alias-rows ()
  "Deduplication compares events even when declared spellings differ."
  (let ((rows '(((:entries ((:key "TAB" :description "First")
                            (:key "C-i" :description "Second")
                            (:key nil :description "Annotation one")
                            (:key nil :description "Annotation two")))))))
    (should (equal
             (mapcar (lambda (entry) (plist-get entry :description))
                     (plist-get (caar (keymap-popup--dedupe-descriptions rows)) :entries))
             '("First" "Annotation one" "Annotation two")))))

(ert-deftest keymap-popup-descriptions-test-alias-popup-policy ()
  "Native key spelling finds stay-open policy for an alias declaration."
  (let* ((map (keymap-popup-descriptions-test--map
               "Actions" "C-i" 'forward-char "Forward" :stay-open t))
         (rows (keymap-popup--resolve-descriptions
                (keymap-popup--collect-descriptions map) map)))
    (should (keymap-popup--keep-popup-p rows "TAB"))
    (should (equal (plist-get (keymap-popup--find-entry-by-key rows "TAB") :key)
                   "C-i"))))

(ert-deftest keymap-popup-descriptions-test-unicode-wrap-and-truncate ()
  "Narrow widths wrap groups, and genuinely overwide labels truncate."
  (let* ((truncate-string-ellipsis "…")
         (rows '(((:name "界" :entries ((:key "a" :description "界界界")))
                  (:name "Next" :entries ((:key "b" :description "OK"))))))
         (wrapped (keymap-popup--render rows nil 12))
         (truncated (keymap-popup--render rows nil 8)))
    (should (string-match-p "界界界\nNext\n" wrapped))
    (should (string-match-p "b  OK" wrapped))
    (should (string-match-p "…" truncated))
    (should (string-match-p "b  OK" truncated))
    (should (seq-every-p (lambda (line) (<= (string-width line) 8))
                        (split-string truncated "\n" t)))))

(defun keymap-popup-descriptions-test--child-local-rebinding (annotated)
  "Check inherited labels and policy after child-local ANNOTATED rebinding."
  (with-temp-buffer
    (insert "ab")
    (goto-char (point-min))
    (let ((parent (make-sparse-keymap))
          (child (make-sparse-keymap)))
      (keymap-set parent "x" #'forward-char)
      (keymap-popup-attach
       parent (if annotated
                  '(forward-char ("Inherited forward" :inapt-if always))
                '("x" ("Inherited forward" forward-char :inapt-if always))))
      (set-keymap-parent child parent)
      (keymap-set child "x" #'backward-char)
      (keymap-set child "y" #'forward-char)
      (keymap-set child "z" #'ignore)
      (keymap-popup-attach child '("z" ("Child action" ignore)))
      (should (eq (keymap-lookup parent "x") #'forward-char))
      (should-not (keymap-lookup parent "y"))
      (should (eq (keymap-lookup child "x") #'backward-char))
      (should (eq (keymap-lookup child "y") #'forward-char))
      (keymap-popup-descriptions-test--popup
       child (lambda (buf)
               (call-interactively
                (keymap-lookup (keymap-popup--active-get buf :wrapper-map) "y"))
               (should (= (point) 1))
               (should (keymap-popup--inapt-key-p buf "y"))
               (with-current-buffer buf
                 (should (string-match-p "y  Inherited forward" (buffer-string))))))
      (call-interactively (keymap-lookup child "y"))
      (should (= (point) 2)))))

(ert-deftest keymap-popup-descriptions-test-child-local-keyed-rebinding ()
  "Inherited keyed metadata follows a command moved only in the child."
  (keymap-popup-descriptions-test--child-local-rebinding nil))

(ert-deftest keymap-popup-descriptions-test-child-local-annotated-rebinding ()
  "Inherited command annotations follow a command moved only in the child."
  (keymap-popup-descriptions-test--child-local-rebinding t))

(ert-deftest keymap-popup-descriptions-test-runtime-submenu-native-winner ()
  "Runtime submenu metadata follows native fallback and parent restoration."
  (let* ((enabled nil)
         (first-target (keymap-popup-descriptions-test--map
                        "First" "a" 'ignore "First target body"))
         (second-target (keymap-popup-descriptions-test--map
                         "Second" "b" 'ignore "Second target body"))
         (a (make-sparse-keymap))
         (b (make-sparse-keymap)))
    (keymap-set a "f" `(menu-item "" ,first-target
                                 :filter ,(lambda (target) (and enabled target))))
    (keymap-set b "f" second-target)
    (keymap-popup-attach a (list "f" (list "First submenu" :keymap first-target)))
    (keymap-popup-attach b (list "f" (list "Second submenu" :keymap second-target)))
    (let ((map (make-composed-keymap (list a b))))
      (should-not (keymap-lookup a "f"))
      (should (eq (keymap-lookup map "f") second-target))
      (keymap-popup-descriptions-test--popup
       map (lambda (buf)
             (let ((open (keymap-lookup (keymap-popup--active-get buf :wrapper-map) "f")))
               (dolist (value '(nil t nil))
                 (setq enabled value)
                 (keymap-popup--refresh buf)
                 (let ((target (if enabled first-target second-target))
                       (entry (keymap-popup--find-entry-by-key
                               (keymap-popup--active-get buf :descriptions) "f")))
                   (should-not (plist-get entry :command))
                   (should (eq (plist-get entry :target) target))
                   (with-current-buffer buf
                     (should (string-match-p
                              (if enabled "First submenu" "Second submenu")
                              (buffer-string)))
                     (should-not (string-match-p
                                  (if enabled "Second submenu" "First submenu")
                                  (buffer-string))))
                   ;; Retain the original handler, not merely a fresh lookup.
                   (call-interactively open)
                   (should (eq (keymap-popup--active-get buf :keymap) target)))
                 ;; A filter changes while the parent is stacked.  Its real
                 ;; transient exit callback must restore a fresh projection.
                 (setq enabled (not enabled))
                 (cl-letf (((symbol-function 'this-command-keys-vector)
                            (lambda () [?q])))
                   (funcall (keymap-popup--active-get buf :exit-function)))
                 (should (eq (keymap-popup--active-get buf :keymap) map))
                 (with-current-buffer buf
                   (should (string-match-p
                            (if enabled "First submenu" "Second submenu")
                            (buffer-string)))))))))))

(ert-deftest keymap-popup-descriptions-test-runtime-submenu-popup-only ()
  "An attached submenu can still override a command or an unbound key."
  (dolist (binding '(nil ignore))
    (let ((map (make-sparse-keymap))
          (target (keymap-popup-descriptions-test--map
                   "Target" "a" 'ignore "Target body")))
      (when binding (keymap-set map "s" binding))
      (keymap-popup-attach map (list "s" (list "Submenu" :keymap target)))
      (keymap-popup-descriptions-test--popup
       map (lambda (buf)
             (call-interactively
              (keymap-lookup (keymap-popup--active-get buf :wrapper-map) "s"))
             (should (eq (keymap-popup--active-get buf :keymap) target))))
      (should (eq (keymap-lookup map "s") binding)))))

(ert-deftest keymap-popup-descriptions-test-runtime-switch-rejected ()
  "Runtime attachments cannot create switch commands."
  (should-error
   (keymap-popup-attach (make-sparse-keymap) '("s" ("Switch" :switch some-switch)))))

(ert-deftest keymap-popup-descriptions-test-live-composed-policy ()
  "A persistent toggle reselects the live component's label and restriction."
  (dolist (initial '(nil t))
    (with-temp-buffer
      (insert "abc")
      (goto-char (point-min))
      (let* ((enabled initial)
             (toggle (lambda () (interactive) (setq enabled (not enabled))))
             (first (keymap-popup-descriptions-test--map
                     "First" "x" 'forward-char "Restricted first"
                     :if (lambda () enabled) :inapt-if #'always))
             (second (keymap-popup-descriptions-test--map
                      "Second" "x" 'forward-char "Available second"))
             (control (keymap-popup-descriptions-test--map
                       "Control" "t" toggle "Toggle" :stay-open t))
             (map (make-composed-keymap (list first second control)))
             (keymap-popup-persistent t))
        (keymap-set first "x" `(menu-item "" forward-char
                                         :filter ,(lambda (cmd) (and enabled cmd))))
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (keymap-popup-descriptions-test--popup
           map (lambda (buf)
                 (dotimes (_ 2)
                   ;; Native pre/post-command hooks and the suffix's normal
                   ;; refresh must preserve the popup and reselect metadata.
                   (execute-kbd-macro (kbd "t"))
                   (with-current-buffer buf
                     (should (string-match-p
                              (if enabled "Restricted first" "Available second")
                              (buffer-string)))
                     (should-not (string-match-p
                                  (if enabled "Available second" "Restricted first")
                                  (buffer-string))))
                   (goto-char (point-min))
                   (execute-kbd-macro (kbd "x"))
                   (should (= (point) (if enabled 1 2)))
                   (should (buffer-live-p buf))
                   (should (eq (keymap-lookup map "x") #'forward-char))))))))))

(ert-deftest keymap-popup-descriptions-test-live-policy-before-refresh ()
  "Native wrapper lookup follows the winner even before a display refresh."
  (with-temp-buffer
    (insert "ab")
    (let* ((enabled nil)
           (first (keymap-popup-descriptions-test--map
                   "First" "x" 'forward-char "Restricted first"
                   :if (lambda () enabled) :inapt-if #'always))
           (second (keymap-popup-descriptions-test--map
                    "Second" "x" 'forward-char "Available second"))
           (map (make-composed-keymap (list first second))))
      (keymap-set first "x" `(menu-item "" forward-char
                                       :filter ,(lambda (cmd) (and enabled cmd))))
      (keymap-popup-descriptions-test--popup
       map (lambda (buf)
             (let* ((wrapper (keymap-popup--active-get buf :wrapper-map))
                    (action (keymap-lookup wrapper "x")))
               (dolist (value '(t nil))
                 (setq enabled value)
                 (goto-char (point-min))
                 (should (commandp (keymap-lookup wrapper "x")))
                 (call-interactively action)
                 (should (= (point) (if enabled 1 2))))))))))

(ert-deftest keymap-popup-descriptions-test-distinct-command-live-filter ()
  "A native filter toggle guards a previously shadowed, different command."
  (with-temp-buffer
    (insert "abcd")
    (let* ((enabled nil)
           (first (keymap-popup-descriptions-test--map
                   "First" "x" 'forward-char "Restricted first"
                   :inapt-if #'always))
           (second (keymap-popup-descriptions-test--map
                    "Second" "x" 'backward-char "Available second"))
           (toggle (lambda () (interactive) (setq enabled (not enabled))))
           (control (keymap-popup-descriptions-test--map
                     "Control" "t" toggle "Toggle" :stay-open t))
           (map (make-composed-keymap (list first second control)))
           (keymap-popup-persistent t))
      (keymap-set first "x" `(menu-item "" forward-char
                                       :filter ,(lambda (cmd) (and enabled cmd))))
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (keymap-popup-descriptions-test--popup
         map (lambda (buf)
               (dolist (value '(nil t nil))
                 (unless (eq enabled value)
                   (execute-kbd-macro (kbd "t")))
                 (should (eq enabled value))
                 (with-current-buffer buf
                   (should (string-match-p
                            (if enabled "Restricted first" "Available second")
                            (buffer-string)))
                   (should-not (string-match-p
                                (if enabled "Available second" "Restricted first")
                                (buffer-string))))
                 (goto-char 2)
                 (execute-kbd-macro (kbd "x"))
                 (should (= (point) (if enabled 2 1)))
                 (should (buffer-live-p buf))
                 (should (eq (keymap-lookup map "x")
                             (if enabled #'forward-char #'backward-char)))))))
      ;; The source keymap never acquires popup-only restrictions.
      (setq enabled t)
      (goto-char 2)
      (call-interactively (keymap-lookup map "x"))
      (should (= (point) 3)))))

(ert-deftest keymap-popup-descriptions-test-potential-stored-and-resolved-keys ()
  "Keep stored and resolved handlers, but select only current native policy."
  (dolist (alias '(nil t))
    (with-temp-buffer
      (insert "abcd")
      (let* ((enabled nil)
             (first (keymap-popup-descriptions-test--map
                     "First" "x" 'forward-char "Restricted first"
                     :inapt-if #'always))
             (second (keymap-popup-descriptions-test--map
                      "Second" "x" 'backward-char "Available second"))
             (map (make-composed-keymap (list first second))))
        (keymap-set first "x" `(menu-item "" forward-char
                                         :filter ,(lambda (cmd) (and enabled cmd))))
        (when alias (keymap-set first "y" #'forward-char))
        (keymap-set second "z" #'backward-char)
        (keymap-popup-descriptions-test--popup
         map (lambda (buf)
               (let* ((wrapper (keymap-popup--active-get buf :wrapper-map))
                      (stored (keymap-lookup wrapper "x"))
                      (resolved (and alias (keymap-lookup wrapper "y"))))
                 (should (commandp stored))
                 (should (eq (keymap-lookup wrapper "z") #'backward-char))
                 ;; No refresh or new wrapper: both retained and fresh lookups
                 ;; must reconsider the currently effective native binding.
                 (dolist (value '(nil t nil))
                   (setq enabled value)
                   (dolist (action (list stored (keymap-lookup wrapper "x")))
                     (goto-char 2)
                     (call-interactively action)
                     (should (= (point) (if enabled 2 1))))
                   (when alias
                     (dolist (action (list resolved (keymap-lookup wrapper "y")))
                       (goto-char 2)
                       (call-interactively action)
                       ;; Once x wins, y has no selected metadata and is native.
                       (should (= (point) (if enabled 3 2)))))))))
        (when alias
          (goto-char 2)
          (call-interactively (keymap-lookup map "y"))
          (should (= (point) 3)))))))

(ert-deftest keymap-popup-descriptions-test-live-alias-roundtrip ()
  "Native filter toggles move rendered restrictions to an existing alias."
  (dolist (initial '(t nil))
    (with-temp-buffer
      (insert "abcdef")
      (let* ((enabled initial)
             (first (keymap-popup-descriptions-test--map
                     "First" "x" 'forward-char "Restricted first"
                     :inapt-if #'always))
             (second (keymap-popup-descriptions-test--map
                      "Second" "x" 'backward-char "Available second"))
             (control (keymap-popup-descriptions-test--map
                       "Control" "t"
                       (lambda () (interactive) (setq enabled (not enabled)))
                       "Toggle" :stay-open t))
             (map (make-composed-keymap (list first second control)))
             (keymap-popup-persistent t))
        (keymap-set first "y" #'forward-char)
        (keymap-set first "x" `(menu-item "" forward-char
                                         :filter ,(lambda (cmd) (and enabled cmd))))
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (keymap-popup-descriptions-test--popup
           map (lambda (buf)
                 (dolist (state (list initial (not initial) initial))
                   (unless (eq enabled state)
                     (execute-kbd-macro (kbd "t")))
                   (should (eq enabled state))
                   (should (keymap-popup--inapt-key-p buf (if enabled "x" "y")))
                   (with-current-buffer buf
                     (should (string-match-p
                              (if enabled "x  Restricted first" "y  Restricted first")
                              (buffer-string))))
                   (goto-char 3)
                   (execute-kbd-macro (kbd "x"))
                   (should (= (point) (if enabled 3 2)))
                   (goto-char 3)
                   (execute-kbd-macro (kbd "y"))
                   (should (= (point) (if enabled 4 3)))
                   (should (buffer-live-p buf))))))
        ;; Popup refusal must never modify direct native dispatch.
        (dolist (state '(nil t))
          (setq enabled state)
          (goto-char 3)
          (call-interactively (keymap-lookup map "x"))
          (should (= (point) (if enabled 4 2)))
          (goto-char 3)
          (call-interactively (keymap-lookup map "y"))
          (should (= (point) 4)))))))

(ert-deftest keymap-popup-descriptions-test-all-potential-alias-handlers ()
  "All known aliases retain handlers; only the current winner is guarded."
  (dolist (initial '(t nil))
    (with-temp-buffer
      (insert "abcdef")
      (let* ((enabled initial)
             (first (keymap-popup-descriptions-test--map
                     "First" "x" 'forward-char "Restricted first"
                     :inapt-if #'always))
             (second (keymap-popup-descriptions-test--map
                      "Second" "x" 'backward-char "Available second"))
             (map (make-composed-keymap (list first second))))
        (keymap-set first "x" `(menu-item "" forward-char
                                         :filter ,(lambda (cmd) (and enabled cmd))))
        (keymap-set first "y" #'forward-char)
        (keymap-set first "z" #'forward-char)
        (keymap-set second "u" #'backward-char)
        (keymap-popup-descriptions-test--popup
         map (lambda (buf)
               (let* ((wrapper (keymap-popup--active-get buf :wrapper-map))
                      ;; Capture installed payloads, not filtered native results.
                      (handlers
                       (mapcar (lambda (key)
                                 (cons key (nth 2 (cdr (assq (aref key 0)
                                                            (cdr wrapper))))))
                               '("x" "y" "z"))))
                 (dolist (pair handlers)
                   (should (commandp (cdr pair))))
                 (dolist (state (list initial (not initial) initial))
                   (setq enabled state)
                   (let* ((entries (keymap-popup-descriptions-test--entries map))
                          (restricted (seq-find
                                       (lambda (entry)
                                         (eq (plist-get entry :command) #'forward-char))
                                       entries))
                          (winner (plist-get restricted :key)))
                     (should (member winner (if enabled '("x") '("y" "z"))))
                     ;; No display refresh precedes these fresh/captured calls.
                     (pcase-dolist (`(,key . ,handler) handlers)
                       (let ((fresh (keymap-lookup wrapper key)))
                         (when (and (not (equal key winner)) (not (equal key "x")))
                           (should (eq fresh (keymap-lookup map key))))
                         (dolist (action (list handler fresh))
                           (goto-char 3)
                           (call-interactively action)
                           (should (= (point) (cond ((equal key winner) 3)
                                                    ((equal key "x") 2)
                                                    (t 4)))))))
                     (should (eq (keymap-lookup wrapper "u") #'backward-char)))))))))))

(provide 'keymap-popup-descriptions-tests)
;;; keymap-popup-descriptions-tests.el ends here
