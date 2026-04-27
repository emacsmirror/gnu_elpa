;;; keymap-popup-tests.el --- Tests -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name
       "../keymap-popup.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; Parser tests

(ert-deftest keymap-popup-test-parse-suffix-entry ()
  "A plain suffix entry parses to type suffix."
  (let ((result (keymap-popup--parse-entry "c" '("Comment" forgejo-view-comment))))
    (should (equal (plist-get result :key) "c"))
    (should (equal (plist-get result :description) "Comment"))
    (should (equal (plist-get result :command) 'forgejo-view-comment))
    (should (equal (plist-get result :type) 'suffix))))

(ert-deftest keymap-popup-test-parse-suffix-with-if ()
  "A suffix with :if stores the predicate."
  (let* ((pred (lambda () t))
         (result (keymap-popup--parse-entry "b" `("Browse" forgejo-browse :if ,pred))))
    (should (equal (plist-get result :type) 'suffix))
    (should (eq (plist-get result :if) pred))))

(ert-deftest keymap-popup-test-parse-switch-entry ()
  "A switch entry parses to type switch with variable."
  (let ((result (keymap-popup--parse-entry "v" '("Verbose" :switch my-verbose-var))))
    (should (equal (plist-get result :type) 'switch))
    (should (equal (plist-get result :variable) 'my-verbose-var))))

(ert-deftest keymap-popup-test-parse-option-entry ()
  "An option entry parses with variable, reader, prompt."
  (let ((result (keymap-popup--parse-entry "n" '("Count" :option my-count-var
                                                 :reader read-number :prompt "Count: "))))
    (should (equal (plist-get result :type) 'option))
    (should (equal (plist-get result :variable) 'my-count-var))
    (should (equal (plist-get result :reader) 'read-number))
    (should (equal (plist-get result :prompt) "Count: "))))

(ert-deftest keymap-popup-test-parse-dynamic-description ()
  "Dynamic description (function) is preserved."
  (let* ((desc-fn (lambda () "dynamic"))
         (result (keymap-popup--parse-entry "d" `(,desc-fn some-command))))
    (should (functionp (plist-get result :description)))))

(ert-deftest keymap-popup-test-parse-bindings-groups ()
  "Parse binding list with :group keywords into rows of groups."
  (let* ((rows (keymap-popup--parse-bindings
                '(:group "Actions"
			 "c" ("Comment" forgejo-view-comment)
			 "r" ("Reply" forgejo-issue-reply)
			 :group "Navigate"
			 "g" ("Refresh" forgejo-view-refresh)
			 "q" ("Quit" quit-window))))
         (row (car rows)))
    ;; One row with two groups
    (should (= (length rows) 1))
    (should (= (length row) 2))
    (should (equal (plist-get (car row) :name) "Actions"))
    (should (= (length (plist-get (car row) :entries)) 2))
    (should (equal (plist-get (cadr row) :name) "Navigate"))
    (should (= (length (plist-get (cadr row) :entries)) 2))))

(ert-deftest keymap-popup-test-parse-bindings-no-group ()
  "Entries before any :group go into unnamed default group."
  (let* ((rows (keymap-popup--parse-bindings
                '("c" ("Comment" forgejo-view-comment)
                  "r" ("Reply" forgejo-issue-reply))))
         (row (car rows)))
    (should (= (length rows) 1))
    (should (= (length row) 1))
    (should (null (plist-get (car row) :name)))
    (should (= (length (plist-get (car row) :entries)) 2))))

(ert-deftest keymap-popup-test-parse-bindings-rows ()
  "Parse binding list with :row keyword into multiple rows."
  (let ((rows (keymap-popup--parse-bindings
               '(:group "A"
			"a" ("Aaa" ignore)
			:group "B"
			"b" ("Bbb" ignore)
			:row
			:group "C"
			"c" ("Ccc" ignore)))))
    (should (= (length rows) 2))
    ;; First row has 2 groups
    (should (= (length (car rows)) 2))
    ;; Second row has 1 group
    (should (= (length (cadr rows)) 1))
    (should (equal (plist-get (caar (cdr rows)) :name) "C"))))

;;; Infix generator tests

(ert-deftest keymap-popup-test-switch-forms ()
  "Generate defvar-local and toggle defun for a switch."
  (let* ((entry '(:key "v" :description "Verbose" :type switch
                       :variable my-verbose-var))
         (forms (keymap-popup--switch-forms 'test-map entry)))
    (should (= (length forms) 2))
    (should (eq (car (nth 0 forms)) 'defvar-local))
    (should (eq (cadr (nth 0 forms)) 'my-verbose-var))
    (should (eq (car (nth 1 forms)) 'defun))
    (should (eq (cadr (nth 1 forms)) 'test-map--toggle-my-verbose-var))))

(ert-deftest keymap-popup-test-option-forms ()
  "Generate defvar-local and setter defun for an option."
  (let* ((entry '(:key "n" :description "Count" :type option
                       :variable my-count-var :reader read-number :prompt "Count: "))
         (forms (keymap-popup--option-forms 'test-map entry)))
    (should (= (length forms) 2))
    (should (eq (car (nth 0 forms)) 'defvar-local))
    (should (eq (cadr (nth 0 forms)) 'my-count-var))
    (should (eq (car (nth 1 forms)) 'defun))
    (should (eq (cadr (nth 1 forms)) 'test-map--set-my-count-var))))

(ert-deftest keymap-popup-test-entry-command ()
  "Derive correct command symbol for each entry type."
  (should (eq (keymap-popup--entry-command 'map '(:type suffix :command my-cmd))
              'my-cmd))
  (should (eq (keymap-popup--entry-command 'map '(:type switch :variable my-var))
              'map--toggle-my-var))
  (should (eq (keymap-popup--entry-command 'map '(:type option :variable my-var))
              'map--set-my-var)))

;;; Macro tests

(ert-deftest keymap-popup-test-macro-creates-keymap ()
  "Macro creates a valid keymap with correct bindings."
  (eval '(keymap-popup-define keymap-popup--test-map-1
           "Test keymap."
           :group "Actions"
           "c" ("Comment" ignore)
           "q" ("Quit" quit-window))
        t)
  (should (keymapp keymap-popup--test-map-1))
  (should (eq (keymap-lookup keymap-popup--test-map-1 "c") #'ignore))
  (should (eq (keymap-lookup keymap-popup--test-map-1 "q") #'quit-window)))

(ert-deftest keymap-popup-test-macro-stores-descriptions ()
  "Macro stores descriptions as rows of groups."
  (eval '(keymap-popup-define keymap-popup--test-map-2
           "Test."
           :group "A"
           "c" ("Comment" ignore)
           :group "B"
           "g" ("Go" ignore))
        t)
  (let* ((descs (get 'keymap-popup--test-map-2 'keymap-popup--descriptions))
         (row (car descs)))
    ;; One row with two groups
    (should (= (length descs) 1))
    (should (= (length row) 2))
    (should (equal (plist-get (car row) :name) "A"))
    (should (equal (plist-get (cadr row) :name) "B"))))

(ert-deftest keymap-popup-test-macro-switch-infix ()
  "Macro generates toggle command and binds it for switches."
  (eval '(keymap-popup-define keymap-popup--test-map-3
           "Test."
           "v" ("Verbose" :switch keymap-popup--test-sw))
        t)
  (should (boundp 'keymap-popup--test-sw))
  (should (fboundp 'keymap-popup--test-map-3--toggle-keymap-popup--test-sw)))

(ert-deftest keymap-popup-test-macro-option-infix ()
  "Macro generates setter command and binds it for options."
  (eval '(keymap-popup-define keymap-popup--test-map-4
           "Test."
           "n" ("Count" :option keymap-popup--test-opt
                :reader read-number :prompt "N: "))
        t)
  (should (boundp 'keymap-popup--test-opt))
  (should (fboundp 'keymap-popup--test-map-4--set-keymap-popup--test-opt)))

(ert-deftest keymap-popup-test-macro-lambda-command ()
  "Lambda commands bind directly in the keymap."
  (eval '(keymap-popup-define keymap-popup--test-map-5
           "Test."
           "x" ("Run" (lambda () (interactive) (message "running"))))
        t)
  (should (functionp (keymap-lookup keymap-popup--test-map-5 "x"))))

(ert-deftest keymap-popup-test-macro-no-docstring ()
  "Macro works without a docstring."
  (eval '(keymap-popup-define keymap-popup--test-map-nodoc
           :group "Actions"
           "c" ("Comment" ignore))
        t)
  (should (keymapp keymap-popup--test-map-nodoc))
  (should (eq (keymap-lookup keymap-popup--test-map-nodoc "c") #'ignore))
  (let* ((descs (get 'keymap-popup--test-map-nodoc 'keymap-popup--descriptions))
         (row (car descs)))
    (should (= (length descs) 1))
    (should (= (length row) 1))
    (should (equal (plist-get (car row) :name) "Actions"))))

(ert-deftest keymap-popup-test-macro-default-popup-key ()
  "Popup is bound to h by default."
  (eval '(keymap-popup-define keymap-popup--test-map-defkey
           "c" ("Comment" ignore))
        t)
  (should (functionp (keymap-lookup keymap-popup--test-map-defkey "h"))))

(ert-deftest keymap-popup-test-macro-custom-popup-key ()
  "Popup key can be customized with :popup-key."
  (eval '(keymap-popup-define keymap-popup--test-map-custkey
           :popup-key "?"
           "c" ("Comment" ignore))
        t)
  (should (functionp (keymap-lookup keymap-popup--test-map-custkey "?")))
  (should (null (keymap-lookup keymap-popup--test-map-custkey "h"))))

;;; Renderer tests

(ert-deftest keymap-popup-test-render-suffix ()
  "Render suffix entry with key and description."
  (let* ((rows (list (list (list :name "Actions"
                                 :entries (list (list :key "c" :description "Comment"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render "Test." rows)))
    (should (string-match-p "Test\\." output))
    (should (string-match-p "Actions" output))
    (should (string-match-p "Comment" output))))

(ert-deftest keymap-popup-test-render-switch-value ()
  "Render switch showing [on]/[off] based on variable."
  (defvar keymap-popup--test-render-sw nil)
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "v" :description "Verbose"
                                                      :type 'switch
                                                      :variable 'keymap-popup--test-render-sw))))))
         (output-off (keymap-popup--render nil rows)))
    (should (string-match-p "\\[off\\]" output-off))
    (setq keymap-popup--test-render-sw t)
    (let ((output-on (keymap-popup--render nil rows)))
      (should (string-match-p "\\[on\\]" output-on)))))

(ert-deftest keymap-popup-test-render-option-value ()
  "Render option showing =VALUE."
  (defvar keymap-popup--test-render-opt 42)
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "n" :description "Count"
                                                      :type 'option
                                                      :variable 'keymap-popup--test-render-opt))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "=42" output))))

(ert-deftest keymap-popup-test-render-if-hidden ()
  "Entry with :if returning nil is omitted."
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "b" :description "Browse"
                                                      :type 'suffix :command 'ignore
                                                      :if (lambda () nil)))))))
         (output (keymap-popup--render nil rows)))
    (should-not (string-match-p "Browse" output))))

(ert-deftest keymap-popup-test-render-if-shown ()
  "Entry with :if returning t is included."
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "b" :description "Browse"
                                                      :type 'suffix :command 'ignore
                                                      :if (lambda () t)))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "Browse" output))))

(ert-deftest keymap-popup-test-render-dynamic-description ()
  "Dynamic description function is called at render time."
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "d"
                                                      :description (lambda () "Dynamic!")
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "Dynamic!" output))))

;;; Popup function tests

(ert-deftest keymap-popup-test-prepare-buffer ()
  "Prepare-buffer creates a buffer with rendered content."
  (eval '(keymap-popup-define keymap-popup--test-popup-map
           "Popup test."
           :group "Commands"
           "c" ("Comment" ignore)
           "q" ("Quit" quit-window))
        t)
  (let ((buf (keymap-popup--prepare-buffer 'keymap-popup--test-popup-map)))
    (unwind-protect
        (with-current-buffer buf
          (should (string-match-p "Commands" (buffer-string)))
          (should (string-match-p "Comment" (buffer-string))))
      (kill-buffer buf))))

(ert-deftest keymap-popup-test-no-descriptions-error ()
  "Signal user-error when symbol has no descriptions."
  (should-error (keymap-popup 'nonexistent-symbol) :type 'user-error))


;;; Column layout tests

(ert-deftest keymap-popup-test-render-columns-side-by-side ()
  "Groups in the same row render as side-by-side columns."
  (let* ((rows (list (list (list :name "Alpha"
                                 :entries (list (list :key "a" :description "Aaa"
                                                      :type 'suffix :command 'ignore)))
                           (list :name "Beta"
                                 :entries (list (list :key "b" :description "Bbb"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows))
         (lines (split-string output "\n" t)))
    (should (string-match-p "Alpha" (car lines)))
    (should (string-match-p "Beta" (car lines)))))

(ert-deftest keymap-popup-test-render-rows-separated ()
  "Groups in different rows render on separate lines."
  (let* ((rows (list (list (list :name "Row1"
                                 :entries (list (list :key "a" :description "Aaa"
                                                      :type 'suffix :command 'ignore))))
                     (list (list :name "Row2"
                                 :entries (list (list :key "b" :description "Bbb"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "Row1" output))
    (should (string-match-p "Row2" output))
    ;; Row1 and Row2 should NOT be on the same line
    (let ((lines (split-string output "\n" t)))
      (should-not (and (string-match-p "Row1" (car lines))
                       (string-match-p "Row2" (car lines)))))))

(ert-deftest keymap-popup-test-columns-aligned-across-rows ()
  "Column positions align across rows."
  (let* ((rows (list
                ;; Row 1: short col 1, long col 2
                (list (list :name "A"
                            :entries (list (list :key "a" :description "X"
                                                 :type 'suffix :command 'ignore)))
                      (list :name "B"
                            :entries (list (list :key "b" :description "Y"
                                                 :type 'suffix :command 'ignore))))
                ;; Row 2: long col 1, short col 2
                (list (list :name "Longer Name"
                            :entries (list (list :key "c" :description "Something longer"
                                                 :type 'suffix :command 'ignore)))
                      (list :name "D"
                            :entries (list (list :key "d" :description "Z"
                                                 :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows))
         (plain (substring-no-properties output))
         (lines (split-string plain "\n" t)))
    ;; Find lines containing each second-column header
    (let ((b-pos (string-match "B" (cl-find-if (lambda (l) (string-match-p "\\bB\\b" l)) lines)))
          (d-pos (string-match "D" (cl-find-if (lambda (l) (string-match-p "\\bD\\b" l)) lines))))
      (should b-pos)
      (should d-pos)
      (should (= b-pos d-pos)))))

(ert-deftest keymap-popup-test-join-columns ()
  "Join columns pads shorter columns with blanks."
  (let* ((col-a '("Header A" "  a  Aaa" "  b  Bbb"))
         (col-b '("Header B" "  c  Ccc"))
         (widths (list (keymap-popup--column-width col-a)
                       (keymap-popup--column-width col-b)))
         (result (keymap-popup--join-columns (list col-a col-b) "  " widths)))
    (should (= (length result) 3))
    (should (string-match-p "Header A" (nth 0 result)))
    (should (string-match-p "Header B" (nth 0 result)))
    (should (string-match-p "Bbb" (nth 2 result)))))

;;; Infix detection tests

(ert-deftest keymap-popup-test-find-entry-by-key ()
  "Find entry by key in descriptions (rows of groups)."
  (let ((descs (list (list (list :name "G"
                                 :entries (list (list :key "c" :description "Comment"
                                                      :type 'suffix :command 'ignore)
                                                (list :key "v" :description "Verbose"
                                                      :type 'switch :variable 'some-var)))))))
    (should (equal (plist-get (keymap-popup--find-entry-by-key descs "c") :type) 'suffix))
    (should (equal (plist-get (keymap-popup--find-entry-by-key descs "v") :type) 'switch))
    (should (null (keymap-popup--find-entry-by-key descs "z")))))

(ert-deftest keymap-popup-test-infix-p ()
  "Correctly identify infix vs suffix entries."
  (let ((descs (list (list (list :name nil
                                 :entries (list (list :key "c" :type 'suffix :command 'ignore)
                                                (list :key "v" :type 'switch :variable 'x)
                                                (list :key "n" :type 'option :variable 'y)))))))
    (should-not (keymap-popup--infix-p descs "c"))
    (should (keymap-popup--infix-p descs "v"))
    (should (keymap-popup--infix-p descs "n"))
    (should-not (keymap-popup--infix-p descs "z"))))

;;; Integration tests

(ert-deftest keymap-popup-test-full-definition ()
  "Full keymap-popup-define with all entry types works end-to-end."
  (eval '(keymap-popup-define keymap-popup--test-full
           "Full test."
           :group "Actions"
           "c" ("Comment" ignore)
           :group "Options"
           "v" ("Verbose" :switch keymap-popup--test-full-verbose)
           "n" ("Count" :option keymap-popup--test-full-count
                :reader read-number :prompt "N: ")
           :row
           :group "Navigate"
           "b" ("Browse" ignore :if (lambda () t))
           "q" ("Quit" quit-window))
        t)
  (should (keymapp keymap-popup--test-full))
  (should (eq (keymap-lookup keymap-popup--test-full "c") #'ignore))
  (let ((descs (get 'keymap-popup--test-full 'keymap-popup--descriptions)))
    ;; Two rows
    (should (= (length descs) 2)))
  (let ((buf (keymap-popup--prepare-buffer 'keymap-popup--test-full)))
    (unwind-protect
        (with-current-buffer buf
          (should (string-match-p "Comment" (buffer-string)))
          (should (string-match-p "\\[off\\]" (buffer-string))))
      (kill-buffer buf))))

(ert-deftest keymap-popup-test-switch-toggle-roundtrip ()
  "Toggle command flips buffer-local variable."
  (eval '(keymap-popup-define keymap-popup--test-rt
           "Test." "v" ("Verbose" :switch keymap-popup--test-rt-sw))
        t)
  (with-temp-buffer
    (should (null keymap-popup--test-rt-sw))
    (funcall-interactively #'keymap-popup--test-rt--toggle-keymap-popup--test-rt-sw)
    (should (eq keymap-popup--test-rt-sw t))
    (funcall-interactively #'keymap-popup--test-rt--toggle-keymap-popup--test-rt-sw)
    (should (null keymap-popup--test-rt-sw))))

(ert-deftest keymap-popup-test-conditional-hidden-in-popup ()
  "Entry with :if nil hidden from rendered popup."
  (eval '(keymap-popup-define keymap-popup--test-cond
           "Test."
           "b" ("Browse" ignore :if (lambda () nil))
           "c" ("Comment" ignore))
        t)
  (let ((buf (keymap-popup--prepare-buffer 'keymap-popup--test-cond)))
    (unwind-protect
        (with-current-buffer buf
          (should-not (string-match-p "Browse" (buffer-string)))
          (should (string-match-p "Comment" (buffer-string))))
      (kill-buffer buf))))

;;; Add/remove entry tests

(ert-deftest keymap-popup-test-add-entry ()
  "Add an entry to an existing described keymap."
  (eval '(keymap-popup-define keymap-popup--test-add
           :group "Actions"
           "c" ("Comment" ignore))
        t)
  (keymap-popup-add-entry 'keymap-popup--test-add "z" "New" #'forward-char "Actions")
  (should (eq (keymap-lookup keymap-popup--test-add "z") #'forward-char))
  (let ((buf (keymap-popup--prepare-buffer 'keymap-popup--test-add)))
    (unwind-protect
        (with-current-buffer buf
          (should (string-match-p "New" (buffer-string))))
      (kill-buffer buf))))

(ert-deftest keymap-popup-test-remove-entry ()
  "Remove an entry from an existing described keymap."
  (eval '(keymap-popup-define keymap-popup--test-rm
           :group "Actions"
           "c" ("Comment" ignore)
           "r" ("Reply" ignore))
        t)
  (keymap-popup-remove-entry 'keymap-popup--test-rm "r")
  (should (null (keymap-lookup keymap-popup--test-rm "r")))
  (let ((buf (keymap-popup--prepare-buffer 'keymap-popup--test-rm)))
    (unwind-protect
        (with-current-buffer buf
          (should (string-match-p "Comment" (buffer-string)))
          (should-not (string-match-p "Reply" (buffer-string))))
      (kill-buffer buf))))

;;; Macro edge cases

(ert-deftest keymap-popup-test-if-on-switch ()
  "Switch with :if is hidden from popup when predicate returns nil."
  (eval '(keymap-popup-define keymap-popup--test-if-sw
           "v" ("Verbose" :switch keymap-popup--test-if-sw-var
                :if (lambda () nil)))
        t)
  ;; Keybinding still exists
  (should (keymap-lookup keymap-popup--test-if-sw "v"))
  ;; But hidden in popup
  (let ((buf (keymap-popup--prepare-buffer 'keymap-popup--test-if-sw)))
    (unwind-protect
        (with-current-buffer buf
          (should-not (string-match-p "Verbose" (buffer-string))))
      (kill-buffer buf))))

(ert-deftest keymap-popup-test-if-on-option ()
  "Option with :if is hidden from popup when predicate returns nil."
  (eval '(keymap-popup-define keymap-popup--test-if-opt
           "n" ("Count" :option keymap-popup--test-if-opt-var
                :reader read-number :prompt "N: "
                :if (lambda () nil)))
        t)
  (should (keymap-lookup keymap-popup--test-if-opt "n"))
  (let ((buf (keymap-popup--prepare-buffer 'keymap-popup--test-if-opt)))
    (unwind-protect
        (with-current-buffer buf
          (should-not (string-match-p "Count" (buffer-string))))
      (kill-buffer buf))))

(ert-deftest keymap-popup-test-stay-open-in-descriptions ()
  "Suffix with :stay-open stores the flag in descriptions."
  (eval '(keymap-popup-define keymap-popup--test-stay
           "g" ("Refresh" ignore :stay-open t))
        t)
  (let* ((descs (get 'keymap-popup--test-stay 'keymap-popup--descriptions))
         (entry (keymap-popup--find-entry-by-key descs "g")))
    (should (plist-get entry :stay-open))))

(ert-deftest keymap-popup-test-popup-key-with-docstring ()
  "Docstring and :popup-key work together."
  (eval '(keymap-popup-define keymap-popup--test-pkdoc
           "My commands."
           :popup-key "?"
           :group "Actions"
           "c" ("Comment" ignore))
        t)
  (should (functionp (keymap-lookup keymap-popup--test-pkdoc "?")))
  (should (null (keymap-lookup keymap-popup--test-pkdoc "h")))
  (should (string-match-p "My commands"
                          (documentation-property 'keymap-popup--test-pkdoc
                                                  'variable-documentation))))

(ert-deftest keymap-popup-test-dynamic-group-name ()
  "Group name can be a function called at render time."
  (eval '(keymap-popup-define keymap-popup--test-dyngrp
           :group (lambda () "Dynamic Group")
           "c" ("Comment" ignore))
        t)
  (let ((buf (keymap-popup--prepare-buffer 'keymap-popup--test-dyngrp)))
    (unwind-protect
        (with-current-buffer buf
          (should (string-match-p "Dynamic Group" (buffer-string))))
      (kill-buffer buf))))

;;; Keymap entry tests

(ert-deftest keymap-popup-test-parse-keymap-entry ()
  "A :keymap entry parses with target symbol."
  (let ((result (keymap-popup--parse-entry "a" '("Metadata" :keymap my-sub-map))))
    (should (equal (plist-get result :type) 'keymap))
    (should (equal (plist-get result :target) 'my-sub-map))))

(ert-deftest keymap-popup-test-keymap-target ()
  "Detect :keymap entries in descriptions."
  (let ((descs (list (list (list :name nil
                                 :entries (list (list :key "c" :type 'suffix :command 'ignore)
                                                (list :key "a" :type 'keymap
                                                      :target 'my-sub)))))))
    (should (eq (keymap-popup--keymap-target descs "a") 'my-sub))
    (should (null (keymap-popup--keymap-target descs "c")))))

(ert-deftest keymap-popup-test-keymap-entry-gets-submenu-face ()
  "Keymap entries render with the submenu face."
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "a" :description "Sub"
                                                      :type 'keymap :target 'x))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "Sub" output))
    (let ((pos (string-match "Sub" output)))
      (should (eq (get-text-property pos 'face output) 'keymap-popup-submenu)))))

;;; C-u rendering tests

(ert-deftest keymap-popup-test-c-u-desc-in-normal-mode ()
  "In normal mode, :c-u description appears in shadow face."
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "s" :description "Submit"
                                                      :type 'suffix :command 'ignore
                                                      :c-u "force push"))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "(force push)" output))
    (let ((pos (string-match "(force push)" output)))
      (should (eq (get-text-property pos 'face output) 'shadow)))))

(ert-deftest keymap-popup-test-c-u-desc-in-prefix-mode ()
  "In prefix mode, :c-u entries are normal and non-c-u entries are dimmed."
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "s" :description "Submit"
                                                      :type 'suffix :command 'ignore
                                                      :c-u "force")
                                                (list :key "g" :description "Refresh"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows t)))
    ;; "force" should NOT be in shadow (it's highlighted in prefix mode)
    (let ((pos (string-match "(force)" output)))
      (should pos)
      (should-not (eq (get-text-property pos 'face output) 'shadow)))
    ;; "Refresh" line should be dimmed
    (let ((pos (string-match "Refresh" output)))
      (should (eq (get-text-property pos 'face output) 'shadow)))))

;;; Stay-open detection tests

(ert-deftest keymap-popup-test-stay-open-p ()
  "Stay-open detects infixes and :stay-open suffixes."
  (let ((descs (list (list (list :name nil
                                 :entries (list (list :key "c" :type 'suffix :command 'ignore)
                                                (list :key "g" :type 'suffix :command 'ignore
                                                      :stay-open t)
                                                (list :key "v" :type 'switch :variable 'x)))))))
    (should-not (keymap-popup--stay-open-p descs "c"))
    (should (keymap-popup--stay-open-p descs "g"))
    (should (keymap-popup--stay-open-p descs "v"))))

;;; Parent inheritance tests

(ert-deftest keymap-popup-test-parent-keymap-bindings ()
  (eval '(keymap-popup-define keymap-popup--test-parent
           :group "Common"
           "g" ("Refresh" ignore)
           "q" ("Quit" quit-window))
        t)
  (eval '(keymap-popup-define keymap-popup--test-child
           :parent keymap-popup--test-parent
           :group "Child"
           "c" ("Comment" ignore))
        t)
  ;; Child has its own binding
  (should (eq (keymap-lookup keymap-popup--test-child "c") #'ignore))
  ;; Child inherits parent binding
  (should (eq (keymap-lookup keymap-popup--test-child "g") #'ignore)))

(ert-deftest keymap-popup-test-parent-descriptions-merged ()
  "Popup shows descriptions from both child and parent."
  (eval '(keymap-popup-define keymap-popup--test-parent2
           :group "Common"
           "g" ("Refresh" ignore))
        t)
  (eval '(keymap-popup-define keymap-popup--test-child2
           :parent keymap-popup--test-parent2
           :group "Child"
           "c" ("Comment" ignore))
        t)
  (let ((buf (keymap-popup--prepare-buffer 'keymap-popup--test-child2)))
    (unwind-protect
        (with-current-buffer buf
          (should (string-match-p "Comment" (buffer-string)))
          (should (string-match-p "Refresh" (buffer-string)))
          (should (string-match-p "Child" (buffer-string)))
          (should (string-match-p "Common" (buffer-string))))
      (kill-buffer buf))))

(ert-deftest keymap-popup-test-collect-descriptions-chain ()
  (eval '(keymap-popup-define keymap-popup--test-grandparent
           :group "GP"
           "g" ("Go" ignore))
        t)
  (eval '(keymap-popup-define keymap-popup--test-mid
           :parent keymap-popup--test-grandparent
           :group "Mid"
           "m" ("Mid cmd" ignore))
        t)
  (eval '(keymap-popup-define keymap-popup--test-leaf
           :parent keymap-popup--test-mid
           :group "Leaf"
           "l" ("Leaf cmd" ignore))
        t)
  (let ((all (keymap-popup--collect-descriptions 'keymap-popup--test-leaf)))
    ;; Should have rows from leaf + mid + grandparent
    (should (>= (length all) 3))))

;;; Inapt tests

(ert-deftest keymap-popup-test-inapt-rendered-with-face ()
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "m" :description "Merge"
                                                      :type 'suffix :command 'ignore
                                                      :inapt-if (lambda () t)))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "Merge" output))
    (let ((pos (string-match "Merge" output)))
      (should (eq (get-text-property pos 'face output) 'keymap-popup-inapt)))))

(ert-deftest keymap-popup-test-inapt-not-when-predicate-nil ()
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "m" :description "Merge"
                                                      :type 'suffix :command 'ignore
                                                      :inapt-if (lambda () nil)))))))
         (output (keymap-popup--render nil rows)))
    (let ((pos (string-match "Merge" output)))
      (should-not (eq (get-text-property pos 'face output) 'keymap-popup-inapt)))))

(ert-deftest keymap-popup-test-inapt-p ()
  (let ((descs (list (list (list :name nil
                                 :entries (list (list :key "m" :type 'suffix :command 'ignore
                                                      :inapt-if (lambda () t))
                                                (list :key "c" :type 'suffix
                                                      :command 'ignore)))))))
    (should (keymap-popup--inapt-p descs "m"))
    (should-not (keymap-popup--inapt-p descs "c"))))

(ert-deftest keymap-popup-test-inapt-via-macro ()
  "Inapt entries work through the macro."
  (eval '(keymap-popup-define keymap-popup--test-inapt-map
           "m" ("Merge" ignore :inapt-if (lambda () t))
           "c" ("Comment" ignore))
        t)
  (let ((buf (keymap-popup--prepare-buffer 'keymap-popup--test-inapt-map)))
    (unwind-protect
        (with-current-buffer buf
          (let* ((content (buffer-string))
                 (pos (string-match "Merge" content)))
            (should pos)
            (should (eq (get-text-property pos 'face content) 'keymap-popup-inapt))
            (should (string-match-p "Comment" content))))
      (kill-buffer buf))))

;;; Read-loop tests

(defvar keymap-popup-test--key-queue nil)
(defvar keymap-popup-test--exec-log nil)

(defun keymap-popup-test--cmd-a ()
  (interactive)
  (push (list 'cmd-a current-prefix-arg) keymap-popup-test--exec-log))

(defun keymap-popup-test--cmd-stay ()
  (interactive)
  (push (list 'cmd-stay current-prefix-arg) keymap-popup-test--exec-log))

(defun keymap-popup-test--cmd-sub ()
  (interactive)
  (push (list 'cmd-sub current-prefix-arg) keymap-popup-test--exec-log))

(eval '(keymap-popup-define keymap-popup--test-rl-basic
         "c" ("Comment" keymap-popup-test--cmd-a))
      t)

(eval '(keymap-popup-define keymap-popup--test-rl-stay
         "g" ("Refresh" keymap-popup-test--cmd-stay :stay-open t)
         "c" ("Comment" keymap-popup-test--cmd-a))
      t)

(eval '(keymap-popup-define keymap-popup--test-rl-inapt
         "m" ("Merge" keymap-popup-test--cmd-a :inapt-if (lambda () t))
         "c" ("Comment" keymap-popup-test--cmd-a))
      t)

(eval '(keymap-popup-define keymap-popup--test-rl-sub
         "s" ("Sub action" keymap-popup-test--cmd-sub))
      t)

(eval '(keymap-popup-define keymap-popup--test-rl-nested
         "a" ("Sub menu" :keymap keymap-popup--test-rl-sub)
         "c" ("Comment" keymap-popup-test--cmd-a))
      t)

(eval '(keymap-popup-define keymap-popup--test-rl-sw
         "v" ("Verbose" :switch keymap-popup--test-rl-sw-var)
         "c" ("Comment" keymap-popup-test--cmd-a))
      t)

(eval '(keymap-popup-define keymap-popup--test-rl-exit
         :exit-key ?x
         "c" ("Comment" keymap-popup-test--cmd-a))
      t)

(defun keymap-popup-test--run-read-loop (map-sym)
  "Run read-loop for MAP-SYM with read-key returning from key-queue."
  (cl-letf (((symbol-function 'read-key)
             (lambda (&rest _)
               (or (pop keymap-popup-test--key-queue)
                   (error "Key queue exhausted")))))
    (with-temp-buffer
      (let* ((buf (current-buffer))
             (km (symbol-value map-sym))
             (descs (keymap-popup--collect-descriptions map-sym))
             (doc (documentation-property map-sym 'variable-documentation))
             (exit-key (or (get map-sym 'keymap-popup--exit-key) ?q)))
        (keymap-popup--read-loop buf nil km descs doc exit-key)))))

(ert-deftest keymap-popup-test-rl-suffix-returns-command ()
  "Suffix key returns (command . nil)."
  (setq keymap-popup-test--key-queue (list ?c)
        keymap-popup-test--exec-log nil)
  (let ((result (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-basic)))
    (should (eq (car result) #'keymap-popup-test--cmd-a))
    (should (null (cdr result)))))

(ert-deftest keymap-popup-test-rl-exit-key-dismisses ()
  "Exit key returns nil."
  (setq keymap-popup-test--key-queue (list ?q))
  (should (null (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-basic))))

(ert-deftest keymap-popup-test-rl-c-g-dismisses ()
  "C-g returns nil when no prefix mode or stack."
  (setq keymap-popup-test--key-queue (list ?\C-g))
  (should (null (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-basic))))

(ert-deftest keymap-popup-test-rl-custom-exit-key ()
  "Custom :exit-key dismisses the popup."
  (setq keymap-popup-test--key-queue (list ?x))
  (should (null (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-exit))))

(ert-deftest keymap-popup-test-rl-prefix-on-suffix ()
  "C-u then suffix returns command with prefix arg (4)."
  (setq keymap-popup-test--key-queue (list ?\C-u ?c))
  (let ((result (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-basic)))
    (should (eq (car result) #'keymap-popup-test--cmd-a))
    (should (equal (cdr result) '(4)))))

(ert-deftest keymap-popup-test-rl-prefix-double-toggle ()
  "C-u twice reverts to no prefix."
  (setq keymap-popup-test--key-queue (list ?\C-u ?\C-u ?c))
  (let ((result (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-basic)))
    (should (eq (car result) #'keymap-popup-test--cmd-a))
    (should (null (cdr result)))))

(ert-deftest keymap-popup-test-rl-prefix-cancelled-by-c-g ()
  "C-g cancels prefix mode without dismissing."
  (setq keymap-popup-test--key-queue (list ?\C-u ?\C-g ?c))
  (let ((result (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-basic)))
    (should (eq (car result) #'keymap-popup-test--cmd-a))
    (should (null (cdr result)))))

(ert-deftest keymap-popup-test-rl-unbound-key-ignored ()
  "Unbound key does nothing, loop continues."
  (setq keymap-popup-test--key-queue (list ?z ?c))
  (let ((result (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-basic)))
    (should (eq (car result) #'keymap-popup-test--cmd-a))
    (should (null keymap-popup-test--key-queue))))

(ert-deftest keymap-popup-test-rl-stay-open-executes ()
  "Stay-open entry runs command and continues the loop."
  (setq keymap-popup-test--key-queue (list ?g ?q)
        keymap-popup-test--exec-log nil)
  (let ((result (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-stay)))
    (should (null result))
    (should (equal keymap-popup-test--exec-log '((cmd-stay nil))))))

(ert-deftest keymap-popup-test-rl-stay-open-with-prefix ()
  "C-u + stay-open passes prefix and clears it for next key."
  (setq keymap-popup-test--key-queue (list ?\C-u ?g ?c)
        keymap-popup-test--exec-log nil)
  (let ((result (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-stay)))
    (should (equal (car keymap-popup-test--exec-log) '(cmd-stay (4))))
    (should (eq (car result) #'keymap-popup-test--cmd-a))
    (should (null (cdr result)))))

(ert-deftest keymap-popup-test-rl-multiple-stay-open ()
  "Multiple stay-open presses all execute."
  (setq keymap-popup-test--key-queue (list ?g ?g ?q)
        keymap-popup-test--exec-log nil)
  (let ((result (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-stay)))
    (should (null result))
    (should (= (length keymap-popup-test--exec-log) 2))))

(ert-deftest keymap-popup-test-rl-inapt-blocked ()
  "Inapt entry is blocked, loop continues."
  (setq keymap-popup-test--key-queue (list ?m ?c)
        keymap-popup-test--exec-log nil)
  (let ((result (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-inapt)))
    (should (eq (car result) #'keymap-popup-test--cmd-a))
    (should (null keymap-popup-test--exec-log))))

(ert-deftest keymap-popup-test-rl-keymap-enters-sub ()
  "Keymap entry enters sub-map where its bindings are active."
  (setq keymap-popup-test--key-queue (list ?a ?s))
  (let ((result (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-nested)))
    (should (eq (car result) #'keymap-popup-test--cmd-sub))
    (should (null (cdr result)))))

(ert-deftest keymap-popup-test-rl-keymap-c-g-pops ()
  "C-g in sub-map returns to parent."
  (setq keymap-popup-test--key-queue (list ?a ?\C-g ?c))
  (let ((result (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-nested)))
    (should (eq (car result) #'keymap-popup-test--cmd-a))))

(ert-deftest keymap-popup-test-rl-keymap-exit-pops ()
  "Exit key in sub-map returns to parent."
  (setq keymap-popup-test--key-queue (list ?a ?q ?c))
  (let ((result (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-nested)))
    (should (eq (car result) #'keymap-popup-test--cmd-a))))

(ert-deftest keymap-popup-test-rl-keymap-resets-prefix ()
  "Entering sub-map clears prefix mode."
  (setq keymap-popup-test--key-queue (list ?\C-u ?a ?s))
  (let ((result (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-nested)))
    (should (eq (car result) #'keymap-popup-test--cmd-sub))
    (should (null (cdr result)))))

(ert-deftest keymap-popup-test-rl-switch-stays-open ()
  "Switch entry stays open after toggling."
  (setq keymap-popup-test--key-queue (list ?v ?q))
  (should (null (keymap-popup-test--run-read-loop 'keymap-popup--test-rl-sw))))

;;; Popup dispatch tests

(defun keymap-popup-test--run-popup (map-sym)
  "Run `keymap-popup' for MAP-SYM with UI operations stubbed."
  (cl-letf (((symbol-function 'read-key)
             (lambda (&rest _)
               (or (pop keymap-popup-test--key-queue)
                   (error "Key queue exhausted"))))
            ((symbol-function 'display-buffer)
             (lambda (&rest _) nil))
            ((symbol-function 'fit-window-to-buffer)
             (lambda (&rest _) nil)))
    (keymap-popup map-sym)))

(ert-deftest keymap-popup-test-popup-dispatches-suffix ()
  "Full popup dispatches suffix command."
  (setq keymap-popup-test--key-queue (list ?c)
        keymap-popup-test--exec-log nil)
  (keymap-popup-test--run-popup 'keymap-popup--test-rl-basic)
  (should (equal keymap-popup-test--exec-log '((cmd-a nil)))))

(ert-deftest keymap-popup-test-popup-dispatches-with-prefix ()
  "Full popup passes C-u prefix to dispatched command."
  (setq keymap-popup-test--key-queue (list ?\C-u ?c)
        keymap-popup-test--exec-log nil)
  (keymap-popup-test--run-popup 'keymap-popup--test-rl-basic)
  (should (equal keymap-popup-test--exec-log '((cmd-a (4))))))

(ert-deftest keymap-popup-test-popup-dismiss-kills-buffer ()
  "Popup buffer is killed after dismissal."
  (setq keymap-popup-test--key-queue (list ?q))
  (keymap-popup-test--run-popup 'keymap-popup--test-rl-basic)
  (should-not (get-buffer "*keymap-popup*")))

(ert-deftest keymap-popup-test-popup-suffix-kills-buffer ()
  "Popup buffer is killed after suffix dispatch."
  (setq keymap-popup-test--key-queue (list ?c)
        keymap-popup-test--exec-log nil)
  (keymap-popup-test--run-popup 'keymap-popup--test-rl-basic)
  (should-not (get-buffer "*keymap-popup*")))

(ert-deftest keymap-popup-test-popup-cleanup-on-error ()
  "Popup buffer is killed even when an error occurs."
  (cl-letf (((symbol-function 'read-key)
             (lambda (&rest _) (error "Unexpected")))
            ((symbol-function 'display-buffer)
             (lambda (&rest _) nil))
            ((symbol-function 'fit-window-to-buffer)
             (lambda (&rest _) nil)))
    (ignore-errors (keymap-popup 'keymap-popup--test-rl-basic))
    (should-not (get-buffer "*keymap-popup*"))))

(ert-deftest keymap-popup-test-popup-window-deleted ()
  "Popup window is deleted after close."
  (setq keymap-popup-test--key-queue (list ?q))
  (let ((deleted nil))
    (cl-letf (((symbol-function 'read-key)
               (lambda (&rest _)
                 (or (pop keymap-popup-test--key-queue)
                     (error "Key queue exhausted"))))
              ((symbol-function 'display-buffer)
               (lambda (&rest _) 'fake-window))
              ((symbol-function 'fit-window-to-buffer)
               (lambda (&rest _) nil))
              ((symbol-function 'window-live-p)
               (lambda (w) (eq w 'fake-window)))
              ((symbol-function 'delete-window)
               (lambda (w) (when (eq w 'fake-window) (setq deleted t)))))
      (keymap-popup 'keymap-popup--test-rl-basic)
      (should deleted))))

;;; Group-level predicate tests

(ert-deftest keymap-popup-test-group-if-hidden ()
  "A group with :if returning nil is hidden from the popup."
  (let* ((rows (list (list (list :name "Visible"
                                 :entries (list (list :key "a" :description "Alpha"
                                                      :type 'suffix :command 'ignore)))
                           (list :name "Hidden" :if (lambda () nil)
                                 :entries (list (list :key "b" :description "Beta"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "Alpha" output))
    (should-not (string-match-p "Beta" output))
    (should-not (string-match-p "Hidden" output))))

(ert-deftest keymap-popup-test-group-if-shown ()
  "A group with :if returning non-nil is shown normally."
  (let* ((rows (list (list (list :name "Shown" :if (lambda () t)
                                 :entries (list (list :key "a" :description "Alpha"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "Alpha" output))
    (should (string-match-p "Shown" output))))

(ert-deftest keymap-popup-test-group-inapt-grays-all ()
  "A group with :inapt-if returning non-nil grays out all entries."
  (let* ((rows (list (list (list :name "Disabled"
                                 :inapt-if (lambda () t)
                                 :entries (list (list :key "a" :description "Alpha"
                                                      :type 'suffix :command 'ignore)
                                                (list :key "b" :description "Beta"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "Alpha" output))
    (let ((pos-a (string-match "Alpha" output))
          (pos-b (string-match "Beta" output)))
      (should (eq (get-text-property pos-a 'face output) 'keymap-popup-inapt))
      (should (eq (get-text-property pos-b 'face output) 'keymap-popup-inapt)))))

(ert-deftest keymap-popup-test-group-inapt-blocks-dispatch ()
  "A group-level :inapt-if blocks dispatch for entries in that group."
  (let ((descs (list (list (list :name "OK"
                                 :entries (list (list :key "a" :type 'suffix :command 'ignore)))
                           (list :name "Nope" :inapt-if (lambda () t)
                                 :entries (list (list :key "b" :type 'suffix :command 'ignore)))))))
    (should-not (keymap-popup--inapt-p descs "a"))
    (should (keymap-popup--inapt-p descs "b"))))

(ert-deftest keymap-popup-test-group-inapt-via-macro ()
  "Group-level :inapt-if works through the macro."
  (eval '(keymap-popup-define keymap-popup--test-group-inapt-map
           :group ("Disabled" :inapt-if (lambda () t))
           "a" ("Alpha" ignore)
           "b" ("Beta" ignore))
        t)
  (let ((buf (keymap-popup--prepare-buffer 'keymap-popup--test-group-inapt-map)))
    (unwind-protect
        (with-current-buffer buf
          (let* ((content (buffer-string))
                 (pos (string-match "Alpha" content)))
            (should pos)
            (should (eq (get-text-property pos 'face content) 'keymap-popup-inapt))))
      (kill-buffer buf))))

(ert-deftest keymap-popup-test-group-if-via-macro ()
  "Group-level :if works through the macro."
  (eval '(keymap-popup-define keymap-popup--test-group-if-map
           :group ("Hidden" :if (lambda () nil))
           "a" ("Alpha" ignore)
           :group "Shown"
           "b" ("Beta" ignore))
        t)
  (let ((buf (keymap-popup--prepare-buffer 'keymap-popup--test-group-if-map)))
    (unwind-protect
        (with-current-buffer buf
          (let ((content (buffer-string)))
            (should-not (string-match-p "Alpha" content))
            (should (string-match-p "Beta" content))))
      (kill-buffer buf))))

(provide 'keymap-popup-tests)
;;; keymap-popup-tests.el ends here
