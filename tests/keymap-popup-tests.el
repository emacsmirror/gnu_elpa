;;; keymap-popup-tests.el --- Tests -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "../keymap-popup.el"
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
  (eval '(define-described-keymap keymap-popup--test-map-1
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
  (eval '(define-described-keymap keymap-popup--test-map-2
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
  (eval '(define-described-keymap keymap-popup--test-map-3
           "Test."
           "v" ("Verbose" :switch keymap-popup--test-sw))
        t)
  (should (boundp 'keymap-popup--test-sw))
  (should (fboundp 'keymap-popup--test-map-3--toggle-keymap-popup--test-sw)))

(ert-deftest keymap-popup-test-macro-option-infix ()
  "Macro generates setter command and binds it for options."
  (eval '(define-described-keymap keymap-popup--test-map-4
           "Test."
           "n" ("Count" :option keymap-popup--test-opt
                :reader read-number :prompt "N: "))
        t)
  (should (boundp 'keymap-popup--test-opt))
  (should (fboundp 'keymap-popup--test-map-4--set-keymap-popup--test-opt)))

(ert-deftest keymap-popup-test-macro-lambda-command ()
  "Lambda commands bind directly in the keymap."
  (eval '(define-described-keymap keymap-popup--test-map-5
           "Test."
           "x" ("Run" (lambda () (interactive) (message "running"))))
        t)
  (should (functionp (keymap-lookup keymap-popup--test-map-5 "x"))))

(ert-deftest keymap-popup-test-macro-no-docstring ()
  "Macro works without a docstring."
  (eval '(define-described-keymap keymap-popup--test-map-nodoc
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
  (eval '(define-described-keymap keymap-popup--test-map-defkey
           "c" ("Comment" ignore))
        t)
  (should (functionp (keymap-lookup keymap-popup--test-map-defkey "h"))))

(ert-deftest keymap-popup-test-macro-custom-popup-key ()
  "Popup key can be customized with :popup-key."
  (eval '(define-described-keymap keymap-popup--test-map-custkey
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
  (eval '(define-described-keymap keymap-popup--test-popup-map
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
  "Full define-described-keymap with all entry types works end-to-end."
  (eval '(define-described-keymap keymap-popup--test-full
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
  (eval '(define-described-keymap keymap-popup--test-rt
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
  (eval '(define-described-keymap keymap-popup--test-cond
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
  (eval '(define-described-keymap keymap-popup--test-add
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
  (eval '(define-described-keymap keymap-popup--test-rm
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
  (eval '(define-described-keymap keymap-popup--test-if-sw
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
  (eval '(define-described-keymap keymap-popup--test-if-opt
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
  (eval '(define-described-keymap keymap-popup--test-stay
           "g" ("Refresh" ignore :stay-open t))
        t)
  (let* ((descs (get 'keymap-popup--test-stay 'keymap-popup--descriptions))
         (entry (keymap-popup--find-entry-by-key descs "g")))
    (should (plist-get entry :stay-open))))

(ert-deftest keymap-popup-test-popup-key-with-docstring ()
  "Docstring and :popup-key work together."
  (eval '(define-described-keymap keymap-popup--test-pkdoc
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
  (eval '(define-described-keymap keymap-popup--test-dyngrp
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

(provide 'keymap-popup-tests)
;;; keymap-popup-tests.el ends here
