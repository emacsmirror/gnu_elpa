;;; keymap-popup-tests.el --- Tests -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name
       "../keymap-popup.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; Parser tests

(ert-deftest keymap-popup-test-parse-suffix-entry ()
  (let ((result (keymap-popup--parse-entry "c" '("Comment" forgejo-view-comment))))
    (should (equal (plist-get result :key) "c"))
    (should (equal (plist-get result :description) "Comment"))
    (should (equal (plist-get result :command) 'forgejo-view-comment))
    (should (equal (plist-get result :type) 'suffix))))

(ert-deftest keymap-popup-test-parse-suffix-with-if ()
  (let* ((pred (lambda () t))
         (result (keymap-popup--parse-entry "b" `("Browse" forgejo-browse :if ,pred))))
    (should (equal (plist-get result :type) 'suffix))
    (should (eq (plist-get result :if) pred))))

(ert-deftest keymap-popup-test-parse-switch-entry ()
  (let ((result (keymap-popup--parse-entry "v" '("Verbose" :switch my-verbose-var))))
    (should (equal (plist-get result :type) 'switch))
    (should (equal (plist-get result :variable) 'my-verbose-var))))

(ert-deftest keymap-popup-test-parse-dynamic-description ()
  (let* ((desc-fn (lambda () "dynamic"))
         (result (keymap-popup--parse-entry "d" `(,desc-fn some-command))))
    (should (functionp (plist-get result :description)))))

(ert-deftest keymap-popup-test-parse-bindings-groups ()
  (let* ((rows (keymap-popup--parse-bindings
                '(:group "Actions"
			 "c" ("Comment" forgejo-view-comment)
			 "r" ("Reply" forgejo-issue-reply)
			 :group "Navigate"
			 "g" ("Refresh" forgejo-view-refresh)
			 "q" ("Quit" quit-window))))
         (row (car rows)))
    (should (= (length rows) 1))
    (should (= (length row) 2))
    (should (equal (plist-get (car row) :name) "Actions"))
    (should (= (length (plist-get (car row) :entries)) 2))
    (should (equal (plist-get (cadr row) :name) "Navigate"))
    (should (= (length (plist-get (cadr row) :entries)) 2))))

(ert-deftest keymap-popup-test-parse-bindings-no-group ()
  (let* ((rows (keymap-popup--parse-bindings
                '("c" ("Comment" forgejo-view-comment)
                  "r" ("Reply" forgejo-issue-reply))))
         (row (car rows)))
    (should (= (length rows) 1))
    (should (= (length row) 1))
    (should (null (plist-get (car row) :name)))
    (should (= (length (plist-get (car row) :entries)) 2))))

(ert-deftest keymap-popup-test-parse-bindings-rows ()
  (let ((rows (keymap-popup--parse-bindings
               '(:group "A"
			"a" ("Aaa" ignore)
			:group "B"
			"b" ("Bbb" ignore)
			:row
			:group "C"
			"c" ("Ccc" ignore)))))
    (should (= (length rows) 2))
    (should (= (length (car rows)) 2))
    (should (= (length (cadr rows)) 1))
    (should (equal (plist-get (caar (cdr rows)) :name) "C"))))

(ert-deftest keymap-popup-test-parse-keymap-entry ()
  (let ((result (keymap-popup--parse-entry "a" '("Metadata" :keymap my-sub-map))))
    (should (equal (plist-get result :type) 'keymap))
    (should (equal (plist-get result :target) 'my-sub-map))))

;;; Infix generator tests

(ert-deftest keymap-popup-test-switch-forms ()
  (let* ((entry '(:key "v" :description "Verbose" :type switch
                       :variable my-verbose-var))
         (forms (keymap-popup--switch-forms 'test-map entry)))
    (should (= (length forms) 2))
    (should (eq (car (nth 0 forms)) 'defvar-local))
    (should (eq (cadr (nth 0 forms)) 'my-verbose-var))
    (should (eq (car (nth 1 forms)) 'defun))
    (should (eq (cadr (nth 1 forms)) 'test-map--toggle-my-verbose-var))))

(ert-deftest keymap-popup-test-entry-command ()
  (should (eq (keymap-popup--entry-command 'map '(:type suffix :command my-cmd))
              'my-cmd))
  (should (eq (keymap-popup--entry-command 'map '(:type switch :variable my-var))
              'map--toggle-my-var)))

;;; Macro tests

(ert-deftest keymap-popup-test-macro-creates-keymap ()
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
  (eval '(keymap-popup-define keymap-popup--test-map-2
           "Test."
           :group "A"
           "c" ("Comment" ignore)
           :group "B"
           "g" ("Go" ignore))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-map-2
                                    'keymap-popup--descriptions))
         (row (car descs)))
    (should (= (length descs) 1))
    (should (= (length row) 2))
    (should (equal (plist-get (car row) :name) "A"))
    (should (equal (plist-get (cadr row) :name) "B"))))

(ert-deftest keymap-popup-test-macro-switch-infix ()
  (eval '(keymap-popup-define keymap-popup--test-map-3
           "Test."
           "v" ("Verbose" :switch keymap-popup--test-sw))
        t)
  (should (boundp 'keymap-popup--test-sw))
  (should (fboundp 'keymap-popup--test-map-3--toggle-keymap-popup--test-sw)))

(ert-deftest keymap-popup-test-macro-lambda-command ()
  (eval '(keymap-popup-define keymap-popup--test-map-5
           "Test."
           "x" ("Run" (lambda () (interactive) (message "running"))))
        t)
  (should (functionp (keymap-lookup keymap-popup--test-map-5 "x"))))

(ert-deftest keymap-popup-test-macro-no-docstring ()
  (eval '(keymap-popup-define keymap-popup--test-map-nodoc
           :group "Actions"
           "c" ("Comment" ignore))
        t)
  (should (keymapp keymap-popup--test-map-nodoc))
  (should (eq (keymap-lookup keymap-popup--test-map-nodoc "c") #'ignore))
  (let* ((descs (keymap-popup--meta keymap-popup--test-map-nodoc
                                    'keymap-popup--descriptions))
         (row (car descs)))
    (should (= (length descs) 1))
    (should (= (length row) 1))
    (should (equal (plist-get (car row) :name) "Actions"))))

(ert-deftest keymap-popup-test-macro-default-popup-key ()
  (eval '(keymap-popup-define keymap-popup--test-map-defkey
           "c" ("Comment" ignore))
        t)
  (should (functionp (keymap-lookup keymap-popup--test-map-defkey "h"))))

(ert-deftest keymap-popup-test-macro-custom-popup-key ()
  (eval '(keymap-popup-define keymap-popup--test-map-custkey
           :popup-key "?"
           "c" ("Comment" ignore))
        t)
  (should (functionp (keymap-lookup keymap-popup--test-map-custkey "?")))
  (should (null (keymap-lookup keymap-popup--test-map-custkey "h"))))

(ert-deftest keymap-popup-test-macro-exit-key ()
  (eval '(keymap-popup-define keymap-popup--test-exit-key
           :exit-key ?x
           "c" ("Comment" ignore))
        t)
  (should (eq (keymap-popup--meta keymap-popup--test-exit-key
                                  'keymap-popup--exit-key)
              ?x)))

(ert-deftest keymap-popup-test-popup-key-with-docstring ()
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

;;; Renderer tests

(ert-deftest keymap-popup-test-render-suffix ()
  (let* ((rows (list (list (list :name "Actions"
                                 :entries (list (list :key "c" :description "Comment"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render "Test." rows)))
    (should (string-match-p "Test\\." output))
    (should (string-match-p "Actions" output))
    (should (string-match-p "Comment" output))))

(ert-deftest keymap-popup-test-render-switch-value ()
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

(ert-deftest keymap-popup-test-render-if-hidden ()
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "b" :description "Browse"
                                                      :type 'suffix :command 'ignore
                                                      :if (lambda () nil)))))))
         (output (keymap-popup--render nil rows)))
    (should-not (string-match-p "Browse" output))))

(ert-deftest keymap-popup-test-render-if-shown ()
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "b" :description "Browse"
                                                      :type 'suffix :command 'ignore
                                                      :if (lambda () t)))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "Browse" output))))

(ert-deftest keymap-popup-test-render-dynamic-description ()
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "d"
                                                      :description (lambda () "Dynamic!")
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "Dynamic!" output))))

;;; Column layout tests

(ert-deftest keymap-popup-test-render-columns-side-by-side ()
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
  (let* ((rows (list (list (list :name "Row1"
                                 :entries (list (list :key "a" :description "Aaa"
                                                      :type 'suffix :command 'ignore))))
                     (list (list :name "Row2"
                                 :entries (list (list :key "b" :description "Bbb"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "Row1" output))
    (should (string-match-p "Row2" output))
    (let ((lines (split-string output "\n" t)))
      (should-not (and (string-match-p "Row1" (car lines))
                       (string-match-p "Row2" (car lines)))))))

(ert-deftest keymap-popup-test-columns-aligned-across-rows ()
  (let* ((rows (list
                (list (list :name "A"
                            :entries (list (list :key "a" :description "X"
                                                 :type 'suffix :command 'ignore)))
                      (list :name "B"
                            :entries (list (list :key "b" :description "Y"
                                                 :type 'suffix :command 'ignore))))
                (list (list :name "Longer Name"
                            :entries (list (list :key "c" :description "Something longer"
                                                 :type 'suffix :command 'ignore)))
                      (list :name "D"
                            :entries (list (list :key "d" :description "Z"
                                                 :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows))
         (plain (substring-no-properties output))
         (lines (split-string plain "\n" t)))
    (let ((b-pos (string-match "B" (cl-find-if (lambda (l) (string-match-p "\\bB\\b" l)) lines)))
          (d-pos (string-match "D" (cl-find-if (lambda (l) (string-match-p "\\bD\\b" l)) lines))))
      (should b-pos)
      (should d-pos)
      (should (= b-pos d-pos)))))

(ert-deftest keymap-popup-test-join-columns ()
  (let* ((col-a '("Header A" "  a  Aaa" "  b  Bbb"))
         (col-b '("Header B" "  c  Ccc"))
         (widths (list (keymap-popup--column-width col-a)
                       (keymap-popup--column-width col-b)))
         (result (keymap-popup--join-columns (list col-a col-b) "  " widths)))
    (should (= (length result) 3))
    (should (string-match-p "Header A" (nth 0 result)))
    (should (string-match-p "Header B" (nth 0 result)))
    (should (string-match-p "Bbb" (nth 2 result)))))

;;; Lookup helper tests

(ert-deftest keymap-popup-test-find-entry-by-key ()
  (let ((descs (list (list (list :name "G"
                                 :entries (list (list :key "c" :description "Comment"
                                                      :type 'suffix :command 'ignore)
                                                (list :key "v" :description "Verbose"
                                                      :type 'switch :variable 'some-var)))))))
    (should (equal (plist-get (keymap-popup--find-entry-by-key descs "c") :type) 'suffix))
    (should (equal (plist-get (keymap-popup--find-entry-by-key descs "v") :type) 'switch))
    (should (null (keymap-popup--find-entry-by-key descs "z")))))

(ert-deftest keymap-popup-test-infix-p ()
  (let ((descs (list (list (list :name nil
                                 :entries (list (list :key "c" :type 'suffix :command 'ignore)
                                                (list :key "v" :type 'switch :variable 'x)))))))
    (should-not (keymap-popup--infix-p descs "c"))
    (should (keymap-popup--infix-p descs "v"))
    (should-not (keymap-popup--infix-p descs "z"))))

(ert-deftest keymap-popup-test-keymap-target ()
  (let ((descs (list (list (list :name nil
                                 :entries (list (list :key "c" :type 'suffix :command 'ignore)
                                                (list :key "a" :type 'keymap
                                                      :target 'my-sub)))))))
    (should (eq (keymap-popup--keymap-target descs "a") 'my-sub))
    (should (null (keymap-popup--keymap-target descs "c")))))

(ert-deftest keymap-popup-test-inapt-p ()
  (let ((descs (list (list (list :name nil
                                 :entries (list (list :key "m" :type 'suffix :command 'ignore
                                                      :inapt-if (lambda () t))
                                                (list :key "c" :type 'suffix
                                                      :command 'ignore)))))))
    (should (keymap-popup--inapt-p descs "m"))
    (should-not (keymap-popup--inapt-p descs "c"))))

(ert-deftest keymap-popup-test-stay-open-p ()
  (let ((descs (list (list (list :name nil
                                 :entries (list (list :key "c" :type 'suffix :command 'ignore)
                                                (list :key "g" :type 'suffix :command 'ignore
                                                      :stay-open t)
                                                (list :key "v" :type 'switch :variable 'x)))))))
    (should-not (keymap-popup--stay-open-p descs "c"))
    (should (keymap-popup--stay-open-p descs "g"))
    (should (keymap-popup--stay-open-p descs "v"))))

(ert-deftest keymap-popup-test-keep-popup-p ()
  (let ((descs (list (list (list :name nil
                                 :entries (list (list :key "c" :type 'suffix :command 'ignore)
                                                (list :key "v" :type 'switch :variable 'x)
                                                (list :key "m" :type 'suffix :command 'ignore
                                                      :inapt-if (lambda () t))
                                                (list :key "a" :type 'keymap :target 'sub)
                                                (list :key "g" :type 'suffix :command 'ignore
                                                      :stay-open t)))))))
    (should-not (keymap-popup--keep-popup-p descs "c"))
    (should (keymap-popup--keep-popup-p descs "v"))
    (should (keymap-popup--keep-popup-p descs "m"))
    (should (keymap-popup--keep-popup-p descs "a"))
    ;; stay-open suffixes dismiss and reopen, not kept
    (should-not (keymap-popup--keep-popup-p descs "g"))
    (should (keymap-popup--keep-popup-p descs "C-u"))))

;;; C-u rendering tests

(ert-deftest keymap-popup-test-c-u-desc-in-normal-mode ()
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "s" :description "Submit"
                                                      :type 'suffix :command 'ignore
                                                      :c-u "force push"))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "(force push)" output))
    (let ((pos (string-match "(force push)" output)))
      (should (eq (get-text-property pos 'face output) 'shadow)))))

(ert-deftest keymap-popup-test-c-u-desc-in-prefix-mode ()
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "s" :description "Submit"
                                                      :type 'suffix :command 'ignore
                                                      :c-u "force")
                                                (list :key "g" :description "Refresh"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows t)))
    (let ((pos (string-match "(force)" output)))
      (should pos)
      (should-not (eq (get-text-property pos 'face output) 'shadow)))
    (let ((pos (string-match "Refresh" output)))
      (should (eq (get-text-property pos 'face output) 'shadow)))))

;;; Inapt rendering tests

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

(ert-deftest keymap-popup-test-keymap-entry-gets-submenu-face ()
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "a" :description "Sub"
                                                      :type 'keymap :target 'x))))))
         (output (keymap-popup--render nil rows)))
    (let ((pos (string-match "Sub" output)))
      (should (eq (get-text-property pos 'face output) 'keymap-popup-submenu)))))

;;; Group-level predicate tests

(ert-deftest keymap-popup-test-group-if-hidden ()
  (let* ((rows (list (list (list :name "Visible"
                                 :entries (list (list :key "a" :description "Alpha"
                                                      :type 'suffix :command 'ignore)))
                           (list :name "Hidden" :if (lambda () nil)
                                 :entries (list (list :key "b" :description "Beta"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "Alpha" output))
    (should-not (string-match-p "Beta" output))))

(ert-deftest keymap-popup-test-group-if-shown ()
  (let* ((rows (list (list (list :name "Shown" :if (lambda () t)
                                 :entries (list (list :key "a" :description "Alpha"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows)))
    (should (string-match-p "Alpha" output))
    (should (string-match-p "Shown" output))))

(ert-deftest keymap-popup-test-group-inapt-grays-all ()
  (let* ((rows (list (list (list :name "Disabled"
                                 :inapt-if (lambda () t)
                                 :entries (list (list :key "a" :description "Alpha"
                                                      :type 'suffix :command 'ignore)
                                                (list :key "b" :description "Beta"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render nil rows)))
    (let ((pos-a (string-match "Alpha" output))
          (pos-b (string-match "Beta" output)))
      (should (eq (get-text-property pos-a 'face output) 'keymap-popup-inapt))
      (should (eq (get-text-property pos-b 'face output) 'keymap-popup-inapt)))))

(ert-deftest keymap-popup-test-group-inapt-blocks-dispatch ()
  (let ((descs (list (list (list :name "OK"
                                 :entries (list (list :key "a" :type 'suffix :command 'ignore)))
                           (list :name "Nope" :inapt-if (lambda () t)
                                 :entries (list (list :key "b" :type 'suffix :command 'ignore)))))))
    (should-not (keymap-popup--inapt-p descs "a"))
    (should (keymap-popup--inapt-p descs "b"))))

;;; Integration tests

(ert-deftest keymap-popup-test-full-definition ()
  (eval '(keymap-popup-define keymap-popup--test-full
           "Full test."
           :group "Actions"
           "c" ("Comment" ignore)
           :group "Switches"
           "v" ("Verbose" :switch keymap-popup--test-full-verbose)
           :row
           :group "Navigate"
           "b" ("Browse" ignore :if (lambda () t))
           "q" ("Quit" quit-window))
        t)
  (should (keymapp keymap-popup--test-full))
  (should (eq (keymap-lookup keymap-popup--test-full "c") #'ignore))
  (let ((descs (keymap-popup--meta keymap-popup--test-full
                                   'keymap-popup--descriptions)))
    (should (= (length descs) 2))))

(ert-deftest keymap-popup-test-switch-toggle-roundtrip ()
  (eval '(keymap-popup-define keymap-popup--test-rt
           "Test." "v" ("Verbose" :switch keymap-popup--test-rt-sw))
        t)
  (with-temp-buffer
    (should (null keymap-popup--test-rt-sw))
    (funcall-interactively #'keymap-popup--test-rt--toggle-keymap-popup--test-rt-sw)
    (should (eq keymap-popup--test-rt-sw t))
    (funcall-interactively #'keymap-popup--test-rt--toggle-keymap-popup--test-rt-sw)
    (should (null keymap-popup--test-rt-sw))))

(ert-deftest keymap-popup-test-stay-open-in-descriptions ()
  (eval '(keymap-popup-define keymap-popup--test-stay
           "g" ("Refresh" ignore :stay-open t))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-stay
                                    'keymap-popup--descriptions))
         (entry (keymap-popup--find-entry-by-key descs "g")))
    (should (plist-get entry :stay-open))))

(ert-deftest keymap-popup-test-dynamic-group-name ()
  (eval '(keymap-popup-define keymap-popup--test-dyngrp
           :group (lambda () "Dynamic Group")
           "c" ("Comment" ignore))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-dyngrp
                                    'keymap-popup--descriptions))
         (output (keymap-popup--render nil descs)))
    (should (string-match-p "Dynamic Group" output))))

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
  (should (eq (keymap-lookup keymap-popup--test-child "c") #'ignore))
  (should (eq (keymap-lookup keymap-popup--test-child "g") #'ignore)))

(ert-deftest keymap-popup-test-parent-descriptions-merged ()
  (eval '(keymap-popup-define keymap-popup--test-parent2
           :group "Common"
           "g" ("Refresh" ignore))
        t)
  (eval '(keymap-popup-define keymap-popup--test-child2
           :parent keymap-popup--test-parent2
           :group "Child"
           "c" ("Comment" ignore))
        t)
  (let ((all (keymap-popup--collect-descriptions keymap-popup--test-child2)))
    (should (>= (length all) 2))))

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
  (let ((all (keymap-popup--collect-descriptions keymap-popup--test-leaf)))
    (should (>= (length all) 3))))

;;; Macro via-macro integration tests

(ert-deftest keymap-popup-test-inapt-via-macro ()
  (eval '(keymap-popup-define keymap-popup--test-inapt-map
           "m" ("Merge" ignore :inapt-if (lambda () t))
           "c" ("Comment" ignore))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-inapt-map
                                    'keymap-popup--descriptions))
         (output (keymap-popup--render nil descs)))
    (let ((pos (string-match "Merge" output)))
      (should pos)
      (should (eq (get-text-property pos 'face output) 'keymap-popup-inapt)))
    (should (string-match-p "Comment" output))))

(ert-deftest keymap-popup-test-group-inapt-via-macro ()
  (eval '(keymap-popup-define keymap-popup--test-group-inapt-map
           :group ("Disabled" :inapt-if (lambda () t))
           "a" ("Alpha" ignore)
           "b" ("Beta" ignore))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-group-inapt-map
                                    'keymap-popup--descriptions))
         (output (keymap-popup--render nil descs))
         (pos (string-match "Alpha" output)))
    (should pos)
    (should (eq (get-text-property pos 'face output) 'keymap-popup-inapt))))

(ert-deftest keymap-popup-test-group-if-via-macro ()
  (eval '(keymap-popup-define keymap-popup--test-group-if-map
           :group ("Hidden" :if (lambda () nil))
           "a" ("Alpha" ignore)
           :group "Shown"
           "b" ("Beta" ignore))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-group-if-map
                                    'keymap-popup--descriptions))
         (output (keymap-popup--render nil descs)))
    (should-not (string-match-p "Alpha" output))
    (should (string-match-p "Beta" output))))

(ert-deftest keymap-popup-test-if-on-switch ()
  (eval '(keymap-popup-define keymap-popup--test-if-sw
           "v" ("Verbose" :switch keymap-popup--test-if-sw-var
                :if (lambda () nil)))
        t)
  (should (keymap-lookup keymap-popup--test-if-sw "v"))
  (let* ((descs (keymap-popup--meta keymap-popup--test-if-sw
                                    'keymap-popup--descriptions))
         (output (keymap-popup--render nil descs)))
    (should-not (string-match-p "Verbose" output))))

;;; Wrapper map tests

(ert-deftest keymap-popup-test-wrapper-map-has-exit-key ()
  (eval '(keymap-popup-define keymap-popup--test-wrap
           "c" ("Comment" ignore))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-wrap
                                    'keymap-popup--descriptions))
         (buf (get-buffer-create "*keymap-popup-test*"))
         (map (keymap-popup--build-wrapper-map
               keymap-popup--test-wrap descs buf ?q)))
    (unwind-protect
        (progn
          (should (functionp (keymap-lookup map "q")))
          (should (eq (keymap-lookup map "c") #'ignore)))
      (kill-buffer buf))))

(ert-deftest keymap-popup-test-wrapper-map-has-c-u ()
  (eval '(keymap-popup-define keymap-popup--test-wrap-cu
           "c" ("Comment" ignore))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-wrap-cu
                                    'keymap-popup--descriptions))
         (buf (get-buffer-create "*keymap-popup-test*"))
         (map (keymap-popup--build-wrapper-map
               keymap-popup--test-wrap-cu descs buf ?q)))
    (unwind-protect
        (should (functionp (keymap-lookup map "C-u")))
      (kill-buffer buf))))

(ert-deftest keymap-popup-test-inapt-keys-collected ()
  (let ((descs (list (list (list :name nil
                                 :entries (list (list :key "m" :type 'suffix
                                                      :inapt-if (lambda () t))
                                                (list :key "c" :type 'suffix)))))))
    (should (equal (keymap-popup--inapt-keys descs) '("m")))))

(ert-deftest keymap-popup-test-inapt-keys-from-group ()
  (let ((descs (list (list (list :name "G" :inapt-if (lambda () t)
                                 :entries (list (list :key "a" :type 'suffix)
                                                (list :key "b" :type 'suffix)))))))
    (should (equal (keymap-popup--inapt-keys descs) '("a" "b")))))

(ert-deftest keymap-popup-test-submenu-keys-collected ()
  (let ((descs (list (list (list :name nil
                                 :entries (list (list :key "a" :type 'keymap :target 'sub)
                                                (list :key "c" :type 'suffix)))))))
    (should (equal (keymap-popup--submenu-keys descs) '(("a" . sub))))))

(ert-deftest keymap-popup-test-stay-open-suffix-keys ()
  (let ((descs (list (list (list :name nil
                                 :entries (list (list :key "g" :type 'suffix :stay-open t)
                                                (list :key "v" :type 'switch :variable 'x)
                                                (list :key "c" :type 'suffix)))))))
    (should (equal (keymap-popup--stay-open-suffix-keys descs) '("g")))))

;;; Add/remove entry tests

(ert-deftest keymap-popup-test-add-entry ()
  (eval '(keymap-popup-define keymap-popup--test-add
           :group "Actions"
           "c" ("Comment" ignore))
        t)
  (keymap-popup-add-entry keymap-popup--test-add "z" "New" #'forward-char "Actions")
  (should (eq (keymap-lookup keymap-popup--test-add "z") #'forward-char))
  (let* ((descs (keymap-popup--meta keymap-popup--test-add
                                    'keymap-popup--descriptions))
         (output (keymap-popup--render nil descs)))
    (should (string-match-p "New" output))))

(ert-deftest keymap-popup-test-remove-entry ()
  (eval '(keymap-popup-define keymap-popup--test-rm
           :group "Actions"
           "c" ("Comment" ignore)
           "r" ("Reply" ignore))
        t)
  (keymap-popup-remove-entry keymap-popup--test-rm "r")
  (should (null (keymap-lookup keymap-popup--test-rm "r")))
  (let* ((descs (keymap-popup--meta keymap-popup--test-rm
                                    'keymap-popup--descriptions))
         (output (keymap-popup--render nil descs)))
    (should (string-match-p "Comment" output))
    (should-not (string-match-p "Reply" output))))

;;; Annotate tests

(defvar keymap-popup--test-annotate-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" #'forward-char)
    (keymap-set map "b" #'backward-char)
    (keymap-set map "c" #'kill-line)
    map))

(ert-deftest keymap-popup-test-annotate-parse ()
  (let ((entry (keymap-popup--parse-entry 'forward-char '("Forward"))))
    (should (eq (plist-get entry :command) 'forward-char))
    (should-not (plist-get entry :key))
    (should (equal (plist-get entry :description) "Forward"))))

(ert-deftest keymap-popup-test-annotate-parse-with-props ()
  (let ((entry (keymap-popup--parse-entry 'forward-char '("Forward" :stay-open t))))
    (should (eq (plist-get entry :command) 'forward-char))
    (should (plist-get entry :stay-open))))

(ert-deftest keymap-popup-test-annotate-parse-bare-string ()
  (let ((entry (keymap-popup--parse-entry 'forward-char "Forward")))
    (should (eq (plist-get entry :command) 'forward-char))
    (should (equal (plist-get entry :description) "Forward"))))

(ert-deftest keymap-popup-test-resolve-key ()
  (let* ((entry (list :key nil :description "Forward" :type 'suffix
                      :command 'forward-char))
         (resolved (keymap-popup--resolve-key entry keymap-popup--test-annotate-map)))
    (should resolved)
    (should (equal (plist-get resolved :key) "a"))))

(ert-deftest keymap-popup-test-resolve-key-unbound ()
  (let ((entry (list :key nil :description "Nope" :type 'suffix
                     :command 'some-nonexistent-command-xyz)))
    (should-not (keymap-popup--resolve-key entry keymap-popup--test-annotate-map))))

(ert-deftest keymap-popup-test-resolve-descriptions ()
  (let* ((rows (list (list (list :name "Test"
                                 :entries (list (list :key nil :description "Forward"
                                                      :type 'suffix :command 'forward-char)
                                                (list :key nil :description "Nope"
                                                      :type 'suffix :command 'nonexistent-xyz))))))
         (resolved (keymap-popup--resolve-descriptions rows keymap-popup--test-annotate-map))
         (entries (plist-get (car (car resolved)) :entries)))
    (should (= (length entries) 1))
    (should (equal (plist-get (car entries) :key) "a"))))

(ert-deftest keymap-popup-test-annotate-macro ()
  (eval '(keymap-popup-annotate keymap-popup--test-annotate-map
           :group "Move"
           forward-char "Forward"
           backward-char "Backward")
        t)
  (should (eq (keymap-popup--meta keymap-popup--test-annotate-map
                                  'keymap-popup--annotated)
              'yes))
  (let* ((descs (keymap-popup--meta keymap-popup--test-annotate-map
                                    'keymap-popup--descriptions))
         (entries (plist-get (car (car descs)) :entries)))
    (should (= (length entries) 2))
    (should (eq (plist-get (car entries) :command) 'forward-char))))

;;; Metadata tests

(ert-deftest keymap-popup-test-meta-read-write ()
  (let ((map (make-sparse-keymap)))
    (setf (keymap-popup--meta map 'keymap-popup--descriptions) '(test-data))
    (should (equal (keymap-popup--meta map 'keymap-popup--descriptions) '(test-data)))))

(ert-deftest keymap-popup-test-meta-nil-for-missing ()
  (let ((map (make-sparse-keymap)))
    (should (null (keymap-popup--meta map 'keymap-popup--descriptions)))))

(ert-deftest keymap-popup-test-no-descriptions-error ()
  (let ((map (make-sparse-keymap)))
    (should-error (keymap-popup map) :type 'user-error)))

(provide 'keymap-popup-tests)
;;; keymap-popup-tests.el ends here
