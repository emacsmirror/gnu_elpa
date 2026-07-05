;;; keymap-popup-tests.el --- Tests -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for keymap-popup.  Run via `make test'.

;;; Code:

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

(ert-deftest keymap-popup-test-split-groups-trailing-row ()
  "A trailing :row with nothing after it yields no empty row."
  (should (= 1 (length (keymap-popup--split-groups
                        '("a" ("A" ignore) :row))))))

(ert-deftest keymap-popup-test-parse-keymap-entry ()
  (let ((result (keymap-popup--parse-entry "a" '("Metadata" :keymap my-sub-map))))
    (should (equal (plist-get result :type) 'keymap))
    (should (equal (plist-get result :target) 'my-sub-map))))

;;; Infix generator tests

(ert-deftest keymap-popup-test-entry-command ()
  (should (eq (keymap-popup--entry-command 'map '(:type suffix :command my-cmd))
              'my-cmd))
  (should (eq (keymap-popup--entry-command 'map '(:type switch :variable my-var))
              'map--toggle-my-var))
  (should (eq (keymap-popup--entry-command 'map '(:type keymap :target my-sub))
              'map--enter-my-sub)))

;;; Macro tests

(ert-deftest keymap-popup-test-macro-declarations-filter-keymap-expressions ()
  (let* ((forms (cdr (macroexpand-1
                      '(keymap-popup-define keymap-popup--test-root-map
                         :parent (make-sparse-keymap)
                         "a" ("A" :keymap keymap-popup--test-sub-map)
                         "b" ("B" :keymap keymap-popup--test-sub-map)
                         "c" ("C" :keymap (make-sparse-keymap))))))
         (declarations (seq-filter
                        (lambda (form) (eq (car-safe form) 'defvar))
                        forms)))
    (should (equal declarations
                   '((defvar keymap-popup--test-root-map)
                     (defvar keymap-popup--test-sub-map))))))

(ert-deftest keymap-popup-test-macro-declarations-omit-nil-parent ()
  (let* ((forms (cdr (macroexpand-1
                      '(keymap-popup-define keymap-popup--test-no-parent-map
                         "r" ("Refresh" ignore)))))
         (declarations (seq-filter
                        (lambda (form) (eq (car-safe form) 'defvar))
                        forms)))
    (should (equal declarations
                   '((defvar keymap-popup--test-no-parent-map))))))

(ert-deftest keymap-popup-test-macro-declarations-include-parent ()
  (let* ((forms (cdr (macroexpand-1
                      '(keymap-popup-define keymap-popup--test-child-map
                         :parent keymap-popup--test-parent-map
                         "s" ("Sub" :keymap keymap-popup--test-parent-map)))))
         (declarations (seq-filter
                        (lambda (form) (eq (car-safe form) 'defvar))
                        forms)))
    (should (equal declarations
                   '((defvar keymap-popup--test-child-map)
                     (defvar keymap-popup--test-parent-map))))))

(ert-deftest keymap-popup-test-macro-expands-to-attach-meta ()
  "Both macros attach metadata via one attach-meta call, no raw setf."
  (dolist (form '((keymap-popup-define keymap-popup--test-attach-form-map
                    :persistent t
                    "r" ("Refresh" ignore))
                  (keymap-popup-annotate keymap-popup--test-attach-form-map
                    :exit-key "x"
                    forward-char "Forward")))
    (let ((forms (cdr (macroexpand-1 form))))
      (should (= 1 (seq-count
                    (lambda (f) (eq (car-safe f) 'keymap-popup--attach-meta))
                    forms)))
      (should-not (seq-some (lambda (f) (eq (car-safe f) 'setf)) forms)))))

(ert-deftest keymap-popup-test-define-switch-entry-stores-command ()
  "Define stores the generated toggle command on switch entries."
  (eval '(keymap-popup-define keymap-popup--test-entry-cmd-map
           :group "Toggles"
           "v" ("Verbose" :switch keymap-popup--test-entry-cmd-verbose))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-entry-cmd-map
                                    'descriptions))
         (entry (car (plist-get (caar descs) :entries))))
    (should (eq (plist-get entry :command)
                (keymap-popup--toggle-name
                 'keymap-popup--test-entry-cmd-map
                 'keymap-popup--test-entry-cmd-verbose)))
    (should (eq (plist-get entry :variable)
                'keymap-popup--test-entry-cmd-verbose))))

(ert-deftest keymap-popup-test-define-keymap-entry-stores-command ()
  "Define stores the generated enter command on keymap entries."
  (defvar keymap-popup--test-entry-cmd-sub (make-sparse-keymap))
  (eval '(keymap-popup-define keymap-popup--test-entry-cmd-root
           :group "Menus"
           "s" ("Sub" :keymap keymap-popup--test-entry-cmd-sub))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-entry-cmd-root
                                    'descriptions))
         (entry (car (plist-get (caar descs) :entries))))
    (should (eq (plist-get entry :command)
                (keymap-popup--enter-name
                 'keymap-popup--test-entry-cmd-root
                 'keymap-popup--test-entry-cmd-sub)))
    (should (eq (plist-get entry :target)
                keymap-popup--test-entry-cmd-sub))))

(ert-deftest keymap-popup-test-annotate-entries-lack-generated-command ()
  "Annotate emits no generated commands, so entries carry none."
  (defvar keymap-popup--test-entry-cmd-sub (make-sparse-keymap))
  (setq keymap-popup--test-annotate-map (make-sparse-keymap))
  (eval '(keymap-popup-annotate keymap-popup--test-annotate-map
           :group "Menus"
           "s" ("Sub" :keymap keymap-popup--test-entry-cmd-sub))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-annotate-map
                                    'descriptions))
         (entry (car (plist-get (caar descs) :entries))))
    (should (eq (plist-get entry :type) 'keymap))
    (should-not (plist-get entry :command))))

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
                                    'descriptions))
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

(ert-deftest keymap-popup-test-macro-declares-maps-before-helpers ()
  (let* ((forms (cdr (macroexpand-1
                      '(keymap-popup-define keymap-popup--test-declared-map
                         "Test."
                         :parent keymap-popup--test-declared-parent
                         "s" ("Sub" :keymap keymap-popup--test-declared-sub)))))
         (map-pos (cl-position '(defvar keymap-popup--test-declared-map)
                               forms :test #'equal))
         (parent-pos (cl-position '(defvar keymap-popup--test-declared-parent)
                                  forms :test #'equal))
         (sub-pos (cl-position '(defvar keymap-popup--test-declared-sub)
                               forms :test #'equal))
         (launcher-pos (cl-position-if
                        (lambda (form)
                          (and (eq (car-safe form) 'defun)
                               (eq (cadr form)
                                   'keymap-popup--test-declared-map-popup)))
                        forms))
         (enter-pos (cl-position-if
                     (lambda (form)
                       (and (eq (car-safe form) 'defun)
                            (eq (cadr form)
                                'keymap-popup--test-declared-map--enter-keymap-popup--test-declared-sub)))
                     forms))
         (keymap-pos (cl-position-if
                      (lambda (form) (eq (car-safe form) 'defvar-keymap))
                      forms)))
    (should map-pos)
    (should parent-pos)
    (should sub-pos)
    (should launcher-pos)
    (should enter-pos)
    (should keymap-pos)
    (should (< map-pos launcher-pos))
    (should (< sub-pos enter-pos))
    (should (< parent-pos keymap-pos))))

(ert-deftest keymap-popup-test-macro-emits-launcher-defun ()
  "`keymap-popup-define' emits a named `defun' for the popup launcher."
  (eval '(keymap-popup-define keymap-popup--test-launcher
           "Test." "c" ("Comment" ignore))
        t)
  (should (fboundp 'keymap-popup--test-launcher-popup))
  (should (commandp 'keymap-popup--test-launcher-popup))
  (should (eq (keymap-lookup keymap-popup--test-launcher "h")
              #'keymap-popup--test-launcher-popup)))

(ert-deftest keymap-popup-test-macro-emits-enterer-defun ()
  "`keymap-popup-define' emits a named `defun' for each `:keymap' sub-menu key."
  (defvar keymap-popup--test-sub-map (make-sparse-keymap))
  (eval '(keymap-popup-define keymap-popup--test-with-sub
           "Test." "s" ("Sub" :keymap keymap-popup--test-sub-map))
        t)
  (should (fboundp 'keymap-popup--test-with-sub--enter-keymap-popup--test-sub-map))
  (should (eq (keymap-lookup keymap-popup--test-with-sub "s")
              #'keymap-popup--test-with-sub--enter-keymap-popup--test-sub-map)))

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
                                    'descriptions))
         (row (car descs)))
    (should (= (length descs) 1))
    (should (= (length row) 1))
    (should (equal (plist-get (car row) :name) "Actions"))))

(ert-deftest keymap-popup-test-macro-default-popup-key ()
  (eval '(keymap-popup-define keymap-popup--test-map-defkey
           "c" ("Comment" ignore))
        t)
  (should (functionp (keymap-lookup keymap-popup--test-map-defkey "h"))))

(ert-deftest keymap-popup-test-macro-popup-key-resolved-at-load ()
  "The default popup key is read at load time, not expansion time."
  (let ((expansion (macroexpand-1
                    '(keymap-popup-define keymap-popup--test-map-loadkey
                       "c" ("Comment" ignore)))))
    (let ((keymap-popup-default-popup-key "!"))
      (eval expansion t))
    (should (functionp (keymap-lookup keymap-popup--test-map-loadkey "!")))
    (should (null (keymap-lookup keymap-popup--test-map-loadkey "h")))))

(ert-deftest keymap-popup-test-macro-no-exit-key-stores-no-metadata ()
  "Without :exit-key, no exit-key metadata is stored; display falls back."
  (eval '(keymap-popup-define keymap-popup--test-map-noexit
           "c" ("Comment" ignore))
        t)
  (should-not (keymap-popup--meta keymap-popup--test-map-noexit 'exit-key)))

(ert-deftest keymap-popup-test-macro-custom-popup-key ()
  (eval '(keymap-popup-define keymap-popup--test-map-custkey
           :popup-key "?"
           "c" ("Comment" ignore))
        t)
  (should (functionp (keymap-lookup keymap-popup--test-map-custkey "?")))
  (should (null (keymap-lookup keymap-popup--test-map-custkey "h"))))

(ert-deftest keymap-popup-test-macro-exit-key ()
  (eval '(keymap-popup-define keymap-popup--test-exit-key
           :exit-key "x"
           "c" ("Comment" ignore))
        t)
  (should (equal (keymap-popup--meta keymap-popup--test-exit-key
                                     'exit-key)
                 "x")))

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

(ert-deftest keymap-popup-test-macro-keyword-order-independent ()
  "Keywords can appear in any order."
  (eval '(keymap-popup-define keymap-popup--test-kworder
           :description "Dynamic"
           :parent special-mode-map
           :popup-key "?"
           :exit-key "x"
           "c" ("Comment" ignore))
        t)
  (should (functionp (keymap-lookup keymap-popup--test-kworder "?")))
  (should (equal (keymap-popup--meta keymap-popup--test-kworder 'exit-key) "x"))
  (should (eq (keymap-parent keymap-popup--test-kworder) special-mode-map)))

;;; Renderer tests

(ert-deftest keymap-popup-test-render-suffix ()
  (let* ((rows (list (list (list :name "Actions"
                                 :entries (list (list :key "c" :description "Comment"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render rows)))
    (should (string-match-p "Actions" output))
    (should (string-match-p "Comment" output))))

(ert-deftest keymap-popup-test-render-switch-value ()
  (defvar keymap-popup--test-render-sw nil)
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "v" :description "Verbose"
                                                      :type 'switch
                                                      :variable 'keymap-popup--test-render-sw))))))
         (output-off (keymap-popup--render rows)))
    (should (string-match-p "\\[off\\]" output-off))
    (setq keymap-popup--test-render-sw t)
    (let ((output-on (keymap-popup--render rows)))
      (should (string-match-p "\\[on\\]" output-on)))))

(ert-deftest keymap-popup-test-render-if-hidden ()
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "b" :description "Browse"
                                                      :type 'suffix :command 'ignore
                                                      :if (lambda () nil)))))))
         (output (keymap-popup--render rows)))
    (should-not (string-match-p "Browse" output))))

(ert-deftest keymap-popup-test-render-if-shown ()
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "b" :description "Browse"
                                                      :type 'suffix :command 'ignore
                                                      :if (lambda () t)))))))
         (output (keymap-popup--render rows)))
    (should (string-match-p "Browse" output))))

(ert-deftest keymap-popup-test-render-dynamic-description ()
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "d"
                                                      :description (lambda () "Dynamic!")
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render rows)))
    (should (string-match-p "Dynamic!" output))))

;;; Column layout tests

(ert-deftest keymap-popup-test-render-columns-side-by-side ()
  (let* ((rows (list (list (list :name "Alpha"
                                 :entries (list (list :key "a" :description "Aaa"
                                                      :type 'suffix :command 'ignore)))
                           (list :name "Beta"
                                 :entries (list (list :key "b" :description "Bbb"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render rows))
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
         (output (keymap-popup--render rows)))
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
         (output (keymap-popup--render rows))
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

(ert-deftest keymap-popup-test-keep-popup-p ()
  (let ((descs (list (list (list :name nil
                                 :entries (list (list :key "c" :type 'suffix :command 'ignore)
                                                (list :key "v" :type 'switch :variable 'x)
                                                (list :key "a" :type 'keymap :target 'sub)
                                                (list :key "g" :type 'suffix :command 'ignore
                                                      :stay-open t)))))))
    (should-not (keymap-popup--keep-popup-p descs "c"))
    (should (keymap-popup--keep-popup-p descs "v"))
    (should (keymap-popup--keep-popup-p descs "a"))
    ;; stay-open suffixes refresh in place, kept open
    (should (keymap-popup--keep-popup-p descs "g"))
    ;; Inapt-key handling lives in the keep-pred via `this-command'
    ;; (see `keymap-popup--make-keep-pred'), not in keep-popup-p.
    ;; C-u is detected via `this-command' too.
    (should-not (keymap-popup--keep-popup-p descs "C-u"))))

;;; C-u rendering tests

(ert-deftest keymap-popup-test-c-u-desc-in-normal-mode ()
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "s" :description "Submit"
                                                      :type 'suffix :command 'ignore
                                                      :c-u "force push"))))))
         (output (keymap-popup--render rows)))
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
         (output (keymap-popup--render rows t)))
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
         (output (keymap-popup--render rows)))
    (should (string-match-p "Merge" output))
    (let ((pos (string-match "Merge" output)))
      (should (eq (get-text-property pos 'face output) 'keymap-popup-inapt)))))

(ert-deftest keymap-popup-test-inapt-not-when-predicate-nil ()
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "m" :description "Merge"
                                                      :type 'suffix :command 'ignore
                                                      :inapt-if (lambda () nil)))))))
         (output (keymap-popup--render rows)))
    (let ((pos (string-match "Merge" output)))
      (should-not (eq (get-text-property pos 'face output) 'keymap-popup-inapt)))))

(ert-deftest keymap-popup-test-keymap-entry-gets-submenu-face ()
  (let* ((rows (list (list (list :name nil
                                 :entries (list (list :key "a" :description "Sub"
                                                      :type 'keymap :target 'x))))))
         (output (keymap-popup--render rows)))
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
         (output (keymap-popup--render rows)))
    (should (string-match-p "Alpha" output))
    (should-not (string-match-p "Beta" output))))

(ert-deftest keymap-popup-test-group-if-shown ()
  (let* ((rows (list (list (list :name "Shown" :if (lambda () t)
                                 :entries (list (list :key "a" :description "Alpha"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render rows)))
    (should (string-match-p "Alpha" output))
    (should (string-match-p "Shown" output))))

(ert-deftest keymap-popup-test-group-inapt-grays-all ()
  (let* ((rows (list (list (list :name "Disabled"
                                 :inapt-if (lambda () t)
                                 :entries (list (list :key "a" :description "Alpha"
                                                      :type 'suffix :command 'ignore)
                                                (list :key "b" :description "Beta"
                                                      :type 'suffix :command 'ignore))))))
         (output (keymap-popup--render rows)))
    (let ((pos-a (string-match "Alpha" output))
          (pos-b (string-match "Beta" output)))
      (should (eq (get-text-property pos-a 'face output) 'keymap-popup-inapt))
      (should (eq (get-text-property pos-b 'face output) 'keymap-popup-inapt)))))

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
                                   'descriptions)))
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
                                    'descriptions))
         (entry (keymap-popup--find-entry-by-key descs "g")))
    (should (plist-get entry :stay-open))))

(ert-deftest keymap-popup-test-dynamic-group-name ()
  (eval '(keymap-popup-define keymap-popup--test-dyngrp
           :group (lambda () "Dynamic Group")
           "c" ("Comment" ignore))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-dyngrp
                                    'descriptions))
         (output (keymap-popup--render descs)))
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

(ert-deftest keymap-popup-test-dedupe-preserves-nil-keys ()
  "Multiple entries with nil :key (annotated, pre-resolve) are all kept."
  (let* ((descs `(((:name "Move" :entries ((:key nil :command forward-char
                                                 :description "Right")
                                           (:key nil :command backward-char
                                                 :description "Left"))))))
         (deduped (keymap-popup--dedupe-descriptions descs))
         (entries (plist-get (caar deduped) :entries)))
    (should (= (length entries) 2))
    (should (equal (mapcar (lambda (e) (plist-get e :description)) entries)
                   '("Right" "Left")))))

(ert-deftest keymap-popup-test-collect-descriptions-child-shadows-parent ()
  "Parent/child key collision: child wins, fully-shadowed parent groups disappear."
  (eval '(keymap-popup-define keymap-popup--test-shadow-parent
           :group "P"
           "a" ("Parent A" ignore)
           :group "Q"
           "b" ("Parent B" ignore))
        t)
  (eval '(keymap-popup-define keymap-popup--test-shadow-child
           :parent keymap-popup--test-shadow-parent
           :group "C"
           "a" ("Child A" ignore))
        t)
  (let* ((all (keymap-popup--collect-descriptions keymap-popup--test-shadow-child))
         (group-names (cl-loop for row in all
                               append (mapcar (lambda (g) (plist-get g :name)) row)))
         (entries (cl-loop for row in all
                           append (cl-loop for group in row
                                           append (plist-get group :entries))))
         (keys (mapcar (lambda (e) (plist-get e :key)) entries))
         (descs (mapcar (lambda (e) (plist-get e :description)) entries)))
    ;; Parent group "P" is fully shadowed and dropped; "Q" survives.
    (should (equal group-names '("C" "Q")))
    (should (equal keys '("a" "b")))
    (should (equal descs '("Child A" "Parent B")))))

;;; Macro via-macro integration tests

(ert-deftest keymap-popup-test-inapt-via-macro ()
  (eval '(keymap-popup-define keymap-popup--test-inapt-map
           "m" ("Merge" ignore :inapt-if (lambda () t))
           "c" ("Comment" ignore))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-inapt-map
                                    'descriptions))
         (output (keymap-popup--render descs)))
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
                                    'descriptions))
         (output (keymap-popup--render descs))
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
                                    'descriptions))
         (output (keymap-popup--render descs)))
    (should-not (string-match-p "Alpha" output))
    (should (string-match-p "Beta" output))))

(ert-deftest keymap-popup-test-if-on-switch ()
  "An :if predicate returning nil makes the switch key act unbound."
  (eval '(keymap-popup-define keymap-popup--test-if-sw
           "v" ("Verbose" :switch keymap-popup--test-if-sw-var
                :if (lambda () nil)))
        t)
  ;; Filter returns nil, so the key resolves to nothing at the keymap level.
  (should-not (keymap-lookup keymap-popup--test-if-sw "v"))
  (let* ((descs (keymap-popup--meta keymap-popup--test-if-sw
                                    'descriptions))
         (output (keymap-popup--render descs)))
    (should-not (string-match-p "Verbose" output))))

;;; Keymap-level predicate enforcement

(ert-deftest keymap-popup-test-if-filter-blocks-when-pred-nil ()
  "An :if predicate returning nil unbinds the key via menu-item :filter."
  (eval '(keymap-popup-define keymap-popup--test-if-block
           "x" ("X" ignore :if (lambda () nil)))
        t)
  (should-not (keymap-lookup keymap-popup--test-if-block "x")))

(ert-deftest keymap-popup-test-if-filter-passes-when-pred-t ()
  "An :if predicate returning non-nil exposes the underlying command."
  (eval '(keymap-popup-define keymap-popup--test-if-pass
           "x" ("X" ignore :if (lambda () t)))
        t)
  (should (eq (keymap-lookup keymap-popup--test-if-pass "x") #'ignore)))

(ert-deftest keymap-popup-test-inapt-filter-routes-to-stub ()
  "An active :inapt-if predicate reroutes dispatch to `keymap-popup--inapt-stub'."
  (eval '(keymap-popup-define keymap-popup--test-inapt-route
           "x" ("X" ignore :inapt-if (lambda () t)))
        t)
  (should (eq (keymap-lookup keymap-popup--test-inapt-route "x")
              #'keymap-popup--inapt-stub)))

(ert-deftest keymap-popup-test-inapt-filter-passes-when-pred-nil ()
  "An inactive :inapt-if predicate exposes the underlying command."
  (eval '(keymap-popup-define keymap-popup--test-inapt-pass
           "x" ("X" ignore :inapt-if (lambda () nil)))
        t)
  (should (eq (keymap-lookup keymap-popup--test-inapt-pass "x") #'ignore)))

(ert-deftest keymap-popup-test-dynamic-inapt-toggle ()
  "Toggling a buffer-local that backs :inapt-if changes dispatch per press."
  (defvar keymap-popup--test-dyn-flag nil)
  (eval '(keymap-popup-define keymap-popup--test-dyn-map
           "x" ("X" ignore
                :inapt-if (lambda () keymap-popup--test-dyn-flag)))
        t)
  (let ((keymap-popup--test-dyn-flag nil))
    (should (eq (keymap-lookup keymap-popup--test-dyn-map "x") #'ignore)))
  (let ((keymap-popup--test-dyn-flag t))
    (should (eq (keymap-lookup keymap-popup--test-dyn-map "x")
                #'keymap-popup--inapt-stub))))

(ert-deftest keymap-popup-test-group-if-blocks-dispatch ()
  "Group :if applies to each entry's keymap binding."
  (eval '(keymap-popup-define keymap-popup--test-grp-if
           :group ("Hidden" :if (lambda () nil))
           "a" ("A" ignore))
        t)
  (should-not (keymap-lookup keymap-popup--test-grp-if "a")))

(ert-deftest keymap-popup-test-group-inapt-routes-to-stub ()
  "Group :inapt-if reroutes each entry's dispatch to the inapt stub."
  (eval '(keymap-popup-define keymap-popup--test-grp-inapt
           :group ("Off" :inapt-if (lambda () t))
           "a" ("A" ignore))
        t)
  (should (eq (keymap-lookup keymap-popup--test-grp-inapt "a")
              #'keymap-popup--inapt-stub)))

(ert-deftest keymap-popup-test-if-and-inapt-combine ()
  ":if takes precedence over :inapt-if when both are present."
  (eval '(keymap-popup-define keymap-popup--test-both
           "x" ("X" ignore
                :if (lambda () nil)
                :inapt-if (lambda () t)))
        t)
  (should-not (keymap-lookup keymap-popup--test-both "x")))

(ert-deftest keymap-popup-test-no-predicates-binds-bare-command ()
  "Entries without predicates are bound directly, not through menu-item."
  (eval '(keymap-popup-define keymap-popup--test-bare
           "x" ("X" ignore))
        t)
  (should (eq (keymap-lookup keymap-popup--test-bare "x") #'ignore))
  ;; Internal: the raw binding is the bare command, not a menu-item form.
  (should (eq (lookup-key keymap-popup--test-bare "x") #'ignore)))

;;; Wrapper map tests

(ert-deftest keymap-popup-test-wrapper-map-has-exit-key ()
  (eval '(keymap-popup-define keymap-popup--test-wrap
           "c" ("Comment" ignore))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-wrap
                                    'descriptions))
         (buf (get-buffer-create "*keymap-popup-test*"))
         (map (keymap-popup--build-wrapper-map
               keymap-popup--test-wrap descs buf "q")))
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
                                    'descriptions))
         (buf (get-buffer-create "*keymap-popup-test*"))
         (map (keymap-popup--build-wrapper-map
               keymap-popup--test-wrap-cu descs buf "q")))
    (unwind-protect
        (should (functionp (keymap-lookup map "C-u")))
      (kill-buffer buf))))

(ert-deftest keymap-popup-test-switch-override-skips-prefix-when-inapt ()
  "Switch override preserves prefix-mode when the binding resolves to the inapt stub."
  (defvar keymap-popup--test-sw-flag t)
  (eval '(keymap-popup-define keymap-popup--test-sw-inapt
           "v" ("Verbose" :switch keymap-popup--test-sw-inapt-var
                :inapt-if (lambda () keymap-popup--test-sw-flag)))
        t)
  (let* ((descs (keymap-popup--meta keymap-popup--test-sw-inapt 'descriptions))
         (buf (get-buffer-create keymap-popup--buffer-name)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local keymap-popup--prefix-mode t)
            (setq-local keymap-popup--active-descriptions descs))
          (let* ((overrides (keymap-popup--switch-overrides
                             keymap-popup--test-sw-inapt '("v") buf))
                 (override (cdr (assoc "v" overrides)))
                 (keymap-popup--test-sw-flag t))
            (let ((prefix-arg '(4)))
              (call-interactively override)
              ;; Inapt: prefix-mode should NOT be cleared.
              (should (buffer-local-value 'keymap-popup--prefix-mode buf)))
            ;; Flip predicate off: prefix-mode is now consumed.
            (let ((keymap-popup--test-sw-flag nil)
                  (prefix-arg '(4)))
              (call-interactively override)
              (should-not (buffer-local-value 'keymap-popup--prefix-mode buf)))))
      (kill-buffer buf))))

(ert-deftest keymap-popup-test-stay-open-override-dispatch ()
  "Stay-open override calls the real binding and refreshes in place."
  (let ((count 0)
        (map (make-sparse-keymap))
        (buf (get-buffer-create keymap-popup--buffer-name))
        (descs '(((:name nil :entries ((:key "g" :description "Go"
                                             :type suffix :stay-open t)))))))
    (keymap-set map "g" (lambda () (interactive) (setq count (1+ count))))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local keymap-popup--active-descriptions descs
                        keymap-popup--source-buffer buf))
          (let* ((overrides (keymap-popup--stay-open-overrides map '("g") buf))
                 (override (cdr (assoc "g" overrides))))
            (call-interactively override)
            (should (= count 1))
            (should (buffer-live-p buf))))
      (kill-buffer buf))))

(ert-deftest keymap-popup-test-classify-submenus ()
  "Keymap entries are classified as submenus."
  (let* ((descs (list (list (list :name nil
                                  :entries (list (list :key "a" :type 'keymap :target 'sub)
                                                 (list :key "c" :type 'suffix))))))
         (classified (keymap-popup--classify-entries descs)))
    (should (equal (plist-get classified :submenus) '(("a" . sub))))))

(ert-deftest keymap-popup-test-classify-stay-open ()
  "Stay-open suffix entries are classified."
  (let* ((descs (list (list (list :name nil
                                  :entries (list (list :key "g" :type 'suffix :stay-open t)
                                                 (list :key "v" :type 'switch :variable 'x)
                                                 (list :key "c" :type 'suffix))))))
         (classified (keymap-popup--classify-entries descs)))
    (should (equal (plist-get classified :stay-open) '("g")))))

(ert-deftest keymap-popup-test-push-submenu-state ()
  "Pushing a sub-menu stacks parent state and activates the child's wrapper."
  (eval '(keymap-popup-define keymap-popup--test-push-child
           "Child." "c" ("Child cmd" ignore))
        t)
  (let ((buf (get-buffer-create keymap-popup--buffer-name))
        (parent-map (make-sparse-keymap))
        (received nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local keymap-popup--source-buffer buf
                        keymap-popup--active-keymap parent-map
                        keymap-popup--active-descriptions '(((:name nil :entries nil)))
                        keymap-popup--active-docstring nil
                        keymap-popup--active-exit-key "q"
                        keymap-popup--stack nil))
          (cl-letf (((symbol-function 'set-transient-map)
                     (lambda (map &rest _) (setq received map))))
            (keymap-popup--push-submenu buf keymap-popup--test-push-child))
          (with-current-buffer buf
            (should (= (length keymap-popup--stack) 1))
            (should (eq (plist-get (car keymap-popup--stack) :keymap) parent-map))
            (should (eq keymap-popup--active-keymap keymap-popup--test-push-child))
            (should keymap-popup--wrapper-map)
            (should (eq received keymap-popup--wrapper-map))))
      (kill-buffer buf))))

;;; Add/remove entry tests

(ert-deftest keymap-popup-test-add-entry ()
  (eval '(keymap-popup-define keymap-popup--test-add
           :group "Actions"
           "c" ("Comment" ignore))
        t)
  (keymap-popup-add-entry keymap-popup--test-add "z" "New" #'forward-char "Actions")
  (should (eq (keymap-lookup keymap-popup--test-add "z") #'forward-char))
  (let* ((descs (keymap-popup--meta keymap-popup--test-add
                                    'descriptions))
         (output (keymap-popup--render descs)))
    (should (string-match-p "New" output))))

(ert-deftest keymap-popup-test-add-entry-replaces-existing ()
  "Adding an entry whose key already exists replaces the prior entry."
  (eval '(keymap-popup-define keymap-popup--test-add-replace
           :group "Actions"
           "c" ("Old comment" ignore))
        t)
  (keymap-popup-add-entry keymap-popup--test-add-replace
                          "c" "New comment" #'backward-char "Actions")
  (should (eq (keymap-lookup keymap-popup--test-add-replace "c") #'backward-char))
  (let* ((descs (keymap-popup--meta keymap-popup--test-add-replace 'descriptions))
         (output (keymap-popup--render descs)))
    (should (string-match-p "New comment" output))
    (should-not (string-match-p "Old comment" output))))

(ert-deftest keymap-popup-test-add-entry-moves-across-groups ()
  "Re-adding a key under a different group removes it from the original group."
  (eval '(keymap-popup-define keymap-popup--test-add-move
           :group "Old"
           "x" ("X-old" ignore)
           :group "New"
           "n" ("Anchor" ignore))
        t)
  (keymap-popup-add-entry keymap-popup--test-add-move
                          "x" "X-new" #'forward-char "New")
  (let* ((descs (keymap-popup--meta keymap-popup--test-add-move 'descriptions))
         (groups (cl-loop for row in descs append row))
         (old (cl-find "Old" groups :key (lambda (g) (plist-get g :name))
                       :test #'equal))
         (new (cl-find "New" groups :key (lambda (g) (plist-get g :name))
                       :test #'equal))
         (new-keys (mapcar (lambda (e) (plist-get e :key))
                           (plist-get new :entries))))
    (should (null (plist-get old :entries)))
    (should (member "x" new-keys))
    (should (member "n" new-keys))))

(ert-deftest keymap-popup-test-remove-entry ()
  (eval '(keymap-popup-define keymap-popup--test-rm
           :group "Actions"
           "c" ("Comment" ignore)
           "r" ("Reply" ignore))
        t)
  (keymap-popup-remove-entry keymap-popup--test-rm "r")
  (should (null (keymap-lookup keymap-popup--test-rm "r")))
  (let* ((descs (keymap-popup--meta keymap-popup--test-rm
                                    'descriptions))
         (output (keymap-popup--render descs)))
    (should (string-match-p "Comment" output))
    (should-not (string-match-p "Reply" output))))

(ert-deftest keymap-popup-test-add-remove-preserve-group-props ()
  "Group :if survives `keymap-popup-add-entry' and `keymap-popup-remove-entry'."
  (eval '(keymap-popup-define keymap-popup--test-group-props
           :group ("Actions" :if (lambda () t))
           "c" ("Comment" ignore)
           "r" ("Reply" ignore))
        t)
  (keymap-popup-add-entry keymap-popup--test-group-props
                          "z" "New" #'forward-char "Actions")
  (keymap-popup-remove-entry keymap-popup--test-group-props "r")
  (let* ((descs (keymap-popup--meta keymap-popup--test-group-props
                                    'descriptions))
         (group (caar descs)))
    (should (functionp (plist-get group :if)))
    (should (equal (mapcar (lambda (e) (plist-get e :key))
                           (plist-get group :entries))
                   '("c" "z")))))

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

(ert-deftest keymap-popup-test-annotate-parse-lambda-description ()
  "Bare lambda as description is preserved, not destructured."
  (let* ((desc '(lambda () (if t "On" "Off")))
         (entry (keymap-popup--parse-entry 'forward-char desc)))
    (should (equal (plist-get entry :description) desc))
    (should (eq (plist-get entry :command) 'forward-char))))


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

(ert-deftest keymap-popup-test-resolve-key-keeps-current-stored-key ()
  "A stored key that still dispatches to its command is kept as-is."
  (let ((map (make-sparse-keymap))
        (entry (list :key "n" :description "Next" :type 'suffix
                     :command 'next-line)))
    (keymap-set map "n" #'next-line)
    (should (eq (keymap-popup--resolve-key entry map) entry))))

(ert-deftest keymap-popup-test-resolve-key-follows-rebinding ()
  "A rebound command resolves to its current key, not the stored one."
  (let ((map (make-sparse-keymap))
        (entry (list :key "n" :description "Next" :type 'suffix
                     :command 'next-line)))
    (keymap-set map "j" #'next-line)
    (should (equal (plist-get (keymap-popup--resolve-key entry map) :key)
                   "j"))))

(ert-deftest keymap-popup-test-resolve-key-stored-key-fallback ()
  "A stored key survives when the command has no binding in the map."
  (let ((map (make-sparse-keymap))
        (entry (list :key "n" :description "Next" :type 'suffix
                     :command 'next-line)))
    (should (equal (plist-get (keymap-popup--resolve-key entry map) :key)
                   "n"))))

(ert-deftest keymap-popup-test-resolve-key-ignores-global-map ()
  "Resolution never falls back to global bindings (C-n is global next-line)."
  (let ((map (make-sparse-keymap))
        (entry (list :key nil :description "Next" :type 'suffix
                     :command 'next-line)))
    (should-not (keymap-popup--resolve-key entry map))))

(ert-deftest keymap-popup-test-resolve-key-keeps-stored-key-when-inapt ()
  "An inapt entry keeps its stored key.
The menu-item filter reroutes the binding to the stub, so
`where-is-internal' cannot find the command; the stored key must
survive so the entry still renders (dimmed)."
  (let* ((map (make-sparse-keymap))
         (wrapped (eval (keymap-popup--wrap-binding-form
                         '(function next-line) nil (lambda () t))
                        t))
         (entry (list :key "n" :description "Next" :type 'suffix
                      :command 'next-line :inapt-if (lambda () t))))
    (keymap-set map "n" wrapped)
    (should (equal (plist-get (keymap-popup--resolve-key entry map) :key)
                   "n"))))

(ert-deftest keymap-popup-test-resolve-key-no-command-entry-kept ()
  "Entries without :command (e.g. pre-groundwork metadata) pass through."
  (let ((map (make-sparse-keymap))
        (entry (list :key "s" :description "Sub" :type 'keymap
                     :target (make-sparse-keymap))))
    (should (eq (keymap-popup--resolve-key entry map) entry))))

(ert-deftest keymap-popup-test-define-rebound-key-updates-popup ()
  "Rebinding a command after keymap-popup-define updates the shown key."
  (eval '(keymap-popup-define keymap-popup--test-rebind-map
           :group "Nav"
           "n" ("Next" next-line))
        t)
  (keymap-unset keymap-popup--test-rebind-map "n" t)
  (keymap-set keymap-popup--test-rebind-map "j" #'next-line)
  (let* ((resolved (keymap-popup--resolve-descriptions
                    (keymap-popup--collect-descriptions
                     keymap-popup--test-rebind-map)
                    keymap-popup--test-rebind-map))
         (entry (car (plist-get (caar resolved) :entries))))
    (should (equal (plist-get entry :key) "j"))))

(ert-deftest keymap-popup-test-define-rebound-switch-updates-popup ()
  "Rebinding a generated toggle command updates the shown key."
  (eval '(keymap-popup-define keymap-popup--test-rebind-sw-map
           :group "Toggles"
           "v" ("Verbose" :switch keymap-popup--test-rebind-sw-var))
        t)
  (keymap-unset keymap-popup--test-rebind-sw-map "v" t)
  (keymap-set keymap-popup--test-rebind-sw-map "V"
              (keymap-popup--toggle-name 'keymap-popup--test-rebind-sw-map
                                         'keymap-popup--test-rebind-sw-var))
  (let* ((resolved (keymap-popup--resolve-descriptions
                    (keymap-popup--collect-descriptions
                     keymap-popup--test-rebind-sw-map)
                    keymap-popup--test-rebind-sw-map))
         (entry (car (plist-get (caar resolved) :entries))))
    (should (equal (plist-get entry :key) "V"))))

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
  (let* ((descs (keymap-popup--meta keymap-popup--test-annotate-map
                                    'descriptions))
         (entries (plist-get (car (car descs)) :entries)))
    (should (= (length entries) 2))
    (should (eq (plist-get (car entries) :command) 'forward-char))))

(ert-deftest keymap-popup-test-annotate-exit-key ()
  "Annotate with :exit-key sets metadata."
  (setq keymap-popup--test-annotate-map (make-sparse-keymap))
  (keymap-set keymap-popup--test-annotate-map "a" #'forward-char)
  (eval '(keymap-popup-annotate keymap-popup--test-annotate-map
           :exit-key "x"
           :group "Move"
           forward-char "Forward")
        t)
  (should (equal (keymap-popup--meta keymap-popup--test-annotate-map
                                     'exit-key)
                 "x")))

(ert-deftest keymap-popup-test-annotate-popup-key ()
  "Annotate with :popup-key binds the popup command."
  (setq keymap-popup--test-annotate-map (make-sparse-keymap))
  (keymap-set keymap-popup--test-annotate-map "a" #'forward-char)
  (eval '(keymap-popup-annotate keymap-popup--test-annotate-map
           :popup-key "?"
           :group "Move"
           forward-char "Forward")
        t)
  (should (functionp (keymap-lookup keymap-popup--test-annotate-map "?"))))

(ert-deftest keymap-popup-test-annotate-description ()
  "Annotate with :description sets metadata."
  (setq keymap-popup--test-annotate-map (make-sparse-keymap))
  (keymap-set keymap-popup--test-annotate-map "a" #'forward-char)
  (eval '(keymap-popup-annotate keymap-popup--test-annotate-map
           :description "My commands"
           :group "Move"
           forward-char "Forward")
        t)
  (should (equal (keymap-popup--meta keymap-popup--test-annotate-map
                                     'description)
                 "My commands")))

(ert-deftest keymap-popup-test-annotate-no-defaults-baked ()
  "Annotate without keywords sets no exit-key or description metadata."
  (setq keymap-popup--test-annotate-map (make-sparse-keymap))
  (keymap-set keymap-popup--test-annotate-map "a" #'forward-char)
  (eval '(keymap-popup-annotate keymap-popup--test-annotate-map
           :group "Move"
           forward-char "Forward")
        t)
  (should-not (keymap-popup--meta keymap-popup--test-annotate-map
                                  'exit-key))
  (should-not (keymap-popup--meta keymap-popup--test-annotate-map
                                  'description)))

;;; Metadata tests

(ert-deftest keymap-popup-test-meta-read-write ()
  (let ((map (make-sparse-keymap)))
    (setf (keymap-popup--meta map 'descriptions) '(test-data))
    (should (equal (keymap-popup--meta map 'descriptions) '(test-data)))))

(ert-deftest keymap-popup-test-meta-nil-for-missing ()
  (let ((map (make-sparse-keymap)))
    (should (null (keymap-popup--meta map 'descriptions)))))

(ert-deftest keymap-popup-test-attach-meta-stores-all ()
  (let* ((map (make-sparse-keymap))
         (result (keymap-popup--attach-meta map '(rows)
                                            :exit-key "x"
                                            :description "Doc"
                                            :persistent t)))
    (should (eq result map))
    (should (equal (keymap-popup--meta map 'descriptions) '(rows)))
    (should (equal (keymap-popup--meta map 'exit-key) "x"))
    (should (equal (keymap-popup--meta map 'description) "Doc"))
    (should (eq (keymap-popup--meta map 'persistent) 'yes))))

(ert-deftest keymap-popup-test-attach-meta-omits-nil-opts ()
  (let ((map (make-sparse-keymap)))
    (keymap-popup--attach-meta map '(rows))
    (should (equal (keymap-popup--meta map 'descriptions) '(rows)))
    (should-not (keymap-popup--meta map 'exit-key))
    (should-not (keymap-popup--meta map 'description))
    (should-not (keymap-popup--meta map 'persistent))))

(ert-deftest keymap-popup-test-attach-meta-ignores-unknown-opts ()
  (let ((map (make-sparse-keymap)))
    (keymap-popup--attach-meta map '(rows) :popup-key "?")
    (should (equal (keymap-popup--meta map 'descriptions) '(rows)))
    (should-not (keymap-popup--meta map 'popup-key))))

(ert-deftest keymap-popup-test-no-descriptions-error ()
  (let ((map (make-sparse-keymap)))
    (should-error (keymap-popup map) :type 'user-error)))

(ert-deftest keymap-popup-test-dismiss-is-command ()
  (should (commandp #'keymap-popup-dismiss)))

(ert-deftest keymap-popup-test-on-exit-tears-down-for-suffix ()
  "On-exit tears down when a suffix (non-exit-key) caused the exit."
  (let ((buf (get-buffer-create "*keymap-popup-test-exit*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local keymap-popup--active-exit-key "q")
            (setq-local keymap-popup--stack '((:keymap nil :descriptions nil
                                                       :docstring nil :exit-key "q")))
            (setq-local keymap-popup--display-backend
                        (list :show #'ignore :fit #'ignore :hide #'ignore)))
          (let ((on-exit (keymap-popup--make-on-exit buf)))
            (cl-letf (((symbol-function 'this-command-keys-vector)
                       (lambda () [?a])))
              (funcall on-exit))
            (should-not (buffer-live-p buf))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest keymap-popup-test-on-exit-pops-for-exit-key ()
  "On-exit pops to parent when exit-key caused the exit."
  (let ((buf (get-buffer-create "*keymap-popup-test-pop*"))
        (parent-map (make-sparse-keymap))
        (descs '(((:name nil :entries nil)))))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local keymap-popup--active-exit-key "q")
            (setq-local keymap-popup--active-keymap (make-sparse-keymap))
            (setq-local keymap-popup--active-descriptions descs)
            (setq-local keymap-popup--active-docstring nil)
            (setq-local keymap-popup--source-buffer buf)
            (setq-local keymap-popup--stack
                        (list (list :keymap parent-map
                                    :descriptions descs
                                    :docstring nil
                                    :exit-key "x")))
            (setq-local keymap-popup--display-backend
                        (list :show #'ignore :fit #'ignore :hide #'ignore)))
          (let ((on-exit (keymap-popup--make-on-exit buf)))
            (cl-letf (((symbol-function 'this-command-keys-vector)
                       (lambda () [?q])))
              (funcall on-exit))
            (should (buffer-live-p buf))
            (with-current-buffer buf
              (should (eq keymap-popup--active-keymap parent-map))
              (should (equal keymap-popup--active-exit-key "x"))
              (should-not keymap-popup--stack))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(provide 'keymap-popup-tests)
;;; keymap-popup-tests.el ends here
