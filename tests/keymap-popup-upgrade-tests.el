;;; keymap-popup-upgrade-tests.el --- Retained map upgrades -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Exercise maps in the metadata format used before inert menu items.
;; The fixture changes only storage: public declarations still create the maps.

;;; Code:

(require 'ert)
(require 'keymap-popup)
(require 'keymap-popup-declarations-tests)

(defvar keymap-popup-upgrade--map)

(defun keymap-popup-upgrade--legacy-set (map prop value)
  "Store VALUE for PROP in MAP using the prior released representation."
  (define-key map (vector 'keymap-popup prop) value))

(defmacro keymap-popup-upgrade--legacy (&rest body)
  "Evaluate public declarations in BODY using legacy metadata storage."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'keymap-popup--set-meta)
              #'keymap-popup-upgrade--legacy-set))
     ,@body))

(ert-deftest keymap-popup-upgrade-annotate-options-and-update ()
  (keymap-popup-declarations-test--with-popup
    (let ((keymap-popup-upgrade--map (make-sparse-keymap)))
      (keymap-set keymap-popup-upgrade--map "a" #'forward-char)
      (keymap-popup-upgrade--legacy
        (keymap-popup-annotate keymap-popup-upgrade--map
          :popup-key "?" :exit-key "x" :persistent t :description "Old title"
          forward-char "Old action"))
      (let ((original keymap-popup-upgrade--map))
        (should (equal (keymap-popup--meta original 'exit-key) "x"))
        (should (eq (keymap-popup--meta original 'persistent) 'yes))
        (should (equal (keymap-popup--meta original 'description) "Old title"))
        (should (string-match-p "Old action"
                                (keymap-popup-declarations-test--text original)))
        (should-not (string-match-p
                     "<keymap-popup>\\|Keyboard Macro\\|:entries"
                     (substitute-command-keys "\\{keymap-popup-upgrade--map}")))
        (keymap-set original "a" nil)
        (keymap-set original "b" #'forward-char)
        (should (string-match-p "Old action"
                                (keymap-popup-declarations-test--text original)))
        (keymap-popup-annotate keymap-popup-upgrade--map
          :popup-key "?" :exit-key "z" :persistent nil forward-char "Updated")
        (should (eq keymap-popup-upgrade--map original))
        (should (eq (keymap-lookup original "b") #'forward-char))
        (should (equal (keymap-popup--meta original 'exit-key) "z"))
        (should (eq (keymap-popup--meta original 'persistent) 'no))
        (should-not (keymap-popup--meta original 'description))))))

(ert-deftest keymap-popup-upgrade-define-identity-and-customization ()
  (keymap-popup-declarations-test--with-popup
    (let ((form '(keymap-popup-define keymap-popup-upgrade--map
                   :popup-key "?" :exit-key "x" :persistent t
                   "a" ("Anonymous" (lambda () (interactive) (forward-char)))
                   "b" ("Replaceable" (lambda () (interactive) (backward-char))))))
      (makunbound 'keymap-popup-upgrade--map)
      (unwind-protect
          (progn
            (keymap-popup-upgrade--legacy (eval form t))
            (let* ((original keymap-popup-upgrade--map)
                   (command (keymap-lookup original "a"))
                   (replaced (keymap-lookup original "b"))
                   (legacy-copy (copy-keymap original)))
              (should (eq command (keymap-lookup legacy-copy "a")))
              (should (string-match-p
                       "Anonymous" (keymap-popup-declarations-test--text legacy-copy)))
              (insert "abc")
              (goto-char (point-min))
              (call-interactively (keymap-lookup legacy-copy "a"))
              (should (= (point) 2))
              (keymap-set original "b" #'ignore)
              (eval form t)
              (should (eq original keymap-popup-upgrade--map))
              (should (eq command (keymap-lookup original "a")))
              (should (eq command (keymap-popup--declared-command original "a")))
              (should (eq replaced (keymap-popup--declared-command original "b")))
              (should (eq (keymap-lookup original "b") #'ignore))
              (let ((text (keymap-popup-declarations-test--text original)))
                (should (string-match-p "Anonymous" text))
                (should-not (string-match-p "Replaceable" text)))
              (let ((copy (copy-keymap original)))
                (should (eq command (keymap-lookup copy "a")))
                (should (string-match-p "Anonymous"
                                        (keymap-popup-declarations-test--text copy))))))
        (makunbound 'keymap-popup-upgrade--map)))))

(ert-deftest keymap-popup-upgrade-inherited-composed-and-copied ()
  (keymap-popup-declarations-test--with-popup
    (let ((parent (make-sparse-keymap))
          (child (make-sparse-keymap))
          (other (make-sparse-keymap)))
      (keymap-set parent "a" #'forward-char)
      (keymap-set other "b" #'backward-char)
      (keymap-popup-upgrade--legacy
        (keymap-popup-attach parent '("a" ("Parent" forward-char))
                             :exit-key "x" :persistent t :description "Inherited")
        (keymap-popup-attach other '("b" ("Other" backward-char))
                             :persistent nil))
      (set-keymap-parent child parent)
      ;; Copy before the first metadata read, as well as after conversion.
      (dolist (map (list child (copy-keymap child)
                        (make-composed-keymap (list other child))))
        (let ((text (keymap-popup-declarations-test--text map)))
          (should (string-match-p "Parent" text)))
        (should (equal (keymap-popup--meta map 'exit-key) "x"))
        (should (equal (keymap-popup--meta map 'description) "Inherited")))
      (should (eq (keymap-popup--meta
                   (make-composed-keymap (list other child)) 'persistent) 'no))
      (let ((copy (copy-keymap parent)))
        (keymap-popup-add-entry copy "c" "Copy only" #'ignore)
        (should-not (keymap-lookup parent "c"))
        (should-not (string-match-p "Copy only"
                                    (keymap-popup-declarations-test--text parent)))))))

(ert-deftest keymap-popup-upgrade-runtime-add-remove-and-replace ()
  (keymap-popup-declarations-test--with-popup
    (let ((map (make-sparse-keymap)))
      (keymap-set map "a" #'forward-char)
      (keymap-popup-upgrade--legacy
        (keymap-popup-attach map '("a" ("Old action" forward-char))
                             :exit-key "x" :persistent nil :description "Old"))
      (keymap-popup-add-entry map "b" "Added" #'backward-char)
      (should (equal (keymap-popup--meta map 'exit-key) "x"))
      (should (eq (keymap-popup--meta map 'persistent) 'no))
      (keymap-popup-remove-entry map "a")
      (should-not (keymap-lookup map "a"))
      (should (string-match-p "Added" (keymap-popup-declarations-test--text map)))
      (should (eq (keymap-popup-attach map '("b" ("Replacement" backward-char))) map))
      (should-not (keymap-popup--meta map 'exit-key))
      (should-not (keymap-popup--meta map 'persistent))
      (should-not (keymap-popup--meta map 'description)))))

(provide 'keymap-popup-upgrade-tests)
;;; keymap-popup-upgrade-tests.el ends here
