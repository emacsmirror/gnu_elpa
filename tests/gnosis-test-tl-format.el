;;; gnosis-test-tl-format.el --- Row formatting parity tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Compare visible cells across full, incremental and replacement rendering.

;;; Code:

(require 'ert)
(require 'gnosis-tl)
(require 'gnosis-test-helpers)
(require 'gnosis-dashboard)

(defun gnosis-test-tl-format--snapshot ()
  "Return row text, tail display column and per-character semantic properties."
  (list (buffer-substring-no-properties (point-min) (point-max))
        (save-excursion
          (goto-char (point-min))
          (search-forward "TAIL")
          (current-column))
        (cl-loop for pos from (point-min) below (point-max)
                 ;; C `format' can extend a cell's face into its padding;
                 ;; compare content faces, not generated blank decoration.
                 collect (mapcar (lambda (property)
                                   (unless (and (eq (char-after pos) ?\s)
                                                (memq property '(face help-echo)))
                                     (get-text-property pos property)))
                                 '(tabulated-list-id tabulated-list-entry
                                   face help-echo)))))

(ert-deftest gnosis-test-tl-format-render-path-parity ()
  "Full, appended and replaced cells have identical text and geometry."
  (dolist (text '("abc" "abcdefghijk" "界界界界界界" "éééééé"
                  "a\tb" "a\001b" "a\177b" ""))
    (dolist (width '(4 5 10))
      (dolist (padding '(0 2))
        (dolist (pad-right '(0 3))
          (dolist (right-align '(nil t))
            (dolist (gnosis-tl-ellipsis '("..." "…" ""))
              (ert-info ((format "%S width=%s padding=%s pad-right=%s right=%s ellipsis=%S"
                                 text width padding pad-right right-align
                                 gnosis-tl-ellipsis))
                (with-temp-buffer
                  (tabulated-list-mode)
                  (setq tabulated-list-format
                        (vector (list "Value" width t :pad-right pad-right
                                      :right-align right-align)
                                '("Tail" 4 t))
                        tabulated-list-padding padding)
                  (let* ((cols (vector (propertize text 'face 'warning
                                                  'help-echo "Full value")
                                       "TAIL"))
                         (original (copy-sequence (aref cols 0)))
                         (entry (list 91 cols)))
                    (setq tabulated-list-entries (list entry))
                    (gnosis-tl-print)
                    (let ((full (gnosis-test-tl-format--snapshot)))
                      (gnosis-tl-replace-entry 91 cols)
                      (should (equal full (gnosis-test-tl-format--snapshot)))
                      ;; Column-name properties remain a replacement-only aid;
                      ;; bulk rendering deliberately avoids those intervals.
                      (should (equal "Tail" (get-text-property
                                             (- (point-max) 2)
                                             'tabulated-list-column-name)))
                      (let ((inhibit-read-only t)) (erase-buffer))
                      (gnosis-tl-append-entries (list entry))
                      (should (equal full (gnosis-test-tl-format--snapshot)))
                      (should-not (get-text-property
                                   (- (point-max) 2) 'tabulated-list-column-name))
                      (should (equal-including-properties original
                                                          (aref cols 0))))))))))))))

(ert-deftest gnosis-test-tl-format-last-column-parity ()
  "The last cell retains its unpadded, truncated contract on all paths."
  (dolist (text '("abc" "abcdefghijk" "界界界界" "éééé" "a\tb" "a\001b"))
    (with-temp-buffer
      (tabulated-list-mode)
      (setq tabulated-list-format [("Last" 4 t :right-align t :pad-right 3)]
            tabulated-list-padding 2
            tabulated-list-entries (list (list 91 (vector text))))
      (gnosis-tl-print)
      (let ((full (buffer-substring-no-properties (point-min) (point-max))))
        (gnosis-tl-replace-entry 91 (vector text))
        (should (equal full (buffer-substring-no-properties (point-min) (point-max))))
        (let ((inhibit-read-only t)) (erase-buffer))
        (gnosis-tl-append-entries tabulated-list-entries)
        (should (equal full (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-deftest gnosis-test-tl-format-visible-column-boundaries ()
  "Wide truncation fills the gap and ASCII right alignment is honored."
  (dolist (case '(("界界界界" nil "...  TAIL\n" 4)
                  ("2" t "    2 TAIL\n" 5)))
    (with-temp-buffer
      (tabulated-list-mode)
      (setq tabulated-list-format
            (vector (list "Value" (nth 3 case) t :right-align (cadr case))
                    '("Tail" 4 t))
            tabulated-list-padding 0
            tabulated-list-entries (list (list 91 (vector (car case) "TAIL"))))
      (gnosis-tl-print)
      (should (equal (nth 2 case) (buffer-substring-no-properties (point-min) (point-max))))
      (search-forward "TAIL")
      (should (= (+ (nth 3 case) 5) (current-column))))))

(ert-deftest gnosis-test-tl-format-dashboard-suspend ()
  "Suspension leaves unchanged visible columns aligned with adjacent rows."
  (gnosis-test-with-db
    (let ((gnosis-dashboard-buffer-name "*Gnosis formatting test*")
          (gnosis-dashboard-render-chunk-size 100)
          (gnosis-dashboard--history nil)
          (gnosis-tl-ellipsis "..."))
      (unwind-protect
          (save-window-excursion
            (dolist (id '(9071 9072))
              (gnosis-add-thema-fields "basic" (make-string 40 ?界)
                                       '("HINT") '("ANSWER") "" '("test")
                                       0 nil nil id))
            (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 80)))
              (gnosis-dashboard-output-themata '(9071 9072)))
            (with-current-buffer gnosis-dashboard-buffer-name
              (goto-char (point-min))
              (let ((before (save-excursion (search-forward "HINT")
                                           (current-column)))
                    (prefix (save-excursion
                              (search-forward "basic")
                              (buffer-substring-no-properties (point-min) (point)))))
                ;; Exercise the actual collection binding, not a formatter.
                (should (eq (key-binding (kbd "s")) 'gnosis-dashboard-suspend-thema))
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                  (call-interactively (key-binding (kbd "s"))))
                (should (gnosis-suspended-p 9071))
                (should-not (gnosis-suspended-p 9072))
                (goto-char (point-min))
                (search-forward "HINT")
                (should (= before (current-column)))
                (search-forward "basic")
                (should (equal prefix (buffer-substring-no-properties
                                       (point-min) (point))))
                (forward-line 1)
                (search-forward "HINT")
                (should (= before (current-column))))))
        (when-let* ((buffer (get-buffer gnosis-dashboard-buffer-name)))
          (kill-buffer buffer))))))

(ert-deftest gnosis-test-tl-format-dashboard-edit-and-link ()
  "Save and bulk-link refreshes keep unchanged cells at their bulk positions."
  (dolist (operation '(save bulk-link))
    (gnosis-test-with-db
      (let ((gnosis-dashboard-buffer-name "*Gnosis formatting update test*")
            (gnosis-dashboard-render-chunk-size 100)
            (gnosis-dashboard--history nil)
            (gnosis-tl-ellipsis "...")
            (gnosis-nodes-dir gnosis-dir)
            (gnosis-journal-dir (expand-file-name "journal" gnosis-dir))
            (org-id-track-globally nil)
            (register-alist nil))
        (unwind-protect
            (save-window-excursion
              (gnosis-add-thema-fields "basic" (make-string 40 ?界)
                                       '("HINT") '("ANSWER") "" '("test")
                                       0 nil nil 9071)
              (let ((file (expand-file-name "source.org" gnosis-dir)))
                (with-temp-file file
                  (insert "#+title: Source\n* Source\n:PROPERTIES:\n"
                          ":ID: formatting-source\n:END:\n"))
                (gnosis-nodes-update-file file))
              (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 80)))
                (gnosis-dashboard-output-themata '(9071)))
              (let ((before (save-excursion (search-forward "HINT")
                                           (current-column))))
                (pcase operation
                  ('save
                   (gnosis-edit-thema 9071)
                   (goto-char (point-min))
                   (search-forward "** Parathema")
                   (forward-line 1)
                   (insert "Updated explanation\n")
                   (call-interactively (key-binding (kbd "C-c C-c")))
                   (should (equal "Updated explanation"
                                  (gnosis-get 'parathema 'extras '(= id 9071)))))
                  ('bulk-link
                   (cl-letf (((symbol-function 'read-string)
                              (lambda (&rest _) "界"))
                             ((symbol-function 'gnosis-completing-read)
                              (lambda (&rest _) "Source"))
                             ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                     (call-interactively (key-binding (kbd "b"))))
                   (should (string-match-p
                            (regexp-quote "[[id:formatting-source][界]]")
                            (gnosis-get 'keimenon 'themata '(= id 9071))))))
                (should (equal (buffer-name) gnosis-dashboard-buffer-name))
                (goto-char (point-min))
                (search-forward "HINT")
                (should (= before (current-column)))))
          (dolist (name (list gnosis-dashboard-buffer-name "*Gnosis Edit*"))
            (when-let* ((buffer (get-buffer name)))
              (kill-buffer buffer))))))))

(provide 'gnosis-test-tl-format)
;;; gnosis-test-tl-format.el ends here
