;;; gnosis-test-boundaries.el --- Cold dependency boundaries -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defun gnosis-test-boundaries--cold (form)
  "Evaluate FORM in a fresh Emacs with disposable user and Gnosis data."
  (let* ((dir (make-temp-file "gnosis-cold-" t))
         (process-environment (copy-sequence process-environment))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (progn
          (dolist (name '("HOME" "XDG_CACHE_HOME" "XDG_CONFIG_HOME"
                          "XDG_DATA_HOME" "XDG_STATE_HOME"))
            (let ((path (expand-file-name name dir)))
              (make-directory path)
              (setenv name path)))
          (with-temp-buffer
            (let* ((print-length nil)
                   (print-level nil)
                   (setup `(setq load-path ',load-path load-prefer-newer t
                                 gnosis-dir ,(file-name-as-directory dir)
                                 gnosis-testing t gnosis-vc-auto-push nil))
                   (status (call-process emacs nil t nil "-Q" "--batch"
                                         "--eval" (prin1-to-string setup)
                                         "--eval" (prin1-to-string form))))
              (ert-info ((buffer-string)) (should (equal status 0))))))
      (delete-directory dir t))))

(ert-deftest gnosis-test-boundaries-cold-graph-owner ()
  "Traverse actual links after requiring their owner, without commands or views."
  (gnosis-test-boundaries--cold
   '(progn
      (require 'gnosis-links)
      (unless (fboundp 'gnosis-collect-nodes-at-depth)
        (error "Graph traversal is unavailable below review"))
      (unwind-protect
          (progn
            (gnosis--insert-into 'nodes
                                 '(["a" "test.org" "A" "1" nil nil nil]
                                   ["b" "test.org" "B" "1" nil nil nil]))
            (gnosis--insert-into 'node-links '(["a" "b"] ["b" "a"]))
            (unless (equal '("b" "a") (gnosis-collect-nodes-at-depth "a" 20 20))
              (error "Cold graph traversal failed")))
        (when gnosis-db (gnosis-sqlite-close gnosis-db)))
      (dolist (feature '(gnosis gnosis-review gnosis-dashboard gnosis-study))
        (when (featurep feature) (error "Graph loaded %s" feature))))))

(ert-deftest gnosis-test-boundaries-cold-study-selection ()
  "Select real linked themata without loading the review or dashboard view."
  (gnosis-test-boundaries--cold
   '(progn
      (require 'gnosis-study)
      (require 'gnosis-test-helpers)
      (gnosis-test-with-db
        (let ((id (gnosis-test--add-basic-thema "Q" "A")))
          (gnosis--insert-into 'nodes
                               '(["a" "test.org" "A" "1" nil nil nil]
                                 ["b" "test.org" "B" "1" nil nil nil]))
          (gnosis--insert-into 'node-links '(["a" "b"]))
          (gnosis--insert-into 'thema-links `([,id "b"]))
          (unless (equal (list id) (gnosis-study-topic-ids '("a") nil 1 0))
            (error "Cold topic selection failed"))))
      (dolist (feature '(gnosis-review gnosis-dashboard))
        (when (featurep feature) (error "Selection loaded %s" feature))))))

(ert-deftest gnosis-test-boundaries-cold-cloze-transform ()
  "Transform clozes without main, storage, Org, or temporary rendering buffers."
  (gnosis-test-boundaries--cold
   '(progn
      (require 'gnosis-cloze)
      (unless (fboundp 'gnosis-cloze--replace)
        (error "No independent cloze transform"))
      (let ((buffers (buffer-list)))
        (unless (equal "*α* (?) β" (gnosis-cloze--replace "*α* β β" '("β") "(?)"))
          (error "Cold cloze transformation failed"))
        (unless (equal buffers (buffer-list)) (error "Cloze created buffers")))
      (dolist (feature '(gnosis gnosis-db gnosis-sqlite gnosis-org org gnosis-review))
        (when (featurep feature) (error "Cloze loaded %s" feature)))
      ;; Only the isolated user directories created by the harness may exist.
      (unless (equal '("HOME" "XDG_CACHE_HOME" "XDG_CONFIG_HOME"
                      "XDG_DATA_HOME" "XDG_STATE_HOME")
                     (directory-files gnosis-dir nil "\\`[^.]"))
        (error "Cloze wrote data")))))

(ert-deftest gnosis-test-boundaries-cold-dashboard-compilation ()
  "Compile the dashboard without preloading review or study commands."
  (gnosis-test-boundaries--cold
   '(progn
      (require 'bytecomp)
      (let* ((source (locate-file "gnosis-dashboard.el" load-path))
             (target (expand-file-name "gnosis-dashboard.el" gnosis-dir))
             ;; Popup-generated long docstrings have their own lint policy.
             (byte-compile-warnings '(not docstrings))
             (byte-compile-error-on-warn t))
        (copy-file source target)
        (unless (byte-compile-file target)
          (error "Cold dashboard compilation failed"))))))

(ert-deftest gnosis-test-boundaries-cold-dashboard-review-depth-cancel ()
  "Reach depth input from a cold dashboard and cancel without study evidence."
  (gnosis-test-boundaries--cold
   '(progn
      (require 'gnosis-dashboard)
      (when (featurep 'gnosis-review)
        (error "Dashboard eagerly loaded review"))
      (let ((prompted nil))
        (cl-letf (((symbol-function 'read-number)
                   (lambda (&rest _args)
                     (setq prompted t)
                     (signal 'quit nil))))
          (with-temp-buffer
            (gnosis-dashboard-nodes-mode)
            (condition-case nil
                (call-interactively (key-binding (kbd "R")))
              (quit nil))))
        (unless prompted (error "Depth command did not reach its prompt"))
        (unless (and (null (gnosis-select '* 'study-session))
                     (null (gnosis-select '* 'review-events)))
          (error "Cancelled selection wrote study evidence"))))))

(provide 'gnosis-test-boundaries)
;;; gnosis-test-boundaries.el ends here
