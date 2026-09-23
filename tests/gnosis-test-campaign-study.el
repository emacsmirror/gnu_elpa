;;; gnosis-test-campaign-study.el --- Study boundary regressions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Native collection rendering, batched selection and source-view lifetimes.

;;; Code:
(require 'gnosis-test-study)
(require 'gnosis-test-agent)
(require 'gnosis-test-review-session-owner)

(ert-deftest gnosis-campaign-study-retired-rendering ()
  (dolist (kind '(topic repair))
    (dolist (detach '(nil t))
      (gnosis-test-study
        (gnosis-test-study-view-fixture "Displayed")
        (gnosis-test-study-open-view kind)
        (let ((evidence (gnosis-test-study-view-snapshot)))
          (gnosis-test-study-view-successor detach)
          (let ((inhibit-read-only t))
            (put-text-property (point-min) (point-max) 'face 'warning))
          (let ((text (buffer-string)) (file buffer-file-name)
                (modified (buffer-modified-p)))
            (dolist (render (list (lambda () (let ((current-prefix-arg 1))
                                              (call-interactively #'tabulated-list-sort)))
                                 (lambda () (tabulated-list-print t))
                                 (lambda ()
                                   (tabulated-list-col-sort
                                    (list 'mouse-1
                                          (list (selected-window) 'header-line
                                                '(0 . 0) 0
                                                (cons (propertize "Question"
                                                                  'tabulated-list-column-name "Question")
                                                      0)))))))
              (should-error (funcall render) :type 'user-error)
              (should (equal-including-properties text (buffer-string)))
              (should (equal file buffer-file-name))
              (should (eq modified (buffer-modified-p)))))
          (should (equal evidence (gnosis-test-study-view-snapshot))))))))

(ert-deftest gnosis-campaign-study-foreign-render-and-refresh ()
  (dolist (kind '(topic repair))
    (gnosis-test-study
      (gnosis-test-study-view-fixture "Original")
      (gnosis-test-study-open-view kind)
      (tabulated-list-sort 1)
      (should (= 42 (tabulated-list-get-id)))
      (let ((text (buffer-string)))
        (gnosis-test-with-db
          (gnosis-test-study-view-fixture "Adopted")
          (should-error (tabulated-list-print t) :type 'user-error)
          (should (equal-including-properties text (buffer-string)))
          (call-interactively (key-binding (kbd "g")))
          (tabulated-list-col-sort "Question")
          (should (string-match-p "Adopted" (buffer-string)))
          (should (gnosis-study--check-owner)))))))

(ert-deftest gnosis-campaign-study-agent-batched-selection ()
  (dolist (topic '(nil t))
    (dolist (size '(32 256))
      (dolist (budget '(16 32766))
        (gnosis-test-agent
          (gnosis--insert-into 'nodes '(["topic" "topic.org" "Topic" "1" nil nil nil]))
          (gnosis-sqlite-with-transaction gnosis-db
            (dotimes (i size)
              (gnosis-test--add-basic-thema "Q" "A" nil nil (1+ i))
              (gnosis--insert-into 'thema-links (list (vector (1+ i) "topic")))))
          (gnosis-toggle-suspend-themata '(2) 1 t)
          (gnosis-update 'scheduler-state '(= due-day 29990101) '(= thema-id 3))
          (let ((select (symbol-function 'sqlite-select)) (calls 0) status)
            (cl-letf (((symbol-function 'sqlite-select)
                       (lambda (&rest args)
                         (cl-incf calls)
                         (should (<= (length (nth 2 args)) budget))
                         (apply select args)))
                      ((symbol-function 'gnosis-sqlite--max-variable-number)
                       (lambda (_) budget)))
              (setq status (if topic
                               (gnosis-agent-start-practice :topic-ids '("topic" "topic") :limit 1)
                             (gnosis-agent-start-practice
                              :thema-ids (append (reverse (number-sequence 1 size)) '(1 999)) :limit 1))))
            (let ((selection (plist-get status :selection)))
              (should (equal [1] (plist-get status :selected-ids)))
              (should (= (plist-get selection :candidates) (+ size (if topic 0 1))))
              (should (= (plist-get selection :eligible) (1- size)))
              (should (= (plist-get selection :selected) 1))
              (should (= (plist-get selection :omitted-by-limit) (- size 2)))
              (should (= (plist-get selection :shortfall) 0))
              (should (equal (plist-get selection :excluded-ids) (if topic [2] [2 999]))))
            (message "Batched selection topic=%S size=%d budget=%d reads=%d" topic size budget calls)
            (should (< calls (+ 20 (ceiling (1+ size) budget))))))))))

(ert-deftest gnosis-campaign-study-source-header-return ()
  (dolist (local '(nil t))
    (with-temp-buffer
      (let ((original (default-value 'header-line-format)))
        (when local (setq-local header-line-format "Original λ")
              (setq original header-line-format))
        (gnosis-link-view-mode 1)
        (gnosis-link-view-mode -1)
        (should (eq local (local-variable-p 'header-line-format)))
        (should (equal original header-line-format))))))

(ert-deftest gnosis-campaign-study-source-entry-failure ()
  (gnosis-test-study
    (gnosis-test-active-owner--seed 'practice)
    (let* ((gnosis-nodes-dir gnosis-dir)
           (org-id-track-globally nil)
           (file (expand-file-name "source.org" gnosis-dir))
           (gnosis-review-buffer-name "*Entry failure*")
           (owner (gnosis-review--setup-buffer '(222) 'practice))
           source answer)
      (with-temp-file file (insert "#+title: Source\n* Source\n:PROPERTIES:\n:ID: source\n:END:\nPassage\n"))
      (gnosis-nodes-update-file file)
      (gnosis--insert-into 'thema-links '([222 "source"]))
      (setq source (find-file-noselect file))
      (with-current-buffer source (setq-local header-line-format "Original"))
      (with-current-buffer owner
        (gnosis-test-content--state 'practice)
        (setq answer (gnosis-test-content--answer "basic"))
        (let ((gnosis-link-view-mode-hook
               (list (lambda () (when gnosis-link-view-mode
                                  (with-current-buffer owner (fundamental-mode)))))))
          (should-error (gnosis-review-action--view-link t 222 (cdr answer)) :type 'user-error)))
      (with-current-buffer source
        (should-not gnosis-link-view-mode)
        (should (equal "Original" header-line-format))))))

(ert-deftest gnosis-campaign-study-source-successor-cleanup ()
  (with-temp-buffer
    (setq-local header-line-format "Original")
    (gnosis-link-view-mode 1)
    (let ((old gnosis-link-view--owner))
      (setq-local header-line-format "Successor header")
      (gnosis-link-view-mode 1)
      (let ((new gnosis-link-view--owner) (header header-line-format))
        (gnosis-link-view--cleanup old)
        (should gnosis-link-view-mode)
        (should (eq new gnosis-link-view--owner))
        (should (eq header header-line-format))
        (gnosis-link-view--cleanup new)
        (should-not gnosis-link-view-mode)
        (should (equal "Successor header" header-line-format)))))
  (with-temp-buffer
    (gnosis-link-view-mode 1)
    (setq-local header-line-format "Changed while viewing")
    (gnosis-link-view-mode -1)
    (should (equal "Changed while viewing" header-line-format))))

(provide 'gnosis-test-campaign-study)
;;; gnosis-test-campaign-study.el ends here
