;;; gnosis-test-source-choice.el --- Optional source selection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

(require 'ert)
(require 'gnosis-review-test-support)
(require 'gnosis-lecture)

(ert-deftest gnosis-source-sole-deduplicated-lecture-opens-directly ()
  (gnosis-test-with-db
    (let* ((file (expand-file-name "lecture.pdf" gnosis-dir))
           (citation (gnosis-lecture--citation file 2))
           (buffer (generate-new-buffer " *source*"))
           opened)
      (unwind-protect
          (progn
            (gnosis-add-thema-fields "basic" "Question" nil '("Answer")
                                    (concat citation "\n" citation) nil 0 nil nil 101)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) (ert-fail "Sole source prompted")))
                      ((symbol-function 'gnosis-lecture-open)
                       (lambda (path &optional _argument validate)
                         (funcall validate)
                         (setq opened (gnosis-lecture--target path))
                         (switch-to-buffer buffer))))
              (gnosis-view-linked-node 101 #'ignore))
            (should (equal opened (list file 2)))
            (should-not (gnosis-select '* 'review-events))
            (should-not (gnosis-select '* 'practice-events)))
        (kill-buffer buffer)))))

(ert-deftest gnosis-source-collision-and-empty-selection ()
  (gnosis-test-with-db
    (let* ((file (expand-file-name "lecture.pdf" gnosis-dir))
           (citation (gnosis-lecture--citation file 2))
           (label (caar (gnosis-lecture-sources citation)))
           (buffer (generate-new-buffer " *source*"))
           opened)
      (unwind-protect
          (progn
            (gnosis-add-thema-fields "basic" "Question" nil '("Answer") citation
                                    nil 0 '("source") nil 101)
            (cl-letf (((symbol-function 'gnosis-study-topic-candidates)
                       (lambda (&rest _) (list (cons label "source"))))
                      ((symbol-function 'completing-read)
                       (lambda (_prompt candidates &rest _)
                         (should (= 2 (length (delete-dups (mapcar #'car candidates)))))
                         (caar candidates)))
                      ((symbol-function 'gnosis-nodes-goto-id)
                       (lambda (id) (setq opened id) (switch-to-buffer buffer))))
              (gnosis-view-linked-node 101)
              (should (equal opened "source"))
              (setq opened nil)
              (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
                (should-error (gnosis-view-linked-node 101) :type 'user-error))
              (should-not opened)))
        (kill-buffer buffer)))))

(ert-deftest gnosis-source-picker-retirement-refuses-navigation ()
  (gnosis-test-with-db
    (gnosis-test-content--add "basic")
    (gnosis-update 'extras '(= parathema "[[gnosis-lecture:2:%2Fmissing.pdf][PDF]]") '(= id 222))
    (gnosis--insert-into 'thema-links '([222 "source"]))
    (dolist (mode '(due practice))
      (let* ((gnosis-review-buffer-name "*Source Ownership*")
             (owner (gnosis-review--setup-buffer '(222) mode)))
        (unwind-protect
            (with-current-buffer owner
              (gnosis-test-content--state mode)
              (let ((answer (gnosis-test-content--answer "basic"))
                    (before (gnosis-test-content--evidence))
                    opened)
                (cl-letf (((symbol-function 'gnosis-study-topic-candidates)
                           (lambda (&rest _) '(("Node" . "source"))))
                          ((symbol-function 'gnosis-get-linked-nodes) (lambda (&rest _) '("Node")))
                          ((symbol-function 'completing-read)
                           (lambda (&rest _)
                             (with-current-buffer owner (fundamental-mode))
                             "Node"))
                          ((symbol-function 'gnosis-nodes-goto-id)
                           (lambda (&rest _) (setq opened t))))
                  (should-error (gnosis-review-action--view-link t 222 (cdr answer)) :type 'user-error))
                (should-not opened)
                (should (equal before (gnosis-test-content--evidence)))))
          (kill-buffer owner))))))

(provide 'gnosis-test-source-choice)
;;; gnosis-test-source-choice.el ends here
