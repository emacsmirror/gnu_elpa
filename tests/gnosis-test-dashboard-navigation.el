;;; gnosis-test-dashboard-navigation.el --- Collection identity tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:
;; Exercise retained collection membership and identity through native bindings.

;;; Code:

(require 'gnosis-test-dashboard-owner)

(defun gnosis-test-dashboard-navigation--node (id title &optional target)
  "Write and index a disposable node ID with TITLE, optionally linking TARGET."
  (let ((file (expand-file-name (concat id ".org") gnosis-nodes-dir)))
    (make-directory gnosis-nodes-dir t)
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: %s\n#+filetags: :tag:\nNeedle\n"
                      id title))
      (when target (insert (format "[[id:%s][Target]]\n" target))))
    (gnosis-nodes--update-file file nil)))

(ert-deftest gnosis-dashboard-navigation-stale-tag-ret ()
  "Stale tag RET cannot adopt the successor database before a later deletion."
  (gnosis-test-dashboard-owner--with-databases
    (gnosis-dashboard-output-tags '("tag"))
    (let ((before (gnosis-test-dashboard-owner--view))
          (a (gnosis-test-dashboard-owner--snapshot original-db))
          (b (gnosis-test-dashboard-owner--snapshot successor-db)))
      (setq gnosis-db successor-db gnosis-dir successor-dir)
      (should-error (call-interactively (local-key-binding (kbd "RET")))
                    :type 'user-error)
      (should (equal before (gnosis-test-dashboard-owner--view)))
      (should (equal a (gnosis-test-dashboard-owner--snapshot original-db)))
      (should (equal b (gnosis-test-dashboard-owner--snapshot successor-db)))
      ;; A deliberate refresh, unlike stale RET, may adopt the new authority.
      (call-interactively (local-key-binding (kbd "g")))
      (gnosis-dashboard--goto-id "tag")
      (call-interactively (local-key-binding (kbd "RET")))
      (should (eq gnosis-dashboard--database successor-db))
      (should (string-match-p "Successor" (buffer-string)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (call-interactively (local-key-binding (kbd "d"))))
      (should-not (gnosis-sqlite-select successor-db "SELECT id FROM themata WHERE id=42"))
      (should (equal a (gnosis-test-dashboard-owner--snapshot original-db))))))

(ert-deftest gnosis-dashboard-navigation-stale-filters-and-history ()
  "Filters, row navigation and Back refuse stale IDs before any lookup or input."
  (gnosis-test-dashboard-owner--with-databases
    (let ((gnosis-nodes-dir (expand-file-name "nodes/" original-dir)))
      (gnosis-test-dashboard-navigation--node "n1" "One" "n2")
      (gnosis-test-dashboard-navigation--node "n2" "Two")
      (dolist (case '((tags gnosis-dashboard-filter-tags)
                      (tags gnosis-dashboard-tags-back)
                      (themata gnosis-dashboard-filter-themata)
                      (themata gnosis-dashboard-filter-themata-by-reviews)
                      (themata gnosis-dashboard-themata-back)
                      (nodes gnosis-dashboard-nodes-show-links)
                      (nodes gnosis-dashboard-nodes-show-backlinks)
                      (nodes gnosis-dashboard-nodes-show-themata-links)
                      (nodes gnosis-dashboard-nodes-visit)
                      (nodes gnosis-dashboard-nodes-filter-by-title)
                      (nodes gnosis-dashboard-nodes-filter-by-content)
                      (nodes gnosis-dashboard-nodes-filter-by-tag)
                      (nodes gnosis-dashboard-nodes-back)))
        (setq gnosis-db original-db gnosis-dir original-dir)
        (pcase (car case)
          ('tags (gnosis-dashboard-output-tags))
          ('themata (gnosis-dashboard-output-themata '(42 43)))
          ('nodes (gnosis-dashboard-output-nodes)))
        (gnosis-dashboard--push-current-view)
        (let ((before (gnosis-test-dashboard-owner--view))
              (history (copy-tree gnosis-dashboard--history))
              (current-prefix-arg '(4))
              prompted)
          (setq gnosis-db successor-db gnosis-dir successor-dir)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) (setq prompted t) "tag"))
                    ((symbol-function 'read-number)
                     (lambda (&rest _) (setq prompted t) 0))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _) (setq prompted t) "tag")))
            (ert-info ((format "%S" case))
              (should-error (call-interactively (cadr case)) :type 'user-error)))
          (should-not prompted)
          (should (equal before (gnosis-test-dashboard-owner--view)))
          (should (equal history gnosis-dashboard--history)))))))

(ert-deftest gnosis-dashboard-navigation-filter-prompt-replacement ()
  "Replacing a connection or rendering during filter input retires the request."
  (gnosis-test-dashboard-owner--with-databases
    (let ((gnosis-nodes-dir (expand-file-name "nodes/" original-dir)))
      (gnosis-test-dashboard-navigation--node "n1" "Needle")
      (dolist (change '(database render))
        (dolist (case '((tags gnosis-dashboard-filter-tags)
                        (themata gnosis-dashboard-filter-themata)
                        (themata gnosis-dashboard-filter-themata-by-reviews)
                        (nodes gnosis-dashboard-nodes-filter-by-title)
                        (nodes gnosis-dashboard-nodes-filter-by-content)
                        (nodes gnosis-dashboard-nodes-filter-by-tag)))
          (setq gnosis-db original-db gnosis-dir original-dir)
          (pcase (car case)
            ('tags (gnosis-dashboard-output-tags))
            ('themata (gnosis-dashboard-output-themata '(42 43)))
            ('nodes (gnosis-dashboard-output-nodes)))
          (let ((current-prefix-arg '(4)) after)
            (cl-labels ((replace-view ()
                          (if (eq change 'database)
                              (setq gnosis-db successor-db gnosis-dir successor-dir)
                            (gnosis-dashboard-output-tags '("TAG")))
                          (setq after (gnosis-test-dashboard-owner--view))))
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) (replace-view) "tag"))
                        ((symbol-function 'read-number)
                         (lambda (&rest _) (replace-view) 0))
                        ((symbol-function 'completing-read)
                         (lambda (&rest _) (replace-view) "tag")))
                (ert-info ((format "%S %S" change case))
                  (should-error (call-interactively (cadr case)) :type 'user-error))))
            (should after)
            (should (equal after (gnosis-test-dashboard-owner--view)))))))))

(ert-deftest gnosis-dashboard-navigation-cold-nodes ()
  "A genuinely closed database opens before a node view captures its owner."
  (gnosis-test-with-db
    (let ((gnosis-nodes-dir (expand-file-name "nodes/" gnosis-dir))
          (gnosis-dashboard-buffer-name " *gnosis-cold-nodes*"))
      (gnosis-test-dashboard-navigation--node "n1" "One")
      (gnosis-sqlite-close gnosis-db)
      (setq gnosis-db nil)
      (unwind-protect
          (save-window-excursion
            (call-interactively #'gnosis-dashboard-output-nodes)
            (should (equal '("n1") gnosis-dashboard-nodes-current-ids))
            (should (eq gnosis-db gnosis-dashboard--database))
            ;; Actual study commands, not faked downstream dispatch.  With no
            ;; themata, review/practice take their ordinary empty-topic path.
            (call-interactively (local-key-binding (kbd "r")))
            (call-interactively (local-key-binding (kbd "p")))
            (call-interactively (local-key-binding (kbd "S")))
            (should (eq major-mode 'gnosis-study-mode)))
        (when-let* ((buffer (get-buffer gnosis-dashboard-buffer-name)))
          (kill-buffer buffer))
        (when-let* ((buffer (get-buffer "*Gnosis Study*")))
          (kill-buffer buffer))))))

(ert-deftest gnosis-dashboard-navigation-bulk-link-distinct-nodes ()
  "Every duplicate and literal disambiguation title links to its exact ID."
  (gnosis-test-dashboard-owner--with-databases
    (let ((gnosis-nodes-dir (expand-file-name "nodes/" gnosis-dir)))
      (gnosis-test-dashboard-navigation--node "n1" "Same")
      (gnosis-test-dashboard-navigation--node "n2" "Same")
      (gnosis-test-dashboard-navigation--node "n3" "Same — n1.org")
      (dolist (node '("n1" "n2" "n3"))
        (gnosis-update 'themata '(= keimenon "Original") '(= id 42))
        (gnosis-sqlite-execute gnosis-db "DELETE FROM thema_links WHERE source=42")
        (gnosis-dashboard-output-themata '(42 43))
        (gnosis-dashboard--goto-id 42)
        (call-interactively (local-key-binding (kbd "m")))
        (let ((candidates (gnosis-study-topic-candidates))
              (gnosis-completing-read-function #'completing-read))
          (should (= 3 (length (delete-dups (mapcar #'car candidates)))))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Original"))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt offered &rest _)
                       (let ((label (car (rassoc node candidates))))
                         (should (member label (all-completions "" offered)))
                         label)))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (call-interactively (local-key-binding (kbd "b")))))
        (should (equal (format "[[id:%s][Original]]" node)
                       (gnosis-get 'keimenon 'themata '(= id 42))))
        (should (equal (list (list 42 node))
                       (gnosis-select '[source dest] 'thema-links)))
        (should (equal "Unrelated" (gnosis-get 'keimenon 'themata '(= id 43))))))
    (gnosis-dashboard-output-themata '(42))
    (call-interactively (local-key-binding (kbd "m")))
    (let ((before (gnosis-test-dashboard-owner--snapshot original-db))
          (view (gnosis-test-dashboard-owner--view)))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Original"))
                ((symbol-function 'completing-read) (lambda (&rest _) (signal 'quit nil))))
        (should (eq 'quit (condition-case nil
                             (call-interactively (local-key-binding (kbd "b")))
                           (quit 'quit)))))
      (should (equal view (gnosis-test-dashboard-owner--view)))
      (should (equal before (gnosis-test-dashboard-owner--snapshot original-db))))))

(ert-deftest gnosis-dashboard-navigation-unmark-idempotent ()
  "Native u only removes marks; m toggles, and both advance in each collection."
  (gnosis-test-dashboard-owner--with-databases
    (let ((gnosis-nodes-dir (expand-file-name "nodes/" gnosis-dir)))
      (gnosis-test-dashboard-navigation--node "n1" "One")
      (gnosis-test-dashboard-navigation--node "n2" "Two")
      (dolist (view '(themata tags nodes))
        (pcase view
          ('themata (gnosis-dashboard-output-themata '(42 43)))
          ('tags (gnosis-dashboard-output-tags))
          ('nodes (gnosis-dashboard-output-nodes)))
        (goto-char (point-min))
        (let ((first (tabulated-list-get-id)))
          (call-interactively (local-key-binding (kbd "u")))
          (should-not gnosis-dashboard--selected-ids)
          (should-not (equal first (tabulated-list-get-id)))
          (gnosis-dashboard--goto-id first)
          (call-interactively (local-key-binding (kbd "m")))
          (should (equal (list first) gnosis-dashboard--selected-ids))
          (gnosis-dashboard--goto-id first)
          (call-interactively (local-key-binding (kbd "m")))
          (should-not gnosis-dashboard--selected-ids)
          (call-interactively (local-key-binding (kbd "M")))
          (gnosis-dashboard--goto-id first)
          (let ((remaining (remove first gnosis-dashboard--selected-ids)))
            (dotimes (_ 2)
              (gnosis-dashboard--goto-id first)
              (call-interactively (local-key-binding (kbd "u")))
              (should (equal remaining gnosis-dashboard--selected-ids))
              (should-not (equal first (tabulated-list-get-id)))
              (save-excursion
                (gnosis-dashboard--goto-id first)
                (should-not (seq-some (lambda (ov) (overlay-get ov 'gnosis-mark))
                                      (overlays-at (point))))))))))))

(ert-deftest gnosis-dashboard-navigation-delete-tag-membership ()
  "Delete then filter/back never resurrects missing tags or their popup count."
  (gnosis-test-dashboard-owner--with-databases
    (gnosis-dashboard-output-tags '("tag" "TAG"))
    (let ((case-fold-search nil)) (gnosis-dashboard-filter-tags "^tag$"))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (call-interactively (local-key-binding (kbd "d"))))
    (should-not gnosis-dashboard-tags-current)
    (should-not tabulated-list-entries)
    (call-interactively (local-key-binding (kbd "q")))
    (should (equal '("TAG") gnosis-dashboard-tags-current))
    (should (equal '("TAG") (mapcar #'car tabulated-list-entries)))
    (gnosis-dashboard-filter-tags "tag")
    (should (equal '("TAG") gnosis-dashboard-tags-current))
    (let ((description (keymap-popup--meta gnosis-dashboard-tags-mode-map 'description)))
      (should (equal "Tags (1)" (substring-no-properties (funcall description)))))))

(ert-deftest gnosis-dashboard-navigation-current-filters-and-global-search ()
  "Owned filters work; global searches and explicit IDs can select a new DB."
  (gnosis-test-dashboard-owner--with-databases
    (let ((gnosis-nodes-dir (expand-file-name "nodes/" gnosis-dir)))
      (gnosis-test-dashboard-navigation--node "n1" "One" "n2")
      (gnosis-test-dashboard-navigation--node "n2" "Two")
      (dolist (command '(gnosis-dashboard-nodes-filter-by-title
                          gnosis-dashboard-nodes-filter-by-content
                          gnosis-dashboard-nodes-filter-by-tag))
        (gnosis-dashboard-output-nodes '("n1"))
        (funcall command (pcase command
                           ('gnosis-dashboard-nodes-filter-by-title "One")
                           ('gnosis-dashboard-nodes-filter-by-content "Needle")
                           (_ "tag")))
        (should (equal '("n1") gnosis-dashboard-nodes-current-ids))
        (call-interactively (local-key-binding (kbd "q"))))
      (gnosis-dashboard-output-nodes '("n1"))
      (call-interactively (local-key-binding (kbd "f")))
      (should (equal '("n2") gnosis-dashboard-nodes-current-ids))
      (call-interactively (local-key-binding (kbd "q")))
      (should (equal '("n1") gnosis-dashboard-nodes-current-ids)))
    (gnosis-dashboard-output-themata '(42 43))
    (gnosis-dashboard-filter-themata "Original")
    (setq gnosis-db successor-db gnosis-dir successor-dir)
    (gnosis-dashboard-search-thema "Successor")
    (should (equal '(42) gnosis-dashboard-themata-current-ids))
    (should (eq gnosis-dashboard--database successor-db))
    (should-not gnosis-dashboard--history)
    (with-temp-buffer
      (gnosis-dashboard-filter-themata "Successor" '(42)))
    (with-temp-buffer (gnosis-dashboard-tag-view-themata "tag"))
    (should (eq gnosis-dashboard--database successor-db))))

(ert-deftest gnosis-dashboard-navigation-history-owner ()
  "Back checks both current authority and the retained history's producer."
  (gnosis-test-dashboard-owner--with-databases
    (gnosis-dashboard-output-tags '("tag"))
    (call-interactively (local-key-binding (kbd "RET")))
    (let ((history gnosis-dashboard--history))
      (setq gnosis-db successor-db gnosis-dir successor-dir)
      (call-interactively (local-key-binding (kbd "g")))
      (should-not gnosis-dashboard--history)
      ;; Even a retained older snapshot cannot be restored into fresh B rows.
      (setq gnosis-dashboard--history history)
      (let ((before (gnosis-test-dashboard-owner--view)))
        (should-error (call-interactively (local-key-binding (kbd "q")))
                      :type 'user-error)
        (should (equal before (gnosis-test-dashboard-owner--view)))
        (should (eq history gnosis-dashboard--history))))))

(ert-deftest gnosis-dashboard-navigation-node-mode-hook-replacement ()
  "Node mode hooks cannot relabel already-read rows with another DB owner."
  (gnosis-test-dashboard-owner--with-databases
    (let ((gnosis-nodes-dir (expand-file-name "nodes/" gnosis-dir)))
      (gnosis-test-dashboard-navigation--node "n1" "One"))
    (let ((gnosis-dashboard-nodes-mode-hook
           (list (lambda () (setq gnosis-db successor-db gnosis-dir successor-dir)))))
      (gnosis-dashboard-output-nodes))
    (should (equal '("n1") gnosis-dashboard-nodes-current-ids))
    (should (eq original-db gnosis-dashboard--database))
    (should-error (call-interactively (local-key-binding (kbd "S")))
                  :type 'user-error)))

(provide 'gnosis-test-dashboard-navigation)
;;; gnosis-test-dashboard-navigation.el ends here
