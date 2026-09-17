;;; gnosis-test-links-plans.el --- Confirmed link mutations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Native SQLite/source fixtures for confirmation ownership and convergence.

;;; Code:

(require 'ert)
(require 'gnosis-test-link-quality)
(require 'gnosis-nodes)

(defun gnosis-test-links-plans--node (id &optional journal)
  "Index a real source with ID, in the JOURNAL directory when non-nil."
  (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
         (gnosis-journal-dir (expand-file-name "journal" gnosis-dir))
         (dir (if journal gnosis-journal-dir gnosis-nodes-dir))
         (file (expand-file-name (concat id ".org") dir)))
    (make-directory dir t)
    (with-temp-file file
      (insert ":PROPERTIES:\n:ID: " id "\n:END:\n#+title: " id "\n"))
    (gnosis-nodes-update-file file)))

(defun gnosis-test-links-plans--seed ()
  "Create selected content with nonempty scheduled and practice evidence."
  (gnosis-add-thema-fields "basic" "Drug [[id:lost][Source]]" nil '("A") "P" '("tag") 0 '("lost") nil 1)
  (gnosis-add-thema-fields "basic" "Neighbor" nil '("N") "P" '("tag") 0 nil nil 2)
  (gnosis-scheduler-accept-review (make-string 64 ?a) 1 'success 1000000 (gnosis--today-int))
  (gnosis--insert-into 'practice-events '(["practice" 1 "session" 1 1 3])))

(ert-deftest gnosis-test-links-plans-bulk-content-drift ()
  "A public author edit or deletion during confirmation defeats stale writes."
  (dolist (change '(edit delete))
    (gnosis-test-with-db
      (gnosis-test-links-plans--seed)
      (gnosis-test-links-plans--node "target")
      (let (after-prompt)
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (should-not gnosis-sqlite--transaction-dbs)
                     (pcase change
                       ('edit (gnosis-update-thema 1 "Revised Drug" nil '("B") "New P" '("tag") nil))
                       ('delete (gnosis-delete-themata '(1))))
                     (setq after-prompt (gnosis-test-draft--rows))
                     t)))
          (should-error (gnosis-bulk-link-string "Drug" "target") :type 'user-error))
        (gnosis-test-link-quality--reopen)
        (should (equal after-prompt (gnosis-test-draft--rows)))))))

(ert-deftest gnosis-test-links-plans-successor-database ()
  "Neither standalone bulk linking nor maintenance may acquire a successor."
  (dolist (command '(bulk gnosis-links-cleanup gnosis-links-sync))
    (gnosis-test-with-db
      (gnosis-test-links-plans--seed)
      (let* ((original gnosis-db)
             (original-rows (gnosis-test-draft--rows))
             (other (gnosis-sqlite-open (expand-file-name "other.db" gnosis-dir)))
             other-rows)
        (unwind-protect
            (progn
              (let ((gnosis-db other))
                (gnosis-db-init)
                (gnosis-test-links-plans--seed)
                (gnosis-test-links-plans--node "lost")
                (setq other-rows (gnosis-test-draft--rows)))
              (cl-letf (((symbol-function 'y-or-n-p)
                         (lambda (&rest _)
                           (should-not gnosis-sqlite--transaction-dbs)
                           (setq gnosis-db other) t)))
                (should-error
                 (if (eq command 'bulk) (gnosis-bulk-link-themata '(1) "Drug" "target")
                   (call-interactively command))
                 :type 'user-error))
              (should (eq gnosis-db other))
              (should (equal other-rows (gnosis-test-draft--rows)))
              (setq gnosis-db original)
              (should (equal original-rows (gnosis-test-draft--rows))))
          (setq gnosis-db original)
          (gnosis-sqlite-close other))))))

(ert-deftest gnosis-test-links-plans-maintenance-drift ()
  "Source/index repairs and membership changes invalidate the original plan."
  (dolist (command '(gnosis-links-cleanup gnosis-links-sync))
    (dolist (change '(text index node new-edge))
      (gnosis-test-with-db
        (gnosis-test-links-plans--seed)
        (gnosis-test-links-plans--node "valid")
        (gnosis--insert-into 'thema-links '([1 "valid"]))
        (let (after-prompt)
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _)
                       (should-not gnosis-sqlite--transaction-dbs)
                       (pcase change
                         ('text (gnosis-update-thema 1 "Drug [[id:valid]]" nil '("A") "P" '("tag") '("valid")))
                         ('index (gnosis-sqlite-execute gnosis-db "DELETE FROM thema_links WHERE dest = ?" '("lost")))
                         ('node (gnosis-test-links-plans--node "lost"))
                         ('new-edge (gnosis--insert-into 'thema-links '([2 "lost"]))))
                       (setq after-prompt (gnosis-test-draft--rows))
                       t)))
            (should-error (call-interactively command) :type 'user-error))
          (gnosis-test-link-quality--reopen)
          (should (equal after-prompt (gnosis-test-draft--rows))))))))

(ert-deftest gnosis-test-links-plans-sync-converges ()
  "Unavailable authored links stay reported, then native node arrival repairs."
  (gnosis-test-with-db
    (gnosis-test-links-plans--seed)
    (gnosis-test-links-plans--node "valid")
    (gnosis-test-links-plans--node "stale")
    (gnosis-update 'themata '(= keimenon "Drug [[id:lost]] [[id:valid]] [[id:journal]]") '(= id 1))
    (gnosis--insert-into 'thema-links '([1 "stale"]))
    (let ((text (gnosis-get 'keimenon 'themata '(= id 1))) snapshot messages)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'message)
                 (lambda (format &rest args) (push (apply #'format format args) messages))))
        (call-interactively #'gnosis-links-sync)
        (should (equal (gnosis-select '* 'thema-links) '((1 "valid"))))
        (setq snapshot (gnosis-test-draft--rows))
        (dotimes (_ 3) (call-interactively #'gnosis-links-sync))
        (should (equal snapshot (gnosis-test-draft--rows)))
        (should (string-match-p "unavailable" (car messages)))
        (should (equal (sort (gnosis--missing-links) (lambda (a b) (string< (cadr a) (cadr b))))
                       '((1 "journal") (1 "lost"))))
        (gnosis-test-links-plans--node "lost")
        (gnosis-test-links-plans--node "journal" t)
        (call-interactively #'gnosis-links-sync)
        (setq snapshot (gnosis-test-draft--rows))
        (call-interactively #'gnosis-links-sync)
        (should (equal snapshot (gnosis-test-draft--rows))))
      (gnosis-test-link-quality--reopen)
      (should (equal text (gnosis-get 'keimenon 'themata '(= id 1))))
      (should (equal (sort (gnosis-select 'dest 'thema-links '(= source 1) t) #'string<)
                     '("journal" "lost" "valid"))))))

(ert-deftest gnosis-test-links-plans-maintenance-cancel-and-fault ()
  "Maintenance preserves evidence on decline/quit/error and succeeds on retry."
  (dolist (command '(gnosis-links-cleanup gnosis-links-sync))
    (dolist (fault '(decline prompt-quit error quit))
      (gnosis-test-with-db
        (gnosis-test-links-plans--seed)
        (let ((before (gnosis-test-draft--rows))
              (delete (symbol-function 'gnosis--delete-orphaned-links))
              (writes 0) caught)
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _)
                       (should-not gnosis-sqlite--transaction-dbs)
                       (pcase fault ('decline nil) ('prompt-quit (signal 'quit nil)) (_ t))))
                    ((symbol-function 'gnosis--delete-orphaned-links)
                     (lambda (ids)
                       (funcall delete ids)
                       (cl-incf writes)
                       (signal fault '("Injected maintenance failure")))))
            (condition-case err (call-interactively command)
              ((error quit) (setq caught (car err)))))
          (should (eq caught (pcase fault ('decline nil) ('prompt-quit 'quit) (_ fault))))
          (should (= writes (if (memq fault '(error quit)) 1 0)))
          (gnosis-test-link-quality--reopen)
          (should (equal before (gnosis-test-draft--rows)))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (call-interactively command))
          (should-not (gnosis-select '* 'thema-links))
          (dolist (table '("review_events" "practice_events" "scheduler_state"))
            (should (cdr (assoc table before)))
            (should (equal (cdr (assoc table before))
                           (cdr (assoc table (gnosis-test-draft--rows)))))))))))

(provide 'gnosis-test-links-plans)
;;; gnosis-test-links-plans.el ends here
