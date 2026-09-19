;;; gnosis-review-test-support.el --- Shared review fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Shared fixtures and assertions; loading this library registers no tests.

;;; Code:
(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-review)

(defun gnosis-test-content--add (kind)
  "Create a disposable question of KIND."
  (gnosis-add-thema-fields kind "The old answer"
                           (cond ((equal kind "basic") nil)
                                 ((equal kind "cloze") '("hint"))
                                 (t '("old" "new")))
                           '("old") "Explanation" nil 0 nil nil 222))

(defun gnosis-test-content--state (mode)
  "Retain a review state for MODE in the current buffer."
  (setq-local gnosis-review--state
              (gnosis-review-state-create
               :mode mode :persistent-p t :database gnosis-db
               :session-id (gnosis-scheduler-event-id)
               :event-id (gnosis-scheduler-event-id)
               :remaining '(222) :selected '(222) :initial 1 :total 1))
  (gnosis-review--save-session gnosis-review--state))

(defun gnosis-test-content--evidence ()
  "Read all scheduled/practice evidence and session projections."
  (mapcar (lambda (table)
            (gnosis-sqlite-select gnosis-db (concat "SELECT * FROM " table)))
          '("review_events" "practice_events" "scheduler_state"
            "study_session" "study_history")))

(defun gnosis-test-content--answer (kind &optional during-input input)
  "Review KIND with INPUT, calling DURING-INPUT at the input boundary."
  (let ((gnosis-review-buffer-name (buffer-name)))
    (cl-letf (((symbol-function 'gnosis-completing-read)
             (lambda (&rest _) (when during-input (funcall during-input)) (or input "old")))
            ((symbol-function 'gnosis--read-string-with-input-method)
             (lambda (&rest _) (when during-input (funcall during-input)) (or input "old"))))
      (funcall (intern (concat "gnosis-review-" kind)) 222))))

(defun gnosis-test-content--edit-field (field text)
  "Replace native draft FIELD's body with TEXT, adding absent fields."
  (goto-char (point-min))
  (if (re-search-forward (concat "^\\*\\* " (regexp-quote field) "[ \t]*$") nil t)
      (progn
        (forward-line 1)
        (let ((start (point))
              (end (if (re-search-forward "^\\*\\* " nil t)
                       (line-beginning-position) (point-max))))
          (delete-region start end)
          (goto-char start)
          (insert text "\n\n")))
    (goto-char (point-max))
    (insert "\n** " field "\n" text "\n")))

(defun gnosis-test-content--edit-actions (answer edits &optional final after-edit)
  "Run public review ANSWER through native EDITS and FINAL actions.
EDITS contains field alists, nil for unchanged save, or `cancel'.
Call AFTER-EDIT in the original buffer after each edit.  Return the exact
answer passed to acceptance; also prove editing itself writes no evidence."
  (let* ((origin (current-buffer))
         (gnosis-review-buffer-name (buffer-name))
         (displayed (buffer-string))
         (before (gnosis-test-content--evidence))
         (gnosis-save-hook nil)
         (gnosis-review-editing-p nil)
         (accept (symbol-function 'gnosis-review-result))
         (final (or final '(?n)))
         accepted)
    (unwind-protect
        (cl-letf (((symbol-function 'recursive-edit)
                   (lambda ()
                     (should (derived-mode-p 'gnosis-edit-mode))
                     (let ((edit (pop edits)))
                       (if (eq edit 'cancel)
                           (gnosis-test-content--edit-field "Keimenon" "Uncommitted change")
                         (dolist (field edit)
                           (gnosis-test-content--edit-field (car field) (cdr field))))
                       (cl-letf (((symbol-function 'exit-recursive-edit) #'ignore))
                         (call-interactively
                          (key-binding (kbd (if (eq edit 'cancel) "C-c C-k" "C-c C-c"))))))
                     (set-buffer origin)
                     (when after-edit (funcall after-edit))))
                  ((symbol-function 'gnosis-review--read-action)
                   (lambda (&rest _)
                     (should (string-prefix-p (car (split-string displayed "Next review:"))
                                              (buffer-string)))
                     (should (equal before (gnosis-test-content--evidence)))
                     (if edits ?e (or (pop final) (ert-fail "Unexpected action prompt")))))
                  ((symbol-function 'gnosis-review-result)
                   (lambda (id success result)
                     (setq accepted (cons success result))
                     (dolist (key '(:event-id :reviewed-at-us :review-day :content :image :model))
                       (should (equal (plist-get (cdr answer) key) (plist-get result key))))
                     (funcall accept id success result))))
          (catch 'review-loop (gnosis-review-actions (car answer) 222 (cdr answer)))
          (should-not edits)
          (should-not final)
          (should (= (if (car accepted) 3 1)
                     (caar (sqlite-select gnosis-db
                             (concat "SELECT rating FROM "
                                     (if (eq (plist-get (cdr accepted) :mode) 'practice)
                                         "practice_events" "review_events"))))))
          accepted)
      (when (get-buffer "*Gnosis Edit*")
        (with-current-buffer "*Gnosis Edit*" (set-buffer-modified-p nil))
        (kill-buffer "*Gnosis Edit*")))))

(defun gnosis-test-active-owner--repurpose (&optional detach)
  "Associate this buffer with a successor file, optionally DETACH it."
  (set-visited-file-name (expand-file-name "successor.org" gnosis-dir) t)
  (should (eq major-mode 'gnosis-mode))
  (erase-buffer)
  (insert "Successor unsaved text")
  (setq-local header-line-format "Successor header")
  (use-local-map (make-sparse-keymap))
  (when detach (set-visited-file-name nil t)))

(defun gnosis-test-active-owner--seed (mode)
  "Keep accepted evidence and an unfinished checkpoint for MODE."
  (gnosis-test-content--add "basic")
  (with-temp-buffer
    (gnosis-mode)
    (gnosis-test-content--state mode)
    (let ((answer (gnosis-test-content--answer "basic")))
      (gnosis-review-result 222 (car answer) (cdr answer)))
    (gnosis-test-content--state mode)))

(defun gnosis-test-active-owner--rename ()
  "Rename this owner and return a foreign draft at its former name."
  (let ((name (buffer-name)))
    (rename-buffer (generate-new-buffer-name "*gnosis-renamed-owner*"))
    (let ((draft (get-buffer-create name)))
      (with-current-buffer draft
        (insert "Successor draft")
        (setq-local header-line-format "Successor header")
        (use-local-map (make-sparse-keymap)))
      draft)))

(defun gnosis-test-active-owner--draft-unchanged (draft map)
  "Assert DRAFT's text, header and MAP are unchanged."
  (with-current-buffer draft
    (should (equal "Successor draft" (buffer-string)))
    (should (equal "Successor header" header-line-format))
    (should (eq map (current-local-map)))))

(provide 'gnosis-review-test-support)
;;; gnosis-review-test-support.el ends here
