;;; gnosis-native-command-loop.el --- Native journey child -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Private child entry point for gnosis-test-native-command-loop.  Requests
;; travel to the parent through a receipt; the parent sends actual terminal
;; bytes.  Readers, recursive-edit and exit-recursive-edit remain untouched.

;;; Code:

(require 'ert)
(require 'gnosis-review-test-support)
(require 'gnosis-journal)

(defvar gnosis-native--inputs nil)
(defvar gnosis-native--answer nil)
(defvar gnosis-native--accepted nil)
(defvar gnosis-native--cancel nil)
(defvar gnosis-native--before nil)
(defvar gnosis-native--completions 0)

(defun gnosis-native--completed (&rest _)
  "Record a real Monkeytype completion before recursive input unwinds."
  (should (= (recursion-depth) 1))
  (cl-incf gnosis-native--completions))

(defun gnosis-native--log (format-string &rest arguments)
  "Append FORMAT-STRING formatted with ARGUMENTS to the parent receipt."
  (let ((print-escape-newlines t))
    (with-temp-buffer
      (insert (apply #'format format-string arguments) "\n")
      (write-region (point-min) (point-max)
                    (expand-file-name "receipt" (getenv "GNOSIS_NATIVE_ROOT"))
                    t 'silent))))

(defun gnosis-native--input (text)
  "Ask the parent to send TEXT as real terminal input."
  (gnosis-native--log "INPUT %S" text))

(defun gnosis-native--monkeytype ()
  "Exercise standalone region completion and cancellation."
  (unwind-protect
      (progn
        (advice-add 'gnosis-monkeytype--calculate-wpm :after #'gnosis-native--completed)
        (dolist (cancel '(nil t))
          (with-temp-buffer
            (text-mode)
            (insert "abc\n\n")
            (switch-to-buffer (current-buffer))
            (goto-char (point-min))
            (set-mark (point-max))
            (let ((gnosis-monkeytype-buffer-name "*Native Monkeytype*")
                  (gnosis-native--completions 0))
              (gnosis-native--log "START monkeytype cancel=%S depth=%d"
                                  cancel (recursion-depth))
              (gnosis-native--input (if cancel "a\3\13" "abc"))
              (call-interactively #'gnosis-monkeytype-region)
              (should (= (recursion-depth) 0))
              (should-not (get-buffer gnosis-monkeytype-buffer-name))
              (should (= gnosis-native--completions (if cancel 0 1)))
              (should (equal (buffer-string) "abc\n\n"))
              (should (eq (key-binding (kbd "M-/")) #'dabbrev-expand))
              (gnosis-native--log "PASS monkeytype cancel=%S depth=0" cancel)))))
    (advice-remove 'gnosis-monkeytype--calculate-wpm #'gnosis-native--completed)))

(defun gnosis-native--read (original context)
  "Observe feedback CONTEXT and arrange input before invoking ORIGINAL."
  (should (equal gnosis-native--before (gnosis-test-content--evidence)))
  (should gnosis-native--inputs)
  (gnosis-native--input (pop gnosis-native--inputs))
  (funcall original context))

(defun gnosis-native--edit (&rest _)
  "Position the actual editor at Answer, then type and save or cancel."
  (goto-char (point-min))
  (re-search-forward "^\\*\\* Answer[ \t]*$")
  (forward-line 1)
  ;; This observer only selects the field; text and acceptance use native keys.
  (gnosis-native--input
   (concat "\13new" (if gnosis-native--cancel "\3\13" "\3\3"))))

(defun gnosis-native--accept (original id success result)
  "Verify captured acceptance, then call ORIGINAL with ID SUCCESS RESULT."
  (should (eq success (car gnosis-native--answer)))
  (dolist (key '(:event-id :reviewed-at-us :review-day :content :preview))
    (should (equal (plist-get result key)
                   (plist-get (cdr gnosis-native--answer) key))))
  (setq gnosis-native--accepted (cons success result))
  (funcall original id success result))

(defun gnosis-native--review ()
  "Exercise due/practice edit-save-Next and edit-cancel-Next."
  (unwind-protect
      (progn
        (advice-add 'gnosis-review--read-action :around #'gnosis-native--read)
        (advice-add 'gnosis-review-result :around #'gnosis-native--accept)
        (advice-add 'gnosis-edit-thema :after #'gnosis-native--edit)
        (dolist (mode '(due practice))
          (dolist (cancel '(nil t))
            (dolist (success '(nil t))
              (gnosis-test-with-db
                (gnosis-test-content--add "basic")
                (let* ((gnosis-review-buffer-name "*Native Review*")
                       (owner (gnosis-review--setup-buffer '(222) mode))
                       (gnosis-review-basic-input 'typed)
                       (gnosis-native--cancel cancel)
                       (gnosis-native--inputs '("e" "n"))
                       (gnosis-native--accepted nil)
                       (schedule (gnosis-select '* 'scheduler-state))
                       (content (gnosis--draft-content gnosis-db 222)))
                  (unwind-protect
                      (progn
                        (switch-to-buffer owner)
                        (gnosis-test-content--state mode)
                        (gnosis-native--input (if success "old\r" "wrong\r"))
                        (let* ((gnosis-native--answer (gnosis-review-basic 222))
                               (gnosis-native--before (gnosis-test-content--evidence)))
                          (should (eq success (car gnosis-native--answer)))
                          (gnosis-review-actions success 222 (cdr gnosis-native--answer))
                          (should-not gnosis-native--inputs)
                          (should gnosis-native--accepted)
                          (should (= (recursion-depth) 0))
                          (should (equal (list (cons 222 success))
                                         (gnosis-review-state-outcomes gnosis-review--state)))
                          (should (= 1 (gnosis-review-state-reviewed gnosis-review--state)))
                          (should (= 1 (length (gnosis-select '*
                                                              (if (eq mode 'due)
                                                                  'review-events 'practice-events)))))
                          (should (= (if success 3 1)
                                     (caar (sqlite-select
                                            gnosis-db
                                            (concat "SELECT rating FROM "
                                                    (if (eq mode 'due)
                                                        "review_events" "practice_events"))))))
                          (let ((once (gnosis-test-content--evidence)))
                            (gnosis-review-result 222 success (cdr gnosis-native--accepted))
                            (should (equal once (gnosis-test-content--evidence))))
                          (if cancel
                              (should (equal content (gnosis--draft-content gnosis-db 222)))
                            (should (equal '("new") (gnosis-get 'answer 'themata '(= id 222))))))
                        (when (eq mode 'practice)
                          (should (equal schedule (gnosis-select '* 'scheduler-state))))
                        (gnosis-native--log "PASS review mode=%S cancel=%S success=%S once=t"
                                            mode cancel success))
                    (dolist (name (list gnosis-review-buffer-name "*Gnosis Edit*"))
                      (when-let* ((buffer (get-buffer name)))
                        (with-current-buffer buffer (set-buffer-modified-p nil))
                        (kill-buffer buffer))))))))))
    (advice-remove 'gnosis-review--read-action #'gnosis-native--read)
    (advice-remove 'gnosis-edit-thema #'gnosis-native--edit)
    (advice-remove 'gnosis-review-result #'gnosis-native--accept)))

(defun gnosis-native--feedback-dismissed ()
  "Verify native dismissal and movement left the answer pending."
  (interactive)
  (should-not (keymap-popup--popup-buffer))
  (should (= (point) (point-min)))
  (should-not (plist-get gnosis-review--feedback :choice))
  (should (equal gnosis-native--before (gnosis-test-content--evidence)))
  (gnosis-native--log "PASS feedback dismissed movement=t evidence=unchanged")
  (when gnosis-native--cancel (gnosis-native--input "\7")))

(defun gnosis-native--feedback-retire ()
  "Retire the native feedback owner, refuse stale actions, then cancel it."
  (interactive)
  (set-visited-file-name (expand-file-name "retired" gnosis-dir) t)
  (set-visited-file-name nil t)
  (should-error (gnosis-review-feedback-next) :type 'user-error)
  (should (equal gnosis-native--before (gnosis-test-content--evidence)))
  (gnosis-native--log "PASS feedback retired binding refused evidence=unchanged")
  (gnosis-native--input "\7"))

(defun gnosis-native--feedback-open (&rest _)
  "Inspect the real rendered popup before sending the next native input."
  (let* ((context gnosis-review--feedback)
         (popup (keymap-popup--popup-buffer))
         (text (with-current-buffer popup (buffer-string))))
    (should (equal gnosis-native--before (gnosis-test-content--evidence)))
    (dolist (label '("Review" "Content" "Manage" "Edit" "View source"
                     "Flag needs_work" "Suspend / unsuspend" "Delete" "Accept & quit"))
      (should (string-search label text)))
    (should (string-search (gnosis-review--feedback-next-label) text))
    (should (string-search (gnosis-review--feedback-override-label) text))
    (dolist (key '(:event-id :reviewed-at-us :review-day))
      (should (equal (plist-get (plist-get context :result) key)
                     (plist-get (plist-get context :alternate) key))))
    (should gnosis-native--inputs)
    ;; C-g is a quit event: send only after recursive input is running,
    ;; rather than racing the synchronous popup setup boundary.
    (run-at-time 0.05 nil #'gnosis-native--input (pop gnosis-native--inputs))))

(defun gnosis-native--feedback ()
  "Exercise real popup dismissal, reopening, overrides, acceptance and abort."
  (unwind-protect
      (progn
        (keymap-set gnosis-review-feedback-mode-map "C-c C-t"
                    #'gnosis-native--feedback-dismissed)
        (keymap-set gnosis-review-feedback-mode-map "C-c C-r"
                    #'gnosis-native--feedback-retire)
        (advice-add 'gnosis-review--feedback-show :after #'gnosis-native--feedback-open)
        (dolist (mode '(due practice))
          (dolist (terminal '(next quit abort retired))
            (gnosis-test-with-db
              (gnosis-test-content--add "basic")
              (let* ((gnosis-review-buffer-name "*Native Feedback*")
                     (owner (gnosis-review--setup-buffer '(222) mode)))
                (unwind-protect
                    (with-current-buffer owner
                      (switch-to-buffer owner)
                      (gnosis-test-content--state mode)
                      (let* ((gnosis-review-basic-input 'typed)
                             (pending (progn
                                        (gnosis-native--input "old\r")
                                        (cdr (gnosis-review-basic 222))))
                             (gnosis-native--before (gnosis-test-content--evidence))
                             (schedule (gnosis-select '* 'scheduler-state))
                             (gnosis-native--cancel (eq terminal 'abort))
                             (gnosis-native--inputs
                              (pcase terminal
                                ('next '("\7\33<\3\24?" "o" "o" "\7n"))
                                ('quit '("o" "o" "q"))
                                ('abort '("\7\33<\3\24"))
                                ('retired '("\7\3\22"))))
                             (outcome (condition-case nil
                                          (catch 'review-loop (gnosis-review-actions t 222 pending))
                                        (quit 'cancelled))))
                        (should-not gnosis-native--inputs)
                        (should (= (recursion-depth) 0))
                        (should-not gnosis-review-feedback-mode)
                        (should-not gnosis-review--feedback)
                        (should-not (keymap-popup--popup-buffer))
                        (if (memq terminal '(abort retired))
                            (progn
                              (should (eq outcome 'cancelled))
                              (should (equal gnosis-native--before (gnosis-test-content--evidence))))
                          (should (= 1 (length (gnosis-select
                                                '* (if (eq mode 'due)
                                                       'review-events 'practice-events)))))
                          (should (= 3 (gnosis-get 'rating
                                                  (if (eq mode 'due) 'review-events 'practice-events)
                                                  '(= thema-id 222)))))
                        (when (eq mode 'practice)
                          (should (equal schedule (gnosis-select '* 'scheduler-state))))
                        (gnosis-native--log "PASS feedback mode=%S terminal=%S depth=0" mode terminal)))
                  (when (buffer-live-p owner)
                    (with-current-buffer owner (set-buffer-modified-p nil))
                    (kill-buffer owner))))))))
    (advice-remove 'gnosis-review--feedback-show #'gnosis-native--feedback-open)))

(defun gnosis-native--keys (keys)
  "Run native KEYS, restoring current buffer to the selected window."
  (execute-kbd-macro keys)
  (set-buffer (window-buffer (selected-window))))

(defun gnosis-native--capture (date key text section)
  "Capture TEXT with KEY in DATE and verify its SECTION and return point."
  (gnosis-native--keys
   (vconcat (kbd (concat "C-c j " key)) text (kbd "C-c C-c")))
  (should (equal date (gnosis-journal--date-at-point)))
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (should (equal date (gnosis-journal--date-at-point)))
    (should (equal section (save-excursion (org-get-heading t t t t))))
    (when (equal key "a")
      (beginning-of-line)
      (should (looking-at-p
               (regexp-quote (concat gnosis-journal-bullet-point-char " [ ] ")))))))

(defun gnosis-native--journal ()
  "Chain captures across dated entries without repairing point between them."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes/" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal/" gnosis-dir))
           (gnosis-journal-file (expand-file-name "journal.org" gnosis-dir))
           (gnosis-journal-todo-files nil)
           (dates '("2001-02-03" "2001-02-04" "2001-02-05"))
           ids)
      (keymap-global-set "C-c j" gnosis-journal-prefix-map)
      (dolist (date dates)
        (gnosis-native--keys (kbd (concat "C-c j d " date " RET")))
        (push (cons date (org-id-get)) ids))
      (dolist (date dates)
        (gnosis-native--keys (kbd (concat "C-c j d " date " RET")))
        (when (equal date "2001-02-04") (org-narrow-to-subtree))
        (gnosis-native--keys (kbd "C-c j i Default RET"))
        (should (and (bolp) (eolp)))
        (gnosis-native--keys (vconcat (concat date " prose\n")))
        (should (equal date (gnosis-journal--date-at-point)))
        (gnosis-native--capture date "c" (concat date " first\nsecond line") "Daily Notes")
        (gnosis-native--capture date "a" (concat date " local task") "Goals")
        (gnosis-native--capture date "c" (concat date " later\nlast line") "Daily Notes")
        (gnosis-native--capture date "a" (concat date " second task") "Goals")
        (gnosis-native--keys (kbd "C-x C-s"))
        (gnosis-native--log "PASS journal date=%s repeated-captures=t" date))
      (widen)
      (let ((before (buffer-string)))
        (dolist (key '("c" "a"))
          (let ((error-data
                 (should-error
                  (gnosis-native--keys
                   (vconcat (kbd (concat "C-c j " key))
                            "Discard this\nsecond line" (kbd "C-c C-k")))
                  :type 'error)))
            ;; Emacs 32 signals plain error here; earlier versions use
            ;; user-error.  Require the native abort, not just any failure.
            (should (equal (cdr error-data) '("Aborted edit"))))
          (set-buffer (get-file-buffer gnosis-journal-file))
          (should (equal before (buffer-string)))
          (should (= (recursion-depth) 0)))
        (kill-buffer (current-buffer))
        (gnosis-native--keys (kbd "C-c j d 2001-02-03 RET"))
        (should (equal before (buffer-string)))
        (dolist (entry ids)
          (gnosis-journal-date (car entry))
          (should (equal (cdr entry) (org-id-get))))
        (kill-buffer (get-file-buffer gnosis-journal-file))))))

(defun gnosis-native--run ()
  "Run the selected native journey and exit with an explicit receipt."
  (condition-case error-data
      (let ((gnosis-dir (file-name-as-directory
                         (expand-file-name "data" (getenv "GNOSIS_NATIVE_ROOT")))))
        (setq gnosis-testing t gnosis-vc-auto-push nil
              create-lockfiles nil make-backup-files nil auto-save-default nil
              org-id-track-globally nil)
        (should-not noninteractive)
        (run-at-time 30 nil
                     (lambda ()
                       (gnosis-native--log "FAIL watchdog depth=%d buffer=%S"
                                           (recursion-depth) (buffer-name))
                       (kill-emacs 3)))
        (gnosis-native--log "START interactive=%S emacs=%s source=%S helper=%S"
                            (not noninteractive) emacs-version
                            (symbol-file 'gnosis-monkeytype-region)
                            (symbol-file 'gnosis-native--run))
        (pcase (getenv "GNOSIS_NATIVE_JOURNEY")
          ("monkeytype" (gnosis-native--monkeytype))
          ("review" (gnosis-native--review))
          ("feedback" (gnosis-native--feedback))
          ("journal" (gnosis-native--journal))
          (_ (error "Unknown native journey")))
        (should (= (recursion-depth) 0))
        (gnosis-native--log "PASS native journey depth=0")
        (kill-emacs 0))
    ((error quit)
     (gnosis-native--log "FAIL %S depth=%d" error-data (recursion-depth))
     (kill-emacs 1))))

(provide 'gnosis-native-command-loop)
;;; gnosis-native-command-loop.el ends here
