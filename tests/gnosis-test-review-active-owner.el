;;; gnosis-test-review-active-owner.el --- Active review lifetime tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Native file association retires input, pending answers and preparation.

;;; Code:
(require 'ert)
(require 'gnosis-test-review-content)
(require 'gnosis-test-model)
(require 'gnosis-test-image)

(defun gnosis-test-active-owner--repurpose (&optional detach)
  "Associate this buffer with a successor file, optionally DETACH it."
  (set-visited-file-name (expand-file-name "successor.org" gnosis-dir) t)
  (should (eq major-mode 'gnosis-mode))
  (erase-buffer)
  (insert "Successor unsaved text")
  (setq-local header-line-format "Successor header")
  (use-local-map (make-sparse-keymap))
  (when detach (set-visited-file-name nil t)))

(ert-deftest gnosis-active-owner-pending-actions ()
  "All retained actions refuse association and detach without changing evidence."
  (dolist (mode '(due practice))
    (dolist (detach '(nil t))
      (dolist (action '(?n ?o ?s ?d ?e ?v ?q ?f))
        (gnosis-test-with-db
          (gnosis-test-content--add "basic")
          (with-temp-buffer
            (gnosis-mode)
            (gnosis-test-content--state mode)
            (let ((accepted (gnosis-test-content--answer "basic")))
              (gnosis-review-result 222 (car accepted) (cdr accepted)))
            (gnosis-test-content--state mode)
            (let* ((answer (gnosis-test-content--answer "basic"))
                   (before (gnosis-test-content--evidence))
                   (content (gnosis-review--content-thema 222)))
              (cl-letf (((symbol-function 'read-char-choice)
                         (lambda (&rest _)
                           (gnosis-test-active-owner--repurpose detach)
                           action))
                        ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                (should-error (catch 'review-loop
                                (gnosis-review-actions (car answer) 222 (cdr answer)))
                              :type 'user-error))
              (should (equal before (gnosis-test-content--evidence)))
              (should (equal content (gnosis-review--content-thema 222)))
              (should (equal "Successor unsaved text" (buffer-string)))
              (should (equal "Successor header" header-line-format))
              (set-buffer-modified-p nil))))))))

(ert-deftest gnosis-active-owner-input-and-positive-acceptance ()
  "Native association during input refuses; same-owner Next accepts once."
  (dolist (mode '(due practice))
    (gnosis-test-with-db
      (gnosis-test-content--add "basic")
      (with-temp-buffer
        (gnosis-mode)
        (gnosis-test-content--state mode)
        (let* ((answer (gnosis-test-content--answer "basic")))
          (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
            (gnosis-review-actions (car answer) 222 (cdr answer)))
          (let ((accepted (gnosis-test-content--evidence)))
            (should (= 1 (length (gnosis-select '* (if (eq mode 'due)
                                                     'review-events 'practice-events)))))
            (gnosis-test-content--state mode)
            (should-error
             (gnosis-test-content--answer "basic"
                                          (lambda ()
                                            (let ((name (buffer-name)))
                                              (gnosis-test-active-owner--repurpose t)
                                              (rename-buffer name))))
             :type 'user-error)
            ;; The second batch is retained; the first accepted evidence stays.
            (should (equal (seq-take accepted 3)
                           (seq-take (gnosis-test-content--evidence) 3)))
            (should (equal "Successor unsaved text" (buffer-string)))
            (set-buffer-modified-p nil)))))))

(defun gnosis-test-active-owner--model (repurpose failure &optional cancel-delivery)
  "Deliver model FAILURE after REPURPOSE, or during CANCEL-DELIVERY."
  (gnosis-test-with-db
    (save-window-excursion
      (let* ((model (gnosis-test-model--add))
             (gnosis-review-buffer-name "*gnosis-active-owner-test*")
             (owner (gnosis-review--setup-buffer (list model) 'practice))
             (row (car (gnosis-review--answer-thema model)))
             (fields (gnosis-model-fields (nth 0 row) (nth 2 row) (nth 3 row)))
             (before (gnosis-test-content--evidence))
             context callback opened successor-map)
        (unwind-protect
            (progn
              (switch-to-buffer owner)
              (cl-letf (((symbol-function 'gnosis-model-prepare)
                         (lambda (_kind _hypothesis _answer done)
                           (setq callback done) 'job))
                        ((symbol-function 'gnosis-model-cancel-preparation)
                         (lambda (_job)
                           (when (and callback cancel-delivery)
                             (funcall callback fields failure))))
                        ((symbol-function 'gnosis-model--canvas-size) (lambda () 256))
                        ((symbol-function 'gnosis-model-open)
                         (lambda (&rest _)
                           (setq opened t)
                           (insert "Attached model")
                           (setq-local canvas-3d--process nil canvas-3d--image nil
                                       canvas-3d-mode-map (make-sparse-keymap))))
                        ((symbol-function 'recursive-edit)
                         (lambda ()
                           (setq context gnosis-review--model-context)
                           (when repurpose
                             (gnosis-test-active-owner--repurpose (eq repurpose 'detach))
                             (setq successor-map (current-local-map)))
                           (unless cancel-delivery
                             (funcall callback fields failure)
                             (funcall callback fields failure))
                           (when (and (not repurpose) (not cancel-delivery))
                             (if failure
                                 (should (equal failure (plist-get context :error)))
                               (should opened)
                               (should (eq (key-binding (kbd "RET")) #'gnosis-review-model-submit)))))))
                (should-error (gnosis-review-model model) :type 'user-error))
              (when (or repurpose cancel-delivery)
                (should-not opened)
                (should-not (plist-get context :fields))
                (should-not (plist-get context :error)))
              (when repurpose
                (should (eq successor-map (current-local-map)))
                (should (equal "Successor header" header-line-format))
                (should (equal "Successor unsaved text" (buffer-string)))
                (funcall callback fields failure)
                (should (equal "Successor unsaved text" (buffer-string))))
              (should (equal before (gnosis-test-content--evidence))))
          (when (buffer-live-p owner)
            (with-current-buffer owner (set-buffer-modified-p nil))
            (kill-buffer owner)))))))

(ert-deftest gnosis-active-owner-preparation-late-delivery ()
  "Queued success and failure cannot settle into file successors or detach."
  (dolist (repurpose '(associated detach))
    (dolist (failure '(nil "Late preparation failure"))
      (gnosis-test-active-owner--model repurpose failure))))

(ert-deftest gnosis-active-owner-preparation-positive ()
  "Same-owner success installs native submit keys; failure reports normally."
  (dolist (failure '(nil "Preparation failure"))
    (gnosis-test-active-owner--model nil failure)))

(ert-deftest gnosis-active-owner-preparation-cancel-reentrancy ()
  "Cleanup retires preparation before invoking a reentrant cancellation."
  (dolist (repurpose '(nil associated detach))
    (dolist (failure '(nil "Cancellation callback"))
      (gnosis-test-active-owner--model repurpose failure t))))

(ert-deftest gnosis-active-owner-setup-refuses-successor ()
  "Setup cannot reclaim a file-backed or detached gnosis-mode successor."
  (gnosis-test-with-db
    (let* ((gnosis-review-buffer-name "*gnosis-active-owner-test*")
           (owner (gnosis-review--setup-buffer '(222))))
      (unwind-protect
          (with-current-buffer owner
            (gnosis-test-active-owner--repurpose t)
            (rename-buffer gnosis-review-buffer-name)
            (should-error (gnosis-review--setup-buffer '(222)) :type 'user-error)
            (should (equal "Successor unsaved text" (buffer-string))))
        (with-current-buffer owner (set-buffer-modified-p nil))
        (kill-buffer owner)))))

(ert-deftest gnosis-active-owner-setup-hook-retirement ()
  "Native initialization hooks cannot retire then resurrect an encounter."
  (gnosis-test-with-db
    (let* ((gnosis-review-buffer-name "*gnosis-active-owner-test*")
           (gnosis-mode-hook
            (list (lambda () (gnosis-test-active-owner--repurpose t))))
           (owner (get-buffer-create gnosis-review-buffer-name)))
      (unwind-protect
          (progn
            (should-error (gnosis-review--setup-buffer '(222)) :type 'user-error)
            (with-current-buffer owner
              (should (equal "Successor unsaved text" (buffer-string)))
              (should-not gnosis-review--state)))
        (with-current-buffer owner (set-buffer-modified-p nil))
        (kill-buffer owner)))))

(ert-deftest gnosis-active-owner-lookahead-retirement ()
  "Retire speculative work before reentrant delivery, including detach."
  (dolist (detach '(nil t))
    (gnosis-test-with-db
      (gnosis-test-content--add "basic")
      (let* ((model (gnosis-test-model--add))
             (gnosis-review-buffer-name "*gnosis-active-owner-test*")
             (owner (gnosis-review--setup-buffer (list 222 model) 'practice)))
        (unwind-protect
            (with-current-buffer owner
              (let ((gnosis-review--running
                     (gnosis-review-state-session-id gnosis-review--state))
                    slot callback)
                (cl-letf (((symbol-function 'gnosis-model-prepare)
                           (lambda (_kind _hypothesis _answer done)
                             (setq callback done) nil))
                          ((symbol-function 'gnosis-model-cancel-preparation)
                           (lambda (_job)
                             (should-not gnosis-review--state)
                             (funcall callback '(:stale t) nil))))
                  (gnosis-review--lookahead-start)
                  (setq slot gnosis-review--lookahead)
                  (should slot)
                  (gnosis-test-active-owner--repurpose detach)
                  (should-not gnosis-review--lookahead)
                  (funcall callback '(:stale t) nil)
                  (funcall callback nil "late failure")
                  (should-not (plist-get slot :fields))
                  (should (equal "Successor unsaved text" (buffer-string))))))
          (with-current-buffer owner (set-buffer-modified-p nil))
          (kill-buffer owner))))))

(ert-deftest gnosis-active-owner-cancel-captures-foreground ()
  "Lookahead cancellation cannot redirect cancellation into a successor."
  (dolist (command '(gnosis-review-model-cancel gnosis-review--retire-buffer))
    (with-temp-buffer
      (let* ((old (list :buffer (current-buffer) :cancelled nil
                        :preparation 'old :depth 0))
             (successor (list :buffer (current-buffer) :cancelled nil
                              :preparation 'successor :depth 0))
             cancelled)
        (setq-local gnosis-review--model-context old
                    gnosis-review--lookahead (list :job 'lookahead))
        (cl-letf (((symbol-function 'gnosis-model-cancel-preparation)
                   (lambda (job)
                     (push job cancelled)
                     (when (eq job 'lookahead)
                       (should (plist-get old :cancelled))
                       (setq gnosis-review--model-context successor)))))
          (funcall command)
          (should-not (plist-get successor :cancelled))
          (should-not (memq 'successor cancelled)))))))

(ert-deftest gnosis-active-owner-preparation-retired-before-return ()
  "A preparation handle returned after retirement is cancelled, not orphaned."
  (dolist (foreground '(nil t))
    (gnosis-test-with-db
      (let* ((model (gnosis-test-model--add))
             (gnosis-review-buffer-name "*gnosis-active-owner-test*")
             (owner (gnosis-review--setup-buffer
                     (if foreground (list model) (list model model)) 'practice))
             (job (list :cancelled nil))
             entered)
        (unwind-protect
            (with-current-buffer owner
              (let ((gnosis-review--running
                     (gnosis-review-state-session-id gnosis-review--state)))
                (cl-letf (((symbol-function 'gnosis-model-prepare)
                           (lambda (&rest _)
                             (gnosis-test-active-owner--repurpose t)
                             job))
                          ((symbol-function 'recursive-edit)
                           (lambda () (setq entered t))))
                  (if foreground
                      (should-error (gnosis-review-model model) :type 'user-error)
                    (gnosis-review--lookahead-start))
                  (should (plist-get job :cancelled))
                  (should-not entered)
                  (should-not gnosis-review--lookahead)
                  (should (equal "Successor unsaved text" (buffer-string))))))
          (with-current-buffer owner (set-buffer-modified-p nil))
          (kill-buffer owner))))))

(ert-deftest gnosis-active-owner-setup-cancel-retirement ()
  "Setup rechecks retirement after cancelling previous speculative work."
  (gnosis-test-with-db
    (let* ((gnosis-review-buffer-name "*gnosis-active-owner-test*")
           (owner (gnosis-review--setup-buffer '(222))))
      (unwind-protect
          (with-current-buffer owner
            (setq gnosis-review--lookahead (list :job nil))
            (cl-letf (((symbol-function 'gnosis-model-cancel-preparation)
                       (lambda (_job) (gnosis-test-active-owner--repurpose t))))
              (should-error (gnosis-review--setup-buffer '(222)) :type 'user-error))
            (should-not gnosis-review--state)
            (should (equal "Successor unsaved text" (buffer-string))))
        (with-current-buffer owner (set-buffer-modified-p nil))
        (kill-buffer owner)))))

(defun gnosis-test-active-owner--setup-replacement (replacement cancellation)
  "Replace setup's owner with REPLACEMENT, optionally during CANCELLATION."
  (dolist (mode '(due practice))
    (dolist (public '(nil t))
      (gnosis-test-with-db
        (gnosis-test-content--add "basic")
        ;; Keep real accepted evidence and a separate unfinished checkpoint.
        (with-temp-buffer
          (gnosis-mode)
          (gnosis-test-content--state mode)
          (let ((answer (gnosis-test-content--answer "basic")))
            (gnosis-review-result 222 (car answer) (cdr answer)))
          (gnosis-test-content--state mode))
        (let* ((gnosis-review-buffer-name "*gnosis-setup-replacement-test*")
               (before (gnosis-test-content--evidence))
               (owner (if cancellation
                          (gnosis-review--setup-buffer '(222) mode)
                        (get-buffer-create gnosis-review-buffer-name)))
               (successor owner)
               (successor-map (make-sparse-keymap))
               successor-state
               (replace
                (lambda (&rest _)
                  (pcase replacement
                    ('mode (fundamental-mode))
                    ('roundtrip
                     (fundamental-mode)
                     (let ((gnosis-mode-hook nil)) (gnosis-mode)))
                    ('reenter
                     (gnosis-review--setup-buffer '(222) mode)
                     (setq successor-state gnosis-review--state))
                    ('kill
                     (kill-buffer owner)
                     (setq successor (get-buffer-create gnosis-review-buffer-name))
                     (set-buffer successor)))
                  (erase-buffer)
                  (insert "Successor draft")
                  (setq-local header-line-format "Successor header")
                  (use-local-map successor-map))))
          (unwind-protect
              (let ((gnosis-mode-hook (unless cancellation (list replace))))
                (when cancellation
                  (with-current-buffer owner
                    (setq gnosis-review--lookahead (list :job 'old))))
                (cl-letf (((symbol-function 'gnosis-model-cancel-preparation) replace)
                          ((symbol-function 'gnosis--read-string-with-input-method)
                           (lambda (&rest _) "old"))
                          ((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
                  (should-error
                   (if public
                       (gnosis-review-loop '(222) mode)
                     (gnosis-review--setup-buffer '(222) mode))
                   :type 'user-error))
                (should (equal before (gnosis-test-content--evidence)))
                (with-current-buffer successor
                  (should (eq successor-state gnosis-review--state))
                  (should (equal "Successor draft" (buffer-string)))
                  (should (equal "Successor header" header-line-format))
                  (should (eq successor-map (current-local-map)))))
            (dolist (buffer (delete-dups (list owner successor)))
              (when (buffer-live-p buffer)
                (with-current-buffer buffer (set-buffer-modified-p nil))
                (kill-buffer buffer)))))))))

(ert-deftest gnosis-active-owner-setup-native-mode-hook ()
  "A native major-mode hook replacement cannot publish state or accept grades."
  (gnosis-test-active-owner--setup-replacement 'mode nil))

(ert-deftest gnosis-active-owner-setup-native-mode-roundtrip ()
  "Returning to gnosis-mode does not restore the interrupted setup's owner."
  (gnosis-test-active-owner--setup-replacement 'roundtrip nil))

(ert-deftest gnosis-active-owner-setup-native-kill-hook ()
  "Killing setup's buffer cannot publish into a same-name successor."
  (gnosis-test-active-owner--setup-replacement 'kill nil))

(ert-deftest gnosis-active-owner-setup-native-mode-cancellation ()
  "Native mode replacement during cancellation preserves the successor."
  (gnosis-test-active-owner--setup-replacement 'mode t))

(ert-deftest gnosis-active-owner-setup-native-roundtrip-cancellation ()
  "Cancellation cannot restore setup ownership by reentering gnosis-mode."
  (gnosis-test-active-owner--setup-replacement 'roundtrip t))

(ert-deftest gnosis-active-owner-setup-native-kill-cancellation ()
  "Killing setup's owner during cancellation cannot write a new buffer."
  (gnosis-test-active-owner--setup-replacement 'kill t))

(ert-deftest gnosis-active-owner-setup-native-reentry-hook ()
  "A nested setup supersedes the setup still running its mode hooks."
  (gnosis-test-active-owner--setup-replacement 'reenter nil))

(ert-deftest gnosis-active-owner-setup-native-reentry-cancellation ()
  "A cancellation callback can replace setup without its state being erased."
  (gnosis-test-active-owner--setup-replacement 'reenter t))

(defun gnosis-test-active-owner--replace (kind)
  "Replace the current encounter with a successor of KIND."
  (pcase kind
    ('mode (fundamental-mode))
    ('roundtrip (fundamental-mode) (gnosis-mode))
    ('reenter (gnosis-review--setup-buffer '(222)))
    ('detach
     (set-visited-file-name (expand-file-name "successor.org" gnosis-dir) t)
     (set-visited-file-name nil t)))
  (erase-buffer)
  (insert "Successor draft")
  (setq-local header-line-format "Successor header")
  (use-local-map (make-sparse-keymap)))

(defun gnosis-test-active-owner--seed (mode)
  "Keep accepted evidence and an unfinished checkpoint for MODE."
  (gnosis-test-content--add "basic")
  (with-temp-buffer
    (gnosis-mode)
    (gnosis-test-content--state mode)
    (let ((answer (gnosis-test-content--answer "basic")))
      (gnosis-review-result 222 (car answer) (cdr answer)))
    (gnosis-test-content--state mode)))

(ert-deftest gnosis-active-owner-setup-minor-mode-body ()
  "Both native mode-body hooks must preserve successors and old evidence."
  (dolist (mode '(due practice))
    (dolist (hook '(read-only-mode-hook display-line-numbers-mode-hook))
      (dolist (kind '(mode roundtrip detach reenter))
        (gnosis-test-with-db
          (gnosis-test-active-owner--seed mode)
          (let* ((gnosis-review-buffer-name "*gnosis-body-owner-test*")
                 (owner (get-buffer-create gnosis-review-buffer-name))
                 (before (gnosis-test-content--evidence))
                 successor-map successor-state called)
            (unwind-protect
                (cl-progv (list hook)
                    (list (list (lambda ()
                                  (unless called
                                    (setq called t)
                                    (gnosis-test-active-owner--replace kind)
                                    (setq-local gnosis-center-content 'successor
                                                display-line-numbers 'relative)
                                    (setq successor-state gnosis-review--state
                                          successor-map (current-local-map))))))
                  (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                             (lambda (&rest _) "old"))
                            ((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
                    (should-error (gnosis-review-loop '(222) mode) :type 'user-error))
                  (should called)
                  (should (equal before (gnosis-test-content--evidence)))
                  (with-current-buffer owner
                    (should (eq successor-state gnosis-review--state))
                    (should (eq gnosis-center-content 'successor))
                    (should (eq display-line-numbers 'relative))
                    (should (equal "Successor draft" (buffer-string)))
                    (should (equal "Successor header" header-line-format))
                    (should (eq successor-map (current-local-map)))))
              (with-current-buffer owner (set-buffer-modified-p nil))
              (kill-buffer owner))))))))

(ert-deftest gnosis-active-owner-setup-minor-mode-positive ()
  "Unchanged native body hooks still run, and ordinary startup accepts Next."
  (dolist (mode '(due practice))
    (gnosis-test-with-db
      (gnosis-test-active-owner--seed mode)
      (let* ((gnosis-review-buffer-name "*gnosis-body-positive-test*")
             (schedule (gnosis-select '* 'scheduler-state))
             (calls nil)
             (read-only-mode-hook (list (lambda () (push 'read-only calls))))
             (display-line-numbers-mode-hook (list (lambda () (push 'numbers calls)))))
        (unwind-protect
            (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                       (lambda (&rest _) "old"))
                      ((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
              (gnosis-review-loop '(222) mode)
              (should (memq 'read-only calls))
              (should (memq 'numbers calls))
              (should (= 2 (length (gnosis-select '* (if (eq mode 'due)
                                                       'review-events 'practice-events)))))
              (when (eq mode 'practice)
                (should (equal schedule (gnosis-select '* 'scheduler-state)))))
          (when-let* ((buffer (get-buffer gnosis-review-buffer-name))) (kill-buffer buffer))
          (when-let* ((buffer (get-buffer "*Gnosis Study Summary*"))) (kill-buffer buffer)))))))

(ert-deftest gnosis-active-owner-self-grade-reveal ()
  "Stale SPC cannot reveal into successors or even start the second prompt."
  (dolist (mode '(due practice))
    (dolist (kind '(mode roundtrip detach))
      (gnosis-test-with-db
        (gnosis-test-active-owner--seed mode)
        (with-temp-buffer
          (gnosis-mode)
          (gnosis-test-content--state mode)
          (let ((gnosis-review-buffer-name (buffer-name))
                (gnosis-review-basic-input 'self-grade)
                (before (gnosis-test-content--evidence))
                successor-map second-prompt)
            (cl-letf (((symbol-function 'read-char-choice)
                       (lambda (prompt &rest _)
                         (if (string-prefix-p "Recall first" prompt)
                             (progn
                               (gnosis-test-active-owner--replace kind)
                               (setq successor-map (current-local-map))
                               ?\s)
                           (setq second-prompt t)
                           ?y))))
              (should-error (gnosis-review-basic 222) :type 'user-error))
            (should-not second-prompt)
            (should (equal before (gnosis-test-content--evidence)))
            (should (eq successor-map (current-local-map)))
            (should (equal "Successor header" header-line-format))
            (should (equal "Successor draft" (buffer-string)))
            (set-buffer-modified-p nil)))))))

(ert-deftest gnosis-active-owner-self-grade-positive ()
  "Same-owner SPC, yes and Next retain ordinary acceptance in both modes."
  (dolist (mode '(due practice))
    (gnosis-test-with-db
      (gnosis-test-active-owner--seed mode)
      (with-temp-buffer
        (gnosis-mode)
        (gnosis-test-content--state mode)
        (let ((gnosis-review-buffer-name (buffer-name))
              (gnosis-review-basic-input 'self-grade)
              (schedule (gnosis-select '* 'scheduler-state))
              prompts)
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (prompt &rest _)
                       (push prompt prompts)
                       (cond ((string-prefix-p "Recall first" prompt) ?\s)
                             ((string-prefix-p "Recalled" prompt) ?y)
                             (t ?n)))))
            (let ((answer (gnosis-review-basic 222)))
              (gnosis-review-actions (car answer) 222 (cdr answer))))
          (should (= 3 (length prompts)))
          (should (= 2 (length (gnosis-select '* (if (eq mode 'due)
                                                   'review-events 'practice-events)))))
          (when (eq mode 'practice)
            (should (equal schedule (gnosis-select '* 'scheduler-state)))))))))

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

(ert-deftest gnosis-active-owner-name-replacement-input ()
  "Feedback follows the captured owner, never a new same-name draft."
  (dolist (mode '(due practice))
    (dolist (kind '("basic" "mcq" "cloze" "mc-cloze" self-grade recall))
      (gnosis-test-with-db
        (gnosis-test-active-owner--seed mode)
        (when (stringp kind)
          (gnosis-update 'themata `(= type ,kind) '(= id 222))
          (unless (equal kind "basic")
            (gnosis-update 'themata '(= hypothesis '("old" "new")) '(= id 222))))
        (with-temp-buffer
          (gnosis-mode)
          (gnosis-test-content--state mode)
          (let* ((gnosis-review-buffer-name (buffer-name))
                 (gnosis-review-basic-input (if (symbolp kind) 'self-grade 'typed))
                 (before (gnosis-test-content--evidence))
                 draft map answer)
            (unwind-protect
                (cl-labels ((replace ()
                             (unless draft
                               (setq draft (gnosis-test-active-owner--rename)
                                     map (with-current-buffer draft (current-local-map))))))
                  (cl-letf (((symbol-function 'read-char-choice)
                             (lambda (prompt &rest _)
                               (when (if (eq kind 'recall)
                                         (string-prefix-p "Recalled" prompt)
                                       (string-prefix-p "Recall first" prompt))
                                 (replace))
                               (if (string-prefix-p "Recall first" prompt) ?\s ?y))))
                    (setq answer
                          (if (symbolp kind) (gnosis-review-basic 222)
                            (gnosis-test-content--answer kind #'replace))))
                  (should draft)
                  (gnosis-test-active-owner--draft-unchanged draft map)
                  (should (equal before (gnosis-test-content--evidence)))
                  (should (string-match-p "\\(?:Next review:\\|Practice: schedule unchanged\\)"
                                          (buffer-string)))
                  (gnosis-review-result 222 (car answer) (cdr answer))
                  (let ((accepted (gnosis-test-content--evidence)))
                    (gnosis-review-result 222 (car answer) (cdr answer))
                    (should (equal accepted (gnosis-test-content--evidence)))))
              (when (buffer-live-p draft) (kill-buffer draft)))))))))

(ert-deftest gnosis-active-owner-name-replacement-actions ()
  "Override and Next use the captured owner after action-time renaming."
  (dolist (mode '(due practice))
    (dolist (override '(nil t))
      (gnosis-test-with-db
        (gnosis-test-active-owner--seed mode)
        (with-temp-buffer
          (gnosis-mode)
          (gnosis-test-content--state mode)
          (let* ((gnosis-review-buffer-name (buffer-name))
                 (answer (gnosis-test-content--answer "basic"))
                 (before (gnosis-test-content--evidence))
                 (schedule (gnosis-select '* 'scheduler-state))
                 draft map)
            (unwind-protect
                (cl-letf (((symbol-function 'read-char-choice)
                           (lambda (&rest _)
                             (should (equal before (gnosis-test-content--evidence)))
                             (if draft ?n
                               (setq draft (gnosis-test-active-owner--rename)
                                     map (with-current-buffer draft (current-local-map)))
                               (if override ?o ?n)))))
                  (gnosis-review-actions (car answer) 222 (cdr answer))
                  (gnosis-test-active-owner--draft-unchanged draft map)
                  (should (= 2 (length (gnosis-select '* (if (eq mode 'due)
                                                           'review-events 'practice-events)))))
                  (should (equal (if override '(222) nil)
                                 (gnosis-review-state-remaining gnosis-review--state)))
                  (when (eq mode 'practice)
                    (should (equal schedule (gnosis-select '* 'scheduler-state)))))
              (when (buffer-live-p draft) (kill-buffer draft)))))))))

(ert-deftest gnosis-active-owner-name-replacement-navigation ()
  "Next presents the next card in the original, renamed review buffer."
  (dolist (mode '(due practice))
    (gnosis-test-with-db
      (gnosis-test-active-owner--seed mode)
      (gnosis-add-thema-fields "basic" "Second question" nil '("old")
                               "Second explanation" nil 0 nil nil 223)
      (let ((gnosis-review-buffer-name "*gnosis-navigation-test*")
            owner draft map (prompts 0))
        (unwind-protect
            (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                       (lambda (&rest _)
                         (cl-incf prompts)
                         (if owner
                             (should (eq owner (current-buffer)))
                           (setq owner (current-buffer)
                                 draft (gnosis-test-active-owner--rename)
                                 map (with-current-buffer draft (current-local-map))))
                         "old"))
                      ((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
              (gnosis-review-loop '(222 223) mode)
              (should (= prompts 2))
              (gnosis-test-active-owner--draft-unchanged draft map)
              (with-current-buffer owner
                (should (string-match-p "Second question" (buffer-string))))
              (should (= 3 (length (gnosis-select '* (if (eq mode 'due)
                                                       'review-events 'practice-events))))))
          (dolist (buffer (list owner draft (get-buffer "*Gnosis Study Summary*")))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest gnosis-active-owner-standalone-display-destination ()
  "Standalone renderers retain their configured destination contract."
  (let* ((buffer (generate-new-buffer "*gnosis-standalone-display*"))
         (gnosis-review-buffer-name (buffer-name buffer)))
    (unwind-protect
        (with-temp-buffer
          (gnosis-display-keimenon "Standalone question")
          (gnosis-display-basic-answer "Standalone answer" t "")
          (gnosis-display-next-review nil t)
          (should (zerop (buffer-size)))
          (with-current-buffer buffer
            (should (string-match-p "Standalone question" (buffer-string)))
            (should (string-match-p "Standalone answer" (buffer-string)))
            (should (string-match-p "Practice: schedule unchanged" (buffer-string)))))
      (kill-buffer buffer))))

(defun gnosis-test-active-owner--file-navigation (mode kind mutation &optional native)
  "Exercise MODE and KIND navigation across MUTATION using NATIVE input."
  (gnosis-test-with-db
    (save-window-excursion
      (gnosis-test-active-owner--seed mode)
      (let* ((image (gnosis-test-image--file))
             (gnosis-review-buffer-name "*gnosis-file-navigation-test*")
             (owner (gnosis-review--setup-buffer '(222) mode))
             (positive (memq mutation '(unchanged rename)))
             before hook-ran entered failure answer draft map successor)
        (unwind-protect
            (progn
              (switch-to-buffer owner)
              (gnosis-test-content--state mode)
              (gnosis-update 'themata `(= type ,kind) '(= id 222))
              (when (equal kind "mcq")
                (gnosis-update 'themata '(= hypothesis '("old" "new")) '(= id 222)))
              (gnosis-update 'themata
                             `(= keimenon ,(format "Old question [file:%s]" image))
                             '(= id 222))
              (setq before (gnosis-test-content--evidence))
              (should (nth (if (eq mode 'due) 0 1) before))
              (should (nth 3 before))
              (let ((org-mode-hook
                     (cons (lambda ()
                             (when (and (eq mutation 'format) hook-ran)
                               (with-current-buffer owner
                                 (unless gnosis-review--retired
                                   (gnosis-test-active-owner--repurpose t)
                                   (setq map (current-local-map))))))
                           org-mode-hook))
                    (buffer-list-update-hook
                     (cons (lambda ()
                             (when (and (eq mutation 'return) hook-ran
                                        (eq (current-buffer) owner)
                                        (not gnosis-review--retired))
                               (gnosis-test-active-owner--repurpose t)
                               (setq map (current-local-map))))
                           buffer-list-update-hook))
                    (find-file-hook
                     (cons
                      (lambda ()
                        (when (and (not hook-ran) (equal buffer-file-name image))
                          (setq hook-ran t)
                          (with-current-buffer owner
                            (pcase mutation
                              ((or 'associated 'detached)
                               (gnosis-test-active-owner--repurpose (eq mutation 'detached)))
                              ((or 'mode 'mode-restore)
                               (let ((state gnosis-review--state))
                                 (fundamental-mode)
                                 (gnosis-mode)
                                 (when (eq mutation 'mode-restore)
                                   (setq gnosis-review--state state))))
                              ((or 'setup 'setup-restore)
                               (let ((state gnosis-review--state))
                                 (gnosis-review--setup-buffer '(222) mode)
                                 (when (eq mutation 'setup-restore)
                                   (setq gnosis-review--state state))))
                              ('content
                               (gnosis-update 'themata '(= keimenon "Changed question") '(= id 222)))
                              ('rename (setq draft (gnosis-test-active-owner--rename))))
                            (unless (or positive (memq mutation '(return format)))
                              (let ((inhibit-read-only t))
                                (erase-buffer)
                                (insert "Successor unsaved text"))
                              (setq-local header-line-format "Successor header")
                              (use-local-map (make-sparse-keymap)))
                            (setq successor (or draft owner)
                                  map (with-current-buffer successor (current-local-map))))))
                      find-file-hook)))
                (cl-labels ((run ()
                             (condition-case err
                                 (setq answer (funcall (intern (concat "gnosis-review-" kind)) 222))
                               (user-error (setq failure err)))))
                  (if native
                      (let ((minibuffer-setup-hook
                             (cons (lambda () (setq entered t)) minibuffer-setup-hook)))
                        (run))
                    (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                               (lambda (&rest _) (setq entered t) "old"))
                              ((symbol-function 'gnosis-completing-read)
                               (lambda (&rest _) (setq entered t) "old")))
                      (run)))))
              (should hook-ran)
              (should (equal before (gnosis-test-content--evidence)))
              (with-current-buffer owner
                (if positive
                    (progn
                      (should-not failure)
                      (should entered)
                      (should (car answer))
                      (should (string-match-p "Old question" (buffer-string)))
                      (when draft (gnosis-test-active-owner--draft-unchanged draft map))
                      (gnosis-review-result 222 (car answer) (cdr answer))
                      (should (= 2 (length (gnosis-select '* (if (eq mode 'due)
                                                               'review-events 'practice-events)))))
                      (when (eq mode 'practice)
                        (should (equal (nth 2 before) (nth 2 (gnosis-test-content--evidence))))))
                  (should failure)
                  (should-not entered)
                  (should (equal "Successor unsaved text" (buffer-string)))
                  (should (eq map (current-local-map)))
                  (should (equal "Successor header" header-line-format)))))
          (dolist (buffer (list owner draft (get-file-buffer image)))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest gnosis-active-owner-file-navigation-retirement ()
  "File hooks cannot retire ownership then render or start a reader."
  (dolist (mode '(due practice))
    (dolist (kind '("basic" "mcq"))
      (dolist (mutation '(associated detached mode mode-restore setup setup-restore content return format))
        (gnosis-test-active-owner--file-navigation mode kind mutation)))))

(ert-deftest gnosis-active-owner-file-navigation-positive ()
  "Unchanged and renamed owners survive native file hooks and accept normally."
  (dolist (mode '(due practice))
    (dolist (kind '("basic" "mcq"))
      (dolist (mutation '(unchanged rename))
        (gnosis-test-active-owner--file-navigation mode kind mutation)))))

(ert-deftest gnosis-active-owner-standalone-image-navigation ()
  "Standalone image display keeps the original destination through renaming."
  (gnosis-test-with-db
    (save-window-excursion
      (let* ((image (gnosis-test-image--file))
             (owner (generate-new-buffer "*gnosis-standalone-image*"))
             (gnosis-review-buffer-name (buffer-name owner))
             draft map)
        (unwind-protect
            (let ((find-file-hook
                   (cons (lambda ()
                           (when (equal buffer-file-name image)
                             (with-current-buffer owner
                               (setq draft (gnosis-test-active-owner--rename)
                                     map (with-current-buffer draft (current-local-map))))))
                         find-file-hook)))
              (gnosis-display-image (format "[file:%s]" image))
              (should (eq (current-buffer) owner))
              (gnosis-test-active-owner--draft-unchanged draft map))
          (dolist (buffer (list owner draft (get-file-buffer image)))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(provide 'gnosis-test-review-active-owner)
;;; gnosis-test-review-active-owner.el ends here
