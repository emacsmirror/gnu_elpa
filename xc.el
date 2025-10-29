;; ex.el -- Emacs integration with the xc task runner

;; This file is not a part of GNU Emacs

(defcustom xc-executable nil
  "The executable of the xc task runner

Either a string containing the name or the path of the executable, or
nil to use the default `xc'.")

(defcustom xc-type nil
  "Type of documentation xc should look for

Either `org' or `md', or nil for default.")

(defun xc--run (args)
  (with-temp-buffer
    (let* ((xc (or xc-executable "xc"))
           (command (concat xc " " args))
           (exit-code (call-process-shell-command command nil (current-buffer) t)))
      (if (zerop exit-code)
          (buffer-string)
        nil))))

(defun xc--tasks (&optional file)
  "Get available tasks using xc"
  (when-let ((xc-output (thread-first
                          "-short"
                          (concat
                           (and xc-type (concat " -type " xc-type))
                           (and file (concat " -file " file)))
                          xc--run)))
    (split-string xc-output)))

(defun xc (task)
  "Run TASK using `xc'"
  (interactive (list (completing-read "Task: " (xc--tasks))))
  (let* ((output-buffer (concat "*xc-" task "*")))
    (thread-first xc-executable
                  (concat " " task
                          (and xc-type (concat " -type " xc-type)))
                  (async-shell-command output-buffer))
    (pop-to-buffer output-buffer)))

(defun xc-treemacs-RET-task-action (&optional _)
  "Run the task associated with the Treemacs button at point"
  (let* ((btn (treemacs-current-button))
         (btn-path (treemacs-button-get btn :path))
         (default-directory (nth 0 btn-path))
         (task-name (treemacs-button-get btn :key)))
    (xc task-name)))

(defun xc-treemacs-DBLCLICK-task-action (&optional _)
  "Run the task associated with the Treemacs button at point"
  (xc-treemacs-RET-task-action))

(defun xc--treemacs-get-tasks (&optional _)
  "Get available tasks for the Treemacs project at point"
  (let* ((btn (treemacs-current-button))
         (btn-path (treemacs-button-get btn :path))
         (default-directory (nth 0 btn-path)))
    (xc--tasks)))

(defun xc--treemacs-tasks-p (&optional _)
  (let* ((btn (treemacs-current-button))
         (btn-path (treemacs-button-get btn :path))
         (default-directory btn-path))
    (xc--tasks)))

(defun xc--treemacs-init ()
  "Initialize xc-Treemacs integration"
  (require 'treemacs-treelib)
  (treemacs-define-entry-node-type xc-tasks
    :key 'xc-tasks
    :label "[XC TASKS]"
    :open-icon (treemacs-get-icon-value 'list)
    :closed-icon (treemacs-get-icon-value 'list)
    :children (xc--treemacs-get-tasks)
    :child-type 'xc-task-node)

  (treemacs-define-leaf-node-type xc-task-node
    :icon "• "
    :label (propertize item 'face 'font-lock-string-face)
    :key item
    :ret-action #'xc-treemacs-RET-task-action
    :double-click-action #'xc-treemacs-DBLCLICK-task-action)

  (treemacs-enable-project-extension
   :extension 'xc-tasks
   :position 'top
   :predicate #'xc--treemacs-tasks-p))
