;;; vc-jj.el --- A vc.el backend for Jujutsu VCS  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Wojciech Siewierski

;; Author: Wojciech Siewierski

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A backend for vc.el to handle Jujutsu repositories.

;;; Code:

(autoload 'vc-setup-buffer "vc-dispatcher")
(autoload 'vc-switches "vc")

(add-to-list 'vc-handled-backends 'JJ)

(defun vc-jj-revision-granularity () 'repository)
(defun vc-jj-checkout-model (_files) 'implicit)
(defun vc-jj-update-on-retrieve-tag () nil)


(defun vc-jj--file-tracked (file)
  (with-temp-buffer
    (and (= 0 (call-process "jj" nil t nil "file" "list" "--" file))
         (not (= (point-min) (point-max))))))

(defun vc-jj--file-modified (file)
  (with-temp-buffer
    (and (= 0 (call-process "jj" nil t nil "diff" "--name-only" "--" file))
         (not (= (point-min) (point-max))))))


;;;###autoload (defun vc-jj-registered (file)
;;;###autoload   "Return non-nil if FILE is registered with jj."
;;;###autoload   (if (vc-find-root file ".jj")       ; Short cut.
;;;###autoload       (progn
;;;###autoload         (load "vc-jj" nil t)
;;;###autoload         (vc-jj-registered file))))

(defun vc-jj-registered (file)
  (when-let ((root (vc-jj-root file)))
    (let ((relative (file-relative-name file root))
          (default-directory root))
      (vc-jj--file-tracked relative))))

(defun vc-jj-state (file)
  (when-let ((root (vc-jj-root file)))
    (let ((relative (file-relative-name file root))
          (default-directory root))
      (cond
       ((vc-jj--file-modified relative)
        'edited)
       ((vc-jj--file-tracked relative)
        'up-to-date)))))

(defun vc-jj-dir-status-files (dir _files update-function)
  ;; TODO: should be async!
  (let ((files (apply #'process-lines "jj" "file" "list" "--" dir))
        (modified (apply #'process-lines "jj" "diff" "--name-only" "--" dir)))
    (mapcar (lambda (file)
              (let ((vc-state (if (member file modified)
                                  'edited
                                'up-to-date)))
                (list file vc-state))))))

(defun vc-jj-working-revision (file)
  (when-let ((root (vc-jj-root file)))
    (let ((relative (file-relative-name file root))
          (default-directory root))
      (let ((rev (if (vc-jj--file-modified relative)
                     "@"
                   "@-")))
        (car (process-lines "jj" "log" "--no-graph"
                            "-r" rev
                            "-T" "self.change_id().short() ++ \"\\n\""))))))

(defun vc-jj-create-repo ()
  (if current-prefix-arg
      (call-process "jj" nil nil nil "git" "init" "--colocate")
    (call-process "jj" nil nil nil "git" "init")))

(defun vc-jj-register (_files &optional _comment)
  ;; No action needed.
  )

(defun vc-jj-checkin (files comment &optional _rev)
  (setq comment (replace-regexp-in-string "\\`Summary: " "" comment))
  (let ((args (append (vc-switches 'jj 'checkin) (list "--") files)))
    (apply #'call-process "jj" nil nil nil "commit" "-m" comment "--" args)))

(defun vc-jj-find-revision (file rev buffer)
  (call-process "jj" nil buffer nil "file" "show" "-r" rev "--" file))

(defun vc-jj-checkout (file &optional rev)
  (let ((args (if rev
                  (list "--from" rev "--" file)
                (list "--" file))))
    (call-process "jj" nil nil nil "restore" args)))

(defun vc-jj-revert (file &optional _contents-done)
  (call-process "jj" nil nil nil "restore" "--" file))

(defun vc-jj-print-log (files buffer &optional _shortlog start-revision limit)
  (vc-setup-buffer buffer)
  (let ((inhibit-read-only t)
        (args (append
               (when limit
                 (list "-n" (number-to-string limit)))
               (when start-revision
                 (list "-r" (concat ".." start-revision)))
               (list "--")
               files)))
    (apply #'call-process "jj" nil buffer nil "log" args))
  (goto-char (point-min)))

(defun vc-jj-show-log-entry (revision)
  (goto-char (point-min))
  (when (search-forward-regexp
         (concat "^[^|]\\s-+\\(" (regexp-quote revision) "\\)\\s-+")
         nil t)
    (goto-char (match-beginning 1))))

;; (defun vc-jj-log-outgoing (buffer remote-location)
;;   ;; TODO
;;   )
;; (defun vc-jj-log-incoming (buffer remote-location)
;;   ;; TODO
;;   )

(defun vc-jj-root (_file)
  (with-temp-buffer
    (when (= 0 (call-process "jj" nil (list t nil) nil "root"))
      (buffer-substring (point-min) (1- (point-max))))))

(defalias 'vc-jj-responsible-p #'vc-jj-root)

(defun vc-jj-find-ignore-file (file)
  (expand-file-name ".gitignore"
		            (vc-jj-root file)))


(defvar vc-jj-diff-switches '("--git"))

(defun vc-jj-diff (files &optional rev1 rev2 buffer async)
  (setq buffer (or buffer "*vc-diff*"))
  (cond
   ((and (null rev1)
         (null rev2))
    (setq rev1 "@-"))
   ((null rev1)
    (setq rev1 "root()")))
  (setq rev2 (or rev2 "@"))
  (let ((inhibit-read-only t)
        (args (append (vc-switches 'jj 'diff) (list "--") files)))
    (apply #'call-process "jj" nil buffer nil "diff" "--from" rev1 "--to" rev2 args)))

(defun vc-jj-revision-completion-table (files)
  (let ((revisions
         (apply #'process-lines
                "jj" "log" "--no-graph"
                "-T" "self.change_id() ++ \"\\n\"" "--" files)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata . ((display-sort-function . ,#'identity)))
        (complete-with-action action revisions string pred)))))


(provide 'vc-jj)
;;; vc-jj.el ends here
