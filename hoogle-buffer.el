;;; hoogle-buffer.el --- Hoogle in a buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Created: February 06, 2024
;; License: GPL-3.0-or-later
;; Version: 0.6.0
;; Keywords: docs languages
;; Homepage: https://codeberg.org/rahguzar/consult-hoogle
;; Package-Requires: ((emacs "27.1"))

;; This file is part of GNU Emacs.

;;; Commentary:

;; Display results of a hoogle search in a buffer.

;;; Code:
(require 'hoogle-base)
(require 'url-handlers)

;;; Internal variables
(defconst hoogle-buffer--url-format
  "https://hoogle.haskell.org/?mode=json&hoogle=%s&start=1&count=50"
  "Url format for using hoogle at hoogle.haskell.org .")

(defvar hoogle-buffer--fontification-buffer nil
  "Indirect buffer used for fontification in a hoogle buffer.")
(put 'hoogle-buffer--fontification-buffer 'permanent-local t)

(defvar hoogle-buffer--query nil)

;;; Keymap
(defvar hoogle-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap
                            hoogle-base-map special-mode-map))
    (define-key map "s" #'hoogle-buffer)
    (define-key map "p" #'hoogle-buffer-project)
    (define-key map "w" #'hoogle-buffer-web)
    map))

;;; Major mode
(defun hoogle-buffer--candidate ()
  "Obtain the current hoogle candidate."
 (get-text-property (point) 'hoogle-result))

(defun hoogle-buffer-modify-query (fun)
  "Run a search with current query modified by FUN."
  (let ((hoogle-base-args (car hoogle-buffer--query)))
    (hoogle-buffer (funcall fun (cdr hoogle-buffer--query))
                   (current-buffer))))

(define-derived-mode hoogle-buffer-mode special-mode "Hoogle"
  "Major mode for displaying Hoogle search results."
  :syntax-table nil :abbrev-table nil :interactive nil
  (setq line-prefix "  ")
  (setq wrap-prefix "  ")
  (setq-local jit-lock-functions '(hoogle-buffer--fontify)
              outline-regexp (rx "§" (* whitespace))
              outline-minor-mode-cycle t
              outline-minor-mode-highlight nil
              hoogle-base-find-candidate #'hoogle-buffer--candidate
              hoogle-base-modify-query-function #'hoogle-buffer-modify-query)
  (hoogle-buffer--setup-fontification-buffer)
  (jit-lock-mode t)
  (outline-minor-mode)
  (visual-line-mode))

;;;; Fontification
(defun hoogle-buffer--pre (dom)
  "Mark rendering of DOM with pre tag as code."
  (let ((start (point))
        (shr-current-font 'default))
    (shr-tag-pre dom)
    (put-text-property start (point) 'hoogle-code t)
    (insert "\n")))

(defun hoogle-buffer--code (dom)
  "Mark rendering of DOM with pre tag as code."
  (let ((start (point)))
    (shr-tag-code dom)
    (put-text-property start (point) 'hoogle-code t)))

(defun hoogle-buffer--shr-renderers ()
  "Return the value of `shr-external-rendering-functions' to be used."
  (let ((renderers shr-external-rendering-functions))
    (push `(pre . hoogle-buffer--pre) renderers)
    (push `(code . hoogle-buffer--code) renderers)
    renderers))

(defun hoogle-buffer--setup-fontification-buffer ()
  "Setup the indirect buffer used for fontification."
  (when (buffer-live-p hoogle-buffer--fontification-buffer)
    (with-current-buffer hoogle-buffer--fontification-buffer
      (setq-local delay-mode-hooks t)
      (hoogle-base--haskell-mode)
      (setq-local font-lock-dont-widen t
                  font-lock-support-mode nil
                  font-lock-global-modes nil)
      (current-buffer))))

(defun hoogle-buffer--get-fontification-buffer ()
  "Get the indirect buffer used for fontification."
  (if (buffer-live-p hoogle-buffer--fontification-buffer)
      hoogle-buffer--fontification-buffer
    (setq-local hoogle-buffer--fontification-buffer
                (make-indirect-buffer (current-buffer)
                                      (generate-new-buffer-name
                                       (concat (buffer-name) "--fontification"))
                                      nil t))
    (hoogle-buffer--setup-fontification-buffer)))

(defun hoogle-buffer--fontify (beg end)
  "Fontify current hoogle buffer from BEG to END."
  (goto-char end)
  (setq end (line-end-position))
  (goto-char beg)
  (beginning-of-line)
  (while (re-search-forward
          (rx bol (? "§ ") (group (or "Package" "Module")) ": " (group (* nonl)))
          end t)
    (put-text-property (match-beginning 2) (match-end 2) 'face
                       (if (equal (match-string 1) "Package")
                           'haskell-quasi-quote-face
                         'haskell-keyword-face)))
  (when (get-text-property beg 'hoogle-code)
    (setq beg (previous-single-property-change beg 'hoogle-code nil (point-min))))
  (when (get-text-property end 'hoogle-code)
    (setq end (next-single-property-change end 'hoogle-code nil (point-max))))
  (with-current-buffer (hoogle-buffer--get-fontification-buffer)
    (goto-char beg)
    (let ((beg beg))
      (while (setq beg (text-property-not-all (point) end 'hoogle-code nil))
        (goto-char (or (text-property-any beg end 'hoogle-code nil) end))
        (narrow-to-region beg (point))
        (font-lock-fontify-region beg (point))
        (goto-char (point-max))
        (widen))))
  `(jit-lock-bounds ,beg . ,end))

;;; Process handling
(defun hoogle-buffer--insert-results (proc)
  "Insert available results from PROC into results buffer."
  (goto-char (point-min))
  (let ((shr-external-rendering-functions (hoogle-buffer--shr-renderers)))
    (while (not (eobp))
      (set-marker (process-mark proc)
                  (min (+ 2 (line-end-position)) (point-max)))
      (when-let ((result (ignore-errors
                           (json-parse-string
                            (delete-and-extract-region
                             (line-beginning-position) (line-end-position))
                            :object-type 'alist)))
                 (start (point)))
        (insert (propertize "§ " 'face 'bold 'line-prefix ""))
        (hoogle-base--details result)
        (goto-char (- (process-mark proc) 2))
        (end-of-line)
        (put-text-property start (point) 'hoogle-result result)
        (insert "\n"))
      (goto-char (process-mark proc))))
  (goto-char (point-min))
  (while (re-search-forward (rx "\n" (>= 2 (: (* whitespace) "\n"))) nil t)
    (replace-match "\n\n"))
  (let ((inhibit-read-only t))
    (with-current-buffer (process-get proc 'results-buffer)
      (save-excursion
        (goto-char (point-max))
        (insert-buffer-substring (process-buffer proc)))))
  (delete-region (point-min) (point-max)))

(defun hoogle-buffer--filter (proc string)
  "Handle output STRING from hoogle process PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (widen)
      (goto-char (point-max))
      (insert string)
      (search-backward "\n" nil t)
      (narrow-to-region (point-min) (1+ (point)))
      (hoogle-buffer--insert-results proc))))

(defun hoogle-buffer--sentinel (proc status)
  "Sentinel for hoogle process PROC, handling STATUS."
  (pcase status
    ("finished\n"
     (when (buffer-live-p (process-buffer proc))
       (with-current-buffer (process-buffer proc)
         (hoogle-buffer--insert-results proc)
         (kill-buffer))))
    ((rx (or "failed" "connection broken" "(core dumped)" "exited abnormally"))
     (message "hoogle process error :%s" status))))

;;; Url Callback
(defun hoogle-buffer--url-results (buffer status)
  "Return results by parsing a hoogle url from BUFFER response with STATUS."
  (unwind-protect
      (if-let ((err (plist-get :error status)))
          (error "Error %s in retrieving hoogle results: %S" (car err) (cdr err))
        (with-temp-buffer
          (erase-buffer)
          (set-buffer-multibyte t)
          (url-insert buffer)
          (decode-coding-region (point-min) (point-max) 'utf-8)
          (goto-char (point-min))
          (json-parse-buffer :object-type 'alist :array-type 'list)))
    (kill-buffer buffer)))

(defun hoogle-buffer--render-html (str)
  "Render the html string STR."
  (with-temp-buffer
    (insert str)
    (shr-render-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun hoogle-buffer--url-callback (status results-buffer)
  "Callback for `url-queue-retrieve' to insert results into RESULTS-BUFFER.
See `url-retrieve' for STATUS."
  (let ((url-buffer (current-buffer))
        (inhibit-read-only t)
        (shr-external-rendering-functions (hoogle-buffer--shr-renderers)))
    (with-current-buffer results-buffer
      (save-excursion
        (goto-char (point-max))
        (dolist (result (hoogle-buffer--url-results url-buffer status))
          (cl-callf hoogle-buffer--render-html (alist-get 'item result))
          (insert (propertize "§ " 'face 'bold 'line-prefix ""))
          (let ((start (point)))
            (hoogle-base--details result)
            (goto-char (point-max))
            (put-text-property (- start 2) (point) 'hoogle-result result))
          (insert "\n\n"))
        (goto-char (point-min))
        (while (re-search-forward
                (rx "\n" (>= 2 (: (* whitespace) "\n"))) nil t)
          (replace-match "\n\n")))
      (pop-to-buffer results-buffer))))

;;; Interactive Commands
;;;###autoload
(defun hoogle-buffer (query results-buffer)
  "Display hoogle search results for QUERY in RESULTS-BUFFER.
By default uses local hoogle executable if it is found. Otherwise hoogle at
hoogle.haskell.org is used. See `hoogle-buffer-args' for customization."
  (interactive (list (read-string "Hoogle: ")
                     (get-buffer-create "*hoogle-search*")))
  (let ((inhibit-read-only t))
    (with-current-buffer results-buffer
      (erase-buffer)
      (hoogle-buffer-mode)
      (setq-local hoogle-buffer--query (cons hoogle-base-args query))))
  (if hoogle-base-args
      (let* ((proc-buffer (get-buffer-create "*hoogle-process*"))
             (proc (make-process :name "*hoogle-process*" :buffer proc-buffer
                                 :noquery t :connection-type 'pipe
                                 :command `(,@hoogle-base-args ,query)
                                 :filter #'hoogle-buffer--filter
                                 :sentinel #'hoogle-buffer--sentinel)))
        (process-put proc 'results-buffer results-buffer)
        (pop-to-buffer results-buffer))
    (url-queue-retrieve (format hoogle-buffer--url-format (url-hexify-string query))
                        #'hoogle-buffer--url-callback
                        `(,results-buffer) t)))

(defun hoogle-buffer-project (query results-buffer)
  "Display search results for QUERY in RESULTS-BUFFER for current project.
By default uses cabal-hoogle and the database should have been generated
by running `cabal-hoogle generate'.  `hoogle-buffer-project-args' can be
customized to configure an alternate command."
  (interactive (list (read-string "Hoogle: ")
                     (get-buffer-create "*hoogle-search*")))
  (let ((hoogle-base-args hoogle-base-project-args)
        (default-directory (funcall hoogle-base-project-root-function)))
    (hoogle-buffer query results-buffer)))

(defun hoogle-buffer-web (query results-buffer)
  "Display hoogle search results for QUERY in RESULTS-BUFFER.
Results are obtained by querying hoogle.haskell.org ."
  (interactive (list (read-string "Hoogle: ")
                     (get-buffer-create "*hoogle-search*")))
  (let (hoogle-base-args)
    (hoogle-buffer query results-buffer)))

(provide 'hoogle-buffer)
;;; hoogle-buffer.el ends here
