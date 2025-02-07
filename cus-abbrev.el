;;; cus-abbrev.el --- Easy Customization interface for Abbrevs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Mauro Aranda

;; Author: Mauro Aranda <maurooaranda@gmail.com>
;; Maintainer: Mauro Aranda <maurooaranda@gmail.com>
;; Created: Tue Jan 21 19:59:28 2025
;; Version: 0.1
;; Package-Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; URL: https://gitlab.com/mauroaranda/cus-abbrev
;; Keywords: convenience

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
;; The following code provides an Easy Customization interface to manage
;; abbrevs.
;; The main command are `customize-abbrevs' and `customize-all-abbrevs'.
;; It presents a Custom-like buffer with abbrevs to edit.

;;; Code:
(require 'cus-edit)

(defgroup cus-abbrev nil
  "Customize Abbrevs with a Customize-like interface."
  :group 'customize)

(defcustom custom-abbrev-file-name nil
  "File to use when saving abbrevs.

If nil, this is the same as `abbrev-file-name'."
  :type '(choice (const :tag "Default abbrev file" nil) (file)))

(defvar-local custom-abbrev-widgets nil
  "List of widgets that hold the abbrev customizations.")

(defvar-keymap custom-abbrev-map
  :doc "Keymap used in buffers for Customizing Abbrevs."
  :full t
  :parent widget-keymap
  "SPC"     #'scroll-up-command
  "S-SPC"   #'scroll-down-command
  "DEL"     #'scroll-down-command
  "C-x C-s" #'Custom-abbrev-save
  "q"       #'Custom-buffer-done
  "n"       #'widget-forward
  "p"       #'widget-backward)

(defvar custom-abbrev-commands
  '((" Save Abbrevs " Custom-abbrev-save t
     "Save Abbrevs to the abbrevs file." "save" "Save" t)
    (" Undo Edits " Custom-abbrev-revert-buffer t
     "Revert buffer, undoing any editions."
     "refresh" "Undo" t)
    (" Help for Customize " Custom-help t "Get help for using Customize."
     "help" "Help" t)
    (" Exit " Custom-buffer-done t "Exit Customize." "exit" "Exit" t))
  "Alist of specifications for Customize menu items, tool bar icons and buttons.
See `custom-commands' for further explanation.")

(easy-menu-define
  Custom-abbrev-menu (list custom-abbrev-map)
  "Menu used in buffers for Customizing Abbrevs."
  (nconc (list "Custom"
               (customize-menu-create 'customize))
         (mapcar (lambda (arg)
                   (let ((tag     (nth 0 arg))
                         (command (nth 1 arg))
                         (visible (nth 2 arg))
                         (help    (nth 3 arg))
                         (active  (nth 6 arg)))
                     (vector tag command :visible (eval visible)
                             :active `(eq t ',active)
                             :help help)))
                 custom-abbrev-commands)))

(defvar custom-abbrev-tool-bar-map nil
  "Keymap for the toolbar in buffers for Customizing Abbrevs.")

(define-widget 'custom-abbrev 'editable-list
  "An editable list to edit abbrevs."
  :entry-format "%i %d %v"
  :insert-button-args '(:help-echo "Insert new abbrev here.")
  :append-button-args '(:help-echo "Append new abbrev here.")
  :delete-button-args '(:help-echo "Delete this abbrev."))

(defun Custom-abbrev-define (&rest _ignored)
  "Define abbrevs in current buffer, without saving them."
  (interactive)
  (dolist (widget custom-abbrev-widgets)
    (let ((table (symbol-value (widget-get widget :custom-abbrev-table))))
      (clear-abbrev-table table)
      (dolist (abbrev-widget (widget-get widget :children))
        (let ((abbrev (widget-value abbrev-widget)))
          (define-abbrev table (nth 0 abbrev) (nth 1 abbrev)
            (nth 2 abbrev)
            :enable-function (nth 3 abbrev)
            :case-fixed (nth 4 abbrev))))))
  (message "Abbrevs defined, but not saved"))

(defun Custom-abbrev-revert-buffer (&rest _ignored)
  "Revert the buffer for customizing abbrevs."
  (interactive)
  (if (>= (length custom-abbrev-widgets) 2)
      (customize-all-abbrevs)
    (customize-abbrevs (widget-get (car custom-abbrev-widgets)
                                   :custom-abbrev-table))))

(defun Custom-abbrev-save (&rest _ignored)
  "Save all abbrevs currently being edited.

This command also saves any other editions made to the abbrev table."
  (interactive)
  (Custom-abbrev-define)
  (write-abbrev-file (or custom-abbrev-file-name) abbrev-file-name)
  (message (format "Abbrevs saved to %s" (or custom-abbrev-file-name
                                             abbrev-file-name))))

(defun custom-abbrev--prepare-buffer (buffer-name)
  "Prepare buffer called BUFFER-NAME for Customizing Abbrevs."
  (switch-to-buffer buffer-name)
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (custom--initialize-widget-variables))

(defun custom-abbrev--prepare-buffer-2 ()
  "Finalize preparations for Customizing Abbrevs in current buffer."
  (goto-char (point-min))
  (setq-local tool-bar-map
              (or custom-abbrev-tool-bar-map
                  ;; Set up `custom-abbrev-tool-bar-map'.
                  (let ((map (make-sparse-keymap)))
                    (mapc
                     (lambda (arg)
                       (tool-bar-local-item-from-menu
                        (nth 1 arg) (nth 4 arg) map custom-abbrev-map
                        :label (nth 5 arg)))
                     custom-abbrev-commands)
                    (setq custom-abbrev-tool-bar-map map))))
  (setq-local revert-buffer-function #'Custom-abbrev-revert-buffer)
  (use-local-map custom-abbrev-map)
  (widget-setup))

(defun custom-abbrev--insert-buttons ()
  "Insert buttons for Customizing Abbrevs in current buffer."
  (widget-insert "Global actions:\n")
  (widget-create 'push-button :tag " Revert "
                 :help-echo "Revert buffer, discarding all edits."
                 :action #'Custom-abbrev-revert-buffer)
  (widget-insert " ")
  (widget-create 'push-button :tag " Define Abbrevs "
                 :help-echo "Define abbrevs for this session, without saving."
                 :action #'Custom-abbrev-define)
  (widget-insert " ")
  (widget-create 'push-button :tag " Save Abbrevs "
                 :help-echo "Save abbrevs to the abbrev file."
                 :action #'Custom-abbrev-save)
  (widget-insert "\n\n"))

;;;###autoload
(defun customize-all-abbrevs ()
  "Customize all Abbrevs in current session."
  (interactive)
  (custom-abbrev--prepare-buffer "*Customize All Abbrevs*")
  (widget-insert "This buffer is for customizing all Abbrevs.\n")
  (widget-insert "Check the ")
  (widget-create 'custom-manual
		 :tag "Abbrev section"
		 "(emacs)Abbrevs")
  (widget-insert " in the Emacs manual for more information.\n\n")
  (custom-abbrev--insert-buttons)
  (custom-group--draw-horizontal-line)
  (widget-insert "\n")
  (dolist (table-name (reverse abbrev-table-name-list))
    (let ((table (symbol-value table-name))
          abbrevs
          abbrev-widget
          visibility)
      (mapatoms (lambda (abbrev)
                  ;; Guard against ##.
                  (unless (string= (symbol-name abbrev) "")
                    (let ((exp (abbrev-expansion abbrev table)))
                      (when exp
                        (push (list (symbol-name abbrev) exp
                                    (symbol-function abbrev)
                                    (abbrev-get abbrev :enable-function)
                                    (abbrev-get abbrev :case-fixed))
                              abbrevs)))))
                table)
      (setq abbrevs (nreverse abbrevs))
      (setq visibility (widget-create
                        'custom-visibility
		        :help-echo "Hide or show this abbrev table."
		        :on "Hide"
		        :off "Show"
		        :on-glyph "down"
		        :off-glyph "right"
		        :action #'custom-abbrev-toggle-hide-abbrev-table
		        t))
      (setq abbrev-widget (widget-create
                           'custom-abbrev
                           :value abbrevs
                           :format "Abbrev Table: %{%t%}\n%v%i\n"
                           :tag (symbol-name table-name)
                           :sample-face 'highlight
                           :custom-abbrev-table table-name
                           '(list :tag "Abbrev"
                                  (string :tag "Abbreviation")
                                  (string :tag "Expansion")
                                  (choice :tag "Hook"
                                          (const :tag "None" nil)
                                          (function))
                                  (choice :tag "Enable function"
                                          (const :tag "None" nil)
                                          (function :value always))
                                  (boolean :tag "Case fixed"))))
      (push abbrev-widget custom-abbrev-widgets)
      (widget-put visibility :widget abbrev-widget)))
  (custom-abbrev--prepare-buffer-2))

(defun custom-abbrev-toggle-hide-abbrev-table (widget &rest _ignore)
  "Hide/Show the abbrev-table widget related to this visibility WIDGET."
  (let ((w (widget-get widget :widget))
        (val (not (widget-value widget))))
    (widget-value-set widget val)
    (widget-put w :format (concat "Abbrev Table: %{%t%}"
                                  (if val "\n%v%i" "")
                                  "\n"))
    (if val
        (widget-value-set w (widget-get w :stashed-value))
      (widget-put w :stashed-value (widget-value w))
      (widget-value-set w (widget-value w)))
    (widget-setup)))
  
;;;###autoload
(defun customize-abbrevs (&optional table-name)
  "Customize Abbrevs in current session for abbrev table TABLE-NAME.

When called interactively, prompts for the abbrev table to customize.
When called from Lisp, optional argument TABLE-NAME is the symbol that holds
the abbrev table to customize.  If nil, it defaults to `global-abbrev-table'."
  (interactive (list
                (intern-soft
                 (completing-read (format-prompt "Customize Abbrev Table"
                                                 "global-abbrev-table")
                                  abbrev-table-name-list nil t))))
  (let* ((table-name (or table-name 'global-abbrev-table))
         (table (symbol-value table-name))
         abbrevs)
    (mapatoms (lambda (abbrev)
                ;; Guard against ##.
                (unless (string= (symbol-name abbrev) "")
                  (let ((exp (abbrev-expansion abbrev table)))
                    (when exp
                      ;; We treat each abbrev as a list:
                      ;; (ABBREV EXPANSION HOOK ENABLE-FUNCTION CASE-FIXED)
                      (push (list (symbol-name abbrev) exp
                                  (symbol-function abbrev)
                                  (abbrev-get abbrev :enable-function)
                                  (abbrev-get abbrev :case-fixed))
                            abbrevs)))))
              table)
    (setq abbrevs (nreverse abbrevs))
    (custom-abbrev--prepare-buffer
     (format "*Customize Abbrevs: %s*" table-name))
    (widget-insert "This buffer is for customizing abbrevs\n")
    (widget-insert "in abbrev table: ")
    (widget-create 'variable-link :button-face 'widget-button table-name)
    (widget-insert "\n\nCheck the ")
    (widget-create 'custom-manual :tag "Abbrev section" "(emacs)Abbrevs")
    (widget-insert " in the Emacs manual for more information.\n\n")
    (custom-abbrev--insert-buttons)
    (custom-group--draw-horizontal-line)
    (widget-insert "\n")
    (setq custom-abbrev-widgets
          ;; It's always a list, even when customizing a single abbrev-table.
          (list
           (widget-create 'custom-abbrev :value abbrevs
                          :custom-abbrev-table table-name
                          '(list :tag "Abbrev"
                                 (string :tag "Abbreviation")
                                 (string :tag "Expansion")
                                 (choice :tag "Hook"
                                          (const :tag "None" nil)
                                          (function))
                                 (choice :tag "Enable function"
                                         (const :tag "None" nil)
                                         (function :value always))
                                 (boolean :tag "Case fixed"))))))
  (custom-abbrev--prepare-buffer-2))

(provide 'cus-abbrev)
;;; cus-abbrev.el ends here
