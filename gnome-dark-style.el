;;; gnome-dark-style.el --- Sync theme with GNOME color-scheme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: David Dimagid <davidimagid@gmail.com>
;; Maintainer: David Dimagid <davidimagid@gmail.com>
;; Version: 0.1
;; Package-Version: 0.1
;; URL: https://github.com/dimagid/gnome-dark-style
;; Package-Requires: ((emacs "30.1"))
;; Keywords: themes, gnome, sync, dark, light, color-scheme

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The `gnome-dark-style' package automatically sets the Emacs theme
;; based on GNOME's color scheme.  It allows the user to define custom
;; light and dark themes.

;; Requirements:
;; - GNOME Shell 47.4. (tested with this version).
;; - The `gsettings' command must be available in the system.

;;; Code:

(defgroup gnome-dark-style nil
  "Customize Emacs theme based on GNOME's color scheme."
  :group 'environment)

(defcustom gnome-light-theme nil
  "Emacs light theme for GNOME's light style.
Set to nil to use the default Emacs theme."
  :type 'symbol
  :group 'gnome-dark-style)

(defcustom gnome-dark-theme 'wombat
  "Emacs dark theme for GNOME's dark style."
  :type 'symbol
  :group 'gnome-dark-style)

;;;###autoload
(defun get-gnome-color-scheme ()
  "Get the current GNOME color-scheme setting with gsettings."
  (string-trim
   (shell-command-to-string
    "gsettings get org.gnome.desktop.interface color-scheme")))

;;;###autoload
(defun gnome-set-theme-based-in-color-scheme ()
  "Switch Emacs themes based on GNOME's color-scheme setting."
  (interactive)
  (let* ((gnome-color-scheme (get-gnome-color-scheme))
         (target-theme (cond
                        ((string-match-p "default" gnome-color-scheme)
                         gnome-light-theme)
                        ((string-match-p "prefer-light" gnome-color-scheme)
                         gnome-light-theme)
                        (t gnome-dark-theme))))
    ;; Disable all themes before loading the new one
    (mapc #'disable-theme custom-enabled-themes)
    (setq custom-enabled-themes nil)
    ;; Load the target theme (if not nil)
    (when target-theme
      (load-theme target-theme t))))

;;;###autoload
(defun gnome-start-color-scheme-monitor ()
  "Start monitoring GNOME's color-scheme."
  ;; Stop any existing monitor process
  (gnome-stop-color-scheme-monitor)
  (let ((process (make-process
                  :name "gsettings-monitor"
                  :command '("gsettings" "monitor" "org.gnome.desktop.interface" "color-scheme")
                  :filter (lambda (process output)
                            (when (string-match-p "color-scheme" output)
                              (gnome-set-theme-based-in-color-scheme))))))
    ;; Mark the process as non-queryable on exit
    (set-process-query-on-exit-flag process nil)))

;;;###autoload
(defun gnome-stop-color-scheme-monitor ()
  "Stop monitoring GNOME's color-scheme."
  (when (get-process "gsettings-monitor")
    (delete-process "gsettings-monitor")))

;;;###autoload
(defun gnome-dark-style--sync-toggle (enable)
  "Enable or disable sync of Emacs theme with GNOME's color scheme.
If ENABLE is non-nil, start monitoring and set the theme.
If ENABLE is nil, stop monitoring and retain the current theme."
  (let ((current-state (get-process "gsettings-monitor")))
    (if enable
        (unless current-state
          ;; Start monitoring GNOME's color scheme
          (gnome-start-color-scheme-monitor)
          ;; Add hook to stop monitoring when Emacs is killed
          (add-hook 'kill-emacs-hook #'gnome-stop-color-scheme-monitor)
          ;; Set the theme based on the current GNOME color scheme
          (gnome-set-theme-based-in-color-scheme))
      (when current-state
        ;; If synchronization is disabled, stop monitoring
        (gnome-stop-color-scheme-monitor)
        ;; Remove the kill-emacs hook
        (remove-hook 'kill-emacs-hook #'gnome-stop-color-scheme-monitor)))))

;;;###autoload
(defcustom gnome-dark-style-sync t
  "Sync with GNOME's color scheme variable.
When nil, synchronization stops and the current theme is retained."
  :type 'boolean
  :group 'gnome-dark-style
  :set (lambda (sym val)
         (set-default sym val)
         (gnome-dark-style--sync-toggle val)
	 val))

(provide 'gnome-dark-style)

;;; gnome-dark-style.el ends here
