;; greader-backend-tests.el --- ERT tests for greader TTS backend CLI generation  -*- lexical-binding: t; -*-
;; Copyright (C) 2017-2026  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Tests verifying that TTS backend functions produce the correct CLI
;; command-line flags after voice/language selection via `completing-read'.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'greader-mac)
(require 'greader-espeak)

;;; greader-mac

(ert-deftest greader-mac-voice-selection-cmdline ()
  "After `completing-read' voice selection, `'lang' returns the correct CLI flag.
Simulates the full flow used by `greader-set-language':
  1. `'set-voice' opens completing-read and returns the plain voice name.
  2. `'lang VOICE' sets the buffer-local voice and returns the \"-v\" flag.
  3. `'lang nil' (as called by `greader-build-args') also returns the flag."
  (with-temp-buffer
    (setq-local greader-mac-voice nil)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "Alex"))
              ((symbol-function 'greader--mac-get-voices)
               (lambda () '("system" "Alex" "Alice"))))
      (let ((voice (greader-mac 'set-voice nil)))
        ;; set-voice must return the plain voice name
        (should (equal voice "Alex"))
        ;; passing that name to 'lang must set the voice and return the flag
        (should (equal (greader-mac 'lang voice) "-vAlex"))
        ;; 'lang without arg (greader-build-args path) must return the same flag
        (should (equal (greader-mac 'lang nil) "-vAlex"))))))

(ert-deftest greader-mac-system-voice-clears-flag ()
  "Selecting \"system\" voice clears the voice flag (nil means system default)."
  (with-temp-buffer
    (setq-local greader-mac-voice "Alex")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "system"))
              ((symbol-function 'greader--mac-get-voices)
               (lambda () '("system" "Alex" "Alice"))))
      (let ((voice (greader-mac 'set-voice nil)))
        (should (equal voice "system"))
        (should (null (greader-mac 'lang voice)))
        (should (null (greader-mac 'lang nil)))))))

;;; greader-espeak

(ert-deftest greader-espeak-voice-selection-cmdline ()
  "After `completing-read' language selection, `'lang' returns the correct CLI flag.
Simulates the full flow used by `greader-set-language':
  1. `'set-voice' opens completing-read and returns the plain language code.
  2. `'lang LANG' sets the buffer-local language and returns the \"-v\" flag.
  3. `'lang nil' (as called by `greader-build-args') also returns the flag."
  (with-temp-buffer
    (setq-local greader-espeak-language "en")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "it"))
              ((symbol-function 'greader--espeak-list-voices)
               (lambda () '("en" "fr" "it" "de"))))
      (let ((lang (greader-espeak 'set-voice nil)))
        ;; set-voice must return the plain language code
        (should (equal lang "it"))
        ;; passing that code to 'lang must set the language and return the flag
        (should (equal (greader-espeak 'lang lang) "-vit"))
        ;; 'lang without arg (greader-build-args path) must return the same flag
        (should (equal (greader-espeak 'lang nil) "-vit"))))))

;;; save-voice persistence

(ert-deftest greader-mac-save-voice-calls-customize ()
  "`'save-voice' calls `customize-save-variable' with the correct variable and value."
  (let (saved-var saved-val)
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (var val &rest _) (setq saved-var var saved-val val))))
      (greader-mac 'save-voice "Alex"))
    (should (eq saved-var 'greader-mac-voice))
    (should (equal saved-val "Alex"))))

(ert-deftest greader-espeak-save-voice-calls-customize ()
  "`'save-voice' calls `customize-save-variable' with the correct variable and value."
  (let (saved-var saved-val)
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (var val &rest _) (setq saved-var var saved-val val))))
      (greader-espeak 'save-voice "it"))
    (should (eq saved-var 'greader-espeak-language))
    (should (equal saved-val "it"))))

(provide 'greader-backend-tests)
;;; greader-backend-tests.el ends here
