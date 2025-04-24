;;; make-nonascii.el --- Make an ASCII file non-ASCII  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

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

;; Just a helper to turn an ASCII file into a non-ASCII one while
;; trying to preserve the "structure" and make it still highlighted
;; and indented in the same way.

;;; Code:

(defun elb--make-nonascii ()
  "."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "[a-z]" nil t)
    (unless (memq (get-text-property (match-beginning 0) 'face)
                  '(font-lock-keyword-face font-lock-preprocessor-face))
      (let ((newchar
             (assoc (char-after (match-beginning 0))
                    '((?a . "α")
                      (?b . "♭")
                      (?c . "©")
                      (?d . "🤞")
                      (?e . "எ")
                      (?f . "ɟ")
                      (?g . "🔒")
                      (?h . "♯")
                      (?i . "இ")
                      (?j . "ĵ")
                      (?k . "κ")
                      (?l . "ℓ")
                      (?m . "ḿ")
                      (?n . "π")
                      (?o . "∘")
                      (?p . "ρ")
                      (?q . "¶")
                      (?r . "ŕ")
                      (?s . "ś")
                      (?t . "τ")
                      (?u . "உ")
                      (?v . "ν")
                      (?w . "ω")
                      (?x . "×")
                      (?y . "γ")
                      (?z . "ź")))))
        (when newchar
          (replace-match (cdr newchar) t t))))))

;;; make-nonascii.el ends here
