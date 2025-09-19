;;; system-packages-test.el --- ERT testing framework for system-packages.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2023 Free Software Foundation, Inc.

;; Author: J. Alexander Branham <alex.branham@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'system-packages)

(ert-deftest system-packages-get-install ()
  "Return correct installation command."
  (should (string=
           (let ((system-packages-use-sudo nil)
                 (system-packages-package-manager 'guix))
             (system-packages-get-command 'install))
           "guix package -i "))
  (should (string=
           (let ((system-packages-use-sudo nil)
                 (system-packages-package-manager 'pacman))
             (system-packages-get-command 'install))
           "pacman -S "))
  (should (string=
           (let ((system-packages-use-sudo nil)
                 (system-packages-package-manager 'dnf))
             (system-packages-get-command 'install "pkgconfig(enchant-2)"))
           "dnf install pkgconfig\\(enchant-2\\)")))

(ert-deftest system-packages-get-install-noconfirm ()
  "Return correct installation command."
  (should (string=
           (let ((system-packages-noconfirm t)
                 (system-packages-use-sudo nil)
                 (system-packages-package-manager 'guix))
             (system-packages-get-command 'install))
           "guix package -i "))
  (should (string=
           (let ((system-packages-noconfirm t)
                 (system-packages-use-sudo nil)
                 (system-packages-package-manager 'pacman))
             (system-packages-get-command 'install))
           "pacman -S --noconfirm"))
  (should (string=
           (let ((system-packages-noconfirm t)
                 (system-packages-use-sudo nil)
                 (system-packages-package-manager 'apt))
             (system-packages-get-command 'install "rg"))
           "apt-get install rg -y")))

(ert-deftest system-packages-errors ()
  "Error when we don't know a command."
  (should-error
   (let ((system-packages-package-manager 'pacaur))
     (system-packages-get-command 'install))))

;;; system-packages-test.el ends here
