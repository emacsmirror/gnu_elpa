;;; futur-tests.el --- Tests for the Futur library   -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

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

;;

;;; Code:

(require 'futur)
(require 'ert)

(ert-deftest futur--resignal ()
  (let ((err1 (list 'error "hello")))
    (should (eq err1 (condition-case err2
                         (progn (futur--resignal err1) nil)
                       (error err2))))))

(ert-deftest futur-simple ()
  (should (equal (futur-blocking-wait-to-get-result (futur-done 5)) 5))

  (let ((p (futur-error '(scan-error "Oops"))))
    (should-error (futur-blocking-wait-to-get-result p) :type 'scan-error))

  (should (equal
           (futur-blocking-wait-to-get-result
            (futur-let* ((x1 5)
                         (x2 <- (futur-done 7)))
              (+ x1 x2)))
           12))

  (let ((p (futur-let* ((x1 5)
                        (x2 <- (futur-error '(scan-error "Oops"))))
             (list (+ x1 x2) (error "Wow!")))))
    (should-error (futur-blocking-wait-to-get-result p) :type 'scan-error)))

(ert-deftest futur-timeout ()
  (let* ((x '())
         (_timer1 (run-with-timer 0.1 nil (lambda () (push 'timer1 x))))
         (_timer2 (run-with-timer 0.3 nil (lambda () (push 'timer2 x))))
         (futur (futur-let* ((_ <- (futur-timeout 0.2))
                             (x1 x)
                             (_ <- (futur-timeout 0.2))
                             (x2 x))
                  (list x1 x2)))
         (res (futur-blocking-wait-to-get-result futur)))
    (should (equal res '((timer1) (timer2 timer1))))))

(ert-deftest futur-list ()
  (let* ((x '())
         (_timer1 (run-with-timer 0.1 nil (lambda () (push 'timer1 x))))
         (_timer2 (run-with-timer 0.6 nil (lambda () (push 'timer2 x))))
         (futur (futur-list
                 (futur-let* ((_ <- (futur-timeout 0.4))) (cons 'a x))
                 (futur-let* ((_ <- (futur-timeout 0.3))) (cons 'b x))))
         (res (futur-blocking-wait-to-get-result futur)))
    (should (equal res '((a timer1) (b timer1))))))

(ert-deftest futur-abort ()
  (let* ((x '())
         (start (float-time))
         (timescale 0.5)
         (_fut1 (futur-let* ((_ <- (futur-timeout (* timescale 1))))
                  (push 'timer1 x)))
         (fut6 (futur-let* ((_ <- (futur-timeout (* timescale 6))))
                 (push 'timer6 x)))
         (fut2 (futur-timeout (* timescale 2)))
         (fut4 (futur-timeout (* timescale 4)))
         (fut22 nil)
         (futA (futur-list
                (futur-let*
                    ((_ <- (futur-list fut2 (futur-timeout (* timescale 2))))
                     (_ (setq fut22 (futur-timeout (* timescale 2))))
                     (_ <- (futur-list fut4 fut22)))
                  (cons 'a x))
                (futur-let* ((_ <- (futur-timeout (* timescale 3))))
                  (signal 'error (cons "" x)))))
         (futB (futur-list fut4)))
    (should (equal x ()))
    (should (equal (condition-case err
                       (progn (futur-blocking-wait-to-get-result futA) t)
                     (error err))
                   '(error "" timer1)))
    (should (equal x '(timer1)))
    (should (< (- (float-time) start) (* timescale 4)))
    (should (pcase fut6 ((futur--waiting _) t)))
    (should (pcase fut2 ((futur--done 'nil) t)))
    (should (pcase fut22 ((futur--error '(futur-aborted)) t)))
    (should (pcase fut4 ((futur--waiting _) t) ((futur--done 'nil) t)))
    (should (pcase futB ((futur--waiting _) t)))
    (should (equal '(nil) (futur-blocking-wait-to-get-result futB)))
    (should (equal x '(timer1)))))

(ert-deftest futur-process ()
  (with-temp-buffer
    (let* ((tmpfile (make-temp-file "futur"))
           (buf (current-buffer))
           (_ (write-region "Emacs" nil tmpfile nil 'silent))
           (futur
            (futur-let* ((exitcode
                          <- (futur-process-make
                              :name "futur-hexl"
                              :command (list (expand-file-name
                                              "hexl" exec-directory)
                                             tmpfile)
                              :buffer buf)))
              (list exitcode
                    (with-current-buffer buf
                      (buffer-string)))))
           (res (futur-blocking-wait-to-get-result futur)))
      (should (equal res
                     '(0 "00000000: 456d 6163 73                             Emacs\n"))))))

(provide 'futur-tests)
;;; futur-tests.el ends here
