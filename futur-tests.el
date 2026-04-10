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
(require 'futur-elisp)
(require 'ert)

(ert-deftest futur--resignal ()
  (let ((err1 (list 'error "hello")))
    (should (eq err1 (condition-case err2
                         (progn (futur--resignal err1) nil)
                       (error err2))))))

(ert-deftest futur-simple ()
  (should (equal (futur-blocking-wait-to-get-result (futur-done 5)) 5))

  (let ((p (futur-failed '(scan-error "Oops"))))
    (should-error (futur-blocking-wait-to-get-result p) :type 'scan-error))

  (should (equal
           (futur-blocking-wait-to-get-result
            (futur-let* ((x1 5)
                         (x2 <- (futur-done 7)))
              (+ x1 x2)))
           12))

  (let ((p (futur-let* ((x1 5)
                        (x2 <- (futur-failed '(scan-error "Oops"))))
             (list (+ x1 x2) (error "Wow!")))))
    (should-error (futur-blocking-wait-to-get-result p) :type 'scan-error))

  (let ((p (futur-let* ((x1 5)
                        (x2 <- (futur-failed '(scan-error "Oops"))))
            :error-fun ((wrong-type-argument (_) 23)
                        (scan-error (_err) 32))
            (list (+ x1 x2) (error "Wow!")))))
    (should (equal (futur-blocking-wait-to-get-result p) 32)))

  (let ((handlers `((wrong-type-argument . ,(lambda (_) :wta))
                    (scan-error . ,(lambda (_) :scan-error))
                    (error . ,(lambda (_) :error)))))
    (should (equal
             (condition-case nil
                 (progn (futur-blocking-wait-to-get-result
                         (futur-failed '(quit)) handlers)
                        :not-quit)
               (quit :quit))
             :quit))
    (should (equal (futur-blocking-wait-to-get-result
                    (futur-failed '(wrong-number-of-arguments)) handlers)
                   :error))
    (should (equal (futur-blocking-wait-to-get-result
                    (futur-failed '(scan-error)) handlers)
                   :scan-error))))

(ert-deftest futur-ordering ()
  "Test order of execution of callbacks."
  (let* ((x '())
         (fut1 (futur-timeout 0))
         (fut21 (futur-bind fut1 (lambda (_) (push 'fut21 x) x)))
         (fut22 (futur-bind fut1 (lambda (_) (push 'fut22 x) x)))
         (fut3 (futur-list fut22 fut21)))
    (should (equal (futur-blocking-wait-to-get-result fut3)
                   '((fut22 fut21) (fut21)))))
  (let* ((x '())
         (fut1 (futur-timeout 0))
         (fut22 (futur-bind fut1 (lambda (_) (push 'fut22 x) x)))
         (fut21 (futur-bind fut1 (lambda (_) (push 'fut21 x) x)))
         (fut3 (futur-list fut22 fut21)))
    (should (equal (futur-blocking-wait-to-get-result fut3)
                   '((fut22) (fut21 fut22))))))

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

(ert-deftest futur-race ()
  (let* ((x '())
         (timescale 0.1)
         (_timer1 (run-with-timer (* 2 timescale) nil (lambda () (push 'timer1 x))))
         (futur (futur-race
                 (futur-let* ((_ <- (futur-timeout (* 2 timescale))))
                   (push 'a x) x)
                 (futur-let* ((_ <- (futur-timeout (* 1 timescale))))
                   (push 'b x) x)
                 (futur-let* ((_ <- (futur-timeout (* 3 timescale))))
                   (push 'c x) x)))
         (res (futur-blocking-wait-to-get-result
               (futur-list (futur-let* ((_ <- (futur-timeout (* 4 timescale))))
                            x)
                           futur))))
   (should (equal res '((timer1 b) (b))))))

(ert-deftest futur-abort ()
  (let* ((x '())
         (start (float-time))
         (timescale 0.5)
         (_fut1 (futur-let* ((_ <- (futur-timeout (* timescale 1))))
                  (push 'timer1 x)
                  nil)) ;; Don't warn about unused return value.
         (fut6 (futur-let* ((_ <- (futur-timeout (* timescale 6))))
                 (push 'timer6 x)
                 nil)) ;; Don't warn about unused return value.
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
    (should (futur--waiting-p fut6))
    (should (pcase fut2 ((futur--done 'nil) t)))
    (should (pcase fut22 ((futur--failed `(futur-aborted . ,_)) t)))
    (should (pcase fut4 ((futur--waiting _) t) ((futur--done 'nil) t)))
    (should (futur--waiting-p futB))
    (should (equal '(nil) (futur-blocking-wait-to-get-result futB)))
    (should (equal x '(timer1)))))

(ert-deftest futur-process ()
  (with-temp-buffer
    (let* ((tmpfile (make-temp-file "futur"))
           (buf (current-buffer))
           (_ (write-region "Emacs" nil tmpfile nil 'silent))
           (futur
            (futur-let* ((exitcode
                          <- (futur--process-make
                              :name "futur-hexl"
                              :command (list (expand-file-name
                                              "hexl" exec-directory)
                                             tmpfile)
                              :buffer buf)))
              (list exitcode
                    (with-current-buffer buf
                     (buffer-string))))))
      (unwind-protect
          (should (equal (futur-blocking-wait-to-get-result futur)
                   '(0 "00000000: 456d 6163 73                             Emacs\n")))
        (delete-file tmpfile)))))

(ert-deftest futur-process-bounded ()
  (let ((run
         (lambda (times concurrency)
           (let* ((futures ())
                  (timescale 1)
                  (start (float-time))
                  (futur-concurrency-bound concurrency))
             (dotimes (_ times)
               (push (futur-concurrency-bound #'futur-timeout (* timescale 0.1))
                     futures))
             (futur-blocking-wait-to-get-result (apply #'futur-list futures))
             (/ (- (float-time) start) timescale)))))
    (should (<= 0.5 (funcall run 10 2) 0.7))
    (should (<= 0.3 (funcall run 10 4) 0.5))))

(ert-deftest futur-server ()
  (let* ((futur (futur-elisp--get-process 'futur-server #'futur-elisp--launch))
         (proc (futur-blocking-wait-to-get-result futur)))
    (should (process-get proc 'futur--ready))
    (should (null (process-get proc 'futur--destination)))))

(defun futur--tests-elisp-funcall (elisp-funcall)
  (let ((fut (funcall elisp-funcall #'+ 5 7)))
    (should (equal 12 (futur-blocking-wait-to-get-result fut))))

  (let ((fut (funcall elisp-funcall #'car 7))
        (normal-error (condition-case err2 (car 7) (error err2))))
    ;; Allow extra debug info tacked to the end of the error descriptor.
    (should (equal (take (length normal-error)
                         (condition-case err1
                             (futur-blocking-wait-to-get-result fut)
                           (error err1)))
                   normal-error)))

  (let ((fut (funcall elisp-funcall #'documentation 'car)))
    (should (equal (futur-blocking-wait-to-get-result fut)
                   (documentation 'car))))

  (let* ((str (let ((chars ()))
                (dotimes (i 1024)
                  (push i chars))
                (apply #'string (nreverse chars))))
         (fut (funcall elisp-funcall #'identity str)))
    (should (equal (futur-blocking-wait-to-get-result fut)
                   str)))

  (let* ((f (lambda (context)
              (futur-reset-context
               'futur-test-mini context)
              (let ((fun (symbol-function 'diff-mode)))
                ;; Beware: can't return a subr because it's not print-readable.
                (if (subrp fun) 'subr fun))))
         (fut
          (futur-let*
              ((da1 <- (funcall elisp-funcall f ()))
               (da2 <- (funcall elisp-funcall f '((require diff-mode))))
               (da3 <- (funcall elisp-funcall f ())))
            (list da1 da2 da3)))
         (vals (futur-blocking-wait-to-get-result fut)))
    (should (autoloadp (nth 0 vals)))
    (should (or (functionp (nth 1 vals)) (eq 'subr (nth 1 vals))))
    (should-not (equal (nth 0 vals) (nth 1 vals)))
    (should (equal (nth 0 vals) (nth 2 vals)))))

(ert-deftest futur-elisp-funcall ()
  (futur--tests-elisp-funcall #'futur-elisp--funcall))


(ert-deftest futur-elisp-sandbox-funcall ()
  (futur--tests-elisp-funcall #'futur-elisp-sandbox--funcall))

(provide 'futur-tests)
;;; futur-tests.el ends here
