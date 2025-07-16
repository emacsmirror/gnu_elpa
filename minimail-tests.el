;;; minimail-tests.el --- tests for minimail.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Augusto Stoffel

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

;; Tests for the Minimail package.

;;; Code:

(require 'ert)
(require 'minimail)

;;; athunk stuff

(defun -with-polling (athunk &optional secs)
  (setq secs (or secs 0.001))
  (let (done err val)
    (funcall athunk (lambda (e v) (setq done t err e val v)))
    (while (not done) (sleep-for secs))
    (when err (signal err val))
    val))

(defmacro -should-take-seconds (secs &rest body)
  "Assert that BODY takes approximately SECS seconds to run."
  (declare (indent 1))
  (let ((time (gensym)))
    `(let ((,time (current-time)))
       ,@body
       (should (< ,secs (float-time (time-since ,time)) ,(* secs 1.05))))))

(ert-deftest minimail-tests-let* ()
  (-with-polling
   (athunk-let* ((x <- (athunk-wrap 2))
                 (y <- (athunk-wrap (1+ x))))
     (should (eq y 3)))))

(ert-deftest minimail-tests-sleep ()
  (-should-take-seconds 1
    (-with-polling
     (athunk-let* ((x <- (athunk-sleep 1 'xxx)))
       (should (eq x 'xxx))))))

(ert-deftest minimail-tests-gather ()
  (-should-take-seconds 0.3
    (-with-polling
     (athunk-let* ((vec <- (athunk-gather (list (athunk-sleep 0.1 1)
                                                (athunk-sleep 0.3 2)
                                                (athunk-sleep 0.2 3)))))
       (should (equal vec [1 2 3]))))))

(ert-deftest minimail-tests-let ()
  (-should-take-seconds 0.2
    (-with-polling
     (athunk-let ((x <- (athunk-sleep 0.2 1))
                  (y <- (athunk-sleep 0.1 2))
                  (z 3))
       (should (eq x 1))
       (should (eq y 2))
       (should (eq z 3))))))

(ert-deftest minimail-tests-memoization ()
  (-with-polling
   (let* ((count 0)
          (place nil)
          (getter (lambda (v)
                    (athunk-memoize (alist-get 'key place)
                      (athunk-let* ((_ <- (athunk-sleep 0)))
                        (cl-incf count)
                        v)))))
     (athunk-let ((x (funcall getter 10))
                  (y (funcall getter 20)))
       (should (eq x 10))
       (should (eq y 10))
       (should (eq count 1))))))

(ert-deftest minimail-tests-memoization-reset ()
  (-with-polling
   (let* ((place nil)
          (getter (lambda (v)
                    (athunk-memoize (alist-get 'key place)
                      (athunk-wrap v)))))
     (athunk-let*
         ((x <- (funcall getter 1))
          (y <- (progn
               (athunk-unmemoize (alist-get 'key place))
               (funcall getter 2))))
       (should (eq x 1))
       (should (eq y 2))))))

;;; IMAP parsing

(ert-deftest minimail-tests-imap-list ()
  (with-temp-buffer
    (insert "\
* LIST (\\HasNoChildren) \"/\" \"INBOX\"
* STATUS \"INBOX\" (MESSAGES 187 UIDNEXT 36406 UNSEEN 14)
* LIST (\\HasChildren \\NonExistent) \"/\" \"[Gmail]\"
* LIST (\\HasNoChildren) \"/\" \"[Gmail]/All Mail\"
" )
    (goto-char (point-min))
    (let ((v (-parse-list)))
      (should (length= v 3))
      (should (equal (mapcar #'car v)
                     '("INBOX" "[Gmail]" "[Gmail]/All Mail"))))))

;; Local Variables:
;; read-symbol-shorthands: (("-" . "minimail--") ("athunk-" . "minimail--athunk-"))
;; End:

;;; minimail-tests.el ends here
