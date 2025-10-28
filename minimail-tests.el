;;; minimail-tests.el --- tests for minimail.el      -*- lexical-binding: t; -*-

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

;; Tests for the Minimail package.

;;; Code:

(require 'ert)
(require 'minimail)

;;; athunk stuff

(defun -with-polling (athunk)
  (athunk-run-polling athunk :interval 0.001 :max-tries 1000))

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
  (-should-take-seconds 0.25
    (-with-polling
     (athunk-let* ((x <- (athunk-sleep 0.25 'xxx)))
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

(ert-deftest minimail-tests-mutex ()
  (let (queue busy result)
    (dotimes (i 3)
      (athunk-run
       (athunk-with-mutex queue
                          (athunk-let*
                              ((_ (when busy (push t result)))
                               (_ (setq busy t))
                               (_ <- (athunk-sleep 0.1)))
                            (setq busy nil)
                            (push i result)))))
    (-with-polling (athunk-sleep 0.35))
    (should (equal result '(2 1 0)))
    (should (equal queue '(1)))))

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
      (should (length= v 4))
      (should (equal (mapcar #'car v)
                     '("INBOX" "INBOX" "[Gmail]" "[Gmail]/All Mail"))))))

;;; Message threading

(ert-deftest minimail-tests-thread-position ()
  (let ((-thread-tree '((2) (3 6 (4 23) (44 7 96)))))
    (should (= 0 (-thread-position 2)))
    (should (= 1 (-thread-position 3)))
    (should (= 2 (-thread-position 6)))
    (should (= 3 (-thread-position 4)))
    (should (= 4 (-thread-position 23)))
    (should (= 5 (-thread-position 44)))
    (should (= 6 (-thread-position 7)))
    (should (= 7 (-thread-position 96)))
    (should-not (-thread-position 999))
    (should-not (-thread-position nil)))

  (let ((-thread-tree '(((3)(5)))))
    (should (= 0 (-thread-position 3)))
    (should (= 1 (-thread-position 5)))))

(ert-deftest minimail-tests-thread-root ()
  (let ((-thread-tree '((2) (3 6 (4 23) (44 7 96)))))
    (should (= 2 (-thread-root 2)))
    (should (= 3 (-thread-root 3)))
    (should (= 3 (-thread-root 6)))
    (should (= 3 (-thread-root 4)))
    (should (= 3 (-thread-root 23)))
    (should (= 3 (-thread-root 44)))
    (should (= 3 (-thread-root 7)))
    (should (= 3 (-thread-root 96)))
    (should-not (-thread-root 999))
    (should-not (-thread-root nil)))

  (let ((-thread-tree '(((3)(5)))))
    (should (= 3 (-thread-root 3)))
    (should (= 5 (-thread-root 5)))))

(ert-deftest minimail-tests-thread-level ()
  (let ((-thread-tree '((2) (3 6 (4 23) (44 7 96)))))
    (should (= 0 (-thread-level 2)))
    (should (= 0 (-thread-level 3)))
    (should (= 1 (-thread-level 6)))
    (should (= 2 (-thread-level 4)))
    (should (= 3 (-thread-level 23)))
    (should (= 2 (-thread-level 44)))
    (should (= 3 (-thread-level 7)))
    (should (= 4 (-thread-level 96)))
    (should-not (-thread-level 999))
    (should-not (-thread-level nil)))

  (let ((-thread-tree '(((3)(5)))))
    (should (= 0 (-thread-level 3)))
    (should (= 0 (-thread-level 5)))))

(ert-deftest minimail-tests-thread-huge-tree ()
  (let* ((n 10000) (-thread-tree `(,(number-sequence 0 n))))
    (should (= n (-thread-position n)))
    (should (= 0 (-thread-root n)))
    (should (= n (-thread-level n))))

  (let* ((n 100)                        ;This test consumes stack
         (-thread-tree (seq-reduce (lambda (i v) (list v i))
                                   (number-sequence n 0 -1) -1)))
    (should (= n (-thread-position n)))
    (should (= 0 (-thread-root n)))
    (should (= n (-thread-level n))))

  (let* ((n 100)                        ;This test consumes stack
         (-thread-tree (seq-reduce #'list (number-sequence 1 n) '(0))))
    (should (eq n (-thread-position n)))
    (should (eq n (-thread-root n)))
    (should (eq 0 (-thread-level n)))))

;; Local Variables:
;; read-symbol-shorthands: (("-" . "minimail--") ("athunk-" . "minimail--athunk-"))
;; End:

;;; minimail-tests.el ends here
