;; parser-generator-lr-test.el --- Tests for LR(k) Parser Generator -*- lexical-binding: t -*-

;; Copyright (C) 2020-2025  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'parser-generator-lr)
(require 'ert)

(defun parser-generator-lr-test--parse-incremental-vs-regular ()
  "Verify that regular and incremental parse results in same data."
  (let ((regular-parse (parser-generator-lr--parse t)))
    ;; (message "regular-parse: %s" regular-parse)
    (let ((regular-parse-history (nth 3 regular-parse)))
      ;; (message "regular-parse-history: %s" regular-parse-history)
      (let ((history-length (length regular-parse-history))
            (history-index 0)
            (history))
        (while (< history-index history-length)
          (setq history (nth history-index regular-parse-history))
          (let ((input-tape-index (nth 0 history))
                (pushdown-list (nth 1 history))
                (output (nth 2 history))
                (translation (nth 3 history))
                (translation-symbol-table (nth 4 history)))

            ;; (message "input-tape-index: %s" input-tape-index)
            ;; (message "pushdown-list: %s" pushdown-list)
            ;; (message "output: %s" output)
            ;; (message "translation: %s" translation)
            ;; (message "history-list: %s" history-list)

            (let ((incremental-parse
                   (parser-generator-lr--parse
                    t
                    input-tape-index
                    pushdown-list
                    output
                    translation
                    translation-symbol-table)))
              ;; (message "incremental-parse: %s" incremental-parse)
              (should
               (equal
                regular-parse
                incremental-parse))
              (message "Passed incremental parse test %s" (1+ history-index)))

            (setq history-index (1+ history-index))))))))

(defun parser-generator-lr-test--generate-precedence-tables ()
  "Test `parser-generator-lr--generate-precedence-tables'."
  (message "Starting tests for (parser-generator-lr--generate-precedence-tables)")

  ;; Test getting token precedence value and type
  (setq
   parser-generator--global-attributes
   '(%left %precedence %right))
  (setq
   parser-generator-lr--global-precedence-attributes
   '(%left %precedence %right))
  (setq
   parser-generator--context-sensitive-attributes
   '(%prec))
  (setq
   parser-generator-lr--context-sensitive-precedence-attribute
   '%prec)
  (setq
   parser-generator--global-declaration
   '((%left a)
     (%right b)
     (%left c)
     (%precedence FIRST)))
  (parser-generator-set-grammar
   '(
     (Sp S A B)
     (a b c)
     (
      (Sp S)
      (S (A c) B)
      (A (a b %prec a))
      (B (a b c %prec FIRST))
      )
     Sp))
  (parser-generator-process-grammar)
  (parser-generator-lr--generate-precedence-tables)
  (should
   (equal
    '%left
    (parser-generator-lr--get-symbol-precedence-type 'a)))
  (should
   (equal
    0
    (parser-generator-lr--get-symbol-precedence-value 'a)))
  (should
   (equal
    '%right
    (parser-generator-lr--get-symbol-precedence-type 'b)))
  (should
   (equal
    1
    (parser-generator-lr--get-symbol-precedence-value 'b)))
  (should
   (equal
    '%left
    (parser-generator-lr--get-symbol-precedence-type 'c)))
  (should
   (equal
    2
    (parser-generator-lr--get-symbol-precedence-value 'c)))
  (message "Passed generation of precedence value and type of symbols.")

  ;; Sp -> S
  (should
   (equal
    nil
    (parser-generator-lr--get-production-number-precedence-type 0)))
  (should
   (equal
    nil
    (parser-generator-lr--get-production-number-precedence-value 0)))

  ;; S -> A c
  (should
   (equal
    '%left
    (parser-generator-lr--get-production-number-precedence-type 1)))
  (should
   (equal
    2
    (parser-generator-lr--get-production-number-precedence-value 1)))

  ;; S -> B
  (should
   (equal
    nil
    (parser-generator-lr--get-production-number-precedence-type 2)))
  (should
   (equal
    nil
    (parser-generator-lr--get-production-number-precedence-value 2)))

  ;; A -> a b %prec a
  (should
   (equal
    '%left
    (parser-generator-lr--get-production-number-precedence-type 3)))
  (should
   (equal
    0
    (parser-generator-lr--get-production-number-precedence-value 3)))

  ;; B -> a b c %prec FIRST
  (should
   (equal
    '%precedence
    (parser-generator-lr--get-production-number-precedence-type 4)))
  (should
   (equal
    3
    (parser-generator-lr--get-production-number-precedence-value 4)))
  (message "Passed generation of precedence value and type of productions.")

  ;; Grammar with conflicts that can be resolved
  ;; using context-sensitive precedence attributes
  (parser-generator-set-grammar
   '(
     (Sp S A B)
     (a b c)
     (
      (Sp S)
      (S (A c) B)
      (A (a b))
      (B (a b c))
      )
     Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (should-error
   (parser-generator-lr-generate-parser-tables))
  (message "Passed test conflicted grammar caused expected exception")

  (setq
   parser-generator--global-attributes
   '(%precedence))
  (setq
   parser-generator-lr--global-precedence-attributes
   '(%precedence))
  (setq
   parser-generator--global-declaration
   '((%precedence FIRST)))
  (setq
   parser-generator--context-sensitive-attributes
   '(%prec))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-grammar
   '(
     (Sp S A B)
     (a b c)
     (
      (Sp S)
      (S (A c) B)
      (A (a b))
      (B (a b c  %prec FIRST))
      )
     Sp))
  (parser-generator-process-grammar)
  (should-error
   (parser-generator-lr-generate-parser-tables))
  (message "Passed test conflicted grammar caused expected exception 2")

  (setq
   parser-generator-lr--context-sensitive-precedence-attribute
   '%prec)
  (setq
   parser-generator-lr--precedence-comparison-function
   (lambda(a-type a-value _b-type b-value)
     (cond

      ((and
        a-value
        b-value)
       (cond
        ((> a-value b-value)
         t)

        ((< a-value b-value)
         nil)

        ((= a-value b-value)

         (cond
          ((equal a-type '%left)
           t)

          ((equal a-type '%right)
           nil)

          ((equal a-type '%precedence)
           t))

         )))

      ((and
        a-value
        (not b-value))
       t)

      ((and
        (not a-value)
        (not b-value))
       nil)

      )))
  (parser-generator-lr-generate-parser-tables)
  (should
   (equal
    '((0 (((a) shift))) (1 (((c) shift))) (2 ((($) reduce 2))) (3 ((($) accept))) (4 (((b) shift))) (5 (((c) shift))) (6 ((($) reduce 4))) (7 ((($) reduce 1))))
    (parser-generator-lr--get-expanded-action-tables)))
  (message "Passed test grammar not conflicting anymore solution #1")

  ;; Example parse "a b c"
  ;; stack: 0
  ;; a -> action shift, goto 4
  ;; stack: 0 a 4
  ;; b -> action shift, goto 5
  ;; stack: 0 a 4 b 5
  ;; c -> shift, goto 6
  ;; stack: 0 a 4 b 5 c 6
  ;; $ -> reduce 4 -> pop 6, new-stack: 0 B, goto 2
  ;; stack: 0 B 2
  ;; $ -> reduce 2, pop 2, new-stack: 0 S, goto 3
  ;; stack: 0 S 3
  ;; $ -> accept

  ;; Make a new context-sensitive precedence that
  ;; makes production 1 take precedence over production 4

  (parser-generator-set-grammar
   '(
     (Sp S A B)
     (a b c)
     (
      (Sp S)
      (S (A c) B)
      (A (a b %prec FIRST))
      (B (a b c))
      )
     Sp))
  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)
  (should
   (equal
    '((0 (((a) shift))) (1 (((c) shift))) (2 ((($) reduce 2))) (3 ((($) accept))) (4 (((b) shift))) (5 (((c) reduce 3))) (6 ((($) reduce 4))) (7 ((($) reduce 1))))
    (parser-generator-lr--get-expanded-action-tables)))
  (message "Passed test grammar not conflicting anymore solution #2")

  ;; Example parse "a b c"
  ;; stack: 0
  ;; a -> action shift, goto 4
  ;; stack: 0 a 4
  ;; b -> action shift, goto 5
  ;; stack: 0 a 4 b 5
  ;; c -> reduce 3 -> pop 4, new-stack: 0, goto 1
  ;; stack: 0 A 1
  ;; c -> shift, goto 7
  ;; stack: 0 A 1 c 7
  ;; $ -> reduce 1 -> pop 4, new-stack: 0, goto 3
  ;; stack: 0 S 3
  ;; $ -> accept

  ;; Test grammar that can be only solved by using global and context-sensitive attributes
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (with-current-buffer "*buffer*"
       (let ((token))
         (when
             (<
              index
              (point-max))
           (goto-char
            index)

           ;; Skip white-space(s)
           (when (looking-at-p "[\t ]+")
             (when
                 (search-forward-regexp "[^\t ]" nil t)
               (forward-char -1)))

           (cond
            ((looking-at "\\([0-9]+\\.[0-9]+\\|[0-9]+\\)")
             (setq
              token
              `(NUM ,(match-beginning 0) . ,(match-end 0))))
            ((looking-at "\\(\\+\\|-\\|*\\|/\\|\\^\\|)\\|(\\|\n\\)")
             (let ((symbol
                    (buffer-substring-no-properties
                     (match-beginning 0)
                     (match-end 0))))
               (setq
                token
                `(,symbol ,(match-beginning 0) . ,(match-end 0)))))
            (t (error "Unexpected input at %d!" index)))
           (list token))))))

  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (with-current-buffer "*buffer*"
       (let ((start (car (cdr token)))
             (end (cdr (cdr token))))
         (when (<= end (point-max))
           (let ((symbol
                  (buffer-substring-no-properties start end)))
             (when
                 (string-match-p "^\\([0-9]+\\.[0-9]+\\|[0-9]+\\)$" symbol)
               (setq
                symbol
                (string-to-number symbol)))
             symbol))))))

  (setq
   parser-generator-lr--global-precedence-attributes
   nil)
  (setq
   parser-generator-lr--context-sensitive-precedence-attribute
   nil)
  (parser-generator-set-e-identifier '%empty)
  (parser-generator-set-look-ahead-number 1)
  (setq
   parser-generator--global-attributes
   '(%left %precedence %right))
  (setq
   parser-generator--context-sensitive-attributes
   '(%prec))
  (setq
   parser-generator--global-declaration
   nil)
  (parser-generator-set-grammar
   '(
     (start input line exp)
     ("+" "-" "*" "/" "^" "(" ")" "\n" NUM)
     (
      (start input)
      (input
       %empty
       (input line (lambda(args _terminals) (nth 1 args))))
      (line
       "\n"
       (exp "\n" (lambda(args _terminals) (nth 0 args))))
      (exp
       NUM
       (exp "+" exp (lambda(args _terminals) (+ (float (nth 0 args)) (nth 2 args))))
       (exp "-" exp (lambda(args _terminals) (- (float (nth 0 args)) (nth 2 args))))
       (exp "*" exp (lambda(args _terminals) (* (float (nth 0 args)) (nth 2 args))))
       (exp "/" exp (lambda(args _terminals) (/ (float (nth 0 args)) (nth 2 args))))
       ("-" exp %prec NEG (lambda(args _terminals) (- (float (nth 1 args)))))
       (exp "^" exp (lambda(args _terminals) (expt (float (nth 0 args)) (nth 2 args))))
       ("(" exp ")" (lambda(args _terminals) (nth 1 args)))))
     start))
  (parser-generator-process-grammar)
  (should-error
   (parser-generator-lr-generate-parser-tables))
  (message "Passed test grammar caused expected conflict 3")

  (setq
   parser-generator-lr--global-precedence-attributes
   '(%left %precedence %right))
  (setq
   parser-generator-lr--context-sensitive-precedence-attribute
   '%prec)
  (parser-generator-lr-generate-parser-tables)
  (message "Passed test grammar not conflicting anymore")

  ;; Parse: 1+1*2\n
  ;;
  ;; Production 0: ((start) (input))
  ;; Production 1: ((input) (%empty))
  ;; Production 2: ((input) (input line))
  ;; Production 3: ((line) ("
  ;; "))
  ;; Production 4: ((line) (exp "
  ;; "))
  ;; Production 5: ((exp) (NUM))
  ;; Production 6: ((exp) (exp "+" exp))
  ;; Production 7: ((exp) (exp "-" exp))
  ;; Production 8: ((exp) (exp "*" exp))
  ;; Production 9: ((exp) (exp "/" exp))
  ;; Production 10: ((exp) ("-" exp))
  ;; Production 11: ((exp) (exp "^" exp))
  ;; Production 12: ((exp) ("(" exp ")"))
  ;;
  ;; stack: 0
  ;; NUM -> reduce 1, pop 0, new-stack: 0 input, GOTO 1
  ;; stack: 0 input 1
  ;; NUM -> shift, GOTO 5
  ;; stack: 0 input 1 NUM 5
  ;; + -> reduce 5 to exp, pop 2, new-stack: 0 input 1, GOTO 6
  ;; new-stack: 0 input 1 exp 6
  ;; + -> shift, new-stack: 0 input 1 exp 6, GOTO 10
  ;; new-stack: 0 input 1 exp 6 + 10
  ;; NUM -> shift, new-stack: 0 input 1 exp 6 10 NUM 5
  ;; * -> reduce 5.. causes expected (1+1)*2 = 4
  (let ((buffer (generate-new-buffer "*buffer*")))
    (switch-to-buffer buffer)

    (kill-region (point-min) (point-max))
    (insert "2+3*5\n")
    (let ((parse (parser-generator-lr-parse)))
      (should
       (equal
        '(1 5 5 5 8 6 4 2)
        parse)))
    (message "Passed parse with correct precedence of 2+3*5 = 2+(3*5)")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        17.0
        translate)))
    (message "Passed translation with correct precedence of 2+3*5 = 2+(3*5) = 17")

    (kill-region (point-min) (point-max))
    (insert "2*3+5\n")
    (let ((parse (parser-generator-lr-parse)))
      (should
       (equal
        '(1 5 5 5 6 8 4 2)
        parse)))
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        16.0
        translate)))
    (message "Passed incorrect precedence of 2*3+5 => 2*(3+5) = 16")

    (kill-region (point-min) (point-max))
    (insert "-5+3\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        -8.0
        translate)))
    (message "Passed incorrect precedence of -5+3 => -(5+3) = -8")
    (kill-buffer))

  ;; Add global precedence, but it should not solve all errors
  (setq
   parser-generator--global-declaration
   '(
     (%left "-" "+")
     (%left "*" "/")
     (%precedence NEG)
     (%right "^")))
  (parser-generator-lr-generate-parser-tables)

    (let ((buffer (generate-new-buffer "*buffer*")))
    (switch-to-buffer buffer)

    (kill-region (point-min) (point-max))
    (insert "2+3*5\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        17.0
        translate)))
    (message "Passed correct precedence of 2+3*5 => 2+(3*5) = 17")

    (kill-region (point-min) (point-max))
    (insert "2*3+5\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        11.0
        translate)))
    (message "Passed correct precedence of 2*3+5 => (2*3)+5 = 11")

    (kill-region (point-min) (point-max))
    (insert "2-3*5\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        -13.0
        translate)))
    (message "Passed correct precedence of 2-3*5 => 2-(3*5) = -13")

    (kill-region (point-min) (point-max))
    (insert "2*3-5\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        1.0
        translate)))
    (message "Passed correct precedence of 2*3-5 => (2*3)-5 = 1")

    (kill-region (point-min) (point-max))
    (insert "2+10/5\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        4.0
        translate)))
    (message "Passed correct precedence of 2+10/5 => 2+(10/5) = 4")

    (kill-region (point-min) (point-max))
    (insert "10/5+2\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        4.0
        translate)))
    (message "Passed correct precedence of 10/5+2 => (10/5)+2 = 4")

    (kill-region (point-min) (point-max))
    (insert "-55\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        -55.0
        translate)))
    (message "Passed correct precedence of -55")

    (kill-region (point-min) (point-max))
    (insert "- 55\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        -55.0
        translate)))
    (message "Passed correct precedence of - 55")

    (kill-region (point-min) (point-max))
    (insert "-56 + 2\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        -54.0
        translate)))
    (message "Passed correct precedence of -56 + 2 = -54")

    (kill-region (point-min) (point-max))
    (insert "3+-3\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        0.0
        translate)))
    (message "Passed correct precedence of 3+-3 = 0")

    (kill-region (point-min) (point-max))
    (insert "(8*3+-3)\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        21.0
        translate)))
    (message "Passed correct precedence of (8*3+-3) = 21")

    (kill-region (point-min) (point-max))
    (insert "34/(8*3+-3)\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        1.619047619047619
        translate)))
    (message "Passed correct precedence of (34/(8*3+-3)) = 1.619047619047619")

    (kill-region (point-min) (point-max))
    (insert "4 + 4.5 - (34/(8*3+-3))\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        6.880952380952381
        translate)))
    (message "Passed correct precedence of 4 + 4.5 - (34/(8*3+-3)) = 6.880952380952381")

    (kill-region (point-min) (point-max))
    (insert "3 ^ 2\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
       (equal
        9.0
        translate)))
    (message "Passed correct precedence of 3 ^ 2 = 9.0")

    (kill-buffer))

  (message "Passed tests for (parser-generator-lr--generate-precedence-tables)"))

(defun parser-generator-lr-test--generate-action-tables ()
  "Test `parser-generator-lr--generate-action-tables'."
  (message "Starting tests for (parser-generator-lr--generate-action-tables)")

  ;; Example 5.32 p. 393
  (parser-generator-set-grammar
   '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)

  ;; Fig. 5.9 p. 374, replaced e with $
  (should
   (equal
    '((0 ((($) reduce 2) ((a) reduce 2)))
      (1 ((($) accept) ((a) shift)))
      (2 (((a) reduce 2) ((b) reduce 2)))
      (3 (((a) shift) ((b) shift)))
      (4 (((a) reduce 2) ((b) reduce 2)))
      (5 ((($) reduce 1) ((a) reduce 1)))
      (6 (((a) shift) ((b) shift)))
      (7 (((a) reduce 1) ((b) reduce 1))))
    (parser-generator-lr--get-expanded-action-tables)))
  (message "Passed Example 5.32 p. 393")

  ;; Cyclical grammar
  (parser-generator-set-grammar
   '(
     (Sp S A B)
     (a b c)
     (
      (Sp S)
      (S A B)
      (A (a b A) (a B))
      (B (c S))
      )
     Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)
  (message "Passed cyclical grammar")

  ;; Test e-identifier in midst of grammar below
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-grammar
   '((Sp S E) (a b) ((Sp S) (S (S a E b)) (S e) (E e)) Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)

  (should
   (equal
    '(
      (0 ((($) reduce 2) ((a) reduce 2)))
      (1 ((($) accept) ((a) shift)))
      (2 (((b) reduce 3)))
      (3 (((b) shift)))
      (4 ((($) reduce 1) ((a) reduce 1))))
    (parser-generator-lr--get-expanded-action-tables)))
  (message "Passed example with e-identifier in middle of rule")

  ;; Another test with e-identifier inside rule here
  (parser-generator-set-e-identifier '%empty)
  (parser-generator-set-grammar
   '(
     (Sp S A B C)
     (a b c)
     (
      (Sp S)
      (S (A C B))
      (A a)
      (B b)
      (C %empty)
      )
     Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)

  (should
   (equal
    '(
      (0 (((a) shift)))
      (1 (((b) reduce 4)))
      (2 ((($) accept)))
      (3 (((b) reduce 2)))
      (4 (((b) shift)))
      (5 ((($) reduce 1)))
      (6 ((($) reduce 3))))
    (parser-generator-lr--get-expanded-action-tables)))
  (message "Passed grammar with e-identifier in middle of rule")

  (message "Passed tests for (parser-generator-lr--generate-action-tables)"))

(defun parser-generator-lr-test--generate-goto-tables ()
  "Test `parser-generator-lr--generate-goto-tables'."
  (message "Starting tests for (parser-generator-lr--generate-goto-tables)")

  ;; Example 5.30, p. 389
  (parser-generator-set-grammar
   '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (let ((table-lr-items
         (parser-generator-lr--generate-goto-tables)))

    (parser-generator--debug
     (message
      "GOTO-table: %s"
      (parser-generator-lr--get-expanded-goto-tables)))
    (should
     (equal
      '((0 ((S 1)))
        (1 ((a 2)))
        (2 ((S 3)))
        (3 ((a 4) (b 5)))
        (4 ((S 6)))
        (5 nil)
        (6 ((a 4) (b 7)))
        (7 nil))
      (parser-generator-lr--get-expanded-goto-tables)))
    (message "Passed GOTO-tables")

    (parser-generator--debug
     (message
      "LR-items: %s"
      (parser-generator--hash-to-list
       table-lr-items)))
    (should
     (equal
      '((0 (((S) nil (S a S b) ($)) ((S) nil (S a S b) (a)) ((S) nil nil ($)) ((S) nil nil (a)) ((Sp) nil (S) ($))))
        (1 (((S) (S) (a S b) ($)) ((S) (S) (a S b) (a)) ((Sp) (S) nil ($))))
        (2 (((S) (S a) (S b) ($)) ((S) (S a) (S b) (a)) ((S) nil (S a S b) (a)) ((S) nil (S a S b) (b)) ((S) nil nil (a)) ((S) nil nil (b))))
        (3 (((S) (S) (a S b) (a)) ((S) (S) (a S b) (b)) ((S) (S a S) (b) ($)) ((S) (S a S) (b) (a))))
        (4 (((S) (S a) (S b) (a)) ((S) (S a) (S b) (b)) ((S) nil (S a S b) (a)) ((S) nil (S a S b) (b)) ((S) nil nil (a)) ((S) nil nil (b))))
        (5 (((S) (S a S b) nil ($)) ((S) (S a S b) nil (a))))
        (6 (((S) (S) (a S b) (a)) ((S) (S) (a S b) (b)) ((S) (S a S) (b) (a)) ((S) (S a S) (b) (b))))
        (7 (((S) (S a S b) nil (a)) ((S) (S a S b) nil (b)))))
      (parser-generator--hash-to-list
       table-lr-items)))
    (message "Passed LR-items"))

  (message "Passed LR-items for example 5.30")

  ;; Example 5.30, p. 389 but with terminals as strings
  (parser-generator-set-grammar
   '((Sp S) ("a" "b") ((Sp S) (S (S "a" S "b")) (S e)) Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)

  (let ((table-lr-items
         (parser-generator-lr--generate-goto-tables)))

    ;; (message "GOTO-table: %s" (parser-generator-lr--get-expanded-goto-tables))
    ;; (message "LR-items: %s" (parser-generator--hash-to-list parser-generator-lr--items))

    (should
     (equal
      '((0 ((S 1)))
        (1 (("a" 2)))
        (2 ((S 3)))
        (3 (("a" 4) ("b" 5)))
        (4 ((S 6)))
        (5 nil)
        (6 (("a" 4) ("b" 7)))
        (7 nil))
      (parser-generator-lr--get-expanded-goto-tables)))
    (message "Passed GOTO-tables with tokens as strings")

    (should
     (equal
      '((0 (((S) nil (S "a" S "b") ($)) ((S) nil (S "a" S "b") ("a")) ((S) nil nil ($)) ((S) nil nil ("a")) ((Sp) nil (S) ($))))
        (1 (((S) (S) ("a" S "b") ($)) ((S) (S) ("a" S "b") ("a")) ((Sp) (S) nil ($))))
        (2 (((S) (S "a") (S "b") ($)) ((S) (S "a") (S "b") ("a")) ((S) nil (S "a" S "b") ("a")) ((S) nil (S "a" S "b") ("b")) ((S) nil nil ("a")) ((S) nil nil ("b"))))
        (3 (((S) (S) ("a" S "b") ("a")) ((S) (S) ("a" S "b") ("b")) ((S) (S "a" S) ("b") ($)) ((S) (S "a" S) ("b") ("a"))))
        (4 (((S) (S "a") (S "b") ("a")) ((S) (S "a") (S "b") ("b")) ((S) nil (S "a" S "b") ("a")) ((S) nil (S "a" S "b") ("b")) ((S) nil nil ("a")) ((S) nil nil ("b"))))
        (5 (((S) (S "a" S "b") nil ($)) ((S) (S "a" S "b") nil ("a"))))
        (6 (((S) (S) ("a" S "b") ("a")) ((S) (S) ("a" S "b") ("b")) ((S) (S "a" S) ("b") ("a")) ((S) (S "a" S) ("b") ("b"))))
        (7 (((S) (S "a" S "b") nil ("a")) ((S) (S "a" S "b") nil ("b")))))
      (parser-generator--hash-to-list table-lr-items)))
    (message "Passed LR-items with tokens as strings"))

  (message "Passed LR-items for example 5.30 but with tokens as strings")

  (message "Passed tests for (parser-r--generate-goto-tables)"))

(defun parser-generator-lr-test--items-for-prefix ()
  "Test `parser-generator-lr--items-for-prefix'."
  (message "Starting tests for (parser-generator-lr--items-for-prefix)")

  ;; Example 5.29 p 387
  (parser-generator-set-grammar
   '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)

  (should
   (equal
    '(((S) nil (S a S b) ($))
      ((S) nil (S a S b) (a))
      ((S) nil nil ($))
      ((S) nil nil (a))
      ((Sp) nil (S) ($)))
    (parser-generator-lr--items-for-prefix 'e)))
  (message "Passed V(e)")

  (should
   (equal
    '(((S) (S) (a S b) ($))
      ((S) (S) (a S b) (a))
      ((Sp) (S) nil ($)))
    (parser-generator-lr--items-for-prefix 'S)))
  (message "Passed V(S)")

  (should
   (equal
    nil
    (parser-generator-lr--items-for-prefix 'a)))
  (message "Passed V(a)")

  (should
   (equal
    nil
    (parser-generator-lr--items-for-prefix 'b)))
  (message "Passed V(b)")

  (should
   (equal
    '(((S) (S a) (S b) ($))
      ((S) (S a) (S b) (a))
      ((S) nil (S a S b) (a))
      ((S) nil (S a S b) (b))
      ((S) nil nil (a))
      ((S) nil nil (b)))
    (parser-generator-lr--items-for-prefix '(S a))))
  (message "Passed V(Sa)")

  (should
   (equal
    nil
    (parser-generator-lr--items-for-prefix '(S S))))
  (message "Passed V(SS)")

  (should
   (equal
    nil
    (parser-generator-lr--items-for-prefix '(S b))))
  (message "Passed V(Sb)")

  ;; a3 p. 390
  (should
   (equal
    '(((S) (S) (a S b) (a))
      ((S) (S) (a S b) (b))
      ((S) (S a S) (b) ($))
      ((S) (S a S) (b) (a)))
    (parser-generator-lr--items-for-prefix '(S a S))))
  (message "Passed V(SaS)")

  (should
   (equal
    nil
    (parser-generator-lr--items-for-prefix '(S a a))))
  (message "Passed V(Saa)")

  (should
   (equal
    nil
    (parser-generator-lr--items-for-prefix '(S a b))))
  (message "Passed V(Sab)")

  (message "Passed tests for (parser-generator-lr--items-for-prefix)"))

(defun parser-generator-lr-test--items-valid-p ()
  "Test `parser-generator-lr--items-valid-p'."
  (message "Started tests for (parser-generator-lr--items-valid-p)")

  (parser-generator-set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator-set-look-ahead-number 1)
  

  (let ((table-lr-items (parser-generator-process-grammar)))
    (parser-generator-lr-generate-parser-tables)

    (should
     (equal
      t
      (parser-generator-lr--items-valid-p (parser-generator--hash-values-to-list table-lr-items t))))

    (message "Passed first"))

  (should
   (equal
    nil
    (parser-generator-lr--items-valid-p
     '(((S nil (S a S b) (a)) (S nil (S a S b) (e)) (S nil nil (a)) (S nil nil (e)) (Sp nil (S) (e))) ((S (S) (a S b) (a)) (S (S) (a S b) (e)) (Sp (S) nil (e))) ((S (S a) (S b) (a)) (S (S a) (S b) (e)) (S nil (S a S b) (a)) (S nil (S a S b) (b)) (S nil nil (a)) (S nil nil (b))) ((S (S) (a S b) (a)) (S (S) (a S b) (b)) (S (S a S) (b) (a)) (S (S a S) (b) (e))) ((S (S a S b) nil (a)) (S (S a S b) (a) (a)) (S (S a S b) nil (e))) ((S (S a) (S b) (a)) (S (S a) (S b) (b)) (S nil (S a S b) (a)) (S nil (S a S b) (b)) (S nil nil (a)) (S nil nil (b))) ((S (S) (a S b) (a)) (S (S) (a S b) (b)) (S (S a S) (b) (a)) (S (S a S) (b) (b))) ((S (S a S b) nil (a)) (S (S a S b) nil (b)))))))

  (message "Passed tests for (parser-generator-lr--items-valid-p)"))

(defun parser-generator-lr-test-parse ()
  "Test `parser-generator-lr-parse'."
  (message "Started tests for (parser-generator-lr-parse)")

  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-grammar
   '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (let* ((string '((a 1 . 2) (a 2 . 3) (b 3 . 4) (b 4 . 5)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (list (car tokens) nil index nil))))
  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (car token)))
  (should
   (equal
    '(2 2 2 1 1)
    (parser-generator-lr-parse)))
  (message "Passed test with terminals as symbols")

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (let* ((string '((a 1 . 2) (a 2 . 3) (b 3 . 4) (b 4 . 5) (b 5 . 6)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (list (car tokens) nil index nil))))

  (should-error
   (parser-generator-lr--parse t))
  (message "Passed test with terminals as symbols, invalid syntax")

  (parser-generator-set-grammar '((Sp S) ("a" "b") ((Sp S) (S (S "a" S "b")) (S e)) Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (let ((lr-items (parser-generator-lr-generate-parser-tables)))
    (parser-generator--debug
     (message "lr-items: %s" (parser-generator--hash-values-to-list lr-items t)))
    )
  (parser-generator--debug
   (message "goto-tables: %s" (parser-generator-lr--get-expanded-goto-tables))
   (message "action-tables: %s" (parser-generator-lr--get-expanded-action-tables)))

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (let* ((string '(("a" 1 . 2) ("a" 2 . 3) ("b" 3 . 4) ("b" 4 . 5)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (list (car tokens) nil index nil))))

  (should
   (equal
    '(2 2 2 1 1)
    (parser-generator-lr-parse)))
  (message "Passed test with terminals as string")

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (let* ((string '(("a" 1 . 2) ("a" 2 . 3) ("b" 3 . 4) ("b" 4 . 5) ("b" 5 . 6)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (list (car tokens) nil index nil))))

  (should-error
   (parser-generator-lr--parse t))
  (message "Passed test with terminals as string, invalid syntax")

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (let* ((string '(("a" 1 . 2) ("a" 2 . 3) ("b" 3 . 4) ("b" 4 . 5)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (list (car tokens) nil index nil))))

  (parser-generator-lr-test--parse-incremental-vs-regular)
  (message "Passed incremental-tests")


  ;; Test left-recursive grammar from PHP 8.0 here
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-e-identifier '%empty)
  (parser-generator-set-grammar
   '(
     (start expr match match_arm_list non_empty_match_arm_list match_arm match_arm_cond_list possible_comma)
     (T_DEFAULT T_MATCH "(" ")" "{" "}" "," T_DOUBLE_ARROW number)
     (
      (start
       expr
       )
      (expr
       number
       match)
      (match
       (T_MATCH "(" expr ")" "{" match_arm_list "}")
       )
      (match_arm_list
       %empty
       (non_empty_match_arm_list possible_comma)
       )
      (non_empty_match_arm_list
       match_arm
       (non_empty_match_arm_list "," match_arm)
       )
      (match_arm
       (match_arm_cond_list possible_comma T_DOUBLE_ARROW expr)
       (T_DEFAULT possible_comma T_DOUBLE_ARROW expr)
       )
      (match_arm_cond_list
       expr
       (match_arm_cond_list "," expr)
       )
      (possible_comma
       %empty
       ",")
      )
     start
     )
   )
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (with-current-buffer "*PHP8.0*"
       (let ((token)
             (move-to-index-flag))
         (goto-char index)
         (cond
          ((looking-at "[ \n\t]+")
           (setq
            move-to-index-flag
            (match-end 0)))
          ((or
            (looking-at "{")
            (looking-at "}")
            (looking-at ",")
            (looking-at "(")
            (looking-at ")"))
           (setq
            token
            `(
              ,(buffer-substring-no-properties
                (match-beginning 0)
                (match-end 0))
              ,(match-beginning 0)
              . ,(match-end 0)
              )
            ))
          ((looking-at "default")
           (setq
            token
            `(
              T_DEFAULT
              ,(match-beginning 0)
              . ,(match-end 0))
            )
           )
          ((looking-at "match")
           (setq
            token
            `(
              T_MATCH
              ,(match-beginning 0)
              . ,(match-end 0))
            )
           )
          ((looking-at "=>")
           (setq
            token
            `(
              T_DOUBLE_ARROW
              ,(match-beginning 0)
              . ,(match-end 0))
            )
           )
          ((looking-at "[0-9]+")
           (setq
            token
            `(
              number
              ,(match-beginning 0)
              . ,(match-end 0))
            )
           )
          )
         (list token move-to-index-flag (match-end 0) nil)))))

  (let ((buffer (generate-new-buffer "*PHP8.0*")))
    (with-current-buffer buffer
      (kill-region (point-min) (point-max))
      (insert "match (55) {\n    default => 33,\n}")
      (parser-generator-lr--parse)
      (kill-buffer)
      (message "Passed test PHP 8.0 match grammar 1")
      ))
  (let ((buffer (generate-new-buffer "*PHP8.0*")))
    (with-current-buffer buffer
      (kill-region (point-min) (point-max))
      (insert "match (55) {\n    22,33 => 22,\n    25 => 20,\n    default => 33\n}")
      (parser-generator-lr--parse)
      (kill-buffer)
      (message "Passed test PHP 8.0 match grammar 2")
      ))

  ;; Test another left-recursive grammar from PHP 8.0 here
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-e-identifier '%empty)
  (parser-generator-set-grammar
   '(
     (start inner_statement_list statement switch_case_list case_list case_separator)
     (T_SWITCH T_ECHO T_CONSTANT_ENCAPSED_STRING ";" ":" "{" "}" T_CASE)
     (
      (start
       inner_statement_list
       )
      (inner_statement_list
       (inner_statement_list statement)
       %empty
       )
      (statement
       (T_SWITCH switch_case_list)
       (T_ECHO T_CONSTANT_ENCAPSED_STRING ";")
       )
      (switch_case_list
       ("{" case_list "}")
       ("{" ";" case_list "}")
       )
      (case_list
       %empty
       (case_list T_CASE case_separator inner_statement_list)
       )
      (case_separator
       ":"
       ";"
       )
      )
     start
     )
   )
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)
  
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (with-current-buffer "*PHP8.0*"
       (let ((token)
             (move-to-index-flag))
         (goto-char index)
         (cond
          ((looking-at "[ \n\t]+")
           (setq
            move-to-index-flag
            (match-end 0)))
          ((looking-at "\\(\".+\"\\)")
           (setq
            token
            `(
              T_CONSTANT_ENCAPSED_STRING
              ,(match-beginning 0)
              . ,(match-end 0)
              )
            )
           )
          ((looking-at "case")
           (setq
            token
            `(
              T_CASE
              ,(match-beginning 0)
              . ,(match-end 0))
            )
           )
          ((looking-at "switch")
           (setq
            token
            `(
              T_SWITCH
              ,(match-beginning 0)
              . ,(match-end 0))
            )
           )
          ((looking-at "echo")
           (setq
            token
            `(
              T_ECHO
              ,(match-beginning 0)
              . ,(match-end 0))
            )
           )
          ((looking-at "[;{}:]")
           (setq
            token
            `(
              ,(match-string-no-properties 0)
              ,(match-beginning 0)
              . ,(match-end 0))
            )
           )
          )
         (list token move-to-index-flag (match-end 0) nil)))))

  (let ((buffer (generate-new-buffer "*PHP8.0*")))
    (with-current-buffer buffer
      (kill-region (point-min) (point-max))
      (insert "switch\n{\n    case:\n        echo \"hello\";\n}\n")
      (parser-generator-lr--parse)
      (kill-buffer)
      (message "Passed test PHP 8.0 switch case grammar 1")
      ))
  (let ((buffer (generate-new-buffer "*PHP8.0*")))
    (with-current-buffer buffer
      (kill-region (point-min) (point-max))
      (insert "switch\n{\n    case:\n    case:\n    case;\n    case;\n        echo \"hello\";\n}\n")
      (parser-generator-lr--parse)
      (kill-buffer)
      (message "Passed test PHP 8.0 switch case grammar 2")))

  (message "Passed tests for (parser-generator-lr--parse)"))

(defun parser-generator-lr-test-parse-k-2 ()
  "Test `parser-generator-lr-parse' with k = 2."
  (message "Started tests for (parser-generator-lr-parse) k = 2")

  ;; https://stackoverflow.com/questions/62075086/what-is-an-lr2-parser-how-does-it-differ-from-an-lr1-parser
  ;; S → RS | R
  ;; R → abT
  ;; T → aT | c | ε

  (parser-generator-set-grammar
   '((Sp S R T) (a b c) ((Sp S) (S (R S) (R)) (R (a b T)) (T (a T) (c) (e))) Sp))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)

  (let ((lr-items (parser-generator-lr--generate-goto-tables)))
    (parser-generator--debug
     (message
      "LR-items: %s"
      (parser-generator--hash-to-list
       lr-items)))

    ;;     (1)
    ;;     S' -> .S   [$$]  // Go to 10
    ;;     S  -> .R   [$$]  // Go to 8
    ;;     S  -> .RS  [$$]  // Go to 8
    ;;     R  -> .abT [$$]  // Shift  on ab, go to (2)
    ;;     R  -> .abT [ab]  // Shift  on ab, go to (2)

    ;; (2)
    ;;     R  -> a.bT [$$]  // Shift  on ba, bc, b$, go to (3)
    ;;     R  -> a.bT [ab]  // Shift  on ba, bc,     go to (3)

    ;; (3)
    ;;     R  -> ab.T [$$] // Go to 7
    ;;     R  -> ab.T [ab] // Go to 7
    ;;     T  -> .aT  [$$] // Shift  on aa, ac, a$, go to (4)
    ;;     T  -> .c   [$$] // Shift  on c$,         go to (5)
    ;;     T  -> .    [$$] // Reduce on $$
    ;;     T  -> .aT  [ab] // Shift  on aa, ac,     go to (4)
    ;;     T  -> .c   [ab] // Shift  on ca,         go to (5)
    ;;     T  -> .    [ab] // Reduce on ab

    ;; (4)
    ;;     T  -> a.T  [$$] // Go to 6
    ;;     T  -> a.T  [ab] // Go to 6
    ;;     T  -> .    [$$] // Reduce on $$
    ;;     T  -> .aT  [$$] // Shift  on aa, ac, a$, go to (4)
    ;;     T  -> .c   [$$] // Shift  on c$,         go to (5)
    ;;     T  -> .    [ab] // Reduce on ab
    ;;     T  -> .aT  [ab] // Shift  on aa, ac,     go to (4)
    ;;     T  -> .c   [ab] // Shift  on ca,         go to (5)

    ;; (5)
    ;;     T  -> c.   [$$] // Reduce on $$
    ;;     T  -> c.   [ab] // Reduce on ab

    ;; (6)
    ;;     T  -> aT.  [$$] // Reduce on $$ 
    ;;     T  -> aT.  [ab] // Reduce on ab

    ;; (7)
    ;;     R  -> abT. [$$] // Reduce on $$
    ;;     R  -> abT. [ab] // Reduce on ab

    ;; (8)
    ;;     S  -> R.   [$$] // Reduce on $$
    ;;     S  -> R.S  [$$] // Go to 9
    ;;     S  -> .RS  [$$] // Go to 8
    ;;     S  -> .R   [$$] // Go to 8
    ;;     R  -> .abT [$$] // Shift  on ab, go to (2)
    ;;     R  -> .abT [ab] // Shift  on ab, go to (2)

    ;; (9)
    ;;     S  -> RS.  [$$] // Reduce on $$

    ;; (10)
    ;;     S' -> S.   [$$] // Accept on $$

    (should
     (equal
      '((0 (
            ((R) nil (a b T) ($ $))
            ((R) nil (a b T) (a b))
            ((S) nil (R) ($ $))
            ((S) nil (R S) ($ $))
            ((Sp) nil (S) ($ $))
            ))
        (1 (
            ((R) nil (a b T) ($ $))
            ((R) nil (a b T) (a b))
            ((S) (R) (S) ($ $))
            ((S) (R) nil ($ $))
            ((S) nil (R) ($ $))
            ((S) nil (R S) ($ $))))
        (2 (
            ((Sp) (S) nil ($ $))))
        (3 (
            ((R) (a) (b T) ($ $))
            ((R) (a) (b T) (a b))
            ))
        (4 (
            ((R) (a b) (T) ($ $))
            ((R) (a b) (T) (a b))
            ((T) nil (a T) ($ $))
            ((T) nil (a T) (a b))
            ((T) nil (c) ($ $))
            ((T) nil (c) (a b))
            ((T) nil nil ($ $))
            ((T) nil nil (a b))
            ))
        (5 (
            ((R) (a b T) nil ($ $))
            ((R) (a b T) nil (a b))
            ))
        (6 (
            ((T) (a) (T) ($ $))
            ((T) (a) (T) (a b))
            ((T) nil (a T) ($ $))
            ((T) nil (a T) (a b))
            ((T) nil (c) ($ $))
            ((T) nil (c) (a b))
            ((T) nil nil ($ $))
            ((T) nil nil (a b))
            ))
        (7 (
            ((T) (c) nil ($ $))
            ((T) (c) nil (a b))
            ))
        (8 (
            ((T) (a T) nil ($ $))
            ((T) (a T) nil (a b))))
        (9 (
            ((S) (R S) nil ($ $)))))
      (parser-generator--hash-to-list
       lr-items)))
    (message "Passed LR-items k = 2")

    (parser-generator--debug
     (message "GOTO-tables k = 2: %s"
              (parser-generator-lr--get-expanded-goto-tables)))

    ;; state |  a  |  b  |  c  |  $  |  S  |  R  |  T
    ;; -------+-----+-----+-----+-----+-----+-----+-----
    ;;    1   |  2  |     |     |     |  10 |  8  |
    ;; -------+-----+-----+-----+-----+-----+-----+-----
    ;;    2   |     |  3  |     |     |     |     |
    ;; -------+-----+-----+-----+-----+-----+-----+-----
    ;;    3   |  4  |     |  5  |     |     |     |  7
    ;; -------+-----+-----+-----+-----+-----+-----+-----
    ;;    4   |  4  |     |  5  |     |     |     |  6
    ;; -------+-----+-----+-----+-----+-----+-----+-----
    ;;    5   |     |     |     |     |     |     |
    ;; -------+-----+-----+-----+-----+-----+-----+-----
    ;;    6   |     |     |     |     |     |     |
    ;; -------+-----+-----+-----+-----+-----+-----+-----
    ;;    7   |     |     |     |     |     |     |
    ;; -------+-----+-----+-----+-----+-----+-----+-----
    ;;    8   |  2  |     |     |     |  9  |  8  |
    ;; -------+-----+-----+-----+-----+-----+-----+-----
    ;;    9   |     |     |     |     |     |     |
    ;; -------+-----+-----+-----+-----+-----+-----+-----
    ;;    10  |     |     |     |     |     |     |

    (should
     (equal
      '((0 ((R 1) (S 2) (a 3)))
        (1 ((R 1) (S 9) (a 3)))
        (2 nil)
        (3 ((b 4)))
        (4 ((T 5) (a 6) (c 7)))
        (5 nil)
        (6 ((T 8) (a 6) (c 7)))
        (7 nil)
        (8 nil)
        (9 nil))
      (parser-generator-lr--get-expanded-goto-tables)))
    (message "Passed GOTO-tables k = 2")

    ;; state | aa | ab | ac | a$ | ba | bb | bc | b$ | ca | cb | cc | c$ | $$ 
    ;; ------+----+----+----+----+----+----+----+----+----+----+----+----+----
    ;;   1   |    | S  |    |    |    |    |    |    |    |    |    |    |
    ;; ------+----+----+----+----+----+----+----+----+----+----+----+----+----
    ;;   2   |    |    |    |    | S  |    | S  | S  |    |    |    |    |
    ;; ------+----+----+----+----+----+----+----+----+----+----+----+----+----
    ;;   3   | S  | R  | S  | S  |    |    |    |    | S  |    |    | S  | R
    ;; ------+----+----+----+----+----+----+----+----+----+----+----+----+----
    ;;   4   | S  | R  | S  | S  |    |    |    |    | S  |    |    | S  | R
    ;; ------+----+----+----+----+----+----+----+----+----+----+----+----+----
    ;;   5   |    | R  |    |    |    |    |    |    |    |    |    |    | R
    ;; ------+----+----+----+----+----+----+----+----+----+----+----+----+----
    ;;   6   |    | R  |    |    |    |    |    |    |    |    |    |    | R
    ;; ------+----+----+----+----+----+----+----+----+----+----+----+----+----
    ;;   7   |    | R  |    |    |    |    |    |    |    |    |    |    | R
    ;; ------+----+----+----+----+----+----+----+----+----+----+----+----+----
    ;;   8   |    | S  |    |    |    |    |    |    |    |    |    |    | R
    ;; ------+----+----+----+----+----+----+----+----+----+----+----+----+----
    ;;   9   |    |    |    |    |    |    |    |    |    |    |    |    | R
    ;; ------+----+----+----+----+----+----+----+----+----+----+----+----+----
    ;;   10  |    |    |    |    |    |    |    |    |    |    |    |    | A

    (parser-generator-lr--generate-action-tables
     lr-items)
    (parser-generator--debug
     (message
      "Action-tables k = 2: %S"
      (parser-generator-lr--get-expanded-action-tables)))
    (should
     (equal
      '(
        (0 (((a b) shift)))
        (1 ((($ $) reduce 2) ((a b) shift)))
        (2 ((($ $) accept)))
        (3 (((b c) shift) ((b a) shift) ((b $) shift)))
        (4 ((($ $) reduce 6) ((a b) reduce 6) ((a c) shift) ((a a) shift) ((a $) shift) ((c a) shift) ((c $) shift)))
        (5 ((($ $) reduce 3) ((a b) reduce 3)))
        (6 ((($ $) reduce 6) ((a b) reduce 6) ((a c) shift) ((a a) shift) ((a $) shift) ((c a) shift) ((c $) shift)))
        (7 ((($ $) reduce 5) ((a b) reduce 5)))
        (8 ((($ $) reduce 4) ((a b) reduce 4)))
        (9 ((($ $) reduce 1)))
        )
      (parser-generator-lr--get-expanded-action-tables)))
    (message "Passed ACTION-tables k = 2")

    )

  (let ((buffer (generate-new-buffer "*a*")))
    (switch-to-buffer buffer)
    (kill-region (point-min) (point-max))
    (insert "abac")

    (parser-generator-set-grammar
     '((Sp S R T) ("a" "b" "c") ((Sp S) (S (R S) (R)) (R ("a" "b" T)) (T ("a" T) ("c") (e))) Sp))
    (parser-generator-set-look-ahead-number 2)
    (parser-generator-process-grammar)
    (parser-generator-lr-generate-parser-tables)

    ;; Setup lex-analyzer
    (setq
     parser-generator-lex-analyzer--function
     (lambda (index _state)
       (with-current-buffer buffer
         (when (<= (+ index 1) (point-max))
           (let ((start index)
                 (end (+ index 1)))
             (let ((token (buffer-substring-no-properties start end)))
               (list `(,token ,start . ,end) nil end nil)))))))

    (setq
     parser-generator-lex-analyzer--get-function
     (lambda (token)
       (with-current-buffer buffer
         (let ((start (car (cdr token)))
               (end (cdr (cdr token))))
           (when (<= end (point-max))
             (buffer-substring-no-properties start end))))))

    (should
     (equal
      '(5 4 3 2)
      (parser-generator-lr-parse)))

    (message "Passed parse with k = 2 # 1")

    (switch-to-buffer buffer)
    (kill-region (point-min) (point-max))
    (insert "aba")

    (should
     (equal
      '(6 4 3 2)
      (parser-generator-lr-parse)))

    (message "Passed parse with k = 2 # 2")

    (kill-buffer))

  (let ((buffer (generate-new-buffer "*a*")))
    (switch-to-buffer buffer)
    (kill-region (point-min) (point-max))
    (insert "abac")

    (parser-generator-set-grammar
     '(
       (Sp S R T)
       ("a" "b" "c")
       (
        (Sp S)
        (S (R S) (R))
        (R ("a" "b" T (lambda(args _terminals) (list "begin" (nth 2 args) "end"))))
        (T ("a" T (lambda(args _terminals) "test")) ("c") (e)
           )
        )
       Sp)
     )
    (parser-generator-set-look-ahead-number 2)
    (parser-generator-process-grammar)
    (parser-generator-lr-generate-parser-tables)

    ;; Setup lex-analyzer
    (setq
     parser-generator-lex-analyzer--function
     (lambda (index _state)
       (with-current-buffer buffer
         (when (<= (+ index 1) (point-max))
           (let ((start index)
                 (end (+ index 1)))
             (let ((token (buffer-substring-no-properties start end)))
               (list `(,token ,start . ,end) nil end nil)))))))
    (setq
     parser-generator-lex-analyzer--get-function
     (lambda (token)
       (with-current-buffer buffer
         (let ((start (car (cdr token)))
               (end (cdr (cdr token))))
           (when (<= end (point-max))
             (buffer-substring-no-properties start end))))))

    (should
     (equal
      '("begin" "test" "end")
      (parser-generator-lr-translate)))

    (message "Passed translation k=2")
    
    (kill-buffer))


  (message "Passed tests for (parser-generator-lr--parse-k-2)"))

(defun parser-generator-lr-test-parse-k-0 ()
  "Test `parser-generator-lr-parse' with k = 0."
  (message "Started tests for (parser-generator-lr-parse) k = 0")

  ;; https://en.wikipedia.org/wiki/LR_parser#Constructing_LR(0)_parsing_tables
  ;; (0) S → E eof
  ;; (1) E → E * B
  ;; (2) E → E + B
  ;; (3) E → B
  ;; (4) B → 0
  ;; (5) B → 1

  (parser-generator-set-grammar
   '(
     (S E B)
     ("*" "+" "0" "1")
     (
      (S (E $))
      (E (E "*" B) (E "+" B) (B))
      (B ("0") ("1"))
      )
     S
     )
   )
  (parser-generator-set-e-identifier nil)
  (parser-generator-set-look-ahead-number 0)
  (parser-generator-process-grammar)

  (let ((lr-items (parser-generator-lr--generate-goto-tables)))
    (parser-generator--debug
     (message
      "LR-items: %s"
      (parser-generator--hash-to-list
       lr-items)))

    ;; Item set 0
    ;; S → • E eof
    ;; + E → • E * B
    ;; + E → • E + B
    ;; + E → • B
    ;; + B → • 0
    ;; + B → • 1

    ;; Item set 1
    ;; B → 0 •

    ;; Item set 2
    ;; B → 1 •

    ;; Item set 3
    ;; S → E • eof
    ;; E → E • * B
    ;; E → E • + B

    ;; Item set 4
    ;; E → B •

    ;; Item set 5
    ;; E → E * • B
    ;; + B → • 0
    ;; + B → • 1

    ;; Item set 6
    ;; E → E + • B
    ;; + B → • 0
    ;; + B → • 1

    ;; Item set 7
    ;; E → E * B •

    ;; Item set 8
    ;; E → E + B •

    (should
     (equal
      '((0 (
            ((B) nil ("0"))
            ((B) nil ("1"))
            ((E) nil (B))
            ((E) nil (E "+" B))
            ((E) nil (E "*" B))
            ((S) nil (E $))
            ))
        (1 (((B) ("0") nil)))
        (2 (((B) ("1") nil)))
        (3 (((E) (B) nil)))
        (4 (
            ((E) (E) ("*" B))
            ((E) (E) ("+" B))
            ((S) (E) ($))
            ))
        (5 (
            ((B) nil ("0"))
            ((B) nil ("1"))
            ((E) (E "*") (B))
            ))
        (6 (
            ((B) nil ("0"))
            ((B) nil ("1"))
            ((E) (E "+") (B))
            ))
        (7 (((E) (E "+" B) nil)))
        (8 (((E) (E "*" B) nil)))
        )
      (parser-generator--hash-to-list
       lr-items)))
    (message "Passed LR-items k = 0")

    (parser-generator--debug
     (message
      "GOTO-tables k = 0: %s"
      (parser-generator-lr--get-expanded-goto-tables)))

    ;; 	* 	+ 	0 	1 	E 	B
    ;; 0 	  	  	1 	2 	3 	4
    ;; 1 	  	  	  	  	  	 
    ;; 2 	  	  	  	  	  	 
    ;; 3 	5 	6 	  	  	  	 
    ;; 4 	  	  	  	  	  	 
    ;; 5 	  	  	1 	2 	  	7
    ;; 6 	  	  	1 	2 	  	8
    ;; 7 	  	  	  	  	  	 
    ;; 8 	  	  	  	  	  	  

    (should
     (equal
      '((0 (("0" 1) ("1" 2) (B 3) (E 4))) ;; 3-4
        (1 nil)
        (2 nil)
        (3 nil)
        (4 (("*" 5) ("+" 6))) ;; 5-6
        (5 (("0" 1) ("1" 2) (B 8))) ;;7-8
        (6 (("0" 1) ("1" 2) (B 7))) ;; 7-8
        (7 nil)
        (8 nil))
      (parser-generator-lr--get-expanded-goto-tables)))
    (message "Passed GOTO-tables k = 0")

    ;;   	*  	+  	0  	1  	$
    ;; 0 	   	   	s1 	s2 	
    ;; 1 	r4 	r4 	r4 	r4 	r4
    ;; 2 	r5 	r5 	r5 	r5 	r5
    ;; 3 	s5 	s6 	   	   	acc
    ;; 4 	r3 	r3 	r3 	r3 	r3
    ;; 5 	   	   	s1 	s2 	
    ;; 6 	   	   	s1 	s2 	
    ;; 7 	r1 	r1 	r1 	r1 	r1
    ;; 8 	r2 	r2 	r2 	r2 	r2

    (parser-generator-lr--generate-action-tables
     lr-items)
    (parser-generator--debug
     (message
      "Action-tables k = 0: %s"
      (parser-generator-lr--get-expanded-action-tables)))

    (should
     (equal
      '(
        (0 ((("0") shift) (("1") shift)))
        (1 ((nil reduce 4)))
        (2 ((nil reduce 5)))
        (3 ((nil reduce 3)))
        (4 ((($) accept) (("*") shift) (("+") shift)))
        (5 ((("0") shift) (("1") shift)))
        (6 ((("0") shift) (("1") shift)))
        (7 ((nil reduce 2)))
        (8 ((nil reduce 1))))
      (parser-generator-lr--get-expanded-action-tables)))
    (message "Passed ACTION-tables k = 0"))

  (let ((buffer (generate-new-buffer "*a*")))
    (switch-to-buffer buffer)
    (kill-region (point-min) (point-max))
    (insert "1+1")

    (parser-generator-set-grammar
     '((S E B) ("*" "+" "0" "1") ((S (E $)) (E (E "*" B) (E "+" B) (B)) (B ("0") ("1"))) S))
    (parser-generator-set-look-ahead-number 0)
    (parser-generator-process-grammar)
    (parser-generator-lr-generate-parser-tables)

    ;; Setup lex-analyzer
    (setq
     parser-generator-lex-analyzer--function
     (lambda (index _state)
       (with-current-buffer buffer
         (when (<= (+ index 1) (point-max))
           (let ((start index)
                 (end (+ index 1)))
             (let ((token (buffer-substring-no-properties start end)))
               (list `(,token ,start . ,end) nil end nil)))))))

    (setq
     parser-generator-lex-analyzer--get-function
     (lambda (token)
       (with-current-buffer buffer
         (let ((start (car (cdr token)))
               (end (cdr (cdr token))))
           (when (<= end (point-max))
             (buffer-substring-no-properties
              start
              end))))))

    (should
     (equal
      '(5 3 5 2)
      (parser-generator-lr-parse)))
    (message "Passed parse with k = 0 # 1")

    (switch-to-buffer buffer)
    (kill-region (point-min) (point-max))
    (insert "1+1*1")

    (should
     (equal
      '(5 3 5 2 5 1)
      (parser-generator-lr-parse)))
    (message "Passed parse with k = 0 # 2")
    (kill-buffer))

  (let ((buffer (generate-new-buffer "*a*")))
    (switch-to-buffer buffer)
    (kill-region (point-min) (point-max))
    (insert "1+1")

    (parser-generator-set-grammar
     '(
       (S E B)
       ("*" "+" "0" "1")
       (
        (S (E $))
        (E
         (E "*" B
            (lambda(args _terminals)
              (let ((ret (list (nth 0 args)))) (when (nth 2 args) (setq ret (append ret `(" x " ,(nth 2 args))))) ret)))
         (E "+" B
            (lambda(args _terminals)
              (let ((ret (list (nth 0 args)))) (when (nth 2 args) (setq ret (append ret `(" . " ,(nth 2 args))))) ret)))
         (B)
         )
        (B
         ("0")
         ("1"))
        )
       S))
    (parser-generator-set-look-ahead-number 0)
    (parser-generator-process-grammar)
    (parser-generator-lr-generate-parser-tables)

    ;; Setup lex-analyzer
    (setq
     parser-generator-lex-analyzer--function
     (lambda (index _state)
       (with-current-buffer buffer
         (when (< index (point-max))
           (let ((start index)
                 (end (+ index 1)))
             (let ((token (buffer-substring-no-properties start end)))
               (list `(,token ,start . ,end) nil end nil)))))))

    (setq
     parser-generator-lex-analyzer--get-function
     (lambda (token)
       (with-current-buffer buffer
         (let ((start (car (cdr token)))
               (end (cdr (cdr token))))
           (when (<= end (point-max))
             (buffer-substring-no-properties start end))))))

    (should
     (equal
      '("1" " . " "1")
      (parser-generator-lr-translate)))
    (message "Passed translation k=0")
    (kill-buffer))

  (message "Passed tests for (parser-generator-lr--parse-k-0)"))

(defun parser-generator-lr-test-translate ()
  "Test `parser-generator-lr-translate'."
  (message "Started tests for (parser-generator-lr-translate)")

  ;; Test translation with terminals as strings here

  (let ((buffer (generate-new-buffer "*a*")))
    (switch-to-buffer buffer)
    (insert "aabb")

    (parser-generator-set-grammar
     '
     (
      (Sp S)
      ("a" "b")
      (
       (Sp S)
       (S
        (S "a" S "b"
           (lambda(args _terminals)
             (let ((list ""))
               (dolist (item args)
                 (when item
                   (setq list (format "%s%s" item list)))
                 )
               list))))
       (S e)
       )
      Sp))
    (parser-generator-set-look-ahead-number 1)
    (parser-generator-set-e-identifier 'e)
    (parser-generator-process-grammar)
    (parser-generator-lr-generate-parser-tables)

    (setq
     parser-generator-lex-analyzer--function
     (lambda (index _state)
       (with-current-buffer buffer
         (when (<= (+ index 1) (point-max))
           (let ((start index)
                 (end (+ index 1)))
             (let ((token (buffer-substring-no-properties start end)))
               (list `(,token ,start . ,end) nil end nil)))))))

    (setq
     parser-generator-lex-analyzer--get-function
     (lambda (token)
       (with-current-buffer buffer
         (let ((start (car (cdr token)))
               (end (cdr (cdr token))))
           (when (<= end (point-max))
             (buffer-substring-no-properties start end))))))

    (should
     (equal
      "bbaa"
      (parser-generator-lr-translate)))

    (kill-buffer buffer))
  (message "Passed test with translation 1")

  (let ((buffer (generate-new-buffer "*a*")))
    (switch-to-buffer buffer)
    (insert "if (a) { b; }")

    (parser-generator-set-grammar
     '
     (
      (Sp S)
      (";" OPEN_ROUND_BRACKET CLOSE_ROUND_BRACKET ECHO IF OPEN_CURLY_BRACKET CLOSE_CURLY_BRACKET VARIABLE)
      (
       (Sp S)
       (S (IF OPEN_ROUND_BRACKET VARIABLE CLOSE_ROUND_BRACKET OPEN_CURLY_BRACKET VARIABLE ";" CLOSE_CURLY_BRACKET (lambda(args _tokens) (format "(when %s %s)" (nth 2 args) (nth 5 args)))))
       )
      Sp
      )
     )
    (parser-generator-set-look-ahead-number 1)
    (parser-generator-process-grammar)
    (parser-generator-lr-generate-parser-tables)

    (setq
     parser-generator-lex-analyzer--function
     (lambda (index _state)
       (with-current-buffer buffer
         (unless (>= index (point-max))
           (goto-char index)
           (unless (looking-at "[^ \n\t]")
             (search-forward-regexp "[^ \n\t]" nil t nil)
             (forward-char -1))
           (let ((token))
             (cond
              ((looking-at "if")
               (setq token `(IF ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at "echo")
               (setq token `(ECHO ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at "(")
               (setq token `(OPEN_ROUND_BRACKET ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at ")")
               (setq token `(CLOSE_ROUND_BRACKET ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at "{")
               (setq token `(OPEN_CURLY_BRACKET ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at "}")
               (setq token `(CLOSE_CURLY_BRACKET ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at ";")
               (setq token `(";" ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at "[a-zA-Z]+")
               (setq token `(VARIABLE ,(match-beginning 0) . ,(match-end 0))))
              (t (error "Invalid syntax! Could not lex-analyze at %s!" (point))))
             (list token nil (match-end 0) nil))))))

    (setq
     parser-generator-lex-analyzer--get-function
     (lambda (token)
       (with-current-buffer buffer
         (let ((start (car (cdr token)))
               (end (cdr (cdr token))))
           (when (<= end (point-max))
             (buffer-substring-no-properties start end))))))

    (should
     (equal
      "(when a b)"
      (parser-generator-lr-translate)))
    (message "Passed test with non-nested translation")

    (switch-to-buffer buffer)
    (kill-region (point-min) (point-max))

    (parser-generator-set-grammar
     '(
       (Sp S T)
       (";" OPEN_ROUND_BRACKET CLOSE_ROUND_BRACKET ECHO IF OPEN_CURLY_BRACKET CLOSE_CURLY_BRACKET VARIABLE)
       (
        (Sp S)
        (S (IF OPEN_ROUND_BRACKET VARIABLE CLOSE_ROUND_BRACKET OPEN_CURLY_BRACKET T CLOSE_CURLY_BRACKET (lambda(args _terminals) (format "(when %s %s)" (nth 2 args) (nth 5 args)))))
        (T (ECHO VARIABLE ";" (lambda(args _terminals) (format "(message %s)" (nth 1 args)))) (VARIABLE ";" (lambda(args _terminals) (format "%s" (nth 0 args)))))
        )
       Sp
       )
     )
    (parser-generator-set-look-ahead-number 1)
    (parser-generator-process-grammar)
    (parser-generator-lr-generate-parser-tables)

    (insert "if (a) { echo b; }")

    (should
     (equal
      "(when a (message b))"
      (parser-generator-lr-translate)))

    (message "Passed test with nested-translation with depth 2")

    (switch-to-buffer buffer)
    (kill-region (point-min) (point-max))
    (goto-char 1)
    (insert "if (a) { echo b }")

    (should-error
     (parser-generator-lr-parse))

    (kill-buffer buffer))
  (message "Passed test with translation 2")

  (let ((buffer (generate-new-buffer "*a*")))
    (switch-to-buffer buffer)
    (insert "if (a) { b; }")

    (parser-generator-set-grammar
     '(
       (Sp S)
       (";" OPEN_ROUND_BRACKET CLOSE_ROUND_BRACKET IF OPEN_CURLY_BRACKET CLOSE_CURLY_BRACKET VARIABLE)
       (
        (Sp S)
        (S (IF OPEN_ROUND_BRACKET VARIABLE CLOSE_ROUND_BRACKET OPEN_CURLY_BRACKET VARIABLE ";" CLOSE_CURLY_BRACKET (lambda(args _terminals) (format "(when %s %s)" (nth 2 args) (nth 5 args)))))
        )
       Sp
       )
     )
    (parser-generator-set-look-ahead-number 1)
    (parser-generator-process-grammar)
    (parser-generator-lr-generate-parser-tables)

    (setq
     parser-generator-lex-analyzer--function
     (lambda (index _state)
       (with-current-buffer "*a*"
         (unless (>= index (point-max))
           (goto-char index)
           (unless (looking-at "[^ \n\t]")
             (search-forward-regexp "[^ \n\t]" nil t nil)
             (forward-char -1))
           (let ((token))
             (cond
              ((looking-at "if")
               (setq token `(IF ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at "(")
               (setq token `(OPEN_ROUND_BRACKET ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at ")")
               (setq token `(CLOSE_ROUND_BRACKET ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at "{")
               (setq token `(OPEN_CURLY_BRACKET ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at "}")
               (setq token `(CLOSE_CURLY_BRACKET ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at ";")
               (setq token `(";" ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at "[a-zA-Z]+")
               (setq token `(VARIABLE ,(match-beginning 0) . ,(match-end 0))))
              (t (error "Invalid syntax! Could not lex-analyze at %s!" (point))))
             (list token nil (match-end 0) nil))))))

    (setq
     parser-generator-lex-analyzer--get-function
     (lambda (token)
       (with-current-buffer "*a*"
         (let ((start (car (cdr token)))
               (end (cdr (cdr token))))
           (when (<= end (point-max))
             (buffer-substring-no-properties start end))))))

    (parser-generator-lr-test--parse-incremental-vs-regular)
    (kill-buffer buffer))

  (message "Passed incremental tests")

  (message "Passed tests for (parser-generator-lr-translate)"))

(defun parser-generator-lr-test ()
  "Run test."
  (setq debug-on-error nil)
  (setq debug-on-signal nil)

  (parser-generator-lr-test--items-for-prefix)
  (parser-generator-lr-test--items-valid-p)
  (parser-generator-lr-test--generate-goto-tables)
  (parser-generator-lr-test--generate-action-tables)
  (parser-generator-lr-test--generate-precedence-tables)
  (parser-generator-lr-test-parse)
  (parser-generator-lr-test-translate)
  (parser-generator-lr-test-parse-k-2)
  (parser-generator-lr-test-parse-k-0))


(provide 'parser-generator-lr-test)

;;; parser-generator-lr-test.el ends here
