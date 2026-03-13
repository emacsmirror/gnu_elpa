;;; tests/parse.el --- javaimp parsing tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert)
(require 'javaimp)
(require 'javaimp-tests)

;; Tests for parse helpers

(ert-deftest javaimp-parse-arglist ()
  (dolist (data '(("")
                  ("  ")
                  ("int i"
                   ("int" . "i"))
                  ("\nint\ni\n,\nint\nj\n"
                   ("int" . "i")
                   ("int" . "j"))
                  (" List<? extends Comparable<? super T>> list, T... elements"
                   ("List<? extends Comparable<? super T>>" . "list")
                   ("T..." . "elements"))
                  ("org.foo.Map <? extends K,   \n? extends V> m, String [] array, int i"
                   ;; TODO remove extra ws within
                   ("org.foo.Map <? extends K, ? extends V>" . "m")
                   ("String []" . "array")
                   ("int" . "i"))
                  (" Bi_Function<? super K, ? super V, ? extends V> function "
                   ("Bi_Function<? super K, ? super V, ? extends V>" . "function"))
                  ("@Annotation1 final int i,
@org.foo.Annotation_2(value_1 = \"value1 , (){}\", value2 = -2.3) String[][] arr"
                   ("int" . "i")
                   ("String[][]" . "arr"))
                  ))
    (javaimp-with-temp-buffer nil
      (insert (car data))
      (should (equal (javaimp-parse--arglist (point-min) (point-max))
                     (cdr data))))))

(ert-deftest javaimp-parse-arglist-throws ()
  (dolist (data '(("")
                  ("  ")
                  ("Exception1"
                   ("Exception1"))
                  ("\nEx1\n,\nEx2\n"
                   ("Ex1")
                   ("Ex2"))
                  (" Exception1 , org.foo_bar_3.Exception_2 "
                   ("Exception1")
                   ("org.foo_bar_3.Exception_2"))
                  ("Exception_1<? extends org.foo.Exception_2<? super Exception3>,
  Exception4<? super Exception5>>, \nException6,Exception7<Exception8>"
                   ("Exception_1<? extends org.foo.Exception_2<? super Exception3>, \
Exception4<? super Exception5>>")
                   ("Exception6")
                   ("Exception7<Exception8>"))))
    (javaimp-with-temp-buffer nil
      (insert (car data))
      (should (equal (javaimp-parse--arglist (point-min) (point-max) t)
                     (cdr data))))))

(ert-deftest javaimp-parse-decl-suffix ()
  (dolist (data '(;; ok
                  (" extends C1 {" "extends" . 2)
                  (" extends C1 implements I1, I2 {" "extends\\|implements" . 13)

                  ;; fail
                  (" extends C1 ({" "extends")
                  ("{" "extends")))
    (javaimp-with-temp-buffer nil
      (insert (car data))
      (should (equal (cddr data)
                     (javaimp-parse--decl-suffix
                      (cadr data) (1- (point)) (point-min)))))))

(ert-deftest javaimp-parse-decl-prefix ()
  (dolist (data '(;; simple
                  (" void " . 2)
                  (" public static void " . 2)

                  ;; fail - no sexps
                  ("")
                  (" ")
                  ;; fail - incomplete sexps
                  ("var)")
                  (")   ")

                  ;; don't go over / into braces
                  ("{  } void" . 6)
                  ("} void" . 3)
                  ;; don't go into parens
                  (") void" . 3)

                  ;; comments
                  ("\
  /**
   * javadoc1
   */
  public
  // line comment
  /**
   * javadoc2
   */
  void " . 29)

                  ;; annotations
                  ("\
  @Annotation1
  @Annotation2(value = \"foo\")
  @Annotation3({@Ann4(value = \"bar\"), @Ann4})
  void " . 3)

                  ;; generics
                  ("public <T extends S> T " . 1)
                  ("<T extends S> T " . 1)
                  ;; don't go into incomplete generic
                  ("S> T " . 4)

                  ;; array
                  ("String[] " . 1)

                  ;; don't go over semicolon
                  ("foo(); void " . 8)))
    (javaimp-with-temp-buffer nil
      (insert (car data))
      (should (equal (cdr data)
                     (javaimp-parse--decl-prefix (point)))))))



;; Tests for scope parsers

(defun javaimp-test-parse--scope (parser &rest test-items)
  (declare (indent 1))
  (dolist (item test-items)
    (javaimp-with-temp-buffer nil
      (insert (car item))
      (let* ((javaimp-parse--scope-hook (javaimp-parse--wrap-parser parser))
             (scopes (mapcar #'javaimp-test-parse--simplify-scope
                             (javaimp-parse-get-all-scopes nil nil nil t))))
        (should (equal (cdr item) scopes))))))

(ert-deftest javaimp-parse-scope-class ()
  (javaimp-test-parse--scope #'javaimp-parse--scope-class
    '("class Foo {"
      ((class "Foo")))
    '("class Foo extends Bar {"
      ((class "Foo")))
    '("public class Foo extends Bar implements Baz1 , Baz2 {"
      ((class "Foo")))
    `(,(subst-char-in-string
        ?  ?\n
        "public class Foo extends Bar implements Baz1 , Baz2 {")
      ((class "Foo")))
    '("class Foo<Bar, Baz> extends FooSuper<Bar, Baz> \
implements Interface1<Bar, Baz>, Interface2 {"
      ((class "Foo")))
    '("class Foo<E extends Bar> {"
      ((class "Foo")))
    ;; enum is also a keyword
    '("class Foo<Enum<?>> {"
      ((class "Foo")))
    '("class Foo<T extends Baz<? extends Baz2>> \
extends Bar<? extends Baz<? extends Baz2>> {"
      ((class "Foo")))
    '("protected abstract class Foo {"
      ((class "Foo" abstract t)))
    '("interface Foo<Bar, Baz> {"
      ((interface "Foo")))
    '("private enum Foo {"
      ((enum "Foo")))
    ))

(ert-deftest javaimp-parse-scope-anon-class ()
  (javaimp-test-parse--scope #'javaimp-parse--scope-anon-class
    '(" = new Object < Class1 , Class2 > ( 1 + 1 , baz ) {"
      ((anon-class "<anon51>Object")))
    `(,(subst-char-in-string
        ?  ?\n
        " = new Object < Class1 , Class2 > ( 1 + 1 , baz ) {")
      ((anon-class "<anon51>Object")))
    '("new Object(foo()) {"
      ((anon-class "<anon19>Object")))
    '(" = (obj.getField()) .new Object<Class1, Class2>(1, baz) {"
      ((anon-class "<anon57>Object")))
    '(" = obj.new Object<>(1, baz) {"
      ((anon-class "<anon29>Object")))
    ))

(ert-deftest javaimp-parse-scope-method-or-stmt ()
  (javaimp-test-parse--scope #'javaimp-parse--scope-method-or-stmt
    '("static void foo_bar ( String a , int b ) {"
      ((method "foo_bar(String,int)")))
    `(,(subst-char-in-string
        ?  ?\n
        "static void foo_bar ( String a , int b ) {")
      ((method "foo_bar(String,int)")))
    '("void foo_bar(String a, int b) throws E1, E2 {"
      ((method "foo_bar(String,int)")))
    '("void foo_bar()
throws E1 {"
      ((method "foo_bar()")))
    '("if (foo_bar(a, b) < 2) {"
      ((statement "if")))
    ))

(ert-deftest javaimp-parse-scope-simple-stmt ()
  (javaimp-test-parse--scope #'javaimp-parse--scope-simple-stmt
    '(" try {"
      ((simple-statement "try")))
    `(,(subst-char-in-string ?  ?\n " try {")
      ((simple-statement "try")))
    '("static {"
      ((simple-statement "static")))
    '("it -> {"
      ((simple-statement "lambda")))
    '("(x, y) -> {"
      ((simple-statement "lambda")))
    ))

(ert-deftest javaimp-parse-scope-array-init ()
  (javaimp-test-parse--scope #'javaimp-parse--scope-array-init
    '("new String[] {"
      ((array-init "")))
    '("new Object[i][] { {"
      ((array-init ""))
      ((array-init nil) (array-init "")))
    '("new int[] {{1, 2}, {"
      ((array-init ""))
      ((array-init nil) (array-init ""))
      ((array-init nil) (array-init "")))
    '("new int[] {{{1,"
      ((array-init ""))
      ((array-init nil) (array-init ""))
      ((array-init nil) (array-init nil) (array-init "")))
    `(,(subst-char-in-string ?  ?\n "new int[] {{1, 2}, {")
      ((array-init ""))
      ((array-init nil) (array-init ""))
      ((array-init nil) (array-init "")))
    '("@FooContainer( {"
      ((array-init "")))
    ))



;; Tests for parse api

(defun javaimp-test-parse--get-all-scopes-defuns ()
  (let* ((scopes (javaimp-parse-get-all-scopes
                  nil nil (javaimp-scope-defun-p t)))
         (actual (mapcar #'javaimp-test-parse--simplify-scope scopes))
         (expected
          '(((class "Top"))

            ((class "CInner1") (class "Top"))

            ((method "foo()") (class "CInner1") (class "Top"))

            ((local-class "<local192>CInner1_CLocal1")
             (method "foo()") (class "CInner1") (class "Top"))

            ((method "foo()")
             (local-class "<local192>CInner1_CLocal1")
             (method "foo()") (class "CInner1") (class "Top"))

            ((local-class "<local384>CInner1_CLocal1_CLocal1")
             (method "foo()")
             (local-class "<local192>CInner1_CLocal1")
             (method "foo()") (class "CInner1") (class "Top"))

            ((method "foo()")
             (local-class "<local384>CInner1_CLocal1_CLocal1")
             (method "foo()")
             (local-class "<local192>CInner1_CLocal1")
             (method "foo()") (class "CInner1") (class "Top"))

            ((local-class "<local692>CInner1_CLocal2")
             (method "foo()") (class "CInner1") (class "Top"))

            ((method "foo()")
             (local-class "<local692>CInner1_CLocal2")
             (method "foo()") (class "CInner1") (class "Top"))

            ((anon-class "<anon895>Object")
             (class "CInner1") (class "Top"))

            ((method "toString()")
             (anon-class "<anon895>Object") (class "CInner1") (class "Top"))

            ((class "CInner1_CInner1" abstract t) (class "CInner1") (class "Top"))

            ((method "foo()")
             (class "CInner1_CInner1" abstract t) (class "CInner1") (class "Top"))

            ((method "bar()")
             (class "CInner1_CInner1" abstract t) (class "CInner1") (class "Top"))

            ((interface "IInner1") (class "Top"))

            ((method "foo()") (interface "IInner1") (class "Top"))

            ((class "IInner1_CInner1") (interface "IInner1") (class "Top"))

            ((method "foo()")
             (class "IInner1_CInner1") (interface "IInner1") (class "Top"))

            ((method "defaultMethod(String)")
             (interface "IInner1") (class "Top"))

            ((interface "IInner1_IInner1") (interface "IInner1") (class "Top"))

            ((method "defaultMethod(String)")
             (interface "IInner1_IInner1") (interface "IInner1") (class "Top"))

            ((enum "EnumInner1") (class "Top"))

            ((method "EnumInner1()") (enum "EnumInner1") (class "Top"))

            ((method "foo()") (enum "EnumInner1") (class "Top"))

            ((enum "EnumInner1_EInner1") (enum "EnumInner1") (class "Top"))

            ((class "ColocatedTop"))

            ((method "foo()") (class "ColocatedTop"))

            ((method "bar(String,String)") (class "ColocatedTop")))))
    (should (= (length expected) (length actual)))
    (dotimes (i (length expected))
      (should (equal (nth i expected) (nth i actual))))
    ;; Selectively check positions
    (let ((data
           `((,(nth 0 scopes) "Top" 26 36)
             (,(nth 17 scopes) "foo()" 1798 1804)
             (,(nth 24 scopes) "EnumInner1_EInner1" 2462 2486)
             (,(nth 26 scopes) "foo()" 2554 2560))))
      (dolist (elt data)
        (let ((scope (nth 0 elt)))
          (should (equal (nth 1 elt) (javaimp-scope-name scope)))
          (should (equal (nth 2 elt) (javaimp-scope-start scope)))
          (should (equal (nth 3 elt) (javaimp-scope-open-brace scope))))))))


(defun javaimp-test-parse--simplify-scope (s)
  (let (res)
    (while s
      (push (append
             (list (javaimp-scope-type s)
                   (javaimp-scope-name s))
             (javaimp-scope-attrs s))
            res)
      (setq s (javaimp-scope-parent s)))
    (nreverse res)))

(ert-deftest javaimp-parse-get-package ()
  (javaimp-with-temp-buffer nil
    (insert "
  package  foo.bar.baz  ;
//package commented.line;
/*
package commented.block;
*/
")
    (should (equal (javaimp-parse-get-package) "foo.bar.baz"))))

(ert-deftest javaimp-parse-get-imports ()
  (javaimp-with-temp-buffer nil
    (insert "
  import  some.class1  ;
import  static  some.class.fun1;
//import commented.line;
/*
import static commented.block;
*/
import someclass2;
import my.package.* ;

import static some_class.fun_2; // comment
// comment outside
")
    (should (equal
             (javaimp-parse-get-imports)
             '((2 . 206)
               ("some.class1" . normal)
               ("some.class.fun1" . static)
               ("someclass2" . normal)
               ("my.package.*" . normal)
               ("some_class.fun_2" . static))))))

(ert-deftest javaimp-parse-get-all-scopes ()
  (javaimp-with-temp-buffer "test1.java"
    (should-not javaimp-parse--dirty-pos)
    ;;
    ;; parse full buffer
    (javaimp-test-parse--get-all-scopes-defuns)
    (should javaimp-parse--dirty-pos)
    (should-not (marker-position javaimp-parse--dirty-pos))
    ;;
    ;; reparse half of the buffer
    (set-marker javaimp-parse--dirty-pos (/ (- (point-max) (point-min)) 2))
    (javaimp-test-parse--get-all-scopes-defuns)
    (should-not (marker-position javaimp-parse--dirty-pos))
    ;;
    ;; don't reparse
    (javaimp-test-parse--get-all-scopes-defuns)))


(ert-deftest javaimp-parse-get-enclosing-scope ()
  (let ((testcases
         '(;; bob
           ((goto-char (point-min)))
           ;; before first scope
           ((re-search-forward "class Top\\>"))
           ;; before first method
           ((re-search-forward "class CInner1\\>")
            (class "Top"))
           ;; inside method
           ((re-search-forward "class CInner1_CLocal2\\>")
            (method "foo()") (class "CInner1") (class "Top"))
           ;; between methods
           ((re-search-forward "class CInner1_CInner1\\>")
            (class "CInner1") (class "Top"))
           ;; after last method
           ((progn
              (re-search-forward "class ColocatedTop\\>")
              (search-backward "}"))
            (class "Top"))
           ;; between top-level scopes
           ((re-search-forward "class ColocatedTop\\>"))
           ;; after last scope
           ((progn
              (goto-char (point-max))
              (search-backward "}")
              (forward-char)))
           ;; eob
           ((goto-char (point-max))))))
    (dolist (testcase testcases)
      (javaimp-with-temp-buffer "test1.java"
        (eval (car testcase))           ;move
        (should
         (equal (cdr testcase)
                (javaimp-test-parse--simplify-scope
                 (javaimp-parse-get-enclosing-scope
                  (javaimp-scope-defun-p 'method)))))))))
