;;; tests/imenu.el --- javaimp Imenu tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert)
(require 'javaimp)
(require 'javaimp-tests)

(defun javaimp-test-imenu--simplify-entries (alist)
  (dolist (elt alist)
    (if (and (= (length elt) 4)
             (functionp (nth 2 elt)))
        (setcdr elt nil)
      (javaimp-test-imenu--simplify-entries (cdr elt)))))


(ert-deftest javaimp-imenu-create-index ()
  (let ((actual (javaimp-with-temp-buffer "test1.java"
                    (javaimp-imenu-create-index)))
        (expected
         '(("Top"
            ("Top#")
            ("CInner1"
             ("CInner1#")
             ("foo()"
              ("foo()#")
              ("<local192>CInner1_CLocal1"
               ("<local192>CInner1_CLocal1#")
               ("foo()"
                ("foo()#")
                ("<local384>CInner1_CLocal1_CLocal1"
                 ("<local384>CInner1_CLocal1_CLocal1#")
                 ("foo()"))))
              ("<local692>CInner1_CLocal2"
               ("<local692>CInner1_CLocal2#")
               ("foo()")))
             ("<anon895>Object"
              ("<anon895>Object#")
              ("toString()"))
             ("CInner1_CInner1"
              ("CInner1_CInner1#")
              ("foo()")
              ("abstract_method()")
              ("bar()")
              ("baz()")))
            ("IInner1"
             ("IInner1#")
             ("foo()")
             ("abstract_method()")
             ("IInner1_CInner1"
              ("IInner1_CInner1#")
              ("foo()"))
             ("baz()")
             ("defaultMethod(String)")
             ("IInner1_IInner1"
              ("IInner1_IInner1#")
              ("foo()")
              ("defaultMethod(String)")
              ("baz()")))
            ("EnumInner1"
             ("EnumInner1#")
             ("EnumInner1()")
             ("foo()")
             ("EnumInner1_EInner1")
             ))
           ("ColocatedTop"
            ("ColocatedTop#")
            ("foo()")
            ("bar(String,String)")))))
    (javaimp-test-imenu--simplify-entries actual)
    (should (equal expected actual))))
