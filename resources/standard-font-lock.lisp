#+sbcl (funcall #'c\L:li|ST|
;<- lisp-ts-mode-positive-read-conditional
;               ^^ lisp-ts-mode-sharpquote
;                 ^ font-lock-keyword-face
;                  ^^ (font-lock-keyword-face font-lock-escape-face)
;                    ^ font-lock-delimiter-face
;                       ^  ^ font-lock-constant-face
                #B1111100)
;               ^^ font-lock-number-face
;                 ^^^^^ (lisp-ts-mode-1-bit font-lock-number-face)
;                      ^^ (lisp-ts-mode-0-bit font-lock-number-face)
#-sbcl #.:hello
;<- lisp-ts-mode-negative-read-conditional
;      ^^ lisp-ts-mode-read-eval
;        ^ font-lock-delimiter-face
;         ^^^^^ font-lock-builtin-face

#| this #| is #| nested! |# |# |#
;<- (font-lock-comment-delimiter-face font-lock-comment-face)
; ^^^^^^ font-lock-comment-face
;       ^^ (font-lock-comment-delimiter-face lisp-ts-mode-block-comment-depth-1 font-lock-comment-face)
;         ^^^^ (lisp-ts-mode-block-comment-depth-1 font-lock-comment-face)
;             ^^ (font-lock-comment-delimiter-face lisp-ts-mode-block-comment-depth-2 lisp-ts-mode-block-comment-depth-1 font-lock-comment-face)
;               ^^^^^^^^^ (lisp-ts-mode-block-comment-depth-2 lisp-ts-mode-block-comment-depth-1 font-lock-comment-face)
;                        ^^ (font-lock-comment-delimiter-face lisp-ts-mode-block-comment-depth-2 lisp-ts-mode-block-comment-depth-1 font-lock-comment-face)
;                          ^ (lisp-ts-mode-block-comment-depth-1 font-lock-comment-face)
;                           ^^ (font-lock-comment-delimiter-face lisp-ts-mode-block-comment-depth-1 font-lock-comment-face)
;                             ^ font-lock-comment-face
;                              ^^ (font-lock-comment-delimiter-face font-lock-comment-face)

(formatter "~@<~11,'2,V,,#@:D~:>")
;          ^ font-lock-string-face
;           ^ (lisp-ts-mode-format-tilde font-lock-string-face)
;            ^ (lisp-ts-mode-format-at font-lock-string-face)
;             ^ (lisp-ts-mode-format-paired-directive font-lock-string-face)
;              ^ (lisp-ts-mode-format-tilde font-lock-string-face)
;               ^^ (lisp-ts-mode-format-numeric-parameter font-lock-string-face)
;                 ^ (lisp-ts-mode-format-comma font-lock-string-face)
;                  ^ font-lock-string-face
;                   ^ (lisp-ts-mode-format-char-parameter font-lock-string-face)
;                    ^ (lisp-ts-mode-format-comma font-lock-string-face)
;                     ^ (lisp-ts-mode-format-arg-parameter font-lock-string-face)
;                      ^^ (lisp-ts-mode-format-comma font-lock-string-face)
;                        ^ (lisp-ts-mode-format-remaining-parameter font-lock-string-face)
;                         ^ (lisp-ts-mode-format-at font-lock-string-face)
;                          ^ (lisp-ts-mode-format-colon font-lock-string-face)
;                           ^ (lisp-ts-mode-format-standalone-directive font-lock-string-face)
;                            ^ (lisp-ts-mode-format-tilde font-lock-string-face)
;                             ^ (lisp-ts-mode-format-colon font-lock-string-face)
;                              ^ (lisp-ts-mode-format-paired-directive font-lock-string-face)
;                               ^ font-lock-string-face

#3r11
;<-^^ font-lock-number-face
#\pile_of_poo
;<- lisp-ts-mode-character-escape
; ^^^^^^^^^^^ lisp-ts-mode-character-name

```',,@(,.(list 'values))
;<- lisp-ts-mode-quasiquote
;  ^ lisp-ts-mode-quote
;   ^ lisp-ts-mode-comma
;    ^^ lisp-ts-mode-comma-at
;       ^^ lisp-ts-mode-comma-dot
;               ^ lisp-ts-mode-quote
