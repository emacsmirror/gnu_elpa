;;; javaimp-parse.el --- javaimp parsing  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

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

;; When changing parsing, it may make sense to benchmark performance
;; on a large source tree with something like this:
;;
;; (benchmark-run 1
;;   (javaimp-flush-cache)
;;   (javaimp--read-dir-source-idents
;;    (javaimp-scope-defun-p t)
;;    "<path-to-jdk-source>/src/java.base/share/classes/"
;;    "bench"))


(require 'cc-mode)                      ;for java-mode-syntax-table
(require 'cl-lib)
(require 'seq)


(cl-defstruct javaimp-scope
  (type
   nil
   :documentation "Scope type, a symbol.  See `javaimp-scope-all-types'.")
  name
  start
  open-brace
  (parent
   nil
   :documentation "Reference to parent scope struct.")
  (attrs
   nil
   :documentation "Attributes plist"))


(defconst javaimp-scope-all-types
  '(anon-class
    array-init
    class
    enum
    interface
    local-class
    method
    simple-statement
    statement)
  "All known scope types.")

(defconst javaimp-parse--class-keywords-regexp
  (regexp-opt (mapcar #'symbol-name
                      '(class interface enum)) 'symbols))


(defconst javaimp-parse--stmt-keywords
  '("if" "else" "for" "while" "do" "switch"
    "try" "catch" "finally"
    ;; synchronized block (not method modifier)
    "synchronized"
    ;; static initializer block (not field modifier)
    "static"
    ))
(defconst javaimp-parse--stmt-keywords-regexp
  (regexp-opt javaimp-parse--stmt-keywords 'words))
(defconst javaimp-parse--stmt-keyword-maxlen
  (seq-max (mapcar #'length javaimp-parse--stmt-keywords)))

(defsubst javaimp-parse--scope-type-defun-p (s)
  (and (javaimp-scope-type s)
       (not (memq (javaimp-scope-type s)
                  '(array-init simple-statement statement)))))


(defun javaimp-parse--directive-regexp (directive)
  "Return regexp suitable for matching package-like DIRECTIVE, a
regexp.  First group is directive, second group is identifier."
  (rx bol (* space)
      (group (regexp directive)) (+ space)
      (group (+ (any alnum ?_)) (* ?. (+ (any alnum ?_ ?*))))
      (* space) ?\;))

(defconst javaimp-parse--package-regexp
  (javaimp-parse--directive-regexp "package"))
(defconst javaimp-parse--import-regexp
  (javaimp-parse--directive-regexp "import\\(?:[[:space:]]+static\\)?"))


(defvar javaimp--parse-syntax-table
  ;; TODO don't depend on cc-mode
  (let ((st (make-syntax-table java-mode-syntax-table)))
    ;; To be able to skip over generic types as over lists
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    ;; Dot separates parts of fully-qualified type
    (modify-syntax-entry ?. "_" st) ;
    ;; Override prefix syntax so that scan-sexps backwards right after
    ;; @ in annotation doesn't ignore it.
    (modify-syntax-entry ?@ "_" st)
    st)
  "Syntax table used in parsing.")

(defvar-local javaimp-parse--dirty-pos nil
  "Marker which points to a buffer position after which all parsed
information should be considered as stale.  Usually set by
modification change hooks.  Nil value means we haven't yet parsed
anything in the buffer.  A marker pointing nowhere means
everything's up-to-date.")



;; Low-level subroutines

(defsubst javaimp-parse--substr-before-< (str)
  (let ((end (string-search "<" str)))
    (if end
        (string-trim (substring str 0 end))
      str)))

(defun javaimp-parse--rsb-keyword (regexp &optional bound noerror count)
  "Like `re-search-backward', but count only occurrences which start
outside any syntactic context as given by `syntax-ppss-context'.
Assumes point is outside of any context initially."
  (or count (setq count 1))
  (let ((step (if (>= count 0) 1 -1))
        (case-fold-search nil)
        res)
    (dotimes (_ (abs count))
      (while (and (setq res (re-search-backward regexp bound noerror step))
                  (syntax-ppss-context (syntax-ppss)))))
    res))

(defun javaimp-parse--arglist (beg end &optional only-type)
  "Parse arg list between BEG and END, of the form \"TYPE NAME,
...\".  Return list of conses (TYPE . NAME).  If ONLY-TYPE is non-nil,
then name parsing is skipped.  This function does not move point."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (with-syntax-table javaimp--parse-syntax-table
        (ignore-errors
          (let (res)
            (goto-char (point-max))
            (while (progn
                     (javaimp-parse--skip-back-until)
                     (not (bobp)))
              (push (javaimp-parse--arglist-one-arg only-type) res)
              ;; Move back to the previous argument, if any
              (when (javaimp-parse--skip-back-until
                     (lambda (_last-what _last-pos)
                       (and (not (bobp))
                            (= (char-before) ?,))))
                (backward-char)))       ; skip comma
            res))))))

(defun javaimp-parse--arglist-one-arg (only-type)
  "Parse one argument as type and name backwards starting from
point and return it in the form (TYPE . NAME).  Name is skipped
if ONLY-TYPE is non-nil.  Leave point at where the job is done:
skipping further backwards is done by the caller."
  (let ((limit (progn
                 (javaimp-parse--skip-back-until)
                 (point)))
        name)
    ;; Parse name
    (unless only-type
      (if (= 0 (skip-syntax-backward "w_"))
          (error "Cannot parse argument name")
        (setq name (buffer-substring-no-properties (point) limit))
        (javaimp-parse--skip-back-until)
        (setq limit (point))))
    ;; Parse type: allow anything, but stop at the word boundary which
    ;; is not inside list (this is presumably the type start..)
    (if-let* ((last-skip
               (javaimp-parse--skip-back-until
                (lambda (_last-what last-pos)
                  (save-excursion
                    (if last-pos (goto-char last-pos))
                    (looking-at "\\_<"))))))
        (progn
          (unless (eq last-skip t)
            (goto-char (cdr last-skip))) ;undo skipping by ..-until
          (let ((type (replace-regexp-in-string
                       "[[:space:]\n]+" " "
                       (buffer-substring-no-properties (point) limit))))
            (cons type name)))
      (error "Cannot parse argument type"))))

(defun javaimp-parse--skip-back-until (&optional stop-p)
  "Goes backwards until position at which STOP-P returns non-nil,
or reaching bob.

STOP-P is invoked with two arguments which describe the last
non-ws thing skipped: LAST-WHAT (symbol - either `list' or
`char') and LAST-POS.  If STOP-P returns non-nil, then the return
value is also non-nil: either (LAST-WHAT . LAST-POS) if both are
non-nil or t.  Otherwise the return value is nil.

If STOP-P wants to look forward, it should be prepared to see
whitespace / comments, this is because backward movement skips
them before invoking STOP-P.  It should not move point.  If
omitted, it defaults to `always', in this case the effect of the
function is to just skip whitespace / comments."
  (or stop-p (setq stop-p #'always))
  (catch 'done
    (let (last-what last-pos)
      (while t
        (skip-syntax-backward " ")
        (let ((state (syntax-ppss)))
          (cond ((syntax-ppss-context state)
                 ;; move out of comment/string if in one
                 (goto-char (nth 8 state)))
                ((and (not (bobp))
                      (memql (syntax-class (syntax-after (1- (point))))
                             ;; comment end, generic comment
                             '(12 14)))
                 (backward-char))
                ((funcall stop-p last-what last-pos)
                 (throw 'done (if (and last-what last-pos)
                                  (cons last-what last-pos)
                                t)))
                ((bobp)
                 (throw 'done nil))
                ((= (syntax-class (syntax-after (1- (point)))) 5) ;close-paren
                 (backward-list)
                 (setq last-what 'list
                       last-pos (point)))
                (t
                 (backward-char)
                 (setq last-what 'char
                       last-pos (point)))))))))

(defun javaimp-parse--preceding (regexp open-brace &optional bound skip-count)
  "Returns non-nil if a match for REGEXP is found before point,
but not before BOUND.  Matches inside comments / strings are
skipped.  A match is checked to be SKIP-COUNT lists away from the
OPEN-BRACE.  If a suitable match is found, then the match-data is
set, as for `re-search-backward'."
  (and (javaimp-parse--rsb-keyword regexp bound t)
       (ignore-errors
         ;; Check if our match belongs to the right block
         (let ((pos (match-end 0)))
           ;; Skip over complete lists
           (when skip-count
             (setq pos (scan-lists pos skip-count 0)))
           ;; Now go past open-brace
           (and pos
                (= (scan-lists pos 1 -1)
                   (1+ open-brace)))))))

(defun javaimp-parse--decl-suffix (regexp brace-pos bound)
  "Attempt to parse defun declaration suffix (like \"extends\"
clause of a class definition) matched by REGEXP.  Matches inside
comments/strings are skipped.

Find match of REGEXP backwards from BRACE-POS (but not farther
than BOUND) and skip complete lists forwards from there, until we
enter the list started by brace at BRACE-POS.  If successful,
repeat those steps starting from the last match.  The result is
the start of the last successful match, or nil."
  (goto-char brace-pos)
  (catch 'found
    (while (javaimp-parse--rsb-keyword regexp bound t)
      (let ((curr-beg (match-beginning 0)))
        (save-excursion
          (save-restriction
            (narrow-to-region (match-end 0) (1+ brace-pos))
            (with-syntax-table javaimp--parse-syntax-table
              ;; Skip over any number of "lists", which may be
              ;; exceptions in "throws" clause, or something alike
              (let ((scan-pos (point-min)))
                (while (and scan-pos (< scan-pos (point-max)))
                  (if (ignore-errors
                        ;; We're done when we enter the brace block
                        ;; started by brace-pos
                        (= (scan-lists scan-pos 1 -1) (point-max)))
                      (throw 'found curr-beg)
                    (setq scan-pos (ignore-errors
                                     (scan-lists scan-pos 1 0)))))))))))))

(defun javaimp-parse--decl-prefix (start-pos &optional bound)
  "Attempt to parse defun declaration prefix backwards from
START-POS, but not farther than BOUND or previous semicolon,
whichever comes first.  START-POS should be somewhere inside the
defun declaration, but outside any nested lists like argument
list.  Return the beginning of the declaration or nil.

Go back by sexps until we either reach BOUND or a semicolon, or
arrive at something which doesn't look like a part of defun
declaration."
  (goto-char start-pos)
  (javaimp-parse--skip-back-until)
  (let* ((fence (save-excursion
                  ;; Semicolon is punctuation and would be skipped by
                  ;; scan-sexp below, so it's necessary to have it
                  ;; here.  Do not add braces here: they might appear
                  ;; in annotations in declaration, in which case we
                  ;; should skip them.
                  (javaimp-parse--rsb-keyword ";" bound t)))
         (bound (apply #'max
                       (delq nil
                             (list bound
                                   (and fence (1+ fence))
                                   (point-min))))))
    (save-restriction
      (narrow-to-region bound (point))
      (with-syntax-table javaimp--parse-syntax-table
        (let (pos res)
          (goto-char (point-max))
          ;; Go back by sexps
          (while (and (ignore-errors
                        (setq pos (scan-sexps (point) -1)))
                      (or (memql (char-after pos)
                                 '(?@ ?\(   ;annotation type / args
                                      ?<    ;generic type
                                      ?\[)) ;array
                          ;; first char of keyword / identifier:
                          ;; word or symbol
                          (memql (syntax-class (syntax-after pos))
                                 '(2 3))))
            (goto-char (setq res pos)))
          res)))))


;;; Scopes

(defun javaimp-scope-copy (scope &optional parent-pred scope-alist)
  "Return a recursive copy of SCOPE and its parents.  If a parent
matches PARENT-PRED (or PARENT-PRED is nil) then it's copied,
otherwise it's excluded from the chain.

SCOPE-ALIST may contain scopes to use instead of copying.  Each
entry is of the form (OPEN-BRACE . SCOPE).  When an entry is
found, use it and stop going up the parent chain.  Note that all
calls with the same SCOPE-ALIST should also use same
PARENT-PRED."
  (let* ((from-alist (alist-get (javaimp-scope-open-brace scope) scope-alist))
         (tmp (or from-alist (copy-javaimp-scope scope)))
         (res tmp)
         parent)
    (while (and (not from-alist)
                (setq parent (javaimp-scope-parent tmp)))
      (if (or (not parent-pred)
              (funcall parent-pred parent))
          ;; Include parent
          (setq tmp
                (setf (javaimp-scope-parent tmp)
                      (or (setq from-alist
                                (alist-get (javaimp-scope-open-brace parent)
                                           scope-alist))
                          (copy-javaimp-scope parent))))
        ;; Skip parent
        (setf (javaimp-scope-parent tmp) (javaimp-scope-parent parent))))
    res))

(defun javaimp-scope-concat-parents (scope)
  (let (parents)
    (while (setq scope (javaimp-scope-parent scope))
      (push scope parents))
    (mapconcat #'javaimp-scope-name parents ".")))

(defsubst javaimp-scope-test-type (scope leaf-types &optional parent-types)
  (declare (indent 1))
  (let ((res (memq (javaimp-scope-type scope) leaf-types)))
    (when parent-types
      (while (and res
                  (setq scope (javaimp-scope-parent scope)))
        (setq res (memq (javaimp-scope-type scope) parent-types))))
    res))

(defun javaimp-scope-defun-p (&optional include-also)
  "Return predicate which matches defun scopes, which are usually
type definitions.  If INCLUDE-ALSO is `method' then also include
methods.  If INCLUDE-ALSO is t, include methods, as well as local
/ anonymous classes and their methods."
  (let ((leaf-types
         (append '(class interface enum)
                 (when include-also
                   '(method))
                 (when (eq include-also t)
                   '(anon-class local-class))))
        (parent-types
         (if (eq include-also t)
             ;; Anonymous / local classes may be inside non-defun
             ;; scopes
             javaimp-scope-all-types
           '(class interface enum))))
    (lambda (s)
      (javaimp-scope-test-type s leaf-types parent-types))))

(defun javaimp-scope-same-parent-p (parent)
  (if parent
      (lambda (s)
        (and (javaimp-scope-parent s)
             (= (javaimp-scope-open-brace (javaimp-scope-parent s))
                (javaimp-scope-open-brace parent))))
    (lambda (s)
      (not (javaimp-scope-parent s)))))


;; Scope parsing

(defsubst javaimp-parse--wrap-parser (parser)
  (lambda (arg)
    (save-excursion
      (funcall parser arg))))

(defvar javaimp-parse--scope-hook
  (mapcar #'javaimp-parse--wrap-parser
          '(;; Should be before method/stmt because it looks similar,
            ;; but with "new" in front
            javaimp-parse--scope-anon-class
            javaimp-parse--scope-class
            javaimp-parse--scope-simple-stmt
            javaimp-parse--scope-method-or-stmt
            ;; Should be after simple/stmt, and preferably last
            javaimp-parse--scope-array-init
            ))
  "List of parser functions, each of which is called in
`save-excursion' and with one argument, the position of opening
brace.")

(defun javaimp-parse--scope-class (brace-pos)
  "Attempt to parse class-like scope."
  (when (javaimp-parse--preceding
         javaimp-parse--class-keywords-regexp
         brace-pos
         ;; Nearest preceding closing paren is a good bound because
         ;; there _will be_ such char in frequent case of
         ;; method/statement/anon-class, and we stop checks here
         ;; sooner than later.
         (save-excursion
           (when (javaimp-parse--rsb-keyword ")" nil t 1)
             (1+ (point)))))
    (let* ((keyword-start (match-beginning 1))
           (keyword-end (match-end 1))
           (arglist-end
            ;; "extends" comes before "implements" etc., thus try in
            ;; this order
            (or (javaimp-parse--decl-suffix
                 "\\_<extends\\_>" brace-pos keyword-end)
                (javaimp-parse--decl-suffix
                 "\\_<implements\\_>" brace-pos keyword-end)
                (javaimp-parse--decl-suffix
                 "\\_<permits\\_>" brace-pos keyword-end)))
           (arglist (javaimp-parse--arglist
                     keyword-end (or arglist-end brace-pos) t)))
      (when (= (length arglist) 1)
        (let* ((type (intern (buffer-substring-no-properties
                              keyword-start keyword-end)))
               (attrs
                (when-let* ((_ (eq type 'class))
                            (decl-start
                             (javaimp-parse--decl-prefix keyword-start)))
                  (goto-char brace-pos)
                  (when (javaimp-parse--rsb-keyword
                         "\\_<abstract\\_>" decl-start t)
                    '(abstract t)))))
          (make-javaimp-scope :type type
                              :name (javaimp-parse--substr-before-< (caar arglist))
                              :start keyword-start
                              :attrs attrs))))))

(defun javaimp-parse--scope-simple-stmt (_brace-pos)
  "Attempt to parse `simple-statement' scope.  Currently block
lambdas are also recognized as such."
  (javaimp-parse--skip-back-until)
  (cond ((looking-back "->" (- (point) 2))
         (make-javaimp-scope
          :type 'simple-statement
          :name "lambda"
          :start (- (point) 2)))
        ((looking-back javaimp-parse--stmt-keywords-regexp
                       (- (point) javaimp-parse--stmt-keyword-maxlen) nil)
         (make-javaimp-scope
          :type 'simple-statement
          :name (match-string 1)
          :start (match-beginning 1)))))

(defun javaimp-parse--scope-anon-class (brace-pos)
  "Attempt to parse `anon-class' scope."
  ;; skip arg-list and ws
  (when (and (progn
               (javaimp-parse--skip-back-until)
               (= (char-before) ?\)))
             (ignore-errors
               (goto-char
                (scan-lists (point) -1 0))))
    (let ((end (point))
          start arglist)
      (when (javaimp-parse--preceding "\\_<new\\_>" brace-pos nil 1)
        (setq start (match-beginning 0)
              arglist (javaimp-parse--arglist (match-end 0) end t))
        (when (= (length arglist) 1)
          (make-javaimp-scope :type 'anon-class
                              :name (format "<anon%d>%s"
                                            brace-pos
                                            (javaimp-parse--substr-before-<
                                             (caar arglist)))
                              :start start))))))

(defun javaimp-parse--scope-method-or-stmt (brace-pos)
  "Attempt to parse `method' or `statement' scope."
  (let ((arglist-end (when (javaimp-parse--rsb-keyword ")" nil t 1)
                       (1+ (point)))))
    (when arglist-end
      (let* ((throws-start (javaimp-parse--decl-suffix
                            "\\_<throws\\_>" brace-pos arglist-end))
             (throws-args (when throws-start
                            (or (javaimp-parse--arglist
                                 (+ throws-start (length "throws")) brace-pos t)
                                t))))   ;invalid (nil is ok)
        (when (and (not (eq throws-args t))
                   (progn
                     (goto-char (or throws-start brace-pos))
                     (javaimp-parse--skip-back-until)
                     (= (char-before) ?\)))
                   (ignore-errors
                     ;; For method this is arglist
                     (goto-char
                      (scan-lists (point) -1 0))))
          (let* (;; leave open/close parens out
                 (arglist-region (cons (1+ (point))
                                       (1- (scan-lists (point) 1 0))))
                 (count (progn
                          (javaimp-parse--skip-back-until)
                          (skip-syntax-backward "w_")))
                 (name (and (< count 0)
                            (buffer-substring-no-properties
                             (point) (+ (point) (abs count)))))
                 (type (when name
                         (if (and (member name javaimp-parse--stmt-keywords)
                                  (not throws-args))
                             'statement 'method))))
            (when type
              (make-javaimp-scope
               :type type
               :name (if (eq type 'method)
                         (let ((args (javaimp-parse--arglist
                                      (car arglist-region)
                                      (cdr arglist-region))))
                           (concat name "(" (mapconcat #'car args ",") ")"))
                       name)
               :start (point)))))))))

(defun javaimp-parse--scope-array-init (_brace-pos)
  "Attempt to parse `array-init' scope."
  (javaimp-parse--skip-back-until)
  (let (decl-prefix-beg)
    (when (or
           ;; Special case for array-valued single-element annotation
           (= (preceding-char) ?\()
           ;; This will be non-nil for "top-level" array initializer.
           ;; Nested array initializers are handled by a special rule
           ;; in `javaimp-parse--scopes'.
           (setq decl-prefix-beg (javaimp-parse--decl-prefix (point))))
      (make-javaimp-scope :type 'array-init
                          :name ""
                          :start decl-prefix-beg))))


(defun javaimp-parse--scopes (count)
  "Attempt to parse COUNT enclosing scopes at point.
Returns most nested one, with its parents sets accordingly.  If
COUNT is nil then goes all the way up.

Examines and sets property `javaimp-parse-scope' at each scope's
open brace.  If neither of functions in
`javaimp-parse--scope-hook' return non-nil then a dummy scope is
created, with `javaimp-scope-type' set to nil.

A couple of additional special rules, which may apply only when
we have full chain of scope nesting:
- If a `class' is nested in `method' (possibly within
statements), then it's changed to `local-class'.
- If an unrecognized scope (with nil type) is nested directly in
`array-init' then it's changed to `array-init'.  This applies
recursively.

If point is inside of any comment/string then this function does
nothing."
  (let ((state (syntax-ppss))
        res)
    (unless (syntax-ppss-context state)
      (while (and (nth 1 state)
                  (or (not count)
                      (>= (setq count (1- count)) 0)))
        ;; Find innermost enclosing open-bracket
        (goto-char (nth 1 state))
        (when (= (char-after) ?{)
          (let ((scope (get-text-property (point) 'javaimp-parse-scope)))
            (unless scope
              (setq scope (or (run-hook-with-args-until-success
                               'javaimp-parse--scope-hook (point))
                              (make-javaimp-scope)))
              (setf (javaimp-scope-open-brace scope) (point))
              (put-text-property (point) (1+ (point))
                                 'javaimp-parse-scope scope))
            (push scope res)
            (when (javaimp-scope-start scope)
              (goto-char (javaimp-scope-start scope)))))
        (setq state (syntax-ppss))))
    (let (curr parent in-method)
      (while res
        (setq curr (car res))
        ;; Additional special rules.  Note that we modify the object
        ;; which is the value of text property, so this will work only
        ;; once.
        (cond ((and (eq 'class (javaimp-scope-type curr))
                    in-method)
               ;; Change to local-class
               (setf (javaimp-scope-type curr) 'local-class
                     (javaimp-scope-name curr)
                     (format "<local%d>%s"
                             (javaimp-scope-open-brace curr)
                             (javaimp-scope-name curr))))
              ((and parent
                    (eq 'array-init (javaimp-scope-type parent))
                    (not (javaimp-scope-type curr)))
               (setf (javaimp-scope-type curr) 'array-init)))
        (if (eq 'method (javaimp-scope-type curr))
            (setq in-method t)
          (if (memq (javaimp-scope-type curr)
                    ;; all non-method defuns
                    '(anon-class class enum interface local-class))
              (setq in-method nil)))
        ;;
        (setf (javaimp-scope-parent curr) parent)
        (setq parent curr
              res (cdr res)))
      parent)))

(defun javaimp-parse--all-scopes ()
  "Parse all scopes in this buffer which are after
`javaimp-parse--dirty-pos', if it points anywhere.  Makes it
point nowhere when done.  All entry-point functions will usually
call this function first."
  (unless javaimp-parse--dirty-pos
    (setq javaimp-parse--dirty-pos (point-min-marker)))
  (when (marker-position javaimp-parse--dirty-pos)
    (with-silent-modifications          ;we update only private props
      (remove-text-properties javaimp-parse--dirty-pos (point-max)
                              '(javaimp-parse-scope nil))
      (goto-char (point-max))
      (let (;; Ignore syntax-table properties set by cc-mode.  Can be
            ;; removed when we no longer rely on cc-mode.
            (parse-sexp-lookup-properties nil))
        (while (javaimp-parse--rsb-keyword "{" javaimp-parse--dirty-pos t)
          (save-excursion
            (forward-char)
            ;; Set props at this brace and all the way up
            (javaimp-parse--scopes nil)))))
    (set-marker javaimp-parse--dirty-pos nil)))

(defun javaimp-parse--update-dirty-pos (beg _end _old-len)
  "Function to add to `after-change-functions' hook."
  (when (and javaimp-parse--dirty-pos
             (or (not (marker-position javaimp-parse--dirty-pos))
                 (< beg javaimp-parse--dirty-pos)))
    (set-marker javaimp-parse--dirty-pos beg)))


(defun javaimp-parse--enclosing-scope (&optional pred)
  "Return innermost enclosing scope matching PRED."
  (javaimp-parse--skip-back-until)
  (catch 'found
    (let ((state (syntax-ppss)))
      (while t
        (when-let* ((res (save-excursion
                           (javaimp-parse--scopes nil)))
                    ((javaimp-scope-type res))
                    ((or (null pred)
                         (funcall pred res))))
          (throw 'found res))
        ;; Go up
        (if (nth 1 state)
            (progn
              (goto-char (nth 1 state))
              (setq state (syntax-ppss)))
          (throw 'found nil))))))

(defun javaimp-parse--abstract-methods (parent-scope)
  (let ((start (1+ (javaimp-scope-open-brace parent-scope)))
        (end (ignore-errors
               (1- (scan-lists (javaimp-scope-open-brace parent-scope) 1 0))))
        res)
    (goto-char (or end (point-max)))
    (while (and (> (point) start)
                (javaimp-parse--rsb-keyword ";" start t))
      ;; Are we in the same nest?
      (if (= (nth 1 (syntax-ppss)) (javaimp-scope-open-brace parent-scope))
          (save-excursion
            ;; Parse as normal method scope, but don't set open-brace
            ;; because there's no body
            (when-let* ((scope (javaimp-parse--scope-method-or-stmt (point))))
              (setf (javaimp-scope-parent scope) parent-scope)
              (push scope res)))
        ;; We've entered another nest, skip to its start
        (goto-char (nth 1 (syntax-ppss)))))
    res))


;; Functions intended to be called from other parts of javaimp.  They
;; do not preserve excursion / restriction - it's the caller's
;; responsibility.

(defun javaimp-parse-get-package ()
  "Return the package declared in the current file.  Leaves point
at the end of directive."
  (javaimp-parse--all-scopes)
  (goto-char (point-max))
  (when (javaimp-parse--rsb-keyword javaimp-parse--package-regexp nil t 1)
    (goto-char (match-end 0))
    (match-string 2)))

(defun javaimp-parse-get-imports ()
  "Parse import directives in the current buffer and return (REGION
. CLASS-ALIST).  REGION, a cons of two positions, spans from bol
of first import to eol of last import.  CLASS-ALIST contains
elements (CLASS . TYPE), where CLASS is a string and TYPE is
either of symbols `normal' or `static'."
  (javaimp-parse--all-scopes)
  (goto-char (point-max))
  (let (start-pos end-pos class-alist)
    (while (javaimp-parse--rsb-keyword javaimp-parse--import-regexp nil t)
      (setq start-pos (line-beginning-position))
      (unless end-pos
        (setq end-pos (line-end-position)))
      (push (cons (match-string 2)
                  (if (string-search "static" (match-string 1))
                      'static 'normal))
            class-alist))
    (cons (and start-pos end-pos (cons start-pos end-pos))
          class-alist)))

(defun javaimp-parse-get-all-scopes (&optional beg end pred no-filter)
  "Return copies of all scopes in the current buffer between
positions BEG and END, both exclusive, optionally filtering them
with PRED.  PRED should not move point.  Note that parents may be
outside of region given by BEG and END.  BEG is the LIMIT
argument to `previous-single-property-change', and so may be nil.
END defaults to end of accessible portion of the buffer.

Scope parents are filtered according to
`javaimp-parse--scope-type-defun-p', but if NO-FILTER is non-nil
then no filtering is done."
  (javaimp-parse--all-scopes)
  (let ((pos (or end (point-max)))
        scope res scope-alist)
    (while (and (setq pos (previous-single-property-change
                           pos 'javaimp-parse-scope nil beg))
                (or (not beg)
                    (/= pos beg)))
      (setq scope (get-text-property pos 'javaimp-parse-scope))
      (when (and scope
                 (javaimp-scope-type scope)
                 (or (null pred)
                     (funcall pred scope)))
        (setq scope
              (javaimp-scope-copy
               scope (unless no-filter #'javaimp-parse--scope-type-defun-p)
               scope-alist))
        (push scope res)
        ;; Fill alist going up.  Stop at the first already existing
        ;; entry because its parents are already there too.
        (let ((tmp scope) done)
          (while (and tmp (not done))
            (if (alist-get (javaimp-scope-open-brace tmp) scope-alist)
                (setq done t)
              (setf (alist-get (javaimp-scope-open-brace tmp) scope-alist) tmp)
              (setq tmp (javaimp-scope-parent tmp)))))))
    res))

(defun javaimp-parse-get-enclosing-scope (&optional pred no-filter)
  "Return copy of innermost enclosing scope at point.  If PRED is
non-nil then the scope must satisfy it, otherwise the next outer
scope is tried.

Scope parents are filtered according to
`javaimp-parse--scope-type-defun-p', but if NO-FILTER is non-nil
then no filtering is done."
  (save-excursion
    (javaimp-parse--all-scopes))
  (when-let* ((scope (javaimp-parse--enclosing-scope pred)))
    (javaimp-scope-copy scope (unless no-filter
                                #'javaimp-parse--scope-type-defun-p))))

(defun javaimp-parse-get-defun-decl-start (pos &optional bound)
  "Return the position of the start of defun declaration at POS,
but not before BOUND.  POS should be at defun name, but actually
can be anywhere within the declaration, as long as it's outside
paren constructs like arg-list."
  (save-excursion
    (javaimp-parse--all-scopes))
  (javaimp-parse--decl-prefix pos bound))

(defun javaimp-parse-get-abstract-methods (scope)
  "Return abstract methods defined in SCOPE."
  (javaimp-parse--all-scopes)
  (javaimp-parse--abstract-methods scope))

(defun javaimp-parse-fully-parsed-p ()
  "Return non-nil if current buffer is fully parsed."
  (and javaimp-parse--dirty-pos
       (not (marker-position javaimp-parse--dirty-pos))))

(defmacro javaimp-parse-without-hook (&rest body)
  "Execute BODY, temporarily removing
`javaimp-parse--update-dirty-pos' from `after-change-functions'
hook."
  (declare (debug t) (indent 0))
  `(unwind-protect
       (progn
         (remove-hook 'after-change-functions #'javaimp-parse--update-dirty-pos)
         ,@body)
     (add-hook 'after-change-functions #'javaimp-parse--update-dirty-pos)))

(provide 'javaimp-parse)
