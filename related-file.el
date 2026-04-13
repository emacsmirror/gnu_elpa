;;; related-file.el --- Finding a file whose name is related to the current one  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:
;; Package-Requires: ((external-completion "0.1"))
;; Version: 0

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

;; This package aims to provide a generalization of relative file name.
;; The idea is for the user to specify a file name by providing some
;; elements of the desired filename while the rest is provided by
;; a "base" filename, typically the name of the current file.
;;
;; More concretely, the specified file is obtained by merging the user's
;; INPUT input with the BASENAME into a resulting TARGETNAME, with
;; the following constraints:
;;
;; - All elements of INPUT appear in TARGETNAME, in the same order.
;; - The remaining elements of TARGETNAME come from BASENAME, in the same order.
;;
;; Here are some examples of valid pairs of INPUT and TARGETNAME,
;; for a BASENAME of "~/foo/bar/baz.c"
;;
;;     +-------------+----------------------+
;;     | INPUT       | TARGETNAME           |
;;     +------------------------------------+
;;     | .h          | ~/foo/bar/baz.h      |
;;     | buzz        | ~/foo/buzz           |
;;     | toto        | ~/foo/baz/toto.c     |
;;     | buzz/.h     | ~/buzz/bar/baz.h     |
;;     +-------------+----------------------+
;;
;; Clearly, contrary to what happens with /relative/ file names,
;; for given BASENAME and INPUT, there can be many different TARGETNAMEs,
;; and it is necessary to consult the filesystem decide which one is meant.

;;;; Design problems:

;; The scheme outlined above is sufficiently general and flexible
;; that it can be used to refer to many if not most files, which is great
;; since it means we could use it in general for things like `find-file'.
;;
;; Problem is: for use with `find-file' it's not sufficient to work
;; for most files, it has to work for *all* files.
;;
;; Difficult cases include:
;;
;; - Specifying the name of a file that doesn't exist yet.
;;   In that case "consult the filesystem" may not help; on the contrary
;;   it will try hard to find *another* (existing) file instead of the
;;   one you meant.
;;   E.g. you're in ~/src/foo/bar.c and you want to create a new file
;;   /etc/profile but there exists a file /src/foo/etc/profile.
;;
;; - You're in ~/src/foo/bar.c and you have the following files:
;;
;;       /etc/profile
;;       ~/etc/profile
;;       ~/src/foo/etc/profile
;;       ~/etc/foo/profile/bar.c
;;
;;   - Which one is meant by an INPUT of `etc/profile'?
;;   - What other INPUT should be used to reach the other 3?
;;
;; I don't have a general solution for the first problem.
;;
;; A possible solution for the second is to always favor the "shortest"
;; choice (i.e. /etc/profile in the above case), so that the users can get
;; to the other choices by using a more explicit/complete INPUT (in
;; the worst case typing out the full absolute name of the file).
;;
;; But that solution means that we prefer the TARGETNAME that uses as
;; little of BASENAME as possible, whereas to minimize the amount of typing
;; we'd prefer to do the opposite: use as much of BASENAME as we can.

;;; Code:

(require 'external-completion)
(require 'cl-lib)

(defcustom related-file-use-face 'font-lock-comment-face
  "Face to use to highlight the non-user-input part of the minibuffer."
  :type 'face)

(defmacro related-file--until (cond &rest body)
  (declare (debug t) (indent 1))
  (let ((res (gensym "res")))
    `(let ((,res))
       (while (not (setq ,res ,cond))
         ,@body)
       ,res)))

(defun related-file--split-fhint (shint &optional point)
  (cl-assert (not (string-match "/" shint)))
  (if (and point (<= 0 point (length shint)))
      (let ((shint-before (substring shint 0 point))
            (shint-after  (substring shint point (length shint))))
        (nconc (related-file--split-fhint shint-before)
               (list 'point)
               (related-file--split-fhint shint-after)))
    (let ((i 0)
          (res '())
          (case-fold-search nil))
      (while (string-match
              "\\*\\|[[:lower:]][[:upper:]]\\|.\\b\\|[0-9][^0-9]\\|[^0-9][0-9]"
              shint i)
        (if (not (eq ?* (aref shint (match-beginning 0))))
            (push (substring shint i (setq i (+ 1 (match-beginning 0)))) res)
          (let ((before (substring shint i (setq i (match-beginning 0)))))
            (unless (equal "" before)
              (push before res))
            ;; FIXME: Should we do something more clever about `***'?
            ;; FIXME: What about `*' at `point'?
            (unless (eq (car res) '*) (push '* res))
            (setq i (+ 1 (match-beginning 0))))))
      (nreverse (if (< i (length shint))
                    (cons (substring shint i) res)
                  res)))))

(defun related-file--split-dhint (shint point)
  (let ((i 0)
        (res '()))
    (while
        (let ((found (string-match "/" shint i)))
          (let ((head (substring shint i (if found (match-beginning 0))))
                (hpoint (- point i)))
            (setq i (match-end 0))
            (push (cond
                   ((equal head "**") '**)
                   (t (related-file--split-fhint head hpoint)))
             res))
          found))
    (nreverse res)))

(defun related-file--parse-input (input point)
  "Return (ANCHOR RELATED-PATTERN POST-PATTERN)."
  ;; Accept `/', `./', `~user/', and `../../' as specifying a start directory.
  (let* ((anchor
          (when (or (string-match "\\`\\(?:\\./\\|~[^/]*/\\|\\(?:\\.\\./\\)+\\)"
                                  input)
                    (and (file-name-absolute-p input)
                         ;; FIXME: Not clear how to find the "end" of the part
                         ;; of the filename that makes it absolute.
                         (string-match "/" input)))
            (prog1 (substring input 0 (match-end 0))
              (setq input (substring input (match-end 0))))))
         (ipoint (if anchor (- point (match-end 0)) point))
         (tail
          (when (string-match "\\`/\\(/\\)?\\|//" input)
            (when (and (equal anchor "/") (not (match-end 1)))
              (setq anchor nil))
            (prog1 (substring input (match-end 0))
              (setq input (substring input 0 (match-beginning 0))))))
         (tpoint (- ipoint (match-end 0))))
    (list anchor
          (when (> (length input) 0)
            (related-file--split-dhint input ipoint))
          (when tail (related-file--split-dhint tail tpoint)))))

(defun related-file--in-dir (dir file fhint &optional dir-only)
  "Return the list of files in DIR which match FILE+FHINT.
A file name \"matches\" if it is made of an interleaving of chunks coming
from FILE and all elements of FHINT, in order.
For example \"mbo.h\" match \"allmyfoo.c\" + (\"b\" \"h\")
because \"mbo.h\" is m+b+o.+h which does contain all the elements from
\(\"b\" \"h\") in the same order and \"allmyfoo,c\" does contain all the
other elements as well (\"m\" and \"o.\"), in the same order.
If DIR-ONLY is non-nil, ignore non-directories."
  ;; FIXME: Add support for a wildcard?
  ;; FIXME: Add support for a list of (FILE . AUX) and return for each match
  ;; the (FILE . AUX) it matched.
  (cl-assert (equal dir (file-name-as-directory dir)))
  (if (null fhint)
      (if (and file (file-exists-p (file-name-concat dir file)))
          (list (list file file)))
    (let ((re (mapconcat #'regexp-quote (remq 'point (remq '* fhint)) ".*"))
          (names ()))
      (when (file-directory-p dir)
        (dolist (candidate (directory-files dir nil re))
          (unless (and dir-only (not (file-directory-p
                                      (file-name-concat dir candidate))))
            (named-let loop ((hints fhint)
                             (chunks ())
                             (bc 0)
                             (bf 0))
              (let (wild)
                (while (memq (car hints) '(* point))
                  (setq hints (cdr hints))
                  (setq wild t))
                (if (null hints)
                    (let* ((suffix (substring candidate bc)))
                      (when (or wild
                                (string-search suffix (or file "")))
                        (push (cons candidate
                                    (reverse (cons suffix chunks)))
                              names)))
                  (let ((hint (car hints))
                        (i bc)
                        mbc)
                    (while (setq mbc (string-search hint candidate i))
                      (let* ((mec (+ mbc (length hint)))
                             (prefix (substring candidate bc mbc))
                             (mbf (or wild (string-search prefix (or file "") bf))))
                        (when mbf
                          (loop (cdr hints) `(,(intern hint) ,prefix ,@chunks)
                                mec (if (numberp mbf) (+ mbf (length prefix)) bf)))
                        (setq i mec)))))))))
        ;; Prefer longer names, i.e. names which keep more of FILE and are
        ;; hence "closer".
        (sort names :key #'length :reverse t)))))

(defun related-file--in-subdirs (dir file dhint)
  (cl-assert (equal dir (file-name-as-directory dir)))
  ;; (if (null dhint)
  ;;     (if (file-exists-p (file-name-concat dir file))
  ;;         (list file))
  (let ((fhint (car dhint))
        (dhints (cdr dhint))
        (res ()))
    (if (eq fhint '**)
        (error "Not implemented yet!")
      (while
          (let* ((fhead (pop file))
                 (dir-fhead (when fhead (file-name-concat dir fhead)))
                 (matches (related-file--in-dir dir fhead fhint dhints))
                 (rest (mapcan
                        (lambda (match)
                          (let* ((f
                                  (file-name-concat dir (car match)))
                                 (submatches
                                  (if (not (or dhints
                                               (and file (file-directory-p f))))
                                      (list nil)
                                    (related-file--in-subdirs
                                     (file-name-as-directory f)
                                     file dhints))))
                            (mapcar (lambda (submatch)
                                      (if (null submatch)
                                          (cdr match)
                                        ;; FIXME: "/" or `/'?
                                        (append (cdr match) '("/") submatch)))
                                    (if (null dhints)
                                        (append submatches (list nil))
                                      submatches))))
                        matches)))
            (setq res (nconc res rest))
            (when (and fhead (file-exists-p dir-fhead))
              (setq res (nconc res
                               (if (null dhint)
                                   (list (list fhead))
                                 (when (file-directory-p dir-fhead)
                                   ;; FIXME: "/" or `/'?
                                   (mapcar (lambda (f) `(,fhead "/" ,@f))
                                           (related-file--in-subdirs
                                            (file-name-as-directory dir-fhead)
                                            file dhint)))))))
            file))
      (delete-dups res))))

(defun related-file (input basename &optional point)
  (setq basename (expand-file-name basename))
  (pcase-let*
      ((dirf (directory-file-name basename))
       (dir  (if (equal dirf basename) (file-name-directory dirf) basename))
       (basename (if (equal dirf basename)
                     (list (file-name-nondirectory dirf))))
       ;; FIXME: Make use of TAIL!
       (`(,anchor ,dhint ,tail)
        (related-file--parse-input input (or point -1))))
    (related-file--until
        (let ((matches
               (and (or (null anchor)
                        (and (equal dir anchor) (setq anchor 'done)))
                    (related-file--in-subdirs dir basename dhint))))
          (if matches
              (cons dir matches)
            (let* ((dirf (directory-file-name dir))
                   (newdir (file-name-directory dirf)))
              (if (not (and newdir (< (length newdir) (length dir))))
                  ;; In case we didn't pass by the anchor, force-feed it.
                  ;; E.g. This can happen if BASENAME started as "a:/foo/bar"
                  ;; and INPUT was "b:/baz".
                  (if (stringp anchor)
                      (progn (setq dir anchor) nil)
                    t)
                (push (file-name-nondirectory dirf) basename)
                (setq dir newdir)
                nil)))))))

(defvar related-file--read-history nil)

(defun related-file--to-string (dir match &optional completion-highlight)
  ;; FIXME: Still to add the `completions-first-difference' in the first
  ;; char after point.
  (concat dir (mapconcat (lambda (x)
                           (cond
                            ((stringp x) x)
                            ((symbolp x)
                             (if (null completion-highlight)
                                 (symbol-name x)
                               (propertize (symbol-name x)
                                           'face 'completions-common-part)))
                            (t (signal 'wrong-type-argument
                                       (list '(or string symbol) x)))))
                         match)))

(defvar related-file--timer nil)

(defun related-file--on-the-fly (minibuf basename predicate)
  (setq related-file--timer nil)
  (with-current-buffer minibuf
    (let* ((beg (minibuffer-prompt-end))
           (input (buffer-substring-no-properties beg (point-max)))
           (matches (related-file input basename))
           (match (car (cdr-safe matches))))
      (remove-overlays beg (point-max) 'related-file t)
      (when (consp matches)
        (save-excursion
          (goto-char beg)
          (let* ((strs (list (car matches)))
                 (mkol
                  (lambda (beg end)
                    (let ((ol (make-overlay beg end nil nil t)))
                      (overlay-put ol 'related-file t)
                      (overlay-put ol 'before-string
                                   (let ((str (mapconcat #'identity
                                                         (nreverse strs) "")))
                                     (setq strs nil)
                                     (if related-file-use-face
                                         (propertize
                                          str 'face related-file-use-face)
                                       (concat "{" str "}"))))))))
            (dolist (x match)
              (cond
               ((stringp x) (push x strs))
               ((symbolp x)
                (let ((name (symbol-name x)))
                  (search-forward name)
                  (funcall mkol (match-beginning 0) (match-end 0))))
               (t (signal 'wrong-type-argument
                          (list '(or string symbol) x)))))
            (when strs
              (funcall mkol (point-max) (point-max)))))))))


(defun related-file--acf (basename predicate)
  (lambda (_ _ _)
    (unless related-file--timer
      (setq related-file--timer
            (run-with-idle-timer 0.1 nil
                                 #'related-file--on-the-fly
                                 (current-buffer)
                                 basename
                                 predicate)))))
  


(defun related-file--completion-table (basename predicate)
  (external-completion-table
   'related-file
   (lambda (input point)
     (let ((matches (related-file input basename point)))
       (if (not (consp matches))
           nil
         (let ((dir (car matches)))
           (mapcar (lambda (match) (related-file--to-string nil match t))
                   (cdr matches))))))))

;;;###autoload
(defun read-related-file-name (prompt
                               &optional basename mustmatch initial predicate)
  (unless basename (setq basename (or buffer-file-name default-directory)))
  (minibuffer-with-setup-hook
      (lambda ()
        (add-hook 'after-change-functions
                  (related-file--acf basename predicate) nil t)
        ;; (add-hook 'post-command-hook (related-file--pch basename predicate)
        ;;           nil t)
        )
    (let* ((res (completing-read prompt
                                 (related-file--completion-table
                                  basename predicate)
                                 nil mustmatch initial
                                 'related-file--read-history))
           (matches (related-file res basename)))
      (related-file--to-string (car matches) (cadr matches)))))

(provide 'related-file)
;;; related-file.el ends here
