;;; related-file.el --- Finding a file whose name is related to the current one  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:
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

(defun related-file--split-fhint (shint &optional point)
  (cl-assert (not (string-match "/" shint)))
  (if (and point (<= 0 point (length shint)))
      (let ((shint-before (substring shint 0 point))
            (shint-after  (substring shint point (length shint))))
        (nconc (related-file--split-fhint shint-before)
               (list '*)
               (related-file--split-fhint shint-after)))
    (let ((i 0)
          (res '())
          (case-fold-search nil))
      (while (string-match
              "\\*\\|[[:lower:]][[:upper:]].\\b\\|[0-9][^0-9]\\|[^0-9][0-9]"
              shint i)
        (if (not (eq ?* (aref shint (match-beginning 0))))
            (push (substring shint i (setq i (+ 1 (match-beginning 0)))) res)
          (let ((before (substring shint i (setq i (match-beginning 0)))))
            (unless (equal "" before)
              (push before res))
            (push '* res)
            (setq i (+ 1 (match-beginning 0))))))
      (nreverse (if (< i (length shint))
                    (cons (substring shint i) res)
                  res)))))

(defun related-file--split-dhint (shint point)
  (let ((i 0)
        (res '()))
    (while (string-match "/" shint i)
      (let ((head (substring shint i (match-beginning 0)))
            (hpoint (- point i)))
        (setq i (match-end 0))
        (push (cond
               ((equal head "") '**)
               (t (related-file--split-fhint head hpoint)))
              res)))
    (push (related-file--split-fhint (substring shint i) (- point i)) res)
    (nreverse res)))

(defun related-file--in-dir (dir file fhint &optional dir-only)
  "Return the list of files in DIR which match FILE+FHINT.
A file name \"matches\" if it is made of an alternation of chunks coming
from FILE and all elements of FHINT, in order.
For example \"mbo.h\" match \"allmyfoo,c\" + (\"b\" \"h\")
because \"mbo.h\" is m+b+o.+h which does contain all the elements from
\(\"b\" \"h\") in the same order and \"allmyfoo,c\" does contain all the
other elements as well (\"m\" and \"o.\"), in the same order.
If DIR-ONLY is non-nil, ignor non-directories."
  ;; FIXME: Add support for a wildcard?
  ;; FIXME: Add support for a list of (FILE . AUX) and return for each match
  ;; the (FILE . AUX) it matched.
  (cl-assert (equal dir (file-name-as-directory dir)))
  (if (null fhint)
      (if (and file (file-exists-p (file-name-concat dir file)))
          (list (list file file)))
    (let ((re (mapconcat #'regexp-quote (remq '* fhint) ".*"))
          (names ()))
      (dolist (candidate (directory-files dir nil re))
        (unless (and dir-only (not (file-directory-p
                                    (file-name-concat dir candidate))))
          (named-let loop ((hints fhint)
                           (chunks ())
                           (bc 0)
                           (bf 0))
            (let (wild)
              (while (eq '* (car hints))
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
      (sort names :key #'length :reverse t))))

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
                                        (append (cdr match) '("/") submatch)))
                                    (if (null dhints)
                                        (cons nil submatches)
                                      submatches))))
                        matches)))
            (setq res (nconc res rest))
            (when (and fhead (file-exists-p dir-fhead))
              (setq res (nconc res
                               (if (null dhint)
                                   (list (list fhead))
                                 (when (file-directory-p dir-fhead)
                                   (mapcar (lambda (f) `(,fhead "/" ,@f))
                                           (related-file--in-subdirs
                                            (file-name-as-directory dir-fhead)
                                            file dhint)))))))
            file))
      (delete-dups res))))

(defun related-file (shint file)
  (setq file (expand-file-name file))
  (let* ((dirf (directory-file-name file))
         (dir  (if (equal dirf file) (file-name-directory dirf) file))
         (file (if (equal dirf file) (list (file-name-nondirectory dirf))))
         (dhint (related-file--split-dhint shint (length shint)))
         res)
    (while
        (let ((matches (related-file--in-subdirs dir file dhint)))
          (if matches
              (progn
                (setq res (cons dir matches))
                ;; Stop the search.
                nil)
            (let* ((dirf (directory-file-name dir))
                   (newdir (file-name-directory dirf)))
              (when (and newdir (< (length newdir) (length dir)))
                (push (file-name-nondirectory dirf) file)
                (setq dir newdir)
                t)))))
    res))

(provide 'related-file)
;;; related-file.el ends here
