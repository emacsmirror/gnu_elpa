;; Emacs wisi project definitions for compiling gpr-query in ELPA or devel workspace -*- no-byte-compile: t lexical-binding:t; -*-
;;
;; Copyright (C) 2021, 2022, 2025  Free Software Foundation, Inc.

(wisi-prj-select-cache
   "gpr-query.prj"
   (create-ada-prj
    :name "gpr-query elpa"
    :compile-env
    (list
     (concat "GNAT_COMPILER=" (expand-file-name "../gnat-compiler"))
     (concat "WISI=" (expand-file-name "../wisi"))
     ))
   "ELPA.make"
   )

(ada-parse-require-process) ;; slow start due to lr1 parse table
;; end of file
