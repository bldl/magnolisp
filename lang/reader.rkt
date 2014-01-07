#lang s-exp syntax/module-reader
magnolisp/main
#:wrapper1 (lambda (t)
             ;; No need to replace reader altogether, just override
             ;; readtable.
             (with-magnolisp-readtable
               (t)))
(require magnolisp/reader-ext) ;; import readtable
