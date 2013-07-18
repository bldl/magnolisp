#lang s-exp syntax/module-reader
magnolisp/evaluator-language
#:wrapper1 (lambda (t)
             ;; No need to replace reader altogether, just override
             ;; readtable.
             (parameterize ((current-readtable magnolisp-readtable))
               (t)))
(require magnolisp/reader-ext) ;; import readtable
