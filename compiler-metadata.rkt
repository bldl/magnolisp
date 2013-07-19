#lang racket

#|

The definitions in this module are mostly exported for-syntax, as the
information is only accessed at macro expansion time.

Note quite sure if we should specify any #:phase for the identifier
tables, but phase 0 would seem appropriate as all Magnolisp names are
such.

|#

(begin-for-syntax

 (require "util.rkt")
 (require syntax/id-table)

 (define type-table (make-bound-id-table #:phase 0))
 (provide type-table)
 
 (define* (record-type! id-stx t)
   (bound-id-table-set! type-table id-stx t))

 ) ; end begin-for-syntax

(require "util.rkt"
         (for-syntax "metadata-defs.rkt"))

(define-syntax* (begin/save-type stx)
  (syntax-case stx ()
    [(_ n t)
     (record-type! #'n (TypeName (syntax->datum #'t)))
     #'(begin)]
    [(_ n)
     (record-type! #'n AnyT)
     #'(begin)]
    ))
