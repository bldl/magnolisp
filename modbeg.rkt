#lang racket

#|

When not being required for compilation, we need nothing but the
standard Racket behavior.

When being required for compilation, we make sure to compute all the
metadata it needs, storing it into a submodule. We take care to
preserve type information.

We have some options for preserving type information. (1) We could
carefully control macro expansion with local-expand, and make sure to
store type info into an id-table before it is expanded away. (2) We
could have the expansion itself generate code to persist the type
information, in compile mode. It is notable that an id-table can be
used even for local names, since identifiers are unique.

|#

(provide my-module-begin)

(begin-for-syntax
 (require "settings.rkt" "util.rkt")

 (define (make-ast-submodule modbeg-stx stx-lst)
   ;; Note that we use racket/base here as this is simply a Racket
   ;; module containing annotations. It is not Magnolisp. The data
   ;; structures containing the annotations will be in Racket.
   #`(module ast racket/base
       (provide src-sexp-lst)
       ;; xxx otherwise good but we lose location info - should instead generate code that creates syntax objects with location information
       (define src-sexp-lst '(#,@stx-lst)))))

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    ((_ x ...)
     #`(#%module-begin
        x ...
        #,(make-ast-submodule stx (syntax->list #'(x ...)))))))
