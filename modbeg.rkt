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

Whatever we export should also have some location information. Should
we discover errors only once we start combining modules, then we need
to be able to still report errors properly.

|#

(provide my-module-begin)

(begin-for-syntax
 (require "compiler-metadata.rkt" "settings.rkt" "util.rkt")

 (define (make-submodule modbeg-stx exp-stx)
   #`(module compilation-info racket/base
       (provide dummy)
       (define dummy '#,exp-stx)))

 ) ;; end begin-for-syntax

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    ((_ x ...)
     (if (not compile?)
         #`(#%module-begin x ...)
         (let ((exp-stx (local-expand #'(begin x ...) 'module-begin null)))
           #`(#%plain-module-begin
              #,exp-stx
              #,(make-submodule stx exp-stx)))))))
