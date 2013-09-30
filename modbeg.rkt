#lang racket

#|

When not being required for compilation or for editor support or such,
we need nothing but the standard Racket behavior.

For when being required for compilation, we must make sure that we
also make available compilation-related metadata in a submodule, which
could be called 'definfo' or something. We take care to preserve type
information, and put it into said module. We also want to put an AST
in there.

The evaluator ignores type annotations (it does not even load them),
whereas the compiler requires a fully typed program (although not all
types have to be written out explicitly -- think 'auto' in C++).

We have some options for preserving type information, but we opt for
having the expansion itself generate a table as well as code to
persist the type information. It is notable that an id-table can be
used even for local names, since identifiers are unique.

Whatever we export should also have some location information. Should
we discover errors only once we start actual compilation or linking,
then we need to be able to still report errors properly.

When required for evaluation, we display the value of module top-level
expressions, whereas otherwise we do not. This can be useful for
testing.

To record metadata for the compiler, we use code that runs in phase
level 1, but concerns phase level 0. Since the recording code lives in
phase 1, the respective module's #%module-begin will be executed in
the same phase, and will hence have access to the information (via the
same variables at the same phase level).

|#

(provide module-begin)

(require "definfo-store.rkt"
         (for-syntax racket/pretty syntax/id-table
                     "util.rkt"))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ . bodies) 
     (with-syntax ([(mb . bodies)
                    (local-expand #'(#%module-begin . bodies)
                                  'module-begin null)])
       (with-syntax ([((d-n . d-k) ...)
                      (bound-id-table-map definfo-table cons)])
         ;;(writeln #'((d-n . d-k) ...))
         (let ((mb-stx
                #'(mb (begin-for-syntax
                       (module* definfo #f
                         (define re-t (make-bound-id-table #:phase 0))
                         (bound-id-table-set! re-t #'d-n d-k) ...
                         (provide (rename-out [re-t m-definfo-tbl]))))
                      . bodies)))
           ;;(pretty-print (syntax->datum mb-stx))
           mb-stx)))]))
