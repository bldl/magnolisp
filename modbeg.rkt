#lang racket

#|

When not being required for compilation, we need nothing but the
standard Racket behavior.

When being required for compilation, we make sure to compute all the
metadata it needs, storing it into a submodule. We take care to
preserve type information.

The evaluator ignores type annotations, whereas the compiler requires
a fully typed program (although not all types have to be written out
explicitly -- think 'auto' in C++).

We have some options for preserving type information. (1) We could
carefully control macro expansion with local-expand, and make sure to
store type info into an id-table before it is expanded away. (2) We
could have the expansion itself generate a table as well as code to
persist the type information, in compile mode. It is notable that an
id-table can be used even for local names, since identifiers are
unique. Option (2) looks more promising.

Whatever we export should also have some location information. Should
we discover errors only once we start combining modules, then we need
to be able to still report errors properly.

When required for evaluation, we evaluate display the value of module
level expressions, whereas otherwise we do not. This can be useful for
testing.

In our module-begin, we must remember that we want to access
metadata from the context of the module that uses the language (and
not this module). From module-begin we can access anything within
begin-for-syntax here, but that is not good enough. It is necessary
for module-begin to generate code for the beginning module such
that the code accesses begin-for-syntax information and emits a
definition for a submodule. The submodule may have to refer directly
to the outer module's for-syntax variables so as to preserve
identities, and there for-template may be handy, although it is
possible to wrap the whole submodule definition into a
begin-for-syntax.

|#

(provide module-begin)

(require "metadata-parser.rkt"
         (for-syntax syntax/id-table
                     "settings.rkt"
                     "util.rkt"))

(define-syntax (compiled-module-begin stx)
  (syntax-case stx ()
    [(_ . bodies) 
     (with-syntax ([(mb . bodies)
                    (local-expand #'(#%module-begin . bodies)
                                 'module-begin
                                 null)])
       (with-syntax (
                     [((d-n . d-k) ...)
                      (bound-id-table-map definfo-table cons)]
                     [((t-n . t-k) ...)
                      (bound-id-table-map type-table cons)]
                     )
         ;;(writeln #'((d-n . d-k) ...))
         #'(mb (begin-for-syntax
                (module* metadata #f
                  (define re-t (make-bound-id-table #:phase 0))
                  (bound-id-table-set! re-t #'d-n d-k) ...
                  (provide (rename-out [re-t m-definfo-tbl])))
                (module* types #f
                  (define re-t (make-bound-id-table #:phase 0))
                  (bound-id-table-set! re-t #'t-n 't-k) ...
                  (provide (rename-out [re-t m-types-tbl])))
                )
               . bodies)))]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    ((_ . ds)
     ;; To avoid unnecessary work we do not export the
     ;; meta-information unless we are actually compiling. It is still
     ;; okay, though, for simplicity, to have macros produce the
     ;; information. We are just not generating submodules based on
     ;; that information.
     (if (and #f (not compile?)) ;; xxx for now always include meta
         #'(#%module-begin . ds)
         #'(compiled-module-begin . ds)))))
