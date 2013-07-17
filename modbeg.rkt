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
could have the expansion itself generate code to persist the type
information, in compile mode. It is notable that an id-table can be
used even for local names, since identifiers are unique.

Whatever we export should also have some location information. Should
we discover errors only once we start combining modules, then we need
to be able to still report errors properly.

When required for evaluation, we evaluate display the value of module
level expressions, whereas otherwise we do not. This can be useful for
testing.

In our my-module-begin, we must remember that we want to access
metadata from the context of the module that uses the language (and
not this module). From my-module-begin we can access anything within
begin-for-syntax here, but that is not good enough. It is necessary
for my-module-begin to generate code for the beginning module such
that the code accesses begin-for-syntax information and emits a
definition for a submodule. The submodule may have to refer directly
to the outer module's for-syntax variables so as to preserve
identities, and there for-template may be handy, although it may be
possible to wrap the whole submodule definition into a
begin-for-syntax.

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
     (if-not-compiling
      #`(#%module-begin x ...)
      (let ((exp-stx (local-expand #'(begin x ...) 'module-begin null)))
        #`(#%plain-module-begin
           #,exp-stx
           #,(make-submodule stx exp-stx)))))))
