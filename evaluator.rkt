#lang racket

#|

Defines a Racket module language for Magnolisp. Implements an
evaluator on top of Racket in your usual way. This can be useful for
testing and as a simulator, to avoid having to build everything via
C++. But do not expect to test everything like this, as you will be
lacking primitives implemented in C++.

The evaluator evaluates top-level expressions, whereas the compiler
ignores them. This can be useful for testing.

The evaluator ignores type annotations, whereas the compiler requires
a fully typed program (although not all types have to be written out
explicitly -- think 'auto' in C++).

We export racket/base as the macro programming language, and we also
provide racket/base as runtime language at present as well. As the
compiler likely will not support all of that language, we will likely
be more selective once we know what the compiler will support.

In our custom #%module-begin we do not macro expand evaluator
language, as that language is different. We cannot easily share work
between macro expanding evaluated and compiled language. All that we
do is macro expand and then store compiled language.

local-expand uses the lexical context of the expression currently
being expanded. In here the outermost expansion is that of
#%module-begin and happens in the lexical context of said module.

|#

(require racket/base)
(provide (rename-out (my-module-begin #%module-begin)))
(provide (except-out (all-from-out racket/base) #%module-begin))

(require (for-syntax racket/base))
(provide (for-syntax (all-from-out racket/base)))

(require "runtime-evaluator.rkt")
(provide (all-from-out "runtime-evaluator.rkt"))

;; xxx how to get local-expand to use the correct namespace - how does it determine the namespace to use - we want "runtime-compiler.rkt" for this expansion only
(begin-for-syntax
 (require "util.rkt" syntax/strip-context)

 (define (make-ast-submodule modbeg-stx stx-lst)
   (define stx
     (parameterize ([current-namespace (make-base-namespace)])
       (expand
        (strip-context
         #`(module some racket/base
             (require (for-syntax racket/base))
             (require "runtime-compiler.rkt")
             #,@stx-lst)))))

   (pretty-println (syntax->datum stx))
    
   ;; Note that we use racket/base here as this is simply a Racket
   ;; module containing annotations. It is not Magnolisp. The data
   ;; structures containing the annotations will be in Racket.
   #`(module ast racket/base
       (begin))))

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    ((_ x ...)
     #`(#%module-begin
        x ...
        #,(make-ast-submodule stx (syntax->list #'(x ...)))))))
