#lang racket/base

#|

Defines a Racket module language for Magnolisp, but with the 2014
edition of the surface syntax. Exists for backward compatibility.

We export racket/base as the macro programming language, and we also
provide racket/base as runtime language at present as well. Such
language may be used, but it will not be compiled into C++.

The idea is not to use Magnolisp as the macro programming language, as
Racket ought to be better for that purpose. We will not provide any
Magnolisp for-syntax.

|#

(module reader syntax/module-reader 
  magnolisp/main
  #:wrapper1 (lambda (t)
               ;; No need to replace reader altogether, just override
               ;; readtable.
               (with-magnolisp-readtable
                   (t)))
  ;; Import readtable. Note that it needs to be in the `reader`
  ;; submodule scope, not the outer one.
  (require magnolisp/reader-ext))

(provide (except-out (all-from-out racket/base) #%module-begin))

(require "modbeg.rkt")
(provide (rename-out [module-begin #%module-begin]))

(require (for-syntax racket/base))
(provide (for-syntax (all-from-out racket/base)))

(require "surface.rkt")
(provide (all-from-out "surface.rkt"))

(require "prelude.rkt")
(provide (all-from-out "prelude.rkt"))
