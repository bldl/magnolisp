#lang racket/base

#|

Defines a Racket module language for Magnolisp.

We export racket/base as the macro programming language, and we also
provide racket/base as runtime language at present as well. Such
language may be used, but it will not be compiled into C++.

The idea is not to use Magnolisp as the macro programming language, as
Racket ought to be better for that purpose. We will not provide any
Magnolisp for-syntax.

|#

(provide (except-out (all-from-out racket/base) #%module-begin))

(require "modbeg.rkt")
(provide (rename-out [module-begin #%module-begin]))

(require (for-syntax racket/base))
(provide (for-syntax (all-from-out racket/base)))

(require "surface.rkt")
(provide (all-from-out "surface.rkt"))

(require "prelude.rkt")
(provide (all-from-out "prelude.rkt"))
