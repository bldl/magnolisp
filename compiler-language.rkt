#lang racket/base

#|

Defines a Racket module language for Magnolisp. This language is not
intended to be evaluated, but rather compiled. Macro expansion will
happen, though.

|#

(provide (except-out (all-from-out racket/base) #%module-begin))

(provide (rename-out (#%plain-module-begin #%module-begin)))

(require (for-syntax racket/base))
(provide (for-syntax (all-from-out racket/base)))

(require "runtime.rkt")
(provide (all-from-out "runtime.rkt"))
