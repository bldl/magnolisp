#lang racket/base

#|

Defines a Racket module language for Magnolisp. Implements an
evaluator on top of Racket in your usual way. This can be useful for
testing and as a simulator, to avoid having to build everything via
C++.

We will have a separate API for compiling to C++, and the compiler
ignores '#lang magnolisp' signatures.

The evaluator evaluates top-level expressions, whereas the compiler
ignores them. This can be useful for testing.

The evaluator ignores type annotations, whereas the compiler requires
a fully typed program (although not all types have to be written out
explicitly -- think 'auto' in C++).

|#

;;; 
;;; exports
;;; 

;; (require racket/base)
;; (provide (rename-out (my-module-begin #%module-begin)))
;; (provide (except-out (all-from-out racket/base) #%module-begin))

(provide #%module-begin)

(require "runtime.rkt")
(provide (all-from-out "runtime.rkt"))

(require (for-syntax racket/base))
(provide (for-syntax (all-from-out racket/base)))

