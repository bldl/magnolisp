#lang racket/base

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

The idea is not to use Magnolisp as the macro programming language, as
Racket ought to be better for that purpose. We will not provide any
Magnolisp for-syntax. If Magnolisp were required for the purpose of
macro programming, it had better be the evaluator language, not the
compiled one.

|#

(provide (except-out (all-from-out racket/base) #%module-begin))

(require "modbeg.rkt")
(provide (rename-out (my-module-begin #%module-begin)))

(require (for-syntax racket/base))
(provide (for-syntax (all-from-out racket/base)))

(require "evaluator-runtime.rkt")
(provide (all-from-out "evaluator-runtime.rkt"))
