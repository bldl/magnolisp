#lang racket/base

#|

Defines a Racket module language for Magnolisp. Implements an
evaluator on top of Racket in your usual way, provided that the Racket
backend is being targeted. This can be useful for testing and as a
simulator, to avoid having to build everything via C++. But do not
expect to test everything like this, as you will be lacking primitives
implemented in C++.

We export racket/base as the macro programming language, and we also
provide racket/base as runtime language at present as well. Such
language may be used, but it will not be compiled into C++.

The idea is not to use Magnolisp as the macro programming language, as
Racket ought to be better for that purpose. We will not provide any
Magnolisp for-syntax. If a macro uses Magnolisp, things will get
screwy when used in compilation mode.

|#

(provide (except-out (all-from-out racket/base) #%module-begin))

(require "modbeg.rkt")
(provide (rename-out (module-begin #%module-begin)))

(require (for-syntax racket/base))
(provide (for-syntax (all-from-out racket/base)))

(require "runtime.rkt")
(provide (all-from-out "runtime.rkt"))
