#lang racket

#|

Implements a compiler for Magnolisp. Loads the code to be compiled
from Racket module metadata, included as submodules by the Racket
'magnolisp' language implementation.

The compiler ignores module top-level expressions.

The compiler requires a fully typed program (although not all types
have to be written out explicitly -- think 'auto' in C++).

Compiles only the 'entry-point' operations of the specified modules,
and their dependencies. This essentially means full program/library
optimization.

Generates a single .cpp, .hpp, and .mk output file. The header file
only declares the entry-point operations, and the abstract types they
depend upon. The implementation file has an non-entry-points declared
as internal (e.g., static). The GNU Make include file lists the
external dependencies for the program/library, as well as the .cpp and
.hpp files.

|#

(require "util.rkt"
         syntax/id-table syntax/moddep)

;; Compilation state.
(struct St () #:transparent)

;; Returns an empty compilation state.
(define* (new-state)
  (St))

;; Returns an updated compilation state.
(define* (compile-module st mp)
  (define m-annos
    (dynamic-require `(submod ,mp definfo) 'm-annos))
  (define m-ast
    (dynamic-require `(submod ,mp definfo) 'm-ast))
  st)

;; Returns an updated compilation state.
(define* (compile-file st fn)
  (define s (if (path? fn) (path->string fn) fn))
  (define mp `(file ,s))
  (compile-module st mp))

(define* (write-cpp-file st cpp-file)
  (void)) ;;xxx

(define* (write-hpp-file st hpp-file)
  (void)) ;;xxx

(define* (write-mk-file st mk-file)
  (void)) ;;xxx

(module* main #f
  (define st (new-state))
  (set! st (compile-module st "test-6-prog.rkt"))
  )
