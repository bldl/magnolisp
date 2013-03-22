#lang racket/base

#|

We shall try to implement the entire language by mapping to
corresponding Racket values and constructs. If this should prove
infeasible, we can use a custom '#%module-begin' to do required
analysis and transformations. Should that also be infeasible, then we
can (less efficiently) implement an evaluator, perhaps by using the
same AST as the compiler uses to for analysis, and then by converting
to syntax objects and invoking 'eval-syntax'.

We should try to make sure to implement exactly the same language as
for the compiler. We should particularly pay attention to the module
system related directives. Even though they trivially work here, this may not be the case for the compiler.

|#

(require "util.rkt")
(require "runtime-common.rkt")

(provide (all-from-out "runtime-common.rkt"))

(provide begin-for-syntax
         define-for-syntax
         define-syntax
         define-syntax-rule
         provide
         require only-in prefix-in for-syntax
         begin
         #%datum)

;;; 
;;; declarations
;;; 

(define-syntax-rule*
  (procedure (n) body ...)
  (define (n) body ...))

(define-syntax-rule*
  (require/racket spec ...)
  (require spec ...))

(define-syntax-rule*
  (primitive/racket (n) body ...)
  (define (n) body ...))

(define-syntax-rule*
  (primitive/c++ (n) body ...)
  (void))

(define-syntax-rule*
  (primitive (n) (r-body ...) (c-body ...))
  (primitive/racket (n) r-body ...))

;;; 
;;; statements
;;; 

(define-syntax-rule*
  (call n)
  (n))

(define-syntax-rule*
  (pass)
  (void))
