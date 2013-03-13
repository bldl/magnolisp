#lang racket/base

#|

We shall try to implement the entire language by mapping to
corresponding Racket values and constructs. If this should prove
infeasible, we can use a custom '#%module-begin' to do required
analysis and transformations. Should that also be infeasible, then we
can (less efficiently) implement an evaluator, perhaps by using the
same AST as the compiler uses to for analysis, and then by converting
to syntax objects and invoking 'eval-syntax'.

|#

(require "util.rkt")
(require "runtime-common.rkt")

(provide (all-from-out "runtime-common.rkt"))

;;; 
;;; declarations
;;; 

(define-syntax-rule*
  (procedure (n) body ...)
  (define (n) body ...))

;;; 
;;; statements
;;; 

(define-syntax-rule*
  (call n)
  (n))

(define-syntax-rule*
  (pass)
  (void))
