#lang magnolisp/base

#|

A library of built-in types and functions in Magnolisp.

|#

(require (for-syntax racket/base) "runtime.rkt")

(provide predicate TRUE FALSE true false)

(typedef predicate (#:annos (foreign mgl_predicate)))
(function (TRUE) (#:annos (type (fn predicate)) (foreign mgl_TRUE)) #t)
(function (FALSE) (#:annos (type (fn predicate)) (foreign mgl_FALSE)) #f)

(define-syntax true
  (syntax-id-rules ()
    [_ (TRUE)]))

(define-syntax false
  (syntax-id-rules ()
    [_ (FALSE)]))
