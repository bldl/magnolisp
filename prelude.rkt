#lang magnolisp/base

#|

A library of built-in types and functions in Magnolisp.

|#

(require (for-syntax racket/base) "surface.rkt")

(provide predicate TRUE FALSE true false)

(typedef predicate (#:annos [foreign mgl_predicate]))
(function (TRUE) (#:annos [foreign mgl_TRUE] [type (fn predicate)]) #t)
(function (FALSE) (#:annos [foreign mgl_FALSE] [type (fn predicate)]) #f)

(define-syntax true
  (syntax-id-rules ()
    [_ (TRUE)]))

(define-syntax false
  (syntax-id-rules ()
    [_ (FALSE)]))
