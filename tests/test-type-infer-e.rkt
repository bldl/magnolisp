#lang magnolisp

(typedef int #:: (foreign))

(define (identity x)
  x)

(function (main x) #:: (export)
  (identity (cast int x)))
