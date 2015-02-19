#lang magnolisp

(define #:type int #:: (foreign))

(define (run x) #:: (export)
  (define y x)
  (define z #:: (^int) y)
  z)

(run 7)
