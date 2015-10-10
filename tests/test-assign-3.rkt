#lang magnolisp

(typedef int #:: (foreign))

(define (f x y)
  #:: (export [type (-> int int int)])
  (set! x y)
  x)

(f 1 7)

(define (g x y)
  #:: (export [type (-> int int int)])
  (set! x (f x y)) 
  x)

(g 1 7)

(define (h x y)
  #:: (export [type (-> int int int)])
  (set!-values (x y) (values (f x y) (f y x)))
  x)

(h 1 7)
