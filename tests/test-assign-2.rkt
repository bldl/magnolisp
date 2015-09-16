#lang magnolisp

(typedef int #:: (foreign))

(define (f x y)
  #:: (export [type (-> int int int)])
  (let ([x y] [y x])
    (set!-values (x y) (values y x))
    x))

(f 5 5)
(f 5 6)
(f 6 5)
