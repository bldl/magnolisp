#lang magnolisp
(require (only-in racket/base +))

(typedef int #:: (foreign))

(function (one)
  #:: (foreign (type (-> int)))
  1)

(function (add x y)
  #:: (foreign (type (-> int int int)) (build need-adder))
  (+ x y))

(function (f x)
  #:: (export (type (-> int int)))
  (define y (let ((x x)) (add (one) x)))
  y)

(f 1)
