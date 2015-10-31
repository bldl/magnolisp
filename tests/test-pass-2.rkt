#lang magnolisp

(typedef int #:: (foreign))

(function (one)
  #:: (foreign (type (-> int)))
  1)

(function (add x y)
  #:: (foreign (type (-> int int int)))
  (let-racket/require ((+))
   (+ x y)))

(function (f x)
  #:: (export (type (-> int int)))
  (let ()
    (void)
    x
    (void))
  x)

(f 1)
