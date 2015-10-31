#lang magnolisp
(require (only-in racket/base +)) 

(typedef int #:: (foreign))

(function (one)
  #:: (foreign (type (-> int)))
  1)

(function (add x y)
  #:: (foreign (type (-> int int int)))
  (+ x y))

(function (f x)
  #:: (export (type (-> int int)))
  (begin
    (void)
    (begin
      (void)
      x
      (void))
    x))

(f 1)
