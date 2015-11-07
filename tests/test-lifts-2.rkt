#lang magnolisp
(require (only-in racket/base +))

(typedef int #:: (foreign))

(function (add x y) #:: (foreign [type (-> int int int)])
  (+ x y))

(function (main3 x) #:: (export [type (-> int int)])
  (define y 1)
  (set! y (add (add y y) 
               (begin (set! y x) y)))
  y)

(main3 7) ;; => 9
