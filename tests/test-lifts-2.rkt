#lang magnolisp/2014

(typedef int (#:annos foreign))

(function (add x y) (#:annos foreign [type (fn int int int)])
  (+ x y))

(function (main3 x) (#:annos export [type (fn int int)])
  (define y 1)
  (set! y (add (add y y) 
               (begin (set! y x) y)))
  y)

(main3 7) ;; => 9
