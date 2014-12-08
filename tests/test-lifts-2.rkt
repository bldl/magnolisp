#lang magnolisp/2014

(typedef int (#:annos foreign))

(function (add x y) (#:annos foreign [type (fn int int int)])
  (+ x y))

(function (main3 x) (#:annos export [type (fn int int)])
  (do
    (var y 1)
    (set! y (add (add y y) 
                 (do (set! y x) 
                     (return y))))
    (return y)))

(main3 7) ;; => 9
