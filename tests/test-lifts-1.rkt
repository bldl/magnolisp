#lang magnolisp

(typedef int (#:annos foreign))

(function (add x y) (#:annos foreign [type (fn int int int)])
  (+ x y))

(function (main1 x) (#:annos export [type (fn int int)])
  (do
    (var y 1)
    (set! y (add y (add (do (set! y x) (return x)) y)))
    (return y)))

(function (main2 x) (#:annos export [type (fn int int)])
  (do
    (var y 1)
    (set! y (add (add y (do (set! y x) (return x))) y))
    (return y)))

(function (main3 x) (#:annos export [type (fn int int)])
  (do
    (var y 1)
    (set! y (add (add y y) (do (set! y x) (return y))))
    (return y)))

(function (main4 x) (#:annos export [type (fn int int)])
  (do
    (var y 1)
    (set! y (add (do (set! y x) (return y)) (add y y)))
    (return y)))

(function (main5 x) (#:annos export [type (fn int int)])
  (do
    (var y 1)
    (set! y (add (do (set! y x) (return (add y y))) (add y y)))
    (return y)))

(function (main6 x) (#:annos export [type (fn int int)])
  (do
    (var y 1)
    (set! y (add 
             (do (set! y (do (set! y (add x y)) 
                             (return y))) 
                 (return y))
             (add y y)))
    (return y)))

(main1 7) ;; => 15
(main2 7) ;; => 15
(main3 7) ;; => 9
(main4 7) ;; => 21
(main5 7) ;; => 28
(main6 7) ;; => 24
