#lang magnolisp/2014

(typedef int (#:annos foreign))

(function (add x y) (#:annos foreign [type (fn int int int)])
  (+ x y))

(function (main1 x) (#:annos export [type (fn int int)])
  (define y 1)
  (set! y (add y (add (begin (set! y x) x) y)))
  y)

(function (main2 x) (#:annos export [type (fn int int)])
  (define y 1)
  (set! y (add (add y (begin (set! y x) x)) y))
  y)

(function (main3 x) (#:annos export [type (fn int int)])
  (define y 1)
  (set! y (add (add y y) (begin (set! y x) y)))
  y)

(function (main4 x) (#:annos export [type (fn int int)])
  (define y 1)
  (set! y (add (begin (set! y x) y) (add y y)))
  y)

(function (main5 x) (#:annos export [type (fn int int)])
  (define y 1)
  (set! y (add (begin (set! y x) (add y y)) (add y y)))
  y)

(function (main6 x) (#:annos export [type (fn int int)])
  (let ()
    (define y 1)
    (set! y (add 
             (begin
               (set! y (begin (set! y (add x y)) 
                              y))
                 y)
             (add y y)))
    y))

(main1 7) ;; => 15
(main2 7) ;; => 15
(main3 7) ;; => 9
(main4 7) ;; => 21
(main5 7) ;; => 28
(main6 7) ;; => 24
