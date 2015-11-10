#lang magnolisp
(require (only-in racket/base +))

(typedef int #:: (foreign))

(function (add x y) #:: (foreign [type (-> int int int)])
  (+ x y))

(function (main1 x) #:: (export [type (-> int int)])
  (define y 1)
  (set! y (add y (add (begin (set! y x) x) y)))
  y)

(function (main2 x) #:: (export [type (-> int int)])
  (define y 1)
  (set! y (add (add y (begin (set! y x) x)) y))
  y)

(function (main3 x) #:: (export [type (-> int int)])
  (define y 1)
  (set! y (add (add y y) (begin (set! y x) y)))
  y)

(function (main4 x) #:: (export [type (-> int int)])
  (define y 1)
  (set! y (add (begin (set! y x) y) (add y y)))
  y)

(function (main5 x) #:: (export [type (-> int int)])
  (define y 1)
  (set! y (add (begin (set! y x) (add y y)) (add y y)))
  y)

(function (main6 x) #:: (export [type (-> int int)])
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
