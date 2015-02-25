#lang magnolisp

(require (only-in racket/base add1 for/hash for/list map))

(typedef int #:: (foreign))

(define (unused-in-cxx)
  (let ((x (let-racket
             (define-values (x y)
               (let ()
                 (values
                  (let c ()
                    5)
                  (for/list ((i '(1 2 3)))
                    (add1 i)))))
             x)))
    x))

(function (f x)
  #:: (export [type (-> int int)])
  (begin-racket (map add1 '(4 5 6)))
  x)

(f (unused-in-cxx))

(begin-racket
  (for/hash ((k '(7 8 9)))
    (values k k)))
