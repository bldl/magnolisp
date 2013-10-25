#lang racket

#|
|#

(provide struct-type-of
         struct-make-constructor
         struct-symbol)

(define (struct-type-of v)
  (define-values (x y) (struct-info v))
  x)

(define (struct-make-constructor v)
  (struct-type-make-constructor (struct-type-of v)))

(define (struct-symbol v)
  (define-values (sym d2 d3 d4 d5 d6 d7 d8)
    (struct-type-info (struct-type-of v)))
  sym)
