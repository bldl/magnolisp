#lang magnolisp

(require (only-in racket/base / -))

(typedef int #:: (foreign))

(define #:function mul #:: (foreign ^(-> int int int)))

(define div #:: (foreign ^(-> int int int)) /)

(define sub #:: (foreign ^(-> int int int))
  (let-racket ((lambda () (let () -)))))

(define (main x y) #:: (export)
  (sub x y)
  (mul x y)
  (div x y))

(main 2 3)
(sub 12 2)

