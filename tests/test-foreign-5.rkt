#lang magnolisp

(require (only-in racket/base + - /))

(typedef int #:: (foreign))

(define (mul x y) #:: (foreign ^(-> int int int)))

(define div #:: (foreign ^(-> int int int)) /)

(define sub #:: (foreign ^(-> int int int))
  (begin-racket ((lambda () (let () -)))))

(define add #:: (foreign) +)

(define another-add #:: (foreign ^(-> int int int)) +)

(define (main x y) #:: (export)
  (sub x (add y y))
  (define z1 (add x x))
  (sub x (another-add y y))
  (define z2 (another-add x x))
  (mul x y)
  (div x y))

(main 2 3)
(sub 12 2)
