#lang magnolisp

(require (only-in racket/base + - * /))

(define (int-f) #:: (export)
  (typedef int #:: (foreign))
  (cast int 1))

(define (long-f x) #:: (export)
  (typedef long #:: (foreign))
  (define add #:: (foreign ^(-> long long long)) +)
  (add x x))

(define (double-f x) #:: (export)
  (typedef double #:: (foreign))
  (define mul #:: (foreign) *)
  (cast double (mul x (cast double x))))
