#lang magnolisp/2014

(define (f) 7)

(provide (rename-out [f seven]))

(define (g) 8)

(provide g)
