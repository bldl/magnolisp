#lang magnolisp/2014

(define (eight) 8)

(define-syntax-rule (eight-m)
  (eight))
(provide eight-m)
