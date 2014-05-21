#lang magnolisp

(define int #ap(foreign) (#%magnolisp 'foreign-type))

(define f
  #ap(^(fn int int) (export public_f))
  (lambda (an-x)
    an-x))

(f 1)
