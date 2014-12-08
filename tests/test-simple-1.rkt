#lang magnolisp/2014

(require magnolisp/core)

(define int #ap(foreign) (if #f (#%magnolisp 'foreign-type) #f))

(define f
  #ap(^(fn int int) (export public_f))
  (lambda (an-x)
    an-x))

(f 1)
