#lang magnolisp

(require "lib-declare-1.rkt")

(declare int 
  (let-annotate (foreign) 
    (abstract-type)))

(declare add1
  (let-annotate (foreign ^(-> int int))
    (lambda (x) (void))))

(define (main x) #:: (export)
  (add1 (add1 x)))

(main 7)
