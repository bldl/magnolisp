#lang magnolisp
(require "lib-cxx-runner.rkt")

(define (main x) #:: (export [type (-> int int)])
  (define (identity x) #:: ([export identity])
    x)
  (define (seven) #:: (export)
    7)
  (identity (int-add (seven) x)))

(main 7)
