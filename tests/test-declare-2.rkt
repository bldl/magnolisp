#lang magnolisp

(require "lib-declare-1.rkt")

(declare #:type int #:: (foreign))

(declare (add1 x) #:: (foreign ^(-> int int)))

(define (main x) #:: (export)
  (add1 (add1 x)))

(main 7)
