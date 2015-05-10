#lang magnolisp

(typedef int #:: (foreign))
(typedef stack #:: (foreign))

(define (stack-id x) #:: ([type (-> (<> stack int) (auto))])
  x)

(function (main x) #:: (export)
  (stack-id x))

