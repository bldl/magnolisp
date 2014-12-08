#lang magnolisp/2014

(typedef Int (#:annos foreign))
(provide Int)

(function (six) #an(^(fn Int))
  6)
(provide six)

(define (eight) 8)

(function (sub-prim x y) #an(foreign ^(fn Int Int Int))
  (- x y))

;; Note that this expands to a use of something that is not exported.
(define-syntax-rule (seven)
  (sub-prim (eight) 1))
(provide seven)

(define (nine) 9)
(provide nine)
