#lang magnolisp
(require (only-in racket/base))

(typedef int #:: (foreign))

(function (f x)
  #:: (export (type (-> int int)))
  (define (local-g x) x)
  (local-g x))

(f 1)
