#lang magnolisp
(require (only-in racket/base))

(typedef int #:: (foreign))

(function (holds? x)
  #:: ([type (-> int Bool)] foreign)
  #t)

(function (f x)
  #:: (export [type (-> int int)])
  (if (holds? x) x 6))

(f 5)
