#lang magnolisp
(require (only-in racket/base))

(typedef int #:: (foreign))

(define (holds? x)
  #:: ([type (-> int Bool)] foreign)
  #t)

(define (f x)
  #:: (export [type (-> int int)])
  (if (holds? x)
      (if (holds? x) 1 2)
      (begin (void) 3)))

(f 5)
