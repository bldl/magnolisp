#lang magnolisp
(require "lib-cxx-runner.rkt"
         (only-in racket/base
                  [box-immutable rkt.box]
                  [unbox rkt.unbox]))

(typedef Box #:: (foreign))
(typedef BoolBox (<> Box Bool))

(define box #:: 
  (foreign [type (for-all E (-> E (<> Box E)))])
  rkt.box)

(define unbox #::
  (foreign [type (for-all E (-> (<> Box E) E))])
  rkt.unbox)

(define (my-main x) #:: (export)
  (define bx #:: ([type BoolBox]) (box x))
  (unbox bx))
