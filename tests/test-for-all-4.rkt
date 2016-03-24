#lang magnolisp
(require "lib-cxx-runner.rkt")

(typedef Box #:: (foreign))

(define (box v) #:: 
  (foreign [type (for-all E (-> E (<> Box E)))]))

(define (unbox box) #::
  (foreign [type (for-all E (-> (<> Box E) E))]))

(define (my-main x) #:: ([type (-> int long)])
  (define boxed-int (box x))
  (define boxed-long (box (->long (unbox boxed-int)))) 
  (unbox boxed-long))

(define (run) #:: (export [type (-> Void)])
  (put-long (my-main 7)))
