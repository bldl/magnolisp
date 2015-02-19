#lang magnolisp

(typedef int #:: (foreign))
(typedef long #:: (foreign))

(define (g) #:: (^(-> int))
  (id 555))

(define (f) #:: (export)
  (g))

(define (id x)
  x)

(define (h-1) #:: (export)
  (begin-return 
    (void) 
    (return (f))))

(define (h-2) #:: (export)
  (begin-return
   (define v (f)) 
   (return v)))
