#lang magnolisp
(define (hello) #:: ([build [+= srcs "Hello"]]))
(define (world) #:: ([build [+= srcs "World"]]))
(define (main) #:: (export ^(-> Void))
  (hello) (world))
