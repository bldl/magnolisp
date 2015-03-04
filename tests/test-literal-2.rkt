#lang magnolisp

(typedef Symbol #:: (foreign))

(define (symbol-use! s) 
  #:: (foreign ^(-> Symbol Void)))

(define (symbol-identity s)
  #:: (^(-> Symbol Symbol))
  s)
  
(define (main)
  #:: (export)
  (define s1 'symbol)
  (symbol-use! s1)
  (define s2 `another)
  (symbol-use! s2)
  (define s3 `,'one-more)
  (symbol-use! s3)
  (symbol-identity s3))

(main)
