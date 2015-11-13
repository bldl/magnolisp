#lang magnolisp
(require "lib-cxx-runner.rkt")

(define (f1 x) #:: (export [type (-> int int)])
  (define (g) x)
  (g))

(define (f2 x) #:: (export [type (-> int int)])
  (define (g) x)
  (define (h) (g))
  (h))

(define (f3 x) #:: (export [type (-> int int)]) ;; non-terminating
  (define (g) (g) x)
  (g))

(define (f4 x) #:: (export [type (-> int int)]) ;; non-terminating
  (define (g) (h) x)
  (define (h) (g) x)
  (h))

(define (f5) #:: (export [type (-> int)])
  (let ((x 5))
    (define (g)
      (define (h)
        x)
      (h))
    (g)))

(f5)

#;
(define (f6) #:: (export [type (-> int)])
  (let ((x 5))
    (define (g)
      (set! x 6) ;; not allowed
      x)
    (g)))

(define (f7) #:: (export [type (-> int)])
  (define (g x)
    (let ((y 7))
      (define (add7 x)
        (int-add y x))
      (add7 x)))
  (define (call-g)
    (g 2))
  (call-g))

(f7)
