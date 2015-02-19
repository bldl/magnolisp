#lang magnolisp

(typedef int #:: (foreign))

(define (holds? x) 
  #:: ((type (-> int Bool)) foreign)
  #f)

(define (f x)
  #:: (export (type (-> int Bool)))
  (begin-return
    (define y (holds? x))
    (let ((x #t))
      (set! y x))
    (return y)
    (set! y #f)))

(f 5)
