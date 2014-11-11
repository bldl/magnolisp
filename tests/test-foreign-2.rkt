#lang magnolisp

(typedef int (#:annos foreign))

(begin-for-racket
 (define (five)
   5)
 (define (inc x)
   (begin 1 2 3 (+ x 1))))

(define-for-racket cmp >)

(define-for-racket (inc2 x)
  (add1 x))

(function (holds? x) ;; whether x > 5
  (#:annos (type (fn int bool)) foreign)
  (begin-racket 1 (begin 2 (cmp (inc2 x) (inc (five))))))

(function (f x)
  (#:annos export (type (fn int int)))
  (do
    (begin-racket 4 5 6)
    (when (holds? x)
      (return 1))
    (return 2)))

(f 5)
(f 6)
