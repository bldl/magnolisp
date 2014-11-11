#lang magnolisp

(require (rename-in "lib-names-3.rkt" [g eight]))

(typedef int (#:annos foreign))

(function (equal? x y)
  (#:annos (type (fn int int bool)) foreign)
  (begin-racket
   (local-require (only-in racket/base equal?))
   (equal? x y)))

(define (g)
  (let ((f 5)) f))

(function (f) #an(export)
  (do
    (define (f) (let ((f 5)) (equal? f (g))))
    (define five=seven (do
                         (define (f) (g))
                         (return (equal? (f) (seven)))))
    (define seven=eight (equal? (seven) (eight)))
    (define five (let ((x (g))) (let ((x x)) (let ((x x)) x))))
    (return (f))))

(f)
