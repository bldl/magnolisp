#lang magnolisp
(require (only-in racket/base equal?))

(typedef int #:: (foreign))

(function (equal x y)
  #:: ((type (-> int int Bool)) foreign)
  (equal? x y))

(function (compute x)
  x)

(define-syntax-rule
  (default e f d)
  (let ((ret e))
    (if (equal ret f)
        d
        ret)))

(function (f x) #:: (export)
  (default (compute x) 0 (compute 42)))

(default 1 1 7)
(default 2 1 7)
