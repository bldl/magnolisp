#lang magnolisp/2014

(typedef int (#:annos foreign))

(function (equal? x y)
  (#:annos (type (fn int int Bool)) foreign)
  (begin-racket
   (local-require (only-in racket/base equal?))
   (equal? x y)))

(function (f x y) (#:annos export (type (fn int int Bool)))
  (equal? x y))

(f 1 1)
(f 1 2)
