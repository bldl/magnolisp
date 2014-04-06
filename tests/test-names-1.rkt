#lang magnolisp

(typedef int (#:annos foreign))

(begin-for-racket
 (require (only-in racket/base [equal? r.equal?])))

(function (equal? x y)
  (#:annos (type (fn int int predicate)) foreign)
  (begin-racket (r.equal? x y)))

(function (f x y) (#:annos export (type (fn int int predicate)))
  (equal? x y))

(f 1 1)
(f 1 2)
