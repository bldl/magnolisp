#lang magnolisp/2014

(typedef int (#:annos foreign))

(function (equal x y)
  (#:annos (type (fn int int Bool)) foreign)
  (begin-racket (equal? x y)))

(function (default e f d) #an([export dflt])
  (if (equal e f) d e))

(function (f x)
  (#:annos export)
  (define y (default x 2 7))
  (default x 1 7))

(f 1)
(f 2)

(provide (rename-out [f g]))
