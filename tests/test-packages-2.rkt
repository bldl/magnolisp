#lang magnolisp/2014

#|

A simple test of Racket's "packages".

|#

(require compatibility/package)

(typedef int #an(foreign))

(define-package p1 (f)
  (function (f) #an(^(fn int)) 5))

(function (g) #an(export)
  (open-package p1)
  (f))

(g)
