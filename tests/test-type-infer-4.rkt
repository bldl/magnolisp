#lang magnolisp/2014

(typedef int (#:annos foreign))

(define (g x) x)

(function (f)
  (#:annos export (type (fn int)))
  (g 1))

(f)
