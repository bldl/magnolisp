#lang magnolisp/2014

#|

A local macro.

|#

(typedef int (#:annos foreign))

(function (holds? x)
  (#:annos (type (fn int Bool)) foreign)
  (= x 1))

(function (f x) #an(export ^(fn int int))
  (let-syntax ([if-not
                (syntax-rules ()
                  [(_ c t e)
                   (if c e t)])])
    (if-not (holds? x) 1 2)))

(f 1)
(f 2)
