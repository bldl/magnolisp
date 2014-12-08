#lang magnolisp/2014

(typedef int (#:annos foreign))

(function (one)
  (#:annos foreign (type (fn int)))
  1)

(function (inc x)
  (#:annos foreign (type (fn int int)))
  (+ x 1))

(function (f x)
  (#:annos export (type (fn int int)))
  (do
    (var x #an(^int) (let-var y #an(^int) (one) (inc y)))
    (return x)))

(f 1)

