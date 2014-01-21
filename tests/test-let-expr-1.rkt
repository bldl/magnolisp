#lang magnolisp

(typedef int (#:annos foreign))

(function (f x)
  (#:annos export (type (fn int int)))
  (let-var y #an(^int) x y))

(f 7)

(function (cxx-f x)
  (#:annos foreign (type (fn int int)))
  (+ x 1))

(function (g x)
  (#:annos export (type (fn int int)))
  (let-var y #an(^int) x (cxx-f y)))

(g 7)

(function (h x)
  (#:annos export (type (fn int int)))
  (let-var x #an(^int) x (cxx-f x)))

(h 8)
