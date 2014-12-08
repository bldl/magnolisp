#lang magnolisp/2014

(typedef int (#:annos foreign))

(function (one)
  (#:annos foreign (type (fn int)))
  1)

(function (add x y)
  (#:annos foreign (type (fn int int int)))
  (+ x y))

(function (f x)
  (#:annos export (type (fn int int)))
  (do
    (void)
    (begin
      (void)
      (return x)
      (void))
    (void)))

(f 1)

