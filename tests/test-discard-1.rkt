#lang magnolisp

(typedef int (#:annos foreign))

(function (f x)
  (#:annos export (type (fn int int)))
  1 x 2 3)

(f 5)
