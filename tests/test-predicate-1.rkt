#lang magnolisp

(function (f)
  (#:annos export (type (fn bool)))
  #t)

(f)

(function (g)
  (#:annos export (type (fn bool)))
  (do
    (var r #f)
    (return r)))

(g)

(function (h)
  (#:annos export (type (fn bool)))
  (do
    (var r #f)
    (var x r)
    (return x)))

(h)
