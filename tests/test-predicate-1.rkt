#lang magnolisp/2014

(function (f)
  (#:annos export (type (fn Bool)))
  #t)

(f)

(function (g)
  (#:annos export (type (fn Bool)))
  (do
    (var r #f)
    (return r)))

(g)

(function (h)
  (#:annos export (type (fn Bool)))
  (do
    (var r #f)
    (var x r)
    (return x)))

(h)
