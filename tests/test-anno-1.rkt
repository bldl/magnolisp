#lang magnolisp

(typedef int #an(foreign))
(typedef long #an(foreign))

(function (h)
  5)

(function (g)
  (let-var x (h) x))

(g)

(anno! g export)
(anno! h ^(fn long))
(anno! h export)
