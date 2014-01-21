#lang magnolisp

(typedef int #an(foreign))
(typedef long #an(foreign))

(function (h) #an(export)
  (lit-of long 5))

(function (g) #an(export)
  (let-var x (h) x))

(h)
(g)
