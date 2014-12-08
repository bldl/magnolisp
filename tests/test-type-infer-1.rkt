#lang magnolisp/2014

(typedef long #an(foreign))

(function (h) #an(export)
  (cast long 5))

(function (g) #an(export)
  (let-var x (h) x))

(h)
(g)
