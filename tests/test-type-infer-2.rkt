#lang magnolisp

(typedef long #an(foreign))

(function (h) #an(export)
  (cast long 5))

(h)

(function (g x) #an(export)
  (cast long x))

(g 7)

(function (f x) #an(export)
  (cast long (let ((x x)) x)))

(f 8)
