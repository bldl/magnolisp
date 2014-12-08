#lang magnolisp/2014

(typedef long #an(foreign))

(function (h) #an(export)
  (cast long 6))

(h)

(function (g x) #an(export)
  (cast long x))

(g 7)

(function (f x) #an(export)
  (cast long (let ((x x)) x)))

(f 8)

(function (ff x) #an(export)
  (let ((x x)) (cast long x)))

(ff 9)
