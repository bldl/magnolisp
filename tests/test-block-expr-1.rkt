#lang magnolisp

(typedef int #an(foreign))
(typedef long #an(foreign))

(function (g) #an(^(fn int))
  (id 555))

(function (f) #an(export)
  (g))

(function (id x)
  x)

(function (h) #an(export)
  (do (void) (return (f))))
