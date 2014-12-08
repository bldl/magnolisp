#lang magnolisp/2014

(typedef int #an(foreign))
(typedef long #an(foreign))

(function (g) #an(^(fn int))
  (id 555))

(function (f) #an(export)
  (g))

(function (id x)
  x)

(function (h-1) #an(export)
  (do (void) (return (f))))

(function (h-2) #an(export)
  (do (var v (f)) (return v)))

