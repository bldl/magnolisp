#lang magnolisp/2014

(typedef int #an(foreign))
(typedef long #an(foreign))

(function (h) #an([export also_f] ^(fn long))
  #ap(^long) 5)

(function (g) #an([export also_f])
  (let-var x (h) x))

(g)
