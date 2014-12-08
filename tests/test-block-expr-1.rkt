#lang magnolisp

(typedef int #an(foreign))
(typedef long #an(foreign))

(function (g) #an(^(-> int))
  (id 555))

(function (f) #an(export)
  (g))

(function (id x)
  x)

(function (h-1) #an(export)
  (begin-return 
    (void) 
    (return (f))))

(function (h-2) #an(export)
  (begin-return
   (var v (f)) 
   (return v)))
