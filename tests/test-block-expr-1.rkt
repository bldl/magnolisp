#lang magnolisp

(typedef int #:: (foreign))
(typedef long #:: (foreign))

(function (g) #:: (^(-> int))
  (id 555))

(function (f) #:: (export)
  (g))

(function (id x)
  x)

(function (h-1) #:: (export)
  (begin-return 
    (void) 
    (return (f))))

(function (h-2) #:: (export)
  (begin-return
   (var v (f)) 
   (return v)))
