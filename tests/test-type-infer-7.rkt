#lang magnolisp

(typedef long #:: (foreign))

(function (id x) #:: (foreign [type (exists T (-> T T))])
  x)

(function (f) #:: (export)
  (id (cast long 0)))

(f)
