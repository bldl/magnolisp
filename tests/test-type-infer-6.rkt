#lang magnolisp

(typedef int #:: (foreign))

(function (seven-of x) #:: ([type (exists T (-> T T))])
  7)

(function (f) #:: (export)
  (seven-of (cast int 0)))

(f)
