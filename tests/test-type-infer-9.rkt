#lang magnolisp

(typedef int #:: (foreign))
(typedef long #:: (foreign))

(function (id x) #:: ([type (-> int int)])
  x)

(function (f x) #:: (export)
  (var y #:: ([type (exists T T)]) 0) ;; pointless annotation
  (set! y x)
  (id y))

(f 9)
