#lang magnolisp

(typedef int #:: (foreign))
(typedef long #:: (foreign))

(function (seven w) #:: ([type (exists T U (-> T U))])
  7)

(function (f x) #:: (export [type (exists T (-> int T))])
  (var y #:: ([type (exists T T)]) x)
  (var z #:: ([type (exists V W int)]) 8)
  (cast long (seven y)))

(f 9)
