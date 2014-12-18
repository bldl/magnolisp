#lang magnolisp

(typedef int #:: (foreign))

(function (forever x) #:: ([type (exists T (-> T T))])
  (forever x))

(function (use-forever) #:: (export)
  (forever (cast int 7)))

(function (c?)
  #:: ([type (-> Bool)] foreign))

(function (f) #:: (export)
  (if (c?)
      (cast int 8)
      (f)))
