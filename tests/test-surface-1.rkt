#lang magnolisp

(typedef int #:: (foreign))

(function (run x) #:: (export)
  (var y x)
  (var z #:: (^int) y)
  z)
