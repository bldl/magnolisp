#lang magnolisp

(typedef int #:: (foreign))

(function (f x)
  #:: (export (type (-> int int)))
  (let ()
    (function (g x) #:: (^(-> int int)) x)
    (let ()
      (function (g x) #:: (^(-> int int)) x)
      (g x))))

(f 1)
