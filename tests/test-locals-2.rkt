#lang magnolisp

(typedef int #:: (foreign))

(function (f x)
  #:: (export (type (-> int int)))
  (begin-return
    (function (g x) #:: (^(-> int int)) x)
    (return
     (begin-return
       (function (g x) #:: (^(-> int int)) x)
       (return (g x))))))

(f 1)
