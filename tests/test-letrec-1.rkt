#lang magnolisp

(typedef int (#:annos foreign))

(function (inc x)
  (#:annos (type (fn int int)) foreign)
  (add1 x))

(function (f x)
  (#:annos (type (fn int int)) export)
  (do
    (var y (inc x)) ;; y = 9
    (set! y (inc y)) ;; y = 10
    (var z y)
    (set! z (inc y)) ;; z = 11
    (return z)))

(f 8)
