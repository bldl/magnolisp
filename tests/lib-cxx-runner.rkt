#lang magnolisp/2014

(provide int put-int inc)

(typedef int (#:annos foreign))

(function (put-int x) 
  (#:annos foreign [type (fn int int)])
  (displayln x)
  x)

(function (inc x)
  (#:annos (type (fn int int)) foreign)
  (add1 x))
