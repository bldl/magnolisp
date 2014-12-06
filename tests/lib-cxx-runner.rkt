#lang magnolisp

(provide int put-int inc)

(typedef int (#:annos foreign))

(function (put-int x) 
  (#:annos foreign [type (fn int Void)])
  (displayln x))

(function (inc x)
  (#:annos (type (fn int int)) foreign)
  (add1 x))
