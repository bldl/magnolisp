#lang magnolisp

(require (prefix-in r. racket/base)) 

(provide int put-int putting-int inc)

(typedef int #:: (foreign))

(function (put-int x) 
  #:: (foreign [type (-> int Void)])
  (r.displayln x))

(function (putting-int x) 
  #:: (foreign [type (-> int int)])
  (r.displayln x)
  x)

(function (inc x)
  #:: ((type (-> int int)) foreign)
  (r.add1 x))
