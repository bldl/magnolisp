#lang magnolisp

(require (prefix-in r. racket/base)) 

(provide int put-int putting-int inc non-zero?)

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

(function (non-zero? x) 
  #:: ([type (-> int Bool)] foreign)
  (r.not (r.= x 0)))
