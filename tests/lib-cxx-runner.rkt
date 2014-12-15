#lang magnolisp

(require (prefix-in r. racket/base)) 

(provide int long put-int put-long putting-int inc non-zero? ->long)

(typedef int #:: (foreign))
(typedef long #:: (foreign))

(function (put-int x) 
  #:: (foreign [type (-> int Void)])
  (r.displayln x))

(function (put-long x) 
  #:: (foreign [type (-> long Void)])
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

(function (->long x)
  #:: ([type (-> int long)] foreign)
  x)
