#lang magnolisp

(require "lib-surface-provide.rkt" (prefix-in r. racket/base)) 

;;; 
;;; integers
;;; 

(typedef* int #:: (foreign))
(typedef* long #:: (foreign))

(function* (put-int x) 
  #:: (foreign [type (-> int Void)])
  (r.displayln x))

(function* (put-long x) 
  #:: (foreign [type (-> long Void)])
  (r.displayln x))

(function* (putting-int x) 
  #:: (foreign [type (-> int int)])
  (r.displayln x)
  x)

(function* (inc x)
  #:: ((type (-> int int)) foreign)
  (r.add1 x))

(function* (zero? x) 
  #:: ([type (-> int Bool)] foreign)
  (r.= x 0))

(function* (non-zero? x) 
  #:: ([type (-> int Bool)] foreign)
  (r.not (r.= x 0)))

(function* (->long x)
  #:: ([type (-> int long)] foreign)
  x)

(function* (int-add x y)
  #:: ([type (-> int int int)] foreign)
  (r.+ x y))

;;; 
;;; lists
;;; 

(typedef* IntList #:: (foreign))

(function* (IntList-empty? lst)
  #:: ([type (-> IntList Bool)] foreign)
  (r.null? lst))

(function* (IntList-head lst)
  #:: ([type (-> IntList int)] foreign)
  (r.car lst))

(function* (IntList-tail lst)
  #:: ([type (-> IntList IntList)] foreign)
  (r.cdr lst))

(function* (IntList-push-back lst x)
  #:: ([type (-> IntList int IntList)] foreign)
  (r.append lst (r.list x)))

(function* (IntList-new)
  #:: ([type (-> IntList)] foreign)
  r.null)
