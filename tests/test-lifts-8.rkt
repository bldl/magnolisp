#lang magnolisp
(require (only-in racket/base =))

(typedef int #:: (foreign))

(function (holds? x) #:: (foreign [type (-> int Bool)])
  (= x 1))

(function (f) #:: ([type (-> Void)])
  (void))

(function (main3 x y) #:: (export [type (-> int int int)])
  (if (holds? (begin (f) x)) x y))

(main3 1 8)
(main3 7 8)

