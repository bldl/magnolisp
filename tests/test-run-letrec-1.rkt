#lang magnolisp
(require "lib-cxx-runner.rkt")

(function (f x)
  #:: ((type (-> int int)))
  (var y (inc x)) ;; y = 9
  (put-int y)
  (set! y (inc y)) ;; y = 10
  (put-int y)
  (var z y)
  (set! z (inc y)) ;; z = 11
  (put-int z)
  z)

(function (run)
  #:: (export [type (-> Void)] [expected 9 10 11])
  (f 8)
  (void))
