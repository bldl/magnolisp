#lang magnolisp/2014
(require "lib-cxx-runner.rkt")

(function (f x)
  (#:annos (type (fn int int)))
  (do
    (var y (inc x)) ;; y = 9
    (put-int y)
    (set! y (inc y)) ;; y = 10
    (put-int y)
    (var z y)
    (set! z (inc y)) ;; z = 11
    (put-int z)
    (return z)))

(function (run)
  (#:annos export [type (fn Void)] [expected 9 10 11])
  (f 8)
  (void))
