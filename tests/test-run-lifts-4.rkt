#lang magnolisp
(require (only-in racket/base))
(require "lib-cxx-runner.rkt")

(function (main3 x) #:: (export [type (-> int int)])
  (define y 1)
  (unless (zero? x)
    (set! y x)) ;; not safe to remove since in branch
  (set! y y) ;; copy propagation should propagate `y`
  y)

(function (main4 x) #:: (export [type (-> int int)])
  (define y 1)
  (set! y y) ;; copy propagation should propagate `y`
  y)

(function (run)
  #:: (export [type (-> Void)] [expected 1 7 1])
  (put-int (main3 0))
  (put-int (main3 7))
  (put-int (main4 2)))

(run)
