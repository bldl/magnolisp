#lang magnolisp
(require (only-in racket/base + =))
(require "lib-cxx-runner.rkt")

(define (main3 x) #:: (export [type (-> int int)])
  (define y 1)
  ;; copy propagation cannot deal with this since these assignments get a different value number
  (if (non-zero? x)
      (set! y x)
      (set! y x))
  y)

(main3 7) ;; => 7

(function (run)
  #:: (export [type (-> Void)] [expected 7])
  (put-int (main3 7)))

(run)
