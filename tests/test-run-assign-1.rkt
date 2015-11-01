#lang magnolisp

(require "lib-cxx-runner.rkt")

(define (f x)
  #:: (export (type (-> int Bool)))
  (define y (non-zero? x))
  (let ((x #t))
    (set! y x))
  (begin0
      y
    (set! y #f)))

(f 5) ;; => #t

(function (run)
  #:: (export [type (-> Void)] [expected 1])
  (put-int (if (f 7) 1 2)))

(run)
