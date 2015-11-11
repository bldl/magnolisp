#lang magnolisp
(require (only-in racket/base cond))
(require "lib-cxx-runner.rkt")

(typedef float #:: (foreign))
(typedef double #:: (foreign))

(function (holds? x) #:: ([type (-> int Bool)])
  (non-zero? x))

(function (f x)
  #:: (export (type (-> int int)))
  (let ()
    (cond
      ((holds? 1) (cast int 1))
      ((holds? 2) (cast long 2))
      ((holds? 3) (cast float 3))
      ((holds? 4) (cast double 4)))
    (if (holds? x) 1 3)))

(f 5) ;; => 1

(function (run)
  #:: (export [type (-> Void)] [expected 1])
  (put-int (f 5)))
