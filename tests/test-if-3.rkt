#lang magnolisp
(require (only-in racket/base))

(typedef int #:: (foreign))

(function (holds? x)
  #:: ((type (-> int Bool)) foreign)
  #f)

(function (f x)
  #:: (export (type (-> int int)))
  (let ((res 0))
    (when (holds? x)
      (set! res 1))
    (unless (holds? x)
      (set! res 2))
    (when (holds? x)
      (void)
      (set! res 3))
    (unless (holds? x)
      (void)
      (set! res 4))
    (set! res 5)
    res))

(f 5)
