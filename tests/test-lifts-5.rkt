#lang magnolisp
(require (only-in racket/base + =))

(typedef int #:: (foreign))

(function (add x y) #:: (foreign [type (-> int int int)])
  (+ x y))

(function (holds? x) #:: (foreign [type (-> int Bool)])
  (= x 1))

(function (main3 x) #:: (export [type (-> int int)])
  (let ((y x)) ;; both `y` and `z` should copy propagate
    (let ((z y))
      (if (holds? z) z y))))

(main3 1) ;; => 1
(main3 7) ;; => 7
