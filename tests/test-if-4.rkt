#lang magnolisp
(require (only-in racket/base not =))

(typedef int #:: (foreign))

(function (holds? x)
  #:: ((type (-> int Bool)) foreign)
  (not (= x 0)))

(function (f x)
  #:: (export (type (-> int int)))
  (begin-return
    (if (holds? x)
        (if (if #t #f #t)
            (return 7)
            (if #f (return 8) (return (if #t 9 10))))
        (return 1))))

(f 5) ;; => 9
(f 0) ;; => 1

