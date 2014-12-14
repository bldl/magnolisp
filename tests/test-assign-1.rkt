#lang magnolisp

(typedef int #:: (foreign))

(function (holds? x)
  #:: ((type (-> int Bool)) foreign)
  #f)

(function (f x)
  #:: (export (type (-> int Bool)))
  (begin-return
    (var y (holds? x))
    (let ((x #t))
      (set! y x))
    (return y)
    (set! y #f)))

(f 5)
