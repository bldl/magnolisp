#lang magnolisp

(typedef int (#:annos foreign))

(function (holds? x)
  (#:annos (type (fn int predicate)) foreign)
  #f)

(function (f x)
  (#:annos export (type (fn int predicate)))
  (do
    (var y (holds? x))
    (let ((x true))
      (set! y x))
    (return y)
    (set! y false)))

(f 5)
