#lang magnolisp

(typedef int (#:annos foreign))

(function (holds? x)
  (#:annos (type (fn int predicate)) foreign)
  #f)

(function (f x)
  (#:annos export (type (fn int predicate)))
  (do
    (var y (holds? x))
    (let ((x #t))
      (set! y x))
    (return y)
    (set! y #f)))

(f 5)
