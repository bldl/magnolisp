#lang magnolisp/2014

(typedef int (#:annos foreign))

(function (holds? x)
  (#:annos (type (fn int Bool)) foreign)
  #f)

(function (f x)
  (#:annos export (type (fn int Bool)))
  (do
    (var y (holds? x))
    (let ((x #t))
      (set! y x))
    (return y)
    (set! y #f)))

(f 5)
