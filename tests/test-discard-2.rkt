#lang magnolisp/2014

(typedef int (#:annos foreign))

(function (holds? x) (#:annos foreign [type (fn int Bool)])
  (> x 0))

(function (f x y)
  (#:annos export (type (fn int int int)))
  (let ()
    (if (holds? x)
        (when (holds? y) (cast int 7))
        (unless (holds? y) (cast int 8)))
    x))

(f 5 6)
