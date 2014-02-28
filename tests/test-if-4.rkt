#lang magnolisp

(typedef int (#:annos foreign))

(function (holds? x)
  (#:annos (type (fn int predicate)) foreign)
  #t)

(function (f x)
  (#:annos export (type (fn int int)))
  (do
    (if (holds? x)
        (if (if true false true)
            (return 7)
            (if false (return 8) (return (if true 9 10))))
        (return 1))))

(f 5)
