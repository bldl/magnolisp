#lang magnolisp

(typedef int (#:annos foreign))

(function (holds? x)
  (#:annos (type (fn int predicate)) foreign)
  #t)

(function (f x)
  (#:annos export (type (fn int int)))
  (do (if (holds? x)
          (return (if (holds? x) 1 2))
          (begin (void) (return 3)))))

(f 5)