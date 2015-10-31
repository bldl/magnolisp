#lang magnolisp/2014

(typedef int (#:annos foreign))

(function (holds? x)
  (#:annos (type (fn int Bool)) foreign)
  #t)

(function (f x)
  (#:annos export (type (fn int int)))
  (if (holds? x)
      (if (holds? x) 1 2)
      (begin (void) 3)))

(f 5)
