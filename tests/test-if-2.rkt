#lang magnolisp

(typedef int (#:annos foreign))

(function (holds? x)
  (#:annos (type (fn int Bool)) foreign)
  #t)

(function (f x)
  (#:annos export (type (fn int int)))
  (do (if (holds? x)
          (return x)
          (void))
      (return 6)))

(f 5)
