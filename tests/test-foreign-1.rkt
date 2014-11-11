#lang magnolisp

(typedef int (#:annos foreign))

(begin-racket 1 2 3 (void))

(function (holds? x)
  (#:annos (type (fn int bool)) foreign)
  (begin-racket (begin 1 #f)))

(function (f x)
  (#:annos export (type (fn int int)))
  (do
    (begin-racket 4 5 6)
    (when (holds? x)
      (return 1))
    (return 2)))

(f 5)
