#lang magnolisp/2014

(typedef int (#:annos foreign))

(begin-racket 1 2 3 (void))

(function (holds? x)
  (#:annos [type (fn int Bool)] foreign)
  (begin-racket (begin 1 #f)))

(function (f x)
  (#:annos export [type (fn int int)])
  (begin-racket 4 5 6)
  (if (holds? x) 1 2))

(f 5)
