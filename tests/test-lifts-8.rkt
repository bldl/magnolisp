#lang magnolisp

(typedef int (#:annos foreign))

(function (holds? x) (#:annos foreign [type (fn int Bool)])
  (= x 1))

(function (f) (#:annos [type (fn Void)])
  (void))

(function (main3 x y) (#:annos export [type (fn int int int)])
  (if (holds? (begin (f) x)) x y))

(main3 1 8)
(main3 7 8)

