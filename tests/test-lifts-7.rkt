#lang magnolisp/2014

(typedef int (#:annos foreign))

(function (holds? x) (#:annos foreign [type (fn int Bool)])
  (= x 1))

(function (f) (#:annos foreign [type (fn Void)])
  (void))

(function (main3 x) (#:annos export [type (fn int Void)])
  (if (holds? x) (void) (f)))

(main3 7)

