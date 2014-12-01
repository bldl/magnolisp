#lang magnolisp

(typedef int (#:annos foreign))

(function (add x y) (#:annos foreign [type (fn int int int)])
  (+ x y))

(function (holds? x) (#:annos foreign [type (fn int Bool)])
  (= x 1))

(function (main3 x) (#:annos export [type (fn int int)])
  (do
    (var y 1)
    ;; copy propagation cannot deal with this since these assignments get a different value number
    (if (holds? x)
        (set! y x)
        (set! y x))
    (return y)))

(main3 7) ;; => 7
