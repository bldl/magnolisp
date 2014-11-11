#lang magnolisp

(typedef int (#:annos foreign))

(function (add x y) (#:annos foreign [type (fn int int int)])
  (+ x y))

(function (holds? x) (#:annos foreign [type (fn int bool)])
  (= x 1))

(function (main3 x) (#:annos export [type (fn int int)])
  (do
    (var y 1)
    (when (holds? x)
      (set! y x)) ;; not safe to remove since in branch
    (set! y y) ;; copy propagation should propagate `y`
    (return y)))

(main3 1) ;; => 1
(main3 7) ;; => 7
