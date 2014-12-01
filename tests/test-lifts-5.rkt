#lang magnolisp

(typedef int (#:annos foreign))

(function (add x y) (#:annos foreign [type (fn int int int)])
  (+ x y))

(function (holds? x) (#:annos foreign [type (fn int Bool)])
  (= x 1))

(function (main3 x) (#:annos export [type (fn int int)])
  (let ((y x)) ;; both `y` and `z` should copy propagate
    (let ((z y))
      (if (holds? z) z y))))

(main3 1) ;; => 1
(main3 7) ;; => 7
