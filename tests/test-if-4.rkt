#lang magnolisp/2014

(typedef int (#:annos foreign))

(function (holds? x)
  (#:annos (type (fn int Bool)) foreign)
  (not (= x 0)))

(function (f x)
  (#:annos export (type (fn int int)))
  (do
    (if (holds? x)
        (if (if #t #f #t)
            (return 7)
            (if #f (return 8) (return (if #t 9 10))))
        (return 1))))

(f 5) ;; => 9
(f 0) ;; => 1

