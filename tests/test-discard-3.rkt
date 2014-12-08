#lang magnolisp/2014

(typedef int (#:annos foreign))
(typedef long (#:annos foreign))
(typedef float (#:annos foreign))
(typedef double (#:annos foreign))

(function (holds? x) (#:annos foreign [type (fn int Bool)])
  (> x 0))

(function (f x)
  (#:annos export (type (fn int int)))
  (let ()
    (cond
     ((holds? 1) (cast int 1))
     ((holds? 2) (cast long 2))
     ((holds? 3) (cast float 3))
     ((holds? 4) (cast double 4)))
    (if (holds? x) 1 3)))

(f 5) ;; => 1

