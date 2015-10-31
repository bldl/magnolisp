#lang magnolisp/2014

(typedef int #an(foreign))

(function (holds? x) (#:annos foreign [type (fn int Bool)])
  (= x 0))

(function (main x) #an(export)
  (if (holds? x)
      5
      (cast int 6)))

(main 0) ;; => 5
(main 7) ;; => 6
