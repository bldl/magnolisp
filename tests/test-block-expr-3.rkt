#lang magnolisp/2014

(typedef int #an(foreign))

(function (holds? x) (#:annos foreign [type (fn int Bool)])
  (= x 0))

(function (main x) #an(export)
  (do 
    (when (holds? x)
      (return 5))
    (cast int 6)))

(main 0) ;; => 5
(main 7) ;; => 6

(function (identity x) (#:annos export [type (fn int int)])
  (do x))

(identity 7) ;; => 7

