#lang magnolisp

(typedef int (#:annos foreign))

(function (main1 x)
  (#:annos export (type (fn int int)))
  (begin0 1 2 3 x))

(main1 5) ;; => 1

(function (main2 x)
  (#:annos export (type (fn int int)))
  (begin0 2 (main1 x) 3))

(main2 5) ;; => 2

(function (main3 x)
  (#:annos export (type (fn int int)))
  (begin0 (main1 (main2 x)) (main1 4)))

(main3 5) ;; => 1

(function (main4 x)
  (#:annos export (type (fn int int)))
  (begin0 (begin0 1 2) (begin0 x 4)))

(main4 5) ;; => 1

(function (main5 x)
  (#:annos export (type (fn int int)))
  (begin0 (begin0 1 (main3 2)) (begin0 (main4 x) 4)))

(main5 5) ;; => 1
