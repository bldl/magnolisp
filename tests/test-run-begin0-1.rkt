#lang magnolisp/2014
(require "lib-cxx-runner.rkt")

(function (main1 x)
  (#:annos export (type (fn int int)))
  (begin0 1 2 3 x))

(function (main2 x)
  (#:annos export (type (fn int int)))
  (begin0 2 (main1 x) 3))

(function (main3 x)
  (#:annos export (type (fn int int)))
  (begin0 (main1 (main2 x)) (main1 4)))

(function (main4 x)
  (#:annos export (type (fn int int)))
  (begin0 (begin0 1 2) (begin0 x 4)))

(function (main5 x)
  (#:annos export (type (fn int int)))
  (begin0 (begin0 1 (main3 2)) (begin0 (main4 x) 4)))

(function (run)
  (#:annos export [type (fn Void)] [expected 1 2 1 1 1])
  (put-int (main1 5))
  (put-int (main2 5))
  (put-int (main3 5))
  (put-int (main4 5))
  (put-int (main5 5)))

(run)

