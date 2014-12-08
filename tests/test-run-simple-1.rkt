#lang magnolisp/2014
(require "lib-cxx-runner.rkt")

(function (run)
  (#:annos export [type (fn Void)] [expected 1 2])
  (put-int 1)
  (put-int 2)
  (void))
