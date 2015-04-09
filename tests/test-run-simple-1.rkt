#lang magnolisp
(require "lib-cxx-runner.rkt")

(function (run)
  #:: (export [type (-> Void)] [expected 1 2])
  (put-int 1)
  (put-int 2))
