#lang magnolisp
(require "lib-cxx-runner.rkt")

(function (run)
  #:: (export [type (-> Void)] [expected 1])
  (put-int (if-cxx (let () 1) (begin0 2 3))))

(run) ;; prints 2
