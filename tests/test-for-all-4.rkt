#lang magnolisp
(require "lib-cxx-runner.rkt")

(typedef Box #:: (foreign))

(function (box v) #:: 
  (foreign [type (for-all E (-> E (<> Box E)))]))

(function (unbox box) #::
  (foreign [type (for-all E (-> (<> Box E) E))]))

(function (main x) #:: (export [type (-> int long)])
  (define boxed-int (box x))
  (define boxed-long (box (->long (unbox boxed-int)))) 
  (unbox boxed-long))

(function (run)
  #:: (export [type (-> Void)] [expected 7])
  (put-long (main 7)))
